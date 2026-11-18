namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Settings for 3D PivotMDS + 3D Stochastic Gradient Descent (SGD) Stress layout (Zheng, Pawar &amp; Goodman 2019).</summary>
type Stress3DConfig =
    { /// <summary>Target 3D bounding volume center.</summary>
      Bounds: Bounds3
      /// <summary>Ideal 3D Euclidean length per graph shortest-path hop.</summary>
      IdealEdgeLength: float
      /// <summary>SGD epoch count (1..256; default 20).</summary>
      Iterations: int
      /// <summary>Final annealing precision epsilon in (0, 1] (default 0.05).</summary>
      Epsilon: float
      /// <summary>Maximum vertex count for exact all-pairs shortest paths before switching to sparse landmark + edge pairs (default 300).</summary>
      ExactPairLimit: int
      /// <summary>Landmark pivot count in sparse mode (2..256; default 48).</summary>
      PivotCount: int
      /// <summary>Optional fixed Z coordinate per node ID for 2.5D/3D stratified metric layouts.</summary>
      FixedZ: Map<string, float>
      /// <summary>Deterministic Fisher-Yates shuffle seed.</summary>
      Seed: uint64 }

/// <summary>Constructors for <c>Stress3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Stress3DConfig =
    /// <summary>Creates a 3D SGD Stress configuration with 95-unit ideal edges, 20 epochs, epsilon 0.05 and no Z pins.</summary>
    let create bounds =
        { Bounds = bounds
          IdealEdgeLength = 95.
          Iterations = 20
          Epsilon = 0.05
          ExactPairLimit = 300
          PivotCount = 48
          FixedZ = Map.empty
          Seed = 42UL }

/// <summary>3D PivotMDS + 3D Stochastic Gradient Descent (SGD) Stress minimization engine.</summary>
[<RequireQualifiedAccess>]
module Stress3D =
    /// <summary>Runs 3D PivotMDS + 3D SGD Stress layout with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: Stress3DConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout3D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.IdealEdgeLength && Double.IsFinite config.Epsilon)
           || config.IdealEdgeLength <= 0. || config.Epsilon <= 0. || config.Epsilon > 1.
           || config.Iterations < 1 || config.Iterations > 256
           || config.ExactPairLimit < 2 || config.PivotCount < 2 || config.PivotCount > 256
           || config.FixedZ |> Map.exists (fun _ z -> not (Double.IsFinite z)) then
            Error [ InvalidObservation "Stress3D requires positive IdealEdgeLength, Epsilon in (0,1], 1..256 Iterations, PivotCount in [2,256] and finite FixedZ." ]
        else
            let resolved = DiagramGraph.resolveCollapsed graph
            let eff = DiagramGraph.effective resolved
            let nodes = eff.Nodes
            let assocs = eff.Associations
            let n = nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = Kernels3D.computeGroups3D resolved [||]; Associations = [] }
            else
                let csr = GraphKernels.buildCsr eff
                // Partition into graph-connected components (without group-bridging edges, matching original 3D component decomposition)
                let compOf = Array.create n -1
                let q = Array.zeroCreate<int> n
                let comps = ResizeArray<int[]>()
                let mutable cid = 0
                for s = 0 to n - 1 do
                    if compOf[s] < 0 then
                        let mutable h, t = 0, 0
                        compOf[s] <- cid; q[t] <- s; t <- t + 1
                        while h < t do
                            let u = q[h]
                            h <- h + 1
                            for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                                let v = csr.UndirTargets[idx]
                                if compOf[v] < 0 then compOf[v] <- cid; q[t] <- v; t <- t + 1
                        let mems = Array.sub q 0 t
                        Array.Sort(mems)
                        comps.Add mems
                        cid <- cid + 1

                let c3 = Bounds3.center config.Bounds
                let globalX = Array.zeroCreate<float> n
                let globalY = Array.zeroCreate<float> n
                let globalZ = Array.zeroCreate<float> n
                let mutable compOffsetX = 0.
                let hasFixedZ = not (Map.isEmpty config.FixedZ)

                for ci = 0 to comps.Count - 1 do
                    let mems = comps[ci]
                    let subN = mems.Length
                    // 3-tier SIMD PivotMDS in 3D + HashSet-free sparse stress pair synthesis!
                    let mds = GraphKernels.pivotMdsComponent token csr mems 3 (min config.PivotCount subN)
                    let mx, my, mz = mds[0], mds[1], mds[2]
                    let xs = Array.init subN (fun li -> c3.X + compOffsetX + mx[li] * config.IdealEdgeLength)
                    let ys = Array.init subN (fun li -> c3.Y + my[li] * config.IdealEdgeLength)
                    let zs = Array.init subN (fun li -> c3.Z + mz[li] * config.IdealEdgeLength)

                    let pinXY = Array.zeroCreate<bool> subN
                    let pinZ = Array.zeroCreate<bool> subN
                    let mutable anyPinned = false
                    for li = 0 to subN - 1 do
                        let nd = nodes[mems[li]]
                        match nd.Pinned with
                        | Some p -> xs[li] <- p.X; ys[li] <- p.Y; pinXY[li] <- true; anyPinned <- true
                        | None -> ()
                        if hasFixedZ then
                            match config.FixedZ |> Map.tryFind nd.Id with
                            | Some z -> zs[li] <- z; pinZ[li] <- true; anyPinned <- true
                            | None -> ()

                    let pairsArr = Stress2D.buildStressPairs token csr mems config.IdealEdgeLength config.ExactPairLimit config.PivotCount
                    let pCount = pairsArr.Length
                    if pCount > 0 then
                        let schedule = Stress2D.annealingSchedule pairsArr config.Iterations config.Epsilon
                        let order = Array.init pCount id
                        let mutable rng = RandomState.create (config.Seed + uint64 ci * 65537UL)
                        for ep = 0 to schedule.Length - 1 do
                            token.ThrowIfCancellationRequested()
                            let eta = schedule[ep]
                            for k = pCount - 1 downto 1 do
                                let struct(bits, nr) = RandomState.nextUInt64 rng
                                rng <- nr
                                let sw = int (bits % uint64 (k + 1))
                                let tmp = order[k]
                                order[k] <- order[sw]
                                order[sw] <- tmp

                            if not anyPinned then
                                for k = 0 to pCount - 1 do
                                    Kernels3D.check token k
                                    let struct(i, j, dij, wij) = pairsArr[order[k]]
                                    let mu = min 1. (eta * wij)
                                    let dx, dy, dz = xs[i] - xs[j], ys[i] - ys[j], zs[i] - zs[j]
                                    let magSq = dx * dx + dy * dy + dz * dz
                                    let rx, ry, rz =
                                        if magSq <= 1e-18 then
                                            let a = float ((i * 19 + j * 31 + ep) % 360) * (Math.PI / 180.)
                                            dij * 0.5 * cos a, dij * 0.5 * sin a, 0.
                                        else
                                            let mag = sqrt magSq
                                            let f = mu * (mag - dij) / (2. * mag)
                                            dx * f, dy * f, dz * f
                                    xs[i] <- xs[i] - rx; ys[i] <- ys[i] - ry; zs[i] <- zs[i] - rz
                                    xs[j] <- xs[j] + rx; ys[j] <- ys[j] + ry; zs[j] <- zs[j] + rz
                            else
                                for k = 0 to pCount - 1 do
                                    Kernels3D.check token k
                                    let struct(i, j, dij, wij) = pairsArr[order[k]]
                                    let mu = min 1. (eta * wij)
                                    let dx, dy, dz = xs[i] - xs[j], ys[i] - ys[j], zs[i] - zs[j]
                                    let magSq = dx * dx + dy * dy + dz * dz
                                    let rx, ry, rz =
                                        if magSq <= 1e-18 then
                                            let a = float ((i * 19 + j * 31 + ep) % 360) * (Math.PI / 180.)
                                            dij * 0.5 * cos a, dij * 0.5 * sin a, 0.
                                        else
                                            let mag = sqrt magSq
                                            let f = mu * (mag - dij) / (2. * mag)
                                            dx * f, dy * f, dz * f
                                    if not pinXY[i] && not pinXY[j] then
                                        xs[i] <- xs[i] - rx; ys[i] <- ys[i] - ry
                                        xs[j] <- xs[j] + rx; ys[j] <- ys[j] + ry
                                    elif pinXY[i] && not pinXY[j] then
                                        xs[j] <- xs[j] + 2. * rx; ys[j] <- ys[j] + 2. * ry
                                    elif not pinXY[i] && pinXY[j] then
                                        xs[i] <- xs[i] - 2. * rx; ys[i] <- ys[i] - 2. * ry

                                    if not pinZ[i] && not pinZ[j] then
                                        zs[i] <- zs[i] - rz; zs[j] <- zs[j] + rz
                                    elif pinZ[i] && not pinZ[j] then
                                        zs[j] <- zs[j] + 2. * rz
                                    elif not pinZ[i] && pinZ[j] then
                                        zs[i] <- zs[i] - 2. * rz

                    let mutable maxX = -infinity
                    let mutable minX = infinity
                    for li = 0 to subN - 1 do
                        let gi = mems[li]
                        globalX[gi] <- xs[li]; globalY[gi] <- ys[li]; globalZ[gi] <- zs[li]
                        if xs[li] > maxX then maxX <- xs[li]
                        if xs[li] < minX then minX <- xs[li]
                    compOffsetX <- compOffsetX + max 120. (maxX - minX + config.IdealEdgeLength)

                let hasGroups = eff.Groups.Length > 0
                let nodeBoxes = if hasGroups then Array.zeroCreate<Bounds3> n else [||]
                let centers = Array.zeroCreate<Point3> n
                let radii = Array.zeroCreate<float> n
                let mutable posNodes = []
                for i = n - 1 downto 0 do
                    let nd = nodes[i]
                    let pt = Point3.create globalX[i] globalY[i] globalZ[i]
                    centers[i] <- pt
                    let box = Kernels3D.nodeBox pt nd.Size.Width nd.Size.Height nd.DepthSize
                    let rad = 0.5 * sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height + nd.DepthSize * nd.DepthSize)
                    radii[i] <- rad
                    if hasGroups then nodeBoxes[i] <- box
                    posNodes <-
                        { Id = nd.Id
                          Kind = nd.Kind
                          Center = pt
                          Bounds = box
                          Radius = rad
                          ScaleFactor = 1.
                          ParentGroup = nd.ParentGroup
                          Layer = None } :: posNodes

                let groups = if hasGroups then Kernels3D.computeGroups3D resolved nodeBoxes else []
                let mutable routed = []
                for e = assocs.Length - 1 downto 0 do
                    let a = assocs[e]
                    let u, v = csr.Src[e], csr.Dst[e]
                    let pts, wps =
                        if u = v then
                            let c, r = centers[u], radii[u]
                            let lp = Point3.create (c.X + r * 1.4) (c.Y - r * 1.4) (c.Z + r * 0.5)
                            [| c; lp; c |], [ c; lp; c ]
                        else
                            let cu, cv = centers[u], centers[v]
                            [| cu; cv |], [ cu; cv ]
                    routed <-
                        { Id = a.Id
                          Source = a.Source
                          Target = a.Target
                          Kind = a.Kind
                          IsBackbone = true
                          Points = Point3Chain.ofOwnedArray pts
                          Waypoints = wps } :: routed

                Ok
                    { Bounds = Kernels3D.computeBounds3D config.Bounds posNodes groups routed
                      Nodes = posNodes
                      Groups = groups
                      Associations = routed }

    /// <summary>Runs 3D PivotMDS + 3D SGD Stress layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

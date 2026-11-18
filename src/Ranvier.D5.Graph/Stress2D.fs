namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Settings for Brandes &amp; Pich (2006) O(k(V + E)) PivotMDS landmark scaling layout.</summary>
type PivotMdsConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Number of k-center landmark pivots per component (2..256; default 50).</summary>
      PivotCount: int
      /// <summary>Unit-hop coordinate scale in layout units.</summary>
      EdgeScale: float }

/// <summary>Constructors for <c>PivotMdsConfig</c>.</summary>
[<RequireQualifiedAccess>]
module PivotMdsConfig =
    /// <summary>Creates a PivotMDS configuration with 50 landmark pivots and 90-unit edge scale.</summary>
    let create bounds = { Bounds = bounds; PivotCount = 50; EdgeScale = 90. }

/// <summary>Linear-time O(k(V + E)) landmark multidimensional scaling layout (Brandes &amp; Pich 2006).</summary>
[<RequireQualifiedAccess>]
module PivotMds =
    /// <summary>Computes a deterministic 2D PivotMDS layout over <c>graph</c> with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: PivotMdsConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if config.PivotCount < 2 || config.PivotCount > 256 || not (Double.IsFinite config.EdgeScale) || config.EdgeScale <= 0. then
            Error [ InvalidObservation "PivotMds requires PivotCount in [2,256] and positive finite EdgeScale." ]
        else
            let eff = DiagramGraph.effective graph
            let csr = GraphKernels.buildCsr eff
            if csr.NodeCount = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let _, components = GraphKernels.connectedComponents eff csr
                let cx0 = Bounds.x config.Bounds + Bounds.width config.Bounds * 0.5
                let cy0 = Bounds.y config.Bounds + Bounds.height config.Bounds * 0.5

                let layoutComponent (members: int[]) : DiagramLayout2D =
                    let subN = members.Length
                    let coords = GraphKernels.pivotMdsComponent token csr members 2 config.PivotCount
                    let subGroups = GraphKernels.filterSubGroups eff members
                    let subEff = GraphKernels.sliceComponentEff eff csr members subGroups
                    let subNodes = subEff.Nodes

                    let posNodes =
                        Array.init subN (fun li ->
                            let nd = subNodes[li]
                            let pt = nd.Pinned |> Option.defaultValue (Point.create (cx0 + coords[0][li] * config.EdgeScale) (cy0 + coords[1][li] * config.EdgeScale))
                            let b = Bounds.create (pt.X - nd.Size.Width * 0.5) (pt.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = b
                              Center = pt
                              ParentGroup = nd.ParentGroup
                              Layer = None
                              Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                    let groups = GraphKernels.computeGroupBounds subEff (posNodes |> Array.map (fun pn -> pn.Bounds))
                    let routed = GraphKernels.routeDirectAssociations subEff posNodes None
                    let nList = Array.toList posNodes
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

                let compLayouts = components |> Array.map layoutComponent
                Ok(BoxPack.packComponentLayouts config.Bounds 28. compLayouts)

    /// <summary>Computes a deterministic 2D PivotMDS layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

/// <summary>Settings for Zheng, Pawar &amp; Goodman (2019) Stochastic Gradient Descent (SGD) Stress minimization.</summary>
type Stress2DConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Ideal Euclidean distance per shortest-path hop.</summary>
      IdealEdgeLength: float
      /// <summary>SGD epoch count (1..256; default 20).</summary>
      Iterations: int
      /// <summary>Stopping precision epsilon controlling final step rate <c>eta_min = epsilon / w_max</c> (default 0.05).</summary>
      Epsilon: float
      /// <summary>Maximum component size for exact all-pairs shortest paths before switching to sparse landmark + edge pairs (default 350).</summary>
      ExactPairLimit: int
      /// <summary>Landmark pivot count used in sparse mode (default 48).</summary>
      PivotCount: int
      /// <summary>Deterministic Fisher-Yates shuffle seed.</summary>
      Seed: uint64
      /// <summary>When true, applies a post-SGD VPSC non-overlap pass over node boxes.</summary>
      AvoidOverlaps: bool }

/// <summary>Constructors for <c>Stress2DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Stress2DConfig =
    /// <summary>Creates a 2D SGD Stress configuration with 95-unit ideal edges, 20 epochs, epsilon 0.05 and seed 42.</summary>
    let create bounds =
        { Bounds = bounds
          IdealEdgeLength = 95.
          Iterations = 20
          Epsilon = 0.05
          ExactPairLimit = 350
          PivotCount = 48
          Seed = 42UL
          AvoidOverlaps = false }

/// <summary>Graph Drawing by Stochastic Gradient Descent (Zheng, Pawar &amp; Goodman, IEEE TVCG 2019) with PivotMDS initialization.</summary>
[<RequireQualifiedAccess>]
module Stress2D =
    /// <summary>Builds exact all-pairs (when small) or sparse landmark + edge stress pairs <c>(u, v, idealDist, weight)</c>.</summary>
    let internal buildStressPairs
        (token: System.Threading.CancellationToken)
        (csr: CsrGraph)
        (members: int[])
        (idealEdgeLength: float)
        (exactLimit: int)
        (pivotCount: int)
        : struct(int * int * float * float)[] =
        let subN = members.Length
        if subN <= 1 then [||]
        else
            let isFull = (subN = csr.NodeCount)
            let globalToLocal =
                if isFull then [||]
                else
                    let arr = Array.create csr.NodeCount -1
                    for li = 0 to subN - 1 do arr[members[li]] <- li
                    arr

            if subN <= exactLimit then
                // Exact APSP via subN fast flat-array BFS sweeps
                let pairs = ResizeArray<struct(int * int * float * float)>(subN * (subN - 1) / 2)
                let dist = Array.zeroCreate<int> subN
                let q = Array.zeroCreate<int> subN
                for s = 0 to subN - 2 do
                    SimdMath.check token s
                    Array.fill dist 0 subN -1
                    dist[s] <- 0
                    let mutable head = 0
                    let mutable tail = 0
                    q[tail] <- s
                    tail <- tail + 1
                    if isFull then
                        while head < tail do
                            let u = q[head]
                            head <- head + 1
                            let dNext = dist[u] + 1
                            for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                                let v = csr.UndirTargets[idx]
                                if dist[v] < 0 then
                                    dist[v] <- dNext
                                    q[tail] <- v
                                    tail <- tail + 1
                    else
                        while head < tail do
                            let lu = q[head]
                            head <- head + 1
                            let dNext = dist[lu] + 1
                            let gu = members[lu]
                            for idx = csr.UndirOffsets[gu] to csr.UndirOffsets[gu + 1] - 1 do
                                let lv = globalToLocal[csr.UndirTargets[idx]]
                                if lv >= 0 && dist[lv] < 0 then
                                    dist[lv] <- dNext
                                    q[tail] <- lv
                                    tail <- tail + 1
                    for t = s + 1 to subN - 1 do
                        if dist[t] > 0 then
                            let dij = float dist[t] * idealEdgeLength
                            let wij = 1. / (dij * dij)
                            pairs.Add(struct(s, t, dij, wij))
                pairs.ToArray()
            else
                // Sparse Stress (Ortmann et al. 2016 / Pivot + Edges): O(k * subN + subE)
                // Verified by Rocq: sparse_stress_pivot_edge_dedup_exact (zero HashSet<uint64> allocations!)
                let pivots, pDists = GraphKernels.componentPivotDistances token csr members pivotCount
                let isPivot = Array.zeroCreate<bool> subN
                for pi = 0 to pivots.Length - 1 do isPivot[pivots[pi]] <- true
                let pairs = ResizeArray<struct(int * int * float * float)>(pivots.Length * subN + subN * 4)
                let edgeDij = idealEdgeLength
                let edgeWij = 1. / (edgeDij * edgeDij)

                if isFull then
                    for u = 0 to subN - 1 do
                        for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                            let v = csr.UndirTargets[idx]
                            if u < v then
                                pairs.Add(struct(u, v, edgeDij, edgeWij))
                else
                    for li = 0 to subN - 1 do
                        let gu = members[li]
                        for idx = csr.UndirOffsets[gu] to csr.UndirOffsets[gu + 1] - 1 do
                            let lj = globalToLocal[csr.UndirTargets[idx]]
                            if lj > li then
                                pairs.Add(struct(li, lj, edgeDij, edgeWij))

                for pi = 0 to pivots.Length - 1 do
                    let lp = pivots[pi]
                    let row = pDists[pi]
                    for lj = 0 to subN - 1 do
                        let hops = row[lj]
                        if hops > 1. && Double.IsFinite hops && (not isPivot[lj] || lp < lj) then
                            let a, b = if lp < lj then lp, lj else lj, lp
                            let dij = hops * idealEdgeLength
                            let wij = 1. / (dij * dij)
                            pairs.Add(struct(a, b, dij, wij))

                pairs.ToArray()

    /// <summary>Computes the Zheng et al. (2019) exponential/Robbins-Monro annealing schedule <c>eta(t)</c>.</summary>
    let internal annealingSchedule (pairs: struct(int * int * float * float)[]) (iterations: int) (epsilon: float) : float[] =
        if pairs.Length = 0 || iterations <= 0 then [||]
        else
            let mutable wMin = infinity
            let mutable wMax = 0.
            for i = 0 to pairs.Length - 1 do
                let struct(_, _, _, w) = pairs[i]
                if w < wMin then wMin <- w
                if w > wMax then wMax <- w
            let etaMax = 1. / max 1e-12 wMin
            let etaMin = epsilon / max 1e-12 wMax
            if iterations = 1 then [| etaMin |]
            else
                let lambda = log (etaMin / etaMax) / float (iterations - 1)
                Array.init iterations (fun t -> etaMax * exp (lambda * float t))

    /// <summary>Runs 2D PivotMDS + SGD Stress layout with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: Stress2DConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.IdealEdgeLength && Double.IsFinite config.Epsilon)
           || config.IdealEdgeLength <= 0. || config.Epsilon <= 0. || config.Epsilon > 1.
           || config.Iterations < 1 || config.Iterations > 256
           || config.ExactPairLimit < 2 || config.PivotCount < 2 || config.PivotCount > 256 then
            Error [ InvalidObservation "Stress2D requires positive IdealEdgeLength, Epsilon in (0,1], 1..256 Iterations and PivotCount in [2,256]." ]
        else
            let eff = DiagramGraph.effective graph
            let csr = GraphKernels.buildCsr eff
            if csr.NodeCount = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let _, components = GraphKernels.connectedComponents eff csr
                let cx0 = Bounds.x config.Bounds + Bounds.width config.Bounds * 0.5
                let cy0 = Bounds.y config.Bounds + Bounds.height config.Bounds * 0.5

                let layoutComponent (compIdx: int) (members: int[]) : DiagramLayout2D =
                    token.ThrowIfCancellationRequested()
                    let subN = members.Length
                    let mds = GraphKernels.pivotMdsComponent token csr members 2 (min config.PivotCount subN)
                    let xs = Array.init subN (fun li -> cx0 + mds[0][li] * config.IdealEdgeLength)
                    let ys = Array.init subN (fun li -> cy0 + mds[1][li] * config.IdealEdgeLength)
                    let pinned = Array.zeroCreate<bool> subN
                    let mutable anyPinned = false
                    for li = 0 to subN - 1 do
                        match eff.Nodes[members[li]].Pinned with
                        | Some pt ->
                            xs[li] <- pt.X; ys[li] <- pt.Y
                            pinned[li] <- true
                            anyPinned <- true
                        | None -> ()

                    let pairs = buildStressPairs token csr members config.IdealEdgeLength config.ExactPairLimit config.PivotCount
                    if pairs.Length > 0 then
                        let schedule = annealingSchedule pairs config.Iterations config.Epsilon
                        let pCount = pairs.Length
                        let order = Array.init pCount id
                        let mutable rng = RandomState.create (config.Seed + uint64 compIdx * 65537UL)

                        for epoch = 0 to schedule.Length - 1 do
                            token.ThrowIfCancellationRequested()
                            let eta = schedule[epoch]
                            // Fisher-Yates shuffle of pair indices
                            for k = pCount - 1 downto 1 do
                                let struct(bits, nextRng) = RandomState.nextUInt64 rng
                                rng <- nextRng
                                let swapIdx = int (bits % uint64 (k + 1))
                                let tmp = order[k]
                                order[k] <- order[swapIdx]
                                order[swapIdx] <- tmp

                            if not anyPinned then
                                for k = 0 to pCount - 1 do
                                    SimdMath.check token k
                                    let struct(i, j, dij, wij) = pairs[order[k]]
                                    let mu = min 1. (eta * wij)
                                    let dx = xs[i] - xs[j]
                                    let dy = ys[i] - ys[j]
                                    let magSq = dx * dx + dy * dy
                                    let rx, ry =
                                        if magSq <= 1e-18 then
                                            let ang = float ((i * 19 + j * 31 + epoch) % 360) * (Math.PI / 180.)
                                            dij * cos ang * 0.5, dij * sin ang * 0.5
                                        else
                                            let mag = sqrt magSq
                                            let factor = mu * (mag - dij) / (2. * mag)
                                            dx * factor, dy * factor
                                    xs[i] <- xs[i] - rx
                                    ys[i] <- ys[i] - ry
                                    xs[j] <- xs[j] + rx
                                    ys[j] <- ys[j] + ry
                            else
                                for k = 0 to pCount - 1 do
                                    SimdMath.check token k
                                    let struct(i, j, dij, wij) = pairs[order[k]]
                                    if not (pinned[i] && pinned[j]) then
                                        let mu = min 1. (eta * wij)
                                        let dx = xs[i] - xs[j]
                                        let dy = ys[i] - ys[j]
                                        let magSq = dx * dx + dy * dy
                                        let rx, ry =
                                            if magSq <= 1e-18 then
                                                let ang = float ((i * 19 + j * 31 + epoch) % 360) * (Math.PI / 180.)
                                                dij * cos ang * 0.5, dij * sin ang * 0.5
                                            else
                                                let mag = sqrt magSq
                                                let factor = mu * (mag - dij) / (2. * mag)
                                                dx * factor, dy * factor
                                        if pinned[i] then
                                            xs[j] <- xs[j] + 2. * rx
                                            ys[j] <- ys[j] + 2. * ry
                                        elif pinned[j] then
                                            xs[i] <- xs[i] - 2. * rx
                                            ys[i] <- ys[i] - 2. * ry
                                        else
                                            xs[i] <- xs[i] - rx
                                            ys[i] <- ys[i] - ry
                                            xs[j] <- xs[j] + rx
                                            ys[j] <- ys[j] + ry

                    if config.AvoidOverlaps && subN > 1 then
                        let halfW = Array.init subN (fun li -> eff.Nodes[members[li]].Size.Width * 0.5)
                        let halfH = Array.init subN (fun li -> eff.Nodes[members[li]].Size.Height * 0.5)
                        let wts = Array.init subN (fun li -> eff.Nodes[members[li]].Mass)
                        let xCons = ResizeArray<SeparationConstraint>()
                        let yCons = ResizeArray<SeparationConstraint>()
                        Cola.appendNonOverlapConstraints xs ys halfW halfH 8. xCons yCons
                        if xCons.Count > 0 then Array.blit (Cola.solveVpsc xs wts pinned xCons) 0 xs 0 subN
                        if yCons.Count > 0 then Array.blit (Cola.solveVpsc ys wts pinned yCons) 0 ys 0 subN

                    let subGroups = GraphKernels.filterSubGroups eff members
                    let subEff = GraphKernels.sliceComponentEff eff csr members subGroups
                    let subNodes = subEff.Nodes

                    let posNodes =
                        Array.init subN (fun li ->
                            let nd = subNodes[li]
                            let b = Bounds.create (xs[li] - nd.Size.Width * 0.5) (ys[li] - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = b
                              Center = Point.create xs[li] ys[li]
                              ParentGroup = nd.ParentGroup
                              Layer = None
                              Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                    let groups = GraphKernels.computeGroupBounds subEff (posNodes |> Array.map (fun pn -> pn.Bounds))
                    let routed = GraphKernels.routeDirectAssociations subEff posNodes None
                    let nList = Array.toList posNodes
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

                let compLayouts = components |> Array.mapi layoutComponent
                Ok(BoxPack.packComponentLayouts config.Bounds 28. compLayouts)

    /// <summary>Runs 2D PivotMDS + SGD Stress layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

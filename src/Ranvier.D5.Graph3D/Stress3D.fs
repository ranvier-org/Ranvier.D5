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
    let private bfsDistances (adj: int[][]) (start: int) (distOut: int[]) (queue: int[]) =
        let n = adj.Length
        Array.fill distOut 0 n -1
        distOut[start] <- 0
        let mutable head = 0
        let mutable tail = 0
        queue[tail] <- start
        tail <- tail + 1
        while head < tail do
            let u = queue[head]
            head <- head + 1
            let dNext = distOut[u] + 1
            let nbs = adj[u]
            for k = 0 to nbs.Length - 1 do
                let v = nbs[k]
                if distOut[v] < 0 then
                    distOut[v] <- dNext
                    queue[tail] <- v
                    tail <- tail + 1

    /// <summary>Computes 3D PivotMDS landmark coordinates <c>(xs, ys, zs)</c> over connected component <c>adj</c> in O(k(V + E)).</summary>
    let private pivotMds3D (token: System.Threading.CancellationToken) (adj: int[][]) (maxPivots: int) : float[] * float[] * float[] =
        let n = adj.Length
        let xs, ys, zs = Array.zeroCreate<float> n, Array.zeroCreate<float> n, Array.zeroCreate<float> n
        if n <= 1 then xs, ys, zs
        elif n = 2 then
            xs[0] <- -0.5; xs[1] <- 0.5
            xs, ys, zs
        else
            let k = min n (max 3 maxPivots)
            let distBuf = Array.zeroCreate<int> n
            let qBuf = Array.zeroCreate<int> n
            bfsDistances adj 0 distBuf qBuf
            let mutable first = 0
            let mutable farthest = -1
            for i = 0 to n - 1 do
                if distBuf[i] > farthest then farthest <- distBuf[i]; first <- i

            let pivots = Array.zeroCreate<int> k
            let dRows = Array.init k (fun _ -> Array.zeroCreate<float> n)
            let minDist = Array.create n infinity

            pivots[0] <- first
            bfsDistances adj first distBuf qBuf
            for i = 0 to n - 1 do
                let d = if distBuf[i] >= 0 then float distBuf[i] else float n
                dRows[0][i] <- d
                minDist[i] <- d

            for p = 1 to k - 1 do
                token.ThrowIfCancellationRequested()
                let mutable nextP = 0
                let mutable best = -1.
                for i = 0 to n - 1 do
                    if minDist[i] > best then best <- minDist[i]; nextP <- i
                pivots[p] <- nextP
                bfsDistances adj nextP distBuf qBuf
                for i = 0 to n - 1 do
                    let d = if distBuf[i] >= 0 then float distBuf[i] else float n
                    dRows[p][i] <- d
                    if d < minDist[i] then minDist[i] <- d

            // Double-center squared distances C[p][i] = -0.5 * (d^2 - rowMean_i - colMean_p + grandMean)
            let cRows = Array.init k (fun p -> Array.init n (fun i -> let d = dRows[p][i] in d * d))
            let colMean = Array.zeroCreate<float> k
            let rowMean = Array.zeroCreate<float> n
            let mutable grand = 0.
            for p = 0 to k - 1 do
                let mutable ps = 0.
                let r = cRows[p]
                for i = 0 to n - 1 do
                    ps <- ps + r[i]
                    rowMean[i] <- rowMean[i] + r[i]
                colMean[p] <- ps / float n
                grand <- grand + ps
            let grandMean = grand / float (k * n)
            for i = 0 to n - 1 do rowMean[i] <- rowMean[i] / float k
            for p = 0 to k - 1 do
                let r = cRows[p]
                let cm = colMean[p]
                for i = 0 to n - 1 do
                    r[i] <- -0.5 * (r[i] - rowMean[i] - cm + grandMean)

            // k x k inner product matrix M = C * C^T
            let mMat = Array.init k (fun _ -> Array.zeroCreate<float> k)
            for p = 0 to k - 1 do
                for q = p to k - 1 do
                    let mutable s = 0.
                    let rp, rq = cRows[p], cRows[q]
                    for i = 0 to n - 1 do s <- s + rp[i] * rq[i]
                    mMat[p][q] <- s
                    mMat[q][p] <- s

            let outArrays = [| xs; ys; zs |]
            let found = ResizeArray<float[]>(3)
            for dim = 0 to 2 do
                let vec = Array.init k (fun p -> sin (float ((p + 1) * (dim + 3)) * 1.61803398875) + (if p = dim % k then 1. else 0.))
                let nextV = Array.zeroCreate<float> k
                let ortho (v: float[]) =
                    for prev in found do
                        let mutable dot = 0.
                        for p = 0 to k - 1 do dot <- dot + v[p] * prev[p]
                        for p = 0 to k - 1 do v[p] <- v[p] - dot * prev[p]
                let norm (v: float[]) =
                    let mutable s = 0.
                    for p = 0 to k - 1 do s <- s + v[p] * v[p]
                    if s <= 1e-24 then false
                    else
                        let inv = 1. / sqrt s
                        for p = 0 to k - 1 do v[p] <- v[p] * inv
                        true
                ortho vec
                if norm vec then
                    let mutable it = 0
                    let mutable doneIter = false
                    while it < 64 && not doneIter do
                        for p = 0 to k - 1 do
                            let mutable acc = 0.
                            let row = mMat[p]
                            for q = 0 to k - 1 do acc <- acc + row[q] * vec[q]
                            nextV[p] <- acc
                        ortho nextV
                        if not (norm nextV) then doneIter <- true
                        else
                            let mutable diff = 0.
                            for p = 0 to k - 1 do
                                let d = nextV[p] - vec[p]
                                diff <- diff + d * d
                                vec[p] <- nextV[p]
                            if diff < 1e-18 then doneIter <- true
                        it <- it + 1
                    found.Add(Array.copy vec)
                    let target = outArrays[dim]
                    for p = 0 to k - 1 do
                        let vp = vec[p]
                        let r = cRows[p]
                        for i = 0 to n - 1 do target[i] <- target[i] + vp * r[i]
                    let mean = (target |> Array.sum) / float n
                    let mutable sq = 0.
                    for i = 0 to n - 1 do
                        target[i] <- target[i] - mean
                        sq <- sq + target[i] * target[i]
                    let rms = sqrt (sq / float n)
                    if rms > 1e-12 then
                        let inv = 1. / rms
                        for i = 0 to n - 1 do target[i] <- target[i] * inv
                else
                    let target = outArrays[dim]
                    for i = 0 to n - 1 do
                        target[i] <- 0.08 * sin (float (i * (dim + 2) + 1) * 2.399963229728653)
            xs, ys, zs

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
            let nodes = DiagramGraph.nodes resolved |> List.toArray
            let assocs = DiagramGraph.associations resolved |> List.toArray
            let n = nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = Kernels3D.computeGroups3D resolved [||]; Associations = [] }
            else
                let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
                for i = 0 to n - 1 do nodeIdx.Add(nodes[i].Id, i)

                let adjSets = Array.init n (fun _ -> HashSet<int>())
                for a in assocs do
                    let u = nodeIdx[a.Source.NodeId]
                    let v = nodeIdx[a.Target.NodeId]
                    if u <> v then
                        adjSets[u].Add v |> ignore
                        adjSets[v].Add u |> ignore
                let adj = adjSets |> Array.map (fun s -> let arr = Seq.toArray s in Array.Sort(arr); arr)

                // Partition into connected components so disconnected subgraphs each get clean 3D coordinates
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
                            for v in adj[u] do
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

                for ci = 0 to comps.Count - 1 do
                    let mems = comps[ci]
                    let subN = mems.Length
                    let g2l = Dictionary<int, int>(subN)
                    for li = 0 to subN - 1 do g2l.Add(mems[li], li)
                    let subAdj =
                        Array.init subN (fun li ->
                            adj[mems[li]] |> Array.choose (fun gv -> match g2l.TryGetValue gv with true, lv -> Some lv | _ -> None))

                    let mx, my, mz = pivotMds3D token subAdj config.PivotCount
                    let xs = Array.init subN (fun li -> c3.X + compOffsetX + mx[li] * config.IdealEdgeLength)
                    let ys = Array.init subN (fun li -> c3.Y + my[li] * config.IdealEdgeLength)
                    let zs = Array.init subN (fun li -> c3.Z + mz[li] * config.IdealEdgeLength)

                    let pinXY = Array.zeroCreate<bool> subN
                    let pinZ = Array.zeroCreate<bool> subN
                    for li = 0 to subN - 1 do
                        let nd = nodes[mems[li]]
                        nd.Pinned |> Option.iter (fun p -> xs[li] <- p.X; ys[li] <- p.Y; pinXY[li] <- true)
                        config.FixedZ |> Map.tryFind nd.Id |> Option.iter (fun z -> zs[li] <- z; pinZ[li] <- true)

                    // Build 3D stress pairs (exact APSP when subN <= ExactPairLimit, sparse k-center pivots + edges otherwise)
                    let pairs = ResizeArray<struct(int * int * float * float)>()
                    let distBuf = Array.zeroCreate<int> subN
                    let qBuf = Array.zeroCreate<int> subN
                    if subN <= config.ExactPairLimit then
                        for s = 0 to subN - 2 do
                            Kernels3D.check token s
                            bfsDistances subAdj s distBuf qBuf
                            for t = s + 1 to subN - 1 do
                                if distBuf[t] > 0 then
                                    let dij = float distBuf[t] * config.IdealEdgeLength
                                    pairs.Add(struct(s, t, dij, 1. / (dij * dij)))
                    else
                        let seen = HashSet<uint64>()
                        let addP u v hops =
                            if u <> v && hops > 0 then
                                let a, b = min u v, max u v
                                let key = (uint64 a <<< 32) ||| uint64 (uint32 b)
                                if seen.Add key then
                                    let dij = float hops * config.IdealEdgeLength
                                    pairs.Add(struct(a, b, dij, 1. / (dij * dij)))
                        for u = 0 to subN - 1 do
                            for v in subAdj[u] do addP u v 1
                        let step = max 1 (subN / config.PivotCount)
                        let mutable pIdx = 0
                        while pIdx < subN do
                            token.ThrowIfCancellationRequested()
                            bfsDistances subAdj pIdx distBuf qBuf
                            for t = 0 to subN - 1 do addP pIdx t distBuf[t]
                            pIdx <- pIdx + step

                    let pairsArr = pairs.ToArray()
                    if pairsArr.Length > 0 then
                        let mutable wMin, wMax = infinity, 0.
                        for p in pairsArr do
                            let struct(_, _, _, w) = p
                            if w < wMin then wMin <- w
                            if w > wMax then wMax <- w
                        let etaMax = 1. / max 1e-12 wMin
                        let etaMin = config.Epsilon / max 1e-12 wMax
                        let schedule =
                            if config.Iterations = 1 then [| etaMin |]
                            else
                                let lam = log (etaMin / etaMax) / float (config.Iterations - 1)
                                Array.init config.Iterations (fun t -> etaMax * exp (lam * float t))

                        let order = Array.init pairsArr.Length id
                        let mutable rng = RandomState.create (config.Seed + uint64 ci * 65537UL)
                        for ep = 0 to schedule.Length - 1 do
                            token.ThrowIfCancellationRequested()
                            let eta = schedule[ep]
                            for k = order.Length - 1 downto 1 do
                                let struct(bits, nr) = RandomState.nextUInt64 rng
                                rng <- nr
                                let sw = int (bits % uint64 (k + 1))
                                let tmp = order[k]
                                order[k] <- order[sw]
                                order[sw] <- tmp

                            for k = 0 to order.Length - 1 do
                                Kernels3D.check token k
                                let struct(i, j, dij, wij) = pairsArr[order[k]]
                                let mu = min 1. (eta * wij)
                                let dx, dy, dz = xs[i] - xs[j], ys[i] - ys[j], zs[i] - zs[j]
                                let mag = sqrt (dx * dx + dy * dy + dz * dz)
                                let rx, ry, rz =
                                    if mag <= 1e-9 then
                                        let a = float ((i * 19 + j * 31 + ep) % 360) * (Math.PI / 180.)
                                        dij * 0.5 * cos a, dij * 0.5 * sin a, 0.
                                    else
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

                let nodeBoxes = Array.zeroCreate<Bounds3> n
                let posNodes =
                    [ for i in 0 .. n - 1 do
                        let nd = nodes[i]
                        let pt = Point3.create globalX[i] globalY[i] globalZ[i]
                        let box = Kernels3D.nodeBox pt nd.Size.Width nd.Size.Height nd.DepthSize
                        let rad = 0.5 * sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height + nd.DepthSize * nd.DepthSize)
                        nodeBoxes[i] <- box
                        yield
                            { Id = nd.Id
                              Kind = nd.Kind
                              Center = pt
                              Bounds = box
                              Radius = rad
                              ScaleFactor = 1.
                              ParentGroup = nd.ParentGroup
                              Layer = None } ]

                let posArr = List.toArray posNodes
                let groups = Kernels3D.computeGroups3D resolved nodeBoxes
                let routed =
                    [ for a in assocs do
                        let u, v = nodeIdx[a.Source.NodeId], nodeIdx[a.Target.NodeId]
                        let pts =
                            if u = v then
                                let c, r = posArr[u].Center, posArr[u].Radius
                                [| c; Point3.create (c.X + r * 1.4) (c.Y - r * 1.4) (c.Z + r * 0.5); c |]
                            else [| posArr[u].Center; posArr[v].Center |]
                        yield
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              IsBackbone = true
                              Points = Point3Chain.ofOwnedArray pts
                              Waypoints = Array.toList pts } ]

                Ok
                    { Bounds = Kernels3D.computeBounds3D config.Bounds posNodes groups routed
                      Nodes = posNodes
                      Groups = groups
                      Associations = routed }

    /// <summary>Runs 3D PivotMDS + 3D SGD Stress layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

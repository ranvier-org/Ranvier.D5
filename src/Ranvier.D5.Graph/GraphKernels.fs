namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif
open Ranvier.D5

/// <summary>SIMD-accelerated contiguous array kernels on .NET 10 with exact scalar fallbacks under Fable.</summary>
module internal SimdMath =
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline check (token: System.Threading.CancellationToken) (i: int) =
        if (i &&& 63) = 0 then token.ThrowIfCancellationRequested()

    let dot (a: float[]) (b: float[]) (length: int) : float =
#if FABLE_COMPILER
        let mutable sum = 0.
        for i = 0 to length - 1 do
            sum <- sum + a[i] * b[i]
        sum
#else
        if length <= 0 then 0.
        else
            let ra = &MemoryMarshal.GetArrayDataReference(a)
            let rb = &MemoryMarshal.GetArrayDataReference(b)
            let mutable i = 0u
            let ulen = uint32 length
            let mutable sum = 0.
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let mutable acc = Vector512<float>.Zero
                while i + step <= ulen do
                    let va = Vector512.LoadUnsafe(&ra, unativeint i)
                    let vb = Vector512.LoadUnsafe(&rb, unativeint i)
                    acc <- acc + va * vb
                    i <- i + step
                sum <- Vector512.Sum(acc)
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let mutable acc = Vector256<float>.Zero
                while i + step <= ulen do
                    let va = Vector256.LoadUnsafe(&ra, unativeint i)
                    let vb = Vector256.LoadUnsafe(&rb, unativeint i)
                    acc <- acc + va * vb
                    i <- i + step
                sum <- Vector256.Sum(acc)
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let mutable acc = Vector128<float>.Zero
                while i + step <= ulen do
                    let va = Vector128.LoadUnsafe(&ra, unativeint i)
                    let vb = Vector128.LoadUnsafe(&rb, unativeint i)
                    acc <- acc + va * vb
                    i <- i + step
                sum <- Vector128.Sum(acc)
            let mutable idx = int i
            while idx < length do
                sum <- sum + a[idx] * b[idx]
                idx <- idx + 1
            sum
#endif

    let sumSquares (a: float[]) (length: int) : float = dot a a length

    let axpy (alpha: float) (x: float[]) (y: float[]) (length: int) : unit =
#if FABLE_COMPILER
        for i = 0 to length - 1 do
            y[i] <- y[i] + alpha * x[i]
#else
        if length > 0 && alpha <> 0. then
            let rx = &MemoryMarshal.GetArrayDataReference(x)
            let ry = &MemoryMarshal.GetArrayDataReference(y)
            let mutable i = 0u
            let ulen = uint32 length
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let va = Vector512.Create(alpha)
                while i + step <= ulen do
                    let vx = Vector512.LoadUnsafe(&rx, unativeint i)
                    let vy = Vector512.LoadUnsafe(&ry, unativeint i)
                    (vy + va * vx).StoreUnsafe(&ry, unativeint i)
                    i <- i + step
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let va = Vector256.Create(alpha)
                while i + step <= ulen do
                    let vx = Vector256.LoadUnsafe(&rx, unativeint i)
                    let vy = Vector256.LoadUnsafe(&ry, unativeint i)
                    (vy + va * vx).StoreUnsafe(&ry, unativeint i)
                    i <- i + step
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let va = Vector128.Create(alpha)
                while i + step <= ulen do
                    let vx = Vector128.LoadUnsafe(&rx, unativeint i)
                    let vy = Vector128.LoadUnsafe(&ry, unativeint i)
                    (vy + va * vx).StoreUnsafe(&ry, unativeint i)
                    i <- i + step
            let mutable idx = int i
            while idx < length do
                y[idx] <- y[idx] + alpha * x[idx]
                idx <- idx + 1
#endif

    let scaleInPlace (alpha: float) (x: float[]) (length: int) : unit =
#if FABLE_COMPILER
        for i = 0 to length - 1 do
            x[i] <- x[i] * alpha
#else
        if length > 0 then
            let rx = &MemoryMarshal.GetArrayDataReference(x)
            let mutable i = 0u
            let ulen = uint32 length
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let va = Vector512.Create(alpha)
                while i + step <= ulen do
                    (Vector512.LoadUnsafe(&rx, unativeint i) * va).StoreUnsafe(&rx, unativeint i)
                    i <- i + step
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let va = Vector256.Create(alpha)
                while i + step <= ulen do
                    (Vector256.LoadUnsafe(&rx, unativeint i) * va).StoreUnsafe(&rx, unativeint i)
                    i <- i + step
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let va = Vector128.Create(alpha)
                while i + step <= ulen do
                    (Vector128.LoadUnsafe(&rx, unativeint i) * va).StoreUnsafe(&rx, unativeint i)
                    i <- i + step
            let mutable idx = int i
            while idx < length do
                x[idx] <- x[idx] * alpha
                idx <- idx + 1
#endif

    let zeroMeanInPlace (x: float[]) (length: int) : unit =
        if length > 0 then
            let mutable sum = 0.
            for i = 0 to length - 1 do sum <- sum + x[i]
            let mean = sum / float length
            for i = 0 to length - 1 do x[i] <- x[i] - mean

/// <summary>Compressed Sparse Row (CSR) directed and symmetric graph representation over dense 0..N-1 indices.</summary>
type internal CsrGraph =
    { NodeCount: int
      EdgeCount: int
      Src: int[]
      Dst: int[]
      Weight: float[]
      MinLen: int[]
      OutOffsets: int[]
      OutEdges: int[]
      InOffsets: int[]
      InEdges: int[]
      UndirOffsets: int[]
      UndirTargets: int[]
      UndirWeights: float[] }

/// <summary>Coarsened graph level for O(V log V + E) multilevel force layouts.</summary>
type internal CoarseLevel =
    { NodeCount: int
      EdgeSrc: int[]
      EdgeDst: int[]
      EdgeWeight: float[]
      FineToCoarse: int[]
      Mass: float[]
      Radius: float[] }

module internal GraphKernels =
    let buildCsr (eff: EffectiveDiagram) : CsrGraph =
        let n = eff.Nodes.Length
        let m = eff.Associations.Length
        let src = Array.zeroCreate<int> m
        let dst = Array.zeroCreate<int> m
        let wt = Array.zeroCreate<float> m
        let minLen = Array.zeroCreate<int> m
        let outDeg = Array.zeroCreate<int> n
        let inDeg = Array.zeroCreate<int> n

        for e = 0 to m - 1 do
            let a = eff.Associations[e]
            let u = eff.ReversedIdMap[a.Source.NodeId]
            let v = eff.ReversedIdMap[a.Target.NodeId]
            src[e] <- u
            dst[e] <- v
            wt[e] <- a.Weight
            minLen[e] <- a.MinLength
            outDeg[u] <- outDeg[u] + 1
            inDeg[v] <- inDeg[v] + 1

        let outOffsets = Array.zeroCreate<int> (n + 1)
        let inOffsets = Array.zeroCreate<int> (n + 1)
        for i = 0 to n - 1 do
            outOffsets[i + 1] <- outOffsets[i] + outDeg[i]
            inOffsets[i + 1] <- inOffsets[i] + inDeg[i]

        let outEdges = Array.zeroCreate<int> m
        let inEdges = Array.zeroCreate<int> m
        let outCursor = Array.copy outOffsets
        let inCursor = Array.copy inOffsets
        for e = 0 to m - 1 do
            let u, v = src[e], dst[e]
            outEdges[outCursor[u]] <- e
            outCursor[u] <- outCursor[u] + 1
            inEdges[inCursor[v]] <- e
            inCursor[v] <- inCursor[v] + 1

        // Build deduplicated symmetric adjacency for shortest-path, component, and coarsening kernels.
        let adjMaps = Array.init n (fun _ -> Dictionary<int, float>())
        for e = 0 to m - 1 do
            let u, v = src[e], dst[e]
            if u <> v then
                let w = wt[e]
                match adjMaps[u].TryGetValue v with
                | true, prev -> if w > prev then adjMaps[u][v] <- w
                | _ -> adjMaps[u].Add(v, w)
                match adjMaps[v].TryGetValue u with
                | true, prev -> if w > prev then adjMaps[v][u] <- w
                | _ -> adjMaps[v].Add(u, w)

        let undirOffsets = Array.zeroCreate<int> (n + 1)
        for i = 0 to n - 1 do
            undirOffsets[i + 1] <- undirOffsets[i] + adjMaps[i].Count
        let totalUndir = undirOffsets[n]
        let undirTargets = Array.zeroCreate<int> totalUndir
        let undirWeights = Array.zeroCreate<float> totalUndir
        for i = 0 to n - 1 do
            let pairs = adjMaps[i] |> Seq.toArray
            Array.Sort(pairs, Comparer<KeyValuePair<int, float>>.Create(fun a b -> compare a.Key b.Key))
            let baseIdx = undirOffsets[i]
            for k = 0 to pairs.Length - 1 do
                undirTargets[baseIdx + k] <- pairs[k].Key
                undirWeights[baseIdx + k] <- pairs[k].Value

        { NodeCount = n
          EdgeCount = m
          Src = src
          Dst = dst
          Weight = wt
          MinLen = minLen
          OutOffsets = outOffsets
          OutEdges = outEdges
          InOffsets = inOffsets
          InEdges = inEdges
          UndirOffsets = undirOffsets
          UndirTargets = undirTargets
          UndirWeights = undirWeights }

    /// <summary>Finds connected components in O(V + E), also keeping nodes sharing a common compound group in the same visual component.</summary>
    let connectedComponents (eff: EffectiveDiagram) (csr: CsrGraph) : int[] * int[][] =
        let n = csr.NodeCount
        if n = 0 then [||], [||]
        else
            let compOf = Array.create n -1
            // Link nodes belonging to the same root compound group so sibling grouped nodes never get split across disconnected boxes.
            let groupExtra = Array.init n (fun _ -> ResizeArray<int>())
            if eff.Groups.Length > 0 then
                let groupParent = Dictionary<string, string option>(StringComparer.Ordinal)
                for g in eff.Groups do groupParent[g.Id] <- g.ParentGroup
                let rec rootGroup gid =
                    match groupParent.TryGetValue gid with
                    | true, Some pid -> rootGroup pid
                    | _ -> gid
                let firstByRoot = Dictionary<string, int>(StringComparer.Ordinal)
                for i = 0 to n - 1 do
                    match eff.Nodes[i].ParentGroup with
                    | Some gid ->
                        let rg = rootGroup gid
                        match firstByRoot.TryGetValue rg with
                        | true, rep ->
                            groupExtra[rep].Add i
                            groupExtra[i].Add rep
                        | _ -> firstByRoot.Add(rg, i)
                    | None -> ()

            let queue = Array.zeroCreate<int> n
            let comps = ResizeArray<int[]>()
            let mutable cid = 0
            for start = 0 to n - 1 do
                if compOf[start] < 0 then
                    let mutable head = 0
                    let mutable tail = 0
                    compOf[start] <- cid
                    queue[tail] <- start
                    tail <- tail + 1
                    while head < tail do
                        let u = queue[head]
                        head <- head + 1
                        for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                            let v = csr.UndirTargets[idx]
                            if compOf[v] < 0 then
                                compOf[v] <- cid
                                queue[tail] <- v
                                tail <- tail + 1
                        for v in groupExtra[u] do
                            if compOf[v] < 0 then
                                compOf[v] <- cid
                                queue[tail] <- v
                                tail <- tail + 1
                    let members = Array.sub queue 0 tail
                    Array.Sort(members)
                    comps.Add members
                    cid <- cid + 1
            compOf, comps.ToArray()

    /// <summary>Eades-Lin-Smyth (1993) greedy Feedback Arc Set cycle breaker, honoring InitialState sources and FinalState sinks.</summary>
    let feedbackArcSet (eff: EffectiveDiagram) (csr: CsrGraph) : bool[] =
        let n = csr.NodeCount
        let m = csr.EdgeCount
        let reversed = Array.zeroCreate<bool> m
        if n <= 1 || m = 0 then reversed
        else
            let inW = Array.zeroCreate<float> n
            let outW = Array.zeroCreate<float> n
            for e = 0 to m - 1 do
                let u, v = csr.Src[e], csr.Dst[e]
                if u <> v then
                    let w = csr.Weight[e]
                    outW[u] <- outW[u] + w
                    inW[v] <- inW[v] + w

            let active = Array.create n true
            let leftSeq = ResizeArray<int>(n)
            let rightSeq = ResizeArray<int>(n)
            let sources = Queue<int>()
            let sinks = Queue<int>()

            for i = 0 to n - 1 do
                if eff.Nodes[i].Kind = NodeKind.InitialState then
                    active[i] <- false
                    leftSeq.Add i
                elif eff.Nodes[i].Kind = NodeKind.FinalState then
                    active[i] <- false
                    rightSeq.Add i

            let removeNode u =
                for idx = csr.OutOffsets[u] to csr.OutOffsets[u + 1] - 1 do
                    let e = csr.OutEdges[idx]
                    let v = csr.Dst[e]
                    if u <> v && active[v] then
                        inW[v] <- max 0. (inW[v] - csr.Weight[e])
                        if inW[v] <= 1e-12 then sources.Enqueue v
                for idx = csr.InOffsets[u] to csr.InOffsets[u + 1] - 1 do
                    let e = csr.InEdges[idx]
                    let v = csr.Src[e]
                    if u <> v && active[v] then
                        outW[v] <- max 0. (outW[v] - csr.Weight[e])
                        if outW[v] <= 1e-12 then sinks.Enqueue v

            for u in leftSeq do removeNode u
            for u in rightSeq do removeNode u

            for i = 0 to n - 1 do
                if active[i] then
                    if inW[i] <= 1e-12 then sources.Enqueue i
                    elif outW[i] <= 1e-12 then sinks.Enqueue i

            let mutable remaining = 0
            for i = 0 to n - 1 do if active[i] then remaining <- remaining + 1

            while remaining > 0 do
                let mutable progressed = false
                while sources.Count > 0 do
                    let u = sources.Dequeue()
                    if active[u] then
                        active[u] <- false
                        remaining <- remaining - 1
                        leftSeq.Add u
                        removeNode u
                        progressed <- true
                while sinks.Count > 0 do
                    let u = sinks.Dequeue()
                    if active[u] then
                        active[u] <- false
                        remaining <- remaining - 1
                        rightSeq.Add u
                        removeNode u
                        progressed <- true
                if remaining > 0 && not progressed then
                    let mutable best = -1
                    let mutable bestDelta = Double.NegativeInfinity
                    for i = 0 to n - 1 do
                        if active[i] then
                            let delta = outW[i] - inW[i]
                            if delta > bestDelta then
                                bestDelta <- delta
                                best <- i
                    active[best] <- false
                    remaining <- remaining - 1
                    leftSeq.Add best
                    removeNode best

            let orderPos = Array.zeroCreate<int> n
            let mutable pos = 0
            for u in leftSeq do
                orderPos[u] <- pos
                pos <- pos + 1
            for k = rightSeq.Count - 1 downto 0 do
                orderPos[rightSeq[k]] <- pos
                pos <- pos + 1

            for e = 0 to m - 1 do
                let u, v = csr.Src[e], csr.Dst[e]
                if u <> v && orderPos[u] > orderPos[v] then
                    reversed[e] <- true
            reversed

    /// <summary>Unweighted k-center MaxMin pivot shortest-path matrix on a subset of component vertices. Returns (pivots: int[], dist: float[][] of shape k x subN).</summary>
    let componentPivotDistances
        (token: System.Threading.CancellationToken)
        (csr: CsrGraph)
        (members: int[])
        (maxPivots: int)
        : int[] * float[][] =
        let subN = members.Length
        let k = min subN (max 1 maxPivots)
        let globalToLocal = Dictionary<int, int>(subN)
        for li = 0 to subN - 1 do globalToLocal.Add(members[li], li)

        let pivots = Array.zeroCreate<int> k
        let dists = Array.init k (fun _ -> Array.create subN infinity)
        let minDist = Array.create subN infinity
        let queue = Array.zeroCreate<int> subN

        let bfsFrom (localStart: int) (targetRow: float[]) =
            Array.fill targetRow 0 subN infinity
            targetRow[localStart] <- 0.
            let mutable head = 0
            let mutable tail = 0
            queue[tail] <- localStart
            tail <- tail + 1
            while head < tail do
                SimdMath.check token head
                let lu = queue[head]
                head <- head + 1
                let dNext = targetRow[lu] + 1.
                let gu = members[lu]
                for idx = csr.UndirOffsets[gu] to csr.UndirOffsets[gu + 1] - 1 do
                    let gv = csr.UndirTargets[idx]
                    match globalToLocal.TryGetValue gv with
                    | true, lv when targetRow[lv] = infinity ->
                        targetRow[lv] <- dNext
                        queue[tail] <- lv
                        tail <- tail + 1
                    | _ -> ()

        // Choose first pivot by peripheral sweep from local vertex 0
        if k > 0 then
            let probe = Array.create subN infinity
            bfsFrom 0 probe
            let mutable first = 0
            let mutable farthest = -1.
            for li = 0 to subN - 1 do
                if Double.IsFinite probe[li] && probe[li] > farthest then
                    farthest <- probe[li]
                    first <- li
            pivots[0] <- first
            bfsFrom first dists[0]
            for li = 0 to subN - 1 do
                minDist[li] <- dists[0][li]

            for p = 1 to k - 1 do
                token.ThrowIfCancellationRequested()
                let mutable nextPivot = 0
                let mutable best = -1.
                for li = 0 to subN - 1 do
                    if minDist[li] > best then
                        best <- minDist[li]
                        nextPivot <- li
                pivots[p] <- nextPivot
                bfsFrom nextPivot dists[p]
                for li = 0 to subN - 1 do
                    if dists[p][li] < minDist[li] then
                        minDist[li] <- dists[p][li]

        pivots, dists

    /// <summary>Brandes &amp; Pich (2006) PivotMDS embedding into <c>dims</c> dimensions (2 or 3) for a single connected component.</summary>
    let pivotMdsComponent
        (token: System.Threading.CancellationToken)
        (csr: CsrGraph)
        (members: int[])
        (dims: int)
        (maxPivots: int)
        : float[][] =
        let subN = members.Length
        let coords = Array.init dims (fun _ -> Array.zeroCreate<float> subN)
        if subN <= 1 then coords
        elif subN = 2 then
            coords[0][0] <- -0.5
            coords[0][1] <- 0.5
            coords
        else
            let pivots, dists = componentPivotDistances token csr members maxPivots
            let k = pivots.Length
            // Build double-centered matrix C (shape k rows x subN cols; transposed so each row is contiguous length subN for SIMD!).
            // C[p][i] = -0.5 * (d^2 - rowMean_i - colMean_p + grandMean)
            let cRows = Array.init k (fun p ->
                let row = Array.zeroCreate<float> subN
                let dRow = dists[p]
                for i = 0 to subN - 1 do
                    let d = if Double.IsFinite dRow[i] then dRow[i] else float subN
                    row[i] <- d * d
                row)

            let colMean = Array.zeroCreate<float> k // mean over subN for each pivot p
            let rowMean = Array.zeroCreate<float> subN // mean over k for each vertex i
            let mutable grandSum = 0.
            for p = 0 to k - 1 do
                let mutable pSum = 0.
                let r = cRows[p]
                for i = 0 to subN - 1 do
                    let v = r[i]
                    pSum <- pSum + v
                    rowMean[i] <- rowMean[i] + v
                colMean[p] <- pSum / float subN
                grandSum <- grandSum + pSum
            let grandMean = grandSum / float (k * subN)
            for i = 0 to subN - 1 do
                rowMean[i] <- rowMean[i] / float k
            for p = 0 to k - 1 do
                let r = cRows[p]
                let cm = colMean[p]
                for i = 0 to subN - 1 do
                    r[i] <- -0.5 * (r[i] - rowMean[i] - cm + grandMean)

            // Small k x k PSD matrix M = C * C^T (where C here is k x subN, so M[p,q] = dot(cRows[p], cRows[q])).
            let mMat = Array.init k (fun _ -> Array.zeroCreate<float> k)
            for p = 0 to k - 1 do
                token.ThrowIfCancellationRequested()
                for q = p to k - 1 do
                    let v = SimdMath.dot cRows[p] cRows[q] subN
                    mMat[p][q] <- v
                    mMat[q][p] <- v

            // Deflated power iteration on k x k matrix M to extract top `dims` eigenvectors u_r, then x_r = C^T u_r / norm.
            let foundVecs = ResizeArray<float[]>(dims)
            for d = 0 to dims - 1 do
                let vec = Array.init k (fun p -> sin(float((p + 1) * (d + 3)) * 1.61803398875) + (if p = d % k then 1. else 0.))
                let nextVec = Array.zeroCreate<float> k
                let orthogonalize (v: float[]) =
                    for prev in foundVecs do
                        let proj = SimdMath.dot v prev k
                        for p = 0 to k - 1 do v[p] <- v[p] - proj * prev[p]
                let normalize (v: float[]) =
                    let nrm = sqrt(SimdMath.sumSquares v k)
                    if nrm > 1e-12 then
                        SimdMath.scaleInPlace (1. / nrm) v k
                        true
                    else false

                orthogonalize vec
                if normalize vec then
                    let mutable iter = 0
                    let mutable converged = false
                    while iter < 64 && not converged do
                        for p = 0 to k - 1 do
                            nextVec[p] <- SimdMath.dot mMat[p] vec k
                        orthogonalize nextVec
                        if not (normalize nextVec) then
                            converged <- true
                        else
                            // Align sign deterministically
                            let mutable firstNonZero = 0.
                            for p = 0 to k - 1 do
                                if firstNonZero = 0. && abs nextVec[p] > 1e-9 then firstNonZero <- nextVec[p]
                            if firstNonZero < 0. then SimdMath.scaleInPlace -1. nextVec k
                            let mutable diff = 0.
                            for p = 0 to k - 1 do
                                let delta = nextVec[p] - vec[p]
                                diff <- diff + delta * delta
                                vec[p] <- nextVec[p]
                            if diff < 1e-18 then converged <- true
                        iter <- iter + 1
                    foundVecs.Add(Array.copy vec)
                    // Project onto subN vertices: out = sum_p vec[p] * cRows[p]
                    let outCoord = coords[d]
                    for p = 0 to k - 1 do
                        SimdMath.axpy vec[p] cRows[p] outCoord subN
                    SimdMath.zeroMeanInPlace outCoord subN
                    let spread = sqrt(SimdMath.sumSquares outCoord subN / float subN)
                    if spread > 1e-12 then
                        // Scale by Fourth-root of eigenvalue (classic MDS coordinate scale) normalized cleanly
                        let mutable rayleigh = 0.
                        for p = 0 to k - 1 do
                            rayleigh <- rayleigh + vec[p] * SimdMath.dot mMat[p] vec k
                        let targetScale = if rayleigh > 1e-12 then Math.Pow(rayleigh, 0.25) / spread else 1. / spread
                        SimdMath.scaleInPlace targetScale outCoord subN
                else
                    // Degenerate rank (e.g. collinear path for 2nd/3rd axis): small deterministic orthogonal perturbation
                    let outCoord = coords[d]
                    for i = 0 to subN - 1 do
                        outCoord[i] <- 0.05 * sin(float(i * (d + 1) + 1) * 2.399963229728653)
                    SimdMath.zeroMeanInPlace outCoord subN
            coords

    /// <summary>Builds a Heavy-Edge Matching multilevel coarsening hierarchy (Walshaw 2003 / FM3) down to <c>minVertices</c>.</summary>
    let buildMultilevel (csr: CsrGraph) (masses: float[]) (radii: float[]) (minVertices: int) : CoarseLevel list =
        let baseEdges = ResizeArray<struct(int * int * float)>()
        for u = 0 to csr.NodeCount - 1 do
            for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                let v = csr.UndirTargets[idx]
                if u < v then baseEdges.Add(struct(u, v, csr.UndirWeights[idx]))
        let eCount = baseEdges.Count
        let eSrc = Array.init eCount (fun i -> let struct(u, _, _) = baseEdges[i] in u)
        let eDst = Array.init eCount (fun i -> let struct(_, v, _) = baseEdges[i] in v)
        let eWt = Array.init eCount (fun i -> let struct(_, _, w) = baseEdges[i] in w)

        let finest =
            { NodeCount = csr.NodeCount
              EdgeSrc = eSrc
              EdgeDst = eDst
              EdgeWeight = eWt
              FineToCoarse = [||]
              Mass = Array.copy masses
              Radius = Array.copy radii }

        let levels = ResizeArray<CoarseLevel>()
        levels.Add finest
        let mutable cur = finest
        let mutable depth = 0

        while cur.NodeCount > max 8 minVertices && depth < 12 do
            let n = cur.NodeCount
            let m = cur.EdgeSrc.Length
            let adj = Array.init n (fun _ -> ResizeArray<struct(int * float)>())
            for e = 0 to m - 1 do
                let u, v, w = cur.EdgeSrc[e], cur.EdgeDst[e], cur.EdgeWeight[e]
                adj[u].Add(struct(v, w))
                adj[v].Add(struct(u, w))

            // Sort vertices by ascending mass so light hubs merge evenly without creating a single super-star
            let order = Array.init n id
            Array.Sort(order, Comparer<int>.Create(fun a b ->
                let c = compare cur.Mass[a] cur.Mass[b]
                if c <> 0 then c else compare a b))

            let matched = Array.create n -1
            let mutable coarseCount = 0
            for idx = 0 to n - 1 do
                let u = order[idx]
                if matched[u] < 0 then
                    let mutable bestV = -1
                    let mutable bestScore = -1.
                    for pair in adj[u] do
                        let struct(v, w) = pair
                        if matched[v] < 0 then
                            let score = w / max 1e-6 (cur.Mass[v])
                            if score > bestScore then
                                bestScore <- score
                                bestV <- v
                    matched[u] <- coarseCount
                    if bestV >= 0 then matched[bestV] <- coarseCount
                    coarseCount <- coarseCount + 1

            if float coarseCount > 0.85 * float n then
                depth <- 99
            else
                levels[levels.Count - 1] <- { cur with FineToCoarse = matched }
                let cMass = Array.zeroCreate<float> coarseCount
                let cArea = Array.zeroCreate<float> coarseCount
                for i = 0 to n - 1 do
                    let ci = matched[i]
                    cMass[ci] <- cMass[ci] + cur.Mass[i]
                    cArea[ci] <- cArea[ci] + cur.Radius[i] * cur.Radius[i]
                let cRadius = Array.init coarseCount (fun ci -> sqrt cArea[ci])

                let cEdgeMap = Dictionary<uint64, float>()
                for e = 0 to m - 1 do
                    let cu = matched[cur.EdgeSrc[e]]
                    let cv = matched[cur.EdgeDst[e]]
                    if cu <> cv then
                        let a, b = min cu cv, max cu cv
                        let key = (uint64 a <<< 32) ||| uint64 (uint32 b)
                        match cEdgeMap.TryGetValue key with
                        | true, prev -> cEdgeMap[key] <- prev + cur.EdgeWeight[e]
                        | _ -> cEdgeMap.Add(key, cur.EdgeWeight[e])

                let pairs = cEdgeMap |> Seq.toArray
                Array.Sort(pairs, Comparer<KeyValuePair<uint64, float>>.Create(fun a b -> compare a.Key b.Key))
                let cm = pairs.Length
                let nextLevel =
                    { NodeCount = coarseCount
                      EdgeSrc = Array.init cm (fun i -> int (pairs[i].Key >>> 32))
                      EdgeDst = Array.init cm (fun i -> int (uint32 pairs[i].Key))
                      EdgeWeight = Array.init cm (fun i -> pairs[i].Value)
                      FineToCoarse = [||]
                      Mass = cMass
                      Radius = cRadius }
                levels.Add nextLevel
                cur <- nextLevel
                depth <- depth + 1

        List.ofSeq levels

    /// <summary>Resolves default port anchors distributed evenly along each node's cardinal boundaries.</summary>
    let resolveNodePorts (node: DiagramNode) (bounds: Bounds) (portSides: Map<string, PortSide>) : PositionedPort list =
        if node.Ports.IsEmpty then []
        else
            let bySide = Dictionary<PortSide, ResizeArray<DiagramPort>>()
            for s in [ PortSide.North; PortSide.South; PortSide.East; PortSide.West ] do
                bySide[s] <- ResizeArray<DiagramPort>()
            for p in node.Ports do
                let resolvedSide =
                    match p.Side with
                    | PortSide.Auto ->
                        match portSides.TryFind p.Id with
                        | Some s when s <> PortSide.Auto -> s
                        | _ -> PortSide.South
                    | explicitSide -> explicitSide
                bySide[resolvedSide].Add p

            let placed = ResizeArray<PositionedPort>(node.Ports.Length)
            let x0, y0, w, h = Bounds.x bounds, Bounds.y bounds, Bounds.width bounds, Bounds.height bounds
            for kv in bySide do
                let side = kv.Key
                let items = kv.Value
                let count = items.Count
                for idx = 0 to count - 1 do
                    let t = float (idx + 1) / float (count + 1)
                    let anchor =
                        match side with
                        | PortSide.North -> Point.create (x0 + w * t) y0
                        | PortSide.South -> Point.create (x0 + w * t) (y0 + h)
                        | PortSide.West -> Point.create x0 (y0 + h * t)
                        | PortSide.East
                        | PortSide.Auto -> Point.create (x0 + w) (y0 + h * t)
                    placed.Add { Id = items[idx].Id; Side = side; Anchor = anchor }
            placed.Sort(Comparison<PositionedPort>(fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id)))
            List.ofSeq placed

    /// <summary>Computes nested <c>PositionedGroup</c> rectangles bottom-up from positioned visible nodes.</summary>
    let computeGroupBounds (eff: EffectiveDiagram) (nodeBounds: Bounds[]) : PositionedGroup list =
        let gCount = eff.Groups.Length
        if gCount = 0 then []
        else
            let gIdx = Dictionary<string, int>(StringComparer.Ordinal)
            for gi = 0 to gCount - 1 do gIdx.Add(eff.Groups[gi].Id, gi)

            let directNodes = Array.init gCount (fun _ -> ResizeArray<int>())
            for ni = 0 to eff.Nodes.Length - 1 do
                match eff.Nodes[ni].ParentGroup with
                | Some gid ->
                    match gIdx.TryGetValue gid with
                    | true, gi -> directNodes[gi].Add ni
                    | _ -> ()
                | None -> ()

            let childGroups = Array.init gCount (fun _ -> ResizeArray<int>())
            for gi = 0 to gCount - 1 do
                match eff.Groups[gi].ParentGroup with
                | Some pid ->
                    match gIdx.TryGetValue pid with
                    | true, pi -> childGroups[pi].Add gi
                    | _ -> ()
                | None -> ()

            // eff.Groups is sorted ascending by depth, so iterating backward computes leaves before ancestors.
            let outerBounds = Array.zeroCreate<Bounds> gCount
            let innerBounds = Array.zeroCreate<Bounds> gCount
            let cxMean =
                if nodeBounds.Length = 0 then 0.
                else (nodeBounds |> Array.sumBy (fun b -> Bounds.x b + Bounds.width b * 0.5)) / float nodeBounds.Length
            let cyMean =
                if nodeBounds.Length = 0 then 0.
                else (nodeBounds |> Array.sumBy (fun b -> Bounds.y b + Bounds.height b * 0.5)) / float nodeBounds.Length

            for gi = gCount - 1 downto 0 do
                let g = eff.Groups[gi]
                let pad = g.Padding
                let mutable hasBox = false
                let mutable acc = Bounds.create 0. 0. 0. 0.
                for ni in directNodes[gi] do
                    acc <- if not hasBox then (hasBox <- true; nodeBounds[ni]) else Bounds.union acc nodeBounds[ni]
                for cgi in childGroups[gi] do
                    acc <- if not hasBox then (hasBox <- true; outerBounds[cgi]) else Bounds.union acc outerBounds[cgi]

                if not hasBox then
                    let w, h = g.MinSize.Width, g.MinSize.Height
                    let outer = Bounds.create (cxMean - w * 0.5 + float gi * 12.) (cyMean - h * 0.5) w h
                    let iw = max 0. (w - pad.Left - pad.Right)
                    let ih = max 0. (h - pad.Top - pad.Bottom)
                    innerBounds[gi] <- Bounds.create (Bounds.x outer + pad.Left) (Bounds.y outer + pad.Top) iw ih
                    outerBounds[gi] <- outer
                else
                    innerBounds[gi] <- acc
                    let rawW = Bounds.width acc + pad.Left + pad.Right
                    let rawH = Bounds.height acc + pad.Top + pad.Bottom
                    let finalW = max g.MinSize.Width rawW
                    let finalH = max g.MinSize.Height rawH
                    let extraX = (finalW - rawW) * 0.5
                    let extraY = (finalH - rawH) * 0.5
                    outerBounds[gi] <-
                        Bounds.create
                            (Bounds.x acc - pad.Left - extraX)
                            (Bounds.y acc - pad.Top - extraY)
                            finalW
                            finalH

            [ for gi = 0 to gCount - 1 do
                let g = eff.Groups[gi]
                yield
                    { Id = g.Id
                      ParentGroup = g.ParentGroup
                      Bounds = outerBounds[gi]
                      InnerBounds = innerBounds[gi]
                      Collapsed = false
                      Depth = eff.GroupDepth[gi] } ]

    /// <summary>Clips a ray from rectangle center toward <c>outsideTarget</c> onto the rectangle boundary.</summary>
    let clipRectBoundary (rect: Bounds) (outsideTarget: Point) : Point =
        let cx = Bounds.x rect + Bounds.width rect * 0.5
        let cy = Bounds.y rect + Bounds.height rect * 0.5
        let hw = max 0.5 (Bounds.width rect * 0.5)
        let hh = max 0.5 (Bounds.height rect * 0.5)
        let dx = outsideTarget.X - cx
        let dy = outsideTarget.Y - cy
        if abs dx < 1e-9 && abs dy < 1e-9 then
            Point.create cx (cy - hh)
        else
            let tx = if abs dx > 1e-12 then hw / abs dx else infinity
            let ty = if abs dy > 1e-12 then hh / abs dy else infinity
            let t = min tx ty
            Point.create (cx + dx * t) (cy + dy * t)

    /// <summary>Places source, center and target association labels cleanly along a polyline waypoint chain.</summary>
    let placeLabelsOnWaypoints (assoc: Association) (pts: Point[]) : PositionedLabel option * PositionedLabel option * PositionedLabel option =
        if pts.Length = 0 then None, None, None
        else
            let segLen = Array.init (max 0 (pts.Length - 1)) (fun i -> Double.Hypot(pts[i + 1].X - pts[i].X, pts[i + 1].Y - pts[i].Y))
            let totalLen = segLen |> Array.sum
            let sampleAt (fraction: float) =
                if pts.Length = 1 || totalLen <= 1e-9 then
                    pts[0], 0., -1.
                else
                    let target = max 0. (min totalLen (fraction * totalLen))
                    let mutable walked = 0.
                    let mutable chosen = segLen.Length - 1
                    let mutable localT = 1.
                    let mutable found = false
                    let mutable idx = 0
                    while idx < segLen.Length && not found do
                        let l = segLen[idx]
                        if walked + l >= target || idx = segLen.Length - 1 then
                            chosen <- idx
                            localT <- if l <= 1e-9 then 0.5 else (target - walked) / l
                            found <- true
                        else
                            walked <- walked + l
                            idx <- idx + 1
                    let a, b = pts[chosen], pts[chosen + 1]
                    let px = a.X + (b.X - a.X) * localT
                    let py = a.Y + (b.Y - a.Y) * localT
                    let dx, dy = b.X - a.X, b.Y - a.Y
                    let len = Double.Hypot(dx, dy)
                    let nx, ny = if len <= 1e-9 then 0., -1. else -dy / len, dx / len
                    Point.create px py, nx, ny

            let makeBox (size: TextSize) (frac: float) (normalSign: float) =
                let anchor, nx, ny = sampleAt frac
                let offset = 6. + 0.35 * min size.Width size.Height
                let cx = anchor.X + nx * offset * normalSign
                let cy = anchor.Y + ny * offset * normalSign
                { Bounds = Bounds.create (cx - size.Width * 0.5) (cy - size.Height * 0.5) size.Width size.Height
                  Center = Point.create cx cy }

            let srcLbl = assoc.SourceLabel |> Option.map (fun s -> makeBox s 0.16 1.)
            let midLbl = assoc.CenterLabel |> Option.map (fun s -> makeBox s 0.5 0.)
            let tgtLbl = assoc.TargetLabel |> Option.map (fun s -> makeBox s 0.84 -1.)
            srcLbl, midLbl, tgtLbl

    /// <summary>Routes direct polylines/curves for force, stress and radial layouts, separating parallel multi-edges and self-loops.</summary>
    let routeDirectAssociations
        (eff: EffectiveDiagram)
        (nodes: PositionedNode[])
        (reversedFlags: bool[] option)
        : RoutedAssociation list =
        let m = eff.Associations.Length
        let pairCounts = Dictionary<string, int>(StringComparer.Ordinal)
        let pairSeen = Dictionary<string, int>(StringComparer.Ordinal)
        for e = 0 to m - 1 do
            let a = eff.Associations[e]
            let s, t = a.Source.NodeId, a.Target.NodeId
            let key = if String.CompareOrdinal(s, t) <= 0 then s + "\u0000" + t else t + "\u0000" + s
            match pairCounts.TryGetValue key with
            | true, c -> pairCounts[key] <- c + 1
            | _ -> pairCounts.Add(key, 1)

        let lookupPort (node: PositionedNode) (portId: string option) =
            match portId with
            | None -> None
            | Some pid -> node.Ports |> List.tryFind (fun p -> p.Id = pid) |> Option.map (fun p -> p.Anchor)

        [ for e = 0 to m - 1 do
            let a = eff.Associations[e]
            let ui = eff.ReversedIdMap[a.Source.NodeId]
            let vi = eff.ReversedIdMap[a.Target.NodeId]
            let uNode, vNode = nodes[ui], nodes[vi]
            let rev = reversedFlags |> Option.map (fun flags -> flags[e]) |> Option.defaultValue false
            if ui = vi then
                // Clean corner self-loop
                let b = uNode.Bounds
                let rx = Bounds.x b + Bounds.width b
                let ty = Bounds.y b
                let p0 = lookupPort uNode a.Source.PortId |> Option.defaultValue (Point.create (rx - min 12. (Bounds.width b * 0.25)) ty)
                let p3 = lookupPort uNode a.Target.PortId |> Option.defaultValue (Point.create rx (ty + min 12. (Bounds.height b * 0.35)))
                let c1 = Point.create (rx + 22.) (ty - 24.)
                let c2 = Point.create (rx + 26.) (ty + 18.)
                let mid = Point.create (rx + 18.) (ty - 4.)
                let wps = [ p0; mid; p3 ]
                let path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                let sLbl, mLbl, tLbl = placeLabelsOnWaypoints a (List.toArray wps)
                yield
                    { Id = a.Id
                      Source = a.Source
                      Target = a.Target
                      Kind = a.Kind
                      Waypoints = wps
                      Path = path
                      ReversedInCycle = rev
                      SourceLabel = sLbl
                      CenterLabel = mLbl
                      TargetLabel = tLbl }
            else
                let s, t = a.Source.NodeId, a.Target.NodeId
                let key = if String.CompareOrdinal(s, t) <= 0 then s + "\u0000" + t else t + "\u0000" + s
                let totalParallel = pairCounts[key]
                let slot =
                    match pairSeen.TryGetValue key with
                    | true, cur -> pairSeen[key] <- cur + 1; cur
                    | _ -> pairSeen.Add(key, 1); 0

                let rawStart = lookupPort uNode a.Source.PortId |> Option.defaultValue (clipRectBoundary uNode.Bounds vNode.Center)
                let rawEnd = lookupPort vNode a.Target.PortId |> Option.defaultValue (clipRectBoundary vNode.Bounds uNode.Center)
                if totalParallel <= 1 then
                    let wps = [ rawStart; rawEnd ]
                    let path = Path.ofCommands [ MoveTo rawStart; LineTo rawEnd ]
                    let sLbl, mLbl, tLbl = placeLabelsOnWaypoints a [| rawStart; rawEnd |]
                    yield
                        { Id = a.Id
                          Source = a.Source
                          Target = a.Target
                          Kind = a.Kind
                          Waypoints = wps
                          Path = path
                          ReversedInCycle = rev
                          SourceLabel = sLbl
                          CenterLabel = mLbl
                          TargetLabel = tLbl }
                else
                    let offsetIndex = float slot - float (totalParallel - 1) * 0.5
                    let dx, dy = rawEnd.X - rawStart.X, rawEnd.Y - rawStart.Y
                    let dist = Double.Hypot(dx, dy)
                    let nx, ny = if dist <= 1e-9 then 0., -1. else -dy / dist, dx / dist
                    let signFlip = if String.CompareOrdinal(s, t) <= 0 then 1. else -1.
                    let bend = offsetIndex * 18. * signFlip
                    let ctrl = Point.create ((rawStart.X + rawEnd.X) * 0.5 + nx * bend) ((rawStart.Y + rawEnd.Y) * 0.5 + ny * bend)
                    let midOnQuad = Point.create (0.25 * rawStart.X + 0.5 * ctrl.X + 0.25 * rawEnd.X) (0.25 * rawStart.Y + 0.5 * ctrl.Y + 0.25 * rawEnd.Y)
                    let wps = [ rawStart; midOnQuad; rawEnd ]
                    let path = Path.ofCommands [ MoveTo rawStart; QuadraticTo(ctrl, rawEnd) ]
                    let sLbl, mLbl, tLbl = placeLabelsOnWaypoints a (List.toArray wps)
                    yield
                        { Id = a.Id
                          Source = a.Source
                          Target = a.Target
                          Kind = a.Kind
                          Waypoints = wps
                          Path = path
                          ReversedInCycle = rev
                          SourceLabel = sLbl
                          CenterLabel = mLbl
                          TargetLabel = tLbl } ]

    /// <summary>Computes the enclosing <c>Bounds</c> of nodes, groups, route waypoints and labels.</summary>
    let computeDiagramBounds
        (fallback: Bounds)
        (nodes: PositionedNode list)
        (groups: PositionedGroup list)
        (assocs: RoutedAssociation list)
        : Bounds =
        let mutable hasAny = false
        let mutable acc = Bounds.create 0. 0. 0. 0.
        let includeBox b =
            acc <- if not hasAny then (hasAny <- true; b) else Bounds.union acc b
        let includePt (p: Point) =
            includeBox (Bounds.create p.X p.Y 0. 0.)
        for n in nodes do includeBox n.Bounds
        for g in groups do includeBox g.Bounds
        for a in assocs do
            for wp in a.Waypoints do includePt wp
            a.SourceLabel |> Option.iter (fun l -> includeBox l.Bounds)
            a.CenterLabel |> Option.iter (fun l -> includeBox l.Bounds)
            a.TargetLabel |> Option.iter (fun l -> includeBox l.Bounds)
        if hasAny then acc else fallback

    /// <summary>Translates an entire <c>DiagramLayout2D</c> by <c>(dx, dy)</c>.</summary>
    let translateLayout (dx: float) (dy: float) (layout: DiagramLayout2D) : DiagramLayout2D =
        if dx = 0. && dy = 0. then layout
        else
            let shiftPt (p: Point) = Point.create (p.X + dx) (p.Y + dy)
            let shiftBox (b: Bounds) = Bounds.create (Bounds.x b + dx) (Bounds.y b + dy) (Bounds.width b) (Bounds.height b)
            let shiftPath (p: Path) =
                Path.commands p
                |> List.map (function
                    | MoveTo a -> MoveTo(shiftPt a)
                    | LineTo a -> LineTo(shiftPt a)
                    | QuadraticTo(a, b) -> QuadraticTo(shiftPt a, shiftPt b)
                    | CubicTo(a, b, c) -> CubicTo(shiftPt a, shiftPt b, shiftPt c)
                    | ArcTo(rx, ry, rot, lg, cw, d) -> ArcTo(rx, ry, rot, lg, cw, shiftPt d)
                    | Close -> Close)
                |> Path.ofCommands
            let shiftLbl (l: PositionedLabel) = { Bounds = shiftBox l.Bounds; Center = shiftPt l.Center }
            let nodes =
                layout.Nodes
                |> List.map (fun n ->
                    { n with
                        Bounds = shiftBox n.Bounds
                        Center = shiftPt n.Center
                        Ports = n.Ports |> List.map (fun p -> { p with Anchor = shiftPt p.Anchor }) })
            let groups =
                layout.Groups
                |> List.map (fun g -> { g with Bounds = shiftBox g.Bounds; InnerBounds = shiftBox g.InnerBounds })
            let assocs =
                layout.Associations
                |> List.map (fun a ->
                    { a with
                        Waypoints = a.Waypoints |> List.map shiftPt
                        Path = shiftPath a.Path
                        SourceLabel = a.SourceLabel |> Option.map shiftLbl
                        CenterLabel = a.CenterLabel |> Option.map shiftLbl
                        TargetLabel = a.TargetLabel |> Option.map shiftLbl })
            { Bounds = shiftBox layout.Bounds; Nodes = nodes; Groups = groups; Associations = assocs }

/// <summary>Backbone spanning-tree extraction policy for general directed/undirected graphs (used by Radial, H3 and Walrus engines).</summary>
[<RequireQualifiedAccess>]
type SpanningTreeStrategy =
    /// <summary>Breadth-first minimum-hop tree, preferring forward directed associations before undirected fallback edges.</summary>
    | Bfs
    /// <summary>Maximum-weight Prim spanning tree prioritizing high-weight structural associations.</summary>
    | MaxWeight
    /// <summary>Shortest weighted hop-distance Dijkstra arborescence.</summary>
    | MinDepth

/// <summary>Extracted spanning forest separating structural backbone tree associations from non-tree cross-links.</summary>
type SpanningForest =
    { /// <summary>Ordered root node IDs (one per connected component).</summary>
      Roots: string list
      /// <summary>Maps every node ID to its parent node ID (<c>None</c> for roots).</summary>
      ParentOf: Map<string, string option>
      /// <summary>Maps every node ID to its hop depth from its component root.</summary>
      DepthOf: Map<string, int>
      /// <summary>Ordered child node IDs for every node ID.</summary>
      ChildrenOf: Map<string, string list>
      /// <summary>Association IDs selected into the spanning forest backbone.</summary>
      TreeAssociationIds: Set<string>
      /// <summary>Association IDs not in the spanning backbone (cross-links, back-edges, self-loops and parallel edges).</summary>
      CrossAssociationIds: Set<string> }

/// <summary>Extracts rooted spanning trees and classifies backbone vs. cross-link associations over any <c>DiagramGraph</c>.</summary>
[<RequireQualifiedAccess>]
module SpanningTree =
    /// <summary>Extracts a spanning forest from <c>graph</c> (resolving collapsed groups), optionally starting at <c>preferredRoot</c>.</summary>
    let extract (strategy: SpanningTreeStrategy) (preferredRoot: string option) (graph: DiagramGraph) : SpanningForest =
        let eff = DiagramGraph.effective graph
        let csr = GraphKernels.buildCsr eff
        let n = csr.NodeCount
        if n = 0 then
            { Roots = []
              ParentOf = Map.empty
              DepthOf = Map.empty
              ChildrenOf = Map.empty
              TreeAssociationIds = Set.empty
              CrossAssociationIds = Set.empty }
        else
            let visited = Array.zeroCreate<bool> n
            let parentIdx = Array.create n -1
            let depthArr = Array.zeroCreate<int> n
            let treeEdges = HashSet<string>(StringComparer.Ordinal)
            let roots = ResizeArray<int>()

            // Candidate root priority: preferredRoot first, then InitialState, then highest (outDeg - inDeg), then ordinal index.
            let rootPriority = Array.init n id
            let score u =
                let outD = csr.OutOffsets[u + 1] - csr.OutOffsets[u]
                let inD = csr.InOffsets[u + 1] - csr.InOffsets[u]
                let kindBonus =
                    match eff.Nodes[u].Kind with
                    | NodeKind.InitialState -> 1_000_000
                    | NodeKind.FinalState -> -100_000
                    | _ -> (if inD = 0 && outD > 0 then 50_000 else 0)
                kindBonus + (outD - inD) * 16 + outD

            Array.Sort(rootPriority, Comparer<int>.Create(fun a b ->
                let c = compare (score b) (score a)
                if c <> 0 then c else compare a b))

            let startCandidates = ResizeArray<int>(n + 1)
            preferredRoot
            |> Option.bind (fun id ->
                match eff.ReversedIdMap.TryGetValue id with
                | true, idx -> Some idx
                | _ -> None)
            |> Option.iter startCandidates.Add
            for idx in rootPriority do startCandidates.Add idx

            for candidate in startCandidates do
                if not visited[candidate] then
                    roots.Add candidate
                    match strategy with
                    | SpanningTreeStrategy.Bfs ->
                        visited[candidate] <- true
                        depthArr[candidate] <- 0
                        let q = Queue<int>()
                        q.Enqueue candidate
                        while q.Count > 0 do
                            let u = q.Dequeue()
                            // First pass: forward outgoing associations
                            for idx = csr.OutOffsets[u] to csr.OutOffsets[u + 1] - 1 do
                                let e = csr.OutEdges[idx]
                                let v = csr.Dst[e]
                                if not visited[v] then
                                    visited[v] <- true
                                    parentIdx[v] <- u
                                    depthArr[v] <- depthArr[u] + 1
                                    treeEdges.Add eff.Associations[e].Id |> ignore
                                    q.Enqueue v
                            // Second pass: incoming associations treated undirected so weakly connected nodes attach
                            for idx = csr.InOffsets[u] to csr.InOffsets[u + 1] - 1 do
                                let e = csr.InEdges[idx]
                                let v = csr.Src[e]
                                if not visited[v] then
                                    visited[v] <- true
                                    parentIdx[v] <- u
                                    depthArr[v] <- depthArr[u] + 1
                                    treeEdges.Add eff.Associations[e].Id |> ignore
                                    q.Enqueue v
                    | SpanningTreeStrategy.MaxWeight
                    | SpanningTreeStrategy.MinDepth ->
#if FABLE_COMPILER
                        let pq = PriorityQueue<struct(int * int * int * int), float>()
#else
                        let pq = System.Collections.Generic.PriorityQueue<struct(int * int * int * int), float>()
#endif
                        visited[candidate] <- true
                        depthArr[candidate] <- 0
                        let pushNeighbors u =
                            for idx = csr.OutOffsets[u] to csr.OutOffsets[u + 1] - 1 do
                                let e = csr.OutEdges[idx]
                                let v = csr.Dst[e]
                                if not visited[v] then
                                    let pri =
                                        match strategy with
                                        | SpanningTreeStrategy.MaxWeight -> -csr.Weight[e]
                                        | _ -> float (depthArr[u] + 1) + 1. / (1. + csr.Weight[e])
                                    pq.Enqueue(struct(v, u, e, depthArr[u] + 1), pri)
                            for idx = csr.InOffsets[u] to csr.InOffsets[u + 1] - 1 do
                                let e = csr.InEdges[idx]
                                let v = csr.Src[e]
                                if not visited[v] then
                                    let pri =
                                        match strategy with
                                        | SpanningTreeStrategy.MaxWeight -> -csr.Weight[e] * 0.85
                                        | _ -> float (depthArr[u] + 1) + 1.25 / (1. + csr.Weight[e])
                                    pq.Enqueue(struct(v, u, e, depthArr[u] + 1), pri)
                        pushNeighbors candidate
                        while pq.Count > 0 do
                            let struct(v, p, e, d) = pq.Dequeue()
                            if not visited[v] then
                                visited[v] <- true
                                parentIdx[v] <- p
                                depthArr[v] <- d
                                treeEdges.Add eff.Associations[e].Id |> ignore
                                pushNeighbors v

            let childrenLists = Array.init n (fun _ -> ResizeArray<string>())
            for i = 0 to n - 1 do
                let p = parentIdx[i]
                if p >= 0 then childrenLists[p].Add eff.Nodes[i].Id

            let parentMap =
                [ for i in 0 .. n - 1 ->
                    eff.Nodes[i].Id, (if parentIdx[i] < 0 then None else Some eff.Nodes[parentIdx[i]].Id) ]
                |> Map.ofList
            let depthMap = [ for i in 0 .. n - 1 -> eff.Nodes[i].Id, depthArr[i] ] |> Map.ofList
            let childMap = [ for i in 0 .. n - 1 -> eff.Nodes[i].Id, List.ofSeq childrenLists[i] ] |> Map.ofList
            let treeSet = Set.ofSeq treeEdges
            let crossSet =
                eff.Associations
                |> Seq.choose (fun a -> if treeSet.Contains a.Id then None else Some a.Id)
                |> Set.ofSeq

            { Roots = roots |> Seq.map (fun i -> eff.Nodes[i].Id) |> Seq.toList
              ParentOf = parentMap
              DepthOf = depthMap
              ChildrenOf = childMap
              TreeAssociationIds = treeSet
              CrossAssociationIds = crossSet }

namespace Ranvier.D5

open System
open System.Collections.Generic

/// <summary>Agglomerative linkage criteria for Lance-Williams hierarchical clustering.</summary>
[<RequireQualifiedAccess>]
type Linkage =
    /// <summary>Minimum pairwise distance between clusters (nearest-neighbor linkage).</summary>
    | Single
    /// <summary>Maximum pairwise distance between clusters (farthest-neighbor linkage).</summary>
    | Complete
    /// <summary>Unweighted pair-group average distance (UPGMA) weighted proportionally to cluster sizes.</summary>
    | Average
    /// <summary>Minimum within-cluster variance increment (Ward's squared-Euclidean Lance-Williams criterion, reporting Euclidean merge height).</summary>
    | Ward

/// <summary>A binary hierarchical clustering tree whose leaves carry identifiers and internal merges record merge height and leaf count.</summary>
[<RequireQualifiedAccess>]
type Dendrogram<'Id> =
    /// <summary>A singleton cluster containing one observation identifier.</summary>
    | Leaf of id: 'Id
    /// <summary>A merged cluster joining <c>left</c> and <c>right</c> subtrees at nonnegative <c>height</c> with total leaf <c>count</c>.</summary>
    | Merge of left: Dendrogram<'Id> * right: Dendrogram<'Id> * height: float * count: int

/// <summary>Density-based cluster membership for a single item; noise outliers receive <c>ClusterId = None</c>.</summary>
type DensityClusterAssignment<'Id> =
    {
        /// <summary>Identifier extracted from the input item.</summary>
        Id: 'Id
        /// <summary>Zero-based cluster index ordered by earliest member, or <c>None</c> for noise outliers.</summary>
        ClusterId: int option
        /// <summary>True when the item's inclusive epsilon-neighborhood contains at least <c>minPoints</c> items.</summary>
        IsCore: bool
    }

module internal SeriationNumeric =
    let toListRev<'T> (buf: ResizeArray<'T>) : 'T list =
        let mutable acc = []
        let mutable i = buf.Count - 1
        while i >= 0 do
            acc <- buf[i] :: acc
            i <- i - 1
        acc

    let arrayToList<'T> (arr: 'T array) : 'T list =
        let mutable acc = []
        let mutable i = arr.Length - 1
        while i >= 0 do
            acc <- arr[i] :: acc
            i <- i - 1
        acc

    let dotProduct (a: float array) (b: float array) (len: int) : float =
        let mutable s = 0.
        for i in 0 .. len - 1 do
            s <- s + a[i] * b[i]
        s

    let norm2 (v: float array) (len: int) : float =
        let mutable maxAbs = 0.
        for i in 0 .. len - 1 do
            let a = abs v[i]
            if a > maxAbs then maxAbs <- a
        if maxAbs = 0. then
            0.
        elif maxAbs > 1e-140 && maxAbs < 1e140 then
            let mutable s = 0.
            for i in 0 .. len - 1 do
                s <- s + v[i] * v[i]
            sqrt s
        else
            let mutable s = 0.
            for i in 0 .. len - 1 do
                let r = v[i] / maxAbs
                s <- s + r * r
            maxAbs * sqrt s

    let scaleInPlace (v: float array) (len: int) (factor: float) =
        for i in 0 .. len - 1 do
            v[i] <- v[i] * factor

    let projectOutUnit (u: float array) (v: float array) (len: int) =
        for _ in 0 .. 1 do
            let proj = dotProduct u v len
            if proj <> 0. then
                for i in 0 .. len - 1 do
                    v[i] <- v[i] - proj * u[i]

    // Cyclic Jacobi eigensolver for a small symmetric k x k matrix A (k <= 64).
    // Returns eigenvalues diag[0..k-1] and orthogonal eigenvectors V[row * k + col].
    let solveSymmetricJacobi (mat: float array) (k: int) : float array * float array =
        let a = Array.copy mat
        let v = Array.zeroCreate<float> (k * k)
        for i in 0 .. k - 1 do
            v[i * k + i] <- 1.
        let mutable sweep = 0
        let mutable active = k > 1
        while active && sweep < 30 do
            sweep <- sweep + 1
            let mutable changed = false
            for p in 0 .. k - 2 do
                for q in p + 1 .. k - 1 do
                    let apq = a[p * k + q]
                    let app = a[p * k + p]
                    let aqq = a[q * k + q]
                    let scale = abs app + abs aqq
                    if abs apq > 1e-15 * max 1e-300 scale then
                        changed <- true
                        let tau = (aqq - app) / (2. * apq)
                        let t =
                            if tau >= 0. then
                                1. / (tau + sqrt (1. + tau * tau))
                            else
                                -1. / (-tau + sqrt (1. + tau * tau))
                        let c = 1. / sqrt (1. + t * t)
                        let s = t * c
                        a[p * k + p] <- app - t * apq
                        a[q * k + q] <- aqq + t * apq
                        a[p * k + q] <- 0.
                        a[q * k + p] <- 0.
                        for r in 0 .. k - 1 do
                            if r <> p && r <> q then
                                let arp = a[r * k + p]
                                let arq = a[r * k + q]
                                let nrp = c * arp - s * arq
                                let nrq = s * arp + c * arq
                                a[r * k + p] <- nrp
                                a[p * k + r] <- nrp
                                a[r * k + q] <- nrq
                                a[q * k + r] <- nrq
                        for r in 0 .. k - 1 do
                            let vrp = v[r * k + p]
                            let vrq = v[r * k + q]
                            v[r * k + p] <- c * vrp - s * vrq
                            v[r * k + q] <- s * vrp + c * vrq
            if not changed then
                active <- false
        let evals = Array.init k (fun i -> a[i * k + i])
        evals, v

    // Computes the dominant unit eigenvector of a symmetric PSD operator M on R^m restricted to u1^perp,
    // using full-reorthogonalized Lanczos with restarts.
    let dominantDeflatedEigenvector
        (m: int)
        (u1: float array)
        (applyOp: float array -> float array -> unit)
        : float array * float =
        let q0 = Array.zeroCreate<float> m
        for i in 0 .. m - 1 do
            q0[i] <-
                (float i - 0.5 * float (m - 1)) / float m
                + 1e-4 * sin (float (i + 1) * 2.399963229728653)
        projectOutUnit u1 q0 m
        let mutable n0 = norm2 q0 m
        if n0 <= 1e-15 then
            // Fallback basis search if q0 happened to align with u1
            let mutable picked = false
            let mutable idx = 0
            while not picked && idx < m do
                Array.fill q0 0 m 0.
                q0[idx] <- 1.
                projectOutUnit u1 q0 m
                n0 <- norm2 q0 m
                if n0 > 1e-8 then
                    picked <- true
                idx <- idx + 1
        if n0 > 0. then
            scaleInPlace q0 m (1. / n0)
        let kMax = min (m - 1) 48
        let maxCycles = if m <= 49 then 1 else 6
        let basis = Array.init kMax (fun _ -> Array.zeroCreate<float> m)
        let alpha = Array.zeroCreate<float> kMax
        let beta = Array.zeroCreate<float> kMax
        let work = Array.zeroCreate<float> m
        let ritzVec = Array.copy q0
        let mutable ritzVal = 0.
        let mutable cycle = 0
        let mutable converged = kMax <= 0
        while not converged && cycle < maxCycles do
            cycle <- cycle + 1
            Array.Copy(ritzVec, 0, basis[0], 0, m)
            let mutable steps = 0
            let mutable breakdown = false
            while not breakdown && steps < kMax do
                let j = steps
                applyOp basis[j] work
                projectOutUnit u1 work m
                if j > 0 then
                    let bPrev = beta[j - 1]
                    for i in 0 .. m - 1 do
                        work[i] <- work[i] - bPrev * basis[j - 1][i]
                let mutable aj = dotProduct basis[j] work m
                for i in 0 .. m - 1 do
                    work[i] <- work[i] - aj * basis[j][i]
                // Full two-pass Gram-Schmidt reorthogonalization against basis[0..j] and u1
                for pass in 0 .. 1 do
                    for prev in 0 .. j do
                        let h = dotProduct basis[prev] work m
                        if h <> 0. then
                            if pass = 0 && prev = j then
                                aj <- aj + h
                            for i in 0 .. m - 1 do
                                work[i] <- work[i] - h * basis[prev][i]
                    projectOutUnit u1 work m
                alpha[j] <- aj
                let bj = norm2 work m
                steps <- j + 1
                if bj <= 1e-13 * (1. + abs aj) || steps = kMax then
                    beta[j] <- bj
                    if bj <= 1e-13 * (1. + abs aj) then
                        breakdown <- true
                else
                    beta[j] <- bj
                    let invB = 1. / bj
                    let nextQ = basis[j + 1]
                    for i in 0 .. m - 1 do
                        nextQ[i] <- work[i] * invB
            let tri = Array.zeroCreate<float> (steps * steps)
            for j in 0 .. steps - 1 do
                tri[j * steps + j] <- alpha[j]
                if j + 1 < steps then
                    tri[j * steps + (j + 1)] <- beta[j]
                    tri[(j + 1) * steps + j] <- beta[j]
            let evals, evecs = solveSymmetricJacobi tri steps
            let mutable bestCol = 0
            let mutable bestEval = evals[0]
            for c in 1 .. steps - 1 do
                if evals[c] > bestEval then
                    bestEval <- evals[c]
                    bestCol <- c
            Array.fill ritzVec 0 m 0.
            for j in 0 .. steps - 1 do
                let coeff = evecs[j * steps + bestCol]
                let qj = basis[j]
                for i in 0 .. m - 1 do
                    ritzVec[i] <- ritzVec[i] + coeff * qj[i]
            projectOutUnit u1 ritzVec m
            let rn = norm2 ritzVec m
            if rn > 0. then
                scaleInPlace ritzVec m (1. / rn)
            applyOp ritzVec work
            projectOutUnit u1 work m
            ritzVal <- dotProduct ritzVec work m
            let mutable resSq = 0.
            for i in 0 .. m - 1 do
                let diff = work[i] - ritzVal * ritzVec[i]
                resSq <- resSq + diff * diff
            if sqrt resSq <= 1e-10 * (1. + abs ritzVal) || breakdown then
                converged <- true
        ritzVec, ritzVal

    let validateGraph
        (nodes: string list)
        (edges: (string * string * float) list)
        : Result<string array * Dictionary<string, int> * ResizeArray<struct(int * int * float)>, LayoutError list> =
        let errors = ResizeArray<LayoutError>()
        let nodeMap = Dictionary<string, int>(StringComparer.Ordinal)
        let nodeList = ResizeArray<string>()
        for id in nodes do
            if String.IsNullOrWhiteSpace id then
                errors.Add(InvalidObservation "Node ID must not be empty.")
            elif nodeMap.ContainsKey id then
                errors.Add(DuplicateId id)
            else
                nodeMap.Add(id, nodeList.Count)
                nodeList.Add id
        let validEdges = ResizeArray<struct(int * int * float)>()
        for (u, v, w) in edges do
            let hasU = not (isNull u) && nodeMap.ContainsKey u
            let hasV = not (isNull v) && nodeMap.ContainsKey v
            if not hasU then
                errors.Add(MissingEndpoint(if isNull u then "" else u))
            if not hasV then
                errors.Add(MissingEndpoint(if isNull v then "" else v))
            if not (Double.IsFinite w) || w < 0. then
                errors.Add(InvalidObservation "Edge weight must be finite and nonnegative.")
            elif hasU && hasV && w > 0. then
                let iu = nodeMap[u]
                let iv = nodeMap[v]
                if iu <> iv then
                    validEdges.Add(struct(iu, iv, w))
        if errors.Count > 0 then
            Error(toListRev errors)
        else
            Ok(nodeList.ToArray(), nodeMap, validEdges)

    let validateBipartite
        (rows: string list)
        (cols: string list)
        (entries: (string * string * float) list)
        (extraErrors: ResizeArray<LayoutError>)
        : Result<string array * string array * ResizeArray<struct(int * int * float)>, LayoutError list> =
        let rowMap = Dictionary<string, int>(StringComparer.Ordinal)
        let rowList = ResizeArray<string>()
        for r in rows do
            if String.IsNullOrWhiteSpace r then
                extraErrors.Add(InvalidObservation "Row ID must not be empty.")
            elif rowMap.ContainsKey r then
                extraErrors.Add(DuplicateId r)
            else
                rowMap.Add(r, rowList.Count)
                rowList.Add r
        let colMap = Dictionary<string, int>(StringComparer.Ordinal)
        let colList = ResizeArray<string>()
        for c in cols do
            if String.IsNullOrWhiteSpace c then
                extraErrors.Add(InvalidObservation "Column ID must not be empty.")
            elif colMap.ContainsKey c then
                extraErrors.Add(DuplicateId c)
            else
                colMap.Add(c, colList.Count)
                colList.Add c
        let validEntries = ResizeArray<struct(int * int * float)>()
        for (r, c, w) in entries do
            let hasR = not (isNull r) && rowMap.ContainsKey r
            let hasC = not (isNull c) && colMap.ContainsKey c
            if not hasR then
                extraErrors.Add(MissingEndpoint(if isNull r then "" else r))
            if not hasC then
                extraErrors.Add(MissingEndpoint(if isNull c then "" else c))
            if not (Double.IsFinite w) || w < 0. then
                extraErrors.Add(InvalidObservation "Matrix entry weight must be finite and nonnegative.")
            elif hasR && hasC && w > 0. then
                validEntries.Add(struct(rowMap[r], colMap[c], w))
        if extraErrors.Count > 0 then
            Error(toListRev extraErrors)
        else
            Ok(rowList.ToArray(), colList.ToArray(), validEntries)

    let runBarycenterSweeps
        (sweeps: int)
        (numRows: int)
        (numCols: int)
        (rowAdj: ResizeArray<struct(int * float)> array)
        (colAdj: ResizeArray<struct(int * float)> array)
        (rowDeg: float array)
        (colDeg: float array)
        (rowPerm: int array)
        (colPerm: int array)
        =
        if sweeps > 0 && numRows > 0 && numCols > 0 then
            let rowPos = Array.zeroCreate<int> numRows
            let colPos = Array.zeroCreate<int> numCols
            for p in 0 .. numRows - 1 do
                rowPos[rowPerm[p]] <- p
            for p in 0 .. numCols - 1 do
                colPos[colPerm[p]] <- p
            let rowWork = Array.zeroCreate<struct(float * int * int)> numRows
            let colWork = Array.zeroCreate<struct(float * int * int)> numCols
            let inline cmpEntry struct(s1: float, p1: int, _) struct(s2: float, p2: int, _) =
                let c = compare s1 s2
                if c <> 0 then c else compare p1 p2
            let mutable iter = 0
            let mutable moving = true
            while moving && iter < sweeps do
                iter <- iter + 1
                let mutable changed = false
                // Column sweep
                for rank in 0 .. numCols - 1 do
                    let c = colPerm[rank]
                    let score =
                        if colDeg[c] > 0. then
                            let adj = colAdj[c]
                            let mutable sumW = 0.
                            for k in 0 .. adj.Count - 1 do
                                let struct(r, w) = adj[k]
                                sumW <- sumW + w * float rowPos[r]
                            sumW / colDeg[c]
                        else
                            float rank
                    colWork[rank] <- struct(score, rank, c)
                Array.sortInPlaceWith cmpEntry colWork
                for rank in 0 .. numCols - 1 do
                    let struct(_, _, c) = colWork[rank]
                    if colPerm[rank] <> c then
                        changed <- true
                    colPerm[rank] <- c
                    colPos[c] <- rank
                // Row sweep
                for rank in 0 .. numRows - 1 do
                    let r = rowPerm[rank]
                    let score =
                        if rowDeg[r] > 0. then
                            let adj = rowAdj[r]
                            let mutable sumW = 0.
                            for k in 0 .. adj.Count - 1 do
                                let struct(c, w) = adj[k]
                                sumW <- sumW + w * float colPos[c]
                            sumW / rowDeg[r]
                        else
                            float rank
                    rowWork[rank] <- struct(score, rank, r)
                Array.sortInPlaceWith cmpEntry rowWork
                for rank in 0 .. numRows - 1 do
                    let struct(_, _, r) = rowWork[rank]
                    if rowPerm[rank] <> r then
                        changed <- true
                    rowPerm[rank] <- r
                    rowPos[r] <- rank
                if not changed then
                    moving <- false

/// <summary>Matrix and graph seriation, agglomerative dendrogram construction, exact optimal leaf ordering and bipartite co-clustering.</summary>
[<RequireQualifiedAccess>]
module Seriation =
    /// <summary>Computes a bandwidth-reducing Reverse Cuthill-McKee node ordering using a Gibbs-Poole-Stockmeyer pseudo-peripheral root per connected component.</summary>
    let reverseCuthillMcKee
        (nodes: string list)
        (edges: (string * string * float) list)
        : Result<string list, LayoutError list> =
        match SeriationNumeric.validateGraph nodes edges with
        | Error errs -> Error errs
        | Ok(nodeNames, _, validEdges) ->
            let n = nodeNames.Length
            if n <= 1 then
                Ok(SeriationNumeric.arrayToList nodeNames)
            else
                let rawSets = Array.init n (fun _ -> HashSet<int>())
                for idx in 0 .. validEdges.Count - 1 do
                    let struct(u, v, _) = validEdges[idx]
                    rawSets[u].Add v |> ignore
                    rawSets[v].Add u |> ignore
                let deg = Array.init n (fun i -> rawSets[i].Count)
                let adj =
                    Array.init n (fun i ->
                        let arr = Seq.toArray rawSets[i]
                        Array.sortInPlaceWith
                            (fun a b ->
                                let c = compare deg[a] deg[b]
                                if c <> 0 then c else compare a b)
                            arr
                        arr)
                let compVisited = Array.zeroCreate<bool> n
                let dist = Array.create n -1
                let bfsQueue = Array.zeroCreate<int> n
                let output = ResizeArray<string>(n)

                let runLevelBfs (startNode: int) : struct(int * int * ResizeArray<int>) =
                    let mutable head = 0
                    let mutable tail = 0
                    bfsQueue[tail] <- startNode
                    tail <- tail + 1
                    dist[startNode] <- 0
                    let mutable maxDepth = 0
                    while head < tail do
                        let u = bfsQueue[head]
                        head <- head + 1
                        let nextD = dist[u] + 1
                        let nbrs = adj[u]
                        for k in 0 .. nbrs.Length - 1 do
                            let w = nbrs[k]
                            if dist[w] < 0 then
                                dist[w] <- nextD
                                if nextD > maxDepth then
                                    maxDepth <- nextD
                                bfsQueue[tail] <- w
                                tail <- tail + 1
                    let levelWidths = Array.zeroCreate<int> (maxDepth + 1)
                    let lastLayer = ResizeArray<int>()
                    for pos in 0 .. tail - 1 do
                        let v = bfsQueue[pos]
                        let dv = dist[v]
                        levelWidths[dv] <- levelWidths[dv] + 1
                        if dv = maxDepth then
                            lastLayer.Add v
                        dist[v] <- -1
                    let mutable maxWidth = 0
                    for d in 0 .. maxDepth do
                        if levelWidths[d] > maxWidth then
                            maxWidth <- levelWidths[d]
                    struct(maxDepth, maxWidth, lastLayer)

                for seed in 0 .. n - 1 do
                    if not compVisited[seed] then
                        if adj[seed].Length = 0 then
                            compVisited[seed] <- true
                            output.Add nodeNames[seed]
                        else
                            // Collect component and pick lowest-degree starting node
                            let mutable cHead = 0
                            let mutable cTail = 0
                            bfsQueue[cTail] <- seed
                            cTail <- cTail + 1
                            compVisited[seed] <- true
                            let mutable root = seed
                            while cHead < cTail do
                                let u = bfsQueue[cHead]
                                cHead <- cHead + 1
                                if deg[u] < deg[root] || (deg[u] = deg[root] && u < root) then
                                    root <- u
                                let nbrs = adj[u]
                                for k in 0 .. nbrs.Length - 1 do
                                    let w = nbrs[k]
                                    if not compVisited[w] then
                                        compVisited[w] <- true
                                        bfsQueue[cTail] <- w
                                        cTail <- cTail + 1
                            let compSize = cTail
                            // Gibbs-Poole-Stockmeyer pseudo-peripheral search
                            let struct(initEcc, initWidth, initLayer) = runLevelBfs root
                            let mutable bestEcc = initEcc
                            let mutable bestWidth = initWidth
                            let mutable frontier = initLayer
                            let mutable searching = true
                            let mutable gpsIter = 0
                            while searching && gpsIter < 32 do
                                gpsIter <- gpsIter + 1
                                searching <- false
                                let layerArr = frontier.ToArray()
                                Array.sortInPlaceWith
                                    (fun a b ->
                                        let c = compare deg[a] deg[b]
                                        if c <> 0 then c else compare a b)
                                    layerArr
                                // George-Liu shrinking: test one representative per distinct degree (up to 8)
                                let mutable lastDeg = -1
                                let mutable tested = 0
                                let mutable k = 0
                                while not searching && k < layerArr.Length && tested < 8 do
                                    let cand = layerArr[k]
                                    k <- k + 1
                                    if deg[cand] <> lastDeg then
                                        lastDeg <- deg[cand]
                                        tested <- tested + 1
                                        let struct(cEcc, cWidth, cLayer) = runLevelBfs cand
                                        if cEcc > bestEcc then
                                            root <- cand
                                            bestEcc <- cEcc
                                            bestWidth <- cWidth
                                            frontier <- cLayer
                                            searching <- true
                                        elif cEcc = bestEcc && cWidth < bestWidth then
                                            root <- cand
                                            bestWidth <- cWidth
                            // Cuthill-McKee BFS from pseudo-peripheral root (neighbors already sorted by ascending degree)
                            let cmOrder = Array.zeroCreate<int> compSize
                            let seenCm = HashSet<int>()
                            let mutable qHead = 0
                            let mutable qTail = 0
                            cmOrder[0] <- root
                            qTail <- 1
                            seenCm.Add root |> ignore
                            while qHead < qTail do
                                let u = cmOrder[qHead]
                                qHead <- qHead + 1
                                let nbrs = adj[u]
                                for idx in 0 .. nbrs.Length - 1 do
                                    let w = nbrs[idx]
                                    if seenCm.Add w then
                                        cmOrder[qTail] <- w
                                        qTail <- qTail + 1
                            // Reverse Cuthill-McKee order for this component
                            let mutable rIdx = qTail - 1
                            while rIdx >= 0 do
                                output.Add nodeNames[cmOrder[rIdx]]
                                rIdx <- rIdx - 1
                Ok(SeriationNumeric.toListRev output)

    /// <summary>Orders nodes by the Atkins-Boman-Hendrickson (1998) Laplacian Fiedler vector (<c>v_2</c>) per connected component via deflated reorthogonalized Lanczos.</summary>
    let spectralFiedler
        (nodes: string list)
        (edges: (string * string * float) list)
        : Result<string list, LayoutError list> =
        match SeriationNumeric.validateGraph nodes edges with
        | Error errs -> Error errs
        | Ok(nodeNames, _, validEdges) ->
            let n = nodeNames.Length
            if n <= 2 then
                Ok(SeriationNumeric.arrayToList nodeNames)
            else
                let pairWeights = Dictionary<struct(int * int), float>()
                for idx in 0 .. validEdges.Count - 1 do
                    let struct(u, v, w) = validEdges[idx]
                    let a = min u v
                    let b = max u v
                    let key = struct(a, b)
                    match pairWeights.TryGetValue key with
                    | true, cur -> pairWeights[key] <- cur + w
                    | false, _ -> pairWeights[key] <- w
                let adjLists =
                    let bufs = Array.init n (fun _ -> ResizeArray<struct(int * float)>())
                    for kv in pairWeights do
                        let struct(a, b) = kv.Key
                        let w = kv.Value
                        bufs[a].Add(struct(b, w))
                        bufs[b].Add(struct(a, w))
                    Array.init n (fun i ->
                        let arr = bufs[i].ToArray()
                        Array.sortInPlaceWith (fun struct(x, _) struct(y, _) -> compare x y) arr
                        arr)
                let visited = Array.zeroCreate<bool> n
                let compNodes = ResizeArray<int>()
                let localOf = Array.create n -1
                let output = ResizeArray<string>(n)
                for seed in 0 .. n - 1 do
                    if not visited[seed] then
                        compNodes.Clear()
                        let mutable qHead = 0
                        visited[seed] <- true
                        compNodes.Add seed
                        while qHead < compNodes.Count do
                            let u = compNodes[qHead]
                            qHead <- qHead + 1
                            let nbrs = adjLists[u]
                            for k in 0 .. nbrs.Length - 1 do
                                let struct(w, _) = nbrs[k]
                                if not visited[w] then
                                    visited[w] <- true
                                    compNodes.Add w
                        let compArr = compNodes.ToArray()
                        Array.sortInPlace compArr
                        let m = compArr.Length
                        if m <= 2 then
                            for i in 0 .. m - 1 do
                                output.Add nodeNames[compArr[i]]
                        else
                            for i in 0 .. m - 1 do
                                localOf[compArr[i]] <- i
                            // Build CSR sparse adjacency on 0 .. m - 1
                            let rowPtr = Array.zeroCreate<int> (m + 1)
                            let mutable nnz = 0
                            for i in 0 .. m - 1 do
                                rowPtr[i] <- nnz
                                nnz <- nnz + adjLists[compArr[i]].Length
                            rowPtr[m] <- nnz
                            let colIdx = Array.zeroCreate<int> nnz
                            let colVal = Array.zeroCreate<float> nnz
                            let deg = Array.zeroCreate<float> m
                            let mutable maxDeg = 0.
                            for i in 0 .. m - 1 do
                                let nbrs = adjLists[compArr[i]]
                                let baseOff = rowPtr[i]
                                let mutable dSum = 0.
                                for k in 0 .. nbrs.Length - 1 do
                                    let struct(globalNbr, w) = nbrs[k]
                                    colIdx[baseOff + k] <- localOf[globalNbr]
                                    colVal[baseOff + k] <- w
                                    dSum <- dSum + w
                                deg[i] <- dSum
                                if dSum > maxDeg then
                                    maxDeg <- dSum
                            for i in 0 .. m - 1 do
                                localOf[compArr[i]] <- -1
                            let shift = 2. * maxDeg + 1.
                            let invSqrtM = 1. / sqrt (float m)
                            let u1 = Array.create m invSqrtM
                            let applyShifted (x: float array) (y: float array) =
                                for i in 0 .. m - 1 do
                                    let mutable acc = (shift - deg[i]) * x[i]
                                    for p in rowPtr[i] .. rowPtr[i + 1] - 1 do
                                        acc <- acc + colVal[p] * x[colIdx[p]]
                                    y[i] <- acc
                            let fiedler, _ = SeriationNumeric.dominantDeflatedEigenvector m u1 applyShifted
                            // Canonical deterministic orientation aligned with input order
                            let mid = 0.5 * float (m - 1)
                            let mutable corr = 0.
                            for i in 0 .. m - 1 do
                                corr <- corr + (float i - mid) * fiedler[i]
                            let flip =
                                if abs corr > 1e-12 then
                                    corr < 0.
                                else
                                    let mutable decided = false
                                    let mutable neg = false
                                    let mutable idx = 0
                                    while not decided && idx < m do
                                        if abs fiedler[idx] > 1e-12 then
                                            decided <- true
                                            neg <- fiedler[idx] > 0.
                                        idx <- idx + 1
                                    neg
                            if flip then
                                SeriationNumeric.scaleInPlace fiedler m -1.
                            let ranked = Array.init m (fun i -> struct(fiedler[i], compArr[i]))
                            Array.sortInPlaceWith
                                (fun struct(v1, g1) struct(v2, g2) ->
                                    let c = compare v1 v2
                                    if c <> 0 then c else compare g1 g2)
                                ranked
                            for i in 0 .. m - 1 do
                                let struct(_, gIdx) = ranked[i]
                                output.Add nodeNames[gIdx]
                Ok(SeriationNumeric.toListRev output)

    /// <summary>Builds an agglomerative dendrogram via the Lance-Williams recurrence (<c>Single</c>, <c>Complete</c>, <c>Average</c> or <c>Ward</c>); empty input yields <c>Ok None</c>.</summary>
    let hierarchicalCluster<'Id when 'Id: equality>
        (linkage: Linkage)
        (items: 'Id list)
        (distance: 'Id -> 'Id -> float)
        : Result<Dendrogram<'Id> option, LayoutError list> =
        let errors = ResizeArray<LayoutError>()
        let seen = HashSet<'Id>()
        let arrList = ResizeArray<'Id>()
        for item in items do
            let isBlankString =
                match box item with
                | :? string as s -> String.IsNullOrWhiteSpace s
                | _ -> false
            if isBlankString then
                errors.Add(InvalidObservation "Item ID must not be empty.")
            elif not (seen.Add item) then
                let idText = if isNull (box item) then "" else (box item).ToString()
                errors.Add(DuplicateId idText)
            else
                arrList.Add item
        if errors.Count > 0 then
            Error(SeriationNumeric.toListRev errors)
        elif arrList.Count = 0 then
            Ok None
        elif arrList.Count = 1 then
            Ok(Some(Dendrogram.Leaf(arrList[0])))
        else
            let arr = arrList.ToArray()
            let n = arr.Length
            let isWard =
                match linkage with
                | Linkage.Ward -> true
                | _ -> false
            // Store working dissimilarities (d for Single/Complete/Average, d^2 for Ward)
            let workMat = Array.zeroCreate<float> (n * n)
            let mutable distInvalid = false
            for i in 0 .. n - 2 do
                let mutable j = i + 1
                while not distInvalid && j < n do
                    let dab = distance arr[i] arr[j]
                    let dba = distance arr[j] arr[i]
                    if not (Double.IsFinite dab) || not (Double.IsFinite dba) || dab < 0. || dba < 0. then
                        distInvalid <- true
                    else
                        let d = 0.5 * (dab + dba)
                        let stored = if isWard then d * d else d
                        if not (Double.IsFinite stored) then
                            distInvalid <- true
                        else
                            workMat[i * n + j] <- stored
                            workMat[j * n + i] <- stored
                    j <- j + 1
            if distInvalid then
                Error [ InvalidObservation "Pairwise distances must be finite and nonnegative." ]
            else
                let active = Array.create n true
                let sizes = Array.create n 1
                let earliest = Array.init n id
                let trees = Array.init n (fun i -> Dendrogram.Leaf(arr[i]))
                // Nearest active neighbor cache per cluster for O(n^2) total agglomeration
                let nearestIdx = Array.create n -1
                let nearestVal = Array.create n Double.PositiveInfinity
                let recomputeNearest (i: int) =
                    let mutable bestJ = -1
                    let mutable bestV = Double.PositiveInfinity
                    let mutable bestEarly = Int32.MaxValue
                    for j in 0 .. n - 1 do
                        if j <> i && active[j] then
                            let v = workMat[i * n + j]
                            let ej = earliest[j]
                            if v < bestV || (v = bestV && ej < bestEarly) then
                                bestV <- v
                                bestJ <- j
                                bestEarly <- ej
                    nearestIdx[i] <- bestJ
                    nearestVal[i] <- bestV

                for i in 0 .. n - 1 do
                    recomputeNearest i

                for _ in 0 .. n - 2 do
                    let mutable bestA = -1
                    let mutable bestB = -1
                    let mutable bestCost = Double.PositiveInfinity
                    let mutable bestEa = Int32.MaxValue
                    let mutable bestEb = Int32.MaxValue
                    for i in 0 .. n - 1 do
                        if active[i] then
                            let mutable j = nearestIdx[i]
                            if j < 0 || not active[j] then
                                recomputeNearest i
                                j <- nearestIdx[i]
                            let cost = nearestVal[i]
                            let ea = min earliest[i] earliest[j]
                            let eb = max earliest[i] earliest[j]
                            if
                                cost < bestCost
                                || (cost = bestCost && (ea < bestEa || (ea = bestEa && eb < bestEb)))
                            then
                                bestCost <- cost
                                bestEa <- ea
                                bestEb <- eb
                                if earliest[i] <= earliest[j] then
                                    bestA <- i
                                    bestB <- j
                                else
                                    bestA <- j
                                    bestB <- i
                    let na = sizes[bestA]
                    let nb = sizes[bestB]
                    let nMerged = na + nb
                    let mergeHeight = if isWard then sqrt (max 0. bestCost) else max 0. bestCost
                    trees[bestA] <- Dendrogram.Merge(trees[bestA], trees[bestB], mergeHeight, nMerged)
                    sizes[bestA] <- nMerged
                    active[bestB] <- false
                    let fNa = float na
                    let fNb = float nb
                    let fMerged = float nMerged
                    for c in 0 .. n - 1 do
                        if c <> bestA && active[c] then
                            let dac = workMat[bestA * n + c]
                            let dbc = workMat[bestB * n + c]
                            let updated =
                                match linkage with
                                | Linkage.Single -> min dac dbc
                                | Linkage.Complete -> max dac dbc
                                | Linkage.Average -> (fNa * dac + fNb * dbc) / fMerged
                                | Linkage.Ward ->
                                    let fNc = float sizes[c]
                                    max 0. (((fNa + fNc) * dac + (fNb + fNc) * dbc - fNc * bestCost) / (fMerged + fNc))
                            workMat[bestA * n + c] <- updated
                            workMat[c * n + bestA] <- updated
                            if nearestIdx[c] = bestA || nearestIdx[c] = bestB then
                                recomputeNearest c
                            else
                                let curBest = nearestVal[c]
                                let curEarly = if nearestIdx[c] >= 0 then earliest[nearestIdx[c]] else Int32.MaxValue
                                if updated < curBest || (updated = curBest && earliest[bestA] < curEarly) then
                                    nearestIdx[c] <- bestA
                                    nearestVal[c] <- updated
                    recomputeNearest bestA
                let mutable finalIdx = 0
                while finalIdx < n && not active[finalIdx] do
                    finalIdx <- finalIdx + 1
                Ok(Some trees[finalIdx])

    /// <summary>Computes the exact Bar-Joseph, Gifford &amp; Jaakkola (2001) optimal dendrogram leaf ordering in O(n^3) time minimizing adjacent pairwise distances.</summary>
    let optimalLeafOrder (tree: Dendrogram<'Id>) (distance: 'Id -> 'Id -> float) : 'Id list =
        let leavesBuf = ResizeArray<'Id>()
        let rec collectLeaves node =
            match node with
            | Dendrogram.Leaf id -> leavesBuf.Add id
            | Dendrogram.Merge(left, right, _, _) ->
                collectLeaves left
                collectLeaves right
        collectLeaves tree
        let leaves = leavesBuf.ToArray()
        let n = leaves.Length
        if n <= 1 then
            SeriationNumeric.arrayToList leaves
        else
            let dist = Array.zeroCreate<float> (n * n)
            for i in 0 .. n - 1 do
                for j in i + 1 .. n - 1 do
                    let dab = distance leaves[i] leaves[j]
                    let dba = distance leaves[j] leaves[i]
                    Check.finite "distance" dab
                    Check.finite "distance" dba
                    if dab < 0. || dba < 0. then
                        invalidArg "distance" "Distances must be nonnegative."
                    dist[i * n + j] <- dab
                    dist[j * n + i] <- dba
            let dp = Array.create (n * n) Double.PositiveInfinity
            for i in 0 .. n - 1 do
                dp[i * n + i] <- 0.
            let midLeft = Array.zeroCreate<int> (n * n)
            let midRight = Array.zeroCreate<int> (n * n)
            let tempCost = Array.zeroCreate<float> (n * n)
            let tempArg = Array.zeroCreate<int> (n * n)

            let rec solveSubtree (node: Dendrogram<'Id>) (startIdx: int) : int =
                match node with
                | Dendrogram.Leaf _ -> startIdx
                | Dendrogram.Merge(leftNode, rightNode, _, _) ->
                    let a = startIdx
                    let b = solveSubtree leftNode a
                    let r0 = b + 1
                    let c = solveSubtree rightNode r0
                    // Orientation 1: L [a..b] followed by R [r0..c]
                    // Stage 1: tempCost[m, w] = min_{k in R} (dist[m, k] + dp[k, w])
                    for m in a .. b do
                        let mRow = m * n
                        for w in r0 .. c do
                            let mutable bestT = Double.PositiveInfinity
                            let mutable bestK = r0
                            for k in r0 .. c do
                                let dpr = dp[k * n + w]
                                if Double.IsFinite dpr then
                                    let cand = dist[mRow + k] + dpr
                                    if cand < bestT then
                                        bestT <- cand
                                        bestK <- k
                            tempCost[mRow + w] <- bestT
                            tempArg[mRow + w] <- bestK
                    // Stage 2: dp[u, w] = min_{m in L} (dp[u, m] + tempCost[m, w])
                    for u in a .. b do
                        let uRow = u * n
                        for w in r0 .. c do
                            let mutable bestCost = Double.PositiveInfinity
                            let mutable bestM = a
                            for m in a .. b do
                                let dpl = dp[uRow + m]
                                if Double.IsFinite dpl then
                                    let cand = dpl + tempCost[m * n + w]
                                    if cand < bestCost then
                                        bestCost <- cand
                                        bestM <- m
                            dp[uRow + w] <- bestCost
                            midLeft[uRow + w] <- bestM
                            midRight[uRow + w] <- tempArg[bestM * n + w]

                    // Orientation 2: R [r0..c] followed by L [a..b]
                    // Stage 1: tempCost[k, u] = min_{m in L} (dist[k, m] + dp[m, u])
                    for k in r0 .. c do
                        let kRow = k * n
                        for u in a .. b do
                            let mutable bestT = Double.PositiveInfinity
                            let mutable bestM = a
                            for m in a .. b do
                                let dpl = dp[m * n + u]
                                if Double.IsFinite dpl then
                                    let cand = dist[kRow + m] + dpl
                                    if cand < bestT then
                                        bestT <- cand
                                        bestM <- m
                            tempCost[kRow + u] <- bestT
                            tempArg[kRow + u] <- bestM
                    // Stage 2: dp[w, u] = min_{k in R} (dp[w, k] + tempCost[k, u])
                    for w in r0 .. c do
                        let wRow = w * n
                        for u in a .. b do
                            let mutable bestCost = Double.PositiveInfinity
                            let mutable bestK = r0
                            for k in r0 .. c do
                                let dpr = dp[wRow + k]
                                if Double.IsFinite dpr then
                                    let cand = dpr + tempCost[k * n + u]
                                    if cand < bestCost then
                                        bestCost <- cand
                                        bestK <- k
                            dp[wRow + u] <- bestCost
                            midLeft[wRow + u] <- bestK
                            midRight[wRow + u] <- tempArg[bestK * n + u]

                    // Clear intra-L and intra-R DP cells so ancestors only match valid endpoints of [a..c]
                    for x in a .. b do
                        let xRow = x * n
                        for y in a .. b do
                            dp[xRow + y] <- Double.PositiveInfinity
                    for x in r0 .. c do
                        let xRow = x * n
                        for y in r0 .. c do
                            dp[xRow + y] <- Double.PositiveInfinity
                    c

            match tree with
            | Dendrogram.Leaf id -> [ id ]
            | Dendrogram.Merge(leftRoot, _, _, _) ->
                let rec countSub node =
                    match node with
                    | Dendrogram.Leaf _ -> 1
                    | Dendrogram.Merge(l, r, _, _) -> countSub l + countSub r
                let rootSplit = countSub leftRoot - 1
                solveSubtree tree 0 |> ignore
                let mutable bestFirst = 0
                let mutable bestLast = n - 1
                let mutable minTotal = Double.PositiveInfinity
                // Scan unflipped (u in L, w in R) before flipped (w in R, u in L) to preserve tree order on ties
                for u in 0 .. rootSplit do
                    for w in rootSplit + 1 .. n - 1 do
                        let cost = dp[u * n + w]
                        if cost < minTotal then
                            minTotal <- cost
                            bestFirst <- u
                            bestLast <- w
                for w in rootSplit + 1 .. n - 1 do
                    for u in 0 .. rootSplit do
                        let cost = dp[w * n + u]
                        if cost < minTotal then
                            minTotal <- cost
                            bestFirst <- w
                            bestLast <- u
                let ordered = Array.zeroCreate<'Id> n
                let mutable writePos = 0
                let rec reconstruct (node: Dendrogram<'Id>) (startIdx: int) (firstLeaf: int) (lastLeaf: int) : int =
                    match node with
                    | Dendrogram.Leaf _ ->
                        ordered[writePos] <- leaves[firstLeaf]
                        writePos <- writePos + 1
                        startIdx
                    | Dendrogram.Merge(lNode, rNode, _, _) ->
                        let rec subLen nd =
                            match nd with
                            | Dendrogram.Leaf _ -> 1
                            | Dendrogram.Merge(la, ra, _, _) -> subLen la + subLen ra
                        let lLen = subLen lNode
                        let rLen = subLen rNode
                        let b = startIdx + lLen - 1
                        let r0 = b + 1
                        let endIdx = r0 + rLen - 1
                        let inner1 = midLeft[firstLeaf * n + lastLeaf]
                        let inner2 = midRight[firstLeaf * n + lastLeaf]
                        if firstLeaf <= b then
                            reconstruct lNode startIdx firstLeaf inner1 |> ignore
                            reconstruct rNode r0 inner2 lastLeaf |> ignore
                        else
                            reconstruct rNode r0 firstLeaf inner1 |> ignore
                            reconstruct lNode startIdx inner2 lastLeaf |> ignore
                        endIdx
                reconstruct tree 0 bestFirst bestLast |> ignore
                SeriationNumeric.arrayToList ordered

    /// <summary>Reorders matrix <c>rows</c> and <c>cols</c> via <c>sweeps</c> alternating weighted-barycenter iterations over nonnegative <c>entries</c>.</summary>
    let barycenterOrder
        (sweeps: int)
        (rows: string list)
        (cols: string list)
        (entries: (string * string * float) list)
        : Result<string list * string list, LayoutError list> =
        let errors = ResizeArray<LayoutError>()
        if sweeps < 0 then
            errors.Add(InvalidObservation "Sweep count must be nonnegative.")
        match SeriationNumeric.validateBipartite rows cols entries errors with
        | Error errs -> Error errs
        | Ok(rowNames, colNames, validEntries) ->
            let numR = rowNames.Length
            let numC = colNames.Length
            if sweeps = 0 || numR <= 1 && numC <= 1 || validEntries.Count = 0 then
                Ok(SeriationNumeric.arrayToList rowNames, SeriationNumeric.arrayToList colNames)
            else
                let pairMap = Dictionary<struct(int * int), float>()
                for idx in 0 .. validEntries.Count - 1 do
                    let struct(r, c, w) = validEntries[idx]
                    let key = struct(r, c)
                    match pairMap.TryGetValue key with
                    | true, cur -> pairMap[key] <- cur + w
                    | false, _ -> pairMap[key] <- w
                let rowAdj = Array.init numR (fun _ -> ResizeArray<struct(int * float)>())
                let colAdj = Array.init numC (fun _ -> ResizeArray<struct(int * float)>())
                let rowDeg = Array.zeroCreate<float> numR
                let colDeg = Array.zeroCreate<float> numC
                for kv in pairMap do
                    let struct(r, c) = kv.Key
                    let w = kv.Value
                    rowAdj[r].Add(struct(c, w))
                    colAdj[c].Add(struct(r, w))
                    rowDeg[r] <- rowDeg[r] + w
                    colDeg[c] <- colDeg[c] + w
                let rowPerm = Array.init numR id
                let colPerm = Array.init numC id
                SeriationNumeric.runBarycenterSweeps sweeps numR numC rowAdj colAdj rowDeg colDeg rowPerm colPerm
                let outRows = [ for i in 0 .. numR - 1 -> rowNames[rowPerm[i]] ]
                let outCols = [ for j in 0 .. numC - 1 -> colNames[colPerm[j]] ]
                Ok(outRows, outCols)

    /// <summary>Co-clusters and reorders <c>rows</c> and <c>cols</c> via Dhillon (2001) normalized bipartite singular vectors (<c>D_R^-1/2 u_2</c>, <c>D_C^-1/2 v_2</c>) followed by alternating barycenter polish.</summary>
    let bicluster
        (rows: string list)
        (cols: string list)
        (entries: (string * string * float) list)
        : Result<string list * string list, LayoutError list> =
        let errors = ResizeArray<LayoutError>()
        match SeriationNumeric.validateBipartite rows cols entries errors with
        | Error errs -> Error errs
        | Ok(rowNames, colNames, validEntries) ->
            let numR = rowNames.Length
            let numC = colNames.Length
            if numR = 0 || numC = 0 || validEntries.Count = 0 then
                Ok(SeriationNumeric.arrayToList rowNames, SeriationNumeric.arrayToList colNames)
            else
                let pairMap = Dictionary<struct(int * int), float>()
                for idx in 0 .. validEntries.Count - 1 do
                    let struct(r, c, w) = validEntries[idx]
                    let key = struct(r, c)
                    match pairMap.TryGetValue key with
                    | true, cur -> pairMap[key] <- cur + w
                    | false, _ -> pairMap[key] <- w
                let rowAdj = Array.init numR (fun _ -> ResizeArray<struct(int * float)>())
                let colAdj = Array.init numC (fun _ -> ResizeArray<struct(int * float)>())
                let rowDeg = Array.zeroCreate<float> numR
                let colDeg = Array.zeroCreate<float> numC
                for kv in pairMap do
                    let struct(r, c) = kv.Key
                    let w = kv.Value
                    rowAdj[r].Add(struct(c, w))
                    colAdj[c].Add(struct(r, w))
                    rowDeg[r] <- rowDeg[r] + w
                    colDeg[c] <- colDeg[c] + w
                let sortAdjInPlace (buf: ResizeArray<struct(int * float)>) =
                    if buf.Count > 1 then
                        let arr = buf.ToArray()
                        Array.sortInPlaceWith (fun struct(a, _) struct(b, _) -> compare a b) arr
                        buf.Clear()
                        for k in 0 .. arr.Length - 1 do
                            buf.Add arr[k]
                for r in 0 .. numR - 1 do
                    sortAdjInPlace rowAdj[r]
                for c in 0 .. numC - 1 do
                    sortAdjInPlace colAdj[c]
                // Decompose into connected bipartite components so disconnected blocks cleanly separate
                let rowVisited = Array.zeroCreate<bool> numR
                let colVisited = Array.zeroCreate<bool> numC
                let localRowOf = Array.create numR -1
                let localColOf = Array.create numC -1
                let finalRowPerm = ResizeArray<int>(numR)
                let finalColPerm = ResizeArray<int>(numC)

                let orderBipartiteComponent (compRows: int array) (compCols: int array) =
                    let rc = compRows.Length
                    let cc = compCols.Length
                    if rc < 2 || cc < 2 then
                        for i in 0 .. rc - 1 do
                            finalRowPerm.Add compRows[i]
                        for j in 0 .. cc - 1 do
                            finalColPerm.Add compCols[j]
                    else
                        for i in 0 .. rc - 1 do
                            localRowOf[compRows[i]] <- i
                        for j in 0 .. cc - 1 do
                            localColOf[compCols[j]] <- j
                        let lRowAdj = Array.init rc (fun _ -> ResizeArray<struct(int * float)>())
                        let lColAdj = Array.init cc (fun _ -> ResizeArray<struct(int * float)>())
                        let lRowDeg = Array.zeroCreate<float> rc
                        let lColDeg = Array.zeroCreate<float> cc
                        let mutable vol = 0.
                        for i in 0 .. rc - 1 do
                            let gr = compRows[i]
                            lRowDeg[i] <- rowDeg[gr]
                            vol <- vol + rowDeg[gr]
                            let nbrs = rowAdj[gr]
                            for k in 0 .. nbrs.Count - 1 do
                                let struct(gc, w) = nbrs[k]
                                let lc = localColOf[gc]
                                lRowAdj[i].Add(struct(lc, w))
                                lColAdj[lc].Add(struct(i, w))
                        for j in 0 .. cc - 1 do
                            lColDeg[j] <- colDeg[compCols[j]]
                        for i in 0 .. rc - 1 do
                            localRowOf[compRows[i]] <- -1
                        for j in 0 .. cc - 1 do
                            localColOf[compCols[j]] <- -1
                        let invSqrtDr = Array.init rc (fun i -> 1. / sqrt lRowDeg[i])
                        let invSqrtDc = Array.init cc (fun j -> 1. / sqrt lColDeg[j])
                        let zRow = Array.zeroCreate<float> rc
                        let zCol = Array.zeroCreate<float> cc
                        if rc <= cc then
                            let u1 = Array.init rc (fun i -> sqrt (lRowDeg[i] / vol))
                            let tmpC = Array.zeroCreate<float> cc
                            let applyAnAnT (x: float array) (y: float array) =
                                for j in 0 .. cc - 1 do
                                    let nbrs = lColAdj[j]
                                    let mutable s = 0.
                                    for k in 0 .. nbrs.Count - 1 do
                                        let struct(i, w) = nbrs[k]
                                        s <- s + w * (invSqrtDr[i] * x[i])
                                    tmpC[j] <- invSqrtDc[j] * s
                                for i in 0 .. rc - 1 do
                                    let nbrs = lRowAdj[i]
                                    let mutable s = 0.
                                    for k in 0 .. nbrs.Count - 1 do
                                        let struct(j, w) = nbrs[k]
                                        s <- s + w * (invSqrtDc[j] * tmpC[j])
                                    y[i] <- invSqrtDr[i] * s + x[i]
                            let u2, _ = SeriationNumeric.dominantDeflatedEigenvector rc u1 applyAnAnT
                            // v2 proportional to A_n^T * u2
                            let v2Raw = Array.zeroCreate<float> cc
                            for j in 0 .. cc - 1 do
                                let nbrs = lColAdj[j]
                                let mutable s = 0.
                                for k in 0 .. nbrs.Count - 1 do
                                    let struct(i, w) = nbrs[k]
                                    s <- s + w * (invSqrtDr[i] * u2[i])
                                v2Raw[j] <- invSqrtDc[j] * s
                            let vNorm = SeriationNumeric.norm2 v2Raw cc
                            if vNorm > 1e-12 then
                                for i in 0 .. rc - 1 do
                                    zRow[i] <- invSqrtDr[i] * u2[i]
                                let invVn = 1. / vNorm
                                for j in 0 .. cc - 1 do
                                    zCol[j] <- invSqrtDc[j] * (v2Raw[j] * invVn)
                        else
                            let v1 = Array.init cc (fun j -> sqrt (lColDeg[j] / vol))
                            let tmpR = Array.zeroCreate<float> rc
                            let applyAnTAn (x: float array) (y: float array) =
                                for i in 0 .. rc - 1 do
                                    let nbrs = lRowAdj[i]
                                    let mutable s = 0.
                                    for k in 0 .. nbrs.Count - 1 do
                                        let struct(j, w) = nbrs[k]
                                        s <- s + w * (invSqrtDc[j] * x[j])
                                    tmpR[i] <- invSqrtDr[i] * s
                                for j in 0 .. cc - 1 do
                                    let nbrs = lColAdj[j]
                                    let mutable s = 0.
                                    for k in 0 .. nbrs.Count - 1 do
                                        let struct(i, w) = nbrs[k]
                                        s <- s + w * (invSqrtDr[i] * tmpR[i])
                                    y[j] <- invSqrtDc[j] * s + x[j]
                            let v2, _ = SeriationNumeric.dominantDeflatedEigenvector cc v1 applyAnTAn
                            let u2Raw = Array.zeroCreate<float> rc
                            for i in 0 .. rc - 1 do
                                let nbrs = lRowAdj[i]
                                let mutable s = 0.
                                for k in 0 .. nbrs.Count - 1 do
                                    let struct(j, w) = nbrs[k]
                                    s <- s + w * (invSqrtDc[j] * v2[j])
                                u2Raw[i] <- invSqrtDr[i] * s
                            let uNorm = SeriationNumeric.norm2 u2Raw rc
                            if uNorm > 1e-12 then
                                let invUn = 1. / uNorm
                                for i in 0 .. rc - 1 do
                                    zRow[i] <- invSqrtDr[i] * (u2Raw[i] * invUn)
                                for j in 0 .. cc - 1 do
                                    zCol[j] <- invSqrtDc[j] * v2[j]
                        // Orient zRow and zCol consistently with original index order
                        let midR = 0.5 * float (rc - 1)
                        let mutable corrR = 0.
                        for i in 0 .. rc - 1 do
                            corrR <- corrR + (float i - midR) * zRow[i]
                        if corrR < -1e-12 then
                            SeriationNumeric.scaleInPlace zRow rc -1.
                            SeriationNumeric.scaleInPlace zCol cc -1.
                        let rOrder = Array.init rc (fun i -> struct(zRow[i], i))
                        let cOrder = Array.init cc (fun j -> struct(zCol[j], j))
                        let inline cmpRank struct(a1: float, i1: int) struct(a2: float, i2: int) =
                            let c = compare a1 a2
                            if c <> 0 then c else compare i1 i2
                        Array.sortInPlaceWith cmpRank rOrder
                        Array.sortInPlaceWith cmpRank cOrder
                        let lRowPerm =
                            Array.init rc (fun rank ->
                                let struct(_, i) = rOrder[rank]
                                i)
                        let lColPerm =
                            Array.init cc (fun rank ->
                                let struct(_, j) = cOrder[rank]
                                j)
                        // Alternating barycenter polish on the spectral coordinates
                        SeriationNumeric.runBarycenterSweeps 4 rc cc lRowAdj lColAdj lRowDeg lColDeg lRowPerm lColPerm
                        for rank in 0 .. rc - 1 do
                            finalRowPerm.Add compRows[lRowPerm[rank]]
                        for rank in 0 .. cc - 1 do
                            finalColPerm.Add compCols[lColPerm[rank]]

                let qRows = ResizeArray<int>()
                let qCols = ResizeArray<int>()
                for rSeed in 0 .. numR - 1 do
                    if not rowVisited[rSeed] && rowAdj[rSeed].Count > 0 then
                        qRows.Clear()
                        qCols.Clear()
                        rowVisited[rSeed] <- true
                        qRows.Add rSeed
                        let mutable rHead = 0
                        let mutable cHead = 0
                        while rHead < qRows.Count || cHead < qCols.Count do
                            while rHead < qRows.Count do
                                let r = qRows[rHead]
                                rHead <- rHead + 1
                                let nbrs = rowAdj[r]
                                for k in 0 .. nbrs.Count - 1 do
                                    let struct(c, _) = nbrs[k]
                                    if not colVisited[c] then
                                        colVisited[c] <- true
                                        qCols.Add c
                            while cHead < qCols.Count do
                                let c = qCols[cHead]
                                cHead <- cHead + 1
                                let nbrs = colAdj[c]
                                for k in 0 .. nbrs.Count - 1 do
                                    let struct(r, _) = nbrs[k]
                                    if not rowVisited[r] then
                                        rowVisited[r] <- true
                                        qRows.Add r
                        let cRows = qRows.ToArray()
                        let cCols = qCols.ToArray()
                        Array.sortInPlace cRows
                        Array.sortInPlace cCols
                        orderBipartiteComponent cRows cCols
                // Append any isolated zero-degree rows and columns deterministically in original order
                for r in 0 .. numR - 1 do
                    if not rowVisited[r] then
                        finalRowPerm.Add r
                for c in 0 .. numC - 1 do
                    if not colVisited[c] then
                        finalColPerm.Add c
                let outRows = [ for i in 0 .. numR - 1 -> rowNames[finalRowPerm[i]] ]
                let outCols = [ for j in 0 .. numC - 1 -> colNames[finalColPerm[j]] ]
                Ok(outRows, outCols)

module internal DbscanCore =
    let expandClusters<'Id>
        (ids: 'Id array)
        (neighbors: int array array)
        (minPoints: int)
        : DensityClusterAssignment<'Id> list =
        let n = ids.Length
        let isCore = Array.init n (fun i -> neighbors[i].Length >= minPoints)
        let rawCluster = Array.create n -1
        let queue = Array.zeroCreate<int> n
        let inQueue = Array.zeroCreate<bool> n
        let mutable rawCount = 0
        for seed in 0 .. n - 1 do
            if rawCluster[seed] < 0 && isCore[seed] then
                let cid = rawCount
                rawCount <- rawCount + 1
                rawCluster[seed] <- cid
                let mutable qHead = 0
                let mutable qTail = 0
                let seedNbrs = neighbors[seed]
                for k in 0 .. seedNbrs.Length - 1 do
                    let nb = seedNbrs[k]
                    if rawCluster[nb] < 0 && not inQueue[nb] then
                        inQueue[nb] <- true
                        queue[qTail] <- nb
                        qTail <- qTail + 1
                while qHead < qTail do
                    let q = queue[qHead]
                    qHead <- qHead + 1
                    inQueue[q] <- false
                    if rawCluster[q] < 0 then
                        rawCluster[q] <- cid
                        if isCore[q] then
                            let qNbrs = neighbors[q]
                            for k in 0 .. qNbrs.Length - 1 do
                                let nb = qNbrs[k]
                                if rawCluster[nb] < 0 && not inQueue[nb] then
                                    inQueue[nb] <- true
                                    queue[qTail] <- nb
                                    qTail <- qTail + 1
        // Renumber clusters 0 .. rawCount - 1 ordered deterministically by earliest member index
        let remap =
            if rawCount = 0 then
                [||]
            else
                let firstMember = Array.create rawCount Int32.MaxValue
                for i in 0 .. n - 1 do
                    let rc = rawCluster[i]
                    if rc >= 0 && i < firstMember[rc] then
                        firstMember[rc] <- i
                let order = Array.init rawCount (fun c -> struct(firstMember[c], c))
                Array.sortInPlaceWith
                    (fun struct(m1, c1) struct(m2, c2) ->
                        let cmp = compare m1 m2
                        if cmp <> 0 then cmp else compare c1 c2)
                    order
                let mapArr = Array.zeroCreate<int> rawCount
                for newCid in 0 .. rawCount - 1 do
                    let struct(_, oldCid) = order[newCid]
                    mapArr[oldCid] <- newCid
                mapArr
        [ for i in 0 .. n - 1 ->
              let rc = rawCluster[i]
              { Id = ids[i]
                ClusterId = if rc < 0 then None else Some remap[rc]
                IsCore = isCore[i] } ]

/// <summary>Two-dimensional density-based spatial clustering of applications with noise (Ester et al. 1996), accelerated by <c>Spatial.create</c>.</summary>
[<RequireQualifiedAccess>]
module Dbscan2D =
    /// <summary>Partitions 2D <c>items</c> into density-connected clusters within inclusive Euclidean <c>epsilon</c> and minimum neighborhood size <c>minPoints</c>.</summary>
    let cluster<'T, 'Id when 'Id: equality>
        (epsilon: float)
        (minPoints: int)
        (key: 'T -> 'Id)
        (project: 'T -> Point)
        (items: 'T list)
        : Result<DensityClusterAssignment<'Id> list, LayoutError list> =
        let errors = ResizeArray<LayoutError>()
        if not (Double.IsFinite epsilon) || epsilon <= 0. then
            errors.Add(InvalidObservation "DBSCAN epsilon must be finite and positive.")
        if minPoints < 1 then
            errors.Add(InvalidObservation "DBSCAN minPoints must be at least 1.")
        let seen = HashSet<'Id>()
        let idsBuf = ResizeArray<'Id>()
        let ptsBuf = ResizeArray<Point>()
        for item in items do
            let id = key item
            let isBlank =
                match box id with
                | :? string as s -> String.IsNullOrWhiteSpace s
                | _ -> false
            if isBlank then
                errors.Add(InvalidObservation "Item ID must not be empty.")
            elif not (seen.Add id) then
                let idText = if isNull (box id) then "" else (box id).ToString()
                errors.Add(DuplicateId idText)
            let pt = project item
            if not (Double.IsFinite pt.X && Double.IsFinite pt.Y) then
                errors.Add(InvalidObservation "Point coordinates must be finite.")
            else
                idsBuf.Add id
                ptsBuf.Add pt
        if errors.Count > 0 then
            Error(SeriationNumeric.toListRev errors)
        elif items.IsEmpty then
            Ok []
        else
            let ids = idsBuf.ToArray()
            let pts = ptsBuf.ToArray()
            let n = pts.Length
            let mutable minX = pts[0].X
            let mutable maxX = pts[0].X
            let mutable minY = pts[0].Y
            let mutable maxY = pts[0].Y
            for i in 1 .. n - 1 do
                let p = pts[i]
                if p.X < minX then minX <- p.X
                if p.X > maxX then maxX <- p.X
                if p.Y < minY then minY <- p.Y
                if p.Y > maxY then maxY <- p.Y
            let indexedPts = Array.init n (fun i -> struct(i, pts[i]))
            let spatial = Spatial.create (fun struct(_, p) -> p) indexedPts
            let spanFinite = Double.IsFinite (maxX - minX) && Double.IsFinite (maxY - minY)
            let neighbors =
                Array.init n (fun i ->
                    let p = pts[i]
                    if spanFinite then
                        let lx = max minX (p.X - epsilon)
                        let hx = min maxX (p.X + epsilon)
                        let ly = max minY (p.Y - epsilon)
                        let hy = min maxY (p.Y + epsilon)
                        let query = Bounds.create lx ly (max 0. (hx - lx)) (max 0. (hy - ly))
                        let cands = Spatial.within query spatial
                        let hitBuf = ResizeArray<int>()
                        for struct(j, q) in cands do
                            if Double.Hypot(q.X - p.X, q.Y - p.Y) <= epsilon then
                                hitBuf.Add j
                        hitBuf.ToArray()
                    else
                        let hitBuf = ResizeArray<int>()
                        for j in 0 .. n - 1 do
                            let q = pts[j]
                            if Double.Hypot(q.X - p.X, q.Y - p.Y) <= epsilon then
                                hitBuf.Add j
                        hitBuf.ToArray())
            Ok(DbscanCore.expandClusters ids neighbors minPoints)

/// <summary>Three-dimensional density-based spatial clustering of applications with noise (Ester et al. 1996), accelerated by <c>Spatial3D.create</c>.</summary>
[<RequireQualifiedAccess>]
module Dbscan3D =
    /// <summary>Partitions 3D <c>items</c> into density-connected clusters within inclusive Euclidean <c>epsilon</c> and minimum neighborhood size <c>minPoints</c>.</summary>
    let cluster<'T, 'Id when 'Id: equality>
        (epsilon: float)
        (minPoints: int)
        (key: 'T -> 'Id)
        (project: 'T -> Point3)
        (items: 'T list)
        : Result<DensityClusterAssignment<'Id> list, LayoutError list> =
        let errors = ResizeArray<LayoutError>()
        if not (Double.IsFinite epsilon) || epsilon <= 0. then
            errors.Add(InvalidObservation "DBSCAN epsilon must be finite and positive.")
        if minPoints < 1 then
            errors.Add(InvalidObservation "DBSCAN minPoints must be at least 1.")
        let seen = HashSet<'Id>()
        let idsBuf = ResizeArray<'Id>()
        let ptsBuf = ResizeArray<Point3>()
        for item in items do
            let id = key item
            let isBlank =
                match box id with
                | :? string as s -> String.IsNullOrWhiteSpace s
                | _ -> false
            if isBlank then
                errors.Add(InvalidObservation "Item ID must not be empty.")
            elif not (seen.Add id) then
                let idText = if isNull (box id) then "" else (box id).ToString()
                errors.Add(DuplicateId idText)
            let pt = project item
            if not (Double.IsFinite pt.X && Double.IsFinite pt.Y && Double.IsFinite pt.Z) then
                errors.Add(InvalidObservation "Point3 coordinates must be finite.")
            else
                idsBuf.Add id
                ptsBuf.Add pt
        if errors.Count > 0 then
            Error(SeriationNumeric.toListRev errors)
        elif items.IsEmpty then
            Ok []
        else
            let ids = idsBuf.ToArray()
            let pts = ptsBuf.ToArray()
            let n = pts.Length
            let indexedPts = Array.init n (fun i -> struct(i, pts[i]))
            let spatial = Spatial3D.create (fun struct(_, p) -> p) indexedPts
            let neighbors =
                Array.init n (fun i ->
                    let cands = Spatial3D.withinRadius pts[i] epsilon spatial
                    let res = Array.zeroCreate<int> (List.length cands)
                    let mutable w = 0
                    for struct(j, _) in cands do
                        res[w] <- j
                        w <- w + 1
                    res)
            Ok(DbscanCore.expandClusters ids neighbors minPoints)

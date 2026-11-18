namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Edge geometry mode for layered Sugiyama / ELK-style hierarchical diagrams.</summary>
[<RequireQualifiedAccess>]
type SugiyamaRouting =
    /// <summary>Obstacle-avoiding orthogonal paths with port stubs, channel separation and optional corner fillets.</summary>
    | Orthogonal
    /// <summary>Smooth cubic splines through normalized inter-layer waypoints.</summary>
    | CubicSpline
    /// <summary>Piecewise-linear polylines through normalized inter-layer waypoints.</summary>
    | Polyline

/// <summary>Settings for compound, port-aware Sugiyama layered layout (Gansner et al. 1993, Brandes &amp; Köpf 2001, Forster 2004).</summary>
type SugiyamaConfig =
    { /// <summary>Target viewport origin and aspect guide.</summary>
      Bounds: Bounds
      /// <summary>Primary progression direction (<c>TopDown</c>, <c>BottomUp</c>, <c>LeftRight</c> or <c>RightLeft</c>).</summary>
      Direction: LayoutDirection
      /// <summary>Minimum separation between adjacent nodes within the same layer.</summary>
      NodeSpacing: float
      /// <summary>Minimum separation between consecutive layers.</summary>
      LayerSpacing: float
      /// <summary>Extra horizontal/intra-layer separation inserted at compound group boundaries.</summary>
      GroupSpacing: float
      /// <summary>Number of alternating barycenter/median + transpose crossing-minimization sweeps (1..128).</summary>
      CrossingSweeps: int
      /// <summary>Association path routing mode.</summary>
      Routing: SugiyamaRouting
      /// <summary>Orthogonal router parameters used when <c>Routing = SugiyamaRouting.Orthogonal</c>.</summary>
      Orthogonal: OrthogonalRouterConfig }

/// <summary>Constructors for <c>SugiyamaConfig</c>.</summary>
[<RequireQualifiedAccess>]
module SugiyamaConfig =
    /// <summary>Creates a top-down compound Sugiyama configuration with 28-unit node spacing, 56-unit layer spacing and orthogonal routing.</summary>
    let create bounds =
        { Bounds = bounds
          Direction = LayoutDirection.TopDown
          NodeSpacing = 28.
          LayerSpacing = 56.
          GroupSpacing = 24.
          CrossingSweeps = 12
          Routing = SugiyamaRouting.Orthogonal
          Orthogonal = OrthogonalRouterConfig.defaultValue }

/// <summary>Compound, port-aware Sugiyama hierarchical layout engine with tight-tree simplex ranking and 4-pass Brandes-Köpf coordinates.</summary>
[<RequireQualifiedAccess>]
module Sugiyama =
    let private rotatePortToCanonical (dir: LayoutDirection) (side: PortSide) =
        match side with
        | PortSide.Auto -> PortSide.Auto
        | _ ->
            match dir with
            | LayoutDirection.TopDown -> side
            | LayoutDirection.BottomUp ->
                match side with
                | PortSide.North -> PortSide.South
                | PortSide.South -> PortSide.North
                | other -> other
            | LayoutDirection.LeftRight ->
                // World East (+X) becomes canonical South (+Y); World West (-X) becomes canonical North (-Y);
                // World South (+Y) becomes canonical East (+X); World North (-Y) becomes canonical West (-X)
                match side with
                | PortSide.East -> PortSide.South
                | PortSide.West -> PortSide.North
                | PortSide.South -> PortSide.East
                | PortSide.North -> PortSide.West
                | PortSide.Auto -> PortSide.Auto
            | LayoutDirection.RightLeft ->
                match side with
                | PortSide.West -> PortSide.South
                | PortSide.East -> PortSide.North
                | PortSide.South -> PortSide.East
                | PortSide.North -> PortSide.West
                | PortSide.Auto -> PortSide.Auto

    /// <summary>Phase 2: Longest-path initialization + Gansner-style bidirectional slack tightening over the acyclic component DAG.</summary>
    let private assignRanks
        (subN: int)
        (kinds: NodeKind[])
        (dagEdges: struct(int * int * int * float)[])
        : int[] =
        let rank = Array.zeroCreate<int> subN
        if subN <= 1 then rank
        else
            let outAdj = Array.init subN (fun _ -> ResizeArray<struct(int * int * float)>())
            let inAdj = Array.init subN (fun _ -> ResizeArray<struct(int * int * float)>())
            let inDeg = Array.zeroCreate<int> subN
            for item in dagEdges do
                let struct(u, v, minLen, w) = item
                let span = max 1 minLen
                outAdj[u].Add(struct(v, span, w))
                inAdj[v].Add(struct(u, span, w))
                inDeg[v] <- inDeg[v] + 1

            // Topological order via Kahn queue
            let q = Queue<int>()
            for i = 0 to subN - 1 do if inDeg[i] = 0 then q.Enqueue i
            let topo = ResizeArray<int>(subN)
            while q.Count > 0 do
                let u = q.Dequeue()
                topo.Add u
                for item in outAdj[u] do
                    let struct(v, _, _) = item
                    let nextR = rank[u] + (let struct(_, sp, _) = item in sp)
                    if nextR > rank[v] then rank[v] <- nextR
                    inDeg[v] <- inDeg[v] - 1
                    if inDeg[v] = 0 then q.Enqueue v

            // Guard in case of any residual cycle from conflicting OrderAlong constraints
            if topo.Count < subN then
                for i = 0 to subN - 1 do
                    if inDeg[i] > 0 then topo.Add i

            let maxRank = rank |> Array.max
            for i = 0 to subN - 1 do
                if kinds[i] = NodeKind.FinalState && outAdj[i].Count = 0 then
                    rank[i] <- maxRank
                elif kinds[i] = NodeKind.InitialState && inAdj[i].Count = 0 then
                    rank[i] <- 0

            // Iterative network-simplex style cut/slack tightening: pull floating sources down toward children and pull interior nodes toward weighted median feasible rank!
            for pass = 0 to 7 do
                if pass % 2 = 0 then
                    for idx = topo.Count - 1 downto 0 do
                        let u = topo[idx]
                        if kinds[u] <> NodeKind.InitialState && outAdj[u].Count > 0 && inAdj[u].Count = 0 then
                            let mutable upper = Int32.MaxValue
                            for item in outAdj[u] do
                                let struct(v, sp, _) = item
                                if rank[v] - sp < upper then upper <- rank[v] - sp
                            if upper < Int32.MaxValue && upper > rank[u] then rank[u] <- upper
                else
                    for idx = 0 to topo.Count - 1 do
                        let u = topo[idx]
                        if kinds[u] <> NodeKind.InitialState && kinds[u] <> NodeKind.FinalState && (inAdj[u].Count > 0 && outAdj[u].Count > 0) then
                            let mutable lo = 0
                            for item in inAdj[u] do
                                let struct(p, sp, _) = item
                                if rank[p] + sp > lo then lo <- rank[p] + sp
                            let mutable hi = Int32.MaxValue
                            for item in outAdj[u] do
                                let struct(c, sp, _) = item
                                if rank[c] - sp < hi then hi <- rank[c] - sp
                            if lo <= hi && hi < Int32.MaxValue then
                                let mutable wSum = 0.
                                let mutable wTarget = 0.
                                for item in inAdj[u] do
                                    let struct(p, sp, w) = item
                                    wSum <- wSum + w
                                    wTarget <- wTarget + w * float (rank[p] + sp)
                                for item in outAdj[u] do
                                    let struct(c, sp, w) = item
                                    wSum <- wSum + w
                                    wTarget <- wTarget + w * float (rank[c] - sp)
                                if wSum > 0. then
                                    let ideal = int (Math.Round(wTarget / wSum))
                                    rank[u] <- max lo (min hi ideal)

            let minR = rank |> Array.min
            if minR > 0 then
                for i = 0 to subN - 1 do rank[i] <- rank[i] - minR
            rank

    /// <summary>Forster-style hierarchical group-contiguous ordering within a single layer.</summary>
    let private sortLayerRespectingGroups
        (hasGroups: bool)
        (layerNodes: int[])
        (bary: float[])
        (nodeGroupChain: string list[])
        : int[] =
        if layerNodes.Length <= 1 then Array.copy layerNodes
        elif not hasGroups then
            let n = layerNodes.Length
            let out = Array.copy layerNodes
            // Stable insertion sort on small layers, index-tie-broken sort on larger layers
            if n <= 32 then
                for i = 1 to n - 1 do
                    let kv = out[i]
                    let kb = bary[kv]
                    let mutable j = i - 1
                    while j >= 0 && bary[out[j]] > kb do
                        out[j + 1] <- out[j]
                        j <- j - 1
                    out[j + 1] <- kv
                out
            else
                let keys = Array.init n (fun i -> bary[out[i]])
                let orig = Array.init n id
                Array.Sort(orig, Comparer<int>.Create(fun a b ->
                    let c = compare keys[a] keys[b]
                    if c <> 0 then c else compare a b))
                Array.init n (fun i -> layerNodes[orig[i]])
        else
            let rec orderSlice (depth: int) (items: int[]) : int[] =
                if items.Length <= 1 then items
                else
                    // Partition into buckets by group ID at `depth` (None = ungrouped leaf item at this level)
                    let groupBuckets = Dictionary<string, ResizeArray<int>>(StringComparer.Ordinal)
                    let entries = ResizeArray<Choice<int, string>>(items.Length)
                    for k = 0 to items.Length - 1 do
                        let v = items[k]
                        let chain = nodeGroupChain[v]
                        if depth >= chain.Length then
                            entries.Add(Choice1Of2 v)
                        else
                            let gid = chain[depth]
                            match groupBuckets.TryGetValue gid with
                            | true, b -> b.Add v
                            | _ ->
                                let b = ResizeArray<int>()
                                b.Add v
                                groupBuckets.Add(gid, b)
                                entries.Add(Choice2Of2 gid)

                    let eCount = entries.Count
                    let scored = Array.zeroCreate<struct(float * int * int[])> eCount
                    for origPos = 0 to eCount - 1 do
                        match entries[origPos] with
                        | Choice1Of2 v -> scored[origPos] <- struct(bary[v], origPos, [| v |])
                        | Choice2Of2 gid ->
                            let sub = groupBuckets[gid].ToArray()
                            let sortedSub = orderSlice (depth + 1) sub
                            let mutable s = 0.
                            for k = 0 to sortedSub.Length - 1 do s <- s + bary[sortedSub[k]]
                            scored[origPos] <- struct(s / float sortedSub.Length, origPos, sortedSub)

                    Array.Sort(scored, Comparer<struct(float * int * int[])>.Create(fun x y ->
                        let struct(ba, pa, _) = x
                        let struct(bb, pb, _) = y
                        let c = compare ba bb
                        if c <> 0 then c else compare pa pb))

                    let out = Array.zeroCreate<int> items.Length
                    let mutable w = 0
                    for k = 0 to eCount - 1 do
                        let struct(_, _, arr) = scored[k]
                        for j = 0 to arr.Length - 1 do
                            out[w] <- arr[j]
                            w <- w + 1
                    out

            orderSlice 0 layerNodes

    /// <summary>Brandes &amp; Köpf (2001) 4-pass O(V + E) horizontal coordinate assignment for heterogeneous node widths and group borders.</summary>
    let private brandesKoepfCoordinates
        (layers: int[][])
        (totalV: int)
        (isDummy: bool[])
        (widths: float[])
        (parentGroupOf: string option[])
        (upNeighbors: int[][])
        (downNeighbors: int[][])
        (nodeSpacing: float)
        (groupSpacing: float)
        : float[] =
        let numLayers = layers.Length
        if totalV = 0 then [||]
        else
            let layerOf = Array.zeroCreate<int> totalV
            let posInLayer = Array.zeroCreate<int> totalV
            for l = 0 to numLayers - 1 do
                let row = layers[l]
                for k = 0 to row.Length - 1 do
                    layerOf[row[k]] <- l
                    posInLayer[row[k]] <- k

            let sepBetween (u: int) (v: int) =
                let gExtra =
                    if parentGroupOf[u] <> parentGroupOf[v]
                       && (parentGroupOf[u].IsSome || parentGroupOf[v].IsSome) then
                        groupSpacing
                    else 0.
                widths[u] * 0.5 + widths[v] * 0.5 + nodeSpacing + gExtra

            // Mark Type-1 conflicts: non-inner segment crossing an inner (dummy-to-dummy) segment between layers l and l+1
            let type1Conflicts = HashSet<uint64>()
            let edgeKey a b =
                let lo, hi = min a b, max a b
                (uint64 lo <<< 32) ||| uint64 (uint32 hi)

            for l = 0 to numLayers - 2 do
                let upper = layers[l]
                let lower = layers[l + 1]
                let mutable k0 = 0
                let mutable scanL = 0
                for l1 = 0 to lower.Length - 1 do
                    let v = lower[l1]
                    let innerPred =
                        if isDummy[v] then
                            upNeighbors[v] |> Array.tryFind (fun u -> isDummy[u])
                        else None
                    if l1 = lower.Length - 1 || innerPred.IsSome then
                        let k1 =
                            match innerPred with
                            | Some u -> posInLayer[u]
                            | None -> upper.Length - 1
                        while scanL <= l1 do
                            let wNode = lower[scanL]
                            for u in upNeighbors[wNode] do
                                let k = posInLayer[u]
                                let isInner = isDummy[u] && isDummy[wNode]
                                if not isInner && (k < k0 || k > k1) then
                                    type1Conflicts.Add(edgeKey u wNode) |> ignore
                            scanL <- scanL + 1
                        k0 <- k1

            // Run one directional Brandes-Köpf pass:
            // vertDown = true (top-to-bottom using upNeighbors) or false (bottom-to-top using downNeighbors)
            // horizLeft = true (left-to-right) or false (right-to-left)
            let singleDirectionPass (vertDown: bool) (horizLeft: bool) : float[] =
                // Flip horizontal positions virtually when horizLeft = false
                let hPos (v: int) =
                    let l = layerOf[v]
                    if horizLeft then posInLayer[v] else (layers[l].Length - 1 - posInLayer[v])

                let root = Array.init totalV id
                let align = Array.init totalV id

                let lStart, lEnd, lStep = if vertDown then 1, numLayers - 1, 1 else numLayers - 2, 0, -1
                let mutable l = lStart
                while (if lStep > 0 then l <= lEnd else l >= lEnd) do
                    let row = layers[l]
                    let orderedRow = if horizLeft then row else Array.rev row
                    let mutable r = -1
                    for k = 0 to orderedRow.Length - 1 do
                        let v = orderedRow[k]
                        let rawNbs = if vertDown then upNeighbors[v] else downNeighbors[v]
                        if rawNbs.Length > 0 then
                            let nbs = Array.copy rawNbs
                            Array.Sort(nbs, Comparer<int>.Create(fun a b -> compare (hPos a) (hPos b)))
                            let d = nbs.Length
                            let m1 = (d - 1) / 2
                            let m2 = d / 2
                            let medians = if m1 = m2 then [| nbs[m1] |] else [| nbs[m1]; nbs[m2] |]
                            let mutable mIdx = 0
                            while mIdx < medians.Length && align[v] = v do
                                let u = medians[mIdx]
                                let conflicted = type1Conflicts.Contains(edgeKey u v)
                                if not conflicted && r < hPos u then
                                    align[u] <- v
                                    root[v] <- root[u]
                                    align[v] <- root[v]
                                    r <- hPos u
                                mIdx <- mIdx + 1
                    l <- l + lStep

                // Horizontal compaction over block DAG:
                // Build block graph over roots: if w sits immediately before v in a layer (in current horizontal direction),
                // then block(root[w]) + sep(w, v) <= block(root[v]).
                let isRoot = Array.zeroCreate<bool> totalV
                for v = 0 to totalV - 1 do
                    if root[v] = v then isRoot[v] <- true

                let blockSucc = Array.init totalV (fun _ -> ResizeArray<struct(int * float)>())
                let blockInDeg = Array.zeroCreate<int> totalV

                for li = 0 to numLayers - 1 do
                    let row = layers[li]
                    let orderedRow = if horizLeft then row else Array.rev row
                    for k = 1 to orderedRow.Length - 1 do
                        let prev = orderedRow[k - 1]
                        let cur = orderedRow[k]
                        let bPrev = root[prev]
                        let bCur = root[cur]
                        if bPrev <> bCur then
                            let sep = sepBetween prev cur
                            blockSucc[bPrev].Add(struct(bCur, sep))
                            blockInDeg[bCur] <- blockInDeg[bCur] + 1

                let blockX = Array.zeroCreate<float> totalV
                let q = Queue<int>()
                for v = 0 to totalV - 1 do
                    if isRoot[v] && blockInDeg[v] = 0 then q.Enqueue v

                let placedRoots = ResizeArray<int>()
                while q.Count > 0 do
                    let b = q.Dequeue()
                    placedRoots.Add b
                    for edge in blockSucc[b] do
                        let struct(nb, sep) = edge
                        if blockX[b] + sep > blockX[nb] then
                            blockX[nb] <- blockX[b] + sep
                        blockInDeg[nb] <- blockInDeg[nb] - 1
                        if blockInDeg[nb] = 0 then q.Enqueue nb

                // Fallback in rare event of a block cycle (caused by non-monotonic multi-segment alignments): resolve layer-by-layer
                if placedRoots.Count < (isRoot |> Array.sumBy (fun f -> if f then 1 else 0)) then
                    for _ = 0 to 3 do
                        for li = 0 to numLayers - 1 do
                            let row = layers[li]
                            let orderedRow = if horizLeft then row else Array.rev row
                            for k = 1 to orderedRow.Length - 1 do
                                let prev, cur = orderedRow[k - 1], orderedRow[k]
                                let bp, bc = root[prev], root[cur]
                                if bp <> bc then
                                    let sep = sepBetween prev cur
                                    if blockX[bp] + sep > blockX[bc] then
                                        blockX[bc] <- blockX[bp] + sep

                // Pull floating source blocks rightward toward their first successor to keep layout compact!
                for idx = placedRoots.Count - 1 downto 0 do
                    let b = placedRoots[idx]
                    if blockSucc[b].Count > 0 then
                        let mutable tight = infinity
                        for edge in blockSucc[b] do
                            let struct(nb, sep) = edge
                            if blockX[nb] - sep < tight then tight <- blockX[nb] - sep
                        if Double.IsFinite tight && tight > blockX[b] then
                            // Only pull if no predecessor pushes b further right (i.e. b had no predecessors or stays feasible)
                            blockX[b] <- max blockX[b] tight

                let x = Array.zeroCreate<float> totalV
                for v = 0 to totalV - 1 do
                    let rx = blockX[root[v]]
                    x[v] <- if horizLeft then rx else -rx
                x

            let candidates =
                [| singleDirectionPass true true
                   singleDirectionPass true false
                   singleDirectionPass false true
                   singleDirectionPass false false |]

            // Shift-align the 4 candidate coordinate assignments to the one with minimum total span
            let minVal = Array.zeroCreate<float> 4
            let maxVal = Array.zeroCreate<float> 4
            let mutable bestIdx = 0
            let mutable bestSpan = infinity
            for k = 0 to 3 do
                let xs = candidates[k]
                let mutable lo = infinity
                let mutable hi = -infinity
                for v = 0 to totalV - 1 do
                    let lEdge = xs[v] - widths[v] * 0.5
                    let rEdge = xs[v] + widths[v] * 0.5
                    if lEdge < lo then lo <- lEdge
                    if rEdge > hi then hi <- rEdge
                minVal[k] <- lo
                maxVal[k] <- hi
                let sp = hi - lo
                if sp < bestSpan then
                    bestSpan <- sp
                    bestIdx <- k

            for k = 0 to 3 do
                if k <> bestIdx then
                    // Left-to-right passes (0, 2) align left edges; right-to-left passes (1, 3) align right edges
                    let delta =
                        if k % 2 = 0 then minVal[bestIdx] - minVal[k]
                        else maxVal[bestIdx] - maxVal[k]
                    let xs = candidates[k]
                    for v = 0 to totalV - 1 do xs[v] <- xs[v] + delta

            // Balanced coordinate = average of the two median values out of the 4 passes
            let finalX = Array.zeroCreate<float> totalV
            let buf = Array.zeroCreate<float> 4
            for v = 0 to totalV - 1 do
                buf[0] <- candidates[0][v]
                buf[1] <- candidates[1][v]
                buf[2] <- candidates[2][v]
                buf[3] <- candidates[3][v]
                Array.Sort(buf)
                finalX[v] <- 0.5 * (buf[1] + buf[2])

            // Post-averaging minimum separation restoration sweep per layer (preserving center of each layer)
            for li = 0 to numLayers - 1 do
                let row = layers[li]
                if row.Length > 1 then
                    let origMean = (row |> Array.sumBy (fun v -> finalX[v])) / float row.Length
                    for k = 1 to row.Length - 1 do
                        let prev, cur = row[k - 1], row[k]
                        let minCur = finalX[prev] + sepBetween prev cur
                        if finalX[cur] < minCur then finalX[cur] <- minCur
                    let newMean = (row |> Array.sumBy (fun v -> finalX[v])) / float row.Length
                    let shift = origMean - newMean
                    for k = 0 to row.Length - 1 do
                        finalX[row[k]] <- finalX[row[k]] + shift

            finalX

    let private buildSmoothSpline (pts: Point[]) : Path =
        if pts.Length = 0 then Path.empty
        elif pts.Length = 1 then Path.ofCommands [ MoveTo pts[0]; LineTo pts[0] ]
        elif pts.Length = 2 then
            let a, b = pts[0], pts[1]
            let dx, dy = abs (b.X - a.X), abs (b.Y - a.Y)
            if dy >= dx then
                let my = (a.Y + b.Y) * 0.5
                Path.ofCommands [ MoveTo a; CubicTo(Point.create a.X my, Point.create b.X my, b) ]
            else
                let mx = (a.X + b.X) * 0.5
                Path.ofCommands [ MoveTo a; CubicTo(Point.create mx a.Y, Point.create mx b.Y, b) ]
        else
            Shape.line (Curve.CatmullRom 0.15) (pts |> Array.map Some |> Array.toList)

    /// <summary>Lays out a compound, port-aware <c>DiagramGraph</c> in layered Sugiyama form with cancellation support.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: SugiyamaConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.NodeSpacing && Double.IsFinite config.LayerSpacing && Double.IsFinite config.GroupSpacing)
           || config.NodeSpacing < 0. || config.LayerSpacing <= 0. || config.GroupSpacing < 0.
           || config.CrossingSweeps < 1 || config.CrossingSweeps > 128 then
            Error [ InvalidObservation "Sugiyama requires finite nonnegative NodeSpacing/GroupSpacing, positive LayerSpacing and 1..128 CrossingSweeps." ]
        else
            let eff = DiagramGraph.effective graph
            let csr = GraphKernels.buildCsr eff
            if csr.NodeCount = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let horizontalFlow =
                    config.Direction = LayoutDirection.LeftRight || config.Direction = LayoutDirection.RightLeft
                // Canonical node dimensions in TopDown frame:
                let canonWidth i = if horizontalFlow then eff.Nodes[i].Size.Height else eff.Nodes[i].Size.Width
                let canonHeight i = if horizontalFlow then eff.Nodes[i].Size.Width else eff.Nodes[i].Size.Height

                let groupParent = Dictionary<string, string option>(StringComparer.Ordinal)
                for g in eff.Groups do groupParent[g.Id] <- g.ParentGroup
                let rec groupChain acc gidOpt =
                    match gidOpt with
                    | None -> acc
                    | Some gid ->
                        let parent = match groupParent.TryGetValue gid with true, p -> p | _ -> None
                        groupChain (gid :: acc) parent
                let fullGroupChain = Array.init csr.NodeCount (fun i -> groupChain [] eff.Nodes[i].ParentGroup)

                let reversedGlobal = GraphKernels.feedbackArcSet eff csr
                let _, components = GraphKernels.connectedComponents eff csr

                let layoutComponent (members: int[]) : DiagramLayout2D =
                    token.ThrowIfCancellationRequested()
                    let subN = members.Length
                    let globalToLocal = Dictionary<int, int>(subN)
                    for li = 0 to subN - 1 do globalToLocal.Add(members[li], li)

                    // Collect component associations
                    let subEdgeIndices = ResizeArray<int>()
                    for e = 0 to csr.EdgeCount - 1 do
                        if globalToLocal.ContainsKey csr.Src[e] && globalToLocal.ContainsKey csr.Dst[e] then
                            subEdgeIndices.Add e

                    let dagEdges = ResizeArray<struct(int * int * int * float)>()
                    for e in subEdgeIndices do
                        let gu, gv = csr.Src[e], csr.Dst[e]
                        if gu <> gv then
                            let lu, lv = globalToLocal[gu], globalToLocal[gv]
                            let minSpan = max 1 csr.MinLen[e]
                            if reversedGlobal[e] then
                                dagEdges.Add(struct(lv, lu, minSpan, csr.Weight[e]))
                            else
                                dagEdges.Add(struct(lu, lv, minSpan, csr.Weight[e]))

                    // Honor OrderAlong constraints along the primary rank axis
                    for c in eff.Constraints do
                        match c with
                        | LayoutConstraint.OrderAlong(ax, bId, aId, _) ->
                            let matchesFlow =
                                (not horizontalFlow && ax = LayoutAxis.Vertical)
                                || (horizontalFlow && ax = LayoutAxis.Horizontal)
                            if matchesFlow then
                                match eff.ReversedIdMap.TryGetValue bId, eff.ReversedIdMap.TryGetValue aId with
                                | (true, gb), (true, ga) when globalToLocal.ContainsKey gb && globalToLocal.ContainsKey ga ->
                                    let lb, la = globalToLocal[gb], globalToLocal[ga]
                                    let forward =
                                        config.Direction = LayoutDirection.TopDown || config.Direction = LayoutDirection.LeftRight
                                    if forward then dagEdges.Add(struct(lb, la, 1, 2.))
                                    else dagEdges.Add(struct(la, lb, 1, 2.))
                                | _ -> ()
                        | _ -> ()

                    let kinds = Array.init subN (fun li -> eff.Nodes[members[li]].Kind)
                    let realRanks = assignRanks subN kinds (dagEdges.ToArray())
                    let maxRank = if subN = 0 then 0 else Array.max realRanks
                    let numLayers = maxRank + 1

                    // Normalization: split edges with rank span > 1 into dummy vertices
                    let dummyWidths = ResizeArray<float>()
                    let dummyLayers = ResizeArray<int>()
                    let dummyGroupChains = ResizeArray<string list>()
                    let dummyParentGroup = ResizeArray<string option>()
                    let edgeChains = Dictionary<int, int[]>() // subEdgeIndex -> array of vertex indices from canonical top to bottom
                    let normEdges = ResizeArray<struct(int * int * float)>() // (upperVertex, lowerVertex, portOffset)

                    let portCanonicalFrac (gNodeIdx: int) (portIdOpt: string option) =
                        match portIdOpt with
                        | None -> 0.
                        | Some pid ->
                            let ports = eff.Nodes[gNodeIdx].Ports
                            match ports |> List.tryFindIndex (fun p -> p.Id = pid) with
                            | None -> 0.
                            | Some idx ->
                                let p = ports[idx]
                                let cSide = rotatePortToCanonical config.Direction p.Side
                                match cSide with
                                | PortSide.West -> -0.35
                                | PortSide.East -> 0.35
                                | _ ->
                                    if ports.Length <= 1 then 0.
                                    else (float idx / float (ports.Length - 1) - 0.5) * 0.5

                    for e in subEdgeIndices do
                        let gu, gv = csr.Src[e], csr.Dst[e]
                        if gu <> gv then
                            let lu, lv = globalToLocal[gu], globalToLocal[gv]
                            let a = eff.Associations[e]
                            let topLocal, botLocal, topGlobal, topPort =
                                if reversedGlobal[e] then lv, lu, gv, a.Target.PortId
                                else lu, lv, gu, a.Source.PortId
                            let rTop, rBot = realRanks[topLocal], realRanks[botLocal]
                            let span = rBot - rTop
                            let pOff = portCanonicalFrac topGlobal topPort
                            if span <= 0 then
                                edgeChains[e] <- [| topLocal; botLocal |]
                            elif span = 1 then
                                edgeChains[e] <- [| topLocal; botLocal |]
                                normEdges.Add(struct(topLocal, botLocal, pOff))
                            else
                                let chain = Array.zeroCreate<int> (span + 1)
                                chain[0] <- topLocal
                                chain[span] <- botLocal
                                // Shared LCA group chain so dummy stays inside common compound group
                                let cTop, cBot = fullGroupChain[gu], fullGroupChain[gv]
                                let mutable commonLen = 0
                                while commonLen < cTop.Length && commonLen < cBot.Length && cTop[commonLen] = cBot[commonLen] do
                                    commonLen <- commonLen + 1
                                let sharedChain = cTop |> List.take commonLen
                                let sharedGroup = if commonLen = 0 then None else Some sharedChain[commonLen - 1]
                                let midStep = span / 2
                                for step = 1 to span - 1 do
                                    let dIdx = subN + dummyWidths.Count
                                    let dWidth =
                                        match a.CenterLabel with
                                        | Some sz when step = midStep ->
                                            (if horizontalFlow then sz.Height else sz.Width) + 8.
                                        | _ -> 6.
                                    dummyWidths.Add dWidth
                                    dummyLayers.Add(rTop + step)
                                    dummyGroupChains.Add sharedChain
                                    dummyParentGroup.Add sharedGroup
                                    chain[step] <- dIdx
                                for step = 0 to span - 1 do
                                    normEdges.Add(struct(chain[step], chain[step + 1], if step = 0 then pOff else 0.))
                                edgeChains[e] <- chain

                    let totalV = subN + dummyWidths.Count
                    let isDummy = Array.init totalV (fun v -> v >= subN)
                    let vWidths = Array.init totalV (fun v -> if v < subN then canonWidth members[v] else dummyWidths[v - subN])
                    let vGroupChains = Array.init totalV (fun v -> if v < subN then fullGroupChain[members[v]] else dummyGroupChains[v - subN])
                    let vParentGroup = Array.init totalV (fun v -> if v < subN then eff.Nodes[members[v]].ParentGroup else dummyParentGroup[v - subN])

                    let layerBuckets = Array.init numLayers (fun _ -> ResizeArray<int>())
                    for v = 0 to subN - 1 do layerBuckets[realRanks[v]].Add v
                    for d = 0 to dummyLayers.Count - 1 do layerBuckets[dummyLayers[d]].Add(subN + d)

                    let layers = layerBuckets |> Array.map (fun b -> b.ToArray())

                    let upAdj = Array.init totalV (fun _ -> ResizeArray<struct(int * float)>())
                    let downAdj = Array.init totalV (fun _ -> ResizeArray<struct(int * float)>())
                    let downPlain = Array.init totalV (fun _ -> ResizeArray<int>())
                    for ne in normEdges do
                        let struct(u, v, pOff) = ne
                        upAdj[v].Add(struct(u, pOff))
                        downAdj[u].Add(struct(v, pOff))
                        downPlain[u].Add v

                    // Phase 4: Crossing minimization (Port-aware Barycenter/Median + Forster group contiguity + O(deg_a * deg_b) Transpose polish)
                    let hasGroups = eff.Groups.Length > 0
                    let pos = Array.create totalV -1
                    let refreshLayerPos li =
                        let row = layers[li]
                        for k = 0 to row.Length - 1 do pos[row[k]] <- k
                    let initBary = Array.init totalV float
                    for li = 0 to numLayers - 1 do
                        layers[li] <- sortLayerRespectingGroups hasGroups layers[li] initBary vGroupChains
                        refreshLayerPos li

                    let bary = Array.zeroCreate<float> totalV
                    let mutable valsBuf = Array.zeroCreate<float> 16
                    let inline computeBaryMed (len: int) =
                        if len = 1 then valsBuf[0]
                        elif len = 2 then 0.5 * (valsBuf[0] + valsBuf[1])
                        else
                            // Sort valsBuf[0 .. len - 1] in-place via insertion sort for small degrees
                            if len <= 24 then
                                for a = 1 to len - 1 do
                                    let kv = valsBuf[a]
                                    let mutable b = a - 1
                                    while b >= 0 && valsBuf[b] > kv do
                                        valsBuf[b + 1] <- valsBuf[b]
                                        b <- b - 1
                                    valsBuf[b + 1] <- kv
                            else
                                Array.Sort(valsBuf, 0, len)
                            let mutable s = 0.
                            for k = 0 to len - 1 do s <- s + valsBuf[k]
                            let mean = s / float len
                            let mid = len >>> 1
                            let med = if (len &&& 1) = 1 then valsBuf[mid] else 0.5 * (valsBuf[mid - 1] + valsBuf[mid])
                            0.5 * mean + 0.5 * med

                    for sweep = 0 to config.CrossingSweeps - 1 do
                        token.ThrowIfCancellationRequested()
                        if sweep % 2 = 0 then
                            for li = 1 to numLayers - 1 do
                                let row = layers[li]
                                for ri = 0 to row.Length - 1 do
                                    let v = row[ri]
                                    let preds = upAdj[v]
                                    let pc = preds.Count
                                    if pc = 0 then
                                        bary[v] <- float pos[v]
                                    else
                                        if valsBuf.Length < pc then valsBuf <- Array.zeroCreate<float> (max pc (valsBuf.Length * 2))
                                        for k = 0 to pc - 1 do
                                            let struct(u, off) = preds[k]
                                            valsBuf[k] <- float pos[u] + off
                                        bary[v] <- computeBaryMed pc
                                layers[li] <- sortLayerRespectingGroups hasGroups row bary vGroupChains
                                refreshLayerPos li
                        else
                            for li = numLayers - 2 downto 0 do
                                let row = layers[li]
                                for ri = 0 to row.Length - 1 do
                                    let u = row[ri]
                                    let succs = downAdj[u]
                                    let sc = succs.Count
                                    if sc = 0 then
                                        bary[u] <- float pos[u]
                                    else
                                        if valsBuf.Length < sc then valsBuf <- Array.zeroCreate<float> (max sc (valsBuf.Length * 2))
                                        for k = 0 to sc - 1 do
                                            let struct(v, off) = succs[k]
                                            valsBuf[k] <- float pos[v] - off
                                        bary[u] <- computeBaryMed sc
                                layers[li] <- sortLayerRespectingGroups hasGroups row bary vGroupChains
                                refreshLayerPos li

                        // Local adjacent-swap crossing reduction via exact O(deg(a) * deg(b)) incident inversion delta (Rocq: sugiyama_adjacent_swap_crossing_delta)
                        if numLayers >= 2 && totalV <= 600 then
                            for li = 0 to numLayers - 1 do
                                let row = layers[li]
                                if row.Length >= 2 then
                                    for k = 0 to row.Length - 2 do
                                        let a, b = row[k], row[k + 1]
                                        if vParentGroup[a] = vParentGroup[b] then
                                            let mutable crossBefore = 0
                                            let mutable crossAfter = 0
                                            if li > 0 then
                                                let uaList = upAdj[a]
                                                let ubList = upAdj[b]
                                                for ia = 0 to uaList.Count - 1 do
                                                    let struct(ua, _) = uaList[ia]
                                                    let pua = pos[ua]
                                                    for ib = 0 to ubList.Count - 1 do
                                                        let struct(ub, _) = ubList[ib]
                                                        let pub = pos[ub]
                                                        if pua > pub then crossBefore <- crossBefore + 1
                                                        elif pua < pub then crossAfter <- crossAfter + 1
                                            if li < numLayers - 1 then
                                                let daList = downPlain[a]
                                                let dbList = downPlain[b]
                                                for ia = 0 to daList.Count - 1 do
                                                    let pva = pos[daList[ia]]
                                                    for ib = 0 to dbList.Count - 1 do
                                                        let pvb = pos[dbList[ib]]
                                                        if pva > pvb then crossBefore <- crossBefore + 1
                                                        elif pva < pvb then crossAfter <- crossAfter + 1
                                            if crossAfter < crossBefore then
                                                row[k] <- b
                                                row[k + 1] <- a
                                                pos[b] <- k
                                                pos[a] <- k + 1

                    let upPlain = Array.init totalV (fun v -> upAdj[v] |> Seq.map (fun (struct(u, _)) -> u) |> Seq.distinct |> Seq.toArray)
                    let dnPlain = Array.init totalV (fun v -> downAdj[v] |> Seq.map (fun (struct(w, _)) -> w) |> Seq.distinct |> Seq.toArray)

                    // Phase 5: 4-Pass Brandes & Köpf horizontal coordinate assignment
                    let canonX =
                        brandesKoepfCoordinates layers totalV isDummy vWidths vParentGroup upPlain dnPlain config.NodeSpacing config.GroupSpacing

                    // Compute canonical Y center per layer accounting for max node height in each layer + compound header padding
                    let layerMaxH = Array.init numLayers (fun li ->
                        let mutable mh = 24.
                        for v in layers[li] do
                            let h = if v < subN then canonHeight members[v] else 8.
                            if h > mh then mh <- h
                        mh)

                    let canonY = Array.zeroCreate<float> numLayers
                    let mutable curY = layerMaxH[0] * 0.5
                    canonY[0] <- curY
                    for li = 1 to numLayers - 1 do
                        // Add extra vertical gap when compound group membership changes across layers
                        let hasGroupTransition =
                            layers[li] |> Array.exists (fun v -> vParentGroup[v].IsSome)
                            || layers[li - 1] |> Array.exists (fun v -> vParentGroup[v].IsSome)
                        let extraGap = if hasGroupTransition && eff.Groups.Length > 0 then config.GroupSpacing * 0.75 else 0.
                        curY <- curY + layerMaxH[li - 1] * 0.5 + config.LayerSpacing + extraGap + layerMaxH[li] * 0.5
                        canonY[li] <- curY

                    let totalDepthSpan = if numLayers = 0 then 0. else canonY[numLayers - 1] + layerMaxH[numLayers - 1] * 0.5

                    // Transform canonical (cx, cy) back to world coordinates for config.Direction
                    let toWorld (cx: float) (cy: float) : Point =
                        match config.Direction with
                        | LayoutDirection.TopDown -> Point.create cx cy
                        | LayoutDirection.BottomUp -> Point.create cx (totalDepthSpan - cy)
                        | LayoutDirection.LeftRight -> Point.create cy cx
                        | LayoutDirection.RightLeft -> Point.create (totalDepthSpan - cy) cx

                    let vLayerOf = Array.zeroCreate<int> totalV
                    for li = 0 to numLayers - 1 do
                        for v in layers[li] do vLayerOf[v] <- li

                    let subNodes = if subN = eff.Nodes.Length then eff.Nodes else Array.init subN (fun li -> eff.Nodes[members[li]])
                    let subGroups = GraphKernels.filterSubGroups eff members
                    let subAssocs = Array.init subEdgeIndices.Count (fun i -> eff.Associations[subEdgeIndices[i]])
                    let subRevFlags = Array.init subEdgeIndices.Count (fun i -> reversedGlobal[subEdgeIndices[i]])
                    let subMap =
                        if subN = eff.Nodes.Length then eff.ReversedIdMap
                        else
                            let m = Dictionary<string, int>(StringComparer.Ordinal)
                            for li = 0 to subN - 1 do m.Add(subNodes[li].Id, li)
                            m
                    let subEff =
                        { Nodes = subNodes
                          Groups = Array.init subGroups.Length (fun gi -> fst subGroups[gi])
                          GroupDepth = Array.init subGroups.Length (fun gi -> snd subGroups[gi])
                          Associations = subAssocs
                          ReversedIdMap = subMap
                          Constraints = [] }

                    let defaultPortSide =
                        match config.Direction with
                        | LayoutDirection.TopDown -> PortSide.South
                        | LayoutDirection.BottomUp -> PortSide.North
                        | LayoutDirection.LeftRight -> PortSide.East
                        | LayoutDirection.RightLeft -> PortSide.West

                    let rawPosNodes =
                        Array.init subN (fun li ->
                            let nd = subNodes[li]
                            let center = nd.Pinned |> Option.defaultValue (toWorld canonX[li] canonY[vLayerOf[li]])
                            let b = Bounds.create (center.X - nd.Size.Width * 0.5) (center.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                            let autoSides =
                                nd.Ports
                                |> List.choose (fun p -> if p.Side = PortSide.Auto then Some(p.Id, defaultPortSide) else None)
                                |> Map.ofList
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = b
                              Center = center
                              ParentGroup = nd.ParentGroup
                              Layer = Some realRanks[li]
                              Ports = GraphKernels.resolveNodePorts nd b autoSides })

                    let posNodes, routed =
                        match config.Routing with
                        | SugiyamaRouting.Orthogonal ->
                            OrthogonalRouter.routeOverNodes token config.Orthogonal subEff rawPosNodes (Some subRevFlags)
                        | SugiyamaRouting.CubicSpline
                        | SugiyamaRouting.Polyline ->
                            // Build routes through normalized dummy waypoints (separating parallel same-pair edges slightly)
                            let pairSeen = Dictionary<string, int>(StringComparer.Ordinal)
                            let pairTotal = Dictionary<string, int>(StringComparer.Ordinal)
                            for a in subAssocs do
                                let k =
                                    if String.CompareOrdinal(a.Source.NodeId, a.Target.NodeId) <= 0 then
                                        a.Source.NodeId + "\u0000" + a.Target.NodeId
                                    else a.Target.NodeId + "\u0000" + a.Source.NodeId
                                pairTotal[k] <- (match pairTotal.TryGetValue k with true, c -> c | _ -> 0) + 1

                            let routes =
                                [ for idx = 0 to subAssocs.Length - 1 do
                                    let ge = subEdgeIndices[idx]
                                    let a = subAssocs[idx]
                                    let rev = subRevFlags[idx]
                                    let lu = subMap[a.Source.NodeId]
                                    let lv = subMap[a.Target.NodeId]
                                    if lu = lv then
                                        // Delegate self-loop to clean cubic loop
                                        let single =
                                            GraphKernels.routeDirectAssociations
                                                { subEff with Associations = [| a |] }
                                                rawPosNodes
                                                (Some [| rev |])
                                        yield single.Head
                                    else
                                        let chain = edgeChains[ge]
                                        let orderedChain = if rev then Array.rev chain else chain
                                        let k =
                                            if String.CompareOrdinal(a.Source.NodeId, a.Target.NodeId) <= 0 then
                                                a.Source.NodeId + "\u0000" + a.Target.NodeId
                                            else a.Target.NodeId + "\u0000" + a.Source.NodeId
                                        let slot =
                                            match pairSeen.TryGetValue k with
                                            | true, c -> pairSeen[k] <- c + 1; c
                                            | _ -> pairSeen.Add(k, 1); 0
                                        let parCount = pairTotal[k]
                                        let offset =
                                            if parCount <= 1 then 0.
                                            else (float slot - float (parCount - 1) * 0.5) * 14.

                                        let interiorPts =
                                            [| for ci = 1 to orderedChain.Length - 2 do
                                                let v = orderedChain[ci]
                                                yield toWorld (canonX[v] + offset) canonY[vLayerOf[v]] |]

                                        let uNode, vNode = rawPosNodes[lu], rawPosNodes[lv]
                                        let firstAim = if interiorPts.Length > 0 then interiorPts[0] else vNode.Center
                                        let lastAim = if interiorPts.Length > 0 then interiorPts[interiorPts.Length - 1] else uNode.Center
                                        let pStart =
                                            a.Source.PortId
                                            |> Option.bind (fun pid -> uNode.Ports |> List.tryFind (fun p -> p.Id = pid))
                                            |> Option.map (fun p -> p.Anchor)
                                            |> Option.defaultValue (GraphKernels.clipRectBoundary uNode.Bounds firstAim)
                                        let pEnd =
                                            a.Target.PortId
                                            |> Option.bind (fun pid -> vNode.Ports |> List.tryFind (fun p -> p.Id = pid))
                                            |> Option.map (fun p -> p.Anchor)
                                            |> Option.defaultValue (GraphKernels.clipRectBoundary vNode.Bounds lastAim)

                                        let wps =
                                            if interiorPts.Length = 0 && parCount > 1 then
                                                let mid = toWorld ((canonX[lu] + canonX[lv]) * 0.5 + offset) ((canonY[vLayerOf[lu]] + canonY[vLayerOf[lv]]) * 0.5)
                                                [| pStart; mid; pEnd |]
                                            else
                                                Array.concat [ [| pStart |]; interiorPts; [| pEnd |] ]

                                        let path =
                                            match config.Routing with
                                            | SugiyamaRouting.CubicSpline -> buildSmoothSpline wps
                                            | _ ->
                                                let cmds = ResizeArray<PathCommand>(wps.Length)
                                                cmds.Add(MoveTo wps[0])
                                                for k = 1 to wps.Length - 1 do cmds.Add(LineTo wps[k])
                                                Path.ofCommands (List.ofSeq cmds)
                                        let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a wps
                                        yield
                                            { Id = a.Id
                                              Source = a.Source
                                              Target = a.Target
                                              Kind = a.Kind
                                              Waypoints = Array.toList wps
                                              Path = path
                                              ReversedInCycle = rev
                                              SourceLabel = sLbl
                                              CenterLabel = mLbl
                                              TargetLabel = tLbl } ]
                            rawPosNodes, routes

                    let groups = GraphKernels.computeGroupBounds subEff (posNodes |> Array.map (fun pn -> pn.Bounds))
                    let nList = Array.toList posNodes
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

                let compLayouts = components |> Array.map layoutComponent
                Ok(BoxPack.packComponentLayouts config.Bounds (max 32. config.NodeSpacing) compLayouts)

    /// <summary>Runs compound Sugiyama layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

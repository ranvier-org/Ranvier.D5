namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Settings for Dwyer-Koren-Marriott IPSep-CoLa constrained stress, flow, grouping and non-overlap layout.</summary>
type ColaConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Ideal Euclidean length of a unit-hop association in layout units.</summary>
      IdealEdgeLength: float
      /// <summary>Minimum clearance margin enforced when preventing rectangle overlaps.</summary>
      NodeMargin: float
      /// <summary>Number of constrained gradient-projection iterations (1..256).</summary>
      Iterations: int
      /// <summary>When true, prevents positioned node rectangles from overlapping.</summary>
      AvoidOverlaps: bool
      /// <summary>Optional directed flow separation (<c>direction, minGap</c>) enforced across non-cyclic directed associations.</summary>
      DirectedFlow: (LayoutDirection * float) option
      /// <summary>When true, routes final associations orthogonally around obstacles instead of straight segments.</summary>
      OrthogonalRouting: bool }

/// <summary>Constructors for <c>ColaConfig</c>.</summary>
[<RequireQualifiedAccess>]
module ColaConfig =
    /// <summary>Creates a constrained layout configuration with 110-unit ideal edges, 12-unit non-overlap margin and 40 iterations.</summary>
    let create bounds =
        { Bounds = bounds
          IdealEdgeLength = 110.
          NodeMargin = 12.
          Iterations = 40
          AvoidOverlaps = true
          DirectedFlow = None
          OrthogonalRouting = false }

/// <summary>1D separation constraint <c>x[Left] + Gap &lt;= x[Right]</c> for VPSC block projection.</summary>
type internal SeparationConstraint =
    struct
        val Left: int
        val Right: int
        val Gap: float
        val Hard: bool
        new(left, right, gap, hard) = { Left = left; Right = right; Gap = gap; Hard = hard }
    end

/// <summary>IPSep-CoLa constrained graph layout engine (Dwyer, Koren &amp; Marriott, IEEE TVCG 2006).</summary>
[<RequireQualifiedAccess>]
module Cola =
    /// <summary>Block-merging Variable Placement with Separation Constraints (VPSC) solver minimizing weighted squared displacement from <c>desired</c>.</summary>
    let internal solveVpsc (desired: float[]) (weights: float[]) (pinned: bool[]) (constraints: ResizeArray<SeparationConstraint>) : float[] =
        let n = desired.Length
        if n = 0 || constraints.Count = 0 then Array.copy desired
        else
            // Sanitize constraint set so cyclic soft constraints cannot prevent termination:
            // keep DAG feasibility by topological/potential check, prioritizing Hard constraints first.
            let ordered = constraints.ToArray()
            Array.Sort(ordered, Comparer<SeparationConstraint>.Create(fun a b ->
                let c = compare b.Hard a.Hard
                if c <> 0 then c
                else
                    let c2 = compare a.Left b.Left
                    if c2 <> 0 then c2 else compare a.Right b.Right))

            let adj = Array.init n (fun _ -> ResizeArray<struct(int * float)>())
            let accepted = ResizeArray<SeparationConstraint>(ordered.Length)
            let visited = Array.create n -1
            let dfsStack = Array.zeroCreate<int> n
            let mutable stamp = 0

            let canReach (start: int) (goal: int) =
                if start = goal then true
                else
                    stamp <- stamp + 1
                    let curStamp = stamp
                    visited[start] <- curStamp
                    let mutable sp = 0
                    dfsStack[sp] <- start
                    sp <- sp + 1
                    let mutable hit = false
                    while sp > 0 && not hit do
                        sp <- sp - 1
                        let u = dfsStack[sp]
                        let list = adj[u]
                        let mutable k = 0
                        while k < list.Count && not hit do
                            let struct(v, _) = list[k]
                            if v = goal then hit <- true
                            elif visited[v] <> curStamp then
                                visited[v] <- curStamp
                                dfsStack[sp] <- v
                                sp <- sp + 1
                            k <- k + 1
                    hit

            for ci = 0 to ordered.Length - 1 do
                let c = ordered[ci]
                if c.Left <> c.Right then
                    // Check if c.Right can already reach c.Left (except exact zero-sum alignment pairs x_a <= x_b and x_b <= x_a)
                    let mutable zeroPair = false
                    if c.Gap = 0. then
                        let rList = adj[c.Right]
                        let mutable k = 0
                        while k < rList.Count && not zeroPair do
                            let struct(t, g) = rList[k]
                            if t = c.Left && g = 0. then zeroPair <- true
                            k <- k + 1
                    if zeroPair then
                        accepted.Add c
                    elif not (canReach c.Right c.Left) then
                        adj[c.Left].Add(struct(c.Right, c.Gap))
                        accepted.Add c

            // Hildreth's exact Dual Coordinate Ascent QP solver:
            // Minimizes 0.5 * sum_i w_i * (x_i - d_i)^2 subject to x[Left] + Gap <= x[Right].
            // Unlike greedy block-merging without tree-splitting, dual multipliers lambda >= 0 split and unsplit diamond/group constraints cleanly!
            let cs = accepted.ToArray()
            let m = cs.Length
            let out = Array.copy desired
            let invW = Array.init n (fun i -> if pinned[i] then 0. else 1. / max 1e-4 weights[i])
            let lambda = Array.zeroCreate<float> m
            let maxPasses = 128
            let mutable pass = 0
            let mutable maxViol = infinity

            while pass < maxPasses && maxViol > 1e-5 do
                maxViol <- 0.
                for ci = 0 to m - 1 do
                    let c = cs[ci]
                    let u, v = c.Left, c.Right
                    let denom = invW[u] + invW[v]
                    if denom > 0. then
                        let viol = (out[u] + c.Gap) - out[v]
                        if viol > maxViol then maxViol <- viol
                        if viol > 1e-8 || lambda[ci] > 0. then
                            let delta = max (-lambda[ci]) (viol / denom)
                            if abs delta > 1e-12 then
                                lambda[ci] <- lambda[ci] + delta
                                out[u] <- out[u] - delta * invW[u]
                                out[v] <- out[v] + delta * invW[v]
                pass <- pass + 1

            for i = 0 to n - 1 do
                if pinned[i] then out[i] <- desired[i]
            out

    /// <summary>Generates orthogonal rectangle non-overlap separation constraints along each overlapping pair's axis of minimum penetration.</summary>
    let internal appendNonOverlapConstraints
        (xs: float[])
        (ys: float[])
        (halfW: float[])
        (halfH: float[])
        (margin: float)
        (xCons: ResizeArray<SeparationConstraint>)
        (yCons: ResizeArray<SeparationConstraint>)
        : unit =
        let n = xs.Length
        if n > 1 then
            // Detect vertices locked onto the same X or Y rail via zero-gap hard alignment constraints so we separate along the free axis.
            let buildRailRoots (cons: ResizeArray<SeparationConstraint>) =
                let parent = Array.init n id
                let rec find a =
                    if parent[a] = a then a
                    else
                        parent[a] <- find parent[a]
                        parent[a]
                let zeroHard = HashSet<uint64>()
                for c in cons do
                    if c.Hard && c.Gap = 0. && c.Left < n && c.Right < n then
                        let kFwd = (uint64 c.Left <<< 32) ||| uint64 (uint32 c.Right)
                        let kRev = (uint64 c.Right <<< 32) ||| uint64 (uint32 c.Left)
                        zeroHard.Add kFwd |> ignore
                        if zeroHard.Contains kRev then
                            let ra, rb = find c.Left, find c.Right
                            if ra <> rb then parent[ra] <- rb
                for i = 0 to n - 1 do find i |> ignore
                parent

            let railX = buildRailRoots xCons
            let railY = buildRailRoots yCons

            // Sweep-line by left X edge so we avoid O(N^2) checks on spread-out layouts!
            let order = Array.init n id
#if FABLE_COMPILER
            Array.Sort(order, Comparer<int>.Create(fun a b -> compare (xs[a] - halfW[a]) (xs[b] - halfW[b])))
#else
            let leftEdges = Array.init n (fun i -> xs[i] - halfW[i])
            Array.Sort(leftEdges, order)
#endif
            for i = 0 to n - 1 do
                let u = order[i]
                let uxMax = xs[u] + halfW[u] + margin
                let mutable j = i + 1
                while j < n && (xs[order[j]] - halfW[order[j]]) < uxMax do
                    let v = order[j]
                    let reqX = halfW[u] + halfW[v] + margin
                    let reqY = halfH[u] + halfH[v] + margin
                    let dx = xs[v] - xs[u]
                    let dy = ys[v] - ys[u]
                    let penX = reqX - abs dx
                    let penY = reqY - abs dy
                    if penX > 0. && penY > 0. then
                        let lockedX = railX[u] = railX[v]
                        let lockedY = railY[u] = railY[v]
                        let chooseX =
                            if lockedY && not lockedX then true
                            elif lockedX && not lockedY then false
                            else penX <= penY
                        if chooseX then
                            if dx > 0. || (dx = 0. && u < v) then
                                xCons.Add(SeparationConstraint(u, v, reqX, false))
                            else
                                xCons.Add(SeparationConstraint(v, u, reqX, false))
                        else
                            if dy > 0. || (dy = 0. && u < v) then
                                yCons.Add(SeparationConstraint(u, v, reqY, false))
                            else
                                yCons.Add(SeparationConstraint(v, u, reqY, false))
                    j <- j + 1

    /// <summary>Runs IPSep-CoLa constrained layout on <c>graph</c> with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: ColaConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.IdealEdgeLength && Double.IsFinite config.NodeMargin)
           || config.IdealEdgeLength <= 0. || config.NodeMargin < 0.
           || config.Iterations < 1 || config.Iterations > 256 then
            Error [ InvalidObservation "Cola requires positive IdealEdgeLength, nonnegative NodeMargin and 1..256 Iterations." ]
        elif config.DirectedFlow |> Option.exists (fun (_, gap) -> not (Double.IsFinite gap) || gap < 0.) then
            Error [ InvalidObservation "Cola DirectedFlow gap must be finite and nonnegative." ]
        else
            let eff = DiagramGraph.effective graph
            let csr = GraphKernels.buildCsr eff
            let n = csr.NodeCount
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let reversedGlobal = GraphKernels.feedbackArcSet eff csr
                let _, components = GraphKernels.connectedComponents eff csr
                let cx0 = Bounds.x config.Bounds + Bounds.width config.Bounds * 0.5
                let cy0 = Bounds.y config.Bounds + Bounds.height config.Bounds * 0.5

                let layoutComponent (members: int[]) : DiagramLayout2D =
                    token.ThrowIfCancellationRequested()
                    let subN = members.Length
                    let globalToLocal = Dictionary<int, int>(subN)
                    for li = 0 to subN - 1 do globalToLocal.Add(members[li], li)

                    // Initialize via PivotMDS in 2D scaled by IdealEdgeLength
                    let mds = GraphKernels.pivotMdsComponent token csr members 2 (min 32 subN)
                    let xs = Array.init subN (fun li -> cx0 + mds[0][li] * config.IdealEdgeLength)
                    let ys = Array.init subN (fun li -> cy0 + mds[1][li] * config.IdealEdgeLength)
                    let pinnedX = Array.zeroCreate<bool> subN
                    let pinnedY = Array.zeroCreate<bool> subN
                    let weights = Array.init subN (fun li -> eff.Nodes[members[li]].Mass)
                    let halfW = Array.init subN (fun li -> eff.Nodes[members[li]].Size.Width * 0.5)
                    let halfH = Array.init subN (fun li -> eff.Nodes[members[li]].Size.Height * 0.5)

                    for li = 0 to subN - 1 do
                        eff.Nodes[members[li]].Pinned |> Option.iter (fun pt ->
                            xs[li] <- pt.X
                            ys[li] <- pt.Y
                            pinnedX[li] <- true
                            pinnedY[li] <- true)

                    // Build structural separation constraints (FixedPosition, Align, OrderAlong, DirectedFlow)
                    let baseXCons = ResizeArray<SeparationConstraint>()
                    let baseYCons = ResizeArray<SeparationConstraint>()

                    for c in eff.Constraints do
                        match c with
                        | LayoutConstraint.FixedPosition(id, pt) ->
                            match eff.ReversedIdMap.TryGetValue id with
                            | true, gi when globalToLocal.ContainsKey gi ->
                                let li = globalToLocal[gi]
                                xs[li] <- pt.X; ys[li] <- pt.Y
                                pinnedX[li] <- true; pinnedY[li] <- true
                            | _ -> ()
                        | LayoutConstraint.AlignHorizontal ids ->
                            let locals =
                                ids
                                |> List.choose (fun id ->
                                    match eff.ReversedIdMap.TryGetValue id with
                                    | true, gi when globalToLocal.ContainsKey gi -> Some globalToLocal[gi]
                                    | _ -> None)
                            for pair in List.pairwise locals do
                                let a, b = pair
                                baseYCons.Add(SeparationConstraint(a, b, 0., true))
                                baseYCons.Add(SeparationConstraint(b, a, 0., true))
                        | LayoutConstraint.AlignVertical ids ->
                            let locals =
                                ids
                                |> List.choose (fun id ->
                                    match eff.ReversedIdMap.TryGetValue id with
                                    | true, gi when globalToLocal.ContainsKey gi -> Some globalToLocal[gi]
                                    | _ -> None)
                            for pair in List.pairwise locals do
                                let a, b = pair
                                baseXCons.Add(SeparationConstraint(a, b, 0., true))
                                baseXCons.Add(SeparationConstraint(b, a, 0., true))
                        | LayoutConstraint.OrderAlong(ax, bId, aId, gap) ->
                            match eff.ReversedIdMap.TryGetValue bId, eff.ReversedIdMap.TryGetValue aId with
                            | (true, gb), (true, ga) when globalToLocal.ContainsKey gb && globalToLocal.ContainsKey ga ->
                                let lb, la = globalToLocal[gb], globalToLocal[ga]
                                match ax with
                                | LayoutAxis.Horizontal -> baseXCons.Add(SeparationConstraint(lb, la, gap, true))
                                | LayoutAxis.Vertical -> baseYCons.Add(SeparationConstraint(lb, la, gap, true))
                            | _ -> ()

                    config.DirectedFlow |> Option.iter (fun (dir, gap) ->
                        for e = 0 to csr.EdgeCount - 1 do
                            let gu, gv = csr.Src[e], csr.Dst[e]
                            if gu <> gv && not reversedGlobal[e] && globalToLocal.ContainsKey gu && globalToLocal.ContainsKey gv then
                                let lu, lv = globalToLocal[gu], globalToLocal[gv]
                                match dir with
                                | LayoutDirection.TopDown ->
                                    baseYCons.Add(SeparationConstraint(lu, lv, halfH[lu] + halfH[lv] + gap, true))
                                | LayoutDirection.BottomUp ->
                                    baseYCons.Add(SeparationConstraint(lv, lu, halfH[lu] + halfH[lv] + gap, true))
                                | LayoutDirection.LeftRight ->
                                    baseXCons.Add(SeparationConstraint(lu, lv, halfW[lu] + halfW[lv] + gap, true))
                                | LayoutDirection.RightLeft ->
                                    baseXCons.Add(SeparationConstraint(lv, lu, halfW[lu] + halfW[lv] + gap, true))
                    )

                    // Build sparse stress pairs: all component edges + pivot pairs!
                    let stressPairs = ResizeArray<struct(int * int * float * float)>()
                    let pivots, pDists = GraphKernels.componentPivotDistances token csr members (min 24 subN)
                    let addedPair = HashSet<uint64>()
                    let addStressPair lu lv hopDist =
                        if lu <> lv && Double.IsFinite hopDist && hopDist > 0. then
                            let a, b = min lu lv, max lu lv
                            let key = (uint64 a <<< 32) ||| uint64 (uint32 b)
                            if addedPair.Add key then
                                let ideal = hopDist * config.IdealEdgeLength + (halfW[a] + halfW[b] + halfH[a] + halfH[b]) * 0.35
                                let w = 1. / max 1. (ideal * ideal)
                                stressPairs.Add(struct(a, b, ideal, w))

                    for li = 0 to subN - 1 do
                        let gu = members[li]
                        for idx = csr.UndirOffsets[gu] to csr.UndirOffsets[gu + 1] - 1 do
                            let gv = csr.UndirTargets[idx]
                            match globalToLocal.TryGetValue gv with
                            | true, lj -> addStressPair li lj 1.
                            | _ -> ()

                    for pi = 0 to pivots.Length - 1 do
                        let lp = pivots[pi]
                        let row = pDists[pi]
                        for lj = 0 to subN - 1 do
                            addStressPair lp lj row[lj]

                    let pairsArr = stressPairs.ToArray()

                    // Group containment variables: each visible group in component gets Left/Right in X and Top/Bottom in Y
                    let subGroupsWithDepth = GraphKernels.filterSubGroups eff members
                    let groupMembers = GraphKernels.subGroupMemberLists eff members subGroupsWithDepth

                    let gCount = subGroupsWithDepth.Length
                    let subGMap = Dictionary<string, int>(StringComparer.Ordinal)
                    for gi = 0 to gCount - 1 do subGMap.Add((fst subGroupsWithDepth[gi]).Id, gi)

                    // Extended VPSC variable layout:
                    // 0 .. subN-1: node centers
                    // subN + 2*gi: group left/top boundary; subN + 2*gi + 1: group right/bottom boundary
                    let totalVars = subN + 2 * gCount
                    let extWeights = Array.init totalVars (fun idx -> if idx < subN then weights[idx] else 0.08)
                    let extPinX = Array.init totalVars (fun idx -> idx < subN && pinnedX[idx])
                    let extPinY = Array.init totalVars (fun idx -> idx < subN && pinnedY[idx])

                    let appendGroupContainment (xCons: ResizeArray<SeparationConstraint>) (yCons: ResizeArray<SeparationConstraint>) =
                        for li = 0 to subN - 1 do
                            match eff.Nodes[members[li]].ParentGroup with
                            | Some gid when subGMap.ContainsKey gid ->
                                let gi = subGMap[gid]
                                let g = fst subGroupsWithDepth[gi]
                                let lVar, rVar = subN + 2 * gi, subN + 2 * gi + 1
                                xCons.Add(SeparationConstraint(lVar, li, g.Padding.Left + halfW[li], true))
                                xCons.Add(SeparationConstraint(li, rVar, halfW[li] + g.Padding.Right, true))
                                yCons.Add(SeparationConstraint(lVar, li, g.Padding.Top + halfH[li], true))
                                yCons.Add(SeparationConstraint(li, rVar, halfH[li] + g.Padding.Bottom, true))
                            | _ -> ()
                        for gi = 0 to gCount - 1 do
                            let g = fst subGroupsWithDepth[gi]
                            let lVar, rVar = subN + 2 * gi, subN + 2 * gi + 1
                            xCons.Add(SeparationConstraint(lVar, rVar, g.MinSize.Width, true))
                            yCons.Add(SeparationConstraint(lVar, rVar, g.MinSize.Height, true))
                            match g.ParentGroup with
                            | Some pid when subGMap.ContainsKey pid ->
                                let pi = subGMap[pid]
                                let pg = fst subGroupsWithDepth[pi]
                                let plVar, prVar = subN + 2 * pi, subN + 2 * pi + 1
                                xCons.Add(SeparationConstraint(plVar, lVar, pg.Padding.Left, true))
                                xCons.Add(SeparationConstraint(rVar, prVar, pg.Padding.Right, true))
                                yCons.Add(SeparationConstraint(plVar, lVar, pg.Padding.Top, true))
                                yCons.Add(SeparationConstraint(rVar, prVar, pg.Padding.Bottom, true))
                            | _ -> ()

                    // Iterative IPSep-CoLa loop: unconstrained stress step -> VPSC projection (with dynamic group & overlap constraints)
                    let desX = Array.zeroCreate<float> totalVars
                    let desY = Array.zeroCreate<float> totalVars
                    let gradX = Array.zeroCreate<float> subN
                    let gradY = Array.zeroCreate<float> subN
                    let diagW = Array.zeroCreate<float> subN

                    for iter = 0 to config.Iterations - 1 do
                        token.ThrowIfCancellationRequested()
                        Array.fill gradX 0 subN 0.
                        Array.fill gradY 0 subN 0.
                        Array.fill diagW 0 subN 0.

                        for p = 0 to pairsArr.Length - 1 do
                            let struct(u, v, ideal, w) = pairsArr[p]
                            let dx = xs[u] - xs[v]
                            let dy = ys[u] - ys[v]
                            let dist = sqrt (dx * dx + dy * dy)
                            let ux, uy =
                                if dist <= 1e-6 then
                                    let ang = float ((u * 31 + v * 17 + iter) % 360) * (Math.PI / 180.)
                                    cos ang, sin ang
                                else dx / dist, dy / dist
                            // Majorization target: u wants v + ideal * (u - v)/|u - v|
                            let txU = xs[v] + ideal * ux
                            let tyU = ys[v] + ideal * uy
                            let txV = xs[u] - ideal * ux
                            let tyV = ys[u] - ideal * uy
                            gradX[u] <- gradX[u] + w * txU
                            gradY[u] <- gradY[u] + w * tyU
                            diagW[u] <- diagW[u] + w
                            gradX[v] <- gradX[v] + w * txV
                            gradY[v] <- gradY[v] + w * tyV
                            diagW[v] <- diagW[v] + w

                        let stepAlpha = if iter < config.Iterations / 2 then 0.85 else 0.65
                        for li = 0 to subN - 1 do
                            if pinnedX[li] then desX[li] <- xs[li]
                            elif diagW[li] > 1e-12 then
                                desX[li] <- xs[li] + stepAlpha * (gradX[li] / diagW[li] - xs[li])
                            else desX[li] <- xs[li]

                            if pinnedY[li] then desY[li] <- ys[li]
                            elif diagW[li] > 1e-12 then
                                desY[li] <- ys[li] + stepAlpha * (gradY[li] / diagW[li] - ys[li])
                            else desY[li] <- ys[li]

                        // Desired group bounds gently pull inward toward member centroid so group boxes stay compact!
                        for gi = 0 to gCount - 1 do
                            let mems = groupMembers[gi]
                            let cnt = mems.Length
                            let mx, my =
                                if cnt = 0 then cx0, cy0
                                else
                                    let mutable sumX, sumY = 0., 0.
                                    for k = 0 to cnt - 1 do
                                        let li = mems[k]
                                        sumX <- sumX + desX[li]
                                        sumY <- sumY + desY[li]
                                    let invC = 1. / float cnt
                                    sumX * invC, sumY * invC
                            desX[subN + 2 * gi] <- mx
                            desX[subN + 2 * gi + 1] <- mx
                            desY[subN + 2 * gi] <- my
                            desY[subN + 2 * gi + 1] <- my

                        let xCons = ResizeArray<SeparationConstraint>(baseXCons)
                        let yCons = ResizeArray<SeparationConstraint>(baseYCons)
                        appendGroupContainment xCons yCons

                        // Start enforcing non-overlap in second half of iterations once global topology unfolds!
                        if config.AvoidOverlaps && iter >= config.Iterations / 3 then
                            appendNonOverlapConstraints xs ys halfW halfH config.NodeMargin xCons yCons
                            // Also keep sibling compound groups from overlapping!
                            if gCount > 1 && iter >= config.Iterations / 2 then
                                // Estimate current group boxes and separate sibling groups along minimum penetration axis
                                let subNodesTmp = members |> Array.map (fun gi -> eff.Nodes[gi])
                                let subMapTmp = Dictionary<string, int>(StringComparer.Ordinal)
                                for li = 0 to subN - 1 do subMapTmp.Add(subNodesTmp[li].Id, li)
                                let tmpEff =
                                    { Nodes = subNodesTmp
                                      Groups = subGroupsWithDepth |> Array.map fst
                                      GroupDepth = subGroupsWithDepth |> Array.map snd
                                      Associations = [||]
                                      ReversedIdMap = subMapTmp
                                      Constraints = [] }
                                let tmpBounds = Array.init subN (fun li -> Bounds.create (xs[li] - halfW[li]) (ys[li] - halfH[li]) (halfW[li] * 2.) (halfH[li] * 2.))
                                let gBoxes = GraphKernels.computeGroupBounds tmpEff tmpBounds |> List.toArray
                                for a = 0 to gBoxes.Length - 1 do
                                    for b = a + 1 to gBoxes.Length - 1 do
                                        if gBoxes[a].ParentGroup = gBoxes[b].ParentGroup then
                                            let ba, bb = gBoxes[a].Bounds, gBoxes[b].Bounds
                                            let cxa, cya = Bounds.x ba + Bounds.width ba * 0.5, Bounds.y ba + Bounds.height ba * 0.5
                                            let cxb, cyb = Bounds.x bb + Bounds.width bb * 0.5, Bounds.y bb + Bounds.height bb * 0.5
                                            let reqX = (Bounds.width ba + Bounds.width bb) * 0.5 + config.NodeMargin
                                            let reqY = (Bounds.height ba + Bounds.height bb) * 0.5 + config.NodeMargin
                                            let penX = reqX - abs (cxb - cxa)
                                            let penY = reqY - abs (cyb - cya)
                                            if penX > 0. && penY > 0. then
                                                let rA, lA = subN + 2 * a + 1, subN + 2 * a
                                                let rB, lB = subN + 2 * b + 1, subN + 2 * b
                                                if penX <= penY then
                                                    if cxb >= cxa then xCons.Add(SeparationConstraint(rA, lB, config.NodeMargin, false))
                                                    else xCons.Add(SeparationConstraint(rB, lA, config.NodeMargin, false))
                                                else
                                                    if cyb >= cya then yCons.Add(SeparationConstraint(rA, lB, config.NodeMargin, false))
                                                    else yCons.Add(SeparationConstraint(rB, lA, config.NodeMargin, false))

                        let projX = solveVpsc desX extWeights extPinX xCons
                        let projY = solveVpsc desY extWeights extPinY yCons
                        for li = 0 to subN - 1 do
                            xs[li] <- projX[li]
                            ys[li] <- projY[li]

                    // Final strict non-overlap sweep
                    if config.AvoidOverlaps && subN > 1 then
                        for _ = 0 to 2 do
                            let fx = ResizeArray<SeparationConstraint>(baseXCons)
                            let fy = ResizeArray<SeparationConstraint>(baseYCons)
                            appendGroupContainment fx fy
                            appendNonOverlapConstraints xs ys halfW halfH config.NodeMargin fx fy
                            for li = 0 to subN - 1 do desX[li] <- xs[li]; desY[li] <- ys[li]
                            let px = solveVpsc desX extWeights extPinX fx
                            let py = solveVpsc desY extWeights extPinY fy
                            for li = 0 to subN - 1 do xs[li] <- px[li]; ys[li] <- py[li]

                    let subEff = GraphKernels.sliceComponentEff eff csr members subGroupsWithDepth
                    let subNodes = subEff.Nodes

                    let rawPosNodes =
                        Array.init subN (fun li ->
                            let nd = subNodes[li]
                            let b = Bounds.create (xs[li] - halfW[li]) (ys[li] - halfH[li]) nd.Size.Width nd.Size.Height
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = b
                              Center = Point.create xs[li] ys[li]
                              ParentGroup = nd.ParentGroup
                              Layer = None
                              Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                    let posNodes, routed =
                        if config.OrthogonalRouting then
                            OrthogonalRouter.routeOverNodes token OrthogonalRouterConfig.defaultValue subEff rawPosNodes None
                        else
                            rawPosNodes, GraphKernels.routeDirectAssociations subEff rawPosNodes None

                    let groups = GraphKernels.computeGroupBounds subEff (posNodes |> Array.map (fun pn -> pn.Bounds))
                    let nList = Array.toList posNodes
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

                let compLayouts = components |> Array.map layoutComponent
                Ok(BoxPack.packComponentLayouts config.Bounds (max 28. config.NodeMargin) compLayouts)

    /// <summary>Runs IPSep-CoLa constrained layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

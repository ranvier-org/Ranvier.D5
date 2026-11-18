namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Settings for Balcı &amp; Dogrusoz (2022) fCoSE fast compound spring embedder with constraint support.</summary>
type FCoseConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Ideal spring edge length in layout units.</summary>
      IdealEdgeLength: float
      /// <summary>Pairwise / Barnes-Hut node repulsion strength.</summary>
      Repulsion: float
      /// <summary>Central gravity strength pulling components gently toward viewport center.</summary>
      Gravity: float
      /// <summary>Intra-group gravity keeping compound cluster members cohesive.</summary>
      CompoundGravity: float
      /// <summary>Barnes-Hut opening angle theta (0 for exact pairwise, 0.8 default).</summary>
      Theta: float
      /// <summary>Number of compound spring-embedder polish iterations after PivotMDS initialization (1..512).</summary>
      Iterations: int
      /// <summary>Deterministic random seed used for tie-breaking jitter.</summary>
      Seed: uint64
      /// <summary>When true, enforces final rectangle non-overlap and constraint projection.</summary>
      EnforceNonOverlap: bool }

/// <summary>Constructors for <c>FCoseConfig</c>.</summary>
[<RequireQualifiedAccess>]
module FCoseConfig =
    /// <summary>Creates an fCoSE configuration with 95-unit ideal edges, 4500 repulsion, 0.8 Barnes-Hut theta and 80 polish iterations.</summary>
    let create bounds =
        { Bounds = bounds
          IdealEdgeLength = 95.
          Repulsion = 4500.
          Gravity = 0.08
          CompoundGravity = 0.18
          Theta = 0.8
          Iterations = 80
          Seed = 42UL
          EnforceNonOverlap = true }

/// <summary>Fast Compound Spring Embedder (fCoSE, Balcı &amp; Dogrusoz, IEEE TVCG 2022) combining algebraic PivotMDS skeleton placement, compound cluster forces and constraint projection.</summary>
[<RequireQualifiedAccess>]
module FCose =
    /// <summary>Runs fCoSE layout on <c>graph</c> with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: FCoseConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.IdealEdgeLength && Double.IsFinite config.Repulsion
                && Double.IsFinite config.Gravity && Double.IsFinite config.CompoundGravity && Double.IsFinite config.Theta)
           || config.IdealEdgeLength <= 0. || config.Repulsion < 0.
           || config.Gravity < 0. || config.CompoundGravity < 0. || config.Theta < 0.
           || config.Iterations < 1 || config.Iterations > 512 then
            Error [ InvalidObservation "FCose requires positive IdealEdgeLength, nonnegative Repulsion/Gravity/Theta and 1..512 Iterations." ]
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
                    let globalToLocal = Dictionary<int, int>(subN)
                    for li = 0 to subN - 1 do globalToLocal.Add(members[li], li)

                    // Phase 1: Algebraic PivotMDS skeleton initialization (including virtual intra-group cohesion links via component CSR)
                    let mds = GraphKernels.pivotMdsComponent token csr members 2 (min 32 subN)
                    let xs = Array.init subN (fun li -> cx0 + mds[0][li] * config.IdealEdgeLength)
                    let ys = Array.init subN (fun li -> cy0 + mds[1][li] * config.IdealEdgeLength)
                    let pinnedX = Array.zeroCreate<bool> subN
                    let pinnedY = Array.zeroCreate<bool> subN
                    let masses = Array.init subN (fun li -> eff.Nodes[members[li]].Mass)
                    let halfW = Array.init subN (fun li -> eff.Nodes[members[li]].Size.Width * 0.5)
                    let halfH = Array.init subN (fun li -> eff.Nodes[members[li]].Size.Height * 0.5)
                    let radii = Array.init subN (fun li -> Double.Hypot(halfW[li], halfH[li]))

                    for li = 0 to subN - 1 do
                        eff.Nodes[members[li]].Pinned |> Option.iter (fun pt ->
                            xs[li] <- pt.X; ys[li] <- pt.Y
                            pinnedX[li] <- true; pinnedY[li] <- true)

                    // Collect component constraints
                    let hardX = ResizeArray<SeparationConstraint>()
                    let hardY = ResizeArray<SeparationConstraint>()
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
                            let locals = ids |> List.choose (fun id -> match eff.ReversedIdMap.TryGetValue id with true, gi when globalToLocal.ContainsKey gi -> Some globalToLocal[gi] | _ -> None)
                            for a, b in List.pairwise locals do
                                hardY.Add(SeparationConstraint(a, b, 0., true))
                                hardY.Add(SeparationConstraint(b, a, 0., true))
                        | LayoutConstraint.AlignVertical ids ->
                            let locals = ids |> List.choose (fun id -> match eff.ReversedIdMap.TryGetValue id with true, gi when globalToLocal.ContainsKey gi -> Some globalToLocal[gi] | _ -> None)
                            for a, b in List.pairwise locals do
                                hardX.Add(SeparationConstraint(a, b, 0., true))
                                hardX.Add(SeparationConstraint(b, a, 0., true))
                        | LayoutConstraint.OrderAlong(ax, bId, aId, gap) ->
                            match eff.ReversedIdMap.TryGetValue bId, eff.ReversedIdMap.TryGetValue aId with
                            | (true, gb), (true, ga) when globalToLocal.ContainsKey gb && globalToLocal.ContainsKey ga ->
                                let lb, la = globalToLocal[gb], globalToLocal[ga]
                                match ax with
                                | LayoutAxis.Horizontal -> hardX.Add(SeparationConstraint(lb, la, gap, true))
                                | LayoutAxis.Vertical -> hardY.Add(SeparationConstraint(lb, la, gap, true))
                            | _ -> ()

                    // Component edges
                    let compEdges = ResizeArray<struct(int * int * float * bool)>()
                    for e = 0 to csr.EdgeCount - 1 do
                        let gu, gv = csr.Src[e], csr.Dst[e]
                        if gu <> gv && globalToLocal.ContainsKey gu && globalToLocal.ContainsKey gv then
                            let lu, lv = globalToLocal[gu], globalToLocal[gv]
                            let sameGroup = eff.Nodes[gu].ParentGroup = eff.Nodes[gv].ParentGroup && eff.Nodes[gu].ParentGroup.IsSome
                            compEdges.Add(struct(lu, lv, csr.Weight[e], sameGroup))
                    let edgesArr = compEdges.ToArray()

                    // Group membership lists for compound gravity & sibling-group separation
                    let subGroupsWithDepth =
                        eff.Groups
                        |> Array.indexed
                        |> Array.choose (fun (gi, g) ->
                            let hasMember = members |> Array.exists (fun mi ->
                                let mutable cur = eff.Nodes[mi].ParentGroup
                                let mutable hit = false
                                while cur.IsSome && not hit do
                                    if cur.Value = g.Id then hit <- true
                                    else
                                        cur <- eff.Groups |> Array.tryFind (fun og -> og.Id = cur.Value) |> Option.bind (fun og -> og.ParentGroup)
                                hit)
                            if hasMember then Some(g, eff.GroupDepth[gi]) else None)

                    let gCount = subGroupsWithDepth.Length
                    let groupMembers = Array.init gCount (fun gi ->
                        let gid = (fst subGroupsWithDepth[gi]).Id
                        [| for li = 0 to subN - 1 do
                            let mutable cur = eff.Nodes[members[li]].ParentGroup
                            let mutable inside = false
                            while cur.IsSome && not inside do
                                if cur.Value = gid then inside <- true
                                else
                                    cur <- eff.Groups |> Array.tryFind (fun og -> og.Id = cur.Value) |> Option.bind (fun og -> og.ParentGroup)
                            if inside then yield li |])

                    // Phase 2: Iterative CoSE compound polish
                    let fx = Array.zeroCreate<float> subN
                    let fy = Array.zeroCreate<float> subN
                    let pts = Array.zeroCreate<Point> subN
                    let mutable rng = RandomState.create (config.Seed + uint64 compIdx * 104729UL)

                    for iter = 0 to config.Iterations - 1 do
                        token.ThrowIfCancellationRequested()
                        let cooling = 1. - float iter / float config.Iterations
                        let maxDisp = max 2. (config.IdealEdgeLength * 1.25 * cooling * cooling + 1.5)
                        Array.fill fx 0 subN 0.
                        Array.fill fy 0 subN 0.

                        // 1. Repulsion via Quadtree Barnes-Hut (or exact for tiny components)
                        if config.Repulsion > 0. && subN > 1 then
                            if subN <= 32 || config.Theta = 0. then
                                for i = 0 to subN - 1 do
                                    for j = i + 1 to subN - 1 do
                                        let dx = xs[i] - xs[j]
                                        let dy = ys[i] - ys[j]
                                        let dist = Double.Hypot(dx, dy)
                                        let ux, uy, effD =
                                            if dist < 1e-4 then
                                                let struct(r, nextRng) = RandomState.nextFloat rng
                                                rng <- nextRng
                                                let ang = r * 2. * Math.PI
                                                cos ang, sin ang, 1.
                                            else
                                                dx / dist, dy / dist, max 1. (dist - (radii[i] + radii[j]) * 0.5)
                                        let rep = config.Repulsion * masses[i] * masses[j] / (effD * effD)
                                        fx[i] <- fx[i] + ux * rep
                                        fy[i] <- fy[i] + uy * rep
                                        fx[j] <- fx[j] - ux * rep
                                        fy[j] <- fy[j] - uy * rep
                            else
                                for i = 0 to subN - 1 do pts[i] <- Point.create xs[i] ys[i]
                                let qtree = Quadtree.buildWithCancellation token (fun i -> pts[i]) (fun i -> masses[i]) (seq { 0 .. subN - 1 })
                                for i = 0 to subN - 1 do
                                    let pi = pts[i]
                                    Quadtree.visit (fun cell ->
                                        let dx = pi.X - cell.Center.X
                                        let dy = pi.Y - cell.Center.Y
                                        let dist = Double.Hypot(dx, dy)
                                        let e = cell.Extent
                                        let cellSize = max (e.Max.X - e.Min.X) (e.Max.Y - e.Min.Y)
                                        let contains = pi.X >= e.Min.X && pi.X <= e.Max.X && pi.Y >= e.Min.Y && pi.Y <= e.Max.Y
                                        if not contains && dist > 1e-4 && cellSize < config.Theta * dist then
                                            let effD = max 1. (dist - radii[i] * 0.5)
                                            let rep = config.Repulsion * masses[i] * cell.Mass / (effD * effD)
                                            fx[i] <- fx[i] + (dx / dist) * rep
                                            fy[i] <- fy[i] + (dy / dist) * rep
                                            false
                                        elif not cell.Items.IsEmpty then
                                            for item in cell.Items do
                                                let j = item.Value
                                                if j <> i then
                                                    let idx = pi.X - item.Position.X
                                                    let idy = pi.Y - item.Position.Y
                                                    let idist = Double.Hypot(idx, idy)
                                                    let ux, uy, effD =
                                                        if idist < 1e-4 then
                                                            let ang = float ((i * 37 + j * 13) % 360) * (Math.PI / 180.)
                                                            cos ang, sin ang, 1.
                                                        else
                                                            idx / idist, idy / idist, max 1. (idist - (radii[i] + radii[j]) * 0.5)
                                                    let rep = config.Repulsion * masses[i] * item.Mass / (effD * effD)
                                                    fx[i] <- fx[i] + ux * rep
                                                    fy[i] <- fy[i] + uy * rep
                                            false
                                        else true) qtree

                        // 2. Hooke spring attraction on intra-group & inter-group associations
                        for e = 0 to edgesArr.Length - 1 do
                            let struct(u, v, w, sameGroup) = edgesArr[e]
                            let dx = xs[v] - xs[u]
                            let dy = ys[v] - ys[u]
                            let dist = Double.Hypot(dx, dy)
                            if dist > 1e-6 then
                                let ideal = (if sameGroup then 0.8 else 1.15) * config.IdealEdgeLength + (radii[u] + radii[v]) * 0.6
                                let springK = 0.12 * w * (if sameGroup then 1.35 else 0.85)
                                let f = springK * (dist - ideal)
                                let ux, uy = dx / dist, dy / dist
                                fx[u] <- fx[u] + ux * f
                                fy[u] <- fy[u] + uy * f
                                fx[v] <- fx[v] - ux * f
                                fy[v] <- fy[v] - uy * f

                        // 3. Central gravity + Compound group cohesion & sibling-group overlap repulsion
                        for i = 0 to subN - 1 do
                            fx[i] <- fx[i] + (cx0 - xs[i]) * config.Gravity
                            fy[i] <- fy[i] + (cy0 - ys[i]) * config.Gravity

                        if gCount > 0 then
                            let gCx = Array.zeroCreate<float> gCount
                            let gCy = Array.zeroCreate<float> gCount
                            let gMinX = Array.zeroCreate<float> gCount
                            let gMaxX = Array.zeroCreate<float> gCount
                            let gMinY = Array.zeroCreate<float> gCount
                            let gMaxY = Array.zeroCreate<float> gCount
                            for gi = 0 to gCount - 1 do
                                let mems = groupMembers[gi]
                                let g = fst subGroupsWithDepth[gi]
                                if mems.Length > 0 then
                                    let mutable sx, sy = 0., 0.
                                    let mutable lx, rx, ty, by = infinity, -infinity, infinity, -infinity
                                    for li in mems do
                                        sx <- sx + xs[li]; sy <- sy + ys[li]
                                        lx <- min lx (xs[li] - halfW[li])
                                        rx <- max rx (xs[li] + halfW[li])
                                        ty <- min ty (ys[li] - halfH[li])
                                        by <- max by (ys[li] + halfH[li])
                                    let mx, my = sx / float mems.Length, sy / float mems.Length
                                    gCx[gi] <- mx; gCy[gi] <- my
                                    gMinX[gi] <- lx - g.Padding.Left
                                    gMaxX[gi] <- rx + g.Padding.Right
                                    gMinY[gi] <- ty - g.Padding.Top
                                    gMaxY[gi] <- by + g.Padding.Bottom
                                    for li in mems do
                                        fx[li] <- fx[li] + (mx - xs[li]) * config.CompoundGravity
                                        fy[li] <- fy[li] + (my - ys[li]) * config.CompoundGravity

                            // Repel overlapping sibling groups (plus ungrouped nodes invading a group box!)
                            for a = 0 to gCount - 1 do
                                for b = a + 1 to gCount - 1 do
                                    if (fst subGroupsWithDepth[a]).ParentGroup = (fst subGroupsWithDepth[b]).ParentGroup
                                       && groupMembers[a].Length > 0 && groupMembers[b].Length > 0 then
                                        let wa, ha = gMaxX[a] - gMinX[a], gMaxY[a] - gMinY[a]
                                        let wb, hb = gMaxX[b] - gMinX[b], gMaxY[b] - gMinY[b]
                                        let penX = (wa + wb) * 0.5 + 20. - abs (gCx[b] - gCx[a])
                                        let penY = (ha + hb) * 0.5 + 20. - abs (gCy[b] - gCy[a])
                                        if penX > 0. && penY > 0. then
                                            let pushX, pushY =
                                                if penX <= penY then
                                                    let s = if gCx[b] >= gCx[a] then 1. else -1.
                                                    s * penX * 0.45, 0.
                                                else
                                                    let s = if gCy[b] >= gCy[a] then 1. else -1.
                                                    0., s * penY * 0.45
                                            for li in groupMembers[a] do
                                                fx[li] <- fx[li] - pushX; fy[li] <- fy[li] - pushY
                                            for li in groupMembers[b] do
                                                fx[li] <- fx[li] + pushX; fy[li] <- fy[li] + pushY

                        // Apply clamped displacements
                        for i = 0 to subN - 1 do
                            let mag = Double.Hypot(fx[i], fy[i])
                            if mag > 1e-9 then
                                let step = min maxDisp mag
                                if not pinnedX[i] then xs[i] <- xs[i] + (fx[i] / mag) * step
                                if not pinnedY[i] then ys[i] <- ys[i] + (fy[i] / mag) * step

                        // Project hard user constraints at every 4th iteration and final iterations
                        if (hardX.Count > 0 || hardY.Count > 0) && (iter % 4 = 3 || iter = config.Iterations - 1) then
                            if hardX.Count > 0 then
                                let px = Cola.solveVpsc xs masses pinnedX hardX
                                Array.blit px 0 xs 0 subN
                            if hardY.Count > 0 then
                                let py = Cola.solveVpsc ys masses pinnedY hardY
                                Array.blit py 0 ys 0 subN

                    // Final non-overlap + constraint polish via VPSC
                    if config.EnforceNonOverlap && subN > 1 then
                        for _ = 0 to 1 do
                            let cx = ResizeArray<SeparationConstraint>(hardX)
                            let cy = ResizeArray<SeparationConstraint>(hardY)
                            Cola.appendNonOverlapConstraints xs ys halfW halfH 10. cx cy
                            if cx.Count > 0 then
                                let px = Cola.solveVpsc xs masses pinnedX cx
                                Array.blit px 0 xs 0 subN
                            if cy.Count > 0 then
                                let py = Cola.solveVpsc ys masses pinnedY cy
                                Array.blit py 0 ys 0 subN

                    let subNodes = members |> Array.map (fun gi -> eff.Nodes[gi])
                    let subNodeIds = subNodes |> Array.map (fun nd -> nd.Id) |> Set.ofArray
                    let subAssocs =
                        eff.Associations
                        |> Array.filter (fun a -> subNodeIds.Contains a.Source.NodeId && subNodeIds.Contains a.Target.NodeId)
                    let subMap = Dictionary<string, int>(StringComparer.Ordinal)
                    for li = 0 to subN - 1 do subMap.Add(subNodes[li].Id, li)
                    let subEff =
                        { Nodes = subNodes
                          Groups = subGroupsWithDepth |> Array.map fst
                          GroupDepth = subGroupsWithDepth |> Array.map snd
                          Associations = subAssocs
                          ReversedIdMap = subMap
                          Constraints = [] }

                    let posNodes =
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

                    let groups = GraphKernels.computeGroupBounds subEff (posNodes |> Array.map (fun pn -> pn.Bounds))
                    let routed = GraphKernels.routeDirectAssociations subEff posNodes None
                    let nList = Array.toList posNodes
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

                let compLayouts = components |> Array.mapi layoutComponent
                Ok(BoxPack.packComponentLayouts config.Bounds 28. compLayouts)

    /// <summary>Runs fCoSE fast compound layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

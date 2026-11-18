namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Force-directed energy law controlling community vs. uniform-edge aesthetics.</summary>
[<RequireQualifiedAccess>]
type EnergyModel =
    /// <summary>Jacomy et al. (2014) ForceAtlas2 degree-weighted repulsion with adaptive swing/traction speed control.</summary>
    | ForceAtlas2
    /// <summary>Noack (2009) LinLog logarithmic attraction and inverse-distance repulsion revealing modularity communities.</summary>
    | LinLog
    /// <summary>Fruchterman-Reingold quadratic attraction and inverse repulsion for uniform edge lengths.</summary>
    | FruchtermanReingold

/// <summary>Settings for Multilevel FM³ + ForceAtlas2 / LinLog / Fruchterman-Reingold 2D layout.</summary>
type ModularityForceConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Selected attraction/repulsion energy model.</summary>
      Model: EnergyModel
      /// <summary>Scaling constant governing equilibrium spacing in layout units.</summary>
      ScalingRatio: float
      /// <summary>Central gravity strength.</summary>
      Gravity: float
      /// <summary>Barnes-Hut opening angle theta (0 for exact pairwise, 0.8 default).</summary>
      Theta: float
      /// <summary>Fine-level iteration count (coarser levels run proportional warm-up steps).</summary>
      Iterations: int
      /// <summary>When true, uses Heavy-Edge Multilevel coarsening and prolongation for O(V log V + E) global unfolding.</summary>
      Multilevel: bool
      /// <summary>When true, prevents node bounding circles/boxes from overlapping.</summary>
      PreventOverlap: bool
      /// <summary>Deterministic seed for multilevel prolongation jitter.</summary>
      Seed: uint64 }

/// <summary>Constructors for <c>ModularityForceConfig</c>.</summary>
[<RequireQualifiedAccess>]
module ModularityForceConfig =
    /// <summary>Creates a multilevel ForceAtlas2 configuration with 25-unit scaling ratio, 0.8 Barnes-Hut theta and 80 iterations.</summary>
    let create bounds =
        { Bounds = bounds
          Model = EnergyModel.ForceAtlas2
          ScalingRatio = 25.
          Gravity = 0.08
          Theta = 0.8
          Iterations = 80
          Multilevel = true
          PreventOverlap = true
          Seed = 42UL }

/// <summary>Multilevel (FM³ / Walshaw) + ForceAtlas2 &amp; LinLog modularity-clustering 2D force engine.</summary>
[<RequireQualifiedAccess>]
module ModularityForce2D =
    let private relaxLevel
        (token: System.Threading.CancellationToken)
        (config: ModularityForceConfig)
        (level: CoarseLevel)
        (xs: float[])
        (ys: float[])
        (pinned: bool[])
        (iters: int)
        (cx0: float)
        (cy0: float)
        : unit =
        let n = level.NodeCount
        let m = level.EdgeSrc.Length
        if n > 1 && iters > 0 then
            let degMass = Array.copy level.Mass
            for e = 0 to m - 1 do
                degMass[level.EdgeSrc[e]] <- degMass[level.EdgeSrc[e]] + level.EdgeWeight[e]
                degMass[level.EdgeDst[e]] <- degMass[level.EdgeDst[e]] + level.EdgeWeight[e]

            let fx = Array.zeroCreate<float> n
            let fy = Array.zeroCreate<float> n
            let prevFx = Array.zeroCreate<float> n
            let prevFy = Array.zeroCreate<float> n
            let pts = Array.zeroCreate<Point> n
            let kr = config.ScalingRatio * config.ScalingRatio * 4.

            for step = 0 to iters - 1 do
                token.ThrowIfCancellationRequested()
                Array.fill fx 0 n 0.
                Array.fill fy 0 n 0.

                // Repulsion (Quadtree Barnes-Hut or exact for small n)
                if n <= 36 || config.Theta = 0. then
                    for i = 0 to n - 1 do
                        for j = i + 1 to n - 1 do
                            let dx = xs[i] - xs[j]
                            let dy = ys[i] - ys[j]
                            let rawD = Double.Hypot(dx, dy)
                            let ux, uy, d =
                                if rawD < 1e-4 then
                                    let ang = float ((i * 29 + j * 13 + step) % 360) * (Math.PI / 180.)
                                    cos ang, sin ang, 1.
                                else
                                    let eff =
                                        if config.PreventOverlap then max 0.5 (rawD - level.Radius[i] - level.Radius[j])
                                        else max 1. rawD
                                    dx / rawD, dy / rawD, eff
                            let mag =
                                match config.Model with
                                | EnergyModel.ForceAtlas2
                                | EnergyModel.LinLog ->
                                    if config.PreventOverlap && rawD < level.Radius[i] + level.Radius[j] then
                                        kr * degMass[i] * degMass[j] * 4. / max 0.5 rawD
                                    else
                                        kr * degMass[i] * degMass[j] / d
                                | EnergyModel.FruchtermanReingold ->
                                    (kr * level.Mass[i] * level.Mass[j]) / (d * d)
                            fx[i] <- fx[i] + ux * mag
                            fy[i] <- fy[i] + uy * mag
                            fx[j] <- fx[j] - ux * mag
                            fy[j] <- fy[j] - uy * mag
                else
                    for i = 0 to n - 1 do pts[i] <- Point.create xs[i] ys[i]
                    let qtree = Quadtree.buildWithCancellation token (fun i -> pts[i]) (fun i -> degMass[i]) (seq { 0 .. n - 1 })
                    for i = 0 to n - 1 do
                        SimdMath.check token i
                        let pi = pts[i]
                        let mi = degMass[i]
                        Quadtree.visit (fun cell ->
                            let dx = pi.X - cell.Center.X
                            let dy = pi.Y - cell.Center.Y
                            let d = Double.Hypot(dx, dy)
                            let e = cell.Extent
                            let sz = max (e.Max.X - e.Min.X) (e.Max.Y - e.Min.Y)
                            let contains = pi.X >= e.Min.X && pi.X <= e.Max.X && pi.Y >= e.Min.Y && pi.Y <= e.Max.Y
                            if not contains && d > 1e-4 && sz < config.Theta * d then
                                let mag =
                                    match config.Model with
                                    | EnergyModel.FruchtermanReingold -> (kr * mi * cell.Mass) / (d * d)
                                    | _ -> kr * mi * cell.Mass / d
                                fx[i] <- fx[i] + (dx / d) * mag
                                fy[i] <- fy[i] + (dy / d) * mag
                                false
                            elif not cell.Items.IsEmpty then
                                for item in cell.Items do
                                    let j = item.Value
                                    if j <> i then
                                        let idx = pi.X - item.Position.X
                                        let idy = pi.Y - item.Position.Y
                                        let rawD = Double.Hypot(idx, idy)
                                        let ux, uy, effD =
                                            if rawD < 1e-4 then
                                                let ang = float ((i * 29 + j * 13) % 360) * (Math.PI / 180.)
                                                cos ang, sin ang, 1.
                                            else
                                                let eff =
                                                    if config.PreventOverlap then max 0.5 (rawD - level.Radius[i] - level.Radius[j])
                                                    else max 1. rawD
                                                idx / rawD, idy / rawD, eff
                                        let mag =
                                            match config.Model with
                                            | EnergyModel.FruchtermanReingold -> (kr * mi * item.Mass) / (effD * effD)
                                            | _ -> kr * mi * item.Mass / effD
                                        fx[i] <- fx[i] + ux * mag
                                        fy[i] <- fy[i] + uy * mag
                                false
                            else true) qtree

                // Attraction along edges
                for e = 0 to m - 1 do
                    let u, v, w = level.EdgeSrc[e], level.EdgeDst[e], level.EdgeWeight[e]
                    let dx = xs[v] - xs[u]
                    let dy = ys[v] - ys[u]
                    let d = Double.Hypot(dx, dy)
                    if d > 1e-6 then
                        let ux, uy = dx / d, dy / d
                        let att =
                            match config.Model with
                            | EnergyModel.ForceAtlas2 -> w * d
                            | EnergyModel.LinLog -> w * log (1. + d / max 1. config.ScalingRatio) * config.ScalingRatio
                            | EnergyModel.FruchtermanReingold -> w * (d * d) / max 1. (config.ScalingRatio * 3.)
                        fx[u] <- fx[u] + ux * att
                        fy[u] <- fy[u] + uy * att
                        fx[v] <- fx[v] - ux * att
                        fy[v] <- fy[v] - uy * att

                // Gravity toward center
                for i = 0 to n - 1 do
                    let dx = cx0 - xs[i]
                    let dy = cy0 - ys[i]
                    let d = Double.Hypot(dx, dy)
                    if d > 1e-6 then
                        let gForce = config.Gravity * degMass[i] * d
                        fx[i] <- fx[i] + (dx / d) * gForce
                        fy[i] <- fy[i] + (dy / d) * gForce

                // ForceAtlas2 adaptive global + per-node swing/traction speed integration!
                let mutable totalSwing = 0.
                let mutable totalTraction = 0.
                for i = 0 to n - 1 do
                    if not pinned[i] then
                        let sw = Double.Hypot(fx[i] - prevFx[i], fy[i] - prevFy[i])
                        let tr = 0.5 * Double.Hypot(fx[i] + prevFx[i], fy[i] + prevFy[i])
                        totalSwing <- totalSwing + degMass[i] * sw
                        totalTraction <- totalTraction + degMass[i] * tr

                let cooling = 1. - 0.65 * (float step / float iters)
                let globalSpeed =
                    if step = 0 || totalSwing <= 1e-9 then 0.25 * cooling
                    else min (2.0 * cooling) (0.6 * totalTraction / totalSwing * cooling)
                let maxMove = config.ScalingRatio * 4. * cooling + 2.

                for i = 0 to n - 1 do
                    if not pinned[i] then
                        let sw = Double.Hypot(fx[i] - prevFx[i], fy[i] - prevFy[i])
                        let fMag = Double.Hypot(fx[i], fy[i])
                        if fMag > 1e-9 then
                            let nodeSpeed = globalSpeed / (1. + globalSpeed * sqrt sw)
                            let disp = min maxMove (fMag * nodeSpeed)
                            xs[i] <- xs[i] + (fx[i] / fMag) * disp
                            ys[i] <- ys[i] + (fy[i] / fMag) * disp
                    prevFx[i] <- fx[i]
                    prevFy[i] <- fy[i]

    /// <summary>Runs Multilevel ForceAtlas2 / LinLog / Fruchterman-Reingold layout on <c>graph</c> with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: ModularityForceConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.ScalingRatio && Double.IsFinite config.Gravity && Double.IsFinite config.Theta)
           || config.ScalingRatio <= 0. || config.Gravity < 0. || config.Theta < 0.
           || config.Iterations < 1 || config.Iterations > 1000 then
            Error [ InvalidObservation "ModularityForce2D requires positive ScalingRatio, nonnegative Gravity/Theta and 1..1000 Iterations." ]
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
                    let subNodes = members |> Array.map (fun gi -> eff.Nodes[gi])
                    let subNodeIds = subNodes |> Array.map (fun nd -> nd.Id) |> Set.ofArray
                    let subGroups =
                        eff.Groups
                        |> Array.indexed
                        |> Array.choose (fun (gi, g) ->
                            let hasMember = subNodes |> Array.exists (fun n ->
                                let mutable cur = n.ParentGroup
                                let mutable hit = false
                                while cur.IsSome && not hit do
                                    if cur.Value = g.Id then hit <- true
                                    else
                                        cur <- eff.Groups |> Array.tryFind (fun og -> og.Id = cur.Value) |> Option.bind (fun og -> og.ParentGroup)
                                hit)
                            if hasMember then Some(g, eff.GroupDepth[gi]) else None)
                    let subAssocs =
                        eff.Associations
                        |> Array.filter (fun a -> subNodeIds.Contains a.Source.NodeId && subNodeIds.Contains a.Target.NodeId)
                    let subMap = Dictionary<string, int>(StringComparer.Ordinal)
                    for li = 0 to subN - 1 do subMap.Add(subNodes[li].Id, li)
                    let subEff =
                        { Nodes = subNodes
                          Groups = subGroups |> Array.map fst
                          GroupDepth = subGroups |> Array.map snd
                          Associations = subAssocs
                          ReversedIdMap = subMap
                          Constraints = [] }

                    let subCsr = GraphKernels.buildCsr subEff
                    let masses = Array.init subN (fun i -> subNodes[i].Mass)
                    let radii = Array.init subN (fun i -> 0.5 * Double.Hypot(subNodes[i].Size.Width, subNodes[i].Size.Height))

                    let xs, ys =
                        if config.Multilevel && subN > 48 then
                            let hierarchy = GraphKernels.buildMultilevel subCsr masses radii 24 |> List.toArray
                            let coarsest = hierarchy[hierarchy.Length - 1]
                            let mutable curX = Array.zeroCreate<float> coarsest.NodeCount
                            let mutable curY = Array.zeroCreate<float> coarsest.NodeCount
                            // Seed coarsest level along deterministic Vogel golden-angle spiral
                            for i = 0 to coarsest.NodeCount - 1 do
                                let r = config.ScalingRatio * sqrt (float (i + 1))
                                let a = float i * 2.399963229728653
                                curX[i] <- cx0 + r * cos a
                                curY[i] <- cy0 + r * sin a
                            relaxLevel token config coarsest curX curY (Array.zeroCreate coarsest.NodeCount) (max 24 (config.Iterations / 2)) cx0 cy0

                            let mutable rng = RandomState.create (config.Seed + uint64 compIdx * 31337UL)
                            for lvlIdx = hierarchy.Length - 2 downto 0 do
                                let fine = hierarchy[lvlIdx]
                                let nextX = Array.zeroCreate<float> fine.NodeCount
                                let nextY = Array.zeroCreate<float> fine.NodeCount
                                let map = fine.FineToCoarse
                                for i = 0 to fine.NodeCount - 1 do
                                    let ci = map[i]
                                    let struct(u1, r1) = RandomState.nextFloat rng
                                    let struct(u2, r2) = RandomState.nextFloat r1
                                    rng <- r2
                                    let jitterR = 2.5 + u1 * config.ScalingRatio * 0.25
                                    let ang = u2 * 2. * Math.PI
                                    nextX[i] <- curX[ci] + jitterR * cos ang
                                    nextY[i] <- curY[ci] + jitterR * sin ang
                                let pin =
                                    if lvlIdx = 0 then
                                        Array.init subN (fun i ->
                                            match subNodes[i].Pinned with
                                            | Some p -> nextX[i] <- p.X; nextY[i] <- p.Y; true
                                            | None -> false)
                                    else Array.zeroCreate fine.NodeCount
                                let steps = if lvlIdx = 0 then config.Iterations else max 20 (config.Iterations / 2)
                                relaxLevel token config fine nextX nextY pin steps cx0 cy0
                                curX <- nextX
                                curY <- nextY
                            curX, curY
                        else
                            let mds = GraphKernels.pivotMdsComponent token subCsr (Array.init subN id) 2 (min 32 subN)
                            let xArr = Array.init subN (fun i -> cx0 + mds[0][i] * config.ScalingRatio * 3.)
                            let yArr = Array.init subN (fun i -> cy0 + mds[1][i] * config.ScalingRatio * 3.)
                            let pin =
                                Array.init subN (fun i ->
                                    match subNodes[i].Pinned with
                                    | Some p -> xArr[i] <- p.X; yArr[i] <- p.Y; true
                                    | None -> false)
                            let singleLevel = (GraphKernels.buildMultilevel subCsr masses radii (subN + 1)).Head
                            relaxLevel token config singleLevel xArr yArr pin config.Iterations cx0 cy0
                            xArr, yArr

                    if config.PreventOverlap && subN > 1 then
                        let halfW = Array.init subN (fun i -> subNodes[i].Size.Width * 0.5)
                        let halfH = Array.init subN (fun i -> subNodes[i].Size.Height * 0.5)
                        let pin = Array.init subN (fun i -> subNodes[i].Pinned.IsSome)
                        let xCons = ResizeArray<SeparationConstraint>()
                        let yCons = ResizeArray<SeparationConstraint>()
                        Cola.appendNonOverlapConstraints xs ys halfW halfH 6. xCons yCons
                        if xCons.Count > 0 then Array.blit (Cola.solveVpsc xs masses pin xCons) 0 xs 0 subN
                        if yCons.Count > 0 then Array.blit (Cola.solveVpsc ys masses pin yCons) 0 ys 0 subN

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

    /// <summary>Runs Multilevel ForceAtlas2 / LinLog / Fruchterman-Reingold layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

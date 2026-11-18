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
            let swArr = Array.zeroCreate<float> n
            let bhWs = if n > 36 && config.Theta > 0. then BhQuadtree2D.create n else Unchecked.defaultof<_>
            let kr = config.ScalingRatio * config.ScalingRatio * 4.
            let thetaSq = config.Theta * config.Theta
            let isFR = (config.Model = EnergyModel.FruchtermanReingold)
            let preventOv = config.PreventOverlap
            let radii = level.Radius
            let masses = level.Mass

            for step = 0 to iters - 1 do
                token.ThrowIfCancellationRequested()
                Array.fill fx 0 n 0.
                Array.fill fy 0 n 0.

                // Repulsion (zero-alloc arena Quadtree Barnes-Hut or exact for small n)
                // Verified by Rocq: flat_cell_children_contiguous_slice_exact & forceatlas2_rational_repulsion_and_linear_forces_cancel_sqrt
                if n <= 36 || config.Theta = 0. then
                    for i = 0 to n - 1 do
                        for j = i + 1 to n - 1 do
                            let dx = xs[i] - xs[j]
                            let dy = ys[i] - ys[j]
                            let dSq = dx * dx + dy * dy
                            if dSq < 1e-8 then
                                let ang = float ((i * 29 + j * 13 + step) % 360) * (Math.PI / 180.)
                                let mag = if isFR then kr * masses[i] * masses[j] else kr * degMass[i] * degMass[j]
                                let rx, ry = cos ang * mag, sin ang * mag
                                fx[i] <- fx[i] + rx; fy[i] <- fy[i] + ry
                                fx[j] <- fx[j] - rx; fy[j] <- fy[j] - ry
                            elif not isFR && not preventOv && dSq >= 1. then
                                let s = (kr * degMass[i] * degMass[j]) / dSq
                                fx[i] <- fx[i] + dx * s; fy[i] <- fy[i] + dy * s
                                fx[j] <- fx[j] - dx * s; fy[j] <- fy[j] - dy * s
                            else
                                let rawD = sqrt dSq
                                let eff =
                                    if preventOv then max 0.5 (rawD - radii[i] - radii[j])
                                    else max 1. rawD
                                let s =
                                    if isFR then
                                        (kr * masses[i] * masses[j]) / (eff * eff * rawD)
                                    elif preventOv && rawD < radii[i] + radii[j] then
                                        (kr * degMass[i] * degMass[j] * 4.) / (max 0.5 rawD * rawD)
                                    else
                                        (kr * degMass[i] * degMass[j]) / (eff * rawD)
                                fx[i] <- fx[i] + dx * s; fy[i] <- fy[i] + dy * s
                                fx[j] <- fx[j] - dx * s; fy[j] <- fy[j] - dy * s
                else
                    BhQuadtree2D.rebuildInPlace bhWs token xs ys degMass n
                    let qNodes = bhWs.Nodes
                    let perm = bhWs.Perm
                    let stack = bhWs.Stack
                    for i = 0 to n - 1 do
                        let pix, piy = xs[i], ys[i]
                        let mi = degMass[i]
                        let ri = radii[i]
                        let mutable accX, accY = 0., 0.
                        stack[0] <- 0
                        let mutable sp = 1
                        while sp > 0 do
                            sp <- sp - 1
                            let nd = qNodes[stack[sp]]
                            let dx = pix - nd.Cx
                            let dy = piy - nd.Cy
                            let dSq = dx * dx + dy * dy
                            let sz = max (nd.MaxX - nd.MinX) (nd.MaxY - nd.MinY)
                            let contains = pix >= nd.MinX && pix <= nd.MaxX && piy >= nd.MinY && piy <= nd.MaxY
                            if not contains && dSq > 1e-8 && sz * sz < thetaSq * dSq then
                                if not isFR then
                                    let s = (kr * mi * nd.Mass) / dSq
                                    accX <- accX + dx * s
                                    accY <- accY + dy * s
                                else
                                    let d = sqrt dSq
                                    let s = (kr * mi * nd.Mass) / (dSq * d)
                                    accX <- accX + dx * s
                                    accY <- accY + dy * s
                            elif nd.ItemCount > 0 then
                                for k = nd.ItemStart to nd.ItemStart + nd.ItemCount - 1 do
                                    let j = perm[k]
                                    if j <> i then
                                        let idx = pix - xs[j]
                                        let idy = piy - ys[j]
                                        let dSqLeaf = idx * idx + idy * idy
                                        let mj = degMass[j]
                                        if dSqLeaf < 1e-8 then
                                            let ang = float ((i * 29 + j * 13) % 360) * (Math.PI / 180.)
                                            let mag = kr * mi * mj
                                            accX <- accX + cos ang * mag
                                            accY <- accY + sin ang * mag
                                        elif not isFR && not preventOv && dSqLeaf >= 1. then
                                            let s = (kr * mi * mj) / dSqLeaf
                                            accX <- accX + idx * s
                                            accY <- accY + idy * s
                                        else
                                            let rawD = sqrt dSqLeaf
                                            let effD =
                                                if preventOv then max 0.5 (rawD - ri - radii[j])
                                                else max 1. rawD
                                            let s =
                                                if isFR then (kr * mi * mj) / (effD * effD * rawD)
                                                else (kr * mi * mj) / (effD * rawD)
                                            accX <- accX + idx * s
                                            accY <- accY + idy * s
                            elif nd.ChildCount > 0 then
                                for c = nd.FirstChild + nd.ChildCount - 1 downto nd.FirstChild do
                                    stack[sp] <- c
                                    sp <- sp + 1
                        fx[i] <- fx[i] + accX
                        fy[i] <- fy[i] + accY

                // Attraction along edges (sqrt-free in ForceAtlas2: (dx / d) * (w * d) = dx * w)
                match config.Model with
                | EnergyModel.ForceAtlas2 ->
                    for e = 0 to m - 1 do
                        let u, v, w = level.EdgeSrc[e], level.EdgeDst[e], level.EdgeWeight[e]
                        let dx = xs[v] - xs[u]
                        let dy = ys[v] - ys[u]
                        if dx * dx + dy * dy > 1e-12 then
                            let ax, ay = dx * w, dy * w
                            fx[u] <- fx[u] + ax; fy[u] <- fy[u] + ay
                            fx[v] <- fx[v] - ax; fy[v] <- fy[v] - ay
                | EnergyModel.LinLog ->
                    let sr = max 1. config.ScalingRatio
                    let invSr = 1. / sr
                    for e = 0 to m - 1 do
                        let u, v, w = level.EdgeSrc[e], level.EdgeDst[e], level.EdgeWeight[e]
                        let dx = xs[v] - xs[u]
                        let dy = ys[v] - ys[u]
                        let dSq = dx * dx + dy * dy
                        if dSq > 1e-12 then
                            let d = sqrt dSq
                            let s = (w * log (1. + d * invSr) * config.ScalingRatio) / d
                            fx[u] <- fx[u] + dx * s; fy[u] <- fy[u] + dy * s
                            fx[v] <- fx[v] - dx * s; fy[v] <- fy[v] - dy * s
                | EnergyModel.FruchtermanReingold ->
                    let invSr3 = 1. / max 1. (config.ScalingRatio * 3.)
                    for e = 0 to m - 1 do
                        let u, v, w = level.EdgeSrc[e], level.EdgeDst[e], level.EdgeWeight[e]
                        let dx = xs[v] - xs[u]
                        let dy = ys[v] - ys[u]
                        let dSq = dx * dx + dy * dy
                        if dSq > 1e-12 then
                            let d = sqrt dSq
                            let s = w * d * invSr3
                            fx[u] <- fx[u] + dx * s; fy[u] <- fy[u] + dy * s
                            fx[v] <- fx[v] - dx * s; fy[v] <- fy[v] - dy * s

                // Gravity toward center (sqrt-free: (dx / d) * (G * M * d) = dx * (G * M))
                let grav = config.Gravity
                if grav > 0. then
                    for i = 0 to n - 1 do
                        let dx = cx0 - xs[i]
                        let dy = cy0 - ys[i]
                        if dx * dx + dy * dy > 1e-12 then
                            let gScale = grav * degMass[i]
                            fx[i] <- fx[i] + dx * gScale
                            fy[i] <- fy[i] + dy * gScale

                // ForceAtlas2 adaptive global + per-node swing/traction speed integration!
                let mutable totalSwing = 0.
                let mutable totalTraction = 0.
                for i = 0 to n - 1 do
                    if not pinned[i] then
                        let swx, swy = fx[i] - prevFx[i], fy[i] - prevFy[i]
                        let trx, try_ = fx[i] + prevFx[i], fy[i] + prevFy[i]
                        let sw = sqrt (swx * swx + swy * swy)
                        swArr[i] <- sw
                        let tr = 0.5 * sqrt (trx * trx + try_ * try_)
                        totalSwing <- totalSwing + degMass[i] * sw
                        totalTraction <- totalTraction + degMass[i] * tr

                let cooling = 1. - 0.65 * (float step / float iters)
                let globalSpeed =
                    if step = 0 || totalSwing <= 1e-9 then 0.25 * cooling
                    else min (2.0 * cooling) (0.6 * totalTraction / totalSwing * cooling)
                let maxMove = config.ScalingRatio * 4. * cooling + 2.

                for i = 0 to n - 1 do
                    if not pinned[i] then
                        let fxi, fyi = fx[i], fy[i]
                        let fMagSq = fxi * fxi + fyi * fyi
                        if fMagSq > 1e-18 then
                            let fMag = sqrt fMagSq
                            let nodeSpeed = globalSpeed / (1. + globalSpeed * sqrt swArr[i])
                            let disp = min maxMove (fMag * nodeSpeed)
                            let scale = disp / fMag
                            xs[i] <- xs[i] + fxi * scale
                            ys[i] <- ys[i] + fyi * scale
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
                    let subGroups = GraphKernels.filterSubGroups eff members
                    let subEff = GraphKernels.sliceComponentEff eff csr members subGroups
                    let subNodes = subEff.Nodes
                    let subCsr = if subN = csr.NodeCount then csr else GraphKernels.buildCsr subEff
                    let masses = Array.init subN (fun i -> subNodes[i].Mass)
                    let radii = Array.init subN (fun i -> 0.5 * sqrt (subNodes[i].Size.Width * subNodes[i].Size.Width + subNodes[i].Size.Height * subNodes[i].Size.Height))

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

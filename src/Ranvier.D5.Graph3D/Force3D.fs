namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Settings for 3D Octree Barnes-Hut + Multilevel ForceAtlas2 / LinLog / Fruchterman-Reingold compound 3D layout.</summary>
type Force3DConfig =
    { /// <summary>Target 3D bounding volume center and reference scale.</summary>
      Bounds: Bounds3
      /// <summary>Force energy law (<c>ForceAtlas2</c>, <c>LinLog</c> or <c>FruchtermanReingold</c>).</summary>
      Model: EnergyModel
      /// <summary>Equilibrium edge/repulsion scale in 3D layout units.</summary>
      ScalingRatio: float
      /// <summary>Central gravity pulling toward the target cuboid center.</summary>
      Gravity: float
      /// <summary>Compound cluster cohesion strength in 3D space.</summary>
      CompoundGravity: float
      /// <summary>3D Octree Barnes-Hut opening angle theta (0 for exact O(N^2), 0.8 default).</summary>
      Theta: float
      /// <summary>Iteration count (1..1000; default 80).</summary>
      Iterations: int
      /// <summary>When true, enforces 3D bounding-sphere collision separation.</summary>
      PreventOverlap: bool
      /// <summary>Deterministic random seed.</summary>
      Seed: uint64 }

/// <summary>Constructors for <c>Force3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Force3DConfig =
    /// <summary>Creates a 3D Octree Barnes-Hut ForceAtlas2 configuration with 35-unit scaling ratio, theta 0.8 and 80 iterations.</summary>
    let create bounds =
        { Bounds = bounds
          Model = EnergyModel.ForceAtlas2
          ScalingRatio = 35.
          Gravity = 0.08
          CompoundGravity = 0.2
          Theta = 0.8
          Iterations = 80
          PreventOverlap = true
          Seed = 42UL }

/// <summary>3D Octree Barnes-Hut force-directed layout engine with ForceAtlas2, LinLog modularity and 3D compound group cohesion.</summary>
[<RequireQualifiedAccess>]
module Force3D =
    /// <summary>Runs 3D Octree force-directed layout with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: Force3DConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout3D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.ScalingRatio && Double.IsFinite config.Gravity
                && Double.IsFinite config.CompoundGravity && Double.IsFinite config.Theta)
           || config.ScalingRatio <= 0. || config.Gravity < 0.
           || config.CompoundGravity < 0. || config.Theta < 0.
           || config.Iterations < 1 || config.Iterations > 1000 then
            Error [ InvalidObservation "Force3D requires positive ScalingRatio, nonnegative Gravity/Theta and 1..1000 Iterations." ]
        else
            let resolved = DiagramGraph.resolveCollapsed graph
            let nodes = DiagramGraph.nodes resolved |> List.toArray
            let assocs = DiagramGraph.associations resolved |> List.toArray
            let groups = DiagramGraph.groups resolved |> List.toArray
            let n = nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = Kernels3D.computeGroups3D resolved [||]; Associations = [] }
            else
                let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
                for i = 0 to n - 1 do nodeIdx.Add(nodes[i].Id, i)

                let center = Bounds3.center config.Bounds
                let cx0, cy0, cz0 = center.X, center.Y, center.Z

                // Deterministic Fibonacci-sphere + group-offset initial 3D placement
                let xs = Array.zeroCreate<float> n
                let ys = Array.zeroCreate<float> n
                let zs = Array.zeroCreate<float> n
                let radii =
                    Array.init n (fun i ->
                        let nd = nodes[i]
                        0.5 * sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height + nd.DepthSize * nd.DepthSize))
                let masses = Array.init n (fun i -> nodes[i].Mass)

                let goldenAngle = Math.PI * (3. - sqrt 5.)
                let initRadius = config.ScalingRatio * Math.Pow(float n, 1. / 3.) * 1.4
                let mutable rng = RandomState.create config.Seed
                for i = 0 to n - 1 do
                    let struct(j, nextR) = RandomState.nextFloat rng
                    rng <- nextR
                    let yUnit = if n = 1 then 0. else 1. - (float i / float (n - 1)) * 2.
                    let ringR = sqrt (max 0. (1. - yUnit * yUnit))
                    let theta = float i * goldenAngle + j * 0.05
                    let shell = initRadius * (0.45 + 0.55 * Math.Pow((float i + 0.5) / float n, 1. / 3.))
                    xs[i] <- cx0 + shell * ringR * cos theta
                    ys[i] <- cy0 + shell * yUnit
                    zs[i] <- cz0 + shell * ringR * sin theta
                    nodes[i].Pinned |> Option.iter (fun p ->
                        xs[i] <- p.X; ys[i] <- p.Y)

                // Deduplicated undirected weighted edge arrays for fast inner relaxation
                let edgeList = ResizeArray<struct(int * int * float)>()
                let degMass = Array.copy masses
                for a in assocs do
                    let u = nodeIdx[a.Source.NodeId]
                    let v = nodeIdx[a.Target.NodeId]
                    if u <> v then
                        edgeList.Add(struct(u, v, a.Weight))
                        degMass[u] <- degMass[u] + a.Weight
                        degMass[v] <- degMass[v] + a.Weight
                let edges = edgeList.ToArray()

                // Group membership index lists for 3D compound group cohesion & sibling separation
                let gMembers =
                    groups
                    |> Array.map (fun g ->
                        [| for ni = 0 to n - 1 do
                            let mutable cur = nodes[ni].ParentGroup
                            let mutable inside = false
                            while cur.IsSome && not inside do
                                if cur.Value = g.Id then inside <- true
                                else
                                    cur <- groups |> Array.tryFind (fun og -> og.Id = cur.Value) |> Option.bind (fun og -> og.ParentGroup)
                            if inside then yield ni |])

                let fx = Array.zeroCreate<float> n
                let fy = Array.zeroCreate<float> n
                let fz = Array.zeroCreate<float> n
                let prevFx = Array.zeroCreate<float> n
                let prevFy = Array.zeroCreate<float> n
                let prevFz = Array.zeroCreate<float> n
                let pts = Array.zeroCreate<Point3> n
                let kr = config.ScalingRatio * config.ScalingRatio * 4.5

                for step = 0 to config.Iterations - 1 do
                    token.ThrowIfCancellationRequested()
                    Array.fill fx 0 n 0.
                    Array.fill fy 0 n 0.
                    Array.fill fz 0 n 0.

                    // 1. 3D Repulsion via Octree Barnes-Hut (or exact for small graphs)
                    if n <= 36 || config.Theta = 0. then
                        for i = 0 to n - 1 do
                            for j = i + 1 to n - 1 do
                                let dx = xs[i] - xs[j]
                                let dy = ys[i] - ys[j]
                                let dz = zs[i] - zs[j]
                                let rawD = sqrt (dx * dx + dy * dy + dz * dz)
                                let ux, uy, uz, d =
                                    if rawD < 1e-4 then
                                        let a1 = float ((i * 29 + j * 13 + step) % 360) * (Math.PI / 180.)
                                        cos a1, sin a1, 0., 1.
                                    else
                                        let eff =
                                            if config.PreventOverlap then max 0.5 (rawD - radii[i] - radii[j])
                                            else max 1. rawD
                                        dx / rawD, dy / rawD, dz / rawD, eff
                                let mag =
                                    match config.Model with
                                    | EnergyModel.FruchtermanReingold -> (kr * masses[i] * masses[j]) / (d * d)
                                    | _ ->
                                        if config.PreventOverlap && rawD < radii[i] + radii[j] then
                                            kr * degMass[i] * degMass[j] * 4. / max 0.5 rawD
                                        else kr * degMass[i] * degMass[j] / d
                                fx[i] <- fx[i] + ux * mag; fy[i] <- fy[i] + uy * mag; fz[i] <- fz[i] + uz * mag
                                fx[j] <- fx[j] - ux * mag; fy[j] <- fy[j] - uy * mag; fz[j] <- fz[j] - uz * mag
                    else
                        for i = 0 to n - 1 do pts[i] <- Point3.create xs[i] ys[i] zs[i]
                        let oct = Octree.buildWithCancellation token (fun i -> pts[i]) (fun i -> degMass[i]) (seq { 0 .. n - 1 })
                        Octree.visitIndexed n (fun i cell ->
                            let pi = pts[i]
                            let mi = degMass[i]
                            let dx = pi.X - cell.Center.X
                            let dy = pi.Y - cell.Center.Y
                            let dz = pi.Z - cell.Center.Z
                            let d = sqrt (dx * dx + dy * dy + dz * dz)
                            let e = cell.Extent
                            let sz = max (Bounds3.sizeX e) (max (Bounds3.sizeY e) (Bounds3.sizeZ e))
                            let contains = Bounds3.contains pi e
                            if not contains && d > 1e-4 && sz < config.Theta * d then
                                let mag =
                                    match config.Model with
                                    | EnergyModel.FruchtermanReingold -> (kr * mi * cell.Mass) / (d * d)
                                    | _ -> kr * mi * cell.Mass / d
                                fx[i] <- fx[i] + (dx / d) * mag
                                fy[i] <- fy[i] + (dy / d) * mag
                                fz[i] <- fz[i] + (dz / d) * mag
                                false
                            elif not cell.Items.IsEmpty then
                                for item in cell.Items do
                                    let j = item.Value
                                    if j <> i then
                                        let idx = pi.X - item.Position.X
                                        let idy = pi.Y - item.Position.Y
                                        let idz = pi.Z - item.Position.Z
                                        let rawD = sqrt (idx * idx + idy * idy + idz * idz)
                                        let ux, uy, uz, effD =
                                            if rawD < 1e-4 then
                                                let a1 = float ((i * 29 + j * 13 + step) % 360) * (Math.PI / 180.)
                                                cos a1, sin a1, 0., 1.
                                            else
                                                let eff =
                                                    if config.PreventOverlap then max 0.5 (rawD - radii[i] - radii[j])
                                                    else max 1. rawD
                                                idx / rawD, idy / rawD, idz / rawD, eff
                                        let mag =
                                            match config.Model with
                                            | EnergyModel.FruchtermanReingold -> (kr * mi * item.Mass) / (effD * effD)
                                            | _ -> kr * mi * item.Mass / effD
                                        fx[i] <- fx[i] + ux * mag
                                        fy[i] <- fy[i] + uy * mag
                                        fz[i] <- fz[i] + uz * mag
                                false
                            else true) oct

                    // 2. 3D Edge attraction
                    for e = 0 to edges.Length - 1 do
                        let struct(u, v, w) = edges[e]
                        let dx = xs[v] - xs[u]
                        let dy = ys[v] - ys[u]
                        let dz = zs[v] - zs[u]
                        let d = sqrt (dx * dx + dy * dy + dz * dz)
                        if d > 1e-6 then
                            let ux, uy, uz = dx / d, dy / d, dz / d
                            let att =
                                match config.Model with
                                | EnergyModel.ForceAtlas2 -> w * d
                                | EnergyModel.LinLog -> w * log (1. + d / max 1. config.ScalingRatio) * config.ScalingRatio
                                | EnergyModel.FruchtermanReingold -> w * (d * d) / max 1. (config.ScalingRatio * 3.)
                            fx[u] <- fx[u] + ux * att; fy[u] <- fy[u] + uy * att; fz[u] <- fz[u] + uz * att
                            fx[v] <- fx[v] - ux * att; fy[v] <- fy[v] - uy * att; fz[v] <- fz[v] - uz * att

                    // 3. Central 3D gravity + Compound 3D group cohesion
                    for i = 0 to n - 1 do
                        let dx, dy, dz = cx0 - xs[i], cy0 - ys[i], cz0 - zs[i]
                        let d = sqrt (dx * dx + dy * dy + dz * dz)
                        if d > 1e-6 then
                            let gf = config.Gravity * degMass[i] * d
                            fx[i] <- fx[i] + (dx / d) * gf
                            fy[i] <- fy[i] + (dy / d) * gf
                            fz[i] <- fz[i] + (dz / d) * gf

                    if groups.Length > 0 && config.CompoundGravity > 0. then
                        for gi = 0 to groups.Length - 1 do
                            let mems = gMembers[gi]
                            if mems.Length > 1 then
                                let mutable sx, sy, sz = 0., 0., 0.
                                for ni in mems do
                                    sx <- sx + xs[ni]; sy <- sy + ys[ni]; sz <- sz + zs[ni]
                                let mx, my, mz = sx / float mems.Length, sy / float mems.Length, sz / float mems.Length
                                for ni in mems do
                                    fx[ni] <- fx[ni] + (mx - xs[ni]) * config.CompoundGravity * degMass[ni]
                                    fy[ni] <- fy[ni] + (my - ys[ni]) * config.CompoundGravity * degMass[ni]
                                    fz[ni] <- fz[ni] + (mz - zs[ni]) * config.CompoundGravity * degMass[ni]

                    // 4. Adaptive ForceAtlas2 3D swing/traction velocity step
                    let mutable totalSwing = 0.
                    let mutable totalTraction = 0.
                    for i = 0 to n - 1 do
                        let swx, swy, swz = fx[i] - prevFx[i], fy[i] - prevFy[i], fz[i] - prevFz[i]
                        let trx, try_, trz = fx[i] + prevFx[i], fy[i] + prevFy[i], fz[i] + prevFz[i]
                        totalSwing <- totalSwing + degMass[i] * sqrt (swx * swx + swy * swy + swz * swz)
                        totalTraction <- totalTraction + degMass[i] * 0.5 * sqrt (trx * trx + try_ * try_ + trz * trz)

                    let cooling = 1. - 0.65 * (float step / float config.Iterations)
                    let globalSpeed =
                        if step = 0 || totalSwing <= 1e-9 then 0.25 * cooling
                        else min (2.0 * cooling) (0.6 * totalTraction / totalSwing * cooling)
                    let maxMove = config.ScalingRatio * 4. * cooling + 2.

                    for i = 0 to n - 1 do
                        let swx, swy, swz = fx[i] - prevFx[i], fy[i] - prevFy[i], fz[i] - prevFz[i]
                        let sw = sqrt (swx * swx + swy * swy + swz * swz)
                        let fMag = sqrt (fx[i] * fx[i] + fy[i] * fy[i] + fz[i] * fz[i])
                        if fMag > 1e-9 then
                            let nodeSpeed = globalSpeed / (1. + globalSpeed * sqrt sw)
                            let disp = min maxMove (fMag * nodeSpeed)
                            match nodes[i].Pinned with
                            | Some p ->
                                xs[i] <- p.X; ys[i] <- p.Y
                                zs[i] <- zs[i] + (fz[i] / fMag) * disp
                            | None ->
                                xs[i] <- xs[i] + (fx[i] / fMag) * disp
                                ys[i] <- ys[i] + (fy[i] / fMag) * disp
                                zs[i] <- zs[i] + (fz[i] / fMag) * disp
                        prevFx[i] <- fx[i]; prevFy[i] <- fy[i]; prevFz[i] <- fz[i]

                // Final 3D sphere overlap resolution pass via Octree
                if config.PreventOverlap && n > 1 then
                    let maxRad = Array.max radii
                    for _ = 0 to 1 do
                        for i = 0 to n - 1 do pts[i] <- Point3.create xs[i] ys[i] zs[i]
                        let oct = Octree.buildWithCancellation token (fun i -> pts[i]) (fun _ -> 1.) (seq { 0 .. n - 1 })
                        Octree.visitIndexed n (fun i cell ->
                            let pi = pts[i]
                            let e = cell.Extent
                            let nearX = max (Bounds3.minX e) (min (Bounds3.maxX e) pi.X)
                            let nearY = max (Bounds3.minY e) (min (Bounds3.maxY e) pi.Y)
                            let nearZ = max (Bounds3.minZ e) (min (Bounds3.maxZ e) pi.Z)
                            let distToBox = Point3.distance pi (Point3.create nearX nearY nearZ)
                            if distToBox > radii[i] + maxRad then false
                            else
                                for item in cell.Items do
                                    let j = item.Value
                                    if j > i then
                                        let dx = xs[j] - xs[i]
                                        let dy = ys[j] - ys[i]
                                        let dz = zs[j] - zs[i]
                                        let d = sqrt (dx * dx + dy * dy + dz * dz)
                                        let overlap = radii[i] + radii[j] + 4. - d
                                        if overlap > 0. then
                                            let ux, uy, uz = if d < 1e-6 then 1., 0., 0. else dx / d, dy / d, dz / d
                                            let shift = overlap * 0.5
                                            xs[i] <- xs[i] - ux * shift; ys[i] <- ys[i] - uy * shift; zs[i] <- zs[i] - uz * shift
                                            xs[j] <- xs[j] + ux * shift; ys[j] <- ys[j] + uy * shift; zs[j] <- zs[j] + uz * shift
                                true) oct

                let nodeBoxes = Array.zeroCreate<Bounds3> n
                let posNodes =
                    [ for i in 0 .. n - 1 do
                        let nd = nodes[i]
                        let c = Point3.create xs[i] ys[i] zs[i]
                        let box = Kernels3D.nodeBox c nd.Size.Width nd.Size.Height nd.DepthSize
                        nodeBoxes[i] <- box
                        yield
                            { Id = nd.Id
                              Kind = nd.Kind
                              Center = c
                              Bounds = box
                              Radius = radii[i]
                              ScaleFactor = 1.
                              ParentGroup = nd.ParentGroup
                              Layer = None } ]

                let posArr = List.toArray posNodes
                let posGroups = Kernels3D.computeGroups3D resolved nodeBoxes
                let routed =
                    [ for a in assocs do
                        let u = nodeIdx[a.Source.NodeId]
                        let v = nodeIdx[a.Target.NodeId]
                        let pts =
                            if u = v then
                                let c = posArr[u].Center
                                let r = posArr[u].Radius
                                [| c; Point3.create (c.X + r * 1.4) (c.Y - r * 1.4) (c.Z + r * 0.6); c |]
                            else
                                [| posArr[u].Center; posArr[v].Center |]
                        yield
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              IsBackbone = true
                              Points = Point3Chain.ofOwnedArray pts
                              Waypoints = Array.toList pts } ]

                Ok
                    { Bounds = Kernels3D.computeBounds3D config.Bounds posNodes posGroups routed
                      Nodes = posNodes
                      Groups = posGroups
                      Associations = routed }

    /// <summary>Runs 3D Octree force-directed layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

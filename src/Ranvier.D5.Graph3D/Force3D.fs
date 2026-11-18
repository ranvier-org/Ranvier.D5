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
            let eff = DiagramGraph.effective resolved
            let nodes = eff.Nodes
            let assocs = eff.Associations
            let groups = eff.Groups
            let n = nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = Kernels3D.computeGroups3D resolved [||]; Associations = [] }
            else
                let nodeIdx = eff.ReversedIdMap
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
                    match nodes[i].Pinned with
                    | Some p -> xs[i] <- p.X; ys[i] <- p.Y
                    | None -> ()

                let mAll = assocs.Length
                let assocSrc = Array.zeroCreate<int> mAll
                let assocDst = Array.zeroCreate<int> mAll
                let edgeList = ResizeArray<struct(int * int * float)>(mAll)
                let degMass = Array.copy masses
                for e = 0 to mAll - 1 do
                    let a = assocs[e]
                    let u = nodeIdx[a.Source.NodeId]
                    let v = nodeIdx[a.Target.NodeId]
                    assocSrc[e] <- u
                    assocDst[e] <- v
                    if u <> v then
                        edgeList.Add(struct(u, v, a.Weight))
                        degMass[u] <- degMass[u] + a.Weight
                        degMass[v] <- degMass[v] + a.Weight
                let edges = edgeList.ToArray()

                let nodeIndices = Array.init n id
                // Group membership index lists for 3D compound group cohesion in O(V * depth + G)
                let gMembers =
                    if groups.Length = 0 then [||]
                    else
                        let gWithDepth = Array.init groups.Length (fun gi -> groups[gi], eff.GroupDepth[gi])
                        GraphKernels.subGroupMemberLists eff nodeIndices gWithDepth

                let fx = Array.zeroCreate<float> n
                let fy = Array.zeroCreate<float> n
                let fz = Array.zeroCreate<float> n
                let prevFx = Array.zeroCreate<float> n
                let prevFy = Array.zeroCreate<float> n
                let prevFz = Array.zeroCreate<float> n
                let swArr = Array.zeroCreate<float> n
                let octWs = if n > 36 && config.Theta > 0. || config.PreventOverlap then Octree.createWorkspace n else Unchecked.defaultof<_>
                let unitMass = if config.PreventOverlap && n > 1 then Array.create n 1. else [||]
                let kr = config.ScalingRatio * config.ScalingRatio * 4.5
                let thetaSq = config.Theta * config.Theta
                let isFR = (config.Model = EnergyModel.FruchtermanReingold)
                let preventOv = config.PreventOverlap

                for step = 0 to config.Iterations - 1 do
                    token.ThrowIfCancellationRequested()
                    Array.fill fx 0 n 0.
                    Array.fill fy 0 n 0.
                    Array.fill fz 0 n 0.

                    // 1. 3D Repulsion via zero-alloc arena Octree Barnes-Hut (Rocq: flat_cell_children_contiguous_slice_exact & forceatlas2_rational_repulsion_and_linear_forces_cancel_sqrt)
                    if n <= 36 || config.Theta = 0. then
                        for i = 0 to n - 1 do
                            for j = i + 1 to n - 1 do
                                let dx = xs[i] - xs[j]
                                let dy = ys[i] - ys[j]
                                let dz = zs[i] - zs[j]
                                let dSq = dx * dx + dy * dy + dz * dz
                                if dSq < 1e-8 then
                                    let a1 = float ((i * 29 + j * 13 + step) % 360) * (Math.PI / 180.)
                                    let mag = if isFR then kr * masses[i] * masses[j] else kr * degMass[i] * degMass[j]
                                    let rx, ry = cos a1 * mag, sin a1 * mag
                                    fx[i] <- fx[i] + rx; fy[i] <- fy[i] + ry
                                    fx[j] <- fx[j] - rx; fy[j] <- fy[j] - ry
                                elif not isFR && not preventOv && dSq >= 1. then
                                    let s = (kr * degMass[i] * degMass[j]) / dSq
                                    fx[i] <- fx[i] + dx * s; fy[i] <- fy[i] + dy * s; fz[i] <- fz[i] + dz * s
                                    fx[j] <- fx[j] - dx * s; fy[j] <- fy[j] - dy * s; fz[j] <- fz[j] - dz * s
                                else
                                    let rawD = sqrt dSq
                                    let effD =
                                        if preventOv then max 0.5 (rawD - radii[i] - radii[j])
                                        else max 1. rawD
                                    let s =
                                        if isFR then (kr * masses[i] * masses[j]) / (effD * effD * rawD)
                                        elif preventOv && rawD < radii[i] + radii[j] then (kr * degMass[i] * degMass[j] * 4.) / (max 0.5 rawD * rawD)
                                        else (kr * degMass[i] * degMass[j]) / (effD * rawD)
                                    fx[i] <- fx[i] + dx * s; fy[i] <- fy[i] + dy * s; fz[i] <- fz[i] + dz * s
                                    fx[j] <- fx[j] - dx * s; fy[j] <- fy[j] - dy * s; fz[j] <- fz[j] - dz * s
                    else
                        Octree.rebuildInPlace octWs token xs ys zs degMass n false
                        let octNodes = octWs.Nodes
                        let perm = octWs.Perm
                        let stack = octWs.Stack
                        for i = 0 to n - 1 do
                            let pix, piy, piz = xs[i], ys[i], zs[i]
                            let mi = degMass[i]
                            let ri = radii[i]
                            let mutable accX, accY, accZ = 0., 0., 0.
                            stack[0] <- 0
                            let mutable sp = 1
                            while sp > 0 do
                                sp <- sp - 1
                                let nd = octNodes[stack[sp]]
                                let dx = pix - nd.Cx
                                let dy = piy - nd.Cy
                                let dz = piz - nd.Cz
                                let dSq = dx * dx + dy * dy + dz * dz
                                let sz = max (nd.MaxX - nd.MinX) (max (nd.MaxY - nd.MinY) (nd.MaxZ - nd.MinZ))
                                let contains =
                                    pix >= nd.MinX && pix <= nd.MaxX
                                    && piy >= nd.MinY && piy <= nd.MaxY
                                    && piz >= nd.MinZ && piz <= nd.MaxZ
                                if not contains && dSq > 1e-8 && sz * sz < thetaSq * dSq then
                                    if not isFR then
                                        let s = (kr * mi * nd.Mass) / dSq
                                        accX <- accX + dx * s
                                        accY <- accY + dy * s
                                        accZ <- accZ + dz * s
                                    else
                                        let d = sqrt dSq
                                        let s = (kr * mi * nd.Mass) / (dSq * d)
                                        accX <- accX + dx * s
                                        accY <- accY + dy * s
                                        accZ <- accZ + dz * s
                                elif nd.ItemCount > 0 then
                                    for k = nd.ItemStart to nd.ItemStart + nd.ItemCount - 1 do
                                        let j = perm[k]
                                        if j <> i then
                                            let idx = pix - xs[j]
                                            let idy = piy - ys[j]
                                            let idz = piz - zs[j]
                                            let dSqLeaf = idx * idx + idy * idy + idz * idz
                                            let mj = degMass[j]
                                            if dSqLeaf < 1e-8 then
                                                let a1 = float ((i * 29 + j * 13 + step) % 360) * (Math.PI / 180.)
                                                let mag = kr * mi * mj
                                                accX <- accX + cos a1 * mag
                                                accY <- accY + sin a1 * mag
                                            elif not isFR && not preventOv && dSqLeaf >= 1. then
                                                let s = (kr * mi * mj) / dSqLeaf
                                                accX <- accX + idx * s
                                                accY <- accY + idy * s
                                                accZ <- accZ + idz * s
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
                                                accZ <- accZ + idz * s
                                elif nd.ChildCount > 0 then
                                    for c = nd.FirstChild + nd.ChildCount - 1 downto nd.FirstChild do
                                        stack[sp] <- c
                                        sp <- sp + 1
                            fx[i] <- fx[i] + accX
                            fy[i] <- fy[i] + accY
                            fz[i] <- fz[i] + accZ

                    // 2. 3D Edge attraction (sqrt-free in ForceAtlas2: (dx / d) * (w * d) = dx * w)
                    match config.Model with
                    | EnergyModel.ForceAtlas2 ->
                        for e = 0 to edges.Length - 1 do
                            let struct(u, v, w) = edges[e]
                            let dx = xs[v] - xs[u]
                            let dy = ys[v] - ys[u]
                            let dz = zs[v] - zs[u]
                            if dx * dx + dy * dy + dz * dz > 1e-12 then
                                let ax, ay, az = dx * w, dy * w, dz * w
                                fx[u] <- fx[u] + ax; fy[u] <- fy[u] + ay; fz[u] <- fz[u] + az
                                fx[v] <- fx[v] - ax; fy[v] <- fy[v] - ay; fz[v] <- fz[v] - az
                    | EnergyModel.LinLog ->
                        let sr = max 1. config.ScalingRatio
                        let invSr = 1. / sr
                        for e = 0 to edges.Length - 1 do
                            let struct(u, v, w) = edges[e]
                            let dx = xs[v] - xs[u]
                            let dy = ys[v] - ys[u]
                            let dz = zs[v] - zs[u]
                            let dSq = dx * dx + dy * dy + dz * dz
                            if dSq > 1e-12 then
                                let d = sqrt dSq
                                let s = (w * log (1. + d * invSr) * config.ScalingRatio) / d
                                fx[u] <- fx[u] + dx * s; fy[u] <- fy[u] + dy * s; fz[u] <- fz[u] + dz * s
                                fx[v] <- fx[v] - dx * s; fy[v] <- fy[v] - dy * s; fz[v] <- fz[v] - dz * s
                    | EnergyModel.FruchtermanReingold ->
                        let invSr3 = 1. / max 1. (config.ScalingRatio * 3.)
                        for e = 0 to edges.Length - 1 do
                            let struct(u, v, w) = edges[e]
                            let dx = xs[v] - xs[u]
                            let dy = ys[v] - ys[u]
                            let dz = zs[v] - zs[u]
                            let dSq = dx * dx + dy * dy + dz * dz
                            if dSq > 1e-12 then
                                let d = sqrt dSq
                                let s = w * d * invSr3
                                fx[u] <- fx[u] + dx * s; fy[u] <- fy[u] + dy * s; fz[u] <- fz[u] + dz * s
                                fx[v] <- fx[v] - dx * s; fy[v] <- fy[v] - dy * s; fz[v] <- fz[v] - dz * s

                    // 3. Central 3D gravity (sqrt-free) + Compound 3D group cohesion
                    let grav = config.Gravity
                    if grav > 0. then
                        for i = 0 to n - 1 do
                            let dx, dy, dz = cx0 - xs[i], cy0 - ys[i], cz0 - zs[i]
                            if dx * dx + dy * dy + dz * dz > 1e-12 then
                                let gScale = grav * degMass[i]
                                fx[i] <- fx[i] + dx * gScale
                                fy[i] <- fy[i] + dy * gScale
                                fz[i] <- fz[i] + dz * gScale

                    if groups.Length > 0 && config.CompoundGravity > 0. then
                        for gi = 0 to groups.Length - 1 do
                            let mems = gMembers[gi]
                            if mems.Length > 1 then
                                let mutable sx, sy, sz = 0., 0., 0.
                                for k = 0 to mems.Length - 1 do
                                    let ni = mems[k]
                                    sx <- sx + xs[ni]; sy <- sy + ys[ni]; sz <- sz + zs[ni]
                                let invM = 1. / float mems.Length
                                let mx, my, mz = sx * invM, sy * invM, sz * invM
                                for k = 0 to mems.Length - 1 do
                                    let ni = mems[k]
                                    let cg = config.CompoundGravity * degMass[ni]
                                    fx[ni] <- fx[ni] + (mx - xs[ni]) * cg
                                    fy[ni] <- fy[ni] + (my - ys[ni]) * cg
                                    fz[ni] <- fz[ni] + (mz - zs[ni]) * cg

                    // 4. Adaptive ForceAtlas2 3D swing/traction velocity step
                    let mutable totalSwing = 0.
                    let mutable totalTraction = 0.
                    for i = 0 to n - 1 do
                        let swx, swy, swz = fx[i] - prevFx[i], fy[i] - prevFy[i], fz[i] - prevFz[i]
                        let trx, try_, trz = fx[i] + prevFx[i], fy[i] + prevFy[i], fz[i] + prevFz[i]
                        let sw = sqrt (swx * swx + swy * swy + swz * swz)
                        swArr[i] <- sw
                        totalSwing <- totalSwing + degMass[i] * sw
                        totalTraction <- totalTraction + degMass[i] * 0.5 * sqrt (trx * trx + try_ * try_ + trz * trz)

                    let cooling = 1. - 0.65 * (float step / float config.Iterations)
                    let globalSpeed =
                        if step = 0 || totalSwing <= 1e-9 then 0.25 * cooling
                        else min (2.0 * cooling) (0.6 * totalTraction / totalSwing * cooling)
                    let maxMove = config.ScalingRatio * 4. * cooling + 2.

                    for i = 0 to n - 1 do
                        let fxi, fyi, fzi = fx[i], fy[i], fz[i]
                        let fMagSq = fxi * fxi + fyi * fyi + fzi * fzi
                        if fMagSq > 1e-18 then
                            let fMag = sqrt fMagSq
                            let nodeSpeed = globalSpeed / (1. + globalSpeed * sqrt swArr[i])
                            let disp = min maxMove (fMag * nodeSpeed)
                            let scale = disp / fMag
                            match nodes[i].Pinned with
                            | Some p ->
                                xs[i] <- p.X; ys[i] <- p.Y
                                zs[i] <- zs[i] + fzi * scale
                            | None ->
                                xs[i] <- xs[i] + fxi * scale
                                ys[i] <- ys[i] + fyi * scale
                                zs[i] <- zs[i] + fzi * scale
                        prevFx[i] <- fxi; prevFy[i] <- fyi; prevFz[i] <- fzi

                // Final 3D sphere overlap resolution pass via zero-alloc arena Octree
                if preventOv && n > 1 then
                    let mutable maxRad = radii[0]
                    for i = 1 to n - 1 do if radii[i] > maxRad then maxRad <- radii[i]
                    for _ = 0 to 1 do
                        Octree.rebuildInPlace octWs token xs ys zs unitMass n false
                        let octNodes = octWs.Nodes
                        let perm = octWs.Perm
                        let stack = octWs.Stack
                        for i = 0 to n - 1 do
                            let pix, piy, piz = xs[i], ys[i], zs[i]
                            let rLim = radii[i] + maxRad + 4.
                            let rLimSq = rLim * rLim
                            stack[0] <- 0
                            let mutable sp = 1
                            while sp > 0 do
                                sp <- sp - 1
                                let nd = octNodes[stack[sp]]
                                let dxB = max nd.MinX (min nd.MaxX pix) - pix
                                let dyB = max nd.MinY (min nd.MaxY piy) - piy
                                let dzB = max nd.MinZ (min nd.MaxZ piz) - piz
                                if dxB * dxB + dyB * dyB + dzB * dzB <= rLimSq then
                                    if nd.ItemCount > 0 then
                                        for k = nd.ItemStart to nd.ItemStart + nd.ItemCount - 1 do
                                            let j = perm[k]
                                            if j > i then
                                                let dx = xs[j] - xs[i]
                                                let dy = ys[j] - ys[i]
                                                let dz = zs[j] - zs[i]
                                                let req = radii[i] + radii[j] + 4.
                                                let dSq = dx * dx + dy * dy + dz * dz
                                                if dSq < req * req then
                                                    let d = sqrt dSq
                                                    let overlap = req - d
                                                    if overlap > 0. then
                                                        let ux, uy, uz = if d < 1e-6 then 1., 0., 0. else dx / d, dy / d, dz / d
                                                        let shift = overlap * 0.5
                                                        xs[i] <- xs[i] - ux * shift; ys[i] <- ys[i] - uy * shift; zs[i] <- zs[i] - uz * shift
                                                        xs[j] <- xs[j] + ux * shift; ys[j] <- ys[j] + uy * shift; zs[j] <- zs[j] + uz * shift
                                    elif nd.ChildCount > 0 then
                                        for c = nd.FirstChild + nd.ChildCount - 1 downto nd.FirstChild do
                                            stack[sp] <- c
                                            sp <- sp + 1

                let hasGroups = groups.Length > 0
                let nodeBoxes = if hasGroups then Array.zeroCreate<Bounds3> n else [||]
                let centers = Array.zeroCreate<Point3> n
                let mutable posNodes = []
                for i = n - 1 downto 0 do
                    let nd = nodes[i]
                    let c = Point3.create xs[i] ys[i] zs[i]
                    centers[i] <- c
                    let box = Kernels3D.nodeBox c nd.Size.Width nd.Size.Height nd.DepthSize
                    if hasGroups then nodeBoxes[i] <- box
                    posNodes <-
                        { Id = nd.Id
                          Kind = nd.Kind
                          Center = c
                          Bounds = box
                          Radius = radii[i]
                          ScaleFactor = 1.
                          ParentGroup = nd.ParentGroup
                          Layer = None } :: posNodes

                let posGroups = if hasGroups then Kernels3D.computeGroups3D resolved nodeBoxes else []
                let mutable routed = []
                for e = mAll - 1 downto 0 do
                    let a = assocs[e]
                    let u, v = assocSrc[e], assocDst[e]
                    let ptsArr, wps =
                        if u = v then
                            let c = centers[u]
                            let r = radii[u]
                            let lp = Point3.create (c.X + r * 1.4) (c.Y - r * 1.4) (c.Z + r * 0.6)
                            [| c; lp; c |], [ c; lp; c ]
                        else
                            let cu, cv = centers[u], centers[v]
                            [| cu; cv |], [ cu; cv ]
                    routed <-
                        { Id = a.Id
                          Source = a.Source
                          Target = a.Target
                          Kind = a.Kind
                          IsBackbone = true
                          Points = Point3Chain.ofOwnedArray ptsArr
                          Waypoints = wps } :: routed

                Ok
                    { Bounds = Kernels3D.computeBounds3D config.Bounds posNodes posGroups routed
                      Nodes = posNodes
                      Groups = posGroups
                      Associations = routed }

    /// <summary>Runs 3D Octree force-directed layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

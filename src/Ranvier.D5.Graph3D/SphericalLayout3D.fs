namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Configuration for <c>SphericalStress3D</c> Riemannian manifold graph layouts on <c>S^2</c>.</summary>
type SphericalLayoutConfig =
    { /// <summary>3D world center of the target layout sphere.</summary>
      Center: Point3
      /// <summary>Positive sphere radius in layout units.</summary>
      Radius: float
      /// <summary>Riemannian SGD epoch count (<c>1..512</c>; default <c>45</c>).</summary>
      Iterations: int
      /// <summary>Number of slerped great-circle polyline segments per association (<c>1..256</c>; default <c>10</c>).</summary>
      ArcSegments: int
      /// <summary>Deterministic Fisher-Yates pair-shuffle seed.</summary>
      Seed: uint64 }

/// <summary>Constructors and defaults for <c>SphericalLayoutConfig</c>.</summary>
[<RequireQualifiedAccess>]
module SphericalLayoutConfig =
    /// <summary>Default spherical layout settings: origin center, radius 260, 45 Riemannian SGD epochs, 10 arc segments and seed 42.</summary>
    let defaultValue : SphericalLayoutConfig =
        { Center = Point3.create 0. 0. 0.
          Radius = 260.
          Iterations = 45
          ArcSegments = 10
          Seed = 42UL }

    /// <summary>Creates a spherical layout configuration with explicit sphere <c>center</c> and <c>radius</c>.</summary>
    let create (center: Point3) (radius: float) : SphericalLayoutConfig =
        { defaultValue with Center = center; Radius = radius }

module private SphericalKernels =
    let inline normalizeVec (x: float) (y: float) (z: float) : struct(float * float * float) =
        let nSq = x * x + y * y + z * z
        if nSq <= 1e-24 then struct(0., 1., 0.)
        else
            let inv = 1. / sqrt nSq
            struct(x * inv, y * inv, z * inv)

    let inline orthoUnit (ux: float) (uy: float) (uz: float) (seedAngle: float) : struct(float * float * float) =
        // Orthonormal tangent basis (e1, e2) perpendicular to unit vector u
        let rx, ry, rz =
            if abs ux < 0.85 then 1., 0., 0.
            else 0., 1., 0.
        let c1x = uy * rz - uz * ry
        let c1y = uz * rx - ux * rz
        let c1z = ux * ry - uy * rx
        let struct(e1x, e1y, e1z) = normalizeVec c1x c1y c1z
        let e2x = uy * e1z - uz * e1y
        let e2y = uz * e1x - ux * e1z
        let e2z = ux * e1y - uy * e1x
        let ca, sa = cos seedAngle, sin seedAngle
        let tx = ca * e1x + sa * e2x
        let ty = ca * e1y + sa * e2y
        let tz = ca * e1z + sa * e2z
        normalizeVec tx ty tz

    /// Computes BFS/degree-ordered deterministic Fibonacci unit vectors on S^2 for vertices 0 .. n - 1.
    let initFibonacciUnitVectors (csr: CsrGraph) : struct(float[] * float[] * float[]) =
        let n = csr.NodeCount
        let ux = Array.zeroCreate<float> n
        let uy = Array.zeroCreate<float> n
        let uz = Array.zeroCreate<float> n
        if n > 0 then
            let deg = Array.init n (fun i -> csr.UndirOffsets[i + 1] - csr.UndirOffsets[i])
            let visited = Array.zeroCreate<bool> n
            let bfsOrder = Array.zeroCreate<int> n
            let queue = Array.zeroCreate<int> n
            let mutable rank = 0

            // Order starting hubs by descending degree (tie-break by vertex index) so connected neighborhoods stay adjacent on the spiral
            let seedOrder =
                Array.init n id
                |> Array.sortWith (fun a b ->
                    let c = compare deg[b] deg[a]
                    if c <> 0 then c else compare a b)

            for sIdx = 0 to n - 1 do
                let start = seedOrder[sIdx]
                if not visited[start] then
                    visited[start] <- true
                    let mutable head = 0
                    let mutable tail = 0
                    queue[tail] <- start
                    tail <- tail + 1
                    while head < tail do
                        let u = queue[head]
                        head <- head + 1
                        bfsOrder[u] <- rank
                        rank <- rank + 1

                        let off0 = csr.UndirOffsets[u]
                        let off1 = csr.UndirOffsets[u + 1]
                        let cnt = off1 - off0
                        if cnt = 1 then
                            let v = csr.UndirTargets[off0]
                            if not visited[v] then
                                visited[v] <- true
                                queue[tail] <- v
                                tail <- tail + 1
                        elif cnt > 1 then
                            let nbrs = Array.init cnt (fun k -> csr.UndirTargets[off0 + k])
                            let sortedNbrs =
                                nbrs
                                |> Array.sortWith (fun a b ->
                                    let c = compare deg[b] deg[a]
                                    if c <> 0 then c else compare a b)
                            for k = 0 to cnt - 1 do
                                let v = sortedNbrs[k]
                                if not visited[v] then
                                    visited[v] <- true
                                    queue[tail] <- v
                                    tail <- tail + 1

            let goldenAngle = Math.PI * (3. - sqrt 5.)
            for i = 0 to n - 1 do
                let k = bfsOrder[i]
                let y = if n = 1 then 0. else 1. - 2. * (float k + 0.5) / float n
                let ring = sqrt (max 0. (1. - y * y))
                let theta = float k * goldenAngle
                let struct(nx, ny, nz) = normalizeVec (ring * cos theta) y (ring * sin theta)
                ux[i] <- nx; uy[i] <- ny; uz[i] <- nz
        struct(ux, uy, uz)

    /// Samples a great-circle arc of `arcSegments + 1` points strictly on the sphere (center, radius).
    let sampleGreatCircleArc
        (center: Point3)
        (radius: float)
        (arcSegments: int)
        (uIdx: int)
        (vIdx: int)
        (ux: float[])
        (uy: float[])
        (uz: float[])
        : Point3[] * Point3 list =
        let count = arcSegments + 1
        let pts = Array.zeroCreate<Point3> count
        let inline onSphere (vx: float) (vy: float) (vz: float) =
            let struct(nx, ny, nz) = normalizeVec vx vy vz
            Point3.create (center.X + radius * nx) (center.Y + radius * ny) (center.Z + radius * nz)

        if uIdx = vIdx then
            // Self-loop strictly on the sphere surface starting and ending at node u
            let ax, ay, az = ux[uIdx], uy[uIdx], uz[uIdx]
            let pSelf = onSphere ax ay az
            pts[0] <- pSelf
            pts[arcSegments] <- pSelf
            if arcSegments >= 2 then
                let struct(e1x, e1y, e1z) = orthoUnit ax ay az 0.35
                let e2x = ay * e1z - az * e1y
                let e2y = az * e1x - ax * e1z
                let e2z = ax * e1y - ay * e1x
                for k = 1 to arcSegments - 1 do
                    let t = float k / float arcSegments
                    let loopAz = 2. * Math.PI * t
                    let flare = 0.08 * sin (Math.PI * t)
                    let ca, sa = cos flare, sin flare
                    let tx = cos loopAz * e1x + sin loopAz * e2x
                    let ty = cos loopAz * e1y + sin loopAz * e2y
                    let tz = cos loopAz * e1z + sin loopAz * e2z
                    pts[k] <- onSphere (ca * ax + sa * tx) (ca * ay + sa * ty) (ca * az + sa * tz)
        else
            let ax, ay, az = ux[uIdx], uy[uIdx], uz[uIdx]
            let bx, by, bz = ux[vIdx], uy[vIdx], uz[vIdx]
            pts[0] <- onSphere ax ay az
            pts[arcSegments] <- onSphere bx by bz
            if arcSegments >= 2 then
                let dot = max -1. (min 1. (ax * bx + ay * by + az * bz))
                let omega = acos dot
                if omega <= 1e-6 then
                    for k = 1 to arcSegments - 1 do
                        let t = float k / float arcSegments
                        pts[k] <- onSphere ((1. - t) * ax + t * bx) ((1. - t) * ay + t * by) ((1. - t) * az + t * bz)
                elif omega >= Math.PI - 1e-6 then
                    // Antipodal meridian free of 0/0 slerp singularity
                    let seed = float ((uIdx * 17 + vIdx * 31) % 360) * (Math.PI / 180.)
                    let struct(mx, my, mz) = orthoUnit ax ay az seed
                    for k = 1 to arcSegments - 1 do
                        let ang = (float k / float arcSegments) * omega
                        let ca, sa = cos ang, sin ang
                        pts[k] <- onSphere (ca * ax + sa * mx) (ca * ay + sa * my) (ca * az + sa * mz)
                else
                    let invSin = 1. / sin omega
                    for k = 1 to arcSegments - 1 do
                        let t = float k / float arcSegments
                        let w0 = sin ((1. - t) * omega) * invSin
                        let w1 = sin (t * omega) * invSin
                        pts[k] <- onSphere (w0 * ax + w1 * bx) (w0 * ay + w1 * by) (w0 * az + w1 * bz)

        let mutable wps = []
        for k = count - 1 downto 0 do
            wps <- pts[k] :: wps
        pts, wps

    let assembleSphericalLayout
        (center: Point3)
        (radius: float)
        (arcSegments: int)
        (resolved: DiagramGraph)
        (eff: EffectiveDiagram)
        (csr: CsrGraph)
        (ux: float[])
        (uy: float[])
        (uz: float[])
        : DiagramLayout3D =
        let nodes = eff.Nodes
        let assocs = eff.Associations
        let n = nodes.Length
        let sphereBox = Kernels3D.nodeBox center (2. * radius) (2. * radius) (2. * radius)
        if n = 0 then
            { Bounds = sphereBox
              Nodes = []
              Groups = Kernels3D.computeGroups3D resolved [||]
              Associations = [] }
        else
            let _, _, _, depths, isTreeAssoc =
                SpanningTree.extractIndexed SpanningTreeStrategy.Bfs None eff csr
            let hasGroups = eff.Groups.Length > 0
            let nodeBoxes = if hasGroups then Array.zeroCreate<Bounds3> n else [||]
            let mutable posNodes = []

            for i = n - 1 downto 0 do
                let nd = nodes[i]
                let struct(nx, ny, nz) = normalizeVec ux[i] uy[i] uz[i]
                ux[i] <- nx; uy[i] <- ny; uz[i] <- nz
                let pt = Point3.create (center.X + radius * nx) (center.Y + radius * ny) (center.Z + radius * nz)
                let box = Kernels3D.nodeBox pt nd.Size.Width nd.Size.Height nd.DepthSize
                let rad = 0.5 * sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height + nd.DepthSize * nd.DepthSize)
                if hasGroups then nodeBoxes[i] <- box
                posNodes <-
                    { Id = nd.Id
                      Kind = nd.Kind
                      Center = pt
                      Bounds = box
                      Radius = rad
                      ScaleFactor = 1.
                      ParentGroup = nd.ParentGroup
                      Layer = Some depths[i] } :: posNodes

            let groups = if hasGroups then Kernels3D.computeGroups3D resolved nodeBoxes else []
            let mutable routed = []
            for e = assocs.Length - 1 downto 0 do
                let a = assocs[e]
                let u, v = csr.Src[e], csr.Dst[e]
                let pts, wps = sampleGreatCircleArc center radius arcSegments u v ux uy uz
                routed <-
                    { Id = a.Id
                      Source = a.Source
                      Target = a.Target
                      Kind = a.Kind
                      IsBackbone = isTreeAssoc[e]
                      Points = Point3Chain.ofOwnedArray pts
                      Waypoints = wps } :: routed

            { Bounds = Kernels3D.computeBounds3D sphereBox posNodes groups routed
              Nodes = posNodes
              Groups = groups
              Associations = routed }

    /// Builds spherical geodesic stress pairs (i, j, thetaStar, weight) across connected and disconnected components.
    let buildSphericalStressPairs
        (token: System.Threading.CancellationToken)
        (csr: CsrGraph)
        (exactLimit: int)
        (maxPivots: int)
        : struct(int * int * float * float)[] =
        let n = csr.NodeCount
        if n <= 1 then [||]
        elif n <= exactLimit then
            let rawPairs = ResizeArray<struct(int * int * int)>(n * (n - 1) / 2)
            let dist = Array.zeroCreate<int> n
            let q = Array.zeroCreate<int> n
            let mutable maxHops = 0

            for s = 0 to n - 2 do
                Kernels3D.check token s
                Array.fill dist 0 n -1
                dist[s] <- 0
                let mutable head = 0
                let mutable tail = 0
                q[tail] <- s
                tail <- tail + 1
                while head < tail do
                    let u = q[head]
                    head <- head + 1
                    let dNext = dist[u] + 1
                    for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                        let v = csr.UndirTargets[idx]
                        if dist[v] < 0 then
                            dist[v] <- dNext
                            if dNext > maxHops then maxHops <- dNext
                            q[tail] <- v
                            tail <- tail + 1
                for t = s + 1 to n - 1 do
                    rawPairs.Add(struct(s, t, dist[t]))

            let dMax = max 1. (float maxHops)
            let denom = dMax + 0.5
            let maxTheta = Math.PI - 0.08
            let count = rawPairs.Count
            let out = Array.zeroCreate<struct(int * int * float * float)> count
            for k = 0 to count - 1 do
                let struct(i, j, hops) = rawPairs[k]
                let dij = if hops > 0 then float hops else dMax + 2.
                let thetaStar = min maxTheta (Math.PI * dij / denom)
                let wij = 1. / max 0.02 (thetaStar * thetaStar)
                out[k] <- struct(i, j, thetaStar, wij)
            out
        else
            // Sparse landmark + edge geodesic stress pairs (O(P * (V + E)), zero HashSet allocations)
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
                        for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                            let v = csr.UndirTargets[idx]
                            if compOf[v] < 0 then compOf[v] <- cid; q[t] <- v; t <- t + 1
                    let mems = Array.sub q 0 t
                    comps.Add mems
                    cid <- cid + 1

            // Select up to maxPivots global landmarks ensuring every component gets at least one pivot
            let pivots = ResizeArray<int>(maxPivots)
            for ci = 0 to comps.Count - 1 do
                if pivots.Count < maxPivots then
                    pivots.Add(comps[ci][0])

            let minDist = Array.create n Int32.MaxValue
            let bfsGlobal (src: int) (outDist: int[]) : int =
                Array.fill outDist 0 n -1
                outDist[src] <- 0
                let mutable h, t = 0, 0
                let mutable localMax = 0
                q[t] <- src; t <- t + 1
                while h < t do
                    Kernels3D.check token h
                    let u = q[h]
                    h <- h + 1
                    let dn = outDist[u] + 1
                    for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                        let v = csr.UndirTargets[idx]
                        if outDist[v] < 0 then
                            outDist[v] <- dn
                            if dn > localMax then localMax <- dn
                            q[t] <- v; t <- t + 1
                localMax

            let pRows = ResizeArray<int[]>(maxPivots)
            let mutable maxHops = 0
            for pi = 0 to pivots.Count - 1 do
                let row = Array.zeroCreate<int> n
                let lm = bfsGlobal pivots[pi] row
                if lm > maxHops then maxHops <- lm
                for v = 0 to n - 1 do
                    if row[v] >= 0 && row[v] < minDist[v] then minDist[v] <- row[v]
                pRows.Add row

            let targetPivots = min n maxPivots
            let mutable morePivots = true
            while morePivots && pivots.Count < targetPivots do
                token.ThrowIfCancellationRequested()
                let mutable bestV = -1
                let mutable bestD = 0
                for v = 0 to n - 1 do
                    if minDist[v] < Int32.MaxValue && minDist[v] > bestD then
                        bestD <- minDist[v]
                        bestV <- v
                if bestV >= 0 then
                    pivots.Add bestV
                    let row = Array.zeroCreate<int> n
                    let lm = bfsGlobal bestV row
                    if lm > maxHops then maxHops <- lm
                    for v = 0 to n - 1 do
                        if row[v] >= 0 && row[v] < minDist[v] then minDist[v] <- row[v]
                    pRows.Add row
                else
                    morePivots <- false

            let isPivot = Array.zeroCreate<bool> n
            for pi = 0 to pivots.Count - 1 do isPivot[pivots[pi]] <- true

            let dMax = max 1. (float maxHops)
            let denom = dMax + 0.5
            let maxTheta = Math.PI - 0.08
            let pairs = ResizeArray<struct(int * int * float * float)>(pivots.Count * n + csr.EdgeCount)

            let inline makePair (a: int) (b: int) (dij: float) =
                let th = min maxTheta (Math.PI * dij / denom)
                let w = 1. / max 0.02 (th * th)
                struct(a, b, th, w)

            // 1. All undirected edges (hops = 1)
            for u = 0 to n - 1 do
                for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                    let v = csr.UndirTargets[idx]
                    if u < v then
                        pairs.Add(makePair u v 1.)

            // 2. Non-edge landmark pairs (hops > 1 within component, or dMax + 2 across components)
            for pi = 0 to pivots.Count - 1 do
                let p = pivots[pi]
                let row = pRows[pi]
                for v = 0 to n - 1 do
                    if v <> p && row[v] <> 1 && (not isPivot[v] || p < v) then
                        // Check non-adjacent when unreachable (row[v] < 0 means non-adjacent automatically!)
                        let a, b = if p < v then p, v else v, p
                        let dij = if row[v] > 1 then float row[v] else dMax + 2.
                        pairs.Add(makePair a b dij)

            pairs.ToArray()

/// <summary>Deterministic golden-spiral spherical Fibonacci graph layout on <c>S^2</c> ordered by BFS/degree.</summary>
[<RequireQualifiedAccess>]
module SphericalFibonacci3D =
    /// <summary>Places all graph vertices strictly on the sphere <c>(center, radius)</c> along a BFS/degree-ordered Fibonacci spiral with slerped great-circle arcs.</summary>
    let layout
        (center: Point3)
        (radius: float)
        (arcSegments: int)
        (graph: DiagramGraph)
        : Result<DiagramLayout3D, LayoutError list> =
        if not (Double.IsFinite center.X && Double.IsFinite center.Y && Double.IsFinite center.Z && Double.IsFinite radius)
           || radius <= 0.
           || arcSegments < 1 || arcSegments > 256 then
            Error [ InvalidObservation "SphericalFibonacci3D requires finite Center, positive Radius and ArcSegments in [1, 256]." ]
        else
            let resolved = DiagramGraph.resolveCollapsed graph
            let eff = DiagramGraph.effective resolved
            let csr = GraphKernels.buildCsr eff
            let struct(ux, uy, uz) = SphericalKernels.initFibonacciUnitVectors csr
            Ok(SphericalKernels.assembleSphericalLayout center radius arcSegments resolved eff csr ux uy uz)

/// <summary>Riemannian Stochastic Gradient Descent geodesic stress layout on <c>S^2</c> (Kobourov &amp; Wampler 2005; Zheng, Pawar &amp; Goodman 2019).</summary>
[<RequireQualifiedAccess>]
module SphericalStress3D =
    /// <summary>Computes a Riemannian SGD spherical geodesic stress layout on <c>S^2</c> with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: SphericalLayoutConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout3D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.Center.X && Double.IsFinite config.Center.Y && Double.IsFinite config.Center.Z && Double.IsFinite config.Radius)
           || config.Radius <= 0.
           || config.Iterations < 1 || config.Iterations > 512
           || config.ArcSegments < 1 || config.ArcSegments > 256 then
            Error [ InvalidObservation "SphericalStress3D requires finite Center, positive Radius, Iterations in [1, 512] and ArcSegments in [1, 256]." ]
        else
            let resolved = DiagramGraph.resolveCollapsed graph
            let eff = DiagramGraph.effective resolved
            let csr = GraphKernels.buildCsr eff
            let n = csr.NodeCount
            let struct(ux, uy, uz) = SphericalKernels.initFibonacciUnitVectors csr

            if n >= 2 then
                let pairs = SphericalKernels.buildSphericalStressPairs token csr 250 48
                let pCount = pairs.Length
                if pCount > 0 then
                    let schedule = Stress2D.annealingSchedule pairs config.Iterations 0.05
                    let order = Array.init pCount id
                    let mutable rng = RandomState.create config.Seed

                    for ep = 0 to schedule.Length - 1 do
                        token.ThrowIfCancellationRequested()
                        let eta = schedule[ep]

                        // Fisher-Yates shuffle
                        for k = pCount - 1 downto 1 do
                            let struct(bits, nextRng) = RandomState.nextUInt64 rng
                            rng <- nextRng
                            let sw = int (bits % uint64 (k + 1))
                            let tmp = order[k]
                            order[k] <- order[sw]
                            order[sw] <- tmp

                        for k = 0 to pCount - 1 do
                            Kernels3D.check token k
                            let struct(i, j, thetaStar, wij) = pairs[order[k]]
                            let uix, uiy, uiz = ux[i], uy[i], uz[i]
                            let ujx, ujy, ujz = ux[j], uy[j], uz[j]
                            let dot = max -1. (min 1. (uix * ujx + uiy * ujy + uiz * ujz))
                            let theta = acos dot
                            let mu = min 1. (eta * wij)
                            let delta = 0.5 * mu * (theta - thetaStar)

                            if abs delta > 1e-12 then
                                let sinTheta = sqrt (max 0. (1. - dot * dot))
                                let mutable tix, tiy, tiz = 0., 0., 0.
                                let mutable tjx, tjy, tjz = 0., 0., 0.

                                if sinTheta > 1e-8 then
                                    // Outward Riemannian geodesic gradients in T_{u_i} S^2 and T_{u_j} S^2
                                    let invSin = 1. / sinTheta
                                    tix <- (uix * dot - ujx) * invSin
                                    tiy <- (uiy * dot - ujy) * invSin
                                    tiz <- (uiz * dot - ujz) * invSin
                                    tjx <- (ujx * dot - uix) * invSin
                                    tjy <- (ujy * dot - uiy) * invSin
                                    tjz <- (ujz * dot - uiz) * invSin
                                else
                                    let seedAng = float ((i * 19 + j * 31 + ep) % 360) * (Math.PI / 180.)
                                    let struct(ox, oy, oz) = SphericalKernels.orthoUnit uix uiy uiz seedAng
                                    tix <- ox; tiy <- oy; tiz <- oz
                                    if dot >= 0. then
                                        // Coincident (theta ~ 0): opposite tangents separate points
                                        tjx <- -ox; tjy <- -oy; tjz <- -oz
                                    else
                                        // Antipodal (theta ~ pi): shared meridian tangent pulls both toward common hemisphere
                                        tjx <- ox; tjy <- oy; tjz <- oz

                                let cd, sd = cos delta, sin delta
                                let struct(nix, niy, niz) =
                                    SphericalKernels.normalizeVec (uix * cd - tix * sd) (uiy * cd - tiy * sd) (uiz * cd - tiz * sd)
                                let struct(njx, njy, njz) =
                                    SphericalKernels.normalizeVec (ujx * cd - tjx * sd) (ujy * cd - tjy * sd) (ujz * cd - tjz * sd)
                                ux[i] <- nix; uy[i] <- niy; uz[i] <- niz
                                ux[j] <- njx; uy[j] <- njy; uz[j] <- njz

            Ok(SphericalKernels.assembleSphericalLayout config.Center config.Radius config.ArcSegments resolved eff csr ux uy uz)

    /// <summary>Computes a Riemannian SGD spherical geodesic stress layout on <c>S^2</c> without cancellation.</summary>
    let layout (config: SphericalLayoutConfig) (graph: DiagramGraph) : Result<DiagramLayout3D, LayoutError list> =
        layoutWithCancellation System.Threading.CancellationToken.None config graph

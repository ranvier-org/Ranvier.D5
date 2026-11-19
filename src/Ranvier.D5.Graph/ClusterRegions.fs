namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>A named 2D cluster or set boundary region composed of one or more closed polygons and total area.</summary>
type ClusterRegion =
    { /// <summary>Group, community or set identifier.</summary>
      GroupId: string
      /// <summary>Closed boundary polygons (without repeated closing vertex) in layout coordinates.</summary>
      Polygons: Point list list
      /// <summary>Total positive area enclosed by the cluster polygons in square layout units.</summary>
      Area: float }

/// <summary>Gansner &amp; Hu (2010) <c>PRISM</c> Delaunay proximity-stress node overlap removal with exact zero-overlap sweep guarantee.</summary>
[<RequireQualifiedAccess>]
module PrismOverlap =
    let private hasAnyStrictOverlap
        (cx: float[])
        (cy: float[])
        (hw: float[])
        (hh: float[])
        (pad: float)
        (pinned: bool[])
        (orderBuf: int[])
        (overlapPairs: ResizeArray<struct(int * int)>)
        : bool =
        let n = cx.Length
        overlapPairs.Clear()
        if n <= 1 then false
        else
            let padHalf = pad * 0.5
            Array.sortInPlaceWith (fun a b -> compare (cx[a] - hw[a]) (cx[b] - hw[b])) orderBuf
            for oi = 0 to n - 2 do
                let i = orderBuf[oi]
                let rightLimit = cx[i] + hw[i] + padHalf
                let mutable oj = oi + 1
                let mutable scanning = true
                while scanning && oj < n do
                    let j = orderBuf[oj]
                    let leftJ = cx[j] - hw[j] - padHalf
                    if leftJ >= rightLimit - 1e-7 then
                        scanning <- false
                    else
                        if not (pinned[i] && pinned[j]) then
                            let reqX = hw[i] + hw[j] + pad
                            let reqY = hh[i] + hh[j] + pad
                            if abs (cx[i] - cx[j]) < reqX - 1e-7 && abs (cy[i] - cy[j]) < reqY - 1e-7 then
                                overlapPairs.Add(struct(min i j, max i j))
                        oj <- oj + 1
            overlapPairs.Count > 0

    let private solvePrismCore
        (token: System.Threading.CancellationToken)
        (padding: float)
        (maxIterations: int)
        (cx: float[])
        (cy: float[])
        (hw: float[])
        (hh: float[])
        (pinned: bool[])
        : unit =
        let n = cx.Length
        if n > 1 then
            let orderBuf = Array.init n (fun i -> i)
            let overlapPairs = ResizeArray<struct(int * int)>(n * 2)
            let edgeSet = HashSet<uint64>()
            let eU = ResizeArray<int>(n * 4)
            let eV = ResizeArray<int>(n * 4)
            let eIdeal = ResizeArray<float>(n * 4)
            let eWeight = ResizeArray<float>(n * 4)
            let numX = Array.zeroCreate<float> n
            let numY = Array.zeroCreate<float> n
            let denW = Array.zeroCreate<float> n

            let inline addUniqueEdge (a: int) (b: int) =
                if a <> b then
                    let u, v = min a b, max a b
                    let key = (uint64 u <<< 32) ||| uint64 (uint32 v)
                    if edgeSet.Add key then
                        eU.Add u
                        eV.Add v

            let mutable iter = 0
            let mutable stillOverlapping = hasAnyStrictOverlap cx cy hw hh padding pinned orderBuf overlapPairs

            while iter < maxIterations && stillOverlapping do
                token.ThrowIfCancellationRequested()
                edgeSet.Clear()
                eU.Clear()
                eV.Clear()
                eIdeal.Clear()
                eWeight.Clear()

                // 1. Build Delaunay triangulation on current box centers
                let pts = [ for i in 0 .. n - 1 -> Point.create cx[i] cy[i] ]
                let mesh = Delaunay.build pts
                let uniqueSites = Delaunay.sites mesh
                let siteCount = uniqueSites.Length
                let siteRep = Array.zeroCreate<int> siteCount

                for s = 0 to siteCount - 1 do
                    let inp = Delaunay.inputIndices s mesh
                    siteRep[s] <- inp[0]
                    if inp.Length > 1 then
                        for k = 0 to inp.Length - 2 do
                            addUniqueEdge inp[k] inp[k + 1]

                for s = 0 to siteCount - 1 do
                    let nbrs = Delaunay.neighbors s mesh
                    let u = siteRep[s]
                    for k = 0 to nbrs.Length - 1 do
                        let ns = nbrs[k]
                        if s < ns then
                            addUniqueEdge u siteRep[ns]

                // Include any strict box overlaps missed by center-only Delaunay (e.g. wide aspect-ratio boxes)
                for k = 0 to overlapPairs.Count - 1 do
                    let struct(u, v) = overlapPairs[k]
                    addUniqueEdge u v

                // 2. Compute exact PRISM ray-box overlap factors t_ij along proximity edges
                let m = eU.Count
                for e = 0 to m - 1 do
                    let u = eU[e]
                    let v = eV[e]
                    let dx = cx[v] - cx[u]
                    let dy = cy[v] - cy[u]
                    let adx, ady = abs dx, abs dy
                    let dist = Double.Hypot(dx, dy)
                    let reqX = hw[u] + hw[v] + padding
                    let reqY = hh[u] + hh[v] + padding
                    if adx < reqX - 1e-7 && ady < reqY - 1e-7 then
                        if dist <= 1e-6 then
                            let ideal = min reqX reqY * 1.05 + 1.
                            eIdeal.Add ideal
                            eWeight.Add (2. / max 1. (ideal * ideal))
                        else
                            let tx = if adx > 1e-9 then reqX / adx else infinity
                            let ty = if ady > 1e-9 then reqY / ady else infinity
                            let tRaw = min tx ty
                            // Gansner & Hu (2010) s_max = 1.5 per-iteration expansion cap for smooth isotropic unfolding
                            let tClamped = min 1.5 (max 1.02 (tRaw * 1.01))
                            let ideal = dist * tClamped
                            eIdeal.Add ideal
                            eWeight.Add (2. / max 1. (ideal * ideal))
                    else
                        let ideal = max 1e-3 dist
                        eIdeal.Add ideal
                        eWeight.Add (1. / max 1. (ideal * ideal))

                // 3. Isotropic proximity stress majorization sweeps
                for _sub = 1 to 4 do
                    Array.fill numX 0 n 0.
                    Array.fill numY 0 n 0.
                    Array.fill denW 0 n 0.
                    for e = 0 to m - 1 do
                        let u = eU[e]
                        let v = eV[e]
                        let w = eWeight[e]
                        let dIdeal = eIdeal[e]
                        let mutable dx = cx[u] - cx[v]
                        let mutable dy = cy[u] - cy[v]
                        let mutable dist = Double.Hypot(dx, dy)
                        if dist <= 1e-7 then
                            let ang = float ((u * 31 + v * 17 + iter) % 360) * (Math.PI / 180.)
                            dx <- cos ang
                            dy <- sin ang
                            dist <- 1.
                        let scale = dIdeal / dist
                        let dirX = dx * scale
                        let dirY = dy * scale
                        numX[u] <- numX[u] + w * (cx[v] + dirX)
                        numY[u] <- numY[u] + w * (cy[v] + dirY)
                        denW[u] <- denW[u] + w
                        numX[v] <- numX[v] + w * (cx[u] - dirX)
                        numY[v] <- numY[v] + w * (cy[u] - dirY)
                        denW[v] <- denW[v] + w

                    for i = 0 to n - 1 do
                        if not pinned[i] && denW[i] > 1e-12 then
                            cx[i] <- numX[i] / denW[i]
                            cy[i] <- numY[i] / denW[i]

                iter <- iter + 1
                stillOverlapping <- hasAnyStrictOverlap cx cy hw hh padding pinned orderBuf overlapPairs

            // 4. Exact zero-overlap sweep guarantee for any residual penetration!
            let maxPolishSweeps = max 64 (n * 4)
            let mutable polishIter = 0
            while stillOverlapping && polishIter < maxPolishSweeps do
                token.ThrowIfCancellationRequested()
                for k = 0 to overlapPairs.Count - 1 do
                    let struct(u, v) = overlapPairs[k]
                    let reqX = hw[u] + hw[v] + padding
                    let reqY = hh[u] + hh[v] + padding
                    let dx = cx[v] - cx[u]
                    let dy = cy[v] - cy[u]
                    let penX = reqX - abs dx
                    let penY = reqY - abs dy
                    if penX > 1e-7 && penY > 1e-7 then
                        let pushAmt p = p + 1e-3
                        if penX <= penY then
                            let dir = if dx > 1e-9 then 1. elif dx < -1e-9 then -1. elif u < v then 1. else -1.
                            let shift = pushAmt penX * dir
                            if pinned[u] && not pinned[v] then cx[v] <- cx[v] + shift
                            elif pinned[v] && not pinned[u] then cx[u] <- cx[u] - shift
                            elif not pinned[u] && not pinned[v] then
                                cx[u] <- cx[u] - shift * 0.5
                                cx[v] <- cx[v] + shift * 0.5
                        else
                            let dir = if dy > 1e-9 then 1. elif dy < -1e-9 then -1. elif u < v then 1. else -1.
                            let shift = pushAmt penY * dir
                            if pinned[u] && not pinned[v] then cy[v] <- cy[v] + shift
                            elif pinned[v] && not pinned[u] then cy[u] <- cy[u] - shift
                            elif not pinned[u] && not pinned[v] then
                                cy[u] <- cy[u] - shift * 0.5
                                cy[v] <- cy[v] + shift * 0.5

                polishIter <- polishIter + 1
                stillOverlapping <- hasAnyStrictOverlap cx cy hw hh padding pinned orderBuf overlapPairs

            // Ultimate deterministic grid fallback if an extreme pileup still had movable overlaps
            if stillOverlapping then
                let cols = max 1 (int (ceil (sqrt (float n))))
                let mutable maxW = 0.
                let mutable maxH = 0.
                let mutable meanX = 0.
                let mutable meanY = 0.
                for i = 0 to n - 1 do
                    if hw[i] * 2. > maxW then maxW <- hw[i] * 2.
                    if hh[i] * 2. > maxH then maxH <- hh[i] * 2.
                    meanX <- meanX + cx[i]
                    meanY <- meanY + cy[i]
                meanX <- meanX / float n
                meanY <- meanY / float n
                let stepX = maxW + padding + 4.
                let stepY = maxH + padding + 4.
                let rows = (n + cols - 1) / cols
                let xStart = meanX - float (cols - 1) * stepX * 0.5
                let yStart = meanY - float (rows - 1) * stepY * 0.5
                let rankOrd = Array.init n (fun i -> i)
                Array.sortInPlaceWith (fun a b ->
                    let ry = compare cy[a] cy[b]
                    if ry <> 0 then ry else compare cx[a] cx[b]) rankOrd
                for r = 0 to n - 1 do
                    let idx = rankOrd[r]
                    if not pinned[idx] then
                        let col = r % cols
                        let row = r / cols
                        cx[idx] <- xStart + float col * stepX
                        cy[idx] <- yStart + float row * stepY

    /// <summary>Removes all overlaps between named <c>Bounds</c> boxes with minimum clearance <c>padding</c> using Delaunay PRISM stress descent and cooperative cancellation.</summary>
    let removeBoxOverlapsWithCancellation
        (token: System.Threading.CancellationToken)
        (padding: float)
        (maxIterations: int)
        (boxes: (string * Bounds) list)
        : Result<(string * Bounds) list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite padding) || padding < 0. then
            Error [ InvalidObservation "PrismOverlap padding must be finite and nonnegative." ]
        elif maxIterations < 1 || maxIterations > 5000 then
            Error [ InvalidObservation "PrismOverlap maxIterations must be in [1, 5000]." ]
        else
            let arr = List.toArray boxes
            let n = arr.Length
            let seen = HashSet<string>(StringComparer.Ordinal)
            let errors = ResizeArray<LayoutError>()
            for i = 0 to n - 1 do
                let id, _ = arr[i]
                if String.IsNullOrWhiteSpace id then
                    errors.Add(InvalidObservation "Box ID must not be empty.")
                elif not (seen.Add id) then
                    errors.Add(DuplicateId id)

            if errors.Count > 0 then
                Error(List.ofSeq errors)
            elif n <= 1 then
                Ok boxes
            else
                let hw = Array.init n (fun i -> Bounds.width (snd arr[i]) * 0.5)
                let hh = Array.init n (fun i -> Bounds.height (snd arr[i]) * 0.5)
                let cx = Array.init n (fun i -> Bounds.x (snd arr[i]) + hw[i])
                let cy = Array.init n (fun i -> Bounds.y (snd arr[i]) + hh[i])
                let pinned = Array.zeroCreate<bool> n

                solvePrismCore token padding maxIterations cx cy hw hh pinned

                let out =
                    [ for i = 0 to n - 1 do
                        let id, orig = arr[i]
                        let w = Bounds.width orig
                        let h = Bounds.height orig
                        yield id, Bounds.create (cx[i] - w * 0.5) (cy[i] - h * 0.5) w h ]
                Ok out

    /// <summary>Removes all overlaps between named <c>Bounds</c> boxes without cancellation.</summary>
    let removeBoxOverlaps padding maxIterations boxes =
        removeBoxOverlapsWithCancellation System.Threading.CancellationToken.None padding maxIterations boxes

    /// <summary>Polishes an existing <c>DiagramLayout2D</c> by removing node bounding-box overlaps via PRISM while respecting pinned nodes and updating ports, groups and routes.</summary>
    let polishLayoutWithCancellation
        (token: System.Threading.CancellationToken)
        (padding: float)
        (maxIterations: int)
        (graph: DiagramGraph)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite padding) || padding < 0. then
            Error [ InvalidObservation "PrismOverlap padding must be finite and nonnegative." ]
        elif maxIterations < 1 || maxIterations > 5000 then
            Error [ InvalidObservation "PrismOverlap maxIterations must be in [1, 5000]." ]
        else
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            if n = 0 then
                Ok layout
            else
                let byId = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
                for pn in layout.Nodes do byId[pn.Id] <- pn
                let missing = eff.Nodes |> Array.tryFind (fun nd -> not (byId.ContainsKey nd.Id))
                match missing with
                | Some nd -> Error [ MissingEndpoint nd.Id ]
                | None ->
                    let orderedPn = Array.init n (fun i -> byId[eff.Nodes[i].Id])
                    let hw = Array.init n (fun i -> Bounds.width orderedPn[i].Bounds * 0.5)
                    let hh = Array.init n (fun i -> Bounds.height orderedPn[i].Bounds * 0.5)
                    let cx = Array.init n (fun i -> orderedPn[i].Center.X)
                    let cy = Array.init n (fun i -> orderedPn[i].Center.Y)
                    let pinned = Array.init n (fun i -> eff.Nodes[i].Pinned.IsSome)

                    solvePrismCore token padding maxIterations cx cy hw hh pinned

                    let updatedNodes =
                        Array.init n (fun i ->
                            let nd = eff.Nodes[i]
                            let oldPn = orderedPn[i]
                            let w = Bounds.width oldPn.Bounds
                            let h = Bounds.height oldPn.Bounds
                            let b = Bounds.create (cx[i] - w * 0.5) (cy[i] - h * 0.5) w h
                            let portSides =
                                oldPn.Ports
                                |> List.fold (fun m p -> Map.add p.Id p.Side m) Map.empty
                            { oldPn with
                                Bounds = b
                                Center = Point.create cx[i] cy[i]
                                Ports = GraphKernels.resolveNodePorts nd b portSides })

                    let revFlags =
                        let map = Dictionary<string, bool>(StringComparer.Ordinal)
                        for ra in layout.Associations do map[ra.Id] <- ra.ReversedInCycle
                        eff.Associations |> Array.map (fun a -> match map.TryGetValue a.Id with true, r -> r | _ -> false)

                    let groups = GraphKernels.computeGroupBounds eff (updatedNodes |> Array.map (fun pn -> pn.Bounds))
                    let routed = GraphKernels.routeDirectAssociations eff updatedNodes (Some revFlags)
                    let nList = Array.toList updatedNodes
                    Ok
                        { Bounds = GraphKernels.computeDiagramBounds layout.Bounds nList groups routed
                          Nodes = nList
                          Groups = groups
                          Associations = routed }

    /// <summary>Polishes an existing <c>DiagramLayout2D</c> via PRISM overlap removal without cancellation.</summary>
    let polishLayout padding maxIterations graph layout =
        polishLayoutWithCancellation System.Threading.CancellationToken.None padding maxIterations graph layout

/// <summary>Gansner, Hu &amp; Kobourov (2010) <c>GMap</c> Voronoi country-region generator with shared-vertex-locked Chaikin border smoothing.</summary>
[<RequireQualifiedAccess>]
module GMap =
    let private pointToRectDist (p: Point) (b: Bounds) : float =
        let x0 = Bounds.x b
        let y0 = Bounds.y b
        let x1 = x0 + Bounds.width b
        let y1 = y0 + Bounds.height b
        let dx = if p.X < x0 then x0 - p.X elif p.X > x1 then p.X - x1 else 0.
        let dy = if p.Y < y0 then y0 - p.Y elif p.Y > y1 then p.Y - y1 else 0.
        Double.Hypot(dx, dy)

    let private shoelaceSignedArea (pts: Point[]) : float =
        let n = pts.Length
        if n < 3 then 0.
        else
            let mutable s = 0.
            let mutable prev = pts[n - 1]
            for i = 0 to n - 1 do
                let cur = pts[i]
                s <- s + (prev.X * cur.Y - cur.X * prev.Y)
                prev <- cur
            0.5 * s

    /// <summary>Canonical commutative 3/4-near + 1/4-far interpolation guaranteeing bit-for-bit identical coordinates on shared opposing borders.</summary>
    let private quarterNear (nearPt: Point) (farPt: Point) : Point =
        // Evaluate (2 * near + (near + far)) * 0.25 where (near + far) is ordered lexicographically so rounding is order-independent!
        let aFirst = nearPt.X < farPt.X || (nearPt.X = farPt.X && nearPt.Y <= farPt.Y)
        let sx = if aFirst then nearPt.X + farPt.X else farPt.X + nearPt.X
        let sy = if aFirst then nearPt.Y + farPt.Y else farPt.Y + nearPt.Y
        Point.create ((2. * nearPt.X + sx) * 0.25) ((2. * nearPt.Y + sy) * 0.25)

    /// <summary>Applies one pass of shared-vertex-locked Chaikin corner cutting, preserving locked triple-point junctions identically across neighbors.</summary>
    let private chaikinLockedStep (ring: Point[]) (locked: bool[]) : Point[] * bool[] =
        let n = ring.Length
        if n < 3 then ring, locked
        else
            let outPts = ResizeArray<Point>(n * 2)
            let outLocked = ResizeArray<bool>(n * 2)
            for i = 0 to n - 1 do
                let j = (i + 1) % n
                let pi, pj = ring[i], ring[j]
                let li, lj = locked[i], locked[j]
                if li && lj then
                    outPts.Add pi
                    outLocked.Add true
                elif li && not lj then
                    outPts.Add pi
                    outLocked.Add true
                    outPts.Add(quarterNear pj pi)
                    outLocked.Add false
                elif not li && lj then
                    outPts.Add(quarterNear pi pj)
                    outLocked.Add false
                else
                    outPts.Add(quarterNear pi pj)
                    outLocked.Add false
                    outPts.Add(quarterNear pj pi)
                    outLocked.Add false
            outPts.ToArray(), outLocked.ToArray()

    /// <summary>Computes gap-free map-like country polygons per cluster over <c>layout</c> from <c>(nodeId, groupId)</c> <c>assignments</c> with cooperative cancellation.</summary>
    let computeWithCancellation
        (token: System.Threading.CancellationToken)
        (margin: float)
        (smoothIterations: int)
        (assignments: (string * string) list)
        (layout: DiagramLayout2D)
        : Result<ClusterRegion list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite margin) || margin <= 0. then
            Error [ InvalidObservation "GMap margin must be finite and positive." ]
        elif smoothIterations < 0 || smoothIterations > 12 then
            Error [ InvalidObservation "GMap smoothIterations must be in [0, 12]." ]
        else
            let nodeById = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
            for pn in layout.Nodes do nodeById[pn.Id] <- pn

            let assignedCluster = Dictionary<string, string>(StringComparer.Ordinal)
            let errors = ResizeArray<LayoutError>()
            for nodeId, groupId in assignments do
                if String.IsNullOrWhiteSpace nodeId || String.IsNullOrWhiteSpace groupId then
                    errors.Add(InvalidObservation "GMap nodeId and groupId must not be empty.")
                elif assignedCluster.ContainsKey nodeId then
                    errors.Add(DuplicateId nodeId)
                elif not (nodeById.ContainsKey nodeId) then
                    errors.Add(MissingEndpoint nodeId)
                else
                    assignedCluster.Add(nodeId, groupId)

            if errors.Count > 0 then
                Error(List.ofSeq errors)
            elif assignedCluster.Count = 0 then
                Ok []
            else
                // Distinct cluster IDs in ordinal order
                let clusterIds =
                    assignedCluster.Values
                    |> Seq.distinct
                    |> Seq.toArray
                Array.sortInPlaceWith (fun a b -> StringComparer.Ordinal.Compare(a, b)) clusterIds
                let clusterIdxMap = Dictionary<string, int>(StringComparer.Ordinal)
                for ci = 0 to clusterIds.Length - 1 do
                    clusterIdxMap.Add(clusterIds[ci], ci)

                let allNodes = List.toArray layout.Nodes
                let sitePts = ResizeArray<Point>(allNodes.Length * 24 + 32)
                // siteOwner: 0 .. clusterIds.Length - 1 for assigned clusters, -1 for sea / unassigned obstacles!
                let siteOwner = ResizeArray<int>(allNodes.Length * 24 + 32)

                let inline addSite (pt: Point) (owner: int) =
                    sitePts.Add pt
                    siteOwner.Add owner

                let mutable minX = infinity
                let mutable minY = infinity
                let mutable maxX = -infinity
                let mutable maxY = -infinity

                // 1. Sample perimeter + center sites for every node in layout.Nodes
                for ni = 0 to allNodes.Length - 1 do
                    let pn = allNodes[ni]
                    let b = pn.Bounds
                    let x0 = Bounds.x b
                    let y0 = Bounds.y b
                    let w = Bounds.width b
                    let h = Bounds.height b
                    let x1 = x0 + w
                    let y1 = y0 + h
                    if x0 < minX then minX <- x0
                    if y0 < minY then minY <- y0
                    if x1 > maxX then maxX <- x1
                    if y1 > maxY then maxY <- y1

                    let owner =
                        match assignedCluster.TryGetValue pn.Id with
                        | true, gid -> clusterIdxMap[gid]
                        | _ -> -1

                    // Slight inward inset so touching neighbor node boxes never collapse onto identical site coordinates
                    let insetX = min 0.5 (w * 0.1)
                    let insetY = min 0.5 (h * 0.1)
                    let sx0, sx1 = x0 + insetX, x1 - insetX
                    let sy0, sy1 = y0 + insetY, y1 - insetY
                    addSite pn.Center owner

                    let step = max 8. (margin * 0.65)
                    let nxSeg = max 2 (min 12 (int (ceil (w / step))))
                    let nySeg = max 2 (min 12 (int (ceil (h / step))))
                    for ix = 0 to nxSeg do
                        let t = float ix / float nxSeg
                        let px = sx0 + (sx1 - sx0) * t
                        addSite (Point.create px sy0) owner
                        addSite (Point.create px sy1) owner
                    for iy = 1 to nySeg - 1 do
                        let t = float iy / float nySeg
                        let py = sy0 + (sy1 - sy0) * t
                        addSite (Point.create sx0 py) owner
                        addSite (Point.create sx1 py) owner

                // 2. Sea / ocean guard halo at 2 * margin around nodes (bisector sits at exact distance `margin`!)
                let seaDist = 2.0 * margin
                let minClearance = 1.82 * margin
                for ni = 0 to allNodes.Length - 1 do
                    SimdMath.check token ni
                    let pn = allNodes[ni]
                    if assignedCluster.ContainsKey pn.Id then
                        let b = pn.Bounds
                        let cx, cy = pn.Center.X, pn.Center.Y
                        let halfW = Bounds.width b * 0.5
                        let halfH = Bounds.height b * 0.5
                        // Exact Minkowski rounded-rectangle offset curve at Euclidean distance `seaDist = 2 * margin` from `pn.Bounds`!
                        let haloCandidates =
                            [| yield Point.create (cx - halfW * 0.5) (cy - halfH - seaDist)
                               yield Point.create cx (cy - halfH - seaDist)
                               yield Point.create (cx + halfW * 0.5) (cy - halfH - seaDist)
                               yield Point.create (cx - halfW * 0.5) (cy + halfH + seaDist)
                               yield Point.create cx (cy + halfH + seaDist)
                               yield Point.create (cx + halfW * 0.5) (cy + halfH + seaDist)
                               yield Point.create (cx - halfW - seaDist) (cy - halfH * 0.5)
                               yield Point.create (cx - halfW - seaDist) cy
                               yield Point.create (cx - halfW - seaDist) (cy + halfH * 0.5)
                               yield Point.create (cx + halfW + seaDist) (cy - halfH * 0.5)
                               yield Point.create (cx + halfW + seaDist) cy
                               yield Point.create (cx + halfW + seaDist) (cy + halfH * 0.5)
                               for sx in [| -1.; 1. |] do
                                   for sy in [| -1.; 1. |] do
                                       for aDeg in [| 22.5; 45.0; 67.5 |] do
                                           let rad = aDeg * (Math.PI / 180.)
                                           yield Point.create (cx + sx * (halfW + seaDist * cos rad)) (cy + sy * (halfH + seaDist * sin rad)) |]
                        for s = 0 to haloCandidates.Length - 1 do
                            let cand = haloCandidates[s]
                            // Keep sea site only if sufficiently clear of ALL other assigned nodes
                            let mutable clear = true
                            let mutable oj = 0
                            while clear && oj < allNodes.Length do
                                if oj <> ni then
                                    let other = allNodes[oj]
                                    if assignedCluster.ContainsKey other.Id then
                                        if pointToRectDist cand other.Bounds < minClearance then
                                            clear <- false
                                oj <- oj + 1
                            if clear then
                                addSite cand -1

                // 3. Outer ocean bounding guard frame
                let framePad = margin * 4.0 + 32.
                let fx0, fy0 = minX - framePad, minY - framePad
                let fx1, fy1 = maxX + framePad, maxY + framePad
                for k = 0 to 4 do
                    let t = float k / 4.0
                    addSite (Point.create (fx0 + (fx1 - fx0) * t) fy0) -1
                    addSite (Point.create (fx0 + (fx1 - fx0) * t) fy1) -1
                for k = 1 to 3 do
                    let t = float k / 4.0
                    addSite (Point.create fx0 (fy0 + (fy1 - fy0) * t)) -1
                    addSite (Point.create fx1 (fy0 + (fy1 - fy0) * t)) -1

                let clipBounds = Bounds.create fx0 fy0 (fx1 - fx0) (fy1 - fy0)
                let mesh = Delaunay.build (List.ofSeq sitePts)
                let vorCells = Voronoi.clip clipBounds mesh

                // 4. Snap shared Voronoi vertices onto a canonical vertex table (1e-5 grid) and cancel internal half-edges per cluster!
                let canonPts = ResizeArray<Point>(vorCells.Length * 4)
                let vertLookup = Dictionary<int64, int>()
                let inline getCanonVertex (pt: Point) : int =
                    let qx = int64 (Math.Round(pt.X * 1e5))
                    let qy = int64 (Math.Round(pt.Y * 1e5))
                    let key = (qx <<< 32) ^^^ (qy &&& 0xFFFFFFFFL)
                    match vertLookup.TryGetValue key with
                    | true, id -> id
                    | _ ->
                        let id = canonPts.Count
                        canonPts.Add pt
                        vertLookup.Add(key, id)
                        id

                let cCount = clusterIds.Length
                // Per-cluster directed half-edge multiset: packed (u <<< 32 ||| uint32 v) -> count
                let clusterEdges = Array.init cCount (fun _ -> Dictionary<uint64, int>())

                for cell in vorCells do
                    if cell.Vertices.Length >= 3 then
                        let mutable owner = -1
                        for inpIdx in cell.InputIndices do
                            if owner < 0 && siteOwner[inpIdx] >= 0 then
                                owner <- siteOwner[inpIdx]
                        if owner >= 0 then
                            let rawV = List.toArray cell.Vertices
                            if shoelaceSignedArea rawV < 0. then
                                let half = rawV.Length / 2
                                for k = 0 to half - 1 do
                                    let opp = rawV.Length - 1 - k
                                    let tmp = rawV[k]
                                    rawV[k] <- rawV[opp]
                                    rawV[opp] <- tmp

                            let ids = Array.init rawV.Length (fun k -> getCanonVertex rawV[k])
                            let edgeDict = clusterEdges[owner]
                            for k = 0 to ids.Length - 1 do
                                let u = ids[k]
                                let v = ids[(k + 1) % ids.Length]
                                if u <> v then
                                    let revKey = (uint64 v <<< 32) ||| uint64 (uint32 u)
                                    match edgeDict.TryGetValue revKey with
                                    | true, cnt when cnt > 1 -> edgeDict[revKey] <- cnt - 1
                                    | true, _ -> edgeDict.Remove revKey |> ignore
                                    | _ ->
                                        let fwdKey = (uint64 u <<< 32) ||| uint64 (uint32 v)
                                        match edgeDict.TryGetValue fwdKey with
                                        | true, cnt -> edgeDict[fwdKey] <- cnt + 1
                                        | _ -> edgeDict.Add(fwdKey, 1)

                // Identify global junction vertices (undirected boundary degree != 2 across the planar country complex)
                let vTotal = canonPts.Count
                let canonArr = canonPts.ToArray()
                let undirNeighbors = Array.init vTotal (fun _ -> HashSet<int>())
                for ci = 0 to cCount - 1 do
                    for kv in clusterEdges[ci] do
                        let u = int (kv.Key >>> 32)
                        let v = int (uint32 kv.Key)
                        undirNeighbors[u].Add v |> ignore
                        undirNeighbors[v].Add u |> ignore

                let isJunction = Array.init vTotal (fun v -> undirNeighbors[v].Count <> 2)

                // 5. Stitch closed country rings per cluster and apply shared-vertex-locked Chaikin smoothing
                let regions = ResizeArray<ClusterRegion>(cCount)
                for ci = 0 to cCount - 1 do
                    token.ThrowIfCancellationRequested()
                    let edgeDict = clusterEdges[ci]
                    let outAdj = Dictionary<int, ResizeArray<int>>()
                    for kv in edgeDict do
                        let u = int (kv.Key >>> 32)
                        let v = int (uint32 kv.Key)
                        let list =
                            match outAdj.TryGetValue u with
                            | true, l -> l
                            | _ ->
                                let l = ResizeArray<int>(2)
                                outAdj.Add(u, l)
                                l
                        for _ = 1 to kv.Value do list.Add v

                    for kv in outAdj do
                        if kv.Value.Count > 1 then
                            kv.Value.Sort(Comparison<int>(fun a b -> compare a b))

                    let startKeys = Seq.toArray outAdj.Keys
                    Array.Sort startKeys

                    let polys = ResizeArray<Point list>()
                    let mutable totalArea = 0.
                    let ringIds = ResizeArray<int>(64)

                    for sk = 0 to startKeys.Length - 1 do
                        let startU = startKeys[sk]
                        let mutable keepTracingFromStart = true
                        while keepTracingFromStart do
                            match outAdj.TryGetValue startU with
                            | false, _ -> keepTracingFromStart <- false
                            | true, startList when startList.Count = 0 -> keepTracingFromStart <- false
                            | true, startList ->
                                ringIds.Clear()
                                let firstNext = startList[startList.Count - 1]
                                startList.RemoveAt(startList.Count - 1)
                                ringIds.Add startU
                                let mutable prevU = startU
                                let mutable curU = firstNext
                                let mutable safety = 0
                                while curU <> startU && safety < vTotal * 4 + 16 do
                                    safety <- safety + 1
                                    ringIds.Add curU
                                    match outAdj.TryGetValue curU with
                                    | false, _ -> curU <- startU
                                    | true, candList when candList.Count = 0 -> curU <- startU
                                    | true, candList when candList.Count = 1 ->
                                        let nxt = candList[0]
                                        candList.RemoveAt 0
                                        prevU <- curU
                                        curU <- nxt
                                    | true, candList ->
                                        // At a pinch vertex, choose the sharpest left-turn continuation to keep simple loops separate
                                        let pPrev = canonArr[prevU]
                                        let pCur = canonArr[curU]
                                        let inAng = atan2 (pPrev.Y - pCur.Y) (pPrev.X - pCur.X)
                                        let mutable bestIdx = 0
                                        let mutable bestDelta = infinity
                                        for k = 0 to candList.Count - 1 do
                                            let w = candList[k]
                                            let pNext = canonArr[w]
                                            let outAng = atan2 (pNext.Y - pCur.Y) (pNext.X - pCur.X)
                                            let mutable d = outAng - inAng
                                            while d <= 0. do d <- d + 2. * Math.PI
                                            if d < bestDelta then
                                                bestDelta <- d
                                                bestIdx <- k
                                        let nxt = candList[bestIdx]
                                        candList.RemoveAt bestIdx
                                        prevU <- curU
                                        curU <- nxt

                                if ringIds.Count >= 3 then
                                    let mutable pts = Array.init ringIds.Count (fun i -> canonArr[ringIds[i]])
                                    let mutable lck = Array.init ringIds.Count (fun i -> isJunction[ringIds[i]])
                                    for _it = 1 to smoothIterations do
                                        let nextPts, nextLck = chaikinLockedStep pts lck
                                        pts <- nextPts
                                        lck <- nextLck

                                    let pList = Array.toList pts
                                    let signedA = shoelaceSignedArea pts
                                    totalArea <- totalArea + signedA
                                    polys.Add pList

                    regions.Add
                        { GroupId = clusterIds[ci]
                          Polygons = List.ofSeq polys
                          Area = max 0. (abs totalArea) }

                Ok(List.ofSeq regions)

    /// <summary>Computes gap-free map-like country polygons per cluster over <c>layout</c> without cancellation.</summary>
    let compute margin smoothIterations assignments layout =
        computeWithCancellation System.Threading.CancellationToken.None margin smoothIterations assignments layout

/// <summary>Configuration for Collins, Penn &amp; Carpendale (2009) Marching-Squares <c>BubbleSets</c> isocontours.</summary>
type BubbleSetConfig =
    { /// <summary>Influence radius <c>R_1</c> around member nodes and virtual MST backbone edges in layout units.</summary>
      RadiusMember: float
      /// <summary>Repulsive influence radius <c>R_0</c> around non-member obstacle nodes in layout units.</summary>
      RadiusNonMember: float
      /// <summary>Multiplier applied to non-member repulsive potential.</summary>
      NegativeWeight: float
      /// <summary>Marching-Squares potential threshold isovalue in <c>(0, 1)</c>.</summary>
      Threshold: float
      /// <summary>Grid cell spacing in layout units used to sample the potential field.</summary>
      GridStep: float }

/// <summary>Default configuration values for <c>BubbleSetConfig</c>.</summary>
[<RequireQualifiedAccess>]
module BubbleSetConfig =
    /// <summary>Default BubbleSets settings: 36-unit member radius, 24-unit non-member radius, 0.8 negative weight, 0.25 threshold and 6-unit grid step.</summary>
    let defaultValue =
        { RadiusMember = 36.
          RadiusNonMember = 24.
          NegativeWeight = 0.8
          Threshold = 0.25
          GridStep = 6. }

/// <summary>Collins, Penn &amp; Carpendale (2009) <c>BubbleSets</c> overlapping set-contour generator via detoured Prim MST and Marching Squares.</summary>
[<RequireQualifiedAccess>]
module BubbleSets =
    let private segHitsInflatedRect (ax: float) (ay: float) (bx: float) (by: float) (b: Bounds) (pad: float) : bool =
        let rx0 = Bounds.x b - pad
        let ry0 = Bounds.y b - pad
        let rx1 = Bounds.x b + Bounds.width b + pad
        let ry1 = Bounds.y b + Bounds.height b + pad
        if rx0 >= rx1 || ry0 >= ry1 then false
        else
            let dx = bx - ax
            let dy = by - ay
            let mutable t0 = 0.
            let mutable t1 = 1.
            let inline clip p q =
                if abs p <= 1e-12 then q >= 0.
                else
                    let r = q / p
                    if p < 0. then
                        if r > t1 then false
                        else (if r > t0 then t0 <- r); true
                    else
                        if r < t0 then false
                        else (if r < t1 then t1 <- r); true
            if clip (-dx) (ax - rx0) && clip dx (rx1 - ax) && clip (-dy) (ay - ry0) && clip dy (ry1 - ay) then
                t0 < t1 - 1e-6
            else
                false

    let private pointToSegDistSq (px: float) (py: float) (ax: float) (ay: float) (bx: float) (by: float) : float =
        let vx = bx - ax
        let vy = by - ay
        let lenSq = vx * vx + vy * vy
        if lenSq <= 1e-12 then
            (px - ax) * (px - ax) + (py - ay) * (py - ay)
        else
            let t = max 0. (min 1. (((px - ax) * vx + (py - ay) * vy) / lenSq))
            let qx = ax + t * vx
            let qy = ay + t * vy
            (px - qx) * (px - qx) + (py - qy) * (py - qy)

    /// <summary>Detours virtual MST segment <c>pA -&gt; pB</c> around any intersecting non-member boxes (Collins et al. 2009 Algorithm 1).</summary>
    let private detourSegment
        (pA: Point)
        (pB: Point)
        (nonMembers: PositionedNode[])
        (clearance: float)
        : struct(Point * Point)[] =
        if nonMembers.Length = 0 then
            [| struct(pA, pB) |]
        else
            let segs = ResizeArray<struct(Point * Point)>(8)
            segs.Add(struct(pA, pB))
            let mutable splits = 0
            let mutable idx = 0
            while idx < segs.Count && splits < 10 do
                let struct(a, b) = segs[idx]
                // Find first blocking non-member box that does not already contain endpoint a or b
                let mutable hitIdx = -1
                let mutable bestHitDist = infinity
                for oi = 0 to nonMembers.Length - 1 do
                    let ob = nonMembers[oi].Bounds
                    let aInside = Bounds.contains a ob
                    let bInside = Bounds.contains b ob
                    if not aInside && not bInside && segHitsInflatedRect a.X a.Y b.X b.Y ob clearance then
                        let d = Double.Hypot(nonMembers[oi].Center.X - a.X, nonMembers[oi].Center.Y - a.Y)
                        if d < bestHitDist then
                            bestHitDist <- d
                            hitIdx <- oi

                if hitIdx < 0 then
                    idx <- idx + 1
                else
                    let ob = nonMembers[hitIdx].Bounds
                    let x0 = Bounds.x ob - clearance - 2.
                    let y0 = Bounds.y ob - clearance - 2.
                    let x1 = Bounds.x ob + Bounds.width ob + clearance + 2.
                    let y1 = Bounds.y ob + Bounds.height ob + clearance + 2.
                    let corners =
                        [| Point.create x0 y0
                           Point.create x1 y0
                           Point.create x1 y1
                           Point.create x0 y1 |]
                    let mutable bestCorner = corners[0]
                    let mutable bestLen = infinity
                    for c = 0 to 3 do
                        let cp = corners[c]
                        let clearSelf =
                            not (segHitsInflatedRect a.X a.Y cp.X cp.Y ob (clearance * 0.5))
                            && not (segHitsInflatedRect cp.X cp.Y b.X b.Y ob (clearance * 0.5))
                        let len = Double.Hypot(cp.X - a.X, cp.Y - a.Y) + Double.Hypot(b.X - cp.X, b.Y - cp.Y)
                        let score = if clearSelf then len else len + 10000.
                        if score < bestLen then
                            bestLen <- score
                            bestCorner <- cp

                    segs[idx] <- struct(a, bestCorner)
                    segs.Insert(idx + 1, struct(bestCorner, b))
                    splits <- splits + 1

            segs.ToArray()

    let private stripClosingDuplicate (ring: Point list) : Point list =
        match ring with
        | [] | [ _ ] -> ring
        | _ ->
            let arr = List.toArray ring
            if arr.Length > 1 && arr[0] = arr[arr.Length - 1] then
                let mutable acc = []
                for i = arr.Length - 2 downto 0 do
                    acc <- arr[i] :: acc
                acc
            else
                ring

    /// <summary>Computes smooth Marching-Squares <c>BubbleSets</c> isocontours supporting overlapping multi-set membership with cooperative cancellation.</summary>
    let computeWithCancellation
        (token: System.Threading.CancellationToken)
        (config: BubbleSetConfig)
        (sets: (string * string list) list)
        (layout: DiagramLayout2D)
        : Result<ClusterRegion list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.RadiusMember && Double.IsFinite config.RadiusNonMember
                && Double.IsFinite config.NegativeWeight && Double.IsFinite config.Threshold
                && Double.IsFinite config.GridStep)
           || config.RadiusMember <= 0. || config.RadiusNonMember < 0.
           || config.NegativeWeight < 0. || config.Threshold <= 0. || config.Threshold >= 1.
           || config.GridStep <= 0. then
            Error [ InvalidObservation "BubbleSets requires positive RadiusMember/GridStep, nonnegative RadiusNonMember/NegativeWeight and Threshold in (0, 1)." ]
        else
            let nodeById = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
            for pn in layout.Nodes do nodeById[pn.Id] <- pn

            let seenSetIds = HashSet<string>(StringComparer.Ordinal)
            let errors = ResizeArray<LayoutError>()
            for setId, memberIds in sets do
                if String.IsNullOrWhiteSpace setId then
                    errors.Add(InvalidObservation "BubbleSet ID must not be empty.")
                elif not (seenSetIds.Add setId) then
                    errors.Add(DuplicateId setId)
                for mid in memberIds do
                    if not (nodeById.ContainsKey mid) then
                        errors.Add(MissingEndpoint mid)

            if errors.Count > 0 then
                Error(List.ofSeq errors)
            elif sets.IsEmpty then
                Ok []
            else
                let allNodes = List.toArray layout.Nodes
                let results = ResizeArray<ClusterRegion>(sets.Length)
                let mutable abortErr : LayoutError list option = None

                let r1 = config.RadiusMember
                let r0 = config.RadiusNonMember
                let invR1 = 1. / r1
                let invR0 = if r0 > 1e-9 then 1. / r0 else 0.

                let setsArr = List.toArray sets
                let mutable sIdx = 0
                while abortErr.IsNone && sIdx < setsArr.Length do
                    token.ThrowIfCancellationRequested()
                    let setId, rawMemberIds = setsArr[sIdx]
                    // Deduplicate member IDs within this set while preserving order
                    let memSet = HashSet<string>(StringComparer.Ordinal)
                    let membersBuf = ResizeArray<PositionedNode>()
                    for mid in rawMemberIds do
                        if memSet.Add mid then
                            membersBuf.Add nodeById[mid]

                    if membersBuf.Count = 0 then
                        results.Add { GroupId = setId; Polygons = []; Area = 0. }
                    else
                        let members = membersBuf.ToArray()
                        let nonMembers =
                            allNodes
                            |> Array.filter (fun pn -> not (memSet.Contains pn.Id))

                        // 1. Prim Minimum Spanning Tree over member centers (penalizing obstacle-crossing chords)
                        let mCount = members.Length
                        let clearance = min (r0 * 0.45) 10.
                        let virtualSegs = ResizeArray<struct(Point * Point)>(max 0 (mCount - 1) * 2)

                        if mCount >= 2 then
                            let inTree = Array.zeroCreate<bool> mCount
                            let bestCost = Array.create mCount infinity
                            let bestFrom = Array.create mCount -1
                            inTree[0] <- true

                            let pairCost (i: int) (j: int) =
                                let a, b = members[i].Center, members[j].Center
                                let baseD = Double.Hypot(b.X - a.X, b.Y - a.Y)
                                let mutable crossings = 0
                                for oi = 0 to nonMembers.Length - 1 do
                                    if segHitsInflatedRect a.X a.Y b.X b.Y nonMembers[oi].Bounds clearance then
                                        crossings <- crossings + 1
                                baseD + float crossings * (r1 * 2.5 + 40.)

                            for j = 1 to mCount - 1 do
                                bestCost[j] <- pairCost 0 j
                                bestFrom[j] <- 0

                            for _step = 1 to mCount - 1 do
                                let mutable pick = -1
                                let mutable minC = infinity
                                for j = 1 to mCount - 1 do
                                    if not inTree[j] && bestCost[j] < minC then
                                        minC <- bestCost[j]
                                        pick <- j
                                if pick >= 0 then
                                    inTree[pick] <- true
                                    let u = bestFrom[pick]
                                    let detoured = detourSegment members[u].Center members[pick].Center nonMembers clearance
                                    for k = 0 to detoured.Length - 1 do
                                        virtualSegs.Add detoured[k]
                                    for j = 1 to mCount - 1 do
                                        if not inTree[j] then
                                            let c = pairCost pick j
                                            if c < bestCost[j] then
                                                bestCost[j] <- c
                                                bestFrom[j] <- pick

                        // 2. Determine padded potential-field grid bounds
                        let mutable minX = infinity
                        let mutable minY = infinity
                        let mutable maxX = -infinity
                        let mutable maxY = -infinity
                        for i = 0 to mCount - 1 do
                            let b = members[i].Bounds
                            let bx0, by0 = Bounds.x b, Bounds.y b
                            let bx1, by1 = bx0 + Bounds.width b, by0 + Bounds.height b
                            if bx0 < minX then minX <- bx0
                            if by0 < minY then minY <- by0
                            if bx1 > maxX then maxX <- bx1
                            if by1 > maxY then maxY <- by1
                        for k = 0 to virtualSegs.Count - 1 do
                            let struct(a, b) = virtualSegs[k]
                            if min a.X b.X < minX then minX <- min a.X b.X
                            if min a.Y b.Y < minY then minY <- min a.Y b.Y
                            if max a.X b.X > maxX then maxX <- max a.X b.X
                            if max a.Y b.Y > maxY then maxY <- max a.Y b.Y

                        let pad = r1 + config.GridStep * 3. + 8.
                        let gx0 = minX - pad
                        let gy0 = minY - pad
                        let gw = max 16. (maxX - minX + 2. * pad)
                        let gh = max 16. (maxY - minY + 2. * pad)

                        // Keep cell count bounded cleanly within 256 x 256 = 65,536 cells for instant interactive performance
                        let effStep = max config.GridStep (max (gw / 240.) (gh / 240.))
                        let cols = max 8 (int (ceil (gw / effStep)))
                        let rows = max 8 (int (ceil (gh / effStep)))
                        let colsF = float cols
                        let rowsF = float rows
                        let cellW = gw / colsF
                        let cellH = gh / rowsF

                        let field = Array.zeroCreate<float> (cols * rows)
                        let insideMember = Array.zeroCreate<bool> (cols * rows)

                        let inline colRange wxMin wxMax =
                            let c0 = max 0 (int (floor ((wxMin - gx0) / cellW - 0.5)))
                            let c1 = min (cols - 1) (int (ceil ((wxMax - gx0) / cellW + 0.5)))
                            struct(c0, c1)

                        let inline rowRange wyMin wyMax =
                            let r0i = max 0 (int (floor ((wyMin - gy0) / cellH - 0.5)))
                            let r1i = min (rows - 1) (int (ceil ((wyMax - gy0) / cellH + 0.5)))
                            struct(r0i, r1i)

                        // Splat positive potential from member node rectangles
                        for i = 0 to mCount - 1 do
                            let b = members[i].Bounds
                            let bx0 = Bounds.x b
                            let by0 = Bounds.y b
                            let bx1 = bx0 + Bounds.width b
                            let by1 = by0 + Bounds.height b
                            let struct(c0, c1) = colRange (bx0 - r1) (bx1 + r1)
                            let struct(ry0, ry1) = rowRange (by0 - r1) (by1 + r1)
                            for ry = ry0 to ry1 do
                                let wy = gy0 + gh * ((float ry + 0.5) / rowsF)
                                let dy = if wy < by0 then by0 - wy elif wy > by1 then wy - by1 else 0.
                                let rowOff = ry * cols
                                for cx = c0 to c1 do
                                    let wx = gx0 + gw * ((float cx + 0.5) / colsF)
                                    let dx = if wx < bx0 then bx0 - wx elif wx > bx1 then wx - bx1 else 0.
                                    if dx = 0. && dy = 0. then
                                        insideMember[rowOff + cx] <- true
                                        field[rowOff + cx] <- field[rowOff + cx] + 1.0
                                    else
                                        let d = Double.Hypot(dx, dy)
                                        if d < r1 then
                                            let u = 1. - d * invR1
                                            field[rowOff + cx] <- field[rowOff + cx] + u * u

                        // Splat positive potential from virtual detoured MST segments
                        for k = 0 to virtualSegs.Count - 1 do
                            let struct(a, b) = virtualSegs[k]
                            let struct(c0, c1) = colRange (min a.X b.X - r1) (max a.X b.X + r1)
                            let struct(ry0, ry1) = rowRange (min a.Y b.Y - r1) (max a.Y b.Y + r1)
                            let r1Sq = r1 * r1
                            for ry = ry0 to ry1 do
                                let wy = gy0 + gh * ((float ry + 0.5) / rowsF)
                                let rowOff = ry * cols
                                for cx = c0 to c1 do
                                    let wx = gx0 + gw * ((float cx + 0.5) / colsF)
                                    let dSq = pointToSegDistSq wx wy a.X a.Y b.X b.Y
                                    if dSq < r1Sq then
                                        let u = 1. - sqrt dSq * invR1
                                        field[rowOff + cx] <- field[rowOff + cx] + 0.85 * u * u

                        // Subtract repulsive potential from nearby non-member obstacle rectangles (outside member interiors)
                        if r0 > 1e-6 && config.NegativeWeight > 0. then
                            for oi = 0 to nonMembers.Length - 1 do
                                let b = nonMembers[oi].Bounds
                                let bx0 = Bounds.x b
                                let by0 = Bounds.y b
                                let bx1 = bx0 + Bounds.width b
                                let by1 = by0 + Bounds.height b
                                if bx1 + r0 >= gx0 && bx0 - r0 <= gx0 + gw && by1 + r0 >= gy0 && by0 - r0 <= gy0 + gh then
                                    let struct(c0, c1) = colRange (bx0 - r0) (bx1 + r0)
                                    let struct(ry0, ry1) = rowRange (by0 - r0) (by1 + r0)
                                    for ry = ry0 to ry1 do
                                        let wy = gy0 + gh * ((float ry + 0.5) / rowsF)
                                        let dy = if wy < by0 then by0 - wy elif wy > by1 then wy - by1 else 0.
                                        let rowOff = ry * cols
                                        for cx = c0 to c1 do
                                            let cellIdx = rowOff + cx
                                            if not insideMember[cellIdx] then
                                                let wx = gx0 + gw * ((float cx + 0.5) / colsF)
                                                let dx = if wx < bx0 then bx0 - wx elif wx > bx1 then wx - bx1 else 0.
                                                let d = Double.Hypot(dx, dy)
                                                if d < r0 then
                                                    let u = 1. - d * invR0
                                                    field[cellIdx] <- field[cellIdx] - config.NegativeWeight * u * u

                        // 3. Extract Marching-Squares isocontours via Ranvier.D5.Contour.computeWithCancellation
                        let gridBounds = Bounds.create gx0 gy0 gw gh
                        let optVals = [ for i in 0 .. field.Length - 1 -> Some field[i] ]
                        match DensityGrid.create gridBounds cols rows optVals with
                        | Error errs -> abortErr <- Some errs
                        | Ok dGrid ->
                            let cCfg = ContourConfig.create [ config.Threshold ]
                            match Contour.computeWithCancellation token cCfg dGrid with
                            | Error errs -> abortErr <- Some errs
                            | Ok contours ->
                                let polys = ResizeArray<Point list>(contours.Length)
                                let mutable areaSum = 0.
                                for cp in contours do
                                    let outerClean = stripClosingDuplicate cp.Outer
                                    if outerClean.Length >= 3 then
                                        polys.Add outerClean
                                        let outerA = abs (Polygon.area outerClean)
                                        let mutable holeA = 0.
                                        for h in cp.Holes do
                                            let hClean = stripClosingDuplicate h
                                            if hClean.Length >= 3 then
                                                holeA <- holeA + abs (Polygon.area hClean)
                                                polys.Add hClean
                                        areaSum <- areaSum + max 0. (outerA - holeA)
                                results.Add
                                    { GroupId = setId
                                      Polygons = List.ofSeq polys
                                      Area = areaSum }

                    sIdx <- sIdx + 1

                match abortErr with
                | Some errs -> Error errs
                | None -> Ok(List.ofSeq results)

    /// <summary>Computes smooth Marching-Squares <c>BubbleSets</c> isocontours without cancellation.</summary>
    let compute config sets layout =
        computeWithCancellation System.Threading.CancellationToken.None config sets layout

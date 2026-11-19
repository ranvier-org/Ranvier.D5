namespace Ranvier.D5

open System
open System.Collections.Generic

/// <summary>A weighted 2D generator site for an Aurenhammer power (Laguerre) diagram.</summary>
type PowerSite<'Id> =
    { /// <summary>Caller-supplied site identifier.</summary>
      Id: 'Id
      /// <summary>Generator position in 2D scene coordinates.</summary>
      Position: Point
      /// <summary>Additive power weight; larger weights expand the cell.</summary>
      Weight: float }

/// <summary>A clipped 2D convex power-diagram cell with exact polygon area and centroid.</summary>
type PowerCell<'Id> =
    { /// <summary>Caller-supplied site identifier.</summary>
      Id: 'Id
      /// <summary>Generator position in 2D scene coordinates.</summary>
      Site: Point
      /// <summary>Additive power weight used during clipping.</summary>
      Weight: float
      /// <summary>Counterclockwise convex polygon vertices without a repeated closing point; empty when submerged.</summary>
      Vertices: Point list
      /// <summary>Nonnegative polygon area (0 when empty).</summary>
      Area: float
      /// <summary>Area-weighted polygon centroid, or <c>Site</c> when the cell is empty.</summary>
      Centroid: Point }

module internal ConvexClip2D =
    let inline isFinitePoint (p: Point) =
        Double.IsFinite p.X && Double.IsFinite p.Y

    /// Computes nonnegative area and centroid of a simple polygon using origin-shifted shoelace moments.
    let areaAndCentroid (fallback: Point) (verts: ResizeArray<Point>) : struct(float * Point) =
        let n = verts.Count
        if n < 3 then
            struct(0., fallback)
        else
            let p0 = verts[0]
            let mutable twiceArea = 0.
            let mutable mx = 0.
            let mutable my = 0.
            for i = 1 to n - 2 do
                let ax = verts[i].X - p0.X
                let ay = verts[i].Y - p0.Y
                let bx = verts[i + 1].X - p0.X
                let by = verts[i + 1].Y - p0.Y
                let cross = ax * by - bx * ay
                twiceArea <- twiceArea + cross
                mx <- mx + (ax + bx) * cross
                my <- my + (ay + by) * cross
            let area = 0.5 * abs twiceArea
            if not (Double.IsFinite area) || area <= 1e-30 then
                struct(0., fallback)
            else
                let inv3A = 1. / (3. * twiceArea)
                let cx = p0.X + mx * inv3A
                let cy = p0.Y + my * inv3A
                if Double.IsFinite cx && Double.IsFinite cy then
                    struct(area, Point.create cx cy)
                else
                    struct(area, fallback)

    /// Validates and normalizes a convex polygon boundary into positive-signed-area vertex order.
    let validateConvexBoundary (boundary: Point list) : Result<Point array, LayoutError list> =
        let raw = ResizeArray<Point>()
        let mutable allFinite = true
        for p in boundary do
            if not (isFinitePoint p) then
                allFinite <- false
            elif raw.Count = 0 || raw[raw.Count - 1] <> p then
                raw.Add p
        if not allFinite then
            Error [ InvalidObservation "Polygon boundary coordinates must be finite." ]
        else
            while raw.Count > 1 && raw[raw.Count - 1] = raw[0] do
                raw.RemoveAt(raw.Count - 1)
            if raw.Count < 3 then
                Error [ InvalidTopology "Clipping boundary requires at least three distinct vertices." ]
            else
                let p0 = raw[0]
                let mutable twiceSigned = 0.
                for i = 1 to raw.Count - 2 do
                    let ax = raw[i].X - p0.X
                    let ay = raw[i].Y - p0.Y
                    let bx = raw[i + 1].X - p0.X
                    let by = raw[i + 1].Y - p0.Y
                    twiceSigned <- twiceSigned + (ax * by - bx * ay)
                if not (Double.IsFinite twiceSigned) || abs twiceSigned <= 1e-24 then
                    Error [ InvalidTopology "Clipping boundary must have positive area." ]
                else
                    let count = raw.Count
                    let poly =
                        if twiceSigned > 0. then
                            raw.ToArray()
                        else
                            Array.init count (fun i -> raw[count - 1 - i])
                    // Verify convexity: all non-degenerate turns must be >= 0 in normalized orientation.
                    let mutable convex = true
                    let mutable i = 0
                    while convex && i < count do
                        let a = poly[(i + count - 1) % count]
                        let b = poly[i]
                        let c = poly[(i + 1) % count]
                        let ux, uy = b.X - a.X, b.Y - a.Y
                        let vx, vy = c.X - b.X, c.Y - b.Y
                        let cross = ux * vy - uy * vx
                        let scale = (abs ux + abs uy) * (abs vx + abs vy)
                        if cross < -1e-10 * max 1e-20 scale then
                            convex <- false
                        i <- i + 1
                    if not convex then
                        Error [ InvalidTopology "Clipping boundary must be a convex polygon." ]
                    else
                        Ok poly

    let inline powerSide (si: Point) (wi: float) (sj: Point) (wj: float) (p: Point) : float =
        let dx = sj.X - si.X
        let dy = sj.Y - si.Y
        dx * ((p.X - si.X) + (p.X - sj.X)) + dy * ((p.Y - si.Y) + (p.Y - sj.Y)) - (wi - wj)

    let inline intersectSegment (p: Point) (q: Point) (sp: float) (sq: float) : Point =
        let denom = sp - sq
        let t = if denom = 0. || not (Double.IsFinite denom) then 0.5 else Math.Clamp(sp / denom, 0., 1.)
        let inline clampSeg (a: float) (b: float) (v: float) =
            let lo = min a b
            let hi = max a b
            max lo (min hi v)
        let ix = clampSeg p.X q.X (Check.lerp p.X q.X t)
        let iy = clampSeg p.Y q.Y (Check.lerp p.Y q.Y t)
        Point.create ix iy

    let inline addUniqueTol (tolSq: float) (buf: ResizeArray<Point>) (pt: Point) =
        let n = buf.Count
        if n = 0 || Point.distanceSquared buf[n - 1] pt > tolSq then
            buf.Add pt

    let inline addTaggedTol (tolSq: float) (buf: ResizeArray<Point>) (tags: ResizeArray<int>) (pt: Point) (tag: int) =
        let n = buf.Count
        if n = 0 || Point.distanceSquared buf[n - 1] pt > tolSq then
            buf.Add pt
            tags.Add tag

    /// Clips cell i against all other power sites using double-buffered ResizeArray arenas, optionally recording bisector sensitivity dA_i / dw_i.
    let clipSingleCell
        (boundary: Point array)
        (tolSq: float)
        (positions: Point array)
        (weights: float array)
        (orderBuf: int array)
        (keyBuf: float array)
        (curPoly: ResizeArray<Point>)
        (nextPoly: ResizeArray<Point>)
        (curTags: ResizeArray<int>)
        (nextTags: ResizeArray<int>)
        (i: int)
        : struct(float * Point * float) =
        let nSites = positions.Length
        curPoly.Clear()
        curTags.Clear()
        for k = 0 to boundary.Length - 1 do
            curPoly.Add boundary[k]
            curTags.Add -1
        let si = positions[i]
        let wi = weights[i]
        let mutable m = 0
        let mutable submerged = false
        for j = 0 to nSites - 1 do
            if j <> i && not submerged then
                let sj = positions[j]
                let wj = weights[j]
                let d2 = Point.distanceSquared si sj
                if d2 = 0. then
                    if wj > wi || (wj = wi && j < i) then
                        submerged <- true
                else
                    orderBuf[m] <- j
                    keyBuf[m] <- (d2 - (wj - wi)) / (2. * sqrt d2)
                    m <- m + 1
        if submerged then
            curPoly.Clear()
            curTags.Clear()
            struct(0., si, 0.)
        else
            // Sort neighbor planes by signed distance from si so tightest bisectors clip first.
            if m > 1 then
                let idxSlice = Array.init m (fun k -> k)
                Array.sortInPlaceWith (fun a b -> compare keyBuf[a] keyBuf[b]) idxSlice
                let tmpOrd = Array.init m (fun k -> orderBuf[idxSlice[k]])
                let tmpKey = Array.init m (fun k -> keyBuf[idxSlice[k]])
                for k = 0 to m - 1 do
                    orderBuf[k] <- tmpOrd[k]
                    keyBuf[k] <- tmpKey[k]
            let mutable cur = curPoly
            let mutable nxt = nextPoly
            let mutable cTags = curTags
            let mutable nTags = nextTags
            let mutable maxRadius = -1.
            let mutable idx = 0
            while idx < m && cur.Count >= 3 do
                let j = orderBuf[idx]
                let bisectorDist = keyBuf[idx]
                if maxRadius < 0. then
                    let mutable mrSq = 0.
                    for v = 0 to cur.Count - 1 do
                        let dSq = Point.distanceSquared si cur[v]
                        if dSq > mrSq then mrSq <- dSq
                    maxRadius <- sqrt mrSq
                if bisectorDist > 0. && bisectorDist > maxRadius * (1. + 1e-12) then
                    // Every remaining neighbor has even larger bisector distance; none can intersect cur.
                    idx <- m
                else
                    let sj = positions[j]
                    let wj = weights[j]
                    let pCount = cur.Count
                    // Quick scan to check if all vertices are inside or all strictly outside.
                    let mutable allInside = true
                    let mutable anyInside = false
                    for k = 0 to pCount - 1 do
                        let sVal = powerSide si wi sj wj cur[k]
                        if sVal <= 0. then anyInside <- true else allInside <- false
                    if not anyInside then
                        cur.Clear()
                        cTags.Clear()
                    elif not allInside then
                        nxt.Clear()
                        nTags.Clear()
                        let mutable prev = cur[pCount - 1]
                        let mutable prevVal = powerSide si wi sj wj prev
                        for k = 0 to pCount - 1 do
                            let curr = cur[k]
                            let edgeTag = cTags[k]
                            let currVal = powerSide si wi sj wj curr
                            let prevIn = prevVal <= 0.
                            let currIn = currVal <= 0.
                            if prevIn && currIn then
                                addTaggedTol tolSq nxt nTags curr edgeTag
                            elif prevIn && not currIn then
                                let inter = intersectSegment prev curr prevVal currVal
                                addTaggedTol tolSq nxt nTags inter edgeTag
                            elif not prevIn && currIn then
                                let inter = intersectSegment prev curr prevVal currVal
                                addTaggedTol tolSq nxt nTags inter j
                                addTaggedTol tolSq nxt nTags curr edgeTag
                            prev <- curr
                            prevVal <- currVal
                        while nxt.Count > 1 && Point.distanceSquared nxt[nxt.Count - 1] nxt[0] <= tolSq do
                            nxt.RemoveAt(nxt.Count - 1)
                            nTags.RemoveAt(nTags.Count - 1)
                        let swapP = cur
                        cur <- nxt
                        nxt <- swapP
                        let swapT = cTags
                        cTags <- nTags
                        nTags <- swapT
                        maxRadius <- -1.
                    idx <- idx + 1
            if not (Object.ReferenceEquals(cur, curPoly)) then
                curPoly.Clear()
                curTags.Clear()
                for k = 0 to cur.Count - 1 do
                    curPoly.Add cur[k]
                    curTags.Add cTags[k]
            if curPoly.Count < 3 then
                curPoly.Clear()
                curTags.Clear()
                struct(0., si, 0.)
            else
                let struct(area, centroid) = areaAndCentroid si curPoly
                if area <= 0. then
                    curPoly.Clear()
                    curTags.Clear()
                    struct(0., si, 0.)
                else
                    let mutable hii = 0.
                    let cnt = curPoly.Count
                    for k = 0 to cnt - 1 do
                        let tag = curTags[k]
                        if tag >= 0 then
                            let pa = curPoly[(k + cnt - 1) % cnt]
                            let pb = curPoly[k]
                            let edgeLen = Double.Hypot(pb.X - pa.X, pb.Y - pa.Y)
                            let siteDist = Double.Hypot(positions[tag].X - si.X, positions[tag].Y - si.Y)
                            if siteDist > 1e-30 then
                                hii <- hii + edgeLen / (2. * siteDist)
                    struct(area, centroid, hii)

/// <summary>2D Aurenhammer power (Laguerre) diagrams clipped to convex polygons or rectangular bounds.</summary>
[<RequireQualifiedAccess>]
module PowerDiagram =
    let private clipValidated (poly: Point array) (sites: PowerSite<'Id> list) : Result<PowerCell<'Id> list, LayoutError list> =
        let siteArr = List.toArray sites
        let mutable validSites = true
        for i = 0 to siteArr.Length - 1 do
            let s = siteArr[i]
            if not (ConvexClip2D.isFinitePoint s.Position && Double.IsFinite s.Weight) then
                validSites <- false
        if not validSites then
            Error [ InvalidObservation "Power diagram sites require finite positions and weights." ]
        elif siteArr.Length = 0 then
            Ok []
        else
            let mutable minX = poly[0].X
            let mutable maxX = poly[0].X
            let mutable minY = poly[0].Y
            let mutable maxY = poly[0].Y
            for i = 1 to poly.Length - 1 do
                let p = poly[i]
                if p.X < minX then minX <- p.X
                if p.X > maxX then maxX <- p.X
                if p.Y < minY then minY <- p.Y
                if p.Y > maxY then maxY <- p.Y
            let diagSq = (maxX - minX) * (maxX - minX) + (maxY - minY) * (maxY - minY)
            let tolSq = max 1e-30 (diagSq * 1e-26)
            let n = siteArr.Length
            let positions = Array.init n (fun i -> siteArr[i].Position)
            let weights = Array.init n (fun i -> siteArr[i].Weight)
            let orderBuf = Array.zeroCreate<int> n
            let keyBuf = Array.zeroCreate<float> n
            let curPoly = ResizeArray<Point>(32)
            let nextPoly = ResizeArray<Point>(32)
            let curTags = ResizeArray<int>(32)
            let nextTags = ResizeArray<int>(32)
            let outCells = Array.zeroCreate<PowerCell<'Id>> n
            for i = 0 to n - 1 do
                let struct(area, centroid, _) =
                    ConvexClip2D.clipSingleCell poly tolSq positions weights orderBuf keyBuf curPoly nextPoly curTags nextTags i
                let verts = Data.resizeToList curPoly
                outCells[i] <-
                    { Id = siteArr[i].Id
                      Site = positions[i]
                      Weight = weights[i]
                      Vertices = verts
                      Area = area
                      Centroid = centroid }
            Ok(Array.toList outCells)

    /// <summary>Clips weighted 2D power cells against a finite convex polygon boundary.</summary>
    let clipPolygon (boundary: Point list) (sites: PowerSite<'Id> list) : Result<PowerCell<'Id> list, LayoutError list> =
        match ConvexClip2D.validateConvexBoundary boundary with
        | Error errs -> Error errs
        | Ok poly -> clipValidated poly sites

    /// <summary>Clips weighted 2D power cells against finite positive rectangular bounds.</summary>
    let clipBounds (bounds: Bounds) (sites: PowerSite<'Id> list) : Result<PowerCell<'Id> list, LayoutError list> =
        let x, y = Bounds.x bounds, Bounds.y bounds
        let w, h = Bounds.width bounds, Bounds.height bounds
        if not (Double.IsFinite x && Double.IsFinite y && Double.IsFinite w && Double.IsFinite h) || w <= 0. || h <= 0. then
            Error [ InvalidObservation "Power diagram bounds must have positive width and height." ]
        else
            let poly =
                [| Point.create x y
                   Point.create (x + w) y
                   Point.create (x + w) (y + h)
                   Point.create x (y + h) |]
            clipValidated poly sites

/// <summary>Configuration for Nocaj &amp; Brandes (2012) resolution-independent Voronoi treemaps.</summary>
type VoronoiTreemapConfig =
    { /// <summary>Enclosing layout bounds.</summary>
      Bounds: Bounds
      /// <summary>Maximum dual weight/centroid adaptation iterations per internal node.</summary>
      MaxIterations: int
      /// <summary>Relative target area tolerance (for example <c>1e-3</c>).</summary>
      AreaTolerance: float
      /// <summary>Deterministic spiral initialization seed.</summary>
      Seed: uint32
      /// <summary><c>None</c> uses rectangular <c>Bounds</c>; <c>Some k</c> (<c>k &gt;= 3</c>) inscribes a regular <c>k</c>-gon.</summary>
      BoundarySides: int option }

/// <summary>Default constructors for <c>VoronoiTreemapConfig</c>.</summary>
[<RequireQualifiedAccess>]
module VoronoiTreemapConfig =
    /// <summary>Creates a rectangular Voronoi treemap configuration with 80 max iterations, 1e-3 area tolerance, and seed 1.</summary>
    let create (bounds: Bounds) : VoronoiTreemapConfig =
        { Bounds = bounds
          MaxIterations = 80
          AreaTolerance = 1e-3
          Seed = 1u
          BoundarySides = None }

/// <summary>A positioned node cell in a hierarchical Voronoi treemap.</summary>
type VoronoiTreemapCell<'Id> =
    { /// <summary>Node identifier.</summary>
      Id: 'Id
      /// <summary>Parent node identifier, or <c>None</c> for the root.</summary>
      Parent: 'Id option
      /// <summary>Zero-based hierarchy depth.</summary>
      Depth: int
      /// <summary>Aggregated nonnegative hierarchy value.</summary>
      Value: float
      /// <summary>Counterclockwise convex polygon vertices of this cell.</summary>
      Polygon: Point list
      /// <summary>Exact area of <c>Polygon</c>.</summary>
      Area: float
      /// <summary>Area-weighted centroid of <c>Polygon</c>.</summary>
      Centroid: Point }

/// <summary>Nocaj &amp; Brandes (2012) resolution-independent hierarchical Voronoi treemap layout.</summary>
[<RequireQualifiedAccess>]
module VoronoiTreemap =
    let private rayPolygonDistance (poly: Point array) (origin: Point) (dx: float) (dy: float) : float =
        let n = poly.Length
        let mutable bestT = Double.PositiveInfinity
        for i = 0 to n - 1 do
            let a = poly[i]
            let b = poly[(i + 1) % n]
            // Outward normal for positive-area polygon: (b.Y - a.Y, -(b.X - a.X))
            let nx = b.Y - a.Y
            let ny = -(b.X - a.X)
            let denom = nx * dx + ny * dy
            if denom > 1e-24 then
                let numer = nx * (a.X - origin.X) + ny * (a.Y - origin.Y)
                let t = numer / denom
                if t >= 0. && t < bestT then
                    bestT <- t
        if Double.IsFinite bestT && bestT > 0. then bestT else 0.

    let private inscribedRadiusSq (poly: ResizeArray<Point>) (center: Point) : float =
        let n = poly.Count
        if n < 3 then 0.
        else
            let mutable bestSq = Double.PositiveInfinity
            for i = 0 to n - 1 do
                let a = poly[i]
                let b = poly[(i + 1) % n]
                let ex, ey = b.X - a.X, b.Y - a.Y
                let lenSq = ex * ex + ey * ey
                let dSq =
                    if lenSq <= 1e-30 then
                        Point.distanceSquared center a
                    else
                        let t = Math.Clamp(((center.X - a.X) * ex + (center.Y - a.Y) * ey) / lenSq, 0., 1.)
                        let px = a.X + t * ex
                        let py = a.Y + t * ey
                        (center.X - px) * (center.X - px) + (center.Y - py) * (center.Y - py)
                if dSq < bestSq then bestSq <- dSq
            if Double.IsFinite bestSq then bestSq else 0.

    let private subdivideParent
        (maxIterations: int)
        (areaTolerance: float)
        (seed: uint32)
        (nodeSalt: int)
        (parentPoly: Point array)
        (parentArea: float)
        (parentCentroid: Point)
        (targetFractions: float array)
        : struct(Point list * float * Point) array =
        let k = targetFractions.Length
        if k = 0 then
            [||]
        elif k = 1 then
            [| struct(Array.toList parentPoly, parentArea, parentCentroid) |]
        else
            let mutable minX = parentPoly[0].X
            let mutable maxX = parentPoly[0].X
            let mutable minY = parentPoly[0].Y
            let mutable maxY = parentPoly[0].Y
            for i = 1 to parentPoly.Length - 1 do
                let p = parentPoly[i]
                if p.X < minX then minX <- p.X
                if p.X > maxX then maxX <- p.X
                if p.Y < minY then minY <- p.Y
                if p.Y > maxY then maxY <- p.Y
            let diagSq = (maxX - minX) * (maxX - minX) + (maxY - minY) * (maxY - minY)
            let tolSq = max 1e-30 (diagSq * 1e-26)
            let targetAreas = Array.init k (fun i -> targetFractions[i] * parentArea)
            // Deterministic golden-angle spiral seeding strictly inside parentPoly.
            let rng0 = RandomState.create (uint64 seed ^^^ (uint64 (uint32 nodeSalt) * 0x9E3779B97F4A7C15UL))
            let struct(uPhase, _) = RandomState.nextFloat rng0
            let phase0 = 2. * Math.PI * uPhase
            let goldenAngle = Math.PI * (3. - sqrt 5.)
            let sites = Array.zeroCreate<Point> k
            for i = 0 to k - 1 do
                let theta = phase0 + float i * goldenAngle
                let struct(st, ct) = Math.SinCos theta
                let rFrac = 0.62 * sqrt ((float i + 0.5) / float k)
                let boundaryR = rayPolygonDistance parentPoly parentCentroid ct st
                let dist = rFrac * boundaryR
                sites[i] <- Point.create (parentCentroid.X + dist * ct) (parentCentroid.Y + dist * st)
            let weights = Array.zeroCreate<float> k
            let areas = Array.zeroCreate<float> k
            let centroids = Array.create k parentCentroid
            let sensitivities = Array.zeroCreate<float> k
            let inRadSq = Array.zeroCreate<float> k
            let orderBuf = Array.zeroCreate<int> k
            let keyBuf = Array.zeroCreate<float> k
            let curPoly = ResizeArray<Point>(32)
            let nextPoly = ResizeArray<Point>(32)
            let curTags = ResizeArray<int>(32)
            let nextTags = ResizeArray<int>(32)

            let evaluate (posArr: Point array) (wArr: float array) (outAreas: float array) (outCentroids: Point array) (outH: float array) (recordInRad: bool) : struct(bool * float * float) =
                let mutable allNonEmpty = true
                let mutable l1Err = 0.
                let mutable maxRelErr = 0.
                for i = 0 to k - 1 do
                    let struct(a, c, hii) =
                        ConvexClip2D.clipSingleCell parentPoly tolSq posArr wArr orderBuf keyBuf curPoly nextPoly curTags nextTags i
                    outAreas[i] <- a
                    outCentroids[i] <- c
                    outH[i] <- hii
                    if recordInRad then
                        inRadSq[i] <- inscribedRadiusSq curPoly c
                    if a <= 1e-14 * parentArea then
                        allNonEmpty <- false
                    let diff = abs (a - targetAreas[i])
                    l1Err <- l1Err + diff
                    let rel = diff / max (1e-12 * parentArea) targetAreas[i]
                    if rel > maxRelErr then maxRelErr <- rel
                struct(allNonEmpty, l1Err, maxRelErr)

            let trialWeights = Array.zeroCreate<float> k
            let trialAreas = Array.zeroCreate<float> k
            let trialCentroids = Array.create k parentCentroid
            let trialH = Array.zeroCreate<float> k
            let deltaW = Array.zeroCreate<float> k

            let mutable struct(_, curL1, curMaxRel) =
                evaluate sites weights areas centroids sensitivities true
            let mutable iter = 0
            let mutable converged = false
            while iter < maxIterations && not converged do
                if curMaxRel <= areaTolerance && iter >= 2 then
                    let mutable maxDrift = 0.
                    for i = 0 to k - 1 do
                        let d = sqrt (Point.distanceSquared sites[i] centroids[i]) / max 1e-15 (sqrt areas[i])
                        if d > maxDrift then maxDrift <- d
                    if maxDrift <= 0.025 then
                        converged <- true
                if not converged then
                    // Step 1: Nocaj & Brandes centroidal position move + inscribed-radius / non-empty weight guard.
                    let moveBlend = if iter < (maxIterations * 3) / 4 then 1.0 else 0.5
                    for i = 0 to k - 1 do
                        let c = centroids[i]
                        sites[i] <- Point.create (Check.lerp sites[i].X c.X moveBlend) (Check.lerp sites[i].Y c.Y moveBlend)
                        // Cap extreme positive magnitudes relative to domain diagonal while preserving relative shifts.
                        let cap = max (4. * inRadSq[i]) diagSq
                        weights[i] <- Math.Clamp(weights[i], -cap, cap)
                    // Zero-mean weights (power bisectors depend only on differences w_j - w_i).
                    let mutable wMean = 0.
                    for i = 0 to k - 1 do wMean <- wMean + weights[i]
                    wMean <- wMean / float k
                    for i = 0 to k - 1 do weights[i] <- weights[i] - wMean

                    // Ensure moving sites to centroids never submerges a cell; damping w -> 0.5*w is guaranteed to succeed since w=0 is unweighted Voronoi of distinct interior points.
                    let mutable struct(validAfterMove, moveL1, moveMaxRel) =
                        evaluate sites weights areas centroids sensitivities true
                    let mutable dampSteps = 0
                    while not validAfterMove && dampSteps < 30 do
                        for i = 0 to k - 1 do weights[i] <- weights[i] * 0.5
                        let struct(okM, l1M, relM) = evaluate sites weights areas centroids sensitivities true
                        validAfterMove <- okM
                        moveL1 <- l1M
                        moveMaxRel <- relM
                        dampSteps <- dampSteps + 1
                    if not validAfterMove then
                        Array.fill weights 0 k 0.
                        let struct(_, l1Z, relZ) = evaluate sites weights areas centroids sensitivities true
                        moveL1 <- l1Z
                        moveMaxRel <- relZ
                    curL1 <- moveL1
                    curMaxRel <- moveMaxRel

                    // Step 2: Inner damped-Newton / multiplicative-additive weight adaptation on concave Kantorovich dual.
                    let innerLimit = 4
                    let mutable inner = 0
                    while inner < innerLimit && curMaxRel > areaTolerance * 0.25 do
                        let fallbackH = float k / max 1e-24 diagSq
                        let mutable dMean = 0.
                        for i = 0 to k - 1 do
                            let effH = max sensitivities[i] (fallbackH * 0.1)
                            // Blend exact dA/dw sensitivity step with Nocaj-Brandes area-ratio multiplicative pressure.
                            let newtonStep = (targetAreas[i] - areas[i]) / effH
                            let ratioFactor = Math.Clamp(targetAreas[i] / max (1e-12 * parentArea) areas[i], 0.25, 4.0)
                            let multStep = inRadSq[i] * (ratioFactor - 1.) * 0.35
                            let step = 0.8 * newtonStep + 0.2 * multStep
                            deltaW[i] <- step
                            dMean <- dMean + step
                        dMean <- dMean / float k
                        for i = 0 to k - 1 do
                            deltaW[i] <- deltaW[i] - dMean

                        let mutable stepAlpha = 1.0
                        let mutable accepted = false
                        let mutable tries = 0
                        while not accepted && tries < 12 do
                            for i = 0 to k - 1 do
                                trialWeights[i] <- weights[i] + stepAlpha * deltaW[i]
                            let struct(okTrial, l1Trial, relTrial) =
                                evaluate sites trialWeights trialAreas trialCentroids trialH false
                            if okTrial && l1Trial < curL1 * (1. - 1e-12) then
                                for i = 0 to k - 1 do
                                    weights[i] <- trialWeights[i]
                                    areas[i] <- trialAreas[i]
                                    centroids[i] <- trialCentroids[i]
                                    sensitivities[i] <- trialH[i]
                                curL1 <- l1Trial
                                curMaxRel <- relTrial
                                accepted <- true
                            else
                                stepAlpha <- stepAlpha * 0.5
                                tries <- tries + 1
                        if not accepted then
                            inner <- innerLimit
                        else
                            inner <- inner + 1
                    iter <- iter + 1

            let results = Array.zeroCreate<struct(Point list * float * Point)> k
            for i = 0 to k - 1 do
                let struct(a, c, _) =
                    ConvexClip2D.clipSingleCell parentPoly tolSq sites weights orderBuf keyBuf curPoly nextPoly curTags nextTags i
                results[i] <- struct(Data.resizeToList curPoly, a, c)
            results

    /// <summary>Computes a hierarchical Voronoi treemap inside an arbitrary finite convex polygon boundary.</summary>
    let layoutInPolygon
        (maxIterations: int)
        (areaTolerance: float)
        (seed: uint32)
        (boundary: Point list)
        (root: HierarchyNode<'Id>)
        : Result<VoronoiTreemapCell<'Id> list, LayoutError list> =
        if maxIterations < 1 then
            Error [ InvalidObservation "VoronoiTreemap maxIterations must be at least 1." ]
        elif not (Double.IsFinite areaTolerance) || areaTolerance < 0. then
            Error [ InvalidObservation "VoronoiTreemap areaTolerance must be finite and nonnegative." ]
        else
            match ConvexClip2D.validateConvexBoundary boundary with
            | Error errs -> Error errs
            | Ok rootPoly ->
                HierarchyIndex.protect (fun () ->
                    let index = HierarchyIndex.create root
                    let values = HierarchyWeights.values HierarchySumPolicy.LeavesOnly index
                    let nNodes = index.Items.Length
                    let polyArr = Array.create<Point array> nNodes [||]
                    let areaArr = Array.zeroCreate<float> nNodes
                    let centArr = Array.zeroCreate<Point> nNodes
                    let rootBuf = ResizeArray<Point>(rootPoly)
                    let struct(rootArea, rootCent) = ConvexClip2D.areaAndCentroid rootPoly[0] rootBuf
                    polyArr[0] <- rootPoly
                    areaArr[0] <- rootArea
                    centArr[0] <- rootCent
                    for i = 0 to nNodes - 1 do
                        let children = index.Children[i]
                        if children.Length > 0 then
                            let pPoly = polyArr[i]
                            let pArea = areaArr[i]
                            let pCent = centArr[i]
                            if pPoly.Length < 3 || pArea <= 0. then
                                for cIdx = 0 to children.Length - 1 do
                                    let ch = children[cIdx]
                                    polyArr[ch] <- [||]
                                    areaArr[ch] <- 0.
                                    centArr[ch] <- pCent
                            else
                                let childVals = Array.init children.Length (fun cIdx -> values[children[cIdx]])
                                let totalVal = Data.sum childVals
                                if totalVal <= 0. then
                                    let eqFrac = 1. / float children.Length
                                    let fracs = Array.create children.Length eqFrac
                                    let sub = subdivideParent maxIterations areaTolerance seed (i + 1) pPoly pArea pCent fracs
                                    for cIdx = 0 to children.Length - 1 do
                                        let ch = children[cIdx]
                                        let struct(verts, a, c) = sub[cIdx]
                                        polyArr[ch] <- List.toArray verts
                                        areaArr[ch] <- a
                                        centArr[ch] <- c
                                else
                                    let activeIndices = ResizeArray<int>(children.Length)
                                    for cIdx = 0 to children.Length - 1 do
                                        if childVals[cIdx] > 0. then
                                            activeIndices.Add cIdx
                                        else
                                            let ch = children[cIdx]
                                            polyArr[ch] <- [||]
                                            areaArr[ch] <- 0.
                                            centArr[ch] <- pCent
                                    let actCount = activeIndices.Count
                                    let fracs = Array.init actCount (fun aIdx -> childVals[activeIndices[aIdx]] / totalVal)
                                    let sub = subdivideParent maxIterations areaTolerance seed (i + 1) pPoly pArea pCent fracs
                                    for aIdx = 0 to actCount - 1 do
                                        let ch = children[activeIndices[aIdx]]
                                        let struct(verts, a, c) = sub[aIdx]
                                        polyArr[ch] <- List.toArray verts
                                        areaArr[ch] <- a
                                        centArr[ch] <- c
                    let out = Array.zeroCreate<VoronoiTreemapCell<'Id>> nNodes
                    for i = 0 to nNodes - 1 do
                        let item = index.Items[i]
                        out[i] <-
                            { Id = item.Id
                              Parent = item.Parent
                              Depth = item.Depth
                              Value = values[i]
                              Polygon = Array.toList polyArr[i]
                              Area = areaArr[i]
                              Centroid = centArr[i] }
                    Array.toList out)

    /// <summary>Computes a hierarchical Voronoi treemap within rectangular or regular-polygon <c>VoronoiTreemapConfig</c> bounds.</summary>
    let layout (config: VoronoiTreemapConfig) (root: HierarchyNode<'Id>) : Result<VoronoiTreemapCell<'Id> list, LayoutError list> =
        let x, y = Bounds.x config.Bounds, Bounds.y config.Bounds
        let w, h = Bounds.width config.Bounds, Bounds.height config.Bounds
        if not (Double.IsFinite x && Double.IsFinite y && Double.IsFinite w && Double.IsFinite h) || w <= 0. || h <= 0. then
            Error [ InvalidObservation "VoronoiTreemap bounds must have positive width and height." ]
        else
            match config.BoundarySides with
            | None ->
                let rect =
                    [ Point.create x y
                      Point.create (x + w) y
                      Point.create (x + w) (y + h)
                      Point.create x (y + h) ]
                layoutInPolygon config.MaxIterations config.AreaTolerance config.Seed rect root
            | Some sides when sides < 3 || sides > 4096 ->
                Error [ InvalidObservation "VoronoiTreemap BoundarySides must be between 3 and 4096." ]
            | Some sides ->
                let cx = x + w * 0.5
                let cy = y + h * 0.5
                let radius = min w h * 0.5
                let step = 2. * Math.PI / float sides
                let gon =
                    List.init sides (fun k ->
                        let angle = -Math.PI * 0.5 + float k * step
                        let struct(sa, ca) = Math.SinCos angle
                        Point.create (cx + radius * ca) (cy + radius * sa))
                layoutInPolygon config.MaxIterations config.AreaTolerance config.Seed gon root

/// <summary>Optimization solver for 2D Centroidal Voronoi Tessellation relaxation.</summary>
[<RequireQualifiedAccess>]
type CvtSolver =
    /// <summary>Classic fixed-point Lloyd iteration moving each site to its cell centroid.</summary>
    | Lloyd
    /// <summary>Quasi-Newton L-BFGS optimization (Liu et al. 2009) minimizing exact CVT energy.</summary>
    | Lbfgs

/// <summary>2D Centroidal Voronoi Tessellation (Du-Faber-Gunzburger 1999 &amp; Liu et al. 2009).</summary>
[<RequireQualifiedAccess>]
module Cvt2D =
    /// Evaluates CVT energy E = sum_i int_{Omega_i} ||x - s_i||^2 dx, cell areas, and cell centroids.
    let private evalCvt (bounds: Bounds) (sites: Point array) : struct(VoronoiCell list * float * float array * Point array) =
        let siteList = Array.toList sites
        let mesh = Delaunay.build siteList
        let cells = Voronoi.clip bounds mesh
        let n = sites.Length
        let areas = Array.zeroCreate<float> n
        let centroids = Array.copy sites
        let mutable totalEnergy = 0.
        for cell in cells do
            let verts = List.toArray cell.Vertices
            let m = verts.Length
            if m >= 3 then
                // Map back to every original input index sharing this unique Delaunay site.
                for idx in cell.InputIndices do
                    let s = sites[idx]
                    let mutable aSum = 0.
                    let mutable cxSum = 0.
                    let mutable cySum = 0.
                    let mutable eSum = 0.
                    for k = 0 to m - 1 do
                        let p = verts[k]
                        let q = verts[(k + 1) % m]
                        let ux, uy = p.X - s.X, p.Y - s.Y
                        let vx, vy = q.X - s.X, q.Y - s.Y
                        let triArea = 0.5 * (ux * vy - vx * uy)
                        let absTri = abs triArea
                        aSum <- aSum + triArea
                        cxSum <- cxSum + triArea * (ux + vx) / 3.
                        cySum <- cySum + triArea * (uy + vy) / 3.
                        let quad = (ux * ux + uy * uy) + (ux * vx + uy * vy) + (vx * vx + vy * vy)
                        eSum <- eSum + (absTri / 6.) * quad
                    let area = abs aSum
                    if area > 1e-30 && Double.IsFinite area then
                        areas[idx] <- area
                        let signFactor = if aSum >= 0. then 1. else -1.
                        centroids[idx] <-
                            Point.create
                                (Math.Clamp(s.X + signFactor * cxSum / area, Bounds.x bounds, Bounds.x bounds + Bounds.width bounds))
                                (Math.Clamp(s.Y + signFactor * cySum / area, Bounds.y bounds, Bounds.y bounds + Bounds.height bounds))
                        totalEnergy <- totalEnergy + eSum
        struct(cells, totalEnergy, areas, centroids)

    /// <summary>Relaxes 2D generator points inside <c>bounds</c> using Lloyd iteration or L-BFGS (<c>m = 6</c>), returning the final clipped Voronoi cells and relaxed sites.</summary>
    let relax (solver: CvtSolver) (iterations: int) (bounds: Bounds) (points: Point list) : Result<VoronoiCell list * Point list, LayoutError list> =
        let bx, by = Bounds.x bounds, Bounds.y bounds
        let bw, bh = Bounds.width bounds, Bounds.height bounds
        if iterations < 0 then
            Error [ InvalidObservation "CVT iterations must be nonnegative." ]
        elif not (Double.IsFinite bx && Double.IsFinite by && Double.IsFinite bw && Double.IsFinite bh) || bw <= 0. || bh <= 0. then
            Error [ InvalidObservation "CVT bounds must have positive width and height." ]
        elif points |> List.exists (ConvexClip2D.isFinitePoint >> not) then
            Error [ InvalidObservation "CVT input points must have finite coordinates." ]
        elif points.IsEmpty then
            Ok([], [])
        else
            let n = points.Length
            let inline clampPt (p: Point) =
                Point.create (Math.Clamp(p.X, bx, bx + bw)) (Math.Clamp(p.Y, by, by + bh))
            let curSites =
                if iterations = 0 then List.toArray points
                else points |> List.map clampPt |> List.toArray
            let scaleLen = max bw bh
            match solver with
            | CvtSolver.Lloyd ->
                let mutable step = 0
                let mutable moving = true
                while step < iterations && moving do
                    let struct(_, _, areas, centroids) = evalCvt bounds curSites
                    let mutable maxShift = 0.
                    for i = 0 to n - 1 do
                        if areas[i] > 0. then
                            let shift = Double.Hypot(centroids[i].X - curSites[i].X, centroids[i].Y - curSites[i].Y)
                            if shift > maxShift then maxShift <- shift
                            curSites[i] <- centroids[i]
                    if maxShift <= 1e-12 * scaleLen then
                        moving <- false
                    step <- step + 1
                let finalList = Array.toList curSites
                let finalMesh = Delaunay.build finalList
                Ok(Voronoi.clip bounds finalMesh, finalList)
            | CvtSolver.Lbfgs ->
                let dim = 2 * n
                let mHist = 6
                let sHist = Array.init mHist (fun _ -> Array.zeroCreate<float> dim)
                let yHist = Array.init mHist (fun _ -> Array.zeroCreate<float> dim)
                let rhoHist = Array.zeroCreate<float> mHist
                let alphaBuf = Array.zeroCreate<float> mHist
                let mutable histCount = 0
                let mutable histHead = 0

                let grad = Array.zeroCreate<float> dim
                let qVec = Array.zeroCreate<float> dim
                let dir = Array.zeroCreate<float> dim
                let trialSites = Array.zeroCreate<Point> n
                let newGrad = Array.zeroCreate<float> dim

                let fillGrad (sitesArr: Point array) (areas: float array) (centroids: Point array) (gOut: float array) =
                    for i = 0 to n - 1 do
                        gOut[2 * i] <- 2. * areas[i] * (sitesArr[i].X - centroids[i].X)
                        gOut[2 * i + 1] <- 2. * areas[i] * (sitesArr[i].Y - centroids[i].Y)

                let dotVec (a: float array) (b: float array) =
                    let mutable s = 0.
                    for k = 0 to dim - 1 do s <- s + a[k] * b[k]
                    s

                let mutable struct(_, curEnergy, curAreas, curCentroids) = evalCvt bounds curSites
                fillGrad curSites curAreas curCentroids grad

                let mutable iter = 0
                let mutable converged = false
                while iter < iterations && not converged do
                    let mutable maxDisp = 0.
                    for i = 0 to n - 1 do
                        let d = Double.Hypot(curCentroids[i].X - curSites[i].X, curCentroids[i].Y - curSites[i].Y)
                        if d > maxDisp then maxDisp <- d
                    if maxDisp <= 1e-12 * scaleLen then
                        converged <- true
                    else
                        if histCount = 0 then
                            // Precondition first iteration by (2 * A_i)^(-1) -> exact Lloyd direction!
                            for i = 0 to n - 1 do
                                dir[2 * i] <- curCentroids[i].X - curSites[i].X
                                dir[2 * i + 1] <- curCentroids[i].Y - curSites[i].Y
                        else
                            for k = 0 to dim - 1 do qVec[k] <- grad[k]
                            for h = histCount - 1 downto 0 do
                                let slot = (histHead + h) % mHist
                                let a = rhoHist[slot] * dotVec sHist[slot] qVec
                                alphaBuf[slot] <- a
                                let yv = yHist[slot]
                                for k = 0 to dim - 1 do qVec[k] <- qVec[k] - a * yv[k]
                            let lastSlot = (histHead + histCount - 1) % mHist
                            let yy = dotVec yHist[lastSlot] yHist[lastSlot]
                            let sy = dotVec sHist[lastSlot] yHist[lastSlot]
                            let gamma = if yy > 1e-30 && sy > 0. then sy / yy else 1.
                            for k = 0 to dim - 1 do dir[k] <- gamma * qVec[k]
                            for h = 0 to histCount - 1 do
                                let slot = (histHead + h) % mHist
                                let beta = rhoHist[slot] * dotVec yHist[slot] dir
                                let sv = sHist[slot]
                                let diff = alphaBuf[slot] - beta
                                for k = 0 to dim - 1 do dir[k] <- dir[k] + sv[k] * diff
                            for k = 0 to dim - 1 do dir[k] <- -dir[k]
                            if dotVec dir grad >= 0. then
                                histCount <- 0
                                histHead <- 0
                                for i = 0 to n - 1 do
                                    dir[2 * i] <- curCentroids[i].X - curSites[i].X
                                    dir[2 * i + 1] <- curCentroids[i].Y - curSites[i].Y

                        let mutable stepSize = 1.0
                        let mutable accepted = false
                        let mutable lsTries = 0
                        while not accepted && lsTries < 10 do
                            let mutable dirSlope = 0.
                            for i = 0 to n - 1 do
                                let nx = Math.Clamp(curSites[i].X + stepSize * dir[2 * i], bx, bx + bw)
                                let ny = Math.Clamp(curSites[i].Y + stepSize * dir[2 * i + 1], by, by + bh)
                                trialSites[i] <- Point.create nx ny
                                dirSlope <- dirSlope + grad[2 * i] * (nx - curSites[i].X) + grad[2 * i + 1] * (ny - curSites[i].Y)
                            let struct(_, trialEnergy, trialAreas, trialCentroids) = evalCvt bounds trialSites
                            if trialEnergy <= curEnergy + 1e-4 * dirSlope && trialEnergy < curEnergy then
                                fillGrad trialSites trialAreas trialCentroids newGrad
                                let slot = (histHead + histCount) % mHist
                                if histCount = mHist then
                                    histHead <- (histHead + 1) % mHist
                                else
                                    histCount <- histCount + 1
                                let sv = sHist[slot]
                                let yv = yHist[slot]
                                for i = 0 to n - 1 do
                                    sv[2 * i] <- trialSites[i].X - curSites[i].X
                                    sv[2 * i + 1] <- trialSites[i].Y - curSites[i].Y
                                    yv[2 * i] <- newGrad[2 * i] - grad[2 * i]
                                    yv[2 * i + 1] <- newGrad[2 * i + 1] - grad[2 * i + 1]
                                let sy = dotVec sv yv
                                if sy > 1e-28 then
                                    rhoHist[slot] <- 1. / sy
                                else
                                    // Discard non-positive curvature update cleanly.
                                    histCount <- max 0 (histCount - 1)
                                for i = 0 to n - 1 do
                                    curSites[i] <- trialSites[i]
                                    curAreas[i] <- trialAreas[i]
                                    curCentroids[i] <- trialCentroids[i]
                                for k = 0 to dim - 1 do
                                    grad[k] <- newGrad[k]
                                curEnergy <- trialEnergy
                                accepted <- true
                            else
                                stepSize <- stepSize * 0.5
                                lsTries <- lsTries + 1

                        if not accepted then
                            // Fallback to safe Lloyd step if energy improves, otherwise terminate.
                            for i = 0 to n - 1 do trialSites[i] <- curCentroids[i]
                            let struct(_, lloydEnergy, lloydAreas, lloydCentroids) = evalCvt bounds trialSites
                            if lloydEnergy < curEnergy then
                                histCount <- 0
                                histHead <- 0
                                for i = 0 to n - 1 do
                                    curSites[i] <- trialSites[i]
                                    curAreas[i] <- lloydAreas[i]
                                    curCentroids[i] <- lloydCentroids[i]
                                fillGrad curSites curAreas curCentroids grad
                                curEnergy <- lloydEnergy
                            else
                                converged <- true
                        iter <- iter + 1

                let finalList = Array.toList curSites
                let finalMesh = Delaunay.build finalList
                Ok(Voronoi.clip bounds finalMesh, finalList)

/// <summary>A 3D convex Voronoi or Laguerre polyhedron cell clipped within a bounding volume.</summary>
type PolyhedronCell3D =
    { /// <summary>Zero-based site index.</summary>
      Site: int
      /// <summary>Generator site 3D coordinate.</summary>
      Position: Point3
      /// <summary>Additive power weight (0 for unweighted Voronoi).</summary>
      Weight: float
      /// <summary>Unique convex polyhedron vertices.</summary>
      Vertices: Point3 list
      /// <summary>Polygonal faces as counter-clockwise vertex index loops into <c>Vertices</c>.</summary>
      Faces: int list list
      /// <summary>Exact polyhedral volume.</summary>
      Volume: float
      /// <summary>Exact volume-weighted 3D centroid.</summary>
      Centroid: Point3 }

/// <summary>3D radical/power and unweighted Voronoi tessellations via direct convex polyhedron half-space clipping (Rycroft Voro++ 2009).</summary>
[<RequireQualifiedAccess>]
module Voronoi3D =
    type private ConvexMesh3D =
        { Verts: Point3 array
          Faces: int array array }

    let inline private isFinitePoint3 (p: Point3) =
        Double.IsFinite p.X && Double.IsFinite p.Y && Double.IsFinite p.Z

    let inline private dot3 ux uy uz vx vy vz =
        ux * vx + uy * vy + uz * vz

    let inline private cross3 ux uy uz vx vy vz =
        struct(uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx)

    let private volumeAndCentroid (fallback: Point3) (verts: Point3 array) (faces: int array array) : struct(float * Point3 * float) =
        if verts.Length < 4 || faces.Length < 4 then
            struct(0., fallback, 0.)
        else
            let p0 = verts[0]
            let mutable vol6 = 0.
            let mutable mx = 0.
            let mutable my = 0.
            let mutable mz = 0.
            for f = 0 to faces.Length - 1 do
                let face = faces[f]
                if face.Length >= 3 then
                    let v0 = verts[face[0]]
                    let ax, ay, az = v0.X - p0.X, v0.Y - p0.Y, v0.Z - p0.Z
                    for k = 1 to face.Length - 2 do
                        let vk = verts[face[k]]
                        let vl = verts[face[k + 1]]
                        let bx, by, bz = vk.X - p0.X, vk.Y - p0.Y, vk.Z - p0.Z
                        let cx, cy, cz = vl.X - p0.X, vl.Y - p0.Y, vl.Z - p0.Z
                        let struct(nx, ny, nz) = cross3 bx by bz cx cy cz
                        let det = dot3 ax ay az nx ny nz
                        vol6 <- vol6 + det
                        mx <- mx + det * (ax + bx + cx)
                        my <- my + det * (ay + by + cy)
                        mz <- mz + det * (az + bz + cz)
            let absVol = abs vol6 / 6.
            if not (Double.IsFinite absVol) || absVol <= 1e-30 then
                struct(0., fallback, vol6)
            else
                let inv4V6 = 1. / (4. * vol6)
                let cx = p0.X + mx * inv4V6
                let cy = p0.Y + my * inv4V6
                let cz = p0.Z + mz * inv4V6
                let cent =
                    if Double.IsFinite cx && Double.IsFinite cy && Double.IsFinite cz then
                        Point3.create cx cy cz
                    else
                        fallback
                struct(absVol, cent, vol6)

    /// Clips a 3D convex polyhedron against half-space n . (x - origin) <= dist, where n is a unit outward normal.
    let private clipPolyhedronByPlane
        (mesh: ConvexMesh3D)
        (origin: Point3)
        (nx: float)
        (ny: float)
        (nz: float)
        (dist: float)
        (eps: float)
        : struct(ConvexMesh3D * bool) =
        let verts = mesh.Verts
        let vCount = verts.Length
        if vCount < 4 || mesh.Faces.Length < 4 then
            struct({ Verts = [||]; Faces = [||] }, false)
        else
            let phi = Array.zeroCreate<float> vCount
            let mutable phiMin = Double.PositiveInfinity
            let mutable phiMax = Double.NegativeInfinity
            for i = 0 to vCount - 1 do
                let p = verts[i]
                let v = (p.X - origin.X) * nx + (p.Y - origin.Y) * ny + (p.Z - origin.Z) * nz - dist
                phi[i] <- v
                if v < phiMin then phiMin <- v
                if v > phiMax then phiMax <- v
            if phiMax <= eps then
                // Entire polyhedron is inside the half-space; untouched.
                struct(mesh, false)
            elif phiMin >= -eps then
                // Entire polyhedron is outside (or touches only on boundary with zero volume).
                struct({ Verts = [||]; Faces = [||] }, true)
            else
                let cls = Array.zeroCreate<int> vCount
                let newVerts = ResizeArray<Point3>(vCount + 8)
                let mapOld = Array.create vCount -1
                for i = 0 to vCount - 1 do
                    let v = phi[i]
                    if v < -eps then
                        cls[i] <- -1
                        mapOld[i] <- newVerts.Count
                        newVerts.Add verts[i]
                    elif v <= eps then
                        cls[i] <- 0
                        mapOld[i] <- newVerts.Count
                        newVerts.Add verts[i]
                    else
                        cls[i] <- 1

                let edgeCache = Dictionary<int64, int>()
                let getEdgeIntersection (a: int) (b: int) =
                    let lo, hi = if a < b then a, b else b, a
                    let key = (int64 lo <<< 32) ||| int64 (uint32 hi)
                    match edgeCache.TryGetValue key with
                    | true, idx -> idx
                    | false, _ ->
                        let pa, pb = verts[a], verts[b]
                        let fa, fb = phi[a], phi[b]
                        let denom = fa - fb
                        let t = if denom = 0. || not (Double.IsFinite denom) then 0.5 else Math.Clamp(fa / denom, 0., 1.)
                        let pt = Point3.lerp pa pb t
                        let idx = newVerts.Count
                        newVerts.Add pt
                        edgeCache.Add(key, idx)
                        idx

                let capFlags = ResizeArray<int>(16)
                let capSeen = HashSet<int>()
                let inline addCapVertex idx =
                    if capSeen.Add idx then capFlags.Add idx

                let newFaces = ResizeArray<int array>(mesh.Faces.Length + 1)
                let faceBuf = ResizeArray<int>(16)
                for f = 0 to mesh.Faces.Length - 1 do
                    let face = mesh.Faces[f]
                    let m = face.Length
                    faceBuf.Clear()
                    for k = 0 to m - 1 do
                        let a = face[k]
                        let b = face[(k + 1) % m]
                        let ca = cls[a]
                        let cb = cls[b]
                        if ca <= 0 then
                            let ma = mapOld[a]
                            if faceBuf.Count = 0 || faceBuf[faceBuf.Count - 1] <> ma then
                                faceBuf.Add ma
                            if ca = 0 then addCapVertex ma
                        if (ca = -1 && cb = 1) || (ca = 1 && cb = -1) then
                            let inter = getEdgeIntersection a b
                            if faceBuf.Count = 0 || faceBuf[faceBuf.Count - 1] <> inter then
                                faceBuf.Add inter
                            addCapVertex inter
                    while faceBuf.Count > 1 && faceBuf[faceBuf.Count - 1] = faceBuf[0] do
                        faceBuf.RemoveAt(faceBuf.Count - 1)
                    if faceBuf.Count >= 3 then
                        newFaces.Add(faceBuf.ToArray())

                // Build counter-clockwise cap face around outward normal (nx, ny, nz).
                if capFlags.Count >= 3 then
                    let mutable cx, cy, cz = 0., 0., 0.
                    let cLen = capFlags.Count
                    for k = 0 to cLen - 1 do
                        let p = newVerts[capFlags[k]]
                        cx <- cx + p.X
                        cy <- cy + p.Y
                        cz <- cz + p.Z
                    let invC = 1. / float cLen
                    cx <- cx * invC
                    cy <- cy * invC
                    cz <- cz * invC
                    // Orthonormal tangent basis (e1, e2) with e1 x e2 = +n.
                    let ax, ay, az =
                        if abs nx <= abs ny && abs nx <= abs nz then 1., 0., 0.
                        elif abs ny <= abs nz then 0., 1., 0.
                        else 0., 0., 1.
                    let struct(t1x, t1y, t1z) = cross3 ax ay az nx ny nz
                    let t1Norm = sqrt (t1x * t1x + t1y * t1y + t1z * t1z)
                    if t1Norm > 1e-20 then
                        let e1x, e1y, e1z = t1x / t1Norm, t1y / t1Norm, t1z / t1Norm
                        // e2 = n x e1 guarantees e1 x e2 = e1 x (n x e1) = +n.
                        let struct(e2x, e2y, e2z) = cross3 nx ny nz e1x e1y e1z
                        let angles = Array.zeroCreate<float> cLen
                        let capArr = capFlags.ToArray()
                        for k = 0 to cLen - 1 do
                            let p = newVerts[capArr[k]]
                            let dx, dy, dz = p.X - cx, p.Y - cy, p.Z - cz
                            let u = dot3 dx dy dz e1x e1y e1z
                            let v = dot3 dx dy dz e2x e2y e2z
                            angles[k] <- atan2 v u
                        let order = Array.init cLen id
                        Array.sortInPlaceWith (fun i j -> compare angles[i] angles[j]) order
                        let capLoop = ResizeArray<int>(cLen)
                        let dedupSq = eps * eps * 4.
                        for k = 0 to cLen - 1 do
                            let vIdx = capArr[order[k]]
                            if capLoop.Count = 0 then
                                capLoop.Add vIdx
                            else
                                let prevPt = newVerts[capLoop[capLoop.Count - 1]]
                                if Point3.distanceSquared prevPt newVerts[vIdx] > dedupSq then
                                    capLoop.Add vIdx
                        while capLoop.Count > 1 && Point3.distanceSquared newVerts[capLoop[capLoop.Count - 1]] newVerts[capLoop[0]] <= dedupSq do
                            capLoop.RemoveAt(capLoop.Count - 1)
                        if capLoop.Count >= 3 then
                            newFaces.Add(capLoop.ToArray())

                if newFaces.Count < 4 then
                    struct({ Verts = [||]; Faces = [||] }, true)
                else
                    // Compact used vertices so Vertices contains only active polyhedron corners.
                    let usedMap = Array.create newVerts.Count -1
                    let compactVerts = ResizeArray<Point3>(newVerts.Count)
                    let finalFaces = Array.zeroCreate<int array> newFaces.Count
                    for f = 0 to newFaces.Count - 1 do
                        let face = newFaces[f]
                        let mapped = Array.zeroCreate<int> face.Length
                        for k = 0 to face.Length - 1 do
                            let oldV = face[k]
                            let mutable nv = usedMap[oldV]
                            if nv < 0 then
                                nv <- compactVerts.Count
                                usedMap[oldV] <- nv
                                compactVerts.Add newVerts[oldV]
                            mapped[k] <- nv
                        finalFaces[f] <- mapped
                    if compactVerts.Count < 4 then
                        struct({ Verts = [||]; Faces = [||] }, true)
                    else
                        struct({ Verts = compactVerts.ToArray(); Faces = finalFaces }, true)

    let private cuboidMesh (bounds: Bounds3) : ConvexMesh3D =
        let x0, y0, z0 = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
        let x1, y1, z1 = Bounds3.maxX bounds, Bounds3.maxY bounds, Bounds3.maxZ bounds
        let verts =
            [| Point3.create x0 y0 z0
               Point3.create x1 y0 z0
               Point3.create x1 y1 z0
               Point3.create x0 y1 z0
               Point3.create x0 y0 z1
               Point3.create x1 y0 z1
               Point3.create x1 y1 z1
               Point3.create x0 y1 z1 |]
        let faces =
            [| [| 0; 3; 2; 1 |] // -Z
               [| 4; 5; 6; 7 |] // +Z
               [| 0; 1; 5; 4 |] // -Y
               [| 3; 7; 6; 2 |] // +Y
               [| 0; 4; 7; 3 |] // -X
               [| 1; 2; 6; 5 |] |] // +X
        { Verts = verts; Faces = faces }

    let private clipSitesInMesh (parentMesh: ConvexMesh3D) (sites: (Point3 * float) list) : Result<PolyhedronCell3D list, LayoutError list> =
        let siteArr = List.toArray sites
        let mutable allFinite = true
        for i = 0 to siteArr.Length - 1 do
            let p, w = siteArr[i]
            if not (isFinitePoint3 p && Double.IsFinite w) then
                allFinite <- false
        if not allFinite then
            Error [ InvalidObservation "3D Voronoi sites require finite coordinates and weights." ]
        elif siteArr.Length = 0 then
            Ok []
        else
            let mutable minX, maxX = parentMesh.Verts[0].X, parentMesh.Verts[0].X
            let mutable minY, maxY = parentMesh.Verts[0].Y, parentMesh.Verts[0].Y
            let mutable minZ, maxZ = parentMesh.Verts[0].Z, parentMesh.Verts[0].Z
            for i = 1 to parentMesh.Verts.Length - 1 do
                let v = parentMesh.Verts[i]
                if v.X < minX then minX <- v.X
                if v.X > maxX then maxX <- v.X
                if v.Y < minY then minY <- v.Y
                if v.Y > maxY then maxY <- v.Y
                if v.Z < minZ then minZ <- v.Z
                if v.Z > maxZ then maxZ <- v.Z
            let diag = Point3.distance (Point3.create minX minY minZ) (Point3.create maxX maxY maxZ)
            let eps = max 1e-15 (diag * 1e-11)

            let n = siteArr.Length
            let orderBuf = Array.zeroCreate<int> n
            let distBuf = Array.zeroCreate<float> n
            let cells = Array.zeroCreate<PolyhedronCell3D> n

            for i = 0 to n - 1 do
                let si, wi = siteArr[i]
                let mutable m = 0
                let mutable submerged = false
                for j = 0 to n - 1 do
                    if j <> i && not submerged then
                        let sj, wj = siteArr[j]
                        let d2 = Point3.distanceSquared si sj
                        if d2 = 0. then
                            if wj > wi || (wj = wi && j < i) then
                                submerged <- true
                        else
                            let r = sqrt d2
                            orderBuf[m] <- j
                            distBuf[m] <- (d2 - (wj - wi)) / (2. * r)
                            m <- m + 1
                if submerged then
                    cells[i] <-
                        { Site = i
                          Position = si
                          Weight = wi
                          Vertices = []
                          Faces = []
                          Volume = 0.
                          Centroid = si }
                else
                    if m > 1 then
                        let perm = Array.init m id
                        Array.sortInPlaceWith (fun a b -> compare distBuf[a] distBuf[b]) perm
                        let ordCopy = Array.init m (fun k -> orderBuf[perm[k]])
                        let dstCopy = Array.init m (fun k -> distBuf[perm[k]])
                        for k = 0 to m - 1 do
                            orderBuf[k] <- ordCopy[k]
                            distBuf[k] <- dstCopy[k]

                    let mutable curMesh = parentMesh
                    let mutable maxRadius = -1.
                    let mutable k = 0
                    while k < m && curMesh.Verts.Length >= 4 do
                        let hDist = distBuf[k]
                        if maxRadius < 0. then
                            let mutable mrSq = 0.
                            for v = 0 to curMesh.Verts.Length - 1 do
                                let dSq = Point3.distanceSquared si curMesh.Verts[v]
                                if dSq > mrSq then mrSq <- dSq
                            maxRadius <- sqrt mrSq
                        if hDist > 0. && hDist > maxRadius * (1. + 1e-11) then
                            // Early-exit: sorted radical bisector distance exceeds farthest polyhedron vertex!
                            k <- m
                        else
                            let sj, _ = siteArr[orderBuf[k]]
                            let dx, dy, dz = sj.X - si.X, sj.Y - si.Y, sj.Z - si.Z
                            let r = Point3.distance si sj
                            if r > 0. then
                                let nx, ny, nz = dx / r, dy / r, dz / r
                                let struct(clippedMesh, changed) = clipPolyhedronByPlane curMesh si nx ny nz hDist eps
                                curMesh <- clippedMesh
                                if changed then maxRadius <- -1.
                            k <- k + 1

                    let struct(vol, cent, _) = volumeAndCentroid si curMesh.Verts curMesh.Faces
                    if vol <= 0. || curMesh.Verts.Length < 4 || curMesh.Faces.Length < 4 then
                        cells[i] <-
                            { Site = i
                              Position = si
                              Weight = wi
                              Vertices = []
                              Faces = []
                              Volume = 0.
                              Centroid = si }
                    else
                        cells[i] <-
                            { Site = i
                              Position = si
                              Weight = wi
                              Vertices = Array.toList curMesh.Verts
                              Faces = curMesh.Faces |> Array.map Array.toList |> Array.toList
                              Volume = vol
                              Centroid = cent }
            Ok(Array.toList cells)

    /// <summary>Clips weighted 3D Laguerre/power polyhedra inside an arbitrary finite convex parent polyhedron.</summary>
    let clipPolyhedronWeighted
        (parentVertices: Point3 list)
        (parentFaces: int list list)
        (sites: (Point3 * float) list)
        : Result<PolyhedronCell3D list, LayoutError list> =
        let verts = List.toArray parentVertices
        if verts.Length < 4 then
            Error [ InvalidTopology "Parent 3D polyhedron requires at least four vertices." ]
        elif verts |> Array.exists (isFinitePoint3 >> not) then
            Error [ InvalidObservation "Parent 3D polyhedron vertices must be finite." ]
        elif parentFaces.Length < 4 then
            Error [ InvalidTopology "Parent 3D polyhedron requires at least four faces." ]
        else
            let rawFaces = parentFaces |> List.map List.toArray |> List.toArray
            let mutable indicesValid = true
            for f = 0 to rawFaces.Length - 1 do
                let face = rawFaces[f]
                if face.Length < 3 then
                    indicesValid <- false
                else
                    for k = 0 to face.Length - 1 do
                        if face[k] < 0 || face[k] >= verts.Length then
                            indicesValid <- false
            if not indicesValid then
                Error [ InvalidTopology "Parent 3D polyhedron faces must have at least three valid vertex indices." ]
            else
                let struct(absVol, cent, signedVol6) = volumeAndCentroid verts[0] verts rawFaces
                if absVol <= 1e-28 then
                    Error [ InvalidTopology "Parent 3D polyhedron must have positive volume." ]
                else
                    let orientedFaces =
                        if signedVol6 > 0. then rawFaces
                        else rawFaces |> Array.map Array.rev
                    // Verify convexity: every vertex lies on or behind every outward face plane.
                    let mutable convex = true
                    let mutable f = 0
                    while convex && f < orientedFaces.Length do
                        let face = orientedFaces[f]
                        let a = verts[face[0]]
                        let mutable nx, ny, nz = 0., 0., 0.
                        for k = 1 to face.Length - 2 do
                            let b = verts[face[k]]
                            let c = verts[face[k + 1]]
                            let struct(cx, cy, cz) =
                                cross3 (b.X - a.X) (b.Y - a.Y) (b.Z - a.Z) (c.X - a.X) (c.Y - a.Y) (c.Z - a.Z)
                            nx <- nx + cx
                            ny <- ny + cy
                            nz <- nz + cz
                        let nLen = sqrt (nx * nx + ny * ny + nz * nz)
                        if nLen > 1e-24 then
                            let unx, uny, unz = nx / nLen, ny / nLen, nz / nLen
                            // Check outward orientation against interior centroid.
                            let cCheck = (a.X - cent.X) * unx + (a.Y - cent.Y) * uny + (a.Z - cent.Z) * unz
                            if cCheck < -1e-8 then
                                convex <- false
                            else
                                for v = 0 to verts.Length - 1 do
                                    let pv = verts[v]
                                    let d = (pv.X - a.X) * unx + (pv.Y - a.Y) * uny + (pv.Z - a.Z) * unz
                                    if d > 1e-8 * max 1. (Point3.distance pv a) then
                                        convex <- false
                        f <- f + 1
                    if not convex then
                        Error [ InvalidTopology "Parent 3D polyhedron must be convex with consistent face winding." ]
                    else
                        clipSitesInMesh { Verts = verts; Faces = orientedFaces } sites

    /// <summary>Clips weighted 3D Laguerre/power polyhedra inside finite positive <c>Bounds3</c>.</summary>
    let clipBoundsWeighted (bounds: Bounds3) (sites: (Point3 * float) list) : Result<PolyhedronCell3D list, LayoutError list> =
        if Bounds3.sizeX bounds <= 0. || Bounds3.sizeY bounds <= 0. || Bounds3.sizeZ bounds <= 0. then
            Error [ InvalidObservation "3D Voronoi bounds must have positive extents." ]
        else
            clipSitesInMesh (cuboidMesh bounds) sites

    /// <summary>Clips unweighted 3D Voronoi polyhedra inside finite positive <c>Bounds3</c>.</summary>
    let clipBounds (bounds: Bounds3) (points: Point3 list) : Result<PolyhedronCell3D list, LayoutError list> =
        let weighted = points |> List.map (fun p -> p, 0.)
        clipBoundsWeighted bounds weighted

/// <summary>3D Centroidal Voronoi Tessellation relaxation inside an axis-aligned cuboid.</summary>
[<RequireQualifiedAccess>]
module Cvt3D =
    /// <summary>Relaxes 3D generator sites inside <c>bounds</c> via Lloyd centroid updates, returning the final polyhedron cells and relaxed points.</summary>
    let relax (iterations: int) (bounds: Bounds3) (points: Point3 list) : Result<PolyhedronCell3D list * Point3 list, LayoutError list> =
        if iterations < 0 then
            Error [ InvalidObservation "3D CVT iterations must be nonnegative." ]
        elif Bounds3.sizeX bounds <= 0. || Bounds3.sizeY bounds <= 0. || Bounds3.sizeZ bounds <= 0. then
            Error [ InvalidObservation "3D CVT bounds must have positive extents." ]
        elif points |> List.exists (fun p -> not (Double.IsFinite p.X && Double.IsFinite p.Y && Double.IsFinite p.Z)) then
            Error [ InvalidObservation "3D CVT points must have finite coordinates." ]
        elif points.IsEmpty then
            Ok([], [])
        else
            let lx, ly, lz = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
            let hx, hy, hz = Bounds3.maxX bounds, Bounds3.maxY bounds, Bounds3.maxZ bounds
            let inline clamp3 (p: Point3) =
                Point3.create (Math.Clamp(p.X, lx, hx)) (Math.Clamp(p.Y, ly, hy)) (Math.Clamp(p.Z, lz, hz))
            let cur =
                if iterations = 0 then List.toArray points
                else points |> List.map clamp3 |> List.toArray
            let scaleLen = max (Bounds3.sizeX bounds) (max (Bounds3.sizeY bounds) (Bounds3.sizeZ bounds))
            let mutable step = 0
            let mutable moving = true
            while step < iterations && moving do
                match Voronoi3D.clipBounds bounds (Array.toList cur) with
                | Error errs ->
                    moving <- false
                    ignore errs
                | Ok cells ->
                    let mutable maxMove = 0.
                    for cell in cells do
                        if cell.Volume > 0. then
                            let nextPt = clamp3 cell.Centroid
                            let d = Point3.distance cur[cell.Site] nextPt
                            if d > maxMove then maxMove <- d
                            cur[cell.Site] <- nextPt
                    if maxMove <= 1e-11 * scaleLen then
                        moving <- false
                step <- step + 1
            let finalPoints = Array.toList cur
            Voronoi3D.clipBounds bounds finalPoints
            |> Result.map (fun finalCells -> finalCells, finalPoints)

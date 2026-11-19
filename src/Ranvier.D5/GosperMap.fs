namespace Ranvier.D5

open System
open System.Collections.Generic

/// <summary>Configuration for Peano-Gosper hexagonal space-filling hierarchy layouts (Auber et al. 2013).</summary>
type GosperMapConfig =
    { /// <summary>Target scene bounds in which the hexagonal island is uniformly inscribed.</summary>
      Bounds: Bounds
      /// <summary>Recursion order in <c>1..5</c>, producing <c>7^Order</c> unit hexagons along the Gosper curve.</summary>
      Order: int
      /// <summary>How branch weights aggregate over descendants.</summary>
      SumPolicy: HierarchySumPolicy
      /// <summary>Number of junction-preserving Chaikin corner-smoothing passes (<c>0..8</c>).</summary>
      SmoothIterations: int }

/// <summary>Default constructors for <c>GosperMapConfig</c>.</summary>
[<RequireQualifiedAccess>]
module GosperMapConfig =
    /// <summary>Creates a configuration at recursion order 3 (<c>343</c> hexagons), <c>LeavesOnly</c> aggregation, and 0 smoothing passes.</summary>
    let create (bounds: Bounds) : GosperMapConfig =
        { Bounds = bounds
          Order = 3
          SumPolicy = HierarchySumPolicy.LeavesOnly
          SmoothIterations = 0 }

/// <summary>A hierarchical region on a Peano-Gosper map with exact nested boundary rings, area, and centroid.</summary>
type GosperRegion<'Id> =
    { /// <summary>Node identifier.</summary>
      Id: 'Id
      /// <summary>Parent node identifier, or <c>None</c> for the root.</summary>
      Parent: 'Id option
      /// <summary>Zero-based hierarchy depth.</summary>
      Depth: int
      /// <summary>Aggregated nonnegative node value.</summary>
      Value: float
      /// <summary>Closed boundary polygon rings (outer rings positive-signed, hole rings negative-signed, without repeated closing point).</summary>
      Boundary: Point list list
      /// <summary>Exact net area enclosed by <c>Boundary</c>.</summary>
      Area: float
      /// <summary>Area-weighted centroid of the region.</summary>
      Centroid: Point }

/// <summary>Peano-Gosper space-filling hexagonal treemap layouts (Auber et al. 2013).</summary>
[<RequireQualifiedAccess>]
module GosperMap =
    // Axial lattice step directions d_0 .. d_5 at 0, 60, 120, 180, 240, 300 degrees.
    let private dqTable = [| 1; 0; -1; -1; 0; 1 |]
    let private drTable = [| 0; 1; 1; 0; -1; -1 |]

    // Integer corner offsets (ix, iy) from hex center (cx = 2*q + r, cy = 3*r) in units of (sqrt(3)/2, 1/2).
    let private cornerDx = [| 0; 1; 1; 0; -1; -1 |]
    let private cornerDy = [| -2; -1; 1; 2; 1; -1 |]

    // Side s (corner s -> corner (s+1)%6) faces axial neighbor direction (s + 5) % 6.
    let private sideNeighborDir = [| 5; 0; 1; 2; 3; 4 |]

    type private GosperGrid =
        { HexCount: int
          HexQ: int array
          HexR: int array
          CornerCount: int
          CornerIx: int array
          CornerIy: int array
          HexCorners: int array
          HexNeighbors: int array }

    let private pow7 (order: int) =
        let mutable n = 1
        for _ = 1 to order do n <- n * 7
        n

    let private buildGrid (order: int) : GosperGrid =
        let hCount = pow7 order
        let hexQ = Array.zeroCreate<int> hCount
        let hexR = Array.zeroCreate<int> hCount
        let mutable writeIdx = 0
        let mutable curQ = 0
        let mutable curR = 0
        let mutable curDir = 0

        let inline turnLeft () = curDir <- (curDir + 1) % 6
        let inline turnRight () = curDir <- (curDir + 5) % 6
        let inline stepForward () =
            hexQ[writeIdx] <- curQ
            hexR[writeIdx] <- curR
            writeIdx <- writeIdx + 1
            curQ <- curQ + dqTable[curDir]
            curR <- curR + drTable[curDir]

        // Peano-Gosper flowsnake L-system:
        // A -> A - B - - B + A + + A A + B -
        // B -> + A - B B - - B - A + + A + B
        let rec emitA (depth: int) =
            if depth = 0 then
                stepForward ()
            else
                let d = depth - 1
                emitA d
                turnRight ()
                emitB d
                turnRight ()
                turnRight ()
                emitB d
                turnLeft ()
                emitA d
                turnLeft ()
                turnLeft ()
                emitA d
                emitA d
                turnLeft ()
                emitB d
                turnRight ()
        and emitB (depth: int) =
            if depth = 0 then
                stepForward ()
            else
                let d = depth - 1
                turnLeft ()
                emitA d
                turnRight ()
                emitB d
                emitB d
                turnRight ()
                turnRight ()
                emitB d
                turnRight ()
                emitA d
                turnLeft ()
                turnLeft ()
                emitA d
                turnLeft ()
                emitB d

        emitA order

        let hexLookup = Dictionary<int64, int>(hCount)
        for h = 0 to hCount - 1 do
            let key = (int64 hexQ[h] <<< 32) ||| int64 (uint32 hexR[h])
            hexLookup[key] <- h

        let cornerLookup = Dictionary<int64, int>(hCount * 3)
        let cornerIx = ResizeArray<int>(hCount * 3)
        let cornerIy = ResizeArray<int>(hCount * 3)
        let hexCorners = Array.zeroCreate<int> (hCount * 6)
        let hexNeighbors = Array.create (hCount * 6) -1

        for h = 0 to hCount - 1 do
            let q = hexQ[h]
            let r = hexR[h]
            let cx = 2 * q + r
            let cy = 3 * r
            for s = 0 to 5 do
                let ix = cx + cornerDx[s]
                let iy = cy + cornerDy[s]
                let cKey = (int64 ix <<< 32) ||| int64 (uint32 iy)
                let cId =
                    match cornerLookup.TryGetValue cKey with
                    | true, existing -> existing
                    | false, _ ->
                        let id = cornerIx.Count
                        cornerIx.Add ix
                        cornerIy.Add iy
                        cornerLookup.Add(cKey, id)
                        id
                hexCorners[h * 6 + s] <- cId

                let nd = sideNeighborDir[s]
                let nq = q + dqTable[nd]
                let nr = r + drTable[nd]
                let nKey = (int64 nq <<< 32) ||| int64 (uint32 nr)
                match hexLookup.TryGetValue nKey with
                | true, nbr -> hexNeighbors[h * 6 + s] <- nbr
                | false, _ -> ()

        { HexCount = hCount
          HexQ = hexQ
          HexR = hexR
          CornerCount = cornerIx.Count
          CornerIx = cornerIx.ToArray()
          CornerIy = cornerIy.ToArray()
          HexCorners = hexCorners
          HexNeighbors = hexNeighbors }

    let private fitCornersToBounds (grid: GosperGrid) (bounds: Bounds) : Point array =
        let n = grid.CornerCount
        let mutable minIx = grid.CornerIx[0]
        let mutable maxIx = grid.CornerIx[0]
        let mutable minIy = grid.CornerIy[0]
        let mutable maxIy = grid.CornerIy[0]
        for i = 1 to n - 1 do
            let ix = grid.CornerIx[i]
            let iy = grid.CornerIy[i]
            if ix < minIx then minIx <- ix
            if ix > maxIx then maxIx <- ix
            if iy < minIy then minIy <- iy
            if iy > maxIy then maxIy <- iy
        let halfSqrt3 = sqrt 3. * 0.5
        let minRx = float minIx * halfSqrt3
        let maxRx = float maxIx * halfSqrt3
        let minRy = float minIy * 0.5
        let maxRy = float maxIy * 0.5
        let spanRx = max 1e-12 (maxRx - minRx)
        let spanRy = max 1e-12 (maxRy - minRy)
        let bw = Bounds.width bounds
        let bh = Bounds.height bounds
        let scale = min (bw / spanRx) (bh / spanRy)
        let ox = Bounds.x bounds + (bw - spanRx * scale) * 0.5 - minRx * scale
        let oy = Bounds.y bounds + (bh - spanRy * scale) * 0.5 - minRy * scale
        Array.init n (fun i ->
            let rx = float grid.CornerIx[i] * halfSqrt3
            let ry = float grid.CornerIy[i] * 0.5
            Point.create (ox + rx * scale) (oy + ry * scale))

    /// Reversal-symmetric Chaikin smoothing on a closed ring, pinning hierarchy degree-3 junctions so sibling/parent borders stay identical.
    let private smoothRing (iterations: int) (cornerPts: Point array) (isPinned: bool array) (ringCorners: int array) : Point list =
        let m = ringCorners.Length
        if iterations <= 0 || m < 3 then
            let mutable out = []
            for k = m - 1 downto 0 do
                out <- cornerPts[ringCorners[k]] :: out
            out
        else
            let mutable firstPinned = -1
            let mutable i = 0
            while firstPinned < 0 && i < m do
                if isPinned[ringCorners[i]] then firstPinned <- i
                i <- i + 1

            if firstPinned < 0 then
                // Closed loop with no T-junctions: standard periodic Chaikin subdivision.
                let mutable pts = Array.init m (fun k -> cornerPts[ringCorners[k]])
                for _ = 1 to iterations do
                    let len = pts.Length
                    let next = Array.zeroCreate<Point> (len * 2)
                    for k = 0 to len - 1 do
                        let p = pts[k]
                        let q = pts[(k + 1) % len]
                        next[2 * k] <- Point.create (0.75 * p.X + 0.25 * q.X) (0.75 * p.Y + 0.25 * q.Y)
                        next[2 * k + 1] <- Point.create (0.25 * p.X + 0.75 * q.X) (0.25 * p.Y + 0.75 * q.Y)
                    pts <- next
                Array.toList pts
            else
                // Rotate ring so index 0 is a pinned junction, then smooth each open chain symmetrically between pinned endpoints.
                let rotated = Array.init m (fun k -> ringCorners[(firstPinned + k) % m])
                let pinPositions = ResizeArray<int>()
                for k = 0 to m - 1 do
                    if isPinned[rotated[k]] then pinPositions.Add k
                pinPositions.Add m // Wrap back to rotated[0]

                let smoothedAll = ResizeArray<Point>(m * (1 <<< min 4 iterations))
                for c = 0 to pinPositions.Count - 2 do
                    let startIdx = pinPositions[c]
                    let endIdx = pinPositions[c + 1]
                    let segLen = endIdx - startIdx + 1
                    let mutable chain =
                        Array.init segLen (fun k ->
                            let rIdx = (startIdx + k) % m
                            cornerPts[rotated[rIdx]])
                    if segLen >= 3 then
                        for _ = 1 to iterations do
                            let len = chain.Length
                            // Endpoints fixed + symmetric (0.75, 0.25) / (0.25, 0.75) pairs on every segment -> exact reversal symmetry!
                            let next = Array.zeroCreate<Point> (2 + 2 * (len - 1))
                            next[0] <- chain[0]
                            let mutable w = 1
                            for k = 0 to len - 2 do
                                let p = chain[k]
                                let q = chain[k + 1]
                                next[w] <- Point.create (0.75 * p.X + 0.25 * q.X) (0.75 * p.Y + 0.25 * q.Y)
                                next[w + 1] <- Point.create (0.25 * p.X + 0.75 * q.X) (0.25 * p.Y + 0.75 * q.Y)
                                w <- w + 2
                            next[w] <- chain[len - 1]
                            chain <- next
                    // Append all points of this chain except its final endpoint (which starts the next chain).
                    for k = 0 to chain.Length - 2 do
                        smoothedAll.Add chain[k]
                Data.resizeToList smoothedAll

    let private ringsAreaAndCentroid (fallback: Point) (rings: Point list list) : struct(float * Point) =
        if rings.IsEmpty then
            struct(0., fallback)
        else
            let mutable totalSignedTwice = 0.
            let mutable momentX = 0.
            let mutable momentY = 0.
            let refPt =
                match rings with
                | (p0 :: _) :: _ -> p0
                | _ -> fallback
            for ring in rings do
                let arr = List.toArray ring
                if arr.Length >= 3 then
                    let p0 = arr[0]
                    for i = 1 to arr.Length - 2 do
                        let ax = arr[i].X - refPt.X
                        let ay = arr[i].Y - refPt.Y
                        let bx = arr[i + 1].X - refPt.X
                        let by = arr[i + 1].Y - refPt.Y
                        let ox = p0.X - refPt.X
                        let oy = p0.Y - refPt.Y
                        let ux, uy = ax - ox, ay - oy
                        let vx, vy = bx - ox, by - oy
                        let cross = ux * vy - vx * uy
                        totalSignedTwice <- totalSignedTwice + cross
                        momentX <- momentX + (ox + ax + bx) * cross
                        momentY <- momentY + (oy + ay + by) * cross
            let area = 0.5 * abs totalSignedTwice
            if not (Double.IsFinite area) || area <= 1e-30 then
                struct(0., fallback)
            else
                let inv3 = 1. / (3. * totalSignedTwice)
                let cx = refPt.X + momentX * inv3
                let cy = refPt.Y + momentY * inv3
                if Double.IsFinite cx && Double.IsFinite cy then
                    struct(area, Point.create cx cy)
                else
                    struct(area, fallback)

    /// Allocates totalHexes integer counts proportionally to slotWeights via Hamilton largest-remainder, guaranteeing >= 1 hex for positive weights when feasible.
    let private allocateHexCounts (totalHexes: int) (slotWeights: float array) : int array =
        let m = slotWeights.Length
        if m = 0 then
            [||]
        else
            let sumW = Data.sum slotWeights
            let effective =
                if sumW <= 0. then Array.create m 1.
                else slotWeights
            let effTotal = if sumW <= 0. then float m else sumW
            let quotas = Array.init m (fun i -> float totalHexes * (effective[i] / effTotal))
            let counts = Array.init m (fun i -> int (floor quotas[i]))
            let mutable assigned = 0
            for i = 0 to m - 1 do assigned <- assigned + counts[i]
            let rem = totalHexes - assigned
            if rem > 0 then
                let order = Array.init m id
                Array.sortInPlaceWith (fun a b ->
                    let ra = quotas[a] - float counts[a]
                    let rb = quotas[b] - float counts[b]
                    let c = compare rb ra
                    if c <> 0 then c else compare a b) order
                for k = 0 to min (rem - 1) (m - 1) do
                    let idx = order[k]
                    counts[idx] <- counts[idx] + 1

            // Ensure every positive-weight slot receives at least 1 hexagon whenever positiveSlots <= totalHexes.
            let mutable posSlots = 0
            for i = 0 to m - 1 do
                if effective[i] > 0. then posSlots <- posSlots + 1
            if posSlots <= totalHexes then
                for i = 0 to m - 1 do
                    if effective[i] > 0. && counts[i] = 0 then
                        // Borrow 1 hexagon from the donor slot with largest surplus above 1.
                        let mutable bestDonor = -1
                        let mutable bestSurplus = Double.NegativeInfinity
                        for j = 0 to m - 1 do
                            if counts[j] >= 2 then
                                let surplus = float counts[j] - quotas[j]
                                if surplus > bestSurplus then
                                    bestSurplus <- surplus
                                    bestDonor <- j
                        if bestDonor >= 0 then
                            counts[bestDonor] <- counts[bestDonor] - 1
                            counts[i] <- 1
            counts

    /// <summary>Returns the ordered <c>7^order</c> hexagon centers along the Peano-Gosper curve uniformly inscribed in <c>bounds</c>.</summary>
    let curve (order: int) (bounds: Bounds) : Result<Point list, LayoutError list> =
        let bw, bh = Bounds.width bounds, Bounds.height bounds
        if order < 1 || order > 5 then
            Error [ InvalidObservation "GosperMap Order must be between 1 and 5." ]
        elif not (Double.IsFinite bw && Double.IsFinite bh) || bw <= 0. || bh <= 0. then
            Error [ InvalidObservation "GosperMap bounds must have positive width and height." ]
        else
            let grid = buildGrid order
            let cornerPts = fitCornersToBounds grid bounds
            let centers = Array.zeroCreate<Point> grid.HexCount
            for h = 0 to grid.HexCount - 1 do
                let mutable sx = 0.
                let mutable sy = 0.
                for s = 0 to 5 do
                    let p = cornerPts[grid.HexCorners[h * 6 + s]]
                    sx <- sx + p.X
                    sy <- sy + p.Y
                centers[h] <- Point.create (sx / 6.) (sy / 6.)
            Ok(Array.toList centers)

    /// <summary>Computes a topologically nested Peano-Gosper hexagonal treemap of <c>root</c> inside <c>config.Bounds</c>.</summary>
    let layout (config: GosperMapConfig) (root: HierarchyNode<'Id>) : Result<GosperRegion<'Id> list, LayoutError list> =
        let bw, bh = Bounds.width config.Bounds, Bounds.height config.Bounds
        if config.Order < 1 || config.Order > 5 then
            Error [ InvalidObservation "GosperMap Order must be between 1 and 5." ]
        elif config.SmoothIterations < 0 || config.SmoothIterations > 8 then
            Error [ InvalidObservation "GosperMap SmoothIterations must be between 0 and 8." ]
        elif not (Double.IsFinite bw && Double.IsFinite bh) || bw <= 0. || bh <= 0. then
            Error [ InvalidObservation "GosperMap bounds must have positive width and height." ]
        else
            HierarchyIndex.protect (fun () ->
                let index = HierarchyIndex.create root
                let values = HierarchyWeights.values config.SumPolicy index
                let nNodes = index.Items.Length
                let grid = buildGrid config.Order
                let cornerPts = fitCornersToBounds grid config.Bounds
                let defaultCenter =
                    Point.create
                        (Bounds.x config.Bounds + bw * 0.5)
                        (Bounds.y config.Bounds + bh * 0.5)

                // Build depth-first leaf/own-weight slots so every node owns a contiguous hex range [hexStart[u], hexEnd[u]).
                let slotNode = ResizeArray<int>(nNodes)
                let slotWeight = ResizeArray<float>(nNodes)
                let firstSlot = Array.zeroCreate<int> nNodes
                let lastSlot = Array.zeroCreate<int> nNodes

                let rec assignSlots (u: int) =
                    firstSlot[u] <- slotNode.Count
                    let children = index.Children[u]
                    if children.Length = 0 then
                        slotNode.Add u
                        slotWeight.Add values[u]
                    else
                        let mutable childSum = 0.
                        for cIdx = 0 to children.Length - 1 do
                            let ch = children[cIdx]
                            childSum <- childSum + values[ch]
                            assignSlots ch
                        if config.SumPolicy = HierarchySumPolicy.IncludeOwnWeight then
                            let ownSlack = max 0. (values[u] - childSum)
                            if ownSlack > 0. then
                                slotNode.Add u
                                slotWeight.Add ownSlack
                    lastSlot[u] <- slotNode.Count

                assignSlots 0

                let counts = allocateHexCounts grid.HexCount (slotWeight.ToArray())
                let slotPrefix = Array.zeroCreate<int> (counts.Length + 1)
                for s = 0 to counts.Length - 1 do
                    slotPrefix[s + 1] <- slotPrefix[s] + counts[s]

                let hexStart = Array.init nNodes (fun u -> slotPrefix[firstSlot[u]])
                let hexEnd = Array.init nNodes (fun u -> slotPrefix[lastSlot[u]])

                // Mark pinned degree-3 junctions on the finest slot partition so Chaikin smoothing keeps sibling/parent boundaries watertight.
                let hexOwnerSlot = Array.create grid.HexCount -1
                for s = 0 to counts.Length - 1 do
                    for h = slotPrefix[s] to slotPrefix[s + 1] - 1 do
                        hexOwnerSlot[h] <- s

                let cornerBoundaryDeg = Array.zeroCreate<int> grid.CornerCount
                for h = 0 to grid.HexCount - 1 do
                    let owner = hexOwnerSlot[h]
                    for s = 0 to 5 do
                        let nbr = grid.HexNeighbors[h * 6 + s]
                        let nbrOwner = if nbr >= 0 then hexOwnerSlot[nbr] else -1
                        if owner <> nbrOwner && (nbr < 0 || h < nbr) then
                            let cA = grid.HexCorners[h * 6 + s]
                            let cB = grid.HexCorners[h * 6 + ((s + 1) % 6)]
                            cornerBoundaryDeg[cA] <- cornerBoundaryDeg[cA] + 1
                            cornerBoundaryDeg[cB] <- cornerBoundaryDeg[cB] + 1

                let isPinned = Array.init grid.CornerCount (fun c -> cornerBoundaryDeg[c] >= 3)

                // Reusable O(perimeter) half-edge cycle tracer (in a 3-valent hex grid, every boundary vertex has out-degree <= 1).
                let nextCorner = Array.create grid.CornerCount -1
                let activeStarts = ResizeArray<int>(256)
                let ringBuf = ResizeArray<int>(256)
                let regions = Array.zeroCreate<GosperRegion<'Id>> nNodes

                for u = 0 to nNodes - 1 do
                    let lo = hexStart[u]
                    let hi = hexEnd[u]
                    let item = index.Items[u]
                    if lo >= hi then
                        regions[u] <-
                            { Id = item.Id
                              Parent = item.Parent
                              Depth = item.Depth
                              Value = values[u]
                              Boundary = []
                              Area = 0.
                              Centroid = defaultCenter }
                    else
                        activeStarts.Clear()
                        for h = lo to hi - 1 do
                            let baseIdx = h * 6
                            for s = 0 to 5 do
                                let nbr = grid.HexNeighbors[baseIdx + s]
                                if nbr < lo || nbr >= hi then
                                    let cA = grid.HexCorners[baseIdx + s]
                                    let cB = grid.HexCorners[baseIdx + ((s + 1) % 6)]
                                    nextCorner[cA] <- cB
                                    activeStarts.Add cA

                        let rings = ResizeArray<Point list>(2)
                        for idx = 0 to activeStarts.Count - 1 do
                            let startC = activeStarts[idx]
                            if nextCorner[startC] >= 0 then
                                ringBuf.Clear()
                                let mutable cur = startC
                                while cur >= 0 do
                                    ringBuf.Add cur
                                    let nxt = nextCorner[cur]
                                    nextCorner[cur] <- -1
                                    cur <- if nxt = startC then -1 else nxt
                                if ringBuf.Count >= 3 then
                                    let smoothed = smoothRing config.SmoothIterations cornerPts isPinned (ringBuf.ToArray())
                                    rings.Add smoothed

                        // Ensure nextCorner is completely clean even if degenerate.
                        for idx = 0 to activeStarts.Count - 1 do
                            nextCorner[activeStarts[idx]] <- -1

                        let ringList = Data.resizeToList rings
                        let struct(area, centroid) = ringsAreaAndCentroid defaultCenter ringList
                        regions[u] <-
                            { Id = item.Id
                              Parent = item.Parent
                              Depth = item.Depth
                              Value = values[u]
                              Boundary = ringList
                              Area = area
                              Centroid = centroid }

                Array.toList regions)

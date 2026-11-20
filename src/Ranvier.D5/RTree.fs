namespace Ranvier.D5

open System
open System.Collections.Generic
#if !FABLE_COMPILER
open System.Numerics
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif

module internal RTreeInternal =
    [<Literal>]
    let NodeDegree = 16

    let inline clz32 (v: uint32) : int =
#if !FABLE_COMPILER
        BitOperations.LeadingZeroCount v
#else
        if v = 0u then
            32
        else
            let mutable x = v
            let mutable n = 0
            if (x &&& 0xFFFF0000u) = 0u then
                n <- n + 16
                x <- x <<< 16
            if (x &&& 0xFF000000u) = 0u then
                n <- n + 8
                x <- x <<< 8
            if (x &&& 0xF0000000u) = 0u then
                n <- n + 4
                x <- x <<< 4
            if (x &&& 0xC0000000u) = 0u then
                n <- n + 2
                x <- x <<< 2
            if (x &&& 0x80000000u) = 0u then
                n <- n + 1
            n
#endif

    let inline normalizeCoord (lo: float) (hi: float) (v: float) : float =
        if lo >= hi then
            0.
        else
            let span = hi - lo
            let c = min hi (max lo v)
            if Double.IsFinite span then
                (c - lo) / span
            else
                let halfSpan = hi * 0.5 - lo * 0.5
                (c * 0.5 - lo * 0.5) / halfSpan

    let inline quantizeNorm16 (t: float) : uint16 =
        let q = int (floor (t * 65536.))
        uint16 (min 65535 (max 0 q))

    let inline quantizeNorm10 (t: float) : uint16 =
        let q = int (floor (t * 1024.))
        uint16 (min 1023 (max 0 q))

    let inline boxDist2D (px: float) (py: float) (minX: float) (minY: float) (maxX: float) (maxY: float) : float =
        let dx =
            if px < minX then minX - px
            elif px > maxX then px - maxX
            else 0.
        let dy =
            if py < minY then minY - py
            elif py > maxY then py - maxY
            else 0.
        Double.Hypot(dx, dy)

    let inline hypot3NonNeg (dx: float) (dy: float) (dz: float) : float =
        let m = max dx (max dy dz)
        if m = 0. then
            0.
        elif Double.IsPositiveInfinity m then
            Double.PositiveInfinity
        elif m > 1e-140 && m < 1e140 then
            sqrt (dx * dx + dy * dy + dz * dz)
        else
            let rx = dx / m
            let ry = dy / m
            let rz = dz / m
            let d = m * sqrt (rx * rx + ry * ry + rz * rz)
            Check.finite "distance" d
            d

    let inline boxDist3D
        (px: float)
        (py: float)
        (pz: float)
        (minX: float)
        (minY: float)
        (minZ: float)
        (maxX: float)
        (maxY: float)
        (maxZ: float)
        : float =
        let dx =
            if px < minX then minX - px
            elif px > maxX then px - maxX
            else 0.
        let dy =
            if py < minY then minY - py
            elif py > maxY then py - maxY
            else 0.
        let dz =
            if pz < minZ then minZ - pz
            elif pz > maxZ then pz - maxZ
            else 0.
        hypot3NonNeg dx dy dz

    let inline slabIntersect3D
        (ox: float)
        (oy: float)
        (oz: float)
        (dx: float)
        (dy: float)
        (dz: float)
        (invDx: float)
        (invDy: float)
        (invDz: float)
        (fastInv: bool)
        (limitT: float)
        (bMinX: float)
        (bMinY: float)
        (bMinZ: float)
        (bMaxX: float)
        (bMaxY: float)
        (bMaxZ: float)
        : struct(bool * float) =
        if fastInv then
            let tx1 = (bMinX - ox) * invDx
            let tx2 = (bMaxX - ox) * invDx
            let mutable tEnter = max 0. (min tx1 tx2)
            let mutable tExit = min limitT (max tx1 tx2)
            let ty1 = (bMinY - oy) * invDy
            let ty2 = (bMaxY - oy) * invDy
            tEnter <- max tEnter (min ty1 ty2)
            tExit <- min tExit (max ty1 ty2)
            let tz1 = (bMinZ - oz) * invDz
            let tz2 = (bMaxZ - oz) * invDz
            tEnter <- max tEnter (min tz1 tz2)
            tExit <- min tExit (max tz1 tz2)
            struct(tEnter <= tExit, tEnter)
        else
            let mutable tEnter = 0.
            let mutable tExit = limitT
            let mutable ok = true
            if dx = 0. then
                if ox < bMinX || ox > bMaxX then
                    ok <- false
            else
                let tx1 = (bMinX - ox) / dx
                let tx2 = (bMaxX - ox) / dx
                tEnter <- max tEnter (min tx1 tx2)
                tExit <- min tExit (max tx1 tx2)
            if ok then
                if dy = 0. then
                    if oy < bMinY || oy > bMaxY then
                        ok <- false
                else
                    let ty1 = (bMinY - oy) / dy
                    let ty2 = (bMaxY - oy) / dy
                    tEnter <- max tEnter (min ty1 ty2)
                    tExit <- min tExit (max ty1 ty2)
            if ok then
                if dz = 0. then
                    if oz < bMinZ || oz > bMaxZ then
                        ok <- false
                else
                    let tz1 = (bMinZ - oz) / dz
                    let tz2 = (bMaxZ - oz) / dz
                    tEnter <- max tEnter (min tz1 tz2)
                    tExit <- min tExit (max tz1 tz2)
            struct(ok && tEnter <= tExit, tEnter)

    let captureArray<'T> (items: seq<'T>) : 'T array =
        match box items with
        | :? ('T array) as arr -> Array.copy arr
        | :? ('T list) as lst -> List.toArray lst
        | _ -> Seq.toArray items

    let indicesToSortedItems<'T> (origItems: 'T array) (indices: ResizeArray<int>) : 'T list =
        let n = indices.Count
        if n = 0 then
            []
        else
            let arr = indices.ToArray()
            Array.sortInPlace arr
            let mutable acc = []
            let mutable i = n - 1
            while i >= 0 do
                acc <- origItems[arr[i]] :: acc
                i <- i - 1
            acc

    let hitsToSortedList<'T> (origItems: 'T array) (hits: ResizeArray<struct(float * int)>) : ('T * float) list =
        let n = hits.Count
        if n = 0 then
            []
        else
            let arr = hits.ToArray()
            Array.sortInPlaceWith
                (fun struct(t1, i1) struct(t2, i2) ->
                    let c = compare t1 t2
                    if c <> 0 then c else compare i1 i2)
                arr
            let mutable acc = []
            let mutable i = n - 1
            while i >= 0 do
                let struct(tVal, origIdx) = arr[i]
                acc <- (origItems[origIdx], tVal) :: acc
                i <- i - 1
            acc

    // Fixed-capacity binary max-heap keeping the k smallest (distance, origIndex) pairs.
    type TopKCollector(capacity: int) =
        let heap = Array.zeroCreate<struct(float * int)> capacity
        let mutable count = 0

        let isWorse (struct(d1, i1): struct(float * int)) (struct(d2, i2): struct(float * int)) =
            d1 > d2 || (d1 = d2 && i1 > i2)

        member _.Count = count

        member _.PruneBound(maxDistance: float) =
            if count < capacity then
                maxDistance
            else
                let struct(worstD, _) = heap[0]
                worstD

        member _.Offer(dist: float, origIdx: int) =
            let candidate = struct(dist, origIdx)
            if count < capacity then
                heap[count] <- candidate
                let mutable pos = count
                count <- count + 1
                let mutable moving = pos > 0
                while moving do
                    let parent = (pos - 1) >>> 1
                    if isWorse heap[pos] heap[parent] then
                        let tmp = heap[parent]
                        heap[parent] <- heap[pos]
                        heap[pos] <- tmp
                        pos <- parent
                        moving <- pos > 0
                    else
                        moving <- false
            elif isWorse heap[0] candidate then
                heap[0] <- candidate
                let mutable pos = 0
                let mutable moving = true
                while moving do
                    let l = (pos <<< 1) + 1
                    let r = l + 1
                    let mutable worst = pos
                    if l < capacity && isWorse heap[l] heap[worst] then
                        worst <- l
                    if r < capacity && isWorse heap[r] heap[worst] then
                        worst <- r
                    if worst = pos then
                        moving <- false
                    else
                        let tmp = heap[pos]
                        heap[pos] <- heap[worst]
                        heap[worst] <- tmp
                        pos <- worst

        member _.ToSortedItems<'T>(origItems: 'T array) : 'T list =
            if count = 0 then
                []
            else
                let res = Array.sub heap 0 count
                Array.sortInPlaceWith
                    (fun struct(d1, i1) struct(d2, i2) ->
                        let c = compare d1 d2
                        if c <> 0 then c else compare i1 i2)
                    res
                let mutable acc = []
                let mutable i = count - 1
                while i >= 0 do
                    let struct(_, origIdx) = res[i]
                    acc <- origItems[origIdx] :: acc
                    i <- i - 1
                acc

/// <summary>An immutable static Hilbert-packed 2D R-tree (Kamel &amp; Faloutsos 1994; Flatbush SoA layout) over <c>Bounds</c>.</summary>
type RTree2D<'T> =
    private
        { BoxMinX: float array
          BoxMinY: float array
          BoxMaxX: float array
          BoxMaxY: float array
          NodeIndices: int array
          LevelBounds: int array
          OrigItems: 'T array
          Envelope: Bounds option }

/// <summary>Builds and queries static packed 2D Hilbert R-trees with SIMD multi-child AABB acceleration and original-order tie breaking.</summary>
[<RequireQualifiedAccess>]
module RTree2D =
    /// <summary>Packs <c>items</c> into a flat SoA 2D Hilbert R-tree of node degree 16.</summary>
    let build (project: 'T -> Bounds) (items: seq<'T>) : RTree2D<'T> =
        let origItems = RTreeInternal.captureArray items
        let n = origItems.Length
        if n = 0 then
            { BoxMinX = [||]
              BoxMinY = [||]
              BoxMaxX = [||]
              BoxMaxY = [||]
              NodeIndices = [||]
              LevelBounds = [||]
              OrigItems = [||]
              Envelope = None }
        else
            let rawMinX = Array.zeroCreate<float> n
            let rawMinY = Array.zeroCreate<float> n
            let rawMaxX = Array.zeroCreate<float> n
            let rawMaxY = Array.zeroCreate<float> n
            let mutable envMinX = Double.PositiveInfinity
            let mutable envMinY = Double.PositiveInfinity
            let mutable envMaxX = Double.NegativeInfinity
            let mutable envMaxY = Double.NegativeInfinity
            for i in 0 .. n - 1 do
                let b = project origItems[i]
                let x0 = Bounds.x b
                let y0 = Bounds.y b
                let x1 = x0 + Bounds.width b
                let y1 = y0 + Bounds.height b
                rawMinX[i] <- x0
                rawMinY[i] <- y0
                rawMaxX[i] <- x1
                rawMaxY[i] <- y1
                if x0 < envMinX then envMinX <- x0
                if y0 < envMinY then envMinY <- y0
                if x1 > envMaxX then envMaxX <- x1
                if y1 > envMaxY then envMaxY <- y1
            let envelope = Bounds.create envMinX envMinY (envMaxX - envMinX) (envMaxY - envMinY)
            let order = Array.zeroCreate<struct(uint32 * int)> n
            for i in 0 .. n - 1 do
                let cx = rawMinX[i] + (rawMaxX[i] - rawMinX[i]) * 0.5
                let cy = rawMinY[i] + (rawMaxY[i] - rawMinY[i]) * 0.5
                let qx = RTreeInternal.quantizeNorm16 (RTreeInternal.normalizeCoord envMinX envMaxX cx)
                let qy = RTreeInternal.quantizeNorm16 (RTreeInternal.normalizeCoord envMinY envMaxY cy)
                let code = SpaceFillingBits.hilbert2DEncodeCore 16 (uint32 qx) (uint32 qy)
                order[i] <- struct(code, i)
            Array.sortInPlaceWith
                (fun struct(c1, i1) struct(c2, i2) ->
                    if c1 < c2 then -1
                    elif c1 > c2 then 1
                    elif i1 < i2 then -1
                    elif i1 > i2 then 1
                    else 0)
                order
            let levelEnds = ResizeArray<int>()
            levelEnds.Add n
            let mutable totalNodes = n
            let mutable levelCount = n
            let mutable buildingLevels = true
            while buildingLevels do
                levelCount <- (levelCount + RTreeInternal.NodeDegree - 1) / RTreeInternal.NodeDegree
                totalNodes <- totalNodes + levelCount
                levelEnds.Add totalNodes
                if levelCount <= 1 then
                    buildingLevels <- false
            let levelBounds = levelEnds.ToArray()
            let boxMinX = Array.zeroCreate<float> totalNodes
            let boxMinY = Array.zeroCreate<float> totalNodes
            let boxMaxX = Array.zeroCreate<float> totalNodes
            let boxMaxY = Array.zeroCreate<float> totalNodes
            let nodeIndices = Array.zeroCreate<int> totalNodes
            for i in 0 .. n - 1 do
                let struct(_, origIdx) = order[i]
                boxMinX[i] <- rawMinX[origIdx]
                boxMinY[i] <- rawMinY[origIdx]
                boxMaxX[i] <- rawMaxX[origIdx]
                boxMaxY[i] <- rawMaxY[origIdx]
                nodeIndices[i] <- origIdx
            let mutable pos = 0
            let mutable outPos = n
            for lvl in 0 .. levelBounds.Length - 2 do
                let endPos = levelBounds[lvl]
                while pos < endPos do
                    let nodeStart = pos
                    let childEnd = min (pos + RTreeInternal.NodeDegree) endPos
                    let mutable nx0 = boxMinX[pos]
                    let mutable ny0 = boxMinY[pos]
                    let mutable nx1 = boxMaxX[pos]
                    let mutable ny1 = boxMaxY[pos]
                    pos <- pos + 1
                    while pos < childEnd do
                        if boxMinX[pos] < nx0 then nx0 <- boxMinX[pos]
                        if boxMinY[pos] < ny0 then ny0 <- boxMinY[pos]
                        if boxMaxX[pos] > nx1 then nx1 <- boxMaxX[pos]
                        if boxMaxY[pos] > ny1 then ny1 <- boxMaxY[pos]
                        pos <- pos + 1
                    boxMinX[outPos] <- nx0
                    boxMinY[outPos] <- ny0
                    boxMaxX[outPos] <- nx1
                    boxMaxY[outPos] <- ny1
                    nodeIndices[outPos] <- nodeStart
                    outPos <- outPos + 1
            { BoxMinX = boxMinX
              BoxMinY = boxMinY
              BoxMaxX = boxMaxX
              BoxMaxY = boxMaxY
              NodeIndices = nodeIndices
              LevelBounds = levelBounds
              OrigItems = origItems
              Envelope = Some envelope }

    /// <summary>Returns the number of indexed items.</summary>
    let count (tree: RTree2D<'T>) : int =
        tree.OrigItems.Length

    /// <summary>Returns the enclosing rectangle of all indexed items, or <c>None</c> when empty.</summary>
    let bounds (tree: RTree2D<'T>) : Bounds option =
        tree.Envelope

    /// <summary>Returns all items whose <c>Bounds</c> intersect <c>query</c>, ordered by original input index.</summary>
    let search (query: Bounds) (tree: RTree2D<'T>) : 'T list =
        if tree.OrigItems.Length = 0 then
            []
        else
            let qx0 = Bounds.x query
            let qy0 = Bounds.y query
            let qx1 = qx0 + Bounds.width query
            let qy1 = qy0 + Bounds.height query
            let minXs = tree.BoxMinX
            let minYs = tree.BoxMinY
            let maxXs = tree.BoxMaxX
            let maxYs = tree.BoxMaxY
            let indices = tree.NodeIndices
            let levelBounds = tree.LevelBounds
            let topLevel = levelBounds.Length - 1
            let rootIdx = levelBounds[topLevel] - 1
            if maxXs[rootIdx] < qx0 || minXs[rootIdx] > qx1 || maxYs[rootIdx] < qy0 || minYs[rootIdx] > qy1 then
                []
            else
                let results = ResizeArray<int>()
                let stack = ResizeArray<struct(int * int)>(32)
                stack.Add(struct(rootIdx, topLevel))
#if !FABLE_COMPILER
                let rMinX = &MemoryMarshal.GetArrayDataReference minXs
                let rMinY = &MemoryMarshal.GetArrayDataReference minYs
                let rMaxX = &MemoryMarshal.GetArrayDataReference maxXs
                let rMaxY = &MemoryMarshal.GetArrayDataReference maxYs
#endif
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(nodeIdx, level) = stack[last]
                    stack.RemoveAt last
                    let childLevel = level - 1
                    let cStart = indices[nodeIdx]
                    let cEnd = min (cStart + RTreeInternal.NodeDegree) levelBounds[childLevel]
                    let mutable i = cStart
#if !FABLE_COMPILER
                    if Vector512.IsHardwareAccelerated && cEnd - i >= 8 then
                        let vQx0 = Vector512.Create qx0
                        let vQy0 = Vector512.Create qy0
                        let vQx1 = Vector512.Create qx1
                        let vQy1 = Vector512.Create qy1
                        while i <= cEnd - 8 do
                            let vMinX = Vector512.LoadUnsafe(&rMinX, unativeint i)
                            let vMinY = Vector512.LoadUnsafe(&rMinY, unativeint i)
                            let vMaxX = Vector512.LoadUnsafe(&rMaxX, unativeint i)
                            let vMaxY = Vector512.LoadUnsafe(&rMaxY, unativeint i)
                            let hit =
                                Vector512.GreaterThanOrEqual(vMaxX, vQx0)
                                &&& Vector512.LessThanOrEqual(vMinX, vQx1)
                                &&& Vector512.GreaterThanOrEqual(vMaxY, vQy0)
                                &&& Vector512.LessThanOrEqual(vMinY, vQy1)
                            if hit <> Vector512<float>.Zero then
                                let mask = hit.AsInt64()
                                for lane in 0 .. 7 do
                                    if mask.GetElement lane < 0L then
                                        let child = i + lane
                                        if childLevel = 0 then
                                            results.Add indices[child]
                                        else
                                            stack.Add(struct(child, childLevel))
                            i <- i + 8
                    elif Vector256.IsHardwareAccelerated && cEnd - i >= 4 then
                        let vQx0 = Vector256.Create qx0
                        let vQy0 = Vector256.Create qy0
                        let vQx1 = Vector256.Create qx1
                        let vQy1 = Vector256.Create qy1
                        while i <= cEnd - 4 do
                            let vMinX = Vector256.LoadUnsafe(&rMinX, unativeint i)
                            let vMinY = Vector256.LoadUnsafe(&rMinY, unativeint i)
                            let vMaxX = Vector256.LoadUnsafe(&rMaxX, unativeint i)
                            let vMaxY = Vector256.LoadUnsafe(&rMaxY, unativeint i)
                            let hit =
                                Vector256.GreaterThanOrEqual(vMaxX, vQx0)
                                &&& Vector256.LessThanOrEqual(vMinX, vQx1)
                                &&& Vector256.GreaterThanOrEqual(vMaxY, vQy0)
                                &&& Vector256.LessThanOrEqual(vMinY, vQy1)
                            if hit <> Vector256<float>.Zero then
                                let mask = hit.AsInt64()
                                for lane in 0 .. 3 do
                                    if mask.GetElement lane < 0L then
                                        let child = i + lane
                                        if childLevel = 0 then
                                            results.Add indices[child]
                                        else
                                            stack.Add(struct(child, childLevel))
                            i <- i + 4
                    elif Vector128.IsHardwareAccelerated && cEnd - i >= 2 then
                        let vQx0 = Vector128.Create qx0
                        let vQy0 = Vector128.Create qy0
                        let vQx1 = Vector128.Create qx1
                        let vQy1 = Vector128.Create qy1
                        while i <= cEnd - 2 do
                            let vMinX = Vector128.LoadUnsafe(&rMinX, unativeint i)
                            let vMinY = Vector128.LoadUnsafe(&rMinY, unativeint i)
                            let vMaxX = Vector128.LoadUnsafe(&rMaxX, unativeint i)
                            let vMaxY = Vector128.LoadUnsafe(&rMaxY, unativeint i)
                            let hit =
                                Vector128.GreaterThanOrEqual(vMaxX, vQx0)
                                &&& Vector128.LessThanOrEqual(vMinX, vQx1)
                                &&& Vector128.GreaterThanOrEqual(vMaxY, vQy0)
                                &&& Vector128.LessThanOrEqual(vMinY, vQy1)
                            if hit <> Vector128<float>.Zero then
                                let mask = hit.AsInt64()
                                for lane in 0 .. 1 do
                                    if mask.GetElement lane < 0L then
                                        let child = i + lane
                                        if childLevel = 0 then
                                            results.Add indices[child]
                                        else
                                            stack.Add(struct(child, childLevel))
                            i <- i + 2
#endif
                    while i < cEnd do
                        if maxXs[i] >= qx0 && minXs[i] <= qx1 && maxYs[i] >= qy0 && minYs[i] <= qy1 then
                            if childLevel = 0 then
                                results.Add indices[i]
                            else
                                stack.Add(struct(i, childLevel))
                        i <- i + 1
                RTreeInternal.indicesToSortedItems tree.OrigItems results

    /// <summary>Returns all items whose <c>Bounds</c> are wholly contained within <c>query</c>, ordered by original input index.</summary>
    let within (query: Bounds) (tree: RTree2D<'T>) : 'T list =
        if tree.OrigItems.Length = 0 then
            []
        else
            let qx0 = Bounds.x query
            let qy0 = Bounds.y query
            let qx1 = qx0 + Bounds.width query
            let qy1 = qy0 + Bounds.height query
            let minXs = tree.BoxMinX
            let minYs = tree.BoxMinY
            let maxXs = tree.BoxMaxX
            let maxYs = tree.BoxMaxY
            let indices = tree.NodeIndices
            let levelBounds = tree.LevelBounds
            let topLevel = levelBounds.Length - 1
            let rootIdx = levelBounds[topLevel] - 1
            if maxXs[rootIdx] < qx0 || minXs[rootIdx] > qx1 || maxYs[rootIdx] < qy0 || minYs[rootIdx] > qy1 then
                []
            else
                let results = ResizeArray<int>()
                let stack = ResizeArray<struct(int * int * bool)>(32)
                let rootContained =
                    minXs[rootIdx] >= qx0 && maxXs[rootIdx] <= qx1 && minYs[rootIdx] >= qy0 && maxYs[rootIdx] <= qy1
                stack.Add(struct(rootIdx, topLevel, rootContained))
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(nodeIdx, level, allInside) = stack[last]
                    stack.RemoveAt last
                    let childLevel = level - 1
                    let cStart = indices[nodeIdx]
                    let cEnd = min (cStart + RTreeInternal.NodeDegree) levelBounds[childLevel]
                    if allInside then
                        if childLevel = 0 then
                            for i in cStart .. cEnd - 1 do
                                results.Add indices[i]
                        else
                            for i in cStart .. cEnd - 1 do
                                stack.Add(struct(i, childLevel, true))
                    elif childLevel = 0 then
                        for i in cStart .. cEnd - 1 do
                            if minXs[i] >= qx0 && maxXs[i] <= qx1 && minYs[i] >= qy0 && maxYs[i] <= qy1 then
                                results.Add indices[i]
                    else
                        for i in cStart .. cEnd - 1 do
                            if maxXs[i] >= qx0 && minXs[i] <= qx1 && maxYs[i] >= qy0 && minYs[i] <= qy1 then
                                let subInside =
                                    minXs[i] >= qx0 && maxXs[i] <= qx1 && minYs[i] >= qy0 && maxYs[i] <= qy1
                                stack.Add(struct(i, childLevel, subInside))
                RTreeInternal.indicesToSortedItems tree.OrigItems results

    /// <summary>Returns up to <c>k</c> nearest items to <c>point</c> within inclusive <c>maxDistance</c>, breaking distance ties by original input index.</summary>
    let kNearest (k: int) (point: Point) (maxDistance: float) (tree: RTree2D<'T>) : 'T list =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        if k < 0 then invalidArg "k" "Neighbor count k must be nonnegative."
        if Double.IsNaN maxDistance || maxDistance < 0. then
            invalidArg "maxDistance" "Maximum distance must be nonnegative."
        if k = 0 || tree.OrigItems.Length = 0 then
            []
        else
            let px = point.X
            let py = point.Y
            let minXs = tree.BoxMinX
            let minYs = tree.BoxMinY
            let maxXs = tree.BoxMaxX
            let maxYs = tree.BoxMaxY
            let indices = tree.NodeIndices
            let levelBounds = tree.LevelBounds
            let topLevel = levelBounds.Length - 1
            let rootIdx = levelBounds[topLevel] - 1
            let rootDist = RTreeInternal.boxDist2D px py minXs[rootIdx] minYs[rootIdx] maxXs[rootIdx] maxYs[rootIdx]
            let inline nodeAllowed bound d =
                if Double.IsPositiveInfinity bound then true
                else d <= bound * (1. + 1e-14) + 1e-300
            if not (nodeAllowed maxDistance rootDist) then
                []
            else
                let topK = RTreeInternal.TopKCollector k
#if FABLE_COMPILER
                let pq = FableCompat.PriorityQueue<struct(int * int * float), float>()
#else
                let pq = PriorityQueue<struct(int * int * float), float>(32)
#endif
                pq.Enqueue(struct(rootIdx, topLevel, rootDist), rootDist)
                let mutable searching = true
                while searching && pq.Count > 0 do
                    let struct(nodeIdx, level, nodeDist) = pq.Dequeue()
                    let curBound = topK.PruneBound maxDistance
                    if not (nodeAllowed curBound nodeDist) then
                        searching <- false
                    else
                        let childLevel = level - 1
                        let cStart = indices[nodeIdx]
                        let cEnd = min (cStart + RTreeInternal.NodeDegree) levelBounds[childLevel]
                        if childLevel = 0 then
                            for i in cStart .. cEnd - 1 do
                                let d = RTreeInternal.boxDist2D px py minXs[i] minYs[i] maxXs[i] maxYs[i]
                                if d <= maxDistance then
                                    topK.Offer(d, indices[i])
                        else
                            let nextBound = topK.PruneBound maxDistance
                            for i in cStart .. cEnd - 1 do
                                let cd = RTreeInternal.boxDist2D px py minXs[i] minYs[i] maxXs[i] maxYs[i]
                                if nodeAllowed nextBound cd then
                                    pq.Enqueue(struct(i, childLevel, cd), cd)
                topK.ToSortedItems tree.OrigItems

    /// <summary>Returns the nearest item to <c>point</c> within inclusive <c>maxDistance</c>, breaking ties by original input index.</summary>
    let nearest (point: Point) (maxDistance: float) (tree: RTree2D<'T>) : 'T option =
        match kNearest 1 point maxDistance tree with
        | head :: _ -> Some head
        | [] -> None

    /// <summary>Performs a synchronized hierarchical spatial join, returning all intersecting <c>(left, right)</c> item pairs in deterministic input-index order.</summary>
    let spatialJoin (left: RTree2D<'A>) (right: RTree2D<'B>) : ('A * 'B) list =
        if left.OrigItems.Length = 0 || right.OrigItems.Length = 0 then
            []
        else
            let lMinX = left.BoxMinX
            let lMinY = left.BoxMinY
            let lMaxX = left.BoxMaxX
            let lMaxY = left.BoxMaxY
            let lIndices = left.NodeIndices
            let lBounds = left.LevelBounds
            let rMinX = right.BoxMinX
            let rMinY = right.BoxMinY
            let rMaxX = right.BoxMaxX
            let rMaxY = right.BoxMaxY
            let rIndices = right.NodeIndices
            let rBounds = right.LevelBounds
            let topA = lBounds.Length - 1
            let topB = rBounds.Length - 1
            let rootA = lBounds[topA] - 1
            let rootB = rBounds[topB] - 1
            let inline overlaps ia ib =
                lMaxX[ia] >= rMinX[ib]
                && lMinX[ia] <= rMaxX[ib]
                && lMaxY[ia] >= rMinY[ib]
                && lMinY[ia] <= rMaxY[ib]
            if not (overlaps rootA rootB) then
                []
            else
                let pairs = ResizeArray<struct(int * int)>()
                let stack = ResizeArray<struct(int * int * int * int)>(64)
                stack.Add(struct(rootA, topA, rootB, topB))
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(na, la, nb, lb) = stack[last]
                    stack.RemoveAt last
                    if la > lb then
                        let childLa = la - 1
                        let aStart = lIndices[na]
                        let aEnd = min (aStart + RTreeInternal.NodeDegree) lBounds[childLa]
                        for ca in aStart .. aEnd - 1 do
                            if overlaps ca nb then
                                stack.Add(struct(ca, childLa, nb, lb))
                    elif lb > la then
                        let childLb = lb - 1
                        let bStart = rIndices[nb]
                        let bEnd = min (bStart + RTreeInternal.NodeDegree) rBounds[childLb]
                        for cb in bStart .. bEnd - 1 do
                            if overlaps na cb then
                                stack.Add(struct(na, la, cb, childLb))
                    else
                        let childLvl = la - 1
                        let aStart = lIndices[na]
                        let aEnd = min (aStart + RTreeInternal.NodeDegree) lBounds[childLvl]
                        let bStart = rIndices[nb]
                        let bEnd = min (bStart + RTreeInternal.NodeDegree) rBounds[childLvl]
                        for ca in aStart .. aEnd - 1 do
                            if overlaps ca nb then
                                for cb in bStart .. bEnd - 1 do
                                    if overlaps ca cb then
                                        if childLvl = 0 then
                                            pairs.Add(struct(lIndices[ca], rIndices[cb]))
                                        else
                                            stack.Add(struct(ca, childLvl, cb, childLvl))
                let countPairs = pairs.Count
                if countPairs = 0 then
                    []
                else
                    let arr = pairs.ToArray()
                    Array.sortInPlaceWith
                        (fun struct(a1, b1) struct(a2, b2) ->
                            let c = compare a1 a2
                            if c <> 0 then c else compare b1 b2)
                        arr
                    let lItems = left.OrigItems
                    let rItems = right.OrigItems
                    let mutable acc = []
                    let mutable i = countPairs - 1
                    while i >= 0 do
                        let struct(ia, ib) = arr[i]
                        acc <- (lItems[ia], rItems[ib]) :: acc
                        i <- i - 1
                    acc

/// <summary>An immutable static Hilbert-packed 3D R-tree over <c>Bounds3</c>.</summary>
type RTree3D<'T> =
    private
        { Box3MinX: float array
          Box3MinY: float array
          Box3MinZ: float array
          Box3MaxX: float array
          Box3MaxY: float array
          Box3MaxZ: float array
          Node3Indices: int array
          Level3Bounds: int array
          Orig3Items: 'T array
          Envelope3: Bounds3 option }

/// <summary>Builds and queries static packed 3D Hilbert R-trees with SIMD AABB filtering, branchless slab raycasting and spatial joins.</summary>
[<RequireQualifiedAccess>]
module RTree3D =
    /// <summary>Packs <c>items</c> into a flat SoA 3D Hilbert R-tree of node degree 16.</summary>
    let build (project: 'T -> Bounds3) (items: seq<'T>) : RTree3D<'T> =
        let origItems = RTreeInternal.captureArray items
        let n = origItems.Length
        if n = 0 then
            { Box3MinX = [||]
              Box3MinY = [||]
              Box3MinZ = [||]
              Box3MaxX = [||]
              Box3MaxY = [||]
              Box3MaxZ = [||]
              Node3Indices = [||]
              Level3Bounds = [||]
              Orig3Items = [||]
              Envelope3 = None }
        else
            let rawMinX = Array.zeroCreate<float> n
            let rawMinY = Array.zeroCreate<float> n
            let rawMinZ = Array.zeroCreate<float> n
            let rawMaxX = Array.zeroCreate<float> n
            let rawMaxY = Array.zeroCreate<float> n
            let rawMaxZ = Array.zeroCreate<float> n
            let mutable envMinX = Double.PositiveInfinity
            let mutable envMinY = Double.PositiveInfinity
            let mutable envMinZ = Double.PositiveInfinity
            let mutable envMaxX = Double.NegativeInfinity
            let mutable envMaxY = Double.NegativeInfinity
            let mutable envMaxZ = Double.NegativeInfinity
            for i in 0 .. n - 1 do
                let b = project origItems[i]
                let x0 = Bounds3.minX b
                let y0 = Bounds3.minY b
                let z0 = Bounds3.minZ b
                let x1 = Bounds3.maxX b
                let y1 = Bounds3.maxY b
                let z1 = Bounds3.maxZ b
                rawMinX[i] <- x0
                rawMinY[i] <- y0
                rawMinZ[i] <- z0
                rawMaxX[i] <- x1
                rawMaxY[i] <- y1
                rawMaxZ[i] <- z1
                if x0 < envMinX then envMinX <- x0
                if y0 < envMinY then envMinY <- y0
                if z0 < envMinZ then envMinZ <- z0
                if x1 > envMaxX then envMaxX <- x1
                if y1 > envMaxY then envMaxY <- y1
                if z1 > envMaxZ then envMaxZ <- z1
            let envelope =
                Bounds3.create
                    envMinX
                    envMinY
                    envMinZ
                    (envMaxX - envMinX)
                    (envMaxY - envMinY)
                    (envMaxZ - envMinZ)
            let order = Array.zeroCreate<struct(uint32 * int)> n
            for i in 0 .. n - 1 do
                let cx = rawMinX[i] + (rawMaxX[i] - rawMinX[i]) * 0.5
                let cy = rawMinY[i] + (rawMaxY[i] - rawMinY[i]) * 0.5
                let cz = rawMinZ[i] + (rawMaxZ[i] - rawMinZ[i]) * 0.5
                let qx = RTreeInternal.quantizeNorm10 (RTreeInternal.normalizeCoord envMinX envMaxX cx)
                let qy = RTreeInternal.quantizeNorm10 (RTreeInternal.normalizeCoord envMinY envMaxY cy)
                let qz = RTreeInternal.quantizeNorm10 (RTreeInternal.normalizeCoord envMinZ envMaxZ cz)
                let code = SpaceFillingBits.hilbert3DEncodeCore 10 (uint32 qx) (uint32 qy) (uint32 qz)
                order[i] <- struct(code, i)
            Array.sortInPlaceWith
                (fun struct(c1, i1) struct(c2, i2) ->
                    if c1 < c2 then -1
                    elif c1 > c2 then 1
                    elif i1 < i2 then -1
                    elif i1 > i2 then 1
                    else 0)
                order
            let levelEnds = ResizeArray<int>()
            levelEnds.Add n
            let mutable totalNodes = n
            let mutable levelCount = n
            let mutable buildingLevels = true
            while buildingLevels do
                levelCount <- (levelCount + RTreeInternal.NodeDegree - 1) / RTreeInternal.NodeDegree
                totalNodes <- totalNodes + levelCount
                levelEnds.Add totalNodes
                if levelCount <= 1 then
                    buildingLevels <- false
            let levelBounds = levelEnds.ToArray()
            let boxMinX = Array.zeroCreate<float> totalNodes
            let boxMinY = Array.zeroCreate<float> totalNodes
            let boxMinZ = Array.zeroCreate<float> totalNodes
            let boxMaxX = Array.zeroCreate<float> totalNodes
            let boxMaxY = Array.zeroCreate<float> totalNodes
            let boxMaxZ = Array.zeroCreate<float> totalNodes
            let nodeIndices = Array.zeroCreate<int> totalNodes
            for i in 0 .. n - 1 do
                let struct(_, origIdx) = order[i]
                boxMinX[i] <- rawMinX[origIdx]
                boxMinY[i] <- rawMinY[origIdx]
                boxMinZ[i] <- rawMinZ[origIdx]
                boxMaxX[i] <- rawMaxX[origIdx]
                boxMaxY[i] <- rawMaxY[origIdx]
                boxMaxZ[i] <- rawMaxZ[origIdx]
                nodeIndices[i] <- origIdx
            let mutable pos = 0
            let mutable outPos = n
            for lvl in 0 .. levelBounds.Length - 2 do
                let endPos = levelBounds[lvl]
                while pos < endPos do
                    let nodeStart = pos
                    let childEnd = min (pos + RTreeInternal.NodeDegree) endPos
                    let mutable nx0 = boxMinX[pos]
                    let mutable ny0 = boxMinY[pos]
                    let mutable nz0 = boxMinZ[pos]
                    let mutable nx1 = boxMaxX[pos]
                    let mutable ny1 = boxMaxY[pos]
                    let mutable nz1 = boxMaxZ[pos]
                    pos <- pos + 1
                    while pos < childEnd do
                        if boxMinX[pos] < nx0 then nx0 <- boxMinX[pos]
                        if boxMinY[pos] < ny0 then ny0 <- boxMinY[pos]
                        if boxMinZ[pos] < nz0 then nz0 <- boxMinZ[pos]
                        if boxMaxX[pos] > nx1 then nx1 <- boxMaxX[pos]
                        if boxMaxY[pos] > ny1 then ny1 <- boxMaxY[pos]
                        if boxMaxZ[pos] > nz1 then nz1 <- boxMaxZ[pos]
                        pos <- pos + 1
                    boxMinX[outPos] <- nx0
                    boxMinY[outPos] <- ny0
                    boxMinZ[outPos] <- nz0
                    boxMaxX[outPos] <- nx1
                    boxMaxY[outPos] <- ny1
                    boxMaxZ[outPos] <- nz1
                    nodeIndices[outPos] <- nodeStart
                    outPos <- outPos + 1
            { Box3MinX = boxMinX
              Box3MinY = boxMinY
              Box3MinZ = boxMinZ
              Box3MaxX = boxMaxX
              Box3MaxY = boxMaxY
              Box3MaxZ = boxMaxZ
              Node3Indices = nodeIndices
              Level3Bounds = levelBounds
              Orig3Items = origItems
              Envelope3 = Some envelope }

    /// <summary>Returns the number of indexed items.</summary>
    let count (tree: RTree3D<'T>) : int =
        tree.Orig3Items.Length

    /// <summary>Returns the enclosing 3D cuboid of all indexed items, or <c>None</c> when empty.</summary>
    let bounds (tree: RTree3D<'T>) : Bounds3 option =
        tree.Envelope3

    /// <summary>Returns all items whose <c>Bounds3</c> intersect <c>query</c>, ordered by original input index.</summary>
    let search (query: Bounds3) (tree: RTree3D<'T>) : 'T list =
        if tree.Orig3Items.Length = 0 then
            []
        else
            let qx0 = Bounds3.minX query
            let qy0 = Bounds3.minY query
            let qz0 = Bounds3.minZ query
            let qx1 = Bounds3.maxX query
            let qy1 = Bounds3.maxY query
            let qz1 = Bounds3.maxZ query
            let minXs = tree.Box3MinX
            let minYs = tree.Box3MinY
            let minZs = tree.Box3MinZ
            let maxXs = tree.Box3MaxX
            let maxYs = tree.Box3MaxY
            let maxZs = tree.Box3MaxZ
            let indices = tree.Node3Indices
            let levelBounds = tree.Level3Bounds
            let topLevel = levelBounds.Length - 1
            let rootIdx = levelBounds[topLevel] - 1
            if
                maxXs[rootIdx] < qx0 || minXs[rootIdx] > qx1
                || maxYs[rootIdx] < qy0 || minYs[rootIdx] > qy1
                || maxZs[rootIdx] < qz0 || minZs[rootIdx] > qz1
            then
                []
            else
                let results = ResizeArray<int>()
                let stack = ResizeArray<struct(int * int)>(32)
                stack.Add(struct(rootIdx, topLevel))
#if !FABLE_COMPILER
                let rMinX = &MemoryMarshal.GetArrayDataReference minXs
                let rMinY = &MemoryMarshal.GetArrayDataReference minYs
                let rMinZ = &MemoryMarshal.GetArrayDataReference minZs
                let rMaxX = &MemoryMarshal.GetArrayDataReference maxXs
                let rMaxY = &MemoryMarshal.GetArrayDataReference maxYs
                let rMaxZ = &MemoryMarshal.GetArrayDataReference maxZs
#endif
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(nodeIdx, level) = stack[last]
                    stack.RemoveAt last
                    let childLevel = level - 1
                    let cStart = indices[nodeIdx]
                    let cEnd = min (cStart + RTreeInternal.NodeDegree) levelBounds[childLevel]
                    let mutable i = cStart
#if !FABLE_COMPILER
                    if Vector512.IsHardwareAccelerated && cEnd - i >= 8 then
                        let vQx0 = Vector512.Create qx0
                        let vQy0 = Vector512.Create qy0
                        let vQz0 = Vector512.Create qz0
                        let vQx1 = Vector512.Create qx1
                        let vQy1 = Vector512.Create qy1
                        let vQz1 = Vector512.Create qz1
                        while i <= cEnd - 8 do
                            let vMinX = Vector512.LoadUnsafe(&rMinX, unativeint i)
                            let vMinY = Vector512.LoadUnsafe(&rMinY, unativeint i)
                            let vMinZ = Vector512.LoadUnsafe(&rMinZ, unativeint i)
                            let vMaxX = Vector512.LoadUnsafe(&rMaxX, unativeint i)
                            let vMaxY = Vector512.LoadUnsafe(&rMaxY, unativeint i)
                            let vMaxZ = Vector512.LoadUnsafe(&rMaxZ, unativeint i)
                            let hit =
                                Vector512.GreaterThanOrEqual(vMaxX, vQx0)
                                &&& Vector512.LessThanOrEqual(vMinX, vQx1)
                                &&& Vector512.GreaterThanOrEqual(vMaxY, vQy0)
                                &&& Vector512.LessThanOrEqual(vMinY, vQy1)
                                &&& Vector512.GreaterThanOrEqual(vMaxZ, vQz0)
                                &&& Vector512.LessThanOrEqual(vMinZ, vQz1)
                            if hit <> Vector512<float>.Zero then
                                let mask = hit.AsInt64()
                                for lane in 0 .. 7 do
                                    if mask.GetElement lane < 0L then
                                        let child = i + lane
                                        if childLevel = 0 then
                                            results.Add indices[child]
                                        else
                                            stack.Add(struct(child, childLevel))
                            i <- i + 8
                    elif Vector256.IsHardwareAccelerated && cEnd - i >= 4 then
                        let vQx0 = Vector256.Create qx0
                        let vQy0 = Vector256.Create qy0
                        let vQz0 = Vector256.Create qz0
                        let vQx1 = Vector256.Create qx1
                        let vQy1 = Vector256.Create qy1
                        let vQz1 = Vector256.Create qz1
                        while i <= cEnd - 4 do
                            let vMinX = Vector256.LoadUnsafe(&rMinX, unativeint i)
                            let vMinY = Vector256.LoadUnsafe(&rMinY, unativeint i)
                            let vMinZ = Vector256.LoadUnsafe(&rMinZ, unativeint i)
                            let vMaxX = Vector256.LoadUnsafe(&rMaxX, unativeint i)
                            let vMaxY = Vector256.LoadUnsafe(&rMaxY, unativeint i)
                            let vMaxZ = Vector256.LoadUnsafe(&rMaxZ, unativeint i)
                            let hit =
                                Vector256.GreaterThanOrEqual(vMaxX, vQx0)
                                &&& Vector256.LessThanOrEqual(vMinX, vQx1)
                                &&& Vector256.GreaterThanOrEqual(vMaxY, vQy0)
                                &&& Vector256.LessThanOrEqual(vMinY, vQy1)
                                &&& Vector256.GreaterThanOrEqual(vMaxZ, vQz0)
                                &&& Vector256.LessThanOrEqual(vMinZ, vQz1)
                            if hit <> Vector256<float>.Zero then
                                let mask = hit.AsInt64()
                                for lane in 0 .. 3 do
                                    if mask.GetElement lane < 0L then
                                        let child = i + lane
                                        if childLevel = 0 then
                                            results.Add indices[child]
                                        else
                                            stack.Add(struct(child, childLevel))
                            i <- i + 4
                    elif Vector128.IsHardwareAccelerated && cEnd - i >= 2 then
                        let vQx0 = Vector128.Create qx0
                        let vQy0 = Vector128.Create qy0
                        let vQz0 = Vector128.Create qz0
                        let vQx1 = Vector128.Create qx1
                        let vQy1 = Vector128.Create qy1
                        let vQz1 = Vector128.Create qz1
                        while i <= cEnd - 2 do
                            let vMinX = Vector128.LoadUnsafe(&rMinX, unativeint i)
                            let vMinY = Vector128.LoadUnsafe(&rMinY, unativeint i)
                            let vMinZ = Vector128.LoadUnsafe(&rMinZ, unativeint i)
                            let vMaxX = Vector128.LoadUnsafe(&rMaxX, unativeint i)
                            let vMaxY = Vector128.LoadUnsafe(&rMaxY, unativeint i)
                            let vMaxZ = Vector128.LoadUnsafe(&rMaxZ, unativeint i)
                            let hit =
                                Vector128.GreaterThanOrEqual(vMaxX, vQx0)
                                &&& Vector128.LessThanOrEqual(vMinX, vQx1)
                                &&& Vector128.GreaterThanOrEqual(vMaxY, vQy0)
                                &&& Vector128.LessThanOrEqual(vMinY, vQy1)
                                &&& Vector128.GreaterThanOrEqual(vMaxZ, vQz0)
                                &&& Vector128.LessThanOrEqual(vMinZ, vQz1)
                            if hit <> Vector128<float>.Zero then
                                let mask = hit.AsInt64()
                                for lane in 0 .. 1 do
                                    if mask.GetElement lane < 0L then
                                        let child = i + lane
                                        if childLevel = 0 then
                                            results.Add indices[child]
                                        else
                                            stack.Add(struct(child, childLevel))
                            i <- i + 2
#endif
                    while i < cEnd do
                        if
                            maxXs[i] >= qx0 && minXs[i] <= qx1
                            && maxYs[i] >= qy0 && minYs[i] <= qy1
                            && maxZs[i] >= qz0 && minZs[i] <= qz1
                        then
                            if childLevel = 0 then
                                results.Add indices[i]
                            else
                                stack.Add(struct(i, childLevel))
                        i <- i + 1
                RTreeInternal.indicesToSortedItems tree.Orig3Items results

    /// <summary>Returns all items whose <c>Bounds3</c> are wholly contained within <c>query</c>, ordered by original input index.</summary>
    let within (query: Bounds3) (tree: RTree3D<'T>) : 'T list =
        if tree.Orig3Items.Length = 0 then
            []
        else
            let qx0 = Bounds3.minX query
            let qy0 = Bounds3.minY query
            let qz0 = Bounds3.minZ query
            let qx1 = Bounds3.maxX query
            let qy1 = Bounds3.maxY query
            let qz1 = Bounds3.maxZ query
            let minXs = tree.Box3MinX
            let minYs = tree.Box3MinY
            let minZs = tree.Box3MinZ
            let maxXs = tree.Box3MaxX
            let maxYs = tree.Box3MaxY
            let maxZs = tree.Box3MaxZ
            let indices = tree.Node3Indices
            let levelBounds = tree.Level3Bounds
            let topLevel = levelBounds.Length - 1
            let rootIdx = levelBounds[topLevel] - 1
            if
                maxXs[rootIdx] < qx0 || minXs[rootIdx] > qx1
                || maxYs[rootIdx] < qy0 || minYs[rootIdx] > qy1
                || maxZs[rootIdx] < qz0 || minZs[rootIdx] > qz1
            then
                []
            else
                let results = ResizeArray<int>()
                let stack = ResizeArray<struct(int * int * bool)>(32)
                let rootContained =
                    minXs[rootIdx] >= qx0 && maxXs[rootIdx] <= qx1
                    && minYs[rootIdx] >= qy0 && maxYs[rootIdx] <= qy1
                    && minZs[rootIdx] >= qz0 && maxZs[rootIdx] <= qz1
                stack.Add(struct(rootIdx, topLevel, rootContained))
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(nodeIdx, level, allInside) = stack[last]
                    stack.RemoveAt last
                    let childLevel = level - 1
                    let cStart = indices[nodeIdx]
                    let cEnd = min (cStart + RTreeInternal.NodeDegree) levelBounds[childLevel]
                    if allInside then
                        if childLevel = 0 then
                            for i in cStart .. cEnd - 1 do
                                results.Add indices[i]
                        else
                            for i in cStart .. cEnd - 1 do
                                stack.Add(struct(i, childLevel, true))
                    elif childLevel = 0 then
                        for i in cStart .. cEnd - 1 do
                            if
                                minXs[i] >= qx0 && maxXs[i] <= qx1
                                && minYs[i] >= qy0 && maxYs[i] <= qy1
                                && minZs[i] >= qz0 && maxZs[i] <= qz1
                            then
                                results.Add indices[i]
                    else
                        for i in cStart .. cEnd - 1 do
                            if
                                maxXs[i] >= qx0 && minXs[i] <= qx1
                                && maxYs[i] >= qy0 && minYs[i] <= qy1
                                && maxZs[i] >= qz0 && minZs[i] <= qz1
                            then
                                let subInside =
                                    minXs[i] >= qx0 && maxXs[i] <= qx1
                                    && minYs[i] >= qy0 && maxYs[i] <= qy1
                                    && minZs[i] >= qz0 && maxZs[i] <= qz1
                                stack.Add(struct(i, childLevel, subInside))
                RTreeInternal.indicesToSortedItems tree.Orig3Items results

    /// <summary>Returns up to <c>k</c> nearest items to <c>point</c> within inclusive <c>maxDistance</c>, breaking distance ties by original input index.</summary>
    let kNearest (k: int) (point: Point3) (maxDistance: float) (tree: RTree3D<'T>) : 'T list =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        Check.finite "z" point.Z
        if k < 0 then invalidArg "k" "Neighbor count k must be nonnegative."
        if Double.IsNaN maxDistance || maxDistance < 0. then
            invalidArg "maxDistance" "Maximum distance must be nonnegative."
        if k = 0 || tree.Orig3Items.Length = 0 then
            []
        else
            let px = point.X
            let py = point.Y
            let pz = point.Z
            let minXs = tree.Box3MinX
            let minYs = tree.Box3MinY
            let minZs = tree.Box3MinZ
            let maxXs = tree.Box3MaxX
            let maxYs = tree.Box3MaxY
            let maxZs = tree.Box3MaxZ
            let indices = tree.Node3Indices
            let levelBounds = tree.Level3Bounds
            let topLevel = levelBounds.Length - 1
            let rootIdx = levelBounds[topLevel] - 1
            let rootDist =
                RTreeInternal.boxDist3D
                    px
                    py
                    pz
                    minXs[rootIdx]
                    minYs[rootIdx]
                    minZs[rootIdx]
                    maxXs[rootIdx]
                    maxYs[rootIdx]
                    maxZs[rootIdx]
            let inline nodeAllowed bound d =
                if Double.IsPositiveInfinity bound then true
                else d <= bound * (1. + 1e-14) + 1e-300
            if not (nodeAllowed maxDistance rootDist) then
                []
            else
                let topK = RTreeInternal.TopKCollector k
#if FABLE_COMPILER
                let pq = FableCompat.PriorityQueue<struct(int * int * float), float>()
#else
                let pq = PriorityQueue<struct(int * int * float), float>(32)
#endif
                pq.Enqueue(struct(rootIdx, topLevel, rootDist), rootDist)
                let mutable searching = true
                while searching && pq.Count > 0 do
                    let struct(nodeIdx, level, nodeDist) = pq.Dequeue()
                    let curBound = topK.PruneBound maxDistance
                    if not (nodeAllowed curBound nodeDist) then
                        searching <- false
                    else
                        let childLevel = level - 1
                        let cStart = indices[nodeIdx]
                        let cEnd = min (cStart + RTreeInternal.NodeDegree) levelBounds[childLevel]
                        if childLevel = 0 then
                            for i in cStart .. cEnd - 1 do
                                let d =
                                    RTreeInternal.boxDist3D
                                        px
                                        py
                                        pz
                                        minXs[i]
                                        minYs[i]
                                        minZs[i]
                                        maxXs[i]
                                        maxYs[i]
                                        maxZs[i]
                                if d <= maxDistance then
                                    topK.Offer(d, indices[i])
                        else
                            let nextBound = topK.PruneBound maxDistance
                            for i in cStart .. cEnd - 1 do
                                let cd =
                                    RTreeInternal.boxDist3D
                                        px
                                        py
                                        pz
                                        minXs[i]
                                        minYs[i]
                                        minZs[i]
                                        maxXs[i]
                                        maxYs[i]
                                        maxZs[i]
                                if nodeAllowed nextBound cd then
                                    pq.Enqueue(struct(i, childLevel, cd), cd)
                topK.ToSortedItems tree.Orig3Items

    /// <summary>Returns the nearest item to <c>point</c> within inclusive <c>maxDistance</c>, breaking ties by original input index.</summary>
    let nearest (point: Point3) (maxDistance: float) (tree: RTree3D<'T>) : 'T option =
        match kNearest 1 point maxDistance tree with
        | head :: _ -> Some head
        | [] -> None

    /// <summary>Intersects ray <c>origin + t * direction</c> for <c>t</c> in <c>[0, maxT]</c> against all items, returning <c>(item, tEnter)</c> sorted by <c>tEnter</c> then original input index.</summary>
    let raycast (origin: Point3) (direction: Point3) (maxT: float) (tree: RTree3D<'T>) : ('T * float) list =
        Check.finite "originX" origin.X
        Check.finite "originY" origin.Y
        Check.finite "originZ" origin.Z
        Check.finite "directionX" direction.X
        Check.finite "directionY" direction.Y
        Check.finite "directionZ" direction.Z
        if direction.X = 0. && direction.Y = 0. && direction.Z = 0. then
            invalidArg "direction" "Ray direction must be nonzero."
        if Double.IsNaN maxT || maxT < 0. then
            invalidArg "maxT" "Maximum ray parameter must be nonnegative."
        if tree.Orig3Items.Length = 0 then
            []
        else
            let ox = origin.X
            let oy = origin.Y
            let oz = origin.Z
            let dx = direction.X
            let dy = direction.Y
            let dz = direction.Z
            let invDx = if dx <> 0. then 1. / dx else 0.
            let invDy = if dy <> 0. then 1. / dy else 0.
            let invDz = if dz <> 0. then 1. / dz else 0.
            let fastInv =
                dx <> 0. && dy <> 0. && dz <> 0.
                && Double.IsFinite invDx && Double.IsFinite invDy && Double.IsFinite invDz
            let minXs = tree.Box3MinX
            let minYs = tree.Box3MinY
            let minZs = tree.Box3MinZ
            let maxXs = tree.Box3MaxX
            let maxYs = tree.Box3MaxY
            let maxZs = tree.Box3MaxZ
            let indices = tree.Node3Indices
            let levelBounds = tree.Level3Bounds
            let topLevel = levelBounds.Length - 1
            let rootIdx = levelBounds[topLevel] - 1
            let struct(rootHit, _) =
                RTreeInternal.slabIntersect3D
                    ox oy oz dx dy dz invDx invDy invDz fastInv maxT
                    minXs[rootIdx] minYs[rootIdx] minZs[rootIdx]
                    maxXs[rootIdx] maxYs[rootIdx] maxZs[rootIdx]
            if not rootHit then
                []
            else
                let hits = ResizeArray<struct(float * int)>()
                let stack = ResizeArray<struct(int * int)>(32)
                stack.Add(struct(rootIdx, topLevel))
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(nodeIdx, level) = stack[last]
                    stack.RemoveAt last
                    let childLevel = level - 1
                    let cStart = indices[nodeIdx]
                    let cEnd = min (cStart + RTreeInternal.NodeDegree) levelBounds[childLevel]
                    for i in cStart .. cEnd - 1 do
                        let struct(isHit, tEnter) =
                            RTreeInternal.slabIntersect3D
                                ox oy oz dx dy dz invDx invDy invDz fastInv maxT
                                minXs[i] minYs[i] minZs[i]
                                maxXs[i] maxYs[i] maxZs[i]
                        if isHit then
                            if childLevel = 0 then
                                hits.Add(struct(tEnter, indices[i]))
                            else
                                stack.Add(struct(i, childLevel))
                RTreeInternal.hitsToSortedList tree.Orig3Items hits

    /// <summary>Performs a synchronized hierarchical 3D spatial join, returning all intersecting <c>(left, right)</c> item pairs in deterministic input-index order.</summary>
    let spatialJoin (left: RTree3D<'A>) (right: RTree3D<'B>) : ('A * 'B) list =
        if left.Orig3Items.Length = 0 || right.Orig3Items.Length = 0 then
            []
        else
            let lMinX = left.Box3MinX
            let lMinY = left.Box3MinY
            let lMinZ = left.Box3MinZ
            let lMaxX = left.Box3MaxX
            let lMaxY = left.Box3MaxY
            let lMaxZ = left.Box3MaxZ
            let lIndices = left.Node3Indices
            let lBounds = left.Level3Bounds
            let rMinX = right.Box3MinX
            let rMinY = right.Box3MinY
            let rMinZ = right.Box3MinZ
            let rMaxX = right.Box3MaxX
            let rMaxY = right.Box3MaxY
            let rMaxZ = right.Box3MaxZ
            let rIndices = right.Node3Indices
            let rBounds = right.Level3Bounds
            let topA = lBounds.Length - 1
            let topB = rBounds.Length - 1
            let rootA = lBounds[topA] - 1
            let rootB = rBounds[topB] - 1
            let inline overlaps ia ib =
                lMaxX[ia] >= rMinX[ib]
                && lMinX[ia] <= rMaxX[ib]
                && lMaxY[ia] >= rMinY[ib]
                && lMinY[ia] <= rMaxY[ib]
                && lMaxZ[ia] >= rMinZ[ib]
                && lMinZ[ia] <= rMaxZ[ib]
            if not (overlaps rootA rootB) then
                []
            else
                let pairs = ResizeArray<struct(int * int)>()
                let stack = ResizeArray<struct(int * int * int * int)>(64)
                stack.Add(struct(rootA, topA, rootB, topB))
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(na, la, nb, lb) = stack[last]
                    stack.RemoveAt last
                    if la > lb then
                        let childLa = la - 1
                        let aStart = lIndices[na]
                        let aEnd = min (aStart + RTreeInternal.NodeDegree) lBounds[childLa]
                        for ca in aStart .. aEnd - 1 do
                            if overlaps ca nb then
                                stack.Add(struct(ca, childLa, nb, lb))
                    elif lb > la then
                        let childLb = lb - 1
                        let bStart = rIndices[nb]
                        let bEnd = min (bStart + RTreeInternal.NodeDegree) rBounds[childLb]
                        for cb in bStart .. bEnd - 1 do
                            if overlaps na cb then
                                stack.Add(struct(na, la, cb, childLb))
                    else
                        let childLvl = la - 1
                        let aStart = lIndices[na]
                        let aEnd = min (aStart + RTreeInternal.NodeDegree) lBounds[childLvl]
                        let bStart = rIndices[nb]
                        let bEnd = min (bStart + RTreeInternal.NodeDegree) rBounds[childLvl]
                        for ca in aStart .. aEnd - 1 do
                            if overlaps ca nb then
                                for cb in bStart .. bEnd - 1 do
                                    if overlaps ca cb then
                                        if childLvl = 0 then
                                            pairs.Add(struct(lIndices[ca], rIndices[cb]))
                                        else
                                            stack.Add(struct(ca, childLvl, cb, childLvl))
                let countPairs = pairs.Count
                if countPairs = 0 then
                    []
                else
                    let arr = pairs.ToArray()
                    Array.sortInPlaceWith
                        (fun struct(a1, b1) struct(a2, b2) ->
                            let c = compare a1 a2
                            if c <> 0 then c else compare b1 b2)
                        arr
                    let lItems = left.Orig3Items
                    let rItems = right.Orig3Items
                    let mutable acc = []
                    let mutable i = countPairs - 1
                    while i >= 0 do
                        let struct(ia, ib) = arr[i]
                        acc <- (lItems[ia], rItems[ib]) :: acc
                        i <- i - 1
                    acc

/// <summary>An immutable Karras (2012) linear 3D bounding volume hierarchy over <c>Bounds3</c>.</summary>
type Bvh3D<'T> =
    private
        { BvhMinX: float array
          BvhMinY: float array
          BvhMinZ: float array
          BvhMaxX: float array
          BvhMaxY: float array
          BvhMaxZ: float array
          BvhLeft: int array
          BvhRight: int array
          BvhOrigIdx: int array
          BvhItems: 'T array
          BvhEnvelope: Bounds3 option }

/// <summary>Constructs and queries O(N) Morton-radix Karras (2012) 3D linear BVHs with tight bottom-up AABBs and ray traversal.</summary>
[<RequireQualifiedAccess>]
module Bvh3D =
    /// <summary>Builds a Karras (2012) 30-bit Morton binary radix BVH in O(N) time via 3-pass 10-bit LSD radix sort and bottom-up tight <c>Bounds3</c> propagation.</summary>
    let build (project: 'T -> Bounds3) (items: seq<'T>) : Bvh3D<'T> =
        let origItems = RTreeInternal.captureArray items
        let n = origItems.Length
        if n = 0 then
            { BvhMinX = [||]
              BvhMinY = [||]
              BvhMinZ = [||]
              BvhMaxX = [||]
              BvhMaxY = [||]
              BvhMaxZ = [||]
              BvhLeft = [||]
              BvhRight = [||]
              BvhOrigIdx = [||]
              BvhItems = [||]
              BvhEnvelope = None }
        elif n = 1 then
            let b = project origItems[0]
            { BvhMinX = [| Bounds3.minX b |]
              BvhMinY = [| Bounds3.minY b |]
              BvhMinZ = [| Bounds3.minZ b |]
              BvhMaxX = [| Bounds3.maxX b |]
              BvhMaxY = [| Bounds3.maxY b |]
              BvhMaxZ = [| Bounds3.maxZ b |]
              BvhLeft = [| -1 |]
              BvhRight = [| -1 |]
              BvhOrigIdx = [| 0 |]
              BvhItems = origItems
              BvhEnvelope = Some b }
        else
            let rawMinX = Array.zeroCreate<float> n
            let rawMinY = Array.zeroCreate<float> n
            let rawMinZ = Array.zeroCreate<float> n
            let rawMaxX = Array.zeroCreate<float> n
            let rawMaxY = Array.zeroCreate<float> n
            let rawMaxZ = Array.zeroCreate<float> n
            let mutable envMinX = Double.PositiveInfinity
            let mutable envMinY = Double.PositiveInfinity
            let mutable envMinZ = Double.PositiveInfinity
            let mutable envMaxX = Double.NegativeInfinity
            let mutable envMaxY = Double.NegativeInfinity
            let mutable envMaxZ = Double.NegativeInfinity
            for i in 0 .. n - 1 do
                let b = project origItems[i]
                let x0 = Bounds3.minX b
                let y0 = Bounds3.minY b
                let z0 = Bounds3.minZ b
                let x1 = Bounds3.maxX b
                let y1 = Bounds3.maxY b
                let z1 = Bounds3.maxZ b
                rawMinX[i] <- x0
                rawMinY[i] <- y0
                rawMinZ[i] <- z0
                rawMaxX[i] <- x1
                rawMaxY[i] <- y1
                rawMaxZ[i] <- z1
                if x0 < envMinX then envMinX <- x0
                if y0 < envMinY then envMinY <- y0
                if z0 < envMinZ then envMinZ <- z0
                if x1 > envMaxX then envMaxX <- x1
                if y1 > envMaxY then envMaxY <- y1
                if z1 > envMaxZ then envMaxZ <- z1
            let envelope =
                Bounds3.create
                    envMinX
                    envMinY
                    envMinZ
                    (envMaxX - envMinX)
                    (envMaxY - envMinY)
                    (envMaxZ - envMinZ)
            let codes = Array.zeroCreate<uint32> n
            for i in 0 .. n - 1 do
                let cx = rawMinX[i] + (rawMaxX[i] - rawMinX[i]) * 0.5
                let cy = rawMinY[i] + (rawMaxY[i] - rawMinY[i]) * 0.5
                let cz = rawMinZ[i] + (rawMaxZ[i] - rawMinZ[i]) * 0.5
                let qx = RTreeInternal.quantizeNorm10 (RTreeInternal.normalizeCoord envMinX envMaxX cx)
                let qy = RTreeInternal.quantizeNorm10 (RTreeInternal.normalizeCoord envMinY envMaxY cy)
                let qz = RTreeInternal.quantizeNorm10 (RTreeInternal.normalizeCoord envMinZ envMaxZ cz)
                codes[i] <- SpaceFillingBits.morton3DEncodeRaw (uint32 qx) (uint32 qy) (uint32 qz)
            // 3-pass 10-bit LSD stable radix sort over 30-bit Morton codes
            let mutable srcPerm = Array.init n id
            let mutable dstPerm = Array.zeroCreate<int> n
            let counts = Array.zeroCreate<int> 1024
            for pass in 0 .. 2 do
                let shift = pass * 10
                Array.fill counts 0 1024 0
                for k in 0 .. n - 1 do
                    let orig = srcPerm[k]
                    let bucket = int ((codes[orig] >>> shift) &&& 0x3FFu)
                    counts[bucket] <- counts[bucket] + 1
                let mutable prefix = 0
                for b in 0 .. 1023 do
                    let c = counts[b]
                    counts[b] <- prefix
                    prefix <- prefix + c
                for k in 0 .. n - 1 do
                    let orig = srcPerm[k]
                    let bucket = int ((codes[orig] >>> shift) &&& 0x3FFu)
                    let pos = counts[bucket]
                    dstPerm[pos] <- orig
                    counts[bucket] <- pos + 1
                let tmp = srcPerm
                srcPerm <- dstPerm
                dstPerm <- tmp
            let sortedCodes = Array.zeroCreate<uint32> n
            for m in 0 .. n - 1 do
                sortedCodes[m] <- codes[srcPerm[m]]
            let totalNodes = 2 * n - 1
            let leafBase = n - 1
            let bMinX = Array.zeroCreate<float> totalNodes
            let bMinY = Array.zeroCreate<float> totalNodes
            let bMinZ = Array.zeroCreate<float> totalNodes
            let bMaxX = Array.zeroCreate<float> totalNodes
            let bMaxY = Array.zeroCreate<float> totalNodes
            let bMaxZ = Array.zeroCreate<float> totalNodes
            let bLeft = Array.zeroCreate<int> totalNodes
            let bRight = Array.zeroCreate<int> totalNodes
            let bOrigIdx = Array.zeroCreate<int> totalNodes
            let parents = Array.zeroCreate<int> totalNodes
            parents[0] <- -1
            for m in 0 .. n - 1 do
                let leafNode = leafBase + m
                let orig = srcPerm[m]
                bMinX[leafNode] <- rawMinX[orig]
                bMinY[leafNode] <- rawMinY[orig]
                bMinZ[leafNode] <- rawMinZ[orig]
                bMaxX[leafNode] <- rawMaxX[orig]
                bMaxY[leafNode] <- rawMaxY[orig]
                bMaxZ[leafNode] <- rawMaxZ[orig]
                bLeft[leafNode] <- -1
                bRight[leafNode] <- -1
                bOrigIdx[leafNode] <- orig
            let inline delta i j =
                if j < 0 || j >= n then
                    -1
                else
                    let ki = sortedCodes[i]
                    let kj = sortedCodes[j]
                    if ki <> kj then
                        RTreeInternal.clz32 (ki ^^^ kj)
                    else
                        32 + RTreeInternal.clz32 (uint32 i ^^^ uint32 j)
            for i in 0 .. n - 2 do
                bOrigIdx[i] <- -1
                let dir = if delta i (i + 1) - delta i (i - 1) >= 0 then 1 else -1
                let deltaMin = delta i (i - dir)
                let mutable lMax = 2
                while delta i (i + lMax * dir) > deltaMin do
                    lMax <- lMax <<< 1
                let mutable len = 0
                let mutable step = lMax >>> 1
                while step >= 1 do
                    if delta i (i + (len + step) * dir) > deltaMin then
                        len <- len + step
                    step <- step >>> 1
                let j = i + len * dir
                let first = if i <= j then i else j
                let last = if i <= j then j else i
                let commonPrefix = delta first last
                let mutable gamma = first
                let mutable splitStep = last - first
                let mutable searchingSplit = true
                while searchingSplit do
                    splitStep <- (splitStep + 1) >>> 1
                    let candidate = gamma + splitStep
                    if candidate < last then
                        if delta first candidate > commonPrefix then
                            gamma <- candidate
                    if splitStep <= 1 then
                        searchingSplit <- false
                let leftChild = if first = gamma then leafBase + gamma else gamma
                let rightChild = if last = gamma + 1 then leafBase + gamma + 1 else gamma + 1
                bLeft[i] <- leftChild
                bRight[i] <- rightChild
                parents[leftChild] <- i
                parents[rightChild] <- i
            // Bottom-up tight Bounds3 propagation (Karras 2012 Section 5)
            let visited = Array.zeroCreate<int> (n - 1)
            for m in 0 .. n - 1 do
                let mutable cur = parents[leafBase + m]
                while cur >= 0 do
                    if visited[cur] = 0 then
                        visited[cur] <- 1
                        cur <- -1
                    else
                        let lc = bLeft[cur]
                        let rc = bRight[cur]
                        bMinX[cur] <- min bMinX[lc] bMinX[rc]
                        bMinY[cur] <- min bMinY[lc] bMinY[rc]
                        bMinZ[cur] <- min bMinZ[lc] bMinZ[rc]
                        bMaxX[cur] <- max bMaxX[lc] bMaxX[rc]
                        bMaxY[cur] <- max bMaxY[lc] bMaxY[rc]
                        bMaxZ[cur] <- max bMaxZ[lc] bMaxZ[rc]
                        cur <- parents[cur]
            { BvhMinX = bMinX
              BvhMinY = bMinY
              BvhMinZ = bMinZ
              BvhMaxX = bMaxX
              BvhMaxY = bMaxY
              BvhMaxZ = bMaxZ
              BvhLeft = bLeft
              BvhRight = bRight
              BvhOrigIdx = bOrigIdx
              BvhItems = origItems
              BvhEnvelope = Some envelope }

    /// <summary>Returns the number of indexed primitives.</summary>
    let count (bvh: Bvh3D<'T>) : int =
        bvh.BvhItems.Length

    /// <summary>Returns the enclosing 3D cuboid of all indexed primitives, or <c>None</c> when empty.</summary>
    let bounds (bvh: Bvh3D<'T>) : Bounds3 option =
        bvh.BvhEnvelope

    /// <summary>Returns all primitives whose <c>Bounds3</c> intersect <c>query</c>, ordered by original input index.</summary>
    let search (query: Bounds3) (bvh: Bvh3D<'T>) : 'T list =
        if bvh.BvhItems.Length = 0 then
            []
        else
            let qx0 = Bounds3.minX query
            let qy0 = Bounds3.minY query
            let qz0 = Bounds3.minZ query
            let qx1 = Bounds3.maxX query
            let qy1 = Bounds3.maxY query
            let qz1 = Bounds3.maxZ query
            let minXs = bvh.BvhMinX
            let minYs = bvh.BvhMinY
            let minZs = bvh.BvhMinZ
            let maxXs = bvh.BvhMaxX
            let maxYs = bvh.BvhMaxY
            let maxZs = bvh.BvhMaxZ
            let lefts = bvh.BvhLeft
            let rights = bvh.BvhRight
            let origIndices = bvh.BvhOrigIdx
            if
                maxXs[0] < qx0 || minXs[0] > qx1
                || maxYs[0] < qy0 || minYs[0] > qy1
                || maxZs[0] < qz0 || minZs[0] > qz1
            then
                []
            else
                let results = ResizeArray<int>()
                let stack = ResizeArray<int>(64)
                stack.Add 0
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let node = stack[last]
                    stack.RemoveAt last
                    let lc = lefts[node]
                    if lc < 0 then
                        results.Add origIndices[node]
                    else
                        let rc = rights[node]
                        if
                            maxXs[rc] >= qx0 && minXs[rc] <= qx1
                            && maxYs[rc] >= qy0 && minYs[rc] <= qy1
                            && maxZs[rc] >= qz0 && minZs[rc] <= qz1
                        then
                            stack.Add rc
                        if
                            maxXs[lc] >= qx0 && minXs[lc] <= qx1
                            && maxYs[lc] >= qy0 && minYs[lc] <= qy1
                            && maxZs[lc] >= qz0 && minZs[lc] <= qz1
                        then
                            stack.Add lc
                RTreeInternal.indicesToSortedItems bvh.BvhItems results

    /// <summary>Intersects ray <c>origin + t * direction</c> for <c>t</c> in <c>[0, maxT]</c> against all primitives, returning <c>(item, tEnter)</c> sorted by <c>tEnter</c> then original input index.</summary>
    let raycast (origin: Point3) (direction: Point3) (maxT: float) (bvh: Bvh3D<'T>) : ('T * float) list =
        Check.finite "originX" origin.X
        Check.finite "originY" origin.Y
        Check.finite "originZ" origin.Z
        Check.finite "directionX" direction.X
        Check.finite "directionY" direction.Y
        Check.finite "directionZ" direction.Z
        if direction.X = 0. && direction.Y = 0. && direction.Z = 0. then
            invalidArg "direction" "Ray direction must be nonzero."
        if Double.IsNaN maxT || maxT < 0. then
            invalidArg "maxT" "Maximum ray parameter must be nonnegative."
        if bvh.BvhItems.Length = 0 then
            []
        else
            let ox = origin.X
            let oy = origin.Y
            let oz = origin.Z
            let dx = direction.X
            let dy = direction.Y
            let dz = direction.Z
            let invDx = if dx <> 0. then 1. / dx else 0.
            let invDy = if dy <> 0. then 1. / dy else 0.
            let invDz = if dz <> 0. then 1. / dz else 0.
            let fastInv =
                dx <> 0. && dy <> 0. && dz <> 0.
                && Double.IsFinite invDx && Double.IsFinite invDy && Double.IsFinite invDz
            let minXs = bvh.BvhMinX
            let minYs = bvh.BvhMinY
            let minZs = bvh.BvhMinZ
            let maxXs = bvh.BvhMaxX
            let maxYs = bvh.BvhMaxY
            let maxZs = bvh.BvhMaxZ
            let lefts = bvh.BvhLeft
            let rights = bvh.BvhRight
            let origIndices = bvh.BvhOrigIdx
            let struct(rootHit, rootT) =
                RTreeInternal.slabIntersect3D
                    ox oy oz dx dy dz invDx invDy invDz fastInv maxT
                    minXs[0] minYs[0] minZs[0]
                    maxXs[0] maxYs[0] maxZs[0]
            if not rootHit then
                []
            elif lefts[0] < 0 then
                [ bvh.BvhItems[origIndices[0]], rootT ]
            else
                let hits = ResizeArray<struct(float * int)>()
                let stack = ResizeArray<int>(64)
                stack.Add 0
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let node = stack[last]
                    stack.RemoveAt last
                    let lc = lefts[node]
                    let rc = rights[node]
                    let struct(hitL, tL) =
                        RTreeInternal.slabIntersect3D
                            ox oy oz dx dy dz invDx invDy invDz fastInv maxT
                            minXs[lc] minYs[lc] minZs[lc]
                            maxXs[lc] maxYs[lc] maxZs[lc]
                    if hitL then
                        if lefts[lc] < 0 then
                            hits.Add(struct(tL, origIndices[lc]))
                        else
                            stack.Add lc
                    let struct(hitR, tR) =
                        RTreeInternal.slabIntersect3D
                            ox oy oz dx dy dz invDx invDy invDz fastInv maxT
                            minXs[rc] minYs[rc] minZs[rc]
                            maxXs[rc] maxYs[rc] maxZs[rc]
                    if hitR then
                        if lefts[rc] < 0 then
                            hits.Add(struct(tR, origIndices[rc]))
                        else
                            stack.Add rc
                RTreeInternal.hitsToSortedList bvh.BvhItems hits

    /// <summary>Finds the closest ray-primitive hit in <c>[0, maxT]</c> using shortened-ray front-to-back traversal, breaking <c>tEnter</c> ties by original input index.</summary>
    let raycastClosest (origin: Point3) (direction: Point3) (maxT: float) (bvh: Bvh3D<'T>) : ('T * float) option =
        Check.finite "originX" origin.X
        Check.finite "originY" origin.Y
        Check.finite "originZ" origin.Z
        Check.finite "directionX" direction.X
        Check.finite "directionY" direction.Y
        Check.finite "directionZ" direction.Z
        if direction.X = 0. && direction.Y = 0. && direction.Z = 0. then
            invalidArg "direction" "Ray direction must be nonzero."
        if Double.IsNaN maxT || maxT < 0. then
            invalidArg "maxT" "Maximum ray parameter must be nonnegative."
        if bvh.BvhItems.Length = 0 then
            None
        else
            let ox = origin.X
            let oy = origin.Y
            let oz = origin.Z
            let dx = direction.X
            let dy = direction.Y
            let dz = direction.Z
            let invDx = if dx <> 0. then 1. / dx else 0.
            let invDy = if dy <> 0. then 1. / dy else 0.
            let invDz = if dz <> 0. then 1. / dz else 0.
            let fastInv =
                dx <> 0. && dy <> 0. && dz <> 0.
                && Double.IsFinite invDx && Double.IsFinite invDy && Double.IsFinite invDz
            let minXs = bvh.BvhMinX
            let minYs = bvh.BvhMinY
            let minZs = bvh.BvhMinZ
            let maxXs = bvh.BvhMaxX
            let maxYs = bvh.BvhMaxY
            let maxZs = bvh.BvhMaxZ
            let lefts = bvh.BvhLeft
            let rights = bvh.BvhRight
            let origIndices = bvh.BvhOrigIdx
            let struct(rootHit, rootT) =
                RTreeInternal.slabIntersect3D
                    ox oy oz dx dy dz invDx invDy invDz fastInv maxT
                    minXs[0] minYs[0] minZs[0]
                    maxXs[0] maxYs[0] maxZs[0]
            if not rootHit then
                None
            elif lefts[0] < 0 then
                Some(bvh.BvhItems[origIndices[0]], rootT)
            else
                let mutable bestT = maxT
                let mutable bestOrig = Int32.MaxValue
                let mutable found = false
                let stack = ResizeArray<struct(int * float)>(64)
                stack.Add(struct(0, rootT))
                while stack.Count > 0 do
                    let last = stack.Count - 1
                    let struct(node, nodeT) = stack[last]
                    stack.RemoveAt last
                    if nodeT <= bestT then
                        let lc = lefts[node]
                        let rc = rights[node]
                        let struct(hitL, tL) =
                            RTreeInternal.slabIntersect3D
                                ox oy oz dx dy dz invDx invDy invDz fastInv bestT
                                minXs[lc] minYs[lc] minZs[lc]
                                maxXs[lc] maxYs[lc] maxZs[lc]
                        let struct(hitR, tR) =
                            RTreeInternal.slabIntersect3D
                                ox oy oz dx dy dz invDx invDy invDz fastInv bestT
                                minXs[rc] minYs[rc] minZs[rc]
                                maxXs[rc] maxYs[rc] maxZs[rc]
                        let mutable pushL = false
                        let mutable pushR = false
                        if hitL then
                            if lefts[lc] < 0 then
                                let orig = origIndices[lc]
                                if not found || tL < bestT || (tL = bestT && orig < bestOrig) then
                                    found <- true
                                    bestT <- tL
                                    bestOrig <- orig
                            else
                                pushL <- true
                        if hitR then
                            if lefts[rc] < 0 then
                                let orig = origIndices[rc]
                                if not found || tR < bestT || (tR = bestT && orig < bestOrig) then
                                    found <- true
                                    bestT <- tR
                                    bestOrig <- orig
                            else
                                pushR <- true
                        if pushL && pushR then
                            if tL <= tR then
                                if tR <= bestT then stack.Add(struct(rc, tR))
                                if tL <= bestT then stack.Add(struct(lc, tL))
                            else
                                if tL <= bestT then stack.Add(struct(lc, tL))
                                if tR <= bestT then stack.Add(struct(rc, tR))
                        elif pushL && tL <= bestT then
                            stack.Add(struct(lc, tL))
                        elif pushR && tR <= bestT then
                            stack.Add(struct(rc, tR))
                if found then Some(bvh.BvhItems[bestOrig], bestT) else None

    /// <summary>Packs point items as centered <c>2 * halfExtent</c> cubes into a <c>Bvh3D</c>; throws for negative or non-finite <c>halfExtent</c>.</summary>
    let buildPoints (halfExtent: float) (project: 'T -> Point3) (items: seq<'T>) : Bvh3D<'T> =
        if isNull (box project) then nullArg (nameof project)
        if isNull (box items) then nullArg (nameof items)
        Check.finite "halfExtent" halfExtent
        if halfExtent < 0. then
            invalidArg "halfExtent" "Half-extent must be nonnegative."
        let fullExtent = 2. * halfExtent
        Check.finite "halfExtent" fullExtent
        build
            (fun item ->
                let pt = project item
                Bounds3.create (pt.X - halfExtent) (pt.Y - halfExtent) (pt.Z - halfExtent) fullExtent fullExtent fullExtent)
            items

    /// <summary>Normalizes <c>direction</c> before running <c>raycastClosest</c> so returned <c>t</c> is always true Euclidean world distance in <c>[0, maxDistance]</c>.</summary>
    let raycastNormalized (origin: Point3) (direction: Point3) (maxDistance: float) (bvh: Bvh3D<'T>) : ('T * float) option =
        Check.finite "directionX" direction.X
        Check.finite "directionY" direction.Y
        Check.finite "directionZ" direction.Z
        let ax, ay, az = abs direction.X, abs direction.Y, abs direction.Z
        let m = max ax (max ay az)
        if m = 0. then
            invalidArg "direction" "Ray direction must be nonzero."
        let rx, ry, rz = direction.X / m, direction.Y / m, direction.Z / m
        let inv = 1. / sqrt (rx * rx + ry * ry + rz * rz)
        let unitDir = Point3(rx * inv, ry * inv, rz * inv)
        raycastClosest origin unitDir maxDistance bvh

[<Struct>]
type private FlatSpatialNode3D<'T> =
    { Px: float
      Py: float
      Pz: float
      Item: 'T
      Order: int
      mutable LeftChild: int
      mutable RightChild: int
      mutable Axis: byte }

/// <summary>An immutable balanced 3D point k-d tree with original-order tie breaking.</summary>
type SpatialIndex3D<'T> =
    private
        { Nodes3D: FlatSpatialNode3D<'T> array
          Root3D: int
          MaxCoordAbs3D: float }

/// <summary>Builds and queries balanced 3D k-d trees over <c>Point3</c>; non-finite coordinates throw <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module Spatial3D =
    let private axisComparer<'T> (axis: byte) =
        { new IComparer<FlatSpatialNode3D<'T>> with
            member _.Compare(a, b) =
                let c =
                    match axis with
                    | 0uy -> a.Px.CompareTo b.Px
                    | 1uy -> a.Py.CompareTo b.Py
                    | _ -> a.Pz.CompareTo b.Pz
                if c <> 0 then c else a.Order.CompareTo b.Order }

    /// <summary>Builds a balanced 3D k-d tree in O(n log^2 n) time; all coordinates must be finite.</summary>
    let create (project: 'T -> Point3) (items: seq<'T>) : SpatialIndex3D<'T> =
        let mutable maxAbs = 0.
        let inline makeNode idx item =
            let pt = project item
            Check.finite "x" pt.X
            Check.finite "y" pt.Y
            Check.finite "z" pt.Z
            let ax = abs pt.X
            let ay = abs pt.Y
            let az = abs pt.Z
            if ax > maxAbs then maxAbs <- ax
            if ay > maxAbs then maxAbs <- ay
            if az > maxAbs then maxAbs <- az
            { Px = pt.X
              Py = pt.Y
              Pz = pt.Z
              Item = item
              Order = idx
              LeftChild = -1
              RightChild = -1
              Axis = 0uy }
        let nodes =
            match box items with
            | :? ('T array) as arr ->
                let res = Array.zeroCreate<FlatSpatialNode3D<'T>> arr.Length
                for i in 0 .. arr.Length - 1 do
                    res[i] <- makeNode i arr[i]
                res
            | :? ('T list) as lst ->
                let n = List.length lst
                let res = Array.zeroCreate<FlatSpatialNode3D<'T>> n
                let mutable i = 0
                for item in lst do
                    res[i] <- makeNode i item
                    i <- i + 1
                res
            | _ ->
                let buf = ResizeArray<FlatSpatialNode3D<'T>>()
                let mutable i = 0
                for item in items do
                    buf.Add(makeNode i item)
                    i <- i + 1
                buf.ToArray()
        if nodes.Length = 0 then
            { Nodes3D = nodes; Root3D = -1; MaxCoordAbs3D = 0. }
        else
#if !FABLE_COMPILER
            let cmpX = axisComparer<'T> 0uy
            let cmpY = axisComparer<'T> 1uy
            let cmpZ = axisComparer<'T> 2uy
            let rec buildRange (axis: byte) lo hi =
                if lo > hi then
                    -1
                elif lo = hi then
                    nodes[lo].Axis <- axis
                    nodes[lo].LeftChild <- -1
                    nodes[lo].RightChild <- -1
                    lo
                else
                    let cmp =
                        match axis with
                        | 0uy -> cmpX
                        | 1uy -> cmpY
                        | _ -> cmpZ
                    Array.Sort(nodes, lo, hi - lo + 1, cmp)
                    let mid = lo + (hi - lo) / 2
                    let nextAxis = if axis = 2uy then 0uy else axis + 1uy
                    nodes[mid].Axis <- axis
                    nodes[mid].LeftChild <- buildRange nextAxis lo (mid - 1)
                    nodes[mid].RightChild <- buildRange nextAxis (mid + 1) hi
                    mid
            let root = buildRange 0uy 0 (nodes.Length - 1)
            { Nodes3D = nodes; Root3D = root; MaxCoordAbs3D = maxAbs }
#else
            let outNodes = Array.zeroCreate<FlatSpatialNode3D<'T>> nodes.Length
            let mutable writeIdx = 0
            let rec buildFable (axis: byte) (slice: FlatSpatialNode3D<'T> array) =
                if slice.Length = 0 then
                    -1
                else
                    let sorted =
                        slice
                        |> Array.sortBy (fun nd ->
                            let coord =
                                match axis with
                                | 0uy -> nd.Px
                                | 1uy -> nd.Py
                                | _ -> nd.Pz
                            coord, nd.Order)
                    let mid = sorted.Length / 2
                    let curIdx = writeIdx
                    writeIdx <- writeIdx + 1
                    let nextAxis = if axis = 2uy then 0uy else axis + 1uy
                    let mutable rootNd = sorted[mid]
                    rootNd.Axis <- axis
                    rootNd.LeftChild <- buildFable nextAxis sorted[.. mid - 1]
                    rootNd.RightChild <- buildFable nextAxis sorted[mid + 1 ..]
                    outNodes[curIdx] <- rootNd
                    curIdx
            let root = buildFable 0uy nodes
            { Nodes3D = outNodes; Root3D = root; MaxCoordAbs3D = maxAbs }
#endif

    /// <summary>Returns the number of indexed points.</summary>
    let count (index: SpatialIndex3D<'T>) : int =
        index.Nodes3D.Length

    /// <summary>Returns items inside inclusive 3D <c>bounds</c> in original input order.</summary>
    let within (bounds: Bounds3) (index: SpatialIndex3D<'T>) : 'T list =
        if index.Root3D < 0 then
            []
        else
            let nodes = index.Nodes3D
            let bx0 = Bounds3.minX bounds
            let by0 = Bounds3.minY bounds
            let bz0 = Bounds3.minZ bounds
            let bx1 = Bounds3.maxX bounds
            let by1 = Bounds3.maxY bounds
            let bz1 = Bounds3.maxZ bounds
            let results = ResizeArray<struct(int * 'T)>()
            let rec search idx =
                if idx >= 0 then
                    let nd = nodes[idx]
                    if
                        nd.Px >= bx0 && nd.Px <= bx1
                        && nd.Py >= by0 && nd.Py <= by1
                        && nd.Pz >= bz0 && nd.Pz <= bz1
                    then
                        results.Add(struct(nd.Order, nd.Item))
                    let low, high, value =
                        match nd.Axis with
                        | 0uy -> bx0, bx1, nd.Px
                        | 1uy -> by0, by1, nd.Py
                        | _ -> bz0, bz1, nd.Pz
                    if low <= value then search nd.LeftChild
                    if high >= value then search nd.RightChild
            search index.Root3D
            let n = results.Count
            if n = 0 then
                []
            else
                let arr = results.ToArray()
                Array.sortInPlaceWith (fun struct(o1, _) struct(o2, _) -> compare o1 o2) arr
                let mutable acc = []
                let mutable i = n - 1
                while i >= 0 do
                    let struct(_, item) = arr[i]
                    acc <- item :: acc
                    i <- i - 1
                acc

    /// <summary>Returns items within inclusive Euclidean <c>radius</c> of <c>center</c> in original input order.</summary>
    let withinRadius (center: Point3) (radius: float) (index: SpatialIndex3D<'T>) : 'T list =
        Check.finite "x" center.X
        Check.finite "y" center.Y
        Check.finite "z" center.Z
        if Double.IsNaN radius || radius < 0. then
            invalidArg "radius" "Radius must be nonnegative."
        if index.Root3D < 0 then
            []
        else
            let nodes = index.Nodes3D
            let cx = center.X
            let cy = center.Y
            let cz = center.Z
            let results = ResizeArray<struct(int * 'T)>()
            if Double.IsPositiveInfinity radius then
                for i in 0 .. nodes.Length - 1 do
                    let nd = nodes[i]
                    results.Add(struct(nd.Order, nd.Item))
            else
                let maxQueryAbs = max (abs cx) (max (abs cy) (abs cz))
                let fastSq = index.MaxCoordAbs3D < 1e140 && maxQueryAbs < 1e140 && radius < 1e140
                let sqLimit = if fastSq then radius * radius * (1. + 1e-12) + 1e-300 else 0.
                let sqInner = if fastSq && radius > 1e-140 then radius * radius * (1. - 1e-12) else -1.
                let rec search idx =
                    if idx >= 0 then
                        let nd = nodes[idx]
                        if fastSq then
                            let dx = abs (nd.Px - cx)
                            let dy = abs (nd.Py - cy)
                            let dz = abs (nd.Pz - cz)
                            let d2 = dx * dx + dy * dy + dz * dz
                            if d2 <= sqInner then
                                results.Add(struct(nd.Order, nd.Item))
                            elif d2 <= sqLimit && RTreeInternal.hypot3NonNeg dx dy dz <= radius then
                                results.Add(struct(nd.Order, nd.Item))
                        else
                            let pt = Point3(nd.Px, nd.Py, nd.Pz)
                            if Point3.distance pt center <= radius then
                                results.Add(struct(nd.Order, nd.Item))
                        let delta =
                            match nd.Axis with
                            | 0uy -> cx - nd.Px
                            | 1uy -> cy - nd.Py
                            | _ -> cz - nd.Pz
                        let nearIdx, farIdx =
                            if delta < 0. then nd.LeftChild, nd.RightChild
                            else nd.RightChild, nd.LeftChild
                        search nearIdx
                        if abs delta <= radius then
                            search farIdx
                search index.Root3D
            let n = results.Count
            if n = 0 then
                []
            else
                let arr = results.ToArray()
                Array.sortInPlaceWith (fun struct(o1, _) struct(o2, _) -> compare o1 o2) arr
                let mutable acc = []
                let mutable i = n - 1
                while i >= 0 do
                    let struct(_, item) = arr[i]
                    acc <- item :: acc
                    i <- i - 1
                acc

    /// <summary>Returns the nearest item to <c>point</c> within inclusive <c>maxDistance</c>, breaking distance ties by original input index.</summary>
    let nearest (point: Point3) (maxDistance: float) (index: SpatialIndex3D<'T>) : 'T option =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        Check.finite "z" point.Z
        if Double.IsNaN maxDistance || maxDistance < 0. then
            invalidArg "maxDistance" "Maximum distance must be nonnegative."
        if index.Root3D < 0 then
            None
        else
            let nodes = index.Nodes3D
            let qx = point.X
            let qy = point.Y
            let qz = point.Z
            let mutable bestDist = maxDistance
            let mutable found = false
            let mutable bestItem = Unchecked.defaultof<'T>
            let mutable bestOrder = Int32.MaxValue
            let maxQueryAbs = max (abs qx) (max (abs qy) (abs qz))
            if index.MaxCoordAbs3D < 1e140 && maxQueryAbs < 1e140 then
                let inline sqLimit d =
                    if Double.IsPositiveInfinity d || d > 1e140 then Double.PositiveInfinity
                    else d * d * (1. + 1e-12) + 1e-300
                let mutable distSqLimit = sqLimit bestDist
                let rec searchFast idx =
                    if idx >= 0 then
                        let nd = nodes[idx]
                        let dx = abs (nd.Px - qx)
                        let dy = abs (nd.Py - qy)
                        let dz = abs (nd.Pz - qz)
                        let d2 = dx * dx + dy * dy + dz * dz
                        if d2 <= distSqLimit then
                            let d = RTreeInternal.hypot3NonNeg dx dy dz
                            if d < bestDist || (d = bestDist && nd.Order < bestOrder) then
                                bestDist <- d
                                distSqLimit <- sqLimit d
                                found <- true
                                bestItem <- nd.Item
                                bestOrder <- nd.Order
                        let delta =
                            match nd.Axis with
                            | 0uy -> qx - nd.Px
                            | 1uy -> qy - nd.Py
                            | _ -> qz - nd.Pz
                        let nearIdx, farIdx =
                            if delta < 0. then nd.LeftChild, nd.RightChild
                            else nd.RightChild, nd.LeftChild
                        searchFast nearIdx
                        if abs delta <= bestDist then
                            searchFast farIdx
                searchFast index.Root3D
            else
                let rec searchExact idx =
                    if idx >= 0 then
                        let nd = nodes[idx]
                        let d = Point3.distance (Point3(nd.Px, nd.Py, nd.Pz)) point
                        if d < bestDist || (d = bestDist && nd.Order < bestOrder) then
                            bestDist <- d
                            found <- true
                            bestItem <- nd.Item
                            bestOrder <- nd.Order
                        let delta =
                            match nd.Axis with
                            | 0uy -> qx - nd.Px
                            | 1uy -> qy - nd.Py
                            | _ -> qz - nd.Pz
                        let nearIdx, farIdx =
                            if delta < 0. then nd.LeftChild, nd.RightChild
                            else nd.RightChild, nd.LeftChild
                        searchExact nearIdx
                        if abs delta <= bestDist then
                            searchExact farIdx
                searchExact index.Root3D
            if found then Some bestItem else None

    /// <summary>Returns up to <c>k</c> nearest items to <c>point</c> within inclusive <c>maxDistance</c>, ordered by distance then original input index.</summary>
    let kNearest (k: int) (point: Point3) (maxDistance: float) (index: SpatialIndex3D<'T>) : 'T list =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        Check.finite "z" point.Z
        if k < 0 then invalidArg "k" "Neighbor count k must be nonnegative."
        if Double.IsNaN maxDistance || maxDistance < 0. then
            invalidArg "maxDistance" "Maximum distance must be nonnegative."
        if k = 0 || index.Root3D < 0 then
            []
        elif k = 1 then
            match nearest point maxDistance index with
            | Some item -> [ item ]
            | None -> []
        else
            let nodes = index.Nodes3D
            let origItems = Array.zeroCreate<'T> nodes.Length
            for i in 0 .. nodes.Length - 1 do
                let nd = nodes[i]
                origItems[nd.Order] <- nd.Item
            let qx = point.X
            let qy = point.Y
            let qz = point.Z
            let topK = RTreeInternal.TopKCollector k
            let rec search idx =
                if idx >= 0 then
                    let nd = nodes[idx]
                    let d =
                        RTreeInternal.hypot3NonNeg
                            (abs (nd.Px - qx))
                            (abs (nd.Py - qy))
                            (abs (nd.Pz - qz))
                    if d <= maxDistance then
                        topK.Offer(d, nd.Order)
                    let delta =
                        match nd.Axis with
                        | 0uy -> qx - nd.Px
                        | 1uy -> qy - nd.Py
                        | _ -> qz - nd.Pz
                    let nearIdx, farIdx =
                        if delta < 0. then nd.LeftChild, nd.RightChild
                        else nd.RightChild, nd.LeftChild
                    search nearIdx
                    let bound = topK.PruneBound maxDistance
                    if abs delta <= bound then
                        search farIdx
            search index.Root3D
            topK.ToSortedItems origItems

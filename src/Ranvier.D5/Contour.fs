namespace Ranvier.D5

open System
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif

/// <summary>A region where values are at or above Threshold, in grid-bounds coordinates. Rings are closed by repeating the first point; Outer has
/// positive signed area and each hole negative.</summary>
type ContourPolygon = { Threshold:float; Outer:Point list; Holes:Point list list }
/// <summary>Contour settings: unique finite thresholds, linear interpolation of crossings (otherwise edge midpoints), and budgets for grid cells,
/// output vertices, polygons and stitching work.</summary>
type ContourConfig =
    { Thresholds:float list; Interpolate:bool; MaxCells:int; MaxVertices:int; MaxPolygons:int; MaxWork:int64 }

/// <summary>Default contour configuration.</summary>
[<RequireQualifiedAccess>]
module ContourConfig =
    /// <summary>Creates an interpolating configuration with budgets of 1,048,576 cells, 2,000,000 vertices, 250,000 polygons and 100,000,000 work
    /// units.</summary>
    let create thresholds =
        {Thresholds=thresholds;Interpolate=true;MaxCells=1048576;MaxVertices=2000000;MaxPolygons=250000;MaxWork=100000000L}

/// <summary>Marching-squares contour polygons over density grids.</summary>
[<RequireQualifiedAccess>]
module Contour =
    exception private ContourAbort of LayoutError

    let inline private packEdge (x: int) (y: int) (horizontal: bool) : int64 =
        ((int64 (x + 1)) <<< 32) ||| ((int64 (y + 1)) <<< 1) ||| (if horizontal then 1L else 0L)
    let inline private unpackX (edge: int64) : int = int (edge >>> 32) - 1
    let inline private unpackY (edge: int64) : int = int ((edge >>> 1) &&& 0x7FFFFFFFL) - 1
    let inline private unpackHoriz (edge: int64) : bool = (edge &&& 1L) <> 0L

    let private saddle a b c d threshold =
        let aa,bb,cc,dd=a-threshold,b-threshold,c-threshold,d-threshold
        let left,right=aa*cc,bb*dd
        let determinant=left-right
        let bound=(abs left+abs right)*1e-14+16.*System.Double.Epsilon
        if Double.IsFinite determinant && Double.IsFinite bound && abs determinant>bound then sign determinant
        else
            let values,_=ExactDyadic.integers [|a;b;c;d;threshold|]
            ((values[0]-values[4])*(values[2]-values[4])-(values[1]-values[4])*(values[3]-values[4])).Sign

    let private fraction a b threshold =
        let numerator,denominator=threshold-a,b-a
        if Double.IsFinite numerator && Double.IsFinite denominator then numerator/denominator
        else
            let values,_=ExactDyadic.integers [|a;b;threshold|]
            ExactDyadic.ratio (values[2]-values[0]) (values[1]-values[0]) 0

    let private signedArea (points:Point list) = Polygon.area points

    let private simplifySlice (token:System.Threading.CancellationToken) (path:ResizeArray<Point>) (startIdx:int) (count:int) (kept:ResizeArray<Point>) =
        kept.Clear()
        for k = 0 to count - 1 do
            if (k &&& 4095) = 0 then token.ThrowIfCancellationRequested()
            let previous = path[startIdx + (if k = 0 then count - 1 else k - 1)]
            let current = path[startIdx + k]
            let next = path[startIdx + (if k + 1 = count then 0 else k + 1)]
            let between =
                current.X >= min previous.X next.X && current.X <= max previous.X next.X &&
                current.Y >= min previous.Y next.Y && current.Y <= max previous.Y next.Y
            if not between || Predicates.orient2d previous current next <> 0 then kept.Add current
        if kept.Count < 3 then []
        else
            let mutable closed = [ kept[0] ]
            for k = kept.Count - 1 downto 0 do
                closed <- kept[k] :: closed
            closed

    let private ringBounds (ring: Point list) : struct(float * float * float * float) =
        let mutable minX = Double.PositiveInfinity
        let mutable minY = Double.PositiveInfinity
        let mutable maxX = Double.NegativeInfinity
        let mutable maxY = Double.NegativeInfinity
        for p in ring do
            if p.X < minX then minX <- p.X
            if p.Y < minY then minY <- p.Y
            if p.X > maxX then maxX <- p.X
            if p.Y > maxY then maxY <- p.Y
        struct(minX, minY, maxX, maxY)

    /// <summary>Traces regions at or above each threshold through cell centers, treating missing cells as below every threshold, and returns polygons
    /// by ascending threshold. Returns Error for invalid thresholds or budgets, exceeded budgets or degenerate topology; cancellation raises
    /// OperationCanceledException.</summary>
    let computeWithCancellation (token:System.Threading.CancellationToken) (config:ContourConfig) (grid:DensityGrid) : Result<ContourPolygon list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let columns,rows=DensityGrid.columns grid,DensityGrid.rows grid
        let cellWork=(int64 columns+1L)*(int64 rows+1L)
        if config.MaxCells<=0 || config.MaxVertices<=0 || config.MaxPolygons<=0 || config.MaxWork<=0L then
            Error [InvalidObservation "Contour budgets must be positive."]
        elif config.Thresholds |> List.exists(Double.IsFinite >> not) then
            Error [InvalidObservation "Contour thresholds must be finite."]
        elif Set.count(Set.ofList config.Thresholds)<>config.Thresholds.Length then
            Error [InvalidObservation "Contour thresholds must be unique."]
        elif int64 columns*int64 rows>int64 config.MaxCells || int64 config.Thresholds.Length>config.MaxWork/cellWork then
            Error [BudgetExceeded "Contour grid or level count exceeds the work budget."]
        else
          try
            let values,observed=DensityGrid.rawSamples grid
#if !FABLE_COMPILER
            let allObserved = Array.IndexOf(observed, false) < 0
#else
            let mutable allObserved = true
            let mutable obsIdx = 0
            while allObserved && obsIdx < observed.Length do
                if not observed[obsIdx] then allObserved <- false
                obsIdx <- obsIdx + 1
#endif
            let inline sample x y =
                if x<0 || x>=columns || y<0 || y>=rows then struct(0.,false)
                else
                    let index=y*columns+x
                    struct(values[index], allObserved || observed[index])
            let mutable work=cellWork*int64 config.Thresholds.Length
            let mutable vertices=0
            let charge amount =
                if amount>config.MaxWork-work then raise(ContourAbort(BudgetExceeded "Contour topology work exceeds its budget."))
                work<-work+amount
            let output=ResizeArray<ContourPolygon>()
            let colsF = float columns
            let rowsF = float rows
            let ringBuf = ResizeArray<Point>()
            let pathBuf = ResizeArray<Point>()
            let positions = System.Collections.Generic.Dictionary<Point, int>()
            let keptBuf = ResizeArray<Point>()
            for threshold in List.sort config.Thresholds do
                token.ThrowIfCancellationRequested()
                let points=System.Collections.Generic.Dictionary<int64,Point>()
                let links=System.Collections.Generic.Dictionary<int64,int64>()
                let crossing (key: int64) =
                    match points.TryGetValue key with
                    | true,point -> point
                    | _ ->
                        let x = unpackX key
                        let y = unpackY key
                        let horizontal = unpackHoriz key
                        let x2,y2=if horizontal then x+1,y else x,y+1
                        let t=
                            if not config.Interpolate then 0.5
                            elif allObserved && x >= 0 && x2 < columns && y >= 0 && y2 < rows then
                                let a = values[y * columns + x]
                                let b = values[y2 * columns + x2]
                                max 0. (min 1. (fraction a b threshold))
                            else
                                match sample x y,sample x2 y2 with
                                | struct(a,true),struct(b,true) -> max 0. (min 1. (fraction a b threshold))
                                | _ -> 0.5
                        let point=Point.create (max 0. (min colsF (float x+0.5+(if horizontal then t else 0.))))
                                               (max 0. (min rowsF (float y+0.5+(if horizontal then 0. else t))))
                        points.Add(key,point)
                        point
                let add (first: int64) (last: int64) =
                    if links.Count>=config.MaxVertices-vertices then raise(ContourAbort(BudgetExceeded "Contour vertices exceed the output budget."))
                    crossing first |> ignore
                    crossing last |> ignore
#if FABLE_COMPILER
                    if links.ContainsKey first then raise(ContourAbort(InvalidTopology "A contour edge has multiple outgoing segments."))
                    links.Add(first,last)
#else
                    if not(links.TryAdd(first,last)) then raise(ContourAbort(InvalidTopology "A contour edge has multiple outgoing segments."))
#endif
                let inline emitCell x y code aVal aObs bVal bObs cVal cObs dVal dObs =
                    let top = packEdge x y true
                    let right = packEdge (x + 1) y false
                    let bottom = packEdge x (y + 1) true
                    let left = packEdge x y false
                    match code with
                    | 1 -> add top left
                    | 2 -> add right top
                    | 3 -> add right left
                    | 4 -> add bottom right
                    | 5 ->
                        let s = if aObs && bObs && cObs && dObs then saddle aVal bVal cVal dVal threshold else 0
                        if s > 0 then add top right; add bottom left
                        else add top left; add bottom right
                    | 6 -> add bottom top
                    | 7 -> add bottom left
                    | 8 -> add left bottom
                    | 9 -> add top bottom
                    | 10 ->
                        let s = if aObs && bObs && cObs && dObs then saddle aVal bVal cVal dVal threshold else 0
                        if s < 0 then add left top; add right bottom
                        else add right top; add left bottom
                    | 11 -> add right bottom
                    | 12 -> add left right
                    | 13 -> add top right
                    | 14 -> add left top
                    | _ -> ()
                let inline scanGeneralCell x y =
                    let struct(av, ao) = sample x y
                    let struct(bv, bo) = sample (x + 1) y
                    let struct(cv, co) = sample (x + 1) (y + 1)
                    let struct(dv, do_) = sample x (y + 1)
                    let code =
                        (if ao && av >= threshold then 1 else 0) +
                        (if bo && bv >= threshold then 2 else 0) +
                        (if co && cv >= threshold then 4 else 0) +
                        (if do_ && dv >= threshold then 8 else 0)
                    if code <> 0 && code <> 15 then
                        emitCell x y code av ao bv bo cv co dv do_
                if allObserved then
                    for x = -1 to columns - 1 do
                        if ((x + 1) &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                        scanGeneralCell x -1
                    for y = 0 to rows - 2 do
                        token.ThrowIfCancellationRequested()
                        scanGeneralCell -1 y
                        let row0 = y * columns
                        let row1 = row0 + columns
                        let mutable x = 0
                        let mutable lastCancel = 0
#if !FABLE_COMPILER
                        let rVal = &MemoryMarshal.GetArrayDataReference values
                        if Vector512.IsHardwareAccelerated && columns >= 9 then
                            let vThresh = Vector512.Create threshold
                            while x <= columns - 9 do
                                if x - lastCancel >= 4096 then
                                    lastCancel <- x
                                    token.ThrowIfCancellationRequested()
                                let v0 = Vector512.LoadUnsafe(&rVal, unativeint (row0 + x))
                                let v1 = Vector512.LoadUnsafe(&rVal, unativeint (row1 + x))
                                let bits0 = int (Vector512.ExtractMostSignificantBits(Vector512.GreaterThanOrEqual(v0, vThresh)))
                                let bits1 = int (Vector512.ExtractMostSignificantBits(Vector512.GreaterThanOrEqual(v1, vThresh)))
                                if (bits0 ||| bits1) <> 0 && (bits0 &&& bits1) <> 0xFF then
                                    for k = 0 to 6 do
                                        let code =
                                            ((bits0 >>> k) &&& 1) |||
                                            (((bits0 >>> (k + 1)) &&& 1) <<< 1) |||
                                            (((bits1 >>> (k + 1)) &&& 1) <<< 2) |||
                                            (((bits1 >>> k) &&& 1) <<< 3)
                                        if code <> 0 && code <> 15 then
                                            let cx = x + k
                                            emitCell cx y code values[row0 + cx] true values[row0 + cx + 1] true values[row1 + cx + 1] true values[row1 + cx] true
                                x <- x + 7
                        elif Vector256.IsHardwareAccelerated && columns >= 5 then
                            let vThresh = Vector256.Create threshold
                            while x <= columns - 5 do
                                if x - lastCancel >= 4096 then
                                    lastCancel <- x
                                    token.ThrowIfCancellationRequested()
                                let v0 = Vector256.LoadUnsafe(&rVal, unativeint (row0 + x))
                                let v1 = Vector256.LoadUnsafe(&rVal, unativeint (row1 + x))
                                let bits0 = int (Vector256.ExtractMostSignificantBits(Vector256.GreaterThanOrEqual(v0, vThresh)))
                                let bits1 = int (Vector256.ExtractMostSignificantBits(Vector256.GreaterThanOrEqual(v1, vThresh)))
                                if (bits0 ||| bits1) <> 0 && (bits0 &&& bits1) <> 0xF then
                                    for k = 0 to 2 do
                                        let code =
                                            ((bits0 >>> k) &&& 1) |||
                                            (((bits0 >>> (k + 1)) &&& 1) <<< 1) |||
                                            (((bits1 >>> (k + 1)) &&& 1) <<< 2) |||
                                            (((bits1 >>> k) &&& 1) <<< 3)
                                        if code <> 0 && code <> 15 then
                                            let cx = x + k
                                            emitCell cx y code values[row0 + cx] true values[row0 + cx + 1] true values[row1 + cx + 1] true values[row1 + cx] true
                                x <- x + 3
#endif
                        while x <= columns - 2 do
                            if x - lastCancel >= 4096 then
                                lastCancel <- x
                                token.ThrowIfCancellationRequested()
                            let av = values[row0 + x]
                            let bv = values[row0 + x + 1]
                            let cv = values[row1 + x + 1]
                            let dv = values[row1 + x]
                            let code =
                                (if av >= threshold then 1 else 0) +
                                (if bv >= threshold then 2 else 0) +
                                (if cv >= threshold then 4 else 0) +
                                (if dv >= threshold then 8 else 0)
                            if code <> 0 && code <> 15 then
                                emitCell x y code av true bv true cv true dv true
                            x <- x + 1
                        scanGeneralCell (columns - 1) y
                    token.ThrowIfCancellationRequested()
                    for x = -1 to columns - 1 do
                        if ((x + 1) &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                        scanGeneralCell x (rows - 1)
                else
                    for y in -1..rows-1 do
                        token.ThrowIfCancellationRequested()
                        for x in -1..columns-1 do
                            if (x+1)%4096=0 then token.ThrowIfCancellationRequested()
                            scanGeneralCell x y
                let rings=ResizeArray<struct(Point list * float)>()
                let mutable shellCount = 0
                let mutable holeCount = 0
                let split (ring:ResizeArray<Point>) =
                    pathBuf.Clear()
                    positions.Clear()
                    for rIdx = 0 to ring.Count - 1 do
                        let point = ring[rIdx]
                        if (pathBuf.Count &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                        match positions.TryGetValue point with
                        | true,index ->
                            let count = pathBuf.Count - index
                            if count >= 3 then
                                let cycle = simplifySlice token pathBuf index count keptBuf
                                let area = if cycle.IsEmpty then 0. else signedArea cycle
                                if area <> 0. then
                                    vertices <- vertices + cycle.Length
                                    if vertices > config.MaxVertices then raise(ContourAbort(BudgetExceeded "Closed contour vertices exceed the output budget."))
                                    if area > 0. then shellCount <- shellCount + 1 else holeCount <- holeCount + 1
                                    rings.Add(struct(cycle, area))
                            for i = index + 1 to pathBuf.Count - 1 do positions.Remove pathBuf[i] |> ignore
                            pathBuf.RemoveRange(index + 1, pathBuf.Count - index - 1)
                        | _ ->
                            positions.Add(point, pathBuf.Count)
                            pathBuf.Add point
                if links.Count > 0 then
                    let starts = Array.zeroCreate<int64> links.Count
                    links.Keys.CopyTo(starts, 0)
                    Array.Sort starts
                    for sIdx = 0 to starts.Length - 1 do
                        let start = starts[sIdx]
                        if links.ContainsKey start then
                            ringBuf.Clear()
                            let mutable current = start
                            let mutable closed = false
                            while not closed do
                                if (ringBuf.Count &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                charge 1L
                                ringBuf.Add points[current]
                                match links.TryGetValue current with
                                | false,_ -> raise(ContourAbort(InvalidTopology "Contour stitching encountered an open ring."))
                                | true,next ->
                                    links.Remove current |> ignore
                                    current <- next
                                    closed <- current = start
                            ringBuf.Add points[start]
                            split ringBuf
                if output.Count + shellCount > config.MaxPolygons then raise(ContourAbort(BudgetExceeded "Contour polygon count exceeds its budget."))
                if shellCount = 0 && holeCount > 0 then raise(ContourAbort(InvalidTopology "Contour hole has no enclosing shell."))
                if shellCount > 0 then
                    let extent=DensityGrid.bounds grid
                    let exX = Bounds.x extent
                    let exY = Bounds.y extent
                    let exW = Bounds.width extent
                    let exH = Bounds.height extent
                    let mapped=System.Collections.Generic.Dictionary<Point,Point>(points.Count)
                    let world (point: Point) =
                        let result=Point.create (exX + exW * (point.X / colsF))
                                                (exY + exH * (point.Y / rowsF))
                        match mapped.TryGetValue result with
                        | true,prior when prior<>point -> raise(ContourAbort(InvalidObservation "Contour coordinates collapse at this extent's numeric precision."))
                        | _ -> mapped[result]<-point
                        result
                    if holeCount = 0 then
                        for rIdx = 0 to rings.Count - 1 do
                            let struct(shell, area) = rings[rIdx]
                            if area > 0. then
                                token.ThrowIfCancellationRequested()
                                output.Add { Threshold = threshold; Outer = List.map world shell; Holes = [] }
                    else
                        let shells = Array.zeroCreate<struct(Point list * float)> shellCount
                        let boxes = Array.zeroCreate<struct(float * float * float * float)> shellCount
                        let mutable sWrite = 0
                        for rIdx = 0 to rings.Count - 1 do
                            let struct(ring, area) = rings[rIdx]
                            if area > 0. then
                                shells[sWrite] <- struct(ring, area)
                                boxes[sWrite] <- ringBounds ring
                                sWrite <- sWrite + 1
                        let holes = Array.zeroCreate<ResizeArray<Point list>> shellCount
                        let shellArrays = Array.zeroCreate<Point array> shellCount
                        let candidates = Array.init shellCount id
                        Array.Sort(candidates, System.Comparison<int>(fun a b ->
                            let struct(_, areaA) = shells[a]
                            let struct(_, areaB) = shells[b]
                            areaA.CompareTo areaB))
                        for rIdx = 0 to rings.Count - 1 do
                            let struct(hole, area) = rings[rIdx]
                            if area < 0. then
                                token.ThrowIfCancellationRequested()
                                let struct(hx, hy, hx2, hy2) = ringBounds hole
                                let mutable parent = -1
                                let mutable index = 0
                                while parent < 0 && index < candidates.Length do
                                    token.ThrowIfCancellationRequested()
                                    charge 1L
                                    let candidate = candidates[index]
                                    let struct(x, y, x2, y2) = boxes[candidate]
                                    if x <= hx && y <= hy && x2 >= hx2 && y2 >= hy2 then
                                        let struct(shell, _) = shells[candidate]
                                        let mutable sArr = shellArrays[candidate]
                                        if isNull sArr then
                                            sArr <- List.toArray shell
                                            shellArrays[candidate] <- sArr
                                        let shellLen = int64 sArr.Length
                                        let contains point =
                                            token.ThrowIfCancellationRequested()
                                            charge shellLen
                                            Polygon.containsArrayWithCancellation token point sArr
                                        if hole |> List.forall contains then parent <- candidate
                                    index <- index + 1
                                if parent < 0 then raise(ContourAbort(InvalidTopology "Contour hole has no enclosing shell."))
                                let mutable hList = holes[parent]
                                if isNull hList then
                                    hList <- ResizeArray<Point list>()
                                    holes[parent] <- hList
                                hList.Add hole
                        for i = 0 to shellCount - 1 do
                            token.ThrowIfCancellationRequested()
                            let struct(shell, _) = shells[i]
                            let hList = holes[i]
                            let mappedHoles =
                                if isNull hList then []
                                else
                                    let mutable acc = []
                                    for k = hList.Count - 1 downto 0 do
                                        acc <- List.map world hList[k] :: acc
                                    acc
                            output.Add { Threshold = threshold; Outer = List.map world shell; Holes = mappedHoles }
            token.ThrowIfCancellationRequested()
            Ok(Data.resizeToList output)
          with ContourAbort error -> Error [error]
    /// <summary>Runs <c>computeWithCancellation</c> without cancellation.</summary>
    let compute config grid = computeWithCancellation System.Threading.CancellationToken.None config grid

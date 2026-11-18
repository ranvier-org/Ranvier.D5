namespace Ranvier.D5

open System
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif

/// <summary>An identified point with a weight, used as density and hexbin input; IDs must be non-empty and unique, and weights finite and
/// nonnegative.</summary>
type WeightedPoint = { Id:string; Position:Point; Weight:float }

/// <summary>How blur handles kernel weight falling outside the grid or on missing cells: Renormalize spreads it over observed cells, Drop discards
/// it.</summary>
[<RequireQualifiedAccess>]
type DensityBoundary = Renormalize | Drop
/// <summary>Mass keeps the summed weight per cell; Density divides it by the cell area in layout units.</summary>
[<RequireQualifiedAccess>]
type DensityNormalization = Mass | Density

/// <summary>Binned density settings: grid bounds and dimensions, Gaussian Bandwidth in layout units (0 disables blur), boundary and normalization
/// policies, and cell, kernel-radius and point budgets.</summary>
type DensityConfig =
    { Bounds:Bounds; Columns:int; Rows:int; Bandwidth:float; Boundary:DensityBoundary
      Normalization:DensityNormalization; MaxCells:int; MaxKernelRadius:int; MaxPoints:int }

/// <summary>Default density configuration.</summary>
[<RequireQualifiedAccess>]
module DensityConfig =
    /// <summary>Creates a configuration without blur, with Renormalize boundary, Mass normalization and budgets of 1,048,576 cells, a 256-cell kernel
    /// radius and 1,000,000 points.</summary>
    let create bounds columns rows =
        {Bounds=bounds;Columns=columns;Rows=rows;Bandwidth=0.;Boundary=DensityBoundary.Renormalize
         Normalization=DensityNormalization.Mass;MaxCells=1048576;MaxKernelRadius=256;MaxPoints=1000000}

/// <summary>An immutable row-major grid of cell values over bounds, with missing cells and the IDs of points binned into each cell.</summary>
type DensityGrid = private { Extent:Bounds; ColumnCount:int; RowCount:int; Data:float array; Observed:bool array; Membership:string list array }

/// <summary>Construction of and access to density grids.</summary>
[<RequireQualifiedAccess>]
module DensityGrid =
    /// <summary>Creates a grid from row-major values, where None marks a missing cell. Returns Error for nonpositive dimensions or budget, empty
    /// bounds, a value count that does not match, too many cells, or a non-finite value.</summary>
    let createWithBudget (maxCells:int) (bounds:Bounds) (columns:int) (rows:int) (values:float option list) : Result<DensityGrid,LayoutError list> =
        let count=int64 columns*int64 rows
        if maxCells<=0 || columns<=0 || rows<=0 then Error [InvalidObservation "Grid dimensions and cell budget must be positive."]
        elif count>int64 maxCells then Error [BudgetExceeded "Grid exceeds the cell budget."]
        elif Bounds.width bounds<=0. || Bounds.height bounds<=0. then Error [InvalidObservation "Grid bounds must have positive dimensions."]
        elif int64 values.Length<>count then Error [InvalidObservation "Grid values must match the dimensions."]
        else
            let n = int count
            let data = Array.zeroCreate<float> n
            let obs = Array.zeroCreate<bool> n
            let mutable i = 0
            let mutable invalid = false
            let mutable cur = values
            while not invalid && not cur.IsEmpty do
                match cur.Head with
                | Some v ->
                    if not (Double.IsFinite v) then invalid <- true
                    else
                        data[i] <- v
                        obs[i] <- true
                | None -> ()
                i <- i + 1
                cur <- cur.Tail
            if invalid then Error [InvalidObservation "Measured cells must be finite; use None for missing cells."]
            else
                Ok {Extent=bounds;ColumnCount=columns;RowCount=rows
                    Data=data;Observed=obs
                    Membership=Array.create n []}
    /// <summary>Runs <c>createWithBudget</c> with a budget of 1,048,576 cells.</summary>
    let create bounds columns rows values = createWithBudget 1048576 bounds columns rows values
    let internal copySamples grid = Array.copy grid.Data,Array.copy grid.Observed
    let internal rawSamples grid = grid.Data,grid.Observed
    /// <summary>Returns the grid's layout bounds.</summary>
    let bounds grid = grid.Extent
    /// <summary>Returns the column count.</summary>
    let columns grid = grid.ColumnCount
    /// <summary>Returns the row count.</summary>
    let rows grid = grid.RowCount
    let private index column row grid =
        if column<0 || column>=grid.ColumnCount || row<0 || row>=grid.RowCount then invalidArg "cell" "Cell is outside the grid."
        row*grid.ColumnCount+column
    /// <summary>Returns the cell value, or None for a missing cell; raises ArgumentException for a cell outside the grid.</summary>
    let value column row grid =
        let i=index column row grid
        if grid.Observed[i] then Some grid.Data[i] else None
    /// <summary>Returns the IDs of points binned into the cell in input order; raises ArgumentException for a cell outside the grid.</summary>
    let members column row grid = grid.Membership[index column row grid]
    /// <summary>Returns every cell in row-major order, with None for missing cells.</summary>
    let values grid = [for row in 0..grid.RowCount-1 do for column in 0..grid.ColumnCount-1 -> value column row grid]

/// <summary>Binned density grids, Gaussian blur and one-dimensional kernel density estimation.</summary>
[<RequireQualifiedAccess>]
module Density =
    /// <summary>Applies a separable Gaussian blur whose standard deviation is the bandwidth in layout units; missing cells neither emit nor receive
    /// weight, and zero bandwidth returns the grid unchanged. Returns Error for invalid arguments, a kernel radius or work above budget, or
    /// non-finite results.</summary>
    let blurWithCancellation (token:System.Threading.CancellationToken) (bandwidth:float) (boundary:DensityBoundary) (maxRadius:int) (grid:DensityGrid) : Result<DensityGrid,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not(Double.IsFinite bandwidth) || bandwidth<0. || maxRadius<0 then
            Error [InvalidObservation "Bandwidth must be finite and nonnegative; kernel radius budget must be nonnegative."]
        elif bandwidth=0. then Ok grid
        else
            let sigmaX=bandwidth/(Bounds.width grid.Extent/float grid.ColumnCount)
            let sigmaY=bandwidth/(Bounds.height grid.Extent/float grid.RowCount)
            let radius sigma=ceil(3.*sigma)
            if not(Double.IsFinite sigmaX && Double.IsFinite sigmaY)
               || radius sigmaX>float(min maxRadius ((System.Int32.MaxValue-1)/2)) || radius sigmaY>float(min maxRadius ((System.Int32.MaxValue-1)/2)) then
                Error [BudgetExceeded "Gaussian radius exceeds the configured kernel budget."]
            elif 4.*float grid.Data.Length*(radius sigmaX+radius sigmaY+1.)>256000000.
                 || 2.*max (radius sigmaX) (radius sigmaY)+1.>1048576. then
                Error [BudgetExceeded "Gaussian blur exceeds the 256 million tap-work or one million kernel-weight budget."]
            else
                let kernel sigma =
                    let r=int(radius sigma)
                    let weights=Array.init (r*2+1) (fun i ->
                        if i%4096=0 then token.ThrowIfCancellationRequested()
                        let offset=float(i-r)
                        if offset=0. then 1.
                        elif sigma=0. then 0.
                        else let z=offset/sigma in exp(-0.5*z*z))
                    r,weights
#if !FABLE_COMPILER
                let allObserved = Array.IndexOf(grid.Observed, false) < 0
#else
                let mutable allObserved = true
                let mutable obsIdx = 0
                while allObserved && obsIdx < grid.Observed.Length do
                    if not grid.Observed[obsIdx] then allObserved <- false
                    obsIdx <- obsIdx + 1
#endif
                let data =
                    if allObserved then
                        let cols = grid.ColumnCount
                        let rows = grid.RowCount
                        let totalCells = grid.Data.Length
                        let buildInvDenom limit r (weights: float array) =
                            let full = Array.sum weights
                            let inv = Array.zeroCreate<float> limit
                            if boundary = DensityBoundary.Drop then
                                let invFull = 1. / full
                                for c = 0 to limit - 1 do inv[c] <- invFull
                            else
                                for c = 0 to limit - 1 do
                                    let first = max 0 (c - r)
                                    let last = min (limit - 1) (c + r)
                                    let mutable d = 0.
                                    for p = first to last do
                                        d <- d + weights[p - c + r]
                                    inv[c] <- 1. / d
                            inv
                        let rx, weightsX = kernel sigmaX
                        let invDenomX = buildInvDenom cols rx weightsX
                        let horizTarget = Array.zeroCreate<float> totalCells
                        let scaledRow = Array.zeroCreate<float> cols
                        let srcData = grid.Data
                        for row = 0 to rows - 1 do
                            token.ThrowIfCancellationRequested()
                            let rowOffset = row * cols
#if !FABLE_COMPILER
                            let anyNonZero = SimdNumeric.mulPairwiseCheckNonZero scaledRow srcData rowOffset invDenomX cols
#else
                            let mutable anyNonZero = false
                            for c = 0 to cols - 1 do
                                let v = srcData[rowOffset + c]
                                if v <> 0. then anyNonZero <- true
                                scaledRow[c] <- v * invDenomX[c]
#endif
                            if anyNonZero then
                                for offset = -rx to rx do
                                    if ((offset + rx) &&& 63) = 0 then token.ThrowIfCancellationRequested()
                                    let w = weightsX[offset + rx]
                                    if w <> 0. then
                                        let srcStart = max 0 (-offset)
                                        let len = min (cols - 1) (cols - 1 - offset) - srcStart + 1
                                        if len > 0 then
                                            let dstOffset = rowOffset + srcStart + offset
#if !FABLE_COMPILER
                                            SimdNumeric.fmaSlice horizTarget dstOffset scaledRow srcStart w len
#else
                                            for k = 0 to len - 1 do
                                                horizTarget[dstOffset + k] <- horizTarget[dstOffset + k] + w * scaledRow[srcStart + k]
#endif
                        let ry, weightsY = kernel sigmaY
                        let invDenomY = buildInvDenom rows ry weightsY
                        let vertTarget = Array.zeroCreate<float> totalCells
                        for srcRow = 0 to rows - 1 do
                            token.ThrowIfCancellationRequested()
                            let srcOffset = srcRow * cols
#if !FABLE_COMPILER
                            let rowActive = SimdNumeric.isSliceNonZero horizTarget srcOffset cols
#else
                            let mutable rowActive = false
                            let mutable c = 0
                            while not rowActive && c < cols do
                                if horizTarget[srcOffset + c] <> 0. then rowActive <- true
                                c <- c + 1
#endif
                            if rowActive then
                                let invD = invDenomY[srcRow]
                                let first = max 0 (srcRow - ry)
                                let last = min (rows - 1) (srcRow + ry)
                                for dstRow = first to last do
                                    if ((dstRow - first) &&& 63) = 0 then token.ThrowIfCancellationRequested()
                                    let factor = weightsY[dstRow - srcRow + ry] * invD
                                    if factor <> 0. then
                                        let dstOffset = dstRow * cols
#if !FABLE_COMPILER
                                        SimdNumeric.fmaSlice vertTarget dstOffset horizTarget srcOffset factor cols
#else
                                        for c = 0 to cols - 1 do
                                            vertTarget[dstOffset + c] <- vertTarget[dstOffset + c] + factor * horizTarget[srcOffset + c]
#endif
                        vertTarget
                    else
                        let pass horizontal sigma (source:float array) =
                            let radius,weights=kernel sigma
                            let full=Array.sum weights
                            let target=Array.zeroCreate<float> source.Length
                            for row in 0..grid.RowCount-1 do
                                token.ThrowIfCancellationRequested()
                                for column in 0..grid.ColumnCount-1 do
                                    if column%64=0 then token.ThrowIfCancellationRequested()
                                    let index=row*grid.ColumnCount+column
                                    if grid.Observed[index] && source[index]<>0. then
                                        let coordinate,limit=if horizontal then column,grid.ColumnCount else row,grid.RowCount
                                        let first,last=max 0 (coordinate-radius),min (limit-1) (coordinate+radius)
                                        let destination position=if horizontal then row*grid.ColumnCount+position else position*grid.ColumnCount+column
                                        let mutable denominator=if boundary=DensityBoundary.Drop then full else 0.
                                        if boundary=DensityBoundary.Renormalize then
                                            for p in first..last do
                                                if (p-first)%4096=0 then token.ThrowIfCancellationRequested()
                                                if grid.Observed[destination p] then denominator<-denominator+weights[p-coordinate+radius]
                                        for p in first..last do
                                            if (p-first)%4096=0 then token.ThrowIfCancellationRequested()
                                            let dest=destination p
                                            if grid.Observed[dest] then
                                                target[dest]<-target[dest]+source[index]*(weights[p-coordinate+radius]/denominator)
                            target
                        pass true sigmaX grid.Data |> pass false sigmaY
                token.ThrowIfCancellationRequested()
#if !FABLE_COMPILER
                let isFinite = SimdNumeric.allFinite data
#else
                let isFinite = not (data |> Array.exists(Double.IsFinite >> not))
#endif
                if not isFinite then Error [InvalidObservation "Blur exceeds the finite numeric range."]
                else Ok {grid with Data=data}
    /// <summary>Runs <c>blurWithCancellation</c> without cancellation.</summary>
    let blur bandwidth boundary maxRadius grid = blurWithCancellation System.Threading.CancellationToken.None bandwidth boundary maxRadius grid
    /// <summary>Bins weighted points into grid cells, blurs and normalizes them. Returns all point errors together (empty or duplicate IDs, invalid
    /// weights, points outside the bounds), or one error for invalid settings, exceeded budgets or non-finite results.</summary>
    let gridWithCancellation (token:System.Threading.CancellationToken) (config:DensityConfig) (points:WeightedPoint list) : Result<DensityGrid,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let count=int64 config.Columns*int64 config.Rows
        let width,height=Bounds.width config.Bounds,Bounds.height config.Bounds
        if config.Columns<=0 || config.Rows<=0 || config.MaxCells<=0 || config.MaxPoints<=0 || width<=0. || height<=0.
           || not(Double.IsFinite config.Bandwidth) || config.Bandwidth<0. || config.MaxKernelRadius<0 then
            Error [InvalidObservation "Require positive grid dimensions, bounds and budgets, and a finite nonnegative bandwidth."]
        elif count>int64 config.MaxCells then Error [BudgetExceeded "Density input or grid exceeds its budget."]
        else
            let ptCount = List.length points
            if ptCount > config.MaxPoints then Error [BudgetExceeded "Density input or grid exceeds its budget."]
            else
                let ptsArr = List.toArray points
                let errors=ResizeArray<LayoutError>()
                let seen=System.Collections.Generic.HashSet<string>(ptCount, StringComparer.Ordinal)
                for idx = 0 to ptCount - 1 do
                    if (idx &&& 63) = 0 then token.ThrowIfCancellationRequested()
                    let point = ptsArr[idx]
                    if System.String.IsNullOrWhiteSpace point.Id then errors.Add(InvalidObservation "Point ID must not be empty.")
                    elif not(seen.Add point.Id) then errors.Add(DuplicateId point.Id)
                    if not(Double.IsFinite point.Weight) || point.Weight<0. then errors.Add(InvalidObservation "Point weights must be finite and nonnegative.")
                    if not(Bounds.contains point.Position config.Bounds) then errors.Add(InvalidObservation("Point lies outside density bounds: "+point.Id))
                if errors.Count>0 then Error(Data.resizeToList errors)
                else
                    let cellCount = int count
                    let data=Array.zeroCreate<float> cellCount
                    let members=Array.create<string list> cellCount []
                    let cols = config.Columns
                    let rows = config.Rows
                    let colsF = float cols
                    let rowsF = float rows
                    let x0 = Bounds.x config.Bounds
                    let y0 = Bounds.y config.Bounds
                    for idx = ptCount - 1 downto 0 do
                        if ((ptCount - 1 - idx) &&& 63) = 0 then token.ThrowIfCancellationRequested()
                        let point = ptsArr[idx]
                        let column = min (cols - 1) (int(floor((point.Position.X - x0) / width * colsF)))
                        let row = min (rows - 1) (int(floor((point.Position.Y - y0) / height * rowsF)))
                        let index = row * cols + column
                        data[index] <- data[index] + point.Weight
                        members[index] <- point.Id :: members[index]
#if !FABLE_COMPILER
                    let weightsFinite = SimdNumeric.allFinite data
#else
                    let weightsFinite = not (data |> Array.exists(Double.IsFinite >> not))
#endif
                    if not weightsFinite then Error [InvalidObservation "Cell weight exceeds the finite numeric range."]
                    else
                        let initial={Extent=config.Bounds;ColumnCount=cols;RowCount=rows;Data=data
                                     Observed=Array.create cellCount true;Membership=members}
                        match blurWithCancellation token config.Bandwidth config.Boundary config.MaxKernelRadius initial with
                        | Error errors -> Error errors
                        | Ok grid when config.Normalization=DensityNormalization.Mass -> Ok grid
                        | Ok grid ->
                            let area=(width/colsF)*(height/rowsF)
                            if not(Double.IsFinite area) || area<=0. then Error [InvalidObservation "Cell area is outside the finite positive numeric range."]
                            else
                                token.ThrowIfCancellationRequested()
#if !FABLE_COMPILER
                                let normFinite = SimdNumeric.divideInPlaceCheckFinite grid.Data area
                                if not normFinite then Error [InvalidObservation "Density exceeds the finite numeric range."]
                                else Ok grid
#else
                                let normalized=grid.Data |> Array.mapi(fun i value ->
                                    if i%4096=0 then token.ThrowIfCancellationRequested()
                                    value/area)
                                if normalized |> Array.exists(Double.IsFinite >> not) then Error [InvalidObservation "Density exceeds the finite numeric range."]
                                else Ok {grid with Data=normalized}
#endif
    /// <summary>Runs <c>gridWithCancellation</c> without cancellation.</summary>
    let grid config points = gridWithCancellation System.Threading.CancellationToken.None config points
    /// <summary>Evaluates a Gaussian kernel density of (position, weight) samples at each query position, normalized to unit total mass; all-zero
    /// weights yield zeros. Returns Error for a nonpositive bandwidth, non-finite input, negative weights or more than 100,000,000 sample-query
    /// pairs.</summary>
    let kde1dWithCancellation (token:System.Threading.CancellationToken) (bandwidth:float) (samples:(float*float) list) (positions:float list) : Result<float list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not(Double.IsFinite bandwidth) || bandwidth<=0. then Error [InvalidObservation "KDE bandwidth must be finite and positive."]
        else
            let sLen = samples.Length
            let qLen = positions.Length
            if sLen>1000000 || qLen>1000000 || int64 sLen*int64 qLen>100000000L then
                Error [BudgetExceeded "KDE exceeds the sample/query work budget."]
            elif samples |> List.exists(fun (position,weight) -> not(Double.IsFinite position && Double.IsFinite weight) || weight<0.)
                 || positions |> List.exists(Double.IsFinite >> not) then
                Error [InvalidObservation "KDE samples and query positions must be finite; weights must be nonnegative."]
            else
                let mutable largest = 0.
                let mutable posCount = 0
                for _, weight in samples do
                    if weight > 0. then
                        posCount <- posCount + 1
                        if weight > largest then largest <- weight
                if largest = 0. then Ok(List.replicate qLen 0.)
                else
                    let mutable scaledTotal = 0.
                    for _, weight in samples do
                        if weight > 0. then scaledTotal <- scaledTotal + weight / largest
                    let logTotal = log largest + log scaledTotal
                    let logFactor = -0.5 * log(2. * System.Math.PI) - log bandwidth
                    let logOffset = logFactor - logTotal
                    let samplePos = Array.zeroCreate<float> posCount
                    let sampleLogW = Array.zeroCreate<float> posCount
                    let mutable maxSampleAbs = 0.
                    let mutable wIdx = 0
                    for position, weight in samples do
                        if weight > 0. then
                            samplePos[wIdx] <- position
                            sampleLogW[wIdx] <- log weight + logOffset
                            let ap = abs position
                            if ap > maxSampleAbs then maxSampleAbs <- ap
                            wIdx <- wIdx + 1
                    let invBw = 1. / bandwidth
                    let invBwOk = Double.IsFinite invBw && invBw > 0. && maxSampleAbs < 1e150
                    let outArr = Array.zeroCreate<float> qLen
                    let mutable qIdx = 0
                    let mutable anyNonFinite = false
                    for query in positions do
                        token.ThrowIfCancellationRequested()
                        let value =
#if !FABLE_COMPILER
                            if invBwOk && abs query < 1e150 then
                                let m = posCount
                                let rPos = &MemoryMarshal.GetArrayDataReference samplePos
                                let rLogW = &MemoryMarshal.GetArrayDataReference sampleLogW
                                let mutable largestTerm = Double.NegativeInfinity
                                let mutable i = 0
                                if Vector512.IsHardwareAccelerated && m >= 8 then
                                    let vQ = Vector512.Create query
                                    let vInv = Vector512.Create invBw
                                    let vNegHalf = Vector512.Create -0.5
                                    let mutable vMax = Vector512.Create Double.NegativeInfinity
                                    while i <= m - 8 do
                                        if (i &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                        let vP = Vector512.LoadUnsafe(&rPos, unativeint i)
                                        let vL = Vector512.LoadUnsafe(&rLogW, unativeint i)
                                        let z = (vQ - vP) * vInv
                                        let vTerm = Vector512.FusedMultiplyAdd(vNegHalf, z * z, vL)
                                        vMax <- Vector512.Max(vMax, vTerm)
                                        i <- i + 8
                                    for lane = 0 to 7 do
                                        let e = vMax.GetElement lane
                                        if e > largestTerm then largestTerm <- e
                                elif Vector256.IsHardwareAccelerated && m >= 4 then
                                    let vQ = Vector256.Create query
                                    let vInv = Vector256.Create invBw
                                    let vNegHalf = Vector256.Create -0.5
                                    let mutable vMax = Vector256.Create Double.NegativeInfinity
                                    while i <= m - 4 do
                                        if (i &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                        let vP = Vector256.LoadUnsafe(&rPos, unativeint i)
                                        let vL = Vector256.LoadUnsafe(&rLogW, unativeint i)
                                        let z = (vQ - vP) * vInv
                                        let vTerm = Vector256.FusedMultiplyAdd(vNegHalf, z * z, vL)
                                        vMax <- Vector256.Max(vMax, vTerm)
                                        i <- i + 4
                                    for lane = 0 to 3 do
                                        let e = vMax.GetElement lane
                                        if e > largestTerm then largestTerm <- e
                                elif Vector128.IsHardwareAccelerated && m >= 2 then
                                    let vQ = Vector128.Create query
                                    let vInv = Vector128.Create invBw
                                    let vNegHalf = Vector128.Create -0.5
                                    let mutable vMax = Vector128.Create Double.NegativeInfinity
                                    while i <= m - 2 do
                                        if (i &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                        let vP = Vector128.LoadUnsafe(&rPos, unativeint i)
                                        let vL = Vector128.LoadUnsafe(&rLogW, unativeint i)
                                        let z = (vQ - vP) * vInv
                                        let vTerm = Vector128.FusedMultiplyAdd(vNegHalf, z * z, vL)
                                        vMax <- Vector128.Max(vMax, vTerm)
                                        i <- i + 2
                                    for lane = 0 to 1 do
                                        let e = vMax.GetElement lane
                                        if e > largestTerm then largestTerm <- e
                                while i < m do
                                    let z = (query - samplePos[i]) * invBw
                                    let term = sampleLogW[i] - 0.5 * z * z
                                    if Double.IsFinite term && term > largestTerm then largestTerm <- term
                                    i <- i + 1
                                if not (Double.IsFinite largestTerm) then 0.
                                else
                                    let cutoff = largestTerm - 746.
                                    let hasCutoff = Double.IsFinite cutoff
                                    let mutable sum = 0.
                                    let mutable j = 0
                                    if Vector512.IsHardwareAccelerated && m >= 8 && hasCutoff then
                                        let vQ = Vector512.Create query
                                        let vInv = Vector512.Create invBw
                                        let vNegHalf = Vector512.Create -0.5
                                        let vCut = Vector512.Create cutoff
                                        while j <= m - 8 do
                                            if (j &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                            let vP = Vector512.LoadUnsafe(&rPos, unativeint j)
                                            let vL = Vector512.LoadUnsafe(&rLogW, unativeint j)
                                            let z = (vQ - vP) * vInv
                                            let vTerm = Vector512.FusedMultiplyAdd(vNegHalf, z * z, vL)
                                            if Vector512.GreaterThan(vTerm, vCut) <> Vector512<float>.Zero then
                                                for lane = 0 to 7 do
                                                    let t = vTerm.GetElement lane
                                                    if t > cutoff then sum <- sum + Math.Exp(t - largestTerm)
                                            j <- j + 8
                                    elif Vector256.IsHardwareAccelerated && m >= 4 && hasCutoff then
                                        let vQ = Vector256.Create query
                                        let vInv = Vector256.Create invBw
                                        let vNegHalf = Vector256.Create -0.5
                                        let vCut = Vector256.Create cutoff
                                        while j <= m - 4 do
                                            if (j &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                            let vP = Vector256.LoadUnsafe(&rPos, unativeint j)
                                            let vL = Vector256.LoadUnsafe(&rLogW, unativeint j)
                                            let z = (vQ - vP) * vInv
                                            let vTerm = Vector256.FusedMultiplyAdd(vNegHalf, z * z, vL)
                                            if Vector256.GreaterThan(vTerm, vCut) <> Vector256<float>.Zero then
                                                for lane = 0 to 3 do
                                                    let t = vTerm.GetElement lane
                                                    if t > cutoff then sum <- sum + Math.Exp(t - largestTerm)
                                            j <- j + 4
                                    elif Vector128.IsHardwareAccelerated && m >= 2 && hasCutoff then
                                        let vQ = Vector128.Create query
                                        let vInv = Vector128.Create invBw
                                        let vNegHalf = Vector128.Create -0.5
                                        let vCut = Vector128.Create cutoff
                                        while j <= m - 2 do
                                            if (j &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                            let vP = Vector128.LoadUnsafe(&rPos, unativeint j)
                                            let vL = Vector128.LoadUnsafe(&rLogW, unativeint j)
                                            let z = (vQ - vP) * vInv
                                            let vTerm = Vector128.FusedMultiplyAdd(vNegHalf, z * z, vL)
                                            if Vector128.GreaterThan(vTerm, vCut) <> Vector128<float>.Zero then
                                                for lane = 0 to 1 do
                                                    let t = vTerm.GetElement lane
                                                    if t > cutoff then sum <- sum + Math.Exp(t - largestTerm)
                                            j <- j + 2
                                    while j < m do
                                        if (j &&& 4095) = 0 then token.ThrowIfCancellationRequested()
                                        let z = (query - samplePos[j]) * invBw
                                        let t = sampleLogW[j] - 0.5 * z * z
                                        if Double.IsFinite t && (not hasCutoff || t > cutoff) then
                                            sum <- sum + Math.Exp(t - largestTerm)
                                        j <- j + 1
                                    if sum = 0. then 0. else Math.Exp(largestTerm + Math.Log sum)
                            else
#endif
                                let mutable largestTerm = System.Double.NegativeInfinity
                                let mutable sum = 0.
                                for index = 0 to posCount - 1 do
                                    if (index &&& 63) = 0 then token.ThrowIfCancellationRequested()
                                    let position = samplePos[index]
                                    let logWeight = sampleLogW[index]
                                    let delta = query - position
                                    let z = if Double.IsFinite delta then delta / bandwidth else query / bandwidth - position / bandwidth
                                    let term = logWeight - 0.5 * z * z
                                    if Double.IsFinite term then
                                        if term > largestTerm then
                                            sum <- sum * exp(largestTerm - term) + 1.
                                            largestTerm <- term
                                        else sum <- sum + exp(term - largestTerm)
                                if sum = 0. then 0. else exp(largestTerm + log sum)
                        if not (Double.IsFinite value) then anyNonFinite <- true
                        outArr[qIdx] <- value
                        qIdx <- qIdx + 1
                    if anyNonFinite then Error [InvalidObservation "KDE density exceeds the finite numeric range."]
                    else
                        let mutable res = []
                        for k = qLen - 1 downto 0 do
                            res <- outArr[k] :: res
                        Ok res
    /// <summary>Runs <c>kde1dWithCancellation</c> without cancellation.</summary>
    let kde1d bandwidth samples positions = kde1dWithCancellation System.Threading.CancellationToken.None bandwidth samples positions

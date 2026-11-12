namespace Ranvier.D5

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
        elif values |> List.exists(Option.exists(System.Double.IsFinite >> not)) then Error [InvalidObservation "Measured cells must be finite; use None for missing cells."]
        else
            Ok {Extent=bounds;ColumnCount=columns;RowCount=rows
                Data=values |> List.map(Option.defaultValue 0.) |> List.toArray
                Observed=values |> List.map Option.isSome |> List.toArray
                Membership=Array.create (int count) []}
    /// <summary>Runs <c>createWithBudget</c> with a budget of 1,048,576 cells.</summary>
    let create bounds columns rows values = createWithBudget 1048576 bounds columns rows values
    let internal copySamples grid = Array.copy grid.Data,Array.copy grid.Observed
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
        if not(System.Double.IsFinite bandwidth) || bandwidth<0. || maxRadius<0 then
            Error [InvalidObservation "Bandwidth must be finite and nonnegative; kernel radius budget must be nonnegative."]
        elif bandwidth=0. then Ok grid
        else
            let sigmaX=bandwidth/(Bounds.width grid.Extent/float grid.ColumnCount)
            let sigmaY=bandwidth/(Bounds.height grid.Extent/float grid.RowCount)
            let radius sigma=ceil(3.*sigma)
            if not(System.Double.IsFinite sigmaX && System.Double.IsFinite sigmaY)
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
                let data=pass true sigmaX grid.Data |> pass false sigmaY
                token.ThrowIfCancellationRequested()
                if data |> Array.exists(System.Double.IsFinite >> not) then Error [InvalidObservation "Blur exceeds the finite numeric range."]
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
           || not(System.Double.IsFinite config.Bandwidth) || config.Bandwidth<0. || config.MaxKernelRadius<0 then
            Error [InvalidObservation "Require positive grid dimensions, bounds and budgets, and a finite nonnegative bandwidth."]
        elif count>int64 config.MaxCells || points.Length>config.MaxPoints then Error [BudgetExceeded "Density input or grid exceeds its budget."]
        else
            let errors=ResizeArray<LayoutError>()
            let seen=System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
            let mutable item=0
            for point in points do
                if item%64=0 then token.ThrowIfCancellationRequested()
                item<-item+1
                if System.String.IsNullOrWhiteSpace point.Id then errors.Add(InvalidObservation "Point ID must not be empty.")
                elif not(seen.Add point.Id) then errors.Add(DuplicateId point.Id)
                if not(System.Double.IsFinite point.Weight) || point.Weight<0. then errors.Add(InvalidObservation "Point weights must be finite and nonnegative.")
                if not(Bounds.contains point.Position config.Bounds) then errors.Add(InvalidObservation("Point lies outside density bounds: "+point.Id))
            if errors.Count>0 then Error(List.ofSeq errors)
            else
                let data=Array.zeroCreate<float> (int count)
                let members=Array.create<string list> (int count) []
                item<-0
                for point in points do
                    if item%64=0 then token.ThrowIfCancellationRequested()
                    item<-item+1
                    let column=min (config.Columns-1) (int(floor((point.Position.X-Bounds.x config.Bounds)/width*float config.Columns)))
                    let row=min (config.Rows-1) (int(floor((point.Position.Y-Bounds.y config.Bounds)/height*float config.Rows)))
                    let index=row*config.Columns+column
                    data[index]<-data[index]+point.Weight
                    members[index]<-point.Id::members[index]
                if data |> Array.exists(System.Double.IsFinite >> not) then Error [InvalidObservation "Cell weight exceeds the finite numeric range."]
                else
                    let initial={Extent=config.Bounds;ColumnCount=config.Columns;RowCount=config.Rows;Data=data
                                 Observed=Array.create (int count) true;Membership=members |> Array.map List.rev}
                    match blurWithCancellation token config.Bandwidth config.Boundary config.MaxKernelRadius initial with
                    | Error errors -> Error errors
                    | Ok grid when config.Normalization=DensityNormalization.Mass -> Ok grid
                    | Ok grid ->
                        let area=(width/float config.Columns)*(height/float config.Rows)
                        if not(System.Double.IsFinite area) || area<=0. then Error [InvalidObservation "Cell area is outside the finite positive numeric range."]
                        else
                            let normalized=grid.Data |> Array.mapi(fun i value ->
                                if i%4096=0 then token.ThrowIfCancellationRequested()
                                value/area)
                            if normalized |> Array.exists(System.Double.IsFinite >> not) then Error [InvalidObservation "Density exceeds the finite numeric range."]
                            else Ok {grid with Data=normalized}
    /// <summary>Runs <c>gridWithCancellation</c> without cancellation.</summary>
    let grid config points = gridWithCancellation System.Threading.CancellationToken.None config points
    /// <summary>Evaluates a Gaussian kernel density of (position, weight) samples at each query position, normalized to unit total mass; all-zero
    /// weights yield zeros. Returns Error for a nonpositive bandwidth, non-finite input, negative weights or more than 100,000,000 sample-query
    /// pairs.</summary>
    let kde1dWithCancellation (token:System.Threading.CancellationToken) (bandwidth:float) (samples:(float*float) list) (positions:float list) : Result<float list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not(System.Double.IsFinite bandwidth) || bandwidth<=0. then Error [InvalidObservation "KDE bandwidth must be finite and positive."]
        elif samples.Length>1000000 || positions.Length>1000000 || int64 samples.Length*int64 positions.Length>100000000L then
            Error [BudgetExceeded "KDE exceeds the sample/query work budget."]
        elif samples |> List.exists(fun (position,weight) -> not(System.Double.IsFinite position && System.Double.IsFinite weight) || weight<0.)
             || positions |> List.exists(System.Double.IsFinite >> not) then
            Error [InvalidObservation "KDE samples and query positions must be finite; weights must be nonnegative."]
        else
            let largest=samples |> List.fold(fun largest (_,weight) -> max largest weight) 0.
            if largest=0. then Ok(List.replicate positions.Length 0.)
            else
                let scaledTotal=samples |> List.sumBy(fun (_,weight) -> weight/largest)
                let logTotal=log largest+log scaledTotal
                let logFactor= -0.5*log(2.*System.Math.PI)-log bandwidth
                let normalized=samples |> List.choose(fun (position,weight) ->
                    if weight=0. then None else Some(position,log weight-logTotal+logFactor))
                let values=positions |> List.map(fun query ->
                    token.ThrowIfCancellationRequested()
                    let mutable largestTerm=System.Double.NegativeInfinity
                    let mutable sum=0.
                    let mutable index=0
                    for position,logWeight in normalized do
                        if index%64=0 then token.ThrowIfCancellationRequested()
                        index<-index+1
                        let delta=query-position
                        let z=if System.Double.IsFinite delta then delta/bandwidth else query/bandwidth-position/bandwidth
                        let term=logWeight-0.5*z*z
                        if System.Double.IsFinite term then
                            if term>largestTerm then
                                sum<-sum*exp(largestTerm-term)+1.
                                largestTerm<-term
                            else sum<-sum+exp(term-largestTerm)
                    if sum=0. then 0. else exp(largestTerm+log sum))
                if values |> List.exists(System.Double.IsFinite >> not) then Error [InvalidObservation "KDE density exceeds the finite numeric range."]
                else Ok values
    /// <summary>Runs <c>kde1dWithCancellation</c> without cancellation.</summary>
    let kde1d bandwidth samples positions = kde1dWithCancellation System.Threading.CancellationToken.None bandwidth samples positions

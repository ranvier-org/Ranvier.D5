namespace Ranvier.D5.Charts

open Ranvier.D5

/// <summary>A chart whose marks each aggregate many observations; Membership maps each mark's semantic key (its only DataIds entry) to the source observation IDs.</summary>
type AggregateChartOutput = { Chart:ChartOutput; Membership:Map<string,string list> }
/// <summary>Aggregate density chart specification: Color maps a weight or density to paint, Selected holds mark keys to outline, and MaxMarks caps visible marks.</summary>
type DensityChartSpec = { Frame:ChartFrame; Color:float -> Paint; Selected:Set<string>; MaxMarks:int }

/// <summary>Defaults for density chart specifications.</summary>
[<RequireQualifiedAccess>]
module DensityChartSpec =
    /// <summary>Creates a specification with no selection and a 100000-mark budget.</summary>
    let create frame color = {Frame=frame;Color=color;Selected=Set.empty;MaxMarks=100000}

/// <summary>Hexbin and grid heatmap charts over aggregated observations.</summary>
[<RequireQualifiedAccess>]
module DensityChart =
    let private number (value:float)=value.ToString("R",System.Globalization.CultureInfo.InvariantCulture)
    /// <summary>As hexbin, but checks the token during aggregation and composition and throws OperationCanceledException when it is cancelled.</summary>
    let hexbinWithCancellation (token:System.Threading.CancellationToken) (spec:DensityChartSpec) (config:HexbinConfig) points : Result<AggregateChartOutput,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if spec.MaxMarks<1 then Error [InvalidObservation "Mark budget must be positive."]
        else
            Hexbin.aggregateWithCancellation token {config with MaxBins=min config.MaxBins spec.MaxMarks} points
            |> Result.bind(fun bins ->
                Recipe.run spec.Frame id [] (fun bounds ->
                    let scale=min (Bounds.width bounds/Bounds.width config.Bounds) (Bounds.height bounds/Bounds.height config.Bounds)
                    let originX=Bounds.x config.Bounds
                    let originY=Bounds.y config.Bounds
                    let offsetX=Bounds.x bounds+(Bounds.width bounds-Bounds.width config.Bounds*scale)*0.5
                    let offsetY=Bounds.y bounds+(Bounds.height bounds-Bounds.height config.Bounds*scale)*0.5
                    let inline project (point:Point)=Point.create (offsetX+(point.X-originX)*scale) (offsetY+(point.Y-originY)*scale)
                    let scope=Recipe.key ["hex";number originX;number originY;number config.Radius]
                    let binArr=List.toArray bins
                    let n=binArr.Length
                    let mutable marks=[]
                    let mutable semantics=[]
                    let mutable hits=[]
                    let mutable memPairs=[]
                    let hasSelection=not spec.Selected.IsEmpty
                    for idx=n-1 downto 0 do
                        if (idx &&& 63)=0 then token.ThrowIfCancellationRequested()
                        let bin=binArr[idx]
                        let struct(q,r)=bin.Key
                        let qText=q.ToString(System.Globalization.CultureInfo.InvariantCulture)
                        let rText=r.ToString(System.Globalization.CultureInfo.InvariantCulture)
                        let key=Recipe.key3 scope qText rText
                        let rawVerts=Hexbin.polygon bin
                        let v0=project rawVerts[0]
                        let v1=project rawVerts[1]
                        let v2=project rawVerts[2]
                        let v3=project rawVerts[3]
                        let v4=project rawVerts[4]
                        let v5=project rawVerts[5]
                        if v0=v1 || v0=v2 || v0=v3 || v0=v4 || v0=v5 ||
                           v1=v2 || v1=v3 || v1=v4 || v1=v5 ||
                           v2=v3 || v2=v4 || v2=v5 ||
                           v3=v4 || v3=v5 || v4=v5 then
                            invalidArg "frame" "Hexagon geometry collapses at the requested coordinate precision."
                        let vertices=[v0;v1;v2;v3;v4;v5]
                        let path=Path.ofCommands [MoveTo v0;LineTo v1;LineTo v2;LineTo v3;LineTo v4;LineTo v5;Close]
                        let label=System.String.Concat [|"Hexagon ";qText;", ";rText;": ";bin.Weight.ToString("F3",System.Globalization.CultureInfo.InvariantCulture);" (";bin.Members.Length.ToString(System.Globalization.CultureInfo.InvariantCulture);" observations)"|]
                        let mark=Mark.path key path |> Mark.withFill(spec.Color bin.Weight) |> Mark.withLabel(Some label)
                        let mark=if hasSelection && spec.Selected.Contains key then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                        let keys=[key]
                        let region=HitRegion([{Outer=vertices;Holes=[]}],[],project bin.Center)
                        let left=min v0.X (min v1.X (min v2.X (min v3.X (min v4.X v5.X))))
                        let right=max v0.X (max v1.X (max v2.X (max v3.X (max v4.X v5.X))))
                        let top=min v0.Y (min v1.Y (min v2.Y (min v3.Y (min v4.Y v5.Y))))
                        let bottom=max v0.Y (max v1.Y (max v2.Y (max v3.Y (max v4.Y v5.Y))))
                        let extent=Bounds.create left top (right-left) (bottom-top)
                        marks<-mark::marks
                        semantics<-Recipe.semantic key keys label::semantics
                        hits<-{Recipe.hit key keys region with Clip=Some extent}::hits
                        memPairs<-(key,bin.Members)::memPairs
                    token.ThrowIfCancellationRequested()
                    {Chart=Recipe.output spec.Frame bounds marks semantics hits []
                     Membership=Map.ofList memPairs}))

    /// <summary>As heatmap, but checks the token while counting and composing cells and throws OperationCanceledException when it is cancelled.</summary>
    let heatmapWithCancellation (token:System.Threading.CancellationToken) (spec:DensityChartSpec) (grid:DensityGrid) : Result<AggregateChartOutput,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let columns,rows=DensityGrid.columns grid,DensityGrid.rows grid
        let mutable count=0
        for row in 0..rows-1 do
            for column in 0..columns-1 do
                if (column &&& 63)=0 then token.ThrowIfCancellationRequested()
                if DensityGrid.value column row grid |> Option.isSome then count<-count+1
        if spec.MaxMarks<1 then Error [InvalidObservation "Mark budget must be positive."]
        elif count>spec.MaxMarks then Error [BudgetExceeded "Observed cells exceed the visible mark budget."]
        else Recipe.run spec.Frame id [] (fun bounds ->
            let extent=DensityGrid.bounds grid
            let scope=Recipe.key ["grid";number(Bounds.x extent);number(Bounds.y extent);number(Bounds.width extent);number(Bounds.height extent);string columns;string rows]
            let bx=Bounds.x bounds
            let by=Bounds.y bounds
            let bw=Bounds.width bounds
            let bh=Bounds.height bounds
            let invCols=1./float columns
            let invRows=1./float rows
            let colStrs=Array.init columns string
            let rowStrs=Array.init rows string
            let hasSelection=not spec.Selected.IsEmpty
            let mutable marksRev=[]
            let mutable semsRev=[]
            let mutable hitsRev=[]
            let mutable memPairs=[]
            for row=0 to rows-1 do
                let rStr=rowStrs[row]
                let y=by+float row*invRows*bh
                let bottom=by+float(row+1)*invRows*bh
                for column=0 to columns-1 do
                    if (column &&& 63)=0 then token.ThrowIfCancellationRequested()
                    match DensityGrid.value column row grid with
                    | None -> ()
                    | Some value ->
                        let cStr=colStrs[column]
                        let key=Recipe.key3 scope cStr rStr
                        let x=bx+float column*invCols*bw
                        let right=bx+float(column+1)*invCols*bw
                        if right<=x || bottom<=y then invalidArg "frame" "Cell geometry collapses at the requested coordinate precision."
                        let cell=Bounds.create x y (right-x) (bottom-y)
                        let label=System.String.Concat("Cell ",cStr,", ",rStr,": ",Recipe.number value)
                        let mark=Recipe.rectangle key label (spec.Color value) cell
                        let mark=if hasSelection && spec.Selected.Contains key then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                        let keys=[key]
                        marksRev<-mark::marksRev
                        semsRev<-Recipe.semantic key keys label::semsRev
                        hitsRev<-Recipe.hit key keys (HitBounds cell)::hitsRev
                        memPairs<-(key,DensityGrid.members column row grid)::memPairs
            token.ThrowIfCancellationRequested()
            {Chart=Recipe.output spec.Frame bounds (List.rev marksRev) (List.rev semsRev) (List.rev hitsRev) []
             Membership=Map.ofList memPairs})

    /// <summary>Draws one rectangle per grid cell with a value, stretched to fill the plot area; more valued cells than MaxMarks is a BudgetExceeded error.</summary>
    let heatmap spec grid = heatmapWithCancellation System.Threading.CancellationToken.None spec grid
    /// <summary>Aggregates points into hexagons (at most MaxMarks bins) fitted uniformly into the plot area, each with a polygon hit region; geometry that collapses at screen precision is a typed error.</summary>
    let hexbin spec config points = hexbinWithCancellation System.Threading.CancellationToken.None spec config points

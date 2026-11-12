namespace Ranvier.D5.Charts

open Ranvier.D5

type AggregateChartOutput = { Chart:ChartOutput; Membership:Map<string,string list> }
type DensityChartSpec = { Frame:ChartFrame; Color:float -> Paint; Selected:Set<string>; MaxMarks:int }

[<RequireQualifiedAccess>]
module DensityChartSpec =
    let create frame color = {Frame=frame;Color=color;Selected=Set.empty;MaxMarks=100000}

[<RequireQualifiedAccess>]
module DensityChart =
    let private number (value:float)=value.ToString("R",System.Globalization.CultureInfo.InvariantCulture)
    let hexbinWithCancellation (token:System.Threading.CancellationToken) (spec:DensityChartSpec) (config:HexbinConfig) points : Result<AggregateChartOutput,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if spec.MaxMarks<1 then Error [InvalidObservation "Mark budget must be positive."]
        else
            Hexbin.aggregateWithCancellation token {config with MaxBins=min config.MaxBins spec.MaxMarks} points
            |> Result.bind(fun bins ->
                Recipe.run spec.Frame id [] (fun bounds ->
                    let scale=min (Bounds.width bounds/Bounds.width config.Bounds) (Bounds.height bounds/Bounds.height config.Bounds)
                    let offsetX=Bounds.x bounds+(Bounds.width bounds-Bounds.width config.Bounds*scale)/2.
                    let offsetY=Bounds.y bounds+(Bounds.height bounds-Bounds.height config.Bounds*scale)/2.
                    let project (point:Point)=Point.create (offsetX+(point.X-Bounds.x config.Bounds)*scale) (offsetY+(point.Y-Bounds.y config.Bounds)*scale)
                    let scope=Recipe.key ["hex";number(Bounds.x config.Bounds);number(Bounds.y config.Bounds);number config.Radius]
                    let cells=bins |> List.map(fun bin ->
                        token.ThrowIfCancellationRequested()
                        let struct(q,r)=bin.Key
                        let key=Recipe.key [scope;string q;string r]
                        let vertices=Hexbin.polygon bin |> List.map project
                        let mutable remaining=vertices
                        while not remaining.IsEmpty do
                            if List.contains remaining.Head remaining.Tail then invalidArg "frame" "Hexagon geometry collapses at the requested coordinate precision."
                            remaining<-remaining.Tail
                        let path=Path.ofCommands(MoveTo vertices.Head::(vertices.Tail |> List.map LineTo)@[Close])
                        let label=sprintf "Hexagon %d, %d: %s (%d observations)" q r (Recipe.number bin.Weight) bin.Members.Length
                        let mark=Mark.path key path |> Mark.withFill(spec.Color bin.Weight) |> Mark.withLabel(Some label)
                        let mark=if spec.Selected.Contains key then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                        let region=HitRegion([{Outer=vertices;Holes=[]}],[],project bin.Center)
                        key,bin.Members,mark,Recipe.semantic key [key] label,Recipe.hit key [key] region)
                    token.ThrowIfCancellationRequested()
                    {Chart=Recipe.output spec.Frame bounds (cells |> List.map(fun (_,_,m,_,_) -> m)) (cells |> List.map(fun (_,_,_,s,_) -> s)) (cells |> List.map(fun (_,_,_,_,h) -> h)) []
                     Membership=cells |> List.map(fun (key,members,_,_,_) -> key,members) |> Map.ofList}))

    let heatmapWithCancellation (token:System.Threading.CancellationToken) (spec:DensityChartSpec) (grid:DensityGrid) : Result<AggregateChartOutput,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let columns,rows=DensityGrid.columns grid,DensityGrid.rows grid
        let mutable count=0
        for row in 0..rows-1 do
            for column in 0..columns-1 do
                if column%64=0 then token.ThrowIfCancellationRequested()
                if DensityGrid.value column row grid |> Option.isSome then count<-count+1
        if spec.MaxMarks<1 then Error [InvalidObservation "Mark budget must be positive."]
        elif count>spec.MaxMarks then Error [BudgetExceeded "Observed cells exceed the visible mark budget."]
        else Recipe.run spec.Frame id [] (fun bounds ->
            let extent=DensityGrid.bounds grid
            let scope=Recipe.key ["grid";number(Bounds.x extent);number(Bounds.y extent);number(Bounds.width extent);number(Bounds.height extent);string columns;string rows]
            let cells=[
                for row in 0..rows-1 do
                    for column in 0..columns-1 do
                        token.ThrowIfCancellationRequested()
                        match DensityGrid.value column row grid with
                        | None -> ()
                        | Some value ->
                            let key=Recipe.key [scope;string column;string row]
                            let x=Bounds.x bounds+float column/float columns*Bounds.width bounds
                            let y=Bounds.y bounds+float row/float rows*Bounds.height bounds
                            let right=Bounds.x bounds+float(column+1)/float columns*Bounds.width bounds
                            let bottom=Bounds.y bounds+float(row+1)/float rows*Bounds.height bounds
                            if right<=x || bottom<=y then invalidArg "frame" "Cell geometry collapses at the requested coordinate precision."
                            let cell=Bounds.create x y (right-x) (bottom-y)
                            let label=sprintf "Cell %d, %d: %s" column row (Recipe.number value)
                            let mark=Recipe.rectangle key label (spec.Color value) cell
                            let mark=if spec.Selected.Contains key then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                            yield key,DensityGrid.members column row grid,mark,Recipe.semantic key [key] label,Recipe.hit key [key] (HitBounds cell)]
            token.ThrowIfCancellationRequested()
            {Chart=Recipe.output spec.Frame bounds (cells |> List.map(fun (_,_,m,_,_) -> m)) (cells |> List.map(fun (_,_,_,s,_) -> s)) (cells |> List.map(fun (_,_,_,_,h) -> h)) []
             Membership=cells |> List.map(fun (key,members,_,_,_) -> key,members) |> Map.ofList})

    let heatmap spec grid = heatmapWithCancellation System.Threading.CancellationToken.None spec grid
    let hexbin spec config points = hexbinWithCancellation System.Threading.CancellationToken.None spec config points

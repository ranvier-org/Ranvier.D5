namespace Ranvier.D5.Charts

open Ranvier.D5

[<RequireQualifiedAccess>]
module ContourChart =
    exception private CompositionAbort of LayoutError

    let private rings (polygons:HitPolygon list) =
        polygons |> List.collect(fun polygon -> polygon.Outer::polygon.Holes)

    let private interior tick included excluded =
        let boundaries=rings included @ rings excluded
        let vertices=boundaries |> List.collect id
        tick (int64 vertices.Length * int64(ceil(System.Math.Log2(float(max 2 vertices.Length)))))
        let ys=vertices |> List.map(fun p -> p.Y) |> List.distinct |> List.sort |> List.toArray
        let edges=boundaries |> List.collect List.pairwise |> List.toArray
        let geometry=HitRegion(included,excluded,Point.create 0. 0.)
        let mutable answer=None
        let mutable row=0
        while answer.IsNone && row<ys.Length-1 do
            let y=ys[row]+(ys[row+1]-ys[row])/2.
            tick (int64 edges.Length)
            if y>ys[row] && y<ys[row+1] then
                let xs=edges |> Array.choose(fun (a,b) ->
                    if (a.Y<=y && b.Y>y) || (b.Y<=y && a.Y>y) then
                        Some(a.X+(b.X-a.X)*((y-a.Y)/(b.Y-a.Y)))
                    else None) |> Array.distinct |> Array.sort
                let mutable column=0
                while answer.IsNone && column<xs.Length-1 do
                    let x=xs[column]+(xs[column+1]-xs[column])/2.
                    tick (int64 vertices.Length)
                    if x>xs[column] && x<xs[column+1] then
                        let point=Point.create x y
                        if HitTest.contains point geometry then answer<-Some point
                    column<-column+1
            row<-row+1
        answer

    let private build (token:System.Threading.CancellationToken) bands (spec:DensityChartSpec) (config:ContourConfig) (grid:DensityGrid) =
        token.ThrowIfCancellationRequested()
        if spec.MaxMarks<1 then Error [InvalidObservation "Mark budget must be positive."]
        else
          Contour.computeWithCancellation token config grid
          |> Result.bind(fun polygons ->
            try
              Recipe.run spec.Frame id [] (fun bounds ->
                let mutable work=0L
                let tick amount =
                    token.ThrowIfCancellationRequested()
                    if amount>config.MaxWork-work then raise(CompositionAbort(BudgetExceeded "Contour composition exceeds the work budget."))
                    work<-work+amount
                let extent=DensityGrid.bounds grid
                let number (value:float)=value.ToString("R",System.Globalization.CultureInfo.InvariantCulture)
                let scope=Recipe.key [(if bands then "isoband" else "contour");number(Bounds.x extent);number(Bounds.y extent);number(Bounds.width extent);number(Bounds.height extent)]
                let project (p:Point)=
                    Point.create (Bounds.x bounds+(p.X-Bounds.x extent)/Bounds.width extent*Bounds.width bounds)
                                 (Bounds.y bounds+(p.Y-Bounds.y extent)/Bounds.height extent*Bounds.height bounds)
                let ring points =
                    tick (int64(List.length points))
                    let output=points |> List.mapi(fun index point ->
                        if index%64=0 then token.ThrowIfCancellationRequested()
                        project point)
                    if output |> List.pairwise |> List.exists(fun (a,b) -> a=b) then
                        invalidArg "frame" "Contour vertices collapse at the requested coordinate precision."
                    output
                let groups=polygons |> List.groupBy(fun p -> p.Threshold) |> List.map(fun (level,polys) ->
                    level,polys |> List.map(fun p -> {HitPolygon.Outer=ring p.Outer;Holes=List.map ring p.Holes})) |> Map.ofList
                let levels=config.Thresholds |> List.sort |> List.toArray
                let marks,semantics,hits,membership=ResizeArray(),ResizeArray(),ResizeArray(),ResizeArray()
                let columns,rows=DensityGrid.columns grid,DensityGrid.rows grid
                for index in 0..levels.Length-1 do
                    token.ThrowIfCancellationRequested()
                    let level=levels[index]
                    let upper=if bands && index+1<levels.Length then Some levels[index+1] else None
                    let included=Map.tryFind level groups |> Option.defaultValue []
                    let excluded=upper |> Option.bind(fun value -> Map.tryFind value groups) |> Option.defaultValue []
                    if not included.IsEmpty then
                        match interior tick included excluded with
                        | None when included=excluded -> ()
                        | None -> invalidArg "frame" "Contour band has no representable interior selection point."
                        | Some anchor ->
                            if marks.Count>=spec.MaxMarks then raise(CompositionAbort(BudgetExceeded "Contour levels exceed the visible mark budget."))
                            let key=Recipe.key [scope;number level;upper |> Option.map number |> Option.defaultValue "unbounded"]
                            let paths=
                                (rings included @ (rings excluded |> List.map List.rev))
                                |> List.map(fun ring -> Path.ofCommands(MoveTo ring.Head::(ring.Tail |> List.map LineTo)@[Close]))
                            let label=
                                match upper with
                                | Some limit -> sprintf "Density %s to %s (upper excluded)" (Recipe.number level) (Recipe.number limit)
                                | None -> sprintf "Density at least %s" (Recipe.number level)
                            let mark=Path.concat paths |> Mark.path key |> Mark.withFill(spec.Color level) |> Mark.withLabel(Some label)
                            let mark=if spec.Selected.Contains key then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                            marks.Add mark
                            semantics.Add(Recipe.semantic key [key] label)
                            hits.Add(Recipe.hit key [key] (HitRegion(included,excluded,anchor)))
                            tick (int64 columns*int64 rows)
                            let members=[
                                for row in 0..rows-1 do
                                    for column in 0..columns-1 do
                                        if column%64=0 then token.ThrowIfCancellationRequested()
                                        let source=DensityGrid.members column row grid
                                        if not source.IsEmpty then
                                            match DensityGrid.value column row grid with
                                            | Some value when value>=level && (upper |> Option.forall(fun limit -> value<limit)) -> yield! source
                                            | _ -> ()]
                            membership.Add(key,members)
                token.ThrowIfCancellationRequested()
                {Chart=Recipe.output spec.Frame bounds (List.ofSeq marks) (List.ofSeq semantics) (List.ofSeq hits) []
                 Membership=Map.ofSeq membership})
            with CompositionAbort error -> Error [error])

    let contoursWithCancellation token spec config grid = build token false spec config grid
    let isobandsWithCancellation token spec config grid = build token true spec config grid
    let contours spec config grid = contoursWithCancellation System.Threading.CancellationToken.None spec config grid
    let isobands spec config grid = isobandsWithCancellation System.Threading.CancellationToken.None spec config grid

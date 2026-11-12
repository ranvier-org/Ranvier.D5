module Ranvier.D5.Gallery.QuadtreeGallery

open Ranvier.D5

let private points=[for i in 0..159 -> Point.create (60.+float((i*197)%680)) (90.+float((i*113)%360))]
let private tree=Quadtree.build id (fun _ -> 1.) points
let private dot key color size point =
    Shape.symbol Symbol.Circle size |> Mark.path key |> Mark.withFill color |> Mark.withTransform(Transform.translate point.X point.Y)
let scene query =
    let range={Min=Point.create (query.X-80.) (query.Y-60.);Max=Point.create (query.X+80.) (query.Y+60.)}
    let selected=Quadtree.within range tree |> Set.ofList
    let nearest=Quadtree.nearest query infinity tree
    let cells=ResizeArray<Mark>()
    Quadtree.visit (fun cell ->
        if cell.Depth<=2 then
            let e=cell.Extent
            cells.Add(Shape.rectangle(Bounds.create e.Min.X e.Min.Y (e.Max.X-e.Min.X) (e.Max.Y-e.Min.Y))
                      |> Mark.path ("cell/"+string cells.Count) |> Mark.withFill Paint.none |> Mark.withStroke (Paint.rgb 205 213 225) 1.)
        cell.Depth<2) tree
    Scene.create 800. 570. "Quadtree range and nearest search" (
        [Mark.text "title" (Point.create 30. 35.) "Immutable quadtree: range and nearest queries" |> Mark.withFontSize 21.]
        @List.ofSeq cells
        @[Shape.rectangle(Bounds.create range.Min.X range.Min.Y 160. 120.) |> Mark.path "range" |> Mark.withFill(Paint.rgb 70 150 220) |> Mark.withOpacity 0.15]
        @(points |> List.mapi(fun i point -> dot ("point/"+string i) (if selected.Contains point then Paint.rgb 35 115 200 else Paint.rgb 160 170 185) 40. point))
        @(nearest |> Option.map(fun point -> dot "nearest" (Paint.rgb 225 90 35) 130. point) |> Option.toList)
        @[Shape.symbol Symbol.Diamond 90. |> Mark.path "query" |> Mark.withFill(Paint.rgb 40 45 60) |> Mark.withTransform(Transform.translate query.X query.Y)
          Mark.text "legend" (Point.create 30. 505.) "Blue: inclusive range | Orange: nearest | Diamond: query" |> Mark.withFontSize 15.
          Mark.text "count" (Point.create 30. 535.) (sprintf "%d of %d points in range; gray outlines show aggregate cells" selected.Count (Quadtree.count tree)) |> Mark.withFontSize 14.])

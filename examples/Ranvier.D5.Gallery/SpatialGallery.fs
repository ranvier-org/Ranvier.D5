module Ranvier.D5.Gallery.SpatialGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let private points=[|for i in 0..29 -> Point.create (40.+float((i*197)%320)) (110.+float((i*113)%300))|]
let private mesh=Delaunay.build (Array.toList points)
let private cells=Voronoi.clip (Bounds.create 20. 85. 360. 370.) mesh
let private outline vertices =
    match vertices with
    | [] -> Path.empty
    | first::rest -> Path.ofCommands(MoveTo first::(rest |> List.map LineTo)@[Close])
let private triangles=
    Delaunay.triangles mesh |> Array.chunkBySize 3 |> Array.mapi(fun i triangle ->
        outline [for site in triangle -> points[site]] |> Mark.path("triangle/"+string i)
        |> Mark.withFill Paint.none |> Mark.withStroke (Paint.rgb 180 195 215) 1.) |> Array.toList
let private scene selected =
    let dot key point color=Shape.symbol Symbol.Circle 55. |> Mark.path key |> Mark.withFill color |> Mark.withTransform(Transform.translate point.X point.Y)
    Scene.create 800. 570. "Delaunay and clipped Voronoi picking" (
        [Mark.text "title-left" (Point.create 25. 35.) "Delaunay mesh" |> Mark.withFontSize 22.
         Mark.text "title-right" (Point.create 425. 35.) "Clipped Voronoi overlay" |> Mark.withFontSize 22.]
        @triangles
        @(cells |> List.map(fun cell ->
            outline cell.Vertices |> Mark.path("cell/"+string cell.Site)
            |> Mark.withTransform(Transform.translate 400. 0.)
            |> Mark.withFill(if selected=Some cell.Site then Paint.rgb 250 185 115 else Paint.rgb 220 235 248)
            |> Mark.withStroke (Paint.rgb 100 140 180) 1.))
        @(points |> Array.mapi(fun i point -> dot ("left/"+string i) point (if selected=Some i then Paint.rgb 225 85 35 else Paint.rgb 35 100 170)) |> Array.toList)
        @(points |> Array.mapi(fun i point -> dot ("right/"+string i) (Point.create (point.X+400.) point.Y) (Paint.rgb 35 100 170)) |> Array.toList)
        @[Mark.text "help" (Point.create 25. 500.) "Move or click in either plot to pick the nearest original site." |> Mark.withFontSize 15.
          Mark.text "keyboard" (Point.create 25. 528.) "Focus the chart: arrows select sites; Escape clears." |> Mark.withFontSize 14.
          Mark.text "selection" (Point.create 425. 528.) (selected |> Option.map(fun i -> "Selected site: "+string i) |> Option.defaultValue "No selection") |> Mark.withFontSize 14.])

let mount (host:WebCoreHost) (graph:Graph) =
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let selected=Api.createSignal(Some 12)
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render Renderer.passive (fun () -> scene selected.Value)) context.Backend)
        host.Cycle()
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        svg.setAttribute("tabindex","0")
        let pick x y =
            InputAdapter.clientToPlot svg (Point.create x y) |> Option.iter(fun point ->
                let point=if point.X>=420. then Point.create (point.X-400.) point.Y else point
                if Bounds.contains point (Bounds.create 20. 85. 360. 370.) then
                    graph.Run(fun () -> selected.Value<-Delaunay.nearest point mesh))
        let pointer (event:Ranvier.Dom.Event) = let e=event :?> Ranvier.Dom.PointerEvent in pick e.clientX e.clientY
        let click (event:Ranvier.Dom.Event) = let e=event :?> Ranvier.Dom.MouseEvent in pick e.clientX e.clientY
        let keyboard (event:Ranvier.Dom.Event) =
            let e=event :?> Ranvier.Dom.KeyboardEvent
            graph.Run(fun () ->
                match e.key with
                | "ArrowRight" | "ArrowDown" -> selected.Value<-Some((Option.defaultValue -1 selected.Value+1)%points.Length);e.preventDefault()
                | "ArrowLeft" | "ArrowUp" -> selected.Value<-Some((Option.defaultValue 0 selected.Value+points.Length-1)%points.Length);e.preventDefault()
                | "Escape" -> selected.Value<-None
                | _ -> ())
        svg.addEventListener("pointermove",pointer)
        svg.addEventListener("click",click)
        svg.addEventListener("keydown",keyboard)
        Api.onCleanup(fun () ->
            svg.removeEventListener("pointermove",pointer)
            svg.removeEventListener("click",click)
            svg.removeEventListener("keydown",keyboard)
            mounted.Dispose();container.remove())
        owner,selected))

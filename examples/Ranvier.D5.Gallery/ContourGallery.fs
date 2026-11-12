module Ranvier.D5.Gallery.ContourGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let scene (polygons:ContourPolygon list) =
    let palette=Chromatic.sequential Ranvier.Anime.ColorInterpolation.Lab Ranvier.Anime.GamutPolicy.Clip Chromatic.blues
    let marks=polygons |> List.mapi(fun index polygon ->
        let commands=polygon.Outer::polygon.Holes |> List.collect(fun ring -> MoveTo ring.Head::(ring.Tail |> List.map LineTo) @ [Close])
        Path.ofCommands commands |> Mark.path(string index)
        |> Mark.withFill(palette(min 1. (polygon.Threshold/40.)) |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip)
        |> Mark.withStroke (Paint.rgb 40 90 150) 0.5)
    Scene.create 800. 570. "Density contours"
        ([Mark.text "title" (Point.create 24. 28.) "Density contours / 100,000 observations" |> Mark.withFontSize 22.
          Mark.text "help" (Point.create 24. 53.) "Left / Right: adjust levels; density and contour topology run in the owned worker." |> Mark.withFontSize 14.]
         @ marks @ [Mark.text "status" (Point.create 24. 550.) (sprintf "%d polygons | closed shells and holes | bilinear saddle decisions" polygons.Length) |> Mark.withFontSize 13.])

let mount (host:WebCoreHost) (graph:Graph) =
    let points=DensityGallery.observations() |> List.map(fun point -> {point with Position=Point.create (24.+point.Position.X*7.52) (80.+point.Position.Y*4.44)})
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let multiplier=Api.createSignal 1.
        let worker=ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> multiplier.Value) (fun token factor ->
            let config={DensityConfig.create (Bounds.create 24. 80. 752. 444.) 128 80 with Bandwidth=12.}
            Density.gridWithCancellation token config points
            |> Result.bind(Contour.computeWithCancellation token (ContourConfig.create ([1.;5.;10.;20.;35.] |> List.map((*) factor))))
            |> Result.map(fun polygons -> factor,polygons))
        let output=Reactive.derive (fun () -> worker.LastGood) (fun result -> result |> Option.map snd |> Option.defaultValue [] |> scene)
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render Renderer.passive (fun () -> output.Value)) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
        host.Cycle()
        let svg=container.querySelector "svg"
        svg.setAttribute("tabindex","0")
        let key (event:Ranvier.Dom.Event)=
            let key=(event :?> Ranvier.Dom.KeyboardEvent).key
            if key="ArrowLeft" || key="ArrowRight" then
                event.preventDefault()
                graph.Run(fun () -> multiplier.Value<-max 0.25 (min 2. (multiplier.Value+(if key="ArrowRight" then 0.25 else -0.25))))
        svg.addEventListener("keydown",key)
        Api.onCleanup(fun () -> svg.removeEventListener("keydown",key))
        owner,multiplier,worker))

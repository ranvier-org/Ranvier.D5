module Ranvier.D5.Gallery.ReductionGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let private line key top color (samples:SeriesSample list) =
    samples |> List.map(fun sample -> sample.Y |> Option.map(fun value -> Point.create (410.+sample.X/99999.*365.) (top+150.-(value+1.)*30.)))
    |> Shape.line Curve.Linear |> Mark.path key |> Mark.withFill Paint.none |> Mark.withStroke color 1.2

let private scene (result:(int*HexBin list*SeriesSample list*SeriesSample list) option) =
    let heading=[Mark.text "title" (Point.create 24. 28.) "100,000 observations: hexbin and line reduction" |> Mark.withFontSize 22.
                 Mark.text "help" (Point.create 24. 53.) "Left / Right: change line budget; original IDs and gaps remain in the worker output." |> Mark.withFontSize 14.]
    let marks=
        match result with
        | None -> [Mark.text "pending" (Point.create 24. 90.) "Computing aggregates and samples..."]
        | Some(target,bins,minmax,lttb) ->
            let palette=Chromatic.sequential Ranvier.Anime.ColorInterpolation.Lab Ranvier.Anime.GamutPolicy.Clip Chromatic.blues
            let peak=bins |> List.map(fun bin -> bin.Weight) |> List.fold max 0.
            let transform=Transform.scale 3.4 3.4 |> Transform.thenApply(Transform.translate 24. 100.)
            let hexes=bins |> List.map(fun bin ->
                Hexbin.path bin |> Mark.path(sprintf "hex-%A" bin.Key) |> Mark.withTransform transform
                |> Mark.withFill(palette(if peak=0. then 0. else bin.Weight/peak) |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip)
                |> Mark.withStroke (Paint.rgb 230 240 250) 0.15)
            hexes @ [Mark.text "hex-label" (Point.create 24. 85.) (sprintf "%d weighted hexagons" bins.Length) |> Mark.withFontSize 15.
                     Mark.text "minmax-label" (Point.create 410. 95.) (sprintf "Min/max: %d original samples" minmax.Length) |> Mark.withFontSize 15.
                     Mark.text "lttb-label" (Point.create 410. 315.) (sprintf "LTTB: %d original samples" lttb.Length) |> Mark.withFontSize 15.
                     line "minmax" 110. (Paint.rgb 40 110 175) minmax
                     line "lttb" 330. (Paint.rgb 205 125 30) lttb
                     Mark.text "status" (Point.create 24. 550.) (sprintf "Budget %d | min/max retains bucket extrema | LTTB is visual reduction, never statistics" target) |> Mark.withFontSize 13.]
    Scene.create 800. 570. "Hexbin and sampling foundations" (heading @ marks)

let mount (host:WebCoreHost) (graph:Graph) =
    let points=DensityGallery.observations()
    let samples=[for i in 0..99999 -> {SeriesSample.Id=string i;X=float i;Y=Some(sin(float i*0.001)+(if i%997=0 then 3. else 0.))}]
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let target=Api.createSignal 200
        let worker=ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> target.Value) (fun token count ->
            Hexbin.aggregateWithCancellation token (HexbinConfig.create (Bounds.create 0. 0. 100. 100.) 4.) points
            |> Result.bind(fun bins ->
                Sampling.minMaxWithCancellation token (SamplingConfig.create count) samples
                |> Result.bind(fun minmax -> Sampling.lttbWithCancellation token (SamplingConfig.create count) samples |> Result.map(fun lttb -> count,bins,minmax,lttb))))
        let output=Reactive.derive (fun () -> worker.LastGood) scene
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
                graph.Run(fun () -> target.Value<-max 50 (min 1000 (target.Value+(if key="ArrowRight" then 50 else -50))))
        svg.addEventListener("keydown",key)
        Api.onCleanup(fun () -> svg.removeEventListener("keydown",key))
        owner,target,worker))

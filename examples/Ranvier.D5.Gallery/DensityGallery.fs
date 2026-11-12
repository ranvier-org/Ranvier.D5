module Ranvier.D5.Gallery.DensityGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let observations () =
    let random=Ranvier.Anime.Utils.createSeededRandom (Some 42.) (Some 0.) (Some 1.) (Some 12)
    [for i in 0..99999 ->
        let centerX,centerY=if i%3=0 then 70.,60. else 30.,35.
        let x=centerX+(random None None None+random None None None-1.)*25.
        let y=centerY+(random None None None+random None None None-1.)*25.
        {WeightedPoint.Id=string i;Position=Point.create x y;Weight=1.}]

let scene bandwidth (grid:DensityGrid option) =
    let title=Mark.text "title" (Point.create 24. 28.) "100,000 observations / 768 visible cells" |> Mark.withFontSize 22.
    let subtitle=Mark.text "subtitle" (Point.create 24. 53.) "Left / Right: change Gaussian bandwidth; computation runs in the owned worker." |> Mark.withFontSize 14.
    let palette=Chromatic.sequential Ranvier.Anime.ColorInterpolation.Lab Ranvier.Anime.GamutPolicy.Clip Chromatic.blues
    let cells,status=
        match grid with
        | None -> [],"Computing density..."
        | Some grid ->
            let peak=DensityGrid.values grid |> List.choose id |> List.fold max 0.
            let cells=[
                for row in 0..23 do
                    for column in 0..31 do
                        let value=DensityGrid.value column row grid |> Option.defaultValue 0.
                        yield Shape.rectangle(Bounds.create (24.+float column*23.5) (80.+float row*18.5) 23.5 18.5)
                              |> Mark.path(sprintf "cell-%d-%d" column row)
                              |> Mark.withFill(palette(if peak=0. then 0. else value/peak) |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip)]
            cells,sprintf "Bandwidth %.1f coordinate units | mass %.0f | measured zero is distinct from missing" bandwidth (DensityGrid.values grid |> List.choose id |> List.sum)
    Scene.create 800. 570. "Reactive Gaussian density"
        (title::subtitle::cells @ [Mark.text "status" (Point.create 24. 550.) status |> Mark.withFontSize 13.])

let mount (host:WebCoreHost) (graph:Graph) =
    let points=observations()
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let bandwidth=Api.createSignal 2.
        let worker=ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> bandwidth.Value) (fun token width ->
            let config={DensityConfig.create (Bounds.create 0. 0. 100. 100.) 32 24 with Bandwidth=width}
            Density.gridWithCancellation token config points |> Result.map(fun grid -> width,grid))
        let output=Reactive.derive (fun () -> worker.LastGood) (function Some(width,grid) -> scene width (Some grid) | None -> scene 0. None)
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
                graph.Run(fun () -> bandwidth.Value<-max 0. (min 10. (bandwidth.Value+(if key="ArrowRight" then 0.5 else -0.5))))
        svg.addEventListener("keydown",key)
        Api.onCleanup(fun () -> svg.removeEventListener("keydown",key))
        owner,bandwidth,worker))

module Ranvier.D5.Gallery.DensityRecipesGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let mount kind (host:WebCoreHost) (graph:Graph) =
    if not(List.contains kind ["heatmap";"hexbin";"contours";"isobands"]) then invalidArg "kind" "Unknown density recipe."
    let random=Ranvier.Anime.Utils.createSeededRandom (Some 73.) (Some 0.) (Some 1.) (Some 12)
    let points=[for i in 0..99999 do
                    let angle=random None None None*2.*Math.PI
                    let radius=24.+random None None None*8.
                    yield {WeightedPoint.Id=string i;Position=Point.create (50.+radius*cos angle) (50.+radius*sin angle);Weight=1.}]
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let frame={ChartFrame.create 800. 570. ("Reactive "+kind) with Margin={Top=80.;Right=24.;Bottom=50.;Left=24.}}
        let bounds=ChartLayout.plot frame |> Result.defaultWith(fun e -> failwithf "%A" e)
        let empty={Chart={Scene=Scene.create 800. 570. frame.Title [];PlotBounds=bounds;Semantics=[];HitTargets=[];Description=frame.Title};Membership=Map.empty}
        let bandwidth=Api.createSignal 2.
        let selected=Api.createSignal Set.empty
        let status=Api.createSignal "Computing…"
        let palette=Chromatic.sequential Ranvier.Anime.ColorInterpolation.Lab Ranvier.Anime.GamutPolicy.Clip Chromatic.blues
        let color value=palette(min 1. (max 0. (value/(if kind="hexbin" then 1500. else 150.)))) |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip
        let spec=DensityChartSpec.create frame color
        let worker=ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> bandwidth.Value) (fun token width ->
            let config={DensityConfig.create (Bounds.create 0. 0. 100. 100.) 64 64 with Bandwidth=width}
            let result=
                if kind="hexbin" then DensityChart.hexbinWithCancellation token spec (HexbinConfig.create config.Bounds width) points
                else
                    Density.gridWithCancellation token config points |> Result.bind(fun grid ->
                        if kind="heatmap" then DensityChart.heatmapWithCancellation token spec grid
                        elif kind="contours" then ContourChart.contoursWithCancellation token spec (ContourConfig.create [5.;25.;75.;125.]) grid
                        else ContourChart.isobandsWithCancellation token spec (ContourConfig.create [5.;25.;75.;125.]) grid)
            result |> Result.map(fun output -> width,output))
        let chart()=worker.LastGood |> Option.map snd |> Option.defaultValue empty
        let scene()=
            let output=(chart()).Chart
            let rec highlight = function
                | Leaf mark when selected.Value.Contains(Mark.key mark) -> Leaf(Mark.withStroke (Paint.rgb 230 140 20) 2.5 mark)
                | Leaf mark -> Leaf mark
                | Group(group,children) -> Group(group,List.map highlight children)
            Scene.compose frame.Viewport frame.Title (Scene.definitions output.Scene)
                (List.map highlight (Scene.nodes output.Scene) @
                 [Leaf(Mark.text "title" (Point.create 24. 28.) ("100,000 observations / "+kind) |> Mark.withFontSize 22.)
                  Leaf(Mark.text "help" (Point.create 24. 54.) ("Hover explores membership; arrows focus; Enter selects; + / - changes "+(if kind="hexbin" then "radius." else "bandwidth.")) |> Mark.withFontSize 13.)
                  Leaf(Mark.text "status" (Point.create 24. 548.) status.Value |> Mark.withFontSize 13.)])
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render Renderer.passive scene) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
        host.Cycle()
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        svg.setAttribute("tabindex","0")
        let input=ChartInteraction.attach svg (fun () -> {Mode=GestureMode.Rectangle;MinZoom=1.;MaxZoom=1.;WheelSensitivity=0.01;MaxLassoPoints=4}) (fun () -> (chart()).Chart) ignore
        Api.createEffect(fun () ->
            let state=input.Selection
            selected.Value<-Set.union state.Selected (Set.ofList(Option.toList state.Focused))
            let output=chart()
            let active=state.Hovered |> Option.orElse state.Focused
            let members=active |> Option.bind(fun key -> Map.tryFind key output.Membership) |> Option.defaultValue []
            status.Value<-sprintf "%d visible aggregates | %d original observations in active aggregate | %s %.1f" output.Membership.Count members.Length (if kind="hexbin" then "radius" else "bandwidth") bandwidth.Value) |> ignore
        let key (event:Ranvier.Dom.Event)=
            let value=(event :?> Ranvier.Dom.KeyboardEvent).key
            if value="+" || value="-" then
                event.preventDefault()
                graph.Run(fun () -> bandwidth.Value<-max (if kind="hexbin" then 0.5 else 0.) (min 6. (bandwidth.Value+(if value="+" then 0.5 else -0.5))))
        svg.addEventListener("keydown",key)
        Api.onCleanup(fun () -> svg.removeEventListener("keydown",key))
        owner,bandwidth,worker,input))

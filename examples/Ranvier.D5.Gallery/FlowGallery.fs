module Ranvier.D5.Gallery.FlowGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let mount kind (host:WebCoreHost) (graph:Graph) =
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let frame=ChartFrame.create 800. 570. (if kind="chord" then "Chord recipe: arrows focus; Enter selects" else "Drag a node vertically; arrows focus; Enter selects")
        let bounds=ChartLayout.plot frame |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let config={SankeyConfig.create bounds with NodeWidth=18.;NodePadding=35.;Iterations=0}
        let edge id source target value : FlowLink = {Id=id;Source=source;Target=target;Value=value}
        let topology=FlowGraph.create ["Solar";"Wind";"Grid";"Homes";"Industry";"Losses"]
                         [edge "solar" "Solar" "Grid" 35.;edge "wind" "Wind" "Grid" 40.
                          edge "homes" "Grid" "Homes" 25.;edge "industry" "Grid" "Industry" 30.;edge "loss" "Grid" "Losses" 5.]
                     |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let stages=Map.ofList ["Solar",0;"Wind",0;"Grid",1;"Homes",3;"Industry",3;"Losses",3]
        let initial=(if kind="alluvial" then Sankey.layoutStaged config stages topology else Sankey.layout config topology)
                    |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let state=Api.createSignal initial
        let selected=Api.createSignal Set.empty
        let colors=Map.ofList ["Solar",Paint.rgb 230 155 40;"Wind",Paint.rgb 40 165 125;"Grid",Paint.rgb 65 120 190]
        let color id=Map.tryFind id colors |> Option.defaultValue(Paint.rgb 65 120 190)
        let chart=Reactive.derive (fun () -> state.Value,selected.Value) (fun (layout,selection) ->
            (if kind="chord" then
                FlowChart.chord {ChordSpec.create frame with Color=color;Selected=selection;Layout={ChordConfig.defaultValue with Mode=ChordMode.Directed;PadAngle=0.08}}
                    ["Solar";"Wind";"Grid"] [[1.;2.;0.5];[4.;0.;3.];[0.5;1.;2.]]
             else FlowChart.sankey {FlowSpec.create frame with Color=color;Selected=selection} layout)
            |> Result.defaultWith(fun errors -> failwithf "%A" errors))
        let scene () =
            let output=chart.Value
            Scene.compose frame.Viewport frame.Title (Scene.definitions output.Scene)
                (Scene.nodes output.Scene @
                 [Leaf(Mark.text "help" (Point.create 15. 18.) frame.Title |> Mark.withFontSize 14.)
                  Leaf(Mark.text "status" (Point.create 15. 560.)
                      (if kind="alluvial" then "Explicit stages 0 → 1 → 3; empty stage 2 is retained. Drag keeps other nodes fixed."
                       elif kind="chord" then "Domain identities survive sorting and matrix permutation; selection is a Ranvier signal."
                       else "Flow widths stay fixed during drag; node height = max(inflow, outflow).") |> Mark.withFontSize 13.)])
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render Renderer.passive scene) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
        host.Cycle()
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        svg.setAttribute("tabindex","0")
        let input=
            if kind="chord" then
                ChartInteraction.attach svg (fun () -> {Mode=GestureMode.Drag;MinZoom=1.;MaxZoom=1.;WheelSensitivity=0.01;MaxLassoPoints=4}) (fun () -> chart.Value) ignore
            else FlowInteraction.attach svg (fun () -> chart.Value) (fun () -> config) (fun () -> state.Value)
                     (fun next -> state.Value<-next) (fun errors -> failwithf "%A" errors)
        Api.createEffect(fun () ->
            let selection=input.Selection
            selected.Value<-selection.Selected |> Set.union(Set.ofList(Option.toList selection.Focused))) |> ignore
        owner,state,input))

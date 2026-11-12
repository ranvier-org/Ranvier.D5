module Ranvier.D5.Gallery.NetworkGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let topology bubbles =
    let count=if bubbles then 120 else 1000
    let nodes=[for i in 0..count-1 ->
                   {GraphNode.create(string i) with
                       Position=Some(Point.create (70.+float(i%40)*17.) (80.+float(i/40)*17.))
                       Radius=if bubbles then 5.+float(i%10) else 2.5}]
    let links=
        if bubbles then [] else
        [for i in 0..count-1 do
            for jump in [1;40] ->
                {GraphLink.create (string i+"/"+string jump) (string i) (string((i+jump)%count)) with Distance=17.;Strength=0.05}]
    GraphData.create nodes links |> Result.defaultWith(fun errors -> failwithf "%A" errors)

let mount bubbles (host:WebCoreHost) (graph:Graph) =
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let topology=topology bubbles
        let config={ForceConfig.create(Point.create 400. 280.) with
                        CenterStrength=0.004;MaxSteps=240
                        X=Some(400.,if bubbles then 0.015 else 0.0005)
                        Y=Some(280.,if bubbles then 0.015 else 0.0005)
                        ManyBody=Some(ManyBodyConfig.create -0.3)
                        Collision=Some {CollisionConfig.create 0.8 with Iterations=2}}
        let initial=Force.initialize config (Seed.create 42u) topology |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let force=ReactiveForce.create (Ranvier.D5.Examples.HostDispatcher.forHost host) ForceChunk.defaultValue initial
        let selected=Api.createSignal Set.empty
        let palette=Array.init 10 (fun i -> Chromatic.categorical Chromatic.category10 i |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip)
        let spec={NetworkSpec.create(ChartFrame.create 800. 570. "Drag a node; arrows focus; Enter selects; Escape clears") with
                    Color=(fun id -> palette[(int id/12)%10]);LinkColor=Paint.rgb 205 213 223;LinkWidth=0.5;AggregateLinks=true}
        let chart=Reactive.derive (fun () -> force.Snapshot,selected.Value) (fun (snapshot,selection) ->
            let spec={spec with Selected=selection}
            (if bubbles then NetworkChart.bubbles spec snapshot else NetworkChart.network spec topology snapshot)
            |> Result.defaultWith(fun errors -> failwithf "%A" errors))
        let scene() =
            let output=chart.Value
            let snapshot=force.Snapshot
            Scene.compose spec.Frame.Viewport spec.Frame.Title (Scene.definitions output.Scene)
                (Scene.nodes output.Scene @
                 [Leaf(Mark.text "help" (Point.create 15. 18.) spec.Frame.Title |> Mark.withFontSize 13.)
                  Leaf(Mark.text "status" (Point.create 15. 560.)
                      (sprintf "%d nodes / %d links | worker step %d | finished %b" snapshot.Nodes.Length (GraphData.links topology).Length snapshot.Iteration snapshot.Finished)
                      |> Mark.withFontSize 13.)])
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render Renderer.passive scene) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
        host.Cycle()
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        svg.setAttribute("tabindex","0")
        let input=NetworkInteraction.attach svg (fun () -> chart.Value) force
        Api.createEffect(fun () ->
            let selection=input.Selection
            selected.Value<-selection.Selected |> Set.union(Set.ofList(Option.toList selection.Focused))) |> ignore
        owner,force,input))

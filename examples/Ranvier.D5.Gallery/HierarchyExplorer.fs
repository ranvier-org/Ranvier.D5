module Ranvier.D5.Gallery.HierarchyExplorer
open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let mount (host:WebCoreHost) (graph:Graph) =
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let frame=ChartFrame.create 800. 570. "Click: select; Shift-click: collapse; Ctrl-click: focus; Escape: reset"
        let initial=HierarchySpec.create frame
        let state=Api.createSignal initial
        let clock=Api.createSignal 0.
        let fallback=HierarchyChart.treemap initial HierarchyChartsGallery.root |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let worker=ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> state.Value) (fun token spec ->
            token.ThrowIfCancellationRequested()
            let result=HierarchyChart.treemap spec HierarchyChartsGallery.root
            token.ThrowIfCancellationRequested()
            result)
        let chart()=worker.LastGood |> Option.defaultValue fallback
        let plotMarks()=Scene.nodes (chart()).Scene |> List.collect(function Group(spec,nodes) when spec.Key="plot" -> nodes |> List.choose(function Leaf mark -> Some mark | _ -> None) | _ -> [])
        let animated=KeyedTransition.animate (fun () -> clock.Value) plotMarks 180. id (fun () -> false)
        let dispatch path eventName (event:Ranvier.Dom.Event) =
            graph.Run(fun () ->
                if eventName="keydown" then
                    let keyboard=event :?> Ranvier.Dom.KeyboardEvent
                    if keyboard.key="Escape" then state.Value<-initial
                elif eventName="click" then
                    match (chart()).Semantics |> List.tryFind(fun semantic -> path |> List.take (min 2 path.Length) = semantic.MarkPath) with
                    | Some semantic ->
                        let id=List.head semantic.DataIds
                        let mouse=event :?> Ranvier.Dom.MouseEvent
                        let previous=state.Value
                        if mouse.ctrlKey then state.Value<-{previous with Focus=Some id}
                        elif mouse.shiftKey then
                            state.Value<-{previous with Collapsed=if previous.Collapsed.Contains id then previous.Collapsed.Remove id else previous.Collapsed.Add id}
                        else state.Value<-{previous with Selected=if previous.Selected.Contains id then previous.Selected.Remove id else previous.Selected.Add id}
                    | None -> ())
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let scene()=
            let output=chart()
            let visible=output.Semantics |> List.collect(fun semantic -> semantic.DataIds) |> Set.ofList
            let branches=Hierarchy.descendants HierarchyChartsGallery.root |> List.choose(fun item -> if visible.Contains item.Id then item.Parent else None) |> Set.ofList
            let nodes=animated.Value |> List.map(function
                | Group(group,children) ->
                    let label=output.HitTargets |> List.tryPick(fun hit ->
                        match hit.Geometry,hit.DataIds with
                        | HitBounds bounds,[id] when hit.MarkPath=["plot";group.Key] && not(branches.Contains id) && Bounds.width bounds>float id.Length*7.+12. && Bounds.height bounds>24. ->
                            Some(Leaf(Mark.text "caption" (Point.create (Bounds.x bounds+6.) (Bounds.y bounds+18.)) id |> Mark.withFontSize 13.))
                        | _ -> None)
                    Group(group,children@(label |> Option.toList))
                | node -> node)
            Scene.compose frame.Viewport frame.Title (Scene.definitions output.Scene)
                [Leaf(Mark.text "help" (Point.create 15. 18.) frame.Title |> Mark.withFontSize 12.)
                 Group({GroupSpec.create "plot" with Clip=Some "plot-clip"},nodes)]
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render (Renderer.createScoped dispatch) scene) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
        host.Cycle()
        let svg=container.querySelector "svg"
        svg.setAttribute("tabindex","0")
        let key event=dispatch [] "keydown" event
        svg.addEventListener("keydown",key)
        Api.onCleanup(fun () -> svg.removeEventListener("keydown",key))
        owner,(fun now -> clock.Value<-now),state,worker))

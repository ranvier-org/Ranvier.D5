namespace Ranvier.D5.View

open System
open Ranvier
open Ranvier.D5

/// <summary>Handle to native listeners attached by InputAdapter.attach. Dispose it on the graph thread, or dispose the owner current at attach time, to cancel the gesture, remove the listeners and release pointer captures.</summary>
type OwnedInput internal (send:InputEvent->unit,dispose:unit->unit) =
    /// <summary>Feeds an event through the gesture state machine under the attaching owner and applies its commands; call on the graph thread. Ignored after disposal.</summary>
    member _.Send event=send event
    interface IDisposable with member _.Dispose()=dispose()

/// <summary>Translates native SVG pointer, wheel and keyboard events into gesture events for a graph owner.</summary>
[<RequireQualifiedAccess>]
module InputAdapter =
    /// <summary>Converts CSS client coordinates using the actual SVG screen matrix, including viewBox, ancestors and device scaling.</summary>
    let clientToPlot (element:Ranvier.Dom.SVGGraphicsElement) point =
        let matrix=element.getScreenCTM()
        if isNull matrix then None else
        Transform.create matrix.a matrix.b matrix.c matrix.d matrix.e matrix.f
        |> Transform.tryInverse |> Option.map (fun inverse -> Transform.apply inverse point)

    /// <summary>Attaches listeners in the current graph owner. Resize and data-exit notifications enter through Send; disposal releases captures and listeners.</summary>
    let attach (element:Ranvier.Dom.SVGGraphicsElement) readConfig readState writeState execute =
        let graph=Graph.Current
        let owner=Api.getOwner()
        let mutable disposed=false
        let captured=Collections.Generic.HashSet<int>()
        let sendCore event =
            if not disposed then
                let state,commands=Gesture.update (readConfig()) event (readState())
                Api.batch(fun () ->
                    writeState state
                    for command in commands do
                        match command with
                        | GestureCommand.Capture id -> element.setPointerCapture id;captured.Add id |> ignore
                        | GestureCommand.Release id ->
                            captured.Remove id |> ignore
                            if element.hasPointerCapture id then element.releasePointerCapture id
                        | _ -> execute command)
        let send event = graph.Run(fun () -> Api.runWithOwner owner (fun () -> Api.untrack(fun () -> sendCore event)))
        let pointer kind (event:Ranvier.Dom.Event) =
            match event with
            | :? Ranvier.Dom.PointerEvent as pointer ->
                if kind<>"pointerdown" || pointer.button=0s then
                    clientToPlot element (Point.create pointer.clientX pointer.clientY) |> Option.iter (fun point ->
                        let target =
                            match event.target with
                            | :? Ranvier.Dom.Element as target ->
                                let value=target.closest("[data-d5-key]")
                                if isNull value then None else
                                let mutable path=[value.getAttribute "data-d5-key"]
                                let mutable parent=value.parentElement
                                while not(isNull parent) do
                                    let group=parent.getAttribute "data-d5-group"
                                    if not(String.IsNullOrEmpty group) then path<-group::path
                                    parent<-parent.parentElement
                                Some path
                            | _ -> None
                        match kind with
                        | "pointerdown" -> send(InputEvent.Down(pointer.pointerId,point,target))
                        | "pointermove" -> send(InputEvent.Move(pointer.pointerId,point))
                        | _ -> send(InputEvent.Up(pointer.pointerId,point)))
            | _ -> ()
        let wheel (event:Ranvier.Dom.Event) =
            match event with
            | :? Ranvier.Dom.WheelEvent as wheel ->
                clientToPlot element (Point.create wheel.clientX wheel.clientY) |> Option.iter (fun point ->
                    let unit=if wheel.deltaMode=1u then 16. elif wheel.deltaMode=2u then element.getBoundingClientRect().height else 1.
                    event.preventDefault()
                    send(InputEvent.Wheel(point,wheel.deltaY*unit)))
            | _ -> ()
        let key (event:Ranvier.Dom.Event) =
            match event with
            | :? Ranvier.Dom.KeyboardEvent as key ->
                if List.contains key.key ["ArrowLeft";"ArrowRight";"ArrowUp";"ArrowDown";"Enter";" ";"Escape"] then event.preventDefault()
                send(InputEvent.Key key.key)
            | _ -> ()
        let listeners =
            ["pointerdown",pointer "pointerdown";"pointermove",pointer "pointermove";"pointerup",pointer "pointerup"
             "pointercancel",(fun _ -> send InputEvent.Cancel);"lostpointercapture",(fun (event:Ranvier.Dom.Event) -> match event with :? Ranvier.Dom.PointerEvent as pointer when captured.Remove pointer.pointerId -> send InputEvent.Cancel | _ -> ())
             "pointerleave",(fun _ -> send InputEvent.Leave);"focusout",(fun _ -> send InputEvent.FocusLost);"wheel",wheel;"keydown",key]
        for name,listener in listeners do element.addEventListener(name,listener)
        let dispose() =
            if not disposed then
                try send InputEvent.Cancel
                finally
                    disposed<-true
                    for name,listener in listeners do element.removeEventListener(name,listener)
                    for id in Seq.toArray captured do if element.hasPointerCapture id then element.releasePointerCapture id
                    captured.Clear()
        Api.onCleanup dispose
        new OwnedInput(send,dispose)

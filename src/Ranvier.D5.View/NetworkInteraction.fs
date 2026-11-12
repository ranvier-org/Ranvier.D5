namespace Ranvier.D5.View

open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts

/// <summary>Node dragging for force-directed network charts.</summary>
[<RequireQualifiedAccess>]
module NetworkInteraction =
    /// <summary>Attaches drag interaction, owned by the current owner, that pins the dragged node through the force handle and unpins it when the drag target changes or the interaction is disposed. The caller keeps ownership of the force and must dispose this interaction first, because disposal mid-drag dispatches Unpin.</summary>
    let attach element (readChart:unit->ChartOutput) (force:OwnedForce) =
        let config={Mode=GestureMode.Drag;MinZoom=1.;MaxZoom=1.;WheelSensitivity=0.01;MaxLassoPoints=4}
        Api.createRoot(fun owner ->
            let nodeId path =
                (readChart()).HitTargets |> List.tryFind(fun target -> target.MarkPath=path)
                |> Option.bind(fun target -> match target.DataIds with [id] -> Some id | _ -> None)
            let send message=force.Dispatch message |> ignore
            let position id=force.Snapshot.Nodes |> List.tryFind(fun node -> node.Id=id) |> Option.map(fun node -> node.Position)
            let input=ChartInteraction.attach element (fun () -> config) readChart (function
                | GestureCommand.DragBy(Some path,delta) ->
                    nodeId path |> Option.iter(fun id ->
                        position id |> Option.iter(fun point -> send(ForceMessage.Pin(id,Point.create (point.X+delta.X) (point.Y+delta.Y)))))
                | _ -> ())
            let mutable dragging=None
            Api.createEffect(fun () ->
                let target=input.Gesture.Target
                Api.untrack(fun () ->
                    let next=target |> Option.bind nodeId
                    if next<>dragging then
                        dragging |> Option.iter(fun id -> send(ForceMessage.Unpin id))
                        dragging<-next
                        next |> Option.iter(fun id -> position id |> Option.iter(fun point -> send(ForceMessage.Pin(id,point)))))) |> ignore
            Api.onCleanup(fun () ->
                dragging |> Option.iter(fun id -> send(ForceMessage.Unpin id))
                dragging<-None)
            new OwnedChartInteraction((fun () -> input.Gesture),(fun () -> input.Selection),
                new OwnedInput(input.Send,(fun () -> (input:>System.IDisposable).Dispose())),
                (fun () -> owner.Dispose())))

namespace Ranvier.D5.View

open Ranvier.D5
open Ranvier.D5.Charts

/// <summary>Node dragging for rendered Sankey charts.</summary>
[<RequireQualifiedAccess>]
module FlowInteraction =
    /// <summary>Attaches drag interaction that moves a dragged single-ID Sankey node vertically with Sankey.moveNode, passing the new layout to publish or the errors to report on the graph thread. The returned interaction belongs to the current owner; dispose it to detach early.</summary>
    let attach element (readChart:unit->ChartOutput) (readConfig:unit->SankeyConfig)
               (readLayout:unit->SankeyLayout) (publish:SankeyLayout->unit) (report:LayoutError list->unit) =
        let config={Mode=GestureMode.Drag;MinZoom=1.;MaxZoom=1.;WheelSensitivity=0.01;MaxLassoPoints=4}
        ChartInteraction.attach element (fun () -> config) readChart (function
            | GestureCommand.DragBy(Some path,delta) ->
                (readChart()).HitTargets |> List.tryFind(fun target -> target.MarkPath=path)
                |> Option.bind(fun target -> match target.DataIds with [id] -> Some id | _ -> None)
                |> Option.iter(fun id ->
                    let layout=readLayout()
                    layout.Nodes |> List.tryFind(fun node -> node.Id=id)
                    |> Option.iter(fun node ->
                        match Sankey.moveNode (readConfig()) id (Bounds.y node.Bounds+delta.Y) layout with
                        | Ok next -> publish next
                        | Error errors -> report errors))
            | _ -> ())

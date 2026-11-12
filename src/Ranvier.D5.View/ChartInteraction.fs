namespace Ranvier.D5.View

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts

type OwnedChartInteraction internal (readGesture:unit->GestureState,readSelection:unit->SelectionState,input:OwnedInput,dispose:unit->unit) =
    member _.Gesture=readGesture()
    member _.Selection=readSelection()
    member _.Send event=input.Send event
    interface IDisposable with member _.Dispose()=dispose()

[<RequireQualifiedAccess>]
module ChartInteraction =
    let private center = function
        | HitPoint(point,_) -> point
        | HitSector(point,inner,outer,start,finish) ->
            let offset=Curves.radialPoint ((start+finish)/2.) ((inner+outer)/2.)
            Point.create (point.X+offset.X) (point.Y+offset.Y)
        | HitBounds bounds -> Point.create (Bounds.x bounds+Bounds.width bounds/2.) (Bounds.y bounds+Bounds.height bounds/2.)
    let private containsPolygon (points:Point list) (point:Point) =
        let mutable inside=false
        if points.Length>=3 then
            for a,b in List.pairwise(points@[List.head points]) do
                if (a.Y>point.Y)<>(b.Y>point.Y) && point.X<(b.X-a.X)*(point.Y-a.Y)/(b.Y-a.Y)+a.X then inside<-not inside
        inside
    /// <summary>Owns linked selection and gesture signals. Brush/lasso select hit-target centers; tooltip clients read Hovered and semantic descriptions.</summary>
    let private attachCore dispose element readConfig readChart onCommand =
        let gesture=Api.createSignal Gesture.empty
        let selection=Api.createSignal Selection.empty
        let select message=
            let current=selection.Value
            let next=Selection.update message current
            if next<>current then selection.Value<-next
        let execute command =
            let chart=readChart()
            match command with
            | GestureCommand.Hover point ->
                let id=point |> Option.bind (fun point -> HitTest.at point chart |> List.rev |> List.tryHead) |> Option.bind (fun target -> List.tryHead target.DataIds)
                select(SelectionMessage.Hover id)
            | GestureCommand.SelectRectangle bounds ->
                chart.HitTargets |> List.filter (fun target -> HitTest.visibleAt (center target.Geometry) target && Bounds.contains (center target.Geometry) bounds) |> List.collect (fun target -> target.DataIds) |> Set.ofList |> SelectionMessage.Replace |> select
            | GestureCommand.SelectLasso polygon ->
                chart.HitTargets |> List.filter (fun target -> HitTest.visibleAt (center target.Geometry) target && containsPolygon polygon (center target.Geometry)) |> List.collect (fun target -> target.DataIds) |> Set.ofList |> SelectionMessage.Replace |> select
            | GestureCommand.MoveFocus delta -> select(SelectionMessage.MoveFocus delta)
            | GestureCommand.SelectFocused -> select SelectionMessage.SelectFocused
            | GestureCommand.ClearSelection -> select SelectionMessage.Clear
            | _ -> ()
            onCommand command
        let input=InputAdapter.attach element readConfig (fun () -> gesture.Value) (fun value -> gesture.Value<-value) execute
        let mutable previousBounds=None
        Api.createEffect(fun () ->
            let chart=readChart()
            let ids=chart.Semantics |> List.collect (fun item -> item.DataIds) |> List.distinct
            Api.untrack(fun () -> Api.batch(fun () ->
                select(SelectionMessage.Available ids)
                if previousBounds.IsSome && previousBounds.Value<>chart.PlotBounds then input.Send InputEvent.Resize
                previousBounds<-Some chart.PlotBounds
                gesture.Value.Target |> Option.iter (fun target ->
                    if not(chart.Semantics |> List.exists (fun item -> item.MarkPath=target)) then input.Send(InputEvent.NodeExit target))))) |> ignore
        new OwnedChartInteraction((fun () -> gesture.Value),(fun () -> selection.Value),input,dispose)

    /// <summary>Creates an independently disposable interaction scope owned by the current signal owner.</summary>
    let attach element readConfig readChart onCommand =
        Api.createRoot(fun owner -> attachCore (fun () -> owner.Dispose()) element readConfig readChart onCommand)

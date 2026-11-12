namespace Ranvier.D5.Charts

/// <summary>Interaction state over semantic data IDs: the selected set, the focused and hovered IDs, and the ordered IDs currently available.</summary>
type SelectionState = {Selected:Set<string>;Focused:string option;Hovered:string option;Available:string list}
/// <summary>Selection events for Selection.update: Available sets the ordered ID domain and prunes state to it; Replace, Toggle, Focus and Hover ignore unavailable IDs; MoveFocus steps cyclically; SelectFocused selects only the focused ID; Clear empties the selection and hover.</summary>
[<RequireQualifiedAccess>]
type SelectionMessage = Available of string list | Replace of Set<string> | Toggle of string | Focus of string option | Hover of string option | MoveFocus of int | SelectFocused | Clear

/// <summary>A pure reducer for chart selection, focus and hover.</summary>
[<RequireQualifiedAccess>]
module Selection =
    /// <summary>A state with nothing selected, focused, hovered or available.</summary>
    let empty={Selected=Set.empty;Focused=None;Hovered=None;Available=[]}
    /// <summary>Returns the state after one message; IDs outside Available never enter the state, MoveFocus wraps around Available (starting at the first or last ID when nothing is focused), and Clear keeps focus.</summary>
    let update message state =
        let allowed=Set.ofList state.Available
        let valid value=value |> Option.filter (fun id -> Set.contains id allowed)
        match message with
        | SelectionMessage.Available values ->
            let values=List.distinct values
            let allowed=Set.ofList values
            {state with Available=values;Selected=Set.intersect allowed state.Selected;Focused=state.Focused |> Option.filter (fun id -> Set.contains id allowed);Hovered=state.Hovered |> Option.filter (fun id -> Set.contains id allowed)}
        | SelectionMessage.Replace ids -> {state with Selected=Set.intersect allowed ids}
        | SelectionMessage.Toggle id when Set.contains id allowed -> {state with Selected=(if Set.contains id state.Selected then Set.remove id state.Selected else Set.add id state.Selected)}
        | SelectionMessage.Toggle _ -> state
        | SelectionMessage.Focus id -> {state with Focused=valid id}
        | SelectionMessage.Hover id -> {state with Hovered=valid id}
        | SelectionMessage.MoveFocus delta ->
            if state.Available.IsEmpty then state else
            let current=state.Focused |> Option.bind (fun id -> List.tryFindIndex ((=) id) state.Available)
            let next=
                match current with
                | None -> if delta<0 then state.Available.Length-1 else 0
                | Some index -> int ((int64 index+int64 delta%int64 state.Available.Length+int64 state.Available.Length)%int64 state.Available.Length)
            {state with Focused=Some state.Available[next]}
        | SelectionMessage.SelectFocused -> {state with Selected=state.Focused |> Option.toList |> Set.ofList}
        | SelectionMessage.Clear -> {state with Selected=Set.empty;Hovered=None}

    /// Reconciles visible interaction targets against the full selectable source domain.
    let reconcile (sourceIds:Set<string>) visibleIds state =
        let visible=visibleIds |> List.filter sourceIds.Contains
        let next=update (SelectionMessage.Available visible) state
        {next with Selected=state.Selected |> Set.filter sourceIds.Contains}

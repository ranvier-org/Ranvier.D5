namespace Ranvier.D5

open System

/// <summary>What a single-pointer drag does: pan the zoom, drag the pressed target, brush a rectangle, or draw a lasso.
/// Two pointers always pinch-zoom.</summary>
[<RequireQualifiedAccess>]
type GestureMode = Pan | Drag | Rectangle | Lasso
/// <summary>Gesture settings: positive zoom-scale limits with <c>MaxZoom</c> at least <c>MinZoom</c>, positive wheel sensitivity,
/// and a lasso point cap in [4,100000].</summary>
type GestureConfig = {Mode:GestureMode;MinZoom:float;MaxZoom:float;WheelSensitivity:float;MaxLassoPoints:int}
/// <summary>A host-neutral input event in screen coordinates, fed to <c>Gesture.update</c>.</summary>
[<RequireQualifiedAccess>]
type InputEvent =
    /// <summary>A pointer press, with the scene-node key path under the pointer when known.</summary>
    | Down of pointer:int * position:Point * target:string list option
    /// <summary>A pointer move; moves of uncaptured pointers produce hover commands.</summary>
    | Move of pointer:int * position:Point
    /// <summary>A pointer release.</summary>
    | Up of pointer:int * position:Point
    /// <summary>A wheel step at an anchor; positive delta zooms out and negative delta zooms in.</summary>
    | Wheel of anchor:Point * delta:float
    /// <summary>A DOM key name; arrows move focus, Enter or space selects, Escape cancels and clears selection.</summary>
    | Key of string
    | Cancel | FocusLost | Leave | Resize | NodeExit of string list
/// <summary>A side effect requested by <c>Gesture.update</c> for the host to apply, such as pointer capture, zoom or selection.</summary>
[<RequireQualifiedAccess>]
type GestureCommand =
    | Capture of int | Release of int | ZoomChanged of Zoom | DragBy of string list option * Point
    | SelectRectangle of Bounds | SelectLasso of Point list | Hover of Point option
    | MoveFocus of int | SelectFocused | ClearSelection
/// <summary>Gesture state: current zoom, active pointers (at most two), gesture start point, pressed target
/// and lasso points in reverse order.</summary>
type GestureState = {Zoom:Zoom;Pointers:Map<int,Point>;Start:Point option;Target:string list option;Lasso:Point list}

/// <summary>A pure pointer, wheel and keyboard gesture state machine.</summary>
[<RequireQualifiedAccess>]
module Gesture =
    /// <summary>The initial state: identity zoom and no active pointers.</summary>
    let empty={Zoom=Zoom.identity;Pointers=Map.empty;Start=None;Target=None;Lasso=[]}
    let private validate config =
        Check.positive "minZoom" config.MinZoom
        Check.positive "maxZoom" config.MaxZoom
        Check.positive "wheelSensitivity" config.WheelSensitivity
        if config.MaxZoom<config.MinZoom || config.MaxLassoPoints<4 || config.MaxLassoPoints>100000 then invalidArg "config" "Invalid gesture bounds."
    let private cancel state =
        {state with Pointers=Map.empty;Start=None;Target=None;Lasso=[]},(state.Pointers |> Map.toList |> List.map (fst >> GestureCommand.Release))@[GestureCommand.Hover None]
    let private clampZoom config anchor factor zoom =
        let scale=Zoom.scaleValue zoom
        let target=Math.Clamp(scale*factor,config.MinZoom,config.MaxZoom)
        Zoom.zoomAt anchor (target/scale) zoom
    let private center (a:Point) (b:Point)=Point.create (Check.lerp a.X b.X 0.5) (Check.lerp a.Y b.Y 0.5)
    let private move config id point state =
        match Map.tryFind id state.Pointers with
        | None -> state,[GestureCommand.Hover(Some point)]
        | Some previous ->
            let pointers=Map.add id point state.Pointers
            if pointers.Count=2 then
                let old=state.Pointers |> Map.toArray |> Array.map snd
                let current=pointers |> Map.toArray |> Array.map snd
                let a,b=center old[0] old[1],center current[0] current[1]
                let distance (a:Point) (b:Point)=Double.Hypot(a.X-b.X,a.Y-b.Y)
                let before,after=distance old[0] old[1],distance current[0] current[1]
                let factor=if before=0. || after=0. then 1. else after/before
                let zoom=clampZoom config a factor state.Zoom |> Zoom.pan (b.X-a.X) (b.Y-a.Y)
                {state with Pointers=pointers;Zoom=zoom},[GestureCommand.ZoomChanged zoom]
            else
                let delta=Point.create (point.X-previous.X) (point.Y-previous.Y)
                match config.Mode with
                | GestureMode.Pan ->
                    let zoom=Zoom.pan delta.X delta.Y state.Zoom
                    {state with Pointers=pointers;Zoom=zoom},[GestureCommand.ZoomChanged zoom]
                | GestureMode.Drag -> {state with Pointers=pointers},[GestureCommand.DragBy(state.Target,delta)]
                | GestureMode.Rectangle -> {state with Pointers=pointers},[]
                | GestureMode.Lasso ->
                    let points=if Double.Hypot(delta.X,delta.Y)<1. then state.Lasso else point::state.Lasso
                    let points=if points.Length>config.MaxLassoPoints then points |> List.indexed |> List.choose (fun (i,p) -> if i%2=0 || i=points.Length-1 then Some p else None) else points
                    {state with Pointers=pointers;Lasso=points},[]
    /// <summary>Applies one event and returns the next state with the commands to perform, in order.
    /// Throws <c>ArgumentException</c> for an invalid configuration or a non-finite pointer position.</summary>
    let update config event state =
        validate config
        match event with
        | InputEvent.Cancel | InputEvent.FocusLost | InputEvent.Resize -> cancel state
        | InputEvent.NodeExit key when state.Target=Some key -> cancel state
        | InputEvent.NodeExit _ -> state,[GestureCommand.Hover None]
        | InputEvent.Leave -> state,[GestureCommand.Hover None]
        | InputEvent.Down(id,point,target) ->
            Point.create point.X point.Y |> ignore
            if state.Pointers.Count>=2 || Map.containsKey id state.Pointers then state,[] else
            let first=state.Pointers.IsEmpty
            {state with Pointers=Map.add id point state.Pointers;Start=(if first then Some point else state.Start);Target=(if first then target else state.Target);Lasso=(if first then [point] else state.Lasso)},[GestureCommand.Capture id]
        | InputEvent.Move(id,point) -> Point.create point.X point.Y |> ignore;move config id point state
        | InputEvent.Up(id,point) ->
            Point.create point.X point.Y |> ignore
            if state.Pointers.Count=2 && Map.containsKey id state.Pointers then cancel state else
            if not(Map.containsKey id state.Pointers) then state,[] else
            let moved,commands=move config id point state
            let pointers=Map.remove id moved.Pointers
            let selected =
                if moved.Pointers.Count<>1 then [] else
                match config.Mode,moved.Start with
                | GestureMode.Rectangle,Some start -> [GestureCommand.SelectRectangle(Brush.between start point)]
                | GestureMode.Lasso,_ -> [GestureCommand.SelectLasso(List.rev moved.Lasso)]
                | _ -> []
            let remaining=pointers |> Map.toList |> List.tryHead |> Option.map snd
            {moved with Pointers=pointers;Start=remaining;Lasso=(remaining |> Option.toList);Target=(if pointers.IsEmpty then None else moved.Target)},commands@[GestureCommand.Release id]@selected
        | InputEvent.Wheel(anchor,delta) ->
            Check.finite "delta" delta
            let zoom=clampZoom config anchor (exp(Math.Clamp(-delta*config.WheelSensitivity,-20.,20.))) state.Zoom
            {state with Zoom=zoom},[GestureCommand.ZoomChanged zoom]
        | InputEvent.Key key ->
            match key with
            | "ArrowRight" | "ArrowDown" -> state,[GestureCommand.MoveFocus 1]
            | "ArrowLeft" | "ArrowUp" -> state,[GestureCommand.MoveFocus -1]
            | "Enter" | " " -> state,[GestureCommand.SelectFocused]
            | "Escape" -> let state,commands=cancel state in state,commands@[GestureCommand.ClearSelection]
            | _ -> state,[]

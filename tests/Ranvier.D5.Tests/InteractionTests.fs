module Ranvier.D5.Tests.InteractionTests

open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let config={GestureConfig.Mode=GestureMode.Pan;MinZoom=0.5;MaxZoom=4.;WheelSensitivity=0.002;MaxLassoPoints=32}
let p x y=Point.create x y
let tests = testList "Owned gesture contracts" [
    testCase "wheel zoom clamps and preserves the anchor" <| fun _ ->
        let state,_=Gesture.update config (InputEvent.Wheel(p 20. 30.,-10000.)) Gesture.empty
        Expect.equal (Zoom.scaleValue state.Zoom) 4. "clamped scale"
        Expect.equal (Zoom.invert (p 20. 30.) state.Zoom) (p 20. 30.) "anchor"
    testCase "cancel releases all captured pointers and resize cancels drag" <| fun _ ->
        let first,_=Gesture.update config (InputEvent.Down(1,p 0. 0.,Some ["a"])) Gesture.empty
        let second,_=Gesture.update config (InputEvent.Down(2,p 10. 0.,None)) first
        let final,commands=Gesture.update config InputEvent.Resize second
        Expect.equal final.Pointers Map.empty "no capture state"
        Expect.equal (commands |> List.choose (function GestureCommand.Release id -> Some id | _ -> None) |> Set.ofList) (set [1;2]) "both released"
    testCase "pinch combines translation and scaling" <| fun _ ->
        let a,_=Gesture.update config (InputEvent.Down(1,p 0. 0.,None)) Gesture.empty
        let b,_=Gesture.update config (InputEvent.Down(2,p 10. 0.,None)) a
        let c,_=Gesture.update config (InputEvent.Move(2,p 20. 0.)) b
        DataScaleTests.near 2. (Zoom.scaleValue c.Zoom)
    testCase "lasso storage stays bounded and exit cancels active drag" <| fun _ ->
        let lasso={config with Mode=GestureMode.Lasso}
        let start,_=Gesture.update lasso (InputEvent.Down(1,p 0. 0.,Some ["a"])) Gesture.empty
        let moved=[1..1000] |> List.fold (fun state i -> Gesture.update lasso (InputEvent.Move(1,p (float i) (float(i%20)))) state |> fst) start
        Expect.isLessThanOrEqual moved.Lasso.Length lasso.MaxLassoPoints "bounded gesture history"
        let exited,commands=Gesture.update lasso (InputEvent.NodeExit ["a"]) moved
        Expect.equal exited.Pointers Map.empty "exited target cancels"
        Expect.contains commands (GestureCommand.Release 1) "released"
    testCase "finishing a pinch cannot accidentally commit a brush" <| fun _ ->
        let brush={config with Mode=GestureMode.Rectangle}
        let a,_=Gesture.update brush (InputEvent.Down(1,p 0. 0.,None)) Gesture.empty
        let b,_=Gesture.update brush (InputEvent.Down(2,p 10. 0.,None)) a
        let c,commands=Gesture.update brush (InputEvent.Up(2,p 20. 0.)) b
        Expect.isEmpty c.Pointers "pinch ends both captures"
        Expect.isFalse (commands |> List.exists(function GestureCommand.SelectRectangle _ -> true | _ -> false)) "no accidental selection"
    testCase "selection discards exited IDs and retains surviving focus" <| fun _ ->
        let state=Selection.empty |> Selection.update (SelectionMessage.Available ["a";"b"]) |> Selection.update (SelectionMessage.Replace(set ["a";"b"])) |> Selection.update (SelectionMessage.Focus(Some "b"))
        let next=Selection.update (SelectionMessage.Available ["b";"c"]) state
        Expect.equal next.Selected (set ["b"]) "exited selection removed"
        Expect.equal next.Focused (Some "b") "focus survives"
]

module Ranvier.D5.Tests.KeyedTransitionTests

open Expecto
open Ranvier
open Ranvier.D5

let mark x=Shape.rectangle(Bounds.create x 0. 10. 10.) |> Mark.path "a"
let layers now state = KeyedTransition.sample now state |> List.collect(function Group(_,children) -> children |> List.choose(function Leaf mark -> Some mark | _ -> None) | _ -> [])
let firstX mark=
    match Mark.geometry mark with
    | PathGeometry path -> match List.head(Path.commands path) with MoveTo p -> p.X | _ -> failwith "move"
    | _ -> failwith "path"
let tests=testList "Keyed transitions" [
    testCase "interrupted compatible morph starts at current geometry" <| fun _ ->
        let a=KeyedTransition.start 0. [mark 0.]
        let b=KeyedTransition.retarget 0. 100. (Ranvier.Anime.Ease.named "linear") false [mark 100.] a
        DataScaleTests.near 50. (layers 50. b |> List.head |> firstX)
        let c=KeyedTransition.retarget 50. 100. id false [mark 200.] b
        DataScaleTests.near 50. (layers 50. c |> List.head |> firstX)
        DataScaleTests.near 125. (layers 100. c |> List.head |> firstX)
    testCase "exit reentry survives original deadline" <| fun _ ->
        let a=KeyedTransition.start 0. [mark 0.]
        let b=KeyedTransition.retarget 0. 100. id false [] a
        let c=KeyedTransition.retarget 50. 100. id false [mark 20.] b
        Expect.hasLength (layers 200. c) 1 "reentered key survives"
        Expect.isEmpty (layers 200. b) "exited key completes"
    testCase "incompatible geometry crossfades and reduced motion completes" <| fun _ ->
        let a=KeyedTransition.start 0. [mark 0.]
        let target=Mark.text "a" (Point.create 10. 10.) "new"
        let b=KeyedTransition.retarget 0. 100. id false [target] a
        Expect.hasLength (layers 50. b) 2 "both geometries visible"
        Expect.hasLength (layers 100. b) 1 "old geometry removed"
        let c=KeyedTransition.retarget 50. 100. id true [] b
        Expect.isEmpty (layers 50. c) "reduced exit immediate"
    testCase "rapid topology interruption has bounded retained layers" <| fun _ ->
        let mutable state=KeyedTransition.start 0. [mark 0.]
        for i in 1..100 do
            let target=if i%2=0 then mark(float i) else Mark.text "a" (Point.create 0. 0.) (string i)
            state<-KeyedTransition.retarget (float i) 1000. id false [target] state
        Expect.isLessThanOrEqual (layers 100. state).Length 8 "bounded crossfade storage"
    testCase "owned clock and reduced-motion signal drive samples" <| fun _ ->
        use graph=new Ranvier.Graph()
        graph.Run(fun () ->
            let time=Ranvier.Api.createSignal 0.
            let target=Ranvier.Api.createSignal [mark 0.]
            let reduced=Ranvier.Api.createSignal false
            let value=KeyedTransition.animate (fun () -> time.Value) (fun () -> target.Value) 100. id (fun () -> reduced.Value)
            value.Value |> ignore
            target.Value<-[mark 100.]
            value.Value |> ignore
            time.Value<-50.
            let getX()=value.Value |> List.collect(function Group(_,children)->children |> List.choose(function Leaf m->Some m | _->None) | _->[]) |> List.head |> firstX
            DataScaleTests.near 50. (getX())
            reduced.Value<-true
            DataScaleTests.near 100. (getX()))
    testCase "duplicate keys and invalid duration rejected" <| fun _ ->
        Expect.throws (fun () -> KeyedTransition.start 0. [mark 0.;mark 1.] |> ignore) "duplicates"
        Expect.throws (fun () -> KeyedTransition.retarget 0. -1. id false [] (KeyedTransition.start 0. []) |> ignore) "duration"
]

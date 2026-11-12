module Ranvier.D5.Tests.ReactiveForceTests

open System
open Expecto
open Ranvier
open Ranvier.D5
open Ranvier.D5.View

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private initial() =
    GraphData.create [GraphNode.create "a";GraphNode.create "b"] [] |> ok
    |> Force.initialize {ForceConfig.create(Point.create 100. 100.) with AlphaDecay=0.;MaxSteps=2} (Seed.create 1u) |> ok
let private chunk={ForceChunk.defaultValue with MaxSteps=1}

let tests=testList "Reactive force" [
    testCase "worker publishes bounded chunks and stops at iteration budget" <| fun _ ->
        use graph=new Graph()
        use pump=new ReactiveLayoutTests.Pump()
        use force=graph.Run(fun () -> ReactiveForce.create pump.Dispatcher chunk (initial()))
        Expect.equal (graph.Run(fun () -> force.Snapshot.Iteration)) 0 "initial snapshot"
        pump.Post()
        Expect.equal (graph.Run(fun () -> force.Snapshot.Iteration)) 0 "completion waits for frame"
        pump.Frame()
        Expect.equal (graph.Run(fun () -> force.Snapshot.Iteration)) 1 "one step per chunk"
        pump.Post();pump.Frame()
        let final=graph.Run(fun () -> force.Snapshot)
        Expect.equal final.Iteration 2 "iteration limit"
        Expect.isTrue final.Finished "worker stops scheduling"
        pump.Frame()
        Expect.equal pump.FrameCount 0 "no self-sustaining frame loop"
    testCase "drag messages supersede stale worker state and topology removes exits" <| fun _ ->
        use graph=new Graph()
        use pump=new ReactiveLayoutTests.Pump()
        use force=graph.Run(fun () -> ReactiveForce.create pump.Dispatcher chunk (initial()))
        graph.Run(fun () -> force.Dispatch(ForceMessage.Pin("a",Point.create 75. 90.)) |> ok)
        pump.Post();pump.Frame()
        pump.Post();pump.Frame()
        let pinned=graph.Run(fun () -> force.Snapshot.Nodes |> List.find(fun node -> node.Id="a"))
        Expect.equal pinned.Position (Point.create 75. 90.) "stale completion cannot undo drag"
        Expect.equal pinned.Pinned (Some(Point.create 75. 90.)) "pin retained in next chunk"
        let replacement=GraphData.create [GraphNode.create "a"] [] |> ok
        graph.Run(fun () -> force.Dispatch(ForceMessage.ReplaceGraph replacement) |> ok)
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes |> List.map(fun node -> node.Id))) ["a"] "exit removed immediately"
        Expect.isError (graph.Run(fun () -> force.Dispatch(ForceMessage.Unpin "missing"))) "unknown semantic id"
        let mutable rounds=0
        while not(graph.Run(fun () -> force.Snapshot.Finished)) && rounds<10 do
            pump.Post();pump.Frame()
            rounds<-rounds+1
        Expect.isTrue (graph.Run(fun () -> force.Snapshot.Finished)) "replacement finishes"
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes |> List.map(fun node -> node.Id))) ["a"] "late worker does not resurrect removed node"
    testCase "disposed controller rejects messages" <| fun _ ->
        use graph=new Graph()
        use pump=new ReactiveLayoutTests.Pump()
        let force=graph.Run(fun () -> ReactiveForce.create pump.Dispatcher chunk (initial()))
        (force:>IDisposable).Dispose()
        Expect.throwsT<ObjectDisposedException> (fun () -> graph.Run(fun () -> force.Dispatch(ForceMessage.Reheat 1.) |> ignore)) "disposed ownership"
        pump.Post();pump.Frame()
        Expect.equal (graph.Run(fun () -> force.Snapshot.Iteration)) 0 "completion after disposal remains inert"
]

module Ranvier.D5.Tests.ReactiveLayoutTests

open System
open System.Collections.Concurrent
open System.Threading
open Expecto
open Ranvier
open Ranvier.D5
open Ranvier.D5.View

type Pump() =
    let posts=new BlockingCollection<unit->unit>()
    let frames=ResizeArray<unit->unit>()
    member _.Dispatcher={HostDispatcher.Post=posts.Add;RequestFrame=frames.Add}
    member _.Post() =
        let mutable callback=Unchecked.defaultof<unit->unit>
        if not(posts.TryTake(&callback,5000)) then failwith "worker completion timeout"
        callback()
    member _.Frame() =
        let ready=frames.ToArray()
        frames.Clear()
        for callback in ready do callback()
    member _.FrameCount=frames.Count
    interface IDisposable with member _.Dispose()=posts.Dispose()

let tests=testList "Owned background layout" [
    testCase "100 requests replace pending and stale completion cannot publish" <| fun _ ->
        use graph=new Graph()
        use pump=new Pump()
        use release=new ManualResetEventSlim(false)
        use started=new BlockingCollection<int>()
        let source=graph.Run(fun () -> Api.createSignal 0)
        use layout=graph.Run(fun () -> ReactiveLayout.create pump.Dispatcher (fun () -> source.Value) (fun token value ->
            started.Add value
            if value=0 then release.Wait()
            Ok value))
        Expect.equal (started.Take()) 0 "first started"
        graph.Run(fun () -> for i in 1..100 do source.Value<-i)
        Expect.equal started.Count 0 "only one running"
        release.Set()
        pump.Post()
        Expect.equal (started.Take()) 100 "only latest pending starts"
        pump.Frame()
        Expect.equal (graph.Run(fun () -> layout.LastGood)) None "stale result suppressed"
        pump.Post()
        Expect.isLessThanOrEqual pump.FrameCount 1 "one frame callback"
        pump.Frame()
        Expect.equal (graph.Run(fun () -> layout.LastGood)) (Some 100) "latest result"
    testCase "fault and retry preserve last good result" <| fun _ ->
        use graph=new Graph()
        use pump=new Pump()
        let mutable fail=false
        use layout=graph.Run(fun () -> ReactiveLayout.create pump.Dispatcher (fun () -> 7) (fun _ value -> if fail then failwith "kernel failed" else Ok value))
        pump.Post();pump.Frame()
        fail<-true
        layout.Retry()
        pump.Post();pump.Frame()
        Expect.equal (graph.Run(fun () -> layout.LastGood)) (Some 7) "last result preserved"
        Expect.isTrue (graph.Run(fun () -> match layout.State with WorkState.Failed _ -> true | _ -> false)) "typed failure"
        fail<-false
        layout.Retry()
        pump.Post();pump.Frame()
        Expect.isTrue (graph.Run(fun () -> match layout.State with WorkState.Ready(_,7) -> true | _ -> false)) "retry succeeds"
    testCase "completion after disposal is inert and cancellation reaches kernel" <| fun _ ->
        use graph=new Graph()
        use pump=new Pump()
        use started=new ManualResetEventSlim(false)
        use finished=new ManualResetEventSlim(false)
        let layout=graph.Run(fun () -> ReactiveLayout.create pump.Dispatcher (fun () -> 0) (fun token _ ->
            started.Set()
            token.WaitHandle.WaitOne(5000) |> ignore
            finished.Set()
            token.ThrowIfCancellationRequested()
            Ok 0))
        Expect.isTrue (started.Wait 5000) "worker started"
        (layout :> IDisposable).Dispose()
        Expect.isTrue (finished.Wait 5000) "cooperative cancellation"
        pump.Post();pump.Frame()
        Expect.equal (graph.Run(fun () -> layout.LastGood)) None "no late publication"
]

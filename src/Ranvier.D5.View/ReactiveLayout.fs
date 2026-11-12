namespace Ranvier.D5.View

open System
open System.Threading
open Ranvier
open Ranvier.D5

/// <summary>Post is thread-safe and queues to the graph thread; RequestFrame queues to its next host frame. Neither callback may execute inline.</summary>
type HostDispatcher = {Post:(unit->unit)->unit;RequestFrame:(unit->unit)->unit}

/// <summary>Progress of a ReactiveLayout or ReactiveForce worker. Each request takes a new generation, and results of superseded generations are never published.</summary>
[<RequireQualifiedAccess>]
type WorkState<'T> =
    /// <summary>No work state has been published yet.</summary>
    | Idle
    /// <summary>The request with this generation is queued or running on the thread pool.</summary>
    | Running of generation:int64
    /// <summary>The kernel completed this generation with a value, which also becomes LastGood.</summary>
    | Ready of generation:int64 * value:'T
    /// <summary>The kernel for this generation returned errors or threw; LastGood keeps the previous value.</summary>
    | Failed of generation:int64 * errors:LayoutError list

/// <summary>Handle to a background layout worker created by ReactiveLayout.create. Dispose it on the graph thread, or dispose the owner current at creation, to cancel running work and drop pending and unpublished results.</summary>
type OwnedLayout<'T> internal (read:unit->WorkState<'T>,lastGood:unit->'T option,retry:unit->unit,dispose:unit->unit) =
    /// <summary>The latest published work state; a tracked signal read, updated at host frames on the graph thread.</summary>
    member _.State=read()
    /// <summary>The value of the most recent Ready publication, retained while later work runs or fails; a tracked signal read.</summary>
    member _.LastGood=lastGood()
    /// <summary>Re-reads the input and requests a new generation, cancelling any running kernel; call on the graph thread. Ignored after disposal.</summary>
    member _.Retry()=retry()
    interface IDisposable with member _.Dispose()=dispose()

/// <summary>Runs pure layout kernels on the thread pool and publishes their results to signals on the graph thread.</summary>
[<RequireQualifiedAccess>]
module ReactiveLayout =
    /// <summary>Snapshots immutable inputs on the owner thread, runs one cancellable kernel with one latest pending request, and publishes only current results at host frames.</summary>
    /// <remarks>The kernel must check its token within bounded chunks and must not access signals, graph owners or native objects. LastGood remains available while running or failed.</remarks>
    let create dispatcher readInput (kernel:CancellationToken->'Input->Result<'Output,LayoutError list>) =
        let graph=Graph.Current
        graph.AssertOnGraphThread "ReactiveLayout.create"
        Api.createRoot(fun owner ->
            let state=Api.createSignal WorkState.Idle
            let lastGood=Api.createSignal None
            let mutable disposed=false
            let mutable generation=0L
            let mutable active:CancellationTokenSource option=None
            let mutable pending:(int64*'Input) option=None
            let mutable publication:WorkState<'Output> option=None
            let mutable frameQueued=false
            let onOwner action =
                graph.AssertOnGraphThread "ReactiveLayout callback"
                if not disposed then graph.Run(fun () -> Api.runWithOwner owner (fun () -> Api.untrack action))
            let publish value =
                publication<-Some value
                if not frameQueued then
                    frameQueued<-true
                    dispatcher.RequestFrame(fun () -> onOwner(fun () ->
                        frameQueued<-false
                        let next=publication
                        publication<-None
                        match next with
                        | Some value ->
                            let version=match value with WorkState.Running g | WorkState.Ready(g,_) | WorkState.Failed(g,_) -> g | WorkState.Idle -> generation
                            if version=generation then
                                Api.batch(fun () ->
                                    state.Value<-value
                                    match value with WorkState.Ready(_,result) -> lastGood.Value<-Some result | _ -> ())
                        | None -> ()))
            let rec launch version input =
                let cancellation=new CancellationTokenSource()
                active<-Some cancellation
                ThreadPool.UnsafeQueueUserWorkItem(WaitCallback(fun _ ->
                    let result=
                        try
                            cancellation.Token.ThrowIfCancellationRequested()
                            kernel cancellation.Token input
                        with
                        | :? OperationCanceledException when cancellation.IsCancellationRequested -> Error []
                        | error -> Error [InvalidObservation("Layout kernel: "+error.Message)]
                    try dispatcher.Post(fun () ->
                        cancellation.Dispose()
                        onOwner(fun () ->
                            active<-None
                            if version=generation then
                                match result with Ok value -> publish(WorkState.Ready(version,value)) | Error errors -> publish(WorkState.Failed(version,errors))
                            let next=pending
                            pending<-None
                            next |> Option.iter(fun (version,input) -> launch version input)))
                    with :? ObjectDisposedException -> cancellation.Dispose()),null) |> ignore
            let request input =
                generation<-generation+1L
                publish(WorkState.Running generation)
                match active with
                | Some cancellation ->
                    pending<-Some(generation,input)
                    cancellation.Cancel()
                | None -> launch generation input
            let dispose() =
                if not disposed then
                    graph.AssertOnGraphThread "ReactiveLayout.Dispose"
                    disposed<-true
                    generation<-generation+1L
                    pending<-None
                    publication<-None
                    active |> Option.iter(fun cancellation -> cancellation.Cancel())
            Api.onCleanup dispose
            try
                Api.createEffect(fun () -> let input=readInput() in Api.untrack(fun () -> request input)) |> ignore
                new OwnedLayout<_>((fun () -> state.Value),(fun () -> lastGood.Value),
                    (fun () -> onOwner(fun () -> request(readInput()))),
                    (fun () -> graph.Run(fun () -> owner.Dispose())))
            with _ ->
                owner.Dispose()
                reraise())

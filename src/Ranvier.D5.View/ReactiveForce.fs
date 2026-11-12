namespace Ranvier.D5.View

open System
open Ranvier
open Ranvier.D5

/// <summary>A change that OwnedForce.Dispatch applies to the current simulation state before requesting further background steps.</summary>
[<RequireQualifiedAccess>]
type ForceMessage =
    /// <summary>Fixes the node with this ID at the position with zero velocity and reheats the simulation.</summary>
    | Pin of id:string * position:Point
    /// <summary>Releases the pin on the node with this ID and reheats the simulation.</summary>
    | Unpin of id:string
    /// <summary>Restarts the iteration budget and raises alpha to at least this value, which must lie in [0, 1].</summary>
    | Reheat of alpha:float
    /// <summary>Replaces the topology, keeping surviving nodes' positions and velocities unless the new graph pins them.</summary>
    | ReplaceGraph of GraphData

/// <summary>The budget of one background force chunk: at most MaxSteps ticks, stopping once Budget has elapsed after the first tick.</summary>
type ForceChunk = { MaxSteps:int; Budget:TimeSpan }
/// <summary>Default force chunk budgets.</summary>
[<RequireQualifiedAccess>]
module ForceChunk =
    /// <summary>Eight ticks within a six-millisecond budget per chunk.</summary>
    let defaultValue={MaxSteps=8;Budget=TimeSpan.FromMilliseconds 6.}

/// <summary>Handle to a force simulation stepped in the background by ReactiveForce.create. Dispose it on the graph thread, or dispose the owner current at creation, to cancel background stepping.</summary>
type OwnedForce internal (read:unit->ForceSnapshot, status:unit->WorkState<ForceState>, dispatch:ForceMessage->Result<unit,LayoutError list>, dispose:unit->unit) =
    /// <summary>The latest published simulation snapshot; a tracked signal read.</summary>
    member _.Snapshot=read()
    /// <summary>The work state of the background stepping worker; a tracked signal read.</summary>
    member _.State=status()
    /// <summary>Applies a message synchronously on the graph thread, publishes the new snapshot and requests further stepping. Returns errors for rejected messages and raises ObjectDisposedException after disposal.</summary>
    member _.Dispatch message=dispatch message
    interface IDisposable with member _.Dispose()=dispose()

/// <summary>Runs force simulations in time-budgeted chunks on the thread pool and publishes snapshots on the graph thread.</summary>
[<RequireQualifiedAccess>]
module ReactiveForce =
    /// <summary>Creates a simulation under the current owner that steps in background chunks and publishes each chunk at the next host frame until it finishes. Must be called on the graph thread; dispose the handle or its owner to stop it.</summary>
    let create (dispatcher:HostDispatcher) (chunk:ForceChunk) (initial:ForceState) : OwnedForce =
        if chunk.MaxSteps<=0 || chunk.Budget<=TimeSpan.Zero then invalidArg "chunk" "A chunk needs a positive step count and time budget."
        let graph=Graph.Current
        graph.AssertOnGraphThread "ReactiveForce.create"
        Api.createRoot(fun owner ->
            let mutable disposed=false
            let mutable current=initial
            let mutable revision=0L
            let input=Api.createSignal(revision,initial)
            let snapshot=Api.createSignal(Force.snapshot initial)
            let worker=ReactiveLayout.create dispatcher (fun () -> input.Value) (fun token (_,state) ->
                let timer=System.Diagnostics.Stopwatch.StartNew()
                let mutable result=state
                let mutable steps=0
                while steps<chunk.MaxSteps && not(Force.isFinished result) && (steps=0 || timer.Elapsed<chunk.Budget) do
                    result<-Force.stepWithCancellation token 1 result
                    steps<-steps+1
                token.ThrowIfCancellationRequested()
                Ok result)
            let request state =
                revision<-revision+1L
                input.Value<-(revision,state)
            Api.onCleanup(fun () -> disposed<-true)
            Api.createEffect(fun () ->
                let next=worker.LastGood
                Api.untrack(fun () ->
                    next |> Option.iter(fun state ->
                        current<-state
                        snapshot.Value<-Force.snapshot state
                        if not(Force.isFinished state) then request state))) |> ignore
            let dispatch message =
                graph.AssertOnGraphThread "OwnedForce.Dispatch"
                if disposed then raise(ObjectDisposedException "OwnedForce")
                graph.Run(fun () ->
                    let next=
                        try
                            match message with
                            | ForceMessage.Pin(id,position) -> Force.pin id (Some position) current
                            | ForceMessage.Unpin id -> Force.pin id None current
                            | ForceMessage.Reheat alpha -> Ok(Force.reheat alpha current)
                            | ForceMessage.ReplaceGraph topology -> Force.updateGraph topology current
                        with :? ArgumentException as error -> Error [InvalidObservation error.Message]
                    next |> Result.map(fun state ->
                        current<-state
                        Api.batch(fun () ->
                            snapshot.Value<-Force.snapshot state
                            request state)))
            new OwnedForce((fun () -> snapshot.Value),(fun () -> worker.State),dispatch,
                (fun () -> graph.Run(fun () -> owner.Dispose()))))

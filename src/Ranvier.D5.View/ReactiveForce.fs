namespace Ranvier.D5.View

open System
open Ranvier
open Ranvier.D5

[<RequireQualifiedAccess>]
type ForceMessage =
    | Pin of id:string * position:Point
    | Unpin of id:string
    | Reheat of alpha:float
    | ReplaceGraph of GraphData

type ForceChunk = { MaxSteps:int; Budget:TimeSpan }
[<RequireQualifiedAccess>]
module ForceChunk =
    let defaultValue={MaxSteps=8;Budget=TimeSpan.FromMilliseconds 6.}

type OwnedForce internal (read:unit->ForceSnapshot, status:unit->WorkState<ForceState>, dispatch:ForceMessage->Result<unit,LayoutError list>, dispose:unit->unit) =
    member _.Snapshot=read()
    member _.State=status()
    member _.Dispatch message=dispatch message
    interface IDisposable with member _.Dispose()=dispose()

[<RequireQualifiedAccess>]
module ReactiveForce =
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

/// <summary>The sampling harness the benchmark programs share: each contract takes 60 timed samples after warm-up, with a full
/// collection between samples, and checks its p95 time and median allocation against limits.</summary>
module Ranvier.D5.BenchmarkHarness

open System
open System.Diagnostics
open System.Threading

/// <summary>One contract's result; <c>Attempts</c> counts the sampling windows it took (more than one only after a timing breach).</summary>
type Measurement = { Name:string; MedianMs:float; P95Ms:float; MedianBytes:int64; LimitMs:float; LimitBytes:int64; Attempts:int }

let private window name limit bytes attempt (action:unit -> unit) =
    let samples =
        Array.init 60 (fun _ ->
            // Collect outside the timed region so one sample's garbage is not charged to the next.
            GC.Collect()
            GC.WaitForPendingFinalizers()
            GC.Collect()
            let allocated=GC.GetAllocatedBytesForCurrentThread()
            let watch=Stopwatch.StartNew()
            action()
            watch.Stop()
            watch.Elapsed.TotalMilliseconds,GC.GetAllocatedBytesForCurrentThread()-allocated)
    let times=samples |> Array.map fst |> Array.sort
    let allocations=samples |> Array.map snd |> Array.sort
    {Name=name;MedianMs=(times[29]+times[30])/2.;P95Ms=times[56];MedianBytes=allocations[30];LimitMs=limit;LimitBytes=bytes;Attempts=attempt}

/// <summary>Measures a contract. A p95 over the time limit gets up to two more sampling windows and the fastest window counts:
/// a shared CI runner can stall for a second or two, which spoils one window's tail, while a real regression is slow in every
/// window. Allocation is never retried, because it does not depend on the machine's load.</summary>
let measure name limit bytes action =
    for _ in 1..3 do action()
    let mutable result=window name limit bytes 1 action
    while result.P95Ms>limit && result.Attempts<3 do
        printfn "%s: p95 %.3f ms is over the %.3f ms limit; sampling a fresh window" name result.P95Ms limit
        Thread.Sleep 250
        let retry=window name limit bytes (result.Attempts+1) action
        result<-if retry.P95Ms<result.P95Ms then retry else {result with Attempts=retry.Attempts}
    printfn "%s: median %.3f ms, p95 %.3f ms, %d bytes" name result.MedianMs result.P95Ms result.MedianBytes
    result

/// <summary>The contracts that breach a limit after their retries.</summary>
let failures (results:Measurement seq) =
    results |> Seq.filter(fun result -> result.P95Ms>result.LimitMs || result.MedianBytes>result.LimitBytes) |> Seq.toArray

/// <summary>The failure line CI shows for a breached contract.</summary>
let describe result =
    sprintf "FAIL %s: p95 %.3f ms (limit %.3f) after %d window(s); allocation %d bytes (limit %d)"
        result.Name result.P95Ms result.LimitMs result.Attempts result.MedianBytes result.LimitBytes

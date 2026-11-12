module Ranvier.D5.Benchmarks.Program

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

type Measurement = { Name: string; MedianMs: float; P95Ms: float; MedianBytes: int64; LimitMs: float; LimitBytes: int64 }

let measure name limit bytes action =
    for _ in 1..3 do action()
    let samples =
        Array.init 60 (fun _ ->
            let allocated = GC.GetAllocatedBytesForCurrentThread()
            let watch = Stopwatch.StartNew()
            action()
            watch.Stop()
            watch.Elapsed.TotalMilliseconds, GC.GetAllocatedBytesForCurrentThread() - allocated)
    let times = samples |> Array.map fst |> Array.sort
    let allocations = samples |> Array.map snd |> Array.sort
    let result = { Name=name; MedianMs=(times[29]+times[30])/2.; P95Ms=times[56]; MedianBytes=allocations[30]; LimitMs=limit; LimitBytes=bytes }
    printfn "%s: median %.3f ms, p95 %.3f ms, %d bytes" name result.MedianMs result.P95Ms result.MedianBytes
    result

[<EntryPoint>]
let main args =
    let data = [0..9999]
    let reversed = List.rev data
    let scale = Scale.linear (0.,10000.) (0.,800.)
    let points = [|for i in 0..9999 -> Point.create (float(i % 100)) (float(i / 100))|]
    let spatial = Spatial.create id points
    let results = ResizeArray<Measurement>()
    results.Add(measure "join-10000" 50. 12000000L (fun () -> Data.join id data reversed |> ignore))
    results.Add(measure "scale-100000" 50. 12000000L (fun () -> for i in 0..99999 do Scale.map scale (float i) |> ignore))
    results.Add(measure "nearest-1000-in-10000" 30. 12000000L (fun () -> for i in 0..999 do Spatial.nearest points[i] infinity spatial |> ignore))
    results.Add(measure "line-10000" 60. 16000000L (fun () -> Shape.line Curve.Linear (points |> Array.map Some |> Array.toList) |> Path.toSvg |> ignore))
    let curvePoints=points |> Array.map Some |> Array.toList
    results.Add(measure "natural-10000" 100. 16000000L (fun () -> Curves.line CurveSpec.Natural curvePoints |> ignore))
    let observations = Array.init 100000 (fun i -> float(i % 1000))
    results.Add(measure "statistics-100000" 100. 16000000L (fun () -> Statistics.variance observations |> ignore; Statistics.histogram BinRule.FreedmanDiaconis observations |> ignore))
    use host = WebCoreHost.Create(800, 600)
    use graph = new Graph()
    let container = host.Document.createElement "div"
    host.Document.body.appendChild container |> ignore
    let make offset =
        [for i in 0..499 ->
            Shape.rectangle (Bounds.create (float(i % 25)*30.) (float(i / 25)*25. + offset) 20. 15.)
            |> Mark.path (string i) |> Mark.withFill (Paint.rgb 45 120 210)]
        |> Scene.create 780. 580. "Benchmark"
    let source = graph.Run(fun () -> Api.createSignal (make 0.))
    let mounted = Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                      (fun context -> Program.run (Component.render Renderer.passive (fun () -> source.Value)) context.Backend)
    host.Cycle()
    let pixels = Array.zeroCreate<byte> (800*600*4)
    let mutable offset = 0.
    results.Add(measure "native-retained-500-paint" 150. 32000000L (fun () ->
        offset <- 1. - offset
        graph.Run(fun () -> source.Value <- make offset)
        host.Cycle()
        host.Paint pixels))
    mounted.Dispose()
    let output = if args.Length > 0 then args[0] else "artifacts/benchmarks.json"
    let directory = Path.GetDirectoryName(Path.GetFullPath output)
    Directory.CreateDirectory directory |> ignore
    File.WriteAllText(output, JsonSerializer.Serialize(results, JsonSerializerOptions(WriteIndented=true)))
    if results |> Seq.exists(fun r -> r.P95Ms > r.LimitMs || r.MedianBytes > r.LimitBytes) then 1 else 0

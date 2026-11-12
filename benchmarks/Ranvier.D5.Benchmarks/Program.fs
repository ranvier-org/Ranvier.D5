module Ranvier.D5.Benchmarks.Program

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.D5.Charts
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
    let baselineOnly=Array.contains "--baseline-only" args
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
    if not baselineOnly then
        let hierarchyRows=[for i in 0..9999 -> string i,(if i=0 then None else Some(string((i-1)/4)))]
        let hierarchy=Hierarchy.stratify fst snd hierarchyRows |> Result.defaultWith(fun e -> failwithf "%A" e)
        results.Add(measure "hierarchy-build-10000" 20. 4000000L (fun () -> Hierarchy.stratify fst snd hierarchyRows |> Result.defaultWith(fun e -> failwithf "%A" e) |> ignore))
        results.Add(measure "tidy-tree-10000" 60. 12000000L (fun () -> Tree.layout (TreeConfig.create(Bounds.create 0. 0. 800. 600.)) hierarchy |> Result.defaultWith(fun e -> failwithf "%A" e) |> ignore))
        let wideHierarchy=Hierarchy.node "root" 0. [for i in 0..9999 -> Hierarchy.node (string i) (float(i%17+1)) []]
        results.Add(measure "treemap-squarified-10000" 75. 20000000L (fun () -> Treemap.layout (TreemapConfig.create(Bounds.create 0. 0. 800. 600.)) wideHierarchy |> Result.defaultWith(fun e -> failwithf "%A" e) |> ignore))
        results.Add(measure "pack-wide-10000" 150. 15000000L (fun () -> Pack.layout (PackConfig.create(Bounds.create 0. 0. 800. 600.)) wideHierarchy |> Result.defaultWith(fun e -> failwithf "%A" e) |> ignore))
        let hierarchySpec=Ranvier.D5.Charts.HierarchySpec.create (Ranvier.D5.Charts.ChartFrame.create 800. 600. "Hierarchy")
        results.Add(measure "hierarchy-chart-10000" 180. 48000000L (fun () -> Ranvier.D5.Charts.HierarchyChart.treemap hierarchySpec hierarchy |> Result.defaultWith(fun e -> failwithf "%A" e) |> ignore))
        let curvePoints=points |> Array.map Some |> Array.toList
        results.Add(measure "natural-10000" 100. 16000000L (fun () -> Curves.line CurveSpec.Natural curvePoints |> ignore))
        let chartData=[0..999]
        let chartSpec=Ranvier.D5.Charts.ChartSpec.scatter (Ranvier.D5.Charts.ChartFrame.create 800. 600. "Benchmark") string
                        (Ranvier.D5.Charts.Encoding.create "X" (float >> Some)) (Ranvier.D5.Charts.Encoding.create "Y" (fun i -> Some(float(i%100))))
        results.Add(measure "scatter-chart-1000" 100. 16000000L (fun () -> Ranvier.D5.Charts.Chart.scatter chartSpec chartData |> Result.defaultWith (fun e -> failwithf "%A" e) |> ignore))
        let histogramSpec={Ranvier.D5.Charts.HistogramSpec.Frame=chartSpec.Frame;Key=string;Value=Ranvier.D5.Charts.Encoding.create "Value" (float >> Some);Bins=BinRule.Count 100;Color=Paint.currentColor}
        results.Add(measure "histogram-chart-10000" 100. 20000000L (fun () -> Ranvier.D5.Charts.Chart.histogram histogramSpec data |> Result.defaultWith (fun e -> failwithf "%A" e) |> ignore))
        let observations = Array.init 100000 (fun i -> float(i % 1000))
        results.Add(measure "statistics-100000" 100. 16000000L (fun () -> Statistics.variance observations |> ignore; Statistics.histogram BinRule.FreedmanDiaconis observations |> ignore))
        let palette=Chromatic.sequential Ranvier.Anime.ColorInterpolation.Hcl Ranvier.Anime.GamutPolicy.Clip Chromatic.blues
        results.Add(measure "chromatic-10000" 100. 16000000L (fun () -> for i in 0..9999 do palette (float i/9999.) |> ignore))
        let transitionMarks=[for i in 0..499 -> Shape.rectangle(Bounds.create (float i) 0. 10. 10.) |> Mark.path(string i)]
        let transition=KeyedTransition.start 0. transitionMarks |> KeyedTransition.retarget 0. 100. id false (transitionMarks |> List.map(Mark.withTransform(Transform.translate 5. 5.)))
        results.Add(measure "transition-sample-500" 100. 16000000L (fun () -> KeyedTransition.sample 50. transition |> ignore))
        let gestureConfig={Mode=GestureMode.Lasso;MinZoom=0.1;MaxZoom=10.;WheelSensitivity=0.01;MaxLassoPoints=128}
        results.Add(measure "gesture-lasso-10000" 100. 32000000L (fun () ->
            let mutable state=Gesture.update gestureConfig (InputEvent.Down(1,Point.create 0. 0.,None)) Gesture.empty |> fst
            for i in 1..10000 do state<-Gesture.update gestureConfig (InputEvent.Move(1,Point.create (float i) (float(i%100)))) state |> fst))
    use host = WebCoreHost.Create(800, 600)
    use graph = new Graph()
    if not baselineOnly then
        use releaseWorker=new System.Threading.ManualResetEventSlim(false)
        let workerInput=graph.Run(fun () -> Api.createSignal 0)
        use worker=graph.Run(fun () -> ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> workerInput.Value) (fun _ value -> releaseWorker.Wait();Ok value))
        let mutable requestId=0
        results.Add(measure "worker-request-burst-1000" 100. 16000000L (fun () ->
            graph.Run(fun () -> for _ in 1..1000 do requestId<-requestId+1;workerInput.Value<-requestId)))
        releaseWorker.Set()
        let workerTimeout=Stopwatch.StartNew()
        while graph.Run(fun () -> worker.LastGood<>Some requestId) && workerTimeout.Elapsed.TotalSeconds<5. do
            host.Cycle()
            host.Frame()
            System.Threading.Thread.Yield() |> ignore
        if graph.Run(fun () -> worker.LastGood<>Some requestId) then failwith "Worker failed to publish latest request."
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

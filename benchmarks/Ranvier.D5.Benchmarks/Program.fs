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
        results.Add(measure "orientation-100000" 10. 1000000L (fun () -> for i in 0..99999 do Predicates.orient2d points[i%9998] points[i%9998+1] (Point.create 200. 300.) |> ignore))
        results.Add(measure "orientation-exact-10000" 30. 10000000L (fun () -> for _ in 0..9999 do Predicates.orient2d (Point.create 0. 0.) (Point.create 1e-300 0.) (Point.create 0. 1e-300) |> ignore))
        results.Add(measure "polygon-hull-10000" 60. 20000000L (fun () -> Polygon.hull (Array.toList points) |> ignore))
        let quadPoints=Array.init 100000 (fun i -> Point.create (float(i%1000)) (float(i/1000)))
        results.Add(measure "quadtree-build-100000" 120. 60000000L (fun () -> Quadtree.build id (fun _ -> 1.) quadPoints |> ignore))
        let quad=Quadtree.build id (fun _ -> 1.) quadPoints
        results.Add(measure "quadtree-nearest-1000-in-100000" 25. 4000000L (fun () ->
            for i in 0..999 do Quadtree.nearest (Point.create (float(i%1000)+0.2) (float(i%100)+0.3)) infinity quad |> ignore))
        results.Add(measure "delaunay-build-100000" 1000. 300000000L (fun () -> Delaunay.build (Array.toList quadPoints) |> ignore))
        let mesh=Delaunay.build (Array.toList points)
        results.Add(measure "voronoi-clip-10000" 500. 150000000L (fun () -> Voronoi.clip (Bounds.create 0. 0. 100. 100.) mesh |> ignore))
        let forceGraph=GraphData.create [for i in 0..999 -> GraphNode.create(string i)]
                            [for i in 0..999 do for jump in [1;7] -> GraphLink.create (string i+"/"+string jump) (string i) (string((i+jump)%1000))]
                       |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let force=Force.initialize (ForceConfig.create(Point.create 0. 0.)) (Seed.create 42u) forceGraph |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        results.Add(measure "force-links-1000-nodes-2000-edges-10-steps" 8. 3000000L (fun () -> Force.step 10 force |> ignore))
        let spatialConfig={ForceConfig.create(Point.create 0. 0.) with
                            ManyBody=Some {ManyBodyConfig.create -5. with Theta=0.5}
                            Collision=Some(CollisionConfig.create 0.7)}
        let spatialForce=Force.initialize spatialConfig (Seed.create 42u) forceGraph |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        results.Add(measure "force-spatial-1000-nodes-2000-edges-step" 25. 8000000L (fun () -> Force.step 1 spatialForce |> ignore))
        let smallGraph=GraphData.create [for i in 0..399 -> GraphNode.create(string i)] [] |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        for theta,label in [0.,"exact";0.5,"barnes-hut"] do
            let state=Force.initialize {spatialConfig with Collision=None;ManyBody=Some {ManyBodyConfig.create -5. with Theta=theta}} (Seed.create 42u) smallGraph |> Result.defaultWith(fun errors -> failwithf "%A" errors)
            results.Add(measure ("force-"+label+"-400-step") 20. 3000000L (fun () -> Force.step 1 state |> ignore))
        let chordMatrix=[for i in 0..255 -> [for j in 0..255 -> float((i+j)%17+1)]]
        results.Add(measure "chord-directed-256" 200. 96000000L (fun () ->
            Chord.layout {ChordConfig.defaultValue with Mode=ChordMode.Directed} chordMatrix |> Result.defaultWith(fun errors -> failwithf "%A" errors) |> ignore))
        let ribbonFlow=(Chord.layout ChordConfig.defaultValue [[1.;2.];[3.;4.]] |> Result.defaultWith(fun errors -> failwithf "%A" errors)).Flows.Head
        results.Add(measure "ribbons-10000-svg" 100. 48000000L (fun () ->
            for _ in 1..10000 do Ribbon.path (RibbonSpec.create 100. ribbonFlow) |> Path.toSvg |> ignore))
        let flowNodes=[for i in 0..999 -> string i]
        let flowEdges=[for e in 0..1999 -> let i=e%900 in {FlowLink.Id=string e;Source=string i;Target=string((i/100+1)*100+(i+e/900)%100);Value=float(1+e%7)}]
        let flowGraph=FlowGraph.create flowNodes flowEdges |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let sankeyConfig={SankeyConfig.create(Bounds.create 0. 0. 800. 500.) with NodePadding=1.}
        results.Add(measure "sankey-1000-nodes-2000-links" 25. 8000000L (fun () ->
            Sankey.layout sankeyConfig flowGraph |> Result.defaultWith(fun errors -> failwithf "%A" errors) |> ignore))
        let nextDensity=Ranvier.Anime.Utils.createSeededRandom (Some 42.) (Some 0.) (Some 1.) (Some 12)
        let densityPoints=[for i in 0..99999 ->
                                {WeightedPoint.Id=string i;Position=Point.create (nextDensity None None None*1024.) (nextDensity None None None*1024.);Weight=1.}]
        for size,limit,bytes in [256,125.,24000000L;1024,250.,80000000L] do
            let cfg={DensityConfig.create (Bounds.create 0. 0. 1024. 1024.) size size with Bandwidth=2.*1024./float size}
            results.Add(measure (sprintf "density-100000-grid-%d" size) limit bytes (fun () ->
                let grid=Density.grid cfg densityPoints |> Result.defaultWith(fun errors -> failwithf "%A" errors)
                if DensityGrid.columns grid<>size || DensityGrid.rows grid<>size then failwith "Density output exceeds requested bounds."))
        for size,limit,bytes in [256,75.,24000000L;1024,250.,100000000L] do
            let field=DensityGrid.create (Bounds.create 0. 0. 1. 1.) size size
                          [for row in 0..size-1 do
                              for column in 0..size-1 ->
                                  let x,y=float column/float size-0.5,float row/float size-0.5
                                  Some(x*x+y*y)]
                      |> Result.defaultWith(fun errors -> failwithf "%A" errors)
            results.Add(measure (sprintf "contour-grid-%d-three-levels" size) limit bytes (fun () ->
                let polygons=Contour.compute (ContourConfig.create [0.025;0.075;0.15]) field |> Result.defaultWith(fun errors -> failwithf "%A" errors)
                if polygons.Length<>3 || polygons |> List.exists(fun polygon -> polygon.Holes.Length<>1) then failwith "Contour benchmark lost ring topology."))
        let fragmented=DensityGrid.create (Bounds.create 0. 0. 64. 64.) 64 64 [for row in 0..63 do for column in 0..63 -> Some(if (row+column)%2=0 then 1. else -1.)]
                       |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        results.Add(measure "contour-fragments-2048" 100. 32000000L (fun () ->
            let polygons=Contour.compute (ContourConfig.create [0.]) fragmented |> Result.defaultWith(fun errors -> failwithf "%A" errors)
            if polygons.Length<>2048 then failwith "Fragment benchmark lost disconnected saddle components."))
        let kdeSamples=[for i in 0..999 -> float i,1.]
        let kdeQueries=[for i in 0..511 -> float i*2.]
        results.Add(measure "density-kde-1000-by-512" 30. 1000000L (fun () -> Density.kde1d 10. kdeSamples kdeQueries |> Result.defaultWith(fun errors -> failwithf "%A" errors) |> ignore))
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
    if not baselineOnly then
        let topology=GraphData.create
                        [for i in 0..999 -> {GraphNode.create(string i) with Position=Some(Point.create (60.+float(i%40)*17.) (60.+float(i/40)*17.));Radius=2.5}]
                        [for i in 0..999 do for jump in [1;40] -> GraphLink.create (string i+"/"+string jump) (string i) (string((i+jump)%1000))]
                     |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let state=Force.initialize (ForceConfig.create(Point.create 400. 280.)) (Seed.create 42u) topology |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let snapshots=[|Force.snapshot state;Force.step 1 state |> Force.snapshot|]
        let spec={Ranvier.D5.Charts.NetworkSpec.create(Ranvier.D5.Charts.ChartFrame.create 800. 600. "Network benchmark") with AggregateLinks=true}
        let build snapshot=Ranvier.D5.Charts.NetworkChart.network spec topology snapshot |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        results.Add(measure "network-chart-1000-nodes-2000-edges" 50. 16000000L (fun () -> build snapshots[0] |> ignore))
        let scene=graph.Run(fun () -> Api.createSignal((build snapshots[0]).Scene))
        let networkMount=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                            (fun context -> Program.run (Component.render Renderer.passive (fun () -> scene.Value)) context.Backend)
        host.Cycle()
        let mutable index=0
        results.Add(measure "native-network-1000-nodes-2000-edges-paint" 100. 32000000L (fun () ->
            index<-1-index
            graph.Run(fun () -> scene.Value<-(build snapshots[index]).Scene)
            host.Cycle()
            host.Paint pixels))
        networkMount.Dispose()
        let flowTopology=FlowGraph.create ("source"::[for i in 0..199 -> string i])
                            [for i in 0..399 -> {FlowLink.Id=string i;Source="source";Target=string(i%200);Value=1.}]
                         |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let flowFrame=Ranvier.D5.Charts.ChartFrame.create 800. 600. "Flow benchmark"
        let flowBounds=Ranvier.D5.Charts.ChartLayout.plot flowFrame |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let flowConfig={SankeyConfig.create flowBounds with NodePadding=1.;Iterations=0}
        let initialFlow=Sankey.layout flowConfig flowTopology |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let sourceNode=initialFlow.Nodes |> List.find(fun node -> node.Id="source")
        let move ()=Sankey.moveNode flowConfig "source" (Bounds.y sourceNode.Bounds+1.) initialFlow |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let movedFlow=move()
        if (movedFlow.Nodes |> List.find(fun node -> node.Id="source") |> fun node -> Bounds.y node.Bounds)=Bounds.y sourceNode.Bounds then failwith "Flow benchmark must move a node."
        results.Add(measure "flow-move-201-nodes-400-links" 2. 256000L (fun () -> move() |> ignore))
        let flowSpec={Ranvier.D5.Charts.FlowSpec.create flowFrame with ShowLabels=false}
        let buildFlow layout=Ranvier.D5.Charts.FlowChart.sankey flowSpec layout |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        results.Add(measure "flow-chart-201-nodes-400-links" 10. 8000000L (fun () -> buildFlow initialFlow |> ignore))
        let layouts=[|initialFlow;movedFlow|]
        let flowScene=graph.Run(fun () -> Api.createSignal((buildFlow initialFlow).Scene))
        let flowMount=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                          (fun context -> Program.run (Component.render Renderer.passive (fun () -> flowScene.Value)) context.Backend)
        host.Cycle()
        let mutable flowIndex=0
        results.Add(measure "native-flow-201-nodes-400-links-paint" 100. 32000000L (fun () ->
            flowIndex<-1-flowIndex
            graph.Run(fun () -> flowScene.Value<-(buildFlow layouts[flowIndex]).Scene)
            host.Cycle()
            host.Paint pixels))
        flowMount.Dispose()
    let output = if args.Length > 0 then args[0] else "artifacts/benchmarks.json"
    let directory = Path.GetDirectoryName(Path.GetFullPath output)
    Directory.CreateDirectory directory |> ignore
    File.WriteAllText(output, JsonSerializer.Serialize(results, JsonSerializerOptions(WriteIndented=true)))
    if results |> Seq.exists(fun r -> r.P95Ms > r.LimitMs || r.MedianBytes > r.LimitBytes) then 1 else 0

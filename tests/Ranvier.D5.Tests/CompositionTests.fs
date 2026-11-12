module Ranvier.D5.Tests.CompositionTests

open System
open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let frame=ChartFrame.create 500. 400. "Composition"
let ok = function Ok value -> value | Error e -> failwithf "%A" e
let tests = testList "Radial and composition recipes" [
    testCase "isobands have disjoint picking and valid anchors without intermediate samples" <| fun _ ->
        let grid=DensityGrid.create (Bounds.create 0. 0. 2. 2.) 2 2 [Some 0.;Some 10.;Some 0.;Some 10.] |> ok
        let localFrame={ChartFrame.create 200. 200. "bands" with Margin={Top=0.;Right=0.;Bottom=0.;Left=0.}}
        let spec=DensityChartSpec.create localFrame (fun _ -> Paint.currentColor)
        let result=ContourChart.isobands spec (ContourConfig.create [2.;8.]) grid |> ok
        Expect.equal result.Chart.HitTargets.Length 2 "lower band and unbounded top band"
        for target in result.Chart.HitTargets do
            match target.Geometry with
            | HitRegion(_,_,anchor) ->
                Expect.isTrue (HitTest.contains anchor target.Geometry) "anchor belongs to actual band"
                Expect.equal (HitTest.at anchor result.Chart).Length 1 "bands do not overlap"
            | _ -> failwith "region"
        Expect.equal (HitTest.at (Point.create 100. 100.) result.Chart).Length 1 "interpolated lower band"
        Expect.equal (HitTest.at (Point.create 145. 100.) result.Chart).Length 1 "upper band"
        Expect.isEmpty (HitTest.at (Point.create 10. 100.) result.Chart) "below lowest threshold"
        let reordered=ContourChart.isobands spec (ContourConfig.create [8.;2.]) grid |> ok
        Expect.equal reordered.Membership result.Membership "threshold order canonical"
    testCase "contour composition preserves holes and removes empty bands" <| fun _ ->
        let values=[for y in 0..4 do for x in 0..4 -> Some(if x=2 && y=2 then 0. else 10.)]
        let grid=DensityGrid.create (Bounds.create 0. 0. 5. 5.) 5 5 values |> ok
        let localFrame={ChartFrame.create 200. 200. "ring" with Margin={Top=0.;Right=0.;Bottom=0.;Left=0.}}
        let spec=DensityChartSpec.create localFrame (fun _ -> Paint.currentColor)
        let result=ContourChart.contours spec (ContourConfig.create [5.]) grid |> ok
        Expect.isEmpty (HitTest.at (Point.create 100. 100.) result.Chart) "central hole"
        Expect.equal (HitTest.at (Point.create 50. 100.) result.Chart).Length 1 "filled shell"
        let constant=DensityGrid.create (Bounds.create 0. 0. 2. 2.) 2 2 (List.replicate 4 (Some 10.)) |> ok
        let bands=ContourChart.isobands spec (ContourConfig.create [2.;8.]) constant |> ok
        Expect.equal bands.Chart.HitTargets.Length 1 "identical superlevels make an empty lower band"

    testCase "generated masked isobands match the contour union without double picking" <| fun _ ->
        let localFrame={ChartFrame.create 200. 200. "bands" with Margin={Top=0.;Right=0.;Bottom=0.;Left=0.}}
        let spec=DensityChartSpec.create localFrame (fun _ -> Paint.currentColor)
        for seed in 0..15 do
            let values=[for i in 0..24 -> if (i+seed)%9=0 then None else Some(float((i*17+seed*11)%19)-9.)]
            let grid=DensityGrid.create (Bounds.create 0. 0. 5. 5.) 5 5 values |> ok
            let config=ContourConfig.create [-2.;2.;5.]
            let contours=Contour.compute config grid |> ok
            let result=ContourChart.isobands spec config grid |> ok
            for y in 0..19 do
                for x in 0..19 do
                    let point=Point.create (float x*0.25+0.125) (float y*0.25+0.125)
                    let project (p:Point)=Point.create (p.X/5.*200.) (p.Y/5.*200.)
                    let screen=Point.create (point.X*40.) (point.Y*40.)
                    let expected=contours |> List.exists(fun p ->
                        p.Threshold= -2. && Polygon.contains screen (List.map project p.Outer) &&
                        not(p.Holes |> List.exists(fun ring -> Polygon.contains screen (List.map project ring))))
                    let hits=HitTest.at screen result.Chart
                    Expect.equal hits.Length (if expected then 1 else 0) (sprintf "same union; disjoint bands: seed %d x %d y %d" seed x y)
    testCase "contour recipes retain membership and handle masks limits and cancellation" <| fun _ ->
        let config=DensityConfig.create (Bounds.create 0. 0. 3. 1.) 3 1
        let grid=Density.grid config [{Id="low";Position=Point.create 0.5 0.5;Weight=3.};{Id="high";Position=Point.create 2.5 0.5;Weight=9.}] |> ok
        let spec=DensityChartSpec.create frame (fun _ -> Paint.currentColor)
        let layout=ContourConfig.create [2.;8.]
        let result=ContourChart.isobands spec layout grid |> ok
        Expect.equal (result.Membership |> Map.toList |> List.collect snd |> List.sort) ["high";"low"] "disjoint memberships conserve sources above cutoff"
        let changed=ContourChart.isobands {spec with Frame={frame with Viewport={Width=900.;Height=700.}}} layout grid |> ok
        Expect.equal changed.Membership result.Membership "resize retains level identity"
        let missing=DensityGrid.create config.Bounds 3 1 [None;None;None] |> ok
        Expect.isEmpty (ContourChart.isobands spec layout missing |> ok).Chart.HitTargets "all missing"
        Expect.isEmpty (ContourChart.contours spec (ContourConfig.create []) grid |> ok).Chart.HitTargets "empty thresholds"
        Expect.isError (ContourChart.isobands spec (ContourConfig.create [2.;2.]) grid) "duplicate thresholds"
        Expect.isError (ContourChart.isobands spec {layout with MaxWork=1L} grid) "work limit"
        match ContourChart.isobands {spec with MaxMarks=1} layout grid with
        | Error [BudgetExceeded _] -> ()
        | other -> failwithf "Expected visible mark limit: %A" other
        use cancel=new System.Threading.CancellationTokenSource()
        cancel.Cancel()
        Expect.throwsT<System.OperationCanceledException> (fun () -> ContourChart.isobandsWithCancellation cancel.Token spec layout grid |> ignore) "pre-cancel"
        use during=new System.Threading.CancellationTokenSource()
        let cancelling={spec with Color=(fun _ -> during.Cancel();Paint.currentColor)}
        Expect.throwsT<System.OperationCanceledException> (fun () -> ContourChart.contoursWithCancellation during.Token cancelling layout grid |> ignore) "composition cancel"
    testCase "density recipes observe cancellation before and during composition" <| fun _ ->
        use cancel=new System.Threading.CancellationTokenSource()
        let grid=DensityGrid.create (Bounds.create 0. 0. 10. 10.) 2 2 [Some 1.;Some 2.;Some 3.;Some 4.] |> ok
        let spec=DensityChartSpec.create frame (fun _ -> Paint.currentColor)
        cancel.Cancel()
        Expect.throwsT<System.OperationCanceledException> (fun () -> DensityChart.heatmapWithCancellation cancel.Token spec grid |> ignore) "heatmap pre-cancel"
        Expect.throwsT<System.OperationCanceledException> (fun () -> DensityChart.hexbinWithCancellation cancel.Token spec (HexbinConfig.create (Bounds.create 0. 0. 10. 10.) 1.) [] |> ignore) "hex pre-cancel"
        use during=new System.Threading.CancellationTokenSource()
        let mutable calls=0
        let cancelling={spec with Color=(fun _ -> calls<-calls+1;during.Cancel();Paint.currentColor)}
        Expect.throwsT<System.OperationCanceledException> (fun () -> DensityChart.heatmapWithCancellation during.Token cancelling grid |> ignore) "cancelled output never returned"
        Expect.isLessThanOrEqual calls 2 "stop during composition"
    testCase "density heatmap output limits use typed budget errors" <| fun _ ->
        let grid=DensityGrid.create (Bounds.create 0. 0. 2. 1.) 2 1 [Some 0.;Some 1.] |> ok
        let spec={DensityChartSpec.create frame (fun _ -> Paint.currentColor) with MaxMarks=1}
        match DensityChart.heatmap spec grid with
        | Error [BudgetExceeded _] -> ()
        | result -> failwithf "Expected output budget error, got %A" result

    testCase "hexbin charts retain aggregate membership and polygon picking" <| fun _ ->
        let config=HexbinConfig.create (Bounds.create 0. 0. 10. 10.) 1.
        let points=[{Id="a";Position=Point.create (3.*sqrt 3.) 3.;Weight=1.};{Id="b";Position=Point.create 5.2 3.1;Weight=2.}]
        let spec=DensityChartSpec.create frame (fun _ -> Paint.currentColor)
        let result=DensityChart.hexbin spec config points |> ok
        Expect.equal result.Membership.Count 1 "one bin"
        Expect.equal (result.Membership |> Map.toList |> List.collect snd) ["a";"b"] "membership"
        match result.Chart.HitTargets.Head.Geometry with
        | HitRegion([polygon],[],center) ->
            Expect.equal (HitTest.at center result.Chart).Length 1 "center"
            let minX=polygon.Outer |> List.minBy(fun p -> p.X) |> fun p -> p.X
            let minY=polygon.Outer |> List.minBy(fun p -> p.Y) |> fun p -> p.Y
            Expect.isEmpty (HitTest.at (Point.create minX minY) result.Chart) "bounding-box corner outside hex"
        | _ -> failwith "polygon region"
        Expect.equal (DensityChart.hexbin spec config (List.rev points) |> ok).Membership result.Membership "reorder"
        Expect.isError (DensityChart.hexbin {spec with MaxMarks=0} config points) "invalid budget"
    testCase "hexbin hit clips bound each hexagon within the plot" <| fun _ ->
        let config=HexbinConfig.create (Bounds.create 0. 0. 10. 10.) 1.
        let points=[for x in 0..9 do for y in 0..9 -> {Id=sprintf "%d,%d" x y;Position=Point.create (float x+0.5) (float y+0.5);Weight=1.}]
        let output=(DensityChart.hexbin (DensityChartSpec.create frame (fun _ -> Paint.currentColor)) config points |> ok).Chart
        let plot=output.PlotBounds
        Expect.isGreaterThan output.HitTargets.Length 1 "several bins"
        for target in output.HitTargets do
            match target.Geometry,target.Clip with
            | HitRegion([polygon],[],anchor),Some clip ->
                let xs,ys=polygon.Outer |> List.map(fun p -> p.X),polygon.Outer |> List.map(fun p -> p.Y)
                let left,top=max (List.min xs) (Bounds.x plot),max (List.min ys) (Bounds.y plot)
                let right,bottom=min (List.max xs) (Bounds.x plot+Bounds.width plot),min (List.max ys) (Bounds.y plot+Bounds.height plot)
                Expect.equal clip (Bounds.create left top (right-left) (bottom-top)) "hexagon bounds intersect plot"
                Expect.isTrue (HitTest.visibleAt anchor target) "anchor remains selectable"
            | other -> failwithf "Unexpected hexbin target %A" other
        let corner=Point.create (Bounds.x plot) (Bounds.y plot)
        let far=output.HitTargets |> List.filter(fun target -> not (HitTest.contains corner target.Geometry))
        Expect.isTrue (far |> List.exists(fun target -> not (HitTest.visibleAt corner target))) "distant targets reject by clip"
    testCase "aggregate identities change when the source lattice changes" <| fun _ ->
        let spec=DensityChartSpec.create frame (fun _ -> Paint.currentColor)
        let points=[{Id="a";Position=Point.create 0.1 0.1;Weight=1.}]
        let config=HexbinConfig.create (Bounds.create 0. 0. 10. 10.) 1.
        let first=DensityChart.hexbin spec config points |> ok
        let second=DensityChart.hexbin spec {config with Radius=2.} points |> ok
        let keys output=output.Membership |> Map.keys |> Set.ofSeq
        Expect.isEmpty (Set.intersect (keys first) (keys second)) "changed radius is a different aggregate"
        let grid bounds=DensityGrid.create bounds 1 1 [Some 1.] |> ok
        let a=DensityChart.heatmap spec (grid (Bounds.create 0. 0. 10. 10.)) |> ok
        let b=DensityChart.heatmap spec (grid (Bounds.create 10. 0. 10. 10.)) |> ok
        Expect.isEmpty (Set.intersect (keys a) (keys b)) "changed source extent is a different cell"
    testCase "density heatmaps retain empty-cell identity and original membership" <| fun _ ->
        let config=DensityConfig.create (Bounds.create 0. 0. 2. 1.) 2 1
        let grid=Density.grid config [{Id="source";Position=Point.create 0.2 0.5;Weight=3.}] |> ok
        let spec=DensityChartSpec.create frame (fun _ -> Paint.currentColor)
        let output=DensityChart.heatmap spec grid |> ok
        Expect.equal output.Chart.HitTargets.Length 2 "zero cell retained"
        Expect.equal (output.Membership |> Map.toList |> List.collect snd) ["source"] "source membership"
        Expect.equal (output.Chart.Semantics |> List.collect (fun s -> s.DataIds) |> Set.ofList) (output.Membership |> Map.keys |> Set.ofSeq) "select aggregates even when empty"
        let larger=DensityChart.heatmap {spec with Frame={frame with Viewport={Width=900.;Height=700.}}} grid |> ok
        Expect.equal larger.Membership output.Membership "resize preserves identities"
        Expect.isError (DensityChart.heatmap {spec with MaxMarks=1} grid) "visible output budget"
    testCase "density heatmaps distinguish missing zero and signed values" <| fun _ ->
        let grid=DensityGrid.create (Bounds.create 0. 0. 3. 1.) 3 1 [None;Some 0.;Some -2.] |> ok
        let values=ResizeArray<float>()
        let spec=DensityChartSpec.create frame (fun value -> values.Add value;Paint.currentColor)
        let output=DensityChart.heatmap spec grid |> ok
        Expect.equal (List.ofSeq values) [0.;-2.] "only missing cells omitted"
        Expect.equal output.Chart.HitTargets.Length 2 "measured zero included"

    testCase "polygon regions preserve holes islands and band subtraction" <| fun _ ->
        let square a b = [Point.create a a;Point.create b a;Point.create b b;Point.create a b]
        let lower={HitPolygon.Outer=square 0. 10.;Holes=[square 4. 6.]}
        let upper={HitPolygon.Outer=square 2. 8.;Holes=[square 3. 7.]}
        let target={MarkPath=["band"];DataIds=["band"];Geometry=HitRegion([lower],[upper],Point.create 1. 1.);Clip=None}
        let output={Scene=Scene.compose frame.Viewport "band" [] [];PlotBounds=Bounds.create 0. 0. 10. 10.;Semantics=[];HitTargets=[target];Description="band"}
        let hit x y = HitTest.at (Point.create x y) output |> List.isEmpty |> not
        Expect.isTrue (hit 1. 1.) "lower region"
        Expect.isFalse (hit 2.5 2.5) "upper region subtracted"
        Expect.isTrue (hit 3.5 3.5) "upper hole restores lower region"
        Expect.isFalse (hit 5. 5.) "lower hole stays empty"
        Expect.isFalse (hit 2. 2.) "upper boundary belongs to upper band"
        Expect.isTrue (hit 0. 0.) "outer boundary included"
        Expect.isFalse (hit 4. 4.) "hole boundary excluded"
        Expect.isFalse (hit 11. 1.) "outside"
        let island={HitPolygon.Outer=square 4.5 5.5;Holes=[]}
        let withIsland={output with HitTargets=[{target with Geometry=HitRegion([lower;island],[],Point.create 1. 1.)}]}
        Expect.equal (HitTest.at (Point.create 5. 5.) withIsland).Length 1 "separate island is unioned"

    testCase "polygon regions translate with facets and respect cell clipping" <| fun _ ->
        let build frame () (_:string list) =
            let polygon={HitPolygon.Outer=[Point.create 10. 10.;Point.create 40. 10.;Point.create 40. 40.;Point.create 10. 40.];Holes=[]}
            let target={MarkPath=["shape"];DataIds=["shape"];Geometry=HitRegion([polygon],[],Point.create 20. 20.);Clip=Some(Bounds.create 15. 15. 20. 20.)}
            Ok {Scene=Scene.compose frame.Viewport "region" [] [];PlotBounds=Bounds.create 0. 0. 50. 50.;Semantics=[];HitTargets=[target];Description="region"}
        let output=Facet.build {Frame=frame;Columns=2;Gap=10.;Domains=FacetDomains.Shared;Domain=ignore;Build=build} ["left",[];"right",[]] |> ok
        for target in output.HitTargets do
            match target.Geometry with
            | HitRegion([polygon],[],anchor) ->
                let first=List.head polygon.Outer
                Expect.equal (HitTest.at anchor output).Length 1 "translated interior"
                Expect.isEmpty (HitTest.at first output) "translated clip excludes edge"
                Expect.equal (anchor.X-first.X,anchor.Y-first.Y) (10.,10.) "anchor follows polygon"
            | _ -> failwith "region"
    testCase "empty reversed and concave polygon regions use geometry rather than bounds" <| fun _ ->
        let ring=[Point.create 0. 0.;Point.create 10. 0.;Point.create 10. 2.;Point.create 2. 2.;Point.create 2. 10.;Point.create 0. 10.]
        let output polygons =
            {Scene=Scene.compose frame.Viewport "region" [] [];PlotBounds=Bounds.create 0. 0. 10. 10.;Semantics=[];Description="region"
             HitTargets=[{MarkPath=["shape"];DataIds=["shape"];Geometry=HitRegion(polygons,[],Point.create 1. 1.);Clip=None}]}
        for vertices in [ring;List.rev ring] do
            let chart=output [{Outer=vertices;Holes=[]}]
            Expect.equal (HitTest.at (Point.create 1. 8.) chart).Length 1 "concave arm"
            Expect.isEmpty (HitTest.at (Point.create 8. 8.) chart) "concavity not a bounding box"
        Expect.isEmpty (HitTest.at (Point.create 1. 1.) (output [])) "empty union"
    testCase "facet hit clips follow each translated plot" <| fun _ ->
        let spec={FacetSpec.Frame=frame;Columns=2;Gap=10.;Domains=FacetDomains.Shared;Domain=ignore;Build=(fun frame () data ->
            let scatter=ChartSpec.scatter frame fst (Encoding.create "X" (snd >> Some)) (Encoding.create "Y" (fun _ -> Some 0.5))
            Chart.scatter {scatter with XDomain=Fixed(0.,1.);YDomain=Fixed(0.,1.)} data)}
        let output=Facet.build spec ["left",["a",1.03];"right",["b",1.03]] |> ok
        for target in output.HitTargets do
            let point=match target.Geometry with HitPoint(point,_) -> point | _ -> failwith "point"
            Expect.isFalse (HitTest.visibleAt point target) "per-cell clip excludes margin"
            Expect.isEmpty (HitTest.at point output) "no invisible target picked"
    testCase "donut rejects negatives and retains semantic slices" <| fun _ ->
        let spec={PieSpec.Frame=frame;Key=fst;Value=Encoding.create "Value" (snd >> Some);Label=fst;Color=ChartSpec.color;InnerRatio=0.5;MeasureLabel=(fun text -> {TextSize.Width=float text.Length*6.;Height=12.})}
        let result=Chart.pie spec ["a",2.;"b",3.] |> ok
        Expect.equal (result.Semantics |> List.collect (fun x -> x.DataIds) |> Set.ofList) (set ["a";"b"]) "slice IDs"
        Expect.isError (Chart.pie spec ["a",-1.]) "negative slice"
        Chart.pie spec [] |> ok |> ignore
        Chart.pie spec ["zero",0.] |> ok |> ignore
        Chart.pie spec ["missing",nan] |> Expect.isError <| "nonfinite rejected"
    testCase "facets namespace shared clip definitions and semantic paths" <| fun _ ->
        let spec={FacetSpec.Frame=frame;Columns=2;Gap=10.;Domains=FacetDomains.Shared;Domain=(fun data -> ChartLayout.domain Auto (data |> List.map (snd >> Some)) |> ok);Build=(fun frame domain data ->
            let bar=ChartSpec.bar frame fst (Encoding.create "Category" fst) (Encoding.create "Value" (snd >> Some))
            Chart.bar {bar with Domain=Fixed domain} data)}
        let result=Facet.build spec ["left",["a",1.];"right",["a",2.]] |> ok
        Expect.equal (Scene.definitions result.Scene |> List.map SceneDefinition.key |> Set.ofList |> Set.count) 2 "definition IDs unique"
        Expect.equal (result.Semantics |> List.map (fun x -> x.MarkPath) |> Set.ofList |> Set.count) 2 "selection paths unique"
    testCase "radial parallel and legends handle empty and one observation" <| fun _ ->
        let value=Encoding.create "Value" (snd >> Some)
        let radial={RadialBarSpec.Frame=frame;Key=fst;Value=value;Color=ChartSpec.color;Encoding=RadiusEncoding.Area;InnerRatio=0.2;Padding=0.1}
        let radar={RadarSpec.Frame=frame;Key=fst;Dimensions=[value;value;value];Domains=[IncludeZero;IncludeZero;IncludeZero];Color=ChartSpec.color}
        let parallelSpec={ParallelSpec.Frame=frame;Key=fst;Dimensions=[value;value];Domains=[Auto;Auto];Color=ChartSpec.color}
        for data in [[];["one",2.]] do
            Chart.radialBar radial data |> ok |> ignore
            Chart.radar radar data |> ok |> ignore
            Chart.parallelCoordinates parallelSpec data |> ok |> ignore
        let item={LegendItem.Id="a";Label="Series A";Color=Paint.currentColor;Visible=true}
        let visible=Legend.categorical frame [item] |> ok
        let hidden=Legend.categorical frame [{item with Visible=false}] |> ok
        Expect.equal (visible.Semantics |> List.map (fun x -> x.MarkPath)) (hidden.Semantics |> List.map (fun x -> x.MarkPath)) "visibility retains legend identity"
        Legend.continuous {Frame=frame;Domain=(0.,10.);Color=(fun _ -> Paint.currentColor);Format=string} |> ok |> ignore
    testCase "calendar uses actual week columns across a year boundary" <| fun _ ->
        let spec={CalendarSpec.Frame=frame;Key=fst;Date=Encoding.create "Date" snd;Value=Encoding.create "Value" (fun _ -> Some 1.);Color=(fun _ -> Paint.currentColor);WeekStartsOn=DayOfWeek.Monday}
        let output=Chart.calendar spec ["a",DateOnly(2024,12,31);"b",DateOnly(2025,1,1)] |> ok
        Expect.equal output.Semantics.Length 2 "both dates"
]

module Ranvier.D5.Tests.DensityDistributionTests

open System
open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private frame=ChartFrame.create 600. 400. "Distributions"
let private group id values : ViolinGroup =
    {Id=id;Observations=values |> List.mapi(fun i value -> {Id=id+string i;Value=value})}
let private spec={ViolinSpec.create frame (-5.,5.) 0.5 with Resolution=101}

let tests=testList "Density distribution recipes" [
    testCase "reduced series retain original statistics even when reduction drops a spike" <| fun _ ->
        let source=[for i in 0..4 -> {SeriesSample.Id=string i;X=float i;Y=Some(if i=2 then 10. else 0.)}]
        let config={ReducedSeriesSpec.create frame 2 with Reduction=SeriesReduction.Lttb}
        let output=ReducedSeriesChart.build config source |> ok
        Expect.equal (output.Visible |> List.map(fun p -> p.Id)) ["0";"4"] "visible endpoints"
        Expect.equal output.Source source "all source observations retained"
        Expect.equal output.Summary {Count=5;Minimum=Some 0.;Maximum=Some 10.;Mean=Some 2.;Median=Some 0.} "original statistics"
        Expect.equal (output.Chart.HitTargets |> List.collect(fun hit -> hit.DataIds)) ["0";"4"] "bounded picking uses original IDs"
    testCase "reduced series preserve gaps and reject invalid order and identity" <| fun _ ->
        let source=[for i in 0..6 -> {SeriesSample.Id=string i;X=float i;Y=if i=3 then None else Some(float(i%3))}]
        let config={ReducedSeriesSpec.create frame 5 with Reduction=SeriesReduction.Lttb}
        let output=ReducedSeriesChart.build config source |> ok
        Expect.equal (output.Visible |> List.map(fun p -> p.Id)) ["0";"2";"3";"4";"6"] "two runs and explicit gap"
        Expect.equal output.Chart.HitTargets.Length 4 "gap not selectable as measurement"
        Expect.isError (ReducedSeriesChart.build config (List.rev source)) "unordered X"
        Expect.isError (ReducedSeriesChart.build config [source.Head;source.Head]) "duplicate IDs"
        Expect.isError (ReducedSeriesChart.build {config with Sampling={config.Sampling with TargetCount=4}} source) "mandatory output budget"
    testCase "reduced series reject collapsed glyphs and retain statistics outside fixed domains" <| fun _ ->
        let source=[{SeriesSample.Id="a";X=0.;Y=Some -1.};{Id="b";X=1.;Y=Some 0.};{Id="c";X=2.;Y=Some 9.}]
        let spec=ReducedSeriesSpec.create frame 3
        Expect.isError (ReducedSeriesChart.build {spec with PointRadius=1e-300} source) "positive radius must not collapse to zero area"
        Expect.isError (ReducedSeriesChart.build {spec with Key=""} source) "blank series identity"
        let output=ReducedSeriesChart.build {spec with XDomain=Fixed(0.,1.);YDomain=Fixed(-1.,1.)} source |> ok
        Expect.equal output.Summary.Maximum (Some 9.) "source summary ignores viewport clipping"
        match (List.last output.Chart.HitTargets).Geometry with
        | HitPoint(point,_) -> Expect.isEmpty (HitTest.at point output.Chart) "clipped point cannot be picked"
        | _ -> failwith "point target"
    testCase "reduced series support empty missing resize and cancellation" <| fun _ ->
        let config=ReducedSeriesSpec.create frame 10
        for data in [[];[{Id="missing";X=0.;Y=None}]] do
            let output=ReducedSeriesChart.build config data |> ok
            Expect.equal output.Summary.Count 0 "no fabricated measurements"
            Expect.isEmpty output.Chart.HitTargets "empty picking"
        let data=[{Id="a";X=0.;Y=Some 2.};{Id="b";X=1.;Y=Some 3.}]
        let first=ReducedSeriesChart.build config data |> ok
        let resized=ReducedSeriesChart.build {config with Frame={frame with Viewport={Width=900.;Height=700.}}} data |> ok
        Expect.equal (resized.Chart.HitTargets |> List.map(fun h -> h.DataIds)) (first.Chart.HitTargets |> List.map(fun h -> h.DataIds)) "resize identity"
        use cancel=new Threading.CancellationTokenSource()
        cancel.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> ReducedSeriesChart.buildWithCancellation cancel.Token config data |> ignore) "pre-cancel"

    testCase "violin summaries and membership use original nonmissing observations" <| fun _ ->
        let output=ViolinChart.build spec [group "a" [Some -1.;None;Some 0.;Some 1.];group "empty" [None]] |> ok
        Expect.equal output.Summaries["a"] {Count=3;Minimum=Some -1.;Maximum=Some 1.;Mean=Some 0.;Median=Some 0.} "original summary"
        Expect.equal output.Summaries["empty"].Count 0 "missing group summary"
        Expect.equal output.Aggregate.Chart.HitTargets.Length 1 "no fabricated empty distribution"
        Expect.equal (output.Aggregate.Membership |> Map.toList |> List.collect snd) ["a0";"a2";"a3"] "source IDs"
        match output.Aggregate.Chart.HitTargets.Head.Geometry with
        | HitRegion(_,_,anchor) -> Expect.equal (HitTest.at anchor output.Aggregate.Chart).Length 1 "peak anchor belongs to violin"
        | _ -> failwith "violin region"
    testCase "violin width normalization is explicit" <| fun _ ->
        let groups=[group "narrow" [Some 0.;Some 0.;Some 0.];group "wide" [Some -2.;Some 0.;Some 2.]]
        let widths output=output.Aggregate.Chart.HitTargets |> List.map(fun target ->
            match target.Geometry with
            | HitRegion([polygon],[],_) -> (polygon.Outer |> List.maxBy(fun p -> p.X)).X-(polygon.Outer |> List.minBy(fun p -> p.X)).X
            | _ -> failwith "polygon")
        let each=ViolinChart.build spec groups |> ok |> widths
        Expect.floatClose Accuracy.medium each[0] each[1] "equal peak widths"
        let shared=ViolinChart.build {spec with Width=SharedPeak} groups |> ok |> widths
        Expect.isGreaterThan shared[0] shared[1] "shared normalized density peak"
    testCase "violin rejects invalid data and bounds work" <| fun _ ->
        let data=[group "a" [Some 1.;Some 2.]]
        Expect.isError (ViolinChart.build {spec with Bandwidth=0.} data) "zero bandwidth"
        Expect.isError (ViolinChart.build {spec with Domain=(1.,1.)} data) "collapsed domain"
        Expect.isError (ViolinChart.build {spec with Resolution=1} data) "resolution"
        Expect.isError (ViolinChart.build spec [group "a" [Some nan]]) "nonfinite observation"
        Expect.isError (ViolinChart.build spec [group "a" [];group "a" []]) "duplicate group"
        Expect.isError (ViolinChart.build spec [{Id="a";Observations=[{Id="same";Value=Some 1.}]};{Id="b";Observations=[{Id="same";Value=Some 2.}]}]) "source IDs are global across groups"
        Expect.isError (ViolinChart.build spec [{Id="a";Observations=[{Id="same";Value=Some 1.};{Id="same";Value=None}]}]) "duplicate observation"
        match ViolinChart.build {spec with MaxSamples=1} data with Error [BudgetExceeded _] -> () | other -> failwithf "%A" other
        Expect.isError (ViolinChart.build {spec with MaxVertices=4} data) "visible geometry budget"
        Expect.isError (ViolinChart.build {spec with MaxWork=1L} data) "work budget"
    testCase "violin rejects unrepresentable query and screen resolution" <| fun _ ->
        let data=[group "a" [Some 1.]]
        Expect.isError (ViolinChart.build {spec with Domain=(1.,Math.BitIncrement 1.)} data) "distinct query samples must be representable"
        let tiny={frame with Viewport={Width=600.;Height=Double.Epsilon};Margin={Top=0.;Right=0.;Bottom=0.;Left=0.}}
        Expect.isError (ViolinChart.build {spec with Frame=tiny} data) "distinct projected queries must be representable"
    testCase "violin and series retain finite summaries across extreme ranges" <| fun _ ->
        for magnitude in [1e-300;1e300] do
            let values=[Some -magnitude;Some 0.;Some magnitude]
            let extreme={spec with Domain=(-2.*magnitude,2.*magnitude);Bandwidth=magnitude/4.;Resolution=17}
            let output=ViolinChart.build extreme [group "extreme" values] |> ok
            Expect.equal output.Summaries["extreme"].Mean (Some 0.) "finite cancellation"
            Expect.equal output.Aggregate.Chart.HitTargets.Length 1 "finite density region"
            let source=values |> List.mapi(fun i value -> {SeriesSample.Id=string i;X=float i;Y=value})
            let series=ReducedSeriesChart.build (ReducedSeriesSpec.create frame 3) source |> ok
            Expect.equal series.Summary.Mean (Some 0.) "series summary remains finite"
        use cancel=new Threading.CancellationTokenSource()
        let mutable colors=0
        let cancelling={spec with Color=(fun _ -> colors<-colors+1;cancel.Cancel();Paint.currentColor)}
        Expect.throwsT<OperationCanceledException> (fun () -> ViolinChart.buildWithCancellation cancel.Token cancelling [group "a" [Some 0.]] |> ignore) "cancel during composition"
        Expect.equal colors 1 "color reached before cancellation"
    testCase "violin handles empty input reordering resizing and cancellation" <| fun _ ->
        Expect.isEmpty (ViolinChart.build spec [] |> ok).Aggregate.Chart.HitTargets "empty"
        let data=[group "a" [Some -1.;Some 1.];group "b" [Some 0.]]
        let output=ViolinChart.build spec data |> ok
        let reordered=ViolinChart.build {spec with Frame={frame with Viewport={Width=900.;Height=700.}}} (List.rev data) |> ok
        Expect.equal reordered.Aggregate.Membership output.Aggregate.Membership "stable groups"
        use cancel=new Threading.CancellationTokenSource()
        cancel.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> ViolinChart.buildWithCancellation cancel.Token spec data |> ignore) "pre-cancel"
]

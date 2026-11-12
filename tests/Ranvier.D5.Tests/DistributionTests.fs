module Ranvier.D5.Tests.DistributionTests

open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let frame=ChartFrame.create 400. 300. "Distribution"
let ok = function Ok value -> value | Error e -> failwithf "%A" e
let tests = testList "Distribution recipes" [
    testCase "histogram bins retain source IDs" <| fun _ ->
        let spec={HistogramSpec.Frame=frame;Key=fst;Value=Encoding.create "Value" (snd >> Some);Bins=BinRule.Count 2;Color=Paint.rgb 10 50 100}
        let output=Chart.histogram spec ["a",1.;"b",2.;"c",3.] |> ok
        Expect.equal (output.Semantics |> List.collect (fun m -> m.DataIds) |> Set.ofList) (set ["a";"b";"c"]) "aggregate membership"
    testCase "OHLC refuses inconsistent extrema" <| fun _ ->
        let spec={OhlcSpec.Frame=frame;Key=fst;X=Encoding.create "X" (fun _ -> 1.);Values=Encoding.create "OHLC" snd;Candlestick=true;UpColor=Paint.rgb 0 150 0;DownColor=Paint.rgb 200 0 0}
        Expect.isError (Chart.ohlc spec ["a",{Open=2.;High=1.;Low=0.;Close=3.}]) "invalid OHLC"
    testCase "interval waterfall bullet and lollipop handle empty and signed values" <| fun _ ->
        let intervals={ErrorBarSpec.Frame=frame;Key=fst;X=Encoding.create "X" (snd >> float);Interval=Encoding.create "95% interval" (fun (_,v) -> {Lower=float v-1.;Upper=float v+1.;Estimate=float v});Color=Paint.currentColor}
        let waterfall={WaterfallSpec.Frame=frame;Key=fst;Value=Encoding.create "Change" (snd >> float);IsTotal=(fun _ -> false);PositiveColor=Paint.currentColor;NegativeColor=Paint.currentColor}
        let bullet={BulletSpec.Frame=frame;Key=fst;Measure=Encoding.create "Measure" (snd >> float);Target=Encoding.create "Target" (fun _ -> 2.);Ranges=[1.;3.];Color=Paint.currentColor}
        let lollipop={LollipopSpec.Frame=frame;Key=fst;Category=Encoding.create "Category" fst;Value=Encoding.create "Value" (snd >> float >> Some);Color=Paint.currentColor}
        for values in [[];["a",0];["a",2;"b",-1]] do
            for build in [Chart.errorBars intervals;Chart.waterfall waterfall;Chart.bullet bullet;Chart.lollipop lollipop] do
                let output=build values |> ok
                Expect.equal (output.Semantics |> List.collect (fun m -> m.DataIds) |> Set.ofList) (values |> List.map fst |> Set.ofList) "membership"
    testCase "box plot empty samples and explicit whisker policy" <| fun _ ->
        let spec={BoxPlotSpec.Frame=frame;Key=fst;Samples=Encoding.create "Samples" snd;Whiskers=WhiskerPolicy.Tukey 1.5;Color=Paint.rgb 50 100 150}
        Expect.equal (Chart.boxPlot spec ["empty",[]] |> ok).Semantics [] "empty box omitted"
        Expect.isGreaterThan (Chart.boxPlot spec ["one",[1.;2.;3.;100.]] |> ok).Semantics.Length 0 "nonempty output"
]

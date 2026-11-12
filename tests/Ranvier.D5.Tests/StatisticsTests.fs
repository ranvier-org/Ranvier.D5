module Ranvier.D5.Tests.StatisticsTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5

let tests = testList "Statistics" [
    testCase "sample variance agrees with pinned D3 fixtures" <| fun _ ->
        use doc=JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","statistics.json")))
        for item in doc.RootElement.EnumerateArray() do
            let values=item.GetProperty("values").EnumerateArray() |> Seq.map (fun v -> v.GetDouble()) |> Seq.toList
            let expected=item.GetProperty("variance")
            if expected.ValueKind=JsonValueKind.Null then Expect.isNone (Statistics.variance values) "insufficient samples"
            else
                let actual=Statistics.variance values |> Option.get
                let target=expected.GetDouble()
                Expect.isLessThanOrEqual (abs(actual-target)) (max 1e-220 (abs target*1e-12)) "reference variance"
    testCase "extreme constant data does not overflow" <| fun _ ->
        Expect.equal (Statistics.variance [1e308;1e308]) (Some 0.) "constant variance"
        Expect.throws (fun () -> Statistics.variance [-1e308;1e308] |> ignore) "unrepresentable variance"
    testCase "histograms conserve observations and constant data terminates" <| fun _ ->
        for rule in [BinRule.Sturges;BinRule.Scott;BinRule.FreedmanDiaconis] do
            for values in [[];[1.];[1.;1.;1.];[0.;0.;0.;10.];[-1e308;1e308]] do
                let bins=Statistics.histogram rule values
                Expect.equal (bins |> List.sumBy (fun b -> b.Values.Length)) values.Length "mass conservation"
                Expect.isLessThan bins.Length 10001 "bounded"
    testCase "bisectors use explicit tie policy" <| fun _ ->
        let a=[|1.;2.;2.;4.|]
        Expect.equal (Statistics.bisectLeft 2. a) 1 "left duplicate"
        Expect.equal (Statistics.bisectNearest 3. a) (Some 3) "right tie"
        Expect.equal (Statistics.bisectNearest 0. [||]) None "empty"
    testCase "cumulative sums retain low-order compensation" <| fun _ ->
        Expect.equal (Statistics.cumulativeSum [1e16;1.;-1e16] |> List.last) 1. "compensated sum"
]

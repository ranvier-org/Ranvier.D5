module Ranvier.D5.Tests.ChartLayoutTests

open Expecto
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.View

let tests = testList "Chart layout contracts" [
    testCase "overlapping labels skip deterministically" <| fun _ ->
        let labels = [for id in ["a";"b"] -> {LabelRequest.Id=id;Text=id;Position=Point.create 10. 10.;Priority=0}]
        let placed = LabelLayout.place (Bounds.create 0. 0. 100. 100.) (fun _ -> {TextSize.Width=20.;Height=10.}) labels
        Expect.equal (placed |> List.map (fun x -> x.Id)) ["a"] "stable winner"
    testCase "measurement cache accounts for fonts and DPI and releases on disposal" <| fun _ ->
        let mutable calls = 0
        let cache = new TextMeasureCache(2, fun request -> calls <- calls+1; {TextSize.Width=float request.Text.Length;Height=10.})
        let request = {TextMeasureRequest.Text="hello";FontFamily="sans-serif";FontWeight=400;FontSize=12.;DeviceScale=1.;FontGeneration=0L}
        cache.Measure request |> ignore
        cache.Measure request |> ignore
        Expect.equal calls 1 "cached"
        cache.Measure {request with DeviceScale=2.} |> ignore
        cache.Measure {request with FontGeneration=1L} |> ignore
        Expect.equal calls 3 "distinct font and scale generation"
        Expect.equal cache.Count 2 "bounded cache"
        (cache :> System.IDisposable).Dispose()
        Expect.throws (fun () -> cache.Measure request |> ignore) "disposed cache rejects use"
    testCase "label feedback has two passes" <| fun _ ->
        let mutable calls = 0
        let result = LabelLayout.refine 0 (fun state -> calls<-calls+1; state+1) (=)
        Expect.equal calls 2 "bounded even when unstable"
        Expect.equal result 2 "last pass"
]

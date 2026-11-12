module Ranvier.D5.Tests.CompositionTests

open System
open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let frame=ChartFrame.create 500. 400. "Composition"
let ok = function Ok value -> value | Error e -> failwithf "%A" e
let tests = testList "Radial and composition recipes" [
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

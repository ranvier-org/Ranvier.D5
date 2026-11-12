module Ranvier.D5.Tests.ChartRecipeTests

open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

type Datum={Id:string;Category:string;Series:string;X:float option;Y:float option}
let frame=ChartFrame.create 400. 300. "Example"
let data=[{Id="a";Category="A";Series="one";X=Some 1.;Y=Some 2.};{Id="b";Category="B";Series="one";X=Some 2.;Y=Some -1.}]
let ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let tests = testList "Chart recipes" [
    testCase "bar IDs survive reorder and all outputs are accessible" <| fun _ ->
        let spec=ChartSpec.bar frame (fun d -> d.Id) (Encoding.create "Category" (fun d -> d.Category)) (Encoding.create "Value" (fun d -> d.Y))
        let a=Chart.bar spec data |> ok
        let b=Chart.bar spec (List.rev data) |> ok
        Expect.equal (a.Semantics |> List.map (fun s -> s.MarkPath) |> Set.ofList) (b.Semantics |> List.map (fun s -> s.MarkPath) |> Set.ofList) "semantic identity"
        Expect.equal a.Semantics.Length 2 "one semantic mark per datum"
        Expect.isFalse (System.String.IsNullOrWhiteSpace a.Description) "accessible title"
        Expect.isError (Chart.bar spec [data.Head;data.Head]) "duplicate IDs"
        Expect.equal (Chart.bar spec [] |> ok).Semantics [] "empty"
    testCase "line gaps and scatter missing points do not fabricate observations" <| fun _ ->
        let line=ChartSpec.line frame (fun d -> d.Id) (Encoding.create "X" (fun d -> d.X)) (Encoding.create "Y" (fun d -> d.Y))
        let missing={data.Head with Id="missing";X=None;Y=None}
        let output=Chart.line line [data.Head;missing;data[1]] |> ok
        Expect.equal output.HitTargets.Length 2 "two observed points"
        let scatter=ChartSpec.scatter frame (fun d -> d.Id) line.X line.Y
        Expect.equal (Chart.scatter scatter [missing] |> ok).Semantics [] "all missing"
    testCase "Cartesian modes handle empty missing resizing and grouped data" <| fun _ ->
        let grouped=[data.Head;{data.Head with Id="c";Series="two";Y=Some 3.}]
        let bar={ChartSpec.bar frame (fun d -> d.Id) (Encoding.create "Category" (fun d -> d.Category)) (Encoding.create "Value" (fun d -> d.Y)) with Series=(fun d -> d.Series)}
        for mode in [BarMode.Grouped;BarMode.Stacked;BarMode.Normalized] do
            for orientation in [Orientation.Horizontal;Orientation.Vertical] do
                let result=Chart.bar {bar with Mode=mode;Orientation=orientation} grouped |> ok
                Expect.equal result.Semantics.Length 2 "both observations"
        let line=ChartSpec.line frame (fun d -> d.Id) (Encoding.create "X" (fun d -> d.X)) (Encoding.create "Y" (fun d -> d.Y))
        for kind in [LineKind.Line;LineKind.Area 0.;LineKind.StackedArea {Order=StackOrder.Input;Offset=StackOffset.Wiggle}] do
            let result=Chart.line {line with Kind=kind;Series=(fun d -> d.Series)} grouped |> ok
            Expect.equal result.Semantics.Length 2 "both series"
        Expect.isError (Chart.bar {bar with Frame={frame with Viewport={Width=20.;Height=10.}}} data) "invalid resize"
    testCase "normalized bars reject negative observations" <| fun _ ->
        let spec={ChartSpec.bar frame (fun d -> d.Id) (Encoding.create "Category" (fun d -> d.Category)) (Encoding.create "Value" (fun d -> d.Y)) with Mode=BarMode.Normalized}
        Expect.isError (Chart.bar spec data) "negative normalized data"
]

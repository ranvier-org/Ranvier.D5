module Ranvier.D5.Tests.ChordTests

open System
open System.Text.Json
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private number (name:string) (element:JsonElement)=element.GetProperty(name).GetDouble()
let private arc (expected:JsonElement) (actual:ChordArc) =
    Expect.equal actual.Index (expected.GetProperty("index").GetInt32()) "original group identity"
    Expect.floatClose Accuracy.high actual.StartAngle (number "startAngle" expected) "start angle"
    Expect.floatClose Accuracy.high actual.EndAngle (number "endAngle" expected) "end angle"
    Expect.floatClose Accuracy.high actual.Value (number "value" expected) "flow value"

let tests=testList "Chord" [
    testCase "pinned D3 modes and orderings agree" <| fun _ ->
        use fixtures=JsonDocument.Parse(IO.File.ReadAllText(IO.Path.Combine(AppContext.BaseDirectory,"fixtures","chords.json")))
        for sample in fixtures.RootElement.EnumerateArray() do
            let matrix=[for row in sample.GetProperty("matrix").EnumerateArray() -> [for value in row.EnumerateArray() -> value.GetDouble()]]
            let mode=match sample.GetProperty("mode").GetString() with "Directed" -> ChordMode.Directed | "Transposed" -> ChordMode.Transposed | _ -> ChordMode.Undirected
            let order=if sample.GetProperty("sorted").GetBoolean() then ChordOrder.Descending else ChordOrder.Input
            let result=Chord.layout {ChordConfig.defaultValue with Mode=mode;PadAngle=number "padAngle" sample;GroupOrder=order;SubgroupOrder=order;FlowOrder=order} matrix |> ok
            let groups=sample.GetProperty("groups").EnumerateArray() |> Seq.toList
            let flows=sample.GetProperty("chords").EnumerateArray() |> Seq.toList
            Expect.equal result.Groups.Length groups.Length "group count"
            Expect.equal result.Flows.Length flows.Length "flow count"
            List.iter2 arc groups result.Groups
            List.iter2 (fun (expected:JsonElement) (actual:ChordFlow) -> arc (expected.GetProperty("source")) actual.Source;arc (expected.GetProperty("target")) actual.Target) flows result.Flows
    testCase "empty and zero matrices have finite explicit output" <| fun _ ->
        Expect.equal (Chord.layout ChordConfig.defaultValue [] |> ok) {Groups=[];Flows=[]} "empty"
        let result=Chord.layout ChordConfig.defaultValue [[0.;0.];[0.;0.]] |> ok
        Expect.isEmpty result.Flows "zero flows omitted"
        Expect.equal (result.Groups |> List.map(fun group -> group.StartAngle,group.EndAngle)) [0.,0.;Math.PI,Math.PI] "zero groups retain angular slots"
    testCase "invalid observations dimensions padding and budgets are rejected" <| fun _ ->
        for matrix in [[[1.;2.]];[[-1.]];[[nan]];[[infinity]]] do
            Expect.isError (Chord.layout ChordConfig.defaultValue matrix) "invalid matrix"
        Expect.isError (Chord.layout {ChordConfig.defaultValue with PadAngle=4.} [[1.;0.];[0.;1.]]) "infeasible padding"
        Expect.isError (Chord.layout {ChordConfig.defaultValue with MaxCells=3} [[1.;0.];[0.;1.]]) "allocation budget"
    testCase "tiny values preserve angular conservation and sorting preserves flow IDs" <| fun _ ->
        let matrix=[[0.;Double.Epsilon];[Double.Epsilon;0.]]
        let result=Chord.layout {ChordConfig.defaultValue with PadAngle=0.1} matrix |> ok
        Expect.floatClose Accuracy.high (result.Groups |> List.sumBy(fun group -> group.EndAngle-group.StartAngle)) (2.*Math.PI-0.2) "subnormal normalization"
        let changed=Chord.layout {ChordConfig.defaultValue with GroupOrder=ChordOrder.Descending;SubgroupOrder=ChordOrder.Descending} matrix |> ok
        Expect.equal (changed.Flows |> List.map(fun flow -> flow.Id) |> Set.ofList) (result.Flows |> List.map(fun flow -> flow.Id) |> Set.ofList) "semantic flow identities"
    testCase "large totals normalize angles and unrepresentable group totals return errors" <| fun _ ->
        let result=Chord.layout ChordConfig.defaultValue [[1e308;0.];[0.;1e308]] |> ok
        Expect.floatClose Accuracy.high result.Groups[0].EndAngle Math.PI "global sum may overflow but normalized angles remain valid"
        Expect.isError (Chord.layout ChordConfig.defaultValue [[1e308;1e308];[0.;0.]]) "unrepresentable public group value"
    testCase "flow ordering distinguishes overflowing pair totals" <| fun _ ->
        let result=Chord.layout {ChordConfig.defaultValue with FlowOrder=ChordOrder.Descending} [[1.2e308;0.];[0.;1.6e308]] |> ok
        Expect.equal (result.Flows |> List.map(fun flow -> flow.Id)) [struct(1,1);struct(0,0)] "larger self flow sorts first even when doubled totals overflow"
    testCase "large sorted layouts observe mid-run cancellation" <| fun _ ->
        let matrix=[for i in 0..699 -> [for j in 0..699 -> float(1+(i+j)%19)]]
        use cancellation=new Threading.CancellationTokenSource()
        let timer=Diagnostics.Stopwatch.StartNew()
        cancellation.CancelAfter 1
        Expect.throwsT<OperationCanceledException> (fun () ->
            Chord.layoutWithCancellation cancellation.Token {ChordConfig.defaultValue with FlowOrder=ChordOrder.Descending} matrix |> ignore) "cancels validation or inner work"
        Expect.isLessThan timer.Elapsed.TotalSeconds 5. "cooperative cancellation bound"
    testCase "cancellation is observed before layout" <| fun _ ->
        use token=new Threading.CancellationTokenSource()
        token.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> Chord.layoutWithCancellation token.Token ChordConfig.defaultValue [[1.]] |> ignore) "cancellation"
]

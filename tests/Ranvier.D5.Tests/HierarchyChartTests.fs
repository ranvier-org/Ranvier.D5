module Ranvier.D5.Tests.HierarchyChartTests
open Expecto
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private root=Hierarchy.node "root" 0. [Hierarchy.node "a" 0. [Hierarchy.node "leaf" 2. []];Hierarchy.node "b" 1. []]
let private spec=HierarchySpec.create (ChartFrame.create 400. 300. "Hierarchy")
let tests=testList "Hierarchy charts" [
    testCase "packed collapse preserves leaf-only weight policy" <| fun _ ->
        let weighted=Hierarchy.node "root" 0. [Hierarchy.node "a" 100. [Hierarchy.node "leaf" 1. []];Hierarchy.node "b" 1. []]
        let ratio output =
            let radius id=output.HitTargets |> List.pick(fun hit -> match hit.DataIds,hit.Geometry with [key],HitPoint(_,r) when key=id -> Some r | _ -> None)
            (radius "a"/radius "b")**2.
        let before=HierarchyChart.packedBubbles {spec with Padding=0.} weighted |> ok |> ratio
        let after=HierarchyChart.packedBubbles {spec with Padding=0.;Collapsed=set ["a"]} weighted |> ok |> ratio
        Expect.floatClose Accuracy.high after before "collapse must not introduce branch own weight"
    testCase "all hierarchy recipes produce semantic hit targets" <| fun _ ->
        for chart in [HierarchyChart.dendrogram;HierarchyChart.radialTree;HierarchyChart.icicle;HierarchyChart.sunburst;HierarchyChart.packedBubbles;HierarchyChart.treemap] do
            let output=chart spec root |> ok
            Expect.equal (output.Semantics |> List.collect(fun s -> s.DataIds) |> Set.ofList) (set ["root";"a";"leaf";"b"]) "domain IDs"
            Expect.equal output.Semantics.Length output.HitTargets.Length "targets"
            Expect.all output.HitTargets (fun hit -> hit.Clip=Some output.PlotBounds) "clipped"
    testCase "collapse and drilldown retain domain keys through signal updates" <| fun _ ->
        use graph=new Graph()
        graph.Run(fun () ->
            let state=Api.createSignal spec
            let output=Reactive.derive (fun () -> state.Value) (fun config -> HierarchyChart.treemap config root |> ok)
            let path=output.Value.Semantics |> List.find(fun s -> s.DataIds=["a"]) |> fun s -> s.MarkPath
            state.Value<-{spec with Collapsed=set ["a"]}
            Expect.equal (output.Value.Semantics |> List.find(fun s -> s.DataIds=["a"]) |> fun s -> s.MarkPath) path "stable collapsed key"
            Expect.isFalse (output.Value.Semantics |> List.exists(fun s -> s.DataIds=["leaf"])) "descendant hidden"
            state.Value<-{spec with Focus=Some "a"}
            Expect.equal (output.Value.Semantics |> List.collect(fun s -> s.DataIds) |> Set.ofList) (set ["a";"leaf"]) "focus subtree"
            state.Value<-spec
            Expect.isTrue (output.Value.Semantics |> List.exists(fun s -> s.DataIds=["leaf"])) "expand restores")
    testCase "unknown focus and invalid padding return typed errors" <| fun _ ->
        Expect.isError (HierarchyChart.treemap {spec with Focus=Some "missing"} root) "unknown focus"
        Expect.isError (HierarchyChart.sunburst {spec with Padding= -1.} root) "invalid padding"
]

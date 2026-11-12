module Ranvier.D5.Tests.FlowChartTests

open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private frame=ChartFrame.create 500. 360. "Flows"
let private spec={FlowSpec.create frame with ShowLabels=false}
let private config={SankeyConfig.create(ChartLayout.plot frame |> ok) with NodeWidth=20.;NodePadding=12.}
let private graph=FlowGraph.create ["a";"b"] [{FlowLink.Id="a";Source="a";Target="b";Value=2.};{FlowLink.Id="other";Source="a";Target="b";Value=1.}] |> ok

let tests=testList "Flow charts" [
    testCase "Sankey recipe keeps parallel edge keys and native node hits" <| fun _ ->
        let layout=Sankey.layout config graph |> ok
        let chart=FlowChart.sankey spec layout |> ok
        Expect.equal chart.Semantics.Length 4 "two nodes and two edges"
        Expect.equal (chart.Semantics |> List.map(fun semantic -> semantic.MarkPath) |> Set.ofList |> Set.count) 4 "node and edge namespaces"
        Expect.equal chart.HitTargets.Length 2 "node hits"
        let node=layout.Nodes.Head
        let p=Point.create (Bounds.x node.Bounds+10.) (Bounds.y node.Bounds+Bounds.height node.Bounds/2.)
        Expect.isTrue (HitTest.at p chart |> List.exists(fun hit -> hit.DataIds=[node.Id])) "visible node is interactive"
        let invalid={layout with Links=[{layout.Links.Head with Width=nan}]}
        Expect.isError (FlowChart.sankey spec invalid) "invalid geometry"
    testCase "chord domain keys survive matrix permutation and config sorting" <| fun _ ->
        let spec={ChordSpec.create frame with ShowLabels=false}
        let a=FlowChart.chord spec ["a";"b"] [[1.;2.];[3.;4.]] |> ok
        let b=FlowChart.chord {spec with Layout={spec.Layout with GroupOrder=ChordOrder.Descending}} ["b";"a"] [[4.;3.];[2.;1.]] |> ok
        let keys chart=chart.Semantics |> List.map(fun semantic -> semantic.MarkPath) |> Set.ofList
        Expect.equal (keys a) (keys b) "domain IDs not matrix indices key the scene"
        Expect.equal a.HitTargets.Length 2 "group sectors"
        Expect.isError (FlowChart.chord spec ["a";"a"] [[1.;2.];[3.;4.]]) "duplicate domains"
        Expect.isError (FlowChart.chord spec ["a"] [[1.;2.];[3.;4.]]) "matrix identity dimensions"
    testCase "alluvial uses explicit stages and empty inputs stay empty" <| fun _ ->
        let chart=FlowChart.alluvial spec config (Map.ofList ["a",0;"b",2]) graph |> ok
        Expect.equal chart.HitTargets.Length 2 "staged nodes"
        Expect.isError (FlowChart.alluvial spec config (Map.ofList ["a",0;"b",0]) graph) "same-stage link rejected"
        Expect.isEmpty (FlowChart.chord (ChordSpec.create frame) [] [] |> ok).Semantics "empty chord"
        Expect.isEmpty (FlowChart.sankey spec {Nodes=[];Links=[]} |> ok).Semantics "empty flow"
]

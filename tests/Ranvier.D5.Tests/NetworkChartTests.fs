module Ranvier.D5.Tests.NetworkChartTests

open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private spec=NetworkSpec.create(ChartFrame.create 800. 600. "Network")
let private graph=GraphData.create [{GraphNode.create "a" with Position=Some(Point.create 100. 100.);Radius=12.};{GraphNode.create "b" with Position=Some(Point.create 200. 200.)}]
                                    [GraphLink.create "a" "a" "b";GraphLink.create "parallel" "a" "b"] |> ok
let private snapshot=Force.initialize (ForceConfig.create(Point.create 0. 0.)) (Seed.create 42u) graph |> ok |> Force.snapshot

let tests=testList "Network recipes" [
    testCase "network preserves semantic node identities and distinct parallel edges" <| fun _ ->
        let chart=NetworkChart.network spec graph snapshot |> ok
        Expect.equal chart.Semantics.Length 4 "two nodes and two independently keyed links"
        Expect.equal (chart.Semantics |> List.map(fun item -> item.MarkPath) |> Set.ofList |> Set.count) 4 "node and link key spaces are separate"
        Expect.equal chart.HitTargets.Length 2 "node targets"
        let hits=HitTest.at (Point.create 100. 100.) chart
        Expect.isTrue (hits |> List.exists(fun hit -> hit.DataIds=["a"])) "node radius drives hit geometry"
        let reversed=NetworkChart.network spec graph {snapshot with Nodes=List.rev snapshot.Nodes} |> ok
        Expect.equal reversed.Semantics chart.Semantics "snapshot order does not change scene identity"
    testCase "aggregated links preserve node interactions with one shared edge mark" <| fun _ ->
        let chart=NetworkChart.network {spec with AggregateLinks=true} graph snapshot |> ok
        Expect.equal chart.Semantics.Length 3 "one aggregate link and two node marks"
        Expect.equal chart.HitTargets.Length 2 "individual node targets remain"
        Expect.equal chart.Semantics.Head.DataIds ["a";"b"] "aggregate reports distinct endpoint identities"
        let noLinks=GraphData.create (GraphData.nodes graph) [] |> ok
        Expect.equal (NetworkChart.network {spec with AggregateLinks=true} noLinks snapshot |> ok).Semantics.Length 2 "empty edge set has no phantom mark"
    testCase "bubble recipe reuses node radii without graph links" <| fun _ ->
        let chart=NetworkChart.bubbles spec snapshot |> ok
        Expect.equal chart.Semantics.Length 2 "one semantic mark per node"
        Expect.equal chart.HitTargets[0].Geometry (HitPoint(Point.create 100. 100.,12.)) "force radius is the visible radius"
        let empty=NetworkChart.bubbles spec {snapshot with Nodes=[]} |> ok
        Expect.isEmpty empty.Semantics "empty layouts are supported"
    testCase "network rejects stale topology and forged invalid snapshot values" <| fun _ ->
        Expect.isError (NetworkChart.network spec graph {snapshot with Nodes=List.tail snapshot.Nodes}) "missing nodes do not silently discard edges"
        let invalid={snapshot with Nodes=[{snapshot.Nodes[0] with Radius=nan};snapshot.Nodes[1]]}
        Expect.isError (NetworkChart.bubbles spec invalid) "nonfinite radius"
        Expect.isError (NetworkChart.bubbles {spec with LinkWidth= -1.} snapshot) "invalid style"
]

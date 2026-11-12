module Ranvier.D5.Tests.TreemapTests
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private root = Hierarchy.node "root" 0. [Hierarchy.node "a" 1. [];Hierarchy.node "b" 3. [];Hierarchy.node "c" 2. []]
let private area b = Bounds.width b * Bounds.height b
let tests = testList "Treemap tiling" [
    testCase "all tilers conserve area and keep siblings disjoint" <| fun _ ->
        for tiler in [TreemapTiling.Slice;TreemapTiling.Dice;TreemapTiling.SliceDice;TreemapTiling.Binary;TreemapTiling.Squarified] do
            let config={TreemapConfig.create (Bounds.create 0. 0. 400. 300.) with Tiling=tiler}
            let cells=Treemap.layout config root |> ok |> List.tail
            Expect.floatClose Accuracy.high (cells |> List.sumBy(fun c -> area c.Bounds)) 120000. "total area"
            Expect.floatClose Accuracy.high (area cells[1].Bounds / area cells[0].Bounds) 3. "weight ratio"
            for a in cells do
                for b in cells do
                    if a.Id<>b.Id then
                        let overlapX=min (Bounds.x a.Bounds+Bounds.width a.Bounds) (Bounds.x b.Bounds+Bounds.width b.Bounds)-max (Bounds.x a.Bounds) (Bounds.x b.Bounds)
                        let overlapY=min (Bounds.y a.Bounds+Bounds.height a.Bounds) (Bounds.y b.Bounds+Bounds.height b.Bounds)-max (Bounds.y a.Bounds) (Bounds.y b.Bounds)
                        Expect.isTrue (overlapX<1e-8 || overlapY<1e-8) "disjoint"
    testCase "retained state reuses rows and invalidates changed configuration or topology" <| fun _ ->
        let config=TreemapConfig.create (Bounds.create 0. 0. 400. 300.)
        let first,state=Treemap.resquarify config root None |> ok
        let again,_=Treemap.resquarify config root (Some state) |> ok
        Expect.equal first again "same input"
        let reweighted=Hierarchy.node "root" 0. [Hierarchy.node "a" 10. [];Hierarchy.node "b" 1. [];Hierarchy.node "c" 2. []]
        let updated,_=Treemap.resquarify config reweighted (Some state) |> ok
        Expect.floatClose Accuracy.high (area updated[1].Bounds/area updated[2].Bounds) 10. "new weights on retained rows"
        Expect.equal (Treemap.resquarify config root (Some state) |> ok |> fst) first "state remains immutable"
        let resized={config with Bounds=Bounds.create 0. 0. 200. 100.}
        let actual,_=Treemap.resquarify resized root (Some state) |> ok
        Expect.equal actual (Treemap.layout resized root |> ok) "resize resets"
        let changed=Hierarchy.node "root" 0. [Hierarchy.node "new" 1. []]
        let actual,_=Treemap.resquarify config changed (Some state) |> ok
        Expect.equal actual (Treemap.layout config changed |> ok) "topology resets"
    testCase "padding zero weights own weights and deep trees are bounded" <| fun _ ->
        let config=TreemapConfig.create (Bounds.create 0. 0. 100. 100.)
        let own=Hierarchy.node "root" 2. [Hierarchy.node "leaf" 2. []]
        Expect.floatClose Accuracy.high (Treemap.layout config own |> ok |> List.last |> fun c -> area c.Bounds) 5000. "own weight reserves space"
        Expect.floatClose Accuracy.high (Treemap.layout {config with SumPolicy=HierarchySumPolicy.LeavesOnly} own |> ok |> List.last |> fun c -> area c.Bounds) 10000. "leaf policy"
        let zero=Hierarchy.node "root" 0. [Hierarchy.node "a" 0. [];Hierarchy.node "b" 0. []]
        Expect.equal (Treemap.layout {config with Padding=1000.} zero |> ok |> List.map(fun c -> area c.Bounds)) [0.;0.;0.] "padding clamps"
        let deep=[for i in 0..49999 -> string i,if i=0 then None else Some(string(i-1))] |> Hierarchy.stratify fst snd |> ok
        Expect.equal (Treemap.layout config deep |> ok |> List.length) 50000 "stack safe"
]

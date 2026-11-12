module Ranvier.D5.Tests.HierarchyLayoutTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5

let ok=HierarchyBuildTests.ok
let bounds=Bounds.create 0. 0. 400. 300.
let tests=testList "Hierarchy layouts" [
    testCase "tidy tree and cluster match pinned D3 coordinates" <| fun _ ->
        use fixture=JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","hierarchy.json")))
        let rows=fixture.RootElement.GetProperty("rows").EnumerateArray() |> Seq.map(fun row -> row[0].GetString(),(if row[1].ValueKind=JsonValueKind.Null then None else Some(row[1].GetString()))) |> Seq.toList
        let root=Hierarchy.stratify fst snd rows |> ok
        for name,layout in ["tree",Tree.layout;"cluster",Cluster.layout] do
            let actual=layout (TreeConfig.create bounds) root |> ok |> List.map(fun item -> item.Id,item.Point) |> Map.ofList
            for row in fixture.RootElement.GetProperty(name).EnumerateArray() do
                let point=actual[row.GetProperty("id").GetString()]
                DataScaleTests.near (row.GetProperty("x").GetDouble()) point.X
                DataScaleTests.near (row.GetProperty("y").GetDouble()) point.Y
    testCase "partition bands contain children and divide weights" <| fun _ ->
        let root=Hierarchy.node "r" 0. [Hierarchy.node "a" 1. [];Hierarchy.node "b" 3. []]
        let boxes=Partition.layout {Bounds=bounds;Padding=0.} root |> ok |> List.map(fun item -> item.Id,item.Bounds) |> Map.ofList
        DataScaleTests.near 100. (Bounds.width boxes["a"])
        DataScaleTests.near 300. (Bounds.width boxes["b"])
        DataScaleTests.near 150. (Bounds.y boxes["a"])
    testCase "deep chain layouts are finite and stack safe" <| fun _ ->
        let root=Hierarchy.stratify fst snd [for i in 0..49999 -> string i,(if i=0 then None else Some(string(i-1)))] |> ok
        for layout in [Tree.layout;Cluster.layout] do
            let result=layout (TreeConfig.create bounds) root |> ok
            Expect.equal result.Length 50000 "chain size"
            Expect.isTrue (result |> List.forall(fun item -> Bounds.contains item.Point bounds)) "finite bounded points"
        Expect.equal (Partition.layout {Bounds=bounds;Padding=0.} root |> ok).Length 50000 "partition chain"
    testCase "invalid separation is a typed error" <| fun _ ->
        let root=Hierarchy.node "r" 0. [Hierarchy.node "a" 1. [];Hierarchy.node "b" 1. []]
        Expect.isError (Tree.layout {TreeConfig.create bounds with Separation=fun _ _ -> nan} root) "invalid distance"
]

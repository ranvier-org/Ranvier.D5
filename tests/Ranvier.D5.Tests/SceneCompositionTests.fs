module Ranvier.D5.Tests.SceneCompositionTests

open Expecto
open Ranvier.D5

let tests = testList "Scene composition" [
    testCase "axis label anchors keep text outside the plot" <| fun _ ->
        for orientation,anchor in [AxisOrientation.Left,"end";AxisOrientation.Right,"start";AxisOrientation.Bottom,"middle";AxisOrientation.Top,"middle"] do
            let mark=Axis.values "test" orientation 20. (0.,100.) ["one",50.,"1.000"] |> List.last
            Expect.equal (Mark.textStyle mark).Anchor anchor "orientation anchor"
    testCase "explicit fill replaces a gradient reference" <| fun _ ->
        let mark=Mark.text "a" (Point.create 0. 0.) "a" |> Mark.withFillDefinition "paint" |> Mark.withFill Paint.currentColor
        Expect.equal (Mark.fillDefinition mark) None "explicit paint wins"
    testCase "keys are scoped by structured paths" <| fun _ ->
        let leaf = Leaf(Mark.text "same" (Point.create 0. 0.) "label")
        let scene = Scene.compose {Width=100.;Height=100.} "two charts" []
                        [Group(GroupSpec.create "a", [leaf]); Group(GroupSpec.create "b", [leaf])]
        Expect.equal (Scene.nodes scene |> List.length) 2 "independent groups"
        Expect.throws (fun () -> Scene.compose {Width=100.;Height=100.} "" [] [leaf;leaf] |> ignore) "duplicate siblings"
    testCase "clip references and definition IDs are validated" <| fun _ ->
        let group = {GroupSpec.create "plot" with Clip=Some "clip"}
        let build defs = Scene.compose {Width=100.;Height=100.} "" defs [Group(group, [])]
        Expect.throws (fun () -> build [] |> ignore) "missing definition"
        let clip = RectClip("clip", Bounds.create 0. 0. 10. 10.)
        build [clip] |> ignore
        Expect.throws (fun () -> build [clip;clip] |> ignore) "duplicate definitions"
    testCase "semantic axis keys survive changes in tick membership" <| fun _ ->
        let build values = Axis.values "x" AxisOrientation.Bottom 20. (0.,100.)
                             (values |> List.map (fun x -> string x, float x, string x))
        let keys = build [1;2;3] |> List.map Mark.key |> Set.ofList
        let survivor = build [2] |> List.map Mark.key |> Set.ofList
        Expect.isTrue (Set.isSubset survivor keys) "value identity"
]

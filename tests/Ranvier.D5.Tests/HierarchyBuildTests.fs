module Ranvier.D5.Tests.HierarchyBuildTests

open Expecto
open Ranvier.D5

let ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let tests=testList "Hierarchy construction" [
    testCase "parent tables preserve order and reject invalid topology" <| fun _ ->
        let root=Hierarchy.stratify fst snd ["child",Some "root";"root",None;"other",Some "root"] |> ok
        Expect.equal (Hierarchy.descendants root |> List.map(fun n -> n.Id)) ["root";"child";"other"] "input sibling order"
        Expect.isError (Hierarchy.stratify fst snd ["a",None;"a",None]) "duplicates"
        Expect.isError (Hierarchy.stratify fst snd ["a",Some "missing"]) "missing parent"
        Expect.isError (Hierarchy.stratify fst snd ["a",Some null]) "null parent"
        Expect.isError (Hierarchy.stratify fst snd ["a",Some ""]) "empty parent"
        Expect.isError (Hierarchy.stratify fst snd ["a",Some "b";"b",Some "a"]) "cycle"
        Expect.isError (Hierarchy.stratify fst snd ["a",None;"b",None]) "multiple roots"
        Expect.isError (Hierarchy.stratify fst snd ["root",None;"a",Some "b";"b",Some "a"]) "disconnected cycle"
    testCase "explicit virtual root joins forests without identity collisions" <| fun _ ->
        let root=Hierarchy.forest "virtual" fst snd ["a",None;"b",None] |> ok
        Expect.equal (Hierarchy.descendants root |> List.map(fun n -> n.Id)) ["virtual";"a";"b"] "forest"
        Expect.isError (Hierarchy.forest "a" fst snd ["a",None]) "virtual collision"
        Expect.equal (Hierarchy.forest "empty" fst snd [] |> ok |> Hierarchy.descendants |> List.length) 1 "empty explicit forest"
    testCase "fifty thousand deep and ten thousand wide nodes stay stack safe" <| fun _ ->
        let rows=[for i in 0..49999 -> string i,(if i=0 then None else Some(string(i-1)))]
        let items=Hierarchy.stratify fst snd rows |> ok |> Hierarchy.descendants
        Expect.equal items.Length 50000 "chain"
        Expect.equal (List.last items).Depth 49999 "depth"
        let wide=("root",None)::[for i in 0..9999 -> string i,Some "root"]
        Expect.equal (Hierarchy.stratify fst snd wide |> ok |> Hierarchy.descendants |> List.length) 10001 "wide"
]

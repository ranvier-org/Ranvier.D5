module Ranvier.D5.Tests.PackTests
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private distance (a:Point) (b:Point) = sqrt ((a.X-b.X)**2.+(a.Y-b.Y)**2.)
let tests = testList "Circle packing" [
    testCase "extreme weight ratios preserve representable positive radii" <| fun _ ->
        let root=Hierarchy.node "root" 0. [Hierarchy.node "tiny" 1e-300 [];Hierarchy.node "large" 1e300 []]
        let circles=Pack.layout (PackConfig.create(Bounds.create 0. 0. 1000. 1000.)) root |> ok
        let tiny=circles |> List.find(fun circle -> circle.Id="tiny")
        let large=circles |> List.find(fun circle -> circle.Id="large")
        Expect.isGreaterThan tiny.Radius 0. "positive radius survives normalization"
        Expect.floatClose Accuracy.high (tiny.Radius/large.Radius/1e-300) 1. "square-root weight ratio"
    testCase "circles preserve leaf areas and siblings do not overlap" <| fun _ ->
        let root=Hierarchy.node "root" 0. [for i in 1..80 -> Hierarchy.node (string i) (float i) []]
        let config=PackConfig.create (Bounds.create 0. 0. 400. 300.)
        let circles=Pack.layout config root |> ok
        Expect.equal circles (Pack.layout config root |> ok) "deterministic"
        let parent=List.head circles
        for a in List.tail circles do
            Expect.isTrue (distance a.Center parent.Center+a.Radius<=parent.Radius+1e-8) "containment"
            for b in List.tail circles do
                if a.Id<>b.Id then Expect.isTrue (distance a.Center b.Center+1e-8>=a.Radius+b.Radius) "nonoverlap"
        Expect.floatClose Accuracy.high (circles[2].Radius**2./circles[1].Radius**2.) 2. "area proportional to weight"
    testCase "deep and wide packs stay finite and stack safe" <| fun _ ->
        let config=PackConfig.create (Bounds.create 10. 20. 400. 300.)
        let deep=[for i in 0..49999 -> string i,if i=0 then None else Some(string(i-1))] |> Hierarchy.stratify fst snd |> ok
        Expect.equal (Pack.layout config deep |> ok |> List.length) 50000 "deep"
        let wide=Hierarchy.node "root" 0. [for i in 1..10000 -> Hierarchy.node (string i) 1. []]
        let packed=Pack.layout config wide |> ok
        Expect.equal packed.Length 10001 "wide"
        Expect.all packed (fun c -> System.Double.IsFinite c.Radius && Bounds.contains c.Center config.Bounds) "finite bounded"
    testCase "zero weights zero bounds padding and invalid padding are defined" <| fun _ ->
        let root=Hierarchy.node "root" 0. [Hierarchy.node "a" 0. [];Hierarchy.node "b" 0. []]
        let config=PackConfig.create (Bounds.create 0. 0. 0. 0.)
        Expect.all (Pack.layout config root |> ok) (fun c -> c.Radius=0.) "zero bounds"
        Expect.isError (Pack.layout {config with Padding= -1.} root) "negative padding"
        let positive={config with Bounds=Bounds.create 0. 0. 400. 300.;Padding=8.}
        let circles=Pack.layout positive root |> ok
        Expect.isTrue (distance circles[1].Center circles[2].Center>circles[1].Radius+circles[2].Radius) "gap"
]

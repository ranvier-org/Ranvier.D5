module Ranvier.D5.Tests.GeometryTests

open System
open System.Globalization
open Expecto
open Ranvier.D5

let near = DataScaleTests.near
let p x y = Point.create x y

let tests = testList "Geometry and layouts" [
    testCase "extreme but invertible affine scales retain finite inverses" <| fun _ ->
        for x,y in [1e200,1e200; 1e-200,1e-200; 1e200,1e-200] do
            let matrix = Transform.scale x y
            let inverse = Transform.tryInverse matrix
            Expect.isSome inverse "representable inverse"
            let roundtrip = p 1. 1. |> Transform.apply matrix |> Transform.apply inverse.Value
            near 1. roundtrip.X
            near 1. roundtrip.Y
    testCase "mixed-magnitude inverse does not lose small columns" <| fun _ ->
        let matrix = Transform.create 1e308 1e308 1e-308 2e-308 0. 0.
        let inverse = Transform.tryInverse matrix
        Expect.isSome inverse "finite raw determinant remains usable"
        let result = p 1e-309 1e307 |> Transform.apply matrix |> Transform.apply inverse.Value
        near 1. (result.X / 1e-309)
        near 1. (result.Y / 1e307)
    testCase "stepped zero-thickness areas have zero signed area" <| fun _ ->
        for curve in [Curve.StepBefore; Curve.StepAfter] do
            let commands = Shape.area curve [Some(p 0. 0., p 0. 0.); Some(p 10. 10., p 10. 10.)] |> Path.commands
            let vertices = commands |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            let closed = vertices @ [vertices.Head]
            let area = closed |> List.pairwise |> List.sumBy(fun (a,b) -> a.X*b.Y - b.X*a.Y)
            near 0. area
            let shifted = Shape.area curve [Some(p 0. 0., p 0. 5.); Some(p 10. 10., p 10. 15.)] |> Path.commands
            let vertices = shifted |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            let area = (vertices @ [vertices.Head]) |> List.pairwise |> List.sumBy(fun (a,b) -> a.X*b.Y - b.X*a.Y)
            near 100. (abs area)

    testCase "affine composition inverse and singular transform" <| fun _ ->
        let transform = Transform.scale 2. 3. |> Transform.thenApply (Transform.translate 10. 20.)
        Expect.equal (Transform.apply transform (p 1. 2.)) (p 12. 26.) "scale then translate"
        let inverse = Transform.tryInverse transform |> Option.get
        for x in [-100.;0.;100.] do
            let point = Transform.apply transform (p x 3.) |> Transform.apply inverse
            near x point.X
            near 3. point.Y
        Expect.equal (Transform.tryInverse (Transform.scale 0. 1.)) None "singular inverse"
    testCase "path serialization is invariant and validates coordinates" <| fun _ ->
        let before = CultureInfo.CurrentCulture
        try
            CultureInfo.CurrentCulture <- CultureInfo("fr-FR")
            Expect.equal (Path.ofCommands [MoveTo(p 1.5 2.5); LineTo(p 3. 4.);Close] |> Path.toSvg) "M1.5,2.5L3,4Z" "SVG decimals"
        finally CultureInfo.CurrentCulture <- before
        Expect.throws (fun () -> Path.ofCommands [LineTo(p 1. 2.)] |> ignore) "starts with move"
        Expect.throws (fun () -> Path.ofCommands [MoveTo {X=nan;Y=0.}] |> ignore) "finite coordinates"
    testCase "missing observations split line segments" <| fun _ ->
        let line = Shape.line Curve.Linear [Some(p 0. 0.);Some(p 1. 1.);None;Some(p 3. 3.)]
        Expect.equal (Path.toSvg line) "M0,0L1,1M3,3" "gap is not connected"
        Expect.equal (Shape.line Curve.Linear [] |> Path.toSvg) "" "empty line"
        let curve = Shape.line (Curve.CatmullRom 0.) [Some(p 0. 0.);Some(p 10. 10.);Some(p 20. 0.)]
        Expect.stringContains (Path.toSvg curve) "C" "cubic commands"
    testCase "area closes independent gaps" <| fun _ ->
        let area = Shape.area Curve.Linear [Some(p 0. 1.,p 0. 4.);None;Some(p 2. 1.,p 2. 8.)]
        Expect.equal (Path.commands area |> List.filter ((=) Close) |> List.length) 2 "two closed areas"
    testCase "full annulus uses four arcs and rejects inverted radii" <| fun _ ->
        let ring = Shape.arc 5. 10. 0. (2.*Math.PI)
        Expect.equal (Path.commands ring |> List.filter (function ArcTo _ -> true | _ -> false) |> List.length) 4 "outer and inner circles"
        Expect.throws (fun () -> Shape.arc 10. 5. 0. 1. |> ignore) "invalid radii"
        Expect.equal (Shape.arc 0. 0. 0. 1. |> Path.toSvg) "" "zero radius"
    testCase "pie preserves data identity and full angular sum" <| fun _ ->
        let slices = Layout.pie snd 0. (2.*Math.PI) ["a",1.;"b",3.]
        near (Math.PI/2.) slices[0].EndAngle
        near (2.*Math.PI) slices[1].EndAngle
        Expect.equal slices[1].Data ("b",3.) "identity"
        Expect.isEmpty (Layout.pie id 0. 1. []) "empty pie"
        Expect.throws (fun () -> Layout.pie id 0. 1. [-1.] |> ignore) "negative weights"
    testCase "diverging stack independently accumulates positive and negative values" <| fun _ ->
        let layers = Layout.stack ["a",[2.;-3.];"b",[4.;-5.]]
        Expect.equal (layers |> List.map _.Values) [[(0.,2.);(0.,-3.)];[(2.,6.);(-3.,-8.)]] "diverging baselines"
        Expect.throws (fun () -> Layout.stack ["a",[1.];"b",[]] |> ignore) "ragged series"
    testCase "hierarchy layout preserves parent relationships and partition area" <| fun _ ->
        let tree = Hierarchy.node "root" 0. [Hierarchy.node "a" 1. [];Hierarchy.node "b" 3. []]
        Expect.equal (Hierarchy.descendants tree |> List.map _.Id) ["root";"a";"b"] "preorder"
        let boxes = Hierarchy.treemap (Bounds.create 0. 0. 400. 100.) tree
        near 100. (boxes |> List.find (fun item -> item.Id="a") |> fun item -> Bounds.width item.Bounds)
        near 300. (boxes |> List.find (fun item -> item.Id="b") |> fun item -> Bounds.width item.Bounds)
        Expect.throws (fun () -> Hierarchy.node "root" 0. [Hierarchy.node "x" 1. [];Hierarchy.node "x" 2. []] |> Hierarchy.descendants |> ignore) "duplicate ids"
    testCase "spatial index agrees with brute force and handles coincident points" <| fun _ ->
        let points = [for x in 0..30 do for y in 0..30 -> (x,y),p (float x) (float y)]
        let index = Spatial.create snd points
        for x,y in [(0.2,0.4);(12.9,4.1);(29.8,30.4)] do
            let query = p x y
            let expected = points |> List.minBy (fun (_,point) -> Point.distanceSquared point query) |> fst
            Expect.equal (Spatial.nearest query infinity index |> Option.map fst) (Some expected) "nearest"
        Expect.equal (Spatial.within (Bounds.create 0. 0. 1. 1.) index |> List.length) 4 "inclusive range"
        let duplicates = Spatial.create snd [1,p 0. 0.;2,p 0. 0.]
        Expect.equal (Spatial.nearest (p 0. 0.) 0. duplicates |> Option.map fst) (Some 1) "stable distance ties"
        Expect.equal (Spatial.nearest (p 5. 5.) 1. duplicates) None "bounded query"
    testCase "spatial distances avoid squared overflow" <| fun _ ->
        let index = Spatial.create snd [1,p 1e200 0.;2,p 2e200 0.]
        Expect.equal (Spatial.nearest (p 1.9e200 0.) infinity index |> Option.map fst) (Some 2) "nearest at large finite magnitude"
    testCase "invalid curve configuration rejects empty and singleton lines" <| fun _ ->
        Expect.throws (fun () -> Shape.line (Curve.CatmullRom nan) [] |> ignore) "empty input still validates configuration"
        Expect.throws (fun () -> Shape.line (Curve.CatmullRom 2.) [Some(p 0. 0.)] |> ignore) "singleton validates tension"
    testCase "scene rejects duplicate identities and axes compose marks" <| fun _ ->
        let mark = Mark.path "a" (Shape.line Curve.Linear [Some(p 0. 0.);Some(p 1. 1.)])
        Expect.throws (fun () -> Scene.create 100. 100. "Test" [mark;mark] |> ignore) "duplicate marks"
        let axis = Axis.numeric "x" AxisOrientation.Bottom 80. 5 (Scale.linear (0.,10.) (10.,90.)) (fun x -> string x)
        Expect.equal axis.Length 13 "domain plus six lines and six labels"
        let scene = Scene.create 100. 100. "Axis" axis
        Expect.equal (Scene.marks scene |> List.length) 13 "composable axis"
]

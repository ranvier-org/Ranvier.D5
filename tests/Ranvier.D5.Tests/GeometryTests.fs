module Ranvier.D5.Tests.GeometryTests

open System
open System.Globalization
open System.Numerics
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
    testCase "triangular inverse preserves a small shear coefficient" <| fun _ ->
        let matrix = Transform.create 1e308 0. 1e-308 1e-308 0. 0.
        let inverse = Transform.tryInverse matrix |> Option.get
        let mapped = p 0. 1e308 |> Transform.apply matrix
        let again = mapped |> Transform.apply inverse |> Transform.apply matrix
        near mapped.X again.X
        near mapped.Y again.Y
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

    testCase "Point3 vector operators, normalization, Vector3 interop and Bounds3 helpers enforce finiteness invariants" <| fun _ ->
        let a = Point3.create 1. -2. 3.
        let b = Point3.create -4. 5. 6.
        Expect.equal Point3.zero (Point3.create 0. 0. 0.) "Point3.zero"
        Expect.equal Point3.origin Point3.zero "Point3.origin"
        Expect.equal (a + b) (Point3.create -3. 3. 9.) "Point3.(+)"
        Expect.equal (a - b) (Point3.create 5. -7. -3.) "Point3.(-)"
        Expect.equal (a * 2.) (Point3.create 2. -4. 6.) "Point3.(* rhs)"
        Expect.equal (2. * a) (Point3.create 2. -4. 6.) "Point3.(* lhs)"
        Expect.equal (a / 2.) (Point3.create 0.5 -1. 1.5) "Point3.(/)"
        Expect.equal (-a) (Point3.create -1. 2. -3.) "Point3.(~-)"
        Expect.equal (Point3.add a b) (a + b) "Point3.add"
        Expect.equal (Point3.sub a b) (a - b) "Point3.sub"
        Expect.equal (Point3.scale 3. a) (Point3.create 3. -6. 9.) "Point3.scale"
        Expect.floatClose Accuracy.veryHigh (Point3.dot a b) (-4. - 10. + 18.) "Point3.dot"
        Expect.equal (Point3.cross (Point3.create 1. 0. 0.) (Point3.create 0. 1. 0.)) (Point3.create 0. 0. 1.) "Point3.cross i x j = k"
        Expect.floatClose Accuracy.veryHigh (Point3.lengthSquared (Point3.create 2. 3. 6.)) 49. "Point3.lengthSquared"
        Expect.floatClose Accuracy.veryHigh (Point3.length (Point3.create 2. 3. 6.)) 7. "Point3.length"

        let huge = Point3.create 1e200 1e200 1e200
        let hugeLen = Point3.length huge
        Expect.isTrue (Double.IsFinite hugeLen) "Point3.length avoids overflow at 1e200"
        Expect.floatClose Accuracy.high hugeLen (1e200 * sqrt 3.) "Point3.length accurate at 1e200"

        Expect.equal (Point3.tryNormalize Point3.zero) ValueNone "tryNormalize zero returns ValueNone"
        Expect.equal (Point3.tryNormalize (Point3.create 1e-16 0. 0.)) ValueNone "tryNormalize <= 1e-15 returns ValueNone"
        let u = Point3.normalize (Point3.create 3. 4. 0.)
        Expect.floatClose Accuracy.veryHigh (Point3.length u) 1. "Point3.normalize unit length"
        Expect.floatClose Accuracy.veryHigh u.X 0.6 "Point3.normalize x"
        Expect.floatClose Accuracy.veryHigh u.Y 0.8 "Point3.normalize y"

        Expect.throwsT<ArgumentException> (fun () -> Point3.normalize Point3.zero |> ignore) "normalize rejects zero"
        Expect.throwsT<ArgumentException> (fun () -> (a / 0.) |> ignore) "division by zero rejects"
        Expect.throwsT<ArgumentException> (fun () -> (a * nan) |> ignore) "NaN scalar rejects"
        Expect.throwsT<ArgumentException> (fun () -> (Point3.create 1e308 0. 0. + Point3.create 1e308 0. 0.) |> ignore) "overflow addition rejects"

        let v3 = Point3.toVector3 a
        Expect.equal v3 (Vector3(1.0f, -2.0f, 3.0f)) "Point3.toVector3"
        Expect.equal (Point3.ofVector3 v3) a "Point3.ofVector3"
        Expect.throwsT<ArgumentException> (fun () -> Point3.ofVector3 (Vector3(Single.NaN, 0.0f, 0.0f)) |> ignore) "ofVector3 rejects NaN"
        Expect.throwsT<ArgumentException> (fun () -> Point3.toVector3 (Point3.create 1e100 0. 0.) |> ignore) "toVector3 rejects float32 overflow"

        let cs = Bounds3.ofCenterSize (Point3.create 10. 20. 30.) 4. 6. 8.
        Expect.equal (Bounds3.minPoint cs) (Point3.create 8. 17. 26.) "ofCenterSize min"
        Expect.equal (Bounds3.maxPoint cs) (Point3.create 12. 23. 34.) "ofCenterSize max"
        Expect.throwsT<ArgumentException> (fun () -> Bounds3.ofCenterSize Point3.zero -1. 1. 1. |> ignore) "ofCenterSize rejects negative"
        Expect.equal (Bounds3.ofPoints []) None "ofPoints empty returns None"
        let bp = Bounds3.ofPoints (seq { yield a; yield b }) |> Option.get
        Expect.equal (Bounds3.minPoint bp) (Point3.create -4. -2. 3.) "ofPoints min"
        Expect.equal (Bounds3.maxPoint bp) (Point3.create 1. 5. 6.) "ofPoints max"
]

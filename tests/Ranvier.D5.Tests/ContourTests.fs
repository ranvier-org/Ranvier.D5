module Ranvier.D5.Tests.ContourTests

open System
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private grid columns rows values = DensityGrid.create (Bounds.create 0. 0. (float columns) (float rows)) columns rows values |> ok
let private area polygons = polygons |> List.sumBy(fun polygon -> Polygon.area polygon.Outer + (polygon.Holes |> List.sumBy Polygon.area))
let private checkRings polygons =
    for polygon in polygons do
        Expect.equal polygon.Outer.Head (List.last polygon.Outer) "closed shell"
        Expect.isGreaterThan (Polygon.area polygon.Outer) 0. "clockwise screen shell"
        for hole in polygon.Holes do
            Expect.equal hole.Head (List.last hole) "closed hole"
            Expect.isLessThan (Polygon.area hole) 0. "counterclockwise screen hole"
            Expect.isTrue (Polygon.contains hole.Head polygon.Outer) "hole belongs to shell"

let tests=testList "Contour" [
    testCase "constant and missing fields retain boundary and absence semantics" <| fun _ ->
        let full=grid 3 3 (List.replicate 9 (Some 1.))
        let polygons=Contour.compute (ContourConfig.create [1.]) full |> ok
        Expect.equal polygons.Length 1 "one boundary shell at equal threshold"
        checkRings polygons
        Expect.equal (Contour.compute (ContourConfig.create [2.]) full |> ok) [] "no high samples"
        Expect.equal (Contour.compute (ContourConfig.create [0.]) (grid 2 2 [None;None;None;None]) |> ok) [] "missing does not become zero"
        Expect.equal (Contour.compute (ContourConfig.create []) full |> ok) [] "no requested levels"
        for point in polygons.Head.Outer do Expect.isTrue (Bounds.contains point (DensityGrid.bounds full)) "boundary clipped to extent"
    testCase "asymptotic decider follows saddle value and separates exact ties" <| fun _ ->
        let compute values=Contour.compute (ContourConfig.create [0.]) (grid 2 2 (List.map Some values)) |> ok
        Expect.equal (compute [2.;-1.;-1.;2.]).Length 1 "positive diagonal connected above saddle"
        Expect.equal (compute [1.;-2.;-2.;1.]).Length 2 "positive diagonal disconnected below saddle"
        Expect.equal (compute [-1.;2.;2.;-1.]).Length 1 "opposite high diagonal connected"
        Expect.equal (compute [-2.;1.;1.;-2.]).Length 2 "opposite high diagonal disconnected"
        Expect.equal (compute [1.;-1.;-1.;1.]).Length 2 "exact tie separates high components"
        for values in [[1e308;-1e307;-1e307;1e308];[Double.Epsilon;-Double.Epsilon;-Double.Epsilon;Double.Epsilon]] do
            compute values |> checkRings
    testCase "nested holes and islands have explicit containment and orientation" <| fun _ ->
        let values=[for row in 0..6 do for column in 0..6 -> Some(if row=0 || row=6 || column=0 || column=6 || (row=3 && column=3) then 2. else 0.)]
        let polygons=Contour.compute (ContourConfig.create [1.]) (grid 7 7 values) |> ok
        Expect.equal polygons.Length 2 "outer region and central island"
        Expect.equal (polygons |> List.sumBy(fun p -> p.Holes.Length)) 1 "one enclosed hole"
        checkRings polygons
    testCase "all sixteen local cases produce closed nonnegative regions" <| fun _ ->
        for mask in 0..15 do
            let values=[for bit in [0;1;3;2] -> Some(if mask &&& (1<<<bit)<>0 then 1. else -1.)]
            let polygons=Contour.compute (ContourConfig.create [0.]) (grid 2 2 values) |> ok
            checkRings polygons
            Expect.isGreaterThanOrEqual (area polygons) 0. "nonnegative region"
    testCase "threshold ordering and interpolation are explicit" <| fun _ ->
        let field=grid 3 3 [for row in 0..2 do for column in 0..2 -> Some(float(column+row))]
        let output=Contour.compute (ContourConfig.create [3.;1.;2.]) field |> ok
        Expect.equal (output |> List.map(fun p -> p.Threshold) |> List.distinct) [1.;2.;3.] "ascending levels"
        let areas=[for threshold in [1.;2.;3.] -> output |> List.filter(fun p -> p.Threshold=threshold) |> area]
        Expect.isTrue (areas[0]>=areas[1] && areas[1]>=areas[2]) "superlevel areas decrease"
        let linear=Contour.compute (ContourConfig.create [1.25]) field |> ok
        let midpoint=Contour.compute {ContourConfig.create [1.25] with Interpolate=false} field |> ok
        Expect.notEqual linear midpoint "interpolated positions differ"
    testCase "collinear boundary samples collapse to exact corner vertices" <| fun _ ->
        let field=grid 64 64 (List.replicate (64*64) (Some 1.))
        let polygons=Contour.compute (ContourConfig.create [0.]) field |> ok
        Expect.equal polygons.Head.Outer.Length 9 "eight beveled corners plus closure, not one vertex per boundary cell"
        checkRings polygons
    testCase "masked holes and equality plateaus remain simple closed cycles" <| fun _ ->
        let intersects a b c d =
            Predicates.orient2d a b c*Predicates.orient2d a b d<0 &&
            Predicates.orient2d c d a*Predicates.orient2d c d b<0
        for seed in 0..39 do
            let values=[for i in 0..63 -> if (i*7+seed)%13=0 then None else Some(float((i*i+seed*17+i*seed)%5-2))]
            let polygons=Contour.compute (ContourConfig.create [0.;0.5]) (grid 8 8 values) |> ok
            checkRings polygons
            for polygon in polygons do
                for ring in polygon.Outer::polygon.Holes do
                    let unique=List.take (ring.Length-1) ring
                    Expect.equal (Set.count(Set.ofList unique)) unique.Length "no repeated vertices in a simple ring"
                    let segments=List.pairwise ring |> List.toArray
                    for i in 0..segments.Length-1 do
                        for j in i+1..segments.Length-1 do
                            let a,b=segments[i]
                            let c,d=segments[j]
                            Expect.isFalse (intersects a b c d) "nonadjacent edges do not cross"
        let masked=grid 3 3 [for i in 0..8 -> if i=4 then None else Some 1.]
        let polygons=Contour.compute (ContourConfig.create [0.]) masked |> ok
        Expect.equal polygons.Head.Holes.Length 1 "missing center is an explicit hole"
    testCase "polygon area retains exact half-grid and fractional geometry" <| fun _ ->
        Expect.equal (Polygon.area [Point.create 0. 0.;Point.create 0.5 0.;Point.create 0. 1.5]) 0.375 "exact half-coordinate area"
        let offset=1073741824.
        Expect.equal (Polygon.area [Point.create offset offset;Point.create (offset+0.5) offset;Point.create offset (offset+1.5)]) 0.375 "large-offset fallback"
        Expect.floatClose Accuracy.high (Polygon.area [Point.create 0. 0.;Point.create 0.3 0.;Point.create 0. 0.7]) 0.105 "non-half-grid fallback"
    testCase "generated superlevel areas are monotone and source grids remain immutable" <| fun _ ->
        for seed in 0..29 do
            let field=grid 9 9 [for i in 0..80 -> Some(sin(float(i*17+seed*31)))]
            let before=DensityGrid.values field
            let polygons=Contour.compute (ContourConfig.create [-0.8;-0.2;0.2;0.8]) field |> ok
            let areas=[for threshold in [-0.8;-0.2;0.2;0.8] -> polygons |> List.filter(fun p -> p.Threshold=threshold) |> area]
            for lower,upper in List.pairwise areas do Expect.isTrue (lower+1e-10>=upper) "higher levels cannot increase area"
            Expect.equal (DensityGrid.values field) before "contouring cannot mutate sample values or mask"
    testCase "contours reject collapsed world coordinates and support finite extreme values" <| fun _ ->
        let huge=DensityGrid.create (Bounds.create 1e300 1e300 1. 1.) 2 2 [Some 1.;Some 1.;Some 1.;Some 1.] |> ok
        Expect.isError (Contour.compute (ContourConfig.create [0.]) huge) "unrepresentable output geometry is explicit"
        let field=grid 2 2 [Some 1e308;Some -1e308;Some -1e308;Some 1e308]
        Contour.compute (ContourConfig.create [1.]) field |> ok |> checkRings
    testCase "contour computation observes cancellation during a large field" <| fun _ ->
        let field=grid 512 512 [for i in 0..512*512-1 -> Some(float(i%13))]
        use cancel=new Threading.CancellationTokenSource()
        let elapsed=Diagnostics.Stopwatch.StartNew()
        cancel.CancelAfter 1
        Expect.throwsT<OperationCanceledException> (fun () -> Contour.computeWithCancellation cancel.Token (ContourConfig.create [3.;5.;8.]) field |> ignore) "grid strip cancellation"
        Expect.isLessThan elapsed.Elapsed.TotalSeconds 2. "cooperative cancellation bound"
    testCase "invalid thresholds budgets and pre-cancellation are rejected" <| fun _ ->
        let field=grid 2 2 [Some 1.;Some 2.;None;Some 3.]
        for config in [ContourConfig.create [nan];ContourConfig.create [1.;1.];{ContourConfig.create [1.] with MaxVertices=2};{ContourConfig.create [1.] with MaxWork=1L};{ContourConfig.create [1.] with MaxCells=1}] do
            Expect.isError (Contour.compute config field) "invalid request"
        let checker=grid 4 4 [for row in 0..3 do for column in 0..3 -> Some(if (row+column)%2=0 then 1. else -1.)]
        Expect.isError (Contour.compute {ContourConfig.create [0.] with MaxPolygons=2} checker) "fragment count budget"
        use cancel=new Threading.CancellationTokenSource()
        cancel.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> Contour.computeWithCancellation cancel.Token (ContourConfig.create [1.]) field |> ignore) "cancellation"
]

module Ranvier.D5.Tests.PlanarTests
open System
open Expecto
open Ranvier.D5

let private p=Point.create
let tests=testList "Robust planar geometry" [
    testCase "predicates and polygon moments match pinned reference fixtures" <| fun _ ->
        use fixture=System.Text.Json.JsonDocument.Parse(System.IO.File.ReadAllText(System.IO.Path.Combine(AppContext.BaseDirectory,"fixtures","planar.json")))
        let points (row:System.Text.Json.JsonElement)=row.GetProperty("points").EnumerateArray() |> Seq.map(fun point -> p (point[0].GetDouble()) (point[1].GetDouble())) |> Seq.toArray
        for row in fixture.RootElement.GetProperty("predicates").EnumerateArray() do
            let values=points row
            Expect.equal (Predicates.orient2d values[0] values[1] values[2]) (row.GetProperty("orientation").GetInt32()) "orientation sign"
            Expect.equal (Predicates.incircle values[0] values[1] values[2] values[3]) (row.GetProperty("incircle").GetInt32()) "incircle sign"
        for row in fixture.RootElement.GetProperty("polygons").EnumerateArray() do
            let vertices=points row |> Array.toList
            Expect.floatClose Accuracy.high (Polygon.area vertices) (row.GetProperty("area").GetDouble()) "signed area"
            let center=Polygon.centroid vertices |> Option.get
            let expected=row.GetProperty("centroid")
            Expect.floatClose Accuracy.high center.X (expected[0].GetDouble()) "centroid x"
            Expect.floatClose Accuracy.high center.Y (expected[1].GetDouble()) "centroid y"
    testCase "orientation and incircle preserve signs across finite scales" <| fun _ ->
        for scale in [1e-300;1.;1e300] do
            let a,b,c=p 0. 0.,p scale 0.,p 0. scale
            Expect.equal (Predicates.orient2d a b c) 1 "counterclockwise"
            Expect.equal (Predicates.orient2d a c b) -1 "clockwise"
            Expect.equal (Predicates.orient2d a b a) 0 "coincident"
            Expect.equal (Predicates.incircle a b c (p (scale/4.) (scale/4.))) 1 "inside"
            Expect.equal (Predicates.incircle a b c (p scale scale)) 0 "cocircular"
            Expect.equal (Predicates.incircle a b c (p (-scale) (-scale))) -1 "outside"
    testCase "near cancellation and subnormal differences remain topological" <| fun _ ->
        let epsilon=Double.Epsilon
        Expect.equal (Predicates.orient2d (p 0. 0.) (p epsilon 0.) (p 0. epsilon)) 1 "subnormal"
        let large=1e150
        Expect.equal (Predicates.orient2d (p large large) (p (Math.BitIncrement large) large) (p large (Math.BitIncrement large))) 1 "translated ULP triangle"
        Expect.equal (Predicates.orient2d (p -1e308 0.) (p 1e308 0.) (p 0. 1e-308)) 1 "overflowing differences"
    testCase "polygon winding boundary and hull degeneracies are explicit" <| fun _ ->
        let square=[p 0. 0.;p 2. 0.;p 2. 2.;p 0. 2.]
        Expect.equal (Polygon.area square) 4. "CCW signed area"
        Expect.equal (Polygon.area (List.rev square)) -4. "CW signed area"
        Expect.equal (Polygon.centroid square) (Some(p 1. 1.)) "centroid"
        Expect.isTrue (Polygon.contains (p 2. 1.) square) "boundary included"
        Expect.isTrue (Polygon.contains (p 1. 1.) square) "interior"
        Expect.isFalse (Polygon.contains (p 3. 1.) square) "outside"
        Expect.equal (Polygon.hull (square@[p 1. 1.;p 0. 0.])) square "CCW hull from lexicographic minimum"
        Expect.equal (Polygon.hull [p 2. 0.;p 1. 0.;p 0. 0.]) [p 0. 0.;p 2. 0.] "collinear endpoints"
        Expect.equal (Polygon.centroid []) None "empty centroid"
    testCase "polygon centroid supports extreme finite coordinates" <| fun _ ->
        let tiny=[p 0. 0.;p 1e-300 0.;p 0. 1e-300]
        let center=Polygon.centroid tiny |> Option.get
        Expect.floatClose Accuracy.high (center.X/1e-300) (1./3.) "tiny centroid"
        let huge=[p 0. 0.;p 1e300 0.;p 0. 1e300]
        Expect.floatClose Accuracy.high ((Polygon.centroid huge |> Option.get).X/1e300) (1./3.) "huge centroid"
        Expect.throws (fun () -> Polygon.area huge |> ignore) "unrepresentable area rejected"
]

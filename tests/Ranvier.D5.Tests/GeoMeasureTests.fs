module Ranvier.D5.Tests.GeoMeasureTests

open System
open System.Threading
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5.Geo

let private ok = function Ok x -> x | Error e -> failtestf "Unexpected error: %A" e
let private p x y = GeoPosition.create x y |> ok
let private length geometry = GeoMeasure.length GeoStreamConfig.defaultValue geometry |> ok
let private near actual expected tolerance = Expect.isLessThanOrEqual (abs(actual-expected)) tolerance "Angular length"

let private referenceGeometry (value: JsonElement) =
    let position (v: JsonElement) = p (v[0].GetDouble()) (v[1].GetDouble())
    let positions (v: JsonElement) = v.EnumerateArray() |> Seq.map position |> Seq.toList
    let rings (v: JsonElement) = v.EnumerateArray() |> Seq.map positions |> Seq.toList
    let rec read (v: JsonElement) =
        match v.GetProperty("type").GetString() with
        | "Sphere" -> GeoGeometry.Sphere
        | "Point" -> GeoGeometry.Point(Some(position(v.GetProperty("coordinates"))))
        | "MultiPoint" -> GeoGeometry.MultiPoint(positions(v.GetProperty("coordinates")))
        | "LineString" -> GeoGeometry.LineString(positions(v.GetProperty("coordinates")))
        | "Polygon" -> GeoGeometry.Polygon(rings(v.GetProperty("coordinates")))
        | "MultiPolygon" -> GeoGeometry.MultiPolygon(v.GetProperty("coordinates").EnumerateArray() |> Seq.map rings |> Seq.toList)
        | "GeometryCollection" -> GeoGeometry.Collection(v.GetProperty("geometries").EnumerateArray() |> Seq.map read |> Seq.toList)
        | kind -> failtestf "Unsupported reference geometry: %s" kind
    read value

let private centroid geometry = GeoMeasure.centroid GeoStreamConfig.defaultValue geometry |> ok
let private centroidNear geometry expected =
    match centroid geometry with
    | Some actual -> Expect.isLessThanOrEqual (Geodesic.distance actual expected) 1e-10 "Spherical centroid direction"
    | None -> failtest "Expected a defined centroid"

let tests = testList "GeoMeasure" [
    testCase "centroids agree with the pinned point line and polygon references" <| fun _ ->
        use fixture=JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","spherical.json")))
        for item in fixture.RootElement.GetProperty("geometries").EnumerateArray() do
            let geometry=referenceGeometry(item.GetProperty("geometry"))
            let expected=item.GetProperty("centroid")
            if expected[0].ValueKind=JsonValueKind.Null then Expect.isNone (centroid geometry) (item.GetProperty("name").GetString())
            else centroidNear geometry (p (expected[0].GetDouble()) (expected[1].GetDouble()))
    testCase "wide spherical triangle uses full arc angles beyond ninety degrees" <| fun _ ->
        let ring=[p 0. 0.;p 0. 90.;p 120. 0.;p 0. 0.]
        let latitude=Math.Atan2(4.,3.*sqrt 3.)*180./Math.PI
        centroidNear (GeoGeometry.Polygon [ring]) (p 60. latitude)
        centroidNear (GeoGeometry.Polygon [List.rev ring]) (p -120. -latitude)
    testCase "centroid falls back through dimensions without inventing a direction" <| fun _ ->
        let a=p 20. 30.
        centroidNear (GeoGeometry.Polygon [[a;a;a;a]]) a
        centroidNear (GeoGeometry.Collection [GeoGeometry.Point(Some(p -120. -45.));GeoGeometry.LineString [p 0. 0.;p 90. 0.]]) (p 45. 0.)
        Expect.isNone (centroid(GeoGeometry.MultiPoint [p 0. 0.;p 180. 0.])) "Opposite points are ambiguous"
        Expect.isNone (centroid GeoGeometry.Sphere) "Sphere has no preferred direction"
        match centroid(GeoGeometry.LineString [p 0. 0.;p 1e-200 0.]) with
        | Some tiny ->
            near (tiny.Longitude/5e-201) 1. 1e-14
            Expect.equal tiny.Latitude 0. "Equatorial midpoint stays on the equator"
        | None -> failtest "A tiny finite line has a defined midpoint"
    testCase "centroid weight rescaling preserves unequal arcs in either order" <| fun _ ->
        let short=[p 0. 0.;p 60. 0.]
        let long=[p 0. 0.;p 120. 0.]
        let expected=p (Math.Atan2(3.*sqrt 3.,5.)*180./Math.PI) 0.
        for lines in [[short;long];[long;short]] do centroidNear (GeoGeometry.MultiLineString lines) expected
        for lines in [[short;[p 0. 0.;p 1e-200 0.]];[[p 0. 0.;p 1e-200 0.];short]] do
            centroidNear (GeoGeometry.MultiLineString lines) (p 30. 0.)
    testCase "centroid rejects antipodal routes and honors traversal limits" <| fun _ ->
        let line=GeoGeometry.LineString [p 0. 0.;p 180. 0.]
        Expect.isError (GeoMeasure.centroid GeoStreamConfig.defaultValue line) "Antipodal route is ambiguous"
        Expect.isError (GeoMeasure.centroid {GeoStreamConfig.defaultValue with MaxPoints=1} (GeoGeometry.MultiPoint[p 0. 0.;p 1. 0.])) "Point budget propagates"

    testCase "area agrees with pinned spherical fixtures without changing winding" <| fun _ ->
        use fixture=JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","spherical.json")))
        for item in fixture.RootElement.GetProperty("geometries").EnumerateArray() do
            let actual=GeoMeasure.area GeoStreamConfig.defaultValue (referenceGeometry(item.GetProperty("geometry"))) |> ok
            Expect.isLessThanOrEqual (abs(actual-item.GetProperty("area").GetDouble())) 1e-9 (item.GetProperty("name").GetString())
    testCase "octant winding selects the region or its spherical complement" <| fun _ ->
        let ring=[p 0. 0.;p 0. 90.;p 90. 0.;p 0. 0.]
        let area rings=GeoMeasure.area GeoStreamConfig.defaultValue (GeoGeometry.Polygon rings) |> ok
        near (area [ring]) (Math.PI/2.) 1e-14
        near (area [List.rev ring]) (3.5*Math.PI) 1e-14
        near (area [ring;List.rev ring]) 0. 1e-14
        near (GeoMeasure.area GeoStreamConfig.defaultValue GeoGeometry.Sphere |> ok) (4.*Math.PI) 1e-14
    testCase "area is rotation invariant and tolerates alternate pole coordinates" <| fun _ ->
        let ring=[p 0. 0.;p 0. 90.;p 90. 0.;p 0. 0.]
        for yaw,pitch,roll in [30.,45.,10.;180.,-90.,0.;-120.,89.,170.;1e12,-1e12,1e12] do
            let rotation=GeoRotation.create yaw pitch roll |> ok
            for coordinates,expected in [ring,Math.PI/2.;List.rev ring,3.5*Math.PI] do
                let rotated=coordinates |> List.map(GeoRotation.apply rotation)
                near (GeoMeasure.area GeoStreamConfig.defaultValue (GeoGeometry.Polygon [rotated]) |> ok) expected 1e-12
        let poleRing=[p 0. 0.;p 0. 90.;p 90. 90.;p 90. 0.;p 0. 0.]
        near (GeoMeasure.area GeoStreamConfig.defaultValue (GeoGeometry.Polygon [poleRing]) |> ok) (Math.PI/2.) 1e-14
        let repeated=[p 0. 0.;p 0. 0.;p 0. 0.;p 0. 0.]
        near (GeoMeasure.area GeoStreamConfig.defaultValue (GeoGeometry.Polygon [repeated]) |> ok) 0. 0.
    testCase "area rejects ambiguous antipodal edges and propagates budgets" <| fun _ ->
        let ring=[p 0. 0.;p 180. 0.;p 0. 45.;p 0. 0.]
        Expect.isError (GeoMeasure.area GeoStreamConfig.defaultValue (GeoGeometry.Polygon [ring])) "Antipodal edge needs an explicit route"
        let octant=GeoGeometry.Polygon [[p 0. 0.;p 0. 90.;p 90. 0.;p 0. 0.]]
        Expect.isError (GeoMeasure.area {GeoStreamConfig.defaultValue with MaxPoints=3} octant) "Closing input point is counted"

    testCase "length uses shortest great circle arcs across the dateline" <| fun _ ->
        near (length(GeoGeometry.LineString [p 170. 0.;p -170. 0.])) (Math.PI/9.) 1e-14
        near (length(GeoGeometry.LineString [p 0. 0.;p 90. 0.;p 90. 90.])) Math.PI 1e-14
        near (length(GeoGeometry.LineString [p 0. 0.;p 180. 0.])) Math.PI 1e-14
    testCase "polygon length includes closure and hole boundaries exactly once" <| fun _ ->
        let ring=[p 0. 0.;p 0. 90.;p 90. 0.;p 0. 0.]
        near (length(GeoGeometry.Polygon [ring])) (1.5*Math.PI) 1e-14
        near (length(GeoGeometry.Polygon [ring;List.rev ring])) (3.*Math.PI) 1e-14
        near (length(GeoGeometry.LineString ring.Tail)) Math.PI 1e-14
    testCase "collections sum independent lines and ignore isolated points and sphere" <| fun _ ->
        let geometry=GeoGeometry.Collection [GeoGeometry.Point(Some(p 30. 40.));GeoGeometry.MultiLineString [[p 0. 0.;p 90. 0.];[];[p 90. 90.;p 90. 0.]];GeoGeometry.Sphere]
        near (length geometry) Math.PI 1e-14
        near (length(GeoGeometry.MultiPoint[p 0. 0.;p 180. 0.])) 0. 0.
    testCase "degenerate and tiny arcs retain their scale and structural validation" <| fun _ ->
        let a=p 0. 0.
        near (length(GeoGeometry.LineString [a;a])) 0. 0.
        let tiny=length(GeoGeometry.LineString [a;p 1e-200 0.])
        Expect.isGreaterThan tiny 0. "Tiny finite arc survives"
        near (tiny/(Math.PI/180.*1e-200)) 1. 1e-14
        Expect.isError (GeoMeasure.length GeoStreamConfig.defaultValue (GeoGeometry.LineString [a])) "Manual malformed geometry is validated"
        Expect.isError (GeoMeasure.length {GeoStreamConfig.defaultValue with MaxPoints=1} (GeoGeometry.LineString [a;a])) "Traversal budgets propagate"
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> GeoMeasure.lengthWithCancellation GeoStreamConfig.defaultValue cancelled.Token GeoGeometry.Sphere |> ignore) "Cancellation propagates"
]

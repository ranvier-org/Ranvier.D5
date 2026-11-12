module Ranvier.D5.Tests.GeoGeodesicTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "Unexpected geographic error: %A" errors
let private position longitude latitude = GeoPosition.create longitude latitude |> ok
let private jsonPosition (value:JsonElement) = position (value.[0].GetDouble()) (value.[1].GetDouble())
let private fixture() = JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","spherical.json")))

let tests = testList "geodesic interpolation and rotation" [
    testCase "interpolates the reference shortest routes and preserves angular progress" <| fun _ ->
        use precise = JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","spherical-precision.json")))
        for pair in precise.RootElement.GetProperty("pairs").EnumerateArray() do
            let start,finish = jsonPosition(pair.GetProperty "from"),jsonPosition(pair.GetProperty "to")
            let distance = Geodesic.distance start finish
            for sample in pair.GetProperty("samples").EnumerateArray() do
                let t = sample.GetProperty("t").GetDouble()
                let actual = Geodesic.interpolate t start finish |> ok
                let expected = jsonPosition(sample.GetProperty "point")
                Expect.isLessThan (Geodesic.distance actual expected) 1e-10 (pair.GetProperty("name").GetString())
                Expect.isLessThan (abs(Geodesic.distance start actual - t*distance)) 1e-12 "proportional angular progress"
                let reverse = Geodesic.interpolate (1.-t) finish start |> ok
                Expect.isLessThan (Geodesic.distance actual reverse) 1e-10 "route reversal"
        let middle = Geodesic.interpolate 0.5 (position 0. 0.) (position 90. 0.) |> ok
        Expect.isLessThan (abs(middle.Longitude-45.)) 1e-12 "independent equatorial midpoint"
    testCase "antipodes reject ambiguous interiors while endpoints and near antipodes remain defined" <| fun _ ->
        for start,finish in [position 0. 0.,position 180. 0.;position 30. 20.,position -150. -20.;position 42. 90.,position 7. -90.] do
            Expect.isError (Geodesic.interpolate 0.5 start finish) "ambiguous shortest route"
            Expect.equal (Geodesic.interpolate 0. start finish |> ok) (GeoPosition.normalize start) "start needs no route"
            Expect.equal (Geodesic.interpolate 1. start finish |> ok) (GeoPosition.normalize finish) "end needs no route"
        let near = Geodesic.interpolate 0.5 (position 0. 0.) (position 180. 1e-12) |> ok
        Expect.isLessThan (abs near.Longitude) 1e-9 "exact opposite meridians follow the northern route"
        Expect.isGreaterThan near.Latitude 89.99999999 "near antipode remains computable"
    testCase "interpolation validates t and preserves tiny distances and equivalent turns" <| fun _ ->
        for t in [nan; infinity; -infinity; -0.01; 1.01] do
            Expect.isError (Geodesic.interpolate t (position 0. 0.) (position 1. 1.)) "t domain"
        for t in [0.;0.25;0.5;1.] do
            Expect.equal (Geodesic.interpolate t (position 540. 20.) (position -180. 20.) |> ok) (position -180. 20.) "same geographic location"
        let tiny = Geodesic.interpolate 0.5 (position 0. 0.) (position 1e-200 0.) |> ok
        Expect.isLessThan (abs(tiny.Longitude / 5e-201-1.)) 1e-14 "tiny interpolation survives underflow"
    testCase "rotations match references and invert away from longitude singularities" <| fun _ ->
        use data = fixture()
        for item in data.RootElement.GetProperty("rotations").EnumerateArray() do
            let angles = item.GetProperty "angles"
            let rotation = GeoRotation.create (angles.[0].GetDouble()) (angles.[1].GetDouble()) (angles.[2].GetDouble()) |> ok
            let start = jsonPosition(item.GetProperty "point")
            let actual = GeoRotation.apply rotation start
            Expect.isLessThan (Geodesic.distance actual (jsonPosition(item.GetProperty "rotated"))) 1e-12 "D3 spherical rotation convention"
            Expect.isLessThan (Geodesic.distance (GeoRotation.inverse rotation actual) start) 1e-12 "inverse"
        for longitude in [-180.;-73.;0.;120.;180.] do
            for latitude in [-90.;-45.;0.;60.;90.] do
                let rotation = GeoRotation.create 37. -68. 123. |> ok
                let start = position longitude latitude
                Expect.isLessThan (start |> GeoRotation.apply rotation |> GeoRotation.inverse rotation |> Geodesic.distance start) 1e-12 "grid roundtrip including poles"
    testCase "rotation rejects invalid angles and handles complete or huge turns" <| fun _ ->
        for yaw,pitch,roll in [nan,0.,0.; 0.,infinity,0.; 0.,0.,-infinity] do
            Expect.isError (GeoRotation.create yaw pitch roll) "finite angles"
        let identity = GeoRotation.create 360. -720. 1080. |> ok
        Expect.equal (GeoRotation.apply identity (position 540. 20.)) (position -180. 20.) "full turns are identity"
        let huge = GeoRotation.create Double.MaxValue -Double.MaxValue 1e100 |> ok
        let point = position 30. 40.
        Expect.isLessThan (point |> GeoRotation.apply huge |> GeoRotation.inverse huge |> Geodesic.distance point) 1e-12 "bounded trigonometric arguments"
]

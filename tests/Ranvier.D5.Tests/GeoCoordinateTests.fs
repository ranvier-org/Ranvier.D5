module Ranvier.D5.Tests.GeoCoordinateTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private position longitude latitude = GeoPosition.create longitude latitude |> ok

let tests = testList "geographic coordinates" [
    testCase "rejects invalid latitude and nonfinite coordinates" <| fun _ ->
        for longitude,latitude in [nan,0.; infinity,0.; -infinity,0.; 0.,nan; 0.,infinity; 0.,-90.001; 0.,90.001] do
            Expect.isError (GeoPosition.create longitude latitude) "invalid geographic coordinate"
        for latitude in [-90.;0.;90.] do
            Expect.equal (position 0. latitude).Latitude latitude "inclusive poles"
    testCase "longitude normalization is explicit and bounded for large turns" <| fun _ ->
        let original = position 540. 20.
        Expect.equal original.Longitude 540. "construction preserves source longitude"
        Expect.equal (GeoPosition.normalize original).Longitude -180. "canonical dateline"
        for longitude,expected in [180.,-180.; -180.,-180.; 720.,0.; -541.,179.; 541.,-179.; -360.,0.] do
            Expect.equal (GeoPosition.normalize(position longitude 0.)).Longitude expected "full turns"
        for longitude in [Double.MaxValue; -Double.MaxValue; 1e100; -1e100] do
            let normalized = GeoPosition.normalize(position longitude 45.)
            Expect.isTrue (Double.IsFinite normalized.Longitude && normalized.Longitude >= -180. && normalized.Longitude < 180.) "finite canonical longitude"
            Expect.equal (GeoPosition.normalize normalized) normalized "idempotent"
            Expect.equal normalized.Latitude 45. "latitude preserved"
    testCase "angular distances match pinned spherical references" <| fun _ ->
        use json = JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","spherical.json")))
        for pair in json.RootElement.GetProperty("pairs").EnumerateArray() do
            let point name =
                let value = pair.GetProperty(name:string)
                position (value.[0].GetDouble()) (value.[1].GetDouble())
            let start,finish = point "from",point "to"
            let actual = Geodesic.distance start finish
            Expect.isLessThan (abs(actual-pair.GetProperty("distance").GetDouble())) 1e-12 (pair.GetProperty("name").GetString())
            Expect.isLessThan (abs(actual-Geodesic.distance finish start)) 1e-14 "symmetric"
    testCase "distance handles antipodes poles full turns and subnormal scale" <| fun _ ->
        Expect.isLessThan (abs(Geodesic.distance (position 0. 0.) (position 180. 0.) - Math.PI)) 1e-14 "antipodes have a distance"
        Expect.isLessThan (Geodesic.distance (position 720. 90.) (position -85. 90.)) 1e-14 "longitude is irrelevant at pole"
        Expect.equal (Geodesic.distance (position 720. 20.) (position 0. 20.)) 0. "full turns"
        let tiny = Geodesic.distance (position 0. 0.) (position 1e-200 0.)
        Expect.isTrue (tiny > 0. && Double.IsFinite tiny) "tiny distances survive squared underflow"
        Expect.isLessThan (abs(tiny / (Math.PI / 180. * 1e-200) - 1.)) 1e-14 "tiny distance units"
]

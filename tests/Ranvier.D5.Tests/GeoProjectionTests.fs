module Ranvier.D5.Tests.GeoProjectionTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private position longitude latitude = GeoPosition.create longitude latitude |> ok
let private some = function Some value -> value | None -> failtest "Expected a projected point"
let private close actual expected message = Expect.isLessThan (abs(actual-expected)) 1e-10 message

[<Tests>]
let tests = testList "geographic projections" [
    testCase "matches pinned projected coordinates and visibility" <| fun _ ->
        use json=JsonDocument.Parse(File.ReadAllText(System.IO.Path.Combine(AppContext.BaseDirectory,"fixtures","projections.json")))
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            let pair name =
                let value=item.GetProperty(name:string)
                value[0].GetDouble(),value[1].GetDouble()
            let kind =
                match item.GetProperty("kind").GetString() with
                | "Equirectangular" -> ProjectionKind.Equirectangular
                | "Mercator" -> ProjectionKind.Mercator
                | "Orthographic" -> ProjectionKind.Orthographic
                | value -> failtestf "Unknown projection %s" value
            let tx,ty=pair "translate"
            let cx,cy=pair "center"
            let angles=item.GetProperty("rotate")
            let rotation=GeoRotation.create (angles[0].GetDouble()) (angles[1].GetDouble()) (angles[2].GetDouble()) |> ok
            let projection=Projection.create {Kind=kind;Scale=item.GetProperty("scale").GetDouble();Translate=Point.create tx ty;Center=position cx cy;Rotation=rotation} |> ok
            let longitude,latitude=pair "point"
            match Projection.project projection (position longitude latitude) with
            | None -> Expect.isFalse (item.GetProperty("visible").GetBoolean()) "only hidden points are omitted"
            | Some actual ->
                Expect.isTrue (item.GetProperty("visible").GetBoolean()) "visible hemisphere"
                let x,y=pair "projected"
                Expect.isLessThan (Double.Hypot(actual.X-x,actual.Y-y)) 1e-9 "pinned D3 screen coordinates"
    testCase "validates finite positive scale and translation" <| fun _ ->
        let config=ProjectionConfig.defaultValue ProjectionKind.Equirectangular
        for scale in [0.; -1.; nan; infinity; -infinity] do
            Expect.isError (Projection.create {config with Scale=scale}) "invalid scale"
        for x,y in [nan,0.;0.,infinity;-infinity,0.] do
            Expect.isError (Projection.create {config with Translate={X=x;Y=y}}) "invalid translation"
    testCase "equirectangular uses radians and screen y points down" <| fun _ ->
        let projection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=100.;Translate=Point.create 200. 150.} |> ok
        let actual=Projection.project projection (position 90. 45.) |> some
        close actual.X (200.+50.*Math.PI) "longitude radians"
        close actual.Y (150.-25.*Math.PI) "latitude radians, screen orientation"
        Expect.equal (Projection.project projection (position 450. 45.)) (Some actual) "full turns"
    testCase "mercator has explicit polar singularities and stable small latitudes" <| fun _ ->
        let projection=Projection.create (ProjectionConfig.defaultValue ProjectionKind.Mercator) |> ok
        for latitude in [-90.;90.] do
            Expect.isNone (Projection.project projection (position 0. latitude)) "pole is undefined"
        let equator=Projection.project projection (position 0. 0.) |> some
        Expect.equal equator.Y 0. "exact equator"
        let tiny=Projection.project projection (position 0. 1e-200) |> some
        Expect.isTrue (tiny.Y < 0.) "tiny latitude survives cancellation"
        close ((Projection.project projection (position 0. 45.) |> some).Y) (-log(1.+sqrt 2.)) "Mercator latitude"
    testCase "orthographic clips the hidden hemisphere and keeps its exact limb" <| fun _ ->
        let projection=Projection.create (ProjectionConfig.defaultValue ProjectionKind.Orthographic) |> ok
        for longitude,latitude in [180.,0.;91.,0.;-91.,0.;120.,45.] do
            Expect.isNone (Projection.project projection (position longitude latitude)) "hidden hemisphere"
        let limb=Projection.project projection (position 90. 0.) |> some
        close limb.X 1. "right limb"
        close limb.Y 0. "equator"
        let pole=Projection.project projection (position 170. 90.) |> some
        close pole.X 0. "pole longitude irrelevant"
        close pole.Y -1. "north limb"
        Expect.isNone (Projection.tryInvert projection (Point.create 1.00001 0.)) "outside disk"
    testCase "inverse roundtrips visible positions with center rotation and translation" <| fun _ ->
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.Mercator;ProjectionKind.Orthographic] do
            let rotation=GeoRotation.create 20. -15. 10. |> ok
            let projection=Projection.create {ProjectionConfig.defaultValue kind with Scale=135.;Translate=Point.create 360. 240.;Center=position 10. 5.;Rotation=rotation} |> ok
            for longitude,latitude in [-30.,-45.;0.,0.;30.,55.;45.,-20.] do
                let original=position longitude latitude
                let screen=Projection.project projection original |> some
                let restored=Projection.tryInvert projection screen |> some
                Expect.isLessThan (Geodesic.distance original restored) 1e-12 "inverse includes all transforms"
    testCase "inverse rejects nonfinite and out of domain coordinates" <| fun _ ->
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.Mercator;ProjectionKind.Orthographic] do
            let projection=Projection.create (ProjectionConfig.defaultValue kind) |> ok
            for x,y in [nan,0.;0.,infinity;-infinity,0.] do
                Expect.isNone (Projection.tryInvert projection ({X=x;Y=y})) "nonfinite screen point"
        let rectangular=Projection.create (ProjectionConfig.defaultValue ProjectionKind.Equirectangular) |> ok
        Expect.isNone (Projection.tryInvert rectangular (Point.create 0. 2.)) "beyond latitude domain"
    testCase "orthographic limb survives floating point roundtrip at every latitude" <| fun _ ->
        for scale,tx,ty in [1.,0.,0.;135.,360.,240.] do
            let projection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with Scale=scale;Translate=Point.create tx ty} |> ok
            for latitude in -90 .. 90 do
                for longitude in [-90.;90.] do
                    let screen=Projection.project projection (position longitude (float latitude)) |> some
                    let restored=Projection.tryInvert projection screen |> some
                    let again=Projection.project projection restored |> some
                    Expect.isLessThan (Double.Hypot(again.X-screen.X,again.Y-screen.Y)) 1e-10 "limb forward/inverse stability"
    testCase "rejects singular projection centers and never publishes overflow" <| fun _ ->
        let mercator=ProjectionConfig.defaultValue ProjectionKind.Mercator
        Expect.isError (Projection.create {mercator with Center=position 0. 90.}) "singular center"
        let huge=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=Double.MaxValue} |> ok
        Expect.isNone (Projection.project huge (position 170. 0.)) "overflow is not a screen point"
]

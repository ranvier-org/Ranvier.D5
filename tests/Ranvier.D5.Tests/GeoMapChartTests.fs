module Ranvier.D5.Tests.GeoMapChartTests

open Expecto
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private p longitude latitude = GeoPosition.create longitude latitude |> ok
let private box west south east north = [p west south;p west north;p east north;p east south;p west south]
let private frame=ChartFrame.create 400. 300. "Map"
let private projection kind scale=Projection.create {ProjectionConfig.defaultValue kind with Scale=scale;Translate=Point.create 210. 145.} |> ok
let private map kind=MapFrame.create frame (MapProjection.Single(projection kind 50.))
let private features=[
    {Id="a";Name="Alpha";Geometry=GeoGeometry.Polygon[box -40. -20. -10. 20.;List.rev(box -30. -10. -20. 10.)]}
    {Id="b";Name="Beta";Geometry=GeoGeometry.Polygon[box 10. -20. 40. 20.]}
    {Id="c";Name="Gamma";Geometry=GeoGeometry.MultiPolygon[[box -60. 30. -50. 40.];[box 50. 30. 60. 40.]]}]
type private Row = { Id:string; Value:float option }
let private spec map={Map=map;Key=(fun (row:Row) -> row.Id);Value=Encoding.create "Value" (fun row -> row.Value);Domain=Auto
                      Color=(fun t -> Paint.rgb (int(255.*t)) 0 0);Missing=Paint.rgb 200 200 200;Stroke=Paint.rgb 255 255 255;Selected=Set.empty}
let private at (projected:Projection) longitude latitude=Projection.project projected (p longitude latitude) |> Option.get

[<Tests>]
let tests = testList "map charts" [
    testCase "choropleth joins values by feature ID with explicit missing data" <| fun _ ->
        let output=MapChart.choropleth (spec(map ProjectionKind.Equirectangular)) features [{Id="a";Value=Some 1.};{Id="b";Value=Some 3.};{Id="c";Value=None}] |> ok
        let labels=output.Semantics |> List.map(fun semantic -> semantic.DataIds.Head,semantic.Description) |> Map.ofList
        Expect.equal labels["a"] "Alpha: 1" "joined value"
        Expect.equal labels["c"] "Gamma: no data" "missing value is explicit"
        Expect.equal output.HitTargets.Length 3 "one target per visible feature, including multipolygons"
    testCase "choropleth rejects unmatched data and duplicate IDs instead of discarding them" <| fun _ ->
        let chart=MapChart.choropleth (spec(map ProjectionKind.Equirectangular))
        Expect.isError (chart features [{Id="missing";Value=Some 1.}]) "data without a feature"
        Expect.isError (chart features [{Id="a";Value=Some 1.};{Id="a";Value=Some 2.}]) "duplicate data key"
        Expect.isError (chart (features@[features.Head]) []) "duplicate feature ID"
    testCase "region picking follows projected polygons and leaves holes empty" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular 50.
        let output=MapChart.choropleth (spec(map ProjectionKind.Equirectangular)) features [] |> ok
        let pick longitude latitude=HitTest.at (at projected longitude latitude) output |> List.collect(fun hit -> hit.DataIds)
        Expect.equal (pick -35. 15.) ["a"] "inside Alpha"
        Expect.isEmpty (pick -25. 0.) "inside Alpha's hole"
        Expect.equal (pick 55. 35.) ["c"] "second member of a multipolygon"
        Expect.isEmpty (pick 0. 0.) "between features"
    testCase "region anchors lie inside concave fills, away from holes" <| fun _ ->
        let crescent={Id="crescent";Name="Crescent";Geometry=GeoGeometry.Polygon[[p -40. -30.;p -40. 30.;p 40. 30.;p 40. 20.;p -30. 20.;p -30. -20.;p 40. -20.;p 40. -30.;p -40. -30.]]}
        for feature in crescent::features do
            let output=MapChart.choropleth (spec(map ProjectionKind.Equirectangular)) [feature] [] |> ok
            match output.HitTargets.Head.Geometry with
            | HitRegion(_,_,anchor) -> Expect.equal (HitTest.at anchor output |> List.collect(fun hit -> hit.DataIds)) [feature.Id] (feature.Id+" anchor picks its own fill")
            | other -> failtestf "Unexpected geometry %A" other
    testCase "semantic IDs stay stable when data is reordered" <| fun _ ->
        let chart rows=MapChart.choropleth (spec(map ProjectionKind.Equirectangular)) features rows |> ok
        let rows=[{Id="a";Value=Some 1.};{Id="b";Value=Some 3.}]
        Expect.equal ((chart rows).Semantics) ((chart (List.rev rows)).Semantics) "reorder keeps identity"
    testCase "hidden features are omitted on a globe" <| fun _ ->
        let globe={map ProjectionKind.Orthographic with Projection=MapProjection.Single(Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with Scale=100.;Translate=Point.create 210. 145.;Rotation=GeoRotation.create 160. 0. 0. |> ok} |> ok)}
        let output=MapChart.choropleth (spec globe) features [] |> ok
        Expect.isFalse (output.Semantics |> List.exists(fun semantic -> semantic.DataIds=["a"])) "far side feature"
    testCase "composite projections draw features in every inset" <| fun _ ->
        let usa=CompositeProjection.albersUsa 500. (Point.create 210. 145.) |> ok
        let states=[{Id="CO";Name="Colorado";Geometry=GeoGeometry.Polygon[box -109. 37. -102. 41.]}
                    {Id="AK";Name="Alaska";Geometry=GeoGeometry.Polygon[box -160. 60. -146. 66.]}]
        let output=MapChart.choropleth (spec(MapFrame.create frame (MapProjection.Composite usa))) states [{Id="CO";Value=Some 2.};{Id="AK";Value=Some 4.}] |> ok
        Expect.equal (output.HitTargets |> List.collect(fun hit -> hit.DataIds) |> List.sort) ["AK";"CO"] "both insets"
        let alaska=CompositeProjection.project usa (p -153. 63.) |> Option.get
        Expect.equal (HitTest.at alaska output |> List.collect(fun hit -> hit.DataIds)) ["AK"] "Alaska inset pick"
    testCase "bubbles scale area, keep small bubbles on top and omit hidden positions" <| fun _ ->
        let cities=["big",p 0. 0.,100.;"small",p 1. 0.,4.;"hidden",p 180. 0.,50.]
        let globe=MapFrame.create frame (MapProjection.Single(Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with Scale=100.;Translate=Point.create 210. 145.} |> ok))
        let spec={Map=globe;Key=(fun (id,_,_) -> id);Position=(fun (_,position,_) -> position);Size=Encoding.create "Population" (fun (_,_,size) -> size)
                  MaxRadius=20.;Color=(fun _ -> Paint.rgb 0 0 200);Selected=Set.empty}
        let output=MapChart.bubbles spec cities |> ok
        Expect.equal (output.HitTargets |> List.map(fun hit -> hit.DataIds.Head)) ["big";"small"] "largest first, hidden omitted"
        match output.HitTargets |> List.map(fun hit -> hit.Geometry) with
        | [HitPoint(_,big);HitPoint(_,small)] ->
            Expect.floatClose Accuracy.high big 20. "largest radius"
            Expect.floatClose Accuracy.high small 4. "area-proportional radius"
        | other -> failtestf "Unexpected targets %A" other
        Expect.isError (MapChart.bubbles {spec with MaxRadius=0.} cities) "positive radius"
        Expect.isError (MapChart.bubbles spec ["bad",p 0. 0.,nan]) "finite sizes"
    testCase "routes follow great circles across the seam and pick along their path" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular 50.
        let spec={Map=map ProjectionKind.Equirectangular;Key=fst;Origin=(fun _ -> p 160. 10.);Destination=(fun _ -> p -160. 10.)
                  Width=2.;Color=(fun _ -> Paint.rgb 200 0 0);Selected=Set.empty}
        let output=MapChart.routes spec ["seam",()] |> ok
        let commands=output.Scene |> Scene.nodes |> List.collect(function Group(_,children) -> children | node -> [node]) |> List.choose(function Leaf mark when (Mark.key mark).Contains "route" -> Some mark | _ -> None)
        Expect.equal commands.Length 1 "one keyed mark"
        let moves=match Mark.geometry commands.Head with PathGeometry path -> Path.commands path |> List.filter(function MoveTo _ -> true | _ -> false) |> List.length | _ -> 0
        Expect.equal moves 2 "split at the antimeridian"
        Expect.equal (HitTest.at (at projected 170. 10.3) output |> List.collect(fun hit -> hit.DataIds)) ["seam"] "pick near the route"
        Expect.isEmpty (HitTest.at (at projected 0. 10.) output) "no straight chord across the map"
    testCase "overlay scopes layers so later ones pick on top" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular 50.
        let regions=MapChart.choropleth (spec(map ProjectionKind.Equirectangular)) features [] |> ok
        let spec={Map=map ProjectionKind.Equirectangular;Key=(fun (id:string,_) -> id);Position=snd;Size=Encoding.create "Size" (fun _ -> 1.)
                  MaxRadius=6.;Color=(fun _ -> Paint.rgb 0 0 0);Selected=Set.empty}
        let points=MapChart.bubbles spec ["a",p -35. 15.] |> ok
        let output=MapChart.overlay ["regions",regions;"points",points] |> ok
        let hits=HitTest.at (at projected -35. 15.) output
        Expect.equal (hits |> List.map(fun hit -> hit.MarkPath.Head)) ["regions";"points"] "drawing order"
        Expect.equal (List.last hits).DataIds ["a"] "top layer is last"
        Expect.isError (MapChart.overlay ["same",regions;"same",points]) "layer scopes are unique"
        Expect.isError (MapChart.overlay []) "empty overlay"
    testCase "GeoJSON features need unique stable IDs and geometry" <| fun _ ->
        let feature id geometry={Id=id;Geometry=geometry;Properties=None;ForeignMembers=Map.empty}
        let polygon=Some(GeoGeometry.Polygon[box 0. 0. 1. 1.])
        let document features=GeoDocument.FeatureCollection(features,Map.empty)
        let converted=MapFeature.ofDocument (fun _ -> None) (document [feature (Some(GeoId.Text "x")) polygon;feature (Some(GeoId.Number "7")) polygon]) |> ok
        Expect.equal (converted |> List.map(fun item -> item.Id)) ["x";"7"] "text and numeric IDs"
        Expect.isError (MapFeature.ofDocument (fun _ -> None) (document [feature None polygon])) "missing ID"
        Expect.isError (MapFeature.ofDocument (fun _ -> None) (document [feature (Some(GeoId.Text "x")) None])) "missing geometry"
        Expect.isError (MapFeature.ofDocument (fun _ -> None) (document [feature (Some(GeoId.Text "x")) polygon;feature (Some(GeoId.Text "x")) polygon])) "duplicate ID"
]

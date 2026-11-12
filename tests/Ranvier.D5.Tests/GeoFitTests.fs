module Ranvier.D5.Tests.GeoFitTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private p longitude latitude = GeoPosition.create longitude latitude |> ok
let private fixture name = JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures",name)))
let private positions (value:JsonElement) = value.EnumerateArray() |> Seq.map(fun point -> p (point[0].GetDouble()) (point[1].GetDouble())) |> Seq.toList
let private geometry (value:JsonElement) =
    match value.GetProperty("type").GetString() with
    | "Sphere" -> GeoGeometry.Sphere
    | "Polygon" -> GeoGeometry.Polygon(value.GetProperty("coordinates").EnumerateArray() |> Seq.map positions |> Seq.toList)
    | "LineString" -> GeoGeometry.LineString(positions(value.GetProperty("coordinates")))
    | "MultiPoint" -> GeoGeometry.MultiPoint(positions(value.GetProperty("coordinates")))
    | other -> failtestf "Unexpected geometry %s" other
let private kind (item:JsonElement) =
    match item.GetProperty("kind").GetString() with
    | "Equirectangular" -> ProjectionKind.Equirectangular
    | "Mercator" -> ProjectionKind.Mercator
    | "Orthographic" -> ProjectionKind.Orthographic
    | "AzimuthalEqualArea" -> ProjectionKind.AzimuthalEqualArea
    | "ConicEqualArea" -> let value=item.GetProperty("parallels") in ProjectionKind.ConicEqualArea(value[0].GetDouble(),value[1].GetDouble())
    | "TransverseMercator" -> ProjectionKind.TransverseMercator
    | other -> failtestf "Unknown projection %s" other
let private extent paths =
    // Complete circles are two half-arcs; their extremes lie one radius from each chord midpoint.
    let points=paths |> List.collect(fun path ->
        let mutable current=Point.create 0. 0.
        Path.commands path |> List.collect(function
            | MoveTo point | LineTo point -> current<-point; [point]
            | ArcTo(radius,_,_,_,_,point) ->
                let middle=Point.create ((current.X+point.X)/2.) ((current.Y+point.Y)/2.)
                current<-point
                [point;Point.create middle.X (middle.Y-radius);Point.create middle.X (middle.Y+radius)]
            | _ -> []))
    List.minBy(fun (point:Point) -> point.X) points,List.maxBy(fun (point:Point) -> point.X) points,
    List.minBy(fun (point:Point) -> point.Y) points,List.maxBy(fun (point:Point) -> point.Y) points

[<Tests>]
let tests = testList "projection fitting and composites" [
    testCase "fitted scale and translation match pinned D3 fits" <| fun _ ->
        use json=fixture "projection-fit.json"
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            let angles=item.GetProperty("rotate")
            let config={ProjectionConfig.defaultValue (kind item) with Rotation=GeoRotation.create (angles[0].GetDouble()) (angles[1].GetDouble()) (angles[2].GetDouble()) |> ok}
            let options={GeoPathOptions.defaultValue with Precision=item.GetProperty("precision").GetDouble()}
            let shape=geometry(item.GetProperty("geometry"))
            let fitted =
                match item.GetProperty("mode").GetString() with
                | "extent" ->
                    let value=item.GetProperty("extent")
                    let lower,upper=value[0],value[1]
                    let x0,y0=lower[0].GetDouble(),lower[1].GetDouble()
                    ProjectionFit.extent options (Bounds.create x0 y0 (upper[0].GetDouble()-x0) (upper[1].GetDouble()-y0)) shape config
                | "size" -> let value=item.GetProperty("size") in ProjectionFit.size options (value[0].GetDouble()) (value[1].GetDouble()) shape config
                | "width" -> ProjectionFit.width options (item.GetProperty("width").GetDouble()) shape config
                | _ -> ProjectionFit.height options (item.GetProperty("height").GetDouble()) shape config
                |> ok
            let label=sprintf "%A %s %s" config.Kind (item.GetProperty("name").GetString()) (item.GetProperty("mode").GetString())
            let expectedScale=item.GetProperty("scale").GetDouble()
            let actual=GeoPath.project {options with PointRadius=0.} fitted shape |> ok
            let translate=item.GetProperty("translate")
            // Both libraries measure bounds of independently resampled streams within the combined precision.
            Expect.isLessThan (abs((Projection.config fitted).Scale/expectedScale-1.)) 5e-3 (label+" scale")
            match item.GetProperty("name").GetString() with
            | "points" -> ()
            | _ ->
                let left,right,top,bottom=extent actual
                let bounds=item.GetProperty("bounds")
                let tolerance=1.+2.*expectedScale/150.*options.Precision
                for actualValue,expected in [left.X,bounds[0][0];top.Y,bounds[0][1];right.X,bounds[1][0];bottom.Y,bounds[1][1]] do
                    Expect.isLessThan (abs(actualValue-expected.GetDouble())) tolerance (label+" fitted bounds")
            let fittedTranslate=(Projection.config fitted).Translate
            Expect.isLessThan (Double.Hypot(fittedTranslate.X-translate[0].GetDouble(),fittedTranslate.Y-translate[1].GetDouble())) (2.+expectedScale/150.*options.Precision*4.) (label+" translation")
    testCase "fitting rejects empty extents and invisible geometry" <| fun _ ->
        let config=ProjectionConfig.defaultValue ProjectionKind.Orthographic
        let shape=GeoGeometry.Polygon[[p -10. -10.;p -10. 10.;p 10. 10.;p 10. -10.;p -10. -10.]]
        Expect.isError (ProjectionFit.size GeoPathOptions.defaultValue 0. 100. shape config) "zero width"
        Expect.isError (ProjectionFit.width GeoPathOptions.defaultValue nan shape config) "nonfinite width"
        let hidden={config with Rotation=GeoRotation.create 180. 0. 0. |> ok}
        Expect.isError (ProjectionFit.size GeoPathOptions.defaultValue 100. 100. shape hidden) "nothing visible"
        Expect.isError (ProjectionFit.size GeoPathOptions.defaultValue 100. 100. (GeoGeometry.Point(Some(p 0. 0.))) config) "a lone point has no extent"
    testCase "fitted geometry fills its extent and stays centred" <| fun _ ->
        let shape=GeoGeometry.Polygon[[p -30. -20.;p -30. 25.;p 40. 25.;p 40. -20.;p -30. -20.]]
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.ConicConformal(20.,50.);ProjectionKind.Stereographic] do
            let fitted=ProjectionFit.extent GeoPathOptions.defaultValue (Bounds.create 10. 20. 300. 200.) shape (ProjectionConfig.defaultValue kind) |> ok
            let left,right,top,bottom=GeoPath.project GeoPathOptions.defaultValue fitted shape |> ok |> extent
            let width,height=right.X-left.X,bottom.Y-top.Y
            Expect.isTrue (abs(width-300.)<1.5 || abs(height-200.)<1.5) (sprintf "%A fills one dimension" kind)
            Expect.isLessThan (abs((left.X+right.X)/2.-160.)) 1.5 (sprintf "%A horizontal centre" kind)
            Expect.isLessThan (abs((top.Y+bottom.Y)/2.-120.)) 1.5 (sprintf "%A vertical centre" kind)
    testCase "Albers USA points and inverses match pinned D3 insets" <| fun _ ->
        use json=fixture "albers-usa.json"
        let usa=CompositeProjection.albersUsa 1070. (Point.create 480. 250.) |> ok
        for item in json.RootElement.GetProperty("points").EnumerateArray() do
            let source=item.GetProperty("point")
            let position=p (source[0].GetDouble()) (source[1].GetDouble())
            let expected=item.GetProperty("projected")
            match CompositeProjection.project usa position,expected.ValueKind with
            | None,JsonValueKind.Null -> ()
            | Some actual,JsonValueKind.Array ->
                Expect.isLessThan (Double.Hypot(actual.X-expected[0].GetDouble(),actual.Y-expected[1].GetDouble())) 1e-8 (sprintf "%A projected" position)
                let inverse=CompositeProjection.tryInvert usa actual |> Option.defaultWith(fun () -> failtestf "%A inverse" position)
                let reference=item.GetProperty("inverse")
                Expect.isLessThan (abs(inverse.Longitude-reference[0].GetDouble())) 1e-7 "inverse longitude"
                Expect.isLessThan (abs(inverse.Latitude-reference[1].GetDouble())) 1e-7 "inverse latitude"
            | actual,_ -> failtestf "%A: expected %A, got %A" position expected actual
    testCase "Albers USA shapes keep D3 topology and area across insets" <| fun _ ->
        use json=fixture "albers-usa.json"
        let usa=CompositeProjection.albersUsa 1070. (Point.create 480. 250.) |> ok
        let precision=json.RootElement.GetProperty("precision").GetDouble()
        for item in json.RootElement.GetProperty("shapes").EnumerateArray() do
            let name=item.GetProperty("name").GetString()
            let paths=CompositeProjection.paths {GeoPathOptions.defaultValue with Precision=precision} usa (geometry(item.GetProperty("geometry"))) |> ok
            let rings=paths |> List.collect(fun path ->
                let rings=ResizeArray<Point list>()
                let mutable current=[]
                for command in Path.commands path do
                    match command with
                    | MoveTo point -> current<-[point]
                    | LineTo point -> current<-point::current
                    | Close -> rings.Add(List.rev current);current<-[]
                    | _ -> ()
                List.ofSeq rings)
            let expected=item.GetProperty("output").EnumerateArray() |> Seq.map(fun ring -> ring.EnumerateArray() |> Seq.map(fun point -> {X=point[0].GetDouble();Y=point[1].GetDouble()}) |> Seq.toList) |> Seq.toList
            Expect.equal rings.Length expected.Length (name+" ring count")
            let perimeter=expected |> List.sumBy(fun ring -> (ring@[ring.Head]) |> List.pairwise |> List.sumBy(fun (a,b) -> sqrt(Point.distanceSquared a b)))
            Expect.isLessThan (abs((rings |> List.sumBy Polygon.area)-(expected |> List.sumBy Polygon.area))) (perimeter*2.*precision+1e-6) (name+" area")
    testCase "Albers USA validates its scale and omits points outside every inset" <| fun _ ->
        Expect.isError (CompositeProjection.albersUsa 0. (Point.create 0. 0.)) "positive scale"
        let usa=CompositeProjection.albersUsa 1070. (Point.create 480. 250.) |> ok
        Expect.equal (CompositeProjection.parts usa).Length 3 "lower 48, Alaska and Hawaii"
        Expect.isNone (CompositeProjection.project usa (p 2.35 48.86)) "Paris lies outside every inset"
]

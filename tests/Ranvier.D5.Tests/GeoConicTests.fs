module Ranvier.D5.Tests.GeoConicTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private p longitude latitude = GeoPosition.create longitude latitude |> ok
let private fixture name = JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures",name)))
let private kindOf (item:JsonElement) =
    let parallels () =
        let value=item.GetProperty("parallels")
        value[0].GetDouble(),value[1].GetDouble()
    match item.GetProperty("kind").GetString() with
    | "ConicConformal" -> ProjectionKind.ConicConformal(parallels())
    | "ConicEqualArea" -> ProjectionKind.ConicEqualArea(parallels())
    | "ConicEquidistant" -> ProjectionKind.ConicEquidistant(parallels())
    | "TransverseMercator" -> ProjectionKind.TransverseMercator
    | value -> failtestf "Unknown projection %s" value
let private vertices path = Path.commands path |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
let private rings paths =
    paths |> List.collect(fun path ->
        let rings=ResizeArray<Point list>()
        let mutable current=[]
        for command in Path.commands path do
            match command with
            | MoveTo point -> current<-[point]
            | LineTo point -> current<-point::current
            | Close -> rings.Add(List.rev current);current<-[]
            | _ -> failtest "sampled polygon commands"
        List.ofSeq rings)
let private perimeter rings =
    rings |> List.sumBy(fun ring -> match ring with [] -> 0. | first::_ -> (ring@[first]) |> List.pairwise |> List.sumBy(fun (a,b) -> sqrt(Point.distanceSquared a b)))
let private configured (item:JsonElement) =
    let rotation =
        match item.TryGetProperty("rotate") with
        | true,angles -> GeoRotation.create (angles[0].GetDouble()) (angles[1].GetDouble()) (angles[2].GetDouble()) |> ok
        | _ -> GeoRotation.create 0. 0. 0. |> ok
    Projection.create {ProjectionConfig.defaultValue (kindOf item) with Scale=item.GetProperty("scale").GetDouble();Rotation=rotation} |> ok

[<Tests>]
let tests = testList "conic and transverse Mercator projections" [
    testCase "match pinned D3 coordinates and inverses" <| fun _ ->
        use json=fixture "conic-projections.json"
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            let pair name = let value=item.GetProperty(name:string) in value[0].GetDouble(),value[1].GetDouble()
            let tx,ty=pair "translate"
            let cx,cy=pair "center"
            let angles=item.GetProperty("rotate")
            let config={ProjectionConfig.defaultValue (kindOf item) with
                            Scale=item.GetProperty("scale").GetDouble();Translate=Point.create tx ty;Center=p cx cy
                            Rotation=GeoRotation.create (angles[0].GetDouble()) (angles[1].GetDouble()) (angles[2].GetDouble()) |> ok}
            let projection=Projection.create config |> ok
            let longitude,latitude=pair "point"
            let label=sprintf "%A %A" config.Kind (longitude,latitude)
            match Projection.project projection (p longitude latitude) with
            | None -> Expect.isFalse (item.GetProperty("defined").GetBoolean()) (label+" omitted only on a rotated pole")
            | Some actual ->
                Expect.isTrue (item.GetProperty("defined").GetBoolean()) (label+" defined")
                let x,y=pair "projected"
                Expect.isLessThan (Double.Hypot(actual.X-x,actual.Y-y)) (1e-9*max 1. (Double.Hypot(x,y))) (label+" pinned screen coordinates")
                let inverse=Projection.tryInvert projection actual |> Option.defaultWith(fun () -> failtestf "%s inverse" label)
                let ix,iy=pair "inverse"
                Expect.isLessThan (abs((inverse.Longitude-ix+540.)%360.-180.)) 1e-7 (label+" inverse longitude")
                Expect.isLessThan (abs(inverse.Latitude-iy)) 1e-7 (label+" inverse latitude")
    testCase "parallels are validated and cylindrical degeneracies rejected" <| fun _ ->
        let create kind=Projection.create (ProjectionConfig.defaultValue kind)
        for kind in [ProjectionKind.ConicConformal(nan,30.);ProjectionKind.ConicEqualArea(0.,91.);ProjectionKind.ConicEquidistant(-infinity,0.)] do
            Expect.isError (create kind) "finite parallels within [-90,90]"
        Expect.isError (create(ProjectionKind.ConicConformal(-30.,30.))) "symmetric conformal parallels form a cylinder"
        Expect.isError (create(ProjectionKind.ConicConformal(90.,90.))) "conformal parallel at a pole"
        Expect.isError (create(ProjectionKind.ConicEqualArea(-20.,20.))) "symmetric equal-area parallels form a cylinder"
        Expect.isError (create(ProjectionKind.ConicEquidistant(0.,0.))) "equatorial equidistant parallel forms a cylinder"
        Expect.isError (Projection.create {ProjectionConfig.defaultValue (ProjectionKind.ConicEqualArea(29.5,45.5)) with ClipAngle=Some 60.}) "conics cut at the antimeridian"
        Expect.isOk (create(ProjectionKind.ConicEqualArea(29.5,45.5))) "Albers parallels"
        Expect.isOk (create ProjectionKind.TransverseMercator) "transverse Mercator"
    testCase "polygon topology and filled area match pinned D3 conic streams" <| fun _ ->
        use json=fixture "conic-streams.json"
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            match item.TryGetProperty("rings") with
            | false,_ -> ()
            | true,ringsJson ->
                let name=sprintf "%A %s" (kindOf item) (item.GetProperty("name").GetString())
                let input=ringsJson.EnumerateArray() |> Seq.map(fun ring -> ring.EnumerateArray() |> Seq.map(fun point -> p (point[0].GetDouble()) (point[1].GetDouble())) |> Seq.toList) |> Seq.toList
                let precision=item.GetProperty("precision").GetDouble()
                let actual=GeoPath.project {GeoPathOptions.defaultValue with Precision=precision} (configured item) (GeoGeometry.Polygon input) |> ok |> rings
                let expected=item.GetProperty("output").EnumerateArray() |> Seq.map(fun ring -> ring.EnumerateArray() |> Seq.map(fun point -> {X=point[0].GetDouble();Y=point[1].GetDouble()}) |> Seq.toList) |> Seq.toList
                Expect.equal actual.Length expected.Length (name+" ring topology")
                let combined=2.*precision
                let expectedArea=expected |> List.sumBy Polygon.area
                let tolerance=max (1e-6*abs expectedArea) (perimeter expected*combined+Math.PI*combined*combined)
                Expect.isLessThan (abs((actual |> List.sumBy Polygon.area)-expectedArea)) tolerance (name+" oriented filled area")
    testCase "line fragments match pinned D3 conic streams" <| fun _ ->
        use json=fixture "conic-streams.json"
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            match item.TryGetProperty("points") with
            | false,_ -> ()
            | true,pointsJson ->
                let kind=kindOf item
                let name=sprintf "%A %s" kind (item.GetProperty("name").GetString())
                let positions=pointsJson.EnumerateArray() |> Seq.map(fun value -> p (value[0].GetDouble()) (value[1].GetDouble())) |> Seq.toList
                let precision=item.GetProperty("precision").GetDouble()
                let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=precision} (configured item) (GeoGeometry.LineString positions) |> ok
                let expected=item.GetProperty("fragments").EnumerateArray() |> Seq.toArray
                Expect.equal paths.Length expected.Length (name+" fragment count")
                // D3 clips transverse Mercator after resampling; D5 clips its spherical band first.
                let tolerance=if kind=ProjectionKind.TransverseMercator then 2.*precision+1e-9 else 1e-6
                for index=0 to paths.Length-1 do
                    let actual=vertices paths[index]
                    let reference=expected[index]
                    for point,target in [actual.Head,reference[0];List.last actual,reference[reference.GetArrayLength()-1]] do
                        Expect.isLessThan (Double.Hypot(point.X-target[0].GetDouble(),point.Y-target[1].GetDouble())) tolerance (name+" fragment endpoint")
    testCase "Sphere outlines match the D3 projected world" <| fun _ ->
        use json=fixture "conic-streams.json"
        for item in json.RootElement.GetProperty("spheres").EnumerateArray() do
            let kind=kindOf item
            let projection=Projection.create {ProjectionConfig.defaultValue kind with Scale=item.GetProperty("scale").GetDouble()} |> ok
            let precision=item.GetProperty("precision").GetDouble()
            let actual=GeoPath.project {GeoPathOptions.defaultValue with Precision=precision} projection GeoGeometry.Sphere |> ok |> rings
            Expect.equal actual.Length 1 (sprintf "%A one world outline" kind)
            let expected=item.GetProperty("area").GetDouble()
            let tolerance=max (1e-4*expected) (item.GetProperty("perimeter").GetDouble()*2.*precision)
            Expect.isLessThan (abs(abs(actual |> List.sumBy Polygon.area)-expected)) tolerance (sprintf "%A projected world area" kind)
    testCase "equal-area conics preserve the sphere's area" <| fun _ ->
        let projection=Projection.create {ProjectionConfig.defaultValue (ProjectionKind.ConicEqualArea(29.5,45.5)) with Scale=100.} |> ok
        let world=GeoPath.project {GeoPathOptions.defaultValue with Precision=0.01} projection GeoGeometry.Sphere |> ok |> rings
        Expect.isLessThan (abs(abs(world |> List.sumBy Polygon.area)/(4.*Math.PI*1e4)-1.)) 1e-3 "unit sphere area 4 pi"
    testCase "transverse Mercator clips its rotated poles and keeps a finite world" <| fun _ ->
        let projection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.TransverseMercator with Scale=1.} |> ok
        Expect.isNone (Projection.project projection (p 90. 0.)) "rotated pole on the equator is singular"
        let paths=GeoPath.project GeoPathOptions.defaultValue projection (GeoGeometry.LineString[p 0. 0.;p 120. 0.]) |> ok
        for point in paths |> List.collect vertices do
            Expect.isLessThanOrEqual (abs point.X) (Math.PI+1e-9) "finite canonical world"
            Expect.isLessThanOrEqual (abs point.Y) (Math.PI+1e-9) "finite canonical world"
]

module Ranvier.D5.Tests.GeoAzimuthalTests

open System
open System.IO
open System.Text.Json
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private p longitude latitude = GeoPosition.create longitude latitude |> ok
let private fixture name = JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures",name)))
let private kindOf = function
    | "AzimuthalEqualArea" -> ProjectionKind.AzimuthalEqualArea
    | "AzimuthalEquidistant" -> ProjectionKind.AzimuthalEquidistant
    | "Gnomonic" -> ProjectionKind.Gnomonic
    | "Stereographic" -> ProjectionKind.Stereographic
    | "Orthographic" -> ProjectionKind.Orthographic
    | value -> failtestf "Unknown projection %s" value
let private azimuthal=[ProjectionKind.AzimuthalEqualArea;ProjectionKind.AzimuthalEquidistant;ProjectionKind.Gnomonic;ProjectionKind.Stereographic]
let private vertices path = Path.commands path |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
let private configured (item:JsonElement) =
    let rotation =
        match item.TryGetProperty("rotate") with
        | true,angles -> GeoRotation.create (angles[0].GetDouble()) (angles[1].GetDouble()) (angles[2].GetDouble()) |> ok
        | _ -> GeoRotation.create 0. 0. 0. |> ok
    let kind=kindOf(item.GetProperty("kind").GetString())
    Projection.create {ProjectionConfig.defaultValue kind with Scale=item.GetProperty("scale").GetDouble();Rotation=rotation;ClipAngle=Some(item.GetProperty("clipAngle").GetDouble())} |> ok
let private radius kind angle =
    let c=angle*Math.PI/180.
    match kind with
    | ProjectionKind.AzimuthalEqualArea -> 2.*sin(c/2.)
    | ProjectionKind.AzimuthalEquidistant -> c
    | ProjectionKind.Gnomonic -> tan c
    | ProjectionKind.Stereographic -> tan(c/2.)
    | _ -> sin c

[<Tests>]
let tests = testList "azimuthal projections" [
    testCase "match pinned D3 coordinates, definedness and inverses" <| fun _ ->
        use json=fixture "azimuthal-projections.json"
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            let pair name = let value=item.GetProperty(name:string) in value[0].GetDouble(),value[1].GetDouble()
            let tx,ty=pair "translate"
            let cx,cy=pair "center"
            let angles=item.GetProperty("rotate")
            let config={ProjectionConfig.defaultValue (kindOf(item.GetProperty("kind").GetString())) with
                            Scale=item.GetProperty("scale").GetDouble();Translate=Point.create tx ty;Center=p cx cy
                            Rotation=GeoRotation.create (angles[0].GetDouble()) (angles[1].GetDouble()) (angles[2].GetDouble()) |> ok}
            let projection=Projection.create config |> ok
            let longitude,latitude=pair "point"
            let label=sprintf "%A %A" config.Kind (longitude,latitude)
            match Projection.project projection (p longitude latitude) with
            | None -> Expect.isFalse (item.GetProperty("defined").GetBoolean()) (label+" omitted only where undefined")
            | Some actual ->
                Expect.isTrue (item.GetProperty("defined").GetBoolean()) (label+" defined")
                let x,y=pair "projected"
                Expect.isLessThan (Double.Hypot(actual.X-x,actual.Y-y)) 1e-9 (label+" pinned screen coordinates")
                let inverse=Projection.tryInvert projection actual |> Option.defaultWith(fun () -> failtestf "%s inverse" label)
                let ix,iy=pair "inverse"
                let wrapped=(inverse.Longitude-ix+540.)%360.-180.
                Expect.isLessThan (abs wrapped) 1e-7 (label+" inverse longitude")
                Expect.isLessThan (abs(inverse.Latitude-iy)) 1e-7 (label+" inverse latitude")
    testCase "clip angles are validated per projection family" <| fun _ ->
        let config kind angle={ProjectionConfig.defaultValue kind with ClipAngle=angle}
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.Mercator] do
            Expect.isError (Projection.create (config kind (Some 45.))) "cylindrical projections cut at the antimeridian"
        for angle in [0.;-1.;nan;infinity] do
            Expect.isError (Projection.create (config ProjectionKind.Stereographic (Some angle))) "positive finite angle"
        Expect.isError (Projection.create (config ProjectionKind.Gnomonic (Some 90.))) "gnomonic horizon is singular"
        Expect.isError (Projection.create (config ProjectionKind.Orthographic (Some 90.5))) "orthographic folds beyond the limb"
        Expect.isError (Projection.create (config ProjectionKind.AzimuthalEqualArea (Some 180.))) "antipode is singular"
        for kind in ProjectionKind.Orthographic::azimuthal do
            Expect.isOk (Projection.create (config kind None)) "default clip angle"
            Expect.isOk (Projection.create (config kind (Some 30.))) "narrow cap"
    testCase "polygon topology and filled area match pinned D3 small-circle clipping" <| fun _ ->
        use json=fixture "azimuthal-streams.json"
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            match item.TryGetProperty("rings") with
            | false,_ -> ()
            | true,ringsJson ->
                let name=item.GetProperty("kind").GetString()+" "+item.GetProperty("name").GetString()
                let rings=ringsJson.EnumerateArray() |> Seq.map(fun ring -> ring.EnumerateArray() |> Seq.map(fun point -> p (point[0].GetDouble()) (point[1].GetDouble())) |> Seq.toList) |> Seq.toList
                let precision=item.GetProperty("precision").GetDouble()
                let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=precision} (configured item) (GeoGeometry.Polygon rings) |> ok
                let actual=paths |> List.collect(fun path ->
                    let rings=ResizeArray<Point list>()
                    let mutable current=[]
                    for command in Path.commands path do
                        match command with
                        | MoveTo point -> current<-[point]
                        | LineTo point -> current<-point::current
                        | Close -> rings.Add(List.rev current);current<-[]
                        | _ -> failtest "sampled polygon commands"
                    List.ofSeq rings)
                let expected=item.GetProperty("output").EnumerateArray() |> Seq.map(fun ring -> ring.EnumerateArray() |> Seq.map(fun point -> {X=point[0].GetDouble();Y=point[1].GetDouble()}) |> Seq.toList) |> Seq.toList
                Expect.equal actual.Length expected.Length (name+" ring topology")
                let perimeter=expected |> List.sumBy(fun ring -> match ring with [] -> 0. | first::_ -> (ring@[first]) |> List.pairwise |> List.sumBy(fun (a,b) -> sqrt(Point.distanceSquared a b)))
                let combined=2.*precision
                let tolerance=max 1e-8 (perimeter*combined+Math.PI*combined*combined)
                Expect.isLessThan (abs((actual |> List.sumBy Polygon.area)-(expected |> List.sumBy Polygon.area))) tolerance (name+" oriented filled area")
    testCase "line fragments and analytic cap intersections match pinned D3 streams" <| fun _ ->
        use json=fixture "azimuthal-streams.json"
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            match item.TryGetProperty("points") with
            | false,_ -> ()
            | true,pointsJson ->
                let name=item.GetProperty("kind").GetString()+" "+item.GetProperty("name").GetString()
                let positions=pointsJson.EnumerateArray() |> Seq.map(fun value -> p (value[0].GetDouble()) (value[1].GetDouble())) |> Seq.toList
                let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=item.GetProperty("precision").GetDouble()} (configured item) (GeoGeometry.LineString positions) |> ok
                let expected=item.GetProperty("fragments").EnumerateArray() |> Seq.toArray
                Expect.equal paths.Length expected.Length (name+" fragment count")
                for index=0 to paths.Length-1 do
                    let actual=vertices paths[index]
                    let reference=expected[index]
                    for point,target in [actual.Head,reference[0];List.last actual,reference[reference.GetArrayLength()-1]] do
                        Expect.isLessThan (Double.Hypot(point.X-target[0].GetDouble(),point.Y-target[1].GetDouble())) 1e-6 (name+" fragment endpoint")
    testCase "arcs ending exactly on the orthographic limb keep their snapped boundary point" <| fun _ ->
        let projection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with Scale=92.;Rotation=GeoRotation.create -120. -20. 0. |> ok} |> ok
        let lines=(Graticule.create GraticuleConfig.defaultValue |> ok).Lines
        Expect.isOk (GeoPath.project GeoPathOptions.defaultValue projection (GeoGeometry.MultiLineString lines)) "graticule touching the limb"
        Expect.isOk (GeoPath.project GeoPathOptions.defaultValue projection (GeoGeometry.LineString[p -152.5 0.;p -150. 0.])) "edge ending on the limb"
    testCase "Sphere draws the projected clip circle" <| fun _ ->
        for kind in ProjectionKind.Orthographic::azimuthal do
            for angle in [30.;60.] do
                let projection=Projection.create {ProjectionConfig.defaultValue kind with Scale=50.;Translate=Point.create 10. 20.;ClipAngle=Some angle} |> ok
                let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=0.05} projection GeoGeometry.Sphere |> ok
                Expect.equal paths.Length 1 "one disk"
                let expected=50.*radius kind angle
                for point in vertices paths.Head do
                    Expect.isLessThan (abs(Double.Hypot(point.X-10.,point.Y-20.)-expected)) 0.05 (sprintf "%A boundary radius" kind)
    testCase "streamed points outside the clip circle are omitted" <| fun _ ->
        let projection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Gnomonic with Scale=10.} |> ok
        let options={GeoPathOptions.defaultValue with PointRadius=1.}
        Expect.equal (GeoPath.project options projection (GeoGeometry.Point(Some(p 50. 0.))) |> ok).Length 1 "inside default 60 degree cap"
        Expect.isEmpty (GeoPath.project options projection (GeoGeometry.Point(Some(p 70. 0.))) |> ok) "outside cap"
        Expect.isSome (Projection.project projection (p 70. 0.)) "point projection remains mathematical before the horizon"
    testCase "point projection omits horizons and antipodes" <| fun _ ->
        let at kind=Projection.create (ProjectionConfig.defaultValue kind) |> ok
        Expect.isNone (Projection.project (at ProjectionKind.Gnomonic) (p 90. 0.)) "gnomonic horizon"
        Expect.isNone (Projection.project (at ProjectionKind.Gnomonic) (p 120. 0.)) "gnomonic far side"
        for kind in [ProjectionKind.AzimuthalEqualArea;ProjectionKind.AzimuthalEquidistant;ProjectionKind.Stereographic] do
            Expect.isNone (Projection.project (at kind) (p 180. 0.)) (sprintf "%A antipode" kind)
            Expect.isSome (Projection.project (at kind) (p 179. 0.)) (sprintf "%A near antipode" kind)
    testCase "inverse rejects points outside the finite azimuthal domain" <| fun _ ->
        let at kind=Projection.create (ProjectionConfig.defaultValue kind) |> ok
        Expect.isNone (Projection.tryInvert (at ProjectionKind.AzimuthalEqualArea) (Point.create 2.1 0.)) "equal-area disk radius 2"
        Expect.isNone (Projection.tryInvert (at ProjectionKind.AzimuthalEquidistant) (Point.create 0. 3.2)) "equidistant disk radius pi"
        Expect.isSome (Projection.tryInvert (at ProjectionKind.Stereographic) (Point.create 50. 0.)) "stereographic plane"
        Expect.isSome (Projection.tryInvert (at ProjectionKind.Gnomonic) (Point.create 50. 0.)) "gnomonic plane"
]

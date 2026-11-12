module Ranvier.D5.Tests.IdentityProjectionTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private identity () = IdentityProjection.create IdentityProjectionConfig.defaultValue |> ok
let private point = Point.create
let private rings paths =
    let result=ResizeArray<Point list>()
    for path in paths do
        let mutable current=[]
        for command in Path.commands path do
            match command with
            | MoveTo value -> current<-[value]
            | LineTo value -> current<-value::current
            | Close -> result.Add(List.rev current);current<-[]
            | _ -> failtest "Expected a straight polygon"
        Expect.isEmpty current "closed rings"
    List.ofSeq result

[<Tests>]
let tests = testList "planar identity" [
    testCase "identity point mappings and clipped polygons match pinned D3 fixtures" <| fun _ ->
        use json=System.Text.Json.JsonDocument.Parse(System.IO.File.ReadAllText(System.IO.Path.Combine(AppContext.BaseDirectory,"fixtures","identity.json")))
        let p (value:System.Text.Json.JsonElement)=point (value[0].GetDouble()) (value[1].GetDouble())
        let config (value:System.Text.Json.JsonElement)=
            {Scale=value.GetProperty("scale").GetDouble();Translate=p(value.GetProperty("translate"))
             AngleDegrees=value.GetProperty("angle").GetDouble();ReflectX=value.GetProperty("reflectX").GetBoolean();ReflectY=value.GetProperty("reflectY").GetBoolean()}
        for item in json.RootElement.GetProperty("points").EnumerateArray() do
            let projection=IdentityProjection.create (config(item.GetProperty("config"))) |> ok
            let source=p(item.GetProperty("input"))
            let actual=IdentityProjection.project projection source |> Option.get
            let expected=p(item.GetProperty("output"))
            Expect.isLessThan (Point.distanceSquared actual expected) 1e-16 "point mapping"
            let inverse=IdentityProjection.invert projection actual |> Option.get
            Expect.isLessThan (Point.distanceSquared inverse source) 1e-16 "round trip"
        for item in json.RootElement.GetProperty("polygons").EnumerateArray() do
            let projection=IdentityProjection.create (config(item.GetProperty("config"))) |> ok
            let readRings (name:string)=item.GetProperty(name).EnumerateArray() |> Seq.map(fun ring -> ring.EnumerateArray() |> Seq.map p |> Seq.toList) |> Seq.toList
            let extent=item.GetProperty("clip")
            let a,b=p extent[0],p extent[1]
            let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create a.X a.Y (b.X-a.X) (b.Y-a.Y))}
            let actual=PlanarPath.project options projection (PlanarGeometry.Polygon(readRings "rings")) |> ok |> rings
            let expected=readRings "output"
            Expect.equal actual.Length expected.Length "clipped ring topology"
            let area=expected |> List.sumBy Polygon.area
            Expect.isLessThan (abs((actual |> List.sumBy Polygon.area)-area)) (max 1e-9 (abs area*1e-10)) "straight-edge area"
    testCase "pinned reflected-hole counterexample retains upstream evidence" <| fun _ ->
        use json=System.Text.Json.JsonDocument.Parse(System.IO.File.ReadAllText(System.IO.Path.Combine(AppContext.BaseDirectory,"fixtures","identity.json")))
        let item=json.RootElement.GetProperty("windingCounterexample")
        let read (name:string)=item.GetProperty(name).EnumerateArray() |> Seq.map(fun ring -> ring.EnumerateArray() |> Seq.map(fun p -> point (p[0].GetDouble()) (p[1].GetDouble())) |> Seq.toList) |> Seq.toList
        Expect.equal (read "output" |> List.sumBy Polygon.area) 416. "unchanged D3 output adds the hole"
        let projection=IdentityProjection.create {IdentityProjectionConfig.defaultValue with ReflectX=true} |> ok
        let actual=PlanarPath.project {GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -10. -10. 20. 20.)} projection (PlanarGeometry.Polygon(read "rings")) |> ok |> rings
        Expect.equal (actual |> List.sumBy Polygon.area) (item.GetProperty("expectedFilledArea").GetDouble()) "D5 subtracts the hole"
    testCase "identity keeps planar units outside geographic bounds" <| fun _ ->
        let projection=identity()
        let p=point 123456. -987654.
        Expect.equal (IdentityProjection.project projection p) (Some p) "no degree conversion or spherical bounds"
        Expect.equal (IdentityProjection.invert projection p) (Some p) "identity inverse"
    testCase "scale reflection rotation and translation have explicit order" <| fun _ ->
        let projection=IdentityProjection.create {IdentityProjectionConfig.defaultValue with Scale=2.;ReflectX=true;AngleDegrees=90.;Translate=point 100. 200.} |> ok
        let source=point 3. 4.
        Expect.equal (IdentityProjection.project projection source) (Some(point 108. 206.)) "D3 identity angle convention"
        Expect.equal (IdentityProjection.invert projection (point 108. 206.)) (Some source) "inverse transform"
    testCase "invalid configuration and unrepresentable points are explicit" <| fun _ ->
        for config in [
            {IdentityProjectionConfig.defaultValue with Scale=0.}
            {IdentityProjectionConfig.defaultValue with Scale= -1.}
            {IdentityProjectionConfig.defaultValue with Scale=Double.PositiveInfinity}
            {IdentityProjectionConfig.defaultValue with AngleDegrees=Double.NaN}
            {IdentityProjectionConfig.defaultValue with Translate={X=Double.NaN;Y=0.}}
        ] do Expect.isError (IdentityProjection.create config) "invalid transform"
        let huge=IdentityProjection.create {IdentityProjectionConfig.defaultValue with Scale=Double.MaxValue} |> ok
        Expect.isNone (IdentityProjection.project huge (point 2. 2.)) "overflow"
        Expect.isNone (IdentityProjection.project (identity()) {X=Double.NaN;Y=0.}) "invalid input"
    testCase "planar lines stay straight and are not split at geographic seams" <| fun _ ->
        let points=[point 170. 200.;point -170. 400.]
        let paths=PlanarPath.project GeoPathOptions.defaultValue (identity()) (PlanarGeometry.LineString points) |> ok
        Expect.equal (paths |> List.map Path.commands) [[MoveTo points[0];LineTo points[1]]] "one straight segment"
    testCase "viewport clipping splits excursions and omits corner-only touches" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create 0. 0. 10. 10.)}
        let run points=PlanarPath.project options (identity()) (PlanarGeometry.LineString points) |> ok
        Expect.equal (run [point 5. 5.;point 20. 5.;point 20. 8.;point 5. 8.] |> List.map Path.commands)
            [[MoveTo(point 5. 5.);LineTo(point 10. 5.)];[MoveTo(point 10. 8.);LineTo(point 5. 8.)]] "disconnected strokes"
        Expect.isEmpty (run [point -1. -1.;point 0. 0.]) "isolated boundary touch"
    testCase "planar holes remain compound and reversing does not add a world" <| fun _ ->
        let outer=[point 0. 0.;point 10. 0.;point 10. 10.;point 0. 10.;point 0. 0.]
        let hole=[point 2. 2.;point 2. 8.;point 8. 8.;point 8. 2.;point 2. 2.]
        let run rings=PlanarPath.project GeoPathOptions.defaultValue (identity()) (PlanarGeometry.Polygon rings) |> ok
        let result=run [outer;hole]
        Expect.equal result.Length 1 "one compound path"
        Expect.equal (rings result |> List.sumBy Polygon.area) 64. "hole area"
        Expect.equal (run [List.rev outer] |> rings |> List.sumBy Polygon.area) -100. "reversal changes winding only"
    testCase "clipped reflected polygons preserve holes when the viewport is inside the exterior" <| fun _ ->
        let outer=[point -20. -20.;point 20. -20.;point 20. 20.;point -20. 20.;point -20. -20.]
        let hole=[point -2. -2.;point -2. 2.;point 2. 2.;point 2. -2.;point -2. -2.]
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -10. -10. 20. 20.)}
        for reverse,reflect in [false,false;true,false;false,true;true,true] do
            let source=if reverse then [List.rev outer;List.rev hole] else [outer;hole]
            let projection=IdentityProjection.create {IdentityProjectionConfig.defaultValue with ReflectX=reflect} |> ok
            let result=PlanarPath.project options projection (PlanarGeometry.Polygon source) |> ok |> rings
            Expect.equal result.Length 2 "viewport and enclosed hole"
            Expect.equal (abs(List.sumBy Polygon.area result)) 384. "opposite winding survives clipping and reflection"
    testCase "clipped concave polygons can have disconnected components" <| fun _ ->
        let ring=[point -40. -40.;point -40. 40.;point -20. 40.;point -20. -20.;point 20. -20.;point 20. 40.;point 40. 40.;point 40. -40.;point -40. -40.] |> List.rev
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -50. 5. 100. 10.)}
        let paths=PlanarPath.project options (identity()) (PlanarGeometry.Polygon[ring]) |> ok
        Expect.equal (rings paths |> List.length) 2 "two visible arms"
        Expect.equal (rings paths |> List.sumBy Polygon.area) 400. "clipped area"
    testCase "empty malformed and nonfinite geometry retain validation contracts" <| fun _ ->
        let run geometry=PlanarPath.project GeoPathOptions.defaultValue (identity()) geometry
        Expect.equal (run (PlanarGeometry.Collection[]) |> ok) [] "empty collection"
        Expect.equal (run (PlanarGeometry.Polygon[]) |> ok) [] "empty polygon"
        for geometry in [PlanarGeometry.LineString[point 0. 0.];PlanarGeometry.Polygon[[]];PlanarGeometry.Polygon[[point 0. 0.;point 1. 0.;point 1. 1.;point 0. 1.]];PlanarGeometry.Point(Some{X=Double.NaN;Y=0.})] do
            Expect.isError (run geometry) "invalid input"
    testCase "planar point symbols use the shared circle and clipping policy" <| fun _ ->
        let geometry=PlanarGeometry.Point(Some(point 0. 0.))
        let options={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Bounds.create -0.5 -0.5 1. 1.)}
        let paths=PlanarPath.project options (identity()) geometry |> ok
        Expect.isLessThan (abs((rings paths |> List.sumBy Polygon.area)-1.)) 1e-12 "viewport enclosed by symbol"
        Expect.equal (PlanarPath.project {options with PointRadius=0.} (identity()) geometry |> ok) [] "zero radius"
    testCase "multipolygons retain ownership across reused clipping buffers" <| fun _ ->
        let square x=[point x 0.;point (x+2.) 0.;point (x+2.) 2.;point x 2.;point x 0.]
        let geometry=PlanarGeometry.MultiPolygon[[square 0.];[square 10.]]
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -1. -1. 20. 5.)}
        let result=PlanarPath.project options (identity()) geometry |> ok
        Expect.equal result.Length 2 "independent polygon paths"
        let before=result |> List.map Path.commands
        PlanarPath.project options (identity()) (PlanarGeometry.Polygon[square 3.]) |> ok |> ignore
        Expect.equal (result |> List.map Path.commands) before "published paths own their coordinates"
        Expect.equal (rings result |> List.sumBy Polygon.area) 8. "both squares"
    testCase "empty viewports validate input but bypass invisible projection overflow" <| fun _ ->
        let projection=IdentityProjection.create {IdentityProjectionConfig.defaultValue with Scale=Double.MaxValue} |> ok
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create 0. 0. 0. 10.)}
        Expect.equal (PlanarPath.project options projection (PlanarGeometry.LineString[point 2. 2.;point 3. 3.]) |> ok) [] "no invisible transform"
        Expect.isError (PlanarPath.project options projection (PlanarGeometry.LineString[point 2. 2.])) "malformed line still rejected"
        Expect.isError (PlanarPath.project options projection (PlanarGeometry.Point(Some{X=Double.NaN;Y=0.}))) "nonfinite input still rejected"
    testCase "clipped polygons count final vertices separately from input and retained buffers" <| fun _ ->
        let ring=[point -20. -20.;point 0. -20.;point 20. -20.;point 20. 20.;point 0. 20.;point -20. 20.;point -20. -20.]
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -1. -1. 2. 2.);MaxOutputPoints=4}
        let run options=PlanarPath.project options (identity()) (PlanarGeometry.Polygon[ring])
        Expect.equal (run options |> ok |> rings |> List.sumBy List.length) 4 "discarded vertices do not consume output budget"
        Expect.isError (run {options with MaxOutputPoints=3}) "output budget"
        Expect.isError (run {options with MaxBufferedPoints=5}) "intermediate polygon buffer budget"
        Expect.isError (run {options with InputLimits={GeoStreamConfig.defaultValue with MaxWork=3L}}) "independent input work budget"
    testCase "invalid path options and unrepresentable inverse are explicit" <| fun _ ->
        let projection=identity()
        let geometry=PlanarGeometry.Point(Some(point 0. 0.))
        for options in [{GeoPathOptions.defaultValue with Precision=0.};{GeoPathOptions.defaultValue with PointRadius=Double.NaN};{GeoPathOptions.defaultValue with MaxSubdivisionDepth=33};{GeoPathOptions.defaultValue with InputLimits={GeoStreamConfig.defaultValue with MaxDepth=129}}] do
            Expect.isError (PlanarPath.project options projection geometry) "invalid policy"
        let tiny=IdentityProjection.create {IdentityProjectionConfig.defaultValue with Scale=System.Double.Epsilon} |> ok
        Expect.isNone (IdentityProjection.invert tiny (point 1. 1.)) "inverse overflow"
        Expect.isSome (IdentityProjection.project tiny (point 1. 1.)) "representable forward result"
    testCase "large planar streams observe cancellation" <| fun _ ->
        let geometry=PlanarGeometry.LineString[for i in 0..199999 -> point (float i) (float(i%2))]
        use cancelled=new CancellationTokenSource()
        cancelled.CancelAfter(1)
        Expect.throwsT<OperationCanceledException> (fun () -> PlanarPath.projectWithCancellation GeoPathOptions.defaultValue cancelled.Token (identity()) geometry |> ignore) "cancellation during traversal or output"
    testCase "output work input depth and cancellation are independently bounded" <| fun _ ->
        let geometry=PlanarGeometry.LineString[point 0. 0.;point 10. 0.]
        for options in [
            {GeoPathOptions.defaultValue with MaxOutputPoints=1}
            {GeoPathOptions.defaultValue with MaxWork=1L}
            {GeoPathOptions.defaultValue with MaxBufferedPoints=1}
            {GeoPathOptions.defaultValue with InputLimits={GeoStreamConfig.defaultValue with MaxPoints=1}}
        ] do Expect.isError (PlanarPath.project options (identity()) geometry) "bounded path"
        let nested=PlanarGeometry.Collection[PlanarGeometry.Collection[geometry]]
        Expect.isError (PlanarPath.project {GeoPathOptions.defaultValue with InputLimits={GeoStreamConfig.defaultValue with MaxDepth=2}} (identity()) nested) "bounded depth"
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> PlanarPath.projectWithCancellation GeoPathOptions.defaultValue cancelled.Token (identity()) geometry |> ignore) "cancelled path"
]

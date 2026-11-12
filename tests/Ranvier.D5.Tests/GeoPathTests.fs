module Ranvier.D5.Tests.GeoPathTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private p longitude latitude = GeoPosition.create longitude latitude |> ok
let private projection kind = Projection.create (ProjectionConfig.defaultValue kind) |> ok

[<Tests>]
let tests = testList "geographic paths" [
    testCase "point paths use existing circle commands and omit hidden points" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.}
        let paths=GeoPath.project options (projection ProjectionKind.Orthographic) (GeoGeometry.MultiPoint[p 0. 0.;p 180. 0.;p 90. 0.]) |> ok
        Expect.equal paths.Length 2 "visible points only"
        match Path.commands paths.Head with
        | [MoveTo start;ArcTo(rx,ry,_,false,true,middle);ArcTo(_,_,_,false,true,finish);Close] ->
            Expect.equal (rx,ry) (2.,2.) "radius in screen units"
            Expect.equal start {X=2.;Y=0.} "right edge"
            Expect.equal middle {X= -2.;Y=0.} "left edge"
            Expect.equal finish start "closed circle"
        | commands -> failtestf "Unexpected point path %A" commands
    testCase "validates positive budgets precision and finite point radius" <| fun _ ->
        let options=GeoPathOptions.defaultValue
        let empty=GeoGeometry.Collection []
        let invalid=[{options with MaxOutputPoints=0};{options with MaxWork=0L};{options with MaxSubdivisionDepth=0};{options with MaxSubdivisionDepth=33};{options with Precision=0.};{options with Precision=nan};{options with PointRadius= -1.};{options with PointRadius=infinity}]
        for value in invalid do
            Expect.isError (GeoPath.project value (projection ProjectionKind.Equirectangular) empty) "invalid policy"
    testCase "bounds all emitted point endpoints and returns no partial success" <| fun _ ->
        let geometry=GeoGeometry.MultiPoint[p 0. 0.;p 10. 0.]
        let projected=projection ProjectionKind.Equirectangular
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=5} projected geometry) "each circle has three emitted endpoints"
        Expect.equal (GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=6} projected geometry |> ok |> List.length) 2 "exact output budget"
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with MaxWork=1L} projected geometry) "bounded work"
    testCase "input budgets cancellation and malformed geometry retain stream contracts" <| fun _ ->
        let options={GeoPathOptions.defaultValue with InputLimits={GeoStreamConfig.defaultValue with MaxPoints=1}}
        Expect.isError (GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.MultiPoint[p 0. 0.;p 1. 0.])) "input point budget"
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> GeoPath.projectWithCancellation GeoPathOptions.defaultValue cancelled.Token (projection ProjectionKind.Equirectangular) (GeoGeometry.Collection []) |> ignore) "pre-cancelled empty input"
    testCase "clips partial point circles within a bounded viewport" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Bounds.create 0. -3. 3. 6.)}
        let paths=GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.Point(Some(p 0. 0.))) |> ok
        Expect.equal paths.Length 1 "half disk"
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isTrue (vertices |> List.forall(fun point -> point.X>=0. && point.X<=3. && point.Y>= -3. && point.Y<=3.)) "inside clip"
        Expect.isLessThan (abs(abs(Polygon.area vertices)-2.*Math.PI)) 0.06 "half circle within requested approximation"
        Expect.equal (Path.commands paths.Head |> List.last) Close "closed clipped symbol"
    testCase "circle clipping preserves a viewport enclosed by the symbol" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=10.;Precision=0.01;ClipExtent=Some(Bounds.create -1. -1. 2. 2.)}
        let paths=GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.Point(Some(p 0. 0.))) |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isLessThan (abs(abs(Polygon.area vertices)-4.)) 1e-12 "whole viewport"
    testCase "clipped paths own their vertices across reused working buffers" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Bounds.create 0. -3. 3. 6.)}
        let positions=[p 0. 0.;p 30. 20.;p -10. -10.]
        let projected=projection ProjectionKind.Equirectangular
        let combined=GeoPath.project options projected (GeoGeometry.MultiPoint positions) |> ok
        let independent=positions |> List.collect(fun point -> GeoPath.project options projected (GeoGeometry.Point(Some point)) |> ok)
        Expect.equal combined independent "later clips never mutate previous output"
    testCase "point clipping bounds tessellation buffers and subdivision precision" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Bounds.create 0. -3. 3. 6.)}
        for limited in [{options with MaxBufferedPoints=8};{options with MaxSubdivisionDepth=1};{options with Precision=1e-300}] do
            match GeoPath.project limited (projection ProjectionKind.Equirectangular) (GeoGeometry.Point(Some(p 0. 0.))) with
            | Error [BudgetExceeded _] -> ()
            | result -> failtestf "Expected bounded tessellation failure, got %A" result
    testCase "projects a continuous equatorial line without point symbols" <| fun _ ->
        let paths=GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString[p 0. 0.;p 10. 0.]) |> ok
        Expect.equal paths.Length 1 "one connected line"
        match Path.commands paths.Head with
        | [MoveTo first;LineTo last] ->
            Expect.equal first {X=0.;Y=0.} "first endpoint"
            Expect.isLessThan (abs(last.X-Math.PI/18.)) 1e-12 "last endpoint"
            Expect.equal last.Y 0. "equator"
        | commands -> failtestf "Expected a line, got %A" commands
    testCase "resamples projected great-circle curvature in screen units" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=100.} |> ok
        let geometry=GeoGeometry.LineString[p -45. 60.;p 45. 60.]
        let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=0.1} projected geometry |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isGreaterThan vertices.Length 2 "curved arc is subdivided"
        let north=atan(tan(Math.PI/3.)/cos(Math.PI/4.))
        Expect.isLessThan (abs((vertices |> List.minBy(fun point -> point.Y)).Y + 100.*north)) 1e-10 "great-circle midpoint"
        Expect.isFalse (Path.commands paths.Head |> List.contains Close) "line remains open"
    testCase "resampling detects symmetric curvature that a midpoint misses" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=10000.} |> ok
        let first,last=p -5. -5.,p 5. 5.
        let options={GeoPathOptions.defaultValue with Precision=0.01}
        let paths=GeoPath.project options projected (GeoGeometry.LineString[first;last]) |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isGreaterThan vertices.Length 2 "symmetric arc still has curvature"
        let segments=List.pairwise vertices
        let distance (point:Point) (a:Point,b:Point) =
            let dx,dy=b.X-a.X,b.Y-a.Y
            let parameter=Math.Clamp(((point.X-a.X)*dx+(point.Y-a.Y)*dy)/(dx*dx+dy*dy),0.,1.)
            Double.Hypot(point.X-(a.X+parameter*dx),point.Y-(a.Y+parameter*dy))
        for index=0 to 100 do
            let point=Geodesic.interpolate (float index/100.) first last |> ok |> Projection.project projected |> Option.get
            Expect.isLessThan (segments |> List.map(distance point) |> List.min) 0.0101 "dense reference samples meet screen tolerance"
    testCase "line resampling preserves tiny and nearly antipodal unique arcs" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular
        let options={GeoPathOptions.defaultValue with Precision=1e-5;MaxOutputPoints=10000}
        let paths=GeoPath.project options projected (GeoGeometry.LineString[p 0. 0.;p 179.999 0.001]) |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isGreaterThan vertices.Length 2 "nearly antipodal arc is unique and subdivided"
        Expect.isTrue (vertices |> List.forall(fun point -> Double.IsFinite point.X && Double.IsFinite point.Y)) "finite output"
        Expect.equal vertices.Head (Projection.project projected (p 0. 0.) |> Option.get) "first endpoint"
        Expect.equal (List.last vertices) (Projection.project projected (p 179.999 0.001) |> Option.get) "last endpoint"
        let tiny=GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString[p 0. 0.;p 1e-200 1e-200]) |> ok
        match Path.commands tiny.Head with
        | [MoveTo _;LineTo point] -> Expect.isTrue (point.X>0. && point.Y<0.) "tiny endpoint survives"
        | commands -> failtestf "Unexpected tiny arc %A" commands
    testCase "resampling rejects unmet precision and exhausted output or buffer budgets" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=100.} |> ok
        let geometry=GeoGeometry.LineString[p -45. 60.;p 45. 60.]
        for options in [{GeoPathOptions.defaultValue with Precision=1e-10;MaxSubdivisionDepth=1};{GeoPathOptions.defaultValue with MaxOutputPoints=2};{GeoPathOptions.defaultValue with MaxBufferedPoints=2}] do
            match GeoPath.project options projected geometry with
            | Error [BudgetExceeded _] -> ()
            | result -> failtestf "Expected bounded resampling failure, got %A" result
    testCase "rejects point-radius overflow instead of emitting nonfinite path commands" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Translate={X=Double.MaxValue;Y=0.}} |> ok
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with PointRadius=Double.MaxValue} projected (GeoGeometry.Point(Some(p 0. 0.)))) "overflow is explicit"
]

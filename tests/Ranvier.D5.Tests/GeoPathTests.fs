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
    testCase "rejects point-radius overflow instead of emitting nonfinite path commands" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Translate={X=Double.MaxValue;Y=0.}} |> ok
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with PointRadius=Double.MaxValue} projected (GeoGeometry.Point(Some(p 0. 0.)))) "overflow is explicit"
]

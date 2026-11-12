module Ranvier.D5.Tests.GeoMeasureTests

open System
open System.Threading
open Expecto
open Ranvier.D5.Geo

let private ok = function Ok x -> x | Error e -> failtestf "Unexpected error: %A" e
let private p x y = GeoPosition.create x y |> ok
let private length geometry = GeoMeasure.length GeoStreamConfig.defaultValue geometry |> ok
let private near actual expected tolerance = Expect.isLessThanOrEqual (abs(actual-expected)) tolerance "Angular length"

let tests = testList "GeoMeasure" [
    testCase "length uses shortest great circle arcs across the dateline" <| fun _ ->
        near (length(GeoGeometry.LineString [p 170. 0.;p -170. 0.])) (Math.PI/9.) 1e-14
        near (length(GeoGeometry.LineString [p 0. 0.;p 90. 0.;p 90. 90.])) Math.PI 1e-14
        near (length(GeoGeometry.LineString [p 0. 0.;p 180. 0.])) Math.PI 1e-14
    testCase "polygon length includes closure and hole boundaries exactly once" <| fun _ ->
        let ring=[p 0. 0.;p 0. 90.;p 90. 0.;p 0. 0.]
        near (length(GeoGeometry.Polygon [ring])) (1.5*Math.PI) 1e-14
        near (length(GeoGeometry.Polygon [ring;List.rev ring])) (3.*Math.PI) 1e-14
        near (length(GeoGeometry.LineString ring.Tail)) Math.PI 1e-14
    testCase "collections sum independent lines and ignore isolated points and sphere" <| fun _ ->
        let geometry=GeoGeometry.Collection [GeoGeometry.Point(Some(p 30. 40.));GeoGeometry.MultiLineString [[p 0. 0.;p 90. 0.];[];[p 90. 90.;p 90. 0.]];GeoGeometry.Sphere]
        near (length geometry) Math.PI 1e-14
        near (length(GeoGeometry.MultiPoint[p 0. 0.;p 180. 0.])) 0. 0.
    testCase "degenerate and tiny arcs retain their scale and structural validation" <| fun _ ->
        let a=p 0. 0.
        near (length(GeoGeometry.LineString [a;a])) 0. 0.
        let tiny=length(GeoGeometry.LineString [a;p 1e-200 0.])
        Expect.isGreaterThan tiny 0. "Tiny finite arc survives"
        near (tiny/(Math.PI/180.*1e-200)) 1. 1e-14
        Expect.isError (GeoMeasure.length GeoStreamConfig.defaultValue (GeoGeometry.LineString [a])) "Manual malformed geometry is validated"
        Expect.isError (GeoMeasure.length {GeoStreamConfig.defaultValue with MaxPoints=1} (GeoGeometry.LineString [a;a])) "Traversal budgets propagate"
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> GeoMeasure.lengthWithCancellation GeoStreamConfig.defaultValue cancelled.Token GeoGeometry.Sphere |> ignore) "Cancellation propagates"
]

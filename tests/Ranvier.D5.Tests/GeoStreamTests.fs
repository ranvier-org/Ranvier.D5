module Ranvier.D5.Tests.GeoStreamTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok x -> x | Error e -> failtestf "Unexpected error: %A" e
let private p x y = GeoPosition.create x y |> ok
let private events geometry = GeoStream.fold GeoStreamConfig.defaultValue (fun acc event -> event::acc) [] geometry |> ok |> List.rev
let private emptyFeature={Id=None;Geometry=None;Properties=None;ForeignMembers=Map.empty}

let tests = testList "GeoStream" [
    testCase "polygon stream preserves rings but omits closing duplicate" <| fun _ ->
        let a,b,c=p 0. 0.,p 0. 10.,p 10. 0.
        let d,e,f=p 1. 1.,p 2. 1.,p 1. 2.
        let geometry=GeoGeometry.Polygon [[a;b;c;a];[d;e;f;d]]
        Expect.equal (events geometry)
            [GeoStreamEvent.PolygonStart;GeoStreamEvent.LineStart;GeoStreamEvent.Point a;GeoStreamEvent.Point b;GeoStreamEvent.Point c;GeoStreamEvent.LineEnd;
             GeoStreamEvent.LineStart;GeoStreamEvent.Point d;GeoStreamEvent.Point e;GeoStreamEvent.Point f;GeoStreamEvent.LineEnd;GeoStreamEvent.PolygonEnd] "Retain polygon topology and winding"
    testCase "nested collections and nullable features stream in source order" <| fun _ ->
        let a,b=p 1. 2.,p 3. 4.
        let geometry=GeoGeometry.Collection [GeoGeometry.Point None;GeoGeometry.MultiPoint [a;b];GeoGeometry.Collection [GeoGeometry.Sphere];GeoGeometry.MultiLineString [[];[b;a]];GeoGeometry.MultiPolygon [[]]]
        let document=GeoDocument.FeatureCollection([emptyFeature;{emptyFeature with Geometry=Some geometry}],Map.empty)
        let actual=GeoStream.foldDocument GeoStreamConfig.defaultValue (fun acc event -> event::acc) [] document |> ok |> List.rev
        Expect.equal actual [GeoStreamEvent.Point a;GeoStreamEvent.Point b;GeoStreamEvent.Sphere;GeoStreamEvent.LineStart;GeoStreamEvent.LineEnd;GeoStreamEvent.LineStart;GeoStreamEvent.Point b;GeoStreamEvent.Point a;GeoStreamEvent.LineEnd;GeoStreamEvent.PolygonStart;GeoStreamEvent.PolygonEnd] "No invented null-feature events"
    testCase "manual malformed lines and rings cannot bypass interchange validation" <| fun _ ->
        let a,b,c=p 0. 0.,p 0. 10.,p 10. 0.
        for geometry in [GeoGeometry.LineString [a];GeoGeometry.Polygon [[a;b;a]];GeoGeometry.Polygon [[a;b;c;b]]] do
            Expect.isError (GeoStream.fold GeoStreamConfig.defaultValue (fun n _ -> n+1) 0 geometry) "Reject malformed spherical structure"
        Expect.isEmpty (events(GeoGeometry.Collection [])) "Empty collection emits nothing"
    testCase "point depth and work budgets apply across an entire document" <| fun _ ->
        let a=p 0. 0.
        let feature={emptyFeature with Geometry=Some(GeoGeometry.MultiPoint [a;a])}
        let document=GeoDocument.FeatureCollection([feature;feature],Map.empty)
        match GeoStream.foldDocument {GeoStreamConfig.defaultValue with MaxPoints=3} (fun n _ -> n+1) 0 document with
        | Error [BudgetExceeded _] -> ()
        | result -> failtestf "Expected shared point budget: %A" result
        let nested=List.fold(fun g _ -> GeoGeometry.Collection [g]) (GeoGeometry.Point(Some a)) [1..100]
        for config,geometry in [{GeoStreamConfig.defaultValue with MaxDepth=4},nested;{GeoStreamConfig.defaultValue with MaxWork=5L},GeoGeometry.Collection(List.replicate 100 (GeoGeometry.Collection []))] do
            match GeoStream.fold config (fun n _ -> n+1) 0 geometry with
            | Error [BudgetExceeded _] -> ()
            | result -> failtestf "Expected bounded traversal: %A" result
    testCase "point traversal does not allocate one event object per vertex" <| fun _ ->
        let geometry=GeoGeometry.MultiPoint(List.replicate 100000 (p 10. 20.))
        let run () = GeoStream.fold GeoStreamConfig.defaultValue (fun n event -> match event with GeoStreamEvent.Point _ -> n+1 | _ -> n) 0 geometry |> ok
        for _ in 1..3 do run() |> ignore
        let before=GC.GetAllocatedBytesForCurrentThread()
        let count=run()
        let allocated=GC.GetAllocatedBytesForCurrentThread()-before
        Expect.equal count 100000 "Every input point is visited"
        Expect.isLessThan allocated 65536L "Streaming must not allocate per input vertex"
    testCase "cancellation during a fold prevents later callbacks" <| fun _ ->
        use cancellation=new CancellationTokenSource()
        let mutable seen=0
        let folder state event =
            match event with GeoStreamEvent.Point _ -> seen<-seen+1;cancellation.Cancel() | _ -> ()
            state+1
        Expect.throwsT<OperationCanceledException> (fun () -> GeoStream.foldWithCancellation GeoStreamConfig.defaultValue cancellation.Token folder 0 (GeoGeometry.MultiPoint [p 1. 2.;p 3. 4.]) |> ignore) "Observe cancellation during traversal"
        Expect.equal seen 1 "No callback after cancellation"
]

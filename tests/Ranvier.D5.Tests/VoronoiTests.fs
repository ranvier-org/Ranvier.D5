module Ranvier.D5.Tests.VoronoiTests

open System
open Expecto
open Ranvier.D5

let private p x y=Point.create x y
let tests=testList "Voronoi" [
    testCase "pinned D3 adjacency and cell areas agree" <| fun _ ->
        use fixture=System.Text.Json.JsonDocument.Parse(System.IO.File.ReadAllText(System.IO.Path.Combine(AppContext.BaseDirectory,"fixtures","triangulation.json")))
        for sample in fixture.RootElement.EnumerateArray() do
            let points=[for point in sample.GetProperty("points").EnumerateArray() -> p (point[0].GetDouble()) (point[1].GetDouble())]
            let mesh=Delaunay.build points
            let expected=[for edge in sample.GetProperty("edges").EnumerateArray() -> edge[0].GetInt32(),edge[1].GetInt32()]
            let actual=[for i in 0..points.Length-1 do for j in Delaunay.neighbors i mesh do if i<j then yield i,j]
            Expect.equal actual expected "nondegenerate reference edges"
            let cells=Voronoi.clip (Bounds.create 0. 0. 100. 100.) mesh
            let areas=sample.GetProperty("areas")
            for i in 0..cells.Length-1 do
                Expect.floatClose Accuracy.medium (Polygon.area cells[i].Vertices) (areas[i].GetDouble()) "reference clipped area"

    testCase "empty singleton duplicate and collinear cells" <| fun _ ->
        let bounds=Bounds.create 0. 0. 10. 10.
        Expect.isEmpty (Delaunay.build [] |> Voronoi.clip bounds) "empty"
        let single=Delaunay.build [p 5. 5.;p 5. 5.] |> Voronoi.clip bounds
        Expect.equal single[0].InputIndices [0;1] "duplicates share one cell"
        Expect.floatClose Accuracy.high (Polygon.area single[0].Vertices) 100. "singleton covers viewport"
        let cells=Delaunay.build [p 2. 5.;p 8. 5.] |> Voronoi.clip bounds
        Expect.equal (cells |> List.map(fun cell -> Polygon.area cell.Vertices)) [50.;50.] "bisector at x=5"
        Expect.isTrue (cells[0].Vertices |> List.forall(fun point -> point.X<=5.)) "left cell"
    testCase "singleton degenerate viewports contain distinct open-ring vertices" <| fun _ ->
        let mesh=Delaunay.build [p 5. 5.]
        for bounds,expected in [Bounds.create 2. 3. 0. 0.,[p 2. 3.];
                                Bounds.create 2. 3. 0. 8.,[p 2. 3.;p 2. 11.];
                                Bounds.create 2. 3. 8. 0.,[p 2. 3.;p 10. 3.]] do
            Expect.equal (Voronoi.clip bounds mesh).Head.Vertices expected "collapsed viewport does not repeat closing corner"
    testCase "cells partition viewport and satisfy all nearest-site constraints" <| fun _ ->
        let points=[p 1. 1.;p 9. 1.;p 5. 4.;p 2. 8.;p 9. 9.;p -50. 20.]
        let bounds=Bounds.create 0. 0. 10. 10.
        let cells=Delaunay.build points |> Voronoi.clip bounds
        Expect.floatClose Accuracy.high (cells |> List.sumBy(fun c -> Polygon.area c.Vertices)) 100. "area conservation"
        for cell in cells do
            for vertex in cell.Vertices do
                Expect.isTrue (Bounds.contains vertex bounds) "clipping containment"
                let distance point=(vertex.X-point.X)**2.+(vertex.Y-point.Y)**2.
                for point in points do Expect.isLessThanOrEqual (distance points[cell.Site]-distance point) 1e-10 "own site is nearest"
    testCase "extreme site coordinates produce a finite central bisector" <| fun _ ->
        let bounds=Bounds.create -10. -10. 20. 20.
        let cells=Delaunay.build [p -1e308 0.;p 1e308 0.] |> Voronoi.clip bounds
        Expect.equal (cells |> List.map(fun c -> Polygon.area c.Vertices)) [200.;200.] "overflow-safe bisector"
        for cell in cells do for point in cell.Vertices do Expect.isTrue (Bounds.contains point bounds) "finite within bounds"
    testCase "subnormal sites and zero-area viewport do not invent NaNs" <| fun _ ->
        let mesh=Delaunay.build [p 0. 0.;p Double.Epsilon 0.;p 0. Double.Epsilon]
        let cells=Voronoi.clip (Bounds.create -1. -1. 2. 2.) mesh
        Expect.floatClose Accuracy.high (cells |> List.sumBy(fun c -> Polygon.area c.Vertices)) 4. "subnormal partition area"
        let zero=Voronoi.clip (Bounds.create 0. 0. 0. 0.) mesh
        for cell in zero do Expect.isTrue (cell.Vertices |> List.forall((=) (p 0. 0.))) "degenerate viewport"
]

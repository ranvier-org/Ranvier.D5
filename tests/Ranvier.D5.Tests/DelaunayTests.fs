module Ranvier.D5.Tests.DelaunayTests

open System
open Expecto
open Ranvier.D5

let private p x y=Point.create x y
let private verify checkAll mesh =
    let points,triangles,halfedges=Delaunay.sites mesh,Delaunay.triangles mesh,Delaunay.halfedges mesh
    Expect.equal triangles.Length halfedges.Length "one opposite per edge"
    let next i=if i%3=2 then i-2 else i+1
    for i in 0..3..triangles.Length-1 do
        let a,b,c=points[triangles[i]],points[triangles[i+1]],points[triangles[i+2]]
        Expect.equal (Predicates.orient2d a b c) 1 "strictly CCW"
        if checkAll then
            for point in points do Expect.isLessThanOrEqual (Predicates.incircle a b c point) 0 "empty circumcircle"
    for i in 0..halfedges.Length-1 do
        let other=halfedges[i]
        if other>=0 then
            Expect.equal halfedges[other] i "symmetric halfedge"
            Expect.equal triangles[i] triangles[next other] "reversed endpoints"
    for i in 0..points.Length-1 do
        for j in Delaunay.neighbors i mesh do
            Expect.contains (Delaunay.neighbors j mesh) i "symmetric adjacency"

let tests=testList "Delaunay" [
    testCase "empty singleton and duplicate identities" <| fun _ ->
        let empty=Delaunay.build []
        Expect.isEmpty (Delaunay.triangles empty) "empty"
        Expect.equal (Delaunay.nearest (p 0. 0.) empty) None "empty nearest"
        let mesh=Delaunay.build [p 1. 2.;p 1. 2.;p 3. 4.]
        Expect.equal (Delaunay.sites mesh) [|p 1. 2.;p 3. 4.|] "exact deduplication"
        Expect.equal (Delaunay.inputIndices 0 mesh) [|0;1|] "all original identities"
        Expect.equal (Delaunay.nearest (p 1. 2.) mesh) (Some 0) "original first input index"
        Expect.equal (Delaunay.neighbors 0 mesh) [|1|] "collinear chain"
        let copy=Delaunay.sites mesh
        copy[0]<-p 99. 99.
        Expect.equal ((Delaunay.sites mesh)[0]) (p 1. 2.) "defensive copy"
    testCase "cocircular choice is deterministic and topology valid" <| fun _ ->
        let points=[p 0. 0.;p 1. 0.;p 1. 1.;p 0. 1.]
        let mesh=Delaunay.build points
        Expect.equal (Delaunay.triangles mesh).Length 6 "two triangles"
        Expect.equal (Delaunay.triangles mesh) (Delaunay.build points |> Delaunay.triangles) "deterministic tie"
        verify true mesh
    testCase "random sites have empty circumcircles" <| fun _ ->
        let random=Random 7821
        let points=[for _ in 1..200 -> p (random.NextDouble()*100.) (random.NextDouble()*100.)]
        let mesh=Delaunay.build points
        verify true mesh
        Expect.equal (Delaunay.triangles mesh |> Set.ofArray |> Set.count) 200 "no sites silently dropped"
    testCase "mixed exponents retain exact topology" <| fun _ ->
        for points in [[p 0. 0.;p 1e-300 0.;p 0. 1e-300;p 1e300 1e300];
                       [p -1e308 0.;p 1e308 0.;p 0. 1e308;p 0. -1e308;p 0. 0.];
                       [p 0. 0.;p Double.Epsilon 0.;p 0. Double.Epsilon]] do
            let mesh=Delaunay.build points
            verify true mesh
            Expect.equal (Delaunay.triangles mesh |> Set.ofArray |> Set.count) points.Length "all distinct sites retained"
    testCase "collinear sites form an ordered chain without jitter" <| fun _ ->
        let mesh=Delaunay.build [p 3. 3.;p 0. 0.;p 2. 2.;p 1. 1.]
        Expect.isEmpty (Delaunay.triangles mesh) "no invented area"
        Expect.equal (Delaunay.hull mesh) [|1;3;2;0|] "lexicographic chain"
        Expect.equal (Delaunay.neighbors 2 mesh) [|0;3|] "only adjacent sites"
    testCase "100k grid construction retains all sites" <| fun _ ->
        let mesh=Delaunay.build [for i in 0..99999 -> p (float(i%1000)) (float(i/1000))]
        Expect.equal (Delaunay.triangles mesh).Length (197802*3) "grid Euler count"
        Expect.equal (Delaunay.triangles mesh |> Set.ofArray |> Set.count) 100000 "every boundary and interior site retained"
        verify false mesh
]

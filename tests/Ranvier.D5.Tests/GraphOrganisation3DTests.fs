module Ranvier.D5.Tests.GraphOrganisation3DTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D

let private ok =
    function
    | Ok v -> v
    | Error errs -> failwithf "Unexpected error: %A" errs

let private cube = Bounds3.create -200. -200. -200. 400. 400. 400.

let private boxVolume3D (b: Bounds3) =
    Bounds3.sizeX b * Bounds3.sizeY b * Bounds3.sizeZ b

let private boxContainsBox3D (tol: float) (outer: Bounds3) (inner: Bounds3) =
    Bounds3.minX inner >= Bounds3.minX outer - tol
    && Bounds3.minY inner >= Bounds3.minY outer - tol
    && Bounds3.minZ inner >= Bounds3.minZ outer - tol
    && Bounds3.maxX inner <= Bounds3.maxX outer + tol
    && Bounds3.maxY inner <= Bounds3.maxY outer + tol
    && Bounds3.maxZ inner <= Bounds3.maxZ outer + tol

let private boxesStrictlyOverlap3D (tol: float) (a: Bounds3) (b: Bounds3) =
    Bounds3.maxX a > Bounds3.minX b + tol
    && Bounds3.maxX b > Bounds3.minX a + tol
    && Bounds3.maxY a > Bounds3.minY b + tol
    && Bounds3.maxY b > Bounds3.minY a + tol
    && Bounds3.maxZ a > Bounds3.minZ b + tol
    && Bounds3.maxZ b > Bounds3.minZ a + tol

let private pointInBounds3Tol (tol: float) (b: Bounds3) (p: Point3) =
    p.X >= Bounds3.minX b - tol
    && p.X <= Bounds3.maxX b + tol
    && p.Y >= Bounds3.minY b - tol
    && p.Y <= Bounds3.maxY b + tol
    && p.Z >= Bounds3.minZ b - tol
    && p.Z <= Bounds3.maxZ b + tol

let tests =
    testList "GraphOrganisation3D" [
        testCase "Treemap3D (CubeSliceDice/CubeSquarified/CubeHilbert/StepTree) and CodeCity satisfy volumetric, bounds and height-monotonic invariants" <| fun _ ->
            let hTree =
                Hierarchy.node "root" 0.
                    [ Hierarchy.node "pkgA" 0.
                        [ Hierarchy.node "a1" 2. []
                          Hierarchy.node "a2" 4. []
                          Hierarchy.node "a3" 6. []
                          Hierarchy.node "a4" 8. [] ]
                      Hierarchy.node "pkgB" 0.
                        [ Hierarchy.node "b1" 3. []
                          Hierarchy.node "b2" 5. []
                          Hierarchy.node "b3" 7. []
                          Hierarchy.node "b4" 9. [] ] ]
            let rootVol = boxVolume3D cube

            for tiling in [ Treemap3DTiling.CubeSliceDice; Treemap3DTiling.CubeSquarified; Treemap3DTiling.CubeHilbert ] do
                let cfg = { Treemap3DConfig.create cube with Tiling = tiling; Padding = 0.; ExplodeFactor = 0. }
                let cuboids = Treemap3D.layout cfg hTree |> ok
                Expect.equal cuboids.Length 11 (sprintf "%A cuboid count" tiling)
                for c in cuboids do
                    Expect.isTrue (boxContainsBox3D 1e-5 cube c.Bounds) (sprintf "%A cuboid inside root Bounds3" tiling)
                let leaves = cuboids |> List.filter (fun (c: HierarchyCuboid<string>) -> c.IsLeaf) |> List.toArray
                Expect.equal leaves.Length 8 (sprintf "%A leaf count" tiling)
                let sumVol = leaves |> Array.sumBy (fun c -> boxVolume3D c.Bounds)
                Expect.isTrue (abs (sumVol - rootVol) / rootVol <= 1e-6) (sprintf "%A conserves root volume" tiling)
                for i in 0 .. leaves.Length - 2 do
                    for j in i + 1 .. leaves.Length - 1 do
                        Expect.isFalse (boxesStrictlyOverlap3D 1e-5 (leaves[i].Bounds) (leaves[j].Bounds)) (sprintf "%A disjoint leaves" tiling)

                let diag3 = Treemap3D.toDiagramLayout cuboids
                Expect.equal diag3.Nodes.Length 8 "Treemap3D.toDiagramLayout 8 leaf nodes"
                Expect.equal diag3.Groups.Length 3 "Treemap3D.toDiagramLayout 3 internal groups"

            // StepTree 2.5D tiered platforms
            let stepCfg = { Treemap3DConfig.create cube with Tiling = Treemap3DTiling.StepTree; StepHeight = 20. }
            let stepCuboids = Treemap3D.layout stepCfg hTree |> ok
            for c in stepCuboids do
                let expectedY = Bounds3.minY cube + float c.Depth * 20.
                Expect.isTrue (abs (Bounds3.minY c.Bounds - expectedY) <= 1e-5) "StepTree platform elevation = minY + depth * StepHeight"
                Expect.isTrue
                    (Bounds3.minX c.Bounds >= Bounds3.minX cube - 1e-5
                     && Bounds3.maxX c.Bounds <= Bounds3.maxX cube + 1e-5
                     && Bounds3.minZ c.Bounds >= Bounds3.minZ cube - 1e-5
                     && Bounds3.maxZ c.Bounds <= Bounds3.maxZ cube + 1e-5)
                    "StepTree X-Z footprint inside root footprint"

            // CodeCity: building +Y heights scale monotonically with height metric
            let heightMap =
                Map.ofList
                    [ "a1", 10.; "a2", 25.; "a3", 55.; "a4", 100.
                      "b1", 15.; "b2", 40.; "b3", 70.; "b4", 85. ]
            let footprint = Bounds.create -150. -150. 300. 300.
            let cityCfg = CodeCityConfig.create footprint 120. (fun id -> Map.tryFind id heightMap |> Option.defaultValue 0.)
            let buildings = CodeCity.layout cityCfg hTree |> ok
            Expect.equal buildings.Length 11 "CodeCity total items"
            let leafBuildings =
                buildings
                |> List.filter (fun (b: CityBuilding<string>) -> not b.IsDistrict)
                |> List.sortBy (fun (b: CityBuilding<string>) -> b.HeightMetric)
            Expect.equal leafBuildings.Length 8 "CodeCity 8 leaf buildings"
            for b0, b1 in List.pairwise leafBuildings do
                Expect.isTrue
                    (Bounds3.sizeY b1.Bounds > Bounds3.sizeY b0.Bounds)
                    (sprintf "CodeCity height scales monotonically (%s: %f -> %s: %f)" b0.Id (Bounds3.sizeY b0.Bounds) b1.Id (Bounds3.sizeY b1.Bounds))
            let tallest = List.last leafBuildings
            Expect.isTrue (abs (Bounds3.sizeY tallest.Bounds - 120.) <= 1e-6) "tallest building reaches MaxBuildingHeight"

            let cityDiag = CodeCity.toDiagramLayout buildings
            Expect.equal cityDiag.Nodes.Length 8 "CodeCity.toDiagramLayout 8 building nodes"
            Expect.equal cityDiag.Groups.Length 3 "CodeCity.toDiagramLayout 3 district plinths"
            let cam = Camera3D.fitBounds 0.6 0.4 cityDiag.Bounds
            let scene = Scene3D.render (Scene3DTheme.create 640. 480. "CodeCity 3D") cam cityDiag
            Expect.isNonEmpty (Scene.nodes scene) "Scene3D renders CodeCity layout"

        testCase "Pack3D satisfies non-overlapping sibling spheres and parent containment and VoronoiTreemap3D conserves volume on 1-level and 2-level trees" <| fun _ ->
            let packTree =
                Hierarchy.node "root" 0.
                    [ Hierarchy.node "g1" 0.
                        [ for i in 1 .. 5 -> Hierarchy.node (sprintf "s%d" i) (float i * 2.) [] ]
                      Hierarchy.node "g2" 0.
                        [ for i in 1 .. 4 -> Hierarchy.node (sprintf "t%d" i) (float (5 - i) * 3.) [] ]
                      Hierarchy.node "solo" 6. [] ]
            let spheres = Pack3D.layout (Pack3DConfig.create cube) packTree |> ok
            Expect.equal spheres.Length 13 "Pack3D sphere count"
            let byId = spheres |> List.map (fun (s: PackedSphere3D<string>) -> s.Id, s) |> Map.ofList

            // Children fit inside parent sphere and siblings do not overlap beyond 1e-5
            for s in spheres do
                match s.Parent with
                | None ->
                    Expect.isTrue (boxContainsBox3D 1e-5 cube (Bounds3.create (s.Center.X - s.Radius) (s.Center.Y - s.Radius) (s.Center.Z - s.Radius) (2. * s.Radius) (2. * s.Radius) (2. * s.Radius))) "root sphere inscribed in Bounds3"
                | Some pid ->
                    let parent = byId[pid]
                    let distToCenter = Point3.distance s.Center parent.Center
                    Expect.isTrue
                        (distToCenter + s.Radius <= parent.Radius + 1e-5)
                        (sprintf "child sphere %s fits inside parent %s (%f + %f <= %f)" s.Id pid distToCenter s.Radius parent.Radius)

            let byParent =
                spheres
                |> List.choose (fun (s: PackedSphere3D<string>) -> s.Parent |> Option.map (fun pid -> pid, s))
                |> List.groupBy fst
            for pid, grp in byParent do
                let sibs = grp |> List.map snd |> List.toArray
                for i in 0 .. sibs.Length - 2 do
                    for j in i + 1 .. sibs.Length - 1 do
                        let d = Point3.distance (sibs[i].Center) (sibs[j].Center)
                        Expect.isTrue
                            (d >= sibs[i].Radius + sibs[j].Radius - 1e-5)
                            (sprintf "siblings %s and %s under %s do not overlap (%f >= %f)" (sibs[i].Id) (sibs[j].Id) pid d (sibs[i].Radius + sibs[j].Radius))

            let packDiag = Pack3D.toDiagramLayout spheres
            Expect.equal packDiag.Nodes.Length 10 "Pack3D.toDiagramLayout 10 leaf spheres"
            Expect.equal packDiag.Groups.Length 3 "Pack3D.toDiagramLayout 3 enclosing spheres"

            // VoronoiTreemap3D on 1-level and 2-level trees inside Bounds3
            let vtBox = Bounds3.create 0. 0. 0. 20. 20. 20.
            let vtVol = boxVolume3D vtBox

            let tree1Level =
                Hierarchy.node "r1" 0.
                    [ Hierarchy.node "l1" 1. []
                      Hierarchy.node "l2" 2. []
                      Hierarchy.node "l3" 3. []
                      Hierarchy.node "l4" 4. [] ]
            let cells1 = VoronoiTreemap3D.layout 80 5e-3 vtBox tree1Level |> ok
            Expect.equal cells1.Length 5 "1-level VoronoiTreemap3D cell count"
            let leaves1 = cells1 |> List.filter (fun (c: VoronoiTreemap3DCell<string>) -> c.Depth = 1)
            let vol1Sum = leaves1 |> List.sumBy (fun (c: VoronoiTreemap3DCell<string>) -> c.Volume)
            Expect.isTrue (abs (vol1Sum - vtVol) <= 1e-2) "1-level VoronoiTreemap3D conserves root volume"
            for c in cells1 do
                Expect.isTrue (c.Vertices.Length >= 4 && c.Faces.Length >= 4) "3D cell has valid polyhedron"
                for v in c.Vertices do
                    Expect.isTrue (pointInBounds3Tol 1e-4 vtBox v) "1-level cell vertex inside root Bounds3"

            let tree2Level =
                Hierarchy.node "r2" 0.
                    [ Hierarchy.node "brA" 0.
                        [ Hierarchy.node "a1" 2. []
                          Hierarchy.node "a2" 3. []
                          Hierarchy.node "a3" 5. [] ]
                      Hierarchy.node "brB" 0.
                        [ Hierarchy.node "b1" 4. []
                          Hierarchy.node "b2" 6. [] ] ]
            let cells2 = VoronoiTreemap3D.layout 80 5e-3 vtBox tree2Level |> ok
            Expect.equal cells2.Length 8 "2-level VoronoiTreemap3D cell count"
            let leaves2 = cells2 |> List.filter (fun (c: VoronoiTreemap3DCell<string>) -> c.Depth = 2)
            let vol2Sum = leaves2 |> List.sumBy (fun (c: VoronoiTreemap3DCell<string>) -> c.Volume)
            Expect.isTrue (abs (vol2Sum - vtVol) <= 1e-2) "2-level VoronoiTreemap3D leaves conserve root volume"
            for c in cells2 do
                Expect.isTrue (c.Vertices.Length >= 4 && c.Faces.Length >= 4) "2-level 3D cell has valid polyhedron"
                for v in c.Vertices do
                    Expect.isTrue (pointInBounds3Tol 1e-4 vtBox v) "2-level cell vertex inside root Bounds3"

        testCase "SphericalFibonacci3D, SphericalStress3D, HierarchicalBundle3D and KdeBundle3D preserve sphere radii, exact anchors and Scene3D rendering" <| fun _ ->
            let groups = [ for g in 0 .. 3 -> DiagramGroup.create (sprintf "sg%d" g) ]
            let nodes =
                [ for i in 0 .. 19 ->
                    DiagramNode.sized (sprintf "sn%02d" i) 18. 18.
                    |> DiagramNode.inGroup (sprintf "sg%d" (i / 5)) ]
            let edges =
                [ for i in 0 .. 19 do
                    yield Association.create (sprintf "ring_%d" i) (sprintf "sn%02d" i) (sprintf "sn%02d" ((i + 1) % 20))
                    if i % 2 = 0 then
                        yield Association.create (sprintf "chord_%d" i) (sprintf "sn%02d" i) (sprintf "sn%02d" ((i + 7) % 20)) ]
            let graph = DiagramGraph.create nodes groups edges |> ok

            let sphCenter = Point3.create 12. -18. 24.
            let radius = 240.
            let arcSegs = 12

            let fibLayout = SphericalFibonacci3D.layout sphCenter radius arcSegs graph |> ok
            let stressCfg = { SphericalLayoutConfig.create sphCenter radius with Iterations = 40; ArcSegments = arcSegs; Seed = 99UL }
            let stressLayout = SphericalStress3D.layout stressCfg graph |> ok

            for label, lay in [ "SphericalFibonacci3D", fibLayout; "SphericalStress3D", stressLayout ] do
                Expect.equal lay.Nodes.Length 20 (label + " node count")
                Expect.equal lay.Associations.Length edges.Length (label + " association count")
                for n in lay.Nodes do
                    let d = Point3.distance sphCenter n.Center
                    Expect.isTrue (abs (d - radius) <= 1e-5) (sprintf "%s node %s on sphere (|d - R| = %g)" label n.Id (abs (d - radius)))
                for a in lay.Associations do
                    Expect.equal a.Points.Length (arcSegs + 1) (label + " Point3Chain length")
                    for k in 0 .. a.Points.Length - 1 do
                        let dPt = Point3.distance sphCenter (a.Points[k])
                        Expect.isTrue (abs (dPt - radius) <= 1e-5) (sprintf "%s arc point on sphere (|d - R| = %g)" label (abs (dPt - radius)))
                let cam = Camera3D.orbit sphCenter 0.55 0.35 720.
                let scene = Scene3D.render { Scene3DTheme.create 800. 600. label with ReferenceSphereRadius = Some radius } cam lay
                Expect.isNonEmpty (Scene.nodes scene) (label + " Scene3D.render non-empty")

            // HierarchicalBundle3D & KdeBundle3D preserve exact start/end node centers and render cleanly
            let hTree =
                Hierarchy.node "sphRoot" 0.
                    [ for g in 0 .. 3 ->
                        Hierarchy.node (sprintf "sg%d" g) 0.
                            [ for k in 0 .. 4 -> Hierarchy.node (sprintf "sn%02d" (g * 5 + k)) 1. [] ] ]
            let nodeCenterMap = fibLayout.Nodes |> List.map (fun (n: PositionedNode3D) -> n.Id, n.Center) |> Map.ofList

            let heb3 = HierarchicalBundle3D.bundle 0.85 16 hTree fibLayout |> ok
            Expect.equal heb3.Associations.Length edges.Length "HierarchicalBundle3D association count"
            for a in heb3.Associations do
                Expect.equal a.Points.Length 17 "HierarchicalBundle3D 17 chain points"
                let srcC = nodeCenterMap[a.Source.NodeId]
                let dstC = nodeCenterMap[a.Target.NodeId]
                Expect.isTrue (Point3.distance (a.Points[0]) srcC <= 1e-6) "HierarchicalBundle3D preserves exact start node center"
                Expect.isTrue (Point3.distance (a.Points[a.Points.Length - 1]) dstC <= 1e-6) "HierarchicalBundle3D preserves exact end node center"

            let kde3Cfg = { KdeBundle3DConfig.defaultValue with GridSize = 32; Iterations = 5; SamplePointsPerEdge = 16 }
            let kde3 = KdeBundle3D.bundle kde3Cfg fibLayout |> ok
            Expect.equal kde3.Associations.Length edges.Length "KdeBundle3D association count"
            for a in kde3.Associations do
                Expect.equal a.Points.Length 16 "KdeBundle3D 16 chain points"
                let srcC = nodeCenterMap[a.Source.NodeId]
                let dstC = nodeCenterMap[a.Target.NodeId]
                Expect.isTrue (Point3.distance (a.Points[0]) srcC <= 1e-6) "KdeBundle3D preserves exact start node center"
                Expect.isTrue (Point3.distance (a.Points[a.Points.Length - 1]) dstC <= 1e-6) "KdeBundle3D preserves exact end node center"

            let cam3 = Camera3D.orbit sphCenter 0.7 0.25 720.
            let hebScene = Scene3D.render (Scene3DTheme.create 800. 600. "HEB3D") cam3 heb3
            let kdeScene = Scene3D.render (Scene3DTheme.create 800. 600. "KDE3D") cam3 kde3
            Expect.isNonEmpty (Scene.nodes hebScene) "Scene3D renders HierarchicalBundle3D"
            Expect.isNonEmpty (Scene.nodes kdeScene) "Scene3D renders KdeBundle3D"

            let cancelled = CancellationToken(true)
            Expect.throwsT<OperationCanceledException>
                (fun () -> SphericalStress3D.layoutWithCancellation cancelled stressCfg graph |> ignore)
                "SphericalStress3D honors cancellation"
            Expect.throwsT<OperationCanceledException>
                (fun () -> KdeBundle3D.bundleWithCancellation cancelled kde3Cfg fibLayout |> ignore)
                "KdeBundle3D honors cancellation"

        testCase "Umap3D (with toDiagramLayout), Tsne3D, Isomap3D and Sammon3D separate 3 clusters in R^8 inside Bounds3" <| fun _ ->
            let centers8D =
                [| [| 12.; -10.; 8.; -6.; 10.; -8.; 6.; -4. |]
                   [| -11.; 13.; -9.; 7.; -8.; 11.; -5.; 9. |]
                   [| 2.; 3.; -14.; -13.; 14.; 12.; -11.; 10. |] |]
            let rows90 =
                [ for c in 0 .. 2 do
                    for i in 0 .. 29 ->
                        let idx = c * 30 + i
                        let feats =
                            [ for d in 0 .. 7 ->
                                centers8D[c][d]
                                + 0.65 * sin (float (idx * 7 + d * 13) * 1.37)
                                + 0.45 * cos (float (idx * 11 + d * 5) * 0.91) ]
                        sprintf "p_%d_%02d" c i, feats ]
            let ds90 = FeatureDataset.ofRows rows90 |> ok
            let clusterOf (id: string) = int (id.Split('_')[1])

            let checkEmbedding3D (label: string) (pts: EmbeddedPoint3D<string> list) =
                Expect.equal pts.Length 90 (label + " embedded count")
                for p in pts do
                    Expect.isTrue (Bounds3.contains p.Position cube) (label + " point inside Bounds3")
                let arr = List.toArray pts
                let mutable intraSum, intraCnt = 0., 0
                let mutable interSum, interCnt = 0., 0
                for i in 0 .. arr.Length - 2 do
                    let ci = clusterOf (arr[i].Id)
                    for j in i + 1 .. arr.Length - 1 do
                        let cj = clusterOf (arr[j].Id)
                        let d = Point3.distance (arr[i].Position) (arr[j].Position)
                        if ci = cj then
                            intraSum <- intraSum + d
                            intraCnt <- intraCnt + 1
                        else
                            interSum <- interSum + d
                            interCnt <- interCnt + 1
                let meanIntra = intraSum / float intraCnt
                let meanInter = interSum / float interCnt
                Expect.isTrue (meanIntra < meanInter * 0.65) (sprintf "%s separates 3D blobs (intra %f < inter %f)" label meanIntra meanInter)

            let umap3Cfg = { Umap3DConfig.create cube with Neighbors = 12; Epochs = 120; Seed = 55u }
            let umap3A = Umap3D.embed umap3Cfg ds90 |> ok
            let umap3B = Umap3D.embed umap3Cfg ds90 |> ok
            checkEmbedding3D "Umap3D" umap3A
            Expect.equal umap3A umap3B "Umap3D deterministic seed"

            let knnLinks = NnDescent.buildGraph DistanceMetric.Euclidean 6 55u ds90 |> ok
            let umapDiag = Umap3D.toDiagramLayout 4.0 umap3A knnLinks
            Expect.equal umapDiag.Nodes.Length 90 "Umap3D.toDiagramLayout 90 nodes"
            Expect.isNonEmpty umapDiag.Associations "Umap3D.toDiagramLayout k-NN skeleton associations"
            let cam = Camera3D.fitBounds 0.5 0.35 umapDiag.Bounds
            let scene = Scene3D.render (Scene3DTheme.create 640. 480. "UMAP 3D") cam umapDiag
            Expect.isNonEmpty (Scene.nodes scene) "Scene3D renders Umap3D.toDiagramLayout"

            let tsne3Cfg = { Tsne3DConfig.create cube with Perplexity = 18.; Iterations = 180; Seed = 55u }
            let tsne3A = Tsne3D.embed tsne3Cfg ds90 |> ok
            let tsne3B = Tsne3D.embed tsne3Cfg ds90 |> ok
            checkEmbedding3D "Tsne3D" tsne3A
            Expect.equal tsne3A tsne3B "Tsne3D deterministic seed"

            let iso3Out = Isomap3D.embed cube DistanceMetric.Euclidean 35 ds90 |> ok
            checkEmbedding3D "Isomap3D" iso3Out

            let sammon3A = Sammon3D.embed cube DistanceMetric.Euclidean 120 55u ds90 |> ok
            let sammon3B = Sammon3D.embed cube DistanceMetric.Euclidean 120 55u ds90 |> ok
            checkEmbedding3D "Sammon3D" sammon3A
            Expect.equal sammon3A sammon3B "Sammon3D deterministic seed"

            let cancelled = CancellationToken(true)
            Expect.throwsT<OperationCanceledException>
                (fun () -> Umap3D.embedWithCancellation cancelled umap3Cfg ds90 |> ignore)
                "Umap3D honors cancellation"
            Expect.throwsT<OperationCanceledException>
                (fun () -> Tsne3D.embedWithCancellation cancelled tsne3Cfg ds90 |> ignore)
                "Tsne3D honors cancellation"
    ]

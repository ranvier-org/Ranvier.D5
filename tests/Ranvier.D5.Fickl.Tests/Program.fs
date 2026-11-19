module Ranvier.D5.Fickl.Tests.Program

open System
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.Geo
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D

let private check condition message =
    if not condition then
        failwith ("Fickl JS check failed: " + message)

let private ok label = function
    | Ok value -> value
    | Error errs -> failwithf "Fickl JS %s returned Error: %A" label errs

[<EntryPoint>]
let main _ =
    let vp2D = Bounds.create 0. 0. 800. 600.
    let vp3D = Bounds3.create -250. -250. -250. 500. 500. 500.

    // 1. Compound DiagramGraph + Sugiyama (Orthogonal & Spline) + DiagramScene
    let groups =
        [ { DiagramGroup.create "gOuter" with Padding = GroupPadding.titled 26. 12. }
          { DiagramGroup.create "gInner" with Padding = GroupPadding.titled 22. 10. } |> DiagramGroup.inGroup "gOuter" ]
    let nodes =
        [ DiagramNode.sized "start" 20. 20. |> DiagramNode.inGroup "gOuter" |> DiagramNode.withKind NodeKind.InitialState
          DiagramNode.sized "n1" 84. 32.
          |> DiagramNode.inGroup "gInner"
          |> DiagramNode.withPorts [ DiagramPort.create "in" PortSide.North; DiagramPort.create "out" PortSide.South ]
          DiagramNode.sized "n2" 84. 32. |> DiagramNode.inGroup "gInner"
          DiagramNode.sized "n3" 84. 32. ]
    let assocs =
        [ { Association.create "e0" "start" "n1" with Target = Endpoint.port "n1" "in"; Kind = AssociationKind.Transition }
          { Association.betweenPorts "e1" (Endpoint.port "n1" "out") (Endpoint.node "n2") AssociationKind.Composition with
              CenterLabel = Some { Width = 32.; Height = 14. } }
          Association.create "e2" "n2" "n3" ]
    let diagram =
        DiagramGraph.createWithConstraints
            nodes
            groups
            assocs
            [ LayoutConstraint.AlignHorizontal [ "n2"; "n3" ] ]
        |> ok "DiagramGraph.createWithConstraints"

    let sugOrtho =
        Sugiyama.layout { SugiyamaConfig.create vp2D with Routing = SugiyamaRouting.Orthogonal } diagram
        |> ok "Sugiyama.Orthogonal"
    check (sugOrtho.Nodes.Length = 4 && sugOrtho.Groups.Length = 2 && sugOrtho.Associations.Length = 3) "Sugiyama counts"
    check (sugOrtho.Associations |> List.forall (fun a -> a.Waypoints.Length >= 2)) "Sugiyama waypoints"
    let rendered2D = DiagramScene.render (DiagramTheme.create 800. 600. "JS Scene 2D") sugOrtho
    check (not (List.isEmpty (Scene.nodes rendered2D))) "DiagramScene.render nodes"

    // 2. IPSep-CoLa, fCoSE, PivotMDS, Stress2D, ModularityForce2D, RadialTree, BoxPack
    let colaOut = Cola.layout { ColaConfig.create vp2D with Iterations = 25; AvoidOverlaps = true } diagram |> ok "Cola"
    let n2Cola = colaOut.Nodes |> List.find (fun n -> n.Id = "n2")
    let n3Cola = colaOut.Nodes |> List.find (fun n -> n.Id = "n3")
    check (abs (n2Cola.Center.Y - n3Cola.Center.Y) < 1e-2) "Cola horizontal alignment rail in JS"

    let fcoseOut = FCose.layout { FCoseConfig.create vp2D with Iterations = 30 } diagram |> ok "FCose"
    check (fcoseOut.Nodes.Length = 4 && fcoseOut.Groups.Length = 2) "FCose counts"

    let meshNodes = [ for i in 0 .. 29 -> DiagramNode.sized (sprintf "m%d" i) 30. 24. ]
    let meshEdges =
        [ for i in 0 .. 29 do
            yield Association.create (sprintf "r%d" i) (sprintf "m%d" i) (sprintf "m%d" ((i + 1) % 30))
            yield Association.create (sprintf "c%d" i) (sprintf "m%d" i) (sprintf "m%d" ((i + 5) % 30)) ]
    let mesh = DiagramGraph.simple meshNodes meshEdges |> ok "mesh"

    let pmdsOut = PivotMds.layout { PivotMdsConfig.create vp2D with PivotCount = 12 } mesh |> ok "PivotMds"
    check (pmdsOut.Nodes.Length = 30) "PivotMds count"

    let stress2DOut = Stress2D.layout { Stress2DConfig.create vp2D with Iterations = 15 } mesh |> ok "Stress2D"
    check (stress2DOut.Nodes.Length = 30) "Stress2D count"

    let modOut = ModularityForce2D.layout { ModularityForceConfig.create vp2D with Iterations = 25; Model = EnergyModel.LinLog } mesh |> ok "ModularityForce2D"
    check (modOut.Nodes.Length = 30) "ModularityForce2D count"

    let treeNodes = [ for i in 0 .. 30 -> DiagramNode.create (sprintf "t%d" i) ]
    let treeEdges = [ for i in 1 .. 30 -> Association.create (sprintf "te%d" i) (sprintf "t%d" ((i - 1) / 3)) (sprintf "t%d" i) ]
    let treeGraph = DiagramGraph.simple treeNodes (Association.create "x0" "t4" "t19" :: treeEdges) |> ok "treeGraph"

    let radialOut = RadialTree.layout { RadialTreeConfig.create vp2D with Mode = RadialMode.BidirectionalMindmap; RootId = Some "t0" } treeGraph |> ok "RadialTree"
    check (radialOut.Nodes.Length = 31) "RadialTree count"

    let boxOut = BoxPack.layout (BoxPackConfig.create vp2D) diagram |> ok "BoxPack"
    check (boxOut.Nodes.Length = 4) "BoxPack count"

    // 3. Octree + Hyperbolic3D + Munzner H3 + CAIDA Walrus + Force3D + Stress3D + ConeTree3D + Layered3D + Scene3D
    let pts3 = [| for i in 0 .. 63 -> i, Point3.create (sin (float i) * 80.) (cos (float i * 1.7) * 80.) (float (i - 32) * 2.) |]
    let oct = Octree.build snd (fun _ -> 1.) pts3
    check (Octree.count oct = 64) "Octree.count"
    let nearest = Octree.nearest (snd pts3[17]) infinity oct
    check (nearest |> Option.exists (fun (idx, _) -> idx = 17)) "Octree.nearest exact match"

    let mp = Hyperbolic3D.ofSpacelike 0.8 -0.5 1.1
    let ballPt, atten = Hyperbolic3D.toBall HyperbolicModel.Poincare mp
    check (atten > 0. && atten <= 1.) "Hyperbolic3D attenuation in JS"
    let roundTrip = Hyperbolic3D.ofBall HyperbolicModel.Poincare ballPt |> ok "Hyperbolic3D.ofBall"
    check (Hyperbolic3D.distance mp roundTrip < 1e-6) "Hyperbolic3D Poincare round-trip in JS"

    let h3State = H3.buildState { H3Config.defaultValue with RootId = Some "t0"; Model = HyperbolicModel.Klein } treeGraph |> ok "H3.buildState"
    let h3Focused = H3.focusNode "t12" h3State |> ok "H3.focusNode" |> H3.project
    let t12 = h3Focused.Nodes |> List.find (fun n -> n.Id = "t12")
    check (Point3.distance t12.Center (Point3.create 0. 0. 0.) < 1e-5) "H3 Lorentz boost moves focus node to origin in JS"

    let walrusOut = Walrus.layout { WalrusConfig.defaultValue with RootId = Some "t0" } treeGraph |> ok "Walrus.layout"
    check (walrusOut.Nodes.Length = 31 && walrusOut.Associations.Length = 31) "Walrus counts"
    check ((walrusOut.Associations |> List.filter (fun a -> a.IsBackbone)).Length = 30) "Walrus backbone count"

    let force3DOut = Force3D.layout { Force3DConfig.create vp3D with Iterations = 20 } mesh |> ok "Force3D"
    check (force3DOut.Nodes.Length = 30) "Force3D count"

    let stress3DOut = Stress3D.layout { Stress3DConfig.create vp3D with Iterations = 15 } mesh |> ok "Stress3D"
    check (stress3DOut.Nodes.Length = 30) "Stress3D count"

    let coneOut = ConeTree3D.layout { ConeTree3DConfig.create vp3D with Mode = ConeTreeMode.SphericalBalloon; RootId = Some "t0" } treeGraph |> ok "ConeTree3D"
    check (coneOut.Nodes.Length = 31) "ConeTree3D count"

    let lay3DOut = Layered3D.layout (Layered3DConfig.create vp3D) diagram |> ok "Layered3D"
    check (lay3DOut.Nodes.Length = 4 && lay3DOut.Groups.Length = 2) "Layered3D counts"

    let cam3 = Camera3D.orbit (Point3.create 0. 0. 0.) 0.5 0.3 650.
    let rendered3D = Scene3D.render { Scene3DTheme.create 800. 600. "JS Scene 3D" with ReferenceSphereRadius = Some 300. } cam3 h3Focused
    check (not (List.isEmpty (Scene.nodes rendered3D))) "Scene3D.render nodes"

    // 4. Wave 2: Space-filling curves + RTree2D/RTree3D + Bvh3D.raycastClosest + Spatial3D
    check (Morton2D.decode (Morton2D.encode 123us 456us) = (123us, 456us)) "Morton2D round-trip in JS"
    check (Morton3D.decode (Morton3D.encode 17us 29us 511us) = (17us, 29us, 511us)) "Morton3D round-trip in JS"
    check (Hilbert2D.decodeOrder 4 (Hilbert2D.encodeOrder 4 9 13) = (9, 13)) "Hilbert2D round-trip in JS"
    check (Moore2D.decodeOrder 4 (Moore2D.encodeOrder 4 6 11) = (6, 11)) "Moore2D round-trip in JS"
    let m0x, m0y = Moore2D.decodeOrder 3 0u
    let mEndX, mEndY = Moore2D.decodeOrder 3 63u
    check (abs (mEndX - m0x) + abs (mEndY - m0y) = 1) "Moore2D closed loop in JS"
    check (Hilbert3D.decodeOrder 3 (Hilbert3D.encodeOrder 3 3 5 6) = (3, 5, 6)) "Hilbert3D round-trip in JS"

    let boxes2 = [| for i in 0 .. 31 -> i, Bounds.create (float (i * 19 % 700)) (float (i * 23 % 500)) 24. 18. |]
    let rt2 = RTree2D.build snd boxes2
    check (RTree2D.count rt2 = 32) "RTree2D.count in JS"
    check (not (List.isEmpty (RTree2D.search (Bounds.create 0. 0. 400. 400.) rt2))) "RTree2D.search in JS"
    check ((RTree2D.nearest (Point.create 100. 100.) infinity rt2).IsSome) "RTree2D.nearest in JS"

    let boxes3 = [| for i in 0 .. 31 -> i, Bounds3.create (float (i * 10 - 150)) -15. -15. 18. 30. 30. |]
    let rt3 = RTree3D.build snd boxes3
    check (RTree3D.count rt3 = 32 && (RTree3D.nearest (Point3.create 0. 0. 0.) infinity rt3).IsSome) "RTree3D in JS"
    let bvh3 = Bvh3D.build snd boxes3
    let rayHit = Bvh3D.raycastClosest (Point3.create -250. 0. 0.) (Point3.create 1. 0. 0.) 1000. bvh3
    check (rayHit |> Option.exists (fun ((idx, _), _) -> idx = 0)) "Bvh3D.raycastClosest hits front box in JS"

    let sp3 = Spatial3D.create snd pts3
    check (Spatial3D.count sp3 = 64 && not (List.isEmpty (Spatial3D.kNearest 5 (Point3.create 0. 0. 0.) infinity sp3))) "Spatial3D in JS"

    // 5. Wave 2: Seriation + Dbscan2D + Dbscan3D
    let sNodes = [ "s3"; "s0"; "s2"; "s4"; "s1"; "s5" ]
    let sEdges = [ for i in 0 .. 4 -> sprintf "s%d" i, sprintf "s%d" (i + 1), 1. ]
    let rcmOrd = Seriation.reverseCuthillMcKee sNodes sEdges |> ok "Seriation.reverseCuthillMcKee"
    let fiedOrd = Seriation.spectralFiedler sNodes sEdges |> ok "Seriation.spectralFiedler"
    check (rcmOrd.Length = 6 && fiedOrd.Length = 6) "Seriation RCM & Fiedler counts in JS"
    let sPos = Map.ofList [ "s0", 0.; "s1", 2.; "s2", 4.; "s3", 50.; "s4", 52.; "s5", 54. ]
    let sDist a b = abs (sPos[a] - sPos[b])
    let dendro = Seriation.hierarchicalCluster Linkage.Ward sNodes sDist |> ok "Seriation.hierarchicalCluster" |> Option.get
    let oloOrd = Seriation.optimalLeafOrder dendro sDist
    check (oloOrd.Length = 6) "Seriation.optimalLeafOrder in JS"
    let biR, biC =
        Seriation.bicluster
            [ "r1"; "r2"; "r3"; "r4" ]
            [ "c1"; "c2"; "c3"; "c4" ]
            [ "r1", "c1", 4.; "r2", "c2", 4.; "r3", "c3", 4.; "r4", "c4", 4. ]
        |> ok "Seriation.bicluster"
    check (biR.Length = 4 && biC.Length = 4) "Seriation.bicluster in JS"

    let db2Pts =
        [ for k in 0 .. 5 -> sprintf "a%d" k, Point.create (float k * 1.5) 0.
          for k in 0 .. 5 -> sprintf "b%d" k, Point.create (100. + float k * 1.5) 0.
          yield "out2", Point.create 500. 500. ]
    let db2Res = Dbscan2D.cluster 4.0 3 fst snd db2Pts |> ok "Dbscan2D.cluster"
    check (db2Res |> List.exists (fun a -> a.Id = "out2" && a.ClusterId.IsNone)) "Dbscan2D noise in JS"

    let db3Pts =
        [ for k in 0 .. 5 -> sprintf "u%d" k, Point3.create (float k * 1.5) 0. 0.
          for k in 0 .. 5 -> sprintf "v%d" k, Point3.create (100. + float k * 1.5) 0. 0.
          yield "out3", Point3.create 0. 400. 0. ]
    let db3Res = Dbscan3D.cluster 4.0 3 fst snd db3Pts |> ok "Dbscan3D.cluster"
    check (db3Res |> List.exists (fun a -> a.Id = "out3" && a.ClusterId.IsNone)) "Dbscan3D noise in JS"

    // 6. Wave 2: PowerDiagram, VoronoiTreemap, Cvt2D, Voronoi3D, Cvt3D, TreemapTiling.Hilbert, GosperMap, UpSet/Euler, SphericalFibonacci/Voronoi
    let unitBox2 = Bounds.create 0. 0. 100. 100.
    let pwrCells =
        PowerDiagram.clipBounds
            unitBox2
            [ { PowerSite.Id = "p0"; Position = Point.create 25. 25.; Weight = 10. }
              { PowerSite.Id = "p1"; Position = Point.create 75. 25.; Weight = 20. }
              { PowerSite.Id = "p2"; Position = Point.create 50. 75.; Weight = 15. } ]
        |> ok "PowerDiagram.clipBounds"
    let pwrArea = pwrCells |> List.sumBy (fun (c: PowerCell<string>) -> c.Area)
    check (abs (pwrArea - 10000.) < 1e-3) "PowerDiagram area in JS"

    let hSample =
        Hierarchy.node "hRoot" 0.
            [ Hierarchy.node "br0" 0. [ Hierarchy.node "l0" 2. []; Hierarchy.node "l1" 3. []; Hierarchy.node "l2" 5. [] ]
              Hierarchy.node "br1" 0. [ Hierarchy.node "l3" 4. []; Hierarchy.node "l4" 6. [] ] ]
    let vt2Cells = VoronoiTreemap.layout { VoronoiTreemapConfig.create unitBox2 with MaxIterations = 40 } hSample |> ok "VoronoiTreemap.layout"
    check (vt2Cells.Length = 8) "VoronoiTreemap count in JS"

    let cvt2Cells, cvt2Pts =
        Cvt2D.relax CvtSolver.Lloyd 5 unitBox2 [ for i in 0 .. 8 -> Point.create (15. + float (i % 3) * 20.) (15. + float (i / 3) * 20.) ]
        |> ok "Cvt2D.relax"
    check (cvt2Cells.Length = 9 && cvt2Pts.Length = 9) "Cvt2D in JS"

    let unitBox3 = Bounds3.create 0. 0. 0. 10. 10. 10.
    let sites3Small =
        [ for i in 0 .. 7 ->
            Point3.create (2. + float (i % 2) * 6. + 0.2 * float i) (2. + float ((i / 2) % 2) * 6.) (2. + float (i / 4) * 6.) ]
    let vor3Cells = Voronoi3D.clipBounds unitBox3 sites3Small |> ok "Voronoi3D.clipBounds"
    let vor3Vol = vor3Cells |> List.sumBy (fun (c: PolyhedronCell3D) -> c.Volume)
    check (abs (vor3Vol - 1000.) < 1e-2) "Voronoi3D volume conservation in JS"

    let cvt3Cells, cvt3Pts = Cvt3D.relax 4 unitBox3 sites3Small |> ok "Cvt3D.relax"
    check (cvt3Cells.Length = 8 && cvt3Pts.Length = 8) "Cvt3D in JS"

    let hilbTm = Treemap.layout { TreemapConfig.create vp2D with Tiling = TreemapTiling.Hilbert } hSample |> ok "Treemap.Hilbert"
    check (hilbTm.Length = 8) "TreemapTiling.Hilbert in JS"

    let gosperOut = GosperMap.layout { GosperMapConfig.create vp2D with Order = 2; SmoothIterations = 1 } hSample |> ok "GosperMap.layout"
    check (gosperOut.Length = 8 && gosperOut |> List.forall (fun r -> r.Area > 0.)) "GosperMap in JS"

    let cFrame = ChartFrame.create 640. 420. "JS Set Charts"
    let upsetOut =
        Chart.upset (UpSetSpec.create cFrame) [ "S1", [ 1; 2; 3; 4 ]; "S2", [ 3; 4; 5 ]; "S3", [ 1; 4; 6 ] ]
        |> ok "Chart.upset"
    check (not (List.isEmpty upsetOut.HitTargets)) "Chart.upset in JS"
    let eulerOut =
        Chart.euler (EulerSpec.create cFrame) [ "A", 40.; "B", 30.; "C", 20. ] [ "A", "B", 10.; "B", "C", 5. ]
        |> ok "Chart.euler"
    check (eulerOut.HitTargets.Length = 3) "Chart.euler in JS"

    let sfPts = SphericalFibonacci.points 24 |> ok "SphericalFibonacci.points"
    let sfNear = SphericalFibonacci.nearestCell 24 (List.head sfPts) |> ok "SphericalFibonacci.nearestCell"
    check (sfPts.Length = 24 && sfNear = 0) "SphericalFibonacci in JS"
    let svSites = sfPts |> List.mapi (fun idx p -> sprintf "sv%d" idx, p)
    let svCells = SphericalVoronoi.relaxCvt 3 svSites |> ok "SphericalVoronoi.relaxCvt"
    let svArea = svCells |> List.sumBy (fun (c: SphericalCell<string>) -> c.Steradians)
    check (abs (svArea - 4. * Math.PI) < 1e-3) "SphericalVoronoi steradians in JS"

    // 7. Wave 2: 2D Community, Edge Bundling, Octilinear Metro Router, Analytical Layouts & Cluster Regions
    let leidenPart = Leiden.detect LeidenConfig.defaultValue mesh |> ok "Leiden.detect"
    check (leidenPart.CommunityCount >= 1) "Leiden communities in JS"
    let leidenHier = Leiden.toHierarchy "meshRoot" mesh leidenPart |> ok "Leiden.toHierarchy"
    let groupedMesh = Leiden.applyGroups mesh leidenPart |> ok "Leiden.applyGroups"

    let circMesh = CircularLayout.layout (CircularConfig.create vp2D) groupedMesh |> ok "CircularLayout"
    check (circMesh.Nodes.Length = 30) "CircularLayout in JS"

    let heb2Out = HierarchicalBundle.bundle 0.85 leidenHier circMesh |> ok "HierarchicalBundle"
    let div2Out = DividedForceBundle.bundle { DividedBundleConfig.defaultValue with Cycles = 2; IterationsInitial = 16 } groupedMesh circMesh |> ok "DividedForceBundle"
    let min2Out = MingleBundle.bundle MingleConfig.defaultValue groupedMesh circMesh |> ok "MingleBundle"
    let kde2Out = KdeBundle2D.bundle { KdeBundleConfig.defaultValue with GridSize = 48; Iterations = 4 } groupedMesh circMesh |> ok "KdeBundle2D"
    check
        (heb2Out.Associations.Length = 60
         && div2Out.Associations.Length = 60
         && min2Out.Associations.Length = 60
         && kde2Out.Associations.Length = 60)
        "2D edge bundlers in JS"

    let octOut = OctilinearRouter.route OctilinearRouterConfig.defaultValue diagram sugOrtho |> ok "OctilinearRouter"
    check (octOut.Associations.Length = 3) "OctilinearRouter in JS"

    let arcOut = ArcLayout.layout (ArcConfig.create vp2D) diagram |> ok "ArcLayout"
    let hiveOut = HivePlot.layout { HiveConfig.create vp2D with SplitAxes = true } diagram |> ok "HivePlot"
    let bfOut : BioFabricLayout = BioFabric.layout vp2D diagram |> ok "BioFabric"
    check (arcOut.Nodes.Length = 4 && hiveOut.Nodes.Length = 4 && bfOut.Rows.Length = 4) "Analytical layouts in JS"

    let prismOut = PrismOverlap.polishLayout 6.0 30 groupedMesh circMesh |> ok "PrismOverlap"
    let gmapAssign = [ for n in prismOut.Nodes -> n.Id, Option.defaultValue "g0" n.ParentGroup ]
    let gmapRegions = GMap.compute 20.0 1 gmapAssign prismOut |> ok "GMap"
    let bubbleSets =
        gmapAssign
        |> List.groupBy snd
        |> List.map (fun (gid, xs) -> gid, List.map fst xs)
    let bubbleRegions = BubbleSets.compute { BubbleSetConfig.defaultValue with GridStep = 10. } bubbleSets prismOut |> ok "BubbleSets"
    check (not (List.isEmpty gmapRegions) && not (List.isEmpty bubbleRegions)) "GMap & BubbleSets in JS"

    // 8. Wave 2: High-D Neighborhood & 2D Manifold Embeddings (BallTree, NnDescent, Umap2D, Tsne2D, Isomap2D, Sammon2D)
    let featRows =
        [ for c in 0 .. 2 do
            for i in 0 .. 9 ->
                let baseVal = float (c * 12)
                let idx = c * 10 + i
                sprintf "f_%d_%d" c i,
                [ baseVal + sin (float idx); -baseVal + cos (float idx); float (c * 5) + 0.2 * float i; sin (float (idx * 2)); cos (float (idx * 3)); float c ] ]
    let featDs = FeatureDataset.ofRows featRows |> ok "FeatureDataset.ofRows"
    let bTree = BallTree.build DistanceMetric.Euclidean featDs
    let btNear = BallTree.kNearest 4 (snd (List.head featRows)) bTree |> ok "BallTree.kNearest"
    check (fst (List.head btNear) = "f_0_0") "BallTree exact self-nearest in JS"
    let nnLinks = NnDescent.buildGraph DistanceMetric.Euclidean 5 42u featDs |> ok "NnDescent.buildGraph"
    check (nnLinks.Length = 150) "NnDescent link count in JS"

    let umap2Pts = Umap2D.embed { UmapConfig.create vp2D with Neighbors = 6; Epochs = 40 } featDs |> ok "Umap2D.embed"
    let tsne2Pts = Tsne2D.embed { TsneConfig.create vp2D with Perplexity = 8.; Iterations = 60 } featDs |> ok "Tsne2D.embed"
    let iso2Pts = Isomap2D.embed vp2D DistanceMetric.Euclidean 12 featDs |> ok "Isomap2D.embed"
    let sam2Pts = Sammon2D.embed vp2D DistanceMetric.Euclidean 40 42u featDs |> ok "Sammon2D.embed"
    check (umap2Pts.Length = 30 && tsne2Pts.Length = 30 && iso2Pts.Length = 30 && sam2Pts.Length = 30) "2D manifold embeddings in JS"

    // 9. Wave 2: 3D Hierarchies, Spherical 3D, 3D Edge Bundling & 3D Manifold Embeddings
    let tm3Out = Treemap3D.layout { Treemap3DConfig.create vp3D with Tiling = Treemap3DTiling.CubeHilbert } hSample |> ok "Treemap3D.layout"
    let tm3Diag = Treemap3D.toDiagramLayout tm3Out
    check (tm3Out.Length = 8 && tm3Diag.Nodes.Length = 5) "Treemap3D in JS"

    let ccOut = CodeCity.layout (CodeCityConfig.create (Bounds.create -100. -100. 200. 200.) 80. (fun _ -> 25.)) hSample |> ok "CodeCity.layout"
    check ((CodeCity.toDiagramLayout ccOut).Nodes.Length = 5) "CodeCity in JS"

    let pk3Out = Pack3D.layout (Pack3DConfig.create vp3D) hSample |> ok "Pack3D.layout"
    check ((Pack3D.toDiagramLayout pk3Out).Nodes.Length = 5) "Pack3D in JS"

    let vt3Out = VoronoiTreemap3D.layout 25 2e-2 unitBox3 hSample |> ok "VoronoiTreemap3D.layout"
    check (vt3Out.Length = 8) "VoronoiTreemap3D in JS"

    let sphFib3 = SphericalFibonacci3D.layout (Point3.create 0. 0. 0.) 200. 8 groupedMesh |> ok "SphericalFibonacci3D"
    let sphStr3 = SphericalStress3D.layout { SphericalLayoutConfig.create (Point3.create 0. 0. 0.) 200. with Iterations = 20; ArcSegments = 8 } groupedMesh |> ok "SphericalStress3D"
    check (sphFib3.Nodes.Length = 30 && sphStr3.Nodes.Length = 30) "Spherical 3D layouts in JS"

    let heb3Out = HierarchicalBundle3D.bundle 0.85 10 leidenHier sphFib3 |> ok "HierarchicalBundle3D"
    let kde3Out = KdeBundle3D.bundle { KdeBundle3DConfig.defaultValue with GridSize = 24; Iterations = 3; SamplePointsPerEdge = 10 } sphFib3 |> ok "KdeBundle3D"
    check (heb3Out.Associations.Length = 60 && kde3Out.Associations.Length = 60) "3D edge bundlers in JS"

    let umap3Pts = Umap3D.embed { Umap3DConfig.create vp3D with Neighbors = 6; Epochs = 40 } featDs |> ok "Umap3D.embed"
    let umap3Diag = Umap3D.toDiagramLayout 4.0 umap3Pts nnLinks
    let tsne3Pts = Tsne3D.embed { Tsne3DConfig.create vp3D with Perplexity = 8.; Iterations = 60 } featDs |> ok "Tsne3D.embed"
    let iso3Pts = Isomap3D.embed vp3D DistanceMetric.Euclidean 12 featDs |> ok "Isomap3D.embed"
    let sam3Pts = Sammon3D.embed vp3D DistanceMetric.Euclidean 40 42u featDs |> ok "Sammon3D.embed"
    check (umap3Diag.Nodes.Length = 30 && tsne3Pts.Length = 30 && iso3Pts.Length = 30 && sam3Pts.Length = 30) "3D manifold embeddings in JS"

    // 10. Atlas Recipes: Hyperbolic2D, PoincareDisk, ConstellationLayout, Pava, IsotonicLayers, LabelCull & PoincareBallLift
    let hpA = Hyperbolic2D.ofPolar 1.1 0.4
    let hpB = Hyperbolic2D.ofPolar 1.8 -0.9
    let hpF = Hyperbolic2D.ofPolar 0.9 1.2
    let hpF0, _ = Hyperbolic2D.mobiusTranslate hpF hpF
    let hpA1, _ = Hyperbolic2D.mobiusTranslate hpF hpA
    let hpB1, _ = Hyperbolic2D.mobiusTranslate hpF hpB
    check
        (abs hpF0.X < 1e-11 && abs hpF0.Y < 1e-11 && abs (Hyperbolic2D.distance hpA hpB - Hyperbolic2D.distance hpA1 hpB1) < 1e-8)
        "Hyperbolic2D.mobiusTranslate isometry in JS"

    let pdState = PoincareDisk.buildState (PoincareDiskConfig.create vp2D) treeGraph |> ok "PoincareDisk.buildState"
    let pdFocused = PoincareDisk.focusNode "t5" pdState |> ok "PoincareDisk.focusNode" |> PoincareDisk.project
    let t5Node = pdFocused.Nodes |> List.find (fun n -> n.Id = "t5")
    check
        (pdFocused.Nodes.Length = 31 && sqrt (Point.distanceSquared (PoincareDisk.diskCenter pdState) t5Node.Center) < 1e-5)
        "PoincareDisk.focusNode in JS"

    let constOut =
        ConstellationLayout.compute
            { ConstellationConfig.create vp2D with ClusterMode = ConstellationClusterMode.HybridGroupsAndLeiden(8, 1.0) }
            groupedMesh
        |> ok "ConstellationLayout.compute"
    check (constOut.Layout.Nodes.Length = 30 && not constOut.Clusters.IsEmpty) "ConstellationLayout.compute in JS"

    let pavaXs = Pava.solveWeighted [ 90.; 40.; 70.; 50. ] [ 1.; 2.; 1.; 2. ] [ 12.; 15.; 10. ] 10. 180. |> ok "Pava.solveWeighted"
    check (pavaXs.Length = 4 && (pavaXs[1]) - (pavaXs[0]) >= 12. - 1e-8) "Pava.solveWeighted in JS"

    let pdBase2D = PoincareDisk.project pdState
    let isoOut =
        IsotonicLayers.compute
            { IsotonicLayersConfig.create vp2D with ReferenceLayout = Some pdBase2D }
            treeGraph
        |> ok "IsotonicLayers.compute"
    let isoMid = IsotonicLayers.interpolate 0.5 pdBase2D isoOut.Layout |> ok "IsotonicLayers.interpolate"
    check (isoOut.Layout.Nodes.Length = 31 && isoMid.Nodes.Length = 31) "IsotonicLayers in JS"

    let culledLbls =
        LabelCull.select
            vp2D
            4.
            []
            [ { LabelCandidate.Id = "l1"; Anchor = Point.create 120. 120.; Width = 44.; Height = 14.; Priority = 10.; OffsetCandidates = [ Point.create 0. 0. ] }
              { LabelCandidate.Id = "l2"; Anchor = Point.create 122. 120.; Width = 44.; Height = 14.; Priority = 2.; OffsetCandidates = [ Point.create 0. 0. ] } ]
    check (culledLbls.Length = 1 && culledLbls.Head.Id = "l1") "LabelCull.select in JS"

    let pbl0 = PoincareBallLift.morph 0.0 (PoincareBallLiftConfig.create (Point3.create 0. 0. 0.) 200.) pdState
    let pbl1 = PoincareBallLift.morph 1.0 (PoincareBallLiftConfig.create (Point3.create 0. 0. 0.) 200.) pdState
    check
        (pbl0.Nodes.Length = 31 && (pbl1.Nodes |> List.forall (fun n -> abs n.Center.Z < 1e-8)))
        "PoincareBallLift.morph in JS"

    printfn "Fickl JS runtime suite passed: all Wave 1, Wave 2 and Atlas Recipe 2D/3D engines executed cleanly in JavaScript."
    0

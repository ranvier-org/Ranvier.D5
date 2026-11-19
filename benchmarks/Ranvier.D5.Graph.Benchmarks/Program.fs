module Ranvier.D5.Graph.Benchmarks.Program

open System
open System.IO
open System.Text.Json
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.Geo
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D
open Ranvier.D5.BenchmarkHarness

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors

let private buildCompoundDag (nodeCount: int) (groupCount: int) =
    let groups = [ for g in 0 .. groupCount - 1 -> DiagramGroup.create (sprintf "g%d" g) ]
    let nodes =
        [ for i in 0 .. nodeCount - 1 ->
            DiagramNode.sized (sprintf "n%d" i) (60. + float ((i * 7) % 40)) (32. + float ((i * 5) % 16))
            |> DiagramNode.inGroup (sprintf "g%d" (i % groupCount)) ]
    let edges =
        [ for i in 0 .. nodeCount - 2 ->
            Association.create (sprintf "e_chain_%d" i) (sprintf "n%d" i) (sprintf "n%d" (min (nodeCount - 1) (i + 1 + (i % 4))))
          for i in 0 .. (nodeCount * 3 / 5) - 1 ->
            let s = (i * 3) % (nodeCount - 3)
            let t = min (nodeCount - 1) (s + 2 + (i % 6))
            Association.create (sprintf "e_skip_%d" i) (sprintf "n%d" s) (sprintf "n%d" t) ]
    DiagramGraph.create nodes groups edges |> ok

let private buildMeshNetwork (nodeCount: int) (groupCount: int) =
    let groups = [ for g in 0 .. groupCount - 1 -> DiagramGroup.create (sprintf "c%d" g) ]
    let nodes =
        [ for i in 0 .. nodeCount - 1 ->
            let nd = DiagramNode.sized (sprintf "v%d" i) 36. 28.
            if groupCount > 0 then DiagramNode.inGroup (sprintf "c%d" (i % groupCount)) nd else nd ]
    let edges =
        [ for i in 0 .. nodeCount - 1 do
            yield Association.create (sprintf "ring_%d" i) (sprintf "v%d" i) (sprintf "v%d" ((i + 1) % nodeCount))
            yield Association.create (sprintf "chord_%d" i) (sprintf "v%d" i) (sprintf "v%d" ((i + 13) % nodeCount)) ]
    DiagramGraph.create nodes groups edges |> ok

let private buildScaleHierarchy (nodeCount: int) (branching: int) (withCrossLinks: bool) =
    let nodes = [ for i in 0 .. nodeCount - 1 -> DiagramNode.create (sprintf "h%d" i) ]
    let treeEdges =
        [ for i in 1 .. nodeCount - 1 ->
            Association.create (sprintf "t%d" i) (sprintf "h%d" ((i - 1) / branching)) (sprintf "h%d" i) ]
    let crossEdges =
        if not withCrossLinks then []
        else
            [ for k in 0 .. (nodeCount / 10) - 1 ->
                let u = (k * 17 + 5) % nodeCount
                let v = (k * 31 + 19) % nodeCount
                Association.create (sprintf "x%d" k) (sprintf "h%d" u) (sprintf "h%d" v) ]
    DiagramGraph.simple nodes (treeEdges @ crossEdges) |> ok

[<EntryPoint>]
let main args =
    let vp2D = Bounds.create 0. 0. 1200. 800.
    let vp3D = Bounds3.create -400. -400. -400. 800. 800. 800.

    let dag250 = buildCompoundDag 250 10
    let dag120 = buildCompoundDag 120 6
    let net120 = buildMeshNetwork 120 4
    let net300 = buildMeshNetwork 300 8
    let net500 = buildMeshNetwork 500 0
    let net2000 = buildMeshNetwork 2000 0
    let net400_3d = buildMeshNetwork 400 6
    let tree1000 = buildScaleHierarchy 1000 4 false
    let tree2000 = buildScaleHierarchy 2000 5 false
    let tree5000 = buildScaleHierarchy 5000 5 true
    let tree10000 = buildScaleHierarchy 10000 6 false
    let h3State10000 = H3.buildState { H3Config.defaultValue with RootId = Some "h0" } tree10000 |> ok

    let octPts =
        [| for i in 0 .. 9999 ->
            Point3.create (sin (float i * 1.37) * 500.) (cos (float i * 2.11) * 500.) (sin (float i * 0.73 + 1.1) * 500.) |]
    let builtOct = Octree.build id (fun _ -> 1.) octPts

    // Wave 2 benchmark fixtures
    let boxes2d_10000 =
        [| for i in 0 .. 9999 ->
            let x = (sin (float i * 1.31) * 0.5 + 0.5) * 1140.
            let y = (cos (float i * 1.93) * 0.5 + 0.5) * 760.
            i, Bounds.create x y (8. + float (i % 15)) (6. + float ((i * 3) % 13)) |]
    let queries2d_200 =
        [| for q in 0 .. 199 ->
            Bounds.create (float ((q * 37) % 1050)) (float ((q * 53) % 680)) 90. 70. |]

    let boxes3d_5000 =
        [| for i in 0 .. 4999 ->
            let x = sin (float i * 1.47) * 360.
            let y = cos (float i * 2.09) * 360.
            let z = sin (float i * 0.83 + 0.6) * 360.
            i, Bounds3.create x y z 18. 16. 20. |]
    let rays3d_500 =
        [| for r in 0 .. 499 ->
            let ox = -450. + float ((r * 19) % 200)
            let oy = -320. + float ((r * 29) % 640)
            let oz = -320. + float ((r * 41) % 640)
            Point3.create ox oy oz, Point3.create 1. 0.25 -0.15 |]

    let oloItems120 = [ 0 .. 119 ]
    let oloDistMat120 =
        let xs = [| for i in 0 .. 119 -> float (i / 20) * 40. + sin (float i * 1.7) * 5. |]
        let ys = [| for i in 0 .. 119 -> float ((i / 20) % 3) * 35. + cos (float i * 2.1) * 5. |]
        [| for i in 0 .. 119 do
            for j in 0 .. 119 ->
                Double.Hypot(xs[i] - xs[j], ys[i] - ys[j]) |]
    let oloDist120 i j = oloDistMat120[i * 120 + j]

    let net1000 = buildMeshNetwork 1000 0

    let hierarchy80Leaves =
        Hierarchy.node "vtRoot" 0.
            [ for g in 0 .. 7 ->
                Hierarchy.node (sprintf "vg%d" g) 0.
                    [ for k in 0 .. 9 -> Hierarchy.node (sprintf "vl_%d_%d" g k) (1. + float ((g * 3 + k) % 9)) [] ] ]

    let sites3d_250 =
        [ for i in 0 .. 249 ->
            let x = -350. + float (i % 5) * 160. + sin (float i * 1.73) * 28.
            let y = -350. + float ((i / 5) % 5) * 160. + cos (float i * 2.19) * 28.
            let z = -350. + float (i / 25) * 72. + sin (float i * 1.11 + 0.4) * 22.
            Point3.create x y z ]

    let hierarchy120Leaves =
        Hierarchy.node "gmRoot" 0.
            [ for g in 0 .. 9 ->
                Hierarchy.node (sprintf "gg%d" g) 0.
                    [ for k in 0 .. 11 -> Hierarchy.node (sprintf "gl_%d_%d" g k) (1. + float ((g + k) % 7)) [] ] ]

    let net200_400 = buildMeshNetwork 200 6
    let circ200_400 = CircularLayout.layout (CircularConfig.create vp2D) net200_400 |> ok

    let metroGraph40 =
        let mNodes = [ for i in 0 .. 23 -> DiagramNode.sized (sprintf "st%d" i) 34. 24. ]
        let mEdges =
            [ for i in 0 .. 23 -> Association.create (sprintf "mr_%d" i) (sprintf "st%d" i) (sprintf "st%d" ((i + 1) % 24))
              for i in 0 .. 15 -> Association.create (sprintf "mx_%d" i) (sprintf "st%d" i) (sprintf "st%d" ((i + 5) % 24)) ]
        DiagramGraph.simple mNodes mEdges |> ok
    let metroBase40 = CircularLayout.layout (CircularConfig.create vp2D) metroGraph40 |> ok

    let cola120 = Cola.layout { ColaConfig.create vp2D with Iterations = 25; AvoidOverlaps = true } net120 |> ok
    let bubbleGroups120 =
        [ for g in 0 .. 3 ->
            let gid = sprintf "c%d" g
            gid,
            cola120.Nodes
            |> List.choose (fun n -> if n.ParentGroup = Some gid then Some n.Id else None) ]

    let ds250_16d =
        [ for i in 0 .. 249 ->
            let c = i / 50
            let feats =
                [ for d in 0 .. 15 ->
                    float (((c * 7 + d * 3) % 11) - 5) * 4.0
                    + sin (float (i * 5 + d * 11) * 1.17)
                    + 0.5 * cos (float (i * 3 + d * 7)) ]
            sprintf "obs%d" i, feats ]
        |> FeatureDataset.ofRows
        |> ok

    let cityTree300 =
        Hierarchy.node "cityRoot" 0.
            [ for d in 0 .. 8 ->
                let leafCount = if d < 2 then 33 else 32
                Hierarchy.node (sprintf "dist%d" d) 0.
                    [ for b in 0 .. leafCount - 1 ->
                        Hierarchy.node (sprintf "bldg_%d_%d" d b) (1. + float ((d * 5 + b) % 12)) [] ] ]
    let cityCfg300 =
        CodeCityConfig.create (Bounds.create -300. -300. 600. 600.) 140. (fun id -> float ((id.GetHashCode() &&& 0x7F) + 10))

    let net250_sph = buildMeshNetwork 250 0
    let sph150_300 = SphericalFibonacci3D.layout (Point3.create 0. 0. 0.) 300. 8 (buildMeshNetwork 150 0) |> ok

    let results = [|
        measure "sugiyama-compound-spline-250-nodes" 120. 32000000L (fun () ->
            let out = Sugiyama.layout { SugiyamaConfig.create vp2D with Routing = SugiyamaRouting.CubicSpline } dag250 |> ok
            if out.Nodes.Length <> 250 then failwith "Sugiyama 250 node count")

        measure "sugiyama-compound-orthogonal-120-nodes" 160. 40000000L (fun () ->
            let out = Sugiyama.layout { SugiyamaConfig.create vp2D with Routing = SugiyamaRouting.Orthogonal } dag120 |> ok
            if out.Nodes.Length <> 120 then failwith "Sugiyama orthogonal node count")

        measure "cola-constrained-nonoverlap-120-nodes" 150. 32000000L (fun () ->
            let out = Cola.layout { ColaConfig.create vp2D with Iterations = 30; AvoidOverlaps = true } net120 |> ok
            if out.Nodes.Length <> 120 then failwith "Cola node count")

        measure "fcose-compound-300-nodes-600-edges" 200. 48000000L (fun () ->
            let out = FCose.layout { FCoseConfig.create vp2D with Iterations = 45 } net300 |> ok
            if out.Nodes.Length <> 300 then failwith "fCoSE node count")

        measure "pivotmds-2d-2000-nodes-4000-edges" 120. 32000000L (fun () ->
            let out = PivotMds.layout { PivotMdsConfig.create vp2D with PivotCount = 40 } net2000 |> ok
            if out.Nodes.Length <> 2000 then failwith "PivotMDS 2000 node count")

        measure "stress-sgd-2d-500-nodes-1000-edges" 120. 28000000L (fun () ->
            let out = Stress2D.layout { Stress2DConfig.create vp2D with Iterations = 15; ExactPairLimit = 200; PivotCount = 36 } net500 |> ok
            if out.Nodes.Length <> 500 then failwith "Stress2D 500 node count")

        measure "modularity-forceatlas2-500-nodes-1000-edges" 220. 48000000L (fun () ->
            let out = ModularityForce2D.layout { ModularityForceConfig.create vp2D with Iterations = 45 } net500 |> ok
            if out.Nodes.Length <> 500 then failwith "ModularityForce2D 500 node count")

        measure "radial-mindmap-1000-nodes" 80. 24000000L (fun () ->
            let out = RadialTree.layout { RadialTreeConfig.create vp2D with Mode = RadialMode.BidirectionalMindmap; RootId = Some "h0" } tree1000 |> ok
            if out.Nodes.Length <> 1000 then failwith "RadialTree 1000 node count")

        measure "boxpack-compound-300-nodes" 50. 16000000L (fun () ->
            let out = BoxPack.layout (BoxPackConfig.create vp2D) net300 |> ok
            if out.Nodes.Length <> 300 then failwith "BoxPack 300 node count")

        measure "h3-munzner-5000-nodes-klein" 150. 48000000L (fun () ->
            let out = H3.layout { H3Config.defaultValue with RootId = Some "h0" } tree5000 |> ok
            if out.Nodes.Length <> 5000 then failwith "H3 5000 node count")

        measure "h3-lorentz-focus-boost-project-10000-nodes" 60. 24000000L (fun () ->
            let focused = H3.focusNode "h9999" h3State10000 |> ok |> H3.project
            if focused.Nodes.Length <> 10000 then failwith "H3 focus boost 10000 node count")

        measure "walrus-caida-5000-nodes-crosslinks" 150. 48000000L (fun () ->
            let out = Walrus.layout { WalrusConfig.defaultValue with RootId = Some "h0" } tree5000 |> ok
            if out.Nodes.Length <> 5000 then failwith "Walrus 5000 node count")

        measure "octree-3d-build-10000" 60. 16000000L (fun () ->
            let t = Octree.build id (fun _ -> 1.) octPts
            if Octree.count t <> 10000 then failwith "Octree count")

        measure "octree-3d-nearest-200-in-10000" 40. 4000000L (fun () ->
            for i = 0 to 199 do
                let q = Point3.create (octPts[i * 47].X + 1.5) (octPts[i * 47].Y - 1.5) octPts[i * 47].Z
                if (Octree.nearest q infinity builtOct).IsNone then failwith "Octree nearest")

        measure "force3d-octree-400-nodes-800-edges" 250. 48000000L (fun () ->
            let out = Force3D.layout { Force3DConfig.create vp3D with Iterations = 40 } net400_3d |> ok
            if out.Nodes.Length <> 400 then failwith "Force3D 400 node count")

        measure "stress-sgd-3d-500-nodes-1000-edges" 150. 32000000L (fun () ->
            let out = Stress3D.layout { Stress3DConfig.create vp3D with Iterations = 15; ExactPairLimit = 200; PivotCount = 36 } net500 |> ok
            if out.Nodes.Length <> 500 then failwith "Stress3D 500 node count")

        measure "conetree-balloon-3d-2000-nodes" 80. 24000000L (fun () ->
            let out = ConeTree3D.layout { ConeTree3DConfig.create vp3D with Mode = ConeTreeMode.SphericalBalloon; RootId = Some "h0" } tree2000 |> ok
            if out.Nodes.Length <> 2000 then failwith "ConeTree3D 2000 node count")

        measure "layered-3d-stratified-250-nodes" 150. 36000000L (fun () ->
            let out = Layered3D.layout (Layered3DConfig.create vp3D) dag250 |> ok
            if out.Nodes.Length <> 250 then failwith "Layered3D 250 node count")

        measure "rtree2d-hilbert-build-and-200-queries-10000" 250. 64000000L (fun () ->
            let rt = RTree2D.build snd boxes2d_10000
            if RTree2D.count rt <> 10000 then failwith "RTree2D 10000 count"
            for q in 0 .. 199 do
                let hits = RTree2D.search (queries2d_200[q]) rt
                if hits.Length < 0 then failwith "RTree2D query")

        measure "bvh3d-morton-build-and-500-raycasts-5000" 250. 64000000L (fun () ->
            let bvh = Bvh3D.build snd boxes3d_5000
            if Bvh3D.count bvh <> 5000 then failwith "Bvh3D 5000 count"
            for r in 0 .. 499 do
                let orig, dir = rays3d_500[r]
                let hit = Bvh3D.raycastClosest orig dir 1200. bvh
                if hit.IsSome && snd hit.Value < 0. then failwith "Bvh3D raycast")

        measure "seriation-olo-ward-120-items" 250. 64000000L (fun () ->
            let tree = Seriation.hierarchicalCluster Linkage.Ward oloItems120 oloDist120 |> ok |> Option.get
            let ord = Seriation.optimalLeafOrder tree oloDist120
            if ord.Length <> 120 then failwith "Seriation OLO 120 count")

        measure "leiden-modularity-1000-nodes" 250. 64000000L (fun () ->
            let part = Leiden.detect LeidenConfig.defaultValue net1000 |> ok
            if part.CommunityCount < 1 then failwith "Leiden 1000 community count")

        measure "voronoi-treemap-2d-80-leaves" 350. 80000000L (fun () ->
            let cells =
                VoronoiTreemap.layout
                    { VoronoiTreemapConfig.create vp2D with MaxIterations = 35; AreaTolerance = 5e-3 }
                    hierarchy80Leaves
                |> ok
            if cells.Length <> 89 then failwith "VoronoiTreemap 80 leaves cell count")

        measure "voronoi3d-polyhedral-clip-250-sites" 350. 80000000L (fun () ->
            let cells = Voronoi3D.clipBounds vp3D sites3d_250 |> ok
            if cells.Length <> 250 then failwith "Voronoi3D 250 sites count")

        measure "gospermap-hex-hierarchy-120-leaves" 250. 64000000L (fun () ->
            let regs = GosperMap.layout { GosperMapConfig.create vp2D with Order = 3; SmoothIterations = 1 } hierarchy120Leaves |> ok
            if regs.Length <> 131 then failwith "GosperMap 120 leaves count")

        measure "spherical-fibonacci-voronoi-300-sites" 250. 64000000L (fun () ->
            let pts = SphericalFibonacci.points 300 |> ok
            let sites = pts |> List.mapi (fun idx p -> idx, p)
            let cells = SphericalVoronoi.compute sites |> ok
            if cells.Length <> 300 then failwith "SphericalVoronoi 300 sites count")

        measure "kde-bundle-2d-400-edges" 350. 80000000L (fun () ->
            let out = KdeBundle2D.bundle { KdeBundleConfig.defaultValue with GridSize = 96; Iterations = 6 } net200_400 circ200_400 |> ok
            if out.Associations.Length <> 400 then failwith "KdeBundle2D 400 edges count")

        measure "mingle-bundle-2d-400-edges" 300. 80000000L (fun () ->
            let out = MingleBundle.bundle MingleConfig.defaultValue net200_400 circ200_400 |> ok
            if out.Associations.Length <> 400 then failwith "MingleBundle 400 edges count")

        measure "octilinear-metro-router-40-edges" 350. 80000000L (fun () ->
            let out = OctilinearRouter.route { OctilinearRouterConfig.defaultValue with GridSpacing = 32. } metroGraph40 metroBase40 |> ok
            if out.Associations.Length <> 40 then failwith "OctilinearRouter 40 edges count")

        measure "bubblesets-contours-120-nodes" 250. 64000000L (fun () ->
            let regs = BubbleSets.compute { BubbleSetConfig.defaultValue with GridStep = 8. } bubbleGroups120 cola120 |> ok
            if regs.Length <> 4 then failwith "BubbleSets 120 nodes group count")

        measure "umap2d-250-points-16d" 350. 80000000L (fun () ->
            let pts = Umap2D.embed { UmapConfig.create vp2D with Neighbors = 12; Epochs = 120 } ds250_16d |> ok
            if pts.Length <> 250 then failwith "Umap2D 250 points count")

        measure "tsne2d-barnes-hut-250-points-16d" 350. 80000000L (fun () ->
            let pts = Tsne2D.embed { TsneConfig.create vp2D with Perplexity = 20.; Iterations = 150 } ds250_16d |> ok
            if pts.Length <> 250 then failwith "Tsne2D 250 points count")

        measure "codecity-treemap3d-300-nodes" 250. 64000000L (fun () ->
            let bldgs = CodeCity.layout cityCfg300 cityTree300 |> ok
            let diag = CodeCity.toDiagramLayout bldgs
            if bldgs.Length <> 300 || diag.Nodes.Length <> 290 then failwith "CodeCity 300 nodes count")

        measure "pack3d-spheres-300-nodes" 250. 64000000L (fun () ->
            let spheres = Pack3D.layout (Pack3DConfig.create vp3D) cityTree300 |> ok
            if spheres.Length <> 300 then failwith "Pack3D 300 nodes count")

        measure "spherical-stress-3d-250-nodes" 350. 80000000L (fun () ->
            let out =
                SphericalStress3D.layout
                    { SphericalLayoutConfig.create (Point3.create 0. 0. 0.) 300. with Iterations = 25; ArcSegments = 8 }
                    net250_sph
                |> ok
            if out.Nodes.Length <> 250 then failwith "SphericalStress3D 250 nodes count")

        measure "kde-bundle-3d-300-edges" 350. 96000000L (fun () ->
            let out =
                KdeBundle3D.bundle
                    { KdeBundle3DConfig.defaultValue with GridSize = 36; Iterations = 5; SamplePointsPerEdge = 16 }
                    sph150_300
                |> ok
            if out.Associations.Length <> 300 then failwith "KdeBundle3D 300 edges count")

        measure "umap3d-250-points-16d" 350. 80000000L (fun () ->
            let pts = Umap3D.embed { Umap3DConfig.create vp3D with Neighbors = 12; Epochs = 120 } ds250_16d |> ok
            if pts.Length <> 250 then failwith "Umap3D 250 points count")

        measure "tsne3d-barnes-hut-250-points-16d" 400. 96000000L (fun () ->
            let pts = Tsne3D.embed { Tsne3DConfig.create vp3D with Perplexity = 20.; Iterations = 150 } ds250_16d |> ok
            if pts.Length <> 250 then failwith "Tsne3D 250 points count")
    |]

    let output = if args.Length = 0 then "artifacts/graph-benchmarks.json" else args[0]
    let directory = Path.GetDirectoryName(Path.GetFullPath output)
    Directory.CreateDirectory directory |> ignore
    File.WriteAllText(output, JsonSerializer.Serialize(results, JsonSerializerOptions(WriteIndented = true)))
    let failed = failures results
    for item in failed do printfn "%s" (describe item)
    if failed.Length > 0 then 1 else 0

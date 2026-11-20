module Ranvier.D5.Wgpu.Tests.GraphScene3DTests

open System
open System.Numerics
open Expecto
open Ranvier
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D
open Ranvier.D5.Wgpu
open Ranvier.D5.Graph3D.Wgpu
open Ranvier.Wgpu

let private unwrapOk msg =
    function
    | Ok value -> value
    | Error err -> failtestf "%s failed: %A" msg err

let private buildGroupedDiagram () =
    let groups =
        [
            { DiagramGroup.create "gOuter" with Padding = GroupPadding.titled 12.0 6.0 }
            { DiagramGroup.create "gInner" with Padding = GroupPadding.titled 10.0 4.0 } |> DiagramGroup.inGroup "gOuter"
        ]

    let nodes =
        [
            for i in 0 .. 7 do
                let id = $"n{i}"
                let grp = if i < 4 then "gInner" else "gOuter"
                yield DiagramNode.sized id 18.0 18.0 |> DiagramNode.inGroup grp
        ]

    let assocs =
        [
            for i in 0 .. 6 -> Association.create $"e{i}" $"n{i}" $"n{i + 1}"
            yield Association.create "cross" "n0" "n7"
        ]

    DiagramGraph.create nodes groups assocs |> unwrapOk "buildGroupedDiagram"

let private buildSampleTree () =
    let nodes = [ for i in 0 .. 12 -> DiagramNode.sized $"t{i}" 12.0 12.0 ]
    let edges =
        [
            for i in 1 .. 12 -> Association.create $"te{i}" $"t{(i - 1) / 3}" $"t{i}"
            yield Association.create "tx0" "t2" "t9"
        ]

    DiagramGraph.simple nodes edges |> unwrapOk "buildSampleTree"

let private buildSampleHierarchy () =
    Hierarchy.node
        "root"
        0.0
        [
            Hierarchy.node
                "alpha"
                0.0
                [
                    Hierarchy.node "a1" 25.0 []
                    Hierarchy.node "a2" 45.0 []
                    Hierarchy.node "a3" 30.0 []
                ]
            Hierarchy.node
                "beta"
                0.0
                [
                    Hierarchy.node "b1" 50.0 []
                    Hierarchy.node "b2" 20.0 []
                ]
        ]

let private openOrderAlternateCheck () =
    let theme = GraphTheme3D.lightDefault
    Expect.isGreaterThan theme.BackboneTubeRadius 0.0f "light theme has positive backbone tube radius"

[<Tests>]
let graphScene3DTests =
    testSequenced
    <| testList
        "Ranvier.D5.Graph3D.Wgpu"
        [
            testCase "scaleOfLayout, packNodes, packGroups, packAssociationLines, and buildAssociationTubes cover all diagram layout engines cleanly"
            <| fun () ->
                openOrderAlternateCheck ()
                let grouped = buildGroupedDiagram ()
                let tree = buildSampleTree ()
                let hier = buildSampleHierarchy ()
                let bounds3 = Bounds3.create -120.0 -120.0 -120.0 240.0 240.0 240.0

                let forceLayout = Force3D.layout { Force3DConfig.create bounds3 with Iterations = 15 } grouped |> unwrapOk "Force3D"
                let stressLayout = Stress3D.layout { Stress3DConfig.create bounds3 with Iterations = 12 } grouped |> unwrapOk "Stress3D"
                let coneLayout = ConeTree3D.layout { ConeTree3DConfig.create bounds3 with RootId = Some "t0" } tree |> unwrapOk "ConeTree3D"
                let layeredLayout = Layered3D.layout (Layered3DConfig.create bounds3) grouped |> unwrapOk "Layered3D"
                let h3Layout = H3.layout { H3Config.defaultValue with RootId = Some "t0"; BallRadius = 1.0 } tree |> unwrapOk "H3"
                let walrusLayout = Walrus.layout { WalrusConfig.defaultValue with RootId = Some "t0"; SphereRadius = 1.0 } tree |> unwrapOk "Walrus"
                let sphStress = SphericalStress3D.layout { SphericalLayoutConfig.create Point3.zero 1.0 with Iterations = 15 } grouped |> unwrapOk "SphericalStress3D"
                let sphFib = SphericalFibonacci3D.layout Point3.zero 1.0 6 grouped |> unwrapOk "SphericalFibonacci3D"
                let pdState = PoincareDisk.buildState (PoincareDiskConfig.create (Bounds.create 0.0 0.0 200.0 200.0)) tree |> unwrapOk "PoincareDisk"
                let pblFlat = PoincareBallLift.morph 1.0 (PoincareBallLiftConfig.create Point3.zero 1.0) pdState
                let hebLayout = HierarchicalBundle3D.bundleGraph 0.8 8 grouped sphStress |> unwrapOk "HierarchicalBundle3D"
                let kdeLayout = KdeBundle3D.bundle { KdeBundle3DConfig.defaultValue with GridSize = 20; Iterations = 3; SamplePointsPerEdge = 8 } hebLayout |> unwrapOk "KdeBundle3D"

                let featRows =
                    [
                        for c in 0 .. 1 do
                            for i in 0 .. 5 ->
                                $"u_{c}_{i}", [ float (c * 10) + sin (float i); cos (float i); float c ]
                    ]

                let fds = FeatureDataset.ofRows featRows |> unwrapOk "FeatureDataset"
                let uPts, uLinks = Umap3D.embedWithLinks { Umap3DConfig.create bounds3 with Neighbors = 4; Epochs = 30 } fds |> unwrapOk "Umap3D"
                let umapLayout = Umap3D.toDiagramLayout 2.5 uPts uLinks

                let allLayouts =
                    [
                        "Force3D", forceLayout
                        "Stress3D", stressLayout
                        "ConeTree3D", coneLayout
                        "Layered3D", layeredLayout
                        "H3", h3Layout
                        "Walrus", walrusLayout
                        "SphericalStress3D", sphStress
                        "SphericalFibonacci3D", sphFib
                        "PoincareBallLift.Flat", pblFlat
                        "HierarchicalBundle3D", hebLayout
                        "KdeBundle3D", kdeLayout
                        "Umap3D", umapLayout
                    ]

                let theme = GraphTheme3D.darkDefault

                for name, layout in allLayouts do
                    let sc = GraphScene3D.scaleOfLayout BoxAspect.Cube UpAxis.YUp 0.06 layout
                    let glyphs = GraphScene3D.packNodes theme (Set.singleton layout.Nodes.Head.Id) layout
                    Expect.equal glyphs.Length layout.Nodes.Length $"{name} glyph count matches nodes"
                    Expect.isTrue glyphs.Span[0].Selected $"{name} first node selected"

                    let lines = GraphScene3D.packAssociationLines sc theme layout
                    Expect.isGreaterThan lines.Length 0 $"{name} packs nonzero association line vertices"

                    let groups = GraphScene3D.packGroups theme layout
                    Expect.equal groups.Length layout.Groups.Length $"{name} group count matches"

                    let tubes = GraphScene3D.buildAssociationTubes sc 5 theme layout |> unwrapOk $"{name} tubes"
                    Expect.isGreaterThan tubes.Vertices.Length 0 $"{name} tube vertices nonempty"
                    Expect.isGreaterThan tubes.Indices.Length 0 $"{name} tube indices nonempty"

                // Hyperbolic attenuation verification on H3: central focus node is strictly larger than boundary leaf
                let h3Glyphs = GraphScene3D.packNodes theme Set.empty h3Layout
                let rootIdx = h3Layout.Nodes |> List.findIndex (fun n -> n.Id = "t0")
                let minFactorIdx = h3Layout.Nodes |> List.indexed |> List.minBy (fun (_, n) -> n.ScaleFactor) |> fst
                Expect.isGreaterThan h3Glyphs.Span[rootIdx].Scale.X h3Glyphs.Span[minFactorIdx].Scale.X "H3 root glyph radius > attenuated rim node"

            testCase "CodeCity scaleOfCodeCity is UpAxis.YUp with zero Y/Z swap and packCodeCity separates leaves from district plinths"
            <| fun () ->
                let hier = buildSampleHierarchy ()
                let cfg = CodeCityConfig.create (Bounds.create -10.0 -10.0 20.0 20.0) 12.0 (fun id -> if id = "a2" then 120.0 else 30.0)
                let buildings = CodeCity.layout cfg hier |> unwrapOk "CodeCity.layout"
                let sc = GraphScene3D.scaleOfCodeCity buildings
                Expect.equal sc.UpAxis UpAxis.YUp "CodeCity scale uses YUp directly"

                let leafTall = buildings |> List.find (fun b -> b.Id = "a2")
                let basePt = Bounds3.minPoint leafTall.Bounds
                let topPt = Bounds3.maxPoint leafTall.Bounds
                let wBase = Scale3D.map sc basePt
                let wTop = Scale3D.map sc topPt
                Expect.isGreaterThan wTop.Y wBase.Y "CodeCity building +Y maps strictly upward to world +Y without Y<->Z swap"

                let struct (leaves, districts) =
                    GraphScene3D.packCodeCity
                        (fun b -> if b.IsDistrict then RgbaColor (0.2f, 0.3f, 0.4f, 0.4f) else RgbaColor (0.2f, 0.8f, 0.9f, 1.0f))
                        (ShadingMode.Lit (0.3f, 0.05f))
                        (Set.singleton "a2")
                        buildings

                Expect.equal leaves.Length 5 "5 leaf buildings packed"
                Expect.equal districts.Length 3 "3 district plinths packed (root, alpha, beta)"
                Expect.equal (leaves.Span.ToArray () |> Array.filter (fun b -> b.Selected) |> Array.length) 1 "Selected building flagged"

                let gpuBoxes = Glyph3D.packBoxes sc leaves.Span
                Expect.equal gpuBoxes.Length 5 "Glyph3D.packBoxes accepts CodeCity leaves directly"

            testCase "Treemap3D cuboids/wireframe and Pack3D spheres pack leaves and internal hierarchies cleanly"
            <| fun () ->
                let hier = buildSampleHierarchy ()
                let bounds3 = Bounds3.create -10.0 -10.0 -10.0 20.0 20.0 20.0
                let cuboids = Treemap3D.layout (Treemap3DConfig.create bounds3) hier |> unwrapOk "Treemap3D"
                let tmScale = Scale3D.ofBounds3 BoxAspect.Cube UpAxis.YUp bounds3

                let allBoxes = GraphScene3D.packTreemapCuboids (fun _ -> RgbaColor.white) ShadingMode.Unlit false cuboids
                let leafBoxes = GraphScene3D.packTreemapCuboids (fun _ -> RgbaColor.white) ShadingMode.Unlit true cuboids
                Expect.equal allBoxes.Length 8 "all Treemap3D cuboids packed"
                Expect.equal leafBoxes.Length 5 "leaf Treemap3D cuboids packed"

                let wireVerts = GraphScene3D.packTreemapWireframe tmScale (fun _ -> RgbaColor.white) false cuboids
                Expect.equal wireVerts.Length (cuboids.Length * 24) "each cuboid emits 12 edges = 24 LineVertex3D"

                let spheres = Pack3D.layout (Pack3DConfig.create bounds3) hier |> unwrapOk "Pack3D"
                let allGlyphs = GraphScene3D.packSpheres (fun _ -> RgbaColor.white) ShadingMode.Unlit false spheres
                let leafGlyphs = GraphScene3D.packSpheres (fun _ -> RgbaColor.white) ShadingMode.Unlit true spheres
                Expect.equal allGlyphs.Length 8 "all spheres packed"
                Expect.equal leafGlyphs.Length 5 "leaf spheres packed"

            testCase "VoronoiTreemap3D & PolyhedronCell3D build exploded convex polyhedra meshes and deduplicate polygonal wireframe edges"
            <| fun () ->
                let unitBox3 = Bounds3.create -1.0 -1.0 -1.0 2.0 2.0 2.0
                let scale = Scale3D.ofBounds3 BoxAspect.Cube UpAxis.YUp unitBox3
                let hier = buildSampleHierarchy ()
                let vtCells = VoronoiTreemap3D.layout 30 2e-2 unitBox3 hier |> unwrapOk "VoronoiTreemap3D"

                let meshFull = GraphScene3D.buildVoronoiCellMeshes scale 1.0 (fun _ -> RgbaColor.white) vtCells
                let meshExploded = GraphScene3D.buildVoronoiCellMeshes scale 0.8 (fun _ -> RgbaColor.white) vtCells
                Expect.isGreaterThan meshFull.Vertices.Length 0 "Voronoi mesh has vertices"
                Expect.equal meshFull.Vertices.Length meshExploded.Vertices.Length "shrinkFactor preserves topology"

                let mutV0 = meshFull.Vertices[0].Position
                let mutVExp0 = meshExploded.Vertices[0].Position
                Expect.isFalse (mutV0 = mutVExp0) "shrinkFactor < 1 pulls vertices toward cell centroid"

                Expect.throwsT<ArgumentOutOfRangeException>
                    (fun () -> GraphScene3D.buildVoronoiCellMeshes scale 0.0 (fun _ -> RgbaColor.white) vtCells |> ignore)
                    "shrinkFactor <= 0 rejected"

                Expect.throwsT<ArgumentOutOfRangeException>
                    (fun () -> GraphScene3D.buildVoronoiCellMeshes scale 1.2 (fun _ -> RgbaColor.white) vtCells |> ignore)
                    "shrinkFactor > 1 rejected"

                // Cube polyhedron has 6 quad faces (24 directed face-edge steps) -> deduplicates strictly to 12 edges = 24 vertices!
                let cubeVerts =
                    [
                        Point3.create -1.0 -1.0 -1.0; Point3.create 1.0 -1.0 -1.0; Point3.create 1.0 1.0 -1.0; Point3.create -1.0 1.0 -1.0
                        Point3.create -1.0 -1.0 1.0; Point3.create 1.0 -1.0 1.0; Point3.create 1.0 1.0 1.0; Point3.create -1.0 1.0 1.0
                    ]

                let cubeFaces =
                    [
                        [ 0; 3; 2; 1 ]
                        [ 4; 5; 6; 7 ]
                        [ 0; 1; 5; 4 ]
                        [ 3; 7; 6; 2 ]
                        [ 0; 4; 7; 3 ]
                        [ 1; 2; 6; 5 ]
                    ]

                let cubePoly: PolyhedronCell3D =
                    {
                        Site = 0
                        Position = Point3.zero
                        Weight = 0.0
                        Vertices = cubeVerts
                        Faces = cubeFaces
                        Volume = 8.0
                        Centroid = Point3.zero
                    }

                let polyMesh = GraphScene3D.buildPolyhedronCellMeshes scale 0.9 (fun _ -> RgbaColor.white) [ cubePoly ]
                Expect.equal polyMesh.Vertices.Length 24 "6 quad faces = 24 flat vertices"
                let polyWire = GraphScene3D.packPolyhedronWireframe scale 0.9 (fun _ -> RgbaColor.white) false [ cubePoly ]
                Expect.equal polyWire.Length 24 "deduplicated cube faces yield exact 12 edges = 24 LineVertex3D (not 48)"

                let vtWire = GraphScene3D.packVoronoiWireframe scale 0.9 (fun _ -> RgbaColor.white) false vtCells
                Expect.isGreaterThan vtWire.Length 0 "Voronoi wireframe emits deduplicated edge vertices"

            testCase "EmbeddedPoint3D, ClusteredPoint3D, and Octree wireframe adapters color clusters, noise, and depth cells"
            <| fun () ->
                let bounds3 = Bounds3.create -10.0 -10.0 -10.0 20.0 20.0 20.0
                let sc = Scale3D.ofBounds3 BoxAspect.Cube UpAxis.YUp bounds3

                let emb =
                    [
                        { EmbeddedPoint3D.Id = "e0"; Position = Point3.create 0.0 0.0 0.0 }
                        { EmbeddedPoint3D.Id = "e1"; Position = Point3.create 5.0 5.0 5.0 }
                    ]

                let eScatter = GraphScene3D.packEmbeddedScatter (fun _ -> RgbaColor (1.0f, 0.2f, 0.2f, 1.0f)) (fun _ -> 8.0f) (Set.singleton "e1") emb
                Expect.equal eScatter.Length 2 "embedded scatter count"
                Expect.isTrue eScatter.Span[1].Selected "selected embedded point"

                let clustered =
                    [
                        { ClusteredPoint3D.Item = "p0"; Id = "p0"; Position = Point3.create 0.0 0.0 0.0; ClusterId = Some 0; IsCore = true }
                        { ClusteredPoint3D.Item = "p1"; Id = "p1"; Position = Point3.create 1.0 0.0 0.0; ClusterId = Some 1; IsCore = false }
                        { ClusteredPoint3D.Item = "p2"; Id = "p2"; Position = Point3.create 9.0 9.0 9.0; ClusterId = None; IsCore = false }
                    ]

                let cScatter = GraphScene3D.packClusteredScatter (fun c -> if c.IsCore then 10.0f else 7.0f) (Set.singleton "p0") clustered
                Expect.equal cScatter.Length 3 "clustered scatter count"
                Expect.notEqual cScatter.Span[0].Color cScatter.Span[1].Color "distinct cluster IDs receive distinct Tableau10 colors"
                Expect.notEqual cScatter.Span[0].Color cScatter.Span[2].Color "noise gets muted slate instead of cluster 0 color"

                let pts = [ for i in 0 .. 19 -> Point3.create (float (i % 5) - 2.0) (float (i / 5) - 2.0) (float (i % 3) - 1.0) ]
                let oct = Octree.build id (fun _ -> 1.0) pts
                let cells = ResizeArray<OctCell<Point3>>()
                Octree.visit (fun c -> cells.Add c; true) oct
                let octWire = GraphScene3D.packOctreeWireframe sc (fun d -> RgbaColor (0.3f, 0.5f, float32 d * 0.1f, 0.8f)) cells
                Expect.equal octWire.Length (cells.Count * 24) "each octree cell emits 24 LineVertex3D"

            testCase "ReactiveGraph3D.bindDiagram, bindCodeCity, bindTreemap3D, bindVoronoiTreemap3D, and bindClusteredCloud integrate WgpuContext, PickRegistry3D, and PlotRevision3D"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 80, initialHeight = 80)
                use graph = new Graph ()
                let grouped = buildGroupedDiagram ()
                let bounds3 = Bounds3.create -10.0 -10.0 -10.0 20.0 20.0 20.0
                let layout0 = Force3D.layout { Force3DConfig.create bounds3 with Iterations = 10 } grouped |> unwrapOk "Force3D"
                let hier = buildSampleHierarchy ()
                let cityCfg = CodeCityConfig.create (Bounds.create -10.0 -10.0 20.0 20.0) 10.0 (fun _ -> 25.0)
                let buildings0 = CodeCity.layout cityCfg hier |> unwrapOk "CodeCity"
                let cuboids0 = Treemap3D.layout (Treemap3DConfig.create bounds3) hier |> unwrapOk "Treemap3D"
                let vtCells0 = VoronoiTreemap3D.layout 20 5e-2 bounds3 hier |> unwrapOk "VoronoiTreemap3D"
                let clustered0 =
                    [
                        { ClusteredPoint3D.Item = "c0"; Id = "c0"; Position = Point3.create 0.0 0.0 0.0; ClusterId = Some 0; IsCore = true }
                        { ClusteredPoint3D.Item = "c1"; Id = "c1"; Position = Point3.create 3.0 3.0 3.0; ClusterId = None; IsCore = false }
                    ]

                let mutable diagRev = Unchecked.defaultof<PlotRevision3D>
                let mutable regRef = Unchecked.defaultof<PickRegistry3D>

                let layoutSig, selSig, shrinkSig, diagBind, cityBind, tmBind, vtBind, cloudSink, sc =
                    graph.Run (fun () ->
                        let reg = PickRegistry3D ()
                        let rev = Reactive3D.createRevision ()
                        diagRev <- rev
                        regRef <- reg
                        let sc0 = GraphScene3D.scaleOfLayout BoxAspect.Cube UpAxis.YUp 0.05 layout0
                        let lSig = Api.createSignal layout0
                        let sSig = Api.createSignal Set.empty<string>
                        let shSig = Api.createSignal 0.9

                        let opts =
                            SinkOptions3D.defaultValue
                            |> SinkOptions3D.withRegistry reg
                            |> SinkOptions3D.withRevision rev

                        let dBind = ReactiveGraph3D.bindDiagram ctx opts GraphTheme3D.darkDefault (fun () -> sc0) (fun () -> sSig.Value) (fun () -> lSig.Value)
                        let cBind = ReactiveGraph3D.bindCodeCity ctx opts (fun _ -> RgbaColor.white) ShadingMode.Unlit (fun () -> Set.empty) (fun () -> sc0) (fun () -> buildings0)
                        let tBind = ReactiveGraph3D.bindTreemap3D ctx opts (fun _ -> RgbaColor.white) ShadingMode.Unlit true None (fun () -> sc0) (fun () -> cuboids0)
                        let vBind = ReactiveGraph3D.bindVoronoiTreemap3D ctx (opts |> SinkOptions3D.withPickId 0x05000001u) (fun () -> shSig.Value) (fun _ -> RgbaColor.white) (Some (fun _ -> RgbaColor.white)) ShadingMode.Unlit (fun () -> sc0) (fun () -> vtCells0)
                        let clSink = ReactiveGraph3D.bindClusteredCloud ctx opts (fun _ -> 8.0f) (fun () -> Set.empty) (fun () -> sc0) (fun () -> clustered0)
                        lSig, sSig, shSig, dBind, cBind, tBind, vBind, clSink, sc0)

                let initialRev = graph.Run (fun () -> diagRev.Peek)
                Expect.isGreaterThan initialRev 0 "ReactiveGraph3D binders mark PlotRevision3D dirty on initial upload"
                Expect.equal diagBind.NodeSink.ActiveCount layout0.Nodes.Length "bindDiagram uploads all nodes"
                Expect.equal diagBind.GroupBoxSink.ActiveCount layout0.Groups.Length "bindDiagram uploads group boxes"
                Expect.isTrue diagBind.GroupBoxSink.Batch.DrawBeforeLines "bindDiagram group boxes render before lines"
                Expect.equal cityBind.BuildingSink.ActiveCount 5 "bindCodeCity leaf count"
                Expect.equal cityBind.DistrictSink.ActiveCount 3 "bindCodeCity district count"
                Expect.equal tmBind.CuboidSink.ActiveCount 5 "bindTreemap3D leaf count"
                Expect.isGreaterThan vtBind.MeshBatch.Count 0 "bindVoronoiTreemap3D renders convex polyhedra mesh"
                Expect.equal cloudSink.ActiveCount 2 "bindClusteredCloud count"

                // Mutate selection and Voronoi shrink -> revision bumps automatically & picks resolve
                let revBefore = initialRev
                let revAfter =
                    graph.Run (fun () ->
                        selSig.Value <- Set.singleton layout0.Nodes.Head.Id
                        shrinkSig.Value <- 0.75
                        diagRev.Peek)

                Expect.isGreaterThan revAfter revBefore "mutations bump PlotRevision3D"

                // Test PickRegistry3D resolution for bound diagram node (slot 0 -> (layerId <<< 24) ||| 1u)
                let nodePick = { PickHit.PickId = (uint32 diagBind.NodeSink.LayerId <<< 24) ||| 1u; WorldPosition = Vector3.Zero }
                let resolvedNode = regRef.TryResolveKeyed<string> (sc, nodePick)
                Expect.isTrue resolvedNode.IsSome "diagram node pick resolves via PickRegistry3D"

            testCase "buildAssociationTubes and ReactiveGraph3D.bindDiagram skip self-loop and out-of-domain associations without dropping valid edge tubes"
            <| fun () ->
                let pNode (id: string) (pos: Point3) : PositionedNode3D =
                    {
                        Id = id
                        Kind = NodeKind.Standard
                        Center = pos
                        Bounds = Bounds3.create (pos.X - 0.5) (pos.Y - 0.5) (pos.Z - 0.5) 1.0 1.0 1.0
                        Radius = 0.5
                        ScaleFactor = 1.0
                        ParentGroup = None
                        Layer = None
                    }

                let mkAssoc (id: string) (s: string) (t: string) (backbone: bool) (pts: Point3 list) : RoutedAssociation3D =
                    {
                        Id = id
                        Source = { NodeId = s; PortId = None }
                        Target = { NodeId = t; PortId = None }
                        Kind = AssociationKind.Transition
                        IsBackbone = backbone
                        Points = Point3Chain.ofList pts
                        Waypoints = pts
                    }

                let posScale = Scale3D.create BoxAspect.Cube UpAxis.YUp (AxisScale3D.log 10.0 (1.0, 20.0)) (AxisScale3D.linear (-5.0, 5.0)) (AxisScale3D.linear (-5.0, 5.0))

                let layoutWithNoise: DiagramLayout3D =
                    {
                        Nodes = [ pNode "a" (Point3.create 2.0 0.0 0.0); pNode "b" (Point3.create 10.0 1.0 0.0) ]
                        Groups = []
                        Associations =
                            [
                                mkAssoc "valid" "a" "b" true [ Point3.create 2.0 0.0 0.0; Point3.create 10.0 1.0 0.0 ]
                                mkAssoc "self_loop" "a" "a" false [ Point3.create 2.0 0.0 0.0; Point3.create 2.0 0.0 0.0 ]
                                mkAssoc "out_of_domain" "a" "b" false [ Point3.create -5.0 0.0 0.0; Point3.create 10.0 1.0 0.0 ]
                            ]
                        Bounds = Bounds3.create 1.0 -5.0 -5.0 19.0 10.0 10.0
                    }

                let mixedTubeMesh =
                    GraphScene3D.buildAssociationTubes posScale 6 GraphTheme3D.darkDefault layoutWithNoise
                    |> unwrapOk "mixed buildAssociationTubes"

                Expect.isGreaterThan mixedTubeMesh.Vertices.Length 0 "valid edge still produces tube vertices alongside self-loop & omitted association"
                Expect.isGreaterThan mixedTubeMesh.Indices.Length 0 "valid edge still produces tube indices"

                let onlySelfLoopLayout: DiagramLayout3D =
                    { layoutWithNoise with
                        Associations = [ mkAssoc "self_loop" "a" "a" false [ Point3.create 2.0 0.0 0.0; Point3.create 2.0 0.0 0.0 ] ]
                    }

                let emptyTubeMesh =
                    GraphScene3D.buildAssociationTubes posScale 6 GraphTheme3D.darkDefault onlySelfLoopLayout
                    |> unwrapOk "self-loop-only buildAssociationTubes"

                Expect.equal emptyTubeMesh.Vertices.Length 0 "degenerate-only edges produce empty MeshData"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use graph = new Graph ()
                let tubeTheme = GraphTheme3D.darkDefault |> GraphTheme3D.withEdgeTubes true 6

                let dBind =
                    graph.Run (fun () ->
                        ReactiveGraph3D.bindDiagram ctx SinkOptions3D.defaultValue tubeTheme (fun () -> posScale) (fun () -> Set.empty) (fun () -> layoutWithNoise))

                Expect.isTrue dBind.EdgeTubeBatch.IsSome "EdgeTubeBatch created when UseEdgeTubes = true"
                Expect.isGreaterThan dBind.EdgeTubeBatch.Value.Count 0 "bindDiagram uploads edge tubes even with self-loop & out-of-domain associations"

                Expect.throwsT<ArgumentOutOfRangeException>
                    (fun () ->
                        graph.Run (fun () ->
                            ReactiveGraph3D.bindVoronoiTreemap3D
                                ctx
                                SinkOptions3D.defaultValue
                                (fun () -> 0.85)
                                (fun _ -> RgbaColor.white)
                                None
                                (ShadingMode.Lit (-0.1f, 0.0f))
                                (fun () -> posScale)
                                (fun () -> []))
                        |> ignore)
                    "bindVoronoiTreemap3D rejects negative roughness in ShadingMode.Lit"

                Expect.throwsT<ArgumentOutOfRangeException>
                    (fun () ->
                        graph.Run (fun () ->
                            ReactiveGraph3D.bindDiagram
                                ctx
                                SinkOptions3D.defaultValue
                                { GraphTheme3D.darkDefault with NodeShading = ShadingMode.Lit (0.2f, Single.NaN) }
                                (fun () -> posScale)
                                (fun () -> Set.empty)
                                (fun () -> layoutWithNoise))
                        |> ignore)
                    "bindDiagram rejects non-finite emissive in ShadingMode.Lit"
        ]

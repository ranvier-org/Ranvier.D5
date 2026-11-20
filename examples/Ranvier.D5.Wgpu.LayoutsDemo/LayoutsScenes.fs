namespace Ranvier.D5.Wgpu.LayoutsDemo

// Ruling L1: open Ranvier.D5.Graph3D BEFORE System.Numerics and Ranvier.D5.Wgpu so
// Quaternion resolves to float32 System.Numerics.Quaternion (.NET) / FableCompat3D.Quaternion (Fable).
// Ruling L2: keep this file 100% free of SDL, WebCore, System.IO, and System.Diagnostics imports.
open System
open System.Collections.Generic
open Ranvier
open Ranvier.Anime
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D
open System.Numerics
open Ranvier.D5.Wgpu
open Ranvier.D5.Graph3D.Wgpu
open Ranvier.Wgpu

/// <summary>Identifies one of the four 0.5.0 3D layout showcase scenes.</summary>
[<RequireQualifiedAccess; Struct>]
type LayoutSceneId =
    /// <summary>Scene 1: Poincaré H^3 hyperbolic graph with smooth Lorentz focus boosts.</summary>
    | PoincareH3
    /// <summary>Scene 2: Wettel-Lanza CodeCity morphing continuously into exploded CubeHilbert Treemap3D.</summary>
    | CodeCityTreemap3D
    /// <summary>Scene 3: Riemannian S^2 SphericalStress3D + Hierarchical/KDE 3D streamtube bundling + packet discs.</summary>
    | SphericalBundling3D
    /// <summary>Scene 4: Umap3D embedding + reactive Dbscan3D density clusters + Hilbert3D tour + Bvh3D raycast.</summary>
    | ManifoldClusterBvh3D

/// <summary>Display metadata, CLI parsing, and output capture filenames for <see cref="T:Ranvier.D5.Wgpu.LayoutsDemo.LayoutSceneId"/>.</summary>
[<RequireQualifiedAccess>]
module LayoutSceneId =
    /// <summary>All four scenes in display order.</summary>
    let all =
        [
            LayoutSceneId.PoincareH3
            LayoutSceneId.CodeCityTreemap3D
            LayoutSceneId.SphericalBundling3D
            LayoutSceneId.ManifoldClusterBvh3D
        ]

    /// <summary>Full display title for the HUD header.</summary>
    let title =
        function
        | LayoutSceneId.PoincareH3 -> "1. Poincaré H³ Hyperbolic Graph + Live Lorentz Focus"
        | LayoutSceneId.CodeCityTreemap3D -> "2. CodeCity ↔ CubeHilbert Treemap3D Morph"
        | LayoutSceneId.SphericalBundling3D -> "3. SphericalStress3D S² + Volumetric KDE Bundling"
        | LayoutSceneId.ManifoldClusterBvh3D -> "4. Umap3D + Reactive Dbscan3D + Hilbert3D + Bvh3D Raycast"

    /// <summary>Short toolbar label for the scene selector buttons.</summary>
    let shortLabel =
        function
        | LayoutSceneId.PoincareH3 -> "1·H³"
        | LayoutSceneId.CodeCityTreemap3D -> "2·City"
        | LayoutSceneId.SphericalBundling3D -> "3·S²"
        | LayoutSceneId.ManifoldClusterBvh3D -> "4·UMAP"

    /// <summary>Canonical capture filename required by `--capture-all`.</summary>
    let captureFileName =
        function
        | LayoutSceneId.PoincareH3 -> "wgpu-layout-h3-poincare.bmp"
        | LayoutSceneId.CodeCityTreemap3D -> "wgpu-layout-codecity-treemap3d.bmp"
        | LayoutSceneId.SphericalBundling3D -> "wgpu-layout-spherical-bundling3d.bmp"
        | LayoutSceneId.ManifoldClusterBvh3D -> "wgpu-layout-umap-dbscan-bvh3d.bmp"

    /// <summary>Parses a CLI scene token (`h3`, `codecity`, `spherical`, `manifold`, or `1`..`4`).</summary>
    let tryParse (text: string) : LayoutSceneId option =
        if isNull text then
            None
        else
            match text.Trim().ToLowerInvariant () with
            | "1" | "h3" | "poincare" | "hyperbolic" -> Some LayoutSceneId.PoincareH3
            | "2" | "codecity" | "treemap" | "treemap3d" -> Some LayoutSceneId.CodeCityTreemap3D
            | "3" | "spherical" | "bundling" | "s2" -> Some LayoutSceneId.SphericalBundling3D
            | "4" | "manifold" | "umap" | "dbscan" | "bvh" -> Some LayoutSceneId.ManifoldClusterBvh3D
            | _ -> None

/// <summary>Immutable 3D frame payload consumed by native `PlotView3D` or browser WebGPU hosts.</summary>
type ActiveLayoutPass3D =
    {
        AxisSpec: Axis3DSpec
        Batches: ReadOnlyMemory<InstanceBatch>
        Lines: ReadOnlyMemory<LineBatch>
        Discs: ReadOnlyMemory<DiscBatch>
        Section: SectionPlane voption
        Subtitle: string
        Detail: string
    }

module private LayoutsHelpers =
    let unwrapOk (label: string) (res: Result<'T, LayoutError list>) : 'T =
        match res with
        | Ok v -> v
        | Error errs -> failwithf "%s failed: %A" label errs

    let smoothstep (x: float) : float =
        let t = Math.Clamp (x, 0.0, 1.0)
        t * t * (3.0 - 2.0 * t)

    let lerpColor (a: RgbaColor) (b: RgbaColor) (t: float32) : RgbaColor =
        let u = Math.Clamp (t, 0.0f, 1.0f)
        RgbaColor (
            a.R + (b.R - a.R) * u,
            a.G + (b.G - a.G) * u,
            a.B + (b.B - a.B) * u,
            a.A + (b.A - a.A) * u
        )

    let hexRgba (hex: int) : Rgba =
        ColorSpace.rgba
            (float ((hex >>> 16) &&& 255) / 255.0)
            (float ((hex >>> 8) &&& 255) / 255.0)
            (float (hex &&& 255) / 255.0)
            1.0

    let plasmaRamp (domain: float * float) : ColorRamp =
        let plasmaPalette =
            [
                0x0d0887
                0x46039f
                0x7201a8
                0x9c179e
                0xbd3786
                0xd8576b
                0xed7953
                0xfb9f3a
                0xfdca26
                0xf0f921
            ]
            |> List.map hexRgba

        ColorRamp.sequential (Scale.linear domain (0.0, 1.0)) plasmaPalette ColorClipPolicy.clampAndOmit

    let buildAxisSpec (scale: Scale3D) (xTitle: string) (yTitle: string) (zTitle: string) (tickCount: int) : Axis3DSpec =
        let baseSpec = Axis3D.defaultSpec scale
        { baseSpec with
            X = { baseSpec.X with Title = xTitle; TickCount = tickCount }
            Y = { baseSpec.Y with Title = yTitle; TickCount = tickCount }
            Z = { baseSpec.Z with Title = zTitle; TickCount = tickCount }
        }

    let makeSphereGlyph (pos: Point3) (radius: float32) (color: RgbaColor) (selected: bool) (emissive: float32) : GlyphDatum3D =
        GlyphDatum3D.sphere pos radius color
        |> GlyphDatum3D.withShading (ShadingMode.Lit (0.24f, emissive))
        |> GlyphDatum3D.withSelected selected

type private SphericalEdgePre =
    {
        Id: string
        SourceId: string
        TargetId: string
        IsBackbone: bool
        StartColor: RgbaColor
        EndColor: RgbaColor
        Unbundled: Point3[]
        Bundled: Point3[]
    }

/// <summary>
/// Dual-target (.NET 10 + Fable/Fickl) 4-scene controller showcasing Ranvier.D5 0.5.0 &amp; Ranvier.D5.Graph3D 0.5.0
/// driven incrementally through Ranvier KeyedCollection, Projection, Reactive3D, and GraphScene3D GPU sinks.
/// </summary>
type LayoutsStage3D(wgpu: WgpuContext) =
    let plotRevision = Reactive3D.createRevision ()
    let pickRegistry = PickRegistry3D ()

    // =========================================================================
    // Scene 1: Poincare H^3 Hyperbolic Graph + Live Lorentz Focus
    // =========================================================================
    let h3Scale =
        Scale3D.create
            BoxAspect.Cube
            UpAxis.YUp
            (AxisScale3D.linear (-1.08, 1.08))
            (AxisScale3D.linear (-1.08, 1.08))
            (AxisScale3D.linear (-1.08, 1.08))

    let h3AxisSpec = LayoutsHelpers.buildAxisSpec h3Scale "Poincaré X" "Poincaré Y" "Poincaré Z" 5
    let h3Ramp = LayoutsHelpers.plasmaRamp (0.0, 1.0)

    let h3Graph, h3TourNodes =
        let nodes = ResizeArray<DiagramNode>()
        let assocs = ResizeArray<Association>()
        let tour = ResizeArray<string>()

        nodes.Add (DiagramNode.sized "root" 18.0 18.0)
        tour.Add "root"

        for b = 0 to 5 do
            let bId = $"b{b}"
            nodes.Add (DiagramNode.sized bId 14.0 14.0)
            assocs.Add (Association.create $"e_root_{bId}" "root" bId)

            for s = 0 to 2 do
                let sId = $"{bId}_s{s}"
                nodes.Add (DiagramNode.sized sId 11.0 11.0)
                assocs.Add (Association.create $"e_{bId}_{sId}" bId sId)

                if s = 1 then
                    tour.Add sId

                let leafCount = if s < 2 then 3 else 2

                for l = 0 to leafCount - 1 do
                    let lId = $"{sId}_l{l}"
                    nodes.Add (DiagramNode.sized lId 8.5 8.5)
                    assocs.Add (Association.create $"e_{sId}_{lId}" sId lId)

        // Add deterministic cross-links across branches so circular-arc geodesics shine
        for b = 0 to 5 do
            let nextB = (b + 1) % 6
            let skipB = (b + 2) % 6
            assocs.Add (Association.create $"x_sub_{b}" $"b{b}_s2" $"b{nextB}_s0")
            assocs.Add (Association.create $"x_leaf_{b}" $"b{b}_s0_l1" $"b{nextB}_s1_l0")
            assocs.Add (Association.create $"x_far_{b}" $"b{b}_s1_l2" $"b{skipB}_s2_l1")

        let g = DiagramGraph.simple (Seq.toList nodes) (Seq.toList assocs) |> LayoutsHelpers.unwrapOk "H3.graph"
        g, tour.ToArray ()

    let h3BaseState =
        H3.buildState
            { H3Config.defaultValue with
                Model = HyperbolicModel.Poincare
                BallRadius = 1.0
                PoincareArcSegments = 12
                RootId = Some "root" }
            h3Graph
        |> LayoutsHelpers.unwrapOk "H3.buildState"

    let h3NodeMinkowski =
        let map = Dictionary<string, MinkowskiPoint>(StringComparer.Ordinal)

        for n in DiagramGraph.nodes h3Graph do
            match H3.tryNodeMinkowski n.Id h3BaseState with
            | Some pt -> map[n.Id] <- pt
            | None -> ()

        map

    let h3Coll = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
    let h3Sink =
        let rows = h3Coll.Rows |> Projection.map (fun struct (_, g) -> g)

        let opts =
            SinkOptions3D.defaultValue
            |> SinkOptions3D.withLayerId (pickRegistry.AllocateLayer ())
            |> SinkOptions3D.withRevision plotRevision
            |> SinkOptions3D.withBlendMode BlendMode.Opaque
            |> SinkOptions3D.withDoubleSided false

        Reactive3D.bindGlyphs wgpu GlyphShape3D.Sphere opts (fun () -> h3Scale) rows

    let h3LineBatch = wgpu.CreateLineBatch ()
    let h3Batches = ReadOnlyMemory<InstanceBatch> [| h3Sink.Batch |]
    let h3Lines = ReadOnlyMemory<LineBatch> [| h3LineBatch |]

    let h3MeridianVerts =
        let cage = Line3D.sphereCage h3Scale Point3.zero 1.0 1 64 (RgbaColor (0.36f, 0.50f, 0.74f, 0.34f)) false
        cage.Span.ToArray ()

    let mutable h3CurrentFocusPt = Hyperbolic3D.origin
    let mutable h3FocusNodeId = "root"
    let mutable h3ManualFromPt = Hyperbolic3D.origin
    let mutable h3ManualTargetPt = Hyperbolic3D.origin
    let mutable h3ManualTargetId : string option = None
    let mutable h3ManualElapsed = 0.0
    let mutable h3ManualSettled = false
    let mutable lastSceneT = 0.0
    let mutable h3TourPhaseOffset = 0.0
    let mutable h3Detail = "Click any node to boost it isometrically to the Poincaré center"

    let applyPoincareFocus (focusPt: MinkowskiPoint) (highlightId: string) =
        h3CurrentFocusPt <- focusPt
        let boost = HyperbolicTransform.boostToOrigin focusPt
        let viewState = H3.withViewTransform boost h3BaseState
        let proj = H3.project viewState
        let nodeScale = Dictionary<string, float>(StringComparer.Ordinal)

        Api.batch (fun () ->
            for nd in proj.Nodes do
                nodeScale[nd.Id] <- nd.ScaleFactor
                let isFocused = nd.Id = highlightId
                let r = float32 (0.013 + 0.048 * Math.Pow (Math.Clamp (nd.ScaleFactor, 0.0, 1.0), 0.60))
                let effR = if isFocused then max 0.068f (r * 1.22f) else r
                let radialNorm = Math.Clamp (Point3.distance (Point3.create 0.0 0.0 0.0) nd.Center, 0.0, 1.0)
                let colorMetric = Math.Clamp (1.0 - radialNorm * 0.82, 0.06, 0.98)
                let baseCol = ColorRamp.mapOrFallback h3Ramp colorMetric RgbaColor.white
                let col = if isFocused then RgbaColor (1.0f, 0.96f, 0.68f, 1.0f) else baseCol
                let emissive = if isFocused then 0.35f else 0.12f
                h3Coll.AddOrUpdate (struct (nd.Id, LayoutsHelpers.makeSphereGlyph nd.Center effR col isFocused emissive)))

        let lineVerts = ResizeArray<LineVertex3D>(h3MeridianVerts.Length + proj.Associations.Length * 26)
        lineVerts.AddRange h3MeridianVerts

        let cyanCol = RgbaColor (0.22f, 0.96f, 1.0f, 0.98f)
        let goldCol = RgbaColor (1.0f, 0.85f, 0.32f, 0.98f)
        let crossCol = RgbaColor (0.58f, 0.50f, 0.98f, 0.60f)

        for assoc in proj.Associations do
            let pts = assoc.Points
            let len = pts.Length

            if len >= 2 then
                let sAtt =
                    match nodeScale.TryGetValue assoc.Source.NodeId with
                    | true, v -> v
                    | _ -> 0.5

                let tAtt =
                    match nodeScale.TryGetValue assoc.Target.NodeId with
                    | true, v -> v
                    | _ -> 0.5

                let col =
                    if assoc.IsBackbone then
                        let blend = float32 (Math.Clamp (1.0 - 0.5 * (sAtt + tAtt), 0.0, 1.0))
                        LayoutsHelpers.lerpColor goldCol cyanCol blend
                    else
                        let vis = float32 (Math.Clamp (0.28 + 0.55 * max sAtt tAtt, 0.20, 0.72))
                        RgbaColor (crossCol.R, crossCol.G, crossCol.B, vis)

                let vCol = col.ToVector4 ()

                for k = 0 to len - 2 do
                    let wA = Scale3D.map h3Scale pts[k]
                    let wB = Scale3D.map h3Scale pts[k + 1]
                    lineVerts.Add (LineVertex3D (wA, vCol))
                    lineVerts.Add (LineVertex3D (wB, vCol))

        h3LineBatch.UpdateVertices (lineVerts.ToArray ())
        plotRevision.MarkDirty ()

    let interpMinkowski (a: MinkowskiPoint) (b: MinkowskiPoint) (u: float) : MinkowskiPoint =
        Hyperbolic3D.slerp a b (LayoutsHelpers.smoothstep u)

    let stepPoincareInternal (t: float) =
        let dt = if t >= lastSceneT then t - lastSceneT else 0.0
        lastSceneT <- t

        match h3ManualTargetId with
        | Some targetId ->
            if not h3ManualSettled then
                h3ManualElapsed <- h3ManualElapsed + dt
                let u = min 1.0 (h3ManualElapsed / 0.65)
                let pt = interpMinkowski h3ManualFromPt h3ManualTargetPt u
                h3FocusNodeId <- targetId
                applyPoincareFocus pt targetId

                if u >= 1.0 then
                    h3ManualSettled <- true
        | None ->
            let segDuration = 1.65
            let count = h3TourNodes.Length
            let tourPeriod = float count * segDuration
            let raw = t + h3TourPhaseOffset
            let wrapped = ((raw % tourPeriod) + tourPeriod) % tourPeriod
            let scaled = wrapped / segDuration
            let segIdx = int (floor scaled) % count
            let nextIdx = (segIdx + 1) % count
            let frac = scaled - floor scaled
            // Hold briefly on each hub before boosting smoothly to the next
            let moveU = if frac < 0.25 then 0.0 else (frac - 0.25) / 0.75
            let idA = h3TourNodes[segIdx]
            let idB = h3TourNodes[nextIdx]
            let pA = h3NodeMinkowski[idA]
            let pB = h3NodeMinkowski[idB]
            let focusPt = interpMinkowski pA pB moveU
            let activeId = if moveU > 0.5 then idB else idA
            h3FocusNodeId <- activeId
            applyPoincareFocus focusPt activeId

    do applyPoincareFocus Hyperbolic3D.origin "root"

    // =========================================================================
    // Scene 2: CodeCity <-> Exploded CubeHilbert Treemap3D Morph + VoronoiTreemap3D
    // =========================================================================
    let districtPalettes =
        dict [
            "Compiler", RgbaColor (0.20f, 0.85f, 0.96f, 1.0f)
            "Runtime", RgbaColor (0.22f, 0.90f, 0.58f, 1.0f)
            "Graphics", RgbaColor (0.98f, 0.58f, 0.22f, 1.0f)
            "Layouts", RgbaColor (0.74f, 0.45f, 0.98f, 1.0f)
        ]

    let archSpec : (string * (string * float * float) list) list =
        [
            "Compiler",
            [
                "Lexer", 18.0, 26.0
                "Parser", 34.0, 68.0
                "SyntaxTree", 26.0, 42.0
                "TypeInfer", 44.0, 92.0
                "TypeCheck", 38.0, 84.0
                "Overloads", 28.0, 58.0
                "PatternMatch", 30.0, 74.0
                "Lowering", 32.0, 62.0
                "Optimizer", 40.0, 88.0
                "CodeGen", 36.0, 79.0
                "Emitter", 22.0, 46.0
            ]
            "Runtime",
            [
                "GraphOwner", 28.0, 54.0
                "SignalCore", 32.0, 70.0
                "MemoEval", 36.0, 82.0
                "Selector", 20.0, 44.0
                "KeyedColl", 42.0, 90.0
                "Projection", 38.0, 86.0
                "BatchQueue", 22.0, 48.0
                "Scheduler", 30.0, 64.0
                "AsyncFlight", 26.0, 58.0
                "ScopeCleanup", 18.0, 34.0
            ]
            "Graphics",
            [
                "WgpuDevice", 30.0, 62.0
                "SwapChain", 22.0, 40.0
                "RenderPass", 34.0, 72.0
                "UniformLut", 24.0, 50.0
                "MeshUpload", 28.0, 56.0
                "LineBatch", 20.0, 45.0
                "DiscBatch", 26.0, 60.0
                "PickPass", 32.0, 76.0
                "SectionCut", 18.0, 38.0
                "AxisFrame", 40.0, 85.0
                "ColorShader", 36.0, 78.0
            ]
            "Layouts",
            [
                "PoincareH3", 46.0, 96.0
                "WalrusH3", 32.0, 68.0
                "Treemap3D", 38.0, 80.0
                "CodeCity", 28.0, 56.0
                "SphereS2", 34.0, 74.0
                "KdeBundle", 42.0, 88.0
                "Umap3D", 44.0, 94.0
                "Dbscan3D", 26.0, 52.0
                "Hilbert3D", 24.0, 48.0
                "Bvh3D", 30.0, 66.0
            ]
        ]

    let moduleDistrict = Dictionary<string, string>(StringComparer.Ordinal)
    let moduleHeightMetric = Dictionary<string, float>(StringComparer.Ordinal)

    let archHierarchy =
        Hierarchy.node
            "Architecture"
            0.0
            [
                for distName, mods in archSpec do
                    yield
                        Hierarchy.node
                            distName
                            0.0
                            [
                                for modName, areaW, compH in mods do
                                    moduleDistrict[modName] <- distName
                                    moduleHeightMetric[modName] <- compH
                                    yield Hierarchy.node modName areaW []
                            ]
            ]

    let voronoiDistrictHierarchy =
        Hierarchy.node
            "Architecture"
            0.0
            [
                for distName, mods in archSpec ->
                    Hierarchy.node distName (mods |> List.sumBy (fun (_, areaW, _) -> areaW)) []
            ]

    let cubeHilbertBounds = Bounds3.create -0.95 0.08 -0.95 1.90 1.84 1.90

    let hilbertCuboids =
        Treemap3D.layout
            { Treemap3DConfig.create cubeHilbertBounds with
                Tiling = Treemap3DTiling.CubeHilbert
                ExplodeFactor = 0.16
                Padding = 0.04 }
            archHierarchy
        |> LayoutsHelpers.unwrapOk "Treemap3D.CubeHilbert"

    let hilbertLeafById =
        let m = Dictionary<string, Bounds3>(StringComparer.Ordinal)

        for c in hilbertCuboids do
            if c.IsLeaf then
                m[c.Id] <- c.Bounds

        m

    let hilbertGroups = hilbertCuboids |> List.filter (fun c -> not c.IsLeaf)

    let voronoiCells =
        VoronoiTreemap3D.layout 16 2e-2 cubeHilbertBounds voronoiDistrictHierarchy
        |> LayoutsHelpers.unwrapOk "VoronoiTreemap3D.layout"

    let mutable cityBuildings : CityBuilding<string> list = []
    let mutable currentMorphT = 0.0
    let mutable cityManualMorph : float option = None
    let mutable cityMorphPhaseOffset = 0.0
    let mutable selectedBuildingId : string option = None
    let mutable cityDetail = "Click a module tower to boost complexity | [ / ] morphs CodeCity ↔ CubeHilbert"

    let resetModuleMetrics () =
        for _, mods in archSpec do
            for modName, _, compH in mods do
                moduleHeightMetric[modName] <- compH

    let recomputeCodeCity () =
        let footprint = Bounds.create -0.95 -0.95 1.90 1.90

        let cfg =
            { CodeCityConfig.create footprint 1.45 (fun id ->
                  match moduleHeightMetric.TryGetValue id with
                  | true, h -> h
                  | _ -> 20.0) with
                DistrictPlinthHeight = 0.08
                StreetPadding = 0.045 }

        cityBuildings <- CodeCity.layout cfg archHierarchy |> LayoutsHelpers.unwrapOk "CodeCity.layout"

    do recomputeCodeCity ()

    // GraphScene3D.scaleOfCodeCity preserves CodeCity's native YUp orientation with zero Y <-> Z swaps.
    let cityFitScale = GraphScene3D.scaleOfCodeCity cityBuildings

    do
        let yUpProbe = Scale3D.map cityFitScale (Point3.create 0.0 1.2 0.0)
        if yUpProbe.Y <= 0.0f then failwith "Expected GraphScene3D.scaleOfCodeCity to map +Y elevation upward."

    let cityScale = Scale3D.cubeYUp (-1.05, 1.05) (0.0, 2.05) (-1.05, 1.05)

    let cityAxisSpec = LayoutsHelpers.buildAxisSpec cityScale "City X (LOC)" "Elevation / Hilbert Y" "City Z (Depth)" 5

    let cityColl = Api.createKeyedCollection (fun struct (k: string, _: BoxDatum3D) -> k)
    let citySink =
        let rows = cityColl.Rows |> Projection.map (fun struct (_, b) -> b)

        let opts =
            SinkOptions3D.defaultValue
            |> SinkOptions3D.withLayerId (pickRegistry.AllocateLayer ())
            |> SinkOptions3D.withRevision plotRevision

        Reactive3D.bindBoxes wgpu opts (fun () -> cityScale) rows

    let cityVoronoiMesh =
        let voronoiColor (cell: VoronoiTreemap3DCell<string>) =
            match districtPalettes.TryGetValue cell.Id with
            | true, c -> RgbaColor (c.R, c.G, c.B, 0.11f)
            | _ -> RgbaColor (0.35f, 0.48f, 0.68f, 0.09f)

        GraphScene3D.buildVoronoiCellMeshes cityScale 0.90 voronoiColor voronoiCells
        |> wgpu.UploadMesh

    let cityVoronoiBatch = wgpu.CreateSingleBatch (cityVoronoiMesh, BlendMode.AlphaBlend, false, drawBeforeLines = true)

    do cityVoronoiBatch.SetSingle (Instance3D.fromMatrix Matrix4x4.Identity RgbaColor.white 0.5f 0u InstanceFlags.Unlit 0.0f)

    let cityWireBatch = wgpu.CreateLineBatch ()
    let cityBatches = ReadOnlyMemory<InstanceBatch> [| citySink.Batch; cityVoronoiBatch |]
    let cityLines = ReadOnlyMemory<LineBatch> [| cityWireBatch |]

    let colorForCityItem (isSelected: bool) (b: CityBuilding<string>) : RgbaColor =
        if b.IsDistrict then
            match districtPalettes.TryGetValue b.Id with
            | true, c -> RgbaColor (c.R * 0.34f, c.G * 0.34f, c.B * 0.38f, 1.0f)
            | _ -> RgbaColor (0.14f, 0.18f, 0.26f, 1.0f)
        elif isSelected then
            RgbaColor (1.0f, 0.92f, 0.45f, 1.0f)
        else
            let distName = moduleDistrict[b.Id]
            let distCol = districtPalettes[distName]
            let hFrac = float32 (Math.Clamp (b.HeightMetric / 110.0, 0.25, 1.0))
            RgbaColor (
                Math.Clamp (distCol.R * (0.55f + 0.50f * hFrac), 0.0f, 1.0f),
                Math.Clamp (distCol.G * (0.55f + 0.50f * hFrac), 0.0f, 1.0f),
                Math.Clamp (distCol.B * (0.55f + 0.50f * hFrac), 0.0f, 1.0f),
                1.0f
            )

    let lerpBounds3 (a: Bounds3) (b: Bounds3) (t: float) : Bounds3 =
        let x0 = Bounds3.minX a + (Bounds3.minX b - Bounds3.minX a) * t
        let y0 = Bounds3.minY a + (Bounds3.minY b - Bounds3.minY a) * t
        let z0 = Bounds3.minZ a + (Bounds3.minZ b - Bounds3.minZ a) * t
        let sx = Bounds3.sizeX a + (Bounds3.sizeX b - Bounds3.sizeX a) * t
        let sy = Bounds3.sizeY a + (Bounds3.sizeY b - Bounds3.sizeY a) * t
        let sz = Bounds3.sizeZ a + (Bounds3.sizeZ b - Bounds3.sizeZ a) * t
        Bounds3.create x0 y0 z0 (max 0.012 sx) (max 0.008 sy) (max 0.012 sz)

    let syncCityVisuals (morphT: float) =
        let u = LayoutsHelpers.smoothstep (Math.Clamp (morphT, 0.0, 1.0))
        currentMorphT <- Math.Clamp (morphT, 0.0, 1.0)
        let selSet = match selectedBuildingId with Some id -> Set.singleton id | None -> Set.empty

        if u <= 1e-4 then
            let struct (leafBoxes, distBoxes) =
                GraphScene3D.packCodeCity
                    (fun b -> colorForCityItem (Set.contains b.Id selSet) b)
                    (ShadingMode.Lit (0.28f, 0.06f))
                    selSet
                    cityBuildings

            Api.batch (fun () ->
                let leafSpan = leafBoxes.Span
                let distSpan = distBoxes.Span
                let mutable li = 0
                let mutable di = 0

                for b in cityBuildings do
                    if b.IsDistrict then
                        if di < distSpan.Length then
                            cityColl.AddOrUpdate (struct ($"plinth:{b.Id}", distSpan[di]))
                            di <- di + 1
                    else
                        if li < leafSpan.Length then
                            cityColl.AddOrUpdate (struct ($"mod:{b.Id}", leafSpan[li]))
                            li <- li + 1)
        else
            Api.batch (fun () ->
                for b in cityBuildings do
                    if b.IsDistrict then
                        let key = $"plinth:{b.Id}"
                        let baseBox = b.Bounds
                        let heightScale = max 0.05 (1.0 - u)
                        let morphedBox =
                            Bounds3.create
                                (Bounds3.minX baseBox)
                                (Bounds3.minY baseBox * (1.0 - u * 0.85))
                                (Bounds3.minZ baseBox)
                                (Bounds3.sizeX baseBox)
                                (Bounds3.sizeY baseBox * heightScale)
                                (Bounds3.sizeZ baseBox)

                        let plinthDatum =
                            BoxDatum3D.create morphedBox (colorForCityItem false b)
                            |> BoxDatum3D.withShading (ShadingMode.Lit (0.28f, 0.03f))

                        cityColl.AddOrUpdate (struct (key, plinthDatum))
                    else
                        let key = $"mod:{b.Id}"
                        let hBox = hilbertLeafById[b.Id]
                        let morphed = lerpBounds3 b.Bounds hBox u
                        let isSel = Set.contains b.Id selSet
                        let em = if isSel then 0.28f else 0.06f

                        let modDatum =
                            BoxDatum3D.create morphed (colorForCityItem isSel b)
                            |> BoxDatum3D.withShading (ShadingMode.Lit (0.28f, em))
                            |> BoxDatum3D.withSelected isSel

                        cityColl.AddOrUpdate (struct (key, modDatum)))

        // Draw exploded CubeHilbert district wireframe envelopes & VoronoiTreemap3D cell cages fading with morphT
        let wireAlpha = float32 (0.14 + 0.58 * u)

        let treemapWire =
            GraphScene3D.packTreemapWireframe
                cityScale
                (fun grp ->
                    match districtPalettes.TryGetValue grp.Id with
                    | true, c -> RgbaColor (c.R, c.G, c.B, wireAlpha)
                    | _ -> RgbaColor (0.45f, 0.56f, 0.72f, wireAlpha * 0.5f))
                false
                hilbertGroups

        let voronoiWire =
            GraphScene3D.packVoronoiWireframe
                cityScale
                0.90
                (fun cell ->
                    match districtPalettes.TryGetValue cell.Id with
                    | true, c -> RgbaColor (c.R, c.G, c.B, float32 (0.18 + 0.24 * u))
                    | _ -> RgbaColor (0.45f, 0.56f, 0.72f, 0.18f))
                false
                voronoiCells

        let combinedWire = ResizeArray<LineVertex3D>(treemapWire.Length + voronoiWire.Length)
        combinedWire.AddRange (treemapWire.Span.ToArray ())
        combinedWire.AddRange (voronoiWire.Span.ToArray ())
        cityWireBatch.UpdateVertices (combinedWire.ToArray ())
        plotRevision.MarkDirty ()

    do syncCityVisuals 0.0

    // =========================================================================
    // Scene 3: SphericalStress3D S^2 + HierarchicalBundle3D / KdeBundle3D + Tubes + Discs
    // =========================================================================
    let sphScale =
        Scale3D.create
            BoxAspect.Cube
            UpAxis.YUp
            (AxisScale3D.linear (-1.20, 1.20))
            (AxisScale3D.linear (-1.20, 1.20))
            (AxisScale3D.linear (-1.20, 1.20))

    let sphAxisSpec = LayoutsHelpers.buildAxisSpec sphScale "Riemannian X" "Riemannian Y" "Riemannian Z" 5

    let regionNames = [| "Americas"; "Europe"; "Asia"; "Africa"; "Oceania" |]

    let regionColors =
        [|
            RgbaColor (0.22f, 0.88f, 0.98f, 1.0f) // Americas: cyan
            RgbaColor (0.38f, 0.92f, 0.52f, 1.0f) // Europe: emerald
            RgbaColor (0.98f, 0.66f, 0.24f, 1.0f) // Asia: amber
            RgbaColor (0.95f, 0.36f, 0.62f, 1.0f) // Africa: rose
            RgbaColor (0.72f, 0.52f, 0.98f, 1.0f) // Oceania: violet
        |]

    let hubsPerRegion = 7

    let sphHierarchy, sphGraph, hubRegionIdx, hubDegree =
        let hubRegion = Dictionary<string, int>(StringComparer.Ordinal)
        let degMap = Dictionary<string, int>(StringComparer.Ordinal)
        let groups = [ for r in regionNames -> DiagramGroup.create r ]
        let nodes = ResizeArray<DiagramNode>(35)
        let assocs = ResizeArray<Association>(90)
        let mutable edgeIdx = 0

        let addEdge (u: string) (v: string) =
            assocs.Add (Association.create $"se_{edgeIdx}" u v)
            edgeIdx <- edgeIdx + 1
            degMap[u] <- (match degMap.TryGetValue u with true, d -> d | _ -> 0) + 1
            degMap[v] <- (match degMap.TryGetValue v with true, d -> d | _ -> 0) + 1

        let hubId (r: int) (k: int) = $"h_{r}_{k}"

        let tree =
            Hierarchy.node
                "Global"
                0.0
                [
                    for r = 0 to regionNames.Length - 1 do
                        let rName = regionNames[r]

                        yield
                            Hierarchy.node
                                rName
                                0.0
                                [
                                    for k = 0 to hubsPerRegion - 1 do
                                        let id = hubId r k
                                        hubRegion[id] <- r
                                        nodes.Add (DiagramNode.sized id 0.08 0.08 |> DiagramNode.inGroup rName)
                                        yield Hierarchy.node id 1.0 []
                                ]
                ]

        // Dense intra-regional local clique-ring so SphericalStress3D spreads all 5 regions evenly over S^2
        for r = 0 to regionNames.Length - 1 do
            for i = 0 to hubsPerRegion - 1 do
                for j = i + 1 to hubsPerRegion - 1 do
                    if (j - i) <= 2 || i = 0 then
                        addEdge (hubId r i) (hubId r j)

        // Inter-regional gateway trunk bundles across adjacent continents
        for r = 0 to regionNames.Length - 1 do
            let nextR = (r + 1) % regionNames.Length
            addEdge (hubId r 0) (hubId nextR 0)
            addEdge (hubId r 1) (hubId nextR 1)
            addEdge (hubId r 0) (hubId nextR 2)

        let g = DiagramGraph.create (Seq.toList nodes) groups (Seq.toList assocs) |> LayoutsHelpers.unwrapOk "Spherical.graph"
        tree, g, hubRegion, degMap

    let sphStressLayout =
        SphericalStress3D.layout
            { SphericalLayoutConfig.create (Point3.create 0.0 0.0 0.0) 1.05 with
                Iterations = 65
                ArcSegments = 14
                Seed = 42UL }
            sphGraph
        |> LayoutsHelpers.unwrapOk "SphericalStress3D"

    let sphHebLayout =
        HierarchicalBundle3D.bundleGraph 0.78 14 sphGraph sphStressLayout
        |> LayoutsHelpers.unwrapOk "HierarchicalBundle3D.bundleGraph"

    let sphKdeLayout =
        KdeBundle3D.bundle
            { KdeBundle3DConfig.defaultValue with
                GridSize = 36
                Iterations = 8
                SamplePointsPerEdge = 15 }
            sphHebLayout
        |> LayoutsHelpers.unwrapOk "KdeBundle3D"

    let sphEdgesPre =
        let unbMap = Dictionary<string, RoutedAssociation3D>(StringComparer.Ordinal)

        for a in sphStressLayout.Associations do
            unbMap[a.Id] <- a

        [|
            for b in sphKdeLayout.Associations do
                match unbMap.TryGetValue b.Id with
                | true, u ->
                    let count = min u.Points.Length b.Points.Length
                    let unbPts = Array.init count (fun i -> u.Points[i])
                    let bndPts = Array.init count (fun i -> b.Points[i])
                    let cSrc = regionColors[hubRegionIdx[b.Source.NodeId]]
                    let cDst = regionColors[hubRegionIdx[b.Target.NodeId]]

                    yield
                        {
                            Id = b.Id
                            SourceId = b.Source.NodeId
                            TargetId = b.Target.NodeId
                            IsBackbone = b.IsBackbone
                            StartColor = cSrc
                            EndColor = cDst
                            Unbundled = unbPts
                            Bundled = bndPts
                        }
                | _ -> ()
        |]

    let sphHubColl = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
    let sphHubSink =
        let rows = sphHubColl.Rows |> Projection.map (fun struct (_, g) -> g)

        let opts =
            SinkOptions3D.defaultValue
            |> SinkOptions3D.withLayerId (pickRegistry.AllocateLayer ())
            |> SinkOptions3D.withRevision plotRevision
            |> SinkOptions3D.withBlendMode BlendMode.Opaque
            |> SinkOptions3D.withDoubleSided false

        Reactive3D.bindGlyphs wgpu GlyphShape3D.Sphere opts (fun () -> sphScale) rows

    let sphPacketColl = Api.createKeyedCollection (fun struct (k: string, _: ScatterDatum3D) -> k)
    let sphPacketSink =
        let rows = sphPacketColl.Rows |> Projection.map (fun struct (_, d) -> d)

        let opts =
            SinkOptions3D.defaultValue
            |> SinkOptions3D.withLayerId (pickRegistry.AllocateLayer ())
            |> SinkOptions3D.withRevision plotRevision
            |> SinkOptions3D.withPreserveOrder true

        Reactive3D.bindDiscsWith wgpu opts (fun () -> sphScale) rows

    let sphWireBatch = wgpu.CreateLineBatch ()

    do
        let shellCol = RgbaColor (0.28f, 0.38f, 0.56f, 0.25f)
        let cageVerts = Line3D.sphereCage sphScale Point3.zero 1.05 1 64 shellCol false
        sphWireBatch.UpdateVertices cageVerts.Span

    let buildSphericalTubesMesh (bundleBlend: float) (selectedHub: string option) : MeshData =
        let beta = LayoutsHelpers.smoothstep (Math.Clamp (bundleBlend, 0.0, 1.0))
        let parts = ResizeArray<MeshData>(sphEdgesPre.Length)
        let dimCol = RgbaColor (0.18f, 0.24f, 0.36f, 1.0f)
        let hiGold = RgbaColor (1.0f, 0.88f, 0.34f, 1.0f)

        for ei = 0 to sphEdgesPre.Length - 1 do
            let e = sphEdgesPre[ei]
            let nPts = e.Unbundled.Length

            if nPts >= 2 then
                let incident =
                    match selectedHub with
                    | Some h -> e.SourceId = h || e.TargetId = h
                    | None -> false

                let anySelected = selectedHub.IsSome
                let baseRad =
                    if incident then 0.015f
                    elif anySelected then 0.006f
                    elif e.IsBackbone then 0.0115f
                    else 0.0085f

                let samples =
                    Array.init nPts (fun k ->
                        let pt = Point3.lerp e.Unbundled[k] e.Bundled[k] beta
                        let u = float32 k / float32 (nPts - 1)

                        let col =
                            if incident then
                                LayoutsHelpers.lerpColor hiGold e.EndColor u
                            elif anySelected then
                                dimCol
                            else
                                LayoutsHelpers.lerpColor e.StartColor e.EndColor u

                        let swell = 1.0f + 0.35f * MathF.Sin (u * MathF.PI)
                        struct (pt, baseRad * swell, col))

                match Line3D.tube sphScale 6 (ReadOnlySpan<struct (Point3 * float32 * RgbaColor)>(samples)) with
                | Ok m -> parts.Add m
                | Error _ -> ()

        MeshData.merge parts

    let sphTubeMesh = wgpu.UploadMesh (buildSphericalTubesMesh 0.82 None)
    let sphTubeBatch = wgpu.CreateBatch (sphTubeMesh, BlendMode.Opaque, false)

    do
        sphTubeBatch.UpdateInstances [| Instance3D.fromMatrix Matrix4x4.Identity RgbaColor.white 0.28f 0u InstanceFlags.None 0.14f |]

    let sphBatches = ReadOnlyMemory<InstanceBatch> [| sphTubeBatch; sphHubSink.Batch |]
    let sphLines = ReadOnlyMemory<LineBatch> [| sphWireBatch |]
    let sphDiscs = ReadOnlyMemory<DiscBatch> [| sphPacketSink.Batch |]

    let mutable sphBundleBlend = 0.82
    let mutable sphManualBlend : float option = None
    let mutable sphBlendPhaseOffset = 0.0
    let mutable sphSelectedHub : string option = None
    let mutable sphLastMeshBlend = 0.82
    let mutable sphLastMeshHub : string option = None
    let mutable sphDetail = "Click any globe hub to highlight incident corridors | [ / ] morphs unbundled ↔ KDE bundled"

    let syncSphericalHubs () =
        Api.batch (fun () ->
            for n in sphStressLayout.Nodes do
                let rIdx = hubRegionIdx[n.Id]
                let deg = match hubDegree.TryGetValue n.Id with true, d -> d | _ -> 1
                let isSel = sphSelectedHub = Some n.Id
                let r = float32 (0.036 + 0.0045 * float (min 8 deg)) * (if isSel then 1.32f else 1.0f)
                let col = if isSel then RgbaColor (1.0f, 0.94f, 0.55f, 1.0f) else regionColors[rIdx]
                let em = if isSel then 0.35f else 0.10f
                sphHubColl.AddOrUpdate (struct (n.Id, LayoutsHelpers.makeSphereGlyph n.Center r col isSel em)))

    let syncSphericalPackets (bundleBlend: float) (t: float) =
        let beta = LayoutsHelpers.smoothstep (Math.Clamp (bundleBlend, 0.0, 1.0))
        let stride = max 1 (sphEdgesPre.Length / 36)

        Api.batch (fun () ->
            let mutable pktIdx = 0

            for ei in 0 .. stride .. sphEdgesPre.Length - 1 do
                let e = sphEdgesPre[ei]
                let nPts = e.Unbundled.Length

                if nPts >= 2 then
                    let raw = t * 0.48 + float ei * 0.137
                    let phase = raw - floor raw
                    let fPos = phase * float (nPts - 1)
                    let idx0 = min (nPts - 2) (max 0 (int (floor fPos)))
                    let frac = Math.Clamp (fPos - float idx0, 0.0, 1.0)
                    let p0 = Point3.lerp e.Unbundled[idx0] e.Bundled[idx0] beta
                    let p1 = Point3.lerp e.Unbundled[idx0 + 1] e.Bundled[idx0 + 1] beta
                    let pt = Point3.lerp p0 p1 frac
                    let c = LayoutsHelpers.lerpColor e.StartColor e.EndColor (float32 phase)

                    let incident =
                        match sphSelectedHub with
                        | Some h -> e.SourceId = h || e.TargetId = h
                        | None -> false

                    let radiusPx = if incident then 8.5f else 6.0f
                    let discCol = if incident then RgbaColor (1.0f, 0.94f, 0.45f, 0.98f) else RgbaColor (c.R, c.G, c.B, 0.95f)

                    let datum =
                        ScatterDatum3D.create pt radiusPx discCol
                        |> ScatterDatum3D.withRings (float32 phase) (1.0f - float32 phase) 1.6f
                        |> ScatterDatum3D.withHollow 0.30f

                    sphPacketColl.AddOrUpdate (struct ($"pkt_{pktIdx}", datum))
                    pktIdx <- pktIdx + 1)

    let stepSphericalInternal (bundleBlend: float) (t: float) =
        let clamped = Math.Clamp (bundleBlend, 0.0, 1.0)
        sphBundleBlend <- clamped

        if abs (clamped - sphLastMeshBlend) > 1e-4 || sphSelectedHub <> sphLastMeshHub then
            sphLastMeshBlend <- clamped
            sphLastMeshHub <- sphSelectedHub
            sphTubeMesh.UpdateMesh (buildSphericalTubesMesh clamped sphSelectedHub)
            syncSphericalHubs ()
            plotRevision.MarkDirty ()

        syncSphericalPackets clamped t

    do
        syncSphericalHubs ()
        syncSphericalPackets sphBundleBlend 0.0

    // =========================================================================
    // Scene 4: Umap3D + Reactive Dbscan3D + Hilbert3D + Bvh3D Raycast
    // =========================================================================
    let umapBounds = Bounds3.create -0.95 -0.95 -0.95 1.90 1.90 1.90

    let manifoldScale =
        Scale3D.create
            BoxAspect.Cube
            UpAxis.YUp
            (AxisScale3D.linear (-1.08, 1.08))
            (AxisScale3D.linear (-1.08, 1.08))
            (AxisScale3D.linear (-1.08, 1.08))

    let manifoldAxisSpec = LayoutsHelpers.buildAxisSpec manifoldScale "UMAP-1" "UMAP-2" "UMAP-3" 5

    let manifoldDataset =
        let centers =
            [|
                [| +6.0; +6.0; +5.0; +4.0; -3.0; -2.0; +1.0; -1.0; +3.0; -3.0; +2.0; -1.0 |]
                [| -6.0; +6.0; -5.0; +3.0; +5.0; -3.0; +4.0; -2.0; -3.0; +3.0; -2.0; +2.0 |]
                [| -6.0; -6.0; +5.0; -5.0; -3.0; +5.0; -4.0; +3.0; +2.0; -4.0; +3.0; +0.5 |]
                [| +6.0; -6.0; -5.0; -3.0; -5.0; -5.0; +2.0; +5.0; -4.0; +4.0; +0.5; -3.0 |]
            |]

        let rows =
            [
                // 4 clusters x 22 points = 88 points (indices 14..21 form broader border halos)
                for c in 0 .. 3 do
                    for i in 0 .. 21 do
                        let idx = c * 22 + i
                        let spread = if i < 14 then 1.0 else 3.6
                        let feats =
                            [
                                for d in 0 .. 11 ->
                                    centers[c][d]
                                    + spread * sin (float (idx * 7 + d * 13) * 1.17)
                                    + spread * 0.75 * cos (float (idx * 11 + d * 5) * 0.83)
                            ]

                        yield $"m_{c}_{i:D2}", feats

                // 8 spaced high-D outliers -> natural DBSCAN border / noise points in the 3D embedding
                for o in 0 .. 7 do
                    let angle = float o * Math.PI * 0.25
                    let feats =
                        [
                            for d in 0 .. 11 ->
                                14.0 * sin (angle + float d * 0.73)
                                + 5.0 * cos (angle * 2.0 + float d * 1.19)
                        ]

                    yield $"out_{o:D2}", feats
            ]

        FeatureDataset.ofRows rows |> LayoutsHelpers.unwrapOk "FeatureDataset.ofRows"

    let embeddedPoints, knnLinks =
        Umap3D.embedWithLinks
            { Umap3DConfig.create umapBounds with
                Neighbors = 10
                MinDist = 1.8
                Spread = 2.0
                NegativeSampleRate = 14
                Epochs = 140
                Seed = 42u }
            manifoldDataset
        |> LayoutsHelpers.unwrapOk "Umap3D.embedWithLinks"

    let umapDiagram = Umap3D.toDiagramLayout 0.045 embeddedPoints knnLinks

    let hilbertOrdered = Hilbert3D.orderBy umapBounds (fun (p: EmbeddedPoint3D<string>) -> p.Position) embeddedPoints
    let hilbertCagePts = Hilbert3D.curve 2 umapBounds

    let bvhPointHalfExtent = 0.085

    let manifoldBvh =
        Bvh3D.buildPoints bvhPointHalfExtent (fun (p: EmbeddedPoint3D<string>) -> p.Position) embeddedPoints

    let manifoldStaticLineBatch = wgpu.CreateLineBatch ()
    let manifoldRayLineBatch = wgpu.CreateLineBatch ()

    do
        let verts = ResizeArray<LineVertex3D>(600)

        // 1. Subtle 3D Hilbert order-2 reference cage
        let cageCol = RgbaColor (0.28f, 0.42f, 0.66f, 0.24f)
        let cageSeq = [| for p in hilbertCagePts -> ValueSome struct (p, cageCol) |]
        let cageVerts = Line3D.polyline manifoldScale cageSeq
        verts.AddRange (cageVerts.ToArray ())

        // 2. Continuous Hilbert3D.orderBy traversal thread connecting the 96 UMAP points
        let tourRamp = ColorRamp.viridis (0.0, float (max 1 (hilbertOrdered.Length - 1)))
        let tourSeq =
            [|
                for i, p in List.indexed hilbertOrdered do
                    let c = ColorRamp.mapOrFallback tourRamp (float i) RgbaColor.white
                    yield ValueSome struct (p.Position, RgbaColor (c.R, c.G, c.B, 0.46f))
            |]

        let tourVerts = Line3D.polyline manifoldScale tourSeq
        verts.AddRange (tourVerts.ToArray ())

        // 3. Subtle k-NN skeleton backbone segments from Umap3D.toDiagramLayout
        let knnPairs = ResizeArray<struct (Point3 * Point3 * RgbaColor)>()
        let knnCol = RgbaColor (0.45f, 0.58f, 0.80f, 0.24f)

        for a in umapDiagram.Associations do
            if a.IsBackbone && a.Points.Length >= 2 then
                knnPairs.Add (struct (a.Points[0], a.Points[a.Points.Length - 1], knnCol))

        let knnVerts = Line3D.segments manifoldScale (ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)>(knnPairs.ToArray ()))
        verts.AddRange (knnVerts.ToArray ())
        manifoldStaticLineBatch.UpdateVertices (verts.ToArray ())

    let manifoldDiscColl = Api.createKeyedCollection (fun struct (k: string, _: ScatterDatum3D) -> k)
    let manifoldSink =
        let rows = manifoldDiscColl.Rows |> Projection.map (fun struct (_, d) -> d)

        let opts =
            SinkOptions3D.defaultValue
            |> SinkOptions3D.withLayerId (pickRegistry.AllocateLayer ())
            |> SinkOptions3D.withRevision plotRevision
            |> SinkOptions3D.withPreserveOrder false

        Reactive3D.bindDiscsWith wgpu opts (fun () -> manifoldScale) rows

    let manifoldBatches = ReadOnlyMemory<InstanceBatch>.Empty
    let manifoldLines = ReadOnlyMemory<LineBatch> [| manifoldStaticLineBatch; manifoldRayLineBatch |]
    let manifoldDiscs = ReadOnlyMemory<DiscBatch> [| manifoldSink.Batch |]

    let mutable dbscanEps = 0.17
    let mutable dbscanClusterCount = 0
    let mutable dbscanNoiseCount = 0
    let mutable dbscanAssignments : ClusteredPoint3D<EmbeddedPoint3D<string>, string> list = []
    let mutable rayHitId : string option = None
    let mutable clickedManifoldId : string option = None
    let mutable manifoldDetail = "Sweeping Bvh3D raycast | [ / ] adjusts DBSCAN epsilon"

    let syncManifoldDiscs () =
        let selSet =
            [ yield! Option.toList rayHitId; yield! Option.toList clickedManifoldId ]
            |> Set.ofList

        let packed =
            GraphScene3D.packClusteredScatter
                (fun item ->
                    let isHit = Set.contains item.Id selSet

                    match item.ClusterId, item.IsCore with
                    | Some _, true -> if isHit then 14.5f else 10.5f
                    | Some _, false -> if isHit then 13.5f else 9.0f
                    | None, _ -> if isHit then 13.0f else 8.5f)
                selSet
                dbscanAssignments

        Api.batch (fun () ->
            let span = packed.Span

            for i, a in List.indexed dbscanAssignments do
                if i < span.Length then
                    manifoldDiscColl.AddOrUpdate (struct (a.Id, span[i])))

    let runDbscan (eps: float) =
        let clamped = Math.Clamp (eps, 0.08, 0.48)
        dbscanEps <- clamped

        let res =
            Dbscan3D.clusterItems clamped 4 (fun (p: EmbeddedPoint3D<string>) -> p.Id) (fun p -> p.Position) embeddedPoints
            |> LayoutsHelpers.unwrapOk "Dbscan3D.clusterItems"

        dbscanAssignments <- res
        let mutable maxCid = -1
        let mutable noise = 0

        for a in res do
            match a.ClusterId with
            | Some c ->
                if c > maxCid then
                    maxCid <- c
            | None -> noise <- noise + 1

        dbscanClusterCount <- maxCid + 1
        dbscanNoiseCount <- noise
        syncManifoldDiscs ()

    let stepManifoldRayInternal (t: float) =
        let clusterIdx = abs (int (Math.Floor (t * 0.75))) % 4
        let anchor = embeddedPoints[clusterIdx * 22].Position
        let az = t * 0.95
        let el = 0.28 * sin (t * 0.65)
        let ce = cos el
        let origin = Point3.create (1.28 * ce * cos az) (1.28 * sin el) (1.28 * ce * sin az)
        let target = Point3.create (anchor.X + 0.04 * sin (t * 1.9)) (anchor.Y + 0.04 * cos (t * 1.5)) anchor.Z
        let rawDir = target - origin
        let dir = Point3.normalize rawDir
        let maxT = 3.4

        let hitOpt = Bvh3D.raycastNormalized origin rawDir maxT manifoldBvh
        let prevHit = rayHitId
        let rayVerts = ResizeArray<LineVertex3D>(32)

        match hitOpt with
        | Some (hitItem, hitT) ->
            rayHitId <- Some hitItem.Id
            let hitPt = origin + dir * hitT
            let wOrigin = Scale3D.map manifoldScale origin
            let wHit = Scale3D.map manifoldScale hitPt
            let rayCol = RgbaColor (1.0f, 0.92f, 0.32f, 0.98f)
            rayVerts.Add (LineVertex3D (wOrigin, rayCol))
            rayVerts.Add (LineVertex3D (wHit, rayCol))

            let hitBox =
                Bounds3.create
                    (hitItem.Position.X - bvhPointHalfExtent)
                    (hitItem.Position.Y - bvhPointHalfExtent)
                    (hitItem.Position.Z - bvhPointHalfExtent)
                    (2.0 * bvhPointHalfExtent)
                    (2.0 * bvhPointHalfExtent)
                    (2.0 * bvhPointHalfExtent)

            let boxWire = Line3D.boxesWireframe manifoldScale (ReadOnlySpan<struct (Bounds3 * RgbaColor)> [| struct (hitBox, RgbaColor (1.0f, 0.88f, 0.24f, 0.96f)) |]) false
            rayVerts.AddRange (boxWire.Span.ToArray ())
            manifoldDetail <- $"Bvh3D ray hit [{hitItem.Id}] at t={hitT:F2} · eps={dbscanEps:F2} ({dbscanClusterCount} clusters, {dbscanNoiseCount} noise)"
        | None ->
            rayHitId <- None
            let endPt = origin + dir * maxT
            let wOrigin = Scale3D.map manifoldScale origin
            let wEnd = Scale3D.map manifoldScale endPt
            let rayCol = RgbaColor (0.45f, 0.72f, 0.95f, 0.55f)
            rayVerts.Add (LineVertex3D (wOrigin, rayCol))
            rayVerts.Add (LineVertex3D (wEnd, rayCol))
            manifoldDetail <- $"Bvh3D ray miss · eps={dbscanEps:F2} ({dbscanClusterCount} clusters, {dbscanNoiseCount} noise)"

        manifoldRayLineBatch.UpdateVertices (rayVerts.ToArray ())
        plotRevision.MarkDirty ()

        if prevHit <> rayHitId then
            syncManifoldDiscs ()

    do
        pickRegistry.RegisterSink (
            h3Sink,
            describe = (fun m -> $"H³ node [{m.Key}] at ({m.DataPoint.X:F2}, {m.DataPoint.Y:F2}, {m.DataPoint.Z:F2}) · click to focus")
        )

        pickRegistry.RegisterSink (
            citySink,
            describe =
                (fun m ->
                    if m.Key.StartsWith "mod:" then
                        let modId = m.Key.Substring 4
                        let dist = match moduleDistrict.TryGetValue modId with true, d -> d | _ -> ""
                        let h = match moduleHeightMetric.TryGetValue modId with true, v -> v | _ -> 0.0
                        $"[{dist}/{modId}] complexity={h:F1} · click to boost"
                    elif m.Key.StartsWith "plinth:" then
                        $"District [{m.Key.Substring 7}]"
                    else
                        $"District [{m.Key}]")
        )

        pickRegistry.RegisterSink (
            sphHubSink,
            describe =
                (fun m ->
                    let rName = regionNames[hubRegionIdx[m.Key]]
                    $"Hub [{m.Key}] ({rName}) · click to isolate corridors")
        )

        pickRegistry.RegisterSink (
            sphPacketSink,
            describe = (fun pkt -> $"Packet [{pkt.Key}] at ({pkt.DataPoint.X:F2}, {pkt.DataPoint.Y:F2}, {pkt.DataPoint.Z:F2})")
        )

        pickRegistry.RegisterSink (
            manifoldSink,
            describe =
                (fun m ->
                    let cidOpt = dbscanAssignments |> List.tryFind (fun x -> x.Id = m.Key)

                    match cidOpt with
                    | Some a ->
                        match a.ClusterId with
                        | Some cid ->
                            let role = if a.IsCore then "core" else "border"
                            $"Point [{m.Key}] · Cluster #{cid} ({role})"
                        | None -> $"Point [{m.Key}] · DBSCAN noise outlier"
                    | None -> $"Point [{m.Key}]")
        )

        runDbscan 0.17
        stepManifoldRayInternal 0.75

    // =========================================================================
    // Public Controller Surface
    // =========================================================================

    /// <summary>Reactive scene revision counter updated when any GPU sink, mesh, or wireframe mutates.</summary>
    member _.Revision: int = plotRevision.Revision

    /// <summary>Returns the <see cref="T:Ranvier.D5.Wgpu.Scale3D"/> active for <paramref name="scene"/>.</summary>
    member _.ScaleFor(scene: LayoutSceneId) : Scale3D =
        match scene with
        | LayoutSceneId.PoincareH3 -> h3Scale
        | LayoutSceneId.CodeCityTreemap3D -> cityScale
        | LayoutSceneId.SphericalBundling3D -> sphScale
        | LayoutSceneId.ManifoldClusterBvh3D -> manifoldScale

    /// <summary>Advances the Poincaré H^3 hyperbolic Lorentz focus tour or click transition.</summary>
    member _.StepPoincare(t: float) =
        stepPoincareInternal t

    /// <summary>True when a manual Poincaré node focus boost transition is currently interpolating.</summary>
    member _.HasPendingPoincareTransition =
        match h3ManualTargetId with
        | Some _ -> not h3ManualSettled && h3ManualElapsed < 0.65
        | None -> false

    /// <summary>Advances an active manual Poincaré focus boost by <paramref name="dt"/> seconds (e.g. while auto-tour is paused).</summary>
    member _.StepPendingPoincare(dt: float) =
        match h3ManualTargetId with
        | Some targetId when not h3ManualSettled && dt > 0.0 ->
            h3ManualElapsed <- h3ManualElapsed + dt
            let u = min 1.0 (h3ManualElapsed / 0.65)
            let pt = interpMinkowski h3ManualFromPt h3ManualTargetPt u
            h3FocusNodeId <- targetId
            applyPoincareFocus pt targetId

            if u >= 1.0 then
                h3ManualSettled <- true
        | _ -> ()

    /// <summary>Smoothly boosts the requested node isometrically to the Poincaré ball center via Lorentz interpolation.</summary>
    member _.FocusNode(nodeId: string) =
        match h3NodeMinkowski.TryGetValue nodeId with
        | true, targetPt ->
            h3ManualFromPt <- h3CurrentFocusPt
            h3ManualTargetPt <- targetPt
            h3ManualTargetId <- Some nodeId
            h3ManualElapsed <- 0.0
            h3ManualSettled <- false
            h3FocusNodeId <- nodeId
            h3Detail <- $"Focusing [{nodeId}] via Lorentz boostToOrigin"
            // Highlight immediately at the current focus position so live interpolation glides cleanly with zero 1-frame snap-back.
            applyPoincareFocus h3CurrentFocusPt nodeId
        | _ -> ()

    /// <summary>Immediately boosts the requested node to the Poincaré ball center without interpolation.</summary>
    member _.FocusNodeImmediate(nodeId: string) =
        match h3NodeMinkowski.TryGetValue nodeId with
        | true, targetPt ->
            h3ManualFromPt <- targetPt
            h3ManualTargetPt <- targetPt
            h3ManualTargetId <- Some nodeId
            h3ManualElapsed <- 0.65
            h3ManualSettled <- true
            h3FocusNodeId <- nodeId
            h3Detail <- $"Focused [{nodeId}] via Lorentz boostToOrigin"
            applyPoincareFocus targetPt nodeId
        | _ -> ()

    /// <summary>Updates the CodeCity ↔ CubeHilbert morph parameter in `[0.0, 1.0]`.</summary>
    member _.StepCodeCity(morphT: float, _t: float) =
        syncCityVisuals morphT

    /// <summary>Sets and holds the CodeCity ↔ CubeHilbert morph parameter in `[0.0, 1.0]`.</summary>
    member _.SetCityMorph(morphT: float) =
        let clamped = Math.Clamp (morphT, 0.0, 1.0)
        cityManualMorph <- Some clamped
        syncCityVisuals clamped

    /// <summary>Current CodeCity ↔ CubeHilbert morph factor in `[0.0, 1.0]`.</summary>
    member _.CityMorph = currentMorphT

    /// <summary>Mutates a module building's complexity metric, recomputes CodeCity layout, and updates GPU bars incrementally.</summary>
    member _.MutateBuilding(id: string) =
        match moduleHeightMetric.TryGetValue id with
        | true, cur ->
            let nextH = if cur * 1.42 > 110.0 then 18.0 + (cur % 19.0) else cur * 1.42
            moduleHeightMetric[id] <- nextH
            selectedBuildingId <- Some id
            recomputeCodeCity ()
            syncCityVisuals currentMorphT
            cityDetail <- $"Mutated [{id}] complexity -> {nextH:F1} (CodeCity recomputed, KeyedCollection updated)"
        | _ -> ()

    /// <summary>Morphs 3D edge corridors between great circles (`0.0`) and KDE-bundled highways (`1.0`) and animates packet discs.</summary>
    member _.StepSphericalBundling(bundleBlend: float, t: float) =
        stepSphericalInternal bundleBlend t

    /// <summary>Sets and holds the spherical bundling blend factor in `[0.0, 1.0]`.</summary>
    member _.SetBundleBlend(bundleBlend: float) =
        let clamped = Math.Clamp (bundleBlend, 0.0, 1.0)
        sphManualBlend <- Some clamped
        stepSphericalInternal clamped lastSceneT

    /// <summary>Current spherical edge-bundling blend in `[0.0, 1.0]`.</summary>
    member _.BundleBlend = sphBundleBlend

    /// <summary>Highlights a clicked spherical hub and all its incident 3D streamtube corridors.</summary>
    member _.SelectHub(hubId: string) =
        let nextSel = if sphSelectedHub = Some hubId then None else Some hubId
        sphSelectedHub <- nextSel
        stepSphericalInternal sphBundleBlend lastSceneT

        sphDetail <-
            match nextSel with
            | Some h -> $"Highlighted hub [{h}] ({regionNames[hubRegionIdx[h]]}) & incident corridors"
            | None -> "Cleared hub filter · all 5 continental regions active"

    /// <summary>Sets the DBSCAN neighborhood radius `eps` and re-clusters the 96-point UMAP embedding incrementally.</summary>
    member _.SetDbscanEpsilon(eps: float) =
        runDbscan eps
        manifoldDetail <- $"DBSCAN eps={dbscanEps:F2} -> {dbscanClusterCount} clusters, {dbscanNoiseCount} noise outliers"

    /// <summary>Current DBSCAN radius epsilon.</summary>
    member _.DbscanEpsilon = dbscanEps

    /// <summary>Sweeps the Bvh3D closest-hit query ray across the 96-point manifold embedding.</summary>
    member _.StepManifoldRay(t: float) =
        stepManifoldRayInternal t

    /// <summary>Advances the active scene's live animation at time <paramref name="t"/> seconds.</summary>
    member _.StepScene(scene: LayoutSceneId, t: float) =
        match scene with
        | LayoutSceneId.PoincareH3 -> stepPoincareInternal t
        | LayoutSceneId.CodeCityTreemap3D ->
            lastSceneT <- t

            match cityManualMorph with
            | Some _ -> ()
            | None ->
                let m = 0.5 - 0.5 * cos ((t + cityMorphPhaseOffset) * 0.55)
                syncCityVisuals m
        | LayoutSceneId.SphericalBundling3D ->
            lastSceneT <- t

            let b =
                match sphManualBlend with
                | Some manual -> manual
                | None -> 0.5 - 0.5 * cos ((t + sphBlendPhaseOffset) * 0.45)

            stepSphericalInternal b t
        | LayoutSceneId.ManifoldClusterBvh3D ->
            lastSceneT <- t
            stepManifoldRayInternal t

    /// <summary>Applies interactive `[` / `]` parameter scrubbing for the active scene.</summary>
    member _.ScrubScene(scene: LayoutSceneId, delta: float) : string =
        match scene with
        | LayoutSceneId.PoincareH3 ->
            h3ManualTargetId <- None
            h3ManualElapsed <- 0.0
            h3ManualSettled <- false
            h3TourPhaseOffset <- h3TourPhaseOffset + delta * 1.65
            stepPoincareInternal lastSceneT
            h3Detail <- $"Poincaré tour focus -> [{h3FocusNodeId}]"
            h3Detail
        | LayoutSceneId.CodeCityTreemap3D ->
            let nextM = Math.Clamp (currentMorphT + delta * 0.14, 0.0, 1.0)
            cityManualMorph <- Some nextM
            syncCityVisuals nextM
            cityDetail <- $"CodeCity ↔ CubeHilbert morph={nextM:F2}"
            cityDetail
        | LayoutSceneId.SphericalBundling3D ->
            let nextB = Math.Clamp (sphBundleBlend + delta * 0.14, 0.0, 1.0)
            sphManualBlend <- Some nextB
            stepSphericalInternal nextB lastSceneT
            sphDetail <- $"Spherical KDE bundle blend={nextB:F2}"
            sphDetail
        | LayoutSceneId.ManifoldClusterBvh3D ->
            let nextEps = Math.Clamp (dbscanEps + delta * 0.025, 0.08, 0.48)
            runDbscan nextEps
            manifoldDetail <- $"DBSCAN eps={dbscanEps:F2} -> {dbscanClusterCount} clusters, {dbscanNoiseCount} noise"
            manifoldDetail

    /// <summary>Resets scene-specific interactive parameters (focus, morph, hub filter, or epsilon).</summary>
    member _.ResetScene(scene: LayoutSceneId) =
        match scene with
        | LayoutSceneId.PoincareH3 ->
            h3ManualTargetId <- None
            h3ManualElapsed <- 0.0
            h3ManualSettled <- false
            h3TourPhaseOffset <- -lastSceneT
            h3FocusNodeId <- "root"
            h3Detail <- "Reset Poincaré H³ focus to root"
            applyPoincareFocus Hyperbolic3D.origin "root"
        | LayoutSceneId.CodeCityTreemap3D ->
            resetModuleMetrics ()
            selectedBuildingId <- None
            recomputeCodeCity ()
            cityManualMorph <- None
            cityMorphPhaseOffset <- -lastSceneT
            syncCityVisuals 0.0
            cityDetail <- "Reset CodeCity towers & CubeHilbert morph"
        | LayoutSceneId.SphericalBundling3D ->
            sphSelectedHub <- None
            sphManualBlend <- None
            sphBlendPhaseOffset <- (Math.Acos (1.0 - 2.0 * 0.82) / 0.45) - lastSceneT
            stepSphericalInternal 0.82 lastSceneT
            sphDetail <- "Reset S² KDE bundle blend & resumed animation"
        | LayoutSceneId.ManifoldClusterBvh3D ->
            clickedManifoldId <- None
            runDbscan 0.17
            stepManifoldRayInternal (if abs lastSceneT > 1e-6 then lastSceneT else 0.75)

    /// <summary>Resolves a GPU hover <see cref="T:Ranvier.Wgpu.PickHit"/> into a concise readout string.</summary>
    member this.DescribeHit(scene: LayoutSceneId, hit: PickHit) : string =
        let sc = this.ScaleFor scene

        match pickRegistry.DescribeHit (sc, hit) with
        | ValueSome desc -> desc
        | ValueNone ->
            let pt = Probe3D.invertHit sc hit

            match scene with
            | LayoutSceneId.PoincareH3 -> $"Poincaré ball ({pt.X:F2}, {pt.Y:F2}, {pt.Z:F2})"
            | LayoutSceneId.CodeCityTreemap3D -> $"City ({pt.X:F2}, {pt.Y:F2}, {pt.Z:F2})"
            | LayoutSceneId.SphericalBundling3D -> $"Sphere ({pt.X:F2}, {pt.Y:F2}, {pt.Z:F2})"
            | LayoutSceneId.ManifoldClusterBvh3D -> $"UMAP ({pt.X:F2}, {pt.Y:F2}, {pt.Z:F2})"

    /// <summary>Executes click interaction for <paramref name="hit"/> on the active scene and returns status text.</summary>
    member this.ClickHit(scene: LayoutSceneId, hit: PickHit) : string =
        match scene with
        | LayoutSceneId.PoincareH3 ->
            match Probe3D.resolveKeyed h3Scale h3Sink hit with
            | ValueSome m ->
                this.FocusNode m.Key
                h3Detail
            | ValueNone -> this.DescribeHit (scene, hit)
        | LayoutSceneId.CodeCityTreemap3D ->
            match Probe3D.resolveKeyed cityScale citySink hit with
            | ValueSome m when m.Key.StartsWith "mod:" ->
                let modId = m.Key.Substring 4
                this.MutateBuilding modId
                cityDetail
            | _ -> this.DescribeHit (scene, hit)
        | LayoutSceneId.SphericalBundling3D ->
            match Probe3D.resolveKeyed sphScale sphHubSink hit with
            | ValueSome m ->
                this.SelectHub m.Key
                sphDetail
            | ValueNone -> this.DescribeHit (scene, hit)
        | LayoutSceneId.ManifoldClusterBvh3D ->
            match Probe3D.resolveDisc manifoldScale manifoldSink hit with
            | ValueSome m ->
                clickedManifoldId <- if clickedManifoldId = Some m.Key then None else Some m.Key
                syncManifoldDiscs ()
                this.DescribeHit (scene, hit)
            | ValueNone -> this.DescribeHit (scene, hit)

    /// <summary>Builds the GPU batches, line batches, disc batches, and HUD summary for <paramref name="scene"/>.</summary>
    member _.BuildPass(scene: LayoutSceneId, section: SectionPlane) : ActiveLayoutPass3D =
        let secOpt = if section.Enabled then ValueSome section else ValueNone

        match scene with
        | LayoutSceneId.PoincareH3 ->
            {
                AxisSpec = h3AxisSpec
                Batches = h3Batches
                Lines = h3Lines
                Discs = ReadOnlyMemory.Empty
                Section = secOpt
                Subtitle = $"73-node Poincaré H³ ball · focus=[{h3FocusNodeId}] · 90 conformal geodesic arcs"
                Detail = h3Detail
            }
        | LayoutSceneId.CodeCityTreemap3D ->
            {
                AxisSpec = cityAxisSpec
                Batches = cityBatches
                Lines = cityLines
                Discs = ReadOnlyMemory.Empty
                Section = secOpt
                Subtitle = $"42 modules / 4 districts · CodeCity ↔ CubeHilbert morph={currentMorphT:F2} (Explode=0.16)"
                Detail = cityDetail
            }
        | LayoutSceneId.SphericalBundling3D ->
            {
                AxisSpec = sphAxisSpec
                Batches = sphBatches
                Lines = sphLines
                Discs = sphDiscs
                Section = secOpt
                Subtitle = $"35 hubs on S² (R=1.05) · bundle={sphBundleBlend:F2} · {sphTubeMesh.TriangleCount} tube tris + packet discs"
                Detail = sphDetail
            }
        | LayoutSceneId.ManifoldClusterBvh3D ->
            {
                AxisSpec = manifoldAxisSpec
                Batches = manifoldBatches
                Lines = manifoldLines
                Discs = manifoldDiscs
                Section = secOpt
                Subtitle = $"96x12D Umap3D · Dbscan3D eps={dbscanEps:F2} ({dbscanClusterCount} clusters, {dbscanNoiseCount} noise) · Hilbert3D + Bvh3D"
                Detail = manifoldDetail
            }

    interface IDisposable with
        member _.Dispose() =
            manifoldRayLineBatch.Dispose ()
            manifoldStaticLineBatch.Dispose ()
            sphWireBatch.Dispose ()
            sphTubeBatch.Dispose ()
            sphTubeMesh.Dispose ()
            cityWireBatch.Dispose ()
            cityVoronoiBatch.Dispose ()
            cityVoronoiMesh.Dispose ()
            h3LineBatch.Dispose ()

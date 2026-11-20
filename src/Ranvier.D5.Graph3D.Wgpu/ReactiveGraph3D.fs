namespace Ranvier.D5.Graph3D.Wgpu

open System
open System.Collections.Generic
open System.Numerics
open Ranvier
open Ranvier.D5
open Ranvier.D5.Graph3D
open Ranvier.D5.Wgpu
open Ranvier.Wgpu

/// <summary>Live GPU batches and composable <c>Layer3D</c> list produced by <c>ReactiveGraph3D.bindDiagram</c>.</summary>
[<NoEquality; NoComparison>]
type DiagramSceneBindings3D =
    {
        /// <summary>Keyed instanced node glyph sink mapped by string node ID.</summary>
        NodeSink: KeyedInstanceSink<string>
        /// <summary>Translucent <c>DrawBeforeLines</c> compound group box sink mapped by string group ID.</summary>
        GroupBoxSink: KeyedInstanceSink<string>
        /// <summary>4x-MSAA hairline batch carrying group wireframes and association polylines.</summary>
        LineBatch: LineBatch
        /// <summary>Optional single-instance cylindrical tube mesh batch when <c>GraphTheme3D.UseEdgeTubes = true</c>.</summary>
        EdgeTubeBatch: InstanceBatch voption
        /// <summary>Composable ordered <c>Layer3D</c> list ready for <c>PlotPass3D.ofLayers</c>.</summary>
        Layers: Layer3D list
    }

/// <summary>Live GPU batches and composable <c>Layer3D</c> list produced by <c>ReactiveGraph3D.bindCodeCity</c>.</summary>
[<NoEquality; NoComparison>]
type CodeCitySceneBindings3D<'Id when 'Id: equality> =
    {
        /// <summary>Keyed opaque leaf building sink mapped by caller entity identifier.</summary>
        BuildingSink: KeyedInstanceSink<'Id>
        /// <summary>Keyed translucent district plinth sink rendered with <c>DrawBeforeLines = true</c>.</summary>
        DistrictSink: KeyedInstanceSink<'Id>
        /// <summary>Composable ordered <c>Layer3D</c> list ready for <c>PlotPass3D.ofLayers</c>.</summary>
        Layers: Layer3D list
    }

/// <summary>Live GPU batches and composable <c>Layer3D</c> list produced by <c>ReactiveGraph3D.bindTreemap3D</c>.</summary>
[<NoEquality; NoComparison>]
type TreemapSceneBindings3D<'Id when 'Id: equality> =
    {
        /// <summary>Keyed 3D hierarchy cuboid sink mapped by caller node identifier.</summary>
        CuboidSink: KeyedInstanceSink<'Id>
        /// <summary>Optional 4x-MSAA wireframe outline batch across cuboid envelopes.</summary>
        WireframeBatch: LineBatch voption
        /// <summary>Composable ordered <c>Layer3D</c> list ready for <c>PlotPass3D.ofLayers</c>.</summary>
        Layers: Layer3D list
    }

/// <summary>Live GPU batches and composable <c>Layer3D</c> list produced by <c>ReactiveGraph3D.bindVoronoiTreemap3D</c>.</summary>
[<NoEquality; NoComparison>]
type VoronoiSceneBindings3D =
    {
        /// <summary>Single-instance merged convex polyhedron mesh batch for Voronoi cell volumes.</summary>
        MeshBatch: InstanceBatch
        /// <summary>Optional deduplicated 4x-MSAA polygonal edge wireframe batch.</summary>
        WireframeBatch: LineBatch voption
        /// <summary>Composable ordered <c>Layer3D</c> list ready for <c>PlotPass3D.ofLayers</c>.</summary>
        Layers: Layer3D list
    }

/// <summary>High-level reactive binders wiring 3D graph, city, treemap, polyhedral Voronoi, and clustered cloud signals directly to GPU sinks, <c>PickRegistry3D</c>, and <c>PlotRevision3D</c>.</summary>
[<RequireQualifiedAccess>]
module ReactiveGraph3D =
    let inline private markRevisionDirty (revision: PlotRevision3D voption) =
        match revision with
        | ValueSome rev -> rev.MarkDirty ()
        | ValueNone -> ()

    let private tryRegisterOwnerCleanup (cleanup: unit -> unit) =
        match Graph.TryCurrent with
        | ValueSome _ ->
            match Api.tryGetOwner () with
            | ValueSome _ -> Api.onCleanup cleanup
            | ValueNone -> ()
        | ValueNone -> ()

    let private secondaryBoxOptions (primaryOptions: SinkOptions3D) : SinkOptions3D =
        let adjustedLayer =
            match primaryOptions.Registry, primaryOptions.LayerId with
            | ValueSome _, _ -> ValueNone
            | ValueNone, ValueSome lid -> ValueSome (if lid = 255uy then 254uy else lid + 1uy)
            | ValueNone, ValueNone -> ValueSome 254uy

        { primaryOptions with
            LayerId = adjustedLayer
            BlendMode = BlendMode.AlphaBlend
            DrawBeforeLines = true
        }

    let private syncKeyedRows<'K, 'Datum when 'K: equality>
        (collection: KeyedCollection<'K, struct ('K * 'Datum)>)
        (activeKeys: HashSet<'K>)
        (items: ('K * 'Datum) array)
        =
        let incomingKeys = HashSet<'K>(HashIdentity.Structural)

        for i = 0 to items.Length - 1 do
            incomingKeys.Add (fst items[i]) |> ignore

        let toRemove = ResizeArray<'K>()

        for k in activeKeys do
            if not (incomingKeys.Contains k) then
                toRemove.Add k

        Api.batch (fun () ->
            for k in toRemove do
                collection.Remove k |> ignore
                activeKeys.Remove k |> ignore

            for i = 0 to items.Length - 1 do
                let k, d = items[i]
                collection.AddOrUpdate (struct (k, d))
                activeKeys.Add k |> ignore)

    let private tryShadingParams (shading: ShadingMode) =
        match shading with
        | ShadingMode.Unlit -> struct (0.5f, 0.0f, InstanceFlags.Unlit)
        | ShadingMode.ExactUnlit -> struct (0.5f, 0.0f, InstanceFlags.Unlit ||| InstanceFlags.ExactColor)
        | ShadingMode.Lit (r, e) -> struct (r, e, InstanceFlags.None)

    /// <summary>
    /// Binds a reactive <c>DiagramLayout3D</c> into keyed node glyphs, translucent <c>DrawBeforeLines</c> group boxes,
    /// edge hairlines or extruded cylindrical tubes, <c>PickRegistry3D</c> registration, and automatic <c>PlotRevision3D</c> dirtying.
    /// </summary>
    let bindDiagram
        (context: WgpuContext)
        (options: SinkOptions3D)
        (theme: GraphTheme3D)
        (scale: unit -> Scale3D)
        (selectedIds: unit -> Set<string>)
        (layout: unit -> DiagramLayout3D)
        : DiagramSceneBindings3D =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box theme) then nullArg (nameof theme)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box selectedIds) then nullArg (nameof selectedIds)
        if isNull (box layout) then nullArg (nameof layout)

        let nodeColl = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
        let groupColl = Api.createKeyedCollection (fun struct (k: string, _: BoxDatum3D) -> k)
        let activeNodeKeys = HashSet<string>(HashIdentity.Structural)
        let activeGroupKeys = HashSet<string>(HashIdentity.Structural)

        let nodeRows = nodeColl.Rows |> Projection.map (fun struct (_, g) -> g)
        let groupRows = groupColl.Rows |> Projection.map (fun struct (_, b) -> b)

        let nodeSink = Reactive3D.bindGlyphs context theme.NodeShape options scale nodeRows
        let groupSink = Reactive3D.bindBoxes context (secondaryBoxOptions options) scale groupRows

        let lineBatch = context.CreateLineBatch ()
        tryRegisterOwnerCleanup lineBatch.Dispose

        let tubeMeshOpt, tubeBatchOpt =
            if theme.UseEdgeTubes then
                let m = context.UploadMesh MeshData.empty
                let b = context.CreateSingleBatch (m, options.BlendMode, options.DoubleSided, drawBeforeLines = options.DrawBeforeLines)
                b.Resize 0
                tryRegisterOwnerCleanup (fun () -> b.Dispose (); m.Dispose ())
                ValueSome m, ValueSome b
            else
                ValueNone, ValueNone

        let mutable prevLineVertCount = 0

        Api.createEffect (fun () ->
            let curLayout = layout ()
            let curScale = scale ()
            let sel = selectedIds ()

            if isNull (box curLayout) then nullArg "layout"
            if isNull (box curScale) then nullArg "scale"

            let packedNodes = GraphScene3D.packNodes theme sel curLayout
            let packedNodeSpan = packedNodes.Span
            let nodeEntries = Array.zeroCreate<string * GlyphDatum3D> (min packedNodeSpan.Length curLayout.Nodes.Length)
            let mutable ni = 0

            for nd in curLayout.Nodes do
                if ni < nodeEntries.Length && Double.IsFinite nd.Center.X && Double.IsFinite nd.Center.Y && Double.IsFinite nd.Center.Z then
                    nodeEntries[ni] <- nd.Id, packedNodeSpan[ni]
                    ni <- ni + 1

            let trimmedNodes = if ni = nodeEntries.Length then nodeEntries else Array.sub nodeEntries 0 ni
            syncKeyedRows nodeColl activeNodeKeys trimmedNodes

            let groupList = ResizeArray<string * BoxDatum3D>(curLayout.Groups.Length)

            for g in curLayout.Groups do
                match theme.GroupBox g with
                | ValueSome b -> groupList.Add (g.Id, b)
                | ValueNone -> ()

            syncKeyedRows groupColl activeGroupKeys (groupList.ToArray ())

            let groupWireVerts = GraphScene3D.packGroupWireframe curScale theme curLayout
            let assocVerts =
                if theme.UseEdgeTubes then ReadOnlyMemory<LineVertex3D>.Empty
                else GraphScene3D.packAssociationLines curScale theme curLayout

            let totalLineVerts = groupWireVerts.Length + assocVerts.Length

            if totalLineVerts > 0 then
                let combined = Array.zeroCreate<LineVertex3D> totalLineVerts

                if groupWireVerts.Length > 0 then
                    let gs = groupWireVerts.Span

                    for i = 0 to gs.Length - 1 do
                        combined[i] <- gs[i]

                if assocVerts.Length > 0 then
                    let av = assocVerts.Span
                    let offset = groupWireVerts.Length

                    for i = 0 to av.Length - 1 do
                        combined[offset + i] <- av[i]

                lineBatch.UpdateVertices (ReadOnlySpan<LineVertex3D>(combined))
                prevLineVertCount <- totalLineVerts
                markRevisionDirty options.Revision
            elif prevLineVertCount > 0 then
                lineBatch.Resize 0
                prevLineVertCount <- 0
                markRevisionDirty options.Revision

            match tubeMeshOpt, tubeBatchOpt with
            | ValueSome mHandle, ValueSome tBatch ->
                match GraphScene3D.buildAssociationTubes curScale theme.TubeSides theme curLayout with
                | Ok mesh when mesh.Vertices.Length > 0 && mesh.Indices.Length > 0 ->
                    mHandle.UpdateMesh mesh
                    let struct (r, em, fl) = tryShadingParams theme.NodeShading
                    tBatch.SetSingle (Instance3D.fromMatrix Matrix4x4.Identity RgbaColor.white r options.PickId fl em)
                    markRevisionDirty options.Revision
                | _ ->
                    mHandle.UpdateMesh MeshData.empty
                    tBatch.Resize 0
                    markRevisionDirty options.Revision
            | _ -> ())

        let layers =
            [
                yield Layer3D.keyedInstances groupSink
                match tubeBatchOpt with
                | ValueSome tb -> yield Layer3D.batch tb
                | ValueNone -> ()
                yield Layer3D.keyedInstances nodeSink
                yield Layer3D.lines lineBatch
            ]

        {
            NodeSink = nodeSink
            GroupBoxSink = groupSink
            LineBatch = lineBatch
            EdgeTubeBatch = tubeBatchOpt
            Layers = layers
        }

    /// <summary>Binds a 2.5D <c>CodeCity</c> hierarchy into separate opaque leaf building and translucent district plinth sinks.</summary>
    let bindCodeCity<'Id when 'Id: comparison>
        (context: WgpuContext)
        (options: SinkOptions3D)
        (colorOf: CityBuilding<'Id> -> RgbaColor)
        (shading: ShadingMode)
        (selectedIds: unit -> Set<'Id>)
        (scale: unit -> Scale3D)
        (buildings: unit -> IReadOnlyList<CityBuilding<'Id>>)
        : CodeCitySceneBindings3D<'Id> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box selectedIds) then nullArg (nameof selectedIds)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box buildings) then nullArg (nameof buildings)

        let leafColl = Api.createKeyedCollection (fun struct (k: 'Id, _: BoxDatum3D) -> k)
        let districtColl = Api.createKeyedCollection (fun struct (k: 'Id, _: BoxDatum3D) -> k)
        let activeLeafKeys = HashSet<'Id>(HashIdentity.Structural)
        let activeDistKeys = HashSet<'Id>(HashIdentity.Structural)

        let leafSink = Reactive3D.bindBoxes context options scale (leafColl.Rows |> Projection.map (fun struct (_, b) -> b))
        let distSink = Reactive3D.bindBoxes context (secondaryBoxOptions options) scale (districtColl.Rows |> Projection.map (fun struct (_, b) -> b))

        Api.createEffect (fun () ->
            let items = buildings ()
            let sel = selectedIds ()

            if isNull (box items) then nullArg "buildings"

            let leafBuf = ResizeArray<'Id * BoxDatum3D>(items.Count)
            let distBuf = ResizeArray<'Id * BoxDatum3D>(items.Count / 3 + 1)

            for i = 0 to items.Count - 1 do
                let b = items[i]
                let isSel = Set.contains b.Id sel

                let datum =
                    BoxDatum3D.create b.Bounds (colorOf b)
                    |> BoxDatum3D.withShading shading
                    |> BoxDatum3D.withSelected isSel

                if b.IsDistrict then distBuf.Add (b.Id, datum) else leafBuf.Add (b.Id, datum)

            syncKeyedRows leafColl activeLeafKeys (leafBuf.ToArray ())
            syncKeyedRows districtColl activeDistKeys (distBuf.ToArray ()))

        {
            BuildingSink = leafSink
            DistrictSink = distSink
            Layers =
                [
                    Layer3D.keyedInstances distSink
                    Layer3D.keyedInstances leafSink
                ]
        }

    /// <summary>Binds 3D <c>Treemap3D</c> cuboids and optional outer cuboid wireframes into keyed reactive GPU batches.</summary>
    let bindTreemap3D<'Id when 'Id: comparison>
        (context: WgpuContext)
        (options: SinkOptions3D)
        (colorOf: HierarchyCuboid<'Id> -> RgbaColor)
        (shading: ShadingMode)
        (leavesOnly: bool)
        (wireframeColor: (HierarchyCuboid<'Id> -> RgbaColor) option)
        (scale: unit -> Scale3D)
        (cuboids: unit -> IReadOnlyList<HierarchyCuboid<'Id>>)
        : TreemapSceneBindings3D<'Id> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box cuboids) then nullArg (nameof cuboids)

        let boxColl = Api.createKeyedCollection (fun struct (k: 'Id, _: BoxDatum3D) -> k)
        let activeKeys = HashSet<'Id>(HashIdentity.Structural)
        let cuboidSink = Reactive3D.bindBoxes context options scale (boxColl.Rows |> Projection.map (fun struct (_, b) -> b))

        let wireBatchOpt =
            match wireframeColor with
            | Some _ ->
                let lb = context.CreateLineBatch ()
                tryRegisterOwnerCleanup lb.Dispose
                ValueSome lb
            | None -> ValueNone

        let mutable prevWireCount = 0

        Api.createEffect (fun () ->
            let items = cuboids ()
            let curScale = scale ()

            if isNull (box items) then nullArg "cuboids"
            if isNull (box curScale) then nullArg "scale"

            let boxList = ResizeArray<'Id * BoxDatum3D>(items.Count)

            for i = 0 to items.Count - 1 do
                let c = items[i]

                if not leavesOnly || c.IsLeaf then
                    boxList.Add (c.Id, BoxDatum3D.create c.Bounds (colorOf c) |> BoxDatum3D.withShading shading)

            syncKeyedRows boxColl activeKeys (boxList.ToArray ())

            match wireBatchOpt, wireframeColor with
            | ValueSome lb, Some wCol ->
                let verts = GraphScene3D.packTreemapWireframe curScale wCol options.IgnoreSectionPlane items

                if verts.Length > 0 then
                    lb.UpdateVertices verts.Span
                    prevWireCount <- verts.Length
                    markRevisionDirty options.Revision
                elif prevWireCount > 0 then
                    lb.Resize 0
                    prevWireCount <- 0
                    markRevisionDirty options.Revision
            | _ -> ())

        let layers =
            [
                yield Layer3D.keyedInstances cuboidSink
                match wireBatchOpt with
                | ValueSome lb -> yield Layer3D.lines lb
                | ValueNone -> ()
            ]

        {
            CuboidSink = cuboidSink
            WireframeBatch = wireBatchOpt
            Layers = layers
        }

    /// <summary>Binds 3D convex polyhedral <c>VoronoiTreemap3D</c> cells with live shrink explosion and optional deduplicated edge wireframes.</summary>
    let bindVoronoiTreemap3D<'Id>
        (context: WgpuContext)
        (options: SinkOptions3D)
        (shrinkFactor: unit -> float)
        (colorOf: VoronoiTreemap3DCell<'Id> -> RgbaColor)
        (wireframeColor: RgbaColor voption)
        (shading: ShadingMode)
        (scale: unit -> Scale3D)
        (cells: unit -> IReadOnlyList<VoronoiTreemap3DCell<'Id>>)
        : VoronoiSceneBindings3D =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box shrinkFactor) then nullArg (nameof shrinkFactor)
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box cells) then nullArg (nameof cells)

        let meshHandle = context.UploadMesh MeshData.empty
        let meshBatch = context.CreateSingleBatch (meshHandle, options.BlendMode, options.DoubleSided, drawBeforeLines = options.DrawBeforeLines)
        meshBatch.Resize 0
        tryRegisterOwnerCleanup (fun () -> meshBatch.Dispose (); meshHandle.Dispose ())

        let wireBatchOpt =
            match wireframeColor with
            | ValueSome _ ->
                let lb = context.CreateLineBatch ()
                tryRegisterOwnerCleanup lb.Dispose
                ValueSome lb
            | ValueNone -> ValueNone

        let mutable prevWireCount = 0

        Api.createEffect (fun () ->
            let curScale = scale ()
            let curShrink = shrinkFactor ()
            let curCells = cells ()

            if isNull (box curScale) then nullArg "scale"
            if isNull (box curCells) then nullArg "cells"

            let mesh = GraphScene3D.buildVoronoiCellMeshes curScale curShrink colorOf curCells

            if mesh.Vertices.Length > 0 && mesh.Indices.Length > 0 then
                meshHandle.UpdateMesh mesh
                let struct (r, em, fl) = tryShadingParams shading
                meshBatch.SetSingle (Instance3D.fromMatrix Matrix4x4.Identity RgbaColor.white r options.PickId fl em)
            else
                meshHandle.UpdateMesh MeshData.empty
                meshBatch.Resize 0

            match wireBatchOpt, wireframeColor with
            | ValueSome lb, ValueSome wCol ->
                let wire = GraphScene3D.packVoronoiWireframe curScale curShrink (fun _ -> wCol) options.IgnoreSectionPlane curCells

                if wire.Length > 0 then
                    lb.UpdateVertices wire.Span
                    prevWireCount <- wire.Length
                elif prevWireCount > 0 then
                    lb.Resize 0
                    prevWireCount <- 0
            | _ -> ()

            markRevisionDirty options.Revision)

        let layers =
            [
                yield Layer3D.batch meshBatch
                match wireBatchOpt with
                | ValueSome lb -> yield Layer3D.lines lb
                | ValueNone -> ()
            ]

        {
            MeshBatch = meshBatch
            WireframeBatch = wireBatchOpt
            Layers = layers
        }

    /// <summary>Binds a <c>ClusteredPoint3D</c> stream into a keyed SDF disc sink with automatic cluster palette &amp; noise outlier styling.</summary>
    let bindClusteredCloud<'T, 'Id when 'Id: comparison>
        (context: WgpuContext)
        (options: SinkOptions3D)
        (radiusPx: ClusteredPoint3D<'T, 'Id> -> float32)
        (selectedIds: unit -> Set<'Id>)
        (scale: unit -> Scale3D)
        (clusters: unit -> IReadOnlyList<ClusteredPoint3D<'T, 'Id>>)
        : KeyedDiscSink<'Id> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box radiusPx) then nullArg (nameof radiusPx)
        if isNull (box selectedIds) then nullArg (nameof selectedIds)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box clusters) then nullArg (nameof clusters)

        let discColl = Api.createKeyedCollection (fun struct (k: 'Id, _: ScatterDatum3D) -> k)
        let activeKeys = HashSet<'Id>(HashIdentity.Structural)
        let discSink = Reactive3D.bindDiscsWith context options scale (discColl.Rows |> Projection.map (fun struct (_, d) -> d))

        Api.createEffect (fun () ->
            let curClusters = clusters ()
            let sel = selectedIds ()

            if isNull (box curClusters) then nullArg "clusters"

            let packed = GraphScene3D.packClusteredScatter radiusPx sel curClusters
            let span = packed.Span
            let pairs = Array.zeroCreate<'Id * ScatterDatum3D> (min span.Length curClusters.Count)
            let mutable idx = 0

            for i = 0 to curClusters.Count - 1 do
                let c = curClusters[i]

                if idx < pairs.Length && Double.IsFinite c.Position.X && Double.IsFinite c.Position.Y && Double.IsFinite c.Position.Z then
                    pairs[idx] <- c.Id, span[idx]
                    idx <- idx + 1

            let trimmed = if idx = pairs.Length then pairs else Array.sub pairs 0 idx
            syncKeyedRows discColl activeKeys trimmed)

        discSink

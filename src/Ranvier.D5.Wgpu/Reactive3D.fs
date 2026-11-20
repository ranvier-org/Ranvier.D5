namespace Ranvier.D5.Wgpu

open System
open System.Collections.Generic
open System.Numerics
open System.Threading.Tasks
open Ranvier
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>
/// Monotonic reactive revision counter that automatically increments whenever any bound <c>Reactive3D</c> sink
/// uploads new or resized GPU geometry, allowing 3D plot views to subscribe to changes without manual tick signals.
/// </summary>
[<Sealed>]
type PlotRevision3D internal (revSig: Signal<int>) =
    /// <summary>Creates a new revision counter initialized to <c>0</c> in the ambient <c>Ranvier.Graph</c>.</summary>
    new() = PlotRevision3D (Api.createSignal 0)

    /// <summary>Reactive revision counter tracked when read inside computations or render selectors.</summary>
    member _.Revision: int = revSig.Value

    /// <summary>Current non-reactive revision snapshot read without creating a dependency.</summary>
    member _.Peek: int = Api.untrack (fun () -> revSig.Value)

    /// <summary>Underlying reactive signal storing the revision counter.</summary>
    member _.Signal: Signal<int> = revSig

    /// <summary>Advances the revision counter by 1 without subscribing the caller to prior values.</summary>
    member _.MarkDirty() : unit =
        let next = Api.untrack (fun () -> revSig.Value) + 1
        revSig.Value <- next

/// <summary>Declarative configuration options shared across reactive 3D GPU batch sinks.</summary>
[<NoEquality; NoComparison>]
type SinkOptions3D =
    {
        /// <summary>Explicit pick layer byte in <c>[1uy .. 255uy]</c>, or <c>ValueNone</c> to auto-allocate when <c>Registry</c> is provided (defaulting to <c>1uy</c> otherwise).</summary>
        LayerId: byte voption
        /// <summary>Optional <c>PickRegistry3D</c> used to allocate layers and auto-register pickable sinks or static <c>PickId</c>s.</summary>
        Registry: PickRegistry3D voption
        /// <summary>Optional <c>PlotRevision3D</c> automatically marked dirty whenever the bound sink uploads or resizes GPU geometry.</summary>
        Revision: PlotRevision3D voption
        /// <summary>GPU color-attachment blend mode (<c>BlendMode.Opaque</c> or <c>BlendMode.AlphaBlend</c>).</summary>
        BlendMode: BlendMode
        /// <summary>When true, disables back-face culling on hardware-instanced mesh batches.</summary>
        DoubleSided: bool
        /// <summary>When true, renders this translucent batch before hairline <c>LineBatch</c> primitives (ideal for translucent containers and back-wall panels).</summary>
        DrawBeforeLines: bool
        /// <summary>When true on keyed sinks, maintains insertion/sort order on updates using positional deltas instead of tail-swap compaction.</summary>
        PreserveOrder: bool
        /// <summary>When true on <c>bindInstancesWith</c>, transfers GPU <c>MeshHandle</c> disposal responsibility to the sink's owning Ranvier scope.</summary>
        OwnsMesh: bool
        /// <summary>Optional reactive world-space camera position reader enabling back-to-front sorting of translucent scatter discs (<c>Color.A &lt; 0.999f</c>).</summary>
        CameraPosition: (unit -> Vector3) voption
        /// <summary>When true on hairline/segment/polyline sinks, marks emitted vertices exempt from active <c>SectionPlane</c> clipping.</summary>
        IgnoreSectionPlane: bool
        /// <summary>Static <c>PickId</c> assigned to single-instance mesh sinks (<c>bindTubes</c>, <c>bindRibbons</c>, <c>bindIsoSurface</c>, <c>bindVolumeSlices</c>, or fallback <c>bindSurfaceWith</c>).</summary>
        PickId: uint32
    }

/// <summary>Default values and pipeable modifiers for <c>SinkOptions3D</c>.</summary>
[<RequireQualifiedAccess>]
module SinkOptions3D =
    /// <summary>Default sink options (<c>Opaque</c>, single-sided, <c>DrawBeforeLines = false</c>, <c>PreserveOrder = false</c>, <c>OwnsMesh = false</c>, <c>IgnoreSectionPlane = false</c>, <c>PickId = 0u</c>).</summary>
    let defaultValue: SinkOptions3D =
        {
            LayerId = ValueNone
            Registry = ValueNone
            Revision = ValueNone
            BlendMode = BlendMode.Opaque
            DoubleSided = false
            DrawBeforeLines = false
            PreserveOrder = false
            OwnsMesh = false
            CameraPosition = ValueNone
            IgnoreSectionPlane = false
            PickId = 0u
        }

    /// <summary>Assigns an explicit pick layer byte (<c>1uy .. 255uy</c>).</summary>
    let withLayerId (layerId: byte) (options: SinkOptions3D) : SinkOptions3D =
        { options with LayerId = ValueSome layerId }

    /// <summary>Attaches a <c>PickRegistry3D</c> for automatic layer allocation and sink/static pick registration.</summary>
    let withRegistry (registry: PickRegistry3D) (options: SinkOptions3D) : SinkOptions3D =
        if isNull (box registry) then nullArg (nameof registry)
        { options with Registry = ValueSome registry }

    /// <summary>Attaches a <c>PlotRevision3D</c> to mark dirty automatically on GPU geometry updates.</summary>
    let withRevision (revision: PlotRevision3D) (options: SinkOptions3D) : SinkOptions3D =
        if isNull (box revision) then nullArg (nameof revision)
        { options with Revision = ValueSome revision }

    /// <summary>Configures <c>BlendMode</c> (<c>Opaque</c> or <c>AlphaBlend</c>).</summary>
    let withBlendMode (blendMode: BlendMode) (options: SinkOptions3D) : SinkOptions3D =
        { options with BlendMode = blendMode }

    /// <summary>Enables or disables double-sided rendering on mesh batches.</summary>
    let withDoubleSided (doubleSided: bool) (options: SinkOptions3D) : SinkOptions3D =
        { options with DoubleSided = doubleSided }

    /// <summary>Configures whether an alpha-blended batch renders before hairline <c>LineBatch</c> primitives.</summary>
    let withDrawBeforeLines (drawBeforeLines: bool) (options: SinkOptions3D) : SinkOptions3D =
        { options with DrawBeforeLines = drawBeforeLines }

    /// <summary>Enables or disables strict slot order preservation across keyed mutations.</summary>
    let withPreserveOrder (preserveOrder: bool) (options: SinkOptions3D) : SinkOptions3D =
        { options with PreserveOrder = preserveOrder }

    /// <summary>Transfers caller-supplied <c>MeshHandle</c> disposal to the sink's owning scope in <c>bindInstancesWith</c>.</summary>
    let withOwnsMesh (ownsMesh: bool) (options: SinkOptions3D) : SinkOptions3D =
        { options with OwnsMesh = ownsMesh }

    /// <summary>Provides a reactive world-space camera position reader for back-to-front translucent scatter disc sorting.</summary>
    let withCameraPosition (cameraPosition: unit -> Vector3) (options: SinkOptions3D) : SinkOptions3D =
        if isNull (box cameraPosition) then nullArg (nameof cameraPosition)
        { options with CameraPosition = ValueSome cameraPosition }

    /// <summary>Sets section-plane exemption (<c>Pad = 1.0f</c>) on line-batch sinks.</summary>
    let withIgnoreSectionPlane (ignoreSectionPlane: bool) (options: SinkOptions3D) : SinkOptions3D =
        { options with IgnoreSectionPlane = ignoreSectionPlane }

    /// <summary>Assigns a static <c>PickId</c> on single-instance mesh sinks.</summary>
    let withPickId (pickId: uint32) (options: SinkOptions3D) : SinkOptions3D =
        { options with PickId = pickId }

[<Sealed>]
type internal DynamicMeshBatchHost(context: WgpuContext, options: SinkOptions3D, doubleSided: bool, initialOpacity: float32) =
    let meshHandle = context.UploadMesh MeshData.empty
    let initialIsAlpha = initialOpacity < 0.999f || options.BlendMode = BlendMode.AlphaBlend
    let initialBlend = if initialIsAlpha then BlendMode.AlphaBlend else BlendMode.Opaque
    let initialBatch = context.CreateSingleBatch (meshHandle, initialBlend, doubleSided, drawBeforeLines = options.DrawBeforeLines)
    do initialBatch.Resize 0

    let mutable opaqueBatchOpt: InstanceBatch option = if initialIsAlpha then None else Some initialBatch
    let mutable alphaBatchOpt: InstanceBatch option = if initialIsAlpha then Some initialBatch else None
    let mutable activeBatch = initialBatch
    let mutable activeIsAlpha = initialIsAlpha
    let mutable hasGeometry = false
    let mutable lastMesh: MeshData voption = ValueNone
    let mutable lastShading: ShadingMode voption = ValueNone
    let mutable lastOpacity = -1.0f
    let mutable lastPickId = 0u
    let mutable lastUseColorLut = false
    let mutable hasInstanceState = false
    let mutable disposed = false

    member _.IsDisposed = disposed
    member _.Batch: InstanceBatch = activeBatch
    member _.TriangleCount: int = if hasGeometry && not disposed then meshHandle.TriangleCount else 0

    member _.SetMesh(mesh: MeshData) : bool =
        if disposed then
            false
        else
            let sameRef =
                match lastMesh with
                | ValueSome prev -> obj.ReferenceEquals (prev, mesh)
                | ValueNone -> false

            if sameRef then
                false
            else
                lastMesh <- ValueSome mesh
                let nonEmpty = not (isNull (box mesh)) && mesh.Vertices.Length > 0 && mesh.Indices.Length > 0

                if nonEmpty then
                    meshHandle.UpdateMesh mesh
                else
                    meshHandle.UpdateMesh MeshData.empty

                let geomChanged = nonEmpty || hasGeometry
                hasGeometry <- nonEmpty
                geomChanged

    member _.ClearGeometry() : bool =
        if disposed || not hasGeometry then
            lastMesh <- ValueNone
            false
        else
            lastMesh <- ValueNone
            hasGeometry <- false
            meshHandle.UpdateMesh MeshData.empty
            activeBatch.Resize 0
            true

    member _.SyncInstance(shading: ShadingMode, opacity: float32, pickId: uint32, useColorLut: bool, forceUpload: bool) : bool =
        if disposed then
            false
        else
            Check3D.finite32 "opacity" opacity

            if opacity < 0.0f || opacity > 1.0f then
                raise (ArgumentOutOfRangeException ("opacity", "Mesh opacity must lie within [0, 1]."))

            let struct (shadeOk, roughness, emissive, baseFlags) = MarksKernel.shadingParams shading false

            if not shadeOk then
                invalidArg "shading" "ShadingMode roughness and emissive values must be finite and nonnegative."

            let flags =
                if useColorLut then baseFlags ||| InstanceFlags.UseColorLut else baseFlags

            let wantsAlpha = opacity < 0.999f || options.BlendMode = BlendMode.AlphaBlend
            let col = RgbaColor (1.0f, 1.0f, 1.0f, opacity)
            let inst = Instance3D.fromMatrix Matrix4x4.Identity col roughness pickId flags emissive

            let blendChanged = wantsAlpha <> activeIsAlpha

            let stateChanged =
                forceUpload
                || blendChanged
                || not hasInstanceState
                || (match lastShading with ValueSome s -> s <> shading | ValueNone -> true)
                || lastOpacity <> opacity
                || lastPickId <> pickId
                || lastUseColorLut <> useColorLut

            if blendChanged then
                activeBatch.Resize 0

                let nextBatch =
                    if wantsAlpha then
                        match alphaBatchOpt with
                        | Some b -> b
                        | None ->
                            let b =
                                context.CreateSingleBatch (
                                    meshHandle,
                                    BlendMode.AlphaBlend,
                                    doubleSided,
                                    drawBeforeLines = options.DrawBeforeLines,
                                    instance = inst
                                )
                            alphaBatchOpt <- Some b
                            b
                    else
                        match opaqueBatchOpt with
                        | Some b -> b
                        | None ->
                            let b =
                                context.CreateSingleBatch (
                                    meshHandle,
                                    BlendMode.Opaque,
                                    doubleSided,
                                    drawBeforeLines = options.DrawBeforeLines,
                                    instance = inst
                                )
                            opaqueBatchOpt <- Some b
                            b

                activeBatch <- nextBatch
                activeIsAlpha <- wantsAlpha

            if stateChanged then
                lastShading <- ValueSome shading
                lastOpacity <- opacity
                lastPickId <- pickId
                lastUseColorLut <- useColorLut
                hasInstanceState <- true

                if hasGeometry then
                    activeBatch.SetSingle inst
                else
                    activeBatch.Resize 0

                hasGeometry || blendChanged || forceUpload
            else
                false

    member _.Dispose() : unit =
        if not disposed then
            disposed <- true

            match opaqueBatchOpt with
            | Some b -> b.Dispose ()
            | None -> ()

            match alphaBatchOpt with
            | Some b -> b.Dispose ()
            | None -> ()

            meshHandle.Dispose ()

    interface IDisposable with
        member this.Dispose() = this.Dispose ()

/// <summary>
/// Reactive GPU isosurface sink managing non-blocking <c>AsyncMemo</c> computation, <c>MeshHandle</c> updates,
/// and dynamic <c>Opaque</c>/<c>AlphaBlend</c> single-instance batch switching.
/// </summary>
[<Sealed>]
type IsoSurfaceSink internal
    (
        memo: AsyncMemo<Result<MeshData, LayoutError list>>,
        host: DynamicMeshBatchHost,
        readLatestError: unit -> LayoutError list voption
    ) =
    /// <summary>Current active <c>InstanceBatch</c> (<c>Opaque</c> or <c>AlphaBlend</c>) rendering the settled isosurface mesh.</summary>
    member _.Batch: InstanceBatch = host.Batch
    /// <summary>Underlying cancellable <c>AsyncMemo</c> computing the CPU triangle mesh.</summary>
    member _.Memo: AsyncMemo<Result<MeshData, LayoutError list>> = memo
    /// <summary>Non-blocking current <c>Reading</c> state of the background isosurface computation.</summary>
    member _.TryValue: Reading<Result<MeshData, LayoutError list>> = memo.TryValue
    /// <summary>Number of GPU triangles currently uploaded for the latest settled mesh.</summary>
    member _.TriangleCount: int = host.TriangleCount
    /// <summary>Latest validation or budget error emitted by marching cubes extraction, if any.</summary>
    member _.LatestError: LayoutError list voption = readLatestError ()

    /// <summary>Disposes owned GPU mesh, batches, and async memo resources.</summary>
    member _.Dispose() : unit =
        host.Dispose ()
        memo.Dispose ()

    interface IDisposable with
        member this.Dispose() = this.Dispose ()

/// <summary>
/// Reactive GPU volume cross-section slice sink managing double-sided <c>MeshHandle</c> updates
/// and dynamic <c>Opaque</c>/<c>AlphaBlend</c> single-instance batch switching.
/// </summary>
[<Sealed>]
type VolumeSliceSink internal (host: DynamicMeshBatchHost, readLatestError: unit -> LayoutError list voption) =
    /// <summary>Current active double-sided <c>InstanceBatch</c> (<c>Opaque</c> or <c>AlphaBlend</c>) rendering the extracted slices.</summary>
    member _.Batch: InstanceBatch = host.Batch
    /// <summary>Number of GPU triangles currently uploaded for the extracted slice planes.</summary>
    member _.TriangleCount: int = host.TriangleCount
    /// <summary>Latest slice-index or grid validation error, if any.</summary>
    member _.LatestError: LayoutError list voption = readLatestError ()

    /// <summary>Disposes owned GPU mesh and slice batch resources.</summary>
    member _.Dispose() : unit = host.Dispose ()

    interface IDisposable with
        member this.Dispose() = this.Dispose ()

/// <summary>Ranvier reactive scales, editables, incremental keyed GPU batch sinks, and async volumetric computations.</summary>
[<RequireQualifiedAccess>]
module Reactive3D =
    let inline private markRevisionDirty (revision: PlotRevision3D voption) =
        match revision with
        | ValueSome rev -> rev.MarkDirty ()
        | ValueNone -> ()

    let private requireCommon (context: WgpuContext) (scale: unit -> Scale3D) =
        if isNull (box context) then
            nullArg (nameof context)

        if isNull (box scale) then
            nullArg (nameof scale)

    let private requireInstanceInputs
        (context: WgpuContext)
        (layerId: byte)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, 'Datum>)
        =
        requireCommon context scale
        PickEncoding.validateLayerId layerId

        if isNull (box rows) then
            nullArg (nameof rows)

    let private resolveLayerId (options: SinkOptions3D) : byte =
        match options.LayerId with
        | ValueSome lid ->
            PickEncoding.validateLayerId lid
            lid
        | ValueNone ->
            match options.Registry with
            | ValueSome reg -> reg.AllocateLayer ()
            | ValueNone -> 1uy

    let private registerKeyedSinkIfNeeded (options: SinkOptions3D) (sink: IKeyedPickResolver<'K>) =
        match options.Registry with
        | ValueSome reg -> reg.RegisterSink sink
        | ValueNone -> ()

    let private registerStaticPickIfNeeded (options: SinkOptions3D) (pickId: uint32) =
        match options.Registry with
        | ValueSome reg when pickId <> 0u -> reg.RegisterStaticPick pickId
        | _ -> ()

    let private attachSyncedEffect
        (rows: Projection<'K, 'Datum>)
        (sync: KeyedSlotSync<'K, 'Datum, 'Gpu>)
        (revision: PlotRevision3D voption)
        =
        let reader = rows.NewValueReader ()

        Api.createEffect (fun () ->
            let delta = reader.Read ()
            let prevActive = sync.ActiveCount
            sync.Sync delta

            match revision with
            | ValueSome rev when sync.LastUpdatedSlots > 0 || sync.ActiveCount <> prevActive -> rev.MarkDirty ()
            | _ -> ())

    let private sortDiscsBackToFront
        (camPos: Vector3)
        (src: DiscInstance3D array)
        (count: int)
        (orderIdx: byref<int array>)
        (distSq: byref<float32 array>)
        (out: byref<DiscInstance3D array>)
        : ReadOnlySpan<DiscInstance3D> =
        if orderIdx.Length < count then
            let newCap = max count (max 16 (orderIdx.Length * 2))
            orderIdx <- Array.zeroCreate<int> newCap
            distSq <- Array.zeroCreate<float32> newCap
            out <- Array.zeroCreate<DiscInstance3D> newCap

        for i = 0 to count - 1 do
            orderIdx[i] <- i
            distSq[i] <- MarksKernel.distSqTo src[i].CenterRadiusPx camPos

        let idxArr = orderIdx
        let dArr = distSq

        let insertionDesc lo hi =
            for i = lo + 1 to hi do
                let key = idxArr[i]
                let keyD = dArr[key]
                let mutable j = i - 1

                while j >= lo && dArr[idxArr[j]] < keyD do
                    idxArr[j + 1] <- idxArr[j]
                    j <- j - 1

                idxArr[j + 1] <- key

        let rec qsortDesc lo hi =
            if hi - lo <= 16 then
                insertionDesc lo hi
            else
                let mid = lo + ((hi - lo) >>> 1)
                let swap a b =
                    let t = idxArr[a]
                    idxArr[a] <- idxArr[b]
                    idxArr[b] <- t

                if dArr[idxArr[mid]] > dArr[idxArr[lo]] then swap lo mid
                if dArr[idxArr[hi]] > dArr[idxArr[lo]] then swap lo hi
                if dArr[idxArr[hi]] > dArr[idxArr[mid]] then swap mid hi
                let pivotD = dArr[idxArr[mid]]
                let mutable i = lo
                let mutable j = hi

                while i <= j do
                    while dArr[idxArr[i]] > pivotD do i <- i + 1
                    while dArr[idxArr[j]] < pivotD do j <- j - 1

                    if i <= j then
                        swap i j
                        i <- i + 1
                        j <- j - 1

                if lo < j then qsortDesc lo j
                if i < hi then qsortDesc i hi

        if count > 1 then
            qsortDesc 0 (count - 1)

        for i = 0 to count - 1 do
            out[i] <- src[idxArr[i]]

        MarksKernel.spanSlice out 0 count

    let private bindInstanceProjection
        (context: WgpuContext)
        (layerId: byte)
        (mesh: MeshHandle)
        (ownsMesh: bool)
        (blendMode: BlendMode)
        (doubleSided: bool)
        (drawBeforeLines: bool)
        (preserveOrder: bool)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, 'Datum>)
        (packSlot: Scale3D -> uint32 -> 'Datum -> Instance3D array -> int -> bool)
        (revision: PlotRevision3D voption)
        (registry: PickRegistry3D voption)
        : KeyedInstanceSink<'K> =
        if ownsMesh then
            OwnerCleanup.tryOnCleanup (fun () -> mesh.Dispose ())

        let batch = context.CreateBatch (mesh, blendMode, doubleSided, drawBeforeLines = drawBeforeLines)
        OwnerCleanup.tryOnCleanup (fun () -> batch.Dispose ())

        let sync =
            KeyedSlotSync<'K, 'Datum, Instance3D>(
                layerId,
                1,
                preserveOrder,
                scale,
                rows,
                packSlot,
                (fun arr slot pickId -> arr[slot].PickId <- pickId),
                (fun arr count -> batch.UpdateInstances (MarksKernel.spanSlice arr 0 count)),
                (fun startIdx arr srcStart count -> batch.UpdateRange (startIdx, MarksKernel.spanSlice arr srcStart count)),
                (fun count -> batch.Resize count)
            )

        attachSyncedEffect rows sync revision
        let sink = KeyedInstanceSink<'K>(layerId, batch, sync :> IKeyedSinkSync<'K>)

        match registry with
        | ValueSome reg -> reg.RegisterSink sink
        | ValueNone -> ()

        sink

    /// <summary>Creates a new <c>PlotRevision3D</c> signal bumped automatically by bound <c>Reactive3D</c> sinks.</summary>
    let createRevision () : PlotRevision3D =
        PlotRevision3D ()

    /// <summary>Unwraps tuple-keyed projection rows (<c>struct (k, datum)</c>) to their datum payload.</summary>
    let values (rows: Projection<'K, struct ('A * 'Datum)>) : Projection<'K, 'Datum> =
        if isNull (box rows) then
            nullArg (nameof rows)

        rows |> Projection.map (fun struct (_, datum) -> datum)

    /// <summary>Projects a tuple-keyed <c>KeyedCollection</c> directly to its unwrapped datum values.</summary>
    let collectionValues (collection: KeyedCollection<'K, struct ('A * 'Datum)>) : Projection<'K, 'Datum> =
        if isNull (box collection) then
            nullArg (nameof collection)

        values collection.Rows

    /// <summary>Exposes a <c>KeyedCollection</c>'s reactive <c>Rows</c> projection directly.</summary>
    let collectionRows (collection: KeyedCollection<'K, 'Datum>) : Projection<'K, 'Datum> =
        if isNull (box collection) then
            nullArg (nameof collection)

        collection.Rows

    /// <summary>
    /// Tracks the minimum and maximum finite values emitted by <c>read</c> over a <c>Projection</c> using <c>Projection.tryMinBy</c>
    /// and <c>Projection.tryMaxBy</c>, suppressing downstream wakeups on interior edits via structural option cutoff.
    /// </summary>
    let domainExtent (read: 'T -> float option) (rows: Projection<'K, 'T>) : Memo<(float * float) option> =
        if isNull (box read) then
            nullArg (nameof read)

        if isNull (box rows) then
            nullArg (nameof rows)

        let finiteValues =
            rows
            |> Projection.choose (fun row ->
                match read row with
                | Some v when Check3D.isFinite v -> Some v
                | _ -> None)

        let minMemo = finiteValues |> Projection.tryMinBy id
        let maxMemo = finiteValues |> Projection.tryMaxBy id

        Api.createOptionMemo (fun () ->
            match minMemo.Value, maxMemo.Value with
            | Some lo, Some hi -> Some (lo, hi)
            | _ -> None)

    /// <summary>
    /// Derives a nice-rounded <c>Ranvier.D5.Continuous</c> scale over a <c>Projection</c>, falling back to <c>fallbackDomain</c>
    /// when empty and preserving reference identity when the nice bounds do not change.
    /// </summary>
    let niceContinuousScale
        (tickCount: int)
        (fallbackDomain: float * float)
        (range: float * float)
        (read: 'T -> float option)
        (rows: Projection<'K, 'T>)
        : Memo<Continuous> =
        if tickCount < 0 || tickCount > 10000 then
            invalidArg (nameof tickCount) "tickCount must lie in [0 .. 10000]."

        Check3D.interval (nameof fallbackDomain) fallbackDomain
        Check3D.interval (nameof range) range

        let extentMemo = domainExtent read rows
        let mutable hasLast = false
        let mutable lastLo = 0.0
        let mutable lastHi = 0.0

        Api.createMemo (fun prev ->
            let lo, hi =
                match extentMemo.Value with
                | None -> fallbackDomain
                | Some ext ->
                    let unitNice = Scale.linear ext (0.0, 1.0) |> Scale.nice tickCount
                    Scale.invert unitNice 0.0, Scale.invert unitNice 1.0

            match prev with
            | ValueSome kept when hasLast && lo = lastLo && hi = lastHi -> kept
            | _ ->
                hasLast <- true
                lastLo <- lo
                lastHi <- hi
                Scale.linear (lo, hi) range)

    /// <summary>Creates an <c>Editable&lt;PlotCamera3D&gt;</c> accepting local pointer/wheel overrides until reset or superseded by an unequal upstream seed.</summary>
    let editableCamera (seed: unit -> PlotCamera3D) : Editable<PlotCamera3D> =
        if isNull (box seed) then
            nullArg (nameof seed)

        Api.createEditable (fun _ -> seed ())

    /// <summary>Creates an <c>Editable&lt;SectionPlane&gt;</c> accepting local interactive plane overrides until reset or superseded by an unequal upstream seed.</summary>
    let editableSection (seed: unit -> SectionPlane) : Editable<SectionPlane> =
        if isNull (box seed) then
            nullArg (nameof seed)

        Api.createEditable (fun _ -> seed ())

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, GlyphDatum3D&gt;</c> incrementally to a GPU <c>InstanceBatch</c> using <c>ProjectionDelta</c>
    /// range uploads, optional order preservation, layer-encoded <c>PickId</c> tracking, and owning-scope cleanup.
    /// </summary>
    let bindInstances
        (context: WgpuContext)
        (layerId: byte)
        (mesh: MeshHandle)
        (blendMode: BlendMode)
        (doubleSided: bool)
        (preserveOrder: bool)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, GlyphDatum3D>)
        : KeyedInstanceSink<'K> =
        requireInstanceInputs context layerId scale rows

        if isNull (box mesh) then
            nullArg (nameof mesh)

        bindInstanceProjection
            context
            layerId
            mesh
            false
            blendMode
            doubleSided
            false
            preserveOrder
            scale
            rows
            MarksKernel.tryPackGlyph
            ValueNone
            ValueNone

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, GlyphDatum3D&gt;</c> to a GPU <c>InstanceBatch</c> using declarative <c>SinkOptions3D</c>
    /// (automatic registry allocation/registration, revision bumping, draw ordering, and optional mesh ownership).
    /// </summary>
    let bindInstancesWith
        (context: WgpuContext)
        (options: SinkOptions3D)
        (mesh: MeshHandle)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, GlyphDatum3D>)
        : KeyedInstanceSink<'K> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box mesh) then nullArg (nameof mesh)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box rows) then nullArg (nameof rows)

        let layerId = resolveLayerId options

        bindInstanceProjection
            context
            layerId
            mesh
            options.OwnsMesh
            options.BlendMode
            options.DoubleSided
            options.DrawBeforeLines
            options.PreserveOrder
            scale
            rows
            MarksKernel.tryPackGlyph
            options.Revision
            options.Registry

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, GlyphDatum3D&gt;</c> directly from a standard <c>GlyphShape3D</c>, uploading and
    /// owning the unit shape mesh automatically alongside full <c>SinkOptions3D</c> support.
    /// </summary>
    let bindGlyphs
        (context: WgpuContext)
        (shape: GlyphShape3D)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, GlyphDatum3D>)
        : KeyedInstanceSink<'K> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box rows) then nullArg (nameof rows)

        let layerId = resolveLayerId options
        let mesh = context.UploadMesh (Glyph3D.unitMesh shape)

        bindInstanceProjection
            context
            layerId
            mesh
            true
            options.BlendMode
            options.DoubleSided
            options.DrawBeforeLines
            options.PreserveOrder
            scale
            rows
            MarksKernel.tryPackGlyph
            options.Revision
            options.Registry

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, BarDatum3D&gt;</c> incrementally to an opaque box <c>InstanceBatch</c> with automatic unit-box mesh
    /// upload/disposal, <c>O(changed)</c> <c>UpdateRange</c> uploads, and tail-swap <c>PickId</c> retirement.
    /// </summary>
    let bindBars
        (context: WgpuContext)
        (layerId: byte)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, BarDatum3D>)
        : KeyedInstanceSink<'K> =
        requireInstanceInputs context layerId scale rows
        let unitBox = context.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Box)

        bindInstanceProjection
            context
            layerId
            unitBox
            true
            BlendMode.Opaque
            false
            false
            false
            scale
            rows
            MarksKernel.tryPackBar
            ValueNone
            ValueNone

    /// <summary>Binds a keyed <c>Projection&lt;'K, BarDatum3D&gt;</c> with full <c>SinkOptions3D</c> support (registry, revision, blending, ordering).</summary>
    let bindBarsWith
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, BarDatum3D>)
        : KeyedInstanceSink<'K> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box rows) then nullArg (nameof rows)

        let layerId = resolveLayerId options
        let unitBox = context.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Box)

        bindInstanceProjection
            context
            layerId
            unitBox
            true
            options.BlendMode
            options.DoubleSided
            options.DrawBeforeLines
            options.PreserveOrder
            scale
            rows
            MarksKernel.tryPackBar
            options.Revision
            options.Registry

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, BoxDatum3D&gt;</c> to an owned unit-box <c>InstanceBatch</c> honoring <c>YUp</c>/<c>ZUp</c>,
    /// <c>BlendMode.AlphaBlend</c>, and <c>DrawBeforeLines</c> for hierarchical 3D container boxes.
    /// </summary>
    let bindBoxes
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, BoxDatum3D>)
        : KeyedInstanceSink<'K> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box rows) then nullArg (nameof rows)

        let layerId = resolveLayerId options
        let unitBox = context.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Box)

        bindInstanceProjection
            context
            layerId
            unitBox
            true
            options.BlendMode
            options.DoubleSided
            options.DrawBeforeLines
            options.PreserveOrder
            scale
            rows
            MarksKernel.tryPackBox
            options.Revision
            options.Registry

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, ArrowDatum3D&gt;</c> to an owned <c>Glyph3D.unitArrowMesh segments</c> batch;
    /// requires <c>segments &gt;= 3</c>.
    /// </summary>
    let bindArrows
        (context: WgpuContext)
        (segments: int)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, ArrowDatum3D>)
        : KeyedInstanceSink<'K> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box rows) then nullArg (nameof rows)

        let arrowMeshData = Glyph3D.unitArrowMesh segments
        let layerId = resolveLayerId options
        let arrowMesh = context.UploadMesh arrowMeshData

        bindInstanceProjection
            context
            layerId
            arrowMesh
            true
            options.BlendMode
            options.DoubleSided
            options.DrawBeforeLines
            options.PreserveOrder
            scale
            rows
            MarksKernel.tryPackArrow
            options.Revision
            options.Registry

    let private bindDiscsCore
        (context: WgpuContext)
        (layerId: byte)
        (preserveOrder: bool)
        (cameraPosOpt: (unit -> Vector3) voption)
        (revision: PlotRevision3D voption)
        (registry: PickRegistry3D voption)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, ScatterDatum3D>)
        : KeyedDiscSink<'K> =
        let batch = context.CreateDiscBatch ()
        OwnerCleanup.tryOnCleanup (fun () -> batch.Dispose ())

        match cameraPosOpt with
        | ValueNone ->
            let sync =
                KeyedSlotSync<'K, ScatterDatum3D, DiscInstance3D>(
                    layerId,
                    1,
                    preserveOrder,
                    scale,
                    rows,
                    MarksKernel.tryPackDisc,
                    (fun arr slot pickId -> arr[slot].PickId <- pickId),
                    (fun arr count -> batch.UpdateDiscs (MarksKernel.spanSlice arr 0 count)),
                    (fun startIdx arr srcStart count -> batch.UpdateRange (startIdx, MarksKernel.spanSlice arr srcStart count)),
                    (fun count -> batch.Resize count)
                )

            attachSyncedEffect rows sync revision
            let sink = KeyedDiscSink<'K>(layerId, batch, sync :> IKeyedSinkSync<'K>)
            registry |> ValueOption.iter (fun reg -> reg.RegisterSink sink)
            sink
        | ValueSome readCamPos ->
            let mutable canonicalShadow = Array.empty<DiscInstance3D>
            let mutable canonicalCount = 0
            let mutable orderIdx = Array.empty<int>
            let mutable distSq = Array.empty<float32>
            let mutable sortScratch = Array.empty<DiscInstance3D>
            let mutable sortedActive = false
            let mutable hasLastCam = false
            let mutable lastCam = Vector3.Zero

            let sync =
                KeyedSlotSync<'K, ScatterDatum3D, DiscInstance3D>(
                    layerId,
                    1,
                    preserveOrder,
                    scale,
                    rows,
                    MarksKernel.tryPackDisc,
                    (fun arr slot pickId -> arr[slot].PickId <- pickId),
                    (fun arr count ->
                        canonicalShadow <- arr
                        canonicalCount <- count),
                    (fun _ arr _ _ -> canonicalShadow <- arr),
                    (fun count -> canonicalCount <- count)
                )

            let flushDiscs (dataChanged: bool) (camPos: Vector3) =
                Check3D.vector3 "cameraPosition" camPos
                let count = sync.ActiveCount
                canonicalCount <- count

                let mutable anyTranslucent = false
                let mutable i = 0

                while not anyTranslucent && i < count do
                    if canonicalShadow[i].FillColor.W < 0.999f then
                        anyTranslucent <- true

                    i <- i + 1

                if count = 0 then
                    if dataChanged then
                        batch.Resize 0

                    sortedActive <- false
                    hasLastCam <- true
                    lastCam <- camPos
                    false
                elif anyTranslucent && count > 1 then
                    if dataChanged || not sortedActive || not hasLastCam || camPos <> lastCam then
                        let span = sortDiscsBackToFront camPos canonicalShadow count &orderIdx &distSq &sortScratch
                        batch.UpdateDiscs span
                        sortedActive <- true
                        hasLastCam <- true
                        lastCam <- camPos
                        true
                    else
                        false
                else
                    if dataChanged || sortedActive then
                        batch.UpdateDiscs (MarksKernel.spanSlice canonicalShadow 0 count)
                        sortedActive <- false
                        hasLastCam <- true
                        lastCam <- camPos
                        true
                    else
                        hasLastCam <- true
                        lastCam <- camPos
                        false

            let reader = rows.NewValueReader ()

            Api.createEffect (fun () ->
                let delta = reader.Read ()
                let prevActive = sync.ActiveCount
                sync.Sync delta
                let dataChanged = sync.LastUpdatedSlots > 0 || sync.ActiveCount <> prevActive
                let camPos = Api.untrack readCamPos
                flushDiscs dataChanged camPos |> ignore

                if dataChanged then
                    markRevisionDirty revision)

            let mutable camInit = false

            Api.createEffect (fun () ->
                let nextCam = readCamPos ()

                if not camInit then
                    camInit <- true
                    Check3D.vector3 "cameraPosition" nextCam
                else if flushDiscs false nextCam then
                    markRevisionDirty revision)

            let sink = KeyedDiscSink<'K>(layerId, batch, sync :> IKeyedSinkSync<'K>)
            registry |> ValueOption.iter (fun reg -> reg.RegisterSink sink)
            sink

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, ScatterDatum3D&gt;</c> incrementally to a billboard <c>DiscBatch</c> using <c>ProjectionDelta</c>
    /// range uploads, optional order preservation, layer-encoded <c>PickId</c> tracking, and owning-scope cleanup.
    /// </summary>
    let bindDiscs
        (context: WgpuContext)
        (layerId: byte)
        (preserveOrder: bool)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, ScatterDatum3D>)
        : KeyedDiscSink<'K> =
        requireInstanceInputs context layerId scale rows
        bindDiscsCore context layerId preserveOrder ValueNone ValueNone ValueNone scale rows

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, ScatterDatum3D&gt;</c> to a <c>DiscBatch</c> using <c>SinkOptions3D</c>, supporting
    /// <c>PickRegistry3D</c>, <c>PlotRevision3D</c>, and automatic back-to-front translucent depth sorting when <c>CameraPosition</c> is provided.
    /// </summary>
    let bindDiscsWith
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, ScatterDatum3D>)
        : KeyedDiscSink<'K> =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box rows) then nullArg (nameof rows)

        let layerId = resolveLayerId options
        bindDiscsCore context layerId options.PreserveOrder options.CameraPosition options.Revision options.Registry scale rows

    let private bindSegmentsCore
        (context: WgpuContext)
        (ignoreSectionPlane: bool)
        (preserveOrder: bool)
        (revision: PlotRevision3D voption)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, struct (Point3 * Point3 * RgbaColor)>)
        : KeyedSegmentSink<'K> =
        let batch = context.CreateLineBatch ()
        OwnerCleanup.tryOnCleanup (fun () -> batch.Dispose ())

        let sync =
            KeyedSlotSync<'K, struct (Point3 * Point3 * RgbaColor), LineVertex3D>(
                0uy,
                2,
                preserveOrder,
                scale,
                rows,
                (fun s _ pickDatum out vertexSlot -> MarksKernel.tryPackSegmentWithSection ignoreSectionPlane s pickDatum out vertexSlot),
                (fun _ _ _ -> ()),
                (fun arr vertexCount -> batch.UpdateVertices (MarksKernel.spanSlice arr 0 vertexCount)),
                (fun startVertex arr srcStart vertexCount -> batch.UpdateRange (startVertex, MarksKernel.spanSlice arr srcStart vertexCount)),
                (fun vertexCount -> batch.Resize vertexCount)
            )

        attachSyncedEffect rows sync revision
        KeyedSegmentSink<'K>(batch, sync :> IKeyedSinkSync<'K>)

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, struct (Point3 * Point3 * RgbaColor)&gt;</c> incrementally to a 4x-MSAA hairline <c>LineBatch</c>
    /// with <c>O(changed)</c> <c>UpdateRange</c> uploads and owning-scope disposal.
    /// </summary>
    let bindSegments
        (context: WgpuContext)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, struct (Point3 * Point3 * RgbaColor)>)
        : KeyedSegmentSink<'K> =
        requireCommon context scale

        if isNull (box rows) then
            nullArg (nameof rows)

        bindSegmentsCore context false false ValueNone scale rows

    /// <summary>Binds a keyed segment <c>Projection</c> to a <c>LineBatch</c> honoring <c>SinkOptions3D.IgnoreSectionPlane</c>, <c>PreserveOrder</c>, and <c>Revision</c>.</summary>
    let bindSegmentsWith
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, struct (Point3 * Point3 * RgbaColor)>)
        : KeyedSegmentSink<'K> =
        requireCommon context scale

        if isNull (box options) then
            nullArg (nameof options)

        if isNull (box rows) then
            nullArg (nameof rows)

        bindSegmentsCore context options.IgnoreSectionPlane options.PreserveOrder options.Revision scale rows

    [<Sealed>]
    type private LineScratchBuffer(initialCap: int) =
        let mutable buf = Array.zeroCreate<LineVertex3D> initialCap
        let mutable count = 0

        member _.Count = count
        member _.Reset() = count <- 0

        member _.EnsureExtra(extraCount: int) =
            let needed = count + extraCount

            if needed > buf.Length then
                let nextCap = max needed (max 64 (buf.Length * 2))
                let nextBuf = Array.zeroCreate<LineVertex3D> nextCap
                if count > 0 then Array.Copy (buf, nextBuf, count)
                buf <- nextBuf

        member _.AddSegment(a: Vector3, colA: RgbaColor, b: Vector3, colB: RgbaColor, ignoreSectionPlane: bool) =
            buf[count] <- LineVertex3D (a, colA, ignoreSectionPlane)
            buf[count + 1] <- LineVertex3D (b, colB, ignoreSectionPlane)
            count <- count + 2

        member _.Span() : ReadOnlySpan<LineVertex3D> =
            MarksKernel.spanSlice buf 0 count

        member _.CopyToArray() : LineVertex3D array =
            if count = 0 then Array.empty else Array.sub buf 0 count

    let private addPolylineStep
        (ignoreSectionPlane: bool)
        (scale: Scale3D)
        (pt: Point3)
        (col: RgbaColor)
        (prev: struct (Vector3 * RgbaColor) voption)
        (scratch: LineScratchBuffer)
        : struct (Vector3 * RgbaColor) voption =
        if MarksKernel.isFinitePoint3 pt && MarksKernel.isFiniteColor col then
            match Scale3D.tryMap scale pt with
            | ValueSome w ->
                match prev with
                | ValueSome struct (prevW, prevCol) ->
                    scratch.AddSegment (prevW, prevCol, w, col, ignoreSectionPlane)
                | ValueNone -> ()

                ValueSome struct (w, col)
            | ValueNone -> ValueNone
        else
            ValueNone

    let private appendRunPolyline
        (ignoreSectionPlane: bool)
        (scale: Scale3D)
        (points: IReadOnlyList<struct (Point3 * RgbaColor)>)
        (scratch: LineScratchBuffer)
        =
        let n = points.Count
        scratch.EnsureExtra ((max 0 (n - 1)) * 2)
        let mutable prev: struct (Vector3 * RgbaColor) voption = ValueNone

        for i = 0 to n - 1 do
            let struct (pt, col) = points[i]
            prev <- addPolylineStep ignoreSectionPlane scale pt col prev scratch

    let private appendGapPolyline
        (ignoreSectionPlane: bool)
        (scale: Scale3D)
        (points: IReadOnlyList<struct (Point3 * RgbaColor) voption>)
        (scratch: LineScratchBuffer)
        =
        let n = points.Count
        scratch.EnsureExtra ((max 0 (n - 1)) * 2)
        let mutable prev: struct (Vector3 * RgbaColor) voption = ValueNone

        for i = 0 to n - 1 do
            match points[i] with
            | ValueNone -> prev <- ValueNone
            | ValueSome struct (pt, col) ->
                prev <- addPolylineStep ignoreSectionPlane scale pt col prev scratch

    let private bindPolylineSeqCore
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (polylines: unit -> 'Seq)
        (appendOne: bool -> Scale3D -> 'Poly -> LineScratchBuffer -> unit)
        : LineBatch when 'Seq :> seq<'Poly> =
        requireCommon context scale

        if isNull (box options) then
            nullArg (nameof options)

        if isNull (box polylines) then
            nullArg (nameof polylines)

        let batch = context.CreateLineBatch ()
        OwnerCleanup.tryOnCleanup (fun () -> batch.Dispose ())
        let scratch = LineScratchBuffer 64
        let mutable hadNonEmpty = false

        Api.createEffect (fun () ->
            let s = scale ()
            let items = polylines ()

            if isNull (box s) then
                nullArg "scale"

            scratch.Reset ()

            if not (isNull (box items)) then
                for poly in items do
                    if not (isNull (box poly)) then
                        appendOne options.IgnoreSectionPlane s poly scratch

            if scratch.Count > 0 then
                batch.UpdateVertices (scratch.Span ())
                hadNonEmpty <- true
                markRevisionDirty options.Revision
            elif hadNonEmpty then
                batch.Resize 0
                hadNonEmpty <- false
                markRevisionDirty options.Revision)

        batch

    /// <summary>Binds a reactive sequence of colored 3D polylines into a 4x-MSAA <c>LineBatch</c> honoring <c>IgnoreSectionPlane</c> and <c>Revision</c>.</summary>
    let bindPolylines
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (polylines: unit -> #seq<#IReadOnlyList<struct (Point3 * RgbaColor)>>)
        : LineBatch =
        bindPolylineSeqCore context options scale polylines (fun exempt s poly scratch ->
            appendRunPolyline exempt s (poly :> IReadOnlyList<struct (Point3 * RgbaColor)>) scratch)

    /// <summary>Binds a reactive sequence of gap-aware (<c>ValueNone</c>) 3D polylines into a 4x-MSAA <c>LineBatch</c>.</summary>
    let bindPolylinesWithGaps
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (polylines: unit -> #seq<#IReadOnlyList<struct (Point3 * RgbaColor) voption>>)
        : LineBatch =
        bindPolylineSeqCore context options scale polylines (fun exempt s poly scratch ->
            appendGapPolyline exempt s (poly :> IReadOnlyList<struct (Point3 * RgbaColor) voption>) scratch)

    /// <summary>
    /// Binds a keyed <c>Projection</c> of gap-aware 3D polylines incrementally to a <c>LineBatch</c>, caching packed vertices
    /// per key so single-key edits only re-tessellate the changed polyline.
    /// </summary>
    let bindKeyedPolylines
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, #IReadOnlyList<struct (Point3 * RgbaColor) voption>>)
        : KeyedPolylineSink<'K> =
        requireCommon context scale

        if isNull (box options) then
            nullArg (nameof options)

        if isNull (box rows) then
            nullArg (nameof rows)

        let batch = context.CreateLineBatch ()
        OwnerCleanup.tryOnCleanup (fun () -> batch.Dispose ())

        let cache = Dictionary<'K, LineVertex3D array>(HashIdentity.Structural)
        let mutable hasNullKey = false
        let mutable nullVerts = Array.empty<LineVertex3D>
        let mutable orderedKeys = Array.empty<'K>
        let mutable activeKeyCount = 0
        let mutable scratch = Array.zeroCreate<LineVertex3D> 64
        let keyTemp = LineScratchBuffer 32
        let mutable initialized = false
        let mutable lastScale = Unchecked.defaultof<Scale3D>
        let mutable hadNonEmpty = false

        let packOne (s: Scale3D) (key: 'K) : LineVertex3D array =
            let datumOpt =
                Api.untrack (fun () ->
                    try
                        rows.TryGet key
                    with NotReadyException _ ->
                        None)

            match datumOpt with
            | Some poly when not (isNull (box poly)) ->
                keyTemp.Reset ()
                appendGapPolyline options.IgnoreSectionPlane s (poly :> IReadOnlyList<struct (Point3 * RgbaColor) voption>) keyTemp
                keyTemp.CopyToArray ()
            | _ -> Array.empty

        let setEntry (k: 'K) (verts: LineVertex3D array) =
            if isNull (box k) then
                hasNullKey <- true
                nullVerts <- verts
            else
                cache[k] <- verts

        let removeEntry (k: 'K) =
            if isNull (box k) then
                hasNullKey <- false
                nullVerts <- Array.empty
            else
                cache.Remove k |> ignore

        let tryGetEntry (k: 'K) =
            if isNull (box k) then
                if hasNullKey then ValueSome nullVerts else ValueNone
            else
                match cache.TryGetValue k with
                | true, v -> ValueSome v
                | _ -> ValueNone

        let reader = rows.NewValueReader ()

        Api.createEffect (fun () ->
            let delta = reader.Read ()
            let s = scale ()

            if isNull (box s) then
                nullArg "scale"

            let scaleChanged = not (obj.ReferenceEquals (s, lastScale))
            let changed = not initialized || delta.IsReset || scaleChanged || delta.Changes.Count > 0 || delta.OrderChanged

            if changed then
                if not initialized || delta.IsReset || scaleChanged then
                    initialized <- true
                    lastScale <- s
                    hasNullKey <- false
                    nullVerts <- Array.empty
                    cache.Clear ()

                    for i = 0 to delta.Keys.Length - 1 do
                        let k = delta.Keys[i]
                        setEntry k (packOne s k)
                else
                    for kvp in delta.Changes do
                        match kvp.Value with
                        | KeyChange.Removed -> removeEntry kvp.Key
                        | _ -> setEntry kvp.Key (packOne s kvp.Key)

                orderedKeys <- delta.Keys
                activeKeyCount <- delta.Keys.Length

                let mutable totalVerts = 0

                for i = 0 to delta.Keys.Length - 1 do
                    match tryGetEntry delta.Keys[i] with
                    | ValueSome arr -> totalVerts <- totalVerts + arr.Length
                    | ValueNone -> ()

                if totalVerts > 0 then
                    if totalVerts > scratch.Length then
                        scratch <- Array.zeroCreate<LineVertex3D> (max totalVerts (scratch.Length * 2))

                    let mutable offset = 0

                    for i = 0 to orderedKeys.Length - 1 do
                        match tryGetEntry orderedKeys[i] with
                        | ValueSome arr when arr.Length > 0 ->
                            Array.Copy (arr, 0, scratch, offset, arr.Length)
                            offset <- offset + arr.Length
                        | _ -> ()

                    batch.UpdateVertices (MarksKernel.spanSlice scratch 0 totalVerts)
                    hadNonEmpty <- true
                    markRevisionDirty options.Revision
                elif hadNonEmpty then
                    batch.Resize 0
                    hadNonEmpty <- false
                    markRevisionDirty options.Revision)

        KeyedPolylineSink<'K>(batch, fun () -> activeKeyCount)

    let private bindSurfaceCore
        (context: WgpuContext)
        (options: SinkOptions3D)
        (spec: unit -> SurfaceSpec)
        (grid: unit -> SurfaceGrid)
        : SurfaceBatch =
        let initialSpecRaw = Api.untrack spec
        let initialSpec =
            if options.PickId <> 0u && initialSpecRaw.PickId = 0u then
                { initialSpecRaw with PickId = options.PickId }
            else
                initialSpecRaw

        let initialGrid = Api.untrack grid
        let surfaceBatch = SurfaceBatch.Create (context, initialSpec, initialGrid)
        OwnerCleanup.tryOnCleanup (fun () -> surfaceBatch.Dispose ())
        registerStaticPickIfNeeded options initialSpec.PickId
        markRevisionDirty options.Revision

        let mutable initialized = false

        Api.createEffect (fun () ->
            let rawSpec = spec ()
            let s =
                if options.PickId <> 0u && rawSpec.PickId = 0u then
                    { rawSpec with PickId = options.PickId }
                else
                    rawSpec

            let g = grid ()

            if not initialized && obj.ReferenceEquals (rawSpec, initialSpecRaw) && obj.ReferenceEquals (g, initialGrid) then
                initialized <- true
            else
                initialized <- true
                surfaceBatch.UpdateGrid (s, g)
                registerStaticPickIfNeeded options s.PickId
                markRevisionDirty options.Revision)

        surfaceBatch

    /// <summary>
    /// Creates a reactive <c>SurfaceBatch</c> in the current owning scope and updates its GPU mesh in place via <c>UpdateGrid</c>
    /// whenever <c>spec</c> or <c>grid</c> signals change.
    /// </summary>
    let bindSurface
        (context: WgpuContext)
        (spec: unit -> SurfaceSpec)
        (grid: unit -> SurfaceGrid)
        : SurfaceBatch =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box spec) then nullArg (nameof spec)
        if isNull (box grid) then nullArg (nameof grid)

        bindSurfaceCore context SinkOptions3D.defaultValue spec grid

    /// <summary>Creates a reactive <c>SurfaceBatch</c> honoring <c>SinkOptions3D.Revision</c>, <c>Registry</c>, and fallback <c>PickId</c>.</summary>
    let bindSurfaceWith
        (context: WgpuContext)
        (options: SinkOptions3D)
        (spec: unit -> SurfaceSpec)
        (grid: unit -> SurfaceGrid)
        : SurfaceBatch =
        if isNull (box context) then nullArg (nameof context)
        if isNull (box options) then nullArg (nameof options)
        if isNull (box spec) then nullArg (nameof spec)
        if isNull (box grid) then nullArg (nameof grid)

        bindSurfaceCore context options spec grid

    let private bindSingleMeshSeries
        (context: WgpuContext)
        (options: SinkOptions3D)
        (shading: unit -> ShadingMode)
        (scale: unit -> Scale3D)
        (data: unit -> 'Data)
        (buildMesh: Scale3D -> 'Data -> Result<MeshData, LayoutError list>)
        : InstanceBatch =
        let meshHandle = context.UploadMesh MeshData.empty
        OwnerCleanup.tryOnCleanup (fun () -> meshHandle.Dispose ())

        let batch = context.CreateBatch (meshHandle, options.BlendMode, options.DoubleSided, drawBeforeLines = options.DrawBeforeLines)
        OwnerCleanup.tryOnCleanup (fun () -> batch.Dispose ())
        registerStaticPickIfNeeded options options.PickId

        let mutable lastScale = Unchecked.defaultof<Scale3D>
        let mutable lastData = Unchecked.defaultof<'Data>
        let mutable hasGeomCache = false
        let mutable hasGeometry = false
        let mutable lastShading: ShadingMode voption = ValueNone
        let mutable lastPickId = 0u
        let mutable hasBatchInit = false

        Api.createEffect (fun () ->
            let s = scale ()
            let d = data ()
            let sh = shading ()

            if isNull (box s) then nullArg "scale"

            let struct (shadeOk, roughness, emissive, flags) = MarksKernel.shadingParams sh false

            if not shadeOk then
                invalidArg "shading" "ShadingMode roughness and emissive values must be finite and nonnegative."

            let geomChanged =
                not hasGeomCache
                || not (obj.ReferenceEquals (s, lastScale))
                || not (obj.ReferenceEquals (box d, box lastData))

            if geomChanged then
                hasGeomCache <- true
                lastScale <- s
                lastData <- d

                let nextHasGeometry =
                    if isNull (box d) then
                        meshHandle.UpdateMesh MeshData.empty
                        false
                    else
                        match buildMesh s d with
                        | Ok mesh when mesh.Vertices.Length > 0 && mesh.Indices.Length > 0 ->
                            meshHandle.UpdateMesh mesh
                            true
                        | _ ->
                            meshHandle.UpdateMesh MeshData.empty
                            false

                hasGeometry <- nextHasGeometry

            let shadeOrPickChanged =
                not hasBatchInit
                || (match lastShading with ValueSome prev -> prev <> sh | ValueNone -> true)
                || lastPickId <> options.PickId

            if geomChanged || shadeOrPickChanged then
                hasBatchInit <- true
                lastShading <- ValueSome sh
                lastPickId <- options.PickId

                if hasGeometry then
                    batch.SetSingle (Instance3D.fromMatrix Matrix4x4.Identity RgbaColor.white roughness options.PickId flags emissive)
                else
                    batch.Resize 0

                markRevisionDirty options.Revision)

        batch

    /// <summary>
    /// Binds variable-radius colored 3D polyline tubes to an owned <c>MeshHandle</c> and single-instance <c>InstanceBatch</c>,
    /// updating in place without re-tessellating when only <c>ShadingMode</c> changes; requires <c>sides &gt;= 3</c>.
    /// </summary>
    let bindTubes
        (context: WgpuContext)
        (options: SinkOptions3D)
        (sides: int)
        (shading: unit -> ShadingMode)
        (scale: unit -> Scale3D)
        (polylines: unit -> #seq<#IReadOnlyList<struct (Point3 * float32 * RgbaColor)>>)
        : InstanceBatch =
        requireCommon context scale
        if isNull (box options) then nullArg (nameof options)
        if isNull (box shading) then nullArg (nameof shading)
        if isNull (box polylines) then nullArg (nameof polylines)

        if sides < 3 then
            raise (ArgumentOutOfRangeException (nameof sides, "Tube cross-section requires at least 3 sides."))

        bindSingleMeshSeries context options shading scale polylines (fun s items -> Line3D.tubes s sides items)

    /// <summary>
    /// Binds double-sided ruled 3D ribbon strips to an owned <c>MeshHandle</c> and single-instance <c>InstanceBatch</c>,
    /// updating in place cleanly on geometry or shading changes.
    /// </summary>
    let bindRibbons
        (context: WgpuContext)
        (options: SinkOptions3D)
        (shading: unit -> ShadingMode)
        (scale: unit -> Scale3D)
        (strips: unit -> #seq<#IReadOnlyList<struct (Point3 * Point3 * RgbaColor)>>)
        : InstanceBatch =
        requireCommon context scale
        if isNull (box options) then nullArg (nameof options)
        if isNull (box shading) then nullArg (nameof shading)
        if isNull (box strips) then nullArg (nameof strips)

        bindSingleMeshSeries context options shading scale strips Line3D.ribbons

    /// <summary>
    /// Computes <c>IsoSurface3D.computeWithCancellation</c> under <c>FlightPolicy.CancelPrevious</c>, cancelling superseded
    /// flights cleanly when <c>scale</c>, <c>color</c>, <c>config</c>, or <c>volume</c> changes.
    /// </summary>
    let asyncIsoSurface
        (scale: unit -> Scale3D)
        (color: unit -> ColorRamp)
        (config: unit -> IsoSurfaceConfig)
        (volume: unit -> VolumeGrid)
        : AsyncMemo<Result<MeshData, LayoutError list>> =
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box color) then nullArg (nameof color)
        if isNull (box config) then nullArg (nameof config)
        if isNull (box volume) then nullArg (nameof volume)

        Api.createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ token ->
            let s = scale ()
            let c = color ()
            let cfg = config ()
            let vol = volume ()
#if FABLE_COMPILER
            Task.FromResult (IsoSurface3D.computeWithCancellation token s c cfg vol)
#else
            Task.Run ((fun () -> IsoSurface3D.computeWithCancellation token s c cfg vol), token)
#endif
        )

    /// <summary>
    /// Binds a volumetric isosurface pipeline to an owned <c>IsoSurfaceSink</c>, executing marching cubes off the UI thread
    /// via <c>asyncIsoSurface</c>, updating GPU buffers when settled, and dynamically switching between <c>Opaque</c> and <c>AlphaBlend</c> batches.
    /// </summary>
    let bindIsoSurface
        (context: WgpuContext)
        (options: SinkOptions3D)
        (shading: unit -> ShadingMode)
        (opacity: unit -> float32)
        (scale: unit -> Scale3D)
        (color: unit -> ColorRamp)
        (config: unit -> IsoSurfaceConfig)
        (volume: unit -> VolumeGrid)
        : IsoSurfaceSink =
        requireCommon context scale
        if isNull (box options) then nullArg (nameof options)
        if isNull (box shading) then nullArg (nameof shading)
        if isNull (box opacity) then nullArg (nameof opacity)
        if isNull (box color) then nullArg (nameof color)
        if isNull (box config) then nullArg (nameof config)
        if isNull (box volume) then nullArg (nameof volume)

        let initOpacity = Api.untrack opacity
        Check3D.finite32 "opacity" initOpacity

        if initOpacity < 0.0f || initOpacity > 1.0f then
            raise (ArgumentOutOfRangeException (nameof opacity, "IsoSurface opacity must lie within [0, 1]."))

        let host = new DynamicMeshBatchHost (context, options, options.DoubleSided, initOpacity)
        let memo = asyncIsoSurface scale color config volume
        let mutable latestError: LayoutError list voption = ValueNone
        let sink = new IsoSurfaceSink (memo, host, fun () -> latestError)
        OwnerCleanup.tryOnCleanup sink.Dispose
        registerStaticPickIfNeeded options options.PickId

        Api.createEffect (fun () ->
            if not host.IsDisposed && not context.IsDisposed then
                let sh = shading ()
                let op = opacity ()
                let col = Api.untrack color
                let useColorLut = not (isNull (box col)) && col.IsScalarPassthrough
                let reading = memo.TryValue

                let meshChanged =
                    match reading with
                    | Reading.Ready (Ok mesh) ->
                        latestError <- ValueNone
                        host.SetMesh mesh
                    | Reading.Ready (Error errs) ->
                        latestError <- ValueSome errs
                        host.ClearGeometry ()
                    | Reading.Failed _ ->
                        host.ClearGeometry ()
                    | Reading.Pending -> false

                let instChanged = host.SyncInstance (sh, op, options.PickId, useColorLut, meshChanged)

                if meshChanged || instChanged then
                    markRevisionDirty options.Revision)

        sink

    /// <summary>
    /// Binds orthogonal X/Y/Z volumetric cross-section planes to a double-sided <c>VolumeSliceSink</c>
    /// with dynamic <c>Opaque</c>/<c>AlphaBlend</c> switching and automatic revision dirtying.
    /// </summary>
    let bindVolumeSlices
        (context: WgpuContext)
        (options: SinkOptions3D)
        (shading: unit -> ShadingMode)
        (opacity: unit -> float32)
        (scale: unit -> Scale3D)
        (color: unit -> ColorRamp)
        (indices: unit -> SliceIndices3D)
        (volume: unit -> VolumeGrid)
        : VolumeSliceSink =
        requireCommon context scale
        if isNull (box options) then nullArg (nameof options)
        if isNull (box shading) then nullArg (nameof shading)
        if isNull (box opacity) then nullArg (nameof opacity)
        if isNull (box color) then nullArg (nameof color)
        if isNull (box indices) then nullArg (nameof indices)
        if isNull (box volume) then nullArg (nameof volume)

        let initOpacity = Api.untrack opacity
        Check3D.finite32 "opacity" initOpacity

        if initOpacity < 0.0f || initOpacity > 1.0f then
            raise (ArgumentOutOfRangeException (nameof opacity, "Volume slice opacity must lie within [0, 1]."))

        // Cross-section slice quads carry single-sided normals and require double-sided rasterization.
        let host = new DynamicMeshBatchHost (context, options, true, initOpacity)
        let mutable latestError: LayoutError list voption = ValueNone
        let sink = new VolumeSliceSink (host, fun () -> latestError)
        OwnerCleanup.tryOnCleanup sink.Dispose
        registerStaticPickIfNeeded options options.PickId

        let mutable hasGeomCache = false
        let mutable lastScale = Unchecked.defaultof<Scale3D>
        let mutable lastColor = Unchecked.defaultof<ColorRamp>
        let mutable lastIndices = Unchecked.defaultof<SliceIndices3D>
        let mutable lastVolume = Unchecked.defaultof<VolumeGrid>

        Api.createEffect (fun () ->
            if not host.IsDisposed && not context.IsDisposed then
                let s = scale ()
                let c = color ()
                let idx = indices ()
                let vol = volume ()
                let sh = shading ()
                let op = opacity ()

                let geomInputsChanged =
                    not hasGeomCache
                    || not (obj.ReferenceEquals (s, lastScale))
                    || not (obj.ReferenceEquals (c, lastColor))
                    || idx <> lastIndices
                    || not (obj.ReferenceEquals (vol, lastVolume))

                let meshChanged =
                    if geomInputsChanged then
                        hasGeomCache <- true
                        lastScale <- s
                        lastColor <- c
                        lastIndices <- idx
                        lastVolume <- vol

                        match VolumeSlices3D.extract s c idx vol with
                        | Ok mesh ->
                            latestError <- ValueNone
                            host.SetMesh mesh
                        | Error errs ->
                            latestError <- ValueSome errs
                            host.ClearGeometry ()
                    else
                        false

                let useColorLut = not (isNull (box c)) && c.IsScalarPassthrough
                let instChanged = host.SyncInstance (sh, op, options.PickId, useColorLut, meshChanged)

                if meshChanged || instChanged then
                    markRevisionDirty options.Revision)

        sink

    /// <summary>
    /// Binds 2D <c>ContourPolygon</c> isolines projected onto the active camera back wall into a section-plane-exempt <c>LineBatch</c>.
    /// </summary>
    let bindWallContours
        (context: WgpuContext)
        (options: SinkOptions3D)
        (scale: unit -> Scale3D)
        (backWalls: unit -> struct (float32 * float32 * float32))
        (plane: unit -> WallPlane)
        (color: unit -> RgbaColor)
        (contours: unit -> ContourPolygon list)
        : LineBatch =
        requireCommon context scale
        if isNull (box options) then nullArg (nameof options)
        if isNull (box backWalls) then nullArg (nameof backWalls)
        if isNull (box plane) then nullArg (nameof plane)
        if isNull (box color) then nullArg (nameof color)
        if isNull (box contours) then nullArg (nameof contours)

        let batch = context.CreateLineBatch ()
        OwnerCleanup.tryOnCleanup (fun () -> batch.Dispose ())
        let mutable hadNonEmpty = false

        Api.createEffect (fun () ->
            let s = scale ()
            let bw = backWalls ()
            let pl = plane ()
            let col = color ()
            let ctr = contours ()
            let verts = WallProjection3D.projectContours s bw pl col ctr

            if verts.Length > 0 then
                batch.UpdateVertices verts.Span
                hadNonEmpty <- true
                markRevisionDirty options.Revision
            elif hadNonEmpty then
                batch.Resize 0
                hadNonEmpty <- false
                markRevisionDirty options.Revision)

        batch

namespace Ranvier.D5.Wgpu

open System
open System.Threading.Tasks
open Ranvier
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Ranvier reactive scales, editables, incremental keyed GPU batch sinks, and async volumetric computations.</summary>
[<RequireQualifiedAccess>]
module Reactive3D =
    let private attachSyncedEffect
        (rows: Projection<'K, 'Datum>)
        (sync: KeyedSlotSync<'K, 'Datum, 'Gpu>)
        =
        let reader = rows.NewValueReader ()

        Api.createEffect (fun () ->
            let delta = reader.Read ()
            sync.Sync delta)

    let private requireInstanceInputs
        (context: WgpuContext)
        (layerId: byte)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, 'Datum>)
        =
        if isNull (box context) then
            nullArg (nameof context)

        PickEncoding.validateLayerId layerId

        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box rows) then
            nullArg (nameof rows)

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

        let batch = context.CreateBatch (mesh, blendMode, doubleSided)
        Api.onCleanup (fun () -> batch.Dispose ())

        let sync =
            KeyedSlotSync<'K, GlyphDatum3D, Instance3D>(
                layerId,
                1,
                preserveOrder,
                scale,
                rows,
                MarksKernel.tryPackGlyph,
                (fun arr slot pickId -> arr[slot].PickId <- pickId),
                (fun arr count -> batch.UpdateInstances (MarksKernel.spanSlice arr 0 count)),
                (fun startIdx arr srcStart count -> batch.UpdateRange (startIdx, MarksKernel.spanSlice arr srcStart count)),
                (fun count -> batch.Resize count)
            )

        attachSyncedEffect rows sync
        KeyedInstanceSink<'K>(layerId, batch, sync :> IKeyedSinkSync<'K>)

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
        Api.onCleanup (fun () -> unitBox.Dispose ())
        let batch = context.CreateBatch (unitBox, BlendMode.Opaque, false)
        Api.onCleanup (fun () -> batch.Dispose ())

        let mutable cachedScale = Unchecked.defaultof<Scale3D>
        let mutable dx = 0.0
        let mutable dy = 0.0
        let mutable upAxis = UpAxis.ZUp
        let mutable xScale = Unchecked.defaultof<AxisScale3D>
        let mutable yScale = Unchecked.defaultof<AxisScale3D>

        let packBar (s: Scale3D) (pickId: uint32) (datum: BarDatum3D) (out: Instance3D array) (slot: int) : bool =
            if not (obj.ReferenceEquals (s, cachedScale)) then
                cachedScale <- s
                let struct (extX, extY, _) = s.DataExtents
                dx <- extX
                dy <- extY
                upAxis <- s.UpAxis
                xScale <- s.XScale
                yScale <- s.YScale

            MarksKernel.tryPackBar s dx dy upAxis xScale yScale pickId datum out slot

        let sync =
            KeyedSlotSync<'K, BarDatum3D, Instance3D>(
                layerId,
                1,
                false,
                scale,
                rows,
                packBar,
                (fun arr slot pickId -> arr[slot].PickId <- pickId),
                (fun arr count -> batch.UpdateInstances (MarksKernel.spanSlice arr 0 count)),
                (fun startIdx arr srcStart count -> batch.UpdateRange (startIdx, MarksKernel.spanSlice arr srcStart count)),
                (fun count -> batch.Resize count)
            )

        attachSyncedEffect rows sync
        KeyedInstanceSink<'K>(layerId, batch, sync :> IKeyedSinkSync<'K>)

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

        let batch = context.CreateDiscBatch ()
        Api.onCleanup (fun () -> batch.Dispose ())

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

        attachSyncedEffect rows sync
        KeyedDiscSink<'K>(layerId, batch, sync :> IKeyedSinkSync<'K>)

    /// <summary>
    /// Binds a keyed <c>Projection&lt;'K, struct (Point3 * Point3 * RgbaColor)&gt;</c> incrementally to a 4x-MSAA hairline <c>LineBatch</c>
    /// with <c>O(changed)</c> <c>UpdateRange</c> uploads and owning-scope disposal.
    /// </summary>
    let bindSegments
        (context: WgpuContext)
        (scale: unit -> Scale3D)
        (rows: Projection<'K, struct (Point3 * Point3 * RgbaColor)>)
        : KeyedSegmentSink<'K> =
        if isNull (box context) then
            nullArg (nameof context)

        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box rows) then
            nullArg (nameof rows)

        let batch = context.CreateLineBatch ()
        Api.onCleanup (fun () -> batch.Dispose ())

        let sync =
            KeyedSlotSync<'K, struct (Point3 * Point3 * RgbaColor), LineVertex3D>(
                0uy,
                2,
                false,
                scale,
                rows,
                (fun s _ pickDatum out vertexSlot -> MarksKernel.tryPackSegment s pickDatum out vertexSlot),
                (fun _ _ _ -> ()),
                (fun arr vertexCount -> batch.UpdateVertices (MarksKernel.spanSlice arr 0 vertexCount)),
                (fun startVertex arr srcStart vertexCount -> batch.UpdateRange (startVertex, MarksKernel.spanSlice arr srcStart vertexCount)),
                (fun vertexCount -> batch.Resize vertexCount)
            )

        attachSyncedEffect rows sync
        KeyedSegmentSink<'K>(batch, sync :> IKeyedSinkSync<'K>)

    /// <summary>
    /// Creates a reactive <c>SurfaceBatch</c> in the current owning scope and updates its GPU mesh in place via <c>UpdateGrid</c>
    /// whenever <c>spec</c> or <c>grid</c> signals change.
    /// </summary>
    let bindSurface
        (context: WgpuContext)
        (spec: unit -> SurfaceSpec)
        (grid: unit -> SurfaceGrid)
        : SurfaceBatch =
        if isNull (box context) then
            nullArg (nameof context)

        if isNull (box spec) then
            nullArg (nameof spec)

        if isNull (box grid) then
            nullArg (nameof grid)

        let initialSpec = Api.untrack spec
        let initialGrid = Api.untrack grid
        let surfaceBatch = SurfaceBatch.Create (context, initialSpec, initialGrid)
        Api.onCleanup (fun () -> surfaceBatch.Dispose ())
        let mutable initialized = false

        Api.createEffect (fun () ->
            let s = spec ()
            let g = grid ()

            if not initialized && obj.ReferenceEquals (s, initialSpec) && obj.ReferenceEquals (g, initialGrid) then
                initialized <- true
            else
                initialized <- true
                surfaceBatch.UpdateGrid (s, g))

        surfaceBatch

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
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box color) then
            nullArg (nameof color)

        if isNull (box config) then
            nullArg (nameof config)

        if isNull (box volume) then
            nullArg (nameof volume)

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

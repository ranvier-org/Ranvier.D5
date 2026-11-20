namespace Ranvier.D5.Wgpu

open System
open System.Numerics
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Result returned by <c>Axis3DMemo.Update</c> describing the current axis frame and whether inputs changed.</summary>
[<Struct>]
type AxisMemoUpdate3D =
    {
        /// <summary>True when <c>Axis3D.build</c> re-executed and uploaded new panel/line buffers.</summary>
        Rebuilt: bool
        /// <summary>True when the <c>Axis3DSpec</c> object reference changed across this update (initial or scene switch).</summary>
        AxisSpecChanged: bool
        /// <summary>Current camera-classified 3D axis frame.</summary>
        Frame: AxisFrame3D
        /// <summary>Base 2D SVG scene holding projected axis ticks, titles, and optional plot title.</summary>
        BaseOverlay: Scene
    }

/// <summary>
/// Stateful memoizer caching <c>(Axis3DSpec, Camera3D, width, height)</c> and uploading to GPU <c>panelBatch</c> and
/// <c>axisLineBatch</c> only when framing inputs change.
/// </summary>
[<Sealed>]
type Axis3DMemo(panelBatch: InstanceBatch, axisLineBatch: LineBatch) =
    do
        if isNull (box panelBatch) then
            nullArg (nameof panelBatch)

        if isNull (box axisLineBatch) then
            nullArg (nameof axisLineBatch)

    let mutable hasCached = false
    let mutable cachedSpec = Unchecked.defaultof<Axis3DSpec>
    let mutable cachedCam = Unchecked.defaultof<Camera3D>
    let mutable cachedW = -1
    let mutable cachedH = -1
    let mutable cachedFrame = Unchecked.defaultof<AxisFrame3D>
    let mutable cachedBaseOverlay = Unchecked.defaultof<Scene>

    /// <summary>GPU <c>InstanceBatch</c> receiving camera-backfacing cuboid wall panels.</summary>
    member _.PanelBatch: InstanceBatch = panelBatch

    /// <summary>GPU <c>LineBatch</c> receiving cuboid spines, gridlines, and outward ticks.</summary>
    member _.LineBatch: LineBatch = axisLineBatch

    /// <summary>True after at least one <c>Axis3D.build</c> frame has been cached.</summary>
    member _.HasCached: bool = hasCached

    /// <summary>Most recently cached <c>Axis3DSpec</c>, if initialized.</summary>
    member _.CurrentSpec: Axis3DSpec voption =
        if hasCached then ValueSome cachedSpec else ValueNone

    /// <summary>Most recently cached <c>AxisFrame3D</c>, if initialized.</summary>
    member _.CurrentFrame: AxisFrame3D voption =
        if hasCached then ValueSome cachedFrame else ValueNone

    /// <summary>Most recently built base 2D SVG overlay scene, if initialized.</summary>
    member _.CurrentBaseOverlay: Scene voption =
        if hasCached then ValueSome cachedBaseOverlay else ValueNone

    /// <summary>Invalidates cached inputs so the next <c>Update</c> rebuilds unconditionally.</summary>
    member _.Invalidate() : unit =
        hasCached <- false
        cachedSpec <- Unchecked.defaultof<Axis3DSpec>
        cachedBaseOverlay <- Unchecked.defaultof<Scene>
        cachedFrame <- Unchecked.defaultof<AxisFrame3D>

    /// <summary>
    /// Updates GPU axis panels and lines when <c>axisSpec</c>, <c>logicalWidth</c>, <c>logicalHeight</c>, or <c>camera</c>
    /// changed since the previous call, returning cached frame and overlay state otherwise.
    /// </summary>
    member _.Update(axisSpec: Axis3DSpec, logicalWidth: int, logicalHeight: int, camera: Camera3D) : AxisMemoUpdate3D =
        if isNull (box axisSpec) then
            nullArg (nameof axisSpec)

        let axisSpecChanged =
            not hasCached
            || not (Object.ReferenceEquals (axisSpec, cachedSpec))

        let needsRebuild =
            axisSpecChanged
            || camera <> cachedCam
            || logicalWidth <> cachedW
            || logicalHeight <> cachedH

        if needsRebuild then
            let nextFrame = Axis3D.build axisSpec logicalWidth logicalHeight camera
            panelBatch.UpdateInstances ((Axis3D.panelInstances nextFrame).Span)
            axisLineBatch.UpdateVertices ((Axis3D.lineVertices nextFrame).Span)
            let nextOverlay = Axis3D.overlayScene (float logicalWidth) (float logicalHeight) nextFrame

            hasCached <- true
            cachedSpec <- axisSpec
            cachedCam <- camera
            cachedW <- logicalWidth
            cachedH <- logicalHeight
            cachedFrame <- nextFrame
            cachedBaseOverlay <- nextOverlay

            {
                Rebuilt = true
                AxisSpecChanged = axisSpecChanged
                Frame = nextFrame
                BaseOverlay = nextOverlay
            }
        else
            {
                Rebuilt = false
                AxisSpecChanged = false
                Frame = cachedFrame
                BaseOverlay = cachedBaseOverlay
            }

/// <summary>Result produced by <c>PlotInputKernel3D.OnPointerMove</c>.</summary>
[<RequireQualifiedAccess; Struct>]
type PointerMoveResult3D =
    /// <summary>No camera change and hover pick suppressed (for example foreign chorded pointer or hover disabled).</summary>
    | NoOp
    /// <summary>Active drag updated <c>PlotCamera3D</c>.</summary>
    | CameraUpdated of camera: PlotCamera3D
    /// <summary>Pointer hovered outside drag with hover picking enabled.</summary>
    | HoverMoved

/// <summary>Result produced by <c>PlotInputKernel3D.OnPointerUp</c>.</summary>
[<Struct>]
type PointerUpResult3D =
    {
        /// <summary>True when the ending pointer matched the active drag/capture pointer.</summary>
        Handled: bool
        /// <summary>True when the interaction remained within click jitter threshold on primary button.</summary>
        WasClick: bool
        /// <summary>Pointer client X coordinate at release.</summary>
        ClientX: float
        /// <summary>Pointer client Y coordinate at release.</summary>
        ClientY: float
    }

/// <summary>Camera action emitted by <c>PlotInputKernel3D.ApplyDefaultKey</c>.</summary>
[<RequireQualifiedAccess; Struct>]
type KeyCameraAction3D =
    /// <summary>Unrecognized key; caller should leave event unhandled.</summary>
    | Ignore
    /// <summary>Reset key (<c>R</c> or <c>Home</c>); caller should reset editable camera and prevent default.</summary>
    | Reset
    /// <summary>Updated <c>PlotCamera3D</c> pose; caller should assign camera and prevent default.</summary>
    | SetCamera of camera: PlotCamera3D

/// <summary>
/// Portable pointer-drag, orbit/pan, click detection, wheel zoom, and default keyboard shortcut state machine
/// shared across native SDL and browser WebGPU plot hosts.
/// </summary>
[<Sealed>]
type PlotInputKernel3D() =
    let mutable dragging = false
    let mutable dragMoved = false
    let mutable dragPan = false
    let mutable activePointerId = -1
    let mutable lastClientX = 0.0
    let mutable lastClientY = 0.0
    let mutable downClientX = 0.0
    let mutable downClientY = 0.0

    /// <summary>True while a primary/pan drag sequence owns the active pointer.</summary>
    member _.IsDragging: bool = dragging

    /// <summary>Identifier of the active captured pointer, or <c>-1</c> when idle.</summary>
    member _.ActivePointerId: int = activePointerId

    /// <summary>
    /// Begins a pointer interaction if idle and <c>isInteractiveTarget</c> is false, returning true when the host
    /// should acquire pointer capture and reset hover picking.
    /// </summary>
    member _.OnPointerDown
        (
            pointerId: int,
            button: int16,
            clientX: float,
            clientY: float,
            shiftKey: bool,
            isInteractiveTarget: bool
        ) : bool =
        if dragging || isInteractiveTarget then
            false
        else
            dragging <- true
            dragMoved <- false
            dragPan <- (button = 1s || button = 2s || shiftKey)
            activePointerId <- pointerId
            lastClientX <- clientX
            lastClientY <- clientY
            downClientX <- clientX
            downClientY <- clientY
            true

    /// <summary>Processes a pointer move for orbit/pan updates or gated hover scheduling.</summary>
    member _.OnPointerMove
        (
            pointerId: int,
            clientX: float,
            clientY: float,
            viewportWidth: float,
            viewportHeight: float,
            fitRadius: float32,
            camera: PlotCamera3D,
            allowHover: bool
        ) : PointerMoveResult3D =
        if dragging && pointerId = activePointerId then
            let dx = float32 (clientX - lastClientX)
            let dy = float32 (clientY - lastClientY)
            lastClientX <- clientX
            lastClientY <- clientY

            if abs (clientX - downClientX) > 2.0 || abs (clientY - downClientY) > 2.0 then
                dragMoved <- true

            if dx <> 0.0f || dy <> 0.0f then
                let nextCam =
                    if dragPan then
                        let safeFitR = if Check3D.isFinite32 fitRadius && fitRadius > 0.0f then fitRadius else 1.0f
                        let ndx = dx / float32 (max 1.0 viewportWidth)
                        let ndy = dy / float32 (max 1.0 viewportHeight)
                        PlotCamera3D.panBy safeFitR AxisLock3D.Free ndx ndy camera
                    else
                        PlotCamera3D.orbitBy dx dy camera

                PointerMoveResult3D.CameraUpdated nextCam
            else
                PointerMoveResult3D.NoOp
        elif not dragging && allowHover then
            PointerMoveResult3D.HoverMoved
        else
            PointerMoveResult3D.NoOp

    /// <summary>Completes an active pointer interaction and classifies primary unmoved clicks.</summary>
    member _.OnPointerUp(pointerId: int, button: int16, clientX: float, clientY: float) : PointerUpResult3D =
        if dragging && pointerId = activePointerId then
            let wasClick = not dragMoved && button = 0s
            dragging <- false
            dragMoved <- false
            dragPan <- false
            activePointerId <- -1

            {
                Handled = true
                WasClick = wasClick
                ClientX = clientX
                ClientY = clientY
            }
        else
            {
                Handled = false
                WasClick = false
                ClientX = clientX
                ClientY = clientY
            }

    /// <summary>Cancels an active pointer interaction matching <c>pointerId</c>, returning true if state changed.</summary>
    member _.OnPointerCancel(pointerId: int) : bool =
        if dragging && pointerId = activePointerId then
            dragging <- false
            dragMoved <- false
            dragPan <- false
            activePointerId <- -1
            true
        else
            false

    /// <summary>Computes exponential wheel zoom update when <c>abs deltaY &gt; 1e-6</c>.</summary>
    static member ApplyWheel(deltaY: float, camera: PlotCamera3D) : PlotCamera3D voption =
        if abs deltaY > 1e-6 then
            let rawFactor = MathF.Exp (-float32 deltaY * 0.0015f)
            let factor = Math.Clamp (rawFactor, 0.25f, 4.0f)
            ValueSome (PlotCamera3D.zoomBy factor camera)
        else
            ValueNone

    /// <summary>Applies standard plot camera keyboard shortcuts (<c>R</c>/<c>Home</c>, <c>P</c>, <c>M</c>, arrows, <c>+</c>/<c>-</c>).</summary>
    static member ApplyDefaultKey(key: string, camera: PlotCamera3D) : KeyCameraAction3D =
        match key with
        | "r" | "R" | "Home" -> KeyCameraAction3D.Reset
        | "p" | "P" ->
            let nextP = if camera.Perspectiveness > 0.5f then 0.0f else 1.0f
            KeyCameraAction3D.SetCamera { camera with Perspectiveness = nextP }
        | "m" | "M" ->
            let nextMode =
                match camera.Mode with
                | OrbitMode3D.Turntable -> OrbitMode3D.Arcball
                | OrbitMode3D.Arcball -> OrbitMode3D.Turntable

            KeyCameraAction3D.SetCamera (PlotCamera3D.withMode nextMode camera)
        | "ArrowLeft" -> KeyCameraAction3D.SetCamera (PlotCamera3D.orbitBy -12.0f 0.0f camera)
        | "ArrowRight" -> KeyCameraAction3D.SetCamera (PlotCamera3D.orbitBy 12.0f 0.0f camera)
        | "ArrowUp" -> KeyCameraAction3D.SetCamera (PlotCamera3D.orbitBy 0.0f -12.0f camera)
        | "ArrowDown" -> KeyCameraAction3D.SetCamera (PlotCamera3D.orbitBy 0.0f 12.0f camera)
        | "+" | "=" -> KeyCameraAction3D.SetCamera (PlotCamera3D.zoomBy 1.15f camera)
        | "-" | "_" -> KeyCameraAction3D.SetCamera (PlotCamera3D.zoomBy (1.0f / 1.15f) camera)
        | _ -> KeyCameraAction3D.Ignore

/// <summary>Zero-allocation steady-state cache merging one or two fixed prefix GPU batches ahead of user <c>Extra*</c> slices.</summary>
[<Sealed>]
type ScenePrefixCache<'T when 'T: not struct>(prefix1: 'T, prefix2Opt: 'T voption) =
    let prefixCount = if prefix2Opt.IsSome then 2 else 1
    let mutable cachedExtra = Array.empty<'T>
    let mutable merged =
        let arr = Array.zeroCreate<'T> prefixCount
        arr[0] <- prefix1

        match prefix2Opt with
        | ValueSome p2 -> arr[1] <- p2
        | ValueNone -> ()

        arr

    let mutable mergedMem = ReadOnlyMemory<'T>(merged)

    /// <summary>
    /// Synchronizes the prefix-merged slice with <c>extra</c>, allocating zero heap memory when elements match by reference identity.
    /// </summary>
    member _.Sync(extra: ReadOnlyMemory<'T>) : ReadOnlyMemory<'T> =
        let span = extra.Span
        let mutable same = span.Length = cachedExtra.Length

        if same then
            let mutable i = 0

            while same && i < span.Length do
                if not (Object.ReferenceEquals (span[i], cachedExtra[i])) then
                    same <- false

                i <- i + 1

        if not same then
            let nextExtra = Array.zeroCreate<'T> span.Length
            let nextMerged = Array.zeroCreate<'T> (prefixCount + span.Length)
            nextMerged[0] <- prefix1

            match prefix2Opt with
            | ValueSome p2 -> nextMerged[1] <- p2
            | ValueNone -> ()

            for i = 0 to span.Length - 1 do
                let item = span[i]
                nextExtra[i] <- item
                nextMerged[prefixCount + i] <- item

            cachedExtra <- nextExtra
            merged <- nextMerged
            mergedMem <- ReadOnlyMemory<'T>(nextMerged)

        mergedMem

/// <summary>Composable declarative 3D scene layer pairing GPU batches and 2D SVG legend/HUD overlays.</summary>
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Layer3D =
    /// <summary>Single hardware-instanced mesh batch.</summary>
    | Batch of batch: InstanceBatch
    /// <summary>Dynamic slice of hardware-instanced mesh batches evaluated at compile time.</summary>
    | Batches of readBatches: (unit -> ReadOnlyMemory<InstanceBatch>)
    /// <summary>Deforming surface batch contributing its active mesh batch and optional wireframe line batch.</summary>
    | Surface of surface: SurfaceBatch
    /// <summary>Async volumetric isosurface sink contributing its active <c>Opaque</c> or <c>AlphaBlend</c> batch.</summary>
    | IsoSurface of isoSurface: IsoSurfaceSink
    /// <summary>Volumetric slice sink contributing its active double-sided <c>Opaque</c> or <c>AlphaBlend</c> batch.</summary>
    | VolumeSlices of volumeSlices: VolumeSliceSink
    /// <summary>3D hairline batch.</summary>
    | Lines of lineBatch: LineBatch
    /// <summary>Camera-facing SDF scatter disc batch.</summary>
    | Discs of discBatch: DiscBatch
    /// <summary>Composed 2D SVG overlay scene (for example custom legends or annotations).</summary>
    | Overlay of readOverlay: (unit -> Scene)
    /// <summary>Scalar colorbar legend specification compiled cleanly to an SVG overlay.</summary>
    | Colorbar of spec: ColorbarSpec3D
    /// <summary>Categorical swatch legend specification compiled cleanly to an SVG overlay.</summary>
    | CategoricalLegend of origin: Point * title: string option * items: LegendSwatch3D list
    /// <summary>Additional standalone 2D SVG overlay marks rendered on the axis overlay.</summary>
    | OverlayMarks of marks: Mark list

/// <summary>Ergonomic constructors lifting GPU batches, reactive sinks, and 2D legend overlays into <c>Layer3D</c> items.</summary>
[<RequireQualifiedAccess>]
module Layer3D =
    /// <summary>Wraps a single GPU <c>InstanceBatch</c>.</summary>
    let batch (batch: InstanceBatch) : Layer3D =
        if isNull (box batch) then nullArg (nameof batch)
        Layer3D.Batch batch

    /// <summary>Wraps a dynamic <c>ReadOnlyMemory&lt;InstanceBatch&gt;</c> producer.</summary>
    let batches (readBatches: unit -> ReadOnlyMemory<InstanceBatch>) : Layer3D =
        if isNull (box readBatches) then nullArg (nameof readBatches)
        Layer3D.Batches readBatches

    /// <summary>Lifts a <c>KeyedInstanceSink</c>'s synchronized <c>InstanceBatch</c> into a layer.</summary>
    let keyedInstances (sink: KeyedInstanceSink<'K>) : Layer3D =
        if isNull (box sink) then nullArg (nameof sink)
        Layer3D.Batch sink.Batch

    /// <summary>Lifts a <c>KeyedDiscSink</c>'s synchronized <c>DiscBatch</c> into a layer.</summary>
    let keyedDiscs (sink: KeyedDiscSink<'K>) : Layer3D =
        if isNull (box sink) then nullArg (nameof sink)
        Layer3D.Discs sink.Batch

    /// <summary>Lifts a <c>KeyedSegmentSink</c>'s synchronized <c>LineBatch</c> into a layer.</summary>
    let keyedSegments (sink: KeyedSegmentSink<'K>) : Layer3D =
        if isNull (box sink) then nullArg (nameof sink)
        Layer3D.Lines sink.Batch

    /// <summary>Lifts a <c>KeyedPolylineSink</c>'s synchronized <c>LineBatch</c> into a layer.</summary>
    let keyedPolylines (sink: KeyedPolylineSink<'K>) : Layer3D =
        if isNull (box sink) then nullArg (nameof sink)
        Layer3D.Lines sink.Batch

    /// <summary>Lifts a <c>SurfaceBatch</c> (including its <c>WireframeBatch</c> when enabled) into a layer.</summary>
    let surface (surfaceBatch: SurfaceBatch) : Layer3D =
        if isNull (box surfaceBatch) then nullArg (nameof surfaceBatch)
        Layer3D.Surface surfaceBatch

    /// <summary>Lifts an <c>IsoSurfaceSink</c> into a layer.</summary>
    let isoSurface (sink: IsoSurfaceSink) : Layer3D =
        if isNull (box sink) then nullArg (nameof sink)
        Layer3D.IsoSurface sink

    /// <summary>Lifts a <c>VolumeSliceSink</c> into a layer.</summary>
    let volumeSlices (sink: VolumeSliceSink) : Layer3D =
        if isNull (box sink) then nullArg (nameof sink)
        Layer3D.VolumeSlices sink

    /// <summary>Wraps a GPU <c>LineBatch</c>.</summary>
    let lines (lineBatch: LineBatch) : Layer3D =
        if isNull (box lineBatch) then nullArg (nameof lineBatch)
        Layer3D.Lines lineBatch

    /// <summary>Wraps a GPU <c>DiscBatch</c>.</summary>
    let discs (discBatch: DiscBatch) : Layer3D =
        if isNull (box discBatch) then nullArg (nameof discBatch)
        Layer3D.Discs discBatch

    /// <summary>Wraps a dynamic 2D overlay <c>Scene</c> producer.</summary>
    let overlay (readScene: unit -> Scene) : Layer3D =
        if isNull (box readScene) then nullArg (nameof readScene)
        Layer3D.Overlay readScene

    /// <summary>Wraps a static 2D overlay <c>Scene</c>.</summary>
    let overlayScene (scene: Scene) : Layer3D =
        if isNull (box scene) then nullArg (nameof scene)
        Layer3D.Overlay (fun () -> scene)

    /// <summary>Wraps a scalar colorbar legend overlay specification.</summary>
    let colorbar (spec: ColorbarSpec3D) : Layer3D =
        if isNull (box spec) then nullArg (nameof spec)
        Layer3D.Colorbar spec

    /// <summary>Wraps a categorical swatch legend overlay specification.</summary>
    let categorical (origin: Point) (title: string option) (items: LegendSwatch3D list) : Layer3D =
        if isNull (box items) then nullArg (nameof items)
        Layer3D.CategoricalLegend (origin, title, items)

    /// <summary>Wraps standalone 2D SVG overlay marks.</summary>
    let overlayMarks (marks: Mark list) : Layer3D =
        if isNull (box marks) then nullArg (nameof marks)
        Layer3D.OverlayMarks marks

/// <summary>Compiled GPU batch slices and 2D SVG overlays extracted from a list of <c>Layer3D</c> descriptors.</summary>
[<NoEquality; NoComparison>]
type CompiledPlotLayers3D =
    {
        /// <summary>Ordered GPU hardware-instanced mesh batches.</summary>
        Batches: ReadOnlyMemory<InstanceBatch>
        /// <summary>Ordered GPU 3D hairline batches (including surface wireframes).</summary>
        Lines: ReadOnlyMemory<LineBatch>
        /// <summary>Ordered GPU billboard SDF scatter disc batches.</summary>
        Discs: ReadOnlyMemory<DiscBatch>
        /// <summary>Standalone 2D SVG overlay marks rendered atop the axis overlay.</summary>
        OverlayMarks: Mark list
        /// <summary>Combined composed 2D SVG overlay scene (legend colorbars, swatches, and custom scenes).</summary>
        ExtraOverlay: Scene voption
    }

/// <summary>Portable compilers assembling <c>Layer3D</c> compositions and merging 2D axis overlays.</summary>
[<RequireQualifiedAccess>]
module Plot3D =
    /// <summary>Composes axis label overlay <c>baseOverlay</c> with optional <c>extraMarks</c>.</summary>
    let composeOverlay (logicalWidth: int) (logicalHeight: int) (baseOverlay: Scene) (extraMarks: Mark list) : Scene =
        if isNull (box baseOverlay) then
            nullArg (nameof baseOverlay)

        if List.isEmpty extraMarks then
            baseOverlay
        else
            Scene.create
                (float (max 1 logicalWidth))
                (float (max 1 logicalHeight))
                (Scene.title baseOverlay)
                (Scene.marks baseOverlay @ extraMarks)

    /// <summary>Compiles a sequence of <c>Layer3D</c> items using explicit viewport dimensions for legend overlays.</summary>
    let compile (viewportWidth: float) (viewportHeight: float) (layers: seq<Layer3D>) : CompiledPlotLayers3D =
        Check3D.positive (nameof viewportWidth) viewportWidth
        Check3D.positive (nameof viewportHeight) viewportHeight

        if isNull (box layers) then
            nullArg (nameof layers)

        let batches = ResizeArray<InstanceBatch>()
        let lines = ResizeArray<LineBatch>()
        let discs = ResizeArray<DiscBatch>()
        let marksAcc = ResizeArray<Mark>()
        let scenesAcc = ResizeArray<Scene>()

        for layer in layers do
            match layer with
            | Layer3D.Batch b ->
                if not (isNull (box b)) then batches.Add b
            | Layer3D.Batches readMem ->
                let mem = readMem ()
                let span = mem.Span

                for i = 0 to span.Length - 1 do
                    let b = span[i]
                    if not (isNull (box b)) then batches.Add b
            | Layer3D.Surface s ->
                if not (isNull (box s)) then
                    batches.Add s.Batch

                    match s.WireframeBatch with
                    | Some lb -> lines.Add lb
                    | None -> ()
            | Layer3D.IsoSurface iso ->
                if not (isNull (box iso)) then batches.Add iso.Batch
            | Layer3D.VolumeSlices vs ->
                if not (isNull (box vs)) then batches.Add vs.Batch
            | Layer3D.Lines l ->
                if not (isNull (box l)) then lines.Add l
            | Layer3D.Discs d ->
                if not (isNull (box d)) then discs.Add d
            | Layer3D.OverlayMarks ms ->
                if not (isNull (box ms)) then
                    for m in ms do
                        marksAcc.Add m
            | Layer3D.Overlay readScene ->
                let sc = readScene ()
                if not (isNull (box sc)) then scenesAcc.Add sc
            | Layer3D.Colorbar spec ->
                scenesAcc.Add (Legend3D.colorbar viewportWidth viewportHeight spec)
            | Layer3D.CategoricalLegend (origin, title, items) ->
                scenesAcc.Add (Legend3D.categorical viewportWidth viewportHeight origin title items)

        let extraOverlay =
            if scenesAcc.Count = 0 then
                ValueNone
            elif scenesAcc.Count = 1 then
                ValueSome scenesAcc[0]
            else
                ValueSome (Legend3D.combine viewportWidth viewportHeight (Seq.toList scenesAcc))

        {
            Batches = if batches.Count = 0 then ReadOnlyMemory.Empty else ReadOnlyMemory<InstanceBatch>(batches.ToArray ())
            Lines = if lines.Count = 0 then ReadOnlyMemory.Empty else ReadOnlyMemory<LineBatch>(lines.ToArray ())
            Discs = if discs.Count = 0 then ReadOnlyMemory.Empty else ReadOnlyMemory<DiscBatch>(discs.ToArray ())
            OverlayMarks = if marksAcc.Count = 0 then [] else Seq.toList marksAcc
            ExtraOverlay = extraOverlay
        }

    /// <summary>
    /// Compiles a sequence of <c>Layer3D</c> items, inferring an enclosing legend viewport when explicit dimensions
    /// are omitted.
    /// </summary>
    let layers (items: seq<Layer3D>) : CompiledPlotLayers3D =
        if isNull (box items) then
            nullArg (nameof items)

        let materialized = ResizeArray<Layer3D>()
        let mutable w = 800.0
        let mutable h = 600.0

        for item in items do
            match item with
            | Layer3D.Overlay readScene ->
                let sc = readScene ()

                if not (isNull (box sc)) then
                    let sw = Scene.width sc
                    let sh = Scene.height sc
                    if Check3D.isFinite sw && sw > w then w <- sw
                    if Check3D.isFinite sh && sh > h then h <- sh
                    materialized.Add (Layer3D.Overlay (fun () -> sc))
            | Layer3D.Colorbar spec ->
                materialized.Add item
                let rx = Bounds.x spec.Bounds + Bounds.width spec.Bounds + 64.0
                let by = Bounds.y spec.Bounds + Bounds.height spec.Bounds + 32.0
                if Check3D.isFinite rx && rx > w then w <- rx
                if Check3D.isFinite by && by > h then h <- by
            | Layer3D.CategoricalLegend (origin, _, _) ->
                materialized.Add item
                if Check3D.isFinite origin.X && origin.X + 180.0 > w then w <- origin.X + 180.0
                if Check3D.isFinite origin.Y && origin.Y + 120.0 > h then h <- origin.Y + 120.0
            | _ ->
                materialized.Add item

        compile w h materialized

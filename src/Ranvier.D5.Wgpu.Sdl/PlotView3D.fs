namespace Ranvier.D5.Wgpu.Sdl

open System
open System.Numerics
open System.Threading
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.D5.Wgpu
open Ranvier.Dom
open Ranvier.View
open Ranvier.View.Dsl
open Ranvier.View.Representation
open Ranvier.View.WebCore.Sdl
open Ranvier.Wgpu
open Ranvier.Wgpu.Sdl

/// <summary>Immutable 3D frame description specifying axis framing, clear/lighting/section state, GPU batches, and 2D SVG overlay marks.</summary>
type PlotPass3D =
    {
        /// <summary>3D axis scale, tick/grid configuration, theme, and margins.</summary>
        AxisSpec: Axis3DSpec
        /// <summary>Viewport clear color.</summary>
        ClearColor: RgbaColor
        /// <summary>Optional directional studio lighting rig.</summary>
        Lighting: LightingRig voption
        /// <summary>
        /// Optional world-space clipping plane.
        /// <c>Axis3D</c> back-wall panels (<c>InstanceFlags.IgnoreSectionPlane</c>), cuboid spines, gridlines, ticks, and <c>Probe3D</c> hover spikes
        /// (<c>LineVertex3D</c> <c>ignoreSectionPlane = true</c>) all opt out of <c>SectionPlane</c> clipping, while user data <c>ExtraBatches</c>,
        /// <c>ExtraLines</c> (unless marked <c>ignoreSectionPlane = true</c>), and <c>ExtraDiscs</c> are clipped in the negative half-space.
        /// </summary>
        SectionPlane: SectionPlane voption
        /// <summary>Additional hardware-instanced mesh batches rendered alongside axis wall panels.</summary>
        ExtraBatches: ReadOnlyMemory<InstanceBatch>
        /// <summary>Additional 3D hairline batches rendered alongside axis lines and hover spikes.</summary>
        ExtraLines: ReadOnlyMemory<LineBatch>
        /// <summary>Additional camera-facing SDF disc batches.</summary>
        ExtraDiscs: ReadOnlyMemory<DiscBatch>
        /// <summary>Additional 2D SVG overlay marks rendered over the axis label overlay.</summary>
        ExtraOverlayMarks: Mark list
        /// <summary>Optional full 2D <c>Scene</c> overlay rendered in a dedicated sibling passive container with scoped definitions.</summary>
        ExtraOverlay: Scene voption
    }

/// <summary>Constructors and pipeable modifiers for <c>PlotPass3D</c>.</summary>
[<RequireQualifiedAccess>]
module PlotPass3D =
    /// <summary>Creates a <c>PlotPass3D</c> for <c>axisSpec</c> with dark backdrop, studio lighting, no section plane, and empty extra layers.</summary>
    let create (axisSpec: Axis3DSpec) : PlotPass3D =
        {
            AxisSpec = axisSpec
            ClearColor = RgbaColor.darkBackdrop
            Lighting = ValueSome LightingRig.studioDefault
            SectionPlane = ValueNone
            ExtraBatches = ReadOnlyMemory.Empty
            ExtraLines = ReadOnlyMemory.Empty
            ExtraDiscs = ReadOnlyMemory.Empty
            ExtraOverlayMarks = []
            ExtraOverlay = ValueNone
        }

    let private fromCompiled (axisSpec: Axis3DSpec) (compiled: CompiledPlotLayers3D) : PlotPass3D =
        { create axisSpec with
            ExtraBatches = compiled.Batches
            ExtraLines = compiled.Lines
            ExtraDiscs = compiled.Discs
            ExtraOverlayMarks = compiled.OverlayMarks
            ExtraOverlay = compiled.ExtraOverlay
        }

    /// <summary>Creates a <c>PlotPass3D</c> compiling composable <c>Layer3D</c> entries alongside <c>axisSpec</c>.</summary>
    let ofLayers (axisSpec: Axis3DSpec) (layers: seq<Layer3D>) : PlotPass3D =
        Plot3D.layers layers |> fromCompiled axisSpec

    /// <summary>Creates a <c>PlotPass3D</c> compiling composable <c>Layer3D</c> entries using explicit viewport overlay bounds.</summary>
    let ofLayersSized (viewportWidth: float) (viewportHeight: float) (axisSpec: Axis3DSpec) (layers: seq<Layer3D>) : PlotPass3D =
        Plot3D.compile viewportWidth viewportHeight layers |> fromCompiled axisSpec

    /// <summary>Sets the viewport clear color on <c>pass</c>.</summary>
    let withClear (clearColor: RgbaColor) (pass: PlotPass3D) : PlotPass3D =
        { pass with ClearColor = clearColor }

    /// <summary>Sets or clears the directional studio lighting rig on <c>pass</c>.</summary>
    let withLighting (lighting: LightingRig voption) (pass: PlotPass3D) : PlotPass3D =
        { pass with Lighting = lighting }

    /// <summary>Sets or clears the world-space section clipping plane on <c>pass</c>.</summary>
    let withSection (sectionPlane: SectionPlane voption) (pass: PlotPass3D) : PlotPass3D =
        { pass with SectionPlane = sectionPlane }

/// <summary>Keyboard shortcut handling policy for <c>PlotView3D</c>.</summary>
[<RequireQualifiedAccess>]
type PlotKeyMap3D =
    /// <summary>Default camera shortcuts (<c>R</c>/<c>Home</c> reset, <c>P</c> projection, <c>M</c> mode, arrows orbit, <c>+</c>/<c>-</c> zoom).</summary>
    | Default
    /// <summary>Disables built-in keyboard camera shortcuts.</summary>
    | Disabled
    /// <summary>Applies a custom handler returning <c>ValueSome nextCamera</c> when the key event should update camera state and be consumed.</summary>
    | Custom of handler: (KeyboardEvent -> PlotCamera3D -> PlotCamera3D voption)

/// <summary>Viewport size, editable camera, input options, and optional GPU pick callbacks for <c>PlotView3D</c>.</summary>
type PlotViewConfig3D =
    {
        /// <summary>Logical viewport width in CSS pixels.</summary>
        Width: int
        /// <summary>Logical viewport height in CSS pixels.</summary>
        Height: int
        /// <summary>Reactive editable 3D camera updated by pointer orbit/pan, wheel zoom, and keyboard navigation.</summary>
        Camera: Editable<PlotCamera3D>
        /// <summary>Optional callback invoked with the resolved <c>PickHit</c> (or <c>ValueNone</c>) while hovering over the plot.</summary>
        OnHoverHit: (PickHit voption -> unit) option
        /// <summary>Optional callback invoked with the resolved <c>PickHit</c> (or <c>ValueNone</c>) on primary click without drag.</summary>
        OnClickHit: (PickHit voption -> unit) option
        /// <summary>When true, renders axis back-wall hairline spikes to the currently hovered pick point.</summary>
        ShowHoverSpikes: bool
        /// <summary>Keyboard shortcut mode (<c>Default</c>, <c>Disabled</c>, or <c>Custom</c>) for camera shortcuts.</summary>
        KeyMap: PlotKeyMap3D
        /// <summary>When true, automatically focuses the plot container on mount/window focus and pointer-down.</summary>
        AutoFocus: bool
    }

/// <summary>Constructors and pipeable configuration helpers for <c>PlotViewConfig3D</c>.</summary>
[<RequireQualifiedAccess>]
module PlotViewConfig3D =
    /// <summary>Creates a <c>PlotViewConfig3D</c> with <c>ShowHoverSpikes = true</c>, <c>KeyMap = PlotKeyMap3D.Default</c>, <c>AutoFocus = true</c>, and no pick callbacks.</summary>
    let create (width: int) (height: int) (camera: Editable<PlotCamera3D>) : PlotViewConfig3D =
        {
            Width = width
            Height = height
            Camera = camera
            OnHoverHit = None
            OnClickHit = None
            ShowHoverSpikes = true
            KeyMap = PlotKeyMap3D.Default
            AutoFocus = true
        }

    /// <summary>Sets the hover GPU pick callback on <c>config</c>.</summary>
    let withOnHoverHit (handler: PickHit voption -> unit) (config: PlotViewConfig3D) : PlotViewConfig3D =
        { config with OnHoverHit = Some handler }

    /// <summary>Sets the click GPU pick callback on <c>config</c>.</summary>
    let withOnClickHit (handler: PickHit voption -> unit) (config: PlotViewConfig3D) : PlotViewConfig3D =
        { config with OnClickHit = Some handler }

    /// <summary>Enables or disables back-wall hover spike rendering on <c>config</c>.</summary>
    let withHoverSpikes (enabled: bool) (config: PlotViewConfig3D) : PlotViewConfig3D =
        { config with ShowHoverSpikes = enabled }

    /// <summary>Sets the keyboard shortcut mode on <c>config</c>.</summary>
    let withKeyMap (keyMap: PlotKeyMap3D) (config: PlotViewConfig3D) : PlotViewConfig3D =
        { config with KeyMap = keyMap }

    /// <summary>Enables or disables automatic focus acquisition on mount and pointer-down.</summary>
    let withAutoFocus (enabled: bool) (config: PlotViewConfig3D) : PlotViewConfig3D =
        { config with AutoFocus = enabled }

module internal PlotView3DStats =
    let mutable PickCalls = 0L
    let mutable PanelDrawBeforeLines = false

type private PlotViewProps3D =
    {
        Window: Window
        Context: WgpuContext
        Config: PlotViewConfig3D
        ReadScene: unit -> PlotPass3D
    }

[<Sealed>]
type private PlotViewInstance3D
    (
        window: Window,
        context: WgpuContext,
        config: PlotViewConfig3D,
        readScene: unit -> PlotPass3D
    ) =
    let containerRef: NodeRef<HTMLDivElement> = NodeRef.create ()
    let canvasRef: NodeRef<HTMLCanvasElement> = NodeRef.create ()
    let emptyOverlay = Scene.create (float config.Width) (float config.Height) "PlotView3D" []
    let overlaySig = Api.createSignal emptyOverlay
    let emptyExtraOverlay = Scene.create (float config.Width) (float config.Height) "PlotView3D.ExtraOverlay" []
    let extraOverlaySig = Api.createSignal emptyExtraOverlay

    let boxMesh = context.UploadMesh (Primitives.unitBox ())
    let panelBatch = context.CreateBatch (boxMesh, BlendMode.AlphaBlend, false, drawBeforeLines = true)
    do PlotView3DStats.PanelDrawBeforeLines <- panelBatch.DrawBeforeLines
    let axisLineBatch = context.CreateLineBatch ()
    let spikeLineBatch = context.CreateLineBatch ()
    let axisMemo = Axis3DMemo (panelBatch, axisLineBatch)
    let inputKernel = PlotInputKernel3D ()
    let hoverCoalescer = HoverPickCoalescer ()
    let batchPrefixCache = ScenePrefixCache (panelBatch, ValueNone)
    let linePrefixCache = ScenePrefixCache (axisLineBatch, ValueSome spikeLineBatch)

    let mutable disposed = false
    let mutable dirty = true
    let mutable rafId: int option = None
    let mutable surfaceOpt: WgpuSdlSurface option = None
    let mutable latestCam: Camera3D option = None
    let mutable latestPass: PlotPass3D option = None
    let mutable hasHoverSpikes = false
    let mutable lastHoverHit: PickHit voption = ValueNone
    let mutable autoFocusedOnMount = false
    let mutable lastCanvasW = config.Width
    let mutable lastCanvasH = config.Height
    let mutable prevHadExtraMarks = false
    let mutable prevExtraScene: Scene voption = ValueNone

    let allowHoverPick = config.ShowHoverSpikes || config.OnHoverHit.IsSome

    let buildScenePass (cam: Camera3D) (pass: PlotPass3D) (includeAxis: bool) =
        let basePass =
            ScenePass.create cam
            |> ScenePass.withClear pass.ClearColor
            |> ScenePass.withDiscMemory pass.ExtraDiscs

        let withLight =
            match pass.Lighting with
            | ValueSome light -> ScenePass.withLighting light basePass
            | ValueNone -> basePass

        let withSec =
            match pass.SectionPlane with
            | ValueSome sec -> ScenePass.withSection sec withLight
            | ValueNone -> withLight

        if includeAxis then
            withSec
            |> ScenePass.withBatchMemory (batchPrefixCache.Sync pass.ExtraBatches)
            |> ScenePass.withLineMemory (linePrefixCache.Sync pass.ExtraLines)
        else
            withSec |> ScenePass.withBatchMemory pass.ExtraBatches

    let renderNow () =
        if not disposed then
            match surfaceOpt, latestCam, latestPass, axisMemo.CurrentFrame with
            | Some surface, Some cam, Some pass, ValueSome _ when not surface.IsDisposed && not context.IsDisposed ->
                dirty <- false
                let scenePass = buildScenePass cam pass true
                WgpuSdlSurface.render surface scenePass |> ignore
            | _ -> ()

    let viewportCssSize () =
        match containerRef.Current with
        | Some el ->
            let rect = el.getBoundingClientRect ()
            let w = if rect.width > 0.0 then rect.width else float config.Width
            let h = if rect.height > 0.0 then rect.height else float config.Height
            struct (rect.x, rect.y, w, h)
        | None -> struct (0.0, 0.0, float config.Width, float config.Height)

    let dprScale () : struct (float * float) =
        let struct (winPxW, winPxH) = window.PixelSize
        let docEl = window.Host.Document.documentElement

        if not (isNull docEl) && docEl.clientWidth > 0 && docEl.clientHeight > 0 && winPxW > 0 && winPxH > 0 then
            struct (float winPxW / float docEl.clientWidth, float winPxH / float docEl.clientHeight)
        elif window.DeviceScale > 0.0 then
            struct (window.DeviceScale, window.DeviceScale)
        else
            struct (1.0, 1.0)

    let isInteractiveFormTarget (target: obj) =
        match target with
        | :? Element as el ->
            let tag = el.tagName

            if isNull tag then
                false
            else
                match tag.ToUpperInvariant () with
                | "INPUT" | "TEXTAREA" | "SELECT" | "BUTTON" -> true
                | _ -> false
        | _ -> false

    let tryAutoFocusContainer (forceFromPointer: bool) =
        if config.AutoFocus && not disposed then
            match containerRef.Current with
            | Some el ->
                let doc = window.Host.Document
                let active = if isNull doc then null else doc.activeElement

                let canSteal =
                    forceFromPointer
                    || isNull active
                    || (not (isNull doc.body) && Object.ReferenceEquals (active, doc.body))
                    || (not (isNull doc.documentElement) && Object.ReferenceEquals (active, doc.documentElement))

                if canSteal && not (Object.ReferenceEquals (active, el)) then
                    try
                        el.focus ()
                    with _ ->
                        ()
            | None -> ()

    let applyOverlayMarks (logicalW: int) (logicalH: int) (baseOverlay: Scene) (extraMarks: Mark list) =
        let hasExtraMarks = not (List.isEmpty extraMarks)
        overlaySig.Value <- Plot3D.composeOverlay logicalW logicalH baseOverlay extraMarks
        prevHadExtraMarks <- hasExtraMarks

    let updateAxisState (axisSpec: Axis3DSpec) (logicalW: int) (logicalH: int) (gpuCam: Camera3D) (extraMarks: Mark list) =
        let hadAxisBefore = axisMemo.HasCached
        let update = axisMemo.Update (axisSpec, logicalW, logicalH, gpuCam)

        if hadAxisBefore && update.AxisSpecChanged then
            hoverCoalescer.AdvanceEpoch ()
            lastHoverHit <- ValueNone

            if hasHoverSpikes then
                spikeLineBatch.Resize 0
                hasHoverSpikes <- false

        if update.Rebuilt || prevHadExtraMarks || not (List.isEmpty extraMarks) then
            applyOverlayMarks logicalW logicalH update.BaseOverlay extraMarks

        update.Rebuilt

    let syncViewport () =
        if not disposed && not context.IsDisposed then
            let struct (_, _, cssW, cssH) = viewportCssSize ()
            let struct (dprX, dprY) = dprScale ()
            let logicalW = max 1 (int (Math.Round cssW))
            let logicalH = max 1 (int (Math.Round cssH))
            let targetDevW = max 1 (int (Math.Round (cssW * dprX)))
            let targetDevH = max 1 (int (Math.Round (cssH * dprY)))

            match surfaceOpt with
            | Some surface when not surface.IsDisposed ->
                if
                    surface.Width <> targetDevW
                    || surface.Height <> targetDevH
                    || context.Width <> targetDevW
                    || context.Height <> targetDevH
                then
                    surface.Resize (targetDevW, targetDevH)
                    dirty <- true

                if lastCanvasW <> targetDevW || lastCanvasH <> targetDevH then
                    match canvasRef.Current with
                    | Some canvasEl ->
                        lastCanvasW <- targetDevW
                        lastCanvasH <- targetDevH
                        canvasEl.setAttribute ("width", string targetDevW)
                        canvasEl.setAttribute ("height", string targetDevH)
                    | None -> ()
            | _ -> ()

            match axisMemo.CurrentSpec, latestCam, latestPass with
            | ValueSome spec, Some gpuCam, Some pass ->
                let update = axisMemo.Update (spec, logicalW, logicalH, gpuCam)

                if update.Rebuilt then
                    applyOverlayMarks logicalW logicalH update.BaseOverlay pass.ExtraOverlayMarks
                    dirty <- true
            | _ -> ()

    let pickAtClient (clientX: float) (clientY: float) : PickHit voption =
        if disposed || context.IsDisposed then
            ValueNone
        else
            match latestCam, latestPass with
            | Some cam, Some pass ->
                let struct (rx, ry, rw, rh) = viewportCssSize ()
                let nx = Math.Clamp ((clientX - rx) / rw, 0.0, 1.0)
                let ny = Math.Clamp ((clientY - ry) / rh, 0.0, 1.0)
                let px = Math.Clamp (int (Math.Round (nx * float context.Width)), 0, max 0 (context.Width - 1))
                let py = Math.Clamp (int (Math.Round (ny * float context.Height)), 0, max 0 (context.Height - 1))
                let pickPass = buildScenePass cam pass false
                Interlocked.Increment (&PlotView3DStats.PickCalls) |> ignore
                WgpuContext.pick px py context pickPass
            | _ -> ValueNone

    let applyHoverSpikes (hit: PickHit voption) =
        if config.ShowHoverSpikes then
            match hit, latestPass, axisMemo.CurrentFrame with
            | ValueSome h, Some pass, ValueSome frame when h.PickId <> 0u ->
                let tc = pass.AxisSpec.Theme.TickColor
                let spikeColor = RgbaColor (tc.R, tc.G, tc.B, 0.95f)
                let spikes = Probe3D.hoverSpikes pass.AxisSpec.Scale frame spikeColor h.WorldPosition
                spikeLineBatch.UpdateVertices spikes.Span
                hasHoverSpikes <- true
                dirty <- true
            | _ ->
                if hasHoverSpikes then
                    spikeLineBatch.Resize 0
                    hasHoverSpikes <- false
                    dirty <- true
        elif hasHoverSpikes then
            spikeLineBatch.Resize 0
            hasHoverSpikes <- false
            dirty <- true

    let rec scheduleFrame () =
        if not disposed && (dirty || hoverCoalescer.HasPendingFrameHover) && rafId.IsNone && surfaceOpt.IsSome then
            let id =
                window.Host.RequestAnimationFrame (fun _ ->
                    rafId <- None

                    if not disposed then
                        syncViewport ()

                        match hoverCoalescer.TakeFrameHover () with
                        | ValueSome struct (hx, hy, pickEpoch) ->
                            if not inputKernel.IsDragging && pickEpoch = hoverCoalescer.CurrentEpoch then
                                let hit = pickAtClient hx hy

                                if not disposed && pickEpoch = hoverCoalescer.CurrentEpoch then
                                    if hit.IsSome || lastHoverHit.IsSome then
                                        lastHoverHit <- hit
                                        applyHoverSpikes hit
                                        config.OnHoverHit |> Option.iter (fun cb -> cb hit)
                        | ValueNone -> ()

                        if not disposed && dirty then
                            renderNow ())

            rafId <- Some id

    let cancelFrame () =
        match rafId with
        | Some id ->
            rafId <- None
            window.Host.CancelAnimationFrame id
        | None -> ()

    let updateSpikes (hit: PickHit voption) =
        applyHoverSpikes hit

        if dirty then
            scheduleFrame ()

    let releaseCapture (pointerId: int) =
        match containerRef.Current with
        | Some el when pointerId >= 0 ->
            if el.hasPointerCapture pointerId then
                el.releasePointerCapture pointerId
        | _ -> ()

    let cleanupResources () =
        if not disposed then
            disposed <- true
            dirty <- false
            hoverCoalescer.AdvanceEpoch ()
            cancelFrame ()
            surfaceOpt <- None
            spikeLineBatch.Dispose ()
            axisLineBatch.Dispose ()
            panelBatch.Dispose ()
            boxMesh.Dispose ()

    do
        Api.onCleanup cleanupResources

        Api.createEffect (fun () ->
            if not disposed && not context.IsDisposed then
                if config.AutoFocus && window.Focused.Value then
                    tryAutoFocusContainer false

                let plotCam = config.Camera.Value
                let pass = readScene ()
                let gpuCam = PlotCamera3D.toCamera3D pass.AxisSpec.Scale plotCam
                let struct (_, _, cssW, cssH) = viewportCssSize ()
                let logicalW = max 1 (int (Math.Round cssW))
                let logicalH = max 1 (int (Math.Round cssH))

                updateAxisState pass.AxisSpec logicalW logicalH gpuCam pass.ExtraOverlayMarks |> ignore

                match pass.ExtraOverlay with
                | ValueSome extraScene ->
                    let unchanged =
                        match prevExtraScene with
                        | ValueSome prev -> Object.ReferenceEquals (box prev, box extraScene)
                        | ValueNone -> false

                    if not unchanged then
                        extraOverlaySig.Value <- extraScene
                        prevExtraScene <- ValueSome extraScene
                | ValueNone ->
                    if prevExtraScene.IsSome then
                        extraOverlaySig.Value <- emptyExtraOverlay
                        prevExtraScene <- ValueNone

                latestCam <- Some gpuCam
                latestPass <- Some pass
                dirty <- true
                scheduleFrame ())

    member _.ContainerRef = containerRef
    member _.CanvasRef = canvasRef
    member _.ReadOverlay = fun () -> overlaySig.Value
    member _.ReadExtraOverlay = fun () -> extraOverlaySig.Value
    member _.ContainerStyle =
        $"position:relative;width:{config.Width}px;height:{config.Height}px;overflow:hidden;outline:none;user-select:none;"
    member _.CanvasWidthAttr = string config.Width
    member _.CanvasHeightAttr = string config.Height

    member _.AttachCanvas(el: HTMLCanvasElement) : unit -> unit =
        let lease =
            WgpuSdlSurface.lend
                window
                context
                (fun surface ->
                    if not disposed then
                        surfaceOpt <- Some surface
                        syncViewport ()

                        if config.AutoFocus && not autoFocusedOnMount then
                            autoFocusedOnMount <- true
                            tryAutoFocusContainer false

                        dirty <- true
                        scheduleFrame ()

                    { new IDisposable with
                        member _.Dispose() = cleanupResources () })
                el

        fun () -> lease.Dispose ()

    member _.OnPointerDown(ev: PointerEvent) =
        if not disposed then
            let isInteractive = isInteractiveFormTarget ev.target

            if inputKernel.OnPointerDown (ev.pointerId, ev.button, ev.clientX, ev.clientY, ev.shiftKey, isInteractive) then
                hoverCoalescer.AdvanceEpoch ()
                tryAutoFocusContainer true

                match containerRef.Current with
                | Some el ->
                    try
                        el.setPointerCapture ev.pointerId
                    with _ ->
                        ()
                | None -> ()

    member _.OnPointerMove(ev: PointerEvent) =
        if not disposed then
            let struct (_, _, rw, rh) = viewportCssSize ()

            let fitR =
                match latestPass with
                | Some p -> Scale3D.fitRadius p.AxisSpec.Scale
                | None -> 1.0f

            match
                inputKernel.OnPointerMove (
                    ev.pointerId,
                    ev.clientX,
                    ev.clientY,
                    rw,
                    rh,
                    fitR,
                    config.Camera.Value,
                    allowHoverPick
                )
            with
            | PointerMoveResult3D.CameraUpdated nextCam ->
                config.Camera.Value <- nextCam
            | PointerMoveResult3D.HoverMoved ->
                hoverCoalescer.QueueFrameHover (ev.clientX, ev.clientY)
                scheduleFrame ()
            | PointerMoveResult3D.NoOp -> ()

    member _.OnPointerUp(ev: PointerEvent) =
        if not disposed then
            let up = inputKernel.OnPointerUp (ev.pointerId, ev.button, ev.clientX, ev.clientY)

            if up.Handled then
                releaseCapture ev.pointerId

                if up.WasClick && (config.ShowHoverSpikes || config.OnClickHit.IsSome) then
                    let hit = pickAtClient up.ClientX up.ClientY
                    lastHoverHit <- hit
                    updateSpikes hit
                    config.OnClickHit |> Option.iter (fun cb -> cb hit)

    member _.OnPointerCancel(ev: PointerEvent) =
        if not disposed && inputKernel.OnPointerCancel ev.pointerId then
            releaseCapture ev.pointerId
            hoverCoalescer.AdvanceEpoch ()

    member _.OnWheel(ev: WheelEvent) =
        if not disposed then
            ev.preventDefault ()

            match PlotInputKernel3D.ApplyWheel (ev.deltaY, config.Camera.Value) with
            | ValueSome nextCam -> config.Camera.Value <- nextCam
            | ValueNone -> ()

    member _.OnKeyDown(ev: KeyboardEvent) =
        if
            not disposed
            && not ev.defaultPrevented
            && not ev.ctrlKey
            && not ev.metaKey
            && not ev.altKey
            && not (isInteractiveFormTarget ev.target)
        then
            match config.KeyMap with
            | PlotKeyMap3D.Disabled -> ()
            | PlotKeyMap3D.Custom handler ->
                match handler ev config.Camera.Value with
                | ValueSome nextCam ->
                    ev.preventDefault ()
                    config.Camera.Value <- nextCam
                | ValueNone -> ()
            | PlotKeyMap3D.Default ->
                match PlotInputKernel3D.ApplyDefaultKey (ev.key, config.Camera.Value) with
                | KeyCameraAction3D.Reset ->
                    ev.preventDefault ()
                    config.Camera.Reset ()
                | KeyCameraAction3D.SetCamera nextCam ->
                    ev.preventDefault ()
                    config.Camera.Value <- nextCam
                | KeyCameraAction3D.Ignore -> ()

/// <summary>Declarative <c>Ranvier.View.Dsl</c> host binding a WebGPU 3D surface and pointer-transparent SVG axis overlay inside an SDL window.</summary>
[<RequireQualifiedAccess>]
module PlotView3D =
    let private plotComponent: Component<PlotViewProps3D> =
        Component.template
            (fun _ (props: PlotViewProps3D) ->
                PlotViewInstance3D (props.Window, props.Context, props.Config, props.ReadScene))
            (div<PlotViewInstance3D>(
                [|
                    div.style (fun (inst: PlotViewInstance3D) -> inst.ContainerStyle)
                    div.props.tabIndex 0
                    div.ref (fun (inst: PlotViewInstance3D) -> inst.ContainerRef)
                    div.onPointerdown (fun (inst: PlotViewInstance3D) ev -> inst.OnPointerDown ev)
                    div.onPointermove (fun (inst: PlotViewInstance3D) ev -> inst.OnPointerMove ev)
                    div.onPointerup (fun (inst: PlotViewInstance3D) ev -> inst.OnPointerUp ev)
                    div.onPointercancel (fun (inst: PlotViewInstance3D) ev -> inst.OnPointerCancel ev)
                    div.onWheel (fun (inst: PlotViewInstance3D) ev -> inst.OnWheel ev)
                    div.onKeydown (fun (inst: PlotViewInstance3D) ev -> inst.OnKeyDown ev)
                |]
            )
                .Run
                [|
                    canvas<PlotViewInstance3D>(
                        [|
                            canvas.style "position:absolute;inset:0;width:100%;height:100%;display:block;"
                            canvas.ref (fun (inst: PlotViewInstance3D) -> inst.CanvasRef)
                            canvas.attribute ("width", (fun (inst: PlotViewInstance3D) -> Some inst.CanvasWidthAttr))
                            canvas.attribute ("height", (fun (inst: PlotViewInstance3D) -> Some inst.CanvasHeightAttr))
                            canvas.lend (fun (inst: PlotViewInstance3D) el -> inst.AttachCanvas el)
                        |]
                    )
                        .Run [||]
                    div<PlotViewInstance3D>(
                        [|
                            div.style "position:absolute;inset:0;pointer-events:none;"
                        |]
                    )
                        .Run [| view (fun (inst: PlotViewInstance3D) -> Component.render Renderer.passive inst.ReadOverlay) |]
                    div<PlotViewInstance3D>(
                        [|
                            div.style "position:absolute;inset:0;pointer-events:none;"
                        |]
                    )
                        .Run [| view (fun (inst: PlotViewInstance3D) -> Component.render Renderer.passive inst.ReadExtraOverlay) |]
                |])

    /// <summary>
    /// Creates a <c>Ranvier.View.Dsl</c> interactive 3D plot view bound to <c>window</c> and <c>context</c>,
    /// rendering <c>readScene ()</c> on dirty frames alongside a pointer-transparent <c>Renderer.passive</c> SVG axis overlay.
    /// </summary>
    let create
        (window: Window)
        (context: WgpuContext)
        (config: PlotViewConfig3D)
        (readScene: unit -> PlotPass3D)
        : Program.View =
        if isNull (box window) then
            nullArg (nameof window)

        if isNull (box context) then
            nullArg (nameof context)

        if isNull (box config.Camera) then
            nullArg "config.Camera"

        if isNull (box readScene) then
            nullArg (nameof readScene)

        if config.Width <= 0 then
            raise (ArgumentOutOfRangeException ("config.Width", "Viewport width must be positive."))

        if config.Height <= 0 then
            raise (ArgumentOutOfRangeException ("config.Height", "Viewport height must be positive."))

        Component.render
            plotComponent
            {
                Window = window
                Context = context
                Config = config
                ReadScene = readScene
            }

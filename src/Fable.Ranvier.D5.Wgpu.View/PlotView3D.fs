namespace Ranvier.D5.Wgpu.View

open System
open System.Threading.Tasks
#if FABLE_COMPILER
open Fable.Core
open Fickl.Core
#endif
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.D5.Wgpu
open Ranvier.Dom
open Ranvier.View
open Ranvier.View.Dsl
open Ranvier.View.Representation
open Ranvier.Wgpu

/// <summary>Immutable 3D frame description specifying axis framing, clear/lighting/section state, GPU batches, and 2D SVG overlay marks.</summary>
type PlotPass3D =
    {
        /// <summary>3D axis scale, tick/grid configuration, theme, and margins.</summary>
        AxisSpec: Axis3DSpec
        /// <summary>Viewport clear color.</summary>
        ClearColor: RgbaColor
        /// <summary>Optional directional studio lighting rig.</summary>
        Lighting: LightingRig voption
        /// <summary>Optional world-space clipping plane; axis frames and hover spikes opt out cleanly.</summary>
        SectionPlane: SectionPlane voption
        /// <summary>Additional hardware-instanced mesh batches rendered alongside axis wall panels.</summary>
        ExtraBatches: ReadOnlyMemory<InstanceBatch>
        /// <summary>Additional 3D hairline batches rendered alongside axis lines and hover spikes.</summary>
        ExtraLines: ReadOnlyMemory<LineBatch>
        /// <summary>Additional camera-facing SDF disc batches.</summary>
        ExtraDiscs: ReadOnlyMemory<DiscBatch>
        /// <summary>Additional 2D SVG overlay marks rendered over the axis label overlay.</summary>
        ExtraOverlayMarks: Mark list
        /// <summary>Optional full 2D <c>Scene</c> overlay rendered in a dedicated sibling passive container preserving <c>&lt;defs&gt;</c> gradients.</summary>
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

/// <summary>Keyboard shortcut handling policy for browser <c>PlotView3D</c>.</summary>
[<RequireQualifiedAccess>]
type PlotKeyMap3D =
    /// <summary>Default camera shortcuts (<c>R</c>/<c>Home</c> reset, <c>P</c> projection, <c>M</c> mode, arrows orbit, <c>+</c>/<c>-</c> zoom).</summary>
    | Default
    /// <summary>Disables built-in keyboard camera shortcuts.</summary>
    | Disabled
    /// <summary>Applies a custom handler returning <c>ValueSome nextCamera</c> when the key event should update camera state and be consumed.</summary>
    | Custom of handler: (KeyboardEvent -> PlotCamera3D -> PlotCamera3D voption)

/// <summary>Viewport size, editable camera, input options, and optional GPU pick callbacks for browser <c>PlotView3D</c>.</summary>
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
        /// <summary>When true, automatically focuses the plot container on mount and pointer-down.</summary>
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

module internal BrowserInterop3D =
#if FABLE_COMPILER
    [<Emit("typeof window !== 'undefined' && window.devicePixelRatio > 0 ? window.devicePixelRatio : 1")>]
    let devicePixelRatio () : float = jsNative

    [<Emit("typeof window !== 'undefined' && window.requestAnimationFrame ? window.requestAnimationFrame($0) : 0")>]
    let requestAnimationFrame (cb: float -> unit) : int = jsNative

    [<Emit("if (typeof window !== 'undefined' && window.cancelAnimationFrame) window.cancelAnimationFrame($0)")>]
    let cancelAnimationFrame (id: int) : unit = jsNative

    [<Emit("(() => { if (typeof ResizeObserver === 'undefined') return () => {}; const obs = new ResizeObserver(() => $1()); obs.observe($0); return () => obs.disconnect(); })()")>]
    let observeResize (el: obj) (onResize: unit -> unit) : unit -> unit = jsNative

    [<Emit("try { if ($0 && $0.setPointerCapture) $0.setPointerCapture($1); } catch (_) {}")>]
    let trySetPointerCapture (el: obj) (pointerId: int) : unit = jsNative

    [<Emit("try { if ($0 && $0.hasPointerCapture && $0.hasPointerCapture($1)) $0.releasePointerCapture($1); } catch (_) {}")>]
    let tryReleasePointerCapture (el: obj) (pointerId: int) : unit = jsNative

    [<Emit("$0.then($1, $2)")>]
    let thenPromise (promise: JS.Promise<'T>) (onOk: 'T -> unit) (onErr: obj -> unit) : unit = jsNative

    [<Emit("$0")>]
    let promiseToTask (promise: JS.Promise<'T>) : Task<'T> = jsNative

    [<Emit("($0 != null && typeof $0.tagName === 'string') ? $0.tagName.toUpperCase() : ''")>]
    let private getUpperTagName (_target: obj) : string = jsNative

    let isInteractiveFormTarget (target: obj) =
        match getUpperTagName target with
        | "INPUT" | "TEXTAREA" | "SELECT" | "BUTTON" -> true
        | _ -> false
#else
    let devicePixelRatio () : float = 1.0
    let requestAnimationFrame (_cb: float -> unit) : int = 0
    let cancelAnimationFrame (_id: int) : unit = ()
    let observeResize (_el: obj) (_onResize: unit -> unit) : unit -> unit = fun () -> ()
    let trySetPointerCapture (_el: obj) (_pointerId: int) : unit = ()
    let tryReleasePointerCapture (_el: obj) (_pointerId: int) : unit = ()

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
#endif

type private PlotViewProps3D =
    {
        Canvas: HTMLCanvasElement
        Context: WgpuContext
        Config: PlotViewConfig3D
        ReadScene: unit -> PlotPass3D
    }

[<Sealed>]
type private PlotViewInstance3D
    (
        canvas: HTMLCanvasElement,
        context: WgpuContext,
        config: PlotViewConfig3D,
        readScene: unit -> PlotPass3D
    ) =
    let containerRef: NodeRef<HTMLDivElement> = NodeRef.create ()
    let emptyOverlay = Scene.create (float config.Width) (float config.Height) "PlotView3D" []
    let overlaySig = Api.createSignal emptyOverlay
    let emptyExtraOverlay = Scene.create (float config.Width) (float config.Height) "PlotView3D.ExtraOverlay" []
    let extraOverlaySig = Api.createSignal emptyExtraOverlay

    let boxMesh = context.UploadMesh (Primitives.unitBox ())
    let panelBatch = context.CreateBatch (boxMesh, BlendMode.AlphaBlend, false, drawBeforeLines = true)
    let axisLineBatch = context.CreateLineBatch ()
    let spikeLineBatch = context.CreateLineBatch ()
    let axisMemo = Axis3DMemo (panelBatch, axisLineBatch)
    let inputKernel = PlotInputKernel3D ()
    let hoverCoalescer = HoverPickCoalescer ()
    let batchPrefixCache = ScenePrefixCache (panelBatch, ValueNone)
    let linePrefixCache = ScenePrefixCache (axisLineBatch, ValueSome spikeLineBatch)

    let mutable disposed = false
    let mutable mounted = false
    let mutable dirty = true
    let mutable rafId: int option = None
    let mutable latestCam: Camera3D option = None
    let mutable latestPass: PlotPass3D option = None
    let mutable hasHoverSpikes = false
    let mutable lastHoverHit: PickHit voption = ValueNone
    let mutable autoFocusedOnMount = false
    let mutable prevHadExtraMarks = false
    let mutable prevExtraScene: Scene voption = ValueNone

    let allowHoverPick = config.ShowHoverSpikes || config.OnHoverHit.IsSome

    let viewportCssSize () =
        match containerRef.Current with
        | Some el ->
            let rect = el.getBoundingClientRect ()
            let w = if rect.width > 0.0 then rect.width else float config.Width
            let h = if rect.height > 0.0 then rect.height else float config.Height
            struct (rect.x, rect.y, w, h)
        | None -> struct (0.0, 0.0, float config.Width, float config.Height)

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
            let dpr = BrowserInterop3D.devicePixelRatio ()
            let logicalW = max 1 (int (Math.Round cssW))
            let logicalH = max 1 (int (Math.Round cssH))
            let targetDevW = max 1 (int (Math.Round (cssW * dpr)))
            let targetDevH = max 1 (int (Math.Round (cssH * dpr)))

            let uDevW = uint32 targetDevW
            let uDevH = uint32 targetDevH

            if context.Width <> targetDevW || context.Height <> targetDevH || canvas.width <> uDevW || canvas.height <> uDevH then
                canvas.width <- uDevW
                canvas.height <- uDevH
                context.Resize (targetDevW, targetDevH)
                dirty <- true

            match axisMemo.CurrentSpec, latestCam, latestPass with
            | ValueSome spec, Some gpuCam, Some pass ->
                let update = axisMemo.Update (spec, logicalW, logicalH, gpuCam)

                if update.Rebuilt then
                    applyOverlayMarks logicalW logicalH update.BaseOverlay pass.ExtraOverlayMarks
                    dirty <- true
            | _ -> ()

    let renderNow () =
        if not disposed && mounted && not context.IsDisposed then
            match latestCam, latestPass, axisMemo.CurrentFrame with
            | Some cam, Some pass, ValueSome _ ->
                dirty <- false
                let scenePass = buildScenePass cam pass true
                context.Render scenePass |> ignore
            | _ -> ()

    let rec scheduleFrame () =
        if not disposed && mounted && dirty && rafId.IsNone then
            let id =
                BrowserInterop3D.requestAnimationFrame (fun _ ->
                    rafId <- None

                    if not disposed then
                        syncViewport ()

                        if not disposed && dirty then
                            renderNow ())

            rafId <- Some id

    let cancelFrame () =
        match rafId with
        | Some id ->
            rafId <- None
            BrowserInterop3D.cancelAnimationFrame id
        | None -> ()

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

    let updateSpikes (hit: PickHit voption) =
        applyHoverSpikes hit

        if dirty then
            scheduleFrame ()

    let tryAutoFocusContainer (forceFromPointer: bool) =
        if config.AutoFocus && not disposed then
            match containerRef.Current with
            | Some el ->
                let doc = (el :> Node).ownerDocument

                if not (isNull doc) then
                    let active = doc.activeElement

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

    let clientToPickCoords (clientX: float) (clientY: float) =
        let struct (rx, ry, rw, rh) = viewportCssSize ()
        let nx = Math.Clamp ((clientX - rx) / rw, 0.0, 1.0)
        let ny = Math.Clamp ((clientY - ry) / rh, 0.0, 1.0)
        let px = Math.Clamp (int (Math.Round (nx * float context.Width)), 0, max 0 (context.Width - 1))
        let py = Math.Clamp (int (Math.Round (ny * float context.Height)), 0, max 0 (context.Height - 1))
        struct (px, py)

    let rec drainHoverFollowup (followupOpt: struct (float * float * int) voption) =
        match followupOpt with
        | ValueSome struct (nx, ny, nextEp) ->
            let struct (_, followAgain) = hoverCoalescer.CompletePick nextEp
            drainHoverFollowup followAgain
        | ValueNone -> ()

    let rec dispatchAsyncHover (clientX: float) (clientY: float) (dispatchEpoch: int) =
        if disposed || context.IsDisposed || inputKernel.IsDragging || dispatchEpoch <> hoverCoalescer.CurrentEpoch then
            let struct (_, followup) = hoverCoalescer.CompletePick dispatchEpoch
            drainHoverFollowup followup
        else
            match latestCam, latestPass with
            | Some cam, Some pass ->
                let struct (px, py) = clientToPickCoords clientX clientY
                let pickPass = buildScenePass cam pass false
                let promise = context.PickAsync (px, py, pickPass)

                BrowserInterop3D.thenPromise
                    promise
                    (fun hit ->
                        let struct (isCurrent, followup) = hoverCoalescer.CompletePick dispatchEpoch

                        if not disposed && isCurrent && not inputKernel.IsDragging then
                            if hit.IsSome || lastHoverHit.IsSome then
                                lastHoverHit <- hit
                                updateSpikes hit
                                config.OnHoverHit |> Option.iter (fun cb -> cb hit)

                        match followup with
                        | ValueSome struct (nx, ny, nextEp) ->
                            if not disposed && not inputKernel.IsDragging && allowHoverPick then
                                dispatchAsyncHover nx ny nextEp
                            else
                                let struct (_, nextFollow) = hoverCoalescer.CompletePick nextEp
                                drainHoverFollowup nextFollow
                        | ValueNone -> ())
                    (fun _ ->
                        let struct (_, followup) = hoverCoalescer.CompletePick dispatchEpoch
                        drainHoverFollowup followup)
            | _ ->
                let struct (_, followup) = hoverCoalescer.CompletePick dispatchEpoch
                drainHoverFollowup followup

    let dispatchClickPick (clientX: float) (clientY: float) (clickEpoch: int) =
        if not disposed && not context.IsDisposed then
            match latestCam, latestPass with
            | Some cam, Some pass ->
                let struct (px, py) = clientToPickCoords clientX clientY
                let pickPass = buildScenePass cam pass false

                BrowserInterop3D.thenPromise
                    (context.PickAsync (px, py, pickPass))
                    (fun hit ->
                        if not disposed && clickEpoch = hoverCoalescer.CurrentEpoch then
                            lastHoverHit <- hit
                            updateSpikes hit
                            config.OnClickHit |> Option.iter (fun cb -> cb hit))
                    (fun _ -> ())
            | _ -> ()

    let cleanupResources () =
        if not disposed then
            disposed <- true
            mounted <- false
            dirty <- false
            hoverCoalescer.AdvanceEpoch ()
            cancelFrame ()
            spikeLineBatch.Dispose ()
            axisLineBatch.Dispose ()
            panelBatch.Dispose ()
            boxMesh.Dispose ()

    do
        Api.onCleanup cleanupResources

        Api.createEffect (fun () ->
            if not disposed && not context.IsDisposed then
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
    member _.ReadOverlay = fun () -> overlaySig.Value
    member _.ReadExtraOverlay = fun () -> extraOverlaySig.Value
    member _.ContainerStyle =
        $"position:relative;width:{config.Width}px;height:{config.Height}px;overflow:hidden;outline:none;user-select:none;"

    member _.AttachCanvasHost(slotEl: HTMLDivElement) : unit -> unit =
        let canvasNode = canvas :> Node
        let prevParent = canvasNode.parentNode
        let prevNext = canvasNode.nextSibling
        let prevStyle = canvas.getAttribute "style"
        let prevWidthAttr = canvas.getAttribute "width"
        let prevHeightAttr = canvas.getAttribute "height"

        canvas.setAttribute ("style", "position:absolute;inset:0;width:100%;height:100%;display:block;")
        slotEl.appendChild canvasNode |> ignore

        let observeTarget: obj =
            match containerRef.Current with
            | Some el -> box el
            | None -> box slotEl

        let stopResize =
            BrowserInterop3D.observeResize observeTarget (fun () ->
                if not disposed then
                    syncViewport ()

                    if dirty then
                        scheduleFrame ())

        mounted <- true
        syncViewport ()

        if config.AutoFocus && not autoFocusedOnMount then
            autoFocusedOnMount <- true
            tryAutoFocusContainer false

        dirty <- true
        scheduleFrame ()

        fun () ->
            stopResize ()
            cleanupResources ()

            if obj.ReferenceEquals (canvasNode.parentNode, slotEl :> Node) then
                slotEl.removeChild canvasNode |> ignore

            match prevStyle with
            | null -> canvas.removeAttribute "style"
            | s -> canvas.setAttribute ("style", s)

            match prevWidthAttr with
            | null -> canvas.removeAttribute "width"
            | w -> canvas.setAttribute ("width", w)

            match prevHeightAttr with
            | null -> canvas.removeAttribute "height"
            | h -> canvas.setAttribute ("height", h)

            if not (isNull prevParent) then
                if not (isNull prevNext) && obj.ReferenceEquals (prevNext.parentNode, prevParent) then
                    prevParent.insertBefore (canvasNode, prevNext) |> ignore
                else
                    prevParent.appendChild canvasNode |> ignore

    member _.OnPointerDown(ev: PointerEvent) =
        if not disposed then
            let isInteractive = BrowserInterop3D.isInteractiveFormTarget ev.target

            if inputKernel.OnPointerDown (ev.pointerId, ev.button, ev.clientX, ev.clientY, ev.shiftKey, isInteractive) then
                hoverCoalescer.AdvanceEpoch ()
                tryAutoFocusContainer true

                match containerRef.Current with
                | Some el -> BrowserInterop3D.trySetPointerCapture el ev.pointerId
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
                match hoverCoalescer.RequestHover (ev.clientX, ev.clientY) with
                | ValueSome pickEpoch -> dispatchAsyncHover ev.clientX ev.clientY pickEpoch
                | ValueNone -> ()
            | PointerMoveResult3D.NoOp -> ()

    member _.OnPointerUp(ev: PointerEvent) =
        if not disposed then
            let up = inputKernel.OnPointerUp (ev.pointerId, ev.button, ev.clientX, ev.clientY)

            if up.Handled then
                match containerRef.Current with
                | Some el -> BrowserInterop3D.tryReleasePointerCapture el ev.pointerId
                | None -> ()

                if up.WasClick && (config.ShowHoverSpikes || config.OnClickHit.IsSome) then
                    dispatchClickPick up.ClientX up.ClientY hoverCoalescer.CurrentEpoch

    member _.OnPointerCancel(ev: PointerEvent) =
        if not disposed && inputKernel.OnPointerCancel ev.pointerId then
            match containerRef.Current with
            | Some el -> BrowserInterop3D.tryReleasePointerCapture el ev.pointerId
            | None -> ()

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
            && not (BrowserInterop3D.isInteractiveFormTarget ev.target)
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

/// <summary>Browser <c>Ranvier.View.Dsl</c> host binding a WebGPU/WebGL2 <c>WgpuContext</c> canvas and pointer-transparent SVG overlays.</summary>
[<AbstractClass; Sealed>]
type PlotView3D private () =
    static let plotComponent: Component<PlotViewProps3D> =
        Component.template
            (fun _ (props: PlotViewProps3D) ->
                PlotViewInstance3D (props.Canvas, props.Context, props.Config, props.ReadScene))
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
                    div<PlotViewInstance3D>(
                        [|
                            div.style "position:absolute;inset:0;"
                            div.lend (fun (inst: PlotViewInstance3D) el -> inst.AttachCanvasHost el)
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
    /// Creates a browser <c>WgpuContext</c> on <c>canvas</c> with <c>ColorShader.wgsl ()</c> and <c>ColorShader.glsl ()</c>
    /// wired automatically when <c>enableColorShader</c> is true (default).
    /// </summary>
    static member createContextAsync
        (
            canvas: HTMLCanvasElement,
            ?backend: WgpuBrowserBackend,
            ?enableColorShader: bool,
            ?width: int,
            ?height: int
        ) : Task<WgpuContext> =
        if isNull (box canvas) then
            nullArg (nameof canvas)

        let wireLut = defaultArg enableColorShader true
        let customWgsl = if wireLut then Some (ColorShader.wgsl ()) else None
        let customGlsl = if wireLut then Some (ColorShader.glsl ()) else None
        let chosenBackend = defaultArg backend WgpuBrowserBackend.Auto

        WgpuContext.CreateAsync (
            canvas,
            ?width = width,
            ?height = height,
            ?customWgsl = customWgsl,
            ?customGlsl = customGlsl,
            backend = chosenBackend
        )
        |> BrowserInterop3D.promiseToTask

    /// <summary>
    /// Creates a browser <c>Ranvier.View.Dsl</c> interactive 3D plot view adopting <c>canvas</c> and <c>context</c>,
    /// rendering <c>readScene ()</c> on dirty <c>requestAnimationFrame</c> ticks alongside passive SVG axis and legend overlays.
    /// </summary>
    static member create
        (canvas: HTMLCanvasElement)
        (context: WgpuContext)
        (config: PlotViewConfig3D)
        (readScene: unit -> PlotPass3D)
        : Program.View =
        if isNull (box canvas) then nullArg (nameof canvas)
        if isNull (box context) then nullArg (nameof context)
        if isNull (box config.Camera) then nullArg "config.Camera"
        if isNull (box readScene) then nullArg (nameof readScene)

        if config.Width <= 0 then
            raise (ArgumentOutOfRangeException ("config.Width", "Viewport width must be positive."))

        if config.Height <= 0 then
            raise (ArgumentOutOfRangeException ("config.Height", "Viewport height must be positive."))

        Component.render
            plotComponent
            {
                Canvas = canvas
                Context = context
                Config = config
                ReadScene = readScene
            }

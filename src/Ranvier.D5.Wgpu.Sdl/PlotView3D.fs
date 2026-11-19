namespace Ranvier.D5.Wgpu.Sdl

open System
open System.Numerics
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
    }

/// <summary>Viewport size, editable camera, and optional GPU pick callbacks for <c>PlotView3D</c>.</summary>
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
    }

type private PlotViewProps3D =
    {
        Window: Window
        Context: WgpuContext
        Config: PlotViewConfig3D
        ReadScene: unit -> PlotPass3D
    }

module private PlotView3DKernel =
    let syncMergedPrefix<'T when 'T: not struct>
        (prefix1: 'T)
        (prefix2Opt: 'T voption)
        (extra: ReadOnlyMemory<'T>)
        (cachedExtra: byref<'T array>)
        (merged: byref<'T array>)
        =
        let span = extra.Span
        let mutable same = span.Length = cachedExtra.Length

        if same then
            let mutable i = 0

            while same && i < span.Length do
                if not (Object.ReferenceEquals (span[i], cachedExtra[i])) then
                    same <- false

                i <- i + 1

        if not same then
            let prefixCount = if prefix2Opt.IsSome then 2 else 1
            let nextExtra = Array.zeroCreate<'T> span.Length
            let nextMerged = Array.zeroCreate<'T> (prefixCount + span.Length)
            nextMerged[0] <- prefix1

            match prefix2Opt with
            | ValueSome prefix2 -> nextMerged[1] <- prefix2
            | ValueNone -> ()

            for i = 0 to span.Length - 1 do
                let item = span[i]
                nextExtra[i] <- item
                nextMerged[prefixCount + i] <- item

            cachedExtra <- nextExtra
            merged <- nextMerged

[<Sealed>]
type private PlotViewInstance3D
    (
        window: Window,
        context: WgpuContext,
        config: PlotViewConfig3D,
        readScene: unit -> PlotPass3D
    ) =
    let containerRef: NodeRef<HTMLDivElement> = NodeRef.create ()
    let emptyOverlay = Scene.create (float config.Width) (float config.Height) "PlotView3D" []
    let overlaySig = Api.createSignal emptyOverlay

    let boxMesh = context.UploadMesh (Primitives.unitBox ())
    let panelBatch = context.CreateBatch (boxMesh, BlendMode.AlphaBlend, false)
    let axisLineBatch = context.CreateLineBatch ()
    let spikeLineBatch = context.CreateLineBatch ()

    let mutable disposed = false
    let mutable dirty = true
    let mutable rafId: int option = None
    let mutable surfaceOpt: WgpuSdlSurface option = None
    let mutable latestCam: Camera3D option = None
    let mutable latestPass: PlotPass3D option = None
    let mutable latestFrame: AxisFrame3D option = None
    let mutable hasHoverSpikes = false
    let mutable lastHoverHit: PickHit voption = ValueNone

    let mutable dragging = false
    let mutable dragMoved = false
    let mutable dragPan = false
    let mutable activePointerId = -1
    let mutable lastClientX = 0.0
    let mutable lastClientY = 0.0
    let mutable downClientX = 0.0
    let mutable downClientY = 0.0

    let mutable cachedExtraBatches: InstanceBatch array = [||]
    let mutable mergedBatches: InstanceBatch array = [| panelBatch |]
    let mutable cachedExtraLines: LineBatch array = [||]
    let mutable mergedLines: LineBatch array = [| axisLineBatch; spikeLineBatch |]
    let mutable hasCachedAxis = false
    let mutable cachedAxisSpec = Unchecked.defaultof<Axis3DSpec>
    let mutable cachedGpuCam = Unchecked.defaultof<Camera3D>
    let mutable cachedWidth = -1
    let mutable cachedHeight = -1
    let mutable cachedFrame = Unchecked.defaultof<AxisFrame3D>
    let mutable cachedBaseOverlay = Unchecked.defaultof<Scene>
    let mutable prevHadExtraMarks = false

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
            PlotView3DKernel.syncMergedPrefix panelBatch ValueNone pass.ExtraBatches &cachedExtraBatches &mergedBatches
            PlotView3DKernel.syncMergedPrefix axisLineBatch (ValueSome spikeLineBatch) pass.ExtraLines &cachedExtraLines &mergedLines

            withSec
            |> ScenePass.withBatches mergedBatches
            |> ScenePass.withLines mergedLines
        else
            withSec |> ScenePass.withBatchMemory pass.ExtraBatches

    let renderNow () =
        if not disposed then
            match surfaceOpt, latestCam, latestPass, latestFrame with
            | Some surface, Some cam, Some pass, Some _ when not surface.IsDisposed && not context.IsDisposed ->
                dirty <- false
                let scenePass = buildScenePass cam pass true
                WgpuSdlSurface.render surface scenePass |> ignore
            | _ -> ()

    let rec scheduleFrame () =
        if not disposed && dirty && rafId.IsNone && surfaceOpt.IsSome then
            let id =
                window.Host.RequestAnimationFrame (fun _ ->
                    rafId <- None

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
        match hit, latestPass, latestFrame with
        | ValueSome h, Some pass, Some frame when h.PickId <> 0u ->
            let tc = pass.AxisSpec.Theme.TickColor
            let spikeColor = RgbaColor (tc.R, tc.G, tc.B, 0.95f)
            let spikes = Probe3D.hoverSpikes pass.AxisSpec.Scale frame spikeColor h.WorldPosition
            spikeLineBatch.UpdateVertices spikes.Span
            hasHoverSpikes <- true
            dirty <- true
            scheduleFrame ()
        | _ ->
            if hasHoverSpikes then
                spikeLineBatch.Resize 0
                hasHoverSpikes <- false
                dirty <- true
                scheduleFrame ()

    let viewportCssSize () =
        match containerRef.Current with
        | Some el ->
            let rect = el.getBoundingClientRect ()
            let w = if rect.width > 0.0 then rect.width else float config.Width
            let h = if rect.height > 0.0 then rect.height else float config.Height
            struct (rect.x, rect.y, w, h)
        | None -> struct (0.0, 0.0, float config.Width, float config.Height)

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
                WgpuContext.pick px py context pickPass
            | _ -> ValueNone

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
                let plotCam = config.Camera.Value
                let pass = readScene ()
                let gpuCam = PlotCamera3D.toCamera3D pass.AxisSpec.Scale plotCam

                let axisChanged =
                    not (
                        hasCachedAxis
                        && Object.ReferenceEquals (pass.AxisSpec, cachedAxisSpec)
                        && gpuCam = cachedGpuCam
                        && config.Width = cachedWidth
                        && config.Height = cachedHeight
                    )

                if axisChanged then
                    let nextFrame = Axis3D.build pass.AxisSpec config.Width config.Height gpuCam
                    panelBatch.UpdateInstances ((Axis3D.panelInstances nextFrame).Span)
                    axisLineBatch.UpdateVertices ((Axis3D.lineVertices nextFrame).Span)
                    let nextBaseOverlay = Axis3D.overlayScene (float config.Width) (float config.Height) nextFrame
                    hasCachedAxis <- true
                    cachedAxisSpec <- pass.AxisSpec
                    cachedGpuCam <- gpuCam
                    cachedWidth <- config.Width
                    cachedHeight <- config.Height
                    cachedFrame <- nextFrame
                    cachedBaseOverlay <- nextBaseOverlay

                let hasExtraMarks = not (List.isEmpty pass.ExtraOverlayMarks)

                if axisChanged || hasExtraMarks || prevHadExtraMarks then
                    let overlay =
                        if not hasExtraMarks then
                            cachedBaseOverlay
                        else
                            Scene.create
                                (float config.Width)
                                (float config.Height)
                                (Scene.title cachedBaseOverlay)
                                (Scene.marks cachedBaseOverlay @ pass.ExtraOverlayMarks)

                    overlaySig.Value <- overlay
                    prevHadExtraMarks <- hasExtraMarks

                latestCam <- Some gpuCam
                latestPass <- Some pass
                latestFrame <- Some cachedFrame
                dirty <- true
                scheduleFrame ())

    member _.ContainerRef = containerRef
    member _.ReadOverlay = fun () -> overlaySig.Value
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
                        dirty <- true
                        scheduleFrame ()

                    { new IDisposable with
                        member _.Dispose() = cleanupResources () })
                el

        fun () -> lease.Dispose ()

    member _.OnPointerDown(ev: PointerEvent) =
        if not disposed && not dragging then
            dragging <- true
            dragMoved <- false
            dragPan <- ev.button = 1s || ev.button = 2s || ev.shiftKey
            activePointerId <- ev.pointerId
            lastClientX <- ev.clientX
            lastClientY <- ev.clientY
            downClientX <- ev.clientX
            downClientY <- ev.clientY

            match containerRef.Current with
            | Some el ->
                try
                    el.setPointerCapture ev.pointerId
                with _ ->
                    ()
            | None -> ()

    member _.OnPointerMove(ev: PointerEvent) =
        if not disposed then
            if dragging && ev.pointerId = activePointerId then
                let dx = float32 (ev.clientX - lastClientX)
                let dy = float32 (ev.clientY - lastClientY)
                lastClientX <- ev.clientX
                lastClientY <- ev.clientY

                if abs (ev.clientX - downClientX) > 2.0 || abs (ev.clientY - downClientY) > 2.0 then
                    dragMoved <- true

                if dx <> 0.0f || dy <> 0.0f then
                    if dragPan then
                        let struct (_, _, rw, rh) = viewportCssSize ()
                        let fitR =
                            match latestPass with
                            | Some p -> Scale3D.fitRadius p.AxisSpec.Scale
                            | None -> 1.0f

                        let ndx = dx / float32 (max 1.0 rw)
                        let ndy = dy / float32 (max 1.0 rh)
                        config.Camera.Value <- PlotCamera3D.panBy fitR AxisLock3D.Free ndx ndy config.Camera.Value
                    else
                        config.Camera.Value <- PlotCamera3D.orbitBy dx dy config.Camera.Value
            elif not dragging then
                let hit = pickAtClient ev.clientX ev.clientY

                if hit.IsSome || lastHoverHit.IsSome then
                    lastHoverHit <- hit
                    updateSpikes hit
                    config.OnHoverHit |> Option.iter (fun cb -> cb hit)

    member _.OnPointerUp(ev: PointerEvent) =
        if not disposed && dragging && ev.pointerId = activePointerId then
            releaseCapture ev.pointerId
            let wasClick = not dragMoved && ev.button = 0s
            dragging <- false
            dragPan <- false
            activePointerId <- -1

            if wasClick then
                let hit = pickAtClient ev.clientX ev.clientY
                lastHoverHit <- hit
                updateSpikes hit
                config.OnClickHit |> Option.iter (fun cb -> cb hit)

    member _.OnPointerCancel(ev: PointerEvent) =
        if not disposed && dragging && ev.pointerId = activePointerId then
            releaseCapture ev.pointerId
            dragging <- false
            dragMoved <- false
            dragPan <- false
            activePointerId <- -1

    member _.OnWheel(ev: WheelEvent) =
        if not disposed then
            ev.preventDefault ()

            if abs ev.deltaY > 1e-6 then
                let rawFactor = MathF.Exp (-float32 ev.deltaY * 0.0015f)
                let factor = Math.Clamp (rawFactor, 0.25f, 4.0f)
                config.Camera.Value <- PlotCamera3D.zoomBy factor config.Camera.Value

    member _.OnKeyDown(ev: KeyboardEvent) =
        if not disposed then
            let cam = config.Camera.Value

            match ev.key with
            | "r" | "R" | "Home" ->
                ev.preventDefault ()
                config.Camera.Reset ()
            | "p" | "P" ->
                ev.preventDefault ()
                let nextP = if cam.Perspectiveness > 0.5f then 0.0f else 1.0f
                config.Camera.Value <- { cam with Perspectiveness = nextP }
            | "m" | "M" ->
                ev.preventDefault ()

                let nextMode =
                    match cam.Mode with
                    | OrbitMode3D.Turntable -> OrbitMode3D.Arcball
                    | OrbitMode3D.Arcball -> OrbitMode3D.Turntable

                config.Camera.Value <- PlotCamera3D.withMode nextMode cam
            | "ArrowLeft" ->
                ev.preventDefault ()
                config.Camera.Value <- PlotCamera3D.orbitBy -12.0f 0.0f cam
            | "ArrowRight" ->
                ev.preventDefault ()
                config.Camera.Value <- PlotCamera3D.orbitBy 12.0f 0.0f cam
            | "ArrowUp" ->
                ev.preventDefault ()
                config.Camera.Value <- PlotCamera3D.orbitBy 0.0f -12.0f cam
            | "ArrowDown" ->
                ev.preventDefault ()
                config.Camera.Value <- PlotCamera3D.orbitBy 0.0f 12.0f cam
            | "+" | "=" ->
                ev.preventDefault ()
                config.Camera.Value <- PlotCamera3D.zoomBy 1.15f cam
            | "-" | "_" ->
                ev.preventDefault ()
                config.Camera.Value <- PlotCamera3D.zoomBy (1.0f / 1.15f) cam
            | _ -> ()

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

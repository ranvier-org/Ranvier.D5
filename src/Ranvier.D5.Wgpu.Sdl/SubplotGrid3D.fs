namespace Ranvier.D5.Wgpu.Sdl

open System
open System.Collections.Generic
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

/// <summary>Viewport rectangle, independent editable camera, reactive scene producer, and pick callbacks for one <c>SubplotGrid3D</c> cell.</summary>
[<NoEquality; NoComparison>]
type SubplotCell3D =
    {
        /// <summary>Cell viewport rectangle in logical/CSS grid pixels.</summary>
        Viewport: ViewportRect
        /// <summary>Reactive editable 3D camera controlled independently by pointer and wheel interactions inside this cell.</summary>
        Camera: Editable<PlotCamera3D>
        /// <summary>Reactive scene pass producer evaluated when tracking cell data or camera state.</summary>
        ReadPass: unit -> PlotPass3D
        /// <summary>Optional callback invoked with resolved GPU hover picks inside this cell.</summary>
        OnHoverHit: (PickHit voption -> unit) option
        /// <summary>Optional callback invoked with resolved GPU primary-click picks inside this cell.</summary>
        OnClickHit: (PickHit voption -> unit) option
        /// <summary>When true, renders axis back-wall hairline hover spikes inside this cell.</summary>
        ShowHoverSpikes: bool
    }

/// <summary>Constructors and pipeable modifiers for <c>SubplotCell3D</c>.</summary>
[<RequireQualifiedAccess>]
module SubplotCell3D =
    /// <summary>Creates a <c>SubplotCell3D</c> with <c>ShowHoverSpikes = true</c> and no pick callbacks.</summary>
    let create (viewport: ViewportRect) (camera: Editable<PlotCamera3D>) (readPass: unit -> PlotPass3D) : SubplotCell3D =
        if isNull (box camera) then
            nullArg (nameof camera)

        if isNull (box readPass) then
            nullArg (nameof readPass)

        {
            Viewport = viewport
            Camera = camera
            ReadPass = readPass
            OnHoverHit = None
            OnClickHit = None
            ShowHoverSpikes = true
        }

    /// <summary>Sets the hover GPU pick callback on <c>cell</c>.</summary>
    let withOnHoverHit (handler: PickHit voption -> unit) (cell: SubplotCell3D) : SubplotCell3D =
        { cell with OnHoverHit = Some handler }

    /// <summary>Sets the primary-click GPU pick callback on <c>cell</c>.</summary>
    let withOnClickHit (handler: PickHit voption -> unit) (cell: SubplotCell3D) : SubplotCell3D =
        { cell with OnClickHit = Some handler }

    /// <summary>Enables or disables back-wall hover spikes inside <c>cell</c>.</summary>
    let withHoverSpikes (enabled: bool) (cell: SubplotCell3D) : SubplotCell3D =
        { cell with ShowHoverSpikes = enabled }

[<Sealed>]
type private SubplotCellRuntime
    (
        index: int,
        cell: SubplotCell3D,
        context: WgpuContext,
        boxMesh: MeshHandle,
        requestSchedule: unit -> unit,
        advanceEpoch: unit -> unit,
        readCssSize: unit -> struct (float * float),
        totalWidth: int,
        totalHeight: int
    ) =
    let panelBatch = context.CreateBatch (boxMesh, BlendMode.AlphaBlend, false, drawBeforeLines = true)
    let axisLineBatch = context.CreateLineBatch ()
    let spikeLineBatch = context.CreateLineBatch ()
    let axisMemo = Axis3DMemo (panelBatch, axisLineBatch)
    let batchPrefixCache = ScenePrefixCache (panelBatch, ValueNone)
    let linePrefixCache = ScenePrefixCache (axisLineBatch, ValueSome spikeLineBatch)
    let emptyOverlay = Scene.create (float cell.Viewport.Width) (float cell.Viewport.Height) $"SubplotCell3D.{index}" []
    let overlaySig = Api.createSignal emptyOverlay
    let emptyExtraOverlay = Scene.create (float cell.Viewport.Width) (float cell.Viewport.Height) $"SubplotCell3D.{index}.ExtraOverlay" []
    let extraOverlaySig = Api.createSignal emptyExtraOverlay

    let mutable disposed = false
    let mutable latestCam: Camera3D option = None
    let mutable latestPass: PlotPass3D option = None
    let mutable hasHoverSpikes = false
    let mutable lastHoverHit: PickHit voption = ValueNone
    let mutable prevHadExtraMarks = false
    let mutable prevExtraScene: Scene voption = ValueNone

    let liveCssViewport () : ViewportRect =
        let struct (cssW, cssH) = readCssSize ()
        let sx = cssW / float (max 1 totalWidth)
        let sy = cssH / float (max 1 totalHeight)
        let x0 = max 0 (int (Math.Round (float cell.Viewport.X * sx)))
        let y0 = max 0 (int (Math.Round (float cell.Viewport.Y * sy)))
        let x1 = int (Math.Round (float (cell.Viewport.X + cell.Viewport.Width) * sx))
        let y1 = int (Math.Round (float (cell.Viewport.Y + cell.Viewport.Height) * sy))
        ViewportRect.create x0 y0 (max 1 (x1 - x0)) (max 1 (y1 - y0))

    let applyOverlayMarks (logicalW: int) (logicalH: int) (baseOverlay: Scene) (extraMarks: Mark list) =
        let hasExtra = not (List.isEmpty extraMarks)
        overlaySig.Value <- Plot3D.composeOverlay logicalW logicalH baseOverlay extraMarks
        prevHadExtraMarks <- hasExtra

    do
        Api.createEffect (fun () ->
            if not disposed && not context.IsDisposed then
                let plotCam = cell.Camera.Value
                let pass = cell.ReadPass ()
                let gpuCam = PlotCamera3D.toCamera3D pass.AxisSpec.Scale plotCam
                let cssVp = liveCssViewport ()

                let hadAxisBefore = axisMemo.HasCached
                let update = axisMemo.Update (pass.AxisSpec, cssVp.Width, cssVp.Height, gpuCam)

                if hadAxisBefore && update.AxisSpecChanged then
                    advanceEpoch ()
                    lastHoverHit <- ValueNone

                    if hasHoverSpikes then
                        spikeLineBatch.Resize 0
                        hasHoverSpikes <- false

                if update.Rebuilt || prevHadExtraMarks || not (List.isEmpty pass.ExtraOverlayMarks) then
                    applyOverlayMarks cssVp.Width cssVp.Height update.BaseOverlay pass.ExtraOverlayMarks

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
                requestSchedule ())

    member _.Index = index
    member _.Cell = cell
    member _.ReadOverlay = fun () -> overlaySig.Value
    member _.ReadExtraOverlay = fun () -> extraOverlaySig.Value
    member _.LastHoverHit = lastHoverHit
    member _.AllowHoverPick = cell.ShowHoverSpikes || cell.OnHoverHit.IsSome

    member _.LiveCssViewport() = liveCssViewport ()

    member _.LiveGpuViewport(dprX: float, dprY: float) : ViewportRect =
        let cssVp = liveCssViewport ()
        let gx0 = max 0 (int (Math.Round (float cssVp.X * dprX)))
        let gy0 = max 0 (int (Math.Round (float cssVp.Y * dprY)))
        let gx1 = int (Math.Round (float (cssVp.X + cssVp.Width) * dprX))
        let gy1 = int (Math.Round (float (cssVp.Y + cssVp.Height) * dprY))
        ViewportRect.create gx0 gy0 (max 1 (gx1 - gx0)) (max 1 (gy1 - gy0))

    member _.OverlayStyle: string =
        let vp = cell.Viewport
        $"position:absolute;left:{vp.X}px;top:{vp.Y}px;width:{vp.Width}px;height:{vp.Height}px;pointer-events:none;overflow:hidden;"

    member _.SyncViewport(dprX: float, dprY: float) : bool =
        ignore (dprX, dprY)
        let cssVp = liveCssViewport ()

        match axisMemo.CurrentSpec, latestCam, latestPass with
        | ValueSome spec, Some gpuCam, Some pass ->
            let update = axisMemo.Update (spec, cssVp.Width, cssVp.Height, gpuCam)

            if update.Rebuilt then
                applyOverlayMarks cssVp.Width cssVp.Height update.BaseOverlay pass.ExtraOverlayMarks
                true
            else
                false
        | _ -> false

    member _.FitRadius: float32 =
        match latestPass with
        | Some p -> Scale3D.fitRadius p.AxisSpec.Scale
        | None -> 1.0f

    member _.BuildScenePass(viewport: ViewportRect, includeAxis: bool) : ScenePass voption =
        match latestCam, latestPass with
        | Some cam, Some pass ->
            let basePass =
                ScenePass.create cam
                |> ScenePass.withClear pass.ClearColor
                |> ScenePass.withViewport viewport
                |> ScenePass.withDiscMemory pass.ExtraDiscs

            let withLight =
                match pass.Lighting with
                | ValueSome light -> ScenePass.withLighting light basePass
                | ValueNone -> basePass

            let withSec =
                match pass.SectionPlane with
                | ValueSome sec -> ScenePass.withSection sec withLight
                | ValueNone -> withLight

            let finalPass =
                if includeAxis then
                    withSec
                    |> ScenePass.withBatchMemory (batchPrefixCache.Sync pass.ExtraBatches)
                    |> ScenePass.withLineMemory (linePrefixCache.Sync pass.ExtraLines)
                else
                    withSec |> ScenePass.withBatchMemory pass.ExtraBatches

            ValueSome finalPass
        | _ -> ValueNone

    member _.ApplyHoverHit(hit: PickHit voption) : bool =
        let mutable spikesDirty = false

        if cell.ShowHoverSpikes then
            match hit, latestPass, axisMemo.CurrentFrame with
            | ValueSome h, Some pass, ValueSome frame when h.PickId <> 0u ->
                let tc = pass.AxisSpec.Theme.TickColor
                let spikeColor = RgbaColor (tc.R, tc.G, tc.B, 0.95f)
                let spikes = Probe3D.hoverSpikes pass.AxisSpec.Scale frame spikeColor h.WorldPosition
                spikeLineBatch.UpdateVertices spikes.Span
                hasHoverSpikes <- true
                spikesDirty <- true
            | _ ->
                if hasHoverSpikes then
                    spikeLineBatch.Resize 0
                    hasHoverSpikes <- false
                    spikesDirty <- true
        elif hasHoverSpikes then
            spikeLineBatch.Resize 0
            hasHoverSpikes <- false
            spikesDirty <- true

        let hadDiff = hit.IsSome || lastHoverHit.IsSome
        lastHoverHit <- hit

        if hadDiff then
            cell.OnHoverHit |> Option.iter (fun cb -> cb hit)

        spikesDirty

    member _.ClearHover() : bool =
        if lastHoverHit.IsSome || hasHoverSpikes then
            let spikesCleared =
                if hasHoverSpikes then
                    spikeLineBatch.Resize 0
                    hasHoverSpikes <- false
                    true
                else
                    false

            if lastHoverHit.IsSome then
                lastHoverHit <- ValueNone
                cell.OnHoverHit |> Option.iter (fun cb -> cb ValueNone)

            spikesCleared
        else
            false

    member _.Dispose() : unit =
        if not disposed then
            disposed <- true
            spikeLineBatch.Dispose ()
            axisLineBatch.Dispose ()
            panelBatch.Dispose ()

    interface IDisposable with
        member this.Dispose() = this.Dispose ()

type private SubplotGridProps =
    {
        Window: Window
        Context: WgpuContext
        TotalWidth: int
        TotalHeight: int
        Cells: SubplotCell3D array
    }

[<Sealed>]
type private SubplotGridInstance
    (
        window: Window,
        context: WgpuContext,
        totalWidth: int,
        totalHeight: int,
        cells: SubplotCell3D array
    ) =
    let containerRef: NodeRef<HTMLDivElement> = NodeRef.create ()
    let canvasRef: NodeRef<HTMLCanvasElement> = NodeRef.create ()
    let boxMesh = context.UploadMesh (Primitives.unitBox ())
    let inputKernel = PlotInputKernel3D ()
    let hoverCoalescer = HoverPickCoalescer ()

    let mutable disposed = false
    let mutable dirty = true
    let mutable rafId: int option = None
    let mutable surfaceOpt: WgpuSdlSurface option = None
    let mutable lastCanvasW = totalWidth
    let mutable lastCanvasH = totalHeight
    let mutable activeDragCell: int voption = ValueNone
    let mutable focusedCell = 0
    let mutable lastHoverCell: int voption = ValueNone
    let mutable cellStates: SubplotCellRuntime array = Array.empty

    let viewportCssSize () : struct (float * float * float * float) =
        match containerRef.Current with
        | Some el ->
            let rect = el.getBoundingClientRect ()
            let w = if rect.width > 0.0 then rect.width else float totalWidth
            let h = if rect.height > 0.0 then rect.height else float totalHeight
            struct (rect.x, rect.y, w, h)
        | None -> struct (0.0, 0.0, float totalWidth, float totalHeight)

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
        if not disposed then
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

    let findCellAtClient (clientX: float) (clientY: float) : int voption =
        let struct (rx, ry, _, _) = viewportCssSize ()
        let lx = int (Math.Floor (clientX - rx))
        let ly = int (Math.Floor (clientY - ry))
        let mutable i = cellStates.Length - 1
        let mutable found = ValueNone

        while i >= 0 && found.IsNone do
            let vp = cellStates[i].LiveCssViewport ()

            if vp.Contains (lx, ly) then
                found <- ValueSome i
            else
                i <- i - 1

        found

    let buildPasses (includeAxis: bool) : ScenePass array =
        let struct (dprX, dprY) = dprScale ()
        let passes = ResizeArray<ScenePass>(cellStates.Length)

        for i = 0 to cellStates.Length - 1 do
            let cs = cellStates[i]
            let gpuVp = cs.LiveGpuViewport (dprX, dprY)

            match cs.BuildScenePass (gpuVp, includeAxis) with
            | ValueSome p -> passes.Add p
            | ValueNone -> ()

        passes.ToArray ()

    let syncViewport () =
        if not disposed && not context.IsDisposed then
            let struct (_, _, cssW, cssH) = viewportCssSize ()
            let struct (dprX, dprY) = dprScale ()
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

            for i = 0 to cellStates.Length - 1 do
                if cellStates[i].SyncViewport (dprX, dprY) then
                    dirty <- true

    let renderNow () =
        if not disposed then
            match surfaceOpt with
            | Some surface when not surface.IsDisposed && not context.IsDisposed ->
                dirty <- false
                let passes = buildPasses true
                surface.RenderPasses (ReadOnlySpan<ScenePass>(passes), RgbaColor.darkBackdrop) |> ignore
            | _ -> ()

    let pickAtClient (clientX: float) (clientY: float) : PickHit voption =
        if disposed || context.IsDisposed then
            ValueNone
        else
            let passes = buildPasses false

            if passes.Length = 0 then
                ValueNone
            else
                let struct (rx, ry, rw, rh) = viewportCssSize ()
                let nx = Math.Clamp ((clientX - rx) / rw, 0.0, 1.0)
                let ny = Math.Clamp ((clientY - ry) / rh, 0.0, 1.0)
                let px = Math.Clamp (int (Math.Round (nx * float context.Width)), 0, max 0 (context.Width - 1))
                let py = Math.Clamp (int (Math.Round (ny * float context.Height)), 0, max 0 (context.Height - 1))
                Interlocked.Increment (&PlotView3DStats.PickCalls) |> ignore
                context.PickPasses (px, py, ReadOnlySpan<ScenePass>(passes))

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
                                let cellIdxOpt = findCellAtClient hx hy

                                if lastHoverCell <> cellIdxOpt then
                                    match lastHoverCell with
                                    | ValueSome prevIdx when prevIdx < cellStates.Length ->
                                        if cellStates[prevIdx].ClearHover () then
                                            dirty <- true
                                    | _ -> ()

                                    lastHoverCell <- cellIdxOpt

                                match cellIdxOpt with
                                | ValueSome idx when cellStates[idx].AllowHoverPick ->
                                    let hit = pickAtClient hx hy

                                    if not disposed && pickEpoch = hoverCoalescer.CurrentEpoch then
                                        if cellStates[idx].ApplyHoverHit hit then
                                            dirty <- true
                                | _ -> ()
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

            for cs in cellStates do
                cs.Dispose ()

            boxMesh.Dispose ()

    do
        cellStates <-
            cells
            |> Array.mapi (fun idx c ->
                new SubplotCellRuntime (
                    idx,
                    c,
                    context,
                    boxMesh,
                    (fun () ->
                        dirty <- true
                        scheduleFrame ()),
                    hoverCoalescer.AdvanceEpoch,
                    (fun () ->
                        let struct (_, _, w, h) = viewportCssSize ()
                        struct (w, h)),
                    totalWidth,
                    totalHeight
                ))

        Api.onCleanup cleanupResources

        Api.createEffect (fun () ->
            if not disposed && not context.IsDisposed && window.Focused.Value then
                tryAutoFocusContainer false)

    member _.ContainerRef = containerRef
    member _.CanvasRef = canvasRef
    member _.CellRuntimes = cellStates
    member _.ContainerStyle =
        $"position:relative;width:{totalWidth}px;height:{totalHeight}px;overflow:hidden;outline:none;user-select:none;"
    member _.CanvasWidthAttr = string totalWidth
    member _.CanvasHeightAttr = string totalHeight

    member _.AttachCanvas(el: HTMLCanvasElement) : unit -> unit =
        let lease =
            WgpuSdlSurface.lend
                window
                context
                (fun surface ->
                    if not disposed then
                        surfaceOpt <- Some surface
                        syncViewport ()
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

            if not isInteractive && inputKernel.OnPointerDown (ev.pointerId, ev.button, ev.clientX, ev.clientY, ev.shiftKey, false) then
                activeDragCell <- findCellAtClient ev.clientX ev.clientY

                match activeDragCell with
                | ValueSome idx -> focusedCell <- idx
                | ValueNone -> ()

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
            match activeDragCell with
            | ValueSome idx when inputKernel.IsDragging && idx < cellStates.Length ->
                let cs = cellStates[idx]
                let cssVp = cs.LiveCssViewport ()

                match
                    inputKernel.OnPointerMove (
                        ev.pointerId,
                        ev.clientX,
                        ev.clientY,
                        float cssVp.Width,
                        float cssVp.Height,
                        cs.FitRadius,
                        cs.Cell.Camera.Value,
                        false
                    )
                with
                | PointerMoveResult3D.CameraUpdated nextCam ->
                    cs.Cell.Camera.Value <- nextCam
                | _ -> ()
            | _ ->
                if not inputKernel.IsDragging then
                    let allowAnyHover = cellStates |> Array.exists (fun cs -> cs.AllowHoverPick)

                    if allowAnyHover then
                        hoverCoalescer.QueueFrameHover (ev.clientX, ev.clientY)
                        scheduleFrame ()

    member _.OnPointerUp(ev: PointerEvent) =
        if not disposed then
            let up = inputKernel.OnPointerUp (ev.pointerId, ev.button, ev.clientX, ev.clientY)

            if up.Handled then
                let targetCell = activeDragCell
                activeDragCell <- ValueNone
                releaseCapture ev.pointerId

                if up.WasClick then
                    let resolvedCell =
                        match targetCell with
                        | ValueSome idx -> ValueSome idx
                        | ValueNone -> findCellAtClient up.ClientX up.ClientY

                    match resolvedCell with
                    | ValueSome idx when idx < cellStates.Length ->
                        let cs = cellStates[idx]

                        if cs.Cell.ShowHoverSpikes || cs.Cell.OnClickHit.IsSome then
                            let hit = pickAtClient up.ClientX up.ClientY

                            if cs.ApplyHoverHit hit then
                                dirty <- true
                                scheduleFrame ()

                            cs.Cell.OnClickHit |> Option.iter (fun cb -> cb hit)
                    | _ -> ()

    member _.OnPointerCancel(ev: PointerEvent) =
        if not disposed && inputKernel.OnPointerCancel ev.pointerId then
            activeDragCell <- ValueNone
            releaseCapture ev.pointerId
            hoverCoalescer.AdvanceEpoch ()

    member _.OnWheel(ev: WheelEvent) =
        if not disposed then
            match findCellAtClient ev.clientX ev.clientY with
            | ValueSome idx when idx < cellStates.Length ->
                ev.preventDefault ()
                let cam = cellStates[idx].Cell.Camera

                match PlotInputKernel3D.ApplyWheel (ev.deltaY, cam.Value) with
                | ValueSome nextCam -> cam.Value <- nextCam
                | ValueNone -> ()
            | _ -> ()

    member _.OnKeyDown(ev: KeyboardEvent) =
        if
            not disposed
            && not ev.defaultPrevented
            && not ev.ctrlKey
            && not ev.metaKey
            && not ev.altKey
            && not (isInteractiveFormTarget ev.target)
            && focusedCell >= 0
            && focusedCell < cellStates.Length
        then
            let cam = cellStates[focusedCell].Cell.Camera

            match PlotInputKernel3D.ApplyDefaultKey (ev.key, cam.Value) with
            | KeyCameraAction3D.Reset ->
                ev.preventDefault ()
                cam.Reset ()
            | KeyCameraAction3D.SetCamera nextCam ->
                ev.preventDefault ()
                cam.Value <- nextCam
            | KeyCameraAction3D.Ignore -> ()

/// <summary>Multi-pass synchronized 3D subplot grid sharing a single <c>WgpuContext</c> and SDL window surface with per-cell camera routing and SVG overlays.</summary>
[<RequireQualifiedAccess>]
module SubplotGrid3D =
    /// <summary>Computes a row-major grid of non-overlapping <c>ViewportRect</c> sub-rectangles inside <c>totalWidth</c> x <c>totalHeight</c> separated by <c>gapPx</c> gutters.</summary>
    let grid (rows: int) (cols: int) (gapPx: int) (totalWidth: int) (totalHeight: int) : IReadOnlyList<ViewportRect> =
        if rows <= 0 then
            raise (ArgumentOutOfRangeException (nameof rows, "Grid row count must be positive."))

        if cols <= 0 then
            raise (ArgumentOutOfRangeException (nameof cols, "Grid column count must be positive."))

        if gapPx < 0 then
            raise (ArgumentOutOfRangeException (nameof gapPx, "Grid gap must be nonnegative."))

        if totalWidth - gapPx * (cols - 1) < cols then
            raise (ArgumentOutOfRangeException (nameof totalWidth, "Total width cannot accommodate requested columns and gaps."))

        if totalHeight - gapPx * (rows - 1) < rows then
            raise (ArgumentOutOfRangeException (nameof totalHeight, "Total height cannot accommodate requested rows and gaps."))

        let availW = totalWidth - gapPx * (cols - 1)
        let availH = totalHeight - gapPx * (rows - 1)
        let baseW = availW / cols
        let remW = availW % cols
        let baseH = availH / rows
        let remH = availH % rows

        let colX = Array.zeroCreate<int> cols
        let colW = Array.zeroCreate<int> cols
        let mutable curX = 0

        for c = 0 to cols - 1 do
            let w = baseW + (if c < remW then 1 else 0)
            colX[c] <- curX
            colW[c] <- w
            curX <- curX + w + gapPx

        let rowY = Array.zeroCreate<int> rows
        let rowH = Array.zeroCreate<int> rows
        let mutable curY = 0

        for r = 0 to rows - 1 do
            let h = baseH + (if r < remH then 1 else 0)
            rowY[r] <- curY
            rowH[r] <- h
            curY <- curY + h + gapPx

        let rects = Array.zeroCreate<ViewportRect> (rows * cols)

        for r = 0 to rows - 1 do
            for c = 0 to cols - 1 do
                rects[r * cols + c] <- ViewportRect.create colX[c] rowY[r] colW[c] rowH[r]

        Array.AsReadOnly rects :> IReadOnlyList<ViewportRect>

    let private gridComponent: Component<SubplotGridProps> =
        Component.template
            (fun _ (props: SubplotGridProps) ->
                SubplotGridInstance (props.Window, props.Context, props.TotalWidth, props.TotalHeight, props.Cells))
            (div<SubplotGridInstance>(
                [|
                    div.style (fun (inst: SubplotGridInstance) -> inst.ContainerStyle)
                    div.props.tabIndex 0
                    div.ref (fun (inst: SubplotGridInstance) -> inst.ContainerRef)
                    div.onPointerdown (fun (inst: SubplotGridInstance) ev -> inst.OnPointerDown ev)
                    div.onPointermove (fun (inst: SubplotGridInstance) ev -> inst.OnPointerMove ev)
                    div.onPointerup (fun (inst: SubplotGridInstance) ev -> inst.OnPointerUp ev)
                    div.onPointercancel (fun (inst: SubplotGridInstance) ev -> inst.OnPointerCancel ev)
                    div.onWheel (fun (inst: SubplotGridInstance) ev -> inst.OnWheel ev)
                    div.onKeydown (fun (inst: SubplotGridInstance) ev -> inst.OnKeyDown ev)
                |]
            )
                .Run
                [|
                    canvas<SubplotGridInstance>(
                        [|
                            canvas.style "position:absolute;inset:0;width:100%;height:100%;display:block;"
                            canvas.ref (fun (inst: SubplotGridInstance) -> inst.CanvasRef)
                            canvas.attribute ("width", (fun (inst: SubplotGridInstance) -> Some inst.CanvasWidthAttr))
                            canvas.attribute ("height", (fun (inst: SubplotGridInstance) -> Some inst.CanvasHeightAttr))
                            canvas.lend (fun (inst: SubplotGridInstance) el -> inst.AttachCanvas el)
                        |]
                    )
                        .Run [||]
                    forEachFixed
                        (fun (cs: SubplotCellRuntime) -> cs.Index)
                        (fun (inst: SubplotGridInstance) -> inst.CellRuntimes)
                        (fragment
                            [|
                                div<SubplotCellRuntime>(
                                    [|
                                        div.attribute ("data-subplot-cell", (fun (cs: SubplotCellRuntime) -> Some (string cs.Index)))
                                        div.style (fun (cs: SubplotCellRuntime) -> cs.OverlayStyle)
                                    |]
                                )
                                    .Run [| view (fun (cs: SubplotCellRuntime) -> Component.render Renderer.passive cs.ReadOverlay) |]
                                div<SubplotCellRuntime>(
                                    [|
                                        div.attribute ("data-subplot-extra", (fun (cs: SubplotCellRuntime) -> Some (string cs.Index)))
                                        div.style (fun (cs: SubplotCellRuntime) -> cs.OverlayStyle)
                                    |]
                                )
                                    .Run [| view (fun (cs: SubplotCellRuntime) -> Component.render Renderer.passive cs.ReadExtraOverlay) |]
                            |])
                |])

    /// <summary>
    /// Creates a multi-pass 3D subplot grid view sharing one <c>WgpuContext</c> and SDL window surface,
    /// submitting all cells through a single <c>RenderPasses</c> call and routing pointer/wheel/pick events per cell viewport.
    /// </summary>
    let create
        (window: Window)
        (context: WgpuContext)
        (totalWidth: int)
        (totalHeight: int)
        (cells: seq<SubplotCell3D>)
        : Program.View =
        if isNull (box window) then nullArg (nameof window)
        if isNull (box context) then nullArg (nameof context)
        if isNull (box cells) then nullArg (nameof cells)

        if totalWidth <= 0 then
            raise (ArgumentOutOfRangeException (nameof totalWidth, "Total grid width must be positive."))

        if totalHeight <= 0 then
            raise (ArgumentOutOfRangeException (nameof totalHeight, "Total grid height must be positive."))

        let cellArr = Seq.toArray cells

        for i = 0 to cellArr.Length - 1 do
            let c = cellArr[i]

            if isNull (box c) then nullArg (nameof cells)
            if isNull (box c.Camera) then nullArg "cell.Camera"
            if isNull (box c.ReadPass) then nullArg "cell.ReadPass"

        Component.render
            gridComponent
            {
                Window = window
                Context = context
                TotalWidth = totalWidth
                TotalHeight = totalHeight
                Cells = cellArr
            }

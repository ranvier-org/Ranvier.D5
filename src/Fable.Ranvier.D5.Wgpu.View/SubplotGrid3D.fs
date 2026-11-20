namespace Ranvier.D5.Wgpu.View

open System
open System.Collections.Generic
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.D5.Wgpu
open Ranvier.Dom
open Ranvier.View
open Ranvier.View.Dsl
open Ranvier.View.Representation
open Ranvier.Wgpu

/// <summary>Viewport rectangle, independent editable camera, reactive scene producer, and pick callbacks for one browser <c>SubplotGrid3D</c> cell.</summary>
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

/// <summary>Constructors and pipeable modifiers for browser <c>SubplotCell3D</c>.</summary>
[<RequireQualifiedAccess>]
module SubplotCell3D =
    /// <summary>Creates a <c>SubplotCell3D</c> with <c>ShowHoverSpikes = true</c> and no pick callbacks.</summary>
    let create (viewport: ViewportRect) (camera: Editable<PlotCamera3D>) (readPass: unit -> PlotPass3D) : SubplotCell3D =
        if isNull (box camera) then nullArg (nameof camera)
        if isNull (box readPass) then nullArg (nameof readPass)

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

    let formatOverlayStyle (vp: ViewportRect) =
        $"position:absolute;left:{vp.X}px;top:{vp.Y}px;width:{vp.Width}px;height:{vp.Height}px;pointer-events:none;overflow:hidden;"

    let overlayStyleSig = Api.createSignal (formatOverlayStyle cell.Viewport)

    let mutable disposed = false
    let mutable latestCam: Camera3D option = None
    let mutable latestPass: PlotPass3D option = None
    let mutable hasHoverSpikes = false
    let mutable lastHoverHit: PickHit voption = ValueNone
    let mutable prevHadExtraMarks = false
    let mutable prevExtraScene: Scene voption = ValueNone
    let mutable prevOverlayVp = cell.Viewport

    let liveCssViewport () : ViewportRect =
        let struct (cssW, cssH) = readCssSize ()
        let sx = cssW / float (max 1 totalWidth)
        let sy = cssH / float (max 1 totalHeight)
        let x0 = max 0 (int (Math.Round (float cell.Viewport.X * sx)))
        let y0 = max 0 (int (Math.Round (float cell.Viewport.Y * sy)))
        let x1 = int (Math.Round (float (cell.Viewport.X + cell.Viewport.Width) * sx))
        let y1 = int (Math.Round (float (cell.Viewport.Y + cell.Viewport.Height) * sy))
        ViewportRect.create x0 y0 (max 1 (x1 - x0)) (max 1 (y1 - y0))

    let syncOverlayStyle (cssVp: ViewportRect) : bool =
        if cssVp <> prevOverlayVp then
            prevOverlayVp <- cssVp
            overlayStyleSig.Value <- formatOverlayStyle cssVp
            true
        else
            false

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
                syncOverlayStyle cssVp |> ignore

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
    member _.AllowHoverPick = cell.ShowHoverSpikes || cell.OnHoverHit.IsSome

    member _.LiveCssViewport() = liveCssViewport ()

    member _.LiveGpuViewport(dpr: float) : ViewportRect =
        let cssVp = liveCssViewport ()
        let gx0 = max 0 (int (Math.Round (float cssVp.X * dpr)))
        let gy0 = max 0 (int (Math.Round (float cssVp.Y * dpr)))
        let gx1 = int (Math.Round (float (cssVp.X + cssVp.Width) * dpr))
        let gy1 = int (Math.Round (float (cssVp.Y + cssVp.Height) * dpr))
        ViewportRect.create gx0 gy0 (max 1 (gx1 - gx0)) (max 1 (gy1 - gy0))

    member _.ReadOverlayStyle() : string = overlayStyleSig.Value

    member _.SyncViewport() : bool =
        let cssVp = liveCssViewport ()
        let styleChanged = syncOverlayStyle cssVp

        match axisMemo.CurrentSpec, latestCam, latestPass with
        | ValueSome spec, Some gpuCam, Some pass ->
            let update = axisMemo.Update (spec, cssVp.Width, cssVp.Height, gpuCam)

            if update.Rebuilt then
                applyOverlayMarks cssVp.Width cssVp.Height update.BaseOverlay pass.ExtraOverlayMarks
                true
            else
                styleChanged
        | _ -> styleChanged

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

    member _.ApplyHoverSpikes(hit: PickHit voption) : bool =
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

        lastHoverHit <- hit
        spikesDirty

    member this.ApplyHoverHit(hit: PickHit voption) : bool =
        let prevWasSome = lastHoverHit.IsSome
        let spikesDirty = this.ApplyHoverSpikes hit

        if hit.IsSome || prevWasSome then
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
        Canvas: HTMLCanvasElement
        Context: WgpuContext
        TotalWidth: int
        TotalHeight: int
        Cells: SubplotCell3D array
    }

[<Sealed>]
type private SubplotGridInstance
    (
        canvas: HTMLCanvasElement,
        context: WgpuContext,
        totalWidth: int,
        totalHeight: int,
        cells: SubplotCell3D array
    ) =
    let containerRef: NodeRef<HTMLDivElement> = NodeRef.create ()
    let boxMesh = context.UploadMesh (Primitives.unitBox ())
    let inputKernel = PlotInputKernel3D ()
    let hoverCoalescer = HoverPickCoalescer ()

    let mutable disposed = false
    let mutable mounted = false
    let mutable dirty = true
    let mutable rafId: int option = None
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
        let dpr = BrowserInterop3D.devicePixelRatio ()
        let passes = ResizeArray<ScenePass>(cellStates.Length)

        for i = 0 to cellStates.Length - 1 do
            let cs = cellStates[i]
            let gpuVp = cs.LiveGpuViewport dpr

            match cs.BuildScenePass (gpuVp, includeAxis) with
            | ValueSome p -> passes.Add p
            | ValueNone -> ()

        passes.ToArray ()

    let syncViewport () =
        if not disposed && not context.IsDisposed then
            let struct (_, _, cssW, cssH) = viewportCssSize ()
            let dpr = BrowserInterop3D.devicePixelRatio ()
            let targetDevW = max 1 (int (Math.Round (cssW * dpr)))
            let targetDevH = max 1 (int (Math.Round (cssH * dpr)))

            let uDevW = uint32 targetDevW
            let uDevH = uint32 targetDevH

            if context.Width <> targetDevW || context.Height <> targetDevH || canvas.width <> uDevW || canvas.height <> uDevH then
                canvas.width <- uDevW
                canvas.height <- uDevH
                context.Resize (targetDevW, targetDevH)
                dirty <- true

            for i = 0 to cellStates.Length - 1 do
                if cellStates[i].SyncViewport () then
                    dirty <- true

    let renderNow () =
        if not disposed && mounted && not context.IsDisposed then
            dirty <- false
            let passes = buildPasses true
            context.RenderPasses (ReadOnlySpan<ScenePass>(passes), RgbaColor.darkBackdrop) |> ignore

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

    let clientToPickCoords (clientX: float) (clientY: float) =
        let struct (rx, ry, rw, rh) = viewportCssSize ()
        let nx = Math.Clamp ((clientX - rx) / rw, 0.0, 1.0)
        let ny = Math.Clamp ((clientY - ry) / rh, 0.0, 1.0)
        let px = Math.Clamp (int (Math.Round (nx * float context.Width)), 0, max 0 (context.Width - 1))
        let py = Math.Clamp (int (Math.Round (ny * float context.Height)), 0, max 0 (context.Height - 1))
        struct (px, py)

    let rec drainHoverFollowup (followupOpt: struct (float * float * int) voption) =
        match followupOpt with
        | ValueSome struct (_, _, nextEp) ->
            let struct (_, followAgain) = hoverCoalescer.CompletePick nextEp
            drainHoverFollowup followAgain
        | ValueNone -> ()

    let clearHoverCell () =
        match lastHoverCell with
        | ValueSome prevIdx when prevIdx < cellStates.Length ->
            if cellStates[prevIdx].ClearHover () then
                dirty <- true
                scheduleFrame ()
        | _ -> ()

        lastHoverCell <- ValueNone

    let rec dispatchAsyncHover (clientX: float) (clientY: float) (dispatchEpoch: int) =
        if disposed || context.IsDisposed || inputKernel.IsDragging || dispatchEpoch <> hoverCoalescer.CurrentEpoch then
            let struct (_, followup) = hoverCoalescer.CompletePick dispatchEpoch
            drainHoverFollowup followup
        else
            let cellIdxOpt = findCellAtClient clientX clientY

            if lastHoverCell <> cellIdxOpt then
                clearHoverCell ()
                lastHoverCell <- cellIdxOpt

            match cellIdxOpt with
            | ValueSome idx when idx < cellStates.Length && cellStates[idx].AllowHoverPick ->
                let pickPasses = buildPasses false

                if pickPasses.Length > 0 then
                    let struct (px, py) = clientToPickCoords clientX clientY

                    BrowserInterop3D.thenPromise
                        (context.PickPassesAsync (px, py, ReadOnlySpan<ScenePass>(pickPasses)))
                        (fun hit ->
                            let struct (isCurrent, followup) = hoverCoalescer.CompletePick dispatchEpoch

                            if not disposed && isCurrent && not inputKernel.IsDragging && idx < cellStates.Length then
                                if cellStates[idx].ApplyHoverHit hit then
                                    dirty <- true
                                    scheduleFrame ()

                            match followup with
                            | ValueSome struct (nx, ny, nextEp) ->
                                if not disposed && not inputKernel.IsDragging then
                                    dispatchAsyncHover nx ny nextEp
                                else
                                    let struct (_, nextFollow) = hoverCoalescer.CompletePick nextEp
                                    drainHoverFollowup nextFollow
                            | ValueNone -> ())
                        (fun _ ->
                            let struct (_, followup) = hoverCoalescer.CompletePick dispatchEpoch
                            drainHoverFollowup followup)
                else
                    let struct (_, followup) = hoverCoalescer.CompletePick dispatchEpoch
                    drainHoverFollowup followup
            | _ ->
                let struct (_, followup) = hoverCoalescer.CompletePick dispatchEpoch
                drainHoverFollowup followup

    let dispatchClickPick (clientX: float) (clientY: float) (idx: int) (clickEpoch: int) =
        if not disposed && not context.IsDisposed && idx >= 0 && idx < cellStates.Length then
            let pickPasses = buildPasses false

            if pickPasses.Length > 0 then
                let struct (px, py) = clientToPickCoords clientX clientY

                BrowserInterop3D.thenPromise
                    (context.PickPassesAsync (px, py, ReadOnlySpan<ScenePass>(pickPasses)))
                    (fun hit ->
                        if not disposed && clickEpoch = hoverCoalescer.CurrentEpoch then
                            let cs = cellStates[idx]

                            if cs.ApplyHoverSpikes hit then
                                dirty <- true
                                scheduleFrame ()

                            cs.Cell.OnClickHit |> Option.iter (fun cb -> cb hit))
                    (fun _ -> ())

    let cleanupResources () =
        if not disposed then
            disposed <- true
            mounted <- false
            dirty <- false
            hoverCoalescer.AdvanceEpoch ()
            cancelFrame ()

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

    member _.ContainerRef = containerRef
    member _.CellRuntimes = cellStates
    member _.ContainerStyle =
        $"position:relative;width:{totalWidth}px;height:{totalHeight}px;overflow:hidden;outline:none;user-select:none;"

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

            if not isInteractive && inputKernel.OnPointerDown (ev.pointerId, ev.button, ev.clientX, ev.clientY, ev.shiftKey, false) then
                activeDragCell <- findCellAtClient ev.clientX ev.clientY

                match activeDragCell with
                | ValueSome idx -> focusedCell <- idx
                | ValueNone -> ()

                hoverCoalescer.AdvanceEpoch ()

                match containerRef.Current with
                | Some el ->
                    BrowserInterop3D.trySetPointerCapture el ev.pointerId

                    try
                        el.focus ()
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
                        match hoverCoalescer.RequestHover (ev.clientX, ev.clientY) with
                        | ValueSome pickEpoch -> dispatchAsyncHover ev.clientX ev.clientY pickEpoch
                        | ValueNone -> ()

    member _.OnPointerUp(ev: PointerEvent) =
        if not disposed then
            let up = inputKernel.OnPointerUp (ev.pointerId, ev.button, ev.clientX, ev.clientY)

            if up.Handled then
                let targetCell = activeDragCell
                activeDragCell <- ValueNone

                match containerRef.Current with
                | Some el -> BrowserInterop3D.tryReleasePointerCapture el ev.pointerId
                | None -> ()

                if up.WasClick then
                    let resolvedCell =
                        match targetCell with
                        | ValueSome idx -> ValueSome idx
                        | ValueNone -> findCellAtClient up.ClientX up.ClientY

                    match resolvedCell with
                    | ValueSome idx when idx < cellStates.Length ->
                        let cs = cellStates[idx]

                        if cs.Cell.ShowHoverSpikes || cs.Cell.OnClickHit.IsSome then
                            dispatchClickPick up.ClientX up.ClientY idx hoverCoalescer.CurrentEpoch
                    | _ -> ()

    member _.OnPointerCancel(ev: PointerEvent) =
        if not disposed && inputKernel.OnPointerCancel ev.pointerId then
            activeDragCell <- ValueNone

            match containerRef.Current with
            | Some el -> BrowserInterop3D.tryReleasePointerCapture el ev.pointerId
            | None -> ()

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
            && not (BrowserInterop3D.isInteractiveFormTarget ev.target)
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

/// <summary>Multi-pass synchronized browser 3D subplot grid sharing one <c>WgpuContext</c> canvas with per-cell camera routing and SVG overlays.</summary>
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

        let rects = ResizeArray<ViewportRect>(rows * cols)

        for r = 0 to rows - 1 do
            for c = 0 to cols - 1 do
                rects.Add (ViewportRect.create colX[c] rowY[r] colW[c] rowH[r])

        rects :> IReadOnlyList<ViewportRect>

    let private gridComponent: Component<SubplotGridProps> =
        Component.template
            (fun _ (props: SubplotGridProps) ->
                SubplotGridInstance (props.Canvas, props.Context, props.TotalWidth, props.TotalHeight, props.Cells))
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
                    div<SubplotGridInstance>(
                        [|
                            div.style "position:absolute;inset:0;"
                            div.lend (fun (inst: SubplotGridInstance) el -> inst.AttachCanvasHost el)
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
                                        div.style (fun (cs: SubplotCellRuntime) -> cs.ReadOverlayStyle ())
                                    |]
                                )
                                    .Run [| view (fun (cs: SubplotCellRuntime) -> Component.render Renderer.passive cs.ReadOverlay) |]
                                div<SubplotCellRuntime>(
                                    [|
                                        div.attribute ("data-subplot-extra", (fun (cs: SubplotCellRuntime) -> Some (string cs.Index)))
                                        div.style (fun (cs: SubplotCellRuntime) -> cs.ReadOverlayStyle ())
                                    |]
                                )
                                    .Run [| view (fun (cs: SubplotCellRuntime) -> Component.render Renderer.passive cs.ReadExtraOverlay) |]
                            |])
                |])

    /// <summary>
    /// Creates a browser multi-pass 3D subplot grid sharing one <c>WgpuContext</c> canvas,
    /// submitting all cells in one <c>RenderPasses</c> call and routing pointer/wheel/pick events per cell viewport.
    /// </summary>
    let create
        (canvas: HTMLCanvasElement)
        (context: WgpuContext)
        (totalWidth: int)
        (totalHeight: int)
        (cells: seq<SubplotCell3D>)
        : Program.View =
        if isNull (box canvas) then nullArg (nameof canvas)
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
                Canvas = canvas
                Context = context
                TotalWidth = totalWidth
                TotalHeight = totalHeight
                Cells = cellArr
            }

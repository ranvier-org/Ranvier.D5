module Ranvier.D5.Wgpu.Native.Tests.Program

open System
open System.Numerics
open Expecto
open Ranvier
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.D5.Wgpu.Sdl
open Ranvier.Dom
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore
open Ranvier.View.WebCore.Sdl
open Ranvier.Wgpu
open Ranvier.Wgpu.Sdl

let private unwrapOk msg =
    function
    | Ok value -> value
    | Error err -> failtestf "%s failed: %A" msg err

let private pumpFrames (win: Window) (count: int) =
    for _ = 1 to count do
        win.Host.Cycle ()
        win.Host.Frame ()
        win.Present ()

let private readWindowPixels (win: Window) =
    let struct (w, h) = win.PixelSize
    let buf = Array.zeroCreate<byte> (w * h * 4)
    let struct (rw, rh) = win.ReadPixelsInto (Span<byte>(buf))
    Expect.equal rw w "ReadPixelsInto width matches PixelSize"
    Expect.equal rh h "ReadPixelsInto height matches PixelSize"
    buf, w, h

let private countNonBackdropPixels (backdrop: RgbaColor) (pixels: byte array) =
    let br = int (MathF.Round (backdrop.R * 255.0f))
    let bg = int (MathF.Round (backdrop.G * 255.0f))
    let bb = int (MathF.Round (backdrop.B * 255.0f))
    let mutable count = 0

    for i in 0 .. 4 .. pixels.Length - 4 do
        let dr = abs (int pixels[i] - br)
        let dg = abs (int pixels[i + 1] - bg)
        let db = abs (int pixels[i + 2] - bb)

        if dr > 12 || dg > 12 || db > 12 then
            count <- count + 1

    count

let private runNativeAcceptance () =
    // Main-thread hidden SDL window creation for macOS/headless CI compatibility.
    let win = Window.Create ("D5WgpuTest", 480, 360, hidden = true)
    let wgpu = WgpuSdlSurface.CreateContext win
    let graph = new Graph ()

    let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.5, 1.5)
    let ramp = ColorRamp.viridis (-1.5, 1.5)

    let waveGrid =
        SurfaceGrid.fromFunction
            32
            32
            (-1.0, 1.0)
            (-1.0, 1.0)
            (fun x y -> 0.8 * sin (x * 2.5) * cos (y * 2.5))
            ValueNone
        |> unwrapOk "waveGrid"

    let surfaceSpec =
        {
            Scale = scale
            Color = ramp
            Shading = ShadingMode.Lit (0.35f, 0.05f)
            Opacity = 1.0f
            Wireframe = ValueNone
            PickId = 0x02000001u
        }

    let hoverHits = ResizeArray<PickHit voption>()
    let clickHits = ResizeArray<PickHit voption>()

    let countBrightPixels (threshold: byte) (pixels: byte array) =
        let mutable count = 0

        for i in 0 .. 4 .. pixels.Length - 4 do
            if pixels[i] > threshold || pixels[i + 1] > threshold || pixels[i + 2] > threshold then
                count <- count + 1

        count

    let mutable tickFormatCalls = 0

    let dataStepSig, gridSig, camera, surfaceBatch, sphereMesh, probeSink, batchesMem, passSig =
        graph.Run (fun () ->
            let stepSig = Api.createSignal 0
            let gSig = Api.createSignal waveGrid
            let cam = Reactive3D.editableCamera (fun () -> PlotCamera3D.defaultValue)
            let surf = Reactive3D.bindSurface wgpu (fun () -> surfaceSpec) (fun () -> gSig.Value)
            let mesh = wgpu.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Sphere)
            Api.onCleanup (fun () -> mesh.Dispose ())

            let probes = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)

            let makeProbe (pt: Point3) (col: RgbaColor) : GlyphDatum3D =
                {
                    Position = pt
                    Scale = Vector3 (0.24f, 0.24f, 0.24f)
                    Rotation = Quaternion.Identity
                    Color = col
                    Shading = ShadingMode.Lit (0.3f, 0.1f)
                    PickId = 0u
                    Selected = false
                }

            probes.AddOrUpdate (struct ("center", makeProbe (Point3.create 0.0 0.0 0.9) (RgbaColor (1.0f, 0.35f, 0.15f, 1.0f))))
            probes.AddOrUpdate (struct ("corner", makeProbe (Point3.create 0.65 -0.65 0.4) (RgbaColor (0.2f, 0.9f, 0.85f, 1.0f))))

            let probeRows = probes.Rows |> Projection.map (fun struct (_, g) -> g)
            let sink = Reactive3D.bindInstances wgpu 1uy mesh BlendMode.Opaque false false (fun () -> scale) probeRows
            let batches = ReadOnlyMemory<InstanceBatch> [| surf.Batch; sink.Batch |]

            let baseAxis = Axis3D.defaultSpec scale

            let axisSpec =
                { baseAxis with
                    X =
                        { baseAxis.X with
                            FormatTick =
                                Some (fun v ->
                                    tickFormatCalls <- tickFormatCalls + 1
                                    v.ToString ("F1", System.Globalization.CultureInfo.InvariantCulture))
                        }
                }

            let pass: PlotPass3D =
                { PlotPass3D.create axisSpec with
                    ExtraBatches = batches
                }

            let passSig = Api.createSignal pass
            stepSig, gSig, cam, surf, mesh, sink, batches, passSig)

    ignore surfaceBatch
    ignore sphereMesh
    ignore batchesMem

    let config: PlotViewConfig3D =
        PlotViewConfig3D.create 480 360 camera
        |> PlotViewConfig3D.withOnHoverHit hoverHits.Add
        |> PlotViewConfig3D.withOnClickHit clickHits.Add

    let container = win.Host.Document.createElement "div"
    win.Host.Document.body.appendChild container |> ignore

    let mounted =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            container
            None
            (fun ctx -> Program.run (PlotView3D.create win wgpu config (fun () -> ignore dataStepSig.Value; passSig.Value)) ctx.Backend)

    pumpFrames win 3
    Expect.isTrue PlotView3DStats.PanelDrawBeforeLines "PlotView3D panelBatch enables drawBeforeLines = true so back walls draw behind axis lines"

    // Verify dirty-only RAF: idle turns trigger 0 extra WebGPU renders
    let renderCallsIdle0 = wgpu.RenderCalls
    pumpFrames win 4
    Expect.equal wgpu.RenderCalls renderCallsIdle0 "idle turns issue 0 WebGPU re-renders"

    // 1. Wave pixels + DOM axis/x/title + 0 wheel listeners on marks
    let pixelsBefore, pw, ph = readWindowPixels win
    let nonBackdrop = countNonBackdropPixels RgbaColor.darkBackdrop pixelsBefore
    Expect.isGreaterThan nonBackdrop 1000 "rendered wave + probe spheres produce non-backdrop pixels"

    let titleGroupBefore = container.querySelector "[data-d5-key='axis/x/title']"
    Expect.isNotNull titleGroupBefore "SVG overlay holds [data-d5-key='axis/x/title']"
    let titleTextBefore = titleGroupBefore.querySelector "text"
    Expect.isNotNull titleTextBefore "axis/x/title contains SVG <text> element"
    let titleXBefore = titleTextBefore.getAttribute "x"
    let titleYBefore = titleTextBefore.getAttribute "y"

    let svgOverlay = container.querySelector "svg"
    Expect.isNotNull svgOverlay "SVG overlay mounted"

    // Dispatch non-bubbling wheel event directly to mark: 0 mark listeners should handle it
    let zoomBefore = graph.Run (fun () -> camera.Value.Zoom)
    let nonBubblingWheel = win.Host.Document.createEvent "Event"
    nonBubblingWheel.initEvent ("wheel", false, false)
    titleGroupBefore.dispatchEvent nonBubblingWheel |> ignore
    Expect.equal (graph.Run (fun () -> camera.Value.Zoom)) zoomBefore "axis mark has 0 wheel listeners of its own"

    // Real wheel event on container zooms camera cleanly
    win.Host.Wheel (240, 180, 0.0f, -120.0f, 0) |> ignore
    pumpFrames win 2
    let zoomAfterWheel = graph.Run (fun () -> camera.Value.Zoom)
    Expect.isGreaterThan zoomAfterWheel zoomBefore "container wheel event zooms PlotCamera3D"

    // 2. Pointer drag (orbitBy) retains DOM node identity while changing projected x/y and rendered pixels
    let camBefore = graph.Run (fun () -> camera.Value)
    win.Host.Mouse (1, 240, 180, 0, 1, 0) |> ignore
    win.Host.Mouse (0, 310, 145, 0, 0, 0) |> ignore
    win.Host.Mouse (2, 310, 145, 0, 1, 0) |> ignore
    pumpFrames win 3

    let camAfterDrag = graph.Run (fun () -> camera.Value)
    Expect.notEqual camAfterDrag.AzimuthRad camBefore.AzimuthRad "pointer drag updates camera AzimuthRad via orbitBy"
    Expect.notEqual camAfterDrag.ElevationRad camBefore.ElevationRad "pointer drag updates camera ElevationRad via orbitBy"

    let titleGroupAfter = container.querySelector "[data-d5-key='axis/x/title']"
    Expect.isTrue (Object.ReferenceEquals (titleGroupBefore, titleGroupAfter)) "axis/x/title retains DOM node reference identity across orbit"
    let titleTextAfter = titleGroupAfter.querySelector "text"
    let titleXAfter = titleTextAfter.getAttribute "x"
    let titleYAfter = titleTextAfter.getAttribute "y"
    Expect.isTrue (titleXBefore <> titleXAfter || titleYBefore <> titleYAfter) "axis/x/title projected SVG x/y attributes change after orbit"

    let pixelsAfterDrag, _, _ = readWindowPixels win
    Expect.isFalse (pixelsBefore = pixelsAfterDrag) "rendered pixels change after pointer orbit"

    // 3. Hover / pick on KeyedCollection sphere updates OnHoverHit, Probe3D readout, and hover spikes
    let gpuCam = PlotCamera3D.toCamera3D scale camAfterDrag
    let probeWorld = Scale3D.map scale (Point3.create 0.0 0.0 0.9)
    let probeScreenPx = Camera3D.projectToScreen wgpu.Width wgpu.Height gpuCam probeWorld |> ValueOption.get
    let cssX = int (MathF.Round (probeScreenPx.X / float32 pw * 480.0f))
    let cssY = int (MathF.Round (probeScreenPx.Y / float32 ph * 360.0f))

    hoverHits.Clear ()
    win.Host.Mouse (0, cssX, cssY, 0, 0, 0) |> ignore
    pumpFrames win 3

    let lastHoverHit =
        hoverHits
        |> Seq.tryLast
        |> Option.defaultWith (fun () -> failtest "Expected OnHoverHit callback on mouse move over probe sphere")

    let resolvedHover =
        match lastHoverHit with
        | ValueSome hit -> Probe3D.resolveKeyed scale probeSink hit
        | ValueNone -> failtest "Expected non-empty PickHit over probe sphere"

    Expect.equal (resolvedHover |> ValueOption.map _.Key) (ValueSome "center") "hover hit resolves KeyedCollection sphere key 'center'"

    let pixelsAfterHover, _, _ = readWindowPixels win
    Expect.isFalse (pixelsAfterDrag = pixelsAfterHover) "hover spikes update rendered pixels"

    // Click also fires OnClickHit resolving the probe
    clickHits.Clear ()
    win.Host.Mouse (1, cssX, cssY, 0, 1, 0) |> ignore
    win.Host.Mouse (2, cssX, cssY, 0, 1, 0) |> ignore
    pumpFrames win 2

    let resolvedClick =
        clickHits
        |> Seq.tryLast
        |> Option.bind (function ValueSome hit -> Some (Probe3D.resolveKeyed scale probeSink hit) | ValueNone -> None)
        |> Option.defaultWith (fun () -> failtest "Expected OnClickHit callback with PickHit over probe sphere")

    Expect.equal (resolvedClick |> ValueOption.map _.Key) (ValueSome "center") "click hit resolves KeyedCollection sphere key 'center'"

    // 4. Keyboard "m" key toggle preserves continuous camera world Position across Turntable <-> Arcball
    let plotRoot = container.querySelector "[tabindex='0']"
    Expect.isNotNull plotRoot "PlotView3D container has tabindex='0'"
    win.Host.SetFocused true
    (plotRoot :?> HTMLElement).focus ()
    pumpFrames win 1

    let camBeforeModeToggle = graph.Run (fun () -> camera.Value)
    let posBeforeModeToggle = (PlotCamera3D.toCamera3D scale camBeforeModeToggle).Position
    Expect.equal camBeforeModeToggle.Mode OrbitMode3D.Turntable "camera starts in Turntable before 'm' toggle"

    win.Host.Key (0, "m", "KeyM", null, 0, false) |> ignore
    win.Host.Key (1, "m", "KeyM", null, 0, false) |> ignore
    pumpFrames win 2

    let camAfterM1 = graph.Run (fun () -> camera.Value)
    let posAfterM1 = (PlotCamera3D.toCamera3D scale camAfterM1).Position
    Expect.equal camAfterM1.Mode OrbitMode3D.Arcball "'m' toggles Turntable -> Arcball"
    Expect.floatClose { absolute = 1e-4; relative = 0.0 } (float (Vector3.Distance (posAfterM1, posBeforeModeToggle))) 0.0 "'m' toggle to Arcball preserves camera world Position"

    win.Host.Key (0, "M", "KeyM", null, 0, false) |> ignore
    win.Host.Key (1, "M", "KeyM", null, 0, false) |> ignore
    pumpFrames win 2

    let camAfterM2 = graph.Run (fun () -> camera.Value)
    let posAfterM2 = (PlotCamera3D.toCamera3D scale camAfterM2).Position
    Expect.equal camAfterM2.Mode OrbitMode3D.Turntable "'M' toggles Arcball -> Turntable"
    Expect.floatClose { absolute = 1e-4; relative = 0.0 } (float (Vector3.Distance (posAfterM2, posAfterM1))) 0.0 "'M' toggle to Turntable preserves camera world Position"

    // 5. Chorded secondary pointerdown while dragging does not steal activePointerId or switch orbit to pan
    let camAtMouseDown = graph.Run (fun () -> camera.Value)
    win.Host.Mouse (1, 240, 180, 0, 1, 0) |> ignore
    win.Host.Mouse (1, 280, 200, 2, 1, 0) |> ignore
    win.Host.Mouse (0, 290, 160, 0, 0, 0) |> ignore
    win.Host.Mouse (2, 290, 160, 0, 1, 0) |> ignore
    win.Host.Mouse (2, 290, 160, 2, 1, 0) |> ignore
    pumpFrames win 1

    let camAfterChordedMouse = graph.Run (fun () -> camera.Value)
    Expect.notEqual camAfterChordedMouse.AzimuthRad camAtMouseDown.AzimuthRad "chorded right-button mouse down preserves left-button orbit drag"
    Expect.equal camAfterChordedMouse.PanOffset camAtMouseDown.PanOffset "chorded right-button mouse down does not flip dragPan on"

    let mutable p1Down = PointerSample.Create (2, 10, 1, 240.0, 180.0)
    p1Down.Button <- 0
    let mutable p2Down = PointerSample.Create (2, 20, 1, 280.0, 200.0)
    p2Down.Button <- 2
    let p2Move = PointerSample.Create (2, 20, 0, 320.0, 230.0)
    let p2Up = PointerSample.Create (2, 20, 2, 320.0, 230.0)
    let p1Move = PointerSample.Create (2, 10, 0, 290.0, 160.0)
    let p1Up = PointerSample.Create (2, 10, 2, 290.0, 160.0)

    let supportsHostPointer =
        try
            win.Host.SetMaxTouchPoints 2
            win.Host.Pointer [| p1Down |] |> ignore
            true
        with :? NotSupportedException ->
            false

    if supportsHostPointer then
        let camAtP1Down = graph.Run (fun () -> camera.Value)

        // Secondary pointerdown while p1 is dragging must be ignored, and moves from p2 must not alter camera
        win.Host.Pointer [| p2Down |] |> ignore
        win.Host.Pointer [| p2Move |] |> ignore
        win.Host.Pointer [| p2Up |] |> ignore
        pumpFrames win 1

        let camAfterChordedP2 = graph.Run (fun () -> camera.Value)
        Expect.equal camAfterChordedP2 camAtP1Down "chorded secondary pointerdown/move/up does not steal activePointerId or mutate camera"

        // Original active pointer (id = 10) still orbits and completes cleanly
        win.Host.Pointer [| p1Move |] |> ignore
        win.Host.Pointer [| p1Up |] |> ignore
        pumpFrames win 1

        let camAfterP1Drag = graph.Run (fun () -> camera.Value)
        Expect.notEqual camAfterP1Drag.AzimuthRad camAtP1Down.AzimuthRad "original activePointerId still orbits after chorded secondary pointer"
        Expect.equal camAfterP1Drag.PanOffset camAtP1Down.PanOffset "chorded secondary pointerdown does not flip dragPan on"

    // 6. Data-only wave step with unchanged Axis3DSpec reference (even with FormatTick = Some) and camera:
    //    updates rendered surface pixels while keeping SVG axis DOM nodes untouched
    graph.Run (fun () -> camera.Reset ())
    win.Host.Mouse (0, 2, 2, 0, 0, 0) |> ignore
    pumpFrames win 3

    let pixelsBeforeDataStep, _, _ = readWindowPixels win
    let renderCallsBeforeDataStep = wgpu.RenderCalls
    let tickFormatCallsBeforeStep = tickFormatCalls
    Expect.isGreaterThan tickFormatCallsBeforeStep 0 "FormatTick ran on initial/orbited Axis3D builds"
    let titleGroupBeforeStep = container.querySelector "[data-d5-key='axis/x/title']"
    let titleTextBeforeStep = titleGroupBeforeStep.querySelector "text"
    let titleXStep = titleTextBeforeStep.getAttribute "x"
    let titleYStep = titleTextBeforeStep.getAttribute "y"
    let tickGroupBeforeStep = container.querySelector "[data-d5-key='axis/x/tick/0']"
    Expect.isNotNull tickGroupBeforeStep "SVG overlay holds [data-d5-key='axis/x/tick/0']"
    let tickTextBeforeStep = tickGroupBeforeStep.querySelector "text"
    Expect.isNotNull tickTextBeforeStep "axis/x/tick/0 contains SVG <text> element"
    let tickXStep = tickTextBeforeStep.getAttribute "x"
    let tickYStep = tickTextBeforeStep.getAttribute "y"
    let tickLabelStep = tickTextBeforeStep.textContent

    let waveGrid2 =
        SurfaceGrid.fromFunction
            32
            32
            (-1.0, 1.0)
            (-1.0, 1.0)
            (fun x y -> -1.0 * cos (x * 2.5) * sin (y * 2.5))
            ValueNone
        |> unwrapOk "waveGrid2"

    graph.Run (fun () ->
        Api.batch (fun () ->
            gridSig.Value <- waveGrid2
            dataStepSig.Value <- dataStepSig.Value + 1))

    pumpFrames win 3

    Expect.isGreaterThan wgpu.RenderCalls renderCallsBeforeDataStep "data-only wave step schedules a WebGPU re-render"
    Expect.equal tickFormatCalls tickFormatCallsBeforeStep "data-only wave step reuses cached Axis3D.build without re-invoking FormatTick"
    let pixelsAfterDataStep, _, _ = readWindowPixels win
    Expect.isFalse (pixelsBeforeDataStep = pixelsAfterDataStep) "data-only wave step updates rendered surface pixels"

    let titleGroupAfterStep = container.querySelector "[data-d5-key='axis/x/title']"
    let titleTextAfterStep = titleGroupAfterStep.querySelector "text"
    let tickGroupAfterStep = container.querySelector "[data-d5-key='axis/x/tick/0']"
    let tickTextAfterStep = tickGroupAfterStep.querySelector "text"
    Expect.isTrue (Object.ReferenceEquals (titleGroupBeforeStep, titleGroupAfterStep)) "axis/x/title group DOM node untouched across data-only wave step"
    Expect.isTrue (Object.ReferenceEquals (titleTextBeforeStep, titleTextAfterStep)) "axis/x/title <text> DOM node untouched across data-only wave step"
    Expect.equal (titleTextAfterStep.getAttribute "x") titleXStep "axis/x/title x unchanged across data-only wave step"
    Expect.equal (titleTextAfterStep.getAttribute "y") titleYStep "axis/x/title y unchanged across data-only wave step"
    Expect.isTrue (Object.ReferenceEquals (tickGroupBeforeStep, tickGroupAfterStep)) "axis/x/tick/0 group DOM node untouched across data-only wave step"
    Expect.isTrue (Object.ReferenceEquals (tickTextBeforeStep, tickTextAfterStep)) "axis/x/tick/0 <text> DOM node untouched across data-only wave step"
    Expect.equal (tickTextAfterStep.getAttribute "x") tickXStep "axis/x/tick/0 x unchanged across data-only wave step"
    Expect.equal (tickTextAfterStep.getAttribute "y") tickYStep "axis/x/tick/0 y unchanged across data-only wave step"
    Expect.equal tickTextAfterStep.textContent tickLabelStep "axis/x/tick/0 text unchanged across data-only wave step"

    // ExtraOverlayMarks transition [] -> [mark] -> [] reuses cached Axis3D.build while mounting and clearing the mark
    let hudMark = Mark.text "hud/cache-check" (Point.create 16.0 20.0) "cached"

    graph.Run (fun () ->
        let cur = passSig.Value
        passSig.Value <- { cur with ExtraOverlayMarks = [ hudMark ] })

    pumpFrames win 1
    Expect.isNotNull (container.querySelector "[data-d5-key='hud/cache-check']") "ExtraOverlayMarks mounts HUD mark on cached axis frame"
    Expect.equal tickFormatCalls tickFormatCallsBeforeStep "adding ExtraOverlayMarks reuses cached Axis3D.build"

    graph.Run (fun () ->
        let cur = passSig.Value
        passSig.Value <- { cur with ExtraOverlayMarks = [] })

    pumpFrames win 1
    Expect.isNull (container.querySelector "[data-d5-key='hud/cache-check']") "clearing ExtraOverlayMarks removes HUD mark cleanly"
    Expect.equal tickFormatCalls tickFormatCallsBeforeStep "clearing ExtraOverlayMarks reuses cached Axis3D.build"

    // 7. Enabling SectionPlane on PlotPass3D clips the wave while keeping Axis3D cuboid lines visible
    let brightUnclipped = countBrightPixels 90uy pixelsAfterDataStep
    Expect.isGreaterThan brightUnclipped 1500 "unclipped wave has >1500 bright surface/axis pixels"

    graph.Run (fun () ->
        let cur = passSig.Value
        passSig.Value <- { cur with SectionPlane = ValueSome (SectionPlane.create Vector3.UnitX 0.0f) })

    pumpFrames win 3
    let pixelsHalfClipped, _, _ = readWindowPixels win
    let brightHalfClipped = countBrightPixels 90uy pixelsHalfClipped
    Expect.isFalse (pixelsAfterDataStep = pixelsHalfClipped) "half-space SectionPlane changes rendered pixels"
    Expect.isLessThan brightHalfClipped brightUnclipped "half-space SectionPlane clips bright wave surface pixels revealing back-wall panel"

    graph.Run (fun () ->
        let cur = passSig.Value
        passSig.Value <- { cur with SectionPlane = ValueSome (SectionPlane.create Vector3.UnitY -10.0f) })

    pumpFrames win 3
    let pixelsFullClipped, _, _ = readWindowPixels win
    let brightLinesFullClipped = countBrightPixels 90uy pixelsFullClipped
    Expect.isLessThan brightLinesFullClipped brightHalfClipped "full SectionPlane clips remaining bright wave + probe pixels"
    Expect.isGreaterThan brightLinesFullClipped 50 "section-exempt Axis3D cuboid spines/gridlines/ticks stay bright and visible"

    // 8. Hover pick coalescing: 10 rapid hover moves before Frame issue at most 1 GPU pick and resolve final probe position
    graph.Run (fun () ->
        let cur = passSig.Value
        passSig.Value <- { cur with SectionPlane = ValueNone })

    win.Host.Mouse (0, 2, 2, 0, 0, 0) |> ignore
    pumpFrames win 2

    let coalescedGpuCam = PlotCamera3D.toCamera3D scale (graph.Run (fun () -> camera.Value))
    let coalescedScrPx = Camera3D.projectToScreen wgpu.Width wgpu.Height coalescedGpuCam (Scale3D.map scale (Point3.create 0.0 0.0 0.9)) |> ValueOption.get
    let finalCssX = int (MathF.Round (coalescedScrPx.X / float32 pw * 480.0f))
    let finalCssY = int (MathF.Round (coalescedScrPx.Y / float32 ph * 360.0f))

    hoverHits.Clear ()
    let pickCallsBeforeCoalesce = PlotView3DStats.PickCalls

    for i = 0 to 8 do
        win.Host.Mouse (0, 200 + i * 3, 160 + i * 2, 0, 0, 0) |> ignore

    win.Host.Mouse (0, finalCssX, finalCssY, 0, 0, 0) |> ignore
    pumpFrames win 1

    let pickCallsDelta = PlotView3DStats.PickCalls - pickCallsBeforeCoalesce
    Expect.isLessThanOrEqual pickCallsDelta 1L "10 rapid hover moves before Frame issue at most 1 GPU pick"
    Expect.isLessThanOrEqual hoverHits.Count 1 "10 rapid hover moves invoke OnHoverHit at most once"

    let coalescedHit =
        hoverHits
        |> Seq.tryLast
        |> Option.bind (function ValueSome h -> Some (Probe3D.resolveKeyed scale probeSink h) | ValueNone -> None)
        |> Option.defaultWith (fun () -> failtest "Expected coalesced final hover hit on probe sphere")

    Expect.equal (coalescedHit |> ValueOption.map _.Key) (ValueSome "center") "coalesced hover pick resolves final 'center' probe"

    // 9. Keyboard hygiene: Ctrl+R and child <input> keydown never reset or orbit PlotCamera3D
    graph.Run (fun () ->
        let c = camera.Value
        camera.Value <- PlotCamera3D.orbitBy 25.0f 15.0f c)

    pumpFrames win 1
    let camBeforeKeyHygiene = graph.Run (fun () -> camera.Value)
    (plotRoot :?> HTMLElement).focus ()
    win.Host.Key (0, "r", "KeyR", null, 2, false) |> ignore
    win.Host.Key (1, "r", "KeyR", null, 2, false) |> ignore
    pumpFrames win 1
    Expect.equal (graph.Run (fun () -> camera.Value)) camBeforeKeyHygiene "Ctrl+R does not reset PlotCamera3D"

    let childInput = win.Host.Document.createElement "input"
    plotRoot.appendChild childInput |> ignore
    (childInput :?> HTMLElement).focus ()
    win.Host.Key (0, "r", "KeyR", null, 0, false) |> ignore
    win.Host.Key (1, "r", "KeyR", null, 0, false) |> ignore
    win.Host.Key (0, "ArrowLeft", "ArrowLeft", null, 0, false) |> ignore
    win.Host.Key (1, "ArrowLeft", "ArrowLeft", null, 0, false) |> ignore
    pumpFrames win 1
    Expect.equal (graph.Run (fun () -> camera.Value)) camBeforeKeyHygiene "keydown originating on child <input> does not reset or orbit PlotCamera3D"
    plotRoot.removeChild childInput |> ignore
    (plotRoot :?> HTMLElement).focus ()

    // 10. PlotPass3D.ExtraOverlay renders full Scene + <linearGradient> defs in sibling passive container without rebuilding Axis3D
    let gradStops =
        [
            { Offset = 0.0; Color = Paint.rgb 255 0 0; Opacity = 1.0 }
            { Offset = 1.0; Color = Paint.rgb 0 0 255; Opacity = 1.0 }
        ]

    let extraScene =
        Scene.compose
            { Width = 480.0; Height = 360.0 }
            "ExtraOverlay"
            [ LinearGradient ("d2-grad", Point.create 0.0 0.0, Point.create 100.0 0.0, gradStops) ]
            [ Leaf (Shape.rectangle (Bounds.create 12.0 12.0 80.0 16.0) |> Mark.path "extra/gradient-box" |> Mark.withFillDefinition "d2-grad") ]

    let tickFormatCallsBeforeExtra = tickFormatCalls

    graph.Run (fun () ->
        let cur = passSig.Value
        passSig.Value <- { cur with ExtraOverlay = ValueSome extraScene })

    pumpFrames win 1
    Expect.isNotNull (container.querySelector "[data-d5-key='extra/gradient-box']") "ExtraOverlay mounts gradient rectangle mark"
    Expect.isNotNull (container.querySelector "defs linearGradient") "ExtraOverlay mounts <linearGradient> inside <defs>"
    Expect.equal tickFormatCalls tickFormatCallsBeforeExtra "adding ExtraOverlay reuses cached Axis3D.build without re-invoking FormatTick"
    Expect.isTrue (Object.ReferenceEquals (titleGroupBeforeStep, container.querySelector "[data-d5-key='axis/x/title']")) "ExtraOverlay leaves axis/x/title DOM node untouched"

    graph.Run (fun () ->
        let cur = passSig.Value
        passSig.Value <- { cur with ExtraOverlay = ValueNone })

    pumpFrames win 1
    Expect.isNull (container.querySelector "[data-d5-key='extra/gradient-box']") "clearing ExtraOverlay removes extra mark"
    Expect.equal tickFormatCalls tickFormatCallsBeforeExtra "clearing ExtraOverlay preserves cached Axis3D.build without re-invoking FormatTick"
    Expect.isTrue (Object.ReferenceEquals (titleGroupBeforeStep, container.querySelector "[data-d5-key='axis/x/title']")) "clearing ExtraOverlay preserves axis/x/title DOM node identity"

    // Review Focus #5: idempotent double mounted.Dispose(), then graph.Dispose(), wgpu.Dispose(), win.Dispose()
    mounted.Dispose ()
    mounted.Dispose ()
    win.Host.Cycle ()
    graph.Dispose ()
    wgpu.Dispose ()
    (win :> IDisposable).Dispose ()

let private runSubRectDprAndKeyMapChecks () =
    // Sub-window PlotView3D (480x300 inside 480x360 window) + OnHoverHit = None hover spikes + Disabled/Custom PlotKeyMap3D + AutoFocus
    let win = Window.Create ("D5WgpuSubRect", 480, 360, hidden = true)
    let wgpu = WgpuSdlSurface.CreateContext win
    let graph = new Graph ()

    let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
    let ramp = ColorRamp.viridis (-1.0, 1.0)

    let boxGrid =
        SurfaceGrid.fromFunction 12 12 (-0.8, 0.8) (-0.8, 0.8) (fun x y -> 0.4 * cos (x * 2.0) * cos (y * 2.0)) ValueNone
        |> unwrapOk "subRectGrid"

    let spec: SurfaceSpec =
        {
            Scale = scale
            Color = ramp
            Shading = ShadingMode.Unlit
            Opacity = 1.0f
            Wireframe = ValueNone
            PickId = 0x01000001u
        }

    let cam, surf =
        graph.Run (fun () ->
            let c = Reactive3D.editableCamera (fun () -> PlotCamera3D.defaultValue)
            let s = SurfaceBatch.Create (wgpu, spec, boxGrid)
            Api.onCleanup (fun () -> s.Dispose ())
            c, s)

    let pass =
        { PlotPass3D.create (Axis3D.defaultSpec scale) with
            ExtraBatches = ReadOnlyMemory<InstanceBatch> [| surf.Batch |]
        }

    let config =
        PlotViewConfig3D.create 480 300 cam
        |> PlotViewConfig3D.withKeyMap PlotKeyMap3D.Disabled
        |> PlotViewConfig3D.withAutoFocus true

    let container = win.Host.Document.createElement "div"
    win.Host.Document.body.appendChild container |> ignore

    let mounted =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            container
            None
            (fun ctx -> Program.run (PlotView3D.create win wgpu config (fun () -> pass)) ctx.Backend)

    pumpFrames win 2

    // 1. Live GPU surface & canvas attributes match 480x300 (scaled by window DPR), NOT initial 480x360
    let docEl = win.Host.Document.documentElement
    let struct (winPxW, winPxH) = win.PixelSize
    let dprX = if docEl.clientWidth > 0 then float winPxW / float docEl.clientWidth else 1.0
    let dprY = if docEl.clientHeight > 0 then float winPxH / float docEl.clientHeight else 1.0
    Expect.equal wgpu.Width (max 1 (int (Math.Round (480.0 * dprX)))) "wgpu.Width matches 480 * dprX"
    Expect.equal wgpu.Height (max 1 (int (Math.Round (300.0 * dprY)))) "wgpu.Height resizes from 360 to 300 * dprY"

    let canvasEl = container.querySelector "canvas"
    Expect.isNotNull canvasEl "canvas mounted"
    Expect.equal (canvasEl.getAttribute "height") (string wgpu.Height) "canvas height attribute synced to device pixels"

    // 2. ShowHoverSpikes = true draws spikes even when OnHoverHit = None
    let pixelsNoHover, _, _ = readWindowPixels win
    let centerPt = Scale3D.map scale (Point3.create 0.0 0.0 0.4)
    let centerPx = Camera3D.projectToScreen wgpu.Width wgpu.Height (PlotCamera3D.toCamera3D scale PlotCamera3D.defaultValue) centerPt |> ValueOption.get
    win.Host.Mouse (0, int (MathF.Round (float32 centerPx.X / float32 wgpu.Width * 480.0f)), int (MathF.Round (float32 centerPx.Y / float32 wgpu.Height * 300.0f)), 0, 0, 0) |> ignore
    pumpFrames win 2
    let pixelsWithHover, _, _ = readWindowPixels win
    Expect.isFalse (pixelsNoHover = pixelsWithHover) "OnHoverHit = None with ShowHoverSpikes = true still renders hover spikes"

    // 3. AutoFocus + PlotKeyMap3D.Disabled ignores built-in shortcuts without requiring manual .focus()
    let plotRoot = container.querySelector "[tabindex='0']"
    Expect.isTrue (Object.ReferenceEquals (win.Host.Document.activeElement, plotRoot)) "AutoFocus focuses container on mount"
    win.Host.SetFocused true
    let camBeforeDisabled = graph.Run (fun () -> cam.Value)
    win.Host.Key (0, "m", "KeyM", null, 0, false) |> ignore
    win.Host.Key (0, "ArrowLeft", "ArrowLeft", null, 0, false) |> ignore
    pumpFrames win 1
    Expect.equal (graph.Run (fun () -> cam.Value)) camBeforeDisabled "PlotKeyMap3D.Disabled ignores 'm' and 'ArrowLeft'"

    // 4. Custom PlotKeyMap3D handles custom shortcut and ignores unhandled keys
    mounted.Dispose ()
    let customContainer = win.Host.Document.createElement "div"
    win.Host.Document.body.appendChild customContainer |> ignore

    let customConfig =
        PlotViewConfig3D.create 480 300 cam
        |> PlotViewConfig3D.withKeyMap
            (PlotKeyMap3D.Custom (fun ev c ->
                if ev.key = "x" then ValueSome { c with Zoom = 2.0f } else ValueNone))

    let customMounted =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            customContainer
            None
            (fun ctx -> Program.run (PlotView3D.create win wgpu customConfig (fun () -> pass)) ctx.Backend)

    pumpFrames win 1
    win.Host.Key (0, "x", "KeyX", null, 0, false) |> ignore
    pumpFrames win 1
    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (graph.Run (fun () -> cam.Value.Zoom))) 2.0 "PlotKeyMap3D.Custom updates camera on mapped key"

    customMounted.Dispose ()
    win.Host.Cycle ()
    graph.Dispose ()
    wgpu.Dispose ()
    (win :> IDisposable).Dispose ()

let private runLayersAndPolishChecks () =
    let win = Window.Create ("D5WgpuLayersPolish", 480, 360, hidden = true)
    let wgpu = WgpuSdlSurface.CreateContext win
    let graph = new Graph ()

    let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
    let ramp = ColorRamp.viridis (-1.0, 1.0)

    let clickHits = ResizeArray<PickHit voption>()
    let surfGrid =
        SurfaceGrid.fromFunction3D 16 16 (-0.8, 0.8) (-0.8, 0.8) (fun _ _ -> 0.5)
        |> unwrapOk "layersPolishGrid"

    let cam, surfBatch, axisSpecSig =
        graph.Run (fun () ->
            let reg = PickRegistry3D ()
            let rev = Reactive3D.createRevision ()
            let opts = SinkOptions3D.defaultValue |> SinkOptions3D.withRegistry reg |> SinkOptions3D.withRevision rev |> SinkOptions3D.withPickId 101u
            let c = Reactive3D.editableCamera (fun () -> PlotCamera3D.defaultValue)
            let specSig = Api.createSignal { SurfaceSpec.create scale ramp with PickId = 101u; Shading = ShadingMode.Unlit }
            let gridSig = Api.createSignal surfGrid
            let s = Reactive3D.bindSurfaceWith wgpu opts (fun () -> specSig.Value) (fun () -> gridSig.Value)
            let aSig = Api.createSignal (Axis3D.defaultSpec scale)
            c, s, aSig)

    let cbSpec = ColorbarSpec3D.vertical (Bounds.create 420.0 40.0 16.0 180.0) "Z" (-1.0, 1.0) ramp
    let container = win.Host.Document.createElement "div"
    win.Host.Document.body.appendChild container |> ignore

    // Part A: ShowHoverSpikes = false and OnHoverHit = None -> 0 hover GPU picks on mouse move; child <input> blocks drag/capture; click still picks
    let clickOnlyConfig =
        PlotViewConfig3D.create 480 360 cam
        |> PlotViewConfig3D.withHoverSpikes false
        |> PlotViewConfig3D.withOnClickHit clickHits.Add

    let mounted1 =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            container
            None
            (fun ctx ->
                Program.run
                    (PlotView3D.create win wgpu clickOnlyConfig (fun () ->
                        PlotPass3D.ofLayers
                            axisSpecSig.Value
                            [
                                Layer3D.surface surfBatch
                                Layer3D.colorbar cbSpec
                            ]))
                    ctx.Backend)

    pumpFrames win 2
    Expect.isNotNull (container.querySelector "[data-d5-key='legend3d/colorbar/bar']") "PlotPass3D.ofLayers mounted colorbar SVG bar"

    let picksBeforeHoverMove = PlotView3DStats.PickCalls
    win.Host.Mouse (0, 240, 180, 0, 0, 0) |> ignore
    win.Host.Mouse (0, 245, 182, 0, 0, 0) |> ignore
    pumpFrames win 2
    Expect.equal PlotView3DStats.PickCalls picksBeforeHoverMove "click-only PlotView3D (ShowHoverSpikes=false, OnHoverHit=None) issues 0 hover GPU picks"

    // Child <input> pointerdown + move must NOT start dragging or orbit camera
    let plotRoot = container.querySelector "[tabindex='0']"
    let childInput = win.Host.Document.createElement "input"
    childInput.setAttribute ("style", "position:absolute;left:10px;top:10px;width:120px;height:40px;")
    plotRoot.appendChild childInput |> ignore
    pumpFrames win 1
    let camBeforeInputDrag = graph.Run (fun () -> cam.Value)
    win.Host.Mouse (1, 40, 25, 0, 1, 0) |> ignore
    win.Host.Mouse (0, 320, 210, 0, 0, 0) |> ignore
    win.Host.Mouse (2, 320, 210, 0, 1, 0) |> ignore
    pumpFrames win 1
    Expect.equal (graph.Run (fun () -> cam.Value)) camBeforeInputDrag "pointerdown on child <input> does not start camera drag"
    plotRoot.removeChild childInput |> ignore

    // Primary click still fires OnClickHit and picks static PickId 101u from PlotPass3D.ofLayers
    clickHits.Clear ()
    win.Host.Mouse (1, 240, 180, 0, 1, 0) |> ignore
    win.Host.Mouse (2, 240, 180, 0, 1, 0) |> ignore
    pumpFrames win 2
    let clickHit =
        clickHits
        |> Seq.tryLast
        |> Option.bind (function ValueSome h -> Some h | ValueNone -> None)
        |> Option.defaultWith (fun () -> failtest "Expected OnClickHit on PlotPass3D.ofLayers surface")
    Expect.equal clickHit.PickId 101u "click-only plot still executes OnClickHit pick"

    mounted1.Dispose ()
    container.innerHTML <- ""

    // Part B: Switching AxisSpec clears spikeLineBatch immediately so prior spikes do not persist across scene switch
    let spikesConfig =
        PlotViewConfig3D.create 480 360 cam
        |> PlotViewConfig3D.withHoverSpikes true

    let mounted2 =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            container
            None
            (fun ctx ->
                Program.run
                    (PlotView3D.create win wgpu spikesConfig (fun () -> PlotPass3D.ofLayers axisSpecSig.Value [ Layer3D.surface surfBatch ]))
                    ctx.Backend)

    pumpFrames win 2
    let centerPt = Scale3D.map scale (Point3.create 0.0 0.0 0.5)
    let centerScr = Camera3D.projectToScreen wgpu.Width wgpu.Height (PlotCamera3D.toCamera3D scale PlotCamera3D.defaultValue) centerPt |> ValueOption.get
    let struct (winPxW, winPxH) = win.PixelSize
    let hoverCssX = int (MathF.Round (centerScr.X / float32 winPxW * 480.0f))
    let hoverCssY = int (MathF.Round (centerScr.Y / float32 winPxH * 360.0f))

    let pixelsBeforeSpikes, _, _ = readWindowPixels win
    win.Host.Mouse (0, hoverCssX, hoverCssY, 0, 0, 0) |> ignore
    pumpFrames win 2
    let pixelsWithSpikes, _, _ = readWindowPixels win
    Expect.isFalse (pixelsBeforeSpikes = pixelsWithSpikes) "hovering draws spikeLineBatch before AxisSpec switch"

    // Switch AxisSpec to a fresh reference with identical framing -> stale spikeLineBatch must clear back to baseline pixels!
    graph.Run (fun () -> axisSpecSig.Value <- Axis3D.defaultSpec scale)
    pumpFrames win 2
    let pixelsAfterSwitch, _, _ = readWindowPixels win
    Expect.equal pixelsAfterSwitch pixelsBeforeSpikes "switching AxisSpec reference clears prior spikeLineBatch pixels"

    mounted2.Dispose ()
    win.Host.Cycle ()
    graph.Dispose ()
    wgpu.Dispose ()
    (win :> IDisposable).Dispose ()

let private runSubplotGridChecks () =
    // 1. Validate SubplotGrid3D.grid layout math & validation guards
    let grid4 = SubplotGrid3D.grid 2 2 8 480 360
    Expect.equal grid4.Count 4 "SubplotGrid3D.grid 2x2 produces 4 viewports"
    Expect.equal grid4[0] (ViewportRect.create 0 0 236 176) "grid cell (0,0)"
    Expect.equal grid4[1] (ViewportRect.create 244 0 236 176) "grid cell (0,1)"
    Expect.equal grid4[2] (ViewportRect.create 0 184 236 176) "grid cell (1,0)"
    Expect.equal grid4[3] (ViewportRect.create 244 184 236 176) "grid cell (1,1)"
    Expect.throwsT<ArgumentOutOfRangeException> (fun () -> SubplotGrid3D.grid 0 2 8 480 360 |> ignore) "rejects rows <= 0"
    Expect.throwsT<ArgumentOutOfRangeException> (fun () -> SubplotGrid3D.grid 2 2 -1 480 360 |> ignore) "rejects gapPx < 0"

    // 2. Live 1x2 multi-pass grid over a SINGLE shared WgpuContext + SDL window
    let win = Window.Create ("D5WgpuSubplotGrid", 480, 360, hidden = true)
    let wgpu = WgpuSdlSurface.CreateContext win
    let graph = new Graph ()

    let scale0 = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
    let scale1 = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)

    let surfGrid0 =
        SurfaceGrid.fromFunction3D 14 14 (-0.9, 0.9) (-0.9, 0.9) (fun x y -> 0.5 * cos (x * 2.0) * sin (y * 2.0))
        |> unwrapOk "subplotGrid0"

    let surfGrid1 =
        SurfaceGrid.fromFunction3D 14 14 (-0.9, 0.9) (-0.9, 0.9) (fun x y -> 0.6 * sin (x * 2.5 + y * 2.5))
        |> unwrapOk "subplotGrid1"

    let clickHits0 = ResizeArray<PickHit voption>()
    let clickHits1 = ResizeArray<PickHit voption>()
    let hoverHits0 = ResizeArray<PickHit voption>()
    let hoverHits1 = ResizeArray<PickHit voption>()

    let cam0, cam1, surf0, surf1 =
        graph.Run (fun () ->
            let c0 = Reactive3D.editableCamera (fun () -> PlotCamera3D.defaultValue)
            let c1 = Reactive3D.editableCamera (fun () -> PlotCamera3D.defaultValue)

            let spec0 =
                { SurfaceSpec.create scale0 (ColorRamp.viridis (-0.6, 0.6)) with
                    PickId = 0x0A000001u
                    Shading = ShadingMode.Unlit
                }

            let spec1 =
                { SurfaceSpec.create scale1 (ColorRamp.turbo (-0.6, 0.6)) with
                    PickId = 0x0B000002u
                    Shading = ShadingMode.Unlit
                }

            let s0 = SurfaceBatch.Create (wgpu, spec0, surfGrid0)
            let s1 = SurfaceBatch.Create (wgpu, spec1, surfGrid1)
            Api.onCleanup (fun () -> s0.Dispose (); s1.Dispose ())
            c0, c1, s0, s1)

    let pass0 = PlotPass3D.ofLayers (Axis3D.defaultSpec scale0) [ Layer3D.surface surf0 ]
    let pass1 = PlotPass3D.ofLayers (Axis3D.defaultSpec scale1) [ Layer3D.surface surf1 ]

    let viewports = SubplotGrid3D.grid 1 2 10 480 360

    let cells =
        [
            SubplotCell3D.create viewports[0] cam0 (fun () -> pass0)
            |> SubplotCell3D.withOnHoverHit hoverHits0.Add
            |> SubplotCell3D.withOnClickHit clickHits0.Add

            SubplotCell3D.create viewports[1] cam1 (fun () -> pass1)
            |> SubplotCell3D.withOnHoverHit hoverHits1.Add
            |> SubplotCell3D.withOnClickHit clickHits1.Add
        ]

    let container = win.Host.Document.createElement "div"
    win.Host.Document.body.appendChild container |> ignore

    let mounted =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            container
            None
            (fun ctx -> Program.run (SubplotGrid3D.create win wgpu 480 360 cells) ctx.Backend)

    pumpFrames win 3

    // Verify rendered non-backdrop pixels strictly exist inside BOTH left (cell 0) and right (cell 1) viewports, while center gap remains backdrop
    let pixels, pw, ph = readWindowPixels win
    let docEl = win.Host.Document.documentElement
    let dprX = if not (isNull docEl) && docEl.clientWidth > 0 then float pw / float docEl.clientWidth else 1.0
    let mutable leftNonBg = 0
    let mutable rightNonBg = 0
    let br = int (MathF.Round (RgbaColor.darkBackdrop.R * 255.0f))
    let bg = int (MathF.Round (RgbaColor.darkBackdrop.G * 255.0f))
    let bb = int (MathF.Round (RgbaColor.darkBackdrop.B * 255.0f))
    let splitXLeft = int (Math.Round (235.0 * dprX))
    let splitXRight = int (Math.Round (245.0 * dprX))

    for y = 0 to ph - 1 do
        for x = 0 to pw - 1 do
            let idx = (y * pw + x) * 4
            let isNonBg = abs (int pixels[idx] - br) > 12 || abs (int pixels[idx + 1] - bg) > 12 || abs (int pixels[idx + 2] - bb) > 12

            if isNonBg then
                if x < splitXLeft then leftNonBg <- leftNonBg + 1
                elif x >= splitXRight then rightNonBg <- rightNonBg + 1

    Expect.isGreaterThan leftNonBg 500 "SubplotGrid3D renders left viewport pixels"
    Expect.isGreaterThan rightNonBg 500 "SubplotGrid3D renders right viewport pixels"

    // Verify each cell's SVG overlay div (both main and extra) is positioned at its ViewportRect and scales reactively on CSS resize
    let cellOverlays = container.querySelectorAll "[data-subplot-cell]"
    let extraOverlays = container.querySelectorAll "[data-subplot-extra]"
    Expect.equal cellOverlays.length 2u "Each subplot cell mounts an SVG overlay host"
    Expect.equal extraOverlays.length 2u "Each subplot cell mounts an extra SVG overlay host"
    let style0 = (cellOverlays.item 0u :?> Element).getAttribute "style"
    let style1 = (cellOverlays.item 1u :?> Element).getAttribute "style"
    let extraStyle1 = (extraOverlays.item 1u :?> Element).getAttribute "style"
    Expect.stringContains style0 "left:0px" "Cell 0 SVG overlay offset at left:0px"
    Expect.stringContains style1 "left:245px" "Cell 1 SVG overlay offset at left:245px"
    Expect.stringContains extraStyle1 "left:245px" "Cell 1 extra SVG overlay offset at left:245px"

    let plotRoot = container.querySelector "[tabindex='0']" :?> HTMLElement
    plotRoot.setAttribute ("style", "position:relative;width:960px;height:720px;overflow:hidden;outline:none;user-select:none;")
    graph.Run (fun () ->
        let c = cam0.Value
        cam0.Value <- { c with AzimuthRad = c.AzimuthRad + 0.01f })
    pumpFrames win 3

    let scaledCell0 = (cellOverlays.item 0u :?> Element).getAttribute "style"
    let scaledCell1 = (cellOverlays.item 1u :?> Element).getAttribute "style"
    let scaledExtra0 = (extraOverlays.item 0u :?> Element).getAttribute "style"
    let scaledExtra1 = (extraOverlays.item 1u :?> Element).getAttribute "style"
    Expect.stringContains scaledCell0 "width:470px;height:720px" "Resized cell 0 overlay width/height scaled to 470x720"
    Expect.stringContains scaledCell1 "left:490px;top:0px;width:470px;height:720px" "Resized cell 1 overlay rect scaled to left:490px, 470x720"
    Expect.stringContains scaledExtra0 "width:470px;height:720px" "Resized cell 0 extra overlay width/height scaled to 470x720"
    Expect.stringContains scaledExtra1 "left:490px;top:0px;width:470px;height:720px" "Resized cell 1 extra overlay rect scaled to left:490px, 470x720"

    // Restore 480x360 CSS dimensions for subsequent pixel/pick routing checks below
    plotRoot.setAttribute ("style", "position:relative;width:480px;height:360px;overflow:hidden;outline:none;user-select:none;")
    graph.Run (fun () -> cam0.Reset ())
    pumpFrames win 3
    Expect.stringContains ((cellOverlays.item 1u :?> Element).getAttribute "style") "left:245px;top:0px;width:235px;height:360px" "Cell 1 overlay restored to 480x360 layout"

    // Verify independent pointer orbit/wheel routing per cell
    let cam0Initial = graph.Run (fun () -> cam0.Value)
    let cam1Initial = graph.Run (fun () -> cam1.Value)

    win.Host.Mouse (1, 110, 180, 0, 1, 0) |> ignore
    win.Host.Mouse (0, 145, 195, 0, 1, 0) |> ignore
    win.Host.Mouse (2, 145, 195, 0, 1, 0) |> ignore
    win.Host.Wheel (110, 180, 0.0f, -120.0f, 0) |> ignore
    pumpFrames win 2

    let cam0AfterCell0 = graph.Run (fun () -> cam0.Value)
    let cam1AfterCell0 = graph.Run (fun () -> cam1.Value)
    Expect.notEqual cam0AfterCell0 cam0Initial "Cell 0 orbit+wheel mutates cam0"
    Expect.equal cam1AfterCell0 cam1Initial "Cell 0 interaction leaves cam1 untouched"

    win.Host.Mouse (1, 360, 180, 0, 1, 0) |> ignore
    win.Host.Mouse (0, 395, 165, 0, 1, 0) |> ignore
    win.Host.Mouse (2, 395, 165, 0, 1, 0) |> ignore
    win.Host.Wheel (360, 180, 0.0f, -120.0f, 0) |> ignore
    pumpFrames win 2

    let cam0AfterCell1 = graph.Run (fun () -> cam0.Value)
    let cam1AfterCell1 = graph.Run (fun () -> cam1.Value)
    Expect.equal cam0AfterCell1 cam0AfterCell0 "Cell 1 interaction leaves cam0 untouched"
    Expect.notEqual cam1AfterCell1 cam1AfterCell0 "Cell 1 orbit+wheel mutates cam1"

    // Verify click pick routing exclusively to the clicked cell via PickPasses WITHOUT firing OnHoverHit
    clickHits0.Clear ()
    clickHits1.Clear ()
    hoverHits0.Clear ()
    hoverHits1.Clear ()
    win.Host.Mouse (1, 117, 180, 0, 1, 0) |> ignore
    win.Host.Mouse (2, 117, 180, 0, 1, 0) |> ignore
    pumpFrames win 2
    let h0 = clickHits0 |> Seq.tryLast |> Option.bind (function ValueSome x -> Some x | _ -> None)
    Expect.equal (h0 |> Option.map (fun h -> h.PickId)) (Some 0x0A000001u) "Click in cell 0 hits cell 0 PickId"
    Expect.equal clickHits1.Count 0 "Click in cell 0 does not fire cell 1 OnClickHit"
    Expect.equal hoverHits0.Count 0 "Click in cell 0 does not invoke OnHoverHit"

    clickHits0.Clear ()
    hoverHits0.Clear ()
    hoverHits1.Clear ()
    win.Host.Mouse (1, 362, 180, 0, 1, 0) |> ignore
    win.Host.Mouse (2, 362, 180, 0, 1, 0) |> ignore
    pumpFrames win 2
    let h1 = clickHits1 |> Seq.tryLast |> Option.bind (function ValueSome x -> Some x | _ -> None)
    Expect.equal (h1 |> Option.map (fun h -> h.PickId)) (Some 0x0B000002u) "Click in cell 1 hits cell 1 PickId"
    Expect.equal clickHits0.Count 0 "Click in cell 1 does not fire cell 0 OnClickHit"
    Expect.equal hoverHits1.Count 0 "Click in cell 1 does not invoke OnHoverHit"

    mounted.Dispose ()
    mounted.Dispose ()
    win.Host.Cycle ()
    graph.Dispose ()
    wgpu.Dispose ()
    (win :> IDisposable).Dispose ()

[<EntryPoint>]
let main _ =
    runNativeAcceptance ()
    runSubRectDprAndKeyMapChecks ()
    runLayersAndPolishChecks ()
    runSubplotGridChecks ()
    printfn "Ranvier.D5.Wgpu.Native.Tests passed: PlotView3D, SubplotGrid3D multi-pass, DOM overlays, DPR resize, PlotPass3D.ofLayers, B0 polish, and safe idempotent disposal."
    0

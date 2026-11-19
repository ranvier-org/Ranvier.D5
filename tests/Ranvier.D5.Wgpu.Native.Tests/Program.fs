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
                {
                    AxisSpec = axisSpec
                    ClearColor = RgbaColor.darkBackdrop
                    Lighting = ValueSome LightingRig.studioDefault
                    SectionPlane = ValueNone
                    ExtraBatches = batches
                    ExtraLines = ReadOnlyMemory.Empty
                    ExtraDiscs = ReadOnlyMemory.Empty
                    ExtraOverlayMarks = []
                }

            let passSig = Api.createSignal pass
            stepSig, gSig, cam, surf, mesh, sink, batches, passSig)

    ignore surfaceBatch
    ignore sphereMesh
    ignore batchesMem

    let config: PlotViewConfig3D =
        {
            Width = 480
            Height = 360
            Camera = camera
            OnHoverHit = Some hoverHits.Add
            OnClickHit = Some clickHits.Add
        }

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

    // Review Focus #5: idempotent double mounted.Dispose(), then graph.Dispose(), wgpu.Dispose(), win.Dispose()
    mounted.Dispose ()
    mounted.Dispose ()
    win.Host.Cycle ()
    graph.Dispose ()
    wgpu.Dispose ()
    (win :> IDisposable).Dispose ()

[<EntryPoint>]
let main _ =
    runNativeAcceptance ()
    printfn "Ranvier.D5.Wgpu.Native.Tests passed: PlotView3D live wave, DOM identity, 0 mark wheel listeners, idle RAF, hover/click Probe3D, and safe idempotent disposal."
    0

module Ranvier.D5.Wgpu.LayoutsDemo.Program

open System
open System.Diagnostics
open System.IO
open System.Numerics
open Ranvier
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.D5.Wgpu.Sdl
open Ranvier.Dom
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.Dsl
open Ranvier.View.Representation
open Ranvier.View.WebCore.Sdl
open Ranvier.Wgpu
open Ranvier.Wgpu.Sdl

[<Literal>]
let private ViewportW = 960

[<Literal>]
let private ViewportH = 680

let private writeBitmap (path: string) (width: int) (height: int) (rgba: byte array) =
    let pixels = Array.copy rgba

    // BMP stores bottom-up BGRA; ReadPixelsInto supplies top-down RGBA.
    for offset in 0 .. 4 .. pixels.Length - 4 do
        let red = pixels[offset]
        pixels[offset] <- pixels[offset + 2]
        pixels[offset + 2] <- red

    let fullPath = Path.GetFullPath path
    let dir = Path.GetDirectoryName fullPath

    if not (String.IsNullOrEmpty dir) then
        Directory.CreateDirectory dir |> ignore

    use stream = File.Create fullPath
    use writer = new BinaryWriter (stream)
    writer.Write [| byte 'B'; byte 'M' |]
    writer.Write (54 + pixels.Length)
    writer.Write 0
    writer.Write 54
    writer.Write 40
    writer.Write width
    writer.Write height
    writer.Write 1s
    writer.Write 32s
    writer.Write 0
    writer.Write pixels.Length
    for _ in 1..4 do writer.Write 0

    for row in height - 1 .. -1 .. 0 do
        writer.Write (pixels, row * width * 4, width * 4)

let private countNonBackdropPixels (backdrop: RgbaColor) (pixels: byte array) : int =
    let br = int (MathF.Round (backdrop.R * 255.0f))
    let bg = int (MathF.Round (backdrop.G * 255.0f))
    let bb = int (MathF.Round (backdrop.B * 255.0f))
    let mutable nonBackdrop = 0

    for i in 0 .. 4 .. pixels.Length - 4 do
        if abs (int pixels[i] - br) > 12 || abs (int pixels[i + 1] - bg) > 12 || abs (int pixels[i + 2] - bb) > 12 then
            nonBackdrop <- nonBackdrop + 1

    nonBackdrop

type private DemoSession =
    {
        Mounted: Mounting.Mounted
        Graph: Graph
        Wgpu: WgpuContext
        Window: Window
        Stage: LayoutsStage3D
        ActiveScene: Signal<LayoutSceneId>
        Camera: Editable<PlotCamera3D>
        Section: Editable<SectionPlane>
        FrameTick: Signal<float>
        Paused: Signal<bool>
        Readout: Signal<string>
    }
    interface IDisposable with
        member this.Dispose() =
            this.Mounted.Dispose ()
            this.Graph.Dispose ()
            this.Wgpu.Dispose ()
            (this.Window :> IDisposable).Dispose ()

let private createDemo (hidden: bool) (initialScene: LayoutSceneId) (width: int) (height: int) : DemoSession =
    let win = Window.Create ("Ranvier.D5.Wgpu — 0.3.0 3D Layouts Showcase", width, height, hidden = hidden)
    let wgpu = WgpuSdlSurface.CreateContext win
    let graph = new Graph ()

    let root = win.Host.Document.createElement "div"
    win.Host.Document.body.setAttribute ("style", "margin:0;padding:0;overflow:hidden;background:#04060b;")
    win.Host.Document.body.appendChild root |> ignore

    let activeScene, camera, section, frameTick, paused, readoutSig, stage =
        graph.Run (fun () ->
            let sc = Api.createSignal initialScene
            let cam = Reactive3D.editableCamera (fun () -> PlotCamera3D.defaultValue)
            let sec = Reactive3D.editableSection (fun () -> SectionPlane.disabled)
            let tick = Api.createSignal 0.0
            let isPaused = Api.createSignal false
            let readout = Api.createSignal "Drag orbit | Shift+Drag pan | Wheel zoom | 1-4 scenes | [ / ] scrub | Click mark"
            let stg = new LayoutsStage3D (wgpu)
            Api.onCleanup (fun () -> (stg :> IDisposable).Dispose ())
            sc, cam, sec, tick, isPaused, readout, stg)

    let focusPlot () =
        match root.querySelector "[tabindex='0']" with
        | null -> ()
        | el -> (el :?> HTMLElement).focus ()

    let switchScene (sc: LayoutSceneId) =
        graph.Run (fun () ->
            activeScene.Value <- sc
            readoutSig.Value <- LayoutSceneId.title sc
            frameTick.Value <- frameTick.Value + 1e-4)

        focusPlot ()

    let toggleSection () =
        graph.Run (fun () ->
            let cur = section.Value

            if cur.Enabled then
                section.Value <- SectionPlane.disabled
            else
                section.Value <- SectionPlane.create (Vector3.Normalize (Vector3 (1.0f, 0.2f, 0.35f))) 0.0f)

        focusPlot ()

    let togglePerspective () =
        graph.Run (fun () ->
            let cam = camera.Value
            let nextP = if cam.Perspectiveness > 0.5f then 0.0f else 1.0f
            camera.Value <- { cam with Perspectiveness = nextP })

        focusPlot ()

    let toggleOrbitMode () =
        graph.Run (fun () ->
            let cam = camera.Value

            let nextM =
                match cam.Mode with
                | OrbitMode3D.Turntable -> OrbitMode3D.Arcball
                | OrbitMode3D.Arcball -> OrbitMode3D.Turntable

            camera.Value <- PlotCamera3D.withMode nextM cam)

        focusPlot ()

    let scrub (delta: float) =
        graph.Run (fun () ->
            let msg = stage.ScrubScene (activeScene.Value, delta)
            readoutSig.Value <- msg
            frameTick.Value <- frameTick.Value + 1e-4)

        focusPlot ()

    let togglePause () =
        graph.Run (fun () -> paused.Value <- not paused.Value)
        focusPlot ()

    let resetAll () =
        graph.Run (fun () ->
            camera.Reset ()
            section.Reset ()
            stage.ResetScene activeScene.Value
            readoutSig.Value <- "Reset camera, section & layout parameters"
            frameTick.Value <- frameTick.Value + 1e-4)

        focusPlot ()

    let onHover (hit: PickHit voption) =
        match hit with
        | ValueSome h when h.PickId <> 0u ->
            readoutSig.Value <- stage.DescribeHit (activeScene.Value, h)
        | _ -> ()

    let onClick (hit: PickHit voption) =
        match hit with
        | ValueSome h when h.PickId <> 0u ->
            readoutSig.Value <- stage.ClickHit (activeScene.Value, h)
            frameTick.Value <- frameTick.Value + 1e-4
        | _ -> ()

    let readScene () : PlotPass3D =
        let _ = frameTick.Value
        let sc = activeScene.Value
        let sec = section.Value
        let pass = stage.BuildPass (sc, sec)

        let titleMark =
            Mark.text "hud/title" (Point.create 18.0 22.0) (LayoutSceneId.title sc)
            |> Mark.withFill (Paint.rgb 248 250 252)
            |> Mark.withFontSize 13.0

        let secSuffix = if sec.Enabled then $" | Sec={sec.Offset:F2}" else ""

        let statusMark =
            Mark.text "hud/status" (Point.create 18.0 40.0) $"{pass.Subtitle}{secSuffix} | {readoutSig.Value}"
            |> Mark.withFill (Paint.rgb 148 163 184)
            |> Mark.withFontSize 11.0

        {
            AxisSpec = pass.AxisSpec
            ClearColor = RgbaColor.darkBackdrop
            Lighting = ValueSome LightingRig.studioDefault
            SectionPlane = pass.Section
            ExtraBatches = pass.Batches
            ExtraLines = pass.Lines
            ExtraDiscs = pass.Discs
            ExtraOverlayMarks = [ titleMark; statusMark ]
        }

    let toolbarHeight = 34
    let plotHeight = max 240 (height - toolbarHeight)

    let plotConfig: PlotViewConfig3D =
        {
            Width = width
            Height = plotHeight
            Camera = camera
            OnHoverHit = Some onHover
            OnClickHit = Some onClick
        }

    let btnStyle (isActive: bool) =
        let bg = if isActive then "rgba(14,116,144,0.92)" else "rgba(15,23,42,0.90)"
        let border = if isActive then "rgba(56,189,248,0.90)" else "rgba(51,65,85,0.85)"
        $"padding:3px 8px;font:500 11px system-ui,sans-serif;color:#f1f5f9;background:{bg};border:1px solid {border};border-radius:4px;cursor:pointer;white-space:nowrap;"

    let toolbarButton (label: unit -> string) (isActive: unit -> bool) (onPress: unit -> unit) =
        button<unit>(
            [|
                button.style (fun () -> btnStyle (isActive ()))
                button.onClick (fun () _ -> onPress ())
            |]
        )
            .Run [| text (fun () -> label ()) |]

    let shellComponent =
        Component.template
            (fun _ _ -> ())
            (div<unit>(
                [|
                    div.style $"position:relative;width:{width}px;height:{height}px;display:flex;flex-direction:column;overflow:hidden;outline:none;background:#04060b;"
                    div.onKeydown (fun () ev ->
                        match ev.key with
                        | "1" -> switchScene LayoutSceneId.PoincareH3
                        | "2" -> switchScene LayoutSceneId.CodeCityTreemap3D
                        | "3" -> switchScene LayoutSceneId.SphericalBundling3D
                        | "4" -> switchScene LayoutSceneId.ManifoldClusterBvh3D
                        | " " ->
                            ev.preventDefault ()
                            togglePause ()
                        | "s" | "S" -> toggleSection ()
                        | "[" -> scrub -1.0
                        | "]" -> scrub 1.0
                        | "r" | "R" | "Home" -> resetAll ()
                        | _ -> ())
                |]
            )
                .Run
                [|
                    view (fun () -> PlotView3D.create win wgpu plotConfig readScene)
                    div<unit>(
                        [|
                            div.style $"width:{width}px;height:{toolbarHeight}px;padding:0 10px;display:flex;flex-wrap:nowrap;justify-content:center;gap:5px;align-items:center;background:#070b14;border-top:1px solid rgba(51,65,85,0.72);box-sizing:border-box;"
                        |]
                    )
                        .Run
                        [|
                            for sc in LayoutSceneId.all do
                                toolbarButton
                                    (fun () -> LayoutSceneId.shortLabel sc)
                                    (fun () -> activeScene.Value = sc)
                                    (fun () -> switchScene sc)

                            toolbarButton
                                (fun () -> if paused.Value then "Play (Space)" else "Pause (Space)")
                                (fun () -> paused.Value)
                                togglePause

                            toolbarButton
                                (fun () -> "Sec (S)")
                                (fun () -> section.Value.Enabled)
                                toggleSection

                            toolbarButton
                                (fun () -> if camera.Value.Perspectiveness > 0.5f then "Persp (P)" else "Ortho (P)")
                                (fun () -> camera.Value.Perspectiveness > 0.5f)
                                togglePerspective

                            toolbarButton
                                (fun () -> if camera.Value.Mode = OrbitMode3D.Arcball then "Arc (M)" else "Turn (M)")
                                (fun () -> camera.Value.Mode = OrbitMode3D.Arcball)
                                toggleOrbitMode

                            toolbarButton (fun () -> "− ([)") (fun () -> false) (fun () -> scrub -1.0)
                            toolbarButton (fun () -> "+ (])") (fun () -> false) (fun () -> scrub 1.0)
                            toolbarButton (fun () -> "Reset (R)") (fun () -> false) resetAll
                        |]
                |])

    let mounted =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            root
            None
            (fun ctx -> Program.run (Component.render shellComponent ()) ctx.Backend)

    {
        Mounted = mounted
        Graph = graph
        Wgpu = wgpu
        Window = win
        Stage = stage
        ActiveScene = activeScene
        Camera = camera
        Section = section
        FrameTick = frameTick
        Paused = paused
        Readout = readoutSig
    }

let private poseSceneForCapture (demo: DemoSession) (scene: LayoutSceneId) =
    let baseAz = PlotCamera3D.defaultValue.AzimuthRad

    demo.Graph.Run (fun () ->
        demo.ActiveScene.Value <- scene
        demo.Section.Value <- SectionPlane.disabled

        match scene with
        | LayoutSceneId.PoincareH3 ->
            demo.Stage.StepPoincare 0.72
            demo.Camera.Value <-
                { PlotCamera3D.defaultValue with
                    AzimuthRad = baseAz + 0.25f
                    ElevationRad = 0.34f
                    Perspectiveness = 0.28f
                    Zoom = 1.06f
                }
        | LayoutSceneId.CodeCityTreemap3D ->
            demo.Stage.StepCodeCity (0.58, 1.0)
            demo.Camera.Value <-
                { PlotCamera3D.defaultValue with
                    AzimuthRad = baseAz + 0.18f
                    ElevationRad = 0.44f
                    Perspectiveness = 0.30f
                    Zoom = 1.06f
                }
        | LayoutSceneId.SphericalBundling3D ->
            demo.Stage.StepSphericalBundling (0.85, 1.45)
            demo.Camera.Value <-
                { PlotCamera3D.defaultValue with
                    AzimuthRad = baseAz + 0.35f
                    ElevationRad = 0.36f
                    Perspectiveness = 0.28f
                    Zoom = 1.08f
                }
        | LayoutSceneId.ManifoldClusterBvh3D ->
            demo.Stage.SetDbscanEpsilon 0.17
            demo.Stage.StepManifoldRay 0.75
            demo.Camera.Value <-
                { PlotCamera3D.defaultValue with
                    AzimuthRad = baseAz + 0.28f
                    ElevationRad = 0.38f
                    Perspectiveness = 0.30f
                    Zoom = 1.08f
                }

        demo.FrameTick.Value <- demo.FrameTick.Value + 1.0)

let private capturePoseToBmp (demo: DemoSession) (scene: LayoutSceneId) (outputPath: string) =
    let sw = Stopwatch.StartNew ()
    poseSceneForCapture demo scene

    // Two warmup frames + final frame ensure PlotView3D RAF and swapchain settle cleanly.
    for _ = 1 to 3 do
        demo.Window.Host.Cycle ()
        demo.Window.Host.Frame ()
        demo.Window.Present ()

    let struct (pw, ph) = demo.Window.PixelSize
    let pixels = Array.zeroCreate<byte> (pw * ph * 4)
    let struct (rw, rh) = demo.Window.ReadPixelsInto (Span<byte>(pixels))
    let nonBackdrop = countNonBackdropPixels RgbaColor.darkBackdrop pixels

    if nonBackdrop <= 5000 then
        failwithf "LayoutsDemo capture for %A produced only %d non-backdrop pixels (expected > 5000)." scene nonBackdrop

    writeBitmap outputPath rw rh pixels
    printfn "Captured %A (%dx%d, %d non-backdrop pixels, %.1f ms) -> %s" scene rw rh nonBackdrop sw.Elapsed.TotalMilliseconds outputPath

let private captureSingle (scene: LayoutSceneId) (outputPath: string) =
    use demo = createDemo true scene ViewportW ViewportH
    capturePoseToBmp demo scene outputPath

let private captureAll (outDir: string) =
    let totalSw = Stopwatch.StartNew ()
    use demo = createDemo true LayoutSceneId.PoincareH3 ViewportW ViewportH

    for sc in LayoutSceneId.all do
        let outPath = Path.Combine (outDir, LayoutSceneId.captureFileName sc)
        capturePoseToBmp demo sc outPath

    printfn "Completed all 4 hidden-window captures in %.1f ms -> %s" totalSw.Elapsed.TotalMilliseconds (Path.GetFullPath outDir)

[<EntryPoint>]
let main args =
    match args with
    | [| "--capture"; sceneName; outBmp |] ->
        match LayoutSceneId.tryParse sceneName with
        | Some sc ->
            captureSingle sc outBmp
            0
        | None ->
            eprintfn "Unknown scene '%s'. Expected: h3 | codecity | spherical | manifold | 1..4." sceneName
            1
    | [| "--capture-all"; outDir |] ->
        captureAll outDir
        0
    | [||] ->
        use demo = createDemo false LayoutSceneId.PoincareH3 ViewportW ViewportH
        let clock = Stopwatch.StartNew ()

        while demo.Window.Step () do
            if not demo.Paused.Value then
                let t = clock.Elapsed.TotalSeconds

                demo.Graph.Run (fun () ->
                    demo.Stage.StepScene (demo.ActiveScene.Value, t)
                    demo.FrameTick.Value <- t)

        0
    | _ ->
        eprintfn "Usage: Ranvier.D5.Wgpu.LayoutsDemo [--capture <h3|codecity|spherical|manifold|1..4> <out.bmp> | --capture-all <outDir>]"
        1

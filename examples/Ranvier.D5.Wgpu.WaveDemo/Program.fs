module Ranvier.D5.Wgpu.WaveDemo.Program

open System
open System.Diagnostics
open System.IO
open System.Numerics
open Ranvier
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.D5.Wgpu.Sdl
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.Dsl
open Ranvier.View.Representation
open Ranvier.View.WebCore.Sdl
open Ranvier.Wgpu
open Ranvier.Wgpu.Sdl

[<Literal>]
let private GridN = 96

let private domainLo = -Math.PI * 1.5
let private domainHi = Math.PI * 1.5

let private unwrapOk msg =
    function
    | Ok value -> value
    | Error err -> failwithf "%s failed: %A" msg err

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

let private evalWave (xs: float array) (ys: float array) (t: float) (heights: float array) (velocities: float array) =
    let mutable maxZ = Double.NegativeInfinity
    let mutable minZ = Double.PositiveInfinity
    let mutable maxX, maxY, maxV = 0.0, 0.0, 0.0
    let mutable minX, minY, minV = 0.0, 0.0, 0.0

    for r = 0 to GridN - 1 do
        let y = ys[r]
        let y2 = y * y
        let rowBase = r * GridN

        for c = 0 to GridN - 1 do
            let x = xs[c]
            let rDist = sqrt (x * x + y2)
            let env = exp (-0.08 * rDist * rDist)
            let radialPhase = 1.6 * rDist - 2.6 * t
            let sx = 0.9 * x + 1.8 * t
            let sy = 0.9 * y - 1.3 * t

            let z = 0.72 * sin radialPhase * env + 0.35 * cos sx * sin sy
            // Independent 4th dimension: instantaneous vertical phase velocity dh/dt
            let v =
                -0.72 * 2.6 * cos radialPhase * env
                - 0.35 * (1.8 * sin sx * sin sy + 1.3 * cos sx * cos sy)

            let idx = rowBase + c
            heights[idx] <- z
            velocities[idx] <- v

            if z > maxZ then
                maxZ <- z
                maxX <- x
                maxY <- y
                maxV <- v

            if z < minZ then
                minZ <- z
                minX <- x
                minY <- y
                minV <- v

    struct (Point3.create maxX maxY maxZ, maxV, Point3.create minX minY minZ, minV)

let private makeExtremaMarker (pt: Point3) (col: RgbaColor) : GlyphDatum3D =
    {
        Position = pt
        Scale = Vector3 (0.11f, 0.11f, 0.11f)
        Rotation = Quaternion.Identity
        Color = col
        Shading = ShadingMode.Lit (0.25f, 0.18f)
        PickId = 0u
        Selected = false
    }

type private DemoSession =
    {
        Mounted: Mounting.Mounted
        Graph: Graph
        Wgpu: WgpuContext
        Window: Window
        StepWave: float -> unit
        Camera: Editable<PlotCamera3D>
        Section: Editable<SectionPlane>
    }
    interface IDisposable with
        member this.Dispose() =
            this.Mounted.Dispose ()
            this.Graph.Dispose ()
            this.Wgpu.Dispose ()
            (this.Window :> IDisposable).Dispose ()

let private createDemo (hidden: bool) (width: int) (height: int) : DemoSession =
    let win = Window.Create ("Ranvier.D5.Wgpu — Live 4D Wave Plane", width, height, hidden = hidden)
    let wgpu = WgpuSdlSurface.CreateContext win
    let graph = new Graph ()

    let scale = Scale3D.cartesian (domainLo, domainHi) (domainLo, domainHi) (-1.35, 1.35)
    let velocityRamp = ColorRamp.turbo (-2.8, 2.8)

    let xs = Array.init GridN (fun c -> domainLo + (domainHi - domainLo) * float c / float (GridN - 1))
    let ys = Array.init GridN (fun r -> domainLo + (domainHi - domainLo) * float r / float (GridN - 1))
    let heights = Array.zeroCreate<float> (GridN * GridN)
    let velocities = Array.zeroCreate<float> (GridN * GridN)

    let baseAxis = Axis3D.defaultSpec scale

    let axisSpec: Axis3DSpec =
        { baseAxis with
            X = { baseAxis.X with Title = "X (rad)" }
            Y = { baseAxis.Y with Title = "Y (rad)" }
            Z = { baseAxis.Z with Title = "Amplitude" }
        }

    let surfaceSpec: SurfaceSpec =
        {
            Scale = scale
            Color = velocityRamp
            Shading = ShadingMode.Lit (0.35f, 0.04f)
            Opacity = 1.0f
            Wireframe = ValueSome (RgbaColor (0.08f, 0.12f, 0.20f, 0.28f))
            PickId = 0x02000001u
        }

    let initialExtrema = evalWave xs ys 0.0 heights velocities

    let initialGrid =
        SurfaceGrid.uniform GridN GridN (domainLo, domainHi) (domainLo, domainHi) (ReadOnlySpan<float>(heights)) (ReadOnlySpan<float>(velocities))
        |> unwrapOk "initialWaveGrid"

    let camera, section, surfaceBatch, probeSink, batchesMem, frameTick, readoutSig, extremaColl =
        graph.Run (fun () ->
            let cam = Reactive3D.editableCamera (fun () -> PlotCamera3D.defaultValue)
            let sec = Reactive3D.editableSection (fun () -> SectionPlane.disabled)
            let surf = SurfaceBatch.Create (wgpu, surfaceSpec, initialGrid)
            Api.onCleanup (fun () -> surf.Dispose ())

            let sphereMesh = wgpu.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Sphere)
            Api.onCleanup (fun () -> sphereMesh.Dispose ())

            let extrema = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)

            let struct (peakPt, _, troughPt, _) = initialExtrema
            extrema.AddOrUpdate (struct ("peak", makeExtremaMarker peakPt (RgbaColor (1.0f, 0.88f, 0.25f, 1.0f))))
            extrema.AddOrUpdate (struct ("trough", makeExtremaMarker troughPt (RgbaColor (0.35f, 0.90f, 1.0f, 1.0f))))

            let rows = extrema.Rows |> Projection.map (fun struct (_, g) -> g)
            let sink = Reactive3D.bindInstances wgpu 1uy sphereMesh BlendMode.Opaque false false (fun () -> scale) rows
            let batches = ReadOnlyMemory<InstanceBatch> [| surf.Batch; sink.Batch |]
            let tick = Api.createSignal 0.0
            let readout = Api.createSignal "Phase velocity 4th-dim ColorRamp | Drag orbit | Wheel zoom | S section | R reset"
            cam, sec, surf, sink, batches, tick, readout, extrema)

    let linesBuf =
        match surfaceBatch.WireframeBatch with
        | Some wb -> ReadOnlyMemory<LineBatch> [| wb |]
        | None -> ReadOnlyMemory.Empty

    let onHover (hit: PickHit voption) =
        match hit with
        | ValueNone ->
            readoutSig.Value <- "Hover wave or extrema markers | Drag orbit | Shift+Drag pan | S section | R reset"
        | ValueSome h ->
            match Probe3D.resolveKeyed scale probeSink h with
            | ValueSome picked ->
                readoutSig.Value <-
                    $"Extremum [{picked.Key}]: x={picked.DataPoint.X:F2}, y={picked.DataPoint.Y:F2}, z={picked.DataPoint.Z:F2}"
            | ValueNone ->
                let pt = Probe3D.invertHit scale h
                readoutSig.Value <- $"Surface: x={pt.X:F2}, y={pt.Y:F2}, z={pt.Z:F2}"

    let readScene () : PlotPass3D =
        let t = frameTick.Value
        let sec = section.Value

        let badgeText =
            if sec.Enabled then
                $"t={t:F2}s | Section offset={sec.Offset:F2} | {readoutSig.Value}"
            else
                $"t={t:F2}s | Section off (S to toggle) | {readoutSig.Value}"

        let hudMark =
            Mark.text "hud/status" (Point.create 18.0 24.0) badgeText
            |> Mark.withFill (Paint.rgb 226 232 240)
            |> Mark.withFontSize 12.0

        {
            AxisSpec = axisSpec
            ClearColor = RgbaColor.darkBackdrop
            Lighting = ValueSome LightingRig.studioDefault
            SectionPlane = if sec.Enabled then ValueSome sec else ValueNone
            ExtraBatches = batchesMem
            ExtraLines = linesBuf
            ExtraDiscs = ReadOnlyMemory.Empty
            ExtraOverlayMarks = [ hudMark ]
        }

    let plotConfig: PlotViewConfig3D =
        {
            Width = width
            Height = height
            Camera = camera
            OnHoverHit = Some onHover
            OnClickHit = Some onHover
        }

    let shellComponent =
        Component.template
            (fun _ _ -> ())
            (div<unit>(
                [|
                    div.style $"position:relative;width:{width}px;height:{height}px;outline:none;"
                    div.onKeydown (fun _ ev ->
                        match ev.key with
                        | "s" | "S" ->
                            let cur = section.Value

                            if cur.Enabled then
                                section.Value <- SectionPlane.disabled
                            else
                                section.Value <- SectionPlane.create (Vector3.Normalize (Vector3 (1.0f, 0.25f, 0.35f))) 0.0f
                        | "[" ->
                            let cur = section.Value
                            let n = if cur.Enabled then cur.Normal else Vector3.Normalize (Vector3 (1.0f, 0.25f, 0.35f))
                            section.Value <- SectionPlane.create n (Math.Clamp (cur.Offset - 0.1f, -1.2f, 1.2f))
                        | "]" ->
                            let cur = section.Value
                            let n = if cur.Enabled then cur.Normal else Vector3.Normalize (Vector3 (1.0f, 0.25f, 0.35f))
                            section.Value <- SectionPlane.create n (Math.Clamp (cur.Offset + 0.1f, -1.2f, 1.2f))
                        | "r" | "R" | "Home" ->
                            camera.Reset ()
                            section.Reset ()
                        | _ -> ())
                |]
            )
                .Run [| view (fun () -> PlotView3D.create win wgpu plotConfig readScene) |])

    let root = win.Host.Document.createElement "div"
    win.Host.Document.body.appendChild root |> ignore

    let mounted =
        Mounting.mount
            graph
            None
            (fun errors -> raise (AggregateException errors))
            root
            None
            (fun ctx -> Program.run (Component.render shellComponent ()) ctx.Backend)

    let stepWave (t: float) =
        let struct (peakPt, _, troughPt, _) = evalWave xs ys t heights velocities

        graph.Run (fun () ->
            surfaceBatch.Update (surfaceSpec, ReadOnlySpan<float>(heights), ReadOnlySpan<float>(velocities))

            Api.batch (fun () ->
                extremaColl.AddOrUpdate (struct ("peak", makeExtremaMarker peakPt (RgbaColor (1.0f, 0.88f, 0.25f, 1.0f))))
                extremaColl.AddOrUpdate (struct ("trough", makeExtremaMarker troughPt (RgbaColor (0.35f, 0.90f, 1.0f, 1.0f))))
                frameTick.Value <- t))

    {
        Mounted = mounted
        Graph = graph
        Wgpu = wgpu
        Window = win
        StepWave = stepWave
        Camera = camera
        Section = section
    }

let private capture (outputPath: string) =
    use demo = createDemo true 960 680

    demo.Graph.Run (fun () ->
        demo.Section.Value <- SectionPlane.create (Vector3.Normalize (Vector3 (1.0f, 0.2f, 0.35f))) 0.35f)

    demo.StepWave 0.85

    for _ = 1 to 3 do
        demo.Window.Host.Cycle ()
        demo.Window.Host.Frame ()
        demo.Window.Present ()

    let struct (pw, ph) = demo.Window.PixelSize
    let pixels = Array.zeroCreate<byte> (pw * ph * 4)
    let struct (rw, rh) = demo.Window.ReadPixelsInto (Span<byte>(pixels))

    let br = int (MathF.Round (RgbaColor.darkBackdrop.R * 255.0f))
    let bg = int (MathF.Round (RgbaColor.darkBackdrop.G * 255.0f))
    let bb = int (MathF.Round (RgbaColor.darkBackdrop.B * 255.0f))
    let mutable nonBackdrop = 0

    for i in 0 .. 4 .. pixels.Length - 4 do
        if abs (int pixels[i] - br) > 12 || abs (int pixels[i + 1] - bg) > 12 || abs (int pixels[i + 2] - bb) > 12 then
            nonBackdrop <- nonBackdrop + 1

    if nonBackdrop < 5000 then
        failwithf "WaveDemo capture produced only %d non-backdrop pixels." nonBackdrop

    writeBitmap outputPath rw rh pixels
    printfn "Captured %dx%d wave frame (%d non-backdrop pixels) to %s" rw rh nonBackdrop outputPath

[<EntryPoint>]
let main args =
    if args.Length = 2 && args[0] = "--capture" then
        capture args[1]
    else
        use demo = createDemo false 960 680
        let clock = Stopwatch.StartNew ()

        while demo.Window.Step () do
            demo.StepWave clock.Elapsed.TotalSeconds

    0

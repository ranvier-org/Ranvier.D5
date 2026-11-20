module Ranvier.D5.Wgpu.Tests.ScaleAxisTests

open System
open System.Numerics
open Expecto
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.Wgpu

let private expectVec3Close (eps: float32) (actual: Vector3) (expected: Vector3) (msg: string) =
    let acc = { absolute = float eps; relative = 0.0 }
    Expect.floatClose acc (float actual.X) (float expected.X) ($"{msg} (X)")
    Expect.floatClose acc (float actual.Y) (float expected.Y) ($"{msg} (Y)")
    Expect.floatClose acc (float actual.Z) (float expected.Z) ($"{msg} (Z)")

let private expectPoint3Close (eps: float) (actual: Point3) (expected: Point3) (msg: string) =
    let acc = { absolute = eps; relative = 0.0 }
    Expect.floatClose acc actual.X expected.X ($"{msg} (X)")
    Expect.floatClose acc actual.Y expected.Y ($"{msg} (Y)")
    Expect.floatClose acc actual.Z expected.Z ($"{msg} (Z)")

[<Tests>]
let scaleAxisTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.ScaleAndAxis3D"
        [
            testCase "Scale3D round-trips invert(map p) across ZUp/YUp, linear, log, symlog, piecewise, timeUtc, and reversed domains"
            <| fun () ->
                for tolOverride, label, scale, pts in
                    [
                        None,
                        "ZUp tight 1e-5 linear/log/symlog/reversed",
                        Scale3D.create
                            BoxAspect.ChartDefault
                            UpAxis.ZUp
                            (AxisScale3D.linear (10.0, -10.0))
                            (AxisScale3D.log 10.0 (1.0, 100.0))
                            (AxisScale3D.symlog 2.0 (-10.0, 10.0)),
                        [
                            Point3.create 10.0 1.0 -10.0
                            Point3.create -2.5 10.0 0.0
                            Point3.create -10.0 100.0 10.0
                            Point3.create 3.25 31.6227766 -4.5
                        ]

                        None,
                        "YUp tight 1e-5 linear/log/symlog/reversed",
                        Scale3D.create
                            BoxAspect.Cube
                            UpAxis.YUp
                            (AxisScale3D.linear (10.0, -10.0))
                            (AxisScale3D.log 10.0 (1.0, 100.0))
                            (AxisScale3D.symlog 2.0 (-10.0, 10.0)),
                        [
                            Point3.create 10.0 1.0 -10.0
                            Point3.create 0.0 10.0 0.0
                            Point3.create -10.0 100.0 10.0
                            Point3.create -3.5 25.0 4.5
                        ]

                        Some 1e-4,
                        "ZUp wide linear/log/symlog",
                        Scale3D.create
                            BoxAspect.ChartDefault
                            UpAxis.ZUp
                            (AxisScale3D.linear (-5.0, 15.0))
                            (AxisScale3D.log 10.0 (1.0, 1000.0))
                            (AxisScale3D.symlog 2.0 (-100.0, 100.0)),
                        [
                            Point3.create -5.0 1.0 -100.0
                            Point3.create 2.5 10.0 0.0
                            Point3.create 15.0 1000.0 100.0
                            Point3.create 7.25 31.6227766 -12.5
                        ]

                        Some 1e-4,
                        "YUp reversed & piecewise",
                        Scale3D.create
                            BoxAspect.Cube
                            UpAxis.YUp
                            (AxisScale3D.linear (10.0, -10.0))
                            (AxisScale3D.piecewise [ 0.0; 10.0; 100.0 ] [ 0.0; 0.5; 1.0 ])
                            (AxisScale3D.log 10.0 (100.0, 1.0)),
                        [
                            Point3.create 10.0 0.0 100.0
                            Point3.create 0.0 5.0 10.0
                            Point3.create -10.0 55.0 1.0
                            Point3.create -3.5 100.0 25.0
                        ]

                        Some 500.0,
                        "ZUp TimeUtc & Manual",
                        Scale3D.create
                            (BoxAspect.Manual (3.0, 1.5, 2.25))
                            UpAxis.ZUp
                            (AxisScale3D.timeUtc (DateTimeOffset (2026, 1, 1, 0, 0, 0, TimeSpan.Zero), DateTimeOffset (2026, 1, 11, 0, 0, 0, TimeSpan.Zero)))
                            (AxisScale3D.linear (0.0, 20.0))
                            (AxisScale3D.linear (100.0, 0.0)),
                        [
                            Point3.create (float ((DateTimeOffset (2026, 1, 1, 0, 0, 0, TimeSpan.Zero)).ToUnixTimeMilliseconds ())) 0.0 100.0
                            Point3.create (float ((DateTimeOffset (2026, 1, 6, 12, 0, 0, TimeSpan.Zero)).ToUnixTimeMilliseconds ())) 10.0 50.0
                            Point3.create (float ((DateTimeOffset (2026, 1, 11, 0, 0, 0, TimeSpan.Zero)).ToUnixTimeMilliseconds ())) 20.0 0.0
                        ]
                    ] do
                    for p in pts do
                        let world = Scale3D.map scale p
                        let recovered = Scale3D.invert scale world
                        let tol = defaultArg tolOverride 1e-5
                        expectPoint3Close tol recovered p $"{label}: round-trip"

                // Verify explicit ZUp world axis mapping: (X, Y, Z) -> (wx, wz, -wy)
                let cart = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)
                expectVec3Close 1e-5f (Scale3D.map cart (Point3.create 10.0 0.0 0.0)) (Vector3 (1.0f, -2.0f / 3.0f, 1.0f)) "ZUp max X, min Y, min Z"
                expectVec3Close 1e-5f (Scale3D.map cart (Point3.create 0.0 10.0 10.0)) (Vector3 (-1.0f, 2.0f / 3.0f, -1.0f)) "ZUp min X, max Y, max Z"

                // Log scale rejects non-positive observations via tryMap / map
                let logScale = Scale3D.create BoxAspect.Cube UpAxis.ZUp (AxisScale3D.log 10.0 (1.0, 100.0)) (AxisScale3D.linear (0.0, 1.0)) (AxisScale3D.linear (0.0, 1.0))
                Expect.equal (Scale3D.tryMap logScale (Point3.create 0.0 0.5 0.5)) ValueNone "log scale omits zero"
                Expect.throwsT<ArgumentException> (fun () -> Scale3D.map logScale (Point3.create -1.0 0.5 0.5) |> ignore) "log scale throws on negative"

                // Categorical band & point mapping
                let catScale =
                    Scale3D.create
                        BoxAspect.ChartDefault
                        UpAxis.ZUp
                        (AxisScale3D.band [ "A"; "B" ] 0.0 0.0)
                        (AxisScale3D.point [ "Y0"; "Y1" ] 0.5)
                        (AxisScale3D.linear (0.0, 10.0))

                let hitCat = Scale3D.tryMapCategory catScale (Some "A") (Some "Y1") 10.0
                Expect.isTrue hitCat.IsSome "known categories map cleanly"
                expectVec3Close 1e-5f hitCat.Value (Vector3 (-0.5f, 2.0f / 3.0f, -0.5f)) "categorical band & point positions"
                Expect.equal (Scale3D.tryMapCategory catScale (Some "Missing") (Some "Y1") 10.0) ValueNone "unknown category returns ValueNone"

            testCase "Constant single-value domains map to 0.0f without NaN and BoxAspect.Auto guards > 4x span ratios"
            <| fun () ->
                let constScale = Scale3D.cartesian (5.0, 5.0) (0.0, 10.0) (-3.0, -3.0)
                let mapped = Scale3D.map constScale (Point3.create 5.0 5.0 -3.0)
                expectVec3Close 1e-6f mapped Vector3.Zero "constant domains map to world midpoint 0.0f"
                let mappedOther = Scale3D.map constScale (Point3.create 99.0 5.0 12.0)
                expectVec3Close 1e-6f mappedOther Vector3.Zero "constant domains stay at 0.0f without NaN"
                let recovered = Scale3D.invert constScale mapped
                expectPoint3Close 1e-6 recovered (Point3.create 5.0 5.0 -3.0) "constant domains invert cleanly"

                // Auto with 100x ratio (> 4x) falls back to ChartDefault (2.0, 4/3, 2.0)
                let extremeAuto =
                    Scale3D.create
                        BoxAspect.Auto
                        UpAxis.ZUp
                        (AxisScale3D.linear (0.0, 1.0))
                        (AxisScale3D.linear (0.0, 1.0))
                        (AxisScale3D.linear (0.0, 100.0))

                expectVec3Close 1e-5f (Scale3D.worldExtents extremeAuto) (Vector3 (2.0f, 4.0f / 3.0f, 2.0f)) "100x span falls back to ChartDefault"

                // Auto with constant zero-span axis falls back to ChartDefault
                let zeroAuto =
                    Scale3D.create
                        BoxAspect.Auto
                        UpAxis.ZUp
                        (AxisScale3D.linear (0.0, 1.0))
                        (AxisScale3D.linear (5.0, 5.0))
                        (AxisScale3D.linear (0.0, 2.0))

                expectVec3Close 1e-5f (Scale3D.worldExtents zeroAuto) (Vector3 (2.0f, 4.0f / 3.0f, 2.0f)) "zero span falls back to ChartDefault"

                // Auto with 2x ratio (<= 4x) uses proportional Data extents
                let mildAuto =
                    Scale3D.create
                        BoxAspect.Auto
                        UpAxis.ZUp
                        (AxisScale3D.linear (0.0, 2.0))
                        (AxisScale3D.linear (0.0, 1.0))
                        (AxisScale3D.linear (0.0, 1.0))

                expectVec3Close 1e-5f (Scale3D.worldExtents mildAuto) (Vector3 (2.0f, 1.0f, 1.0f)) "2x span uses proportional Data extents"

                // Auto at exact 4.0x boundary still uses Data extents; 4.01x falls back
                let exactFour =
                    Scale3D.create
                        BoxAspect.Auto
                        UpAxis.ZUp
                        (AxisScale3D.linear (0.0, 4.0))
                        (AxisScale3D.linear (0.0, 1.0))
                        (AxisScale3D.linear (0.0, 1.0))

                expectVec3Close 1e-5f (Scale3D.worldExtents exactFour) (Vector3 (2.0f, 0.5f, 0.5f)) "exact 4x boundary uses Data extents"

                let justOverFour =
                    Scale3D.create
                        BoxAspect.Auto
                        UpAxis.ZUp
                        (AxisScale3D.linear (0.0, 4.01))
                        (AxisScale3D.linear (0.0, 1.0))
                        (AxisScale3D.linear (0.0, 1.0))

                expectVec3Close 1e-5f (Scale3D.worldExtents justOverFour) (Vector3 (2.0f, 4.0f / 3.0f, 2.0f)) "4.01x falls back to ChartDefault"

            testCase "All 8 camera octants (ortho & perspective) emit 3 non-occluding back panels, 1 tick spine per axis, and stable label keys"
            <| fun () ->
                let scale = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)
                let spec = Axis3D.defaultSpec scale
                let ext = Scale3D.worldExtents scale
                let hx, hy, hz = ext.X * 0.5f, ext.Y * 0.5f, ext.Z * 0.5f

                let octants =
                    [
                        for sx in [ -1.0f; 1.0f ] do
                            for sy in [ -1.0f; 1.0f ] do
                                for sz in [ -1.0f; 1.0f ] do
                                    yield sx, sy, sz
                    ]

                let mutable expectedKeys: Set<string> option = None

                for (sx, sy, sz) in octants do
                    let eye = Vector3 (sx * 5.0f, sy * 5.0f, sz * 5.0f)

                    let orthoCam =
                        {
                            Position = eye
                            Target = Vector3.Zero
                            Up = Vector3.UnitY
                            FovYDeg = -2.2f
                            NearPlane = 0.05f
                            FarPlane = 50.0f
                        }

                    let perspCam =
                        {
                            Position = eye
                            Target = Vector3.Zero
                            Up = Vector3.UnitY
                            FovYDeg = 40.0f
                            NearPlane = 0.05f
                            FarPlane = 50.0f
                        }

                    for isOrtho, cam in [ true, orthoCam; false, perspCam ] do
                        let tag = $"octant ({sx},{sy},{sz}) ortho={isOrtho}"
                        let frame = Axis3D.build spec 800 600 cam

                        // 1. Back wall planes match far cuboid faces
                        let struct (bx, by, bz) = Axis3D.backWallPlanes frame
                        let acc = { absolute = 1e-5; relative = 0.0 }
                        Expect.floatClose acc (float bx) (float (-sx * hx)) $"{tag} worldBackX"
                        Expect.floatClose acc (float by) (float (-sy * hy)) $"{tag} worldFloorY"
                        Expect.floatClose acc (float bz) (float (-sz * hz)) $"{tag} worldBackZ"

                        // 2. Exactly 3 back-facing panel instances with negative dot(inwardNormal, camToWall)
                        let panels = Axis3D.panelInstances frame
                        Expect.equal panels.Length 3 $"{tag} emits 3 panel instances"

                        let panelSpan = panels.Span

                        for i = 0 to panelSpan.Length - 1 do
                            let p = panelSpan[i]
                            Expect.equal p.PickId 0u $"{tag} panel[{i}] has zero PickId"
                            Expect.equal p.Flags (InstanceFlags.Unlit ||| InstanceFlags.IgnoreSectionPlane) $"{tag} panel[{i}] flags"
                            let inwardN = Vector3.Normalize (Vector3 (p.ModelCol2.X, p.ModelCol2.Y, p.ModelCol2.Z))
                            let wallPos = Vector3 (p.ModelCol3.X, p.ModelCol3.Y, p.ModelCol3.Z)
                            let camToWall = if isOrtho then Vector3.Normalize (cam.Target - cam.Position) else wallPos - cam.Position
                            let dotVal = Vector3.Dot (inwardN, camToWall)
                            Expect.isLessThan dotVal 0.0f $"{tag} panel[{i}] inward normal has negative dot with camera-to-wall ({dotVal})"

                        // 3. Exactly 1 outer silhouette tick spine per axis (detectable from tick hairlines in lineVertices)
                        let verts = Axis3D.lineVertices frame
                        Expect.isGreaterThan verts.Length 0 $"{tag} has line vertices"
                        Expect.equal (verts.Length % 2) 0 $"{tag} line vertices paired"

                        let tickColorV4 = spec.Theme.TickColor.ToVector4 ()
                        let vArr = verts.ToArray ()

                        let tickStarts =
                            [
                                for i = 0 to (vArr.Length / 2) - 1 do
                                    let a = vArr[i * 2]
                                    let b = vArr[i * 2 + 1]
                                    if (a.Color - tickColorV4).LengthSquared () < 1e-6f then
                                        yield a.Position, b.Position
                            ]

                        // Group tick start positions by which cuboid axis they vary along -> must be 1 distinct spine per axis
                        let xSpines =
                            tickStarts
                            |> List.choose (fun (a, b) ->
                                if abs (abs a.Y - hy) < 1e-4f && abs (abs a.Z - hz) < 1e-4f && abs (a.X - b.X) < 1e-4f then
                                    Some (MathF.Round (a.Y, 3), MathF.Round (a.Z, 3))
                                else
                                    None)
                            |> List.distinct

                        let ySpines =
                            tickStarts
                            |> List.choose (fun (a, b) ->
                                if abs (abs a.X - hx) < 1e-4f && abs (abs a.Z - hz) < 1e-4f && abs (a.Y - b.Y) < 1e-4f then
                                    Some (MathF.Round (a.X, 3), MathF.Round (a.Z, 3))
                                else
                                    None)
                            |> List.distinct

                        let zSpines =
                            tickStarts
                            |> List.choose (fun (a, b) ->
                                if abs (abs a.X - hx) < 1e-4f && abs (abs a.Y - hy) < 1e-4f && abs (a.Z - b.Z) < 1e-4f then
                                    Some (MathF.Round (a.X, 3), MathF.Round (a.Y, 3))
                                else
                                    None)
                            |> List.distinct

                        Expect.equal xSpines.Length 1 $"{tag} exactly 1 tick spine along world X"
                        Expect.equal ySpines.Length 1 $"{tag} exactly 1 tick spine along world Y"
                        Expect.equal zSpines.Length 1 $"{tag} exactly 1 tick spine along world Z"

                        // Each tick spine must be a silhouette edge (one back sign, one front sign — never interior corner or frontmost corner)
                        let xSy, xSz = xSpines.Head
                        Expect.notEqual (sign xSy, sign xSz) (sign by, sign bz) $"{tag} X spine is not interior corner"
                        Expect.notEqual (sign xSy, sign xSz) (-sign by, -sign bz) $"{tag} X spine is not frontmost corner"
                        let ySx, ySz = ySpines.Head
                        Expect.notEqual (sign ySx, sign ySz) (sign bx, sign bz) $"{tag} Y spine is not interior corner"
                        Expect.notEqual (sign ySx, sign ySz) (-sign bx, -sign bz) $"{tag} Y spine is not frontmost corner"
                        let zSx, zSy = zSpines.Head
                        Expect.notEqual (sign zSx, sign zSy) (sign bx, sign by) $"{tag} Z spine is not interior corner"
                        Expect.notEqual (sign zSx, sign zSy) (-sign bx, -sign by) $"{tag} Z spine is not frontmost corner"

                        // 4. Stable AxisLabel2D keys across all octants and valid Scene overlay
                        let labels = Axis3D.labels frame
                        let keys = labels |> Seq.map _.Key |> Set.ofSeq
                        Expect.isTrue (keys.Contains "axis/x/title") $"{tag} has axis/x/title"
                        Expect.isTrue (keys.Contains "axis/y/title") $"{tag} has axis/y/title"
                        Expect.isTrue (keys.Contains "axis/z/title") $"{tag} has axis/z/title"
                        Expect.isTrue (keys.Contains "axis/x/tick/0") $"{tag} has axis/x/tick/0"

                        match expectedKeys with
                        | None -> expectedKeys <- Some keys
                        | Some firstKeys -> Expect.equal keys firstKeys $"{tag} label keys stay identical across rotation"

                        let scene = Axis3D.overlayScene 800.0 600.0 frame
                        let sceneKeys = Scene.marks scene |> List.map Mark.key |> Set.ofList
                        Expect.equal sceneKeys keys $"{tag} overlayScene marks match label keys"

            testCase "Axis3D panels and lines render cleanly on offscreen WgpuContext"
            <| fun () ->
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let spec = Axis3D.defaultSpec scale
                let cam = Camera3D.orbitPerspectiveness Vector3.Zero 35.0f 25.0f (Scale3D.fitRadius scale) 0.0f
                let frame = Axis3D.build spec 128 128 cam

                use ctx = new WgpuContext (initialWidth = 128, initialHeight = 128)
                use boxMesh = ctx.UploadMesh (Primitives.unitBox ())
                use panelBatch = ctx.CreateBatch (boxMesh, BlendMode.AlphaBlend, false)
                use lineBatch = ctx.CreateLineBatch ()

                panelBatch.UpdateInstances ((Axis3D.panelInstances frame).Span)
                lineBatch.UpdateVertices ((Axis3D.lineVertices frame).Span)

                let metrics =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| panelBatch |]
                    |> ScenePass.withLines [| lineBatch |]
                    |> WgpuContext.render ctx

                Expect.equal metrics.InstanceCount 3 "3 back wall panels rendered"
                let hasVisible = ctx.RgbaPixels |> Array.chunkBySize 4 |> Array.exists (fun px -> px[0] > 5uy || px[1] > 5uy || px[2] > 5uy)
                Expect.isTrue hasVisible "axis panels/lines produce non-black pixels on GPU"

            testCase "Axis3D configuration toggles, TimeUtc formatting, Protrusions defaults, and invalid argument guards"
            <| fun () ->
                Expect.equal Protrusions.defaultValue { Left = 36.0; Right = 36.0; Top = 28.0; Bottom = 32.0 } "Protrusions.defaultValue"

                // BoxAspect.Data with one zero-span axis & all-constant fallback
                let dataOneZero =
                    Scale3D.create
                        BoxAspect.Data
                        UpAxis.ZUp
                        (AxisScale3D.linear (0.0, 2.0))
                        (AxisScale3D.linear (5.0, 5.0))
                        (AxisScale3D.linear (0.0, 1.0))

                expectVec3Close 1e-5f (Scale3D.worldExtents dataOneZero) (Vector3 (2.0f, 1.0f, 0.0f)) "Data keeps zero extent on constant axis"
                Expect.isGreaterThan (Scale3D.fitRadius dataOneZero) 0.0f "Data with one constant axis has positive fitRadius"
                expectPoint3Close 1e-6 (Scale3D.invert dataOneZero (Vector3.Zero)) (Point3.create 1.0 5.0 0.5) "Data zero extent inverts without NaN"

                let allConst = Scale3D.create BoxAspect.Data UpAxis.ZUp (AxisScale3D.linear (5.0, 5.0)) (AxisScale3D.linear (5.0, 5.0)) (AxisScale3D.linear (5.0, 5.0))
                expectVec3Close 1e-5f (Scale3D.worldExtents allConst) (Vector3 (2.0f, 4.0f / 3.0f, 2.0f)) "all-constant Data falls back to ChartDefault"
                Expect.floatClose { absolute = 1e-4; relative = 0.0 } (float (Scale3D.fitRadius allConst)) (Bounds3.boundingRadius (Scale3D.worldBounds allConst)) "fitRadius matches worldBounds radius"

                // Validation guards
                Expect.throwsT<ArgumentOutOfRangeException> (fun () ->
                    Scale3D.create (BoxAspect.Manual (0.0, 1.0, 1.0)) UpAxis.ZUp (AxisScale3D.linear (0.0, 1.0)) (AxisScale3D.linear (0.0, 1.0)) (AxisScale3D.linear (0.0, 1.0)) |> ignore)
                    "Manual rejects non-positive width"

                Expect.throwsT<ArgumentException> (fun () ->
                    Scale3D.invert allConst (Vector3 (Single.NaN, 0.0f, 0.0f)) |> ignore)
                    "invert rejects non-finite vector"

                let tScale =
                    Scale3D.create
                        BoxAspect.ChartDefault
                        UpAxis.ZUp
                        (AxisScale3D.timeUtc (DateTimeOffset (2026, 1, 1, 0, 0, 0, TimeSpan.Zero), DateTimeOffset (2026, 1, 11, 0, 0, 0, TimeSpan.Zero)))
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))

                let cam = Camera3D.orbitPerspectiveness Vector3.Zero 35.0f 25.0f (Scale3D.fitRadius tScale) 0.0f
                let baseSpec = Axis3D.defaultSpec tScale
                let baseFrame = Axis3D.build baseSpec 640 480 cam
                let frontFrame = Axis3D.build { baseSpec with ShowFrontSpines = true } 640 480 cam
                Expect.equal (Axis3D.lineVertices frontFrame).Length ((Axis3D.lineVertices baseFrame).Length + 6) "ShowFrontSpines adds 3 front edges (6 vertices)"

                let noXPanelFrame = Axis3D.build { baseSpec with X = { baseSpec.X with ShowPanel = false } } 640 480 cam
                Expect.equal (Axis3D.panelInstances noXPanelFrame).Length 2 "ShowPanel = false omits 1 back panel"

                let customFmtSpec =
                    { baseSpec with
                        Y = { baseSpec.Y with FormatTick = Some (fun v -> $"y={int v}") }
                        Z = { baseSpec.Z with ShowGrid = false }
                    }

                let customFrame = Axis3D.build customFmtSpec 640 480 cam
                let yLabels = Axis3D.labels customFrame |> Seq.filter (fun l -> l.Axis = AxisId3D.Y && not l.IsTitle) |> Seq.map _.Text |> Seq.toList
                Expect.isTrue (yLabels |> List.exists (fun s -> s.StartsWith "y=")) "FormatTick custom formatter applied"

                let xLabels = Axis3D.labels customFrame |> Seq.filter (fun l -> l.Axis = AxisId3D.X && not l.IsTitle) |> Seq.map _.Text |> Seq.toList
                Expect.isTrue (xLabels |> List.exists (fun s -> s.StartsWith "2026-")) "TimeUtc formats ISO-date ticks"

                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Axis3D.build baseSpec 0 480 cam |> ignore) "rejects zero viewportWidth"
                Expect.throwsT<ArgumentException> (fun () -> Axis3D.build baseSpec 640 480 { cam with Position = cam.Target } |> ignore) "rejects Position = Target"

            testCase "Categorical AxisScale3D.band and AxisScale3D.point emit ticks, gridlines, tick hairlines, category labels, and Glyph3D.tryBarFromCategories bars"
            <| fun () ->
                let revBand = Scale.band [ "Q1"; "Q2"; "Q3" ] (1.0, 0.0) 0.1 0.1
                Expect.equal (Scale.bandCategories revBand) [ "Q1"; "Q2"; "Q3" ] "Scale.bandCategories preserves input domain order even when range is reversed"

                let bandX = AxisScale3D.band [ "Q1"; "Q2"; "Q3" ] 0.2 0.1
                let pointY = AxisScale3D.point [ "North"; "South" ] 0.5
                let bandY = AxisScale3D.band [ "North"; "South" ] 0.2 0.1
                let pointX = AxisScale3D.point [ "Q1"; "Q2"; "Q3" ] 0.5
                let linZ = AxisScale3D.linear (0.0, 100.0)

                Expect.equal (AxisScale3D.ticks 5 bandX).Length 3 "AxisScale3D.ticks emits 3 centered ticks for 3-category band"
                Expect.equal (AxisScale3D.ticks 5 pointX).Length 3 "AxisScale3D.ticks emits 3 centered ticks for 3-category point"
                Expect.equal (AxisScale3D.ticks 0 bandX) [] "AxisScale3D.ticks count=0 returns []"
                Expect.equal (AxisScale3D.ticks 2 (AxisScale3D.band [ "A"; "B"; "C"; "D" ] 0.1 0.1)).Length 2 "categorical ticks stride down to <= count"

                let cam = Camera3D.orthographic Vector3.Zero 35.0f 25.0f 1.5f 8.0f

                for label, sx, sy in
                    [
                        "Band", bandX, bandY
                        "Point", pointX, pointY
                    ] do
                    let catScale = Scale3D.create BoxAspect.ChartDefault UpAxis.ZUp sx sy linZ
                    let spec = Axis3D.defaultSpec catScale
                    let frameWithGrid = Axis3D.build spec 640 480 cam
                    let noGridSpec =
                        { spec with
                            X = { spec.X with ShowGrid = false }
                            Y = { spec.Y with ShowGrid = false }
                            Z = { spec.Z with ShowGrid = false }
                        }
                    let frameNoGrid = Axis3D.build noGridSpec 640 480 cam
                    Expect.isGreaterThan (Axis3D.lineVertices frameWithGrid).Length (Axis3D.lineVertices frameNoGrid).Length $"{label}: categorical axes emit back-wall gridlines"

                    let xLabels =
                        Axis3D.labels frameWithGrid
                        |> Seq.filter (fun l -> l.Axis = AxisId3D.X && not l.IsTitle)
                        |> Seq.toList

                    Expect.equal (xLabels |> List.map _.Text) [ "Q1"; "Q2"; "Q3" ] $"{label}: exact X category labels"
                    Expect.equal (xLabels |> List.map _.Key) [ "axis/x/tick/0"; "axis/x/tick/1"; "axis/x/tick/2" ] $"{label}: stable X tick keys"

                    let yLabels =
                        Axis3D.labels frameWithGrid
                        |> Seq.filter (fun l -> l.Axis = AxisId3D.Y && not l.IsTitle)
                        |> Seq.toList

                    Expect.equal (yLabels |> List.map _.Text) [ "North"; "South" ] $"{label}: exact Y category labels"

                    let tickColorV4 = spec.Theme.TickColor.ToVector4 ()
                    let tickSegCount =
                        (Axis3D.lineVertices frameWithGrid).ToArray ()
                        |> Array.chunkBySize 2
                        |> Array.filter (fun pair -> (pair[0].Color - tickColorV4).LengthSquared () < 1e-6f)
                        |> Array.length

                    Expect.isGreaterThanOrEqual tickSegCount 5 $"{label}: tick hairlines emitted for categorical X + Y ticks"

                    let barOpt = Glyph3D.tryBarFromCategories catScale "Q2" "South" 0.0 75.0 (RgbaColor (0.2f, 0.7f, 0.9f, 1.0f)) ShadingMode.Unlit 42u true
                    Expect.isTrue barOpt.IsSome $"{label}: tryBarFromCategories succeeds for known categories"
                    let bar = barOpt.Value
                    Expect.isGreaterThan bar.WidthX 0.0 $"{label}: bar WidthX is positive"
                    Expect.isGreaterThan bar.WidthY 0.0 $"{label}: bar WidthY is positive"

                    let packed = Glyph3D.packBars catScale (ReadOnlySpan<BarDatum3D> [| bar |])
                    Expect.equal packed.Length 1 $"{label}: Glyph3D.packBars packs categorical bar"
                    Expect.equal packed.Span[0].PickId 42u $"{label}: packed bar keeps PickId"
                    Expect.isGreaterThan (abs packed.Span[0].ModelCol0.X) 0.01f $"{label}: packed bar has positive world X width"
                    Expect.isGreaterThan (abs packed.Span[0].ModelCol1.Y) 0.01f $"{label}: packed bar has positive world height"
                    Expect.isGreaterThan (abs packed.Span[0].ModelCol2.Z) 0.01f $"{label}: packed bar has positive world Y depth"

                    Expect.equal (Glyph3D.tryBarFromCategories catScale "Missing" "South" 0.0 10.0 RgbaColor.white ShadingMode.Unlit 0u false) ValueNone $"{label}: unknown X category returns ValueNone"
                    Expect.equal (Glyph3D.tryBarFromCategories catScale "Q2" "Missing" 0.0 10.0 RgbaColor.white ShadingMode.Unlit 0u false) ValueNone $"{label}: unknown Y category returns ValueNone"
                    Expect.equal (Glyph3D.tryBarFromCategories catScale "Q2" "South" Double.NaN 10.0 RgbaColor.white ShadingMode.Unlit 0u false) ValueNone $"{label}: NaN z0 returns ValueNone"

                let nonCatScale = Scale3D.cartesian (0.0, 1.0) (0.0, 1.0) (0.0, 1.0)
                Expect.equal (Glyph3D.tryBarFromCategories nonCatScale "Q1" "North" 0.0 1.0 RgbaColor.white ShadingMode.Unlit 0u false) ValueNone "non-categorical scale returns ValueNone"

            testCase "Axis3D directional Protrusions shift Left, Right, Top, and Bottom titles outward without [20, 30] clamping"
            <| fun () ->
                let scale = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)
                let baseSpec = Axis3D.defaultSpec scale

                let titlePos (cam: Camera3D) (protrusions: Protrusions) (axisId: AxisId3D) =
                    let frame = Axis3D.build { baseSpec with Protrusions = protrusions } 800 600 cam
                    (Axis3D.labels frame |> Seq.find (fun l -> l.Axis = axisId && l.IsTitle)).ScreenPosition

                let neutral = { Left = 25.0; Right = 25.0; Top = 25.0; Bottom = 25.0 }

                // 1. Left & Bottom: camera in (+X, +Y, +Z) -> Z title projects outward to -X (Left), X title projects downward to +Y (Bottom)
                let camAbove =
                    {
                        Position = Vector3 (3.0f, 3.0f, 5.0f)
                        Target = Vector3.Zero
                        Up = Vector3.UnitY
                        FovYDeg = -2.2f
                        NearPlane = 0.05f
                        FarPlane = 50.0f
                    }

                let zBase = titlePos camAbove neutral AxisId3D.Z
                let zSmallLeft = titlePos camAbove { neutral with Left = 5.0 } AxisId3D.Z
                let zLargeLeft = titlePos camAbove { neutral with Left = 60.0 } AxisId3D.Z
                Expect.isGreaterThan (zBase.X - zLargeLeft.X) 15.0 "Left=60 pushes Z title further left beyond 30px clamp"
                Expect.isGreaterThan (zSmallLeft.X - zBase.X) 10.0 "Left=5 pulls Z title closer inward below 20px clamp"

                let xBase = titlePos camAbove neutral AxisId3D.X
                let xSmallBottom = titlePos camAbove { neutral with Bottom = 5.0 } AxisId3D.X
                let xLargeBottom = titlePos camAbove { neutral with Bottom = 60.0 } AxisId3D.X
                Expect.isGreaterThan (xLargeBottom.Y - xBase.Y) 15.0 "Bottom=60 pushes X title further down beyond 30px clamp"
                Expect.isGreaterThan (xBase.Y - xSmallBottom.Y) 10.0 "Bottom=5 pulls X title closer up below 20px clamp"

                // 2. Right: camera from (0.5, 0.1, 5.0) -> Y title outward normal (+1, -1, 0) points right (+X > 0.5)
                let camRight =
                    {
                        Position = Vector3 (0.5f, 0.1f, 5.0f)
                        Target = Vector3.Zero
                        Up = Vector3.UnitY
                        FovYDeg = -2.2f
                        NearPlane = 0.05f
                        FarPlane = 50.0f
                    }

                let yBase = titlePos camRight neutral AxisId3D.Y
                let ySmallRight = titlePos camRight { neutral with Right = 5.0 } AxisId3D.Y
                let yLargeRight = titlePos camRight { neutral with Right = 60.0 } AxisId3D.Y
                Expect.isGreaterThan (yLargeRight.X - yBase.X) 10.0 "Right=60 pushes Y title further right beyond 30px clamp"
                Expect.isGreaterThan (yBase.X - ySmallRight.X) 5.0 "Right=5 pulls Y title closer left below 20px clamp"

                // 3. Top: camera looking upward from below (0.05, -10, 1) -> Z silhouette edge outward normal points upward (-Y on screen, ~ -0.70)
                let camBelow =
                    {
                        Position = Vector3 (0.05f, -10.0f, 1.0f)
                        Target = Vector3.Zero
                        Up = Vector3.UnitY
                        FovYDeg = -2.2f
                        NearPlane = 0.05f
                        FarPlane = 50.0f
                    }

                let zTopBase = titlePos camBelow neutral AxisId3D.Z
                let zSmallTop = titlePos camBelow { neutral with Top = 5.0 } AxisId3D.Z
                let zLargeTop = titlePos camBelow { neutral with Top = 60.0 } AxisId3D.Z
                Expect.isGreaterThan (zTopBase.Y - zLargeTop.Y) 10.0 "Top=60 pushes Z title further up beyond 30px clamp"
                Expect.isGreaterThan (zSmallTop.Y - zTopBase.Y) 5.0 "Top=5 pulls Z title closer down below 20px clamp"
                Expect.isGreaterThan (zSmallTop.Y - zLargeTop.Y) 15.0 "Top=5..60 spans full directional range without clamping"

            testCase "Point3.toVector3f overflow guard and Check3D boundary helpers"
            <| fun () ->
                Expect.throwsT<ArgumentException> (fun () -> Point3.toVector3f (Point3.create 1e300 0.0 0.0) |> ignore) "toVector3f rejects +1e300 overflow"
                Expect.throwsT<ArgumentException> (fun () -> Point3.toVector3f (Point3.create 0.0 -1e300 0.0) |> ignore) "toVector3f rejects -1e300 overflow"
                let maxVec = Point3.toVector3f (Point3.create (float Single.MaxValue) (float -Single.MaxValue) 0.0)
                Expect.equal maxVec (Vector3 (Single.MaxValue, -Single.MaxValue, 0.0f)) "toVector3f preserves Single.MaxValue"

                Expect.isTrue (Check3D.isFinite 0.0) "isFinite 0.0"
                Expect.isTrue (Check3D.isFinite -0.0) "isFinite -0.0"
                Expect.isTrue (Check3D.isFinite Double.MaxValue) "isFinite Double.MaxValue"
                Expect.isFalse (Check3D.isFinite Double.NaN) "isFinite NaN"
                Expect.isFalse (Check3D.isFinite Double.PositiveInfinity) "isFinite +Inf"
                Expect.isFalse (Check3D.isFinite Double.NegativeInfinity) "isFinite -Inf"

                Expect.isTrue (Check3D.isFinite32 0.0f) "isFinite32 0.0f"
                Expect.isTrue (Check3D.isFinite32 -0.0f) "isFinite32 -0.0f"
                Expect.isTrue (Check3D.isFinite32 Single.MaxValue) "isFinite32 Single.MaxValue"
                Expect.isFalse (Check3D.isFinite32 Single.NaN) "isFinite32 NaN"
                Expect.isFalse (Check3D.isFinite32 Single.PositiveInfinity) "isFinite32 +Inf"
                Expect.isFalse (Check3D.isFinite32 Single.NegativeInfinity) "isFinite32 -Inf"

                Check3D.finite "ok" 0.0
                Check3D.finite "ok" -0.0
                Check3D.finite "ok" Double.MaxValue
                Expect.throwsT<ArgumentException> (fun () -> Check3D.finite "x" Double.NaN) "finite rejects NaN"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.finite "x" Double.PositiveInfinity) "finite rejects +Inf"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.finite "x" Double.NegativeInfinity) "finite rejects -Inf"

                Check3D.finite32 "ok" 0.0f
                Check3D.finite32 "ok" -0.0f
                Check3D.finite32 "ok" Single.MaxValue
                Expect.throwsT<ArgumentException> (fun () -> Check3D.finite32 "x" Single.NaN) "finite32 rejects NaN"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.finite32 "x" Single.PositiveInfinity) "finite32 rejects +Inf"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.finite32 "x" Single.NegativeInfinity) "finite32 rejects -Inf"

                Check3D.positive "ok" 1.0
                Check3D.positive "ok" Double.MaxValue
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive "x" 0.0) "positive rejects 0.0"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive "x" -0.0) "positive rejects -0.0"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive "x" -1.0) "positive rejects negative"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive "x" Double.NaN) "positive rejects NaN"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive "x" Double.PositiveInfinity) "positive rejects +Inf"

                Check3D.positive32 "ok" 1.0f
                Check3D.positive32 "ok" Single.MaxValue
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive32 "x" 0.0f) "positive32 rejects 0.0f"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive32 "x" -0.0f) "positive32 rejects -0.0f"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive32 "x" -1.0f) "positive32 rejects negative"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive32 "x" Single.NaN) "positive32 rejects NaN"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.positive32 "x" Single.PositiveInfinity) "positive32 rejects +Inf"

                Check3D.nonNegative "ok" 0.0
                Check3D.nonNegative "ok" -0.0
                Check3D.nonNegative "ok" Double.MaxValue
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.nonNegative "x" -1e-12) "nonNegative rejects negative"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.nonNegative "x" Double.NaN) "nonNegative rejects NaN"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.nonNegative "x" Double.NegativeInfinity) "nonNegative rejects -Inf"

                Check3D.nonNegative32 "ok" 0.0f
                Check3D.nonNegative32 "ok" -0.0f
                Check3D.nonNegative32 "ok" Single.MaxValue
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.nonNegative32 "x" -1e-6f) "nonNegative32 rejects negative"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.nonNegative32 "x" Single.NaN) "nonNegative32 rejects NaN"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Check3D.nonNegative32 "x" Single.NegativeInfinity) "nonNegative32 rejects -Inf"

                Check3D.vector3 "ok" (Vector3 (Single.MaxValue, -Single.MaxValue, 0.0f))
                Expect.throwsT<ArgumentException> (fun () -> Check3D.vector3 "v" (Vector3 (Single.NaN, 0.0f, 0.0f))) "vector3 rejects NaN"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.vector3 "v" (Vector3 (0.0f, Single.PositiveInfinity, 0.0f))) "vector3 rejects +Inf"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.vector3 "v" (Vector3 (0.0f, 0.0f, Single.NegativeInfinity))) "vector3 rejects -Inf"

                Check3D.interval "ok" (0.0, 0.0)
                Check3D.interval "ok" (-0.0, 0.0)
                Check3D.interval "ok" (-1e300, 1e300)
                Expect.throwsT<ArgumentException> (fun () -> Check3D.interval "iv" (Double.NaN, 1.0)) "interval rejects NaN"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.interval "iv" (0.0, Double.PositiveInfinity)) "interval rejects +Inf"
                Expect.throwsT<ArgumentException> (fun () -> Check3D.interval "iv" (-Double.MaxValue, Double.MaxValue)) "interval rejects overflowing span"

            testCase "AxisScale3D.log handles extreme exponents without overflow/underflow and arbitrary bases"
            <| fun () ->
                let hiLog = AxisScale3D.log 10.0 (1e190, 1e210)
                Expect.isTrue (AxisScale3D.isLogScale hiLog) "1e190..1e210 classifies as log"
                let hiTicks = AxisScale3D.ticks 5 hiLog
                Expect.isNonEmpty hiTicks "1e190..1e210 emits ticks"
                Expect.isTrue (hiTicks |> List.forall Check3D.isFinite) "1e190..1e210 ticks are finite"
                Expect.isTrue (hiTicks |> List.exists (fun v -> abs (v - 1e200) <= 1e-9 * 1e200)) "1e190..1e210 includes 1e200"

                let loLog = AxisScale3D.log 10.0 (1e-210, 1e-190)
                Expect.isTrue (AxisScale3D.isLogScale loLog) "1e-210..1e-190 classifies as log"
                let loTicks = AxisScale3D.ticks 5 loLog
                Expect.isNonEmpty loTicks "1e-210..1e-190 emits ticks"
                Expect.isTrue (loTicks |> List.forall (fun v -> Check3D.isFinite v && v > 0.0)) "1e-210..1e-190 ticks stay finite and positive"
                Expect.isTrue (loTicks |> List.exists (fun v -> abs (v - 1e-200) <= 1e-9 * 1e-200)) "1e-210..1e-190 includes 1e-200"

                let negLoLog = AxisScale3D.log 10.0 (-1e-190, -1e-210)
                Expect.isTrue (AxisScale3D.isLogScale negLoLog) "-1e-190..-1e-210 classifies as log"
                let negLoTicks = AxisScale3D.ticks 5 negLoLog
                Expect.isTrue (negLoTicks |> List.forall (fun v -> Check3D.isFinite v && v < 0.0)) "negative extreme log ticks stay negative"

                let base2 = AxisScale3D.log 2.0 (1.0, 64.0)
                Expect.isTrue (AxisScale3D.isLogScale base2) "base-2 scale classifies as log"
                Expect.equal (AxisScale3D.ticks 5 base2) [ 1.0; 2.0; 4.0; 8.0; 16.0; 32.0; 64.0 ] "base-2 emits powers of 2 at count=5"
                Expect.equal (AxisScale3D.ticks 10 base2) [ 1.0; 2.0; 4.0; 8.0; 16.0; 32.0; 64.0 ] "base-2 emits powers of 2 at count=10"

                let baseHalf = AxisScale3D.log 0.5 (1.0, 64.0)
                Expect.equal (AxisScale3D.ticks 5 baseHalf) [ 1.0; 2.0; 4.0; 8.0; 16.0; 32.0; 64.0 ] "base-0.5 emits powers of 2 without throwing"

                let base10 = AxisScale3D.log 10.0 (1.0, 1000.0)
                let base10Ticks = AxisScale3D.ticks 5 base10

                for p in [ 1.0; 10.0; 100.0; 1000.0 ] do
                    Expect.isTrue (base10Ticks |> List.contains p) $"base-10 count=5 includes {p}"

                let base10FiveDec = AxisScale3D.log 10.0 (1.0, 100000.0)
                Expect.equal (AxisScale3D.ticks 5 base10FiveDec) [ 1.0; 10.0; 100.0; 1000.0; 10000.0; 100000.0 ] "base-10 5 decades emits exact powers of 10"

            testCase "Axis3D camera FovYDeg/NearPlane/FarPlane validation, wide-range log formatting, and section-exempt lines"
            <| fun () ->
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let baseSpec = Axis3D.defaultSpec scale
                let validCam = Camera3D.orthographic Vector3.Zero 35.0f 25.0f 1.5f 6.0f

                for badFov in [ 0.0f; 1e-7f; -1e-7f; 179.9f; 180.0f ] do
                    Expect.throwsT<ArgumentOutOfRangeException>
                        (fun () -> Axis3D.build baseSpec 128 128 { validCam with FovYDeg = badFov } |> ignore)
                        $"rejects FovYDeg={badFov}"

                for badNonFiniteFov in [ Single.NaN; Single.PositiveInfinity; Single.NegativeInfinity ] do
                    Expect.throwsT<ArgumentException>
                        (fun () -> Axis3D.build baseSpec 128 128 { validCam with FovYDeg = badNonFiniteFov } |> ignore)
                        $"rejects non-finite FovYDeg={badNonFiniteFov}"

                for badPlane in [ 0.0f; -0.5f; Single.NaN; Single.PositiveInfinity ] do
                    Expect.throwsT<ArgumentOutOfRangeException>
                        (fun () -> Axis3D.build baseSpec 128 128 { validCam with NearPlane = badPlane } |> ignore)
                        $"rejects NearPlane={badPlane}"

                    Expect.throwsT<ArgumentOutOfRangeException>
                        (fun () -> Axis3D.build baseSpec 128 128 { validCam with FarPlane = badPlane } |> ignore)
                        $"rejects FarPlane={badPlane}"

                Expect.throwsT<ArgumentException>
                    (fun () -> Axis3D.build baseSpec 128 128 { validCam with NearPlane = 2.0f; FarPlane = 2.0f } |> ignore)
                    "rejects FarPlane = NearPlane"

                Expect.throwsT<ArgumentException>
                    (fun () -> Axis3D.build baseSpec 128 128 { validCam with NearPlane = 3.0f; FarPlane = 1.0f } |> ignore)
                    "rejects FarPlane < NearPlane"

                // Wide-range [1e-4 .. 1e6] log tick formatting
                let wideLogScale =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.ZUp
                        (AxisScale3D.log 10.0 (1e-4, 1e6))
                        (AxisScale3D.log 10.0 (1e-210, 1e-190))
                        (AxisScale3D.linear (0.0, 10.0))

                let wideSpec =
                    let s = Axis3D.defaultSpec wideLogScale
                    { s with X = { s.X with TickCount = 10 } }

                let wideFrame = Axis3D.build wideSpec 800 600 validCam
                let xTickTexts =
                    Axis3D.labels wideFrame
                    |> Seq.filter (fun l -> l.Axis = AxisId3D.X && not l.IsTitle)
                    |> Seq.map _.Text
                    |> Seq.toList

                Expect.isNonEmpty xTickTexts "wide log X axis emits tick labels"
                Expect.isFalse (xTickTexts |> List.exists (fun s -> s.Contains ".0000")) "wide log ticks omit .0000 trailing zeros"
                Expect.isTrue (xTickTexts |> List.contains "0.0001") "wide log ticks format 1e-4 cleanly"
                Expect.isTrue (xTickTexts |> List.contains "1") "wide log ticks format 1 cleanly without .0000"
                Expect.isTrue (xTickTexts |> List.contains "1E+06") "wide log ticks format 1e6 cleanly without 1000000.0000"

                let yTickTexts =
                    Axis3D.labels wideFrame
                    |> Seq.filter (fun l -> l.Axis = AxisId3D.Y && not l.IsTitle)
                    |> Seq.map _.Text
                    |> Seq.toList

                Expect.isNonEmpty yTickTexts "extreme negative-exponent log axis emits tick labels"
                Expect.isFalse (yTickTexts |> List.exists (fun s -> s = "0")) "1e-210..1e-190 ticks do not clamp to 0"

                // Section-exempt Axis3D lines remain visible in clipped half-space on GPU
                let brightLineColor = RgbaColor (1.0f, 0.25f, 0.25f, 1.0f)

                let lineOnlySpec =
                    { baseSpec with
                        X = { baseSpec.X with ShowPanel = false }
                        Y = { baseSpec.Y with ShowPanel = false }
                        Z = { baseSpec.Z with ShowPanel = false }
                        Theme =
                            { baseSpec.Theme with
                                SpineColor = brightLineColor
                                GridColor = brightLineColor
                                TickColor = brightLineColor
                            }
                    }

                let lineFrame = Axis3D.build lineOnlySpec 128 128 validCam
                let exemptVerts = (Axis3D.lineVertices lineFrame).ToArray ()
                Expect.isGreaterThan exemptVerts.Length 0 "Axis3D emits line vertices"

                for i = 0 to exemptVerts.Length - 1 do
                    Expect.equal exemptVerts[i].Pad 1.0f "Axis3D LineVertex3D sets Pad = 1.0f (ignoreSectionPlane)"

                let unexemptVerts =
                    exemptVerts
                    |> Array.map (fun v -> LineVertex3D (v.Position, v.Color))

                use ctx = new WgpuContext (initialWidth = 128, initialHeight = 128)
                use clippedLineBatch = ctx.CreateLineBatch ()
                use exemptLineBatch = ctx.CreateLineBatch ()
                clippedLineBatch.UpdateVertices (ReadOnlySpan<LineVertex3D> unexemptVerts)
                exemptLineBatch.UpdateVertices (ReadOnlySpan<LineVertex3D> exemptVerts)

                // Clip entire cuboid (all y in [-1, 1] have y - 10 < 0)
                let clipAllPlane = SectionPlane.create Vector3.UnitY -10.0f

                ScenePass.create validCam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withSection clipAllPlane
                |> ScenePass.withLines [| clippedLineBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let clippedVisible =
                    ctx.RgbaPixels
                    |> Array.chunkBySize 4
                    |> Array.exists (fun px -> px[0] > 20uy || px[1] > 20uy || px[2] > 20uy)

                Expect.isFalse clippedVisible "unexempt lines in clipped half-space are discarded"

                ScenePass.create validCam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withSection clipAllPlane
                |> ScenePass.withLines [| exemptLineBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let exemptVisible =
                    ctx.RgbaPixels
                    |> Array.chunkBySize 4
                    |> Array.exists (fun px -> px[0] > 100uy)

                Expect.isTrue exemptVisible "section-exempt Axis3D lines stay visible in clipped half-space on GPU"

            testCase "AxisConfig3D.FormatCategoryTick receives exact category strings rather than normalized floats"
            <| fun () ->
                let catScale =
                    Scale3D.create
                        BoxAspect.ChartDefault
                        UpAxis.ZUp
                        (AxisScale3D.band [ "Alpha"; "Beta"; "Gamma" ] 0.2 0.1)
                        (AxisScale3D.point [ "North"; "South" ] 0.5)
                        (AxisScale3D.linear (0.0, 10.0))

                let cam = Camera3D.orthographic Vector3.Zero 35.0f 25.0f 1.5f 8.0f
                let baseSpec = Axis3D.defaultSpec catScale

                let catSpec =
                    { baseSpec with
                        X =
                            { baseSpec.X with
                                FormatTick = Some (fun v -> $"num={v:F2}")
                                FormatCategoryTick = Some (fun s -> s.ToUpperInvariant ())
                            }
                        Y = { baseSpec.Y with FormatCategoryTick = Some (fun s -> $"[{s}]") }
                        Z = { baseSpec.Z with FormatTick = Some (fun v -> $"z={int v}") }
                    }

                let frame = Axis3D.build catSpec 640 480 cam

                let xTicks =
                    Axis3D.labels frame
                    |> Seq.filter (fun l -> l.Axis = AxisId3D.X && not l.IsTitle)
                    |> Seq.map _.Text
                    |> Seq.toList

                let yTicks =
                    Axis3D.labels frame
                    |> Seq.filter (fun l -> l.Axis = AxisId3D.Y && not l.IsTitle)
                    |> Seq.map _.Text
                    |> Seq.toList

                Expect.equal xTicks [ "ALPHA"; "BETA"; "GAMMA" ] "Band axis applies FormatCategoryTick to category strings (precedence over FormatTick)"
                Expect.equal yTicks [ "[North]"; "[South]" ] "Point axis applies FormatCategoryTick to category strings"

                let fallbackSpec = { baseSpec with X = { baseSpec.X with FormatTick = Some (fun v -> $"t={v:F1}") } }
                let fallbackFrame = Axis3D.build fallbackSpec 640 480 cam
                let fallbackTicks =
                    Axis3D.labels fallbackFrame
                    |> Seq.filter (fun l -> l.Axis = AxisId3D.X && not l.IsTitle)
                    |> Seq.map _.Text
                    |> Seq.toList
                Expect.isTrue (fallbackTicks |> List.forall (fun s -> s.StartsWith "t=")) "FormatTick normalized fallback preserved when FormatCategoryTick is None"
        ]

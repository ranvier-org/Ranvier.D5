module Ranvier.D5.Wgpu.Tests.ChannelTests

open System
open System.Numerics
open Expecto
open Ranvier.Anime
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.Wgpu

let private expectRgbaClose (eps: float32) (actual: RgbaColor) (expected: RgbaColor) (msg: string) =
    let acc = { absolute = float eps; relative = 0.0 }
    Expect.floatClose acc (float actual.R) (float expected.R) ($"{msg} (R)")
    Expect.floatClose acc (float actual.G) (float expected.G) ($"{msg} (G)")
    Expect.floatClose acc (float actual.B) (float expected.B) ($"{msg} (B)")
    Expect.floatClose acc (float actual.A) (float expected.A) ($"{msg} (A)")

let private expectVec3Close (eps: float32) (actual: Vector3) (expected: Vector3) (msg: string) =
    let acc = { absolute = float eps; relative = 0.0 }
    Expect.floatClose acc (float actual.X) (float expected.X) ($"{msg} (X)")
    Expect.floatClose acc (float actual.Y) (float expected.Y) ($"{msg} (Y)")
    Expect.floatClose acc (float actual.Z) (float expected.Z) ($"{msg} (Z)")

let private toRgbaColor (c: Rgba) =
    RgbaColor (float32 c.Red, float32 c.Green, float32 c.Blue, float32 c.Alpha)

let private pixelAt (ctx: WgpuContext) (x: int) (y: int) =
    let idx = (y * ctx.Width + x) * 4
    let px = ctx.RgbaPixels
    struct (px[idx], px[idx + 1], px[idx + 2], px[idx + 3])

[<Tests>]
let channelTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.Channels3D"
        [
            testCase "ColorRamp.tryMap matches endpoints, midpoints, clip overrides, and MissingColorPolicy"
            <| fun () ->
                let seqRamp =
                    ColorRamp.sequential
                        (Scale.linear (0.0, 100.0) (0.0, 1.0))
                        Chromatic.blues
                        ColorClipPolicy.clampAndOmit

                expectRgbaClose 1e-5f (ColorRamp.tryMap seqRamp 0.0 |> ValueOption.get) (toRgbaColor Chromatic.blues.Head) "sequential start"
                expectRgbaClose 1e-5f (ColorRamp.tryMap seqRamp 100.0 |> ValueOption.get) (toRgbaColor (List.last Chromatic.blues)) "sequential end"

                let expectedMid =
                    Chromatic.sequential ColorInterpolation.Lab GamutPolicy.Clip Chromatic.blues 0.5
                    |> toRgbaColor

                expectRgbaClose 2e-3f (ColorRamp.tryMap seqRamp 50.0 |> ValueOption.get) expectedMid "sequential midpoint"
                expectRgbaClose 1e-5f (ColorRamp.tryMap seqRamp -25.0 |> ValueOption.get) (toRgbaColor Chromatic.blues.Head) "unclipped low clamps"
                expectRgbaClose 1e-5f (ColorRamp.tryMap seqRamp 125.0 |> ValueOption.get) (toRgbaColor (List.last Chromatic.blues)) "unclipped high clamps"

                Expect.equal (ColorRamp.tryMap seqRamp Double.NaN) ValueNone "NaN omitted under Omit"
                Expect.equal (ColorRamp.tryMap seqRamp Double.PositiveInfinity) ValueNone "+Inf omitted under Omit"
                Expect.equal (ColorRamp.tryMap seqRamp Double.NegativeInfinity) ValueNone "-Inf omitted under Omit"

                let divRamp =
                    ColorRamp.diverging
                        (Scale.linear (-10.0, 10.0) (0.0, 1.0))
                        Chromatic.redBlue
                        ColorClipPolicy.clampAndOmit

                expectRgbaClose 4e-3f (ColorRamp.tryMap divRamp 0.0 |> ValueOption.get) (toRgbaColor Chromatic.redBlue[5]) "diverging midpoint"

                let lowOverride = RgbaColor (0.05f, 0.1f, 0.2f, 1.0f)
                let highOverride = RgbaColor (0.95f, 0.9f, 0.1f, 1.0f)
                let missingSentinel = RgbaColor (0.5f, 0.0f, 0.5f, 1.0f)

                let clipPolicy =
                    ColorClipPolicy.clampAndOmit
                    |> ColorClipPolicy.withClips (ValueSome lowOverride) (ValueSome highOverride)
                    |> fun p -> { p with Missing = MissingColorPolicy.Fallback missingSentinel }

                let clippedRamp =
                    ColorRamp.sequential
                        (Scale.linear (0.0, 100.0) (0.0, 1.0))
                        Chromatic.blues
                        clipPolicy

                expectRgbaClose 1e-6f (ColorRamp.tryMap clippedRamp -0.01 |> ValueOption.get) lowOverride "LowClip override"
                expectRgbaClose 1e-6f (ColorRamp.tryMap clippedRamp 100.01 |> ValueOption.get) highOverride "HighClip override"
                expectRgbaClose 1e-6f (ColorRamp.tryMap clippedRamp Double.NaN |> ValueOption.get) missingSentinel "Fallback sentinel returned for NaN"

                let callerFallback = RgbaColor (0.2f, 0.3f, 0.4f, 1.0f)
                expectRgbaClose 1e-6f (ColorRamp.mapOrFallback seqRamp Double.NaN callerFallback) callerFallback "mapOrFallback uses caller fallback on Omit"
                expectRgbaClose 1e-6f (ColorRamp.mapOrFallback clippedRamp Double.NaN callerFallback) missingSentinel "mapOrFallback uses policy Fallback sentinel"

                // Log scale support & rejection of non-positive inputs
                let logRamp =
                    ColorRamp.sequential
                        (Scale.log 10.0 (1.0, 100.0) (0.0, 1.0))
                        Chromatic.blues
                        ColorClipPolicy.clampAndOmit

                expectRgbaClose 2e-3f (ColorRamp.tryMap logRamp 10.0 |> ValueOption.get) expectedMid "log midpoint at geometric mean"
                Expect.equal (ColorRamp.tryMap logRamp 0.0) ValueNone "log omits zero"
                Expect.equal (ColorRamp.tryMap logRamp -5.0) ValueNone "log omits negative"

                // Built-in palettes & stops
                let vir = ColorRamp.viridis (0.0, 10.0)
                let tur = ColorRamp.turbo (0.0, 10.0)
                let cw = ColorRamp.coolWarm (-5.0, 5.0)
                Expect.equal (ColorRamp.stops 13 vir).Length 13 "stops 13 length"
                Expect.equal (ColorRamp.stops 0 vir).Length 0 "stops 0 length"
                Expect.notEqual (ColorRamp.tryMap vir 0.0) (ColorRamp.tryMap vir 10.0) "viridis endpoints differ"
                Expect.notEqual (ColorRamp.tryMap tur 0.0) (ColorRamp.tryMap tur 5.0) "turbo low vs mid differ"
                Expect.notEqual (ColorRamp.tryMap cw -5.0) (ColorRamp.tryMap cw 5.0) "coolWarm endpoints differ"

                // Palette & stops validation
                Expect.throwsT<ArgumentException> (fun () ->
                    ColorRamp.sequential (Scale.linear (0.0, 1.0) (0.0, 1.0)) [] ColorClipPolicy.clampAndOmit |> ignore)
                    "empty palette rejected"

                Expect.throwsT<ArgumentException> (fun () ->
                    ColorRamp.diverging (Scale.linear (0.0, 1.0) (0.0, 1.0)) (List.take 2 Chromatic.blues) ColorClipPolicy.clampAndOmit |> ignore)
                    "even-stop diverging palette rejected"

                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> ColorRamp.stops -1 vir |> ignore) "negative stop count rejected"

            testCase "ColorRamp.tryMap and mapOrFallback allocate 0 managed bytes across 10,000 warmed calls"
            <| fun () ->
                let vir = ColorRamp.viridis (0.0, 100.0)
                let logRamp = ColorRamp.sequential (Scale.log 10.0 (1.0, 1000.0) (0.0, 1.0)) Chromatic.blues ColorClipPolicy.clampAndOmit
                let fallback = RgbaColor (0.0f, 0.0f, 0.0f, 0.0f)

                let mutable accR = 0.0f

                // Warmup JIT & thread state
                for i = 0 to 100 do
                    let v = float i
                    match ColorRamp.tryMap vir v with
                    | ValueSome c -> accR <- accR + c.R
                    | ValueNone -> ()

                    match ColorRamp.tryMap logRamp (v + 1.0) with
                    | ValueSome c -> accR <- accR + c.R
                    | ValueNone -> ()

                    let c2 = ColorRamp.mapOrFallback vir v fallback
                    accR <- accR + c2.G

                let beforeLin = GC.GetAllocatedBytesForCurrentThread ()

                for i = 0 to 9999 do
                    let v = float (i % 120) - 10.0
                    match ColorRamp.tryMap vir v with
                    | ValueSome c -> accR <- accR + c.R
                    | ValueNone -> ()

                    let c2 = ColorRamp.mapOrFallback vir v fallback
                    accR <- accR + c2.G

                let afterLin = GC.GetAllocatedBytesForCurrentThread ()

                let beforeLog = GC.GetAllocatedBytesForCurrentThread ()

                for i = 0 to 9999 do
                    let v = float (i % 100) + 1.0
                    match ColorRamp.tryMap logRamp v with
                    | ValueSome c -> accR <- accR + c.R
                    | ValueNone -> ()

                let afterLog = GC.GetAllocatedBytesForCurrentThread ()

                Expect.isTrue (Single.IsFinite accR) "accumulator stays finite"
                Expect.equal (afterLin - beforeLin) 0L "10,000 linear ColorRamp.tryMap/mapOrFallback calls allocate 0 bytes"
                Expect.equal (afterLog - beforeLog) 0L "10,000 log ColorRamp.tryMap calls allocate 0 bytes"

            testCase "Orientation3D.alignYToVector handles UnitY, antiparallel -UnitY, Zero, non-finite, and arbitrary directions"
            <| fun () ->
                let qId = Orientation3D.alignYToVector Vector3.UnitY
                expectVec3Close 1e-5f (Vector3.Transform (Vector3.UnitY, qId)) Vector3.UnitY "+UnitY identity"

                let qAnti = Orientation3D.alignYToVector -Vector3.UnitY
                Expect.isFalse (Single.IsNaN qAnti.X || Single.IsNaN qAnti.Y || Single.IsNaN qAnti.Z || Single.IsNaN qAnti.W) "-UnitY has 0 NaNs"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (qAnti.Length ())) 1.0 "-UnitY quaternion is unit length"
                expectVec3Close 1e-5f (Vector3.Transform (Vector3.UnitY, qAnti)) (Vector3 (0.0f, -1.0f, 0.0f)) "-UnitY rotates +Y to -Y"

                Expect.equal (Orientation3D.alignYToVector Vector3.Zero) Quaternion.Identity "Zero returns Identity"
                Expect.equal (Orientation3D.alignYToVector (Vector3 (1e-7f, 0.0f, 0.0f))) Quaternion.Identity "near-zero returns Identity"
                Expect.equal (Orientation3D.alignYToVector (Vector3 (Single.NaN, 1.0f, 0.0f))) Quaternion.Identity "NaN returns Identity"
                Expect.equal (Orientation3D.alignYToVector (Vector3 (Single.PositiveInfinity, 0.0f, 0.0f))) Quaternion.Identity "Inf returns Identity"

                let dirs =
                    [
                        Vector3.UnitX
                        -Vector3.UnitX
                        Vector3.UnitZ
                        -Vector3.UnitZ
                        Vector3 (1.0f, 1.0f, 1.0f)
                        Vector3 (-3.0f, 2.0f, -5.0f)
                        Vector3 (0.001f, -0.9999f, 0.001f)
                        Vector3 (1e20f, 2e20f, -1e20f)
                    ]

                for dir in dirs do
                    let q = Orientation3D.alignYToVector dir
                    Expect.isFalse (Single.IsNaN q.X || Single.IsNaN q.Y || Single.IsNaN q.Z || Single.IsNaN q.W) $"no NaNs for {dir}"
                    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (q.Length ())) 1.0 $"unit length for {dir}"
                    let m = max (abs dir.X) (max (abs dir.Y) (abs dir.Z))
                    let expectedDir = Vector3.Normalize (dir / m)
                    expectVec3Close 1e-5f (Vector3.Transform (Vector3.UnitY, q)) expectedDir $"rotates +Y onto {dir}"

                let pos = Vector3 (1.0f, 2.0f, -3.0f)
                let dir = Vector3 (2.0f, 3.0f, -1.0f)
                let mat = Orientation3D.anisotropicMatrix pos dir 0.25f 1.5f
                let tip = Vector3.Transform (Vector3.UnitY, mat)
                let expectedTip = pos + Vector3.Normalize dir * 1.5f
                expectVec3Close 1e-5f tip expectedTip "anisotropicMatrix maps +UnitY to pos + dir * lengthY"
                let side = Vector3.Transform (Vector3.UnitX, mat) - pos
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (side.Length ())) 0.25 "anisotropicMatrix scales XZ by radiusXZ"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (Vector3.Dot (side, Vector3.Normalize dir))) 0.0 "anisotropicMatrix keeps XZ orthogonal to dir"

            testCase "WallProjection3D projects points and ContourPolygons onto active back walls under ZUp and YUp"
            <| fun () ->
                let scaleZ = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)
                let cam = Camera3D.orbitPerspectiveness Vector3.Zero 35.0f 25.0f (Scale3D.fitRadius scaleZ) 0.0f
                let frameZ = Axis3D.build (Axis3D.defaultSpec scaleZ) 320 240 cam
                let struct (bxZ, fyZ, bzZ) = Axis3D.backWallPlanes frameZ

                let p = Point3.create 2.5 7.5 6.0
                let w = Scale3D.map scaleZ p
                let pXY = WallProjection3D.projectPoint scaleZ (struct (bxZ, fyZ, bzZ)) WallPlane.XY p
                let pXZ = WallProjection3D.projectPoint scaleZ (struct (bxZ, fyZ, bzZ)) WallPlane.XZ p
                let pYZ = WallProjection3D.projectPoint scaleZ (struct (bxZ, fyZ, bzZ)) WallPlane.YZ p

                expectVec3Close 1e-5f pXY (Vector3 (w.X, fyZ, w.Z)) "ZUp XY projects onto world floor Y"
                expectVec3Close 1e-5f pXZ (Vector3 (w.X, w.Y, bzZ)) "ZUp XZ projects onto world back Z"
                expectVec3Close 1e-5f pYZ (Vector3 (bxZ, w.Y, w.Z)) "ZUp YZ projects onto world back X"

                let scaleY =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.YUp
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))

                let frameY = Axis3D.build (Axis3D.defaultSpec scaleY) 320 240 cam
                let struct (bxY, fyY, bzY) = Axis3D.backWallPlanes frameY
                let wY = Scale3D.map scaleY p
                expectVec3Close 1e-5f (WallProjection3D.projectPoint scaleY (struct (bxY, fyY, bzY)) WallPlane.XY p) (Vector3 (wY.X, wY.Y, bzY)) "YUp XY projects onto world Z"
                expectVec3Close 1e-5f (WallProjection3D.projectPoint scaleY (struct (bxY, fyY, bzY)) WallPlane.XZ p) (Vector3 (wY.X, fyY, wY.Z)) "YUp XZ projects onto world Y"
                expectVec3Close 1e-5f (WallProjection3D.projectPoint scaleY (struct (bxY, fyY, bzY)) WallPlane.YZ p) (Vector3 (bxY, wY.Y, wY.Z)) "YUp YZ projects onto world X"

                let values =
                    [ for r in 0..4 do
                        for c in 0..4 ->
                            Some (if r >= 1 && r <= 3 && c >= 1 && c <= 3 && not (r = 2 && c = 2) then 2.0 else 0.0) ]

                let grid =
                    match DensityGrid.create (Bounds.create 0.0 0.0 10.0 10.0) 5 5 values with
                    | Ok g -> g
                    | Error e -> failtestf "grid failed: %A" e

                let contours =
                    match Contour.compute (ContourConfig.create [ 1.0 ]) grid with
                    | Ok c -> c
                    | Error e -> failtestf "contour failed: %A" e

                let lineColor = RgbaColor (0.9f, 0.7f, 0.2f, 0.85f)
                let xySegs = WallProjection3D.projectContours scaleZ (struct (bxZ, fyZ, bzZ)) WallPlane.XY lineColor contours
                let xzSegs = WallProjection3D.projectContours scaleZ (struct (bxZ, fyZ, bzZ)) WallPlane.XZ lineColor contours
                let yzSegs = WallProjection3D.projectContours scaleZ (struct (bxZ, fyZ, bzZ)) WallPlane.YZ lineColor contours

                Expect.isGreaterThan xySegs.Length 0 "XY contours emit line vertices"
                Expect.equal (xySegs.Length % 2) 0 "XY contours emit paired LineList vertices"
                Expect.equal (xzSegs.Length % 2) 0 "XZ contours emit paired LineList vertices"
                Expect.equal (yzSegs.Length % 2) 0 "YZ contours emit paired LineList vertices"

                for i = 0 to xySegs.Length - 1 do
                    let v = xySegs.Span[i]
                    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float v.Position.Y) (float fyZ) "XY contour vertex lies on world floor Y"
                    Expect.equal v.Color (lineColor.ToVector4 ()) "contour color preserved"

                for i = 0 to xzSegs.Length - 1 do
                    let v = xzSegs.Span[i]
                    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float v.Position.Z) (float bzZ) "XZ contour vertex lies on world back Z"

                for i = 0 to yzSegs.Length - 1 do
                    let v = yzSegs.Span[i]
                    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float v.Position.X) (float bxZ) "YZ contour vertex lies on world back X"

            testCase "ColorShader WGSL validates and shifts rendered quad color when cmin/cmax change via SetUserUniforms without re-uploading MeshHandle"
            <| fun () ->
                let wgsl = ColorShader.wgsl ()
                Expect.equal (WgpuContext.ValidateWgsl wgsl) (Ok ()) "ColorShader.wgsl passes WgpuContext.ValidateWgsl"

                let coolWarm = ColorRamp.coolWarm (0.0, 1.0)
                let uDefault = ColorShader.packUniforms 0.0f 1.0f coolWarm
                Expect.equal uDefault.Length 16 "packUniforms emits 16 Vector4 slots"
                Expect.equal uDefault.Span[0] (Vector4 (0.0f, 1.0f, 13.0f, 1.0f)) "slot 0 packs [cmin, cmax, stopCount, enabled]"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64, customWgsl = wgsl)

                // Upload a single quad once with raw scalar 0.5 encoded in vertexColor.x
                let scalarCol = Vector4 (0.5f, 0.0f, 0.0f, 1.0f)

                let quadMesh =
                    {
                        Vertices =
                            [|
                                Vertex3D (Vector3 (-1.0f, -1.0f, 0.0f), Vector3.UnitZ, scalarCol)
                                Vertex3D (Vector3 (1.0f, -1.0f, 0.0f), Vector3.UnitZ, scalarCol)
                                Vertex3D (Vector3 (1.0f, 1.0f, 0.0f), Vector3.UnitZ, scalarCol)
                                Vertex3D (Vector3 (-1.0f, 1.0f, 0.0f), Vector3.UnitZ, scalarCol)
                            |]
                        Indices = [| 0u; 1u; 2u; 0u; 2u; 3u |]
                    }

                use meshHandle = ctx.UploadMesh quadMesh
                use batch = ctx.CreateBatch (meshHandle, BlendMode.Opaque, true)
                batch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 1u InstanceFlags.Unlit)

                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.5f 4.0f

                let pass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| batch |]

                // 1. Range [0.0, 1.0] -> scalar 0.5 sits at neutral center (R ~= G ~= B)
                ctx.SetUserUniforms (uDefault.Span)
                pass |> WgpuContext.render ctx |> ignore
                let struct (mr, mg, mb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan mr 150uy "neutral center bright red"
                Expect.isLessThan (abs (int mr - int mb)) 20 "neutral center balanced R and B"
                Expect.isLessThan (abs (int mr - int mg)) 20 "neutral center balanced R and G"

                // 2. Shift [cmin, cmax] to [0.5, 1.0] WITHOUT re-uploading MeshHandle -> scalar 0.5 shifts to cool blue!
                let uCool = ColorShader.packUniforms 0.5f 1.0f coolWarm
                ctx.SetUserUniforms (uCool.Span)
                pass |> WgpuContext.render ctx |> ignore
                let struct (cr, cg, cb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan cb (cr + 40uy) "cmin=0.5 shifts quad to cool blue"

                // 3. Shift [cmin, cmax] to [0.0, 0.5] WITHOUT re-uploading MeshHandle -> scalar 0.5 shifts to warm red!
                let uWarm = ColorShader.packUniforms 0.0f 0.5f coolWarm
                ctx.SetUserUniforms (uWarm.Span)
                pass |> WgpuContext.render ctx |> ignore
                let struct (wr, wg, wb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan wr (wb + 40uy) "cmax=0.5 shifts quad to warm red"

                // Verify ColorShader pixel matches CPU ColorRamp + stock shader (slots[0].w = 0)
                let expectedWarmColor = ColorRamp.tryMap coolWarm 1.0 |> ValueOption.get
                let expectedCoolColor = ColorRamp.tryMap coolWarm 0.0 |> ValueOption.get
                use stockMesh =
                    ctx.UploadMesh
                        {
                            Vertices =
                                [|
                                    Vertex3D (Vector3 (-1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (-1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                |]
                            Indices = quadMesh.Indices
                        }
                use stockBatch = ctx.CreateBatch (stockMesh, BlendMode.Opaque, true)
                ctx.SetUserUniform (0, Vector4.Zero)
                let stockPass = pass |> ScenePass.withBatches [| stockBatch |]
                stockBatch.SetSingle (Instance3D.identity expectedCoolColor 0.5f 1u InstanceFlags.Unlit)
                stockPass |> WgpuContext.render ctx |> ignore
                let struct (scr, scg, scb, _) = pixelAt ctx 32 32
                Expect.isLessThanOrEqual (abs (int cr - int scr)) 2 "ColorShader cool R matches stock CPU ColorRamp"
                Expect.isLessThanOrEqual (abs (int cg - int scg)) 2 "ColorShader cool G matches stock CPU ColorRamp"
                Expect.isLessThanOrEqual (abs (int cb - int scb)) 2 "ColorShader cool B matches stock CPU ColorRamp"

                stockBatch.SetSingle (Instance3D.identity expectedWarmColor 0.5f 1u InstanceFlags.Unlit)
                stockPass |> WgpuContext.render ctx |> ignore
                let struct (swr, swg, swb, _) = pixelAt ctx 32 32
                Expect.isLessThanOrEqual (abs (int wr - int swr)) 2 "ColorShader warm R matches stock CPU ColorRamp"
                Expect.isLessThanOrEqual (abs (int wg - int swg)) 2 "ColorShader warm G matches stock CPU ColorRamp"
                Expect.isLessThanOrEqual (abs (int wb - int swb)) 2 "ColorShader warm B matches stock CPU ColorRamp"

                // 4. Verify Lit shading mode also renders ColorShader color cleanly without blackening
                batch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 1u InstanceFlags.None)
                ctx.SetUserUniforms (uWarm.Span)
                pass |> WgpuContext.render ctx |> ignore
                let struct (lr, _, lb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan lr (lb + 40uy) "Lit mode preserves ColorShader warm red shift"

                // 5. ColorShader.disabledUniforms () disables LUT remapping cleanly
                let disabled = ColorShader.disabledUniforms ()
                Expect.equal disabled.Length 16 "disabledUniforms emits 16 Vector4 slots"
                Expect.equal disabled.Span[0] Vector4.Zero "disabledUniforms slot 0 is zeroed"
                ctx.SetUserUniforms (disabled.Span)
                stockBatch.SetSingle (Instance3D.identity expectedCoolColor 0.5f 1u InstanceFlags.Unlit)
                stockPass |> WgpuContext.render ctx |> ignore
                let struct (dr, dg, db, _) = pixelAt ctx 32 32
                Expect.isLessThanOrEqual (abs (int dr - int scr)) 2 "disabledUniforms restores raw instance color R"
                Expect.isLessThanOrEqual (abs (int dg - int scg)) 2 "disabledUniforms restores raw instance color G"
                Expect.isLessThanOrEqual (abs (int db - int scb)) 2 "disabledUniforms restores raw instance color B"

            testCase "ColorShader Lit evaluates 13-stop LUT per-fragment, supports 1e-20 domain, and Channels3D enforces null guards + YUp contours + AxisCoordMapper"
            <| fun () ->
                let wgsl = ColorShader.wgsl ()
                Expect.equal (WgpuContext.ValidateWgsl wgsl) (Ok ()) "ColorShader.wgsl passes WgpuContext.ValidateWgsl"

                // 1. Per-fragment Lit LUT interpolation across one large triangle spanning t = 0.0 -> 1.0
                //    (At world origin (0,0,0), barycentric weights are 0.25*0 + 0.5*1 + 0.25*0 = 0.5 -> bright turbo green,
                //     whereas per-vertex RGB averaging yields dark brownish purple with very low green.)
                let turbo = ColorRamp.turbo (0.0, 1.0)
                let uTurbo = ColorShader.packUniforms 0.0f 1.0f turbo

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64, customWgsl = wgsl)

                let triMesh =
                    {
                        Vertices =
                            [|
                                Vertex3D (Vector3 (-1.0f, -2.0f, 0.0f), Vector3.UnitZ, Vector4 (0.0f, 0.0f, 0.0f, 1.0f))
                                Vertex3D (Vector3 (1.0f, 0.0f, 0.0f), Vector3.UnitZ, Vector4 (1.0f, 0.0f, 0.0f, 1.0f))
                                Vertex3D (Vector3 (-1.0f, 2.0f, 0.0f), Vector3.UnitZ, Vector4 (0.0f, 0.0f, 0.0f, 1.0f))
                            |]
                        Indices = [| 0u; 1u; 2u |]
                    }

                use triHandle = ctx.UploadMesh triMesh
                use triBatch = ctx.CreateBatch (triHandle, BlendMode.Opaque, true)
                triBatch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 1u InstanceFlags.None)

                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.0f 4.0f

                let triPass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| triBatch |]

                ctx.SetUserUniforms (uTurbo.Span)
                triPass |> WgpuContext.render ctx |> ignore
                let struct (lr, lg, lb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan lg 170uy "Lit ColorShader midpoint yields bright turbo green"
                Expect.isGreaterThan lg lr "Lit ColorShader midpoint green exceeds red"
                Expect.isGreaterThan lg (lb + 60uy) "Lit ColorShader midpoint green exceeds blue"

                // Compare Lit ColorShader pixel against both:
                // (a) vertex-interpolated endpoint colors (proving per-fragment LUT evaluation beats vertex RGB average), and
                // (b) stock shader Lit triangle colored at exact midpoint t=0.5 (proving studio lighting parity).
                let loTurbo = ColorRamp.tryMap turbo 0.0 |> ValueOption.get
                let hiTurbo = ColorRamp.tryMap turbo 1.0 |> ValueOption.get
                let midTurbo = ColorRamp.tryMap turbo 0.5 |> ValueOption.get

                use vertexInterpTri =
                    ctx.UploadMesh
                        {
                            Vertices =
                                [|
                                    Vertex3D (Vector3 (-1.0f, -2.0f, 0.0f), Vector3.UnitZ, loTurbo.ToVector4 ())
                                    Vertex3D (Vector3 (1.0f, 0.0f, 0.0f), Vector3.UnitZ, hiTurbo.ToVector4 ())
                                    Vertex3D (Vector3 (-1.0f, 2.0f, 0.0f), Vector3.UnitZ, loTurbo.ToVector4 ())
                                |]
                            Indices = [| 0u; 1u; 2u |]
                        }

                use vertexInterpBatch = ctx.CreateBatch (vertexInterpTri, BlendMode.Opaque, true)
                vertexInterpBatch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 1u InstanceFlags.None)
                ctx.SetUserUniforms ((ColorShader.disabledUniforms ()).Span)

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| vertexInterpBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (vr, vg, _, _) = pixelAt ctx 32 32
                Expect.isLessThan vg vr "vertex-interpolated RGB average lacks turbo midpoint green"
                Expect.isGreaterThan (int lg - int vg) 80 "per-fragment Lit LUT green dramatically exceeds vertex-interpolated RGB average"

                use stockTri =
                    ctx.UploadMesh
                        {
                            Vertices =
                                [|
                                    Vertex3D (Vector3 (-1.0f, -2.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (1.0f, 0.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (-1.0f, 2.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                |]
                            Indices = [| 0u; 1u; 2u |]
                        }

                use stockTriBatch = ctx.CreateBatch (stockTri, BlendMode.Opaque, true)
                stockTriBatch.SetSingle (Instance3D.identity midTurbo 0.5f 1u InstanceFlags.None)

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| stockTriBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (sr, sg, sb, _) = pixelAt ctx 32 32
                Expect.isLessThanOrEqual (abs (int lr - int sr)) 2 "Lit ColorShader R matches stock studio lighting"
                Expect.isLessThanOrEqual (abs (int lg - int sg)) 2 "Lit ColorShader G matches stock studio lighting"
                Expect.isLessThanOrEqual (abs (int lb - int sb)) 2 "Lit ColorShader B matches stock studio lighting"

                // 2. Tiny domain [0.0f, 1e-20f] maps low and high stops distinctly via 1e-30 guard
                let coolWarm = ColorRamp.coolWarm (0.0, 1.0)
                let uTiny = ColorShader.packUniforms 0.0f 1e-20f coolWarm

                let makeTinyQuad s =
                    ctx.UploadMesh
                        {
                            Vertices =
                                [|
                                    Vertex3D (Vector3 (-1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4 (s, 0.0f, 0.0f, 1.0f))
                                    Vertex3D (Vector3 (1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4 (s, 0.0f, 0.0f, 1.0f))
                                    Vertex3D (Vector3 (1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4 (s, 0.0f, 0.0f, 1.0f))
                                    Vertex3D (Vector3 (-1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4 (s, 0.0f, 0.0f, 1.0f))
                                |]
                            Indices = [| 0u; 1u; 2u; 0u; 2u; 3u |]
                        }

                use lowTinyMesh = makeTinyQuad 0.0f
                use highTinyMesh = makeTinyQuad 1e-20f
                use lowTinyBatch = ctx.CreateBatch (lowTinyMesh, BlendMode.Opaque, true)
                use highTinyBatch = ctx.CreateBatch (highTinyMesh, BlendMode.Opaque, true)
                lowTinyBatch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 1u InstanceFlags.Unlit)
                highTinyBatch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 1u InstanceFlags.Unlit)

                ctx.SetUserUniforms (uTiny.Span)

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| lowTinyBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (tlr, _, tlb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan tlb (tlr + 40uy) "tiny domain [0, 1e-20] maps 0.0f to cool blue"

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| highTinyBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (thr, _, thb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan thr (thb + 40uy) "tiny domain [0, 1e-20] maps 1e-20f to warm red"

                // 3. nullArg guards on ColorRamp and WallProjection3D
                Expect.throwsT<ArgumentNullException> (fun () -> ColorRamp.tryMap (Unchecked.defaultof<ColorRamp>) 0.5 |> ignore) "tryMap rejects null ramp"
                Expect.throwsT<ArgumentNullException> (fun () -> ColorRamp.stops 5 (Unchecked.defaultof<ColorRamp>) |> ignore) "stops rejects null ramp"
                Expect.throwsT<ArgumentNullException> (fun () -> ColorRamp.stops 0 (Unchecked.defaultof<ColorRamp>) |> ignore) "stops(0) rejects null ramp"

                let walls = struct (-1.0f, -1.0f, -1.0f)

                Expect.throwsT<ArgumentNullException>
                    (fun () -> WallProjection3D.projectPoint (Unchecked.defaultof<Scale3D>) walls WallPlane.XY (Point3.create 0.0 0.0 0.0) |> ignore)
                    "projectPoint rejects null scale"

                Expect.throwsT<ArgumentNullException>
                    (fun () -> WallProjection3D.projectContours (Unchecked.defaultof<Scale3D>) walls WallPlane.XY RgbaColor.white [] |> ignore)
                    "projectContours rejects null scale"

                let validScale = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)

                Expect.throwsT<ArgumentNullException>
                    (fun () -> WallProjection3D.projectContours validScale walls WallPlane.XY RgbaColor.white (Unchecked.defaultof<ContourPolygon list>) |> ignore)
                    "projectContours rejects null contours"

                // 4. YUp test case for WallProjection3D.projectContours
                let scaleY =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.YUp
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))

                let frameY = Axis3D.build (Axis3D.defaultSpec scaleY) 320 240 cam
                let struct (bxY, fyY, bzY) = Axis3D.backWallPlanes frameY

                let values =
                    [ for r in 0..4 do
                        for c in 0..4 ->
                            Some (if r >= 1 && r <= 3 && c >= 1 && c <= 3 && not (r = 2 && c = 2) then 2.0 else 0.0) ]

                let grid =
                    match DensityGrid.create (Bounds.create 0.0 0.0 10.0 10.0) 5 5 values with
                    | Ok g -> g
                    | Error e -> failtestf "grid failed: %A" e

                let contours =
                    match Contour.compute (ContourConfig.create [ 1.0 ]) grid with
                    | Ok c -> c
                    | Error e -> failtestf "contour failed: %A" e

                let lineCol = RgbaColor (0.3f, 0.8f, 0.5f, 1.0f)
                let xyY = WallProjection3D.projectContours scaleY (struct (bxY, fyY, bzY)) WallPlane.XY lineCol contours
                let xzY = WallProjection3D.projectContours scaleY (struct (bxY, fyY, bzY)) WallPlane.XZ lineCol contours
                let yzY = WallProjection3D.projectContours scaleY (struct (bxY, fyY, bzY)) WallPlane.YZ lineCol contours

                Expect.isGreaterThan xyY.Length 0 "YUp XY contours emit vertices"
                Expect.equal (xyY.Length % 2) 0 "YUp XY contours paired"
                Expect.equal (xzY.Length % 2) 0 "YUp XZ contours paired"
                Expect.equal (yzY.Length % 2) 0 "YUp YZ contours paired"

                for i = 0 to xyY.Length - 1 do
                    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float xyY.Span[i].Position.Z) (float bzY) "YUp XY contour lies on world Z"

                for i = 0 to xzY.Length - 1 do
                    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float xzY.Span[i].Position.Y) (float fyY) "YUp XZ contour lies on world Y"

                for i = 0 to yzY.Length - 1 do
                    Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float yzY.Span[i].Position.X) (float bxY) "YUp YZ contour lies on world X"

                // 5. Shared internal AxisCoordMapper works across linear, log, time, piecewise, and categorical
                let linMap = AxisCoordMapper.classify (AxisScale3D.linear (10.0, 30.0))
                let linOut = Array.zeroCreate<float> 3
                AxisCoordMapper.mapArray linMap [| 10.0; 20.0; Double.NaN |] linOut
                Expect.floatClose { absolute = 1e-9; relative = 0.0 } linOut[0] 0.0 "AxisCoordMapper linear start"
                Expect.floatClose { absolute = 1e-9; relative = 0.0 } linOut[1] 0.5 "AxisCoordMapper linear midpoint"
                Expect.isTrue (Double.IsNaN linOut[2]) "AxisCoordMapper NaN returns NaN"

                let logMap = AxisCoordMapper.classify (AxisScale3D.log 10.0 (1.0, 100.0))
                let logOut = Array.zeroCreate<float> 2
                AxisCoordMapper.mapArray logMap [| 10.0; 0.0 |] logOut
                Expect.floatClose { absolute = 1e-9; relative = 0.0 } logOut[0] 0.5 "AxisCoordMapper log midpoint"
                Expect.isTrue (Double.IsNaN logOut[1]) "AxisCoordMapper log zero returns NaN"

                let wCoords, wValid = AxisCoordMapper.mapWorldCoords linMap 2.0 [| 10.0; 20.0; Double.NaN |]
                Expect.equal wValid [| true; true; false |] "AxisCoordMapper.mapWorldCoords flags finite vs NaN"
                Expect.floatClose { absolute = 1e-9; relative = 0.0 } wCoords[0] -1.0 "AxisCoordMapper.mapWorldCoords min"
                Expect.floatClose { absolute = 1e-9; relative = 0.0 } wCoords[1] 0.0 "AxisCoordMapper.mapWorldCoords mid"
                Expect.equal (AxisCoordMapper.axisDir (AxisScale3D.linear (10.0, 0.0))) -1.0 "AxisCoordMapper.axisDir reversed"

                let subLogPos = RampTransform.classify (Scale.log 10.0 (1e-210, 1e-190) (0.0, 1.0))
                let subLogNeg = RampTransform.classify (Scale.log 10.0 (-1e-190, -1e-210) (0.0, 1.0))

                match subLogPos with
                | RampTransform.Log (sign, _, _, false) -> Expect.equal sign 1.0 "positive sub-1e-162 log classifies as RampTransform.Log"
                | other -> failtestf "expected RampTransform.Log for positive sub-1e-162 domain, got %A" other

                match subLogNeg with
                | RampTransform.Log (sign, _, _, false) -> Expect.equal sign -1.0 "negative sub-1e-162 log classifies as RampTransform.Log"
                | other -> failtestf "expected RampTransform.Log for negative sub-1e-162 domain, got %A" other

                Expect.floatClose { absolute = 1e-9; relative = 0.0 } (RampTransform.eval subLogPos 1e-200) 0.5 "positive sub-1e-162 log midpoint"
                Expect.floatClose { absolute = 1e-9; relative = 0.0 } (RampTransform.eval subLogNeg -1e-200) 0.5 "negative sub-1e-162 log midpoint"
                Expect.isTrue (Double.IsNaN (RampTransform.eval subLogPos 0.0)) "positive sub-1e-162 log rejects zero"
                Expect.isTrue (Double.IsNaN (RampTransform.eval subLogNeg 1e-200)) "negative sub-1e-162 log rejects positive"

            testCase "WallProjection3D.projectContours emits Pad = 1.0f and stays visible under clipping SectionPlane"
            <| fun () ->
                let scale = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)
                let cam = Camera3D.orthographic Vector3.Zero 0.0f 85.0f 1.5f 6.0f
                let frame = Axis3D.build (Axis3D.defaultSpec scale) 64 64 cam
                let walls = Axis3D.backWallPlanes frame

                let values =
                    [ for r in 0..4 do
                        for c in 0..4 ->
                            Some (if r >= 1 && r <= 3 && c >= 1 && c <= 3 && not (r = 2 && c = 2) then 2.0 else 0.0) ]

                let grid =
                    match DensityGrid.create (Bounds.create 0.0 0.0 10.0 10.0) 5 5 values with
                    | Ok g -> g
                    | Error e -> failtestf "grid failed: %A" e

                let contours =
                    match Contour.compute (ContourConfig.create [ 1.0 ]) grid with
                    | Ok c -> c
                    | Error e -> failtestf "contour failed: %A" e

                let segs = WallProjection3D.projectContours scale walls WallPlane.XY RgbaColor.white contours
                Expect.isGreaterThan segs.Length 0 "XY contours emit segments"

                for i = 0 to segs.Length - 1 do
                    Expect.equal segs.Span[i].Pad 1.0f $"projectContours vertex[{i}] sets Pad = 1.0f (ignoreSectionPlane)"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use lineBatch = ctx.CreateLineBatch ()
                lineBatch.UpdateVertices segs.Span
                let clipAllPlane = SectionPlane.create Vector3.UnitY -10.0f

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withSection clipAllPlane
                |> ScenePass.withLines [| lineBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let visible =
                    ctx.RgbaPixels
                    |> Array.chunkBySize 4
                    |> Array.exists (fun px -> px[0] > 100uy && px[1] > 100uy && px[2] > 100uy)

                Expect.isTrue visible "section-exempt projected contours stay visible under clipping SectionPlane"

            testCase "ColorShader.packScopedUniforms + ColorRamp.scalarPassthrough scope LUT to UseColorLut batches without altering Axis3D/non-LUT pixels"
            <| fun () ->
                let wgsl = ColorShader.wgsl ()
                Expect.equal (WgpuContext.ValidateWgsl wgsl) (Ok ()) "scoped ColorShader.wgsl passes WgpuContext.ValidateWgsl"

                // 1. ColorRamp.scalarPassthrough encodes raw finite float32 scalars in R with neutral 1.0f G/B/A and omits non-finite / float32-overflow
                let passRamp = ColorRamp.scalarPassthrough
                let negMapped = ColorRamp.tryMap passRamp -2.8 |> ValueOption.get
                let posMapped = ColorRamp.tryMap passRamp 2.8 |> ValueOption.get
                expectRgbaClose 1e-6f negMapped (RgbaColor (-2.8f, 1.0f, 1.0f, 1.0f)) "scalarPassthrough preserves negative float32 scalar"
                expectRgbaClose 1e-6f posMapped (RgbaColor (2.8f, 1.0f, 1.0f, 1.0f)) "scalarPassthrough preserves >1 float32 scalar"
                Expect.equal (ColorRamp.tryMap passRamp Double.NaN) ValueNone "scalarPassthrough omits NaN"
                Expect.equal (ColorRamp.tryMap passRamp Double.PositiveInfinity) ValueNone "scalarPassthrough omits +Inf"
                Expect.equal (ColorRamp.tryMap passRamp 1e40) ValueNone "scalarPassthrough omits float32 overflow"

                let fb = RgbaColor (0.25f, 0.5f, 0.75f, 1.0f)
                let fallbackRamp = ColorRamp.scalarPassthroughWithMissing (MissingColorPolicy.Fallback fb)
                Expect.equal (ColorRamp.tryMap fallbackRamp Double.NaN) (ValueSome fb) "scalarPassthroughWithMissing uses fallback on NaN"
                Expect.equal (ColorRamp.stops 5 passRamp).Length 5 "ColorRamp.stops safely handles 1-element scalarPassthrough LUT"

                let coolWarm = ColorRamp.coolWarm (0.0, 1.0)
                let uDefaultScoped = ColorShader.packScopedUniforms -2.0f 2.0f coolWarm
                Expect.equal uDefaultScoped.Length 16 "packScopedUniforms emits 16 Vector4 slots"
                Expect.equal uDefaultScoped.Span[0] (Vector4 (-2.0f, 2.0f, 13.0f, 2.0f)) "slot 0 packs [cmin, cmax, stopCount, 2.0f]"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64, customWgsl = wgsl)
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let cam = Camera3D.orthographic Vector3.Zero 0.0f 85.0f 1.5f 6.0f

                // Left surface (world X in [-0.95, -0.05]) uses ColorRamp.scalarPassthrough; 4th-dim scalar varies linearly in [-2.0, +2.0] with Y
                let surfSpec: SurfaceSpec =
                    {
                        Scale = scale
                        Color = passRamp
                        Shading = ShadingMode.Unlit
                        Opacity = 1.0f
                        Wireframe = ValueNone
                        PickId = 1u
                    }

                let surfGrid =
                    SurfaceGrid.fromFunction 5 5 (-0.95, -0.05) (-0.8, 0.8) (fun _ _ -> 0.0) (ValueSome (fun _ y -> y * 2.5))
                    |> Result.defaultWith (fun errs -> failtestf "surfGrid failed: %A" errs)

                use surfBatch = SurfaceBatch.Create (ctx, surfSpec, surfGrid)

                // Right non-LUT panel box (world X in [+0.2, +0.9]) without UseColorLut
                use boxMesh = ctx.UploadMesh (Primitives.unitBox ())
                use panelBatch = ctx.CreateBatch (boxMesh, BlendMode.Opaque, false)
                let panelMat = Matrix4x4.CreateScale (0.7f, 0.2f, 1.2f) * Matrix4x4.CreateTranslation (0.55f, 0.0f, 0.0f)
                let panelColor = RgbaColor (0.2f, 0.85f, 0.35f, 1.0f)
                panelBatch.SetSingle (Instance3D.fromMatrix panelMat panelColor 0.5f 0u (InstanceFlags.Unlit ||| InstanceFlags.IgnoreSectionPlane) 0.0f)

                let surfScr = Camera3D.projectToScreen 64 64 cam (Scale3D.map scale (Point3.create -0.5 0.0 0.0)) |> ValueOption.get
                let panelScr = Camera3D.projectToScreen 64 64 cam (Vector3 (0.55f, 0.1f, 0.0f)) |> ValueOption.get
                let sx, sy = int (MathF.Round surfScr.X), int (MathF.Round surfScr.Y)
                let px, py = int (MathF.Round panelScr.X), int (MathF.Round panelScr.Y)

                let panelPassOnly =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| panelBatch |]

                ctx.SetUserUniforms ((ColorShader.disabledUniforms ()).Span)
                panelPassOnly |> WgpuContext.render ctx |> ignore
                let expectedPanel = pixelAt ctx px py

                let combinedPass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| panelBatch; surfBatch.Batch |]

                // Scrub 1: [-2.0, +2.0] -> scalar 0.0 at surfScr sits near neutral cool-warm midpoint
                ctx.SetUserUniforms (uDefaultScoped.Span)
                combinedPass |> WgpuContext.render ctx |> ignore
                let struct (mr, mg, mb, _) = pixelAt ctx sx sy
                Expect.equal (pixelAt ctx px py) expectedPanel "non-LUT panel pixel stays 100% invariant under packScopedUniforms [-2, 2]"
                Expect.isGreaterThan mr 150uy "scoped surface center (scalar=0 in [-2,2]) renders bright neutral"
                Expect.isLessThan (abs (int mr - int mb)) 20 "scoped surface center balanced R/B"
                Expect.isLessThan (abs (int mr - int mg)) 20 "scoped surface center balanced R/G"

                // Scrub 2: [0.0, 2.0] -> scalar 0.0 shifts to cool blue while non-LUT panel pixel remains identical
                ctx.SetUserUniforms ((ColorShader.packScopedUniforms 0.0f 2.0f coolWarm).Span)
                combinedPass |> WgpuContext.render ctx |> ignore
                let struct (cr, _, cb, _) = pixelAt ctx sx sy
                Expect.equal (pixelAt ctx px py) expectedPanel "non-LUT panel pixel stays 100% invariant when cmin=0.0f"
                Expect.isGreaterThan cb (cr + 40uy) "cmin=0.0f shifts scalar=0.0 surface pixel to cool blue"

                // Scrub 3: [-2.0, 0.0] -> scalar 0.0 shifts to warm red while non-LUT panel pixel remains identical
                ctx.SetUserUniforms ((ColorShader.packScopedUniforms -2.0f 0.0f coolWarm).Span)
                combinedPass |> WgpuContext.render ctx |> ignore
                let struct (wr, _, wb, _) = pixelAt ctx sx sy
                Expect.equal (pixelAt ctx px py) expectedPanel "non-LUT panel pixel stays 100% invariant when cmax=0.0f"
                Expect.isGreaterThan wr (wb + 40uy) "cmax=0.0f shifts scalar=0.0 surface pixel to warm red"

                // ExactColor ||| UseColorLut renders exact unlit LUT color without tone-mapping compression
                let exactRedRamp = ColorRamp.sequential (Scale.linear (0.0, 1.0) (0.0, 1.0)) [ Ranvier.Anime.ColorSpace.rgba 1.0 0.0 0.0 1.0; Ranvier.Anime.ColorSpace.rgba 1.0 0.0 0.0 1.0 ] ColorClipPolicy.clampAndOmit
                use exactMesh =
                    ctx.UploadMesh
                        {
                            Vertices =
                                [|
                                    Vertex3D (Vector3 (-1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4 (0.5f, 0.0f, 0.0f, 1.0f))
                                    Vertex3D (Vector3 (1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4 (0.5f, 0.0f, 0.0f, 1.0f))
                                    Vertex3D (Vector3 (1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4 (0.5f, 0.0f, 0.0f, 1.0f))
                                    Vertex3D (Vector3 (-1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4 (0.5f, 0.0f, 0.0f, 1.0f))
                                |]
                            Indices = [| 0u; 1u; 2u; 0u; 2u; 3u |]
                        }
                use exactBatch = ctx.CreateBatch (exactMesh, BlendMode.Opaque, true)
                exactBatch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 1u (InstanceFlags.ExactColor ||| InstanceFlags.UseColorLut))
                ctx.SetUserUniforms ((ColorShader.packScopedUniforms 0.0f 1.0f exactRedRamp).Span)
                ScenePass.create (Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.5f 4.0f)
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| exactBatch |]
                |> WgpuContext.render ctx
                |> ignore
                let struct (er, eg, eb, _) = pixelAt ctx 32 32
                Expect.equal er 255uy "ExactColor ||| UseColorLut renders exact saturated red R=255"
                Expect.equal eg 0uy "ExactColor ||| UseColorLut renders G=0"
                Expect.equal eb 0uy "ExactColor ||| UseColorLut renders B=0"

                // Switching SurfaceBatch.Update from scalarPassthrough -> CPU-baked ColorRamp clears UseColorLut flag on the single instance
                let heights = Array.create 25 0.0
                let scalars = [| for r in 0..4 do let y = -0.8 + 0.4 * float r in for _ in 0..4 -> y * 2.5 |]
                let flatCpuRamp = ColorRamp.sequential (Scale.linear (-2.0, 2.0) (0.0, 1.0)) [ Ranvier.Anime.ColorSpace.rgba 0.0 1.0 0.0 1.0; Ranvier.Anime.ColorSpace.rgba 0.0 1.0 0.0 1.0 ] ColorClipPolicy.clampAndOmit
                surfBatch.Update ({ surfSpec with Color = flatCpuRamp }, ReadOnlySpan<float>(heights), ReadOnlySpan<float>(scalars))
                ctx.SetUserUniforms ((ColorShader.packScopedUniforms -2.0f 0.0f coolWarm).Span)
                combinedPass |> WgpuContext.render ctx |> ignore
                let struct (srAfter, sgAfter, sbAfter, _) = pixelAt ctx sx sy
                Expect.isGreaterThan sgAfter (srAfter + 80uy) "switching back to CPU ColorRamp disables UseColorLut and renders pure green despite active packScopedUniforms"
                Expect.isGreaterThan sgAfter (sbAfter + 80uy) "switching back to CPU ColorRamp omits scoped LUT blue shift"
        ]

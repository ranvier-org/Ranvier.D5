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
        ]

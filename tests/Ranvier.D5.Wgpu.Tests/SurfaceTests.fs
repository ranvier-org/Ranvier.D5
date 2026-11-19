module Ranvier.D5.Wgpu.Tests.SurfaceTests

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

let private expectFiniteVec3 (v: Vector3) (msg: string) =
    Expect.isTrue
        (Single.IsFinite v.X && Single.IsFinite v.Y && Single.IsFinite v.Z)
        $"{msg} is finite ({v})"

let private pixelAt (ctx: WgpuContext) (x: int) (y: int) =
    let idx = (y * ctx.Width + x) * 4
    let px = ctx.RgbaPixels
    struct (px[idx], px[idx + 1], px[idx + 2], px[idx + 3])

let private unwrapOk msg =
    function
    | Ok value -> value
    | Error err -> failtestf "%s failed: %A" msg err

let private defaultSpec (scale: Scale3D) (ramp: ColorRamp) : SurfaceSpec =
    {
        Scale = scale
        Color = ramp
        Shading = ShadingMode.Lit (0.45f, 0.0f)
        Opacity = 1.0f
        Wireframe = ValueNone
        PickId = 42u
    }

[<Tests>]
let surfaceTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.Surface3D"
        [
            testCase "Flat ZUp plane z=0 has +UnitY normals and tilted z=x has upward-left normals matching CCW winding"
            <| fun () ->
                let scale = Scale3D.create BoxAspect.Cube UpAxis.ZUp (AxisScale3D.linear (0.0, 2.0)) (AxisScale3D.linear (0.0, 2.0)) (AxisScale3D.linear (0.0, 2.0))
                let ramp = ColorRamp.viridis (0.0, 2.0)
                let spec = defaultSpec scale ramp

                let flatGrid =
                    SurfaceGrid.fromFunction 5 5 (0.0, 2.0) (0.0, 2.0) (fun _ _ -> 0.0) ValueNone
                    |> unwrapOk "flatGrid"

                let flatMesh = Surface3D.build spec flatGrid
                let flatVerts = SurfaceMesh.vertices flatMesh
                let flatInds = SurfaceMesh.indices flatMesh
                Expect.equal flatVerts.Length 25 "5x5 flat grid has 25 vertices"
                Expect.equal flatInds.Length (4 * 4 * 2 * 3) "5x5 grid has 32 triangles (96 indices)"

                for i = 0 to flatVerts.Length - 1 do
                    let v = flatVerts.Span[i]
                    expectFiniteVec3 v.Position $"flat pos[{i}]"
                    expectVec3Close 1e-5f v.Normal Vector3.UnitY $"flat normal[{i}] points +UnitY"

                for t = 0 to (flatInds.Length / 3) - 1 do
                    let a = flatVerts.Span[int flatInds.Span[t * 3]].Position
                    let b = flatVerts.Span[int flatInds.Span[t * 3 + 1]].Position
                    let c = flatVerts.Span[int flatInds.Span[t * 3 + 2]].Position
                    let triN = Vector3.Normalize (Vector3.Cross (b - a, c - a))
                    expectVec3Close 1e-5f triN Vector3.UnitY $"flat tri[{t}] CCW winding aligns with +UnitY"

                // Tilted plane z = x (under Cube aspect dx=dy=dz=2, dWorld/dx = (+1, +1, 0), analytical normal = (-1/sqrt2, +1/sqrt2, 0))
                let tiltedGrid =
                    SurfaceGrid.fromFunction 5 5 (0.0, 2.0) (0.0, 2.0) (fun x _ -> x) ValueNone
                    |> unwrapOk "tiltedGrid"

                let tiltedMesh = Surface3D.build spec tiltedGrid
                let tiltedVerts = SurfaceMesh.vertices tiltedMesh
                let tiltedInds = SurfaceMesh.indices tiltedMesh
                let invSqrt2 = 1.0f / MathF.Sqrt 2.0f
                let expectedTiltedNormal = Vector3 (-invSqrt2, invSqrt2, 0.0f)

                for i = 0 to tiltedVerts.Length - 1 do
                    let v = tiltedVerts.Span[i]
                    expectFiniteVec3 v.Position $"tilted pos[{i}]"
                    expectFiniteVec3 v.Normal $"tilted normal[{i}]"
                    Expect.isLessThan v.Normal.X 0.0f $"tilted normal[{i}].X < 0"
                    Expect.isGreaterThan v.Normal.Y 0.0f $"tilted normal[{i}].Y > 0"
                    expectVec3Close 1e-5f v.Normal expectedTiltedNormal $"tilted analytical normal[{i}]"

                for t = 0 to (tiltedInds.Length / 3) - 1 do
                    let a = tiltedVerts.Span[int tiltedInds.Span[t * 3]].Position
                    let b = tiltedVerts.Span[int tiltedInds.Span[t * 3 + 1]].Position
                    let c = tiltedVerts.Span[int tiltedInds.Span[t * 3 + 2]].Position
                    let triN = Vector3.Normalize (Vector3.Cross (b - a, c - a))
                    expectVec3Close 1e-5f triN expectedTiltedNormal $"tilted tri[{t}] CCW winding aligns with upward-left normal"

                // Reversed xDomain (2.0, 0.0) in SurfaceGrid preserves upward-left normal and CCW winding
                let revGrid =
                    SurfaceGrid.fromFunction 5 5 (2.0, 0.0) (0.0, 2.0) (fun x _ -> x) ValueNone
                    |> unwrapOk "revGrid"

                let revMesh = Surface3D.build spec revGrid
                let revVerts = SurfaceMesh.vertices revMesh
                let revInds = SurfaceMesh.indices revMesh

                for i = 0 to revVerts.Length - 1 do
                    expectVec3Close 1e-5f revVerts.Span[i].Normal expectedTiltedNormal $"reversed xDomain normal[{i}]"

                for t = 0 to (revInds.Length / 3) - 1 do
                    let a = revVerts.Span[int revInds.Span[t * 3]].Position
                    let b = revVerts.Span[int revInds.Span[t * 3 + 1]].Position
                    let c = revVerts.Span[int revInds.Span[t * 3 + 2]].Position
                    let triN = Vector3.Normalize (Vector3.Cross (b - a, c - a))
                    expectVec3Close 1e-5f triN expectedTiltedNormal $"reversed xDomain tri[{t}] CCW winding"

                // Reversed X in Scale3D also preserves upward-left normal and CCW winding
                let revScale = Scale3D.create BoxAspect.Cube UpAxis.ZUp (AxisScale3D.linear (2.0, 0.0)) (AxisScale3D.linear (0.0, 2.0)) (AxisScale3D.linear (0.0, 2.0))
                let revScaleMesh = Surface3D.build { spec with Scale = revScale } flatGrid
                let revScaleVerts = SurfaceMesh.vertices revScaleMesh
                let revScaleInds = SurfaceMesh.indices revScaleMesh

                for i = 0 to revScaleVerts.Length - 1 do
                    expectVec3Close 1e-5f revScaleVerts.Span[i].Normal Vector3.UnitY $"reversed Scale3D X flat normal[{i}]"

                for t = 0 to (revScaleInds.Length / 3) - 1 do
                    let a = revScaleVerts.Span[int revScaleInds.Span[t * 3]].Position
                    let b = revScaleVerts.Span[int revScaleInds.Span[t * 3 + 1]].Position
                    let c = revScaleVerts.Span[int revScaleInds.Span[t * 3 + 2]].Position
                    let triN = Vector3.Normalize (Vector3.Cross (b - a, c - a))
                    expectVec3Close 1e-5f triN Vector3.UnitY $"reversed Scale3D X flat tri[{t}] CCW winding"

                // YUp plane z = 0 produces +UnitZ normals matching CCW winding
                let yupScale = Scale3D.create BoxAspect.Cube UpAxis.YUp (AxisScale3D.linear (0.0, 2.0)) (AxisScale3D.linear (0.0, 2.0)) (AxisScale3D.linear (0.0, 2.0))
                let yupMesh = Surface3D.build { spec with Scale = yupScale } flatGrid
                let yupVerts = SurfaceMesh.vertices yupMesh

                for i = 0 to yupVerts.Length - 1 do
                    expectVec3Close 1e-5f yupVerts.Span[i].Normal Vector3.UnitZ $"YUp flat normal[{i}] points +UnitZ"

            testCase "Center NaN hole omits 8 touching triangles, keeps neighbor normals finite, and SurfaceBatch updates holes and allocates 0 bytes on steady waves"
            <| fun () ->
                let cols, rows = 5, 5
                let scale = Scale3D.cartesian (0.0, 4.0) (0.0, 4.0) (-2.0, 2.0)
                let ramp = ColorRamp.viridis (-2.0, 2.0)
                let spec = { defaultSpec scale ramp with Wireframe = ValueSome (RgbaColor (1.0f, 1.0f, 1.0f, 0.5f)) }

                let heightsWithHole = Array.init (cols * rows) (fun idx ->
                    let c = idx % cols
                    let r = idx / cols
                    if c = 2 && r = 2 then Double.NaN
                    else 0.5 * float c)

                let holeGrid =
                    SurfaceGrid.uniform cols rows (0.0, 4.0) (0.0, 4.0) (ReadOnlySpan<float>(heightsWithHole)) ReadOnlySpan.Empty
                    |> unwrapOk "holeGrid"

                let holeMesh = Surface3D.build spec holeGrid
                let holeVerts = SurfaceMesh.vertices holeMesh
                let holeInds = SurfaceMesh.indices holeMesh
                let holeWire = SurfaceMesh.wireframe holeMesh

                // 16 cells total -> 4 cells touch (2, 2) -> 12 active cells -> 24 triangles (72 indices)
                Expect.equal (holeInds.Length / 3) 24 "5x5 grid with center NaN omits 8 triangles (24 remain)"
                Expect.isTrue (holeWire.Length > 0 && holeWire.Length % 2 = 0) "wireframe produces paired endpoints"

                let centerIdx = 2 * cols + 2
                for t = 0 to holeInds.Length - 1 do
                    Expect.notEqual (int holeInds.Span[t]) centerIdx "no triangle index references missing vertex (2,2)"

                // Adjacent neighbors use one-sided differences without NaN contamination
                for i = 0 to holeVerts.Length - 1 do
                    let v = holeVerts.Span[i]
                    expectFiniteVec3 v.Position $"holeMesh vertex[{i}].Position"
                    expectFiniteVec3 v.Normal $"holeMesh vertex[{i}].Normal"
                    Expect.floatClose { absolute = 1e-4; relative = 0.0 } (float (v.Normal.Length ())) 1.0 $"holeMesh vertex[{i}].Normal is unit"

                let expectedSlopeNormal =
                    let tangentU = Vector3 (0.5f, 1.0f / 6.0f, 0.0f) // dx=2 over 4 steps => 0.5; dz=2 over span 4 => 0.5 * 4/3 / 4 = 1/6
                    let tangentV = Vector3 (0.0f, 0.0f, -0.5f)
                    Vector3.Normalize (Vector3.Cross (tangentU, tangentV))

                for adj in [ 2 * cols + 1; 2 * cols + 3; 1 * cols + 2; 3 * cols + 2 ] do
                    expectVec3Close 1e-5f holeVerts.Span[adj].Normal expectedSlopeNormal $"adjacent vertex[{adj}] keeps exact one-sided normal"

                // Isolated vertex surrounded by 4 NaN neighbors falls back to Vector3.UnitY without NaNs
                let isolatedHeights = Array.init (cols * rows) (fun idx ->
                    let c = idx % cols
                    let r = idx / cols
                    if (c = 1 && r = 2) || (c = 3 && r = 2) || (c = 2 && r = 1) || (c = 2 && r = 3) then Double.NaN
                    else 0.0)

                let isolatedGrid =
                    SurfaceGrid.uniform cols rows (0.0, 4.0) (0.0, 4.0) (ReadOnlySpan<float>(isolatedHeights)) ReadOnlySpan.Empty
                    |> unwrapOk "isolatedGrid"

                let isolatedMesh = Surface3D.build spec isolatedGrid
                expectVec3Close 1e-5f (SurfaceMesh.vertices isolatedMesh).Span[centerIdx].Normal Vector3.UnitY "isolated vertex falls back to Vector3.UnitY"

                // SurfaceBatch hole-mask transitions: 0 NaNs -> 1 NaN -> 0 NaNs, plus zero-alloc steady-state wave update
                let fullHeights = Array.zeroCreate<float> (cols * rows)
                let initialGrid =
                    SurfaceGrid.uniform cols rows (0.0, 4.0) (0.0, 4.0) (ReadOnlySpan<float>(fullHeights)) ReadOnlySpan.Empty
                    |> unwrapOk "initialGrid"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use surfaceBatch = SurfaceBatch.Create (ctx, spec, initialGrid)
                Expect.isTrue surfaceBatch.WireframeBatch.IsSome "WireframeBatch created when Wireframe is ValueSome"
                Expect.equal surfaceBatch.Batch.Mesh.TriangleCount 32 "initial 5x5 grid has 32 triangles"

                // 0 NaNs -> 1 NaN -> all NaNs (0 tris) -> 0 NaNs
                surfaceBatch.Update (spec, ReadOnlySpan<float>(heightsWithHole), ReadOnlySpan.Empty)
                Expect.equal surfaceBatch.Batch.Mesh.TriangleCount 24 "1 NaN reduces TriangleCount from 32 to 24"

                let allNanHeights = Array.create (cols * rows) Double.NaN
                surfaceBatch.Update ({ spec with Wireframe = ValueNone }, ReadOnlySpan<float>(allNanHeights), ReadOnlySpan.Empty)
                Expect.equal surfaceBatch.Batch.Mesh.TriangleCount 0 "all NaNs sets TriangleCount to 0"
                Expect.equal surfaceBatch.WireframeBatch.Value.SegmentCount 0 "Wireframe = ValueNone clears wireframe segments"

                let camEmpty = Camera3D.orthographic Vector3.Zero 0.0f 85.0f 1.5f 6.0f
                ScenePass.create camEmpty
                |> ScenePass.withBatches [| surfaceBatch.Batch |]
                |> ScenePass.withLines [| surfaceBatch.WireframeBatch.Value |]
                |> WgpuContext.render ctx
                |> ignore

                surfaceBatch.Update (spec, ReadOnlySpan<float>(fullHeights), ReadOnlySpan.Empty)
                Expect.equal surfaceBatch.Batch.Mesh.TriangleCount 32 "restoring 0 NaNs restores TriangleCount back to 32"
                Expect.isGreaterThan surfaceBatch.WireframeBatch.Value.SegmentCount 0 "restoring Wireframe restores segments"

                // AlphaBlend when Opacity < 1.0f
                use alphaBatch = SurfaceBatch.Create (ctx, { spec with Opacity = 0.5f }, initialGrid)
                Expect.equal alphaBatch.Batch.BlendMode BlendMode.AlphaBlend "Opacity < 1 uses AlphaBlend"
                Expect.isTrue alphaBatch.Batch.DoubleSided "SurfaceBatch is double-sided"

                // Zero-allocation steady-state wave sin(x + t) across 20 frames after 1 warmup call (both with wireframe & without)
                let waveCols, waveRows = 32, 32
                let waveCount = waveCols * waveRows
                let waveHeights = Array.zeroCreate<float> waveCount
                let waveScalars = Array.zeroCreate<float> waveCount

                let fillWave (t: float) =
                    for r = 0 to waveRows - 1 do
                        let y = float r / float (waveRows - 1) * 4.0
                        for c = 0 to waveCols - 1 do
                            let x = float c / float (waveCols - 1) * 4.0
                            let idx = r * waveCols + c
                            waveHeights[idx] <- sin (x + t) * cos (y * 0.5)
                            waveScalars[idx] <- cos (x - t)

                fillWave 0.0
                let waveGrid =
                    SurfaceGrid.uniform waveCols waveRows (0.0, 4.0) (0.0, 4.0) (ReadOnlySpan<float>(waveHeights)) (ReadOnlySpan<float>(waveScalars))
                    |> unwrapOk "waveGrid"

                // Test without wireframe
                let waveSpecNoWire = { spec with Wireframe = ValueNone }
                use waveBatchNoWire = SurfaceBatch.Create (ctx, waveSpecNoWire, waveGrid)
                Expect.isTrue waveBatchNoWire.WireframeBatch.IsNone "WireframeBatch is None when Wireframe is ValueNone"

                fillWave 0.1
                waveBatchNoWire.Update (waveSpecNoWire, ReadOnlySpan<float>(waveHeights), ReadOnlySpan<float>(waveScalars))

                let beforeNoWire = GC.GetAllocatedBytesForCurrentThread ()
                for frame = 1 to 20 do
                    let t = 0.1 * float (frame + 1)
                    fillWave t
                    waveBatchNoWire.Update (waveSpecNoWire, ReadOnlySpan<float>(waveHeights), ReadOnlySpan<float>(waveScalars))
                let afterNoWire = GC.GetAllocatedBytesForCurrentThread ()
                Expect.equal (afterNoWire - beforeNoWire) 0L "20 warmed SurfaceBatch.Update frames allocate 0 bytes (no wireframe)"

                // Test with wireframe
                use waveBatchWithWire = SurfaceBatch.Create (ctx, spec, waveGrid)
                fillWave 0.1
                waveBatchWithWire.Update (spec, ReadOnlySpan<float>(waveHeights), ReadOnlySpan<float>(waveScalars))

                let beforeWire = GC.GetAllocatedBytesForCurrentThread ()
                for frame = 1 to 20 do
                    let t = 0.1 * float (frame + 1)
                    fillWave t
                    waveBatchWithWire.Update (spec, ReadOnlySpan<float>(waveHeights), ReadOnlySpan<float>(waveScalars))
                let afterWire = GC.GetAllocatedBytesForCurrentThread ()
                Expect.equal (afterWire - beforeWire) 0L "20 warmed SurfaceBatch.Update frames allocate 0 bytes (with wireframe)"

                // Also test UpdateGrid with resized grid
                let biggerGrid =
                    SurfaceGrid.fromFunction 8 8 (0.0, 4.0) (0.0, 4.0) (fun x y -> sin x * cos y) ValueNone
                    |> unwrapOk "biggerGrid"
                waveBatchWithWire.UpdateGrid (spec, biggerGrid)
                Expect.equal waveBatchWithWire.Batch.Mesh.TriangleCount (7 * 7 * 2) "UpdateGrid resizes mesh triangle count"

            testCase "Independent 4th-dimension scalars color constant-height z=1 plane with Unlit ColorRamp output on WgpuContext"
            <| fun () ->
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (0.0, 2.0)
                let coolWarm = ColorRamp.coolWarm (-1.0, 1.0)
                let spec =
                    {
                        Scale = scale
                        Color = coolWarm
                        Shading = ShadingMode.Unlit
                        Opacity = 1.0f
                        Wireframe = ValueNone
                        PickId = 99u
                    }

                // Constant height z = 1.0 (worldY = 0.0f), 4th-dim scalar varies linearly with x in [-1, 1]
                let grid =
                    SurfaceGrid.fromFunction 9 9 (-1.0, 1.0) (-1.0, 1.0) (fun _ _ -> 1.0) (ValueSome (fun x _ -> x))
                    |> unwrapOk "scalarPlane"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use surfaceBatch = SurfaceBatch.Create (ctx, spec, grid)

                let cam = Camera3D.orthographic Vector3.Zero 0.0f 85.0f 1.2f 6.0f
                let pass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| surfaceBatch.Batch |]

                let metrics = pass |> WgpuContext.render ctx
                Expect.equal metrics.DrawCalls 1 "surface batch issues 1 draw call"

                let struct (lr, lg, lb, _) = pixelAt ctx 14 32
                let struct (cr, cg, cb, _) = pixelAt ctx 32 32
                let struct (rr, rg, rb, _) = pixelAt ctx 50 32

                Expect.isGreaterThan lb (lr + 40uy) "left pixel (scalar < 0) renders cool blue"
                Expect.isGreaterThan rr (rb + 40uy) "right pixel (scalar > 0) renders warm red"
                Expect.isGreaterThan cr 150uy "center pixel (scalar = 0) renders bright neutral"
                Expect.isLessThan (abs (int cr - int cb)) 20 "center pixel balanced R/B"
                Expect.isLessThan (abs (int cr - int cg)) 20 "center pixel balanced R/G"

                // Verify Unlit matches stock Unlit quad rendered with ColorRamp midpoint without directional darkening
                let midExpected = ColorRamp.tryMap coolWarm 0.0 |> ValueOption.get
                use quadMesh =
                    ctx.UploadMesh
                        {
                            Vertices =
                                [|
                                    Vertex3D (Vector3 (-1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (1.0f, -1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                    Vertex3D (Vector3 (-1.0f, 1.0f, 0.0f), Vector3.UnitZ, Vector4.One)
                                |]
                            Indices = [| 0u; 1u; 2u; 0u; 2u; 3u |]
                        }
                use quadBatch = ctx.CreateBatch (quadMesh, BlendMode.Opaque, true)
                quadBatch.SetSingle (Instance3D.identity midExpected 0.5f 1u InstanceFlags.Unlit)

                let flatCam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.5f 4.0f
                ScenePass.create flatCam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| quadBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (er, eg, eb, _) = pixelAt ctx 32 32
                Expect.isLessThanOrEqual (abs (int cr - int er)) 2 "Unlit surface center R matches ColorRamp tone-mapped output"
                Expect.isLessThanOrEqual (abs (int cg - int eg)) 2 "Unlit surface center G matches ColorRamp tone-mapped output"
                Expect.isLessThanOrEqual (abs (int cb - int eb)) 2 "Unlit surface center B matches ColorRamp tone-mapped output"

                // GPU picking returns surface PickId
                let hit = pass |> WgpuContext.pick 32 32 ctx
                Expect.equal (hit |> ValueOption.map _.PickId) (ValueSome 99u) "GPU picking returns surface PickId"

            testCase "Rectilinear, curvilinear, Delaunay triangulatePoints, and grid validation errors work cleanly"
            <| fun () ->
                let scale = Scale3D.create BoxAspect.Cube UpAxis.ZUp (AxisScale3D.linear (0.0, 1.0)) (AxisScale3D.linear (0.0, 1.0)) (AxisScale3D.linear (0.0, 1.0))
                let ramp = ColorRamp.viridis (0.0, 1.0)
                let spec = { defaultSpec scale ramp with Wireframe = ValueSome RgbaColor.white }

                // Rectilinear non-uniform grid
                let xs = [| 0.0; 0.2; 1.0 |]
                let ys = [| 0.0; 0.7; 1.0 |]
                let rHeights = [| for y in ys do for x in xs -> x |]
                let rectGrid =
                    SurfaceGrid.rectilinear (ReadOnlySpan<float>(xs)) (ReadOnlySpan<float>(ys)) (ReadOnlySpan<float>(rHeights)) ReadOnlySpan.Empty
                    |> unwrapOk "rectGrid"
                let rectMesh = Surface3D.build spec rectGrid
                let rectVerts = SurfaceMesh.vertices rectMesh
                let invSqrt2 = 1.0f / MathF.Sqrt 2.0f
                for i = 0 to rectVerts.Length - 1 do
                    expectVec3Close 1e-5f rectVerts.Span[i].Normal (Vector3 (-invSqrt2, invSqrt2, 0.0f)) $"rectilinear non-uniform z=x normal[{i}]"

                // Curvilinear grid
                let curviPts =
                    [|
                        for r = 0 to 2 do
                            for c = 0 to 2 do
                                let u = float c * 0.5
                                let v = float r * 0.5
                                yield Point3.create u v (0.5 * u)
                    |]
                let curviGrid =
                    SurfaceGrid.curvilinear 3 3 (ReadOnlySpan<Point3>(curviPts)) ReadOnlySpan.Empty
                    |> unwrapOk "curviGrid"
                let curviMesh = Surface3D.build spec curviGrid
                Expect.equal (SurfaceMesh.indices curviMesh).Length (4 * 2 * 3) "3x3 curvilinear grid produces 8 triangles"

                // Delaunay triangulatePoints over (X, Y) lifted by Z with independent scalars
                let delPts : System.Collections.Generic.IReadOnlyList<Point3> =
                    [|
                        Point3.create 0.0 0.0 0.0
                        Point3.create 1.0 0.0 0.0
                        Point3.create 1.0 1.0 0.0
                        Point3.create 0.0 1.0 0.0
                        Point3.create 0.5 0.5 1.0
                    |]
                let delScalars = [| 0.0; 0.25; 0.5; 0.75; 1.0 |]
                let delMesh =
                    Surface3D.triangulatePoints spec delPts (ReadOnlySpan<float>(delScalars))
                    |> unwrapOk "delMesh"

                let delVerts = SurfaceMesh.vertices delMesh
                let delInds = SurfaceMesh.indices delMesh
                let delWire = SurfaceMesh.wireframe delMesh
                Expect.equal delVerts.Length 5 "5 Delaunay sites"
                Expect.equal (delInds.Length / 3) 4 "square with center peak produces 4 Delaunay triangles"
                Expect.equal (delWire.Length / 2) 8 "4 triangles around center peak share 8 unique wireframe edges"
                expectVec3Close 1e-5f delVerts.Span[4].Normal Vector3.UnitY "symmetric pyramid peak normal points +UnitY"

                let meshData = SurfaceMesh.toMeshData delMesh
                Expect.equal meshData.Vertices.Length 5 "toMeshData copies vertices"
                Expect.equal meshData.Indices.Length delInds.Length "toMeshData copies indices"

                // Delaunay scalar NaN omits touching triangles while keeping remaining vertices finite
                let delHoleMesh =
                    Surface3D.triangulatePoints spec delPts (ReadOnlySpan<float> [| Double.NaN; 0.25; 0.5; 0.75; 1.0 |])
                    |> unwrapOk "delHoleMesh"
                Expect.equal ((SurfaceMesh.indices delHoleMesh).Length / 3) 2 "omitting one corner of square+center omits 2 of 4 triangles"
                for i = 0 to (SurfaceMesh.vertices delHoleMesh).Length - 1 do
                    let v = (SurfaceMesh.vertices delHoleMesh).Span[i]
                    expectFiniteVec3 v.Position $"delHole vertex[{i}].Position"
                    expectFiniteVec3 v.Normal $"delHole vertex[{i}].Normal"

                Expect.equal (SurfaceGrid.columns rectGrid) 3 "SurfaceGrid.columns"
                Expect.equal (SurfaceGrid.rows rectGrid) 3 "SurfaceGrid.rows"

                // Validation errors
                Expect.isError (SurfaceGrid.uniform 1 4 (0.0, 1.0) (0.0, 1.0) (ReadOnlySpan<float> [| 0.0; 1.0; 2.0; 3.0 |]) ReadOnlySpan.Empty) "columns < 2 returns Error"
                Expect.isError (SurfaceGrid.uniform 2 2 (0.0, 1.0) (0.0, 1.0) (ReadOnlySpan<float> [| 0.0; 1.0 |]) ReadOnlySpan.Empty) "mismatched height length returns Error"
                Expect.isError (SurfaceGrid.uniform 2 2 (0.0, 1.0) (0.0, 1.0) (ReadOnlySpan<float> [| 0.0; 1.0; 2.0; 3.0 |]) (ReadOnlySpan<float> [| 1.0 |])) "mismatched scalar length returns Error"
                Expect.isError (SurfaceGrid.rectilinear (ReadOnlySpan<float> [| 0.0; 2.0; 1.0 |]) (ReadOnlySpan<float> [| 0.0; 1.0 |]) (ReadOnlySpan<float>(Array.zeroCreate 6)) ReadOnlySpan.Empty) "non-monotone xs returns Error"
                Expect.isError (Surface3D.triangulatePoints spec ([| Point3.create 0.0 0.0 0.0; Point3.create 1.0 0.0 0.0 |] :> System.Collections.Generic.IReadOnlyList<Point3>) ReadOnlySpan.Empty) "< 3 points returns Error"
                Expect.isError (Surface3D.triangulatePoints spec ([| Point3.create 0.0 0.0 0.0; Point3.create 0.5 0.5 0.0; Point3.create 1.0 1.0 0.0 |] :> System.Collections.Generic.IReadOnlyList<Point3>) ReadOnlySpan.Empty) "collinear points returns Error"

                match SurfaceGrid.fromFunction 20000 20000 (0.0, 1.0) (0.0, 1.0) (fun x _ -> x) ValueNone with
                | Error [ LayoutError.BudgetExceeded _ ] -> ()
                | other -> failtestf "Expected BudgetExceeded for oversized 20000x20000 grid indices, got %A" other
        ]

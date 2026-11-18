module Ranvier.D5.Wgpu.Tests.VolumeTests

open System
open System.Collections.Generic
open System.Numerics
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.Wgpu

let private pixelAt (ctx: WgpuContext) (x: int) (y: int) =
    let idx = (y * ctx.Width + x) * 4
    let px = ctx.RgbaPixels
    struct (px[idx], px[idx + 1], px[idx + 2], px[idx + 3])

let private expectFiniteVec3 (v: Vector3) (msg: string) =
    Expect.isTrue
        (Single.IsFinite v.X && Single.IsFinite v.Y && Single.IsFinite v.Z)
        $"{msg} is finite ({v})"

let private unwrapOk msg =
    function
    | Ok value -> value
    | Error err -> failtestf "%s failed: %A" msg err

let private verifyClosedManifoldByPosition (mesh: MeshData) (label: string) =
    let quantize (v: Vector3) =
        struct (int (MathF.Round (v.X * 10000.0f)), int (MathF.Round (v.Y * 10000.0f)), int (MathF.Round (v.Z * 10000.0f)))

    let counts = Dictionary<struct (int * int * int) * struct (int * int * int), int>()
    let triCount = mesh.Indices.Length / 3

    for t = 0 to triCount - 1 do
        let p0 = quantize mesh.Vertices[int mesh.Indices[t * 3]].Position
        let p1 = quantize mesh.Vertices[int mesh.Indices[t * 3 + 1]].Position
        let p2 = quantize mesh.Vertices[int mesh.Indices[t * 3 + 2]].Position

        for (a, b) in [ (p0, p1); (p1, p2); (p2, p0) ] do
            let key = (a, b)
            let cur = if counts.ContainsKey key then counts[key] else 0
            counts[key] <- cur + 1

    let mutable openEdges = 0

    for kv in counts do
        let (a, b) = kv.Key
        let rev = (b, a)
        let revCount = if counts.ContainsKey rev then counts[rev] else 0

        if kv.Value <> revCount then
            openEdges <- openEdges + 1

    Expect.equal openEdges 0 $"{label} has 0 unmatched boundary half-edges"

let private verifyOutwardWinding (mesh: MeshData) (label: string) =
    Expect.isGreaterThan mesh.Vertices.Length 0 $"{label} has vertices"
    Expect.isGreaterThan mesh.Indices.Length 0 $"{label} has indices"
    Expect.equal (mesh.Indices.Length % 3) 0 $"{label} index count divisible by 3"

    for i = 0 to mesh.Vertices.Length - 1 do
        let v = mesh.Vertices[i]
        expectFiniteVec3 v.Position $"{label} v[{i}].Position"
        expectFiniteVec3 v.Normal $"{label} v[{i}].Normal"
        Expect.floatClose { absolute = 1e-3; relative = 0.0 } (float (v.Normal.Length ())) 1.0 $"{label} v[{i}].Normal is unit"

    for t = 0 to (mesh.Indices.Length / 3) - 1 do
        let i0 = int mesh.Indices[t * 3]
        let i1 = int mesh.Indices[t * 3 + 1]
        let i2 = int mesh.Indices[t * 3 + 2]
        Expect.isTrue (i0 <> i1 && i1 <> i2 && i2 <> i0) $"{label} tri[{t}] has distinct vertex indices"
        let v0 = mesh.Vertices[i0]
        let v1 = mesh.Vertices[i1]
        let v2 = mesh.Vertices[i2]
        let cross = Vector3.Cross (v1.Position - v0.Position, v2.Position - v0.Position)
        Expect.isGreaterThan (cross.LengthSquared ()) 1e-14f $"{label} tri[{t}] is non-degenerate"
        let triN = Vector3.Normalize cross
        let avgN = Vector3.Normalize (v0.Normal + v1.Normal + v2.Normal)
        Expect.isGreaterThan (Vector3.Dot (triN, avgN)) 0.5f $"{label} tri[{t}] CCW winding aligns with outward normals"

[<Tests>]
let volumeTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.Volume3D"
        [
            testCase "IsoSurface3D extracts closed unit sphere with outward normals and front-facing CCW winding across ZUp, YUp, and reversed axes"
            <| fun () ->
                let bounds = Bounds3.create -1.5 -1.5 -1.5 3.0 3.0 3.0

                let sphereGrid =
                    VolumeGrid.fromFunction
                        24
                        24
                        24
                        bounds
                        (fun x y z -> 1.0f - float32 (x * x + y * y + z * z))
                        ValueNone
                    |> unwrapOk "sphereGrid"

                Expect.equal (VolumeGrid.dimensions sphereGrid) struct (24, 24, 24) "VolumeGrid.dimensions"
                Expect.equal (VolumeGrid.bounds sphereGrid) bounds "VolumeGrid.bounds"

                let unitWorldScale =
                    Scale3D.create
                        (BoxAspect.Manual (3.0, 3.0, 3.0))
                        UpAxis.ZUp
                        (AxisScale3D.linear (-1.5, 1.5))
                        (AxisScale3D.linear (-1.5, 1.5))
                        (AxisScale3D.linear (-1.5, 1.5))

                let ramp = ColorRamp.viridis (-1.0, 1.0)
                let cfg = IsoSurfaceConfig.create 0.0f

                let mesh =
                    IsoSurface3D.compute unitWorldScale ramp cfg sphereGrid
                    |> unwrapOk "IsoSurface3D sphere"

                Expect.isLessThan mesh.Vertices.Length (mesh.Indices.Length / 2) "shared-edge deduplication reduces vertices well below half index count"
                verifyOutwardWinding mesh "unitWorldScale ZUp sphere"
                verifyClosedManifoldByPosition mesh "unitWorldScale ZUp sphere"

                for i = 0 to mesh.Vertices.Length - 1 do
                    let v = mesh.Vertices[i]
                    let r = v.Position.Length ()
                    Expect.isLessThanOrEqual (abs (r - 1.0f)) 0.08f $"vertex[{i}] lies within 0.08f of world radius 1.0 (got {r})"
                    let dir = Vector3.Normalize v.Position
                    Expect.isGreaterThan (Vector3.Dot (dir, v.Normal)) 0.8f $"vertex[{i}] has outward normal (dot={Vector3.Dot (dir, v.Normal)})"

                // Verify YUp, ChartDefault (anisotropic), and reversed X axis preserve outward CCW winding
                for label, altScale in
                    [
                        "YUp Manual",
                        Scale3D.create
                            (BoxAspect.Manual (3.0, 3.0, 3.0))
                            UpAxis.YUp
                            (AxisScale3D.linear (-1.5, 1.5))
                            (AxisScale3D.linear (-1.5, 1.5))
                            (AxisScale3D.linear (-1.5, 1.5))

                        "ZUp ChartDefault",
                        Scale3D.cartesian (-1.5, 1.5) (-1.5, 1.5) (-1.5, 1.5)

                        "ZUp Reversed X",
                        Scale3D.create
                            BoxAspect.Cube
                            UpAxis.ZUp
                            (AxisScale3D.linear (1.5, -1.5))
                            (AxisScale3D.linear (-1.5, 1.5))
                            (AxisScale3D.linear (-1.5, 1.5))
                    ] do
                    let altMesh =
                        IsoSurface3D.compute altScale ramp cfg sphereGrid
                        |> unwrapOk label

                    verifyOutwardWinding altMesh label
                    verifyClosedManifoldByPosition altMesh label

                // Render single-sided (doubleSided = false) opaque mesh on WgpuContext to prove front-facing CCW winding is not culled
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use gpuMesh = ctx.UploadMesh mesh
                use batch = ctx.CreateBatch (gpuMesh, BlendMode.Opaque, false)
                batch.SetSingle (Instance3D.identity RgbaColor.white 0.4f 17u InstanceFlags.None)
                let cam = Camera3D.orthographic Vector3.Zero 25.0f 20.0f 1.5f 6.0f

                let pass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| batch |]

                let metrics = pass |> WgpuContext.render ctx
                Expect.equal metrics.DrawCalls 1 "sphere renders in 1 draw call"
                let struct (cr, cg, cb, _) = pixelAt ctx 32 32
                Expect.isTrue (cr > 20uy || cg > 20uy || cb > 20uy) "center pixel of single-sided sphere renders visibly"
                let hit = pass |> WgpuContext.pick 32 32 ctx
                Expect.equal (hit |> ValueOption.map _.PickId) (ValueSome 17u) "single-sided sphere picked at center"

            testCase "Clipped octant sphere with CapBoundaries = true seals x=0, y=0, and z=0 boundary faces inside Bounds3"
            <| fun () ->
                let bounds = Bounds3.create 0.0 0.0 0.0 1.5 1.5 1.5

                let octantGrid =
                    VolumeGrid.fromFunction
                        16
                        16
                        16
                        bounds
                        (fun x y z -> 1.0f - float32 (x * x + y * y + z * z))
                        ValueNone
                    |> unwrapOk "octantGrid"

                let scale =
                    Scale3D.create
                        (BoxAspect.Manual (1.5, 1.5, 1.5))
                        UpAxis.ZUp
                        (AxisScale3D.linear (0.0, 1.5))
                        (AxisScale3D.linear (0.0, 1.5))
                        (AxisScale3D.linear (0.0, 1.5))

                let ramp = ColorRamp.viridis (-1.0, 1.0)
                let uncappedCfg = { IsoSurfaceConfig.create 0.0f with CapBoundaries = false }
                let cappedCfg = IsoSurfaceConfig.create 0.0f

                let uncappedMesh = IsoSurface3D.compute scale ramp uncappedCfg octantGrid |> unwrapOk "uncapped"
                let cappedMesh = IsoSurface3D.compute scale ramp cappedCfg octantGrid |> unwrapOk "capped"

                Expect.isGreaterThan cappedMesh.Indices.Length uncappedMesh.Indices.Length "CapBoundaries=true produces more triangles than CapBoundaries=false"
                verifyOutwardWinding cappedMesh "capped octant sphere"
                verifyClosedManifoldByPosition cappedMesh "capped octant sphere"

                // Under ZUp on [0, 1.5]^3 mapped to [-0.75, +0.75]^3:
                //   data x = 0 -> worldX = -0.75 (outward -UnitX)
                //   data y = 0 -> worldZ = +0.75 (outward +UnitZ)
                //   data z = 0 -> worldY = -0.75 (outward -UnitY)
                let worldBounds = Scale3D.worldBounds scale
                let minWX = float32 (Bounds3.minX worldBounds)
                let maxWX = float32 (Bounds3.maxX worldBounds)
                let minWY = float32 (Bounds3.minY worldBounds)
                let maxWY = float32 (Bounds3.maxY worldBounds)
                let minWZ = float32 (Bounds3.minZ worldBounds)
                let maxWZ = float32 (Bounds3.maxZ worldBounds)

                let mutable x0CapTris = 0
                let mutable y0CapTris = 0
                let mutable z0CapTris = 0

                for i = 0 to cappedMesh.Vertices.Length - 1 do
                    let p = cappedMesh.Vertices[i].Position
                    Expect.isTrue (p.X >= minWX - 1e-4f && p.X <= maxWX + 1e-4f) $"vertex[{i}].X within Bounds3 ({p.X})"
                    Expect.isTrue (p.Y >= minWY - 1e-4f && p.Y <= maxWY + 1e-4f) $"vertex[{i}].Y within Bounds3 ({p.Y})"
                    Expect.isTrue (p.Z >= minWZ - 1e-4f && p.Z <= maxWZ + 1e-4f) $"vertex[{i}].Z within Bounds3 ({p.Z})"

                for t = 0 to (cappedMesh.Indices.Length / 3) - 1 do
                    let p0 = cappedMesh.Vertices[int cappedMesh.Indices[t * 3]].Position
                    let p1 = cappedMesh.Vertices[int cappedMesh.Indices[t * 3 + 1]].Position
                    let p2 = cappedMesh.Vertices[int cappedMesh.Indices[t * 3 + 2]].Position

                    if abs (p0.X - minWX) < 1e-4f && abs (p1.X - minWX) < 1e-4f && abs (p2.X - minWX) < 1e-4f then
                        x0CapTris <- x0CapTris + 1

                    if abs (p0.Z - maxWZ) < 1e-4f && abs (p1.Z - maxWZ) < 1e-4f && abs (p2.Z - maxWZ) < 1e-4f then
                        y0CapTris <- y0CapTris + 1

                    if abs (p0.Y - minWY) < 1e-4f && abs (p1.Y - minWY) < 1e-4f && abs (p2.Y - minWY) < 1e-4f then
                        z0CapTris <- z0CapTris + 1

                Expect.isGreaterThan x0CapTris 0 "sealing triangles emitted on x = 0 face"
                Expect.isGreaterThan y0CapTris 0 "sealing triangles emitted on y = 0 face"
                Expect.isGreaterThan z0CapTris 0 "sealing triangles emitted on z = 0 face"

                // Render capped octant looking toward the x=0/y=0 cut faces from (-X, -Y, +Z) with single-sided CullMode.Back
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use gpuMesh = ctx.UploadMesh cappedMesh
                use batch = ctx.CreateBatch (gpuMesh, BlendMode.Opaque, false)
                batch.SetSingle (Instance3D.identity RgbaColor.white 0.4f 23u InstanceFlags.Unlit)
                let cam = Camera3D.orthographic Vector3.Zero -45.0f -35.0f 1.2f 6.0f

                let pass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| batch |]

                pass |> WgpuContext.render ctx |> ignore
                let struct (cr, cg, cb, _) = pixelAt ctx 32 32
                Expect.isTrue (cr > 20uy || cg > 20uy || cb > 20uy) "capped boundary faces render visibly with CullMode.Back"

                // Also verify maxX/maxY/maxZ boundary face capping on [-1.5, 0.0]^3 under YUp
                let negOctBounds = Bounds3.create -1.5 -1.5 -1.5 1.5 1.5 1.5

                let negOctGrid =
                    VolumeGrid.fromFunction
                        16
                        16
                        16
                        negOctBounds
                        (fun x y z -> 1.0f - float32 (x * x + y * y + z * z))
                        ValueNone
                    |> unwrapOk "negOctGrid"

                let negOctScale =
                    Scale3D.create
                        (BoxAspect.Manual (1.5, 1.5, 1.5))
                        UpAxis.YUp
                        (AxisScale3D.linear (-1.5, 0.0))
                        (AxisScale3D.linear (-1.5, 0.0))
                        (AxisScale3D.linear (-1.5, 0.0))

                let negUncapped = IsoSurface3D.compute negOctScale ramp uncappedCfg negOctGrid |> unwrapOk "negUncapped"
                let negCapped = IsoSurface3D.compute negOctScale ramp cappedCfg negOctGrid |> unwrapOk "negCapped"
                Expect.isGreaterThan negCapped.Indices.Length negUncapped.Indices.Length "max-face CapBoundaries=true emits sealing triangles"
                verifyOutwardWinding negCapped "capped negative octant YUp"
                verifyClosedManifoldByPosition negCapped "capped negative octant YUp"

            testCase "Secondary colorScalar colors isosurface independently, MaxTriangles/MaxVertices enforce budgets, and CancellationToken aborts"
            <| fun () ->
                let bounds = Bounds3.create -1.5 -1.5 -1.5 3.0 3.0 3.0

                let grid =
                    VolumeGrid.fromFunction
                        16
                        16
                        16
                        bounds
                        (fun x y z -> 1.0f - float32 (x * x + y * y + z * z))
                        (ValueSome (fun x _ _ -> float32 x))
                    |> unwrapOk "secondaryScalarGrid"

                let scale =
                    Scale3D.create
                        (BoxAspect.Manual (3.0, 3.0, 3.0))
                        UpAxis.ZUp
                        (AxisScale3D.linear (-1.5, 1.5))
                        (AxisScale3D.linear (-1.5, 1.5))
                        (AxisScale3D.linear (-1.5, 1.5))

                let coolWarm = ColorRamp.coolWarm (-1.0, 1.0)
                let cfg = IsoSurfaceConfig.create 0.0f

                let mesh =
                    IsoSurface3D.compute scale coolWarm cfg grid
                    |> unwrapOk "coloredSphere"

                let mutable sawCoolLeft = false
                let mutable sawWarmRight = false

                for i = 0 to mesh.Vertices.Length - 1 do
                    let v = mesh.Vertices[i]

                    if v.Position.X < -0.7f then
                        Expect.isGreaterThan v.Color.Z (v.Color.X + 0.15f) $"left vertex (x={v.Position.X}) colored cool blue"
                        sawCoolLeft <- true
                    elif v.Position.X > 0.7f then
                        Expect.isGreaterThan v.Color.X (v.Color.Z + 0.15f) $"right vertex (x={v.Position.X}) colored warm red"
                        sawWarmRight <- true

                Expect.isTrue sawCoolLeft "left hemisphere vertices tested"
                Expect.isTrue sawWarmRight "right hemisphere vertices tested"

                // Render Unlit on WgpuContext and check left blue vs right red pixels
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use gpuMesh = ctx.UploadMesh mesh
                use batch = ctx.CreateBatch (gpuMesh, BlendMode.Opaque, false)
                batch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 31u InstanceFlags.Unlit)
                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.5f 6.0f

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| batch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (lr, _, lb, _) = pixelAt ctx 18 32
                let struct (rr, _, rb, _) = pixelAt ctx 46 32
                Expect.isGreaterThan lb (lr + 40uy) "left rendered pixel is cool blue"
                Expect.isGreaterThan rr (rb + 40uy) "right rendered pixel is warm red"

                // Budget enforcement
                match IsoSurface3D.compute scale coolWarm { cfg with MaxTriangles = 10 } grid with
                | Error [ LayoutError.BudgetExceeded _ ] -> ()
                | other -> failtestf "Expected BudgetExceeded for MaxTriangles=10, got %A" other

                match IsoSurface3D.compute scale coolWarm { cfg with MaxVertices = 10 } grid with
                | Error [ LayoutError.BudgetExceeded _ ] -> ()
                | other -> failtestf "Expected BudgetExceeded for MaxVertices=10, got %A" other

                // Pre-canceled CancellationToken raises OperationCanceledException
                use cts = new CancellationTokenSource ()
                cts.Cancel ()

                Expect.throwsT<OperationCanceledException>
                    (fun () -> IsoSurface3D.computeWithCancellation cts.Token scale coolWarm cfg grid |> ignore)
                    "pre-canceled CancellationToken throws OperationCanceledException"

                // VolumeGrid validation checks
                Expect.isError
                    (VolumeGrid.uniformWithBudget 100 5 5 5 bounds (ReadOnlySpan<float32>(Array.zeroCreate 125)) ReadOnlySpan.Empty)
                    "voxel count exceeding maxVoxels returns Error"

                Expect.isError
                    (VolumeGrid.uniform 1 4 4 bounds (ReadOnlySpan<float32>(Array.zeroCreate 16)) ReadOnlySpan.Empty)
                    "nx < 2 returns Error"

                Expect.isError
                    (VolumeGrid.uniform 2 2 2 bounds (ReadOnlySpan<float32>(Array.zeroCreate 7)) ReadOnlySpan.Empty)
                    "mismatched values length returns Error"

                Expect.isError
                    (VolumeGrid.uniform 2 2 2 bounds (ReadOnlySpan<float32>(Array.zeroCreate 8)) (ReadOnlySpan<float32>(Array.zeroCreate 3)))
                    "mismatched secondaryScalars length returns Error"

                Expect.isError
                    (VolumeGrid.uniform 2 2 2 (Bounds3.create 0. 0. 0. 0. 1. 1.) (ReadOnlySpan<float32>(Array.zeroCreate 8)) ReadOnlySpan.Empty)
                    "zero-extent bounds returns Error"

                Expect.isError
                    (IsoSurface3D.compute scale coolWarm { cfg with Threshold = Single.NaN } grid)
                    "non-finite threshold returns Error"

                Expect.isError
                    (IsoSurface3D.compute scale coolWarm { cfg with MaxTriangles = -1 } grid)
                    "negative MaxTriangles returns Error"

                // Verify X-fastest voxel indexing ix + nx * (iy + ny * iz) matches VolumeGrid.fromFunction
                let anx, any, anz = 7, 9, 11
                let aBounds = Bounds3.create -1.0 0.0 1.0 2.0 4.0 6.0
                let rawVals = Array.zeroCreate<float32> (anx * any * anz)
                let rawSec = Array.zeroCreate<float32> (anx * any * anz)

                for iz = 0 to anz - 1 do
                    let z = 1.0 + 6.0 * float iz / float (anz - 1)
                    for iy = 0 to any - 1 do
                        let y = 0.0 + 4.0 * float iy / float (any - 1)
                        for ix = 0 to anx - 1 do
                            let x = -1.0 + 2.0 * float ix / float (anx - 1)
                            let idx = ix + anx * (iy + any * iz)
                            rawVals[idx] <- float32 (x - 0.5 * y + 0.25 * z)
                            rawSec[idx] <- float32 (x + y - z)

                let uGrid =
                    VolumeGrid.uniform anx any anz aBounds (ReadOnlySpan<float32> rawVals) (ReadOnlySpan<float32> rawSec)
                    |> unwrapOk "uniform asymmetric"

                let fGrid =
                    VolumeGrid.fromFunction
                        anx
                        any
                        anz
                        aBounds
                        (fun x y z -> float32 (x - 0.5 * y + 0.25 * z))
                        (ValueSome (fun x y z -> float32 (x + y - z)))
                    |> unwrapOk "fromFunction asymmetric"

                let aScale = Scale3D.cartesian (-1.0, 1.0) (0.0, 4.0) (1.0, 7.0)
                let aRamp = ColorRamp.coolWarm (-6.0, 6.0)
                let aCfg = IsoSurfaceConfig.create 0.0f
                let uMesh = IsoSurface3D.compute aScale aRamp aCfg uGrid |> unwrapOk "uMesh"
                let fMesh = IsoSurface3D.compute aScale aRamp aCfg fGrid |> unwrapOk "fMesh"
                Expect.equal uMesh.Vertices.Length fMesh.Vertices.Length "uniform and fromFunction produce identical vertex counts"
                Expect.equal uMesh.Indices fMesh.Indices "uniform and fromFunction produce identical indices"

                // Non-finite NaN voxel in field omits touching cells cleanly without NaN vertex positions or normals
                let nanGrid =
                    VolumeGrid.fromFunction
                        10
                        10
                        10
                        bounds
                        (fun x y z ->
                            let r2 = x * x + y * y + z * z
                            if r2 < 0.05 then Single.NaN else 1.0f - float32 r2)
                        ValueNone
                    |> unwrapOk "nanGrid"

                let nanMesh = IsoSurface3D.compute scale coolWarm cfg nanGrid |> unwrapOk "nanMesh"
                verifyOutwardWinding nanMesh "NaN-tolerant sphere"

            testCase "VolumeSlices3D.extract builds 3 orthogonal cross-section planes (1350 triangles) and renders cleanly on WgpuContext"
            <| fun () ->
                let bounds = Bounds3.create -1.0 -1.0 -1.0 2.0 2.0 2.0

                let grid =
                    VolumeGrid.fromFunction
                        16
                        16
                        16
                        bounds
                        (fun x y z -> float32 (x + 0.5 * y - 0.25 * z))
                        ValueNone
                    |> unwrapOk "sliceGrid"

                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let ramp = ColorRamp.coolWarm (-1.5, 1.5)
                let slices = { XIndex = Some 8; YIndex = Some 8; ZIndex = Some 8 }

                let sliceMesh =
                    VolumeSlices3D.extract scale ramp slices grid
                    |> unwrapOk "VolumeSlices3D.extract"

                Expect.equal (sliceMesh.Indices.Length / 3) 1350 "3 orthogonal 16x16 slices produce 3 * 15 * 15 * 2 = 1350 triangles"
                Expect.equal sliceMesh.Vertices.Length (3 * 16 * 16) "3 orthogonal 16x16 slices produce 768 vertices"
                verifyOutwardWinding sliceMesh "VolumeSlices3D 3 planes"

                let emptySlices =
                    VolumeSlices3D.extract scale ramp { XIndex = None; YIndex = None; ZIndex = None } grid
                    |> unwrapOk "empty slices"

                Expect.equal emptySlices.Vertices.Length 0 "no active slices produces 0 vertices"
                Expect.equal emptySlices.Indices.Length 0 "no active slices produces 0 indices"

                Expect.isError
                    (VolumeSlices3D.extract scale ramp { XIndex = Some 16; YIndex = None; ZIndex = None } grid)
                    "XIndex = nx returns Error"

                Expect.isError
                    (VolumeSlices3D.extract scale ramp { XIndex = None; YIndex = Some -1; ZIndex = None } grid)
                    "YIndex = -1 returns Error"

                Expect.isError
                    (VolumeSlices3D.extract scale ramp { XIndex = None; YIndex = None; ZIndex = Some 16 } grid)
                    "ZIndex = nz returns Error"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use gpuMesh = ctx.UploadMesh sliceMesh
                use batch = ctx.CreateBatch (gpuMesh, BlendMode.Opaque, true)
                batch.SetSingle (Instance3D.identity RgbaColor.white 0.5f 41u InstanceFlags.Unlit)
                let cam = Camera3D.orthographic Vector3.Zero 35.0f 30.0f 1.8f 6.0f

                let pass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| batch |]

                let metrics = pass |> WgpuContext.render ctx
                Expect.equal metrics.DrawCalls 1 "VolumeSlices3D renders in 1 draw call"
                let struct (cr, cg, cb, _) = pixelAt ctx 32 32
                Expect.isTrue (cr > 20uy || cg > 20uy || cb > 20uy) "VolumeSlices3D renders visible pixels at center"
                let hit = pass |> WgpuContext.pick 32 32 ctx
                Expect.equal (hit |> ValueOption.map _.PickId) (ValueSome 41u) "VolumeSlices3D picked cleanly"

                // Secondary colorScalar coloring on VolumeSlices3D
                let secSliceGrid =
                    VolumeGrid.fromFunction
                        8
                        8
                        8
                        bounds
                        (fun _ _ _ -> 0.0f)
                        (ValueSome (fun x _ _ -> float32 x))
                    |> unwrapOk "secSliceGrid"

                let secSlices =
                    VolumeSlices3D.extract scale (ColorRamp.coolWarm (-1.0, 1.0)) { XIndex = None; YIndex = None; ZIndex = Some 4 } secSliceGrid
                    |> unwrapOk "secSlices"

                Expect.equal (secSlices.Indices.Length / 3) (7 * 7 * 2) "single Z slice on 8^3 grid emits 98 triangles"
                Expect.isGreaterThan secSlices.Vertices[0].Color.Z (secSlices.Vertices[0].Color.X + 0.2f) "secondary scalar x=-1 colors cool blue on slice"
                Expect.isGreaterThan secSlices.Vertices[7].Color.X (secSlices.Vertices[7].Color.Z + 0.2f) "secondary scalar x=+1 colors warm red on slice"
        ]

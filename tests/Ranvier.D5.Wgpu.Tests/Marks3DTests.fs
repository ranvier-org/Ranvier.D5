module Ranvier.D5.Wgpu.Tests.Marks3DTests

open System
open System.Numerics
open Expecto
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.Wgpu

let private pixelAt (ctx: WgpuContext) (x: int) (y: int) =
    let idx = (y * ctx.Width + x) * 4
    let px = ctx.RgbaPixels
    struct (px[idx], px[idx + 1], px[idx + 2], px[idx + 3])

let private expectVec3Close (eps: float32) (actual: Vector3) (expected: Vector3) (msg: string) =
    let acc = { absolute = float eps; relative = 0.0 }
    Expect.floatClose acc (float actual.X) (float expected.X) ($"{msg} (X)")
    Expect.floatClose acc (float actual.Y) (float expected.Y) ($"{msg} (Y)")
    Expect.floatClose acc (float actual.Z) (float expected.Z) ($"{msg} (Z)")

let private expectFiniteVec3 (v: Vector3) (msg: string) =
    Expect.isTrue
        (Single.IsFinite v.X && Single.IsFinite v.Y && Single.IsFinite v.Z)
        $"{msg} is finite ({v})"

let private unwrapOk msg =
    function
    | Ok value -> value
    | Error err -> failtestf "%s failed: %A" msg err

let private verifyValidMesh (mesh: MeshData) (label: string) =
    Expect.isGreaterThan mesh.Vertices.Length 0 $"{label} has vertices"
    Expect.isGreaterThan mesh.Indices.Length 0 $"{label} has indices"
    Expect.equal (mesh.Indices.Length % 3) 0 $"{label} index count is divisible by 3"

    for i = 0 to mesh.Vertices.Length - 1 do
        let v = mesh.Vertices[i]
        expectFiniteVec3 v.Position $"{label} v[{i}].Position"
        expectFiniteVec3 v.Normal $"{label} v[{i}].Normal"

    for i = 0 to mesh.Indices.Length - 1 do
        Expect.isLessThan (int mesh.Indices[i]) mesh.Vertices.Length $"{label} idx[{i}] in bounds"

let private verifyOutwardWinding (mesh: MeshData) (label: string) =
    verifyValidMesh mesh label

    for t = 0 to (mesh.Indices.Length / 3) - 1 do
        let v0 = mesh.Vertices[int mesh.Indices[t * 3]]
        let v1 = mesh.Vertices[int mesh.Indices[t * 3 + 1]]
        let v2 = mesh.Vertices[int mesh.Indices[t * 3 + 2]]
        let cross = Vector3.Cross (v1.Position - v0.Position, v2.Position - v0.Position)

        if cross.LengthSquared () > 1e-12f then
            let triN = Vector3.Normalize cross
            let avgN = Vector3.Normalize (v0.Normal + v1.Normal + v2.Normal)
            Expect.isGreaterThan (Vector3.Dot (triN, avgN)) 0.5f $"{label} tri[{t}] CCW winding agrees with vertex normals"

[<Tests>]
let marks3DTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.Marks3D"
        [
            testCase "Scatter3D.pack renders and picks billboard discs, applies rings/hollow, omits unmappable items, and sorts translucent discs far-to-near"
            <| fun () ->
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)

                let baseDatum =
                    ScatterDatum3D.create (Point3.create 0.0 0.0 0.0) 14.0f (RgbaColor (0.2f, 0.9f, 0.3f, 1.0f))
                    |> ScatterDatum3D.withRings 0.65f 0.2f 2.5f
                    |> ScatterDatum3D.withHollow 0.5f

                let ringed = { baseDatum with PickId = 101u; Selected = true }
                let unmappable = ScatterDatum3D.create (Point3.create 0.0 0.0 0.0) Single.NaN RgbaColor.white

                let packedSingle =
                    Scatter3D.pack scale ValueNone (ReadOnlySpan<ScatterDatum3D> [| ringed; unmappable |])

                Expect.equal packedSingle.Length 1 "non-finite RadiusPx scatter item omitted cleanly"

                let logScale =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.ZUp
                        (AxisScale3D.log 10.0 (1.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))

                let packedLogOmit =
                    Scatter3D.pack
                        logScale
                        ValueNone
                        (ReadOnlySpan<ScatterDatum3D> [| ScatterDatum3D.create (Point3.create 0.0 5.0 5.0) 8.0f RgbaColor.white |])

                Expect.equal packedLogOmit.Length 0 "out-of-log-domain scatter item omitted cleanly"
                Expect.equal packedSingle.Span[0].PickId 101u "PickId preserved"
                Expect.equal (packedSingle.Span[0].Flags &&& InstanceFlags.Selected) InstanceFlags.Selected "Selected flag packed"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float packedSingle.Span[0].ArcParams.X) 0.65 "RingPrimary packed"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float packedSingle.Span[0].ArcParams.Y) 0.20 "RingSecondary packed"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float packedSingle.Span[0].ArcParams.Z) 2.50 "RingThicknessPx packed"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float packedSingle.Span[0].ArcParams.W) 0.50 "PortalHollow packed"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use discBatch = ctx.CreateDiscBatch ()
                discBatch.UpdateDiscs packedSingle.Span

                let camFront = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.5f 5.0f

                let pickPass =
                    ScenePass.create camFront
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withDiscs [| discBatch |]

                let metrics = pickPass |> WgpuContext.render ctx
                Expect.equal metrics.DrawCalls 1 "one draw call for disc batch"
                let hit = pickPass |> WgpuContext.pick 32 32 ctx
                Expect.equal (hit |> ValueOption.map _.PickId) (ValueSome 101u) "disc picked at center"

                // Two overlapping translucent discs (alpha = 0.5f):
                // Under ZUp, Y = +0.5 -> worldZ = -0.5 (farther from +Z camera)
                //             Y = -0.5 -> worldZ = +0.5 (nearer to +Z camera)
                let farRed =
                    { ScatterDatum3D.create (Point3.create 0.0 0.5 0.0) 18.0f (RgbaColor (1.0f, 0.0f, 0.0f, 0.5f)) with
                        PickId = 10u
                    }

                let nearBlue =
                    { ScatterDatum3D.create (Point3.create 0.0 -0.5 0.0) 18.0f (RgbaColor (0.0f, 0.0f, 1.0f, 0.5f)) with
                        PickId = 20u
                    }

                // Pass near first, far second -> pack with cameraPos sorts far-to-near!
                let inputPair = [| nearBlue; farRed |]
                let sortedFront = Scatter3D.pack scale (ValueSome camFront.Position) (ReadOnlySpan<ScatterDatum3D> inputPair)
                Expect.equal sortedFront.Length 2 "both translucent discs packed"
                Expect.equal sortedFront.Span[0].PickId 10u "far red sorted first for +Z camera"
                Expect.equal sortedFront.Span[1].PickId 20u "near blue sorted second for +Z camera"

                discBatch.UpdateDiscs sortedFront.Span
                pickPass |> WgpuContext.render ctx |> ignore
                let struct (fr, _, fb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan fb (fr + 30uy) "near blue blends on top of far red"
                let frontHit = pickPass |> WgpuContext.pick 32 32 ctx
                Expect.equal (frontHit |> ValueOption.map _.PickId) (ValueSome 20u) "pick returns nearer blue disc"

                // Flip camera to -Z (yaw = 180 deg) -> farRed is now nearer, nearBlue is farther!
                let camBack = Camera3D.orthographic Vector3.Zero 180.0f 0.0f 1.5f 5.0f
                let sortedBack = Scatter3D.pack scale (ValueSome camBack.Position) (ReadOnlySpan<ScatterDatum3D> inputPair)
                Expect.equal sortedBack.Span[0].PickId 20u "blue sorted first (far) for -Z camera"
                Expect.equal sortedBack.Span[1].PickId 10u "red sorted second (near) for -Z camera"

                discBatch.UpdateDiscs sortedBack.Span

                ScenePass.create camBack
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withDiscs [| discBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (br, _, bb, _) = pixelAt ctx 32 32
                Expect.isGreaterThan br (bb + 30uy) "near red blends on top of far blue from -Z camera"

                // Without cameraPos, order is preserved
                let unsorted = Scatter3D.pack scale ValueNone (ReadOnlySpan<ScatterDatum3D> inputPair)
                Expect.equal unsorted.Span[0].PickId 20u "ValueNone cameraPos preserves input order"

            testCase "Glyph3D.unitMesh, packGlyphs, packBars, and packArrows orient, anchor across z=0, and render cleanly on WgpuContext"
            <| fun () ->
                for shape in
                    [
                        GlyphShape3D.Sphere
                        GlyphShape3D.Box
                        GlyphShape3D.Cylinder
                        GlyphShape3D.Cone
                        GlyphShape3D.Torus
                    ] do
                    verifyValidMesh (Glyph3D.unitMesh shape) $"unitMesh %A{shape}"

                Expect.throwsT<ArgumentOutOfRangeException>
                    (fun () -> Glyph3D.unitArrowMesh 2 |> ignore)
                    "unitArrowMesh rejects segments = 2 (< 3)"

                Expect.throwsT<ArgumentOutOfRangeException>
                    (fun () -> Glyph3D.unitArrowMesh 0 |> ignore)
                    "unitArrowMesh rejects segments = 0"

                Expect.throwsT<ArgumentOutOfRangeException>
                    (fun () -> Glyph3D.unitArrowMesh -1 |> ignore)
                    "unitArrowMesh rejects negative segments"

                verifyOutwardWinding (Glyph3D.unitArrowMesh 3) "unitArrowMesh 3"

                let arrowMesh = Glyph3D.unitArrowMesh 12
                verifyOutwardWinding arrowMesh "unitArrowMesh 12"

                let mutable signedVol6 = 0.0

                for t = 0 to (arrowMesh.Indices.Length / 3) - 1 do
                    let v0 = arrowMesh.Vertices[int arrowMesh.Indices[t * 3]]
                    let v1 = arrowMesh.Vertices[int arrowMesh.Indices[t * 3 + 1]]
                    let v2 = arrowMesh.Vertices[int arrowMesh.Indices[t * 3 + 2]]
                    let triCross = Vector3.Cross (v1.Position - v0.Position, v2.Position - v0.Position)
                    signedVol6 <- signedVol6 + float (Vector3.Dot (v0.Position, Vector3.Cross (v1.Position, v2.Position)))

                    if triCross.LengthSquared () > 1e-12f then
                        let avgNormal = v0.Normal + v1.Normal + v2.Normal
                        Expect.isGreaterThan (Vector3.Dot (triCross, avgNormal)) 0.0f $"unitArrowMesh 12 tri[{t}] dot(triCross, avgNormal) > 0"

                Expect.isGreaterThan signedVol6 0.0 "unitArrowMesh 12 has positive signed volume"

                let mutable minY = Single.PositiveInfinity
                let mutable maxY = Single.NegativeInfinity

                for v in arrowMesh.Vertices do
                    minY <- min minY v.Position.Y
                    maxY <- max maxY v.Position.Y

                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float minY) 0.0 "unitArrowMesh anchored at y = 0.0"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float maxY) 1.0 "unitArrowMesh tip reaches y = 1.0"

                // packGlyphs verifies rotation, scale, shading, and omission
                let glyphScale = Scale3D.cartesian (-2.0, 2.0) (-2.0, 2.0) (-2.0, 2.0)
                let qRot = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, MathF.PI * 0.5f)

                let glyphItems =
                    [|
                        {
                            Position = Point3.create 1.0 0.0 0.0
                            Scale = Vector3 (0.25f, 0.5f, 0.25f)
                            Rotation = qRot
                            Color = RgbaColor (0.9f, 0.5f, 0.1f, 1.0f)
                            Shading = ShadingMode.Lit (0.35f, 0.1f)
                            PickId = 5u
                            Selected = true
                        }
                        {
                            Position = Point3.create 0.0 0.0 0.0
                            Scale = Vector3 (Single.NaN, 1.0f, 1.0f)
                            Rotation = Quaternion.Identity
                            Color = RgbaColor.white
                            Shading = ShadingMode.Unlit
                            PickId = 6u
                            Selected = false
                        }
                    |]

                let packedGlyphs = Glyph3D.packGlyphs glyphScale (ReadOnlySpan<GlyphDatum3D> glyphItems)
                Expect.equal packedGlyphs.Length 1 "non-finite Scale glyph omitted"
                Expect.equal (packedGlyphs.Span[0].Flags &&& InstanceFlags.Selected) InstanceFlags.Selected "glyph Selected flag set"

                let gMat =
                    let g = packedGlyphs.Span[0]
                    Matrix4x4 (
                        g.ModelCol0.X, g.ModelCol0.Y, g.ModelCol0.Z, g.ModelCol0.W,
                        g.ModelCol1.X, g.ModelCol1.Y, g.ModelCol1.Z, g.ModelCol1.W,
                        g.ModelCol2.X, g.ModelCol2.Y, g.ModelCol2.Z, g.ModelCol2.W,
                        g.ModelCol3.X, g.ModelCol3.Y, g.ModelCol3.Z, g.ModelCol3.W
                    )

                let glyphOrigin = Scale3D.map glyphScale (Point3.create 1.0 0.0 0.0)
                expectVec3Close 1e-5f (Vector3.Transform (Vector3.UnitX, gMat) - glyphOrigin) (Vector3 (0.0f, 0.25f, 0.0f)) "90deg Z rotation maps local +X to +Y"
                expectVec3Close 1e-5f (Vector3.Transform (Vector3.UnitY, gMat) - glyphOrigin) (Vector3 (-0.5f, 0.0f, 0.0f)) "90deg Z rotation maps local +Y to -X"

                // packBars anchors positive (0..5) and negative (0..-3) columns on opposite sides of z=0
                let barScale = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (-5.0, 5.0)

                let bars =
                    [|
                        {
                            X = 2.5
                            Y = 5.0
                            Z0 = 0.0
                            Z1 = 5.0
                            WidthX = 1.5
                            WidthY = 1.5
                            Color = RgbaColor (0.0f, 1.0f, 0.0f, 1.0f)
                            Shading = ShadingMode.Unlit
                            PickId = 11u
                            Selected = false
                        }
                        {
                            X = 7.5
                            Y = 5.0
                            Z0 = 0.0
                            Z1 = -3.0
                            WidthX = 1.5
                            WidthY = 1.5
                            Color = RgbaColor (1.0f, 0.0f, 0.0f, 1.0f)
                            Shading = ShadingMode.Unlit
                            PickId = 12u
                            Selected = false
                        }
                    |]

                let packedBars = Glyph3D.packBars barScale (ReadOnlySpan<BarDatum3D> bars)
                Expect.equal packedBars.Length 2 "both bars packed"

                let posBar = packedBars.Span[0]
                let negBar = packedBars.Span[1]
                // World Y bounds of unitBox [-0.5, +0.5] transformed by ModelCol1.Y (scaleY) and ModelCol3.Y (centerY)
                let posCenterY = posBar.ModelCol3.Y
                let posHalfH = abs posBar.ModelCol1.Y * 0.5f
                let negCenterY = negBar.ModelCol3.Y
                let negHalfH = abs negBar.ModelCol1.Y * 0.5f

                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (posCenterY - posHalfH)) 0.0 "positive bar bottom anchored at world y=0 (z=0 plane)"
                Expect.isGreaterThan (posCenterY + posHalfH) 0.1f "positive bar extends above z=0 plane"
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (negCenterY + negHalfH)) 0.0 "negative bar top anchored at world y=0 (z=0 plane)"
                Expect.isLessThan (negCenterY - negHalfH) -0.1f "negative bar extends below z=0 plane"

                // Also verify YUp bar orientation
                let barScaleYUp =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.YUp
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (0.0, 10.0))
                        (AxisScale3D.linear (-5.0, 5.0))

                let packedBarsYUp = Glyph3D.packBars barScaleYUp (ReadOnlySpan<BarDatum3D> bars)
                Expect.equal packedBarsYUp.Length 2 "YUp bars packed"
                Expect.isGreaterThan packedBarsYUp.Span[0].ModelCol3.Z 0.0f "YUp positive bar has center.Z > 0"
                Expect.isLessThan packedBarsYUp.Span[1].ModelCol3.Z 0.0f "YUp negative bar has center.Z < 0"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use boxMesh = ctx.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Box)
                use barBatch = ctx.CreateBatch boxMesh
                barBatch.UpdateInstances packedBars.Span

                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.2f 6.0f

                let barPass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| barBatch |]

                barPass |> WgpuContext.render ctx |> ignore

                // Left bar (x = 2.5 -> worldX = -0.5 -> screen x ~ 19) is positive (y in top half < 32)
                let struct (_, posTopG, _, _) = pixelAt ctx 19 20
                let struct (_, posBotG, _, _) = pixelAt ctx 19 42
                Expect.isGreaterThan posTopG 180uy "positive bar renders in top half (+Z above z=0)"
                Expect.equal posBotG 0uy "positive bar does not cross below z=0"

                // Right bar (x = 7.5 -> worldX = +0.5 -> screen x ~ 45) is negative (y in bottom half > 32)
                let struct (negTopR, _, _, _) = pixelAt ctx 45 20
                let struct (negBotR, _, _, _) = pixelAt ctx 45 40
                Expect.equal negTopR 0uy "negative bar does not cross above z=0"
                Expect.isGreaterThan negBotR 180uy "negative bar renders in bottom half (-Z below z=0)"

                // packArrows from (0, 0, 0) toward (+1, 0, 0) renders on +X half and NOT -X half
                let arrowScale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)

                let arrows =
                    [|
                        {
                            Origin = Point3.create 0.0 0.0 0.0
                            Vector = Point3.create 1.0 0.0 0.0
                            ShaftRadius = 0.08f
                            LengthScale = 1.0f
                            Color = RgbaColor (1.0f, 1.0f, 0.0f, 1.0f)
                            Shading = ShadingMode.Unlit
                            PickId = 31u
                            Selected = false
                        }
                        // Zero-length and non-finite arrows skipped cleanly
                        {
                            Origin = Point3.create 0.0 0.0 0.0
                            Vector = Point3.create 0.0 0.0 0.0
                            ShaftRadius = 0.05f
                            LengthScale = 1.0f
                            Color = RgbaColor.white
                            Shading = ShadingMode.Unlit
                            PickId = 32u
                            Selected = false
                        }
                        {
                            Origin = Point3.create 0.0 0.0 0.0
                            Vector = Point3.create 1.0 0.0 0.0
                            ShaftRadius = Single.NaN
                            LengthScale = 1.0f
                            Color = RgbaColor.white
                            Shading = ShadingMode.Unlit
                            PickId = 33u
                            Selected = false
                        }
                        {
                            Origin = Point3.create 1e308 0.0 0.0
                            Vector = Point3.create 1e308 0.0 0.0
                            ShaftRadius = 0.05f
                            LengthScale = 1.0f
                            Color = RgbaColor.white
                            Shading = ShadingMode.Unlit
                            PickId = 34u
                            Selected = false
                        }
                    |]

                let packedArrows = Glyph3D.packArrows arrowScale (ReadOnlySpan<ArrowDatum3D> arrows)
                Expect.equal packedArrows.Length 1 "zero-length, NaN, and overflow arrows skipped cleanly"

                use arrowGpuMesh = ctx.UploadMesh arrowMesh
                use arrowBatch = ctx.CreateBatch arrowGpuMesh
                arrowBatch.UpdateInstances packedArrows.Span

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| arrowBatch |]
                |> WgpuContext.render ctx
                |> ignore

                // +X half (x = 42, y = 32) has bright yellow arrow pixels; -X half (x = 18, y = 32) is black
                let struct (arR, arG, arB, _) = pixelAt ctx 42 32
                Expect.isGreaterThan arR 180uy "arrow renders on +X half (R)"
                Expect.isGreaterThan arG 180uy "arrow renders on +X half (G)"
                Expect.isLessThan arB 40uy "arrow renders yellow on +X half (B)"

                for pxX = 4 to 28 do
                    let struct (lxR, lxG, lxB, _) = pixelAt ctx pxX 32
                    Expect.equal lxR 0uy $"no arrow pixels on -X half at x={pxX} (R)"
                    Expect.equal lxG 0uy $"no arrow pixels on -X half at x={pxX} (G)"
                    Expect.equal lxB 0uy $"no arrow pixels on -X half at x={pxX} (B)"

            testCase "Line3D.segments, polyline gap splitting, tube, and double-sided ribbon build valid geometry and render cleanly on WgpuContext"
            <| fun () ->
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let cyan = RgbaColor (0.0f, 1.0f, 1.0f, 1.0f)

                let segVerts =
                    Line3D.segments
                        scale
                        (ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)>
                            [|
                                struct (Point3.create -0.5 0.0 0.0, Point3.create 0.5 0.0 0.0, cyan)
                                struct (Point3.create 0.0 0.0 0.0, Point3.create 1.0 0.0 0.0, RgbaColor (Single.NaN, 0.0f, 0.0f, 1.0f))
                            |])

                Expect.equal segVerts.Length 2 "segments skips invalid color pair and emits 2 endpoints"

                let polyPts: System.Collections.Generic.IReadOnlyList<struct (Point3 * RgbaColor) voption> =
                    [|
                        ValueSome struct (Point3.create -0.9 0.0 0.0, cyan)
                        ValueSome struct (Point3.create -0.4 0.0 0.0, cyan)
                        ValueNone
                        ValueSome struct (Point3.create 0.4 0.0 0.0, cyan)
                        ValueSome struct (Point3.create 0.9 0.0 0.0, cyan)
                    |]

                let polyVerts = Line3D.polyline scale polyPts
                Expect.equal polyVerts.Length 4 "polyline splits across ValueNone into 2 disconnected segments (4 vertices)"
                let leftEnd = Scale3D.map scale (Point3.create -0.4 0.0 0.0)
                let rightStart = Scale3D.map scale (Point3.create 0.4 0.0 0.0)
                expectVec3Close 1e-5f polyVerts.Span[1].Position leftEnd "first segment ends at -0.4"
                expectVec3Close 1e-5f polyVerts.Span[2].Position rightStart "second segment starts at +0.4 without bridging gap"

                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use lineBatch = ctx.CreateLineBatch ()
                lineBatch.UpdateVertices polyVerts.Span

                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.2f 5.0f

                ScenePass.create cam
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withLines [| lineBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (_, centerGapG, _, _) = pixelAt ctx 32 32
                Expect.equal centerGapG 0uy "center gap between polyline runs remains unbridged backdrop"
                let struct (_, leftRunG, _, _) = pixelAt ctx 14 32
                let struct (_, rightRunG, _, _) = pixelAt ctx 50 32
                Expect.isGreaterThan leftRunG 120uy "left polyline run renders"
                Expect.isGreaterThan rightRunG 120uy "right polyline run renders"

                // Tube3D
                let tubePts =
                    [|
                        struct (Point3.create -0.8 0.0 -0.3, 0.12f, RgbaColor (1.0f, 0.2f, 0.2f, 1.0f))
                        struct (Point3.create 0.0 0.2 0.2, 0.14f, RgbaColor (0.2f, 1.0f, 0.2f, 1.0f))
                        struct (Point3.create 0.0 0.2 0.2, 0.14f, RgbaColor (0.2f, 1.0f, 0.2f, 1.0f))
                        struct (Point3.create 0.8 0.0 0.3, 0.12f, RgbaColor (0.2f, 0.2f, 1.0f, 1.0f))
                    |]

                let tubeMesh =
                    Line3D.tube scale 8 (ReadOnlySpan<struct (Point3 * float32 * RgbaColor)> tubePts)
                    |> unwrapOk "Line3D.tube"

                verifyOutwardWinding tubeMesh "Line3D.tube"

                Expect.isError
                    (Line3D.tube scale 2 (ReadOnlySpan<struct (Point3 * float32 * RgbaColor)> tubePts))
                    "tube rejects sides < 3"

                Expect.isError
                    (Line3D.tube scale 8 (ReadOnlySpan<struct (Point3 * float32 * RgbaColor)> [| tubePts[0] |]))
                    "tube rejects < 2 points"

                Expect.isError
                    (Line3D.tube scale 8 (ReadOnlySpan<struct (Point3 * float32 * RgbaColor)> [| tubePts[0]; tubePts[0] |]))
                    "tube rejects collapsed zero-length path"

                Expect.isError
                    (Line3D.tube scale 8 (ReadOnlySpan<struct (Point3 * float32 * RgbaColor)> [| struct (Point3.create 0.0 0.0 0.0, -0.1f, cyan); struct (Point3.create 0.5 0.0 0.0, 0.1f, cyan) |]))
                    "tube rejects negative radius"

                use tubeGpu = ctx.UploadMesh tubeMesh
                use tubeBatch = ctx.CreateBatch tubeGpu
                tubeBatch.SetSingle (Instance3D.identity RgbaColor.white 0.4f 55u InstanceFlags.None)

                let tubePass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| tubeBatch |]

                let tubeMetrics = tubePass |> WgpuContext.render ctx
                Expect.equal tubeMetrics.DrawCalls 1 "tube renders in 1 draw call"
                let struct (tr, tg, tb, _) = pixelAt ctx 32 28
                Expect.isTrue (tr > 20uy || tg > 20uy || tb > 20uy) "tube renders visible pixels"
                let tubeHit = tubePass |> WgpuContext.pick 32 28 ctx
                Expect.equal (tubeHit |> ValueOption.map _.PickId) (ValueSome 55u) "tube picked by WgpuContext"

                // Ribbon3D (double-sided ruled strip rendered with single-sided CullMode.Back batch from BOTH front and back!)
                let ribbonSamples =
                    [|
                        struct (Point3.create -0.7 0.0 0.0, Point3.create -0.7 0.0 0.0, RgbaColor (1.0f, 0.4f, 0.1f, 1.0f))
                        struct (Point3.create 0.0 0.0 -0.5, Point3.create 0.0 0.0 0.5, RgbaColor (0.9f, 0.7f, 0.2f, 1.0f))
                        struct (Point3.create 0.7 0.0 -0.4, Point3.create 0.7 0.0 0.4, RgbaColor (0.2f, 0.8f, 1.0f, 1.0f))
                    |]

                let ribbonMesh =
                    Line3D.ribbon scale (ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)> ribbonSamples)
                    |> unwrapOk "Line3D.ribbon"

                verifyOutwardWinding ribbonMesh "Line3D.ribbon"

                Expect.isError
                    (Line3D.ribbon scale (ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)> [| ribbonSamples[0] |]))
                    "ribbon rejects < 2 samples"

                use ribbonGpu = ctx.UploadMesh ribbonMesh
                use ribbonBatch = ctx.CreateBatch (ribbonGpu, BlendMode.Opaque, false)
                ribbonBatch.SetSingle (Instance3D.identity RgbaColor.white 0.4f 77u InstanceFlags.Unlit)

                let ribbonFrontPass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| ribbonBatch |]

                ribbonFrontPass |> WgpuContext.render ctx |> ignore
                let struct (rfR, rfG, rfB, _) = pixelAt ctx 32 32
                Expect.isTrue (rfR > 50uy || rfG > 50uy || rfB > 50uy) "ribbon front side renders with CullMode.Back"

                let camOpposite = Camera3D.orthographic Vector3.Zero 180.0f 0.0f 1.2f 5.0f

                ScenePass.create camOpposite
                |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                |> ScenePass.withBatches [| ribbonBatch |]
                |> WgpuContext.render ctx
                |> ignore

                let struct (rbR, rbG, rbB, _) = pixelAt ctx 32 32
                Expect.isTrue (rbR > 50uy || rbG > 50uy || rbB > 50uy) "ribbon back side also renders with CullMode.Back"

            testCase "Line3D.tube preserves finite unit orthogonal frames across a finely sampled 3,000-segment 225-degree curve"
            <| fun () ->
                let scale = Scale3D.create BoxAspect.Cube UpAxis.ZUp (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0))
                let segments = 3000
                let sides = 8
                let tubeRadius = 0.04f
                let sweep = 1.25 * Math.PI

                let pts =
                    Array.init (segments + 1) (fun i ->
                        let theta = sweep * float i / float segments
                        struct (Point3.create (cos theta) (-sin theta) 0.0, tubeRadius, RgbaColor.white))

                let mesh =
                    Line3D.tube scale sides (ReadOnlySpan<struct (Point3 * float32 * RgbaColor)> pts)
                    |> unwrapOk "fine 3000-segment tube"

                verifyOutwardWinding mesh "fine 3000-segment tube"

                for i = 0 to segments do
                    let theta = sweep * float i / float segments
                    let center = Scale3D.map scale (Point3.create (cos theta) (-sin theta) 0.0)
                    let tangent = Vector3.Normalize (Vector3 (-float32 (sin theta), 0.0f, float32 (cos theta)))

                    for s = 0 to sides - 1 do
                        let v = mesh.Vertices[i * sides + s]
                        Expect.floatClose { absolute = 1e-4; relative = 0.0 } (float (v.Normal.Length ())) 1.0 $"fine tube ring[{i}].v[{s}] unit normal"
                        Expect.floatClose { absolute = 1e-3; relative = 0.0 } (float (Vector3.Dot (v.Normal, tangent))) 0.0 $"fine tube ring[{i}].v[{s}] normal orthogonal to tangent"
                        Expect.floatClose { absolute = 1e-4; relative = 0.0 } (float ((v.Position - center).Length ())) (float tubeRadius) $"fine tube ring[{i}].v[{s}] preserves circular radius"

            testCase "MarksKernel.tryPackBar omits float32 wzSize overflow bars and packBars/packArrows/segments match finite invariants"
            <| fun () ->
                let overflowScale =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.ZUp
                        (AxisScale3D.linear (-1.0, 1.0))
                        (AxisScale3D.linear (-1.0, 1.0))
                        (AxisScale3D.linear (-1.0, 1.0))

                // w0.Y = -2e38f (finite), w1.Y = +2e38f (finite), but wzSize = abs (w1.Y - w0.Y) = +Infinityf -> omitted
                let overflowBars =
                    [|
                        {
                            X = 0.0
                            Y = 0.0
                            Z0 = -2.0e38
                            Z1 = 2.0e38
                            WidthX = 0.5
                            WidthY = 0.5
                            Color = RgbaColor.white
                            Shading = ShadingMode.Unlit
                            PickId = 90u
                            Selected = false
                        }
                        {
                            X = 0.0
                            Y = 0.0
                            Z0 = 0.0
                            Z1 = 0.5
                            WidthX = 0.5
                            WidthY = 0.5
                            Color = RgbaColor.white
                            Shading = ShadingMode.Unlit
                            PickId = 91u
                            Selected = false
                        }
                    |]

                let packedOverflow = Glyph3D.packBars overflowScale (ReadOnlySpan<BarDatum3D> overflowBars)
                Expect.equal packedOverflow.Length 1 "bar with overflowing wzSize omitted while finite bar is packed"
                Expect.equal packedOverflow.Span[0].PickId 91u "surviving finite bar retains PickId"

                // Verify YUp wzSize overflow guard as well
                let overflowScaleYUp =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.YUp
                        (AxisScale3D.linear (-1.0, 1.0))
                        (AxisScale3D.linear (-1.0, 1.0))
                        (AxisScale3D.linear (-1.0, 1.0))

                let packedOverflowYUp = Glyph3D.packBars overflowScaleYUp (ReadOnlySpan<BarDatum3D> [| overflowBars[0] |])
                Expect.equal packedOverflowYUp.Length 0 "YUp bar with overflowing wzSize omitted cleanly"

                // Warmed tryPackBar and tryPackSegment allocate 0 managed bytes (no reference tuples or FSharpOption<double>)
                let barOut = Array.zeroCreate<Instance3D> 1
                let segOut = Array.zeroCreate<LineVertex3D> 2
                let finiteBar = overflowBars[1]
                let finitePair = struct (Point3.create -0.5 0.0 0.0, Point3.create 0.5 0.0 0.0, RgbaColor.white)

                let logSub1e162Scale =
                    Scale3D.create
                        BoxAspect.Cube
                        UpAxis.ZUp
                        (AxisScale3D.log 10.0 (1e-210, 1e-190))
                        (AxisScale3D.log 10.0 (-1e-190, -1e-210))
                        (AxisScale3D.piecewise [ 0.0; 1.0; 2.0 ] [ 0.0; 0.5; 1.0 ])

                let logBar =
                    { finiteBar with
                        X = 1e-200
                        Y = -1e-200
                        Z0 = 0.0
                        Z1 = 1.5
                        WidthX = 1e-201
                        WidthY = 1e-201
                    }

                let logPair = struct (Point3.create 1e-205 -1e-205 0.25, Point3.create 1e-195 -1e-195 1.75, RgbaColor.white)

                for _ = 1 to 100 do
                    MarksKernel.tryPackBar overflowScale 1u finiteBar barOut 0 |> ignore
                    MarksKernel.tryPackSegment overflowScale finitePair segOut 0 |> ignore
                    MarksKernel.tryPackBar logSub1e162Scale 1u logBar barOut 0 |> ignore
                    MarksKernel.tryPackSegment logSub1e162Scale logPair segOut 0 |> ignore

                let allocBefore = GC.GetAllocatedBytesForCurrentThread ()
                let mutable okBar = true
                let mutable okSeg = true

                for _ = 1 to 1000 do
                    okBar <- okBar && MarksKernel.tryPackBar overflowScale 1u finiteBar barOut 0
                    okSeg <- okSeg && MarksKernel.tryPackSegment overflowScale finitePair segOut 0
                    okBar <- okBar && MarksKernel.tryPackBar logSub1e162Scale 1u logBar barOut 0
                    okSeg <- okSeg && MarksKernel.tryPackSegment logSub1e162Scale logPair segOut 0

                let allocAfter = GC.GetAllocatedBytesForCurrentThread ()
                Expect.isTrue okBar "warmed tryPackBar succeeds"
                Expect.isTrue okSeg "warmed tryPackSegment succeeds"
                Expect.equal (allocAfter - allocBefore) 0L "1000 warmed tryPackBar and tryPackSegment calls allocate 0 bytes"
        ]

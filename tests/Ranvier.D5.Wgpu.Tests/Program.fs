module Ranvier.D5.Wgpu.Tests.Program

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

[<Tests>]
let contracts3DTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.Contracts3D"
        [
            testCase "Point3.toVector3f and Point3.ofVector3f convert finite 3D coordinates and reject non-finite vectors"
            <| fun () ->
                let p = Point3.create 1.5 -2.25 3.75
                let v = Point3.toVector3f p
                Expect.equal v (Vector3 (1.5f, -2.25f, 3.75f)) "Point3 converts to Vector3"
                let roundTrip = Point3.ofVector3f v
                Expect.equal roundTrip p "Vector3 converts back to Point3"
                Expect.throwsT<ArgumentException> (fun () -> Point3.ofVector3f (Vector3 (Single.NaN, 0.0f, 0.0f)) |> ignore) "rejects NaN X"
                Expect.throwsT<ArgumentException> (fun () -> Point3.ofVector3f (Vector3 (0.0f, Single.PositiveInfinity, 0.0f)) |> ignore) "rejects Inf Y"

            testCase "Bounds3.boundingRadius computes overflow-protected half-diagonal radius"
            <| fun () ->
                let zero = Bounds3.create 1. 2. 3. 0. 0. 0.
                Expect.floatClose Accuracy.veryHigh (Bounds3.boundingRadius zero) 0. "degenerate point bounds has radius 0"

                let cube = Bounds3.create -1. -1. -1. 2. 2. 2.
                Expect.floatClose Accuracy.veryHigh (Bounds3.boundingRadius cube) (sqrt 3.) "unit half-extent cube has radius sqrt(3)"

                let pythagorean = Bounds3.create 0. 0. 0. 2. 3. 6.
                Expect.floatClose Accuracy.veryHigh (Bounds3.boundingRadius pythagorean) 3.5 "2x3x6 cuboid has half-diagonal 3.5"

                let huge = Bounds3.create 0. 0. 0. 1e200 1e200 1e200
                let hugeRadius = Bounds3.boundingRadius huge
                Expect.isTrue (Double.IsFinite hugeRadius) "large finite bounds avoids squared overflow"
                Expect.floatClose Accuracy.high hugeRadius (0.5 * sqrt 3. * 1e200) "large finite bounds preserves accurate half-diagonal"

            testCase "WgpuContext 0.3.0 initializes offscreen and renders with Camera3D.orthographic"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use boxMesh = ctx.UploadMesh (Primitives.unitBox ())
                use boxes = ctx.CreateBatch boxMesh

                let red = RgbaColor (1.0f, 0.0f, 0.0f, 1.0f)
                let green = RgbaColor (0.0f, 1.0f, 0.0f, 1.0f)

                boxes.UpdateInstances
                    [|
                        Instance3D.trs (Vector3 (-0.9f, 0.0f, 1.0f)) 0.0f (Vector3 (0.5f, 0.5f, 0.5f)) red 0.5f 7u InstanceFlags.Unlit
                        Instance3D.trs (Vector3 (0.9f, 0.0f, -1.0f)) 0.0f (Vector3 (0.5f, 0.5f, 0.5f)) green 0.5f 8u InstanceFlags.Unlit
                    |]

                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 2.0f 6.0f

                let pass =
                    ScenePass.create cam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withBatches [| boxes |]

                let metrics = pass |> WgpuContext.render ctx
                Expect.equal metrics.DrawCalls 1 "one draw call for the box batch"
                Expect.equal metrics.InstanceCount 2 "two box instances rendered"

                let leftHit = pass |> WgpuContext.pick 18 32 ctx
                let rightHit = pass |> WgpuContext.pick 46 32 ctx
                Expect.equal (leftHit |> ValueOption.map _.PickId) (ValueSome 7u) "left box picked under orthographic camera"
                Expect.equal (rightHit |> ValueOption.map _.PickId) (ValueSome 8u) "right box picked under orthographic camera"

                let struct (lr, lg, lb, _) = pixelAt ctx 18 32
                Expect.isGreaterThan lr 200uy "left box renders red"
                Expect.isLessThan lg 30uy "left box has low green"
                Expect.isLessThan lb 30uy "left box has low blue"

                let struct (rr, rg, rb, _) = pixelAt ctx 46 32
                Expect.isLessThan rr 30uy "right box has low red"
                Expect.isGreaterThan rg 200uy "right box renders green"
                Expect.isLessThan rb 30uy "right box has low blue"
        ]

[<EntryPoint>]
let main args =
    let allTests =
        testList
            "Ranvier.D5.Wgpu"
            [
                contracts3DTests
                ScaleAxisTests.scaleAxisTests
            ]

    runTestsWithCLIArgs [] args allTests

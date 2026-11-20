module Ranvier.D5.Wgpu.Tests.Reactive3DTests

open System
open System.Diagnostics
open System.Numerics
open System.Threading
open Expecto
open Ranvier
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

let private unwrapOk msg =
    function
    | Ok value -> value
    | Error err -> failtestf "%s failed: %A" msg err

type private DomainRow =
    {
        Id: string
        Value: float option
    }

[<Tests>]
let reactive3DTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.Reactive3D"
        [
            testCase "KeyedCollection 100-item bindBars mutates 1 key with LastUpdatedSlots = 1 and updates only that bar on WgpuContext"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 80, initialHeight = 80)
                let scale = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)
                let blue = RgbaColor (0.0f, 0.2f, 1.0f, 1.0f)
                let red = RgbaColor (1.0f, 0.1f, 0.0f, 1.0f)

                let makeBar idx z1 color =
                    let gx = float (idx % 10) + 0.5
                    let gy = float (idx / 10) + 0.5

                    {
                        X = gx
                        Y = gy
                        Z0 = 0.0
                        Z1 = z1
                        WidthX = 0.75
                        WidthY = 0.75
                        Color = color
                        Shading = ShadingMode.Unlit
                        PickId = 0u
                        Selected = false
                    }

                use graph = new Graph ()

                graph.Run (fun () ->
                    let scaleSig = Api.createSignal scale
                    let collection = Api.createKeyedCollection (fun struct (k: string, _: BarDatum3D) -> k)

                    for i = 0 to 99 do
                        collection.AddOrUpdate (struct ($"k{i}", makeBar i 5.0 blue))

                    let projected = collection.Rows |> Projection.map (fun struct (_, d) -> d)
                    let sink = Reactive3D.bindBars ctx 1uy (fun () -> scaleSig.Value) projected

                    Expect.equal sink.LayerId 1uy "LayerId matches"
                    Expect.equal sink.ActiveCount 100 "100 bars active"
                    Expect.equal sink.Batch.Count 100 "GPU Batch.Count = 100"
                    Expect.equal sink.LastUpdatedSlots 100 "initial reset uploads 100 slots"

                    // Top-down orthographic camera looking onto XY plane (ZUp)
                    let cam = Camera3D.orthographic Vector3.Zero 0.0f 85.0f 1.15f 6.0f

                    let pass =
                        ScenePass.create cam
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withBatches [| sink.Batch |]

                    pass |> WgpuContext.render ctx |> ignore

                    // k0 is at (0.5, 0.5) -> bottom-left; k99 is at (9.5, 9.5) -> top-right; k55 is near center-top-right
                    let world0 = Scale3D.map scale (Point3.create 0.5 0.5 5.0)
                    let world55 = Scale3D.map scale (Point3.create 5.5 5.5 5.0)
                    let scr0 = Camera3D.projectToScreen 80 80 cam world0 |> ValueOption.get
                    let scr55 = Camera3D.projectToScreen 80 80 cam world55 |> ValueOption.get
                    let px0Before = pixelAt ctx (int (MathF.Round scr0.X)) (int (MathF.Round scr0.Y))
                    let px55Before = pixelAt ctx (int (MathF.Round scr55.X)) (int (MathF.Round scr55.Y))

                    let struct (r0b, _, b0b, _) = px0Before
                    let struct (r55b, _, b55b, _) = px55Before
                    Expect.isGreaterThan b0b (r0b + 100uy) "k0 initially renders blue"
                    Expect.isGreaterThan b55b (r55b + 100uy) "k55 initially renders blue"

                    // Single-row mutation
                    collection.AddOrUpdate (struct ("k55", makeBar 55 8.0 red))
                    Expect.equal sink.LastUpdatedSlots 1 "single-row AddOrUpdate writes exactly 1 GPU slot via UpdateRange"
                    Expect.equal sink.ActiveCount 100 "ActiveCount remains 100"

                    pass |> WgpuContext.render ctx |> ignore
                    let world55After = Scale3D.map scale (Point3.create 5.5 5.5 8.0)
                    let scr55After = Camera3D.projectToScreen 80 80 cam world55After |> ValueOption.get
                    let px0After = pixelAt ctx (int (MathF.Round scr0.X)) (int (MathF.Round scr0.Y))
                    let px55After = pixelAt ctx (int (MathF.Round scr55After.X)) (int (MathF.Round scr55After.Y))

                    Expect.equal px0After px0Before "unmutated bar k0 pixel stays unchanged"
                    let struct (r55a, _, b55a, _) = px55After
                    Expect.isGreaterThan r55a (b55a + 100uy) "mutated bar k55 now renders red"

                    // Scale3D signal change with 0 row changes re-packs all active keys cleanly
                    scaleSig.Value <- Scale3D.cartesian (-1.0, 11.0) (-1.0, 11.0) (0.0, 10.0)
                    Expect.equal sink.LastUpdatedSlots 100 "Scale3D change re-packs all 100 active slots"

                    // Scope cleanup disposes GPU batch
                    let owner =
                        Api.createRoot (fun root ->
                            let segColl = Api.createKeyedCollection (fun struct (k: string, _: struct (Point3 * Point3 * RgbaColor)) -> k)
                            segColl.AddOrUpdate (struct ("s1", struct (Point3.create 0.0 0.0 0.0, Point3.create 1.0 1.0 1.0, red)))
                            let segRows = segColl.Rows |> Projection.map (fun struct (_, s) -> s)
                            let segSink = Reactive3D.bindSegments ctx (fun () -> scale) segRows
                            Expect.equal segSink.ActiveCount 1 "segment sink activeCount"
                            Expect.equal segSink.Batch.SegmentCount 1 "LineBatch SegmentCount = 1"
                            segColl.AddOrUpdate (struct ("s1", struct (Point3.create 0.0 0.0 0.0, Point3.create 2.0 2.0 2.0, blue)))
                            Expect.equal segSink.LastUpdatedSlots 1 "single segment edit writes 1 slot"
                            root)

                    owner.Dispose ()
                )

            testCase "Api.createSelector hover integration re-evaluates and uploads only 2 slots out of 100"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                let scale = Scale3D.cartesian (0.0, 10.0) (0.0, 10.0) (0.0, 10.0)
                let baseColor = RgbaColor (0.2f, 0.7f, 0.4f, 1.0f)

                use graph = new Graph ()

                graph.Run (fun () ->
                    let collection = Api.createKeyedCollection (fun struct (k: string, _: BarDatum3D) -> k)

                    for i = 0 to 99 do
                        let d: BarDatum3D =
                            {
                                X = float (i % 10) + 0.5
                                Y = float (i / 10) + 0.5
                                Z0 = 0.0
                                Z1 = 5.0
                                WidthX = 0.6
                                WidthY = 0.6
                                Color = baseColor
                                Shading = ShadingMode.Unlit
                                PickId = 0u
                                Selected = false
                            }

                        collection.AddOrUpdate (struct ($"k{i}", d))

                    let hoveredKey = Api.createSignal "k1"
                    let hoveredSelector = Api.createSelector (fun () -> hoveredKey.Value)
                    let mutable evalCount = 0

                    let selectedRows =
                        collection.Rows
                        |> Projection.mapWith (fun k getRow ->
                            fun () ->
                                evalCount <- evalCount + 1
                                let struct (_, d) = getRow ()
                                { d with Selected = hoveredSelector.Get k })

                    let sink = Reactive3D.bindBars ctx 2uy (fun () -> scale) selectedRows
                    Expect.equal sink.ActiveCount 100 "100 bars bound"
                    Expect.equal evalCount 100 "initial pass evaluates 100 rows"

                    evalCount <- 0
                    hoveredKey.Value <- "k2"

                    Expect.equal evalCount 2 "selector change re-evaluates only previous (k1) and next (k2) rows"
                    Expect.equal sink.LastUpdatedSlots 2 "selector transition uploads only 2 GPU slots"
                )

            testCase "Unordered KeyedInstanceSink and KeyedDiscSink retire removed PickIds via tail-swap and resolve Probe3D marks"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 96, initialHeight = 96)
                let scale = Scale3D.cartesian (-2.0, 2.0) (-2.0, 2.0) (-1.0, 1.0)
                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.5f 5.0f

                use graph = new Graph ()

                graph.Run (fun () ->
                    Expect.throwsT<ArgumentOutOfRangeException>
                        (fun () ->
                            let empty = Api.createKeyedCollection (fun struct (k: string, _: BarDatum3D) -> k)
                            let rows = empty.Rows |> Projection.map (fun struct (_, d) -> d)
                            Reactive3D.bindBars ctx 0uy (fun () -> scale) rows |> ignore)
                        "layerId = 0uy is rejected"

                    let collection = Api.createKeyedCollection (fun struct (k: string, _: BarDatum3D) -> k)

                    let barAt x color : BarDatum3D =
                        {
                            X = x
                            Y = 0.0
                            Z0 = -0.5
                            Z1 = 0.5
                            WidthX = 0.45
                            WidthY = 0.45
                            Color = color
                            Shading = ShadingMode.Unlit
                            PickId = 0u
                            Selected = false
                        }

                    collection.AddOrUpdate (struct ("k1", barAt -1.2 (RgbaColor (1.0f, 0.2f, 0.2f, 1.0f))))
                    collection.AddOrUpdate (struct ("k2", barAt 0.0 (RgbaColor (0.2f, 1.0f, 0.2f, 1.0f))))
                    collection.AddOrUpdate (struct ("k3", barAt 1.2 (RgbaColor (0.2f, 0.2f, 1.0f, 1.0f))))

                    let rows = collection.Rows |> Projection.map (fun struct (_, d) -> d)
                    let sink = Reactive3D.bindBars ctx 1uy (fun () -> scale) rows

                    Expect.equal sink.ActiveCount 3 "3 bars initially"
                    Expect.equal (sink.TryResolvePick ((1u <<< 24) ||| 1u)) (ValueSome "k1") "slot 0 -> k1"
                    Expect.equal (sink.TryResolvePick ((1u <<< 24) ||| 2u)) (ValueSome "k2") "slot 1 -> k2"
                    Expect.equal (sink.TryResolvePick ((1u <<< 24) ||| 3u)) (ValueSome "k3") "slot 2 -> k3"

                    let pass =
                        ScenePass.create cam
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withBatches [| sink.Batch |]

                    let scrK1 = Camera3D.projectToScreen 96 96 cam (Scale3D.map scale (Point3.create -1.2 0.0 0.0)) |> ValueOption.get
                    let scrK3 = Camera3D.projectToScreen 96 96 cam (Scale3D.map scale (Point3.create 1.2 0.0 0.0)) |> ValueOption.get
                    let pxK1 = int (MathF.Round scrK1.X), int (MathF.Round scrK1.Y)
                    let pxK3 = int (MathF.Round scrK3.X), int (MathF.Round scrK3.Y)

                    let hitK1Before = pass |> WgpuContext.pick (fst pxK1) (snd pxK1) ctx
                    Expect.isTrue hitK1Before.IsSome "k1 picked before removal"
                    let resolvedBefore = hitK1Before |> ValueOption.bind (Probe3D.resolveKeyed scale sink)
                    Expect.equal (resolvedBefore |> ValueOption.map _.Key) (ValueSome "k1") "Probe3D resolves k1 before removal"

                    // Remove k1 -> tail k3 swaps into slot 0, slot 2 retired
                    Expect.isTrue (collection.Remove "k1") "Remove k1 returns true"
                    Expect.equal sink.ActiveCount 2 "ActiveCount shrinks to 2"
                    Expect.equal sink.Batch.Count 2 "Batch.Count shrinks to 2"
                    Expect.equal sink.LastUpdatedSlots 1 "tail swap writes 1 GPU slot"

                    Expect.equal (sink.TryResolvePick ((1u <<< 24) ||| 3u)) ValueNone "old slot 2 (PickId 3) is retired immediately"
                    Expect.equal (sink.TryResolvePick ((1u <<< 24) ||| 1u)) (ValueSome "k3") "slot 0 (PickId 1) now resolves to swapped tail key k3"
                    Expect.equal (sink.TryResolvePick ((1u <<< 24) ||| 2u)) (ValueSome "k2") "slot 1 still resolves to k2"
                    Expect.equal (sink.TryResolvePick ((2u <<< 24) ||| 1u)) ValueNone "mismatched layerId returns ValueNone"
                    Expect.equal (sink.TryResolvePick 0u) ValueNone "0u PickId returns ValueNone"

                    let hitK1After = pass |> WgpuContext.pick (fst pxK1) (snd pxK1) ctx
                    Expect.equal hitK1After ValueNone "GPU pick at k1 old position returns ValueNone"

                    let hitK3After = pass |> WgpuContext.pick (fst pxK3) (snd pxK3) ctx
                    Expect.equal (hitK3After |> ValueOption.map _.PickId) (ValueSome ((1u <<< 24) ||| 1u)) "GPU pick at k3 returns updated PickId for slot 0"
                    let resolvedK3 = hitK3After |> ValueOption.bind (Probe3D.resolveKeyed scale sink)
                    Expect.equal (resolvedK3 |> ValueOption.map _.Key) (ValueSome "k3") "Probe3D resolves k3 after swap"
                    Expect.equal (resolvedK3 |> ValueOption.map _.LayerId) (ValueSome 1uy) "LayerId preserved"

                    // Ordered KeyedDiscSink preserves SortView order on positional move & resolves Probe3D.resolveDisc
                    let discColl = Api.createKeyedCollection (fun struct (k: string, _: ScatterDatum3D) -> k)
                    discColl.AddOrUpdate (struct ("d1", ScatterDatum3D.create (Point3.create -1.0 0.0 0.0) 14.0f (RgbaColor (1.0f, 0.0f, 0.0f, 0.5f))))
                    discColl.AddOrUpdate (struct ("d2", ScatterDatum3D.create (Point3.create 1.0 0.0 0.0) 14.0f (RgbaColor (0.0f, 0.0f, 1.0f, 0.5f))))
                    let sortedDiscs =
                        discColl.Rows
                        |> Projection.map (fun struct (_, d) -> d)
                        |> Projection.sortByDescending (fun d -> d.Position.X)

                    let discSink = Reactive3D.bindDiscs ctx 3uy true (fun () -> scale) sortedDiscs
                    Expect.equal discSink.ActiveCount 2 "2 ordered discs active"
                    Expect.equal (discSink.TryResolvePick ((3u <<< 24) ||| 1u)) (ValueSome "d2") "d2 has higher X so sorts first"
                    Expect.equal (discSink.TryResolvePick ((3u <<< 24) ||| 2u)) (ValueSome "d1") "d1 sorts second"

                    // Move d1 past d2 in sort order -> positional move updates slots
                    discColl.AddOrUpdate (struct ("d1", ScatterDatum3D.create (Point3.create 2.0 0.0 0.0) 14.0f (RgbaColor (1.0f, 0.0f, 0.0f, 0.5f))))
                    Expect.equal (discSink.TryResolvePick ((3u <<< 24) ||| 1u)) (ValueSome "d1") "d1 moved to slot 0 after sort flip"
                    Expect.equal (discSink.TryResolvePick ((3u <<< 24) ||| 2u)) (ValueSome "d2") "d2 moved to slot 1 after sort flip"

                    let discPass =
                        ScenePass.create cam
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withDiscs [| discSink.Batch |]

                    let scrD2 = Camera3D.projectToScreen 96 96 cam (Scale3D.map scale (Point3.create 1.0 0.0 0.0)) |> ValueOption.get
                    let discHit = discPass |> WgpuContext.pick (int (MathF.Round scrD2.X)) (int (MathF.Round scrD2.Y)) ctx
                    let resolvedDisc = discHit |> ValueOption.bind (Probe3D.resolveDisc scale discSink)
                    Expect.equal (resolvedDisc |> ValueOption.map _.Key) (ValueSome "d2") "Probe3D.resolveDisc resolves d2"
                )

            testCase "Reactive3D.niceContinuousScale ignores interior edits and widens when extremum expands"
            <| fun () ->
                use graph = new Graph ()

                graph.Run (fun () ->
                    let collection = Api.createKeyedCollection (fun r -> r.Id)
                    let emptyExt = Reactive3D.domainExtent (fun r -> r.Value) collection.Rows
                    let emptyScale = Reactive3D.niceContinuousScale 5 (-5.0, 5.0) (0.0, 1.0) (fun r -> r.Value) collection.Rows
                    Expect.equal emptyExt.Value None "empty collection domainExtent is None"
                    Expect.equal (Scale.invert emptyScale.Value 0.0, Scale.invert emptyScale.Value 1.0) (-5.0, 5.0) "uses fallbackDomain when empty"

                    collection.AddOrUpdate { Id = "a"; Value = Some 0.0 }
                    collection.AddOrUpdate { Id = "b"; Value = Some 50.0 }
                    collection.AddOrUpdate { Id = "c"; Value = Some 100.0 }
                    collection.AddOrUpdate { Id = "none"; Value = None }
                    collection.AddOrUpdate { Id = "nan"; Value = Some Double.NaN }

                    let mutable runs = 0
                    let niceScale = Reactive3D.niceContinuousScale 5 (0.0, 1.0) (0.0, 1.0) (fun r -> r.Value) collection.Rows
                    let mutable lastMax = Double.NaN

                    Api.createEffect (fun () ->
                        let s = niceScale.Value
                        lastMax <- Scale.invert s 1.0
                        runs <- runs + 1)

                    Expect.equal runs 1 "initial effect runs once"
                    Expect.floatClose { absolute = 1e-9; relative = 0.0 } lastMax 100.0 "initial nice upper domain = 100.0"

                    // Interior edits inside [0.0, 100.0] do NOT wake scale observer
                    collection.AddOrUpdate { Id = "b"; Value = Some 75.0 }
                    collection.AddOrUpdate { Id = "interior"; Value = Some 12.0 }
                    Expect.equal runs 1 "interior edits inside [0.0, 100.0] keep runs = 1"

                    // Inserting 145.0 widens the nice domain cleanly
                    collection.AddOrUpdate { Id = "outer"; Value = Some 145.0 }
                    Expect.equal runs 2 "inserting 145.0 wakes scale observer (runs = 2)"
                    Expect.isGreaterThanOrEqual lastMax 145.0 "widened nice upper bound covers 145.0"
                )

            testCase "Editable<PlotCamera3D>, Probe3D.invertHit/hoverSpikes, bindSurface, and asyncIsoSurface work end-to-end"
            <| fun () ->
                // 1. PlotCamera3D defaults, orbit, pan locks, zoom, focusOn, toCamera3D
                let defCam = PlotCamera3D.defaultValue
                Expect.equal defCam.Mode OrbitMode3D.Turntable "default Mode = Turntable"
                Expect.floatClose { absolute = 1e-6; relative = 0.0 } (float defCam.AzimuthRad) (float (1.275f * MathF.PI)) "default AzimuthRad = 1.275 * pi"
                Expect.floatClose { absolute = 1e-6; relative = 0.0 } (float defCam.ElevationRad) (float (MathF.PI / 8.0f)) "default ElevationRad = pi / 8"
                Expect.equal defCam.Zoom 1.0f "default Zoom = 1.0f"
                Expect.equal defCam.Perspectiveness 0.0f "default Perspectiveness = 0.0f"

                let clampedElev = PlotCamera3D.orbitBy 0.0f 5000.0f defCam
                Expect.isLessThanOrEqual (abs clampedElev.ElevationRad) (85.0f * MathF.PI / 180.0f + 1e-5f) "Turntable elevation clamps away from gimbal poles"

                let arcCam = { defCam with Mode = OrbitMode3D.Arcball } |> PlotCamera3D.orbitBy 40.0f -30.0f
                Expect.floatClose { absolute = 1e-5; relative = 0.0 } (float (arcCam.Arcball.Length ())) 1.0 "Arcball quaternion stays normalized"

                let scale = Scale3D.cartesian (0.0, 2.0) (0.0, 2.0) (0.0, 2.0)

                // Screen-direction parity between Turntable and Arcball for horizontal and vertical drags
                let turnZero = { defCam with Mode = OrbitMode3D.Turntable; AzimuthRad = 0.0f; ElevationRad = 0.0f }
                let arcZero = { defCam with Mode = OrbitMode3D.Arcball; Arcball = Quaternion.Identity }
                let frontPt = Vector3 (0.0f, 0.0f, 0.5f)
                let scrTurnHoriz = Camera3D.projectToScreen 100 100 (PlotCamera3D.toCamera3D scale (PlotCamera3D.orbitBy 40.0f 0.0f turnZero)) frontPt |> ValueOption.get
                let scrArcHoriz = Camera3D.projectToScreen 100 100 (PlotCamera3D.toCamera3D scale (PlotCamera3D.orbitBy 40.0f 0.0f arcZero)) frontPt |> ValueOption.get
                Expect.isGreaterThan scrTurnHoriz.X 50.0f "Turntable +dx shifts front point right on screen"
                Expect.isGreaterThan scrArcHoriz.X 50.0f "Arcball +dx shifts front point right on screen"
                Expect.floatClose { absolute = 1e-3; relative = 0.0 } (float scrArcHoriz.X) (float scrTurnHoriz.X) "Arcball horizontal drag matches Turntable screen X"

                let scrTurnVert = Camera3D.projectToScreen 100 100 (PlotCamera3D.toCamera3D scale (PlotCamera3D.orbitBy 0.0f 40.0f turnZero)) frontPt |> ValueOption.get
                let scrArcVert = Camera3D.projectToScreen 100 100 (PlotCamera3D.toCamera3D scale (PlotCamera3D.orbitBy 0.0f 40.0f arcZero)) frontPt |> ValueOption.get
                Expect.isGreaterThan scrTurnVert.Y 50.0f "Turntable +dy shifts front point down on screen"
                Expect.isGreaterThan scrArcVert.Y 50.0f "Arcball +dy shifts front point down on screen"
                Expect.floatClose { absolute = 1e-3; relative = 0.0 } (float scrArcVert.Y) (float scrTurnVert.Y) "Arcball vertical drag matches Turntable screen Y"
                let panX = PlotCamera3D.panBy (Scale3D.fitRadius scale) AxisLock3D.LockX 0.25f 0.25f defCam
                Expect.equal panX.PanOffset.Y 0.0f "LockX keeps Y unchanged"
                Expect.equal panX.PanOffset.Z 0.0f "LockX keeps Z unchanged"

                let panY = PlotCamera3D.panBy (Scale3D.fitRadius scale) AxisLock3D.LockY 0.25f 0.25f defCam
                Expect.equal panY.PanOffset.X 0.0f "LockY keeps X unchanged"
                Expect.equal panY.PanOffset.Z 0.0f "LockY keeps Z unchanged"

                let panZ = PlotCamera3D.panBy (Scale3D.fitRadius scale) AxisLock3D.LockZ 0.25f 0.25f defCam
                Expect.equal panZ.PanOffset.X 0.0f "LockZ keeps X unchanged"
                Expect.equal panZ.PanOffset.Y 0.0f "LockZ keeps Y unchanged"

                let focused = defCam |> PlotCamera3D.focusOn (Vector3 (0.3f, -0.2f, 0.1f)) |> PlotCamera3D.zoomBy 2.0f
                let gpuCam = PlotCamera3D.toCamera3D scale focused
                expectVec3Close 1e-5f gpuCam.Target (Vector3 (0.3f, -0.2f, 0.1f)) "focusOn sets Camera3D.Target"
                Expect.isLessThan gpuCam.FovYDeg 0.0f "Perspectiveness = 0.0f emits orthographic Camera3D"

                let perspGpuCam = PlotCamera3D.toCamera3D scale { defCam with Perspectiveness = 1.0f }
                Expect.isGreaterThan perspGpuCam.FovYDeg 0.0f "Perspectiveness = 1.0f emits perspective Camera3D"

                // 2. Editable<PlotCamera3D> + Editable<SectionPlane> + bindSurface + Probe3D.invertHit + Probe3D.hoverSpikes
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)

                use graph = new Graph ()

                graph.Run (fun () ->
                    let camSeed = Api.createSignal defCam
                    let editCam = Reactive3D.editableCamera (fun () -> camSeed.Value)
                    let secSeed = Api.createSignal SectionPlane.disabled
                    let editSec = Reactive3D.editableSection (fun () -> secSeed.Value)

                    editCam.Value <- PlotCamera3D.orbitBy 25.0f 10.0f editCam.Value
                    Expect.notEqual editCam.Value.AzimuthRad defCam.AzimuthRad "local orbitBy overrides seed"
                    editCam.Reset ()
                    Expect.equal editCam.Value defCam "Reset restores upstream seed"

                    editSec.Value <- SectionPlane.create Vector3.UnitY 0.1f
                    Expect.isTrue editSec.Value.Enabled "local section edit overrides disabled seed"
                    editSec.Reset ()
                    Expect.isFalse editSec.Value.Enabled "Reset restores disabled section seed"

                    let ramp = ColorRamp.viridis (0.0, 2.0)
                    let specSig =
                        Api.createSignal
                            {
                                Scale = scale
                                Color = ramp
                                Shading = ShadingMode.Unlit
                                Opacity = 1.0f
                                Wireframe = ValueNone
                                PickId = 77u
                            }

                    let grid1 =
                        SurfaceGrid.fromFunction 9 9 (0.0, 2.0) (0.0, 2.0) (fun x y -> 0.5 * x + 0.5 * y) ValueNone
                        |> unwrapOk "surface grid1"

                    let gridSig = Api.createSignal grid1
                    let surfBatch = Reactive3D.bindSurface ctx (fun () -> specSig.Value) (fun () -> gridSig.Value)

                    // Look from +Y (top-down onto ZUp surface) so (32, 32) hits center (1.0, 1.0, 1.0)
                    let topCam = Camera3D.orthographic Vector3.Zero 0.0f 85.0f 1.2f 6.0f
                    let surfPass =
                        ScenePass.create topCam
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withBatches [| surfBatch.Batch |]

                    surfPass |> WgpuContext.render ctx |> ignore
                    let surfHit = surfPass |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal surfHit.PickId 77u "surface picked at center"
                    let dataPt = Probe3D.invertHit scale surfHit
                    Expect.floatClose { absolute = 0.05; relative = 0.0 } dataPt.X 1.0 "invertHit recovers dataX within 0.05"
                    Expect.floatClose { absolute = 0.05; relative = 0.0 } dataPt.Y 1.0 "invertHit recovers dataY within 0.05"
                    Expect.floatClose { absolute = 0.05; relative = 0.0 } dataPt.Z 1.0 "invertHit recovers dataZ within 0.05"

                    let axisFrame = Axis3D.build (Axis3D.defaultSpec scale) 64 64 topCam
                    let spikes = Probe3D.hoverSpikes scale axisFrame RgbaColor.white surfHit.WorldPosition
                    Expect.equal spikes.Length 6 "hoverSpikes emits 6 LineVertex3D endpoints (3 spikes)"

                    for i = 0 to spikes.Length - 1 do
                        Expect.equal spikes.Span[i].Pad 1.0f $"hoverSpikes vertex {i} sets Pad = 1.0f (ignoreSectionPlane)"

                    let struct (bx, fy, bz) = Axis3D.backWallPlanes axisFrame
                    expectVec3Close 1e-5f spikes.Span[0].Position surfHit.WorldPosition "spike 0 starts at worldPos"
                    expectVec3Close 1e-5f spikes.Span[1].Position (Vector3 (bx, surfHit.WorldPosition.Y, surfHit.WorldPosition.Z)) "spike 0 drops to back X wall"
                    expectVec3Close 1e-5f spikes.Span[3].Position (Vector3 (surfHit.WorldPosition.X, fy, surfHit.WorldPosition.Z)) "spike 1 drops to floor Y wall"
                    expectVec3Close 1e-5f spikes.Span[5].Position (Vector3 (surfHit.WorldPosition.X, surfHit.WorldPosition.Y, bz)) "spike 2 drops to back Z wall"

                    // Hover spikes stay visible on GPU even when an active SectionPlane clips the entire cuboid
                    use spikeBatch = ctx.CreateLineBatch ()
                    spikeBatch.UpdateVertices spikes.Span
                    let clipAllPlane = SectionPlane.create Vector3.UnitY -10.0f

                    ScenePass.create topCam
                    |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                    |> ScenePass.withSection clipAllPlane
                    |> ScenePass.withLines [| spikeBatch |]
                    |> WgpuContext.render ctx
                    |> ignore

                    let spikePixelsVisible =
                        ctx.RgbaPixels
                        |> Array.chunkBySize 4
                        |> Array.exists (fun px -> px[0] > 100uy && px[1] > 100uy && px[2] > 100uy)

                    Expect.isTrue spikePixelsVisible "section-exempt hoverSpikes render visibly under clipping SectionPlane"

                    // Updating gridSig updates SurfaceBatch in place
                    let grid2 =
                        SurfaceGrid.fromFunction 9 9 (0.0, 2.0) (0.0, 2.0) (fun _ _ -> 1.8) ValueNone
                        |> unwrapOk "surface grid2"

                    gridSig.Value <- grid2
                    surfPass |> WgpuContext.render ctx |> ignore
                    let surfHit2 = surfPass |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    let dataPt2 = Probe3D.invertHit scale surfHit2
                    Expect.floatClose { absolute = 0.05; relative = 0.0 } dataPt2.Z 1.8 "updated SurfaceGrid moves picked Z to 1.8"

                    // 3. Reactive3D.asyncIsoSurface cancels superseded run and settles on latest threshold
                    let volBounds = Bounds3.create -1.5 -1.5 -1.5 3.0 3.0 3.0
                    let vol =
                        VolumeGrid.fromFunction
                            32
                            32
                            32
                            volBounds
                            (fun x y z -> 1.0f - float32 (x * x + y * y + z * z))
                            ValueNone
                        |> unwrapOk "async vol"

                    let isoScale = Scale3D.cartesian (-1.5, 1.5) (-1.5, 1.5) (-1.5, 1.5)
                    let isoRamp = ColorRamp.viridis (-1.0, 1.0)
                    let thresholdSig = Api.createSignal 0.8f
                    let configMemo = Api.createMemo (fun _ -> IsoSurfaceConfig.create thresholdSig.Value)

                    let isoMemo =
                        Reactive3D.asyncIsoSurface
                            (fun () -> isoScale)
                            (fun () -> isoRamp)
                            (fun () -> configMemo.Value)
                            (fun () -> vol)

                    // Trigger first flight (0.8f) then supersede immediately with 0.2f
                    ignore isoMemo.TryValue
                    thresholdSig.Value <- 0.2f
                    ignore isoMemo.TryValue

                    let sw = Stopwatch.StartNew ()

                    while (match isoMemo.TryValue with Ready _ -> false | _ -> true)
                          && sw.ElapsedMilliseconds < 10000L do
                        graph.Pump () |> ignore
                        Thread.Sleep 5

                    let finalMesh =
                        match isoMemo.TryValue with
                        | Ready r -> r |> unwrapOk "asyncIsoSurface settled"
                        | other -> failtestf "Expected asyncIsoSurface to settle, got %A" other

                    let expectedMesh =
                        IsoSurface3D.compute isoScale isoRamp (IsoSurfaceConfig.create 0.2f) vol
                        |> unwrapOk "expected 0.2f mesh"

                    Expect.equal finalMesh.Vertices.Length expectedMesh.Vertices.Length "settled mesh matches latest threshold 0.2f vertex count"
                    Expect.equal finalMesh.Indices.Length expectedMesh.Indices.Length "settled mesh matches latest threshold 0.2f index count"
                )

            testCase "bindInstances, IPickResolver, contiguous coalescing, null/None keys, and domain shrinking work cleanly"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                let scale = Scale3D.cartesian (-2.0, 2.0) (-2.0, 2.0) (-2.0, 2.0)

                use graph = new Graph ()

                graph.Run (fun () ->
                    use sphereMesh = ctx.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Sphere)
                    let coll = Api.createKeyedCollection (fun struct (k: int option, _: GlyphDatum3D) -> k)

                    let makeGlyph x : GlyphDatum3D =
                        {
                            Position = Point3.create x 0.0 0.0
                            Scale = Vector3 0.2f
                            Rotation = Quaternion.Identity
                            Color = RgbaColor (0.9f, 0.5f, 0.2f, 1.0f)
                            Shading = ShadingMode.Unlit
                            PickId = 0u
                            Selected = false
                        }

                    coll.AddOrUpdate (struct (None, makeGlyph -1.5))
                    coll.AddOrUpdate (struct (Some 1, makeGlyph -0.5))
                    coll.AddOrUpdate (struct (Some 2, makeGlyph 0.5))
                    coll.AddOrUpdate (struct (Some 3, makeGlyph 1.5))

                    let glyphRows = coll.Rows |> Projection.map (fun struct (_, g) -> g)
                    let sink = Reactive3D.bindInstances ctx 7uy sphereMesh BlendMode.Opaque false false (fun () -> scale) glyphRows
                    let resolver = sink :> IPickResolver

                    Expect.equal resolver.LayerId 7uy "IPickResolver exposes LayerId"
                    Expect.equal resolver.ActiveCount 4 "4 glyphs bound including None key"
                    Expect.isTrue (resolver.ContainsPick ((7u <<< 24) ||| 1u)) "slot 0 active"
                    Expect.equal (sink.TryResolvePick ((7u <<< 24) ||| 1u)) (ValueSome None) "None key resolves cleanly"
                    Expect.equal (resolver.TryResolveBoxedKey ((7u <<< 24) ||| 2u)) (ValueSome (box (Some 1))) "boxed key resolves Some 1"

                    // Coalesced batch update of contiguous keys 1 and 2 + discontiguous None -> 3 slots total
                    Api.batch (fun () ->
                        coll.AddOrUpdate (struct (None, makeGlyph -1.2))
                        coll.AddOrUpdate (struct (Some 1, makeGlyph -0.3))
                        coll.AddOrUpdate (struct (Some 2, makeGlyph 0.3)))

                    Expect.equal sink.LastUpdatedSlots 3 "coalesced batch update writes 3 dirty slots"

                    // Simultaneous removal of slot 0 (None) and slot 3 (Some 3) -> remaining Some 1 & Some 2 stay valid
                    Api.batch (fun () ->
                        coll.Remove None |> ignore
                        coll.Remove (Some 3) |> ignore)

                    Expect.equal sink.ActiveCount 2 "2 active after removing None and Some 3"
                    Expect.isFalse (resolver.ContainsPick ((7u <<< 24) ||| 3u)) "retired slot 2 returns false"
                    Expect.equal (sink.TryResolvePick ((7u <<< 24) ||| 1u)) (ValueSome (Some 2)) "remaining slot 0 valid"
                    Expect.equal (sink.TryResolvePick ((7u <<< 24) ||| 2u)) (ValueSome (Some 1)) "remaining slot 1 valid"

                    // niceContinuousScale shrinks cleanly when extremum is removed
                    let numColl = Api.createKeyedCollection (fun r -> r.Id)
                    numColl.AddOrUpdate { Id = "a"; Value = Some 0.0 }
                    numColl.AddOrUpdate { Id = "b"; Value = Some 50.0 }
                    numColl.AddOrUpdate { Id = "c"; Value = Some 180.0 }
                    let s = Reactive3D.niceContinuousScale 5 (0.0, 1.0) (0.0, 1.0) (fun r -> r.Value) numColl.Rows
                    Expect.isGreaterThanOrEqual (Scale.invert s.Value 1.0) 180.0 "initial nice upper covers 180"
                    Expect.isTrue (numColl.Remove "c") "remove max holder c"
                    Expect.floatClose { absolute = 1e-9; relative = 0.0 } (Scale.invert s.Value 1.0) 50.0 "nice upper shrinks back to 50.0 when max removed"

                    // Zoomed PlotCamera3D retains full cuboid depth without clipping box corners
                    let r = Scale3D.fitRadius scale
                    let zoomedOrtho = PlotCamera3D.defaultValue |> PlotCamera3D.zoomBy 20.0f |> PlotCamera3D.toCamera3D scale
                    let zoomedPersp = { PlotCamera3D.defaultValue with Mode = OrbitMode3D.Arcball; Perspectiveness = 1.0f } |> PlotCamera3D.zoomBy 20.0f |> PlotCamera3D.toCamera3D scale
                    let orthoDist = Vector3.Distance (zoomedOrtho.Position, zoomedOrtho.Target)
                    let perspDist = Vector3.Distance (zoomedPersp.Position, zoomedPersp.Target)
                    Expect.isLessThan zoomedOrtho.NearPlane (orthoDist - r) "zoomed ortho NearPlane stays ahead of front box bound"
                    Expect.isGreaterThan zoomedOrtho.FarPlane (orthoDist + r) "zoomed ortho FarPlane stays behind back box bound"
                    Expect.isGreaterThan zoomedPersp.NearPlane 0.0f "zoomed Arcball persp NearPlane stays strictly positive"
                    Expect.isLessThan zoomedPersp.NearPlane (perspDist * 0.15f) "zoomed Arcball persp NearPlane stays tight near camera eye"
                    Expect.isGreaterThan zoomedPersp.FarPlane (perspDist + r * 2.0f) "zoomed Arcball persp FarPlane covers full cuboid behind target"
                )

            testCase "PlotCamera3D.withMode preserves camera world Position across Turntable -> Arcball -> Turntable after orbiting"
            <| fun () ->
                let scale = Scale3D.cartesian (-2.0, 2.0) (-2.0, 2.0) (-1.0, 1.0)

                let orbitedTurn =
                    PlotCamera3D.defaultValue
                    |> PlotCamera3D.focusOn (Vector3 (0.25f, -0.15f, 0.1f))
                    |> PlotCamera3D.orbitBy 45.0f -20.0f

                for persp in [ 0.0f; 1.0f ] do
                    let cTurn0 = { orbitedTurn with Perspectiveness = persp }
                    let wTurn0 = PlotCamera3D.toCamera3D scale cTurn0

                    let cArc = PlotCamera3D.withMode OrbitMode3D.Arcball cTurn0
                    Expect.equal cArc.Mode OrbitMode3D.Arcball "switched to Arcball"
                    let wArc = PlotCamera3D.toCamera3D scale cArc
                    expectVec3Close 1e-4f wArc.Position wTurn0.Position $"Persp={persp}: Turntable -> Arcball preserves camera world Position"
                    expectVec3Close 1e-4f wArc.Target wTurn0.Target $"Persp={persp}: Turntable -> Arcball preserves Target"

                    let cArc2 = PlotCamera3D.orbitBy 30.0f 15.0f cArc
                    let wArc2 = PlotCamera3D.toCamera3D scale cArc2

                    let cTurn2 = PlotCamera3D.withMode OrbitMode3D.Turntable cArc2
                    Expect.equal cTurn2.Mode OrbitMode3D.Turntable "switched back to Turntable"
                    let wTurn2 = PlotCamera3D.toCamera3D scale cTurn2
                    expectVec3Close 1e-4f wTurn2.Position wArc2.Position $"Persp={persp}: Arcball -> Turntable preserves camera world Position"
                    expectVec3Close 1e-4f wTurn2.Target wArc2.Target $"Persp={persp}: Arcball -> Turntable preserves Target"

                Expect.equal (PlotCamera3D.withMode OrbitMode3D.Turntable orbitedTurn) orbitedTurn "withMode same mode is identity"

                let nanAz = { orbitedTurn with AzimuthRad = Single.NaN }
                let nanGpu = PlotCamera3D.toCamera3D scale nanAz
                Expect.isTrue (Single.IsFinite nanGpu.Position.X && Single.IsFinite nanGpu.Position.Y && Single.IsFinite nanGpu.Position.Z) "non-finite AzimuthRad sanitizes cleanly"

            testCase "KeyedSlotSync handles empty -> add -> empty -> add, preserveOrder RemoveAt, and temporary-NaN slot recovery"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 96, initialHeight = 96)
                let scale = Scale3D.cartesian (-2.0, 2.0) (-2.0, 2.0) (-1.0, 1.0)
                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.5f 5.0f

                let makeGlyph x color : GlyphDatum3D =
                    {
                        Position = Point3.create x 0.0 0.0
                        Scale = Vector3 (0.35f, 0.35f, 0.35f)
                        Rotation = Quaternion.Identity
                        Color = color
                        Shading = ShadingMode.Unlit
                        PickId = 0u
                        Selected = false
                    }

                let pickKeyAt (sink: KeyedInstanceSink<string>) (pass: ScenePass) (xData: float) =
                    let scr = Camera3D.projectToScreen 96 96 cam (Scale3D.map scale (Point3.create xData 0.0 0.0)) |> ValueOption.get
                    let hit = pass |> WgpuContext.pick (int (MathF.Round scr.X)) (int (MathF.Round scr.Y)) ctx
                    hit |> ValueOption.bind (Probe3D.resolveKeyed scale sink) |> ValueOption.map _.Key

                use graph = new Graph ()

                graph.Run (fun () ->
                    use boxMesh = ctx.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Box)

                    // 1. Empty initial collection -> add -> remove all down to 0 -> add back (both preserveOrder=false and preserveOrder=true)
                    for preserveOrder in [ false; true ] do
                        let emptyColl = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
                        let emptyRows = emptyColl.Rows |> Projection.map (fun struct (_, g) -> g)
                        let layerId = if preserveOrder then 2uy else 1uy
                        let sink = Reactive3D.bindInstances ctx layerId boxMesh BlendMode.Opaque false preserveOrder (fun () -> scale) emptyRows

                        Expect.equal sink.ActiveCount 0 $"preserveOrder={preserveOrder}: starts empty with ActiveCount = 0"
                        Expect.equal sink.Batch.Count 0 $"preserveOrder={preserveOrder}: starts empty with Batch.Count = 0"

                        emptyColl.AddOrUpdate (struct ("a", makeGlyph -1.0 RgbaColor.white))
                        emptyColl.AddOrUpdate (struct ("b", makeGlyph 1.0 RgbaColor.white))
                        Expect.equal sink.ActiveCount 2 $"preserveOrder={preserveOrder}: ActiveCount = 2 after adding"
                        Expect.equal sink.Batch.Count 2 $"preserveOrder={preserveOrder}: Batch.Count = 2 after adding"

                        Expect.isTrue (emptyColl.Remove "a") "remove a"
                        Expect.isTrue (emptyColl.Remove "b") "remove b"
                        Expect.equal sink.ActiveCount 0 $"preserveOrder={preserveOrder}: ActiveCount returns to 0"
                        Expect.equal sink.Batch.Count 0 $"preserveOrder={preserveOrder}: Batch.Count returns to 0"
                        Expect.equal (sink.TryResolvePick ((uint32 layerId <<< 24) ||| 1u)) ValueNone $"preserveOrder={preserveOrder}: retired slot 0 returns ValueNone"

                        emptyColl.AddOrUpdate (struct ("c", makeGlyph 0.0 RgbaColor.white))
                        Expect.equal sink.ActiveCount 1 $"preserveOrder={preserveOrder}: ActiveCount = 1 after re-adding"
                        Expect.equal sink.Batch.Count 1 $"preserveOrder={preserveOrder}: Batch.Count = 1 after re-adding"
                        Expect.equal (sink.TryResolvePick ((uint32 layerId <<< 24) ||| 1u)) (ValueSome "c") $"preserveOrder={preserveOrder}: re-added item maps to slot 0"

                    // 2. preserveOrder = true RemoveAt middle item preserves exact slot order of trailing items
                    let removeColl = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
                    removeColl.AddOrUpdate (struct ("k0", makeGlyph -1.5 RgbaColor.white))
                    removeColl.AddOrUpdate (struct ("k1", makeGlyph -0.5 RgbaColor.white))
                    removeColl.AddOrUpdate (struct ("k2", makeGlyph 0.5 RgbaColor.white))
                    removeColl.AddOrUpdate (struct ("k3", makeGlyph 1.5 RgbaColor.white))
                    let removeRows = removeColl.Rows |> Projection.map (fun struct (_, g) -> g)
                    let removeSink = Reactive3D.bindInstances ctx 3uy boxMesh BlendMode.Opaque false true (fun () -> scale) removeRows

                    Expect.isTrue (removeColl.Remove "k1") "remove middle k1"
                    Expect.equal removeSink.ActiveCount 3 "ActiveCount = 3 after removing middle item"
                    Expect.equal removeSink.Batch.Count 3 "Batch.Count = 3 after removing middle item"
                    Expect.equal (removeSink.TryResolvePick ((3u <<< 24) ||| 1u)) (ValueSome "k0") "slot 0 stays k0"
                    Expect.equal (removeSink.TryResolvePick ((3u <<< 24) ||| 2u)) (ValueSome "k2") "slot 1 shifts to k2 (not tail-swapped k3)"
                    Expect.equal (removeSink.TryResolvePick ((3u <<< 24) ||| 3u)) (ValueSome "k3") "slot 2 shifts to k3"
                    Expect.equal (removeSink.TryResolvePick ((3u <<< 24) ||| 4u)) ValueNone "retired slot 3 returns ValueNone"

                    let removePass =
                        ScenePass.create cam
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withBatches [| removeSink.Batch |]

                    Expect.equal (pickKeyAt removeSink removePass -1.5) (ValueSome "k0") "GPU pick resolves k0 after middle RemoveAt"
                    Expect.equal (pickKeyAt removeSink removePass 0.5) (ValueSome "k2") "GPU pick resolves k2 after middle RemoveAt"
                    Expect.equal (pickKeyAt removeSink removePass 1.5) (ValueSome "k3") "GPU pick resolves k3 after middle RemoveAt"

                    // 3. preserveOrder = true [k0; k1; k2] where k1 mutates to NaN then recovers without OrderChanged
                    let nanColl = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
                    nanColl.AddOrUpdate (struct ("k0", makeGlyph -1.2 (RgbaColor (1.0f, 0.2f, 0.2f, 1.0f))))
                    nanColl.AddOrUpdate (struct ("k1", makeGlyph 0.0 (RgbaColor (0.2f, 1.0f, 0.2f, 1.0f))))
                    nanColl.AddOrUpdate (struct ("k2", makeGlyph 1.2 (RgbaColor (0.2f, 0.2f, 1.0f, 1.0f))))
                    let nanRows = nanColl.Rows |> Projection.map (fun struct (_, g) -> g)
                    let nanSink = Reactive3D.bindInstances ctx 4uy boxMesh BlendMode.Opaque false true (fun () -> scale) nanRows

                    let nanPass =
                        ScenePass.create cam
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withBatches [| nanSink.Batch |]

                    // Mutate k1 to NaN (OrderChanged = false) -> ActiveCount becomes 2: [k0; k2]
                    let nanGlyph = { makeGlyph 0.0 (RgbaColor (0.2f, 1.0f, 0.2f, 1.0f)) with Scale = Vector3 (Single.NaN, 0.35f, 0.35f) }
                    nanColl.AddOrUpdate (struct ("k1", nanGlyph))
                    Expect.equal nanSink.ActiveCount 2 "ActiveCount drops to 2 when k1 becomes NaN"
                    Expect.equal nanSink.Batch.Count 2 "Batch.Count drops to 2 when k1 becomes NaN"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 1u)) (ValueSome "k0") "slot 0 remains k0 while k1 is NaN"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 2u)) (ValueSome "k2") "slot 1 shifts to k2 while k1 is NaN"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 3u)) ValueNone "slot 2 is retired while k1 is NaN"
                    Expect.equal (pickKeyAt nanSink nanPass 0.0) ValueNone "GPU pick at k1 position is ValueNone while NaN"
                    Expect.equal (pickKeyAt nanSink nanPass 1.2) (ValueSome "k2") "GPU pick resolves k2 at slot 1 while k1 is NaN"

                    // Recover k1 back to finite without OrderChanged -> [k0; k1; k2] exact order restored
                    nanColl.AddOrUpdate (struct ("k1", makeGlyph 0.0 (RgbaColor (0.2f, 1.0f, 0.2f, 1.0f))))
                    Expect.equal nanSink.ActiveCount 3 "ActiveCount recovers to 3 when k1 becomes finite"
                    Expect.equal nanSink.Batch.Count 3 "Batch.Count recovers to 3 when k1 becomes finite"
                    Expect.equal nanSink.LastUpdatedSlots 2 "recovery uploads only [insertSlot .. activeCount - 1] (slots 1 and 2)"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 1u)) (ValueSome "k0") "slot 0 is k0 after recovery"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 2u)) (ValueSome "k1") "slot 1 is k1 after recovery"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 3u)) (ValueSome "k2") "slot 2 is k2 after recovery"
                    Expect.equal (pickKeyAt nanSink nanPass -1.2) (ValueSome "k0") "GPU pick resolves k0 at slot 0 after recovery"
                    Expect.equal (pickKeyAt nanSink nanPass 0.0) (ValueSome "k1") "GPU pick resolves k1 at slot 1 after recovery"
                    Expect.equal (pickKeyAt nanSink nanPass 1.2) (ValueSome "k2") "GPU pick resolves k2 at slot 2 after recovery"

                    // Multi-key NaN invalidation and batched recovery also preserves exact order [k0; k1; k2]
                    Api.batch (fun () ->
                        nanColl.AddOrUpdate (struct ("k0", { makeGlyph -1.2 RgbaColor.white with Scale = Vector3 (Single.NaN, 0.35f, 0.35f) }))
                        nanColl.AddOrUpdate (struct ("k1", { makeGlyph 0.0 RgbaColor.white with Scale = Vector3 (Single.NaN, 0.35f, 0.35f) })))

                    Expect.equal nanSink.ActiveCount 1 "only k2 remains active"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 1u)) (ValueSome "k2") "slot 0 is k2 when k0 and k1 are NaN"

                    Api.batch (fun () ->
                        nanColl.AddOrUpdate (struct ("k1", makeGlyph 0.0 RgbaColor.white))
                        nanColl.AddOrUpdate (struct ("k0", makeGlyph -1.2 RgbaColor.white)))

                    Expect.equal nanSink.ActiveCount 3 "batched recovery restores all 3 active slots"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 1u)) (ValueSome "k0") "slot 0 = k0 after batched recovery"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 2u)) (ValueSome "k1") "slot 1 = k1 after batched recovery"
                    Expect.equal (nanSink.TryResolvePick ((4u <<< 24) ||| 3u)) (ValueSome "k2") "slot 2 = k2 after batched recovery"
                )

            testCase "HoverPickCoalescer enforces single-flight coalescing, epoch fencing, follow-up dispatch, and synchronous frame coalescing"
            <| fun () ->
                let coalescer = HoverPickCoalescer ()
                Expect.equal coalescer.CurrentEpoch 0 "starts at epoch 0"
                Expect.equal coalescer.InFlightEpoch ValueNone "starts idle"
                Expect.equal coalescer.PendingClientXY ValueNone "starts with no pending coords"

                // First move dispatches immediately in epoch 0; next 9 overwrite PendingClientXY without launching picks
                Expect.equal (coalescer.RequestHover (10.0, 20.0)) (ValueSome 0) "first move starts single-flight pick at epoch 0"
                Expect.equal coalescer.InFlightEpoch (ValueSome 0) "in-flight epoch tracks 0"

                for i = 1 to 9 do
                    Expect.equal (coalescer.RequestHover (100.0 + float i, 200.0 + float i)) ValueNone $"move {i} coalesces while in flight"

                Expect.equal coalescer.PendingClientXY (ValueSome struct (109.0, 209.0)) "latest coalesced coordinates preserved"

                // Completing current epoch dispatches queued follow-up coordinates at epoch 0
                let struct (isCur1, follow1) = coalescer.CompletePick 0
                Expect.isTrue isCur1 "matching epoch completion is current"
                Expect.equal follow1 (ValueSome struct (109.0, 209.0, 0)) "completing current pick emits last pending coords at epoch 0"
                Expect.equal coalescer.PendingClientXY ValueNone "pending cleared once follow-up starts"
                Expect.equal coalescer.InFlightEpoch (ValueSome 0) "follow-up keeps single-flight in-flight"

                // Queue another move, then switch epoch (scene switch / drag) -> pending cleared and stale completion rejected
                Expect.equal (coalescer.RequestHover (50.0, 60.0)) ValueNone "queues behind follow-up pick"
                coalescer.AdvanceEpoch ()
                Expect.equal coalescer.CurrentEpoch 1 "AdvanceEpoch increments epoch"
                Expect.equal coalescer.PendingClientXY ValueNone "AdvanceEpoch drops pre-switch pending coords"

                let struct (isCurStale, followStale) = coalescer.CompletePick 0
                Expect.isFalse isCurStale "stale epoch 0 completion rejected after AdvanceEpoch"
                Expect.equal followStale ValueNone "no follow-up when no move occurred after AdvanceEpoch"
                Expect.equal coalescer.InFlightEpoch ValueNone "in-flight returns idle"

                // Post-switch move while a pick is in flight still dispatches fresh coordinates when stale readback completes
                Expect.equal (coalescer.RequestHover (1.0, 2.0)) (ValueSome 1) "dispatches in epoch 1"
                coalescer.AdvanceEpoch ()
                Expect.equal coalescer.CurrentEpoch 2 "epoch advances to 2"
                Expect.equal (coalescer.RequestHover (300.0, 400.0)) ValueNone "post-switch move queues while epoch 1 still in flight"
                let struct (isCurStale2, followFresh) = coalescer.CompletePick 1
                Expect.isFalse isCurStale2 "stale epoch 1 response rejected"
                Expect.equal followFresh (ValueSome struct (300.0, 400.0, 2)) "post-switch move dispatches under epoch 2"
                let struct (isCurFinal, followDone) = coalescer.CompletePick 2
                Expect.isTrue isCurFinal "epoch 2 completion accepted"
                Expect.equal followDone ValueNone "becomes idle after final completion"

                // Synchronous frame coalescing for SDL RAF
                for i = 1 to 10 do
                    coalescer.QueueFrameHover (float (i * 10), float (i * 5))

                Expect.isTrue coalescer.HasPendingFrameHover "frame hover queued"
                Expect.equal (coalescer.TakeFrameHover ()) (ValueSome struct (100.0, 50.0, 2)) "TakeFrameHover returns only latest coordinates with CurrentEpoch"
                Expect.isFalse coalescer.HasPendingFrameHover "TakeFrameHover drains queue"
                Expect.equal (coalescer.TakeFrameHover ()) ValueNone "empty frame queue returns ValueNone"

                coalescer.QueueFrameHover (12.0, 34.0)
                coalescer.AdvanceEpoch ()
                Expect.isFalse coalescer.HasPendingFrameHover "AdvanceEpoch clears pending synchronous frame hover"
                Expect.equal (coalescer.TakeFrameHover ()) ValueNone "cleared frame hover yields ValueNone"

            testCase "PickRegistry3D allocates [1..255] layers, rejects duplicates, cleans up on owner disposal, and resolves/dispatches heterogeneous sinks and static picks"
            <| fun () ->
                let scale = Scale3D.cartesian (-2.0, 2.0) (-2.0, 2.0) (-2.0, 2.0)

                // 1. Out-of-graph safety + allocator [1..255] + exhaustion + explicit reservation + release reuse
                let reg = PickRegistry3D ()
                Expect.isTrue (reg.IsLayerInUse 0uy) "layer 0uy is permanently reserved as background sentinel"
                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> reg.AllocateLayer (preferredLayer = 0uy) |> ignore) "preferredLayer = 0uy rejected"

                let l1 = reg.AllocateLayer ()
                let l10 = reg.AllocateLayer (preferredLayer = 10uy)
                let l2 = reg.AllocateLayer ()
                Expect.equal l1 1uy "first auto layer is 1uy"
                Expect.equal l10 10uy "explicit layer 10uy granted"
                Expect.equal l2 2uy "next auto layer is 2uy"
                Expect.throwsT<InvalidOperationException> (fun () -> reg.AllocateLayer (preferredLayer = 10uy) |> ignore) "duplicate explicit reservation rejected"

                // Exhaust remaining 252 layers -> total 255 active
                let rest = [ for _ in 1 .. 252 -> reg.AllocateLayer () ]
                Expect.equal rest.Length 252 "remaining 252 layers allocated"
                Expect.isFalse (List.contains 10uy rest) "explicit layer 10uy skipped by auto-allocator"
                Expect.equal reg.ActiveLayerCount 255 "all 255 layers active"
                Expect.throwsT<InvalidOperationException> (fun () -> reg.AllocateLayer () |> ignore) "256th allocation throws InvalidOperationException"

                reg.ReleaseLayer 10uy
                Expect.isFalse (reg.IsLayerInUse 10uy) "released layer 10uy becomes free"
                Expect.equal (reg.AllocateLayer ()) 10uy "lowest free layer 10uy reused after release"

                for l in rest do
                    reg.ReleaseLayer l
                reg.ReleaseLayer l1
                reg.ReleaseLayer l2
                reg.ReleaseLayer 10uy
                Expect.equal reg.ActiveLayerCount 0 "all layers released"

                // 2. Ranvier nested owner scope cleanup + heterogeneous sinks + option None key + static picks + dispatch
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                use graph = new Graph ()

                graph.Run (fun () ->
                    use boxMesh = ctx.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Box)
                    let scopedReg = PickRegistry3D ()

                    let barHoverCalls = ResizeArray<PickedMark<string>>()
                    let barClickCalls = ResizeArray<PickedMark<string>>()
                    let discHoverCalls = ResizeArray<PickedMark<int option>>()
                    let staticClickCalls = ResizeArray<PickHit>()

                    let barColl = Api.createKeyedCollection (fun struct (k: string, _: BarDatum3D) -> k)
                    barColl.AddOrUpdate (struct ("b1", BarDatum3D.create 0.0 0.0 0.0 1.0 0.5 0.5 RgbaColor.white))
                    barColl.AddOrUpdate (struct ("b2", BarDatum3D.create 1.0 0.0 0.0 1.0 0.5 0.5 RgbaColor.white))
                    let barRows = barColl.Rows |> Projection.map (fun struct (_, b) -> b)

                    let discColl = Api.createKeyedCollection (fun struct (k: int option, _: ScatterDatum3D) -> k)
                    discColl.AddOrUpdate (struct (None, ScatterDatum3D.create (Point3.create -1.0 0.0 0.0) 6.0f RgbaColor.white))
                    discColl.AddOrUpdate (struct (Some 42, ScatterDatum3D.create (Point3.create 1.0 0.0 0.0) 6.0f RgbaColor.white))
                    let discRows = discColl.Rows |> Projection.map (fun struct (_, d) -> d)

                    let mutable layerBar = 0uy
                    let mutable layerDisc = 0uy
                    let mutable staleLayer = 0uy

                    let innerOwner =
                        Api.createRoot (fun root ->
                            layerBar <- scopedReg.AllocateLayer ()
                            let barSink = Reactive3D.bindBars ctx layerBar (fun () -> scale) barRows
                            scopedReg.RegisterSink(
                                barSink,
                                describe = (fun m -> $"Bar:{m.Key}"),
                                onHover = barHoverCalls.Add,
                                onClick = barClickCalls.Add
                            )

                            layerDisc <- scopedReg.AllocateLayer ()
                            let discSink = Reactive3D.bindDiscs ctx layerDisc false (fun () -> scale) discRows
                            scopedReg.RegisterSink(
                                discSink,
                                describe = (fun m -> match m.Key with Some v -> $"Disc:{v}" | None -> "Disc:None"),
                                onHover = discHoverCalls.Add
                            )

                            // Duplicate sink on an occupied layer is rejected
                            Expect.throwsT<InvalidOperationException>
                                (fun () ->
                                    let dupSink = Reactive3D.bindBars ctx layerBar (fun () -> scale) barRows
                                    scopedReg.RegisterSink(dupSink))
                                "collision on active sink layer rejected"

                            // Static picks (layer 0uy and custom pickId)
                            scopedReg.RegisterStaticPick(77u, label = "Surface:wave", onClick = staticClickCalls.Add)
                            scopedReg.RegisterStaticPick(0xFE000001u, label = "IsoSurface:blob")

                            // Stale cleanup token test: manually release staleLayer before innerOwner disposes
                            staleLayer <- scopedReg.AllocateLayer ()
                            scopedReg.ReleaseLayer staleLayer
                            Expect.equal (scopedReg.IsLayerInUse staleLayer) false "manually released layer free"
                            root)

                    // Re-allocate staleLayer outside innerOwner before innerOwner disposes
                    let heldOutside = scopedReg.AllocateLayer (preferredLayer = staleLayer)
                    Expect.equal heldOutside staleLayer "re-allocated staleLayer outside inner scope"

                    let barHit1 = { PickId = ((uint32 layerBar <<< 24) ||| 1u); WorldPosition = Scale3D.map scale (Point3.create 0.0 0.0 1.0) }
                    let discHitNone = { PickId = ((uint32 layerDisc <<< 24) ||| 1u); WorldPosition = Vector3.Zero }
                    let discHit42 = { PickId = ((uint32 layerDisc <<< 24) ||| 2u); WorldPosition = Vector3.Zero }
                    let staticHit = { PickId = 77u; WorldPosition = Vector3 (0.25f, 0.0f, 0.0f) }

                    Expect.equal (scopedReg.TryResolveKeyed<string>(scale, barHit1) |> ValueOption.map _.Key) (ValueSome "b1") "TryResolveKeyed resolves bar b1"
                    Expect.equal (scopedReg.TryResolveKeyed<int>(scale, barHit1)) ValueNone "wrong generic key type returns ValueNone"
                    Expect.equal (scopedReg.TryResolveKeyed<int option>(scale, discHitNone) |> ValueOption.map _.Key) (ValueSome None) "TryResolveKeyed preserves ValueSome None option key"
                    Expect.equal (scopedReg.TryResolveKeyed<int option>(scale, discHit42) |> ValueOption.map _.Key) (ValueSome (Some 42)) "TryResolveKeyed resolves disc Some 42"

                    match scopedReg.TryResolveBoxed(scale, barHit1) with
                    | ValueSome struct (lid, boxed, worldPos, dataPt) ->
                        Expect.equal lid layerBar "boxed layer matches"
                        Expect.equal (unbox<string> boxed) "b1" "boxed key matches"
                        expectVec3Close 1e-5f worldPos barHit1.WorldPosition "boxed world pos matches"
                        Expect.floatClose { absolute = 1e-4; relative = 0.0 } dataPt.Z 1.0 "boxed data point inverted cleanly"
                    | ValueNone -> failtest "Expected TryResolveBoxed hit for barHit1"

                    Expect.equal (scopedReg.DescribeHit(scale, barHit1)) (ValueSome "Bar:b1") "DescribeHit formats bar"
                    Expect.equal (scopedReg.DescribeHit(scale, discHitNone)) (ValueSome "Disc:None") "DescribeHit formats None key"
                    Expect.equal (scopedReg.DescribeHit(scale, staticHit)) (ValueSome "Surface:wave") "DescribeHit formats static pick"
                    Expect.equal (scopedReg.DescribeHit(scale, { PickId = 0xFE000001u; WorldPosition = Vector3.Zero })) (ValueSome "IsoSurface:blob") "DescribeHit formats custom PickId"
                    Expect.equal (scopedReg.DescribeHit(scale, { PickId = 0u; WorldPosition = Vector3.Zero })) ValueNone "DescribeHit ignores PickId 0u"

                    scopedReg.DispatchHover(scale, ValueSome barHit1)
                    scopedReg.DispatchHover(scale, ValueSome discHitNone)
                    scopedReg.DispatchHover(scale, ValueNone)
                    scopedReg.DispatchClick(scale, ValueSome barHit1)
                    scopedReg.DispatchClick(scale, ValueSome staticHit)
                    Expect.equal barHoverCalls.Count 1 "hover dispatched once to bar sink"
                    Expect.equal barHoverCalls[0].Key "b1" "hover passed PickedMark b1"
                    Expect.equal discHoverCalls.Count 1 "hover dispatched once to disc sink"
                    Expect.equal discHoverCalls[0].Key None "hover passed PickedMark None"
                    Expect.equal barClickCalls.Count 1 "click dispatched once to bar sink"
                    Expect.equal staticClickCalls.Count 1 "click dispatched once to static pick"
                    Expect.equal staticClickCalls[0].PickId 77u "static click received PickHit"

                    // Remove b1 -> retired pick ID ignored immediately
                    Expect.isTrue (barColl.Remove "b1") "remove b1"
                    Expect.equal (scopedReg.TryResolveKeyed<string>(scale, { PickId = ((uint32 layerBar <<< 24) ||| 2u); WorldPosition = Vector3.Zero })) ValueNone "retired slot 1 returns ValueNone"
                    Expect.equal (scopedReg.DescribeHit(scale, { PickId = ((uint32 layerBar <<< 24) ||| 2u); WorldPosition = Vector3.Zero })) ValueNone "retired slot DescribeHit returns ValueNone"

                    // Dispose innerOwner -> auto cleans layers & static picks, while keeping staleLayer owned by outer scope
                    innerOwner.Dispose ()
                    Expect.isFalse (scopedReg.IsLayerInUse layerBar) "layerBar freed on owner dispose"
                    Expect.isFalse (scopedReg.IsLayerInUse layerDisc) "layerDisc freed on owner dispose"
                    Expect.isTrue (scopedReg.IsLayerInUse staleLayer) "re-allocated staleLayer NOT clobbered by old owner cleanup"
                    Expect.equal (scopedReg.TryResolveKeyed<string>(scale, barHit1)) ValueNone "disposed scope resolves ValueNone"
                    Expect.equal (scopedReg.DescribeHit(scale, staticHit)) ValueNone "disposed static pick resolves ValueNone"
                    Expect.equal (scopedReg.AllocateLayer ()) 1uy "freed layer 1uy reused after owner disposal"
                )

            testCase "PlotRevision3D, SinkOptions3D, tuple helpers, and translucent depth-sorted bindDiscsWith work seamlessly"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                let scale = Scale3D.cartesian (-2.0, 2.0) (-2.0, 2.0) (-1.0, 1.0)
                use graph = new Graph ()

                graph.Run (fun () ->
                    let reg = PickRegistry3D ()
                    let rev = Reactive3D.createRevision ()
                    let mutable revEvents = 0
                    Api.createEffect (fun () ->
                        let _ = rev.Revision
                        revEvents <- revEvents + 1)

                    Expect.equal rev.Revision 0 "initial PlotRevision3D is 0"
                    Expect.equal revEvents 1 "initial revision observer fires once"

                    let opts =
                        SinkOptions3D.defaultValue
                        |> SinkOptions3D.withRegistry reg
                        |> SinkOptions3D.withRevision rev
                        |> SinkOptions3D.withDrawBeforeLines true
                        |> SinkOptions3D.withBlendMode BlendMode.AlphaBlend
                        |> SinkOptions3D.withPreserveOrder true

                    let mesh = ctx.UploadMesh (Glyph3D.unitMesh GlyphShape3D.Box)
                    let optsOwn = opts |> SinkOptions3D.withOwnsMesh true

                    let coll = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
                    coll.AddOrUpdate (struct ("g1", { GlyphDatum3D.sphere (Point3.create 0.0 0.0 0.0) 0.4f (RgbaColor (0.2f, 0.7f, 1.0f, 0.5f)) with Shading = ShadingMode.Unlit }))

                    let inner =
                        Api.createRoot (fun root ->
                            let sink = Reactive3D.bindInstancesWith ctx optsOwn mesh (fun () -> scale) (Reactive3D.collectionValues coll)
                            Expect.equal sink.LayerId 1uy "SinkOptions3D auto-allocates layer 1uy from Registry"
                            Expect.isTrue sink.Batch.DrawBeforeLines "DrawBeforeLines forwarded to Batch"
                            Expect.equal sink.Batch.BlendMode BlendMode.AlphaBlend "BlendMode forwarded to Batch"
                            Expect.isTrue (reg.IsLayerInUse 1uy) "auto-registered in PickRegistry3D"
                            root)

                    Expect.isGreaterThan rev.Revision 0 "initial sink creation bumped PlotRevision3D"
                    let revAfterCreate = rev.Revision

                    // Mutate g1 -> bumps revision
                    coll.AddOrUpdate (struct ("g1", { GlyphDatum3D.sphere (Point3.create 0.5 0.0 0.0) 0.4f (RgbaColor (0.2f, 0.7f, 1.0f, 0.5f)) with Shading = ShadingMode.Unlit }))
                    let revAfterEdit = rev.Revision
                    Expect.isGreaterThan revAfterEdit revAfterCreate "row mutation bumps PlotRevision3D"

                    // Remove last item -> Batch shrinks to 0 and MUST still bump PlotRevision3D!
                    Expect.isTrue (coll.Remove "g1") "remove last item"
                    Expect.isGreaterThan rev.Revision revAfterEdit "removing last item (Count -> 0) bumps PlotRevision3D"

                    // Disposing inner scope frees registry layer and disposes owned mesh
                    inner.Dispose ()
                    Expect.isFalse (reg.IsLayerInUse 1uy) "disposing scope releases auto-allocated Registry layer"
                    Expect.throwsT<ObjectDisposedException> (fun () -> mesh.UpdateMesh MeshData.empty) "OwnsMesh=true disposed caller-supplied mesh"

                    // Translucent bindDiscsWith with CameraPosition back-to-front sorting & canonical PickId resolution
                    let camPosSig = Api.createSignal (Vector3 (0.0f, 0.0f, 5.0f))
                    let yUpScale = Scale3D.create BoxAspect.Cube UpAxis.YUp (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0))
                    let discColl = Api.createKeyedCollection (fun struct (k: string, _: ScatterDatum3D) -> k)
                    discColl.AddOrUpdate (struct ("back", ScatterDatum3D.create (Point3.create 0.0 0.0 -0.5) 20.0f (RgbaColor (1.0f, 0.0f, 0.0f, 0.55f))))
                    discColl.AddOrUpdate (struct ("front", ScatterDatum3D.create (Point3.create 0.0 0.0 0.5) 20.0f (RgbaColor (0.0f, 0.0f, 1.0f, 0.55f))))

                    let discOpts =
                        SinkOptions3D.defaultValue
                        |> SinkOptions3D.withRegistry reg
                        |> SinkOptions3D.withRevision rev
                        |> SinkOptions3D.withCameraPosition (fun () -> camPosSig.Value)

                    let discSink = Reactive3D.bindDiscsWith ctx discOpts (fun () -> yUpScale) (Reactive3D.values discColl.Rows)
                    let camFront = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.2f 5.0f
                    let discPassFront =
                        ScenePass.create camFront
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withDiscs [| discSink.Batch |]

                    discPassFront |> WgpuContext.render ctx |> ignore
                    let struct (fr, _, fb, _) = pixelAt ctx 32 32
                    Expect.isGreaterThan fb (fr + 30uy) "looking from +Z renders closer blue 'front' disc on top of red 'back' disc"
                    let hitFront = discPassFront |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal (reg.TryResolveKeyed<string>(yUpScale, hitFront) |> ValueOption.map _.Key) (ValueSome "front") "pick from +Z resolves 'front' even after depth sort"

                    // Flip camera to -Z: now red 'back' at z=-0.5 is closer and must render on top!
                    camPosSig.Value <- Vector3 (0.0f, 0.0f, -5.0f)
                    let camOpposite = Camera3D.orthographic Vector3.Zero 180.0f 0.0f 1.2f 5.0f
                    let discPassOpposite =
                        ScenePass.create camOpposite
                        |> ScenePass.withClear (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                        |> ScenePass.withDiscs [| discSink.Batch |]

                    discPassOpposite |> WgpuContext.render ctx |> ignore
                    let struct (orr, _, obb, _) = pixelAt ctx 32 32
                    Expect.isGreaterThan orr (obb + 30uy) "looking from -Z re-sorts translucent discs so closer red 'back' disc draws on top"
                    let hitOpposite = discPassOpposite |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal (reg.TryResolveKeyed<string>(yUpScale, hitOpposite) |> ValueOption.map _.Key) (ValueSome "back") "pick from -Z still resolves 'back' after re-sort"
                )

            testCase "Reactive3D bindGlyphs, bindBoxes, bindArrows, bindPolylines, bindKeyedPolylines, bindTubes, bindRibbons, bindIsoSurface, bindVolumeSlices, bindWallContours"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let yUpScale = Scale3D.create BoxAspect.Cube UpAxis.YUp (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0))
                let cam = Camera3D.orthographic Vector3.Zero 0.0f 0.0f 1.2f 5.0f

                use graph = new Graph ()

                graph.Run (fun () ->
                    let reg = PickRegistry3D ()
                    let rev = Reactive3D.createRevision ()
                    let opts = SinkOptions3D.defaultValue |> SinkOptions3D.withRegistry reg |> SinkOptions3D.withRevision rev

                    // 1. bindGlyphs (owns mesh)
                    let gColl = Api.createKeyedCollection (fun struct (k: string, _: GlyphDatum3D) -> k)
                    gColl.AddOrUpdate (struct ("s1", { GlyphDatum3D.sphere (Point3.create 0.0 0.0 0.0) 0.5f (RgbaColor (0.2f, 0.9f, 0.3f, 1.0f)) with Shading = ShadingMode.Unlit }))
                    let gSink = Reactive3D.bindGlyphs ctx GlyphShape3D.Sphere opts (fun () -> scale) (Reactive3D.collectionValues gColl)
                    Expect.equal gSink.ActiveCount 1 "bindGlyphs activeCount = 1"

                    let gPass = ScenePass.create cam |> ScenePass.withClear RgbaColor.black |> ScenePass.withBatches [| gSink.Batch |]
                    gPass |> WgpuContext.render ctx |> ignore
                    let struct (_, gg, _, _) = pixelAt ctx 32 32
                    Expect.isGreaterThan gg 150uy "bindGlyphs renders green sphere"
                    let gHit = gPass |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal (reg.TryResolveKeyed<string>(scale, gHit) |> ValueOption.map _.Key) (ValueSome "s1") "bindGlyphs picked via PickRegistry3D"

                    // 2. bindBoxes (both YUp and translucent DrawBeforeLines)
                    let boxOpts = opts |> SinkOptions3D.withBlendMode BlendMode.AlphaBlend |> SinkOptions3D.withDrawBeforeLines true
                    let bColl = Api.createKeyedCollection (fun struct (k: string, _: BoxDatum3D) -> k)
                    bColl.AddOrUpdate (struct ("bx1", { BoxDatum3D.create (Bounds3.create -0.4 -0.4 -0.4 0.8 0.8 0.8) (RgbaColor (0.9f, 0.2f, 0.2f, 0.6f)) with Shading = ShadingMode.Unlit }))
                    let bSink = Reactive3D.bindBoxes ctx boxOpts (fun () -> yUpScale) (Reactive3D.values bColl.Rows)
                    Expect.isTrue bSink.Batch.DrawBeforeLines "bindBoxes honors DrawBeforeLines"
                    Expect.equal bSink.Batch.BlendMode BlendMode.AlphaBlend "bindBoxes honors AlphaBlend"
                    let bHit = (ScenePass.create cam |> ScenePass.withBatches [| bSink.Batch |]) |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal (reg.TryResolveKeyed<string>(yUpScale, bHit) |> ValueOption.map _.Key) (ValueSome "bx1") "bindBoxes picked"

                    // 3. bindArrows (validates segments >= 3 and renders/picks)
                    Expect.throwsT<ArgumentOutOfRangeException> (fun () ->
                        let eColl = Api.createKeyedCollection (fun struct (k: string, _: ArrowDatum3D) -> k)
                        Reactive3D.bindArrows ctx 2 opts (fun () -> scale) (Reactive3D.collectionValues eColl) |> ignore) "bindArrows rejects segments < 3"

                    let aColl = Api.createKeyedCollection (fun struct (k: string, _: ArrowDatum3D) -> k)
                    aColl.AddOrUpdate (struct ("a1", { ArrowDatum3D.create (Point3.create -0.5 0.0 0.0) (Point3.create 1.0 0.0 0.0) 0.1f (RgbaColor (1.0f, 1.0f, 0.0f, 1.0f)) with Shading = ShadingMode.Unlit }))
                    let aSink = Reactive3D.bindArrows ctx 10 opts (fun () -> scale) (Reactive3D.collectionValues aColl)
                    Expect.equal aSink.ActiveCount 1 "bindArrows activeCount = 1"
                    let aPass = ScenePass.create cam |> ScenePass.withClear RgbaColor.black |> ScenePass.withBatches [| aSink.Batch |]
                    aPass |> WgpuContext.render ctx |> ignore
                    let aHit = aPass |> WgpuContext.pick 36 32 ctx |> ValueOption.get
                    Expect.equal (reg.TryResolveKeyed<string>(scale, aHit) |> ValueOption.map _.Key) (ValueSome "a1") "bindArrows picked"

                    // bindBarsWith, bindSegmentsWith, bindSurfaceWith
                    let barWColl = Api.createKeyedCollection (fun struct (k: string, _: BarDatum3D) -> k)
                    barWColl.AddOrUpdate (struct ("bw1", BarDatum3D.create 0.0 0.0 -0.5 0.5 0.4 0.4 RgbaColor.white))
                    let barWSink = Reactive3D.bindBarsWith ctx opts (fun () -> scale) (Reactive3D.collectionValues barWColl)
                    Expect.equal barWSink.ActiveCount 1 "bindBarsWith activeCount = 1"

                    let segWColl = Api.createKeyedCollection (fun struct (k: string, _: struct (Point3 * Point3 * RgbaColor)) -> k)
                    segWColl.AddOrUpdate (struct ("sw1", struct (Point3.create -0.5 0.0 0.0, Point3.create 0.5 0.0 0.0, RgbaColor.white)))
                    let segWSink = Reactive3D.bindSegmentsWith ctx { opts with IgnoreSectionPlane = true } (fun () -> scale) (Reactive3D.collectionValues segWColl)
                    Expect.equal segWSink.ActiveCount 1 "bindSegmentsWith activeCount = 1"

                    let surfSpecSig = Api.createSignal (SurfaceSpec.create scale (ColorRamp.viridis (-1.0, 1.0)))
                    let surfGridSig = Api.createSignal (SurfaceGrid.fromFunction3D 4 4 (-0.5, 0.5) (-0.5, 0.5) (fun x y -> x + y) |> unwrapOk "surfGrid")
                    let revBeforeSurf = rev.Revision
                    let surfWSink = Reactive3D.bindSurfaceWith ctx (opts |> SinkOptions3D.withPickId 89u) (fun () -> surfSpecSig.Value) (fun () -> surfGridSig.Value)
                    Expect.isGreaterThan rev.Revision revBeforeSurf "bindSurfaceWith bumps revision on init"
                    let revAfterSurfInit = rev.Revision
                    surfGridSig.Value <- (SurfaceGrid.fromFunction3D 4 4 (-0.5, 0.5) (-0.5, 0.5) (fun _ _ -> 0.2) |> unwrapOk "surfGrid2")
                    Expect.isGreaterThan rev.Revision revAfterSurfInit "bindSurfaceWith bumps revision on UpdateGrid"
                    Expect.equal surfWSink.Batch.Count 1 "bindSurfaceWith active batch count = 1"

                    // 4. bindPolylines, bindPolylinesWithGaps, and bindKeyedPolylines
                    let cyan = RgbaColor (0.0f, 1.0f, 1.0f, 1.0f)
                    let polySig = Api.createSignal [| [| struct (Point3.create -0.8 0.0 0.0, cyan); struct (Point3.create 0.8 0.0 0.0, cyan) |] |]
                    let pBatch = Reactive3D.bindPolylines ctx { opts with IgnoreSectionPlane = true } (fun () -> scale) (fun () -> polySig.Value)
                    Expect.equal pBatch.SegmentCount 1 "bindPolylines initial segment count = 1"
                    polySig.Value <- [| [| struct (Point3.create -0.8 0.0 0.0, cyan); struct (Point3.create 0.0 0.0 0.0, cyan); struct (Point3.create 0.8 0.0 0.0, cyan) |] |]
                    Expect.equal pBatch.SegmentCount 2 "bindPolylines updated segment count = 2"

                    let gapSig =
                        Api.createSignal
                            [|
                                [|
                                    ValueSome struct (Point3.create -0.8 0.0 0.0, cyan)
                                    ValueSome struct (Point3.create -0.2 0.0 0.0, cyan)
                                    ValueNone
                                    ValueSome struct (Point3.create 0.2 0.0 0.0, cyan)
                                    ValueSome struct (Point3.create 0.8 0.0 0.0, cyan)
                                |]
                            |]
                    let gLineBatch = Reactive3D.bindPolylinesWithGaps ctx opts (fun () -> scale) (fun () -> gapSig.Value)
                    Expect.equal gLineBatch.SegmentCount 2 "bindPolylinesWithGaps splits across ValueNone into 2 segments"

                    let kPolyColl = Api.createKeyedCollection (fun struct (k: string, _: struct (Point3 * RgbaColor) voption array) -> k)
                    kPolyColl.AddOrUpdate (struct ("p1", [| ValueSome struct (Point3.create -0.5 0.0 0.0, cyan); ValueSome struct (Point3.create 0.5 0.0 0.0, cyan) |]))
                    let kPolySink = Reactive3D.bindKeyedPolylines ctx opts (fun () -> scale) (Reactive3D.collectionValues kPolyColl)
                    Expect.equal kPolySink.ActiveCount 1 "bindKeyedPolylines activeCount = 1"
                    Expect.equal kPolySink.SegmentCount 1 "bindKeyedPolylines SegmentCount = 1"
                    Expect.isTrue (kPolyColl.Remove "p1") "remove keyed polyline"
                    Expect.equal kPolySink.ActiveCount 0 "bindKeyedPolylines shrinks to 0"
                    Expect.equal kPolySink.Batch.SegmentCount 0 "bindKeyedPolylines LineBatch shrinks to 0"

                    // 5. bindTubes and bindRibbons (ShadingMode change preserves mesh + empty recovery + pickId)
                    let tubeOpts = opts |> SinkOptions3D.withPickId 91u
                    let tubeShadeSig = Api.createSignal ShadingMode.Unlit
                    let tubeInputSig =
                        Api.createSignal
                            [|
                                [|
                                    struct (Point3.create -0.7 0.0 0.0, 0.12f, RgbaColor (1.0f, 0.3f, 0.2f, 1.0f))
                                    struct (Point3.create 0.7 0.0 0.0, 0.12f, RgbaColor (0.2f, 0.6f, 1.0f, 1.0f))
                                |]
                            |]

                    Expect.throwsT<ArgumentOutOfRangeException> (fun () ->
                        Reactive3D.bindTubes ctx tubeOpts 2 (fun () -> ShadingMode.Unlit) (fun () -> scale) (fun () -> tubeInputSig.Value) |> ignore) "bindTubes rejects sides < 3"

                    let tubeBatch = Reactive3D.bindTubes ctx tubeOpts 8 (fun () -> tubeShadeSig.Value) (fun () -> scale) (fun () -> tubeInputSig.Value)
                    let tubePass = ScenePass.create cam |> ScenePass.withClear RgbaColor.black |> ScenePass.withBatches [| tubeBatch |]
                    tubePass |> WgpuContext.render ctx |> ignore
                    let tubeHit = tubePass |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal tubeHit.PickId 91u "bindTubes renders and returns PickId 91u"

                    let revBeforeShade = rev.Revision
                    tubeShadeSig.Value <- ShadingMode.ExactUnlit
                    Expect.isGreaterThan rev.Revision revBeforeShade "ShadingMode change on bindTubes updates batch and bumps revision"
                    tubeInputSig.Value <- [||]
                    Expect.equal tubeBatch.Count 0 "empty tube list clears instance count"

                    let ribOpts = opts |> SinkOptions3D.withPickId 92u
                    let ribSig =
                        Api.createSignal
                            [|
                                [|
                                    struct (Point3.create -0.6 0.0 -0.4, Point3.create -0.6 0.0 0.4, cyan)
                                    struct (Point3.create 0.6 0.0 -0.4, Point3.create 0.6 0.0 0.4, cyan)
                                |]
                            |]
                    let ribBatch = Reactive3D.bindRibbons ctx ribOpts (fun () -> ShadingMode.Unlit) (fun () -> scale) (fun () -> ribSig.Value)
                    let ribPass = ScenePass.create cam |> ScenePass.withClear RgbaColor.black |> ScenePass.withBatches [| ribBatch |]
                    let ribHit = ribPass |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal ribHit.PickId 92u "bindRibbons renders and returns PickId 92u"

                    // 6. bindIsoSurface (async non-blocking settle + dynamic Opaque <-> AlphaBlend + PickId + revision bump)
                    let volBounds = Bounds3.create -1.0 -1.0 -1.0 2.0 2.0 2.0
                    let vol = VolumeGrid.fromFunction 16 16 16 volBounds (fun x y z -> 1.0f - float32 (x * x + y * y + z * z)) ValueNone |> unwrapOk "isoVol"
                    let volRamp = ColorRamp.viridis (-1.0, 1.0)
                    let isoThreshSig = Api.createSignal 0.3f
                    let isoCfgMemo = Api.createMemo (fun _ -> IsoSurfaceConfig.create isoThreshSig.Value)
                    let isoOpacitySig = Api.createSignal 1.0f
                    let isoSink =
                        Reactive3D.bindIsoSurface
                            ctx
                            (opts |> SinkOptions3D.withPickId 93u)
                            (fun () -> ShadingMode.Unlit)
                            (fun () -> isoOpacitySig.Value)
                            (fun () -> scale)
                            (fun () -> volRamp)
                            (fun () -> isoCfgMemo.Value)
                            (fun () -> vol)

                    let swIso = Stopwatch.StartNew ()
                    while isoSink.TriangleCount = 0 && swIso.ElapsedMilliseconds < 10000L do
                        graph.Pump () |> ignore
                        Thread.Sleep 5

                    Expect.isGreaterThan isoSink.TriangleCount 0 "bindIsoSurface settled asynchronously without blocking"
                    Expect.equal isoSink.Batch.BlendMode BlendMode.Opaque "isoSink starts Opaque at opacity 1.0f"
                    let isoRevAtSettle = rev.Revision
                    isoOpacitySig.Value <- 0.5f
                    Expect.equal isoSink.Batch.BlendMode BlendMode.AlphaBlend "isoSink switches dynamically to AlphaBlend at opacity 0.5f"
                    Expect.isGreaterThan rev.Revision isoRevAtSettle "opacity switch bumps PlotRevision3D"
                    let isoHit = (ScenePass.create cam |> ScenePass.withBatches [| isoSink.Batch |]) |> WgpuContext.pick 32 32 ctx |> ValueOption.get
                    Expect.equal isoHit.PickId 93u "isoSink picks at 93u"

                    // 7. bindVolumeSlices (double-sided + dynamic Opaque <-> AlphaBlend)
                    let sliceOpSig = Api.createSignal 1.0f
                    let sliceIdxSig = Api.createSignal (SliceIndices3D.ofY 8)
                    let sliceSink =
                        Reactive3D.bindVolumeSlices
                            ctx
                            (opts |> SinkOptions3D.withPickId 94u)
                            (fun () -> ShadingMode.Unlit)
                            (fun () -> sliceOpSig.Value)
                            (fun () -> scale)
                            (fun () -> volRamp)
                            (fun () -> sliceIdxSig.Value)
                            (fun () -> vol)

                    Expect.isGreaterThan sliceSink.TriangleCount 0 "bindVolumeSlices extracts Y-slice triangles"
                    Expect.isTrue sliceSink.Batch.DoubleSided "bindVolumeSlices creates double-sided slice batch"
                    Expect.equal sliceSink.Batch.BlendMode BlendMode.Opaque "bindVolumeSlices starts Opaque"
                    sliceOpSig.Value <- 0.45f
                    Expect.equal sliceSink.Batch.BlendMode BlendMode.AlphaBlend "bindVolumeSlices transitions to AlphaBlend"

                    // 8. bindWallContours (section-exempt back-wall projection LineBatch)
                    let axisFrame = Axis3D.build (Axis3D.defaultSpec scale) 64 64 cam
                    let backWallsSig = Api.createSignal (Axis3D.backWallPlanes axisFrame)
                    let densityGrid =
                        Density.grid
                            (DensityConfig.create (Bounds.create -1.0 -1.0 2.0 2.0) 8 8)
                            [ { Id = "c"; Position = Point.create 0.0 0.0; Weight = 10.0 } ]
                        |> unwrapOk "densityGrid"
                    let contours = Contour.compute (ContourConfig.create [ 2.0 ]) densityGrid |> unwrapOk "contours"
                    let contourBatch =
                        Reactive3D.bindWallContours
                            ctx
                            opts
                            (fun () -> scale)
                            (fun () -> backWallsSig.Value)
                            (fun () -> WallPlane.XY)
                            (fun () -> RgbaColor.white)
                            (fun () -> contours)

                    Expect.isGreaterThan contourBatch.SegmentCount 0 "bindWallContours projects ContourPolygon segments"
                    ScenePass.create cam
                    |> ScenePass.withClear RgbaColor.black
                    |> ScenePass.withSection (SectionPlane.create Vector3.UnitY -10.0f)
                    |> ScenePass.withLines [| contourBatch |]
                    |> WgpuContext.render ctx
                    |> ignore
                    let contourVisible = ctx.RgbaPixels |> Array.chunkBySize 4 |> Array.exists (fun px -> px[0] > 100uy)
                    Expect.isTrue contourVisible "bindWallContours stays visible under full clipping SectionPlane"
                )

            testCase "Portable Plot3D: Axis3DMemo caches rebuilds, PlotInputKernel3D handles orbit/pan/hover-gate/interactive-guard/keys, and Layer3D/Plot3D.layers compose sinks"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let mutable formatCalls = 0
                let baseSpec = Axis3D.defaultSpec scale
                let spec1 =
                    { baseSpec with
                        X = { baseSpec.X with FormatTick = Some (fun v -> formatCalls <- formatCalls + 1; $"{v:F1}") }
                    }
                let cam = PlotCamera3D.toCamera3D scale PlotCamera3D.defaultValue

                // 1. Axis3DMemo uploads once and caches unchanged inputs
                use boxMesh = ctx.UploadMesh (Primitives.unitBox ())
                use panelBatch = ctx.CreateBatch (boxMesh, BlendMode.AlphaBlend, false, drawBeforeLines = true)
                use axisLineBatch = ctx.CreateLineBatch ()
                let memo = Axis3DMemo (panelBatch, axisLineBatch)

                let u1 = memo.Update (spec1, 480, 360, cam)
                Expect.isTrue u1.Rebuilt "first Axis3DMemo update builds"
                let callsAfterFirst = formatCalls
                Expect.isGreaterThan callsAfterFirst 0 "FormatTick called on initial Axis3DMemo build"

                let u2 = memo.Update (spec1, 480, 360, cam)
                Expect.isFalse u2.Rebuilt "unchanged inputs hit Axis3DMemo cache"
                Expect.isFalse u2.AxisSpecChanged "unchanged spec ref reports AxisSpecChanged = false"
                Expect.equal formatCalls callsAfterFirst "cache hit invokes 0 FormatTick calls"

                let spec2 = Axis3D.withPlotTitle "Switched Scene" spec1
                let u3 = memo.Update (spec2, 480, 360, cam)
                Expect.isTrue u3.Rebuilt "changed AxisSpec rebuilds"
                Expect.isTrue u3.AxisSpecChanged "changed AxisSpec reference reports AxisSpecChanged = true"

                // 2. PlotInputKernel3D guards interactive target, gates hover on allowHover, orbits, clicks, wheels, keys
                let kernel = PlotInputKernel3D ()
                Expect.isFalse (kernel.OnPointerDown (1, 0s, 100.0, 100.0, false, true)) "interactive target pointerdown ignored"
                Expect.isFalse kernel.IsDragging "interactive target never starts drag"
                Expect.equal (kernel.OnPointerMove (1, 120.0, 120.0, 480.0, 360.0, 1.0f, PlotCamera3D.defaultValue, false)) PointerMoveResult3D.NoOp "hover gated off when allowHover = false"
                Expect.equal (kernel.OnPointerMove (1, 120.0, 120.0, 480.0, 360.0, 1.0f, PlotCamera3D.defaultValue, true)) PointerMoveResult3D.HoverMoved "hover emits HoverMoved when allowHover = true"

                Expect.isTrue (kernel.OnPointerDown (5, 0s, 100.0, 100.0, false, false)) "canvas pointerdown starts drag"
                Expect.isTrue kernel.IsDragging "dragging active"
                Expect.equal (kernel.OnPointerMove (99, 120.0, 120.0, 480.0, 360.0, 1.0f, PlotCamera3D.defaultValue, true)) PointerMoveResult3D.NoOp "foreign pointerId move ignored while dragging"

                match kernel.OnPointerMove (5, 130.0, 110.0, 480.0, 360.0, 1.0f, PlotCamera3D.defaultValue, false) with
                | PointerMoveResult3D.CameraUpdated cNext ->
                    Expect.notEqual cNext.AzimuthRad PlotCamera3D.defaultValue.AzimuthRad "drag updates camera AzimuthRad"
                | other -> failtestf "Expected CameraUpdated, got %A" other

                let upDrag = kernel.OnPointerUp (5, 0s, 130.0, 110.0)
                Expect.isTrue upDrag.Handled "pointerup handled for active pointer"
                Expect.isFalse upDrag.WasClick "moved drag is not a click"

                Expect.isTrue (kernel.OnPointerDown (6, 0s, 50.0, 50.0, false, false)) "second pointerdown"
                let upClick = kernel.OnPointerUp (6, 0s, 50.5, 50.5)
                Expect.isTrue upClick.Handled "click pointerup handled"
                Expect.isTrue upClick.WasClick "unmoved primary pointerup is a click"

                Expect.equal (PlotInputKernel3D.ApplyDefaultKey ("r", PlotCamera3D.defaultValue)) KeyCameraAction3D.Reset "'r' resets camera"
                Expect.isTrue (match PlotInputKernel3D.ApplyWheel (-120.0, PlotCamera3D.defaultValue) with ValueSome c -> c.Zoom > 1.0f | ValueNone -> false) "negative wheel zooms in"

                // 3. Layer3D and Plot3D.layers compile Surface+Wireframe, KeyedInstances, Discs, Polylines, Colorbar, and CategoricalLegend
                use graph = new Graph ()

                graph.Run (fun () ->
                    let reg = PickRegistry3D ()
                    let opts = SinkOptions3D.defaultValue |> SinkOptions3D.withRegistry reg
                    let ramp = ColorRamp.viridis (-1.0, 1.0)
                    let sGrid = SurfaceGrid.fromFunction3D 4 4 (-0.5, 0.5) (-0.5, 0.5) (fun x y -> x * y) |> unwrapOk "layerGrid"
                    let sSpec = { SurfaceSpec.create scale ramp with Wireframe = ValueSome RgbaColor.white }
                    let surf = Reactive3D.bindSurfaceWith ctx opts (fun () -> sSpec) (fun () -> sGrid)

                    let barColl = Api.createKeyedCollection (fun struct (k: string, _: BarDatum3D) -> k)
                    barColl.AddOrUpdate (struct ("b1", BarDatum3D.create 0.0 0.0 0.0 0.5 0.3 0.3 RgbaColor.white))
                    let barSink = Reactive3D.bindBarsWith ctx opts (fun () -> scale) (Reactive3D.collectionValues barColl)

                    let discColl = Api.createKeyedCollection (fun struct (k: string, _: ScatterDatum3D) -> k)
                    discColl.AddOrUpdate (struct ("d1", ScatterDatum3D.create (Point3.create 0.0 0.0 0.5) 8.0f RgbaColor.white))
                    let discSink = Reactive3D.bindDiscsWith ctx opts (fun () -> scale) (Reactive3D.collectionValues discColl)

                    let cbSpec = ColorbarSpec3D.vertical (Bounds.create 720.0 80.0 18.0 240.0) "Height" (-1.0, 1.0) ramp
                    let swatches = [ { Id = "s1"; Label = "Group A"; Color = RgbaColor.white; Visible = true } ]

                    let compiled =
                        Plot3D.layers
                            [
                                Layer3D.surface surf
                                Layer3D.keyedInstances barSink
                                Layer3D.keyedDiscs discSink
                                Layer3D.colorbar cbSpec
                                Layer3D.categorical (Point.create 20.0 20.0) (Some "Legend") swatches
                                Layer3D.overlayMarks [ Mark.text "hud/layer" (Point.create 10.0 10.0) "OK" ]
                            ]

                    Expect.equal compiled.Batches.Length 2 "surface + barSink batches compiled"
                    Expect.equal compiled.Lines.Length 1 "surface WireframeBatch automatically collected into Lines"
                    Expect.equal compiled.Discs.Length 1 "discSink batch compiled"
                    Expect.equal compiled.OverlayMarks.Length 1 "OverlayMarks collected"
                    Expect.isTrue compiled.ExtraOverlay.IsSome "Colorbar + CategoricalLegend combined into ExtraOverlay"
                    let combinedDefs = Scene.definitions compiled.ExtraOverlay.Value
                    Expect.equal combinedDefs.Length 1 "colorbar LinearGradient preserved in combined overlay"
                    let combinedMarks = Scene.marks compiled.ExtraOverlay.Value
                    Expect.isTrue (combinedMarks |> List.exists (fun m -> Mark.key m = "legend3d/swatch/s1/box")) "categorical swatch preserved in combined overlay"
                )

            testCase "PickRegistry3D preserves static pick labels/handlers across SinkOptions3D sinks and UpdateGrid, and rejects colliding static PickIds"
            <| fun () ->
                use ctx = new WgpuContext (initialWidth = 64, initialHeight = 64)
                let scale = Scale3D.cartesian (-1.0, 1.0) (-1.0, 1.0) (-1.0, 1.0)
                let ramp = ColorRamp.viridis (-1.0, 1.0)
                let grid1 = SurfaceGrid.fromFunction3D 6 6 (-0.8, 0.8) (-0.8, 0.8) (fun x y -> x * y) |> unwrapOk "staticRegGrid1"
                let grid2 = SurfaceGrid.fromFunction3D 6 6 (-0.8, 0.8) (-0.8, 0.8) (fun x y -> x + y) |> unwrapOk "staticRegGrid2"
                use graph = new Graph ()

                graph.Run (fun () ->
                    let reg = PickRegistry3D ()
                    let preHovers = ResizeArray<PickHit>()
                    let preClicks = ResizeArray<PickHit>()
                    let postHovers = ResizeArray<PickHit>()
                    let postClicks = ResizeArray<PickHit>()
                    let tubeHovers = ResizeArray<PickHit>()
                    let tubeClicks = ResizeArray<PickHit>()

                    // 1. Pre-registered static picks survive bindSurfaceWith + reactive UpdateGrid, and bindTubes
                    reg.RegisterStaticPick(701u, label = "Surface:pre", onHover = preHovers.Add, onClick = preClicks.Add)
                    reg.RegisterStaticPick(703u, label = "Tubes:pre", onHover = tubeHovers.Add, onClick = tubeClicks.Add)

                    let specSig1 = Api.createSignal { SurfaceSpec.create scale ramp with PickId = 701u }
                    let gridSig1 = Api.createSignal grid1
                    let _surfPre =
                        Reactive3D.bindSurfaceWith
                            ctx
                            (SinkOptions3D.defaultValue |> SinkOptions3D.withRegistry reg |> SinkOptions3D.withPickId 701u)
                            (fun () -> specSig1.Value)
                            (fun () -> gridSig1.Value)

                    let hitPre = { PickId = 701u; WorldPosition = Vector3.Zero }
                    Expect.equal (reg.DescribeHit(scale, hitPre)) (ValueSome "Surface:pre") "bindSurfaceWith preserves pre-registered static PickId label"
                    reg.DispatchHover(scale, ValueSome hitPre)
                    reg.DispatchClick(scale, ValueSome hitPre)
                    Expect.equal preHovers.Count 1 "bindSurfaceWith preserves pre-registered static PickId hover handler"
                    Expect.equal preClicks.Count 1 "bindSurfaceWith preserves pre-registered static PickId click handler"

                    // Reactive UpdateGrid must NOT clobber pre-registered static pick
                    gridSig1.Value <- grid2
                    specSig1.Value <- { SurfaceSpec.create scale ramp with PickId = 701u; Opacity = 0.9f }
                    Expect.equal (reg.DescribeHit(scale, hitPre)) (ValueSome "Surface:pre") "UpdateGrid preserves pre-registered static PickId label"
                    reg.DispatchHover(scale, ValueSome hitPre)
                    reg.DispatchClick(scale, ValueSome hitPre)
                    Expect.equal preHovers.Count 2 "UpdateGrid preserves pre-registered static PickId hover handler"
                    Expect.equal preClicks.Count 2 "UpdateGrid preserves pre-registered static PickId click handler"

                    let _tubeBatch =
                        Reactive3D.bindTubes
                            ctx
                            (SinkOptions3D.defaultValue |> SinkOptions3D.withRegistry reg |> SinkOptions3D.withPickId 703u)
                            6
                            (fun () -> ShadingMode.Unlit)
                            (fun () -> scale)
                            (fun () -> [| [| struct (Point3.create -0.5 0.0 0.0, 0.1f, RgbaColor.white); struct (Point3.create 0.5 0.0 0.0, 0.1f, RgbaColor.white) |] |])

                    let hitTube = { PickId = 703u; WorldPosition = Vector3.Zero }
                    Expect.equal (reg.DescribeHit(scale, hitTube)) (ValueSome "Tubes:pre") "bindTubes preserves pre-registered static PickId label"
                    reg.DispatchHover(scale, ValueSome hitTube)
                    reg.DispatchClick(scale, ValueSome hitTube)
                    Expect.equal tubeHovers.Count 1 "bindTubes preserves pre-registered static PickId hover handler"
                    Expect.equal tubeClicks.Count 1 "bindTubes preserves pre-registered static PickId click handler"

                    // 2. Post-registered static pick after bindSurfaceWith survives subsequent UpdateGrid updates
                    let specSig2 = Api.createSignal (SurfaceSpec.create scale ramp)
                    let gridSig2 = Api.createSignal grid1
                    let _surfPost =
                        Reactive3D.bindSurfaceWith
                            ctx
                            (SinkOptions3D.defaultValue |> SinkOptions3D.withRegistry reg |> SinkOptions3D.withPickId 702u)
                            (fun () -> specSig2.Value)
                            (fun () -> gridSig2.Value)

                    reg.RegisterStaticPick(702u, label = "Surface:post", onHover = postHovers.Add, onClick = postClicks.Add)
                    gridSig2.Value <- grid2
                    let hitPost = { PickId = 702u; WorldPosition = Vector3.Zero }
                    Expect.equal (reg.DescribeHit(scale, hitPost)) (ValueSome "Surface:post") "UpdateGrid preserves post-registered static PickId label"
                    reg.DispatchHover(scale, ValueSome hitPost)
                    reg.DispatchClick(scale, ValueSome hitPost)
                    Expect.equal postHovers.Count 1 "UpdateGrid preserves post-registered static PickId hover handler"
                    Expect.equal postClicks.Count 1 "UpdateGrid preserves post-registered static PickId click handler"

                    // 3. Duplicate active RegisterStaticPick throws InvalidOperationException and inner disposal never deletes outer entry
                    let outerClicks = ResizeArray<PickHit>()
                    reg.RegisterStaticPick(704u, label = "Outer:static", onClick = outerClicks.Add)

                    let collideInner =
                        Api.createRoot (fun root ->
                            Expect.throwsT<InvalidOperationException>
                                (fun () -> reg.RegisterStaticPick(704u, label = "Inner:collide"))
                                "duplicate active RegisterStaticPick throws InvalidOperationException"
                            root)

                    collideInner.Dispose ()
                    let hit704 = { PickId = 704u; WorldPosition = Vector3.Zero }
                    Expect.equal (reg.DescribeHit(scale, hit704)) (ValueSome "Outer:static") "colliding inner registration and disposal preserve outer static label"
                    reg.DispatchClick(scale, ValueSome hit704)
                    Expect.equal outerClicks.Count 1 "colliding inner registration and disposal preserve outer static click handler"

                    // 4. Manual UnregisterStaticPick + re-register in outer scope does not let old owner disposal remove new entry
                    let manualInner =
                        Api.createRoot (fun root ->
                            reg.RegisterStaticPick(705u, label = "ScopeA")
                            reg.UnregisterStaticPick 705u
                            root)

                    reg.RegisterStaticPick(705u, label = "ScopeB")
                    manualInner.Dispose ()
                    Expect.equal (reg.DescribeHit(scale, { PickId = 705u; WorldPosition = Vector3.Zero })) (ValueSome "ScopeB") "old scope disposal after manual UnregisterStaticPick does not clear later ScopeB entry"
                )
        ]

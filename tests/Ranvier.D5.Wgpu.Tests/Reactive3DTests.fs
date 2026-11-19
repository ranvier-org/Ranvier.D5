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
                    let struct (bx, fy, bz) = Axis3D.backWallPlanes axisFrame
                    expectVec3Close 1e-5f spikes.Span[0].Position surfHit.WorldPosition "spike 0 starts at worldPos"
                    expectVec3Close 1e-5f spikes.Span[1].Position (Vector3 (bx, surfHit.WorldPosition.Y, surfHit.WorldPosition.Z)) "spike 0 drops to back X wall"
                    expectVec3Close 1e-5f spikes.Span[3].Position (Vector3 (surfHit.WorldPosition.X, fy, surfHit.WorldPosition.Z)) "spike 1 drops to floor Y wall"
                    expectVec3Close 1e-5f spikes.Span[5].Position (Vector3 (surfHit.WorldPosition.X, surfHit.WorldPosition.Y, bz)) "spike 2 drops to back Z wall"

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
        ]

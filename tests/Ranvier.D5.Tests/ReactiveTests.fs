module Ranvier.D5.Tests.ReactiveTests

open Expecto
open Ranvier
open Ranvier.D5

let tests = testList "reactivity and interaction" [
    testCase "nice preserves logarithmic domains" <| fun _ ->
        let scale = Scale.log 10. (0.1, 9.) (0., 1.) |> Scale.nice 4
        Expect.isTrue (System.Double.IsFinite(Scale.map scale 1.)) "nice log remains usable"
        Expect.throwsT<System.ArgumentException> (fun () -> Scale.log 10. (nan, 9.) (0., 1.) |> ignore) "nonfinite domain rejected clearly"
    testCase "transition retargets from its current sample" <| fun _ ->
        let ease = Ranvier.Anime.Ease.named "linear"
        let segment = Transition.start 0. 100. ease Ranvier.Anime.Utils.lerp 0. 10.
        Expect.equal (Transition.sample 50. segment) 5. "midpoint"
        let next = Transition.retarget 50. 20. segment
        Expect.equal (Transition.sample 50. next) 5. "continuous retarget"
        Expect.equal (Transition.sample 150. next) 20. "exact endpoint"
        Expect.equal (Transition.sample -10. segment) 0. "clock before start"
    testCase "zero duration completes immediately" <| fun _ ->
        let segment = Transition.start 10. 0. id Ranvier.Anime.Utils.lerp 2. 8.
        Expect.equal (Transition.sample 10. segment) 8. "instant"
        Expect.throws (fun () -> Transition.start 0. -1. id Ranvier.Anime.Utils.lerp 0. 1. |> ignore) "negative duration"
    testCase "derived values use Ranvier signals" <| fun _ ->
        use graph = new Graph()
        graph.Run(fun () ->
            let source = Api.createSignal 2
            let doubled = Reactive.derive (fun () -> source.Value) ((*) 2)
            Expect.equal doubled.Value 4 "initial"
            source.Value <- 7
            Expect.equal doubled.Value 14 "updated")
    testCase "animation follows injected clock and target signals" <| fun _ ->
        use graph = new Graph()
        graph.Run(fun () ->
            let clock = Api.createSignal 0.
            let target = Api.createSignal 0.
            let value = Reactive.animate (fun () -> clock.Value) (fun () -> target.Value) 100. id Ranvier.Anime.Utils.lerp
            Expect.equal value.Value 0. "initial"
            target.Value <- 10.
            Expect.equal value.Value 0. "starts at current value"
            clock.Value <- 50.
            Expect.equal value.Value 5. "halfway"
            target.Value <- 20.
            Expect.equal value.Value 5. "retarget without jump"
            clock.Value <- 150.
            Expect.equal value.Value 20. "done")
    testCase "owned reactive effects batch and stop on disposal" <| fun _ ->
        use graph = new Graph()
        graph.Run(fun () ->
            let source = Api.createSignal 0
            let observed = ResizeArray<int>()
            let owner = Api.createRoot(fun owner ->
                let value = Reactive.derive (fun () -> source.Value) ((*) 2)
                Api.createEffect (fun () -> observed.Add value.Value)
                owner)
            Api.flush ()
            Api.batch (fun () -> source.Value <- 1; source.Value <- 2)
            Expect.sequenceEqual observed [0;4] "one update per batch"
            owner.Dispose ()
            source.Value <- 3
            Expect.sequenceEqual observed [0;4] "disposed owner stops effects")
    testCase "zoom preserves the anchored data point" <| fun _ ->
        let anchor = Point.create 20. 30.
        let zoom = Zoom.identity |> Zoom.zoomAt anchor 3.
        let actual = Zoom.apply anchor zoom
        Expect.equal actual anchor "anchor remains under pointer"
        Expect.equal (Zoom.invert (Point.create 50. 90.) zoom) (Point.create 30. 50.) "inverse"
        let bounds = Brush.between (Point.create 8. 9.) (Point.create 2. 3.)
        Expect.equal (Bounds.width bounds, Bounds.height bounds) (6.,6.) "normalized brush"
]

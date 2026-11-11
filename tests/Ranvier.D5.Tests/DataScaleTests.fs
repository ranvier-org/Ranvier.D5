module Ranvier.D5.Tests.DataScaleTests

open System
open Expecto
open Ranvier.D5

let near expected actual = Expect.floatClose Accuracy.high actual expected "numeric result"

let tests = testList "Data and scales" [
    testCase "finite summaries and compensated sum" <| fun _ ->
        Expect.equal (Data.extent [nan; infinity; -4.; 9.]) (Some (-4., 9.)) "omit missing values"
        Expect.equal (Data.extent []) None "empty extent"
        near 1. (Data.sum [1e16; 1.; -1e16])
        Expect.equal (Data.mean []) None "empty mean"
        near 4. (Data.mean [2.; nan; 6.] |> Option.get)
    testCase "quantile interpolates a sorted copy" <| fun _ ->
        near 7.5 (Data.quantile 0.75 [10.; 0.; nan] |> Option.get)
        Expect.equal (Data.quantile 0.5 []) None "empty quantile"
        Expect.throws (fun () -> Data.quantile 1.1 [1.] |> ignore) "invalid probability"
    testCase "grouping keeps source order" <| fun _ ->
        Expect.equal (Data.groupBy fst ["b",1; "a",2; "b",3]) (Map.ofList ["a",["a",2]; "b",["b",1;"b",3]]) "stable groups"
    testCase "bin boundaries and inclusive final edge" <| fun _ ->
        let bins = Data.bin id [0.; 10.; 20.] [-1.;0.;9.;10.;20.;21.;nan]
        Expect.equal (bins |> List.map _.Values) [[0.;9.];[10.;20.]] "left closed, final right closed"
        Expect.throws (fun () -> Data.bin id [0.;0.] [] |> ignore) "strict boundaries"
    testCase "join reports enter update exit in target order" <| fun _ ->
        let joined = Data.join fst ["a",1;"b",2] ["b",3;"c",4]
        Expect.equal (joined.Enter |> List.map (fun (k,_,_) -> k)) ["c"] "enter"
        Expect.equal (joined.Update |> List.map (fun (k,oldIndex,newIndex,_,_) -> k,oldIndex,newIndex)) ["b",1,0] "retained reorder"
        Expect.equal (joined.Exit |> List.map (fun (k,_,_) -> k)) ["a"] "exit"
        Expect.throws (fun () -> Data.join id [1;1] [] |> ignore) "duplicate source"
        Expect.throws (fun () -> Data.join id [] [1;1] |> ignore) "duplicate target"
    testCase "linear maps inverts clamps and handles constant domain" <| fun _ ->
        let scale = Scale.linear (0., 10.) (100., 0.)
        near 75. (Scale.map scale 2.5)
        near 2.5 (Scale.invert scale 75.)
        near -100. (Scale.map scale 20.)
        near 0. (Scale.map (Scale.clamp true scale) 20.)
        near 50. (Scale.map (Scale.linear (1.,1.) (0.,100.)) 1.)
        Expect.equal (Scale.tryMap scale nan) None "missing numeric observation"
        Expect.throws (fun () -> Scale.linear (0., infinity) (0.,1.) |> ignore) "invalid domain"
    testCase "linear roundtrip property over reversed domains and ranges" <| fun _ ->
        for a,b in [(-100.,100.); (20.,-30.); (0.001,0.009)] do
            for c,d in [(0.,1000.); (640.,0.)] do
                let scale = Scale.linear (a,b) (c,d)
                for i in 0..100 do
                    let x = a + (b-a) * float i / 100.
                    near x (Scale.map scale x |> Scale.invert scale)
    testCase "nice numeric ticks including descending and tiny intervals" <| fun _ ->
        Expect.equal (Scale.ticks 5 (Scale.linear (0.,10.) (0.,1.))) [0.;2.;4.;6.;8.;10.] "nice ticks"
        Expect.equal (Scale.ticks 5 (Scale.linear (10.,0.) (0.,1.))) [10.;8.;6.;4.;2.;0.] "descending ticks"
        Expect.equal (Scale.ticks 5 (Scale.linear (2.,2.) (0.,1.))) [2.] "constant tick"
        Expect.equal (Scale.ticks 0 (Scale.linear (0.,10.) (0.,1.))) [] "no ticks"
        Expect.isGreaterThan (Scale.ticks 5 (Scale.linear (1e-8,9e-8) (0.,1.)) |> List.length) 2 "small ticks"
    testCase "log power and symlog preserve inverse semantics" <| fun _ ->
        let log = Scale.log 10. (1.,100.) (0.,1.)
        near 0.5 (Scale.map log 10.)
        near 10. (Scale.invert log 0.5)
        Expect.equal (Scale.tryMap log 0.) None "log excludes zero"
        let negative = Scale.log 10. (-100.,-1.) (0.,1.)
        near -10. (Scale.invert negative 0.5)
        Expect.throws (fun () -> Scale.log 10. (-1.,1.) (0.,1.) |> ignore) "mixed sign log"
        near 0.25 (Scale.map (Scale.power 2. (0.,10.) (0.,1.)) 5.)
        let symmetric = Scale.symlog 1. (-100.,100.) (0.,1.)
        near 0.5 (Scale.map symmetric 0.)
        near -3. (Scale.map symmetric -3. |> Scale.invert symmetric)
    testCase "band point and ordinal are immutable and explicit on unknowns" <| fun _ ->
        let band = Scale.band ["a";"b"] (0.,100.) 0. 0.
        near 50. (Scale.bandwidth band)
        Expect.equal (Scale.bandPosition band "a") (Some 0.) "first band"
        Expect.equal (Scale.bandPosition band "x") None "unknown"
        let reverse = Scale.band ["a";"b"] (100.,0.) 0. 0.
        Expect.equal (Scale.bandPosition reverse "a") (Some 50.) "reversed band origin"
        Expect.equal (Scale.point ["a"] (0.,100.) 0. |> fun s -> Scale.bandPosition s "a") (Some 50.) "singleton point"
        let ordinal = Scale.ordinal ["a";"b";"c"] ["red";"blue"]
        Expect.equal (ordinal "c") (Some "red") "range cycles"
        Expect.equal (ordinal "other") None "domain does not mutate"
        Expect.throws (fun () -> Scale.band ["a";"a"] (0.,1.) 0. 0. |> ignore) "duplicate domain"
    testCase "threshold quantize and quantile boundaries" <| fun _ ->
        let threshold = Scale.threshold [10.;20.] ["low";"mid";"high"]
        Expect.equal (threshold 10.) (Some "mid") "threshold right bin"
        Expect.equal (threshold nan) None "NaN missing"
        Expect.equal (Scale.quantize (0.,100.) [0;1] 100.) (Some 1) "max clamps"
        Expect.equal (Scale.quantile [0.;10.;20.;30.] [0;1] 15.) (Some 1) "median boundary"
    testCase "UTC time scale handles leap day and ordered ticks" <| fun _ ->
        let first = DateTimeOffset(2024,2,28,0,0,0,TimeSpan.Zero)
        let last = first.AddDays 2.
        let scale = Scale.utcTime (first,last) (0.,200.)
        near 100. (Scale.timeMap scale (first.AddDays 1.))
        Expect.equal (Scale.timeInvert scale 100.) (first.AddDays 1.) "time inverse"
        Expect.equal (Scale.timeTicks TimeInterval.Day scale) [first;first.AddDays 1.;last] "leap day"
    testCase "sequential and diverging accept a reusable interpolator" <| fun _ ->
        let seq = Scale.sequential (0.,10.) (fun t -> t*t)
        near 0.25 (seq 5. |> Option.get)
        let div = Scale.diverging (-10.,0.,30.) id
        near 0.25 (div -5. |> Option.get)
        near 0.75 (div 15. |> Option.get)
]

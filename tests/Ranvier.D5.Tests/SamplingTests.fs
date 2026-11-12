module Ranvier.D5.Tests.SamplingTests

open System
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private sample id x y = {SeriesSample.Id=id;X=x;Y=y}
let private values ys = ys |> List.mapi(fun i y -> sample (string i) (float i) (Some y))
let private ids (samples:SeriesSample list) = samples |> List.map(fun sample -> sample.Id)

let tests=testList "Sampling" [
    testCase "minmax preserves ordered bucket extrema and endpoints" <| fun _ ->
        let input=values [0.;1.;-10.;1.;10.;2.;-2.;5.;-1.;0.]
        let output=Sampling.minMax (SamplingConfig.create 6) input |> ok
        Expect.equal (ids output) ["0";"2";"4";"6";"7";"9"] "both extrema per interior index bucket"
        Expect.isTrue (output |> List.forall(fun item -> List.contains item input)) "original records retained"
    testCase "LTTB preserves a spike and deterministic plateau ties" <| fun _ ->
        let input=values [for i in 0..10 -> if i=5 then 20. else 0.]
        Expect.equal (Sampling.lttb (SamplingConfig.create 3) input |> ok |> ids) ["0";"5";"10"] "largest triangle"
        let plateau=values(List.replicate 11 1.)
        Expect.equal (Sampling.lttb (SamplingConfig.create 5) plateau |> ok |> ids) ["0";"1";"4";"7";"10"] "earliest index wins ties"
        Expect.equal (Sampling.lttb (SamplingConfig.create 2) input |> ok |> ids) ["0";"10"] "two-point reduction retains endpoints"
    testCase "reduction preserves gap markers and every run endpoint" <| fun _ ->
        let input=[for i in 0..4 -> sample ("a"+string i) (float i) (Some(float i))]
                  @ [sample "gap1" 5. None;sample "gap2" 6. None]
                  @ [for i in 0..4 -> sample ("b"+string i) (float(i+7)) (Some(float(-i)))]
        for reduce,target in [Sampling.minMax,9;Sampling.lttb,7] do
            let result=reduce (SamplingConfig.create target) input |> ok
            Expect.isLessThanOrEqual result.Length target "global visible budget"
            for id in ["a0";"a4";"gap1";"b0";"b4"] do Expect.contains (ids result) id "mandatory boundary"
            Expect.equal (result |> List.filter(fun item -> item.Y.IsNone) |> ids) ["gap1"] "one original marker per missing run"
            Expect.isError (reduce (SamplingConfig.create 3) input) "budget cannot erase mandatory gaps/endpoints"
    testCase "small empty and allmissing input are explicit" <| fun _ ->
        for reduce in [Sampling.minMax;Sampling.lttb] do
            Expect.equal (reduce (SamplingConfig.create 0) [] |> ok) [] "empty"
            let input=values [1.;2.;3.]
            Expect.equal (reduce (SamplingConfig.create 20) input |> ok) input "no reduction leaves original sequence"
            let missing=[for i in 0..5 -> sample (string i) (float i) None]
            Expect.equal (reduce (SamplingConfig.create 1) missing |> ok |> ids) ["0"] "one retained missing marker"
    testCase "invalid identities ordering values and budgets are rejected" <| fun _ ->
        for reduce in [Sampling.minMax;Sampling.lttb] do
            for input in [[sample "a" 1. (Some 1.);sample "a" 2. (Some 2.)];[sample "a" 2. (Some 1.);sample "b" 1. (Some 2.)];[sample "a" nan None];[sample "a" 0. (Some infinity)];[sample "" 0. (Some 1.)]] do
                Expect.isError (reduce (SamplingConfig.create 10) input) "invalid input"
            Expect.isError (reduce {SamplingConfig.create 3 with MaxInput=1} (values [1.;2.])) "input budget"
        Expect.isError (Sampling.minMax (SamplingConfig.create 3) (values [0.;-10.;10.;1.;0.])) "cannot promise both extrema with only one interior slot"
    testCase "equal X and extreme finite geometry retain original records" <| fun _ ->
        let repeated=[for i in 0..9 -> sample (string i) 1. (Some(float(i%3)))]
        for reduce in [Sampling.minMax;Sampling.lttb] do
            let result=reduce (SamplingConfig.create 4) repeated |> ok
            Expect.equal result.Head repeated.Head "first"
            Expect.equal (List.last result) (List.last repeated) "last"
        let extreme=[sample "a" -1e308 (Some -1e308);sample "b" 0. (Some 1e308);sample "c" 1e308 (Some -1e308)]
        Expect.equal (Sampling.lttb (SamplingConfig.create 3) extreme |> ok) extreme "finite extremes without reduction"
        let wide=[sample "a" -1e308 (Some 0.);sample "b" -5e307 (Some 1e308);sample "c" 0. (Some 0.);sample "d" 5e307 (Some -1e308);sample "e" 1e308 (Some 0.)]
        let result=Sampling.lttb (SamplingConfig.create 3) wide |> ok
        Expect.equal (ids result) ["a";"b";"e"] "overflow-safe triangle comparison and earliest tie"
    testCase "tiny triangle areas and generated reductions stay bounded" <| fun _ ->
        let tiny=[sample "a" 0. (Some 0.);sample "b" 1e-300 (Some 1e-300);sample "c" 2e-300 (Some 0.);sample "d" 3e-300 (Some -1e-300);sample "e" 4e-300 (Some 0.)]
        Expect.equal (Sampling.lttb (SamplingConfig.create 3) tiny |> ok |> ids) ["a";"b";"e"] "underflow-safe triangle ranking"
        for length in [5;17;100;257] do
            let input=values [for i in 0..length-1 -> sin(float(i*17))]
            for target in [4;7;20] do
                for reduce in [Sampling.minMax;Sampling.lttb] do
                    let result=reduce (SamplingConfig.create target) input |> ok
                    Expect.isLessThanOrEqual result.Length target "visible budget"
                    Expect.equal result.Head input.Head "first sample"
                    Expect.equal (List.last result) (List.last input) "last sample"
                    Expect.isTrue (result |> List.pairwise |> List.forall(fun (a,b) -> a.X<b.X)) "original order without duplicate picks"
    testCase "sampling checks cancellation while validating a large request" <| fun _ ->
        let input=values [for i in 0..199999 -> float(i%101)]
        for reduce in [Sampling.minMaxWithCancellation;Sampling.lttbWithCancellation] do
            use cancel=new Threading.CancellationTokenSource()
            let elapsed=Diagnostics.Stopwatch.StartNew()
            cancel.CancelAfter 1
            Expect.throwsT<OperationCanceledException> (fun () -> reduce cancel.Token (SamplingConfig.create 1000) input |> ignore) "mid-request cancellation"
            Expect.isLessThan elapsed.Elapsed.TotalSeconds 2. "cooperative cancellation bound"
    testCase "sampling observes cancellation" <| fun _ ->
        use cancel=new Threading.CancellationTokenSource()
        cancel.Cancel()
        for reduce in [Sampling.minMaxWithCancellation;Sampling.lttbWithCancellation] do
            Expect.throwsT<OperationCanceledException> (fun () -> reduce cancel.Token (SamplingConfig.create 10) [] |> ignore) "pre-cancellation"
]

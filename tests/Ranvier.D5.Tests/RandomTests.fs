module Ranvier.D5.Tests.RandomTests

open System
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private draws count distribution = Distribution.samples count (ok distribution) (RandomState.create 20261113UL) |> ok |> fst
let private mean (values:float array) = Array.average values
let private variance (values:float array) = let m=mean values in values |> Array.averageBy(fun value -> (value-m)*(value-m))
/// Seeded draws are deterministic, so a six-standard-error band cannot flake; it still catches biased samplers.
let private expectMoments name expectedMean expectedVariance (values:float array) =
    let n=float values.Length
    Expect.isLessThan (abs(mean values-expectedMean)) (6.*sqrt(expectedVariance/n)+1e-12) (name+" mean")
    Expect.isLessThan (abs(variance values/expectedVariance-1.)) 0.03 (name+" variance")

[<Tests>]
let tests = testList "random distributions" [
    testCase "xoshiro256** with splitmix64 seeding matches the reference algorithm" <| fun _ ->
        let mutable state=RandomState.create 42UL
        for expected in [1546998764402558742UL;6990951692964543102UL;12544586762248559009UL] do
            let struct(value,next)=RandomState.nextUInt64 state
            Expect.equal value expected "reference output"
            state<-next
    testCase "states are immutable, replayable and split into independent streams" <| fun _ ->
        let start=RandomState.create 7UL
        let struct(first,_)=RandomState.nextFloat start
        let struct(again,_)=RandomState.nextFloat start
        Expect.equal first again "an earlier state replays"
        let struct(child,parent)=RandomState.split start
        let childValues=Distribution.samples 32 (Distribution.uniform 0. 1. |> ok) child |> ok |> fst
        let parentValues=Distribution.samples 32 (Distribution.uniform 0. 1. |> ok) parent |> ok |> fst
        Expect.notEqual childValues parentValues "child stream differs"
        for value in childValues do Expect.isTrue (value>=0. && value<1.) "unit interval"
    testCase "continuous distributions have their analytic moments" <| fun _ ->
        let n=200_000
        expectMoments "uniform" 2.5 (9./12.) (draws n (Distribution.uniform 1. 4.))
        expectMoments "normal" -3. 4. (draws n (Distribution.normal -3. 2.))
        expectMoments "log-normal" (exp 0.125) ((exp 0.25-1.)*exp 0.25) (draws n (Distribution.logNormal 0. 0.5))
        expectMoments "exponential" 0.5 0.25 (draws n (Distribution.exponential 2.))
        expectMoments "gamma k>1" 6. 12. (draws n (Distribution.gamma 3. 2.))
        expectMoments "gamma k<1" 0.6 0.9 (draws n (Distribution.gamma 0.4 1.5))
        expectMoments "beta" (2./7.) (10./(49.*8.)) (draws n (Distribution.beta 2. 5.))
        expectMoments "logistic" 1. (Math.PI*Math.PI/3.*4.) (draws n (Distribution.logistic 1. 2.))
        expectMoments "Irwin-Hall" 3. 0.5 (draws n (Distribution.irwinHall 6))
        expectMoments "Bates" 0.5 (1./48.) (draws n (Distribution.bates 4))
        expectMoments "Weibull" (2.+3.*0.886226925452758) (9.*(1.-0.785398163397448)) (draws n (Distribution.weibull 2. 2. 3.))
    testCase "discrete distributions are integral with their analytic moments" <| fun _ ->
        let n=200_000
        for name,distribution,expectedMean,expectedVariance in [
            "Poisson small",Distribution.poisson 3.5,3.5,3.5
            "Poisson large",Distribution.poisson 250.,250.,250.
            "binomial small",Distribution.binomial 20 0.3,6.,4.2
            "binomial large",Distribution.binomial 100000 0.4,40000.,24000.
            "geometric",Distribution.geometric 0.25,4.,12.
            "Bernoulli",Distribution.bernoulli 0.3,0.3,0.21] do
            let values=draws n distribution
            Expect.isTrue (values |> Array.forall(fun value -> value=floor value && value>=0.)) (name+" nonnegative integers")
            expectMoments name expectedMean expectedVariance values
        let integers=draws 10000 (Distribution.integer 3. 7.)
        Expect.equal (integers |> Array.distinct |> Array.sort) [|3.;4.;5.;6.|] "integer range is half open"
    testCase "heavy-tailed distributions keep their medians and minimums" <| fun _ ->
        let median (values:float array)=let sorted=Array.sort values in sorted[sorted.Length/2]
        let cauchy=draws 100_001 (Distribution.cauchy 5. 2.)
        Expect.isLessThan (abs(median cauchy-5.)) 0.05 "Cauchy median"
        let pareto=draws 100_001 (Distribution.pareto 3.)
        Expect.isGreaterThanOrEqual (Array.min pareto) 1. "Pareto minimum"
        Expect.isLessThan (abs(median pareto-Math.Pow(2.,1./3.))) 0.01 "Pareto median"
    testCase "degenerate parameters stay finite and exact" <| fun _ ->
        Expect.allEqual (draws 100 (Distribution.normal 4. 0.)) 4. "zero deviation"
        Expect.allEqual (draws 100 (Distribution.poisson 0.)) 0. "zero mean"
        Expect.allEqual (draws 100 (Distribution.binomial 9 1.)) 9. "certain success"
        Expect.allEqual (draws 100 (Distribution.binomial 9 0.)) 0. "certain failure"
        Expect.allEqual (draws 100 (Distribution.geometric 1.)) 1. "first trial succeeds"
        Expect.allEqual (draws 100 (Distribution.irwinHall 0)) 0. "empty sum"
    testCase "invalid parameters are rejected when a distribution is created" <| fun _ ->
        for invalid in [Distribution.uniform 2. 1.;Distribution.integer 1. 1.;Distribution.normal 0. -1.;Distribution.normal nan 1.
                        Distribution.exponential 0.;Distribution.gamma 0. 1.;Distribution.beta 1. -1.;Distribution.poisson -1.
                        Distribution.binomial -1 0.5;Distribution.binomial 5 1.5;Distribution.geometric 0.;Distribution.bernoulli 2.
                        Distribution.pareto 0.;Distribution.logistic 0. 0.;Distribution.weibull 1. 0. 0.;Distribution.cauchy 0. -1.
                        Distribution.irwinHall -1;Distribution.bates 0] do
            Expect.isError invalid "invalid parameters"
        Expect.isError (Distribution.samples -1 (Distribution.uniform 0. 1. |> ok) (RandomState.create 1UL)) "negative count"
]

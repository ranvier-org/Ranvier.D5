module Ranvier.D5.Tests.DensityTests

open System
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private config=DensityConfig.create (Bounds.create 0. 0. 4. 4.) 4 4
let private point id x y weight : WeightedPoint = {Id=id;Position=Point.create x y;Weight=weight}
let private total grid=DensityGrid.values grid |> List.sumBy(Option.defaultValue 0.)

let tests=testList "Density" [
    testCase "grid distinguishes missing and measured zero and enforces dimensions" <| fun _ ->
        let grid=DensityGrid.create config.Bounds 2 2 [Some 0.;None;Some 2.;Some -1.] |> ok
        Expect.equal (DensityGrid.value 0 0 grid) (Some 0.) "measured zero"
        Expect.equal (DensityGrid.value 1 0 grid) None "missing"
        Expect.equal (DensityGrid.values grid) [Some 0.;None;Some 2.;Some -1.] "row major"
        Expect.isError (DensityGrid.create config.Bounds 2 2 [Some 1.]) "dimensions"
        Expect.isError (DensityGrid.create config.Bounds 1 1 [Some nan]) "nonfinite"
        Expect.isError (DensityGrid.createWithBudget 3 config.Bounds 2 2 [None;None;None;None]) "cell budget"
    testCase "aggregation includes upper boundary and retains original cell membership" <| fun _ ->
        let grid=Density.grid config [point "a" 0. 0. 2.;point "b" 4. 4. 3.;point "c" 0.5 0.5 1.] |> ok
        Expect.equal (DensityGrid.value 0 0 grid) (Some 3.) "origin cell"
        Expect.equal (DensityGrid.value 3 3 grid) (Some 3.) "upper edge"
        Expect.equal (DensityGrid.members 0 0 grid |> Set.ofList) (set ["a";"c"]) "aggregate identities"
        Expect.equal (DensityGrid.value 2 2 grid) (Some 0.) "observed empty cell"
        Expect.equal (total grid) 6. "mass conservation"
    testCase "invalid observations and excessive kernel work are explicit errors" <| fun _ ->
        for points in [[point "a" -1. 0. 1.];[point "a" 0. 0. -1.];[point "a" 0. 0. infinity];[point "a" 0. 0. 1.;point "a" 1. 1. 1.]] do
            Expect.isError (Density.grid config points) "invalid data"
        Expect.isError (Density.grid {config with MaxCells=3} []) "cell budget"
        Expect.isError (Density.grid {config with Bandwidth=100.;MaxKernelRadius=1} [point "a" 1. 1. 1.]) "kernel radius budget"
    testCase "blur rejects excessive total work before allocating kernels" <| fun _ ->
        let grid=DensityGrid.create (Bounds.create 0. 0. 256. 256.) 256 256 (List.replicate (256*256) (Some 1.)) |> ok
        Expect.isError (Density.blur 200. DensityBoundary.Renormalize 1000 grid) "radius alone does not bound grid work"
        let one=DensityGrid.create (Bounds.create 0. 0. 1. 1.) 1 1 [Some 1.] |> ok
        Expect.isError (Density.blur 1e9 DensityBoundary.Drop Int32.MaxValue one) "kernel array length cannot overflow"
    testCase "Gaussian blur conserves corner mass when renormalized and drops it explicitly otherwise" <| fun _ ->
        let grid=Density.grid config [point "a" 0. 0. 7.] |> ok
        let preserved=Density.blur 0.7 DensityBoundary.Renormalize 10 grid |> ok
        let dropped=Density.blur 0.7 DensityBoundary.Drop 10 grid |> ok
        Expect.floatClose Accuracy.high (total preserved) 7. "normalized boundary conservation"
        Expect.isLessThan (total dropped) 7. "declared edge loss"
        Expect.isGreaterThan (DensityGrid.value 1 0 preserved |> Option.get) 0. "neighbor receives mass"
        Expect.floatClose Accuracy.high (DensityGrid.value 1 0 preserved |> Option.get) (DensityGrid.value 0 1 preserved |> Option.get) "axis symmetry"
    testCase "blur preserves missing cells and allmissing input" <| fun _ ->
        let masked=DensityGrid.create (Bounds.create 0. 0. 3. 1.) 3 1 [Some 5.;None;Some 0.] |> ok
        let result=Density.blur 1. DensityBoundary.Renormalize 10 masked |> ok
        Expect.equal (DensityGrid.value 1 0 result) None "mask retained"
        Expect.floatClose Accuracy.high (total result) 5. "mass is not deposited into missing cells"
        let missing=DensityGrid.create config.Bounds 2 2 [None;None;None;None] |> ok
        Expect.equal (Density.blur 1. DensityBoundary.Renormalize 10 missing |> ok |> DensityGrid.values) [None;None;None;None] "no fabricated zeroes"
    testCase "normalization reports mass per coordinate area and empty observations are zero" <| fun _ ->
        let cfg={config with Bounds=Bounds.create 0. 0. 8. 8.;Normalization=DensityNormalization.Density}
        let result=Density.grid cfg [point "a" 1. 1. 8.] |> ok
        Expect.equal (DensityGrid.value 0 0 result) (Some 2.) "cell area four"
        Expect.equal (Density.grid config [] |> ok |> total) 0. "empty input measured zero"
    testCase "one dimensional Gaussian KDE matches analytic values" <| fun _ ->
        let values=Density.kde1d 1. [0.,2.] [0.;1.;-1.] |> ok
        let peak=1./sqrt(2.*Math.PI)
        Expect.floatClose Accuracy.high values[0] peak "normalized peak"
        Expect.floatClose Accuracy.high values[1] (peak*exp -0.5) "unit offset"
        Expect.equal values[1] values[2] "symmetry"
        Expect.equal (Density.kde1d 1. [] [0.;1.] |> ok) [0.;0.] "empty density"
        Expect.isError (Density.kde1d 0. [0.,1.] [0.]) "bandwidth"
    testCase "extreme numeric ranges return finite mass or explicit density errors" <| fun _ ->
        let tiny={DensityConfig.create (Bounds.create 0. 0. Double.Epsilon Double.Epsilon) 1 1 with Normalization=DensityNormalization.Mass}
        Expect.equal (Density.grid tiny [point "tiny" 0. 0. 1.] |> ok |> total) 1. "mass does not require representable area"
        Expect.isError (Density.grid {tiny with Normalization=DensityNormalization.Density} [point "tiny" 0. 0. 1.]) "density requires positive area"
        Expect.isError (Density.grid config [point "a" 0. 0. 1e308;point "b" 0. 0. 1e308]) "cell total overflow"
        let wide=Density.kde1d 1e308 [-1e308,1.] [1e308] |> ok |> List.head
        let expected=exp(-2.)/sqrt(2.*Math.PI)/1e308
        Expect.isGreaterThan wide 0. "opposite extreme coordinates retain representable density"
        Expect.floatClose Accuracy.medium (wide/expected) 1. "overflow-safe standardized distance"
        Expect.isError (Density.kde1d Double.Epsilon [0.,1.] [0.]) "unrepresentable peak"
    testCase "KDE preserves representable density when probability weights underflow" <| fun _ ->
        let tail=Density.kde1d 1e-300 [0.,1.] [4e-299] |> ok |> List.head
        let expected=exp(-800.-log(1e-300)-0.5*log(2.*Math.PI))
        Expect.isGreaterThan tail 0. "tiny bandwidth restores a representable far-tail density"
        Expect.floatClose Accuracy.medium (tail/expected) 1. "log-domain tail"
        let light=Density.kde1d 1e-308 [1.,1e308;0.,1e-308] [0.] |> ok |> List.head
        let expectedLight=1e-308/sqrt(2.*Math.PI)
        Expect.isGreaterThan light 0. "small sample weight still contributes near a narrow kernel"
        Expect.floatClose Accuracy.medium (light/expectedLight) 1. "weight normalization avoids underflow"
    testCase "KDE integrates to one and blur retains independent original grid values" <| fun _ ->
        let positions=[for i in 0..1200 -> -6.+float i*0.01]
        let estimate=Density.kde1d 1. [0.,1.] positions |> ok
        Expect.isLessThan (abs(List.sum estimate*0.01-1.)) 1e-7 "Gaussian probability integral"
        let original=DensityGrid.create config.Bounds 4 4 [for i in 0..15 -> Some(float i)] |> ok
        let before=DensityGrid.values original
        let blurred=Density.blur 0.6 DensityBoundary.Renormalize 8 original |> ok
        Expect.equal (DensityGrid.values original) before "input remains immutable"
        Expect.floatClose Accuracy.high (total blurred) (total original) "constant/dense observed grid conserves mass"
    testCase "blur and KDE observe mid-run cancellation" <| fun _ ->
        let grid=DensityGrid.create (Bounds.create 0. 0. 512. 512.) 512 512 (List.replicate (512*512) (Some 1.)) |> ok
        use blurCancel=new Threading.CancellationTokenSource()
        blurCancel.CancelAfter 1
        Expect.throwsT<OperationCanceledException> (fun () -> Density.blurWithCancellation blurCancel.Token 16. DensityBoundary.Renormalize 64 grid |> ignore) "strip cancellation"
        use kdeCancel=new Threading.CancellationTokenSource()
        let samples=[for i in 0..9999 -> float i,1.]
        let queries=[for i in 0..9999 -> float i]
        let elapsed=Diagnostics.Stopwatch.StartNew()
        kdeCancel.CancelAfter 1
        Expect.throwsT<OperationCanceledException> (fun () -> Density.kde1dWithCancellation kdeCancel.Token 1. samples queries |> ignore) "query/inner-loop cancellation"
        Expect.isLessThan elapsed.Elapsed.TotalSeconds 2. "cooperative cancellation finishes within a generous non-real-time bound"
    testCase "pre-cancellation prevents grid and blur work" <| fun _ ->
        use cancel=new Threading.CancellationTokenSource()
        cancel.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> Density.gridWithCancellation cancel.Token config [] |> ignore) "grid cancellation"
        Expect.throwsT<OperationCanceledException> (fun () -> Density.kde1dWithCancellation cancel.Token 1. [0.,1.] [0.] |> ignore) "KDE cancellation"
        let grid=DensityGrid.create config.Bounds 1 1 [Some 1.] |> ok
        Expect.throwsT<OperationCanceledException> (fun () -> Density.blurWithCancellation cancel.Token 1. DensityBoundary.Renormalize 10 grid |> ignore) "blur cancellation"
]

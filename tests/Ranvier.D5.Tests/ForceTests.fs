module Ranvier.D5.Tests.ForceTests

open System
open System.Threading
open Expecto
open Ranvier.D5

let private p x y=Point.create x y
let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private config={ForceConfig.create(p 0. 0.) with AlphaDecay=0.;VelocityDecay=0.;CenterStrength=0.}
let private positioned id x y={GraphNode.create id with Position=Some(p x y)}
let private start cfg nodes links=GraphData.create nodes links |> ok |> Force.initialize cfg (Seed.create 42u) |> ok
let tests=testList "Force" [
    testCase "graph validation reports duplicate and unknown IDs" <| fun _ ->
        Expect.isError (GraphData.create [GraphNode.create "a";GraphNode.create "a"] []) "duplicate node"
        Expect.isError (GraphData.create [GraphNode.create "a"] [GraphLink.create "link" "a" "missing"]) "unknown endpoint"
        Expect.isError (GraphData.create [{GraphNode.create "a" with Mass=0.}] []) "positive mass"
        Expect.isError (GraphData.create [GraphNode.create "a"] [{GraphLink.create "l" "a" "a" with Strength=Double.NaN}]) "finite strength"
    testCase "seed and stable IDs determine reproducible immutable states" <| fun _ ->
        let nodes=[GraphNode.create "b";GraphNode.create "a";GraphNode.create "c"]
        let first=start config nodes []
        let before=Force.snapshot first
        let reversed=start config (List.rev nodes) []
        Expect.equal before (Force.snapshot reversed) "input permutation does not change seeded positions"
        let evolved=Force.step 3 first
        Expect.equal (Force.snapshot first) before "earlier state remains unchanged"
        Expect.equal (Force.snapshot evolved) (Force.step 3 reversed |> Force.snapshot) "reproducible evolution"
    testCase "axis radial and center forces integrate fixed steps" <| fun _ ->
        let axis=start {config with X=Some(10.,0.5);Y=Some(-4.,0.5)} [positioned "a" 0. 0.] [] |> Force.step 1 |> Force.snapshot
        Expect.equal axis.Nodes[0].Position (p 5. -2.) "unit step axis acceleration"
        let radial=start {config with Radial=Some(p 0. 0.,10.,0.5)} [positioned "a" 2. 0.] [] |> Force.step 1 |> Force.snapshot
        Expect.equal radial.Nodes[0].Position (p 6. 0.) "radial attraction"
        let center=start {config with CenterStrength=1.} [positioned "a" 10. 0.] [] |> Force.step 1 |> Force.snapshot
        Expect.equal center.Nodes[0].Position (p 0. 0.) "centering"
    testCase "links apply symmetric forces and pins override integration" <| fun _ ->
        let link={GraphLink.create "l" "a" "b" with Distance=10.;Strength=1.}
        let graph=[positioned "a" 0. 0.;positioned "b" 20. 0.]
        let output=start config graph [link] |> Force.step 1 |> Force.snapshot
        Expect.equal (output.Nodes |> List.map(fun n -> n.Position.X)) [5.;15.] "link split by mass"
        let pinned=start config [{graph[0] with Pinned=Some(p 0. 0.)};graph[1]] [link] |> Force.step 3 |> Force.snapshot
        Expect.equal pinned.Nodes[0].Position (p 0. 0.) "fixed position"
        Expect.equal pinned.Nodes[0].Velocity (p 0. 0.) "fixed velocity"
    testCase "cooling and iteration budget stop work and reheat restarts it" <| fun _ ->
        let cooled=start {config with AlphaDecay=0.5;AlphaMin=0.2} [positioned "a" 0. 0.] [] |> Force.step 100 |> Force.snapshot
        Expect.equal cooled.Iteration 3 "cooling stops below threshold"
        Expect.isTrue cooled.Converged "cooled"
        let stopped=start {config with MaxSteps=2} [positioned "a" 0. 0.] [] |> Force.step 100
        Expect.equal (Force.snapshot stopped).Iteration 2 "hard step budget"
        Expect.equal (Force.reheat 1. stopped |> Force.step 1 |> Force.snapshot).Iteration 1 "new run budget after reheat"
    testCase "topology changes preserve survivors and never resurrect exits" <| fun _ ->
        let initial=start {config with X=Some(10.,0.5)} [positioned "a" 0. 0.;GraphNode.create "b"] [] |> Force.step 1
        let previous=Force.snapshot initial
        let replaced=GraphData.create [GraphNode.create "a";GraphNode.create "c"] [] |> ok |> fun graph -> Force.updateGraph graph initial |> ok |> Force.snapshot
        Expect.equal (replaced.Nodes |> List.map(fun n -> n.Id)) ["a";"c"] "new topology only"
        Expect.equal replaced.Nodes[0].Position previous.Nodes[0].Position "surviving position retained"
        Expect.equal replaced.Nodes[0].Velocity previous.Nodes[0].Velocity "surviving velocity retained"
        let moved=Force.pin "a" (Some(p 50. 60.)) initial |> ok |> Force.snapshot
        Expect.equal moved.Nodes[0].Position (p 50. 60.) "drag pin"
        Expect.equal (Force.snapshot initial) previous "pin leaves previous state intact"
        Expect.isError (Force.pin "absent" None initial) "unknown pin ID"
    testCase "cancelled stepping cannot mutate its input" <| fun _ ->
        let state=start config [GraphNode.create "a"] []
        let before=Force.snapshot state
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> Force.stepWithCancellation cancelled.Token 100 state |> ignore) "cancelled"
        Expect.equal (Force.snapshot state) before "snapshot remains intact"
    testCase "disabled forces do not overflow on finite stationary extremes" <| fun _ ->
        let stationary={config with Center=p -1e308 0.;X=Some(-1e308,0.);Radial=Some(p -1e308 0.,1.,0.)}
        let state=start stationary [positioned "a" 1e308 0.;positioned "b" -1e308 0.] [{GraphLink.create "l" "a" "b" with Strength=0.}]
        Expect.equal (Force.step 1 state |> Force.snapshot).Nodes (Force.snapshot state).Nodes "zero forces leave finite state intact"
    testCase "empty topology remains finished after reheat" <| fun _ ->
        let empty=start config [] [] |> Force.reheat 1. |> Force.step 100 |> Force.snapshot
        Expect.isTrue empty.Finished "empty solver does not spin"
        Expect.equal empty.Iteration 0 "no empty ticks"
    testCase "coincident links separate deterministically and momentum is mass weighted" <| fun _ ->
        let link={GraphLink.create "l" "a" "b" with Distance=10.;Strength=1.}
        let initial=start config [{positioned "a" 0. 0. with Mass=3.};positioned "b" 0. 0.] [link]
        let result=Force.step 1 initial |> Force.snapshot
        let a,b=result.Nodes[0],result.Nodes[1]
        Expect.floatClose Accuracy.high (System.Double.Hypot(a.Position.X-b.Position.X,a.Position.Y-b.Position.Y)) 10. "deterministic separation"
        Expect.floatClose Accuracy.high (a.Velocity.X*3.+b.Velocity.X) 0. "x momentum"
        Expect.floatClose Accuracy.high (a.Velocity.Y*3.+b.Velocity.Y) 0. "y momentum"
    testCase "mid-run cancellation interrupts a long solve without publishing partial state" <| fun _ ->
        let nodes=[for i in 0..999 -> GraphNode.create(string i)]
        let state=start {config with MaxSteps=Int32.MaxValue;X=Some(1.,0.001)} nodes []
        let before=Force.snapshot state
        use cancelled=new CancellationTokenSource()
        cancelled.CancelAfter 20
        let clock=Diagnostics.Stopwatch.StartNew()
        Expect.throwsT<OperationCanceledException> (fun () -> Force.stepWithCancellation cancelled.Token Int32.MaxValue state |> ignore) "long solve interrupted"
        Expect.isLessThan clock.Elapsed.TotalSeconds 5. "bounded cancellation latency"
        Expect.equal (Force.snapshot state) before "no partially updated input"
    testCase "chunked stepping matches the same total fixed steps" <| fun _ ->
        let state=start {config with X=Some(1.,0.1);VelocityDecay=0.2} [GraphNode.create "a";GraphNode.create "b"] [GraphLink.create "l" "a" "b"]
        Expect.equal (Force.step 6 state |> Force.snapshot) (state |> Force.step 2 |> Force.step 4 |> Force.snapshot) "chunk boundaries do not change physics"
    testCase "nonfinite active-force results fail without changing the prior state" <| fun _ ->
        let state=start {config with X=Some(-1e308,1.)} [positioned "a" 1e308 0.] []
        let before=Force.snapshot state
        Expect.throwsT<ArgumentException> (fun () -> Force.step 1 state |> ignore) "overflow rejected"
        Expect.equal (Force.snapshot state) before "failed integration is isolated"
    testCase "invalid solver settings are rejected" <| fun _ ->
        let graph=GraphData.create [GraphNode.create "a"] [] |> ok
        for invalid in [{config with AlphaDecay= -1.};{config with VelocityDecay=2.};{config with AlphaMin=0.};{config with MaxSteps=0};{config with InitialRadius=Double.NaN}] do
            Expect.isError (Force.initialize invalid (Seed.create 0u) graph) "invalid config"
]

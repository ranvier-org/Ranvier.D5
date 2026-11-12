module Ranvier.D5.Tests.ForceSpatialTests

open System
open Expecto
open Ranvier.D5

let private p x y=Point.create x y
let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private config={ForceConfig.create(p 0. 0.) with AlphaDecay=0.;VelocityDecay=0.;CenterStrength=0.}
let private node id x y={GraphNode.create id with Position=Some(p x y);Radius=2.}
let private run config nodes=GraphData.create nodes [] |> ok |> Force.initialize config (Seed.create 1u) |> ok |> Force.step 1 |> Force.snapshot
let tests=testList "Spatial forces" [
    testCase "spatial force reuses traversal storage across node queries" <| fun _ ->
        let graph=GraphData.create [for i in 0..999 -> GraphNode.create(string i)]
                        [for i in 0..999 do for jump in [1;7] -> GraphLink.create (string i+"/"+string jump) (string i) (string((i+jump)%1000))] |> ok
        let cfg={ForceConfig.create(p 0. 0.) with ManyBody=Some {ManyBodyConfig.create -5. with Theta=0.5};Collision=Some(CollisionConfig.create 0.7)}
        let state=Force.initialize cfg (Seed.create 42u) graph |> ok
        for _ in 1..3 do Force.step 1 state |> ignore
        let before=GC.GetAllocatedBytesForCurrentThread()
        let next=Force.step 1 state
        let allocated=GC.GetAllocatedBytesForCurrentThread()-before
        Expect.equal (Force.snapshot next).Nodes.Length 1000 "Every node remains present"
        Expect.isLessThan allocated 1000000L "Do not allocate a fresh traversal stack for each node"

    testCase "exact pair forces follow source mass and conserve pair momentum" <| fun _ ->
        let body={ManyBodyConfig.create -10. with Theta=0.}
        let result=run {config with ManyBody=Some body} [{node "a" 0. 0. with Mass=3.};node "b" 10. 0.]
        Expect.equal (result.Nodes |> List.map(fun n -> n.Position.X)) [-1.;13.] "analytic pair result"
        Expect.equal (result.Nodes[0].Velocity.X*3.+result.Nodes[1].Velocity.X) 0. "mass-weighted momentum"
    testCase "Barnes Hut approximation meets declared relative error" <| fun _ ->
        let random=Random 92
        let nodes=[for i in 0..399 -> node (string i) (random.NextDouble()*500.) (random.NextDouble()*500.)]
        let exact=run {config with ManyBody=Some {ManyBodyConfig.create -5. with Theta=0.}} nodes
        let approx=run {config with ManyBody=Some {ManyBodyConfig.create -5. with Theta=0.5}} nodes
        let mutable error,norm=0.,0.
        for a,b in List.zip exact.Nodes approx.Nodes do
            error<-error+(a.Velocity.X-b.Velocity.X)**2.+(a.Velocity.Y-b.Velocity.Y)**2.
            norm<-norm+a.Velocity.X**2.+a.Velocity.Y**2.
        Expect.isLessThan (sqrt(error/norm)) 0.03 "theta0.5 relative RMS velocity error below3% on pinned distribution"
        Expect.isGreaterThan norm 1. "force is not accidentally disabled"
    testCase "cutoff cells crossing the radius do not include distant sources" <| fun _ ->
        let nodes=[node "target" 0. 0.;node "near" 9. 0.]@[for i in 1..30 -> node (string i) 11. 0.]
        let body={ManyBodyConfig.create -1. with MaxDistance=10.}
        let exact=run {config with ManyBody=Some {body with Theta=0.}} nodes
        let approx=run {config with ManyBody=Some {body with Theta=100.}} nodes
        let a=exact.Nodes |> List.find(fun n -> n.Id="target")
        let b=approx.Nodes |> List.find(fun n -> n.Id="target")
        Expect.floatClose Accuracy.high a.Velocity.X b.Velocity.X "cutoff exact traversal"
        Expect.floatClose Accuracy.high a.Velocity.Y b.Velocity.Y "cutoff exact traversal y"
    testCase "collision resolves pair overlap with mass weighting and fixed pins" <| fun _ ->
        let cfg={config with Collision=Some(CollisionConfig.create 1.)}
        let result=run cfg [{node "a" 0. 0. with Mass=3.};node "b" 1. 0.]
        Expect.equal (result.Nodes |> List.map(fun n -> n.Position.X)) [-0.75;3.25] "analytic collision correction"
        let fixedResult=run cfg [{node "a" 0. 0. with Pinned=Some(p 0. 0.)};node "b" 1. 0.]
        Expect.equal (fixedResult.Nodes |> List.map(fun n -> n.Position.X)) [0.;4.] "free node takes full correction against pin"
    testCase "coincident pairs separate deterministically without NaNs" <| fun _ ->
        let nodes=[node "a" 0. 0.;node "b" 0. 0.]
        let cfg={config with Collision=Some(CollisionConfig.create 1.)}
        let a,b=run cfg nodes,run cfg nodes
        Expect.equal a b "deterministic jitter"
        let x,y=a.Nodes[0].Position,a.Nodes[1].Position
        Expect.floatClose Accuracy.high (Double.Hypot(x.X-y.X,x.Y-y.Y)) 4. "radii sum"
        let body=run {config with ManyBody=Some(ManyBodyConfig.create -1.)} nodes
        Expect.isGreaterThan (Double.Hypot(body.Nodes[0].Velocity.X,body.Nodes[0].Velocity.Y)) 0. "coincident repulsion"
    testCase "spatial kernels cancel during large work without mutating input" <| fun _ ->
        let nodes=[for i in 0..19999 -> node (string i) (float(i%200)) (float(i/200))]
        let graph=GraphData.create nodes [] |> ok
        for cfg in [{config with ManyBody=Some {ManyBodyConfig.create -1. with Theta=0.}};
                    {config with ManyBody=Some(ManyBodyConfig.create -1.)};
                    {config with Collision=Some {CollisionConfig.create 1. with Iterations=1000}}] do
            let state=Force.initialize cfg (Seed.create 42u) graph |> ok
            let before=Force.snapshot state
            use cancellation=new System.Threading.CancellationTokenSource()
            let timer=System.Diagnostics.Stopwatch.StartNew()
            cancellation.CancelAfter 20
            Expect.throwsT<OperationCanceledException> (fun () -> Force.stepWithCancellation cancellation.Token 1000 state |> ignore) "inner loops observe cancellation"
            Expect.isLessThan timer.Elapsed.TotalSeconds 5. "bounded cooperative cancellation"
            Expect.equal (Force.snapshot state) before "input state remains immutable"
    testCase "disabled spatial forces preserve extreme finite positions" <| fun _ ->
        let nodes=[node "a" -1e308 0.;node "b" 1e308 0.]
        let result=run {config with ManyBody=Some(ManyBodyConfig.create 0.);Collision=Some(CollisionConfig.create 0.)} nodes
        Expect.equal (result.Nodes |> List.map(fun n -> n.Position.X)) [-1e308;1e308] "disabled kernels do no distance arithmetic"
    testCase "invalid spatial force settings are rejected" <| fun _ ->
        let graph=GraphData.create [node "a" 0. 0.] [] |> ok
        for cfg in [{config with ManyBody=Some {ManyBodyConfig.create -1. with Theta= -1.}};
                    {config with ManyBody=Some {ManyBodyConfig.create -1. with MinDistance=0.}};
                    {config with Collision=Some {CollisionConfig.create 1. with Iterations=0}};
                    {config with Collision=Some(CollisionConfig.create 2.)}] do
            Expect.isError (Force.initialize cfg (Seed.create 0u) graph) "invalid settings"
]

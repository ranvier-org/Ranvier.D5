module Ranvier.D5.Tests.HexbinTests

open System
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private config=HexbinConfig.create (Bounds.create 0. 0. 100. 100.) 10.
let private point id x y weight = {WeightedPoint.Id=id;Position=Point.create x y;Weight=weight}

let tests=testList "Hexbin" [
    testCase "pointy lattice aggregation retains weighted semantic membership" <| fun _ ->
        let bins=Hexbin.aggregate config [point "b" 1. 1. 2.;point "a" 0. 0. 3.;point "c" (sqrt 3.*10.) 0. 4.] |> ok
        Expect.equal bins.Length 2 "two lattice cells"
        let origin=bins |> List.find(fun bin -> bin.Key=struct(0L,0L))
        Expect.equal origin.Members ["a";"b"] "canonical original membership"
        Expect.equal origin.Weight 5. "aggregate mass"
        Expect.equal (bins |> List.sumBy(fun bin -> bin.Weight)) 9. "mass conservation"
        let adjacent=bins |> List.find(fun bin -> bin.Key=struct(1L,0L))
        Expect.floatClose Accuracy.high adjacent.Center.X (sqrt 3.*10.) "pointy horizontal spacing"
        Expect.equal adjacent.Center.Y 0. "same lattice row"
    testCase "permutations and edge ties are deterministic" <| fun _ ->
        let points=[point "c" 1. 1. 1e16;point "a" 2. 2. 1.;point "b" 3. 3. 1.;point "edge" (sqrt 3.*5.) 0. 1.]
        let forward=Hexbin.aggregate config points |> ok
        Expect.equal (Hexbin.aggregate config (List.rev points) |> ok) forward "sum and members use canonical ID order"
        Expect.equal (forward |> List.collect(fun bin -> bin.Members) |> List.sort) ["a";"b";"c";"edge"] "boundary observations are never lost"
    testCase "hexagon path closes and uses the configured circumradius" <| fun _ ->
        let bin=Hexbin.aggregate config [point "a" 0. 0. 1.] |> ok |> List.head
        let commands=Hexbin.path bin |> Path.commands
        Expect.equal commands.Length 7 "six vertices and close"
        Expect.equal (List.last commands) Close "closed polygon"
        for command in commands do
            match command with
            | MoveTo p | LineTo p -> Expect.floatClose Accuracy.high (sqrt(Point.distanceSquared p bin.Center)) 10. "circumradius"
            | _ -> ()
    testCase "invalid weights identities coordinates and budgets are explicit" <| fun _ ->
        for points in [[point "a" -1. 0. 1.];[point "a" 0. 0. -1.];[point "a" 0. 0. infinity];[point "a" 0. 0. 1.;point "a" 1. 1. 2.];[point "" 1. 1. 1.]] do
            Expect.isError (Hexbin.aggregate config points) "invalid data"
        for config in [{config with Radius=0.};{config with Radius=nan};{config with MaxPoints=0}] do
            Expect.isError (Hexbin.aggregate config []) "invalid configuration"
        Expect.isError (Hexbin.aggregate {config with MaxBins=1} [point "a" 0. 0. 1.;point "b" 90. 90. 1.]) "bin budget"
        Expect.isError (Hexbin.aggregate {config with MaxPoints=1} [point "a" 0. 0. 1.;point "b" 1. 1. 1.]) "point budget"
    testCase "numeric extremes return typed errors and empty input is valid" <| fun _ ->
        Expect.equal (Hexbin.aggregate config [] |> ok) [] "empty bins"
        Expect.isError (Hexbin.aggregate config [point "a" 0. 0. 1e308;point "b" 1. 1. 1e308]) "weight overflow"
        Expect.isError (Hexbin.aggregate {config with Radius=Double.Epsilon} [point "a" 1. 1. 1.]) "unrepresentable lattice coordinate"
    testCase "hexbin rejects collapsed hexagon geometry" <| fun _ ->
        let large=HexbinConfig.create (Bounds.create 1e300 1e300 1. 1.) 0.1
        Expect.isError (Hexbin.aggregate large [point "a" 1e300 1e300 1.]) "distinct corners must remain representable"
        Expect.isError (Hexbin.aggregate {config with Radius=Double.Epsilon} [point "a" 0. 0. 1.]) "subnormal corners must not collapse"
    testCase "generated bins cover each point within their circumradius" <| fun _ ->
        let points=[for i in 0..999 -> point (string i) (float((i*37)%1000)/10.) (float((i*97)%1000)/10.) (float(i%7))]
        let source=points |> List.map(fun p -> p.Id,p.Position) |> Map.ofList
        let bins=Hexbin.aggregate config points |> ok
        Expect.equal (bins |> List.sumBy(fun bin -> bin.Members.Length)) points.Length "every original ID retained once"
        for bin in bins do
            for id in bin.Members do
                Expect.isLessThanOrEqual (sqrt(Point.distanceSquared source[id] bin.Center)) (config.Radius*(1.+1e-12)) "nearest lattice cell"
        Expect.floatClose Accuracy.high (bins |> List.sumBy(fun bin -> bin.Weight)) (points |> List.sumBy(fun p -> p.Weight)) "generated mass conservation"
    testCase "hexbin checks cancellation during a large request" <| fun _ ->
        let input=[for i in 0..199999 -> point (string i) (float(i%100)) (float((i/100)%100)) 1.]
        use cancel=new Threading.CancellationTokenSource()
        let elapsed=Diagnostics.Stopwatch.StartNew()
        cancel.CancelAfter 1
        Expect.throwsT<OperationCanceledException> (fun () -> Hexbin.aggregateWithCancellation cancel.Token config input |> ignore) "mid-request cancellation"
        Expect.isLessThan elapsed.Elapsed.TotalSeconds 2. "cooperative cancellation bound"
    testCase "hexbin observes cancellation" <| fun _ ->
        use cancel=new Threading.CancellationTokenSource()
        cancel.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> Hexbin.aggregateWithCancellation cancel.Token config [] |> ignore) "pre-cancellation"
]

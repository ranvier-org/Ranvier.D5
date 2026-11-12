module Ranvier.D5.Tests.SankeyTests

open System
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private link id source target value : FlowLink = {Id=id;Source=source;Target=target;Value=value}
let private config = {SankeyConfig.create(Bounds.create 10. 20. 400. 240.) with NodeWidth=20.;NodePadding=10.}
let private graph () = FlowGraph.create ["a";"b";"c";"d"] [link "ab" "a" "b" 3.;link "ac" "a" "c" 2.;link "bd" "b" "d" 2.;link "cd" "c" "d" 2.] |> ok

let tests = testList "Sankey" [
    testCase "pinned D3 initial layouts agree across all four alignments" <| fun _ ->
        use document=Text.Json.JsonDocument.Parse(IO.File.ReadAllText(IO.Path.Combine(AppContext.BaseDirectory,"fixtures","sankey.json")))
        for sample in document.RootElement.EnumerateArray() do
            let input=sample.GetProperty("input")
            let nodes=[for node in input.GetProperty("nodes").EnumerateArray() -> node.GetString()]
            let edges=[for edge in input.GetProperty("links").EnumerateArray() -> link (edge[0].GetString()) (edge[1].GetString()) (edge[2].GetString()) (edge[3].GetDouble())]
            let alignment=
                match sample.GetProperty("alignment").GetString() with
                | "Left" -> SankeyAlignment.Left | "Right" -> SankeyAlignment.Right
                | "Center" -> SankeyAlignment.Center | _ -> SankeyAlignment.Justify
            let result=FlowGraph.create nodes edges |> ok |> Sankey.layout {config with Alignment=alignment;Iterations=0} |> ok
            let actualNodes=result.Nodes |> List.map(fun node -> node.Id,node) |> Map.ofList
            for expected in sample.GetProperty("nodes").EnumerateArray() do
                let actual=actualNodes[expected.GetProperty("id").GetString()]
                let number (name:string)=expected.GetProperty(name).GetDouble()
                Expect.equal actual.Layer (expected.GetProperty("layer").GetInt32()) "layer"
                for name,value in ["value",actual.Value;"x0",Bounds.x actual.Bounds;"x1",Bounds.x actual.Bounds+Bounds.width actual.Bounds;"y0",Bounds.y actual.Bounds;"y1",Bounds.y actual.Bounds+Bounds.height actual.Bounds] do
                    Expect.floatClose Accuracy.high value (number name) name
            let actualEdges=result.Links |> List.map(fun edge -> edge.Id,edge) |> Map.ofList
            for expected in sample.GetProperty("links").EnumerateArray() do
                let actual=actualEdges[expected.GetProperty("id").GetString()]
                Expect.floatClose Accuracy.high actual.Width (expected.GetProperty("width").GetDouble()) "link width"

    testCase "graph validates IDs endpoints and values without losing parallel edges" <| fun _ ->
        let result=FlowGraph.create ["b";"a"] [link "second" "a" "b" 2.;link "first" "a" "b" 1.] |> ok
        Expect.equal (FlowGraph.nodes result) ["a";"b"] "ordinal nodes"
        Expect.equal (FlowGraph.links result |> List.map(fun edge -> edge.Id)) ["first";"second"] "parallel edges retained"
        for nodes,edges in [(["a";"a"],[]);([" "],[]);(["a"],[link "x" "a" "b" 1.]);(["a"],[link "x" "a" "a" -1.]);(["a"],[link "x" "a" "a" nan]);(["a"],[link "x" "a" "a" infinity]);(["a"],[link "x" "a" "a" 1.;link "x" "a" "a" 2.])] do
            Expect.isError (FlowGraph.create nodes edges) "invalid graph"
    testCase "DAG layers preserve explicit imbalance and stack links inside nodes" <| fun _ ->
        let result=Sankey.layout config (graph()) |> ok
        let nodes=result.Nodes |> List.map(fun node -> node.Id,node) |> Map.ofList
        Expect.equal (result.Nodes |> List.map(fun node -> node.Layer)) [0;1;1;2] "longest path layers"
        Expect.equal (nodes["b"].Inflow,nodes["b"].Outflow,nodes["b"].Value,nodes["b"].Imbalance) (3.,2.,3.,-1.) "imbalance is out minus in"
        let scale=Bounds.height nodes["a"].Bounds/5.
        for edge in result.Links do
            Expect.floatClose Accuracy.high edge.Width (edge.Value*scale) "one shared scale"
            for point,id in [edge.Start,edge.Source;edge.Finish,edge.Target] do
                let bounds=nodes[id].Bounds
                Expect.isTrue (point.Y-edge.Width/2.>=Bounds.y bounds-1e-9 && point.Y+edge.Width/2.<=Bounds.y bounds+Bounds.height bounds+1e-9) "attachments inside node"
        for _,column in result.Nodes |> List.groupBy(fun node -> node.Layer) do
            for a,b in column |> List.sortBy(fun node -> Bounds.y node.Bounds) |> List.pairwise do
                Expect.isTrue (Bounds.y a.Bounds+Bounds.height a.Bounds+config.NodePadding<=Bounds.y b.Bounds+1e-9) "no overlap after relaxation"
    testCase "cycles including zero links return typed topology errors" <| fun _ ->
        for edges in [[link "self" "a" "a" 0.];[link "ab" "a" "b" 1.;link "ba" "b" "a" 0.]] do
            let result=FlowGraph.create ["a";"b"] edges |> ok |> Sankey.layout config
            match result with Error errors -> Expect.isTrue (errors |> List.exists(function InvalidTopology _ -> true | _ -> false)) "typed cycle" | Ok _ -> failtest "cycle accepted"
    testCase "empty disconnected and zero flow graphs remain finite" <| fun _ ->
        let empty=FlowGraph.create [] [] |> ok |> Sankey.layout config |> ok
        Expect.isEmpty empty.Nodes "empty"
        let result=FlowGraph.create ["a";"b";"c"] [link "zero" "a" "b" 0.] |> ok |> Sankey.layout config |> ok
        Expect.equal result.Nodes.Length 3 "retain isolated node"
        Expect.equal result.Links.Length 1 "retain zero edge identity"
        for node in result.Nodes do
            Expect.equal node.Value 0. "no invented flow"
            Expect.equal (Bounds.height node.Bounds) 0. "zero height"
            Expect.isTrue (Double.IsFinite(Bounds.y node.Bounds)) "finite"
    testCase "input permutation is deterministic and parallel links are distinct" <| fun _ ->
        let nodes=["a";"b";"c"]
        let edges=[link "p" "a" "b" 1.;link "q" "a" "b" 2.;link "r" "b" "c" 3.]
        let run nodes edges=FlowGraph.create nodes edges |> ok |> Sankey.layout config |> ok
        let first=run nodes edges
        Expect.equal first (run (List.rev nodes) (List.rev edges)) "input order independent"
        Expect.notEqual first.Links[0].Start first.Links[1].Start "parallel attachment slots"
    testCase "infeasible viewport padding and budgets are rejected" <| fun _ ->
        let graph=graph()
        for invalid in [{config with NodeWidth=401.};{config with NodeWidth=210.};{config with NodePadding=241.};{config with NodePadding=nan};{config with Iterations= -1};{config with MaxNodes=3};{config with MaxLinks=3}] do
            Expect.isError (Sankey.layout invalid graph) "invalid/infeasible config"
    testCase "positive flows require height after requested padding" <| fun _ ->
        Expect.isError (Sankey.layout {config with Bounds=Bounds.create 0. 0. 400. 0.} (graph())) "zero-height viewport"
        Expect.isError (Sankey.layout {config with NodePadding=240.} (graph())) "padding consumes positive-flow height"
    testCase "subnormal and large totals have finite geometry or explicit errors" <| fun _ ->
        for value in [Double.Epsilon;1e308] do
            let result=FlowGraph.create ["a";"b"] [link "ab" "a" "b" value] |> ok |> Sankey.layout config |> ok
            Expect.floatClose Accuracy.high result.Links.Head.Width 240. "normalize before scaling"
        let excessive=FlowGraph.create ["a";"b"] [link "ab1" "a" "b" 1e308;link "ab2" "a" "b" 1e308] |> ok
        Expect.isError (Sankey.layout config excessive) "public totals overflow rejected"
    testCase "relaxation reduces crossed-link vertical distance without changing widths" <| fun _ ->
        let input=FlowGraph.create ["a";"b";"c";"d"] [link "ad" "a" "d" 3.;link "bc" "b" "c" 3.] |> ok
        let before=Sankey.layout {config with Iterations=0} input |> ok
        let after=Sankey.layout config input |> ok
        let distance result=result.Links |> List.sumBy(fun edge -> abs(edge.Start.Y-edge.Finish.Y))
        Expect.isLessThan (distance after) (distance before) "relaxation improves this crossing"
        Expect.equal (after.Links |> List.map(fun edge -> edge.Width)) (before.Links |> List.map(fun edge -> edge.Width)) "relaxation preserves flow width"
    testCase "varied DAGs preserve bounds padding and finite attachments" <| fun _ ->
        let random=Random 42
        for sample in 1..30 do
            let nodes=[for i in 0..23 -> sprintf "n%02d" i]
            let edges=[for e in 0..59 ->
                           let a=random.Next(23)
                           let b=random.Next(a+1,24)
                           link (string e) nodes[a] nodes[b] (float(random.Next(10)))]
            let graph=FlowGraph.create nodes edges |> ok
            for alignment in [SankeyAlignment.Left;SankeyAlignment.Right;SankeyAlignment.Center;SankeyAlignment.Justify] do
                let result=Sankey.layout {config with NodeWidth=4.;NodePadding=3.;Alignment=alignment} graph |> ok
                for node in result.Nodes do
                    let bounds=node.Bounds
                    Expect.isTrue (Bounds.y bounds>=20.-1e-9 && Bounds.y bounds+Bounds.height bounds<=260.+1e-9) (sprintf "vertical bounds sample %d" sample)
                    Expect.isTrue (Bounds.x bounds>=10. && Bounds.x bounds+Bounds.width bounds<=410.) "horizontal bounds"
                for _,column in result.Nodes |> List.groupBy(fun node -> node.Layer) do
                    for a,b in column |> List.sortBy(fun node -> Bounds.y node.Bounds) |> List.pairwise do
                        Expect.isTrue (Bounds.y a.Bounds+Bounds.height a.Bounds+3.<=Bounds.y b.Bounds+1e-9) "padding survives relaxation"
                for edge in result.Links do
                    Expect.isTrue (Double.IsFinite edge.Start.Y && Double.IsFinite edge.Finish.Y && Double.IsFinite edge.Width) "finite link"
    testCase "large iterative layouts observe cancellation" <| fun _ ->
        let nodes=[for i in 0..9999 -> string i]
        let edges=[for i in 0..4999 -> link (string i) (string i) (string(i+5000)) 1.]
        let input=FlowGraph.create nodes edges |> ok
        use cancel=new Threading.CancellationTokenSource()
        cancel.CancelAfter 1
        let timer=Diagnostics.Stopwatch.StartNew()
        Expect.throwsT<OperationCanceledException> (fun () ->
            Sankey.layoutWithCancellation cancel.Token {config with NodePadding=0.;Iterations=256} input |> ignore) "cooperative cancellation"
        Expect.isLessThan timer.Elapsed.TotalSeconds 5. "cancellation bound"
    testCase "pre-cancelled layout never publishes" <| fun _ ->
        use cancel=new Threading.CancellationTokenSource()
        cancel.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> Sankey.layoutWithCancellation cancel.Token config (graph()) |> ignore) "cancelled"
]

module Ranvier.D5.Graph.Benchmarks.Program

open System
open System.IO
open System.Text.Json
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D
open Ranvier.D5.BenchmarkHarness

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors

let private buildCompoundDag (nodeCount: int) (groupCount: int) =
    let groups = [ for g in 0 .. groupCount - 1 -> DiagramGroup.create (sprintf "g%d" g) ]
    let nodes =
        [ for i in 0 .. nodeCount - 1 ->
            DiagramNode.sized (sprintf "n%d" i) (60. + float ((i * 7) % 40)) (32. + float ((i * 5) % 16))
            |> DiagramNode.inGroup (sprintf "g%d" (i % groupCount)) ]
    let edges =
        [ for i in 0 .. nodeCount - 2 ->
            Association.create (sprintf "e_chain_%d" i) (sprintf "n%d" i) (sprintf "n%d" (min (nodeCount - 1) (i + 1 + (i % 4))))
          for i in 0 .. (nodeCount * 3 / 5) - 1 ->
            let s = (i * 3) % (nodeCount - 3)
            let t = min (nodeCount - 1) (s + 2 + (i % 6))
            Association.create (sprintf "e_skip_%d" i) (sprintf "n%d" s) (sprintf "n%d" t) ]
    DiagramGraph.create nodes groups edges |> ok

let private buildMeshNetwork (nodeCount: int) (groupCount: int) =
    let groups = [ for g in 0 .. groupCount - 1 -> DiagramGroup.create (sprintf "c%d" g) ]
    let nodes =
        [ for i in 0 .. nodeCount - 1 ->
            let nd = DiagramNode.sized (sprintf "v%d" i) 36. 28.
            if groupCount > 0 then DiagramNode.inGroup (sprintf "c%d" (i % groupCount)) nd else nd ]
    let edges =
        [ for i in 0 .. nodeCount - 1 do
            yield Association.create (sprintf "ring_%d" i) (sprintf "v%d" i) (sprintf "v%d" ((i + 1) % nodeCount))
            yield Association.create (sprintf "chord_%d" i) (sprintf "v%d" i) (sprintf "v%d" ((i + 13) % nodeCount)) ]
    DiagramGraph.create nodes groups edges |> ok

let private buildScaleHierarchy (nodeCount: int) (branching: int) (withCrossLinks: bool) =
    let nodes = [ for i in 0 .. nodeCount - 1 -> DiagramNode.create (sprintf "h%d" i) ]
    let treeEdges =
        [ for i in 1 .. nodeCount - 1 ->
            Association.create (sprintf "t%d" i) (sprintf "h%d" ((i - 1) / branching)) (sprintf "h%d" i) ]
    let crossEdges =
        if not withCrossLinks then []
        else
            [ for k in 0 .. (nodeCount / 10) - 1 ->
                let u = (k * 17 + 5) % nodeCount
                let v = (k * 31 + 19) % nodeCount
                Association.create (sprintf "x%d" k) (sprintf "h%d" u) (sprintf "h%d" v) ]
    DiagramGraph.simple nodes (treeEdges @ crossEdges) |> ok

[<EntryPoint>]
let main args =
    let vp2D = Bounds.create 0. 0. 1200. 800.
    let vp3D = Bounds3.create -400. -400. -400. 800. 800. 800.

    let dag250 = buildCompoundDag 250 10
    let dag120 = buildCompoundDag 120 6
    let net120 = buildMeshNetwork 120 4
    let net300 = buildMeshNetwork 300 8
    let net500 = buildMeshNetwork 500 0
    let net2000 = buildMeshNetwork 2000 0
    let net400_3d = buildMeshNetwork 400 6
    let tree1000 = buildScaleHierarchy 1000 4 false
    let tree2000 = buildScaleHierarchy 2000 5 false
    let tree5000 = buildScaleHierarchy 5000 5 true
    let tree10000 = buildScaleHierarchy 10000 6 false
    let h3State10000 = H3.buildState { H3Config.defaultValue with RootId = Some "h0" } tree10000 |> ok

    let octPts =
        [| for i in 0 .. 9999 ->
            Point3.create (sin (float i * 1.37) * 500.) (cos (float i * 2.11) * 500.) (sin (float i * 0.73 + 1.1) * 500.) |]
    let builtOct = Octree.build id (fun _ -> 1.) octPts

    let results = [|
        measure "sugiyama-compound-spline-250-nodes" 120. 32000000L (fun () ->
            let out = Sugiyama.layout { SugiyamaConfig.create vp2D with Routing = SugiyamaRouting.CubicSpline } dag250 |> ok
            if out.Nodes.Length <> 250 then failwith "Sugiyama 250 node count")

        measure "sugiyama-compound-orthogonal-120-nodes" 160. 40000000L (fun () ->
            let out = Sugiyama.layout { SugiyamaConfig.create vp2D with Routing = SugiyamaRouting.Orthogonal } dag120 |> ok
            if out.Nodes.Length <> 120 then failwith "Sugiyama orthogonal node count")

        measure "cola-constrained-nonoverlap-120-nodes" 150. 32000000L (fun () ->
            let out = Cola.layout { ColaConfig.create vp2D with Iterations = 30; AvoidOverlaps = true } net120 |> ok
            if out.Nodes.Length <> 120 then failwith "Cola node count")

        measure "fcose-compound-300-nodes-600-edges" 200. 48000000L (fun () ->
            let out = FCose.layout { FCoseConfig.create vp2D with Iterations = 45 } net300 |> ok
            if out.Nodes.Length <> 300 then failwith "fCoSE node count")

        measure "pivotmds-2d-2000-nodes-4000-edges" 120. 32000000L (fun () ->
            let out = PivotMds.layout { PivotMdsConfig.create vp2D with PivotCount = 40 } net2000 |> ok
            if out.Nodes.Length <> 2000 then failwith "PivotMDS 2000 node count")

        measure "stress-sgd-2d-500-nodes-1000-edges" 120. 28000000L (fun () ->
            let out = Stress2D.layout { Stress2DConfig.create vp2D with Iterations = 15; ExactPairLimit = 200; PivotCount = 36 } net500 |> ok
            if out.Nodes.Length <> 500 then failwith "Stress2D 500 node count")

        measure "modularity-forceatlas2-500-nodes-1000-edges" 220. 48000000L (fun () ->
            let out = ModularityForce2D.layout { ModularityForceConfig.create vp2D with Iterations = 45 } net500 |> ok
            if out.Nodes.Length <> 500 then failwith "ModularityForce2D 500 node count")

        measure "radial-mindmap-1000-nodes" 80. 24000000L (fun () ->
            let out = RadialTree.layout { RadialTreeConfig.create vp2D with Mode = RadialMode.BidirectionalMindmap; RootId = Some "h0" } tree1000 |> ok
            if out.Nodes.Length <> 1000 then failwith "RadialTree 1000 node count")

        measure "boxpack-compound-300-nodes" 50. 16000000L (fun () ->
            let out = BoxPack.layout (BoxPackConfig.create vp2D) net300 |> ok
            if out.Nodes.Length <> 300 then failwith "BoxPack 300 node count")

        measure "h3-munzner-5000-nodes-klein" 150. 48000000L (fun () ->
            let out = H3.layout { H3Config.defaultValue with RootId = Some "h0" } tree5000 |> ok
            if out.Nodes.Length <> 5000 then failwith "H3 5000 node count")

        measure "h3-lorentz-focus-boost-project-10000-nodes" 60. 24000000L (fun () ->
            let focused = H3.focusNode "h9999" h3State10000 |> ok |> H3.project
            if focused.Nodes.Length <> 10000 then failwith "H3 focus boost 10000 node count")

        measure "walrus-caida-5000-nodes-crosslinks" 150. 48000000L (fun () ->
            let out = Walrus.layout { WalrusConfig.defaultValue with RootId = Some "h0" } tree5000 |> ok
            if out.Nodes.Length <> 5000 then failwith "Walrus 5000 node count")

        measure "octree-3d-build-10000" 60. 16000000L (fun () ->
            let t = Octree.build id (fun _ -> 1.) octPts
            if Octree.count t <> 10000 then failwith "Octree count")

        measure "octree-3d-nearest-200-in-10000" 40. 4000000L (fun () ->
            for i = 0 to 199 do
                let q = Point3.create (octPts[i * 47].X + 1.5) (octPts[i * 47].Y - 1.5) octPts[i * 47].Z
                if (Octree.nearest q infinity builtOct).IsNone then failwith "Octree nearest")

        measure "force3d-octree-400-nodes-800-edges" 250. 48000000L (fun () ->
            let out = Force3D.layout { Force3DConfig.create vp3D with Iterations = 40 } net400_3d |> ok
            if out.Nodes.Length <> 400 then failwith "Force3D 400 node count")

        measure "stress-sgd-3d-500-nodes-1000-edges" 150. 32000000L (fun () ->
            let out = Stress3D.layout { Stress3DConfig.create vp3D with Iterations = 15; ExactPairLimit = 200; PivotCount = 36 } net500 |> ok
            if out.Nodes.Length <> 500 then failwith "Stress3D 500 node count")

        measure "conetree-balloon-3d-2000-nodes" 80. 24000000L (fun () ->
            let out = ConeTree3D.layout { ConeTree3DConfig.create vp3D with Mode = ConeTreeMode.SphericalBalloon; RootId = Some "h0" } tree2000 |> ok
            if out.Nodes.Length <> 2000 then failwith "ConeTree3D 2000 node count")

        measure "layered-3d-stratified-250-nodes" 150. 36000000L (fun () ->
            let out = Layered3D.layout (Layered3DConfig.create vp3D) dag250 |> ok
            if out.Nodes.Length <> 250 then failwith "Layered3D 250 node count")
    |]

    let output = if args.Length = 0 then "artifacts/graph-benchmarks.json" else args[0]
    let directory = Path.GetDirectoryName(Path.GetFullPath output)
    Directory.CreateDirectory directory |> ignore
    File.WriteAllText(output, JsonSerializer.Serialize(results, JsonSerializerOptions(WriteIndented = true)))
    let failed = failures results
    for item in failed do printfn "%s" (describe item)
    if failed.Length > 0 then 1 else 0

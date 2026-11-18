module Ranvier.D5.Fickl.Tests.Program

open System
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D

let private check condition message =
    if not condition then
        failwith ("Fickl JS check failed: " + message)

let private ok label = function
    | Ok value -> value
    | Error errs -> failwithf "Fickl JS %s returned Error: %A" label errs

[<EntryPoint>]
let main _ =
    let vp2D = Bounds.create 0. 0. 800. 600.
    let vp3D = Bounds3.create -250. -250. -250. 500. 500. 500.

    // 1. Compound DiagramGraph + Sugiyama (Orthogonal & Spline) + DiagramScene
    let groups =
        [ { DiagramGroup.create "gOuter" with Padding = GroupPadding.titled 26. 12. }
          { DiagramGroup.create "gInner" with Padding = GroupPadding.titled 22. 10. } |> DiagramGroup.inGroup "gOuter" ]
    let nodes =
        [ DiagramNode.sized "start" 20. 20. |> DiagramNode.inGroup "gOuter" |> DiagramNode.withKind NodeKind.InitialState
          DiagramNode.sized "n1" 84. 32.
          |> DiagramNode.inGroup "gInner"
          |> DiagramNode.withPorts [ DiagramPort.create "in" PortSide.North; DiagramPort.create "out" PortSide.South ]
          DiagramNode.sized "n2" 84. 32. |> DiagramNode.inGroup "gInner"
          DiagramNode.sized "n3" 84. 32. ]
    let assocs =
        [ { Association.create "e0" "start" "n1" with Target = Endpoint.port "n1" "in"; Kind = AssociationKind.Transition }
          { Association.betweenPorts "e1" (Endpoint.port "n1" "out") (Endpoint.node "n2") AssociationKind.Composition with
              CenterLabel = Some { Width = 32.; Height = 14. } }
          Association.create "e2" "n2" "n3" ]
    let diagram =
        DiagramGraph.createWithConstraints
            nodes
            groups
            assocs
            [ LayoutConstraint.AlignHorizontal [ "n2"; "n3" ] ]
        |> ok "DiagramGraph.createWithConstraints"

    let sugOrtho =
        Sugiyama.layout { SugiyamaConfig.create vp2D with Routing = SugiyamaRouting.Orthogonal } diagram
        |> ok "Sugiyama.Orthogonal"
    check (sugOrtho.Nodes.Length = 4 && sugOrtho.Groups.Length = 2 && sugOrtho.Associations.Length = 3) "Sugiyama counts"
    check (sugOrtho.Associations |> List.forall (fun a -> a.Waypoints.Length >= 2)) "Sugiyama waypoints"
    let rendered2D = DiagramScene.render (DiagramTheme.create 800. 600. "JS Scene 2D") sugOrtho
    check (not (List.isEmpty (Scene.nodes rendered2D))) "DiagramScene.render nodes"

    // 2. IPSep-CoLa, fCoSE, PivotMDS, Stress2D, ModularityForce2D, RadialTree, BoxPack
    let colaOut = Cola.layout { ColaConfig.create vp2D with Iterations = 25; AvoidOverlaps = true } diagram |> ok "Cola"
    let n2Cola = colaOut.Nodes |> List.find (fun n -> n.Id = "n2")
    let n3Cola = colaOut.Nodes |> List.find (fun n -> n.Id = "n3")
    check (abs (n2Cola.Center.Y - n3Cola.Center.Y) < 1e-2) "Cola horizontal alignment rail in JS"

    let fcoseOut = FCose.layout { FCoseConfig.create vp2D with Iterations = 30 } diagram |> ok "FCose"
    check (fcoseOut.Nodes.Length = 4 && fcoseOut.Groups.Length = 2) "FCose counts"

    let meshNodes = [ for i in 0 .. 29 -> DiagramNode.sized (sprintf "m%d" i) 30. 24. ]
    let meshEdges =
        [ for i in 0 .. 29 do
            yield Association.create (sprintf "r%d" i) (sprintf "m%d" i) (sprintf "m%d" ((i + 1) % 30))
            yield Association.create (sprintf "c%d" i) (sprintf "m%d" i) (sprintf "m%d" ((i + 5) % 30)) ]
    let mesh = DiagramGraph.simple meshNodes meshEdges |> ok "mesh"

    let pmdsOut = PivotMds.layout { PivotMdsConfig.create vp2D with PivotCount = 12 } mesh |> ok "PivotMds"
    check (pmdsOut.Nodes.Length = 30) "PivotMds count"

    let stress2DOut = Stress2D.layout { Stress2DConfig.create vp2D with Iterations = 15 } mesh |> ok "Stress2D"
    check (stress2DOut.Nodes.Length = 30) "Stress2D count"

    let modOut = ModularityForce2D.layout { ModularityForceConfig.create vp2D with Iterations = 25; Model = EnergyModel.LinLog } mesh |> ok "ModularityForce2D"
    check (modOut.Nodes.Length = 30) "ModularityForce2D count"

    let treeNodes = [ for i in 0 .. 30 -> DiagramNode.create (sprintf "t%d" i) ]
    let treeEdges = [ for i in 1 .. 30 -> Association.create (sprintf "te%d" i) (sprintf "t%d" ((i - 1) / 3)) (sprintf "t%d" i) ]
    let treeGraph = DiagramGraph.simple treeNodes (Association.create "x0" "t4" "t19" :: treeEdges) |> ok "treeGraph"

    let radialOut = RadialTree.layout { RadialTreeConfig.create vp2D with Mode = RadialMode.BidirectionalMindmap; RootId = Some "t0" } treeGraph |> ok "RadialTree"
    check (radialOut.Nodes.Length = 31) "RadialTree count"

    let boxOut = BoxPack.layout (BoxPackConfig.create vp2D) diagram |> ok "BoxPack"
    check (boxOut.Nodes.Length = 4) "BoxPack count"

    // 3. Octree + Hyperbolic3D + Munzner H3 + CAIDA Walrus + Force3D + Stress3D + ConeTree3D + Layered3D + Scene3D
    let pts3 = [| for i in 0 .. 63 -> i, Point3.create (sin (float i) * 80.) (cos (float i * 1.7) * 80.) (float (i - 32) * 2.) |]
    let oct = Octree.build snd (fun _ -> 1.) pts3
    check (Octree.count oct = 64) "Octree.count"
    let nearest = Octree.nearest (snd pts3[17]) infinity oct
    check (nearest |> Option.exists (fun (idx, _) -> idx = 17)) "Octree.nearest exact match"

    let mp = Hyperbolic3D.ofSpacelike 0.8 -0.5 1.1
    let ballPt, atten = Hyperbolic3D.toBall HyperbolicModel.Poincare mp
    check (atten > 0. && atten <= 1.) "Hyperbolic3D attenuation in JS"
    let roundTrip = Hyperbolic3D.ofBall HyperbolicModel.Poincare ballPt |> ok "Hyperbolic3D.ofBall"
    check (Hyperbolic3D.distance mp roundTrip < 1e-6) "Hyperbolic3D Poincare round-trip in JS"

    let h3State = H3.buildState { H3Config.defaultValue with RootId = Some "t0"; Model = HyperbolicModel.Klein } treeGraph |> ok "H3.buildState"
    let h3Focused = H3.focusNode "t12" h3State |> ok "H3.focusNode" |> H3.project
    let t12 = h3Focused.Nodes |> List.find (fun n -> n.Id = "t12")
    check (Point3.distance t12.Center (Point3.create 0. 0. 0.) < 1e-5) "H3 Lorentz boost moves focus node to origin in JS"

    let walrusOut = Walrus.layout { WalrusConfig.defaultValue with RootId = Some "t0" } treeGraph |> ok "Walrus.layout"
    check (walrusOut.Nodes.Length = 31 && walrusOut.Associations.Length = 31) "Walrus counts"
    check ((walrusOut.Associations |> List.filter (fun a -> a.IsBackbone)).Length = 30) "Walrus backbone count"

    let force3DOut = Force3D.layout { Force3DConfig.create vp3D with Iterations = 20 } mesh |> ok "Force3D"
    check (force3DOut.Nodes.Length = 30) "Force3D count"

    let stress3DOut = Stress3D.layout { Stress3DConfig.create vp3D with Iterations = 15 } mesh |> ok "Stress3D"
    check (stress3DOut.Nodes.Length = 30) "Stress3D count"

    let coneOut = ConeTree3D.layout { ConeTree3DConfig.create vp3D with Mode = ConeTreeMode.SphericalBalloon; RootId = Some "t0" } treeGraph |> ok "ConeTree3D"
    check (coneOut.Nodes.Length = 31) "ConeTree3D count"

    let lay3DOut = Layered3D.layout (Layered3DConfig.create vp3D) diagram |> ok "Layered3D"
    check (lay3DOut.Nodes.Length = 4 && lay3DOut.Groups.Length = 2) "Layered3D counts"

    let cam3 = Camera3D.orbit (Point3.create 0. 0. 0.) 0.5 0.3 650.
    let rendered3D = Scene3D.render { Scene3DTheme.create 800. 600. "JS Scene 3D" with ReferenceSphereRadius = Some 300. } cam3 h3Focused
    check (not (List.isEmpty (Scene.nodes rendered3D))) "Scene3D.render nodes"

    printfn "Fickl JS runtime suite passed: all 2D and 3D layout engines executed cleanly in JavaScript."
    0

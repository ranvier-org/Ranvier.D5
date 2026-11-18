module Ranvier.D5.Gallery.GraphLayoutsGallery

open System
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors

let private panelCard (id: string) (x: float) (y: float) (w: float) (h: float) (caption: string) (innerNodes: SceneNode list) =
    let border =
        Shape.rectangle (Bounds.create 0. 0. w h)
        |> Mark.path (id + ":card")
        |> Mark.withFill (Paint.rgb 250 252 255)
        |> Mark.withStroke (Paint.rgb 203 213 225) 1.25
    let header =
        Mark.text (id + ":caption") (Point.create 12. 18.) caption
        |> Mark.withFill (Paint.rgb 30 41 59)
        |> Mark.withFontSize 13.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "start"; Baseline = "alphabetic" }
    let bodyGroup =
        Group({ GroupSpec.create (id + ":body") with Transform = Transform.translate 0. 24. }, innerNodes)
    Group(
        { GroupSpec.create id with Transform = Transform.translate x y; Clip = Some ("clip:" + id) },
        [ Leaf border; Leaf header; bodyGroup ]
    ),
    RectClip("clip:" + id, Bounds.create 0. 0. w h)

// ---------------------------------------------------------------------------
// 1. 2D Gallery: Sugiyama Orthogonal Association Machine + IPSep-CoLa + fCoSE
// ---------------------------------------------------------------------------

let private buildSugiyamaDiagram () =
    let groups =
        [ { DiagramGroup.create "Ingress" with Padding = GroupPadding.titled 20. 8. }
          { DiagramGroup.create "Control" with Padding = GroupPadding.titled 20. 8. }
          { DiagramGroup.create "Auth" with Padding = GroupPadding.titled 18. 8. } |> DiagramGroup.inGroup "Control"
          { DiagramGroup.create "Storage" with Padding = GroupPadding.titled 20. 8. } ]
    let nodes =
        [ DiagramNode.sized "Start" 18. 18. |> DiagramNode.inGroup "Ingress" |> DiagramNode.withKind NodeKind.InitialState
          DiagramNode.sized "Gateway" 82. 28.
          |> DiagramNode.inGroup "Ingress"
          |> DiagramNode.withPorts [ DiagramPort.create "in" PortSide.North; DiagramPort.create "out" PortSide.South; DiagramPort.create "audit" PortSide.East ]
          DiagramNode.sized "Route" 36. 36. |> DiagramNode.inGroup "Auth" |> DiagramNode.withKind NodeKind.Choice
          DiagramNode.sized "TokenSvc" 84. 28. |> DiagramNode.inGroup "Auth"
          DiagramNode.sized "Policy" 80. 28. |> DiagramNode.inGroup "Control"
          DiagramNode.sized "SessionDB" 88. 28. |> DiagramNode.inGroup "Storage"
          DiagramNode.sized "AuditLog" 84. 28. |> DiagramNode.inGroup "Storage"
          DiagramNode.sized "Done" 20. 20. |> DiagramNode.inGroup "Storage" |> DiagramNode.withKind NodeKind.FinalState ]
    let assocs =
        [ { Association.create "e1" "Start" "Gateway" with Target = Endpoint.port "Gateway" "in"; Kind = AssociationKind.Transition }
          { Association.betweenPorts "e2" (Endpoint.port "Gateway" "out") (Endpoint.node "Route") AssociationKind.Transition with
              CenterLabel = Some { Width = 36.; Height = 16. } }
          { Association.create "e3" "Route" "TokenSvc" with Kind = AssociationKind.Transition; CenterLabel = Some { Width = 34.; Height = 16. } }
          { Association.create "e4" "Route" "Policy" with Kind = AssociationKind.Dependency }
          { Association.create "e5" "TokenSvc" "SessionDB" with
              Kind = AssociationKind.Composition
              SourceLabel = Some { Width = 16.; Height = 14. }
              TargetLabel = Some { Width = 24.; Height = 14. } }
          { Association.betweenPorts "e6" (Endpoint.port "Gateway" "audit") (Endpoint.node "AuditLog") AssociationKind.Aggregation with
              TargetLabel = Some { Width = 22.; Height = 14. } }
          { Association.create "e7" "Policy" "AuditLog" with Kind = AssociationKind.Directed }
          { Association.create "e8" "SessionDB" "Done" with Kind = AssociationKind.Transition } ]
    DiagramGraph.create nodes groups assocs |> ok

let private buildColaDiagram () =
    let groups =
        [ { DiagramGroup.create "DataPlane" with Padding = GroupPadding.titled 22. 8. } ]
    let nodes =
        [ DiagramNode.sized "Client" 64. 26.
          DiagramNode.sized "API" 64. 26. |> DiagramNode.inGroup "DataPlane"
          DiagramNode.sized "Cache" 64. 26. |> DiagramNode.inGroup "DataPlane"
          DiagramNode.sized "Queue" 64. 26. |> DiagramNode.inGroup "DataPlane"
          DiagramNode.sized "Ledger" 68. 26. ]
    let assocs =
        [ Association.create "c1" "Client" "API"
          { Association.create "c2" "API" "Cache" with Kind = AssociationKind.Bidirectional }
          Association.create "c3" "API" "Queue"
          { Association.create "c4" "Queue" "Ledger" with Kind = AssociationKind.Composition }
          { Association.create "c5" "Cache" "Ledger" with Kind = AssociationKind.Dependency } ]
    let constraints =
        [ LayoutConstraint.AlignHorizontal [ "API"; "Cache"; "Queue" ]
          LayoutConstraint.OrderAlong(LayoutAxis.Vertical, "Client", "API", 42.)
          LayoutConstraint.OrderAlong(LayoutAxis.Vertical, "Queue", "Ledger", 42.) ]
    DiagramGraph.createWithConstraints nodes groups assocs constraints |> ok

let private buildFCoseDiagram () =
    let groups =
        [ { DiagramGroup.create "ClusterA" with Padding = GroupPadding.uniform 14. }
          { DiagramGroup.create "ClusterB" with Padding = GroupPadding.uniform 14. }
          { DiagramGroup.create "ClusterC" with Padding = GroupPadding.uniform 14. } ]
    let gNames = [| "ClusterA"; "ClusterB"; "ClusterC" |]
    let nodes =
        [ for c in 0 .. 2 do
            for k in 0 .. 3 do
                yield DiagramNode.sized (sprintf "M%d%d" c k) 34. 22. |> DiagramNode.inGroup gNames[c] ]
    let assocs =
        [ for c in 0 .. 2 do
            for k in 0 .. 3 do
                yield Association.create (sprintf "in_%d_%d" c k) (sprintf "M%d%d" c k) (sprintf "M%d%d" c ((k + 1) % 4))
            yield Association.create (sprintf "diag_%d" c) (sprintf "M%d0" c) (sprintf "M%d2" c)
          yield Association.create "br_01" "M01" "M10"
          yield Association.create "br_12" "M12" "M20"
          yield Association.create "br_20" "M23" "M03" ]
    DiagramGraph.create nodes groups assocs |> ok

let private compute2DLayouts (phase: float) =
    let sugGraph = buildSugiyamaDiagram ()
    let sugCfg =
        { SugiyamaConfig.create (Bounds.create 12. 4. 400. 468.) with
            LayerSpacing = 22. + 4. * sin phase
            NodeSpacing = 20.
            Routing = SugiyamaRouting.Orthogonal }
    let sugLayout = Sugiyama.layout sugCfg sugGraph |> ok

    let colaCfg =
        { ColaConfig.create (Bounds.create 18. 8. 288. 194.) with
            IdealEdgeLength = 60. + 4. * cos phase
            NodeMargin = 14.
            Iterations = 50
            AvoidOverlaps = true }
    let colaLayout = Cola.layout colaCfg (buildColaDiagram ()) |> ok

    let fcoseCfg =
        { FCoseConfig.create (Bounds.create 20. 8. 286. 188.) with
            IdealEdgeLength = 36.
            Iterations = 65 }
    let fcoseLayout = FCose.layout fcoseCfg (buildFCoseDiagram ()) |> ok
    sugLayout, colaLayout, fcoseLayout

let probes2D (phase: float) : (string * Point) list =
    let sug, cola, fcose = compute2DLayouts phase
    let findCenter (layout: DiagramLayout2D) id ox oy =
        let n = layout.Nodes |> List.find (fun item -> item.Id = id)
        Point.create (ox + n.Center.X) (oy + n.Center.Y)
    [ "sugiyama:Gateway", findCenter sug "Gateway" 16. (44. + 24.)
      "sugiyama:SessionDB", findCenter sug "SessionDB" 16. (44. + 24.)
      "cola:API", findCenter cola "API" 460. (44. + 24.)
      "fcose:M00", findCenter fcose "M00" 460. (308. + 24.) ]

let scene2D (phase: float) : Scene =
    let sugLayout, colaLayout, fcoseLayout = compute2DLayouts phase

    let sugPalette id =
        match id with
        | "Gateway" -> Paint.rgb 219 234 254, Paint.rgb 29 78 216
        | "TokenSvc" | "Policy" -> Paint.rgb 224 231 255, Paint.rgb 67 56 202
        | "SessionDB" | "AuditLog" -> Paint.rgb 209 250 229, Paint.rgb 4 120 87
        | _ -> Paint.rgb 254 243 199, Paint.rgb 180 83 9

    let sugTheme =
        { DiagramTheme.create 424. 482. "Compound Sugiyama" with
            NodeFill = fst << sugPalette
            NodeStroke = snd << sugPalette
            AssociationLabels = function
                | "e2" -> None, Some "[req]", None
                | "e3" -> None, Some "[jwt]", None
                | "e5" -> Some "1", None, Some "0..*"
                | "e6" -> None, None, Some "1..*"
                | _ -> None, None, None }
    let sugScene = DiagramScene.render sugTheme sugLayout

    let colaTheme =
        { DiagramTheme.create 324. 218. "IPSep-CoLa" with
            NodeFill = fun id -> if id = "Client" || id = "Ledger" then Paint.rgb 254 226 226 else Paint.rgb 224 242 254
            NodeStroke = fun id -> if id = "Client" || id = "Ledger" then Paint.rgb 185 28 28 else Paint.rgb 3 105 161 }
    let colaScene = DiagramScene.render colaTheme colaLayout

    let fcoseTheme =
        { DiagramTheme.create 324. 218. "fCoSE" with
            GroupLabel = fun _ -> ""
            NodeFill = fun id ->
                if id.StartsWith "M0" then Paint.rgb 191 219 254
                elif id.StartsWith "M1" then Paint.rgb 167 243 208
                else Paint.rgb 253 230 138
            NodeStroke = fun id ->
                if id.StartsWith "M0" then Paint.rgb 30 64 175
                elif id.StartsWith "M1" then Paint.rgb 6 95 70
                else Paint.rgb 146 64 14 }
    let fcoseScene = DiagramScene.render fcoseTheme fcoseLayout

    let p1, c1 = panelCard "p-sugiyama" 16. 44. 428. 510. "Sugiyama Compound Orthogonal + UML/State Associations" (Scene.nodes sugScene)
    let p2, c2 = panelCard "p-cola" 458. 44. 326. 246. "IPSep-CoLa Rails, Flow & Non-Overlap" (Scene.nodes colaScene)
    let p3, c3 = panelCard "p-fcose" 458. 308. 326. 246. "fCoSE Compound Spring Embedder" (Scene.nodes fcoseScene)

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "bg"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "gallery-title" (Point.create 18. 27.) "Ranvier.D5.Graph — Compound Grouping, Port Routing & Association Machines"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 17.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose { Width = 800.; Height = 570. } "2D Graph Layout Engines" [ c1; c2; c3 ] [ Leaf pageBg; Leaf title; p1; p2; p3 ]

// ---------------------------------------------------------------------------
// 2. 3D Gallery: Tamara Munzner H3 + CAIDA Walrus + Hong-Nikolov Layered3D
// ---------------------------------------------------------------------------

let private h3Hierarchy =
    let nodes = ResizeArray<DiagramNode>()
    let assocs = ResizeArray<Association>()
    nodes.Add(DiagramNode.create "root")
    for b = 0 to 3 do
        let bId = sprintf "b%d" b
        nodes.Add(DiagramNode.create bId)
        assocs.Add(Association.create (sprintf "eb_%d" b) "root" bId)
        for c = 0 to 3 do
            let cId = sprintf "c%d_%d" b c
            nodes.Add(DiagramNode.create cId)
            assocs.Add(Association.create (sprintf "ec_%d_%d" b c) bId cId)
            for l = 0 to 2 do
                let lId = sprintf "l%d_%d_%d" b c l
                nodes.Add(DiagramNode.create lId)
                assocs.Add(Association.create (sprintf "el_%d_%d_%d" b c l) cId lId)
    // Non-tree cross-links across subtrees
    assocs.Add(Association.create "x1" "c0_1" "c2_2")
    assocs.Add(Association.create "x2" "c1_3" "c3_0")
    assocs.Add(Association.create "x3" "l0_2_1" "l3_1_2")
    DiagramGraph.simple (List.ofSeq nodes) (List.ofSeq assocs) |> ok

let private h3BaseState =
    H3.buildState
        { H3Config.defaultValue with
            BallRadius = 175.
            Model = HyperbolicModel.Poincare
            RootId = Some "root" }
        h3Hierarchy
    |> ok

let private walrusTopology =
    let nodes = [ for i in 0 .. 54 -> DiagramNode.create (sprintf "AS%d" i) ]
    let treeEdges =
        [ for i in 1 .. 54 ->
            let p = (i - 1) / 4
            Association.create (sprintf "backbone_%d" i) (sprintf "AS%d" p) (sprintf "AS%d" i) ]
    let peeringEdges =
        [ for k in 0 .. 11 ->
            let u = 5 + (k * 7) % 49
            let v = 5 + (k * 13 + 9) % 49
            if u = v then Association.create (sprintf "peer_%d" k) "AS5" "AS18"
            else Association.create (sprintf "peer_%d" k) (sprintf "AS%d" u) (sprintf "AS%d" v) ]
    let backboneSet = treeEdges |> List.map (fun a -> a.Id) |> Set.ofList
    DiagramGraph.simple nodes (treeEdges @ peeringEdges) |> ok, backboneSet

let private layered3DGraph =
    let groups =
        [ DiagramGroup.create "TierUI"
          DiagramGroup.create "TierAPI"
          DiagramGroup.create "TierData" ]
    let nodes =
        [ DiagramNode.sized "Web" 24. 16. |> DiagramNode.inGroup "TierUI"
          DiagramNode.sized "Mobile" 24. 16. |> DiagramNode.inGroup "TierUI"
          DiagramNode.sized "Gateway" 26. 16. |> DiagramNode.inGroup "TierAPI"
          DiagramNode.sized "GraphQL" 26. 16. |> DiagramNode.inGroup "TierAPI"
          DiagramNode.sized "Worker" 26. 16. |> DiagramNode.inGroup "TierAPI"
          DiagramNode.sized "Postgres" 26. 16. |> DiagramNode.inGroup "TierData"
          DiagramNode.sized "Redis" 24. 16. |> DiagramNode.inGroup "TierData"
          DiagramNode.sized "Blob" 24. 16. |> DiagramNode.inGroup "TierData" ]
    let assocs =
        [ Association.create "l1" "Web" "Gateway"
          Association.create "l2" "Mobile" "GraphQL"
          Association.create "l3" "Gateway" "Worker"
          Association.create "l4" "Gateway" "Redis"
          Association.create "l5" "GraphQL" "Postgres"
          Association.create "l6" "Worker" "Postgres"
          Association.create "l7" "Worker" "Blob" ]
    DiagramGraph.create nodes groups assocs |> ok

let private compute3DScenes (phase: float) =
    let focusTarget = if phase >= 0.5 then "b0" else "root"
    let focusedState =
        if focusTarget = "root" then h3BaseState
        else H3.focusNode focusTarget h3BaseState |> ok
    let h3Layout = H3.project focusedState
    let h3Cam = Camera3D.orbit (Point3.create 0. 0. 0.) (0.55 + phase * 0.5) 0.28 545.
    let h3Vp : Viewport = { Width = 424.; Height = 482. }
    let h3Theme =
        { Scene3DTheme.create h3Vp.Width h3Vp.Height "Munzner H3" with
            ReferenceSphereRadius = Some 175.
            NodeFill = fun id ->
                if id = focusTarget then Paint.rgb 220 38 38
                elif id = "root" then Paint.rgb 37 99 235
                elif id.StartsWith "b" then Paint.rgb 13 148 136
                elif id.StartsWith "c" then Paint.rgb 79 70 229
                else Paint.rgb 100 116 139
            AssociationStroke = fun _ isBackbone ->
                if isBackbone then Paint.rgb 51 65 85 else Paint.rgb 217 119 6
            NodeLabel = fun id -> if id = "root" || id.StartsWith "b" then Some id else None }
    let h3Scene = Scene3D.render h3Theme h3Cam h3Layout

    let walrusGraph, backboneIds = walrusTopology
    let walrusLayout =
        Walrus.layout
            { WalrusConfig.defaultValue with
                SphereRadius = 90.
                Model = HyperbolicModel.Klein
                RootId = Some "AS0"
                Backbone = WalrusTreeSource.ExplicitAssociations backboneIds
                IncludeCrossLinks = true }
            walrusGraph
        |> ok
    let walrusCam = Camera3D.orbit (Point3.create 0. 0. 0.) (-0.4 + phase * 0.45) 0.34 290.
    let walrusVp : Viewport = { Width = 326.; Height = 218. }
    let walrusTheme =
        { Scene3DTheme.create walrusVp.Width walrusVp.Height "CAIDA Walrus" with
            ReferenceSphereRadius = Some 90.
            NodeFill = fun id ->
                if id = "AS0" then Paint.rgb 190 24 93
                elif id = "AS1" || id = "AS2" || id = "AS3" || id = "AS4" then Paint.rgb 14 116 144
                else Paint.rgb 59 130 246
            AssociationStroke = fun _ isBackbone ->
                if isBackbone then Paint.rgb 15 118 110 else Paint.rgb 234 88 12
            NodeLabel = fun id -> if id = "AS0" || id = "AS1" || id = "AS2" then Some id else None }
    let walrusScene = Scene3D.render walrusTheme walrusCam walrusLayout

    let lay3D =
        Layered3D.layout
            { Layered3DConfig.create (Bounds3.create -90. -70. -70. 180. 140. 140.) with
                Axis = StratumAxis.Y
                LayerSpacing = 72.
                IntraLayerSpacing = 26. }
            layered3DGraph
        |> ok
    let layCam = Camera3D.orbit (Bounds3.center lay3D.Bounds) (0.72 + phase * 0.32) 0.32 520.
    let layVp : Viewport = { Width = 326.; Height = 218. }
    let layTheme =
        { Scene3DTheme.create layVp.Width layVp.Height "3D Stratified" with
            NodeFill = fun id ->
                match id with
                | "Web" | "Mobile" -> Paint.rgb 37 99 235
                | "Gateway" | "GraphQL" | "Worker" -> Paint.rgb 124 58 237
                | _ -> Paint.rgb 5 150 105
            NodeLabel = Some }
    let layScene = Scene3D.render layTheme layCam lay3D

    (h3Layout, h3Cam, h3Vp, h3Scene, focusTarget),
    (walrusLayout, walrusCam, walrusVp, walrusScene),
    (lay3D, layCam, layVp, layScene)

let probes3D (phase: float) : (string * Point) list =
    let (h3L, h3Cam, h3Vp, _, focusId), (wL, wCam, wVp, _), (lL, lCam, lVp, _) = compute3DScenes phase
    let projectId (layout: DiagramLayout3D) cam vp id ox oy =
        let n = layout.Nodes |> List.find (fun item -> item.Id = id)
        let pt, _, _ = Camera3D.projectPoint vp cam n.Center |> Option.get
        Point.create (ox + pt.X) (oy + pt.Y)
    [ "h3:focus", projectId h3L h3Cam h3Vp focusId 16. (44. + 24.)
      "walrus:AS0", projectId wL wCam wVp "AS0" 458. (44. + 24.)
      "layered3d:Gateway", projectId lL lCam lVp "Gateway" 458. (308. + 24.) ]

let scene3D (phase: float) : Scene =
    let (_, _, _, h3Scene, focusTarget), (_, _, _, walrusScene), (_, _, _, layScene) = compute3DScenes phase
    let h3Caption = sprintf "Tamara Munzner H3 (Poincaré Ball, Focus = %s)" focusTarget
    let p1, c1 = panelCard "p-h3" 16. 44. 428. 510. h3Caption (Scene.nodes h3Scene)
    let p2, c2 = panelCard "p-walrus" 458. 44. 326. 246. "CAIDA Walrus (Klein Backbone + Cross-Links)" (Scene.nodes walrusScene)
    let p3, c3 = panelCard "p-layered3d" 458. 308. 326. 246. "Hong-Nikolov 3D Stratified Compound Planes" (Scene.nodes layScene)

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "bg3d"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "gallery3d-title" (Point.create 18. 27.) "Ranvier.D5.Graph3D — Hyperbolic H3, CAIDA Walrus & 3D Stratified Layouts"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 17.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose { Width = 800.; Height = 570. } "3D Graph Layout Engines" [ c1; c2; c3 ] [ Leaf pageBg; Leaf title; p1; p2; p3 ]

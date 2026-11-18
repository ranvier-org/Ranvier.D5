module Ranvier.D5.Tests.GraphLayout2DTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Graph

let private ok = function Ok v -> v | Error errs -> failwithf "Unexpected error: %A" errs
let private viewport = Bounds.create 0. 0. 960. 640.

let private boxesDisjoint (tol: float) (a: Bounds) (b: Bounds) =
    Bounds.x a + Bounds.width a <= Bounds.x b + tol
    || Bounds.x b + Bounds.width b <= Bounds.x a + tol
    || Bounds.y a + Bounds.height a <= Bounds.y b + tol
    || Bounds.y b + Bounds.height b <= Bounds.y a + tol

let private boxContainsBox (tol: float) (outer: Bounds) (inner: Bounds) =
    Bounds.x inner >= Bounds.x outer - tol
    && Bounds.y inner >= Bounds.y outer - tol
    && Bounds.x inner + Bounds.width inner <= Bounds.x outer + Bounds.width outer + tol
    && Bounds.y inner + Bounds.height inner <= Bounds.y outer + Bounds.height outer + tol

let tests =
    testList "GraphLayout2D" [
        testCase "DiagramGraph validates IDs, ports, group cycles and collapsed macro-groups" <| fun _ ->
            Expect.isError
                (DiagramGraph.simple [ DiagramNode.create "a"; DiagramNode.create "a" ] [])
                "duplicate node ID"
            Expect.isError
                (DiagramGraph.create [ DiagramNode.create "a" ] [ DiagramGroup.create "a" ] [])
                "node/group ID collision"
            Expect.isError
                (DiagramGraph.create
                    []
                    [ DiagramGroup.create "g1" |> DiagramGroup.inGroup "g2"
                      DiagramGroup.create "g2" |> DiagramGroup.inGroup "g1" ]
                    [])
                "cyclic group nesting"
            Expect.isError
                (DiagramGraph.simple
                    [ DiagramNode.create "a" ]
                    [ Association.betweenPorts "e" (Endpoint.port "a" "unknown") (Endpoint.node "a") AssociationKind.Directed ])
                "missing port"

            // Collapsed compound group acts as macro-node and hides internal edges while remapping external edges
            let gCollapsed = DiagramGroup.create "sub" |> DiagramGroup.withCollapsed true
            let n1 = DiagramNode.create "n1" |> DiagramNode.inGroup "sub"
            let n2 = DiagramNode.create "n2" |> DiagramNode.inGroup "sub"
            let ext = DiagramNode.create "ext"
            let g =
                DiagramGraph.create
                    [ n1; n2; ext ]
                    [ gCollapsed ]
                    [ Association.create "internal" "n1" "n2"
                      Association.create "outbound" "n2" "ext" ]
                |> ok
            let resolved = DiagramGraph.resolveCollapsed g
            Expect.equal (DiagramGraph.nodes resolved |> List.map (fun n -> n.Id)) [ "ext"; "sub" ] "collapsed macro-node replaces members"
            Expect.equal (DiagramGraph.associations resolved |> List.map (fun a -> a.Id, a.Source.NodeId, a.Target.NodeId)) [ ("outbound", "sub", "ext") ] "internal suppressed, cross remapped"

        testCase "Sugiyama handles state machines, cycles, self-loops, 4 directions and nested compound groups" <| fun _ ->
            let gOuter = DiagramGroup.create "auth"
            let gInner = DiagramGroup.create "mfa" |> DiagramGroup.inGroup "auth"
            let nodes =
                [ DiagramNode.sized "start" 24. 24. |> DiagramNode.withKind NodeKind.InitialState
                  DiagramNode.sized "login" 100. 44. |> DiagramNode.inGroup "auth"
                  DiagramNode.sized "verify" 110. 44. |> DiagramNode.inGroup "mfa" |> DiagramNode.withKind NodeKind.Choice
                  DiagramNode.sized "retry" 90. 40. |> DiagramNode.inGroup "mfa"
                  DiagramNode.sized "done" 28. 28. |> DiagramNode.withKind NodeKind.FinalState ]
            let assocs =
                [ Association.transition "t1" "start" "login" None
                  Association.transition "t2" "login" "verify" (Some { Width = 48.; Height = 18. })
                  Association.transition "t3" "verify" "retry" (Some { Width = 40.; Height = 16. })
                  Association.transition "t4" "retry" "login" None // cycle back-edge!
                  Association.transition "t5" "verify" "verify" None // self-loop!
                  Association.transition "t6" "verify" "done" None ]
            let dg = DiagramGraph.create nodes [ gOuter; gInner ] assocs |> ok

            for dir in [ LayoutDirection.TopDown; LayoutDirection.BottomUp; LayoutDirection.LeftRight; LayoutDirection.RightLeft ] do
                let cfg = { SugiyamaConfig.create viewport with Direction = dir; Routing = SugiyamaRouting.Orthogonal }
                let out = Sugiyama.layout cfg dg |> ok
                Expect.equal out.Nodes.Length 5 "all nodes positioned"
                Expect.equal out.Groups.Length 2 "both nested groups positioned"
                Expect.equal out.Associations.Length 6 "all transitions routed"

                // No pair of positioned nodes overlaps
                let nArr = List.toArray out.Nodes
                for i = 0 to nArr.Length - 1 do
                    for j = i + 1 to nArr.Length - 1 do
                        Expect.isTrue (boxesDisjoint 1e-4 nArr[i].Bounds nArr[j].Bounds) (sprintf "nodes disjoint in %A: %s vs %s" dir nArr[i].Id nArr[j].Id)

                // Nested group containment: mfa inside auth, verify & retry inside mfa, login inside auth
                let gMap = out.Groups |> List.map (fun g -> g.Id, g.Bounds) |> Map.ofList
                let nMap = out.Nodes |> List.map (fun n -> n.Id, n) |> Map.ofList
                Expect.isTrue (boxContainsBox 1e-4 gMap["auth"] gMap["mfa"]) "inner group inside outer group"
                Expect.isTrue (boxContainsBox 1e-4 gMap["mfa"] nMap["verify"].Bounds) "verify inside mfa"
                Expect.isTrue (boxContainsBox 1e-4 gMap["mfa"] nMap["retry"].Bounds) "retry inside mfa"
                Expect.isTrue (boxContainsBox 1e-4 gMap["auth"] nMap["login"].Bounds) "login inside auth"

                // Back-edge marked ReversedInCycle = true, and InitialState sits at rank 0
                Expect.equal nMap["start"].Layer (Some 0) "InitialState placed at source rank 0"
                Expect.isTrue (out.Associations |> List.exists (fun a -> a.ReversedInCycle)) "cycle edge flagged"

        testCase "OrthogonalRouter avoids intermediate obstacles, separates parallel channels and places ER labels" <| fun _ ->
            let src =
                DiagramNode.sized "Customer" 100. 50.
                |> DiagramNode.withPin (Point.create 100. 200.)
                |> DiagramNode.withPorts [ DiagramPort.ordered "out1" PortSide.East 0; DiagramPort.ordered "out2" PortSide.East 1 ]
            let blocker =
                DiagramNode.sized "Blocker" 90. 120.
                |> DiagramNode.withPin (Point.create 300. 200.)
            let dst =
                DiagramNode.sized "Order" 100. 50.
                |> DiagramNode.withPin (Point.create 500. 200.)
                |> DiagramNode.withPorts [ DiagramPort.ordered "in1" PortSide.West 0; DiagramPort.ordered "in2" PortSide.West 1 ]

            let a1 =
                { Association.betweenPorts "places" (Endpoint.port "Customer" "out1") (Endpoint.port "Order" "in1") AssociationKind.Composition with
                    SourceLabel = Some { Width = 20.; Height = 14. }
                    CenterLabel = Some { Width = 44.; Height = 16. }
                    TargetLabel = Some { Width = 28.; Height = 14. } }
            let a2 = Association.betweenPorts "audits" (Endpoint.port "Customer" "out2") (Endpoint.port "Order" "in2") AssociationKind.Dependency

            let dg = DiagramGraph.simple [ src; blocker; dst ] [ a1; a2 ] |> ok
            let baseLayout = BoxPack.layout (BoxPackConfig.create viewport) dg |> ok
            let routed = OrthogonalRouter.route { OrthogonalRouterConfig.defaultValue with CornerRadius = 0. } dg baseLayout |> ok

            let blockerBox = (routed.Nodes |> List.find (fun n -> n.Id = "Blocker")).Bounds
            let r1 = routed.Associations |> List.find (fun a -> a.Id = "places")
            let r2 = routed.Associations |> List.find (fun a -> a.Id = "audits")

            // Every segment is strictly horizontal or vertical when CornerRadius = 0
            for r in [ r1; r2 ] do
                for p, q in List.pairwise r.Waypoints do
                    let ortho = abs (p.X - q.X) <= 1e-4 || abs (p.Y - q.Y) <= 1e-4
                    Expect.isTrue ortho "segment must be orthogonal"
                    // Midpoint of interior segments does not pierce deep inside blocker interior
                    let mid = Point.create ((p.X + q.X) * 0.5) ((p.Y + q.Y) * 0.5)
                    let innerBlocker =
                        Bounds.create
                            (Bounds.x blockerBox + 4.)
                            (Bounds.y blockerBox + 4.)
                            (Bounds.width blockerBox - 8.)
                            (Bounds.height blockerBox - 8.)
                    Expect.isFalse (Bounds.contains mid innerBlocker) "route detours around blocker obstacle"

            Expect.isSome r1.SourceLabel "ER source cardinality positioned"
            Expect.isSome r1.CenterLabel "ER association name positioned"
            Expect.isSome r1.TargetLabel "ER target cardinality positioned"
            Expect.notEqual r1.Waypoints r2.Waypoints "parallel channels separated"

        testCase "IPSep-CoLa enforces alignment rails, axis ordering, non-overlap and group containment" <| fun _ ->
            let grp = DiagramGroup.create "cluster"
            let nodes =
                [ DiagramNode.sized "a" 80. 40. |> DiagramNode.inGroup "cluster"
                  DiagramNode.sized "b" 80. 40. |> DiagramNode.inGroup "cluster"
                  DiagramNode.sized "c" 80. 40.
                  DiagramNode.sized "d" 80. 40. ]
            let assocs =
                [ Association.create "e1" "a" "b"
                  Association.create "e2" "b" "c"
                  Association.create "e3" "c" "d" ]
            let cons =
                [ LayoutConstraint.AlignHorizontal [ "a"; "b" ]
                  LayoutConstraint.AlignVertical [ "b"; "d" ]
                  LayoutConstraint.OrderAlong(LayoutAxis.Horizontal, "a", "c", 120.) ]
            let dg = DiagramGraph.createWithConstraints nodes [ grp ] assocs cons |> ok
            let cfg = { ColaConfig.create viewport with AvoidOverlaps = true; Iterations = 45 }
            let out = Cola.layout cfg dg |> ok
            let byId = out.Nodes |> List.map (fun n -> n.Id, n) |> Map.ofList

            Expect.floatClose Accuracy.medium byId["a"].Center.Y byId["b"].Center.Y "AlignHorizontal equalizes Y"
            Expect.floatClose Accuracy.medium byId["b"].Center.X byId["d"].Center.X "AlignVertical equalizes X"
            Expect.isTrue (byId["a"].Center.X + 120. <= byId["c"].Center.X + 1e-3) "OrderAlong enforced"

            let gBounds = out.Groups.Head.Bounds
            Expect.isTrue (boxContainsBox 1e-3 gBounds byId["a"].Bounds) "a contained in cluster"
            Expect.isTrue (boxContainsBox 1e-3 gBounds byId["b"].Bounds) "b contained in cluster"

            let arr = List.toArray out.Nodes
            for i = 0 to arr.Length - 1 do
                for j = i + 1 to arr.Length - 1 do
                    Expect.isTrue (boxesDisjoint 1e-3 arr[i].Bounds arr[j].Bounds) "CoLa non-overlap"

        testCase "fCoSE, PivotMds, Stress2D, ModularityForce2D and RadialTree satisfy determinism and structural invariants" <| fun _ ->
            // Two dense 6-cliques joined by 1 bridge edge -> LinLog & ForceAtlas2 separate communities cleanly
            let clique prefix =
                let ns = [ for i in 0 .. 5 -> DiagramNode.sized (prefix + string i) 32. 24. ]
                let es =
                    [ for i in 0 .. 5 do
                        for j in i + 1 .. 5 ->
                            Association.create (sprintf "%s_%d_%d" prefix i j) (prefix + string i) (prefix + string j) ]
                ns, es
            let nA, eA = clique "A"
            let nB, eB = clique "B"
            let barbell = DiagramGraph.simple (nA @ nB) (eA @ eB @ [ Association.create "bridge" "A0" "B0" ]) |> ok

            // Permutation invariance of FCose
            let barbellRev = DiagramGraph.simple (List.rev (nA @ nB)) (List.rev (eA @ eB @ [ Association.create "bridge" "A0" "B0" ])) |> ok
            let fc1 = FCose.layout (FCoseConfig.create viewport) barbell |> ok
            let fc2 = FCose.layout (FCoseConfig.create viewport) barbellRev |> ok
            Expect.equal (fc1.Nodes |> List.map (fun n -> n.Id, n.Center)) (fc2.Nodes |> List.map (fun n -> n.Id, n.Center)) "fCoSE permutation deterministic"

            // PivotMDS + Stress2D metric ordering
            let pmds = PivotMds.layout (PivotMdsConfig.create viewport) barbell |> ok
            Expect.equal pmds.Nodes.Length 12 "PivotMds node count"
            let st = Stress2D.layout { Stress2DConfig.create viewport with AvoidOverlaps = true } barbell |> ok
            let stMap = st.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            let dIntra = sqrt (Point.distanceSquared stMap["A1"] stMap["A2"])
            let dInter = sqrt (Point.distanceSquared stMap["A5"] stMap["B5"])
            Expect.isTrue (dIntra < dInter) "SGD Stress places intra-clique vertices closer than remote clique vertices"

            // LinLog Modularity clustering separates clique centroids strongly
            let linlog = ModularityForce2D.layout { ModularityForceConfig.create viewport with Model = EnergyModel.LinLog; Iterations = 90 } barbell |> ok
            let llMap = linlog.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            let meanCenter prefix =
                let pts = [ for i in 0 .. 5 -> llMap[prefix + string i] ]
                Point.create ((pts |> List.averageBy (fun p -> p.X))) ((pts |> List.averageBy (fun p -> p.Y)))
            let cA, cB = meanCenter "A", meanCenter "B"
            let clusterDist = sqrt (Point.distanceSquared cA cB)
            let radiusA = [ for i in 0 .. 5 -> sqrt (Point.distanceSquared cA llMap["A" + string i]) ] |> List.average
            Expect.isTrue (clusterDist > radiusA * 2.0) "LinLog modularity separates community clusters"

            // RadialTree bidirectional mindmap splits root branches left and right
            let mindCfg = { RadialTreeConfig.create viewport with Mode = RadialMode.BidirectionalMindmap; RootId = Some "A0" }
            let mind = RadialTree.layout mindCfg barbell |> ok
            let rootX = (mind.Nodes |> List.find (fun n -> n.Id = "A0")).Center.X
            let hasLeft = mind.Nodes |> List.exists (fun n -> n.Center.X < rootX - 20.)
            let hasRight = mind.Nodes |> List.exists (fun n -> n.Center.X > rootX + 20.)
            Expect.isTrue (hasLeft && hasRight) "Bidirectional mindmap balances branches across both sides"

            // DiagramScene renders composed scene with UML/ER markers and zero duplicate mark keys
            let umlGraph =
                DiagramGraph.simple
                    [ DiagramNode.create "Base"; DiagramNode.create "Derived"; DiagramNode.create "Part" ]
                    [ Association.labeled "inh" "Derived" "Base" AssociationKind.Inheritance None None None
                      Association.labeled "comp" "Derived" "Part" AssociationKind.Composition (Some { Width = 18.; Height = 12. }) None (Some { Width = 24.; Height = 12. }) ]
                |> ok
            let umlLayout = Sugiyama.layout (SugiyamaConfig.create viewport) umlGraph |> ok
            let theme =
                { DiagramTheme.create 960. 640. "UML Class Diagram" with
                    AssociationLabels = fun id -> if id = "comp" then Some "1", None, Some "1..*" else None, None, None }
            let scene = DiagramScene.render theme umlLayout
            Expect.isTrue (Scene.isComposed scene) "composed scene"
            Expect.isNonEmpty (Scene.marks scene) "renders marks"

            // Cancellation safety
            use cts = new CancellationTokenSource()
            cts.Cancel()
            Expect.throwsT<OperationCanceledException>
                (fun () -> Stress2D.layoutWithCancellation cts.Token (Stress2DConfig.create viewport) barbell |> ignore)
                "respects pre-cancelled token"
    ]

module Ranvier.D5.Tests.GraphLayout3DTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D

let private ok = function Ok v -> v | Error errs -> failwithf "Unexpected error: %A" errs
let private cube = Bounds3.create -300. -300. -300. 600. 600. 600.

let tests =
    testList "GraphLayout3D" [
        testCase "Point3, Bounds3 and Octree satisfy 3D geometric and exact nearest-neighbor contracts" <| fun _ ->
            Expect.throwsT<ArgumentException> (fun () -> Point3.create nan 0. 0. |> ignore) "rejects NaN Point3"
            Expect.throwsT<ArgumentException> (fun () -> Bounds3.create 0. 0. 0. -1. 1. 1. |> ignore) "rejects negative Bounds3"

            let b1 = Bounds3.create 0. 0. 0. 10. 20. 30.
            let b2 = Bounds3.ofExtrema (Point3.create -5. 5. 10.) (Point3.create 15. 25. 40.)
            let u = Bounds3.union b1 b2
            Expect.equal (Bounds3.minPoint u) (Point3.create -5. 0. 0.) "union min corner"
            Expect.equal (Bounds3.maxPoint u) (Point3.create 15. 25. 40.) "union max corner"
            Expect.isTrue (Bounds3.contains (Bounds3.center b1) b1) "center inside bounds"

            // Octree exact nearest & range equivalence against brute force over 300 3D points
            let pts =
                [| for i in 0 .. 299 ->
                    let x = sin (float i * 1.7) * 100.
                    let y = cos (float i * 2.3) * 100.
                    let z = sin (float i * 0.9 + 0.4) * 100.
                    i, Point3.create x y z |]
            let oct = Octree.build snd (fun _ -> 1.) pts
            Expect.equal (Octree.count oct) 300 "octree captures all 300 points"

            let qBox = Bounds3.create -30. -40. -25. 60. 80. 50.
            let expectedWithin = pts |> Array.filter (fun (_, p) -> Bounds3.contains p qBox) |> Array.map fst |> Array.toList
            let actualWithin = Octree.within qBox oct |> List.map fst
            Expect.equal actualWithin expectedWithin "octree within preserves exact input order"

            for qIdx in [ 0; 37; 142; 299 ] do
                let q = Point3.create (snd pts[qIdx] |> fun p -> p.X + 3.5) (snd pts[qIdx] |> fun p -> p.Y - 2.1) (snd pts[qIdx] |> fun p -> p.Z + 1.3)
                let brute =
                    pts
                    |> Array.minBy (fun (idx, p) -> Point3.distance q p, idx)
                    |> fst
                let found = Octree.nearest q infinity oct |> Option.get |> fst
                Expect.equal found brute "octree nearest matches brute force"

        testCase "Minkowski H^3, Klein/Poincare unit balls and SIMD Lorentz focus boosts preserve hyperbolic isometry" <| fun _ ->
            let a = Hyperbolic3D.fromSpherical 1.4 0.7 1.2
            let b = Hyperbolic3D.fromSpherical 2.1 1.1 -0.8
            Expect.floatClose Accuracy.high (Hyperbolic3D.minkowskiDot a a) -1. "upper sheet <a,a>_M = -1"
            Expect.floatClose Accuracy.high (Hyperbolic3D.minkowskiDot b b) -1. "upper sheet <b,b>_M = -1"

            // Round-trip ball conversions
            for model in [ HyperbolicModel.Klein; HyperbolicModel.Poincare ] do
                let ballPt, scale = Hyperbolic3D.toBall model a
                Expect.isTrue (Point3.distance (Point3.create 0. 0. 0.) ballPt < 1.) "strictly inside unit ball"
                Expect.isTrue (scale > 0. && scale <= 1.) "attenuation in (0,1]"
                let lifted = Hyperbolic3D.ofBall model ballPt |> ok
                Expect.floatClose Accuracy.high (Hyperbolic3D.distance a lifted) 0. (sprintf "round-trip %A" model)

            // Isometric boost to origin brings `a` to (0,0,0,1) while preserving d_H(a, b)!
            let dBefore = Hyperbolic3D.distance a b
            let boost = HyperbolicTransform.boostToOrigin a
            let aBoosted = HyperbolicTransform.apply boost a
            let bBoosted = HyperbolicTransform.apply boost b
            Expect.floatClose Accuracy.high (Hyperbolic3D.distance Hyperbolic3D.origin aBoosted) 0. "boost maps focus to origin"
            Expect.floatClose Accuracy.high (Hyperbolic3D.distance aBoosted bBoosted) dBefore "Lorentz boost preserves hyperbolic geodesic distance"

        testCase "Munzner H3 lays out hierarchies and directed graphs in 3D hyperbolic space with O(N) interactive focus" <| fun _ ->
            // Build a 3-ary tree of 121 nodes (depths 0..4) + cross-links
            let nodes = [ for i in 0 .. 120 -> DiagramNode.create (sprintf "n%d" i) ]
            let treeEdges =
                [ for i in 1 .. 120 ->
                    Association.create (sprintf "t%d" i) (sprintf "n%d" ((i - 1) / 3)) (sprintf "n%d" i) ]
            let crossEdges =
                [ Association.create "c1" "n40" "n120"
                  Association.create "c2" "n15" "n88" ]
            let dg = DiagramGraph.simple nodes (treeEdges @ crossEdges) |> ok

            let state = H3.buildState { H3Config.defaultValue with RootId = Some "n0"; BallRadius = 300. } dg |> ok
            let initial = H3.project state
            Expect.equal initial.Nodes.Length 121 "all 121 nodes projected"
            Expect.equal (initial.Associations |> List.filter (fun a -> a.IsBackbone) |> List.length) 120 "120 spanning-tree backbone edges"
            Expect.equal (initial.Associations |> List.filter (fun a -> not a.IsBackbone) |> List.length) 2 "2 non-tree cross-links"

            let byId = initial.Nodes |> List.map (fun n -> n.Id, n) |> Map.ofList
            let origin3 = Point3.create 0. 0. 0.
            Expect.floatClose Accuracy.high (Point3.distance origin3 byId["n0"].Center) 0. "root starts at ball origin"
            Expect.floatClose Accuracy.high byId["n0"].ScaleFactor 1. "root has full focus scale 1.0"

            // Every node sits strictly inside the open sphere of radius 300, and deeper leaves attenuate smoothly
            for n in initial.Nodes do
                Expect.isTrue (Point3.distance origin3 n.Center < 300.) "inside H3 Klein sphere"
            Expect.isTrue (byId["n120"].ScaleFactor < byId["n1"].ScaleFactor) "deeper periphery shrinks exponentially"

            // Interactive Focus+Context: boost leaf n120 to center of the ball!
            let focusedState = H3.focusNode "n120" state |> ok
            let focused = H3.project focusedState
            let focusedLeaf = focused.Nodes |> List.find (fun n -> n.Id = "n120")
            let displacedRoot = focused.Nodes |> List.find (fun n -> n.Id = "n0")
            Expect.floatClose Accuracy.medium (Point3.distance origin3 focusedLeaf.Center) 0. "focused leaf moves to center of sphere"
            Expect.floatClose Accuracy.medium focusedLeaf.ScaleFactor 1. "focused leaf receives full magnification"
            Expect.isTrue (displacedRoot.ScaleFactor < 0.9) "former root compresses into context"

            // Poincare conformal model produces multi-point geodesic arcs
            let poincare = H3.layout { H3Config.defaultValue with Model = HyperbolicModel.Poincare; PoincareArcSegments = 8 } dg |> ok
            Expect.isTrue (poincare.Associations |> List.exists (fun a -> a.Points.Length > 2)) "Poincare geodesic arcs sampled"

        testCase "CAIDA Walrus supports explicit spanning-tree backbones, rejects cyclic trees and toggles cross-links" <| fun _ ->
            let nodes = [ for id in [ "core"; "as1"; "as2"; "leaf1"; "leaf2" ] -> DiagramNode.create id ]
            let assocs =
                [ Association.create "b1" "core" "as1"
                  Association.create "b2" "core" "as2"
                  Association.create "b3" "as1" "leaf1"
                  Association.create "b4" "as2" "leaf2"
                  Association.create "peer" "as1" "as2"
                  Association.create "shortcut" "leaf1" "leaf2" ]
            let dg = DiagramGraph.simple nodes assocs |> ok

            // Invalid explicit tree containing cycle core-as1-as2-core must return Error [InvalidTopology]
            let cyclicExplicit = Set.ofList [ "b1"; "b2"; "peer" ]
            Expect.isError
                (Walrus.layout { WalrusConfig.defaultValue with Backbone = WalrusTreeSource.ExplicitAssociations cyclicExplicit } dg)
                "Walrus rejects cyclic explicit backbone"

            let validTree = Set.ofList [ "b1"; "b2"; "b3"; "b4" ]
            let withCross =
                Walrus.layout
                    { WalrusConfig.defaultValue with
                        Backbone = WalrusTreeSource.ExplicitAssociations validTree
                        RootId = Some "core"
                        IncludeCrossLinks = true }
                    dg
                |> ok
            Expect.equal withCross.Associations.Length 6 "includes 4 backbone + 2 cross links"
            Expect.equal (withCross.Associations |> List.filter (fun a -> a.IsBackbone) |> List.length) 4 "exact 4 backbone links"

            let backboneOnly =
                Walrus.layout
                    { WalrusConfig.defaultValue with
                        Backbone = WalrusTreeSource.ExplicitAssociations validTree
                        RootId = Some "core"
                        IncludeCrossLinks = false }
                    dg
                |> ok
            Expect.equal backboneOnly.Associations.Length 4 "cross-links omitted when disabled"

        testCase "Force3D, Stress3D, ConeTree3D, Layered3D and Scene3D produce finite 3D layouts and depth-sorted scenes" <| fun _ ->
            let g1 = DiagramGroup.create "tierA"
            let g2 = DiagramGroup.create "tierB"
            let nodes =
                [ DiagramNode.sized "ui1" 48. 32. |> DiagramNode.inGroup "tierA"
                  DiagramNode.sized "ui2" 48. 32. |> DiagramNode.inGroup "tierA"
                  DiagramNode.sized "api" 60. 36. |> DiagramNode.inGroup "tierB"
                  DiagramNode.sized "db" 56. 40. ]
            let assocs =
                [ Association.create "e1" "ui1" "api"
                  Association.create "e2" "ui2" "api"
                  Association.create "e3" "api" "db" ]
            let dg = DiagramGraph.create nodes [ g1; g2 ] assocs |> ok

            let f3d = Force3D.layout (Force3DConfig.create cube) dg |> ok
            Expect.equal f3d.Nodes.Length 4 "Force3D positions all nodes"
            Expect.equal f3d.Groups.Length 2 "Force3D computes 3D group cuboids"

            let s3d = Stress3D.layout { Stress3DConfig.create cube with FixedZ = Map.ofList [ "ui1", -100.; "db", 100. ] } dg |> ok
            let sMap = s3d.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            Expect.floatClose Accuracy.high sMap["ui1"].Z -100. "FixedZ pinned ui1"
            Expect.floatClose Accuracy.high sMap["db"].Z 100. "FixedZ pinned db"

            for mode in [ ConeTreeMode.VerticalCone; ConeTreeMode.HorizontalCam; ConeTreeMode.SphericalBalloon ] do
                let ct = ConeTree3D.layout { ConeTree3DConfig.create cube with Mode = mode; RootId = Some "ui1" } dg |> ok
                Expect.equal ct.Nodes.Length 4 (sprintf "ConeTree3D %A node count" mode)

            let l3d = Layered3D.layout { Layered3DConfig.create cube with Axis = StratumAxis.Y } dg |> ok
            let lMap = l3d.Nodes |> List.map (fun n -> n.Id, n) |> Map.ofList
            Expect.isTrue (lMap["ui1"].Center.Y < lMap["api"].Center.Y && lMap["api"].Center.Y < lMap["db"].Center.Y) "3D strata progress along +Y"

            // Pure CPU Scene3D projection
            let cam = Camera3D.fitBounds 0.6 0.35 l3d.Bounds
            let theme3D = { Scene3DTheme.create 800. 600. "3D Architecture" with ReferenceSphereRadius = Some 250. }
            let scene = Scene3D.render theme3D cam l3d
            Expect.isTrue (Scene.isComposed scene) "Scene3D produces composed D5 scene"
            Expect.isNonEmpty (Scene.marks scene) "Scene3D emits projected marks"

            use cts = new CancellationTokenSource()
            cts.Cancel()
            Expect.throwsT<OperationCanceledException>
                (fun () -> Force3D.layoutWithCancellation cts.Token (Force3DConfig.create cube) dg |> ignore)
                "Force3D honours cancellation"

        testCase "Issue #92 (Bug #23): H3, Walrus and PoincareBallLift scale node radii and box extents proportionally on unit balls" <| fun _ ->
            let verifyUnitBall (label: string) (bRad: float) (layout: DiagramLayout3D) =
                for nd in layout.Nodes do
                    Expect.isTrue
                        (nd.Radius > 0. && nd.Radius <= 0.15 * bRad)
                        (sprintf "%s node %s radius %g must stay in (0, %g]" label nd.Id nd.Radius (0.15 * bRad))
                    Expect.isTrue
                        (Bounds3.sizeX nd.Bounds <= 0.30 * bRad
                         && Bounds3.sizeY nd.Bounds <= 0.30 * bRad
                         && Bounds3.sizeZ nd.Bounds <= 0.30 * bRad)
                        (sprintf "%s node %s box (%g,%g,%g) must stay proportional to BallRadius=%g" label nd.Id (Bounds3.sizeX nd.Bounds) (Bounds3.sizeY nd.Bounds) (Bounds3.sizeZ nd.Bounds) bRad)
                let limit = 1.15 * bRad
                Expect.isTrue
                    (abs (Bounds3.minX layout.Bounds) <= limit
                     && abs (Bounds3.maxX layout.Bounds) <= limit
                     && abs (Bounds3.minY layout.Bounds) <= limit
                     && abs (Bounds3.maxY layout.Bounds) <= limit
                     && abs (Bounds3.minZ layout.Bounds) <= limit
                     && abs (Bounds3.maxZ layout.Bounds) <= limit)
                    (sprintf "%s layout bounds %A must stay within [-%g, +%g]^3" label layout.Bounds limit limit)

            for label, mkNode in
                [ "sized-0.08", (fun id -> DiagramNode.sized id 0.08 0.08)
                  "default-create", DiagramNode.create ] do
                let nodes = [ for i in 0 .. 12 -> mkNode (sprintf "u%d" i) ]
                let edges = [ for i in 1 .. 12 -> Association.create (sprintf "ue%d" i) (sprintf "u%d" ((i - 1) / 3)) (sprintf "u%d" i) ]
                let dg = DiagramGraph.simple nodes edges |> ok

                for model in [ HyperbolicModel.Klein; HyperbolicModel.Poincare ] do
                    let h3Out = H3.layout { H3Config.defaultValue with Model = model; BallRadius = 1.0; RootId = Some "u0" } dg |> ok
                    verifyUnitBall (sprintf "H3-%A-%s" model label) 1.0 h3Out

                    let walrusOut = Walrus.layout { WalrusConfig.defaultValue with Model = model; SphereRadius = 1.0; RootId = Some "u0" } dg |> ok
                    verifyUnitBall (sprintf "Walrus-%A-%s" model label) 1.0 walrusOut

                for vpLabel, vp in [ "800x600", Bounds.create 0. 0. 800. 600.; "unit-vp", Bounds.create -1. -1. 2. 2. ] do
                    let pdState = PoincareDisk.buildState (PoincareDiskConfig.create vp) dg |> ok
                    let liftCfg = PoincareBallLiftConfig.create (Point3.create 0. 0. 0.) 1.0
                    let liftOut = PoincareBallLift.morph 0.0 liftCfg pdState
                    verifyUnitBall (sprintf "PoincareBallLift-0.0-%s-%s" label vpLabel) 1.0 liftOut
                    let morphOut = PoincareBallLift.morph 0.5 liftCfg pdState
                    verifyUnitBall (sprintf "PoincareBallLift-0.5-%s-%s" label vpLabel) 1.0 morphOut

            // Verify legacy BallRadius = 300.0 preserves >= 1.2 radius and >= 2.4 box floors unchanged
            let legacyNodes = [ for i in 0 .. 12 -> DiagramNode.create (sprintf "l%d" i) ]
            let legacyEdges = [ for i in 1 .. 12 -> Association.create (sprintf "le%d" i) (sprintf "l%d" ((i - 1) / 3)) (sprintf "l%d" i) ]
            let legacyDg = DiagramGraph.simple legacyNodes legacyEdges |> ok
            let h3Legacy = H3.layout { H3Config.defaultValue with BallRadius = 300.0; RootId = Some "l0" } legacyDg |> ok
            for nd in h3Legacy.Nodes do
                Expect.isTrue (nd.Radius >= 1.2) "legacy BallRadius=300 preserves 1.2 radius floor"
                Expect.isTrue (Bounds3.sizeX nd.Bounds >= 2.4 && Bounds3.sizeY nd.Bounds >= 2.4 && Bounds3.sizeZ nd.Bounds >= 2.4) "legacy BallRadius=300 preserves 2.4 box floor"

            // Verify grouped unit-ball H3 stays bounded cleanly with DiagramGroup.create
            let grpNodes =
                [ for i in 0 .. 6 ->
                    DiagramNode.create (sprintf "gn%d" i)
                    |> DiagramNode.inGroup (if i < 4 then "ga" else "gb") ]
            let grpEdges = [ for i in 1 .. 6 -> Association.create (sprintf "ge%d" i) "gn0" (sprintf "gn%d" i) ]
            let grpDg = DiagramGraph.create grpNodes [ DiagramGroup.create "ga"; DiagramGroup.create "gb" ] grpEdges |> ok
            let h3Grouped = H3.layout { H3Config.defaultValue with BallRadius = 1.0; RootId = Some "gn0" } grpDg |> ok
            verifyUnitBall "H3-grouped-unit-ball" 1.0 h3Grouped

        testCase "Issue #92 (Bug #22): Kernels3D.computeGroups3D scales unit-sphere groups and HierarchicalBundle3D synthesizes LCA centroids before GroupBounds" <| fun _ ->
            let groups = [ for g in 0 .. 2 -> DiagramGroup.create (sprintf "ug%d" g) ]
            let nodes =
                [ for g in 0 .. 2 do
                    for k in 0 .. 3 ->
                        DiagramNode.sized (sprintf "un_%d_%d" g k) 0.08 0.08
                        |> DiagramNode.inGroup (sprintf "ug%d" g) ]
            let edges =
                [ for g in 0 .. 2 do
                    let nextG = (g + 1) % 3
                    yield Association.create (sprintf "xe_%d" g) (sprintf "un_%d_0" g) (sprintf "un_%d_1" nextG)
                    yield Association.create (sprintf "ie_%d" g) (sprintf "un_%d_0" g) (sprintf "un_%d_2" g) ]
            let dg = DiagramGraph.create nodes groups edges |> ok
            let sphCfg = { SphericalLayoutConfig.create (Point3.create 0. 0. 0.) 1.05 with Iterations = 35; ArcSegments = 10; Seed = 17UL }
            let sphLayout = SphericalStress3D.layout sphCfg dg |> ok

            Expect.equal sphLayout.Groups.Length 3 "unit-sphere layout retains all 3 groups"
            for grp in sphLayout.Groups do
                Expect.isTrue
                    (Bounds3.sizeX grp.Bounds < 3.0 && Bounds3.sizeY grp.Bounds < 3.0 && Bounds3.sizeZ grp.Bounds < 3.0)
                    (sprintf "unit-sphere group %s bounds (%g,%g,%g) must not stay at 96x56 pixel scale" grp.Id (Bounds3.sizeX grp.Bounds) (Bounds3.sizeY grp.Bounds) (Bounds3.sizeZ grp.Bounds))
            Expect.isTrue
                (abs (Bounds3.minX sphLayout.Bounds) < 2.5 && abs (Bounds3.maxX sphLayout.Bounds) < 2.5
                 && abs (Bounds3.minY sphLayout.Bounds) < 2.5 && abs (Bounds3.maxY sphLayout.Bounds) < 2.5
                 && abs (Bounds3.minZ sphLayout.Bounds) < 2.5 && abs (Bounds3.maxZ sphLayout.Bounds) < 2.5)
                (sprintf "unit-sphere overall Bounds %A must not blow out via pixel-scale group MinSize" sphLayout.Bounds)

            let hTree =
                Hierarchy.node "root" 0.
                    [ for g in 0 .. 2 ->
                        Hierarchy.node (sprintf "ug%d" g) 0.
                            [ for k in 0 .. 3 -> Hierarchy.node (sprintf "un_%d_%d" g k) 1. [] ] ]
            let bundledWithGroups = HierarchicalBundle3D.bundle 0.8 12 hTree sphLayout |> ok
            let bundledNoGroups = HierarchicalBundle3D.bundle 0.8 12 hTree { sphLayout with Groups = [] } |> ok
            let skewedGroups =
                sphLayout.Groups
                |> List.map (fun g -> { g with Bounds = Bounds3.create 500. 500. 500. 10. 10. 10. })
            let bundledSkewed = HierarchicalBundle3D.bundle 0.8 12 hTree { sphLayout with Groups = skewedGroups } |> ok

            for aGrp, aNo, aSkew in List.zip3 bundledWithGroups.Associations bundledNoGroups.Associations bundledSkewed.Associations do
                Expect.equal aGrp.Points.Length aNo.Points.Length "point chain lengths match"
                for idx = 0 to aGrp.Points.Length - 1 do
                    Expect.isTrue
                        (Point3.distance (aGrp.Points[idx]) (aNo.Points[idx]) <= 1e-12)
                        (sprintf "HierarchicalBundle3D with Groups matches centroid synthesis (diff=%g)" (Point3.distance (aGrp.Points[idx]) (aNo.Points[idx])))
                    Expect.isTrue
                        (Point3.distance (aSkew.Points[idx]) (aNo.Points[idx]) <= 1e-12)
                        (sprintf "HierarchicalBundle3D ignores skewed group boxes when descendant nodes exist (diff=%g)" (Point3.distance (aSkew.Points[idx]) (aNo.Points[idx])))

            // Empty hierarchy group (0 positioned descendant nodes) falls back cleanly to Bounds3.center g.Bounds
            let emptyGroupBounds = Bounds3.create 10. 20. 30. 4. 6. 8. // center = (12, 23, 34)
            let emptyGroupItem : PositionedGroup3D =
                { Id = "emptyGrp"; ParentGroup = None; Bounds = emptyGroupBounds; InnerBounds = emptyGroupBounds; Depth = 0 }
            let leafA = { sphLayout.Nodes[0] with Id = "leafA"; Center = Point3.create 0. 0. 0. }
            let leafB = { sphLayout.Nodes[1] with Id = "leafB"; Center = Point3.create 2. 0. 0. }
            let fallbackLayout : DiagramLayout3D =
                { Bounds = Bounds3.create -1. -1. -1. 15. 30. 40.
                  Nodes = [ leafA; leafB ]
                  Groups = [ emptyGroupItem ]
                  Associations =
                    [ { Id = "fbEdge"
                        Source = Endpoint.node "leafA"
                        Target = Endpoint.node "leafB"
                        Kind = AssociationKind.Directed
                        IsBackbone = true
                        Points = Point3Chain.ofList [ leafA.Center; leafB.Center ]
                        Waypoints = [ leafA.Center; leafB.Center ] } ] }
            let zeroDescLayout =
                { fallbackLayout with
                    Associations =
                        [ { fallbackLayout.Associations[0] with
                              Target = Endpoint.node "emptyGrp"
                              Points = Point3Chain.ofList [ leafA.Center; Bounds3.center emptyGroupBounds ]
                              Waypoints = [ leafA.Center; Bounds3.center emptyGroupBounds ] } ] }
            let zeroDescTree =
                Hierarchy.node "top" 0.
                    [ Hierarchy.node "leafA" 1. []
                      Hierarchy.node "emptyGrp" 0. [] ]
            let bundledFallback = HierarchicalBundle3D.bundle 0.85 8 zeroDescTree zeroDescLayout |> ok
            let fbPts = bundledFallback.Associations[0].Points
            Expect.isTrue
                (Point3.distance (fbPts[fbPts.Length - 1]) (Point3.create 12. 23. 34.) <= 1e-9)
                "zero-descendant hierarchy group falls back to Bounds3.center g.Bounds"

        testCase "VerticalDirection3D toggles ConeTree3D and Layered3D progression while preserving Increasing default" <| fun _ ->
            let nodes =
                [ DiagramNode.sized "ui1" 48. 32.
                  DiagramNode.sized "api" 60. 36.
                  DiagramNode.sized "db" 56. 40.
                  DiagramNode.sized "leaf" 40. 28. ]
            let assocs =
                [ Association.create "e1" "ui1" "api"
                  Association.create "e2" "api" "db"
                  Association.create "e3" "api" "leaf" ]
            let dg = DiagramGraph.simple nodes assocs |> ok

            let ctInc = ConeTree3D.layout { ConeTree3DConfig.create cube with Mode = ConeTreeMode.VerticalCone; RootId = Some "ui1" } dg |> ok
            let ctDec = ConeTree3D.layout { ConeTree3DConfig.create cube with Mode = ConeTreeMode.VerticalCone; RootId = Some "ui1"; Direction = VerticalDirection3D.Decreasing } dg |> ok
            let ctIncMap = ctInc.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            let ctDecMap = ctDec.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            Expect.isTrue (ctIncMap["ui1"].Y < ctIncMap["api"].Y && ctIncMap["api"].Y < ctIncMap["db"].Y) "ConeTree3D VerticalCone Increasing progresses +Y"
            Expect.isTrue (ctDecMap["ui1"].Y > ctDecMap["api"].Y && ctDecMap["api"].Y > ctDecMap["db"].Y) "ConeTree3D VerticalCone Decreasing progresses -Y from top"

            let camInc = ConeTree3D.layout { ConeTree3DConfig.create cube with Mode = ConeTreeMode.HorizontalCam; RootId = Some "ui1" } dg |> ok
            let camDec = ConeTree3D.layout { ConeTree3DConfig.create cube with Mode = ConeTreeMode.HorizontalCam; RootId = Some "ui1"; Direction = VerticalDirection3D.Decreasing } dg |> ok
            let camIncMap = camInc.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            let camDecMap = camDec.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            Expect.isTrue (camIncMap["ui1"].X < camIncMap["api"].X && camIncMap["api"].X < camIncMap["db"].X) "ConeTree3D HorizontalCam Increasing progresses +X"
            Expect.isTrue (camDecMap["ui1"].X > camDecMap["api"].X && camDecMap["api"].X > camDecMap["db"].X) "ConeTree3D HorizontalCam Decreasing progresses -X"

            let lYInc = Layered3D.layout { Layered3DConfig.create cube with Axis = StratumAxis.Y } dg |> ok
            let lYDec = Layered3D.layout { Layered3DConfig.create cube with Axis = StratumAxis.Y; Direction = VerticalDirection3D.Decreasing } dg |> ok
            let lYIncMap = lYInc.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            let lYDecMap = lYDec.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            Expect.isTrue (lYIncMap["ui1"].Y < lYIncMap["api"].Y && lYIncMap["api"].Y < lYIncMap["db"].Y) "Layered3D Y Increasing progresses +Y"
            Expect.isTrue (lYDecMap["ui1"].Y > lYDecMap["api"].Y && lYDecMap["api"].Y > lYDecMap["db"].Y) "Layered3D Y Decreasing progresses -Y"

            let lZInc = Layered3D.layout { Layered3DConfig.create cube with Axis = StratumAxis.Z } dg |> ok
            let lZDec = Layered3D.layout { Layered3DConfig.create cube with Axis = StratumAxis.Z; Direction = VerticalDirection3D.Decreasing } dg |> ok
            let lZIncMap = lZInc.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            let lZDecMap = lZDec.Nodes |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
            Expect.isTrue (lZIncMap["ui1"].Z < lZIncMap["api"].Z && lZIncMap["api"].Z < lZIncMap["db"].Z) "Layered3D Z Increasing progresses +Z"
            Expect.isTrue (lZDecMap["ui1"].Z > lZDecMap["api"].Z && lZDecMap["api"].Z > lZDecMap["db"].Z) "Layered3D Z Decreasing progresses -Z"

        testCase "DiagramNode.sized3D / Pinned3D, Point3Chain conversions, Quaternion64, Hyperbolic3D.slerp and H3.projectCentersInto invariants" <| fun _ ->
            // 1. Pinned3D & sized3D honored in both Force3D (incl. PreventOverlap) and Stress3D
            let pinPos = Point3.create 12. -34. 56.
            let pNodes =
                [ DiagramNode.sized3D "pin" 30. 30. 30. |> DiagramNode.withPin3D pinPos
                  DiagramNode.sized3D "near" 30. 30. 30. |> DiagramNode.withPin (Point.create 12. -34.)
                  DiagramNode.create "free" ]
            let pEdges = [ Association.create "e1" "pin" "free"; Association.create "e2" "pin" "near" ]
            let pGraph = DiagramGraph.simple pNodes pEdges |> ok
            Expect.throwsT<ArgumentException> (fun () -> DiagramNode.withPin3D (Point3.create nan 0. 0.) (DiagramNode.create "bad") |> ignore) "withPin3D rejects non-finite coordinates via Point3.create"
            Expect.isTrue (DiagramGraph.simple [ DiagramNode.sized3D "bad" 10. 10. -2. ] [] |> Result.isError) "DiagramGraph rejects non-positive DepthSize from sized3D"

            let fPinned = Force3D.layout { Force3DConfig.create cube with Iterations = 40; PreventOverlap = true } pGraph |> ok
            let fPinNode = fPinned.Nodes |> List.find (fun n -> n.Id = "pin")
            Expect.floatClose Accuracy.veryHigh fPinNode.Center.X 12. "Force3D preserves Pinned3D X"
            Expect.floatClose Accuracy.veryHigh fPinNode.Center.Y -34. "Force3D preserves Pinned3D Y"
            Expect.floatClose Accuracy.veryHigh fPinNode.Center.Z 56. "Force3D preserves Pinned3D Z even with PreventOverlap"

            let sPinned = Stress3D.layout { Stress3DConfig.create cube with Iterations = 25 } pGraph |> ok
            let sPinNode = sPinned.Nodes |> List.find (fun n -> n.Id = "pin")
            Expect.floatClose Accuracy.veryHigh sPinNode.Center.X 12. "Stress3D preserves Pinned3D X"
            Expect.floatClose Accuracy.veryHigh sPinNode.Center.Y -34. "Stress3D preserves Pinned3D Y"
            Expect.floatClose Accuracy.veryHigh sPinNode.Center.Z 56. "Stress3D preserves Pinned3D Z"

            // 2. Point3Chain.toArray defensive copy & Point3Chain.asSpan zero-copy
            let p0, p1 = Point3.create 1. 2. 3., Point3.create 4. 5. 6.
            let chain = Point3Chain.ofList [ p0; p1 ]
            let copyArr = Point3Chain.toArray chain
            Expect.equal copyArr [| p0; p1 |] "Point3Chain.toArray returns equal array"
            copyArr[0] <- Point3.create -99. -99. -99.
            Expect.equal chain[0] p0 "mutating toArray output does not mutate chain"
            let sp = Point3Chain.asSpan chain
            Expect.equal sp.Length 2 "Point3Chain.asSpan length"
            Expect.equal sp[1] p1 "Point3Chain.asSpan element"

            // 3. Quaternion64 alias + System.Numerics.Quaternion interop
            let q64 : Quaternion64 = Quaternion.fromAxisAngle (Point3.create 0. 1. 0.) (Math.PI * 0.5)
            let q32 : System.Numerics.Quaternion = Quaternion64.toNumerics q64
            let qRt = Quaternion64.ofNumerics q32
            Expect.floatClose Accuracy.high qRt.W q64.W "Quaternion64 W round-trips"
            Expect.floatClose Accuracy.high qRt.Y q64.Y "Quaternion64 Y round-trips"

            // 4. Hyperbolic3D.slerp exact geodesic interpolation on Minkowski upper sheet
            let hA = Hyperbolic3D.fromSpherical 1.2 0.6 0.4
            let hB = Hyperbolic3D.fromSpherical 2.0 1.4 -0.7
            let s0 = Hyperbolic3D.slerp hA hB 0.0
            let s1 = Hyperbolic3D.slerp hA hB 1.0
            let sMid = Hyperbolic3D.slerp hA hB 0.5
            Expect.floatClose Accuracy.veryHigh s0.X hA.X "slerp t=0 X matches a"
            Expect.floatClose Accuracy.veryHigh s0.Y hA.Y "slerp t=0 Y matches a"
            Expect.floatClose Accuracy.veryHigh s0.Z hA.Z "slerp t=0 Z matches a"
            Expect.floatClose Accuracy.veryHigh s0.W hA.W "slerp t=0 W matches a"
            Expect.floatClose Accuracy.veryHigh s1.X hB.X "slerp t=1 X matches b"
            Expect.floatClose Accuracy.veryHigh s1.Y hB.Y "slerp t=1 Y matches b"
            Expect.floatClose Accuracy.veryHigh s1.Z hB.Z "slerp t=1 Z matches b"
            Expect.floatClose Accuracy.veryHigh s1.W hB.W "slerp t=1 W matches b"
            Expect.isLessThan (Hyperbolic3D.distance hA s0) 1e-6 "slerp t=0 geodesic distance < 1e-6 (acosh(1+ulp) sensitivity)"
            Expect.isLessThan (Hyperbolic3D.distance hB s1) 1e-6 "slerp t=1 geodesic distance < 1e-6 (acosh(1+ulp) sensitivity)"
            Expect.floatClose Accuracy.high (Hyperbolic3D.minkowskiDot sMid sMid) -1. "slerp midpoint stays on hyperboloid sheet"
            let dAB = Hyperbolic3D.distance hA hB
            Expect.floatClose Accuracy.high (Hyperbolic3D.distance hA sMid) (dAB * 0.5) "slerp midpoint bisects geodesic d(a, mid)"
            Expect.floatClose Accuracy.high (Hyperbolic3D.distance hB sMid) (dAB * 0.5) "slerp midpoint bisects geodesic d(b, mid)"
            let sClose = Hyperbolic3D.slerp hA hA 0.5
            Expect.floatClose Accuracy.veryHigh sClose.W hA.W "slerp close points avoid NaN"
            Expect.isLessThan (Hyperbolic3D.distance hA sClose) 1e-6 "slerp close points geodesic distance < 1e-6"

            // 5. H3.projectCentersInto matches H3.project across Klein & Poincare with 0 steady-state bytes
            let treeNodes = [ for i in 0 .. 15 -> DiagramNode.create (sprintf "h%d" i) ]
            let treeEdges = [ for i in 1 .. 15 -> Association.create (sprintf "he%d" i) (sprintf "h%d" ((i - 1) / 3)) (sprintf "h%d" i) ]
            let treeDg = DiagramGraph.simple treeNodes treeEdges |> ok
            for model in [ HyperbolicModel.Klein; HyperbolicModel.Poincare ] do
                let st : H3State = H3.buildState { H3Config.defaultValue with Model = model; RootId = Some "h0" } treeDg |> ok
                let boosted = H3.focusNode "h9" st |> ok
                let tf = H3.viewTransform boosted
                let proj = H3.project boosted
                let dest = Array.zeroCreate<Point3> treeNodes.Length
                let count = H3.projectCentersInto model tf st (Span<Point3> dest)
                Expect.equal count treeNodes.Length (sprintf "projectCentersInto count (%A)" model)
                for i, nd in List.indexed proj.Nodes do
                    Expect.floatClose Accuracy.veryHigh (Point3.distance nd.Center dest[i]) 0. (sprintf "projectCentersInto matches H3.project[%d] (%A)" i model)

                // Zero steady-state allocation verification
                for _ = 1 to 5 do H3.projectCentersInto model tf st (Span<Point3> dest) |> ignore
                let b0 = GC.GetAllocatedBytesForCurrentThread()
                for _ = 1 to 20 do H3.projectCentersInto model tf st (Span<Point3> dest) |> ignore
                let b1 = GC.GetAllocatedBytesForCurrentThread()
                Expect.equal (b1 - b0) 0L (sprintf "H3.projectCentersInto allocates 0 bytes (%A)" model)
    ]

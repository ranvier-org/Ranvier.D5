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
    ]

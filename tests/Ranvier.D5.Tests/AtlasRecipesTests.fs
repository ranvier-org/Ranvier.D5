module Ranvier.D5.Tests.AtlasRecipesTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D

let private ok =
    function
    | Ok v -> v
    | Error errs -> failwithf "Unexpected error: %A" errs

let private viewport = Bounds.create 0. 0. 800. 600.

let private dist2D (a: Point) (b: Point) =
    sqrt (Point.distanceSquared a b)

let private boxesStrictlyOverlap (tol: float) (a: Bounds) (b: Bounds) =
    Bounds.x a + Bounds.width a > Bounds.x b + tol
    && Bounds.x b + Bounds.width b > Bounds.x a + tol
    && Bounds.y a + Bounds.height a > Bounds.y b + tol
    && Bounds.y b + Bounds.height b > Bounds.y a + tol

let private boxInside (tol: float) (outer: Bounds) (inner: Bounds) =
    Bounds.x inner >= Bounds.x outer - tol
    && Bounds.y inner >= Bounds.y outer - tol
    && Bounds.x inner + Bounds.width inner <= Bounds.x outer + Bounds.width outer + tol
    && Bounds.y inner + Bounds.height inner <= Bounds.y outer + Bounds.height outer + tol

let private circumcircle (a: Point) (b: Point) (c: Point) : (Point * float) option =
    let d = 2. * (a.X * (b.Y - c.Y) + b.X * (c.Y - a.Y) + c.X * (a.Y - b.Y))
    if abs d <= 1e-9 then
        None
    else
        let a2 = a.X * a.X + a.Y * a.Y
        let b2 = b.X * b.X + b.Y * b.Y
        let c2 = c.X * c.X + c.Y * c.Y
        let ux = (a2 * (b.Y - c.Y) + b2 * (c.Y - a.Y) + c2 * (a.Y - b.Y)) / d
        let uy = (a2 * (c.X - b.X) + b2 * (a.X - c.X) + c2 * (b.X - a.X)) / d
        let center = Point.create ux uy
        Some(center, dist2D center a)

/// 4-tier ecosystem dependency graph (`libc`, `zlib`, `crypto` <- `http`, `tls`, `xml` <- `cli`, `server` <- `suite`)
/// plus mutual recursion cycle `parseA <-> parseB`.
let private buildEcosystemCyclicDag () =
    let ids =
        [ "libc"; "zlib"; "crypto"
          "http"; "tls"; "xml"; "parseA"; "parseB"
          "cli"; "server"
          "suite" ]
    let nodes = ids |> List.map (fun id -> DiagramNode.sized id 28. 18.)
    let edges =
        [ Association.create "e01" "suite" "cli"
          Association.create "e02" "suite" "server"
          Association.create "e03" "cli" "http"
          Association.create "e04" "cli" "xml"
          Association.create "e05" "server" "http"
          Association.create "e06" "server" "tls"
          Association.create "e07" "server" "xml"
          Association.create "e08" "xml" "parseA"
          Association.create "e09" "parseA" "parseB"
          Association.create "e10" "parseB" "parseA"
          Association.create "e11" "parseA" "zlib"
          Association.create "e12" "parseB" "libc"
          Association.create "e13" "http" "libc"
          Association.create "e14" "http" "zlib"
          Association.create "e15" "tls" "libc"
          Association.create "e16" "tls" "crypto" ]
    DiagramGraph.simple nodes edges |> ok

/// Larger 18-package 4-tier DAG spanning reach >= 9 (foundations), 4..8 (libraries), 1..3 (components), and 0 (applications).
let private buildTieredReachDag () =
    let foundations = [ "libc"; "zlib"; "crypto" ]
    let libraries = [ "tls"; "http"; "xml"; "sqlite" ]
    let components = [ "curl"; "gitcore"; "avcodec"; "archive"; "grpc" ]
    let apps = [ "git"; "ffmpeg"; "nginx"; "browser"; "backup"; "cli" ]
    let nodes =
        (foundations @ libraries @ components @ apps)
        |> List.map (fun id -> DiagramNode.sized id 32. 18.)
    let pairs =
        [ // Apps -> Components (each component gets 1..3 app dependents; apps have 0 dependents)
          "git", "gitcore"; "git", "curl"
          "ffmpeg", "avcodec"; "ffmpeg", "curl"
          "nginx", "grpc"; "nginx", "archive"
          "browser", "curl"; "browser", "avcodec"; "browser", "grpc"
          "backup", "archive"; "backup", "gitcore"
          "cli", "gitcore"; "cli", "archive"
          // Components -> Libraries (each library gets 6..8 transitive dependents)
          "curl", "tls"; "curl", "http"
          "gitcore", "tls"; "gitcore", "xml"
          "avcodec", "sqlite"; "avcodec", "xml"
          "archive", "sqlite"
          "grpc", "http"; "grpc", "sqlite"
          // Libraries -> Foundations (each foundation gets >= 9 transitive dependents)
          "tls", "libc"; "tls", "crypto"
          "http", "libc"; "http", "zlib"; "http", "crypto"
          "xml", "libc"; "xml", "zlib"
          "sqlite", "libc"; "sqlite", "zlib"; "sqlite", "crypto" ]
    let assocs =
        pairs |> List.mapi (fun idx (u, v) -> Association.create (sprintf "tr_%02d" idx) u v)
    DiagramGraph.simple nodes assocs |> ok, pairs

let tests =
    testList "AtlasRecipes" [
        testCase "Hyperbolic2D satisfies polar round-trip, Mobius isometry invariance and orthogonal Poincare geodesic arcs" <| fun _ ->
            Expect.equal Hyperbolic2D.origin { PoincarePoint.X = 0.; Y = 0. } "origin is (0, 0)"
            Expect.isTrue (abs (Hyperbolic2D.hyperbolicRadius Hyperbolic2D.origin) < 1e-12) "origin hyperbolicRadius = 0"
            Expect.isTrue (abs (Hyperbolic2D.conformalScale Hyperbolic2D.origin - 1.) < 1e-12) "origin conformalScale = 1"

            let validDisk = Hyperbolic2D.ofDisk 0.35 -0.45 |> ok
            Expect.isTrue (abs (validDisk.X - 0.35) < 1e-12 && abs (validDisk.Y + 0.45) < 1e-12) "ofDisk interior coordinates"
            Expect.isError (Hyperbolic2D.ofDisk 1.0 0.0) "ofDisk rejects unit circle boundary"
            Expect.isError (Hyperbolic2D.ofDisk 0.8 0.8) "ofDisk rejects exterior point"

            // Polar <-> hyperbolicRadius round-trip exactness
            for d in [ 0.; 0.25; 0.85; 1.6; 2.9; 4.2 ] do
                for theta in [ -2.5; -0.8; 0.; 1.15; 2.8 ] do
                    let p = Hyperbolic2D.ofPolar d theta
                    let rBack = Hyperbolic2D.hyperbolicRadius p
                    Expect.isTrue
                        (abs (rBack - d) < 1e-9)
                        (sprintf "hyperbolicRadius(ofPolar %g %g) = %g" d theta rBack)

            // Mobius translation maps focus -> (0, 0) within 1e-12 and preserves pairwise hyperbolic distances within 1e-9
            let samplePts =
                [ Hyperbolic2D.origin
                  Hyperbolic2D.ofPolar 0.45 0.3
                  Hyperbolic2D.ofPolar 1.25 -1.1
                  Hyperbolic2D.ofPolar 2.15 2.2
                  Hyperbolic2D.ofPolar 3.05 -2.6 ]
            let focuses =
                [ Hyperbolic2D.ofPolar 0.6 -0.5
                  Hyperbolic2D.ofPolar 1.5 1.35
                  Hyperbolic2D.ofPolar 2.4 -2.1 ]

            for focus in focuses do
                let mappedFocus, fScale = Hyperbolic2D.mobiusTranslate focus focus
                Expect.isTrue
                    (abs mappedFocus.X < 1e-12 && abs mappedFocus.Y < 1e-12)
                    (sprintf "mobiusTranslate maps focus to origin (got %g, %g)" mappedFocus.X mappedFocus.Y)
                Expect.isTrue (fScale > 0. && Double.IsFinite fScale) "positive finite conformal scale at focus"

                for a in samplePts do
                    for b in samplePts do
                        let dBefore = Hyperbolic2D.distance a b
                        let a', sA = Hyperbolic2D.mobiusTranslate focus a
                        let b', sB = Hyperbolic2D.mobiusTranslate focus b
                        let dAfter = Hyperbolic2D.distance a' b'
                        Expect.isTrue (sA > 0. && sB > 0.) "positive Mobius Jacobian scales"
                        Expect.isTrue
                            (abs (dBefore - dAfter) < 1e-9)
                            (sprintf "Mobius isometry preserves distance (%g vs %g)" dBefore dAfter)

            // Geodesic arc orthogonality, endpoint exactness, inward curvature and strict disk containment
            let diskCenter = Point.create 240. 200.
            let diskRadius = 150.
            let p = Point.create (diskCenter.X + 112. * cos 0.22) (diskCenter.Y + 112. * sin 0.22)
            let q = Point.create (diskCenter.X + 118. * cos 1.68) (diskCenter.Y + 118. * sin 1.68)
            let arcPath, arcPts = Hyperbolic2D.geodesicArc diskCenter diskRadius p q

            Expect.isNonEmpty (Path.commands arcPath) "geodesicArc non-empty Path"
            Expect.isTrue (arcPts.Length >= 3) "geodesicArc sampled waypoints >= 3"
            Expect.isTrue (dist2D (List.head arcPts) p < 1e-6) "geodesicArc starts at p"
            Expect.isTrue (dist2D (List.last arcPts) q < 1e-6) "geodesicArc ends at q"
            for w in arcPts do
                Expect.isTrue (dist2D diskCenter w < diskRadius) "all geodesic waypoints stay strictly inside diskRadius"

            let mChord = Point.create (0.5 * (p.X + q.X)) (0.5 * (p.Y + q.Y))
            let mArc = arcPts[arcPts.Length / 2]
            Expect.isTrue
                (dist2D diskCenter mArc < dist2D diskCenter mChord - 1.0)
                "non-collinear Poincare geodesic bows inward toward diskCenter"

            match circumcircle p mArc q with
            | None -> failtest "Non-collinear interior points must define a curved geodesic arc circle"
            | Some(cArc, rArc) ->
                let lhs = Point.distanceSquared cArc diskCenter
                let rhs = diskRadius * diskRadius + rArc * rArc
                Expect.isTrue
                    (abs (lhs - rhs) <= 1e-3 * rhs)
                    (sprintf "Poincare geodesic circle is orthogonal to boundary circle (|C_arc - C_disk|^2 = %g vs R_disk^2 + R_arc^2 = %g)" lhs rhs)

            // Collinear diameter chord degenerates cleanly without NaN
            let dA = Point.create (diskCenter.X - 90.) (diskCenter.Y - 45.)
            let dB = Point.create (diskCenter.X + 60.) (diskCenter.Y + 30.)
            let colPath, colPts = Hyperbolic2D.geodesicArc diskCenter diskRadius dA dB
            Expect.isNonEmpty (Path.commands colPath) "collinear diameter geodesic non-empty Path"
            Expect.isTrue (dist2D (List.head colPts) dA < 1e-6 && dist2D (List.last colPts) dB < 1e-6) "collinear geodesic endpoints"

        testCase "PoincareDisk stratifies FoundationsInner vs RootsInner, handles cycles, focuses nodes via Mobius translation and resets focus" <| fun _ ->
            let dag = buildEcosystemCyclicDag ()

            for layering in [ PoincareRingLayering.LogTransitiveReach; PoincareRingLayering.LongestPath ] do
                let cfgFound =
                    { PoincareDiskConfig.create viewport with
                        RingLayering = layering
                        CenterBias = PoincareCenterBias.FoundationsInner }
                let stateFound = PoincareDisk.buildState cfgFound dag |> ok
                let dc = PoincareDisk.diskCenter stateFound
                let dr = PoincareDisk.diskRadius stateFound
                let projFound = PoincareDisk.project stateFound

                Expect.equal projFound.Nodes.Length 11 (sprintf "%A node count" layering)
                Expect.equal projFound.Associations.Length 16 (sprintf "%A association count" layering)
                for n in projFound.Nodes do
                    Expect.isTrue
                        (dist2D dc n.Center < dr)
                        (sprintf "%A node %s strictly inside diskRadius (%g < %g)" layering n.Id (dist2D dc n.Center) dr)

                let radOf (layout: DiagramLayout2D) id =
                    let nd = layout.Nodes |> List.find (fun n -> n.Id = id)
                    dist2D dc nd.Center

                let meanFoundationRad =
                    ([ "libc"; "zlib"; "crypto" ] |> List.averageBy (radOf projFound))
                let suiteRad = radOf projFound "suite"
                Expect.isTrue
                    (meanFoundationRad < suiteRad)
                    (sprintf "FoundationsInner (%A) places foundations closer to center than suite (%g < %g)" layering meanFoundationRad suiteRad)

                let coordsFound = PoincareDisk.nodes stateFound
                let maxRing = coordsFound |> List.map (fun c -> c.Ring) |> List.max
                let suiteCoord = coordsFound |> List.find (fun c -> c.Id = "suite")
                Expect.equal suiteCoord.Ring maxRing (sprintf "FoundationsInner (%A) places suite on outermost ring" layering)

                // RootsInner reverses radial hierarchy so suite (root) sits closer to diskCenter than foundations
                let cfgRoots = { cfgFound with CenterBias = PoincareCenterBias.RootsInner }
                let projRoots = PoincareDisk.layout cfgRoots dag |> ok
                let meanFoundationRoots =
                    ([ "libc"; "zlib"; "crypto" ] |> List.averageBy (radOf projRoots))
                let suiteRoots = radOf projRoots "suite"
                Expect.isTrue
                    (suiteRoots < meanFoundationRoots)
                    (sprintf "RootsInner (%A) reverses hierarchy (%g < %g)" layering suiteRoots meanFoundationRoots)

            // Mobius focusNode + resetFocus round-trip
            let baseState = PoincareDisk.buildState (PoincareDiskConfig.create viewport) dag |> ok
            let canonicalProj = PoincareDisk.project baseState
            let dc = PoincareDisk.diskCenter baseState

            Expect.isError (PoincareDisk.focusNode "nonexistent" baseState) "focusNode rejects missing node ID"

            let focusedState = PoincareDisk.focusNode "tls" baseState |> ok
            let focusedProj = PoincareDisk.project focusedState
            Expect.equal focusedProj.Nodes.Length canonicalProj.Nodes.Length "focusNode preserves node count"
            Expect.equal focusedProj.Associations.Length canonicalProj.Associations.Length "focusNode preserves association count"
            let tlsFocused = focusedProj.Nodes |> List.find (fun n -> n.Id = "tls")
            Expect.isTrue
                (dist2D dc tlsFocused.Center < 1e-6)
                (sprintf "focusNode translates tls to diskCenter within 1e-6 (got %g)" (dist2D dc tlsFocused.Center))

            let resetProj = PoincareDisk.resetFocus focusedState |> PoincareDisk.project
            for n0, n1 in List.zip canonicalProj.Nodes resetProj.Nodes do
                Expect.equal n0.Id n1.Id "resetFocus node order"
                Expect.isTrue
                    (dist2D n0.Center n1.Center < 1e-9)
                    (sprintf "resetFocus restores canonical position of %s" n0.Id)

            let cancelled = CancellationToken(true)
            Expect.throwsT<OperationCanceledException>
                (fun () -> PoincareDisk.buildStateWithCancellation cancelled (PoincareDiskConfig.create viewport) dag |> ignore)
                "PoincareDisk honors cooperative cancellation"

        testCase "ConstellationLayout encloses discs tightly and packs non-overlapping cluster constellations with geodesic + LCA spline routing" <| fun _ ->
            // 1. Welzl / Apollonius encloseDiscs on 1, 2, and 12 varied discs
            Expect.isError (ConstellationLayout.encloseDiscs []) "encloseDiscs rejects empty list"

            let c1, r1 = ConstellationLayout.encloseDiscs [ Point.create 14. -9., 6.5 ] |> ok
            Expect.isTrue (dist2D c1 (Point.create 14. -9.) < 1e-9 && abs (r1 - 6.5) < 1e-9) "single-disc enclosure exact"

            let twoDiscs = [ Point.create 0. 0., 5.; Point.create 20. 0., 7. ]
            let c2, r2 = ConstellationLayout.encloseDiscs twoDiscs |> ok
            for ci, ri in twoDiscs do
                Expect.isTrue (dist2D c2 ci + ri <= r2 + 1e-6) "2-disc containment"
            let tight2 =
                twoDiscs
                |> List.filter (fun (ci, ri) -> abs ((dist2D c2 ci + ri) - r2) <= 1e-5)
                |> List.length
            Expect.isTrue (tight2 >= 2) "2 non-nested discs have >= 2 boundary-supporting discs"

            let twelveDiscs =
                [ for i in 0 .. 11 ->
                    let ang = 2. * Math.PI * float i / 12. + 0.17 * sin (float i)
                    let rad = 45. + 18. * cos (float (i * 2))
                    let rDisc = 4. + float ((i * 5) % 9)
                    Point.create (120. + rad * cos ang) (95. + rad * sin ang), rDisc ]
            let c12, r12 = ConstellationLayout.encloseDiscs twelveDiscs |> ok
            for ci, ri in twelveDiscs do
                Expect.isTrue
                    (dist2D c12 ci + ri <= r12 + 1e-6)
                    (sprintf "12-disc containment (%g + %g <= %g)" (dist2D c12 ci) ri r12)
            let tight12 =
                twelveDiscs
                |> List.filter (fun (ci, ri) -> abs ((dist2D c12 ci + ri) - r12) <= 1e-5)
                |> List.length
            Expect.isTrue (tight12 >= 2) (sprintf "12 varied discs have >= 2 tight boundary supports (got %d)" tight12)

            // 2. ConstellationLayout.compute across ExplicitGroups, LeidenAuto, and HybridGroupsAndLeiden
            let groups = [ DiagramGroup.create "modA"; DiagramGroup.create "modB"; DiagramGroup.create "modC" ]
            let modSizes = [ "modA", 8; "modB", 8; "modC", 5 ]
            let nodes =
                [ for gid, cnt in modSizes do
                    for k in 0 .. cnt - 1 ->
                        DiagramNode.sized (sprintf "%s_f%d" gid k) 22. 16. |> DiagramNode.inGroup gid ]
            let intraEdges =
                [ for gid, cnt in modSizes do
                    for k in 0 .. cnt - 2 do
                        yield Association.create (sprintf "in_%s_%d" gid k) (sprintf "%s_f%d" gid k) (sprintf "%s_f%d" gid (k + 1))
                        if k + 2 < cnt then
                            yield Association.create (sprintf "sk_%s_%d" gid k) (sprintf "%s_f%d" gid k) (sprintf "%s_f%d" gid (k + 2)) ]
            let interEdges =
                [ Association.create "xr_0" "modA_f0" "modB_f1"
                  Association.create "xr_1" "modA_f3" "modB_f5"
                  Association.create "xr_2" "modB_f2" "modC_f0"
                  Association.create "xr_3" "modC_f1" "modA_f6" ]
            let callGraph = DiagramGraph.create nodes groups (intraEdges @ interEdges) |> ok

            for mode in
                [ ConstellationClusterMode.ExplicitGroups
                  ConstellationClusterMode.LeidenAuto 1.0
                  ConstellationClusterMode.HybridGroupsAndLeiden(6, 1.0) ] do
                let cfg = { ConstellationConfig.create viewport with ClusterMode = mode }
                let out = ConstellationLayout.compute cfg callGraph |> ok

                Expect.equal out.Layout.Nodes.Length 21 (sprintf "%A node count" mode)
                Expect.equal out.Layout.Associations.Length (intraEdges.Length + interEdges.Length) (sprintf "%A association count" mode)
                Expect.isNonEmpty out.Clusters (sprintf "%A produces ClusterDiscs" mode)

                // Sibling ClusterDiscs sharing the same Parent do not overlap
                let byParent = out.Clusters |> List.groupBy (fun c -> c.Parent)
                for _, sibs in byParent do
                    let arr = List.toArray sibs
                    for i in 0 .. arr.Length - 2 do
                        for j in i + 1 .. arr.Length - 1 do
                            let sep = dist2D (arr[i].Center) (arr[j].Center)
                            let minSep = arr[i].Radius + arr[j].Radius
                            Expect.isTrue
                                (sep >= minSep - 1e-3)
                                (sprintf "%A sibling discs %s and %s non-overlapping (%g >= %g)" mode (arr[i].Id) (arr[j].Id) sep minSep)

                // Every member node lies strictly inside a leaf ClusterDisc, and map each node to its enclosing leaf disc
                let leafDiscs = out.Clusters |> List.filter (fun c -> c.IsLeafCluster)
                Expect.isNonEmpty leafDiscs (sprintf "%A has leaf ClusterDiscs" mode)

                let nodeLeafCluster =
                    out.Layout.Nodes
                    |> List.map (fun pn ->
                        let matchingLeaf =
                            leafDiscs
                            |> List.tryFind (fun cd -> dist2D pn.Center cd.Center < cd.Radius)
                        Expect.isSome
                            matchingLeaf
                            (sprintf "%A node %s strictly inside its leaf ClusterDisc" mode pn.Id)
                        pn.Id, matchingLeaf.Value.Id)
                    |> Map.ofList

                // Intra-cluster edges use ArcTo (or LineTo when collinear); inter-cluster edges use multi-segment LCA splines
                for ra in out.Layout.Associations do
                    let cu = nodeLeafCluster[ra.Source.NodeId]
                    let cv = nodeLeafCluster[ra.Target.NodeId]
                    let cmds = Path.commands ra.Path
                    Expect.isNonEmpty cmds ("non-empty route commands on " + ra.Id)
                    if cu = cv then
                        let hasGeodesicCmd =
                            cmds |> List.exists (function ArcTo _ | LineTo _ -> true | _ -> false)
                        Expect.isTrue hasGeodesicCmd (sprintf "%A intra-cluster edge %s uses ArcTo or LineTo" mode ra.Id)
                    else
                        let isSpline =
                            ra.Waypoints.Length >= 3
                            || (cmds |> List.exists (function CubicTo _ | QuadraticTo _ -> true | _ -> false))
                        Expect.isTrue isSpline (sprintf "%A inter-cluster edge %s uses multi-segment LCA spline" mode ra.Id)

        testCase "Pava.solveWeighted, IsotonicLayers (reach, non-overlap & interpolation) and LabelCull.select satisfy optimality and geometric invariants" <| fun _ ->
            // 1. Pava.solveWeighted minimum-gap + box-bound + L2 optimality + feasible fixed-point
            let targets = [ 130.; 75.; 95.; 55.; 165.; 145. ]
            let weights = [ 1.; 2.; 1.5; 3.; 1.; 2. ]
            let minGaps = [ 16.; 14.; 18.; 22.; 12. ]
            let lo, hi = 20., 260.
            let sol = Pava.solveWeighted targets weights minGaps lo hi |> ok
            let solArr = List.toArray sol
            let gapArr = List.toArray minGaps
            let tgtArr = List.toArray targets
            let wArr = List.toArray weights

            Expect.equal solArr.Length 6 "Pava solution length"
            for i in 0 .. 5 do
                Expect.isTrue (solArr[i] >= lo - 1e-9 && solArr[i] <= hi + 1e-9) "Pava respects [L, U] box bounds"
            for i in 0 .. 4 do
                Expect.isTrue
                    (solArr[i + 1] - solArr[i] >= gapArr[i] - 1e-9)
                    (sprintf "Pava enforces x[%d] - x[%d] >= %g (got %g)" (i + 1) i gapArr[i] (solArr[i + 1] - solArr[i]))

            let l2Cost (xs: float array) =
                let mutable s = 0.
                for i in 0 .. xs.Length - 1 do
                    let d = xs[i] - tgtArr[i]
                    s <- s + wArr[i] * d * d
                s

            let optCost = l2Cost solArr
            for shift in [ -12.; -4.; -0.5; 0.5; 4.; 12. ] do
                let cand = solArr |> Array.map ((+) shift)
                if cand[0] >= lo && cand[5] <= hi then
                    Expect.isTrue
                        (optCost <= l2Cost cand + 1e-7)
                        (sprintf "Pava beats feasible uniform shift %g (%g <= %g)" shift optCost (l2Cost cand))

            let altFeas = [| 35.; 60.; 90.; 125.; 165.; 210. |]
            for alpha in [ 0.1; 0.35; 0.7; 1.0 ] do
                let mix = Array.init 6 (fun i -> (1. - alpha) * solArr[i] + alpha * altFeas[i])
                Expect.isTrue (optCost <= l2Cost mix + 1e-7) "Pava beats convex combination with feasible point"

            // Already-feasible targets reproduced identically within 1e-12
            let feasTargets = [ 32.; 64.; 102.; 150. ]
            let feasSol = Pava.solveWeighted feasTargets [ 1.; 2.; 1.; 3. ] [ 20.; 25.; 30. ] 10. 220. |> ok
            for a, b in List.zip feasTargets feasSol do
                Expect.isTrue (abs (a - b) < 1e-12) (sprintf "Pava reproduces feasible target (%g vs %g)" a b)

            // Proportional gap compression when sum(minGaps) > U - L, and Error on invalid bounds/weights
            let compressed = Pava.solveWeighted [ 50.; 60.; 70. ] [ 1.; 1.; 1. ] [ 80.; 80. ] 0. 100. |> ok
            Expect.equal compressed.Length 3 "Pava compresses infeasible total gaps gracefully inside [L, U]"
            Expect.isTrue (compressed[0] >= -1e-9 && compressed[2] <= 100. + 1e-9 && compressed[1] > compressed[0] && compressed[2] > compressed[1]) "compressed Pava stays monotone in [0, 100]"
            Expect.isError (Pava.solveWeighted [ 50.; 60. ] [ 1.; 1. ] [ 10. ] 100. 20.) "Pava rejects lowerBound > upperBound"
            Expect.isError (Pava.solveWeighted [ 50.; 60. ] [ 1. ] [ 10. ] 0. 100.) "Pava rejects mismatched weights length"

            // 2. IsotonicLayers: computeReach, zero-overlap PAVA bands, and 0..1 interpolation
            let reachGraph, rawPairs = buildTieredReachDag ()
            let reachMap = IsotonicLayers.computeReach reachGraph

            // Exact reference dependent reachability (number of distinct other nodes u with u ->+ v)
            let revAdj =
                rawPairs
                |> List.groupBy snd
                |> List.map (fun (v, xs) -> v, xs |> List.map fst)
                |> Map.ofList
            let exactDependents (targetId: string) =
                let rec bfs (vis: Set<string>) (q: string list) =
                    match q with
                    | [] -> vis
                    | cur :: rest ->
                        let parents =
                            Map.tryFind cur revAdj
                            |> Option.defaultValue []
                            |> List.filter (fun p -> p <> targetId && not (Set.contains p vis))
                        bfs (List.fold (fun s x -> Set.add x s) vis parents) (parents @ rest)
                bfs Set.empty [ targetId ] |> Set.count

            for nd in DiagramGraph.nodes reachGraph do
                Expect.equal
                    reachMap[nd.Id]
                    (exactDependents nd.Id)
                    (sprintf "computeReach exact transitive dependent count for %s" nd.Id)

            let diskRef = PoincareDisk.layout (PoincareDiskConfig.create viewport) reachGraph |> ok
            let customTiers = [ "applications", 0; "components", 1; "libraries", 4; "foundations", 9 ]

            for strat in [ LayerStratification.LogTransitiveReach; LayerStratification.CustomReachTiers customTiers ] do
                let isoCfg =
                    { IsotonicLayersConfig.create viewport with
                        Stratification = strat
                        ReferenceLayout = Some diskRef }
                let isoOut = IsotonicLayers.compute isoCfg reachGraph |> ok
                Expect.equal isoOut.Layout.Nodes.Length 18 (sprintf "%A node count" strat)
                Expect.isNonEmpty isoOut.Bands (sprintf "%A non-empty bands" strat)

                let nodeArr = List.toArray isoOut.Layout.Nodes
                for i in 0 .. nodeArr.Length - 2 do
                    for j in i + 1 .. nodeArr.Length - 1 do
                        Expect.isFalse
                            (boxesStrictlyOverlap 1e-4 (nodeArr[i].Bounds) (nodeArr[j].Bounds))
                            (sprintf "%A zero box overlap between %s and %s" strat (nodeArr[i].Id) (nodeArr[j].Id))

            let customOut =
                IsotonicLayers.compute
                    { IsotonicLayersConfig.create viewport with
                        Stratification = LayerStratification.CustomReachTiers customTiers
                        ReferenceLayout = Some diskRef }
                    reachGraph
                |> ok
            Expect.equal customOut.Bands.Length 4 "CustomReachTiers produces 4 named bands"

            let interp0 = IsotonicLayers.interpolate 0.0 diskRef customOut.Layout |> ok
            let interp1 = IsotonicLayers.interpolate 1.0 diskRef customOut.Layout |> ok
            let interpMid = IsotonicLayers.interpolate 0.5 diskRef customOut.Layout |> ok

            for s, m0 in List.zip diskRef.Nodes interp0.Nodes do
                Expect.isTrue (dist2D s.Center m0.Center < 1e-9) "interpolate 0.0 matches source centers"
            for t, m1 in List.zip customOut.Layout.Nodes interp1.Nodes do
                Expect.isTrue (dist2D t.Center m1.Center < 1e-9) "interpolate 1.0 matches target centers"
            for (s, t), mm in List.zip (List.zip diskRef.Nodes customOut.Layout.Nodes) interpMid.Nodes do
                let expectedMid = Point.create (0.5 * (s.Center.X + t.Center.X)) (0.5 * (s.Center.Y + t.Center.Y))
                Expect.isTrue (Double.IsFinite mm.Center.X && Double.IsFinite mm.Center.Y) "interpolate 0.5 finite centers"
                Expect.isTrue (dist2D expectedMid mm.Center < 1e-6) "interpolate 0.5 midpoint center"

            // 3. LabelCull.select priority selection, overlap rejection, obstacle avoidance and viewport bounds
            let cullVp = Bounds.create 0. 0. 400. 300.
            let margin = 6.
            let obstacle = Bounds.create 190. 115. 40. 24.
            let candidates : LabelCandidate<string> list =
                [ { Id = "highPrio"
                    Anchor = Point.create 100. 100.
                    Width = 48.
                    Height = 16.
                    Priority = 100.
                    OffsetCandidates = [ Point.create 0. 0. ] }
                  { Id = "lowOverlap"
                    Anchor = Point.create 104. 102.
                    Width = 48.
                    Height = 16.
                    Priority = 10.
                    OffsetCandidates = [ Point.create 0. 0. ] }
                  { Id = "dodgeObstacle"
                    Anchor = Point.create 210. 127.
                    Width = 52.
                    Height = 16.
                    Priority = 60.
                    OffsetCandidates = [ Point.create 0. 0.; Point.create 0. -30. ] }
                  { Id = "outsideVp"
                    Anchor = Point.create -40. 50.
                    Width = 40.
                    Height = 14.
                    Priority = 90.
                    OffsetCandidates = [ Point.create 0. 0. ] } ]

            let placed = LabelCull.select cullVp margin [ obstacle ] candidates
            let placedIds = placed |> List.map (fun p -> p.Id) |> Set.ofList
            Expect.isTrue (Set.contains "highPrio" placedIds) "selects high-priority label"
            Expect.isFalse (Set.contains "lowOverlap" placedIds) "rejects overlapping lower-priority label"
            Expect.isTrue (Set.contains "dodgeObstacle" placedIds) "places obstacle-dodging label via fallback offset"
            Expect.isFalse (Set.contains "outsideVp" placedIds) "rejects out-of-viewport label"

            let placedArr = List.toArray placed
            for i in 0 .. placedArr.Length - 1 do
                Expect.isTrue (boxInside 1e-6 cullVp (placedArr[i].Bounds)) "placed label inside viewport"
                Expect.isFalse (boxesStrictlyOverlap 1e-6 obstacle (placedArr[i].Bounds)) "placed label avoids obstacle"
                for j in i + 1 .. placedArr.Length - 1 do
                    Expect.isFalse
                        (boxesStrictlyOverlap 1e-6 (placedArr[i].Bounds) (placedArr[j].Bounds))
                        "placed labels do not overlap"

        testCase "PoincareBallLift lifts 2D PoincareDiskState into 3D conformal ball, flattens exactly to 2D disk at fold = 1 and renders via Scene3D" <| fun _ ->
            let dag = buildEcosystemCyclicDag ()
            let diskCfg = PoincareDiskConfig.create viewport
            let diskState = PoincareDisk.buildState diskCfg dag |> ok

            let ballCenter = Point3.create 18. -24. 32.
            let ballRadius = 220.
            let liftCfg =
                { PoincareBallLiftConfig.create ballCenter ballRadius with
                    Yaw = 0.42
                    Pitch = -0.26
                    ArcSegments = 14 }

            let lifted = PoincareBallLift.lift liftCfg diskState
            Expect.equal lifted.Nodes.Length 11 "PoincareBallLift 3D node count"
            Expect.equal lifted.Associations.Length 16 "PoincareBallLift 3D association count"

            for n in lifted.Nodes do
                let d3 = Point3.distance ballCenter n.Center
                Expect.isTrue
                    (d3 < ballRadius)
                    (sprintf "lifted 3D node %s strictly inside BallRadius (%g < %g)" n.Id d3 ballRadius)

            let zs = lifted.Nodes |> List.map (fun n -> n.Center.Z)
            let zSpread = List.max zs - List.min zs
            Expect.isTrue (zSpread > 1.0) (sprintf "PoincareBallLift fold=0 produces genuine 3D Z spread (got %g)" zSpread)

            for a in lifted.Associations do
                Expect.isTrue (a.Points.Length >= 14) "curved Point3Chain has >= ArcSegments points"
                for k in 0 .. a.Points.Length - 1 do
                    Expect.isTrue (Point3.distance ballCenter (a.Points[k]) < ballRadius) "3D geodesic arc point strictly inside BallRadius"

            // Morph at fold = 1.0 flattens onto Z = BallCenter.Z within 1e-9 matching 2D Poincare disk unit ratios
            let flat = PoincareBallLift.morph 1.0 liftCfg diskState
            let diskCoordMap =
                PoincareDisk.nodes diskState
                |> List.map (fun nc -> nc.Id, nc.DiskPosition)
                |> Map.ofList
            for n in flat.Nodes do
                Expect.isTrue
                    (abs (n.Center.Z - ballCenter.Z) < 1e-9)
                    (sprintf "morph 1.0 flattens %s onto Z = BallCenter.Z (got %g vs %g)" n.Id n.Center.Z ballCenter.Z)
                let dp = diskCoordMap[n.Id]
                let rx = (n.Center.X - ballCenter.X) / ballRadius
                let ry = (n.Center.Y - ballCenter.Y) / ballRadius
                Expect.isTrue
                    (abs (rx - dp.X) < 1e-9 && abs (ry - dp.Y) < 1e-9)
                    (sprintf "morph 1.0 matches 2D Poincare disk ratios for %s" n.Id)

            let directLayout = PoincareBallLift.layout liftCfg diskCfg dag |> ok
            Expect.equal directLayout.Nodes.Length lifted.Nodes.Length "PoincareBallLift.layout matches lift"

            // Scene3D.render renders the lifted 3D globe cleanly (with guides centered at origin when BallCenter = origin)
            let originLift = PoincareBallLift.lift { liftCfg with BallCenter = Point3.create 0. 0. 0. } diskState
            let cam = Camera3D.orbit (Point3.create 0. 0. 0.) 0.65 0.32 620.
            let scene =
                Scene3D.render
                    { Scene3DTheme.create 640. 480. "Poincare Ball Globe" with
                        ReferenceSphereRadius = Some ballRadius }
                    cam
                    originLift
            Expect.isNonEmpty (Scene.nodes scene) "Scene3D.render renders lifted 3D Poincare ball cleanly"

            let cancelled = CancellationToken(true)
            Expect.throwsT<OperationCanceledException>
                (fun () -> PoincareBallLift.liftWithCancellation cancelled 0.3 liftCfg diskState |> ignore)
                "PoincareBallLift honors cooperative cancellation"
    ]

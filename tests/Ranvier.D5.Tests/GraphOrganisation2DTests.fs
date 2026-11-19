module Ranvier.D5.Tests.GraphOrganisation2DTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Graph

let private ok =
    function
    | Ok v -> v
    | Error errs -> failwithf "Unexpected error: %A" errs

let private viewport = Bounds.create 0. 0. 960. 640.

let private boxesStrictlyOverlap (tol: float) (a: Bounds) (b: Bounds) =
    Bounds.x a + Bounds.width a > Bounds.x b + tol
    && Bounds.x b + Bounds.width b > Bounds.x a + tol
    && Bounds.y a + Bounds.height a > Bounds.y b + tol
    && Bounds.y b + Bounds.height b > Bounds.y a + tol

let private pointInPolygonRayCast (poly: Point list) (pt: Point) : bool =
    let arr = List.toArray poly
    let n = arr.Length
    if n < 3 then
        false
    else
        let mutable inside = false
        let mutable j = n - 1
        for i in 0 .. n - 1 do
            let pi = arr[i]
            let pj = arr[j]
            if ((pi.Y > pt.Y) <> (pj.Y > pt.Y))
               && (pt.X < (pj.X - pi.X) * (pt.Y - pi.Y) / (pj.Y - pi.Y) + pi.X) then
                inside <- not inside
            j <- i
        inside

let private pointInClusterRegion (region: ClusterRegion) (pt: Point) : bool =
    let hits = region.Polygons |> List.filter (fun ring -> pointInPolygonRayCast ring pt) |> List.length
    (hits % 2) = 1

let private bruteDistance (metric: DistanceMetric) (a: float array) (b: float array) : float =
    let d = a.Length
    match metric with
    | DistanceMetric.Euclidean ->
        let mutable s = 0.
        for k in 0 .. d - 1 do
            let diff = a[k] - b[k]
            s <- s + diff * diff
        sqrt s
    | DistanceMetric.Manhattan ->
        let mutable s = 0.
        for k in 0 .. d - 1 do
            s <- s + abs (a[k] - b[k])
        s
    | DistanceMetric.Cosine ->
        let mutable dotAB = 0.
        let mutable nA = 0.
        let mutable nB = 0.
        for k in 0 .. d - 1 do
            dotAB <- dotAB + a[k] * b[k]
            nA <- nA + a[k] * a[k]
            nB <- nB + b[k] * b[k]
        if nA <= 1e-300 && nB <= 1e-300 then 0.
        elif nA <= 1e-300 || nB <= 1e-300 then 1.
        else max 0. (min 2. (1. - dotAB / (sqrt nA * sqrt nB)))
    | DistanceMetric.Correlation ->
        let inv = 1. / float d
        let mutable sumA = 0.
        let mutable sumB = 0.
        for k in 0 .. d - 1 do
            sumA <- sumA + a[k]
            sumB <- sumB + b[k]
        let mA = sumA * inv
        let mB = sumB * inv
        let mutable cov = 0.
        let mutable vA = 0.
        let mutable vB = 0.
        for k in 0 .. d - 1 do
            let da = a[k] - mA
            let db = b[k] - mB
            cov <- cov + da * db
            vA <- vA + da * da
            vB <- vB + db * db
        if vA <= 1e-300 && vB <= 1e-300 then 0.
        elif vA <= 1e-300 || vB <= 1e-300 then 1.
        else max 0. (min 2. (1. - cov / (sqrt vA * sqrt vB)))

let private makeStationLayout
    (bounds: Bounds)
    (stations: (string * Point * float * float * DiagramPort list) list)
    (assocs: Association list)
    : DiagramGraph * DiagramLayout2D =
    let dNodes =
        stations
        |> List.map (fun (id, _, w, h, ports) ->
            DiagramNode.sized id w h |> DiagramNode.withPorts ports)
    let graph = DiagramGraph.simple dNodes assocs |> ok
    let pNodes =
        stations
        |> List.sortBy (fun (id, _, _, _, _) -> id)
        |> List.map (fun (id, center, w, h, ports) ->
            let b = Bounds.create (center.X - w * 0.5) (center.Y - h * 0.5) w h
            let pPorts =
                ports
                |> List.sortBy (fun (p: DiagramPort) -> p.Id)
                |> List.map (fun (p: DiagramPort) ->
                    let anchor =
                        match p.Side with
                        | PortSide.North -> Point.create center.X (Bounds.y b)
                        | PortSide.South -> Point.create center.X (Bounds.y b + Bounds.height b)
                        | PortSide.West -> Point.create (Bounds.x b) center.Y
                        | PortSide.East
                        | PortSide.Auto -> Point.create (Bounds.x b + Bounds.width b) center.Y
                    { Id = p.Id; Side = p.Side; Anchor = anchor })
            { Id = id
              Kind = NodeKind.Standard
              Bounds = b
              Center = center
              ParentGroup = None
              Layer = None
              Ports = pPorts })
    let nodeLookup = pNodes |> List.map (fun (n: PositionedNode) -> n.Id, n) |> Map.ofList
    let initRouted =
        assocs
        |> List.sortBy (fun (a: Association) -> a.Id)
        |> List.map (fun (a: Association) ->
            let p0 = nodeLookup[a.Source.NodeId].Center
            let p1 = nodeLookup[a.Target.NodeId].Center
            { Id = a.Id
              Source = a.Source
              Target = a.Target
              Kind = a.Kind
              Waypoints = [ p0; p1 ]
              Path = Path.ofCommands [ MoveTo p0; LineTo p1 ]
              ReversedInCycle = false
              SourceLabel = None
              CenterLabel = None
              TargetLabel = None })
    let layout : DiagramLayout2D =
        { Bounds = bounds
          Nodes = pNodes
          Groups = []
          Associations = initRouted }
    graph, layout

let tests =
    testList "GraphOrganisation2D" [
        testCase "Leiden recovers planted 3-clique caveman communities, guarantees connectedness, supports ConstantPotts, hierarchy/group projection and cancellation" <| fun _ ->
            let cliqueNodes =
                [ for c in 0 .. 2 do
                    for v in 0 .. 5 -> DiagramNode.create (sprintf "c%d_%d" c v) ]
            let cliqueEdges =
                [ for c in 0 .. 2 do
                    for u in 0 .. 4 do
                        for v in u + 1 .. 5 ->
                            Association.create (sprintf "in_%d_%d_%d" c u v) (sprintf "c%d_%d" c u) (sprintf "c%d_%d" c v) ]
            let bridgeEdges =
                [ Association.create "br_0_1" "c0_5" "c1_0"
                  Association.create "br_1_2" "c1_5" "c2_0"
                  Association.create "br_2_0" "c2_5" "c0_0" ]
            let allEdges = cliqueEdges @ bridgeEdges
            let caveman = DiagramGraph.simple cliqueNodes allEdges |> ok

            let part = Leiden.detect LeidenConfig.defaultValue caveman |> ok
            Expect.equal part.CommunityCount 3 "Leiden recovers 3 caveman communities"
            Expect.isTrue (part.QualityScore > 0.5) (sprintf "Modularity > 0.5 (got %f)" part.QualityScore)

            for c in 0 .. 2 do
                let c0Id = part.Communities[sprintf "c%d_0" c]
                for v in 1 .. 5 do
                    Expect.equal (part.Communities[sprintf "c%d_%d" c v]) c0Id "all members of planted clique share community"
            Expect.equal
                (Set.ofList [ part.Communities["c0_0"]; part.Communities["c1_0"]; part.Communities["c2_0"] ] |> Set.count)
                3
                "3 cliques map to 3 distinct community indices"

            // Verify every discovered community is connected via BFS inside the induced subgraph
            let adj =
                let pairs =
                    allEdges
                    |> List.collect (fun (a: Association) ->
                        [ a.Source.NodeId, a.Target.NodeId
                          a.Target.NodeId, a.Source.NodeId ])
                pairs
                |> List.groupBy fst
                |> List.map (fun (u, xs) -> u, xs |> List.map snd |> Set.ofList)
                |> Map.ofList
            for KeyValue(cid, members) in part.CommunityMembers do
                Expect.isNonEmpty members (sprintf "community %d non-empty" cid)
                let memberSet = Set.ofList members
                let rec bfs (visited: Set<string>) (frontier: string list) =
                    match frontier with
                    | [] -> visited
                    | u :: rest ->
                        let nbrs =
                            Map.tryFind u adj
                            |> Option.defaultValue Set.empty
                            |> Set.intersect memberSet
                            |> fun s -> Set.difference s visited
                            |> Set.toList
                        bfs (List.fold (fun acc x -> Set.add x acc) visited nbrs) (nbrs @ rest)
                let reached = bfs (Set.singleton (List.head members)) [ List.head members ]
                Expect.equal reached memberSet (sprintf "community %d is connected" cid)

            // ConstantPotts mode
            let cpmCfg = { LeidenConfig.defaultValue with Quality = CommunityQuality.ConstantPotts; Resolution = 0.25 }
            let cpmPart = Leiden.detect cpmCfg caveman |> ok
            Expect.equal cpmPart.CommunityCount 3 "ConstantPotts recovers 3 caveman communities"
            Expect.isTrue (cpmPart.QualityScore > 0.) "ConstantPotts positive quality score"

            // toHierarchy & applyGroups
            let hRoot = Leiden.toHierarchy "cavemanRoot" caveman part |> ok
            let hItems = Hierarchy.descendants hRoot
            Expect.equal (List.head hItems).Id "cavemanRoot" "toHierarchy root ID"
            let leafIds = cliqueNodes |> List.map (fun (n: DiagramNode) -> n.Id) |> Set.ofList
            let hIds = hItems |> List.map (fun (it: HierarchyItem<string>) -> it.Id) |> Set.ofList
            Expect.isTrue (Set.isSubset leafIds hIds) "toHierarchy includes all 18 graph nodes"

            let groupedGraph = Leiden.applyGroups caveman part |> ok
            Expect.equal (DiagramGraph.groups groupedGraph).Length 3 "applyGroups creates 3 DiagramGroups"
            Expect.isTrue
                (DiagramGraph.nodes groupedGraph |> List.forall (fun (n: DiagramNode) -> n.ParentGroup.IsSome))
                "applyGroups assigns ParentGroup to every ungrouped node"

            // Pre-cancelled CancellationToken
            let cancelled = CancellationToken(true)
            Expect.throwsT<OperationCanceledException>
                (fun () -> Leiden.detectWithCancellation cancelled LeidenConfig.defaultValue caveman |> ignore)
                "Leiden honors pre-cancelled token"

        testCase "HierarchicalBundle, DividedForceBundle, MingleBundle and KdeBundle2D produce deterministic bundled routes and honor cancellation" <| fun _ ->
            let groups = [ for g in 0 .. 3 -> DiagramGroup.create (sprintf "grp%d" g) ]
            let nodes =
                [ for g in 0 .. 3 do
                    for k in 0 .. 3 ->
                        DiagramNode.sized (sprintf "n_%d_%d" g k) 28. 22.
                        |> DiagramNode.inGroup (sprintf "grp%d" g) ]
            let assocs =
                [ for k in 0 .. 3 do
                    yield Association.create (sprintf "ab_fwd_%d" k) (sprintf "n_0_%d" k) (sprintf "n_2_%d" k)
                    yield Association.create (sprintf "ab_rev_%d" k) (sprintf "n_2_%d" k) (sprintf "n_0_%d" ((k + 1) % 4))
                    yield Association.create (sprintf "cd_fwd_%d" k) (sprintf "n_1_%d" k) (sprintf "n_3_%d" k)
                    yield Association.create (sprintf "cross_%d" k) (sprintf "n_0_%d" k) (sprintf "n_3_%d" ((k + 2) % 4)) ]
            let graph = DiagramGraph.create nodes groups assocs |> ok
            let baseLayout = CircularLayout.layout (CircularConfig.create viewport) graph |> ok

            let hTree =
                Hierarchy.node "bundleRoot" 0.
                    [ for g in 0 .. 3 ->
                        Hierarchy.node (sprintf "grp%d" g) 0.
                            [ for k in 0 .. 3 -> Hierarchy.node (sprintf "n_%d_%d" g k) 1. [] ] ]

            let assertValidBundled (label: string) (out1: DiagramLayout2D) (out2: DiagramLayout2D) =
                Expect.equal out1.Associations.Length assocs.Length (label + " association count")
                for a in out1.Associations do
                    Expect.isTrue (a.Waypoints.Length >= 2) (label + " >= 2 waypoints on " + a.Id)
                    Expect.isNonEmpty (Path.commands a.Path) (label + " non-empty Path on " + a.Id)
                    for p in a.Waypoints do
                        Expect.isTrue (Double.IsFinite p.X && Double.IsFinite p.Y) (label + " finite waypoint")
                let wp1 = out1.Associations |> List.map (fun (a: RoutedAssociation) -> a.Id, a.Waypoints)
                let wp2 = out2.Associations |> List.map (fun (a: RoutedAssociation) -> a.Id, a.Waypoints)
                Expect.equal wp1 wp2 (label + " deterministic waypoints across runs")

            let heb1 = HierarchicalBundle.bundle 0.85 hTree baseLayout |> ok
            let heb2 = HierarchicalBundle.bundle 0.85 hTree baseLayout |> ok
            assertValidBundled "HierarchicalBundle.bundle" heb1 heb2

            let hebG1 = HierarchicalBundle.bundleGraph 0.85 graph baseLayout |> ok
            let hebG2 = HierarchicalBundle.bundleGraph 0.85 graph baseLayout |> ok
            assertValidBundled "HierarchicalBundle.bundleGraph" hebG1 hebG2

            let divCfg = { DividedBundleConfig.defaultValue with Cycles = 3; IterationsInitial = 24 }
            let div1 = DividedForceBundle.bundle divCfg graph baseLayout |> ok
            let div2 = DividedForceBundle.bundle divCfg graph baseLayout |> ok
            assertValidBundled "DividedForceBundle" div1 div2

            let min1 = MingleBundle.bundle MingleConfig.defaultValue graph baseLayout |> ok
            let min2 = MingleBundle.bundle MingleConfig.defaultValue graph baseLayout |> ok
            assertValidBundled "MingleBundle" min1 min2

            let kdeCfg = { KdeBundleConfig.defaultValue with GridSize = 64; Iterations = 5 }
            let kde1 = KdeBundle2D.bundle kdeCfg graph baseLayout |> ok
            let kde2 = KdeBundle2D.bundle kdeCfg graph baseLayout |> ok
            assertValidBundled "KdeBundle2D" kde1 kde2
            Expect.isTrue
                (kde1.Associations |> List.exists (fun a -> a.Waypoints.Length >= 5))
                "KdeBundle2D subdivides and advects interior polyline waypoints"

            let cancelled = CancellationToken(true)
            Expect.throwsT<OperationCanceledException>
                (fun () -> KdeBundle2D.bundleWithCancellation cancelled kdeCfg graph baseLayout |> ignore)
                "KdeBundle2D honors cancellation"
            Expect.throwsT<OperationCanceledException>
                (fun () -> MingleBundle.bundleWithCancellation cancelled MingleConfig.defaultValue graph baseLayout |> ignore)
                "MingleBundle honors cancellation"

        testCase "OctilinearRouter enforces strict 0/45/90-degree segments, avoids station obstacles and nudges shared parallel metro tracks" <| fun _ ->
            let pSouth = [ DiagramPort.create "pS" PortSide.South ]
            let stations =
                [ "s1", Point.create 80. 140., 32., 24., pSouth
                  "s2", Point.create 140. 140., 32., 24., pSouth
                  "t1", Point.create 480. 140., 32., 24., pSouth
                  "t2", Point.create 540. 140., 32., 24., pSouth
                  "u1", Point.create 100. 320., 36., 28., []
                  "blocker", Point.create 290. 320., 64., 54., []
                  "u2", Point.create 480. 320., 36., 28., []
                  "d1", Point.create 100. 460., 34., 26., []
                  "d2", Point.create 310. 560., 34., 26., [] ]
            let assocs =
                [ Association.betweenPorts "metro_1" (Endpoint.port "s1" "pS") (Endpoint.port "t1" "pS") AssociationKind.Directed
                  Association.betweenPorts "metro_2" (Endpoint.port "s2" "pS") (Endpoint.port "t2" "pS") AssociationKind.Directed
                  Association.create "detour" "u1" "u2"
                  Association.create "diag" "d1" "d2" ]
            let graph, initLayout = makeStationLayout viewport stations assocs

            let crispCfg =
                { OctilinearRouterConfig.defaultValue with
                    PortStubLength = 24.
                    GridSpacing = 20.
                    TrackSeparation = 8.
                    CornerFilletRadius = 0. }
            let zeroSepCfg = { crispCfg with TrackSeparation = 0. }

            let routedZero = OctilinearRouter.route zeroSepCfg graph initLayout |> ok
            let routedNudged = OctilinearRouter.route crispCfg graph initLayout |> ok

            // Every polyline segment must be horizontal, vertical, or +-45 degrees
            for ra in routedNudged.Associations do
                Expect.isTrue (ra.Waypoints.Length >= 2) ("waypoints on " + ra.Id)
                for p, q in List.pairwise ra.Waypoints do
                    let dx = q.X - p.X
                    let dy = q.Y - p.Y
                    let isOctilinear =
                        abs dx <= 1e-4
                        || abs dy <= 1e-4
                        || abs (abs dx - abs dy) < 1e-4 * max 1. (max (abs dx) (abs dy))
                    Expect.isTrue isOctilinear (sprintf "segment (%f,%f)->(%f,%f) on %s is octilinear" p.X p.Y q.X q.Y ra.Id)

            // Detour route must bypass blocker interior (258..322 x 293..347)
            let detour = routedNudged.Associations |> List.find (fun (a: RoutedAssociation) -> a.Id = "detour")
            let blockerInner = Bounds.create 260. 295. 60. 50.
            for p, q in List.pairwise detour.Waypoints do
                for step in 0 .. 8 do
                    let t = float step / 8.
                    let sample = Point.create (p.X + (q.X - p.X) * t) (p.Y + (q.Y - p.Y) * t)
                    Expect.isFalse (Bounds.contains sample blockerInner) "detour avoids station obstacle interior"

            // Shared parallel trunks metro_1 and metro_2 are nudged apart when TrackSeparation > 0
            let trunkY (layout: DiagramLayout2D) (id: string) =
                let wps =
                    layout.Associations
                    |> List.find (fun (a: RoutedAssociation) -> a.Id = id)
                    |> fun a -> List.toArray a.Waypoints
                Expect.isTrue (wps.Length >= 4) "4-point U-shaped metro trunk"
                wps[1].Y
            let zY1, zY2 = trunkY routedZero "metro_1", trunkY routedZero "metro_2"
            let nY1, nY2 = trunkY routedNudged "metro_1", trunkY routedNudged "metro_2"
            Expect.isTrue (abs (zY1 - zY2) <= 1e-4) "collinear parallel trunks without separation"
            Expect.isTrue (abs (abs (nY1 - nY2) - 8.) <= 1e-3) (sprintf "parallel metro trunks nudged apart by 8.0 (got %f vs %f)" nY1 nY2)

            // Default fillet config emits rounded QuadraticTo metro bends
            let filleted = OctilinearRouter.route OctilinearRouterConfig.defaultValue graph initLayout |> ok
            let hasQuadBend =
                filleted.Associations
                |> List.exists (fun (a: RoutedAssociation) ->
                    Path.commands a.Path
                    |> List.exists (function QuadraticTo _ -> true | _ -> false))
            Expect.isTrue hasQuadBend "positive CornerFilletRadius emits QuadraticTo metro bends"

        testCase "CircularLayout sifting reduces crossings and ArcLayout, HivePlot (with SplitAxes) and BioFabric satisfy structural invariants" <| fun _ ->
            // 12-cycle + local chords where canonical circular order has far fewer crossings than a shuffled permutation
            let nRing = 12
            let ringNodes = [ for i in 0 .. nRing - 1 -> DiagramNode.sized (sprintf "v%02d" i) 24. 24. ]
            let ringEdges =
                [ for i in 0 .. nRing - 1 do
                    yield Association.create (sprintf "cyc_%d" i) (sprintf "v%02d" i) (sprintf "v%02d" ((i + 1) % nRing))
                    if i % 2 = 0 then
                        yield Association.create (sprintf "ch_%d" i) (sprintf "v%02d" i) (sprintf "v%02d" ((i + 2) % nRing)) ]
            let ringGraph = DiagramGraph.simple ringNodes ringEdges |> ok
            let shuffledOrder = [ "v00"; "v06"; "v03"; "v09"; "v01"; "v07"; "v04"; "v10"; "v02"; "v08"; "v05"; "v11" ]
            let initialCrossings = CircularLayout.countCrossings shuffledOrder ringGraph

            let circCfg = { CircularConfig.create viewport with OrderByCommunities = false; SiftingSweeps = 6 }
            let circOut = CircularLayout.layout circCfg ringGraph |> ok
            Expect.equal circOut.Nodes.Length nRing "CircularLayout node count"

            let cx, cy = Bounds.x viewport + Bounds.width viewport * 0.5, Bounds.y viewport + Bounds.height viewport * 0.5
            let recoveredOrder =
                circOut.Nodes
                |> List.sortBy (fun (n: PositionedNode) ->
                    let a = atan2 (n.Center.Y - cy) (n.Center.X - cx)
                    if a < -Math.PI * 0.5 then a + 2. * Math.PI else a)
                |> List.map (fun (n: PositionedNode) -> n.Id)
            let siftedCrossings = CircularLayout.countCrossings recoveredOrder ringGraph
            Expect.isTrue
                (siftedCrossings <= initialCrossings && siftedCrossings < initialCrossings)
                (sprintf "CircularLayout sifting reduces crossings (%d < %d)" siftedCrossings initialCrossings)

            // ArcLayout places nodes on a common horizontal baseline with ArcTo semicircular arcs
            let arcOut = ArcLayout.layout (ArcConfig.create viewport) ringGraph |> ok
            Expect.equal arcOut.Nodes.Length nRing "ArcLayout node count"
            let baseY = (List.head arcOut.Nodes).Center.Y
            for n in arcOut.Nodes do
                Expect.isTrue (abs (n.Center.Y - baseY) <= 1e-6) "ArcLayout collinear baseline Y"
            Expect.isTrue
                (arcOut.Associations |> List.forall (fun (a: RoutedAssociation) ->
                    Path.commands a.Path |> List.exists (function ArcTo _ -> true | _ -> false)))
                "ArcLayout emits semicircular ArcTo commands"

            // HivePlot including SplitAxes = true with intra-tier edges
            let hiveNodes =
                [ DiagramNode.create "src1" |> DiagramNode.inGroup "tierA"
                  DiagramNode.create "src2" |> DiagramNode.inGroup "tierA"
                  DiagramNode.create "mid1" |> DiagramNode.inGroup "tierB"
                  DiagramNode.create "mid2" |> DiagramNode.inGroup "tierB"
                  DiagramNode.create "snk1" |> DiagramNode.inGroup "tierC"
                  DiagramNode.create "snk2" |> DiagramNode.inGroup "tierC" ]
            let hiveGroups = [ DiagramGroup.create "tierA"; DiagramGroup.create "tierB"; DiagramGroup.create "tierC" ]
            let hiveEdges =
                [ Association.create "e1" "src1" "mid1"
                  Association.create "e2" "src2" "mid2"
                  Association.create "e3" "mid1" "mid2" // intra-axis edge exercised by SplitAxes!
                  Association.create "e4" "mid1" "snk1"
                  Association.create "e5" "mid2" "snk2" ]
            let hiveGraph = DiagramGraph.create hiveNodes hiveGroups hiveEdges |> ok

            for rule in [ HiveAxisRule.SourceIntermediarySink; HiveAxisRule.DegreeTiers 3; HiveAxisRule.ByGroup ] do
                let hOut =
                    HivePlot.layout
                        { HiveConfig.create viewport with AxisRule = rule; SplitAxes = true }
                        hiveGraph
                    |> ok
                Expect.equal hOut.Nodes.Length 6 (sprintf "HivePlot %A node count" rule)
                Expect.equal hOut.Associations.Length 5 (sprintf "HivePlot %A association count" rule)

            // BioFabric assigns distinct horizontal row Ys and distinct vertical edge column Xs
            let bf : BioFabricLayout = BioFabric.layout viewport hiveGraph |> ok
            Expect.equal bf.Rows.Length 6 "BioFabric 6 node rows"
            Expect.equal bf.Columns.Length 5 "BioFabric 5 edge columns"
            Expect.equal bf.Diagram.Nodes.Length 6 "BioFabric Diagram nodes"
            let rowYs = bf.Rows |> List.map (fun r -> r.Y)
            let colXs = bf.Columns |> List.map (fun c -> c.X)
            Expect.isTrue (rowYs |> List.pairwise |> List.forall (fun (y0, y1) -> y1 > y0)) "BioFabric strictly increasing row Y"
            Expect.isTrue (colXs |> List.pairwise |> List.forall (fun (x0, x1) -> x1 > x0)) "BioFabric strictly increasing column X"

        testCase "PrismOverlap eliminates all pairwise overlaps on 25 piled rectangles and GMap and BubbleSets enclose group members" <| fun _ ->
            // 1. PrismOverlap on 25 piled rectangles
            let piledBoxes =
                [ for i in 0 .. 24 ->
                    let jx = sin (float i * 1.9) * 3.5
                    let jy = cos (float i * 2.3) * 3.5
                    let w = 36. + float ((i * 7) % 28)
                    let h = 24. + float ((i * 5) % 20)
                    sprintf "box_%02d" i, Bounds.create (200. + jx - w * 0.5) (200. + jy - h * 0.5) w h ]
            let separated = PrismOverlap.removeBoxOverlaps 4.0 80 piledBoxes |> ok
            Expect.equal separated.Length 25 "PrismOverlap preserves all 25 boxes"
            let sepArr = List.toArray separated
            for i in 0 .. sepArr.Length - 2 do
                for j in i + 1 .. sepArr.Length - 1 do
                    Expect.isFalse
                        (boxesStrictlyOverlap 1e-4 (snd (sepArr[i])) (snd (sepArr[j])))
                        (sprintf "no overlap between %s and %s" (fst (sepArr[i])) (fst (sepArr[j])))

            // 2. GMap & BubbleSets on a 3-country clustered layout
            let clusterStations =
                [ for c in 0 .. 2 do
                    let baseX = 180. + float c * 280.
                    let baseY = if c = 1 then 180. else 380.
                    for k in 0 .. 4 ->
                        let ang = 2. * Math.PI * float k / 5.
                        let pt = Point.create (baseX + 38. * cos ang) (baseY + 34. * sin ang)
                        sprintf "m_%d_%d" c k, pt, 24., 20., [] ]
            let cGraph, cLayout = makeStationLayout viewport clusterStations []
            let polished = PrismOverlap.polishLayout 4.0 40 cGraph cLayout |> ok
            Expect.equal polished.Nodes.Length 15 "PrismOverlap.polishLayout node count"

            let gmapAssign =
                [ for c in 0 .. 2 do
                    for k in 0 .. 4 -> sprintf "m_%d_%d" c k, sprintf "country_%d" c ]
            let countries = GMap.compute 28.0 2 gmapAssign polished |> ok
            Expect.equal countries.Length 3 "GMap produces 3 country regions"
            let posById = polished.Nodes |> List.map (fun (n: PositionedNode) -> n.Id, n.Center) |> Map.ofList
            for c in 0 .. 2 do
                let gid = sprintf "country_%d" c
                let reg = countries |> List.find (fun r -> r.GroupId = gid)
                Expect.isNonEmpty reg.Polygons (gid + " has country polygons")
                Expect.isTrue (reg.Area > 0.) (gid + " has positive country area")
                for k in 0 .. 4 do
                    let memberPt = posById[sprintf "m_%d_%d" c k]
                    Expect.isTrue (pointInClusterRegion reg memberPt) (sprintf "GMap %s encloses m_%d_%d" gid c k)

            let bubbleInput =
                [ "set_0", [ for k in 0 .. 4 -> sprintf "m_0_%d" k ]
                  "set_1", [ for k in 0 .. 4 -> sprintf "m_1_%d" k ]
                  "set_bridge", [ "m_0_0"; "m_1_0"; "m_2_0" ] ]
            let bubbles = BubbleSets.compute BubbleSetConfig.defaultValue bubbleInput polished |> ok
            Expect.equal bubbles.Length 3 "BubbleSets produces 3 contour regions"
            for sid, members in bubbleInput do
                let reg = bubbles |> List.find (fun r -> r.GroupId = sid)
                Expect.isNonEmpty reg.Polygons (sid + " non-empty closed contours")
                Expect.isTrue (reg.Polygons |> List.forall (fun ring -> ring.Length >= 3)) (sid + " closed contour rings")
                Expect.isTrue (reg.Area > 0.) (sid + " positive contour area")
                for mid in members do
                    Expect.isTrue (pointInClusterRegion reg (posById[mid])) (sprintf "BubbleSet %s encloses member %s" sid mid)

        testCase "FeatureDataset, BallTree, NnDescent and Umap2D/Tsne2D/Isomap2D/Sammon2D satisfy exact metric search, high k-NN recall and 8D blob separation" <| fun _ ->
            // 90 points in R^8 from 3 well-separated clusters (30 points each)
            let centers8D =
                [| [| 12.; -10.; 8.; -6.; 10.; -8.; 6.; -4. |]
                   [| -11.; 13.; -9.; 7.; -8.; 11.; -5.; 9. |]
                   [| 2.; 3.; -14.; -13.; 14.; 12.; -11.; 10. |] |]
            let rows90 =
                [ for c in 0 .. 2 do
                    for i in 0 .. 29 ->
                        let idx = c * 30 + i
                        let feats =
                            [ for d in 0 .. 7 ->
                                let jitter =
                                    0.65 * sin (float (idx * 7 + d * 13) * 1.37)
                                    + 0.45 * cos (float (idx * 11 + d * 5) * 0.91)
                                centers8D[c][d] + jitter ]
                        sprintf "p_%d_%02d" c i, feats ]
            let ds90 = FeatureDataset.ofRows rows90 |> ok
            Expect.equal (FeatureDataset.count ds90) 90 "FeatureDataset count = 90"
            Expect.equal (FeatureDataset.dimension ds90) 8 "FeatureDataset dim = 8"
            Expect.isError (FeatureDataset.ofRows [ "a", [ 1.; 2. ]; "b", [ 1. ] ]) "rejects ragged feature rows"

            // BallTree exact match vs brute-force across Euclidean, Cosine, Manhattan, Correlation
            let rawArr90 = rows90 |> List.map (fun (id, fs) -> id, List.toArray fs) |> List.toArray
            let queries8D =
                [ for q in 0 .. 19 ->
                    [ for d in 0 .. 7 -> 9. * sin (float (q * 5 + d * 3) * 1.13) + 4. * cos (float (q + d * 7)) ] ]

            for metric in [ DistanceMetric.Euclidean; DistanceMetric.Cosine; DistanceMetric.Manhattan; DistanceMetric.Correlation ] do
                let bt = BallTree.build metric ds90
                Expect.equal (BallTree.count bt) 90 "BallTree count"
                Expect.equal (BallTree.dimension bt) 8 "BallTree dimension"
                Expect.equal (BallTree.metric bt) metric "BallTree metric"
                for q in queries8D do
                    let qArr = List.toArray q
                    let bruteTop7 =
                        rawArr90
                        |> Array.mapi (fun idx (id, vec) -> bruteDistance metric qArr vec, idx, id)
                        |> Array.sortBy (fun (dist, idx, _) -> dist, idx)
                        |> Array.take 7
                        |> Array.toList
                    let treeTop7 = BallTree.kNearest 7 q bt |> ok
                    Expect.equal (treeTop7 |> List.map fst) (bruteTop7 |> List.map (fun (_, _, id) -> id)) (sprintf "BallTree %A exact kNearest IDs" metric)
                    for (_, td), (bd, _, _) in List.zip treeTop7 bruteTop7 do
                        Expect.isTrue (abs (td - bd) <= 1e-11) (sprintf "BallTree %A exact distance" metric)

            // NnDescent recall >= 0.85 on 90 points AND on 300 points (exercising iterative RP-tree + NN-Descent path N > 256)
            let checkNnRecall (rows: (string * float list) list) (k: int) =
                let ds = FeatureDataset.ofRows rows |> ok
                let links = NnDescent.buildGraph DistanceMetric.Euclidean k 42u ds |> ok
                let arr = rows |> List.map (fun (id, fs) -> id, List.toArray fs) |> List.toArray
                let bySource =
                    links
                    |> List.groupBy (fun (lnk: NeighborLink<string>) -> lnk.Source)
                    |> List.map (fun (s, xs) -> s, xs |> List.map (fun (lnk: NeighborLink<string>) -> lnk.Target) |> Set.ofList)
                    |> Map.ofList
                let mutable hits = 0
                let mutable total = 0
                for i in 0 .. arr.Length - 1 do
                    let sid, svec = arr[i]
                    let truth =
                        arr
                        |> Array.mapi (fun j (tid, tvec) ->
                            if i = j then infinity, j, tid
                            else bruteDistance DistanceMetric.Euclidean svec tvec, j, tid)
                        |> Array.sortBy (fun (d, j, _) -> d, j)
                        |> Array.take k
                        |> Array.map (fun (_, _, tid) -> tid)
                        |> Set.ofArray
                    let found = Map.tryFind sid bySource |> Option.defaultValue Set.empty
                    hits <- hits + (Set.intersect truth found |> Set.count)
                    total <- total + k
                float hits / float total

            Expect.isTrue (checkNnRecall rows90 8 >= 0.85) "NnDescent 90-point recall >= 0.85"
            let rows300 =
                [ for c in 0 .. 2 do
                    for i in 0 .. 99 ->
                        let idx = c * 100 + i
                        let feats =
                            [ for d in 0 .. 7 ->
                                centers8D[c][d] + 1.1 * sin (float (idx * 9 + d * 17) * 1.21) + 0.7 * cos (float (idx * 3 + d * 11)) ]
                        sprintf "q_%d_%02d" c i, feats ]
            let recall300 = checkNnRecall rows300 10
            Expect.isTrue (recall300 >= 0.85) (sprintf "NnDescent 300-point RP-tree + local-join recall >= 0.85 (got %f)" recall300)

            // Umap2D, Tsne2D, Isomap2D, Sammon2D separate 3 Gaussian blobs in R^8 inside viewport
            let clusterOf (id: string) = int (id.Split('_')[1])
            let checkEmbedding2D (label: string) (pts: EmbeddedPoint2D<string> list) =
                Expect.equal pts.Length 90 (label + " embedded count")
                for p in pts do
                    Expect.isTrue (Bounds.contains p.Position viewport) (label + " point inside viewport")
                let arr = List.toArray pts
                let mutable intraSum, intraCnt = 0., 0
                let mutable interSum, interCnt = 0., 0
                for i in 0 .. arr.Length - 2 do
                    let ci = clusterOf (arr[i].Id)
                    for j in i + 1 .. arr.Length - 1 do
                        let cj = clusterOf (arr[j].Id)
                        let d = sqrt (Point.distanceSquared (arr[i].Position) (arr[j].Position))
                        if ci = cj then
                            intraSum <- intraSum + d
                            intraCnt <- intraCnt + 1
                        else
                            interSum <- interSum + d
                            interCnt <- interCnt + 1
                let meanIntra = intraSum / float intraCnt
                let meanInter = interSum / float interCnt
                Expect.isTrue (meanIntra < meanInter * 0.65) (sprintf "%s separates blobs (intra %f < inter %f)" label meanIntra meanInter)

            let umapCfg = { UmapConfig.create viewport with Neighbors = 12; Epochs = 120; Seed = 77u }
            let umapA = Umap2D.embed umapCfg ds90 |> ok
            let umapB = Umap2D.embed umapCfg ds90 |> ok
            checkEmbedding2D "Umap2D" umapA
            Expect.equal umapA umapB "Umap2D deterministic seed"

            let tsneCfg = { TsneConfig.create viewport with Perplexity = 18.; Iterations = 180; Seed = 77u }
            let tsneA = Tsne2D.embed tsneCfg ds90 |> ok
            let tsneB = Tsne2D.embed tsneCfg ds90 |> ok
            checkEmbedding2D "Tsne2D" tsneA
            Expect.equal tsneA tsneB "Tsne2D deterministic seed"

            // Connected neighborhood across clusters (neighbors = 35 > cluster size 30) so shortest-path geodesics connect all 3 blobs
            let isoOut = Isomap2D.embed viewport DistanceMetric.Euclidean 35 ds90 |> ok
            checkEmbedding2D "Isomap2D" isoOut

            let sammonA = Sammon2D.embed viewport DistanceMetric.Euclidean 120 77u ds90 |> ok
            let sammonB = Sammon2D.embed viewport DistanceMetric.Euclidean 120 77u ds90 |> ok
            checkEmbedding2D "Sammon2D" sammonA
            Expect.equal sammonA sammonB "Sammon2D deterministic seed"

            let cancelled = CancellationToken(true)
            Expect.throwsT<OperationCanceledException>
                (fun () -> Umap2D.embedWithCancellation cancelled umapCfg ds90 |> ignore)
                "Umap2D honors cancellation"
            Expect.throwsT<OperationCanceledException>
                (fun () -> Tsne2D.embedWithCancellation cancelled tsneCfg ds90 |> ignore)
                "Tsne2D honors cancellation"
    ]

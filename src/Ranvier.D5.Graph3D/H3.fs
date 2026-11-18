namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Settings for Tamara Munzner's H3 3D hyperbolic graph layout engine (IEEE InfoVis 1997 / CG&amp;A 1998).</summary>
type H3Config =
    { /// <summary>Unit-ball embedding model (<c>Klein</c> uses straight-chord geodesics as in H3; <c>Poincare</c> uses conformal circular arcs).</summary>
      Model: HyperbolicModel
      /// <summary>Euclidean radius in world units to which the open unit ball (<c>||v|| &lt; 1</c>) is scaled (default 300).</summary>
      BallRadius: float
      /// <summary>Optional preferred root node ID for the backbone spanning tree.</summary>
      RootId: string option
      /// <summary>Spanning-tree extraction policy over arbitrary directed/undirected graphs.</summary>
      SpanningStrategy: SpanningTreeStrategy
      /// <summary>Base hyperbolic disc radius allocated to a unit-size leaf node (default 0.12).</summary>
      LeafHyperbolicRadius: float
      /// <summary>Hemisphere packing area multiplier (&gt;= 1.1; default 1.45) accounting for spherical-cap packing gaps.</summary>
      HemisphereAreaScale: float
      /// <summary>Minimum hyperbolic radial step between parent and child hemispheres (default 0.28).</summary>
      RadialGap: float
      /// <summary>Minimum projected scale factor in [0, 1) below which tiny peripheral cross-links can be omitted when desired (0 keeps all).</summary>
      MinVisibleScale: float
      /// <summary>Number of arc sample segments when <c>Model = HyperbolicModel.Poincare</c> (default 10).</summary>
      PoincareArcSegments: int }

/// <summary>Constructors for <c>H3Config</c>.</summary>
[<RequireQualifiedAccess>]
module H3Config =
    /// <summary>Creates Munzner H3 defaults in the Beltrami-Klein model with 300-unit ball radius.</summary>
    let defaultValue =
        { Model = HyperbolicModel.Klein
          BallRadius = 300.
          RootId = None
          SpanningStrategy = SpanningTreeStrategy.Bfs
          LeafHyperbolicRadius = 0.12
          HemisphereAreaScale = 1.45
          RadialGap = 0.28
          MinVisibleScale = 0.
          PoincareArcSegments = 10 }

/// <summary>Persistent, immutable 3D hyperbolic layout state supporting O(N) SIMD interactive Focus+Context Lorentz boosts without re-running layout.</summary>
type HyperbolicLayoutState =
    internal
        { Config: H3Config
          Graph: DiagramGraph
          Nodes: DiagramNode[]
          NodeIndex: Dictionary<string, int>
          Associations: Association[]
          IsBackbone: bool[]
          Depths: int[]
          BaseRadius: float[]
          BaseX: float[]
          BaseY: float[]
          BaseZ: float[]
          BaseW: float[]
          ViewTransform: HyperbolicTransform }

/// <summary>Tamara Munzner's H3 3D hyperbolic hemisphere-packing layout and interactive Lorentz focus engine.</summary>
[<RequireQualifiedAccess>]
module H3 =
    let private validateConfig (config: H3Config) =
        if not (Double.IsFinite config.BallRadius && Double.IsFinite config.LeafHyperbolicRadius
                && Double.IsFinite config.HemisphereAreaScale && Double.IsFinite config.RadialGap
                && Double.IsFinite config.MinVisibleScale)
           || config.BallRadius <= 0. || config.LeafHyperbolicRadius <= 0.
           || config.HemisphereAreaScale < 1.05 || config.RadialGap <= 0.
           || config.MinVisibleScale < 0. || config.MinVisibleScale >= 1.
           || config.PoincareArcSegments < 2 || config.PoincareArcSegments > 64 then
            Error [ InvalidObservation "H3 requires positive BallRadius/LeafHyperbolicRadius/RadialGap, HemisphereAreaScale >= 1.05, MinVisibleScale in [0,1) and 2..64 arc segments." ]
        else
            Ok()

    /// <summary>Projects a <c>HyperbolicLayoutState</c> under its current <c>ViewTransform</c> into a Euclidean <c>DiagramLayout3D</c> inside <c>[-BallRadius, +BallRadius]^3</c>.</summary>
    let project (state: HyperbolicLayoutState) : DiagramLayout3D =
        let cfg = state.Config
        let n = state.Nodes.Length
        let bRad = cfg.BallRadius
        let boundBox = Bounds3.create -bRad -bRad -bRad (2. * bRad) (2. * bRad) (2. * bRad)
        if n = 0 then
            { Bounds = boundBox; Nodes = []; Groups = Kernels3D.computeGroups3D state.Graph [||]; Associations = [] }
        else
            let tx = Array.zeroCreate<float> n
            let ty = Array.zeroCreate<float> n
            let tz = Array.zeroCreate<float> n
            let tw = Array.zeroCreate<float> n
            HyperbolicTransform.applySoA state.ViewTransform state.BaseX state.BaseY state.BaseZ state.BaseW tx ty tz tw n

            let minkPts = Array.init n (fun i -> MinkowskiPoint(tx[i], ty[i], tz[i], tw[i]))
            let nodeBoxes = Array.zeroCreate<Bounds3> n
            let posNodes =
                [ for i in 0 .. n - 1 do
                    let nd = state.Nodes[i]
                    let unitBallPt, atten = Hyperbolic3D.toBall cfg.Model minkPts[i]
                    let worldCenter = Point3.create (unitBallPt.X * bRad) (unitBallPt.Y * bRad) (unitBallPt.Z * bRad)
                    let clampedScale = max 0.015 (min 1. atten)
                    let effRad = max 1.2 (state.BaseRadius[i] * clampedScale)
                    let sx = max 2.4 (nd.Size.Width * 0.25 * clampedScale)
                    let sy = max 2.4 (nd.Size.Height * 0.25 * clampedScale)
                    let sz = max 2.4 (nd.DepthSize * 0.25 * clampedScale)
                    let box = Kernels3D.nodeBox worldCenter sx sy sz
                    nodeBoxes[i] <- box
                    yield
                        { Id = nd.Id
                          Kind = nd.Kind
                          Center = worldCenter
                          Bounds = box
                          Radius = effRad
                          ScaleFactor = atten
                          ParentGroup = nd.ParentGroup
                          Layer = Some state.Depths[i] } ]

            let groups = Kernels3D.computeGroups3D state.Graph nodeBoxes
            let scaleChain (chain: Point3Chain) =
                let arr = Array.init chain.Length (fun k ->
                    let p = chain[k]
                    Point3.create (p.X * bRad) (p.Y * bRad) (p.Z * bRad))
                Point3Chain.ofOwnedArray arr, Array.toList arr

            let routed =
                [ for e in 0 .. state.Associations.Length - 1 do
                    let a = state.Associations[e]
                    let ui = state.NodeIndex[a.Source.NodeId]
                    let vi = state.NodeIndex[a.Target.NodeId]
                    let isTree = state.IsBackbone[e]
                    let _, sA = Hyperbolic3D.toBall cfg.Model minkPts[ui]
                    let _, sB = Hyperbolic3D.toBall cfg.Model minkPts[vi]
                    if isTree || max sA sB >= cfg.MinVisibleScale then
                        let unitChain =
                            if ui = vi then
                                let p, _ = Hyperbolic3D.toBall cfg.Model minkPts[ui]
                                Point3Chain.ofOwnedArray [| p; Point3.create (p.X + 0.02) (p.Y + 0.02) p.Z; p |]
                            else
                                Hyperbolic3D.geodesicPoints cfg.Model cfg.PoincareArcSegments minkPts[ui] minkPts[vi]
                        let pts, wps = scaleChain unitChain
                        yield
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              IsBackbone = isTree
                              Points = pts
                              Waypoints = wps } ]

            { Bounds = Kernels3D.computeBounds3D boundBox posNodes groups routed
              Nodes = posNodes
              Groups = groups
              Associations = routed }

    /// <summary>Returns the current <c>HyperbolicTransform</c> view matrix of the state.</summary>
    let viewTransform (state: HyperbolicLayoutState) = state.ViewTransform

    /// <summary>Returns the transformed <c>MinkowskiPoint</c> of <c>nodeId</c> under the current view, or <c>None</c> if unknown.</summary>
    let tryNodeMinkowski (nodeId: string) (state: HyperbolicLayoutState) : MinkowskiPoint option =
        match state.NodeIndex.TryGetValue nodeId with
        | true, idx ->
            let raw = MinkowskiPoint(state.BaseX[idx], state.BaseY[idx], state.BaseZ[idx], state.BaseW[idx])
            Some(HyperbolicTransform.apply state.ViewTransform raw)
        | _ -> None

    /// <summary>Focuses <c>nodeId</c> isometrically at the center of the 3D hyperbolic ball via an exact Lorentz boost in O(1) state update.</summary>
    let focusNode (nodeId: string) (state: HyperbolicLayoutState) : Result<HyperbolicLayoutState, LayoutError list> =
        match tryNodeMinkowski nodeId state with
        | None -> Error [ MissingEndpoint nodeId ]
        | Some currentPos ->
            let boost = HyperbolicTransform.boostToOrigin currentPos
            let nextView = state.ViewTransform |> HyperbolicTransform.thenApply boost |> HyperbolicTransform.orthonormalize
            Ok { state with ViewTransform = nextView }

    /// <summary>Applies an incremental 3D <c>Quaternion</c> rotation around the hyperbolic ball origin.</summary>
    let rotate (rotation: Quaternion) (state: HyperbolicLayoutState) : HyperbolicLayoutState =
        let rot = HyperbolicTransform.ofRotation rotation
        { state with ViewTransform = state.ViewTransform |> HyperbolicTransform.thenApply rot |> HyperbolicTransform.orthonormalize }

    /// <summary>Replaces the view transform directly (or resets with <c>HyperbolicTransform.identity</c>).</summary>
    let withViewTransform (transform: HyperbolicTransform) (state: HyperbolicLayoutState) : HyperbolicLayoutState =
        { state with ViewTransform = HyperbolicTransform.orthonormalize transform }

    /// <summary>Computes the persistent <c>HyperbolicLayoutState</c> in H^3 using Munzner's bottom-up hemisphere synthesis and top-down spherical-cap band packing.</summary>
    let buildStateWithCancellation
        (token: System.Threading.CancellationToken)
        (config: H3Config)
        (graph: DiagramGraph)
        : Result<HyperbolicLayoutState, LayoutError list> =
        token.ThrowIfCancellationRequested()
        match validateConfig config with
        | Error e -> Error e
        | Ok () ->
            let resolved = DiagramGraph.resolveCollapsed graph
            let nodes = DiagramGraph.nodes resolved |> List.toArray
            let assocs = DiagramGraph.associations resolved |> List.toArray
            let n = nodes.Length
            let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
            for i = 0 to n - 1 do nodeIdx.Add(nodes[i].Id, i)

            if n = 0 then
                Ok
                    { Config = config
                      Graph = resolved
                      Nodes = [||]
                      NodeIndex = nodeIdx
                      Associations = [||]
                      IsBackbone = [||]
                      Depths = [||]
                      BaseRadius = [||]
                      BaseX = [||]; BaseY = [||]; BaseZ = [||]; BaseW = [||]
                      ViewTransform = HyperbolicTransform.identity }
            else
                let forest = SpanningTree.extract config.SpanningStrategy config.RootId resolved
                let children = Array.init n (fun i ->
                    forest.ChildrenOf
                    |> Map.tryFind nodes[i].Id
                    |> Option.defaultValue []
                    |> List.map (fun cid -> nodeIdx[cid])
                    |> List.toArray)
                let depths = Array.init n (fun i -> forest.DepthOf |> Map.tryFind nodes[i].Id |> Option.defaultValue 0)
                let rootIndices = forest.Roots |> List.map (fun rid -> nodeIdx[rid]) |> List.toArray

                // Compute normalized visual size weight per node
                let meanDiag =
                    (nodes |> Array.sumBy (fun nd -> sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height))) / float n
                let normScale = Array.init n (fun i ->
                    let diag = sqrt (nodes[i].Size.Width * nodes[i].Size.Width + nodes[i].Size.Height * nodes[i].Size.Height)
                    if meanDiag <= 1e-6 then 1. else max 0.5 (min 2.5 (diag / meanDiag)))

                // Postorder traversal across forest
                let postorder = ResizeArray<int>(n)
                let stack = Stack<struct(int * bool)>()
                for r = rootIndices.Length - 1 downto 0 do
                    stack.Push(struct(rootIndices[r], false))
                while stack.Count > 0 do
                    let struct(u, doneKids) = stack.Pop()
                    if doneKids then postorder.Add u
                    else
                        stack.Push(struct(u, true))
                        let ch = children[u]
                        for k = ch.Length - 1 downto 0 do
                            stack.Push(struct(ch[k], false))

                // Pass 1 (Bottom-Up): Munzner H3 Appendix A hyperbolic hemisphere radius synthesis
                // Hemisphere surface area in H^3: H_p = 2 * pi * sinh^2(r_p) => r_p = asinh(sqrt(H_p / (2 * pi)))
                let hemiRadius = Array.zeroCreate<float> n
                for idx = 0 to postorder.Count - 1 do
                    Kernels3D.check token idx
                    let u = postorder[idx]
                    let rLeaf = config.LeafHyperbolicRadius * normScale[u]
                    let ch = children[u]
                    if ch.Length = 0 then
                        hemiRadius[u] <- rLeaf
                    else
                        let mutable areaSum = 0.
                        for c in ch do
                            let s = sinh (min 12. hemiRadius[c])
                            let childArea = 2. * Math.PI * s * s
                            areaSum <- areaSum + childArea
                        let totalArea = config.HemisphereAreaScale * areaSum
                        let reqR = Math.Asinh(sqrt (totalArea / (2. * Math.PI)))
                        hemiRadius[u] <- max rLeaf (min 12. reqR)

                // Pass 2 (Top-Down): Concentric spherical-cap band packing on parent hemispheres in H^3
                let baseX = Array.zeroCreate<float> n
                let baseY = Array.zeroCreate<float> n
                let baseZ = Array.zeroCreate<float> n
                let baseW = Array.create n 1.

                // Pack `kids` onto a hemisphere (maxPhi = pi/2) or full sphere (maxPhi = pi at root!) around parent frame `parentFrame`
                let packChildrenOnCap (parentFrame: HyperbolicTransform) (parentR: float) (maxPhi: float) (kids: int[]) =
                    if kids.Length > 0 then
                        // Sort children by descending hemisphere radius so dominant subtrees sit near the pole!
                        let sorted = Array.copy kids
                        Array.Sort(sorted, Comparer<int>.Create(fun a b ->
                            let c = compare hemiRadius[b] hemiRadius[a]
                            if c <> 0 then c else compare a b))

                        let sinhRp = max 0.05 (sinh (min 12. parentR))
                        let mutable cursorIdx = 0
                        let mutable phi = 0.

                        // Place largest child right at pole (phi = 0) when there is 1 child or when packing a hemisphere
                        if sorted.Length = 1 || maxPhi <= Math.PI * 0.6 then
                            let poleChild = sorted[0]
                            let rc = hemiRadius[poleChild]
                            let stepD = parentR + rc + config.RadialGap
                            let localPt = Hyperbolic3D.fromSpherical stepD 0. 0.
                            let worldPt = HyperbolicTransform.apply parentFrame localPt
                            baseX[poleChild] <- worldPt.X; baseY[poleChild] <- worldPt.Y
                            baseZ[poleChild] <- worldPt.Z; baseW[poleChild] <- worldPt.W
                            let dPhi0 = atan (tanh rc / sinhRp)
                            phi <- max 0.18 (2. * dPhi0 + 0.06)
                            cursorIdx <- 1
                        else
                            phi <- 0.35

                        // Pack remaining children into concentric latitude bands (phi, theta)
                        let childFrames = ResizeArray<struct(int * HyperbolicTransform)>(sorted.Length)
                        if cursorIdx = 1 then
                            let poleChild = sorted[0]
                            let pLocal = Hyperbolic3D.fromSpherical (parentR + hemiRadius[poleChild] + config.RadialGap) 0. 0.
                            let cFrame =
                                HyperbolicTransform.boostFromOrigin pLocal
                                |> HyperbolicTransform.thenApply parentFrame
                                |> HyperbolicTransform.orthonormalize
                            childFrames.Add(struct(poleChild, cFrame))

                        while cursorIdx < sorted.Length do
                            let firstC = sorted[cursorIdx]
                            let dPhiFirst = atan (tanh hemiRadius[firstC] / sinhRp)
                            let bandPhi = min maxPhi (max 0.12 (phi + dPhiFirst))
                            let sinhRPhi = max 0.04 (sinhRp * sin (min (Math.PI - 0.08) bandPhi))

                            // Greedily collect children that fit in the 2*pi azimuth circumference of this band
                            let bandMembers = ResizeArray<struct(int * float)>()
                            let mutable thetaUsed = 0.
                            let mutable bandMaxDPhi = dPhiFirst
                            let mutable bandFull = false
                            while cursorIdx < sorted.Length && not bandFull do
                                let c = sorted[cursorIdx]
                                let rc = hemiRadius[c]
                                let dTheta = max 0.08 (atan (tanh rc / sinhRPhi))
                                let dPhi = atan (tanh rc / sinhRp)
                                if bandMembers.Count > 0 && thetaUsed + 2. * dTheta > 2. * Math.PI + 1e-6 then
                                    bandFull <- true
                                else
                                    bandMembers.Add(struct(c, dTheta))
                                    thetaUsed <- thetaUsed + 2. * dTheta
                                    if dPhi > bandMaxDPhi then bandMaxDPhi <- dPhi
                                    cursorIdx <- cursorIdx + 1

                            // Distribute band members evenly around [0, 2*pi) azimuth
                            let slack = if thetaUsed < 2. * Math.PI then (2. * Math.PI - thetaUsed) / float bandMembers.Count else 0.
                            let scaleAz = if thetaUsed > 2. * Math.PI then (2. * Math.PI) / thetaUsed else 1.
                            let mutable theta = (float bandMembers.Count * 0.61803398875) % (2. * Math.PI)
                            for m = 0 to bandMembers.Count - 1 do
                                let struct(c, dTheta) = bandMembers[m]
                                let halfSpan = dTheta * scaleAz + slack * 0.5
                                let az = theta + halfSpan
                                theta <- theta + 2. * halfSpan

                                let stepD = parentR + hemiRadius[c] + config.RadialGap
                                let localPt = Hyperbolic3D.fromSpherical stepD bandPhi az
                                let dir =
                                    let sp = sin bandPhi
                                    Point3.create (sp * cos az) (sp * sin az) (cos bandPhi)
                                let orient = Quaternion.fromUnitVectors (Point3.create 0. 0. 1.) dir
                                let localFrame =
                                    HyperbolicTransform.ofRotation orient
                                    |> HyperbolicTransform.thenApply (HyperbolicTransform.boostFromOrigin localPt)
                                let cFrame =
                                    localFrame
                                    |> HyperbolicTransform.thenApply parentFrame
                                    |> HyperbolicTransform.orthonormalize
                                let worldOrigin = HyperbolicTransform.apply cFrame Hyperbolic3D.origin
                                baseX[c] <- worldOrigin.X; baseY[c] <- worldOrigin.Y
                                baseZ[c] <- worldOrigin.Z; baseW[c] <- worldOrigin.W
                                childFrames.Add(struct(c, cFrame))

                            phi <- min (maxPhi - 0.05) (bandPhi + bandMaxDPhi + 0.08)

                        childFrames.ToArray()
                    else [||]

                // Traverse top-down with an explicit queue of (nodeIdx, frame)
                let frameQueue = Queue<struct(int * HyperbolicTransform)>()
                if rootIndices.Length = 1 then
                    let r0 = rootIndices[0]
                    baseX[r0] <- 0.; baseY[r0] <- 0.; baseZ[r0] <- 0.; baseW[r0] <- 1.
                    // At root, utilize full sphere (maxPhi = pi * 0.88) so branches fan out isotropically in 3D!
                    let rootKids = packChildrenOnCap HyperbolicTransform.identity (hemiRadius[r0] * 0.5) (Math.PI * 0.88) children[r0]
                    for item in rootKids do frameQueue.Enqueue item
                else
                    // Multiple forest roots: place them around a virtual origin sphere
                    let virtualR =
                        let mutable a = 0.
                        for r in rootIndices do
                            let s = sinh (min 10. hemiRadius[r])
                            a <- a + 2. * Math.PI * s * s
                        Math.Asinh(sqrt (config.HemisphereAreaScale * a / (4. * Math.PI)))
                    let placedRoots = packChildrenOnCap HyperbolicTransform.identity (max 0.25 virtualR) (Math.PI * 0.88) rootIndices
                    for item in placedRoots do frameQueue.Enqueue item

                let mutable visitedCount = 0
                while frameQueue.Count > 0 do
                    Kernels3D.check token visitedCount
                    visitedCount <- visitedCount + 1
                    let struct(u, uFrame) = frameQueue.Dequeue()
                    if children[u].Length > 0 then
                        let sub = packChildrenOnCap uFrame hemiRadius[u] (Math.PI * 0.48) children[u]
                        for item in sub do frameQueue.Enqueue item

                let baseRadii = Array.init n (fun i -> max 3.5 (10. * normScale[i]))
                let isBackbone = assocs |> Array.map (fun a -> forest.TreeAssociationIds.Contains a.Id)

                Ok
                    { Config = config
                      Graph = resolved
                      Nodes = nodes
                      NodeIndex = nodeIdx
                      Associations = assocs
                      IsBackbone = isBackbone
                      Depths = depths
                      BaseRadius = baseRadii
                      BaseX = baseX
                      BaseY = baseY
                      BaseZ = baseZ
                      BaseW = baseW
                      ViewTransform = HyperbolicTransform.identity }

    /// <summary>Builds the persistent <c>HyperbolicLayoutState</c> in H^3 without cancellation.</summary>
    let buildState config graph = buildStateWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Computes a projected 3D hyperbolic Munzner H3 layout with cooperative cancellation.</summary>
    let layoutWithCancellation token config graph =
        buildStateWithCancellation token config graph |> Result.map project

    /// <summary>Computes a projected 3D hyperbolic Munzner H3 layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

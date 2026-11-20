namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Spanning-tree backbone source for CAIDA Walrus 3D hyperbolic graph visualization.</summary>
[<RequireQualifiedAccess>]
type WalrusTreeSource =
    /// <summary>Uses an explicit caller-supplied set of spanning-tree association IDs (CAIDA Walrus native workflow).</summary>
    | ExplicitAssociations of associationIds: Set<string>
    /// <summary>Extracts a rooted spanning forest automatically from the graph.</summary>
    | Automatic of strategy: SpanningTreeStrategy

/// <summary>Settings for the CAIDA Walrus 3D hyperbolic spanning-tree + cross-link sphere engine.</summary>
type WalrusConfig =
    { /// <summary>Unit-ball projection model (<c>Klein</c> or <c>Poincare</c>; Walrus defaults to Klein/hyperboloid projection).</summary>
      Model: HyperbolicModel
      /// <summary>Euclidean world radius of the bounding 3D sphere (default 300).</summary>
      SphereRadius: float
      /// <summary>Explicit or automatic spanning-tree backbone specification.</summary>
      Backbone: WalrusTreeSource
      /// <summary>Optional root node ID.</summary>
      RootId: string option
      /// <summary>Half-angle of child cone fan-out in radians (in [0.2, 1.4]; default 0.92).</summary>
      ConeHalfAngle: float
      /// <summary>Base hyperbolic radial link length per tree level (default 0.68).</summary>
      BaseLinkLength: float
      /// <summary>When true, includes non-tree cross-link associations alongside spanning-tree backbone links.</summary>
      IncludeCrossLinks: bool
      /// <summary>Arc discretization count when <c>Model = HyperbolicModel.Poincare</c>.</summary>
      PoincareArcSegments: int }

/// <summary>Constructors for <c>WalrusConfig</c>.</summary>
[<RequireQualifiedAccess>]
module WalrusConfig =
    /// <summary>Creates CAIDA Walrus defaults with automatic BFS spanning backbone, 300-unit sphere radius and cross-links enabled.</summary>
    let defaultValue =
        { Model = HyperbolicModel.Klein
          SphereRadius = 300.
          Backbone = WalrusTreeSource.Automatic SpanningTreeStrategy.Bfs
          RootId = None
          ConeHalfAngle = 0.92
          BaseLinkLength = 0.68
          IncludeCrossLinks = true
          PoincareArcSegments = 10 }

/// <summary>CAIDA Walrus 3D hyperbolic spanning-tree + cross-link layout engine (<c>H3GraphLayout</c> Minkowski cone allocation).</summary>
[<RequireQualifiedAccess>]
module Walrus =
    let private validateExplicitForest
        (nodes: DiagramNode[])
        (nodeIdx: Dictionary<string, int>)
        (assocs: Association[])
        (explicitIds: Set<string>)
        (preferredRoot: string option)
        : Result<int[] * int[][] * int[] * Set<string>, LayoutError list> =
        let n = nodes.Length
        let adj = Array.init n (fun _ -> ResizeArray<struct(int * string)>())
        let mutable selectedCount = 0
        let missing = ResizeArray<LayoutError>()
        let knownAssoc = HashSet<string>(StringComparer.Ordinal)
        for a in assocs do knownAssoc.Add a.Id |> ignore
        for id in explicitIds do
            if not (knownAssoc.Contains id) then missing.Add(MissingEndpoint id)

        if missing.Count > 0 then Error(List.ofSeq missing)
        else
            for a in assocs do
                if explicitIds.Contains a.Id then
                    let u = nodeIdx[a.Source.NodeId]
                    let v = nodeIdx[a.Target.NodeId]
                    if u = v then
                        missing.Add(InvalidTopology("Explicit Walrus spanning tree cannot contain self-loop: " + a.Id))
                    else
                        selectedCount <- selectedCount + 1
                        adj[u].Add(struct(v, a.Id))
                        adj[v].Add(struct(u, a.Id))

            if missing.Count > 0 then Error(List.ofSeq missing)
            else
                let visited = Array.zeroCreate<bool> n
                let children = Array.init n (fun _ -> ResizeArray<int>())
                let depths = Array.zeroCreate<int> n
                let roots = ResizeArray<int>()
                let usedEdges = HashSet<string>(StringComparer.Ordinal)
                let mutable cycleFound = false

                let startOrder = ResizeArray<int>(n + 1)
                preferredRoot
                |> Option.bind (fun id -> match nodeIdx.TryGetValue id with true, i -> Some i | _ -> None)
                |> Option.iter startOrder.Add
                for i = 0 to n - 1 do startOrder.Add i

                for s in startOrder do
                    if not visited[s] && not cycleFound then
                        roots.Add s
                        visited[s] <- true
                        depths[s] <- 0
                        let q = Queue<struct(int * int)>()
                        q.Enqueue(struct(s, -1))
                        while q.Count > 0 && not cycleFound do
                            let struct(u, parent) = q.Dequeue()
                            for item in adj[u] do
                                let struct(v, eid) = item
                                if v <> parent then
                                    if visited[v] then
                                        cycleFound <- true
                                    else
                                        visited[v] <- true
                                        depths[v] <- depths[u] + 1
                                        children[u].Add v
                                        usedEdges.Add eid |> ignore
                                        q.Enqueue(struct(v, u))

                if cycleFound || usedEdges.Count <> selectedCount then
                    Error [ InvalidTopology "Explicit Walrus spanning associations contain a cycle or parallel duplicate." ]
                else
                    Ok(roots.ToArray(), children |> Array.map (fun c -> c.ToArray()), depths, Set.ofSeq usedEdges)

    /// <summary>Builds a persistent <c>HyperbolicLayoutState</c> using CAIDA Walrus subtree-weighted 3D Fibonacci cone allocation in Minkowski space.</summary>
    let buildStateWithCancellation
        (token: System.Threading.CancellationToken)
        (config: WalrusConfig)
        (graph: DiagramGraph)
        : Result<HyperbolicLayoutState, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.SphereRadius && Double.IsFinite config.ConeHalfAngle && Double.IsFinite config.BaseLinkLength)
           || config.SphereRadius <= 0. || config.ConeHalfAngle < 0.2 || config.ConeHalfAngle > 1.45
           || config.BaseLinkLength <= 0. || config.PoincareArcSegments < 2 || config.PoincareArcSegments > 64 then
            Error [ InvalidObservation "Walrus requires positive SphereRadius/BaseLinkLength, ConeHalfAngle in [0.2, 1.45] and 2..64 arc segments." ]
        else
            let resolved = DiagramGraph.resolveCollapsed graph
            let eff = DiagramGraph.effective resolved
            let nodes = eff.Nodes
            let assocs = eff.Associations
            let n = nodes.Length
            let nodeIdx = eff.ReversedIdMap

            let h3CompatConfig =
                { H3Config.defaultValue with
                    Model = config.Model
                    BallRadius = config.SphereRadius
                    PoincareArcSegments = config.PoincareArcSegments }

            if n = 0 then
                Ok
                    { Config = h3CompatConfig
                      Graph = resolved
                      Nodes = [||]
                      NodeIndex = nodeIdx
                      Associations = [||]
                      AssocSrc = [||]
                      AssocDst = [||]
                      IsBackbone = [||]
                      Depths = [||]
                      BaseRadius = [||]
                      BaseX = [||]; BaseY = [||]; BaseZ = [||]; BaseW = [||]
                      ViewTransform = HyperbolicTransform.identity }
            else
                let csr = GraphKernels.buildCsr eff
                let forestResult =
                    match config.Backbone with
                    | WalrusTreeSource.ExplicitAssociations ids ->
                        validateExplicitForest nodes nodeIdx assocs ids config.RootId
                        |> Result.map (fun (rArr, cArr, dArr, edgeSet) ->
                            let treeFlags = Array.init assocs.Length (fun e -> edgeSet.Contains assocs[e].Id)
                            rArr, cArr, dArr, treeFlags)
                    | WalrusTreeSource.Automatic strategy ->
                        let rArr, _, cArr, dArr, treeFlags =
                            SpanningTree.extractIndexed strategy config.RootId eff csr
                        Ok(rArr, cArr, dArr, treeFlags)

                match forestResult with
                | Error errs -> Error errs
                | Ok(rootIndices, children, depths, isTreeAssoc) ->
                    // Compute subtree weight (1 + descendant count) via flat postorder stack
                    let postorder = Array.zeroCreate<int> n
                    let mutable postLen = 0
                    let postStack = Array.zeroCreate<int> (max 1 (n * 2))
                    let mutable sp = 0
                    for r = rootIndices.Length - 1 downto 0 do
                        postStack[sp] <- rootIndices[r]
                        sp <- sp + 1
                    while sp > 0 do
                        sp <- sp - 1
                        let raw = postStack[sp]
                        if raw < 0 then
                            postorder[postLen] <- ~~~raw
                            postLen <- postLen + 1
                        else
                            let u = raw
                            postStack[sp] <- ~~~u
                            sp <- sp + 1
                            let ch = children[u]
                            for k = ch.Length - 1 downto 0 do
                                postStack[sp] <- ch[k]
                                sp <- sp + 1

                    let subtreeSize = Array.create n 1
                    for idx = 0 to postLen - 1 do
                        let u = postorder[idx]
                        let ch = children[u]
                        let mutable s = 1
                        for k = 0 to ch.Length - 1 do s <- s + subtreeSize[ch[k]]
                        subtreeSize[u] <- s

                    let logSubtree = Array.init n (fun u -> log (float subtreeSize[u]))
                    let solidWeight u = 1. + logSubtree[u] * 1.5

                    let baseX = Array.zeroCreate<float> n
                    let baseY = Array.zeroCreate<float> n
                    let baseZ = Array.zeroCreate<float> n
                    let baseW = Array.create n 1.
                    let zUnit = Point3.create 0. 0. 1.

                    let qNodes = Array.zeroCreate<int> n
                    let qFrames = Array.zeroCreate<HyperbolicTransform> n
                    let mutable qHead = 0
                    let mutable qTail = 0
                    let goldenAngle = Math.PI * (3. - sqrt 5.)

                    // Walrus cone distribution: distribute `kids` across spherical cap [0, capAngle] weighted by `solidWeight`
                    let placeConeChildren (parentFrame: HyperbolicTransform) (capAngle: float) (kids: int[]) =
                        let kLen = kids.Length
                        if kLen = 0 then ()
                        elif kLen = 1 && capAngle < Math.PI * 0.8 then
                            let c = kids[0]
                            let len = config.BaseLinkLength * (1. + 0.15 * logSubtree[c])
                            let localPt = Hyperbolic3D.fromSpherical len 0. 0.
                            let wPt = HyperbolicTransform.apply parentFrame localPt
                            baseX[c] <- wPt.X; baseY[c] <- wPt.Y; baseZ[c] <- wPt.Z; baseW[c] <- wPt.W
                            if children[c].Length > 0 then
                                let cFrame =
                                    HyperbolicTransform.boostFromOrigin localPt
                                    |> HyperbolicTransform.thenApply parentFrame
                                    |> HyperbolicTransform.orthonormalize
                                qNodes[qTail] <- c
                                qFrames[qTail] <- cFrame
                                qTail <- qTail + 1
                        else
                            let ordered =
                                if kLen <= 16 then
                                    let cp = Array.copy kids
                                    for a = 1 to kLen - 1 do
                                        let kv = cp[a]
                                        let ks = subtreeSize[kv]
                                        let mutable b = a - 1
                                        while b >= 0 && (subtreeSize[cp[b]] < ks || (subtreeSize[cp[b]] = ks && cp[b] > kv)) do
                                            cp[b + 1] <- cp[b]
                                            b <- b - 1
                                        cp[b + 1] <- kv
                                    cp
                                else
                                    let cp = Array.copy kids
                                    Array.Sort(cp, Comparer<int>.Create(fun a b ->
                                        let c = compare subtreeSize[b] subtreeSize[a]
                                        if c <> 0 then c else compare a b))
                                    cp

                            let mutable totalW = 0.
                            for k = 0 to kLen - 1 do totalW <- totalW + solidWeight ordered[k]
                            let invTotalW = 1. / max 1e-9 totalW
                            let cosMin = cos capAngle
                            let branchScale = 1. + 0.22 * log (float kLen)
                            let mutable cumW = 0.

                            for k = 0 to kLen - 1 do
                                let c = ordered[k]
                                let w = solidWeight c
                                let frac = (cumW + 0.5 * w) * invTotalW
                                cumW <- cumW + w
                                let cosPhi = max -0.9999 (min 0.9999 (1. - frac * (1. - cosMin)))
                                let phi = acos cosPhi
                                let theta = float k * goldenAngle
                                let stepLen = min 8. (config.BaseLinkLength * branchScale * (1. + 0.12 * logSubtree[c]))
                                let localPt = Hyperbolic3D.fromSpherical stepLen phi theta
                                if children[c].Length > 0 then
                                    let spPhi = sqrt (max 0. (1. - cosPhi * cosPhi))
                                    let dir = Point3.create (spPhi * cos theta) (spPhi * sin theta) cosPhi
                                    let orient = Quaternion.fromUnitVectors zUnit dir
                                    let cFrame =
                                        HyperbolicTransform.ofRotation orient
                                        |> HyperbolicTransform.thenApply (HyperbolicTransform.boostFromOrigin localPt)
                                        |> HyperbolicTransform.thenApply parentFrame
                                        |> HyperbolicTransform.orthonormalize
                                    let wPt = HyperbolicTransform.apply cFrame Hyperbolic3D.origin
                                    baseX[c] <- wPt.X; baseY[c] <- wPt.Y; baseZ[c] <- wPt.Z; baseW[c] <- wPt.W
                                    qNodes[qTail] <- c
                                    qFrames[qTail] <- cFrame
                                    qTail <- qTail + 1
                                else
                                    // Leaf shortcut: cFrame * origin == parentFrame * localPt!
                                    let wPt = HyperbolicTransform.apply parentFrame localPt
                                    baseX[c] <- wPt.X; baseY[c] <- wPt.Y; baseZ[c] <- wPt.Z; baseW[c] <- wPt.W

                    if rootIndices.Length = 1 then
                        let r0 = rootIndices[0]
                        baseX[r0] <- 0.; baseY[r0] <- 0.; baseZ[r0] <- 0.; baseW[r0] <- 1.
                        placeConeChildren HyperbolicTransform.identity (Math.PI * 0.95) children[r0]
                    else
                        placeConeChildren HyperbolicTransform.identity (Math.PI * 0.95) rootIndices

                    while qHead < qTail do
                        Kernels3D.check token qHead
                        let u = qNodes[qHead]
                        let uFrame = qFrames[qHead]
                        qHead <- qHead + 1
                        placeConeChildren uFrame config.ConeHalfAngle children[u]

                    let visibleAssocs, visSrc, visDst, isBackbone =
                        if config.IncludeCrossLinks then
                            assocs, csr.Src, csr.Dst, isTreeAssoc
                        else
                            let mutable cnt = 0
                            for e = 0 to assocs.Length - 1 do if isTreeAssoc[e] then cnt <- cnt + 1
                            let va = Array.zeroCreate<Association> cnt
                            let vs = Array.zeroCreate<int> cnt
                            let vd = Array.zeroCreate<int> cnt
                            let vb = Array.create cnt true
                            let mutable w = 0
                            for e = 0 to assocs.Length - 1 do
                                if isTreeAssoc[e] then
                                    va[w] <- assocs[e]
                                    vs[w] <- csr.Src[e]
                                    vd[w] <- csr.Dst[e]
                                    w <- w + 1
                            va, vs, vd, vb

                    let unitScale = max 1e-6 (config.SphereRadius / 300.0)
                    let baseRadii =
                        Array.init n (fun i ->
                            let importance = 1. + 0.35 * logSubtree[i]
                            max (3.5 * unitScale) (min (18. * unitScale) (5.5 * unitScale * importance)))

                    Ok
                        { Config = h3CompatConfig
                          Graph = resolved
                          Nodes = nodes
                          NodeIndex = nodeIdx
                          Associations = visibleAssocs
                          AssocSrc = visSrc
                          AssocDst = visDst
                          IsBackbone = isBackbone
                          Depths = depths
                          BaseRadius = baseRadii
                          BaseX = baseX
                          BaseY = baseY
                          BaseZ = baseZ
                          BaseW = baseW
                          ViewTransform = HyperbolicTransform.identity }

    /// <summary>Builds the persistent CAIDA Walrus <c>HyperbolicLayoutState</c> without cancellation.</summary>
    let buildState config graph = buildStateWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Focuses any node at the center of the Walrus sphere via an isometric Lorentz boost.</summary>
    let focusNode nodeId state = H3.focusNode nodeId state

    /// <summary>Rotates the Walrus hyperbolic sphere around its origin.</summary>
    let rotate rotation state = H3.rotate rotation state

    /// <summary>Projects the Walrus state into a 3D Euclidean <c>DiagramLayout3D</c>.</summary>
    let project state = H3.project state

    /// <summary>Computes a projected 3D CAIDA Walrus layout with cooperative cancellation.</summary>
    let layoutWithCancellation token config graph =
        buildStateWithCancellation token config graph |> Result.map H3.project

    /// <summary>Computes a projected 3D CAIDA Walrus layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

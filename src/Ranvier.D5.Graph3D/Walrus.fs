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
            let nodes = DiagramGraph.nodes resolved |> List.toArray
            let assocs = DiagramGraph.associations resolved |> List.toArray
            let n = nodes.Length
            let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
            for i = 0 to n - 1 do nodeIdx.Add(nodes[i].Id, i)

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
                      IsBackbone = [||]
                      Depths = [||]
                      BaseRadius = [||]
                      BaseX = [||]; BaseY = [||]; BaseZ = [||]; BaseW = [||]
                      ViewTransform = HyperbolicTransform.identity }
            else
                let forestResult =
                    match config.Backbone with
                    | WalrusTreeSource.ExplicitAssociations ids ->
                        validateExplicitForest nodes nodeIdx assocs ids config.RootId
                    | WalrusTreeSource.Automatic strategy ->
                        let f = SpanningTree.extract strategy config.RootId resolved
                        let rArr = f.Roots |> List.map (fun id -> nodeIdx[id]) |> List.toArray
                        let cArr = Array.init n (fun i ->
                            f.ChildrenOf
                            |> Map.tryFind nodes[i].Id
                            |> Option.defaultValue []
                            |> List.map (fun cid -> nodeIdx[cid])
                            |> List.toArray)
                        let dArr = Array.init n (fun i -> f.DepthOf |> Map.tryFind nodes[i].Id |> Option.defaultValue 0)
                        Ok(rArr, cArr, dArr, f.TreeAssociationIds)

                match forestResult with
                | Error errs -> Error errs
                | Ok(rootIndices, children, depths, treeEdgeSet) ->
                    // Compute subtree weight (1 + descendant count, log-dampened for balanced solid-angle allocation as in Walrus)
                    let postorder = ResizeArray<int>(n)
                    let stack = Stack<struct(int * bool)>()
                    for r = rootIndices.Length - 1 downto 0 do stack.Push(struct(rootIndices[r], false))
                    while stack.Count > 0 do
                        let struct(u, expanded) = stack.Pop()
                        if expanded then postorder.Add u
                        else
                            stack.Push(struct(u, true))
                            let ch = children[u]
                            for k = ch.Length - 1 downto 0 do stack.Push(struct(ch[k], false))

                    let subtreeSize = Array.create n 1
                    for idx = 0 to postorder.Count - 1 do
                        let u = postorder[idx]
                        let mutable s = 1
                        for c in children[u] do s <- s + subtreeSize[c]
                        subtreeSize[u] <- s

                    let solidWeight u = 1. + log (float subtreeSize[u]) * 1.5

                    let baseX = Array.zeroCreate<float> n
                    let baseY = Array.zeroCreate<float> n
                    let baseZ = Array.zeroCreate<float> n
                    let baseW = Array.create n 1.

                    // Walrus cone distribution: distribute `kids` across spherical cap [0, capAngle] weighted by `solidWeight`
                    // using cumulative solid-angle z-mapping on Fibonacci golden spiral!
                    let placeConeChildren (parentFrame: HyperbolicTransform) (capAngle: float) (kids: int[]) =
                        if kids.Length = 0 then [||]
                        elif kids.Length = 1 && capAngle < Math.PI * 0.8 then
                            let c = kids[0]
                            let len = config.BaseLinkLength * (1. + 0.15 * log (float subtreeSize[c]))
                            let localPt = Hyperbolic3D.fromSpherical len 0. 0.
                            let cFrame =
                                HyperbolicTransform.boostFromOrigin localPt
                                |> HyperbolicTransform.thenApply parentFrame
                                |> HyperbolicTransform.orthonormalize
                            let wPt = HyperbolicTransform.apply cFrame Hyperbolic3D.origin
                            baseX[c] <- wPt.X; baseY[c] <- wPt.Y; baseZ[c] <- wPt.Z; baseW[c] <- wPt.W
                            [| struct(c, cFrame) |]
                        else
                            let ordered = Array.copy kids
                            Array.Sort(ordered, Comparer<int>.Create(fun a b ->
                                let c = compare subtreeSize[b] subtreeSize[a]
                                if c <> 0 then c else compare a b))

                            let totalW = ordered |> Array.sumBy solidWeight
                            let cosMin = cos capAngle // cap covers z in [cosMin, 1.0]
                            let goldenAngle = Math.PI * (3. - sqrt 5.)
                            // Scale link length slightly when branching factor is large so sibling cones never collide in H^3!
                            let branchScale = 1. + 0.22 * log (float (max 1 ordered.Length))
                            let mutable cumW = 0.
                            let out = Array.zeroCreate<struct(int * HyperbolicTransform)> ordered.Length

                            for k = 0 to ordered.Length - 1 do
                                let c = ordered[k]
                                let w = solidWeight c
                                let frac = (cumW + 0.5 * w) / max 1e-9 totalW
                                cumW <- cumW + w
                                // Uniform solid-angle mapping on spherical cap: cos(phi) = 1 - frac * (1 - cosMin)
                                let cosPhi = max -0.9999 (min 0.9999 (1. - frac * (1. - cosMin)))
                                let phi = acos cosPhi
                                let theta = float k * goldenAngle
                                let stepLen = min 8. (config.BaseLinkLength * branchScale * (1. + 0.12 * log (float subtreeSize[c])))
                                let localPt = Hyperbolic3D.fromSpherical stepLen phi theta
                                let sp = sin phi
                                let dir = Point3.create (sp * cos theta) (sp * sin theta) cosPhi
                                let orient = Quaternion.fromUnitVectors (Point3.create 0. 0. 1.) dir
                                let cFrame =
                                    HyperbolicTransform.ofRotation orient
                                    |> HyperbolicTransform.thenApply (HyperbolicTransform.boostFromOrigin localPt)
                                    |> HyperbolicTransform.thenApply parentFrame
                                    |> HyperbolicTransform.orthonormalize
                                let wPt = HyperbolicTransform.apply cFrame Hyperbolic3D.origin
                                baseX[c] <- wPt.X; baseY[c] <- wPt.Y; baseZ[c] <- wPt.Z; baseW[c] <- wPt.W
                                out[k] <- struct(c, cFrame)
                            out

                    let q = Queue<struct(int * HyperbolicTransform)>()
                    if rootIndices.Length = 1 then
                        let r0 = rootIndices[0]
                        baseX[r0] <- 0.; baseY[r0] <- 0.; baseZ[r0] <- 0.; baseW[r0] <- 1.
                        for item in placeConeChildren HyperbolicTransform.identity (Math.PI * 0.95) children[r0] do
                            q.Enqueue item
                    else
                        for item in placeConeChildren HyperbolicTransform.identity (Math.PI * 0.95) rootIndices do
                            q.Enqueue item

                    let mutable visited = 0
                    while q.Count > 0 do
                        Kernels3D.check token visited
                        visited <- visited + 1
                        let struct(u, uFrame) = q.Dequeue()
                        if children[u].Length > 0 then
                            for item in placeConeChildren uFrame config.ConeHalfAngle children[u] do
                                q.Enqueue item

                    let visibleAssocs =
                        if config.IncludeCrossLinks then assocs
                        else assocs |> Array.filter (fun a -> treeEdgeSet.Contains a.Id)
                    let isBackbone = visibleAssocs |> Array.map (fun a -> treeEdgeSet.Contains a.Id)
                    let baseRadii =
                        Array.init n (fun i ->
                            let importance = 1. + 0.35 * log (float subtreeSize[i])
                            max 3.5 (min 18. (5.5 * importance)))

                    Ok
                        { Config = h3CompatConfig
                          Graph = resolved
                          Nodes = nodes
                          NodeIndex = nodeIdx
                          Associations = visibleAssocs
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

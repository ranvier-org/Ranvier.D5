namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Arrangement geometry for tree and spanning-backbone mindmap diagrams.</summary>
[<RequireQualifiedAccess>]
type RadialMode =
    /// <summary>360-degree concentric rings radiating outward from the root.</summary>
    | Concentric
    /// <summary>Balanced left/right bidirectional mindmap branches (<c>@mermaid-js/layout-tidy-tree</c> style).</summary>
    | BidirectionalMindmap

/// <summary>Settings for radial and bidirectional mindmap layouts over any <c>DiagramGraph</c>.</summary>
type RadialTreeConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Concentric 360-degree rings or bidirectional left/right mindmap mode.</summary>
      Mode: RadialMode
      /// <summary>Optional root node ID; when <c>None</c>, chooses initial states or highest-outdegree roots automatically.</summary>
      RootId: string option
      /// <summary>Backbone spanning-tree extraction strategy when the graph contains cycles or cross-links.</summary>
      SpanningStrategy: SpanningTreeStrategy
      /// <summary>Radial ring step or horizontal column gap between tree depths, in layout units.</summary>
      LevelStep: float
      /// <summary>Minimum sibling clearance gap in layout units.</summary>
      NodeGap: float }

/// <summary>Constructors for <c>RadialTreeConfig</c>.</summary>
[<RequireQualifiedAccess>]
module RadialTreeConfig =
    /// <summary>Creates a concentric radial configuration with 110-unit ring steps and 18-unit sibling clearance.</summary>
    let create bounds =
        { Bounds = bounds
          Mode = RadialMode.Concentric
          RootId = None
          SpanningStrategy = SpanningTreeStrategy.Bfs
          LevelStep = 110.
          NodeGap = 18. }

/// <summary>Size-aware concentric radial tree and bidirectional mindmap layout engine.</summary>
[<RequireQualifiedAccess>]
module RadialTree =
    /// <summary>Lays out any <c>DiagramGraph</c> via its spanning forest in concentric rings or bidirectional mindmap branches.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: RadialTreeConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.LevelStep && Double.IsFinite config.NodeGap)
           || config.LevelStep <= 0. || config.NodeGap < 0. then
            Error [ InvalidObservation "RadialTree requires positive finite LevelStep and nonnegative finite NodeGap." ]
        else
            let eff = DiagramGraph.effective graph
            if eff.Nodes.Length = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let forest = SpanningTree.extract config.SpanningStrategy config.RootId graph
                let nodeMap = eff.ReversedIdMap
                let childrenOf idx =
                    forest.ChildrenOf
                    |> Map.tryFind eff.Nodes[idx].Id
                    |> Option.defaultValue []
                    |> List.map (fun cid -> nodeMap[cid])

                let layoutSingleTree (rootIdx: int) : DiagramLayout2D =
                    token.ThrowIfCancellationRequested()
                    // Postorder traversal for subtree leaf/span sizing
                    let order = ResizeArray<int>()
                    let depth = Dictionary<int, int>()
                    let stack = Stack<int * bool>()
                    depth[rootIdx] <- 0
                    stack.Push(rootIdx, false)
                    while stack.Count > 0 do
                        let u, expanded = stack.Pop()
                        if expanded then order.Add u
                        else
                            stack.Push(u, true)
                            let ch = childrenOf u
                            for k = ch.Length - 1 downto 0 do
                                depth[ch[k]] <- depth[u] + 1
                                stack.Push(ch[k], false)

                    let span = Dictionary<int, float>()
                    for u in order do
                        let nd = eff.Nodes[u]
                        let ownExtent =
                            match config.Mode with
                            | RadialMode.Concentric -> Double.Hypot(nd.Size.Width, nd.Size.Height) + config.NodeGap
                            | RadialMode.BidirectionalMindmap -> nd.Size.Height + config.NodeGap
                        let ch = childrenOf u
                        if ch.IsEmpty then
                            span[u] <- ownExtent
                        else
                            let childSum = ch |> List.sumBy (fun c -> span[c])
                            span[u] <- max ownExtent childSum

                    let centers = Dictionary<int, Point>()
                    let cx0 = Bounds.x config.Bounds + Bounds.width config.Bounds * 0.5
                    let cy0 = Bounds.y config.Bounds + Bounds.height config.Bounds * 0.5

                    match config.Mode with
                    | RadialMode.Concentric ->
                        centers[rootIdx] <- Point.create cx0 cy0
                        // Ensure ring radius at each depth d can accommodate total circumference sum at depth d
                        let circByDepth = Dictionary<int, float>()
                        for u in order do
                            let d = depth[u]
                            if d > 0 then
                                let nd = eff.Nodes[u]
                                let w = Double.Hypot(nd.Size.Width, nd.Size.Height) + config.NodeGap
                                circByDepth[d] <- (match circByDepth.TryGetValue d with true, prev -> prev | _ -> 0.) + w
                        let maxDepth = order |> Seq.map (fun u -> depth[u]) |> Seq.max
                        let radiusAt = Array.zeroCreate<float> (maxDepth + 1)
                        for d = 1 to maxDepth do
                            let minR =
                                match circByDepth.TryGetValue d with
                                | true, c -> c / (2. * Math.PI)
                                | _ -> 0.
                            radiusAt[d] <- max (radiusAt[d - 1] + config.LevelStep) minR

                        let rec assignWedge u a0 a1 =
                            let ch = childrenOf u
                            if not ch.IsEmpty then
                                let total = ch |> List.sumBy (fun c -> span[c])
                                let mutable curA = a0
                                for c in ch do
                                    let share = if total <= 1e-9 then (a1 - a0) / float ch.Length else (a1 - a0) * (span[c] / total)
                                    let midA = curA + share * 0.5
                                    let r = radiusAt[depth[c]]
                                    centers[c] <- Point.create (cx0 + r * cos midA) (cy0 + r * sin midA)
                                    assignWedge c curA (curA + share)
                                    curA <- curA + share

                        assignWedge rootIdx 0. (2. * Math.PI)

                    | RadialMode.BidirectionalMindmap ->
                        centers[rootIdx] <- Point.create cx0 cy0
                        let rootChildren = childrenOf rootIdx
                        // Balance root's direct subtrees across right (+1) and left (-1) sides by greedy partition
                        let rightKids = ResizeArray<int>()
                        let leftKids = ResizeArray<int>()
                        let mutable rightSpan = 0.
                        let mutable leftSpan = 0.
                        for c in rootChildren do
                            if rightSpan <= leftSpan then
                                rightKids.Add c
                                rightSpan <- rightSpan + span[c]
                            else
                                leftKids.Add c
                                leftSpan <- leftSpan + span[c]

                        let rec placeBranch (dir: float) (parentX: float) (parentW: float) (kids: int list) (topY: float) =
                            let mutable cursorY = topY
                            for u in kids do
                                let s = span[u]
                                let midY = cursorY + s * 0.5
                                let nd = eff.Nodes[u]
                                let cx = parentX + dir * (parentW * 0.5 + config.LevelStep + nd.Size.Width * 0.5)
                                centers[u] <- Point.create cx midY
                                let subKids = childrenOf u
                                if not subKids.IsEmpty then
                                    let childTotal = subKids |> List.sumBy (fun k -> span[k])
                                    placeBranch dir cx nd.Size.Width subKids (midY - childTotal * 0.5)
                                cursorY <- cursorY + s

                        let rootW = eff.Nodes[rootIdx].Size.Width
                        if rightKids.Count > 0 then
                            placeBranch 1. cx0 rootW (List.ofSeq rightKids) (cy0 - rightSpan * 0.5)
                        if leftKids.Count > 0 then
                            placeBranch -1. cx0 rootW (List.ofSeq leftKids) (cy0 - leftSpan * 0.5)

                    let treeSet = HashSet<int>(order)
                    let subIndices = order |> Seq.sort |> Seq.toArray
                    let posNodes =
                        subIndices
                        |> Array.map (fun idx ->
                            let nd = eff.Nodes[idx]
                            let c = nd.Pinned |> Option.defaultValue centers[idx]
                            let b = Bounds.create (c.X - nd.Size.Width * 0.5) (c.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = b
                              Center = c
                              ParentGroup = nd.ParentGroup
                              Layer = Some depth[idx]
                              Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                    // Filter effective diagram to this component for routing & group bounds
                    let subNodes = subIndices |> Array.map (fun i -> eff.Nodes[i])
                    let subNodeIds = subNodes |> Array.map (fun n -> n.Id) |> Set.ofArray
                    let subGroups =
                        eff.Groups
                        |> Array.indexed
                        |> Array.choose (fun (gi, g) ->
                            // Include group if any descendant node is in this component
                            let hasMember = subNodes |> Array.exists (fun n ->
                                let mutable cur = n.ParentGroup
                                let mutable hit = false
                                while cur.IsSome && not hit do
                                    if cur.Value = g.Id then hit <- true
                                    else
                                        cur <- eff.Groups |> Array.tryFind (fun og -> og.Id = cur.Value) |> Option.bind (fun og -> og.ParentGroup)
                                hit)
                            if hasMember then Some(g, eff.GroupDepth[gi]) else None)
                    let subAssocs =
                        eff.Associations
                        |> Array.filter (fun a -> subNodeIds.Contains a.Source.NodeId && subNodeIds.Contains a.Target.NodeId)
                    let subMap = Dictionary<string, int>(StringComparer.Ordinal)
                    for i = 0 to subNodes.Length - 1 do subMap.Add(subNodes[i].Id, i)
                    let subEff =
                        { Nodes = subNodes
                          Groups = subGroups |> Array.map fst
                          GroupDepth = subGroups |> Array.map snd
                          Associations = subAssocs
                          ReversedIdMap = subMap
                          Constraints = [] }

                    let nBounds = posNodes |> Array.map (fun pn -> pn.Bounds)
                    let groups = GraphKernels.computeGroupBounds subEff nBounds
                    let routed = GraphKernels.routeDirectAssociations subEff posNodes None
                    let nList = Array.toList posNodes
                    let _ = treeSet
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

                let compLayouts =
                    forest.Roots
                    |> List.map (fun rid -> layoutSingleTree nodeMap[rid])
                    |> List.toArray
                Ok(BoxPack.packComponentLayouts config.Bounds (max 24. config.NodeGap) compLayouts)

    /// <summary>Runs radial or bidirectional mindmap layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

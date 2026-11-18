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
            let n = eff.Nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let csr = GraphKernels.buildCsr eff
                let rootIndices, _, childrenIdx, depthArr, _ =
                    SpanningTree.extractIndexed config.SpanningStrategy config.RootId eff csr

                let span = Array.zeroCreate<float> n
                let centers = Array.zeroCreate<Point> n
                let postStack = Array.zeroCreate<int> (max 1 (n * 2))

                let layoutSingleTree (rootIdx: int) : DiagramLayout2D =
                    token.ThrowIfCancellationRequested()
                    // Postorder traversal for subtree leaf/span sizing via flat stack
                    let order = ResizeArray<int>()
                    let mutable sp = 0
                    // Encode (u, expanded) as u (unexpanded >= 0) and ~~~u (expanded < 0)
                    postStack[sp] <- rootIdx
                    sp <- sp + 1
                    while sp > 0 do
                        sp <- sp - 1
                        let raw = postStack[sp]
                        if raw < 0 then
                            order.Add (~~~raw)
                        else
                            let u = raw
                            postStack[sp] <- ~~~u
                            sp <- sp + 1
                            let ch = childrenIdx[u]
                            for k = ch.Length - 1 downto 0 do
                                postStack[sp] <- ch[k]
                                sp <- sp + 1

                    for oi = 0 to order.Count - 1 do
                        let u = order[oi]
                        let nd = eff.Nodes[u]
                        let ownExtent =
                            match config.Mode with
                            | RadialMode.Concentric -> sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height) + config.NodeGap
                            | RadialMode.BidirectionalMindmap -> nd.Size.Height + config.NodeGap
                        let ch = childrenIdx[u]
                        if ch.Length = 0 then
                            span[u] <- ownExtent
                        else
                            let mutable childSum = 0.
                            for k = 0 to ch.Length - 1 do childSum <- childSum + span[ch[k]]
                            span[u] <- max ownExtent childSum

                    let cx0 = Bounds.x config.Bounds + Bounds.width config.Bounds * 0.5
                    let cy0 = Bounds.y config.Bounds + Bounds.height config.Bounds * 0.5

                    match config.Mode with
                    | RadialMode.Concentric ->
                        centers[rootIdx] <- Point.create cx0 cy0
                        let mutable maxDepth = 0
                        for oi = 0 to order.Count - 1 do
                            let d = depthArr[order[oi]]
                            if d > maxDepth then maxDepth <- d
                        let circByDepth = Array.zeroCreate<float> (maxDepth + 1)
                        for oi = 0 to order.Count - 1 do
                            let u = order[oi]
                            let d = depthArr[u]
                            if d > 0 then
                                let nd = eff.Nodes[u]
                                let w = sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height) + config.NodeGap
                                circByDepth[d] <- circByDepth[d] + w
                        let radiusAt = Array.zeroCreate<float> (maxDepth + 1)
                        let invTwoPi = 1. / (2. * Math.PI)
                        for d = 1 to maxDepth do
                            let minR = circByDepth[d] * invTwoPi
                            radiusAt[d] <- max (radiusAt[d - 1] + config.LevelStep) minR

                        let rec assignWedge u a0 a1 =
                            let ch = childrenIdx[u]
                            if ch.Length > 0 then
                                let mutable total = 0.
                                for k = 0 to ch.Length - 1 do total <- total + span[ch[k]]
                                let mutable curA = a0
                                for k = 0 to ch.Length - 1 do
                                    let c = ch[k]
                                    let share = if total <= 1e-9 then (a1 - a0) / float ch.Length else (a1 - a0) * (span[c] / total)
                                    let midA = curA + share * 0.5
                                    let r = radiusAt[depthArr[c]]
                                    centers[c] <- Point.create (cx0 + r * cos midA) (cy0 + r * sin midA)
                                    assignWedge c curA (curA + share)
                                    curA <- curA + share

                        assignWedge rootIdx 0. (2. * Math.PI)

                    | RadialMode.BidirectionalMindmap ->
                        centers[rootIdx] <- Point.create cx0 cy0
                        let rootChildren = childrenIdx[rootIdx]
                        // Balance root's direct subtrees across right (+1) and left (-1) sides by greedy partition
                        let rightKids = ResizeArray<int>(rootChildren.Length)
                        let leftKids = ResizeArray<int>(rootChildren.Length)
                        let mutable rightSpan = 0.
                        let mutable leftSpan = 0.
                        for k = 0 to rootChildren.Length - 1 do
                            let c = rootChildren[k]
                            if rightSpan <= leftSpan then
                                rightKids.Add c
                                rightSpan <- rightSpan + span[c]
                            else
                                leftKids.Add c
                                leftSpan <- leftSpan + span[c]

                        let rec placeBranch (dir: float) (parentX: float) (parentW: float) (kids: int[]) (topY: float) =
                            let mutable cursorY = topY
                            for k = 0 to kids.Length - 1 do
                                let u = kids[k]
                                let s = span[u]
                                let midY = cursorY + s * 0.5
                                let nd = eff.Nodes[u]
                                let cx = parentX + dir * (parentW * 0.5 + config.LevelStep + nd.Size.Width * 0.5)
                                centers[u] <- Point.create cx midY
                                let subKids = childrenIdx[u]
                                if subKids.Length > 0 then
                                    let mutable childTotal = 0.
                                    for sk = 0 to subKids.Length - 1 do childTotal <- childTotal + span[subKids[sk]]
                                    placeBranch dir cx nd.Size.Width subKids (midY - childTotal * 0.5)
                                cursorY <- cursorY + s

                        let rootW = eff.Nodes[rootIdx].Size.Width
                        if rightKids.Count > 0 then
                            placeBranch 1. cx0 rootW (rightKids.ToArray()) (cy0 - rightSpan * 0.5)
                        if leftKids.Count > 0 then
                            placeBranch -1. cx0 rootW (leftKids.ToArray()) (cy0 - leftSpan * 0.5)

                    let subIndices =
                        if order.Count = n then Array.init n id
                        else
                            let arr = order.ToArray()
                            Array.Sort(arr)
                            arr
                    let posNodes =
                        Array.init subIndices.Length (fun li ->
                            let idx = subIndices[li]
                            let nd = eff.Nodes[idx]
                            let c = match nd.Pinned with Some p -> p | None -> centers[idx]
                            let b = Bounds.create (c.X - nd.Size.Width * 0.5) (c.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = b
                              Center = c
                              ParentGroup = nd.ParentGroup
                              Layer = Some depthArr[idx]
                              Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                    let subGroups = GraphKernels.filterSubGroups eff subIndices
                    let subEff = GraphKernels.sliceComponentEff eff csr subIndices subGroups

                    let groups =
                        if subEff.Groups.Length = 0 then []
                        else GraphKernels.computeGroupBounds subEff (Array.init posNodes.Length (fun i -> posNodes[i].Bounds))
                    let routed = GraphKernels.routeDirectAssociations subEff posNodes None
                    let mutable nList = []
                    for i = posNodes.Length - 1 downto 0 do nList <- posNodes[i] :: nList
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

                let compLayouts = Array.init rootIndices.Length (fun r -> layoutSingleTree rootIndices[r])
                Ok(BoxPack.packComponentLayouts config.Bounds (max 24. config.NodeGap) compLayouts)

    /// <summary>Runs radial or bidirectional mindmap layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

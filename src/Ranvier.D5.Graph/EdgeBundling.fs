namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

module private BundleCommon =
    let inline lerp a b t = a + (b - a) * t

    let resolveEndpointAnchor (node: PositionedNode) (ep: Endpoint) (toward: Point) : Point =
        match ep.PortId with
        | Some pid ->
            node.Ports
            |> List.tryFind (fun p -> p.Id = pid)
            |> Option.map (fun p -> p.Anchor)
            |> Option.defaultValue (GraphKernels.clipRectBoundary node.Bounds toward)
        | None -> GraphKernels.clipRectBoundary node.Bounds toward

    let syntheticAssoc (ra: RoutedAssociation) : Association =
        let toSize (lblOpt: PositionedLabel option) : TextSize option =
            lblOpt |> Option.map (fun l -> { TextSize.Width = Bounds.width l.Bounds; Height = Bounds.height l.Bounds })
        { Id = ra.Id
          Source = ra.Source
          Target = ra.Target
          Kind = ra.Kind
          Weight = 1.0
          MinLength = 1
          SourceLabel = toSize ra.SourceLabel
          CenterLabel = toSize ra.CenterLabel
          TargetLabel = toSize ra.TargetLabel }

    let updateRouted (assocOpt: Association option) (ra: RoutedAssociation) (pts: Point[]) (path: Path) : RoutedAssociation =
        let assoc = match assocOpt with Some a -> a | None -> syntheticAssoc ra
        let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints assoc pts
        let mutable wpList = []
        for i = pts.Length - 1 downto 0 do
            wpList <- pts[i] :: wpList
        { ra with
            Waypoints = wpList
            Path = path
            SourceLabel = sLbl
            CenterLabel = mLbl
            TargetLabel = tLbl }

    let validateGraphAndLayout (graph: DiagramGraph) (layout: DiagramLayout2D) =
        let eff = DiagramGraph.effective graph
        let nodeById = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
        for pn in layout.Nodes do
            nodeById[pn.Id] <- pn
        let assocById = Dictionary<string, Association>(StringComparer.Ordinal)
        for a in eff.Associations do
            assocById[a.Id] <- a
        let mutable missing : string option = None
        for i = 0 to eff.Nodes.Length - 1 do
            let nid = eff.Nodes[i].Id
            if missing.IsNone && not (nodeById.ContainsKey nid) then
                missing <- Some nid
        for ra in layout.Associations do
            if missing.IsNone && not (nodeById.ContainsKey ra.Source.NodeId) then
                missing <- Some ra.Source.NodeId
            if missing.IsNone && not (nodeById.ContainsKey ra.Target.NodeId) then
                missing <- Some ra.Target.NodeId
        match missing with
        | Some mid -> Error [ MissingEndpoint mid ]
        | None -> Ok(eff, nodeById, assocById)

    let smoothClampedSplinePath (pts: Point[]) : Path =
        if pts.Length = 0 then Path.empty
        elif pts.Length = 1 then Path.ofCommands [ MoveTo pts[0]; LineTo pts[0] ]
        elif pts.Length = 2 then Path.ofCommands [ MoveTo pts[0]; LineTo pts[1] ]
        else
            let optPts = [ for i in 0 .. pts.Length - 1 -> Some pts[i] ]
            Curves.line (CurveSpec.Basis CurveBoundary.Clamped) optPts

/// <summary>Holten (2006) Hierarchical Edge Bundling (HEB) routing associations through Lowest Common Ancestor spline control chains.</summary>
[<RequireQualifiedAccess>]
module HierarchicalBundle =
    let private bundleCore
        (token: System.Threading.CancellationToken)
        (beta: float)
        (hierarchy: HierarchyNode<string>)
        (assocLookup: Dictionary<string, Association> option)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite beta) || beta < 0. || beta > 1. then
            Error [ InvalidObservation "HierarchicalBundle beta must be finite and in [0.0, 1.0]." ]
        else
            let itemsRes =
                try Ok(Hierarchy.descendants hierarchy)
                with :? ArgumentException as ex -> Error [ InvalidTopology ex.Message ]
            match itemsRes with
            | Error errs -> Error errs
            | Ok items ->
                let hCount = items.Length
                if hCount = 0 || layout.Associations.IsEmpty then
                    Ok layout
                else
                    let nodeById = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
                    for pn in layout.Nodes do nodeById[pn.Id] <- pn
                    let groupById = Dictionary<string, PositionedGroup>(StringComparer.Ordinal)
                    for pg in layout.Groups do groupById[pg.Id] <- pg

                    let hItems = List.toArray items
                    let idToH = Dictionary<string, int>(StringComparer.Ordinal)
                    for i = 0 to hCount - 1 do
                        idToH[hItems[i].Id] <- i

                    let parentH = Array.create hCount -1
                    let depthH = Array.zeroCreate<int> hCount
                    let childBuckets = Array.init hCount (fun _ -> ResizeArray<int>())
                    for i = 0 to hCount - 1 do
                        depthH[i] <- hItems[i].Depth
                        match hItems[i].Parent with
                        | Some pid ->
                            match idToH.TryGetValue pid with
                            | true, pi ->
                                parentH[i] <- pi
                                childBuckets[pi].Add i
                            | _ -> ()
                        | None -> ()

                    // Compute 2D position for every hierarchy item bottom-up (reverse preorder).
                    let posH = Array.zeroCreate<Point> hCount
                    let hasPosH = Array.zeroCreate<bool> hCount
                    let fallbackCenter =
                        Point.create
                            (Bounds.x layout.Bounds + Bounds.width layout.Bounds * 0.5)
                            (Bounds.y layout.Bounds + Bounds.height layout.Bounds * 0.5)

                    for i = hCount - 1 downto 0 do
                        let hid = hItems[i].Id
                        match nodeById.TryGetValue hid with
                        | true, pn ->
                            posH[i] <- pn.Center
                            hasPosH[i] <- true
                        | _ ->
                            match groupById.TryGetValue hid with
                            | true, pg ->
                                let b = pg.Bounds
                                posH[i] <- Point.create (Bounds.x b + Bounds.width b * 0.5) (Bounds.y b + Bounds.height b * 0.5)
                                hasPosH[i] <- true
                            | _ ->
                                let kids = childBuckets[i]
                                let mutable sx = 0.
                                let mutable sy = 0.
                                let mutable cnt = 0
                                for k = 0 to kids.Count - 1 do
                                    let ci = kids[k]
                                    if hasPosH[ci] then
                                        sx <- sx + posH[ci].X
                                        sy <- sy + posH[ci].Y
                                        cnt <- cnt + 1
                                if cnt > 0 then
                                    posH[i] <- Point.create (sx / float cnt) (sy / float cnt)
                                    hasPosH[i] <- true
                                else
                                    posH[i] <- fallbackCenter

                    let mutable missingId : string option = None
                    for ra in layout.Associations do
                        let sId = ra.Source.NodeId
                        let tId = ra.Target.NodeId
                        if missingId.IsNone && (not (nodeById.ContainsKey sId) || not (idToH.ContainsKey sId)) then
                            missingId <- Some sId
                        if missingId.IsNone && (not (nodeById.ContainsKey tId) || not (idToH.ContainsKey tId)) then
                            missingId <- Some tId

                    match missingId with
                    | Some mid -> Error [ MissingEndpoint mid ]
                    | None ->
                        let upBuf = ResizeArray<int>(16)
                        let downBuf = ResizeArray<int>(16)
                        let rawArr = layout.Associations |> List.toArray
                        let outArr = Array.zeroCreate<RoutedAssociation> rawArr.Length

                        for e = 0 to rawArr.Length - 1 do
                            SimdMath.check token e
                            let ra = rawArr[e]
                            let sId = ra.Source.NodeId
                            let tId = ra.Target.NodeId
                            if sId = tId then
                                outArr[e] <- ra
                            else
                                let uNode = nodeById[sId]
                                let vNode = nodeById[tId]
                                let mutable u = idToH[sId]
                                let mutable v = idToH[tId]
                                upBuf.Clear()
                                downBuf.Clear()
                                while depthH[u] > depthH[v] do
                                    upBuf.Add u
                                    u <- parentH[u]
                                while depthH[v] > depthH[u] do
                                    downBuf.Add v
                                    v <- parentH[v]
                                while u <> v && u >= 0 && v >= 0 do
                                    upBuf.Add u
                                    downBuf.Add v
                                    u <- parentH[u]
                                    v <- parentH[v]

                                let lca = if u >= 0 then u else idToH[sId]
                                let chainLen = upBuf.Count + 1 + downBuf.Count
                                let chainIdx = Array.zeroCreate<int> chainLen
                                for k = 0 to upBuf.Count - 1 do
                                    chainIdx[k] <- upBuf[k]
                                chainIdx[upBuf.Count] <- lca
                                for k = 0 to downBuf.Count - 1 do
                                    chainIdx[upBuf.Count + 1 + k] <- downBuf[downBuf.Count - 1 - k]

                                let rawPts = Array.init chainLen (fun k -> posH[chainIdx[k]])
                                let firstAim = if chainLen >= 2 then rawPts[1] else vNode.Center
                                let lastAim = if chainLen >= 2 then rawPts[chainLen - 2] else uNode.Center
                                rawPts[0] <- BundleCommon.resolveEndpointAnchor uNode ra.Source firstAim
                                rawPts[chainLen - 1] <- BundleCommon.resolveEndpointAnchor vNode ra.Target lastAim

                                let p0 = rawPts[0]
                                let pEnd = rawPts[chainLen - 1]
                                let straightened =
                                    if chainLen <= 2 then rawPts
                                    else
                                        Array.init chainLen (fun i ->
                                            let t = float i / float (chainLen - 1)
                                            let chordX = BundleCommon.lerp p0.X pEnd.X t
                                            let chordY = BundleCommon.lerp p0.Y pEnd.Y t
                                            Point.create
                                                (BundleCommon.lerp chordX rawPts[i].X beta)
                                                (BundleCommon.lerp chordY rawPts[i].Y beta))

                                let path =
                                    if chainLen <= 2 || beta <= 1e-6 then
                                        Path.ofCommands [ MoveTo p0; LineTo pEnd ]
                                    else
                                        let optPts = [ for i in 0 .. chainLen - 1 -> Some rawPts[i] ]
                                        Curves.line (CurveSpec.Bundle beta) optPts

                                let assocOpt =
                                    match assocLookup with
                                    | Some dict ->
                                        match dict.TryGetValue ra.Id with
                                        | true, a -> Some a
                                        | _ -> None
                                    | None -> None
                                outArr[e] <- BundleCommon.updateRouted assocOpt ra straightened path

                        let routed = Array.toList outArr
                        Ok
                            { layout with
                                Bounds = GraphKernels.computeDiagramBounds layout.Bounds layout.Nodes layout.Groups routed
                                Associations = routed }

    /// <summary>Bundles all associations of <c>layout</c> along LCA paths in <c>hierarchy</c> with bundling strength <c>beta</c> in <c>[0.0, 1.0]</c> and cooperative cancellation.</summary>
    let bundleWithCancellation
        (token: System.Threading.CancellationToken)
        (beta: float)
        (hierarchy: HierarchyNode<string>)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        bundleCore token beta hierarchy None layout

    /// <summary>Bundles all associations of <c>layout</c> along LCA paths in <c>hierarchy</c> with bundling strength <c>beta</c> in <c>[0.0, 1.0]</c>.</summary>
    let bundle
        (beta: float)
        (hierarchy: HierarchyNode<string>)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        bundleCore System.Threading.CancellationToken.None beta hierarchy None layout

    /// <summary>Derives a tree hierarchy from <c>graph</c>'s compound groups (or spanning forest when ungrouped) and applies Holten hierarchical edge bundling with cooperative cancellation.</summary>
    let bundleGraphWithCancellation
        (token: System.Threading.CancellationToken)
        (beta: float)
        (graph: DiagramGraph)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        match BundleCommon.validateGraphAndLayout graph layout with
        | Error errs -> Error errs
        | Ok(eff, _, assocById) ->
            let reserved = HashSet<string>(StringComparer.Ordinal)
            for nd in eff.Nodes do reserved.Add nd.Id |> ignore
            for g in eff.Groups do reserved.Add g.Id |> ignore
            let mutable rootId = "__heb_root__"
            let mutable sfx = 1
            while not (reserved.Add rootId) do
                rootId <- "__heb_root__" + string sfx
                sfx <- sfx + 1

            let hierarchyRes =
                if eff.Groups.Length > 0 then
                    let rows =
                        [ for g in eff.Groups do yield g.Id, g.ParentGroup
                          for nd in eff.Nodes do yield nd.Id, nd.ParentGroup ]
                    Hierarchy.forest rootId fst snd rows
                else
                    let forest = SpanningTree.extract SpanningTreeStrategy.Bfs None graph
                    let rows = [ for nd in eff.Nodes -> nd.Id, Map.tryFind nd.Id forest.ParentOf |> Option.flatten ]
                    Hierarchy.forest rootId fst snd rows

            match hierarchyRes with
            | Error errs -> Error errs
            | Ok h -> bundleCore token beta h (Some assocById) layout

    /// <summary>Derives a tree hierarchy from <c>graph</c> and applies Holten hierarchical edge bundling without cancellation.</summary>
    let bundleGraph beta graph layout =
        bundleGraphWithCancellation System.Threading.CancellationToken.None beta graph layout

/// <summary>Configuration for Holten &amp; van Wijk (2009) Force-Directed Edge Bundling with Selassie, Heller &amp; Heer (2011) directional lane separation.</summary>
type DividedBundleConfig =
    { /// <summary>Minimum geometric compatibility product <c>C_a * C_s * C_p * C_v</c> in <c>[0.0, 1.0]</c> for two edges to attract.</summary>
      CompatibilityThreshold: float
      /// <summary>Number of subdivision doubling cycles (<c>2^Cycles</c> segments per bundled edge).</summary>
      Cycles: int
      /// <summary>Initial iteration count in cycle 0 (decays by 2/3 each subsequent cycle).</summary>
      IterationsInitial: int
      /// <summary>Initial advection step factor (decays by 1/2 each cycle).</summary>
      StepSize: float
      /// <summary>Global spring stiffness opposing excessive lateral bending.</summary>
      Stiffness: float
      /// <summary>When true, separates antiparallel <c>u -&gt; v</c> and <c>v -&gt; u</c> traffic into distinct opposing lanes.</summary>
      DirectionalSeparation: bool
      /// <summary>Target lateral distance in layout units between opposing directional lanes.</summary>
      LaneWidth: float }

/// <summary>Default configuration values for <c>DividedBundleConfig</c>.</summary>
[<RequireQualifiedAccess>]
module DividedBundleConfig =
    /// <summary>Default Divided FDEB settings: 0.6 compatibility threshold, 5 subdivision cycles, 50 initial iterations, 0.04 step size, 0.1 stiffness, directional separation enabled and 12-unit lane width.</summary>
    let defaultValue =
        { CompatibilityThreshold = 0.6
          Cycles = 5
          IterationsInitial = 50
          StepSize = 0.04
          Stiffness = 0.1
          DirectionalSeparation = true
          LaneWidth = 12.0 }

/// <summary>Force-Directed &amp; Divided Edge Bundling (Holten &amp; van Wijk 2009; Selassie, Heller &amp; Heer 2011).</summary>
[<RequireQualifiedAccess>]
module DividedForceBundle =
    let private visibilityFactor (p0x: float) (p0y: float) (pdx: float) (pdy: float) (pLenSq: float) (pmx: float) (pmy: float) (q0x: float) (q0y: float) (q1x: float) (q1y: float) =
        let t0 = ((q0x - p0x) * pdx + (q0y - p0y) * pdy) / pLenSq
        let t1 = ((q1x - p0x) * pdx + (q1y - p0y) * pdy) / pLenSq
        let i0x, i0y = p0x + t0 * pdx, p0y + t0 * pdy
        let i1x, i1y = p0x + t1 * pdx, p0y + t1 * pdy
        let imx, imy = 0.5 * (i0x + i1x), 0.5 * (i0y + i1y)
        let iLen = Double.Hypot(i1x - i0x, i1y - i0y)
        let distMid = Double.Hypot(pmx - imx, pmy - imy)
        if iLen <= 1e-6 then 0.
        else max 0. (1. - 2. * distMid / iLen)

    /// <summary>Bundles all non-loop associations of <c>layout</c> using compatible force attraction and Selassie directional lane separation with cooperative cancellation.</summary>
    let bundleWithCancellation
        (token: System.Threading.CancellationToken)
        (config: DividedBundleConfig)
        (graph: DiagramGraph)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.CompatibilityThreshold && Double.IsFinite config.StepSize
                && Double.IsFinite config.Stiffness && Double.IsFinite config.LaneWidth)
           || config.CompatibilityThreshold < 0. || config.CompatibilityThreshold > 1.
           || config.Cycles < 1 || config.Cycles > 8
           || config.IterationsInitial < 1 || config.IterationsInitial > 500
           || config.StepSize <= 0. || config.Stiffness < 0. || config.LaneWidth < 0. then
            Error [ InvalidObservation "DividedForceBundle requires threshold in [0,1], Cycles in 1..8, IterationsInitial in 1..500, positive StepSize and nonnegative Stiffness/LaneWidth." ]
        else
            match BundleCommon.validateGraphAndLayout graph layout with
            | Error errs -> Error errs
            | Ok(_, nodeById, assocById) ->
                let rawAssocs = layout.Associations |> List.toArray
                let totalAssocs = rawAssocs.Length
                if totalAssocs <= 1 then
                    Ok layout
                else
                    // Extract non-degenerate non-self-loop associations for bundling.
                    let activeMap = ResizeArray<int>(totalAssocs)
                    let p0x = ResizeArray<float>(totalAssocs)
                    let p0y = ResizeArray<float>(totalAssocs)
                    let p1x = ResizeArray<float>(totalAssocs)
                    let p1y = ResizeArray<float>(totalAssocs)

                    for idx = 0 to totalAssocs - 1 do
                        let ra = rawAssocs[idx]
                        if ra.Source.NodeId <> ra.Target.NodeId then
                            let uNode = nodeById[ra.Source.NodeId]
                            let vNode = nodeById[ra.Target.NodeId]
                            let sPt = BundleCommon.resolveEndpointAnchor uNode ra.Source vNode.Center
                            let tPt = BundleCommon.resolveEndpointAnchor vNode ra.Target uNode.Center
                            if Double.Hypot(tPt.X - sPt.X, tPt.Y - sPt.Y) > 1e-3 then
                                activeMap.Add idx
                                p0x.Add sPt.X; p0y.Add sPt.Y
                                p1x.Add tPt.X; p1y.Add tPt.Y

                    let eCount = activeMap.Count
                    if eCount <= 1 then
                        Ok layout
                    else
                        let sX = p0x.ToArray()
                        let sY = p0y.ToArray()
                        let tX = p1x.ToArray()
                        let tY = p1y.ToArray()
                        let dxArr = Array.init eCount (fun e -> tX[e] - sX[e])
                        let dyArr = Array.init eCount (fun e -> tY[e] - sY[e])
                        let lenArr = Array.init eCount (fun e -> Double.Hypot(dxArr[e], dyArr[e]))
                        let midX = Array.init eCount (fun e -> 0.5 * (sX[e] + tX[e]))
                        let midY = Array.init eCount (fun e -> 0.5 * (sY[e] + tY[e]))
                        // Unit left normal n_P = (-dy / |P|, dx / |P|) -> antiparallel P and Q have opposite normals!
                        let normX = Array.init eCount (fun e -> -dyArr[e] / lenArr[e])
                        let normY = Array.init eCount (fun e -> dxArr[e] / lenArr[e])

                        let mutable meanLen = 0.
                        for e = 0 to eCount - 1 do meanLen <- meanLen + lenArr[e]
                        meanLen <- meanLen / float eCount

                        // Precompute sparse symmetric compatibility CSR (C_a * C_s * C_p * C_v >= tau) in flat arrays!
                        let tau = config.CompatibilityThreshold
                        let pairU = ResizeArray<int>()
                        let pairV = ResizeArray<int>()
                        let pairW = ResizeArray<float>()
                        let pairSame = ResizeArray<bool>()
                        let compatDeg = Array.zeroCreate<int> eCount

                        for p = 0 to eCount - 2 do
                            SimdMath.check token p
                            let lp = lenArr[p]
                            let lpSq = lp * lp
                            let pdx, pdy = dxArr[p], dyArr[p]
                            let pmx, pmy = midX[p], midY[p]
                            for q = p + 1 to eCount - 1 do
                                let lq = lenArr[q]
                                let lAvg = 0.5 * (lp + lq)
                                let minL, maxL = min lp lq, max lp lq
                                let cs = 2. / ((lAvg / minL) + (maxL / lAvg))
                                if cs >= tau then
                                    let distMid = Double.Hypot(pmx - midX[q], pmy - midY[q])
                                    let cp = lAvg / (lAvg + distMid)
                                    if cs * cp >= tau then
                                        let dot = (pdx * dxArr[q] + pdy * dyArr[q]) / (lp * lq)
                                        let ca = min 1. (abs dot)
                                        if ca * cs * cp >= tau then
                                            let vp = visibilityFactor sX[p] sY[p] pdx pdy lpSq pmx pmy sX[q] sY[q] tX[q] tY[q]
                                            if ca * cs * cp * vp >= tau then
                                                let vq = visibilityFactor sX[q] sY[q] dxArr[q] dyArr[q] (lq * lq) midX[q] midY[q] sX[p] sY[p] tX[p] tY[p]
                                                let ce = ca * cs * cp * min vp vq
                                                if ce >= tau then
                                                    pairU.Add p
                                                    pairV.Add q
                                                    pairW.Add ce
                                                    pairSame.Add (dot >= 0.)
                                                    compatDeg[p] <- compatDeg[p] + 1
                                                    compatDeg[q] <- compatDeg[q] + 1

                        let compatOffsets = Array.zeroCreate<int> (eCount + 1)
                        for e = 0 to eCount - 1 do
                            compatOffsets[e + 1] <- compatOffsets[e] + compatDeg[e]
                        let totalCompat = compatOffsets[eCount]
                        let compatTargets = Array.zeroCreate<int> totalCompat
                        let compatWeights = Array.zeroCreate<float> totalCompat
                        let compatSameDir = Array.zeroCreate<bool> totalCompat
                        let cursor = Array.copy compatOffsets

                        for k = 0 to pairU.Count - 1 do
                            let u = pairU[k]
                            let v = pairV[k]
                            let w = pairW[k]
                            let sd = pairSame[k]
                            let cu = cursor[u]
                            compatTargets[cu] <- v
                            compatWeights[cu] <- w
                            compatSameDir[cu] <- sd
                            cursor[u] <- cu + 1
                            let cv = cursor[v]
                            compatTargets[cv] <- u
                            compatWeights[cv] <- w
                            compatSameDir[cv] <- sd
                            cursor[v] <- cv + 1

                        // Flat polyline storage: eCount * curPts
                        let mutable curPts = 2
                        let mutable px = Array.zeroCreate<float> (eCount * 2)
                        let mutable py = Array.zeroCreate<float> (eCount * 2)
                        for e = 0 to eCount - 1 do
                            px[e * 2] <- sX[e]; py[e * 2] <- sY[e]
                            px[e * 2 + 1] <- tX[e]; py[e * 2 + 1] <- tY[e]

                        let mutable iters = config.IterationsInitial
                        let mutable stepScale = config.StepSize * max 24. (meanLen * 0.35)
                        let softCore = max 2. (if config.LaneWidth > 0. then config.LaneWidth * 0.35 else 4.)

                        for _cycle = 0 to config.Cycles - 1 do
                            token.ThrowIfCancellationRequested()
                            // Subdivide 1 -> 2 -> 4 -> ... segments
                            let prevPts = curPts
                            let prevSegs = prevPts - 1
                            let nextSegs = prevSegs * 2
                            let nextPts = nextSegs + 1
                            let nextPx = Array.zeroCreate<float> (eCount * nextPts)
                            let nextPy = Array.zeroCreate<float> (eCount * nextPts)

                            for e = 0 to eCount - 1 do
                                let pBase = e * prevPts
                                let nBase = e * nextPts
                                for s = 0 to prevSegs - 1 do
                                    let ax, ay = px[pBase + s], py[pBase + s]
                                    let bx, by = px[pBase + s + 1], py[pBase + s + 1]
                                    nextPx[nBase + 2 * s] <- ax
                                    nextPy[nBase + 2 * s] <- ay
                                    nextPx[nBase + 2 * s + 1] <- 0.5 * (ax + bx)
                                    nextPy[nBase + 2 * s + 1] <- 0.5 * (ay + by)
                                nextPx[nBase + nextSegs] <- px[pBase + prevSegs]
                                nextPy[nBase + nextSegs] <- py[pBase + prevSegs]

                            px <- nextPx
                            py <- nextPy
                            curPts <- nextPts

                            let bufX = Array.copy px
                            let bufY = Array.copy py
                            let springAlpha = min 0.42 (config.Stiffness * 2.2)

                            for it = 0 to iters - 1 do
                                SimdMath.check token it
                                let cool = 1. - 0.35 * (float it / float (max 1 iters))
                                let curStep = stepScale * cool

                                for e = 0 to eCount - 1 do
                                    let baseOff = e * curPts
                                    let cStart = compatOffsets[e]
                                    let cStop = compatOffsets[e + 1]
                                    if cStart = cStop then
                                        // Pure spring relaxation if no compatible partners
                                        for i = 1 to curPts - 2 do
                                            let idx = baseOff + i
                                            bufX[idx] <- px[idx] + springAlpha * 0.5 * (px[idx - 1] - 2. * px[idx] + px[idx + 1])
                                            bufY[idx] <- py[idx] + springAlpha * 0.5 * (py[idx - 1] - 2. * py[idx] + py[idx + 1])
                                    else
                                        let nPx, nPy = normX[e], normY[e]
                                        // Taper lane separation smoothly to 0 at endpoints so anchors stay exact without sharp hooks
                                        for i = 1 to curPts - 2 do
                                            let idx = baseOff + i
                                            let curX, curY = px[idx], py[idx]
                                            let frac = float i / float (curPts - 1)
                                            let taper = sin (Math.PI * frac)
                                            let laneShift = if config.DirectionalSeparation then config.LaneWidth * taper else 0.

                                            let mutable attrX = 0.
                                            let mutable attrY = 0.
                                            let mutable wSum = 0.
                                            for cIdx = cStart to cStop - 1 do
                                                let q = compatTargets[cIdx]
                                                let w = compatWeights[cIdx]
                                                let same = compatSameDir[cIdx]
                                                let qPtIdx =
                                                    if config.DirectionalSeparation && not same then
                                                        q * curPts + (curPts - 1 - i)
                                                    else
                                                        q * curPts + i
                                                let targetX, targetY =
                                                    if config.DirectionalSeparation && not same then
                                                        px[qPtIdx] + laneShift * nPx, py[qPtIdx] + laneShift * nPy
                                                    else
                                                        px[qPtIdx], py[qPtIdx]
                                                let dx = targetX - curX
                                                let dy = targetY - curY
                                                let dist = Double.Hypot(dx, dy)
                                                let invDenom = w / (dist + softCore)
                                                attrX <- attrX + dx * invDenom
                                                attrY <- attrY + dy * invDenom
                                                wSum <- wSum + w

                                            let normW = 1. / max 1. wSum
                                            let springX = springAlpha * (px[idx - 1] - 2. * curX + px[idx + 1]) * 0.5
                                            let springY = springAlpha * (py[idx - 1] - 2. * curY + py[idx + 1]) * 0.5
                                            bufX[idx] <- curX + springX + attrX * normW * curStep
                                            bufY[idx] <- curY + springY + attrY * normW * curStep

                                Array.blit bufX 0 px 0 px.Length
                                Array.blit bufY 0 py 0 py.Length

                            iters <- max 6 ((iters * 2) / 3)
                            stepScale <- stepScale * 0.55

                        let outAssocs = Array.copy rawAssocs
                        for e = 0 to eCount - 1 do
                            let origIdx = activeMap[e]
                            let ra = rawAssocs[origIdx]
                            let baseOff = e * curPts
                            let pts = Array.init curPts (fun i -> Point.create px[baseOff + i] py[baseOff + i])
                            let path = BundleCommon.smoothClampedSplinePath pts
                            let assocOpt =
                                match assocById.TryGetValue ra.Id with
                                | true, a -> Some a
                                | _ -> None
                            outAssocs[origIdx] <- BundleCommon.updateRouted assocOpt ra pts path

                        let routed = Array.toList outAssocs
                        Ok
                            { layout with
                                Bounds = GraphKernels.computeDiagramBounds layout.Bounds layout.Nodes layout.Groups routed
                                Associations = routed }

    /// <summary>Bundles all non-loop associations of <c>layout</c> using compatible force attraction and Selassie directional lane separation without cancellation.</summary>
    let bundle config graph layout =
        bundleWithCancellation System.Threading.CancellationToken.None config graph layout

/// <summary>Configuration for Gansner, Hu, North &amp; Scheidegger (2011) Multilevel Agglomerative Edge Bundling (<c>MINGLE</c>).</summary>
type MingleConfig =
    { /// <summary>Maximum allowed turning angle in radians <c>(0, pi)</c> at bundle fan-in / fan-out junctions.</summary>
      MaxTurningAngleRadians: float
      /// <summary>Number of nearest neighbors queried in 4D edge endpoint space <c>(x_s, y_s, x_t, y_t)</c> per bundle.</summary>
      Neighbors: int
      /// <summary>Spline curvature smoothing factor in <c>[0.0, 1.0]</c> (<c>0.0</c> produces crisp Y-trunk-Y polylines).</summary>
      Curvature: float }

/// <summary>Default configuration values for <c>MingleConfig</c>.</summary>
[<RequireQualifiedAccess>]
module MingleConfig =
    /// <summary>Default MINGLE settings: 40-degree (<c>pi / 4.5</c>) max turning angle, 10 nearest 4D neighbors and 0.85 spline curvature.</summary>
    let defaultValue =
        { MaxTurningAngleRadians = Math.PI / 4.5
          Neighbors = 10
          Curvature = 0.85 }

/// <summary>Gansner, Hu, North &amp; Scheidegger (2011) <c>O(E log E)</c> Multilevel Agglomerative Ink-Saving Edge Bundling (<c>MINGLE</c>).</summary>
[<RequireQualifiedAccess>]
module MingleBundle =
    [<Struct>]
    type private KdNode4D =
        val Min0: float
        val Min1: float
        val Min2: float
        val Min3: float
        val Max0: float
        val Max1: float
        val Max2: float
        val Max3: float
        val Start: int
        val Count: int
        val Left: int
        val Right: int
        new(min0, min1, min2, min3, max0, max1, max2, max3, start, count, left, right) =
            { Min0 = min0; Min1 = min1; Min2 = min2; Min3 = min3
              Max0 = max0; Max1 = max1; Max2 = max2; Max3 = max3
              Start = start; Count = count; Left = left; Right = right }

    let private buildKdTree4D (c0: float[]) (c1: float[]) (c2: float[]) (c3: float[]) (count: int) : KdNode4D[] * int[] =
        let perm = Array.init count (fun i -> i)
        if count = 0 then
            [||], perm
        else
            let nodes = ResizeArray<KdNode4D>(max 4 (count / 2))
            let rec build (offset: int) (len: int) : int =
                let slot = nodes.Count
                nodes.Add(Unchecked.defaultof<KdNode4D>)
                let i0 = perm[offset]
                let mutable mn0, mx0 = c0[i0], c0[i0]
                let mutable mn1, mx1 = c1[i0], c1[i0]
                let mutable mn2, mx2 = c2[i0], c2[i0]
                let mutable mn3, mx3 = c3[i0], c3[i0]
                for k = 1 to len - 1 do
                    let idx = perm[offset + k]
                    let v0, v1, v2, v3 = c0[idx], c1[idx], c2[idx], c3[idx]
                    if v0 < mn0 then mn0 <- v0
                    if v0 > mx0 then mx0 <- v0
                    if v1 < mn1 then mn1 <- v1
                    if v1 > mx1 then mx1 <- v1
                    if v2 < mn2 then mn2 <- v2
                    if v2 > mx2 then mx2 <- v2
                    if v3 < mn3 then mn3 <- v3
                    if v3 > mx3 then mx3 <- v3

                if len <= 8 then
                    nodes[slot] <- KdNode4D(mn0, mn1, mn2, mn3, mx0, mx1, mx2, mx3, offset, len, -1, -1)
                    slot
                else
                    let s0, s1, s2, s3 = mx0 - mn0, mx1 - mn1, mx2 - mn2, mx3 - mn3
                    let axisCoords =
                        if s0 >= s1 && s0 >= s2 && s0 >= s3 then c0
                        elif s1 >= s2 && s1 >= s3 then c1
                        elif s2 >= s3 then c2
                        else c3

                    let sub = Array.sub perm offset len
                    Array.sortInPlaceWith (fun a b -> compare axisCoords[a] axisCoords[b]) sub
                    Array.blit sub 0 perm offset len

                    let mid = len / 2
                    let leftSlot = build offset mid
                    let rightSlot = build (offset + mid) (len - mid)
                    nodes[slot] <- KdNode4D(mn0, mn1, mn2, mn3, mx0, mx1, mx2, mx3, offset, 0, leftSlot, rightSlot)
                    slot

            build 0 count |> ignore
            nodes.ToArray(), perm

    let private queryKnn4D
        (tree: KdNode4D[])
        (perm: int[])
        (c0: float[])
        (c1: float[])
        (c2: float[])
        (c3: float[])
        (queryIdx: int)
        (kMax: int)
        (outIdx: int[])
        (outDistSq: float[])
        (stack: int[])
        : int =
        if tree.Length = 0 || kMax <= 0 then 0
        else
            let q0, q1, q2, q3 = c0[queryIdx], c1[queryIdx], c2[queryIdx], c3[queryIdx]
            let mutable found = 0

            let inline insertCandidate (cand: int) (dSq: float) =
                if found < kMax then
                    let mutable pos = found
                    while pos > 0 && outDistSq[pos - 1] > dSq do
                        outDistSq[pos] <- outDistSq[pos - 1]
                        outIdx[pos] <- outIdx[pos - 1]
                        pos <- pos - 1
                    outDistSq[pos] <- dSq
                    outIdx[pos] <- cand
                    found <- found + 1
                elif dSq < outDistSq[found - 1] then
                    let mutable pos = found - 1
                    while pos > 0 && outDistSq[pos - 1] > dSq do
                        outDistSq[pos] <- outDistSq[pos - 1]
                        outIdx[pos] <- outIdx[pos - 1]
                        pos <- pos - 1
                    outDistSq[pos] <- dSq
                    outIdx[pos] <- cand

            let inline boxDistSq (nd: KdNode4D) =
                let d0 = if q0 < nd.Min0 then nd.Min0 - q0 elif q0 > nd.Max0 then q0 - nd.Max0 else 0.
                let d1 = if q1 < nd.Min1 then nd.Min1 - q1 elif q1 > nd.Max1 then q1 - nd.Max1 else 0.
                let d2 = if q2 < nd.Min2 then nd.Min2 - q2 elif q2 > nd.Max2 then q2 - nd.Max2 else 0.
                let d3 = if q3 < nd.Min3 then nd.Min3 - q3 elif q3 > nd.Max3 then q3 - nd.Max3 else 0.
                d0 * d0 + d1 * d1 + d2 * d2 + d3 * d3

            stack[0] <- 0
            let mutable sp = 1
            while sp > 0 do
                sp <- sp - 1
                let slot = stack[sp]
                let nd = tree[slot]
                let worstBound = if found < kMax then infinity else outDistSq[found - 1]
                if boxDistSq nd < worstBound then
                    if nd.Count > 0 then
                        for i = nd.Start to nd.Start + nd.Count - 1 do
                            let cand = perm[i]
                            if cand <> queryIdx then
                                let d0 = q0 - c0[cand]
                                let d1 = q1 - c1[cand]
                                let d2 = q2 - c2[cand]
                                let d3 = q3 - c3[cand]
                                insertCandidate cand (d0 * d0 + d1 * d1 + d2 * d2 + d3 * d3)
                    else
                        let lSlot = nd.Left
                        let rSlot = nd.Right
                        let dl = boxDistSq tree[lSlot]
                        let dr = boxDistSq tree[rSlot]
                        // Push farther child first so nearer child is popped first!
                        if dl <= dr then
                            if dr < worstBound then stack[sp] <- rSlot; sp <- sp + 1
                            if dl < worstBound then stack[sp] <- lSlot; sp <- sp + 1
                        else
                            if dl < worstBound then stack[sp] <- lSlot; sp <- sp + 1
                            if dr < worstBound then stack[sp] <- rSlot; sp <- sp + 1
            found

    /// <summary>Optimizes meet points <c>(m_u, m_v)</c> via golden-section search to maximize ink saving under turning-angle cap <c>cosMinTurn</c>.</summary>
    let private optimizeMeetPoints
        (u1: Point)
        (v1: Point)
        (u2: Point)
        (v2: Point)
        (cosMinTurn: float)
        : struct(float * Point * Point) =
        let len1 = Double.Hypot(v1.X - u1.X, v1.Y - u1.Y)
        let len2 = Double.Hypot(v2.X - u2.X, v2.Y - u2.Y)
        let origInk = len1 + len2

        // Ensure chords are roughly co-directional before attempting to merge.
        let chordDot = ((v1.X - u1.X) * (v2.X - u2.X) + (v1.Y - u1.Y) * (v2.Y - u2.Y)) / max 1e-9 (len1 * len2)
        if chordDot <= 0.15 then
            struct(-1., u1, v1)
        else
            let uMidX, uMidY = 0.5 * (u1.X + u2.X), 0.5 * (u1.Y + u2.Y)
            let vMidX, vMidY = 0.5 * (v1.X + v2.X), 0.5 * (v1.Y + v2.Y)
            let axisDx = vMidX - uMidX
            let axisDy = vMidY - uMidY
            let axisLen = Double.Hypot(axisDx, axisDy)
            if axisLen <= 1e-6 then
                struct(-1., u1, v1)
            else
                let evalSaving (tu: float) (tv: float) : struct(float * Point * Point) =
                    let mux = uMidX + tu * axisDx
                    let muy = uMidY + tu * axisDy
                    let mvx = vMidX - tv * axisDx
                    let mvy = vMidY - tv * axisDy
                    let mu = Point.create mux muy
                    let mv = Point.create mvx mvy

                    let tdx, tdy = mvx - mux, mvy - muy
                    let trunkLen = Double.Hypot(tdx, tdy)
                    if trunkLen <= 1e-6 then
                        struct(-1., mu, mv)
                    else
                        let su1 = Double.Hypot(mux - u1.X, muy - u1.Y)
                        let su2 = Double.Hypot(mux - u2.X, muy - u2.Y)
                        let sv1 = Double.Hypot(v1.X - mvx, v1.Y - mvy)
                        let sv2 = Double.Hypot(v2.X - mvx, v2.Y - mvy)

                        let inline angleOk ax ay alen bx by blen =
                            if alen <= 1e-6 || blen <= 1e-6 then true
                            else (ax * bx + ay * by) / (alen * blen) >= cosMinTurn

                        let ok =
                            angleOk (mux - u1.X) (muy - u1.Y) su1 tdx tdy trunkLen
                            && angleOk (mux - u2.X) (muy - u2.Y) su2 tdx tdy trunkLen
                            && angleOk tdx tdy trunkLen (v1.X - mvx) (v1.Y - mvy) sv1
                            && angleOk tdx tdy trunkLen (v2.X - mvx) (v2.Y - mvy) sv2
                            && angleOk (mux - u1.X) (muy - u1.Y) su1 (v1.X - u1.X) (v1.Y - u1.Y) len1
                            && angleOk (mux - u2.X) (muy - u2.Y) su2 (v2.X - u2.X) (v2.Y - u2.Y) len2
                            && angleOk (v1.X - mvx) (v1.Y - mvy) sv1 (v1.X - u1.X) (v1.Y - u1.Y) len1
                            && angleOk (v2.X - mvx) (v2.Y - mvy) sv2 (v2.X - u2.X) (v2.Y - u2.Y) len2

                        if not ok then
                            struct(-1., mu, mv)
                        else
                            let mergedInk = su1 + su2 + trunkLen + sv1 + sv2
                            struct(origInk - mergedInk, mu, mv)

                // Analytical Steiner seed + golden-section search over tu and tv in [0.0, 0.46].
                let invPhi = 0.6180339887498949
                let uSpread = Double.Hypot(u2.X - u1.X, u2.Y - u1.Y)
                let vSpread = Double.Hypot(v2.X - v1.X, v2.Y - v1.Y)
                let mutable bestTu = min 0.42 (uSpread / (2. * sqrt 3. * axisLen))
                let mutable bestTv = min 0.42 (vSpread / (2. * sqrt 3. * axisLen))
                let struct(sInit, muInit, mvInit) = evalSaving bestTu bestTv
                let mutable bestSave = sInit
                let mutable bestMu = muInit
                let mutable bestMv = mvInit

                // Symmetric golden-section pass first to find feasible basin even when Steiner angle exceeds cap
                let mutable loS = 0.0
                let mutable hiS = 0.45
                for _ = 1 to 10 do
                    let m1 = hiS - (hiS - loS) * invPhi
                    let m2 = loS + (hiS - loS) * invPhi
                    let struct(f1, p1u, p1v) = evalSaving m1 m1
                    let struct(f2, p2u, p2v) = evalSaving m2 m2
                    if f1 > bestSave then bestSave <- f1; bestMu <- p1u; bestMv <- p1v; bestTu <- m1; bestTv <- m1
                    if f2 > bestSave then bestSave <- f2; bestMu <- p2u; bestMv <- p2v; bestTu <- m2; bestTv <- m2
                    if f1 >= f2 then hiS <- m2 else loS <- m1

                // Coordinate golden-section refinement on tu, then tv
                let mutable loU = 0.0
                let mutable hiU = min 0.46 (0.92 - bestTv)
                for _ = 1 to 9 do
                    let m1 = hiU - (hiU - loU) * invPhi
                    let m2 = loU + (hiU - loU) * invPhi
                    let struct(f1, p1u, p1v) = evalSaving m1 bestTv
                    let struct(f2, p2u, p2v) = evalSaving m2 bestTv
                    if f1 > bestSave then bestSave <- f1; bestMu <- p1u; bestMv <- p1v; bestTu <- m1
                    if f2 > bestSave then bestSave <- f2; bestMu <- p2u; bestMv <- p2v; bestTu <- m2
                    if f1 >= f2 then hiU <- m2 else loU <- m1

                let mutable loV = 0.0
                let mutable hiV = min 0.46 (0.92 - bestTu)
                for _ = 1 to 9 do
                    let m1 = hiV - (hiV - loV) * invPhi
                    let m2 = loV + (hiV - loV) * invPhi
                    let struct(f1, p1u, p1v) = evalSaving bestTu m1
                    let struct(f2, p2u, p2v) = evalSaving bestTu m2
                    if f1 > bestSave then bestSave <- f1; bestMu <- p1u; bestMv <- p1v; bestTv <- m1
                    if f2 > bestSave then bestSave <- f2; bestMu <- p2u; bestMv <- p2v; bestTv <- m2
                    if f1 >= f2 then hiV <- m2 else loV <- m1

                struct(bestSave, bestMu, bestMv)

    /// <summary>Bundles all non-loop associations of <c>layout</c> using multilevel ink-saving 4D agglomeration with cooperative cancellation.</summary>
    let bundleWithCancellation
        (token: System.Threading.CancellationToken)
        (config: MingleConfig)
        (graph: DiagramGraph)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.MaxTurningAngleRadians && Double.IsFinite config.Curvature)
           || config.MaxTurningAngleRadians <= 0. || config.MaxTurningAngleRadians >= Math.PI
           || config.Neighbors < 1 || config.Neighbors > 256
           || config.Curvature < 0. || config.Curvature > 1. then
            Error [ InvalidObservation "MingleBundle requires MaxTurningAngleRadians in (0, pi), Neighbors in 1..256 and Curvature in [0, 1]." ]
        else
            match BundleCommon.validateGraphAndLayout graph layout with
            | Error errs -> Error errs
            | Ok(_, nodeById, assocById) ->
                let rawAssocs = layout.Associations |> List.toArray
                let totalAssocs = rawAssocs.Length
                if totalAssocs <= 1 then
                    Ok layout
                else
                    let activeMap = ResizeArray<int>(totalAssocs)
                    let flipped = ResizeArray<bool>(totalAssocs)
                    let nodeU = ResizeArray<Point>(totalAssocs * 2)
                    let nodeV = ResizeArray<Point>(totalAssocs * 2)
                    let parentTree = ResizeArray<int>(totalAssocs * 2)

                    for idx = 0 to totalAssocs - 1 do
                        let ra = rawAssocs[idx]
                        if ra.Source.NodeId <> ra.Target.NodeId then
                            let uNd = nodeById[ra.Source.NodeId]
                            let vNd = nodeById[ra.Target.NodeId]
                            let sPt = BundleCommon.resolveEndpointAnchor uNd ra.Source vNd.Center
                            let tPt = BundleCommon.resolveEndpointAnchor vNd ra.Target uNd.Center
                            if Double.Hypot(tPt.X - sPt.X, tPt.Y - sPt.Y) > 1e-3 then
                                activeMap.Add idx
                                // Canonicalize chord orientation so both u->v and v->u cluster together in 4D endpoint space!
                                let isFlip = sPt.X > tPt.X || (sPt.X = tPt.X && sPt.Y > tPt.Y)
                                flipped.Add isFlip
                                if isFlip then
                                    nodeU.Add tPt
                                    nodeV.Add sPt
                                else
                                    nodeU.Add sPt
                                    nodeV.Add tPt
                                parentTree.Add -1

                    let leafCount = activeMap.Count
                    if leafCount <= 1 then
                        Ok layout
                    else
                        let cosMinTurn = cos config.MaxTurningAngleRadians
                        let mutable activeIds = Array.init leafCount (fun i -> i)
                        let knnIdx = Array.zeroCreate<int> config.Neighbors
                        let knnDist = Array.zeroCreate<float> config.Neighbors
                        let kdStack = Array.zeroCreate<int> 128

                        let mutable level = 0
                        let mutable keepMerging = true
                        while keepMerging && activeIds.Length >= 2 && level < 16 do
                            token.ThrowIfCancellationRequested()
                            let bCount = activeIds.Length
                            let c0 = Array.init bCount (fun i -> nodeU[activeIds[i]].X)
                            let c1 = Array.init bCount (fun i -> nodeU[activeIds[i]].Y)
                            let c2 = Array.init bCount (fun i -> nodeV[activeIds[i]].X)
                            let c3 = Array.init bCount (fun i -> nodeV[activeIds[i]].Y)

                            let kdTree, kdPerm = buildKdTree4D c0 c1 c2 c3 bCount
                            let kEff = min config.Neighbors (bCount - 1)

                            let candPairs = ResizeArray<struct(float * int * int * Point * Point)>()
                            for i = 0 to bCount - 1 do
                                SimdMath.check token i
                                let found = queryKnn4D kdTree kdPerm c0 c1 c2 c3 i kEff knnIdx knnDist kdStack
                                let idI = activeIds[i]
                                let ui, vi = nodeU[idI], nodeV[idI]
                                for f = 0 to found - 1 do
                                    let j = knnIdx[f]
                                    if i < j then
                                        let idJ = activeIds[j]
                                        let struct(saving, mu, mv) = optimizeMeetPoints ui vi nodeU[idJ] nodeV[idJ] cosMinTurn
                                        if saving > 1e-4 then
                                            candPairs.Add(struct(saving, i, j, mu, mv))

                            if candPairs.Count = 0 then
                                keepMerging <- false
                            else
                                let candArr = candPairs.ToArray()
                                Array.sortInPlaceWith (fun a b ->
                                    let struct(sa, ia, ja, _, _) = a
                                    let struct(sb, ib, jb, _, _) = b
                                    let c = compare sb sa
                                    if c <> 0 then c
                                    else
                                        let c2 = compare ia ib
                                        if c2 <> 0 then c2 else compare ja jb) candArr

                                let matched = Array.zeroCreate<bool> bCount
                                let nextActive = ResizeArray<int>(bCount)
                                let mutable mergeCount = 0

                                for k = 0 to candArr.Length - 1 do
                                    let struct(_, i, j, mu, mv) = candArr[k]
                                    if not matched[i] && not matched[j] then
                                        matched[i] <- true
                                        matched[j] <- true
                                        let newTreeId = nodeU.Count
                                        nodeU.Add mu
                                        nodeV.Add mv
                                        parentTree.Add -1
                                        parentTree[activeIds[i]] <- newTreeId
                                        parentTree[activeIds[j]] <- newTreeId
                                        nextActive.Add newTreeId
                                        mergeCount <- mergeCount + 1

                                if mergeCount = 0 then
                                    keepMerging <- false
                                else
                                    for i = 0 to bCount - 1 do
                                        if not matched[i] then
                                            nextActive.Add activeIds[i]
                                    activeIds <- nextActive.ToArray()
                                    level <- level + 1

                        // Reconstruct bundled meet-point chain for every original leaf association.
                        let outAssocs = Array.copy rawAssocs
                        let upChain = ResizeArray<Point>(16)
                        let downChain = ResizeArray<Point>(16)
                        let dedupChain = ResizeArray<Point>(24)

                        for leaf = 0 to leafCount - 1 do
                            let origIdx = activeMap[leaf]
                            let ra = rawAssocs[origIdx]
                            upChain.Clear()
                            downChain.Clear()
                            let mutable cur = leaf
                            while cur >= 0 do
                                upChain.Add nodeU[cur]
                                downChain.Add nodeV[cur]
                                cur <- parentTree[cur]

                            dedupChain.Clear()
                            let inline pushDedup (pt: Point) =
                                if dedupChain.Count = 0 then dedupChain.Add pt
                                else
                                    let prev = dedupChain[dedupChain.Count - 1]
                                    if Double.Hypot(pt.X - prev.X, pt.Y - prev.Y) > 1e-4 then
                                        dedupChain.Add pt

                            for k = 0 to upChain.Count - 1 do pushDedup upChain[k]
                            for k = downChain.Count - 1 downto 0 do pushDedup downChain[k]

                            let rawChain =
                                let arr = dedupChain.ToArray()
                                if flipped[leaf] then Array.rev arr else arr

                            // Re-clip exact start/end anchors toward first/last meet point
                            if rawChain.Length >= 2 then
                                let uNd = nodeById[ra.Source.NodeId]
                                let vNd = nodeById[ra.Target.NodeId]
                                rawChain[0] <- BundleCommon.resolveEndpointAnchor uNd ra.Source rawChain[1]
                                rawChain[rawChain.Length - 1] <- BundleCommon.resolveEndpointAnchor vNd ra.Target rawChain[rawChain.Length - 2]

                            let path =
                                if rawChain.Length <= 2 || config.Curvature <= 1e-6 then
                                    let cmds = ResizeArray<PathCommand>(rawChain.Length)
                                    cmds.Add(MoveTo rawChain[0])
                                    for i = 1 to rawChain.Length - 1 do cmds.Add(LineTo rawChain[i])
                                    Path.ofCommands(List.ofSeq cmds)
                                else
                                    let optPts = [ for i in 0 .. rawChain.Length - 1 -> Some rawChain[i] ]
                                    Curves.line (CurveSpec.Bundle config.Curvature) optPts

                            let assocOpt =
                                match assocById.TryGetValue ra.Id with
                                | true, a -> Some a
                                | _ -> None
                            outAssocs[origIdx] <- BundleCommon.updateRouted assocOpt ra rawChain path

                        let routed = Array.toList outAssocs
                        Ok
                            { layout with
                                Bounds = GraphKernels.computeDiagramBounds layout.Bounds layout.Nodes layout.Groups routed
                                Associations = routed }

    /// <summary>Bundles all non-loop associations of <c>layout</c> using multilevel ink-saving 4D agglomeration without cancellation.</summary>
    let bundle config graph layout =
        bundleWithCancellation System.Threading.CancellationToken.None config graph layout

/// <summary>Configuration for Hurter, Ersoy &amp; Telea (2012) Kernel Density Estimation Edge Bundling (<c>KDEEB</c>).</summary>
type KdeBundleConfig =
    { /// <summary>Grid resolution <c>G</c> along each axis (<c>G x G</c> density cells).</summary>
      GridSize: int
      /// <summary>Number of mean-shift KDE advection iterations.</summary>
      Iterations: int
      /// <summary>Initial kernel smoothing radius in layout units.</summary>
      InitialKernelRadius: float
      /// <summary>Multiplicative kernel decay factor per iteration in <c>(0.0, 1.0]</c>.</summary>
      KernelDecay: float
      /// <summary>Initial gradient-ascent advection step in layout units.</summary>
      StepSize: float
      /// <summary>Target arc-length distance between resampled edge polyline vertices in layout units.</summary>
      SampleStep: float }

/// <summary>Default configuration values for <c>KdeBundleConfig</c>.</summary>
[<RequireQualifiedAccess>]
module KdeBundleConfig =
    /// <summary>Default KDEEB settings: <c>128 x 128</c> grid, 10 iterations, 32-unit initial kernel radius, 0.85 decay, 4-unit advection step and 12-unit sample step.</summary>
    let defaultValue =
        { GridSize = 128
          Iterations = 10
          InitialKernelRadius = 32.0
          KernelDecay = 0.85
          StepSize = 4.0
          SampleStep = 12.0 }

/// <summary>Hurter, Ersoy &amp; Telea (2012) linear-time <c>O(I * (E * S + G^2))</c> 2D Kernel Density Estimation Edge Bundling (<c>KDEEB</c>).</summary>
[<RequireQualifiedAccess>]
module KdeBundle2D =
    /// <summary>O(1)-per-cell sliding-window horizontal box blur from <c>src</c> into <c>dst</c>.</summary>
    let private boxBlurH (src: float[]) (dst: float[]) (g: int) (r: int) =
        let invW = 1. / float (2 * r + 1)
        for y = 0 to g - 1 do
            let rowOff = y * g
            let mutable acc = 0.
            let firstVal = src[rowOff]
            for k = -r to r do
                let cx = if k < 0 then 0 elif k >= g then g - 1 else k
                acc <- acc + src[rowOff + cx]
            dst[rowOff] <- acc * invW
            for x = 1 to g - 1 do
                let remIdx = x - r - 1
                let addIdx = x + r
                let remVal = if remIdx < 0 then firstVal else src[rowOff + remIdx]
                let addVal = if addIdx >= g then src[rowOff + g - 1] else src[rowOff + addIdx]
                acc <- acc + addVal - remVal
                dst[rowOff + x] <- acc * invW

    /// <summary>O(1)-per-cell sliding-window vertical box blur from <c>src</c> into <c>dst</c>.</summary>
    let private boxBlurV (src: float[]) (dst: float[]) (g: int) (r: int) =
        let invW = 1. / float (2 * r + 1)
        for x = 0 to g - 1 do
            let mutable acc = 0.
            let firstVal = src[x]
            for k = -r to r do
                let cy = if k < 0 then 0 elif k >= g then g - 1 else k
                acc <- acc + src[cy * g + x]
            dst[x] <- acc * invW
            let lastVal = src[(g - 1) * g + x]
            for y = 1 to g - 1 do
                let remIdx = y - r - 1
                let addIdx = y + r
                let remVal = if remIdx < 0 then firstVal else src[remIdx * g + x]
                let addVal = if addIdx >= g then lastVal else src[addIdx * g + x]
                acc <- acc + addVal - remVal
                dst[y * g + x] <- acc * invW

    let private resamplePolyline (sampleStep: float) (pts: Point[]) : Point[] =
        let n = pts.Length
        if n < 2 then pts
        else
            let cumLen = Array.zeroCreate<float> n
            for i = 1 to n - 1 do
                cumLen[i] <- cumLen[i - 1] + Double.Hypot(pts[i].X - pts[i - 1].X, pts[i].Y - pts[i - 1].Y)
            let totalLen = cumLen[n - 1]
            if totalLen <= 1e-6 then
                [| pts[0]; pts[n - 1] |]
            else
                let segCount = max 2 (min 96 (int (Math.Round(totalLen / sampleStep))))
                let out = Array.zeroCreate<Point> (segCount + 1)
                out[0] <- pts[0]
                out[segCount] <- pts[n - 1]
                let mutable segIdx = 0
                for k = 1 to segCount - 1 do
                    let targetD = totalLen * (float k / float segCount)
                    while segIdx < n - 2 && cumLen[segIdx + 1] < targetD do
                        segIdx <- segIdx + 1
                    let segStart = cumLen[segIdx]
                    let segSpan = cumLen[segIdx + 1] - segStart
                    let t = if segSpan <= 1e-9 then 0. else (targetD - segStart) / segSpan
                    let a, b = pts[segIdx], pts[segIdx + 1]
                    out[k] <- Point.create (BundleCommon.lerp a.X b.X t) (BundleCommon.lerp a.Y b.Y t)
                out

    let private laplacianSmoothInPlace (pts: Point[]) (scratch: ResizeArray<Point>) =
        let n = pts.Length
        if n >= 3 then
            scratch.Clear()
            for i = 0 to n - 1 do scratch.Add pts[i]
            for i = 1 to n - 2 do
                let prev = scratch[i - 1]
                let cur = scratch[i]
                let next = scratch[i + 1]
                pts[i] <-
                    Point.create
                        (0.7 * cur.X + 0.15 * (prev.X + next.X))
                        (0.7 * cur.Y + 0.15 * (prev.Y + next.Y))

    /// <summary>Bundles all non-loop associations of <c>layout</c> via grid KDE splatting, 3-pass separable box blur and normalized gradient advection with cooperative cancellation.</summary>
    let bundleWithCancellation
        (token: System.Threading.CancellationToken)
        (config: KdeBundleConfig)
        (graph: DiagramGraph)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if config.GridSize < 8 || config.GridSize > 2048
           || config.Iterations < 1 || config.Iterations > 500
           || not (Double.IsFinite config.InitialKernelRadius && Double.IsFinite config.KernelDecay
                   && Double.IsFinite config.StepSize && Double.IsFinite config.SampleStep)
           || config.InitialKernelRadius <= 0. || config.KernelDecay <= 0. || config.KernelDecay > 1.
           || config.StepSize <= 0. || config.SampleStep <= 0. then
            Error [ InvalidObservation "KdeBundle2D requires GridSize in 8..2048, Iterations in 1..500, KernelDecay in (0, 1] and positive radius/step settings." ]
        else
            match BundleCommon.validateGraphAndLayout graph layout with
            | Error errs -> Error errs
            | Ok(_, nodeById, assocById) ->
                let rawAssocs = layout.Associations |> List.toArray
                let totalAssocs = rawAssocs.Length
                if totalAssocs <= 1 then
                    Ok layout
                else
                    let activeMap = ResizeArray<int>(totalAssocs)
                    let chains = ResizeArray<Point[]>(totalAssocs)

                    let mutable minX = infinity
                    let mutable minY = infinity
                    let mutable maxX = -infinity
                    let mutable maxY = -infinity
                    let inline expandPt (p: Point) =
                        if p.X < minX then minX <- p.X
                        if p.Y < minY then minY <- p.Y
                        if p.X > maxX then maxX <- p.X
                        if p.Y > maxY then maxY <- p.Y

                    for idx = 0 to totalAssocs - 1 do
                        let ra = rawAssocs[idx]
                        if ra.Source.NodeId <> ra.Target.NodeId then
                            let uNd = nodeById[ra.Source.NodeId]
                            let vNd = nodeById[ra.Target.NodeId]
                            let sPt = BundleCommon.resolveEndpointAnchor uNd ra.Source vNd.Center
                            let tPt = BundleCommon.resolveEndpointAnchor vNd ra.Target uNd.Center
                            if Double.Hypot(tPt.X - sPt.X, tPt.Y - sPt.Y) > 1e-3 then
                                expandPt sPt
                                expandPt tPt
                                activeMap.Add idx
                                chains.Add(resamplePolyline config.SampleStep [| sPt; tPt |])

                    let eCount = activeMap.Count
                    if eCount <= 1 then
                        Ok layout
                    else
                        let pad = max 24. (config.InitialKernelRadius * 1.5)
                        let x0 = minX - pad
                        let y0 = minY - pad
                        let spanX = max 16. (maxX - minX + 2. * pad)
                        let spanY = max 16. (maxY - minY + 2. * pad)
                        let g = config.GridSize
                        let gf = float (g - 1)
                        let cellX = spanX / gf
                        let cellY = spanY / gf
                        let cellAvg = 0.5 * (cellX + cellY)

                        let density = Array.zeroCreate<float> (g * g)
                        let scratch = Array.zeroCreate<float> (g * g)
                        let polyArr = chains.ToArray()
                        let smoothScratch = ResizeArray<Point>(128)

                        let inline sampleGrid (gx: float) (gy: float) =
                            let cx = max 0. (min (float (g - 1) - 1e-6) gx)
                            let cy = max 0. (min (float (g - 1) - 1e-6) gy)
                            let ix = int cx
                            let iy = int cy
                            let fx = cx - float ix
                            let fy = cy - float iy
                            let ix1 = min (g - 1) (ix + 1)
                            let iy1 = min (g - 1) (iy + 1)
                            let r0 = iy * g
                            let r1 = iy1 * g
                            let v00 = density[r0 + ix]
                            let v10 = density[r0 + ix1]
                            let v01 = density[r1 + ix]
                            let v11 = density[r1 + ix1]
                            let top = v00 + (v10 - v00) * fx
                            let bot = v01 + (v11 - v01) * fx
                            top + (bot - top) * fy

                        let mutable curRadius = config.InitialKernelRadius
                        let mutable curStep = config.StepSize

                        for _iter = 0 to config.Iterations - 1 do
                            token.ThrowIfCancellationRequested()
                            Array.fill density 0 (g * g) 0.

                            // 1. Bilinear splatting of all edge polyline samples onto G x G density grid
                            for e = 0 to eCount - 1 do
                                let pts = polyArr[e]
                                for k = 0 to pts.Length - 1 do
                                    let p = pts[k]
                                    let gx = (p.X - x0) / cellX
                                    let gy = (p.Y - y0) / cellY
                                    if gx >= 0. && gx <= gf && gy >= 0. && gy <= gf then
                                        let ix = min (g - 2) (max 0 (int gx))
                                        let iy = min (g - 2) (max 0 (int gy))
                                        let fx = max 0. (min 1. (gx - float ix))
                                        let fy = max 0. (min 1. (gy - float iy))
                                        let r0 = iy * g + ix
                                        let r1 = (iy + 1) * g + ix
                                        density[r0] <- density[r0] + (1. - fx) * (1. - fy)
                                        density[r0 + 1] <- density[r0 + 1] + fx * (1. - fy)
                                        density[r1] <- density[r1] + (1. - fx) * fy
                                        density[r1 + 1] <- density[r1 + 1] + fx * fy

                            // 2. Fast 3-pass separable horizontal/vertical box-blur approximating isotropic Gaussian!
                            let rCells = max 1 (min (g / 4) (int (Math.Round(curRadius / max 1e-6 cellAvg))))
                            for _pass = 1 to 3 do
                                boxBlurH density scratch g rCells
                                boxBlurV scratch density g rCells

                            // 3. Advect interior sample points along normalized density gradient
                            for e = 0 to eCount - 1 do
                                SimdMath.check token e
                                let pts = polyArr[e]
                                let m = pts.Length
                                for k = 1 to m - 2 do
                                    let p = pts[k]
                                    let gx = (p.X - x0) / cellX
                                    let gy = (p.Y - y0) / cellY
                                    let dRhoDx = (sampleGrid (gx + 0.5) gy - sampleGrid (gx - 0.5) gy) / cellX
                                    let dRhoDy = (sampleGrid gx (gy + 0.5) - sampleGrid gx (gy - 0.5)) / cellY
                                    let gradMag = Double.Hypot(dRhoDx, dRhoDy)
                                    if gradMag > 1e-9 then
                                        let invNorm = curStep / (gradMag + 1e-9)
                                        let nx = max x0 (min (x0 + spanX) (p.X + dRhoDx * invNorm))
                                        let ny = max y0 (min (y0 + spanY) (p.Y + dRhoDy * invNorm))
                                        pts[k] <- Point.create nx ny

                                // 4. Uniform arc-length resampling + 1D Laplacian smoothing
                                let resampled = resamplePolyline config.SampleStep pts
                                laplacianSmoothInPlace resampled smoothScratch
                                polyArr[e] <- resampled

                            curRadius <- curRadius * config.KernelDecay
                            curStep <- curStep * config.KernelDecay

                        let outAssocs = Array.copy rawAssocs
                        for e = 0 to eCount - 1 do
                            let origIdx = activeMap[e]
                            let ra = rawAssocs[origIdx]
                            let pts = polyArr[e]
                            let path = BundleCommon.smoothClampedSplinePath pts
                            let assocOpt =
                                match assocById.TryGetValue ra.Id with
                                | true, a -> Some a
                                | _ -> None
                            outAssocs[origIdx] <- BundleCommon.updateRouted assocOpt ra pts path

                        let routed = Array.toList outAssocs
                        Ok
                            { layout with
                                Bounds = GraphKernels.computeDiagramBounds layout.Bounds layout.Nodes layout.Groups routed
                                Associations = routed }

    /// <summary>Bundles all non-loop associations of <c>layout</c> via grid KDE splatting, 3-pass separable box blur and normalized gradient advection without cancellation.</summary>
    let bundle config graph layout =
        bundleWithCancellation System.Threading.CancellationToken.None config graph layout

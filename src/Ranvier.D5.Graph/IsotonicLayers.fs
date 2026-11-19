namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Exact linear-time <c>O(n)</c> Pool Adjacent Violators Algorithm (PAVA) for weighted isotonic regression with minimum gaps and box bounds.</summary>
[<RequireQualifiedAccess>]
module Pava =
    /// <summary>Solves <c>min_x sum_i weights[i] * (x[i] - targets[i])^2</c> subject to <c>x[i+1] - x[i] &gt;= minGaps[i]</c> (scaled proportionally when total gaps exceed <c>upperBound - lowerBound</c>) and <c>lowerBound &lt;= x[0], x[n-1] &lt;= upperBound</c>.</summary>
    let solveWeighted
        (targets: float list)
        (weights: float list)
        (minGaps: float list)
        (lowerBound: float)
        (upperBound: float)
        : Result<float list, LayoutError list> =
        if not (Double.IsFinite lowerBound && Double.IsFinite upperBound) || lowerBound > upperBound then
            Error [ InvalidObservation "Pava.solveWeighted requires finite lowerBound <= upperBound." ]
        else
            let yArr = List.toArray targets
            let wArr = List.toArray weights
            let gArr = List.toArray minGaps
            let n = yArr.Length
            let expectedGaps = if n <= 1 then 0 else n - 1
            if wArr.Length <> n then
                Error [ InvalidObservation "Pava.solveWeighted requires targets and weights to have equal length." ]
            elif gArr.Length <> expectedGaps then
                Error [ InvalidObservation "Pava.solveWeighted requires minGaps length equal to max 0 (targets.Length - 1)." ]
            else
                let mutable badTarget = false
                let mutable badWeight = false
                let mutable badGap = false
                for i = 0 to n - 1 do
                    if not (Double.IsFinite (yArr[i])) then badTarget <- true
                    if not (Double.IsFinite (wArr[i])) || wArr[i] <= 0. then badWeight <- true
                for k = 0 to expectedGaps - 1 do
                    if not (Double.IsFinite (gArr[k])) || gArr[k] < 0. then badGap <- true

                if badTarget then
                    Error [ InvalidObservation "Pava.solveWeighted targets must be finite." ]
                elif badWeight then
                    Error [ InvalidObservation "Pava.solveWeighted weights must be finite and strictly positive." ]
                elif badGap then
                    Error [ InvalidObservation "Pava.solveWeighted minGaps must be finite and nonnegative." ]
                elif n = 0 then
                    Ok []
                elif n = 1 then
                    Ok [ max lowerBound (min upperBound (yArr[0])) ]
                else
                    let prefS = Array.zeroCreate<float> n
                    for k = 0 to n - 2 do
                        prefS[k + 1] <- prefS[k] + gArr[k]

                    let boxSpan = upperBound - lowerBound
                    let totalGap = prefS[n - 1]
                    let effGaps =
                        if totalGap <= boxSpan then
                            gArr
                        elif boxSpan <= 0. then
                            Array.fill prefS 0 n 0.
                            Array.zeroCreate<float> (n - 1)
                        else
                            let scale = boxSpan / totalGap
                            let scaled = Array.init (n - 1) (fun k -> gArr[k] * scale)
                            prefS[0] <- 0.
                            for k = 0 to n - 2 do
                                prefS[k + 1] <- prefS[k] + scaled[k]
                            prefS[n - 1] <- boxSpan
                            scaled

                    let loZ = lowerBound
                    let hiZ = max lowerBound (upperBound - prefS[n - 1])

                    // Linear-time O(n) stack-based Pool Adjacent Violators on shifted targets yTilde[i] = y[i] - prefS[i]
                    let blkStart = Array.zeroCreate<int> n
                    let blkEnd = Array.zeroCreate<int> n
                    let blkW = Array.zeroCreate<float> n
                    let blkWY = Array.zeroCreate<float> n
                    let blkMean = Array.zeroCreate<float> n
                    let mutable top = 0

                    for i = 0 to n - 1 do
                        let yi = yArr[i] - prefS[i]
                        let wi = wArr[i]
                        let mutable sIdx = i
                        let mutable wSum = wi
                        let mutable wySum = wi * yi
                        let mutable mean = yi
                        while top > 0 && blkMean[top - 1] >= mean do
                            top <- top - 1
                            sIdx <- blkStart[top]
                            wSum <- blkW[top] + wSum
                            wySum <- blkWY[top] + wySum
                            mean <- wySum / wSum
                        blkStart[top] <- sIdx
                        blkEnd[top] <- i
                        blkW[top] <- wSum
                        blkWY[top] <- wySum
                        blkMean[top] <- mean
                        top <- top + 1

                    let xOut = Array.zeroCreate<float> n
                    for b = 0 to top - 1 do
                        let zClamped = max loZ (min hiZ (blkMean[b]))
                        let s = blkStart[b]
                        let e = blkEnd[b]
                        xOut[s] <- zClamped + prefS[s]
                        for i = s + 1 to e do
                            xOut[i] <- xOut[i - 1] + effGaps[i - 1]

                    xOut[0] <- max lowerBound (min upperBound (xOut[0]))
                    xOut[n - 1] <- max lowerBound (min upperBound (xOut[n - 1]))
                    Ok(Array.toList xOut)

/// <summary>Horizontal layer-band assignment policy for <c>IsotonicLayers</c>.</summary>
[<RequireQualifiedAccess>]
type LayerStratification =
    /// <summary>Stratifies nodes automatically by logarithmic transitive incoming dependency reach <c>floor(log2(1 + reach))</c>.</summary>
    | LogTransitiveReach
    /// <summary>Stratifies nodes into caller-named reach tiers <c>(label, minReachThreshold)</c> (e.g., <c>["applications", 0; "libraries", 4; "foundations", 9]</c>).</summary>
    | CustomReachTiers of tiers: (string * int) list
    /// <summary>Stratifies nodes by longest directed path distance over the Tarjan SCC condensation DAG.</summary>
    | TopologicalLongestPath

/// <summary>A positioned horizontal architectural band produced by <c>IsotonicLayers</c>.</summary>
type IsotonicLayerBand =
    { /// <summary>Zero-based vertical band index from top (<c>0</c>) to bottom.</summary>
      Index: int
      /// <summary>Human-readable band caption.</summary>
      Label: string
      /// <summary>Axis-aligned band rectangle spanning the layout width.</summary>
      Bounds: Bounds
      /// <summary>Left-to-right ordered node identifiers placed inside this band.</summary>
      NodeIds: string list }

/// <summary>Configuration for PAVA-constrained isotonic horizontal layer layout and Poincaré disk angular unrolling.</summary>
type IsotonicLayersConfig =
    { /// <summary>Target viewport bounds.</summary>
      Viewport: Bounds
      /// <summary>Reach-tier or topological layer stratification policy.</summary>
      Stratification: LayerStratification
      /// <summary>When true, places high-reach depended-upon foundations in the bottom band and entry applications at the top.</summary>
      FoundationsAtBottom: bool
      /// <summary>Minimum horizontal clearance gap between adjacent nodes in the same band.</summary>
      NodeGap: float
      /// <summary>Nonnegative interior padding inside each horizontal band.</summary>
      BandPadding: float
      /// <summary>Number of alternating top-down / bottom-up neighbor barycenter sweeps when <c>ReferenceLayout</c> is <c>None</c>.</summary>
      BarycenterSweeps: int
      /// <summary>Optional reference layout (such as a <c>PoincareDisk</c> layout) whose angular node positions are unrolled onto horizontal targets.</summary>
      ReferenceLayout: DiagramLayout2D option }

/// <summary>Constructors for <c>IsotonicLayersConfig</c>.</summary>
[<RequireQualifiedAccess>]
module IsotonicLayersConfig =
    /// <summary>Creates default isotonic layer settings: <c>LogTransitiveReach</c>, foundations at bottom, 10-unit node gap, 12-unit band padding and 6 sweeps.</summary>
    let create (viewport: Bounds) : IsotonicLayersConfig =
        { Viewport = viewport
          Stratification = LayerStratification.LogTransitiveReach
          FoundationsAtBottom = true
          NodeGap = 10.
          BandPadding = 12.
          BarycenterSweeps = 6
          ReferenceLayout = None }

/// <summary>Complete isotonic layer layout output containing the 2D diagram layout, horizontal bands and exact transitive reach table.</summary>
type IsotonicLayersOutput =
    { /// <summary>Positioned 2D nodes, compound groups and cubic-routed associations.</summary>
      Layout: DiagramLayout2D
      /// <summary>Positioned horizontal bands ordered top-to-bottom (<c>Index = 0 .. B - 1</c>).</summary>
      Bands: IsotonicLayerBand list
      /// <summary>Exact incoming transitive dependency reach count for every visible node ID.</summary>
      ReachByNode: Map<string, int> }

/// <summary>PAVA-constrained isotonic architectural layer engine with water-filling band allocation, disk unrolling and smooth 60fps layout morphing.</summary>
[<RequireQualifiedAccess>]
module IsotonicLayers =
    let private validateConfig (config: IsotonicLayersConfig) : Result<unit, LayoutError list> =
        let vx = Bounds.x config.Viewport
        let vy = Bounds.y config.Viewport
        let vw = Bounds.width config.Viewport
        let vh = Bounds.height config.Viewport
        let stratOk =
            match config.Stratification with
            | LayerStratification.LogTransitiveReach
            | LayerStratification.TopologicalLongestPath -> true
            | LayerStratification.CustomReachTiers tiers ->
                (not tiers.IsEmpty)
                && (tiers |> List.forall (fun (lbl, minR) -> not (String.IsNullOrWhiteSpace lbl) && minR >= 0))
        if not (Double.IsFinite vx && Double.IsFinite vy && Double.IsFinite vw && Double.IsFinite vh)
           || vw <= 0. || vh <= 0. then
            Error [ InvalidObservation "IsotonicLayersConfig Viewport must have finite coordinates and positive width and height." ]
        elif not stratOk then
            Error [ InvalidObservation "IsotonicLayersConfig CustomReachTiers must be non-empty with non-blank labels and nonnegative thresholds." ]
        elif not (Double.IsFinite config.NodeGap) || config.NodeGap < 0. then
            Error [ InvalidObservation "IsotonicLayersConfig NodeGap must be finite and nonnegative." ]
        elif not (Double.IsFinite config.BandPadding) || config.BandPadding < 0. then
            Error [ InvalidObservation "IsotonicLayersConfig BandPadding must be finite and nonnegative." ]
        elif config.BarycenterSweeps < 0 || config.BarycenterSweeps > 500 then
            Error [ InvalidObservation "IsotonicLayersConfig BarycenterSweeps must be in [0, 500]." ]
        else
            Ok()

    // Exact O(B log B) piecewise-linear water-filling solver: h[b] = max(minH[b], tau * weight[b]) summing to max(availH, sum minH).
    let private waterFill (availH: float) (minH: float[]) (weights: float[]) : float[] =
        let bCount = minH.Length
        if bCount = 0 then
            [||]
        elif bCount = 1 then
            [| max availH (minH[0]) |]
        else
            let mutable sumMin = 0.
            for b = 0 to bCount - 1 do sumMin <- sumMin + minH[b]
            if availH <= sumMin + 1e-9 then
                Array.copy minH
            else
                let safeW = Array.init bCount (fun b -> max 1e-6 (weights[b]))
                let ratios = Array.init bCount (fun b -> minH[b] / safeW[b])
                let ord = Array.init bCount id
                Array.sortInPlaceWith (fun a b ->
                    let c = compare (ratios[a]) (ratios[b])
                    if c <> 0 then c else compare a b) ord

                let suffixMin = Array.zeroCreate<float> (bCount + 1)
                for k = bCount - 1 downto 0 do
                    suffixMin[k] <- suffixMin[k + 1] + minH[ord[k]]

                let mutable prefW = 0.
                let mutable tau = ratios[ord[bCount - 1]]
                let mutable solved = false
                let mutable k = 0
                while k < bCount && not solved do
                    prefW <- prefW + safeW[ord[k]]
                    let candTau = (availH - suffixMin[k + 1]) / prefW
                    let nextBreak = if k + 1 < bCount then ratios[ord[k + 1]] else infinity
                    if candTau <= nextBreak + 1e-12 then
                        tau <- candTau
                        solved <- true
                    k <- k + 1

                Array.init bCount (fun b -> max (minH[b]) (tau * safeW[b]))

    let private solveBandPava
        (nodesInOrder: int[])
        (targetX: float[])
        (nodeWidths: float[])
        (nodeWeights: float[])
        (nodeGap: float)
        (xLeft: float)
        (xRight: float)
        (outX: float[])
        : float * float =
        let k = nodesInOrder.Length
        if k = 0 then
            xLeft, xRight
        elif k = 1 then
            let u = nodesInOrder[0]
            let hw = 0.5 * nodeWidths[u]
            let mid = 0.5 * (xLeft + xRight)
            let effL = min xLeft (mid - hw)
            let effR = max xRight (mid + hw)
            outX[u] <- max (effL + hw) (min (effR - hw) (targetX[u]))
            effL, effR
        else
            let w0 = nodeWidths[nodesInOrder[0]]
            let wLast = nodeWidths[nodesInOrder[k - 1]]
            let gaps =
                [ for m in 0 .. k - 2 ->
                    0.5 * (nodeWidths[nodesInOrder[m]] + nodeWidths[nodesInOrder[m + 1]]) + nodeGap ]
            let mutable gapSum = 0.
            for g in gaps do gapSum <- gapSum + g
            let reqTotalW = gapSum + 0.5 * (w0 + wLast)
            let curSpan = xRight - xLeft
            let effL, effR =
                if reqTotalW <= curSpan then
                    xLeft, xRight
                else
                    let mid = 0.5 * (xLeft + xRight)
                    mid - 0.5 * reqTotalW, mid + 0.5 * reqTotalW

            let lo = effL + 0.5 * w0
            let hi = max lo (effR - 0.5 * wLast)
            let tList = [ for m in 0 .. k - 1 -> targetX[nodesInOrder[m]] ]
            let wList = [ for m in 0 .. k - 1 -> max 0.1 (nodeWeights[nodesInOrder[m]]) ]

            match Pava.solveWeighted tList wList gaps lo hi with
            | Ok solved ->
                let arr = List.toArray solved
                for m = 0 to k - 1 do
                    outX[nodesInOrder[m]] <- arr[m]
            | Error _ ->
                let mutable cur = lo
                outX[nodesInOrder[0]] <- cur
                for m = 0 to k - 2 do
                    cur <- cur + 0.5 * (nodeWidths[nodesInOrder[m]] + nodeWidths[nodesInOrder[m + 1]]) + nodeGap
                    outX[nodesInOrder[m + 1]] <- cur
            effL, effR

    /// <summary>Computes the exact incoming transitive dependency reach <c>|{ u != v : u -&gt;+ v }|</c> for every visible node in <c>graph</c>.</summary>
    let computeReach (graph: DiagramGraph) : Map<string, int> =
        let eff = DiagramGraph.effective graph
        let n = eff.Nodes.Length
        if n = 0 then
            Map.empty
        else
            let csr = GraphKernels.buildCsr eff
            let cond = TransitiveReachKernel.analyze csr
            let mutable m = Map.empty
            for i = n - 1 downto 0 do
                m <- Map.add (eff.Nodes[i].Id) (cond.IncomingReach[i]) m
            m

    /// <summary>Stratifies <c>graph</c> into water-filled horizontal bands and solves exact PAVA horizontal coordinates with cooperative cancellation.</summary>
    let computeWithCancellation
        (token: System.Threading.CancellationToken)
        (config: IsotonicLayersConfig)
        (graph: DiagramGraph)
        : Result<IsotonicLayersOutput, LayoutError list> =
        token.ThrowIfCancellationRequested()
        match validateConfig config with
        | Error errs -> Error errs
        | Ok () ->
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            let vx = Bounds.x config.Viewport
            let vy = Bounds.y config.Viewport
            let vw = Bounds.width config.Viewport
            let vh = Bounds.height config.Viewport

            if n = 0 then
                Ok
                    { Layout =
                        { Bounds = config.Viewport
                          Nodes = []
                          Groups = GraphKernels.computeGroupBounds eff [||]
                          Associations = [] }
                      Bands = []
                      ReachByNode = Map.empty }
            else
                let csr = GraphKernels.buildCsr eff
                let cond = TransitiveReachKernel.analyze csr
                let reachArr = cond.IncomingReach
                let mutable reachMap = Map.empty
                for i = n - 1 downto 0 do
                    reachMap <- Map.add (eff.Nodes[i].Id) (reachArr[i]) reachMap

                // Build raw band buckets + captions ordered top-to-bottom according to Stratification & FoundationsAtBottom
                let bandSpecs : (string * int[])[] =
                    match config.Stratification with
                    | LayerStratification.LogTransitiveReach ->
                        let invLog2 = 1. / log 2.
                        let tiers = Array.init n (fun i -> int (floor (log (1. + float reachArr[i]) * invLog2 + 1e-12)))
                        let distinct = tiers |> Array.distinct
                        Array.sortInPlaceWith (fun a b ->
                            if config.FoundationsAtBottom then compare a b else compare b a) distinct
                        Array.init distinct.Length (fun bi ->
                            let t = distinct[bi]
                            let mems = [| for i in 0 .. n - 1 do if tiers[i] = t then yield i |]
                            let minThresh = if t = 0 then 0 else (1 <<< t) - 1
                            let lbl = if t = 0 then "applications (reach 0)" else sprintf "tier %d (reach >= %d)" t minThresh
                            lbl, mems)

                    | LayerStratification.CustomReachTiers rawTiers ->
                        let sortedTiers =
                            rawTiers
                            |> List.mapi (fun origIdx (lbl, minR) -> lbl, minR, origIdx)
                            |> List.sortWith (fun (_, rA, iA) (_, rB, iB) ->
                                let c = compare rA rB
                                if c <> 0 then c else compare iA iB)
                            |> List.toArray
                        let tCount = sortedTiers.Length
                        let assigned =
                            Array.init n (fun i ->
                                let r = reachArr[i]
                                let mutable chosen = 0
                                for ti = 0 to tCount - 1 do
                                    let _, minR, _ = sortedTiers[ti]
                                    if r >= minR then chosen <- ti
                                chosen)
                        let indices =
                            if config.FoundationsAtBottom then [| for ti in 0 .. tCount - 1 -> ti |]
                            else [| for ti in tCount - 1 .. -1 .. 0 -> ti |]
                        indices
                        |> Array.choose (fun ti ->
                            let lbl, _, _ = sortedTiers[ti]
                            let mems = [| for i in 0 .. n - 1 do if assigned[i] = ti then yield i |]
                            if mems.Length > 0 then Some(lbl, mems) else None)

                    | LayerStratification.TopologicalLongestPath ->
                        let dists = cond.DistToSink
                        let distinct = dists |> Array.distinct
                        // Foundations have DistToSink = 0; callers have highest DistToSink.
                        Array.sortInPlaceWith (fun a b ->
                            if config.FoundationsAtBottom then compare b a else compare a b) distinct
                        Array.init distinct.Length (fun bi ->
                            let d = distinct[bi]
                            let mems = [| for i in 0 .. n - 1 do if dists[i] = d then yield i |]
                            let lbl = if d = 0 then "foundations" else sprintf "layer %d" d
                            lbl, mems)

                let bCount = bandSpecs.Length
                let bandOf = Array.zeroCreate<int> n
                for bi = 0 to bCount - 1 do
                    let mems = snd (bandSpecs[bi])
                    for k = 0 to mems.Length - 1 do
                        bandOf[mems[k]] <- bi

                // Vertical water-filling band height allocation
                let minBandH =
                    Array.init bCount (fun bi ->
                        let mems = snd (bandSpecs[bi])
                        let mutable maxH = 16.
                        for k = 0 to mems.Length - 1 do
                            let h = eff.Nodes[mems[k]].Size.Height
                            if h > maxH then maxH <- h
                        maxH + 2. * config.BandPadding)

                let demandW =
                    Array.init bCount (fun bi ->
                        let mems = snd (bandSpecs[bi])
                        let mutable maxH = 16.
                        for k = 0 to mems.Length - 1 do
                            let h = eff.Nodes[mems[k]].Size.Height
                            if h > maxH then maxH <- h
                        sqrt (float (max 1 mems.Length)) * maxH)

                let bandHeights = waterFill vh minBandH demandW
                let bandTopY = Array.zeroCreate<float> bCount
                let mutable curY = vy
                for bi = 0 to bCount - 1 do
                    bandTopY[bi] <- curY
                    curY <- curY + bandHeights[bi]

                let xLeftBase = vx + config.BandPadding
                let xRightBase = max (xLeftBase + 8.) (vx + vw - config.BandPadding)
                let nodeWidths = Array.init n (fun i -> eff.Nodes[i].Size.Width)
                let nodeWeights =
                    Array.init n (fun i ->
                        let deg = csr.UndirOffsets[i + 1] - csr.UndirOffsets[i]
                        max 1. (eff.Nodes[i].Mass + 0.25 * float deg))

                let xPos = Array.zeroCreate<float> n
                let targetX = Array.zeroCreate<float> n
                let bandOrder = Array.init bCount (fun bi -> Array.copy (snd (bandSpecs[bi])))

                let mutable globalBandLeft = vx
                let mutable globalBandRight = vx + vw

                match config.ReferenceLayout with
                | Some refLayout ->
                    // Unroll node angles atan2(y - cy, x - cx) around ReferenceLayout.Bounds center onto [xLeftBase, xRightBase]
                    let refCx = Bounds.x refLayout.Bounds + Bounds.width refLayout.Bounds * 0.5
                    let refCy = Bounds.y refLayout.Bounds + Bounds.height refLayout.Bounds * 0.5
                    let refMap = Dictionary<string, Point>(StringComparer.Ordinal)
                    for pn in refLayout.Nodes do refMap[pn.Id] <- pn.Center
                    let spanX = xRightBase - xLeftBase

                    for i = 0 to n - 1 do
                        match refMap.TryGetValue (eff.Nodes[i].Id) with
                        | true, pt ->
                            let dx = pt.X - refCx
                            let dy = pt.Y - refCy
                            if Double.Hypot(dx, dy) <= 1e-6 then
                                targetX[i] <- 0.5 * (xLeftBase + xRightBase)
                            else
                                let ang = atan2 dy dx
                                let uFrac = (ang + Math.PI) / (2. * Math.PI)
                                targetX[i] <- xLeftBase + uFrac * spanX
                        | _ ->
                            targetX[i] <- 0.5 * (xLeftBase + xRightBase)

                    for bi = 0 to bCount - 1 do
                        let ord = bandOrder[bi]
                        Array.sortInPlaceWith (fun a b ->
                            let c = compare (targetX[a]) (targetX[b])
                            if c <> 0 then c else compare a b) ord
                        let effL, effR = solveBandPava ord targetX nodeWidths nodeWeights config.NodeGap xLeftBase xRightBase xPos
                        if effL - config.BandPadding < globalBandLeft then globalBandLeft <- effL - config.BandPadding
                        if effR + config.BandPadding > globalBandRight then globalBandRight <- effR + config.BandPadding

                | None ->
                    // Initialize grouped left-to-right order per band and refine with alternating barycenter + PAVA sweeps
                    let groupKey (i: int) =
                        match eff.Nodes[i].ParentGroup with
                        | Some gid -> "0:" + gid + ":" + eff.Nodes[i].Id
                        | None -> "1:" + eff.Nodes[i].Id

                    for bi = 0 to bCount - 1 do
                        let ord = bandOrder[bi]
                        Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(groupKey a, groupKey b)) ord
                        let k = ord.Length
                        for m = 0 to k - 1 do
                            let frac = (float m + 0.5) / float (max 1 k)
                            targetX[ord[m]] <- xLeftBase + frac * (xRightBase - xLeftBase)
                        solveBandPava ord targetX nodeWidths nodeWeights config.NodeGap xLeftBase xRightBase xPos |> ignore

                    for sweep = 0 to config.BarycenterSweeps - 1 do
                        token.ThrowIfCancellationRequested()
                        let down = (sweep % 2 = 0)
                        let startB = if down then 1 else bCount - 2
                        let stopB = if down then bCount - 1 else 0
                        let stepB = if down then 1 else -1
                        let mutable bi = startB
                        while (if stepB > 0 then bi <= stopB else bi >= stopB) do
                            let ord = bandOrder[bi]
                            if ord.Length > 0 then
                                for m = 0 to ord.Length - 1 do
                                    let u = ord[m]
                                    let mutable wSum = 0.15
                                    let mutable wxSum = 0.15 * xPos[u]
                                    for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                                        let v = csr.UndirTargets[idx]
                                        let bv = bandOf[v]
                                        if bv <> bi then
                                            let dirBias = if (down && bv < bi) || (not down && bv > bi) then 1.5 else 0.75
                                            let w = csr.UndirWeights[idx] * dirBias
                                            wxSum <- wxSum + w * xPos[v]
                                            wSum <- wSum + w
                                    targetX[u] <- wxSum / wSum
                                Array.sortInPlaceWith (fun a b ->
                                    let c = compare (targetX[a]) (targetX[b])
                                    if c <> 0 then c else compare a b) ord
                                solveBandPava ord targetX nodeWidths nodeWeights config.NodeGap xLeftBase xRightBase xPos |> ignore
                            bi <- bi + stepB

                    // Final exact PAVA projection on every band
                    for bi = 0 to bCount - 1 do
                        let ord = bandOrder[bi]
                        for m = 0 to ord.Length - 1 do
                            let u = ord[m]
                            let mutable wSum = 0.25
                            let mutable wxSum = 0.25 * xPos[u]
                            for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                                let v = csr.UndirTargets[idx]
                                if bandOf[v] <> bi then
                                    let w = csr.UndirWeights[idx]
                                    wxSum <- wxSum + w * xPos[v]
                                    wSum <- wSum + w
                            targetX[u] <- wxSum / wSum
                        Array.sortInPlaceWith (fun a b ->
                            let c = compare (targetX[a]) (targetX[b])
                            if c <> 0 then c else compare a b) ord
                        let effL, effR = solveBandPava ord targetX nodeWidths nodeWeights config.NodeGap xLeftBase xRightBase xPos
                        if effL - config.BandPadding < globalBandLeft then globalBandLeft <- effL - config.BandPadding
                        if effR + config.BandPadding > globalBandRight then globalBandRight <- effR + config.BandPadding

                let finalBandW = max vw (globalBandRight - globalBandLeft)
                let bands =
                    [ for bi in 0 .. bCount - 1 ->
                        let lbl = fst (bandSpecs[bi])
                        let ord = bandOrder[bi]
                        { Index = bi
                          Label = lbl
                          Bounds = Bounds.create globalBandLeft (bandTopY[bi]) finalBandW (bandHeights[bi])
                          NodeIds = [ for m in 0 .. ord.Length - 1 -> eff.Nodes[ord[m]].Id ] } ]

                let posNodes =
                    Array.init n (fun i ->
                        let nd = eff.Nodes[i]
                        let bi = bandOf[i]
                        let cx = xPos[i]
                        let cy = bandTopY[bi] + 0.5 * bandHeights[bi]
                        let c = match nd.Pinned with Some p -> p | None -> Point.create cx cy
                        let w, h = nd.Size.Width, nd.Size.Height
                        let b = Bounds.create (c.X - 0.5 * w) (c.Y - 0.5 * h) w h
                        { Id = nd.Id
                          Kind = nd.Kind
                          Bounds = b
                          Center = c
                          ParentGroup = nd.ParentGroup
                          Layer = Some bi
                          Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                let groups = GraphKernels.computeGroupBounds eff (posNodes |> Array.map (fun pn -> pn.Bounds))

                // Route associations as smooth cubic Bézier curves between top/bottom (or lateral) node boundaries
                let lookupPort (node: PositionedNode) (portId: string option) =
                    match portId with
                    | Some pid -> node.Ports |> List.tryFind (fun p -> p.Id = pid) |> Option.map (fun p -> p.Anchor)
                    | None -> None

                let cubicSample (p0: Point) (c1: Point) (c2: Point) (p3: Point) : Point[] =
                    Array.init 9 (fun k ->
                        if k = 0 then p0
                        elif k = 8 then p3
                        else
                            let t = float k / 8.
                            let mt = 1. - t
                            let w0 = mt * mt * mt
                            let w1 = 3. * mt * mt * t
                            let w2 = 3. * mt * t * t
                            let w3 = t * t * t
                            Point.create
                                (w0 * p0.X + w1 * c1.X + w2 * c2.X + w3 * p3.X)
                                (w0 * p0.Y + w1 * c1.Y + w2 * c2.Y + w3 * p3.Y))

                let m = eff.Associations.Length
                let outAssocs = Array.zeroCreate<RoutedAssociation> m
                for e = 0 to m - 1 do
                    let a = eff.Associations[e]
                    let ui = eff.ReversedIdMap[a.Source.NodeId]
                    let vi = eff.ReversedIdMap[a.Target.NodeId]
                    let uNode = posNodes[ui]
                    let vNode = posNodes[vi]
                    let hasLabels = a.SourceLabel.IsSome || a.CenterLabel.IsSome || a.TargetLabel.IsSome

                    if ui = vi then
                        let b = uNode.Bounds
                        let rx = Bounds.x b + Bounds.width b
                        let ty = Bounds.y b
                        let p0 = lookupPort uNode a.Source.PortId |> Option.defaultValue (Point.create (rx - min 12. (Bounds.width b * 0.25)) ty)
                        let p3 = lookupPort uNode a.Target.PortId |> Option.defaultValue (Point.create rx (ty + min 12. (Bounds.height b * 0.35)))
                        let c1 = Point.create (rx + 20.) (ty - 22.)
                        let c2 = Point.create (rx + 24.) (ty + 16.)
                        let wps = cubicSample p0 c1 c2 p3
                        let path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                        let sLbl, mLbl, tLbl =
                            if hasLabels then GraphKernels.placeLabelsOnWaypoints a wps
                            else None, None, None
                        outAssocs[e] <-
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              Waypoints = Array.toList wps
                              Path = path
                              ReversedInCycle = false
                              SourceLabel = sLbl
                              CenterLabel = mLbl
                              TargetLabel = tLbl }
                    elif bandOf[ui] = bandOf[vi] then
                        // Intra-band horizontal arch above the nodes so sibling boxes are not crossed
                        let ub = uNode.Bounds
                        let vb = vNode.Bounds
                        let p0 = lookupPort uNode a.Source.PortId |> Option.defaultValue (Point.create uNode.Center.X (Bounds.y ub))
                        let p3 = lookupPort vNode a.Target.PortId |> Option.defaultValue (Point.create vNode.Center.X (Bounds.y vb))
                        let span = abs (p3.X - p0.X)
                        let archLift = max 14. (min 42. (0.22 * span))
                        let topArchY = min p0.Y p3.Y - archLift
                        let c1 = Point.create (p0.X + 0.25 * (p3.X - p0.X)) topArchY
                        let c2 = Point.create (p0.X + 0.75 * (p3.X - p0.X)) topArchY
                        let wps = cubicSample p0 c1 c2 p3
                        let path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                        let sLbl, mLbl, tLbl =
                            if hasLabels then GraphKernels.placeLabelsOnWaypoints a wps
                            else None, None, None
                        outAssocs[e] <-
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              Waypoints = Array.toList wps
                              Path = path
                              ReversedInCycle = false
                              SourceLabel = sLbl
                              CenterLabel = mLbl
                              TargetLabel = tLbl }
                    else
                        let goingDown = vNode.Center.Y >= uNode.Center.Y
                        let ub = uNode.Bounds
                        let vb = vNode.Bounds
                        let defaultStart =
                            Point.create uNode.Center.X (if goingDown then Bounds.y ub + Bounds.height ub else Bounds.y ub)
                        let defaultEnd =
                            Point.create vNode.Center.X (if goingDown then Bounds.y vb else Bounds.y vb + Bounds.height vb)
                        let p0 = lookupPort uNode a.Source.PortId |> Option.defaultValue defaultStart
                        let p3 = lookupPort vNode a.Target.PortId |> Option.defaultValue defaultEnd
                        let dy = p3.Y - p0.Y
                        let c1 = Point.create p0.X (p0.Y + 0.45 * dy)
                        let c2 = Point.create p3.X (p3.Y - 0.45 * dy)
                        let wps = cubicSample p0 c1 c2 p3
                        let path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                        let sLbl, mLbl, tLbl =
                            if hasLabels then GraphKernels.placeLabelsOnWaypoints a wps
                            else None, None, None
                        let rev =
                            if config.FoundationsAtBottom then bandOf[ui] > bandOf[vi]
                            else bandOf[ui] < bandOf[vi]
                        outAssocs[e] <-
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              Waypoints = Array.toList wps
                              Path = path
                              ReversedInCycle = rev
                              SourceLabel = sLbl
                              CenterLabel = mLbl
                              TargetLabel = tLbl }

                let nList = Array.toList posNodes
                let routed = Array.toList outAssocs
                let baseBox = Bounds.create globalBandLeft vy finalBandW (max vh (curY - vy))
                let finalBounds = GraphKernels.computeDiagramBounds baseBox nList groups routed
                Ok
                    { Layout =
                        { Bounds = finalBounds
                          Nodes = nList
                          Groups = groups
                          Associations = routed }
                      Bands = bands
                      ReachByNode = reachMap }

    /// <summary>Stratifies <c>graph</c> into water-filled horizontal bands and solves exact PAVA horizontal coordinates without cancellation.</summary>
    let compute (config: IsotonicLayersConfig) (graph: DiagramGraph) : Result<IsotonicLayersOutput, LayoutError list> =
        computeWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Computes the 2D diagram layout of PAVA isotonic layers with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: IsotonicLayersConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        computeWithCancellation token config graph |> Result.map (fun o -> o.Layout)

    /// <summary>Computes the 2D diagram layout of PAVA isotonic layers without cancellation.</summary>
    let layout (config: IsotonicLayersConfig) (graph: DiagramGraph) : Result<DiagramLayout2D, LayoutError list> =
        layoutWithCancellation System.Threading.CancellationToken.None config graph

    let inline private lerpScalar (a: float) (b: float) (alpha: float) : float =
        if alpha <= 0. then a
        elif alpha >= 1. then b
        else a + (b - a) * alpha

    let inline private lerpPoint (a: Point) (b: Point) (alpha: float) : Point =
        Point.create (lerpScalar a.X b.X alpha) (lerpScalar a.Y b.Y alpha)

    let private lerpRect (a: Bounds) (b: Bounds) (alpha: float) : Bounds =
        if alpha <= 0. then a
        elif alpha >= 1. then b
        else
            let x = lerpScalar (Bounds.x a) (Bounds.x b) alpha
            let y = lerpScalar (Bounds.y a) (Bounds.y b) alpha
            let w = max 0. (lerpScalar (Bounds.width a) (Bounds.width b) alpha)
            let h = max 0. (lerpScalar (Bounds.height a) (Bounds.height b) alpha)
            Bounds.create x y w h

    let private resampleWaypoints (pts: Point list) (fallback: Point) (samples: int) : Point[] =
        let raw = List.toArray pts
        if raw.Length = 0 then
            Array.create samples fallback
        elif raw.Length = 1 then
            Array.create samples (raw[0])
        else
            let segs = raw.Length - 1
            let cumLen = Array.zeroCreate<float> raw.Length
            for i = 0 to segs - 1 do
                cumLen[i + 1] <- cumLen[i] + Double.Hypot(raw[i + 1].X - raw[i].X, raw[i + 1].Y - raw[i].Y)
            let total = cumLen[segs]
            if total <= 1e-9 then
                Array.init samples (fun k -> if k = 0 then raw[0] elif k = samples - 1 then raw[segs] else raw[0])
            else
                Array.init samples (fun k ->
                    if k = 0 then raw[0]
                    elif k = samples - 1 then raw[segs]
                    else
                        let dTarget = total * (float k / float (samples - 1))
                        let mutable s = 0
                        while s < segs - 1 && cumLen[s + 1] < dTarget do
                            s <- s + 1
                        let segLen = cumLen[s + 1] - cumLen[s]
                        let localT = if segLen <= 1e-12 then 0. else (dTarget - cumLen[s]) / segLen
                        lerpPoint (raw[s]) (raw[s + 1]) localT)

    let private lerpOptionalLabel (a: PositionedLabel option) (b: PositionedLabel option) (alpha: float) : PositionedLabel option =
        match a, b with
        | Some la, Some lb ->
            let rect = lerpRect la.Bounds lb.Bounds alpha
            let ctr = lerpPoint la.Center lb.Center alpha
            Some { PositionedLabel.Bounds = rect; Center = ctr }
        | Some la, None -> if alpha < 0.5 then Some la else None
        | None, Some lb -> if alpha >= 0.5 then Some lb else None
        | None, None -> None

    /// <summary>Linearly interpolates node, group and association geometry between <c>source</c> (<c>t = 0</c>) and <c>target</c> (<c>t = 1</c>), clamping finite <c>t</c> to <c>[0, 1]</c>.</summary>
    let interpolate
        (t: float)
        (source: DiagramLayout2D)
        (target: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        if not (Double.IsFinite t) then
            Error [ InvalidObservation "IsotonicLayers.interpolate parameter t must be finite." ]
        else
            let alpha = max 0. (min 1. t)
            let srcNodes = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
            let tgtNodes = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
            let nodeIds = HashSet<string>(StringComparer.Ordinal)
            for n in source.Nodes do
                srcNodes[n.Id] <- n
                nodeIds.Add n.Id |> ignore
            for n in target.Nodes do
                tgtNodes[n.Id] <- n
                nodeIds.Add n.Id |> ignore

            let sortedNodeIds = Seq.toArray nodeIds
            Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(a, b)) sortedNodeIds

            let interpNodes =
                [ for i in 0 .. sortedNodeIds.Length - 1 do
                    let id = sortedNodeIds[i]
                    match srcNodes.TryGetValue id, tgtNodes.TryGetValue id with
                    | (true, sn), (true, tn) ->
                        if alpha <= 0. then yield sn
                        elif alpha >= 1. then yield tn
                        else
                            let ctr = lerpPoint sn.Center tn.Center alpha
                            let w = max 0. (lerpScalar (Bounds.width sn.Bounds) (Bounds.width tn.Bounds) alpha)
                            let h = max 0. (lerpScalar (Bounds.height sn.Bounds) (Bounds.height tn.Bounds) alpha)
                            let rect = Bounds.create (ctr.X - 0.5 * w) (ctr.Y - 0.5 * h) w h
                            let sPorts = Dictionary<string, PositionedPort>(StringComparer.Ordinal)
                            let tPorts = Dictionary<string, PositionedPort>(StringComparer.Ordinal)
                            let pIds = HashSet<string>(StringComparer.Ordinal)
                            for p in sn.Ports do sPorts[p.Id] <- p; pIds.Add p.Id |> ignore
                            for p in tn.Ports do tPorts[p.Id] <- p; pIds.Add p.Id |> ignore
                            let sortedPIds = Seq.toArray pIds
                            Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(a, b)) sortedPIds
                            let ports =
                                [ for pi in 0 .. sortedPIds.Length - 1 do
                                    let pid = sortedPIds[pi]
                                    match sPorts.TryGetValue pid, tPorts.TryGetValue pid with
                                    | (true, sp), (true, tp) ->
                                        yield
                                            { Id = pid
                                              Side = if alpha < 0.5 then sp.Side else tp.Side
                                              Anchor = lerpPoint sp.Anchor tp.Anchor alpha }
                                    | (true, sp), _ -> yield sp
                                    | _, (true, tp) -> yield tp
                                    | _ -> () ]
                            yield
                                { Id = id
                                  Kind = if alpha < 0.5 then sn.Kind else tn.Kind
                                  Bounds = rect
                                  Center = ctr
                                  ParentGroup = if alpha < 0.5 then sn.ParentGroup else tn.ParentGroup
                                  Layer = if alpha < 0.5 then sn.Layer else tn.Layer
                                  Ports = ports }
                    | (true, sn), _ -> yield sn
                    | _, (true, tn) -> yield tn
                    | _ -> () ]

            let nodeCenterMap = Dictionary<string, Point>(StringComparer.Ordinal)
            for pn in interpNodes do nodeCenterMap[pn.Id] <- pn.Center

            let srcGroups = Dictionary<string, PositionedGroup>(StringComparer.Ordinal)
            let tgtGroups = Dictionary<string, PositionedGroup>(StringComparer.Ordinal)
            let groupIds = HashSet<string>(StringComparer.Ordinal)
            for g in source.Groups do srcGroups[g.Id] <- g; groupIds.Add g.Id |> ignore
            for g in target.Groups do tgtGroups[g.Id] <- g; groupIds.Add g.Id |> ignore
            let rawGroups = ResizeArray<PositionedGroup>(groupIds.Count)
            for gid in groupIds do
                match srcGroups.TryGetValue gid, tgtGroups.TryGetValue gid with
                | (true, sg), (true, tg) ->
                    if alpha <= 0. then rawGroups.Add sg
                    elif alpha >= 1. then rawGroups.Add tg
                    else
                        rawGroups.Add
                            { Id = gid
                              ParentGroup = if alpha < 0.5 then sg.ParentGroup else tg.ParentGroup
                              Bounds = lerpRect sg.Bounds tg.Bounds alpha
                              InnerBounds = lerpRect sg.InnerBounds tg.InnerBounds alpha
                              Collapsed = if alpha < 0.5 then sg.Collapsed else tg.Collapsed
                              Depth = if alpha < 0.5 then sg.Depth else tg.Depth }
                | (true, sg), _ -> rawGroups.Add sg
                | _, (true, tg) -> rawGroups.Add tg
                | _ -> ()
            let groupArr = rawGroups.ToArray()
            Array.sortInPlaceWith (fun (a: PositionedGroup) (b: PositionedGroup) ->
                let c = compare a.Depth b.Depth
                if c <> 0 then c else String.CompareOrdinal(a.Id, b.Id)) groupArr
            let interpGroups = Array.toList groupArr

            let srcAssocs = Dictionary<string, RoutedAssociation>(StringComparer.Ordinal)
            let tgtAssocs = Dictionary<string, RoutedAssociation>(StringComparer.Ordinal)
            let assocIds = HashSet<string>(StringComparer.Ordinal)
            for a in source.Associations do srcAssocs[a.Id] <- a; assocIds.Add a.Id |> ignore
            for a in target.Associations do tgtAssocs[a.Id] <- a; assocIds.Add a.Id |> ignore
            let sortedAssocIds = Seq.toArray assocIds
            Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(a, b)) sortedAssocIds

            let fallbackPt =
                Point.create
                    (lerpScalar (Bounds.x source.Bounds + 0.5 * Bounds.width source.Bounds) (Bounds.x target.Bounds + 0.5 * Bounds.width target.Bounds) alpha)
                    (lerpScalar (Bounds.y source.Bounds + 0.5 * Bounds.height source.Bounds) (Bounds.y target.Bounds + 0.5 * Bounds.height target.Bounds) alpha)

            let interpAssocs =
                [ for i in 0 .. sortedAssocIds.Length - 1 do
                    let aid = sortedAssocIds[i]
                    match srcAssocs.TryGetValue aid, tgtAssocs.TryGetValue aid with
                    | (true, sa), (true, ta) ->
                        if alpha <= 0. then yield sa
                        elif alpha >= 1. then yield ta
                        else
                            let sFallback =
                                match nodeCenterMap.TryGetValue sa.Source.NodeId with
                                | true, p -> p
                                | _ -> fallbackPt
                            let sChain = resampleWaypoints sa.Waypoints sFallback 13
                            let tChain = resampleWaypoints ta.Waypoints sFallback 13
                            let mPts = Array.init 13 (fun k -> lerpPoint (sChain[k]) (tChain[k]) alpha)
                            let optPts = [ for k in 0 .. 12 -> Some (mPts[k]) ]
                            let path = Curves.line (CurveSpec.CatmullRom(0.5, CurveBoundary.Clamped)) optPts
                            yield
                                { Id = aid
                                  Source = if alpha < 0.5 then sa.Source else ta.Source
                                  Target = if alpha < 0.5 then sa.Target else ta.Target
                                  Kind = if alpha < 0.5 then sa.Kind else ta.Kind
                                  Waypoints = Array.toList mPts
                                  Path = path
                                  ReversedInCycle = if alpha < 0.5 then sa.ReversedInCycle else ta.ReversedInCycle
                                  SourceLabel = lerpOptionalLabel sa.SourceLabel ta.SourceLabel alpha
                                  CenterLabel = lerpOptionalLabel sa.CenterLabel ta.CenterLabel alpha
                                  TargetLabel = lerpOptionalLabel sa.TargetLabel ta.TargetLabel alpha }
                    | (true, sa), _ -> yield sa
                    | _, (true, ta) -> yield ta
                    | _ -> () ]

            let interpBounds =
                if alpha <= 0. then source.Bounds
                elif alpha >= 1. then target.Bounds
                else
                    let baseRect = lerpRect source.Bounds target.Bounds alpha
                    GraphKernels.computeDiagramBounds baseRect interpNodes interpGroups interpAssocs

            Ok
                { Bounds = interpBounds
                  Nodes = interpNodes
                  Groups = interpGroups
                  Associations = interpAssocs }

/// <summary>A candidate text label with anchor coordinate, dimensions, priority score and preferred offset candidates.</summary>
type LabelCandidate<'Id> =
    { /// <summary>Identifier associated with this label candidate.</summary>
      Id: 'Id
      /// <summary>Reference anchor point in scene coordinates.</summary>
      Anchor: Point
      /// <summary>Positive finite label box width in scene units.</summary>
      Width: float
      /// <summary>Positive finite label box height in scene units.</summary>
      Height: float
      /// <summary>Priority weight; higher-priority candidates are evaluated first.</summary>
      Priority: float
      /// <summary>Candidate label-center offsets from <c>Anchor</c> tried in order (defaults to <c>(0, -0.75 * Height)</c> when empty).</summary>
      OffsetCandidates: Point list }

/// <summary>An accepted non-overlapping label placed inside the viewport by <c>LabelCull.select</c>.</summary>
type PlacedLabelCandidate<'Id> =
    { /// <summary>Identifier of the accepted label candidate.</summary>
      Id: 'Id
      /// <summary>Placed axis-aligned label bounding box strictly inside the viewport.</summary>
      Bounds: Bounds
      /// <summary>Center point of <c>Bounds</c>.</summary>
      Center: Point
      /// <summary>Priority weight of the accepted candidate.</summary>
      Priority: float }

/// <summary>Greedy priority-ordered 2D spatial-grid label culling avoiding viewport overflow, obstacles and label-label collisions.</summary>
[<RequireQualifiedAccess>]
module LabelCull =
    /// <summary>Selects a non-overlapping subset of <c>candidates</c> strictly inside <c>viewport</c> avoiding <c>obstacles</c> and previously placed labels inflated by <c>margin</c>, returned in descending priority order (tie-broken by original candidate index).</summary>
    let select
        (viewport: Bounds)
        (margin: float)
        (obstacles: Bounds list)
        (candidates: LabelCandidate<'Id> list)
        : PlacedLabelCandidate<'Id> list =
        let vx0 = Bounds.x viewport
        let vy0 = Bounds.y viewport
        let vw = Bounds.width viewport
        let vh = Bounds.height viewport
        if not (Double.IsFinite vx0 && Double.IsFinite vy0 && Double.IsFinite vw && Double.IsFinite vh)
           || vw <= 0. || vh <= 0. || candidates.IsEmpty then
            []
        else
            let vx1 = vx0 + vw
            let vy1 = vy0 + vh
            let effMargin = if Double.IsFinite margin && margin > 0. then margin else 0.
            let candArr = List.toArray candidates
            let cLen = candArr.Length

            // Estimate grid cell size from median/mean candidate dimensions
            let mutable sumW = 0.
            let mutable sumH = 0.
            let mutable validCnt = 0
            for i = 0 to cLen - 1 do
                let c = candArr[i]
                if Double.IsFinite c.Width && Double.IsFinite c.Height && c.Width > 0. && c.Height > 0. then
                    sumW <- sumW + c.Width
                    sumH <- sumH + c.Height
                    validCnt <- validCnt + 1

            if validCnt = 0 then
                []
            else
                let meanW = max 16. (sumW / float validCnt + effMargin)
                let meanH = max 12. (sumH / float validCnt + effMargin)
                let cols = max 1 (min 128 (int (ceil (vw / meanW))))
                let rows = max 1 (min 128 (int (ceil (vh / meanH))))
                let cellW = vw / float cols
                let cellH = vh / float rows

                let boxX0 = ResizeArray<float>(obstacles.Length + cLen)
                let boxY0 = ResizeArray<float>(obstacles.Length + cLen)
                let boxX1 = ResizeArray<float>(obstacles.Length + cLen)
                let boxY1 = ResizeArray<float>(obstacles.Length + cLen)
                let seenStamp = ResizeArray<int>(obstacles.Length + cLen)
                let cells = Array.init (cols * rows) (fun _ -> ResizeArray<int>())
                let mutable queryStamp = 0

                let inline colOf (x: float) =
                    max 0 (min (cols - 1) (int (floor ((x - vx0) / cellW))))

                let inline rowOf (y: float) =
                    max 0 (min (rows - 1) (int (floor ((y - vy0) / cellH))))

                let insertInflated (bx0: float) (by0: float) (bx1: float) (by1: float) =
                    let ix0 = bx0 - effMargin
                    let iy0 = by0 - effMargin
                    let ix1 = bx1 + effMargin
                    let iy1 = by1 + effMargin
                    if ix1 > vx0 && ix0 < vx1 && iy1 > vy0 && iy0 < vy1 then
                        let bid = boxX0.Count
                        boxX0.Add ix0
                        boxY0.Add iy0
                        boxX1.Add ix1
                        boxY1.Add iy1
                        seenStamp.Add 0
                        let c0 = colOf ix0
                        let c1 = colOf ix1
                        let r0 = rowOf iy0
                        let r1 = rowOf iy1
                        for r = r0 to r1 do
                            let rBase = r * cols
                            for c = c0 to c1 do
                                cells[rBase + c].Add bid

                let collides (lx0: float) (ly0: float) (lx1: float) (ly1: float) : bool =
                    queryStamp <- queryStamp + 1
                    let stamp = queryStamp
                    let c0 = colOf lx0
                    let c1 = colOf lx1
                    let r0 = rowOf ly0
                    let r1 = rowOf ly1
                    let mutable hit = false
                    let mutable r = r0
                    while not hit && r <= r1 do
                        let rBase = r * cols
                        let mutable c = c0
                        while not hit && c <= c1 do
                            let bucket = cells[rBase + c]
                            let mutable k = 0
                            while not hit && k < bucket.Count do
                                let bid = bucket[k]
                                if seenStamp[bid] <> stamp then
                                    seenStamp[bid] <- stamp
                                    if lx0 < boxX1[bid] - 1e-9
                                       && lx1 > boxX0[bid] + 1e-9
                                       && ly0 < boxY1[bid] - 1e-9
                                       && ly1 > boxY0[bid] + 1e-9 then
                                        hit <- true
                                k <- k + 1
                            c <- c + 1
                        r <- r + 1
                    hit

                for obs in obstacles do
                    let ox = Bounds.x obs
                    let oy = Bounds.y obs
                    let ow = Bounds.width obs
                    let oh = Bounds.height obs
                    if ow > 0. && oh > 0. then
                        insertInflated ox oy (ox + ow) (oy + oh)

                // Sort indices descending by Priority, tie-breaking by ascending original index (keeps 'Id unconstrained!)
                let order = Array.init cLen id
                Array.sortInPlaceWith (fun a b ->
                    let pa = candArr[a].Priority
                    let pb = candArr[b].Priority
                    let c =
                        if Double.IsNaN pa && Double.IsNaN pb then 0
                        elif Double.IsNaN pa then 1
                        elif Double.IsNaN pb then -1
                        else compare pb pa
                    if c <> 0 then c else compare a b) order

                let accepted = ResizeArray<PlacedLabelCandidate<'Id>>()
                for oi = 0 to cLen - 1 do
                    let cand = candArr[order[oi]]
                    if Double.IsFinite cand.Anchor.X
                       && Double.IsFinite cand.Anchor.Y
                       && Double.IsFinite cand.Width
                       && Double.IsFinite cand.Height
                       && Double.IsFinite cand.Priority
                       && cand.Width > 0.
                       && cand.Height > 0. then
                        let offsets =
                            if cand.OffsetCandidates.IsEmpty then
                                [ Point.create 0. (-cand.Height * 0.75) ]
                            else
                                cand.OffsetCandidates

                        let mutable placed = false
                        let mutable rem = offsets
                        while not placed && not rem.IsEmpty do
                            let off = rem.Head
                            rem <- rem.Tail
                            if Double.IsFinite off.X && Double.IsFinite off.Y then
                                let cx = cand.Anchor.X + off.X
                                let cy = cand.Anchor.Y + off.Y
                                let lx0 = cx - 0.5 * cand.Width
                                let ly0 = cy - 0.5 * cand.Height
                                let lx1 = lx0 + cand.Width
                                let ly1 = ly0 + cand.Height
                                if lx0 >= vx0 && ly0 >= vy0 && lx1 <= vx1 && ly1 <= vy1 then
                                    if not (collides lx0 ly0 lx1 ly1) then
                                        placed <- true
                                        insertInflated lx0 ly0 lx1 ly1
                                        accepted.Add
                                            { Id = cand.Id
                                              Bounds = Bounds.create lx0 ly0 cand.Width cand.Height
                                              Center = Point.create cx cy
                                              Priority = cand.Priority }

                List.ofSeq accepted

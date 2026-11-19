namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

module private CircularOrderKernel =
    /// <summary>Builds symmetric integer chord-multiplicity CSR over <c>0 .. n - 1</c> ignoring self-loops.</summary>
    let buildMultiplicityGraph (n: int) (src: int[]) (dst: int[]) : int[] * int[] * int[] * int[] =
        let pairMult = Dictionary<uint64, int>()
        for e = 0 to src.Length - 1 do
            let u, v = src[e], dst[e]
            if u <> v then
                let a, b = min u v, max u v
                let key = (uint64 a <<< 32) ||| uint64 (uint32 b)
                match pairMult.TryGetValue key with
                | true, prev -> pairMult[key] <- prev + 1
                | _ -> pairMult.Add(key, 1)

        let deg = Array.zeroCreate<int> n
        let weightedDeg = Array.zeroCreate<int> n
        for kv in pairMult do
            let a = int (kv.Key >>> 32)
            let b = int (uint32 kv.Key)
            let w = kv.Value
            deg[a] <- deg[a] + 1
            deg[b] <- deg[b] + 1
            weightedDeg[a] <- weightedDeg[a] + w
            weightedDeg[b] <- weightedDeg[b] + w

        let offsets = Array.zeroCreate<int> (n + 1)
        for i = 0 to n - 1 do offsets[i + 1] <- offsets[i] + deg[i]
        let total = offsets[n]
        let targets = Array.zeroCreate<int> total
        let mults = Array.zeroCreate<int> total
        let cur = Array.copy offsets

        let pairs = Seq.toArray pairMult
        Array.sortInPlaceWith (fun (a: KeyValuePair<uint64, int>) (b: KeyValuePair<uint64, int>) -> compare a.Key b.Key) pairs
        for i = 0 to pairs.Length - 1 do
            let key = pairs[i].Key
            let w = pairs[i].Value
            let a = int (key >>> 32)
            let b = int (uint32 key)
            let ca = cur[a]
            targets[ca] <- b; mults[ca] <- w; cur[a] <- ca + 1
            let cb = cur[b]
            targets[cb] <- a; mults[cb] <- w; cur[b] <- cb + 1

        offsets, targets, mults, weightedDeg

    /// <summary>Phase 1 of Baur &amp; Brandes (2005): Greedy Connectivity Deque placement on a subset <c>subset</c> of vertices.</summary>
    let greedyConnectivityDeque
        (nTotal: int)
        (offsets: int[])
        (targets: int[])
        (mults: int[])
        (subset: int[])
        : int[] =
        let k = subset.Length
        if k <= 2 then Array.copy subset
        else
            let inSub = Array.create nTotal -1
            for li = 0 to k - 1 do inSub[subset[li]] <- li

            let unplacedW = Array.zeroCreate<int> k
            let placedW = Array.zeroCreate<int> k
            for li = 0 to k - 1 do
                let u = subset[li]
                let mutable wSum = 0
                for idx = offsets[u] to offsets[u + 1] - 1 do
                    let lv = inSub[targets[idx]]
                    if lv >= 0 then wSum <- wSum + mults[idx]
                unplacedW[li] <- wSum

            let isPlaced = Array.zeroCreate<bool> k
            // Double-ended buffer centered at index k
            let deque = Array.zeroCreate<int> (2 * k + 2)
            let mutable head = k
            let mutable tail = k // [head .. tail - 1]
            let posInDeque = Array.zeroCreate<int> k

            // Pick initial seed with maximum internal degree (tie-break by local index)
            let mutable first = 0
            for li = 1 to k - 1 do
                if unplacedW[li] > unplacedW[first] then first <- li

            let placeVertex (li: int) (atFront: bool) =
                isPlaced[li] <- true
                if atFront then
                    head <- head - 1
                    deque[head] <- li
                else
                    deque[tail] <- li
                    tail <- tail + 1
                let u = subset[li]
                for idx = offsets[u] to offsets[u + 1] - 1 do
                    let lv = inSub[targets[idx]]
                    if lv >= 0 then
                        let w = mults[idx]
                        unplacedW[lv] <- unplacedW[lv] - w
                        if not isPlaced[lv] then
                            placedW[lv] <- placedW[lv] + w

            placeVertex first false

            for _step = 1 to k - 1 do
                let mutable best = -1
                for li = 0 to k - 1 do
                    if not isPlaced[li] then
                        if best < 0
                           || placedW[li] > placedW[best]
                           || (placedW[li] = placedW[best] && unplacedW[li] < unplacedW[best]) then
                            best <- li

                let pLen = tail - head
                if pLen <= 1 || placedW[best] = 0 then
                    placeVertex best false
                else
                    // Baur & Brandes open-edge prefix sum rule: compare front vs back insertion cost!
                    let openPref = Array.zeroCreate<int> (pLen + 1)
                    for di = 0 to pLen - 1 do
                        let dLi = deque[head + di]
                        posInDeque[dLi] <- di
                        openPref[di + 1] <- openPref[di] + max 0 unplacedW[dLi]
                    let totalOpen = openPref[pLen]

                    let mutable costFront = 0
                    let mutable costBack = 0
                    let u = subset[best]
                    for idx = offsets[u] to offsets[u + 1] - 1 do
                        let lv = inSub[targets[idx]]
                        if lv >= 0 && isPlaced[lv] then
                            let di = posInDeque[lv]
                            let w = mults[idx]
                            costFront <- costFront + w * openPref[di]
                            costBack <- costBack + w * (totalOpen - openPref[di + 1])

                    placeVertex best (costFront < costBack)

            Array.init k (fun i -> subset[deque[head + i]])

    /// <summary>Phase 2 of Baur &amp; Brandes (2005): <c>O(V * E)</c> exact crossing-delta incremental circular sifting.</summary>
    let circularSifting
        (token: System.Threading.CancellationToken)
        (n: int)
        (offsets: int[])
        (targets: int[])
        (mults: int[])
        (weightedDeg: int[])
        (sweeps: int)
        (order: int[])
        : unit =
        if n >= 4 && sweeps > 0 then
            let siftingOrder = Array.init n (fun i -> i)
            Array.sortInPlaceWith (fun a b ->
                let c = compare weightedDeg[b] weightedDeg[a]
                if c <> 0 then c else compare a b) siftingOrder

            let ringLen = n - 1
            let ring = Array.zeroCreate<int> ringLen
            let ringPos = Array.create n -1
            let uPos = Array.zeroCreate<int> n
            let uMultAt = Array.zeroCreate<int> n
            let uPref = Array.zeroCreate<int> (n + 1)

            // Sum of u-neighbor multiplicities with ring index strictly in (lo, hi)
            let rec lowerBound (arr: int[]) (len: int) (value: int) =
                let mutable lo = 0
                let mutable hi = len
                while lo < hi do
                    let mid = (lo + hi) >>> 1
                    if arr[mid] < value then lo <- mid + 1 else hi <- mid
                lo

            let inline sumInterval (cnt: int) (loOpen: int) (hiOpen: int) : int =
                if hiOpen <= loOpen + 1 || cnt = 0 then 0
                else
                    let a = lowerBound uPos cnt (loOpen + 1)
                    let b = lowerBound uPos cnt hiOpen
                    if b > a then uPref[b] - uPref[a] else 0

            let mutable sweep = 0
            let mutable improved = true
            while sweep < sweeps && improved do
                token.ThrowIfCancellationRequested()
                improved <- false
                for si = 0 to n - 1 do
                    SimdMath.check token si
                    let u = siftingOrder[si]
                    let degU = offsets[u + 1] - offsets[u]
                    if degU > 0 then
                        // Find current slot of u in `order` and build clockwise ring w_0 .. w_{n-2} of V \ {u}
                        let mutable uSlot = 0
                        while uSlot < n && order[uSlot] <> u do uSlot <- uSlot + 1
                        for k = 0 to ringLen - 1 do
                            let w = order[(uSlot + 1 + k) % n]
                            ring[k] <- w
                            ringPos[w] <- k
                        ringPos[u] <- -1

                        // Gather u's neighbors sorted by ringPos
                        let mutable uCnt = 0
                        for idx = offsets[u] to offsets[u + 1] - 1 do
                            let x = targets[idx]
                            let rp = ringPos[x]
                            if rp >= 0 then
                                // Insertion into sorted uPos (or sort after)
                                uPos[uCnt] <- rp
                                uMultAt[rp] <- mults[idx]
                                uCnt <- uCnt + 1
                        if uCnt > 1 then
                            if uCnt <= 32 then
                                for a = 1 to uCnt - 1 do
                                    let kv = uPos[a]
                                    let mutable b = a - 1
                                    while b >= 0 && uPos[b] > kv do
                                        uPos[b + 1] <- uPos[b]
                                        b <- b - 1
                                    uPos[b + 1] <- kv
                            else
                                let mutable gap = uCnt / 2
                                while gap > 0 do
                                    for a = gap to uCnt - 1 do
                                        let kv = uPos[a]
                                        let mutable b = a
                                        while b >= gap && uPos[b - gap] > kv do
                                            uPos[b] <- uPos[b - gap]
                                            b <- b - gap
                                        uPos[b] <- kv
                                    gap <- gap / 2
                        uPref[0] <- 0
                        for i = 0 to uCnt - 1 do
                            uPref[i + 1] <- uPref[i] + uMultAt[uPos[i]]
                        let totalUW = uPref[uCnt]

                        // Sweep k = 0 .. ringLen - 1: slot k means u sits right before ring[k] (slot 0 = original position!)
                        let mutable cumDelta = 0
                        let mutable bestDelta = 0
                        let mutable bestK = 0

                        for k = 0 to ringLen - 2 do
                            let vk = ring[k]
                            let muk = uMultAt[k] // multiplicity of (u, vk) if adjacent, else 0
                            let mutable stepDelta = 0
                            for idx = offsets[vk] to offsets[vk + 1] - 1 do
                                let y = targets[idx]
                                if y <> u then
                                    let jy = ringPos[y]
                                    let mvy = mults[idx]
                                    let myu = uMultAt[jy]
                                    // Count weight of x in N(u) \ {vk, y} with rotRank(jx) < rotRank(jy) (`beforeW`)
                                    let beforeW =
                                        if jy > k then
                                            sumInterval uCnt k jy
                                        else
                                            (totalUW - uPref[lowerBound uPos uCnt (k + 1)]) + sumInterval uCnt -1 jy
                                    let afterW = totalUW - muk - myu - beforeW
                                    stepDelta <- stepDelta + mvy * (afterW - beforeW)

                            cumDelta <- cumDelta + stepDelta
                            if cumDelta < bestDelta then
                                bestDelta <- cumDelta
                                bestK <- k + 1

                        // Clear uMultAt entries we touched
                        for i = 0 to uCnt - 1 do
                            uMultAt[uPos[i]] <- 0

                        if bestDelta < 0 && bestK > 0 then
                            improved <- true
                            // Rebuild `order` with u placed immediately after ring[bestK - 1]:
                            // [ring[0] .. ring[bestK - 1]; u; ring[bestK] .. ring[ringLen - 1]]
                            for k = 0 to bestK - 1 do order[k] <- ring[k]
                            order[bestK] <- u
                            for k = bestK to ringLen - 1 do order[k + 1] <- ring[k]

                sweep <- sweep + 1

    /// <summary>Computes a crossing-minimized circular vertex order over <c>graph</c>.</summary>
    let computeOrder
        (token: System.Threading.CancellationToken)
        (orderByCommunities: bool)
        (siftingSweeps: int)
        (eff: EffectiveDiagram)
        (csr: CsrGraph)
        (graph: DiagramGraph)
        : int[] =
        let n = csr.NodeCount
        if n <= 2 then
            Array.init n (fun i -> i)
        else
            let offsets, targets, mults, weightedDeg = buildMultiplicityGraph n csr.Src csr.Dst
            let initOrder =
                if not orderByCommunities || n <= 3 then
                    greedyConnectivityDeque n offsets targets mults (Array.init n (fun i -> i))
                else
                    // Determine community blocks from explicit ParentGroup if present, else Leiden modularity!
                    let hasExplicitGroups = eff.Nodes |> Array.exists (fun nd -> nd.ParentGroup.IsSome)
                    let commArr, commCount =
                        if hasExplicitGroups then
                            let gMap = Dictionary<string, int>(StringComparer.Ordinal)
                            let arr = Array.zeroCreate<int> n
                            let mutable nextC = 0
                            for i = 0 to n - 1 do
                                let key = match eff.Nodes[i].ParentGroup with Some g -> "g:" + g | None -> "u:" + string i
                                match gMap.TryGetValue key with
                                | true, c -> arr[i] <- c
                                | _ -> gMap.Add(key, nextC); arr[i] <- nextC; nextC <- nextC + 1
                            arr, nextC
                        else
                            match Leiden.detectWithCancellation token LeidenConfig.defaultValue graph with
                            | Ok part when part.CommunityCount > 1 && part.CommunityCount < n ->
                                let arr = Array.init n (fun i -> Map.find eff.Nodes[i].Id part.Communities)
                                arr, part.CommunityCount
                            | _ ->
                                Array.zeroCreate<int> n, 1

                    if commCount <= 1 then
                        greedyConnectivityDeque n offsets targets mults (Array.init n (fun i -> i))
                    else
                        let buckets = Array.init commCount (fun _ -> ResizeArray<int>())
                        for i = 0 to n - 1 do buckets[commArr[i]].Add i

                        // Order communities on the quotient graph via greedyConnectivityDeque!
                        let qSrc = ResizeArray<int>()
                        let qDst = ResizeArray<int>()
                        for e = 0 to csr.EdgeCount - 1 do
                            let cu = commArr[csr.Src[e]]
                            let cv = commArr[csr.Dst[e]]
                            if cu <> cv then
                                qSrc.Add cu
                                qDst.Add cv
                        let qOff, qTgt, qMult, _ = buildMultiplicityGraph commCount (qSrc.ToArray()) (qDst.ToArray())
                        let commOrder = greedyConnectivityDeque commCount qOff qTgt qMult (Array.init commCount (fun c -> c))

                        let combined = ResizeArray<int>(n)
                        for ci = 0 to commCount - 1 do
                            let c = commOrder[ci]
                            let sub = buckets[c].ToArray()
                            let subOrd = greedyConnectivityDeque n offsets targets mults sub
                            // Orient subOrd forward vs reversed to align best with preceding block
                            if combined.Count > 0 && subOrd.Length >= 2 then
                                let prevSet = HashSet<int>()
                                let lookback = max 0 (combined.Count - 16)
                                for p = lookback to combined.Count - 1 do prevSet.Add combined[p] |> ignore
                                let mutable fwdBias = 0
                                for si = 0 to subOrd.Length - 1 do
                                    let u = subOrd[si]
                                    let weightPos = (subOrd.Length - 1) - 2 * si
                                    for idx = offsets[u] to offsets[u + 1] - 1 do
                                        if prevSet.Contains targets[idx] then
                                            fwdBias <- fwdBias + weightPos * mults[idx]
                                if fwdBias < 0 then
                                    let half = subOrd.Length / 2
                                    for k = 0 to half - 1 do
                                        let opp = subOrd.Length - 1 - k
                                        let tmp = subOrd[k]
                                        subOrd[k] <- subOrd[opp]
                                        subOrd[opp] <- tmp
                            for k = 0 to subOrd.Length - 1 do combined.Add subOrd[k]
                        combined.ToArray()

            circularSifting token n offsets targets mults weightedDeg siftingSweeps initOrder
            initOrder

/// <summary>Settings for crossing-minimized circular diagram layout (Baur &amp; Brandes 2005).</summary>
type CircularConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>When true, groups communities contiguously along the perimeter before sifting.</summary>
      OrderByCommunities: bool
      /// <summary>Number of <c>O(V * E)</c> circular sifting sweeps.</summary>
      SiftingSweeps: int }

/// <summary>Constructors for <c>CircularConfig</c>.</summary>
[<RequireQualifiedAccess>]
module CircularConfig =
    /// <summary>Creates a circular layout configuration with community grouping enabled and 4 sifting sweeps.</summary>
    let create bounds =
        { Bounds = bounds
          OrderByCommunities = true
          SiftingSweeps = 4 }

/// <summary>Baur &amp; Brandes (2005) two-phase crossing-minimized circular layout engine.</summary>
[<RequireQualifiedAccess>]
module CircularLayout =
    /// <summary>Counts the exact number of chord crossings among non-self associations of <c>graph</c> induced by cyclic node order <c>order</c>.</summary>
    let countCrossings (order: string list) (graph: DiagramGraph) : int =
        let posMap = Dictionary<string, int>(StringComparer.Ordinal)
        let mutable slot = 0
        for nid in order do
            if not (posMap.ContainsKey nid) then
                posMap.Add(nid, slot)
                slot <- slot + 1
        if slot < 4 then 0
        else
            let eff = DiagramGraph.effective graph
            let chords = ResizeArray<struct(int * int)>(eff.Associations.Length)
            for e = 0 to eff.Associations.Length - 1 do
                let a = eff.Associations[e]
                let sId, tId = a.Source.NodeId, a.Target.NodeId
                if sId <> tId then
                    match posMap.TryGetValue sId, posMap.TryGetValue tId with
                    | (true, ps), (true, pt) when ps <> pt ->
                        chords.Add(struct(min ps pt, max ps pt))
                    | _ -> ()
            let m = chords.Count
            let mutable crossings = 0
            for i = 0 to m - 2 do
                let struct(a1, b1) = chords[i]
                for j = i + 1 to m - 1 do
                    let struct(a2, b2) = chords[j]
                    if (a1 < a2 && a2 < b1 && b1 < b2) || (a2 < a1 && a1 < b2 && b2 < b1) then
                        crossings <- crossings + 1
            crossings

    /// <summary>Lays out <c>graph</c> around a crossing-minimized circle with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: CircularConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if config.SiftingSweeps < 0 || config.SiftingSweeps > 200 then
            Error [ InvalidObservation "CircularLayout SiftingSweeps must be in [0, 200]." ]
        else
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let csr = GraphKernels.buildCsr eff
                let order = CircularOrderKernel.computeOrder token config.OrderByCommunities config.SiftingSweeps eff csr graph

                let cx0 = Bounds.x config.Bounds + Bounds.width config.Bounds * 0.5
                let cy0 = Bounds.y config.Bounds + Bounds.height config.Bounds * 0.5

                let centers = Array.zeroCreate<Point> n
                let rankOf = Array.zeroCreate<int> n
                if n = 1 then
                    centers[order[0]] <- Point.create cx0 cy0
                    rankOf[order[0]] <- 0
                else
                    let mutable maxDiag = 0.
                    let spans = Array.zeroCreate<float> n
                    let mutable totalSpan = 0.
                    for r = 0 to n - 1 do
                        let ni = order[r]
                        rankOf[ni] <- r
                        let nd = eff.Nodes[ni]
                        let diag = Double.Hypot(nd.Size.Width, nd.Size.Height)
                        if diag > maxDiag then maxDiag <- diag
                        let sp = diag + 14.
                        spans[r] <- sp
                        totalSpan <- totalSpan + sp

                    let viewportR = max 24. (min (Bounds.width config.Bounds) (Bounds.height config.Bounds) * 0.5 - maxDiag * 0.5 - 8.)
                    let minNoOverlapR = totalSpan / (2. * Math.PI)
                    let radius =
                        if Bounds.width config.Bounds > 1e-3 && Bounds.height config.Bounds > 1e-3 then
                            max viewportR (min (viewportR * 1.5) minNoOverlapR)
                        else
                            max 36. minNoOverlapR

                    let mutable cumAngle = -Math.PI * 0.5
                    for r = 0 to n - 1 do
                        let ni = order[r]
                        let dTheta = 2. * Math.PI * (spans[r] / totalSpan)
                        let midTheta = cumAngle + 0.5 * dTheta
                        centers[ni] <- Point.create (cx0 + radius * cos midTheta) (cy0 + radius * sin midTheta)
                        cumAngle <- cumAngle + dTheta

                let posNodes =
                    Array.init n (fun ni ->
                        let nd = eff.Nodes[ni]
                        let c = match nd.Pinned with Some p -> p | None -> centers[ni]
                        let b = Bounds.create (c.X - nd.Size.Width * 0.5) (c.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                        { Id = nd.Id
                          Kind = nd.Kind
                          Bounds = b
                          Center = c
                          ParentGroup = nd.ParentGroup
                          Layer = Some rankOf[ni]
                          Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                let groups = GraphKernels.computeGroupBounds eff (posNodes |> Array.map (fun pn -> pn.Bounds))
                let routed = GraphKernels.routeDirectAssociations eff posNodes None
                let nList = Array.toList posNodes
                Ok
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

    /// <summary>Lays out <c>graph</c> around a crossing-minimized circle without cancellation.</summary>
    let layout config graph =
        layoutWithCancellation System.Threading.CancellationToken.None config graph

/// <summary>Settings for Wattenberg (2002) linear baseline semicircular Arc Diagram layout.</summary>
type ArcConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Number of crossing-reduction sifting sweeps.</summary>
      SiftingSweeps: int
      /// <summary>When true, routes forward associations above the baseline and backward associations below the baseline.</summary>
      BidirectionalSplitHemispheres: bool }

/// <summary>Constructors for <c>ArcConfig</c>.</summary>
[<RequireQualifiedAccess>]
module ArcConfig =
    /// <summary>Creates an arc diagram configuration with 4 sifting sweeps and bidirectional upper/lower hemisphere splitting.</summary>
    let create bounds =
        { Bounds = bounds
          SiftingSweeps = 4
          BidirectionalSplitHemispheres = true }

/// <summary>Wattenberg (2002) linear Arc Diagram layout engine with semicircular SVG <c>ArcTo</c> routing.</summary>
[<RequireQualifiedAccess>]
module ArcLayout =
    /// <summary>Places crossing-minimized nodes horizontally along a baseline and routes associations as semicircular SVG <c>ArcTo</c> arcs with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: ArcConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if config.SiftingSweeps < 0 || config.SiftingSweeps > 200 then
            Error [ InvalidObservation "ArcLayout SiftingSweeps must be in [0, 200]." ]
        else
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let csr = GraphKernels.buildCsr eff
                let cycOrder = CircularOrderKernel.computeOrder token true config.SiftingSweeps eff csr graph

                // Pick cyclic cut index 0..n-1 that minimizes edges wrapping across the linear cut!
                let cutIndex =
                    if n <= 2 || csr.EdgeCount = 0 then 0
                    else
                        let cycPos = Array.zeroCreate<int> n
                        for r = 0 to n - 1 do cycPos[cycOrder[r]] <- r
                        // Cutting between r and (r+1)%n cuts every edge (a, b) with a <= r < b
                        let diff = Array.zeroCreate<float> (n + 1)
                        for e = 0 to csr.EdgeCount - 1 do
                            let u, v = csr.Src[e], csr.Dst[e]
                            if u <> v then
                                let a, b = min cycPos[u] cycPos[v], max cycPos[u] cycPos[v]
                                let w = csr.Weight[e]
                                diff[a] <- diff[a] + w
                                diff[b] <- diff[b] - w
                        let mutable bestCut = 0
                        let mutable curCutW = 0.
                        let mutable minCutW = infinity
                        for r = 0 to n - 1 do
                            curCutW <- curCutW + diff[r]
                            if curCutW < minCutW then
                                minCutW <- curCutW
                                bestCut <- (r + 1) % n
                        bestCut

                let linOrder = Array.init n (fun r -> cycOrder[(cutIndex + r) % n])
                let linPos = Array.zeroCreate<int> n
                for r = 0 to n - 1 do linPos[linOrder[r]] <- r

                let mutable hasBackward = false
                if config.BidirectionalSplitHemispheres then
                    for e = 0 to csr.EdgeCount - 1 do
                        let u, v = csr.Src[e], csr.Dst[e]
                        if u <> v && linPos[u] > linPos[v] then hasBackward <- true

                let bx = Bounds.x config.Bounds
                let by = Bounds.y config.Bounds
                let bw = Bounds.width config.Bounds
                let bh = Bounds.height config.Bounds

                let baseY =
                    if bh <= 1e-3 then by
                    elif config.BidirectionalSplitHemispheres && hasBackward then by + bh * 0.5
                    else by + bh * 0.78

                let mutable sumWidths = 0.
                let mutable maxNodeH = 0.
                for i = 0 to n - 1 do
                    sumWidths <- sumWidths + eff.Nodes[i].Size.Width
                    if eff.Nodes[i].Size.Height > maxNodeH then maxNodeH <- eff.Nodes[i].Size.Height

                let minGap = 14.
                let availExtra = bw - sumWidths
                let gap = if n <= 1 then 0. else max minGap (availExtra / float (n + 1))
                let totalUsedW = sumWidths + gap * float (max 0 (n - 1))
                let startX = bx + max 0. ((bw - totalUsedW) * 0.5)

                let centers = Array.zeroCreate<Point> n
                let mutable cursorX = startX
                for r = 0 to n - 1 do
                    let ni = linOrder[r]
                    let w = eff.Nodes[ni].Size.Width
                    centers[ni] <- Point.create (cursorX + w * 0.5) baseY
                    cursorX <- cursorX + w + gap

                let posNodes =
                    Array.init n (fun ni ->
                        let nd = eff.Nodes[ni]
                        let c = match nd.Pinned with Some p -> p | None -> centers[ni]
                        let b = Bounds.create (c.X - nd.Size.Width * 0.5) (c.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                        { Id = nd.Id
                          Kind = nd.Kind
                          Bounds = b
                          Center = c
                          ParentGroup = nd.ParentGroup
                          Layer = Some linPos[ni]
                          Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                // Route semicircular / elliptical arcs above (and optionally below) baseline!
                let m = eff.Associations.Length
                let pairSeen = Dictionary<uint64, int>()
                let maxUpperRy = if bh > 1e-3 then max 24. (baseY - by - maxNodeH * 0.5 - 8.) else infinity
                let maxLowerRy = if bh > 1e-3 then max 24. (by + bh - baseY - maxNodeH * 0.5 - 8.) else infinity

                let outAssocs = Array.zeroCreate<RoutedAssociation> m
                for e = 0 to m - 1 do
                    let a = eff.Associations[e]
                    let ui = eff.ReversedIdMap[a.Source.NodeId]
                    let vi = eff.ReversedIdMap[a.Target.NodeId]
                    let uNd = posNodes[ui]
                    let vNd = posNodes[vi]
                    if ui = vi then
                        let topY = Bounds.y uNd.Bounds
                        let p0 = Point.create (uNd.Center.X - min 10. (Bounds.width uNd.Bounds * 0.25)) topY
                        let p1 = Point.create (uNd.Center.X + min 10. (Bounds.width uNd.Bounds * 0.25)) topY
                        let rLoop = max 10. (Double.Hypot(p1.X - p0.X, p1.Y - p0.Y) * 0.8)
                        let apex = Point.create uNd.Center.X (topY - rLoop * 1.6)
                        let wps = [| p0; apex; p1 |]
                        let path = Path.ofCommands [ MoveTo p0; ArcTo(rLoop, rLoop, 0., true, true, p1) ]
                        let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a wps
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
                        let above = (linPos[ui] < linPos[vi]) || (not config.BidirectionalSplitHemispheres)
                        let hemiTag = if above then 0u else 1u
                        let lo, hi = min ui vi, max ui vi
                        let pKey = (uint64 lo <<< 33) ||| (uint64 hi <<< 1) ||| uint64 hemiTag
                        let slot =
                            match pairSeen.TryGetValue pKey with
                            | true, s -> pairSeen[pKey] <- s + 1; s
                            | _ -> pairSeen.Add(pKey, 1); 0

                        let ySrc = if above then Bounds.y uNd.Bounds else Bounds.y uNd.Bounds + Bounds.height uNd.Bounds
                        let yDst = if above then Bounds.y vNd.Bounds else Bounds.y vNd.Bounds + Bounds.height vNd.Bounds
                        let pS = Point.create uNd.Center.X ySrc
                        let pT = Point.create vNd.Center.X yDst
                        let span = Double.Hypot(pT.X - pS.X, pT.Y - pS.Y)
                        let rx = max 6. (span * 0.5)
                        let capRy = if above then maxUpperRy else maxLowerRy
                        let ry = min capRy (rx + float slot * 10.)
                        let goingRight = pT.X >= pS.X
                        let clockwise = (goingRight = above)
                        let path = Path.ofCommands [ MoveTo pS; ArcTo(rx, ry, 0., false, clockwise, pT) ]

                        let hemiSign = if above then -1. else 1.
                        let sampleCount = 9
                        let wps =
                            Array.init sampleCount (fun k ->
                                if k = 0 then pS
                                elif k = sampleCount - 1 then pT
                                else
                                    let t = float k / float (sampleCount - 1)
                                    let theta = t * Math.PI
                                    let xInterp = pS.X + (pT.X - pS.X) * (0.5 * (1. - cos theta))
                                    let yBase = pS.Y + (pT.Y - pS.Y) * t
                                    Point.create xInterp (yBase + hemiSign * ry * sin theta))

                        let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a wps
                        outAssocs[e] <-
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              Waypoints = Array.toList wps
                              Path = path
                              ReversedInCycle = not above
                              SourceLabel = sLbl
                              CenterLabel = mLbl
                              TargetLabel = tLbl }

                let groups = GraphKernels.computeGroupBounds eff (posNodes |> Array.map (fun pn -> pn.Bounds))
                let routed = Array.toList outAssocs
                let nList = Array.toList posNodes
                Ok
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                      Nodes = nList
                      Groups = groups
                      Associations = routed }

    /// <summary>Lays out <c>graph</c> as a Wattenberg (2002) linear Arc Diagram without cancellation.</summary>
    let layout config graph =
        layoutWithCancellation System.Threading.CancellationToken.None config graph

/// <summary>Rule assigning diagram nodes to radial axes in a Krzywinski et al. (2012) Hive Plot.</summary>
[<RequireQualifiedAccess>]
type HiveAxisRule =
    /// <summary>Three semantic axes: axis 0 = sources, axis 1 = intermediaries/isolated, axis 2 = sinks.</summary>
    | SourceIntermediarySink
    /// <summary>Partitions nodes evenly by total degree into <c>axes</c> radial quantile tiers (<c>2 .. 32</c>).</summary>
    | DegreeTiers of axes: int
    /// <summary>Assigns one radial axis per compound <c>ParentGroup</c> (plus an ungrouped axis when needed).</summary>
    | ByGroup
    /// <summary>Custom deterministic function <c>(node, totalDegree) -&gt; axisIndex</c>.</summary>
    | Custom of (DiagramNode -> int -> int)

/// <summary>Settings for Krzywinski et al. (2012) deterministic radial Hive Plot layout.</summary>
type HiveConfig =
    { /// <summary>Target viewport bounds.</summary>
      Bounds: Bounds
      /// <summary>Node-to-axis assignment policy.</summary>
      AxisRule: HiveAxisRule
      /// <summary>Empty central hub radius in layout units where radial axes begin.</summary>
      InnerRadius: float
      /// <summary>When true, splits each axis into twin angular rails so intra-axis edges render as clean outer ribbons.</summary>
      SplitAxes: bool }

/// <summary>Constructors for <c>HiveConfig</c>.</summary>
[<RequireQualifiedAccess>]
module HiveConfig =
    /// <summary>Creates a 3-axis <c>SourceIntermediarySink</c> Hive Plot configuration with 36-unit inner radius.</summary>
    let create bounds =
        { Bounds = bounds
          AxisRule = HiveAxisRule.SourceIntermediarySink
          InnerRadius = 36.
          SplitAxes = false }

/// <summary>Krzywinski et al. (2012) Hive Plot layout engine.</summary>
[<RequireQualifiedAccess>]
module HivePlot =
    /// <summary>Lays out <c>graph</c> deterministically along radial Hive Plot axes with smooth polar Bézier ribbons and cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: HiveConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        let validRule =
            match config.AxisRule with
            | HiveAxisRule.DegreeTiers k -> k >= 2 && k <= 32
            | _ -> true
        if not (Double.IsFinite config.InnerRadius) || config.InnerRadius < 0. || not validRule then
            Error [ InvalidObservation "HivePlot requires finite nonnegative InnerRadius and DegreeTiers in [2, 32]." ]
        else
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            else
                let csr = GraphKernels.buildCsr eff
                let inDeg = Array.init n (fun u -> csr.InOffsets[u + 1] - csr.InOffsets[u])
                let outDeg = Array.init n (fun u -> csr.OutOffsets[u + 1] - csr.OutOffsets[u])
                let totDeg = Array.init n (fun u -> inDeg[u] + outDeg[u])

                let axisRes =
                    match config.AxisRule with
                    | HiveAxisRule.SourceIntermediarySink ->
                        let ax =
                            Array.init n (fun i ->
                                let kd = eff.Nodes[i].Kind
                                if kd = NodeKind.InitialState || (inDeg[i] = 0 && outDeg[i] > 0) then 0
                                elif kd = NodeKind.FinalState || (outDeg[i] = 0 && inDeg[i] > 0) then 2
                                else 1)
                        Ok(ax, 3)
                    | HiveAxisRule.DegreeTiers k ->
                        let sorted = Array.init n (fun i -> i)
                        Array.sortInPlaceWith (fun a b ->
                            let c = compare totDeg[a] totDeg[b]
                            if c <> 0 then c
                            else
                                let cm = compare eff.Nodes[a].Mass eff.Nodes[b].Mass
                                if cm <> 0 then cm else compare a b) sorted
                        let ax = Array.zeroCreate<int> n
                        for rank = 0 to n - 1 do
                            ax[sorted[rank]] <- min (k - 1) ((rank * k) / n)
                        Ok(ax, k)
                    | HiveAxisRule.ByGroup ->
                        let gSeen = Dictionary<string, int>(StringComparer.Ordinal)
                        let keys =
                            Array.init n (fun i ->
                                match eff.Nodes[i].ParentGroup with
                                | Some gid -> "0:" + gid
                                | None -> "1:__ungrouped__")
                        let distinctKeys = keys |> Array.distinct
                        Array.sortInPlaceWith (fun a b -> StringComparer.Ordinal.Compare(a, b)) distinctKeys
                        for idx = 0 to distinctKeys.Length - 1 do
                            gSeen.Add(distinctKeys[idx], idx)
                        let ax = Array.init n (fun i -> gSeen[keys[i]])
                        Ok(ax, max 2 distinctKeys.Length)
                    | HiveAxisRule.Custom fn ->
                        let ax = Array.zeroCreate<int> n
                        let mutable maxA = 1
                        let mutable invalid = false
                        for i = 0 to n - 1 do
                            let a = fn eff.Nodes[i] totDeg[i]
                            if a < 0 || a >= 64 then invalid <- true
                            else
                                ax[i] <- a
                                if a > maxA then maxA <- a
                        if invalid then
                            Error [ InvalidObservation "HivePlot Custom axis rule must return an axis index in [0, 63]." ]
                        else
                            Ok(ax, max 2 (maxA + 1))

                match axisRes with
                | Error errs -> Error errs
                | Ok(axisOf, axisCount) ->
                    let buckets = Array.init axisCount (fun _ -> ResizeArray<int>())
                    for i = 0 to n - 1 do buckets[axisOf[i]].Add i

                    let mutable maxDiag = 0.
                    for i = 0 to n - 1 do
                        let d = Double.Hypot(eff.Nodes[i].Size.Width, eff.Nodes[i].Size.Height)
                        if d > maxDiag then maxDiag <- d

                    let cx0 = Bounds.x config.Bounds + Bounds.width config.Bounds * 0.5
                    let cy0 = Bounds.y config.Bounds + Bounds.height config.Bounds * 0.5
                    let rInner = max 12. config.InnerRadius
                    let viewportOuter =
                        if Bounds.width config.Bounds > 1e-3 && Bounds.height config.Bounds > 1e-3 then
                            max (rInner + 48.) (min (Bounds.width config.Bounds) (Bounds.height config.Bounds) * 0.5 - maxDiag * 0.5 - 10.)
                        else
                            rInner + 180.

                    let radiusOf = Array.zeroCreate<float> n
                    let angleOf = Array.init axisCount (fun a -> -Math.PI * 0.5 + 2. * Math.PI * float a / float axisCount)
                    let splitDelta = if config.SplitAxes then min (Math.PI / 10.) (Math.PI / float (4 * axisCount)) else 0.

                    for a = 0 to axisCount - 1 do
                        let members = buckets[a].ToArray()
                        if members.Length > 0 then
                            // Sort radially outward by (totalDegree, Mass, ordinal index)
                            Array.sortInPlaceWith (fun u v ->
                                let c = compare totDeg[u] totDeg[v]
                                if c <> 0 then c
                                else
                                    let cm = compare eff.Nodes[u].Mass eff.Nodes[v].Mass
                                    if cm <> 0 then cm else compare u v) members

                            if members.Length = 1 then
                                radiusOf[members[0]] <- 0.5 * (rInner + viewportOuter)
                            else
                                let reqSpan = float (members.Length - 1) * (maxDiag * 0.65 + 8.)
                                let rOuter = max viewportOuter (min (viewportOuter * 1.6) (rInner + reqSpan))
                                for k = 0 to members.Length - 1 do
                                    let t = float k / float (members.Length - 1)
                                    radiusOf[members[k]] <- rInner + t * (rOuter - rInner)

                    let posNodes =
                        Array.init n (fun ni ->
                            let nd = eff.Nodes[ni]
                            let th = angleOf[axisOf[ni]]
                            let r = radiusOf[ni]
                            let rawC = Point.create (cx0 + r * cos th) (cy0 + r * sin th)
                            let c = match nd.Pinned with Some p -> p | None -> rawC
                            let b = Bounds.create (c.X - nd.Size.Width * 0.5) (c.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = b
                              Center = c
                              ParentGroup = nd.ParentGroup
                              Layer = Some axisOf[ni]
                              Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                    // Route smooth polar Bézier curves between axes (and split-axis ribbons for intra-axis links)
                    let m = eff.Associations.Length
                    let outAssocs = Array.zeroCreate<RoutedAssociation> m
                    for e = 0 to m - 1 do
                        SimdMath.check token e
                        let a = eff.Associations[e]
                        let ui = eff.ReversedIdMap[a.Source.NodeId]
                        let vi = eff.ReversedIdMap[a.Target.NodeId]
                        let uNd = posNodes[ui]
                        let vNd = posNodes[vi]
                        if ui = vi then
                            let b = uNd.Bounds
                            let p0 = Point.create (Bounds.x b + Bounds.width b) (uNd.Center.Y - 6.)
                            let p3 = Point.create (Bounds.x b + Bounds.width b) (uNd.Center.Y + 6.)
                            let c1 = Point.create (p0.X + 22.) (p0.Y - 16.)
                            let c2 = Point.create (p3.X + 22.) (p3.Y + 16.)
                            let mid = Point.create (p0.X + 16.) uNd.Center.Y
                            let wps = [| p0; mid; p3 |]
                            let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a wps
                            outAssocs[e] <-
                                { Id = a.Id
                                  Source = a.Source
                                  Target = a.Target
                                  Kind = a.Kind
                                  Waypoints = Array.toList wps
                                  Path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                                  ReversedInCycle = false
                                  SourceLabel = sLbl
                                  CenterLabel = mLbl
                                  TargetLabel = tLbl }
                        else
                            let axU = axisOf[ui]
                            let axV = axisOf[vi]
                            let ru = radiusOf[ui]
                            let rv = radiusOf[vi]

                            if axU = axV then
                                // Intra-axis association: route as an outer arc/ribbon bowed to +splitDelta (or clockwise side)
                                let baseTh = angleOf[axU]
                                let bowAngle =
                                    if config.SplitAxes then splitDelta * 2.2
                                    else min (Math.PI / float (axisCount * 2)) 0.35
                                let signDir = if ui < vi then 1. else -1.
                                let th1 = baseTh + signDir * (if config.SplitAxes then splitDelta else 0.04)
                                let th2 = baseTh + signDir * (if config.SplitAxes then -splitDelta else 0.04)
                                let rMid = 0.5 * (ru + rv) + 0.32 * abs (rv - ru) + 18.
                                let thMid = baseTh + signDir * bowAngle
                                let ctrlAim = Point.create (cx0 + rMid * cos thMid) (cy0 + rMid * sin thMid)
                                let p0 = GraphKernels.clipRectBoundary uNd.Bounds (Point.create (cx0 + ru * cos th1 + cos thMid * 12.) (cy0 + ru * sin th1 + sin thMid * 12.))
                                let p3 = GraphKernels.clipRectBoundary vNd.Bounds (Point.create (cx0 + rv * cos th2 + cos thMid * 12.) (cy0 + rv * sin th2 + sin thMid * 12.))
                                let c1 = Point.create (0.35 * p0.X + 0.65 * ctrlAim.X) (0.35 * p0.Y + 0.65 * ctrlAim.Y)
                                let c2 = Point.create (0.35 * p3.X + 0.65 * ctrlAim.X) (0.35 * p3.Y + 0.65 * ctrlAim.Y)
                                let mid = Point.create (0.125 * p0.X + 0.375 * c1.X + 0.375 * c2.X + 0.125 * p3.X) (0.125 * p0.Y + 0.375 * c1.Y + 0.375 * c2.Y + 0.125 * p3.Y)
                                let wps = [| p0; mid; p3 |]
                                let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a wps
                                outAssocs[e] <-
                                    { Id = a.Id
                                      Source = a.Source
                                      Target = a.Target
                                      Kind = a.Kind
                                      Waypoints = Array.toList wps
                                      Path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                                      ReversedInCycle = false
                                      SourceLabel = sLbl
                                      CenterLabel = mLbl
                                      TargetLabel = tLbl }
                            else
                                // Inter-axis association: smooth polar cubic curve through shortest angular sector
                                let mutable dTh = angleOf[axV] - angleOf[axU]
                                while dTh > Math.PI do dTh <- dTh - 2. * Math.PI
                                while dTh < -Math.PI do dTh <- dTh + 2. * Math.PI
                                let dirSign = if dTh >= 0. then 1. else -1.
                                let thStart = angleOf[axU] + dirSign * splitDelta
                                let thEnd = angleOf[axV] - dirSign * splitDelta
                                let spanTh = thEnd - thStart
                                let thC1 = thStart + spanTh * 0.33
                                let thC2 = thStart + spanTh * 0.67
                                let rC1 = 0.72 * ru + 0.28 * rv
                                let rC2 = 0.28 * ru + 0.72 * rv
                                let c1 = Point.create (cx0 + rC1 * cos thC1) (cy0 + rC1 * sin thC1)
                                let c2 = Point.create (cx0 + rC2 * cos thC2) (cy0 + rC2 * sin thC2)
                                let p0 = GraphKernels.clipRectBoundary uNd.Bounds c1
                                let p3 = GraphKernels.clipRectBoundary vNd.Bounds c2
                                let mid =
                                    Point.create
                                        (0.125 * p0.X + 0.375 * c1.X + 0.375 * c2.X + 0.125 * p3.X)
                                        (0.125 * p0.Y + 0.375 * c1.Y + 0.375 * c2.Y + 0.125 * p3.Y)
                                let wps = [| p0; mid; p3 |]
                                let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a wps
                                outAssocs[e] <-
                                    { Id = a.Id
                                      Source = a.Source
                                      Target = a.Target
                                      Kind = a.Kind
                                      Waypoints = Array.toList wps
                                      Path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                                      ReversedInCycle = false
                                      SourceLabel = sLbl
                                      CenterLabel = mLbl
                                      TargetLabel = tLbl }

                    let groups = GraphKernels.computeGroupBounds eff (posNodes |> Array.map (fun pn -> pn.Bounds))
                    let routed = Array.toList outAssocs
                    let nList = Array.toList posNodes
                    Ok
                        { Bounds = GraphKernels.computeDiagramBounds config.Bounds nList groups routed
                          Nodes = nList
                          Groups = groups
                          Associations = routed }

    /// <summary>Lays out <c>graph</c> as a Krzywinski et al. (2012) Hive Plot without cancellation.</summary>
    let layout config graph =
        layoutWithCancellation System.Threading.CancellationToken.None config graph

/// <summary>A horizontal node strand row in a Longabaugh (2012) BioFabric layout.</summary>
type BioFabricRow =
    { /// <summary>Node identifier.</summary>
      NodeId: string
      /// <summary>Vertical Y coordinate of the horizontal node strand.</summary>
      Y: float
      /// <summary>Leftmost X coordinate spanned by incident edge columns on this row.</summary>
      XStart: float
      /// <summary>Rightmost X coordinate spanned by incident edge columns on this row.</summary>
      XEnd: float
      /// <summary>Total number of incident associations attached to this node row.</summary>
      Degree: int }

/// <summary>A unique vertical association column in a Longabaugh (2012) BioFabric layout.</summary>
type BioFabricEdgeColumn =
    { /// <summary>Association identifier.</summary>
      AssociationId: string
      /// <summary>Source node identifier.</summary>
      SourceId: string
      /// <summary>Target node identifier.</summary>
      TargetId: string
      /// <summary>Unique horizontal X coordinate assigned to this vertical edge column.</summary>
      X: float
      /// <summary>Upper endpoint Y coordinate (<c>min(Y_source, Y_target)</c>).</summary>
      YTop: float
      /// <summary>Lower endpoint Y coordinate (<c>max(Y_source, Y_target)</c>).</summary>
      YBottom: float }

/// <summary>Complete Longabaugh (2012) BioFabric network matrix/strand output with structured rows, columns and drawable 2D diagram.</summary>
type BioFabricLayout =
    { /// <summary>Enclosing layout bounds.</summary>
      Bounds: Bounds
      /// <summary>Horizontal node rows ordered top-to-bottom in BFS/degree wedge order.</summary>
      Rows: BioFabricRow list
      /// <summary>Vertical association columns ordered left-to-right by <c>(minRow, maxRow)</c> shadow sorting.</summary>
      Columns: BioFabricEdgeColumn list
      /// <summary>Ready-to-render <c>DiagramLayout2D</c> representation of the BioFabric layout.</summary>
      Diagram: DiagramLayout2D }

/// <summary>Longabaugh (2012) BioFabric layout ("combing the hairball" with horizontal node strands and unique vertical edge columns).</summary>
[<RequireQualifiedAccess>]
module BioFabric =
    /// <summary>Computes a Longabaugh (2012) BioFabric layout over <c>graph</c> inside <c>bounds</c> with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (bounds: Bounds)
        (graph: DiagramGraph)
        : Result<BioFabricLayout, LayoutError list> =
        token.ThrowIfCancellationRequested()
        let eff = DiagramGraph.effective graph
        let n = eff.Nodes.Length
        let m = eff.Associations.Length
        if n = 0 then
            let emptyDiag = { Bounds = bounds; Nodes = []; Groups = GraphKernels.computeGroupBounds eff [||]; Associations = [] }
            Ok { Bounds = bounds; Rows = []; Columns = []; Diagram = emptyDiag }
        else
            let csr = GraphKernels.buildCsr eff
            // Incident association count per node (including parallel associations)
            let assocDeg = Array.zeroCreate<int> n
            for e = 0 to m - 1 do
                let u = csr.Src[e]
                let v = csr.Dst[e]
                assocDeg[u] <- assocDeg[u] + 1
                if v <> u then assocDeg[v] <- assocDeg[v] + 1

            let undirDeg = Array.init n (fun u -> csr.UndirOffsets[u + 1] - csr.UndirOffsets[u])

            // 1. BFS-by-degree node row ordering (Longabaugh 2012 default wedge order)
            let seeds = Array.init n (fun i -> i)
            Array.sortInPlaceWith (fun a b ->
                let c = compare undirDeg[b] undirDeg[a]
                if c <> 0 then c
                else
                    let c2 = compare assocDeg[b] assocDeg[a]
                    if c2 <> 0 then c2 else compare a b) seeds

            let visited = Array.zeroCreate<bool> n
            let rowNodes = Array.zeroCreate<int> n
            let rowOfNode = Array.zeroCreate<int> n
            let bfsQueue = Array.zeroCreate<int> n
            let nbrBuf = ResizeArray<int>(32)
            let mutable writeRow = 0

            for si = 0 to n - 1 do
                let root = seeds[si]
                if not visited[root] then
                    visited[root] <- true
                    let mutable qHead = 0
                    let mutable qTail = 0
                    bfsQueue[qTail] <- root
                    qTail <- qTail + 1
                    while qHead < qTail do
                        SimdMath.check token qHead
                        let u = bfsQueue[qHead]
                        qHead <- qHead + 1
                        rowNodes[writeRow] <- u
                        rowOfNode[u] <- writeRow
                        writeRow <- writeRow + 1

                        nbrBuf.Clear()
                        for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                            let v = csr.UndirTargets[idx]
                            if not visited[v] then
                                visited[v] <- true
                                nbrBuf.Add v
                        if nbrBuf.Count > 1 then
                            let sub = nbrBuf.ToArray()
                            Array.sortInPlaceWith (fun a b ->
                                let c = compare undirDeg[b] undirDeg[a]
                                if c <> 0 then c else compare a b) sub
                            for k = 0 to sub.Length - 1 do
                                bfsQueue[qTail] <- sub[k]
                                qTail <- qTail + 1
                        elif nbrBuf.Count = 1 then
                            bfsQueue[qTail] <- nbrBuf[0]
                            qTail <- qTail + 1

            // 2. Shadow-sort associations into unique X columns by (minRow, maxRow, ordinal Id)
            let edgeOrder = Array.init m (fun e -> e)
            Array.sortInPlaceWith (fun ea eb ->
                let ua, va = csr.Src[ea], csr.Dst[ea]
                let ub, vb = csr.Src[eb], csr.Dst[eb]
                let minA, maxA = min rowOfNode[ua] rowOfNode[va], max rowOfNode[ua] rowOfNode[va]
                let minB, maxB = min rowOfNode[ub] rowOfNode[vb], max rowOfNode[ub] rowOfNode[vb]
                let c1 = compare minA minB
                if c1 <> 0 then c1
                else
                    let c2 = compare maxA maxB
                    if c2 <> 0 then c2
                    else StringComparer.Ordinal.Compare(eff.Associations[ea].Id, eff.Associations[eb].Id)) edgeOrder

            let bx = Bounds.x bounds
            let by = Bounds.y bounds
            let bw = Bounds.width bounds
            let bh = Bounds.height bounds

            let mutable maxNodeW = 0.
            let mutable maxNodeH = 0.
            for i = 0 to n - 1 do
                if eff.Nodes[i].Size.Width > maxNodeW then maxNodeW <- eff.Nodes[i].Size.Width
                if eff.Nodes[i].Size.Height > maxNodeH then maxNodeH <- eff.Nodes[i].Size.Height

            let headerCenterX = bx + maxNodeW * 0.5 + 8.
            let firstColX = bx + maxNodeW + 32.
            let rowStep =
                if n <= 1 then 0.
                else
                    let targetStep = if bh > maxNodeH then (bh - maxNodeH) / float (n - 1) else maxNodeH + 10.
                    max (maxNodeH + 8.) targetStep
            let startY = by + maxNodeH * 0.5 + 4.

            let colStep =
                if m <= 1 then 0.
                else
                    let availW = bw - (firstColX - bx) - 16.
                    let rawStep = if availW > 8. then availW / float (m - 1) else 16.
                    max 12. rawStep

            let rowY = Array.init n (fun r -> startY + float r * rowStep)
            let colXByEdge = Array.zeroCreate<float> m
            let minColForRow = Array.create n infinity
            let maxColForRow = Array.create n -infinity

            let columns = Array.zeroCreate<BioFabricEdgeColumn> m
            for colIdx = 0 to m - 1 do
                let e = edgeOrder[colIdx]
                let a = eff.Associations[e]
                let u, v = csr.Src[e], csr.Dst[e]
                let ru, rv = rowOfNode[u], rowOfNode[v]
                let cx = firstColX + float colIdx * colStep
                colXByEdge[e] <- cx
                if cx < minColForRow[ru] then minColForRow[ru] <- cx
                if cx > maxColForRow[ru] then maxColForRow[ru] <- cx
                if cx < minColForRow[rv] then minColForRow[rv] <- cx
                if cx > maxColForRow[rv] then maxColForRow[rv] <- cx
                let yu, yv = rowY[ru], rowY[rv]
                columns[colIdx] <-
                    { AssociationId = a.Id
                      SourceId = a.Source.NodeId
                      TargetId = a.Target.NodeId
                      X = cx
                      YTop = min yu yv
                      YBottom = max yu yv }

            let rows =
                Array.init n (fun r ->
                    let ni = rowNodes[r]
                    let y = rowY[r]
                    let defaultX = headerCenterX + eff.Nodes[ni].Size.Width * 0.5
                    let x0 = if Double.IsFinite minColForRow[r] then minColForRow[r] else defaultX
                    let x1 = if Double.IsFinite maxColForRow[r] then maxColForRow[r] else defaultX
                    { NodeId = eff.Nodes[ni].Id
                      Y = y
                      XStart = x0
                      XEnd = x1
                      Degree = assocDeg[ni] })

            let posNodes =
                Array.init n (fun ni ->
                    let nd = eff.Nodes[ni]
                    let r = rowOfNode[ni]
                    let c = match nd.Pinned with Some p -> p | None -> Point.create headerCenterX rowY[r]
                    let b = Bounds.create (c.X - nd.Size.Width * 0.5) (c.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                    { Id = nd.Id
                      Kind = nd.Kind
                      Bounds = b
                      Center = c
                      ParentGroup = nd.ParentGroup
                      Layer = Some r
                      Ports = GraphKernels.resolveNodePorts nd b Map.empty })

            let routedAssocs =
                Array.init m (fun e ->
                    let a = eff.Associations[e]
                    let ui, vi = csr.Src[e], csr.Dst[e]
                    let uNd, vNd = posNodes[ui], posNodes[vi]
                    let cx = colXByEdge[e]
                    let pStart = Point.create (Bounds.x uNd.Bounds + Bounds.width uNd.Bounds) uNd.Center.Y
                    let pEnd = Point.create (Bounds.x vNd.Bounds + Bounds.width vNd.Bounds) vNd.Center.Y
                    let wps =
                        if ui = vi then
                            let topY = uNd.Center.Y - 8.
                            [| pStart; Point.create cx pStart.Y; Point.create cx topY; pEnd |]
                        else
                            [| pStart; Point.create cx pStart.Y; Point.create cx pEnd.Y; pEnd |]
                    let cmds = [ yield MoveTo wps[0]; for k = 1 to wps.Length - 1 do yield LineTo wps[k] ]
                    let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a wps
                    { Id = a.Id
                      Source = a.Source
                      Target = a.Target
                      Kind = a.Kind
                      Waypoints = Array.toList wps
                      Path = Path.ofCommands cmds
                      ReversedInCycle = rowOfNode[ui] > rowOfNode[vi]
                      SourceLabel = sLbl
                      CenterLabel = mLbl
                      TargetLabel = tLbl })

            let groups = GraphKernels.computeGroupBounds eff (posNodes |> Array.map (fun pn -> pn.Bounds))
            let nList = Array.toList posNodes
            let aList = Array.toList routedAssocs
            let diagBounds = GraphKernels.computeDiagramBounds bounds nList groups aList
            let diagram =
                { Bounds = diagBounds
                  Nodes = nList
                  Groups = groups
                  Associations = aList }

            Ok
                { Bounds = diagBounds
                  Rows = Array.toList rows
                  Columns = Array.toList columns
                  Diagram = diagram }

    /// <summary>Computes a Longabaugh (2012) BioFabric layout over <c>graph</c> without cancellation.</summary>
    let layout bounds graph =
        layoutWithCancellation System.Threading.CancellationToken.None bounds graph

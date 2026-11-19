namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Objective function optimized during Leiden community detection.</summary>
[<RequireQualifiedAccess>]
type CommunityQuality =
    /// <summary>Newman-Girvan / Reichardt-Bornholdt modularity with resolution scaling.</summary>
    | Modularity
    /// <summary>Traag-Van Dooren-Nesterov Constant Potts Model (resolution-limit-free density objective).</summary>
    | ConstantPotts

/// <summary>Settings for Traag, Waltman &amp; van Eck (2019) three-phase Leiden community detection.</summary>
type LeidenConfig =
    { /// <summary>Community quality objective (<c>Modularity</c> or <c>ConstantPotts</c>).</summary>
      Quality: CommunityQuality
      /// <summary>Nonnegative resolution parameter gamma (higher values yield more, smaller communities).</summary>
      Resolution: float
      /// <summary>Maximum coarsening / aggregation hierarchy levels.</summary>
      MaxLevels: int
      /// <summary>Maximum local-moving sweeps per level.</summary>
      MaxIterationsPerLevel: int
      /// <summary>Deterministic PRNG seed used for vertex visitation order and probabilistic refinement merges.</summary>
      Seed: uint64 }

/// <summary>Default configuration values for <c>LeidenConfig</c>.</summary>
[<RequireQualifiedAccess>]
module LeidenConfig =
    /// <summary>Default Leiden settings: <c>Modularity</c>, resolution 1.0, 8 max levels, 20 max iterations per level and seed 42.</summary>
    let defaultValue =
        { Quality = CommunityQuality.Modularity
          Resolution = 1.0
          MaxLevels = 8
          MaxIterationsPerLevel = 20
          Seed = 42UL }

/// <summary>Discovered community partition with multi-level coarsening history and final quality score.</summary>
type CommunityPartition =
    { /// <summary>Maps every visible node ID to its zero-based community index <c>0 .. CommunityCount - 1</c>.</summary>
      Communities: Map<string, int>
      /// <summary>Maps each community index to its sorted member node IDs.</summary>
      CommunityMembers: Map<int, string list>
      /// <summary>Total number of non-empty connected communities.</summary>
      CommunityCount: int
      /// <summary>Evaluated modularity or Constant Potts Model quality score of the final partition.</summary>
      QualityScore: float
      /// <summary>Hierarchical partitions from finest level to coarsest final level.</summary>
      Levels: Map<string, int> list }

/// <summary>Traag, Waltman &amp; van Eck (2019) Leiden community detection guaranteeing connected communities.</summary>
[<RequireQualifiedAccess>]
module Leiden =
    /// <summary>Internal weighted undirected level graph with self-weights and generalized vertex masses.</summary>
    type private LevelGraph =
        { NodeCount: int
          Offsets: int[]
          Targets: int[]
          Weights: float[]
          SelfEdgeWeight: float[]
          NodeDegree: float[]
          NodeSize: float[]
          VertexMass: float[] }

    let private shuffleInPlace (arr: int[]) (rng: RandomState) : RandomState =
        let mutable st = rng
        let n = arr.Length
        for i = n - 1 downto 1 do
            let struct(bits, nextSt) = RandomState.nextUInt64 st
            st <- nextSt
            let j = int (bits % uint64 (i + 1))
            let tmp = arr[i]
            arr[i] <- arr[j]
            arr[j] <- tmp
        st

    let private buildInitialLevelGraph (csr: CsrGraph) (quality: CommunityQuality) : LevelGraph * float =
        let n = csr.NodeCount
        let m = csr.EdgeCount
        let selfEdge = Array.zeroCreate<float> n
        let rawDeg = Array.zeroCreate<int> n
        let mutable totalEdgeWeight = 0.

        for e = 0 to m - 1 do
            let u = csr.Src[e]
            let v = csr.Dst[e]
            let w = csr.Weight[e]
            totalEdgeWeight <- totalEdgeWeight + w
            if u = v then
                selfEdge[u] <- selfEdge[u] + w
            else
                rawDeg[u] <- rawDeg[u] + 1
                rawDeg[v] <- rawDeg[v] + 1

        let rawOffsets = Array.zeroCreate<int> (n + 1)
        for i = 0 to n - 1 do
            rawOffsets[i + 1] <- rawOffsets[i] + rawDeg[i]
        let totalRaw = rawOffsets[n]
        let rawTargets = Array.zeroCreate<int> totalRaw
        let rawWeights = Array.zeroCreate<float> totalRaw
        let cursor = Array.copy rawOffsets

        for e = 0 to m - 1 do
            let u = csr.Src[e]
            let v = csr.Dst[e]
            if u <> v then
                let w = csr.Weight[e]
                let cu = cursor[u]
                rawTargets[cu] <- v
                rawWeights[cu] <- w
                cursor[u] <- cu + 1
                let cv = cursor[v]
                rawTargets[cv] <- u
                rawWeights[cv] <- w
                cursor[v] <- cv + 1

        // Sort and sum parallel undirected edges per vertex in flat arrays.
        let offsets = Array.zeroCreate<int> (n + 1)
        let nodeDeg = Array.zeroCreate<float> n
        let mutable writePos = 0
        for u = 0 to n - 1 do
            offsets[u] <- writePos
            let start = rawOffsets[u]
            let stop = rawOffsets[u + 1]
            let cnt = stop - start
            let mutable degSum = 2. * selfEdge[u]
            if cnt = 1 then
                let t = rawTargets[start]
                let w = rawWeights[start]
                rawTargets[writePos] <- t
                rawWeights[writePos] <- w
                degSum <- degSum + w
                writePos <- writePos + 1
            elif cnt > 1 then
                // Shell sort on slice [start .. stop - 1] keeping targets & weights synchronized under both .NET and Fable.
                let mutable gap = cnt / 2
                while gap > 0 do
                    for a = start + gap to stop - 1 do
                        let kt = rawTargets[a]
                        let kw = rawWeights[a]
                        let mutable b = a
                        while b >= start + gap && rawTargets[b - gap] > kt do
                            rawTargets[b] <- rawTargets[b - gap]
                            rawWeights[b] <- rawWeights[b - gap]
                            b <- b - gap
                        rawTargets[b] <- kt
                        rawWeights[b] <- kw
                    gap <- gap / 2

                let mutable prevT = rawTargets[start]
                let mutable sumW = rawWeights[start]
                for k = start + 1 to stop - 1 do
                    let curT = rawTargets[k]
                    let curW = rawWeights[k]
                    if curT = prevT then
                        sumW <- sumW + curW
                    else
                        rawTargets[writePos] <- prevT
                        rawWeights[writePos] <- sumW
                        degSum <- degSum + sumW
                        writePos <- writePos + 1
                        prevT <- curT
                        sumW <- curW
                rawTargets[writePos] <- prevT
                rawWeights[writePos] <- sumW
                degSum <- degSum + sumW
                writePos <- writePos + 1
            nodeDeg[u] <- degSum
        offsets[n] <- writePos

        let targets = if writePos = totalRaw then rawTargets else Array.sub rawTargets 0 writePos
        let weights = if writePos = totalRaw then rawWeights else Array.sub rawWeights 0 writePos
        let nodeSize = Array.create n 1.0
        let vMass =
            match quality with
            | CommunityQuality.Modularity -> nodeDeg
            | CommunityQuality.ConstantPotts -> nodeSize

        { NodeCount = n
          Offsets = offsets
          Targets = targets
          Weights = weights
          SelfEdgeWeight = selfEdge
          NodeDegree = nodeDeg
          NodeSize = nodeSize
          VertexMass = vMass },
        totalEdgeWeight

    /// <summary>Phase 1 of Leiden: Fast Local Moving using a FIFO queue of vertices whose neighborhood communities changed.</summary>
    let private moveNodesFast
        (token: System.Threading.CancellationToken)
        (g: LevelGraph)
        (comm: int[])
        (gammaEff: float)
        (maxIters: int)
        (rng: RandomState)
        : bool * RandomState =
        let n = g.NodeCount
        if n <= 1 then
            false, rng
        else
            let maxCommCap = n * 2 + 4
            let commMass = Array.zeroCreate<float> maxCommCap
            let commSize = Array.zeroCreate<int> maxCommCap
            let mutable nextFreshComm = 0
            for u = 0 to n - 1 do
                let c = comm[u]
                if c >= nextFreshComm then nextFreshComm <- c + 1
                commMass[c] <- commMass[c] + g.VertexMass[u]
                commSize[c] <- commSize[c] + 1

            let freeComms = Stack<int>()
            let allocEmptyComm () =
                if freeComms.Count > 0 then freeComms.Pop()
                else
                    let id = nextFreshComm
                    nextFreshComm <- nextFreshComm + 1
                    id

            let order = Array.init n (fun i -> i)
            let nextRng = shuffleInPlace order rng

            let ringCap = n + 1
            let queue = Array.zeroCreate<int> ringCap
            let inQueue = Array.create n true
            let mutable qHead = 0
            let mutable qTail = n
            for i = 0 to n - 1 do queue[i] <- order[i]

            let inline enqueue u =
                if not inQueue[u] then
                    inQueue[u] <- true
                    queue[qTail] <- u
                    qTail <- if qTail + 1 = ringCap then 0 else qTail + 1

            let inline dequeue () =
                let u = queue[qHead]
                qHead <- if qHead + 1 = ringCap then 0 else qHead + 1
                inQueue[u] <- false
                u

            let touchedComms = ResizeArray<int>(32)
            let commEdgeW = Array.zeroCreate<float> maxCommCap
            let commSeen = Array.zeroCreate<bool> maxCommCap

            let mutable anyMoved = false
            let maxPops = max n (n * maxIters)
            let mutable pops = 0

            while qHead <> qTail && pops < maxPops do
                SimdMath.check token pops
                pops <- pops + 1
                let u = dequeue ()
                let oldC = comm[u]
                let mu = g.VertexMass[u]

                // Temporarily remove u from oldC
                commMass[oldC] <- max 0. (commMass[oldC] - mu)
                commSize[oldC] <- commSize[oldC] - 1

                touchedComms.Clear()
                for idx = g.Offsets[u] to g.Offsets[u + 1] - 1 do
                    let v = g.Targets[idx]
                    let cv = comm[v]
                    let w = g.Weights[idx]
                    if not commSeen[cv] then
                        commSeen[cv] <- true
                        commEdgeW[cv] <- w
                        touchedComms.Add cv
                    else
                        commEdgeW[cv] <- commEdgeW[cv] + w

                // Baseline gain of staying in oldC relative to an isolated singleton community (0.0).
                let wOld = if commSeen[oldC] then commEdgeW[oldC] else 0.
                let gainStay = wOld - gammaEff * mu * commMass[oldC]

                // Candidate 0: isolated empty community (gain = 0.0), only meaningful if commSize[oldC] > 0.
                let mutable bestC = if commSize[oldC] = 0 || gainStay >= -1e-12 then oldC else -1
                let mutable bestGain = if bestC = oldC then gainStay else 0.

                for k = 0 to touchedComms.Count - 1 do
                    let cand = touchedComms[k]
                    if cand <> oldC then
                        let gCand = commEdgeW[cand] - gammaEff * mu * commMass[cand]
                        if gCand > bestGain + 1e-12 || (abs (gCand - bestGain) <= 1e-12 && bestC >= 0 && cand < bestC) then
                            bestGain <- gCand
                            bestC <- cand

                for k = 0 to touchedComms.Count - 1 do
                    let cand = touchedComms[k]
                    commSeen[cand] <- false
                    commEdgeW[cand] <- 0.

                let targetC =
                    if bestC >= 0 then bestC
                    else allocEmptyComm ()

                if commSize[oldC] = 0 && targetC <> oldC then
                    commMass[oldC] <- 0.
                    freeComms.Push oldC

                comm[u] <- targetC
                commMass[targetC] <- commMass[targetC] + mu
                commSize[targetC] <- commSize[targetC] + 1

                if targetC <> oldC then
                    anyMoved <- true
                    for idx = g.Offsets[u] to g.Offsets[u + 1] - 1 do
                        let v = g.Targets[idx]
                        if comm[v] <> targetC then
                            enqueue v

            anyMoved, nextRng

    /// <summary>Canonically reindexes a partition array into dense <c>0 .. k - 1</c> sorted by earliest vertex index.</summary>
    let private reindexPartition (comm: int[]) : int =
        let n = comm.Length
        if n = 0 then 0
        else
            let map = Dictionary<int, int>()
            let mutable nextId = 0
            for i = 0 to n - 1 do
                let raw = comm[i]
                match map.TryGetValue raw with
                | true, cid -> comm[i] <- cid
                | _ ->
                    map.Add(raw, nextId)
                    comm[i] <- nextId
                    nextId <- nextId + 1
            nextId

    /// <summary>Phase 2 of Leiden: Connected Refinement within each block of <c>coarseComm</c>, guaranteeing connected sub-communities.</summary>
    let private refinePartition
        (token: System.Threading.CancellationToken)
        (g: LevelGraph)
        (coarseComm: int[])
        (coarseCount: int)
        (gammaEff: float)
        (rng: RandomState)
        : int[] * int * RandomState =
        let n = g.NodeCount
        let refined = Array.init n (fun i -> i)
        if n <= 1 || coarseCount >= n then
            let cnt = reindexPartition refined
            refined, cnt, rng
        else
            // Group vertices by coarseComm in flat CSR buckets.
            let cCounts = Array.zeroCreate<int> coarseCount
            for u = 0 to n - 1 do
                let c = coarseComm[u]
                cCounts[c] <- cCounts[c] + 1
            let cOffsets = Array.zeroCreate<int> (coarseCount + 1)
            for c = 0 to coarseCount - 1 do
                cOffsets[c + 1] <- cOffsets[c] + cCounts[c]
            let cMembers = Array.zeroCreate<int> n
            let cCursor = Array.copy cOffsets
            for u = 0 to n - 1 do
                let c = coarseComm[u]
                let pos = cCursor[c]
                cMembers[pos] <- u
                cCursor[c] <- pos + 1

            let subMass = Array.copy g.VertexMass
            let subSize = Array.create n 1
            let subCutToS = Array.zeroCreate<float> n
            let edgeToS = Array.zeroCreate<float> n

            let candSub = ResizeArray<int>(32)
            let candGain = ResizeArray<float>(32)
            let subEdgeW = Array.zeroCreate<float> n
            let subSeen = Array.zeroCreate<bool> n

            let mutable st = rng
            let thetaInv = 100.0 // 1 / theta with theta = 0.01 per Traag et al. (2019)

            for c = 0 to coarseCount - 1 do
                token.ThrowIfCancellationRequested()
                let start = cOffsets[c]
                let stop = cOffsets[c + 1]
                let sz = stop - start
                if sz > 1 then
                    let mutable massS = 0.
                    for idx = start to stop - 1 do
                        let u = cMembers[idx]
                        massS <- massS + g.VertexMass[u]
                        let mutable wInS = 0.
                        for eIdx = g.Offsets[u] to g.Offsets[u + 1] - 1 do
                            let v = g.Targets[eIdx]
                            if coarseComm[v] = c then
                                wInS <- wInS + g.Weights[eIdx]
                        edgeToS[u] <- wInS
                        subCutToS[u] <- wInS

                    // Shuffle vertices within S for randomized local merging
                    let localOrder = Array.sub cMembers start sz
                    st <- shuffleInPlace localOrder st

                    for k = 0 to sz - 1 do
                        let u = localOrder[k]
                        let ru = refined[u]
                        let mu = g.VertexMass[u]
                        // Only singleton refined sub-communities that are gamma-well-connected to S \ {u} may initiate a merge!
                        if subSize[ru] = 1 && edgeToS[u] >= gammaEff * mu * (massS - mu) - 1e-12 then
                            candSub.Clear()
                            candGain.Clear()

                            // Only examine refined sub-communities reachable via an edge inside S -> GUARANTEES connectedness!
                            for eIdx = g.Offsets[u] to g.Offsets[u + 1] - 1 do
                                let v = g.Targets[eIdx]
                                if coarseComm[v] = c then
                                    let rv = refined[v]
                                    if rv <> ru then
                                        let w = g.Weights[eIdx]
                                        if not subSeen[rv] then
                                            subSeen[rv] <- true
                                            subEdgeW[rv] <- w
                                            candSub.Add rv
                                        else
                                            subEdgeW[rv] <- subEdgeW[rv] + w

                            let mutable maxGain = Double.NegativeInfinity
                            let eligibleSub = ResizeArray<int>(candSub.Count)
                            for ci = 0 to candSub.Count - 1 do
                                let tSub = candSub[ci]
                                let mt = subMass[tSub]
                                // Candidate target sub-community T must also be well-connected to S \ T
                                if subCutToS[tSub] >= gammaEff * mt * (massS - mt) - 1e-12 then
                                    let gain = subEdgeW[tSub] - gammaEff * mu * mt
                                    if gain > 1e-12 then
                                        eligibleSub.Add tSub
                                        candGain.Add gain
                                        if gain > maxGain then maxGain <- gain

                            for ci = 0 to candSub.Count - 1 do
                                let tSub = candSub[ci]
                                subSeen[tSub] <- false

                            if eligibleSub.Count > 0 then
                                let chosenT =
                                    if eligibleSub.Count = 1 then
                                        eligibleSub[0]
                                    else
                                        let weights = Array.zeroCreate<float> eligibleSub.Count
                                        let mutable wTotal = 0.
                                        for ei = 0 to eligibleSub.Count - 1 do
                                            // Shift by maxGain so exponent <= 0, completely preventing overflow!
                                            let rel = max -60. ((candGain[ei] - maxGain) * thetaInv)
                                            let p = exp rel
                                            weights[ei] <- p
                                            wTotal <- wTotal + p
                                        let struct(uDraw, nextSt) = RandomState.nextFloat st
                                        st <- nextSt
                                        let targetP = uDraw * wTotal
                                        let mutable accP = 0.
                                        let mutable pick = eligibleSub[eligibleSub.Count - 1]
                                        let mutable found = false
                                        let mutable ei = 0
                                        while ei < eligibleSub.Count && not found do
                                            accP <- accP + weights[ei]
                                            if accP >= targetP then
                                                pick <- eligibleSub[ei]
                                                found <- true
                                            ei <- ei + 1
                                        pick

                                let wUT = subEdgeW[chosenT]
                                // Clear subEdgeW after reading wUT
                                for ci = 0 to candSub.Count - 1 do
                                    subEdgeW[candSub[ci]] <- 0.

                                // Merge singleton {u} into chosenT
                                refined[u] <- chosenT
                                subSize[ru] <- 0
                                subMass[ru] <- 0.
                                subSize[chosenT] <- subSize[chosenT] + 1
                                subMass[chosenT] <- subMass[chosenT] + mu
                                subCutToS[chosenT] <- subCutToS[chosenT] + edgeToS[u] - 2. * wUT
                            else
                                for ci = 0 to candSub.Count - 1 do
                                    subEdgeW[candSub[ci]] <- 0.

            let refinedCount = reindexPartition refined
            refined, refinedCount, st

    /// <summary>Phase 3 of Leiden: Aggregates <c>g</c> over <c>refined</c> while initializing next-level partition from <c>coarseComm</c>.</summary>
    let private aggregateGraph
        (g: LevelGraph)
        (refined: int[])
        (refinedCount: int)
        (coarseComm: int[])
        (quality: CommunityQuality)
        : LevelGraph * int[] =
        let selfEdge = Array.zeroCreate<float> refinedCount
        let nodeDeg = Array.zeroCreate<float> refinedCount
        let nodeSize = Array.zeroCreate<float> refinedCount
        let initCoarse = Array.create refinedCount -1

        for u = 0 to g.NodeCount - 1 do
            let ru = refined[u]
            selfEdge[ru] <- selfEdge[ru] + g.SelfEdgeWeight[u]
            nodeDeg[ru] <- nodeDeg[ru] + g.NodeDegree[u]
            nodeSize[ru] <- nodeSize[ru] + g.NodeSize[u]
            if initCoarse[ru] < 0 then
                initCoarse[ru] <- coarseComm[u]

        // Accumulate undirected super-edges (ru < rv) via packed uint64 keys
        let edgeAcc = Dictionary<uint64, float>()
        for u = 0 to g.NodeCount - 1 do
            let ru = refined[u]
            for idx = g.Offsets[u] to g.Offsets[u + 1] - 1 do
                let v = g.Targets[idx]
                if u < v then
                    let rv = refined[v]
                    let w = g.Weights[idx]
                    if ru = rv then
                        selfEdge[ru] <- selfEdge[ru] + w
                    else
                        let a, b = min ru rv, max ru rv
                        let key = (uint64 a <<< 32) ||| uint64 (uint32 b)
                        match edgeAcc.TryGetValue key with
                        | true, prev -> edgeAcc[key] <- prev + w
                        | _ -> edgeAcc.Add(key, w)

        let degCount = Array.zeroCreate<int> refinedCount
        for kv in edgeAcc do
            let a = int (kv.Key >>> 32)
            let b = int (uint32 kv.Key)
            degCount[a] <- degCount[a] + 1
            degCount[b] <- degCount[b] + 1

        let offsets = Array.zeroCreate<int> (refinedCount + 1)
        for i = 0 to refinedCount - 1 do
            offsets[i + 1] <- offsets[i] + degCount[i]
        let total = offsets[refinedCount]
        let targets = Array.zeroCreate<int> total
        let weights = Array.zeroCreate<float> total
        let cur = Array.copy offsets

        let pairs = Seq.toArray edgeAcc
        Array.sortInPlaceWith (fun (a: KeyValuePair<uint64, float>) (b: KeyValuePair<uint64, float>) -> compare a.Key b.Key) pairs

        for i = 0 to pairs.Length - 1 do
            let key = pairs[i].Key
            let w = pairs[i].Value
            let a = int (key >>> 32)
            let b = int (uint32 key)
            let ca = cur[a]
            targets[ca] <- b
            weights[ca] <- w
            cur[a] <- ca + 1
            let cb = cur[b]
            targets[cb] <- a
            weights[cb] <- w
            cur[b] <- cb + 1

        reindexPartition initCoarse |> ignore
        let vMass =
            match quality with
            | CommunityQuality.Modularity -> nodeDeg
            | CommunityQuality.ConstantPotts -> nodeSize

        { NodeCount = refinedCount
          Offsets = offsets
          Targets = targets
          Weights = weights
          SelfEdgeWeight = selfEdge
          NodeDegree = nodeDeg
          NodeSize = nodeSize
          VertexMass = vMass },
        initCoarse

    /// <summary>Splits any disconnected community components on the base graph, strictly guaranteeing induced connectivity.</summary>
    let private enforceConnectedCommunities (baseG: LevelGraph) (comm: int[]) : int =
        let n = baseG.NodeCount
        if n <= 1 then
            reindexPartition comm
        else
            let visited = Array.zeroCreate<bool> n
            let queue = Array.zeroCreate<int> n
            let newComm = Array.create n -1
            let mutable cid = 0
            for start = 0 to n - 1 do
                if not visited[start] then
                    let targetC = comm[start]
                    visited[start] <- true
                    newComm[start] <- cid
                    let mutable head = 0
                    let mutable tail = 0
                    queue[tail] <- start
                    tail <- tail + 1
                    while head < tail do
                        let u = queue[head]
                        head <- head + 1
                        for idx = baseG.Offsets[u] to baseG.Offsets[u + 1] - 1 do
                            let v = baseG.Targets[idx]
                            if not visited[v] && comm[v] = targetC then
                                visited[v] <- true
                                newComm[v] <- cid
                                queue[tail] <- v
                                tail <- tail + 1
                    cid <- cid + 1
            Array.blit newComm 0 comm 0 n
            reindexPartition comm

    /// <summary>Evaluates exact Modularity Q or Constant Potts Model H quality score on the finest level graph.</summary>
    let private computeQualityScore
        (baseG: LevelGraph)
        (totalEdgeWeight: float)
        (comm: int[])
        (commCount: int)
        (config: LeidenConfig)
        : float =
        if baseG.NodeCount = 0 || commCount = 0 then
            0.
        else
            let internalEdge = Array.zeroCreate<float> commCount
            let totalDeg = Array.zeroCreate<float> commCount
            let memberCount = Array.zeroCreate<float> commCount

            for u = 0 to baseG.NodeCount - 1 do
                let cu = comm[u]
                internalEdge[cu] <- internalEdge[cu] + baseG.SelfEdgeWeight[u]
                totalDeg[cu] <- totalDeg[cu] + baseG.NodeDegree[u]
                memberCount[cu] <- memberCount[cu] + 1.0
                for idx = baseG.Offsets[u] to baseG.Offsets[u + 1] - 1 do
                    let v = baseG.Targets[idx]
                    if u < v && comm[v] = cu then
                        internalEdge[cu] <- internalEdge[cu] + baseG.Weights[idx]

            match config.Quality with
            | CommunityQuality.Modularity ->
                if totalEdgeWeight <= 1e-15 then
                    0.
                else
                    let twoM = 2. * totalEdgeWeight
                    let mutable q = 0.
                    for c = 0 to commCount - 1 do
                        let ec = internalEdge[c] / totalEdgeWeight
                        let kc = totalDeg[c] / twoM
                        q <- q + (ec - config.Resolution * kc * kc)
                    q
            | CommunityQuality.ConstantPotts ->
                let mutable h = 0.
                for c = 0 to commCount - 1 do
                    let sc = memberCount[c]
                    h <- h + (internalEdge[c] - config.Resolution * sc * (sc - 1.) * 0.5)
                h

    let private samePartition (a: int[]) (b: int[]) : bool =
        if a.Length <> b.Length then false
        else
            let mutable eq = true
            let mutable i = 0
            while eq && i < a.Length do
                if a[i] <> b[i] then eq <- false
                i <- i + 1
            eq

    /// <summary>Detects connected communities over <c>graph</c> using the 3-phase Leiden algorithm with cooperative cancellation.</summary>
    let detectWithCancellation
        (token: System.Threading.CancellationToken)
        (config: LeidenConfig)
        (graph: DiagramGraph)
        : Result<CommunityPartition, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.Resolution) || config.Resolution < 0. then
            Error [ InvalidObservation "Leiden Resolution must be finite and nonnegative." ]
        elif config.MaxLevels < 1 || config.MaxLevels > 256 then
            Error [ InvalidObservation "Leiden MaxLevels must be in [1, 256]." ]
        elif config.MaxIterationsPerLevel < 1 || config.MaxIterationsPerLevel > 10000 then
            Error [ InvalidObservation "Leiden MaxIterationsPerLevel must be in [1, 10000]." ]
        else
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            if n = 0 then
                Ok
                    { Communities = Map.empty
                      CommunityMembers = Map.empty
                      CommunityCount = 0
                      QualityScore = 0.
                      Levels = [] }
            else
                let csr = GraphKernels.buildCsr eff
                let baseGraph, totalEdgeWeight = buildInitialLevelGraph csr config.Quality
                let gammaEff =
                    match config.Quality with
                    | CommunityQuality.Modularity ->
                        if totalEdgeWeight > 1e-15 then config.Resolution / (2. * totalEdgeWeight) else 1.0
                    | CommunityQuality.ConstantPotts -> config.Resolution

                let mutable curGraph = baseGraph
                let mutable curComm = Array.init n (fun i -> i)
                // Maps each original vertex 0..n-1 to its current supernode index in curGraph.
                let fineToSuper = Array.init n (fun i -> i)
                let recordedLevels = ResizeArray<int[]>()
                let mutable rng = RandomState.create config.Seed
                let mutable level = 0
                let mutable converged = (n <= 1 || totalEdgeWeight <= 1e-15)

                while level < config.MaxLevels && not converged do
                    token.ThrowIfCancellationRequested()
                    let moved, rng1 = moveNodesFast token curGraph curComm gammaEff config.MaxIterationsPerLevel rng
                    rng <- rng1
                    let coarseCount = reindexPartition curComm

                    if not moved && coarseCount = curGraph.NodeCount then
                        converged <- true
                    else
                        let refined, refinedCount, rng2 =
                            refinePartition token curGraph curComm coarseCount gammaEff rng
                        rng <- rng2

                        // Project current refined partition back onto original 0..n-1 nodes for hierarchical level history.
                        let projectedRefined = Array.init n (fun u -> refined[fineToSuper[u]])
                        enforceConnectedCommunities baseGraph projectedRefined |> ignore
                        if recordedLevels.Count = 0 || not (samePartition recordedLevels[recordedLevels.Count - 1] projectedRefined) then
                            recordedLevels.Add projectedRefined

                        if refinedCount = curGraph.NodeCount && coarseCount = curGraph.NodeCount then
                            converged <- true
                        elif level + 1 >= config.MaxLevels then
                            // Final configured level reached: project curComm and enforce strict connectivity.
                            for u = 0 to n - 1 do
                                fineToSuper[u] <- curComm[fineToSuper[u]]
                            converged <- true
                        else
                            for u = 0 to n - 1 do
                                fineToSuper[u] <- refined[fineToSuper[u]]
                            let nextGraph, nextInitComm = aggregateGraph curGraph refined refinedCount curComm config.Quality
                            curGraph <- nextGraph
                            curComm <- nextInitComm
                            level <- level + 1

                let finalArr =
                    if curGraph.NodeCount = n then Array.copy curComm
                    else Array.init n (fun u -> curComm[fineToSuper[u]])
                let finalCount = enforceConnectedCommunities baseGraph finalArr

                if recordedLevels.Count = 0 || not (samePartition recordedLevels[recordedLevels.Count - 1] finalArr) then
                    recordedLevels.Add(Array.copy finalArr)

                let toMap (partArr: int[]) =
                    let mutable m = Map.empty
                    for i = n - 1 downto 0 do
                        m <- Map.add eff.Nodes[i].Id partArr[i] m
                    m

                let finalMap = toMap finalArr
                let buckets = Array.init finalCount (fun _ -> ResizeArray<string>())
                // eff.Nodes is already sorted in ordinal ID order!
                for i = 0 to n - 1 do
                    buckets[finalArr[i]].Add eff.Nodes[i].Id

                let mutable memberMap = Map.empty
                for c = finalCount - 1 downto 0 do
                    memberMap <- Map.add c (List.ofSeq buckets[c]) memberMap

                let mutable levelMaps = []
                for li = recordedLevels.Count - 1 downto 0 do
                    levelMaps <- toMap recordedLevels[li] :: levelMaps

                let score = computeQualityScore baseGraph totalEdgeWeight finalArr finalCount config

                Ok
                    { Communities = finalMap
                      CommunityMembers = memberMap
                      CommunityCount = finalCount
                      QualityScore = score
                      Levels = levelMaps }

    /// <summary>Detects connected communities over <c>graph</c> using the 3-phase Leiden algorithm without cancellation.</summary>
    let detect config graph =
        detectWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Converts a multi-level Leiden <c>CommunityPartition</c> and <c>DiagramGraph</c> into a <c>Ranvier.D5.HierarchyNode&lt;string&gt;</c> suitable for <c>Treemap</c>, <c>VoronoiTreemap</c> or <c>HierarchicalBundle</c>.</summary>
    let toHierarchy
        (rootId: string)
        (graph: DiagramGraph)
        (partition: CommunityPartition)
        : Result<HierarchyNode<string>, LayoutError list> =
        if String.IsNullOrWhiteSpace rootId then
            Error [ InvalidObservation "Hierarchy root ID must not be empty." ]
        else
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            let reserved = HashSet<string>(StringComparer.Ordinal)
            reserved.Add rootId |> ignore
            let errors = ResizeArray<LayoutError>()

            for i = 0 to n - 1 do
                let nid = eff.Nodes[i].Id
                if nid = rootId then
                    errors.Add(DuplicateId rootId)
                else
                    reserved.Add nid |> ignore
                    if not (Map.containsKey nid partition.Communities) then
                        errors.Add(MissingEndpoint nid)

            if errors.Count > 0 then
                Error(List.ofSeq errors)
            elif n = 0 then
                Ok(Hierarchy.node rootId 0. [])
            else
                // Order levels from coarsest (fewest communities, top under rootId) to finest (closest to leaves).
                let rawLevels =
                    if partition.Levels.IsEmpty then [ partition.Communities ]
                    else partition.Levels
                let validLevels =
                    rawLevels
                    |> List.filter (fun lm ->
                        let mutable ok = true
                        for i = 0 to n - 1 do
                            if not (Map.containsKey eff.Nodes[i].Id lm) then ok <- false
                        ok)
                let orderedLevels =
                    let baseList = if validLevels.IsEmpty then [ partition.Communities ] else validLevels
                    baseList
                    |> List.map (fun lm ->
                        let arr = Array.init n (fun i -> lm[eff.Nodes[i].Id])
                        let cnt = reindexPartition arr
                        cnt, arr)
                    |> List.sortBy fst
                    |> List.fold (fun (acc: (int * int[]) list) (cnt, arr) ->
                        match acc with
                        | (_, prevArr) :: _ when samePartition prevArr arr -> acc
                        | _ -> (cnt, arr) :: acc) []
                    |> List.rev
                    |> List.toArray

                let makeUniqueId (baseName: string) =
                    if reserved.Add baseName then baseName
                    else
                        let mutable suffix = 1
                        let mutable candidate = baseName + "#" + string suffix
                        while not (reserved.Add candidate) do
                            suffix <- suffix + 1
                            candidate <- baseName + "#" + string suffix
                        candidate

                let levelCount = orderedLevels.Length
                let rec buildSubtree (lvlIdx: int) (memberIndices: int[]) : HierarchyNode<string> list =
                    if lvlIdx >= levelCount then
                        [ for k = 0 to memberIndices.Length - 1 do
                            let ni = memberIndices[k]
                            let nd = eff.Nodes[ni]
                            yield Hierarchy.node nd.Id nd.Mass [] ]
                    else
                        let _, lvlArr = orderedLevels[lvlIdx]
                        // Group memberIndices by community ID at lvlIdx preserving deterministic earliest-member order.
                        let groupOrder = ResizeArray<int>()
                        let groupMap = Dictionary<int, ResizeArray<int>>()
                        for k = 0 to memberIndices.Length - 1 do
                            let ni = memberIndices[k]
                            let cid = lvlArr[ni]
                            match groupMap.TryGetValue cid with
                            | true, bucket -> bucket.Add ni
                            | _ ->
                                let bucket = ResizeArray<int>()
                                bucket.Add ni
                                groupMap.Add(cid, bucket)
                                groupOrder.Add cid

                        // Skip redundant intermediate wrapper when a multi-level subgroup does not split at this intermediate level.
                        if lvlIdx > 0 && groupOrder.Count = 1 then
                            buildSubtree (lvlIdx + 1) memberIndices
                        else
                            [ for gi = 0 to groupOrder.Count - 1 do
                                let cid = groupOrder[gi]
                                let subMembers = groupMap[cid].ToArray()
                                let rawLabel =
                                    if levelCount = 1 then "community:" + string cid
                                    else "community:L" + string lvlIdx + ":" + string cid
                                let commNodeId = makeUniqueId rawLabel
                                let children = buildSubtree (lvlIdx + 1) subMembers
                                yield Hierarchy.node commNodeId 0. children ]

                let allIndices = Array.init n (fun i -> i)
                let topChildren = buildSubtree 0 allIndices
                Ok(Hierarchy.node rootId 0. topChildren)

    /// <summary>Returns a new <c>DiagramGraph</c> assigning every ungrouped node (<c>ParentGroup = None</c>) to compound <c>DiagramGroup</c>s <c>"community:0"</c>, <c>"community:1"</c>, etc.</summary>
    let applyGroups
        (graph: DiagramGraph)
        (partition: CommunityPartition)
        : Result<DiagramGraph, LayoutError list> =
        let origNodes = DiagramGraph.nodes graph
        let origGroups = DiagramGraph.groups graph
        let origAssocs = DiagramGraph.associations graph
        let origConstraints = DiagramGraph.constraints graph

        let existingGroupIds = HashSet<string>(StringComparer.Ordinal)
        for g in origGroups do existingGroupIds.Add g.Id |> ignore

        let neededCommSet = HashSet<int>()
        let errors = ResizeArray<LayoutError>()

        let updatedNodes =
            origNodes
            |> List.map (fun nd ->
                match nd.ParentGroup with
                | Some _ -> nd
                | None ->
                    match Map.tryFind nd.Id partition.Communities with
                    | None ->
                        errors.Add(MissingEndpoint nd.Id)
                        nd
                    | Some cid when cid < 0 ->
                        errors.Add(InvalidObservation("Community index must be nonnegative for node: " + nd.Id))
                        nd
                    | Some cid ->
                        neededCommSet.Add cid |> ignore
                        { nd with ParentGroup = Some("community:" + string cid) })

        if errors.Count > 0 then
            Error(List.ofSeq errors)
        else
            let neededCommIds = Seq.toArray neededCommSet
            Array.Sort neededCommIds
            let newGroups =
                [ for i = 0 to neededCommIds.Length - 1 do
                    let gid = "community:" + string neededCommIds[i]
                    if not (existingGroupIds.Contains gid) then
                        yield DiagramGroup.create gid ]
            DiagramGraph.createWithConstraints updatedNodes (origGroups @ newGroups) origAssocs origConstraints

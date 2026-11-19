namespace Ranvier.D5

open System
open System.Collections.Generic

/// <summary>Intersection aggregation semantics for UpSet set algebra (Lex et al. 2014).</summary>
[<RequireQualifiedAccess>]
type UpSetMode =
    /// <summary>Partitions items by exact set membership: items in <c>A &amp; B</c> belong to <c>A</c> and <c>B</c> and to no other set.</summary>
    | Exclusive
    /// <summary>Counts all items containing at least the participating sets, regardless of membership in other sets.</summary>
    | Inclusive

/// <summary>Ordering criterion for UpSet set intersections.</summary>
[<RequireQualifiedAccess>]
type UpSetSort =
    /// <summary>Descending intersection cardinality, breaking ties by ascending degree then ordinal ID.</summary>
    | ByCardinality
    /// <summary>Ascending participating set degree, breaking ties by descending cardinality then ordinal ID.</summary>
    | ByDegree
    /// <summary>Descending absolute deviation from statistical independence.</summary>
    | ByDeviation

/// <summary>Marginal cardinality summary of one input set.</summary>
type SetSummary =
    { /// <summary>Unique set name.</summary>
      Name: string
      /// <summary>Number of distinct items in this set.</summary>
      Count: int }

/// <summary>A non-empty set intersection with observed count, members, and Lex et al. (2014) independence deviation.</summary>
type SetIntersection<'Id> =
    { /// <summary>Unique human-readable intersection identifier.</summary>
      Id: string
      /// <summary>Participating set names in input set order.</summary>
      Sets: string list
      /// <summary>Number of participating sets (<c>Sets.Length</c>).</summary>
      Degree: int
      /// <summary>Observed item cardinality in this intersection.</summary>
      Count: int
      /// <summary>Distinct member identifiers belonging to this intersection.</summary>
      Members: 'Id list
      /// <summary>Expected item count under marginal set independence over <c>TotalItems</c>.</summary>
      ExpectedCount: float
      /// <summary>Signed count deviation (<c>float Count - ExpectedCount</c>) from independence.</summary>
      Deviation: float }

/// <summary>Complete UpSet set-algebra summary over a family of named sets.</summary>
type UpSetSummary<'Id> =
    { /// <summary>Total distinct items across the union of all input sets.</summary>
      TotalItems: int
      /// <summary>Marginal set cardinalities in input set order.</summary>
      Sets: SetSummary list
      /// <summary>Sorted non-empty intersections truncated to <c>maxIntersections</c>.</summary>
      Intersections: SetIntersection<'Id> list }

/// <summary>Lex et al. (2014) UpSet bitmask set-intersection algebra.</summary>
[<RequireQualifiedAccess>]
module UpSet =
    let inline private popcount32 (v: uint32) : int =
        let x1 = v - ((v >>> 1) &&& 0x55555555u)
        let x2 = (x1 &&& 0x33333333u) + ((x1 >>> 2) &&& 0x33333333u)
        let x3 = (x2 + (x2 >>> 4)) &&& 0x0F0F0F0Fu
        int ((x3 * 0x01010101u) >>> 24)

    let private compareWords (a: uint32 array) (b: uint32 array) : int =
        let mutable i = 0
        let mutable diff = 0
        while diff = 0 && i < a.Length do
            diff <- compare a[i] b[i]
            i <- i + 1
        diff

    let private equalWords (a: uint32 array) (b: uint32 array) : bool =
        let mutable i = 0
        let mutable eq = true
        while eq && i < a.Length do
            if a[i] <> b[i] then eq <- false
            i <- i + 1
        eq

    let inline private hasBit (words: uint32 array) (bit: int) : bool =
        (words[bit >>> 5] &&& (1u <<< (bit &&& 31))) <> 0u

    let inline private isSubsetWords (sub: uint32 array) (sup: uint32 array) : bool =
        let mutable i = 0
        let mutable ok = true
        while ok && i < sub.Length do
            let sw = sub[i]
            if (sw &&& sup[i]) <> sw then ok <- false
            i <- i + 1
        ok

    let private popcountWords (words: uint32 array) : int =
        let mutable total = 0
        for i = 0 to words.Length - 1 do
            total <- total + popcount32 words[i]
        total

    let private maskSets (setNames: string array) (words: uint32 array) : string list =
        let acc = ResizeArray<string>(8)
        for s = 0 to setNames.Length - 1 do
            if hasBit words s then acc.Add setNames[s]
        Data.resizeToList acc

    let private sortIntersections (sort: UpSetSort) (arr: struct(string * string list * int * int * float * float * uint32 array) array) =
        Array.sortInPlaceWith
            (fun
                struct(idA, _, degA, cntA, _, devA, _)
                struct(idB, _, degB, cntB, _, devB, _) ->
                match sort with
                | UpSetSort.ByCardinality ->
                    let c1 = compare cntB cntA
                    if c1 <> 0 then c1
                    else
                        let c2 = compare degA degB
                        if c2 <> 0 then c2
                        else String.CompareOrdinal(idA, idB)
                | UpSetSort.ByDegree ->
                    let c1 = compare degA degB
                    if c1 <> 0 then c1
                    else
                        let c2 = compare cntB cntA
                        if c2 <> 0 then c2
                        else String.CompareOrdinal(idA, idB)
                | UpSetSort.ByDeviation ->
                    let c1 = compare (abs devB) (abs devA)
                    if c1 <> 0 then c1
                    else
                        let c2 = compare devB devA
                        if c2 <> 0 then c2
                        else
                            let c3 = compare cntB cntA
                            if c3 <> 0 then c3
                            else String.CompareOrdinal(idA, idB))
            arr

    /// <summary>Computes marginal set cardinalities, non-empty intersections, and independence deviations using 32-bit bit-word masks.</summary>
    let compute
        (mode: UpSetMode)
        (sort: UpSetSort)
        (maxIntersections: int)
        (sets: (string * 'Id list) list)
        : Result<UpSetSummary<'Id>, LayoutError list> =
        if maxIntersections < 0 then
            Error [ InvalidObservation "UpSet maxIntersections must be nonnegative." ]
        else
            let errors = ResizeArray<LayoutError>()
            let seenNames = HashSet<string>(StringComparer.Ordinal)
            for name, _ in sets do
                if String.IsNullOrEmpty name then
                    errors.Add(InvalidObservation "Set names must be nonempty.")
                elif not (seenNames.Add name) then
                    errors.Add(DuplicateId name)
            if errors.Count > 0 then
                Error(Data.resizeToList errors)
            elif sets.IsEmpty then
                Ok { TotalItems = 0; Sets = []; Intersections = [] }
            else
                let setArr = List.toArray sets
                let numSets = setArr.Length
                let numWords = (numSets + 31) / 32
                let setNames = Array.init numSets (fun s -> fst setArr[s])

                // Deduplicate items per set and map distinct items across the union to 0 .. totalItems - 1.
                let mutable itemMap = Map.empty<'Id, int>
                let itemsRev = ResizeArray<'Id>()
                let perSetDistinct = Array.zeroCreate<int array> numSets
                for s = 0 to numSets - 1 do
                    let _, rawMembers = setArr[s]
                    let mutable seenInSet = Set.empty<'Id>
                    let memberIndices = ResizeArray<int>()
                    for m in rawMembers do
                        if not (Set.contains m seenInSet) then
                            seenInSet <- Set.add m seenInSet
                            let idx =
                                match Map.tryFind m itemMap with
                                | Some existing -> existing
                                | None ->
                                    let fresh = itemsRev.Count
                                    itemsRev.Add m
                                    itemMap <- Map.add m fresh itemMap
                                    fresh
                            memberIndices.Add idx
                    perSetDistinct[s] <- memberIndices.ToArray()

                let totalItems = itemsRev.Count
                let allItems = itemsRev.ToArray()
                let setCounts = Array.init numSets (fun s -> perSetDistinct[s].Length)
                let summaries =
                    Array.init numSets (fun s -> { Name = setNames[s]; Count = setCounts[s] })
                    |> Array.toList

                if totalItems = 0 || maxIntersections = 0 then
                    Ok { TotalItems = totalItems; Sets = summaries; Intersections = [] }
                else
                    let itemMasks = Array.init totalItems (fun _ -> Array.zeroCreate<uint32> numWords)
                    for s = 0 to numSets - 1 do
                        let wIdx = s >>> 5
                        let bit = 1u <<< (s &&& 31)
                        let members = perSetDistinct[s]
                        for k = 0 to members.Length - 1 do
                            let u = members[k]
                            let mask = itemMasks[u]
                            mask[wIdx] <- mask[wIdx] ||| bit

                    let probs = Array.init numSets (fun s -> float setCounts[s] / float totalItems)

                    let buildUniqueIds (rawEntries: struct(string list * int * int * float * float * uint32 array) array) =
                        let usedIds = HashSet<string>(StringComparer.Ordinal)
                        Array.init rawEntries.Length (fun i ->
                            let struct(sList, deg, cnt, expCnt, dev, wMask) = rawEntries[i]
                            let baseId = String.concat " & " sList
                            let uniqueId =
                                if usedIds.Add baseId then baseId
                                else
                                    let mutable suffix = 2
                                    let mutable cand = baseId + " #" + string suffix
                                    while not (usedIds.Add cand) do
                                        suffix <- suffix + 1
                                        cand <- baseId + " #" + string suffix
                                    cand
                            struct(uniqueId, sList, deg, cnt, expCnt, dev, wMask))

                    match mode with
                    | UpSetMode.Exclusive ->
                        let perm = Array.init totalItems id
                        Array.sortInPlaceWith (fun a b ->
                            let c = compareWords itemMasks[a] itemMasks[b]
                            if c <> 0 then c else compare a b) perm

                        let groups = ResizeArray<struct(uint32 array * int * int)>()
                        let mutable start = 0
                        while start < totalItems do
                            let repMask = itemMasks[perm[start]]
                            let mutable finish = start + 1
                            while finish < totalItems && equalWords repMask itemMasks[perm[finish]] do
                                finish <- finish + 1
                            groups.Add(struct(repMask, start, finish))
                            start <- finish

                        let raw =
                            Array.init groups.Count (fun g ->
                                let struct(wMask, lo, hi) = groups[g]
                                let cnt = hi - lo
                                let deg = popcountWords wMask
                                let sList = maskSets setNames wMask
                                let mutable p = 1.
                                for s = 0 to numSets - 1 do
                                    p <- p * (if hasBit wMask s then probs[s] else (1. - probs[s]))
                                let expCnt = float totalItems * p
                                let dev = float cnt - expCnt
                                struct(sList, deg, cnt, expCnt, dev, wMask))

                        let keyed = buildUniqueIds raw
                        sortIntersections sort keyed
                        let takeCount = min maxIntersections keyed.Length

                        // Lookup group range per mask to recover exact member list in O(1) without rescanning all items when small.
                        let results = Array.zeroCreate<SetIntersection<'Id>> takeCount
                        for k = 0 to takeCount - 1 do
                            let struct(interId, sList, deg, cnt, expCnt, dev, wMask) = keyed[k]
                            let members = ResizeArray<'Id>(cnt)
                            for u = 0 to totalItems - 1 do
                                if equalWords itemMasks[u] wMask then
                                    members.Add allItems[u]
                            results[k] <-
                                { Id = interId
                                  Sets = sList
                                  Degree = deg
                                  Count = cnt
                                  Members = Data.resizeToList members
                                  ExpectedCount = expCnt
                                  Deviation = dev }
                        Ok { TotalItems = totalItems; Sets = summaries; Intersections = Array.toList results }

                    | UpSetMode.Inclusive ->
                        // Check maximum active bits per item so power-set subset enumeration stays bounded.
                        let mutable maxItemDeg = 0
                        for u = 0 to totalItems - 1 do
                            let d = popcountWords itemMasks[u]
                            if d > maxItemDeg then maxItemDeg <- d
                        if numSets <= 15 then
                            let totalMasks = 1 <<< numSets
                            let counts = Array.zeroCreate<int> totalMasks
                            for u = 0 to totalItems - 1 do
                                let m = int (itemMasks[u][0])
                                counts[m] <- counts[m] + 1
                            // Fast Zeta / Sum-Over-Supersets transform: counts[m] <- sum_{sup >= m} excl[sup]
                            for bit = 0 to numSets - 1 do
                                let bMask = 1 <<< bit
                                for m = 0 to totalMasks - 1 do
                                    if (m &&& bMask) = 0 then
                                        counts[m] <- counts[m] + counts[m ||| bMask]
                            let occupied = ResizeArray<struct(string list * int * int * float * float * uint32 array)>()
                            for m = 1 to totalMasks - 1 do
                                let cnt = counts[m]
                                if cnt > 0 then
                                    let wMask = [| uint32 m |]
                                    let deg = popcount32 (uint32 m)
                                    let sList = maskSets setNames wMask
                                    let mutable p = 1.
                                    for s = 0 to numSets - 1 do
                                        if (m &&& (1 <<< s)) <> 0 then
                                            p <- p * probs[s]
                                    let expCnt = float totalItems * p
                                    let dev = float cnt - expCnt
                                    occupied.Add(struct(sList, deg, cnt, expCnt, dev, wMask))
                            let keyed = buildUniqueIds (occupied.ToArray())
                            sortIntersections sort keyed
                            let takeCount = min maxIntersections keyed.Length
                            let results = Array.zeroCreate<SetIntersection<'Id>> takeCount
                            for k = 0 to takeCount - 1 do
                                let struct(interId, sList, deg, cnt, expCnt, dev, wMask) = keyed[k]
                                let mReq = wMask[0]
                                let members = ResizeArray<'Id>(cnt)
                                for u = 0 to totalItems - 1 do
                                    if (itemMasks[u][0] &&& mReq) = mReq then
                                        members.Add allItems[u]
                                results[k] <-
                                    { Id = interId
                                      Sets = sList
                                      Degree = deg
                                      Count = cnt
                                      Members = Data.resizeToList members
                                      ExpectedCount = expCnt
                                      Deviation = dev }
                            Ok { TotalItems = totalItems; Sets = summaries; Intersections = Array.toList results }
                        elif maxItemDeg > 16 then
                            Error [ BudgetExceeded "Inclusive UpSet mode exceeds subset budget when items belong to more than 16 sets simultaneously." ]
                        else
                            // Sparse arbitrary-set-count inclusive enumeration via uint32 list structural Map keys.
                            let perm = Array.init totalItems id
                            Array.sortInPlaceWith (fun a b -> compareWords itemMasks[a] itemMasks[b]) perm
                            let mutable subsetCounts = Map.empty<uint32 list, int>
                            let mutable budgetExceeded = false
                            let activeBits = ResizeArray<int>(16)
                            let mutable start = 0
                            while start < totalItems && not budgetExceeded do
                                let rep = itemMasks[perm[start]]
                                let mutable finish = start + 1
                                while finish < totalItems && equalWords rep itemMasks[perm[finish]] do
                                    finish <- finish + 1
                                let mult = finish - start
                                activeBits.Clear()
                                for s = 0 to numSets - 1 do
                                    if hasBit rep s then activeBits.Add s
                                let kBits = activeBits.Count
                                let subTotal = (1 <<< kBits) - 1
                                for sub = 1 to subTotal do
                                    if not budgetExceeded then
                                        let subWords = Array.zeroCreate<uint32> numWords
                                        for b = 0 to kBits - 1 do
                                            if (sub &&& (1 <<< b)) <> 0 then
                                                let sIdx = activeBits[b]
                                                subWords[sIdx >>> 5] <- subWords[sIdx >>> 5] ||| (1u <<< (sIdx &&& 31))
                                        let key = Array.toList subWords
                                        match Map.tryFind key subsetCounts with
                                        | Some c -> subsetCounts <- Map.add key (c + mult) subsetCounts
                                        | None ->
                                            if subsetCounts.Count >= 100_000 then
                                                budgetExceeded <- true
                                            else
                                                subsetCounts <- Map.add key mult subsetCounts
                                start <- finish

                            if budgetExceeded then
                                Error [ BudgetExceeded "Inclusive UpSet mode exceeded 100,000 non-empty intersection subsets." ]
                            else
                                let occupied = ResizeArray<struct(string list * int * int * float * float * uint32 array)>(subsetCounts.Count)
                                for kv in subsetCounts do
                                    let wMask = List.toArray kv.Key
                                    let cnt = kv.Value
                                    let deg = popcountWords wMask
                                    let sList = maskSets setNames wMask
                                    let mutable p = 1.
                                    for s = 0 to numSets - 1 do
                                        if hasBit wMask s then p <- p * probs[s]
                                    let expCnt = float totalItems * p
                                    let dev = float cnt - expCnt
                                    occupied.Add(struct(sList, deg, cnt, expCnt, dev, wMask))
                                let keyed = buildUniqueIds (occupied.ToArray())
                                sortIntersections sort keyed
                                let takeCount = min maxIntersections keyed.Length
                                let results = Array.zeroCreate<SetIntersection<'Id>> takeCount
                                for k = 0 to takeCount - 1 do
                                    let struct(interId, sList, deg, cnt, expCnt, dev, wMask) = keyed[k]
                                    let members = ResizeArray<'Id>(cnt)
                                    for u = 0 to totalItems - 1 do
                                        if isSubsetWords wMask itemMasks[u] then
                                            members.Add allItems[u]
                                    results[k] <-
                                        { Id = interId
                                          Sets = sList
                                          Degree = deg
                                          Count = cnt
                                          Members = Data.resizeToList members
                                          ExpectedCount = expCnt
                                          Deviation = dev }
                                Ok { TotalItems = totalItems; Sets = summaries; Intersections = Array.toList results }

/// <summary>A placed area-proportional circle in a Wilkinson (2012) Euler or Venn diagram.</summary>
type EulerCircle =
    { /// <summary>Set name.</summary>
      SetName: string
      /// <summary>Original nonnegative set size.</summary>
      Size: float
      /// <summary>Placed circle center in scene coordinates.</summary>
      Center: Point
      /// <summary>Placed circle radius in scene units.</summary>
      Radius: float }

/// <summary>Completed circular Euler/Venn layout and normalized area-fit stress.</summary>
type EulerLayout =
    { /// <summary>Placed circles in input set order.</summary>
      Circles: EulerCircle list
      /// <summary>Normalized pairwise overlap residual stress (<c>0</c> indicates an exact pairwise fit).</summary>
      Stress: float }

/// <summary>Wilkinson (2012) area-proportional circular Euler and Venn diagram layout.</summary>
[<RequireQualifiedAccess>]
module EulerVenn =
    /// Exact analytical two-circle lens intersection area L(r1, r2, d) and derivative dL/dd.
    let private lensAreaAndDeriv (r1: float) (r2: float) (d: float) : struct(float * float) =
        if r1 <= 0. || r2 <= 0. || d >= r1 + r2 then
            struct(0., 0.)
        else
            let diff = abs (r1 - r2)
            if d <= diff then
                let rMin = min r1 r2
                struct(Math.PI * rMin * rMin, 0.)
            else
                let c1 = Math.Clamp((d * d + r1 * r1 - r2 * r2) / (2. * d * r1), -1., 1.)
                let c2 = Math.Clamp((d * d + r2 * r2 - r1 * r1) / (2. * d * r2), -1., 1.)
                let delta = max 0. ((-d + r1 + r2) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2))
                let root = sqrt delta
                let area = r1 * r1 * acos c1 + r2 * r2 * acos c2 - 0.5 * root
                let deriv = if d > 1e-30 then -root / d else 0.
                struct(max 0. area, deriv)

    /// Inverts L(r1, r2, d) = targetArea via safeguarded Newton-bisection on [|r1 - r2|, r1 + r2].
    let private invertTargetDistance (r1: float) (r2: float) (targetArea: float) : float =
        if r1 <= 0. || r2 <= 0. then
            0.
        elif targetArea <= 0. then
            1.08 * (r1 + r2)
        else
            let rMin = min r1 r2
            let maxOverlap = Math.PI * rMin * rMin
            if targetArea >= maxOverlap * (1. - 1e-12) then
                0.5 * abs (r1 - r2)
            else
                let mutable lo = abs (r1 - r2)
                let mutable hi = r1 + r2
                let mutable mid = 0.5 * (lo + hi)
                for _ = 1 to 60 do
                    let struct(valMid, dMid) = lensAreaAndDeriv r1 r2 mid
                    let err = valMid - targetArea
                    if err > 0. then
                        // Area too large -> increase separation lo <- mid
                        lo <- mid
                    else
                        hi <- mid
                    // Safeguarded Newton step
                    let newton = if dMid < -1e-20 then mid - err / dMid else Double.NaN
                    if Double.IsFinite newton && newton > lo && newton < hi then
                        mid <- newton
                    else
                        mid <- 0.5 * (lo + hi)
                mid

    /// Classical Torgerson MDS into 2D using cyclic symmetric Jacobi eigendecomposition (exact even on repeated eigenvalues).
    let private classicalMds2D (dist: float array array) : struct(float array * float array) =
        let n = dist.Length
        let px = Array.zeroCreate<float> n
        let py = Array.zeroCreate<float> n
        if n = 1 then
            struct(px, py)
        elif n = 2 then
            let half = 0.5 * dist[0][1]
            px[0] <- -half
            px[1] <- half
            struct(px, py)
        else
            let dSq = Array.init n (fun i -> Array.init n (fun j -> let d = dist[i][j] in d * d))
            let rowMean = Array.zeroCreate<float> n
            let mutable grandMean = 0.
            for i = 0 to n - 1 do
                let mutable s = 0.
                for j = 0 to n - 1 do s <- s + dSq[i][j]
                rowMean[i] <- s / float n
                grandMean <- grandMean + s
            grandMean <- grandMean / float (n * n)

            let bMat = Array.init n (fun i ->
                Array.init n (fun j -> -0.5 * (dSq[i][j] - rowMean[i] - rowMean[j] + grandMean)))
            let eigVecs = Array.init n (fun i -> Array.init n (fun j -> if i = j then 1. else 0.))

            // Cyclic Jacobi sweeps
            let maxSweeps = if n <= 40 then 30 else 12
            for _ = 1 to maxSweeps do
                for p = 0 to n - 2 do
                    for q = p + 1 to n - 1 do
                        let apq = bMat[p][q]
                        if abs apq > 1e-14 then
                            let app = bMat[p][p]
                            let aqq = bMat[q][q]
                            let tau = (aqq - app) / (2. * apq)
                            let t =
                                if tau >= 0. then 1. / (tau + sqrt (1. + tau * tau))
                                else -1. / (-tau + sqrt (1. + tau * tau))
                            let c = 1. / sqrt (1. + t * t)
                            let s = t * c
                            bMat[p][p] <- app - t * apq
                            bMat[q][q] <- aqq + t * apq
                            bMat[p][q] <- 0.
                            bMat[q][p] <- 0.
                            for k = 0 to n - 1 do
                                if k <> p && k <> q then
                                    let akp = bMat[k][p]
                                    let akq = bMat[k][q]
                                    bMat[k][p] <- c * akp - s * akq
                                    bMat[p][k] <- bMat[k][p]
                                    bMat[k][q] <- s * akp + c * akq
                                    bMat[q][k] <- bMat[k][q]
                            for k = 0 to n - 1 do
                                let vkp = eigVecs[k][p]
                                let vkq = eigVecs[k][q]
                                eigVecs[k][p] <- c * vkp - s * vkq
                                eigVecs[k][q] <- s * vkp + c * vkq

            let order = Array.init n id
            Array.sortInPlaceWith (fun a b -> compare (bMat[b][b]) (bMat[a][a])) order
            let col0 = order[0]
            let col1 = order[1]
            let scale0 = sqrt (max 0. (bMat[col0][col0]))
            let scale1 = sqrt (max 0. (bMat[col1][col1]))
            for i = 0 to n - 1 do
                px[i] <- eigVecs[i][col0] * scale0
                py[i] <- eigVecs[i][col1] * scale1
            // Slight deterministic symmetry break if MDS collapsed into 1D for n >= 3.
            if scale1 <= 1e-10 then
                for i = 0 to n - 1 do
                    py[i] <- 1e-3 * sin (float (i + 1) * 2.399963229728653)
            struct(px, py)

    /// <summary>Computes an area-proportional circular Euler/Venn layout inside <c>bounds</c> via analytical lens inversion, classical MDS, and stress minimization.</summary>
    let layout
        (bounds: Bounds)
        (sets: (string * float) list)
        (pairwiseOverlaps: (string * string * float) list)
        : Result<EulerLayout, LayoutError list> =
        let bw, bh = Bounds.width bounds, Bounds.height bounds
        if not (Double.IsFinite bw && Double.IsFinite bh) || bw <= 0. || bh <= 0. then
            Error [ InvalidObservation "EulerVenn bounds must have positive width and height." ]
        else
            let errors = ResizeArray<LayoutError>()
            let nameToIndex = Dictionary<string, int>(StringComparer.Ordinal)
            let mutable idx = 0
            for name, sz in sets do
                if String.IsNullOrEmpty name then
                    errors.Add(InvalidObservation "Set names must be nonempty.")
                elif nameToIndex.ContainsKey name then
                    errors.Add(DuplicateId name)
                else
                    nameToIndex.Add(name, idx)
                if not (Double.IsFinite sz) || sz < 0. then
                    errors.Add(InvalidObservation("Set size must be finite and nonnegative: " + name))
                idx <- idx + 1

            let setArr = List.toArray sets
            let n = setArr.Length
            let seenPairs = HashSet<int64>()
            if errors.Count = 0 then
                for a, b, ov in pairwiseOverlaps do
                    let hasA, ia = nameToIndex.TryGetValue a
                    let hasB, ib = nameToIndex.TryGetValue b
                    if not hasA then errors.Add(MissingEndpoint a)
                    if not hasB then errors.Add(MissingEndpoint b)
                    if not (Double.IsFinite ov) || ov < 0. then
                        errors.Add(InvalidObservation "Pairwise overlap must be finite and nonnegative.")
                    elif hasA && hasB then
                        if ia = ib then
                            errors.Add(InvalidTopology("Self-overlap is invalid: " + a))
                        else
                            let lo, hi = if ia < ib then ia, ib else ib, ia
                            let pKey = (int64 lo <<< 32) ||| int64 (uint32 hi)
                            if not (seenPairs.Add pKey) then
                                errors.Add(DuplicateId(a + "&" + b))
                            else
                                let maxAllowed = min (snd setArr[ia]) (snd setArr[ib])
                                if ov > maxAllowed * (1. + 1e-9) + 1e-12 then
                                    errors.Add(InvalidObservation("Pairwise overlap exceeds constituent set size: " + a + " & " + b))

            if errors.Count > 0 then
                Error(Data.resizeToList errors)
            elif n = 0 then
                Ok { Circles = []; Stress = 0. }
            elif n = 1 then
                let name, sz = setArr[0]
                let center = Point.create (Bounds.x bounds + bw * 0.5) (Bounds.y bounds + bh * 0.5)
                let rad = if sz > 0. then min bw bh * 0.42 else 0.
                Ok
                    { Circles = [ { SetName = name; Size = sz; Center = center; Radius = rad } ]
                      Stress = 0. }
            else
                let mutable maxSize = 0.
                for i = 0 to n - 1 do
                    let sz = snd setArr[i]
                    if sz > maxSize then maxSize <- sz

                if maxSize <= 0. then
                    let step = bw / float (n + 1)
                    let cy = Bounds.y bounds + bh * 0.5
                    let circles =
                        List.init n (fun i ->
                            let name, sz = setArr[i]
                            { SetName = name
                              Size = sz
                              Center = Point.create (Bounds.x bounds + step * float (i + 1)) cy
                              Radius = 0. })
                    Ok { Circles = circles; Stress = 0. }
                else
                    let normAreas = Array.init n (fun i -> snd setArr[i] / maxSize)
                    let radii = Array.init n (fun i -> sqrt (normAreas[i] / Math.PI))
                    let targetOverlaps = Array.init n (fun _ -> Array.zeroCreate<float> n)
                    for a, b, ov in pairwiseOverlaps do
                        let ia = nameToIndex[a]
                        let ib = nameToIndex[b]
                        let clamped = Math.Clamp(ov / maxSize, 0., min normAreas[ia] normAreas[ib])
                        targetOverlaps[ia][ib] <- clamped
                        targetOverlaps[ib][ia] <- clamped

                    let targetDist = Array.init n (fun _ -> Array.zeroCreate<float> n)
                    for i = 0 to n - 2 do
                        for j = i + 1 to n - 1 do
                            let dStar = invertTargetDistance radii[i] radii[j] (targetOverlaps[i][j])
                            targetDist[i][j] <- dStar
                            targetDist[j][i] <- dStar

                    let struct(cx, cy) = classicalMds2D targetDist

                    let gx = Array.zeroCreate<float> n
                    let gy = Array.zeroCreate<float> n
                    let trialX = Array.zeroCreate<float> n
                    let trialY = Array.zeroCreate<float> n

                    // Evaluates objective + gradient; distWeight > 0 guides separated pairs into overlap range during Stage 1.
                    let evalLossAndGrad (xs: float array) (ys: float array) (distWeight: float) (writeGrad: bool) : float =
                        if writeGrad then
                            for i = 0 to n - 1 do
                                gx[i] <- 0.
                                gy[i] <- 0.
                        let mutable loss = 0.
                        for i = 0 to n - 2 do
                            let ri = radii[i]
                            for j = i + 1 to n - 1 do
                                let rj = radii[j]
                                if ri > 0. || rj > 0. then
                                    let dx = xs[i] - xs[j]
                                    let dy = ys[i] - ys[j]
                                    let d = Double.Hypot(dx, dy)
                                    let struct(fitA, dLdD) = lensAreaAndDeriv ri rj d
                                    let targA = targetOverlaps[i][j]
                                    let errA = fitA - targA
                                    loss <- loss + errA * errA
                                    let mutable dLossDd = 2. * errA * dLdD
                                    if distWeight > 0. then
                                        let dStar = targetDist[i][j]
                                        let needGuide =
                                            (targA > 0. && d >= ri + rj)
                                            || (targA = 0. && d < ri + rj)
                                            || (d <= abs (ri - rj) && targA < Math.PI * (min ri rj) * (min ri rj) * (1. - 1e-9))
                                            || distWeight >= 0.05
                                        if needGuide then
                                            let errD = d - dStar
                                            // Ignore surplus separation when targA = 0 and d >= dStar.
                                            if not (targA = 0. && d >= ri + rj) then
                                                loss <- loss + distWeight * errD * errD
                                                dLossDd <- dLossDd + 2. * distWeight * errD
                                    if writeGrad && abs dLossDd > 0. then
                                        let ux, uy =
                                            if d > 1e-15 then dx / d, dy / d
                                            else
                                                let ang = float (i * 7 + j * 13) * 0.7
                                                cos ang, sin ang
                                        let fx = dLossDd * ux
                                        let fy = dLossDd * uy
                                        gx[i] <- gx[i] + fx
                                        gy[i] <- gy[i] + fy
                                        gx[j] <- gx[j] - fx
                                        gy[j] <- gy[j] - fy
                        loss

                    let optimizeStage (distWeight: float) (maxSteps: int) =
                        let mutable curLoss = evalLossAndGrad cx cy distWeight true
                        let mutable step = 0.25
                        let mutable it = 0
                        while it < maxSteps && curLoss > 1e-24 && step > 1e-14 do
                            let mutable gNormSq = 0.
                            for i = 0 to n - 1 do
                                gNormSq <- gNormSq + gx[i] * gx[i] + gy[i] * gy[i]
                            if gNormSq <= 1e-26 then
                                it <- maxSteps
                            else
                                let mutable accepted = false
                                let mutable tries = 0
                                while not accepted && tries < 12 do
                                    for i = 0 to n - 1 do
                                        trialX[i] <- cx[i] - step * gx[i]
                                        trialY[i] <- cy[i] - step * gy[i]
                                    let nextLoss = evalLossAndGrad trialX trialY distWeight false
                                    if nextLoss < curLoss - 1e-4 * step * gNormSq then
                                        for i = 0 to n - 1 do
                                            cx[i] <- trialX[i]
                                            cy[i] <- trialY[i]
                                        curLoss <- evalLossAndGrad cx cy distWeight true
                                        step <- min 2.0 (step * 1.2)
                                        accepted <- true
                                    else
                                        step <- step * 0.5
                                        tries <- tries + 1
                                if not accepted then
                                    it <- maxSteps
                                else
                                    it <- it + 1

                    if n > 2 then
                        optimizeStage 0.1 120
                        optimizeStage 0.0 180
                        // Coordinate Hooke-Jeeves polish for piecewise-smooth grazing tangencies.
                        let mutable delta = 0.02
                        let mutable curPure = evalLossAndGrad cx cy 0.0 false
                        for _ = 1 to 25 do
                            let mutable improved = false
                            for i = 0 to n - 1 do
                                let ox = cx[i]
                                cx[i] <- ox + delta
                                let lPosX = evalLossAndGrad cx cy 0.0 false
                                if lPosX < curPure then
                                    curPure <- lPosX
                                    improved <- true
                                else
                                    cx[i] <- ox - delta
                                    let lNegX = evalLossAndGrad cx cy 0.0 false
                                    if lNegX < curPure then
                                        curPure <- lNegX
                                        improved <- true
                                    else
                                        cx[i] <- ox

                                let oy = cy[i]
                                cy[i] <- oy + delta
                                let lPosY = evalLossAndGrad cx cy 0.0 false
                                if lPosY < curPure then
                                    curPure <- lPosY
                                    improved <- true
                                else
                                    cy[i] <- oy - delta
                                    let lNegY = evalLossAndGrad cx cy 0.0 false
                                    if lNegY < curPure then
                                        curPure <- lNegY
                                        improved <- true
                                    else
                                        cy[i] <- oy
                            if not improved then
                                delta <- delta * 0.5

                    // Compute scale-invariant Wilkinson stress over all unordered pairs.
                    let mutable rss = 0.
                    let mutable sumTargSq = 0.
                    let mutable sumFitSq = 0.
                    for i = 0 to n - 2 do
                        for j = i + 1 to n - 1 do
                            let d = Double.Hypot(cx[i] - cx[j], cy[i] - cy[j])
                            let struct(fitA, _) = lensAreaAndDeriv radii[i] radii[j] d
                            let targA = targetOverlaps[i][j]
                            let diff = fitA - targA
                            rss <- rss + diff * diff
                            sumTargSq <- sumTargSq + targA * targA
                            sumFitSq <- sumFitSq + fitA * fitA
                    let denom = max sumTargSq sumFitSq
                    let stress =
                        if rss <= 1e-26 then 0.
                        elif denom > 1e-26 then rss / denom
                        else rss

                    // Uniformly fit bounding box of all placed circles inside bounds.
                    let mutable minEx = Double.PositiveInfinity
                    let mutable maxEx = Double.NegativeInfinity
                    let mutable minEy = Double.PositiveInfinity
                    let mutable maxEy = Double.NegativeInfinity
                    for i = 0 to n - 1 do
                        let r = radii[i]
                        if cx[i] - r < minEx then minEx <- cx[i] - r
                        if cx[i] + r > maxEx then maxEx <- cx[i] + r
                        if cy[i] - r < minEy then minEy <- cy[i] - r
                        if cy[i] + r > maxEy then maxEy <- cy[i] + r
                    let spanX = max 1e-12 (maxEx - minEx)
                    let spanY = max 1e-12 (maxEy - minEy)
                    let padFrac = 0.94
                    let scale = min ((bw * padFrac) / spanX) ((bh * padFrac) / spanY)
                    let ox = Bounds.x bounds + (bw - spanX * scale) * 0.5 - minEx * scale
                    let oy = Bounds.y bounds + (bh - spanY * scale) * 0.5 - minEy * scale

                    let circles =
                        List.init n (fun i ->
                            let name, sz = setArr[i]
                            { SetName = name
                              Size = sz
                              Center = Point.create (ox + cx[i] * scale) (oy + cy[i] * scale)
                              Radius = radii[i] * scale })
                    Ok { Circles = circles; Stress = stress }

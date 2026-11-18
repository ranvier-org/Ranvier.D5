namespace Ranvier.D5

open System
open System.Collections.Generic
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif

module internal Check =
    let inline finite name value =
        if not (Double.IsFinite value) then invalidArg name "Expected a finite number."
    let inline positive name value =
        finite name value
        if value <= 0. then invalidArg name "Expected a positive number."
    let inline interval name (a, b) =
        finite name a
        finite name b
        finite name (b - a)
    let inline lerp a b t =
        let value = Ranvier.Anime.Utils.lerp a b t
        if Double.IsFinite value then value else a * (1. - t) + b * t
    let unique name key values =
        let seen = HashSet<_>()
        for value in values do
            if not (seen.Add (key value)) then invalidArg name "Keys must be unique."

#if !FABLE_COMPILER
module internal SimdNumeric =
    /// Scans an array for finiteness, min, and max in one SIMD pass.
    /// Returns struct(allFinite, finiteCount, minVal, maxVal).
    let scanArrayMinMax (arr: float array) : struct(bool * int * float * float) =
        let n = arr.Length
        if n = 0 then struct(true, 0, Double.PositiveInfinity, Double.NegativeInfinity)
        else
            let mutable i = 0
            let mutable allFinite = true
            let mutable count = 0
            let mutable lo = Double.PositiveInfinity
            let mutable hi = Double.NegativeInfinity
            if Vector512.IsHardwareAccelerated && n >= 8 then
                let vInf = Vector512.Create Double.PositiveInfinity
                let mutable vMin = Vector512.Create Double.PositiveInfinity
                let mutable vMax = Vector512.Create Double.NegativeInfinity
                let mutable vecCount = 0
                while i <= n - 8 do
                    let v = Vector512.LoadUnsafe(&MemoryMarshal.GetArrayDataReference arr, unativeint i)
                    let fin = Vector512.LessThan(Vector512.Abs v, vInf)
                    if fin = Vector512<float>.AllBitsSet then
                        vMin <- Vector512.Min(vMin, v)
                        vMax <- Vector512.Max(vMax, v)
                        vecCount <- vecCount + 8
                    else
                        allFinite <- false
                        for lane = 0 to 7 do
                            let x = arr[i + lane]
                            if Double.IsFinite x then
                                count <- count + 1
                                if x < lo then lo <- x
                                if x > hi then hi <- x
                    i <- i + 8
                if vecCount > 0 then
                    count <- count + vecCount
                    for lane = 0 to 7 do
                        let mn = vMin.GetElement lane
                        let mx = vMax.GetElement lane
                        if mn < lo then lo <- mn
                        if mx > hi then hi <- mx
            elif Vector256.IsHardwareAccelerated && n >= 4 then
                let vInf = Vector256.Create Double.PositiveInfinity
                let mutable vMin = Vector256.Create Double.PositiveInfinity
                let mutable vMax = Vector256.Create Double.NegativeInfinity
                let mutable vecCount = 0
                while i <= n - 4 do
                    let v = Vector256.LoadUnsafe(&MemoryMarshal.GetArrayDataReference arr, unativeint i)
                    let fin = Vector256.LessThan(Vector256.Abs v, vInf)
                    if fin = Vector256<float>.AllBitsSet then
                        vMin <- Vector256.Min(vMin, v)
                        vMax <- Vector256.Max(vMax, v)
                        vecCount <- vecCount + 4
                    else
                        allFinite <- false
                        for lane = 0 to 3 do
                            let x = arr[i + lane]
                            if Double.IsFinite x then
                                count <- count + 1
                                if x < lo then lo <- x
                                if x > hi then hi <- x
                    i <- i + 4
                if vecCount > 0 then
                    count <- count + vecCount
                    for lane = 0 to 3 do
                        let mn = vMin.GetElement lane
                        let mx = vMax.GetElement lane
                        if mn < lo then lo <- mn
                        if mx > hi then hi <- mx
            elif Vector128.IsHardwareAccelerated && n >= 2 then
                let vInf = Vector128.Create Double.PositiveInfinity
                let mutable vMin = Vector128.Create Double.PositiveInfinity
                let mutable vMax = Vector128.Create Double.NegativeInfinity
                let mutable vecCount = 0
                while i <= n - 2 do
                    let v = Vector128.LoadUnsafe(&MemoryMarshal.GetArrayDataReference arr, unativeint i)
                    let fin = Vector128.LessThan(Vector128.Abs v, vInf)
                    if fin = Vector128<float>.AllBitsSet then
                        vMin <- Vector128.Min(vMin, v)
                        vMax <- Vector128.Max(vMax, v)
                        vecCount <- vecCount + 2
                    else
                        allFinite <- false
                        for lane = 0 to 1 do
                            let x = arr[i + lane]
                            if Double.IsFinite x then
                                count <- count + 1
                                if x < lo then lo <- x
                                if x > hi then hi <- x
                    i <- i + 2
                if vecCount > 0 then
                    count <- count + vecCount
                    for lane = 0 to 1 do
                        let mn = vMin.GetElement lane
                        let mx = vMax.GetElement lane
                        if mn < lo then lo <- mn
                        if mx > hi then hi <- mx
            while i < n do
                let x = arr[i]
                if Double.IsFinite x then
                    count <- count + 1
                    if x < lo then lo <- x
                    if x > hi then hi <- x
                else
                    allFinite <- false
                i <- i + 1
            struct(allFinite, count, lo, hi)

    /// Sums a contiguous finite ReadOnlySpan<float> with 3-tier SIMD.
    let sumSpan (span: ReadOnlySpan<float>) : float =
        let n = span.Length
        let mutable i = 0
        let mutable acc = 0.
        if Vector512.IsHardwareAccelerated && n >= 8 then
            let mutable vSum = Vector512<float>.Zero
            let r0 = &MemoryMarshal.GetReference span
            while i <= n - 8 do
                vSum <- vSum + Vector512.LoadUnsafe(&r0, unativeint i)
                i <- i + 8
            acc <- Vector512.Sum vSum
        elif Vector256.IsHardwareAccelerated && n >= 4 then
            let mutable vSum = Vector256<float>.Zero
            let r0 = &MemoryMarshal.GetReference span
            while i <= n - 4 do
                vSum <- vSum + Vector256.LoadUnsafe(&r0, unativeint i)
                i <- i + 4
            acc <- Vector256.Sum vSum
        elif Vector128.IsHardwareAccelerated && n >= 2 then
            let mutable vSum = Vector128<float>.Zero
            let r0 = &MemoryMarshal.GetReference span
            while i <= n - 2 do
                vSum <- vSum + Vector128.LoadUnsafe(&r0, unativeint i)
                i <- i + 2
            acc <- Vector128.Sum vSum
        while i < n do
            acc <- acc + span[i]
            i <- i + 1
        acc

    /// Computes scaled corrected two-pass sum of squared deviations on an all-finite float array:
    /// returns sum((x_i * invMag - mean)^2) - (sum(x_i * invMag - mean))^2 / n.
    let scaledCenteredSumSquares (arr: float array) (invMag: float) : float =
        let n = arr.Length
        let mutable i = 0
        let mutable sumScaled = 0.
        let r0 = &MemoryMarshal.GetArrayDataReference arr
        if Vector512.IsHardwareAccelerated && n >= 8 then
            let vInv = Vector512.Create invMag
            let mutable vAcc = Vector512<float>.Zero
            while i <= n - 8 do
                vAcc <- Vector512.FusedMultiplyAdd(Vector512.LoadUnsafe(&r0, unativeint i), vInv, vAcc)
                i <- i + 8
            sumScaled <- Vector512.Sum vAcc
        elif Vector256.IsHardwareAccelerated && n >= 4 then
            let vInv = Vector256.Create invMag
            let mutable vAcc = Vector256<float>.Zero
            while i <= n - 4 do
                vAcc <- Vector256.FusedMultiplyAdd(Vector256.LoadUnsafe(&r0, unativeint i), vInv, vAcc)
                i <- i + 4
            sumScaled <- Vector256.Sum vAcc
        elif Vector128.IsHardwareAccelerated && n >= 2 then
            let vInv = Vector128.Create invMag
            let mutable vAcc = Vector128<float>.Zero
            while i <= n - 2 do
                vAcc <- Vector128.FusedMultiplyAdd(Vector128.LoadUnsafe(&r0, unativeint i), vInv, vAcc)
                i <- i + 2
            sumScaled <- Vector128.Sum vAcc
        while i < n do
            sumScaled <- sumScaled + arr[i] * invMag
            i <- i + 1
        let mean = sumScaled / float n
        let mutable j = 0
        let mutable sumSq = 0.
        let mutable sumDiff = 0.
        if Vector512.IsHardwareAccelerated && n >= 8 then
            let vInv = Vector512.Create invMag
            let vNegMean = Vector512.Create(-mean)
            let mutable vSq = Vector512<float>.Zero
            let mutable vDiff = Vector512<float>.Zero
            while j <= n - 8 do
                let d = Vector512.FusedMultiplyAdd(Vector512.LoadUnsafe(&r0, unativeint j), vInv, vNegMean)
                vSq <- Vector512.FusedMultiplyAdd(d, d, vSq)
                vDiff <- vDiff + d
                j <- j + 8
            sumSq <- Vector512.Sum vSq
            sumDiff <- Vector512.Sum vDiff
        elif Vector256.IsHardwareAccelerated && n >= 4 then
            let vInv = Vector256.Create invMag
            let vNegMean = Vector256.Create(-mean)
            let mutable vSq = Vector256<float>.Zero
            let mutable vDiff = Vector256<float>.Zero
            while j <= n - 4 do
                let d = Vector256.FusedMultiplyAdd(Vector256.LoadUnsafe(&r0, unativeint j), vInv, vNegMean)
                vSq <- Vector256.FusedMultiplyAdd(d, d, vSq)
                vDiff <- vDiff + d
                j <- j + 4
            sumSq <- Vector256.Sum vSq
            sumDiff <- Vector256.Sum vDiff
        elif Vector128.IsHardwareAccelerated && n >= 2 then
            let vInv = Vector128.Create invMag
            let vNegMean = Vector128.Create(-mean)
            let mutable vSq = Vector128<float>.Zero
            let mutable vDiff = Vector128<float>.Zero
            while j <= n - 2 do
                let d = Vector128.FusedMultiplyAdd(Vector128.LoadUnsafe(&r0, unativeint j), vInv, vNegMean)
                vSq <- Vector128.FusedMultiplyAdd(d, d, vSq)
                vDiff <- vDiff + d
                j <- j + 2
            sumSq <- Vector128.Sum vSq
            sumDiff <- Vector128.Sum vDiff
        while j < n do
            let d = arr[j] * invMag - mean
            sumSq <- sumSq + d * d
            sumDiff <- sumDiff + d
            j <- j + 1
        max 0. (sumSq - (sumDiff * sumDiff) / float n)
#endif

module internal Quickselect =
    let inline private swap (arr: float array) i j =
        let tmp = arr[i]
        arr[i] <- arr[j]
        arr[j] <- tmp

    /// Partitions arr[left..right] so arr[k] holds the k-th smallest element, with introsort depth guard.
    let selectInPlace (arr: float array) (leftInit: int) (rightInit: int) (k: int) =
        let mutable left = leftInit
        let mutable right = rightInit
        let mutable budget = 32
        let mutable salt = 0x9E3779B9u
        while right > left do
            if budget <= 0 || right - left <= 24 then
                Array.Sort(arr, left, right - left + 1)
                left <- right
            else
                budget <- budget - 1
                let span = uint32 (right - left + 1)
                salt <- salt + 0x9E3779B9u
                let i1 = left + int ((uint64 (salt ^^^ 0x85EBCA6Bu) * uint64 span) >>> 32)
                let i2 = left + int ((uint64 (salt * 1664525u + 1013904223u) * uint64 span) >>> 32)
                let i3 = left + int ((uint64 (salt * 22695477u + 1u) * uint64 span) >>> 32)
                let a, b, c = arr[i1], arr[i2], arr[i3]
                let pivotIdx =
                    if a <= b then
                        if b <= c then i2 elif a <= c then i3 else i1
                    else
                        if a <= c then i1 elif b <= c then i3 else i2
                let pivot = arr[pivotIdx]
                // 3-way Dutch National Flag partition (< pivot | = pivot | > pivot)
                let mutable lt = left
                let mutable idx = left
                let mutable gt = right
                while idx <= gt do
                    let v = arr[idx]
                    if v < pivot then
                        swap arr lt idx
                        lt <- lt + 1
                        idx <- idx + 1
                    elif v > pivot then
                        swap arr idx gt
                        gt <- gt - 1
                    else
                        idx <- idx + 1
                if k < lt then
                    right <- lt - 1
                elif k > gt then
                    left <- gt + 1
                else
                    left <- right

    /// Places both arr[lower] and arr[upper] (lower <= upper) at their sorted positions.
    let selectAdjacentPair (arr: float array) (lower: int) (upper: int) =
        let n = arr.Length
        selectInPlace arr 0 (n - 1) lower
        if upper > lower && upper < n then
            let mutable mn = arr[lower + 1]
            for idx = lower + 2 to n - 1 do
                if arr[idx] < mn then mn <- arr[idx]
            arr[upper] <- mn

    /// Copies finite elements from values into a fresh array without intermediate LINQ allocations.
    let copyFinite (values: seq<float>) : float array =
        match box values with
        | :? (float array) as arr ->
            let mutable count = 0
            for i = 0 to arr.Length - 1 do
                if Double.IsFinite arr[i] then count <- count + 1
            if count = 0 then [||]
            else
                let res = Array.zeroCreate<float> count
                let mutable w = 0
                for i = 0 to arr.Length - 1 do
                    let v = arr[i]
                    if Double.IsFinite v then
                        res[w] <- v
                        w <- w + 1
                res
        | :? (float list) as lst ->
            let mutable count = 0
            for v in lst do
                if Double.IsFinite v then count <- count + 1
            if count = 0 then [||]
            else
                let res = Array.zeroCreate<float> count
                let mutable w = 0
                for v in lst do
                    if Double.IsFinite v then
                        res[w] <- v
                        w <- w + 1
                res
        | _ ->
            let buf = ResizeArray<float>()
            for v in values do
                if Double.IsFinite v then buf.Add v
            buf.ToArray()

/// <summary>A numeric interval and the observations assigned to it.</summary>
type Bin<'T> = { Lower: float; Upper: float; Values: 'T list }

/// <summary>Keyed changes with source and target indices; lists follow their respective input order.</summary>
type Join<'Key, 'T> = {
    /// <summary>Keys only in the next input, with their index in next and their value.</summary>
    Enter: ('Key * int * 'T) list
    /// <summary>Keys in both inputs, with the previous index, next index, previous value and next value.</summary>
    Update: ('Key * int * int * 'T * 'T) list
    /// <summary>Keys only in the previous input, with their index in previous and their value.</summary>
    Exit: ('Key * int * 'T) list
}

/// <summary>Pure data transforms. Numeric summaries omit NaN and infinite observations.</summary>
[<RequireQualifiedAccess>]
module Data =
    /// <summary>Returns the finite minimum and maximum, or None when no finite observations exist.</summary>
    let extent (values: seq<float>) =
#if !FABLE_COMPILER
        match box values with
        | :? (float array) as arr ->
            let struct(_, count, lo, hi) = SimdNumeric.scanArrayMinMax arr
            if count = 0 then None else Some(lo, hi)
        | :? (float list) as lst ->
            let mutable hasAny = false
            let mutable lo = Double.PositiveInfinity
            let mutable hi = Double.NegativeInfinity
            for value in lst do
                if Double.IsFinite value then
                    hasAny <- true
                    if value < lo then lo <- value
                    if value > hi then hi <- value
            if hasAny then Some(lo, hi) else None
        | _ ->
#endif
            let mutable hasAny = false
            let mutable lo = Double.PositiveInfinity
            let mutable hi = Double.NegativeInfinity
            for value in values do
                if Double.IsFinite value then
                    hasAny <- true
                    if value < lo then lo <- value
                    if value > hi then hi <- value
            if hasAny then Some(lo, hi) else None

    /// <summary>Sums finite observations with compensated accumulation; rejects arithmetic overflow.</summary>
    let sum values =
        let mutable total = 0.
        let mutable correction = 0.
        for value in values do
            if Double.IsFinite value then
                let next = total + value
                Check.finite "values" next
                correction <- correction + (if abs total >= abs value then (total - next) + value else (value - next) + total)
                total <- next
        let result = total + correction
        Check.finite "values" result
        result

    /// <summary>Returns the mean of finite observations, or None for an empty sample.</summary>
    let mean (values: seq<float>) =
#if !FABLE_COMPILER
        match box values with
        | :? (float array) as arr ->
            let struct(allFinite, count, lo, hi) = SimdNumeric.scanArrayMinMax arr
            if count = 0 then None
            else
                let maxAbs = max (abs lo) (abs hi)
                if allFinite && maxAbs <= 1e290 then
                    let mutable total = 0.
                    let mutable correction = 0.
                    for i = 0 to arr.Length - 1 do
                        let value = arr[i]
                        let next = total + value
                        correction <- correction + (if abs total >= abs value then (total - next) + value else (value - next) + total)
                        total <- next
                    let result = (total + correction) / float count
                    Check.finite "values" result
                    Some result
                else
                    let invN = 1. / float count
                    let mutable total = 0.
                    let mutable correction = 0.
                    for i = 0 to arr.Length - 1 do
                        let raw = arr[i]
                        if Double.IsFinite raw then
                            let value = if maxAbs <= 1e290 then raw * invN else raw / float count
                            let next = total + value
                            correction <- correction + (if abs total >= abs value then (total - next) + value else (value - next) + total)
                            total <- next
                    let result = total + correction
                    Check.finite "values" result
                    Some result
        | :? (float list) as lst ->
            let mutable count = 0
            let mutable maxAbs = 0.
            for v in lst do
                if Double.IsFinite v then
                    count <- count + 1
                    let av = abs v
                    if av > maxAbs then maxAbs <- av
            if count = 0 then None
            elif maxAbs <= 1e290 then
                let mutable total = 0.
                let mutable correction = 0.
                for value in lst do
                    if Double.IsFinite value then
                        let next = total + value
                        correction <- correction + (if abs total >= abs value then (total - next) + value else (value - next) + total)
                        total <- next
                let result = (total + correction) / float count
                Check.finite "values" result
                Some result
            else
                let denom = float count
                let mutable total = 0.
                let mutable correction = 0.
                for raw in lst do
                    if Double.IsFinite raw then
                        let value = raw / denom
                        let next = total + value
                        correction <- correction + (if abs total >= abs value then (total - next) + value else (value - next) + total)
                        total <- next
                let result = total + correction
                Check.finite "values" result
                Some result
        | _ ->
#endif
            let finite = Quickselect.copyFinite values
            if finite.Length = 0 then None
            else
                let denom = float finite.Length
                let mutable total = 0.
                let mutable correction = 0.
                for i = 0 to finite.Length - 1 do
                    let value = finite[i] / denom
                    let next = total + value
                    Check.finite "values" next
                    correction <- correction + (if abs total >= abs value then (total - next) + value else (value - next) + total)
                    total <- next
                let result = total + correction
                Check.finite "values" result
                Some result

    /// <summary>Computes a linearly interpolated quantile from a sorted copy; probability must be in [0,1].</summary>
    let quantile probability values =
        Check.finite "probability" probability
        if probability < 0. || probability > 1. then invalidArg "probability" "Expected a probability in [0,1]."
        let work = Quickselect.copyFinite values
        if work.Length = 0 then None
        else
            let position = probability * float (work.Length - 1)
            let lower = int (floor position)
            let upper = min (lower + 1) (work.Length - 1)
            Quickselect.selectAdjacentPair work lower upper
            Some (Check.lerp work[lower] work[upper] (position - float lower))

    /// <summary>Groups observations by an ordered key, preserving observation order within each group.</summary>
    let groupBy key values =
        values |> Seq.fold (fun groups value ->
            let k = key value
            Map.change k (fun previous -> Some (value :: Option.defaultValue [] previous)) groups) Map.empty
        |> Map.map (fun _ -> List.rev)

    /// <summary>Returns the insertion index to the right of equal values in an ascending array.</summary>
    let bisectRight (ordered: float array) value =
        let mutable low, high = 0, ordered.Length
        while low < high do
            let middle = low + (high - low) / 2
            if value < ordered[middle] then high <- middle else low <- middle + 1
        low

    let internal resizeToList (buf: ResizeArray<'T>) : 'T list =
        let mutable acc = []
        for j = buf.Count - 1 downto 0 do
            acc <- buf[j] :: acc
        acc

    /// <summary>Bins finite observations within strictly increasing boundaries. The final upper edge is inclusive.</summary>
    let bin project boundaries values =
        let edges = List.toArray boundaries
        if edges.Length < 2 then invalidArg "boundaries" "At least two boundaries are required."
        for i = 0 to edges.Length - 1 do
            Check.finite "boundaries" edges[i]
        for i = 1 to edges.Length - 1 do
            if edges[i] <= edges[i - 1] then invalidArg "boundaries" "Boundaries must increase strictly."
        let numBins = edges.Length - 1
        let buckets = Array.init numBins (fun _ -> ResizeArray<_>())
        let loEdge = edges[0]
        let hiEdge = edges[numBins]
        for value in values do
            let x = project value
            if Double.IsFinite x && x >= loEdge && x <= hiEdge then
                let index = min (numBins - 1) (bisectRight edges x - 1)
                buckets[index].Add value
        let mutable out = []
        for i = numBins - 1 downto 0 do
            out <- { Lower = edges[i]; Upper = edges[i + 1]; Values = resizeToList buckets[i] } :: out
        out

    /// <summary>Computes a keyed join in O(n) expected time; duplicate keys in either input reject the join.</summary>
    let join key (previous: 'T list) (next: 'T list) =
        let prevLen = List.length previous
        let old = Dictionary<'Key, struct(int * 'T)>(prevLen)
        let mutable pIdx = 0
        for value in previous do
            let k = key value
#if FABLE_COMPILER
            if old.ContainsKey k then invalidArg "previous" "Keys must be unique."
            old.Add(k, struct(pIdx, value))
#else
            if not (old.TryAdd(k, struct(pIdx, value))) then invalidArg "previous" "Keys must be unique."
#endif
            pIdx <- pIdx + 1
        let mutable enterSeen: HashSet<'Key> = null
        let enter = ResizeArray<'Key * int * 'T>()
        let update = ResizeArray<'Key * int * int * 'T * 'T>(min prevLen (List.length next))
        let mutable nIdx = 0
        for value in next do
            let k = key value
            match old.TryGetValue k with
            | true, struct(oldIndex, oldValue) ->
                if oldIndex < 0 then invalidArg "next" "Keys must be unique."
                old[k] <- struct(-oldIndex - 1, oldValue)
                update.Add(k, oldIndex, nIdx, oldValue, value)
            | false, _ ->
                if isNull enterSeen then enterSeen <- HashSet<'Key>()
                if not (enterSeen.Add k) then invalidArg "next" "Keys must be unique."
                enter.Add(k, nIdx, value)
            nIdx <- nIdx + 1
        let exits =
            if update.Count = prevLen then []
            else
                let exitBuf = ResizeArray<'Key * int * 'T>(prevLen - update.Count)
                let mutable i = 0
                for value in previous do
                    let k = key value
                    let struct(markedIdx, _) = old[k]
                    if markedIdx >= 0 then
                        exitBuf.Add(k, i, value)
                    i <- i + 1
                resizeToList exitBuf
        { Enter = resizeToList enter; Update = resizeToList update; Exit = exits }

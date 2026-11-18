namespace Ranvier.D5

open System

/// <summary>How histogram chooses its bin count. Count must be in [1,10000]; Scott and FreedmanDiaconis fall back to Sturges when their spread
/// estimate is zero.</summary>
[<RequireQualifiedAccess>]
type BinRule = Sturges | Scott | FreedmanDiaconis | Count of int

/// <summary>Sample statistics, cumulative sums, grouping, bisection and histograms. Numeric summaries ignore NaN and infinite observations.</summary>
[<RequireQualifiedAccess>]
module Statistics =
    let private extractFiniteWithBounds (values: seq<float>) : struct(float array * float * float) =
#if !FABLE_COMPILER
        match box values with
        | :? (float array) as arr ->
            let struct(allFinite, count, lo, hi) = SimdNumeric.scanArrayMinMax arr
            if count = 0 then struct([||], Double.PositiveInfinity, Double.NegativeInfinity)
            elif allFinite then struct(arr, lo, hi)
            else
                let res = Array.zeroCreate<float> count
                let mutable w = 0
                for i = 0 to arr.Length - 1 do
                    let v = arr[i]
                    if Double.IsFinite v then
                        res[w] <- v
                        w <- w + 1
                struct(res, lo, hi)
        | _ ->
#endif
            let data = Quickselect.copyFinite values
            if data.Length = 0 then struct([||], Double.PositiveInfinity, Double.NegativeInfinity)
            else
                let mutable lo = data[0]
                let mutable hi = data[0]
                for i = 1 to data.Length - 1 do
                    let v = data[i]
                    if v < lo then lo <- v
                    if v > hi then hi <- v
                struct(data, lo, hi)

    let private welfordVariance (data: float array) (magnitude: float) =
        let mutable mean = 0.
        let mutable squared = 0.
        for i = 0 to data.Length - 1 do
            let value = data[i] / magnitude
            let delta = value - mean
            mean <- mean + delta / float (i + 1)
            squared <- squared + delta * (value - mean)
        squared

    /// <summary>Sample variance of finite observations; missing/nonfinite observations are ignored, as in Data summaries. Unrepresentable results reject.</summary>
    let variance values =
        let struct(data, lo, hi) = extractFiniteWithBounds values
        if data.Length < 2 then None
        else
            let magnitude = max (abs lo) (abs hi)
            if magnitude = 0. then Some 0.
            else
                let squared =
#if !FABLE_COMPILER
                    let invMag = 1. / magnitude
                    if Double.IsFinite invMag then
                        SimdNumeric.scaledCenteredSumSquares data invMag
                    else
                        welfordVariance data magnitude
#else
                    welfordVariance data magnitude
#endif
                let result = (squared / float (data.Length - 1) * magnitude) * magnitude
                Check.finite "values" result
                Some result

    /// <summary>Returns the compensated running total after each observation; a non-finite observation adds nothing but still yields an entry.
    /// Overflow raises ArgumentException.</summary>
    let cumulativeSum values =
        let mutable total = 0.
        let mutable correction = 0.
        [for value in values do
            if Double.IsFinite value then
                let next = total + value
                Check.finite "values" next
                correction <- correction + (if abs total >= abs value then (total - next) + value else (value - next) + total)
                total <- next
            let result = total + correction
            Check.finite "values" result
            yield result]

    /// <summary>Groups values by key in first-occurrence order and reduces each group, preserving value order within the group.</summary>
    let rollup key reduce values = values |> Seq.groupBy key |> Seq.map (fun (key, items) -> key, reduce items) |> Seq.toList

    /// <summary>Insertion before equal values in an ascending array.</summary>
    let bisectLeft value (values: float array) =
        Check.finite "value" value
        let mutable lo = 0
        let mutable hi = values.Length
        while lo < hi do
            let mid = lo + (hi - lo) / 2
            if values[mid] < value then lo <- mid + 1 else hi <- mid
        lo

    /// <summary>Nearest ascending-array observation; exact distance ties choose the right observation.</summary>
    let bisectNearest value (values: float array) =
        let i = bisectLeft value values
        if values.Length = 0 then None
        elif i = 0 then Some 0
        elif i = values.Length then Some(i - 1)
        else
            let magnitude = max (abs value) (max (abs values[i - 1]) (abs values[i]))
            let left = if magnitude = 0. then 0. else value / magnitude - values[i - 1] / magnitude
            let right = if magnitude = 0. then 0. else values[i] / magnitude - value / magnitude
            Some(if left < right then i - 1 else i)

    /// <summary>Bounded equal-width bins. Zero spread yields one closed bin; zero IQR falls back to Sturges. Nonfinite observations are missing.</summary>
    let histogram rule values =
        match rule with BinRule.Count n when n < 1 || n > 10000 -> invalidArg "rule" "Bin count must be in [1,10000]." | _ -> ()
        let struct(data, lo, hi) = extractFiniteWithBounds values
        if data.Length = 0 then []
        elif lo = hi then [{ Lower = lo; Upper = hi; Values = Array.toList data }]
        else
            let magnitude = max (abs lo) (abs hi)
            let span = hi / magnitude - lo / magnitude
            let sturges () = ceil (Math.Log2(float data.Length)) + 1.
            let count =
                match rule with
                | BinRule.Count n -> float n
                | BinRule.Sturges -> sturges ()
                | BinRule.Scott ->
                    let invMag = 1. / magnitude
                    let sd =
                        if data.Length < 2 then 0.
                        else
                            let sq =
#if !FABLE_COMPILER
                                if Double.IsFinite invMag then SimdNumeric.scaledCenteredSumSquares data invMag
                                else welfordVariance data magnitude
#else
                                welfordVariance data magnitude
#endif
                            sqrt (sq / float (data.Length - 1))
                    if sd = 0. then sturges () else ceil (span / (3.49 * sd * Math.Pow(float data.Length, -1. / 3.)))
                | BinRule.FreedmanDiaconis ->
                    let work = Array.copy data
                    let n = work.Length
                    let idx25 = 0.25 * float (n - 1)
                    let l25 = int (floor idx25)
                    let u25 = min (l25 + 1) (n - 1)
                    Quickselect.selectAdjacentPair work l25 u25
                    let q25 = Check.lerp (work[l25] / magnitude) (work[u25] / magnitude) (idx25 - float l25)
                    let idx75 = 0.75 * float (n - 1)
                    let l75 = int (floor idx75)
                    let u75 = min (l75 + 1) (n - 1)
                    if l75 > u25 then
                        Quickselect.selectInPlace work (u25 + 1) (n - 1) l75
                        if u75 > l75 && u75 < n then
                            let mutable mn = work[l75 + 1]
                            for k = l75 + 2 to n - 1 do
                                if work[k] < mn then mn <- work[k]
                            work[u75] <- mn
                    elif l75 = u25 && u75 > u25 && u75 < n then
                        let mutable mn = work[u25 + 1]
                        for k = u25 + 2 to n - 1 do
                            if work[k] < mn then mn <- work[k]
                        work[u75] <- mn
                    let q75 = Check.lerp (work[l75] / magnitude) (work[u75] / magnitude) (idx75 - float l75)
                    let iqr = q75 - q25
                    if iqr = 0. then sturges () else ceil (span / (2. * iqr * Math.Pow(float data.Length, -1. / 3.)))
            let count = int (min 10000. (max 1. count))
            let rawEdges = Array.zeroCreate<float> (count + 1)
            let mutable edgeLen = 0
            for i = 0 to count do
                let e = Check.lerp lo hi (float i / float count)
                if edgeLen = 0 || e > rawEdges[edgeLen - 1] then
                    rawEdges[edgeLen] <- e
                    edgeLen <- edgeLen + 1
            if edgeLen < 2 then
                [{ Lower = lo; Upper = hi; Values = Array.toList data }]
            else
                let numBins = edgeLen - 1
                let buckets = Array.init numBins (fun _ -> ResizeArray<float>())
                let rawSpan = hi - lo
                if edgeLen = count + 1 && Double.IsFinite rawSpan && rawSpan > 0. then
                    let scale = float numBins / rawSpan
                    for i = 0 to data.Length - 1 do
                        let x = data[i]
                        if x >= rawEdges[0] && x <= rawEdges[numBins] then
                            let mutable idx = int ((x - lo) * scale)
                            if idx < 0 then idx <- 0
                            elif idx >= numBins then idx <- numBins - 1
                            while idx > 0 && x < rawEdges[idx] do idx <- idx - 1
                            while idx < numBins - 1 && x >= rawEdges[idx + 1] do idx <- idx + 1
                            buckets[idx].Add x
                else
                    let edgesTrimmed = if edgeLen = rawEdges.Length then rawEdges else Array.sub rawEdges 0 edgeLen
                    for i = 0 to data.Length - 1 do
                        let x = data[i]
                        if x >= edgesTrimmed[0] && x <= edgesTrimmed[numBins] then
                            let idx = min (numBins - 1) (Data.bisectRight edgesTrimmed x - 1)
                            buckets[idx].Add x
                let mutable out = []
                for i = numBins - 1 downto 0 do
                    out <- { Lower = rawEdges[i]; Upper = rawEdges[i + 1]; Values = Data.resizeToList buckets[i] } :: out
                out

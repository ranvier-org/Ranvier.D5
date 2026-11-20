namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif
open Ranvier.D5

/// <summary>High-dimensional pairwise dissimilarity metric used by <c>BallTree</c>, <c>NnDescent</c>, <c>Umap2D</c>, <c>Tsne2D</c>, <c>Isomap2D</c> and <c>Sammon2D</c>.</summary>
[<RequireQualifiedAccess>]
type DistanceMetric =
    /// <summary>Standard L2 Euclidean distance <c>sqrt(sum_k (x_k - y_k)^2)</c>.</summary>
    | Euclidean
    /// <summary>Cosine dissimilarity <c>1 - (x . y) / (||x||_2 * ||y||_2)</c> in <c>[0, 2]</c>.</summary>
    | Cosine
    /// <summary>City-block L1 Manhattan distance <c>sum_k |x_k - y_k|</c>.</summary>
    | Manhattan
    /// <summary>Pearson correlation dissimilarity <c>1 - corr(x, y)</c> in <c>[0, 2]</c>.</summary>
    | Correlation

/// <summary>An immutable high-dimensional observation pairing a unique identifier with its finite feature coordinates.</summary>
type FeatureRow<'Id> =
    { /// <summary>Unique observation identifier.</summary>
      Id: 'Id
      /// <summary>Finite feature coordinates of uniform dimensionality.</summary>
      Features: float list }

/// <summary>An opaque, validated high-dimensional dataset stored as a contiguous row-major matrix for SIMD neighborhood and manifold kernels.</summary>
type FeatureDataset<'Id> =
    private
        { Ids: 'Id[]
          Dim: int
          Data: float[] }

/// <summary>Deterministic 32-bit Xoshiro128++ PRNG producing identical streams across .NET 10 and Fable JavaScript without 64-bit BigInt overhead.</summary>
type internal FastRng32(seed: uint32) =
    static let mix32 (z0: uint32) : uint32 =
        let mutable z = z0 + 0x9E3779B9u
        z <- (z ^^^ (z >>> 16)) * 0x85EBCA6Bu
        z <- (z ^^^ (z >>> 13)) * 0xC2B2AE35u
        z ^^^ (z >>> 16)

    static let rotl (x: uint32) (k: int) : uint32 =
        (x <<< k) ||| (x >>> (32 - k))

    let mutable s0 = mix32 seed
    let mutable s1 = mix32 (seed + 0x9E3779B9u)
    let mutable s2 = mix32 (seed + 0x3C6EF372u)
    let mutable s3 =
        let raw = mix32 (seed + 0xDAA66D2Bu)
        if (s0 ||| s1 ||| s2 ||| raw) = 0u then 1u else raw

    member _.NextUInt32() : uint32 =
        let res = rotl (s0 + s3) 7 + s0
        let t = s1 <<< 9
        s2 <- s2 ^^^ s0
        s3 <- s3 ^^^ s1
        s1 <- s1 ^^^ s2
        s0 <- s0 ^^^ s3
        s2 <- s2 ^^^ t
        s3 <- rotl s3 11
        res

    member this.NextBounded(bound: int) : int =
        if bound <= 1 then 0
        else int (this.NextUInt32() % uint32 bound)

    member this.NextFloat() : float =
        float (this.NextUInt32()) * (1.0 / 4294967296.0)

    member this.NextSignedJitter(scale: float) : float =
        (this.NextFloat() * 2.0 - 1.0) * scale

/// <summary>SIMD-accelerated row-pair distance and matrix-free linear algebra kernels on .NET 10 with exact scalar fallbacks under Fable.</summary>
module internal ManifoldSimd =
    let private euclideanScaledFallback (a: float[]) (offA: int) (b: float[]) (offB: int) (dim: int) : float =
        let mutable maxAbs = 0.0
        for k = 0 to dim - 1 do
            let d = abs (a[offA + k] - b[offB + k])
            if d > maxAbs then maxAbs <- d
        if maxAbs = 0.0 then 0.0
        elif Double.IsPositiveInfinity maxAbs then infinity
        else
            let inv = 1.0 / maxAbs
            let mutable s = 0.0
            for k = 0 to dim - 1 do
                let r = (a[offA + k] - b[offB + k]) * inv
                s <- s + r * r
            maxAbs * sqrt s

    let euclideanAt (a: float[]) (offA: int) (b: float[]) (offB: int) (dim: int) : float =
        if dim <= 0 then 0.0
        else
#if FABLE_COMPILER
            let mutable sumSq = 0.0
            for k = 0 to dim - 1 do
                let diff = a[offA + k] - b[offB + k]
                sumSq <- sumSq + diff * diff
            if Double.IsPositiveInfinity sumSq then euclideanScaledFallback a offA b offB dim
            else sqrt (max 0.0 sumSq)
#else
            let ra = &MemoryMarshal.GetArrayDataReference(a)
            let rb = &MemoryMarshal.GetArrayDataReference(b)
            let uOffA = uint32 offA
            let uOffB = uint32 offB
            let uDim = uint32 dim
            let mutable k = 0u
            let mutable sumSq = 0.0
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let mutable acc = Vector512<float>.Zero
                while k + step <= uDim do
                    let va = Vector512.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector512.LoadUnsafe(&rb, unativeint (uOffB + k))
                    let d = va - vb
                    acc <- acc + d * d
                    k <- k + step
                sumSq <- Vector512.Sum(acc)
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let mutable acc = Vector256<float>.Zero
                while k + step <= uDim do
                    let va = Vector256.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector256.LoadUnsafe(&rb, unativeint (uOffB + k))
                    let d = va - vb
                    acc <- acc + d * d
                    k <- k + step
                sumSq <- Vector256.Sum(acc)
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let mutable acc = Vector128<float>.Zero
                while k + step <= uDim do
                    let va = Vector128.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector128.LoadUnsafe(&rb, unativeint (uOffB + k))
                    let d = va - vb
                    acc <- acc + d * d
                    k <- k + step
                sumSq <- Vector128.Sum(acc)
            let mutable idx = int k
            while idx < dim do
                let diff = a[offA + idx] - b[offB + idx]
                sumSq <- sumSq + diff * diff
                idx <- idx + 1
            if Double.IsPositiveInfinity sumSq then euclideanScaledFallback a offA b offB dim
            else sqrt (max 0.0 sumSq)
#endif

    let manhattanAt (a: float[]) (offA: int) (b: float[]) (offB: int) (dim: int) : float =
        if dim <= 0 then 0.0
        else
#if FABLE_COMPILER
            let mutable sumAbs = 0.0
            for k = 0 to dim - 1 do
                sumAbs <- sumAbs + abs (a[offA + k] - b[offB + k])
            sumAbs
#else
            let ra = &MemoryMarshal.GetArrayDataReference(a)
            let rb = &MemoryMarshal.GetArrayDataReference(b)
            let uOffA = uint32 offA
            let uOffB = uint32 offB
            let uDim = uint32 dim
            let mutable k = 0u
            let mutable sumAbs = 0.0
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let mutable acc = Vector512<float>.Zero
                while k + step <= uDim do
                    let va = Vector512.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector512.LoadUnsafe(&rb, unativeint (uOffB + k))
                    acc <- acc + Vector512.Abs(va - vb)
                    k <- k + step
                sumAbs <- Vector512.Sum(acc)
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let mutable acc = Vector256<float>.Zero
                while k + step <= uDim do
                    let va = Vector256.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector256.LoadUnsafe(&rb, unativeint (uOffB + k))
                    acc <- acc + Vector256.Abs(va - vb)
                    k <- k + step
                sumAbs <- Vector256.Sum(acc)
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let mutable acc = Vector128<float>.Zero
                while k + step <= uDim do
                    let va = Vector128.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector128.LoadUnsafe(&rb, unativeint (uOffB + k))
                    acc <- acc + Vector128.Abs(va - vb)
                    k <- k + step
                sumAbs <- Vector128.Sum(acc)
            let mutable idx = int k
            while idx < dim do
                sumAbs <- sumAbs + abs (a[offA + idx] - b[offB + idx])
                idx <- idx + 1
            sumAbs
#endif

    let cosineAt (a: float[]) (offA: int) (b: float[]) (offB: int) (dim: int) : float =
        if dim <= 0 then 0.0
        else
#if FABLE_COMPILER
            let mutable dotAB = 0.0
            let mutable normA = 0.0
            let mutable normB = 0.0
            for k = 0 to dim - 1 do
                let xa = a[offA + k]
                let xb = b[offB + k]
                dotAB <- dotAB + xa * xb
                normA <- normA + xa * xa
                normB <- normB + xb * xb
#else
            let ra = &MemoryMarshal.GetArrayDataReference(a)
            let rb = &MemoryMarshal.GetArrayDataReference(b)
            let uOffA = uint32 offA
            let uOffB = uint32 offB
            let uDim = uint32 dim
            let mutable k = 0u
            let mutable dotAB = 0.0
            let mutable normA = 0.0
            let mutable normB = 0.0
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let mutable accDot = Vector512<float>.Zero
                let mutable accA = Vector512<float>.Zero
                let mutable accB = Vector512<float>.Zero
                while k + step <= uDim do
                    let va = Vector512.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector512.LoadUnsafe(&rb, unativeint (uOffB + k))
                    accDot <- accDot + va * vb
                    accA <- accA + va * va
                    accB <- accB + vb * vb
                    k <- k + step
                dotAB <- Vector512.Sum(accDot)
                normA <- Vector512.Sum(accA)
                normB <- Vector512.Sum(accB)
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let mutable accDot = Vector256<float>.Zero
                let mutable accA = Vector256<float>.Zero
                let mutable accB = Vector256<float>.Zero
                while k + step <= uDim do
                    let va = Vector256.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector256.LoadUnsafe(&rb, unativeint (uOffB + k))
                    accDot <- accDot + va * vb
                    accA <- accA + va * va
                    accB <- accB + vb * vb
                    k <- k + step
                dotAB <- Vector256.Sum(accDot)
                normA <- Vector256.Sum(accA)
                normB <- Vector256.Sum(accB)
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let mutable accDot = Vector128<float>.Zero
                let mutable accA = Vector128<float>.Zero
                let mutable accB = Vector128<float>.Zero
                while k + step <= uDim do
                    let va = Vector128.LoadUnsafe(&ra, unativeint (uOffA + k))
                    let vb = Vector128.LoadUnsafe(&rb, unativeint (uOffB + k))
                    accDot <- accDot + va * vb
                    accA <- accA + va * va
                    accB <- accB + vb * vb
                    k <- k + step
                dotAB <- Vector128.Sum(accDot)
                normA <- Vector128.Sum(accA)
                normB <- Vector128.Sum(accB)
            let mutable idx = int k
            while idx < dim do
                let xa = a[offA + idx]
                let xb = b[offB + idx]
                dotAB <- dotAB + xa * xb
                normA <- normA + xa * xa
                normB <- normB + xb * xb
                idx <- idx + 1
#endif
            if normA <= 1e-300 && normB <= 1e-300 then 0.0
            elif normA <= 1e-300 || normB <= 1e-300 then 1.0
            else
                let denom = sqrt normA * sqrt normB
                if denom <= 0.0 then 1.0
                else max 0.0 (min 2.0 (1.0 - dotAB / denom))

    let correlationAt (a: float[]) (offA: int) (b: float[]) (offB: int) (dim: int) : float =
        if dim <= 1 then 0.0
        else
            let invDim = 1.0 / float dim
#if FABLE_COMPILER
            let mutable sumA = 0.0
            let mutable sumB = 0.0
            for k = 0 to dim - 1 do
                sumA <- sumA + a[offA + k]
                sumB <- sumB + b[offB + k]
            let meanA = sumA * invDim
            let meanB = sumB * invDim
            let mutable cov = 0.0
            let mutable varA = 0.0
            let mutable varB = 0.0
            for k = 0 to dim - 1 do
                let da = a[offA + k] - meanA
                let db = b[offB + k] - meanB
                cov <- cov + da * db
                varA <- varA + da * da
                varB <- varB + db * db
#else
            let ra = &MemoryMarshal.GetArrayDataReference(a)
            let rb = &MemoryMarshal.GetArrayDataReference(b)
            let uOffA = uint32 offA
            let uOffB = uint32 offB
            let uDim = uint32 dim
            let mutable k1 = 0u
            let mutable sumA = 0.0
            let mutable sumB = 0.0
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let mutable accSA = Vector512<float>.Zero
                let mutable accSB = Vector512<float>.Zero
                while k1 + step <= uDim do
                    accSA <- accSA + Vector512.LoadUnsafe(&ra, unativeint (uOffA + k1))
                    accSB <- accSB + Vector512.LoadUnsafe(&rb, unativeint (uOffB + k1))
                    k1 <- k1 + step
                sumA <- Vector512.Sum(accSA)
                sumB <- Vector512.Sum(accSB)
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let mutable accSA = Vector256<float>.Zero
                let mutable accSB = Vector256<float>.Zero
                while k1 + step <= uDim do
                    accSA <- accSA + Vector256.LoadUnsafe(&ra, unativeint (uOffA + k1))
                    accSB <- accSB + Vector256.LoadUnsafe(&rb, unativeint (uOffB + k1))
                    k1 <- k1 + step
                sumA <- Vector256.Sum(accSA)
                sumB <- Vector256.Sum(accSB)
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let mutable accSA = Vector128<float>.Zero
                let mutable accSB = Vector128<float>.Zero
                while k1 + step <= uDim do
                    accSA <- accSA + Vector128.LoadUnsafe(&ra, unativeint (uOffA + k1))
                    accSB <- accSB + Vector128.LoadUnsafe(&rb, unativeint (uOffB + k1))
                    k1 <- k1 + step
                sumA <- Vector128.Sum(accSA)
                sumB <- Vector128.Sum(accSB)
            let mutable idx1 = int k1
            while idx1 < dim do
                sumA <- sumA + a[offA + idx1]
                sumB <- sumB + b[offB + idx1]
                idx1 <- idx1 + 1

            let meanA = sumA * invDim
            let meanB = sumB * invDim
            let mutable k2 = 0u
            let mutable cov = 0.0
            let mutable varA = 0.0
            let mutable varB = 0.0
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let vmA = Vector512.Create(meanA)
                let vmB = Vector512.Create(meanB)
                let mutable accCov = Vector512<float>.Zero
                let mutable accVA = Vector512<float>.Zero
                let mutable accVB = Vector512<float>.Zero
                while k2 + step <= uDim do
                    let da = Vector512.LoadUnsafe(&ra, unativeint (uOffA + k2)) - vmA
                    let db = Vector512.LoadUnsafe(&rb, unativeint (uOffB + k2)) - vmB
                    accCov <- accCov + da * db
                    accVA <- accVA + da * da
                    accVB <- accVB + db * db
                    k2 <- k2 + step
                cov <- Vector512.Sum(accCov)
                varA <- Vector512.Sum(accVA)
                varB <- Vector512.Sum(accVB)
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let vmA = Vector256.Create(meanA)
                let vmB = Vector256.Create(meanB)
                let mutable accCov = Vector256<float>.Zero
                let mutable accVA = Vector256<float>.Zero
                let mutable accVB = Vector256<float>.Zero
                while k2 + step <= uDim do
                    let da = Vector256.LoadUnsafe(&ra, unativeint (uOffA + k2)) - vmA
                    let db = Vector256.LoadUnsafe(&rb, unativeint (uOffB + k2)) - vmB
                    accCov <- accCov + da * db
                    accVA <- accVA + da * da
                    accVB <- accVB + db * db
                    k2 <- k2 + step
                cov <- Vector256.Sum(accCov)
                varA <- Vector256.Sum(accVA)
                varB <- Vector256.Sum(accVB)
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let vmA = Vector128.Create(meanA)
                let vmB = Vector128.Create(meanB)
                let mutable accCov = Vector128<float>.Zero
                let mutable accVA = Vector128<float>.Zero
                let mutable accVB = Vector128<float>.Zero
                while k2 + step <= uDim do
                    let da = Vector128.LoadUnsafe(&ra, unativeint (uOffA + k2)) - vmA
                    let db = Vector128.LoadUnsafe(&rb, unativeint (uOffB + k2)) - vmB
                    accCov <- accCov + da * db
                    accVA <- accVA + da * da
                    accVB <- accVB + db * db
                    k2 <- k2 + step
                cov <- Vector128.Sum(accCov)
                varA <- Vector128.Sum(accVA)
                varB <- Vector128.Sum(accVB)
            let mutable idx2 = int k2
            while idx2 < dim do
                let da = a[offA + idx2] - meanA
                let db = b[offB + idx2] - meanB
                cov <- cov + da * db
                varA <- varA + da * da
                varB <- varB + db * db
                idx2 <- idx2 + 1
#endif
            if varA <= 1e-300 && varB <= 1e-300 then 0.0
            elif varA <= 1e-300 || varB <= 1e-300 then 1.0
            else
                let denom = sqrt varA * sqrt varB
                if denom <= 0.0 then 1.0
                else max 0.0 (min 2.0 (1.0 - cov / denom))

    let distanceAt (metric: DistanceMetric) (a: float[]) (offA: int) (b: float[]) (offB: int) (dim: int) : float =
        match metric with
        | DistanceMetric.Euclidean -> euclideanAt a offA b offB dim
        | DistanceMetric.Cosine -> cosineAt a offA b offB dim
        | DistanceMetric.Manhattan -> manhattanAt a offA b offB dim
        | DistanceMetric.Correlation -> correlationAt a offA b offB dim

    let inline rowDistance (metric: DistanceMetric) (data: float[]) (dim: int) (i: int) (j: int) : float =
        if i = j then 0.0
        else distanceAt metric data (i * dim) data (j * dim) dim

    let dotRowVec (data: float[]) (offA: int) (vec: float[]) (dim: int) : float =
        if dim <= 0 then 0.0
        else
#if FABLE_COMPILER
            let mutable sum = 0.0
            for k = 0 to dim - 1 do
                sum <- sum + data[offA + k] * vec[k]
            sum
#else
            let rd = &MemoryMarshal.GetArrayDataReference(data)
            let rv = &MemoryMarshal.GetArrayDataReference(vec)
            let uOff = uint32 offA
            let uDim = uint32 dim
            let mutable k = 0u
            let mutable sum = 0.0
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let mutable acc = Vector512<float>.Zero
                while k + step <= uDim do
                    acc <- acc + Vector512.LoadUnsafe(&rd, unativeint (uOff + k)) * Vector512.LoadUnsafe(&rv, unativeint k)
                    k <- k + step
                sum <- Vector512.Sum(acc)
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let mutable acc = Vector256<float>.Zero
                while k + step <= uDim do
                    acc <- acc + Vector256.LoadUnsafe(&rd, unativeint (uOff + k)) * Vector256.LoadUnsafe(&rv, unativeint k)
                    k <- k + step
                sum <- Vector256.Sum(acc)
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let mutable acc = Vector128<float>.Zero
                while k + step <= uDim do
                    acc <- acc + Vector128.LoadUnsafe(&rd, unativeint (uOff + k)) * Vector128.LoadUnsafe(&rv, unativeint k)
                    k <- k + step
                sum <- Vector128.Sum(acc)
            let mutable idx = int k
            while idx < dim do
                sum <- sum + data[offA + idx] * vec[idx]
                idx <- idx + 1
            sum
#endif

    let axpyRowToVec (alpha: float) (data: float[]) (offA: int) (dst: float[]) (dim: int) : unit =
        if dim > 0 && alpha <> 0.0 then
#if FABLE_COMPILER
            for k = 0 to dim - 1 do
                dst[k] <- dst[k] + alpha * data[offA + k]
#else
            let rd = &MemoryMarshal.GetArrayDataReference(data)
            let ry = &MemoryMarshal.GetArrayDataReference(dst)
            let uOff = uint32 offA
            let uDim = uint32 dim
            let mutable k = 0u
            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let va = Vector512.Create(alpha)
                while k + step <= uDim do
                    let vx = Vector512.LoadUnsafe(&rd, unativeint (uOff + k))
                    let vy = Vector512.LoadUnsafe(&ry, unativeint k)
                    (vy + va * vx).StoreUnsafe(&ry, unativeint k)
                    k <- k + step
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let va = Vector256.Create(alpha)
                while k + step <= uDim do
                    let vx = Vector256.LoadUnsafe(&rd, unativeint (uOff + k))
                    let vy = Vector256.LoadUnsafe(&ry, unativeint k)
                    (vy + va * vx).StoreUnsafe(&ry, unativeint k)
                    k <- k + step
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let va = Vector128.Create(alpha)
                while k + step <= uDim do
                    let vx = Vector128.LoadUnsafe(&rd, unativeint (uOff + k))
                    let vy = Vector128.LoadUnsafe(&ry, unativeint k)
                    (vy + va * vx).StoreUnsafe(&ry, unativeint k)
                    k <- k + step
            let mutable idx = int k
            while idx < dim do
                dst[idx] <- dst[idx] + alpha * data[offA + idx]
                idx <- idx + 1
#endif

    /// <summary>Sorts <c>(dist, idx)</c> slices in ascending order of distance, breaking ties by smaller index (Fable &amp; .NET identical).</summary>
    let sortDistIdxSlice (dists: float[]) (indices: int[]) (offset: int) (len: int) : unit =
        if len > 1 then
            let inline lessThan dA iA dB iB =
                if abs (dA - dB) > 1e-14 * max 1.0 (max (abs dA) (abs dB)) then dA < dB
                elif dA <> dB then dA < dB
                else iA < iB
            if len <= 32 then
                for a = offset + 1 to offset + len - 1 do
                    let kd = dists[a]
                    let ki = indices[a]
                    let mutable b = a - 1
                    while b >= offset && lessThan kd ki dists[b] indices[b] do
                        dists[b + 1] <- dists[b]
                        indices[b + 1] <- indices[b]
                        b <- b - 1
                    dists[b + 1] <- kd
                    indices[b + 1] <- ki
            else
                let mutable gap = len / 2
                while gap > 0 do
                    for a = offset + gap to offset + len - 1 do
                        let kd = dists[a]
                        let ki = indices[a]
                        let mutable b = a
                        while b >= offset + gap && lessThan kd ki dists[b - gap] indices[b - gap] do
                            dists[b] <- dists[b - gap]
                            indices[b] <- indices[b - gap]
                            b <- b - gap
                        dists[b] <- kd
                        indices[b] <- ki
                    gap <- gap / 2

    /// <summary>Sorts <c>(indices, weights)</c> slices in ascending order of target vertex index (Fable &amp; .NET identical).</summary>
    let sortIdxWeightSlice (indices: int[]) (weights: float[]) (offset: int) (len: int) : unit =
        if len > 1 then
            if len <= 32 then
                for a = offset + 1 to offset + len - 1 do
                    let ki = indices[a]
                    let kw = weights[a]
                    let mutable b = a - 1
                    while b >= offset && indices[b] > ki do
                        indices[b + 1] <- indices[b]
                        weights[b + 1] <- weights[b]
                        b <- b - 1
                    indices[b + 1] <- ki
                    weights[b + 1] <- kw
            else
                let mutable gap = len / 2
                while gap > 0 do
                    for a = offset + gap to offset + len - 1 do
                        let ki = indices[a]
                        let kw = weights[a]
                        let mutable b = a
                        while b >= offset + gap && indices[b - gap] > ki do
                            indices[b] <- indices[b - gap]
                            weights[b] <- weights[b - gap]
                            b <- b - gap
                        indices[b] <- ki
                        weights[b] <- kw
                    gap <- gap / 2

/// <summary>Validation, construction and inspection for contiguous SIMD-backed <c>FeatureDataset&lt;'Id&gt;</c> matrices.</summary>
[<RequireQualifiedAccess>]
module FeatureDataset =
    let internal raw (dataset: FeatureDataset<'Id>) = dataset.Ids, dataset.Dim, dataset.Data

    /// <summary>Validates non-empty uniform dimensionality, unique IDs and finite feature coordinates into a contiguous row-major dataset.</summary>
    let ofRows (rows: ('Id * float list) list) : Result<FeatureDataset<'Id>, LayoutError list> =
        match rows with
        | [] -> Error [ InvalidObservation "FeatureDataset requires at least one observation row." ]
        | (_, firstFeatures) :: _ ->
            let dim = List.length firstFeatures
            if dim <= 0 then
                Error [ InvalidObservation "FeatureDataset requires positive feature dimensionality." ]
            else
                let n = List.length rows
                if int64 n * int64 dim > 100_000_000L then
                    Error [ BudgetExceeded "FeatureDataset exceeds the 100,000,000-element matrix budget." ]
                else
                    let errors = ResizeArray<LayoutError>()
                    let mutable seen = Set.empty<'Id>
                    let mutable dimMismatch = false
                    let mutable nonFinite = false
                    let ids = Array.zeroCreate<'Id> n
                    let data = Array.zeroCreate<float> (n * dim)
                    let mutable rIdx = 0

                    for (obsId, feats) in rows do
                        if isNull (box obsId) then
                            errors.Add(InvalidObservation "Observation ID must not be null.")
                        elif Set.contains obsId seen then
                            errors.Add(DuplicateId(string (box obsId)))
                        else
                            seen <- Set.add obsId seen

                        ids[rIdx] <- obsId
                        let rowBase = rIdx * dim
                        let mutable cIdx = 0
                        for x in feats do
                            if not (Double.IsFinite x) then
                                nonFinite <- true
                            if cIdx < dim then
                                data[rowBase + cIdx] <- x
                            cIdx <- cIdx + 1
                        if cIdx <> dim then
                            dimMismatch <- true
                        rIdx <- rIdx + 1

                    if dimMismatch then
                        errors.Add(InvalidTopology "All feature rows in FeatureDataset must share identical dimensionality.")
                    if nonFinite then
                        errors.Add(InvalidObservation "All feature coordinates in FeatureDataset must be finite.")

                    if errors.Count > 0 then Error(List.ofSeq errors)
                    else Ok { Ids = ids; Dim = dim; Data = data }

    /// <summary>Constructs a validated <c>FeatureDataset&lt;'Id&gt;</c> from typed <c>FeatureRow&lt;'Id&gt;</c> records.</summary>
    let ofFeatureRows (rows: FeatureRow<'Id> list) : Result<FeatureDataset<'Id>, LayoutError list> =
        rows |> List.map (fun r -> r.Id, r.Features) |> ofRows

    /// <summary>Materializes all rows of the dataset as immutable <c>FeatureRow&lt;'Id&gt;</c> records in input order.</summary>
    let toFeatureRows (dataset: FeatureDataset<'Id>) : FeatureRow<'Id> list =
        let n = dataset.Ids.Length
        let dim = dataset.Dim
        let data = dataset.Data
        [ for i in 0 .. n - 1 ->
            let rowBase = i * dim
            { Id = dataset.Ids[i]
              Features = [ for k in 0 .. dim - 1 -> data[rowBase + k] ] } ]

    /// <summary>Returns the number of observation rows in the dataset.</summary>
    let count (dataset: FeatureDataset<'Id>) : int = dataset.Ids.Length

    /// <summary>Returns the uniform feature dimensionality of each observation.</summary>
    let dimension (dataset: FeatureDataset<'Id>) : int = dataset.Dim

    /// <summary>Returns the observation identifiers in dataset order.</summary>
    let ids (dataset: FeatureDataset<'Id>) : 'Id list = Array.toList dataset.Ids

type internal BallTreeData<'Id> =
    { Ids: 'Id[]
      OrigDim: int
      OrigData: float[]
      Metric: DistanceMetric
      TreeDim: int
      TreeData: float[]
      Centers: float[]
      Radii: float[]
      LeftChild: int[]
      RightChild: int[]
      ItemStart: int[]
      ItemCount: int[]
      Perm: int[]
      NodeCount: int }

/// <summary>An opaque, immutable Omohundro (1989) metric ball tree supporting exact branch-and-bound k-nearest-neighbor search in arbitrary dimension.</summary>
type BallTree<'Id> = private BallTree of BallTreeData<'Id>

/// <summary>Omohundro (1989) two-pivot farthest-point diameter ball tree construction and exact branch-and-bound k-NN queries.</summary>
[<RequireQualifiedAccess>]
module BallTree =
    let private embedQueryForTree (metric: DistanceMetric) (origDim: int) (qOrig: float[]) : float[] =
        match metric with
        | DistanceMetric.Euclidean
        | DistanceMetric.Manhattan -> qOrig
        | DistanceMetric.Cosine ->
            let out = Array.zeroCreate<float> (origDim + 1)
            let mutable n2 = 0.0
            for k = 0 to origDim - 1 do
                let v = qOrig[k]
                n2 <- n2 + v * v
            if n2 > 1e-300 then
                let inv = 1.0 / sqrt n2
                for k = 0 to origDim - 1 do out[k] <- qOrig[k] * inv
            else
                out[origDim] <- 1.0
            out
        | DistanceMetric.Correlation ->
            let out = Array.zeroCreate<float> (origDim + 1)
            if origDim <= 1 then
                out[origDim] <- 1.0
                out
            else
                let mutable s = 0.0
                for k = 0 to origDim - 1 do s <- s + qOrig[k]
                let mean = s / float origDim
                let mutable v2 = 0.0
                for k = 0 to origDim - 1 do
                    let d = qOrig[k] - mean
                    out[k] <- d
                    v2 <- v2 + d * d
                if v2 > 1e-300 then
                    let inv = 1.0 / sqrt v2
                    for k = 0 to origDim - 1 do out[k] <- out[k] * inv
                else
                    for k = 0 to origDim - 1 do out[k] <- 0.0
                    out[origDim] <- 1.0
                out

    let private buildTreeSpace (metric: DistanceMetric) (n: int) (origDim: int) (origData: float[]) : int * float[] =
        match metric with
        | DistanceMetric.Euclidean
        | DistanceMetric.Manhattan -> origDim, origData
        | DistanceMetric.Cosine ->
            let tDim = origDim + 1
            let tData = Array.zeroCreate<float> (n * tDim)
            for i = 0 to n - 1 do
                let srcBase = i * origDim
                let dstBase = i * tDim
                let mutable n2 = 0.0
                for k = 0 to origDim - 1 do
                    let v = origData[srcBase + k]
                    n2 <- n2 + v * v
                if n2 > 1e-300 then
                    let inv = 1.0 / sqrt n2
                    for k = 0 to origDim - 1 do
                        tData[dstBase + k] <- origData[srcBase + k] * inv
                else
                    tData[dstBase + origDim] <- 1.0
            tDim, tData
        | DistanceMetric.Correlation ->
            let tDim = origDim + 1
            let tData = Array.zeroCreate<float> (n * tDim)
            for i = 0 to n - 1 do
                let srcBase = i * origDim
                let dstBase = i * tDim
                if origDim <= 1 then
                    tData[dstBase + origDim] <- 1.0
                else
                    let mutable s = 0.0
                    for k = 0 to origDim - 1 do s <- s + origData[srcBase + k]
                    let mean = s / float origDim
                    let mutable v2 = 0.0
                    for k = 0 to origDim - 1 do
                        let d = origData[srcBase + k] - mean
                        tData[dstBase + k] <- d
                        v2 <- v2 + d * d
                    if v2 > 1e-300 then
                        let inv = 1.0 / sqrt v2
                        for k = 0 to origDim - 1 do
                            tData[dstBase + k] <- tData[dstBase + k] * inv
                    else
                        for k = 0 to origDim - 1 do tData[dstBase + k] <- 0.0
                        tData[dstBase + origDim] <- 1.0
            tDim, tData

    let inline private treeMetricDist (metric: DistanceMetric) (a: float[]) (offA: int) (b: float[]) (offB: int) (treeDim: int) : float =
        match metric with
        | DistanceMetric.Manhattan -> ManifoldSimd.manhattanAt a offA b offB treeDim
        | DistanceMetric.Euclidean
        | DistanceMetric.Cosine
        | DistanceMetric.Correlation -> ManifoldSimd.euclideanAt a offA b offB treeDim

    /// <summary>Quickselects <c>perm[offset .. offset + len - 1]</c> with parallel <c>proj[0 .. len - 1]</c> so the median sits at <c>midIdx = len / 2</c>.</summary>
    let private partitionMedianInPlace (perm: int[]) (proj: float[]) (offset: int) (len: int) : unit =
        let target = len / 2
        let mutable lo = 0
        let mutable hi = len - 1
        let inline lessAt a b =
            let pa, pb = proj[a], proj[b]
            if pa <> pb then pa < pb else perm[offset + a] < perm[offset + b]
        let inline swapLocal a b =
            if a <> b then
                let tp = proj[a]
                proj[a] <- proj[b]
                proj[b] <- tp
                let ti = perm[offset + a]
                perm[offset + a] <- perm[offset + b]
                perm[offset + b] <- ti

        while lo < hi do
            let mid = lo + (hi - lo) / 2
            if lessAt mid lo then swapLocal lo mid
            if lessAt hi lo then swapLocal lo hi
            if lessAt hi mid then swapLocal mid hi
            swapLocal mid (hi - 1)
            let pivVal = proj[hi - 1]
            let pivIdx = perm[offset + hi - 1]
            let mutable i = lo
            for j = lo to hi - 2 do
                let pj = proj[j]
                let ij = perm[offset + j]
                if (if pj <> pivVal then pj < pivVal else ij < pivIdx) then
                    swapLocal i j
                    i <- i + 1
            swapLocal i (hi - 1)
            if i = target then
                lo <- hi
            elif i < target then
                lo <- i + 1
            else
                hi <- i - 1

    /// <summary>Builds an immutable Omohundro (1989) ball tree with cooperative cancellation.</summary>
    let buildWithCancellation
        (token: System.Threading.CancellationToken)
        (metric: DistanceMetric)
        (dataset: FeatureDataset<'Id>)
        : BallTree<'Id> =
        token.ThrowIfCancellationRequested()
        let ids, origDim, origData = FeatureDataset.raw dataset
        let n = ids.Length
        let treeDim, treeData = buildTreeSpace metric n origDim origData
        let leafSize = 16
        let initCap = max 4 (min (max 4 (2 * n)) (max 8 ((n / 4) * 2 + 8)))
        let mutable centers = Array.zeroCreate<float> (initCap * treeDim)
        let mutable radii = Array.zeroCreate<float> initCap
        let mutable leftChild = Array.create initCap -1
        let mutable rightChild = Array.create initCap -1
        let mutable itemStart = Array.zeroCreate<int> initCap
        let mutable itemCount = Array.zeroCreate<int> initCap
        let perm = Array.init n id
        let dirBuf = Array.zeroCreate<float> treeDim
        let projBuf = Array.zeroCreate<float> n
        let mutable nodeCount = 1

        let ensureCap (needed: int) =
            if radii.Length < needed then
                let nextCap = max needed (radii.Length * 2)
                let nextCenters = Array.zeroCreate<float> (nextCap * treeDim)
                Array.Copy(centers, 0, nextCenters, 0, nodeCount * treeDim)
                centers <- nextCenters
                let nextRadii = Array.zeroCreate<float> nextCap
                Array.Copy(radii, 0, nextRadii, 0, nodeCount)
                radii <- nextRadii
                let nextL = Array.create nextCap -1
                Array.Copy(leftChild, 0, nextL, 0, nodeCount)
                leftChild <- nextL
                let nextR = Array.create nextCap -1
                Array.Copy(rightChild, 0, nextR, 0, nodeCount)
                rightChild <- nextR
                let nextIS = Array.zeroCreate<int> nextCap
                Array.Copy(itemStart, 0, nextIS, 0, nodeCount)
                itemStart <- nextIS
                let nextIC = Array.zeroCreate<int> nextCap
                Array.Copy(itemCount, 0, nextIC, 0, nodeCount)
                itemCount <- nextIC

        let rec buildNode (slot: int) (depth: int) (offset: int) (len: int) =
            SimdMath.check token slot
            let cOff = slot * treeDim
            Array.fill dirBuf 0 treeDim 0.0
            let invLen = 1.0 / float len
            for k = 0 to len - 1 do
                let idx = perm[offset + k]
                ManifoldSimd.axpyRowToVec invLen treeData (idx * treeDim) dirBuf treeDim
            Array.blit dirBuf 0 centers cOff treeDim

            let seedIdx = perm[offset]
            let seedOff = seedIdx * treeDim
            let mutable maxRad = 0.0
            let mutable p1 = seedIdx
            let mutable farthestFromSeed = -1.0
            for k = 0 to len - 1 do
                let idx = perm[offset + k]
                let rOff = idx * treeDim
                let dCenter = treeMetricDist metric treeData rOff centers cOff treeDim
                if dCenter > maxRad then maxRad <- dCenter
                let dSeed = treeMetricDist metric treeData rOff treeData seedOff treeDim
                if dSeed > farthestFromSeed then
                    farthestFromSeed <- dSeed
                    p1 <- idx

            radii[slot] <- maxRad
            itemStart[slot] <- offset

            if len <= leafSize || depth >= 48 || maxRad <= 1e-15 then
                leftChild[slot] <- -1
                rightChild[slot] <- -1
                itemCount[slot] <- len
            else
                let p1Off = p1 * treeDim
                let mutable p2 = p1
                let mutable diameter = -1.0
                for k = 0 to len - 1 do
                    let idx = perm[offset + k]
                    let d1 = treeMetricDist metric treeData (idx * treeDim) treeData p1Off treeDim
                    if d1 > diameter then
                        diameter <- d1
                        p2 <- idx

                if p1 = p2 || diameter <= 1e-15 then
                    leftChild[slot] <- -1
                    rightChild[slot] <- -1
                    itemCount[slot] <- len
                else
                    let p2Off = p2 * treeDim
                    for d = 0 to treeDim - 1 do
                        dirBuf[d] <- treeData[p2Off + d] - treeData[p1Off + d]
                    let p1Proj = ManifoldSimd.dotRowVec treeData p1Off dirBuf treeDim
                    for k = 0 to len - 1 do
                        let idx = perm[offset + k]
                        projBuf[k] <- ManifoldSimd.dotRowVec treeData (idx * treeDim) dirBuf treeDim - p1Proj

                    partitionMedianInPlace perm projBuf offset len
                    let leftLen = len / 2
                    let rightLen = len - leftLen
                    let lSlot = nodeCount
                    let rSlot = nodeCount + 1
                    ensureCap (nodeCount + 2)
                    nodeCount <- nodeCount + 2
                    leftChild[slot] <- lSlot
                    rightChild[slot] <- rSlot
                    itemCount[slot] <- 0
                    buildNode lSlot (depth + 1) offset leftLen
                    buildNode rSlot (depth + 1) (offset + leftLen) rightLen

        if n > 0 then
            buildNode 0 0 0 n

        BallTree
            { Ids = ids
              OrigDim = origDim
              OrigData = origData
              Metric = metric
              TreeDim = treeDim
              TreeData = treeData
              Centers = if centers.Length = nodeCount * treeDim then centers else Array.sub centers 0 (nodeCount * treeDim)
              Radii = if radii.Length = nodeCount then radii else Array.sub radii 0 nodeCount
              LeftChild = if leftChild.Length = nodeCount then leftChild else Array.sub leftChild 0 nodeCount
              RightChild = if rightChild.Length = nodeCount then rightChild else Array.sub rightChild 0 nodeCount
              ItemStart = if itemStart.Length = nodeCount then itemStart else Array.sub itemStart 0 nodeCount
              ItemCount = if itemCount.Length = nodeCount then itemCount else Array.sub itemCount 0 nodeCount
              Perm = perm
              NodeCount = nodeCount }

    /// <summary>Builds an immutable Omohundro (1989) ball tree over <c>dataset</c> using <c>metric</c>.</summary>
    let build (metric: DistanceMetric) (dataset: FeatureDataset<'Id>) : BallTree<'Id> =
        buildWithCancellation System.Threading.CancellationToken.None metric dataset

    /// <summary>Returns the number of indexed observations in the ball tree.</summary>
    let count (BallTree d) : int = d.Ids.Length

    /// <summary>Returns the feature dimensionality expected by queries on this ball tree.</summary>
    let dimension (BallTree d) : int = d.OrigDim

    /// <summary>Returns the distance metric under which the ball tree was constructed.</summary>
    let metric (BallTree d) : DistanceMetric = d.Metric

    let inline private worseThan dA iA dB iB =
        if abs (dA - dB) > 1e-14 * max 1.0 (max (abs dA) (abs dB)) then dA > dB
        elif dA <> dB then dA > dB
        else iA > iB

    let private heapPushBounded (heapDist: float[]) (heapIdx: int[]) (cap: int) (curCount: int) (candDist: float) (candIdx: int) : int =
        if curCount < cap then
            let mutable pos = curCount
            heapDist[pos] <- candDist
            heapIdx[pos] <- candIdx
            while pos > 0 do
                let parent = (pos - 1) / 2
                if worseThan heapDist[pos] heapIdx[pos] heapDist[parent] heapIdx[parent] then
                    let td, ti = heapDist[pos], heapIdx[pos]
                    heapDist[pos] <- heapDist[parent]
                    heapIdx[pos] <- heapIdx[parent]
                    heapDist[parent] <- td
                    heapIdx[parent] <- ti
                    pos <- parent
                else
                    pos <- 0
            curCount + 1
        elif worseThan heapDist[0] heapIdx[0] candDist candIdx then
            heapDist[0] <- candDist
            heapIdx[0] <- candIdx
            let mutable pos = 0
            let mutable cont = true
            while cont do
                let l = 2 * pos + 1
                let r = l + 1
                let mutable largest = pos
                if l < cap && worseThan heapDist[l] heapIdx[l] heapDist[largest] heapIdx[largest] then
                    largest <- l
                if r < cap && worseThan heapDist[r] heapIdx[r] heapDist[largest] heapIdx[largest] then
                    largest <- r
                if largest = pos then
                    cont <- false
                else
                    let td, ti = heapDist[pos], heapIdx[pos]
                    heapDist[pos] <- heapDist[largest]
                    heapIdx[pos] <- heapIdx[largest]
                    heapDist[largest] <- td
                    heapIdx[largest] <- ti
                    pos <- largest
            cap
        else
            cap

    let inline private treeThresholdForBest (metric: DistanceMetric) (bestTrueDist: float) : float =
        match metric with
        | DistanceMetric.Euclidean
        | DistanceMetric.Manhattan -> bestTrueDist
        | DistanceMetric.Cosine
        | DistanceMetric.Correlation -> sqrt (2.0 * max 0.0 bestTrueDist)

    /// <summary>Finds the exact <c>k</c> nearest observations to <c>query</c> sorted by ascending distance, pruning subtrees when <c>d(q, c_v) - r_v &gt;= d_k^best</c>.</summary>
    let kNearest (k: int) (query: float list) (BallTree d) : Result<('Id * float) list, LayoutError list> =
        if k <= 0 then
            Error [ InvalidObservation "BallTree.kNearest requires k >= 1." ]
        else
            let qLen = List.length query
            if qLen <> d.OrigDim then
                Error [ InvalidTopology "Query feature dimensionality must match BallTree dataset dimensionality." ]
            else
                let qOrig = Array.zeroCreate<float> d.OrigDim
                let mutable qi = 0
                let mutable anyNonFinite = false
                for v in query do
                    if not (Double.IsFinite v) then anyNonFinite <- true
                    qOrig[qi] <- v
                    qi <- qi + 1
                if anyNonFinite then
                    Error [ InvalidObservation "Query feature coordinates must be finite." ]
                elif d.Ids.Length = 0 || d.NodeCount = 0 then
                    Ok []
                else
                    let kEff = min k d.Ids.Length
                    let qTree = embedQueryForTree d.Metric d.OrigDim qOrig
                    let heapDist = Array.zeroCreate<float> kEff
                    let heapIdx = Array.zeroCreate<int> kEff
                    let mutable found = 0
                    let mutable stackNode = Array.zeroCreate<int> 128
                    let mutable stackLower = Array.zeroCreate<float> 128
                    let rootDist = treeMetricDist d.Metric qTree 0 d.Centers 0 d.TreeDim
                    stackNode[0] <- 0
                    stackLower[0] <- max 0.0 (rootDist - d.Radii[0])
                    let mutable sp = 1

                    while sp > 0 do
                        sp <- sp - 1
                        let slot = stackNode[sp]
                        let lowerBound = stackLower[sp]
                        let canPrune =
                            if found < kEff then false
                            else
                                let tau = treeThresholdForBest d.Metric heapDist[0]
                                lowerBound > tau * (1.0 + 1e-12) + 1e-15
                        if not canPrune then
                            let ic = d.ItemCount[slot]
                            if ic > 0 then
                                let start = d.ItemStart[slot]
                                for s = start to start + ic - 1 do
                                    let idx = d.Perm[s]
                                    let dist = ManifoldSimd.distanceAt d.Metric qOrig 0 d.OrigData (idx * d.OrigDim) d.OrigDim
                                    found <- heapPushBounded heapDist heapIdx kEff found dist idx
                            else
                                let lc = d.LeftChild[slot]
                                let rc = d.RightChild[slot]
                                if lc >= 0 && rc >= 0 then
                                    let dl = max 0.0 (treeMetricDist d.Metric qTree 0 d.Centers (lc * d.TreeDim) d.TreeDim - d.Radii[lc])
                                    let dr = max 0.0 (treeMetricDist d.Metric qTree 0 d.Centers (rc * d.TreeDim) d.TreeDim - d.Radii[rc])
                                    if sp + 2 > stackNode.Length then
                                        let grownN = Array.zeroCreate<int> (stackNode.Length * 2)
                                        let grownL = Array.zeroCreate<float> (stackLower.Length * 2)
                                        Array.Copy(stackNode, 0, grownN, 0, sp)
                                        Array.Copy(stackLower, 0, grownL, 0, sp)
                                        stackNode <- grownN
                                        stackLower <- grownL
                                    // Push farther child first so closer child is visited immediately (maximizing pruning).
                                    if dl <= dr then
                                        stackNode[sp] <- rc; stackLower[sp] <- dr; sp <- sp + 1
                                        stackNode[sp] <- lc; stackLower[sp] <- dl; sp <- sp + 1
                                    else
                                        stackNode[sp] <- lc; stackLower[sp] <- dl; sp <- sp + 1
                                        stackNode[sp] <- rc; stackLower[sp] <- dr; sp <- sp + 1

                    ManifoldSimd.sortDistIdxSlice heapDist heapIdx 0 found
                    Ok [ for r in 0 .. found - 1 -> d.Ids[heapIdx[r]], heapDist[r] ]

/// <summary>A directed k-nearest-neighbor graph edge pairing <c>Source</c> with one of its nearest <c>Target</c> observations and their metric distance.</summary>
type NeighborLink<'Id> =
    { /// <summary>Query observation identifier.</summary>
      Source: 'Id
      /// <summary>Nearest-neighbor observation identifier.</summary>
      Target: 'Id
      /// <summary>Nonnegative dissimilarity from <c>Source</c> to <c>Target</c>.</summary>
      Distance: float }

/// <summary>Dong, Moses &amp; Li (2011) NN-Descent approximate/exact k-nearest-neighbor graph builder.</summary>
[<RequireQualifiedAccess>]
module NnDescent =
    let inline private worseThan dA iA dB iB =
        if abs (dA - dB) > 1e-14 * max 1.0 (max (abs dA) (abs dB)) then dA > dB
        elif dA <> dB then dA > dB
        else iA > iB

    let private tryInsertHeap
        (heapIdx: int[])
        (heapDist: float[])
        (heapNew: bool[])
        (kWork: int)
        (row: int)
        (candIdx: int)
        (candDist: float)
        (isNew: bool)
        : int =
        if row = candIdx then 0
        else
            let baseOff = row * kWork
            let rootIdx = heapIdx[baseOff]
            let rootDist = heapDist[baseOff]
            let beatsRoot =
                if rootIdx < 0 then true
                else worseThan rootDist rootIdx candDist candIdx
            if not beatsRoot then 0
            else
                let mutable dup = false
                let mutable s = 0
                while s < kWork && not dup do
                    if heapIdx[baseOff + s] = candIdx then dup <- true
                    s <- s + 1
                if dup then 0
                else
                    heapIdx[baseOff] <- candIdx
                    heapDist[baseOff] <- candDist
                    heapNew[baseOff] <- isNew
                    let mutable pos = 0
                    let mutable cont = true
                    while cont do
                        let l = 2 * pos + 1
                        let r = l + 1
                        let mutable worst = pos
                        let inline slotWorse a b =
                            let ia, ib = heapIdx[baseOff + a], heapIdx[baseOff + b]
                            if ia < 0 && ib < 0 then a < b
                            elif ia < 0 then true
                            elif ib < 0 then false
                            else worseThan heapDist[baseOff + a] ia heapDist[baseOff + b] ib
                        if l < kWork && slotWorse l worst then worst <- l
                        if r < kWork && slotWorse r worst then worst <- r
                        if worst = pos then
                            cont <- false
                        else
                            let ti = heapIdx[baseOff + pos]
                            let td = heapDist[baseOff + pos]
                            let tn = heapNew[baseOff + pos]
                            heapIdx[baseOff + pos] <- heapIdx[baseOff + worst]
                            heapDist[baseOff + pos] <- heapDist[baseOff + worst]
                            heapNew[baseOff + pos] <- heapNew[baseOff + worst]
                            heapIdx[baseOff + worst] <- ti
                            heapDist[baseOff + worst] <- td
                            heapNew[baseOff + worst] <- tn
                            pos <- worst
                    1

    let private seedWithRpTree
        (token: System.Threading.CancellationToken)
        (metric: DistanceMetric)
        (n: int)
        (dim: int)
        (data: float[])
        (rng: FastRng32)
        (heapIdx: int[])
        (heapDist: float[])
        (heapNew: bool[])
        (kWork: int)
        : unit =
        let perm = Array.init n id
        let proj = Array.zeroCreate<float> n
        let dir = Array.zeroCreate<float> dim
        let leafCap = 20

        let rec splitSlice (depth: int) (offset: int) (len: int) =
            SimdMath.check token offset
            if len <= leafCap || depth >= 20 then
                for a = 0 to len - 2 do
                    let u = perm[offset + a]
                    for b = a + 1 to len - 1 do
                        let v = perm[offset + b]
                        let d = ManifoldSimd.rowDistance metric data dim u v
                        tryInsertHeap heapIdx heapDist heapNew kWork u v d true |> ignore
                        tryInsertHeap heapIdx heapDist heapNew kWork v u d true |> ignore
            else
                let r1 = perm[offset + rng.NextBounded(len)]
                let mutable r2 = perm[offset + rng.NextBounded(len)]
                if r1 = r2 then r2 <- perm[offset + ((rng.NextBounded(len - 1) + 1) % len)]
                let off1 = r1 * dim
                let off2 = r2 * dim
                let mutable normSq = 0.0
                for d = 0 to dim - 1 do
                    let delta = data[off2 + d] - data[off1 + d]
                    dir[d] <- delta
                    normSq <- normSq + delta * delta
                if normSq <= 1e-24 then
                    for a = 0 to len - 2 do
                        let u = perm[offset + a]
                        for b = a + 1 to min (len - 1) (a + kWork) do
                            let v = perm[offset + b]
                            let d = ManifoldSimd.rowDistance metric data dim u v
                            tryInsertHeap heapIdx heapDist heapNew kWork u v d true |> ignore
                            tryInsertHeap heapIdx heapDist heapNew kWork v u d true |> ignore
                else
                    let mutable sumProj = 0.0
                    for s = 0 to len - 1 do
                        let idx = perm[offset + s]
                        let p = ManifoldSimd.dotRowVec data (idx * dim) dir dim
                        proj[offset + s] <- p
                        sumProj <- sumProj + p
                    let meanProj = sumProj / float len
                    let mutable left = offset
                    let mutable right = offset + len - 1
                    while left <= right do
                        while left <= right && proj[left] <= meanProj do left <- left + 1
                        while left <= right && proj[right] > meanProj do right <- right - 1
                        if left < right then
                            let ti = perm[left]
                            perm[left] <- perm[right]
                            perm[right] <- ti
                            let tp = proj[left]
                            proj[left] <- proj[right]
                            proj[right] <- tp
                            left <- left + 1
                            right <- right - 1
                    let leftLen =
                        let raw = left - offset
                        if raw <= 0 || raw >= len then len / 2 else raw
                    splitSlice (depth + 1) offset leftLen
                    splitSlice (depth + 1) (offset + leftLen) (len - leftLen)

        splitSlice 0 0 n

    /// <summary>Builds sorted <c>(knnIdx, knnDist)</c> arrays of shape <c>n * kEff</c> for zero-copy sharing across 2D and 3D manifold engines.</summary>
    let internal buildIndexedKnn
        (token: System.Threading.CancellationToken)
        (metric: DistanceMetric)
        (k: int)
        (seed: uint32)
        (n: int)
        (dim: int)
        (data: float[])
        : int[] * float[] =
        token.ThrowIfCancellationRequested()
        if n <= 1 || k <= 0 then
            [||], [||]
        else
            let kEff = min k (n - 1)
            if n <= 256 then
                let heapIdx = Array.create (n * kEff) -1
                let heapDist = Array.create (n * kEff) infinity
                let dummyNew = Array.zeroCreate<bool> (n * kEff)
                for i = 0 to n - 2 do
                    SimdMath.check token i
                    for j = i + 1 to n - 1 do
                        let d = ManifoldSimd.rowDistance metric data dim i j
                        tryInsertHeap heapIdx heapDist dummyNew kEff i j d false |> ignore
                        tryInsertHeap heapIdx heapDist dummyNew kEff j i d false |> ignore
                for i = 0 to n - 1 do
                    ManifoldSimd.sortDistIdxSlice heapDist heapIdx (i * kEff) kEff
                heapIdx, heapDist
            else
                let kWork = min (n - 1) (max kEff 16)
                let heapIdx = Array.create (n * kWork) -1
                let heapDist = Array.create (n * kWork) infinity
                let heapNew = Array.zeroCreate<bool> (n * kWork)
                let rng = FastRng32(seed)

                // 1. Seed with 3 fast Random Projection Trees
                for _ = 0 to 2 do
                    token.ThrowIfCancellationRequested()
                    seedWithRpTree token metric n dim data rng heapIdx heapDist heapNew kWork

                // 2. Fill any remaining empty slots with distinct random neighbors
                for i = 0 to n - 1 do
                    SimdMath.check token i
                    let baseOff = i * kWork
                    let mutable attempts = 0
                    while heapIdx[baseOff] < 0 && attempts < kWork * 4 do
                        let mutable cand = rng.NextBounded(n - 1)
                        if cand >= i then cand <- cand + 1
                        let d = ManifoldSimd.rowDistance metric data dim i cand
                        tryInsertHeap heapIdx heapDist heapNew kWork i cand d true |> ignore
                        attempts <- attempts + 1

                // 3. Dong, Moses & Li (2011) iterative local join over new/old + reverse lists
                let maxSample = min 64 (kWork * 2)
                let newBuckets = Array.init n (fun _ -> ResizeArray<int>(maxSample))
                let oldBuckets = Array.init n (fun _ -> ResizeArray<int>(maxSample))
                let threshold = int (0.001 * float (n * kWork))
                let maxIter = 12
                let mutable iter = 0
                let mutable converged = false

                let inline addUniqueBounded (bucket: ResizeArray<int>) (cap: int) (v: int) =
                    if bucket.Count < cap then
                        let mutable exists = false
                        let mutable q = 0
                        while q < bucket.Count && not exists do
                            if bucket[q] = v then exists <- true
                            q <- q + 1
                        if not exists then bucket.Add v

                while iter < maxIter && not converged do
                    token.ThrowIfCancellationRequested()
                    for i = 0 to n - 1 do
                        newBuckets[i].Clear()
                        oldBuckets[i].Clear()

                    for i = 0 to n - 1 do
                        let baseOff = i * kWork
                        for s = 0 to kWork - 1 do
                            let j = heapIdx[baseOff + s]
                            if j >= 0 then
                                if heapNew[baseOff + s] then
                                    heapNew[baseOff + s] <- false
                                    addUniqueBounded newBuckets[i] maxSample j
                                    addUniqueBounded newBuckets[j] maxSample i
                                else
                                    addUniqueBounded oldBuckets[i] maxSample j
                                    addUniqueBounded oldBuckets[j] maxSample i

                    let mutable updates = 0
                    for v = 0 to n - 1 do
                        SimdMath.check token v
                        let nb = newBuckets[v]
                        let ob = oldBuckets[v]
                        let nCount = nb.Count
                        let oCount = ob.Count
                        for a = 0 to nCount - 1 do
                            let u = nb[a]
                            for b = a + 1 to nCount - 1 do
                                let w = nb[b]
                                if u <> w then
                                    let d = ManifoldSimd.rowDistance metric data dim u w
                                    updates <- updates + tryInsertHeap heapIdx heapDist heapNew kWork u w d true
                                    updates <- updates + tryInsertHeap heapIdx heapDist heapNew kWork w u d true
                            for b = 0 to oCount - 1 do
                                let w = ob[b]
                                if u <> w then
                                    let d = ManifoldSimd.rowDistance metric data dim u w
                                    updates <- updates + tryInsertHeap heapIdx heapDist heapNew kWork u w d true
                                    updates <- updates + tryInsertHeap heapIdx heapDist heapNew kWork w u d true

                    if updates <= threshold then converged <- true
                    iter <- iter + 1

                for i = 0 to n - 1 do
                    ManifoldSimd.sortDistIdxSlice heapDist heapIdx (i * kWork) kWork

                if kWork = kEff then
                    heapIdx, heapDist
                else
                    let outIdx = Array.zeroCreate<int> (n * kEff)
                    let outDist = Array.zeroCreate<float> (n * kEff)
                    for i = 0 to n - 1 do
                        let srcOff = i * kWork
                        let dstOff = i * kEff
                        for s = 0 to kEff - 1 do
                            outIdx[dstOff + s] <- heapIdx[srcOff + s]
                            outDist[dstOff + s] <- heapDist[srcOff + s]
                    outIdx, outDist

    /// <summary>Materializes directed <c>NeighborLink&lt;'Id&gt;</c> records from indexed k-NN arrays.</summary>
    let internal linksFromIndexedKnn (ids: 'Id[]) (kEff: int) (knnIdx: int[]) (knnDist: float[]) : NeighborLink<'Id> list =
        let n = ids.Length
        [ for i = 0 to n - 1 do
            let rowOff = i * kEff
            let srcId = ids[i]
            for s = 0 to kEff - 1 do
                let tgtIdx = knnIdx[rowOff + s]
                if tgtIdx >= 0 then
                    yield
                        { Source = srcId
                          Target = ids[tgtIdx]
                          Distance = knnDist[rowOff + s] } ]

    /// <summary>Builds a directed k-nearest-neighbor graph with cooperative cancellation.</summary>
    let buildGraphWithCancellation
        (token: System.Threading.CancellationToken)
        (metric: DistanceMetric)
        (k: int)
        (seed: uint32)
        (dataset: FeatureDataset<'Id>)
        : Result<NeighborLink<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if k < 1 then
            Error [ InvalidObservation "NnDescent.buildGraph requires k >= 1." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let n = ids.Length
            if n <= 1 then
                Ok []
            else
                let kEff = min k (n - 1)
                let knnIdx, knnDist = buildIndexedKnn token metric kEff seed n dim data
                Ok(linksFromIndexedKnn ids kEff knnIdx knnDist)

    /// <summary>Builds a directed k-nearest-neighbor graph using exact SIMD search for <c>N &lt;= 256</c> and Dong et al. (2011) NN-Descent for larger datasets.</summary>
    let buildGraph
        (metric: DistanceMetric)
        (k: int)
        (seed: uint32)
        (dataset: FeatureDataset<'Id>)
        : Result<NeighborLink<'Id> list, LayoutError list> =
        buildGraphWithCancellation System.Threading.CancellationToken.None metric k seed dataset

/// <summary>An observation embedded at a finite 2D <c>Point</c> coordinate.</summary>
type EmbeddedPoint2D<'Id> =
    { /// <summary>Original observation identifier.</summary>
      Id: 'Id
      /// <summary>2D embedded viewport coordinate.</summary>
      Position: Point }

/// <summary>Dimension-parametric manifold kernels (PCA, fuzzy simplicial sets, spectral initialization, UMAP SGD, t-SNE affinities, Isomap and Sammon) shared across 2D and 3D.</summary>
module internal ManifoldCore =
    /// <summary>Matrix-free centered SIMD power iteration extracting the top <c>dims</c> principal component coordinate arrays of length <c>n</c>.</summary>
    let pcaCoordinates
        (token: System.Threading.CancellationToken)
        (dims: int)
        (n: int)
        (dim: int)
        (data: float[])
        : float[][] =
        let coords = Array.init dims (fun _ -> Array.zeroCreate<float> n)
        if n <= 1 || dim <= 0 then coords
        else
            let colMean = Array.zeroCreate<float> dim
            let invN = 1.0 / float n
            for i = 0 to n - 1 do
                SimdMath.check token i
                ManifoldSimd.axpyRowToVec invN data (i * dim) colMean dim

            let foundAxes = ResizeArray<float[]>(dims)
            let vec = Array.zeroCreate<float> dim
            let nextVec = Array.zeroCreate<float> dim
            let rowProj = Array.zeroCreate<float> n

            let orthogonalize (v: float[]) =
                for prev in foundAxes do
                    let proj = SimdMath.dot v prev dim
                    for d = 0 to dim - 1 do
                        v[d] <- v[d] - proj * prev[d]

            let normalizeVec (v: float[]) : bool =
                let nrm = sqrt (SimdMath.sumSquares v dim)
                if nrm > 1e-13 then
                    SimdMath.scaleInPlace (1.0 / nrm) v dim
                    true
                else
                    false

            for axis = 0 to dims - 1 do
                token.ThrowIfCancellationRequested()
                for d = 0 to dim - 1 do
                    vec[d] <- sin (float ((d + 1) * (axis + 3)) * 1.618033988749895) + (if d = axis % dim then 1.0 else 0.0)
                orthogonalize vec
                if axis < dim && normalizeVec vec then
                    let mutable iter = 0
                    let mutable converged = false
                    while iter < 64 && not converged do
                        let muDotV = SimdMath.dot colMean vec dim
                        let mutable projSum = 0.0
                        for i = 0 to n - 1 do
                            let u = ManifoldSimd.dotRowVec data (i * dim) vec dim - muDotV
                            rowProj[i] <- u
                            projSum <- projSum + u
                        for d = 0 to dim - 1 do nextVec[d] <- 0.0
                        for i = 0 to n - 1 do
                            ManifoldSimd.axpyRowToVec rowProj[i] data (i * dim) nextVec dim
                        for d = 0 to dim - 1 do
                            nextVec[d] <- nextVec[d] - projSum * colMean[d]
                        orthogonalize nextVec
                        if not (normalizeVec nextVec) then
                            converged <- true
                        else
                            let mutable firstNz = 0.0
                            for d = 0 to dim - 1 do
                                if firstNz = 0.0 && abs nextVec[d] > 1e-9 then firstNz <- nextVec[d]
                            if firstNz < 0.0 then SimdMath.scaleInPlace -1.0 nextVec dim
                            let mutable diff = 0.0
                            for d = 0 to dim - 1 do
                                let delta = nextVec[d] - vec[d]
                                diff <- diff + delta * delta
                                vec[d] <- nextVec[d]
                            if diff < 1e-18 then converged <- true
                        iter <- iter + 1

                    let muDotV = SimdMath.dot colMean vec dim
                    let outC = coords[axis]
                    for i = 0 to n - 1 do
                        outC[i] <- ManifoldSimd.dotRowVec data (i * dim) vec dim - muDotV
                    SimdMath.zeroMeanInPlace outC n
                    let varSum = SimdMath.sumSquares outC n
                    if varSum > 1e-24 then
                        foundAxes.Add(Array.copy vec)
                    else
                        for i = 0 to n - 1 do
                            outC[i] <- 1e-3 * sin (float ((i + 1) * (axis + 2)) * 2.399963229728653)
                        SimdMath.zeroMeanInPlace outC n
                else
                    let outC = coords[axis]
                    for i = 0 to n - 1 do
                        outC[i] <- 1e-3 * sin (float ((i + 1) * (axis + 2)) * 2.399963229728653)
                    SimdMath.zeroMeanInPlace outC n
            coords

    /// <summary>Damped Levenberg-Marquardt curve fit for UMAP's smooth approximation <c>(1 + a * d^(2b))^(-1)</c> from <c>(minDist, spread)</c>.</summary>
    let fitUmapAb (minDist: float) (spread: float) : struct(float * float) =
        let mCount = 300
        let xs = Array.zeroCreate<float> mCount
        let ys = Array.zeroCreate<float> mCount
        let maxX = 3.0 * max 1e-4 spread
        for m = 0 to mCount - 1 do
            let x = maxX * float (m + 1) / float mCount
            xs[m] <- x
            ys[m] <- if x <= minDist then 1.0 else exp (-(x - minDist) / spread)

        let evalCost (a: float) (b: float) =
            let mutable s = 0.0
            for m = 0 to mCount - 1 do
                let p = Math.Pow(xs[m], 2.0 * b)
                let r = 1.0 / (1.0 + a * p) - ys[m]
                s <- s + r * r
            0.5 * s

        let mutable a = 1.0
        let mutable b = 1.0
        let mutable cost = evalCost a b
        let mutable lambda = 1e-3
        let mutable step = 0
        let mutable doneFit = false

        while step < 80 && not doneFit do
            let mutable j00, j01, j11 = 0.0, 0.0, 0.0
            let mutable g0, g1 = 0.0, 0.0
            for m = 0 to mCount - 1 do
                let x = xs[m]
                let lnX = log x
                let p = Math.Pow(x, 2.0 * b)
                let denom = 1.0 + a * p
                let invD = 1.0 / denom
                let invD2 = invD * invD
                let r = invD - ys[m]
                let ja = -p * invD2
                let jb = -2.0 * a * p * lnX * invD2
                j00 <- j00 + ja * ja
                j01 <- j01 + ja * jb
                j11 <- j11 + jb * jb
                g0 <- g0 + ja * r
                g1 <- g1 + jb * r

            let a00 = j00 + lambda * (1.0 + j00)
            let a11 = j11 + lambda * (1.0 + j11)
            let det = a00 * a11 - j01 * j01
            if abs det <= 1e-30 then
                doneFit <- true
            else
                let da = -(a11 * g0 - j01 * g1) / det
                let db = -(-j01 * g0 + a00 * g1) / det
                let candA = max 1e-6 (min 1e4 (a + da))
                let candB = max 1e-3 (min 20.0 (b + db))
                let candCost = evalCost candA candB
                if candCost < cost then
                    let relMove = abs (candA - a) + abs (candB - b)
                    a <- candA
                    b <- candB
                    cost <- candCost
                    lambda <- max 1e-9 (lambda * 0.5)
                    if relMove < 1e-11 then doneFit <- true
                else
                    lambda <- min 1e9 (lambda * 5.0)
            step <- step + 1

        struct(a, b)

    /// <summary>Builds smooth k-NN membership strengths and symmetric fuzzy simplicial set union <c>w_ij = p_i|j + p_j|i - p_i|j * p_j|i</c>.</summary>
    let buildFuzzySimplicialSet
        (token: System.Threading.CancellationToken)
        (n: int)
        (kEff: int)
        (targetNeighbors: int)
        (knnIdx: int[])
        (knnDist: float[])
        : int[] * int[] * float[] * int[] * int[] * float[] =
        let rho = Array.zeroCreate<float> n
        let sigma = Array.zeroCreate<float> n
        let effTargetK = min (max 2 targetNeighbors) (max 2 kEff)
        let targetVal = log (float effTargetK) / log 2.0

        let mutable totalDistSum = 0.0
        let totalEntries = n * kEff
        for idx = 0 to totalEntries - 1 do
            totalDistSum <- totalDistSum + knnDist[idx]
        let globalMeanDist = if totalEntries > 0 then totalDistSum / float totalEntries else 1.0

        for i = 0 to n - 1 do
            SimdMath.check token i
            let rowOff = i * kEff
            let mutable rhoI = infinity
            let mutable rowSum = 0.0
            for s = 0 to kEff - 1 do
                let d = knnDist[rowOff + s]
                rowSum <- rowSum + d
                if d > 0.0 && d < rhoI then rhoI <- d
            if Double.IsPositiveInfinity rhoI then rhoI <- 0.0
            rho[i] <- rhoI
            let rowMean = if kEff > 0 then rowSum / float kEff else globalMeanDist

            let mutable lo = 0.0
            let mutable hi = infinity
            let mutable mid = 1.0
            let mutable bStep = 0
            while bStep < 64 do
                let mutable psum = 0.0
                for s = 0 to kEff - 1 do
                    let d = knnDist[rowOff + s] - rhoI
                    if d > 0.0 then psum <- psum + exp (-d / mid)
                    else psum <- psum + 1.0
                if abs (psum - targetVal) < 1e-5 then
                    bStep <- 64
                else
                    if psum > targetVal then
                        hi <- mid
                        mid <- (lo + hi) * 0.5
                    else
                        lo <- mid
                        if Double.IsPositiveInfinity hi then mid <- mid * 2.0
                        else mid <- (lo + hi) * 0.5
                    bStep <- bStep + 1

            let minScale = 1e-3 * (if rhoI > 0.0 && rowMean > 0.0 then rowMean else max 1e-9 globalMeanDist)
            sigma[i] <- max 1e-12 (max minScale mid)

        // Directed probabilities p_{i|j} -> deduplicated undirected CSR + directed edge list
        let rawDeg = Array.zeroCreate<int> n
        let dirProb = Array.zeroCreate<float> totalEntries
        for i = 0 to n - 1 do
            let rowOff = i * kEff
            let rhoI = rho[i]
            let sigI = sigma[i]
            for s = 0 to kEff - 1 do
                let j = knnIdx[rowOff + s]
                if j >= 0 && j <> i then
                    let d = knnDist[rowOff + s]
                    let p = if d <= rhoI || sigI <= 0.0 then 1.0 else exp (-(d - rhoI) / sigI)
                    dirProb[rowOff + s] <- p
                    rawDeg[i] <- rawDeg[i] + 1
                    rawDeg[j] <- rawDeg[j] + 1

        // Lookup helper for p_{j|i} via small linear scan on row j's kEff slots
        let inline lookupProb (src: int) (dst: int) : float =
            let rOff = src * kEff
            let mutable foundP = 0.0
            let mutable s = 0
            while s < kEff do
                if knnIdx[rOff + s] = dst then
                    foundP <- dirProb[rOff + s]
                    s <- kEff
                else
                    s <- s + 1
            foundP

        let rawOffsets = Array.zeroCreate<int> (n + 1)
        for i = 0 to n - 1 do rawOffsets[i + 1] <- rawOffsets[i] + rawDeg[i]
        let rawTotal = rawOffsets[n]
        let rawTargets = Array.zeroCreate<int> rawTotal
        let rawWeights = Array.zeroCreate<float> rawTotal
        let cursor = Array.copy rawOffsets

        for i = 0 to n - 1 do
            let rowOff = i * kEff
            for s = 0 to kEff - 1 do
                let j = knnIdx[rowOff + s]
                if j >= 0 && j <> i then
                    let pij = dirProb[rowOff + s]
                    let pji = lookupProb j i
                    let w = pij + pji - pij * pji
                    let ci = cursor[i]
                    rawTargets[ci] <- j
                    rawWeights[ci] <- w
                    cursor[i] <- ci + 1
                    let cj = cursor[j]
                    rawTargets[cj] <- i
                    rawWeights[cj] <- w
                    cursor[j] <- cj + 1

        let undirOffsets = Array.zeroCreate<int> (n + 1)
        let mutable writePos = 0
        for i = 0 to n - 1 do
            undirOffsets[i] <- writePos
            let start = rawOffsets[i]
            let len = rawOffsets[i + 1] - start
            if len > 0 then
                ManifoldSimd.sortIdxWeightSlice rawTargets rawWeights start len
                let mutable prevT = rawTargets[start]
                let mutable maxW = rawWeights[start]
                for idx = start + 1 to start + len - 1 do
                    let curT = rawTargets[idx]
                    let curW = rawWeights[idx]
                    if curT = prevT then
                        if curW > maxW then maxW <- curW
                    else
                        if maxW > 0.0 then
                            rawTargets[writePos] <- prevT
                            rawWeights[writePos] <- maxW
                            writePos <- writePos + 1
                        prevT <- curT
                        maxW <- curW
                if maxW > 0.0 then
                    rawTargets[writePos] <- prevT
                    rawWeights[writePos] <- maxW
                    writePos <- writePos + 1
        undirOffsets[n] <- writePos

        let undirTargets = if writePos = rawTotal then rawTargets else Array.sub rawTargets 0 writePos
        let undirWeights = if writePos = rawTotal then rawWeights else Array.sub rawWeights 0 writePos
        let edgeHeads = Array.zeroCreate<int> writePos
        for i = 0 to n - 1 do
            for idx = undirOffsets[i] to undirOffsets[i + 1] - 1 do
                edgeHeads[idx] <- i

        edgeHeads, undirTargets, undirWeights, undirOffsets, undirTargets, undirWeights

    /// <summary>Normalized Laplacian spectral initialization when the fuzzy graph is connected, falling back to deterministic PCA on disconnected graphs.</summary>
    let spectralOrPcaInit
        (token: System.Threading.CancellationToken)
        (dims: int)
        (n: int)
        (dim: int)
        (data: float[])
        (undirOffsets: int[])
        (undirTargets: int[])
        (undirWeights: float[])
        (rng: FastRng32)
        : float[][] =
        let visited = Array.zeroCreate<bool> n
        let queue = Array.zeroCreate<int> n
        let mutable compCount = 0
        for s = 0 to n - 1 do
            if not visited[s] then
                compCount <- compCount + 1
                visited[s] <- true
                queue[0] <- s
                let mutable h = 0
                let mutable t = 1
                while h < t do
                    let u = queue[h]
                    h <- h + 1
                    for idx = undirOffsets[u] to undirOffsets[u + 1] - 1 do
                        let v = undirTargets[idx]
                        if not visited[v] then
                            visited[v] <- true
                            queue[t] <- v
                            t <- t + 1

        let scaleToTenWithJitter (coords: float[][]) =
            let mutable maxAbs = 0.0
            for c = 0 to dims - 1 do
                SimdMath.zeroMeanInPlace coords[c] n
                for i = 0 to n - 1 do
                    let a = abs (coords[c][i])
                    if a > maxAbs then maxAbs <- a
            let scale = if maxAbs > 1e-12 then 10.0 / maxAbs else 1.0
            for c = 0 to dims - 1 do
                let arr = coords[c]
                for i = 0 to n - 1 do
                    arr[i] <- arr[i] * scale + rng.NextSignedJitter(1e-4)
                SimdMath.zeroMeanInPlace arr n
            coords

        if compCount <> 1 || n <= dims + 1 then
            let pca = pcaCoordinates token dims n dim data
            scaleToTenWithJitter pca
        else
            let deg = Array.zeroCreate<float> n
            let invSqrtDeg = Array.zeroCreate<float> n
            let u0 = Array.zeroCreate<float> n
            let mutable degSum = 0.0
            for i = 0 to n - 1 do
                let mutable d = 0.0
                for idx = undirOffsets[i] to undirOffsets[i + 1] - 1 do
                    d <- d + undirWeights[idx]
                deg[i] <- d
                degSum <- degSum + d
                invSqrtDeg[i] <- if d > 1e-15 then 1.0 / sqrt d else 0.0
            let invSqrtTotal = if degSum > 1e-15 then 1.0 / sqrt degSum else 0.0
            for i = 0 to n - 1 do
                u0[i] <- sqrt (max 0.0 deg[i]) * invSqrtTotal

            // Top eigenvectors of M = I + D^(-1/2) W D^(-1/2) orthogonal to trivial eigenvector u0
            let foundU = ResizeArray<float[]>(dims + 1)
            foundU.Add u0
            let coords = Array.init dims (fun _ -> Array.zeroCreate<float> n)
            let cur = Array.zeroCreate<float> n
            let nxt = Array.zeroCreate<float> n
            let scaled = Array.zeroCreate<float> n
            let mutable spectralOk = true

            let orthogonalizeU (v: float[]) =
                for prev in foundU do
                    let proj = SimdMath.dot v prev n
                    SimdMath.axpy (-proj) prev v n

            let normalizeU (v: float[]) =
                let nrm = sqrt (SimdMath.sumSquares v n)
                if nrm > 1e-12 then
                    SimdMath.scaleInPlace (1.0 / nrm) v n
                    true
                else false

            for c = 0 to dims - 1 do
                if spectralOk then
                    token.ThrowIfCancellationRequested()
                    for i = 0 to n - 1 do
                        cur[i] <- sin (float ((i + 1) * (c + 2)) * 1.618033988749895) + cos (float ((i + 3) * (c + 5)) * 0.7548776662466927)
                    orthogonalizeU cur
                    if not (normalizeU cur) then
                        spectralOk <- false
                    else
                        let mutable it = 0
                        let mutable conv = false
                        while it < 180 && not conv do
                            for i = 0 to n - 1 do
                                scaled[i] <- cur[i] * invSqrtDeg[i]
                            for i = 0 to n - 1 do
                                let mutable acc = 0.0
                                for idx = undirOffsets[i] to undirOffsets[i + 1] - 1 do
                                    acc <- acc + undirWeights[idx] * scaled[undirTargets[idx]]
                                nxt[i] <- cur[i] + invSqrtDeg[i] * acc
                            orthogonalizeU nxt
                            if not (normalizeU nxt) then
                                spectralOk <- false
                                conv <- true
                            else
                                let mutable fnz = 0.0
                                for i = 0 to n - 1 do
                                    if fnz = 0.0 && abs nxt[i] > 1e-9 then fnz <- nxt[i]
                                if fnz < 0.0 then SimdMath.scaleInPlace -1.0 nxt n
                                let mutable diff = 0.0
                                for i = 0 to n - 1 do
                                    let dl = nxt[i] - cur[i]
                                    diff <- diff + dl * dl
                                    cur[i] <- nxt[i]
                                if diff < 1e-14 then conv <- true
                            it <- it + 1

                        if spectralOk then
                            foundU.Add(Array.copy cur)
                            let outC = coords[c]
                            for i = 0 to n - 1 do
                                outC[i] <- cur[i] * invSqrtDeg[i]
                            SimdMath.zeroMeanInPlace outC n
                            if SimdMath.sumSquares outC n <= 1e-24 then
                                spectralOk <- false

            if not spectralOk then
                let pca = pcaCoordinates token dims n dim data
                scaleToTenWithJitter pca
            else
                scaleToTenWithJitter coords

    /// <summary>Runs the dimension-generic McInnes et al. (2018) UMAP embedding pipeline into <c>dims</c> coordinates and returns the underlying indexed k-NN arrays.</summary>
    let embedUmapCoreWithKnn
        (token: System.Threading.CancellationToken)
        (dims: int)
        (metric: DistanceMetric)
        (neighbors: int)
        (minDist: float)
        (spread: float)
        (epochs: int)
        (negSampleRate: int)
        (learningRate: float)
        (seed: uint32)
        (n: int)
        (dim: int)
        (data: float[])
        : float[][] * int * int[] * float[] =
        if n <= 1 then
            Array.init dims (fun _ -> Array.zeroCreate<float> n), 0, [||], [||]
        else
            let kEff = min neighbors (n - 1)
            let knnIdx, knnDist = NnDescent.buildIndexedKnn token metric kEff seed n dim data
            let heads, tails, weights, undirOffsets, undirTargets, undirWeights =
                buildFuzzySimplicialSet token n kEff neighbors knnIdx knnDist
            let struct(aParam, bParam) = fitUmapAb minDist spread
            let rng = FastRng32(seed ^^^ 0xA5A55A5Au)
            let coords = spectralOrPcaInit token dims n dim data undirOffsets undirTargets undirWeights rng

            let eCount = weights.Length
            if eCount > 0 then
                let mutable wMax = 0.0
                for e = 0 to eCount - 1 do
                    if weights[e] > wMax then wMax <- weights[e]

                if wMax > 0.0 then
                    let epochsPerSample = Array.create eCount -1.0
                    let epochOfNextSample = Array.create eCount infinity
                    let epochsPerNeg = Array.create eCount infinity
                    let epochOfNextNeg = Array.create eCount infinity
                    let fEpochs = float epochs
                    let negRate = float (max 0 negSampleRate)

                    for e = 0 to eCount - 1 do
                        let nSamples = fEpochs * (weights[e] / wMax)
                        if nSamples >= 1.0 then
                            let eps = fEpochs / nSamples
                            epochsPerSample[e] <- eps
                            epochOfNextSample[e] <- eps
                            if negRate > 0.0 then
                                let epn = eps / negRate
                                epochsPerNeg[e] <- epn
                                epochOfNextNeg[e] <- epn

                    let inline clampGrad g =
                        if g > 4.0 then 4.0
                        elif g < -4.0 then -4.0
                        else g

                    for ep = 0 to epochs - 1 do
                        token.ThrowIfCancellationRequested()
                        let fEp = float ep
                        let alpha = learningRate * (1.0 - fEp / fEpochs)

                        for e = 0 to eCount - 1 do
                            if epochOfNextSample[e] <= fEp then
                                SimdMath.check token e
                                let j = heads[e]
                                let k = tails[e]
                                let mutable dSq = 0.0
                                for c = 0 to dims - 1 do
                                    let diff = coords[c][j] - coords[c][k]
                                    dSq <- dSq + diff * diff

                                let gradCoeffAttr =
                                    if dSq > 1e-12 then
                                        let powB = Math.Pow(dSq, bParam)
                                        let powBm1 = Math.Pow(dSq, bParam - 1.0)
                                        (-2.0 * aParam * bParam * powBm1) / (1.0 + aParam * powB)
                                    else
                                        0.0

                                if gradCoeffAttr <> 0.0 then
                                    for c = 0 to dims - 1 do
                                        let diff = coords[c][j] - coords[c][k]
                                        let stepD = alpha * clampGrad (gradCoeffAttr * diff)
                                        coords[c][j] <- coords[c][j] + stepD
                                        coords[c][k] <- coords[c][k] - stepD

                                epochOfNextSample[e] <- epochOfNextSample[e] + epochsPerSample[e]

                                if negSampleRate > 0 && Double.IsFinite epochsPerNeg[e] then
                                    let rawNeg = int ((fEp - epochOfNextNeg[e]) / epochsPerNeg[e])
                                    if rawNeg > 0 then
                                        let cappedNeg = min 32 rawNeg
                                        for _ = 0 to cappedNeg - 1 do
                                            let m = rng.NextBounded(n)
                                            if m <> j then
                                                let mutable dNegSq = 0.0
                                                for c = 0 to dims - 1 do
                                                    let diff = coords[c][j] - coords[c][m]
                                                    dNegSq <- dNegSq + diff * diff
                                                if dNegSq > 1e-12 then
                                                    let powB = Math.Pow(dNegSq, bParam)
                                                    let gradCoeffRep = (2.0 * bParam) / ((0.001 + dNegSq) * (1.0 + aParam * powB))
                                                    for c = 0 to dims - 1 do
                                                        let diff = coords[c][j] - coords[c][m]
                                                        coords[c][j] <- coords[c][j] + alpha * clampGrad (gradCoeffRep * diff)
                                                else
                                                    for c = 0 to dims - 1 do
                                                        coords[c][j] <- coords[c][j] + alpha * rng.NextSignedJitter(4.0)
                                        epochOfNextNeg[e] <- epochOfNextNeg[e] + float rawNeg * epochsPerNeg[e]
            coords, kEff, knnIdx, knnDist

    /// <summary>Runs the dimension-generic McInnes et al. (2018) UMAP embedding pipeline into <c>dims</c> coordinates.</summary>
    let embedUmapCore
        (token: System.Threading.CancellationToken)
        (dims: int)
        (metric: DistanceMetric)
        (neighbors: int)
        (minDist: float)
        (spread: float)
        (epochs: int)
        (negSampleRate: int)
        (learningRate: float)
        (seed: uint32)
        (n: int)
        (dim: int)
        (data: float[])
        : float[][] =
        let coords, _, _, _ = embedUmapCoreWithKnn token dims metric neighbors minDist spread epochs negSampleRate learningRate seed n dim data
        coords

    /// <summary>Binary-searches conditional Gaussian precisions <c>beta_i</c> to match Shannon <c>perplexity</c> and symmetrizes sparse CSR <c>P_ij = (p_j|i + p_i|j) / (2N)</c>.</summary>
    let buildTsneSymmetrizedP
        (token: System.Threading.CancellationToken)
        (n: int)
        (kEff: int)
        (perplexity: float)
        (knnIdx: int[])
        (knnDist: float[])
        : int[] * int[] * float[] =
        let effPerp =
            if float kEff <= perplexity then max 1.0 (min perplexity (float kEff * 0.85))
            else perplexity
        let targetEntropy = log effPerp
        let condP = Array.zeroCreate<float> (n * kEff)
        let rowExp = Array.zeroCreate<float> kEff

        for i = 0 to n - 1 do
            SimdMath.check token i
            let rowOff = i * kEff
            let mutable minD2 = infinity
            for s = 0 to kEff - 1 do
                let d = knnDist[rowOff + s]
                let d2 = d * d
                if d2 < minD2 then minD2 <- d2
            if not (Double.IsFinite minD2) then minD2 <- 0.0

            let mutable betaMin = -infinity
            let mutable betaMax = infinity
            let mutable beta = 1.0
            let mutable step = 0
            let mutable sumP = 1.0

            while step < 64 do
                sumP <- 0.0
                for s = 0 to kEff - 1 do
                    let d = knnDist[rowOff + s]
                    let shiftedD2 = max 0.0 (d * d - minD2)
                    let w = exp (-beta * shiftedD2)
                    rowExp[s] <- w
                    sumP <- sumP + w

                if sumP <= 1e-300 then
                    // Beta too large; shrink beta
                    betaMax <- beta
                    beta <- if Double.IsNegativeInfinity betaMin then beta * 0.5 else 0.5 * (beta + betaMin)
                    step <- step + 1
                else
                    let invS = 1.0 / sumP
                    let mutable h = 0.0
                    for s = 0 to kEff - 1 do
                        let p = rowExp[s] * invS
                        if p > 1e-300 then h <- h - p * log p
                    let hDiff = h - targetEntropy
                    if abs hDiff < 1e-5 then
                        step <- 64
                    else
                        if hDiff > 0.0 then
                            betaMin <- beta
                            beta <- if Double.IsPositiveInfinity betaMax then beta * 2.0 else 0.5 * (beta + betaMax)
                        else
                            betaMax <- beta
                            beta <- if Double.IsNegativeInfinity betaMin then beta * 0.5 else 0.5 * (beta + betaMin)
                        step <- step + 1

            let finalInv = if sumP > 1e-300 then 1.0 / sumP else 1.0 / float kEff
            for s = 0 to kEff - 1 do
                condP[rowOff + s] <- if sumP > 1e-300 then rowExp[s] * finalInv else finalInv

        let inline lookupCondP (src: int) (dst: int) : float =
            let rOff = src * kEff
            let mutable v = 0.0
            let mutable s = 0
            while s < kEff do
                if knnIdx[rOff + s] = dst then
                    v <- condP[rOff + s]
                    s <- kEff
                else
                    s <- s + 1
            v

        let rawDeg = Array.zeroCreate<int> n
        for i = 0 to n - 1 do
            let rowOff = i * kEff
            for s = 0 to kEff - 1 do
                let j = knnIdx[rowOff + s]
                if j >= 0 && j <> i then
                    rawDeg[i] <- rawDeg[i] + 1
                    rawDeg[j] <- rawDeg[j] + 1

        let rawOffsets = Array.zeroCreate<int> (n + 1)
        for i = 0 to n - 1 do rawOffsets[i + 1] <- rawOffsets[i] + rawDeg[i]
        let rawTotal = rawOffsets[n]
        let rawTargets = Array.zeroCreate<int> rawTotal
        let rawWeights = Array.zeroCreate<float> rawTotal
        let cursor = Array.copy rawOffsets
        let invTwoN = 1.0 / (2.0 * float n)

        for i = 0 to n - 1 do
            let rowOff = i * kEff
            for s = 0 to kEff - 1 do
                let j = knnIdx[rowOff + s]
                if j >= 0 && j <> i then
                    let pij = (condP[rowOff + s] + lookupCondP j i) * invTwoN
                    let ci = cursor[i]
                    rawTargets[ci] <- j
                    rawWeights[ci] <- pij
                    cursor[i] <- ci + 1
                    let cj = cursor[j]
                    rawTargets[cj] <- i
                    rawWeights[cj] <- pij
                    cursor[j] <- cj + 1

        let pOffsets = Array.zeroCreate<int> (n + 1)
        let mutable wp = 0
        for i = 0 to n - 1 do
            pOffsets[i] <- wp
            let start = rawOffsets[i]
            let len = rawOffsets[i + 1] - start
            if len > 0 then
                ManifoldSimd.sortIdxWeightSlice rawTargets rawWeights start len
                let mutable prevT = rawTargets[start]
                let mutable bestW = rawWeights[start]
                for idx = start + 1 to start + len - 1 do
                    let ct = rawTargets[idx]
                    let cw = rawWeights[idx]
                    if ct = prevT then
                        if cw > bestW then bestW <- cw
                    else
                        if bestW > 0.0 then
                            rawTargets[wp] <- prevT
                            rawWeights[wp] <- max 1e-12 bestW
                            wp <- wp + 1
                        prevT <- ct
                        bestW <- cw
                if bestW > 0.0 then
                    rawTargets[wp] <- prevT
                    rawWeights[wp] <- max 1e-12 bestW
                    wp <- wp + 1
        pOffsets[n] <- wp

        let pTargets = if wp = rawTotal then rawTargets else Array.sub rawTargets 0 wp
        let pWeights = if wp = rawTotal then rawWeights else Array.sub rawWeights 0 wp
        pOffsets, pTargets, pWeights

    /// <summary>Initializes t-SNE coordinates via <c>1e-4</c> standard-deviation scaled PCA (van der Maaten / openTSNE convention).</summary>
    let initTsnePca
        (token: System.Threading.CancellationToken)
        (dims: int)
        (n: int)
        (dim: int)
        (data: float[])
        (seed: uint32)
        : float[][] =
        let coords = pcaCoordinates token dims n dim data
        if n > 1 then
            let std0 = sqrt (SimdMath.sumSquares coords[0] n / float n)
            if std0 > 1e-15 then
                let scale = 1e-4 / std0
                for c = 0 to dims - 1 do
                    SimdMath.scaleInPlace scale coords[c] n
            else
                let rng = FastRng32(seed ^^^ 0x31415926u)
                for c = 0 to dims - 1 do
                    for i = 0 to n - 1 do
                        coords[c][i] <- rng.NextSignedJitter(1e-4)
                    SimdMath.zeroMeanInPlace coords[c] n
        coords

    /// <summary>Tenenbaum, de Silva &amp; Langford (2000) Isomap / Landmark Isomap geodesic multidimensional scaling into <c>dims</c> coordinates.</summary>
    let embedIsomapCore
        (token: System.Threading.CancellationToken)
        (dims: int)
        (metric: DistanceMetric)
        (neighbors: int)
        (n: int)
        (dim: int)
        (data: float[])
        : float[][] =
        let coords = Array.init dims (fun _ -> Array.zeroCreate<float> n)
        if n <= 1 then coords
        elif n = 2 then
            let d01 = max 1e-6 (ManifoldSimd.rowDistance metric data dim 0 1)
            coords[0][0] <- -0.5 * d01
            coords[0][1] <- 0.5 * d01
            coords
        else
            let kEff = min neighbors (n - 1)
            let knnIdx, knnDist = NnDescent.buildIndexedKnn token metric kEff 42u n dim data

            let adj = Array.init n (fun _ -> ResizeArray<struct(int * float)>(kEff * 2))
            let inline addSymEdge u v w =
                let mutable seenU = false
                for s = 0 to adj[u].Count - 1 do
                    let struct(nb, _) = adj[u][s]
                    if nb = v then seenU <- true
                if not seenU then adj[u].Add(struct(v, w))
                let mutable seenV = false
                for s = 0 to adj[v].Count - 1 do
                    let struct(nb, _) = adj[v][s]
                    if nb = u then seenV <- true
                if not seenV then adj[v].Add(struct(u, w))

            for i = 0 to n - 1 do
                let rOff = i * kEff
                for s = 0 to kEff - 1 do
                    let j = knnIdx[rOff + s]
                    if j >= 0 && j <> i then
                        addSymEdge i j (max 0.0 knnDist[rOff + s])

            // Ensure graph connectivity across separated clusters via minimum-distance MST bridges
            let compOf = Array.create n -1
            let compMembers = ResizeArray<int[]>()
            let bfsQ = Array.zeroCreate<int> n
            for s = 0 to n - 1 do
                if compOf[s] < 0 then
                    let cid = compMembers.Count
                    compOf[s] <- cid
                    bfsQ[0] <- s
                    let mutable h, t = 0, 1
                    while h < t do
                        let u = bfsQ[h]
                        h <- h + 1
                        for e = 0 to adj[u].Count - 1 do
                            let struct(v, _) = adj[u][e]
                            if compOf[v] < 0 then
                                compOf[v] <- cid
                                bfsQ[t] <- v
                                t <- t + 1
                    compMembers.Add(Array.sub bfsQ 0 t)

            if compMembers.Count > 1 then
                let cCount = compMembers.Count
                let inTree = Array.zeroCreate<bool> cCount
                let bestBridgeDist = Array.create cCount infinity
                let bestBridgeU = Array.create cCount -1
                let bestBridgeV = Array.create cCount -1

                let closestBetween (ca: int) (cb: int) : struct(int * int * float) =
                    let ma = compMembers[ca]
                    let mb = compMembers[cb]
                    let stepA = max 1 (ma.Length / 32)
                    let stepB = max 1 (mb.Length / 32)
                    let mutable bestD = infinity
                    let mutable bu, bv = ma[0], mb[0]
                    let mutable ia = 0
                    while ia < ma.Length do
                        let u = ma[ia]
                        let mutable ib = 0
                        while ib < mb.Length do
                            let v = mb[ib]
                            let d = ManifoldSimd.rowDistance metric data dim u v
                            if d < bestD then
                                bestD <- d
                                bu <- u
                                bv <- v
                            ib <- ib + stepB
                        ia <- ia + stepA
                    struct(bu, bv, bestD)

                inTree[0] <- true
                for c = 1 to cCount - 1 do
                    let struct(bu, bv, bd) = closestBetween 0 c
                    bestBridgeDist[c] <- bd
                    bestBridgeU[c] <- bu
                    bestBridgeV[c] <- bv

                for _ = 1 to cCount - 1 do
                    let mutable pick = -1
                    let mutable minD = infinity
                    for c = 1 to cCount - 1 do
                        if not inTree[c] && bestBridgeDist[c] < minD then
                            minD <- bestBridgeDist[c]
                            pick <- c
                    if pick >= 0 then
                        inTree[pick] <- true
                        addSymEdge bestBridgeU[pick] bestBridgeV[pick] minD
                        for c = 1 to cCount - 1 do
                            if not inTree[c] then
                                let struct(bu, bv, bd) = closestBetween pick c
                                if bd < bestBridgeDist[c] then
                                    bestBridgeDist[c] <- bd
                                    bestBridgeU[c] <- bu
                                    bestBridgeV[c] <- bv

            // Flatten into contiguous CSR for fast Dijkstra sweeps
            let csrOff = Array.zeroCreate<int> (n + 1)
            for i = 0 to n - 1 do csrOff[i + 1] <- csrOff[i] + adj[i].Count
            let csrTgt = Array.zeroCreate<int> csrOff[n]
            let csrWt = Array.zeroCreate<float> csrOff[n]
            for i = 0 to n - 1 do
                let bOff = csrOff[i]
                let listI = adj[i]
                for s = 0 to listI.Count - 1 do
                    let struct(v, w) = listI[s]
                    csrTgt[bOff + s] <- v
                    csrWt[bOff + s] <- w

            let dijkstraFrom (src: int) (distOut: float[]) =
                Array.fill distOut 0 n infinity
                distOut[src] <- 0.0
#if FABLE_COMPILER
                let pq = PriorityQueue<int, float>()
#else
                let pq = System.Collections.Generic.PriorityQueue<int, float>(min n 256)
#endif
                pq.Enqueue(src, 0.0)
                while pq.Count > 0 do
                    let u = pq.Dequeue()
                    let du = distOut[u]
                    for idx = csrOff[u] to csrOff[u + 1] - 1 do
                        let v = csrTgt[idx]
                        let cand = du + csrWt[idx]
                        if cand + 1e-15 < distOut[v] then
                            distOut[v] <- cand
                            pq.Enqueue(v, cand)

            // Exact Classical MDS for N <= 384; de Silva & Tenenbaum (2002) Landmark Isomap for N > 384
            let lCount = if n <= 384 then n else 96
            let landmarks = Array.zeroCreate<int> lCount
            let lDistSq = Array.init lCount (fun _ -> Array.zeroCreate<float> n)

            if lCount = n then
                let rowBuf = Array.zeroCreate<float> n
                for p = 0 to n - 1 do
                    SimdMath.check token p
                    landmarks[p] <- p
                    dijkstraFrom p rowBuf
                    let sqRow = lDistSq[p]
                    for j = 0 to n - 1 do
                        let d = if Double.IsFinite rowBuf[j] then rowBuf[j] else 0.0
                        sqRow[j] <- d * d
            else
                let rowBuf = Array.zeroCreate<float> n
                let minGeo = Array.create n infinity
                dijkstraFrom 0 rowBuf
                let mutable first = 0
                let mutable far = -1.0
                for j = 0 to n - 1 do
                    if Double.IsFinite rowBuf[j] && rowBuf[j] > far then
                        far <- rowBuf[j]
                        first <- j
                landmarks[0] <- first
                dijkstraFrom first rowBuf
                for j = 0 to n - 1 do
                    let d = if Double.IsFinite rowBuf[j] then rowBuf[j] else 0.0
                    minGeo[j] <- d
                    lDistSq[0][j] <- d * d
                for p = 1 to lCount - 1 do
                    token.ThrowIfCancellationRequested()
                    let mutable nxt = 0
                    let mutable best = -1.0
                    for j = 0 to n - 1 do
                        if minGeo[j] > best then
                            best <- minGeo[j]
                            nxt <- j
                    landmarks[p] <- nxt
                    dijkstraFrom nxt rowBuf
                    let sqRow = lDistSq[p]
                    for j = 0 to n - 1 do
                        let d = if Double.IsFinite rowBuf[j] then rowBuf[j] else 0.0
                        if d < minGeo[j] then minGeo[j] <- d
                        sqRow[j] <- d * d

            // Double-center the L x L landmark squared geodesic matrix B_L = -0.5 * H * D_LL^(2) * H
            let dLLMean = Array.zeroCreate<float> lCount
            let mutable grandSum = 0.0
            let invL = 1.0 / float lCount
            for p = 0 to lCount - 1 do
                let mutable s = 0.0
                let sqRow = lDistSq[p]
                for q = 0 to lCount - 1 do
                    s <- s + sqRow[landmarks[q]]
                dLLMean[p] <- s * invL
                grandSum <- grandSum + s
            let grandMean = grandSum * invL * invL

            let bMat = Array.init lCount (fun _ -> Array.zeroCreate<float> lCount)
            let mutable shift = 0.0
            for p = 0 to lCount - 1 do
                let sqRow = lDistSq[p]
                let bRow = bMat[p]
                let mutable absRowSum = 0.0
                for q = 0 to lCount - 1 do
                    let v = -0.5 * (sqRow[landmarks[q]] - dLLMean[p] - dLLMean[q] + grandMean)
                    bRow[q] <- v
                    absRowSum <- absRowSum + abs v
                if absRowSum > shift then shift <- absRowSum

            // Gershgorin shift ensures (B_L + shift * I) is PSD so power iteration extracts strictly top algebraic eigenvalues
            let onesVec = Array.create lCount (sqrt invL)
            let foundVecs = ResizeArray<float[]>(dims + 1)
            foundVecs.Add onesVec
            let vec = Array.zeroCreate<float> lCount
            let nextVec = Array.zeroCreate<float> lCount

            let orthogonalizeL (v: float[]) =
                for prev in foundVecs do
                    let proj = SimdMath.dot v prev lCount
                    SimdMath.axpy (-proj) prev v lCount

            let normalizeL (v: float[]) =
                let nrm = sqrt (SimdMath.sumSquares v lCount)
                if nrm > 1e-12 then
                    SimdMath.scaleInPlace (1.0 / nrm) v lCount
                    true
                else false

            for c = 0 to dims - 1 do
                token.ThrowIfCancellationRequested()
                for p = 0 to lCount - 1 do
                    vec[p] <- sin (float ((p + 1) * (c + 3)) * 1.618033988749895) + (if p = c % lCount then 1.0 else 0.0)
                orthogonalizeL vec
                if normalizeL vec then
                    let mutable it = 0
                    let mutable conv = false
                    while it < 96 && not conv do
                        for p = 0 to lCount - 1 do
                            nextVec[p] <- SimdMath.dot bMat[p] vec lCount + shift * vec[p]
                        orthogonalizeL nextVec
                        if not (normalizeL nextVec) then
                            conv <- true
                        else
                            let mutable fnz = 0.0
                            for p = 0 to lCount - 1 do
                                if fnz = 0.0 && abs nextVec[p] > 1e-9 then fnz <- nextVec[p]
                            if fnz < 0.0 then SimdMath.scaleInPlace -1.0 nextVec lCount
                            let mutable diff = 0.0
                            for p = 0 to lCount - 1 do
                                let dl = nextVec[p] - vec[p]
                                diff <- diff + dl * dl
                                vec[p] <- nextVec[p]
                            if diff < 1e-18 then conv <- true
                        it <- it + 1

                    let mutable eigVal = 0.0
                    for p = 0 to lCount - 1 do
                        eigVal <- eigVal + vec[p] * SimdMath.dot bMat[p] vec lCount
                    foundVecs.Add(Array.copy vec)

                    let outC = coords[c]
                    if eigVal > 1e-12 then
                        if lCount = n then
                            let rootEig = sqrt eigVal
                            for i = 0 to n - 1 do
                                outC[i] <- rootEig * vec[i]
                        else
                            // Exact de Silva & Tenenbaum (2002) Nystrom triangulation for all N vertices
                            let coeff = -0.5 / sqrt eigVal
                            for p = 0 to lCount - 1 do
                                let w = coeff * vec[p]
                                let meanP = dLLMean[p]
                                let sqRow = lDistSq[p]
                                for i = 0 to n - 1 do
                                    outC[i] <- outC[i] + w * (sqRow[i] - meanP)
                        SimdMath.zeroMeanInPlace outC n
                    else
                        for i = 0 to n - 1 do
                            outC[i] <- 1e-3 * sin (float ((i + 1) * (c + 2)) * 2.399963229728653)
                        SimdMath.zeroMeanInPlace outC n
            coords

    /// <summary>Sammon (1969) nonlinear stress <c>w_ij = 1 / d*_ij</c> embedding via Fisher-Yates shuffled SGD initialized from metric-scaled PCA.</summary>
    let embedSammonCore
        (token: System.Threading.CancellationToken)
        (dims: int)
        (metric: DistanceMetric)
        (iterations: int)
        (seed: uint32)
        (n: int)
        (dim: int)
        (data: float[])
        : float[][] =
        let coords = pcaCoordinates token dims n dim data
        if n <= 1 then coords
        elif n = 2 then
            let d01 = max 1e-6 (ManifoldSimd.rowDistance metric data dim 0 1)
            coords[0][0] <- -0.5 * d01
            coords[0][1] <- 0.5 * d01
            for c = 1 to dims - 1 do
                coords[c][0] <- 0.0
                coords[c][1] <- 0.0
            coords
        else
            let pairs =
                if n <= 512 then
                    let buf = ResizeArray<struct(int * int * float * float)>(n * (n - 1) / 2)
                    for i = 0 to n - 2 do
                        SimdMath.check token i
                        for j = i + 1 to n - 1 do
                            let d = ManifoldSimd.rowDistance metric data dim i j
                            if d > 1e-12 then
                                buf.Add(struct(i, j, d, 1.0 / d))
                    buf.ToArray()
                else
                    // Sparse landmark + k-NN pair set for large N
                    let kEff = min 16 (n - 1)
                    let knnIdx, knnDist = NnDescent.buildIndexedKnn token metric kEff seed n dim data
                    let lCount = min 64 n
                    let pivots = Array.zeroCreate<int> lCount
                    let minD = Array.create n infinity
                    pivots[0] <- 0
                    for i = 0 to n - 1 do
                        minD[i] <- ManifoldSimd.rowDistance metric data dim 0 i
                    for p = 1 to lCount - 1 do
                        let mutable bestI = 0
                        let mutable bestV = -1.0
                        for i = 0 to n - 1 do
                            if minD[i] > bestV then
                                bestV <- minD[i]
                                bestI <- i
                        pivots[p] <- bestI
                        for i = 0 to n - 1 do
                            let d = ManifoldSimd.rowDistance metric data dim bestI i
                            if d < minD[i] then minD[i] <- d

                    let isPiv = Array.zeroCreate<bool> n
                    for p = 0 to lCount - 1 do isPiv[pivots[p]] <- true
                    let buf = ResizeArray<struct(int * int * float * float)>(lCount * n + n * kEff)
                    for p = 0 to lCount - 1 do
                        let u = pivots[p]
                        for v = 0 to n - 1 do
                            if u <> v && (not isPiv[v] || u < v) then
                                let a, b = if u < v then u, v else v, u
                                let d = ManifoldSimd.rowDistance metric data dim a b
                                if d > 1e-12 then
                                    buf.Add(struct(a, b, d, 1.0 / d))
                    for i = 0 to n - 1 do
                        let rOff = i * kEff
                        for s = 0 to kEff - 1 do
                            let j = knnIdx[rOff + s]
                            if j > i && not isPiv[i] && not isPiv[j] then
                                let d = knnDist[rOff + s]
                                if d > 1e-12 then
                                    buf.Add(struct(i, j, d, 1.0 / d))
                    buf.ToArray()

            let pCount = pairs.Length
            if pCount > 0 then
                // Scale initial PCA coordinates so pairwise distances match mean target metric distance
                let mutable sumTarget = 0.0
                let mutable sumInit = 0.0
                let mutable wMin = infinity
                let mutable wMax = 0.0
                for p = 0 to pCount - 1 do
                    let struct(i, j, dij, wij) = pairs[p]
                    sumTarget <- sumTarget + dij
                    if wij < wMin then wMin <- wij
                    if wij > wMax then wMax <- wij
                    let mutable dSq = 0.0
                    for c = 0 to dims - 1 do
                        let dl = coords[c][i] - coords[c][j]
                        dSq <- dSq + dl * dl
                    sumInit <- sumInit + sqrt dSq

                let rng = FastRng32(seed)
                if sumInit > 1e-12 then
                    let initScale = sumTarget / sumInit
                    for c = 0 to dims - 1 do
                        SimdMath.scaleInPlace initScale coords[c] n
                else
                    let meanD = sumTarget / float pCount
                    for c = 0 to dims - 1 do
                        for i = 0 to n - 1 do
                            coords[c][i] <- rng.NextSignedJitter(meanD * 0.5)
                        SimdMath.zeroMeanInPlace coords[c] n

                let etaMax = 1.0 / max 1e-12 wMin
                let etaMin = 0.03 / max 1e-12 wMax
                let lambda =
                    if iterations <= 1 || etaMax <= etaMin then 0.0
                    else log (etaMin / etaMax) / float (iterations - 1)
                let order = Array.init pCount id

                for ep = 0 to iterations - 1 do
                    token.ThrowIfCancellationRequested()
                    let eta = if iterations <= 1 then min etaMax (etaMax * 0.5) else etaMax * exp (lambda * float ep)
                    for k = pCount - 1 downto 1 do
                        let sw = rng.NextBounded(k + 1)
                        let tmp = order[k]
                        order[k] <- order[sw]
                        order[sw] <- tmp

                    for k = 0 to pCount - 1 do
                        SimdMath.check token k
                        let struct(i, j, dij, wij) = pairs[order[k]]
                        let mu = min 0.9 (eta * wij)
                        let mutable magSq = 0.0
                        for c = 0 to dims - 1 do
                            let dl = coords[c][i] - coords[c][j]
                            magSq <- magSq + dl * dl
                        if magSq <= 1e-20 then
                            let kick = 0.25 * mu * dij
                            for c = 0 to dims - 1 do
                                let jiggle = rng.NextSignedJitter(kick)
                                coords[c][i] <- coords[c][i] + jiggle
                                coords[c][j] <- coords[c][j] - jiggle
                        else
                            let mag = sqrt magSq
                            let factor = mu * (mag - dij) / (2.0 * mag)
                            for c = 0 to dims - 1 do
                                let stepC = (coords[c][i] - coords[c][j]) * factor
                                coords[c][i] <- coords[c][i] - stepC
                                coords[c][j] <- coords[c][j] + stepC

                for c = 0 to dims - 1 do
                    SimdMath.zeroMeanInPlace coords[c] n
            coords

    /// <summary>Isotropically scales and centers 2D coordinate arrays into <c>bounds</c> while guaranteeing inclusive containment.</summary>
    let fitPoints2D (bounds: Bounds) (ids: 'Id[]) (xs: float[]) (ys: float[]) : EmbeddedPoint2D<'Id> list =
        let n = ids.Length
        if n = 0 then []
        else
            let bx, by = Bounds.x bounds, Bounds.y bounds
            let bw, bh = Bounds.width bounds, Bounds.height bounds
            let cx, cy = bx + bw * 0.5, by + bh * 0.5
            if n = 1 then
                [ { Id = ids[0]; Position = Point.create cx cy } ]
            else
                let mutable minX, maxX = infinity, -infinity
                let mutable minY, maxY = infinity, -infinity
                for i = 0 to n - 1 do
                    let x, y = xs[i], ys[i]
                    if x < minX then minX <- x
                    if x > maxX then maxX <- x
                    if y < minY then minY <- y
                    if y > maxY then maxY <- y
                let spanX = maxX - minX
                let spanY = maxY - minY
                let midX = 0.5 * (minX + maxX)
                let midY = 0.5 * (minY + maxY)
                let usableW = bw * 0.90
                let usableH = bh * 0.90
                let scale =
                    match spanX > 1e-12 && bw > 0.0, spanY > 1e-12 && bh > 0.0 with
                    | true, true -> min (usableW / spanX) (usableH / spanY)
                    | true, false -> usableW / spanX
                    | false, true -> usableH / spanY
                    | false, false -> 0.0
                let maxEdgeX = bx + bw
                let maxEdgeY = by + bh
                [ for i in 0 .. n - 1 ->
                    let px = if bw <= 0.0 then bx else max bx (min maxEdgeX (cx + (xs[i] - midX) * scale))
                    let py = if bh <= 0.0 then by else max by (min maxEdgeY (cy + (ys[i] - midY) * scale))
                    { Id = ids[i]; Position = Point.create px py } ]

/// <summary>Hyperparameters for McInnes, Healy &amp; Melville (2018) 2D Uniform Manifold Approximation and Projection (<c>Umap2D</c>).</summary>
type UmapConfig =
    { /// <summary>Target 2D viewport rectangle into which embedded points are scaled.</summary>
      Bounds: Bounds
      /// <summary>High-dimensional dissimilarity metric.</summary>
      Metric: DistanceMetric
      /// <summary>Local neighborhood size <c>k</c> (<c>&gt;= 2</c>; default 15).</summary>
      Neighbors: int
      /// <summary>Minimum separation parameter controlling cluster tightness (<c>&gt;= 0</c>; default 0.1).</summary>
      MinDist: float
      /// <summary>Effective global scale of embedded points (<c>&gt; 0</c>; default 1.0).</summary>
      Spread: float
      /// <summary>Negative-sampling SGD optimization epochs (<c>1..10000</c>; default 200).</summary>
      Epochs: int
      /// <summary>Number of repulsive negative samples per positive edge sample (default 5).</summary>
      NegativeSampleRate: int
      /// <summary>Initial SGD learning rate before linear annealing (<c>&gt; 0</c>; default 1.0).</summary>
      LearningRate: float
      /// <summary>Deterministic PRNG seed (default 42).</summary>
      Seed: uint32 }

/// <summary>Constructors for <c>UmapConfig</c>.</summary>
[<RequireQualifiedAccess>]
module UmapConfig =
    /// <summary>Creates a 2D UMAP configuration with 15 neighbors, min-dist 0.1, spread 1.0, 200 epochs, negative sample rate 5, learning rate 1.0 and seed 42.</summary>
    let create (bounds: Bounds) : UmapConfig =
        { Bounds = bounds
          Metric = DistanceMetric.Euclidean
          Neighbors = 15
          MinDist = 0.1
          Spread = 1.0
          Epochs = 200
          NegativeSampleRate = 5
          LearningRate = 1.0
          Seed = 42u }

/// <summary>McInnes, Healy &amp; Melville (2018) 2D Uniform Manifold Approximation and Projection (<c>Umap2D</c>).</summary>
[<RequireQualifiedAccess>]
module Umap2D =
    /// <summary>Computes a deterministic 2D UMAP embedding over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (config: UmapConfig)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if config.Neighbors < 2
           || not (Double.IsFinite config.MinDist && Double.IsFinite config.Spread && Double.IsFinite config.LearningRate)
           || config.MinDist < 0.0 || config.Spread <= 0.0 || config.LearningRate <= 0.0
           || config.Epochs < 1 || config.Epochs > 10000
           || config.NegativeSampleRate < 0 || config.NegativeSampleRate > 256 then
            Error [ InvalidObservation "Umap2D requires Neighbors >= 2, MinDist >= 0, Spread > 0, LearningRate > 0, Epochs in [1,10000] and NegativeSampleRate in [0,256]." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let coords =
                ManifoldCore.embedUmapCore
                    token
                    2
                    config.Metric
                    config.Neighbors
                    config.MinDist
                    config.Spread
                    config.Epochs
                    config.NegativeSampleRate
                    config.LearningRate
                    config.Seed
                    ids.Length
                    dim
                    data
            Ok(ManifoldCore.fitPoints2D config.Bounds ids coords[0] coords[1])

    /// <summary>Computes a deterministic 2D UMAP embedding over <c>dataset</c> without cancellation.</summary>
    let embed (config: UmapConfig) (dataset: FeatureDataset<'Id>) : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None config dataset

/// <summary>Hyperparameters for van der Maaten (2014) Barnes-Hut 2D t-Distributed Stochastic Neighbor Embedding (<c>Tsne2D</c>).</summary>
type TsneConfig =
    { /// <summary>Target 2D viewport rectangle into which embedded points are scaled.</summary>
      Bounds: Bounds
      /// <summary>High-dimensional dissimilarity metric.</summary>
      Metric: DistanceMetric
      /// <summary>Target Shannon perplexity of conditional Gaussian neighborhoods (<c>&gt; 0</c>; default 30.0).</summary>
      Perplexity: float
      /// <summary>Gradient descent iteration count (<c>1..10000</c>; default 250).</summary>
      Iterations: int
      /// <summary>Barnes-Hut quadtree opening threshold theta (<c>0</c> for exact <c>O(N^2)</c>, default 0.5).</summary>
      Theta: float
      /// <summary>Gradient descent base learning rate (<c>&gt; 0</c>; default 200.0).</summary>
      LearningRate: float
      /// <summary>Attractive affinity multiplier applied during the initial early-exaggeration phase (<c>&gt;= 1</c>; default 4.0).</summary>
      EarlyExaggeration: float
      /// <summary>Deterministic PRNG seed (default 42).</summary>
      Seed: uint32 }

/// <summary>Constructors for <c>TsneConfig</c>.</summary>
[<RequireQualifiedAccess>]
module TsneConfig =
    /// <summary>Creates a 2D Barnes-Hut t-SNE configuration with perplexity 30.0, 250 iterations, theta 0.5, learning rate 200.0, early exaggeration 4.0 and seed 42.</summary>
    let create (bounds: Bounds) : TsneConfig =
        { Bounds = bounds
          Metric = DistanceMetric.Euclidean
          Perplexity = 30.0
          Iterations = 250
          Theta = 0.5
          LearningRate = 200.0
          EarlyExaggeration = 4.0
          Seed = 42u }

/// <summary>van der Maaten (2014) <c>O(N log N)</c> Barnes-Hut 2D t-Distributed Stochastic Neighbor Embedding (<c>Tsne2D</c>).</summary>
[<RequireQualifiedAccess>]
module Tsne2D =
    /// <summary>Computes a deterministic 2D Barnes-Hut t-SNE embedding over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (config: TsneConfig)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.Perplexity && Double.IsFinite config.Theta
                && Double.IsFinite config.LearningRate && Double.IsFinite config.EarlyExaggeration)
           || config.Perplexity <= 0.0 || config.Theta < 0.0 || config.Theta > 2.0
           || config.LearningRate <= 0.0 || config.EarlyExaggeration < 1.0
           || config.Iterations < 1 || config.Iterations > 10000 then
            Error [ InvalidObservation "Tsne2D requires Perplexity > 0, Theta in [0,2], LearningRate > 0, EarlyExaggeration >= 1 and Iterations in [1,10000]." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let n = ids.Length
            if n <= 1 then
                Ok(ManifoldCore.fitPoints2D config.Bounds ids (Array.zeroCreate<float> n) (Array.zeroCreate<float> n))
            else
                let kEff = min (n - 1) (max 3 (int (floor (3.0 * config.Perplexity))))
                let knnIdx, knnDist = NnDescent.buildIndexedKnn token config.Metric kEff config.Seed n dim data
                let pOffsets, pTargets, pWeights =
                    ManifoldCore.buildTsneSymmetrizedP token n kEff config.Perplexity knnIdx knnDist
                let init = ManifoldCore.initTsnePca token 2 n dim data config.Seed
                let xs = init[0]
                let ys = init[1]

                let attrX = Array.zeroCreate<float> n
                let attrY = Array.zeroCreate<float> n
                let repX = Array.zeroCreate<float> n
                let repY = Array.zeroCreate<float> n
                let velX = Array.zeroCreate<float> n
                let velY = Array.zeroCreate<float> n
                let gainX = Array.create n 1.0
                let gainY = Array.create n 1.0
                let useBh = n > 32 && config.Theta > 0.0
                let quadWs = if useBh then BhQuadtree2D.create n else Unchecked.defaultof<_>
                let unitMass = if useBh then Array.create n 1.0 else [||]
                let thetaSq = config.Theta * config.Theta
                let stopLyingIter = min 250 (config.Iterations / 4)
                let lr = config.LearningRate

                for iter = 0 to config.Iterations - 1 do
                    token.ThrowIfCancellationRequested()
                    let exag = if iter < stopLyingIter then config.EarlyExaggeration else 1.0
                    let momentum = if iter < stopLyingIter then 0.5 else 0.8

                    // 1. Exact sparse Student-t attractive forces over symmetrized CSR P
                    for i = 0 to n - 1 do
                        let xi, yi = xs[i], ys[i]
                        let mutable ax, ay = 0.0, 0.0
                        for idx = pOffsets[i] to pOffsets[i + 1] - 1 do
                            let j = pTargets[idx]
                            let dx = xi - xs[j]
                            let dy = yi - ys[j]
                            let w = 1.0 / (1.0 + dx * dx + dy * dy)
                            let s = (pWeights[idx] * exag) * w
                            ax <- ax + s * dx
                            ay <- ay + s * dy
                        attrX[i] <- ax
                        attrY[i] <- ay

                    // 2. Repulsive Student-t forces and partition sum Z via O(N log N) BhQuadtree2D (or exact O(N^2) when theta = 0)
                    let mutable zSum = 0.0
                    if not useBh then
                        Array.fill repX 0 n 0.0
                        Array.fill repY 0 n 0.0
                        for i = 0 to n - 2 do
                            let xi, yi = xs[i], ys[i]
                            for j = i + 1 to n - 1 do
                                let dx = xi - xs[j]
                                let dy = yi - ys[j]
                                let w = 1.0 / (1.0 + dx * dx + dy * dy)
                                zSum <- zSum + 2.0 * w
                                let w2 = w * w
                                let rx, ry = w2 * dx, w2 * dy
                                repX[i] <- repX[i] + rx
                                repY[i] <- repY[i] + ry
                                repX[j] <- repX[j] - rx
                                repY[j] <- repY[j] - ry
                    else
                        BhQuadtree2D.rebuildInPlace quadWs token xs ys unitMass n
                        let qNodes = quadWs.Nodes
                        let perm = quadWs.Perm
                        let stack = quadWs.Stack
                        for i = 0 to n - 1 do
                            SimdMath.check token i
                            let xi, yi = xs[i], ys[i]
                            let mutable rx, ry = 0.0, 0.0
                            let mutable zi = 0.0
                            stack[0] <- 0
                            let mutable sp = 1
                            while sp > 0 do
                                sp <- sp - 1
                                let nd = qNodes[stack[sp]]
                                let dx = xi - nd.Cx
                                let dy = yi - nd.Cy
                                let dSq = dx * dx + dy * dy
                                let sz = max (nd.MaxX - nd.MinX) (nd.MaxY - nd.MinY)
                                let contains = xi >= nd.MinX && xi <= nd.MaxX && yi >= nd.MinY && yi <= nd.MaxY
                                if not contains && dSq > 1e-12 && sz * sz < thetaSq * dSq then
                                    let w = 1.0 / (1.0 + dSq)
                                    let mw = nd.Mass * w
                                    zi <- zi + mw
                                    let mw2 = mw * w
                                    rx <- rx + mw2 * dx
                                    ry <- ry + mw2 * dy
                                elif nd.ItemCount > 0 then
                                    for k = nd.ItemStart to nd.ItemStart + nd.ItemCount - 1 do
                                        let j = perm[k]
                                        if j <> i then
                                            let ldx = xi - xs[j]
                                            let ldy = yi - ys[j]
                                            let w = 1.0 / (1.0 + ldx * ldx + ldy * ldy)
                                            zi <- zi + w
                                            let w2 = w * w
                                            rx <- rx + w2 * ldx
                                            ry <- ry + w2 * ldy
                                elif nd.ChildCount > 0 then
                                    for c = nd.FirstChild + nd.ChildCount - 1 downto nd.FirstChild do
                                        stack[sp] <- c
                                        sp <- sp + 1
                            repX[i] <- rx
                            repY[i] <- ry
                            zSum <- zSum + zi

                    // 3. Adaptive delta-bar-delta gains + momentum update + zero-mean centering
                    let invZ = 1.0 / max 1e-12 zSum
                    for i = 0 to n - 1 do
                        let gx = 4.0 * (attrX[i] - repX[i] * invZ)
                        let gy = 4.0 * (attrY[i] - repY[i] * invZ)
                        let nextGainX = if (gx > 0.0) <> (velX[i] > 0.0) then gainX[i] + 0.2 else max 0.01 (gainX[i] * 0.8)
                        let nextGainY = if (gy > 0.0) <> (velY[i] > 0.0) then gainY[i] + 0.2 else max 0.01 (gainY[i] * 0.8)
                        gainX[i] <- nextGainX
                        gainY[i] <- nextGainY
                        let vx = momentum * velX[i] - lr * nextGainX * gx
                        let vy = momentum * velY[i] - lr * nextGainY * gy
                        velX[i] <- vx
                        velY[i] <- vy
                        xs[i] <- xs[i] + vx
                        ys[i] <- ys[i] + vy

                    SimdMath.zeroMeanInPlace xs n
                    SimdMath.zeroMeanInPlace ys n

                Ok(ManifoldCore.fitPoints2D config.Bounds ids xs ys)

    /// <summary>Computes a deterministic 2D Barnes-Hut t-SNE embedding over <c>dataset</c> without cancellation.</summary>
    let embed (config: TsneConfig) (dataset: FeatureDataset<'Id>) : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None config dataset

/// <summary>Tenenbaum, de Silva &amp; Langford (2000) 2D Isometric Feature Mapping (<c>Isomap2D</c>) via k-NN geodesic shortest paths and double-centered MDS.</summary>
[<RequireQualifiedAccess>]
module Isomap2D =
    /// <summary>Computes a deterministic 2D Isomap embedding over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (bounds: Bounds)
        (metric: DistanceMetric)
        (neighbors: int)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if neighbors < 1 then
            Error [ InvalidObservation "Isomap2D requires neighbors >= 1." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let coords = ManifoldCore.embedIsomapCore token 2 metric neighbors ids.Length dim data
            Ok(ManifoldCore.fitPoints2D bounds ids coords[0] coords[1])

    /// <summary>Computes a deterministic 2D Isomap embedding over <c>dataset</c> without cancellation.</summary>
    let embed
        (bounds: Bounds)
        (metric: DistanceMetric)
        (neighbors: int)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None bounds metric neighbors dataset

/// <summary>Sammon (1969) 2D nonlinear stress mapping (<c>Sammon2D</c>) minimizing <c>sum_(i&lt;j) (d_ij - d*_ij)^2 / d*_ij</c> via shuffled SGD.</summary>
[<RequireQualifiedAccess>]
module Sammon2D =
    /// <summary>Computes a deterministic 2D Sammon nonlinear mapping over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (bounds: Bounds)
        (metric: DistanceMetric)
        (iterations: int)
        (seed: uint32)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if iterations < 1 || iterations > 10000 then
            Error [ InvalidObservation "Sammon2D requires iterations in [1,10000]." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let coords = ManifoldCore.embedSammonCore token 2 metric iterations seed ids.Length dim data
            Ok(ManifoldCore.fitPoints2D bounds ids coords[0] coords[1])

    /// <summary>Computes a deterministic 2D Sammon nonlinear mapping over <c>dataset</c> without cancellation.</summary>
    let embed
        (bounds: Bounds)
        (metric: DistanceMetric)
        (iterations: int)
        (seed: uint32)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint2D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None bounds metric iterations seed dataset

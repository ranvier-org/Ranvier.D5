namespace Ranvier.D5

#if FABLE_COMPILER
open System
open System.Collections.Generic

/// <summary>Stand-ins for .NET members that Fable does not translate. Compiled only under Fable: each nested module shadows the BCL
/// type of the same name for the members it defines, and the BCL type still answers every other member.</summary>
/// <remarks>Needs Fickl 0.10.1 or later, which wraps uint32 arithmetic and translates BitConverter.DoubleToUInt64Bits; with
/// earlier compilers the Force and hierarchy hashes differ from .NET and Predicates fails to load.</remarks>
[<AutoOpen>]
module FableCompat =
    [<RequireQualifiedAccess>]
    module Double =
        /// <summary>True for any value that is neither NaN nor infinite (x - x is 0 only then).</summary>
        let inline IsFinite (x: float) = x - x = 0.

        /// <summary>sqrt(x² + y²) without intermediate overflow or underflow; infinity wins over NaN, as in .NET.</summary>
        let Hypot (x: float, y: float) =
            let ax, ay = abs x, abs y
            if Double.IsPositiveInfinity ax || Double.IsPositiveInfinity ay then Double.PositiveInfinity
            elif Double.IsNaN ax || Double.IsNaN ay then Double.NaN
            else
                let large, small = max ax ay, min ax ay
                if large = 0. then 0.
                else
                    let ratio = small / large
                    large * sqrt (1. + ratio * ratio)

        /// <summary>True for finite values with no fractional part.</summary>
        let IsInteger (x: float) = IsFinite x && Math.Truncate x = x

        /// <summary>log(1 + x), accurate near zero.</summary>
        let LogP1 (x: float) =
            let u = 1. + x
            if u = 1. || Double.IsPositiveInfinity u then x else Math.Log u * x / (u - 1.)

        /// <summary>exp(x) - 1, accurate near zero.</summary>
        let ExpM1 (x: float) =
            let u = Math.Exp x
            if u = 1. || Double.IsPositiveInfinity u then (if u = 1. then x else u)
            elif u - 1. = -1. then -1.
            else (u - 1.) * x / Math.Log u

    [<RequireQualifiedAccess>]
    module Math =
        /// <summary>Sine and cosine as a struct pair, as Math.SinCos returns.</summary>
        let SinCos (x: float) = struct (Math.Sin x, Math.Cos x)

        /// <summary>Inverse hyperbolic sine, odd-symmetric so negative inputs keep their precision.</summary>
        let Asinh (x: float) =
            if Double.IsNaN x || Double.IsInfinity x then x
            else
                let a = abs x
                let r = Math.Log (a + sqrt (a * a + 1.))
                if x < 0. then -r else r

        /// <summary>The base-2 exponent of x: Int32.MinValue for 0, Int32.MaxValue for NaN and infinities.</summary>
        let ILogB (x: float) =
            if x = 0. then Int32.MinValue
            elif Double.IsNaN x || Double.IsInfinity x then Int32.MaxValue
            else
                let a = abs x
                let e = int (floor (Math.Log2 a))
                // Log2 can round across a power of two; Math.Pow(2, e) is exact, so correct by one either way.
                if Math.Pow (2., float e) > a then e - 1
                elif Math.Pow (2., float (e + 1)) <= a then e + 1
                else e

        /// <summary>x * 2^n, in steps that keep each power of two representable.</summary>
        let ScaleB (x: float, n: int) =
            let mutable value = x
            let mutable k = n
            while k > 1023 do
                value <- value * Math.Pow (2., 1023.)
                k <- k - 1023
            while k < -1022 do
                value <- value * Math.Pow (2., -1022.)
                k <- k + 1022
            value * Math.Pow (2., float k)

        /// <summary>The next representable value above x.</summary>
        let BitIncrement (x: float) =
            if Double.IsNaN x || Double.IsPositiveInfinity x then x
            elif Double.IsNegativeInfinity x then -Double.MaxValue
            elif x = 0. then Double.Epsilon
            else
                let e = ILogB x
                let powerOfTwo = Math.Pow (2., float e) = abs x
                // Below a power of two the spacing halves, so stepping a negative power of two toward zero uses the finer ulp.
                let step = if x < 0. && powerOfTwo then e - 53 else e - 52
                x + Math.Pow (2., float (max step -1074))

    [<RequireQualifiedAccess>]
    module Environment =
        /// <summary>JavaScript runs D5 on one thread.</summary>
        let CurrentManagedThreadId = 1

    /// <summary>Ordinal string order and equality, usable where .NET passes StringComparer.Ordinal.</summary>
    type OrdinalStringComparer() =
        member _.Compare (a: string, b: string) = String.CompareOrdinal (a, b)
        member _.Equals (a: string, b: string) = String.Equals (a, b)
        interface IComparer<string> with
            member _.Compare (a, b) = String.CompareOrdinal (a, b)
        interface IEqualityComparer<string> with
            member _.Equals (a, b) = String.Equals (a, b)
            member _.GetHashCode value = if isNull value then 0 else value.GetHashCode ()

    [<RequireQualifiedAccess>]
    module StringComparer =
        let Ordinal = OrdinalStringComparer ()

    /// <summary>UTF-8 byte counts for the budget checks that call Encoding.UTF8.GetByteCount.</summary>
    type Utf8Counter() =
        member _.GetByteCount (text: string) =
            let mutable count = 0
            let mutable i = 0
            while i < text.Length do
                let c = int text[i]
                if c < 0x80 then count <- count + 1
                elif c < 0x800 then count <- count + 2
                elif c >= 0xD800 && c <= 0xDBFF && i + 1 < text.Length && int text[i + 1] >= 0xDC00 && int text[i + 1] <= 0xDFFF then
                    count <- count + 4
                    i <- i + 1
                else count <- count + 3 // a lone surrogate encodes as U+FFFD, also 3 bytes
                i <- i + 1
            count

    [<RequireQualifiedAccess>]
    module Encoding =
        let UTF8 = Utf8Counter ()

    /// <summary>Under Fable only UTC exists: there is no time zone database. <c>Utc</c> is a marker value; zone queries answer as UTC.</summary>
    [<RequireQualifiedAccess>]
    module TimeZoneInfo =
        let Utc: System.TimeZoneInfo = unbox "UTC"
        let ConvertTime (instant: DateTimeOffset, _zone: System.TimeZoneInfo) = instant.ToUniversalTime ()

    /// <summary>A binary min-heap with the members of PriorityQueue that D5 uses. Equal priorities dequeue in no defined order, as in .NET.</summary>
    type PriorityQueue<'T, 'P when 'P: comparison>() =
        let items = ResizeArray<struct ('T * 'P)> ()
        let priority i = let struct (_, p) = items[i] in p
        let swap i j =
            let t = items[i]
            items[i] <- items[j]
            items[j] <- t
        member _.Count = items.Count
        member _.Enqueue (item: 'T, p: 'P) =
            items.Add (struct (item, p))
            let mutable i = items.Count - 1
            while i > 0 && priority ((i - 1) / 2) > priority i do
                swap i ((i - 1) / 2)
                i <- (i - 1) / 2
        member _.Dequeue () : 'T =
            if items.Count = 0 then invalidOp "The queue is empty."
            let struct (top, _) = items[0]
            items[0] <- items[items.Count - 1]
            items.RemoveAt (items.Count - 1)
            let mutable i = 0
            let mutable moving = true
            while moving do
                let l, r = 2 * i + 1, 2 * i + 2
                let mutable smallest = i
                if l < items.Count && priority l < priority smallest then smallest <- l
                if r < items.Count && priority r < priority smallest then smallest <- r
                if smallest = i then moving <- false
                else
                    swap i smallest
                    i <- smallest
            top
#endif

/// <summary>Time zone queries that D5 makes. On .NET they call TimeZoneInfo; under Fable, where only UTC exists, they answer as UTC.</summary>
module internal Zone =
    open System
    let id (zone: TimeZoneInfo) =
#if FABLE_COMPILER
        "UTC"
#else
        zone.Id
#endif
    let isInvalidTime (zone: TimeZoneInfo) (local: DateTime) =
#if FABLE_COMPILER
        false
#else
        zone.IsInvalidTime local
#endif
    let isAmbiguousTime (zone: TimeZoneInfo) (local: DateTime) =
#if FABLE_COMPILER
        false
#else
        zone.IsAmbiguousTime local
#endif
    let ambiguousOffsets (zone: TimeZoneInfo) (local: DateTime) =
#if FABLE_COMPILER
        [| TimeSpan.Zero |]
#else
        zone.GetAmbiguousTimeOffsets local
#endif
    let utcOffset (zone: TimeZoneInfo) (local: DateTime) =
#if FABLE_COMPILER
        TimeSpan.Zero
#else
        zone.GetUtcOffset local
#endif

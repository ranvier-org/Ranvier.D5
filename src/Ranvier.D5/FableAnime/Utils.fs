namespace Ranvier.Anime

open System

/// <summary>The two Ranvier.Anime utilities D5 calls, copied from Ranvier.Anime 0.1.0-rc.2 (src/Ranvier.Anime/Utils.fs) because that package
/// ships no F# sources for Fable. Compiled only under Fable; .NET builds reference the package.</summary>
[<RequireQualifiedAccess>]
module Utils =
    let mutable private autoSeed = 0.

    /// <summary>Linearly interpolates between start and finish by factor.</summary>
    let lerp (start: float) (finish: float) (factor: float) : float =
        if factor = 1. then finish elif factor = 0. then start else start + (finish - start) * factor

    /// <summary>Anime.js seeded random: a stateful generator with default bounds and decimal length.</summary>
    let createSeededRandom (seed: float option) (seededMin: float option) (seededMax: float option) (seededDecimalLength: int option)
                           : (float option -> float option -> int option -> float) =
        let defaultMin = defaultArg seededMin 0.
        let defaultMax = defaultArg seededMax 1.
        let defaultDecimals = defaultArg seededDecimalLength 0
        let mutable t =
            match seed with
            | Some s -> s
            | None ->
                let s = autoSeed
                autoSeed <- autoSeed + 1.
                s
        let toUint32 (v: float) =
            let rem = Math.Truncate(v) % 4294967296.
            uint32 (if rem < 0. then rem + 4294967296. else rem)
        fun minOpt maxOpt decOpt ->
            let lo = defaultArg minOpt defaultMin
            let hi = defaultArg maxOpt defaultMax
            let dec = defaultArg decOpt defaultDecimals
            let u0 = toUint32 (t + 1831565813.)
            let u1 = (u0 ^^^ (u0 >>> 15)) * (u0 ||| 1u)
            let u2 = u1 ^^^ (u1 + ((u1 ^^^ (u1 >>> 7)) * (u1 ||| 61u)))
            t <- float (int32 u2)
            let normalized = float (u2 ^^^ (u2 >>> 14)) / 4294967296.
            let m = Math.Pow(10., float dec)
            Math.Floor((normalized * (hi - lo + (1. / m)) + lo) * m) / m

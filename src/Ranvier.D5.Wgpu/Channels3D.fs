namespace Ranvier.D5.Wgpu

open System
open System.Globalization
open System.Numerics
open Ranvier.Anime
open Ranvier.D5
open Ranvier.Wgpu

#if FABLE_COMPILER
/// <summary>
/// Browser/Fable compatibility stand-ins for <c>System.Numerics.Quaternion</c> members not lowered by Fickl 0.11.0.
/// Browser/Fable callers referencing <c>Quaternion</c> directly should <c>open System.Numerics</c> before <c>open Ranvier.D5.Wgpu</c>
/// so <c>FableCompat3D.Quaternion</c> shadows <c>System.Numerics.Quaternion</c> cleanly.
/// </summary>
[<AutoOpen>]
module FableCompat3D =
    /// <summary>Single-precision unit quaternion stand-in matching <c>System.Numerics.Quaternion</c> layout and conventions.</summary>
    [<Struct>]
    type Quaternion =
        /// <summary>X vector component.</summary>
        val X: float32
        /// <summary>Y vector component.</summary>
        val Y: float32
        /// <summary>Z vector component.</summary>
        val Z: float32
        /// <summary>W scalar component.</summary>
        val W: float32

        /// <summary>Creates a quaternion from <c>(x, y, z, w)</c> components.</summary>
        new(x: float32, y: float32, z: float32, w: float32) = { X = x; Y = y; Z = z; W = w }

        /// <summary>The multiplicative identity quaternion <c>(0, 0, 0, 1)</c>.</summary>
        static member Identity = Quaternion (0.0f, 0.0f, 0.0f, 1.0f)

        /// <summary>Returns the squared Euclidean norm of this quaternion.</summary>
        member this.LengthSquared() : float32 =
            this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W

        /// <summary>Returns the Euclidean norm of this quaternion.</summary>
        member this.Length() : float32 =
            MathF.Sqrt (this.X * this.X + this.Y * this.Y + this.Z * this.Z + this.W * this.W)

        /// <summary>Returns a unit-length copy of <c>q</c>.</summary>
        static member Normalize(q: Quaternion) : Quaternion =
            let len = q.Length ()
            if len > 0.0f then
                let inv = 1.0f / len
                Quaternion (q.X * inv, q.Y * inv, q.Z * inv, q.W * inv)
            else
                q

        /// <summary>Constructs a quaternion from a unit rotation <c>axis</c> and rotation <c>angle</c> in radians.</summary>
        static member CreateFromAxisAngle(axis: Vector3, angle: float32) : Quaternion =
            let half = angle * 0.5f
            let s = MathF.Sin half
            let c = MathF.Cos half
            Quaternion (axis.X * s, axis.Y * s, axis.Z * s, c)

        /// <summary>Multiplies two quaternions in Hamilton order.</summary>
        static member op_Multiply(value1: Quaternion, value2: Quaternion) : Quaternion =
            let q1x, q1y, q1z, q1w = value1.X, value1.Y, value1.Z, value1.W
            let q2x, q2y, q2z, q2w = value2.X, value2.Y, value2.Z, value2.W
            let cx = q1y * q2z - q1z * q2y
            let cy = q1z * q2x - q1x * q2z
            let cz = q1x * q2y - q1y * q2x
            let dot = q1x * q2x + q1y * q2y + q1z * q2z
            Quaternion (q1x * q2w + q2x * q1w + cx, q1y * q2w + q2y * q1w + cy, q1z * q2w + q2z * q1w + cz, q1w * q2w - dot)

        /// <summary>Concatenates two quaternions so <c>value1</c> is applied before <c>value2</c>.</summary>
        static member Concatenate(value1: Quaternion, value2: Quaternion) : Quaternion =
            value2 * value1
#endif

/// <summary>Policy applied when a scalar observation is non-finite or rejected by the continuous color scale.</summary>
[<RequireQualifiedAccess; Struct>]
type MissingColorPolicy =
    /// <summary>Omits the observation by returning <c>ValueNone</c> from <c>ColorRamp.tryMap</c>.</summary>
    | Omit
    /// <summary>Substitutes the supplied fallback color instead of omitting the observation.</summary>
    | Fallback of RgbaColor

/// <summary>Out-of-range clipping overrides and missing-value policy for <c>ColorRamp</c>.</summary>
[<Struct>]
type ColorClipPolicy =
    {
        /// <summary>Optional color returned when normalized scale progress falls below <c>0.0</c>; <c>ValueNone</c> clamps to the lowest stop.</summary>
        LowClip: RgbaColor voption
        /// <summary>Optional color returned when normalized scale progress exceeds <c>1.0</c>; <c>ValueNone</c> clamps to the highest stop.</summary>
        HighClip: RgbaColor voption
        /// <summary>Behavior when an observation is non-finite or outside the continuous scale transform.</summary>
        Missing: MissingColorPolicy
    }

/// <summary>Default values and pipeable helpers for <c>ColorClipPolicy</c>.</summary>
[<RequireQualifiedAccess>]
module ColorClipPolicy =
    /// <summary>Clamps out-of-range observations to ramp endpoints and omits missing or non-finite values.</summary>
    let clampAndOmit: ColorClipPolicy =
        {
            LowClip = ValueNone
            HighClip = ValueNone
            Missing = MissingColorPolicy.Omit
        }

    /// <summary>Returns a copy of <c>policy</c> with explicit low and high out-of-range clip colors.</summary>
    let withClips (lowClip: RgbaColor voption) (highClip: RgbaColor voption) (policy: ColorClipPolicy) : ColorClipPolicy =
        { policy with
            LowClip = lowClip
            HighClip = highClip
        }

/// <summary>Controls whether 3D geometry preserves raw perceptual palette colors (<c>Unlit</c> / <c>ExactUnlit</c>) or receives studio lighting.</summary>
[<RequireQualifiedAccess; Struct>]
type ShadingMode =
    /// <summary>Renders raw albedo colors without directional surface shading (subject to scene tone-mapping).</summary>
    | Unlit
    /// <summary>Preserves exact linear/sRGB brand and categorical colors without directional lighting or filmic tone-mapping.</summary>
    | ExactUnlit
    /// <summary>Applies studio hemisphere and key/rim lighting with explicit surface roughness and emissive boost.</summary>
    | Lit of roughness: float32 * emissive: float32

/// <summary>Immutable 256-entry GPU colormap lookup table (or scalar passthrough sentinel) paired with a continuous scale and clipping policy.</summary>
type ColorRamp =
    private
        {
            Transform: RampTransform
            Clip: ColorClipPolicy
            Lut: RgbaColor array
            ScalarPassthrough: bool
        }

    member internal this.ClipPolicy = this.Clip
    member internal this.LowStop = this.Lut[0]
    member internal this.HighStop = this.Lut[this.Lut.Length - 1]
    member internal this.IsScalarPassthrough = this.ScalarPassthrough

/// <summary>Constructors, built-in perceptually uniform palettes, and zero-allocation lookups for <c>ColorRamp</c>.</summary>
[<RequireQualifiedAccess>]
module ColorRamp =
    let private hexStop value =
        ColorSpace.rgba
            (float ((value >>> 16) &&& 255) / 255.0)
            (float ((value >>> 8) &&& 255) / 255.0)
            (float (value &&& 255) / 255.0)
            1.0

    let private viridisPalette =
        [
            0x440154
            0x482475
            0x414487
            0x355f8d
            0x2a788e
            0x21918c
            0x22a884
            0x44bf70
            0x7ad151
            0xbddf26
            0xfde725
        ]
        |> List.map hexStop

    let private turboPalette =
        [
            0x30123b
            0x4454c4
            0x4490fe
            0x1fc8dd
            0x29ef8c
            0xa2fc3c
            0xe1dd37
            0xfea632
            0xf05b12
            0xc42503
            0x7a0403
        ]
        |> List.map hexStop

    let private coolWarmPalette =
        [
            0x3b4cc0
            0x5977e3
            0x7b9ff9
            0x9ebeff
            0xdddddd
            0xf2cbb7
            0xf7a889
            0xe26952
            0xb40426
        ]
        |> List.map hexStop

    let private plasmaPalette =
        [
            0x0d0887
            0x46039f
            0x7201a8
            0x9c179e
            0xbd3786
            0xd8576b
            0xed7953
            0xfb9f3a
            0xfdca26
            0xf0f921
        ]
        |> List.map hexStop

    let private infernoPalette =
        [
            0x000004
            0x1b0c41
            0x4a0c6b
            0x781c6d
            0xa52c60
            0xcf4446
            0xed6925
            0xfb9b06
            0xf7d13d
            0xfcffa4
        ]
        |> List.map hexStop

    let private magmaPalette =
        [
            0x000004
            0x180f3d
            0x440f76
            0x721f81
            0x9e2f7f
            0xcd4071
            0xf1605d
            0xfd9668
            0xfeca8d
            0xfcfdbf
        ]
        |> List.map hexStop

    let private cividisPalette =
        [
            0x00224e
            0x123570
            0x3b496c
            0x575d6d
            0x707173
            0x8a8678
            0xa59c74
            0xc3b369
            0xe1cc55
            0xfee838
        ]
        |> List.map hexStop

    let private hexColor (value: int) : RgbaColor =
        RgbaColor (
            float32 ((value >>> 16) &&& 255) / 255.0f,
            float32 ((value >>> 8) &&& 255) / 255.0f,
            float32 (value &&& 255) / 255.0f,
            1.0f
        )

    /// <summary>10-color Tableau categorical palette in sRGB <c>RgbaColor</c>.</summary>
    let tableau10: RgbaColor list =
        [
            0x4e79a7
            0xf28e2b
            0xe15759
            0x76b7b2
            0x59a14f
            0xedc948
            0xb07aa1
            0xff9da7
            0x9c755f
            0xbab0ac
        ]
        |> List.map hexColor

    /// <summary>8-color Okabe-Ito colorblind-safe categorical palette in sRGB <c>RgbaColor</c>.</summary>
    let okabeIto: RgbaColor list =
        [
            0x000000
            0xe69f00
            0x56b4e9
            0x009e73
            0xf0e442
            0x0072b2
            0xd55e00
            0xcc79a7
        ]
        |> List.map hexColor

    let private samplePaletteLut (eval: float -> Rgba) : RgbaColor array =
        let lut = Array.zeroCreate<RgbaColor> 256

        for i = 0 to 255 do
            let c = eval (float i / 255.0)
            lut[i] <- RgbaColor (float32 c.Red, float32 c.Green, float32 c.Blue, float32 c.Alpha)

        lut

    let private sampleNormalizedLut (lut: RgbaColor array) (t: float) : RgbaColor =
        if lut.Length <= 1 then
            lut[0]
        else
            let maxIdx = lut.Length - 1
            let scaled = t * float maxIdx

            if scaled <= 0.0 then
                lut[0]
            elif scaled >= float maxIdx then
                lut[maxIdx]
            else
                let i = int scaled
                let frac = float32 (scaled - float i)
                let c0 = lut[i]
                let c1 = lut[i + 1]

                RgbaColor (
                    c0.R + (c1.R - c0.R) * frac,
                    c0.G + (c1.G - c0.G) * frac,
                    c0.B + (c1.B - c0.B) * frac,
                    c0.A + (c1.A - c0.A) * frac
                )

    let private buildRamp (scale: Continuous) (eval: float -> Rgba) (clip: ColorClipPolicy) : ColorRamp =
        if isNull (box scale) then
            nullArg (nameof scale)

        {
            Transform = RampTransform.classify scale
            Clip = clip
            Lut = samplePaletteLut eval
            ScalarPassthrough = false
        }

    /// <summary>Builds a 256-entry sequential CIE Lab colormap ramp over <c>scale</c>, <c>palette</c>, and <c>clip</c>.</summary>
    let sequential (scale: Continuous) (palette: Rgba list) (clip: ColorClipPolicy) : ColorRamp =
        if isNull (box palette) then
            nullArg (nameof palette)

        let eval = Chromatic.sequential ColorInterpolation.Lab GamutPolicy.Clip palette
        buildRamp scale eval clip

    /// <summary>Builds a 256-entry diverging CIE Lab colormap ramp requiring an odd palette of at least three stops.</summary>
    let diverging (scale: Continuous) (palette: Rgba list) (clip: ColorClipPolicy) : ColorRamp =
        if isNull (box palette) then
            nullArg (nameof palette)

        let eval = Chromatic.diverging ColorInterpolation.Lab GamutPolicy.Clip palette
        buildRamp scale eval clip

    /// <summary>Creates a perceptually uniform Viridis sequential ramp over <c>domain</c> with clamping and missing-value omission.</summary>
    let viridis (domain: float * float) : ColorRamp =
        sequential (Scale.linear domain (0.0, 1.0)) viridisPalette ColorClipPolicy.clampAndOmit

    /// <summary>Creates a smooth Turbo rainbow sequential ramp over <c>domain</c> with clamping and missing-value omission.</summary>
    let turbo (domain: float * float) : ColorRamp =
        sequential (Scale.linear domain (0.0, 1.0)) turboPalette ColorClipPolicy.clampAndOmit

    /// <summary>Creates a Moreland Cool-Warm diverging ramp over <c>domain</c> with a neutral midpoint, clamping, and missing-value omission.</summary>
    let coolWarm (domain: float * float) : ColorRamp =
        diverging (Scale.linear domain (0.0, 1.0)) coolWarmPalette ColorClipPolicy.clampAndOmit

    /// <summary>Creates a perceptually uniform Plasma sequential ramp over <c>domain</c> with clamping and missing-value omission.</summary>
    let plasma (domain: float * float) : ColorRamp =
        sequential (Scale.linear domain (0.0, 1.0)) plasmaPalette ColorClipPolicy.clampAndOmit

    /// <summary>Creates a perceptually uniform Inferno sequential ramp over <c>domain</c> with clamping and missing-value omission.</summary>
    let inferno (domain: float * float) : ColorRamp =
        sequential (Scale.linear domain (0.0, 1.0)) infernoPalette ColorClipPolicy.clampAndOmit

    /// <summary>Creates a perceptually uniform Magma sequential ramp over <c>domain</c> with clamping and missing-value omission.</summary>
    let magma (domain: float * float) : ColorRamp =
        sequential (Scale.linear domain (0.0, 1.0)) magmaPalette ColorClipPolicy.clampAndOmit

    /// <summary>Creates a colorblind-friendly Cividis sequential ramp over <c>domain</c> with clamping and missing-value omission.</summary>
    let cividis (domain: float * float) : ColorRamp =
        sequential (Scale.linear domain (0.0, 1.0)) cividisPalette ColorClipPolicy.clampAndOmit

    /// <summary>Builds a deterministic category-to-<c>RgbaColor</c> lookup map cycling through <c>palette</c> in <c>categories</c> order.</summary>
    let categorical (palette: Rgba list) (categories: 'K list) : Map<'K, RgbaColor> when 'K: comparison =
        if isNull (box palette) then
            nullArg (nameof palette)

        if List.isEmpty palette then
            invalidArg (nameof palette) "Categorical palette requires at least one color."

        if isNull (box categories) then
            nullArg (nameof categories)

        let colors =
            palette
            |> List.map (fun c ->
                let srgb = ColorSpace.applyGamut GamutPolicy.Clip c
                RgbaColor (float32 srgb.Red, float32 srgb.Green, float32 srgb.Blue, float32 srgb.Alpha))
            |> List.toArray

        categories
        |> List.mapi (fun i k -> k, colors[i % colors.Length])
        |> Map.ofList

    /// <summary>
    /// Creates a scalar-passthrough <c>ColorRamp</c> encoding finite raw <c>float32</c> scalars directly into the vertex red channel (<c>RgbaColor (float32 v, 1.0f, 1.0f, 1.0f)</c>)
    /// while applying <c>missing</c> for non-finite values, for use with <c>ColorShader.packScopedUniforms</c> and <c>InstanceFlags.UseColorLut</c>.
    /// </summary>
    let scalarPassthroughWithMissing (missing: MissingColorPolicy) : ColorRamp =
        {
            Transform = RampTransform.classify (Scale.linear (0.0, 1.0) (0.0, 1.0))
            Clip = { ColorClipPolicy.clampAndOmit with Missing = missing }
            Lut = [| RgbaColor.white |]
            ScalarPassthrough = true
        }

    /// <summary>
    /// Scalar-passthrough <c>ColorRamp</c> omitting non-finite values (<c>MissingColorPolicy.Omit</c>) and preserving unclamped raw scalars in vertex <c>R</c>
    /// for scoped <c>ColorShader.packScopedUniforms</c> GPU LUT shading.
    /// </summary>
    let scalarPassthrough: ColorRamp = scalarPassthroughWithMissing MissingColorPolicy.Omit

    let private missingColor (missing: MissingColorPolicy) : RgbaColor voption =
        match missing with
        | MissingColorPolicy.Omit -> ValueNone
        | MissingColorPolicy.Fallback c -> ValueSome c

    let private mapNormalized (ramp: ColorRamp) (t: float) : RgbaColor voption =
        if not (Check3D.isFinite t) then
            missingColor ramp.Clip.Missing
        elif t < 0.0 then
            match ramp.Clip.LowClip with
            | ValueSome c -> ValueSome c
            | ValueNone -> ValueSome ramp.Lut[0]
        elif t > 1.0 then
            match ramp.Clip.HighClip with
            | ValueSome c -> ValueSome c
            | ValueNone -> ValueSome ramp.Lut[ramp.Lut.Length - 1]
        else
            ValueSome (sampleNormalizedLut ramp.Lut t)

    /// <summary>Maps a scalar <c>value</c> into an <c>RgbaColor</c> in <c>O(1)</c> with zero heap allocations on linear, logarithmic, constant, and scalar-passthrough ramps.</summary>
    let tryMap (ramp: ColorRamp) (value: float) : RgbaColor voption =
        if isNull (box ramp) then
            nullArg (nameof ramp)

        if ramp.ScalarPassthrough then
            if Check3D.isFinite value then
                let s = float32 value

                if Check3D.isFinite32 s then
                    ValueSome (RgbaColor (s, 1.0f, 1.0f, 1.0f))
                else
                    missingColor ramp.Clip.Missing
            else
                missingColor ramp.Clip.Missing
        else
            let t = RampTransform.eval ramp.Transform value
            if Check3D.isFinite t then
                mapNormalized ramp t
            else
                missingColor ramp.Clip.Missing

    /// <summary>Maps a scalar <c>value</c> through <c>ramp</c>, returning <c>fallback</c> when <c>tryMap</c> yields <c>ValueNone</c>.</summary>
    let mapOrFallback (ramp: ColorRamp) (value: float) (fallback: RgbaColor) : RgbaColor =
        match tryMap ramp value with
        | ValueSome c -> c
        | ValueNone -> fallback

    /// <summary>Samples <c>count</c> evenly spaced <c>RgbaColor</c> stops across the ramp's <c>[0.0, 1.0]</c> range.</summary>
    let stops (count: int) (ramp: ColorRamp) : ReadOnlyMemory<RgbaColor> =
        if isNull (box ramp) then
            nullArg (nameof ramp)

        if count < 0 then
            raise (ArgumentOutOfRangeException (nameof count, "Stop count must be nonnegative."))
        elif count = 0 then
            ReadOnlyMemory<RgbaColor>.Empty
        elif count = 1 then
            ReadOnlyMemory<RgbaColor> [| ramp.Lut[0] |]
        else
            let arr = Array.zeroCreate<RgbaColor> count
            let denom = float (count - 1)

            for i = 0 to count - 1 do
                arr[i] <- sampleNormalizedLut ramp.Lut (float i / denom)

            ReadOnlyMemory<RgbaColor>(arr)

/// <summary>Conversions between GPU <c>RgbaColor</c>, <c>Ranvier.Anime.Rgba</c>, and CSS <c>Ranvier.D5.Paint</c>.</summary>
[<RequireQualifiedAccess>]
module ColorConv3D =
    let private hexDigit (ch: char) : int =
        if ch >= '0' && ch <= '9' then int ch - int '0'
        elif ch >= 'a' && ch <= 'f' then 10 + int ch - int 'a'
        elif ch >= 'A' && ch <= 'F' then 10 + int ch - int 'A'
        else -1

    let private parseHexByte (hi: char) (lo: char) : float32 voption =
        let a = hexDigit hi
        let b = hexDigit lo
        if a < 0 || b < 0 then ValueNone else ValueSome (float32 ((a <<< 4) ||| b) / 255.0f)

    let private parseHexNibble (ch: char) : float32 voption =
        let a = hexDigit ch
        if a < 0 then ValueNone else ValueSome (float32 ((a <<< 4) ||| a) / 255.0f)

    let private tryParseFloat (s: string) : float voption =
        let ok, v =
#if FABLE_COMPILER
            Double.TryParse s
#else
            Double.TryParse (s, NumberStyles.Float, CultureInfo.InvariantCulture)
#endif
        if ok && Check3D.isFinite v then ValueSome v else ValueNone

    let private tryParseNormalized (percentDenom: float) (unitDenom: float) (token: string) : float voption =
        let s = token.Trim ()
        if s.Length = 0 then
            ValueNone
        elif s.EndsWith ("%", StringComparison.Ordinal) then
            match tryParseFloat (s.Substring (0, s.Length - 1)) with
            | ValueSome v -> ValueSome (Math.Clamp (v / percentDenom, 0.0, 1.0))
            | ValueNone -> ValueNone
        else
            match tryParseFloat s with
            | ValueSome v -> ValueSome (Math.Clamp (v / unitDenom, 0.0, 1.0))
            | ValueNone -> ValueNone

    let private tryParseRgbChannel (token: string) = tryParseNormalized 100.0 255.0 token

    let private tryParseAlphaChannel (token: string) = tryParseNormalized 100.0 1.0 token

    /// <summary>Converts an <c>Rgba</c> color in <c>[0, 1]</c> into a clipped <c>RgbaColor</c>.</summary>
    let ofRgba (c: Rgba) : RgbaColor =
        Check3D.finite "c.Red" c.Red
        Check3D.finite "c.Green" c.Green
        Check3D.finite "c.Blue" c.Blue
        Check3D.finite "c.Alpha" c.Alpha

        RgbaColor (
            float32 (Math.Clamp (c.Red, 0.0, 1.0)),
            float32 (Math.Clamp (c.Green, 0.0, 1.0)),
            float32 (Math.Clamp (c.Blue, 0.0, 1.0)),
            float32 (Math.Clamp (c.Alpha, 0.0, 1.0))
        )

    /// <summary>Converts an <c>RgbaColor</c> into a <c>Ranvier.Anime.Rgba</c> color.</summary>
    let toRgba (c: RgbaColor) : Rgba =
        Check3D.finite32 "c.R" c.R
        Check3D.finite32 "c.G" c.G
        Check3D.finite32 "c.B" c.B
        Check3D.finite32 "c.A" c.A
        ColorSpace.rgba (float c.R) (float c.G) (float c.B) (float c.A)

    /// <summary>Serializes an <c>RgbaColor</c> into a CSS <c>Ranvier.D5.Paint</c> using straight alpha in sRGB gamut.</summary>
    let toPaint (c: RgbaColor) : Paint =
        Chromatic.paint GamutPolicy.Clip (toRgba c)

    /// <summary>Attempts to parse a CSS <c>Paint</c> (<c>#rgb</c>, <c>#rgba</c>, <c>#rrggbb</c>, <c>#rrggbbaa</c>, <c>rgb(...)</c>, <c>rgba(...)</c>, or standard named colors) into <c>RgbaColor</c>; returns <c>ValueNone</c> for <c>none</c>, <c>currentColor</c>, or unsupported syntax.</summary>
    let tryOfPaint (paint: Paint) : RgbaColor voption =
        if isNull (box paint) then
            nullArg (nameof paint)

        let raw = Paint.toCss paint
        if String.IsNullOrWhiteSpace raw then
            ValueNone
        else
            let s = raw.Trim().ToLowerInvariant()
            if s = "none" || s = "currentcolor" then
                ValueNone
            elif s.StartsWith ("#", StringComparison.Ordinal) then
                match s.Length with
                | 4 ->
                    match parseHexNibble s[1], parseHexNibble s[2], parseHexNibble s[3] with
                    | ValueSome r, ValueSome g, ValueSome b -> ValueSome (RgbaColor (r, g, b, 1.0f))
                    | _ -> ValueNone
                | 5 ->
                    match parseHexNibble s[1], parseHexNibble s[2], parseHexNibble s[3], parseHexNibble s[4] with
                    | ValueSome r, ValueSome g, ValueSome b, ValueSome a -> ValueSome (RgbaColor (r, g, b, a))
                    | _ -> ValueNone
                | 7 ->
                    match parseHexByte s[1] s[2], parseHexByte s[3] s[4], parseHexByte s[5] s[6] with
                    | ValueSome r, ValueSome g, ValueSome b -> ValueSome (RgbaColor (r, g, b, 1.0f))
                    | _ -> ValueNone
                | 9 ->
                    match parseHexByte s[1] s[2], parseHexByte s[3] s[4], parseHexByte s[5] s[6], parseHexByte s[7] s[8] with
                    | ValueSome r, ValueSome g, ValueSome b, ValueSome a -> ValueSome (RgbaColor (r, g, b, a))
                    | _ -> ValueNone
                | _ -> ValueNone
            elif (s.StartsWith ("rgb(", StringComparison.Ordinal) || s.StartsWith ("rgba(", StringComparison.Ordinal))
                 && s.EndsWith (")", StringComparison.Ordinal) then
                let openIdx = s.IndexOf '('
                let inner = s.Substring (openIdx + 1, s.Length - openIdx - 2)
                let parts = inner.Split ([| ','; ' '; '\t'; '/' |], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length = 3 then
                    match tryParseRgbChannel parts[0], tryParseRgbChannel parts[1], tryParseRgbChannel parts[2] with
                    | ValueSome r, ValueSome g, ValueSome b -> ValueSome (RgbaColor (float32 r, float32 g, float32 b, 1.0f))
                    | _ -> ValueNone
                elif parts.Length = 4 then
                    match tryParseRgbChannel parts[0], tryParseRgbChannel parts[1], tryParseRgbChannel parts[2], tryParseAlphaChannel parts[3] with
                    | ValueSome r, ValueSome g, ValueSome b, ValueSome a -> ValueSome (RgbaColor (float32 r, float32 g, float32 b, float32 a))
                    | _ -> ValueNone
                else
                    ValueNone
            else
                match s with
                | "transparent" -> ValueSome (RgbaColor (0.0f, 0.0f, 0.0f, 0.0f))
                | "black" -> ValueSome (RgbaColor (0.0f, 0.0f, 0.0f, 1.0f))
                | "white" -> ValueSome (RgbaColor (1.0f, 1.0f, 1.0f, 1.0f))
                | "red" -> ValueSome (RgbaColor (1.0f, 0.0f, 0.0f, 1.0f))
                | "green" -> ValueSome (RgbaColor (0.0f, 128.0f / 255.0f, 0.0f, 1.0f))
                | "blue" -> ValueSome (RgbaColor (0.0f, 0.0f, 1.0f, 1.0f))
                | "yellow" -> ValueSome (RgbaColor (1.0f, 1.0f, 0.0f, 1.0f))
                | "cyan" | "aqua" -> ValueSome (RgbaColor (0.0f, 1.0f, 1.0f, 1.0f))
                | "magenta" | "fuchsia" -> ValueSome (RgbaColor (1.0f, 0.0f, 1.0f, 1.0f))
                | "gray" | "grey" -> ValueSome (RgbaColor (128.0f / 255.0f, 128.0f / 255.0f, 128.0f / 255.0f, 1.0f))
                | "silver" -> ValueSome (RgbaColor (192.0f / 255.0f, 192.0f / 255.0f, 192.0f / 255.0f, 1.0f))
                | "orange" -> ValueSome (RgbaColor (1.0f, 165.0f / 255.0f, 0.0f, 1.0f))
                | "purple" -> ValueSome (RgbaColor (128.0f / 255.0f, 0.0f, 128.0f / 255.0f, 1.0f))
                | _ -> ValueNone

/// <summary>Twist-free 3D vector orientation quaternions and anisotropic glyph transform matrices.</summary>
[<RequireQualifiedAccess>]
module Orientation3D =
    /// <summary>
    /// Computes a unit quaternion rotating <c>+Vector3.UnitY</c> onto <c>Normalize direction</c>.
    /// Antiparallel <c>-Vector3.UnitY</c> rotates by 180 degrees around <c>+X</c> without <c>NaN</c>s, and near-zero or non-finite directions return <c>Quaternion.Identity</c>.
    /// </summary>
    let alignYToVector (direction: Vector3) : Quaternion =
        if not (Check3D.isFinite32 direction.X && Check3D.isFinite32 direction.Y && Check3D.isFinite32 direction.Z) then
            Quaternion.Identity
        else
            let lenSq = direction.LengthSquared ()

            if lenSq < 1e-12f then
                Quaternion.Identity
            else
                let dx = float direction.X
                let dy = float direction.Y
                let dz = float direction.Z
                let m = max (abs dx) (max (abs dy) (abs dz))
                let sx, sy, sz = dx / m, dy / m, dz / m
                let norm = sqrt (sx * sx + sy * sy + sz * sz)
                let ux, uy, uz = sx / norm, sy / norm, sz / norm

                if uy >= 1.0 - 1e-11 then
                    Quaternion.Identity
                elif 1.0 + uy < 1e-11 then
                    Quaternion (1.0f, 0.0f, 0.0f, 0.0f)
                else
                    let w = 1.0 + uy
                    let qLen = sqrt (uz * uz + ux * ux + w * w)
                    Quaternion (float32 (uz / qLen), 0.0f, float32 (-ux / qLen), float32 (w / qLen))

    let internal matrixFromQuaternion (q: Quaternion) : Matrix4x4 =
#if FABLE_COMPILER
        let xx = q.X * q.X
        let yy = q.Y * q.Y
        let zz = q.Z * q.Z
        let xy = q.X * q.Y
        let xz = q.X * q.Z
        let yz = q.Y * q.Z
        let wx = q.W * q.X
        let wy = q.W * q.Y
        let wz = q.W * q.Z

        Matrix4x4 (
            1.0f - 2.0f * (yy + zz),
            2.0f * (xy + wz),
            2.0f * (xz - wy),
            0.0f,
            2.0f * (xy - wz),
            1.0f - 2.0f * (xx + zz),
            2.0f * (yz + wx),
            0.0f,
            2.0f * (xz + wy),
            2.0f * (yz - wx),
            1.0f - 2.0f * (xx + yy),
            0.0f,
            0.0f,
            0.0f,
            0.0f,
            1.0f
        )
#else
        Matrix4x4.CreateFromQuaternion q
#endif

    /// <summary>Builds a row-vector world matrix scaling local XZ by <c>radiusXZ</c>, local +Y by <c>lengthY</c>, aligning +Y onto <c>direction</c>, and translating to <c>position</c>.</summary>
    let anisotropicMatrix (position: Vector3) (direction: Vector3) (radiusXZ: float32) (lengthY: float32) : Matrix4x4 =
        Check3D.vector3 (nameof position) position
        Check3D.nonNegative32 (nameof radiusXZ) radiusXZ
        Check3D.nonNegative32 (nameof lengthY) lengthY
        let rot = alignYToVector direction

        Matrix4x4.CreateScale (radiusXZ, lengthY, radiusXZ)
        * matrixFromQuaternion rot
        * Matrix4x4.CreateTranslation position

/// <summary>Identifies one of the three data-coordinate projection walls of the 3D axis box.</summary>
[<RequireQualifiedAccess; Struct>]
type WallPlane =
    /// <summary>Constant-Z data floor/ceiling wall carrying <c>(X, Y)</c> projections.</summary>
    | XY
    /// <summary>Constant-Y data side wall carrying <c>(X, Z)</c> projections.</summary>
    | XZ
    /// <summary>Constant-X data side wall carrying <c>(Y, Z)</c> projections.</summary>
    | YZ

/// <summary>Projects 3D data observations and 2D contour polygons onto the active camera-backfacing cuboid walls.</summary>
[<RequireQualifiedAccess>]
module WallProjection3D =
    let private validateWalls (struct (bx, fy, bz)) =
        Check3D.finite32 "activeBackWalls" bx
        Check3D.finite32 "activeBackWalls" fy
        Check3D.finite32 "activeBackWalls" bz

    /// <summary>Maps a data <c>Point3</c> into world space and collapses the coordinate perpendicular to <c>plane</c> onto <c>activeBackWalls</c>.</summary>
    let projectPoint
        (scale: Scale3D)
        (activeBackWalls: struct (float32 * float32 * float32))
        (plane: WallPlane)
        (point: Point3)
        : Vector3 =
        if isNull (box scale) then
            nullArg (nameof scale)

        validateWalls activeBackWalls
        let struct (bx, fy, bz) = activeBackWalls
        let world = Scale3D.map scale point

        match scale.UpAxis, plane with
        | UpAxis.ZUp, WallPlane.XY -> Vector3 (world.X, fy, world.Z)
        | UpAxis.ZUp, WallPlane.XZ -> Vector3 (world.X, world.Y, bz)
        | UpAxis.ZUp, WallPlane.YZ -> Vector3 (bx, world.Y, world.Z)
        | UpAxis.YUp, WallPlane.XY -> Vector3 (world.X, world.Y, bz)
        | UpAxis.YUp, WallPlane.XZ -> Vector3 (world.X, fy, world.Z)
        | UpAxis.YUp, WallPlane.YZ -> Vector3 (bx, world.Y, world.Z)

    let private mapAxisPair
        (sU: AxisScale3D)
        (extU: float)
        (sV: AxisScale3D)
        (extV: float)
        (u: float)
        (v: float)
        : struct (float32 * float32) voption =
        match AxisScale3D.tryMapNormalized sU u, AxisScale3D.tryMapNormalized sV v with
        | ValueSome tu, ValueSome tv ->
            let wu = float32 ((tu - 0.5) * extU)
            let wv = float32 ((tv - 0.5) * extV)
            if Check3D.isFinite32 wu && Check3D.isFinite32 wv then ValueSome struct (wu, wv) else ValueNone
        | _ -> ValueNone

    let private tryProject2D
        (scale: Scale3D)
        (bx: float32)
        (fy: float32)
        (bz: float32)
        (plane: WallPlane)
        (p: Point)
        : Vector3 voption =
        let struct (dx, dy, dz) = scale.DataExtents

        match plane with
        | WallPlane.XY ->
            match mapAxisPair scale.XScale dx scale.YScale dy p.X p.Y with
            | ValueSome struct (wx, wy) ->
                match scale.UpAxis with
                | UpAxis.ZUp -> ValueSome (Vector3 (wx, fy, -wy))
                | UpAxis.YUp -> ValueSome (Vector3 (wx, wy, bz))
            | ValueNone -> ValueNone
        | WallPlane.XZ ->
            match mapAxisPair scale.XScale dx scale.ZScale dz p.X p.Y with
            | ValueSome struct (wx, wz) ->
                match scale.UpAxis with
                | UpAxis.ZUp -> ValueSome (Vector3 (wx, wz, bz))
                | UpAxis.YUp -> ValueSome (Vector3 (wx, fy, wz))
            | ValueNone -> ValueNone
        | WallPlane.YZ ->
            match mapAxisPair scale.YScale dy scale.ZScale dz p.X p.Y with
            | ValueSome struct (wy, wz) ->
                match scale.UpAxis with
                | UpAxis.ZUp -> ValueSome (Vector3 (bx, wz, -wy))
                | UpAxis.YUp -> ValueSome (Vector3 (bx, wy, wz))
            | ValueNone -> ValueNone

    /// <summary>
    /// Projects 2D <c>ContourPolygon</c> rings and holes onto the active <c>plane</c> back wall as paired <c>LineList</c> vertices
    /// with <c>ignoreSectionPlane = true</c> (<c>Pad = 1.0f</c>) so back-wall contours remain visible under active <c>SectionPlane</c> cuts.
    /// </summary>
    let projectContours
        (scale: Scale3D)
        (activeBackWalls: struct (float32 * float32 * float32))
        (plane: WallPlane)
        (color: RgbaColor)
        (contours: ContourPolygon list)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        validateWalls activeBackWalls

        if isNull (box contours) then
            nullArg (nameof contours)

        let struct (bx, fy, bz) = activeBackWalls
        let vertices = ResizeArray<LineVertex3D>()

        let appendRing (ring: Point list) =
            let mutable prev = ValueNone

            for pt in ring do
                let mapped = tryProject2D scale bx fy bz plane pt

                match prev, mapped with
                | ValueSome a, ValueSome b ->
                    vertices.Add (LineVertex3D (a, color, true))
                    vertices.Add (LineVertex3D (b, color, true))
                | _ -> ()

                prev <- mapped

        for poly in contours do
            appendRing poly.Outer

            for hole in poly.Holes do
                appendRing hole

        if vertices.Count = 0 then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            ReadOnlyMemory<LineVertex3D>(vertices.ToArray ())

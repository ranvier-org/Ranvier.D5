namespace Ranvier.D5.Wgpu

open System
open System.Numerics
open Ranvier.Anime
open Ranvier.D5
open Ranvier.Wgpu

#if FABLE_COMPILER
/// <summary>Browser/Fable compatibility stand-ins for <c>System.Numerics.Quaternion</c> members not lowered by Fickl 0.11.0.</summary>
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

/// <summary>Controls whether 3D geometry preserves raw perceptual palette colors (<c>Unlit</c>) or receives studio lighting.</summary>
[<RequireQualifiedAccess; Struct>]
type ShadingMode =
    /// <summary>Renders exact colormap colors without directional surface shading.</summary>
    | Unlit
    /// <summary>Applies studio hemisphere and key/rim lighting with explicit surface roughness and emissive boost.</summary>
    | Lit of roughness: float32 * emissive: float32

[<Struct>]
type internal RampTransform =
    | Linear of x0: float * span: float * clamped: bool
    | Log of sign: float * logAbs0: float * logSpan: float * clamped: bool
    | Constant of value: float
    | General of scale: Continuous

module internal RampTransform =
    let inline eval (transform: RampTransform) (value: float) : float =
        if not (Check3D.isFinite value) then
            Double.NaN
        else
            match transform with
            | RampTransform.Linear (x0, span, clamped) ->
                let raw = (value - x0) / span
                let t = if clamped then Math.Clamp (raw, 0.0, 1.0) else raw
                if Check3D.isFinite t then t else Double.NaN
            | RampTransform.Log (sign, la, lSpan, clamped) ->
                if value * sign <= 0.0 then
                    Double.NaN
                else
                    let raw = (Math.Log (abs value) - la) / lSpan
                    let t = if clamped then Math.Clamp (raw, 0.0, 1.0) else raw
                    if Check3D.isFinite t then t else Double.NaN
            | RampTransform.Constant t -> t
            | RampTransform.General scale ->
                match Scale.tryMap scale value with
                | Some t when Check3D.isFinite t -> t
                | _ -> Double.NaN

/// <summary>Immutable 256-entry GPU colormap lookup table paired with a continuous scale and clipping policy.</summary>
type ColorRamp =
    private
        {
            Transform: RampTransform
            Clip: ColorClipPolicy
            Lut: RgbaColor array
        }

    member internal this.ClipPolicy = this.Clip
    member internal this.LowStop = this.Lut[0]
    member internal this.HighStop = this.Lut[this.Lut.Length - 1]

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

    let private approxEq (a: float) (b: float) =
        abs (a - b) <= 1e-9 * max 1.0 (max (abs a) (abs b))

    let private matchesAt (scale: Continuous) (x: float) (expected: float) =
        match Scale.tryMap scale x with
        | Some actual -> approxEq actual expected
        | None -> false

    let internal classifyTransform (scale: Continuous) : RampTransform =
        try
            let x0 = Scale.invert scale 0.0
            let x1 = Scale.invert scale 1.0

            if not (Check3D.isFinite x0 && Check3D.isFinite x1) then
                RampTransform.General scale
            elif x0 = x1 then
                match Scale.tryMap scale x0 with
                | Some mid when
                    Check3D.isFinite mid
                    && matchesAt scale (x0 - 1.0) mid
                    && matchesAt scale (x0 + 1.0) mid
                    && matchesAt scale (-abs x0 - 1.0) mid
                    && matchesAt scale 0.0 mid
                    ->
                    RampTransform.Constant mid
                | _ -> RampTransform.General scale
            else
                let span = x1 - x0

                if
                    Check3D.isFinite span
                    && matchesAt scale x0 0.0
                    && matchesAt scale x1 1.0
                    && matchesAt scale (x0 + 0.25 * span) 0.25
                    && matchesAt scale (x0 + 0.5 * span) 0.5
                    && matchesAt scale (x0 + 0.75 * span) 0.75
                then
                    let xNeg = x0 - 0.5 * span
                    let xPos = x1 + 0.5 * span

                    if Check3D.isFinite xNeg && Check3D.isFinite xPos then
                        if matchesAt scale xNeg -0.5 && matchesAt scale xPos 1.5 then
                            RampTransform.Linear (x0, span, false)
                        elif matchesAt scale xNeg 0.0 && matchesAt scale xPos 1.0 then
                            RampTransform.Linear (x0, span, true)
                        else
                            RampTransform.General scale
                    else
                        RampTransform.General scale
                elif x0 * x1 > 0.0 then
                    let sign = if x0 < 0.0 then -1.0 else 1.0
                    let la = Math.Log (abs x0)
                    let lb = Math.Log (abs x1)
                    let lSpan = lb - la

                    if
                        Check3D.isFinite lSpan
                        && lSpan <> 0.0
                        && matchesAt scale x0 0.0
                        && matchesAt scale x1 1.0
                        && matchesAt scale (sign * Math.Exp (la + 0.25 * lSpan)) 0.25
                        && matchesAt scale (sign * Math.Exp (la + 0.5 * lSpan)) 0.5
                        && matchesAt scale (sign * Math.Exp (la + 0.75 * lSpan)) 0.75
                        && (Scale.tryMap scale 0.0).IsNone
                        && (Scale.tryMap scale (-x0)).IsNone
                    then
                        let xNeg = sign * Math.Exp (la - 0.25 * lSpan)
                        let xPos = sign * Math.Exp (lb + 0.25 * lSpan)

                        if Check3D.isFinite xNeg && Check3D.isFinite xPos then
                            if matchesAt scale xNeg -0.25 && matchesAt scale xPos 1.25 then
                                RampTransform.Log (sign, la, lSpan, false)
                            elif matchesAt scale xNeg 0.0 && matchesAt scale xPos 1.0 then
                                RampTransform.Log (sign, la, lSpan, true)
                            else
                                RampTransform.General scale
                        else
                            RampTransform.General scale
                    else
                        RampTransform.General scale
                else
                    RampTransform.General scale
        with :? ArgumentException ->
            RampTransform.General scale

    let private samplePaletteLut (eval: float -> Rgba) : RgbaColor array =
        let lut = Array.zeroCreate<RgbaColor> 256

        for i = 0 to 255 do
            let c = eval (float i / 255.0)
            lut[i] <- RgbaColor (float32 c.Red, float32 c.Green, float32 c.Blue, float32 c.Alpha)

        lut

    let private sampleNormalizedLut (lut: RgbaColor array) (t: float) : RgbaColor =
        let scaled = t * 255.0

        if scaled <= 0.0 then
            lut[0]
        elif scaled >= 255.0 then
            lut[255]
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
            Transform = classifyTransform scale
            Clip = clip
            Lut = samplePaletteLut eval
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
            | ValueNone -> ValueSome ramp.Lut[255]
        else
            ValueSome (sampleNormalizedLut ramp.Lut t)

    /// <summary>Maps a scalar <c>value</c> into an <c>RgbaColor</c> in <c>O(1)</c> with zero heap allocations on linear, logarithmic, and constant scales.</summary>
    let tryMap (ramp: ColorRamp) (value: float) : RgbaColor voption =
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

    /// <summary>Projects 2D <c>ContourPolygon</c> rings and holes onto the active <c>plane</c> back wall as paired <c>LineList</c> vertices.</summary>
    let projectContours
        (scale: Scale3D)
        (activeBackWalls: struct (float32 * float32 * float32))
        (plane: WallPlane)
        (color: RgbaColor)
        (contours: ContourPolygon list)
        : ReadOnlyMemory<LineVertex3D> =
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
                    vertices.Add (LineVertex3D (a, color))
                    vertices.Add (LineVertex3D (b, color))
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

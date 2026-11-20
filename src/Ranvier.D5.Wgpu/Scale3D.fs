namespace Ranvier.D5.Wgpu

open System
open System.Numerics
#if FABLE_COMPILER
open Fable.Core
#else
open System.Runtime.CompilerServices
#endif
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Vertical data-axis orientation mapping data <c>(X, Y, Z)</c> into right-handed <c>Ranvier.Wgpu</c> Y-up world space.</summary>
[<RequireQualifiedAccess; Struct>]
type UpAxis =
    /// <summary>Scientific Z-up convention: data <c>(X, Y, Z)</c> maps to world <c>(wx, wz, -wy)</c>.</summary>
    | ZUp
    /// <summary>Graphics Y-up convention: data <c>(X, Y, Z)</c> maps directly to world <c>(wx, wy, wz)</c>.</summary>
    | YUp

/// <summary>Policy governing the relative world-space cuboid proportions of a 3D plot box.</summary>
[<RequireQualifiedAccess>]
type BoxAspect =
    /// <summary>Wide horizontal floor with shorter vertical axis: <c>(2.0, 2.0, 4.0 / 3.0)</c> under <c>ZUp</c> (Makie <c>Axis3</c> proportions).</summary>
    | ChartDefault
    /// <summary>Equal world-space extents <c>(2.0, 2.0, 2.0)</c> on all three axes.</summary>
    | Cube
    /// <summary>Proportional to each axis domain span, normalized so the largest span has world extent <c>2.0</c>.</summary>
    | Data
    /// <summary>Uses proportional <c>Data</c> extents unless any span is zero or the max-to-min span ratio exceeds <c>4.0</c>, in which case falls back to <c>ChartDefault</c>.</summary>
    | Auto
    /// <summary>Explicit positive world cuboid widths <c>(widthX, widthY, widthZ)</c> along data <c>X</c>, <c>Y</c>, and <c>Z</c>.</summary>
    | Manual of widthX: float * widthY: float * widthZ: float

/// <summary>Axis scale descriptor wrapping <c>Ranvier.D5.Scale</c> continuous, piecewise, UTC time, or categorical band mappings.</summary>
[<RequireQualifiedAccess>]
type AxisScale3D =
    /// <summary>Numeric continuous scale normalized to <c>[0.0, 1.0]</c>.</summary>
    | Continuous of Continuous
    /// <summary>Piecewise-linear knot scale normalized to <c>[0.0, 1.0]</c>.</summary>
    | Piecewise of Piecewise
    /// <summary>UTC instant scale normalized to <c>[0.0, 1.0]</c>; numeric inputs represent Unix epoch milliseconds.</summary>
    | TimeUtc of TimeScale
    /// <summary>Categorical band or point scale normalized to <c>[0.0, 1.0]</c>.</summary>
    | Categorical of Band<string>

/// <summary>Constructors, domain inspection, normalized mapping, inversion, and tick generation for <c>AxisScale3D</c>.</summary>
[<RequireQualifiedAccess>]
module AxisScale3D =
#if FABLE_COMPILER
    [<Emit("new WeakMap()")>]
    let private createWeakMap () : obj = jsNative

    [<Emit("$0.set($1, $2)")>]
    let private weakMapSet (_map: obj) (_key: Continuous) (_value: float) : unit = jsNative

    [<Emit("$0.get($1)")>]
    let private weakMapGet (_map: obj) (_key: Continuous) : obj = jsNative

    let private logBases = createWeakMap ()

    let private registerLogBasis (scale: Continuous) (basis: float) =
        weakMapSet logBases scale basis

    let private tryGetLogBasis (scale: Continuous) : float voption =
        let v = weakMapGet logBases scale
        if isNull v then ValueNone else ValueSome (unbox<float> v)
#else
    let private logBases = ConditionalWeakTable<Continuous, ref<float>>()

    let private registerLogBasis (scale: Continuous) (basis: float) =
        logBases.AddOrUpdate (scale, ref basis)

    let private tryGetLogBasis (scale: Continuous) : float voption =
        match logBases.TryGetValue scale with
        | true, basisRef -> ValueSome basisRef.Value
        | false, _ -> ValueNone
#endif

    let private isLogContinuous (scale: Continuous) =
        let a = Scale.invert scale 0.0
        let b = Scale.invert scale 1.0

        if ((a > 0.0 && b > 0.0) || (a < 0.0 && b < 0.0)) && a <> b then
            let mid = Scale.invert scale 0.5
            let sign = if a < 0.0 then -1.0 else 1.0
            let logA = Math.Log (abs a)
            let logB = Math.Log (abs b)
            let geom = sign * Math.Exp (0.5 * (logA + logB))
            let arith = 0.5 * a + 0.5 * b
            abs (mid - geom) <= 1e-9 * abs geom
            && abs (mid - arith) > 1e-6 * abs arith
        else
            false

    let internal isLogScale (axisScale: AxisScale3D) : bool =
        match axisScale with
        | AxisScale3D.Continuous c -> isLogContinuous c
        | _ -> false

    /// <summary>Creates a linear numeric axis scale normalized to <c>[0.0, 1.0]</c>; throws <c>ArgumentException</c> for non-finite domains.</summary>
    let linear (domain: float * float) : AxisScale3D =
        AxisScale3D.Continuous (Scale.linear domain (0.0, 1.0))

    /// <summary>Creates a logarithmic numeric axis scale normalized to <c>[0.0, 1.0]</c>; domain endpoints must be nonzero and share one sign.</summary>
    let log (basis: float) (domain: float * float) : AxisScale3D =
        let scale = Scale.log basis domain (0.0, 1.0)
        registerLogBasis scale basis
        AxisScale3D.Continuous scale

    /// <summary>Creates a symmetric logarithmic axis scale with positive linear-region constant normalized to <c>[0.0, 1.0]</c>.</summary>
    let symlog (constant: float) (domain: float * float) : AxisScale3D =
        AxisScale3D.Continuous (Scale.symlog constant domain (0.0, 1.0))

    /// <summary>Creates a sign-preserving power axis scale normalized to <c>[0.0, 1.0]</c>; requires a finite positive exponent.</summary>
    let pow (exponent: float) (domain: float * float) : AxisScale3D =
        AxisScale3D.Continuous (Scale.power exponent domain (0.0, 1.0))

    /// <summary>Creates a sign-preserving square-root axis scale normalized to <c>[0.0, 1.0]</c>.</summary>
    let sqrt (domain: float * float) : AxisScale3D =
        AxisScale3D.Continuous (Scale.sqrt domain (0.0, 1.0))

    /// <summary>Creates a piecewise-linear axis scale from strictly monotone domain knots to normalized outputs (typically <c>[0.0 .. 1.0]</c>).</summary>
    let piecewise (domain: float list) (normalizedOutputs: float list) : AxisScale3D =
        AxisScale3D.Piecewise (Scale.piecewise domain normalizedOutputs)

    /// <summary>Creates a UTC time axis scale normalized to <c>[0.0, 1.0]</c>; numeric coordinates use Unix epoch milliseconds.</summary>
    let timeUtc (domain: DateTimeOffset * DateTimeOffset) : AxisScale3D =
        AxisScale3D.TimeUtc (Scale.utcTime domain (0.0, 1.0))

    /// <summary>Creates an evenly spaced categorical band axis scale normalized to <c>[0.0, 1.0]</c>.</summary>
    let band (categories: string list) (innerPadding: float) (outerPadding: float) : AxisScale3D =
        AxisScale3D.Categorical (Scale.band categories (0.0, 1.0) innerPadding outerPadding)

    /// <summary>Creates a zero-width centered categorical point axis scale normalized to <c>[0.0, 1.0]</c>.</summary>
    let point (categories: string list) (outerPadding: float) : AxisScale3D =
        AxisScale3D.Categorical (Scale.point categories (0.0, 1.0) outerPadding)

    /// <summary>Returns the nonnegative domain span in data units (<c>0.0</c> for constant domains; <c>1.0</c> for categorical bands).</summary>
    let domainSpan (axisScale: AxisScale3D) : float =
        match axisScale with
        | AxisScale3D.Continuous c ->
            let a = Scale.invert c 0.0
            let b = Scale.invert c 1.0
            abs (b - a)
        | AxisScale3D.Piecewise p ->
            let a = Scale.piecewiseInvert p 0.0
            let b = Scale.piecewiseInvert p 1.0
            abs (b - a)
        | AxisScale3D.TimeUtc ts ->
            let t0 = Scale.timeInvert ts 0.0
            let t1 = Scale.timeInvert ts 1.0
            abs (float (t1.ToUnixTimeMilliseconds ()) - float (t0.ToUnixTimeMilliseconds ()))
        | AxisScale3D.Categorical _ -> 1.0

    /// <summary>Maps a data value into normalized <c>[0.0, 1.0]</c> axis coordinates, or <c>ValueNone</c> when non-finite or outside the transform domain.</summary>
    let tryMapNormalized (axisScale: AxisScale3D) (value: float) : float voption =
        if not (Check3D.isFinite value) then
            ValueNone
        else
            match axisScale with
            | AxisScale3D.Continuous c ->
                match Scale.tryMap c value with
                | Some t when Check3D.isFinite t -> ValueSome t
                | _ -> ValueNone
            | AxisScale3D.Piecewise p ->
                try
                    let t = Scale.piecewiseMap p value
                    if Check3D.isFinite t then ValueSome t else ValueNone
                with :? ArgumentException ->
                    ValueNone
            | AxisScale3D.TimeUtc ts ->
                let t0 = Scale.timeInvert ts 0.0
                let t1 = Scale.timeInvert ts 1.0
                let ms0 = float (t0.ToUnixTimeMilliseconds ())
                let ms1 = float (t1.ToUnixTimeMilliseconds ())

                if ms0 = ms1 then
                    ValueSome 0.5
                else
                    let t = (value - ms0) / (ms1 - ms0)
                    if Check3D.isFinite t then ValueSome t else ValueNone
            | AxisScale3D.Categorical _ -> ValueSome value

    /// <summary>Inverts a finite normalized <c>[0.0, 1.0]</c> axis coordinate back to data units.</summary>
    let invertNormalized (axisScale: AxisScale3D) (t: float) : float =
        Check3D.finite "t" t

        match axisScale with
        | AxisScale3D.Continuous c -> Scale.invert c t
        | AxisScale3D.Piecewise p -> Scale.piecewiseInvert p t
        | AxisScale3D.TimeUtc ts ->
            let t0 = Scale.timeInvert ts 0.0
            let t1 = Scale.timeInvert ts 1.0
            let ms0 = float (t0.ToUnixTimeMilliseconds ())
            let ms1 = float (t1.ToUnixTimeMilliseconds ())
            if ms0 = ms1 then ms0 else ms0 + t * (ms1 - ms0)
        | AxisScale3D.Categorical _ -> t

    let internal categoricalTicks (count: int) (band: Band<string>) : (string * float) list =
        if count <= 0 then
            []
        elif count > 10000 then
            invalidArg "count" "Tick count cannot exceed 10,000."
        else
            let cats = Scale.bandCategories band |> List.toArray

            if cats.Length = 0 then
                []
            else
                let stride = max 1 (int (Math.Ceiling (float cats.Length / float count)))
                let halfWidth = Scale.bandwidth band * 0.5

                [ 0 .. stride .. cats.Length - 1 ]
                |> List.choose (fun i ->
                    let k = cats[i]

                    match Scale.bandPosition band k with
                    | Some pos -> Some (k, Math.Clamp (pos + halfWidth, 0.0, 1.0))
                    | None -> None)

    /// <summary>Generates approximately <c>count</c> nice domain-unit tick values along the axis.</summary>
    let ticks (count: int) (axisScale: AxisScale3D) : float list =
        if count <= 0 then
            []
        elif count > 10000 then
            invalidArg "count" "Tick count cannot exceed 10,000."
        else
            match axisScale with
            | AxisScale3D.Continuous c ->
                if isLogContinuous c then
                    let a = Scale.invert c 0.0
                    let b = Scale.invert c 1.0
                    let rawBasis = tryGetLogBasis c |> ValueOption.defaultValue 10.0

                    let tickBasis =
                        if rawBasis > 0.0 && rawBasis < 1.0 then
                            let inv = 1.0 / rawBasis
                            if Check3D.isFinite inv && inv > 1.0 then inv else 10.0
                        elif Check3D.isFinite rawBasis && rawBasis > 1.0 then
                            rawBasis
                        else
                            10.0

                    let decades = abs (Math.Log (abs b) - Math.Log (abs a)) / abs (Math.Log tickBasis)

                    let minorMult =
                        if tickBasis = floor tickBasis && tickBasis <= 100.0 then
                            max 1.0 (tickBasis - 1.0)
                        else
                            1.0

                    if decades < float count && decades * minorMult > 10000.0 then
                        Scale.ticks count c
                    else
                        Scale.logTicks tickBasis count c
                else
                    Scale.ticks count c
            | AxisScale3D.Piecewise p ->
                let d0 = Scale.piecewiseInvert p 0.0
                let d1 = Scale.piecewiseInvert p 1.0
                Scale.ticks count (Scale.linear (d0, d1) (0.0, 1.0))
            | AxisScale3D.TimeUtc ts ->
                let t0 = Scale.timeInvert ts 0.0
                let t1 = Scale.timeInvert ts 1.0

                if t0 = t1 then
                    [ float (t0.ToUnixTimeMilliseconds ()) ]
                else
                    let interval = TimeTicks.choose count (t0, t1)
                    let ticks = TimeTicks.utc interval (t0, t1) |> List.map (fun dt -> float (dt.ToUnixTimeMilliseconds ()))

                    if List.isEmpty ticks then
                        let ms0 = float (t0.ToUnixTimeMilliseconds ())
                        let ms1 = float (t1.ToUnixTimeMilliseconds ())
                        Scale.ticks count (Scale.linear (ms0, ms1) (0.0, 1.0))
                    else
                        ticks
            | AxisScale3D.Categorical band -> categoricalTicks count band |> List.map snd

[<Struct>]
type internal RampTransform =
    | Linear of x0: float * span: float * clamped: bool
    | Log of sign: float * logAbs0: float * logSpan: float * clamped: bool
    | Constant of value: float
    | General of scale: Continuous

module internal RampTransform =
    let private approxEq (a: float) (b: float) =
        abs (a - b) <= 1e-9 * max 1.0 (max (abs a) (abs b))

    let private matchesAt (scale: Continuous) (x: float) (expected: float) =
        match Scale.tryMap scale x with
        | Some actual -> approxEq actual expected
        | None -> false

    let classify (scale: Continuous) : RampTransform =
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
                elif (x0 > 0.0 && x1 > 0.0) || (x0 < 0.0 && x1 < 0.0) then
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

#if FABLE_COMPILER
    let inline eval (transform: RampTransform) (value: float) : float =
#else
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let eval (transform: RampTransform) (value: float) : float =
#endif
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

[<Struct>]
type internal AxisCoordMapper =
    | ContinuousFast of transform: RampTransform
    | TimeLinear of ms0: float * spanMs: float
    | TimeConst of mid: float
    | PiecewiseAxis of piecewise: Piecewise
    | CategoricalAxis

module internal AxisCoordMapper =
    let classify (axis: AxisScale3D) : AxisCoordMapper =
        match axis with
        | AxisScale3D.Continuous c -> ContinuousFast (RampTransform.classify c)
        | AxisScale3D.TimeUtc ts ->
            let ms0 = float ((Scale.timeInvert ts 0.0).ToUnixTimeMilliseconds ())
            let ms1 = float ((Scale.timeInvert ts 1.0).ToUnixTimeMilliseconds ())

            if ms0 = ms1 then
                TimeConst 0.5
            else
                TimeLinear (ms0, ms1 - ms0)
        | AxisScale3D.Piecewise p -> PiecewiseAxis p
        | AxisScale3D.Categorical _ -> CategoricalAxis

#if FABLE_COMPILER
    let inline evalNormalized (mapper: AxisCoordMapper) (value: float) : float =
#else
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let evalNormalized (mapper: AxisCoordMapper) (value: float) : float =
#endif
        if not (Check3D.isFinite value) then
            Double.NaN
        else
            match mapper with
            | ContinuousFast rt -> RampTransform.eval rt value
            | TimeLinear (ms0, spanMs) ->
                let t = (value - ms0) / spanMs
                if Check3D.isFinite t then t else Double.NaN
            | TimeConst mid -> mid
            | PiecewiseAxis p ->
                try
                    let t = Scale.piecewiseMap p value
                    if Check3D.isFinite t then t else Double.NaN
                with :? ArgumentException ->
                    Double.NaN
            | CategoricalAxis -> value

    let axisDir (axis: AxisScale3D) : float =
        match axis with
        | AxisScale3D.Categorical _ -> 1.0
        | _ ->
            let d = AxisScale3D.invertNormalized axis 1.0 - AxisScale3D.invertNormalized axis 0.0
            if d < 0.0 then -1.0 else 1.0

    let mapArray (mapper: AxisCoordMapper) (coords: float array) (dst: float array) =
        for i = 0 to coords.Length - 1 do
            dst[i] <- evalNormalized mapper coords[i]

    let mapWorldCoords (mapper: AxisCoordMapper) (extent: float) (coords: float array) : float array * bool array =
        let n = coords.Length
        let world = Array.zeroCreate<float> n
        let valid = Array.zeroCreate<bool> n

        for i = 0 to n - 1 do
            let t = evalNormalized mapper coords[i]

            if Check3D.isFinite t then
                let w = (t - 0.5) * extent

                if Check3D.isFinite w && Check3D.isFinite32 (float32 w) then
                    world[i] <- w
                    valid[i] <- true

        world, valid

    let inline mapInterpolatedWorld (mapper: AxisCoordMapper) (extent: float) (c0: float) (c1: float) (w0: float) (w1: float) (mu: float) : float =
        let c = c0 + (c1 - c0) * mu
        let t = evalNormalized mapper c

        if Check3D.isFinite t then
            let w = (t - 0.5) * extent
            if Check3D.isFinite w && Check3D.isFinite32 (float32 w) then w else w0 + (w1 - w0) * mu
        else
            w0 + (w1 - w0) * mu

/// <summary>Immutable 3D data-to-world coordinate mapping combining three <c>AxisScale3D</c>s, <c>BoxAspect</c>, and <c>UpAxis</c>.</summary>
type Scale3D =
    private
        {
            Aspect: BoxAspect
            Up: UpAxis
            AxisX: AxisScale3D
            AxisY: AxisScale3D
            AxisZ: AxisScale3D
            MapX: AxisCoordMapper
            MapY: AxisCoordMapper
            MapZ: AxisCoordMapper
            DataExtentX: float
            DataExtentY: float
            DataExtentZ: float
            WorldExtents: Vector3
            WorldBounds: Bounds3
            FitRadius: float32
        }

    member internal this.UpAxis = this.Up
    member internal this.XScale = this.AxisX
    member internal this.YScale = this.AxisY
    member internal this.ZScale = this.AxisZ
    member internal this.MapperX = this.MapX
    member internal this.MapperY = this.MapY
    member internal this.MapperZ = this.MapZ
    member internal this.DataExtents = struct (this.DataExtentX, this.DataExtentY, this.DataExtentZ)

/// <summary>Typed data coordinate over numeric, categorical band, or UTC instant 3D axes.</summary>
[<RequireQualifiedAccess>]
type AxisCoord3D =
    /// <summary>Numeric observation mapped through a continuous or piecewise axis scale.</summary>
    | Num of float
    /// <summary>Categorical label resolved to the center of its band or point slot.</summary>
    | Cat of string
    /// <summary>UTC timestamp mapped along a <c>AxisScale3D.TimeUtc</c> axis.</summary>
    | Time of DateTimeOffset

/// <summary>Constructors, coordinate transforms, and bounding helpers for <c>Scale3D</c>.</summary>
[<RequireQualifiedAccess>]
module Scale3D =
    let private chartDefaultDataExtents (upAxis: UpAxis) =
        match upAxis with
        | UpAxis.ZUp -> struct (2.0, 2.0, 4.0 / 3.0)
        | UpAxis.YUp -> struct (2.0, 4.0 / 3.0, 2.0)

    let private resolveDataExtents (aspect: BoxAspect) (upAxis: UpAxis) (x: AxisScale3D) (y: AxisScale3D) (z: AxisScale3D) =
        match aspect with
        | BoxAspect.ChartDefault -> chartDefaultDataExtents upAxis
        | BoxAspect.Cube -> struct (2.0, 2.0, 2.0)
        | BoxAspect.Manual (wx, wy, wz) ->
            Check3D.positive "widthX" wx
            Check3D.positive "widthY" wy
            Check3D.positive "widthZ" wz
            struct (wx, wy, wz)
        | BoxAspect.Data ->
            let sx = AxisScale3D.domainSpan x
            let sy = AxisScale3D.domainSpan y
            let sz = AxisScale3D.domainSpan z
            let maxS = max sx (max sy sz)

            if not (Check3D.isFinite maxS) || maxS <= 0.0 then
                chartDefaultDataExtents upAxis
            else
                struct (2.0 * sx / maxS, 2.0 * sy / maxS, 2.0 * sz / maxS)
        | BoxAspect.Auto ->
            let sx = AxisScale3D.domainSpan x
            let sy = AxisScale3D.domainSpan y
            let sz = AxisScale3D.domainSpan z
            let minS = min sx (min sy sz)
            let maxS = max sx (max sy sz)

            if not (Check3D.isFinite maxS) || minS <= 0.0 || maxS / minS > 4.0 then
                chartDefaultDataExtents upAxis
            else
                struct (2.0 * sx / maxS, 2.0 * sy / maxS, 2.0 * sz / maxS)

    let private normalizedToWorld (upAxis: UpAxis) (dx: float) (dy: float) (dz: float) (tx: float) (ty: float) (tz: float) : Vector3 voption =
        let wx = float32 ((tx - 0.5) * dx)
        let wy = float32 ((ty - 0.5) * dy)
        let wz = float32 ((tz - 0.5) * dz)

        if Check3D.isFinite32 wx && Check3D.isFinite32 wy && Check3D.isFinite32 wz then
            match upAxis with
            | UpAxis.ZUp -> ValueSome (Vector3 (wx, wz, -wy))
            | UpAxis.YUp -> ValueSome (Vector3 (wx, wy, wz))
        else
            ValueNone

    /// <summary>Creates a <c>Scale3D</c> combining box aspect policy, vertical up-axis convention, and three axis scales.</summary>
    let create (aspect: BoxAspect) (upAxis: UpAxis) (x: AxisScale3D) (y: AxisScale3D) (z: AxisScale3D) : Scale3D =
        let struct (dx, dy, dz) = resolveDataExtents aspect upAxis x y z

        let worldExt =
            match upAxis with
            | UpAxis.ZUp -> Vector3 (float32 dx, float32 dz, float32 dy)
            | UpAxis.YUp -> Vector3 (float32 dx, float32 dy, float32 dz)

        let bounds =
            Bounds3.create
                (-0.5 * float worldExt.X)
                (-0.5 * float worldExt.Y)
                (-0.5 * float worldExt.Z)
                (float worldExt.X)
                (float worldExt.Y)
                (float worldExt.Z)

        let fitR = float32 (Bounds3.boundingRadius bounds)

        {
            Aspect = aspect
            Up = upAxis
            AxisX = x
            AxisY = y
            AxisZ = z
            MapX = AxisCoordMapper.classify x
            MapY = AxisCoordMapper.classify y
            MapZ = AxisCoordMapper.classify z
            DataExtentX = dx
            DataExtentY = dy
            DataExtentZ = dz
            WorldExtents = worldExt
            WorldBounds = bounds
            FitRadius = fitR
        }

    /// <summary>Creates a standard Z-up linear Cartesian <c>Scale3D</c> with Makie <c>ChartDefault</c> cuboid proportions <c>(2.0, 2.0, 4.0 / 3.0)</c>.</summary>
    let cartesian (xDomain: float * float) (yDomain: float * float) (zDomain: float * float) : Scale3D =
        create BoxAspect.ChartDefault UpAxis.ZUp (AxisScale3D.linear xDomain) (AxisScale3D.linear yDomain) (AxisScale3D.linear zDomain)

    /// <summary>Creates a Y-up linear Cartesian <c>Scale3D</c> with <c>ChartDefault</c> cuboid proportions <c>(2.0, 4.0 / 3.0, 2.0)</c>.</summary>
    let cartesianYUp (xDomain: float * float) (yDomain: float * float) (zDomain: float * float) : Scale3D =
        create BoxAspect.ChartDefault UpAxis.YUp (AxisScale3D.linear xDomain) (AxisScale3D.linear yDomain) (AxisScale3D.linear zDomain)

    /// <summary>Creates a Y-up linear <c>Scale3D</c> with equal <c>[-1, +1]^3</c> unit-cube world extents.</summary>
    let cubeYUp (xDomain: float * float) (yDomain: float * float) (zDomain: float * float) : Scale3D =
        create BoxAspect.Cube UpAxis.YUp (AxisScale3D.linear xDomain) (AxisScale3D.linear yDomain) (AxisScale3D.linear zDomain)

    /// <summary>Creates a linear <c>Scale3D</c> from the <c>X/Y/Z</c> extents of <c>bounds</c>.</summary>
    let ofBounds3 (aspect: BoxAspect) (upAxis: UpAxis) (bounds: Bounds3) : Scale3D =
        create
            aspect
            upAxis
            (AxisScale3D.linear (Bounds3.minX bounds, Bounds3.maxX bounds))
            (AxisScale3D.linear (Bounds3.minY bounds, Bounds3.maxY bounds))
            (AxisScale3D.linear (Bounds3.minZ bounds, Bounds3.maxZ bounds))

    /// <summary>Returns the cuboid world extents <c>(spanWorldX, spanWorldY, spanWorldZ)</c> in <c>Ranvier.Wgpu</c> world coordinates.</summary>
    let worldExtents (scale: Scale3D) : Vector3 =
        scale.WorldExtents

    /// <summary>Returns the centered world-space bounding box <c>Bounds3</c> in <c>Ranvier.Wgpu</c> coordinates.</summary>
    let worldBounds (scale: Scale3D) : Bounds3 =
        scale.WorldBounds

    /// <summary>Returns the half-diagonal bounding-sphere radius of the centered world cuboid.</summary>
    let fitRadius (scale: Scale3D) : float32 =
        scale.FitRadius

    /// <summary>Maps a data <c>Point3</c> into centered <c>Ranvier.Wgpu</c> world coordinates, returning <c>ValueNone</c> when any axis omits the observation.</summary>
    let tryMap (scale: Scale3D) (point: Point3) : Vector3 voption =
        let tx = AxisCoordMapper.evalNormalized scale.MapX point.X
        let ty = AxisCoordMapper.evalNormalized scale.MapY point.Y
        let tz = AxisCoordMapper.evalNormalized scale.MapZ point.Z

        if Check3D.isFinite tx && Check3D.isFinite ty && Check3D.isFinite tz then
            normalizedToWorld scale.Up scale.DataExtentX scale.DataExtentY scale.DataExtentZ tx ty tz
        else
            ValueNone

    /// <summary>Maps a data <c>Point3</c> into centered <c>Ranvier.Wgpu</c> world coordinates; raises <c>ArgumentException</c> when any coordinate cannot be mapped.</summary>
    let map (scale: Scale3D) (point: Point3) : Vector3 =
        match tryMap scale point with
        | ValueSome v -> v
        | ValueNone -> invalidArg "point" "3D observation cannot be mapped to a finite world position."

    /// <summary>Maps optional categorical X/Y band keys (centering within each band) and a numeric Z value into centered <c>Ranvier.Wgpu</c> world coordinates.</summary>
    let tryMapCategory (scale: Scale3D) (xKey: string option) (yKey: string option) (zVal: float) : Vector3 voption =
        let resolveAxis (axisScale: AxisScale3D) (keyOpt: string option) : float voption =
            match axisScale, keyOpt with
            | AxisScale3D.Categorical band, Some key ->
                if isNull key then
                    ValueNone
                else
                    match Scale.bandPosition band key with
                    | Some pos -> ValueSome (pos + Scale.bandwidth band * 0.5)
                    | None -> ValueNone
            | AxisScale3D.Categorical _, None -> ValueNone
            | _, Some _ -> ValueNone
            | _, None -> ValueSome 0.5

        match resolveAxis scale.AxisX xKey with
        | ValueNone -> ValueNone
        | ValueSome tx ->
            match resolveAxis scale.AxisY yKey with
            | ValueNone -> ValueNone
            | ValueSome ty ->
                let tz = AxisCoordMapper.evalNormalized scale.MapZ zVal

                if Check3D.isFinite tz then
                    normalizedToWorld scale.Up scale.DataExtentX scale.DataExtentY scale.DataExtentZ tx ty tz
                else
                    ValueNone

    /// <summary>Inverts a finite <c>Ranvier.Wgpu</c> world position back into data coordinates; under <c>ZUp</c>, world <c>(wx, wy, wz)</c> maps back via <c>(wx, -wz, wy)</c>.</summary>
    let invert (scale: Scale3D) (worldPos: Vector3) : Point3 =
        Check3D.vector3 "worldPos" worldPos

        let invNorm extent worldCoord =
            if extent = 0.0 then 0.5 else float worldCoord / extent + 0.5

        let struct (tx, ty, tz) =
            match scale.Up with
            | UpAxis.ZUp ->
                struct (
                    invNorm scale.DataExtentX worldPos.X,
                    invNorm scale.DataExtentY (-worldPos.Z),
                    invNorm scale.DataExtentZ worldPos.Y
                )
            | UpAxis.YUp ->
                struct (
                    invNorm scale.DataExtentX worldPos.X,
                    invNorm scale.DataExtentY worldPos.Y,
                    invNorm scale.DataExtentZ worldPos.Z
                )

        Point3.create
            (AxisScale3D.invertNormalized scale.AxisX tx)
            (AxisScale3D.invertNormalized scale.AxisY ty)
            (AxisScale3D.invertNormalized scale.AxisZ tz)

    /// <summary>Maps a typed <c>AxisCoord3D</c> (<c>Num</c>, <c>Cat</c>, or <c>Time</c>) into normalized <c>[0.0, 1.0]</c> coordinates along <c>axisScale</c>, resolving category centers automatically.</summary>
    let tryMapCoord (axisScale: AxisScale3D) (coord: AxisCoord3D) : float voption =
        match coord with
        | AxisCoord3D.Num value ->
            match axisScale with
            | AxisScale3D.Categorical _ -> ValueNone
            | _ -> AxisScale3D.tryMapNormalized axisScale value
        | AxisCoord3D.Cat key ->
            if isNull key then
                ValueNone
            else
                match axisScale with
                | AxisScale3D.Categorical band ->
                    match Scale.bandPosition band key with
                    | Some pos -> ValueSome (Math.Clamp (pos + Scale.bandwidth band * 0.5, 0.0, 1.0))
                    | None -> ValueNone
                | _ -> ValueNone
        | AxisCoord3D.Time instant ->
            match axisScale with
            | AxisScale3D.TimeUtc _ -> AxisScale3D.tryMapNormalized axisScale (float (instant.ToUnixTimeMilliseconds ()))
            | _ -> ValueNone

    /// <summary>Maps mixed numeric, categorical, or UTC time coordinates along <c>X</c>, <c>Y</c>, and <c>Z</c> into centered <c>Ranvier.Wgpu</c> world space.</summary>
    let tryMapCoordPoint (scale: Scale3D) (x: AxisCoord3D) (y: AxisCoord3D) (z: AxisCoord3D) : Vector3 voption =
        if isNull (box scale) then
            nullArg (nameof scale)

        match tryMapCoord scale.XScale x with
        | ValueNone -> ValueNone
        | ValueSome tx ->
            match tryMapCoord scale.YScale y with
            | ValueNone -> ValueNone
            | ValueSome ty ->
                match tryMapCoord scale.ZScale z with
                | ValueNone -> ValueNone
                | ValueSome tz -> normalizedToWorld scale.Up scale.DataExtentX scale.DataExtentY scale.DataExtentZ tx ty tz

    /// <summary>Maps a data <c>Point3</c> through <c>scale</c> and <c>camera</c> into a 2D viewport screen <c>Point</c>, or <c>ValueNone</c> when unmappable or clipped.</summary>
    let projectToScreen (viewportWidth: int) (viewportHeight: int) (camera: Camera3D) (scale: Scale3D) (point: Point3) : Point voption =
        if isNull (box scale) then
            nullArg (nameof scale)

        if viewportWidth <= 0 then
            raise (ArgumentOutOfRangeException (nameof viewportWidth, "Viewport width must be positive."))

        if viewportHeight <= 0 then
            raise (ArgumentOutOfRangeException (nameof viewportHeight, "Viewport height must be positive."))

        match tryMap scale point with
        | ValueNone -> ValueNone
        | ValueSome worldPos ->
            match Camera3D.projectToScreen viewportWidth viewportHeight camera worldPos with
            | ValueSome screenPt when Check3D.isFinite32 screenPt.X && Check3D.isFinite32 screenPt.Y ->
                ValueSome (Point.create (float screenPt.X) (float screenPt.Y))
            | _ -> ValueNone

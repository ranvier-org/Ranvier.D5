namespace Ranvier.D5.Wgpu

open System
open System.Numerics
#if FABLE_COMPILER
open Fable.Core
#else
open System.Runtime.CompilerServices
#endif
open Ranvier.D5

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

/// <summary>Immutable 3D data-to-world coordinate mapping combining three <c>AxisScale3D</c>s, <c>BoxAspect</c>, and <c>UpAxis</c>.</summary>
type Scale3D =
    private
        {
            Aspect: BoxAspect
            Up: UpAxis
            AxisX: AxisScale3D
            AxisY: AxisScale3D
            AxisZ: AxisScale3D
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
    member internal this.DataExtents = struct (this.DataExtentX, this.DataExtentY, this.DataExtentZ)

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
        match AxisScale3D.tryMapNormalized scale.AxisX point.X with
        | ValueNone -> ValueNone
        | ValueSome tx ->
            match AxisScale3D.tryMapNormalized scale.AxisY point.Y with
            | ValueNone -> ValueNone
            | ValueSome ty ->
                match AxisScale3D.tryMapNormalized scale.AxisZ point.Z with
                | ValueNone -> ValueNone
                | ValueSome tz -> normalizedToWorld scale.Up scale.DataExtentX scale.DataExtentY scale.DataExtentZ tx ty tz

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
                match AxisScale3D.tryMapNormalized scale.AxisZ zVal with
                | ValueNone -> ValueNone
                | ValueSome tz -> normalizedToWorld scale.Up scale.DataExtentX scale.DataExtentY scale.DataExtentZ tx ty tz

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

namespace Ranvier.D5.Wgpu

open System
open System.Numerics
open System.Threading
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>
/// Immutable 3D uniform scalar volume grid over a <c>Bounds3</c> domain, storing primary scalars
/// (and optional secondary coloring scalars) in X-fastest linear order <c>ix + nx * (iy + ny * iz)</c>.
/// </summary>
type VolumeGrid =
    private
        {
            Nx: int
            Ny: int
            Nz: int
            GridBounds: Bounds3
            Values: float32 array
            SecondaryScalars: float32 array
        }

/// <summary>Configuration for <c>IsoSurface3D</c> marching-cubes superlevel-set extraction.</summary>
type IsoSurfaceConfig =
    {
        /// <summary>Scalar threshold defining the superlevel-set interior (<c>value &gt;= Threshold</c>).</summary>
        Threshold: float32
        /// <summary>When true, seals superlevel regions meeting any of the six <c>Bounds3</c> boundary faces.</summary>
        CapBoundaries: bool
        /// <summary>Maximum number of output triangles allowed before returning <c>BudgetExceeded</c>.</summary>
        MaxTriangles: int
        /// <summary>Maximum number of output vertices allowed before returning <c>BudgetExceeded</c>.</summary>
        MaxVertices: int
    }

/// <summary>Default constructors for <c>IsoSurfaceConfig</c>.</summary>
[<RequireQualifiedAccess>]
module IsoSurfaceConfig =
    /// <summary>
    /// Creates an <c>IsoSurfaceConfig</c> at <c>threshold</c> with <c>CapBoundaries = true</c>,
    /// <c>MaxTriangles = 1_000_000</c>, and <c>MaxVertices = 1_000_000</c>.
    /// </summary>
    let create (threshold: float32) : IsoSurfaceConfig =
        {
            Threshold = threshold
            CapBoundaries = true
            MaxTriangles = 1_000_000
            MaxVertices = 1_000_000
        }

/// <summary>Optional voxel indices for extracting orthogonal X, Y, and Z cross-section planes.</summary>
type SliceIndices3D =
    {
        /// <summary>Optional X voxel index in <c>[0 .. nx - 1]</c> for a constant-X slice plane.</summary>
        XIndex: int option
        /// <summary>Optional Y voxel index in <c>[0 .. ny - 1]</c> for a constant-Y slice plane.</summary>
        YIndex: int option
        /// <summary>Optional Z voxel index in <c>[0 .. nz - 1]</c> for a constant-Z slice plane.</summary>
        ZIndex: int option
    }

[<Struct>]
type private VolumeAxisMapper =
    | ContinuousFast of transform: RampTransform
    | TimeLinear of ms0: float * spanMs: float
    | TimeConst of mid: float
    | PiecewiseAxis of piecewise: Piecewise
    | CategoricalAxis

[<Struct>]
type private MappedVolumeAxes =
    {
        Xs: float array
        Ys: float array
        Zs: float array
        Dx: float
        Dy: float
        Dz: float
        UpAxis: UpAxis
        XMapper: VolumeAxisMapper
        YMapper: VolumeAxisMapper
        ZMapper: VolumeAxisMapper
        WxArr: float array
        WxValid: bool array
        WyArr: float array
        WyValid: bool array
        WzArr: float array
        WzValid: bool array
    }

[<Struct>]
type private BoundaryFaceDesc =
    {
        AxisType: int // 0 = X-const, 1 = Y-const, 2 = Z-const
        FixedIdx: int
        Nu: int
        Nv: int
        OutwardNormal: Vector3
        BaseMask: int
        RealSlot0: int
        RealSlot1: int
        RealSlot2: int
        RealSlot3: int
        EdgeSlotKind: int array // -1 = unused, 0..3 = corner 0..3, 4 = u0, 5 = u1, 6 = v0, 7 = v1
    }

module private VolumeKernel =
    [<Literal>]
    let DefaultMaxVoxels = 4_194_304

    let inline checkCancellation (token: CancellationToken) =
#if FABLE_COMPILER
        if token.IsCancellationRequested then
            raise (OperationCanceledException ())
#else
        token.ThrowIfCancellationRequested ()
#endif

    let copySpan32 (span: ReadOnlySpan<float32>) : float32 array =
        if span.Length = 0 then
            [||]
        else
            let arr = Array.zeroCreate<float32> span.Length

            for i = 0 to span.Length - 1 do
                arr[i] <- span[i]

            arr

    let sampleLinearAxis (count: int) (minCoord: float) (sizeCoord: float) : float array =
        let arr = Array.zeroCreate<float> count
        let denom = float (count - 1)
        let maxCoord = minCoord + sizeCoord

        for i = 0 to count - 1 do
            arr[i] <- if i = count - 1 then maxCoord else minCoord + sizeCoord * (float i / denom)

        arr

    let validateGrid (maxVoxels: int) (nx: int) (ny: int) (nz: int) (bounds: Bounds3) : Result<int, LayoutError list> =
        if maxVoxels <= 0 then
            Error [ LayoutError.InvalidObservation "Volume grid voxel budget must be positive." ]
        elif nx < 2 || ny < 2 || nz < 2 then
            Error [ LayoutError.InvalidObservation "Volume grid requires at least 2 samples along each axis (nx, ny, nz >= 2)." ]
        elif Bounds3.sizeX bounds <= 0.0 || Bounds3.sizeY bounds <= 0.0 || Bounds3.sizeZ bounds <= 0.0 then
            Error [ LayoutError.InvalidObservation "Volume grid bounds must have positive extents along X, Y, and Z." ]
        else
            let xy64 = int64 nx * int64 ny
            let maxAllowed = int64 (min maxVoxels Int32.MaxValue)

            if xy64 > maxAllowed || xy64 > maxAllowed / int64 nz then
                Error
                    [
                        LayoutError.BudgetExceeded
                            $"Volume grid voxel count ({int64 nx}x{int64 ny}x{int64 nz}) exceeds budget of {maxAllowed}."
                    ]
            else
                Ok (int xy64 * nz)

    let classifyAxis (axis: AxisScale3D) : VolumeAxisMapper =
        match axis with
        | AxisScale3D.Continuous c -> VolumeAxisMapper.ContinuousFast (ColorRamp.classifyTransform c)
        | AxisScale3D.TimeUtc ts ->
            let ms0 = float ((Scale.timeInvert ts 0.0).ToUnixTimeMilliseconds ())
            let ms1 = float ((Scale.timeInvert ts 1.0).ToUnixTimeMilliseconds ())

            if ms0 = ms1 then
                VolumeAxisMapper.TimeConst 0.5
            else
                VolumeAxisMapper.TimeLinear (ms0, ms1 - ms0)
        | AxisScale3D.Piecewise p -> VolumeAxisMapper.PiecewiseAxis p
        | AxisScale3D.Categorical _ -> VolumeAxisMapper.CategoricalAxis

    let inline evalAxis (mapper: VolumeAxisMapper) (value: float) : float =
        if not (Check3D.isFinite value) then
            Double.NaN
        else
            match mapper with
            | VolumeAxisMapper.ContinuousFast rt -> RampTransform.eval rt value
            | VolumeAxisMapper.TimeLinear (ms0, spanMs) ->
                let t = (value - ms0) / spanMs
                if Check3D.isFinite t then t else Double.NaN
            | VolumeAxisMapper.TimeConst mid -> mid
            | VolumeAxisMapper.PiecewiseAxis p ->
                try
                    let t = Scale.piecewiseMap p value
                    if Check3D.isFinite t then t else Double.NaN
                with :? ArgumentException ->
                    Double.NaN
            | VolumeAxisMapper.CategoricalAxis -> value

    let axisDir (axis: AxisScale3D) : float =
        match axis with
        | AxisScale3D.Categorical _ -> 1.0
        | _ ->
            let d = AxisScale3D.invertNormalized axis 1.0 - AxisScale3D.invertNormalized axis 0.0
            if d < 0.0 then -1.0 else 1.0

    let mapAxisCoords (mapper: VolumeAxisMapper) (extent: float) (coords: float array) : float array * bool array =
        let n = coords.Length
        let world = Array.zeroCreate<float> n
        let valid = Array.zeroCreate<bool> n

        for i = 0 to n - 1 do
            let t = evalAxis mapper coords[i]

            if Check3D.isFinite t then
                let w = (t - 0.5) * extent

                if Check3D.isFinite w && Check3D.isFinite32 (float32 w) then
                    world[i] <- w
                    valid[i] <- true

        world, valid

    let inline mapInterpolatedAxis (mapper: VolumeAxisMapper) (extent: float) (c0: float) (c1: float) (w0: float) (w1: float) (mu: float) : float =
        let c = c0 + (c1 - c0) * mu
        let t = evalAxis mapper c

        if Check3D.isFinite t then
            let w = (t - 0.5) * extent
            if Check3D.isFinite w && Check3D.isFinite32 (float32 w) then w else w0 + (w1 - w0) * mu
        else
            w0 + (w1 - w0) * mu

    let inline worldPoint (upAxis: UpAxis) (wx: float) (wy: float) (wz: float) : Vector3 =
        match upAxis with
        | UpAxis.ZUp -> Vector3 (float32 wx, float32 wz, float32 (-wy))
        | UpAxis.YUp -> Vector3 (float32 wx, float32 wy, float32 wz)

    let inline safeNormalize (x: float) (y: float) (z: float) (fallback: Vector3) : Vector3 =
        let lenSq = x * x + y * y + z * z

        if lenSq > 1e-24 && Check3D.isFinite lenSq then
            let inv = 1.0 / sqrt lenSq
            Vector3 (float32 (x * inv), float32 (y * inv), float32 (z * inv))
        else
            fallback

    let inline safeNormalizeCross (u: Vector3) (v: Vector3) : Vector3 =
        let ux, uy, uz = float u.X, float u.Y, float u.Z
        let vx, vy, vz = float v.X, float v.Y, float v.Z
        safeNormalize (uy * vz - uz * vy) (uz * vx - ux * vz) (ux * vy - uy * vx) Vector3.UnitY

    let clearIntArray (arr: int array) =
        for i = 0 to arr.Length - 1 do
            arr[i] <- -1

    let prepareMappedAxes (scale: Scale3D) (volume: VolumeGrid) : MappedVolumeAxes =
        let gb = volume.GridBounds
        let xs = sampleLinearAxis volume.Nx (Bounds3.minX gb) (Bounds3.sizeX gb)
        let ys = sampleLinearAxis volume.Ny (Bounds3.minY gb) (Bounds3.sizeY gb)
        let zs = sampleLinearAxis volume.Nz (Bounds3.minZ gb) (Bounds3.sizeZ gb)
        let struct (dx, dy, dz) = scale.DataExtents
        let xMapper = classifyAxis scale.XScale
        let yMapper = classifyAxis scale.YScale
        let zMapper = classifyAxis scale.ZScale
        let wxArr, wxValid = mapAxisCoords xMapper dx xs
        let wyArr, wyValid = mapAxisCoords yMapper dy ys
        let wzArr, wzValid = mapAxisCoords zMapper dz zs

        {
            Xs = xs
            Ys = ys
            Zs = zs
            Dx = dx
            Dy = dy
            Dz = dz
            UpAxis = scale.UpAxis
            XMapper = xMapper
            YMapper = yMapper
            ZMapper = zMapper
            WxArr = wxArr
            WxValid = wxValid
            WyArr = wyArr
            WyValid = wyValid
            WzArr = wzArr
            WzValid = wzValid
        }

    let inline axisFiniteDiff
        (values: float32 array)
        (wArr: float array)
        (wValid: bool array)
        (coord: int)
        (limit: int)
        (idx: int)
        (stride: int)
        : float =
        let hasPrev = coord > 0 && wValid[coord - 1] && Check3D.isFinite32 values[idx - stride]
        let hasNext = coord < limit - 1 && wValid[coord + 1] && Check3D.isFinite32 values[idx + stride]

        if hasPrev && hasNext then
            let d = wArr[coord + 1] - wArr[coord - 1]
            if abs d > 1e-12 then float (values[idx + stride] - values[idx - stride]) / d else 0.0
        elif hasNext then
            let d = wArr[coord + 1] - wArr[coord]
            if abs d > 1e-12 then float (values[idx + stride] - values[idx]) / d else 0.0
        elif hasPrev then
            let d = wArr[coord] - wArr[coord - 1]
            if abs d > 1e-12 then float (values[idx] - values[idx - stride]) / d else 0.0
        else
            0.0

    let inline planeTo3D (axisType: int) (fixedIdx: int) (u: int) (v: int) : struct (int * int * int) =
        match axisType with
        | 0 -> struct (fixedIdx, u, v)
        | 1 -> struct (u, fixedIdx, v)
        | _ -> struct (u, v, fixedIdx)

    // Montani-disambiguated 256-case marching cubes triangle table (16 ints/row, -1 terminated).
    // Bit i = 1 iff value[i] < threshold; triangles wind CCW around outward normals of superlevel set (value >= threshold).
    let triTable: int array =
        [|
            -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   0
             3;  0;  8; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   1
             0;  1;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   2
             8;  3;  1;  1;  9;  8; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   3
             2; 10;  1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   4
             3;  0;  8;  1;  2; 10; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   5
             9;  0;  2;  2; 10;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   6
             8;  3;  2;  2; 10;  9;  2;  9;  8; -1; -1; -1; -1; -1; -1; -1; //   7
            11;  2;  3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   8
             0;  8; 11; 11;  2;  0; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //   9
             0;  1;  9;  2;  3; 11; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  10
            11;  2;  1;  1;  9;  8;  1;  8; 11; -1; -1; -1; -1; -1; -1; -1; //  11
             3; 11; 10; 10;  1;  3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  12
            10;  1;  0;  0;  8; 11;  0; 11; 10; -1; -1; -1; -1; -1; -1; -1; //  13
             9;  0;  3;  3; 11; 10;  3; 10;  9; -1; -1; -1; -1; -1; -1; -1; //  14
            10;  9;  8;  8; 11; 10; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  15
             7;  8;  4; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  16
             4;  7;  3;  3;  0;  4; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  17
             8;  4;  7;  9;  0;  1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  18
             1;  9;  4;  4;  7;  3;  4;  3;  1; -1; -1; -1; -1; -1; -1; -1; //  19
             1;  2; 10;  4;  7;  8; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  20
             7;  3;  0;  0;  4;  7;  1;  2; 10; -1; -1; -1; -1; -1; -1; -1; //  21
            10;  9;  0;  0;  2; 10;  8;  4;  7; -1; -1; -1; -1; -1; -1; -1; //  22
             3;  2; 10;  7;  3; 10;  7; 10;  9;  9;  4;  7; -1; -1; -1; -1; //  23
             7;  8;  4;  3; 11;  2; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  24
             4;  7; 11; 11;  2;  0; 11;  0;  4; -1; -1; -1; -1; -1; -1; -1; //  25
             2;  3; 11;  0;  1;  9;  4;  7;  8; -1; -1; -1; -1; -1; -1; -1; //  26
            11;  2;  1; 11;  1;  9; 11;  9;  4;  4;  7; 11; -1; -1; -1; -1; //  27
             1;  3; 11; 11; 10;  1;  7;  8;  4; -1; -1; -1; -1; -1; -1; -1; //  28
            10;  1;  0; 10;  0;  4;  4;  7; 11;  4; 11; 10; -1; -1; -1; -1; //  29
             9;  0;  3;  3; 11; 10;  3; 10;  9;  4;  7;  8; -1; -1; -1; -1; //  30
             9;  4;  7;  7; 11; 10;  7; 10;  9; -1; -1; -1; -1; -1; -1; -1; //  31
             5;  4;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  32
             4;  9;  5;  0;  8;  3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  33
             5;  4;  0;  0;  1;  5; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  34
             5;  4;  8;  8;  3;  1;  8;  1;  5; -1; -1; -1; -1; -1; -1; -1; //  35
             9;  5;  4; 10;  1;  2; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  36
             3;  0;  8;  1;  2; 10;  5;  4;  9; -1; -1; -1; -1; -1; -1; -1; //  37
             2; 10;  5;  5;  4;  0;  5;  0;  2; -1; -1; -1; -1; -1; -1; -1; //  38
             8;  3;  2;  8;  2; 10;  8; 10;  5;  5;  4;  8; -1; -1; -1; -1; //  39
             3; 11;  2;  9;  5;  4; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  40
             2;  0;  8;  8; 11;  2;  4;  9;  5; -1; -1; -1; -1; -1; -1; -1; //  41
             4;  0;  1;  1;  5;  4;  2;  3; 11; -1; -1; -1; -1; -1; -1; -1; //  42
            11;  2;  1; 11;  1;  5;  5;  4;  8;  5;  8; 11; -1; -1; -1; -1; //  43
            11; 10;  1;  1;  3; 11;  9;  5;  4; -1; -1; -1; -1; -1; -1; -1; //  44
            10;  1;  0;  0;  8; 11;  0; 11; 10;  5;  4;  9; -1; -1; -1; -1; //  45
             0;  3; 11;  4;  0; 11;  4; 11; 10; 10;  5;  4; -1; -1; -1; -1; //  46
            10;  5;  4;  4;  8; 11;  4; 11; 10; -1; -1; -1; -1; -1; -1; -1; //  47
             9;  5;  7;  7;  8;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  48
             3;  0;  9;  9;  5;  7;  9;  7;  3; -1; -1; -1; -1; -1; -1; -1; //  49
             7;  8;  0;  0;  1;  5;  0;  5;  7; -1; -1; -1; -1; -1; -1; -1; //  50
             3;  1;  5;  5;  7;  3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  51
             8;  9;  5;  5;  7;  8; 10;  1;  2; -1; -1; -1; -1; -1; -1; -1; //  52
             3;  0;  9;  9;  5;  7;  9;  7;  3;  2; 10;  1; -1; -1; -1; -1; //  53
             7;  8;  0;  7;  0;  2;  2; 10;  5;  2;  5;  7; -1; -1; -1; -1; //  54
             3;  2; 10; 10;  5;  7; 10;  7;  3; -1; -1; -1; -1; -1; -1; -1; //  55
             5;  7;  8;  8;  9;  5;  3; 11;  2; -1; -1; -1; -1; -1; -1; -1; //  56
             7; 11;  2;  5;  7;  2;  5;  2;  0;  0;  9;  5; -1; -1; -1; -1; //  57
             7;  8;  0;  0;  1;  5;  0;  5;  7; 11;  2;  3; -1; -1; -1; -1; //  58
             7; 11;  2;  2;  1;  5;  2;  5;  7; -1; -1; -1; -1; -1; -1; -1; //  59
            10;  1;  3;  3; 11; 10;  9;  5;  7;  7;  8;  9; -1; -1; -1; -1; //  60
            10;  1;  0;  0;  9;  5; 10;  0;  5; 10;  5;  7;  7; 11; 10; -1; //  61
             7;  8;  0;  0;  3; 11;  7;  0; 11;  7; 11; 10; 10;  5;  7; -1; //  62
            10;  5;  7;  7; 11; 10; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  63
             5; 10;  6; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  64
            10;  6;  5;  0;  8;  3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  65
             5; 10;  6;  1;  9;  0; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  66
             3;  1;  9;  9;  8;  3;  5; 10;  6; -1; -1; -1; -1; -1; -1; -1; //  67
             1;  2;  6;  6;  5;  1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  68
             5;  1;  2;  2;  6;  5;  3;  0;  8; -1; -1; -1; -1; -1; -1; -1; //  69
             6;  5;  9;  9;  0;  2;  9;  2;  6; -1; -1; -1; -1; -1; -1; -1; //  70
             8;  3;  2;  8;  2;  6;  6;  5;  9;  6;  9;  8; -1; -1; -1; -1; //  71
            11;  2;  3; 10;  6;  5; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  72
             8; 11;  2;  2;  0;  8; 10;  6;  5; -1; -1; -1; -1; -1; -1; -1; //  73
            11;  2;  3; 10;  6;  5;  9;  0;  1; -1; -1; -1; -1; -1; -1; -1; //  74
            11;  2;  1;  1;  9;  8;  1;  8; 11;  6;  5; 10; -1; -1; -1; -1; //  75
             3; 11;  6;  6;  5;  1;  6;  1;  3; -1; -1; -1; -1; -1; -1; -1; //  76
            11;  6;  5;  8; 11;  5;  8;  5;  1;  1;  0;  8; -1; -1; -1; -1; //  77
             3; 11;  6;  3;  6;  5;  3;  5;  9;  9;  0;  3; -1; -1; -1; -1; //  78
            11;  6;  5;  5;  9;  8;  5;  8; 11; -1; -1; -1; -1; -1; -1; -1; //  79
             4;  7;  8;  6;  5; 10; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  80
             0;  4;  7;  7;  3;  0;  6;  5; 10; -1; -1; -1; -1; -1; -1; -1; //  81
             6;  5; 10;  4;  7;  8;  0;  1;  9; -1; -1; -1; -1; -1; -1; -1; //  82
             1;  9;  4;  4;  7;  3;  4;  3;  1; 10;  6;  5; -1; -1; -1; -1; //  83
             2;  6;  5;  5;  1;  2;  4;  7;  8; -1; -1; -1; -1; -1; -1; -1; //  84
             1;  2;  6;  6;  5;  1;  3;  0;  4;  4;  7;  3; -1; -1; -1; -1; //  85
             6;  5;  9;  9;  0;  2;  9;  2;  6;  7;  8;  4; -1; -1; -1; -1; //  86
             6;  5;  9;  9;  4;  7;  6;  9;  7;  6;  7;  3;  3;  2;  6; -1; //  87
             4;  7;  8;  6;  5; 10;  2;  3; 11; -1; -1; -1; -1; -1; -1; -1; //  88
             4;  7; 11; 11;  2;  0; 11;  0;  4;  5; 10;  6; -1; -1; -1; -1; //  89
             2;  3; 11;  0;  1;  9;  4;  7;  8;  6;  5; 10; -1; -1; -1; -1; //  90
            11;  2;  1; 11;  1;  9; 11;  9;  4;  4;  7; 11;  6;  5; 10; -1; //  91
             3; 11;  6;  6;  5;  1;  6;  1;  3;  8;  4;  7; -1; -1; -1; -1; //  92
             4;  7; 11; 11;  6;  5;  4; 11;  5;  4;  5;  1;  1;  0;  4; -1; //  93
             3; 11;  6;  3;  6;  5;  3;  5;  9;  9;  0;  3;  8;  4;  7; -1; //  94
             9;  4;  7;  9;  7; 11;  9; 11;  6;  6;  5;  9; -1; -1; -1; -1; //  95
            10;  6;  4;  4;  9; 10; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; //  96
             6;  4;  9;  9; 10;  6;  0;  8;  3; -1; -1; -1; -1; -1; -1; -1; //  97
             0;  1; 10; 10;  6;  4; 10;  4;  0; -1; -1; -1; -1; -1; -1; -1; //  98
             4;  8;  3;  6;  4;  3;  6;  3;  1;  1; 10;  6; -1; -1; -1; -1; //  99
             4;  9;  1;  1;  2;  6;  1;  6;  4; -1; -1; -1; -1; -1; -1; -1; // 100
             4;  9;  1;  1;  2;  6;  1;  6;  4;  8;  3;  0; -1; -1; -1; -1; // 101
             0;  2;  6;  6;  4;  0; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 102
             4;  8;  3;  3;  2;  6;  3;  6;  4; -1; -1; -1; -1; -1; -1; -1; // 103
             9; 10;  6;  6;  4;  9; 11;  2;  3; -1; -1; -1; -1; -1; -1; -1; // 104
            11;  2;  0;  0;  8; 11; 10;  6;  4;  4;  9; 10; -1; -1; -1; -1; // 105
             0;  1; 10; 10;  6;  4; 10;  4;  0;  3; 11;  2; -1; -1; -1; -1; // 106
            11;  2;  1;  1; 10;  6; 11;  1;  6; 11;  6;  4;  4;  8; 11; -1; // 107
             3; 11;  6;  3;  6;  4;  4;  9;  1;  4;  1;  3; -1; -1; -1; -1; // 108
             4;  9;  1;  1;  0;  8;  4;  1;  8;  4;  8; 11; 11;  6;  4; -1; // 109
             0;  3; 11; 11;  6;  4; 11;  4;  0; -1; -1; -1; -1; -1; -1; -1; // 110
            11;  6;  4;  4;  8; 11; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 111
            10;  6;  7;  7;  8;  9;  7;  9; 10; -1; -1; -1; -1; -1; -1; -1; // 112
             3;  0;  9;  3;  9; 10; 10;  6;  7; 10;  7;  3; -1; -1; -1; -1; // 113
            10;  6;  7; 10;  7;  8; 10;  8;  0;  0;  1; 10; -1; -1; -1; -1; // 114
             1; 10;  6;  6;  7;  3;  6;  3;  1; -1; -1; -1; -1; -1; -1; -1; // 115
             6;  7;  8;  2;  6;  8;  2;  8;  9;  9;  1;  2; -1; -1; -1; -1; // 116
             3;  0;  9;  9;  1;  2;  3;  9;  2;  3;  2;  6;  6;  7;  3; -1; // 117
             6;  7;  8;  8;  0;  2;  8;  2;  6; -1; -1; -1; -1; -1; -1; -1; // 118
             3;  2;  6;  6;  7;  3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 119
            10;  6;  7;  7;  8;  9;  7;  9; 10;  2;  3; 11; -1; -1; -1; -1; // 120
            10;  6;  7;  7; 11;  2; 10;  7;  2; 10;  2;  0;  0;  9; 10; -1; // 121
            10;  6;  7; 10;  7;  8; 10;  8;  0;  0;  1; 10;  2;  3; 11; -1; // 122
             1; 10;  6;  1;  6;  7;  1;  7; 11; 11;  2;  1; -1; -1; -1; -1; // 123
             3; 11;  6;  6;  7;  8;  3;  6;  8;  3;  8;  9;  9;  1;  3; -1; // 124
             1;  0;  9;  6;  7; 11; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 125
             6;  7;  8;  6;  8;  0;  6;  0;  3;  3; 11;  6; -1; -1; -1; -1; // 126
             6;  7; 11; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 127
             7;  6; 11; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 128
             8;  3;  0; 11;  7;  6; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 129
            11;  7;  6;  1;  9;  0; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 130
             9;  8;  3;  3;  1;  9; 11;  7;  6; -1; -1; -1; -1; -1; -1; -1; // 131
             6; 11;  7;  2; 10;  1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 132
             6; 11;  7;  2; 10;  1;  0;  8;  3; -1; -1; -1; -1; -1; -1; -1; // 133
             0;  2; 10; 10;  9;  0;  6; 11;  7; -1; -1; -1; -1; -1; -1; -1; // 134
             8;  3;  2;  2; 10;  9;  2;  9;  8;  7;  6; 11; -1; -1; -1; -1; // 135
             7;  6;  2;  2;  3;  7; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 136
             0;  8;  7;  7;  6;  2;  7;  2;  0; -1; -1; -1; -1; -1; -1; -1; // 137
             6;  2;  3;  3;  7;  6;  0;  1;  9; -1; -1; -1; -1; -1; -1; -1; // 138
             8;  7;  6;  9;  8;  6;  9;  6;  2;  2;  1;  9; -1; -1; -1; -1; // 139
             7;  6; 10; 10;  1;  3; 10;  3;  7; -1; -1; -1; -1; -1; -1; -1; // 140
             7;  6; 10;  7; 10;  1;  7;  1;  0;  0;  8;  7; -1; -1; -1; -1; // 141
             7;  6; 10;  7; 10;  9;  9;  0;  3;  9;  3;  7; -1; -1; -1; -1; // 142
             8;  7;  6;  6; 10;  9;  6;  9;  8; -1; -1; -1; -1; -1; -1; -1; // 143
             8;  4;  6;  6; 11;  8; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 144
             6; 11;  3;  3;  0;  4;  3;  4;  6; -1; -1; -1; -1; -1; -1; -1; // 145
            11;  8;  4;  4;  6; 11;  9;  0;  1; -1; -1; -1; -1; -1; -1; -1; // 146
             6; 11;  3;  6;  3;  1;  1;  9;  4;  1;  4;  6; -1; -1; -1; -1; // 147
             4;  6; 11; 11;  8;  4;  2; 10;  1; -1; -1; -1; -1; -1; -1; -1; // 148
             6; 11;  3;  3;  0;  4;  3;  4;  6; 10;  1;  2; -1; -1; -1; -1; // 149
             6; 11;  8;  8;  4;  6;  2; 10;  9;  9;  0;  2; -1; -1; -1; -1; // 150
             6; 11;  3;  3;  2; 10;  6;  3; 10;  6; 10;  9;  9;  4;  6; -1; // 151
             2;  3;  8;  8;  4;  6;  8;  6;  2; -1; -1; -1; -1; -1; -1; -1; // 152
             2;  0;  4;  4;  6;  2; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 153
             2;  3;  8;  8;  4;  6;  8;  6;  2;  1;  9;  0; -1; -1; -1; -1; // 154
             2;  1;  9;  9;  4;  6;  9;  6;  2; -1; -1; -1; -1; -1; -1; -1; // 155
             6; 10;  1;  4;  6;  1;  4;  1;  3;  3;  8;  4; -1; -1; -1; -1; // 156
             6; 10;  1;  1;  0;  4;  1;  4;  6; -1; -1; -1; -1; -1; -1; -1; // 157
             9;  0;  3;  3;  8;  4;  9;  3;  4;  9;  4;  6;  6; 10;  9; -1; // 158
             6; 10;  9;  9;  4;  6; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 159
             7;  6; 11;  5;  4;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 160
            11;  7;  6;  8;  3;  0;  9;  5;  4; -1; -1; -1; -1; -1; -1; -1; // 161
             1;  5;  4;  4;  0;  1;  7;  6; 11; -1; -1; -1; -1; -1; -1; -1; // 162
             5;  4;  8;  8;  3;  1;  8;  1;  5;  6; 11;  7; -1; -1; -1; -1; // 163
             9;  5;  4; 10;  1;  2; 11;  7;  6; -1; -1; -1; -1; -1; -1; -1; // 164
             6; 11;  7;  2; 10;  1;  0;  8;  3;  4;  9;  5; -1; -1; -1; -1; // 165
             2; 10;  5;  5;  4;  0;  5;  0;  2; 11;  7;  6; -1; -1; -1; -1; // 166
             8;  3;  2;  8;  2; 10;  8; 10;  5;  5;  4;  8;  7;  6; 11; -1; // 167
             3;  7;  6;  6;  2;  3;  5;  4;  9; -1; -1; -1; -1; -1; -1; -1; // 168
             0;  8;  7;  7;  6;  2;  7;  2;  0;  9;  5;  4; -1; -1; -1; -1; // 169
             2;  3;  7;  7;  6;  2;  0;  1;  5;  5;  4;  0; -1; -1; -1; -1; // 170
             5;  4;  8;  8;  7;  6;  5;  8;  6;  5;  6;  2;  2;  1;  5; -1; // 171
             7;  6; 10; 10;  1;  3; 10;  3;  7;  4;  9;  5; -1; -1; -1; -1; // 172
             7;  6; 10;  7; 10;  1;  7;  1;  0;  0;  8;  7;  4;  9;  5; -1; // 173
             7;  6; 10; 10;  5;  4;  7; 10;  4;  7;  4;  0;  0;  3;  7; -1; // 174
            10;  5;  4; 10;  4;  8; 10;  8;  7;  7;  6; 10; -1; -1; -1; -1; // 175
             9;  5;  6;  6; 11;  8;  6;  8;  9; -1; -1; -1; -1; -1; -1; -1; // 176
             6; 11;  3;  6;  3;  0;  6;  0;  9;  9;  5;  6; -1; -1; -1; -1; // 177
             5;  6; 11;  1;  5; 11;  1; 11;  8;  8;  0;  1; -1; -1; -1; -1; // 178
             5;  6; 11; 11;  3;  1; 11;  1;  5; -1; -1; -1; -1; -1; -1; -1; // 179
             9;  5;  6;  6; 11;  8;  6;  8;  9;  1;  2; 10; -1; -1; -1; -1; // 180
             6; 11;  3;  6;  3;  0;  6;  0;  9;  9;  5;  6; 10;  1;  2; -1; // 181
             2; 10;  5;  5;  6; 11;  2;  5; 11;  2; 11;  8;  8;  0;  2; -1; // 182
             5;  6; 11;  5; 11;  3;  5;  3;  2;  2; 10;  5; -1; -1; -1; -1; // 183
             2;  3;  8;  2;  8;  9;  9;  5;  6;  9;  6;  2; -1; -1; -1; -1; // 184
             0;  9;  5;  5;  6;  2;  5;  2;  0; -1; -1; -1; -1; -1; -1; -1; // 185
             2;  3;  8;  8;  0;  1;  2;  8;  1;  2;  1;  5;  5;  6;  2; -1; // 186
             2;  1;  5;  5;  6;  2; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 187
             9;  5;  6;  6; 10;  1;  9;  6;  1;  9;  1;  3;  3;  8;  9; -1; // 188
             0;  9;  5;  0;  5;  6;  0;  6; 10; 10;  1;  0; -1; -1; -1; -1; // 189
             3;  8;  0; 10;  5;  6; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 190
            10;  5;  6; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 191
            11;  7;  5;  5; 10; 11; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 192
            10; 11;  7;  7;  5; 10;  8;  3;  0; -1; -1; -1; -1; -1; -1; -1; // 193
             7;  5; 10; 10; 11;  7;  1;  9;  0; -1; -1; -1; -1; -1; -1; -1; // 194
             5; 10; 11; 11;  7;  5;  1;  9;  8;  8;  3;  1; -1; -1; -1; -1; // 195
             1;  2; 11; 11;  7;  5; 11;  5;  1; -1; -1; -1; -1; -1; -1; -1; // 196
             1;  2; 11; 11;  7;  5; 11;  5;  1;  0;  8;  3; -1; -1; -1; -1; // 197
             2; 11;  7;  0;  2;  7;  0;  7;  5;  5;  9;  0; -1; -1; -1; -1; // 198
             8;  3;  2;  2; 11;  7;  8;  2;  7;  8;  7;  5;  5;  9;  8; -1; // 199
             5; 10;  2;  2;  3;  7;  2;  7;  5; -1; -1; -1; -1; -1; -1; -1; // 200
             5; 10;  2;  5;  2;  0;  0;  8;  7;  0;  7;  5; -1; -1; -1; -1; // 201
             5; 10;  2;  2;  3;  7;  2;  7;  5;  9;  0;  1; -1; -1; -1; -1; // 202
             5; 10;  2;  2;  1;  9;  5;  2;  9;  5;  9;  8;  8;  7;  5; -1; // 203
             5;  1;  3;  3;  7;  5; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 204
             1;  0;  8;  8;  7;  5;  8;  5;  1; -1; -1; -1; -1; -1; -1; -1; // 205
             5;  9;  0;  0;  3;  7;  0;  7;  5; -1; -1; -1; -1; -1; -1; -1; // 206
             5;  9;  8;  8;  7;  5; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 207
             8;  4;  5;  5; 10; 11;  5; 11;  8; -1; -1; -1; -1; -1; -1; -1; // 208
             4;  5; 10;  0;  4; 10;  0; 10; 11; 11;  3;  0; -1; -1; -1; -1; // 209
             8;  4;  5;  5; 10; 11;  5; 11;  8;  0;  1;  9; -1; -1; -1; -1; // 210
             1;  9;  4;  4;  5; 10;  1;  4; 10;  1; 10; 11; 11;  3;  1; -1; // 211
             1;  2; 11;  1; 11;  8;  8;  4;  5;  8;  5;  1; -1; -1; -1; -1; // 212
             1;  2; 11; 11;  3;  0;  1; 11;  0;  1;  0;  4;  4;  5;  1; -1; // 213
             8;  4;  5;  5;  9;  0;  8;  5;  0;  8;  0;  2;  2; 11;  8; -1; // 214
             2; 11;  3;  9;  4;  5; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 215
             8;  4;  5;  8;  5; 10;  8; 10;  2;  2;  3;  8; -1; -1; -1; -1; // 216
             4;  5; 10; 10;  2;  0; 10;  0;  4; -1; -1; -1; -1; -1; -1; -1; // 217
             8;  4;  5;  8;  5; 10;  8; 10;  2;  2;  3;  8;  0;  1;  9; -1; // 218
             2;  1;  9;  2;  9;  4;  2;  4;  5;  5; 10;  2; -1; -1; -1; -1; // 219
             3;  8;  4;  4;  5;  1;  4;  1;  3; -1; -1; -1; -1; -1; -1; -1; // 220
             1;  0;  4;  4;  5;  1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 221
             3;  8;  4;  3;  4;  5;  3;  5;  9;  9;  0;  3; -1; -1; -1; -1; // 222
             9;  4;  5; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 223
            11;  7;  4;  4;  9; 10;  4; 10; 11; -1; -1; -1; -1; -1; -1; -1; // 224
            11;  7;  4;  4;  9; 10;  4; 10; 11;  3;  0;  8; -1; -1; -1; -1; // 225
            11;  7;  4; 11;  4;  0;  0;  1; 10;  0; 10; 11; -1; -1; -1; -1; // 226
            11;  7;  4;  4;  8;  3; 11;  4;  3; 11;  3;  1;  1; 10; 11; -1; // 227
             4;  9;  1;  4;  1;  2;  4;  2; 11; 11;  7;  4; -1; -1; -1; -1; // 228
             4;  9;  1;  4;  1;  2;  4;  2; 11; 11;  7;  4;  8;  3;  0; -1; // 229
             2; 11;  7;  7;  4;  0;  7;  0;  2; -1; -1; -1; -1; -1; -1; -1; // 230
             2; 11;  7;  2;  7;  4;  2;  4;  8;  8;  3;  2; -1; -1; -1; -1; // 231
             7;  4;  9;  3;  7;  9;  3;  9; 10; 10;  2;  3; -1; -1; -1; -1; // 232
             0;  8;  7;  7;  4;  9;  0;  7;  9;  0;  9; 10; 10;  2;  0; -1; // 233
             0;  1; 10; 10;  2;  3;  0; 10;  3;  0;  3;  7;  7;  4;  0; -1; // 234
            10;  2;  1;  4;  8;  7; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 235
             7;  4;  9;  9;  1;  3;  9;  3;  7; -1; -1; -1; -1; -1; -1; -1; // 236
             1;  0;  8;  1;  8;  7;  1;  7;  4;  4;  9;  1; -1; -1; -1; -1; // 237
             7;  4;  0;  0;  3;  7; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 238
             4;  8;  7; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 239
             9; 10; 11; 11;  8;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 240
            11;  3;  0;  0;  9; 10;  0; 10; 11; -1; -1; -1; -1; -1; -1; -1; // 241
             8;  0;  1;  1; 10; 11;  1; 11;  8; -1; -1; -1; -1; -1; -1; -1; // 242
            11;  3;  1;  1; 10; 11; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 243
             9;  1;  2;  2; 11;  8;  2;  8;  9; -1; -1; -1; -1; -1; -1; -1; // 244
            11;  3;  0; 11;  0;  9; 11;  9;  1;  1;  2; 11; -1; -1; -1; -1; // 245
             8;  0;  2;  2; 11;  8; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 246
             2; 11;  3; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 247
            10;  2;  3;  3;  8;  9;  3;  9; 10; -1; -1; -1; -1; -1; -1; -1; // 248
            10;  2;  0;  0;  9; 10; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 249
            10;  2;  3; 10;  3;  8; 10;  8;  0;  0;  1; 10; -1; -1; -1; -1; // 250
             2;  1; 10; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 251
             9;  1;  3;  3;  8;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 252
             1;  0;  9; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 253
             0;  3;  8; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 254
            -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; // 255
        |]

/// <summary>Validating constructors and dimension/bounds queries for <c>VolumeGrid</c>.</summary>
[<RequireQualifiedAccess>]
module VolumeGrid =
    /// <summary>
    /// Creates a uniform 3D scalar grid over <c>bounds</c> enforcing <c>maxVoxels</c>, <c>nx, ny, nz &gt;= 2</c>,
    /// positive bounds extents, and X-fastest voxel ordering <c>ix + nx * (iy + ny * iz)</c>;
    /// <c>secondaryScalars</c> may be empty or match <c>nx * ny * nz</c>.
    /// </summary>
    let uniformWithBudget
        (maxVoxels: int)
        (nx: int)
        (ny: int)
        (nz: int)
        (bounds: Bounds3)
        (values: ReadOnlySpan<float32>)
        (secondaryScalars: ReadOnlySpan<float32>)
        : Result<VolumeGrid, LayoutError list> =
        match VolumeKernel.validateGrid maxVoxels nx ny nz bounds with
        | Error errs -> Error errs
        | Ok count ->
            if values.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected {count} primary scalar samples for {nx}x{ny}x{nz} volume, got {values.Length}." ]
            elif secondaryScalars.Length <> 0 && secondaryScalars.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected 0 or {count} secondary scalar samples for {nx}x{ny}x{nz} volume, got {secondaryScalars.Length}." ]
            else
                Ok
                    {
                        Nx = nx
                        Ny = ny
                        Nz = nz
                        GridBounds = bounds
                        Values = VolumeKernel.copySpan32 values
                        SecondaryScalars = VolumeKernel.copySpan32 secondaryScalars
                    }

    /// <summary>Creates a uniform 3D scalar grid with the default <c>4,194,304</c>-voxel budget (<c>160^3</c> range).</summary>
    let uniform
        (nx: int)
        (ny: int)
        (nz: int)
        (bounds: Bounds3)
        (values: ReadOnlySpan<float32>)
        (secondaryScalars: ReadOnlySpan<float32>)
        : Result<VolumeGrid, LayoutError list> =
        uniformWithBudget VolumeKernel.DefaultMaxVoxels nx ny nz bounds values secondaryScalars

    /// <summary>
    /// Samples an analytical 3D scalar field (and optional secondary coloring field) across <c>bounds</c>
    /// in X-fastest voxel order <c>ix + nx * (iy + ny * iz)</c>.
    /// </summary>
    let fromFunction
        (nx: int)
        (ny: int)
        (nz: int)
        (bounds: Bounds3)
        (eval: float -> float -> float -> float32)
        (evalColorScalar: (float -> float -> float -> float32) voption)
        : Result<VolumeGrid, LayoutError list> =
        if isNull (box eval) then
            nullArg (nameof eval)

        match evalColorScalar with
        | ValueSome f when isNull (box f) -> nullArg (nameof evalColorScalar)
        | _ -> ()

        match VolumeKernel.validateGrid VolumeKernel.DefaultMaxVoxels nx ny nz bounds with
        | Error errs -> Error errs
        | Ok count ->
            let xs = VolumeKernel.sampleLinearAxis nx (Bounds3.minX bounds) (Bounds3.sizeX bounds)
            let ys = VolumeKernel.sampleLinearAxis ny (Bounds3.minY bounds) (Bounds3.sizeY bounds)
            let zs = VolumeKernel.sampleLinearAxis nz (Bounds3.minZ bounds) (Bounds3.sizeZ bounds)
            let values = Array.zeroCreate<float32> count

            let secondary =
                match evalColorScalar with
                | ValueSome _ -> Array.zeroCreate<float32> count
                | ValueNone -> [||]

            for iz = 0 to nz - 1 do
                let z = zs[iz]
                let zStride = iz * ny

                for iy = 0 to ny - 1 do
                    let y = ys[iy]
                    let yOffset = nx * (iy + zStride)

                    for ix = 0 to nx - 1 do
                        let x = xs[ix]
                        let idx = ix + yOffset
                        values[idx] <- eval x y z

                        match evalColorScalar with
                        | ValueSome f -> secondary[idx] <- f x y z
                        | ValueNone -> ()

            Ok
                {
                    Nx = nx
                    Ny = ny
                    Nz = nz
                    GridBounds = bounds
                    Values = values
                    SecondaryScalars = secondary
                }

    /// <summary>Returns the grid sample counts <c>struct (nx, ny, nz)</c>.</summary>
    let dimensions (volume: VolumeGrid) : struct (int * int * int) =
        if isNull (box volume) then
            nullArg (nameof volume)

        struct (volume.Nx, volume.Ny, volume.Nz)

    /// <summary>Returns the data-space <c>Bounds3</c> extent of the volume grid.</summary>
    let bounds (volume: VolumeGrid) : Bounds3 =
        if isNull (box volume) then
            nullArg (nameof volume)

        volume.GridBounds

/// <summary>Marching-cubes isosurface extraction from a <c>VolumeGrid</c> with shared-edge deduplication and boundary capping.</summary>
[<RequireQualifiedAccess>]
module IsoSurface3D =
    /// <summary>
    /// Extracts a triangulated superlevel-set <c>MeshData</c> (<c>value &gt;= config.Threshold</c>) with shared-edge vertex deduplication,
    /// central-difference gradient normals, optional boundary face caps, secondary scalar coloring, budget guards, and periodic <c>CancellationToken</c> checks.
    /// </summary>
    let computeWithCancellation
        (token: CancellationToken)
        (scale: Scale3D)
        (color: ColorRamp)
        (config: IsoSurfaceConfig)
        (volume: VolumeGrid)
        : Result<MeshData, LayoutError list> =
        VolumeKernel.checkCancellation token

        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box color) then
            nullArg (nameof color)

        if isNull (box volume) then
            nullArg (nameof volume)

        if not (Check3D.isFinite32 config.Threshold) then
            Error [ LayoutError.InvalidObservation "Iso-surface threshold must be finite." ]
        elif config.MaxTriangles < 0 || config.MaxVertices < 0 then
            Error [ LayoutError.InvalidObservation "Iso-surface triangle and vertex budgets must be nonnegative." ]
        else
            let nx = volume.Nx
            let ny = volume.Ny
            let nz = volume.Nz
            let strideY = nx
            let strideZ = nx * ny
            let mapped = VolumeKernel.prepareMappedAxes scale volume
            let xs, ys, zs = mapped.Xs, mapped.Ys, mapped.Zs
            let dx, dy, dz = mapped.Dx, mapped.Dy, mapped.Dz
            let upAxis = mapped.UpAxis
            let xMapper, yMapper, zMapper = mapped.XMapper, mapped.YMapper, mapped.ZMapper
            let wxArr, wxValid = mapped.WxArr, mapped.WxValid
            let wyArr, wyValid = mapped.WyArr, mapped.WyValid
            let wzArr, wzValid = mapped.WzArr, mapped.WzValid
            let flipWinding = (VolumeKernel.axisDir scale.XScale * VolumeKernel.axisDir scale.YScale * VolumeKernel.axisDir scale.ZScale) < 0.0

            let values = volume.Values
            let secondary = volume.SecondaryScalars
            let hasSecondary = secondary.Length > 0
            let threshold = config.Threshold
            let maxVerts = config.MaxVertices
            let maxTris = config.MaxTriangles

            let verts = ResizeArray<Vertex3D>()
            let inds = ResizeArray<uint32>()
            let mutable triCount = 0
            let mutable budgetError: LayoutError voption = ValueNone

            let inline voxelIndex (ix: int) (iy: int) (iz: int) = ix + strideY * iy + strideZ * iz

            let voxelOutwardGrad (ix: int) (iy: int) (iz: int) : Vector3 =
                let idx = voxelIndex ix iy iz
                let dfx = VolumeKernel.axisFiniteDiff values wxArr wxValid ix nx idx 1
                let dfy = VolumeKernel.axisFiniteDiff values wyArr wyValid iy ny idx strideY
                let dfz = VolumeKernel.axisFiniteDiff values wzArr wzValid iz nz idx strideZ

                match upAxis with
                | UpAxis.ZUp -> Vector3 (float32 (-dfx), float32 (-dfz), float32 dfy)
                | UpAxis.YUp -> Vector3 (float32 (-dfx), float32 (-dfy), float32 (-dfz))

            let inline emitEdgeVertex (x0: int) (y0: int) (z0: int) (x1: int) (y1: int) (z1: int) (cache: int array) (slot: int) (fixedNormal: Vector3 voption) : int =
                let existing = cache[slot]

                if existing <> -1 then
                    existing
                else
                    let idx0 = voxelIndex x0 y0 z0
                    let idx1 = voxelIndex x1 y1 z1
                    let v0 = values[idx0]
                    let v1 = values[idx1]
                    let dv = v1 - v0
                    let mu = if abs dv <= 1e-20f then 0.5f else Math.Clamp ((threshold - v0) / dv, 0.0f, 1.0f)
                    let muf = float mu

                    let wx =
                        if x0 = x1 then wxArr[x0]
                        else VolumeKernel.mapInterpolatedAxis xMapper dx xs[x0] xs[x1] wxArr[x0] wxArr[x1] muf

                    let wy =
                        if y0 = y1 then wyArr[y0]
                        else VolumeKernel.mapInterpolatedAxis yMapper dy ys[y0] ys[y1] wyArr[y0] wyArr[y1] muf

                    let wz =
                        if z0 = z1 then wzArr[z0]
                        else VolumeKernel.mapInterpolatedAxis zMapper dz zs[z0] zs[z1] wzArr[z0] wzArr[z1] muf

                    let pos = VolumeKernel.worldPoint upAxis wx wy wz

                    let scalar =
                        if hasSecondary then
                            let s0 = float secondary[idx0]
                            let s1 = float secondary[idx1]
                            s0 + (s1 - s0) * muf
                        else
                            float threshold

                    match ColorRamp.tryMap color scalar with
                    | ValueNone ->
                        cache[slot] <- -2
                        -2
                    | ValueSome rgba ->
                        if verts.Count >= maxVerts then
                            budgetError <- ValueSome (LayoutError.BudgetExceeded $"Iso-surface vertex count exceeds MaxVertices ({maxVerts}).")
                            -2
                        else
                            let normal =
                                match fixedNormal with
                                | ValueSome fn -> fn
                                | ValueNone ->
                                    let g0 = voxelOutwardGrad x0 y0 z0
                                    let g1 = voxelOutwardGrad x1 y1 z1
                                    let g = Vector3.Lerp (g0, g1, mu)
                                    let fallbackDir =
                                        let p0 = VolumeKernel.worldPoint upAxis wxArr[x0] wyArr[y0] wzArr[z0]
                                        let p1 = VolumeKernel.worldPoint upAxis wxArr[x1] wyArr[y1] wzArr[z1]
                                        let d = if v0 >= threshold then p1 - p0 else p0 - p1
                                        VolumeKernel.safeNormalize (float d.X) (float d.Y) (float d.Z) Vector3.UnitY

                                    VolumeKernel.safeNormalize (float g.X) (float g.Y) (float g.Z) fallbackDir

                            let vIdx = verts.Count
                            verts.Add (Vertex3D (pos, normal, rgba.ToVector4 ()))
                            cache[slot] <- vIdx
                            vIdx

            let inline emitCornerCapVertex (ix: int) (iy: int) (iz: int) (cache: int array) (slot: int) (capNormal: Vector3) : int =
                let existing = cache[slot]

                if existing <> -1 then
                    existing
                else
                    let idx = voxelIndex ix iy iz
                    let pos = VolumeKernel.worldPoint upAxis wxArr[ix] wyArr[iy] wzArr[iz]

                    let scalar =
                        if hasSecondary then float secondary[idx] else float values[idx]

                    match ColorRamp.tryMap color scalar with
                    | ValueNone ->
                        cache[slot] <- -2
                        -2
                    | ValueSome rgba ->
                        if verts.Count >= maxVerts then
                            budgetError <- ValueSome (LayoutError.BudgetExceeded $"Iso-surface vertex count exceeds MaxVertices ({maxVerts}).")
                            -2
                        else
                            let vIdx = verts.Count
                            verts.Add (Vertex3D (pos, capNormal, rgba.ToVector4 ()))
                            cache[slot] <- vIdx
                            vIdx

            let inline addTriangle (i0: int) (i1: int) (i2: int) =
                if i0 >= 0 && i1 >= 0 && i2 >= 0 && i0 <> i1 && i1 <> i2 && i2 <> i0 then
                    let p0 = verts[i0].Position
                    let p1 = verts[i1].Position
                    let p2 = verts[i2].Position
                    let cross = Vector3.Cross (p1 - p0, p2 - p0)

                    if cross.LengthSquared () > 1e-20f then
                        if triCount >= maxTris then
                            budgetError <- ValueSome (LayoutError.BudgetExceeded $"Iso-surface triangle count exceeds MaxTriangles ({maxTris}).")
                        else
                            inds.Add (uint32 i0)

                            if flipWinding then
                                inds.Add (uint32 i2)
                                inds.Add (uint32 i1)
                            else
                                inds.Add (uint32 i1)
                                inds.Add (uint32 i2)

                            triCount <- triCount + 1

            // Z-slab edge caches for shared-edge deduplication
            let xSliceCount = (nx - 1) * ny
            let ySliceCount = nx * (ny - 1)
            let zSlabCount = nx * ny
            let mutable xEdgesCurr = Array.create xSliceCount -1
            let mutable xEdgesNext = Array.create xSliceCount -1
            let mutable yEdgesCurr = Array.create ySliceCount -1
            let mutable yEdgesNext = Array.create ySliceCount -1
            let zEdges = Array.create zSlabCount -1

            let resolveInteriorEdge (ix: int) (iy: int) (iz: int) (edge: int) : int =
                match edge with
                | 0 -> emitEdgeVertex ix iy iz (ix + 1) iy iz xEdgesCurr (iy * (nx - 1) + ix) ValueNone
                | 1 -> emitEdgeVertex (ix + 1) iy iz (ix + 1) (iy + 1) iz yEdgesCurr (iy * nx + ix + 1) ValueNone
                | 2 -> emitEdgeVertex ix (iy + 1) iz (ix + 1) (iy + 1) iz xEdgesCurr ((iy + 1) * (nx - 1) + ix) ValueNone
                | 3 -> emitEdgeVertex ix iy iz ix (iy + 1) iz yEdgesCurr (iy * nx + ix) ValueNone
                | 4 -> emitEdgeVertex ix iy (iz + 1) (ix + 1) iy (iz + 1) xEdgesNext (iy * (nx - 1) + ix) ValueNone
                | 5 -> emitEdgeVertex (ix + 1) iy (iz + 1) (ix + 1) (iy + 1) (iz + 1) yEdgesNext (iy * nx + ix + 1) ValueNone
                | 6 -> emitEdgeVertex ix (iy + 1) (iz + 1) (ix + 1) (iy + 1) (iz + 1) xEdgesNext ((iy + 1) * (nx - 1) + ix) ValueNone
                | 7 -> emitEdgeVertex ix iy (iz + 1) ix (iy + 1) (iz + 1) yEdgesNext (iy * nx + ix) ValueNone
                | 8 -> emitEdgeVertex ix iy iz ix iy (iz + 1) zEdges (iy * nx + ix) ValueNone
                | 9 -> emitEdgeVertex (ix + 1) iy iz (ix + 1) iy (iz + 1) zEdges (iy * nx + ix + 1) ValueNone
                | 10 -> emitEdgeVertex (ix + 1) (iy + 1) iz (ix + 1) (iy + 1) (iz + 1) zEdges ((iy + 1) * nx + ix + 1) ValueNone
                | _ -> emitEdgeVertex ix (iy + 1) iz ix (iy + 1) (iz + 1) zEdges ((iy + 1) * nx + ix) ValueNone

            let mutable iz = 0

            while iz < nz - 1 && budgetError.IsNone do
                VolumeKernel.checkCancellation token

                if wzValid[iz] && wzValid[iz + 1] then
                    let mutable iy = 0

                    while iy < ny - 1 && budgetError.IsNone do
                        if wyValid[iy] && wyValid[iy + 1] then
                            let mutable ix = 0

                            while ix < nx - 1 && budgetError.IsNone do
                                if wxValid[ix] && wxValid[ix + 1] then
                                    let i0 = voxelIndex ix iy iz
                                    let i1 = i0 + 1
                                    let i3 = i0 + strideY
                                    let i2 = i3 + 1
                                    let i4 = i0 + strideZ
                                    let i5 = i4 + 1
                                    let i7 = i4 + strideY
                                    let i6 = i7 + 1

                                    let v0 = values[i0]
                                    let v1 = values[i1]
                                    let v2 = values[i2]
                                    let v3 = values[i3]
                                    let v4 = values[i4]
                                    let v5 = values[i5]
                                    let v6 = values[i6]
                                    let v7 = values[i7]

                                    if
                                        Check3D.isFinite32 v0
                                        && Check3D.isFinite32 v1
                                        && Check3D.isFinite32 v2
                                        && Check3D.isFinite32 v3
                                        && Check3D.isFinite32 v4
                                        && Check3D.isFinite32 v5
                                        && Check3D.isFinite32 v6
                                        && Check3D.isFinite32 v7
                                    then
                                        let mutable cubeMask = 0
                                        if v0 < threshold then cubeMask <- cubeMask ||| 1
                                        if v1 < threshold then cubeMask <- cubeMask ||| 2
                                        if v2 < threshold then cubeMask <- cubeMask ||| 4
                                        if v3 < threshold then cubeMask <- cubeMask ||| 8
                                        if v4 < threshold then cubeMask <- cubeMask ||| 16
                                        if v5 < threshold then cubeMask <- cubeMask ||| 32
                                        if v6 < threshold then cubeMask <- cubeMask ||| 64
                                        if v7 < threshold then cubeMask <- cubeMask ||| 128

                                        if cubeMask <> 0 && cubeMask <> 255 then
                                            let row = cubeMask * 16
                                            let mutable k = 0

                                            while k < 15 && VolumeKernel.triTable[row + k] <> -1 && budgetError.IsNone do
                                                let a = resolveInteriorEdge ix iy iz VolumeKernel.triTable[row + k]
                                                let b = resolveInteriorEdge ix iy iz VolumeKernel.triTable[row + k + 1]
                                                let c = resolveInteriorEdge ix iy iz VolumeKernel.triTable[row + k + 2]
                                                addTriangle a b c
                                                k <- k + 3

                                ix <- ix + 1

                        iy <- iy + 1

                let tmpX = xEdgesCurr
                xEdgesCurr <- xEdgesNext
                xEdgesNext <- tmpX
                VolumeKernel.clearIntArray xEdgesNext

                let tmpY = yEdgesCurr
                yEdgesCurr <- yEdgesNext
                yEdgesNext <- tmpY
                VolumeKernel.clearIntArray yEdgesNext

                VolumeKernel.clearIntArray zEdges
                iz <- iz + 1

            // Optional boundary capping across the 6 Bounds3 faces
            if config.CapBoundaries && budgetError.IsNone then
                let sX = float32 (VolumeKernel.axisDir scale.XScale)
                let sY = float32 (VolumeKernel.axisDir scale.YScale)
                let sZ = float32 (VolumeKernel.axisDir scale.ZScale)

                let unitX, unitY, unitZ =
                    match upAxis with
                    | UpAxis.ZUp ->
                        Vector3 (sX, 0.0f, 0.0f),
                        Vector3 (0.0f, 0.0f, -sY),
                        Vector3 (0.0f, sZ, 0.0f)
                    | UpAxis.YUp ->
                        Vector3 (sX, 0.0f, 0.0f),
                        Vector3 (0.0f, sY, 0.0f),
                        Vector3 (0.0f, 0.0f, sZ)

                let faceDescriptors =
                    [|
                        // x = 0 (virtual ix = -1 -> corners 0,3,4,7)
                        {
                            AxisType = 0
                            FixedIdx = 0
                            Nu = ny
                            Nv = nz
                            OutwardNormal = -unitX
                            BaseMask = (1 <<< 0) ||| (1 <<< 3) ||| (1 <<< 4) ||| (1 <<< 7)
                            RealSlot0 = 1
                            RealSlot1 = 2
                            RealSlot2 = 6
                            RealSlot3 = 5
                            EdgeSlotKind = [| 0; 4; 1; -1; 3; 5; 2; -1; -1; 6; 7; -1 |]
                        }
                        // x = nx - 1 (virtual ix = nx -> corners 1,2,5,6)
                        {
                            AxisType = 0
                            FixedIdx = nx - 1
                            Nu = ny
                            Nv = nz
                            OutwardNormal = unitX
                            BaseMask = (1 <<< 1) ||| (1 <<< 2) ||| (1 <<< 5) ||| (1 <<< 6)
                            RealSlot0 = 0
                            RealSlot1 = 3
                            RealSlot2 = 7
                            RealSlot3 = 4
                            EdgeSlotKind = [| 0; -1; 1; 4; 3; -1; 2; 5; 6; -1; -1; 7 |]
                        }
                        // y = 0 (virtual iy = -1 -> corners 0,1,4,5)
                        {
                            AxisType = 1
                            FixedIdx = 0
                            Nu = nx
                            Nv = nz
                            OutwardNormal = -unitY
                            BaseMask = (1 <<< 0) ||| (1 <<< 1) ||| (1 <<< 4) ||| (1 <<< 5)
                            RealSlot0 = 3
                            RealSlot1 = 2
                            RealSlot2 = 6
                            RealSlot3 = 7
                            EdgeSlotKind = [| -1; 1; 4; 0; -1; 2; 5; 3; -1; -1; 7; 6 |]
                        }
                        // y = ny - 1 (virtual iy = ny -> corners 2,3,6,7)
                        {
                            AxisType = 1
                            FixedIdx = ny - 1
                            Nu = nx
                            Nv = nz
                            OutwardNormal = unitY
                            BaseMask = (1 <<< 2) ||| (1 <<< 3) ||| (1 <<< 6) ||| (1 <<< 7)
                            RealSlot0 = 0
                            RealSlot1 = 1
                            RealSlot2 = 5
                            RealSlot3 = 4
                            EdgeSlotKind = [| 4; 1; -1; 0; 5; 2; -1; 3; 6; 7; -1; -1 |]
                        }
                        // z = 0 (virtual iz = -1 -> corners 0,1,2,3)
                        {
                            AxisType = 2
                            FixedIdx = 0
                            Nu = nx
                            Nv = ny
                            OutwardNormal = -unitZ
                            BaseMask = (1 <<< 0) ||| (1 <<< 1) ||| (1 <<< 2) ||| (1 <<< 3)
                            RealSlot0 = 4
                            RealSlot1 = 5
                            RealSlot2 = 6
                            RealSlot3 = 7
                            EdgeSlotKind = [| -1; -1; -1; -1; 4; 7; 5; 6; 0; 1; 2; 3 |]
                        }
                        // z = nz - 1 (virtual iz = nz -> corners 4,5,6,7)
                        {
                            AxisType = 2
                            FixedIdx = nz - 1
                            Nu = nx
                            Nv = ny
                            OutwardNormal = unitZ
                            BaseMask = (1 <<< 4) ||| (1 <<< 5) ||| (1 <<< 6) ||| (1 <<< 7)
                            RealSlot0 = 0
                            RealSlot1 = 1
                            RealSlot2 = 2
                            RealSlot3 = 3
                            EdgeSlotKind = [| 4; 7; 5; 6; -1; -1; -1; -1; 0; 1; 2; 3 |]
                        }
                    |]

                let emitBoundaryFace (desc: BoundaryFaceDesc) =
                    VolumeKernel.checkCancellation token

                    let fixedValid =
                        match desc.AxisType with
                        | 0 -> wxValid[desc.FixedIdx]
                        | 1 -> wyValid[desc.FixedIdx]
                        | _ -> wzValid[desc.FixedIdx]

                    if fixedValid then
                        let nu = desc.Nu
                        let nv = desc.Nv
                        let cornerCache = Array.create (nu * nv) -1
                        let uEdgeCache = Array.create ((nu - 1) * nv) -1
                        let vEdgeCache = Array.create (nu * (nv - 1)) -1
                        let capNormal = ValueSome desc.OutwardNormal

                        let inline to3D u v = VolumeKernel.planeTo3D desc.AxisType desc.FixedIdx u v

                        let inline isUVValid u v =
                            match desc.AxisType with
                            | 0 -> wyValid[u] && wzValid[v]
                            | 1 -> wxValid[u] && wzValid[v]
                            | _ -> wxValid[u] && wyValid[v]

                        let inline resolveCapEdge (u: int) (v: int) (edge: int) : int =
                            let kind = desc.EdgeSlotKind[edge]

                            match kind with
                            | 0 ->
                                let struct (ax, ay, az) = to3D u v
                                emitCornerCapVertex ax ay az cornerCache (v * nu + u) desc.OutwardNormal
                            | 1 ->
                                let struct (ax, ay, az) = to3D (u + 1) v
                                emitCornerCapVertex ax ay az cornerCache (v * nu + u + 1) desc.OutwardNormal
                            | 2 ->
                                let struct (ax, ay, az) = to3D (u + 1) (v + 1)
                                emitCornerCapVertex ax ay az cornerCache ((v + 1) * nu + u + 1) desc.OutwardNormal
                            | 3 ->
                                let struct (ax, ay, az) = to3D u (v + 1)
                                emitCornerCapVertex ax ay az cornerCache ((v + 1) * nu + u) desc.OutwardNormal
                            | 4 ->
                                let struct (ax, ay, az) = to3D u v
                                let struct (bx, by, bz) = to3D (u + 1) v
                                emitEdgeVertex ax ay az bx by bz uEdgeCache (v * (nu - 1) + u) capNormal
                            | 5 ->
                                let struct (ax, ay, az) = to3D u (v + 1)
                                let struct (bx, by, bz) = to3D (u + 1) (v + 1)
                                emitEdgeVertex ax ay az bx by bz uEdgeCache ((v + 1) * (nu - 1) + u) capNormal
                            | 6 ->
                                let struct (ax, ay, az) = to3D u v
                                let struct (bx, by, bz) = to3D u (v + 1)
                                emitEdgeVertex ax ay az bx by bz vEdgeCache (v * nu + u) capNormal
                            | _ ->
                                let struct (ax, ay, az) = to3D (u + 1) v
                                let struct (bx, by, bz) = to3D (u + 1) (v + 1)
                                emitEdgeVertex ax ay az bx by bz vEdgeCache (v * nu + u + 1) capNormal

                        let mutable v = 0

                        while v < nv - 1 && budgetError.IsNone do
                            let mutable u = 0

                            while u < nu - 1 && budgetError.IsNone do
                                if isUVValid u v && isUVValid (u + 1) (v + 1) then
                                    let struct (x00, y00, z00) = to3D u v
                                    let struct (x10, y10, z10) = to3D (u + 1) v
                                    let struct (x11, y11, z11) = to3D (u + 1) (v + 1)
                                    let struct (x01, y01, z01) = to3D u (v + 1)

                                    let s00 = values[voxelIndex x00 y00 z00]
                                    let s10 = values[voxelIndex x10 y10 z10]
                                    let s11 = values[voxelIndex x11 y11 z11]
                                    let s01 = values[voxelIndex x01 y01 z01]

                                    if
                                        Check3D.isFinite32 s00
                                        && Check3D.isFinite32 s10
                                        && Check3D.isFinite32 s11
                                        && Check3D.isFinite32 s01
                                    then
                                        let mutable cubeMask = desc.BaseMask
                                        if s00 < threshold then cubeMask <- cubeMask ||| (1 <<< desc.RealSlot0)
                                        if s10 < threshold then cubeMask <- cubeMask ||| (1 <<< desc.RealSlot1)
                                        if s11 < threshold then cubeMask <- cubeMask ||| (1 <<< desc.RealSlot2)
                                        if s01 < threshold then cubeMask <- cubeMask ||| (1 <<< desc.RealSlot3)

                                        if cubeMask <> 0 && cubeMask <> 255 then
                                            let row = cubeMask * 16
                                            let mutable k = 0

                                            while k < 15 && VolumeKernel.triTable[row + k] <> -1 && budgetError.IsNone do
                                                let a = resolveCapEdge u v VolumeKernel.triTable[row + k]
                                                let b = resolveCapEdge u v VolumeKernel.triTable[row + k + 1]
                                                let c = resolveCapEdge u v VolumeKernel.triTable[row + k + 2]
                                                addTriangle a b c
                                                k <- k + 3

                                u <- u + 1

                            v <- v + 1

                for fIdx = 0 to faceDescriptors.Length - 1 do
                    if budgetError.IsNone then
                        emitBoundaryFace faceDescriptors[fIdx]

            match budgetError with
            | ValueSome err -> Error [ err ]
            | ValueNone ->
                Ok
                    {
                        Vertices = verts.ToArray ()
                        Indices = inds.ToArray ()
                    }

    /// <summary>Extracts a superlevel-set isosurface <c>MeshData</c> (<c>value &gt;= config.Threshold</c>) without cancellation.</summary>
    let compute (scale: Scale3D) (color: ColorRamp) (config: IsoSurfaceConfig) (volume: VolumeGrid) : Result<MeshData, LayoutError list> =
        computeWithCancellation CancellationToken.None scale color config volume

/// <summary>Orthogonal cross-section plane extraction for <c>VolumeGrid</c>.</summary>
[<RequireQualifiedAccess>]
module VolumeSlices3D =
    /// <summary>
    /// Extracts up to three orthogonal colored cross-section planes at the requested <c>XIndex</c>, <c>YIndex</c>, and <c>ZIndex</c>
    /// into a single <c>MeshData</c> (intended for double-sided rendering), validating all provided slice indices against <c>[0 .. n - 1]</c>.
    /// </summary>
    let extract (scale: Scale3D) (color: ColorRamp) (slices: SliceIndices3D) (volume: VolumeGrid) : Result<MeshData, LayoutError list> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box color) then
            nullArg (nameof color)

        if isNull (box volume) then
            nullArg (nameof volume)

        let nx = volume.Nx
        let ny = volume.Ny
        let nz = volume.Nz

        let validateIndex (name: string) (limit: int) (opt: int option) : LayoutError option =
            match opt with
            | Some i when i < 0 || i >= limit ->
                Some (LayoutError.InvalidObservation $"Slice {name} ({i}) must lie within [0 .. {limit - 1}].")
            | _ -> None

        let errs =
            [
                validateIndex "XIndex" nx slices.XIndex
                validateIndex "YIndex" ny slices.YIndex
                validateIndex "ZIndex" nz slices.ZIndex
            ]
            |> List.choose id

        if not (List.isEmpty errs) then
            Error errs
        elif slices.XIndex.IsNone && slices.YIndex.IsNone && slices.ZIndex.IsNone then
            Ok { Vertices = [||]; Indices = [||] }
        else
            let mapped = VolumeKernel.prepareMappedAxes scale volume
            let upAxis = mapped.UpAxis
            let wxArr, wxValid = mapped.WxArr, mapped.WxValid
            let wyArr, wyValid = mapped.WyArr, mapped.WyValid
            let wzArr, wzValid = mapped.WzArr, mapped.WzValid

            let values = volume.Values
            let secondary = volume.SecondaryScalars
            let hasSecondary = secondary.Length > 0
            let strideY = nx
            let strideZ = nx * ny

            let verts = ResizeArray<Vertex3D>()
            let inds = ResizeArray<uint32>()

            let appendSlicePlane (axisType: int) (fixedIdx: int) (nu: int) (nv: int) =
                let inline to3D u v = VolumeKernel.planeTo3D axisType fixedIdx u v

                let inline isCoordValid u v =
                    match axisType with
                    | 0 -> wxValid[fixedIdx] && wyValid[u] && wzValid[v]
                    | 1 -> wyValid[fixedIdx] && wxValid[u] && wzValid[v]
                    | _ -> wzValid[fixedIdx] && wxValid[u] && wyValid[v]

                let inline worldAt u v =
                    let struct (ix, iy, iz) = to3D u v
                    VolumeKernel.worldPoint upAxis wxArr[ix] wyArr[iy] wzArr[iz]

                let struct (ix0, iy0, iz0) = to3D 0 0
                let struct (ixU, iyU, izU) = to3D (nu - 1) 0
                let struct (ixV, iyV, izV) = to3D 0 (nv - 1)

                let planeNormal =
                    if
                        wxValid[ix0] && wyValid[iy0] && wzValid[iz0]
                        && wxValid[ixU] && wyValid[iyU] && wzValid[izU]
                        && wxValid[ixV] && wyValid[iyV] && wzValid[izV]
                    then
                        let p00 = VolumeKernel.worldPoint upAxis wxArr[ix0] wyArr[iy0] wzArr[iz0]
                        let pU = VolumeKernel.worldPoint upAxis wxArr[ixU] wyArr[iyU] wzArr[izU]
                        let pV = VolumeKernel.worldPoint upAxis wxArr[ixV] wyArr[iyV] wzArr[izV]
                        VolumeKernel.safeNormalizeCross (pU - p00) (pV - p00)
                    else
                        Vector3.UnitY

                let baseVert = verts.Count
                let valid = Array.zeroCreate<bool> (nu * nv)

                for v = 0 to nv - 1 do
                    let vOffset = v * nu

                    for u = 0 to nu - 1 do
                        let localIdx = vOffset + u

                        if isCoordValid u v then
                            let struct (ix, iy, iz) = to3D u v
                            let gridIdx = ix + strideY * iy + strideZ * iz
                            let scalar = if hasSecondary then float secondary[gridIdx] else float values[gridIdx]

                            match ColorRamp.tryMap color scalar with
                            | ValueSome col ->
                                valid[localIdx] <- true
                                verts.Add (Vertex3D (worldAt u v, planeNormal, col.ToVector4 ()))
                            | ValueNone ->
                                verts.Add (Vertex3D (Vector3.Zero, planeNormal, Vector4.Zero))
                        else
                            verts.Add (Vertex3D (Vector3.Zero, planeNormal, Vector4.Zero))

                for v = 0 to nv - 2 do
                    let r0 = v * nu
                    let r1 = r0 + nu

                    for u = 0 to nu - 2 do
                        let l00 = r0 + u
                        let l10 = l00 + 1
                        let l01 = r1 + u
                        let l11 = l01 + 1

                        if valid[l00] && valid[l10] && valid[l01] && valid[l11] then
                            let v00 = uint32 (baseVert + l00)
                            let v10 = uint32 (baseVert + l10)
                            let v11 = uint32 (baseVert + l11)
                            let v01 = uint32 (baseVert + l01)
                            inds.Add v00
                            inds.Add v10
                            inds.Add v11
                            inds.Add v00
                            inds.Add v11
                            inds.Add v01

            match slices.XIndex with
            | Some ix -> appendSlicePlane 0 ix ny nz
            | None -> ()

            match slices.YIndex with
            | Some iy -> appendSlicePlane 1 iy nx nz
            | None -> ()

            match slices.ZIndex with
            | Some iz -> appendSlicePlane 2 iz nx ny
            | None -> ()

            Ok
                {
                    Vertices = verts.ToArray ()
                    Indices = inds.ToArray ()
                }

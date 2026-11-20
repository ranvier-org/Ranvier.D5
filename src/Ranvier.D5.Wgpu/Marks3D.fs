namespace Ranvier.D5.Wgpu

open System
open System.Collections.Generic
open System.Numerics
#if FABLE_COMPILER
open Ranvier.D5.Wgpu.FableCompat3D
#endif
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Input datum describing a camera-facing SDF scatter disc with optional concentric progress rings and hollow portal.</summary>
[<Struct>]
type ScatterDatum3D =
    {
        /// <summary>3D position in data coordinates.</summary>
        Position: Point3
        /// <summary>Screen-space disc radius in logical pixels (clamped at least <c>0.5f</c>).</summary>
        RadiusPx: float32
        /// <summary>Base RGBA fill color; alpha below <c>0.999f</c> triggers back-to-front sorting when a camera position is supplied.</summary>
        Color: RgbaColor
        /// <summary>Primary concentric progress ring fraction in <c>[0.0, 1.0]</c>.</summary>
        RingPrimary: float32
        /// <summary>Secondary concentric progress ring fraction in <c>[0.0, 1.0]</c>.</summary>
        RingSecondary: float32
        /// <summary>Concentric progress ring thickness in pixels (<c>0.0f</c> disables the ring).</summary>
        RingThicknessPx: float32
        /// <summary>Hollow portal interior blend factor in <c>[0.0, 1.0]</c>.</summary>
        PortalHollow: float32
        /// <summary>Hardware picking identifier (<c>0u</c> disables picking).</summary>
        PickId: uint32
        /// <summary>When true, renders the selection halo ring.</summary>
        Selected: bool
    }

/// <summary>Constructors and pipeable modifiers for <c>ScatterDatum3D</c>.</summary>
[<RequireQualifiedAccess>]
module ScatterDatum3D =
    /// <summary>Creates a solid unringed scatter disc datum with <c>PickId = 0u</c> and <c>Selected = false</c>.</summary>
    let create (position: Point3) (radiusPx: float32) (color: RgbaColor) : ScatterDatum3D =
        {
            Position = position
            RadiusPx = radiusPx
            Color = color
            RingPrimary = 0.0f
            RingSecondary = 0.0f
            RingThicknessPx = 0.0f
            PortalHollow = 0.0f
            PickId = 0u
            Selected = false
        }

    /// <summary>Configures concentric primary/secondary progress ring fractions and pixel thickness on a scatter datum.</summary>
    let withRings (primary: float32) (secondary: float32) (thicknessPx: float32) (datum: ScatterDatum3D) : ScatterDatum3D =
        { datum with
            RingPrimary = primary
            RingSecondary = secondary
            RingThicknessPx = thicknessPx
        }

    /// <summary>Configures the hollow-portal interior blend factor in <c>[0.0, 1.0]</c> on a scatter datum.</summary>
    let withHollow (hollow: float32) (datum: ScatterDatum3D) : ScatterDatum3D =
        { datum with PortalHollow = hollow }

/// <summary>Standard unit-scale 3D glyph geometry primitives.</summary>
[<RequireQualifiedAccess; Struct>]
type GlyphShape3D =
    /// <summary>Smooth unit-radius icosphere.</summary>
    | Sphere
    /// <summary>Unit box centered at origin with extent <c>[-0.5, +0.5]^3</c>.</summary>
    | Box
    /// <summary>Capped unit cylinder along <c>+Y</c> with radius <c>0.5f</c> and height <c>1.0f</c>.</summary>
    | Cylinder
    /// <summary>Capped unit cone along <c>+Y</c> with base radius <c>0.5f</c> at <c>-0.5f</c> and apex at <c>+0.5f</c>.</summary>
    | Cone
    /// <summary>Unit torus in the XZ plane with major radius <c>0.5f</c>.</summary>
    | Torus

/// <summary>Input datum describing an oriented, non-uniformly scaled 3D glyph instance.</summary>
[<Struct>]
type GlyphDatum3D =
    {
        /// <summary>Glyph anchor position in data coordinates.</summary>
        Position: Point3
        /// <summary>Local non-uniform world scale applied before rotation.</summary>
        Scale: Vector3
        /// <summary>World-space rotation quaternion.</summary>
        Rotation: Quaternion
        /// <summary>Per-instance RGBA albedo color and opacity.</summary>
        Color: RgbaColor
        /// <summary>Unlit or directional studio lighting mode.</summary>
        Shading: ShadingMode
        /// <summary>Hardware picking identifier (<c>0u</c> disables picking).</summary>
        PickId: uint32
        /// <summary>When true, renders selection rim highlighting.</summary>
        Selected: bool
    }

/// <summary>Constructors and pipeable modifiers for <c>GlyphDatum3D</c>.</summary>
[<RequireQualifiedAccess>]
module GlyphDatum3D =
    /// <summary>Creates a studio-lit <c>GlyphDatum3D</c> with identity rotation and picking disabled.</summary>
    let create (position: Point3) (scale: Vector3) (color: RgbaColor) : GlyphDatum3D =
        {
            Position = position
            Scale = scale
            Rotation = Quaternion.Identity
            Color = color
            Shading = ShadingMode.Lit (0.45f, 0.0f)
            PickId = 0u
            Selected = false
        }

    /// <summary>Creates a uniformly scaled spherical glyph datum.</summary>
    let sphere (position: Point3) (radius: float32) (color: RgbaColor) : GlyphDatum3D =
        create position (Vector3 (radius, radius, radius)) color

    /// <summary>Creates a box glyph datum with explicit world dimensions along X, Y, and Z.</summary>
    let box (position: Point3) (size: Vector3) (color: RgbaColor) : GlyphDatum3D =
        create position size color

    /// <summary>Replaces the glyph's non-uniform local scale vector.</summary>
    let withScale (scale: Vector3) (datum: GlyphDatum3D) : GlyphDatum3D =
        { datum with Scale = scale }

    /// <summary>Sets equal local scale across all three axes.</summary>
    let withUniformScale (scale: float32) (datum: GlyphDatum3D) : GlyphDatum3D =
        { datum with Scale = Vector3 (scale, scale, scale) }

    /// <summary>Sets the world-space rotation quaternion.</summary>
    let withRotation (rotation: Quaternion) (datum: GlyphDatum3D) : GlyphDatum3D =
        { datum with Rotation = rotation }

    /// <summary>Sets the glyph <c>ShadingMode</c>.</summary>
    let withShading (shading: ShadingMode) (datum: GlyphDatum3D) : GlyphDatum3D =
        { datum with Shading = shading }

    /// <summary>Sets the hardware picking identifier.</summary>
    let withPickId (pickId: uint32) (datum: GlyphDatum3D) : GlyphDatum3D =
        { datum with PickId = pickId }

    /// <summary>Sets the selection rim-highlight flag.</summary>
    let withSelected (selected: bool) (datum: GlyphDatum3D) : GlyphDatum3D =
        { datum with Selected = selected }

/// <summary>Input datum describing an axis-aligned 3D bar/column anchored from <c>Z0</c> to <c>Z1</c>.</summary>
[<Struct>]
type BarDatum3D =
    {
        /// <summary>Center data X coordinate.</summary>
        X: float
        /// <summary>Center data Y coordinate.</summary>
        Y: float
        /// <summary>Baseline data Z coordinate (commonly <c>0.0</c>).</summary>
        Z0: float
        /// <summary>Top/end data Z coordinate (positive or negative relative to <c>Z0</c>).</summary>
        Z1: float
        /// <summary>Bar footprint width along the data X axis.</summary>
        WidthX: float
        /// <summary>Bar footprint width along the data Y axis.</summary>
        WidthY: float
        /// <summary>Per-instance RGBA albedo color and opacity.</summary>
        Color: RgbaColor
        /// <summary>Unlit or directional studio lighting mode.</summary>
        Shading: ShadingMode
        /// <summary>Hardware picking identifier (<c>0u</c> disables picking).</summary>
        PickId: uint32
        /// <summary>When true, renders selection rim highlighting.</summary>
        Selected: bool
    }

/// <summary>Constructors and pipeable modifiers for <c>BarDatum3D</c>.</summary>
[<RequireQualifiedAccess>]
module BarDatum3D =
    /// <summary>Creates a studio-lit <c>BarDatum3D</c> anchored between <c>z0</c> and <c>z1</c> with picking disabled.</summary>
    let create (x: float) (y: float) (z0: float) (z1: float) (widthX: float) (widthY: float) (color: RgbaColor) : BarDatum3D =
        {
            X = x
            Y = y
            Z0 = z0
            Z1 = z1
            WidthX = widthX
            WidthY = widthY
            Color = color
            Shading = ShadingMode.Lit (0.45f, 0.0f)
            PickId = 0u
            Selected = false
        }

    /// <summary>Sets the bar <c>ShadingMode</c>.</summary>
    let withShading (shading: ShadingMode) (datum: BarDatum3D) : BarDatum3D =
        { datum with Shading = shading }

    /// <summary>Sets the hardware picking identifier.</summary>
    let withPickId (pickId: uint32) (datum: BarDatum3D) : BarDatum3D =
        { datum with PickId = pickId }

    /// <summary>Sets the selection rim-highlight flag.</summary>
    let withSelected (selected: bool) (datum: BarDatum3D) : BarDatum3D =
        { datum with Selected = selected }

/// <summary>Input datum describing a 3D vector field arrow anchored at <c>Origin</c> pointing along <c>Vector</c>.</summary>
[<Struct>]
type ArrowDatum3D =
    {
        /// <summary>Tail position in data coordinates.</summary>
        Origin: Point3
        /// <summary>Displacement vector in data coordinates relative to <c>Origin</c>.</summary>
        Vector: Point3
        /// <summary>World-space cylindrical shaft radius.</summary>
        ShaftRadius: float32
        /// <summary>Multiplier applied to the mapped world-space vector length.</summary>
        LengthScale: float32
        /// <summary>Per-instance RGBA albedo color and opacity.</summary>
        Color: RgbaColor
        /// <summary>Unlit or directional studio lighting mode.</summary>
        Shading: ShadingMode
        /// <summary>Hardware picking identifier (<c>0u</c> disables picking).</summary>
        PickId: uint32
        /// <summary>When true, renders selection rim highlighting.</summary>
        Selected: bool
    }

/// <summary>Constructors and pipeable modifiers for <c>ArrowDatum3D</c>.</summary>
[<RequireQualifiedAccess>]
module ArrowDatum3D =
    /// <summary>Creates a studio-lit <c>ArrowDatum3D</c> with <c>LengthScale = 1.0f</c> and picking disabled.</summary>
    let create (origin: Point3) (vector: Point3) (shaftRadius: float32) (color: RgbaColor) : ArrowDatum3D =
        {
            Origin = origin
            Vector = vector
            ShaftRadius = shaftRadius
            LengthScale = 1.0f
            Color = color
            Shading = ShadingMode.Lit (0.45f, 0.0f)
            PickId = 0u
            Selected = false
        }

    /// <summary>Creates an <c>ArrowDatum3D</c> converting a <c>System.Numerics.Vector3</c> displacement vector into data-space <c>Point3</c>.</summary>
    let ofVector3 (origin: Point3) (vector: Vector3) (shaftRadius: float32) (color: RgbaColor) : ArrowDatum3D =
        create origin (Point3.create (float vector.X) (float vector.Y) (float vector.Z)) shaftRadius color

    /// <summary>Sets the arrow <c>ShadingMode</c>.</summary>
    let withShading (shading: ShadingMode) (datum: ArrowDatum3D) : ArrowDatum3D =
        { datum with Shading = shading }

    /// <summary>Sets the hardware picking identifier.</summary>
    let withPickId (pickId: uint32) (datum: ArrowDatum3D) : ArrowDatum3D =
        { datum with PickId = pickId }

    /// <summary>Sets the selection rim-highlight flag.</summary>
    let withSelected (selected: bool) (datum: ArrowDatum3D) : ArrowDatum3D =
        { datum with Selected = selected }

/// <summary>Input datum describing an axis-aligned 3D cuboid spanning a data-space <c>Bounds3</c> box (preserving Y/Z orientation directly under both <c>YUp</c> and <c>ZUp</c>).</summary>
[<Struct>]
type BoxDatum3D =
    {
        /// <summary>Data-space bounding cuboid.</summary>
        Bounds: Bounds3
        /// <summary>Per-instance RGBA albedo color and opacity.</summary>
        Color: RgbaColor
        /// <summary>Unlit or directional studio lighting mode.</summary>
        Shading: ShadingMode
        /// <summary>Hardware picking identifier (<c>0u</c> disables picking).</summary>
        PickId: uint32
        /// <summary>When true, renders selection rim highlighting.</summary>
        Selected: bool
    }

/// <summary>Constructors and pipeable modifiers for <c>BoxDatum3D</c>.</summary>
[<RequireQualifiedAccess>]
module BoxDatum3D =
    /// <summary>Creates a studio-lit <c>BoxDatum3D</c> over <c>bounds</c> with picking disabled.</summary>
    let create (bounds: Bounds3) (color: RgbaColor) : BoxDatum3D =
        {
            Bounds = bounds
            Color = color
            Shading = ShadingMode.Lit (0.45f, 0.0f)
            PickId = 0u
            Selected = false
        }

    /// <summary>Sets the cuboid <c>ShadingMode</c>.</summary>
    let withShading (shading: ShadingMode) (datum: BoxDatum3D) : BoxDatum3D =
        { datum with Shading = shading }

    /// <summary>Sets the hardware picking identifier.</summary>
    let withPickId (pickId: uint32) (datum: BoxDatum3D) : BoxDatum3D =
        { datum with PickId = pickId }

    /// <summary>Sets the selection rim-highlight flag.</summary>
    let withSelected (selected: bool) (datum: BoxDatum3D) : BoxDatum3D =
        { datum with Selected = selected }

module internal MarksKernel =
    let inline spanSlice (arr: 'T array) (start: int) (count: int) : ReadOnlySpan<'T> =
        if count <= 0 then
            ReadOnlySpan<'T>.Empty
        elif start = 0 && count = arr.Length then
            ReadOnlySpan<'T>(arr)
        else
#if FABLE_COMPILER
            ReadOnlySpan<'T>(Array.sub arr start count)
#else
            ReadOnlySpan<'T>(arr, start, count)
#endif

    let inline isFiniteColor (c: RgbaColor) =
        Check3D.isFinite32 c.R
        && Check3D.isFinite32 c.G
        && Check3D.isFinite32 c.B
        && Check3D.isFinite32 c.A

    let inline isFiniteVec3 (v: Vector3) =
        Check3D.isFinite32 v.X && Check3D.isFinite32 v.Y && Check3D.isFinite32 v.Z

    let inline isFinitePoint3 (p: Point3) =
        Check3D.isFinite p.X && Check3D.isFinite p.Y && Check3D.isFinite p.Z

    let inline shadingParams (shading: ShadingMode) (selected: bool) : struct (bool * float32 * float32 * uint32) =
        let selFlag = if selected then InstanceFlags.Selected else InstanceFlags.None

        match shading with
        | ShadingMode.Unlit -> struct (true, 0.5f, 0.0f, InstanceFlags.Unlit ||| selFlag)
        | ShadingMode.ExactUnlit -> struct (true, 0.5f, 0.0f, InstanceFlags.Unlit ||| InstanceFlags.ExactColor ||| selFlag)
        | ShadingMode.Lit (roughness, emissive) ->
            if
                Check3D.isFinite32 roughness
                && Check3D.isFinite32 emissive
                && roughness >= 0.0f
                && emissive >= 0.0f
            then
                struct (true, roughness, emissive, selFlag)
            else
                struct (false, 0.0f, 0.0f, 0u)

    let inline trimMemory (arr: 'T array) (count: int) : ReadOnlyMemory<'T> =
        if count <= 0 then
            ReadOnlyMemory<'T>.Empty
        elif count = arr.Length then
            ReadOnlyMemory<'T>(arr)
        else
            ReadOnlyMemory<'T>(Array.sub arr 0 count)

    let inline distSqTo (p: Vector4) (cam: Vector3) =
        let dx = p.X - cam.X
        let dy = p.Y - cam.Y
        let dz = p.Z - cam.Z
        dx * dx + dy * dy + dz * dz

    let inline normalizeOrIdentity (q: Quaternion) : struct (bool * Quaternion) =
        if
            Check3D.isFinite32 q.X
            && Check3D.isFinite32 q.Y
            && Check3D.isFinite32 q.Z
            && Check3D.isFinite32 q.W
        then
            let lenSq = q.LengthSquared ()
            if Check3D.isFinite32 lenSq && lenSq > 1e-12f then struct (true, Quaternion.Normalize q) else struct (true, Quaternion.Identity)
        else
            struct (false, Quaternion.Identity)

    let fallbackPerp (t: Vector3) : Vector3 =
        let refAxis = if MathF.Abs t.Y < 0.9f then Vector3.UnitY else Vector3.UnitX
        let cross = Vector3.Cross (refAxis, t)
        if cross.LengthSquared () > 1e-12f then Vector3.Normalize cross else Vector3.UnitX

    let rotateMinimal (u: Vector3) (v: Vector3) (vec: Vector3) : Vector3 =
        let d = Vector3.Dot (u, v)

        let candidate =
            if d <= -1.0f + 1e-6f then
                vec
            else
                let qxyz = Vector3.Cross (u, v)
                let qw = 1.0f + d
                let lenSq = qxyz.LengthSquared () + qw * qw

                if lenSq <= 1e-12f then
                    vec
                else
                    let inv = 1.0f / MathF.Sqrt lenSq
                    let rx = qxyz.X * inv
                    let ry = qxyz.Y * inv
                    let rz = qxyz.Z * inv
                    let rw = qw * inv
                    let tx = 2.0f * (ry * vec.Z - rz * vec.Y)
                    let ty = 2.0f * (rz * vec.X - rx * vec.Z)
                    let tz = 2.0f * (rx * vec.Y - ry * vec.X)
                    let ccx = ry * tz - rz * ty
                    let ccy = rz * tx - rx * tz
                    let ccz = rx * ty - ry * tx
                    Vector3 (vec.X + rw * tx + ccx, vec.Y + rw * ty + ccy, vec.Z + rw * tz + ccz)

        let ortho = candidate - v * Vector3.Dot (candidate, v)

        if ortho.LengthSquared () > 1e-12f then
            Vector3.Normalize ortho
        else
            fallbackPerp v

    let tryPackDisc (scale: Scale3D) (pickId: uint32) (d: ScatterDatum3D) (out: DiscInstance3D array) (slot: int) : bool =
        if
            isFinitePoint3 d.Position
            && Check3D.isFinite32 d.RadiusPx
            && isFiniteColor d.Color
            && Check3D.isFinite32 d.RingPrimary
            && Check3D.isFinite32 d.RingSecondary
            && Check3D.isFinite32 d.RingThicknessPx
            && Check3D.isFinite32 d.PortalHollow
        then
            match Scale3D.tryMap scale d.Position with
            | ValueSome worldPos ->
                let flags = if d.Selected then InstanceFlags.Selected else InstanceFlags.None

                out[slot] <-
                    DiscInstance3D.create
                        worldPos
                        d.RadiusPx
                        d.Color
                        d.RingPrimary
                        d.RingSecondary
                        d.RingThicknessPx
                        d.PortalHollow
                        pickId
                        flags

                true
            | ValueNone -> false
        else
            false

    let tryPackGlyph (scale: Scale3D) (pickId: uint32) (d: GlyphDatum3D) (out: Instance3D array) (slot: int) : bool =
        let struct (qOk, unitQ) = normalizeOrIdentity d.Rotation
        let struct (shadeOk, roughness, emissive, flags) = shadingParams d.Shading d.Selected

        if
            qOk
            && shadeOk
            && isFinitePoint3 d.Position
            && isFiniteVec3 d.Scale
            && isFiniteColor d.Color
        then
            match Scale3D.tryMap scale d.Position with
            | ValueSome worldPos ->
                let worldMat =
                    Matrix4x4.CreateScale d.Scale
                    * Orientation3D.matrixFromQuaternion unitQ
                    * Matrix4x4.CreateTranslation worldPos

                out[slot] <- Instance3D.fromMatrix worldMat d.Color roughness pickId flags emissive
                true
            | ValueNone -> false
        else
            false

    let tryPackBox (scale: Scale3D) (pickId: uint32) (d: BoxDatum3D) (out: Instance3D array) (slot: int) : bool =
        let struct (shadeOk, roughness, emissive, flags) = shadingParams d.Shading d.Selected

        if shadeOk && isFiniteColor d.Color then
            let minPt = Bounds3.minPoint d.Bounds
            let maxPt = Bounds3.maxPoint d.Bounds
            let sx = Bounds3.sizeX d.Bounds
            let sy = Bounds3.sizeY d.Bounds
            let sz = Bounds3.sizeZ d.Bounds

            if isFinitePoint3 minPt && isFinitePoint3 maxPt && Check3D.isFinite sx && Check3D.isFinite sy && Check3D.isFinite sz then
                match Scale3D.tryMap scale minPt with
                | ValueSome wMin ->
                    match Scale3D.tryMap scale maxPt with
                    | ValueSome wMax ->
                        let center = (wMin + wMax) * 0.5f
                        let ex = MathF.Abs (wMax.X - wMin.X)
                        let ey = MathF.Abs (wMax.Y - wMin.Y)
                        let ez = MathF.Abs (wMax.Z - wMin.Z)

                        if isFiniteVec3 center && Check3D.isFinite32 ex && Check3D.isFinite32 ey && Check3D.isFinite32 ez then
                            let worldMat = Matrix4x4.CreateScale (ex, ey, ez) * Matrix4x4.CreateTranslation center
                            out[slot] <- Instance3D.fromMatrix worldMat d.Color roughness pickId flags emissive
                            true
                        else
                            false
                    | ValueNone -> false
                | ValueNone -> false
            else
                false
        else
            false

    let private packBarMapped
        (dx: float)
        (dy: float)
        (dz: float)
        (upAxis: UpAxis)
        (tx: float)
        (ty: float)
        (tz0: float)
        (tz1: float)
        (txLo: float)
        (txHi: float)
        (tyLo: float)
        (tyHi: float)
        (color: RgbaColor)
        (roughness: float32)
        (emissive: float32)
        (pickId: uint32)
        (flags: uint32)
        (out: Instance3D array)
        (slot: int)
        : bool =
        let wx = float32 ((tx - 0.5) * dx)
        let wy = float32 ((ty - 0.5) * dy)
        let wz0 = float32 ((tz0 - 0.5) * dz)
        let wz1 = float32 ((tz1 - 0.5) * dz)
        let wxSize = float32 (abs (txHi - txLo) * dx)
        let wySize = float32 (abs (tyHi - tyLo) * dy)

        if
            Check3D.isFinite32 wx
            && Check3D.isFinite32 wy
            && Check3D.isFinite32 wz0
            && Check3D.isFinite32 wz1
            && Check3D.isFinite32 wxSize
            && Check3D.isFinite32 wySize
        then
            let w0 =
                match upAxis with
                | UpAxis.ZUp -> Vector3 (wx, wz0, -wy)
                | UpAxis.YUp -> Vector3 (wx, wy, wz0)

            let w1 =
                match upAxis with
                | UpAxis.ZUp -> Vector3 (wx, wz1, -wy)
                | UpAxis.YUp -> Vector3 (wx, wy, wz1)

            let wzSize =
                match upAxis with
                | UpAxis.ZUp -> abs (w1.Y - w0.Y)
                | UpAxis.YUp -> abs (w1.Z - w0.Z)

            if Check3D.isFinite32 wzSize then
                let center = w0 * 0.5f + w1 * 0.5f

                let boxScale =
                    match upAxis with
                    | UpAxis.ZUp -> Vector3 (wxSize, wzSize, wySize)
                    | UpAxis.YUp -> Vector3 (wxSize, wySize, wzSize)

                let worldMat =
                    Matrix4x4.CreateScale boxScale
                    * Matrix4x4.CreateTranslation center

                out[slot] <- Instance3D.fromMatrix worldMat color roughness pickId flags emissive
                true
            else
                false
        else
            false

    let tryPackBar
        (scale: Scale3D)
        (pickId: uint32)
        (d: BarDatum3D)
        (out: Instance3D array)
        (slot: int)
        : bool =
        let struct (shadeOk, roughness, emissive, flags) = shadingParams d.Shading d.Selected

        if
            shadeOk
            && Check3D.isFinite d.X
            && Check3D.isFinite d.Y
            && Check3D.isFinite d.Z0
            && Check3D.isFinite d.Z1
            && Check3D.isFinite d.WidthX
            && Check3D.isFinite d.WidthY
            && d.WidthX > 0.0
            && d.WidthY > 0.0
            && isFiniteColor d.Color
        then
            let struct (dx, dy, dz) = scale.DataExtents
            let mx = scale.MapperX
            let my = scale.MapperY
            let mz = scale.MapperZ
            let tx = AxisCoordMapper.evalNormalized mx d.X
            let ty = AxisCoordMapper.evalNormalized my d.Y
            let tz0 = AxisCoordMapper.evalNormalized mz d.Z0
            let tz1 = AxisCoordMapper.evalNormalized mz d.Z1
            let txLo = AxisCoordMapper.evalNormalized mx (d.X - d.WidthX * 0.5)
            let txHi = AxisCoordMapper.evalNormalized mx (d.X + d.WidthX * 0.5)
            let tyLo = AxisCoordMapper.evalNormalized my (d.Y - d.WidthY * 0.5)
            let tyHi = AxisCoordMapper.evalNormalized my (d.Y + d.WidthY * 0.5)

            if
                Check3D.isFinite tx
                && Check3D.isFinite ty
                && Check3D.isFinite tz0
                && Check3D.isFinite tz1
                && Check3D.isFinite txLo
                && Check3D.isFinite txHi
                && Check3D.isFinite tyLo
                && Check3D.isFinite tyHi
            then
                packBarMapped dx dy dz scale.UpAxis tx ty tz0 tz1 txLo txHi tyLo tyHi d.Color roughness emissive pickId flags out slot
            else
                false
        else
            false

    let tryPackSegmentWithSection
        (ignoreSectionPlane: bool)
        (scale: Scale3D)
        (pair: struct (Point3 * Point3 * RgbaColor))
        (out: LineVertex3D array)
        (vertexSlot: int)
        : bool =
        let struct (pA, pB, col) = pair

        if isFinitePoint3 pA && isFinitePoint3 pB && isFiniteColor col then
            match Scale3D.tryMap scale pA with
            | ValueSome wA ->
                match Scale3D.tryMap scale pB with
                | ValueSome wB ->
                    out[vertexSlot] <- LineVertex3D (wA, col, ignoreSectionPlane)
                    out[vertexSlot + 1] <- LineVertex3D (wB, col, ignoreSectionPlane)
                    true
                | ValueNone -> false
            | ValueNone -> false
        else
            false

    let tryPackSegment (scale: Scale3D) (pair: struct (Point3 * Point3 * RgbaColor)) (out: LineVertex3D array) (vertexSlot: int) : bool =
        tryPackSegmentWithSection false scale pair out vertexSlot

    let tryPackArrow (scale: Scale3D) (pickId: uint32) (d: ArrowDatum3D) (out: Instance3D array) (slot: int) : bool =
        let struct (shadeOk, roughness, emissive, flags) = shadingParams d.Shading d.Selected

        if
            shadeOk
            && isFinitePoint3 d.Origin
            && isFinitePoint3 d.Vector
            && Check3D.isFinite32 d.ShaftRadius
            && Check3D.isFinite32 d.LengthScale
            && d.ShaftRadius > 0.0f
            && d.LengthScale > 0.0f
            && isFiniteColor d.Color
        then
            let tipX = d.Origin.X + d.Vector.X
            let tipY = d.Origin.Y + d.Vector.Y
            let tipZ = d.Origin.Z + d.Vector.Z

            if Check3D.isFinite tipX && Check3D.isFinite tipY && Check3D.isFinite tipZ then
                let tipPt = Point3.create tipX tipY tipZ

                match Scale3D.tryMap scale d.Origin with
                | ValueSome worldOrigin ->
                    match Scale3D.tryMap scale tipPt with
                    | ValueSome worldTip ->
                        let disp = (worldTip - worldOrigin) * d.LengthScale
                        let lenSq = disp.LengthSquared ()

                        if Check3D.isFinite32 lenSq && lenSq >= 1e-12f then
                            let arrowLen = MathF.Sqrt lenSq
                            let worldMat = Orientation3D.anisotropicMatrix worldOrigin disp d.ShaftRadius arrowLen
                            out[slot] <- Instance3D.fromMatrix worldMat d.Color roughness pickId flags emissive
                            true
                        else
                            false
                    | ValueNone -> false
                | ValueNone -> false
            else
                false
        else
            false

/// <summary>Billboard scatter point packer emitting <c>DiscInstance3D</c> slices with optional translucent depth sorting.</summary>
[<RequireQualifiedAccess>]
module Scatter3D =
    /// <summary>
    /// Maps scatter datums through <c>scale</c> into <c>DiscInstance3D</c> GPU instances, omitting unmappable or non-finite entries.
    /// When <c>cameraPos</c> is <c>ValueSome</c> and any emitted disc has <c>Color.A &lt; 0.999f</c>, sorts back-to-front by squared distance to the camera.
    /// </summary>
    let pack (scale: Scale3D) (cameraPos: Vector3 voption) (items: ReadOnlySpan<ScatterDatum3D>) : ReadOnlyMemory<DiscInstance3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        match cameraPos with
        | ValueSome cam -> Check3D.vector3 (nameof cameraPos) cam
        | ValueNone -> ()

        if items.Length = 0 then
            ReadOnlyMemory<DiscInstance3D>.Empty
        else
            let out = Array.zeroCreate<DiscInstance3D> items.Length
            let mutable count = 0
            let mutable hasTranslucent = false

            for i = 0 to items.Length - 1 do
                let d = items[i]

                if MarksKernel.tryPackDisc scale d.PickId d out count then
                    if d.Color.A < 0.999f then
                        hasTranslucent <- true

                    count <- count + 1

            if count = 0 then
                ReadOnlyMemory<DiscInstance3D>.Empty
            else
                let trimmed =
                    if count = out.Length then out else Array.sub out 0 count

                match cameraPos with
                | ValueSome cam when hasTranslucent && trimmed.Length > 1 ->
                    Array.sortInPlaceWith
                        (fun (a: DiscInstance3D) (b: DiscInstance3D) ->
                            let da = MarksKernel.distSqTo a.CenterRadiusPx cam
                            let db = MarksKernel.distSqTo b.CenterRadiusPx cam
                            if da > db then -1
                            elif da < db then 1
                            else 0)
                        trimmed
                | _ -> ()

                ReadOnlyMemory<DiscInstance3D>(trimmed)

/// <summary>Unit mesh constructors and hardware-instanced packers for 3D glyphs, anchored bars, and vector arrows.</summary>
[<RequireQualifiedAccess>]
module Glyph3D =
    /// <summary>Returns the canonical procedural <c>MeshData</c> for a <c>GlyphShape3D</c>.</summary>
    let unitMesh (shape: GlyphShape3D) : MeshData =
        match shape with
        | GlyphShape3D.Sphere -> Primitives.unitIcosphere 2
        | GlyphShape3D.Box -> Primitives.unitBox ()
        | GlyphShape3D.Cylinder -> Primitives.unitCylinder 20
        | GlyphShape3D.Cone -> Primitives.unitCone 20
        | GlyphShape3D.Torus -> Primitives.unitTorus 24 12 0.3f

    let private unitCylinderMesh (segments: int) : MeshData =
        let verts = ResizeArray<Vertex3D>(segments * 4 + 2)
        let inds = ResizeArray<uint32>(segments * 12)
        let twoPi = MathF.PI * 2.0f

        for i = 0 to segments - 1 do
            let a = float32 i / float32 segments * twoPi
            let nx = MathF.Cos a
            let nz = MathF.Sin a
            let n = Vector3 (nx, 0.0f, nz)
            verts.Add (Vertex3D (Vector3 (nx * 0.5f, -0.5f, nz * 0.5f), n))
            verts.Add (Vertex3D (Vector3 (nx * 0.5f, 0.5f, nz * 0.5f), n))

        for i = 0 to segments - 1 do
            let next = (i + 1) % segments
            let b0 = uint32 (i * 2)
            let t0 = b0 + 1u
            let b1 = uint32 (next * 2)
            let t1 = b1 + 1u
            inds.Add b0
            inds.Add t1
            inds.Add b1
            inds.Add b0
            inds.Add t0
            inds.Add t1

        let topCenter = uint32 verts.Count
        verts.Add (Vertex3D (Vector3 (0.0f, 0.5f, 0.0f), Vector3.UnitY))

        for i = 0 to segments - 1 do
            let a = float32 i / float32 segments * twoPi
            verts.Add (Vertex3D (Vector3 (MathF.Cos a * 0.5f, 0.5f, MathF.Sin a * 0.5f), Vector3.UnitY))

        for i = 0 to segments - 1 do
            let c0 = topCenter + 1u + uint32 i
            let c1 = topCenter + 1u + uint32 ((i + 1) % segments)
            inds.Add topCenter
            inds.Add c1
            inds.Add c0

        let botCenter = uint32 verts.Count
        verts.Add (Vertex3D (Vector3 (0.0f, -0.5f, 0.0f), -Vector3.UnitY))

        for i = 0 to segments - 1 do
            let a = float32 i / float32 segments * twoPi
            verts.Add (Vertex3D (Vector3 (MathF.Cos a * 0.5f, -0.5f, MathF.Sin a * 0.5f), -Vector3.UnitY))

        for i = 0 to segments - 1 do
            let c0 = botCenter + 1u + uint32 i
            let c1 = botCenter + 1u + uint32 ((i + 1) % segments)
            inds.Add botCenter
            inds.Add c0
            inds.Add c1

        {
            Vertices = verts.ToArray ()
            Indices = inds.ToArray ()
        }

    let private unitConeMesh (segments: int) : MeshData =
        let verts = ResizeArray<Vertex3D>(segments * 4 + 1)
        let inds = ResizeArray<uint32>(segments * 6)
        let twoPi = MathF.PI * 2.0f
        let slopeY = 0.5f / MathF.Sqrt 1.25f
        let slopeR = 1.0f / MathF.Sqrt 1.25f

        for i = 0 to segments - 1 do
            let a0 = float32 i / float32 segments * twoPi
            let a1 = float32 (i + 1) / float32 segments * twoPi
            let am = (a0 + a1) * 0.5f
            let p0 = Vector3 (MathF.Cos a0 * 0.5f, -0.5f, MathF.Sin a0 * 0.5f)
            let p1 = Vector3 (MathF.Cos a1 * 0.5f, -0.5f, MathF.Sin a1 * 0.5f)
            let apex = Vector3 (0.0f, 0.5f, 0.0f)
            let n0 = Vector3 (MathF.Cos a0 * slopeR, slopeY, MathF.Sin a0 * slopeR)
            let n1 = Vector3 (MathF.Cos a1 * slopeR, slopeY, MathF.Sin a1 * slopeR)
            let nm = Vector3 (MathF.Cos am * slopeR, slopeY, MathF.Sin am * slopeR)
            let b = uint32 verts.Count
            verts.Add (Vertex3D (p0, n0))
            verts.Add (Vertex3D (apex, nm))
            verts.Add (Vertex3D (p1, n1))
            inds.Add b
            inds.Add (b + 1u)
            inds.Add (b + 2u)

        let botCenter = uint32 verts.Count
        verts.Add (Vertex3D (Vector3 (0.0f, -0.5f, 0.0f), -Vector3.UnitY))

        for i = 0 to segments - 1 do
            let a = float32 i / float32 segments * twoPi
            verts.Add (Vertex3D (Vector3 (MathF.Cos a * 0.5f, -0.5f, MathF.Sin a * 0.5f), -Vector3.UnitY))

        for i = 0 to segments - 1 do
            let c0 = botCenter + 1u + uint32 i
            let c1 = botCenter + 1u + uint32 ((i + 1) % segments)
            inds.Add botCenter
            inds.Add c0
            inds.Add c1

        {
            Vertices = verts.ToArray ()
            Indices = inds.ToArray ()
        }

    /// <summary>
    /// Builds a unit-length arrow mesh along <c>+Y</c> anchored at <c>y = 0.0f</c> with cylindrical shaft <c>[0.0f..0.72f]</c> (radius <c>1.0f</c>)
    /// and conical arrowhead <c>[0.72f..1.0f]</c> (base radius <c>2.2f</c>); requires <c>segments &gt;= 3</c>.
    /// </summary>
    let unitArrowMesh (segments: int) : MeshData =
        if segments < 3 then
            raise (ArgumentOutOfRangeException (nameof segments, "Arrow mesh requires at least 3 radial segments."))

        let builder = MeshBuilder ()

        let shaftMat =
            Matrix4x4.CreateScale (2.0f, 0.72f, 2.0f)
            * Matrix4x4.CreateTranslation (0.0f, 0.36f, 0.0f)

        let headMat =
            Matrix4x4.CreateScale (4.4f, 0.28f, 4.4f)
            * Matrix4x4.CreateTranslation (0.0f, 0.86f, 0.0f)

        builder.AddMesh (unitCylinderMesh segments, shaftMat, RgbaColor.white)
        builder.AddMesh (unitConeMesh segments, headMat, RgbaColor.white)
        builder.ToMeshData ()

    /// <summary>Packs oriented 3D glyphs into <c>Instance3D</c> GPU payloads, omitting unmappable or non-finite entries.</summary>
    let packGlyphs (scale: Scale3D) (items: ReadOnlySpan<GlyphDatum3D>) : ReadOnlyMemory<Instance3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if items.Length = 0 then
            ReadOnlyMemory<Instance3D>.Empty
        else
            let out = Array.zeroCreate<Instance3D> items.Length
            let mutable count = 0

            for i = 0 to items.Length - 1 do
                let d = items[i]

                if MarksKernel.tryPackGlyph scale d.PickId d out count then
                    count <- count + 1

            MarksKernel.trimMemory out count

    /// <summary>
    /// Packs axis-aligned 3D cuboids for <c>Primitives.unitBox ()</c>, mapping <c>Bounds3</c> corners through <c>scale</c>
    /// so <c>UpAxis.YUp</c> heights remain strictly along <c>+Y</c> without coordinate swapping.
    /// </summary>
    let packBoxes (scale: Scale3D) (items: ReadOnlySpan<BoxDatum3D>) : ReadOnlyMemory<Instance3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if items.Length = 0 then
            ReadOnlyMemory<Instance3D>.Empty
        else
            let out = Array.zeroCreate<Instance3D> items.Length
            let mutable count = 0

            for i = 0 to items.Length - 1 do
                let d = items[i]

                if MarksKernel.tryPackBox scale d.PickId d out count then
                    count <- count + 1

            MarksKernel.trimMemory out count

    let private resolveBandCenterWidth (band: Band<string>) (key: string) : struct (float * float) voption =
        if isNull key then
            ValueNone
        else
            match Scale.bandPosition band key with
            | Some pos ->
                let bw = Scale.bandwidth band
                let w = if bw > 0.0 then bw else 1.0 / float (max 1 (Scale.bandCategories band).Length) * 0.6
                ValueSome struct (pos + bw * 0.5, w)
            | None -> ValueNone

    let private resolveBarCoord (axis: AxisScale3D) (coord: AxisCoord3D) (fallbackWidth: float) : struct (float * float) voption =
        match axis, coord with
        | AxisScale3D.Categorical band, AxisCoord3D.Cat key -> resolveBandCenterWidth band key
        | AxisScale3D.TimeUtc _, AxisCoord3D.Time instant ->
            if Check3D.isFinite fallbackWidth && fallbackWidth > 0.0 then
                let ms = float (instant.ToUnixTimeMilliseconds ())
                match AxisScale3D.tryMapNormalized axis ms with
                | ValueSome _ -> ValueSome struct (ms, fallbackWidth)
                | ValueNone -> ValueNone
            else
                ValueNone
        | AxisScale3D.Continuous _, AxisCoord3D.Num v
        | AxisScale3D.Piecewise _, AxisCoord3D.Num v
        | AxisScale3D.TimeUtc _, AxisCoord3D.Num v ->
            if Check3D.isFinite v && Check3D.isFinite fallbackWidth && fallbackWidth > 0.0 then
                match AxisScale3D.tryMapNormalized axis v with
                | ValueSome _ -> ValueSome struct (v, fallbackWidth)
                | ValueNone -> ValueNone
            else
                ValueNone
        | _ -> ValueNone

    /// <summary>
    /// Builds a <c>BarDatum3D</c> centered on <c>xCategory</c> and <c>yCategory</c> from categorical <c>Scale3D</c> X/Y axes,
    /// returning <c>ValueNone</c> when either axis is not <c>AxisScale3D.Categorical</c>, either category is unknown, or <c>z0</c>/<c>z1</c> is non-finite.
    /// </summary>
    let tryBarFromCategories
        (scale: Scale3D)
        (xCategory: string)
        (yCategory: string)
        (z0: float)
        (z1: float)
        (color: RgbaColor)
        (shading: ShadingMode)
        (pickId: uint32)
        (selected: bool)
        : BarDatum3D voption =
        if isNull (box scale) then
            nullArg (nameof scale)

        if not (Check3D.isFinite z0 && Check3D.isFinite z1) then
            ValueNone
        else
            match scale.XScale, scale.YScale with
            | AxisScale3D.Categorical bandX, AxisScale3D.Categorical bandY ->
                match resolveBandCenterWidth bandX xCategory, resolveBandCenterWidth bandY yCategory with
                | ValueSome struct (cx, wX), ValueSome struct (cy, wY) ->
                    ValueSome
                        {
                            X = cx
                            Y = cy
                            Z0 = z0
                            Z1 = z1
                            WidthX = wX
                            WidthY = wY
                            Color = color
                            Shading = shading
                            PickId = pickId
                            Selected = selected
                        }
                | _ -> ValueNone
            | _ -> ValueNone

    /// <summary>
    /// Builds a <c>BarDatum3D</c> from mixed categorical, numeric, or UTC time <c>AxisCoord3D</c> coordinates on X and Y,
    /// using categorical band widths automatically on categorical axes and <c>defaultWidthX</c>/<c>defaultWidthY</c> on continuous axes.
    /// </summary>
    let tryBarFromCoords
        (scale: Scale3D)
        (x: AxisCoord3D)
        (y: AxisCoord3D)
        (z0: float)
        (z1: float)
        (defaultWidthX: float)
        (defaultWidthY: float)
        (color: RgbaColor)
        : BarDatum3D voption =
        if isNull (box scale) then
            nullArg (nameof scale)

        if not (Check3D.isFinite z0 && Check3D.isFinite z1 && MarksKernel.isFiniteColor color) then
            ValueNone
        else
            match resolveBarCoord scale.XScale x defaultWidthX, resolveBarCoord scale.YScale y defaultWidthY with
            | ValueSome struct (cx, wx), ValueSome struct (cy, wy) ->
                ValueSome (BarDatum3D.create cx cy z0 z1 wx wy color)
            | _ -> ValueNone

    /// <summary>
    /// Packs axis-aligned 3D columns for <c>Primitives.unitBox ()</c> anchored between <c>Z0</c> and <c>Z1</c> under both <c>UpAxis.ZUp</c> and <c>UpAxis.YUp</c>.
    /// </summary>
    let packBars (scale: Scale3D) (items: ReadOnlySpan<BarDatum3D>) : ReadOnlyMemory<Instance3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if items.Length = 0 then
            ReadOnlyMemory<Instance3D>.Empty
        else
            let out = Array.zeroCreate<Instance3D> items.Length
            let mutable count = 0

            for i = 0 to items.Length - 1 do
                let d = items[i]

                if MarksKernel.tryPackBar scale d.PickId d out count then
                    count <- count + 1

            MarksKernel.trimMemory out count

    /// <summary>
    /// Packs oriented 3D arrows for <c>Glyph3D.unitArrowMesh</c>, anchoring the tail at <c>Origin</c> and pointing toward <c>Origin + Vector</c>.
    /// Non-finite, unmappable, non-positive, or near-zero-length arrows are omitted cleanly.
    /// </summary>
    let packArrows (scale: Scale3D) (items: ReadOnlySpan<ArrowDatum3D>) : ReadOnlyMemory<Instance3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if items.Length = 0 then
            ReadOnlyMemory<Instance3D>.Empty
        else
            let out = Array.zeroCreate<Instance3D> items.Length
            let mutable count = 0

            for i = 0 to items.Length - 1 do
                let d = items[i]

                if MarksKernel.tryPackArrow scale d.PickId d out count then
                    count <- count + 1

            MarksKernel.trimMemory out count

/// <summary>3D hairline segment, gap-aware polyline, extruded smooth tube, double-sided ribbon, and wireframe primitive builders.</summary>
[<RequireQualifiedAccess>]
module Line3D =
    let private segmentsWithSection (ignoreSectionPlane: bool) (scale: Scale3D) (pairs: ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)>) : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if pairs.Length = 0 then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let out = Array.zeroCreate<LineVertex3D> (pairs.Length * 2)
            let mutable count = 0

            for i = 0 to pairs.Length - 1 do
                if MarksKernel.tryPackSegmentWithSection ignoreSectionPlane scale pairs[i] out count then
                    count <- count + 2

            MarksKernel.trimMemory out count

    /// <summary>Maps independent 3D segment pairs into paired <c>LineList</c> vertices, omitting any pair with unmappable endpoints or non-finite colors.</summary>
    let segments (scale: Scale3D) (pairs: ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)>) : ReadOnlyMemory<LineVertex3D> =
        segmentsWithSection false scale pairs

    /// <summary>Maps independent 3D segment pairs into paired <c>LineList</c> vertices exempt from active <c>SectionPlane</c> clipping (<c>Pad = 1.0f</c>).</summary>
    let segmentsExempt (scale: Scale3D) (pairs: ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)>) : ReadOnlyMemory<LineVertex3D> =
        segmentsWithSection true scale pairs

    let private polylineWithSection (ignoreSectionPlane: bool) (scale: Scale3D) (points: IReadOnlyList<struct (Point3 * RgbaColor) voption>) : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box points) then
            nullArg (nameof points)

        if points.Count < 2 then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let out = ResizeArray<LineVertex3D>((points.Count - 1) * 2)
            let mutable prev: struct (Vector3 * RgbaColor) voption = ValueNone

            for i = 0 to points.Count - 1 do
                match points[i] with
                | ValueNone -> prev <- ValueNone
                | ValueSome struct (pt, col) ->
                    if MarksKernel.isFinitePoint3 pt && MarksKernel.isFiniteColor col then
                        match Scale3D.tryMap scale pt with
                        | ValueSome w ->
                            match prev with
                            | ValueSome struct (prevW, prevCol) ->
                                out.Add (LineVertex3D (prevW, prevCol, ignoreSectionPlane))
                                out.Add (LineVertex3D (w, col, ignoreSectionPlane))
                            | ValueNone -> ()

                            prev <- ValueSome struct (w, col)
                        | ValueNone -> prev <- ValueNone
                    else
                        prev <- ValueNone

            if out.Count = 0 then
                ReadOnlyMemory<LineVertex3D>.Empty
            else
                ReadOnlyMemory<LineVertex3D>(out.ToArray ())

    /// <summary>Maps a polyline sequence into paired <c>LineList</c> vertices, breaking runs cleanly across <c>ValueNone</c> or unmappable gaps.</summary>
    let polyline (scale: Scale3D) (points: IReadOnlyList<struct (Point3 * RgbaColor) voption>) : ReadOnlyMemory<LineVertex3D> =
        polylineWithSection false scale points

    /// <summary>Maps a polyline sequence into section-plane-exempt (<c>Pad = 1.0f</c>) paired <c>LineList</c> vertices, breaking runs cleanly across gaps.</summary>
    let polylineExempt (scale: Scale3D) (points: IReadOnlyList<struct (Point3 * RgbaColor) voption>) : ReadOnlyMemory<LineVertex3D> =
        polylineWithSection true scale points

    let private appendBoxWireframe (scale: Scale3D) (bounds: Bounds3) (color: RgbaColor) (ignoreSectionPlane: bool) (out: ResizeArray<LineVertex3D>) =
        if MarksKernel.isFiniteColor color then
            let x0, y0, z0 = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
            let x1, y1, z1 = Bounds3.maxX bounds, Bounds3.maxY bounds, Bounds3.maxZ bounds

            if Check3D.isFinite x0 && Check3D.isFinite y0 && Check3D.isFinite z0
               && Check3D.isFinite x1 && Check3D.isFinite y1 && Check3D.isFinite z1 then
                let c000 = Scale3D.tryMap scale (Point3.create x0 y0 z0)
                let c100 = Scale3D.tryMap scale (Point3.create x1 y0 z0)
                let c110 = Scale3D.tryMap scale (Point3.create x1 y1 z0)
                let c010 = Scale3D.tryMap scale (Point3.create x0 y1 z0)
                let c001 = Scale3D.tryMap scale (Point3.create x0 y0 z1)
                let c101 = Scale3D.tryMap scale (Point3.create x1 y0 z1)
                let c111 = Scale3D.tryMap scale (Point3.create x1 y1 z1)
                let c011 = Scale3D.tryMap scale (Point3.create x0 y1 z1)

                let emitEdge a b =
                    match a, b with
                    | ValueSome wa, ValueSome wb ->
                        out.Add (LineVertex3D (wa, color, ignoreSectionPlane))
                        out.Add (LineVertex3D (wb, color, ignoreSectionPlane))
                    | _ -> ()

                emitEdge c000 c100
                emitEdge c100 c110
                emitEdge c110 c010
                emitEdge c010 c000
                emitEdge c001 c101
                emitEdge c101 c111
                emitEdge c111 c011
                emitEdge c011 c001
                emitEdge c000 c001
                emitEdge c100 c101
                emitEdge c110 c111
                emitEdge c010 c011

    /// <summary>Emits the 12 edges (24 <c>LineVertex3D</c> endpoints) of <c>bounds</c> mapped through <c>scale</c>.</summary>
    let boxWireframe (scale: Scale3D) (bounds: Bounds3) (color: RgbaColor) (ignoreSectionPlane: bool) : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        let out = ResizeArray<LineVertex3D>(24)
        appendBoxWireframe scale bounds color ignoreSectionPlane out
        if out.Count = 0 then ReadOnlyMemory<LineVertex3D>.Empty else ReadOnlyMemory<LineVertex3D>(out.ToArray ())

    /// <summary>Batches wireframe edges across multiple <c>Bounds3</c> cuboids into a single contiguous <c>LineVertex3D</c> slice.</summary>
    let boxesWireframe (scale: Scale3D) (boxes: ReadOnlySpan<struct (Bounds3 * RgbaColor)>) (ignoreSectionPlane: bool) : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if boxes.Length = 0 then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let out = ResizeArray<LineVertex3D>(boxes.Length * 24)

            for i = 0 to boxes.Length - 1 do
                let struct (b, col) = boxes[i]
                appendBoxWireframe scale b col ignoreSectionPlane out

            if out.Count = 0 then ReadOnlyMemory<LineVertex3D>.Empty else ReadOnlyMemory<LineVertex3D>(out.ToArray ())

    let private appendCircle
        (scale: Scale3D)
        (center: Point3)
        (normal: Vector3)
        (radiusData: float)
        (segments: int)
        (color: RgbaColor)
        (ignoreSectionPlane: bool)
        (out: ResizeArray<LineVertex3D>)
        =
        if MarksKernel.isFinitePoint3 center
           && MarksKernel.isFiniteVec3 normal
           && Check3D.isFinite radiusData
           && radiusData > 0.0
           && MarksKernel.isFiniteColor color then
            let lenSq = normal.LengthSquared ()
            if lenSq > 1e-12f then
                let n = Vector3.Normalize normal
                let refAxis = if MathF.Abs n.Y < 0.9f then Vector3.UnitY else Vector3.UnitX
                let u = Vector3.Normalize (Vector3.Cross (refAxis, n))
                let v = Vector3.Cross (n, u)
                let ux, uy, uz = float u.X * radiusData, float u.Y * radiusData, float u.Z * radiusData
                let vx, vy, vz = float v.X * radiusData, float v.Y * radiusData, float v.Z * radiusData

                let sampleRingPt i =
                    let ang = float i * (Math.PI * 2.0 / float segments)
                    let c = cos ang
                    let s = sin ang
                    let px = center.X + ux * c + vx * s
                    let py = center.Y + uy * c + vy * s
                    let pz = center.Z + uz * c + vz * s
                    if Check3D.isFinite px && Check3D.isFinite py && Check3D.isFinite pz then
                        Scale3D.tryMap scale (Point3.create px py pz)
                    else
                        ValueNone

                let firstOpt = sampleRingPt 0
                let mutable prevOpt = firstOpt

                for i = 1 to segments do
                    let currOpt = if i = segments then firstOpt else sampleRingPt i
                    match prevOpt, currOpt with
                    | ValueSome wa, ValueSome wb ->
                        out.Add (LineVertex3D (wa, color, ignoreSectionPlane))
                        out.Add (LineVertex3D (wb, color, ignoreSectionPlane))
                    | _ -> ()
                    prevOpt <- currOpt

    /// <summary>Emits a closed 3D wireframe circle centered at <c>center</c> perpendicular to <c>normal</c> with radius <c>radiusData</c> in data units; requires <c>segments &gt;= 3</c>.</summary>
    let circle
        (scale: Scale3D)
        (center: Point3)
        (normal: Vector3)
        (radiusData: float)
        (segments: int)
        (color: RgbaColor)
        (ignoreSectionPlane: bool)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if segments < 3 then
            raise (ArgumentOutOfRangeException (nameof segments, "Circle requires at least 3 segments."))

        let out = ResizeArray<LineVertex3D>(segments * 2)
        appendCircle scale center normal radiusData segments color ignoreSectionPlane out
        if out.Count = 0 then ReadOnlyMemory<LineVertex3D>.Empty else ReadOnlyMemory<LineVertex3D>(out.ToArray ())

    /// <summary>Emits a multi-ring spherical wireframe cage (latitude rings plus orthogonal meridian great circles); requires <c>rings &gt;= 1</c> and <c>segmentsPerRing &gt;= 3</c>.</summary>
    let sphereCage
        (scale: Scale3D)
        (center: Point3)
        (radiusData: float)
        (rings: int)
        (segmentsPerRing: int)
        (color: RgbaColor)
        (ignoreSectionPlane: bool)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if rings < 1 then
            raise (ArgumentOutOfRangeException (nameof rings, "Sphere cage requires at least 1 latitude ring."))

        if segmentsPerRing < 3 then
            raise (ArgumentOutOfRangeException (nameof segmentsPerRing, "Sphere cage requires at least 3 segments per ring."))

        if not (MarksKernel.isFinitePoint3 center && Check3D.isFinite radiusData && radiusData > 0.0 && MarksKernel.isFiniteColor color) then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let out = ResizeArray<LineVertex3D>((rings + 2) * segmentsPerRing * 2)

            for r = 1 to rings do
                let phi = Math.PI * float r / float (rings + 1)
                let ringZ = center.Z + radiusData * cos phi
                let ringRad = radiusData * sin phi
                if Check3D.isFinite ringZ && ringRad > 1e-12 then
                    appendCircle scale (Point3.create center.X center.Y ringZ) Vector3.UnitZ ringRad segmentsPerRing color ignoreSectionPlane out

            appendCircle scale center Vector3.UnitX radiusData segmentsPerRing color ignoreSectionPlane out
            appendCircle scale center Vector3.UnitY radiusData segmentsPerRing color ignoreSectionPlane out
            if out.Count = 0 then ReadOnlyMemory<LineVertex3D>.Empty else ReadOnlyMemory<LineVertex3D>(out.ToArray ())

    let private computePolylineTangents (pts: Vector3 array) : Vector3 array voption =
        let n = pts.Length
        let tangents = Array.zeroCreate<Vector3> n
        let mutable anyValid = false

        let findNeighborDir (idx: int) (step: int) =
            let mutable j = idx + step
            let mutable found = Vector3.Zero
            let mutable ok = false

            while not ok && j >= 0 && j < n do
                let d = pts[j] - pts[idx]

                if d.LengthSquared () > 1e-12f then
                    found <- if step > 0 then Vector3.Normalize d else Vector3.Normalize (-d)
                    ok <- true
                else
                    j <- j + step

            struct (ok, found)

        for i = 0 to n - 1 do
            let struct (hasPrev, dirPrev) = findNeighborDir i -1
            let struct (hasNext, dirNext) = findNeighborDir i 1

            if hasPrev && hasNext then
                let sum = dirPrev + dirNext

                if sum.LengthSquared () > 1e-12f then
                    tangents[i] <- Vector3.Normalize sum
                else
                    tangents[i] <- dirNext

                anyValid <- true
            elif hasNext then
                tangents[i] <- dirNext
                anyValid <- true
            elif hasPrev then
                tangents[i] <- dirPrev
                anyValid <- true

        if anyValid then ValueSome tangents else ValueNone

    let private appendTubeFromSpan
        (scale: Scale3D)
        (sides: int)
        (points: ReadOnlySpan<struct (Point3 * float32 * RgbaColor)>)
        (verts: ResizeArray<Vertex3D>)
        (inds: ResizeArray<uint32>)
        : Result<unit, LayoutError list> =
        let n = points.Length
        if n < 2 then
            Error [ LayoutError.InvalidObservation "Tube requires at least 2 points." ]
        else
            let wPts = Array.zeroCreate<Vector3> n
            let radii = Array.zeroCreate<float32> n
            let colors = Array.zeroCreate<Vector4> n
            let mutable errMsg: string option = None
            let mutable i = 0

            while i < n && errMsg.IsNone do
                let struct (pt, r, col) = points[i]

                if not (MarksKernel.isFinitePoint3 pt) then
                    errMsg <- Some $"Tube point {i} has non-finite coordinates."
                elif not (Check3D.isFinite32 r) || r < 0.0f then
                    errMsg <- Some $"Tube radius at point {i} must be finite and nonnegative."
                elif not (MarksKernel.isFiniteColor col) then
                    errMsg <- Some $"Tube color at point {i} has non-finite channels."
                else
                    match Scale3D.tryMap scale pt with
                    | ValueSome wp ->
                        wPts[i] <- wp
                        radii[i] <- r
                        colors[i] <- col.ToVector4 ()
                    | ValueNone ->
                        errMsg <- Some $"Tube point {i} lies outside the active Scale3D domain."

                i <- i + 1

            match errMsg with
            | Some msg -> Error [ LayoutError.InvalidObservation msg ]
            | None ->
                match computePolylineTangents wPts with
                | ValueNone ->
                    Error [ LayoutError.InvalidObservation "Tube points must span a non-zero path." ]
                | ValueSome tangents ->
                    let baseVert = uint32 verts.Count
                    let cosTable = Array.zeroCreate<float32> sides
                    let sinTable = Array.zeroCreate<float32> sides

                    for s = 0 to sides - 1 do
                        let ang = float32 s / float32 sides * MathF.PI * 2.0f
                        cosTable[s] <- MathF.Cos ang
                        sinTable[s] <- MathF.Sin ang

                    let t0 = tangents[0]
                    let mutable prevSide = MarksKernel.fallbackPerp t0
                    let mutable prevT = t0

                    let ringNormals = Array.zeroCreate<Vector3> (n * sides)

                    for idx = 0 to n - 1 do
                        let tCurr = tangents[idx]

                        let side =
                            if idx = 0 then
                                prevSide
                            else
                                MarksKernel.rotateMinimal prevT tCurr prevSide

                        let up = Vector3.Normalize (Vector3.Cross (tCurr, side))
                        prevSide <- side
                        prevT <- tCurr

                        let p = wPts[idx]
                        let r = radii[idx]
                        let col = colors[idx]

                        for s = 0 to sides - 1 do
                            let nrm = side * cosTable[s] + up * sinTable[s]
                            ringNormals[idx * sides + s] <- nrm
                            verts.Add (Vertex3D (p + nrm * r, nrm, col))

                    for idx = 0 to n - 2 do
                        let ring0 = baseVert + uint32 (idx * sides)
                        let ring1 = baseVert + uint32 ((idx + 1) * sides)

                        for s = 0 to sides - 1 do
                            let sNext = (s + 1) % sides
                            let a = ring0 + uint32 s
                            let b = ring0 + uint32 sNext
                            let c = ring1 + uint32 sNext
                            let d = ring1 + uint32 s
                            inds.Add a
                            inds.Add b
                            inds.Add c
                            inds.Add a
                            inds.Add c
                            inds.Add d

                    if radii[0] > 0.0f then
                        let capNormal = -tangents[0]
                        let centerIdx = uint32 verts.Count
                        verts.Add (Vertex3D (wPts[0], capNormal, colors[0]))
                        let rimStart = uint32 verts.Count

                        for s = 0 to sides - 1 do
                            let p = wPts[0] + ringNormals[s] * radii[0]
                            verts.Add (Vertex3D (p, capNormal, colors[0]))

                        for s = 0 to sides - 1 do
                            let sNext = (s + 1) % sides
                            inds.Add centerIdx
                            inds.Add (rimStart + uint32 sNext)
                            inds.Add (rimStart + uint32 s)

                    let lastIdx = n - 1

                    if radii[lastIdx] > 0.0f then
                        let capNormal = tangents[lastIdx]
                        let centerIdx = uint32 verts.Count
                        verts.Add (Vertex3D (wPts[lastIdx], capNormal, colors[lastIdx]))
                        let rimStart = uint32 verts.Count
                        let lastRingOffset = lastIdx * sides

                        for s = 0 to sides - 1 do
                            let p = wPts[lastIdx] + ringNormals[lastRingOffset + s] * radii[lastIdx]
                            verts.Add (Vertex3D (p, capNormal, colors[lastIdx]))

                        for s = 0 to sides - 1 do
                            let sNext = (s + 1) % sides
                            inds.Add centerIdx
                            inds.Add (rimStart + uint32 s)
                            inds.Add (rimStart + uint32 sNext)

                    Ok ()

    /// <summary>
    /// Extrudes a closed, capped cylindrical <c>MeshData</c> tube along variable-radius, per-vertex colored 3D polyline samples
    /// using twist-free parallel transport frames and outward CCW winding.
    /// </summary>
    let tube (scale: Scale3D) (sides: int) (points: ReadOnlySpan<struct (Point3 * float32 * RgbaColor)>) : Result<MeshData, LayoutError list> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if sides < 3 then
            Error [ LayoutError.InvalidObservation "Tube cross-section requires at least 3 sides." ]
        elif points.Length < 2 then
            Error [ LayoutError.InvalidObservation "Tube requires at least 2 points." ]
        else
            let verts = ResizeArray<Vertex3D>(points.Length * sides + (sides + 1) * 2)
            let inds = ResizeArray<uint32>((points.Length - 1) * sides * 6 + sides * 6)

            match appendTubeFromSpan scale sides points verts inds with
            | Error errs -> Error errs
            | Ok () ->
                Ok
                    {
                        Vertices = verts.ToArray ()
                        Indices = inds.ToArray ()
                    }

    let private toSpanOfList (items: IReadOnlyList<'T>) : ReadOnlySpan<'T> =
        match items with
        | :? ('T array) as arr -> ReadOnlySpan<'T>(arr)
        | _ ->
            let arr = Array.zeroCreate<'T> items.Count
            for i = 0 to items.Count - 1 do
                arr[i] <- items[i]
            ReadOnlySpan<'T>(arr)

    /// <summary>Batches multiple variable-radius colored polyline tubes into a single indexed <c>MeshData</c> with merged vertex offsets.</summary>
    let tubes (scale: Scale3D) (sides: int) (polylines: seq<#IReadOnlyList<struct (Point3 * float32 * RgbaColor)>>) : Result<MeshData, LayoutError list> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box polylines) then
            nullArg (nameof polylines)

        if sides < 3 then
            Error [ LayoutError.InvalidObservation "Tube cross-section requires at least 3 sides." ]
        else
            let verts = ResizeArray<Vertex3D>()
            let inds = ResizeArray<uint32>()
            let mutable firstErr: LayoutError list option = None
            use e = polylines.GetEnumerator ()

            while e.MoveNext () && firstErr.IsNone do
                let poly = e.Current
                if isNull (box poly) then
                    nullArg "polylines"

                match appendTubeFromSpan scale sides (toSpanOfList poly) verts inds with
                | Ok () -> ()
                | Error errs -> firstErr <- Some errs

            match firstErr with
            | Some errs -> Error errs
            | None ->
                Ok
                    {
                        Vertices = verts.ToArray ()
                        Indices = inds.ToArray ()
                    }

    let private appendRibbonFromSpan
        (scale: Scale3D)
        (samples: ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)>)
        (verts: ResizeArray<Vertex3D>)
        (inds: ResizeArray<uint32>)
        : Result<unit, LayoutError list> =
        let n = samples.Length
        if n < 2 then
            Error [ LayoutError.InvalidObservation "Ribbon requires at least 2 samples." ]
        else
            let wLow = Array.zeroCreate<Vector3> n
            let wHigh = Array.zeroCreate<Vector3> n
            let wMid = Array.zeroCreate<Vector3> n
            let colors = Array.zeroCreate<Vector4> n
            let mutable errMsg: string option = None
            let mutable i = 0

            while i < n && errMsg.IsNone do
                let struct (pLow, pHigh, col) = samples[i]

                if not (MarksKernel.isFinitePoint3 pLow && MarksKernel.isFinitePoint3 pHigh) then
                    errMsg <- Some $"Ribbon sample {i} has non-finite point coordinates."
                elif not (MarksKernel.isFiniteColor col) then
                    errMsg <- Some $"Ribbon sample {i} has non-finite color channels."
                else
                    match Scale3D.tryMap scale pLow with
                    | ValueSome wl ->
                        match Scale3D.tryMap scale pHigh with
                        | ValueSome wh ->
                            wLow[i] <- wl
                            wHigh[i] <- wh
                            wMid[i] <- (wl + wh) * 0.5f
                            colors[i] <- col.ToVector4 ()
                        | ValueNone ->
                            errMsg <- Some $"Ribbon sample {i} lies outside the active Scale3D domain."
                    | ValueNone ->
                        errMsg <- Some $"Ribbon sample {i} lies outside the active Scale3D domain."

                i <- i + 1

            match errMsg with
            | Some msg -> Error [ LayoutError.InvalidObservation msg ]
            | None ->
                let normals = Array.zeroCreate<Vector3> n
                let validNormal = Array.zeroCreate<bool> n
                let mutable hasAnyNormal = false

                for idx = 0 to n - 1 do
                    let tU =
                        if idx = 0 then wMid[1] - wMid[0]
                        elif idx = n - 1 then wMid[n - 1] - wMid[n - 2]
                        else wMid[idx + 1] - wMid[idx - 1]

                    let tV = wHigh[idx] - wLow[idx]
                    let rawN = Vector3.Cross (tU, tV)

                    if rawN.LengthSquared () > 1e-12f then
                        normals[idx] <- Vector3.Normalize rawN
                        validNormal[idx] <- true
                        hasAnyNormal <- true

                if not hasAnyNormal then
                    for idx = 0 to n - 1 do
                        normals[idx] <- Vector3.UnitY
                else
                    let mutable lastGood = Vector3.UnitY
                    let mutable seenGood = false

                    for idx = 0 to n - 1 do
                        if validNormal[idx] then
                            lastGood <- normals[idx]
                            seenGood <- true
                        elif seenGood then
                            normals[idx] <- lastGood

                    seenGood <- false

                    for idx = n - 1 downto 0 do
                        if validNormal[idx] then
                            lastGood <- normals[idx]
                            seenGood <- true
                        elif seenGood && not validNormal[idx] then
                            normals[idx] <- lastGood

                let baseVert = uint32 verts.Count
                let backVertexBase = uint32 (2 * n)

                for idx = 0 to n - 1 do
                    let nrm = normals[idx]
                    let col = colors[idx]
                    verts.Add (Vertex3D (wLow[idx], nrm, col))
                    verts.Add (Vertex3D (wHigh[idx], nrm, col))

                for idx = 0 to n - 1 do
                    let nrm = normals[idx]
                    let col = colors[idx]
                    verts.Add (Vertex3D (wLow[idx], -nrm, col))
                    verts.Add (Vertex3D (wHigh[idx], -nrm, col))

                for idx = 0 to n - 2 do
                    let fl0 = baseVert + uint32 (idx * 2)
                    let fu0 = fl0 + 1u
                    let fl1 = fl0 + 2u
                    let fu1 = fl0 + 3u
                    inds.Add fl0
                    inds.Add fl1
                    inds.Add fu1
                    inds.Add fl0
                    inds.Add fu1
                    inds.Add fu0

                for idx = 0 to n - 2 do
                    let bl0 = baseVert + backVertexBase + uint32 (idx * 2)
                    let bu0 = bl0 + 1u
                    let bl1 = bl0 + 2u
                    let bu1 = bl0 + 3u
                    inds.Add bl0
                    inds.Add bu1
                    inds.Add bl1
                    inds.Add bl0
                    inds.Add bu0
                    inds.Add bu1

                Ok ()

    /// <summary>
    /// Builds a double-sided ruled ribbon <c>MeshData</c> between paired <c>lower</c> and <c>upper</c> 3D curves with smooth normals
    /// and explicit front/back faces so it remains visible from both sides even with back-face culling enabled.
    /// </summary>
    let ribbon (scale: Scale3D) (samples: ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)>) : Result<MeshData, LayoutError list> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if samples.Length < 2 then
            Error [ LayoutError.InvalidObservation "Ribbon requires at least 2 samples." ]
        else
            let verts = ResizeArray<Vertex3D>(4 * samples.Length)
            let inds = ResizeArray<uint32>(12 * (samples.Length - 1))

            match appendRibbonFromSpan scale samples verts inds with
            | Error errs -> Error errs
            | Ok () ->
                Ok
                    {
                        Vertices = verts.ToArray ()
                        Indices = inds.ToArray ()
                    }

    /// <summary>Batches multiple double-sided ruled ribbon strips into a single indexed <c>MeshData</c> with merged vertex offsets.</summary>
    let ribbons (scale: Scale3D) (strips: seq<#IReadOnlyList<struct (Point3 * Point3 * RgbaColor)>>) : Result<MeshData, LayoutError list> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box strips) then
            nullArg (nameof strips)

        let verts = ResizeArray<Vertex3D>()
        let inds = ResizeArray<uint32>()
        let mutable firstErr: LayoutError list option = None
        use e = strips.GetEnumerator ()

        while e.MoveNext () && firstErr.IsNone do
            let strip = e.Current
            if isNull (box strip) then
                nullArg "strips"

            match appendRibbonFromSpan scale (toSpanOfList strip) verts inds with
            | Ok () -> ()
            | Error errs -> firstErr <- Some errs

        match firstErr with
        | Some errs -> Error errs
        | None ->
            Ok
                {
                    Vertices = verts.ToArray ()
                    Indices = inds.ToArray ()
                }

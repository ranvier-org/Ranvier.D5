namespace Ranvier.D5

open System
open System.Numerics

/// <summary>A finite three-dimensional point in an explicitly chosen coordinate system.</summary>
[<Struct>]
type Point3 =
    /// <summary>X coordinate.</summary>
    val X: float
    /// <summary>Y coordinate.</summary>
    val Y: float
    /// <summary>Z coordinate.</summary>
    val Z: float
    internal new(x: float, y: float, z: float) = { X = x; Y = y; Z = z }

    /// <summary>Returns the component-wise sum of two 3D points; rejects non-finite results.</summary>
    static member (+) (a: Point3, b: Point3) : Point3 =
        let x, y, z = a.X + b.X, a.Y + b.Y, a.Z + b.Z
        Check.finite "x" x; Check.finite "y" y; Check.finite "z" z
        Point3(x, y, z)

    /// <summary>Returns the component-wise difference of two 3D points; rejects non-finite results.</summary>
    static member (-) (a: Point3, b: Point3) : Point3 =
        let x, y, z = a.X - b.X, a.Y - b.Y, a.Z - b.Z
        Check.finite "x" x; Check.finite "y" y; Check.finite "z" z
        Point3(x, y, z)

    /// <summary>Scales a 3D point by a finite scalar; rejects non-finite results.</summary>
    static member (*) (p: Point3, scalar: float) : Point3 =
        Check.finite "scalar" scalar
        let x, y, z = p.X * scalar, p.Y * scalar, p.Z * scalar
        Check.finite "x" x; Check.finite "y" y; Check.finite "z" z
        Point3(x, y, z)

    /// <summary>Scales a 3D point by a finite scalar; rejects non-finite results.</summary>
    static member (*) (scalar: float, p: Point3) : Point3 =
        p * scalar

    /// <summary>Divides a 3D point by a finite nonzero scalar; rejects zero divisors and non-finite results.</summary>
    static member (/) (p: Point3, divisor: float) : Point3 =
        Check.finite "divisor" divisor
        if divisor = 0. then invalidArg "divisor" "Divisor must be nonzero."
        let x, y, z = p.X / divisor, p.Y / divisor, p.Z / divisor
        Check.finite "x" x; Check.finite "y" y; Check.finite "z" z
        Point3(x, y, z)

    /// <summary>Negates all coordinates of a 3D point.</summary>
    static member (~-) (p: Point3) : Point3 =
        Point3(-p.X, -p.Y, -p.Z)

/// <summary>Construction, vector arithmetic, distance and linear interpolation for <c>Point3</c>.</summary>
[<RequireQualifiedAccess>]
module Point3 =
    /// <summary>Creates a 3D point with finite X, Y and Z coordinates.</summary>
    let create x y z =
        Check.finite "x" x
        Check.finite "y" y
        Check.finite "z" z
        Point3(x, y, z)

    /// <summary>The 3D origin <c>(0, 0, 0)</c>.</summary>
    let zero = Point3(0., 0., 0.)

    /// <summary>Alias for <c>Point3.zero</c>.</summary>
    let origin = zero

    /// <summary>Returns the component-wise sum <c>a + b</c>.</summary>
    let add (a: Point3) (b: Point3) : Point3 = a + b

    /// <summary>Returns the component-wise difference <c>a - b</c>.</summary>
    let sub (a: Point3) (b: Point3) : Point3 = a - b

    /// <summary>Scales <c>p</c> by finite <c>scalar</c>.</summary>
    let scale (scalar: float) (p: Point3) : Point3 = p * scalar

    /// <summary>Returns the Euclidean dot product of two 3D vectors; rejects non-finite overflow.</summary>
    let dot (a: Point3) (b: Point3) : float =
        let d = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        Check.finite "dot" d
        d

    /// <summary>Returns the 3D cross product <c>a x b</c>; rejects non-finite results.</summary>
    let cross (a: Point3) (b: Point3) : Point3 =
        create (a.Y * b.Z - a.Z * b.Y) (a.Z * b.X - a.X * b.Z) (a.X * b.Y - a.Y * b.X)

    /// <summary>Returns overflow-protected Euclidean distance between two 3D points.</summary>
    let distance (a: Point3) (b: Point3) =
        let dx, dy, dz = abs (a.X - b.X), abs (a.Y - b.Y), abs (a.Z - b.Z)
        let m = max dx (max dy dz)
        if m = 0. then 0.
        elif Double.IsPositiveInfinity m then infinity
        else
            let rx, ry, rz = dx / m, dy / m, dz / m
            let d = m * sqrt (rx * rx + ry * ry + rz * rz)
            Check.finite "distance" d
            d

    /// <summary>Returns squared Euclidean length; very large vectors may overflow to infinity.</summary>
    let lengthSquared (p: Point3) : float =
        p.X * p.X + p.Y * p.Y + p.Z * p.Z

    /// <summary>Returns overflow-protected Euclidean length of <c>p</c>.</summary>
    let length (p: Point3) : float =
        distance zero p

    /// <summary>Returns the unit-direction vector when <c>length p &gt; 1e-15</c>, or <c>ValueNone</c> for zero/degenerate vectors.</summary>
    let tryNormalize (p: Point3) : Point3 voption =
        let len = length p
        if len <= 1e-15 then ValueNone
        else ValueSome(create (p.X / len) (p.Y / len) (p.Z / len))

    /// <summary>Returns the unit-direction vector of <c>p</c>; throws <c>ArgumentException</c> when <c>length p &lt;= 1e-15</c>.</summary>
    let normalize (p: Point3) : Point3 =
        match tryNormalize p with
        | ValueSome u -> u
        | ValueNone -> invalidArg "p" "Cannot normalize a zero or degenerate Point3 vector."

    /// <summary>Converts a <c>Point3</c> into a single-precision <c>System.Numerics.Vector3</c>; rejects coordinates that overflow <c>float32</c>.</summary>
    let toVector3 (p: Point3) : Vector3 =
        let v = Vector3(float32 p.X, float32 p.Y, float32 p.Z)
        Check.finite "x" (float v.X)
        Check.finite "y" (float v.Y)
        Check.finite "z" (float v.Z)
        v

    /// <summary>Converts a finite single-precision <c>System.Numerics.Vector3</c> into a <c>Point3</c>.</summary>
    let ofVector3 (v: Vector3) : Point3 =
        create (float v.X) (float v.Y) (float v.Z)

    /// <summary>Returns squared Euclidean distance; very large separations may overflow to infinity.</summary>
    let distanceSquared (a: Point3) (b: Point3) =
        let dx, dy, dz = a.X - b.X, a.Y - b.Y, a.Z - b.Z
        dx * dx + dy * dy + dz * dz

    /// <summary>Linearly interpolates between two finite 3D points at finite parameter <c>t</c>.</summary>
    let lerp (a: Point3) (b: Point3) (t: float) =
        Check.finite "t" t
        create (Check.lerp a.X b.X t) (Check.lerp a.Y b.Y t) (Check.lerp a.Z b.Z t)

/// <summary>A finite axis-aligned 3D cuboid with nonnegative extents.</summary>
[<Struct>]
type Bounds3 = private { MinX: float; MinY: float; MinZ: float; SizeX: float; SizeY: float; SizeZ: float }

/// <summary>Construction, accessors and containment/union operations for <c>Bounds3</c>.</summary>
[<RequireQualifiedAccess>]
module Bounds3 =
    /// <summary>Creates an axis-aligned 3D bounding box; minimum coordinates, nonnegative extents and far edges must be finite.</summary>
    let create minX minY minZ sizeX sizeY sizeZ =
        Check.finite "bounds3" minX; Check.finite "bounds3" minY; Check.finite "bounds3" minZ
        Check.finite "bounds3" sizeX; Check.finite "bounds3" sizeY; Check.finite "bounds3" sizeZ
        Check.finite "bounds3" (minX + sizeX); Check.finite "bounds3" (minY + sizeY); Check.finite "bounds3" (minZ + sizeZ)
        if sizeX < 0. || sizeY < 0. || sizeZ < 0. then
            invalidArg "bounds3" "Dimensions must be nonnegative."
        { MinX = minX; MinY = minY; MinZ = minZ; SizeX = sizeX; SizeY = sizeY; SizeZ = sizeZ }

    /// <summary>Creates a 3D bounding box spanning ordered minimum and maximum corner points.</summary>
    let ofExtrema (minPoint: Point3) (maxPoint: Point3) =
        Point3.create minPoint.X minPoint.Y minPoint.Z |> ignore
        Point3.create maxPoint.X maxPoint.Y maxPoint.Z |> ignore
        if minPoint.X > maxPoint.X || minPoint.Y > maxPoint.Y || minPoint.Z > maxPoint.Z then
            invalidArg "bounds3" "Minimum corner must not exceed maximum corner."
        create minPoint.X minPoint.Y minPoint.Z (maxPoint.X - minPoint.X) (maxPoint.Y - minPoint.Y) (maxPoint.Z - minPoint.Z)

    /// <summary>Creates a 3D bounding cuboid centered at <c>center</c> with nonnegative finite extents <c>(sizeX, sizeY, sizeZ)</c>.</summary>
    let ofCenterSize (center: Point3) (sizeX: float) (sizeY: float) (sizeZ: float) : Bounds3 =
        Check.finite "centerX" center.X; Check.finite "centerY" center.Y; Check.finite "centerZ" center.Z
        Check.finite "sizeX" sizeX; Check.finite "sizeY" sizeY; Check.finite "sizeZ" sizeZ
        if sizeX < 0. || sizeY < 0. || sizeZ < 0. then
            invalidArg "bounds3" "Dimensions must be nonnegative."
        create (center.X - sizeX * 0.5) (center.Y - sizeY * 0.5) (center.Z - sizeZ * 0.5) sizeX sizeY sizeZ

    /// <summary>Returns the tight axis-aligned bounding cuboid enclosing <c>points</c>, or <c>None</c> when empty.</summary>
    let ofPoints (points: seq<Point3>) : Bounds3 option =
        if isNull (box points) then nullArg (nameof points)
        let mutable any = false
        let mutable minX, minY, minZ = Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity
        let mutable maxX, maxY, maxZ = Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity
        for p in points do
            Check.finite "x" p.X; Check.finite "y" p.Y; Check.finite "z" p.Z
            any <- true
            if p.X < minX then minX <- p.X
            if p.Y < minY then minY <- p.Y
            if p.Z < minZ then minZ <- p.Z
            if p.X > maxX then maxX <- p.X
            if p.Y > maxY then maxY <- p.Y
            if p.Z > maxZ then maxZ <- p.Z
        if not any then None
        else Some(create minX minY minZ (maxX - minX) (maxY - minY) (maxZ - minZ))

    /// <summary>Returns the minimum X coordinate.</summary>
    let minX (bounds: Bounds3) = bounds.MinX
    /// <summary>Returns the minimum Y coordinate.</summary>
    let minY (bounds: Bounds3) = bounds.MinY
    /// <summary>Returns the minimum Z coordinate.</summary>
    let minZ (bounds: Bounds3) = bounds.MinZ
    /// <summary>Returns the nonnegative X extent.</summary>
    let sizeX (bounds: Bounds3) = bounds.SizeX
    /// <summary>Returns the nonnegative Y extent.</summary>
    let sizeY (bounds: Bounds3) = bounds.SizeY
    /// <summary>Returns the nonnegative Z extent.</summary>
    let sizeZ (bounds: Bounds3) = bounds.SizeZ
    /// <summary>Returns the maximum X coordinate.</summary>
    let maxX (bounds: Bounds3) = bounds.MinX + bounds.SizeX
    /// <summary>Returns the maximum Y coordinate.</summary>
    let maxY (bounds: Bounds3) = bounds.MinY + bounds.SizeY
    /// <summary>Returns the maximum Z coordinate.</summary>
    let maxZ (bounds: Bounds3) = bounds.MinZ + bounds.SizeZ
    /// <summary>Returns the minimum corner point.</summary>
    let minPoint (bounds: Bounds3) = Point3(bounds.MinX, bounds.MinY, bounds.MinZ)
    /// <summary>Returns the maximum corner point.</summary>
    let maxPoint (bounds: Bounds3) = Point3(bounds.MinX + bounds.SizeX, bounds.MinY + bounds.SizeY, bounds.MinZ + bounds.SizeZ)
    /// <summary>Returns the center point of the cuboid.</summary>
    let center (bounds: Bounds3) =
        Point3(bounds.MinX + bounds.SizeX * 0.5, bounds.MinY + bounds.SizeY * 0.5, bounds.MinZ + bounds.SizeZ * 0.5)

    /// <summary>Tests inclusive face containment; non-finite points are outside.</summary>
    let contains (point: Point3) (bounds: Bounds3) =
        point.X >= bounds.MinX && point.X <= bounds.MinX + bounds.SizeX
        && point.Y >= bounds.MinY && point.Y <= bounds.MinY + bounds.SizeY
        && point.Z >= bounds.MinZ && point.Z <= bounds.MinZ + bounds.SizeZ

    /// <summary>Returns the smallest axis-aligned cuboid enclosing both bounds.</summary>
    let union (a: Bounds3) (b: Bounds3) =
        let lx, ly, lz = min a.MinX b.MinX, min a.MinY b.MinY, min a.MinZ b.MinZ
        let hx = max (a.MinX + a.SizeX) (b.MinX + b.SizeX)
        let hy = max (a.MinY + a.SizeY) (b.MinY + b.SizeY)
        let hz = max (a.MinZ + a.SizeZ) (b.MinZ + b.SizeZ)
        create lx ly lz (hx - lx) (hy - ly) (hz - lz)

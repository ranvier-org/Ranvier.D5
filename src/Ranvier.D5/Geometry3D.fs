namespace Ranvier.D5

open System

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

/// <summary>Construction, distance and linear interpolation for <c>Point3</c>.</summary>
[<RequireQualifiedAccess>]
module Point3 =
    /// <summary>Creates a 3D point with finite X, Y and Z coordinates.</summary>
    let create x y z =
        Check.finite "x" x
        Check.finite "y" y
        Check.finite "z" z
        Point3(x, y, z)

    /// <summary>Returns squared Euclidean distance; very large separations may overflow to infinity.</summary>
    let distanceSquared (a: Point3) (b: Point3) =
        let dx, dy, dz = a.X - b.X, a.Y - b.Y, a.Z - b.Z
        dx * dx + dy * dy + dz * dz

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

namespace Ranvier.D5.Graph3D

open System
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif
open Ranvier.D5

/// <summary>Bounded unit-ball model used to embed 3D hyperbolic space <c>H^3</c> into Euclidean <c>R^3</c>.</summary>
[<RequireQualifiedAccess>]
type HyperbolicModel =
    /// <summary>Beltrami-Klein projective unit ball (<c>v = (x/w, y/w, z/w)</c>): hyperbolic geodesics are straight Euclidean chords (Munzner H3).</summary>
    | Klein
    /// <summary>Poincaré conformal unit ball (<c>v = (x/(1+w), y/(1+w), z/(1+w))</c>): angles are preserved and geodesics are circular arcs.</summary>
    | Poincare

/// <summary>A point on the upper sheet of the (3+1)D Minkowski hyperboloid (<c>x^2 + y^2 + z^2 - w^2 = -1, w &gt;= 1</c>).</summary>
[<Struct>]
type MinkowskiPoint =
    /// <summary>Spacelike X coordinate.</summary>
    val X: float
    /// <summary>Spacelike Y coordinate.</summary>
    val Y: float
    /// <summary>Spacelike Z coordinate.</summary>
    val Z: float
    /// <summary>Timelike W coordinate (<c>&gt;= 1</c>).</summary>
    val W: float
    internal new(x: float, y: float, z: float, w: float) = { X = x; Y = y; Z = z; W = w }

/// <summary>An isometric rigid motion of 3D hyperbolic space <c>H^3</c> represented in the orthochronous Lorentz group <c>O^+(3,1)</c>.</summary>
type HyperbolicTransform =
    private
        { M00: float; M01: float; M02: float; M03: float
          M10: float; M11: float; M12: float; M13: float
          M20: float; M21: float; M22: float; M23: float
          M30: float; M31: float; M32: float; M33: float }

/// <summary>Exact Minkowski hyperboloid, Beltrami-Klein ball, Poincaré ball and SIMD Lorentz boost operations for H3 and Walrus.</summary>
[<RequireQualifiedAccess>]
module Hyperbolic3D =
    let inline internal acoshSafe (x: float) =
        let c = max 1. x
#if FABLE_COMPILER
        log (c + sqrt ((c - 1.) * (c + 1.)))
#else
        Math.Acosh c
#endif

    let inline internal atanhSafe (x: float) =
        let c = max -0.999999999999 (min 0.999999999999 x)
#if FABLE_COMPILER
        0.5 * log ((1. + c) / (1. - c))
#else
        Math.Atanh c
#endif

    /// <summary>The hyperbolic origin <c>(0, 0, 0, 1)</c> on the Minkowski upper sheet.</summary>
    let origin = MinkowskiPoint(0., 0., 0., 1.)

    /// <summary>Projects arbitrary finite spacelike coordinates <c>(x, y, z)</c> onto the upper sheet <c>w = sqrt(1 + x^2 + y^2 + z^2)</c>.</summary>
    let ofSpacelike x y z =
        if not (Double.IsFinite x && Double.IsFinite y && Double.IsFinite z) then
            invalidArg "spacelike" "Minkowski coordinates must be finite."
        let m = max (abs x) (max (abs y) (abs z))
        let w =
            if m = 0. then 1.
            elif m > 1e150 then invalidArg "spacelike" "Hyperbolic coordinate magnitude overflows double precision."
            else
                let rx, ry, rz = x / m, y / m, z / m
                let invM = 1. / m
                m * sqrt (invM * invM + rx * rx + ry * ry + rz * rz)
        MinkowskiPoint(x, y, z, w)

    /// <summary>Re-projects a point onto the exact hyperboloid sheet <c>x^2 + y^2 + z^2 - w^2 = -1</c> to prevent numerical drift across deep trees.</summary>
    let renormalize (p: MinkowskiPoint) = ofSpacelike p.X p.Y p.Z

    /// <summary>Creates a point on <c>H^3</c> at hyperbolic geodesic distance <c>hyperbolicRadius</c> from the origin along <c>direction</c>.</summary>
    let fromSpherical (hyperbolicRadius: float) (polarPhi: float) (azimuthTheta: float) =
        if not (Double.IsFinite hyperbolicRadius && Double.IsFinite polarPhi && Double.IsFinite azimuthTheta) || hyperbolicRadius < 0. then
            invalidArg "hyperbolicRadius" "Hyperbolic radius must be finite and nonnegative."
        // Clamp extreme hyperbolic radii (> 35 gives cosh > 7.9e14, well past sub-pixel boundary visibility) so deep trees never overflow IEEE-754!
        let r = min 32. hyperbolicRadius
        let sr = sinh r
        let sp = sin polarPhi
        let x = sr * sp * cos azimuthTheta
        let y = sr * sp * sin azimuthTheta
        let z = sr * cos polarPhi
        ofSpacelike x y z

    /// <summary>Minkowski bilinear form <c>x1*x2 + y1*y2 + z1*z2 - w1*w2</c> (signature +,+,+,-).</summary>
    let minkowskiDot (a: MinkowskiPoint) (b: MinkowskiPoint) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z - a.W * b.W

    /// <summary>Exact geodesic distance in <c>H^3</c> between two points on the hyperboloid.</summary>
    let distance (a: MinkowskiPoint) (b: MinkowskiPoint) =
        acoshSafe (-(minkowskiDot a b))

    /// <summary>Projects a Minkowski hyperboloid point into the Euclidean unit ball (<c>||v|| &lt; 1</c>) under the chosen <c>HyperbolicModel</c>.</summary>
    let toBall (model: HyperbolicModel) (p: MinkowskiPoint) : Point3 * float =
        let w = max 1. p.W
        match model with
        | HyperbolicModel.Klein ->
            let invW = 1. / w
            // Clamp inside open ball (radius <= 1 - 1e-12)
            let kx, ky, kz = p.X * invW, p.Y * invW, p.Z * invW
            Point3.create kx ky kz, invW
        | HyperbolicModel.Poincare ->
            let denom = 1. + w
            let invD = 1. / denom
            let ux, uy, uz = p.X * invD, p.Y * invD, p.Z * invD
            let r2 = min (1. - 1e-15) (ux * ux + uy * uy + uz * uz)
            Point3.create ux uy uz, (1. - r2)

    /// <summary>Lifts a point strictly inside the open unit ball (<c>||v|| &lt; 1</c>) onto the Minkowski upper sheet.</summary>
    let ofBall (model: HyperbolicModel) (v: Point3) : Result<MinkowskiPoint, LayoutError list> =
        let r2 = v.X * v.X + v.Y * v.Y + v.Z * v.Z
        if not (Double.IsFinite r2) || r2 >= 1. then
            Error [ InvalidObservation "Unit-ball hyperbolic coordinate must satisfy ||v|| < 1." ]
        else
            match model with
            | HyperbolicModel.Klein ->
                let s = max 1e-15 (1. - r2)
                let w = 1. / sqrt s
                Ok(ofSpacelike (v.X * w) (v.Y * w) (v.Z * w))
            | HyperbolicModel.Poincare ->
                let d = max 1e-15 (1. - r2)
                let scale = 2. / d
                Ok(ofSpacelike (v.X * scale) (v.Y * scale) (v.Z * scale))

    /// <summary>Samples a hyperbolic geodesic segment in <c>H^3</c> projected into the chosen unit ball (2 points in Klein; smooth arc in Poincaré).</summary>
    let geodesicPoints (model: HyperbolicModel) (steps: int) (a: MinkowskiPoint) (b: MinkowskiPoint) : Point3Chain =
        match model with
        | HyperbolicModel.Klein ->
            let pa, _ = toBall HyperbolicModel.Klein a
            let pb, _ = toBall HyperbolicModel.Klein b
            Point3Chain.ofOwnedArray [| pa; pb |]
        | HyperbolicModel.Poincare ->
            let segs = max 2 (min 32 steps)
            let d = distance a b
            if d <= 1e-5 then
                let pa, _ = toBall HyperbolicModel.Poincare a
                let pb, _ = toBall HyperbolicModel.Poincare b
                Point3Chain.ofOwnedArray [| pa; pb |]
            else
                // Slerp on Minkowski hyperboloid: gamma(t) = (sinh((1-t)d)/sinh d)*a + (sinh(t*d)/sinh d)*b
                let invSinh = 1. / sinh d
                let arr = Array.zeroCreate<Point3> (segs + 1)
                for k = 0 to segs do
                    let t = float k / float segs
                    let wa = sinh ((1. - t) * d) * invSinh
                    let wb = sinh (t * d) * invSinh
                    let pt = ofSpacelike (wa * a.X + wb * b.X) (wa * a.Y + wb * b.Y) (wa * a.Z + wb * b.Z)
                    let ballPt, _ = toBall HyperbolicModel.Poincare pt
                    arr[k] <- ballPt
                Point3Chain.ofOwnedArray arr

/// <summary>Construction, composition, Minkowski re-orthonormalization and SIMD batch application for <c>HyperbolicTransform</c>.</summary>
[<RequireQualifiedAccess>]
module HyperbolicTransform =
    /// <summary>The 4x4 identity <c>O^+(3,1)</c> transform.</summary>
    let identity =
        { M00 = 1.; M01 = 0.; M02 = 0.; M03 = 0.
          M10 = 0.; M11 = 1.; M12 = 0.; M13 = 0.
          M20 = 0.; M21 = 0.; M22 = 1.; M23 = 0.
          M30 = 0.; M31 = 0.; M32 = 0.; M33 = 1. }

    /// <summary>Pure Lorentz boost mapping the origin <c>(0, 0, 0, 1)</c> isometrically onto <c>target</c> in <c>H^3</c>.</summary>
    let boostFromOrigin (target: MinkowskiPoint) : HyperbolicTransform =
        let p = Hyperbolic3D.renormalize target
        let x, y, z, w = p.X, p.Y, p.Z, p.W
        let s = 1. / (1. + w)
        { M00 = 1. + s * x * x; M01 = s * x * y;      M02 = s * x * z;      M03 = x
          M10 = s * y * x;      M11 = 1. + s * y * y; M12 = s * y * z;      M13 = y
          M20 = s * z * x;      M21 = s * z * y;      M22 = 1. + s * z * z; M23 = z
          M30 = x;              M31 = y;              M32 = z;              M33 = w }

    /// <summary>Pure Lorentz boost mapping <c>focus</c> isometrically to the hyperbolic origin <c>(0, 0, 0, 1)</c> (for interactive Focus+Context).</summary>
    let boostToOrigin (focus: MinkowskiPoint) : HyperbolicTransform =
        let p = Hyperbolic3D.renormalize focus
        boostFromOrigin (MinkowskiPoint(-p.X, -p.Y, -p.Z, p.W))

    /// <summary>Embeds a 3D Euclidean <c>Quaternion</c> rotation around the origin as an <c>O^+(3,1)</c> hyperbolic isometry.</summary>
    let ofRotation (q: Quaternion) : HyperbolicTransform =
        let nq = Quaternion.normalize q
        let x, y, z, w = nq.X, nq.Y, nq.Z, nq.W
        let xx, yy, zz = x * x, y * y, z * z
        let xy, xz, yz = x * y, x * z, y * z
        let wx, wy, wz = w * x, w * y, w * z
        { M00 = 1. - 2. * (yy + zz); M01 = 2. * (xy - wz);      M02 = 2. * (xz + wy);      M03 = 0.
          M10 = 2. * (xy + wz);      M11 = 1. - 2. * (xx + zz); M12 = 2. * (yz - wx);      M13 = 0.
          M20 = 2. * (xz - wy);      M21 = 2. * (yz + wx);      M22 = 1. - 2. * (xx + yy); M23 = 0.
          M30 = 0.;                  M31 = 0.;                  M32 = 0.;                  M33 = 1. }

    /// <summary>Composes two hyperbolic transforms in pipeline order (<c>first |&gt; thenApply second</c> computes <c>second * first</c>).</summary>
    let thenApply (second: HyperbolicTransform) (first: HyperbolicTransform) : HyperbolicTransform =
        let a, b = second, first
        { M00 = a.M00*b.M00 + a.M01*b.M10 + a.M02*b.M20 + a.M03*b.M30
          M01 = a.M00*b.M01 + a.M01*b.M11 + a.M02*b.M21 + a.M03*b.M31
          M02 = a.M00*b.M02 + a.M01*b.M12 + a.M02*b.M22 + a.M03*b.M32
          M03 = a.M00*b.M03 + a.M01*b.M13 + a.M02*b.M23 + a.M03*b.M33

          M10 = a.M10*b.M00 + a.M11*b.M10 + a.M12*b.M20 + a.M13*b.M30
          M11 = a.M10*b.M01 + a.M11*b.M11 + a.M12*b.M21 + a.M13*b.M31
          M12 = a.M10*b.M02 + a.M11*b.M12 + a.M12*b.M22 + a.M13*b.M32
          M13 = a.M10*b.M03 + a.M11*b.M13 + a.M12*b.M23 + a.M13*b.M33

          M20 = a.M20*b.M00 + a.M21*b.M10 + a.M22*b.M20 + a.M23*b.M30
          M21 = a.M20*b.M01 + a.M21*b.M11 + a.M22*b.M21 + a.M23*b.M31
          M22 = a.M20*b.M02 + a.M21*b.M12 + a.M22*b.M22 + a.M23*b.M32
          M23 = a.M20*b.M03 + a.M21*b.M13 + a.M22*b.M23 + a.M23*b.M33

          M30 = a.M30*b.M00 + a.M31*b.M10 + a.M32*b.M20 + a.M33*b.M30
          M31 = a.M30*b.M01 + a.M31*b.M11 + a.M32*b.M21 + a.M33*b.M31
          M32 = a.M30*b.M02 + a.M31*b.M12 + a.M32*b.M22 + a.M33*b.M32
          M33 = a.M30*b.M03 + a.M31*b.M13 + a.M32*b.M23 + a.M33*b.M33 }

    /// <summary>Re-orthonormalizes a transform in <c>O^+(3,1)</c> via Minkowski Gram-Schmidt to eliminate accumulated navigation drift.</summary>
    let orthonormalize (m: HyperbolicTransform) : HyperbolicTransform =
        let mDot (ax, ay, az, aw) (bx, by, bz, bw) = ax * bx + ay * by + az * bz - aw * bw
        // Timelike 4th column c3 satisfies <c3, c3>_M = -1, w > 0
        let c3Raw = Hyperbolic3D.ofSpacelike m.M03 m.M13 m.M23
        let c3 = (c3Raw.X, c3Raw.Y, c3Raw.Z, c3Raw.W)

        let normSpacelike (vx, vy, vz, vw) =
            let n2 = mDot (vx, vy, vz, vw) (vx, vy, vz, vw)
            if n2 <= 1e-20 then (1., 0., 0., 0.)
            else
                let inv = 1. / sqrt n2
                (vx * inv, vy * inv, vz * inv, vw * inv)

        let subScale (ax, ay, az, aw) s (bx, by, bz, bw) =
            (ax - s * bx, ay - s * by, az - s * bz, aw - s * bw)

        // c0: remove timelike projection (- <v, c3>_M * c3 since <c3, c3>_M = -1 => v + <v, c3>_M * c3)
        let mutable v0 = (m.M00, m.M10, m.M20, m.M30)
        v0 <- subScale v0 (-(mDot v0 c3)) c3
        let c0 = normSpacelike v0

        let mutable v1 = (m.M01, m.M11, m.M21, m.M31)
        v1 <- subScale v1 (-(mDot v1 c3)) c3
        v1 <- subScale v1 (mDot v1 c0) c0
        let c1 = normSpacelike v1

        let mutable v2 = (m.M02, m.M12, m.M22, m.M32)
        v2 <- subScale v2 (-(mDot v2 c3)) c3
        v2 <- subScale v2 (mDot v2 c0) c0
        v2 <- subScale v2 (mDot v2 c1) c1
        let c2 = normSpacelike v2

        let c0x, c0y, c0z, c0w = c0
        let c1x, c1y, c1z, c1w = c1
        let c2x, c2y, c2z, c2w = c2
        let c3x, c3y, c3z, c3w = c3
        { M00 = c0x; M01 = c1x; M02 = c2x; M03 = c3x
          M10 = c0y; M11 = c1y; M12 = c2y; M13 = c3y
          M20 = c0z; M21 = c1z; M22 = c2z; M23 = c3z
          M30 = c0w; M31 = c1w; M32 = c2w; M33 = c3w }

    /// <summary>Applies <c>transform</c> to a single <c>MinkowskiPoint</c> and renormalizes onto the upper sheet.</summary>
    let apply (m: HyperbolicTransform) (p: MinkowskiPoint) : MinkowskiPoint =
        let nx = m.M00 * p.X + m.M01 * p.Y + m.M02 * p.Z + m.M03 * p.W
        let ny = m.M10 * p.X + m.M11 * p.Y + m.M12 * p.Z + m.M13 * p.W
        let nz = m.M20 * p.X + m.M21 * p.Y + m.M22 * p.Z + m.M23 * p.W
        Hyperbolic3D.ofSpacelike nx ny nz

    /// <summary>SIMD-accelerated SoA batch Lorentz transformation of <c>count</c> Minkowski points in-place into destination buffers.</summary>
    let internal applySoA
        (m: HyperbolicTransform)
        (sx: float[]) (sy: float[]) (sz: float[]) (sw: float[])
        (dx: float[]) (dy: float[]) (dz: float[]) (dw: float[])
        (count: int)
        : unit =
#if FABLE_COMPILER
        for i = 0 to count - 1 do
            let x, y, z, w = sx[i], sy[i], sz[i], sw[i]
            let nx = m.M00 * x + m.M01 * y + m.M02 * z + m.M03 * w
            let ny = m.M10 * x + m.M11 * y + m.M12 * z + m.M13 * w
            let nz = m.M20 * x + m.M21 * y + m.M22 * z + m.M23 * w
            let nw = sqrt (1. + nx * nx + ny * ny + nz * nz)
            dx[i] <- nx; dy[i] <- ny; dz[i] <- nz; dw[i] <- nw
#else
        if count > 0 then
            let rsx = &MemoryMarshal.GetArrayDataReference(sx)
            let rsy = &MemoryMarshal.GetArrayDataReference(sy)
            let rsz = &MemoryMarshal.GetArrayDataReference(sz)
            let rsw = &MemoryMarshal.GetArrayDataReference(sw)
            let rdx = &MemoryMarshal.GetArrayDataReference(dx)
            let rdy = &MemoryMarshal.GetArrayDataReference(dy)
            let rdz = &MemoryMarshal.GetArrayDataReference(dz)
            let rdw = &MemoryMarshal.GetArrayDataReference(dw)
            let mutable i = 0u
            let ulen = uint32 count

            if Vector512.IsHardwareAccelerated && Vector512<float>.IsSupported then
                let step = uint32 Vector512<float>.Count
                let m00, m01, m02, m03 = Vector512.Create m.M00, Vector512.Create m.M01, Vector512.Create m.M02, Vector512.Create m.M03
                let m10, m11, m12, m13 = Vector512.Create m.M10, Vector512.Create m.M11, Vector512.Create m.M12, Vector512.Create m.M13
                let m20, m21, m22, m23 = Vector512.Create m.M20, Vector512.Create m.M21, Vector512.Create m.M22, Vector512.Create m.M23
                let one = Vector512.Create 1.
                while i + step <= ulen do
                    let off = unativeint i
                    let vx = Vector512.LoadUnsafe(&rsx, off)
                    let vy = Vector512.LoadUnsafe(&rsy, off)
                    let vz = Vector512.LoadUnsafe(&rsz, off)
                    let vw = Vector512.LoadUnsafe(&rsw, off)
                    let nx = m00 * vx + m01 * vy + m02 * vz + m03 * vw
                    let ny = m10 * vx + m11 * vy + m12 * vz + m13 * vw
                    let nz = m20 * vx + m21 * vy + m22 * vz + m23 * vw
                    let nw = Vector512.Sqrt(one + nx * nx + ny * ny + nz * nz)
                    nx.StoreUnsafe(&rdx, off)
                    ny.StoreUnsafe(&rdy, off)
                    nz.StoreUnsafe(&rdz, off)
                    nw.StoreUnsafe(&rdw, off)
                    i <- i + step
            elif Vector256.IsHardwareAccelerated && Vector256<float>.IsSupported then
                let step = uint32 Vector256<float>.Count
                let m00, m01, m02, m03 = Vector256.Create m.M00, Vector256.Create m.M01, Vector256.Create m.M02, Vector256.Create m.M03
                let m10, m11, m12, m13 = Vector256.Create m.M10, Vector256.Create m.M11, Vector256.Create m.M12, Vector256.Create m.M13
                let m20, m21, m22, m23 = Vector256.Create m.M20, Vector256.Create m.M21, Vector256.Create m.M22, Vector256.Create m.M23
                let one = Vector256.Create 1.
                while i + step <= ulen do
                    let off = unativeint i
                    let vx = Vector256.LoadUnsafe(&rsx, off)
                    let vy = Vector256.LoadUnsafe(&rsy, off)
                    let vz = Vector256.LoadUnsafe(&rsz, off)
                    let vw = Vector256.LoadUnsafe(&rsw, off)
                    let nx = m00 * vx + m01 * vy + m02 * vz + m03 * vw
                    let ny = m10 * vx + m11 * vy + m12 * vz + m13 * vw
                    let nz = m20 * vx + m21 * vy + m22 * vz + m23 * vw
                    let nw = Vector256.Sqrt(one + nx * nx + ny * ny + nz * nz)
                    nx.StoreUnsafe(&rdx, off)
                    ny.StoreUnsafe(&rdy, off)
                    nz.StoreUnsafe(&rdz, off)
                    nw.StoreUnsafe(&rdw, off)
                    i <- i + step
            elif Vector128.IsHardwareAccelerated && Vector128<float>.IsSupported then
                let step = uint32 Vector128<float>.Count
                let m00, m01, m02, m03 = Vector128.Create m.M00, Vector128.Create m.M01, Vector128.Create m.M02, Vector128.Create m.M03
                let m10, m11, m12, m13 = Vector128.Create m.M10, Vector128.Create m.M11, Vector128.Create m.M12, Vector128.Create m.M13
                let m20, m21, m22, m23 = Vector128.Create m.M20, Vector128.Create m.M21, Vector128.Create m.M22, Vector128.Create m.M23
                let one = Vector128.Create 1.
                while i + step <= ulen do
                    let off = unativeint i
                    let vx = Vector128.LoadUnsafe(&rsx, off)
                    let vy = Vector128.LoadUnsafe(&rsy, off)
                    let vz = Vector128.LoadUnsafe(&rsz, off)
                    let vw = Vector128.LoadUnsafe(&rsw, off)
                    let nx = m00 * vx + m01 * vy + m02 * vz + m03 * vw
                    let ny = m10 * vx + m11 * vy + m12 * vz + m13 * vw
                    let nz = m20 * vx + m21 * vy + m22 * vz + m23 * vw
                    let nw = Vector128.Sqrt(one + nx * nx + ny * ny + nz * nz)
                    nx.StoreUnsafe(&rdx, off)
                    ny.StoreUnsafe(&rdy, off)
                    nz.StoreUnsafe(&rdz, off)
                    nw.StoreUnsafe(&rdw, off)
                    i <- i + step

            let mutable idx = int i
            while idx < count do
                let x, y, z, w = sx[idx], sy[idx], sz[idx], sw[idx]
                let nx = m.M00 * x + m.M01 * y + m.M02 * z + m.M03 * w
                let ny = m.M10 * x + m.M11 * y + m.M12 * z + m.M13 * w
                let nz = m.M20 * x + m.M21 * y + m.M22 * z + m.M23 * w
                let p = Hyperbolic3D.ofSpacelike nx ny nz
                dx[idx] <- p.X; dy[idx] <- p.Y; dz[idx] <- p.Z; dw[idx] <- p.W
                idx <- idx + 1
#endif

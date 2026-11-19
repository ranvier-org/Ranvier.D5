namespace Ranvier.D5.Wgpu

open System
open System.Numerics
#if !FABLE_COMPILER
open System.Runtime.CompilerServices
#endif
open Ranvier.D5

/// <summary>Internal finite and domain-validation helpers shared across 3D modules.</summary>
module internal Check3D =
#if FABLE_COMPILER
    /// <summary>True when a 64-bit float is neither NaN nor infinite (Fable-compatible).</summary>
    let inline isFinite (value: float) = value - value = 0.0

    /// <summary>True when a 32-bit float is neither NaN nor infinite (Fable-compatible).</summary>
    let inline isFinite32 (value: float32) = value - value = 0.0f
#else
    /// <summary>True when a 64-bit float is neither NaN nor infinite (Fable-compatible).</summary>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let isFinite (value: float) = value - value = 0.0

    /// <summary>True when a 32-bit float is neither NaN nor infinite (Fable-compatible).</summary>
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let isFinite32 (value: float32) = value - value = 0.0f
#endif

    let finite (name: string) (value: float) =
        if not (isFinite value) then
            invalidArg name "Expected a finite number."

    let finite32 (name: string) (value: float32) =
        if not (isFinite32 value) then
            invalidArg name "Expected a finite single-precision number."

    let positive (name: string) (value: float) =
        if not (isFinite value) || value <= 0.0 then
            raise (ArgumentOutOfRangeException (name, "Expected a finite positive number."))

    let positive32 (name: string) (value: float32) =
        if not (isFinite32 value) || value <= 0.0f then
            raise (ArgumentOutOfRangeException (name, "Expected a finite positive single-precision number."))

    let nonNegative (name: string) (value: float) =
        if not (isFinite value) || value < 0.0 then
            raise (ArgumentOutOfRangeException (name, "Expected a finite nonnegative number."))

    let nonNegative32 (name: string) (value: float32) =
        if not (isFinite32 value) || value < 0.0f then
            raise (ArgumentOutOfRangeException (name, "Expected a finite nonnegative single-precision number."))

    let vector3 (name: string) (v: Vector3) =
        if not (isFinite32 v.X && isFinite32 v.Y && isFinite32 v.Z) then
            invalidArg name "Expected a finite 3D vector."

    let interval (name: string) (a: float, b: float) =
        finite name a
        finite name b
        finite name (b - a)

/// <summary>Type extensions adapting <c>Ranvier.D5</c> 3D spatial values to <c>System.Numerics</c> GPU vectors and bounding spheres.</summary>
[<AutoOpen>]
module Geometry3DExtensions =
    type Point3 with
        /// <summary>Converts a finite <c>Point3</c> into a single-precision <c>System.Numerics.Vector3</c>; throws <c>ArgumentException</c> when any coordinate overflows <c>float32</c>.</summary>
        static member toVector3f(point: Point3) : Vector3 =
            let v = Vector3 (float32 point.X, float32 point.Y, float32 point.Z)
            Check3D.vector3 (nameof point) v
            v

        /// <summary>Converts a finite <c>System.Numerics.Vector3</c> into a <c>Point3</c>; throws <c>ArgumentException</c> for non-finite components.</summary>
        static member ofVector3f(vector: Vector3) : Point3 =
            Point3.create (float vector.X) (float vector.Y) (float vector.Z)

    type Bounds3 with
        /// <summary>Returns the overflow-protected half-diagonal bounding-sphere radius of a <c>Bounds3</c>.</summary>
        static member boundingRadius(bounds: Bounds3) : float =
            let sx = Bounds3.sizeX bounds
            let sy = Bounds3.sizeY bounds
            let sz = Bounds3.sizeZ bounds
            let m = max sx (max sy sz)

            if m = 0.0 then
                0.0
            else
                let rx = sx / m
                let ry = sy / m
                let rz = sz / m
                0.5 * m * sqrt (rx * rx + ry * ry + rz * rz)

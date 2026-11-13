namespace Ranvier.D5.Geo

open System

open Ranvier.D5

/// <summary>Already planar geometry with straight edges and unrestricted finite Point coordinates.</summary>
/// <remarks>Polygon rings are simple and close explicitly: the first ring is the exterior and later rings are enclosed holes with opposite winding. Reversing every ring preserves its filled region. Viewport clipping normalizes the exterior winding; it never adds a spherical complement.</remarks>
[<RequireQualifiedAccess>]
type PlanarGeometry =
    | Point of Point option
    | MultiPoint of Point list
    | LineString of Point list
    | MultiLineString of Point list list
    | Polygon of Point list list
    | MultiPolygon of Point list list list
    | Collection of PlanarGeometry list

/// <summary>Planar scale and reflections, followed by an angle in degrees and screen translation.</summary>
/// <remarks>Positive angle rotates counterclockwise in screen coordinates, matching D3 geoIdentity. Scale must be positive and finite.</remarks>
type IdentityProjectionConfig = {
    /// <summary>Positive finite scale applied to both planar axes.</summary>
    Scale: float
    /// <summary>Final translation in output coordinate units.</summary>
    Translate: Point
    /// <summary>Counterclockwise screen rotation in degrees, after scaling and reflection.</summary>
    AngleDegrees: float
    /// <summary>Negates the input X coordinate before rotation.</summary>
    ReflectX: bool
    /// <summary>Negates the input Y coordinate before rotation.</summary>
    ReflectY: bool
}

/// <summary>Defaults for an unchanged planar coordinate system.</summary>
module IdentityProjectionConfig =
    /// <summary>Unit scale, zero translation and angle, and no reflection.</summary>
    let defaultValue = {Scale=1.;Translate=Point.create 0. 0.;AngleDegrees=0.;ReflectX=false;ReflectY=false}

/// <summary>An immutable planar affine projection and its optional representable inverse.</summary>
type IdentityProjection = private {Forward:Transform;Backward:Transform option}

/// <summary>Projects already planar coordinates without geographic clipping or resampling.</summary>
module IdentityProjection =
    /// <summary>Validates a finite planar projection configuration.</summary>
    let create (config:IdentityProjectionConfig) : Result<IdentityProjection,LayoutError list> =
        if not(Double.IsFinite config.Scale) || config.Scale<=0. ||
           not(Double.IsFinite config.AngleDegrees) ||
           not(Double.IsFinite config.Translate.X && Double.IsFinite config.Translate.Y) then
            Error [InvalidObservation "Planar identity requires positive finite scale, finite angle and translation."]
        else
            let struct(sine,cosine)=GeoMath.sinCosDegrees(-config.AngleDegrees)
            let x=config.Scale*(if config.ReflectX then -1. else 1.)
            let y=config.Scale*(if config.ReflectY then -1. else 1.)
            let matrix=Transform.create (x*cosine) (x*sine) (-y*sine) (y*cosine) config.Translate.X config.Translate.Y
            Ok {Forward=matrix;Backward=Transform.tryInverse matrix}

    let internal projectValue projection (point:Point) =
        if not(Double.IsFinite point.X && Double.IsFinite point.Y) then ValueNone
        else
            try ValueSome(Transform.apply projection.Forward point)
            with :? System.ArgumentException -> ValueNone
    /// <summary>Returns a transformed point, or None when its input or result is nonfinite.</summary>
    let project projection point = projectValue projection point |> ValueOption.toOption
    /// <summary>Returns an inverse point, or None when the inverse or result is not representable.</summary>
    let invert projection (point:Point) =
        if not(Double.IsFinite point.X && Double.IsFinite point.Y) then None
        else
            projection.Backward |> Option.bind(fun matrix ->
                try Some(Transform.apply matrix point)
                with :? System.ArgumentException -> None)

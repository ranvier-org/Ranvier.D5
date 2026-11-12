namespace Ranvier.D5.Geo

open System
open Ranvier.D5

/// <summary>A finite longitude and latitude in degrees, with latitude in [-90,90].</summary>
[<Struct>]
type GeoPosition =
    private { LongitudeDegrees: float; LatitudeDegrees: float }
    /// <summary>The original longitude in degrees, including complete turns.</summary>
    member this.Longitude = this.LongitudeDegrees
    /// <summary>The latitude in degrees in [-90,90].</summary>
    member this.Latitude = this.LatitudeDegrees

/// <summary>Validated geographic coordinates and explicit longitude normalization.</summary>
module GeoPosition =
    /// <summary>Validates finite degree coordinates and latitude in [-90,90]; preserves longitude turns.</summary>
    let create longitude latitude : Result<GeoPosition, LayoutError list> =
        if not(Double.IsFinite longitude && Double.IsFinite latitude) || latitude < -90. || latitude > 90. then
            Error [InvalidObservation "Geographic coordinates require finite degrees and latitude in [-90,90]."]
        else Ok { LongitudeDegrees=longitude; LatitudeDegrees=latitude }

    /// <summary>Returns the same location with longitude in [-180,180).</summary>
    let normalize position =
        let remainder = position.LongitudeDegrees % 360.
        let longitude =
            if remainder >= 180. then remainder - 360.
            elif remainder < -180. then remainder + 360.
            else remainder
        { position with LongitudeDegrees=longitude }

/// <summary>Angular measurements on a unit sphere.</summary>
module Geodesic =
    /// <summary>Returns the shortest great-circle distance in radians, in [0,pi].</summary>
    let distance (from:GeoPosition) (destination:GeoPosition) =
        let radians = Math.PI / 180.
        let start = GeoPosition.normalize from
        let finish = GeoPosition.normalize destination
        let delta = (finish.Longitude - start.Longitude) * radians
        let phi0,phi1 = start.Latitude * radians,finish.Latitude * radians
        let sin0,cos0 = sin phi0,cos phi0
        let sin1,cos1 = sin phi1,cos phi1
        let half = sin(delta / 2.)
        let x = cos1 * sin delta
        let y = sin((finish.Latitude - start.Latitude) * radians) + sin0 * cos1 * (2. * half * half)
        atan2 (Double.Hypot(x,y)) (sin0 * sin1 + cos0 * cos1 * cos delta)

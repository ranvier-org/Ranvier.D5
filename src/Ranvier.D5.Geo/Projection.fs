namespace Ranvier.D5.Geo

open System
open Ranvier.D5

/// <summary>The spherical mapping used before screen scale and translation.</summary>
[<RequireQualifiedAccess>]
type ProjectionKind =
    /// <summary>Longitude and latitude in radians.</summary>
    | Equirectangular
    /// <summary>Cylindrical conformal mapping, undefined at either pole.</summary>
    | Mercator
    /// <summary>The visible hemisphere projected onto its equatorial disk.</summary>
    | Orthographic

/// <summary>Immutable point-projection parameters; create a Projection to validate them.</summary>
type ProjectionConfig = {
    /// <summary>The spherical mapping.</summary>
    Kind: ProjectionKind
    /// <summary>Positive finite screen units per raw projected unit.</summary>
    Scale: float
    /// <summary>Screen location of the projected center; positive Y points down.</summary>
    Translate: Point
    /// <summary>Unrotated geographic position whose raw projection defines the screen offset.</summary>
    Center: GeoPosition
    /// <summary>Spherical rotation applied to input positions before projection.</summary>
    Rotation: GeoRotation
}

/// <summary>Defaults for point projections; stream clipping and resampling are separate policies.</summary>
module ProjectionConfig =
    /// <summary>Uses unit scale, zero screen translation, the geographic origin and no rotation.</summary>
    let defaultValue kind = {
        Kind=kind; Scale=1.; Translate=Point.create 0. 0.
        Center=GeoPosition.create 0. 0. |> Result.defaultWith (fun _ -> failwith "origin")
        Rotation=GeoRotation.create 0. 0. 0. |> Result.defaultWith (fun _ -> failwith "identity")
    }

/// <summary>A validated immutable projection with a cached raw center.</summary>
type Projection = private { Config:ProjectionConfig; CenterX:float; CenterY:float }

/// <summary>Pure point projections and inverse mappings; these do not clip or rejoin polygon edges.</summary>
module Projection =
    let private radians = Math.PI / 180.
    let private degrees = 180. / Math.PI

    let private raw kind (position:GeoPosition) =
        let canonical=position
        match kind with
        | ProjectionKind.Equirectangular ->
            ValueSome (struct(canonical.Longitude*radians, canonical.Latitude*radians))
        | ProjectionKind.Mercator ->
            if abs canonical.Latitude = 90. then ValueNone
            else ValueSome (struct(canonical.Longitude*radians, Math.Asinh(tan(canonical.Latitude*radians))))
        | ProjectionKind.Orthographic ->
            let struct(sinLongitude,_)=GeoMath.sinCosDegrees canonical.Longitude
            let struct(sinLatitude,cosLatitude)=GeoMath.sinCosDegrees canonical.Latitude
            ValueSome (struct(cosLatitude*sinLongitude,sinLatitude))

    /// <summary>Validates scale, translation and the projection center; Mercator centers cannot be poles.</summary>
    let create config : Result<Projection,LayoutError list> =
        if not(Double.IsFinite config.Scale) || config.Scale <= 0. ||
           not(Double.IsFinite config.Translate.X && Double.IsFinite config.Translate.Y) then
            Error [InvalidObservation "Projection requires positive finite scale and finite screen translation."]
        else
            match raw config.Kind (GeoPosition.normalize config.Center) with
            | ValueNone -> Error [InvalidObservation "The projection center is singular."]
            | ValueSome struct(x,y) -> Ok {Config=config;CenterX=x;CenterY=y}

    let internal rotate projection position = GeoRotation.apply projection.Config.Rotation position

    let internal kind projection = projection.Config.Kind

    let internal projectRotatedValue projection (rotated:GeoPosition) =
        let config=projection.Config
        let visible =
            match config.Kind with
            | ProjectionKind.Orthographic ->
                let struct(_,cosLongitude)=GeoMath.sinCosDegrees rotated.Longitude
                let struct(_,cosLatitude)=GeoMath.sinCosDegrees rotated.Latitude
                cosLongitude*cosLatitude >= 0.
            | _ -> true
        if not visible then ValueNone
        else
            match raw config.Kind rotated with
            | ValueNone -> ValueNone
            | ValueSome struct(x,y) ->
                let screenX=config.Translate.X + config.Scale*(x-projection.CenterX)
                let screenY=config.Translate.Y - config.Scale*(y-projection.CenterY)
                if Double.IsFinite screenX && Double.IsFinite screenY then ValueSome {X=screenX;Y=screenY}
                else ValueNone

    let internal projectValue projection position = projectRotatedValue projection (rotate projection position)

    /// <summary>Maps a position to screen coordinates, or None at a singularity, behind the orthographic limb, or on overflow.</summary>
    let project projection position = projectValue projection position |> ValueOption.toOption

    /// <summary>Inverts a finite screen point within the raw mapping domain, then undoes rotation.</summary>
    /// <remarks>Rejects points outside the canonical cylindrical world or orthographic disk, and Mercator latitudes that round to a singular pole. Orthographic inversion admits eight unit-roundoff steps beyond the limb to absorb screen-transform rounding.</remarks>
    let tryInvert projection (point:Point) =
        let config=projection.Config
        let x=(point.X-config.Translate.X)/config.Scale + projection.CenterX
        let y= -(point.Y-config.Translate.Y)/config.Scale + projection.CenterY
        if not(Double.IsFinite x && Double.IsFinite y) then None
        else
            let geographic =
                match config.Kind with
                | ProjectionKind.Equirectangular ->
                    if abs x > Math.PI || abs y > Math.PI/2. then None
                    else Some(struct(x*degrees,y*degrees))
                | ProjectionKind.Mercator ->
                    let latitude=atan(Math.Sinh y)*degrees
                    if abs x > Math.PI || abs latitude >= 90. then None
                    else Some(struct(x*degrees,latitude))
                | ProjectionKind.Orthographic ->
                    let radius=Double.Hypot(x,y)
                    if radius > 1. + 8.*(Math.BitIncrement(1.)-1.) then None
                    else
                        let divisor=max 1. radius
                        let boundedRadius=min 1. radius
                        let z=sqrt(max 0. ((1.-boundedRadius)*(1.+boundedRadius)))
                        Some(struct(atan2 (x/divisor) z*degrees,asin (y/divisor)*degrees))
            match geographic with
            | None -> None
            | Some struct(longitude,latitude) ->
                GeoPosition.create longitude latitude
                |> Result.toOption
                |> Option.map (GeoRotation.inverse config.Rotation)

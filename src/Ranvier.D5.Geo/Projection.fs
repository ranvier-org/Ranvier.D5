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
    /// <summary>Lambert azimuthal equal-area; radius 2 sin(c/2) at angular distance c, singular at the antipode.</summary>
    | AzimuthalEqualArea
    /// <summary>Azimuthal equidistant; radius equals angular distance from the center, singular at the antipode.</summary>
    | AzimuthalEquidistant
    /// <summary>Central projection with radius tan c; great circles map to straight lines and the horizon is singular.</summary>
    | Gnomonic
    /// <summary>Conformal azimuthal projection with radius tan(c/2), singular at the antipode.</summary>
    | Stereographic

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
    /// <summary>Visible cap radius in degrees for orthographic and azimuthal kinds; None selects the D3 default for the kind. Cylindrical kinds cut at the antimeridian and require None.</summary>
    ClipAngle: float option
}

/// <summary>Defaults for point projections; stream clipping and resampling are separate policies.</summary>
module ProjectionConfig =
    /// <summary>Uses unit scale, zero screen translation, the geographic origin and no rotation.</summary>
    let defaultValue kind = {
        Kind=kind; Scale=1.; Translate=Point.create 0. 0.
        Center=GeoPosition.create 0. 0. |> Result.defaultWith (fun _ -> failwith "origin")
        Rotation=GeoRotation.create 0. 0. 0. |> Result.defaultWith (fun _ -> failwith "identity")
        ClipAngle=None
    }

/// <summary>A validated immutable projection with a cached raw center and visible cap.</summary>
type Projection = private { Config:ProjectionConfig; CenterX:float; CenterY:float; ClipSine:float; ClipCosine:float; ClipRadius:float }

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
        | ProjectionKind.AzimuthalEqualArea | ProjectionKind.AzimuthalEquidistant | ProjectionKind.Gnomonic | ProjectionKind.Stereographic ->
            let struct(sinLongitude,cosLongitude)=GeoMath.sinCosDegrees canonical.Longitude
            let struct(sinLatitude,cosLatitude)=GeoMath.sinCosDegrees canonical.Latitude
            let cosine=cosLatitude*cosLongitude
            // Radial scale k(cos c) as in the d3-geo azimuthal raw projections.
            let k =
                match kind with
                | ProjectionKind.AzimuthalEqualArea -> sqrt(2./(1.+cosine))
                | ProjectionKind.AzimuthalEquidistant ->
                    let c=acos(Math.Clamp(cosine,-1.,1.))
                    if c=0. then 1. else c/sin c
                | ProjectionKind.Gnomonic -> 1./cosine
                | _ -> 1./(1.+cosine)
            if cosine <= -1. || (kind=ProjectionKind.Gnomonic && cosine<=0.) || not(Double.IsFinite k) then ValueNone
            else ValueSome (struct(k*cosLatitude*sinLongitude,k*sinLatitude))

    let private defaultClipAngle kind =
        match kind with
        | ProjectionKind.Orthographic -> ValueSome 90.
        | ProjectionKind.Gnomonic -> ValueSome 60.
        | ProjectionKind.Stereographic -> ValueSome 142.
        | ProjectionKind.AzimuthalEqualArea | ProjectionKind.AzimuthalEquidistant -> ValueSome(180.-1e-3)
        | _ -> ValueNone

    let private validClipAngle kind angle =
        Double.IsFinite angle && angle>0. &&
        match kind with
        | ProjectionKind.Orthographic -> angle<=90.
        | ProjectionKind.Gnomonic -> angle<90.
        | _ -> angle<180.

    /// <summary>Validates scale, translation, the projection center and clip angle; Mercator centers cannot be poles.</summary>
    /// <remarks>Default clip angles follow d3-geo: orthographic 90, gnomonic 60, stereographic 142, and azimuthal equal-area and equidistant 179.999 degrees.</remarks>
    let create config : Result<Projection,LayoutError list> =
        if not(Double.IsFinite config.Scale) || config.Scale <= 0. ||
           not(Double.IsFinite config.Translate.X && Double.IsFinite config.Translate.Y) then
            Error [InvalidObservation "Projection requires positive finite scale and finite screen translation."]
        else
            let clip =
                match defaultClipAngle config.Kind,config.ClipAngle with
                | ValueNone,Some _ -> Error [InvalidObservation "Cylindrical projections cut at the antimeridian and take no clip angle."]
                | ValueNone,None -> Ok ValueNone
                | ValueSome _,Some angle when not(validClipAngle config.Kind angle) ->
                    Error [InvalidObservation "Clip angle must be positive and finite, at most 90 degrees orthographic, below 90 gnomonic and below 180 otherwise."]
                | ValueSome fallback,angle -> Ok(ValueSome(Option.defaultValue fallback angle))
            match clip with
            | Error errors -> Error errors
            | Ok clip ->
                match raw config.Kind (GeoPosition.normalize config.Center) with
                | ValueNone -> Error [InvalidObservation "The projection center is singular."]
                | ValueSome struct(x,y) ->
                    let struct(sine,cosine)=clip |> ValueOption.map GeoMath.sinCosDegrees |> ValueOption.defaultValue struct(0.,1.)
                    let radius=clip |> ValueOption.map(fun angle -> angle*radians) |> ValueOption.defaultValue nan
                    Ok {Config=config;CenterX=x;CenterY=y;ClipSine=sine;ClipCosine=cosine;ClipRadius=radius}

    let internal rotate projection position = GeoRotation.apply projection.Config.Rotation position

    let internal kind projection = projection.Config.Kind
    let internal scale projection = projection.Config.Scale
    /// Angular radius in radians, sine and cosine of the visible cap, for projections clipped by a small circle.
    let internal clipCircle projection =
        if Double.IsNaN projection.ClipRadius then ValueNone
        else ValueSome struct(projection.ClipRadius,projection.ClipSine,projection.ClipCosine)
    /// Raw-plane radius of the visible cap boundary, which azimuthal symmetry maps to a circle about the center.
    let internal clipPlaneRadius projection =
        match projection.Config.Kind with
        | ProjectionKind.Orthographic -> projection.ClipSine
        | ProjectionKind.AzimuthalEqualArea -> 2.*sin(projection.ClipRadius/2.)
        | ProjectionKind.AzimuthalEquidistant -> projection.ClipRadius
        | ProjectionKind.Gnomonic -> projection.ClipSine/projection.ClipCosine
        | ProjectionKind.Stereographic -> projection.ClipSine/(1.+projection.ClipCosine)
        | _ -> nan

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
    /// <remarks>Rejects points outside the canonical cylindrical world, the orthographic disk or the finite equal-area and equidistant disks, and Mercator latitudes that round to a singular pole. Bounded disks admit eight unit-roundoff steps beyond their edge to absorb screen-transform rounding.</remarks>
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
                | ProjectionKind.AzimuthalEqualArea | ProjectionKind.AzimuthalEquidistant | ProjectionKind.Gnomonic | ProjectionKind.Stereographic ->
                    let radius=Double.Hypot(x,y)
                    let slack=1.+8.*(Math.BitIncrement(1.)-1.)
                    // Angular distance from the center, as in the d3-geo azimuthal inverses.
                    let angle =
                        match config.Kind with
                        | ProjectionKind.AzimuthalEqualArea -> if radius>2.*slack then nan else 2.*asin(min 1. (radius/2.))
                        | ProjectionKind.AzimuthalEquidistant -> if radius>Math.PI*slack then nan else min Math.PI radius
                        | ProjectionKind.Gnomonic -> atan radius
                        | _ -> 2.*atan radius
                    if Double.IsNaN angle then None
                    else
                        let sine,cosine=sin angle,cos angle
                        let latitude=if radius=0. then 0. else asin(Math.Clamp(y*sine/radius,-1.,1.))
                        Some(struct(atan2 (x*sine) (radius*cosine)*degrees,latitude*degrees))
            match geographic with
            | None -> None
            | Some struct(longitude,latitude) ->
                GeoPosition.create longitude latitude
                |> Result.toOption
                |> Option.map (GeoRotation.inverse config.Rotation)

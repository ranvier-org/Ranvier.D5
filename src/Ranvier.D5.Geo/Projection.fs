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
    /// <summary>Lambert conformal conic with two standard parallels in degrees; the pole away from the apex is clamped one microradian short, as in D3.</summary>
    | ConicConformal of firstParallel:float * secondParallel:float
    /// <summary>Albers equal-area conic with two standard parallels in degrees.</summary>
    | ConicEqualArea of firstParallel:float * secondParallel:float
    /// <summary>Equidistant conic with two standard parallels in degrees.</summary>
    | ConicEquidistant of firstParallel:float * secondParallel:float
    /// <summary>Mercator about a rotated axis, with the D3 extra 90 degree roll and swapped center; its rotated poles are singular.</summary>
    | TransverseMercator

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

/// <summary>A validated immutable projection with a cached raw center, visible cap and conic constants.</summary>
type Projection = private {
    Config:ProjectionConfig; CenterX:float; CenterY:float; ClipSine:float; ClipCosine:float; ClipRadius:float
    Roll:GeoRotation voption; ConicN:float; ConicA:float; ConicB:float }

/// <summary>Pure point projections and inverse mappings; these do not clip or rejoin polygon edges.</summary>
module Projection =
    let private radians = Math.PI / 180.
    let private degrees = 180. / Math.PI

    let private halfPi=Math.PI/2.
    let private tanHalf latitude=tan((halfPi+latitude)/2.)

    // Constants of the d3-geo conic raw projections: exponent n plus the two scalars each family needs.
    let private conicConstants kind =
        let finite a b=Double.IsFinite a && Double.IsFinite b && abs a<=90. && abs b<=90.
        match kind with
        | ProjectionKind.ConicConformal(a,b) when finite a b ->
            let phi0,phi1=a*radians,b*radians
            if abs a=90. || abs b=90. then Error "Conformal conic parallels cannot lie on a pole."
            else
                let cy0=cos phi0
                let n=if phi0=phi1 then sin phi0 else log(cy0/cos phi1)/log(tanHalf phi1/tanHalf phi0)
                if not(Double.IsFinite n) || abs n<1e-6 then Error "Conformal conic parallels describe a cylinder; use Mercator."
                else Ok(struct(n,cy0*Math.Pow(tanHalf phi0,n)/n,0.))
        | ProjectionKind.ConicEqualArea(a,b) when finite a b ->
            let sy0=sin(a*radians)
            let n=(sy0+sin(b*radians))/2.
            if abs n<1e-6 then Error "Equal-area conic parallels describe a cylinder."
            else
                let c=1.+sy0*(2.*n-sy0)
                Ok(struct(n,c,sqrt c/n))
        | ProjectionKind.ConicEquidistant(a,b) when finite a b ->
            let phi0,phi1=a*radians,b*radians
            let cy0=cos phi0
            let n=if phi0=phi1 then sin phi0 else (cy0-cos phi1)/(phi1-phi0)
            if not(Double.IsFinite n) || abs n<1e-6 then Error "Equidistant conic parallels describe a cylinder; use equirectangular."
            else Ok(struct(n,cy0/n+phi0,0.))
        | ProjectionKind.ConicConformal _ | ProjectionKind.ConicEqualArea _ | ProjectionKind.ConicEquidistant _ ->
            Error "Conic parallels must be finite degrees in [-90,90]."
        | _ -> Ok(struct(nan,nan,nan))

    let private raw kind (struct(n,a,b)) (position:GeoPosition) =
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
        | ProjectionKind.ConicConformal _ ->
            let latitude=canonical.Latitude*radians
            let latitude=if a>0. then max latitude (-halfPi+1e-6) else min latitude (halfPi-1e-6)
            let r=a/Math.Pow(tanHalf latitude,n)
            let struct(sine,cosine)=Math.SinCos(n*canonical.Longitude*radians)
            if Double.IsFinite r then ValueSome(struct(r*sine,a-r*cosine)) else ValueNone
        | ProjectionKind.ConicEqualArea _ ->
            let r=sqrt(max 0. (a-2.*n*sin(canonical.Latitude*radians)))/n
            let struct(sine,cosine)=Math.SinCos(n*canonical.Longitude*radians)
            ValueSome(struct(r*sine,b-r*cosine))
        | ProjectionKind.ConicEquidistant _ ->
            let gy=a-canonical.Latitude*radians
            let struct(sine,cosine)=Math.SinCos(n*canonical.Longitude*radians)
            ValueSome(struct(gy*sine,a-gy*cosine))
        | ProjectionKind.TransverseMercator ->
            if abs canonical.Latitude = 90. then ValueNone
            else ValueSome (struct(Math.Asinh(tan(canonical.Latitude*radians)),-canonical.Longitude*radians))

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
            match clip,conicConstants config.Kind with
            | Error errors,_ -> Error errors
            | _,Error message -> Error [InvalidObservation message]
            | Ok clip,Ok constants ->
                // D3 transverse Mercator swaps its center into the rolled frame before projecting it.
                let center =
                    if config.Kind=ProjectionKind.TransverseMercator then
                        GeoPosition.create -config.Center.Latitude config.Center.Longitude |> Result.toOption
                    else Some(GeoPosition.normalize config.Center)
                match center |> Option.map(raw config.Kind constants) |> Option.defaultValue ValueNone with
                | ValueNone -> Error [InvalidObservation "The projection center is singular."]
                | ValueSome struct(x,y) ->
                    let struct(sine,cosine)=clip |> ValueOption.map GeoMath.sinCosDegrees |> ValueOption.defaultValue struct(0.,1.)
                    let radius=clip |> ValueOption.map(fun angle -> angle*radians) |> ValueOption.defaultValue nan
                    let roll=if config.Kind=ProjectionKind.TransverseMercator then GeoRotation.create 0. 0. 90. |> Result.toOption |> Option.toValueOption else ValueNone
                    let struct(n,a,b)=constants
                    Ok {Config=config;CenterX=x;CenterY=y;ClipSine=sine;ClipCosine=cosine;ClipRadius=radius;Roll=roll;ConicN=n;ConicA=a;ConicB=b}

    let internal rotate projection position =
        let rotated=GeoRotation.apply projection.Config.Rotation position
        match projection.Roll with
        | ValueSome roll -> GeoRotation.apply roll rotated
        | ValueNone -> rotated
    /// Whether streams clip to the finite Mercator latitude band in the rotated frame.
    let internal mercatorBand projection =
        match projection.Config.Kind with
        | ProjectionKind.Mercator | ProjectionKind.TransverseMercator -> true
        | _ -> false
    /// Whether antimeridian-cut boundary edges at the poles curve on screen.
    let internal conic projection =
        match projection.Config.Kind with
        | ProjectionKind.ConicConformal _ | ProjectionKind.ConicEqualArea _ | ProjectionKind.ConicEquidistant _ -> true
        | _ -> false

    /// <summary>Returns the validated configuration, including the scale and translation a fit chose.</summary>
    let config projection = projection.Config
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
            match raw config.Kind (struct(projection.ConicN,projection.ConicA,projection.ConicB)) rotated with
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
                | ProjectionKind.ConicConformal _ | ProjectionKind.ConicEqualArea _ | ProjectionKind.ConicEquidistant _ ->
                    let n,a,b=projection.ConicN,projection.ConicA,projection.ConicB
                    let reference=match config.Kind with ProjectionKind.ConicEqualArea _ -> b | _ -> a
                    let offset=reference-y
                    // Angle about the apex; the D3 correction keeps it on the cone's side of the apex.
                    let mutable angle=atan2 x (abs offset)*float(sign offset)
                    if offset*n<0. then angle<-angle-Math.PI*float(sign x)*float(sign offset)
                    let longitude=angle/n*degrees
                    let distance=Double.Hypot(x,offset)
                    let latitude =
                        match config.Kind with
                        | ProjectionKind.ConicConformal _ ->
                            let r=float(sign n)*distance
                            if r=0. then float(sign n)*halfPi else 2.*atan(Math.Pow(a/r,1./n))-halfPi
                        | ProjectionKind.ConicEqualArea _ ->
                            let value=(a-distance*distance*n*n)/(2.*n)
                            if abs value>1.+1e-12 then nan else asin(Math.Clamp(value,-1.,1.))
                        | _ -> a-float(sign n)*distance
                    if Double.IsNaN latitude || abs latitude>halfPi*(1.+1e-12) || abs longitude>180.*(1.+1e-12) then None
                    else Some(struct(Math.Clamp(longitude,-180.,180.),Math.Clamp(latitude,-halfPi,halfPi)*degrees))
                | ProjectionKind.TransverseMercator ->
                    let latitude=atan(Math.Sinh x)*degrees
                    if abs y > Math.PI || abs latitude >= 90. then None
                    else Some(struct(-y*degrees,latitude))
            match geographic with
            | None -> None
            | Some struct(longitude,latitude) ->
                GeoPosition.create longitude latitude
                |> Result.toOption
                |> Option.map(fun position ->
                    let unrolled=match projection.Roll with ValueSome roll -> GeoRotation.inverse roll position | ValueNone -> position
                    GeoRotation.inverse config.Rotation unrolled)

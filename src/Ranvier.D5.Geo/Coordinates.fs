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

/// <summary>Degree-aware unit-sphere arithmetic shared by geographic kernels.</summary>
module internal GeoMath =
    /// <summary>Computes sine and cosine with exact cardinal angles and bounded arguments.</summary>
    let sinCosDegrees angle =
        let reduced=angle % 360.
        let reduced=if reduced>=180. then reduced-360. elif reduced< -180. then reduced+360. else reduced
        match reduced with
        | 0. -> struct(0.,1.)
        | 90. -> struct(1.,0.)
        | -90. -> struct(-1.,0.)
        | -180. -> struct(0.,-1.)
        | _ -> Math.SinCos(reduced*(Math.PI/180.))

    /// <summary>Maps a geographic position to its unit Cartesian vector.</summary>
    let vector (position:GeoPosition) =
        let struct(sinLongitude,cosLongitude)=sinCosDegrees position.Longitude
        let struct(sinLatitude,cosLatitude)=sinCosDegrees position.Latitude
        struct(cosLatitude*cosLongitude,cosLatitude*sinLongitude,sinLatitude)

    /// <summary>Maps a finite nonzero Cartesian vector to canonical geographic coordinates.</summary>
    let position struct(x,y,z) =
        let longitude=atan2 y x * (180./Math.PI)
        let latitude=atan2 z (Double.Hypot(x,y)) * (180./Math.PI)
        match GeoPosition.create longitude latitude with
        | Ok value -> GeoPosition.normalize value
        | Error _ -> invalidArg "vector" "Spherical arithmetic requires a finite nonzero vector."

    /// <summary>Returns angular distance and the east/north components of the initial bearing.</summary>
    let arc (from:GeoPosition) (destination:GeoPosition) =
        let start,finish=GeoPosition.normalize from,GeoPosition.normalize destination
        let delta=finish.Longitude-start.Longitude
        let struct(sinDelta,cosDelta)=sinCosDegrees delta
        let struct(sinHalf,cosHalf)=sinCosDegrees(delta/2.)
        let struct(sin0,cos0)=sinCosDegrees start.Latitude
        let struct(sin1,cos1)=sinCosDegrees finish.Latitude
        let struct(sinDifference,_)=sinCosDegrees(finish.Latitude-start.Latitude)
        let struct(sinSum,_)=sinCosDegrees(finish.Latitude+start.Latitude)
        let east=cos1*sinDelta
        let north=sinDifference*cosHalf*cosHalf+sinSum*sinHalf*sinHalf
        struct(atan2 (Double.Hypot(east,north)) (sin0*sin1+cos0*cos1*cosDelta),east,north)

/// <summary>Angular measurements on a unit sphere.</summary>
module Geodesic =
    /// <summary>Returns the shortest great-circle distance in radians, in [0,pi].</summary>
    let distance (from:GeoPosition) (destination:GeoPosition) =
        let struct(angle,_,_)=GeoMath.arc from destination
        angle

    /// <summary>Interpolates a shortest great-circle route for t in [0,1]; interior antipodal samples return an ambiguity error.</summary>
    let interpolate t (from:GeoPosition) (destination:GeoPosition) =
        if not(Double.IsFinite t) || t<0. || t>1. then
            Error [InvalidObservation "Geodesic interpolation requires t in [0,1]."]
        else
            let start,finish=GeoPosition.normalize from,GeoPosition.normalize destination
            if t=0. then Ok start
            elif t=1. then Ok finish
            elif start.Latitude=finish.Latitude && (start.Longitude=finish.Longitude || abs start.Latitude=90.) then Ok start
            elif start.Latitude= -finish.Latitude &&
                 (abs(start.Longitude-finish.Longitude)=180. || abs start.Latitude=90.) then
                Error [InvalidObservation "Antipodal endpoints require an explicit intermediate route point."]
            else
                let struct(angle,east,north)=GeoMath.arc start finish
                let magnitude=Double.Hypot(east,north)
                if magnitude=0. then
                    Error [InvalidObservation "The initial great-circle direction is not representable."]
                else
                    let east,north=east/magnitude,north/magnitude
                    let struct(sinLongitude,cosLongitude)=GeoMath.sinCosDegrees start.Longitude
                    let struct(sinLatitude,cosLatitude)=GeoMath.sinCosDegrees start.Latitude
                    let tangentX= -east*sinLongitude-north*sinLatitude*cosLongitude
                    let tangentY=east*cosLongitude-north*sinLatitude*sinLongitude
                    let tangentZ=north*cosLatitude
                    let struct(x,y,z)=GeoMath.vector start
                    let struct(sine,cosine)=Math.SinCos(angle*t)
                    GeoMath.position struct(cosine*x+sine*tangentX,cosine*y+sine*tangentY,cosine*z+sine*tangentZ) |> Ok

[<Struct>]
type internal GeoArc = {
    Start:GeoPosition
    Finish:GeoPosition
    Angle:float
    X:float
    Y:float
    Z:float
    TangentX:float
    TangentY:float
    TangentZ:float
}

module internal GeoArc =
    let create from destination =
        let start,finish=GeoPosition.normalize from,GeoPosition.normalize destination
        let struct(x,y,z)=GeoMath.vector start
        let initial={Start=start;Finish=finish;Angle=0.;X=x;Y=y;Z=z;TangentX=0.;TangentY=0.;TangentZ=0.}
        if start.Latitude=finish.Latitude && (start.Longitude=finish.Longitude || abs start.Latitude=90.) then Ok initial
        elif start.Latitude= -finish.Latitude && (abs(start.Longitude-finish.Longitude)=180. || abs start.Latitude=90.) then
            Error [InvalidObservation "Antipodal endpoints require an explicit intermediate route point."]
        else
            let struct(angle,east,north)=GeoMath.arc start finish
            let magnitude=Double.Hypot(east,north)
            if magnitude=0. then Error [InvalidObservation "The initial great-circle direction is not representable."]
            else
                let east,north=east/magnitude,north/magnitude
                let struct(sinLongitude,cosLongitude)=GeoMath.sinCosDegrees start.Longitude
                let struct(sinLatitude,cosLatitude)=GeoMath.sinCosDegrees start.Latitude
                Ok {initial with Angle=angle
                                 TangentX= -east*sinLongitude-north*sinLatitude*cosLongitude
                                 TangentY=east*cosLongitude-north*sinLatitude*sinLongitude
                                 TangentZ=north*cosLatitude}

    let atFraction amount arc =
        if amount=0. then arc.Start
        elif amount=1. then arc.Finish
        elif arc.Angle=0. then arc.Start
        else
            let struct(sine,cosine)=Math.SinCos(arc.Angle*amount)
            GeoMath.position struct(cosine*arc.X+sine*arc.TangentX,
                                    cosine*arc.Y+sine*arc.TangentY,
                                    cosine*arc.Z+sine*arc.TangentZ)

/// <summary>A longitude shift followed by spherical pitch and roll, in the D3 rotation convention.</summary>
type GeoRotation = private { SinLongitude:float; CosLongitude:float; SinLatitude:float; CosLatitude:float; SinRoll:float; CosRoll:float }

/// <summary>Validated spherical rotations and their inverses.</summary>
module GeoRotation =
    /// <summary>Creates a rotation from finite longitude, latitude and roll angles in degrees.</summary>
    let create longitude latitude roll =
        if not(Double.IsFinite longitude && Double.IsFinite latitude && Double.IsFinite roll) then
            Error [InvalidObservation "Rotation angles require finite degrees."]
        else
            let struct(sl,cl)=GeoMath.sinCosDegrees longitude
            let struct(sp,cp)=GeoMath.sinCosDegrees latitude
            let struct(sr,cr)=GeoMath.sinCosDegrees roll
            Ok {SinLongitude=sl;CosLongitude=cl;SinLatitude=sp;CosLatitude=cp;SinRoll=sr;CosRoll=cr}

    let private identity rotation =
        rotation.CosLongitude=1. && rotation.SinLongitude=0. &&
        rotation.CosLatitude=1. && rotation.SinLatitude=0. &&
        rotation.CosRoll=1. && rotation.SinRoll=0.

    /// <summary>Rotates a location and returns canonical longitude in [-180,180).</summary>
    let apply rotation (position:GeoPosition) =
        if identity rotation then GeoPosition.normalize position
        else
            let struct(x,y,z)=GeoMath.vector position
            let x1=rotation.CosLongitude*x-rotation.SinLongitude*y
            let y1=rotation.SinLongitude*x+rotation.CosLongitude*y
            let x2=rotation.CosLatitude*x1-rotation.SinLatitude*z
            let z2=rotation.SinLatitude*x1+rotation.CosLatitude*z
            GeoMath.position struct(x2,rotation.CosRoll*y1-rotation.SinRoll*z2,rotation.SinRoll*y1+rotation.CosRoll*z2)

    /// <summary>Applies the inverse rotation, with canonical longitude.</summary>
    let inverse rotation (position:GeoPosition) =
        if identity rotation then GeoPosition.normalize position
        else
            let struct(x,y,z)=GeoMath.vector position
            let y1=rotation.CosRoll*y+rotation.SinRoll*z
            let z1= -rotation.SinRoll*y+rotation.CosRoll*z
            let x2=rotation.CosLatitude*x+rotation.SinLatitude*z1
            let z2= -rotation.SinLatitude*x+rotation.CosLatitude*z1
            GeoMath.position struct(rotation.CosLongitude*x2+rotation.SinLongitude*y1,
                                    -rotation.SinLongitude*x2+rotation.CosLongitude*y1,z2)

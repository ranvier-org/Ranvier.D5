namespace Ranvier.D5.Geo

open System
open System.Collections.Generic
open Ranvier.D5

type internal GeoClipSink = {
    Start: GeoPosition -> unit
    Arc: GeoPosition -> GeoPosition -> unit
    Boundary: GeoPosition -> unit
    Finish: unit -> unit
}

module internal GeoPreclip =
    exception private Rejected of LayoutError

    let private position longitude latitude =
        match GeoPosition.create longitude latitude with
        | Ok value -> value
        | Error errors -> raise(Rejected errors.Head)

    let private arcBetween first last =
        match GeoArc.create first last with
        | Ok value -> value
        | Error errors -> raise(Rejected errors.Head)

    let line projection spend sink (input:IReadOnlyList<GeoPosition>) =
        try
            let cylindrical=Projection.kind projection<>ProjectionKind.Orthographic
            let mutable tail=ValueNone
            let same (a:GeoPosition) (b:GeoPosition) =
                a=b || (not cylindrical && abs a.Latitude=90. && a.Latitude=b.Latitude)
            let finish () = sink.Finish(); tail<-ValueNone
            let start point = sink.Start point; tail<-ValueSome point
            let arc first last = sink.Arc first last; tail<-ValueSome last
            let boundary point =
                match tail with
                | ValueSome previous when same previous point -> ()
                | _ -> sink.Boundary point; tail<-ValueSome point
            let ensureStart point =
                match tail with
                | ValueSome current when same current point -> ()
                | _ -> finish(); start point
            let edge (source:GeoPosition) (destination:GeoPosition) =
                let first=if abs source.Latitude=90. then position destination.Longitude source.Latitude else source
                let last=if abs destination.Latitude=90. then position first.Longitude destination.Latitude else destination
                if cylindrical && abs source.Latitude=90. then finish()
                let delta=abs(first.Longitude-last.Longitude)
                let side longitude=if longitude>0. then 180. else -180.
                if not cylindrical then
                    let struct(x0,_,_)=GeoMath.vector first
                    let struct(x1,_,_)=GeoMath.vector last
                    if x0>=0. && x1>=0. then
                        ensureStart first
                        arc first last
                    elif x0<=0. && x1<=0. then finish()
                    else
                        let route=arcBetween first last
                        let root=atan2 (-route.X) route.TangentX
                        let root=if root<0. then root+Math.PI else root
                        if root>route.Angle+1e-12 then
                            raise(Rejected(InvalidObservation "Hemisphere intersection lies outside its great-circle arc."))
                        let sine,cosine=sin root,cos root
                        let intersection=GeoMath.position(struct(0.,route.Y*cosine+route.TangentY*sine,route.Z*cosine+route.TangentZ*sine))
                        if x0>0. then
                            ensureStart first
                            arc first intersection
                            finish()
                        else
                            finish()
                            start intersection
                            arc intersection last
                elif abs first.Longitude=180. && abs last.Longitude=180. then
                    let positive=position 180. first.Latitude
                    let longitude =
                        match tail with
                        | ValueSome current when same current positive -> 180.
                        | _ -> -180.
                    let before,after=position longitude first.Latitude,position longitude last.Latitude
                    ensureStart before
                    arc before after
                elif delta=180. then
                    ensureStart first
                    arcBetween first last |> ignore
                    let latitude=if first.Latitude+last.Latitude>0. then 90. else -90.
                    let before=position first.Longitude latitude
                    let seamBefore=position (side first.Longitude) latitude
                    let seamAfter=position (side last.Longitude) latitude
                    let after=position last.Longitude latitude
                    arc first before
                    boundary seamBefore
                    finish()
                    start seamAfter
                    boundary after
                    arc after last
                elif delta>180. then
                    let route=arcBetween first last
                    let latitude =
                        if abs first.Longitude=180. then first.Latitude
                        elif abs last.Longitude=180. then last.Latitude
                        else
                            let root=atan2 (-route.Y) route.TangentY
                            let root=if root<0. then root+Math.PI else root
                            if root>route.Angle+1e-12 then
                                raise(Rejected(InvalidObservation "Antimeridian intersection lies outside its great-circle arc."))
                            (GeoArc.atFraction (Math.Clamp(root/route.Angle,0.,1.)) route).Latitude
                    let before=position (side first.Longitude) latitude
                    let after=position (side last.Longitude) latitude
                    if abs first.Longitude=180. then
                        ensureStart after
                        if Geodesic.distance after last>0. then arc after last
                    elif abs last.Longitude=180. then
                        ensureStart first
                        arc first before
                    else
                        ensureStart first
                        arc first before
                        finish()
                        start after
                        arc after last
                else
                    ensureStart first
                    arc first last
            if input.Count>0 then
                let mutable previous=Projection.rotate projection input[0]
                for index=1 to input.Count-1 do
                    spend 1L
                    let current=Projection.rotate projection input[index]
                    edge previous current
                    previous<-current
                finish()
            Ok ()
        with Rejected error -> Error error

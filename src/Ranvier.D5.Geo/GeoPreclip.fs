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

    let mercatorLatitude = atan(sinh Math.PI)*180./Math.PI

    let private mercatorBand spend (sink:GeoClipSink) =
        let limit=tanh Math.PI
        let roots=ResizeArray<struct(float*float voption)>(8)
        let mutable tail=ValueNone
        let finish () =
            if ValueOption.isSome tail then sink.Finish()
            tail<-ValueNone
        let emit first last =
            match tail with
            | ValueSome previous when previous=first -> ()
            | _ -> finish();sink.Start first
            sink.Arc first last
            tail<-ValueSome last
        let clip (first:GeoPosition) (last:GeoPosition) =
            spend 1L
            // A unit-speed spherical arc cannot change latitude faster than its arc length.
            if min first.Latitude last.Latitude>=mercatorLatitude || max first.Latitude last.Latitude<= -mercatorLatitude then
                finish()
            // A cheap bound on the arc length keeps short arcs from solving their great circle at all.
            elif max(abs first.Latitude)(abs last.Latitude)+GeoMath.distanceBound first last<=mercatorLatitude then
                emit first last
            else
            let arc=arcBetween first last
            if max(abs first.Latitude)(abs last.Latitude)+arc.Angle*180./Math.PI<=mercatorLatitude then
                emit first last
            else
                roots.Clear()
                roots.Add(struct(0.,ValueNone))
                let amplitude=Double.Hypot(arc.Z,arc.TangentZ)
                if amplitude>=limit then
                    let phase=atan2 arc.TangentZ arc.Z
                    for sign in [-1.;1.] do
                        let offset=acos(Math.Clamp(sign*limit/amplitude,-1.,1.))
                        for direction in [-1.;1.] do
                            for turn in -1..1 do
                                spend 1L
                                let angle=phase+direction*offset+float turn*2.*Math.PI
                                if angle>0. && angle<arc.Angle then
                                    roots.Add(struct(angle,ValueSome(sign*mercatorLatitude)))
                roots.Add(struct(arc.Angle,ValueNone))
                roots.Sort(Comparison(fun struct(a,_) struct(b,_) -> compare a b))
                let endpoint angle latitude =
                    let point=GeoArc.atFraction (angle/arc.Angle) arc
                    let longitude=
                        if first.Longitude=last.Longitude && abs first.Longitude=180. then first.Longitude
                        else point.Longitude
                    position longitude (ValueOption.defaultValue point.Latitude latitude)
                for index=1 to roots.Count-1 do
                    spend 1L
                    let struct(a,latitudeA)=roots[index-1]
                    let struct(b,latitudeB)=roots[index]
                    if b>a then
                        let middle=(a+b)/2.
                        let z=arc.Z*cos middle+arc.TangentZ*sin middle
                        if abs z<=limit then
                            let before=if a=0. then first else endpoint a latitudeA
                            let after=if b=arc.Angle then last else endpoint b latitudeB
                            emit before after
                        else finish()
        {
            Start=fun _ -> finish()
            Arc=clip
            // Seam connectors run along the geographic poles, wholly outside this band.
            Boundary=fun _ -> finish()
            Finish=finish
        }

    /// Rotation normalizes longitude to [-180,180); keeps the seam side an input names explicitly, as D3 does.
    let rotateKeepingSeam projection (source:GeoPosition) =
        let rotated=Projection.rotate projection source
        let turn=source.Longitude%360.
        if abs rotated.Longitude=180. && abs turn=180. then position turn rotated.Latitude else rotated

    let line projection spend sink (input:IReadOnlyList<GeoPosition>) =
        try
            let sink=if Projection.kind projection=ProjectionKind.Mercator then mercatorBand spend sink else sink
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
            let clippedEdge (source:GeoPosition) (destination:GeoPosition) =
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
                    let longitude =
                        match tail with
                        | ValueSome current when same current (position 180. first.Latitude) -> 180.
                        | ValueSome current when same current (position -180. first.Latitude) -> -180.
                        | _ -> first.Longitude
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
            // A pole-to-pole edge is antipodal, but once both poles carry one longitude it names that meridian.
            let edge (source:GeoPosition) (destination:GeoPosition) =
                if abs source.Latitude=90. && source.Latitude= -destination.Latitude && source.Longitude=destination.Longitude then
                    let middle=position source.Longitude 0.
                    clippedEdge source middle
                    clippedEdge middle destination
                else clippedEdge source destination
            let rotate=rotateKeepingSeam projection
            if input.Count>0 then
                let mutable previous=rotate input[0]
                for index=1 to input.Count-1 do
                    spend 1L
                    let current=rotate input[index]
                    edge previous current
                    previous<-current
                finish()
            Ok ()
        with Rejected error -> Error error

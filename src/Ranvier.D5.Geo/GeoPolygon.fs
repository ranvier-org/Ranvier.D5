namespace Ranvier.D5.Geo

open System
open Ranvier.D5

// Spherical parity and boundary rejoining adapted from d3-geo, Copyright 2010-2024 Mike Bostock.
// Its ISC permission notice is distributed in licenses/d3-geo.txt.
module internal GeoPolygon =
    exception private Rejected of LayoutError

    let private position longitude latitude =
        match GeoPosition.create longitude latitude with
        | Ok value -> value
        | Error errors -> raise(Rejected errors.Head)

    // Meridian parity relative to the south pole, with compensated spherical excess.
    let private contains spend (rings:GeoPosition array array) (point:GeoPosition) =
        let mutable area=0.
        let mutable correction=0.
        let mutable angle=0.
        let mutable angleCorrection=0.
        let mutable winding=0
        let struct(sinLongitude,cosLongitude)=GeoMath.sinCosDegrees point.Longitude
        for ring in rings do
            if ring.Length>0 then
                let mutable previous=ring[ring.Length-1]
                for current in ring do
                    spend 1L
                    let delta=current.Longitude-previous.Longitude
                    let sign=if delta>=0. then 1. else -1.
                    let antimeridian=abs delta>180.
                    let struct(sin0,cos0)=GeoMath.sinCosDegrees(previous.Latitude/2.+45.)
                    let struct(sin1,cos1)=GeoMath.sinCosDegrees(current.Latitude/2.+45.)
                    let struct(sinDelta,cosDelta)=GeoMath.sinCosDegrees(abs delta)
                    let k=sin0*sin1
                    let value=atan2 (k*sign*sinDelta) (cos0*cos1+k*cosDelta)-correction
                    let total=area+value
                    correction<-(total-area)-value
                    area<-total
                    let adjustedAngle=(if antimeridian then delta+sign*360. else delta)-angleCorrection
                    let totalAngle=angle+adjustedAngle
                    angleCorrection<-(totalAngle-angle)-adjustedAngle
                    angle<-totalAngle
                    if (antimeridian <> (previous.Longitude>=point.Longitude)) <> (current.Longitude>=point.Longitude) then
                        let route =
                            match GeoArc.create previous current with
                            | Ok value -> value
                            | Error errors -> raise(Rejected errors.Head)
                        if route.Angle>0. then
                            let nx=route.Y*route.TangentZ-route.Z*route.TangentY
                            let ny=route.Z*route.TangentX-route.X*route.TangentZ
                            let nz=route.X*route.TangentY-route.Y*route.TangentX
                            let ix,iy,iz= -cosLongitude*nz,-sinLongitude*nz,sinLongitude*ny+cosLongitude*nx
                            let reversed=antimeridian <> (delta>=0.)
                            let latitude=(if reversed then -1. else 1.)*atan2 iz (Double.Hypot(ix,iy))*(180./Math.PI)
                            if point.Latitude>latitude || (point.Latitude=latitude && (nx<>0. || ny<>0.)) then
                                winding<-winding+(if reversed then 1 else -1)
                    previous<-current
        let southInside=angle< -1e-9 || (abs angle<1e-9 && area<0.)
        if point.Latitude= -90. then southInside else southInside <> (winding%2<>0)

    let project projection precision maxDepth maxBuffer spend makeSampler (rings:GeoPosition array array) =
        try
            if Projection.kind projection=ProjectionKind.Mercator then
                raise(Rejected(InvalidObservation "Mercator polygon clipping requires its world singularity policy."))
            let screen point =
                match Projection.projectRotatedValue projection point with
                | ValueSome value -> value
                | ValueNone -> raise(Rejected(InvalidObservation "Polygon preclipping left an undefined projected vertex."))
            let orthographic=Projection.kind projection=ProjectionKind.Orthographic
            let origin=screen(position 0. 0.)
            let scale=Projection.scale projection
            let period=if orthographic then 2.*Math.PI else 4.
            let normalize value =
                let result=value%period
                if result<0. then result+period else result
            let boundaryPosition amount =
                let t=normalize amount
                if orthographic then
                    let sine,cosine=sin t,cos t
                    GeoMath.position(struct(0.,sine,cosine))
                elif t<=1. then position (-180.+360.*t) 90.
                elif t<=2. then position 180. (90.-180.*(t-1.))
                elif t<=3. then position (180.-360.*(t-2.)) -90.
                else position -180. (-90.+180.*(t-3.))
            let boundaryPoint amount = boundaryPosition amount |> screen
            let parameter (point:Point) =
                let x,y=(point.X-origin.X)/scale,(origin.Y-point.Y)/scale
                if orthographic then normalize(atan2 x y)
                elif abs(abs x-Math.PI)<=abs(abs y-Math.PI/2.) then
                    if x>=0. then 1.+Math.Clamp((Math.PI/2.-y)/Math.PI,0.,1.)
                    else normalize(3.+Math.Clamp((y+Math.PI/2.)/Math.PI,0.,1.))
                elif y>=0. then Math.Clamp((x+Math.PI)/(2.*Math.PI),0.,1.)
                else 2.+Math.Clamp((Math.PI-x)/(2.*Math.PI),0.,1.)
            let maxAngle=4.*asin(sqrt(min 0.5 (precision/scale/2.)))
            let checkBuffer count =
                if count>maxBuffer then raise(Rejected(BudgetExceeded "Polygon clipping exceeds its vertex buffer budget."))
            let boundaryBetween fromPoint toPoint direction whole emit =
                let first=parameter fromPoint
                let mutable last=parameter toPoint
                if whole then last<-first+float direction*period
                elif direction>0 then
                    if last<first then last<-last+period
                elif last>first then last<-last-period
                if orthographic then
                    let required=ceil(abs(last-first)/maxAngle)
                    if not(Double.IsFinite required) || required>float maxBuffer || required>4.*pown 2. maxDepth then
                        raise(Rejected(BudgetExceeded "Polygon boundary precision exceeds its buffer or subdivision budget."))
                    let count=max 1 (int required)
                    for index=1 to count-1 do
                        spend 1L
                        emit(boundaryPoint(first+(last-first)*float index/float count))
                else
                    let mutable corner=if direction>0 then floor first+1. else ceil first-1.
                    while (if direction>0 then corner<last else corner>last) do
                        spend 1L
                        emit(boundaryPoint corner)
                        corner<-corner+float direction
                emit toPoint
            let fragments=ResizeArray<Point array>()
            let working=ResizeArray<Point>()
            let mutable buffered=0
            let add (point:Point) =
                spend 1L
                if working.Count=0 || working[working.Count-1]<>point then
                    checkBuffer(buffered+working.Count+1)
                    working.Add point
            let finish () =
                if working.Count>1 then
                    buffered<-buffered+working.Count
                    fragments.Add(working.ToArray())
                working.Clear()
            let sample=makeSampler add
            let sink = {
                Start=fun point -> working.Clear();add(screen point)
                Arc=sample
                Boundary=fun point -> add(screen point)
                Finish=finish
            }
            let rotated=ResizeArray<GeoPosition array>()
            for ring in rings do
                spend 1L
                let transformed=Array.zeroCreate<GeoPosition> ring.Length
                for index=0 to ring.Length-1 do
                    spend 1L
                    transformed[index]<-Projection.rotate projection ring[index]
                rotated.Add transformed
                if ring.Length>0 then
                    let start=fragments.Count
                    checkBuffer(ring.Length+1)
                    let closed=Array.zeroCreate<GeoPosition>(ring.Length+1)
                    for index=0 to ring.Length-1 do
                        spend 1L
                        closed[index]<-ring[index]
                    closed[ring.Length]<-ring[0]
                    match GeoPreclip.line projection spend sink closed with
                    | Error error -> raise(Rejected error)
                    | Ok () -> ()
                    if fragments.Count-start>1 then
                        let first,last=fragments[start],fragments[fragments.Count-1]
                        if first[0]=last[last.Length-1] then
                            checkBuffer(first.Length+last.Length-1)
                            let merged=Array.zeroCreate<Point>(first.Length+last.Length-1)
                            for index=0 to last.Length-2 do
                                spend 1L
                                merged[index]<-last[index]
                            for index=0 to first.Length-1 do
                                spend 1L
                                merged[last.Length-1+index]<-first[index]
                            fragments[start]<-merged
                            fragments.RemoveAt(fragments.Count-1)
                            buffered<-buffered-1
            let rotated=rotated.ToArray()
            let boundary = {
                Period=period
                Parameter=parameter
                PointAt=boundaryPoint
                ContainsAt=fun amount -> contains spend rotated (boundaryPosition amount)
                Between=boundaryBetween
            }
            GeoRejoin.run maxBuffer spend boundary fragments
        with Rejected error -> Error error

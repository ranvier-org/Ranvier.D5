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

    let private projectWith whole projection precision maxDepth maxBuffer spend makeSampler (rings:GeoPosition array array) =
        try
            let mercator=Projection.mercatorBand projection
            let halfHeight=if mercator then Math.PI else Math.PI/2.
            let latitude y=if mercator then atan(sinh y)*180./Math.PI else y*180./Math.PI
            let screen point =
                match Projection.projectRotatedValue projection point with
                | ValueSome value -> value
                | ValueNone -> raise(Rejected(InvalidObservation "Polygon preclipping left an undefined projected vertex."))
            let cap=Projection.clipCircle projection
            let circular=cap.IsSome
            let struct(_,capSine,capCosine)=cap |> ValueOption.defaultValue struct(0.,0.,0.)
            let origin=screen(position 0. 0.)
            let scale=Projection.scale projection
            let period=if circular then 2.*Math.PI else 4.
            let normalize value =
                let result=value%period
                if result<0. then result+period else result
            let boundaryPosition amount =
                let t=normalize amount
                if circular then
                    let sine,cosine=sin t,cos t
                    GeoMath.position(struct(capCosine,capSine*sine,capSine*cosine))
                elif t<=1. then position (-180.+360.*t) (latitude halfHeight)
                elif t<=2. then position 180. (latitude(halfHeight-2.*halfHeight*(t-1.)))
                elif t<=3. then position (180.-360.*(t-2.)) (latitude -halfHeight)
                else position -180. (latitude(-halfHeight+2.*halfHeight*(t-3.)))
            let boundaryPoint amount = boundaryPosition amount |> screen
            let northLimit=latitude halfHeight
            // Preclipping leaves open fragments on the cap circle, an exact seam longitude or an exact pole or band latitude.
            let parameter (point:GeoPosition) =
                if circular then
                    let struct(_,y,z)=GeoMath.vector point
                    normalize(atan2 y z)
                elif point.Latitude>=northLimit then Math.Clamp((point.Longitude+180.)/360.,0.,1.)
                elif point.Longitude>=180. then 1.+Math.Clamp((northLimit-point.Latitude)/(2.*northLimit),0.,1.)
                elif point.Latitude<= -northLimit then 2.+Math.Clamp((180.-point.Longitude)/360.,0.,1.)
                else normalize(3.+Math.Clamp((point.Latitude+northLimit)/(2.*northLimit),0.,1.))
            // Boundary chords of a screen circle with this radius stay within precision.
            let boundaryRadius=if circular then scale*Projection.clipPlaneRadius projection else scale
            let maxAngle=4.*asin(sqrt(min 0.5 (precision/boundaryRadius/2.)))
            let checkBuffer count =
                if count>maxBuffer then raise(Rejected(BudgetExceeded "Polygon clipping exceeds its vertex buffer budget."))
            let boundaryBetween first last toPoint direction whole emit =
                let mutable last=last
                if whole then last<-first+float direction*period
                elif direction>0 then
                    if last<first then last<-last+period
                elif last>first then last<-last-period
                if circular then
                    let required=ceil(abs(last-first)/maxAngle)
                    if not(Double.IsFinite required) || required>float maxBuffer || required>4.*pown 2. maxDepth then
                        raise(Rejected(BudgetExceeded "Polygon boundary precision exceeds its buffer or subdivision budget."))
                    let count=max 1 (int required)
                    for index=1 to count-1 do
                        spend 1L
                        emit(boundaryPoint(first+(last-first)*float index/float count))
                else
                    let curved=Projection.conic projection
                    let mutable previousParameter=first
                    let mutable previousPoint=boundaryPoint first
                    // Conic pole edges are circular arcs about the apex; refine them until their chords meet precision.
                    let rec refine depth t0 (p0:Point) t1 (p1:Point) =
                        spend 1L
                        let middle=(t0+t1)/2.
                        let point=boundaryPoint middle
                        let dx,dy=p1.X-p0.X,p1.Y-p0.Y
                        let length=Double.Hypot(dx,dy)
                        let deviation=
                            if length=0. then Double.Hypot(point.X-p0.X,point.Y-p0.Y)
                            else abs(dx*(point.Y-p0.Y)-dy*(point.X-p0.X))/length
                        let imprecise=deviation>precision
                        if imprecise && depth>=maxDepth then
                            raise(Rejected(BudgetExceeded "Polygon boundary precision exceeds its subdivision budget."))
                        if (imprecise || abs(t1-t0)>0.125) && depth<maxDepth then
                            refine (depth+1) t0 p0 middle point
                            emit point
                            refine (depth+1) middle point t1 p1
                    let advance parameter (point:Point) =
                        if curved && int(floor(normalize((previousParameter+parameter)/2.)))%2=0 then
                            refine 0 previousParameter previousPoint parameter point
                        previousParameter<-parameter
                        previousPoint<-point
                    let mutable corner=if direction>0 then floor first+1. else ceil first-1.
                    while (if direction>0 then corner<last else corner>last) do
                        spend 1L
                        let point=boundaryPoint corner
                        advance corner point
                        emit point
                        corner<-corner+float direction
                    advance last toPoint
                emit toPoint
            let fragments=ResizeArray<struct(Point array*GeoPosition*GeoPosition)>()
            let working=ResizeArray<Point>()
            let mutable startPosition=Unchecked.defaultof<GeoPosition>
            let mutable endPosition=Unchecked.defaultof<GeoPosition>
            let mutable buffered=0
            let add (point:Point) =
                spend 1L
                if working.Count=0 || working[working.Count-1]<>point then
                    checkBuffer(buffered+working.Count+1)
                    working.Add point
            let finish () =
                if working.Count>1 then
                    buffered<-buffered+working.Count
                    fragments.Add(struct(working.ToArray(),startPosition,endPosition))
                working.Clear()
            let sample=makeSampler add
            let sink = {
                Start=fun point -> working.Clear();startPosition<-point;endPosition<-point;add(screen point)
                Arc=fun first last -> sample first last;endPosition<-last
                Boundary=fun point -> add(screen point);endPosition<-point
                Finish=finish
            }
            let rotated=ResizeArray<GeoPosition array>()
            for ring in rings do
                spend 1L
                let transformed=Array.zeroCreate<GeoPosition> ring.Length
                for index=0 to ring.Length-1 do
                    spend 1L
                    transformed[index]<-GeoPreclip.rotateKeepingSeam projection ring[index]
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
                        let struct(first,_,firstEnd)=fragments[start]
                        let struct(last,lastStart,_)=fragments[fragments.Count-1]
                        if first[0]=last[last.Length-1] then
                            checkBuffer(first.Length+last.Length-1)
                            let merged=Array.zeroCreate<Point>(first.Length+last.Length-1)
                            for index=0 to last.Length-2 do
                                spend 1L
                                merged[index]<-last[index]
                            for index=0 to first.Length-1 do
                                spend 1L
                                merged[last.Length-1+index]<-first[index]
                            fragments[start]<-struct(merged,lastStart,firstEnd)
                            fragments.RemoveAt(fragments.Count-1)
                            buffered<-buffered-1
            let rotated=rotated.ToArray()
            // Rings lying wholly on the cut, such as the GeoJSON world box, enclose zero spherical area. Their planar
            // longitude/latitude winding decides instead: clockwise covers the world and the reverse covers nothing.
            let onCut (point:GeoPosition)=abs point.Longitude=180. || abs point.Latitude=90.
            let wholeCut=not circular && rotated |> Array.exists(fun ring -> ring.Length>0) && rotated |> Array.forall(Array.forall onCut)
            let coversWorld=
                wholeCut &&
                (rotated |> Array.sumBy(fun ring ->
                    let mutable twice=0.
                    for index=0 to ring.Length-1 do
                        let a,b=ring[index],ring[(index+1)%ring.Length]
                        twice<-twice+a.Longitude*b.Latitude-b.Longitude*a.Latitude
                    twice))<0.
            let boundary = {
                Period=period
                PointAt=boundaryPoint
                ContainsAt=fun amount -> whole || (if wholeCut then coversWorld else contains spend rotated (boundaryPosition amount))
                Between=boundaryBetween
            }
            // Fragments of a whole-cut ring lie on the boundary itself, so emit the complete world or nothing.
            if wholeCut then
                if coversWorld then GeoRejoin.run maxBuffer spend boundary (ResizeArray()) else Ok []
            else
                let ended=ResizeArray<struct(Point array*float*float)>(fragments.Count)
                for struct(points,first,last) in fragments do
                    spend 1L
                    ended.Add(struct(points,parameter first,parameter last))
                GeoRejoin.run maxBuffer spend boundary ended
        with Rejected error -> Error error

    let project projection precision maxDepth maxBuffer spend makeSampler rings =
        projectWith false projection precision maxDepth maxBuffer spend makeSampler rings

    /// The complete projected world boundary, used for Sphere where no rectangle or circle describes it.
    let outline projection precision maxDepth maxBuffer spend makeSampler =
        projectWith true projection precision maxDepth maxBuffer spend makeSampler [||]

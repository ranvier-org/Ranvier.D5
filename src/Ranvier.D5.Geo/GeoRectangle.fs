namespace Ranvier.D5.Geo

open System
open Ranvier.D5

module internal GeoRectangle =
    exception private Rejected of LayoutError

    let private clipSegmentBounds spend left top right bottom (first:Point) (last:Point) =
        let inline outcode (p:Point) =
            (if p.X<left then 1 else 0) |||
            (if p.X>right then 2 else 0) |||
            (if p.Y<top then 4 else 0) |||
            (if p.Y>bottom then 8 else 0)
        let ca=outcode first
        let cb=outcode last
        if (ca ||| cb)=0 then
            spend 1L
            if first<>last then ValueSome(struct(first,last)) else ValueNone
        elif (ca &&& cb)<>0 then
            spend 1L
            ValueNone
        else
            let mutable a=first
            let mutable b=last
            let mutable visible=true
            let mutable edge=0
            while visible && edge<4 do
                spend 1L
                let horizontal=edge<2
                let boundary =
                    match edge with
                    | 0 -> left
                    | 1 -> right
                    | 2 -> top
                    | _ -> bottom
                let coordA,coordB=if horizontal then a.X,b.X else a.Y,b.Y
                let insideA,insideB =
                    if edge%2=0 then coordA>=boundary,coordB>=boundary
                    else coordA<=boundary,coordB<=boundary
                if not insideA && not insideB then visible<-false
                elif insideA<>insideB then
                    let da,db=coordA-boundary,coordB-boundary
                    let t =
                        if Double.IsFinite da && Double.IsFinite db && Double.IsFinite(da-db) then da/(da-db)
                        else
                            let scale=max (abs boundary) (max (abs coordA) (abs coordB))
                            let sa,sb=coordA/scale-boundary/scale,coordB/scale-boundary/scale
                            sa/(sa-sb)
                    let mix x y=(1.-t)*x+t*y
                    let intersection =
                        if horizontal then {X=boundary;Y=mix a.Y b.Y}
                        else {X=mix a.X b.X;Y=boundary}
                    if insideA then b<-intersection else a<-intersection
                edge<-edge+1
            if visible then
                let clamp (point:Point)={X=Math.Clamp(point.X,left,right);Y=Math.Clamp(point.Y,top,bottom)}
                a<-clamp a
                b<-clamp b
            if visible && a<>b then ValueSome(struct(a,b)) else ValueNone

    let clipSegment spend clip (first:Point) (last:Point) =
        let width,height=Bounds.width clip,Bounds.height clip
        if width<=0. || height<=0. then ValueNone
        else
            let left,top=Bounds.x clip,Bounds.y clip
            clipSegmentBounds spend left top (left+width) (top+height) first last

    let polygons maxBuffer spend clip (rings:Point array list) =
        try
            if Bounds.width clip=0. || Bounds.height clip=0. then Ok []
            else
                let left,top=Bounds.x clip,Bounds.y clip
                let width,height=Bounds.width clip,Bounds.height clip
                let right,bottom=left+width,top+height
                let normalize value =
                    let result=value%4.
                    if result<0. then result+4. else result
                let pointAt amount =
                    let t=normalize amount
                    if t<=1. then {X=left;Y=bottom-height*t}
                    elif t<=2. then {X=left+width*(t-1.);Y=top}
                    elif t<=3. then {X=right;Y=top+height*(t-2.)}
                    else {X=right-width*(t-3.);Y=bottom}
                let parameter (point:Point) =
                    if point.X=left then Math.Clamp((bottom-point.Y)/height,0.,1.)
                    elif point.Y=top then 1.+Math.Clamp((point.X-left)/width,0.,1.)
                    elif point.X=right then 2.+Math.Clamp((point.Y-top)/height,0.,1.)
                    else normalize(3.+Math.Clamp((right-point.X)/width,0.,1.))
                let contains (point:Point) =
                    let mutable winding=0
                    for ring in rings do
                        if ring.Length>0 then
                            let mutable a=ring[ring.Length-1]
                            for index=0 to ring.Length-1 do
                                spend 1L
                                let b=ring[index]
                                if a.Y<=point.Y then
                                    if b.Y>point.Y && Predicates.orient2d a b point>0 then winding<-winding+1
                                elif b.Y<=point.Y && Predicates.orient2d a b point<0 then winding<-winding-1
                                a<-b
                    winding<>0
                let between first last toPoint direction whole emit =
                    let mutable last=last
                    if whole then last<-first+float direction*4.
                    elif direction>0 then
                        if last<first then last<-last+4.
                    elif last>first then last<-last-4.
                    let mutable corner=if direction>0 then floor first+1. else ceil first-1.
                    while (if direction>0 then corner<last else corner>last) do
                        spend 1L
                        emit(pointAt corner)
                        corner<-corner+float direction
                    emit toPoint
                let fragments=ResizeArray<Point array>()
                let working=ResizeArray<Point>()
                let mutable buffered=0
                let checkBuffer count =
                    if count>maxBuffer then raise(Rejected(BudgetExceeded "Rectangular polygon clipping exceeds its vertex buffer budget."))
                let add point =
                    spend 1L
                    if working.Count=0 || working[working.Count-1]<>point then
                        checkBuffer(buffered+working.Count+1)
                        working.Add point
                let finish () =
                    if working.Count>1 then
                        buffered<-buffered+working.Count
                        fragments.Add(working.ToArray())
                    working.Clear()
                for ring in rings do
                    spend 1L
                    if ring.Length>0 then
                        let start=fragments.Count
                        let mutable a=ring[0]
                        for index=0 to ring.Length-1 do
                            spend 1L
                            let b=if index+1<ring.Length then ring[index+1] else ring[0]
                            match clipSegmentBounds spend left top right bottom a b with
                            | ValueNone -> if a<>b then finish()
                            | ValueSome(struct(first,last)) ->
                                if working.Count=0 || working[working.Count-1]<>first then
                                    finish()
                                    add first
                                add last
                                if last<>b then finish()
                            a<-b
                        finish()
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
                let boundary={Period=4.;PointAt=pointAt;ContainsAt=(fun amount -> contains(pointAt amount));Between=between}
                let ended=ResizeArray<struct(Point array*float*float)>(fragments.Count)
                for fragment in fragments do
                    spend 1L
                    ended.Add(struct(fragment,parameter fragment[0],parameter fragment[fragment.Length-1]))
                GeoRejoin.run maxBuffer spend boundary ended
        with Rejected error -> Error error

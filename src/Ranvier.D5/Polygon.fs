namespace Ranvier.D5
open System.Numerics

[<RequireQualifiedAccess>]
module Polygon =
    let private coordinates points =
        points |> List.toArray |> Array.collect(fun point -> [|point.X;point.Y|]) |> ExactDyadic.integers
    let private moments points =
        let v,exponent=coordinates points
        let count=v.Length/2
        let mutable area,x,y=BigInteger.Zero,BigInteger.Zero,BigInteger.Zero
        for i in 0..count-1 do
            let j=(i+1)%count
            let cross=v[2*i]*v[2*j+1]-v[2*j]*v[2*i+1]
            area<-area+cross
            x<-x+(v[2*i]+v[2*j])*cross
            y<-y+(v[2*i+1]+v[2*j+1])*cross
        area,x,y,exponent
    let area points =
        let area,_,_,exponent=moments points
        ExactDyadic.ratio area (BigInteger 2) (2*exponent)
    let centroid points =
        let area,x,y,exponent=moments points
        if area.IsZero then None else
        let denominator=BigInteger 3*area
        Some(Point.create (ExactDyadic.ratio x denominator exponent) (ExactDyadic.ratio y denominator exponent))
    let contains (point:Point) points =
        Check.finite "x" point.X;Check.finite "y" point.Y
        let vertices=List.toArray points
        for vertex in vertices do Check.finite "x" vertex.X;Check.finite "y" vertex.Y
        let mutable winding=0
        let mutable boundary=false
        for i in 0..vertices.Length-1 do
            let a,b=vertices[i],vertices[(i+1)%vertices.Length]
            let orientation=Predicates.orient2d a b point
            if orientation=0 && point.X>=min a.X b.X && point.X<=max a.X b.X && point.Y>=min a.Y b.Y && point.Y<=max a.Y b.Y then boundary<-true
            elif a.Y<=point.Y && b.Y>point.Y && orientation>0 then winding<-winding+1
            elif a.Y>point.Y && b.Y<=point.Y && orientation<0 then winding<-winding-1
        boundary || winding<>0
    let hull points =
        for point in points do Check.finite "x" point.X;Check.finite "y" point.Y
        let ordered=List.toArray points
        Array.sortInPlaceWith(fun a b -> let x=compare a.X b.X in if x=0 then compare a.Y b.Y else x) ordered
        let unique=ResizeArray<Point>()
        for point in ordered do
            if unique.Count=0 || unique[unique.Count-1]<>point then unique.Add point
        let sorted=unique.ToArray()
        if sorted.Length<2 then Array.toList sorted else
        let chain sequence =
            let result=ResizeArray<Point>()
            for point in sequence do
                while result.Count>=2 && Predicates.orient2d result[result.Count-2] result[result.Count-1] point<=0 do result.RemoveAt(result.Count-1)
                result.Add point
            result.RemoveAt(result.Count-1)
            List.ofSeq result
        chain sorted @ chain (Array.rev sorted)

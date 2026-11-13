namespace Ranvier.D5

open System
open System.Numerics

/// <summary>Area, centroid, containment and convex hull of polygons given as vertex lists, using exact arithmetic.</summary>
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
    /// <summary>Returns the signed area of the closed polygon, positive for counterclockwise vertices in y-up coordinates. Raises ArgumentException
    /// for non-finite coordinates or an area outside the finite range.</summary>
    let area points =
        let mutable eligible=true
        let mutable largest=0.
        for point in points do
            let x,y=point.X*2.,point.Y*2.
            if not(Double.IsFinite x && Double.IsFinite y) || floor x<>x || floor y<>y then eligible<-false
            largest<-max largest (max (abs x) (abs y))
        if eligible && 2.*float(List.length points)*largest*largest<=4503599627370496. then
            match points with
            | [] -> 0.
            | first::rest ->
                let mutable previous=first
                let mutable twiceScaledArea=0.
                for current in rest do
                    twiceScaledArea<-twiceScaledArea+(previous.X*2.)*(current.Y*2.)-(current.X*2.)*(previous.Y*2.)
                    previous<-current
                (twiceScaledArea+(previous.X*2.)*(first.Y*2.)-(first.X*2.)*(previous.Y*2.))/8.
        else
            let values,exponent=coordinates points
            let count=values.Length/2
            let mutable area=BigInteger.Zero
            for i in 0..count-1 do
                let j=(i+1)%count
                area<-area+values[2*i]*values[2*j+1]-values[2*j]*values[2*i+1]
            ExactDyadic.ratio area (BigInteger 2) (2*exponent)
    /// <summary>Returns the area centroid, or None when the signed area is zero. Raises ArgumentException for non-finite coordinates or
    /// results.</summary>
    let centroid points =
        let area,x,y,exponent=moments points
        if area.IsZero then None else
        let denominator=BigInteger 3*area
        Some(Point.create (ExactDyadic.ratio x denominator exponent) (ExactDyadic.ratio y denominator exponent))
    let internal containsArrayWithCancellation (token:System.Threading.CancellationToken) (point:Point) (vertices:Point array) =
        token.ThrowIfCancellationRequested()
        let mutable winding=0
        let mutable boundary=false
        for i in 0..vertices.Length-1 do
            if i%1024=0 then token.ThrowIfCancellationRequested()
            let a,b=vertices[i],vertices[(i+1)%vertices.Length]
            let orientation=Predicates.orient2d a b point
            if orientation=0 && point.X>=min a.X b.X && point.X<=max a.X b.X && point.Y>=min a.Y b.Y && point.Y<=max a.Y b.Y then boundary<-true
            elif a.Y<=point.Y && b.Y>point.Y && orientation>0 then winding<-winding+1
            elif a.Y>point.Y && b.Y<=point.Y && orientation<0 then winding<-winding-1
        boundary || winding<>0

    /// <summary>Tests whether the point lies inside the polygon by the nonzero winding rule; boundary points count as inside. Raises
    /// ArgumentException for non-finite coordinates.</summary>
    let contains (point:Point) points =
        Check.finite "x" point.X;Check.finite "y" point.Y
        let vertices=List.toArray points
        for vertex in vertices do Check.finite "x" vertex.X;Check.finite "y" vertex.Y
        containsArrayWithCancellation System.Threading.CancellationToken.None point vertices
    /// <summary>Returns the convex hull without duplicate or collinear points, counterclockwise in y-up coordinates from the lowest-x, then lowest-y
    /// point. Raises ArgumentException for non-finite coordinates.</summary>
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

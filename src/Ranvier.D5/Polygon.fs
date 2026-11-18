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
        let n = vertices.Length
        if n = 0 then false
        else
            let px = point.X
            let py = point.Y
            let mutable winding = 0
            let mutable boundary = false
            let mutable a = vertices[n - 1]
            for i = 0 to n - 1 do
                if (i &&& 1023) = 0 then token.ThrowIfCancellationRequested()
                let b = vertices[i]
                // Proven in GeometryGeo.v (y_straddle_filter_preserves_polygon_containment)
                if not ((a.Y < py && b.Y < py) || (a.Y > py && b.Y > py)) then
                    let orientation = Predicates.orient2d a b point
                    if orientation = 0 && px >= min a.X b.X && px <= max a.X b.X && py >= min a.Y b.Y && py <= max a.Y b.Y then
                        boundary <- true
                    elif a.Y <= py && b.Y > py && orientation > 0 then
                        winding <- winding + 1
                    elif a.Y > py && b.Y <= py && orientation < 0 then
                        winding <- winding - 1
                a <- b
            boundary || winding <> 0

    /// <summary>Tests whether the point lies inside the polygon by the nonzero winding rule; boundary points count as inside. Raises
    /// ArgumentException for non-finite coordinates.</summary>
    let contains (point:Point) points =
        Check.finite "x" point.X;Check.finite "y" point.Y
        match points with
        | [] -> false
        | first :: rest ->
            Check.finite "x" first.X;Check.finite "y" first.Y
            let px = point.X
            let py = point.Y
            let mutable winding = 0
            let mutable boundary = false
            let inline stepEdge (a:Point) (b:Point) =
                if not ((a.Y < py && b.Y < py) || (a.Y > py && b.Y > py)) then
                    let orientation = Predicates.orient2d a b point
                    if orientation = 0 && px >= min a.X b.X && px <= max a.X b.X && py >= min a.Y b.Y && py <= max a.Y b.Y then
                        boundary <- true
                    elif a.Y <= py && b.Y > py && orientation > 0 then
                        winding <- winding + 1
                    elif a.Y > py && b.Y <= py && orientation < 0 then
                        winding <- winding - 1
            let mutable a = first
            let mutable cur = rest
            while not cur.IsEmpty do
                let b = cur.Head
                Check.finite "x" b.X;Check.finite "y" b.Y
                stepEdge a b
                a <- b
                cur <- cur.Tail
            stepEdge a first
            boundary || winding <> 0

    let private pointLexComparer =
        { new System.Collections.Generic.IComparer<Point> with
            member _.Compare(a, b) =
                let x = a.X.CompareTo b.X
                if x <> 0 then x else a.Y.CompareTo b.Y }

    let internal hullFromValidatedCopy (ordered: Point array) : Point list =
#if !FABLE_COMPILER
        Array.Sort(ordered, pointLexComparer)
#else
        Array.sortInPlaceWith(fun a b -> let x=compare a.X b.X in if x=0 then compare a.Y b.Y else x) ordered
#endif
        let unique=ResizeArray<Point>(ordered.Length)
        for i = 0 to ordered.Length - 1 do
            let point = ordered[i]
            if unique.Count=0 || unique[unique.Count-1]<>point then unique.Add point
        let m = unique.Count
        if m = 0 then []
        elif m = 1 then [ unique[0] ]
        else
            let lower = ResizeArray<Point>(16)
            for i = 0 to m - 1 do
                let point = unique[i]
                while lower.Count >= 2 && Predicates.orient2d lower[lower.Count - 2] lower[lower.Count - 1] point <= 0 do
                    lower.RemoveAt(lower.Count - 1)
                lower.Add point
            let upper = ResizeArray<Point>(16)
            for i = m - 1 downto 0 do
                let point = unique[i]
                while upper.Count >= 2 && Predicates.orient2d upper[upper.Count - 2] upper[upper.Count - 1] point <= 0 do
                    upper.RemoveAt(upper.Count - 1)
                upper.Add point
            let mutable acc = []
            for i = upper.Count - 2 downto 0 do
                acc <- upper[i] :: acc
            for i = lower.Count - 2 downto 0 do
                acc <- lower[i] :: acc
            acc

    /// <summary>Returns the convex hull without duplicate or collinear points, counterclockwise in y-up coordinates from the lowest-x, then lowest-y
    /// point. Raises ArgumentException for non-finite coordinates.</summary>
    let hull points =
        let ordered=List.toArray points
        for i = 0 to ordered.Length - 1 do
            let point = ordered[i]
            Check.finite "x" point.X; Check.finite "y" point.Y
        hullFromValidatedCopy ordered

namespace Ranvier.D5

/// <summary>A clipped unique-site cell with every original input identity; vertices have no repeated closing point.</summary>
type VoronoiCell = { Site:int; InputIndices:int list; Vertices:Point list }

[<RequireQualifiedAccess>]
module Voronoi =
    let private exactSide (a:Point) (b:Point) (p:Point) =
        let v,_=ExactDyadic.integers [|a.X;a.Y;b.X;b.Y;p.X;p.Y|]
        ((v[2]-v[0])*(2I*v[4]-v[0]-v[2])+(v[3]-v[1])*(2I*v[5]-v[1]-v[3])).Sign

    let private side (a:Point) (b:Point) (p:Point) =
        let x=(b.X-a.X)*((p.X-a.X)+(p.X-b.X))
        let y=(b.Y-a.Y)*((p.Y-a.Y)+(p.Y-b.Y))
        let value=x+y
        if System.Double.IsFinite value && abs value> (abs x+abs y)*1e-14 && abs value>1e-280 then sign value,value
        else
            let halfInteger x=abs x<=1048576. && System.Double.IsInteger(x*2.)
            let bounded (point:Point)=halfInteger point.X && halfInteger point.Y
            // On this half-integer lattice, each side-expression intermediate is exact in binary64.
            if bounded a && bounded b && bounded p then sign value,value
            else exactSide a b p,System.Double.NaN

    let private intersection (a:Point) (b:Point) (p:Point) (q:Point) sp sq =
        let clamp lo hi x=max (min lo hi) (min (max lo hi) x)
        let x,y=
            if System.Double.IsFinite sp && System.Double.IsFinite sq && System.Double.IsFinite(sp-sq) then
                let t=sp/(sp-sq)
                p.X+(q.X-p.X)*t,p.Y+(q.Y-p.Y)*t
            else
                let v,e=ExactDyadic.integers [|a.X;a.Y;b.X;b.Y;p.X;p.Y;q.X;q.Y|]
                let dx,dy=v[2]-v[0],v[3]-v[1]
                let left=dx*(2I*v[4]-v[0]-v[2])+dy*(2I*v[5]-v[1]-v[3])
                let right=dx*(2I*v[6]-v[0]-v[2])+dy*(2I*v[7]-v[1]-v[3])
                let denominator=left-right
                ExactDyadic.ratio (v[4]*denominator+(v[6]-v[4])*left) denominator e,
                ExactDyadic.ratio (v[5]*denominator+(v[7]-v[5])*left) denominator e
        Point.create (clamp p.X q.X x) (clamp p.Y q.Y y)

    /// <summary>Clips each unique site's closed cell to finite bounds. Empty cells have no vertices; duplicate inputs share one cell. Bisector classification is exact, intersections round to finite doubles.</summary>
    let clip bounds mesh =
        let points=Delaunay.sites mesh
        let x,y=Bounds.x bounds,Bounds.y bounds
        let right,bottom=x+Bounds.width bounds,y+Bounds.height bounds
        [for i in 0..points.Length-1 ->
            let mutable polygon=[Point.create x y;Point.create right y;Point.create right bottom;Point.create x bottom]
            for neighbor in Delaunay.neighbors i mesh do
                if not polygon.IsEmpty then
                    let output=ResizeArray<Point>()
                    let mutable previous=List.last polygon
                    let mutable previousSign,previousValue=side points[i] points[neighbor] previous
                    for current in polygon do
                        let currentSign,currentValue=side points[i] points[neighbor] current
                        if (previousSign<=0)<>(currentSign<=0) then
                            output.Add(intersection points[i] points[neighbor] previous current previousValue currentValue)
                        if currentSign<=0 then output.Add current
                        previous<-current;previousSign<-currentSign;previousValue<-currentValue
                    polygon<-output |> Seq.distinct |> Seq.toList
            {Site=i;InputIndices=Delaunay.inputIndices i mesh |> Array.toList;Vertices=polygon}]


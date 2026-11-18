namespace Ranvier.D5

open System

/// <summary>A clipped unique-site cell with every original input identity; vertices have no repeated closing point.</summary>
type VoronoiCell = { Site:int; InputIndices:int list; Vertices:Point list }

/// <summary>Voronoi cells derived from a Delaunay <c>Triangulation</c>.</summary>
[<RequireQualifiedAccess>]
module Voronoi =
    let private exactSide (a:Point) (b:Point) (p:Point) =
        let v,_=ExactDyadic.integers [|a.X;a.Y;b.X;b.Y;p.X;p.Y|]
        ((v[2]-v[0])*(2I*v[4]-v[0]-v[2])+(v[3]-v[1])*(2I*v[5]-v[1]-v[3])).Sign

    let inline private side (a:Point) (b:Point) (p:Point) : struct(int * float) =
        let x=(b.X-a.X)*((p.X-a.X)+(p.X-b.X))
        let y=(b.Y-a.Y)*((p.Y-a.Y)+(p.Y-b.Y))
        let value=x+y
        if Double.IsFinite value && abs value> (abs x+abs y)*1e-14 && abs value>1e-280 then struct(sign value, value)
        else
            let halfInteger v=abs v<=1048576. && Double.IsInteger(v*2.)
            let bounded (point:Point)=halfInteger point.X && halfInteger point.Y
            // On this half-integer lattice, each side-expression intermediate is exact in binary64.
            if bounded a && bounded b && bounded p then struct(sign value, value)
            else struct(exactSide a b p, System.Double.NaN)

    let private intersection (a:Point) (b:Point) (p:Point) (q:Point) sp sq =
        let inline clamp lo hi x=max (min lo hi) (min (max lo hi) x)
        let x,y=
            if Double.IsFinite sp && Double.IsFinite sq && Double.IsFinite(sp-sq) then
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

    let inline private addUnique (buf: ResizeArray<Point>) (pt: Point) =
        let mutable exists = false
        let mutable k = 0
        let count = buf.Count
        while not exists && k < count do
            if buf[k] = pt then exists <- true
            k <- k + 1
        if not exists then buf.Add pt

    /// <summary>Clips each unique site's closed cell to finite bounds. Empty cells have no vertices; duplicate inputs share one cell. Bisector classification is exact, intersections round to finite doubles.</summary>
    let clip bounds mesh =
        let points = Delaunay.rawSites mesh
        let x, y = Bounds.x bounds, Bounds.y bounds
        let right, bottom = x + Bounds.width bounds, y + Bounds.height bounds
        let viewportBuf = ResizeArray<Point>(4)
        addUnique viewportBuf (Point.create x y)
        addUnique viewportBuf (Point.create right y)
        addUnique viewportBuf (Point.create right bottom)
        addUnique viewportBuf (Point.create x bottom)
        let vpCount = viewportBuf.Count
        let v0 = if vpCount > 0 then viewportBuf[0] else { Point.X = 0.; Y = 0. }
        let v1 = if vpCount > 1 then viewportBuf[1] else v0
        let v2 = if vpCount > 2 then viewportBuf[2] else v0
        let v3 = if vpCount > 3 then viewportBuf[3] else v0
        let nSites = points.Length
        if nSites = 0 then []
        else
            let cells = Array.zeroCreate<VoronoiCell> nSites
            let mutable curPoly = ResizeArray<Point>(16)
            let mutable nextPoly = ResizeArray<Point>(16)
            for i = 0 to nSites - 1 do
                curPoly.Clear()
                if vpCount > 0 then curPoly.Add v0
                if vpCount > 1 then curPoly.Add v1
                if vpCount > 2 then curPoly.Add v2
                if vpCount > 3 then curPoly.Add v3
                let pi = points[i]
                let nbrs = Delaunay.rawNeighbors i mesh
                let mutable nIdx = 0
                while nIdx < nbrs.Length && curPoly.Count > 0 do
                    let pn = points[nbrs[nIdx]]
                    nextPoly.Clear()
                    let pCount = curPoly.Count
                    let mutable previous = curPoly[pCount - 1]
                    let struct(prevSign0, prevVal0) = side pi pn previous
                    let mutable previousSign = prevSign0
                    let mutable previousValue = prevVal0
                    for k = 0 to pCount - 1 do
                        let current = curPoly[k]
                        let struct(currentSign, currentValue) = side pi pn current
                        if (previousSign <= 0) <> (currentSign <= 0) then
                            addUnique nextPoly (intersection pi pn previous current previousValue currentValue)
                        if currentSign <= 0 then
                            addUnique nextPoly current
                        previous <- current
                        previousSign <- currentSign
                        previousValue <- currentValue
                    let swap = curPoly
                    curPoly <- nextPoly
                    nextPoly <- swap
                    nIdx <- nIdx + 1
                let verts = Data.resizeToList curPoly
                cells[i] <- { Site = i; InputIndices = Delaunay.inputIndicesList i mesh; Vertices = verts }
            let mutable out = []
            for i = nSites - 1 downto 0 do
                out <- cells[i] :: out
            out

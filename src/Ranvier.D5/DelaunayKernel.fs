namespace Ranvier.D5

open System
open System.Collections.Generic

// Advancing-hull construction adapted from Delaunator 5.1.0 (ISC, Mapbox).
// Topology uses original-coordinate exact predicates; normalized coordinates only guide search.
module internal DelaunayKernel =
    let fast (points:Point[]) : struct(int array * int array) voption =
        let count=points.Length
        let mutable maxCoordinate = 0.
        for i = 0 to count - 1 do
            let p = points[i]
            let ax = abs p.X
            let ay = abs p.Y
            if ax > maxCoordinate then maxCoordinate <- ax
            if ay > maxCoordinate then maxCoordinate <- ay
        let exponent=if maxCoordinate=0. then 0 else Math.ILogB maxCoordinate
        let scaled = Array.zeroCreate<Point> count
        let distinct = HashSet<struct(float*float)>(count)
        let mutable preserved = true
        let mutable minX = Double.PositiveInfinity
        let mutable maxX = Double.NegativeInfinity
        let mutable minY = Double.PositiveInfinity
        let mutable maxY = Double.NegativeInfinity
        for i = 0 to count - 1 do
            let p = points[i]
            let sx = Math.ScaleB(p.X, -exponent)
            let sy = Math.ScaleB(p.Y, -exponent)
            Check.finite "x" sx
            Check.finite "y" sy
            scaled[i] <- { Point.X = sx; Y = sy }
            if preserved && not (distinct.Add(struct(sx, sy))) then preserved <- false
            if sx < minX then minX <- sx
            if sx > maxX then maxX <- sx
            if sy < minY then minY <- sy
            if sy > maxY then maxY <- sy
        if not preserved then ValueNone
        else
            let inline distance (a: Point) (b: Point) =
                let dx, dy = a.X - b.X, a.Y - b.Y
                dx * dx + dy * dy
            let center (a: Point) (b: Point) (c: Point) =
                let dx,dy,ex,ey=b.X-a.X,b.Y-a.Y,c.X-a.X,c.Y-a.Y
                let bl,cl=dx*dx+dy*dy,ex*ex+ey*ey
                let d=0.5/(dx*ey-dy*ex)
                let x,y=a.X+(ey*bl-dy*cl)*d,a.Y+(dx*cl-ex*bl)*d
                if Double.IsFinite x && Double.IsFinite y then ValueSome { Point.X = x; Y = y } else ValueNone
            let midpoint = { Point.X = (minX + maxX) / 2.; Y = (minY + maxY) / 2. }
            let mutable i0 = 0
            let mutable bestD0 = distance scaled[0] midpoint
            for i = 1 to count - 1 do
                let d = distance scaled[i] midpoint
                if d < bestD0 then
                    bestD0 <- d
                    i0 <- i
            let mutable i1 = -1
            let mutable bestD1 = Double.PositiveInfinity
            let s0 = scaled[i0]
            for i = 0 to count - 1 do
                if i <> i0 then
                    let d = distance s0 scaled[i]
                    if d < bestD1 then
                        bestD1 <- d
                        i1 <- i
            let mutable seedIdx = -1
            let mutable seedCenter = { Point.X = 0.; Y = 0. }
            let mutable radius = Double.PositiveInfinity
            if i1 >= 0 then
                for i = 0 to count - 1 do
                    if i <> i0 && i <> i1 && Predicates.orient2d points[i0] points[i1] points[i] <> 0 then
                        match center s0 scaled[i1] scaled[i] with
                        | ValueSome c ->
                            let d = distance c s0
                            if d < radius then
                                seedIdx <- i
                                seedCenter <- c
                                radius <- d
                        | ValueNone -> ()
            if seedIdx < 0 then ValueNone
            else
                let c = seedCenter
                let i1, i2 =
                    if Predicates.orient2d points[i0] points[i1] points[seedIdx] > 0 then seedIdx, i1
                    else i1, seedIdx
                let maxTriCap = max 6 (6 * count)
                let triangles = ResizeArray<int>(maxTriCap)
                let opposite = ResizeArray<int>(maxTriCap)
                let previous, next, hullTri = Array.zeroCreate<int> count, Array.zeroCreate<int> count, Array.zeroCreate<int> count
                let hashSize = max 1 (int (ceil (sqrt (float count))))
                let hash = Array.create hashSize -1
                let hashSizeF = float hashSize
                let inline key i =
                    let dx, dy = scaled[i].X - c.X, scaled[i].Y - c.Y
                    let sum = abs dx + abs dy
                    let p = if sum = 0. then 0. else dx / sum
                    int (floor ((if dy > 0. then 3. - p else 1. + p) * 0.25 * hashSizeF)) % hashSize
                let mutable start = i0
                let inline link a b =
                    opposite[a] <- b
                    if b >= 0 then opposite[b] <- a
                let add a b cVert x y z =
                    let t = triangles.Count
                    triangles.Add a; triangles.Add b; triangles.Add cVert
                    opposite.Add -1; opposite.Add -1; opposite.Add -1
                    link t x; link (t + 1) y; link (t + 2) z
                    t
                let legalizeStack = Stack<int>(32)
                let legalize edge =
                    legalizeStack.Clear()
                    let mutable a, ar, running = edge, 0, true
                    while running do
                        let b = opposite[a]
                        let a0 = a - a % 3
                        ar <- a0 + (a + 2) % 3
                        if b >= 0 then
                            let b0 = b - b % 3
                            let al, bl = a0 + (a + 1) % 3, b0 + (b + 2) % 3
                            let p0, pr, pl, p1 = triangles[ar], triangles[a], triangles[al], triangles[bl]
                            if Predicates.incircle points[p0] points[pr] points[pl] points[p1] < 0 then
                                triangles[a] <- p1; triangles[b] <- p0
                                let hbl = opposite[bl]
                                if hbl = -1 then
                                    let mutable e, search = start, true
                                    while search do
                                        if hullTri[e] = bl then hullTri[e] <- a; search <- false
                                        else
                                            e <- previous[e]
                                            if e = start then search <- false
                                link a hbl; link b opposite[ar]; link ar bl
                                legalizeStack.Push(b0 + (b + 1) % 3)
                            elif legalizeStack.Count = 0 then running <- false
                            else a <- legalizeStack.Pop()
                        elif legalizeStack.Count = 0 then running <- false
                        else a <- legalizeStack.Pop()
                    ar
                next[i0] <- i1; previous[i2] <- i1
                next[i1] <- i2; previous[i0] <- i2
                next[i2] <- i0; previous[i1] <- i0
                hullTri[i0] <- 0; hullTri[i1] <- 1; hullTri[i2] <- 2
                hash[key i0] <- i0; hash[key i1] <- i1; hash[key i2] <- i2
                add i0 i1 i2 -1 -1 -1 |> ignore
                let ids = Array.init count id
#if !FABLE_COMPILER
                let dists = Array.zeroCreate<float> count
                for i = 0 to count - 1 do
                    dists[i] <- distance scaled[i] c
                Array.Sort(dists, ids)
                let mutable tieStart = 0
                while tieStart < count - 1 do
                    if dists[tieStart + 1] = dists[tieStart] then
                        let dTie = dists[tieStart]
                        let mutable tieEnd = tieStart + 2
                        while tieEnd < count && dists[tieEnd] = dTie do
                            tieEnd <- tieEnd + 1
                        Array.Sort(ids, tieStart, tieEnd - tieStart)
                        tieStart <- tieEnd
                    else
                        tieStart <- tieStart + 1
#else
                Array.sortInPlaceWith (fun a b ->
                    let order = compare (distance scaled[a] c) (distance scaled[b] c)
                    if order = 0 then compare a b else order) ids
#endif
                let inline visible i a b = Predicates.orient2d points[i] points[a] points[b] > 0
                let mutable valid = true
                let mutable idIdx = 0
                while valid && idIdx < count do
                    let i = ids[idIdx]
                    if i <> i0 && i <> i1 && i <> i2 then
                        let mutable found, j = -1, 0
                        let k = key i
                        while found < 0 && j < hashSize do
                            let h = hash[(k + j) % hashSize]
                            if h >= 0 && h <> next[h] then found <- h
                            j <- j + 1
                        if found < 0 then valid <- false
                        else
                            let initial = previous[found]
                            let mutable e = initial
                            let mutable searching = true
                            while searching do
                                if visible i e next[e] then searching <- false
                                else
                                    e <- next[e]
                                    if e = initial then e <- -1; searching <- false
                            if e < 0 then valid <- false
                            else
                                let t = add e i next[e] -1 -1 hullTri[e]
                                hullTri[i] <- legalize (t + 2); hullTri[e] <- t
                                let mutable n = next[e]
                                while visible i n next[n] do
                                    let q = next[n]
                                    let t2 = add n i q hullTri[i] -1 hullTri[n]
                                    hullTri[i] <- legalize (t2 + 2); next[n] <- n; n <- q
                                if e = initial then
                                    while visible i previous[e] e do
                                        let q = previous[e]
                                        let t2 = add q i e -1 hullTri[e] hullTri[q]
                                        legalize (t2 + 2) |> ignore
                                        hullTri[q] <- t2; next[e] <- e; e <- q
                                start <- e; previous[i] <- e
                                next[e] <- i; previous[n] <- i; next[i] <- n
                                hash[key i] <- i; hash[key e] <- e
                    idIdx <- idIdx + 1
                if not valid then ValueNone
                else
                    // Convert CW triangles & half-edges to Cartesian CCW via flip_halfedge involution
                    // (proven in GeometryGeo.v: delaunay_cw_to_ccw_halfedge_involution).
                    let nHalf = triangles.Count
                    let result = Array.zeroCreate<int> nHalf
                    let oppResult = Array.zeroCreate<int> nHalf
                    let inline flip e =
                        if e < 0 then -1
                        else
                            let r = e % 3
                            if r = 0 then e + 2 elif r = 2 then e - 2 else e
                    let mutable k = 0
                    while k < nHalf do
                        result[k] <- triangles[k]
                        result[k + 1] <- triangles[k + 2]
                        result[k + 2] <- triangles[k + 1]
                        oppResult[k] <- flip opposite[k + 2]
                        oppResult[k + 1] <- flip opposite[k + 1]
                        oppResult[k + 2] <- flip opposite[k]
                        k <- k + 3
                    ValueSome struct(result, oppResult)

    let fallback (points:Point[]) (hull:int[]) =
        let triangles=ResizeArray<struct(int*int*int)>()
        for i in 1..hull.Length-2 do triangles.Add(struct(hull[0],hull[i],hull[i+1]))
        let hullSet=HashSet<int>(hull)
        for p in 0..points.Length-1 do
            if not(hullSet.Contains p) then
                let boundary=Dictionary<struct(int*int),struct(int*int)>()
                let removed=ResizeArray<int>()
                for i in 0..triangles.Count-1 do
                    let struct(a,b,c)=triangles[i]
                    if Predicates.orient2d points[a] points[b] points[p]>=0 && Predicates.orient2d points[b] points[c] points[p]>=0 && Predicates.orient2d points[c] points[a] points[p]>=0 then
                        removed.Add i
                        for (u,v) in [a,b;b,c;c,a] do
                            let key=struct(min u v,max u v)
                            if boundary.ContainsKey key then boundary.Remove key |> ignore else boundary.Add(key,struct(u,v))
                if removed.Count=0 then invalidOp "Unable to locate a site in the exact hull triangulation."
                for i in removed |> Seq.rev do triangles.RemoveAt i
                for struct(a,b) in boundary.Values do
                    if Predicates.orient2d points[a] points[b] points[p]>0 then triangles.Add(struct(a,b,p))
        let mutable changed=true
        while changed do
            changed<-false
            let edges=Dictionary<struct(int*int),struct(int*int*int*int)>()
            let mutable i=0
            while i<triangles.Count && not changed do
                let struct(a,b,c)=triangles[i]
                for (u,v,w) in [a,b,c;b,c,a;c,a,b] do
                    if not changed then
                        match edges.TryGetValue(struct(v,u)) with
                        | true,struct(j,_,_,d) when Predicates.incircle points[u] points[v] points[w] points[d]>0 ->
                            triangles[i]<-struct(w,d,v)
                            triangles[j]<-struct(d,w,u)
                            changed<-true
                        | _ -> edges[struct(u,v)]<-struct(i,u,v,w)
                i<-i+1
        triangles |> Seq.collect(fun struct(a,b,c) -> [a;b;c]) |> Seq.toArray

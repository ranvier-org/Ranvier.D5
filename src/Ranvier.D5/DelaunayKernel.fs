namespace Ranvier.D5

open System
open System.Collections.Generic

// Advancing-hull construction adapted from Delaunator 5.1.0 (ISC, Mapbox).
// Topology uses original-coordinate exact predicates; normalized coordinates only guide search.
module internal DelaunayKernel =
    let fast (points:Point[]) =
        let count=points.Length
        let maxCoordinate=points |> Array.fold(fun m p -> max m (max(abs p.X)(abs p.Y))) 0.
        let exponent=if maxCoordinate=0. then 0 else Math.ILogB maxCoordinate
        let scaled=points |> Array.map(fun p -> Point.create (Math.ScaleB(p.X,-exponent)) (Math.ScaleB(p.Y,-exponent)))
        let distinct=HashSet<struct(float*float)>()
        let preserved=scaled |> Array.forall(fun p -> distinct.Add(struct(p.X,p.Y)))
        let distance a b=let dx,dy=a.X-b.X,a.Y-b.Y in dx*dx+dy*dy
        let center a b c =
            let dx,dy,ex,ey=b.X-a.X,b.Y-a.Y,c.X-a.X,c.Y-a.Y
            let bl,cl=dx*dx+dy*dy,ex*ex+ey*ey
            let d=0.5/(dx*ey-dy*ex)
            let x,y=a.X+(ey*bl-dy*cl)*d,a.Y+(dx*cl-ex*bl)*d
            if Double.IsFinite x && Double.IsFinite y then Some(Point.create x y) else None
        let x0,x1=scaled |> Array.minBy(fun p -> p.X),scaled |> Array.maxBy(fun p -> p.X)
        let y0,y1=scaled |> Array.minBy(fun p -> p.Y),scaled |> Array.maxBy(fun p -> p.Y)
        let midpoint=Point.create ((x0.X+x1.X)/2.) ((y0.Y+y1.Y)/2.)
        let ids=Array.init count id
        let i0=ids |> Array.minBy(fun i -> distance scaled[i] midpoint,i)
        let i1=ids |> Array.filter((<>) i0) |> Array.minBy(fun i -> distance scaled[i0] scaled[i],i)
        let mutable seed=None
        let mutable radius=infinity
        for i in ids do
            if i<>i0 && i<>i1 && Predicates.orient2d points[i0] points[i1] points[i]<>0 then
                match center scaled[i0] scaled[i1] scaled[i] with
                | Some c when distance c scaled[i0]<radius -> seed<-Some(i,c);radius<-distance c scaled[i0]
                | _ -> ()
        match seed with
        | None -> None
        | Some _ when not preserved -> None
        | Some(i2,c) ->
            let i1,i2=if Predicates.orient2d points[i0] points[i1] points[i2]>0 then i2,i1 else i1,i2
            let triangles=ResizeArray<int>()
            let opposite=ResizeArray<int>()
            let previous,next,hullTri=Array.zeroCreate count,Array.zeroCreate count,Array.zeroCreate count
            let hashSize=max 1 (int(ceil(sqrt(float count))))
            let hash=Array.create hashSize -1
            let key i =
                let dx,dy=scaled[i].X-c.X,scaled[i].Y-c.Y
                let sum=abs dx+abs dy
                let p=if sum=0. then 0. else dx/sum
                int(floor((if dy>0. then 3.-p else 1.+p)/4.*float hashSize))%hashSize
            let mutable start=i0
            let link a b = opposite[a]<-b;if b>=0 then opposite[b]<-a
            let add a b c x y z =
                let t=triangles.Count
                triangles.Add a;triangles.Add b;triangles.Add c
                opposite.Add -1;opposite.Add -1;opposite.Add -1
                link t x;link (t+1) y;link (t+2) z
                t
            let legalize edge =
                let stack=Stack<int>()
                let mutable a,ar,running=edge,0,true
                while running do
                    let b=opposite[a]
                    let a0=a-a%3
                    ar<-a0+(a+2)%3
                    if b>=0 then
                        let b0=b-b%3
                        let al,bl=a0+(a+1)%3,b0+(b+2)%3
                        let p0,pr,pl,p1=triangles[ar],triangles[a],triangles[al],triangles[bl]
                        if Predicates.incircle points[p0] points[pr] points[pl] points[p1]<0 then
                            triangles[a]<-p1;triangles[b]<-p0
                            let hbl=opposite[bl]
                            if hbl = -1 then
                                let mutable e,search=start,true
                                while search do
                                    if hullTri[e]=bl then hullTri[e]<-a;search<-false
                                    else
                                        e<-previous[e]
                                        if e=start then search<-false
                            link a hbl;link b opposite[ar];link ar bl
                            stack.Push(b0+(b+1)%3)
                        elif stack.Count=0 then running<-false
                        else a<-stack.Pop()
                    elif stack.Count=0 then running<-false
                    else a<-stack.Pop()
                ar
            next[i0]<-i1;previous[i2]<-i1
            next[i1]<-i2;previous[i0]<-i2
            next[i2]<-i0;previous[i1]<-i0
            hullTri[i0]<-0;hullTri[i1]<-1;hullTri[i2]<-2
            hash[key i0]<-i0;hash[key i1]<-i1;hash[key i2]<-i2
            add i0 i1 i2 -1 -1 -1 |> ignore
            Array.sortInPlaceWith(fun a b -> let order=compare (distance scaled[a] c) (distance scaled[b] c) in if order=0 then compare a b else order) ids
            let visible i a b=Predicates.orient2d points[i] points[a] points[b]>0
            let mutable valid=true
            for i in ids do
                if valid && i<>i0 && i<>i1 && i<>i2 then
                    let mutable found,j = -1,0
                    let k=key i
                    while found<0 && j<hashSize do
                        let h=hash[(k+j)%hashSize]
                        if h>=0 && h<>next[h] then found<-h
                        j<-j+1
                    if found<0 then valid<-false else
                    let initial=previous[found]
                    let mutable e=initial
                    let mutable searching=true
                    while searching do
                        if visible i e next[e] then searching<-false
                        else
                            e<-next[e]
                            if e=initial then e<- -1;searching<-false
                    if e<0 then valid<-false else
                    let t=add e i next[e] -1 -1 hullTri[e]
                    hullTri[i]<-legalize(t+2);hullTri[e]<-t
                    let mutable n=next[e]
                    while visible i n next[n] do
                        let q=next[n]
                        let t=add n i q hullTri[i] -1 hullTri[n]
                        hullTri[i]<-legalize(t+2);next[n]<-n;n<-q
                    if e=initial then
                        while visible i previous[e] e do
                            let q=previous[e]
                            let t=add q i e -1 hullTri[e] hullTri[q]
                            legalize(t+2) |> ignore
                            hullTri[q]<-t;next[e]<-e;e<-q
                    start<-e;previous[i]<-e
                    next[e]<-i;previous[n]<-i;next[i]<-n
                    hash[key i]<-i;hash[key e]<-e
            if not valid then None else
            // Convert the kernel's clockwise triangles to the public Cartesian CCW convention.
            let result=triangles.ToArray()
            for i in 0..3..result.Length-1 do
                let b=result[i+1]
                result[i+1]<-result[i+2];result[i+2]<-b
            Some result

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

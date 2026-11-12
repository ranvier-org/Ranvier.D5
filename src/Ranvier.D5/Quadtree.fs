namespace Ranvier.D5

/// <summary>Finite endpoint bounds; their span need not fit in a double.</summary>
type QuadExtent = { Min:Point; Max:Point }

/// <summary>A captured coordinate and nonnegative mass with original input identity.</summary>
type QuadItem<'T> = { Value:'T; Position:Point; Mass:float; Index:int }

/// <summary>An immutable cell summary. Items are present only in leaf buckets.</summary>
type QuadCell<'T> = { Extent:QuadExtent; Count:int; Mass:float; Center:Point; Depth:int; Items:QuadItem<'T> list }

type private QuadNode<'T> = QuadNode of QuadCell<'T> * QuadNode<'T>[]

/// <summary>An opaque, immutable point-region quadtree with per-cell mass summaries;
/// leaves hold at most eight items unless further splitting is impossible.</summary>
type Quadtree<'T> = private Quadtree of QuadNode<'T> option

/// <summary>Builds and queries <c>Quadtree</c> values; non-finite positions or invalid masses throw <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module Quadtree =
    let private interpolate a b t =
        if (a>=0.)=(b>=0.) then a+(b-a)*t else a*(1.-t)+b*t

    let private validate (e:QuadExtent) =
        Point.create e.Min.X e.Min.Y |> ignore
        Point.create e.Max.X e.Max.Y |> ignore
        if e.Min.X>e.Max.X || e.Min.Y>e.Max.Y then invalidArg "extent" "Bounds must be ordered."

    /// <summary>Captures each position and nonnegative mass once. Subdivision stops at depth 64 or when no interior split is representable.</summary>
    let buildWithCancellation (token:System.Threading.CancellationToken) (position:'T -> Point) (mass:'T -> float) (items:seq<'T>) =
        token.ThrowIfCancellationRequested()
        let check i=if i%64=0 then token.ThrowIfCancellationRequested()
        let captured=items |> Seq.mapi(fun i value ->
            check i
            let p,m=position value,mass value
            Point.create p.X p.Y |> ignore
            Check.finite "mass" m
            if m<0. then invalidArg "mass" "Mass must be nonnegative."
            {Value=value;Position=p;Mass=m;Index=i}) |> Seq.toArray
        let rec buildNode depth (rows:QuadItem<'T>[]) =
            token.ThrowIfCancellationRequested()
            let mutable x0,y0,x1,y1=rows[0].Position.X,rows[0].Position.Y,rows[0].Position.X,rows[0].Position.Y
            let mutable total,cx,cy=0.,0.,0.
            for index in 0..rows.Length-1 do
                check index
                let item=rows[index]
                let p=item.Position
                x0<-min x0 p.X; y0<-min y0 p.Y; x1<-max x1 p.X; y1<-max y1 p.Y
                if item.Mass>0. then
                    let next=total+item.Mass
                    Check.finite "mass" next
                    let t=item.Mass/next
                    cx<-interpolate cx p.X t; cy<-interpolate cy p.Y t; total<-next
            let mx,my=interpolate x0 x1 0.5,interpolate y0 y1 0.5
            let splitX,splitY=(mx > x0 && mx < x1),(my > y0 && my < y1)
            let children=
                if rows.Length<=8 || depth>=64 || not(splitX || splitY) then [||]
                else
                    let buckets=Array.init 4 (fun _ -> ResizeArray<QuadItem<'T>>())
                    for index in 0..rows.Length-1 do
                        check index
                        let item=rows[index]
                        let quadrant=(if splitX && item.Position.X>=mx then 1 else 0)+(if splitY && item.Position.Y>=my then 2 else 0)
                        buckets[quadrant].Add item
                    buckets |> Array.choose(fun bucket -> if bucket.Count=0 then None else Some(buildNode (depth+1) (bucket.ToArray())))
            let mutable leafItems=[]
            if children.Length=0 then
                for index in rows.Length-1 .. -1 .. 0 do
                    check index
                    leafItems<-rows[index]::leafItems
            let cell={Extent={Min=Point.create x0 y0;Max=Point.create x1 y1};Count=rows.Length;Mass=total
                      Center=Point.create (if total=0. then mx else cx) (if total=0. then my else cy)
                      Depth=depth;Items=leafItems}
            QuadNode(cell,children)
        Quadtree(if captured.Length=0 then None else Some(buildNode 0 captured))

    /// <summary>Captures an immutable quadtree without cancellation; use buildWithCancellation for cancellable worker construction.</summary>
    let build position mass items = buildWithCancellation System.Threading.CancellationToken.None position mass items

    /// <summary>Returns the number of captured items, including those at duplicate positions.</summary>
    let count (Quadtree root) = match root with None -> 0 | Some(QuadNode(cell,_)) -> cell.Count

    // Force passes query the same immutable tree once per node; keep the traversal buffer local to that pass.
    let internal visitIndexed count visitor (Quadtree root) =
        match root with
        | None -> ()
        | Some node ->
            let stack=System.Collections.Generic.Stack<QuadNode<'T>>()
            for index in 0..count-1 do
                stack.Push node
                while stack.Count>0 do
                    let (QuadNode(cell,children))=stack.Pop()
                    if visitor index cell then
                        for i in children.Length-1 .. -1 .. 0 do stack.Push children[i]

    /// <summary>Visits cells in quadrant order; false prunes the current cell's descendants. Summaries and leaf items are immutable snapshots.</summary>
    let visit visitor (Quadtree root) =
        let stack=System.Collections.Generic.Stack<QuadNode<'T>>()
        root |> Option.iter stack.Push
        while stack.Count>0 do
            let (QuadNode(cell,children))=stack.Pop()
            if visitor cell then
                for i in children.Length-1 .. -1 .. 0 do stack.Push children[i]

    /// <summary>Returns original values inside inclusive endpoint bounds, in input order.</summary>
    let within extent tree =
        validate extent
        let results=ResizeArray<QuadItem<'T>>()
        visit (fun cell ->
            let e=cell.Extent
            let overlaps=e.Max.X>=extent.Min.X && e.Min.X<=extent.Max.X && e.Max.Y>=extent.Min.Y && e.Min.Y<=extent.Max.Y
            if overlaps then
                for item in cell.Items do
                    let p=item.Position
                    if p.X>=extent.Min.X && p.X<=extent.Max.X && p.Y>=extent.Min.Y && p.Y<=extent.Max.Y then results.Add item
            overlaps) tree
        results |> Seq.sortBy(fun item -> item.Index) |> Seq.map(fun item -> item.Value) |> Seq.toList

    let private distance (q:Point) (p:Point) = System.Double.Hypot(q.X-p.X,q.Y-p.Y)
    let private exactDistances (q:Point) (a:Point) (b:Point) =
        let v,_=ExactDyadic.integers [|q.X;q.Y;a.X;a.Y;b.X;b.Y|]
        let ax,ay,bx,by=v[2]-v[0],v[3]-v[1],v[4]-v[0],v[5]-v[1]
        compare (ax*ax+ay*ay) (bx*bx+by*by)
    let private compareDistance q a b =
        let da,db=distance q a,distance q b
        if System.Double.IsFinite da && System.Double.IsFinite db && abs(da-db)>max da db*1e-14 then compare da db
        elif a=b then 0
        else exactDistances q a b
    let private insideRadius q p radius =
        if System.Double.IsPositiveInfinity radius then true else
        let d=distance q p
        if System.Double.IsFinite d && abs(d-radius)>max d radius*1e-14 then d<radius else
        let v,_=ExactDyadic.integers [|q.X;q.Y;p.X;p.Y;radius|]
        let dx,dy=v[2]-v[0],v[3]-v[1]
        dx*dx+dy*dy<=v[4]*v[4]

    /// <summary>Returns the nearest original value within an inclusive radius; exact distance ties use input order. Positive infinity means unbounded.</summary>
    let nearest query radius (Quadtree root) =
        Point.create query.X query.Y |> ignore
        if System.Double.IsNaN radius || radius<0. then invalidArg "radius" "Radius must be nonnegative."
        let closest e=Point.create (max e.Min.X (min e.Max.X query.X)) (max e.Min.Y (min e.Max.Y query.Y))
        let pending=System.Collections.Generic.PriorityQueue<QuadNode<'T>,float>()
        root |> Option.iter(fun (QuadNode(cell,_) as node) -> pending.Enqueue(node,distance query (closest cell.Extent)))
        let mutable best:QuadItem<'T> option=None
        while pending.Count>0 do
            let (QuadNode(cell,children))=pending.Dequeue()
            let near=closest cell.Extent
            let possible=match best with Some item -> compareDistance query near item.Position<=0 | None -> insideRadius query near radius
            if possible then
                for item in cell.Items do
                    let better=
                        match best with
                        | None -> insideRadius query item.Position radius
                        | Some previous ->
                            let order=compareDistance query item.Position previous.Position
                            order<0 || (order=0 && item.Index<previous.Index)
                    if better then best<-Some item
                for (QuadNode(child,_) as node) in children do pending.Enqueue(node,distance query (closest child.Extent))
        best |> Option.map(fun item -> item.Value)

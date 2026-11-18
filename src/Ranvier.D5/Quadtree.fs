namespace Ranvier.D5

open System

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
        let inline check i = if (i &&& 63) = 0 then token.ThrowIfCancellationRequested()
        let inline makeItem i value =
            let p = position value
            let m = mass value
            Check.finite "x" p.X
            Check.finite "y" p.Y
            Check.finite "mass" m
            if m < 0. then invalidArg "mass" "Mass must be nonnegative."
            { Value = value; Position = p; Mass = m; Index = i }
        let captured =
            match box items with
            | :? ('T array) as arr ->
                let res = Array.zeroCreate<QuadItem<'T>> arr.Length
                for i = 0 to arr.Length - 1 do
                    check i
                    res[i] <- makeItem i arr[i]
                res
            | :? ('T list) as lst ->
                let n = List.length lst
                let res = Array.zeroCreate<QuadItem<'T>> n
                let mutable i = 0
                for v in lst do
                    check i
                    res[i] <- makeItem i v
                    i <- i + 1
                res
            | _ ->
                let buf = ResizeArray<QuadItem<'T>>()
                let mutable i = 0
                for v in items do
                    check i
                    buf.Add(makeItem i v)
                    i <- i + 1
                buf.ToArray()
        if captured.Length = 0 then Quadtree None
        else
            let scratch = Array.zeroCreate<QuadItem<'T>> captured.Length
            let rec buildSlice depth offset count =
                token.ThrowIfCancellationRequested()
                let firstItem = captured[offset]
                let mutable x0 = firstItem.Position.X
                let mutable y0 = firstItem.Position.Y
                let mutable x1 = x0
                let mutable y1 = y0
                let mutable total = 0.
                let mutable cx = 0.
                let mutable cy = 0.
                for k = 0 to count - 1 do
                    check k
                    let item = captured[offset + k]
                    let p = item.Position
                    if p.X < x0 then x0 <- p.X
                    if p.Y < y0 then y0 <- p.Y
                    if p.X > x1 then x1 <- p.X
                    if p.Y > y1 then y1 <- p.Y
                    if item.Mass > 0. then
                        let next = total + item.Mass
                        Check.finite "mass" next
                        let t = item.Mass / next
                        cx <- interpolate cx p.X t
                        cy <- interpolate cy p.Y t
                        total <- next
                let mx = interpolate x0 x1 0.5
                let my = interpolate y0 y1 0.5
                let splitX = mx > x0 && mx < x1
                let splitY = my > y0 && my < y1
                let children =
                    if count <= 8 || depth >= 64 || not (splitX || splitY) then [||]
                    else
                        let mutable c0 = 0
                        let mutable c1 = 0
                        let mutable c2 = 0
                        let mutable c3 = 0
                        for k = 0 to count - 1 do
                            check k
                            let p = captured[offset + k].Position
                            let q = (if splitX && p.X >= mx then 1 else 0) + (if splitY && p.Y >= my then 2 else 0)
                            match q with
                            | 0 -> c0 <- c0 + 1
                            | 1 -> c1 <- c1 + 1
                            | 2 -> c2 <- c2 + 1
                            | _ -> c3 <- c3 + 1
                        let mutable w0 = 0
                        let mutable w1 = c0
                        let mutable w2 = c0 + c1
                        let mutable w3 = c0 + c1 + c2
                        for k = 0 to count - 1 do
                            let item = captured[offset + k]
                            let p = item.Position
                            let q = (if splitX && p.X >= mx then 1 else 0) + (if splitY && p.Y >= my then 2 else 0)
                            match q with
                            | 0 -> scratch[w0] <- item; w0 <- w0 + 1
                            | 1 -> scratch[w1] <- item; w1 <- w1 + 1
                            | 2 -> scratch[w2] <- item; w2 <- w2 + 1
                            | _ -> scratch[w3] <- item; w3 <- w3 + 1
                        Array.Copy(scratch, 0, captured, offset, count)
                        let childCount =
                            (if c0 > 0 then 1 else 0) + (if c1 > 0 then 1 else 0) +
                            (if c2 > 0 then 1 else 0) + (if c3 > 0 then 1 else 0)
                        let childArr = Array.zeroCreate<QuadNode<'T>> childCount
                        let mutable cw = 0
                        if c0 > 0 then childArr[cw] <- buildSlice (depth + 1) offset c0; cw <- cw + 1
                        if c1 > 0 then childArr[cw] <- buildSlice (depth + 1) (offset + c0) c1; cw <- cw + 1
                        if c2 > 0 then childArr[cw] <- buildSlice (depth + 1) (offset + c0 + c1) c2; cw <- cw + 1
                        if c3 > 0 then childArr[cw] <- buildSlice (depth + 1) (offset + c0 + c1 + c2) c3
                        childArr
                let mutable leafItems = []
                if children.Length = 0 then
                    for k = count - 1 downto 0 do
                        check k
                        leafItems <- captured[offset + k] :: leafItems
                let cell =
                    { Extent = { Min = { Point.X = x0; Y = y0 }; Max = { Point.X = x1; Y = y1 } }
                      Count = count
                      Mass = total
                      Center = Point.create (if total = 0. then mx else cx) (if total = 0. then my else cy)
                      Depth = depth
                      Items = leafItems }
                QuadNode(cell, children)
            Quadtree(Some(buildSlice 0 0 captured.Length))

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

    let private distance (q:Point) (p:Point) = Double.Hypot(q.X-p.X,q.Y-p.Y)
    let private exactDistances (q:Point) (a:Point) (b:Point) =
        let v,_=ExactDyadic.integers [|q.X;q.Y;a.X;a.Y;b.X;b.Y|]
        let ax,ay,bx,by=v[2]-v[0],v[3]-v[1],v[4]-v[0],v[5]-v[1]
        compare (ax*ax+ay*ay) (bx*bx+by*by)
    let private compareDistance q a b =
        let da,db=distance q a,distance q b
        if Double.IsFinite da && Double.IsFinite db && abs(da-db)>max da db*1e-14 then compare da db
        elif a=b then 0
        else exactDistances q a b
    let private insideRadius q p radius =
        if System.Double.IsPositiveInfinity radius then true else
        let d=distance q p
        if Double.IsFinite d && abs(d-radius)>max d radius*1e-14 then d<radius else
        let v,_=ExactDyadic.integers [|q.X;q.Y;p.X;p.Y;radius|]
        let dx,dy=v[2]-v[0],v[3]-v[1]
        dx*dx+dy*dy<=v[4]*v[4]

    /// <summary>Returns the nearest original value within an inclusive radius; exact distance ties use input order. Positive infinity means unbounded.</summary>
    let nearest query radius (Quadtree root) =
        Check.finite "x" query.X
        Check.finite "y" query.Y
        if System.Double.IsNaN radius || radius < 0. then invalidArg "radius" "Radius must be nonnegative."
        match root with
        | None -> None
        | Some(QuadNode(rootCell, _) as rootNode) ->
            let qx = query.X
            let qy = query.Y
            let inline closest (e: QuadExtent) =
                { Point.X = max e.Min.X (min e.Max.X qx)
                  Y = max e.Min.Y (min e.Max.Y qy) }
            let maxExt =
                max
                    (max (abs rootCell.Extent.Min.X) (abs rootCell.Extent.Max.X))
                    (max (abs rootCell.Extent.Min.Y) (abs rootCell.Extent.Max.Y))
            let fastSq = maxExt < 1e140 && abs qx < 1e140 && abs qy < 1e140
            let inline compareDist (a: Point) (b: Point) =
                if fastSq then
                    let dax = qx - a.X
                    let day = qy - a.Y
                    let dbx = qx - b.X
                    let dby = qy - b.Y
                    let da2 = dax * dax + day * day
                    let db2 = dbx * dbx + dby * dby
                    if abs (da2 - db2) > max da2 db2 * 1e-12 && max da2 db2 > 1e-280 then
                        da2.CompareTo db2
                    elif a = b then 0
                    else compareDistance query a b
                else compareDistance query a b
            let inline withinRad (p: Point) =
                if Double.IsPositiveInfinity radius then true
                elif fastSq && radius < 1e140 then
                    let dx = qx - p.X
                    let dy = qy - p.Y
                    let d2 = dx * dx + dy * dy
                    let r2 = radius * radius
                    if abs (d2 - r2) > max d2 r2 * 1e-12 && max d2 r2 > 1e-280 then d2 < r2
                    else insideRadius query p radius
                else insideRadius query p radius
            let inline priorityOf (p: Point) =
                if fastSq then
                    let dx = qx - p.X
                    let dy = qy - p.Y
                    dx * dx + dy * dy
                else distance query p
#if FABLE_COMPILER
            let pending = PriorityQueue<QuadNode<'T>, float>()
#else
            let pending = System.Collections.Generic.PriorityQueue<QuadNode<'T>, float>(32)
#endif
            pending.Enqueue(rootNode, priorityOf (closest rootCell.Extent))
            let mutable hasBest = false
            let mutable bestItem = Unchecked.defaultof<QuadItem<'T>>
            while pending.Count > 0 do
                let (QuadNode(cell, children)) = pending.Dequeue()
                let near = closest cell.Extent
                let possible =
                    if hasBest then compareDist near bestItem.Position <= 0
                    else withinRad near
                if possible then
                    for item in cell.Items do
                        let better =
                            if not hasBest then withinRad item.Position
                            else
                                let order = compareDist item.Position bestItem.Position
                                order < 0 || (order = 0 && item.Index < bestItem.Index)
                        if better then
                            hasBest <- true
                            bestItem <- item
                    for i = 0 to children.Length - 1 do
                        let (QuadNode(child, _) as node) = children[i]
                        let cNear = closest child.Extent
                        let childPossible =
                            if hasBest then compareDist cNear bestItem.Position <= 0
                            else withinRad cNear
                        if childPossible then
                            pending.Enqueue(node, priorityOf cNear)
            if hasBest then Some bestItem.Value else None

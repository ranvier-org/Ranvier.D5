namespace Ranvier.D5

open System
open System.Collections.Generic

[<Struct>]
type private FlatSpatialNode<'T> =
    { Px: float
      Py: float
      Item: 'T
      Order: int
      mutable Left: int
      mutable Right: int
      mutable SplitX: bool }

/// <summary>An immutable balanced spatial index with original-order tie breaking.</summary>
type SpatialIndex<'T> =
    private
        { Nodes: FlatSpatialNode<'T> array
          Root: int
          MaxCoordAbs: float }

/// <summary>Builds and queries <c>SpatialIndex</c> values; non-finite points throw <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module Spatial =
    let private xComparer<'T> () =
        { new IComparer<FlatSpatialNode<'T>> with
            member _.Compare(a, b) =
                let c = a.Px.CompareTo b.Px
                if c <> 0 then c else a.Order.CompareTo b.Order }

    let private yComparer<'T> () =
        { new IComparer<FlatSpatialNode<'T>> with
            member _.Compare(a, b) =
                let c = a.Py.CompareTo b.Py
                if c <> 0 then c else a.Order.CompareTo b.Order }

    /// <summary>Builds a balanced k-d tree from a copy in O(n log-squared n); points must be finite.</summary>
    let create (project: 'T -> Point) (items: seq<'T>) =
        let mutable maxAbs = 0.
        let nodes =
            match box items with
            | :? ('T array) as arr ->
                let res = Array.zeroCreate<FlatSpatialNode<'T>> arr.Length
                for i = 0 to arr.Length - 1 do
                    let item = arr[i]
                    let pt = project item
                    Check.finite "x" pt.X
                    Check.finite "y" pt.Y
                    let ax = abs pt.X
                    let ay = abs pt.Y
                    if ax > maxAbs then maxAbs <- ax
                    if ay > maxAbs then maxAbs <- ay
                    res[i] <- { Px = pt.X; Py = pt.Y; Item = item; Order = i; Left = -1; Right = -1; SplitX = true }
                res
            | :? ('T list) as lst ->
                let n = List.length lst
                let res = Array.zeroCreate<FlatSpatialNode<'T>> n
                let mutable i = 0
                for item in lst do
                    let pt = project item
                    Check.finite "x" pt.X
                    Check.finite "y" pt.Y
                    let ax = abs pt.X
                    let ay = abs pt.Y
                    if ax > maxAbs then maxAbs <- ax
                    if ay > maxAbs then maxAbs <- ay
                    res[i] <- { Px = pt.X; Py = pt.Y; Item = item; Order = i; Left = -1; Right = -1; SplitX = true }
                    i <- i + 1
                res
            | _ ->
                let buf = ResizeArray<FlatSpatialNode<'T>>()
                let mutable i = 0
                for item in items do
                    let pt = project item
                    Check.finite "x" pt.X
                    Check.finite "y" pt.Y
                    let ax = abs pt.X
                    let ay = abs pt.Y
                    if ax > maxAbs then maxAbs <- ax
                    if ay > maxAbs then maxAbs <- ay
                    buf.Add { Px = pt.X; Py = pt.Y; Item = item; Order = i; Left = -1; Right = -1; SplitX = true }
                    i <- i + 1
                buf.ToArray()
        if nodes.Length = 0 then
            { Nodes = nodes; Root = -1; MaxCoordAbs = 0. }
        else
#if !FABLE_COMPILER
            let cmpX = xComparer<'T> ()
            let cmpY = yComparer<'T> ()
            let rec buildRange splitX lo hi =
                if lo > hi then -1
                elif lo = hi then
                    nodes[lo].SplitX <- splitX
                    nodes[lo].Left <- -1
                    nodes[lo].Right <- -1
                    lo
                else
                    Array.Sort(nodes, lo, hi - lo + 1, if splitX then cmpX else cmpY)
                    let mid = lo + (hi - lo) / 2
                    nodes[mid].SplitX <- splitX
                    nodes[mid].Left <- buildRange (not splitX) lo (mid - 1)
                    nodes[mid].Right <- buildRange (not splitX) (mid + 1) hi
                    mid
            let root = buildRange true 0 (nodes.Length - 1)
            { Nodes = nodes; Root = root; MaxCoordAbs = maxAbs }
#else
            let outNodes = Array.zeroCreate<FlatSpatialNode<'T>> nodes.Length
            let mutable writeIdx = 0
            let rec buildFable splitX (slice: FlatSpatialNode<'T> array) =
                if slice.Length = 0 then -1
                else
                    let sorted = slice |> Array.sortBy (fun nd -> (if splitX then nd.Px else nd.Py), nd.Order)
                    let mid = sorted.Length / 2
                    let curIdx = writeIdx
                    writeIdx <- writeIdx + 1
                    let mutable rootNd = sorted[mid]
                    rootNd.SplitX <- splitX
                    rootNd.Left <- buildFable (not splitX) sorted[.. mid - 1]
                    rootNd.Right <- buildFable (not splitX) sorted[mid + 1 ..]
                    outNodes[curIdx] <- rootNd
                    curIdx
            let root = buildFable true nodes
            { Nodes = outNodes; Root = root; MaxCoordAbs = maxAbs }
#endif

    /// <summary>Returns the nearest item within an inclusive nonnegative radius. Positive infinity searches all points.</summary>
    let nearest (query: Point) radius (index: SpatialIndex<'T>) =
        Check.finite "x" query.X
        Check.finite "y" query.Y
        if Double.IsNaN radius || radius < 0. then invalidArg "radius" "Radius must be nonnegative."
        if index.Root < 0 then None
        else
            let nodes = index.Nodes
            let qx = query.X
            let qy = query.Y
            let mutable distance = radius
            let mutable found = false
            let mutable bestItem = Unchecked.defaultof<'T>
            let mutable bestOrder = Int32.MaxValue
            if index.MaxCoordAbs < 1e140 && abs qx < 1e140 && abs qy < 1e140 then
                let inline sqLimit d =
                    if Double.IsPositiveInfinity d || d > 1e140 then Double.PositiveInfinity
                    else d * d * (1. + 1e-12) + 1e-300
                let mutable distSqLimit = sqLimit distance
                let rec searchFast idx =
                    if idx >= 0 then
                        let nd = nodes[idx]
                        let dx = nd.Px - qx
                        let dy = nd.Py - qy
                        let d2 = dx * dx + dy * dy
                        if d2 <= distSqLimit then
                            let d = Double.Hypot(dx, dy)
                            if d < distance || (d = distance && nd.Order < bestOrder) then
                                distance <- d
                                distSqLimit <- sqLimit d
                                found <- true
                                bestItem <- nd.Item
                                bestOrder <- nd.Order
                        let delta = if nd.SplitX then qx - nd.Px else qy - nd.Py
                        let nearIdx, farIdx = if delta < 0. then nd.Left, nd.Right else nd.Right, nd.Left
                        searchFast nearIdx
                        if abs delta <= distance then searchFast farIdx
                searchFast index.Root
            else
                let rec searchExact idx =
                    if idx >= 0 then
                        let nd = nodes[idx]
                        let d = Double.Hypot(nd.Px - qx, nd.Py - qy)
                        Check.finite "query" d
                        if d < distance || (d = distance && nd.Order < bestOrder) then
                            distance <- d
                            found <- true
                            bestItem <- nd.Item
                            bestOrder <- nd.Order
                        let delta = if nd.SplitX then qx - nd.Px else qy - nd.Py
                        let nearIdx, farIdx = if delta < 0. then nd.Left, nd.Right else nd.Right, nd.Left
                        searchExact nearIdx
                        if abs delta <= distance then searchExact farIdx
                searchExact index.Root
            if found then Some bestItem else None

    /// <summary>Returns items inside inclusive bounds in original input order.</summary>
    let within bounds (index: SpatialIndex<'T>) =
        if index.Root < 0 then []
        else
            let nodes = index.Nodes
            let bx0 = Bounds.x bounds
            let by0 = Bounds.y bounds
            let bx1 = bx0 + Bounds.width bounds
            let by1 = by0 + Bounds.height bounds
            let results = ResizeArray<struct(int * 'T)>()
            let rec search idx =
                if idx >= 0 then
                    let nd = nodes[idx]
                    if nd.Px >= bx0 && nd.Px <= bx1 && nd.Py >= by0 && nd.Py <= by1 then
                        results.Add(struct(nd.Order, nd.Item))
                    let low, high, value =
                        if nd.SplitX then bx0, bx1, nd.Px
                        else by0, by1, nd.Py
                    if low <= value then search nd.Left
                    if high >= value then search nd.Right
            search index.Root
            let n = results.Count
            if n = 0 then []
            else
                let arr = results.ToArray()
                Array.Sort(arr, Comparison<struct(int * 'T)>(fun struct(o1, _) struct(o2, _) -> o1.CompareTo o2))
                let mutable acc = []
                for i = n - 1 downto 0 do
                    let struct(_, item) = arr[i]
                    acc <- item :: acc
                acc

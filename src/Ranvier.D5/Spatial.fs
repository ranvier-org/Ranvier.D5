namespace Ranvier.D5

open System

type private SpatialNode<'T> =
    | Empty
    | Node of point:Point * item:'T * order:int * splitX:bool * left:SpatialNode<'T> * right:SpatialNode<'T>

/// <summary>An immutable balanced spatial index with original-order tie breaking.</summary>
type SpatialIndex<'T> = private SpatialIndex of SpatialNode<'T>

/// <summary>Builds and queries <c>SpatialIndex</c> values; non-finite points throw <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module Spatial =
    /// <summary>Builds a balanced k-d tree from a copy in O(n log-squared n); points must be finite.</summary>
    let create project items =
        let entries = items |> Seq.mapi (fun i item ->
            let point = project item
            Point.create point.X point.Y |> ignore
            point,item,i) |> Seq.toArray
        let rec build splitX entries =
            if Array.isEmpty entries then Empty
            else
                let sorted = entries |> Array.sortBy (fun (p,_,i) -> (if splitX then p.X else p.Y),i)
                let middle = sorted.Length/2
                let point,item,order = sorted[middle]
                Node(point,item,order,splitX,build (not splitX) sorted[..middle-1],build (not splitX) sorted[middle+1..])
        SpatialIndex(build true entries)
    /// <summary>Returns the nearest item within an inclusive nonnegative radius. Positive infinity searches all points.</summary>
    let nearest query radius (SpatialIndex root) =
        Point.create query.X query.Y |> ignore
        if Double.IsNaN radius || radius<0. then invalidArg "radius" "Radius must be nonnegative."
        let mutable distance = radius
        let mutable best = None
        let mutable bestOrder = Int32.MaxValue
        let rec search node =
            match node with
            | Empty -> ()
            | Node(point,item,order,splitX,left,right) ->
                let d = Double.Hypot(point.X-query.X, point.Y-query.Y)
                Check.finite "query" d
                if d<distance || (d=distance && order<bestOrder) then
                    distance <- d
                    best <- Some item
                    bestOrder <- order
                let delta = if splitX then query.X-point.X else query.Y-point.Y
                let near,far = if delta<0. then left,right else right,left
                search near
                if abs delta<=distance then search far
        search root
        best
    /// <summary>Returns items inside inclusive bounds in original input order.</summary>
    let within bounds (SpatialIndex root) =
        let results = ResizeArray<_>()
        let rec search node =
            match node with
            | Empty -> ()
            | Node(point,item,order,splitX,left,right) ->
                if Bounds.contains point bounds then results.Add(order,item)
                let low,high,value =
                    if splitX then Bounds.x bounds,Bounds.x bounds+Bounds.width bounds,point.X
                    else Bounds.y bounds,Bounds.y bounds+Bounds.height bounds,point.Y
                if low<=value then search left
                if high>=value then search right
        search root
        results |> Seq.sortBy fst |> Seq.map snd |> Seq.toList

namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>A captured 3D item with coordinate, nonnegative mass and original input index.</summary>
type OctItem<'T> =
    { /// <summary>Original payload value.</summary>
      Value: 'T
      /// <summary>Captured 3D coordinate.</summary>
      Position: Point3
      /// <summary>Captured nonnegative mass.</summary>
      Mass: float
      /// <summary>Zero-based insertion index for deterministic tie-breaking.</summary>
      Index: int }

/// <summary>An immutable octant summary; <c>Items</c> is populated only on leaf cells.</summary>
type OctCell<'T> =
    { /// <summary>Axis-aligned cuboid extent of captured points in this cell.</summary>
      Extent: Bounds3
      /// <summary>Total item count in this subtree.</summary>
      Count: int
      /// <summary>Total nonnegative mass in this subtree.</summary>
      Mass: float
      /// <summary>Mass-weighted 3D center of mass (or geometric midpoint when total mass is zero).</summary>
      Center: Point3
      /// <summary>Subdivision depth from root (0 at root).</summary>
      Depth: int
      /// <summary>Leaf items when not further subdivided.</summary>
      Items: OctItem<'T> list }

type private OctNode<'T> = OctNode of OctCell<'T> * OctNode<'T>[]

/// <summary>An opaque, immutable 3D point-region octree supporting O(log N) Barnes-Hut force aggregation, range queries and nearest-neighbor search.</summary>
type Octree<'T> = private Octree of OctNode<'T> option

/// <summary>Construction and spatial queries over 3D <c>Octree</c> values.</summary>
[<RequireQualifiedAccess>]
module Octree =
    let private interpolate a b t =
        if (a >= 0.) = (b >= 0.) then a + (b - a) * t else a * (1. - t) + b * t

    /// <summary>Builds an immutable 3D octree with cooperative cancellation; subdivides up to depth 48 with leaf capacity 8.</summary>
    let buildWithCancellation
        (token: System.Threading.CancellationToken)
        (position: 'T -> Point3)
        (mass: 'T -> float)
        (items: seq<'T>)
        : Octree<'T> =
        token.ThrowIfCancellationRequested()
        let captured =
            items
            |> Seq.mapi (fun i value ->
                Kernels3D.check token i
                let p = position value
                let m = mass value
                Point3.create p.X p.Y p.Z |> ignore
                if not (Double.IsFinite m) || m < 0. then invalidArg "mass" "Mass must be finite and nonnegative."
                { Value = value; Position = p; Mass = m; Index = i })
            |> Seq.toArray

        let rec buildNode depth (rows: OctItem<'T>[]) =
            token.ThrowIfCancellationRequested()
            let p0 = rows[0].Position
            let mutable x0, y0, z0 = p0.X, p0.Y, p0.Z
            let mutable x1, y1, z1 = p0.X, p0.Y, p0.Z
            let mutable total, cx, cy, cz = 0., 0., 0., 0.
            for idx = 0 to rows.Length - 1 do
                Kernels3D.check token idx
                let item = rows[idx]
                let p = item.Position
                if p.X < x0 then x0 <- p.X
                if p.Y < y0 then y0 <- p.Y
                if p.Z < z0 then z0 <- p.Z
                if p.X > x1 then x1 <- p.X
                if p.Y > y1 then y1 <- p.Y
                if p.Z > z1 then z1 <- p.Z
                if item.Mass > 0. then
                    let next = total + item.Mass
                    if not (Double.IsFinite next) then invalidArg "mass" "Total octree mass overflowed."
                    let t = item.Mass / next
                    cx <- interpolate cx p.X t
                    cy <- interpolate cy p.Y t
                    cz <- interpolate cz p.Z t
                    total <- next

            let mx, my, mz = interpolate x0 x1 0.5, interpolate y0 y1 0.5, interpolate z0 z1 0.5
            let sx, sy, sz = (mx > x0 && mx < x1), (my > y0 && my < y1), (mz > z0 && mz < z1)
            let children =
                if rows.Length <= 8 || depth >= 48 || not (sx || sy || sz) then [||]
                else
                    let buckets = Array.init 8 (fun _ -> ResizeArray<OctItem<'T>>())
                    for idx = 0 to rows.Length - 1 do
                        let p = rows[idx].Position
                        let oct =
                            (if sx && p.X >= mx then 1 else 0)
                            + (if sy && p.Y >= my then 2 else 0)
                            + (if sz && p.Z >= mz then 4 else 0)
                        buckets[oct].Add rows[idx]
                    buckets
                    |> Array.choose (fun b -> if b.Count = 0 then None else Some(buildNode (depth + 1) (b.ToArray())))

            let mutable leafItems = []
            if children.Length = 0 then
                for idx = rows.Length - 1 downto 0 do
                    leafItems <- rows[idx] :: leafItems

            let extent = Bounds3.create x0 y0 z0 (x1 - x0) (y1 - y0) (z1 - z0)
            let center =
                if total = 0. then Point3.create mx my mz
                else Point3.create cx cy cz
            OctNode({ Extent = extent; Count = rows.Length; Mass = total; Center = center; Depth = depth; Items = leafItems }, children)

        Octree(if captured.Length = 0 then None else Some(buildNode 0 captured))

    /// <summary>Builds an immutable 3D octree without cancellation.</summary>
    let build position mass items = buildWithCancellation System.Threading.CancellationToken.None position mass items

    /// <summary>Returns the total number of captured items in the octree.</summary>
    let count (Octree root) =
        match root with
        | None -> 0
        | Some(OctNode(cell, _)) -> cell.Count

    /// <summary>Visits octree cells depth-first; returning <c>false</c> prunes the current cell's children.</summary>
    let visit (visitor: OctCell<'T> -> bool) (Octree root) =
        match root with
        | None -> ()
        | Some node ->
            let stack = Stack<OctNode<'T>>()
            stack.Push node
            while stack.Count > 0 do
                let (OctNode(cell, children)) = stack.Pop()
                if visitor cell then
                    for i = children.Length - 1 downto 0 do stack.Push children[i]

    /// <summary>Reuses a single traversal stack across <c>0 .. count - 1</c> indexed force/collision queries.</summary>
    let internal visitIndexed (count: int) (visitor: int -> OctCell<'T> -> bool) (Octree root) =
        match root with
        | None -> ()
        | Some node ->
            let stack = Stack<OctNode<'T>>()
            for idx = 0 to count - 1 do
                stack.Push node
                while stack.Count > 0 do
                    let (OctNode(cell, children)) = stack.Pop()
                    if visitor idx cell then
                        for i = children.Length - 1 downto 0 do stack.Push children[i]

    /// <summary>Returns all original values lying inside inclusive 3D <c>bounds</c>, preserving original input order.</summary>
    let within (bounds: Bounds3) (tree: Octree<'T>) : 'T list =
        let bx0, by0, bz0 = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
        let bx1, by1, bz1 = Bounds3.maxX bounds, Bounds3.maxY bounds, Bounds3.maxZ bounds
        let hits = ResizeArray<OctItem<'T>>()
        visit (fun cell ->
            let e = cell.Extent
            let overlaps =
                Bounds3.maxX e >= bx0 && Bounds3.minX e <= bx1
                && Bounds3.maxY e >= by0 && Bounds3.minY e <= by1
                && Bounds3.maxZ e >= bz0 && Bounds3.minZ e <= bz1
            if overlaps then
                for item in cell.Items do
                    if Bounds3.contains item.Position bounds then hits.Add item
            overlaps) tree
        hits |> Seq.sortBy (fun i -> i.Index) |> Seq.map (fun i -> i.Value) |> Seq.toList

    /// <summary>Returns the nearest original value to <c>query</c> within inclusive <c>maxRadius</c> (<c>infinity</c> for unbounded), breaking ties by input order.</summary>
    let nearest (query: Point3) (maxRadius: float) (Octree root) : 'T option =
        Point3.create query.X query.Y query.Z |> ignore
        if Double.IsNaN maxRadius || maxRadius < 0. then invalidArg "maxRadius" "Radius must be nonnegative."
        let closestOnBox (b: Bounds3) =
            Point3.create
                (max (Bounds3.minX b) (min (Bounds3.maxX b) query.X))
                (max (Bounds3.minY b) (min (Bounds3.maxY b) query.Y))
                (max (Bounds3.minZ b) (min (Bounds3.maxZ b) query.Z))
#if FABLE_COMPILER
        let pq = PriorityQueue<OctNode<'T>, float>()
#else
        let pq = System.Collections.Generic.PriorityQueue<OctNode<'T>, float>()
#endif
        root |> Option.iter (fun (OctNode(cell, _) as n) -> pq.Enqueue(n, Point3.distance query (closestOnBox cell.Extent)))
        let mutable best: (OctItem<'T> * float) option = None
        while pq.Count > 0 do
            let (OctNode(cell, children)) = pq.Dequeue()
            let boxDist = Point3.distance query (closestOnBox cell.Extent)
            let canImprove =
                match best with
                | None -> boxDist <= maxRadius
                | Some(_, bestD) -> boxDist <= bestD + 1e-13
            if canImprove then
                for item in cell.Items do
                    let d = Point3.distance query item.Position
                    if d <= maxRadius then
                        let better =
                            match best with
                            | None -> true
                            | Some(prev, bestD) ->
                                if abs (d - bestD) > 1e-13 * max 1. bestD then d < bestD
                                else item.Index < prev.Index
                        if better then best <- Some(item, d)
                for (OctNode(childCell, _) as ch) in children do
                    pq.Enqueue(ch, Point3.distance query (closestOnBox childCell.Extent))
        best |> Option.map (fun (item, _) -> item.Value)

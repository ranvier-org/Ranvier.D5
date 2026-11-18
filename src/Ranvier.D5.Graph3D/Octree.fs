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

[<Struct>]
type internal FlatOctNode =
    val MinX: float
    val MinY: float
    val MinZ: float
    val MaxX: float
    val MaxY: float
    val MaxZ: float
    val Cx: float
    val Cy: float
    val Cz: float
    val Mass: float
    val ItemStart: int
    val ItemCount: int
    val SubtreeCount: int
    val Depth: int
    val FirstChild: int
    val ChildCount: int
    new(minX, minY, minZ, maxX, maxY, maxZ, cx, cy, cz, mass, itemStart, itemCount, subtreeCount, depth, firstChild, childCount) =
        { MinX = minX; MinY = minY; MinZ = minZ
          MaxX = maxX; MaxY = maxY; MaxZ = maxZ
          Cx = cx; Cy = cy; Cz = cz; Mass = mass
          ItemStart = itemStart; ItemCount = itemCount; SubtreeCount = subtreeCount
          Depth = depth; FirstChild = firstChild; ChildCount = childCount }

type internal BhOctreeWorkspace =
    { mutable Nodes: FlatOctNode[]
      mutable NodeCount: int
      Perm: int[]
      Scratch: int[]
      Stack: int[] }

type private OctData<'T> =
    { Nodes: FlatOctNode[]
      NodeCount: int
      Perm: int[]
      Ix: float[]
      Iy: float[]
      Iz: float[]
      IMass: float[]
      IVal: 'T[] }

/// <summary>An opaque, immutable 3D point-region octree supporting O(log N) Barnes-Hut force aggregation, range queries and nearest-neighbor search.</summary>
type Octree<'T> = private Octree of OctData<'T> option

/// <summary>Construction and spatial queries over 3D <c>Octree</c> values.</summary>
[<RequireQualifiedAccess>]
module Octree =
    let private interpolate a b t =
        if (a >= 0.) = (b >= 0.) then a + (b - a) * t else a * (1. - t) + b * t

    let internal createWorkspace (n: int) : BhOctreeWorkspace =
        let cap = max 16 n
        { Nodes = Array.zeroCreate<FlatOctNode> (max 32 (cap / 2 + 16))
          NodeCount = 0
          Perm = Array.zeroCreate<int> cap
          Scratch = Array.zeroCreate<int> cap
          Stack = Array.zeroCreate<int> 512 }

    let private ensureNodeCapacity (ws: BhOctreeWorkspace) (needed: int) =
        if ws.Nodes.Length < needed then
            let nextCap = max needed (ws.Nodes.Length * 2)
            let grown = Array.zeroCreate<FlatOctNode> nextCap
            Array.Copy(ws.Nodes, 0, grown, 0, ws.NodeCount)
            ws.Nodes <- grown

    // Verified by Rocq: octree_aabb3_lower_bound_and_leaf_conservation & flat_cell_children_contiguous_slice_exact
    let internal rebuildInPlace
        (ws: BhOctreeWorkspace)
        (token: System.Threading.CancellationToken)
        (xs: float[])
        (ys: float[])
        (zs: float[])
        (masses: float[])
        (count: int)
        (validateMassSum: bool)
        : unit =
        if count <= 0 then
            ws.NodeCount <- 0
        else
            let perm = ws.Perm
            let scratch = ws.Scratch
            for i = 0 to count - 1 do perm[i] <- i
            ws.NodeCount <- 1

            let rec fillSlot (slot: int) (depth: int) (offset: int) (len: int) =
                token.ThrowIfCancellationRequested()
                let i0 = perm[offset]
                let mutable x0, y0, z0 = xs[i0], ys[i0], zs[i0]
                let mutable x1, y1, z1 = x0, y0, z0
                let mutable total, cx, cy, cz = 0., 0., 0., 0.
                for k = 0 to len - 1 do
                    Kernels3D.check token k
                    let idx = perm[offset + k]
                    let px, py, pz = xs[idx], ys[idx], zs[idx]
                    let pm = masses[idx]
                    if px < x0 then x0 <- px
                    if py < y0 then y0 <- py
                    if pz < z0 then z0 <- pz
                    if px > x1 then x1 <- px
                    if py > y1 then y1 <- py
                    if pz > z1 then z1 <- pz
                    if pm > 0. then
                        let next = total + pm
                        if validateMassSum && not (Double.IsFinite next) then invalidArg "mass" "Total octree mass overflowed."
                        let t = pm / next
                        cx <- interpolate cx px t
                        cy <- interpolate cy py t
                        cz <- interpolate cz pz t
                        total <- next

                let mx, my, mz = interpolate x0 x1 0.5, interpolate y0 y1 0.5, interpolate z0 z1 0.5
                let sx, sy, sz = (mx > x0 && mx < x1), (my > y0 && my < y1), (mz > z0 && mz < z1)
                let fcx, fcy, fcz = if total = 0. then mx, my, mz else cx, cy, cz

                if len <= 8 || depth >= 48 || not (sx || sy || sz) then
                    ws.Nodes[slot] <- FlatOctNode(x0, y0, z0, x1, y1, z1, fcx, fcy, fcz, total, offset, len, len, depth, -1, 0)
                else
                    let mutable c0, c1, c2, c3 = 0, 0, 0, 0
                    let mutable c4, c5, c6, c7 = 0, 0, 0, 0
                    for k = 0 to len - 1 do
                        let idx = perm[offset + k]
                        let oct =
                            (if sx && xs[idx] >= mx then 1 else 0)
                            + (if sy && ys[idx] >= my then 2 else 0)
                            + (if sz && zs[idx] >= mz then 4 else 0)
                        match oct with
                        | 0 -> c0 <- c0 + 1
                        | 1 -> c1 <- c1 + 1
                        | 2 -> c2 <- c2 + 1
                        | 3 -> c3 <- c3 + 1
                        | 4 -> c4 <- c4 + 1
                        | 5 -> c5 <- c5 + 1
                        | 6 -> c6 <- c6 + 1
                        | _ -> c7 <- c7 + 1

                    let childCount =
                        (if c0 > 0 then 1 else 0) + (if c1 > 0 then 1 else 0) +
                        (if c2 > 0 then 1 else 0) + (if c3 > 0 then 1 else 0) +
                        (if c4 > 0 then 1 else 0) + (if c5 > 0 then 1 else 0) +
                        (if c6 > 0 then 1 else 0) + (if c7 > 0 then 1 else 0)

                    if childCount <= 1 then
                        // Degenerate numeric tie where all points fell into one octant
                        ws.Nodes[slot] <- FlatOctNode(x0, y0, z0, x1, y1, z1, fcx, fcy, fcz, total, offset, len, len, depth, -1, 0)
                    else
                        let o0 = 0
                        let o1 = c0
                        let o2 = o1 + c1
                        let o3 = o2 + c2
                        let o4 = o3 + c3
                        let o5 = o4 + c4
                        let o6 = o5 + c5
                        let o7 = o6 + c6
                        let mutable w0, w1, w2, w3 = o0, o1, o2, o3
                        let mutable w4, w5, w6, w7 = o4, o5, o6, o7
                        for k = 0 to len - 1 do
                            let idx = perm[offset + k]
                            let oct =
                                (if sx && xs[idx] >= mx then 1 else 0)
                                + (if sy && ys[idx] >= my then 2 else 0)
                                + (if sz && zs[idx] >= mz then 4 else 0)
                            match oct with
                            | 0 -> scratch[w0] <- idx; w0 <- w0 + 1
                            | 1 -> scratch[w1] <- idx; w1 <- w1 + 1
                            | 2 -> scratch[w2] <- idx; w2 <- w2 + 1
                            | 3 -> scratch[w3] <- idx; w3 <- w3 + 1
                            | 4 -> scratch[w4] <- idx; w4 <- w4 + 1
                            | 5 -> scratch[w5] <- idx; w5 <- w5 + 1
                            | 6 -> scratch[w6] <- idx; w6 <- w6 + 1
                            | _ -> scratch[w7] <- idx; w7 <- w7 + 1

                        Array.Copy(scratch, 0, perm, offset, len)
                        let firstChild = ws.NodeCount
                        ensureNodeCapacity ws (firstChild + childCount)
                        ws.NodeCount <- firstChild + childCount
                        ws.Nodes[slot] <- FlatOctNode(x0, y0, z0, x1, y1, z1, fcx, fcy, fcz, total, offset, 0, len, depth, firstChild, childCount)

                        let mutable cw = firstChild
                        if c0 > 0 then fillSlot cw (depth + 1) (offset + o0) c0; cw <- cw + 1
                        if c1 > 0 then fillSlot cw (depth + 1) (offset + o1) c1; cw <- cw + 1
                        if c2 > 0 then fillSlot cw (depth + 1) (offset + o2) c2; cw <- cw + 1
                        if c3 > 0 then fillSlot cw (depth + 1) (offset + o3) c3; cw <- cw + 1
                        if c4 > 0 then fillSlot cw (depth + 1) (offset + o4) c4; cw <- cw + 1
                        if c5 > 0 then fillSlot cw (depth + 1) (offset + o5) c5; cw <- cw + 1
                        if c6 > 0 then fillSlot cw (depth + 1) (offset + o6) c6; cw <- cw + 1
                        if c7 > 0 then fillSlot cw (depth + 1) (offset + o7) c7

            fillSlot 0 0 0 count

    /// <summary>Builds an immutable 3D octree with cooperative cancellation; subdivides up to depth 48 with leaf capacity 8.</summary>
    let buildWithCancellation
        (token: System.Threading.CancellationToken)
        (position: 'T -> Point3)
        (mass: 'T -> float)
        (items: seq<'T>)
        : Octree<'T> =
        token.ThrowIfCancellationRequested()
        let inline checkPointAndMass (p: Point3) (m: float) =
            if not (Double.IsFinite p.X && Double.IsFinite p.Y && Double.IsFinite p.Z) then
                invalidArg "position" "Point3 coordinates must be finite."
            if not (Double.IsFinite m) || m < 0. then
                invalidArg "mass" "Mass must be finite and nonnegative."

        let ix, iy, iz, imass, ival =
            match box items with
            | :? ('T array) as arr ->
                let n = arr.Length
                let ax = Array.zeroCreate<float> n
                let ay = Array.zeroCreate<float> n
                let az = Array.zeroCreate<float> n
                let am = Array.zeroCreate<float> n
                for i = 0 to n - 1 do
                    Kernels3D.check token i
                    let v = arr[i]
                    let p = position v
                    let m = mass v
                    checkPointAndMass p m
                    ax[i] <- p.X; ay[i] <- p.Y; az[i] <- p.Z; am[i] <- m
                ax, ay, az, am, Array.copy arr
            | :? ('T list) as lst ->
                let n = List.length lst
                let ax = Array.zeroCreate<float> n
                let ay = Array.zeroCreate<float> n
                let az = Array.zeroCreate<float> n
                let am = Array.zeroCreate<float> n
                let av = Array.zeroCreate<'T> n
                let mutable i = 0
                for v in lst do
                    Kernels3D.check token i
                    let p = position v
                    let m = mass v
                    checkPointAndMass p m
                    ax[i] <- p.X; ay[i] <- p.Y; az[i] <- p.Z; am[i] <- m; av[i] <- v
                    i <- i + 1
                ax, ay, az, am, av
            | _ ->
                let bx = ResizeArray<float>()
                let by = ResizeArray<float>()
                let bz = ResizeArray<float>()
                let bm = ResizeArray<float>()
                let bv = ResizeArray<'T>()
                let mutable i = 0
                for v in items do
                    Kernels3D.check token i
                    let p = position v
                    let m = mass v
                    checkPointAndMass p m
                    bx.Add p.X; by.Add p.Y; bz.Add p.Z; bm.Add m; bv.Add v
                    i <- i + 1
                bx.ToArray(), by.ToArray(), bz.ToArray(), bm.ToArray(), bv.ToArray()

        let n = ival.Length
        if n = 0 then Octree None
        else
            let ws = createWorkspace n
            rebuildInPlace ws token ix iy iz imass n true
            Octree(
                Some
                    { Nodes = ws.Nodes
                      NodeCount = ws.NodeCount
                      Perm = ws.Perm
                      Ix = ix
                      Iy = iy
                      Iz = iz
                      IMass = imass
                      IVal = ival }
            )

    /// <summary>Builds an immutable 3D octree without cancellation.</summary>
    let build position mass items = buildWithCancellation System.Threading.CancellationToken.None position mass items

    /// <summary>Returns the total number of captured items in the octree.</summary>
    let count (Octree dataOpt) =
        match dataOpt with
        | None -> 0
        | Some d -> d.IVal.Length

    let private materializeCell (d: OctData<'T>) (nd: FlatOctNode) : OctCell<'T> =
        let mutable leafItems = []
        if nd.ItemCount > 0 then
            for k = nd.ItemStart + nd.ItemCount - 1 downto nd.ItemStart do
                let idx = d.Perm[k]
                leafItems <-
                    { Value = d.IVal[idx]
                      Position = Point3.create d.Ix[idx] d.Iy[idx] d.Iz[idx]
                      Mass = d.IMass[idx]
                      Index = idx } :: leafItems
        { Extent = Bounds3.create nd.MinX nd.MinY nd.MinZ (nd.MaxX - nd.MinX) (nd.MaxY - nd.MinY) (nd.MaxZ - nd.MinZ)
          Count = nd.SubtreeCount
          Mass = nd.Mass
          Center = Point3.create nd.Cx nd.Cy nd.Cz
          Depth = nd.Depth
          Items = leafItems }

    /// <summary>Visits octree cells depth-first; returning <c>false</c> prunes the current cell's children.</summary>
    let visit (visitor: OctCell<'T> -> bool) (Octree dataOpt) =
        match dataOpt with
        | None -> ()
        | Some d when d.NodeCount > 0 ->
            let stack = Stack<int>()
            stack.Push 0
            while stack.Count > 0 do
                let ni = stack.Pop()
                let nd = d.Nodes[ni]
                if visitor (materializeCell d nd) && nd.ChildCount > 0 then
                    for c = nd.FirstChild + nd.ChildCount - 1 downto nd.FirstChild do
                        stack.Push c
        | _ -> ()

    /// <summary>Returns all original values lying inside inclusive 3D <c>bounds</c>, preserving original input order.</summary>
    let within (bounds: Bounds3) (Octree dataOpt) : 'T list =
        let bx0, by0, bz0 = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
        let bx1, by1, bz1 = Bounds3.maxX bounds, Bounds3.maxY bounds, Bounds3.maxZ bounds
        match dataOpt with
        | None -> []
        | Some d when d.NodeCount > 0 ->
            let hits = ResizeArray<int>()
            let mutable stack = Array.zeroCreate<int> 128
            let mutable sp = 0
            stack[0] <- 0
            sp <- 1
            while sp > 0 do
                sp <- sp - 1
                let nd = d.Nodes[stack[sp]]
                if nd.MaxX >= bx0 && nd.MinX <= bx1 && nd.MaxY >= by0 && nd.MinY <= by1 && nd.MaxZ >= bz0 && nd.MinZ <= bz1 then
                    if nd.ItemCount > 0 then
                        for k = nd.ItemStart to nd.ItemStart + nd.ItemCount - 1 do
                            let idx = d.Perm[k]
                            let px, py, pz = d.Ix[idx], d.Iy[idx], d.Iz[idx]
                            if px >= bx0 && px <= bx1 && py >= by0 && py <= by1 && pz >= bz0 && pz <= bz1 then
                                hits.Add idx
                    elif nd.ChildCount > 0 then
                        if sp + nd.ChildCount > stack.Length then
                            let grown = Array.zeroCreate<int> (stack.Length * 2)
                            Array.Copy(stack, 0, grown, 0, sp)
                            stack <- grown
                        for c = nd.FirstChild + nd.ChildCount - 1 downto nd.FirstChild do
                            stack[sp] <- c
                            sp <- sp + 1
            if hits.Count = 0 then []
            else
                let arr = hits.ToArray()
                Array.Sort(arr)
                let mutable res = []
                for i = arr.Length - 1 downto 0 do
                    res <- d.IVal[arr[i]] :: res
                res
        | _ -> []

    /// <summary>Returns the nearest original value to <c>query</c> within inclusive <c>maxRadius</c> (<c>infinity</c> for unbounded), breaking ties by input order.</summary>
    let nearest (query: Point3) (maxRadius: float) (Octree dataOpt) : 'T option =
        let qx, qy, qz = query.X, query.Y, query.Z
        if not (Double.IsFinite qx && Double.IsFinite qy && Double.IsFinite qz) then
            invalidArg "query" "Point3 coordinates must be finite."
        if Double.IsNaN maxRadius || maxRadius < 0. then invalidArg "maxRadius" "Radius must be nonnegative."
        match dataOpt with
        | None -> None
        | Some d when d.NodeCount > 0 ->
            let nodes = d.Nodes
            let perm = d.Perm
            let ix, iy, iz = d.Ix, d.Iy, d.Iz
            let inline nodeBoxDistSq (nd: FlatOctNode) =
                let cx = max nd.MinX (min nd.MaxX qx) - qx
                let cy = max nd.MinY (min nd.MaxY qy) - qy
                let cz = max nd.MinZ (min nd.MaxZ qz) - qz
                cx * cx + cy * cy + cz * cz
            let unbounded = Double.IsPositiveInfinity maxRadius
            let maxRadSq = if unbounded then infinity else maxRadius * maxRadius
            let rootD2 = nodeBoxDistSq nodes[0]
            if not unbounded && rootD2 > maxRadSq then None
            else
#if FABLE_COMPILER
                let pq = PriorityQueue<int, float>()
#else
                let pq = System.Collections.Generic.PriorityQueue<int, float>(32)
#endif
                pq.Enqueue(0, rootD2)
                let mutable bestIdx = -1
                let mutable bestD2 = infinity
                let mutable bestD = infinity
                while pq.Count > 0 do
                    let ni = pq.Dequeue()
                    let nd = nodes[ni]
                    let bD2 = nodeBoxDistSq nd
                    let canImprove =
                        if bestIdx < 0 then unbounded || bD2 <= maxRadSq
                        else bD2 <= bestD2 * (1. + 1e-12) + 1e-24
                    if canImprove then
                        if nd.ItemCount > 0 then
                            for k = nd.ItemStart to nd.ItemStart + nd.ItemCount - 1 do
                                let idx = perm[k]
                                let dx = qx - ix[idx]
                                let dy = qy - iy[idx]
                                let dz = qz - iz[idx]
                                let d2 = dx * dx + dy * dy + dz * dz
                                let inRad =
                                    if unbounded then true
                                    elif abs (d2 - maxRadSq) > 1e-12 * max 1. maxRadSq then d2 <= maxRadSq
                                    else sqrt d2 <= maxRadius
                                if inRad then
                                    if bestIdx < 0 then
                                        bestIdx <- idx
                                        bestD2 <- d2
                                        bestD <- sqrt d2
                                    else
                                        let better =
                                            if abs (d2 - bestD2) > 1e-11 * max 1. bestD2 then d2 < bestD2
                                            else
                                                let dist = sqrt d2
                                                if abs (dist - bestD) > 1e-13 * max 1. bestD then dist < bestD
                                                else idx < bestIdx
                                        if better then
                                            bestIdx <- idx
                                            bestD2 <- d2
                                            bestD <- sqrt d2
                        elif nd.ChildCount > 0 then
                            for c = nd.FirstChild to nd.FirstChild + nd.ChildCount - 1 do
                                let cD2 = nodeBoxDistSq nodes[c]
                                let childPossible =
                                    if bestIdx < 0 then unbounded || cD2 <= maxRadSq
                                    else cD2 <= bestD2 * (1. + 1e-12) + 1e-24
                                if childPossible then
                                    pq.Enqueue(c, cD2)
                if bestIdx >= 0 then Some d.IVal[bestIdx] else None
        | _ -> None

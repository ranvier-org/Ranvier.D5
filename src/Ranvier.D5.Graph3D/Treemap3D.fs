namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>3D space-filling cuboid and 2.5D stepped platform tiling strategies.</summary>
[<RequireQualifiedAccess>]
type Treemap3DTiling =
    /// <summary>Alternates axis-aligned orthogonal cuts across X, Y and Z axes by <c>depth % 3</c>.</summary>
    | CubeSliceDice
    /// <summary>Slices perpendicular to the longest cuboid axis using 2D cross-section squarification to minimize worst 3D aspect ratio.</summary>
    | CubeSquarified
    /// <summary>Order-preserving recursive 8-octant subdivision following a continuous 3D Hilbert curve traversal.</summary>
    | CubeHilbert
    /// <summary>Bladh, Carr &amp; Scholl (2004) 2.5D tiered platform treemap with 2D squarified X-Z footprints stepping upward along +Y.</summary>
    | StepTree

/// <summary>A hierarchy node positioned as an axis-aligned 3D cuboid in world coordinates.</summary>
type HierarchyCuboid<'Id> =
    { /// <summary>Node identifier.</summary>
      Id: 'Id
      /// <summary>Enclosing parent identifier, or <c>None</c> at the root.</summary>
      Parent: 'Id option
      /// <summary>Tree depth from root (0 at root).</summary>
      Depth: int
      /// <summary>Aggregated nonnegative hierarchy value under the active sum policy.</summary>
      Value: float
      /// <summary>Positioned 3D axis-aligned bounding cuboid.</summary>
      Bounds: Bounds3
      /// <summary>True when the node has no children.</summary>
      IsLeaf: bool }

/// <summary>Configuration for <c>Treemap3D</c> volumetric and stepped hierarchy layouts.</summary>
type Treemap3DConfig =
    { /// <summary>Target 3D bounding volume (or base X-Z footprint and base Y origin in <c>StepTree</c> mode).</summary>
      Bounds: Bounds3
      /// <summary>Selected 3D cuboid or 2.5D stepped tiling strategy.</summary>
      Tiling: Treemap3DTiling
      /// <summary>Nonnegative padding inset in layout units between sibling cuboids and parent walls.</summary>
      Padding: float
      /// <summary>Positive vertical platform thickness along +Y per hierarchy depth in <c>StepTree</c> mode.</summary>
      StepHeight: float
      /// <summary>Radial contraction factor in <c>[0, 0.5]</c> pulling child cuboids toward their parent center for onion-skin inspection.</summary>
      ExplodeFactor: float
      /// <summary>Hierarchy weight aggregation policy (<c>IncludeOwnWeight</c> or <c>LeavesOnly</c>).</summary>
      SumPolicy: HierarchySumPolicy }

/// <summary>Constructors for <c>Treemap3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Treemap3DConfig =
    /// <summary>Creates a <c>CubeSquarified</c> 3D treemap configuration with zero padding, 18-unit step height, zero explode factor and <c>LeavesOnly</c> sums.</summary>
    let create (bounds: Bounds3) : Treemap3DConfig =
        { Bounds = bounds
          Tiling = Treemap3DTiling.CubeSquarified
          Padding = 0.
          StepHeight = 18.
          ExplodeFactor = 0.
          SumPolicy = HierarchySumPolicy.LeavesOnly }

/// <summary>A district plinth or 3D building positioned in a Wettel &amp; Lanza (2007/2008) 2.5D bivariate <c>CodeCity</c> layout.</summary>
type CityBuilding<'Id> =
    { /// <summary>Entity identifier.</summary>
      Id: 'Id
      /// <summary>Parent district identifier, or <c>None</c> at the root.</summary>
      Parent: 'Id option
      /// <summary>Nesting depth from the city root (0 at root).</summary>
      Depth: int
      /// <summary>Aggregated nonnegative hierarchy weight governing X-Z footprint area.</summary>
      AreaMetric: float
      /// <summary>Nonnegative height metric governing +Y building elevation on leaf nodes (0 on district plinths).</summary>
      HeightMetric: float
      /// <summary>Positioned 3D cuboid in X-Z footprint and +Y elevation coordinates.</summary>
      Bounds: Bounds3
      /// <summary>True for internal package/module district plinths; false for leaf buildings.</summary>
      IsDistrict: bool }

/// <summary>Configuration for Wettel &amp; Lanza (2007/2008) 2.5D bivariate <c>CodeCity</c> layouts.</summary>
type CodeCityConfig<'Id> =
    { /// <summary>2D city footprint mapped onto the horizontal X-Z ground plane (<c>Y = 0</c> upward).</summary>
      FootprintBounds: Bounds
      /// <summary>Maximum +Y height in layout units assigned to the tallest leaf building.</summary>
      MaxBuildingHeight: float
      /// <summary>Nonnegative +Y terrace height added per nested district level.</summary>
      DistrictPlinthHeight: float
      /// <summary>Nonnegative X-Z street corridor padding separating nested districts and sibling buildings.</summary>
      StreetPadding: float
      /// <summary>Callback returning a finite nonnegative height metric for each leaf identifier.</summary>
      BuildingHeight: 'Id -> float }

/// <summary>Constructors for <c>CodeCityConfig</c>.</summary>
[<RequireQualifiedAccess>]
module CodeCityConfig =
    /// <summary>Creates a 2.5D <c>CodeCity</c> configuration with 6-unit district plinths and 4-unit street corridors.</summary>
    let create (footprint: Bounds) (maxBuildingHeight: float) (heightFn: 'Id -> float) : CodeCityConfig<'Id> =
        { FootprintBounds = footprint
          MaxBuildingHeight = maxBuildingHeight
          DistrictPlinthHeight = 6.
          StreetPadding = 4.
          BuildingHeight = heightFn }

module private HierarchyFlat3D =
    type IndexedTree<'Id> =
        { Ids: 'Id[]
          Parents: ('Id option)[]
          ParentIdx: int[]
          Depths: int[]
          OwnWeights: float[]
          Children: int[][]
          Postorder: int[] }

    let build (root: HierarchyNode<'Id>) : Result<IndexedTree<'Id>, LayoutError list> =
        let ids = ResizeArray<'Id>()
        let parents = ResizeArray<'Id option>()
        let parentIdx = ResizeArray<int>()
        let depths = ResizeArray<int>()
        let ownWeights = ResizeArray<float>()
        let childLists = ResizeArray<ResizeArray<int>>()
        let seen = HashSet<'Id>()
        let stack = Stack<HierarchyNode<'Id> * 'Id option * int * int>()
        stack.Push(root, None, -1, 0)
        let mutable duplicate = false

        while stack.Count > 0 && not duplicate do
            let cur, parOpt, pIdx, depth = stack.Pop()
            let cid = Hierarchy.id cur
            if not (seen.Add cid) then
                duplicate <- true
            else
                let myIdx = ids.Count
                ids.Add cid
                parents.Add parOpt
                parentIdx.Add pIdx
                depths.Add depth
                ownWeights.Add(Hierarchy.weight cur)
                childLists.Add(ResizeArray<int>())
                if pIdx >= 0 then
                    childLists[pIdx].Add myIdx
                let ch = Hierarchy.children cur
                for c in List.rev ch do
                    stack.Push(c, Some cid, myIdx, depth + 1)

        if duplicate then
            Error [ InvalidObservation "Hierarchy identities must be unique." ]
        else
            let n = ids.Count
            let children = Array.init n (fun i -> childLists[i].ToArray())
            let post = ResizeArray<int>(n)
            let pStack = Stack<int * bool>()
            pStack.Push(0, false)
            while pStack.Count > 0 do
                let u, visited = pStack.Pop()
                if visited then
                    post.Add u
                else
                    pStack.Push(u, true)
                    let ch = children[u]
                    for k = ch.Length - 1 downto 0 do
                        pStack.Push(ch[k], false)

            Ok
                { Ids = ids.ToArray()
                  Parents = parents.ToArray()
                  ParentIdx = parentIdx.ToArray()
                  Depths = depths.ToArray()
                  OwnWeights = ownWeights.ToArray()
                  Children = children
                  Postorder = post.ToArray() }

    let computeValues (policy: HierarchySumPolicy) (tree: IndexedTree<'Id>) : float[] =
        let n = tree.Ids.Length
        let values = Array.zeroCreate<float> n
        for p = 0 to tree.Postorder.Length - 1 do
            let u = tree.Postorder[p]
            let ch = tree.Children[u]
            if ch.Length = 0 then
                values[u] <- tree.OwnWeights[u]
            else
                let mutable sum = if policy = HierarchySumPolicy.IncludeOwnWeight then tree.OwnWeights[u] else 0.
                let mutable corr = 0.
                for k = 0 to ch.Length - 1 do
                    let y = values[ch[k]] - corr
                    let t = sum + y
                    corr <- (t - sum) - y
                    sum <- t
                values[u] <- sum
        values

    let inline lerpClamped (a: float) (b: float) (t: float) : float =
        let tc = max 0. (min 1. t)
        if tc <= 0. then a
        elif tc >= 1. then b
        else a + (b - a) * tc

    let inset3D (pad: float) (b: Bounds3) : Bounds3 =
        if pad <= 0. then b
        else
            let sx, sy, sz = Bounds3.sizeX b, Bounds3.sizeY b, Bounds3.sizeZ b
            let px = min (pad * 0.5) (sx * 0.45)
            let py = min (pad * 0.5) (sy * 0.45)
            let pz = min (pad * 0.5) (sz * 0.45)
            Bounds3.create
                (Bounds3.minX b + px)
                (Bounds3.minY b + py)
                (Bounds3.minZ b + pz)
                (max 0. (sx - 2. * px))
                (max 0. (sy - 2. * py))
                (max 0. (sz - 2. * pz))

    let inset2D (pad: float) (x0: float, z0: float, w: float, d: float) : float * float * float * float =
        if pad <= 0. then x0, z0, w, d
        else
            let px = min (pad * 0.5) (w * 0.45)
            let pz = min (pad * 0.5) (d * 0.45)
            x0 + px, z0 + pz, max 0. (w - 2. * px), max 0. (d - 2. * pz)

    let explodeCuboid3D (ef: float) (parentBox: Bounds3) (childBox: Bounds3) : Bounds3 =
        if ef <= 0. then childBox
        else
            let alpha = ef * 0.5
            let pc = Bounds3.center parentBox
            let x0 = pc.X + (1. - alpha) * (Bounds3.minX childBox - pc.X)
            let x1 = pc.X + (1. - alpha) * (Bounds3.maxX childBox - pc.X)
            let y0 = pc.Y + (1. - alpha) * (Bounds3.minY childBox - pc.Y)
            let y1 = pc.Y + (1. - alpha) * (Bounds3.maxY childBox - pc.Y)
            let z0 = pc.Z + (1. - alpha) * (Bounds3.minZ childBox - pc.Z)
            let z1 = pc.Z + (1. - alpha) * (Bounds3.maxZ childBox - pc.Z)
            let cx, cy, cz = 0.5 * (x0 + x1), 0.5 * (y0 + y1), 0.5 * (z0 + z1)
            let sx = max 0. ((x1 - x0) * (1. - alpha))
            let sy = max 0. ((y1 - y0) * (1. - alpha))
            let sz = max 0. ((z1 - z0) * (1. - alpha))
            Bounds3.create (cx - sx * 0.5) (cy - sy * 0.5) (cz - sz * 0.5) sx sy sz

    let explodeFootprintXZ (ef: float) (px0: float, pz0: float, pw: float, pd: float) (cx0: float, cz0: float, cw: float, cd: float) =
        if ef <= 0. then cx0, cz0, cw, cd
        else
            let alpha = ef * 0.5
            let pcx, pcz = px0 + pw * 0.5, pz0 + pd * 0.5
            let x0 = pcx + (1. - alpha) * (cx0 - pcx)
            let x1 = pcx + (1. - alpha) * (cx0 + cw - pcx)
            let z0 = pcz + (1. - alpha) * (cz0 - pcz)
            let z1 = pcz + (1. - alpha) * (cz0 + cd - pcz)
            let mx, mz = 0.5 * (x0 + x1), 0.5 * (z0 + z1)
            let nw = max 0. ((x1 - x0) * (1. - alpha))
            let nd = max 0. ((z1 - z0) * (1. - alpha))
            mx - nw * 0.5, mz - nd * 0.5, nw, nd

    /// 1D slice along axis 0 (X), 1 (Y) or 2 (Z) over index range [first .. last - 1].
    let sliceAxis3D (axis: int) (weights: float[]) (first: int) (last: int) (box: Bounds3) (outBoxes: Bounds3[]) : unit =
        let count = last - first
        if count > 0 then
            let mutable total = 0.
            for i = first to last - 1 do total <- total + weights[i]
            let x0, y0, z0 = Bounds3.minX box, Bounds3.minY box, Bounds3.minZ box
            let sx, sy, sz = Bounds3.sizeX box, Bounds3.sizeY box, Bounds3.sizeZ box
            let x1, y1, z1 = x0 + sx, y0 + sy, z0 + sz
            let mutable frac = 0.
            for i = first to last - 1 do
                let nextFrac =
                    if i = last - 1 then 1.
                    elif total <= 0. then min 1. (frac + 1. / float count)
                    else min 1. (frac + weights[i] / total)
                outBoxes[i] <-
                    match axis with
                    | 0 ->
                        let a = lerpClamped x0 x1 frac
                        let b = lerpClamped x0 x1 nextFrac
                        Bounds3.create a y0 z0 (max 0. (b - a)) sy sz
                    | 1 ->
                        let a = lerpClamped y0 y1 frac
                        let b = lerpClamped y0 y1 nextFrac
                        Bounds3.create x0 a z0 sx (max 0. (b - a)) sz
                    | _ ->
                        let a = lerpClamped z0 z1 frac
                        let b = lerpClamped z0 z1 nextFrac
                        Bounds3.create x0 y0 a sx sy (max 0. (b - a))
                frac <- nextFrac

    /// Exact-edge-preserving 2D squarification over (u0, v0, uLen, vLen) for range [first .. last - 1].
    let squarify2D
        (weights: float[])
        (first: int)
        (last: int)
        (u0: float)
        (v0: float)
        (uLen: float)
        (vLen: float)
        (outRects: struct(float * float * float * float)[])
        : unit =
        let mutable remU0 = u0
        let mutable remV0 = v0
        let endU = u0 + uLen
        let endV = v0 + vLen
        let mutable remTotal = 0.
        for i = first to last - 1 do remTotal <- remTotal + weights[i]

        let stripe2D alongU sFirst sLast rU0 rV0 rW rH =
            let cnt = sLast - sFirst
            let mutable subTot = 0.
            for k = sFirst to sLast - 1 do subTot <- subTot + weights[k]
            let mutable f = 0.
            for k = sFirst to sLast - 1 do
                let nf =
                    if k = sLast - 1 then 1.
                    elif subTot <= 0. then min 1. (f + 1. / float cnt)
                    else min 1. (f + weights[k] / subTot)
                if alongU then
                    let a = lerpClamped rU0 (rU0 + rW) f
                    let b = lerpClamped rU0 (rU0 + rW) nf
                    outRects[k] <- struct(a, rV0, max 0. (b - a), rH)
                else
                    let a = lerpClamped rV0 (rV0 + rH) f
                    let b = lerpClamped rV0 (rV0 + rH) nf
                    outRects[k] <- struct(rU0, a, rW, max 0. (b - a))
                f <- nf

        let mutable cur = first
        while cur < last do
            let w = max 0. (endU - remU0)
            let h = max 0. (endV - remV0)
            let mutable rowEnd = cur + 1
            let mutable rowSum = weights[cur]
            while rowSum = 0. && rowEnd < last do
                rowSum <- weights[rowEnd]
                rowEnd <- rowEnd + 1
            let alongU = w < h
            if w <= 1e-12 || h <= 1e-12 || remTotal <= 0. then
                rowEnd <- last
                rowSum <- remTotal
            else
                let mutable minW = rowSum
                let mutable maxW = rowSum
                let alpha = max (h / w) (w / h) / remTotal
                let inline worst2D s lo hi =
                    let beta = s * s * alpha
                    if beta <= 0. || lo <= 0. then Double.PositiveInfinity
                    else max (hi / beta) (beta / lo)
                let mutable best = worst2D rowSum minW maxW
                let mutable extending = true
                while extending && rowEnd < last do
                    let candW = weights[rowEnd]
                    if candW <= 0. then
                        rowEnd <- rowEnd + 1
                    else
                        let nextSum = rowSum + candW
                        let nextLo = min minW candW
                        let nextHi = max maxW candW
                        let score = worst2D nextSum nextLo nextHi
                        if score <= best then
                            rowSum <- nextSum
                            minW <- nextLo
                            maxW <- nextHi
                            best <- score
                            rowEnd <- rowEnd + 1
                        else
                            extending <- false

            let frac =
                if rowEnd = last then 1.
                elif remTotal <= 0. then float (rowEnd - cur) / float (last - cur)
                else min 1. (rowSum / remTotal)

            if alongU then
                let cutV = if rowEnd = last then endV else lerpClamped remV0 endV frac
                stripe2D true cur rowEnd remU0 remV0 w (max 0. (cutV - remV0))
                remV0 <- cutV
            else
                let cutU = if rowEnd = last then endU else lerpClamped remU0 endU frac
                stripe2D false cur rowEnd remU0 remV0 (max 0. (cutU - remU0)) h
                remU0 <- cutU
            cur <- rowEnd
            remTotal <- max 0. (remTotal - rowSum)

    /// 3D Squarified Cube Treemap over weights[0 .. m - 1].
    let squarify3D (weights: float[]) (box: Bounds3) (outCells: Bounds3[]) : unit =
        let m = weights.Length
        if m = 1 then
            outCells[0] <- box
        elif m > 1 then
            // Sort indices by descending weight (deterministic tie-break by original index) for optimal 3D cuboid aspect ratios
            let order =
                Array.init m id
                |> Array.sortWith (fun a b ->
                    let c = compare weights[b] weights[a]
                    if c <> 0 then c else compare a b)
            let sortedW = Array.init m (fun i -> weights[order[i]])
            let sortedBoxes = Array.zeroCreate<Bounds3> m
            let scratch2D = Array.zeroCreate<struct(float * float * float * float)> m

            let mutable rem = box
            let mutable remTotal = 0.
            for i = 0 to m - 1 do remTotal <- remTotal + sortedW[i]
            let mutable first = 0

            let inline aspect3D (dx: float) (dy: float) (dz: float) =
                let hi = max dx (max dy dz)
                let lo = min dx (min dy dz)
                if lo <= 1e-12 then
                    if hi <= 1e-12 then 1. else 1e12
                else
                    hi / lo

            let evalSlabWorst (sFirst: int) (sLast: int) (thick: float) (uLen: float) (vLen: float) =
                squarify2D sortedW sFirst sLast 0. 0. uLen vLen scratch2D
                let mutable worst = 1.
                for k = sFirst to sLast - 1 do
                    let struct(_, _, du, dv) = scratch2D[k]
                    let a = aspect3D thick du dv
                    if a > worst then worst <- a
                worst

            while first < m do
                let x0, y0, z0 = Bounds3.minX rem, Bounds3.minY rem, Bounds3.minZ rem
                let sx, sy, sz = Bounds3.sizeX rem, Bounds3.sizeY rem, Bounds3.sizeZ rem
                let axis =
                    if sx >= sy && sx >= sz then 0
                    elif sy >= sx && sy >= sz then 1
                    else 2
                let axisLen, uLen, vLen =
                    match axis with
                    | 0 -> sx, sy, sz
                    | 1 -> sy, sx, sz
                    | _ -> sz, sx, sy

                if axisLen <= 1e-12 || uLen <= 1e-12 || vLen <= 1e-12 || remTotal <= 0. then
                    sliceAxis3D axis sortedW first m rem sortedBoxes
                    first <- m
                else
                    let mutable initEnd = first + 1
                    let mutable initSum = sortedW[first]
                    while initSum = 0. && initEnd < m do
                        initSum <- sortedW[initEnd]
                        initEnd <- initEnd + 1

                    let initFrac = if initEnd = m then 1. else min 1. (initSum / remTotal)
                    let mutable bestEnd = initEnd
                    let mutable bestSum = initSum
                    let mutable bestScore = evalSlabWorst first initEnd (axisLen * initFrac) uLen vLen

                    let mutable candEnd = initEnd + 1
                    let mutable candSum = initSum
                    let mutable extraLookahead = 0
                    let mutable searching = true
                    while searching && candEnd <= m do
                        candSum <- candSum + sortedW[candEnd - 1]
                        let frac = if candEnd = m then 1. else min 1. (candSum / remTotal)
                        let thick = axisLen * frac
                        let score = evalSlabWorst first candEnd thick uLen vLen
                        if score <= bestScore then
                            bestScore <- score
                            bestEnd <- candEnd
                            bestSum <- candSum
                            extraLookahead <- 0
                            candEnd <- candEnd + 1
                        else
                            let avgCrossSide = sqrt (uLen * vLen / float (candEnd - first))
                            if thick < avgCrossSide && extraLookahead < 8 then
                                extraLookahead <- extraLookahead + 1
                                candEnd <- candEnd + 1
                            else
                                searching <- false

                    let finalFrac =
                        if bestEnd = m then 1.
                        else min 1. (bestSum / remTotal)

                    match axis with
                    | 0 ->
                        let cutX = if bestEnd = m then x0 + sx else lerpClamped x0 (x0 + sx) finalFrac
                        let thick = max 0. (cutX - x0)
                        squarify2D sortedW first bestEnd y0 z0 sy sz scratch2D
                        for k = first to bestEnd - 1 do
                            let struct(ru, rv, du, dv) = scratch2D[k]
                            sortedBoxes[k] <- Bounds3.create x0 ru rv thick du dv
                        rem <- Bounds3.create cutX y0 z0 (max 0. (x0 + sx - cutX)) sy sz
                    | 1 ->
                        let cutY = if bestEnd = m then y0 + sy else lerpClamped y0 (y0 + sy) finalFrac
                        let thick = max 0. (cutY - y0)
                        squarify2D sortedW first bestEnd x0 z0 sx sz scratch2D
                        for k = first to bestEnd - 1 do
                            let struct(ru, rv, du, dv) = scratch2D[k]
                            sortedBoxes[k] <- Bounds3.create ru y0 rv du thick dv
                        rem <- Bounds3.create x0 cutY z0 sx (max 0. (y0 + sy - cutY)) sz
                    | _ ->
                        let cutZ = if bestEnd = m then z0 + sz else lerpClamped z0 (z0 + sz) finalFrac
                        let thick = max 0. (cutZ - z0)
                        squarify2D sortedW first bestEnd x0 y0 sx sy scratch2D
                        for k = first to bestEnd - 1 do
                            let struct(ru, rv, du, dv) = scratch2D[k]
                            sortedBoxes[k] <- Bounds3.create ru rv z0 du dv thick
                        rem <- Bounds3.create x0 y0 cutZ sx sy (max 0. (z0 + sz - cutZ))

                    first <- bestEnd
                    remTotal <- max 0. (remTotal - bestSum)

            for i = 0 to m - 1 do
                outCells[order[i]] <- sortedBoxes[i]

    /// Continuous 3D Hilbert recursive 8-octant subdivision preserving exact child input order [0 .. m - 1].
    let hilbert3D (weights: float[]) (box: Bounds3) (outCells: Bounds3[]) : unit =
        let m = weights.Length
        if m = 1 then
            outCells[0] <- box
        elif m > 1 then
            let prefix = Array.zeroCreate<float> (m + 1)
            for i = 0 to m - 1 do
                prefix[i + 1] <- prefix[i] + weights[i]

            let splitAlongX (w0: float) (w1: float) (b: Bounds3) =
                let tot = w0 + w1
                let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
                let sx, sy, sz = Bounds3.sizeX b, Bounds3.sizeY b, Bounds3.sizeZ b
                if w0 <= 0. && w1 > 0. then
                    Bounds3.create x0 y0 z0 0. sy sz, b
                elif w1 <= 0. && w0 > 0. then
                    b, Bounds3.create (x0 + sx) y0 z0 0. sy sz
                else
                    let f = if tot <= 0. then 0.5 else w0 / tot
                    let cx = lerpClamped x0 (x0 + sx) f
                    Bounds3.create x0 y0 z0 (max 0. (cx - x0)) sy sz,
                    Bounds3.create cx y0 z0 (max 0. (x0 + sx - cx)) sy sz

            let splitAlongY (w0: float) (w1: float) (b: Bounds3) =
                let tot = w0 + w1
                let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
                let sx, sy, sz = Bounds3.sizeX b, Bounds3.sizeY b, Bounds3.sizeZ b
                if w0 <= 0. && w1 > 0. then
                    Bounds3.create x0 y0 z0 sx 0. sz, b
                elif w1 <= 0. && w0 > 0. then
                    b, Bounds3.create x0 (y0 + sy) z0 sx 0. sz
                else
                    let f = if tot <= 0. then 0.5 else w0 / tot
                    let cy = lerpClamped y0 (y0 + sy) f
                    Bounds3.create x0 y0 z0 sx (max 0. (cy - y0)) sz,
                    Bounds3.create x0 cy z0 sx (max 0. (y0 + sy - cy)) sz

            let splitAlongZ (w0: float) (w1: float) (b: Bounds3) =
                let tot = w0 + w1
                let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
                let sx, sy, sz = Bounds3.sizeX b, Bounds3.sizeY b, Bounds3.sizeZ b
                if w0 <= 0. && w1 > 0. then
                    Bounds3.create x0 y0 z0 sx sy 0., b
                elif w1 <= 0. && w0 > 0. then
                    b, Bounds3.create x0 y0 (z0 + sz) sx sy 0.
                else
                    let f = if tot <= 0. then 0.5 else w0 / tot
                    let cz = lerpClamped z0 (z0 + sz) f
                    Bounds3.create x0 y0 z0 sx sy (max 0. (cz - z0)),
                    Bounds3.create x0 y0 cz sx sy (max 0. (z0 + sz - cz))

            // Partition range [first .. last - 1] into k <= 8 non-empty balanced contiguous buckets
            let partitionBuckets (first: int) (last: int) (k: int) : int[] =
                let cuts = Array.zeroCreate<int> (k + 1)
                cuts[0] <- first
                cuts[k] <- last
                let mutable cur = first
                for b = 1 to k - 1 do
                    let remBuckets = k - b + 1
                    let maxEnd = last - (remBuckets - 1)
                    let remW = prefix[last] - prefix[cur]
                    if remW <= 0. then
                        let step = max 1 ((last - cur) / remBuckets)
                        cur <- min maxEnd (cur + step)
                    else
                        let target = prefix[cur] + remW / float remBuckets
                        let mutable bestSplit = cur + 1
                        let mutable bestDiff = abs (prefix[bestSplit] - target)
                        let mutable s = cur + 2
                        while s <= maxEnd do
                            let d = abs (prefix[s] - target)
                            if d < bestDiff then
                                bestDiff <- d
                                bestSplit <- s
                            s <- s + 1
                        cur <- bestSplit
                    cuts[b] <- cur
                cuts

            // Orient initial Hilbert basis (e0, e1, e2) by descending box extents
            let sx0, sy0, sz0 = Bounds3.sizeX box, Bounds3.sizeY box, Bounds3.sizeZ box
            let e0Init, e1Init, e2Init =
                if sx0 >= sy0 && sy0 >= sz0 then 1, 2, 4
                elif sx0 >= sz0 && sz0 >= sy0 then 1, 4, 2
                elif sy0 >= sx0 && sx0 >= sz0 then 2, 1, 4
                elif sy0 >= sz0 && sz0 >= sx0 then 2, 4, 1
                elif sz0 >= sx0 && sx0 >= sy0 then 4, 1, 2
                else 4, 2, 1

            let rec recurse (first: int) (last: int) (curBox: Bounds3) (s: int) (e0: int) (e1: int) (e2: int) =
                let count = last - first
                if count = 1 then
                    outCells[first] <- curBox
                elif count > 1 then
                    let k = min 8 count
                    let cuts = partitionBuckets first last k
                    // 8 octants in continuous 3D Hilbert Gray-code order
                    let octSeq =
                        [| s
                           s ^^^ e2
                           s ^^^ e2 ^^^ e1
                           s ^^^ e1
                           s ^^^ e1 ^^^ e0
                           s ^^^ e2 ^^^ e1 ^^^ e0
                           s ^^^ e2 ^^^ e0
                           s ^^^ e0 |]

                    let wOct = Array.zeroCreate<float> 8
                    let slotOfBucket = Array.zeroCreate<int> k
                    for b = 0 to k - 1 do
                        let q = if k = 8 then b else (b * 8) / k
                        slotOfBucket[b] <- q
                        let oct = octSeq[q]
                        let bw = prefix[cuts[b + 1]] - prefix[cuts[b]]
                        wOct[oct] <- if bw > 0. then bw else 1e-9 * float (cuts[b + 1] - cuts[b])

                    // Exact 3-level weighted orthogonal k-d split guaranteeing exact proportional volumes
                    let wX0 = wOct[0] + wOct[2] + wOct[4] + wOct[6]
                    let wX1 = wOct[1] + wOct[3] + wOct[5] + wOct[7]
                    let bX0, bX1 = splitAlongX wX0 wX1 curBox

                    let bX0Y0, bX0Y1 = splitAlongY (wOct[0] + wOct[4]) (wOct[2] + wOct[6]) bX0
                    let bX1Y0, bX1Y1 = splitAlongY (wOct[1] + wOct[5]) (wOct[3] + wOct[7]) bX1

                    let octBoxes = Array.zeroCreate<Bounds3> 8
                    let b0, b4 = splitAlongZ wOct[0] wOct[4] bX0Y0
                    let b2, b6 = splitAlongZ wOct[2] wOct[6] bX0Y1
                    let b1, b5 = splitAlongZ wOct[1] wOct[5] bX1Y0
                    let b3, b7 = splitAlongZ wOct[3] wOct[7] bX1Y1
                    octBoxes[0] <- b0; octBoxes[4] <- b4
                    octBoxes[2] <- b2; octBoxes[6] <- b6
                    octBoxes[1] <- b1; octBoxes[5] <- b5
                    octBoxes[3] <- b3; octBoxes[7] <- b7

                    for b = 0 to k - 1 do
                        let q = slotOfBucket[b]
                        let oct = octSeq[q]
                        let subBox = octBoxes[oct]
                        // Exact Butz/Skilling 3D Hilbert sub-cube orientation transitions
                        let ns, ne0, ne1, ne2 =
                            match q with
                            | 0 -> s, e2, e1, e0
                            | 1 -> s, e1, e2, e0
                            | 2 -> s, e0, e1, e2
                            | 3 -> s ^^^ e0 ^^^ e2, e2, e1, e0
                            | 4 -> s, e2, e1, e0
                            | 5 -> s, e0, e1, e2
                            | 6 -> s ^^^ e0 ^^^ e1, e1, e2, e0
                            | _ -> s ^^^ e0 ^^^ e2, e2, e1, e0
                        recurse cuts[b] cuts[b + 1] subBox ns ne0 ne1 ne2

            recurse 0 m box 0 e0Init e1Init e2Init

    let cuboidRadius (b: Bounds3) : float =
        let sx, sy, sz = Bounds3.sizeX b, Bounds3.sizeY b, Bounds3.sizeZ b
        let m = min sx (min sy sz)
        if m > 0. then 0.5 * m
        else 0.5 * sqrt (sx * sx + sy * sy + sz * sz)

/// <summary>3D Cube Treemaps (<c>CubeSliceDice</c>, <c>CubeSquarified</c>, <c>CubeHilbert</c>) and Bladh <c>StepTree</c> engines.</summary>
[<RequireQualifiedAccess>]
module Treemap3D =
    /// <summary>Computes a preorder list of 3D hierarchy cuboids over <c>root</c>.</summary>
    let layout
        (config: Treemap3DConfig)
        (root: HierarchyNode<'Id>)
        : Result<HierarchyCuboid<'Id> list, LayoutError list> =
        if not (Double.IsFinite config.Padding && Double.IsFinite config.StepHeight && Double.IsFinite config.ExplodeFactor)
           || config.Padding < 0.
           || config.StepHeight <= 0.
           || config.ExplodeFactor < 0. || config.ExplodeFactor > 0.5 then
            Error [ InvalidObservation "Treemap3D requires nonnegative Padding, positive StepHeight and ExplodeFactor in [0, 0.5]." ]
        else
            match HierarchyFlat3D.build root with
            | Error errs -> Error errs
            | Ok tree ->
                let n = tree.Ids.Length
                let values = HierarchyFlat3D.computeValues config.SumPolicy tree
                let cuboids = Array.zeroCreate<Bounds3> n

                match config.Tiling with
                | Treemap3DTiling.StepTree ->
                    let baseY = Bounds3.minY config.Bounds
                    let footprints = Array.zeroCreate<struct(float * float * float * float)> n
                    footprints[0] <-
                        struct(
                            Bounds3.minX config.Bounds,
                            Bounds3.minZ config.Bounds,
                            Bounds3.sizeX config.Bounds,
                            Bounds3.sizeZ config.Bounds
                        )

                    for u = 0 to n - 1 do
                        let struct(fx0, fz0, fw, fd) = footprints[u]
                        let y0 = baseY + float tree.Depths[u] * config.StepHeight
                        cuboids[u] <- Bounds3.create fx0 y0 fz0 fw config.StepHeight fd

                        let ch = tree.Children[u]
                        if ch.Length > 0 then
                            let ix0, iz0, iw, idp = HierarchyFlat3D.inset2D config.Padding (fx0, fz0, fw, fd)
                            let childVals = ch |> Array.map (fun c -> values[c])
                            let mutable childSum = 0.
                            for k = 0 to childVals.Length - 1 do childSum <- childSum + childVals[k]
                            let own =
                                if config.SumPolicy = HierarchySumPolicy.IncludeOwnWeight then
                                    max 0. (values[u] - childSum)
                                else 0.
                            let weights = if own > 0. then Array.append childVals [| own |] else childVals
                            if Array.forall ((=) 0.) weights then
                                Array.fill weights 0 weights.Length 1.
                            let order =
                                Array.init weights.Length id
                                |> Array.sortWith (fun a b ->
                                    let c = compare weights[b] weights[a]
                                    if c <> 0 then c else compare a b)
                            let sortedW = Array.init weights.Length (fun k -> weights[order[k]])
                            let rects = Array.zeroCreate<struct(float * float * float * float)> weights.Length
                            HierarchyFlat3D.squarify2D sortedW 0 sortedW.Length ix0 iz0 iw idp rects
                            let byOrig = Array.zeroCreate<struct(float * float * float * float)> weights.Length
                            for k = 0 to weights.Length - 1 do
                                byOrig[order[k]] <- rects[k]
                            for k = 0 to ch.Length - 1 do
                                let struct(rx, rz, rw, rd) = byOrig[k]
                                let px, pz, pw, pd = HierarchyFlat3D.inset2D config.Padding (rx, rz, rw, rd)
                                let ex, ez, ew, ed = HierarchyFlat3D.explodeFootprintXZ config.ExplodeFactor (fx0, fz0, fw, fd) (px, pz, pw, pd)
                                footprints[ch[k]] <- struct(ex, ez, ew, ed)

                | Treemap3DTiling.CubeSliceDice
                | Treemap3DTiling.CubeSquarified
                | Treemap3DTiling.CubeHilbert ->
                    cuboids[0] <- config.Bounds
                    for u = 0 to n - 1 do
                        let ch = tree.Children[u]
                        if ch.Length > 0 then
                            let parentBox = cuboids[u]
                            let innerBox = HierarchyFlat3D.inset3D config.Padding parentBox
                            let childVals = ch |> Array.map (fun c -> values[c])
                            let mutable childSum = 0.
                            for k = 0 to childVals.Length - 1 do childSum <- childSum + childVals[k]
                            let own =
                                if config.SumPolicy = HierarchySumPolicy.IncludeOwnWeight then
                                    max 0. (values[u] - childSum)
                                else 0.
                            let weights = if own > 0. then Array.append childVals [| own |] else childVals
                            if Array.forall ((=) 0.) weights then
                                Array.fill weights 0 weights.Length 1.
                            let maxW = Array.max weights
                            if maxW > 0. then
                                for k = 0 to weights.Length - 1 do weights[k] <- weights[k] / maxW

                            let cells = Array.create weights.Length innerBox
                            match config.Tiling with
                            | Treemap3DTiling.CubeSliceDice ->
                                HierarchyFlat3D.sliceAxis3D (tree.Depths[u] % 3) weights 0 weights.Length innerBox cells
                            | Treemap3DTiling.CubeSquarified ->
                                HierarchyFlat3D.squarify3D weights innerBox cells
                            | Treemap3DTiling.CubeHilbert ->
                                HierarchyFlat3D.hilbert3D weights innerBox cells
                            | Treemap3DTiling.StepTree -> ()

                            for k = 0 to ch.Length - 1 do
                                let padded = HierarchyFlat3D.inset3D config.Padding cells[k]
                                cuboids[ch[k]] <- HierarchyFlat3D.explodeCuboid3D config.ExplodeFactor parentBox padded

                let mutable result = []
                for i = n - 1 downto 0 do
                    result <-
                        { Id = tree.Ids[i]
                          Parent = tree.Parents[i]
                          Depth = tree.Depths[i]
                          Value = values[i]
                          Bounds = cuboids[i]
                          IsLeaf = (tree.Children[i].Length = 0) } :: result
                Ok result

    /// <summary>Converts <c>HierarchyCuboid&lt;string&gt;</c> items into a <c>DiagramLayout3D</c> where leaves become <c>PositionedNode3D</c> items and internal branches become wireframe <c>PositionedGroup3D</c> cuboids.</summary>
    let toDiagramLayout (cuboids: HierarchyCuboid<string> list) : DiagramLayout3D =
        let nodes =
            cuboids
            |> List.choose (fun c ->
                if c.IsLeaf then
                    Some
                        { Id = c.Id
                          Kind = NodeKind.Standard
                          Center = Bounds3.center c.Bounds
                          Bounds = c.Bounds
                          Radius = HierarchyFlat3D.cuboidRadius c.Bounds
                          ScaleFactor = 1.
                          ParentGroup = c.Parent
                          Layer = Some c.Depth }
                else None)
            |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))

        let groups =
            cuboids
            |> List.choose (fun c ->
                if not c.IsLeaf then
                    Some
                        { Id = c.Id
                          ParentGroup = c.Parent
                          Bounds = c.Bounds
                          InnerBounds = c.Bounds
                          Depth = c.Depth }
                else None)
            |> List.sortWith (fun a b ->
                let d = compare a.Depth b.Depth
                if d <> 0 then d else StringComparer.Ordinal.Compare(a.Id, b.Id))

        let fallback = Bounds3.create 0. 0. 0. 0. 0. 0.
        { Bounds = Kernels3D.computeBounds3D fallback nodes groups []
          Nodes = nodes
          Groups = groups
          Associations = [] }

/// <summary>Wettel &amp; Lanza (2007/2008) 2.5D bivariate <c>CodeCity</c> layout mapping hierarchy weight to X-Z lot area and an independent metric to +Y building height.</summary>
[<RequireQualifiedAccess>]
module CodeCity =
    /// <summary>Computes a preorder list of stepped X-Z district plinths and bivariate +Y 3D buildings over <c>root</c>.</summary>
    let layout
        (config: CodeCityConfig<'Id>)
        (root: HierarchyNode<'Id>)
        : Result<CityBuilding<'Id> list, LayoutError list> =
        if not (Double.IsFinite config.MaxBuildingHeight
                && Double.IsFinite config.DistrictPlinthHeight
                && Double.IsFinite config.StreetPadding)
           || config.MaxBuildingHeight <= 0.
           || config.DistrictPlinthHeight < 0.
           || config.StreetPadding < 0. then
            Error [ InvalidObservation "CodeCity requires positive MaxBuildingHeight and nonnegative DistrictPlinthHeight and StreetPadding." ]
        else
            match HierarchyFlat3D.build root with
            | Error errs -> Error errs
            | Ok tree ->
                let n = tree.Ids.Length
                let areaMetrics = HierarchyFlat3D.computeValues HierarchySumPolicy.LeavesOnly tree
                let heightMetrics = Array.zeroCreate<float> n
                let mutable maxRawHeight = 0.
                let mutable invalidHeight = false

                for i = 0 to n - 1 do
                    if tree.Children[i].Length = 0 then
                        let h = config.BuildingHeight tree.Ids[i]
                        if not (Double.IsFinite h) || h < 0. then
                            invalidHeight <- true
                        else
                            heightMetrics[i] <- h
                            if h > maxRawHeight then maxRawHeight <- h

                if invalidHeight then
                    Error [ InvalidObservation "CodeCity BuildingHeight callback must return finite nonnegative values." ]
                else
                    let lots = Array.zeroCreate<struct(float * float * float * float)> n
                    lots[0] <-
                        struct(
                            Bounds.x config.FootprintBounds,
                            Bounds.y config.FootprintBounds,
                            Bounds.width config.FootprintBounds,
                            Bounds.height config.FootprintBounds
                        )

                    let boxes = Array.zeroCreate<Bounds3> n
                    for u = 0 to n - 1 do
                        let struct(lx, lz, lw, ld) = lots[u]
                        let depth = tree.Depths[u]
                        let ch = tree.Children[u]
                        if ch.Length > 0 then
                            // Stepped district plinth
                            let y0 = float depth * config.DistrictPlinthHeight
                            boxes[u] <- Bounds3.create lx y0 lz lw config.DistrictPlinthHeight ld

                            let ix0, iz0, iw, idp = HierarchyFlat3D.inset2D config.StreetPadding (lx, lz, lw, ld)
                            let weights = ch |> Array.map (fun c -> areaMetrics[c])
                            if Array.forall ((=) 0.) weights then
                                Array.fill weights 0 weights.Length 1.
                            let order =
                                Array.init weights.Length id
                                |> Array.sortWith (fun a b ->
                                    let c = compare weights[b] weights[a]
                                    if c <> 0 then c else compare a b)
                            let sortedW = Array.init weights.Length (fun k -> weights[order[k]])
                            let rects = Array.zeroCreate<struct(float * float * float * float)> weights.Length
                            HierarchyFlat3D.squarify2D sortedW 0 sortedW.Length ix0 iz0 iw idp rects
                            let byOrig = Array.zeroCreate<struct(float * float * float * float)> weights.Length
                            for k = 0 to weights.Length - 1 do
                                byOrig[order[k]] <- rects[k]
                            for k = 0 to ch.Length - 1 do
                                let struct(rx, rz, rw, rd) = byOrig[k]
                                let px, pz, pw, pd = HierarchyFlat3D.inset2D config.StreetPadding (rx, rz, rw, rd)
                                lots[ch[k]] <- struct(px, pz, pw, pd)
                        else
                            // Leaf 3D building standing on top of its enclosing district plinth
                            let baseY = float depth * config.DistrictPlinthHeight
                            let rawH = heightMetrics[u]
                            let scaledH =
                                if maxRawHeight <= 0. then 0.
                                else (rawH / maxRawHeight) * config.MaxBuildingHeight
                            boxes[u] <- Bounds3.create lx baseY lz lw scaledH ld

                    let mutable result = []
                    for i = n - 1 downto 0 do
                        result <-
                            { Id = tree.Ids[i]
                              Parent = tree.Parents[i]
                              Depth = tree.Depths[i]
                              AreaMetric = areaMetrics[i]
                              HeightMetric = heightMetrics[i]
                              Bounds = boxes[i]
                              IsDistrict = (tree.Children[i].Length > 0) } :: result
                    Ok result

    /// <summary>Converts <c>CityBuilding&lt;string&gt;</c> items into a <c>DiagramLayout3D</c> where district plinths become <c>PositionedGroup3D</c> wireframes and buildings become <c>PositionedNode3D</c> items.</summary>
    let toDiagramLayout (buildings: CityBuilding<string> list) : DiagramLayout3D =
        let nodes =
            buildings
            |> List.choose (fun b ->
                if not b.IsDistrict then
                    Some
                        { Id = b.Id
                          Kind = NodeKind.Standard
                          Center = Bounds3.center b.Bounds
                          Bounds = b.Bounds
                          Radius = HierarchyFlat3D.cuboidRadius b.Bounds
                          ScaleFactor = 1.
                          ParentGroup = b.Parent
                          Layer = Some b.Depth }
                else None)
            |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))

        let groups =
            buildings
            |> List.choose (fun b ->
                if b.IsDistrict then
                    Some
                        { Id = b.Id
                          ParentGroup = b.Parent
                          Bounds = b.Bounds
                          InnerBounds = b.Bounds
                          Depth = b.Depth }
                else None)
            |> List.sortWith (fun a b ->
                let d = compare a.Depth b.Depth
                if d <> 0 then d else StringComparer.Ordinal.Compare(a.Id, b.Id))

        let fallback = Bounds3.create 0. 0. 0. 0. 0. 0.
        { Bounds = Kernels3D.computeBounds3D fallback nodes groups []
          Nodes = nodes
          Groups = groups
          Associations = [] }

namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>A hierarchy node positioned as a 3D sphere with center and radius in world coordinates.</summary>
type PackedSphere3D<'Id> =
    { /// <summary>Node identifier.</summary>
      Id: 'Id
      /// <summary>Enclosing parent identifier, or <c>None</c> at the root.</summary>
      Parent: 'Id option
      /// <summary>Tree depth from root (0 at root).</summary>
      Depth: int
      /// <summary>Aggregated nonnegative hierarchy value under the active sum policy.</summary>
      Value: float
      /// <summary>3D center of the packed sphere.</summary>
      Center: Point3
      /// <summary>Nonnegative sphere radius in world units.</summary>
      Radius: float
      /// <summary>True when the node has no children.</summary>
      IsLeaf: bool }

/// <summary>Configuration for bottom-up 3D tangent sphere packing (<c>Pack3D</c>).</summary>
type Pack3DConfig =
    { /// <summary>Target 3D bounding volume into which the root enclosing sphere is inscribed.</summary>
      Bounds: Bounds3
      /// <summary>Nonnegative gap padding in world units between sibling spheres and parent walls.</summary>
      Padding: float
      /// <summary>Hierarchy weight aggregation policy (<c>IncludeOwnWeight</c> or <c>LeavesOnly</c>).</summary>
      SumPolicy: HierarchySumPolicy }

/// <summary>Constructors for <c>Pack3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Pack3DConfig =
    /// <summary>Creates a 3D sphere-packing configuration with zero padding and <c>LeavesOnly</c> sums.</summary>
    let create (bounds: Bounds3) : Pack3DConfig =
        { Bounds = bounds
          Padding = 0.
          SumPolicy = HierarchySumPolicy.LeavesOnly }

/// <summary>A hierarchy node positioned as a 3D convex polyhedral cell in a volumetric <c>VoronoiTreemap3D</c>.</summary>
type VoronoiTreemap3DCell<'Id> =
    { /// <summary>Node identifier.</summary>
      Id: 'Id
      /// <summary>Enclosing parent identifier, or <c>None</c> at the root.</summary>
      Parent: 'Id option
      /// <summary>Tree depth from root (0 at root).</summary>
      Depth: int
      /// <summary>Aggregated nonnegative hierarchy value.</summary>
      Value: float
      /// <summary>3D convex polyhedron vertices in world coordinates.</summary>
      Vertices: Point3 list
      /// <summary>Polygonal faces as 0-based vertex index loops into <c>Vertices</c>.</summary>
      Faces: int list list
      /// <summary>Exact 3D polyhedral volume of this cell.</summary>
      Volume: float
      /// <summary>3D volumetric centroid of this cell.</summary>
      Centroid: Point3 }

module private Pack3DKernels =
    type IndexedTree<'Id> =
        { Ids: 'Id[]
          Parents: ('Id option)[]
          ParentIdx: int[]
          Depths: int[]
          OwnWeights: float[]
          Children: int[][]
          Postorder: int[] }

    let buildTree (root: HierarchyNode<'Id>) : Result<IndexedTree<'Id>, LayoutError list> =
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
                for c in List.rev (Hierarchy.children cur) do
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

    [<Struct>]
    type Ball3 =
        val Cx: float
        val Cy: float
        val Cz: float
        val R: float
        new(cx, cy, cz, r) = { Cx = cx; Cy = cy; Cz = cz; R = r }

    let inline encloses (b: Ball3) (s: Ball3) (tol: float) : bool =
        if b.R < 0. then false
        else
            let dx, dy, dz = s.Cx - b.Cx, s.Cy - b.Cy, s.Cz - b.Cz
            let d = sqrt (dx * dx + dy * dy + dz * dz)
            d + s.R <= b.R + tol * (1. + b.R)

    let inline enclosesAll (b: Ball3) (arr: Ball3[]) (count: int) (tol: float) : bool =
        if b.R < 0. then false
        else
            let mutable ok = true
            let mutable i = 0
            while ok && i < count do
                ok <- encloses b arr[i] tol
                i <- i + 1
            ok

    let ball2 (s0: Ball3) (s1: Ball3) : Ball3 =
        let dx, dy, dz = s1.Cx - s0.Cx, s1.Cy - s0.Cy, s1.Cz - s0.Cz
        let d = sqrt (dx * dx + dy * dy + dz * dz)
        if d + s1.R <= s0.R then s0
        elif d + s0.R <= s1.R then s1
        else
            let r = 0.5 * (d + s0.R + s1.R)
            let t = if d <= 1e-15 then 0.5 else (r - s0.R) / d
            Ball3(s0.Cx + dx * t, s0.Cy + dy * t, s0.Cz + dz * t, r)

    let ball3 (s0: Ball3) (s1: Ball3) (s2: Ball3) : Ball3 =
        let items = [| s0; s1; s2 |]
        let mutable best = Ball3(0., 0., 0., infinity)
        let inline tryCand (cand: Ball3) =
            if cand.R >= 0. && cand.R < best.R && enclosesAll cand items 3 1e-10 then
                best <- cand

        tryCand (ball2 s0 s1)
        tryCand (ball2 s0 s2)
        tryCand (ball2 s1 s2)

        if Double.IsFinite best.R then best
        else
            // Solve exact 3-sphere Apollonius outer tangency in the plane of (s0, s1, s2)
            let ux, uy, uz = s1.Cx - s0.Cx, s1.Cy - s0.Cy, s1.Cz - s0.Cz
            let l1 = sqrt (ux * ux + uy * uy + uz * uz)
            if l1 > 1e-12 then
                let exX, exY, exZ = ux / l1, uy / l1, uz / l1
                let vx, vy, vz = s2.Cx - s0.Cx, s2.Cy - s0.Cy, s2.Cz - s0.Cz
                let vxProj = vx * exX + vy * exY + vz * exZ
                let px, py, pz = vx - vxProj * exX, vy - vxProj * exY, vz - vxProj * exZ
                let l2 = sqrt (px * px + py * py + pz * pz)
                if l2 > 1e-12 then
                    let eyX, eyY, eyZ = px / l2, py / l2, pz / l2
                    let r0, r1, r2 = s0.R, s1.R, s2.R
                    let ax = (l1 * l1 + r0 * r0 - r1 * r1) / (2. * l1)
                    let bx = (r1 - r0) / l1
                    let ay = (vxProj * vxProj + l2 * l2 + r0 * r0 - r2 * r2 - 2. * vxProj * ax) / (2. * l2)
                    let by = (2. * (r2 - r0) - 2. * vxProj * bx) / (2. * l2)
                    // (ax + bx*R)^2 + (ay + by*R)^2 = (R - r0)^2  =>  aQ * R^2 + 2 * bQ * R + cQ = 0
                    let aQ = bx * bx + by * by - 1.
                    let bQ = ax * bx + ay * by + r0
                    let cQ = ax * ax + ay * ay - r0 * r0
                    let minR = max r0 (max r1 r2)
                    let evalRoot (rVal: float) =
                        if Double.IsFinite rVal && rVal >= minR - 1e-10 then
                            let rClamped = max minR rVal
                            let x = ax + bx * rClamped
                            let y = ay + by * rClamped
                            let cx = s0.Cx + x * exX + y * eyX
                            let cy = s0.Cy + x * exY + y * eyY
                            let cz = s0.Cz + x * exZ + y * eyZ
                            let mutable rad = rClamped
                            for k = 0 to 2 do
                                let dk = sqrt ((items[k].Cx - cx) * (items[k].Cx - cx) + (items[k].Cy - cy) * (items[k].Cy - cy) + (items[k].Cz - cz) * (items[k].Cz - cz)) + items[k].R
                                if dk > rad then rad <- dk
                            if rad < best.R then best <- Ball3(cx, cy, cz, rad)

                    if abs aQ <= 1e-13 then
                        if abs bQ > 1e-13 then evalRoot (-cQ / (2. * bQ))
                    else
                        let disc = bQ * bQ - aQ * cQ
                        if disc >= -1e-10 then
                            let rt = sqrt (max 0. disc)
                            evalRoot ((-bQ - rt) / aQ)
                            evalRoot ((-bQ + rt) / aQ)

            if Double.IsFinite best.R then best
            else
                // Fallback tight bounding ball around the 3 spheres
                let cx = (s0.Cx + s1.Cx + s2.Cx) / 3.
                let cy = (s0.Cy + s1.Cy + s2.Cy) / 3.
                let cz = (s0.Cz + s1.Cz + s2.Cz) / 3.
                let mutable rad = 0.
                for k = 0 to 2 do
                    let dk = sqrt ((items[k].Cx - cx) * (items[k].Cx - cx) + (items[k].Cy - cy) * (items[k].Cy - cy) + (items[k].Cz - cz) * (items[k].Cz - cz)) + items[k].R
                    if dk > rad then rad <- dk
                Ball3(cx, cy, cz, rad)

    let ball4 (s0: Ball3) (s1: Ball3) (s2: Ball3) (s3: Ball3) : Ball3 =
        let items = [| s0; s1; s2; s3 |]
        let mutable best = Ball3(0., 0., 0., infinity)
        let inline tryCand (cand: Ball3) =
            if cand.R >= 0. && cand.R < best.R && enclosesAll cand items 4 1e-10 then
                best <- cand

        tryCand (ball3 s0 s1 s2)
        tryCand (ball3 s0 s1 s3)
        tryCand (ball3 s0 s2 s3)
        tryCand (ball3 s1 s2 s3)

        if Double.IsFinite best.R then best
        else
            // 4-sphere Apollonius linear system 2 * (c_m - c_0) . dC = p_m + q_m * R (m = 1, 2, 3)
            let a1x, a1y, a1z = 2. * (s1.Cx - s0.Cx), 2. * (s1.Cy - s0.Cy), 2. * (s1.Cz - s0.Cz)
            let a2x, a2y, a2z = 2. * (s2.Cx - s0.Cx), 2. * (s2.Cy - s0.Cy), 2. * (s2.Cz - s0.Cz)
            let a3x, a3y, a3z = 2. * (s3.Cx - s0.Cx), 2. * (s3.Cy - s0.Cy), 2. * (s3.Cz - s0.Cz)

            let c0x, c0y, c0z = a2y * a3z - a2z * a3y, a2z * a3x - a2x * a3z, a2x * a3y - a2y * a3x
            let det = a1x * c0x + a1y * c0y + a1z * c0z
            if abs det > 1e-12 then
                let invDet = 1. / det
                let c1x, c1y, c1z = a3y * a1z - a3z * a1y, a3z * a1x - a3x * a1z, a3x * a1y - a3y * a1x
                let c2x, c2y, c2z = a1y * a2z - a1z * a2y, a1z * a2x - a1x * a2z, a1x * a2y - a1y * a2x

                let r0 = s0.R
                let inline rhsConst (sm: Ball3) =
                    let dx, dy, dz = sm.Cx - s0.Cx, sm.Cy - s0.Cy, sm.Cz - s0.Cz
                    dx * dx + dy * dy + dz * dz + r0 * r0 - sm.R * sm.R
                let p1, p2, p3 = rhsConst s1, rhsConst s2, rhsConst s3
                let q1, q2, q3 = 2. * (s1.R - r0), 2. * (s2.R - r0), 2. * (s3.R - r0)

                let ax = (p1 * c0x + p2 * c1x + p3 * c2x) * invDet
                let ay = (p1 * c0y + p2 * c1y + p3 * c2y) * invDet
                let az = (p1 * c0z + p2 * c1z + p3 * c2z) * invDet

                let bx = (q1 * c0x + q2 * c1x + q3 * c2x) * invDet
                let by = (q1 * c0y + q2 * c1y + q3 * c2y) * invDet
                let bz = (q1 * c0z + q2 * c1z + q3 * c2z) * invDet

                let aQ = bx * bx + by * by + bz * bz - 1.
                let bQ = ax * bx + ay * by + az * bz + r0
                let cQ = ax * ax + ay * ay + az * az - r0 * r0
                let minR = max (max s0.R s1.R) (max s2.R s3.R)

                let evalRoot (rVal: float) =
                    if Double.IsFinite rVal && rVal >= minR - 1e-10 then
                        let rClamped = max minR rVal
                        let cx = s0.Cx + ax + bx * rClamped
                        let cy = s0.Cy + ay + by * rClamped
                        let cz = s0.Cz + az + bz * rClamped
                        let mutable rad = rClamped
                        for k = 0 to 3 do
                            let dk = sqrt ((items[k].Cx - cx) * (items[k].Cx - cx) + (items[k].Cy - cy) * (items[k].Cy - cy) + (items[k].Cz - cz) * (items[k].Cz - cz)) + items[k].R
                            if dk > rad then rad <- dk
                        if rad < best.R then best <- Ball3(cx, cy, cz, rad)

                if abs aQ <= 1e-13 then
                    if abs bQ > 1e-13 then evalRoot (-cQ / (2. * bQ))
                else
                    let disc = bQ * bQ - aQ * cQ
                    if disc >= -1e-10 then
                        let rt = sqrt (max 0. disc)
                        evalRoot ((-bQ - rt) / aQ)
                        evalRoot ((-bQ + rt) / aQ)

            if Double.IsFinite best.R then best
            else
                let cx = 0.25 * (s0.Cx + s1.Cx + s2.Cx + s3.Cx)
                let cy = 0.25 * (s0.Cy + s1.Cy + s2.Cy + s3.Cy)
                let cz = 0.25 * (s0.Cz + s1.Cz + s2.Cz + s3.Cz)
                let mutable rad = 0.
                for k = 0 to 3 do
                    let dk = sqrt ((items[k].Cx - cx) * (items[k].Cx - cx) + (items[k].Cy - cy) * (items[k].Cy - cy) + (items[k].Cz - cz) * (items[k].Cz - cz)) + items[k].R
                    if dk > rad then rad <- dk
                Ball3(cx, cy, cz, rad)

    /// Welzl (1991) / Gartner (1999) move-to-front smallest enclosing ball of 3D spheres (bounded recursion depth <= 4).
    let welzlSmallestEnclosingBall (spheres: Ball3[]) : Ball3 =
        let m = spheres.Length
        if m = 0 then Ball3(0., 0., 0., 0.)
        elif m = 1 then spheres[0]
        else
            let order = Array.init m id
            let support = Array.zeroCreate<Ball3> 4

            let basisBall (sCount: int) =
                match sCount with
                | 0 -> Ball3(0., 0., 0., -1.)
                | 1 -> support[0]
                | 2 -> ball2 support[0] support[1]
                | 3 -> ball3 support[0] support[1] support[2]
                | _ -> ball4 support[0] support[1] support[2] support[3]

            let rec mtfMb (limit: int) (sCount: int) : Ball3 =
                let mutable cur = basisBall sCount
                if sCount < 4 then
                    let mutable i = 0
                    while i < limit do
                        let idx = order[i]
                        if not (encloses cur spheres[idx] 1e-11) then
                            support[sCount] <- spheres[idx]
                            cur <- mtfMb i (sCount + 1)
                            for k = i downto 1 do
                                order[k] <- order[k - 1]
                            order[0] <- idx
                        i <- i + 1
                cur

            let raw = mtfMb m 0
            // Exact numerical clamp pass guaranteeing strict enclosure of every child sphere
            let mutable finalR = max 0. raw.R
            for i = 0 to m - 1 do
                let s = spheres[i]
                let dx, dy, dz = s.Cx - raw.Cx, s.Cy - raw.Cy, s.Cz - raw.Cz
                let req = sqrt (dx * dx + dy * dy + dz * dz) + s.R
                if req > finalR then finalR <- req
            Ball3(raw.Cx, raw.Cy, raw.Cz, finalR)

    /// Exact 3-sphere trilateration solving ||p - c_m||^2 = d_m^2 for m in {a, b, c}. Returns 0, 1 or 2 solutions.
    let inline trilaterate3
        (ax: float, ay: float, az: float, da: float)
        (bx: float, by: float, bz: float, db: float)
        (cx: float, cy: float, cz: float, dc: float)
        : struct(int * Point3 * Point3) =
        let ux, uy, uz = bx - ax, by - ay, bz - az
        let lbSq = ux * ux + uy * uy + uz * uz
        let zeroPt = Point3.create 0. 0. 0.
        if lbSq <= 1e-20 then
            struct(0, zeroPt, zeroPt)
        else
            let lb = sqrt lbSq
            let exX, exY, exZ = ux / lb, uy / lb, uz / lb
            let vx, vy, vz = cx - ax, cy - ay, cz - az
            let ix = vx * exX + vy * exY + vz * exZ
            let px, py, pz = vx - ix * exX, vy - ix * exY, vz - ix * exZ
            let jySq = px * px + py * py + pz * pz
            if jySq <= 1e-20 then
                struct(0, zeroPt, zeroPt)
            else
                let jy = sqrt jySq
                let eyX, eyY, eyZ = px / jy, py / jy, pz / jy
                let ezX = exY * eyZ - exZ * eyY
                let ezY = exZ * eyX - exX * eyZ
                let ezZ = exX * eyY - exY * eyX
                let daSq, dbSq, dcSq = da * da, db * db, dc * dc
                let x = (daSq - dbSq + lbSq) / (2. * lb)
                let y = (daSq - dcSq + ix * ix + jySq - 2. * x * ix) / (2. * jy)
                let zSq = daSq - x * x - y * y
                if zSq < -1e-9 then
                    struct(0, zeroPt, zeroPt)
                else
                    let z = sqrt (max 0. zSq)
                    let baseX = ax + x * exX + y * eyX
                    let baseY = ay + x * exY + y * eyY
                    let baseZ = az + x * exZ + y * eyZ
                    let p1 = Point3.create (baseX + z * ezX) (baseY + z * ezY) (baseZ + z * ezZ)
                    if z <= 1e-12 then
                        struct(1, p1, p1)
                    else
                        let p2 = Point3.create (baseX - z * ezX) (baseY - z * ezY) (baseZ - z * ezZ)
                        struct(2, p1, p2)

    /// Packs m child spheres of radii[0 .. m - 1] (sorted descending) with inter-sibling gap `pad`.
    /// Centers the cluster around (0, 0, 0) via Welzl smallest enclosing ball and returns parent enclosing radius.
    let packSiblingSpheres
        (radii: float[])
        (pad: float)
        (outX: float[])
        (outY: float[])
        (outZ: float[])
        : float =
        let m = radii.Length
        if m = 0 then 0.
        elif m = 1 then
            outX[0] <- 0.; outY[0] <- 0.; outZ[0] <- 0.
            radii[0] + pad
        else
            let r0, r1 = radii[0], radii[1]
            let d01 = r0 + r1 + pad
            outX[0] <- -(r1 + pad * 0.5); outY[0] <- 0.; outZ[0] <- 0.
            outX[1] <- (r0 + pad * 0.5); outY[1] <- 0.; outZ[1] <- 0.

            if m >= 3 then
                let r2 = radii[2]
                let d02 = r0 + r2 + pad
                let d12 = r1 + r2 + pad
                let xRel =
                    if d01 <= 1e-12 then 0.
                    else (d02 * d02 - d12 * d12 + d01 * d01) / (2. * d01)
                let yRel = sqrt (max 0. (d02 * d02 - xRel * xRel))
                outX[2] <- outX[0] + xRel
                outY[2] <- yRel
                outZ[2] <- 0.

            let inline massOf (r: float) = r * r * r
            let mutable massSum = 0.
            let mutable wxSum, wySum, wzSum = 0., 0., 0.
            let initCount = min m 3
            for j = 0 to initCount - 1 do
                let w = massOf radii[j]
                massSum <- massSum + w
                wxSum <- wxSum + w * outX[j]
                wySum <- wySum + w * outY[j]
                wzSum <- wzSum + w * outZ[j]

            let isCollisionFree (px: float) (py: float) (pz: float) (rk: float) (placed: int) : bool =
                let mutable free = true
                let mutable j = 0
                while free && j < placed do
                    let req = (radii[j] + rk + pad) * (1. - 1e-7)
                    let dx, dy, dz = px - outX[j], py - outY[j], pz - outZ[j]
                    if dx * dx + dy * dy + dz * dz < req * req then
                        free <- false
                    j <- j + 1
                free

            // Reusable neighbor list for pair-distance-pruned front triple search
            let nbrBuf = Array.zeroCreate<int> m

            for k = 3 to m - 1 do
                let rk = radii[k]
                let comX = if massSum > 0. then wxSum / massSum else 0.
                let comY = if massSum > 0. then wySum / massSum else 0.
                let comZ = if massSum > 0. then wzSum / massSum else 0.

                let mutable bestFound = false
                let mutable bestDistSq = infinity
                let mutable bestX, bestY, bestZ = 0., 0., 0.

                let inline consider (pt: Point3) =
                    let dx, dy, dz = pt.X - comX, pt.Y - comY, pt.Z - comZ
                    let dSq = dx * dx + dy * dy + dz * dz
                    if dSq < bestDistSq && isCollisionFree pt.X pt.Y pt.Z rk k then
                        bestFound <- true
                        bestDistSq <- dSq
                        bestX <- pt.X; bestY <- pt.Y; bestZ <- pt.Z

                // Limit outer anchor pool on very high-degree nodes while testing all feasible tangent triples for typical degrees
                let anchorStart = if k <= 48 then 0 else k - 40
                for a = anchorStart to k - 3 do
                    let ax, ay, az = outX[a], outY[a], outZ[a]
                    let da = radii[a] + rk + pad
                    let mutable nLen = 0
                    for b = a + 1 to k - 1 do
                        let dx, dy, dz = outX[b] - ax, outY[b] - ay, outZ[b] - az
                        let dSq = dx * dx + dy * dy + dz * dz
                        let db = radii[b] + rk + pad
                        let maxSep = da + db
                        let minSep = abs (da - db)
                        if dSq <= maxSep * maxSep + 1e-9 && dSq >= minSep * minSep - 1e-9 then
                            nbrBuf[nLen] <- b
                            nLen <- nLen + 1

                    for bi = 0 to nLen - 2 do
                        let b = nbrBuf[bi]
                        let bx, by, bz = outX[b], outY[b], outZ[b]
                        let db = radii[b] + rk + pad
                        for ci = bi + 1 to nLen - 1 do
                            let c = nbrBuf[ci]
                            let cx, cy, cz = outX[c], outY[c], outZ[c]
                            let dc = radii[c] + rk + pad
                            let dxBC, dyBC, dzBC = cx - bx, cy - by, cz - bz
                            let dSqBC = dxBC * dxBC + dyBC * dyBC + dzBC * dzBC
                            let maxBC = db + dc
                            let minBC = abs (db - dc)
                            if dSqBC <= maxBC * maxBC + 1e-9 && dSqBC >= minBC * minBC - 1e-9 then
                                let struct(cnt, p1, p2) = trilaterate3 (ax, ay, az, da) (bx, by, bz, db) (cx, cy, cz, dc)
                                if cnt >= 1 then consider p1
                                if cnt >= 2 then consider p2

                // Fallback radial placement along deterministic Fibonacci ray if numerical degeneracy prevented 3-sphere tangency
                if not bestFound then
                    let golden = Math.PI * (3. - sqrt 5.)
                    let yDir = 1. - 2. * float ((k * 7) % 29) / 29.
                    let ring = sqrt (max 0. (1. - yDir * yDir))
                    let ang = float k * golden
                    let ux, uy, uz = ring * cos ang, yDir, ring * sin ang
                    let mutable tMax = 0.
                    for j = 0 to k - 1 do
                        let vx, vy, vz = outX[j] - comX, outY[j] - comY, outZ[j] - comZ
                        let proj = vx * ux + vy * uy + vz * uz
                        let perpSq = (vx * vx + vy * vy + vz * vz) - proj * proj
                        let req = radii[j] + rk + pad
                        if perpSq < req * req then
                            let tHit = proj + sqrt (max 0. (req * req - perpSq))
                            if tHit > tMax then tMax <- tHit
                    bestX <- comX + ux * tMax
                    bestY <- comY + uy * tMax
                    bestZ <- comZ + uz * tMax

                outX[k] <- bestX
                outY[k] <- bestY
                outZ[k] <- bestZ
                let wk = massOf rk
                massSum <- massSum + wk
                wxSum <- wxSum + wk * bestX
                wySum <- wySum + wk * bestY
                wzSum <- wzSum + wk * bestZ

            let balls = Array.init m (fun i -> Ball3(outX[i], outY[i], outZ[i], radii[i]))
            let enc = welzlSmallestEnclosingBall balls
            for i = 0 to m - 1 do
                outX[i] <- outX[i] - enc.Cx
                outY[i] <- outY[i] - enc.Cy
                outZ[i] <- outZ[i] - enc.Cz
            enc.R + pad

/// <summary>Bottom-up 3D tangent sphere packing engine with Welzl (1991) tightest enclosing balls.</summary>
[<RequireQualifiedAccess>]
module Pack3D =
    /// <summary>Computes a preorder list of 3D tangent-packed hierarchy spheres inscribed inside <c>config.Bounds</c>.</summary>
    let layout
        (config: Pack3DConfig)
        (root: HierarchyNode<'Id>)
        : Result<PackedSphere3D<'Id> list, LayoutError list> =
        if not (Double.IsFinite config.Padding) || config.Padding < 0. then
            Error [ InvalidObservation "Pack3D requires finite nonnegative Padding." ]
        else
            match Pack3DKernels.buildTree root with
            | Error errs -> Error errs
            | Ok tree ->
                let n = tree.Ids.Length
                let values = Pack3DKernels.computeValues config.SumPolicy tree
                let localX = Array.zeroCreate<float> n
                let localY = Array.zeroCreate<float> n
                let localZ = Array.zeroCreate<float> n
                let radii = Array.zeroCreate<float> n

                let inline leafRad (v: float) =
                    if v <= 0. then 1e-4
                    else max 1e-4 (Math.Pow(v, 1. / 3.))

                let runPackPass (localPad: float) =
                    for p = 0 to tree.Postorder.Length - 1 do
                        let u = tree.Postorder[p]
                        let ch = tree.Children[u]
                        if ch.Length = 0 then
                            radii[u] <- leafRad values[u]
                            localX[u] <- 0.; localY[u] <- 0.; localZ[u] <- 0.
                        else
                            let hasOwn =
                                config.SumPolicy = HierarchySumPolicy.IncludeOwnWeight && tree.OwnWeights[u] > 0.
                            let totalCount = ch.Length + (if hasOwn then 1 else 0)
                            let rawRadii = Array.zeroCreate<float> totalCount
                            for k = 0 to ch.Length - 1 do
                                rawRadii[k] <- radii[ch[k]]
                            if hasOwn then
                                rawRadii[ch.Length] <- leafRad tree.OwnWeights[u]

                            // Sort descending by radius with deterministic tie-break by child index
                            let order =
                                Array.init totalCount id
                                |> Array.sortWith (fun a b ->
                                    let c = compare rawRadii[b] rawRadii[a]
                                    if c <> 0 then c else compare a b)
                            let sortedR = Array.init totalCount (fun k -> rawRadii[order[k]])
                            let sx = Array.zeroCreate<float> totalCount
                            let sy = Array.zeroCreate<float> totalCount
                            let sz = Array.zeroCreate<float> totalCount
                            let parentR = Pack3DKernels.packSiblingSpheres sortedR localPad sx sy sz
                            radii[u] <- parentR
                            for k = 0 to totalCount - 1 do
                                let orig = order[k]
                                if orig < ch.Length then
                                    let childNode = ch[orig]
                                    localX[childNode] <- sx[k]
                                    localY[childNode] <- sy[k]
                                    localZ[childNode] <- sz[k]

                runPackPass 0.
                let minSide = min (Bounds3.sizeX config.Bounds) (min (Bounds3.sizeY config.Bounds) (Bounds3.sizeZ config.Bounds))
                if config.Padding > 0. && minSide > 0. && radii[0] > 0. then
                    let localPad = min 0.5 (config.Padding / minSide) * 2. * radii[0]
                    runPackPass localPad

                let targetRadius = 0.5 * minSide
                let scale = if radii[0] <= 0. then 0. else targetRadius / radii[0]
                let rootCenter = Bounds3.center config.Bounds

                let worldX = Array.zeroCreate<float> n
                let worldY = Array.zeroCreate<float> n
                let worldZ = Array.zeroCreate<float> n
                let worldR = Array.zeroCreate<float> n

                worldX[0] <- rootCenter.X
                worldY[0] <- rootCenter.Y
                worldZ[0] <- rootCenter.Z
                worldR[0] <- radii[0] * scale

                for i = 1 to n - 1 do
                    let par = tree.ParentIdx[i]
                    worldX[i] <- worldX[par] + localX[i] * scale
                    worldY[i] <- worldY[par] + localY[i] * scale
                    worldZ[i] <- worldZ[par] + localZ[i] * scale
                    worldR[i] <- radii[i] * scale

                let mutable result = []
                for i = n - 1 downto 0 do
                    result <-
                        { Id = tree.Ids[i]
                          Parent = tree.Parents[i]
                          Depth = tree.Depths[i]
                          Value = values[i]
                          Center = Point3.create worldX[i] worldY[i] worldZ[i]
                          Radius = max 0. worldR[i]
                          IsLeaf = (tree.Children[i].Length = 0) } :: result
                Ok result

    /// <summary>Converts <c>PackedSphere3D&lt;string&gt;</c> items into a <c>DiagramLayout3D</c> with leaves as <c>PositionedNode3D</c> items and internal spheres as enclosing <c>PositionedGroup3D</c> cuboids.</summary>
    let toDiagramLayout (spheres: PackedSphere3D<string> list) : DiagramLayout3D =
        let nodes =
            spheres
            |> List.choose (fun s ->
                if s.IsLeaf then
                    let d = 2. * s.Radius
                    let box = Kernels3D.nodeBox s.Center d d d
                    Some
                        { Id = s.Id
                          Kind = NodeKind.Standard
                          Center = s.Center
                          Bounds = box
                          Radius = s.Radius
                          ScaleFactor = 1.
                          ParentGroup = s.Parent
                          Layer = Some s.Depth }
                else None)
            |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))

        let groups =
            spheres
            |> List.choose (fun s ->
                if not s.IsLeaf then
                    let d = 2. * s.Radius
                    let box = Kernels3D.nodeBox s.Center d d d
                    Some
                        { Id = s.Id
                          ParentGroup = s.Parent
                          Bounds = box
                          InnerBounds = box
                          Depth = s.Depth }
                else None)
            |> List.sortWith (fun a b ->
                let c = compare a.Depth b.Depth
                if c <> 0 then c else StringComparer.Ordinal.Compare(a.Id, b.Id))

        let fallback = Bounds3.create 0. 0. 0. 0. 0. 0.
        { Bounds = Kernels3D.computeBounds3D fallback nodes groups []
          Nodes = nodes
          Groups = groups
          Associations = [] }

/// <summary>3D volumetric Voronoi Treemap engine generalizing Nocaj &amp; Brandes (2012) to convex 3D polyhedra via weighted Laguerre/power diagram clipping.</summary>
[<RequireQualifiedAccess>]
module VoronoiTreemap3D =
    let private cuboidPolyhedron (bounds: Bounds3) : Point3 list * int list list * float * Point3 =
        let x0, y0, z0 = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
        let sx, sy, sz = Bounds3.sizeX bounds, Bounds3.sizeY bounds, Bounds3.sizeZ bounds
        let x1, y1, z1 = x0 + sx, y0 + sy, z0 + sz
        let verts =
            [ Point3.create x0 y0 z0
              Point3.create x1 y0 z0
              Point3.create x1 y1 z0
              Point3.create x0 y1 z0
              Point3.create x0 y0 z1
              Point3.create x1 y0 z1
              Point3.create x1 y1 z1
              Point3.create x0 y1 z1 ]
        let faces =
            [ [ 0; 3; 2; 1 ] // -Z
              [ 4; 5; 6; 7 ] // +Z
              [ 0; 1; 5; 4 ] // -Y
              [ 3; 7; 6; 2 ] // +Y
              [ 0; 4; 7; 3 ] // -X
              [ 1; 2; 6; 5 ] ] // +X
        verts, faces, sx * sy * sz, Bounds3.center bounds

    let private inradiusAroundCentroid (verts: Point3[]) (faces: int list list) (centroid: Point3) : float =
        let mutable minDist = infinity
        for face in faces do
            match face with
            | i0 :: i1 :: i2 :: _ ->
                let p0, p1, p2 = verts[i0], verts[i1], verts[i2]
                let ux, uy, uz = p1.X - p0.X, p1.Y - p0.Y, p1.Z - p0.Z
                let vx, vy, vz = p2.X - p0.X, p2.Y - p0.Y, p2.Z - p0.Z
                let nx = uy * vz - uz * vy
                let ny = uz * vx - ux * vz
                let nz = ux * vy - uy * vx
                let nLen = sqrt (nx * nx + ny * ny + nz * nz)
                if nLen > 1e-12 then
                    let d = abs ((p0.X - centroid.X) * nx + (p0.Y - centroid.Y) * ny + (p0.Z - centroid.Z) * nz) / nLen
                    if d < minDist then minDist <- d
            | _ -> ()
        if Double.IsFinite minDist && minDist > 0. then minDist else 0.

    /// <summary>Recursively subdivides <c>bounds</c> into preorder 3D convex polyhedral Voronoi cells matching hierarchy weights within <c>volumeTolerance</c>.</summary>
    let layout
        (maxIterations: int)
        (volumeTolerance: float)
        (bounds: Bounds3)
        (root: HierarchyNode<'Id>)
        : Result<VoronoiTreemap3DCell<'Id> list, LayoutError list> =
        if maxIterations < 1 || maxIterations > 2000 then
            Error [ InvalidObservation "VoronoiTreemap3D requires maxIterations in [1, 2000]." ]
        elif not (Double.IsFinite volumeTolerance) || volumeTolerance <= 0. || volumeTolerance >= 1. then
            Error [ InvalidObservation "VoronoiTreemap3D requires volumeTolerance in (0, 1)." ]
        elif Bounds3.sizeX bounds <= 0. || Bounds3.sizeY bounds <= 0. || Bounds3.sizeZ bounds <= 0. then
            Error [ InvalidObservation "VoronoiTreemap3D requires Bounds3 with positive extents along X, Y and Z." ]
        else
            match Pack3DKernels.buildTree root with
            | Error errs -> Error errs
            | Ok tree ->
                let n = tree.Ids.Length
                let values = Pack3DKernels.computeValues HierarchySumPolicy.LeavesOnly tree
                let cellVerts = Array.zeroCreate<Point3 list> n
                let cellFaces = Array.zeroCreate<int list list> n
                let cellVols = Array.zeroCreate<float> n
                let cellCentroids = Array.zeroCreate<Point3> n

                let rVertsFallback, rFacesFallback, rVolFallback, rCentroidFallback = cuboidPolyhedron bounds
                match Voronoi3D.clipBoundsWeighted bounds [ rCentroidFallback, 0. ] with
                | Ok [ rootPoly ] when rootPoly.Vertices.Length >= 4 && rootPoly.Volume > 0. ->
                    cellVerts[0] <- rootPoly.Vertices
                    cellFaces[0] <- rootPoly.Faces
                    cellVols[0] <- rootPoly.Volume
                    cellCentroids[0] <- rootPoly.Centroid
                | _ ->
                    cellVerts[0] <- rVertsFallback
                    cellFaces[0] <- rFacesFallback
                    cellVols[0] <- rVolFallback
                    cellCentroids[0] <- rCentroidFallback

                let isRootBox = Array.zeroCreate<bool> n
                isRootBox[0] <- true
                let goldenAngle = Math.PI * (3. - sqrt 5.)
                let threeOverFourPi = 3. / (4. * Math.PI)
                let mutable clipErr : LayoutError list option = None

                let mutable u = 0
                while u < n && clipErr.IsNone do
                    let ch = tree.Children[u]
                    let m = ch.Length
                    if m = 1 then
                        let c = ch[0]
                        isRootBox[c] <- isRootBox[u]
                        cellVerts[c] <- cellVerts[u]
                        cellFaces[c] <- cellFaces[u]
                        cellVols[c] <- cellVols[u]
                        cellCentroids[c] <- cellCentroids[u]
                    elif m >= 2 then
                        let pVerts = cellVerts[u]
                        let pFaces = cellFaces[u]
                        let pVol = cellVols[u]
                        let pCentroid = cellCentroids[u]
                        let pVertArr = List.toArray pVerts

                        // Target volumes strictly proportional to positive child weights (or equal share if all zero)
                        let rawW = Array.init m (fun k -> values[ch[k]])
                        let mutable wSum = 0.
                        for k = 0 to m - 1 do wSum <- wSum + rawW[k]
                        let targetFrac =
                            if wSum <= 0. then Array.create m (1. / float m)
                            else
                                // Give zero-weight siblings a tiny floor so every sibling retains a valid non-empty convex polyhedron
                                let floored = Array.init m (fun k -> max (wSum * 1e-5) rawW[k])
                                let mutable fSum = 0.
                                for k = 0 to m - 1 do fSum <- fSum + floored[k]
                                Array.init m (fun k -> floored[k] / fSum)
                        let targetVol = Array.init m (fun k -> pVol * targetFrac[k])

                        // Place initial sites strictly inside the convex parent polyhedron on a small Fibonacci shell around pCentroid
                        let inRad = inradiusAroundCentroid pVertArr pFaces pCentroid
                        let initR =
                            if inRad > 1e-9 then 0.32 * inRad
                            else 0.1 * Math.Pow(max 1e-18 pVol, 1. / 3.)
                        let sites =
                            Array.init m (fun k ->
                                let y = 1. - 2. * (float k + 0.5) / float m
                                let ring = sqrt (max 0. (1. - y * y))
                                let th = float k * goldenAngle
                                let shell = initR * (0.65 + 0.35 * float ((k * 5) % m) / float m)
                                Point3.create
                                    (pCentroid.X + shell * ring * cos th)
                                    (pCentroid.Y + shell * y)
                                    (pCentroid.Z + shell * ring * sin th))
                        let weights = Array.zeroCreate<float> m

                        let mutable bestCells : PolyhedronCell3D[] option = None
                        let mutable bestErr = infinity
                        let mutable lastValidSites = Array.copy sites
                        let mutable lastValidWeights = Array.copy weights
                        let mutable stepGain = 0.85
                        let mutable converged = false
                        let mutable iter = 0

                        while iter < maxIterations && not converged && clipErr.IsNone do
                            let siteInput = [ for k in 0 .. m - 1 -> sites[k], weights[k] ]
                            let clippedResult =
                                if isRootBox[u] then
                                    Voronoi3D.clipBoundsWeighted bounds siteInput
                                else
                                    Voronoi3D.clipPolyhedronWeighted pVerts pFaces siteInput
                            match clippedResult with
                            | Error errs ->
                                clipErr <- Some errs
                            | Ok clipped ->
                                let bySite = Array.create<PolyhedronCell3D option> m None
                                for c in clipped do
                                    if c.Site >= 0 && c.Site < m then
                                        bySite[c.Site] <- Some c

                                let minVolFloor = max 1e-18 (pVol * 1e-10)
                                let mutable allValid = true
                                for k = 0 to m - 1 do
                                    match bySite[k] with
                                    | Some c when c.Volume > minVolFloor && c.Vertices.Length >= 4 -> ()
                                    | _ -> allValid <- false

                                if not allValid then
                                    // Backtrack halfway toward last known all-valid configuration
                                    stepGain <- max 0.05 (stepGain * 0.5)
                                    for k = 0 to m - 1 do
                                        sites[k] <- Point3.lerp lastValidSites[k] sites[k] 0.5
                                        weights[k] <- 0.5 * (lastValidWeights[k] + weights[k])
                                else
                                    let resolved = Array.init m (fun k -> bySite[k].Value)
                                    let mutable maxRelErr = 0.
                                    for k = 0 to m - 1 do
                                        let rel = abs (resolved[k].Volume - targetVol[k]) / max 1e-18 pVol
                                        if rel > maxRelErr then maxRelErr <- rel

                                    if maxRelErr < bestErr || bestCells.IsNone then
                                        bestErr <- maxRelErr
                                        bestCells <- Some resolved

                                    lastValidSites <- Array.copy sites
                                    lastValidWeights <- Array.copy weights

                                    if maxRelErr <= volumeTolerance then
                                        converged <- true
                                    elif iter + 1 < maxIterations then
                                        stepGain <- min 1.0 (stepGain * 1.06)
                                        let centroidBlend =
                                            if iter < (maxIterations * 3) / 4 then 0.8
                                            else 0.25
                                        let mutable wMean = 0.
                                        for k = 0 to m - 1 do
                                            let curCell = resolved[k]
                                            sites[k] <- Point3.lerp sites[k] curCell.Centroid centroidBlend
                                            let rCurSq = Math.Pow(max 1e-18 (curCell.Volume * threeOverFourPi), 2. / 3.)
                                            let rTgtSq = Math.Pow(max 1e-18 (targetVol[k] * threeOverFourPi), 2. / 3.)
                                            weights[k] <- weights[k] + stepGain * (rTgtSq - rCurSq)
                                            wMean <- wMean + weights[k]
                                        wMean <- wMean / float m
                                        for k = 0 to m - 1 do
                                            weights[k] <- weights[k] - wMean

                            iter <- iter + 1

                        if clipErr.IsNone then
                            match bestCells with
                            | Some chosen ->
                                for k = 0 to m - 1 do
                                    let childIdx = ch[k]
                                    let poly = chosen[k]
                                    cellVerts[childIdx] <- poly.Vertices
                                    cellFaces[childIdx] <- poly.Faces
                                    cellVols[childIdx] <- poly.Volume
                                    cellCentroids[childIdx] <- poly.Centroid
                            | None ->
                                clipErr <- Some [ InvalidTopology "VoronoiTreemap3D failed to compute a valid non-empty polyhedral partition." ]

                    u <- u + 1

                match clipErr with
                | Some errs -> Error errs
                | None ->
                    let mutable outList = []
                    for i = n - 1 downto 0 do
                        outList <-
                            { Id = tree.Ids[i]
                              Parent = tree.Parents[i]
                              Depth = tree.Depths[i]
                              Value = values[i]
                              Vertices = cellVerts[i]
                              Faces = cellFaces[i]
                              Volume = cellVols[i]
                              Centroid = cellCentroids[i] } :: outList
                    Ok outList

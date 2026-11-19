namespace Ranvier.D5.Geo

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>An occupied equal-area spherical Fibonacci bin with its center, aggregated weight, and assigned items.</summary>
type SphericalBin<'T> =
    { /// <summary>Zero-based Fibonacci lattice cell index in <c>0 .. count - 1</c>.</summary>
      CellIndex: int
      /// <summary>Geographic center of this Fibonacci cell on the unit sphere.</summary>
      Center: GeoPosition
      /// <summary>Compensated sum of nonnegative item weights assigned to this cell.</summary>
      TotalWeight: float
      /// <summary>Items assigned to this cell in input order.</summary>
      Members: 'T list }

/// <summary>Keinert et al. (2015) equal-area spherical Fibonacci point sets and O(1) closed-form nearest-cell inversion.</summary>
[<RequireQualifiedAccess>]
module SphericalFibonacci =
    let private phi = (1. + sqrt 5.) * 0.5
    let private invPhi = phi - 1.
    let private logPhiSq = log (phi * phi)
    let private sqrt5 = sqrt 5.

    let private toList (buf: ResizeArray<'T>) : 'T list =
        let mutable acc = []
        for i = buf.Count - 1 downto 0 do
            acc <- buf[i] :: acc
        acc

    let inline private validPosition (pos: GeoPosition) =
        Double.IsFinite pos.Longitude
        && Double.IsFinite pos.Latitude
        && pos.Latitude >= -90.
        && pos.Latitude <= 90.

    let inline private sfVector (count: int) (index: int) : struct(float * float * float) =
        let z = 1. - (2. * float index + 1.) / float count
        let r = sqrt (max 0. (1. - z * z))
        let turn = float index * invPhi
        let frac = turn - floor turn
        let angle = 2. * Math.PI * frac
        let struct(s, c) = Math.SinCos angle
        struct(r * c, r * s, z)

    let private inverseNearest (count: int) (qx: float) (qy: float) (qz: float) : int =
        if count <= 1 then
            0
        elif count <= 24 then
            let mutable bestIdx = 0
            let mutable bestDot = Double.NegativeInfinity
            for i = 0 to count - 1 do
                let struct(sx, sy, sz) = sfVector count i
                let d = qx * sx + qy * sy + qz * sz
                if d > bestDot then
                    bestDot <- d
                    bestIdx <- i
            bestIdx
        else
            let rawPhi = atan2 qy qx
            let azim = if rawPhi < 0. then rawPhi + 2. * Math.PI else rawPhi
            let oneMinusZ2 = max 0. (1. - qz * qz)
            let kZone =
                if oneMinusZ2 <= 1e-16 then 2.
                else max 2. (floor (log (float count * Math.PI * sqrt5 * oneMinusZ2) / logPhiSq))
            let fk = Math.Pow(phi, kZone) / sqrt5
            let f0 = Math.Round fk
            let f1 = Math.Round(fk * phi)
            let inline signedFrac (x: float) = x - Math.Round x
            let b00 = 2. * Math.PI * signedFrac (f0 * invPhi)
            let b01 = 2. * Math.PI * signedFrac (f1 * invPhi)
            let b10 = -2. * f0 / float count
            let b11 = -2. * f1 / float count
            let det = b00 * b11 - b01 * b10
            let zShift = qz - (1. - 1. / float count)
            let c0, c1 =
                if abs det <= 1e-30 then 0L, 0L
                else
                    let u0 = (b11 * azim - b01 * zShift) / det
                    let u1 = (-b10 * azim + b00 * zShift) / det
                    int64 (floor u0), int64 (floor u1)

            let f0L = int64 f0
            let f1L = int64 f1
            let nL = int64 count
            let mutable bestIdx = 0
            let mutable bestDot = Double.NegativeInfinity
            let inline testIndex (idx: int) =
                let struct(sx, sy, sz) = sfVector count idx
                let d = qx * sx + qy * sy + qz * sz
                if d > bestDot || (d = bestDot && idx < bestIdx) then
                    bestDot <- d
                    bestIdx <- idx

            // Test 4x4 local Fibonacci zone stencil so curved spherical Voronoi boundaries never miss the exact closest site.
            for du = -1 to 2 do
                for dv = -1 to 2 do
                    let cand = (c0 + int64 du) * f0L + (c1 + int64 dv) * f1L
                    if cand >= 0L && cand < nL then
                        testIndex (int cand)

            // Guard polar caps in O(1) where cylindrical zone approximation transitions to polar rings.
            let polarBand = 24. / float count
            if 1. - qz <= polarBand then
                let capHi = min (count - 1) 15
                for idx = 0 to capHi do
                    testIndex idx
            if 1. + qz <= polarBand then
                let capLo = max 0 (count - 16)
                for idx = capLo to count - 1 do
                    testIndex idx

            // Fallback safety if extreme lattice clamping skipped [0..count-1].
            if Double.IsNegativeInfinity bestDot then
                let approx = int (Math.Clamp(Math.Round((1. - qz) * 0.5 * float count - 0.5), 0., float (count - 1)))
                for idx = max 0 (approx - 8) to min (count - 1) (approx + 8) do
                    testIndex idx

            bestIdx

    /// <summary>Computes the <c>index</c>-th spherical Fibonacci lattice point in O(1) time for <c>0 &lt;= index &lt; count</c>.</summary>
    let point (count: int) (index: int) : Result<GeoPosition, LayoutError list> =
        if count < 1 then
            Error [ InvalidObservation "Spherical Fibonacci count must be at least 1." ]
        elif count > 100_000_000 then
            Error [ BudgetExceeded "Spherical Fibonacci count exceeds 100,000,000." ]
        elif index < 0 || index >= count then
            Error [ InvalidObservation "Spherical Fibonacci index must lie in [0, count - 1]." ]
        else
            let struct(x, y, z) = sfVector count index
            Ok(GeoMath.position struct(x, y, z))

    /// <summary>Generates all <c>count</c> quasi-uniform equal-area spherical Fibonacci lattice points on the unit sphere.</summary>
    let points (count: int) : Result<GeoPosition list, LayoutError list> =
        if count < 1 then
            Error [ InvalidObservation "Spherical Fibonacci count must be at least 1." ]
        elif count > 1_000_000 then
            Error [ BudgetExceeded "Spherical Fibonacci point list count exceeds 1,000,000." ]
        else
            let arr = Array.zeroCreate<GeoPosition> count
            for i = 0 to count - 1 do
                let struct(x, y, z) = sfVector count i
                arr[i] <- GeoMath.position struct(x, y, z)
            Ok(Array.toList arr)

    /// <summary>Finds the exact closest spherical Fibonacci lattice index in <c>0 .. count - 1</c> in O(1) time (Keinert et al. 2015).</summary>
    let nearestCell (count: int) (position: GeoPosition) : Result<int, LayoutError list> =
        if count < 1 then
            Error [ InvalidObservation "Spherical Fibonacci count must be at least 1." ]
        elif count > 100_000_000 then
            Error [ BudgetExceeded "Spherical Fibonacci count exceeds 100,000,000." ]
        elif not (validPosition position) then
            Error [ InvalidObservation "Geographic position requires finite degrees and latitude in [-90, 90]." ]
        else
            let struct(vx, vy, vz) = GeoMath.vector position
            Ok(inverseNearest count vx vy vz)

    /// <summary>Assigns weighted geographic items to their nearest spherical Fibonacci cell in O(1) time per item and returns occupied bins ordered by cell index.</summary>
    let aggregate
        (count: int)
        (locate: 'T -> GeoPosition)
        (weight: 'T -> float)
        (items: 'T list)
        : Result<SphericalBin<'T> list, LayoutError list> =
        if count < 1 then
            Error [ InvalidObservation "Spherical Fibonacci count must be at least 1." ]
        elif count > 100_000_000 then
            Error [ BudgetExceeded "Spherical Fibonacci count exceeds 100,000,000." ]
        elif items.IsEmpty then
            Ok []
        else
            let buckets = Dictionary<int, ResizeArray<'T>>()
            let sums = Dictionary<int, struct(float * float)>()
            let mutable errOpt: LayoutError voption = ValueNone
            let mutable cur = items
            while errOpt.IsNone && not cur.IsEmpty do
                let item = cur.Head
                cur <- cur.Tail
                let pos = locate item
                let w = weight item
                if not (validPosition pos) then
                    errOpt <- ValueSome(InvalidObservation "Item geographic position must be finite with latitude in [-90, 90].")
                elif not (Double.IsFinite w) || w < 0. then
                    errOpt <- ValueSome(InvalidObservation "Spherical bin item weights must be finite and nonnegative.")
                else
                    let struct(vx, vy, vz) = GeoMath.vector pos
                    let cellIdx = inverseNearest count vx vy vz
                    match buckets.TryGetValue cellIdx with
                    | true, listBuf ->
                        listBuf.Add item
                        let struct(tot, corr) = sums[cellIdx]
                        let adj = w - corr
                        let nextTot = tot + adj
                        if not (Double.IsFinite nextTot) then
                            errOpt <- ValueSome(InvalidObservation "Spherical bin total weight overflowed finite range.")
                        else
                            sums[cellIdx] <- struct(nextTot, (nextTot - tot) - adj)
                    | false, _ ->
                        let listBuf = ResizeArray<'T>(4)
                        listBuf.Add item
                        buckets.Add(cellIdx, listBuf)
                        sums.Add(cellIdx, struct(w, 0.))

            match errOpt with
            | ValueSome err -> Error [ err ]
            | ValueNone ->
                let keys = Array.zeroCreate<int> buckets.Count
                let mutable k = 0
                for kv in buckets do
                    keys[k] <- kv.Key
                    k <- k + 1
                Array.sortInPlaceWith compare keys
                let out = Array.zeroCreate<SphericalBin<'T>> keys.Length
                for i = 0 to keys.Length - 1 do
                    let cellIdx = keys[i]
                    let struct(cx, cy, cz) = sfVector count cellIdx
                    let struct(tot, _) = sums[cellIdx]
                    out[i] <-
                        { CellIndex = cellIdx
                          Center = GeoMath.position struct(cx, cy, cz)
                          TotalWeight = tot
                          Members = toList buckets[cellIdx] }
                Ok(Array.toList out)

/// <summary>A convex spherical Voronoi polygon cell on the unit sphere with solid angle in steradians.</summary>
type SphericalCell<'Id> =
    { /// <summary>Caller-supplied site identifier.</summary>
      Id: 'Id
      /// <summary>Generator site position on the sphere.</summary>
      Site: GeoPosition
      /// <summary>Voronoi cell vertices ordered counter-clockwise around the outward site normal.</summary>
      Vertices: GeoPosition list
      /// <summary>Exact spherical solid angle of the cell in steradians (summing to <c>4 * pi</c> across all cells).</summary>
      Steradians: float }

/// <summary>Exact spherical Voronoi diagrams and Centroidal Voronoi Tessellation via 3D unit-sphere convex hulls (Renka 1997).</summary>
[<RequireQualifiedAccess>]
module SphericalVoronoi =
    type private Vec3 =
        struct
            val Ux: float
            val Uy: float
            val Uz: float
            new(ux: float, uy: float, uz: float) = { Ux = ux; Uy = uy; Uz = uz }
        end

    type private HullFace =
        { V0: int
          V1: int
          V2: int
          Nx: float
          Ny: float
          Nz: float
          Offset: float
          mutable Alive: bool }

    let inline private dot (a: Vec3) (b: Vec3) =
        a.Ux * b.Ux + a.Uy * b.Uy + a.Uz * b.Uz

    let inline private sub (a: Vec3) (b: Vec3) =
        Vec3(a.Ux - b.Ux, a.Uy - b.Uy, a.Uz - b.Uz)

    let inline private cross (a: Vec3) (b: Vec3) =
        Vec3(a.Uy * b.Uz - a.Uz * b.Uy, a.Uz * b.Ux - a.Ux * b.Uz, a.Ux * b.Uy - a.Uy * b.Ux)

    let inline private normSq (a: Vec3) =
        a.Ux * a.Ux + a.Uy * a.Uy + a.Uz * a.Uz

    let inline private normalize (a: Vec3) =
        let len = Double.Hypot(Double.Hypot(a.Ux, a.Uy), a.Uz)
        if len > 0. then Vec3(a.Ux / len, a.Uy / len, a.Uz / len) else a

    let inline private edgeKey (u: int) (v: int) : int64 =
        (int64 u <<< 32) ||| int64 (uint32 v)

    let private makeFace (pts: Vec3 array) (interior: Vec3) (a: int) (b: int) (c: int) : HullFace =
        let pa, pb, pc = pts[a], pts[b], pts[c]
        let rawN = cross (sub pb pa) (sub pc pa)
        let nUnit = normalize rawN
        let outCheck = nUnit.Ux * (pa.Ux - interior.Ux) + nUnit.Uy * (pa.Uy - interior.Uy) + nUnit.Uz * (pa.Uz - interior.Uz)
        if outCheck >= 0. then
            let off = nUnit.Ux * pa.Ux + nUnit.Uy * pa.Uy + nUnit.Uz * pa.Uz
            { V0 = a; V1 = b; V2 = c; Nx = nUnit.Ux; Ny = nUnit.Uy; Nz = nUnit.Uz; Offset = off; Alive = true }
        else
            let nx, ny, nz = -nUnit.Ux, -nUnit.Uy, -nUnit.Uz
            let off = nx * pa.Ux + ny * pa.Uy + nz * pa.Uz
            { V0 = a; V1 = c; V2 = b; Nx = nx; Ny = ny; Nz = nz; Offset = off; Alive = true }

    let private makeConeFace (pts: Vec3 array) (interior: Vec3) (u: int) (v: int) (k: int) : HullFace =
        let pu, pv, pk = pts[u], pts[v], pts[k]
        let rawN = cross (sub pv pu) (sub pk pu)
        let nUnit = normalize rawN
        let outCheck = nUnit.Ux * (pu.Ux - interior.Ux) + nUnit.Uy * (pu.Uy - interior.Uy) + nUnit.Uz * (pu.Uz - interior.Uz)
        let nx, ny, nz = if outCheck >= 0. then nUnit.Ux, nUnit.Uy, nUnit.Uz else -nUnit.Ux, -nUnit.Uy, -nUnit.Uz
        let off = nx * pu.Ux + ny * pu.Uy + nz * pu.Uz
        { V0 = u; V1 = v; V2 = k; Nx = nx; Ny = ny; Nz = nz; Offset = off; Alive = true }

    /// Builds the outward-oriented 3D convex hull of distinct non-coplanar unit vectors on S^2, returning cell vertex loops on S^2 and steradians.
    let private computeUnitVectors (pts: Vec3 array) : Result<struct(Vec3 array * float) array, LayoutError list> =
        let n = pts.Length
        if n < 4 then
            Error [ InvalidTopology "Spherical Voronoi requires at least four non-coplanar sites." ]
        else
            // O(N log N) duplicate-site check via Z-sorted sweep.
            let zOrder = Array.init n id
            Array.sortInPlaceWith (fun a b -> compare pts[a].Uz pts[b].Uz) zOrder
            let mutable hasDuplicate = false
            let mutable i = 0
            while not hasDuplicate && i < n do
                let idxA = zOrder[i]
                let pa = pts[idxA]
                let mutable j = i + 1
                while not hasDuplicate && j < n && pts[zOrder[j]].Uz - pa.Uz <= 1e-9 do
                    let pb = pts[zOrder[j]]
                    if normSq (sub pa pb) <= 1e-18 then
                        hasDuplicate <- true
                    j <- j + 1
                i <- i + 1

            if hasDuplicate then
                Error [ InvalidTopology "Spherical Voronoi sites must be distinct locations on the sphere." ]
            else
                // Select maximally non-degenerate initial tetrahedron (i0, i1, i2, i3).
                let i0 = 0
                let mutable i1 = 1
                let mutable bestD2 = -1.
                for k = 1 to n - 1 do
                    let d2 = normSq (sub pts[k] pts[i0])
                    if d2 > bestD2 then
                        bestD2 <- d2
                        i1 <- k

                let e01 = sub pts[i1] pts[i0]
                let mutable i2 = -1
                let mutable bestArea2 = -1.
                for k = 1 to n - 1 do
                    if k <> i1 then
                        let a2 = normSq (cross e01 (sub pts[k] pts[i0]))
                        if a2 > bestArea2 then
                            bestArea2 <- a2
                            i2 <- k

                if i2 < 0 || bestArea2 <= 1e-22 then
                    Error [ InvalidTopology "Spherical Voronoi sites are collinear on the sphere." ]
                else
                    let baseCross = cross e01 (sub pts[i2] pts[i0])
                    let mutable i3 = -1
                    let mutable bestVol = -1.
                    for k = 1 to n - 1 do
                        if k <> i1 && k <> i2 then
                            let v = abs (dot (sub pts[k] pts[i0]) baseCross)
                            if v > bestVol then
                                bestVol <- v
                                i3 <- k

                    if i3 < 0 || bestVol <= 1e-11 then
                        Error [ InvalidTopology "Spherical Voronoi requires at least four non-coplanar sites." ]
                    else
                        let interior =
                            Vec3(
                                0.25 * (pts[i0].Ux + pts[i1].Ux + pts[i2].Ux + pts[i3].Ux),
                                0.25 * (pts[i0].Uy + pts[i1].Uy + pts[i2].Uy + pts[i3].Uy),
                                0.25 * (pts[i0].Uz + pts[i1].Uz + pts[i2].Uz + pts[i3].Uz)
                            )

                        let faces = ResizeArray<HullFace>(2 * n)
                        let edgeMap = Dictionary<int64, int>(6 * n)

                        let registerFace (f: HullFace) =
                            let idx = faces.Count
                            faces.Add f
                            edgeMap[edgeKey f.V0 f.V1] <- idx
                            edgeMap[edgeKey f.V1 f.V2] <- idx
                            edgeMap[edgeKey f.V2 f.V0] <- idx

                        let unregisterFace (f: HullFace) =
                            f.Alive <- false
                            edgeMap.Remove(edgeKey f.V0 f.V1) |> ignore
                            edgeMap.Remove(edgeKey f.V1 f.V2) |> ignore
                            edgeMap.Remove(edgeKey f.V2 f.V0) |> ignore

                        registerFace (makeFace pts interior i0 i1 i2)
                        registerFace (makeFace pts interior i0 i2 i3)
                        registerFace (makeFace pts interior i0 i3 i1)
                        registerFace (makeFace pts interior i1 i2 i3)

                        let visSet = HashSet<int>()
                        let bfsQueue = Queue<int>()
                        let horizonEdges = ResizeArray<struct(int * int)>(32)

                        for k = 0 to n - 1 do
                            if k <> i0 && k <> i1 && k <> i2 && k <> i3 then
                                let pk = pts[k]
                                let mutable bestFace = -1
                                let mutable maxDist = 1e-13
                                for fIdx = 0 to faces.Count - 1 do
                                    let f = faces[fIdx]
                                    if f.Alive then
                                        let d = pk.Ux * f.Nx + pk.Uy * f.Ny + pk.Uz * f.Nz - f.Offset
                                        if d > maxDist then
                                            maxDist <- d
                                            bestFace <- fIdx

                                if bestFace >= 0 then
                                    visSet.Clear()
                                    bfsQueue.Clear()
                                    visSet.Add bestFace |> ignore
                                    bfsQueue.Enqueue bestFace

                                    let inline enqueueNeighbor (u: int) (v: int) =
                                        match edgeMap.TryGetValue(edgeKey v u) with
                                        | true, nbrIdx ->
                                            let nf = faces[nbrIdx]
                                            if nf.Alive && not (visSet.Contains nbrIdx) then
                                                let d = pk.Ux * nf.Nx + pk.Uy * nf.Ny + pk.Uz * nf.Nz - nf.Offset
                                                if d > 1e-13 then
                                                    visSet.Add nbrIdx |> ignore
                                                    bfsQueue.Enqueue nbrIdx
                                        | false, _ -> ()

                                    while bfsQueue.Count > 0 do
                                        let curIdx = bfsQueue.Dequeue()
                                        let cf = faces[curIdx]
                                        enqueueNeighbor cf.V0 cf.V1
                                        enqueueNeighbor cf.V1 cf.V2
                                        enqueueNeighbor cf.V2 cf.V0

                                    // Topological disk closure: absorb any non-visible face bounded on >= 2 sides by visSet that is near-coplanar.
                                    let mutable changed = true
                                    while changed do
                                        changed <- false
                                        for fIdx = 0 to faces.Count - 1 do
                                            let f = faces[fIdx]
                                            if f.Alive && not (visSet.Contains fIdx) then
                                                let inline isVisTwin u v =
                                                    match edgeMap.TryGetValue(edgeKey v u) with
                                                    | true, tIdx -> visSet.Contains tIdx
                                                    | false, _ -> false
                                                let mutable visNbrs = 0
                                                if isVisTwin f.V0 f.V1 then visNbrs <- visNbrs + 1
                                                if isVisTwin f.V1 f.V2 then visNbrs <- visNbrs + 1
                                                if isVisTwin f.V2 f.V0 then visNbrs <- visNbrs + 1
                                                let d = pk.Ux * f.Nx + pk.Uy * f.Ny + pk.Uz * f.Nz - f.Offset
                                                if visNbrs >= 2 && d > -1e-9 then
                                                    visSet.Add fIdx |> ignore
                                                    changed <- true

                                    horizonEdges.Clear()
                                    for vIdx in visSet do
                                        let vf = faces[vIdx]
                                        let inline checkHorizon u v =
                                            match edgeMap.TryGetValue(edgeKey v u) with
                                            | true, twinIdx ->
                                                if not (visSet.Contains twinIdx) then
                                                    horizonEdges.Add(struct(u, v))
                                            | false, _ -> ()
                                        checkHorizon vf.V0 vf.V1
                                        checkHorizon vf.V1 vf.V2
                                        checkHorizon vf.V2 vf.V0

                                    for vIdx in visSet do
                                        unregisterFace faces[vIdx]

                                    for h = 0 to horizonEdges.Count - 1 do
                                        let struct(u, v) = horizonEdges[h]
                                        registerFace (makeConeFace pts interior u v k)

                        // Collect incident hull face normals (spherical circumcenters) per site.
                        let incident = Array.init n (fun _ -> ResizeArray<Vec3>(8))
                        for fIdx = 0 to faces.Count - 1 do
                            let f = faces[fIdx]
                            if f.Alive then
                                let nv = Vec3(f.Nx, f.Ny, f.Nz)
                                incident[f.V0].Add nv
                                incident[f.V1].Add nv
                                incident[f.V2].Add nv

                        let cells = Array.zeroCreate<struct(Vec3 array * float)> n
                        let mutable topologyOk = true
                        let mutable sIdx = 0
                        while topologyOk && sIdx < n do
                            let sVec = pts[sIdx]
                            let rawVerts = incident[sIdx]
                            if rawVerts.Count < 3 then
                                topologyOk <- false
                            else
                                // Build tangent frame (e1, e2) with e1 x e2 = +sVec.
                                let refAxis =
                                    if abs sVec.Ux <= abs sVec.Uy && abs sVec.Ux <= abs sVec.Uz then Vec3(1., 0., 0.)
                                    elif abs sVec.Uy <= abs sVec.Uz then Vec3(0., 1., 0.)
                                    else Vec3(0., 0., 1.)
                                let e1 = normalize (cross refAxis sVec)
                                let e2 = cross sVec e1
                                let cnt = rawVerts.Count
                                let angles = Array.init cnt (fun k ->
                                    let v = rawVerts[k]
                                    atan2 (dot v e2) (dot v e1))
                                let order = Array.init cnt id
                                Array.sortInPlaceWith (fun a b -> compare angles[a] angles[b]) order

                                let dedup = ResizeArray<Vec3>(cnt)
                                for k = 0 to cnt - 1 do
                                    let v = rawVerts[order[k]]
                                    if dedup.Count = 0 || normSq (sub dedup[dedup.Count - 1] v) > 1e-18 then
                                        dedup.Add v
                                while dedup.Count > 1 && normSq (sub dedup[dedup.Count - 1] dedup[0]) <= 1e-18 do
                                    dedup.RemoveAt(dedup.Count - 1)

                                let m = dedup.Count
                                if m < 3 then
                                    topologyOk <- false
                                else
                                    let vArr = dedup.ToArray()
                                    let mutable solidAngle = 0.
                                    for k = 0 to m - 1 do
                                        let va = vArr[k]
                                        let vb = vArr[(k + 1) % m]
                                        if dot sVec va <= -1. + 1e-11 || dot va vb <= -1. + 1e-11 then
                                            topologyOk <- false
                                        else
                                            // Van Oosterom & Strackee (1983) exact spherical triangle solid angle.
                                            let det = dot sVec (cross va vb)
                                            let denom = 1. + dot sVec va + dot va vb + dot vb sVec
                                            let triOmega = 2. * atan2 det denom
                                            solidAngle <- solidAngle + triOmega
                                    if solidAngle <= 0. || not (Double.IsFinite solidAngle) then
                                        topologyOk <- false
                                    else
                                        cells[sIdx] <- struct(vArr, solidAngle)
                            sIdx <- sIdx + 1

                        if not topologyOk then
                            Error [ InvalidTopology "Sites lie in a degenerate single-hemisphere cap producing fewer than three distinct vertices or antipodal arcs." ]
                        else
                            Ok cells

    /// Computes exact surface-area-weighted spherical centroid on S^2 via Stokes' boundary integral over great-circle arcs.
    let private sphericalCellCentroid (fallback: Vec3) (verts: Vec3 array) : Vec3 =
        let m = verts.Length
        if m < 3 then
            fallback
        else
            let mutable mx, my, mz = 0., 0., 0.
            for k = 0 to m - 1 do
                let va = verts[k]
                let vb = verts[(k + 1) % m]
                let cr = cross va vb
                let sinLen = Double.Hypot(Double.Hypot(cr.Ux, cr.Uy), cr.Uz)
                if sinLen > 1e-24 then
                    let arcLen = atan2 sinLen (dot va vb)
                    let factor = arcLen / sinLen
                    mx <- mx + cr.Ux * factor
                    my <- my + cr.Uy * factor
                    mz <- mz + cr.Uz * factor
            let norm = Double.Hypot(Double.Hypot(mx, my), mz)
            if norm > 1e-20 then Vec3(mx / norm, my / norm, mz / norm) else fallback

    let private parseSites (sites: ('Id * GeoPosition) list) : Result<('Id * GeoPosition * Vec3) array, LayoutError list> =
        let arr = List.toArray sites
        let parsed = Array.zeroCreate<'Id * GeoPosition * Vec3> arr.Length
        let mutable ok = true
        for i = 0 to arr.Length - 1 do
            let idVal, pos = arr[i]
            if not (Double.IsFinite pos.Longitude && Double.IsFinite pos.Latitude && pos.Latitude >= -90. && pos.Latitude <= 90.) then
                ok <- false
            else
                let normPos = GeoPosition.normalize pos
                let struct(vx, vy, vz) = GeoMath.vector normPos
                parsed[i] <- (idVal, normPos, normalize (Vec3(vx, vy, vz)))
        if not ok then
            Error [ InvalidObservation "Spherical Voronoi sites require finite degrees and latitude in [-90, 90]." ]
        else
            Ok parsed

    /// <summary>Computes the exact spherical Voronoi diagram on the unit sphere for <c>N &gt;= 4</c> distinct non-coplanar sites.</summary>
    let compute (sites: ('Id * GeoPosition) list) : Result<SphericalCell<'Id> list, LayoutError list> =
        match parseSites sites with
        | Error errs -> Error errs
        | Ok parsed ->
            let unitPts = Array.init parsed.Length (fun i -> let _, _, v = parsed[i] in v)
            match computeUnitVectors unitPts with
            | Error errs -> Error errs
            | Ok rawCells ->
                let out = Array.zeroCreate<SphericalCell<'Id>> parsed.Length
                for i = 0 to parsed.Length - 1 do
                    let idVal, sitePos, _ = parsed[i]
                    let struct(vArr, omega) = rawCells[i]
                    let geoVerts =
                        Array.init vArr.Length (fun k ->
                            let v = vArr[k]
                            GeoMath.position struct(v.Ux, v.Uy, v.Uz))
                        |> Array.toList
                    out[i] <-
                        { Id = idVal
                          Site = sitePos
                          Vertices = geoVerts
                          Steradians = omega }
                Ok(Array.toList out)

    /// <summary>Relaxes spherical Voronoi generator sites toward their exact spherical polygon centroids over <c>iterations</c> Lloyd steps.</summary>
    let relaxCvt (iterations: int) (sites: ('Id * GeoPosition) list) : Result<SphericalCell<'Id> list, LayoutError list> =
        if iterations < 0 then
            Error [ InvalidObservation "Spherical CVT iterations must be nonnegative." ]
        else
            match parseSites sites with
            | Error errs -> Error errs
            | Ok parsed ->
                let ids = Array.init parsed.Length (fun i -> let idVal, _, _ = parsed[i] in idVal)
                let curPts = Array.init parsed.Length (fun i -> let _, _, v = parsed[i] in v)
                if iterations = 0 then
                    compute sites
                else
                    let mutable step = 0
                    let mutable moving = true
                    let mutable errOpt: LayoutError list voption = ValueNone
                    while step < iterations && moving && errOpt.IsNone do
                        match computeUnitVectors curPts with
                        | Error errs -> errOpt <- ValueSome errs
                        | Ok rawCells ->
                            let mutable maxShiftSq = 0.
                            for i = 0 to curPts.Length - 1 do
                                let struct(vArr, _) = rawCells[i]
                                let nextVec = sphericalCellCentroid curPts[i] vArr
                                let dSq = normSq (sub nextVec curPts[i])
                                if dSq > maxShiftSq then maxShiftSq <- dSq
                                curPts[i] <- nextVec
                            if maxShiftSq <= 1e-24 then
                                moving <- false
                        step <- step + 1

                    match errOpt with
                    | ValueSome errs -> Error errs
                    | ValueNone ->
                        let finalSites =
                            List.init curPts.Length (fun i ->
                                let v = curPts[i]
                                ids[i], GeoMath.position struct(v.Ux, v.Uy, v.Uz))
                        compute finalSites

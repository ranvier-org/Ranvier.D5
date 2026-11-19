namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Configuration for <c>KdeBundle3D</c> volumetric 3D Kernel Density Estimation edge bundling (Hurter et al. 2012; van der Zwan et al. 2016 <c>CUBu</c>).</summary>
type KdeBundle3DConfig =
    { /// <summary>Cubic 3D density grid resolution <c>G</c> along each axis (<c>8..160</c>; default <c>48</c>).</summary>
      GridSize: int
      /// <summary>Number of KDE splat-smooth-advect-relax iterations (<c>1..128</c>; default <c>8</c>).</summary>
      Iterations: int
      /// <summary>Initial separable smoothing kernel radius in voxel units (default <c>5.0</c>).</summary>
      InitialKernelRadius: float
      /// <summary>Per-iteration kernel radius and step cooling multiplier in <c>(0, 1]</c> (default <c>0.78</c>).</summary>
      KernelDecay: float
      /// <summary>Initial advection step length in voxel units along the normalized 3D density gradient (default <c>1.6</c>).</summary>
      StepSize: float
      /// <summary>Uniform polyline sample count per bundled association including pinned endpoints (<c>3..256</c>; default <c>20</c>).</summary>
      SamplePointsPerEdge: int }

/// <summary>Defaults for <c>KdeBundle3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module KdeBundle3DConfig =
    /// <summary>Default 3D KDE edge-bundling settings: <c>48^3</c> voxel grid, 8 iterations, initial radius 5.0, decay 0.78, step 1.6 and 20 samples per edge.</summary>
    let defaultValue : KdeBundle3DConfig =
        { GridSize = 48
          Iterations = 8
          InitialKernelRadius = 5.0
          KernelDecay = 0.78
          StepSize = 1.6
          SamplePointsPerEdge = 20 }

/// <summary>3D generalization of Holten (2006) Hierarchical Edge Bundling over <c>DiagramLayout3D</c> using 3D LCA control polygons and clamped uniform cubic B-splines.</summary>
[<RequireQualifiedAccess>]
module HierarchicalBundle3D =
    let private evaluateClampedBSpline3D (controls: Point3[]) (segments: int) : Point3[] * Point3 list =
        let m = controls.Length
        let outCount = segments + 1
        let outArr = Array.zeroCreate<Point3> outCount
        if m = 0 then
            [||], []
        elif m = 1 then
            for k = 0 to segments do outArr[k] <- controls[0]
            let mutable wps = []
            for k = segments downto 0 do wps <- outArr[k] :: wps
            outArr, wps
        elif m = 2 then
            let p0, p1 = controls[0], controls[1]
            outArr[0] <- p0
            outArr[segments] <- p1
            for k = 1 to segments - 1 do
                outArr[k] <- Point3.lerp p0 p1 (float k / float segments)
            let mutable wps = []
            for k = segments downto 0 do wps <- outArr[k] :: wps
            outArr, wps
        else
            // Triply-clamped endpoint control polygon of length m + 4 -> (m + 1) uniform C^2 cubic spans
            let padLen = m + 4
            let padded = Array.zeroCreate<Point3> padLen
            padded[0] <- controls[0]
            padded[1] <- controls[0]
            padded[2] <- controls[0]
            for j = 1 to m - 2 do
                padded[j + 2] <- controls[j]
            padded[m + 1] <- controls[m - 1]
            padded[m + 2] <- controls[m - 1]
            padded[m + 3] <- controls[m - 1]

            let numSpans = m + 1
            outArr[0] <- controls[0]
            outArr[segments] <- controls[m - 1]
            for k = 1 to segments - 1 do
                let s = (float k / float segments) * float numSpans
                let span = min m (int (floor s))
                let u = s - float span
                let om = 1. - u
                let u2 = u * u
                let u3 = u2 * u
                let b0 = (om * om * om) / 6.
                let b1 = (3. * u3 - 6. * u2 + 4.) / 6.
                let b2 = (-3. * u3 + 3. * u2 + 3. * u + 1.) / 6.
                let b3 = u3 / 6.
                let c0, c1, c2, c3 = padded[span], padded[span + 1], padded[span + 2], padded[span + 3]
                outArr[k] <-
                    Point3.create
                        (b0 * c0.X + b1 * c1.X + b2 * c2.X + b3 * c3.X)
                        (b0 * c0.Y + b1 * c1.Y + b2 * c2.Y + b3 * c3.Y)
                        (b0 * c0.Z + b1 * c1.Z + b2 * c2.Z + b3 * c3.Z)

            let mutable wps = []
            for k = segments downto 0 do wps <- outArr[k] :: wps
            outArr, wps

    /// <summary>Bundles all non-loop associations in <c>layout</c> along 3D LCA paths in <c>hierarchy</c> with straightening strength <c>beta</c> in <c>[0, 1]</c> and <c>segments</c> spline subdivisions.</summary>
    let bundle
        (beta: float)
        (segments: int)
        (hierarchy: HierarchyNode<string>)
        (layout: DiagramLayout3D)
        : Result<DiagramLayout3D, LayoutError list> =
        if not (Double.IsFinite beta) || beta < 0. || beta > 1. then
            Error [ InvalidObservation "HierarchicalBundle3D requires finite beta in [0, 1]." ]
        elif segments < 1 || segments > 1024 then
            Error [ InvalidObservation "HierarchicalBundle3D requires segments in [1, 1024]." ]
        else
            // Index hierarchy and check uniqueness
            let ids = ResizeArray<string>()
            let parentIdx = ResizeArray<int>()
            let depths = ResizeArray<int>()
            let childLists = ResizeArray<ResizeArray<int>>()
            let idLookup = Dictionary<string, int>(StringComparer.Ordinal)
            let stack = Stack<HierarchyNode<string> * int * int>()
            stack.Push(hierarchy, -1, 0)
            let mutable duplicateId : string option = None

            while stack.Count > 0 && duplicateId.IsNone do
                let cur, pIdx, d = stack.Pop()
                let cid = Hierarchy.id cur
                if idLookup.ContainsKey cid then
                    duplicateId <- Some cid
                else
                    let myIdx = ids.Count
                    idLookup.Add(cid, myIdx)
                    ids.Add cid
                    parentIdx.Add pIdx
                    depths.Add d
                    childLists.Add(ResizeArray<int>())
                    if pIdx >= 0 then childLists[pIdx].Add myIdx
                    for c in List.rev (Hierarchy.children cur) do
                        stack.Push(c, myIdx, d + 1)

            match duplicateId with
            | Some _ -> Error [ InvalidObservation "Hierarchy identities must be unique." ]
            | None ->
                if layout.Associations.IsEmpty then
                    Ok layout
                else
                    let hCount = ids.Count
                    let children = Array.init hCount (fun i -> childLists[i].ToArray())

                    // Postorder traversal for synthesizing internal cluster 3D centers when not explicitly in layout.Nodes
                    let post = ResizeArray<int>(hCount)
                    let pStack = Stack<int * bool>()
                    pStack.Push(0, false)
                    while pStack.Count > 0 do
                        let u, visited = pStack.Pop()
                        if visited then post.Add u
                        else
                            pStack.Push(u, true)
                            let ch = children[u]
                            for k = ch.Length - 1 downto 0 do
                                pStack.Push(ch[k], false)

                    let nodePos = Dictionary<string, Point3>(StringComparer.Ordinal)
                    for n in layout.Nodes do
                        if not (nodePos.ContainsKey n.Id) then nodePos.Add(n.Id, n.Center)
                    let groupPos = Dictionary<string, Point3>(StringComparer.Ordinal)
                    for g in layout.Groups do
                        if not (groupPos.ContainsKey g.Id) then groupPos.Add(g.Id, Bounds3.center g.Bounds)

                    let hasPos = Array.zeroCreate<bool> hCount
                    let pos3D = Array.zeroCreate<Point3> hCount
                    let fallbackCenter = Bounds3.center layout.Bounds

                    for p = 0 to post.Count - 1 do
                        let u = post[p]
                        let uid = ids[u]
                        match nodePos.TryGetValue uid with
                        | true, pt ->
                            pos3D[u] <- pt
                            hasPos[u] <- true
                        | _ ->
                            let ch = children[u]
                            let mutable sx, sy, sz = 0., 0., 0.
                            let mutable cnt = 0
                            for k = 0 to ch.Length - 1 do
                                let c = ch[k]
                                if hasPos[c] then
                                    sx <- sx + pos3D[c].X
                                    sy <- sy + pos3D[c].Y
                                    sz <- sz + pos3D[c].Z
                                    cnt <- cnt + 1
                            if cnt > 0 then
                                let inv = 1. / float cnt
                                pos3D[u] <- Point3.create (sx * inv) (sy * inv) (sz * inv)
                                hasPos[u] <- true
                            else
                                match groupPos.TryGetValue uid with
                                | true, gp ->
                                    pos3D[u] <- gp
                                    hasPos[u] <- true
                                | _ ->
                                    pos3D[u] <- fallbackCenter

                    let missing = ResizeArray<LayoutError>()
                    let routedRev = ResizeArray<RoutedAssociation3D>(layout.Associations.Length)
                    let upU = ResizeArray<int>(32)
                    let upV = ResizeArray<int>(32)

                    for a in layout.Associations do
                        let sId = a.Source.NodeId
                        let tId = a.Target.NodeId
                        match idLookup.TryGetValue sId, idLookup.TryGetValue tId with
                        | (false, _), (false, _) ->
                            missing.Add(MissingEndpoint sId)
                            if sId <> tId then missing.Add(MissingEndpoint tId)
                        | (false, _), _ ->
                            missing.Add(MissingEndpoint sId)
                        | _, (false, _) ->
                            missing.Add(MissingEndpoint tId)
                        | (true, uIdx), (true, vIdx) ->
                            if uIdx = vIdx then
                                routedRev.Add a
                            else
                                upU.Clear()
                                upV.Clear()
                                let mutable cu = uIdx
                                let mutable cv = vIdx
                                upU.Add cu
                                upV.Add cv
                                while depths[cu] > depths[cv] do
                                    cu <- parentIdx[cu]
                                    upU.Add cu
                                while depths[cv] > depths[cu] do
                                    cv <- parentIdx[cv]
                                    upV.Add cv
                                while cu <> cv do
                                    cu <- parentIdx[cu]
                                    cv <- parentIdx[cv]
                                    upU.Add cu
                                    upV.Add cv

                                // upU ends at lca = cu, upV also ends at lca = cv; concatenate upU + reverse(upV without last element)
                                let chainLen = upU.Count + upV.Count - 1
                                let rawCtrl = Array.zeroCreate<Point3> chainLen
                                for i = 0 to upU.Count - 1 do
                                    rawCtrl[i] <- pos3D[upU[i]]
                                let mutable w = upU.Count
                                for j = upV.Count - 2 downto 0 do
                                    rawCtrl[w] <- pos3D[upV[j]]
                                    w <- w + 1

                                // Preserve exact association start/end anchors if already routed on boundaries/ports
                                if a.Points.Length >= 2 then
                                    rawCtrl[0] <- a.Points[0]
                                    rawCtrl[chainLen - 1] <- a.Points[a.Points.Length - 1]

                                // Holten beta straightening toward direct 3D chord
                                let startPt = rawCtrl[0]
                                let endPt = rawCtrl[chainLen - 1]
                                let straightened =
                                    Array.init chainLen (fun i ->
                                        if i = 0 then startPt
                                        elif i = chainLen - 1 then endPt
                                        else
                                            let t = float i / float (chainLen - 1)
                                            let chordPt = Point3.lerp startPt endPt t
                                            Point3.lerp chordPt rawCtrl[i] beta)

                                let ptsArr, wps = evaluateClampedBSpline3D straightened segments
                                routedRev.Add
                                    { a with
                                        Points = Point3Chain.ofOwnedArray ptsArr
                                        Waypoints = wps }

                    if missing.Count > 0 then
                        Error(List.ofSeq missing)
                    else
                        let routed = List.ofSeq routedRev
                        Ok
                            { layout with
                                Bounds = Kernels3D.computeBounds3D layout.Bounds layout.Nodes layout.Groups routed
                                Associations = routed }

module private KdeVolumetricKernels =
    /// Uniformly resamples an existing Point3Chain into `sampleCount` points by 3D arc length, keeping endpoints exact.
    let resampleChainUniform
        (chain: Point3Chain)
        (sampleCount: int)
        (outX: float[])
        (outY: float[])
        (outZ: float[])
        (offset: int)
        : unit =
        let srcLen = chain.Length
        if srcLen = 0 then
            for k = 0 to sampleCount - 1 do
                outX[offset + k] <- 0.; outY[offset + k] <- 0.; outZ[offset + k] <- 0.
        elif srcLen = 1 then
            let p = chain[0]
            for k = 0 to sampleCount - 1 do
                outX[offset + k] <- p.X; outY[offset + k] <- p.Y; outZ[offset + k] <- p.Z
        else
            let pStart = chain[0]
            let pEnd = chain[srcLen - 1]
            outX[offset] <- pStart.X; outY[offset] <- pStart.Y; outZ[offset] <- pStart.Z
            outX[offset + sampleCount - 1] <- pEnd.X
            outY[offset + sampleCount - 1] <- pEnd.Y
            outZ[offset + sampleCount - 1] <- pEnd.Z

            if srcLen = 2 then
                for k = 1 to sampleCount - 2 do
                    let t = float k / float (sampleCount - 1)
                    outX[offset + k] <- pStart.X + (pEnd.X - pStart.X) * t
                    outY[offset + k] <- pStart.Y + (pEnd.Y - pStart.Y) * t
                    outZ[offset + k] <- pStart.Z + (pEnd.Z - pStart.Z) * t
            else
                let cumLen = Array.zeroCreate<float> srcLen
                for i = 1 to srcLen - 1 do
                    cumLen[i] <- cumLen[i - 1] + Point3.distance chain[i - 1] chain[i]
                let total = cumLen[srcLen - 1]
                if total <= 1e-12 then
                    for k = 1 to sampleCount - 2 do
                        let t = float k / float (sampleCount - 1)
                        outX[offset + k] <- pStart.X + (pEnd.X - pStart.X) * t
                        outY[offset + k] <- pStart.Y + (pEnd.Y - pStart.Y) * t
                        outZ[offset + k] <- pStart.Z + (pEnd.Z - pStart.Z) * t
                else
                    let mutable seg = 0
                    for k = 1 to sampleCount - 2 do
                        let target = (float k / float (sampleCount - 1)) * total
                        while seg < srcLen - 2 && cumLen[seg + 1] < target do
                            seg <- seg + 1
                        let l0 = cumLen[seg]
                        let segLen = cumLen[seg + 1] - l0
                        let lt = if segLen <= 1e-12 then 0. else max 0. (min 1. ((target - l0) / segLen))
                        let a, b = chain[seg], chain[seg + 1]
                        outX[offset + k] <- a.X + (b.X - a.X) * lt
                        outY[offset + k] <- a.Y + (b.Y - a.Y) * lt
                        outZ[offset + k] <- a.Z + (b.Z - a.Z) * lt

    /// Re-spaces interior SoA polyline points [offset .. offset + s - 1] uniformly by arc length while keeping endpoints pinned.
    let reparameterizeEdgeInPlace
        (px: float[])
        (py: float[])
        (pz: float[])
        (offset: int)
        (s: int)
        (cumBuf: float[])
        (tmpX: float[])
        (tmpY: float[])
        (tmpZ: float[])
        : unit =
        cumBuf[0] <- 0.
        for k = 1 to s - 1 do
            let dx = px[offset + k] - px[offset + k - 1]
            let dy = py[offset + k] - py[offset + k - 1]
            let dz = pz[offset + k] - pz[offset + k - 1]
            cumBuf[k] <- cumBuf[k - 1] + sqrt (dx * dx + dy * dy + dz * dz)
        let total = cumBuf[s - 1]
        if total > 1e-12 then
            let mutable seg = 0
            for k = 1 to s - 2 do
                let target = (float k / float (s - 1)) * total
                while seg < s - 2 && cumBuf[seg + 1] < target do
                    seg <- seg + 1
                let l0 = cumBuf[seg]
                let dl = cumBuf[seg + 1] - l0
                let lt = if dl <= 1e-12 then 0. else max 0. (min 1. ((target - l0) / dl))
                let i0 = offset + seg
                let i1 = i0 + 1
                tmpX[k] <- px[i0] + (px[i1] - px[i0]) * lt
                tmpY[k] <- py[i0] + (py[i1] - py[i0]) * lt
                tmpZ[k] <- pz[i0] + (pz[i1] - pz[i0]) * lt
            for k = 1 to s - 2 do
                px[offset + k] <- tmpX[k]
                py[offset + k] <- tmpY[k]
                pz[offset + k] <- tmpZ[k]

    let boxBlurX (g: int) (rad: int) (src: float[]) (dst: float[]) : unit =
        let invW = 1. / float (2 * rad + 1)
        let rClamp = min (g - 1) rad
        for z = 0 to g - 1 do
            for y = 0 to g - 1 do
                let rowOff = (z * g + y) * g
                let mutable acc = 0.
                for x = 0 to rClamp do
                    acc <- acc + src[rowOff + x]
                for x = 0 to g - 1 do
                    dst[rowOff + x] <- max 0. (acc * invW)
                    let addIdx = x + 1 + rad
                    let subIdx = x - rad
                    if addIdx < g then acc <- acc + src[rowOff + addIdx]
                    if subIdx >= 0 then acc <- acc - src[rowOff + subIdx]

    let boxBlurY (g: int) (rad: int) (src: float[]) (dst: float[]) : unit =
        let invW = 1. / float (2 * rad + 1)
        let rClamp = min (g - 1) rad
        for z = 0 to g - 1 do
            let zBase = z * g * g
            for x = 0 to g - 1 do
                let colOff = zBase + x
                let mutable acc = 0.
                for y = 0 to rClamp do
                    acc <- acc + src[colOff + y * g]
                for y = 0 to g - 1 do
                    dst[colOff + y * g] <- max 0. (acc * invW)
                    let addIdx = y + 1 + rad
                    let subIdx = y - rad
                    if addIdx < g then acc <- acc + src[colOff + addIdx * g]
                    if subIdx >= 0 then acc <- acc - src[colOff + subIdx * g]

    let boxBlurZ (g: int) (rad: int) (src: float[]) (dst: float[]) : unit =
        let invW = 1. / float (2 * rad + 1)
        let rClamp = min (g - 1) rad
        let g2 = g * g
        for y = 0 to g - 1 do
            let yBase = y * g
            for x = 0 to g - 1 do
                let pilOff = yBase + x
                let mutable acc = 0.
                for z = 0 to rClamp do
                    acc <- acc + src[pilOff + z * g2]
                for z = 0 to g - 1 do
                    dst[pilOff + z * g2] <- max 0. (acc * invW)
                    let addIdx = z + 1 + rad
                    let subIdx = z - rad
                    if addIdx < g then acc <- acc + src[pilOff + addIdx * g2]
                    if subIdx >= 0 then acc <- acc - src[pilOff + subIdx * g2]

/// <summary>Linear-time <c>O(I(ES + G^3))</c> volumetric 3D Kernel Density Estimation edge bundler (Hurter et al. 2012; van der Zwan et al. 2016 <c>CUBu</c>).</summary>
[<RequireQualifiedAccess>]
module KdeBundle3D =
    /// <summary>Bundles 3D associations in <c>layout</c> by splatting polylines into a <c>G x G x G</c> density volume, smoothing with separable 3D kernels and advecting interior waypoints along <c>grad(rho)</c> with cooperative cancellation.</summary>
    let bundleWithCancellation
        (token: System.Threading.CancellationToken)
        (config: KdeBundle3DConfig)
        (layout: DiagramLayout3D)
        : Result<DiagramLayout3D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if config.GridSize < 8 || config.GridSize > 160
           || config.Iterations < 1 || config.Iterations > 128
           || config.SamplePointsPerEdge < 3 || config.SamplePointsPerEdge > 256
           || not (Double.IsFinite config.InitialKernelRadius && Double.IsFinite config.KernelDecay && Double.IsFinite config.StepSize)
           || config.InitialKernelRadius <= 0.
           || config.KernelDecay <= 0. || config.KernelDecay > 1.
           || config.StepSize <= 0. then
            Error [ InvalidObservation "KdeBundle3D requires GridSize in [8, 160], Iterations in [1, 128], SamplePointsPerEdge in [3, 256], positive InitialKernelRadius/StepSize and KernelDecay in (0, 1]." ]
        elif layout.Associations.IsEmpty then
            Ok layout
        else
            let assocs = List.toArray layout.Associations
            let eCount = assocs.Length
            let s = config.SamplePointsPerEdge

            let isActive = Array.zeroCreate<bool> eCount
            let mutable activeEdges = 0
            for e = 0 to eCount - 1 do
                let a = assocs[e]
                let len = a.Points.Length
                if len >= 2 then
                    let p0 = a.Points[0]
                    let p1 = a.Points[len - 1]
                    if Point3.distance p0 p1 > 1e-6 then
                        isActive[e] <- true
                        activeEdges <- activeEdges + 1

            if activeEdges = 0 then
                Ok layout
            else
                let totalSamples = eCount * s
                let px = Array.zeroCreate<float> totalSamples
                let py = Array.zeroCreate<float> totalSamples
                let pz = Array.zeroCreate<float> totalSamples

                let mutable minX, minY, minZ = infinity, infinity, infinity
                let mutable maxX, maxY, maxZ = -infinity, -infinity, -infinity

                for e = 0 to eCount - 1 do
                    if isActive[e] then
                        let off = e * s
                        KdeVolumetricKernels.resampleChainUniform assocs[e].Points s px py pz off
                        for k = 0 to s - 1 do
                            let x, y, z = px[off + k], py[off + k], pz[off + k]
                            if x < minX then minX <- x
                            if y < minY then minY <- y
                            if z < minZ then minZ <- z
                            if x > maxX then maxX <- x
                            if y > maxY then maxY <- y
                            if z > maxZ then maxZ <- z

                let g = config.GridSize
                let g2 = g * g
                let g3 = g2 * g
                let vol = Array.zeroCreate<float> g3
                let tmp = Array.zeroCreate<float> g3

                let rawSpan = max 1e-3 (max (maxX - minX) (max (maxY - minY) (maxZ - minZ)))
                let paddedSpan = rawSpan * 1.18
                let cellSize = paddedSpan / float (g - 4)
                let invCell = 1. / cellSize
                let midX, midY, midZ = 0.5 * (minX + maxX), 0.5 * (minY + maxY), 0.5 * (minZ + maxZ)
                let halfGrid = 0.5 * float (g - 1)
                let gridMaxCoord = float (g - 3) - 1e-6

                let inline toGridX (wx: float) = max 1. (min gridMaxCoord (halfGrid + (wx - midX) * invCell))
                let inline toGridY (wy: float) = max 1. (min gridMaxCoord (halfGrid + (wy - midY) * invCell))
                let inline toGridZ (wz: float) = max 1. (min gridMaxCoord (halfGrid + (wz - midZ) * invCell))

                let inline splatTrilinear (wx: float) (wy: float) (wz: float) (weight: float) =
                    let gx = toGridX wx
                    let gy = toGridY wy
                    let gz = toGridZ wz
                    let ix = int (floor gx)
                    let iy = int (floor gy)
                    let iz = int (floor gz)
                    let fx = gx - float ix
                    let fy = gy - float iy
                    let fz = gz - float iz
                    let ox, oy, oz = 1. - fx, 1. - fy, 1. - fz
                    let b000 = (iz * g + iy) * g + ix
                    let b001 = b000 + 1
                    let b010 = b000 + g
                    let b011 = b010 + 1
                    let b100 = b000 + g2
                    let b101 = b100 + 1
                    let b110 = b100 + g
                    let b111 = b110 + 1
                    vol[b000] <- vol[b000] + weight * ox * oy * oz
                    vol[b001] <- vol[b001] + weight * fx * oy * oz
                    vol[b010] <- vol[b010] + weight * ox * fy * oz
                    vol[b011] <- vol[b011] + weight * fx * fy * oz
                    vol[b100] <- vol[b100] + weight * ox * oy * fz
                    vol[b101] <- vol[b101] + weight * fx * oy * fz
                    vol[b110] <- vol[b110] + weight * ox * fy * fz
                    vol[b111] <- vol[b111] + weight * fx * fy * fz

                let lapX = Array.zeroCreate<float> s
                let lapY = Array.zeroCreate<float> s
                let lapZ = Array.zeroCreate<float> s
                let cumBuf = Array.zeroCreate<float> s

                for iter = 0 to config.Iterations - 1 do
                    token.ThrowIfCancellationRequested()
                    Array.fill vol 0 g3 0.

                    // 1. Splat interior edge samples + segment midpoints into 3D voxel grid
                    for e = 0 to eCount - 1 do
                        if isActive[e] then
                            Kernels3D.check token e
                            let off = e * s
                            for k = 1 to s - 2 do
                                let xk, yk, zk = px[off + k], py[off + k], pz[off + k]
                                splatTrilinear xk yk zk 1.
                                let mx = 0.5 * (xk + px[off + k - 1])
                                let my = 0.5 * (yk + py[off + k - 1])
                                let mz = 0.5 * (zk + pz[off + k - 1])
                                splatTrilinear mx my mz 0.5
                            let mxEnd = 0.5 * (px[off + s - 2] + px[off + s - 1])
                            let myEnd = 0.5 * (py[off + s - 2] + py[off + s - 1])
                            let mzEnd = 0.5 * (pz[off + s - 2] + pz[off + s - 1])
                            splatTrilinear mxEnd myEnd mzEnd 0.5

                    // 2. Two-pass 3-axis separable box-filter smoothing (vol -> tmp -> vol -> tmp -> vol -> tmp -> vol)
                    let rFloat = config.InitialKernelRadius * Math.Pow(config.KernelDecay, float iter)
                    let rad = max 1 (min (g / 3) (int (Math.Round rFloat)))
                    KdeVolumetricKernels.boxBlurX g rad vol tmp
                    KdeVolumetricKernels.boxBlurY g rad tmp vol
                    KdeVolumetricKernels.boxBlurZ g rad vol tmp
                    KdeVolumetricKernels.boxBlurX g rad tmp vol
                    KdeVolumetricKernels.boxBlurY g rad vol tmp
                    KdeVolumetricKernels.boxBlurZ g rad tmp vol

                    // Peak density scale for smooth stationary-ridge regularization eps
                    let mutable maxRho = 0.
                    for idx = 0 to g3 - 1 do
                        if vol[idx] > maxRho then maxRho <- vol[idx]
                    let ridgeEps = max 1e-12 (0.06 * maxRho / float (max 1 rad))

                    // 3. Trilinearly interpolated 3D central-difference gradient + normalized advection
                    let worldStep = config.StepSize * cellSize * Math.Pow(config.KernelDecay, float iter)
                    for e = 0 to eCount - 1 do
                        if isActive[e] then
                            Kernels3D.check token e
                            let off = e * s
                            for k = 1 to s - 2 do
                                let gx = toGridX px[off + k]
                                let gy = toGridY py[off + k]
                                let gz = toGridZ pz[off + k]
                                let ix = int (floor gx)
                                let iy = int (floor gy)
                                let iz = int (floor gz)
                                let fx = gx - float ix
                                let fy = gy - float iy
                                let fz = gz - float iz
                                let ox, oy, oz = 1. - fx, 1. - fy, 1. - fz

                                let mutable gradX, gradY, gradZ = 0., 0., 0.
                                for dz = 0 to 1 do
                                    let wz = if dz = 0 then oz else fz
                                    let cz = iz + dz
                                    for dy = 0 to 1 do
                                        let wyz = wz * (if dy = 0 then oy else fy)
                                        let cy = iy + dy
                                        let rowBase = (cz * g + cy) * g + ix
                                        for dx = 0 to 1 do
                                            let w = wyz * (if dx = 0 then ox else fx)
                                            let cIdx = rowBase + dx
                                            gradX <- gradX + w * 0.5 * (vol[cIdx + 1] - vol[cIdx - 1])
                                            gradY <- gradY + w * 0.5 * (vol[cIdx + g] - vol[cIdx - g])
                                            gradZ <- gradZ + w * 0.5 * (vol[cIdx + g2] - vol[cIdx - g2])

                                let gNorm = sqrt (gradX * gradX + gradY * gradY + gradZ * gradZ)
                                let denom = gNorm + ridgeEps
                                // Smooth endpoint taper preserving tangent continuity near pinned anchors
                                let taper = sin (Math.PI * float k / float (s - 1))
                                let effStep = worldStep * Math.Pow(max 0. taper, 0.35)
                                px[off + k] <- px[off + k] + effStep * (gradX / denom)
                                py[off + k] <- py[off + k] + effStep * (gradY / denom)
                                pz[off + k] <- pz[off + k] + effStep * (gradZ / denom)

                            // 4. Two-pass 3D Laplacian polyline smoothing keeping endpoints 0 and s - 1 strictly pinned
                            let lambda = 0.32
                            let keepW = 1. - lambda
                            let nbrW = 0.5 * lambda
                            for _ = 0 to 1 do
                                for k = 1 to s - 2 do
                                    lapX[k] <- keepW * px[off + k] + nbrW * (px[off + k - 1] + px[off + k + 1])
                                    lapY[k] <- keepW * py[off + k] + nbrW * (py[off + k - 1] + py[off + k + 1])
                                    lapZ[k] <- keepW * pz[off + k] + nbrW * (pz[off + k - 1] + pz[off + k + 1])
                                for k = 1 to s - 2 do
                                    px[off + k] <- lapX[k]
                                    py[off + k] <- lapY[k]
                                    pz[off + k] <- lapZ[k]

                            KdeVolumetricKernels.reparameterizeEdgeInPlace px py pz off s cumBuf lapX lapY lapZ

                let mutable routed = []
                for e = eCount - 1 downto 0 do
                    let a = assocs[e]
                    if not isActive[e] then
                        routed <- a :: routed
                    else
                        let off = e * s
                        let ptsArr = Array.init s (fun k -> Point3.create px[off + k] py[off + k] pz[off + k])
                        let mutable wps = []
                        for k = s - 1 downto 0 do
                            wps <- ptsArr[k] :: wps
                        routed <-
                            { a with
                                Points = Point3Chain.ofOwnedArray ptsArr
                                Waypoints = wps } :: routed

                Ok
                    { layout with
                        Bounds = Kernels3D.computeBounds3D layout.Bounds layout.Nodes layout.Groups routed
                        Associations = routed }

    /// <summary>Bundles 3D associations in <c>layout</c> via volumetric 3D KDE advection without cancellation.</summary>
    let bundle (config: KdeBundle3DConfig) (layout: DiagramLayout3D) : Result<DiagramLayout3D, LayoutError list> =
        bundleWithCancellation System.Threading.CancellationToken.None config layout

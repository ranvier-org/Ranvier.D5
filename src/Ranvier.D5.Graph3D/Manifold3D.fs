namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>An observation embedded at a finite 3D <c>Point3</c> world coordinate.</summary>
type EmbeddedPoint3D<'Id> =
    { /// <summary>Original observation identifier.</summary>
      Id: 'Id
      /// <summary>3D embedded coordinate inside the target cuboid.</summary>
      Position: Point3 }

module private Manifold3DFit =
    let fitPoints3D
        (bounds: Bounds3)
        (ids: 'Id[])
        (xs: float[])
        (ys: float[])
        (zs: float[])
        : EmbeddedPoint3D<'Id> list =
        let n = ids.Length
        if n = 0 then []
        else
            let x0, y0, z0 = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
            let sx, sy, sz = Bounds3.sizeX bounds, Bounds3.sizeY bounds, Bounds3.sizeZ bounds
            let x1, y1, z1 = Bounds3.maxX bounds, Bounds3.maxY bounds, Bounds3.maxZ bounds
            let c = Bounds3.center bounds
            if n = 1 then
                [ { Id = ids[0]; Position = c } ]
            else
                let mutable minX, maxX = infinity, -infinity
                let mutable minY, maxY = infinity, -infinity
                let mutable minZ, maxZ = infinity, -infinity
                for i = 0 to n - 1 do
                    let x, y, z = xs[i], ys[i], zs[i]
                    if x < minX then minX <- x
                    if x > maxX then maxX <- x
                    if y < minY then minY <- y
                    if y > maxY then maxY <- y
                    if z < minZ then minZ <- z
                    if z > maxZ then maxZ <- z

                let spanX = maxX - minX
                let spanY = maxY - minY
                let spanZ = maxZ - minZ
                let midX = 0.5 * (minX + maxX)
                let midY = 0.5 * (minY + maxY)
                let midZ = 0.5 * (minZ + maxZ)

                let mutable scale = infinity
                if spanX > 1e-12 && sx > 0.0 then scale <- min scale ((sx * 0.90) / spanX)
                if spanY > 1e-12 && sy > 0.0 then scale <- min scale ((sy * 0.90) / spanY)
                if spanZ > 1e-12 && sz > 0.0 then scale <- min scale ((sz * 0.90) / spanZ)
                if Double.IsPositiveInfinity scale then scale <- 0.0

                [ for i in 0 .. n - 1 ->
                    let px = if sx <= 0.0 then x0 else max x0 (min x1 (c.X + (xs[i] - midX) * scale))
                    let py = if sy <= 0.0 then y0 else max y0 (min y1 (c.Y + (ys[i] - midY) * scale))
                    let pz = if sz <= 0.0 then z0 else max z0 (min z1 (c.Z + (zs[i] - midZ) * scale))
                    { Id = ids[i]; Position = Point3.create px py pz } ]

/// <summary>Hyperparameters for McInnes, Healy &amp; Melville (2018) 3D Uniform Manifold Approximation and Projection (<c>Umap3D</c>).</summary>
type Umap3DConfig =
    { /// <summary>Target 3D bounding cuboid into which embedded points are scaled.</summary>
      Bounds: Bounds3
      /// <summary>High-dimensional dissimilarity metric.</summary>
      Metric: DistanceMetric
      /// <summary>Local neighborhood size <c>k</c> (<c>&gt;= 2</c>; default 15).</summary>
      Neighbors: int
      /// <summary>Minimum separation parameter controlling cluster tightness (<c>&gt;= 0</c>; default 0.1).</summary>
      MinDist: float
      /// <summary>Effective global scale of embedded points (<c>&gt; 0</c>; default 1.0).</summary>
      Spread: float
      /// <summary>Negative-sampling SGD optimization epochs (<c>1..10000</c>; default 200).</summary>
      Epochs: int
      /// <summary>Number of repulsive negative samples per positive edge sample (default 5).</summary>
      NegativeSampleRate: int
      /// <summary>Initial SGD learning rate before linear annealing (<c>&gt; 0</c>; default 1.0).</summary>
      LearningRate: float
      /// <summary>Deterministic PRNG seed (default 42).</summary>
      Seed: uint32 }

/// <summary>Constructors for <c>Umap3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Umap3DConfig =
    /// <summary>Creates a 3D UMAP configuration with 15 neighbors, min-dist 0.1, spread 1.0, 200 epochs, negative sample rate 5, learning rate 1.0 and seed 42.</summary>
    let create (bounds: Bounds3) : Umap3DConfig =
        { Bounds = bounds
          Metric = DistanceMetric.Euclidean
          Neighbors = 15
          MinDist = 0.1
          Spread = 1.0
          Epochs = 200
          NegativeSampleRate = 5
          LearningRate = 1.0
          Seed = 42u }

/// <summary>McInnes, Healy &amp; Melville (2018) 3D Uniform Manifold Approximation and Projection (<c>Umap3D</c>) and <c>DiagramLayout3D</c> skeleton conversion.</summary>
[<RequireQualifiedAccess>]
module Umap3D =
    let private validateConfig (config: Umap3DConfig) =
        if config.Neighbors < 2
           || not (Double.IsFinite config.MinDist && Double.IsFinite config.Spread && Double.IsFinite config.LearningRate)
           || config.MinDist < 0.0 || config.Spread <= 0.0 || config.LearningRate <= 0.0
           || config.Epochs < 1 || config.Epochs > 10000
           || config.NegativeSampleRate < 0 || config.NegativeSampleRate > 256 then
            Error [ InvalidObservation "Umap3D requires Neighbors >= 2, MinDist >= 0, Spread > 0, LearningRate > 0, Epochs in [1,10000] and NegativeSampleRate in [0,256]." ]
        else
            Ok ()

    /// <summary>Computes a deterministic 3D UMAP embedding over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (config: Umap3DConfig)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        match validateConfig config with
        | Error e -> Error e
        | Ok () ->
            let ids, dim, data = FeatureDataset.raw dataset
            let coords =
                ManifoldCore.embedUmapCore
                    token
                    3
                    config.Metric
                    config.Neighbors
                    config.MinDist
                    config.Spread
                    config.Epochs
                    config.NegativeSampleRate
                    config.LearningRate
                    config.Seed
                    ids.Length
                    dim
                    data
            Ok(Manifold3DFit.fitPoints3D config.Bounds ids coords[0] coords[1] coords[2])

    /// <summary>Computes a deterministic 3D UMAP embedding over <c>dataset</c> without cancellation.</summary>
    let embed (config: Umap3DConfig) (dataset: FeatureDataset<'Id>) : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None config dataset

    /// <summary>Computes a deterministic 3D UMAP embedding together with the underlying directed k-NN <c>NeighborLink</c> list in a single pass, with cooperative cancellation.</summary>
    let embedWithLinksWithCancellation
        (token: System.Threading.CancellationToken)
        (config: Umap3DConfig)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint3D<'Id> list * NeighborLink<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        match validateConfig config with
        | Error e -> Error e
        | Ok () ->
            let ids, dim, data = FeatureDataset.raw dataset
            let coords, kEff, knnIdx, knnDist =
                ManifoldCore.embedUmapCoreWithKnn
                    token
                    3
                    config.Metric
                    config.Neighbors
                    config.MinDist
                    config.Spread
                    config.Epochs
                    config.NegativeSampleRate
                    config.LearningRate
                    config.Seed
                    ids.Length
                    dim
                    data
            let pts = Manifold3DFit.fitPoints3D config.Bounds ids coords[0] coords[1] coords[2]
            let links = NnDescent.linksFromIndexedKnn ids kEff knnIdx knnDist
            Ok(pts, links)

    /// <summary>Computes a deterministic 3D UMAP embedding alongside its directed k-NN <c>NeighborLink</c> list without recomputing the k-NN graph.</summary>
    let embedWithLinks (config: Umap3DConfig) (dataset: FeatureDataset<'Id>) : Result<EmbeddedPoint3D<'Id> list * NeighborLink<'Id> list, LayoutError list> =
        embedWithLinksWithCancellation System.Threading.CancellationToken.None config dataset

    /// <summary>Converts a 3D point cloud and k-NN <c>NeighborLink&lt;string&gt;</c> skeleton into an immutable <c>DiagramLayout3D</c> ready for <c>Scene3D</c> or <c>Ranvier.D5.Wgpu</c>.</summary>
    let toDiagramLayout
        (nodeRadius: float)
        (points: EmbeddedPoint3D<string> list)
        (links: NeighborLink<string> list)
        : DiagramLayout3D =
        if not (Double.IsFinite nodeRadius) || nodeRadius <= 0.0 then
            invalidArg "nodeRadius" "Node radius must be positive and finite."

        let posMap = Dictionary<string, Point3>(StringComparer.Ordinal)
        let uniquePts = ResizeArray<EmbeddedPoint3D<string>>()
        for p in points do
            if not (isNull p.Id) && not (posMap.ContainsKey p.Id) then
                Point3.create p.Position.X p.Position.Y p.Position.Z |> ignore
                posMap.Add(p.Id, p.Position)
                uniquePts.Add p

        let sortedPts = uniquePts.ToArray()
        Array.Sort(sortedPts, Comparer<EmbeddedPoint3D<string>>.Create(fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id)))

        let diam = nodeRadius * 2.0
        let mutable posNodes = []
        for i = sortedPts.Length - 1 downto 0 do
            let p = sortedPts[i]
            let box = Kernels3D.nodeBox p.Position diam diam diam
            posNodes <-
                { Id = p.Id
                  Kind = NodeKind.Standard
                  Center = p.Position
                  Bounds = box
                  Radius = nodeRadius
                  ScaleFactor = 1.0
                  ParentGroup = None
                  Layer = None } :: posNodes

        // Classify mutual k-NN & 1-NN backbone edges and deduplicate symmetric undirected skeleton segments
        let nearestDist = Dictionary<string, float>(StringComparer.Ordinal)
        let nearestPeer = Dictionary<string, string>(StringComparer.Ordinal)
        let mutable directedSet = Set.empty<struct(string * string)>

        for l in links do
            if not (isNull l.Source || isNull l.Target)
               && l.Source <> l.Target
               && posMap.ContainsKey l.Source
               && posMap.ContainsKey l.Target
               && Double.IsFinite l.Distance then
                directedSet <- Set.add (struct(l.Source, l.Target)) directedSet
                match nearestDist.TryGetValue l.Source with
                | true, prevD ->
                    if l.Distance < prevD
                       || (l.Distance = prevD && StringComparer.Ordinal.Compare(l.Target, nearestPeer[l.Source]) < 0) then
                        nearestDist[l.Source] <- l.Distance
                        nearestPeer[l.Source] <- l.Target
                | _ ->
                    nearestDist.Add(l.Source, l.Distance)
                    nearestPeer.Add(l.Source, l.Target)

        let edgeMap = Dictionary<string, RoutedAssociation3D>(StringComparer.Ordinal)
        for l in links do
            if not (isNull l.Source || isNull l.Target)
               && l.Source <> l.Target
               && posMap.ContainsKey l.Source
               && posMap.ContainsKey l.Target
               && Double.IsFinite l.Distance then
                let lo, hi =
                    if StringComparer.Ordinal.Compare(l.Source, l.Target) <= 0 then l.Source, l.Target
                    else l.Target, l.Source
                let edgeId = "knn:" + lo + "--" + hi
                if not (edgeMap.ContainsKey edgeId) then
                    let isMutual =
                        Set.contains (struct(lo, hi)) directedSet && Set.contains (struct(hi, lo)) directedSet
                    let isPrimary =
                        (match nearestPeer.TryGetValue lo with true, p -> p = hi | _ -> false)
                        || (match nearestPeer.TryGetValue hi with true, p -> p = lo | _ -> false)
                    let pLo = posMap[lo]
                    let pHi = posMap[hi]
                    edgeMap.Add(
                        edgeId,
                        { Id = edgeId
                          Source = Endpoint.node lo
                          Target = Endpoint.node hi
                          Kind = AssociationKind.Undirected
                          IsBackbone = isMutual || isPrimary
                          Points = Point3Chain.ofOwnedArray [| pLo; pHi |]
                          Waypoints = [ pLo; pHi ] }
                    )

        let routedArr = edgeMap.Values |> Seq.toArray
        Array.Sort(routedArr, Comparer<RoutedAssociation3D>.Create(fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id)))
        let mutable routed = []
        for i = routedArr.Length - 1 downto 0 do
            routed <- routedArr[i] :: routed

        let fallback = Bounds3.create 0.0 0.0 0.0 0.0 0.0 0.0
        { Bounds = Kernels3D.computeBounds3D fallback posNodes [] routed
          Nodes = posNodes
          Groups = []
          Associations = routed }

/// <summary>Hyperparameters for van der Maaten (2014) Octree Barnes-Hut 3D t-Distributed Stochastic Neighbor Embedding (<c>Tsne3D</c>).</summary>
type Tsne3DConfig =
    { /// <summary>Target 3D bounding cuboid into which embedded points are scaled.</summary>
      Bounds: Bounds3
      /// <summary>High-dimensional dissimilarity metric.</summary>
      Metric: DistanceMetric
      /// <summary>Target Shannon perplexity of conditional Gaussian neighborhoods (<c>&gt; 0</c>; default 30.0).</summary>
      Perplexity: float
      /// <summary>Gradient descent iteration count (<c>1..10000</c>; default 250).</summary>
      Iterations: int
      /// <summary>3D Octree Barnes-Hut opening threshold theta (<c>0</c> for exact <c>O(N^2)</c>, default 0.5).</summary>
      Theta: float
      /// <summary>Gradient descent base learning rate (<c>&gt; 0</c>; default 200.0).</summary>
      LearningRate: float
      /// <summary>Attractive affinity multiplier applied during the initial early-exaggeration phase (<c>&gt;= 1</c>; default 4.0).</summary>
      EarlyExaggeration: float
      /// <summary>Deterministic PRNG seed (default 42).</summary>
      Seed: uint32 }

/// <summary>Constructors for <c>Tsne3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Tsne3DConfig =
    /// <summary>Creates a 3D Octree Barnes-Hut t-SNE configuration with perplexity 30.0, 250 iterations, theta 0.5, learning rate 200.0, early exaggeration 4.0 and seed 42.</summary>
    let create (bounds: Bounds3) : Tsne3DConfig =
        { Bounds = bounds
          Metric = DistanceMetric.Euclidean
          Perplexity = 30.0
          Iterations = 250
          Theta = 0.5
          LearningRate = 200.0
          EarlyExaggeration = 4.0
          Seed = 42u }

/// <summary>van der Maaten (2014) <c>O(N log N)</c> 3D Octree Barnes-Hut t-Distributed Stochastic Neighbor Embedding (<c>Tsne3D</c>).</summary>
[<RequireQualifiedAccess>]
module Tsne3D =
    /// <summary>Computes a deterministic 3D Octree Barnes-Hut t-SNE embedding over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (config: Tsne3DConfig)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.Perplexity && Double.IsFinite config.Theta
                && Double.IsFinite config.LearningRate && Double.IsFinite config.EarlyExaggeration)
           || config.Perplexity <= 0.0 || config.Theta < 0.0 || config.Theta > 2.0
           || config.LearningRate <= 0.0 || config.EarlyExaggeration < 1.0
           || config.Iterations < 1 || config.Iterations > 10000 then
            Error [ InvalidObservation "Tsne3D requires Perplexity > 0, Theta in [0,2], LearningRate > 0, EarlyExaggeration >= 1 and Iterations in [1,10000]." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let n = ids.Length
            if n <= 1 then
                let z = Array.zeroCreate<float> n
                Ok(Manifold3DFit.fitPoints3D config.Bounds ids z z z)
            else
                let kEff = min (n - 1) (max 3 (int (floor (3.0 * config.Perplexity))))
                let knnIdx, knnDist = NnDescent.buildIndexedKnn token config.Metric kEff config.Seed n dim data
                let pOffsets, pTargets, pWeights =
                    ManifoldCore.buildTsneSymmetrizedP token n kEff config.Perplexity knnIdx knnDist
                let init = ManifoldCore.initTsnePca token 3 n dim data config.Seed
                let xs = init[0]
                let ys = init[1]
                let zs = init[2]

                let attrX = Array.zeroCreate<float> n
                let attrY = Array.zeroCreate<float> n
                let attrZ = Array.zeroCreate<float> n
                let repX = Array.zeroCreate<float> n
                let repY = Array.zeroCreate<float> n
                let repZ = Array.zeroCreate<float> n
                let velX = Array.zeroCreate<float> n
                let velY = Array.zeroCreate<float> n
                let velZ = Array.zeroCreate<float> n
                let gainX = Array.create n 1.0
                let gainY = Array.create n 1.0
                let gainZ = Array.create n 1.0
                let useBh = n > 32 && config.Theta > 0.0
                let octWs = if useBh then Octree.createWorkspace n else Unchecked.defaultof<_>
                let unitMass = if useBh then Array.create n 1.0 else [||]
                let thetaSq = config.Theta * config.Theta
                let stopLyingIter = min 250 (config.Iterations / 4)
                let lr = config.LearningRate

                for iter = 0 to config.Iterations - 1 do
                    token.ThrowIfCancellationRequested()
                    let exag = if iter < stopLyingIter then config.EarlyExaggeration else 1.0
                    let momentum = if iter < stopLyingIter then 0.5 else 0.8

                    // 1. Exact sparse 3D Student-t attractive forces over symmetrized CSR P
                    for i = 0 to n - 1 do
                        let xi, yi, zi = xs[i], ys[i], zs[i]
                        let mutable ax, ay, az = 0.0, 0.0, 0.0
                        for idx = pOffsets[i] to pOffsets[i + 1] - 1 do
                            let j = pTargets[idx]
                            let dx = xi - xs[j]
                            let dy = yi - ys[j]
                            let dz = zi - zs[j]
                            let w = 1.0 / (1.0 + dx * dx + dy * dy + dz * dz)
                            let s = (pWeights[idx] * exag) * w
                            ax <- ax + s * dx
                            ay <- ay + s * dy
                            az <- az + s * dz
                        attrX[i] <- ax
                        attrY[i] <- ay
                        attrZ[i] <- az

                    // 2. Repulsive 3D Student-t forces and partition sum Z via O(N log N) BhOctreeWorkspace
                    let mutable zSum = 0.0
                    if not useBh then
                        Array.fill repX 0 n 0.0
                        Array.fill repY 0 n 0.0
                        Array.fill repZ 0 n 0.0
                        for i = 0 to n - 2 do
                            let xi, yi, zi = xs[i], ys[i], zs[i]
                            for j = i + 1 to n - 1 do
                                let dx = xi - xs[j]
                                let dy = yi - ys[j]
                                let dz = zi - zs[j]
                                let w = 1.0 / (1.0 + dx * dx + dy * dy + dz * dz)
                                zSum <- zSum + 2.0 * w
                                let w2 = w * w
                                let rx, ry, rz = w2 * dx, w2 * dy, w2 * dz
                                repX[i] <- repX[i] + rx
                                repY[i] <- repY[i] + ry
                                repZ[i] <- repZ[i] + rz
                                repX[j] <- repX[j] - rx
                                repY[j] <- repY[j] - ry
                                repZ[j] <- repZ[j] - rz
                    else
                        Octree.rebuildInPlace octWs token xs ys zs unitMass n false
                        let octNodes = octWs.Nodes
                        let perm = octWs.Perm
                        let stack = octWs.Stack
                        for i = 0 to n - 1 do
                            Kernels3D.check token i
                            let xi, yi, zi = xs[i], ys[i], zs[i]
                            let mutable rx, ry, rz = 0.0, 0.0, 0.0
                            let mutable zLocal = 0.0
                            stack[0] <- 0
                            let mutable sp = 1
                            while sp > 0 do
                                sp <- sp - 1
                                let nd = octNodes[stack[sp]]
                                let dx = xi - nd.Cx
                                let dy = yi - nd.Cy
                                let dz = zi - nd.Cz
                                let dSq = dx * dx + dy * dy + dz * dz
                                let sz = max (nd.MaxX - nd.MinX) (max (nd.MaxY - nd.MinY) (nd.MaxZ - nd.MinZ))
                                let contains =
                                    xi >= nd.MinX && xi <= nd.MaxX
                                    && yi >= nd.MinY && yi <= nd.MaxY
                                    && zi >= nd.MinZ && zi <= nd.MaxZ
                                if not contains && dSq > 1e-12 && sz * sz < thetaSq * dSq then
                                    let w = 1.0 / (1.0 + dSq)
                                    let mw = nd.Mass * w
                                    zLocal <- zLocal + mw
                                    let mw2 = mw * w
                                    rx <- rx + mw2 * dx
                                    ry <- ry + mw2 * dy
                                    rz <- rz + mw2 * dz
                                elif nd.ItemCount > 0 then
                                    for k = nd.ItemStart to nd.ItemStart + nd.ItemCount - 1 do
                                        let j = perm[k]
                                        if j <> i then
                                            let ldx = xi - xs[j]
                                            let ldy = yi - ys[j]
                                            let ldz = zi - zs[j]
                                            let w = 1.0 / (1.0 + ldx * ldx + ldy * ldy + ldz * ldz)
                                            zLocal <- zLocal + w
                                            let w2 = w * w
                                            rx <- rx + w2 * ldx
                                            ry <- ry + w2 * ldy
                                            rz <- rz + w2 * ldz
                                elif nd.ChildCount > 0 then
                                    for c = nd.FirstChild + nd.ChildCount - 1 downto nd.FirstChild do
                                        stack[sp] <- c
                                        sp <- sp + 1
                            repX[i] <- rx
                            repY[i] <- ry
                            repZ[i] <- rz
                            zSum <- zSum + zLocal

                    // 3. Adaptive delta-bar-delta gains + momentum update + zero-mean centering
                    let invZ = 1.0 / max 1e-12 zSum
                    for i = 0 to n - 1 do
                        let gx = 4.0 * (attrX[i] - repX[i] * invZ)
                        let gy = 4.0 * (attrY[i] - repY[i] * invZ)
                        let gz = 4.0 * (attrZ[i] - repZ[i] * invZ)
                        let nextGX = if (gx > 0.0) <> (velX[i] > 0.0) then gainX[i] + 0.2 else max 0.01 (gainX[i] * 0.8)
                        let nextGY = if (gy > 0.0) <> (velY[i] > 0.0) then gainY[i] + 0.2 else max 0.01 (gainY[i] * 0.8)
                        let nextGZ = if (gz > 0.0) <> (velZ[i] > 0.0) then gainZ[i] + 0.2 else max 0.01 (gainZ[i] * 0.8)
                        gainX[i] <- nextGX
                        gainY[i] <- nextGY
                        gainZ[i] <- nextGZ
                        let vx = momentum * velX[i] - lr * nextGX * gx
                        let vy = momentum * velY[i] - lr * nextGY * gy
                        let vz = momentum * velZ[i] - lr * nextGZ * gz
                        velX[i] <- vx
                        velY[i] <- vy
                        velZ[i] <- vz
                        xs[i] <- xs[i] + vx
                        ys[i] <- ys[i] + vy
                        zs[i] <- zs[i] + vz

                    SimdMath.zeroMeanInPlace xs n
                    SimdMath.zeroMeanInPlace ys n
                    SimdMath.zeroMeanInPlace zs n

                Ok(Manifold3DFit.fitPoints3D config.Bounds ids xs ys zs)

    /// <summary>Computes a deterministic 3D Octree Barnes-Hut t-SNE embedding over <c>dataset</c> without cancellation.</summary>
    let embed (config: Tsne3DConfig) (dataset: FeatureDataset<'Id>) : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None config dataset

/// <summary>Tenenbaum, de Silva &amp; Langford (2000) 3D Isometric Feature Mapping (<c>Isomap3D</c>) via k-NN geodesic shortest paths and double-centered MDS.</summary>
[<RequireQualifiedAccess>]
module Isomap3D =
    /// <summary>Computes a deterministic 3D Isomap embedding over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (bounds: Bounds3)
        (metric: DistanceMetric)
        (neighbors: int)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if neighbors < 1 then
            Error [ InvalidObservation "Isomap3D requires neighbors >= 1." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let coords = ManifoldCore.embedIsomapCore token 3 metric neighbors ids.Length dim data
            Ok(Manifold3DFit.fitPoints3D bounds ids coords[0] coords[1] coords[2])

    /// <summary>Computes a deterministic 3D Isomap embedding over <c>dataset</c> without cancellation.</summary>
    let embed
        (bounds: Bounds3)
        (metric: DistanceMetric)
        (neighbors: int)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None bounds metric neighbors dataset

/// <summary>Sammon (1969) 3D nonlinear stress mapping (<c>Sammon3D</c>) minimizing <c>sum_(i&lt;j) (d_ij - d*_ij)^2 / d*_ij</c> via shuffled SGD.</summary>
[<RequireQualifiedAccess>]
module Sammon3D =
    /// <summary>Computes a deterministic 3D Sammon nonlinear mapping over <c>dataset</c> with cooperative cancellation.</summary>
    let embedWithCancellation
        (token: System.Threading.CancellationToken)
        (bounds: Bounds3)
        (metric: DistanceMetric)
        (iterations: int)
        (seed: uint32)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if iterations < 1 || iterations > 10000 then
            Error [ InvalidObservation "Sammon3D requires iterations in [1,10000]." ]
        else
            let ids, dim, data = FeatureDataset.raw dataset
            let coords = ManifoldCore.embedSammonCore token 3 metric iterations seed ids.Length dim data
            Ok(Manifold3DFit.fitPoints3D bounds ids coords[0] coords[1] coords[2])

    /// <summary>Computes a deterministic 3D Sammon nonlinear mapping over <c>dataset</c> without cancellation.</summary>
    let embed
        (bounds: Bounds3)
        (metric: DistanceMetric)
        (iterations: int)
        (seed: uint32)
        (dataset: FeatureDataset<'Id>)
        : Result<EmbeddedPoint3D<'Id> list, LayoutError list> =
        embedWithCancellation System.Threading.CancellationToken.None bounds metric iterations seed dataset

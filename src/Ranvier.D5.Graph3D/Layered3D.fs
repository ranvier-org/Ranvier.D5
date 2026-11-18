namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Primary stacking axis along which 3D architectural / Sugiyama strata planes are separated.</summary>
[<RequireQualifiedAccess>]
type StratumAxis =
    /// <summary>Horizontal planes stacked downward along +Y (intra-layer nodes spread across X and Z).</summary>
    | Y
    /// <summary>Depth planes stacked along +Z (intra-layer nodes spread across X and Y).</summary>
    | Z

/// <summary>Settings for 3D Stratified Compound Layered layout (Hong &amp; Nikolov 2005).</summary>
type Layered3DConfig =
    { /// <summary>Target 3D bounding volume.</summary>
      Bounds: Bounds3
      /// <summary>Axis perpendicular to the parallel stratum planes.</summary>
      Axis: StratumAxis
      /// <summary>Separation between adjacent stratum planes in 3D layout units.</summary>
      LayerSpacing: float
      /// <summary>Minimum separation between nodes within the same stratum plane.</summary>
      IntraLayerSpacing: float
      /// <summary>Number of 3D barycenter + repulsion sweeps across strata (1..64; default 16).</summary>
      Sweeps: int
      /// <summary>Optional explicit stratum index override per node ID.</summary>
      ExplicitStrata: Map<string, int> }

/// <summary>Constructors for <c>Layered3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module Layered3DConfig =
    /// <summary>Creates a Y-stacked 3D stratified configuration with 110-unit layer spacing and 28-unit intra-plane clearance.</summary>
    let create bounds =
        { Bounds = bounds
          Axis = StratumAxis.Y
          LayerSpacing = 110.
          IntraLayerSpacing = 28.
          Sweeps = 16
          ExplicitStrata = Map.empty }

/// <summary>3D Stratified Compound Architecture &amp; Directed Graph Layout Engine across parallel 3D planes.</summary>
[<RequireQualifiedAccess>]
module Layered3D =
    /// <summary>Lays out a directed/compound <c>DiagramGraph</c> onto parallel 3D strata planes with 2D intra-plane barycenter optimization.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: Layered3DConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout3D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.LayerSpacing && Double.IsFinite config.IntraLayerSpacing)
           || config.LayerSpacing <= 0. || config.IntraLayerSpacing < 0.
           || config.Sweeps < 1 || config.Sweeps > 64
           || config.ExplicitStrata |> Map.exists (fun _ s -> s < 0 || s > 4096) then
            Error [ InvalidObservation "Layered3D requires positive LayerSpacing, nonnegative IntraLayerSpacing, 1..64 Sweeps and strata in [0,4096]." ]
        else
            // Reuse Sugiyama's cycle breaking and network-simplex ranking on the resolved graph for automatic strata!
            let resolved = DiagramGraph.resolveCollapsed graph
            let nodes = DiagramGraph.nodes resolved |> List.toArray
            let assocs = DiagramGraph.associations resolved |> List.toArray
            let n = nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = Kernels3D.computeGroups3D resolved [||]; Associations = [] }
            else
                let sugCfg =
                    { SugiyamaConfig.create (Bounds.create 0. 0. 800. 600.) with
                        CrossingSweeps = 4
                        Routing = SugiyamaRouting.Polyline }
                match Sugiyama.layoutWithCancellation token sugCfg resolved with
                | Error errs -> Error errs
                | Ok sug2D ->
                    let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
                    for i = 0 to n - 1 do nodeIdx.Add(nodes[i].Id, i)

                    let revAssocSet =
                        sug2D.Associations
                        |> Seq.choose (fun ra -> if ra.ReversedInCycle then Some ra.Id else None)
                        |> Set.ofSeq

                    let strata = Array.zeroCreate<int> n
                    for pn in sug2D.Nodes do
                        let idx = nodeIdx[pn.Id]
                        let autoLayer = pn.Layer |> Option.defaultValue 0
                        strata[idx] <- config.ExplicitStrata |> Map.tryFind pn.Id |> Option.defaultValue autoLayer

                    // Compact strata to contiguous 0 .. numStrata-1
                    let distinctStrata = strata |> Array.distinct |> Array.sort
                    let stratumMap = Dictionary<int, int>(distinctStrata.Length)
                    for si = 0 to distinctStrata.Length - 1 do stratumMap.Add(distinctStrata[si], si)
                    for i = 0 to n - 1 do strata[i] <- stratumMap[strata[i]]
                    let numStrata = distinctStrata.Length

                    let layerBuckets = Array.init numStrata (fun _ -> ResizeArray<int>())
                    for i = 0 to n - 1 do layerBuckets[strata[i]].Add i
                    let layers = layerBuckets |> Array.map (fun b -> b.ToArray())

                    let upNbs = Array.init n (fun _ -> ResizeArray<int>())
                    let downNbs = Array.init n (fun _ -> ResizeArray<int>())
                    for a in assocs do
                        let u, v = nodeIdx[a.Source.NodeId], nodeIdx[a.Target.NodeId]
                        if u <> v && strata[u] <> strata[v] then
                            let lo, hi = if strata[u] < strata[v] then u, v else v, u
                            downNbs[lo].Add hi
                            upNbs[hi].Add lo

                    // Initialize intra-plane 2D coordinates (uCoord, vCoord) on deterministic golden-angle disc per stratum, grouped by ParentGroup!
                    let uCoord = Array.zeroCreate<float> n
                    let vCoord = Array.zeroCreate<float> n
                    let planeRad =
                        Array.init n (fun i ->
                            let nd = nodes[i]
                            match config.Axis with
                            | StratumAxis.Y -> 0.5 * Double.Hypot(nd.Size.Width, nd.DepthSize)
                            | StratumAxis.Z -> 0.5 * Double.Hypot(nd.Size.Width, nd.Size.Height))

                    let goldenAngle = Math.PI * (3. - sqrt 5.)
                    for s = 0 to numStrata - 1 do
                        let row = layers[s]
                        // Sort by ParentGroup first so grouped nodes start adjacent on their 3D plane!
                        let sortedRow = Array.copy row
                        Array.Sort(sortedRow, Comparer<int>.Create(fun a b ->
                            let ga = Option.defaultValue "" nodes[a].ParentGroup
                            let gb = Option.defaultValue "" nodes[b].ParentGroup
                            let c = StringComparer.Ordinal.Compare(ga, gb)
                            if c <> 0 then c else compare a b))
                        for k = 0 to sortedRow.Length - 1 do
                            let ni = sortedRow[k]
                            let r = (planeRad[ni] * 1.8 + config.IntraLayerSpacing) * sqrt (float k)
                            let ang = float k * goldenAngle
                            uCoord[ni] <- r * cos ang
                            vCoord[ni] <- r * sin ang

                    // Iterative 2D intra-plane barycenter attraction + non-overlap repulsion + compound group cohesion!
                    for sweep = 0 to config.Sweeps - 1 do
                        token.ThrowIfCancellationRequested()
                        let sStart, sEnd, sStep = if sweep % 2 = 0 then 0, numStrata - 1, 1 else numStrata - 1, 0, -1
                        let mutable s = sStart
                        while (if sStep > 0 then s <= sEnd else s >= sEnd) do
                            let row = layers[s]
                            // Pull toward mean (u, v) of adjacent-strata neighbors
                            for ni in row do
                                let mutable su, sv, cnt = 0., 0., 0
                                for p in upNbs[ni] do su <- su + uCoord[p]; sv <- sv + vCoord[p]; cnt <- cnt + 1
                                for c in downNbs[ni] do su <- su + uCoord[c]; sv <- sv + vCoord[c]; cnt <- cnt + 1
                                if cnt > 0 then
                                    let tu, tv = su / float cnt, sv / float cnt
                                    uCoord[ni] <- uCoord[ni] * 0.35 + tu * 0.65
                                    vCoord[ni] <- vCoord[ni] * 0.35 + tv * 0.65

                            // Group cohesion within this stratum
                            if row.Length > 1 then
                                let gSums = Dictionary<string, struct(float * float * int)>(StringComparer.Ordinal)
                                for ni in row do
                                    nodes[ni].ParentGroup |> Option.iter (fun gid ->
                                        let struct(au, av, ac) = match gSums.TryGetValue gid with true, v -> v | _ -> struct(0., 0., 0)
                                        gSums[gid] <- struct(au + uCoord[ni], av + vCoord[ni], ac + 1))
                                for ni in row do
                                    nodes[ni].ParentGroup |> Option.iter (fun gid ->
                                        let struct(au, av, ac) = gSums[gid]
                                        if ac > 1 then
                                            uCoord[ni] <- uCoord[ni] + (au / float ac - uCoord[ni]) * 0.25
                                            vCoord[ni] <- vCoord[ni] + (av / float ac - vCoord[ni]) * 0.25)

                            // Resolve 2D intra-stratum overlaps
                            if row.Length > 1 then
                                for _ = 0 to 2 do
                                    for a = 0 to row.Length - 1 do
                                        for b = a + 1 to row.Length - 1 do
                                            let ia, ib = row[a], row[b]
                                            let du = uCoord[ib] - uCoord[ia]
                                            let dv = vCoord[ib] - vCoord[ia]
                                            let d = Double.Hypot(du, dv)
                                            let minSep =
                                                planeRad[ia] + planeRad[ib] + config.IntraLayerSpacing
                                                + (if nodes[ia].ParentGroup <> nodes[ib].ParentGroup then 16. else 0.)
                                            if d < minSep then
                                                let uu, uv =
                                                    if d < 1e-5 then
                                                        let ang = float ((ia * 17 + ib * 31) % 360) * (Math.PI / 180.)
                                                        cos ang, sin ang
                                                    else du / d, dv / d
                                                let push = (minSep - d) * 0.52
                                                uCoord[ia] <- uCoord[ia] - uu * push
                                                vCoord[ia] <- vCoord[ia] - uv * push
                                                uCoord[ib] <- uCoord[ib] + uu * push
                                                vCoord[ib] <- vCoord[ib] + uv * push
                            s <- s + sStep

                    let c3 = Bounds3.center config.Bounds
                    let totalSpan = float (max 0 (numStrata - 1)) * config.LayerSpacing
                    let startPlane = -totalSpan * 0.5

                    let toPoint3 ni =
                        let planePos = startPlane + float strata[ni] * config.LayerSpacing
                        match config.Axis with
                        | StratumAxis.Y -> Point3.create (c3.X + uCoord[ni]) (c3.Y + planePos) (c3.Z + vCoord[ni])
                        | StratumAxis.Z -> Point3.create (c3.X + uCoord[ni]) (c3.Y + vCoord[ni]) (c3.Z + planePos)

                    let nodeBoxes = Array.zeroCreate<Bounds3> n
                    let centers = Array.init n toPoint3
                    let posNodes =
                        [ for i in 0 .. n - 1 do
                            let nd = nodes[i]
                            let box = Kernels3D.nodeBox centers[i] nd.Size.Width nd.Size.Height nd.DepthSize
                            let rad = 0.5 * sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height + nd.DepthSize * nd.DepthSize)
                            nodeBoxes[i] <- box
                            yield
                                { Id = nd.Id
                                  Kind = nd.Kind
                                  Center = centers[i]
                                  Bounds = box
                                  Radius = rad
                                  ScaleFactor = 1.
                                  ParentGroup = nd.ParentGroup
                                  Layer = Some strata[i] } ]

                    let groups = Kernels3D.computeGroups3D resolved nodeBoxes
                    // Route inter-stratum 3D associations with clean vertical/axial riser waypoints!
                    let routed =
                        [ for a in assocs do
                            let u, v = nodeIdx[a.Source.NodeId], nodeIdx[a.Target.NodeId]
                            let cu, cv = centers[u], centers[v]
                            let pts =
                                if u = v then
                                    let r = planeRad[u]
                                    [| cu; Point3.create (cu.X + r * 1.4) cu.Y (cu.Z + r * 1.4); cu |]
                                elif strata[u] = strata[v] then
                                    [| cu; cv |]
                                else
                                    match config.Axis with
                                    | StratumAxis.Y ->
                                        let midY = (cu.Y + cv.Y) * 0.5
                                        [| cu; Point3.create cu.X midY cu.Z; Point3.create cv.X midY cv.Z; cv |]
                                    | StratumAxis.Z ->
                                        let midZ = (cu.Z + cv.Z) * 0.5
                                        [| cu; Point3.create cu.X cu.Y midZ; Point3.create cv.X cv.Y midZ; cv |]
                            yield
                                { Id = a.Id
                                  Source = a.Source
                                  Target = a.Target
                                  Kind = a.Kind
                                  IsBackbone = not (revAssocSet.Contains a.Id)
                                  Points = Point3Chain.ofOwnedArray pts
                                  Waypoints = Array.toList pts } ]

                    Ok
                        { Bounds = Kernels3D.computeBounds3D config.Bounds posNodes groups routed
                          Nodes = posNodes
                          Groups = groups
                          Associations = routed }

    /// <summary>Runs 3D Stratified Compound Layered layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

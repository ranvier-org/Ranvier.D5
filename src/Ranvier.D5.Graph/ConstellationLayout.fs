namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Clustering policy used by <c>ConstellationLayout</c> to partition a graph into packed Poincaré sub-discs.</summary>
[<RequireQualifiedAccess>]
type ConstellationClusterMode =
    /// <summary>Uses explicit <c>DiagramGroup</c> nesting from the graph (falling back to a single disc when ungrouped).</summary>
    | ExplicitGroups
    /// <summary>Partitions nodes automatically using 3-phase Leiden modularity detection at the given nonnegative <c>resolution</c>.</summary>
    | LeidenAuto of resolution: float
    /// <summary>Preserves explicit <c>DiagramGroup</c> hierarchy and splits ungrouped graphs or leaf groups larger than <c>maxGroupSize</c> via Leiden at <c>resolution</c>.</summary>
    | HybridGroupsAndLeiden of maxGroupSize: int * resolution: float

/// <summary>A circular cluster boundary in a multi-scale <c>ConstellationLayout</c> hierarchy.</summary>
type ClusterDisc =
    { /// <summary>Unique cluster identifier (matching <c>DiagramGroup.Id</c> when derived from an explicit group).</summary>
      Id: string
      /// <summary>Enclosing parent cluster identifier, or <c>None</c> at the root constellation disc.</summary>
      Parent: string option
      /// <summary>Tree depth from the root constellation disc (<c>0</c> at root).</summary>
      Depth: int
      /// <summary>Euclidean center of the cluster disc in viewport coordinates.</summary>
      Center: Point
      /// <summary>Positive Euclidean radius of the cluster disc in layout units.</summary>
      Radius: float
      /// <summary>True when this disc directly hosts a local <c>PoincareDisk</c> node sub-layout rather than child cluster discs.</summary>
      IsLeafCluster: bool
      /// <summary>Total count of descendant graph nodes enclosed within this cluster disc.</summary>
      MemberCount: int
      /// <summary>Concentric internal hyperbolic guide ring radii inside a leaf cluster disc in viewport units (<c>[]</c> for non-leaf parent discs).</summary>
      InnerRingRadii: float list
      /// <summary>Perimeter angular sector arc bands (<c>treeBands</c>) around this disc marking child clusters or subgroups.</summary>
      SectorArcs: RimArcSegment list }

/// <summary>Configuration for multi-scale constellation layout combining tangent circle packing, local Poincaré disks and LCA spline bundling.</summary>
type ConstellationConfig =
    { /// <summary>Target viewport into which the outer constellation disc is inscribed.</summary>
      Viewport: Bounds
      /// <summary>Cluster hierarchy discovery policy.</summary>
      ClusterMode: ConstellationClusterMode
      /// <summary>Nonnegative radial separation padding between sibling cluster discs and parent walls.</summary>
      ClusterPadding: float
      /// <summary>Concentric ring assignment rule inside each leaf Poincaré sub-disc.</summary>
      RingLayering: PoincareRingLayering
      /// <summary>Innermost ring bias inside each leaf Poincaré sub-disc.</summary>
      CenterBias: PoincareCenterBias
      /// <summary>Conformal node foreshortening exponent inside each leaf Poincaré sub-disc.</summary>
      ConformalExponent: float
      /// <summary>Holten hierarchical spline bundling strength in <c>[0.0, 1.0]</c> for inter-cluster associations.</summary>
      BundleBeta: float
      /// <summary>Circular barycenter crossing-reduction sweeps per leaf Poincaré sub-disc.</summary>
      BarycenterSweeps: int }

/// <summary>Constructors for <c>ConstellationConfig</c>.</summary>
[<RequireQualifiedAccess>]
module ConstellationConfig =
    /// <summary>Creates default constellation settings: <c>HybridGroupsAndLeiden(18, 1.0)</c>, 14-unit cluster padding, <c>LongestPath</c> foundations-inner rings, 0.55 conformal exponent, 0.82 bundle beta and 6 sweeps.</summary>
    let create (viewport: Bounds) : ConstellationConfig =
        { Viewport = viewport
          ClusterMode = ConstellationClusterMode.HybridGroupsAndLeiden(18, 1.0)
          ClusterPadding = 14.
          RingLayering = PoincareRingLayering.LongestPath
          CenterBias = PoincareCenterBias.FoundationsInner
          ConformalExponent = 0.55
          BundleBeta = 0.82
          BarycenterSweeps = 6 }

/// <summary>Complete multi-scale constellation layout result including the 2D diagram layout and enclosing <c>ClusterDisc</c> hierarchy.</summary>
type ConstellationOutput =
    { /// <summary>Positioned 2D nodes, compound groups and hybrid geodesic/bundled associations.</summary>
      Layout: DiagramLayout2D
      /// <summary>Hierarchy of packed cluster discs ordered outermost-first (by ascending depth, then ordinal ID).</summary>
      Clusters: ClusterDisc list
      /// <summary>Center of the root outer constellation disc.</summary>
      OuterCenter: Point
      /// <summary>Radius of the root outer constellation disc.</summary>
      OuterRadius: float }

/// <summary>An internal multi-scale constellation miniature inscribed inside a host node's circular disc ("graph-in-disc" LOD inlay).</summary>
type DiscInlay =
    { /// <summary>Identifier of the host node in the outer layout whose disc contains this inlay.</summary>
      HostNodeId: string
      /// <summary>Euclidean center of the host disc in outer layout coordinates.</summary>
      Center: Point
      /// <summary>Inscribed Euclidean radius of the host disc in outer layout coordinates.</summary>
      Radius: float
      /// <summary>Scaled nested cluster discs (including <c>InnerRingRadii</c> and <c>SectorArcs</c>) inside the host disc.</summary>
      Clusters: ClusterDisc list
      /// <summary>Scaled internal nodes positioned inside the host disc.</summary>
      Nodes: PositionedNode list
      /// <summary>Scaled intra-cluster Poincaré geodesic arcs and inter-cluster LCA bundled splines inside the host disc.</summary>
      Associations: RoutedAssociation list
      /// <summary>Optional outer rim status/category gauge arc segments hugging the host disc boundary.</summary>
      RimGauge: RimArcSegment list }

/// <summary>Multi-scale constellation graph layout packing local Poincaré sub-discs via Apollonius/Welzl disc enclosure and routing intra-cluster geodesics + inter-cluster LCA splines.</summary>
[<RequireQualifiedAccess>]
module ConstellationLayout =
    [<Struct>]
    type private Disc2 =
        val Cx: float
        val Cy: float
        val R: float
        new(cx, cy, r) = { Cx = cx; Cy = cy; R = r }

    let inline private enclosesDisc (b: Disc2) (s: Disc2) (tol: float) : bool =
        if b.R < 0. then false
        else Double.Hypot(s.Cx - b.Cx, s.Cy - b.Cy) + s.R <= b.R + tol * (1. + b.R)

    let inline private enclosesAllDiscs (b: Disc2) (items: Disc2[]) (count: int) (tol: float) : bool =
        if b.R < 0. then false
        else
            let mutable ok = true
            let mutable i = 0
            while ok && i < count do
                ok <- enclosesDisc b (items[i]) tol
                i <- i + 1
            ok

    let private enclose2 (s0: Disc2) (s1: Disc2) : Disc2 =
        let dx = s1.Cx - s0.Cx
        let dy = s1.Cy - s0.Cy
        let d = Double.Hypot(dx, dy)
        if d + s1.R <= s0.R then s0
        elif d + s0.R <= s1.R then s1
        else
            let r = 0.5 * (d + s0.R + s1.R)
            let t = if d <= 1e-15 then 0.5 else (r - s0.R) / d
            Disc2(s0.Cx + dx * t, s0.Cy + dy * t, r)

    let private enclose3 (s0: Disc2) (s1: Disc2) (s2: Disc2) : Disc2 =
        let items = [| s0; s1; s2 |]
        let mutable best = Disc2(0., 0., infinity)
        let inline tryCand (cand: Disc2) =
            if cand.R >= 0. && cand.R < best.R && enclosesAllDiscs cand items 3 1e-10 then
                best <- cand

        tryCand (enclose2 s0 s1)
        tryCand (enclose2 s0 s2)
        tryCand (enclose2 s1 s2)

        if Double.IsFinite best.R then
            best
        else
            // Exact 2D Apollonius outer tangent circle in the local orthonormal frame of (s0 -> s1)
            let ux = s1.Cx - s0.Cx
            let uy = s1.Cy - s0.Cy
            let l1 = Double.Hypot(ux, uy)
            if l1 > 1e-12 then
                let exX = ux / l1
                let exY = uy / l1
                let eyX = -exY
                let eyY = exX
                let dvx = s2.Cx - s0.Cx
                let dvy = s2.Cy - s0.Cy
                let vx = dvx * exX + dvy * exY
                let vy = dvx * eyX + dvy * eyY
                if abs vy > 1e-12 then
                    let r0, r1, r2 = s0.R, s1.R, s2.R
                    let ax = (l1 * l1 + r0 * r0 - r1 * r1) / (2. * l1)
                    let bx = (r1 - r0) / l1
                    let ay = (vx * vx + vy * vy + r0 * r0 - r2 * r2 - 2. * vx * ax) / (2. * vy)
                    let by = (r2 - r0 - vx * bx) / vy
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
                            let mutable rad = rClamped
                            for k = 0 to 2 do
                                let dk = Double.Hypot(items[k].Cx - cx, items[k].Cy - cy) + items[k].R
                                if dk > rad then rad <- dk
                            if rad < best.R then
                                best <- Disc2(cx, cy, rad)

                    if abs aQ <= 1e-13 then
                        if abs bQ > 1e-13 then evalRoot (-cQ / (2. * bQ))
                    else
                        let disc = bQ * bQ - aQ * cQ
                        if disc >= -1e-10 then
                            let rt = sqrt (max 0. disc)
                            evalRoot ((-bQ - rt) / aQ)
                            evalRoot ((-bQ + rt) / aQ)

            if Double.IsFinite best.R then
                best
            else
                let cx = (s0.Cx + s1.Cx + s2.Cx) / 3.
                let cy = (s0.Cy + s1.Cy + s2.Cy) / 3.
                let mutable rad = 0.
                for k = 0 to 2 do
                    let dk = Double.Hypot(items[k].Cx - cx, items[k].Cy - cy) + items[k].R
                    if dk > rad then rad <- dk
                Disc2(cx, cy, rad)

    let private welzlEncloseArray (discs: Disc2[]) : Disc2 =
        let m = discs.Length
        if m = 0 then
            Disc2(0., 0., 0.)
        elif m = 1 then
            discs[0]
        else
            let order = Array.init m id
            let support = Array.zeroCreate<Disc2> 3

            let basisDisc (sCount: int) =
                match sCount with
                | 0 -> Disc2(0., 0., -1.)
                | 1 -> support[0]
                | 2 -> enclose2 (support[0]) (support[1])
                | _ -> enclose3 (support[0]) (support[1]) (support[2])

            // Recursion depth is bounded strictly by sCount <= 3.
            let rec mtfDisc (limit: int) (sCount: int) : Disc2 =
                let mutable cur = basisDisc sCount
                if sCount < 3 then
                    let mutable i = 0
                    while i < limit do
                        let idx = order[i]
                        if not (enclosesDisc cur (discs[idx]) 1e-11) then
                            support[sCount] <- discs[idx]
                            cur <- mtfDisc i (sCount + 1)
                            for k = i downto 1 do
                                order[k] <- order[k - 1]
                            order[0] <- idx
                        i <- i + 1
                cur

            let raw = mtfDisc m 0
            let mutable tightR = max 0. raw.R
            for i = 0 to m - 1 do
                let req = Double.Hypot(discs[i].Cx - raw.Cx, discs[i].Cy - raw.Cy) + discs[i].R
                if req > tightR then tightR <- req
            Disc2(raw.Cx, raw.Cy, tightR)

    /// <summary>Computes the exact minimum enclosing circle <c>(center, radius)</c> of a non-empty list of 2D discs <c>(center, radius &gt;= 0)</c> using Welzl move-to-front + Apollonius tangency.</summary>
    let encloseDiscs (discs: (Point * float) list) : Result<Point * float, LayoutError list> =
        if discs.IsEmpty then
            Error [ InvalidObservation "ConstellationLayout.encloseDiscs requires at least one disc." ]
        else
            let arr = ResizeArray<Disc2>()
            let mutable invalid = false
            for (pt, r) in discs do
                if not (Double.IsFinite pt.X && Double.IsFinite pt.Y && Double.IsFinite r) || r < 0. then
                    invalid <- true
                else
                    arr.Add(Disc2(pt.X, pt.Y, r))
            if invalid then
                Error [ InvalidObservation "All disc centers must be finite and disc radii must be finite and nonnegative." ]
            else
                let enc = welzlEncloseArray (arr.ToArray())
                Ok(Point.create enc.Cx enc.Cy, enc.R)

    // Front-chain tangent circle placement adapted from d3-hierarchy packSiblings, refined by exact Welzl disc enclosure.
    let private packSiblingDiscs (radii: float[]) (pad: float) : Point[] * float =
        let n = radii.Length
        if n = 0 then
            [||], 0.
        elif n = 1 then
            [| Point.create 0. 0. |], radii[0]
        else
            let halfPad = 0.5 * pad
            let r = Array.init n (fun i -> max 1e-6 (radii[i] + halfPad))
            let x = Array.zeroCreate<float> n
            let y = Array.zeroCreate<float> n
            let next = Array.zeroCreate<int> n
            let prev = Array.zeroCreate<int> n

            let inline place b a c =
                let dx = x[b] - x[a]
                let dy = y[b] - y[a]
                let d2 = dx * dx + dy * dy
                if d2 <= 1e-18 then
                    x[c] <- x[a] + r[a] + r[c]
                    y[c] <- y[a]
                else
                    let rac = r[a] + r[c]
                    let rbc = r[b] + r[c]
                    let a2 = rac * rac
                    let b2 = rbc * rbc
                    if a2 > b2 then
                        let along = (d2 + b2 - a2) / (2. * d2)
                        let perp = sqrt (max 0. (b2 / d2 - along * along))
                        x[c] <- x[b] - along * dx - perp * dy
                        y[c] <- y[b] - along * dy + perp * dx
                    else
                        let along = (d2 + a2 - b2) / (2. * d2)
                        let perp = sqrt (max 0. (a2 / d2 - along * along))
                        x[c] <- x[a] + along * dx - perp * dy
                        y[c] <- y[a] + along * dy + perp * dx

            let inline intersects a b =
                let dr = (r[a] + r[b]) * (1. - 1e-11)
                let dx = x[a] - x[b]
                let dy = y[a] - y[b]
                dr > 0. && dr * dr > dx * dx + dy * dy

            x[0] <- -r[1]
            y[0] <- 0.
            x[1] <- r[0]
            y[1] <- 0.

            if n > 2 then
                place 1 0 2
                next[0] <- 1; prev[1] <- 0
                next[1] <- 2; prev[2] <- 1
                next[2] <- 0; prev[0] <- 2

                let inline score a =
                    let b = next[a]
                    let invR = 1. / (r[a] + r[b])
                    let dx = (x[a] * r[b] + x[b] * r[a]) * invR
                    let dy = (y[a] * r[b] + y[b] * r[a]) * invR
                    dx * dx + dy * dy

                let mutable a = 0
                let mutable b = 1
                let mutable i = 3
                while i < n do
                    place a b i
                    let mutable j = next[b]
                    let mutable k = prev[a]
                    let mutable sj = r[b]
                    let mutable sk = r[a]
                    let mutable searching = true
                    let mutable retry = false
                    while searching do
                        if sj <= sk then
                            if intersects j i then
                                b <- j
                                next[a] <- b
                                prev[b] <- a
                                retry <- true
                            else
                                sj <- sj + r[j]
                                j <- next[j]
                        else
                            if intersects k i then
                                a <- k
                                next[a] <- b
                                prev[b] <- a
                                retry <- true
                            else
                                sk <- sk + r[k]
                                k <- prev[k]
                        searching <- (not retry) && (j <> next[k])

                    if not retry then
                        prev[i] <- a
                        next[i] <- b
                        next[a] <- i
                        prev[b] <- i
                        b <- i
                        let mutable best = score a
                        let mutable c = next[i]
                        while c <> b do
                            let cand = score c
                            if cand < best then
                                a <- c
                                best <- cand
                            c <- next[c]
                        b <- next[a]
                        i <- i + 1

            let rawDiscs = Array.init n (fun idx -> Disc2(x[idx], y[idx], radii[idx]))
            let enc = welzlEncloseArray rawDiscs
            let centers = Array.init n (fun idx -> Point.create (x[idx] - enc.Cx) (y[idx] - enc.Cy))
            centers, enc.R

    type private ClusterNodeSpec =
        { Id: string
          ParentIdx: int
          Depth: int
          mutable Children: int[]
          mutable DirectMembers: int[]
          mutable TotalMembers: int
          IsExplicitGroup: bool }

    let private uniqueSyntheticId (reserved: HashSet<string>) (preferred: string) : string =
        if reserved.Add preferred then
            preferred
        else
            let mutable sfx = 1
            let mutable cand = preferred + ":" + string sfx
            while not (reserved.Add cand) do
                sfx <- sfx + 1
                cand <- preferred + ":" + string sfx
            cand

    let private inducedSubgraph (eff: EffectiveDiagram) (csr: CsrGraph) (members: int[]) : DiagramGraph option =
        let mLen = members.Length
        if mLen = 0 then
            None
        else
            let inSub = Array.zeroCreate<bool> eff.Nodes.Length
            for k = 0 to mLen - 1 do inSub[members[k]] <- true
            let subNodes =
                [ for k in 0 .. mLen - 1 ->
                    let nd = eff.Nodes[members[k]]
                    { nd with ParentGroup = None; Pinned = None } ]
            let subAssocs =
                [ for e in 0 .. csr.EdgeCount - 1 do
                    if inSub[csr.Src[e]] && inSub[csr.Dst[e]] then
                        yield eff.Associations[e] ]
            match DiagramGraph.simple subNodes subAssocs with
            | Ok g -> Some g
            | Error _ -> None

    // Computes tangent angular sector slots (startAngle, endAngle) around a parent disc for packed child discs.
    let private computeChildSectorSlots (childKeys: string[]) (offsets: Point[]) (radii: float[]) : (string * float * float)[] =
        let m = offsets.Length
        if m = 0 then
            [||]
        elif m = 1 then
            [| childKeys[0], -0.5 * Math.PI, 1.5 * Math.PI - 1e-4 |]
        else
            let theta = Array.init m (fun k -> atan2 offsets[k].Y offsets[k].X)
            let halfCone =
                Array.init m (fun k ->
                    let d = Double.Hypot(offsets[k].X, offsets[k].Y)
                    if d > radii[k] && d > 1e-9 then
                        asin (min 1. (max 0.05 (radii[k] / d)))
                    else
                        0.5 * Math.PI)
            let order = Array.init m id
            Array.sortInPlaceWith (fun a b ->
                let c = compare theta[a] theta[b]
                if c <> 0 then c else compare a b) order

            let splitAfter = Array.zeroCreate<float> m
            for pos = 0 to m - 1 do
                let q = order[pos]
                let nxt = order[(pos + 1) % m]
                let mutable delta = (theta[nxt] - theta[q]) % (2. * Math.PI)
                if delta < 0. then delta <- delta + 2. * Math.PI
                if delta <= 1e-9 then delta <- 2. * Math.PI / float m
                let denom = max 1e-9 (halfCone[q] + halfCone[nxt])
                splitAfter[pos] <- theta[q] + delta * (halfCone[q] / denom)

            let out = Array.zeroCreate<string * float * float> m
            for pos = 0 to m - 1 do
                let prevPos = (pos - 1 + m) % m
                let q = order[pos]
                let mutable a0 = splitAfter[prevPos]
                let mutable a1 = splitAfter[pos]
                while a0 > theta[q] do a0 <- a0 - 2. * Math.PI
                while a1 < a0 do a1 <- a1 + 2. * Math.PI
                let pad = min 0.012 (0.16 * (a1 - a0))
                out[pos] <- (childKeys[q], a0 + pad, a1 - pad)
            out

    let private validateConfig (config: ConstellationConfig) : Result<unit, LayoutError list> =
        let vx = Bounds.x config.Viewport
        let vy = Bounds.y config.Viewport
        let vw = Bounds.width config.Viewport
        let vh = Bounds.height config.Viewport
        let modeOk =
            match config.ClusterMode with
            | ConstellationClusterMode.ExplicitGroups -> true
            | ConstellationClusterMode.LeidenAuto res -> Double.IsFinite res && res >= 0.
            | ConstellationClusterMode.HybridGroupsAndLeiden(maxSz, res) -> maxSz >= 1 && Double.IsFinite res && res >= 0.
        if not (Double.IsFinite vx && Double.IsFinite vy && Double.IsFinite vw && Double.IsFinite vh)
           || vw <= 0. || vh <= 0. then
            Error [ InvalidObservation "ConstellationConfig Viewport must have finite coordinates and positive width and height." ]
        elif not modeOk then
            Error [ InvalidObservation "ConstellationConfig ClusterMode requires positive maxGroupSize and finite nonnegative Leiden resolution." ]
        elif not (Double.IsFinite config.ClusterPadding) || config.ClusterPadding < 0. then
            Error [ InvalidObservation "ConstellationConfig ClusterPadding must be finite and nonnegative." ]
        elif not (Double.IsFinite config.ConformalExponent) || config.ConformalExponent < 0. then
            Error [ InvalidObservation "ConstellationConfig ConformalExponent must be finite and nonnegative." ]
        elif not (Double.IsFinite config.BundleBeta) || config.BundleBeta < 0. || config.BundleBeta > 1. then
            Error [ InvalidObservation "ConstellationConfig BundleBeta must be finite and in [0.0, 1.0]." ]
        elif config.BarycenterSweeps < 0 || config.BarycenterSweeps > 500 then
            Error [ InvalidObservation "ConstellationConfig BarycenterSweeps must be in [0, 500]." ]
        else
            Ok()

    let private computeCore
        (token: System.Threading.CancellationToken)
        (minNodePx: float)
        (config: ConstellationConfig)
        (graph: DiagramGraph)
        : Result<ConstellationOutput, LayoutError list> =
        token.ThrowIfCancellationRequested()
        match validateConfig config with
        | Error errs -> Error errs
        | Ok () ->
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            let vx = Bounds.x config.Viewport
            let vy = Bounds.y config.Viewport
            let vw = Bounds.width config.Viewport
            let vh = Bounds.height config.Viewport
            let outerCenter = Point.create (vx + vw * 0.5) (vy + vh * 0.5)
            let availOuterR = max 2. (0.5 * min vw vh - 0.5 * config.ClusterPadding)

            if n = 0 then
                let emptyGroups = GraphKernels.computeGroupBounds eff [||]
                Ok
                    { Layout =
                        { Bounds = config.Viewport
                          Nodes = []
                          Groups = emptyGroups
                          Associations = [] }
                      Clusters = []
                      OuterCenter = outerCenter
                      OuterRadius = availOuterR }
            else
                let csr = GraphKernels.buildCsr eff
                let reserved = HashSet<string>(StringComparer.Ordinal)
                for i = 0 to n - 1 do reserved.Add(eff.Nodes[i].Id) |> ignore
                for gi = 0 to eff.Groups.Length - 1 do reserved.Add(eff.Groups[gi].Id) |> ignore

                let specs = ResizeArray<ClusterNodeSpec>()
                let addSpec id parentIdx depth directMembers isExplicit =
                    let idx = specs.Count
                    specs.Add
                        { Id = id
                          ParentIdx = parentIdx
                          Depth = depth
                          Children = [||]
                          DirectMembers = directMembers
                          TotalMembers = directMembers.Length
                          IsExplicitGroup = isExplicit }
                    idx

                let buildLeidenRoot (res: float) (maxSplitOpt: int option) : Result<unit, LayoutError list> =
                    match Leiden.detectWithCancellation token { LeidenConfig.defaultValue with Resolution = res } graph with
                    | Error errs -> Error errs
                    | Ok part ->
                        let k = max 1 part.CommunityCount
                        if k = 1 then
                            let rootId = uniqueSyntheticId reserved "cluster:0"
                            let allMems = Array.init n id
                            addSpec rootId -1 0 allMems false |> ignore
                            Ok()
                        else
                            let rootId = uniqueSyntheticId reserved "constellation"
                            let rootIdx = addSpec rootId -1 0 [||] false
                            let buckets = Array.init k (fun _ -> ResizeArray<int>())
                            for i = 0 to n - 1 do
                                let cid =
                                    match Map.tryFind (eff.Nodes[i].Id) part.Communities with
                                    | Some c when c >= 0 && c < k -> c
                                    | _ -> 0
                                buckets[cid].Add i
                            let kids = ResizeArray<int>(k)
                            for c = 0 to k - 1 do
                                if buckets[c].Count > 0 then
                                    let mems = buckets[c].ToArray()
                                    let cidStr = uniqueSyntheticId reserved ("cluster:" + string c)
                                    let cIdx = addSpec cidStr rootIdx 1 mems false
                                    kids.Add cIdx
                                    match maxSplitOpt with
                                    | Some maxSz when mems.Length > maxSz ->
                                        match inducedSubgraph eff csr mems with
                                        | Some subG ->
                                            match Leiden.detectWithCancellation token { LeidenConfig.defaultValue with Resolution = max 1.25 (res * 1.5) } subG with
                                            | Ok subPart when subPart.CommunityCount > 1 ->
                                                let subBuckets = Array.init subPart.CommunityCount (fun _ -> ResizeArray<int>())
                                                for m = 0 to mems.Length - 1 do
                                                    let u = mems[m]
                                                    let sc =
                                                        match Map.tryFind (eff.Nodes[u].Id) subPart.Communities with
                                                        | Some x when x >= 0 && x < subPart.CommunityCount -> x
                                                        | _ -> 0
                                                    subBuckets[sc].Add u
                                                let subKids = ResizeArray<int>()
                                                for sc = 0 to subPart.CommunityCount - 1 do
                                                    if subBuckets[sc].Count > 0 then
                                                        let sid = uniqueSyntheticId reserved (cidStr + ":" + string sc)
                                                        subKids.Add(addSpec sid cIdx 2 (subBuckets[sc].ToArray()) false)
                                                if subKids.Count > 1 then
                                                    specs[cIdx].DirectMembers <- [||]
                                                    specs[cIdx].Children <- subKids.ToArray()
                                            | _ -> ()
                                        | None -> ()
                                    | _ -> ()
                            specs[rootIdx].Children <- kids.ToArray()
                            Ok()

                let buildGroupTree (splitOversized: (int * float) option) : Result<unit, LayoutError list> =
                    let gLen = eff.Groups.Length
                    if gLen = 0 then
                        match splitOversized with
                        | None ->
                            let rootId = uniqueSyntheticId reserved "constellation"
                            addSpec rootId -1 0 (Array.init n id) false |> ignore
                            Ok()
                        | Some(maxSz, res) ->
                            buildLeidenRoot res (Some maxSz)
                    else
#if FABLE_COMPILER
                        let gIdxMap = Dictionary<string, int>(StringComparer.Ordinal)
#else
                        let gIdxMap = Dictionary<string, int>(gLen, StringComparer.Ordinal)
#endif
                        for gi = 0 to gLen - 1 do gIdxMap[eff.Groups[gi].Id] <- gi

                        let directByGroup = Array.init gLen (fun _ -> ResizeArray<int>())
                        let ungrouped = ResizeArray<int>()
                        for i = 0 to n - 1 do
                            match eff.Nodes[i].ParentGroup with
                            | Some gid ->
                                match gIdxMap.TryGetValue gid with
                                | true, gi -> directByGroup[gi].Add i
                                | _ -> ungrouped.Add i
                            | None -> ungrouped.Add i

                        let childGroups = Array.init gLen (fun _ -> ResizeArray<int>())
                        let topGroups = ResizeArray<int>()
                        for gi = 0 to gLen - 1 do
                            match eff.Groups[gi].ParentGroup with
                            | Some pid ->
                                match gIdxMap.TryGetValue pid with
                                | true, pi -> childGroups[pi].Add gi
                                | _ -> topGroups.Add gi
                            | None -> topGroups.Add gi

                        let singleRootGroup = (topGroups.Count = 1 && ungrouped.Count = 0)

                        let rec emitGroup (gi: int) (parentSpecIdx: int) (depth: int) : int =
                            let g = eff.Groups[gi]
                            let cGroups = childGroups[gi]
                            let dNodes = directByGroup[gi].ToArray()
                            if cGroups.Count = 0 then
                                addSpec g.Id parentSpecIdx depth dNodes true
                            else
                                let myIdx = addSpec g.Id parentSpecIdx depth [||] true
                                let kids = ResizeArray<int>(cGroups.Count + 1)
                                for k = 0 to cGroups.Count - 1 do
                                    kids.Add(emitGroup (cGroups[k]) myIdx (depth + 1))
                                if dNodes.Length > 0 then
                                    let dId = uniqueSyntheticId reserved (g.Id + ":nodes")
                                    kids.Add(addSpec dId myIdx (depth + 1) dNodes false)
                                specs[myIdx].Children <- kids.ToArray()
                                myIdx

                        if singleRootGroup then
                            emitGroup (topGroups[0]) -1 0 |> ignore
                        else
                            let rootId = uniqueSyntheticId reserved "constellation"
                            let rootIdx = addSpec rootId -1 0 [||] false
                            let rootKids = ResizeArray<int>(topGroups.Count + 1)
                            for k = 0 to topGroups.Count - 1 do
                                rootKids.Add(emitGroup (topGroups[k]) rootIdx 1)
                            if ungrouped.Count > 0 then
                                let uId = uniqueSyntheticId reserved "ungrouped"
                                rootKids.Add(addSpec uId rootIdx 1 (ungrouped.ToArray()) false)
                            specs[rootIdx].Children <- rootKids.ToArray()

                        match splitOversized with
                        | None -> Ok()
                        | Some(maxSz, res) ->
                            // Snapshot current leaves and subdivide any leaf exceeding maxSz via Leiden on its induced subgraph.
                            let initialCount = specs.Count
                            for sIdx = 0 to initialCount - 1 do
                                let sp = specs[sIdx]
                                if sp.Children.Length = 0 && sp.DirectMembers.Length > maxSz then
                                    let mems = sp.DirectMembers
                                    match inducedSubgraph eff csr mems with
                                    | Some subG ->
                                        match Leiden.detectWithCancellation token { LeidenConfig.defaultValue with Resolution = res } subG with
                                        | Ok part when part.CommunityCount > 1 ->
                                            let buckets = Array.init part.CommunityCount (fun _ -> ResizeArray<int>())
                                            for m = 0 to mems.Length - 1 do
                                                let u = mems[m]
                                                let c =
                                                    match Map.tryFind (eff.Nodes[u].Id) part.Communities with
                                                    | Some x when x >= 0 && x < part.CommunityCount -> x
                                                    | _ -> 0
                                                buckets[c].Add u
                                            let subKids = ResizeArray<int>()
                                            for c = 0 to part.CommunityCount - 1 do
                                                if buckets[c].Count > 0 then
                                                    let cid = uniqueSyntheticId reserved (sp.Id + ":c" + string c)
                                                    subKids.Add(addSpec cid sIdx (sp.Depth + 1) (buckets[c].ToArray()) false)
                                            if subKids.Count > 1 then
                                                specs[sIdx].DirectMembers <- [||]
                                                specs[sIdx].Children <- subKids.ToArray()
                                        | _ -> ()
                                    | None -> ()
                            Ok()

                let treeResult =
                    match config.ClusterMode with
                    | ConstellationClusterMode.ExplicitGroups -> buildGroupTree None
                    | ConstellationClusterMode.LeidenAuto res -> buildLeidenRoot res None
                    | ConstellationClusterMode.HybridGroupsAndLeiden(maxSz, res) -> buildGroupTree (Some(maxSz, res))

                match treeResult with
                | Error errs -> Error errs
                | Ok () ->
                    let cCount = specs.Count
                    // Iterative postorder traversal of cluster tree from root 0
                    let postorder = ResizeArray<int>(cCount)
                    let pStack = Stack<struct(int * bool)>()
                    pStack.Push(struct(0, false))
                    while pStack.Count > 0 do
                        let struct(u, visited) = pStack.Pop()
                        if visited then
                            postorder.Add u
                        else
                            pStack.Push(struct(u, true))
                            let kids = specs[u].Children
                            for k = kids.Length - 1 downto 0 do
                                pStack.Push(struct(kids[k], false))

                    let localNodeCx = Array.zeroCreate<float> n
                    let localNodeCy = Array.zeroCreate<float> n
                    let localNodeW = Array.zeroCreate<float> n
                    let localNodeH = Array.zeroCreate<float> n
                    let localNodeLayer = Array.zeroCreate<int> n
                    let leafOfNode = Array.zeroCreate<int> n

                    let clusterRelPos = Array.create cCount (Point.create 0. 0.)
                    let clusterLocalR = Array.zeroCreate<float> cCount
                    let localInnerRings = Array.create cCount ([||] : float[])
                    let localSectorSlots = Array.create cCount ([||] : (string * float * float)[])
                    let innerPad = max 4. (config.ClusterPadding * 0.4)

                    // Bottom-up leaf Poincaré layouts + tangent circle packing + exact Welzl disc enclosure
                    for pi = 0 to postorder.Count - 1 do
                        token.ThrowIfCancellationRequested()
                        let cIdx = postorder[pi]
                        let sp = specs[cIdx]
                        if sp.Children.Length = 0 then
                            let mems = sp.DirectMembers
                            let mLen = mems.Length
                            sp.TotalMembers <- mLen
                            if mLen = 0 then
                                clusterLocalR[cIdx] <- max 28. (config.ClusterPadding + 16.)
                            elif mLen = 1 then
                                let u = mems[0]
                                let nd = eff.Nodes[u]
                                leafOfNode[u] <- cIdx
                                localNodeCx[u] <- 0.
                                localNodeCy[u] <- 0.
                                localNodeW[u] <- nd.Size.Width
                                localNodeH[u] <- nd.Size.Height
                                localNodeLayer[u] <- 0
                                clusterLocalR[cIdx] <- 0.5 * Double.Hypot(nd.Size.Width, nd.Size.Height) + innerPad
                            else
                                let mutable areaSum = 0.
                                let mutable maxDiag = 0.
                                for k = 0 to mLen - 1 do
                                    let s = eff.Nodes[mems[k]].Size
                                    areaSum <- areaSum + s.Width * s.Height
                                    let d = Double.Hypot(s.Width, s.Height)
                                    if d > maxDiag then maxDiag <- d

                                let targetR = max (maxDiag * 1.15 + 18.) (1.60 * sqrt areaSum + float (min 14 mLen) * 5.2 + 16.)
                                let localBox = Bounds.create (-targetR) (-targetR) (2. * targetR) (2. * targetR)
                                let diskCfg =
                                    { PoincareDiskConfig.create localBox with
                                        RingLayering = config.RingLayering
                                        CenterBias = config.CenterBias
                                        ConformalExponent = config.ConformalExponent
                                        BarycenterSweeps = config.BarycenterSweeps
                                        MinNodeScale = 0.28
                                        Padding = innerPad
                                        SplitDenseRings = true }

                                match inducedSubgraph eff csr mems with
                                | Some subG ->
                                    match PoincareDisk.buildStateWithCancellation token diskCfg subG with
                                    | Ok diskState ->
                                        let subLayout = PoincareDisk.project diskState
                                        let byId = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
                                        for pn in subLayout.Nodes do byId[pn.Id] <- pn

                                        // Preserve exact (0, 0) Poincaré disk center so concentric hyperbolic rings & orthogonal geodesics stay exact
                                        let mutable maxReqR = PoincareDisk.diskRadius diskState
                                        for k = 0 to mLen - 1 do
                                            let u = mems[k]
                                            let nd = eff.Nodes[u]
                                            leafOfNode[u] <- cIdx
                                            match byId.TryGetValue nd.Id with
                                            | true, pn ->
                                                let hw = 0.5 * Bounds.width pn.Bounds
                                                let hh = 0.5 * Bounds.height pn.Bounds
                                                localNodeCx[u] <- pn.Center.X
                                                localNodeCy[u] <- pn.Center.Y
                                                localNodeW[u] <- 2. * hw
                                                localNodeH[u] <- 2. * hh
                                                localNodeLayer[u] <- Option.defaultValue 0 pn.Layer
                                                let req = Double.Hypot(pn.Center.X, pn.Center.Y) + Double.Hypot(hw, hh) + innerPad * 0.65
                                                if req > maxReqR then maxReqR <- req
                                            | _ ->
                                                localNodeCx[u] <- 0.
                                                localNodeCy[u] <- 0.
                                                localNodeW[u] <- nd.Size.Width
                                                localNodeH[u] <- nd.Size.Height
                                                localNodeLayer[u] <- 0

                                        localInnerRings[cIdx] <-
                                            PoincareDisk.ringRadii diskState
                                            |> List.filter (fun r -> r > 0.5 && r < maxReqR - 0.5)
                                            |> List.distinct
                                            |> List.toArray
                                        clusterLocalR[cIdx] <- max 12. maxReqR
                                    | Error _ ->
                                        for k = 0 to mLen - 1 do
                                            let u = mems[k]
                                            let ang = 2. * Math.PI * float k / float mLen
                                            leafOfNode[u] <- cIdx
                                            localNodeCx[u] <- targetR * 0.6 * cos ang
                                            localNodeCy[u] <- targetR * 0.6 * sin ang
                                            localNodeW[u] <- eff.Nodes[u].Size.Width * 0.5
                                            localNodeH[u] <- eff.Nodes[u].Size.Height * 0.5
                                            localNodeLayer[u] <- 0
                                        clusterLocalR[cIdx] <- targetR
                                | None ->
                                    clusterLocalR[cIdx] <- targetR
                        else
                            let kids = sp.Children
                            let kLen = kids.Length
                            let mutable totM = 0
                            for k = 0 to kLen - 1 do
                                totM <- totM + specs[kids[k]].TotalMembers
                            sp.TotalMembers <- totM

                            // Sort children descending by radius (tie-breaking by index) for tight tangent circle packing
                            let sortedKids = Array.copy kids
                            Array.sortInPlaceWith (fun a b ->
                                let c = compare (clusterLocalR[b]) (clusterLocalR[a])
                                if c <> 0 then c else compare a b) sortedKids

                            let childRadii = Array.init kLen (fun k -> clusterLocalR[sortedKids[k]])
                            let offsets, encR = packSiblingDiscs childRadii config.ClusterPadding
                            let childKeys = Array.init kLen (fun k -> specs[sortedKids[k]].Id)
                            for k = 0 to kLen - 1 do
                                clusterRelPos[sortedKids[k]] <- offsets[k]
                            localSectorSlots[cIdx] <- computeChildSectorSlots childKeys offsets childRadii
                            clusterLocalR[cIdx] <- encR + config.ClusterPadding

                    // Top-down similarity transform fitting root cluster 0 into Viewport
                    let rootLocalR = max 1e-6 (clusterLocalR[0])
                    let globalScale = availOuterR / rootLocalR
                    let worldCenter = Array.zeroCreate<Point> cCount
                    let worldRadius = Array.zeroCreate<float> cCount
                    worldCenter[0] <- outerCenter
                    worldRadius[0] <- rootLocalR * globalScale

                    for pi = postorder.Count - 2 downto 0 do
                        let cIdx = postorder[pi]
                        let pIdx = specs[cIdx].ParentIdx
                        let rel = clusterRelPos[cIdx]
                        let pc = worldCenter[pIdx]
                        worldCenter[cIdx] <- Point.create (pc.X + rel.X * globalScale) (pc.Y + rel.Y * globalScale)
                        worldRadius[cIdx] <- clusterLocalR[cIdx] * globalScale

                    // Position all visible nodes inside their scaled leaf discs
                    let posNodes =
                        Array.init n (fun i ->
                            let nd = eff.Nodes[i]
                            let leafIdx = leafOfNode[i]
                            let lc = worldCenter[leafIdx]
                            let cx = lc.X + localNodeCx[i] * globalScale
                            let cy = lc.Y + localNodeCy[i] * globalScale
                            let sw = max minNodePx (localNodeW[i] * globalScale)
                            let sh = max minNodePx (localNodeH[i] * globalScale)
                            let center = Point.create cx cy
                            let bounds = Bounds.create (cx - sw * 0.5) (cy - sh * 0.5) sw sh
                            { Id = nd.Id
                              Kind = nd.Kind
                              Bounds = bounds
                              Center = center
                              ParentGroup = nd.ParentGroup
                              Layer = Some (localNodeLayer[i])
                              Ports = GraphKernels.resolveNodePorts nd bounds Map.empty })

                    // Emit ClusterDisc list sorted outermost-first (ascending Depth, then ordinal Id)
                    let clusterOrder = Array.init cCount id
                    Array.sortInPlaceWith (fun a b ->
                        let c = compare (specs[a].Depth) (specs[b].Depth)
                        if c <> 0 then c else String.CompareOrdinal(specs[a].Id, specs[b].Id)) clusterOrder

                    let clusterDiscs =
                        [ for oi in 0 .. cCount - 1 ->
                            let idx = clusterOrder[oi]
                            let sp = specs[idx]
                            let parentId = if sp.ParentIdx >= 0 then Some (specs[sp.ParentIdx].Id) else None
                            let wc = worldCenter[idx]
                            let wr = worldRadius[idx]
                            let innerRings =
                                localInnerRings[idx]
                                |> Array.map (fun r -> r * globalScale)
                                |> Array.filter (fun r -> r > 0.25 && r < wr - 0.25)
                                |> Array.toList
                            let rimOff = max 0.8 (min 3.2 (wr * 0.045))
                            let rimSw = max 0.6 (min 2.4 (wr * 0.032))
                            let secArcs =
                                [ for key, a0, a1 in localSectorSlots[idx] do
                                    if a1 > a0 + 1e-4 then
                                        let arcR = wr + rimOff
                                        yield
                                            { Key = Some key
                                              Center = wc
                                              Radius = arcR
                                              StartAngle = a0
                                              EndAngle = a1
                                              StrokeWidth = rimSw
                                              Path = Hyperbolic2D.circularArcPath wc arcR a0 a1 } ]
                            { Id = sp.Id
                              Parent = parentId
                              Depth = sp.Depth
                              Center = wc
                              Radius = wr
                              IsLeafCluster = (sp.Children.Length = 0)
                              MemberCount = sp.TotalMembers
                              InnerRingRadii = innerRings
                              SectorArcs = secArcs } ]

                    // Emit PositionedGroup rectangles matching explicit DiagramGroup discs when present, else tight child bounds
                    let explicitSpecById = Dictionary<string, int>(StringComparer.Ordinal)
                    for idx = 0 to cCount - 1 do
                        if specs[idx].IsExplicitGroup then
                            explicitSpecById[specs[idx].Id] <- idx

                    let groups =
                        if eff.Groups.Length = 0 then
                            []
                        elif explicitSpecById.Count = eff.Groups.Length then
                            [ for gi in 0 .. eff.Groups.Length - 1 ->
                                let g = eff.Groups[gi]
                                let cIdx = explicitSpecById[g.Id]
                                let cc = worldCenter[cIdx]
                                let cr = worldRadius[cIdx]
                                let outer = Bounds.create (cc.X - cr) (cc.Y - cr) (2. * cr) (2. * cr)
                                let pad = g.Padding
                                let iw = max 0. (2. * cr - pad.Left - pad.Right)
                                let ih = max 0. (2. * cr - pad.Top - pad.Bottom)
                                let inner = Bounds.create (Bounds.x outer + min cr pad.Left) (Bounds.y outer + min cr pad.Top) iw ih
                                { Id = g.Id
                                  ParentGroup = g.ParentGroup
                                  Bounds = outer
                                  InnerBounds = inner
                                  Collapsed = false
                                  Depth = eff.GroupDepth[gi] } ]
                        else
                            GraphKernels.computeGroupBounds eff (posNodes |> Array.map (fun pn -> pn.Bounds))

                    // Hybrid Edge Routing: intra-cluster Poincaré geodesic arcs + inter-cluster LCA bundled splines
                    let lookupAnchor (node: PositionedNode) (ep: Endpoint) (toward: Point) : Point =
                        match ep.PortId with
                        | Some pid ->
                            node.Ports
                            |> List.tryFind (fun p -> p.Id = pid)
                            |> Option.map (fun p -> p.Anchor)
                            |> Option.defaultValue (GraphKernels.clipRectBoundary node.Bounds toward)
                        | None -> GraphKernels.clipRectBoundary node.Bounds toward

                    let upChain = ResizeArray<int>(8)
                    let downChain = ResizeArray<int>(8)
                    let m = eff.Associations.Length
                    let outAssocs = Array.zeroCreate<RoutedAssociation> m

                    for e = 0 to m - 1 do
                        SimdMath.check token e
                        let a = eff.Associations[e]
                        let ui = eff.ReversedIdMap[a.Source.NodeId]
                        let vi = eff.ReversedIdMap[a.Target.NodeId]
                        let uNode = posNodes[ui]
                        let vNode = posNodes[vi]
                        let hasLabels = a.SourceLabel.IsSome || a.CenterLabel.IsSome || a.TargetLabel.IsSome

                        if ui = vi then
                            let b = uNode.Bounds
                            let rx = Bounds.x b + Bounds.width b
                            let ty = Bounds.y b
                            let p0 = Point.create (rx - min 10. (Bounds.width b * 0.25)) ty
                            let p3 = Point.create rx (ty + min 10. (Bounds.height b * 0.35))
                            let c1 = Point.create (rx + 16.) (ty - 18.)
                            let c2 = Point.create (rx + 20.) (ty + 14.)
                            let mid = Point.create (rx + 14.) (ty - 3.)
                            let wps = [ p0; mid; p3 ]
                            let path = Path.ofCommands [ MoveTo p0; CubicTo(c1, c2, p3) ]
                            let sLbl, mLbl, tLbl =
                                if hasLabels then GraphKernels.placeLabelsOnWaypoints a [| p0; mid; p3 |]
                                else None, None, None
                            outAssocs[e] <-
                                { Id = a.Id
                                  Source = a.Source
                                  Target = a.Target
                                  Kind = a.Kind
                                  Waypoints = wps
                                  Path = path
                                  ReversedInCycle = false
                                  SourceLabel = sLbl
                                  CenterLabel = mLbl
                                  TargetLabel = tLbl }
                        else
                            let lu = leafOfNode[ui]
                            let lv = leafOfNode[vi]
                            if lu = lv then
                                let pStart = lookupAnchor uNode a.Source vNode.Center
                                let pEnd = lookupAnchor vNode a.Target uNode.Center
                                let path, wps = Hyperbolic2D.geodesicArc (worldCenter[lu]) (worldRadius[lu]) pStart pEnd
                                let sLbl, mLbl, tLbl =
                                    if hasLabels then GraphKernels.placeLabelsOnWaypoints a (List.toArray wps)
                                    else None, None, None
                                outAssocs[e] <-
                                    { Id = a.Id
                                      Source = a.Source
                                      Target = a.Target
                                      Kind = a.Kind
                                      Waypoints = wps
                                      Path = path
                                      ReversedInCycle = false
                                      SourceLabel = sLbl
                                      CenterLabel = mLbl
                                      TargetLabel = tLbl }
                            else
                                upChain.Clear()
                                downChain.Clear()
                                let mutable cu = lu
                                let mutable cv = lv
                                while specs[cu].Depth > specs[cv].Depth do
                                    upChain.Add cu
                                    cu <- specs[cu].ParentIdx
                                while specs[cv].Depth > specs[cu].Depth do
                                    downChain.Add cv
                                    cv <- specs[cv].ParentIdx
                                while cu <> cv && cu >= 0 && cv >= 0 do
                                    upChain.Add cu
                                    downChain.Add cv
                                    cu <- specs[cu].ParentIdx
                                    cv <- specs[cv].ParentIdx
                                let lca = if cu >= 0 then cu else 0

                                let interior = ResizeArray<Point>(upChain.Count + downChain.Count + 1)
                                for k = 0 to upChain.Count - 1 do
                                    interior.Add(worldCenter[upChain[k]])
                                if lca <> 0 || (upChain.Count + downChain.Count < 2) then
                                    interior.Add(worldCenter[lca])
                                else
                                    let topU = worldCenter[upChain[upChain.Count - 1]]
                                    let topV = worldCenter[downChain[downChain.Count - 1]]
                                    let rc = worldCenter[0]
                                    interior.Add(Point.create (0.5 * rc.X + 0.25 * (topU.X + topV.X)) (0.5 * rc.Y + 0.25 * (topU.Y + topV.Y)))
                                for k = downChain.Count - 1 downto 0 do
                                    interior.Add(worldCenter[downChain[k]])

                                let firstAim = if interior.Count > 0 then interior[0] else vNode.Center
                                let lastAim = if interior.Count > 0 then interior[interior.Count - 1] else uNode.Center
                                let pStart = lookupAnchor uNode a.Source firstAim
                                let pEnd = lookupAnchor vNode a.Target lastAim

                                let ctrlLen = interior.Count + 2
                                let ctrlPts = Array.zeroCreate<Point> ctrlLen
                                ctrlPts[0] <- pStart
                                for k = 0 to interior.Count - 1 do
                                    ctrlPts[k + 1] <- interior[k]
                                ctrlPts[ctrlLen - 1] <- pEnd

                                let beta = config.BundleBeta
                                let path, wpsArr =
                                    if beta <= 1e-6 || ctrlLen <= 2 then
                                        Path.ofCommands [ MoveTo pStart; LineTo pEnd ], [| pStart; pEnd |]
                                    else
                                        let optPts = [ for k in 0 .. ctrlLen - 1 -> Some (ctrlPts[k]) ]
                                        let p = Curves.line (CurveSpec.Bundle beta) optPts
                                        let straightened =
                                            Array.init ctrlLen (fun k ->
                                                if k = 0 then pStart
                                                elif k = ctrlLen - 1 then pEnd
                                                else
                                                    let t = float k / float (ctrlLen - 1)
                                                    let chordX = pStart.X + (pEnd.X - pStart.X) * t
                                                    let chordY = pStart.Y + (pEnd.Y - pStart.Y) * t
                                                    Point.create
                                                        (chordX + (ctrlPts[k].X - chordX) * beta)
                                                        (chordY + (ctrlPts[k].Y - chordY) * beta))
                                        p, straightened

                                let sLbl, mLbl, tLbl =
                                    if hasLabels then GraphKernels.placeLabelsOnWaypoints a wpsArr
                                    else None, None, None
                                outAssocs[e] <-
                                    { Id = a.Id
                                      Source = a.Source
                                      Target = a.Target
                                      Kind = a.Kind
                                      Waypoints = Array.toList wpsArr
                                      Path = path
                                      ReversedInCycle = false
                                      SourceLabel = sLbl
                                      CenterLabel = mLbl
                                      TargetLabel = tLbl }

                    let nList = Array.toList posNodes
                    let routed = Array.toList outAssocs
                    let finalBounds = GraphKernels.computeDiagramBounds config.Viewport nList groups routed
                    Ok
                        { Layout =
                            { Bounds = finalBounds
                              Nodes = nList
                              Groups = groups
                              Associations = routed }
                          Clusters = clusterDiscs
                          OuterCenter = worldCenter[0]
                          OuterRadius = worldRadius[0] }

    /// <summary>Computes a multi-scale constellation layout and its enclosing <c>ClusterDisc</c> hierarchy with cooperative cancellation.</summary>
    let computeWithCancellation
        (token: System.Threading.CancellationToken)
        (config: ConstellationConfig)
        (graph: DiagramGraph)
        : Result<ConstellationOutput, LayoutError list> =
        computeCore token 2.0 config graph

    /// <summary>Computes a multi-scale constellation layout and its enclosing <c>ClusterDisc</c> hierarchy without cancellation.</summary>
    let compute (config: ConstellationConfig) (graph: DiagramGraph) : Result<ConstellationOutput, LayoutError list> =
        computeWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Computes the 2D diagram layout of a multi-scale constellation with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: ConstellationConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        computeWithCancellation token config graph |> Result.map (fun o -> o.Layout)

    /// <summary>Computes the 2D diagram layout of a multi-scale constellation without cancellation.</summary>
    let layout (config: ConstellationConfig) (graph: DiagramGraph) : Result<DiagramLayout2D, LayoutError list> =
        layoutWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Embeds internal multi-scale constellation miniatures (and optional outer rim progress gauges) directly inside the inscribed discs of matching nodes in <c>hostLayout</c> with cooperative cancellation.</summary>
    let inlaySubgraphsWithCancellation
        (token: System.Threading.CancellationToken)
        (config: ConstellationConfig)
        (rimShares: Map<string, (string * float) list>)
        (subgraphs: Map<string, DiagramGraph>)
        (hostLayout: DiagramLayout2D)
        : Result<DiscInlay list, LayoutError list> =
        token.ThrowIfCancellationRequested()
        let out = ResizeArray<DiscInlay>()
        let mutable firstErr : LayoutError list option = None
        for hostNode in hostLayout.Nodes do
            if firstErr.IsNone then
                token.ThrowIfCancellationRequested()
                let subOpt = Map.tryFind hostNode.Id subgraphs
                let sharesOpt = Map.tryFind hostNode.Id rimShares
                if subOpt.IsSome || sharesOpt.IsSome then
                    let discR = 0.5 * min (Bounds.width hostNode.Bounds) (Bounds.height hostNode.Bounds)
                    if discR > 1e-3 then
                        let gaugeSw = max 0.8 (min 4.5 (discR * 0.11))
                        let gaugeOff = max 1.2 (min 8.0 (discR * 0.16)) + 0.5 * gaugeSw
                        let gauge =
                            match sharesOpt with
                            | Some sh when not sh.IsEmpty -> Hyperbolic2D.rimGaugeArcs hostNode.Center discR gaugeOff gaugeSw sh
                            | _ -> []
                        match subOpt with
                        | None ->
                            out.Add
                                { HostNodeId = hostNode.Id
                                  Center = hostNode.Center
                                  Radius = discR
                                  Clusters = []
                                  Nodes = []
                                  Associations = []
                                  RimGauge = gauge }
                        | Some subG ->
                            let insetR = max 1.5 (discR * 0.92)
                            let localBox = Bounds.create (hostNode.Center.X - insetR) (hostNode.Center.Y - insetR) (2. * insetR) (2. * insetR)
                            // Canonicalize internal node sizes inside miniature inlays so 50+ functions never bloat local boxes
                            let localPad = max 1.2 (min config.ClusterPadding (insetR * 0.12))
                            let minPx = min 2.0 (max 0.55 (insetR * 0.04))
                            let localCfg =
                                { config with
                                    Viewport = localBox
                                    ClusterPadding = localPad }
                            match computeCore token minPx localCfg subG with
                            | Error errs -> firstErr <- Some errs
                            | Ok subOut ->
                                out.Add
                                    { HostNodeId = hostNode.Id
                                      Center = hostNode.Center
                                      Radius = discR
                                      Clusters = subOut.Clusters
                                      Nodes = subOut.Layout.Nodes
                                      Associations = subOut.Layout.Associations
                                      RimGauge = gauge }
        match firstErr with
        | Some errs -> Error errs
        | None -> Ok(Seq.toList out)

    /// <summary>Embeds internal multi-scale constellation miniatures (and optional outer rim progress gauges) directly inside the inscribed discs of matching nodes in <c>hostLayout</c> without cancellation.</summary>
    let inlaySubgraphs
        (config: ConstellationConfig)
        (rimShares: Map<string, (string * float) list>)
        (subgraphs: Map<string, DiagramGraph>)
        (hostLayout: DiagramLayout2D)
        : Result<DiscInlay list, LayoutError list> =
        inlaySubgraphsWithCancellation System.Threading.CancellationToken.None config rimShares subgraphs hostLayout

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
    /// <summary>Directory-tree + greedy Newman-Girvan file modularity agglomeration with bottom-up tree DP and sparse-satellite coalescing (<c>minClusterFraction</c> in <c>[0, 0.5]</c>).</summary>
    | DirectoryModularity of resolution: float * minClusterFraction: float

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
      /// <summary>Outer Euclidean packing radius of the cluster disc in layout units (includes reserved annular clearance for <c>SectorArcs</c>).</summary>
      Radius: float
      /// <summary>Tight inner Welzl child-enclosure radius for dashed parent directory rims (<c>InnerRadius = Radius</c> on leaf discs).</summary>
      InnerRadius: float
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
      BarycenterSweeps: int
      /// <summary>Topological hub-vs-stardust node sizing rule inside each leaf Poincaré sub-disc.</summary>
      SizeBy: PoincareSizeBy
      /// <summary>Angular node ordering policy inside each leaf Poincaré sub-disc.</summary>
      RingOrder: PoincareRingOrder }

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
          BarycenterSweeps = 6
          SizeBy = PoincareSizeBy.Uniform
          RingOrder = PoincareRingOrder.Barycenter }

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

    type private FileModBucket =
        { Name: string
          Members: ResizeArray<int>
          mutable InsideEdges: int
          mutable TotalDegree: int }

    type private DirModTree =
        { Name: string
          Parent: DirModTree option
          Dirs: Dictionary<string, DirModTree>
          Files: Dictionary<string, FileModBucket>
          FileLinks: Dictionary<string, int>
          mutable InsideEdges: int
          mutable TotalDegree: int
          mutable LooseInside: int
          mutable LooseDegree: int }

    type private ModPart =
        { Label: string
          Home: string
          Kind: string
          Files: string list
          Members: int[]
          Inside: int
          Degree: int
          Score: float }

    type private AggrFileCluster =
        { Files: ResizeArray<FileModBucket>
          mutable Inside: int
          mutable Degree: int
          mutable Alive: bool }

    type private DirHierarchyEntry =
        { Path: string
          SubDirs: ResizeArray<string>
          Clusters: ResizeArray<int> }

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

    // Exact directory-tree + greedy Newman-Girvan file modularity agglomeration + bottom-up tree DP (matching &Atlas An + On).
    let private buildDirectoryModularitySpecs
        (eff: EffectiveDiagram)
        (csr: CsrGraph)
        (reserved: HashSet<string>)
        (specs: ResizeArray<ClusterNodeSpec>)
        (resolution: float)
        (minFrac: float)
        : unit =
        let n = eff.Nodes.Length
        // Reconstruct slash-delimited file path per node from ParentGroup chain (or slash/underscore prefix of Id when ungrouped)
        let gParent = Dictionary<string, string option>(StringComparer.Ordinal)
        for gi = 0 to eff.Groups.Length - 1 do
            gParent[eff.Groups[gi].Id] <- eff.Groups[gi].ParentGroup
        let rec fullGroupPath (gid: string) (acc: string) =
            match gParent.TryGetValue gid with
            | true, Some pid -> fullGroupPath pid (pid + "/" + acc)
            | _ -> acc

        let filePathOf =
            Array.init n (fun i ->
                let nd = eff.Nodes[i]
                match nd.ParentGroup with
                | Some gid -> fullGroupPath gid gid
                | None ->
                    let idStr = nd.Id
                    let slashIdx = idStr.LastIndexOf('/')
                    if slashIdx > 0 then idStr.Substring(0, slashIdx)
                    else
                        let usIdx = idStr.IndexOf('_')
                        if usIdx > 0 then idStr.Substring(0, usIdx)
                        else "(no file)")

        // Count undirected non-self edges and node degrees
        let mutable edgeCount = 0
        let nodeDeg = Array.zeroCreate<int> n
        let nonSelfEdges = ResizeArray<struct(int * int)>(csr.EdgeCount)
        for e = 0 to csr.EdgeCount - 1 do
            let u = csr.Src[e]
            let v = csr.Dst[e]
            if u <> v then
                edgeCount <- edgeCount + 1
                nodeDeg[u] <- nodeDeg[u] + 1
                nodeDeg[v] <- nodeDeg[v] + 1
                nonSelfEdges.Add(struct(u, v))

        let addSpec id parentIdx depth directMembers =
            let idx = specs.Count
            specs.Add
                { Id = id
                  ParentIdx = parentIdx
                  Depth = depth
                  Children = [||]
                  DirectMembers = directMembers
                  TotalMembers = directMembers.Length
                  IsExplicitGroup = false }
            idx

        if edgeCount = 0 || n < 2 then
            let rootId = uniqueSyntheticId reserved "constellation"
            addSpec rootId -1 0 (Array.init n id) |> ignore
        else
            let mkDir name parent =
                { Name = name
                  Parent = parent
                  Dirs = Dictionary<string, DirModTree>(StringComparer.Ordinal)
                  Files = Dictionary<string, FileModBucket>(StringComparer.Ordinal)
                  FileLinks = Dictionary<string, int>(StringComparer.Ordinal)
                  InsideEdges = 0
                  TotalDegree = 0
                  LooseInside = 0
                  LooseDegree = 0 }

            let rootDir = mkDir "" None
            let nodeFileRef = Array.zeroCreate<DirModTree * FileModBucket> n

            for i = 0 to n - 1 do
                let raw = filePathOf[i]
                let segs = raw.Split('/') |> Array.filter (fun s -> s.Length > 0)
                let dirSegs, fileName =
                    if segs.Length = 0 then [||], "(no file)"
                    else segs[0 .. segs.Length - 2], segs[segs.Length - 1]
                let mutable cur = rootDir
                for d = 0 to dirSegs.Length - 1 do
                    let dn = dirSegs[d]
                    match cur.Dirs.TryGetValue dn with
                    | true, sub -> cur <- sub
                    | _ ->
                        let sub = mkDir dn (Some cur)
                        cur.Dirs[dn] <- sub
                        cur <- sub
                let fb =
                    match cur.Files.TryGetValue fileName with
                    | true, existing -> existing
                    | _ ->
                        let created =
                            { Name = fileName
                              Members = ResizeArray<int>()
                              InsideEdges = 0
                              TotalDegree = 0 }
                        cur.Files[fileName] <- created
                        created
                fb.Members.Add i
                let dg = nodeDeg[i]
                fb.TotalDegree <- fb.TotalDegree + dg
                cur.LooseDegree <- cur.LooseDegree + dg
                nodeFileRef[i] <- (cur, fb)
                let mutable anc : DirModTree option = Some cur
                while anc.IsSome do
                    anc.Value.TotalDegree <- anc.Value.TotalDegree + dg
                    anc <- anc.Value.Parent

            for k = 0 to nonSelfEdges.Count - 1 do
                let struct(u, v) = nonSelfEdges[k]
                let dU, fU = nodeFileRef[u]
                let dV, fV = nodeFileRef[v]
                if Object.ReferenceEquals(fU, fV) then
                    fU.InsideEdges <- fU.InsideEdges + 1
                if Object.ReferenceEquals(dU, dV) then
                    dU.LooseInside <- dU.LooseInside + 1
                    if not (Object.ReferenceEquals(fU, fV)) then
                        let pairKey =
                            if String.CompareOrdinal(fU.Name, fV.Name) < 0 then fU.Name + "\t" + fV.Name
                            else fV.Name + "\t" + fU.Name
                        match dU.FileLinks.TryGetValue pairKey with
                        | true, cnt -> dU.FileLinks[pairKey] <- cnt + 1
                        | _ -> dU.FileLinks[pairKey] <- 1
                let seenAnc = HashSet<DirModTree>()
                let mutable pU : DirModTree option = Some dU
                while pU.IsSome do
                    seenAnc.Add pU.Value |> ignore
                    pU <- pU.Value.Parent
                let mutable lcaDir = dV
                while not (seenAnc.Contains lcaDir) && lcaDir.Parent.IsSome do
                    lcaDir <- lcaDir.Parent.Value
                let mutable pL : DirModTree option = Some lcaDir
                while pL.IsSome do
                    pL.Value.InsideEdges <- pL.Value.InsideEdges + 1
                    pL <- pL.Value.Parent

            let mEdgesF = float edgeCount
            let inline modScore (ins: int) (deg: int) : float =
                let ratio = float deg / (2. * mEdgesF)
                float ins / mEdgesF - resolution * ratio * ratio

            let rec collectAllMembers (d: DirModTree) (acc: ResizeArray<int>) =
                for kv in d.Files do
                    let mems = kv.Value.Members
                    for i = 0 to mems.Count - 1 do acc.Add mems[i]
                for kv in d.Dirs do
                    collectAllMembers kv.Value acc

            // Greedy Newman-Girvan agglomeration among sibling files inside directory d
            let agglomerateFiles (d: DirModTree) : AggrFileCluster[] =
                let fileArr = [| for kv in d.Files -> kv.Value |]
                Array.sortInPlaceWith (fun (a: FileModBucket) (b: FileModBucket) -> String.CompareOrdinal(a.Name, b.Name)) fileArr
                let fLen = fileArr.Length
                let idxByName = Dictionary<string, int>(fLen, StringComparer.Ordinal)
                let clusters =
                    Array.init fLen (fun idx ->
                        idxByName[fileArr[idx].Name] <- idx
                        let fs = ResizeArray<FileModBucket>(2)
                        fs.Add fileArr[idx]
                        { Files = fs
                          Inside = fileArr[idx].InsideEdges
                          Degree = fileArr[idx].TotalDegree
                          Alive = true })
                let links = Dictionary<uint64, int>()
                let inline packPair (a: int) (b: int) =
                    let lo = min a b
                    let hi = max a b
                    (uint64 lo <<< 32) ||| uint64 (uint32 hi)
                for kv in d.FileLinks do
                    let parts = kv.Key.Split('\t')
                    if parts.Length = 2 then
                        let ia = idxByName[parts[0]]
                        let ib = idxByName[parts[1]]
                        if ia <> ib then links[packPair ia ib] <- kv.Value

                let twoM2 = 2. * mEdgesF * mEdgesF
                let mutable merging = true
                while merging do
                    let mutable bestGain = 1e-12
                    let mutable bestI = -1
                    let mutable bestW = -1
                    let mutable bestE = 0
                    for kv in links do
                        let ia = int (kv.Key >>> 32)
                        let ib = int (uint32 kv.Key)
                        if clusters[ia].Alive && clusters[ib].Alive then
                            let gain = float kv.Value / mEdgesF - resolution * float clusters[ia].Degree * float clusters[ib].Degree / twoM2
                            if gain > bestGain then
                                bestGain <- gain
                                bestI <- ia
                                bestW <- ib
                                bestE <- kv.Value
                    if bestI < 0 then
                        merging <- false
                    else
                        let p = clusters[bestI]
                        let w = clusters[bestW]
                        for k = 0 to w.Files.Count - 1 do p.Files.Add w.Files[k]
                        p.Inside <- p.Inside + w.Inside + bestE
                        p.Degree <- p.Degree + w.Degree
                        w.Alive <- false
                        links.Remove(packPair bestI bestW) |> ignore
                        let snapshot = [| for kv in links -> kv.Key, kv.Value |]
                        for key, cnt in snapshot do
                            let x = int (key >>> 32)
                            let z = int (uint32 key)
                            if x = bestW || z = bestW then
                                links.Remove key |> ignore
                                let other = if x = bestW then z else x
                                if other <> bestI && clusters[other].Alive then
                                    let nk = packPair bestI other
                                    match links.TryGetValue nk with
                                    | true, cur -> links[nk] <- cur + cnt
                                    | _ -> links[nk] <- cnt

                clusters |> Array.filter (fun c -> c.Alive)

            let minSize = max 1 (int (ceil (minFrac * float n)))
            let inline isTooSmall (p: ModPart) = minSize > 1 && p.Members.Length < minSize
            let inline isStrong (p: ModPart) = (not (isTooSmall p)) && (p.Kind = "dir" || 4 * p.Inside > p.Degree)

            let coalesceWeakAtHome (parts: ModPart list) (home: string) : ModPart list =
                let inline atHome (p: ModPart) =
                    String.Equals(p.Home, home, StringComparison.Ordinal) && (p.Kind <> "dir" || isTooSmall p)
                let kept = parts |> List.filter (fun p -> (not (atHome p)) || isStrong p)
                let weak = parts |> List.filter (fun p -> atHome p && not (isStrong p))
                if weak.Length < 2 then
                    parts
                else
                    let allFiles = weak |> List.forall (fun p -> p.Kind <> "dir" && String.Equals(p.Home, home, StringComparison.Ordinal))
                    let ins = weak |> List.sumBy (fun p -> p.Inside)
                    let deg = weak |> List.sumBy (fun p -> p.Degree)
                    let mems = [| for p in weak do yield! p.Members |]
                    let fls = [ for p in weak do yield! p.Files ]
                    let merged =
                        { Label = if home.Length = 0 then "other" else home + "other"
                          Home = home
                          Kind = if allFiles then "other" else "rest"
                          Files = fls
                          Members = mems
                          Inside = ins
                          Degree = deg
                          Score = modScore ins deg }
                    kept @ [ merged ]

            let rec solveDirDP (d0: DirModTree) (prefix0: string) (home: string) : float * ModPart list =
                let mutable d = d0
                let mutable prefix = prefix0
                while d.Dirs.Count = 1 && d.Files.Count = 0 do
                    let mutable child = d
                    for kv in d.Dirs do
                        child <- kv.Value
                    prefix <- prefix + child.Name + "/"
                    d <- child

                let allBuf = ResizeArray<int>()
                collectAllMembers d allBuf
                let wholePart =
                    { Label = if prefix.Length = 0 then "all" else prefix
                      Home = home
                      Kind = "dir"
                      Files = []
                      Members = allBuf.ToArray()
                      Inside = d.InsideEdges
                      Degree = d.TotalDegree
                      Score = modScore d.InsideEdges d.TotalDegree }

                let mutable bestScore = wholePart.Score
                let mutable bestParts = [ wholePart ]

                let subKeys = [| for k in d.Dirs.Keys -> k |]
                Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(a, b)) subKeys
                let subParts =
                    [ for sk in subKeys do
                        let _, sp = solveDirDP (d.Dirs[sk]) (prefix + sk + "/") prefix
                        yield! sp ]

                let tryCandidate (cand: ModPart list) =
                    let sc = cand |> List.sumBy (fun p -> p.Score)
                    if sc > bestScore + 1e-12 then
                        bestScore <- sc
                        bestParts <- cand

                if subKeys.Length > 0 then
                    let looseList =
                        if d.Files.Count > 0 then
                            let lMems = ResizeArray<int>()
                            let lNames = ResizeArray<string>()
                            for kv in d.Files do
                                lNames.Add kv.Key
                                let ms = kv.Value.Members
                                for idx = 0 to ms.Count - 1 do lMems.Add ms[idx]
                            [ { Label = if prefix.Length = 0 then "files" else prefix + "files"
                                Home = prefix
                                Kind = "files"
                                Files = Seq.toList lNames
                                Members = lMems.ToArray()
                                Inside = d.LooseInside
                                Degree = d.LooseDegree
                                Score = modScore d.LooseInside d.LooseDegree } ]
                        else
                            []
                    tryCandidate (coalesceWeakAtHome (subParts @ looseList) prefix)

                if d.Files.Count > 1 then
                    let aggr = agglomerateFiles d
                    let fileClusterParts =
                        [ for c in aggr ->
                            let fNames = [ for fb in c.Files -> fb.Name ]
                            let fMems = [| for fb in c.Files do for k in 0 .. fb.Members.Count - 1 do yield fb.Members[k] |]
                            let primary = c.Files[0].Name
                            let lbl =
                                if c.Files.Count = 1 then prefix + primary
                                else sprintf "%s%s+%d" prefix primary (c.Files.Count - 1)
                            { Label = lbl
                              Home = prefix
                              Kind = "files"
                              Files = fNames
                              Members = fMems
                              Inside = c.Inside
                              Degree = c.Degree
                              Score = modScore c.Inside c.Degree } ]
                    tryCandidate (coalesceWeakAtHome (subParts @ fileClusterParts) prefix)

                bestScore, bestParts

            let _, rawParts = solveDirDP rootDir "" ""
            let parts = rawParts |> List.filter (fun p -> p.Members.Length > 0) |> List.toArray
            if parts.Length = 0 then
                let rootId = uniqueSyntheticId reserved "constellation"
                addSpec rootId -1 0 (Array.init n id) |> ignore
            elif parts.Length = 1 then
                let rootId = uniqueSyntheticId reserved (if parts[0].Label.Length > 0 then parts[0].Label else "constellation")
                addSpec rootId -1 0 parts[0].Members |> ignore
            else
                // Reconstruct nested directory-group hull hierarchy above the winning modularity clusters (matching &Atlas On)
                let parentDirPath (path: string) : string option =
                    if path.Length = 0 then None
                    else
                        let trimmed = if path.EndsWith("/") then path.Substring(0, path.Length - 1) else path
                        let slash = trimmed.LastIndexOf('/')
                        if slash < 0 then Some "" else Some(trimmed.Substring(0, slash + 1))

                let dirEntries = Dictionary<string, DirHierarchyEntry>(StringComparer.Ordinal)
                let rec ensureDir (path: string) : DirHierarchyEntry =
                    match dirEntries.TryGetValue path with
                    | true, entry -> entry
                    | _ ->
                        let created =
                            { Path = path
                              SubDirs = ResizeArray<string>()
                              Clusters = ResizeArray<int>() }
                        dirEntries[path] <- created
                        match parentDirPath path with
                        | Some pPath ->
                            let pEntry = ensureDir pPath
                            pEntry.SubDirs.Add path
                        | None -> ()
                        created

                for cIdx = 0 to parts.Length - 1 do
                    let homeEntry = ensureDir parts[cIdx].Home
                    homeEntry.Clusters.Add cIdx

                let rec emitDirTree (path: string) (parentSpecIdx: int) (depth: int) : int option =
                    let entry = dirEntries[path]
                    let subDirs = entry.SubDirs.ToArray()
                    Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(a, b)) subDirs
                    let clIdxs = entry.Clusters.ToArray()
                    Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(parts[a].Label, parts[b].Label)) clIdxs

                    let totalItems = subDirs.Length + clIdxs.Length
                    if totalItems = 0 then
                        None
                    elif totalItems = 1 && parentSpecIdx >= 0 then
                        // Collapse unary intermediate directory wrapper
                        if subDirs.Length = 1 then emitDirTree subDirs[0] parentSpecIdx depth
                        else
                            let p = parts[clIdxs[0]]
                            let cid = uniqueSyntheticId reserved p.Label
                            Some(addSpec cid parentSpecIdx depth p.Members)
                    else
                        let dirName = if path.Length = 0 then "constellation" else path
                        let myId = uniqueSyntheticId reserved dirName
                        let myIdx = addSpec myId parentSpecIdx depth [||]
                        let kids = ResizeArray<int>(totalItems)
                        for k = 0 to subDirs.Length - 1 do
                            match emitDirTree subDirs[k] myIdx (depth + 1) with
                            | Some childIdx -> kids.Add childIdx
                            | None -> ()
                        for k = 0 to clIdxs.Length - 1 do
                            let p = parts[clIdxs[k]]
                            let cid = uniqueSyntheticId reserved p.Label
                            kids.Add(addSpec cid myIdx (depth + 1) p.Members)
                        specs[myIdx].Children <- kids.ToArray()
                        Some myIdx

                ensureDir "" |> ignore
                emitDirTree "" -1 0 |> ignore

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
            | ConstellationClusterMode.DirectoryModularity(res, minFrac) ->
                Double.IsFinite res && res >= 0. && Double.IsFinite minFrac && minFrac >= 0. && minFrac <= 0.5
        let sizeOk =
            match config.SizeBy with
            | PoincareSizeBy.Uniform -> true
            | PoincareSizeBy.InDegree m
            | PoincareSizeBy.TransitiveReach m
            | PoincareSizeBy.DagDepth m -> Double.IsFinite m && m >= 1. && m <= 12.
        if not (Double.IsFinite vx && Double.IsFinite vy && Double.IsFinite vw && Double.IsFinite vh)
           || vw <= 0. || vh <= 0. then
            Error [ InvalidObservation "ConstellationConfig Viewport must have finite coordinates and positive width and height." ]
        elif not modeOk then
            Error [ InvalidObservation "ConstellationConfig ClusterMode requires positive maxGroupSize, finite nonnegative resolution and minClusterFraction in [0.0, 0.5]." ]
        elif not sizeOk then
            Error [ InvalidObservation "ConstellationConfig SizeBy maxScale must be finite and in [1.0, 12.0]." ]
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
                    | ConstellationClusterMode.DirectoryModularity(res, minFrac) ->
                        buildDirectoryModularitySpecs eff csr reserved specs res minFrac
                        Ok()

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
                    let clusterTightR = Array.zeroCreate<float> cCount
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
                                let r0 = max 28. (config.ClusterPadding + 16.)
                                clusterLocalR[cIdx] <- r0
                                clusterTightR[cIdx] <- r0
                            elif mLen = 1 then
                                let u = mems[0]
                                let nd = eff.Nodes[u]
                                leafOfNode[u] <- cIdx
                                localNodeCx[u] <- 0.
                                localNodeCy[u] <- 0.
                                localNodeW[u] <- nd.Size.Width
                                localNodeH[u] <- nd.Size.Height
                                localNodeLayer[u] <- 0
                                let r1 = 0.5 * Double.Hypot(nd.Size.Width, nd.Size.Height) + innerPad
                                clusterLocalR[cIdx] <- r1
                                clusterTightR[cIdx] <- r1
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
                                        SplitDenseRings = true
                                        SizeBy = config.SizeBy
                                        RingOrder = config.RingOrder }

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
                                        let rLeaf = max 12. maxReqR
                                        clusterLocalR[cIdx] <- rLeaf
                                        clusterTightR[cIdx] <- rLeaf
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
                                        clusterTightR[cIdx] <- targetR
                                | None ->
                                    clusterLocalR[cIdx] <- targetR
                                    clusterTightR[cIdx] <- targetR
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
                            // Reserve dedicated annular lane outside tight Welzl radius encR so SectorArcs never collide with sibling/parent rims (matching &Atlas b(E, fileRings))
                            let bandLane = max (config.ClusterPadding + 4.) (min 26. (encR * 0.08 + config.ClusterPadding))
                            clusterTightR[cIdx] <- encR
                            clusterLocalR[cIdx] <- encR + bandLane

                    // Top-down similarity transform fitting root cluster 0 into Viewport
                    let rootLocalR = max 1e-6 (clusterLocalR[0])
                    let globalScale = availOuterR / rootLocalR
                    let worldCenter = Array.zeroCreate<Point> cCount
                    let worldRadius = Array.zeroCreate<float> cCount
                    let worldTightRadius = Array.zeroCreate<float> cCount
                    worldCenter[0] <- outerCenter
                    worldRadius[0] <- rootLocalR * globalScale
                    worldTightRadius[0] <- clusterTightR[0] * globalScale

                    for pi = postorder.Count - 2 downto 0 do
                        let cIdx = postorder[pi]
                        let pIdx = specs[cIdx].ParentIdx
                        let rel = clusterRelPos[cIdx]
                        let pc = worldCenter[pIdx]
                        worldCenter[cIdx] <- Point.create (pc.X + rel.X * globalScale) (pc.Y + rel.Y * globalScale)
                        worldRadius[cIdx] <- clusterLocalR[cIdx] * globalScale
                        worldTightRadius[cIdx] <- clusterTightR[cIdx] * globalScale

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
                            let wt = worldTightRadius[idx]
                            let isLeaf = (sp.Children.Length = 0)
                            let innerRings =
                                localInnerRings[idx]
                                |> Array.map (fun r -> r * globalScale)
                                |> Array.filter (fun r -> r > 0.25 && r < wr - 0.25)
                                |> Array.toList
                            let arcR = if isLeaf then wr + max 0.8 (min 2.8 (wr * 0.04)) else 0.5 * (wt + wr)
                            let rimSw = max 0.6 (min 2.4 (max 0.8 ((wr - wt) * 0.38)))
                            let secArcs =
                                [ for key, a0, a1 in localSectorSlots[idx] do
                                    if a1 > a0 + 1e-4 then
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
                              InnerRadius = wt
                              IsLeafCluster = isLeaf
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

                    // Hybrid Edge Routing: intra-cluster Poincaré geodesic arcs + inter-cluster ancestor-only midpoint QuadraticTo splines (matching &Atlas ft + vn)
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
                                // Start at enclosing parent directory discs (specs[lu].ParentIdx, specs[lv].ParentIdx), NEVER leaf centers lu/lv!
                                upChain.Clear()
                                downChain.Clear()
                                let mutable cu = max 0 (specs[lu].ParentIdx)
                                let mutable cv = max 0 (specs[lv].ParentIdx)
                                while specs[cu].Depth > specs[cv].Depth do
                                    upChain.Add cu
                                    cu <- max 0 (specs[cu].ParentIdx)
                                while specs[cv].Depth > specs[cu].Depth do
                                    downChain.Add cv
                                    cv <- max 0 (specs[cv].ParentIdx)
                                while cu <> cv && cu > 0 && cv > 0 do
                                    upChain.Add cu
                                    downChain.Add cv
                                    cu <- max 0 (specs[cu].ParentIdx)
                                    cv <- max 0 (specs[cv].ParentIdx)
                                let lca = if cu = cv then cu else 0

                                let interior = ResizeArray<Point>(upChain.Count + downChain.Count + 1)
                                for k = 0 to upChain.Count - 1 do
                                    interior.Add(worldCenter[upChain[k]])
                                interior.Add(worldCenter[lca])
                                for k = downChain.Count - 1 downto 0 do
                                    interior.Add(worldCenter[downChain[k]])

                                let firstAim = if interior.Count > 0 then interior[0] else vNode.Center
                                let lastAim = if interior.Count > 0 then interior[interior.Count - 1] else uNode.Center
                                let pStart = lookupAnchor uNode a.Source firstAim
                                let pEnd = lookupAnchor vNode a.Target lastAim

                                let beta = config.BundleBeta
                                let path, wpsArr =
                                    if beta <= 1e-6 || interior.Count = 0 then
                                        Path.ofCommands [ MoveTo pStart; LineTo pEnd ], [| pStart; pEnd |]
                                    else
                                        let mWay = interior.Count
                                        let blended =
                                            Array.init mWay (fun k ->
                                                let t = float (k + 1) / float (mWay + 1)
                                                let chordX = pStart.X + (pEnd.X - pStart.X) * t
                                                let chordY = pStart.Y + (pEnd.Y - pStart.Y) * t
                                                Point.create
                                                    (chordX + (interior[k].X - chordX) * beta)
                                                    (chordY + (interior[k].Y - chordY) * beta))

                                        let cmds = ResizeArray<PathCommand>(mWay + 1)
                                        cmds.Add(MoveTo pStart)
                                        for k = 0 to mWay - 1 do
                                            let ctrl = blended[k]
                                            let endPt =
                                                if k + 1 < mWay then
                                                    Point.create (0.5 * (ctrl.X + blended[k + 1].X)) (0.5 * (ctrl.Y + blended[k + 1].Y))
                                                else
                                                    pEnd
                                            cmds.Add(QuadraticTo(ctrl, endPt))

                                        // Sample smooth waypoint polyline along the C1 quadratic spline chain
                                        let sampled = ResizeArray<Point>(mWay * 4 + 3)
                                        sampled.Add pStart
                                        let mutable segStart = pStart
                                        for k = 0 to mWay - 1 do
                                            let ctrl = blended[k]
                                            let segEnd =
                                                if k + 1 < mWay then
                                                    Point.create (0.5 * (ctrl.X + blended[k + 1].X)) (0.5 * (ctrl.Y + blended[k + 1].Y))
                                                else
                                                    pEnd
                                            for s = 1 to 4 do
                                                let t = float s / 4.
                                                let omt = 1. - t
                                                let sx = omt * omt * segStart.X + 2. * omt * t * ctrl.X + t * t * segEnd.X
                                                let sy = omt * omt * segStart.Y + 2. * omt * t * ctrl.Y + t * t * segEnd.Y
                                                sampled.Add(Point.create sx sy)
                                            segStart <- segEnd

                                        Path.ofCommands (Seq.toList cmds), sampled.ToArray()

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

namespace Ranvier.D5

open System

/// <summary>An immutable mesh over unique sites. Public indices refer to the unique-site array; inputIndices retains original identity.</summary>
type Triangulation = private {
    Points:Point[]
    FirstInput:int[]
    ExtraInputs:System.Collections.Generic.Dictionary<int, int[]> option
    Triangles:int[]; Opposites:int[]; Hull:int[]
    Neighbors:int[][]; Search:Quadtree<int>
}

/// <summary>Builds and queries <c>Triangulation</c> meshes; non-finite points throw <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module Delaunay =
    /// <summary>Builds a deterministic Cartesian CCW triangulation without jitter. Exact duplicates share a site; collinear sites form a chain.</summary>
    let build (points:Point list) =
        let nInput = List.length points
        let indices = System.Collections.Generic.Dictionary<struct(float*float), int>(nInput)
        let unique = ResizeArray<Point>(nInput)
        let firstInputBuf = ResizeArray<int>(nInput)
        let mutable extraMap : System.Collections.Generic.Dictionary<int, ResizeArray<int>> = null
        let mutable inputIdx = 0
        for p in points do
            Check.finite "x" p.X
            Check.finite "y" p.Y
            let key = struct(p.X, p.Y)
            match indices.TryGetValue key with
            | true, site ->
                if isNull extraMap then
                    extraMap <- System.Collections.Generic.Dictionary<int, ResizeArray<int>>()
                match extraMap.TryGetValue site with
                | true, dupes -> dupes.Add inputIdx
                | _ ->
                    let dupes = ResizeArray<int>(2)
                    dupes.Add firstInputBuf[site]
                    dupes.Add inputIdx
                    extraMap[site] <- dupes
            | _ ->
                let site = unique.Count
                indices.Add(key, site)
                unique.Add p
                firstInputBuf.Add inputIdx
            inputIdx <- inputIdx + 1
        let sites = unique.ToArray()
        let nSites = sites.Length
        let firstInput = firstInputBuf.ToArray()
        let extraInputs =
            if isNull extraMap then None
            else
                let frozen = System.Collections.Generic.Dictionary<int, int[]>(extraMap.Count)
                for kv in extraMap do
                    frozen.Add(kv.Key, kv.Value.ToArray())
                Some frozen
        let boundaryList = Polygon.hullFromValidatedCopy (Array.copy sites)
        let boundary = Array.zeroCreate<int> boundaryList.Length
        let mutable bi = 0
        for p in boundaryList do
            boundary[bi] <- indices[struct(p.X, p.Y)]
            bi <- bi + 1
        let collinear = boundary.Length < 3
        let inline nextHalf i = if i % 3 = 2 then i - 2 else i + 1
        let triangles, opposite =
            if collinear then [||], [||]
            else
                match DelaunayKernel.fast sites with
                | ValueSome struct(tri, opp) -> tri, opp
                | ValueNone ->
                    let tri = DelaunayKernel.fallback sites boundary
                    let opp = Array.create tri.Length -1
                    let edges = System.Collections.Generic.Dictionary<int64, int>(tri.Length)
                    for i = 0 to tri.Length - 1 do
                        let a = tri[i]
                        let b = tri[nextHalf i]
                        let revKey = (int64 b <<< 32) ||| int64 (uint32 a)
                        match edges.TryGetValue revKey with
                        | true, j -> opp[i] <- j; opp[j] <- i
                        | _ -> ()
                        edges.Add((int64 a <<< 32) ||| int64 (uint32 b), i)
                    tri, opp
        let hull, neighbors =
            if collinear then
                let ordered = Array.init nSites id
#if !FABLE_COMPILER
                Array.Sort(ordered, System.Comparison<int>(fun a b ->
                    let pa, pb = sites[a], sites[b]
                    let cx = pa.X.CompareTo pb.X
                    if cx <> 0 then cx else pa.Y.CompareTo pb.Y))
#else
                Array.sortInPlaceWith (fun a b ->
                    let pa, pb = sites[a], sites[b]
                    let cx = compare pa.X pb.X
                    if cx <> 0 then cx else compare pa.Y pb.Y) ordered
#endif
                let nbrs = Array.create<int array> nSites [||]
                if nSites = 2 then
                    nbrs[ordered[0]] <- [| ordered[1] |]
                    nbrs[ordered[1]] <- [| ordered[0] |]
                elif nSites > 2 then
                    nbrs[ordered[0]] <- [| ordered[1] |]
                    nbrs[ordered[nSites - 1]] <- [| ordered[nSites - 2] |]
                    for k = 1 to nSites - 2 do
                        let u = ordered[k - 1]
                        let v = ordered[k + 1]
                        nbrs[ordered[k]] <- if u < v then [| u; v |] else [| v; u |]
                ordered, nbrs
            else
                let deg = Array.zeroCreate<int> nSites
                let successor = Array.create nSites -1
                let mutable hullEdgeCount = 0
                let mutable firstHullSite = -1
                let nHalf = triangles.Length
                for i = 0 to nHalf - 1 do
                    let opp = opposite[i]
                    let a = triangles[i]
                    let b = triangles[nextHalf i]
                    if opp < 0 then
                        successor[a] <- b
                        hullEdgeCount <- hullEdgeCount + 1
                        if firstHullSite < 0 then firstHullSite <- a
                        else
                            let pa, pf = sites[a], sites[firstHullSite]
                            if pa.X < pf.X || (pa.X = pf.X && pa.Y < pf.Y) then firstHullSite <- a
                        deg[a] <- deg[a] + 1
                        deg[b] <- deg[b] + 1
                    elif i < opp then
                        deg[a] <- deg[a] + 1
                        deg[b] <- deg[b] + 1
                let nbrs = Array.zeroCreate<int array> nSites
                for u = 0 to nSites - 1 do
                    nbrs[u] <- Array.zeroCreate<int> deg[u]
                    deg[u] <- 0
                for i = 0 to nHalf - 1 do
                    let opp = opposite[i]
                    if opp < 0 || i < opp then
                        let a = triangles[i]
                        let b = triangles[nextHalf i]
                        let wa = deg[a]
                        nbrs[a][wa] <- b
                        deg[a] <- wa + 1
                        let wb = deg[b]
                        nbrs[b][wb] <- a
                        deg[b] <- wb + 1
                for u = 0 to nSites - 1 do
                    let arr = nbrs[u]
                    if arr.Length > 1 then
                        Array.Sort arr
                        let mutable hasDup = false
                        for k = 1 to arr.Length - 1 do
                            if arr[k] = arr[k - 1] then hasDup <- true
                        if hasDup then
                            let mutable w = 1
                            for k = 1 to arr.Length - 1 do
                                if arr[k] <> arr[w - 1] then
                                    arr[w] <- arr[k]
                                    w <- w + 1
                            nbrs[u] <- Array.sub arr 0 w
                let hullArr = Array.zeroCreate<int> hullEdgeCount
                let mutable cur = firstHullSite
                let mutable hw = 0
                while hw = 0 || cur <> firstHullSite do
                    if hw >= hullEdgeCount then invalidOp "Invalid hull cycle."
                    hullArr[hw] <- cur
                    hw <- hw + 1
                    cur <- successor[cur]
                let finalHull = if hw = hullEdgeCount then hullArr else Array.sub hullArr 0 hw
                finalHull, nbrs
        let siteIds = Array.init nSites id
        { Points = sites
          FirstInput = firstInput
          ExtraInputs = extraInputs
          Triangles = triangles
          Opposites = opposite
          Hull = hull
          Neighbors = neighbors
          Search = Quadtree.build (fun i -> sites[i]) (fun _ -> 1.) siteIds }

    let internal rawSites mesh = mesh.Points
    let internal rawNeighbors site mesh = mesh.Neighbors[site]
    let internal inputIndicesList site mesh =
        match mesh.ExtraInputs with
        | Some map ->
            match map.TryGetValue site with
            | true, arr ->
                let mutable acc = []
                for k = arr.Length - 1 downto 0 do
                    acc <- arr[k] :: acc
                acc
            | _ -> [ mesh.FirstInput[site] ]
        | None -> [ mesh.FirstInput[site] ]

    /// <summary>Returns a copy of unique sites, in first-occurrence order.</summary>
    let sites mesh = Array.copy mesh.Points
    /// <summary>Returns copied triples of unique-site indices, each strictly Cartesian CCW.</summary>
    let triangles mesh = Array.copy mesh.Triangles
    /// <summary>Returns copied opposite halfedge indices; -1 denotes a hull edge.</summary>
    let halfedges mesh = Array.copy mesh.Opposites
    /// <summary>Returns the CCW hull including collinear boundary sites, or the lexicographic chain for a collinear mesh.</summary>
    let hull mesh = Array.copy mesh.Hull
    /// <summary>Returns copied adjacent unique-site indices in ascending order.</summary>
    let neighbors site mesh = Array.copy mesh.Neighbors[site]
    /// <summary>Returns copied original input indices represented by this unique site.</summary>
    let inputIndices site mesh =
        match mesh.ExtraInputs with
        | Some map ->
            match map.TryGetValue site with
            | true, arr -> Array.copy arr
            | _ -> [| mesh.FirstInput[site] |]
        | None -> [| mesh.FirstInput[site] |]
    /// <summary>Returns the nearest original input index, with exact ties resolved by input order.</summary>
    let nearest query mesh = Quadtree.nearest query infinity mesh.Search |> Option.map(fun site -> mesh.FirstInput[site])

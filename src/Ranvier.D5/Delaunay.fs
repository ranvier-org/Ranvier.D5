namespace Ranvier.D5

/// <summary>An immutable mesh over unique sites. Public indices refer to the unique-site array; inputIndices retains original identity.</summary>
type Triangulation = private {
    Points:Point[]; Inputs:int[][]; Triangles:int[]; Opposites:int[]; Hull:int[]
    Neighbors:int[][]; Search:Quadtree<int>
}

/// <summary>Builds and queries <c>Triangulation</c> meshes; non-finite points throw <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module Delaunay =
    /// <summary>Builds a deterministic Cartesian CCW triangulation without jitter. Exact duplicates share a site; collinear sites form a chain.</summary>
    let build (points:Point list) =
        let indices=System.Collections.Generic.Dictionary<struct(float*float),int>()
        let unique=ResizeArray<Point>()
        let inputs=ResizeArray<ResizeArray<int>>()
        points |> List.iteri(fun i p ->
            Point.create p.X p.Y |> ignore
            match indices.TryGetValue(struct(p.X,p.Y)) with
            | true,site -> inputs[site].Add i
            | _ ->
                indices.Add(struct(p.X,p.Y),unique.Count)
                unique.Add p
                inputs.Add(ResizeArray<int>([i])))
        let sites=unique.ToArray()
        let boundary=Polygon.hull (List.ofArray sites) |> List.map(fun p -> indices[struct(p.X,p.Y)]) |> List.toArray
        let collinear=boundary.Length<3
        let triangles=
            if collinear then [||] else
            DelaunayKernel.fast sites |> Option.defaultWith(fun () -> DelaunayKernel.fallback sites boundary)
        let opposite=Array.create triangles.Length -1
        let edges=System.Collections.Generic.Dictionary<struct(int*int),int>()
        let neighbors=Array.init sites.Length (fun _ -> System.Collections.Generic.HashSet<int>())
        let next i=if i%3=2 then i-2 else i+1
        for i in 0..triangles.Length-1 do
            let a,b=triangles[i],triangles[next i]
            neighbors[a].Add b |> ignore
            neighbors[b].Add a |> ignore
            match edges.TryGetValue(struct(b,a)) with
            | true,j -> opposite[i]<-j;opposite[j]<-i
            | _ -> ()
            edges.Add(struct(a,b),i)
        let hull=
            if collinear then
                let ordered=Array.init sites.Length id |> Array.sortBy(fun i -> sites[i].X,sites[i].Y)
                for i in 1..ordered.Length-1 do
                    neighbors[ordered[i-1]].Add ordered[i] |> ignore
                    neighbors[ordered[i]].Add ordered[i-1] |> ignore
                ordered
            else
                let successor=System.Collections.Generic.Dictionary<int,int>()
                for i in 0..opposite.Length-1 do
                    if opposite[i]<0 then successor.Add(triangles[i],triangles[next i])
                let first=successor.Keys |> Seq.minBy(fun i -> sites[i].X,sites[i].Y)
                let result=ResizeArray<int>()
                let mutable current=first
                while result.Count=0 || current<>first do
                    result.Add current
                    current<-successor[current]
                    if result.Count>successor.Count then invalidOp "Invalid hull cycle."
                result.ToArray()
        {Points=sites;Inputs=inputs |> Seq.map(fun values -> values.ToArray()) |> Seq.toArray
         Triangles=triangles;Opposites=opposite;Hull=hull
         Neighbors=neighbors |> Array.map(fun values -> values |> Seq.sort |> Seq.toArray)
         Search=Quadtree.build (fun i -> sites[i]) (fun _ -> 1.) (seq{0..sites.Length-1})}

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
    let inputIndices site mesh = Array.copy mesh.Inputs[site]
    /// <summary>Returns the nearest original input index, with exact ties resolved by input order.</summary>
    let nearest query mesh = Quadtree.nearest query infinity mesh.Search |> Option.map(fun site -> mesh.Inputs[site][0])

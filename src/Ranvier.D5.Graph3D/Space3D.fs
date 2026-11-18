namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Unit quaternion representing a gimbal-lock-free 3D rotation.</summary>
[<Struct>]
type Quaternion =
    /// <summary>Vector i component.</summary>
    val X: float
    /// <summary>Vector j component.</summary>
    val Y: float
    /// <summary>Vector k component.</summary>
    val Z: float
    /// <summary>Scalar real component.</summary>
    val W: float
    internal new(x: float, y: float, z: float, w: float) = { X = x; Y = y; Z = z; W = w }

/// <summary>Construction, composition and 3D vector rotation for <c>Quaternion</c>.</summary>
[<RequireQualifiedAccess>]
module Quaternion =
    /// <summary>The identity rotation quaternion <c>(0, 0, 0, 1)</c>.</summary>
    let identity = Quaternion(0., 0., 0., 1.)

    /// <summary>Normalizes a finite quaternion, returning <c>identity</c> if degenerate.</summary>
    let normalize (q: Quaternion) =
        let n2 = q.X * q.X + q.Y * q.Y + q.Z * q.Z + q.W * q.W
        if not (Double.IsFinite n2) then invalidArg "q" "Quaternion components must be finite."
        if n2 <= 1e-24 then identity
        else
            let inv = 1. / sqrt n2
            Quaternion(q.X * inv, q.Y * inv, q.Z * inv, q.W * inv)

    /// <summary>Creates a normalized rotation around <c>axis</c> by <c>angleRadians</c>.</summary>
    let fromAxisAngle (axis: Point3) (angleRadians: float) =
        if not (Double.IsFinite angleRadians) then invalidArg "angleRadians" "Angle must be finite."
        let len = Point3.distance (Point3.create 0. 0. 0.) axis
        if len <= 1e-12 then identity
        else
            let half = angleRadians * 0.5
            let s, c = sin half, cos half
            normalize (Quaternion((axis.X / len) * s, (axis.Y / len) * s, (axis.Z / len) * s, c))

    /// <summary>Creates a normalized rotation mapping unit direction <c>fromDir</c> onto unit direction <c>toDir</c>.</summary>
    let fromUnitVectors (fromDir: Point3) (toDir: Point3) =
        let dot = fromDir.X * toDir.X + fromDir.Y * toDir.Y + fromDir.Z * toDir.Z
        if dot >= 1. - 1e-12 then identity
        elif dot <= -1. + 1e-12 then
            // 180-degree rotation around an orthogonal axis
            let ortho =
                if abs fromDir.X < 0.9 then
                    Point3.create 0. -fromDir.Z fromDir.Y
                else
                    Point3.create -fromDir.Z 0. fromDir.X
            fromAxisAngle ortho Math.PI
        else
            let cx = fromDir.Y * toDir.Z - fromDir.Z * toDir.Y
            let cy = fromDir.Z * toDir.X - fromDir.X * toDir.Z
            let cz = fromDir.X * toDir.Y - fromDir.Y * toDir.X
            normalize (Quaternion(cx, cy, cz, 1. + dot))

    /// <summary>Hamilton product <c>a * b</c> applying rotation <c>b</c> followed by <c>a</c>.</summary>
    let multiply (a: Quaternion) (b: Quaternion) =
        normalize (
            Quaternion(
                a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y,
                a.W * b.Y - a.X * b.Z + a.Y * b.W + a.Z * b.X,
                a.W * b.Z + a.X * b.Y - a.Y * b.X + a.Z * b.W,
                a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z
            )
        )

    /// <summary>Rotates a 3D point around the origin by <c>q</c>.</summary>
    let rotate (q: Quaternion) (p: Point3) =
        let tx = 2. * (q.Y * p.Z - q.Z * p.Y)
        let ty = 2. * (q.Z * p.X - q.X * p.Z)
        let tz = 2. * (q.X * p.Y - q.Y * p.X)
        Point3.create
            (p.X + q.W * tx + (q.Y * tz - q.Z * ty))
            (p.Y + q.W * ty + (q.Z * tx - q.X * tz))
            (p.Z + q.W * tz + (q.X * ty - q.Y * tx))

/// <summary>Immutable, contiguous 3D waypoint chain providing O(1) indexed access for zero-allocation GPU batching without exposing mutable arrays.</summary>
[<Struct>]
type Point3Chain =
    private
    | Point3Chain of Point3[]
    /// <summary>Number of 3D points in the chain.</summary>
    member this.Length =
        let (Point3Chain arr) = this
        if isNull (box arr) then 0 else arr.Length
    /// <summary>Indexed 3D point at <c>index</c>.</summary>
    member this.Item
        with get (index: int) =
            let (Point3Chain arr) = this
            arr[index]

/// <summary>Constructors and conversions for <c>Point3Chain</c>.</summary>
[<RequireQualifiedAccess>]
module Point3Chain =
    /// <summary>An empty 3D waypoint chain.</summary>
    let empty = Point3Chain [||]
    /// <summary>Copies a list of 3D points into an immutable contiguous chain.</summary>
    let ofList (points: Point3 list) = Point3Chain(List.toArray points)
    /// <summary>Wraps a freshly allocated internal array without extra copy.</summary>
    let internal ofOwnedArray (points: Point3[]) = Point3Chain points
    /// <summary>Returns the number of points in the chain.</summary>
    let length (chain: Point3Chain) = chain.Length
    /// <summary>Returns the point at <c>index</c>.</summary>
    let item index (chain: Point3Chain) = chain[index]
    /// <summary>Returns the points as an immutable F# list.</summary>
    let toList (Point3Chain arr) = if isNull (box arr) then [] else Array.toList arr

/// <summary>A node positioned in 3D space with bounding cuboid, radius, conformal/hyperbolic scale attenuation and stable ID.</summary>
type PositionedNode3D =
    { /// <summary>Stable node identifier.</summary>
      Id: string
      /// <summary>Semantic node kind.</summary>
      Kind: NodeKind
      /// <summary>3D center coordinate.</summary>
      Center: Point3
      /// <summary>Axis-aligned 3D bounding cuboid.</summary>
      Bounds: Bounds3
      /// <summary>Bounding sphere radius in layout units.</summary>
      Radius: float
      /// <summary>Attenuation factor in [0,1] (1.0 in Euclidean layouts; conformal/Klein fisheye scale in hyperbolic H3/Walrus layouts).</summary>
      ScaleFactor: float
      /// <summary>Optional enclosing compound group identifier.</summary>
      ParentGroup: string option
      /// <summary>Optional stratum, rank or tree depth index.</summary>
      Layer: int option }

/// <summary>A compound group positioned as a 3D bounding cuboid around its member nodes and nested child groups.</summary>
type PositionedGroup3D =
    { /// <summary>Stable group identifier.</summary>
      Id: string
      /// <summary>Enclosing parent group identifier, if any.</summary>
      ParentGroup: string option
      /// <summary>Outer padded 3D cuboid.</summary>
      Bounds: Bounds3
      /// <summary>Inner content 3D cuboid.</summary>
      InnerBounds: Bounds3
      /// <summary>Nesting depth (0 at root groups).</summary>
      Depth: int }

/// <summary>A 3D routed association carrying both O(1)-indexed <c>Points</c> for GPU line/tube batching and <c>IsBackbone</c> classification.</summary>
type RoutedAssociation3D =
    { /// <summary>Stable association identifier.</summary>
      Id: string
      /// <summary>Origin endpoint.</summary>
      Source: Endpoint
      /// <summary>Destination endpoint.</summary>
      Target: Endpoint
      /// <summary>Semantic relationship kind.</summary>
      Kind: AssociationKind
      /// <summary>True when this association belongs to the primary spanning-tree or forward DAG backbone; false for cross-links/back-edges.</summary>
      IsBackbone: bool
      /// <summary>Contiguous O(1)-indexed 3D polyline points.</summary>
      Points: Point3Chain
      /// <summary>3D polyline waypoints as an F# list.</summary>
      Waypoints: Point3 list }

/// <summary>Complete, immutable 3D diagram/network layout shared across Euclidean and projected hyperbolic 3D engines.</summary>
type DiagramLayout3D =
    { /// <summary>Enclosing 3D bounding cuboid of all nodes, groups and associations.</summary>
      Bounds: Bounds3
      /// <summary>Positioned 3D nodes in ordinal ID order.</summary>
      Nodes: PositionedNode3D list
      /// <summary>Positioned 3D compound groups ordered outermost-first.</summary>
      Groups: PositionedGroup3D list
      /// <summary>Routed 3D associations in ordinal ID order.</summary>
      Associations: RoutedAssociation3D list }

module internal Kernels3D =
    let inline check (token: System.Threading.CancellationToken) (i: int) =
        if (i &&& 63) = 0 then token.ThrowIfCancellationRequested()

    let nodeBox (center: Point3) (sx: float) (sy: float) (sz: float) =
        Bounds3.create (center.X - sx * 0.5) (center.Y - sy * 0.5) (center.Z - sz * 0.5) sx sy sz

    let computeGroups3D (resolvedGraph: DiagramGraph) (nodeBoxes: Bounds3[]) : PositionedGroup3D list =
        let groups = DiagramGraph.groupArray resolvedGraph
        let gCount = groups.Length
        if gCount = 0 then []
        else
            let nodes = DiagramGraph.nodeArray resolvedGraph
            let gMap = Dictionary<string, int>(StringComparer.Ordinal)
            for gi = 0 to gCount - 1 do gMap.Add(groups[gi].Id, gi)
            let rec depthOf gi =
                match groups[gi].ParentGroup with
                | Some pid when gMap.ContainsKey pid -> 1 + depthOf gMap[pid]
                | _ -> 0
            let depths = Array.init gCount depthOf
            let order = Array.init gCount id
            Array.Sort(order, Comparer<int>.Create(fun a b ->
                let c = compare depths[a] depths[b]
                if c <> 0 then c else StringComparer.Ordinal.Compare(groups[a].Id, groups[b].Id)))

            let orderedGroups = order |> Array.map (fun i -> groups[i])
            let orderedDepths = order |> Array.map (fun i -> depths[i])
            let idxMap = Dictionary<string, int>(StringComparer.Ordinal)
            for gi = 0 to gCount - 1 do idxMap.Add(orderedGroups[gi].Id, gi)

            let directNodes = Array.init gCount (fun _ -> ResizeArray<int>())
            for ni = 0 to nodes.Length - 1 do
                match nodes[ni].ParentGroup with
                | Some gid when idxMap.ContainsKey gid -> directNodes[idxMap[gid]].Add ni
                | _ -> ()

            let childGroups = Array.init gCount (fun _ -> ResizeArray<int>())
            for gi = 0 to gCount - 1 do
                match orderedGroups[gi].ParentGroup with
                | Some pid when idxMap.ContainsKey pid -> childGroups[idxMap[pid]].Add gi
                | _ -> ()

            let outer = Array.zeroCreate<Bounds3> gCount
            let inner = Array.zeroCreate<Bounds3> gCount
            for gi = gCount - 1 downto 0 do
                let g = orderedGroups[gi]
                let p = g.Padding
                let padZ = 0.5 * (p.Left + p.Right)
                let mutable hasAny = false
                let mutable acc = Bounds3.create 0. 0. 0. 0. 0. 0.
                for ni in directNodes[gi] do
                    acc <- if not hasAny then (hasAny <- true; nodeBoxes[ni]) else Bounds3.union acc nodeBoxes[ni]
                for cgi in childGroups[gi] do
                    acc <- if not hasAny then (hasAny <- true; outer[cgi]) else Bounds3.union acc outer[cgi]
                if not hasAny then
                    let w, h = g.MinSize.Width, g.MinSize.Height
                    let d = min w h
                    outer[gi] <- Bounds3.create 0. 0. 0. w h d
                    inner[gi] <- Bounds3.create p.Left p.Top padZ (max 0. (w - p.Left - p.Right)) (max 0. (h - p.Top - p.Bottom)) (max 0. (d - 2. * padZ))
                else
                    inner[gi] <- acc
                    outer[gi] <-
                        Bounds3.create
                            (Bounds3.minX acc - p.Left)
                            (Bounds3.minY acc - p.Top)
                            (Bounds3.minZ acc - padZ)
                            (max g.MinSize.Width (Bounds3.sizeX acc + p.Left + p.Right))
                            (max g.MinSize.Height (Bounds3.sizeY acc + p.Top + p.Bottom))
                            (Bounds3.sizeZ acc + 2. * padZ)

            [ for gi in 0 .. gCount - 1 ->
                { Id = orderedGroups[gi].Id
                  ParentGroup = orderedGroups[gi].ParentGroup
                  Bounds = outer[gi]
                  InnerBounds = inner[gi]
                  Depth = orderedDepths[gi] } ]

    let computeBounds3D
        (fallback: Bounds3)
        (nodes: PositionedNode3D list)
        (groups: PositionedGroup3D list)
        (assocs: RoutedAssociation3D list)
        : Bounds3 =
        let mutable hasAny = false
        let mutable minX, minY, minZ = infinity, infinity, infinity
        let mutable maxX, maxY, maxZ = -infinity, -infinity, -infinity
        let inline addBox (b: Bounds3) =
            let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
            let x1, y1, z1 = Bounds3.maxX b, Bounds3.maxY b, Bounds3.maxZ b
            hasAny <- true
            if x0 < minX then minX <- x0
            if y0 < minY then minY <- y0
            if z0 < minZ then minZ <- z0
            if x1 > maxX then maxX <- x1
            if y1 > maxY then maxY <- y1
            if z1 > maxZ then maxZ <- z1
        for n in nodes do addBox n.Bounds
        for g in groups do addBox g.Bounds
        for a in assocs do
            let pts = a.Points
            for i = 0 to pts.Length - 1 do
                let p = pts[i]
                hasAny <- true
                if p.X < minX then minX <- p.X
                if p.Y < minY then minY <- p.Y
                if p.Z < minZ then minZ <- p.Z
                if p.X > maxX then maxX <- p.X
                if p.Y > maxY then maxY <- p.Y
                if p.Z > maxZ then maxZ <- p.Z
        if hasAny then Bounds3.create minX minY minZ (maxX - minX) (maxY - minY) (maxZ - minZ)
        else fallback

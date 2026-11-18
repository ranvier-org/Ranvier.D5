namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>3D hierarchical tree geometry family.</summary>
[<RequireQualifiedAccess>]
type ConeTreeMode =
    /// <summary>Robertson, Mackinlay &amp; Card (CHI 1991) top-down 3D circular Cone Tree along +Y.</summary>
    | VerticalCone
    /// <summary>Horizontal Cam Tree progressing along +X with circular YZ child cones.</summary>
    | HorizontalCam
    /// <summary>Carrière &amp; Kazman (InfoVis 1995) 3D isotropic Spherical Balloon Tree radiating in 3D shells.</summary>
    | SphericalBalloon

/// <summary>Settings for 3D Cone Tree, Cam Tree and 3D Spherical Balloon Tree layouts.</summary>
type ConeTree3DConfig =
    { /// <summary>Target 3D bounding volume origin/center.</summary>
      Bounds: Bounds3
      /// <summary>Selected 3D tree geometry mode.</summary>
      Mode: ConeTreeMode
      /// <summary>Optional root node ID.</summary>
      RootId: string option
      /// <summary>Spanning-tree extraction policy over cyclic or general graphs.</summary>
      SpanningStrategy: SpanningTreeStrategy
      /// <summary>Axial distance between parent apex and child cone base (or radial step in balloon mode).</summary>
      LevelStep: float
      /// <summary>Minimum sibling clearance gap on cone perimeters / balloon spheres.</summary>
      SiblingGap: float }

/// <summary>Constructors for <c>ConeTree3DConfig</c>.</summary>
[<RequireQualifiedAccess>]
module ConeTree3DConfig =
    /// <summary>Creates a vertical 3D Cone Tree configuration with 110-unit level steps and 20-unit sibling clearance.</summary>
    let create bounds =
        { Bounds = bounds
          Mode = ConeTreeMode.VerticalCone
          RootId = None
          SpanningStrategy = SpanningTreeStrategy.Bfs
          LevelStep = 110.
          SiblingGap = 20. }

/// <summary>Robertson-Mackinlay-Card (1991) 3D Cone Trees and Carrière-Kazman (1995) 3D Spherical Balloon Trees.</summary>
[<RequireQualifiedAccess>]
module ConeTree3D =
    /// <summary>Computes a 3D Cone, Cam or Spherical Balloon layout with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: ConeTree3DConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout3D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.LevelStep && Double.IsFinite config.SiblingGap)
           || config.LevelStep <= 0. || config.SiblingGap < 0. then
            Error [ InvalidObservation "ConeTree3D requires positive finite LevelStep and nonnegative finite SiblingGap." ]
        else
            let resolved = DiagramGraph.resolveCollapsed graph
            let nodes = DiagramGraph.nodes resolved |> List.toArray
            let assocs = DiagramGraph.associations resolved |> List.toArray
            let n = nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = Kernels3D.computeGroups3D resolved [||]; Associations = [] }
            else
                let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
                for i = 0 to n - 1 do nodeIdx.Add(nodes[i].Id, i)

                let forest = SpanningTree.extract config.SpanningStrategy config.RootId resolved
                let children = Array.init n (fun i ->
                    forest.ChildrenOf
                    |> Map.tryFind nodes[i].Id
                    |> Option.defaultValue []
                    |> List.map (fun cid -> nodeIdx[cid])
                    |> List.toArray)
                let depths = Array.init n (fun i -> forest.DepthOf |> Map.tryFind nodes[i].Id |> Option.defaultValue 0)
                let rootIndices = forest.Roots |> List.map (fun rid -> nodeIdx[rid]) |> List.toArray

                let selfRadius =
                    Array.init n (fun i ->
                        let nd = nodes[i]
                        0.5 * sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height + nd.DepthSize * nd.DepthSize))

                // Postorder bottom-up synthesis of non-overlapping subtree footprint radius!
                let postorder = ResizeArray<int>(n)
                let stack = Stack<struct(int * bool)>()
                for r = rootIndices.Length - 1 downto 0 do stack.Push(struct(rootIndices[r], false))
                while stack.Count > 0 do
                    let struct(u, expanded) = stack.Pop()
                    if expanded then postorder.Add u
                    else
                        stack.Push(struct(u, true))
                        let ch = children[u]
                        for k = ch.Length - 1 downto 0 do stack.Push(struct(ch[k], false))

                let coneRadius = Array.zeroCreate<float> n
                let childRingRadius = Array.zeroCreate<float> n
                for idx = 0 to postorder.Count - 1 do
                    Kernels3D.check token idx
                    let u = postorder[idx]
                    let ch = children[u]
                    if ch.Length = 0 then
                        coneRadius[u] <- selfRadius[u]
                        childRingRadius[u] <- 0.
                    elif ch.Length = 1 then
                        let c = ch[0]
                        childRingRadius[u] <- 0.
                        coneRadius[u] <- max selfRadius[u] coneRadius[c]
                    else
                        match config.Mode with
                        | ConeTreeMode.VerticalCone
                        | ConeTreeMode.HorizontalCam ->
                            // Sum of child diameters + gaps around 2*pi perimeter => R_ring = circumference / (2*pi)
                            let mutable circ = 0.
                            let mutable maxChildR = 0.
                            for c in ch do
                                circ <- circ + 2. * coneRadius[c] + config.SiblingGap
                                if coneRadius[c] > maxChildR then maxChildR <- coneRadius[c]
                            let rRing = max (maxChildR + config.SiblingGap * 0.5) (circ / (2. * Math.PI))
                            childRingRadius[u] <- rRing
                            coneRadius[u] <- max selfRadius[u] (rRing + maxChildR)
                        | ConeTreeMode.SphericalBalloon ->
                            // Surface area of sphere 4*pi*R^2 >= 1.45 * sum(pi * (r_c + gap/2)^2)
                            let mutable areaSum = 0.
                            let mutable maxChildR = 0.
                            for c in ch do
                                let rc = coneRadius[c] + config.SiblingGap * 0.5
                                areaSum <- areaSum + Math.PI * rc * rc
                                if coneRadius[c] > maxChildR then maxChildR <- coneRadius[c]
                            let rShell = max (selfRadius[u] + maxChildR + config.SiblingGap) (sqrt (1.45 * areaSum / (4. * Math.PI)))
                            childRingRadius[u] <- max config.LevelStep rShell
                            coneRadius[u] <- childRingRadius[u] + maxChildR

                // Top-down placement in 3D
                let centers = Array.zeroCreate<Point3> n
                let c0 = Bounds3.center config.Bounds
                let goldenAngle = Math.PI * (3. - sqrt 5.)

                let placeTree (rootIdx: int) (rootOrigin: Point3) =
                    centers[rootIdx] <- rootOrigin
                    let q = Queue<struct(int * Point3)>()
                    // Second element is parent's incoming direction (for balloon outward hemisphere bias)
                    q.Enqueue(struct(rootIdx, Point3.create 0. 1. 0.))
                    while q.Count > 0 do
                        let struct(u, outDir) = q.Dequeue()
                        let pCenter = centers[u]
                        let ch = children[u]
                        if ch.Length = 1 then
                            let c = ch[0]
                            let nextPt =
                                match config.Mode with
                                | ConeTreeMode.VerticalCone -> Point3.create pCenter.X (pCenter.Y + config.LevelStep) pCenter.Z
                                | ConeTreeMode.HorizontalCam -> Point3.create (pCenter.X + config.LevelStep) pCenter.Y pCenter.Z
                                | ConeTreeMode.SphericalBalloon ->
                                    let step = max config.LevelStep (selfRadius[u] + coneRadius[c] + config.SiblingGap)
                                    Point3.create (pCenter.X + outDir.X * step) (pCenter.Y + outDir.Y * step) (pCenter.Z + outDir.Z * step)
                            centers[c] <- nextPt
                            q.Enqueue(struct(c, outDir))
                        elif ch.Length > 1 then
                            match config.Mode with
                            | ConeTreeMode.VerticalCone
                            | ConeTreeMode.HorizontalCam ->
                                let rRing = childRingRadius[u]
                                let totalSpan = ch |> Array.sumBy (fun c -> 2. * coneRadius[c] + config.SiblingGap)
                                let mutable curAngle = 0.
                                for c in ch do
                                    let share = 2. * Math.PI * ((2. * coneRadius[c] + config.SiblingGap) / max 1e-6 totalSpan)
                                    let midA = curAngle + share * 0.5
                                    curAngle <- curAngle + share
                                    let cPt =
                                        match config.Mode with
                                        | ConeTreeMode.VerticalCone ->
                                            Point3.create (pCenter.X + rRing * cos midA) (pCenter.Y + config.LevelStep) (pCenter.Z + rRing * sin midA)
                                        | _ ->
                                            Point3.create (pCenter.X + config.LevelStep) (pCenter.Y + rRing * cos midA) (pCenter.Z + rRing * sin midA)
                                    centers[c] <- cPt
                                    q.Enqueue(struct(c, outDir))
                            | ConeTreeMode.SphericalBalloon ->
                                let rShell = childRingRadius[u]
                                let isRoot = (u = rootIdx)
                                let orient = Quaternion.fromUnitVectors (Point3.create 0. 0. 1.) outDir
                                for k = 0 to ch.Length - 1 do
                                    let c = ch[k]
                                    // Root uses full sphere [-0.95, 0.95]; deeper nodes use outward hemisphere [0.05, 0.98] to avoid parent collision!
                                    let zLocal =
                                        if isRoot then 1. - (2. * (float k + 0.5) / float ch.Length) * 0.95
                                        else 0.98 - (float k / float (max 1 (ch.Length - 1))) * 0.88
                                    let ring = sqrt (max 0. (1. - zLocal * zLocal))
                                    let az = float k * goldenAngle
                                    let localDir = Point3.create (ring * cos az) (ring * sin az) zLocal
                                    let worldDir = if isRoot then localDir else Quaternion.rotate orient localDir
                                    let cPt = Point3.create (pCenter.X + worldDir.X * rShell) (pCenter.Y + worldDir.Y * rShell) (pCenter.Z + worldDir.Z * rShell)
                                    centers[c] <- cPt
                                    q.Enqueue(struct(c, worldDir))

                // Offset multiple disconnected roots cleanly along X
                let mutable cursorX = c0.X
                for r = 0 to rootIndices.Length - 1 do
                    let ri = rootIndices[r]
                    if r > 0 then
                        cursorX <- cursorX + coneRadius[rootIndices[r - 1]] + coneRadius[ri] + config.SiblingGap * 2.
                    let startOrigin =
                        match config.Mode with
                        | ConeTreeMode.VerticalCone -> Point3.create cursorX (Bounds3.minY config.Bounds + selfRadius[ri]) c0.Z
                        | ConeTreeMode.HorizontalCam -> Point3.create (Bounds3.minX config.Bounds + selfRadius[ri]) cursorX c0.Z
                        | ConeTreeMode.SphericalBalloon -> Point3.create cursorX c0.Y c0.Z
                    placeTree ri startOrigin

                let nodeBoxes = Array.zeroCreate<Bounds3> n
                let posNodes =
                    [ for i in 0 .. n - 1 do
                        let nd = nodes[i]
                        let box = Kernels3D.nodeBox centers[i] nd.Size.Width nd.Size.Height nd.DepthSize
                        nodeBoxes[i] <- box
                        yield
                            { Id = nd.Id
                              Kind = nd.Kind
                              Center = centers[i]
                              Bounds = box
                              Radius = selfRadius[i]
                              ScaleFactor = 1.
                              ParentGroup = nd.ParentGroup
                              Layer = Some depths[i] } ]

                let groups = Kernels3D.computeGroups3D resolved nodeBoxes
                let routed =
                    [ for a in assocs do
                        let u, v = nodeIdx[a.Source.NodeId], nodeIdx[a.Target.NodeId]
                        let isTree = forest.TreeAssociationIds.Contains a.Id
                        let pts =
                            if u = v then
                                let c, r = centers[u], selfRadius[u]
                                [| c; Point3.create (c.X + r * 1.4) (c.Y - r * 1.4) (c.Z + r * 0.5); c |]
                            else [| centers[u]; centers[v] |]
                        yield
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              IsBackbone = isTree
                              Points = Point3Chain.ofOwnedArray pts
                              Waypoints = Array.toList pts } ]

                Ok
                    { Bounds = Kernels3D.computeBounds3D config.Bounds posNodes groups routed
                      Nodes = posNodes
                      Groups = groups
                      Associations = routed }

    /// <summary>Computes a 3D Cone, Cam or Spherical Balloon layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

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
      SiblingGap: float
      /// <summary>Axial progression direction for <c>VerticalCone</c> (<c>+Y</c> vs <c>-Y</c>) and <c>HorizontalCam</c> (<c>+X</c> vs <c>-X</c>); default <c>VerticalDirection3D.Increasing</c>.</summary>
      Direction: VerticalDirection3D }

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
          SiblingGap = 20.
          Direction = VerticalDirection3D.Increasing }

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
            let eff = DiagramGraph.effective resolved
            let nodes = eff.Nodes
            let assocs = eff.Associations
            let n = nodes.Length
            if n = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = Kernels3D.computeGroups3D resolved [||]; Associations = [] }
            else
                let csr = GraphKernels.buildCsr eff
                let rootIndices, _, children, depths, isTreeAssoc =
                    SpanningTree.extractIndexed config.SpanningStrategy config.RootId eff csr

                let selfRadius =
                    Array.init n (fun i ->
                        let nd = nodes[i]
                        0.5 * sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height + nd.DepthSize * nd.DepthSize))

                // Postorder bottom-up synthesis of non-overlapping subtree footprint radius via flat stack!
                let postorder = Array.zeroCreate<int> n
                let mutable postLen = 0
                let postStack = Array.zeroCreate<int> (max 1 (n * 2))
                let mutable sp = 0
                for r = rootIndices.Length - 1 downto 0 do
                    postStack[sp] <- rootIndices[r]
                    sp <- sp + 1
                while sp > 0 do
                    sp <- sp - 1
                    let raw = postStack[sp]
                    if raw < 0 then
                        postorder[postLen] <- ~~~raw
                        postLen <- postLen + 1
                    else
                        let u = raw
                        postStack[sp] <- ~~~u
                        sp <- sp + 1
                        let ch = children[u]
                        for k = ch.Length - 1 downto 0 do
                            postStack[sp] <- ch[k]
                            sp <- sp + 1

                let coneRadius = Array.zeroCreate<float> n
                let childRingRadius = Array.zeroCreate<float> n
                for idx = 0 to postLen - 1 do
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
                            let mutable circ = 0.
                            let mutable maxChildR = 0.
                            for k = 0 to ch.Length - 1 do
                                let rc = coneRadius[ch[k]]
                                circ <- circ + 2. * rc + config.SiblingGap
                                if rc > maxChildR then maxChildR <- rc
                            let rRing = max (maxChildR + config.SiblingGap * 0.5) (circ / (2. * Math.PI))
                            childRingRadius[u] <- rRing
                            coneRadius[u] <- max selfRadius[u] (rRing + maxChildR)
                        | ConeTreeMode.SphericalBalloon ->
                            let mutable rcSqSum = 0.
                            let mutable maxChildR = 0.
                            let halfGap = config.SiblingGap * 0.5
                            for k = 0 to ch.Length - 1 do
                                let cr = coneRadius[ch[k]]
                                let rc = cr + halfGap
                                rcSqSum <- rcSqSum + rc * rc
                                if cr > maxChildR then maxChildR <- cr
                            let rShell = max (selfRadius[u] + maxChildR + config.SiblingGap) (sqrt (1.45 * rcSqSum * 0.25))
                            childRingRadius[u] <- max config.LevelStep rShell
                            coneRadius[u] <- childRingRadius[u] + maxChildR

                // Top-down placement in 3D via reusable flat BFS arrays
                let centers = Array.zeroCreate<Point3> n
                let c0 = Bounds3.center config.Bounds
                let goldenAngle = Math.PI * (3. - sqrt 5.)
                let zUnit = Point3.create 0. 0. 1.
                let yUnit = Point3.create 0. 1. 0.
                let qNodes = Array.zeroCreate<int> n
                let qDirs = Array.zeroCreate<Point3> n
                let axialSign =
                    match config.Direction with
                    | VerticalDirection3D.Increasing -> 1.
                    | VerticalDirection3D.Decreasing -> -1.
                let axialStep = axialSign * config.LevelStep

                let placeTree (rootIdx: int) (rootOrigin: Point3) =
                    centers[rootIdx] <- rootOrigin
                    let mutable qHead = 0
                    let mutable qTail = 0
                    qNodes[qTail] <- rootIdx
                    qDirs[qTail] <- yUnit
                    qTail <- qTail + 1
                    while qHead < qTail do
                        let u = qNodes[qHead]
                        let outDir = qDirs[qHead]
                        qHead <- qHead + 1
                        let pCenter = centers[u]
                        let ch = children[u]
                        if ch.Length = 1 then
                            let c = ch[0]
                            let nextPt =
                                match config.Mode with
                                | ConeTreeMode.VerticalCone -> Point3.create pCenter.X (pCenter.Y + axialStep) pCenter.Z
                                | ConeTreeMode.HorizontalCam -> Point3.create (pCenter.X + axialStep) pCenter.Y pCenter.Z
                                | ConeTreeMode.SphericalBalloon ->
                                    let step = max config.LevelStep (selfRadius[u] + coneRadius[c] + config.SiblingGap)
                                    Point3.create (pCenter.X + outDir.X * step) (pCenter.Y + outDir.Y * step) (pCenter.Z + outDir.Z * step)
                            centers[c] <- nextPt
                            qNodes[qTail] <- c
                            qDirs[qTail] <- outDir
                            qTail <- qTail + 1
                        elif ch.Length > 1 then
                            match config.Mode with
                            | ConeTreeMode.VerticalCone
                            | ConeTreeMode.HorizontalCam ->
                                let rRing = childRingRadius[u]
                                let mutable totalSpan = 0.
                                for k = 0 to ch.Length - 1 do
                                    totalSpan <- totalSpan + 2. * coneRadius[ch[k]] + config.SiblingGap
                                let scaleSpan = (2. * Math.PI) / max 1e-6 totalSpan
                                let mutable curAngle = 0.
                                for k = 0 to ch.Length - 1 do
                                    let c = ch[k]
                                    let share = (2. * coneRadius[c] + config.SiblingGap) * scaleSpan
                                    let midA = curAngle + share * 0.5
                                    curAngle <- curAngle + share
                                    let cPt =
                                        match config.Mode with
                                        | ConeTreeMode.VerticalCone ->
                                            Point3.create (pCenter.X + rRing * cos midA) (pCenter.Y + axialStep) (pCenter.Z + rRing * sin midA)
                                        | _ ->
                                            Point3.create (pCenter.X + axialStep) (pCenter.Y + rRing * cos midA) (pCenter.Z + rRing * sin midA)
                                    centers[c] <- cPt
                                    qNodes[qTail] <- c
                                    qDirs[qTail] <- outDir
                                    qTail <- qTail + 1
                            | ConeTreeMode.SphericalBalloon ->
                                let rShell = childRingRadius[u]
                                let isRoot = (u = rootIdx)
                                let orient = if isRoot then Quaternion.identity else Quaternion.fromUnitVectors zUnit outDir
                                let invLen = 1. / float ch.Length
                                let invLenM1 = 1. / float (max 1 (ch.Length - 1))
                                for k = 0 to ch.Length - 1 do
                                    let c = ch[k]
                                    let zLocal =
                                        if isRoot then 1. - (2. * (float k + 0.5) * invLen) * 0.95
                                        else 0.98 - (float k * invLenM1) * 0.88
                                    let ring = sqrt (max 0. (1. - zLocal * zLocal))
                                    let az = float k * goldenAngle
                                    let localDir = Point3.create (ring * cos az) (ring * sin az) zLocal
                                    let worldDir = if isRoot then localDir else Quaternion.rotate orient localDir
                                    let cPt = Point3.create (pCenter.X + worldDir.X * rShell) (pCenter.Y + worldDir.Y * rShell) (pCenter.Z + worldDir.Z * rShell)
                                    centers[c] <- cPt
                                    qNodes[qTail] <- c
                                    qDirs[qTail] <- worldDir
                                    qTail <- qTail + 1

                let mutable cursorX = c0.X
                for r = 0 to rootIndices.Length - 1 do
                    let ri = rootIndices[r]
                    if r > 0 then
                        cursorX <- cursorX + coneRadius[rootIndices[r - 1]] + coneRadius[ri] + config.SiblingGap * 2.
                    let startOrigin =
                        match config.Mode with
                        | ConeTreeMode.VerticalCone ->
                            match config.Direction with
                            | VerticalDirection3D.Increasing -> Point3.create cursorX (Bounds3.minY config.Bounds + selfRadius[ri]) c0.Z
                            | VerticalDirection3D.Decreasing -> Point3.create cursorX (Bounds3.maxY config.Bounds - selfRadius[ri]) c0.Z
                        | ConeTreeMode.HorizontalCam ->
                            match config.Direction with
                            | VerticalDirection3D.Increasing -> Point3.create (Bounds3.minX config.Bounds + selfRadius[ri]) cursorX c0.Z
                            | VerticalDirection3D.Decreasing -> Point3.create (Bounds3.maxX config.Bounds - selfRadius[ri]) cursorX c0.Z
                        | ConeTreeMode.SphericalBalloon -> Point3.create cursorX c0.Y c0.Z
                    placeTree ri startOrigin

                let hasGroups = eff.Groups.Length > 0
                let nodeBoxes = if hasGroups then Array.zeroCreate<Bounds3> n else [||]
                let mutable posNodes = []
                for i = n - 1 downto 0 do
                    let nd = nodes[i]
                    let box = Kernels3D.nodeBox centers[i] nd.Size.Width nd.Size.Height nd.DepthSize
                    if hasGroups then nodeBoxes[i] <- box
                    posNodes <-
                        { Id = nd.Id
                          Kind = nd.Kind
                          Center = centers[i]
                          Bounds = box
                          Radius = selfRadius[i]
                          ScaleFactor = 1.
                          ParentGroup = nd.ParentGroup
                          Layer = Some depths[i] } :: posNodes

                let groups = if hasGroups then Kernels3D.computeGroups3D resolved nodeBoxes else []
                let mutable routed = []
                for e = assocs.Length - 1 downto 0 do
                    let a = assocs[e]
                    let u, v = csr.Src[e], csr.Dst[e]
                    let isTree = isTreeAssoc[e]
                    let pts, wps =
                        if u = v then
                            let c, r = centers[u], selfRadius[u]
                            let lp = Point3.create (c.X + r * 1.4) (c.Y - r * 1.4) (c.Z + r * 0.5)
                            [| c; lp; c |], [ c; lp; c ]
                        else
                            let cu, cv = centers[u], centers[v]
                            [| cu; cv |], [ cu; cv ]
                    routed <-
                        { Id = a.Id
                          Source = a.Source
                          Target = a.Target
                          Kind = a.Kind
                          IsBackbone = isTree
                          Points = Point3Chain.ofOwnedArray pts
                          Waypoints = wps } :: routed

                Ok
                    { Bounds = Kernels3D.computeBounds3D config.Bounds posNodes groups routed
                      Nodes = posNodes
                      Groups = groups
                      Associations = routed }

    /// <summary>Computes a 3D Cone, Cam or Spherical Balloon layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Settings for Tamara Munzner's H3 3D hyperbolic graph layout engine (IEEE InfoVis 1997 / CG&amp;A 1998).</summary>
type H3Config =
    { /// <summary>Unit-ball embedding model (<c>Klein</c> uses straight-chord geodesics as in H3; <c>Poincare</c> uses conformal circular arcs).</summary>
      Model: HyperbolicModel
      /// <summary>Euclidean radius in world units to which the open unit ball (<c>||v|| &lt; 1</c>) is scaled (default 300).</summary>
      BallRadius: float
      /// <summary>Optional preferred root node ID for the backbone spanning tree.</summary>
      RootId: string option
      /// <summary>Spanning-tree extraction policy over arbitrary directed/undirected graphs.</summary>
      SpanningStrategy: SpanningTreeStrategy
      /// <summary>Base hyperbolic disc radius allocated to a unit-size leaf node (default 0.12).</summary>
      LeafHyperbolicRadius: float
      /// <summary>Hemisphere packing area multiplier (&gt;= 1.1; default 1.45) accounting for spherical-cap packing gaps.</summary>
      HemisphereAreaScale: float
      /// <summary>Minimum hyperbolic radial step between parent and child hemispheres (default 0.28).</summary>
      RadialGap: float
      /// <summary>Minimum projected scale factor in [0, 1) below which tiny peripheral cross-links can be omitted when desired (0 keeps all).</summary>
      MinVisibleScale: float
      /// <summary>Number of arc sample segments when <c>Model = HyperbolicModel.Poincare</c> (default 10).</summary>
      PoincareArcSegments: int }

/// <summary>Constructors for <c>H3Config</c>.</summary>
[<RequireQualifiedAccess>]
module H3Config =
    /// <summary>Creates Munzner H3 defaults in the Beltrami-Klein model with 300-unit ball radius.</summary>
    let defaultValue =
        { Model = HyperbolicModel.Klein
          BallRadius = 300.
          RootId = None
          SpanningStrategy = SpanningTreeStrategy.Bfs
          LeafHyperbolicRadius = 0.12
          HemisphereAreaScale = 1.45
          RadialGap = 0.28
          MinVisibleScale = 0.
          PoincareArcSegments = 10 }

/// <summary>Persistent, immutable 3D hyperbolic layout state supporting O(N) SIMD interactive Focus+Context Lorentz boosts without re-running layout.</summary>
type HyperbolicLayoutState =
    internal
        { Config: H3Config
          Graph: DiagramGraph
          Nodes: DiagramNode[]
          NodeIndex: Dictionary<string, int>
          Associations: Association[]
          AssocSrc: int[]
          AssocDst: int[]
          IsBackbone: bool[]
          Depths: int[]
          BaseRadius: float[]
          BaseX: float[]
          BaseY: float[]
          BaseZ: float[]
          BaseW: float[]
          ViewTransform: HyperbolicTransform }

/// <summary>Ergonomic alias for <c>HyperbolicLayoutState</c> used with interactive H3 projection helpers.</summary>
type H3State = HyperbolicLayoutState

/// <summary>Tamara Munzner's H3 3D hyperbolic hemisphere-packing layout and interactive Lorentz focus engine.</summary>
[<RequireQualifiedAccess>]
module H3 =
    let private validateConfig (config: H3Config) =
        if not (Double.IsFinite config.BallRadius && Double.IsFinite config.LeafHyperbolicRadius
                && Double.IsFinite config.HemisphereAreaScale && Double.IsFinite config.RadialGap
                && Double.IsFinite config.MinVisibleScale)
           || config.BallRadius <= 0. || config.LeafHyperbolicRadius <= 0.
           || config.HemisphereAreaScale < 1.05 || config.RadialGap <= 0.
           || config.MinVisibleScale < 0. || config.MinVisibleScale >= 1.
           || config.PoincareArcSegments < 2 || config.PoincareArcSegments > 64 then
            Error [ InvalidObservation "H3 requires positive BallRadius/LeafHyperbolicRadius/RadialGap, HemisphereAreaScale >= 1.05, MinVisibleScale in [0,1) and 2..64 arc segments." ]
        else
            Ok()

    /// <summary>Projects a <c>HyperbolicLayoutState</c> under its current <c>ViewTransform</c> into a Euclidean <c>DiagramLayout3D</c> inside <c>[-BallRadius, +BallRadius]^3</c>.</summary>
    let project (state: HyperbolicLayoutState) : DiagramLayout3D =
        let cfg = state.Config
        let n = state.Nodes.Length
        let bRad = cfg.BallRadius
        let boundBox = Bounds3.create -bRad -bRad -bRad (2. * bRad) (2. * bRad) (2. * bRad)
        if n = 0 then
            { Bounds = boundBox; Nodes = []; Groups = Kernels3D.computeGroups3D state.Graph [||]; Associations = [] }
        else
            let unitScale = max 1e-6 (bRad / 300.0)
            let mutable maxInputDim = 0.
            for i = 0 to n - 1 do
                let nd = state.Nodes[i]
                if nd.Size.Width > maxInputDim then maxInputDim <- nd.Size.Width
                if nd.Size.Height > maxInputDim then maxInputDim <- nd.Size.Height
                if nd.DepthSize > maxInputDim then maxInputDim <- nd.DepthSize
            let dimScale =
                if maxInputDim > 0.5 * bRad then (0.5 * bRad) / maxInputDim
                else 1.0
            let inline projectNodeSizes (nd: DiagramNode) (baseRad: float) (atten: float) =
                let clampedScale = max 0.015 (min 1. atten)
                let effRad = max (1.2 * unitScale) (baseRad * clampedScale)
                let minBox = 2.4 * unitScale
                let w = if dimScale = 1.0 then nd.Size.Width else nd.Size.Width * dimScale
                let h = if dimScale = 1.0 then nd.Size.Height else nd.Size.Height * dimScale
                let d = if dimScale = 1.0 then nd.DepthSize else nd.DepthSize * dimScale
                let sx = max minBox (w * 0.25 * clampedScale)
                let sy = max minBox (h * 0.25 * clampedScale)
                let sz = max minBox (d * 0.25 * clampedScale)
                struct (effRad, sx, sy, sz)

            let worldCenters = Array.zeroCreate<Point3> n
            let attens = Array.zeroCreate<float> n
            let hasGroups = DiagramGraph.groupCount state.Graph > 0
            let nodeBoxes = if hasGroups then Array.zeroCreate<Bounds3> n else [||]
            let mutable minX, minY, minZ = infinity, infinity, infinity
            let mutable maxX, maxY, maxZ = -infinity, -infinity, -infinity

            match cfg.Model with
            | HyperbolicModel.Klein ->
                let wx = Array.zeroCreate<float> n
                let wy = Array.zeroCreate<float> n
                let wz = Array.zeroCreate<float> n
                HyperbolicTransform.projectKleinWorldSoA state.ViewTransform bRad state.BaseX state.BaseY state.BaseZ state.BaseW wx wy wz attens n

                let mutable posNodes = []
                for i = n - 1 downto 0 do
                    let nd = state.Nodes[i]
                    let cx, cy, cz = wx[i], wy[i], wz[i]
                    let atten = attens[i]
                    let worldCenter = Point3.create cx cy cz
                    worldCenters[i] <- worldCenter
                    let struct (effRad, sx, sy, sz) = projectNodeSizes nd state.BaseRadius[i] atten
                    let hx, hy, hz = sx * 0.5, sy * 0.5, sz * 0.5
                    if cx - hx < minX then minX <- cx - hx
                    if cy - hy < minY then minY <- cy - hy
                    if cz - hz < minZ then minZ <- cz - hz
                    if cx + hx > maxX then maxX <- cx + hx
                    if cy + hy > maxY then maxY <- cy + hy
                    if cz + hz > maxZ then maxZ <- cz + hz
                    let box = Bounds3.create (cx - hx) (cy - hy) (cz - hz) sx sy sz
                    if hasGroups then nodeBoxes[i] <- box
                    posNodes <-
                        { Id = nd.Id
                          Kind = nd.Kind
                          Center = worldCenter
                          Bounds = box
                          Radius = effRad
                          ScaleFactor = atten
                          ParentGroup = nd.ParentGroup
                          Layer = Some state.Depths[i] } :: posNodes

                let groups = if hasGroups then Kernels3D.computeGroups3D state.Graph nodeBoxes else []
                for g in groups do
                    let gb = g.Bounds
                    if Bounds3.minX gb < minX then minX <- Bounds3.minX gb
                    if Bounds3.minY gb < minY then minY <- Bounds3.minY gb
                    if Bounds3.minZ gb < minZ then minZ <- Bounds3.minZ gb
                    if Bounds3.maxX gb > maxX then maxX <- Bounds3.maxX gb
                    if Bounds3.maxY gb > maxY then maxY <- Bounds3.maxY gb
                    if Bounds3.maxZ gb > maxZ then maxZ <- Bounds3.maxZ gb

                let m = state.Associations.Length
                let minVis = cfg.MinVisibleScale
                let deltaLoop = 0.02 * bRad
                let mutable routed = []
                for e = m - 1 downto 0 do
                    let ui = state.AssocSrc[e]
                    let vi = state.AssocDst[e]
                    let isTree = state.IsBackbone[e]
                    if isTree || max attens[ui] attens[vi] >= minVis then
                        let a = state.Associations[e]
                        let pts, wps =
                            if ui = vi then
                                let c = worldCenters[ui]
                                let loopPt = Point3.create (c.X + deltaLoop) (c.Y + deltaLoop) c.Z
                                if loopPt.X > maxX then maxX <- loopPt.X
                                if loopPt.Y > maxY then maxY <- loopPt.Y
                                if loopPt.X < minX then minX <- loopPt.X
                                if loopPt.Y < minY then minY <- loopPt.Y
                                [| c; loopPt; c |], [ c; loopPt; c ]
                            else
                                // Klein geodesics are exact Euclidean chords between worldCenters[ui] and worldCenters[vi],
                                // whose coordinates are already strictly inside nodeBoxes[ui] and nodeBoxes[vi]!
                                let cu, cv = worldCenters[ui], worldCenters[vi]
                                [| cu; cv |], [ cu; cv ]
                        routed <-
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              IsBackbone = isTree
                              Points = Point3Chain.ofOwnedArray pts
                              Waypoints = wps } :: routed

                { Bounds = Bounds3.create minX minY minZ (maxX - minX) (maxY - minY) (maxZ - minZ)
                  Nodes = posNodes
                  Groups = groups
                  Associations = routed }

            | HyperbolicModel.Poincare ->
                let tx = Array.zeroCreate<float> n
                let ty = Array.zeroCreate<float> n
                let tz = Array.zeroCreate<float> n
                let tw = Array.zeroCreate<float> n
                HyperbolicTransform.applySoA state.ViewTransform state.BaseX state.BaseY state.BaseZ state.BaseW tx ty tz tw n

                let minkPts = Array.init n (fun i -> MinkowskiPoint(tx[i], ty[i], tz[i], tw[i]))
                let mutable posNodes = []
                for i = n - 1 downto 0 do
                    let nd = state.Nodes[i]
                    let unitBallPt, atten = Hyperbolic3D.toBall HyperbolicModel.Poincare minkPts[i]
                    attens[i] <- atten
                    let worldCenter = Point3.create (unitBallPt.X * bRad) (unitBallPt.Y * bRad) (unitBallPt.Z * bRad)
                    worldCenters[i] <- worldCenter
                    let struct (effRad, sx, sy, sz) = projectNodeSizes nd state.BaseRadius[i] atten
                    let box = Kernels3D.nodeBox worldCenter sx sy sz
                    if hasGroups then nodeBoxes[i] <- box
                    posNodes <-
                        { Id = nd.Id
                          Kind = nd.Kind
                          Center = worldCenter
                          Bounds = box
                          Radius = effRad
                          ScaleFactor = atten
                          ParentGroup = nd.ParentGroup
                          Layer = Some state.Depths[i] } :: posNodes

                let groups = if hasGroups then Kernels3D.computeGroups3D state.Graph nodeBoxes else []
                let m = state.Associations.Length
                let minVis = cfg.MinVisibleScale
                let mutable routed = []
                for e = m - 1 downto 0 do
                    let ui = state.AssocSrc[e]
                    let vi = state.AssocDst[e]
                    let isTree = state.IsBackbone[e]
                    if isTree || max attens[ui] attens[vi] >= minVis then
                        let a = state.Associations[e]
                        let pts, wps =
                            if ui = vi then
                                let c = worldCenters[ui]
                                let lp = Point3.create (c.X + 0.02 * bRad) (c.Y + 0.02 * bRad) c.Z
                                [| c; lp; c |], [ c; lp; c ]
                            else
                                let unitChain = Hyperbolic3D.geodesicPoints HyperbolicModel.Poincare cfg.PoincareArcSegments minkPts[ui] minkPts[vi]
                                let len = unitChain.Length
                                let arr = Array.zeroCreate<Point3> len
                                let mutable wpL = []
                                for k = len - 1 downto 0 do
                                    let p = unitChain[k]
                                    let wp = Point3.create (p.X * bRad) (p.Y * bRad) (p.Z * bRad)
                                    arr[k] <- wp
                                    wpL <- wp :: wpL
                                arr, wpL
                        routed <-
                            { Id = a.Id
                              Source = a.Source
                              Target = a.Target
                              Kind = a.Kind
                              IsBackbone = isTree
                              Points = Point3Chain.ofOwnedArray pts
                              Waypoints = wps } :: routed

                { Bounds = Kernels3D.computeBounds3D boundBox posNodes groups routed
                  Nodes = posNodes
                  Groups = groups
                  Associations = routed }

    /// <summary>Returns the current <c>HyperbolicTransform</c> view matrix of the state.</summary>
    let viewTransform (state: HyperbolicLayoutState) = state.ViewTransform

    /// <summary>Projects all node centers from <c>state</c> under <c>model</c> and <c>transform</c> directly into <c>destination</c> with zero heap allocations, returning the number of points written.</summary>
    let projectCentersInto (model: HyperbolicModel) (transform: HyperbolicTransform) (state: H3State) (destination: Span<Point3>) : int =
        if isNull (box state) then
            nullArg (nameof state)
        if isNull (box transform) then
            nullArg (nameof transform)
        let n = state.Nodes.Length
        if destination.Length < n then
            raise (ArgumentException("Destination span length must be at least the number of nodes in state.", nameof destination))
        if n > 0 then
            HyperbolicTransform.projectBallWorldSpan model transform state.Config.BallRadius state.BaseX state.BaseY state.BaseZ state.BaseW n destination
        n

    /// <summary>Returns the transformed <c>MinkowskiPoint</c> of <c>nodeId</c> under the current view, or <c>None</c> if unknown.</summary>
    let tryNodeMinkowski (nodeId: string) (state: HyperbolicLayoutState) : MinkowskiPoint option =
        match state.NodeIndex.TryGetValue nodeId with
        | true, idx ->
            let raw = MinkowskiPoint(state.BaseX[idx], state.BaseY[idx], state.BaseZ[idx], state.BaseW[idx])
            Some(HyperbolicTransform.apply state.ViewTransform raw)
        | _ -> None

    /// <summary>Focuses <c>nodeId</c> isometrically at the center of the 3D hyperbolic ball via an exact Lorentz boost in O(1) state update.</summary>
    let focusNode (nodeId: string) (state: HyperbolicLayoutState) : Result<HyperbolicLayoutState, LayoutError list> =
        match tryNodeMinkowski nodeId state with
        | None -> Error [ MissingEndpoint nodeId ]
        | Some currentPos ->
            let boost = HyperbolicTransform.boostToOrigin currentPos
            let nextView = state.ViewTransform |> HyperbolicTransform.thenApply boost |> HyperbolicTransform.orthonormalize
            Ok { state with ViewTransform = nextView }

    /// <summary>Applies an incremental 3D <c>Quaternion</c> rotation around the hyperbolic ball origin.</summary>
    let rotate (rotation: Quaternion) (state: HyperbolicLayoutState) : HyperbolicLayoutState =
        let rot = HyperbolicTransform.ofRotation rotation
        { state with ViewTransform = state.ViewTransform |> HyperbolicTransform.thenApply rot |> HyperbolicTransform.orthonormalize }

    /// <summary>Replaces the view transform directly (or resets with <c>HyperbolicTransform.identity</c>).</summary>
    let withViewTransform (transform: HyperbolicTransform) (state: HyperbolicLayoutState) : HyperbolicLayoutState =
        { state with ViewTransform = HyperbolicTransform.orthonormalize transform }

    /// <summary>Computes the persistent <c>HyperbolicLayoutState</c> in H^3 using Munzner's bottom-up hemisphere synthesis and top-down spherical-cap band packing.</summary>
    let buildStateWithCancellation
        (token: System.Threading.CancellationToken)
        (config: H3Config)
        (graph: DiagramGraph)
        : Result<HyperbolicLayoutState, LayoutError list> =
        token.ThrowIfCancellationRequested()
        match validateConfig config with
        | Error e -> Error e
        | Ok () ->
            let resolved = DiagramGraph.resolveCollapsed graph
            let eff = DiagramGraph.effective resolved
            let nodes = eff.Nodes
            let assocs = eff.Associations
            let n = nodes.Length
            let nodeIdx = eff.ReversedIdMap

            if n = 0 then
                Ok
                    { Config = config
                      Graph = resolved
                      Nodes = [||]
                      NodeIndex = nodeIdx
                      Associations = [||]
                      AssocSrc = [||]
                      AssocDst = [||]
                      IsBackbone = [||]
                      Depths = [||]
                      BaseRadius = [||]
                      BaseX = [||]; BaseY = [||]; BaseZ = [||]; BaseW = [||]
                      ViewTransform = HyperbolicTransform.identity }
            else
                let csr = GraphKernels.buildCsr eff
                let rootIndices, _, children, depths, isBackbone =
                    SpanningTree.extractIndexed config.SpanningStrategy config.RootId eff csr

                // Compute normalized visual size weight per node in single pass
                let diagArr = Array.zeroCreate<float> n
                let mutable diagSum = 0.
                for i = 0 to n - 1 do
                    let nd = nodes[i]
                    let d = sqrt (nd.Size.Width * nd.Size.Width + nd.Size.Height * nd.Size.Height)
                    diagArr[i] <- d
                    diagSum <- diagSum + d
                let meanDiag = diagSum / float n
                let normScale =
                    if meanDiag <= 1e-6 then Array.create n 1.
                    else
                        let invMean = 1. / meanDiag
                        Array.init n (fun i -> max 0.5 (min 2.5 (diagArr[i] * invMean)))

                // Postorder traversal across forest via flat stack
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

                // Pass 1 (Bottom-Up): Munzner H3 Appendix A hyperbolic hemisphere radius synthesis
                let hemiRadius = Array.zeroCreate<float> n
                for idx = 0 to postLen - 1 do
                    Kernels3D.check token idx
                    let u = postorder[idx]
                    let rLeaf = config.LeafHyperbolicRadius * normScale[u]
                    let ch = children[u]
                    if ch.Length = 0 then
                        hemiRadius[u] <- rLeaf
                    else
                        let mutable sSqSum = 0.
                        for k = 0 to ch.Length - 1 do
                            let s = sinh (min 12. hemiRadius[ch[k]])
                            sSqSum <- sSqSum + s * s
                        // H_p = AreaScale * sum(2 * pi * s^2) => H_p / (2 * pi) = AreaScale * sSqSum!
                        let reqR = Math.Asinh(sqrt (config.HemisphereAreaScale * sSqSum))
                        hemiRadius[u] <- max rLeaf (min 12. reqR)

                // Pass 2 (Top-Down): Concentric spherical-cap band packing on parent hemispheres in H^3
                let baseX = Array.zeroCreate<float> n
                let baseY = Array.zeroCreate<float> n
                let baseZ = Array.zeroCreate<float> n
                let baseW = Array.create n 1.
                let zUnit = Point3.create 0. 0. 1.

                let qNodes = Array.zeroCreate<int> n
                let qFrames = Array.zeroCreate<HyperbolicTransform> n
                let mutable qHead = 0
                let mutable qTail = 0
                let bandMembers = ResizeArray<struct(int * float)>(32)

                let packChildrenOnCap (parentFrame: HyperbolicTransform) (parentR: float) (maxPhi: float) (kids: int[]) =
                    let kLen = kids.Length
                    if kLen > 0 then
                        let sorted =
                            if kLen = 1 then kids
                            elif kLen <= 16 then
                                let cp = Array.copy kids
                                for a = 1 to kLen - 1 do
                                    let kv = cp[a]
                                    let kr = hemiRadius[kv]
                                    let mutable b = a - 1
                                    while b >= 0 && (hemiRadius[cp[b]] < kr || (hemiRadius[cp[b]] = kr && cp[b] > kv)) do
                                        cp[b + 1] <- cp[b]
                                        b <- b - 1
                                    cp[b + 1] <- kv
                                cp
                            else
                                let cp = Array.copy kids
                                Array.Sort(cp, Comparer<int>.Create(fun a b ->
                                    let c = compare hemiRadius[b] hemiRadius[a]
                                    if c <> 0 then c else compare a b))
                                cp

                        let sinhRp = max 0.05 (sinh (min 12. parentR))
                        let invSinhRp = 1. / sinhRp
                        let mutable cursorIdx = 0
                        let mutable phi = 0.

                        if kLen = 1 || maxPhi <= Math.PI * 0.6 then
                            let poleChild = sorted[0]
                            let rc = hemiRadius[poleChild]
                            let stepD = parentR + rc + config.RadialGap
                            let localPt = Hyperbolic3D.fromSpherical stepD 0. 0.
                            let worldPt = HyperbolicTransform.apply parentFrame localPt
                            baseX[poleChild] <- worldPt.X; baseY[poleChild] <- worldPt.Y
                            baseZ[poleChild] <- worldPt.Z; baseW[poleChild] <- worldPt.W
                            let dPhi0 = atan (tanh rc * invSinhRp)
                            phi <- max 0.18 (2. * dPhi0 + 0.06)
                            cursorIdx <- 1
                            if children[poleChild].Length > 0 then
                                let cFrame =
                                    HyperbolicTransform.boostFromOrigin localPt
                                    |> HyperbolicTransform.thenApply parentFrame
                                    |> HyperbolicTransform.orthonormalize
                                qNodes[qTail] <- poleChild
                                qFrames[qTail] <- cFrame
                                qTail <- qTail + 1
                        else
                            phi <- 0.35

                        while cursorIdx < kLen do
                            let firstC = sorted[cursorIdx]
                            let dPhiFirst = atan (tanh hemiRadius[firstC] * invSinhRp)
                            let bandPhi = min maxPhi (max 0.12 (phi + dPhiFirst))
                            let spBand = sin bandPhi
                            let cpBand = cos bandPhi
                            let sinhRPhi = max 0.04 (sinhRp * sin (min (Math.PI - 0.08) bandPhi))
                            let invSinhRPhi = 1. / sinhRPhi

                            bandMembers.Clear()
                            let mutable thetaUsed = 0.
                            let mutable bandMaxDPhi = dPhiFirst
                            let mutable bandFull = false
                            while cursorIdx < kLen && not bandFull do
                                let c = sorted[cursorIdx]
                                let rc = hemiRadius[c]
                                let tanhRc = tanh rc
                                let dTheta = max 0.08 (atan (tanhRc * invSinhRPhi))
                                let dPhi = atan (tanhRc * invSinhRp)
                                if bandMembers.Count > 0 && thetaUsed + 2. * dTheta > 2. * Math.PI + 1e-6 then
                                    bandFull <- true
                                else
                                    bandMembers.Add(struct(c, dTheta))
                                    thetaUsed <- thetaUsed + 2. * dTheta
                                    if dPhi > bandMaxDPhi then bandMaxDPhi <- dPhi
                                    cursorIdx <- cursorIdx + 1

                            let bmCount = bandMembers.Count
                            let slack = if thetaUsed < 2. * Math.PI then (2. * Math.PI - thetaUsed) / float bmCount else 0.
                            let scaleAz = if thetaUsed > 2. * Math.PI then (2. * Math.PI) / thetaUsed else 1.
                            let mutable theta = (float bmCount * 0.61803398875) % (2. * Math.PI)
                            for m = 0 to bmCount - 1 do
                                let struct(c, dTheta) = bandMembers[m]
                                let halfSpan = dTheta * scaleAz + slack * 0.5
                                let az = theta + halfSpan
                                theta <- theta + 2. * halfSpan

                                let stepD = parentR + hemiRadius[c] + config.RadialGap
                                let localPt = Hyperbolic3D.fromSpherical stepD bandPhi az
                                if children[c].Length > 0 then
                                    let dir = Point3.create (spBand * cos az) (spBand * sin az) cpBand
                                    let orient = Quaternion.fromUnitVectors zUnit dir
                                    let localFrame =
                                        HyperbolicTransform.ofRotation orient
                                        |> HyperbolicTransform.thenApply (HyperbolicTransform.boostFromOrigin localPt)
                                    let cFrame =
                                        localFrame
                                        |> HyperbolicTransform.thenApply parentFrame
                                        |> HyperbolicTransform.orthonormalize
                                    let worldOrigin = HyperbolicTransform.apply cFrame Hyperbolic3D.origin
                                    baseX[c] <- worldOrigin.X; baseY[c] <- worldOrigin.Y
                                    baseZ[c] <- worldOrigin.Z; baseW[c] <- worldOrigin.W
                                    qNodes[qTail] <- c
                                    qFrames[qTail] <- cFrame
                                    qTail <- qTail + 1
                                else
                                    // Leaf node: cFrame * origin == parentFrame * (boostFromOrigin localPt * origin) == parentFrame * localPt!
                                    let worldOrigin = HyperbolicTransform.apply parentFrame localPt
                                    baseX[c] <- worldOrigin.X; baseY[c] <- worldOrigin.Y
                                    baseZ[c] <- worldOrigin.Z; baseW[c] <- worldOrigin.W

                            phi <- min (maxPhi - 0.05) (bandPhi + bandMaxDPhi + 0.08)

                if rootIndices.Length = 1 then
                    let r0 = rootIndices[0]
                    baseX[r0] <- 0.; baseY[r0] <- 0.; baseZ[r0] <- 0.; baseW[r0] <- 1.
                    packChildrenOnCap HyperbolicTransform.identity (hemiRadius[r0] * 0.5) (Math.PI * 0.88) children[r0]
                else
                    let virtualR =
                        let mutable sSq = 0.
                        for r in rootIndices do
                            let s = sinh (min 10. hemiRadius[r])
                            sSq <- sSq + s * s
                        Math.Asinh(sqrt (config.HemisphereAreaScale * sSq * 0.5))
                    packChildrenOnCap HyperbolicTransform.identity (max 0.25 virtualR) (Math.PI * 0.88) rootIndices

                while qHead < qTail do
                    Kernels3D.check token qHead
                    let u = qNodes[qHead]
                    let uFrame = qFrames[qHead]
                    qHead <- qHead + 1
                    packChildrenOnCap uFrame hemiRadius[u] (Math.PI * 0.48) children[u]

                let unitScale = max 1e-6 (config.BallRadius / 300.0)
                let baseRadii = Array.init n (fun i -> max (3.5 * unitScale) (10. * unitScale * normScale[i]))

                Ok
                    { Config = config
                      Graph = resolved
                      Nodes = nodes
                      NodeIndex = nodeIdx
                      Associations = assocs
                      AssocSrc = csr.Src
                      AssocDst = csr.Dst
                      IsBackbone = isBackbone
                      Depths = depths
                      BaseRadius = baseRadii
                      BaseX = baseX
                      BaseY = baseY
                      BaseZ = baseZ
                      BaseW = baseW
                      ViewTransform = HyperbolicTransform.identity }

    /// <summary>Builds the persistent <c>HyperbolicLayoutState</c> in H^3 without cancellation.</summary>
    let buildState config graph = buildStateWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Computes a projected 3D hyperbolic Munzner H3 layout with cooperative cancellation.</summary>
    let layoutWithCancellation token config graph =
        buildStateWithCancellation token config graph |> Result.map project

    /// <summary>Computes a projected 3D hyperbolic Munzner H3 layout without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

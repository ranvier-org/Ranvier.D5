namespace Ranvier.D5.Graph3D

open System
open System.Collections.Generic
open System.Threading
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Configuration for lifting a 2D <c>PoincareDiskState</c> into a 3D conformal Poincaré ball and morphing continuously between the 3D ball (<c>fold = 0</c>) and flat 2D disk (<c>fold = 1</c>).</summary>
type PoincareBallLiftConfig =
    { /// <summary>3D world center of the Poincaré ball.</summary>
      BallCenter: Point3
      /// <summary>Positive Euclidean radius of the Poincaré ball in world units.</summary>
      BallRadius: float
      /// <summary>Hyperbolic radial stretch multiplier (<c>&gt;= 0.1</c>; default <c>1.6</c>) applied to <c>atanh(rho)</c> in 3D.</summary>
      RadialBoost: float
      /// <summary>Number of polyline segments per curved 3D geodesic association (<c>&gt;= 4</c>; default <c>14</c>).</summary>
      ArcSegments: int
      /// <summary>Yaw rotation angle in radians around the Y axis applied at <c>fold = 0</c>.</summary>
      Yaw: float
      /// <summary>Pitch rotation angle in radians around the X axis applied at <c>fold = 0</c>.</summary>
      Pitch: float
      /// <summary>32-bit FNV-1a salt controlling deterministic spherical elevation angles per node ID (default <c>0x811C9DC5u</c>).</summary>
      ElevationSalt: uint32 }

/// <summary>Constructors and defaults for <c>PoincareBallLiftConfig</c>.</summary>
[<RequireQualifiedAccess>]
module PoincareBallLiftConfig =
    /// <summary>Creates a 3D Poincaré ball lift configuration centered at <c>center</c> with <c>radius</c>, radial boost <c>1.6</c>, <c>14</c> arc segments, zero yaw/pitch and FNV-1a salt <c>0x811C9DC5u</c>.</summary>
    let create (center: Point3) (radius: float) : PoincareBallLiftConfig =
        { BallCenter = center
          BallRadius = radius
          RadialBoost = 1.6
          ArcSegments = 14
          Yaw = 0.
          Pitch = 0.
          ElevationSalt = 0x811C9DC5u }

/// <summary>Conformal 3D Poincaré ball lift and continuous 3D-to-2D hyperbolic disk morphing over <c>PoincareDiskState</c>.</summary>
[<RequireQualifiedAccess>]
module PoincareBallLift =
    let private slerpFromIdentity (target: Quaternion) (t: float) : Quaternion =
        if t <= 1e-12 then
            Quaternion.identity
        elif t >= 1. - 1e-12 then
            Quaternion.normalize target
        else
            let q1 =
                let nq = Quaternion.normalize target
                if nq.W < 0. then Quaternion(-nq.X, -nq.Y, -nq.Z, -nq.W) else nq
            let cosHalf = min 1. q1.W
            if cosHalf >= 1. - 1e-8 then
                Quaternion.normalize (Quaternion(q1.X * t, q1.Y * t, q1.Z * t, (1. - t) + t * q1.W))
            else
                let halfTheta = acos cosHalf
                let invSin = 1. / sin halfTheta
                let w0 = sin ((1. - t) * halfTheta) * invSin
                let w1 = sin (t * halfTheta) * invSin
                Quaternion.normalize (Quaternion(w1 * q1.X, w1 * q1.Y, w1 * q1.Z, w0 + w1 * q1.W))

    let private sampleBallGeodesic
        (center: Point3)
        (bRad: float)
        (segments: int)
        (uA: Point3)
        (uB: Point3)
        (worldA: Point3)
        (worldB: Point3)
        : Point3[] * Point3 list =
        let count = segments + 1
        let pts = Array.zeroCreate<Point3> count
        pts[0] <- worldA
        pts[segments] <- worldB

        let dx = uB.X - uA.X
        let dy = uB.Y - uA.Y
        let dz = uB.Z - uA.Z
        let chordSq = dx * dx + dy * dy + dz * dz

        if chordSq <= 1e-16 then
            // Self-loop flare strictly inside the open ball
            let rA = sqrt (uA.X * uA.X + uA.Y * uA.Y + uA.Z * uA.Z)
            let flare = min (0.04 * bRad) (max (0.01 * bRad) (0.35 * (1. - rA) * bRad))
            for k = 1 to segments - 1 do
                let ang = 2. * Math.PI * float k / float segments
                let s = sin (Math.PI * float k / float segments)
                pts[k] <-
                    Point3.create
                        (worldA.X + flare * s * cos ang)
                        (worldA.Y + flare * s * sin ang)
                        worldA.Z
        else
            let r2A = min 0.999999 (uA.X * uA.X + uA.Y * uA.Y + uA.Z * uA.Z)
            let r2B = min 0.999999 (uB.X * uB.X + uB.Y * uB.Y + uB.Z * uB.Z)
            let scaleA = 2. / max 1e-9 (1. - r2A)
            let scaleB = 2. / max 1e-9 (1. - r2B)
            let mA = Hyperbolic3D.ofSpacelike (uA.X * scaleA) (uA.Y * scaleA) (uA.Z * scaleA)
            let mB = Hyperbolic3D.ofSpacelike (uB.X * scaleB) (uB.Y * scaleB) (uB.Z * scaleB)
            let hypDist = Hyperbolic3D.distance mA mB

            if hypDist <= 1e-5 then
                for k = 1 to segments - 1 do
                    pts[k] <- Point3.lerp worldA worldB (float k / float segments)
            else
                let invSinh = 1. / sinh hypDist
                for k = 1 to segments - 1 do
                    let t = float k / float segments
                    let wa = sinh ((1. - t) * hypDist) * invSinh
                    let wb = sinh (t * hypDist) * invSinh
                    let mPt =
                        Hyperbolic3D.ofSpacelike
                            (wa * mA.X + wb * mB.X)
                            (wa * mA.Y + wb * mB.Y)
                            (wa * mA.Z + wb * mB.Z)
                    let invDenom = 1. / (1. + max 1. mPt.W)
                    let ux = mPt.X * invDenom
                    let uy = mPt.Y * invDenom
                    let uz = mPt.Z * invDenom
                    pts[k] <-
                        Point3.create
                            (center.X + bRad * ux)
                            (center.Y + bRad * uy)
                            (center.Z + bRad * uz)

        let mutable wps = []
        for k = count - 1 downto 0 do
            wps <- pts[k] :: wps
        pts, wps

    /// <summary>Computes a deterministic spherical elevation angle in <c>(-pi/2, pi/2)</c> from <c>salt</c> and UTF-16 code units of <c>id</c> via 32-bit FNV-1a hashing and Archimedes equal-area <c>asin</c> mapping.</summary>
    let fnv1aElevation (salt: uint32) (id: string) : float =
        let s = if isNull id then "" else id
        let mutable hash = (salt ^^^ 2166136261u) * 16777619u ^^^ 2166136261u
        for i = 0 to s.Length - 1 do
            let code = uint32 (uint16 s[i])
            hash <- (hash ^^^ (code &&& 0xFFu)) * 16777619u
            hash <- (hash ^^^ (code >>> 8)) * 16777619u
        hash <- hash ^^^ (hash >>> 16)
        hash <- hash * 0x85EBCA6Bu
        hash <- hash ^^^ (hash >>> 13)
        hash <- hash * 0xC2B2AE35u
        hash <- hash ^^^ (hash >>> 16)
        let u = (float hash + 0.5) / 4294967296.0
        let z = max -0.999999 (min 0.999999 (2.0 * u - 1.0))
        asin z

    /// <summary>Lifts a 2D <c>PoincareDiskState</c> into a 3D conformal Poincaré ball at morph parameter <c>fold</c> (<c>0</c> = full 3D ball, <c>1</c> = flat <c>Z = BallCenter.Z</c> 2D Poincaré disk) with cooperative cancellation.</summary>
    let liftWithCancellation
        (token: CancellationToken)
        (fold: float)
        (config: PoincareBallLiftConfig)
        (diskState: PoincareDiskState)
        : DiagramLayout3D =
        token.ThrowIfCancellationRequested()
        let f = max 0. (min 1. (if Double.IsFinite fold then fold else 0.))
        let bRad =
            if Double.IsFinite config.BallRadius && config.BallRadius > 1e-6 then config.BallRadius
            else 1.
        let center = config.BallCenter
        let boost =
            if Double.IsFinite config.RadialBoost then max 0.1 config.RadialBoost
            else 1.6
        let segs = max 4 (min 128 config.ArcSegments)
        let yaw = if Double.IsFinite config.Yaw then config.Yaw else 0.
        let pitch = if Double.IsFinite config.Pitch then config.Pitch else 0.

        let resolved = DiagramGraph.resolveCollapsed (PoincareDisk.graph diskState)
        let eff = DiagramGraph.effective resolved
        let coordsRaw =
            PoincareDisk.nodes diskState
            |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))
            |> List.toArray
        let n = coordsRaw.Length
        let ballBox = Kernels3D.nodeBox center (2. * bRad) (2. * bRad) (2. * bRad)

        if n = 0 then
            { Bounds = ballBox
              Nodes = []
              Groups = Kernels3D.computeGroups3D resolved [||]
              Associations = [] }
        else
            let qYaw = Quaternion.fromAxisAngle (Point3.create 0. 1. 0.) (yaw * (1. - f))
            let qPitch = Quaternion.fromAxisAngle (Point3.create 1. 0. 0.) (pitch * (1. - f))
            let qFull = Quaternion.multiply qPitch qYaw
            let qRot = slerpFromIdentity qFull (1. - f)

            let diskRad = max 1. (PoincareDisk.diskRadius diskState)
            let worldScale = bRad / diskRad

            let coordIdx = Dictionary<string, int>(StringComparer.Ordinal)
            for i = 0 to n - 1 do
                if not (coordIdx.ContainsKey coordsRaw[i].Id) then
                    coordIdx.Add(coordsRaw[i].Id, i)

            let unitPts = Array.zeroCreate<Point3> n
            let worldCenters = Array.zeroCreate<Point3> n
            let rings = Array.zeroCreate<int> n
            let hasGroups = eff.Groups.Length > 0
            let nodeBoxes = if hasGroups then Array.zeroCreate<Bounds3> eff.Nodes.Length else [||]
            let mutable posNodes = []

            for i = n - 1 downto 0 do
                Kernels3D.check token i
                let nc = coordsRaw[i]
                rings[i] <- nc.Ring

                let rawDx = if Double.IsFinite nc.DiskPosition.X then nc.DiskPosition.X else 0.
                let rawDy = if Double.IsFinite nc.DiskPosition.Y then nc.DiskPosition.Y else 0.
                let rhoRaw = sqrt (rawDx * rawDx + rawDy * rawDy)
                let dx, dy, rho =
                    if rhoRaw <= 0.999999 then
                        rawDx, rawDy, rhoRaw
                    else
                        let inv = 0.999999 / rhoRaw
                        rawDx * inv, rawDy * inv, 0.999999

                let alpha = atan2 dy dx
                let beta = fnv1aElevation config.ElevationSalt nc.Id
                let rho3 =
                    if rho <= 0. then 0.
                    else min 0.995 (tanh (Hyperbolic3D.atanhSafe rho * boost))

                let cb = cos beta
                let p3Unrot = Point3.create (rho3 * cb * cos alpha) (rho3 * cb * sin alpha) (rho3 * sin beta)
                let p3Rot = Quaternion.rotate qRot p3Unrot

                let ux = p3Rot.X + (dx - p3Rot.X) * f
                let uy = p3Rot.Y + (dy - p3Rot.Y) * f
                let uz = p3Rot.Z * (1. - f)
                let uPt = Point3.create ux uy uz
                unitPts[i] <- uPt

                let wPt =
                    Point3.create
                        (center.X + bRad * ux)
                        (center.Y + bRad * uy)
                        (center.Z + bRad * uz)
                worldCenters[i] <- wPt

                let oneMinusRho2 = max 1e-6 (1. - rho * rho)
                let uNorm2 = min 0.999999 (ux * ux + uy * uy + uz * uz)
                let scale3 = max 1e-6 (1. - uNorm2) / oneMinusRho2
                let conformalAtten = max 0.03 (min 1. (oneMinusRho2 * scale3))

                let kind, parentGroup, depthBase =
                    match eff.ReversedIdMap.TryGetValue nc.Id with
                    | true, effIdx ->
                        let dn = eff.Nodes[effIdx]
                        dn.Kind, dn.ParentGroup, dn.DepthSize
                    | _ ->
                        NodeKind.Standard, None, min nc.BaseWidth nc.BaseHeight

                let rawDiag = 0.5 * sqrt (nc.BaseWidth * nc.BaseWidth + nc.BaseHeight * nc.BaseHeight)
                let baseRadWorld = max (0.045 * bRad) (min (0.14 * bRad) (rawDiag * worldScale * 0.55))
                let effRad = max 1.2 (baseRadWorld * conformalAtten)

                let sx = max 2.0 (nc.BaseWidth * worldScale * 0.55 * conformalAtten)
                let sy = max 2.0 (nc.BaseHeight * worldScale * 0.55 * conformalAtten)
                let sz = max 2.0 (depthBase * worldScale * 0.55 * conformalAtten)
                let box = Kernels3D.nodeBox wPt sx sy sz
                if hasGroups then
                    match eff.ReversedIdMap.TryGetValue nc.Id with
                    | true, effIdx -> nodeBoxes[effIdx] <- box
                    | _ -> ()

                posNodes <-
                    { Id = nc.Id
                      Kind = kind
                      Center = wPt
                      Bounds = box
                      Radius = effRad
                      ScaleFactor = conformalAtten
                      ParentGroup = parentGroup
                      Layer = Some nc.Ring } :: posNodes

            let groups = if hasGroups then Kernels3D.computeGroups3D resolved nodeBoxes else []

            let assocs = eff.Associations
            let mutable routed = []
            for e = assocs.Length - 1 downto 0 do
                Kernels3D.check token e
                let a = assocs[e]
                match coordIdx.TryGetValue a.Source.NodeId, coordIdx.TryGetValue a.Target.NodeId with
                | (true, ui), (true, vi) ->
                    let pts, wps =
                        sampleBallGeodesic center bRad segs (unitPts[ui]) (unitPts[vi]) (worldCenters[ui]) (worldCenters[vi])
                    let isBackbone = abs (rings[ui] - rings[vi]) <= 1
                    routed <-
                        { Id = a.Id
                          Source = a.Source
                          Target = a.Target
                          Kind = a.Kind
                          IsBackbone = isBackbone
                          Points = Point3Chain.ofOwnedArray pts
                          Waypoints = wps } :: routed
                | _ -> ()

            { Bounds = Kernels3D.computeBounds3D ballBox posNodes groups routed
              Nodes = posNodes
              Groups = groups
              Associations = routed }

    /// <summary>Lifts a 2D <c>PoincareDiskState</c> into a full 3D conformal Poincaré ball (<c>fold = 0.0</c>) without cancellation.</summary>
    let lift (config: PoincareBallLiftConfig) (diskState: PoincareDiskState) : DiagramLayout3D =
        liftWithCancellation CancellationToken.None 0.0 config diskState

    /// <summary>Morphs a lifted 3D Poincaré ball continuously at <c>fold</c> in <c>[0, 1]</c> (<c>0</c> = 3D ball, <c>1</c> = flat 2D Poincaré disk).</summary>
    let morph (fold: float) (config: PoincareBallLiftConfig) (diskState: PoincareDiskState) : DiagramLayout3D =
        liftWithCancellation CancellationToken.None fold config diskState

    /// <summary>Builds a 2D <c>PoincareDiskState</c> from <c>diskConfig</c> and <c>graph</c> and lifts it into a 3D conformal Poincaré ball.</summary>
    let layout
        (config: PoincareBallLiftConfig)
        (diskConfig: PoincareDiskConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout3D, LayoutError list> =
        PoincareDisk.buildState diskConfig graph |> Result.map (lift config)

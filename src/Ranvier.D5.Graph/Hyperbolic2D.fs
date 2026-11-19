namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>A point strictly inside the open unit Poincaré disk (<c>X^2 + Y^2 &lt; 1</c>) representing 2D hyperbolic space <c>H^2</c>.</summary>
[<Struct>]
type PoincarePoint =
    { /// <summary>Horizontal Euclidean coordinate inside the open unit disk <c>(-1, 1)</c>.</summary>
      X: float
      /// <summary>Vertical Euclidean coordinate inside the open unit disk <c>(-1, 1)</c>.</summary>
      Y: float }

/// <summary>Exact 2D Poincaré disk hyperbolic geometry, Lamping-Rao-Pirolli Möbius focus automorphisms and orthogonal boundary geodesic arcs.</summary>
[<RequireQualifiedAccess>]
module Hyperbolic2D =
    let inline internal acoshSafe (x: float) : float =
        let c = max 1. x
#if FABLE_COMPILER
        log (c + sqrt ((c - 1.) * (c + 1.)))
#else
        Math.Acosh c
#endif

    let inline internal atanhSafe (x: float) : float =
        let c = max -0.999999999999 (min 0.999999999999 x)
#if FABLE_COMPILER
        0.5 * log ((1. + c) / (1. - c))
#else
        Math.Atanh c
#endif

    let inline internal asinhNonneg (x: float) : float =
        let a = max 0. x
#if FABLE_COMPILER
        log (a + sqrt (a * a + 1.))
#else
        Math.Asinh a
#endif

    let inline internal clampDiskPoint (p: PoincarePoint) : PoincarePoint =
        if not (Double.IsFinite p.X && Double.IsFinite p.Y) then
            { PoincarePoint.X = 0.; Y = 0. }
        else
            let maxR = 1. - 1e-12
            let r = Double.Hypot(p.X, p.Y)
            if r > maxR && r > 0. then
                let s = maxR / r
                { PoincarePoint.X = p.X * s; Y = p.Y * s }
            else
                p

    /// <summary>The hyperbolic origin <c>(0, 0)</c> at the center of the unit Poincaré disk.</summary>
    let origin : PoincarePoint = { PoincarePoint.X = 0.; Y = 0. }

    /// <summary>Validates that <c>(x, y)</c> is finite and lies strictly inside the open unit disk <c>x^2 + y^2 &lt; 1</c>.</summary>
    let ofDisk (x: float) (y: float) : Result<PoincarePoint, LayoutError list> =
        let r2 = x * x + y * y
        if not (Double.IsFinite x && Double.IsFinite y && Double.IsFinite r2) || r2 >= 1. then
            Error [ InvalidObservation "PoincarePoint coordinates must be finite and satisfy x^2 + y^2 < 1." ]
        else
            Ok { PoincarePoint.X = x; Y = y }

    /// <summary>Creates a unit-disk point at hyperbolic geodesic distance <c>hyperbolicDistance</c> from the origin and polar angle <c>angleRadians</c>.</summary>
    let ofPolar (hyperbolicDistance: float) (angleRadians: float) : PoincarePoint =
        let d =
            if Double.IsNaN hyperbolicDistance || hyperbolicDistance <= 0. then 0.
            elif hyperbolicDistance >= 32. then 32.
            else hyperbolicDistance
        let ang = if Double.IsFinite angleRadians then angleRadians else 0.
        let rho = min (1. - 1e-12) (tanh (0.5 * d))
        { PoincarePoint.X = rho * cos ang; Y = rho * sin ang }

    /// <summary>Returns the hyperbolic geodesic distance <c>2 * atanh(||p||)</c> from <c>origin</c> to <c>p</c>.</summary>
    let hyperbolicRadius (p: PoincarePoint) : float =
        let r = min (1. - 1e-12) (Double.Hypot(p.X, p.Y))
        2. * atanhSafe r

    /// <summary>Computes the exact symmetric, Möbius-invariant Poincaré hyperbolic distance between <c>a</c> and <c>b</c>.</summary>
    let distance (a: PoincarePoint) (b: PoincarePoint) : float =
        let dx = a.X - b.X
        let dy = a.Y - b.Y
        let num = dx * dx + dy * dy
        if num <= 0. then
            0.
        else
            let ra2 = min (1. - 1e-15) (a.X * a.X + a.Y * a.Y)
            let rb2 = min (1. - 1e-15) (b.X * b.X + b.Y * b.Y)
            let denom = max 1e-30 ((1. - ra2) * (1. - rb2))
            let halfDelta = num / denom
            // Exact identity acosh(1 + 2*halfDelta) = 2*asinh(sqrt(halfDelta)) avoids precision loss near a = b.
            2. * asinhNonneg (sqrt (max 0. halfDelta))

    /// <summary>Returns the Euclidean conformal metric factor <c>max 0 (1 - (X^2 + Y^2))</c> at <c>p</c>.</summary>
    let conformalScale (p: PoincarePoint) : float =
        max 0. (1. - (p.X * p.X + p.Y * p.Y))

    /// <summary>Applies the Lamping-Rao-Pirolli conformal disk automorphism <c>w = (z - focus) / (1 - conj(focus) * z)</c> mapping <c>focus</c> to <c>(0, 0)</c>, returning <c>(w, |dw/dz|)</c>.</summary>
    let mobiusTranslate (focus: PoincarePoint) (z: PoincarePoint) : PoincarePoint * float =
        let a = clampDiskPoint focus
        let p = clampDiskPoint z
        let dx = p.X - a.X
        let dy = p.Y - a.Y
        let reDen = 1. - (a.X * p.X + a.Y * p.Y)
        let imDen = -(a.X * p.Y - a.Y * p.X)
        let denSq = max 1e-30 (reDen * reDen + imDen * imDen)
        let wx = (dx * reDen + dy * imDen) / denSq
        let wy = (dy * reDen - dx * imDen) / denSq
        let maxR = 1. - 1e-12
        let rSq = wx * wx + wy * wy
        let w =
            if rSq > maxR * maxR then
                let s = maxR / sqrt rSq
                { PoincarePoint.X = wx * s; Y = wy * s }
            else
                { PoincarePoint.X = wx; Y = wy }
        let wRSq = w.X * w.X + w.Y * w.Y
        let zRSq = p.X * p.X + p.Y * p.Y
        let mag = max 0. (1. - wRSq) / max 1e-12 (1. - zRSq)
        w, mag

    /// <summary>Constructs the unique Poincaré geodesic circular arc (and sampled waypoint chain) from <c>p</c> to <c>q</c> orthogonal to the boundary circle <c>(diskCenter, diskRadius)</c>.</summary>
    let geodesicArc (diskCenter: Point) (diskRadius: float) (p: Point) (q: Point) : Path * Point list =
        let chordLen = Double.Hypot(q.X - p.X, q.Y - p.Y)
        if not (Double.IsFinite diskRadius) || diskRadius <= 1e-9 || chordLen <= 1e-7 then
            Path.ofCommands [ MoveTo p; LineTo q ], [ p; q ]
        else
            let linearFallback () =
                let wps =
                    [ for k in 0 .. 8 ->
                        if k = 0 then p
                        elif k = 8 then q
                        else
                            let t = float k / 8.
                            Point.create (p.X + (q.X - p.X) * t) (p.Y + (q.Y - p.Y) * t) ]
                Path.ofCommands [ MoveTo p; LineTo q ], wps

            let maxNorm = 0.999999 * diskRadius
            let inline clampInside x y =
                let d = Double.Hypot(x, y)
                if d > maxNorm && d > 0. then
                    let s = maxNorm / d
                    x * s, y * s
                else
                    x, y

            let ux, uy = clampInside (p.X - diskCenter.X) (p.Y - diskCenter.Y)
            let vx, vy = clampInside (q.X - diskCenter.X) (q.Y - diskCenter.Y)
            let nu2 = ux * ux + uy * uy
            let nv2 = vx * vx + vy * vy
            let rSq = diskRadius * diskRadius

            let ax, ay, bx, by, na2 =
                if nu2 >= nv2 then ux, uy, vx, vy, nu2
                else vx, vy, ux, uy, nv2

            if na2 <= 1e-12 * rSq then
                linearFallback ()
            else
                let invScale = rSq / na2
                let aStarX = ax * invScale
                let aStarY = ay * invScale
                let d1x = bx - ax
                let d1y = by - ay
                let d2x = aStarX - ax
                let d2y = aStarY - ay
                let det = 2. * (d1x * d2y - d1y * d2x)

                if abs det <= 1e-7 * rSq then
                    linearFallback ()
                else
                    let l1 = d1x * d1x + d1y * d1y
                    let l2 = d2x * d2x + d2y * d2y
                    let mx = ax + (l1 * d2y - l2 * d1y) / det
                    let my = ay + (d1x * l2 - d2x * l1) / det
                    let arcCx = diskCenter.X + mx
                    let arcCy = diskCenter.Y + my
                    let rp = Double.Hypot(p.X - arcCx, p.Y - arcCy)
                    let rq = Double.Hypot(q.X - arcCx, q.Y - arcCy)
                    let arcR = max (0.5 * chordLen) (0.5 * (rp + rq))

                    if not (Double.IsFinite arcCx && Double.IsFinite arcCy && Double.IsFinite arcR)
                       || arcR > 50. * diskRadius
                       || rp <= 1e-9
                       || rq <= 1e-9 then
                        linearFallback ()
                    else
                        let thetaP = atan2 (p.Y - arcCy) (p.X - arcCx)
                        let thetaQ = atan2 (q.Y - arcCy) (q.X - arcCx)
                        let mutable dTheta = thetaQ - thetaP
                        while dTheta <= -Math.PI do dTheta <- dTheta + 2. * Math.PI
                        while dTheta > Math.PI do dTheta <- dTheta - 2. * Math.PI

                        if abs dTheta <= 1e-6 || abs dTheta >= Math.PI - 1e-6 then
                            linearFallback ()
                        else
                            let sweepPositive = dTheta > 0.
                            let path = Path.ofCommands [ MoveTo p; ArcTo(arcR, arcR, 0., false, sweepPositive, q) ]
                            let wps =
                                [ for k in 0 .. 12 ->
                                    if k = 0 then p
                                    elif k = 12 then q
                                    else
                                        let t = float k / 12.
                                        let ang = thetaP + dTheta * t
                                        let rad = rp + (rq - rp) * t
                                        Point.create (arcCx + rad * cos ang) (arcCy + rad * sin ang) ]
                            path, wps

/// <summary>Shared iterative Tarjan SCC condensation + chunked 32-bit bitset transitive reach and DAG longest-path kernel.</summary>
module internal TransitiveReachKernel =
    type CondensationSummary =
        { IncomingReach: int[]
          OutgoingReach: int[]
          DistToSink: int[]
          DistFromSource: int[]
          SccOf: int[]
          SccCount: int }

    let inline private popCount32 (v0: uint32) : int =
#if FABLE_COMPILER
        let v1 = v0 - ((v0 >>> 1) &&& 0x55555555u)
        let v2 = (v1 &&& 0x33333333u) + ((v1 >>> 2) &&& 0x33333333u)
        int ((((v2 + (v2 >>> 4)) &&& 0x0F0F0F0Fu) * 0x01010101u) >>> 24)
#else
        System.Numerics.BitOperations.PopCount v0
#endif

    let analyze (csr: CsrGraph) : CondensationSummary =
        let n = csr.NodeCount
        if n = 0 then
            { IncomingReach = [||]
              OutgoingReach = [||]
              DistToSink = [||]
              DistFromSource = [||]
              SccOf = [||]
              SccCount = 0 }
        else
            // Iterative Tarjan (1972) strongly connected components.
            // Emits SCC indices 0 .. sccCount - 1 in exact reverse topological order (cSrc > cDst for every inter-SCC edge).
            let dfn = Array.create n -1
            let low = Array.zeroCreate<int> n
            let onStack = Array.zeroCreate<bool> n
            let tarjanStack = Array.zeroCreate<int> n
            let mutable tsp = 0
            let callNode = Array.zeroCreate<int> n
            let callEdge = Array.zeroCreate<int> n
            let mutable csp = 0
            let sccOf = Array.create n -1
            let sccMembers = ResizeArray<int[]>()
            let mutable timer = 0

            for start = 0 to n - 1 do
                if dfn[start] < 0 then
                    dfn[start] <- timer
                    low[start] <- timer
                    timer <- timer + 1
                    tarjanStack[tsp] <- start
                    tsp <- tsp + 1
                    onStack[start] <- true
                    callNode[0] <- start
                    callEdge[0] <- csr.OutOffsets[start]
                    csp <- 1

                    while csp > 0 do
                        let u = callNode[csp - 1]
                        let ei = callEdge[csp - 1]
                        if ei < csr.OutOffsets[u + 1] then
                            callEdge[csp - 1] <- ei + 1
                            let v = csr.Dst[csr.OutEdges[ei]]
                            if dfn[v] < 0 then
                                dfn[v] <- timer
                                low[v] <- timer
                                timer <- timer + 1
                                tarjanStack[tsp] <- v
                                tsp <- tsp + 1
                                onStack[v] <- true
                                callNode[csp] <- v
                                callEdge[csp] <- csr.OutOffsets[v]
                                csp <- csp + 1
                            elif onStack[v] then
                                if dfn[v] < low[u] then low[u] <- dfn[v]
                        else
                            csp <- csp - 1
                            if csp > 0 then
                                let p = callNode[csp - 1]
                                if low[u] < low[p] then low[p] <- low[u]
                            if low[u] = dfn[u] then
                                let cid = sccMembers.Count
                                let buf = ResizeArray<int>()
                                let mutable popping = true
                                while popping do
                                    tsp <- tsp - 1
                                    let w = tarjanStack[tsp]
                                    onStack[w] <- false
                                    sccOf[w] <- cid
                                    buf.Add w
                                    if w = u then popping <- false
                                sccMembers.Add(buf.ToArray())

            let sccCount = sccMembers.Count
            let outBuckets = Array.init sccCount (fun _ -> ResizeArray<int>())
            let inBuckets = Array.init sccCount (fun _ -> ResizeArray<int>())
            let seenPair = HashSet<uint64>()

            for e = 0 to csr.EdgeCount - 1 do
                let cu = sccOf[csr.Src[e]]
                let cv = sccOf[csr.Dst[e]]
                if cu <> cv then
                    let key = (uint64 cu <<< 32) ||| uint64 (uint32 cv)
                    if seenPair.Add key then
                        outBuckets[cu].Add cv
                        inBuckets[cv].Add cu

            let dagOut = Array.init sccCount (fun c -> outBuckets[c].ToArray())
            let dagIn = Array.init sccCount (fun c -> inBuckets[c].ToArray())

            // Longest DAG path to sinks (reverse topo order c = 0 .. sccCount - 1)
            let sccDistToSink = Array.zeroCreate<int> sccCount
            for c = 0 to sccCount - 1 do
                let outs = dagOut[c]
                let mutable best = 0
                for k = 0 to outs.Length - 1 do
                    let d = 1 + sccDistToSink[outs[k]]
                    if d > best then best <- d
                sccDistToSink[c] <- best

            // Longest DAG path from sources (forward topo order c = sccCount - 1 downto 0)
            let sccDistFromSource = Array.zeroCreate<int> sccCount
            for c = sccCount - 1 downto 0 do
                let ins = dagIn[c]
                let mutable best = 0
                for k = 0 to ins.Length - 1 do
                    let d = 1 + sccDistFromSource[ins[k]]
                    if d > best then best <- d
                sccDistFromSource[c] <- best

            let maxToSink = sccDistToSink |> Array.max
            let maxFromSource = sccDistFromSource |> Array.max

            // Place completely isolated singletons on the outer perimeter when the graph has multi-tier hierarchy.
            for c = 0 to sccCount - 1 do
                if dagOut[c].Length = 0 && dagIn[c].Length = 0 && sccMembers[c].Length = 1 then
                    if maxToSink > 0 then sccDistToSink[c] <- maxToSink
                    if maxFromSource > 0 then sccDistFromSource[c] <- maxFromSource

            // Chunked 64-word (2048-vertex) bitset OR propagation for exact incoming and outgoing transitive reach.
            let sccAncCount = Array.zeroCreate<int> sccCount
            let sccDescCount = Array.zeroCreate<int> sccCount
            let chunkVertices = 2048
            let mutable baseV = 0
            while baseV < n do
                let vSpan = min chunkVertices (n - baseV)
                let words = (vSpan + 31) >>> 5
                let bits = Array.zeroCreate<uint32> (sccCount * words)

                // Pass 1: Incoming reach (ancestors p ->* c, forward topo c = sccCount - 1 downto 0)
                for c = 0 to sccCount - 1 do
                    let mems = sccMembers[c]
                    let rowOff = c * words
                    for m = 0 to mems.Length - 1 do
                        let v = mems[m]
                        if v >= baseV && v < baseV + vSpan then
                            let rel = v - baseV
                            let wIdx = rowOff + (rel >>> 5)
                            bits[wIdx] <- bits[wIdx] ||| (1u <<< (rel &&& 31))

                for c = sccCount - 1 downto 0 do
                    let ins = dagIn[c]
                    let cOff = c * words
                    for k = 0 to ins.Length - 1 do
                        let pOff = ins[k] * words
                        for w = 0 to words - 1 do
                            bits[cOff + w] <- bits[cOff + w] ||| bits[pOff + w]
                    let mutable cnt = 0
                    for w = 0 to words - 1 do
                        cnt <- cnt + popCount32 (bits[cOff + w])
                    sccAncCount[c] <- sccAncCount[c] + cnt

                // Pass 2: Outgoing reach (descendants c ->* s, reverse topo c = 0 .. sccCount - 1)
                Array.fill bits 0 bits.Length 0u
                for c = 0 to sccCount - 1 do
                    let mems = sccMembers[c]
                    let cOff = c * words
                    for m = 0 to mems.Length - 1 do
                        let v = mems[m]
                        if v >= baseV && v < baseV + vSpan then
                            let rel = v - baseV
                            let wIdx = cOff + (rel >>> 5)
                            bits[wIdx] <- bits[wIdx] ||| (1u <<< (rel &&& 31))
                    let outs = dagOut[c]
                    for k = 0 to outs.Length - 1 do
                        let sOff = outs[k] * words
                        for w = 0 to words - 1 do
                            bits[cOff + w] <- bits[cOff + w] ||| bits[sOff + w]
                    let mutable cnt = 0
                    for w = 0 to words - 1 do
                        cnt <- cnt + popCount32 (bits[cOff + w])
                    sccDescCount[c] <- sccDescCount[c] + cnt

                baseV <- baseV + chunkVertices

            let incomingReach = Array.init n (fun v -> max 0 (sccAncCount[sccOf[v]] - 1))
            let outgoingReach = Array.init n (fun v -> max 0 (sccDescCount[sccOf[v]] - 1))
            let distToSink = Array.init n (fun v -> sccDistToSink[sccOf[v]])
            let distFromSource = Array.init n (fun v -> sccDistFromSource[sccOf[v]])

            { IncomingReach = incomingReach
              OutgoingReach = outgoingReach
              DistToSink = distToSink
              DistFromSource = distFromSource
              SccOf = sccOf
              SccCount = sccCount }

/// <summary>Radial ring stratification rule for 2D Poincaré disk layouts.</summary>
[<RequireQualifiedAccess>]
type PoincareRingLayering =
    /// <summary>Places nodes by longest directed path distance over the Tarjan SCC condensation DAG.</summary>
    | LongestPath
    /// <summary>Places nodes into logarithmic transitive-reach tiers <c>floor(log2(1 + reach))</c> inverted toward the center.</summary>
    | LogTransitiveReach

/// <summary>Which topological pole occupies the innermost ring of the Poincaré disk.</summary>
[<RequireQualifiedAccess>]
type PoincareCenterBias =
    /// <summary>Places depended-upon sink foundations (<c>out-degree = 0</c> in <c>caller -&gt; callee</c>, highest incoming transitive reach) at ring 0.</summary>
    | FoundationsInner
    /// <summary>Places source / root nodes (<c>in-degree = 0</c> in <c>parent -&gt; child</c>, highest outgoing reach) at ring 0.</summary>
    | RootsInner

/// <summary>Configuration for concentric 2D Poincaré disk hyperbolic layout and interactive Möbius focus.</summary>
type PoincareDiskConfig =
    { /// <summary>Target viewport into which the Poincaré disk is inscribed.</summary>
      Viewport: Bounds
      /// <summary>Concentric ring assignment rule (<c>LongestPath</c> or <c>LogTransitiveReach</c>).</summary>
      RingLayering: PoincareRingLayering
      /// <summary>Whether depended-upon foundations (<c>FoundationsInner</c>) or root sources (<c>RootsInner</c>) sit at ring 0.</summary>
      CenterBias: PoincareCenterBias
      /// <summary>Nonnegative exponent controlling peripheral conformal size foreshortening <c>(1 - r^2)^ConformalExponent</c>.</summary>
      ConformalExponent: float
      /// <summary>Number of alternating inner/outer circular barycenter crossing-reduction sweeps.</summary>
      BarycenterSweeps: int
      /// <summary>Lower bound in <c>(0, 1]</c> on peripheral node conformal scale factor.</summary>
      MinNodeScale: float
      /// <summary>Nonnegative inset margin between the disk boundary circle and <c>Viewport</c>.</summary>
      Padding: float }

/// <summary>Constructors for <c>PoincareDiskConfig</c>.</summary>
[<RequireQualifiedAccess>]
module PoincareDiskConfig =
    /// <summary>Creates default Poincaré disk settings: <c>LongestPath</c> rings, <c>FoundationsInner</c>, conformal exponent 0.65, 8 sweeps, min scale 0.22 and 16-unit padding.</summary>
    let create (viewport: Bounds) : PoincareDiskConfig =
        { Viewport = viewport
          RingLayering = PoincareRingLayering.LongestPath
          CenterBias = PoincareCenterBias.FoundationsInner
          ConformalExponent = 0.65
          BarycenterSweeps = 8
          MinNodeScale = 0.22
          Padding = 16. }

/// <summary>Unit-disk hyperbolic coordinate, ring metadata and unscaled dimensions of a node in a <c>PoincareDiskState</c>.</summary>
type PoincareNodeCoord =
    { /// <summary>Node identifier.</summary>
      Id: string
      /// <summary>Current open unit-disk coordinate under the active Möbius focus.</summary>
      DiskPosition: PoincarePoint
      /// <summary>Zero-based concentric ring index (<c>0</c> = innermost).</summary>
      Ring: int
      /// <summary>Polar angle <c>atan2(Y, X)</c> in radians around the disk center.</summary>
      Angle: float
      /// <summary>Unscaled node width in layout units.</summary>
      BaseWidth: float
      /// <summary>Unscaled node height in layout units.</summary>
      BaseHeight: float
      /// <summary>Exact transitive reach count under the configured <c>CenterBias</c>.</summary>
      Reach: int }

/// <summary>Persistent, immutable 2D Poincaré disk layout state supporting <c>O(V + E)</c> drift-free Möbius focus translations.</summary>
type PoincareDiskState =
    internal
        { Config: PoincareDiskConfig
          Graph: DiagramGraph
          Eff: EffectiveDiagram
          DiskCenter: Point
          DiskRadius: float
          CanonicalDisk: PoincarePoint[]
          CurrentDisk: PoincarePoint[]
          Rings: int[]
          CanonicalAngles: float[]
          CurrentAngles: float[]
          Reach: int[]
          RingRadii: float[]
          Focus: PoincarePoint }

/// <summary>2D Poincaré disk concentric hyperbolic layout engine with Lamping-Rao-Pirolli Möbius Focus+Context navigation.</summary>
[<RequireQualifiedAccess>]
module PoincareDisk =
    let private validateConfig (config: PoincareDiskConfig) : Result<unit, LayoutError list> =
        let vx = Bounds.x config.Viewport
        let vy = Bounds.y config.Viewport
        let vw = Bounds.width config.Viewport
        let vh = Bounds.height config.Viewport
        if not (Double.IsFinite vx && Double.IsFinite vy && Double.IsFinite vw && Double.IsFinite vh)
           || vw <= 0. || vh <= 0. then
            Error [ InvalidObservation "PoincareDisk Viewport must have finite coordinates and positive width and height." ]
        elif not (Double.IsFinite config.ConformalExponent) || config.ConformalExponent < 0. then
            Error [ InvalidObservation "PoincareDisk ConformalExponent must be finite and nonnegative." ]
        elif not (Double.IsFinite config.Padding) || config.Padding < 0. then
            Error [ InvalidObservation "PoincareDisk Padding must be finite and nonnegative." ]
        elif not (Double.IsFinite config.MinNodeScale) || config.MinNodeScale <= 0. || config.MinNodeScale > 1. then
            Error [ InvalidObservation "PoincareDisk MinNodeScale must be finite and in (0.0, 1.0]." ]
        elif config.BarycenterSweeps < 0 || config.BarycenterSweeps > 500 then
            Error [ InvalidObservation "PoincareDisk BarycenterSweeps must be in [0, 500]." ]
        else
            Ok()

    let private buildGroupSectorKey (eff: EffectiveDiagram) : string[] =
        let n = eff.Nodes.Length
        if eff.Groups.Length = 0 then
            Array.init n (fun i -> eff.Nodes[i].Id)
        else
#if FABLE_COMPILER
            let gParent = Dictionary<string, string option>(StringComparer.Ordinal)
#else
            let gParent = Dictionary<string, string option>(eff.Groups.Length, StringComparer.Ordinal)
#endif
            for i = 0 to eff.Groups.Length - 1 do
                let g = eff.Groups[i]
                gParent[g.Id] <- g.ParentGroup
            let rec chainOf (gid: string) (acc: string) =
                match gParent.TryGetValue gid with
                | true, Some pid -> chainOf pid (pid + "/" + acc)
                | _ -> acc
            Array.init n (fun i ->
                let nd = eff.Nodes[i]
                match nd.ParentGroup with
                | Some gid -> "0/" + chainOf gid gid + "/" + nd.Id
                | None -> "1/" + nd.Id)

    let private wrapAnglePi (a: float) : float =
        let mutable x = a
        while x <= -Math.PI do x <- x + 2. * Math.PI
        while x > Math.PI do x <- x - 2. * Math.PI
        x

    let private placeProportionalAngles
        (ringNodes: int[])
        (targetAngles: float[])
        (nodeSpans: float[])
        (outAngles: float[])
        : unit =
        let m = ringNodes.Length
        if m = 1 then
            outAngles[ringNodes[0]] <- wrapAnglePi (targetAngles[ringNodes[0]])
        elif m > 1 then
            let order = Array.copy ringNodes
            Array.sortInPlaceWith (fun a b ->
                let c = compare targetAngles[a] targetAngles[b]
                if c <> 0 then c else compare a b) order

            let mutable totalSpan = 0.
            for k = 0 to m - 1 do
                totalSpan <- totalSpan + nodeSpans[order[k]]
            let invSpan = if totalSpan > 1e-9 then (2. * Math.PI) / totalSpan else (2. * Math.PI) / float m

            let relAngle = Array.zeroCreate<float> m
            let mutable cursor = 0.
            let mutable sinDiff = 0.
            let mutable cosDiff = 0.
            for k = 0 to m - 1 do
                let u = order[k]
                let share = if totalSpan > 1e-9 then nodeSpans[u] * invSpan else invSpan
                let mid = cursor + 0.5 * share
                relAngle[k] <- mid
                cursor <- cursor + share
                let delta = targetAngles[u] - mid
                sinDiff <- sinDiff + sin delta
                cosDiff <- cosDiff + cos delta

            let rotOffset = if abs sinDiff > 1e-12 || abs cosDiff > 1e-12 then atan2 sinDiff cosDiff else 0.
            for k = 0 to m - 1 do
                let u = order[k]
                outAngles[u] <- wrapAnglePi (relAngle[k] + rotOffset)

    /// <summary>Builds a persistent <c>PoincareDiskState</c> from <c>graph</c> with cooperative cancellation.</summary>
    let buildStateWithCancellation
        (token: System.Threading.CancellationToken)
        (config: PoincareDiskConfig)
        (graph: DiagramGraph)
        : Result<PoincareDiskState, LayoutError list> =
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
            let diskCenter = Point.create (vx + vw * 0.5) (vy + vh * 0.5)
            let diskRadius = max 8. (0.5 * min vw vh - config.Padding)

            if n = 0 then
                Ok
                    { Config = config
                      Graph = graph
                      Eff = eff
                      DiskCenter = diskCenter
                      DiskRadius = diskRadius
                      CanonicalDisk = [||]
                      CurrentDisk = [||]
                      Rings = [||]
                      CanonicalAngles = [||]
                      CurrentAngles = [||]
                      Reach = [||]
                      RingRadii = [||]
                      Focus = Hyperbolic2D.origin }
            elif n = 1 then
                Ok
                    { Config = config
                      Graph = graph
                      Eff = eff
                      DiskCenter = diskCenter
                      DiskRadius = diskRadius
                      CanonicalDisk = [| Hyperbolic2D.origin |]
                      CurrentDisk = [| Hyperbolic2D.origin |]
                      Rings = [| 0 |]
                      CanonicalAngles = [| 0. |]
                      CurrentAngles = [| 0. |]
                      Reach = [| 0 |]
                      RingRadii = [| 0. |]
                      Focus = Hyperbolic2D.origin }
            else
                let csr = GraphKernels.buildCsr eff
                let cond = TransitiveReachKernel.analyze csr

                let reachArr =
                    match config.CenterBias with
                    | PoincareCenterBias.FoundationsInner -> cond.IncomingReach
                    | PoincareCenterBias.RootsInner -> cond.OutgoingReach

                let rawRing =
                    match config.RingLayering with
                    | PoincareRingLayering.LongestPath ->
                        match config.CenterBias with
                        | PoincareCenterBias.FoundationsInner -> Array.copy cond.DistToSink
                        | PoincareCenterBias.RootsInner -> Array.copy cond.DistFromSource
                    | PoincareRingLayering.LogTransitiveReach ->
                        let invLog2 = 1. / log 2.
                        let tiers = Array.init n (fun i -> int (floor (log (1. + float reachArr[i]) * invLog2 + 1e-12)))
                        let maxT = tiers |> Array.max
                        Array.init n (fun i -> maxT - tiers[i])

                // Compress occupied rings into dense 0 .. numRings - 1
                let distinctRings = rawRing |> Array.distinct
                Array.sortInPlaceWith compare distinctRings
                let numRings = distinctRings.Length
                let ringRemap = Dictionary<int, int>()
                for r = 0 to numRings - 1 do ringRemap[distinctRings[r]] <- r
                let rings = Array.init n (fun i -> ringRemap[rawRing[i]])

                let ringBuckets = Array.init numRings (fun _ -> ResizeArray<int>())
                for i = 0 to n - 1 do ringBuckets[rings[i]].Add i
                let ringMembers = Array.init numRings (fun r -> ringBuckets[r].ToArray())

                let diagArr =
                    Array.init n (fun i ->
                        let s = eff.Nodes[i].Size
                        Double.Hypot(s.Width, s.Height))
                let baseSpan = Array.init n (fun i -> diagArr[i] + 4.)

                // Hierarchical ParentGroup sector initialization
                let sectorKeys = buildGroupSectorKey eff
                let globalSectorOrder = Array.init n id
                Array.sortInPlaceWith (fun a b -> String.CompareOrdinal(sectorKeys[a], sectorKeys[b])) globalSectorOrder
                let initAngle = Array.zeroCreate<float> n
                for pos = 0 to n - 1 do
                    let u = globalSectorOrder[pos]
                    initAngle[u] <- wrapAnglePi (-Math.PI + 2. * Math.PI * ((float pos + 0.5) / float n))

                let angles = Array.copy initAngle
                let targetAngles = Array.copy initAngle
                let singleCenter = (ringMembers[0].Length = 1)
                if singleCenter then
                    angles[ringMembers[0][0]] <- 0.

                for r = 0 to numRings - 1 do
                    if not (r = 0 && singleCenter) then
                        placeProportionalAngles (ringMembers[r]) initAngle baseSpan angles

                // Alternating inner-to-outer and outer-to-inner circular barycenter sweeps
                for sweep = 0 to config.BarycenterSweeps - 1 do
                    token.ThrowIfCancellationRequested()
                    let outward = (sweep % 2 = 0)
                    let startR = if outward then (if singleCenter then 1 else 0) else numRings - 1
                    let stopR = if outward then numRings - 1 else (if singleCenter then 1 else 0)
                    let stepR = if outward then 1 else -1
                    let mutable r = startR
                    while (if stepR > 0 then r <= stopR else r >= stopR) do
                        let mems = ringMembers[r]
                        if mems.Length >= 2 then
                            for k = 0 to mems.Length - 1 do
                                let u = mems[k]
                                let mutable sumSin = 0.08 * sin initAngle[u] + 0.04 * sin angles[u]
                                let mutable sumCos = 0.08 * cos initAngle[u] + 0.04 * cos angles[u]
                                for idx = csr.UndirOffsets[u] to csr.UndirOffsets[u + 1] - 1 do
                                    let v = csr.UndirTargets[idx]
                                    let rv = rings[v]
                                    if rv <> r && not (rv = 0 && singleCenter) then
                                        let dirWeight =
                                            if (outward && rv < r) || (not outward && rv > r) then 1.5 else 0.75
                                        let w = csr.UndirWeights[idx] * dirWeight
                                        sumSin <- sumSin + w * sin angles[v]
                                        sumCos <- sumCos + w * cos angles[v]
                                targetAngles[u] <- atan2 sumSin sumCos
                            placeProportionalAngles mems targetAngles baseSpan angles
                        r <- r + stepR

                if singleCenter then
                    angles[ringMembers[0][0]] <- 0.

                // Binary-search hyperbolic ring spacing so every ring fits inside <= 0.90 * diskRadius without overlap when feasible
                let dMaxCap = 2. * Hyperbolic2D.atanhSafe 0.90
                let inline ringNeed (r: int) (dHyp: float) : float * float =
                    let rho = min 0.90 (tanh (0.5 * dHyp))
                    let euclidR = diskRadius * rho
                    let s = max config.MinNodeScale (Math.Pow(max 0. (1. - rho * rho), config.ConformalExponent))
                    let mems = ringMembers[r]
                    if mems.Length <= 1 then
                        euclidR, 0.
                    else
                        let mutable need = 0.
                        for k = 0 to mems.Length - 1 do
                            need <- need + diagArr[mems[k]] * s + 4.
                        euclidR, need

                let d0 =
                    if singleCenter then
                        0.
                    else
                        let hi0 = if numRings = 1 then dMaxCap * 0.72 else min 1.15 (dMaxCap * 0.42)
                        let lo0 = 0.22
                        let mutable a = lo0
                        let mutable b = max lo0 hi0
                        for _it = 0 to 27 do
                            let mid = 0.5 * (a + b)
                            let euclidR, need = ringNeed 0 mid
                            if 2. * Math.PI * euclidR >= need then b <- mid else a <- mid
                        let comfortable0 = if numRings = 1 then max b (min hi0 0.95) else max b (min hi0 0.42)
                        comfortable0

                let stepD =
                    if numRings <= 1 then
                        0.
                    else
                        let maxStep = max 0.18 (min 3.2 ((dMaxCap - d0) / float (numRings - 1)))
                        let minStep = min 0.18 maxStep
                        let feasible (st: float) =
                            let mutable ok = true
                            let mutable prevR = if singleCenter then 0. else diskRadius * tanh (0.5 * d0)
                            let mutable r = 1
                            while ok && r < numRings do
                                let d = min dMaxCap (d0 + float r * st)
                                let euclidR, need = ringNeed r d
                                if 2. * Math.PI * euclidR < need || euclidR - prevR < 6. then
                                    ok <- false
                                prevR <- euclidR
                                r <- r + 1
                            ok

                        let mutable lo = minStep
                        let mutable hi = maxStep
                        if not (feasible hi) then
                            hi
                        else
                            for _it = 0 to 31 do
                                let mid = 0.5 * (lo + hi)
                                if feasible mid then hi <- mid else lo <- mid
                            let targetRho = min 0.86 (0.36 + 0.15 * float (numRings - 1))
                            let targetD = 2. * Hyperbolic2D.atanhSafe targetRho
                            let aestheticFloor = min maxStep (max 0.48 ((targetD - d0) / float (numRings - 1)))
                            max hi aestheticFloor

                let ringHypD =
                    Array.init numRings (fun r ->
                        if r = 0 && singleCenter then 0.
                        else min dMaxCap (d0 + float r * stepD))

                let ringRadii = Array.init numRings (fun r -> diskRadius * min 0.90 (tanh (0.5 * ringHypD[r])))

                let canonicalDisk =
                    Array.init n (fun i ->
                        let r = rings[i]
                        if r = 0 && singleCenter then
                            Hyperbolic2D.origin
                        else
                            Hyperbolic2D.ofPolar ringHypD[r] angles[i])

                Ok
                    { Config = config
                      Graph = graph
                      Eff = eff
                      DiskCenter = diskCenter
                      DiskRadius = diskRadius
                      CanonicalDisk = canonicalDisk
                      CurrentDisk = Array.copy canonicalDisk
                      Rings = rings
                      CanonicalAngles = angles
                      CurrentAngles = Array.copy angles
                      Reach = reachArr
                      RingRadii = ringRadii
                      Focus = Hyperbolic2D.origin }

    /// <summary>Builds a persistent <c>PoincareDiskState</c> from <c>graph</c> without cancellation.</summary>
    let buildState (config: PoincareDiskConfig) (graph: DiagramGraph) : Result<PoincareDiskState, LayoutError list> =
        buildStateWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Applies a drift-free Lamping-Rao-Pirolli Möbius translation from canonical disk coordinates centering <c>focus</c> at <c>(0, 0)</c>.</summary>
    let focusPoint (focus: PoincarePoint) (state: PoincareDiskState) : PoincareDiskState =
        let safeFocus = Hyperbolic2D.clampDiskPoint focus
        let n = state.CanonicalDisk.Length
        if n = 0 then
            { state with Focus = safeFocus }
        else
            let curDisk = Array.zeroCreate<PoincarePoint> n
            let curAngles = Array.zeroCreate<float> n
            for i = 0 to n - 1 do
                let w, _ = Hyperbolic2D.mobiusTranslate safeFocus (state.CanonicalDisk[i])
                curDisk[i] <- w
                curAngles[i] <- if abs w.X <= 1e-15 && abs w.Y <= 1e-15 then 0. else atan2 w.Y w.X
            { state with
                CurrentDisk = curDisk
                CurrentAngles = curAngles
                Focus = safeFocus }

    /// <summary>Centers <c>nodeId</c> at the Poincaré disk origin via an exact Möbius translation from canonical coordinates.</summary>
    let focusNode (nodeId: string) (state: PoincareDiskState) : Result<PoincareDiskState, LayoutError list> =
        match state.Eff.ReversedIdMap.TryGetValue nodeId with
        | true, idx -> Ok(focusPoint (state.CanonicalDisk[idx]) state)
        | _ -> Error [ MissingEndpoint nodeId ]

    /// <summary>Restores the unshifted canonical Poincaré disk coordinates (<c>Focus = Hyperbolic2D.origin</c>).</summary>
    let resetFocus (state: PoincareDiskState) : PoincareDiskState =
        { state with
            CurrentDisk = Array.copy state.CanonicalDisk
            CurrentAngles = Array.copy state.CanonicalAngles
            Focus = Hyperbolic2D.origin }

    /// <summary>Returns the positioned unit-disk coordinates and ring metadata for all visible nodes in ordinal ID order.</summary>
    let nodes (state: PoincareDiskState) : PoincareNodeCoord list =
        let n = state.Eff.Nodes.Length
        [ for i in 0 .. n - 1 ->
            let nd = state.Eff.Nodes[i]
            { Id = nd.Id
              DiskPosition = state.CurrentDisk[i]
              Ring = state.Rings[i]
              Angle = state.CurrentAngles[i]
              BaseWidth = nd.Size.Width
              BaseHeight = nd.Size.Height
              Reach = state.Reach[i] } ]

    /// <summary>Returns the Euclidean center of the Poincaré disk in viewport coordinates.</summary>
    let diskCenter (state: PoincareDiskState) : Point = state.DiskCenter

    /// <summary>Returns the Euclidean boundary radius of the Poincaré disk in layout units.</summary>
    let diskRadius (state: PoincareDiskState) : float = state.DiskRadius

    /// <summary>Returns the canonical concentric Euclidean ring radii from innermost ring 0 outward.</summary>
    let ringRadii (state: PoincareDiskState) : float list = Array.toList state.RingRadii

    /// <summary>Returns the source <c>DiagramGraph</c> associated with <c>state</c>.</summary>
    let graph (state: PoincareDiskState) : DiagramGraph = state.Graph

    /// <summary>Projects <c>state</c> under its active Möbius focus into a Euclidean <c>DiagramLayout2D</c> with conformal node scaling and orthogonal geodesic arcs.</summary>
    let project (state: PoincareDiskState) : DiagramLayout2D =
        let eff = state.Eff
        let n = eff.Nodes.Length
        if n = 0 then
            { Bounds = state.Config.Viewport
              Nodes = []
              Groups = GraphKernels.computeGroupBounds eff [||]
              Associations = [] }
        else
            let cx0 = state.DiskCenter.X
            let cy0 = state.DiskCenter.Y
            let diskR = state.DiskRadius
            let minScale = state.Config.MinNodeScale
            let expAlpha = state.Config.ConformalExponent

            let posNodes =
                Array.init n (fun i ->
                    let nd = eff.Nodes[i]
                    let w = state.CurrentDisk[i]
                    let r2 = min (1. - 1e-12) (w.X * w.X + w.Y * w.Y)
                    let scale = max minScale (Math.Pow(max 0. (1. - r2), expAlpha))
                    let cx = cx0 + diskR * w.X
                    let cy = cy0 + diskR * w.Y
                    let sw = max 2. (nd.Size.Width * scale)
                    let sh = max 2. (nd.Size.Height * scale)
                    let b = Bounds.create (cx - sw * 0.5) (cy - sh * 0.5) sw sh
                    let c = Point.create cx cy
                    { Id = nd.Id
                      Kind = nd.Kind
                      Bounds = b
                      Center = c
                      ParentGroup = nd.ParentGroup
                      Layer = Some state.Rings[i]
                      Ports = GraphKernels.resolveNodePorts nd b Map.empty })

            let groups = GraphKernels.computeGroupBounds eff (posNodes |> Array.map (fun pn -> pn.Bounds))

            let lookupPort (node: PositionedNode) (portId: string option) (toward: Point) =
                match portId with
                | Some pid ->
                    node.Ports
                    |> List.tryFind (fun p -> p.Id = pid)
                    |> Option.map (fun p -> p.Anchor)
                    |> Option.defaultValue (GraphKernels.clipRectBoundary node.Bounds toward)
                | None -> GraphKernels.clipRectBoundary node.Bounds toward

            let m = eff.Associations.Length
            let outAssocs = Array.zeroCreate<RoutedAssociation> m
            for e = 0 to m - 1 do
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
                    let pStart = lookupPort uNode a.Source.PortId vNode.Center
                    let pEnd = lookupPort vNode a.Target.PortId uNode.Center
                    let path, wps = Hyperbolic2D.geodesicArc state.DiskCenter diskR pStart pEnd
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

            let nList = Array.toList posNodes
            let routed = Array.toList outAssocs
            { Bounds = GraphKernels.computeDiagramBounds state.Config.Viewport nList groups routed
              Nodes = nList
              Groups = groups
              Associations = routed }

    /// <summary>Computes a 2D Poincaré disk hyperbolic layout with cooperative cancellation.</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: PoincareDiskConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        buildStateWithCancellation token config graph |> Result.map project

    /// <summary>Computes a 2D Poincaré disk hyperbolic layout without cancellation.</summary>
    let layout (config: PoincareDiskConfig) (graph: DiagramGraph) : Result<DiagramLayout2D, LayoutError list> =
        layoutWithCancellation System.Threading.CancellationToken.None config graph

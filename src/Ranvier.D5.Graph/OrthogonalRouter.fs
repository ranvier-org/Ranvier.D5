namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Configuration for obstacle-avoiding orthogonal port and association routing.</summary>
type OrthogonalRouterConfig =
    { /// <summary>Clearance margin around node obstacles in layout units.</summary>
      ObstacleMargin: float
      /// <summary>Perpendicular stub distance extending outward from each port before turning.</summary>
      PortStubLength: float
      /// <summary>Additional path-cost penalty per 90-degree bend in A* search.</summary>
      BendPenalty: float
      /// <summary>Parallel track spacing between overlapping collinear association segments.</summary>
      ChannelSpacing: float
      /// <summary>Optional fillet corner radius at 90-degree bends (0 produces sharp right angles).</summary>
      CornerRadius: float }

/// <summary>Constructors for <c>OrthogonalRouterConfig</c>.</summary>
[<RequireQualifiedAccess>]
module OrthogonalRouterConfig =
    /// <summary>Default router settings: 10-unit obstacle margin, 14-unit port stubs, 28-unit bend penalty, 8-unit channel spacing and 4-unit rounded corners.</summary>
    let defaultValue =
        { ObstacleMargin = 10.
          PortStubLength = 14.
          BendPenalty = 28.
          ChannelSpacing = 8.
          CornerRadius = 4. }

/// <summary>Obstacle-avoiding orthogonal port-to-port association router (Wybrow, Marriott &amp; Stuckey 2009) with parallel channel nudging.</summary>
[<RequireQualifiedAccess>]
module OrthogonalRouter =
    let private sideNormal = function
        | PortSide.North -> 0., -1.
        | PortSide.South -> 0., 1.
        | PortSide.West -> -1., 0.
        | PortSide.East
        | PortSide.Auto -> 1., 0.

    let private chooseAutoSide (fromCenter: Point) (toCenter: Point) =
        let dx = toCenter.X - fromCenter.X
        let dy = toCenter.Y - fromCenter.Y
        if abs dx >= abs dy then
            if dx >= 0. then PortSide.East else PortSide.West
        else
            if dy >= 0. then PortSide.South else PortSide.North

    let private simplifyOrthogonal (pts: ResizeArray<Point>) : Point[] =
        if pts.Count <= 2 then pts.ToArray()
        else
            let out = ResizeArray<Point>(pts.Count)
            for i = 0 to pts.Count - 1 do
                let p = pts[i]
                if out.Count = 0 then out.Add p
                else
                    let prev = out[out.Count - 1]
                    if abs (p.X - prev.X) > 1e-6 || abs (p.Y - prev.Y) > 1e-6 then
                        if out.Count >= 2 then
                            let prev2 = out[out.Count - 2]
                            let collinearX = abs (prev2.X - prev.X) <= 1e-6 && abs (prev.X - p.X) <= 1e-6
                            let collinearY = abs (prev2.Y - prev.Y) <= 1e-6 && abs (prev.Y - p.Y) <= 1e-6
                            if collinearX || collinearY then
                                out[out.Count - 1] <- p
                            else
                                out.Add p
                        else
                            out.Add p
            out.ToArray()

    let private buildPathWithCorners (cornerRadius: float) (pts: Point[]) : Path =
        if pts.Length = 0 then Path.empty
        elif pts.Length = 1 then Path.ofCommands [ MoveTo pts[0]; LineTo pts[0] ]
        elif cornerRadius <= 1e-6 || pts.Length = 2 then
            let cmds = ResizeArray<PathCommand>(pts.Length)
            cmds.Add(MoveTo pts[0])
            for i = 1 to pts.Length - 1 do cmds.Add(LineTo pts[i])
            Path.ofCommands (List.ofSeq cmds)
        else
            let cmds = ResizeArray<PathCommand>(pts.Length * 2)
            cmds.Add(MoveTo pts[0])
            for i = 1 to pts.Length - 2 do
                let a, b, c = pts[i - 1], pts[i], pts[i + 1]
                let d1 = Double.Hypot(b.X - a.X, b.Y - a.Y)
                let d2 = Double.Hypot(c.X - b.X, c.Y - b.Y)
                let r = min cornerRadius (0.45 * min d1 d2)
                if r <= 0.25 || d1 <= 1e-6 || d2 <= 1e-6 then
                    cmds.Add(LineTo b)
                else
                    let pre = Point.create (b.X - (b.X - a.X) * (r / d1)) (b.Y - (b.Y - a.Y) * (r / d1))
                    let post = Point.create (b.X + (c.X - b.X) * (r / d2)) (b.Y + (c.Y - b.Y) * (r / d2))
                    cmds.Add(LineTo pre)
                    cmds.Add(QuadraticTo(b, post))
            cmds.Add(LineTo pts[pts.Length - 1])
            Path.ofCommands (List.ofSeq cmds)

    let private segmentHitsRect (ax: float) (ay: float) (bx: float) (by: float) (rx0: float) (ry0: float) (rx1: float) (ry1: float) =
        // Strict interior intersection of an axis-aligned segment with open rectangle (rx0, ry0)..(rx1, ry1)
        let eps = 1e-5
        if abs (ay - by) <= 1e-7 then
            // Horizontal segment at y = ay
            if ay > ry0 + eps && ay < ry1 - eps then
                let s0, s1 = min ax bx, max ax bx
                max s0 (rx0 + eps) < min s1 (rx1 - eps)
            else false
        elif abs (ax - bx) <= 1e-7 then
            // Vertical segment at x = ax
            if ax > rx0 + eps && ax < rx1 - eps then
                let s0, s1 = min ay by, max ay by
                max s0 (ry0 + eps) < min s1 (ry1 - eps)
            else false
        else false

    let private sortDedup (vals: ResizeArray<float>) : float[] =
        let arr = vals.ToArray()
        Array.Sort(arr)
        let out = ResizeArray<float>(arr.Length)
        for i = 0 to arr.Length - 1 do
            if out.Count = 0 || abs (arr[i] - out[out.Count - 1]) > 1e-4 then
                out.Add arr[i]
        out.ToArray()

    let private findCoordIndex (coords: float[]) (v: float) : int =
        let mutable best = 0
        let mutable bestDiff = abs (coords[0] - v)
        for i = 1 to coords.Length - 1 do
            let d = abs (coords[i] - v)
            if d < bestDiff then
                bestDiff <- d
                best <- i
        best

    let private dxStep = [| 1; 0; -1; 0 |]
    let private dyStep = [| 0; 1; 0; -1 |]

    type private AStarWorkspace =
        { mutable GScore: float[]
          mutable Parent: int[]
          mutable Stamp: int[]
          mutable CurStamp: int
          RawXs: ResizeArray<float>
          RawYs: ResizeArray<float>
          RevPts: ResizeArray<Point>
#if FABLE_COMPILER
          Pq: PriorityQueue<int, float>
#else
          Pq: System.Collections.Generic.PriorityQueue<int, float>
#endif
        }

    let private createWorkspace () =
        let initCap = 4096
        { GScore = Array.zeroCreate<float> initCap
          Parent = Array.zeroCreate<int> initCap
          Stamp = Array.zeroCreate<int> initCap
          CurStamp = 0
          RawXs = ResizeArray<float>(112)
          RawYs = ResizeArray<float>(112)
          RevPts = ResizeArray<Point>(32)
#if FABLE_COMPILER
          Pq = PriorityQueue<int, float>()
#else
          Pq = System.Collections.Generic.PriorityQueue<int, float>(256)
#endif
        }

    /// <summary>Routes between two stub points avoiding inflated obstacles using local Hanan-grid A* search with pooled stamp-cleared state arrays.</summary>
    let private aStarOrthogonal
        (ws: AStarWorkspace)
        (stubA: Point)
        (dirA: int)
        (stubB: Point)
        (bendPenalty: float)
        (obstacles: ResizeArray<struct(float * float * float * float)>)
        : Point[] =
        let obsCount = obstacles.Count
        // Fast path when corridor has zero obstacles or direct 1-bend / 2-bend orthogonal link is completely clear:
        let clearSeg x0 y0 x1 y1 =
            let mutable ok = true
            let mutable k = 0
            while ok && k < obsCount do
                let struct(rx0, ry0, rx1, ry1) = obstacles[k]
                if segmentHitsRect x0 y0 x1 y1 rx0 ry0 rx1 ry1 then ok <- false
                k <- k + 1
            ok

        let midX = (stubA.X + stubB.X) * 0.5
        let midY = (stubA.Y + stubB.Y) * 0.5
        if abs (stubA.X - stubB.X) <= 1e-5 && clearSeg stubA.X stubA.Y stubB.X stubB.Y then
            [| stubA; stubB |]
        elif abs (stubA.Y - stubB.Y) <= 1e-5 && clearSeg stubA.X stubA.Y stubB.X stubB.Y then
            [| stubA; stubB |]
        elif (dirA = 0 || dirA = 2) && clearSeg stubA.X stubA.Y midX stubA.Y && clearSeg midX stubA.Y midX stubB.Y && clearSeg midX stubB.Y stubB.X stubB.Y then
            [| stubA; Point.create midX stubA.Y; Point.create midX stubB.Y; stubB |]
        elif (dirA = 1 || dirA = 3) && clearSeg stubA.X stubA.Y stubA.X midY && clearSeg stubA.X midY stubB.X midY && clearSeg stubB.X midY stubB.X stubB.Y then
            [| stubA; Point.create stubA.X midY; Point.create stubB.X midY; stubB |]
        elif clearSeg stubA.X stubA.Y stubB.X stubA.Y && clearSeg stubB.X stubA.Y stubB.X stubB.Y then
            [| stubA; Point.create stubB.X stubA.Y; stubB |]
        elif clearSeg stubA.X stubA.Y stubA.X stubB.Y && clearSeg stubA.X stubB.Y stubB.X stubB.Y then
            [| stubA; Point.create stubA.X stubB.Y; stubB |]
        else
            let rawXs = ws.RawXs
            let rawYs = ws.RawYs
            rawXs.Clear()
            rawYs.Clear()
            rawXs.Add stubA.X; rawXs.Add stubB.X; rawXs.Add midX
            rawYs.Add stubA.Y; rawYs.Add stubB.Y; rawYs.Add midY
            let limitObs = min obsCount 48
            for k = 0 to limitObs - 1 do
                let struct(rx0, ry0, rx1, ry1) = obstacles[k]
                rawXs.Add rx0; rawXs.Add rx1
                rawYs.Add ry0; rawYs.Add ry1

            let xs = sortDedup rawXs
            let ys = sortDedup rawYs
            let nx, ny = xs.Length, ys.Length
            let sx, sy = findCoordIndex xs stubA.X, findCoordIndex ys stubA.Y
            let tx, ty = findCoordIndex xs stubB.X, findCoordIndex ys stubB.Y

            // State = ((y * nx + x) * 4 + dir), dir: 0=E, 1=S, 2=W, 3=N
            let stateCount = nx * ny * 4
            if ws.GScore.Length < stateCount then
                let cap = max stateCount (ws.GScore.Length * 2)
                ws.GScore <- Array.zeroCreate<float> cap
                ws.Parent <- Array.zeroCreate<int> cap
                ws.Stamp <- Array.zeroCreate<int> cap
                ws.CurStamp <- 0

            ws.CurStamp <- ws.CurStamp + 1
            if ws.CurStamp = Int32.MaxValue then
                Array.fill ws.Stamp 0 ws.Stamp.Length 0
                ws.CurStamp <- 1
            let stamp = ws.CurStamp
            let gScore = ws.GScore
            let parent = ws.Parent
            let stampArr = ws.Stamp
#if FABLE_COMPILER
            let pq = PriorityQueue<int, float>()
#else
            let pq = ws.Pq
            pq.Clear()
#endif

            let txX = xs[tx]
            let tyY = ys[ty]
            let inline heuristic xi yi = abs (txX - xs[xi]) + abs (tyY - ys[yi])
            for d = 0 to 3 do
                let s0 = (sy * nx + sx) * 4 + d
                let initBend = if d = dirA then 0. else bendPenalty * 0.5
                stampArr[s0] <- stamp
                gScore[s0] <- initBend
                parent[s0] <- -1
                pq.Enqueue(s0, initBend + heuristic sx sy)

            // Precompute elementary grid-edge blocked flags (nx * ny horizontal + nx * ny vertical) lazily on first probe via stamp!
            let mutable goalState = -1

            while pq.Count > 0 && goalState < 0 do
                let cur = pq.Dequeue()
                if stampArr[cur] = stamp then
                    let curG = gScore[cur]
                    let d = cur &&& 3
                    let cell = cur >>> 2
                    let xi = cell % nx
                    let yi = cell / nx
                    if xi = tx && yi = ty then
                        goalState <- cur
                    else
                        let x0, y0 = xs[xi], ys[yi]
                        for nd = 0 to 3 do
                            // Disallow immediate 180-degree reversal
                            if ((nd + 2) &&& 3) <> d then
                                let nxi = xi + dxStep[nd]
                                let nyi = yi + dyStep[nd]
                                if nxi >= 0 && nxi < nx && nyi >= 0 && nyi < ny then
                                    let nextState = (nyi * nx + nxi) * 4 + nd
                                    let x1, y1 = xs[nxi], ys[nyi]
                                    let stepLen = abs (x1 - x0) + abs (y1 - y0)
                                    let bendCost = if nd = d then 0. else bendPenalty
                                    // Check optimistic bound before scanning obstacles!
                                    let minTentative = curG + stepLen + bendCost
                                    let prevG = if stampArr[nextState] = stamp then gScore[nextState] else infinity
                                    if minTentative < prevG - 1e-9 then
                                        let blocked = not (clearSeg x0 y0 x1 y1)
                                        let obsCost = if blocked then 5000. + stepLen * 20. else 0.
                                        let tentative = minTentative + obsCost
                                        if tentative < prevG - 1e-9 then
                                            stampArr[nextState] <- stamp
                                            gScore[nextState] <- tentative
                                            parent[nextState] <- cur
                                            pq.Enqueue(nextState, tentative + heuristic nxi nyi)

            if goalState < 0 then
                [| stubA; Point.create stubB.X stubA.Y; stubB |]
            else
                let revPts = ws.RevPts
                revPts.Clear()
                let mutable st = goalState
                while st >= 0 do
                    let cell = st >>> 2
                    let xi = cell % nx
                    let yi = cell / nx
                    revPts.Add(Point.create xs[xi] ys[yi])
                    st <- parent[st]
                let cnt = revPts.Count
                let out = Array.zeroCreate<Point> cnt
                for i = 0 to cnt - 1 do
                    out[i] <- revPts[cnt - 1 - i]
                if cnt > 0 then
                    out[0] <- stubA
                    out[cnt - 1] <- stubB
                out

    /// <summary>Routes all associations in <c>eff</c> over <c>positionedNodes</c>, resolving auto-ports, avoiding obstacles and nudging parallel channels.</summary>
    let internal routeOverNodes
        (token: System.Threading.CancellationToken)
        (config: OrthogonalRouterConfig)
        (eff: EffectiveDiagram)
        (positionedNodes: PositionedNode[])
        (reversedFlags: bool[] option)
        : PositionedNode[] * RoutedAssociation list =
        let n = positionedNodes.Length
        let m = eff.Associations.Length
        if n = 0 || m = 0 then
            positionedNodes, []
        else
            let margin = max 2. config.ObstacleMargin
            let stubLen = max 4. config.PortStubLength
            let spacing = max 0. config.ChannelSpacing

            // Allocate boundary slots per (nodeIndex, side) so both explicit and unnamed endpoints separate cleanly along node sides!
            let sideRequests = Array.init (n * 4) (fun _ -> ResizeArray<struct(int * bool * float * string option)>())
            let sideToIdx = function
                | PortSide.North -> 0
                | PortSide.South -> 1
                | PortSide.East -> 2
                | PortSide.West
                | PortSide.Auto -> 3
            let idxToSide = function
                | 0 -> PortSide.North
                | 1 -> PortSide.South
                | 2 -> PortSide.East
                | _ -> PortSide.West

            let findPortSpec (nd: DiagramNode) (pid: string option) =
                pid |> Option.bind (fun id -> nd.Ports |> List.tryFind (fun p -> p.Id = id))

            let srcSides = Array.zeroCreate<PortSide> m
            let dstSides = Array.zeroCreate<PortSide> m

            for e = 0 to m - 1 do
                let a = eff.Associations[e]
                let ui = eff.ReversedIdMap[a.Source.NodeId]
                let vi = eff.ReversedIdMap[a.Target.NodeId]
                if ui = vi then
                    srcSides[e] <- PortSide.North
                    dstSides[e] <- PortSide.East
                else
                    let uC = positionedNodes[ui].Center
                    let vC = positionedNodes[vi].Center
                    let sSide =
                        match findPortSpec eff.Nodes[ui] a.Source.PortId with
                        | Some p when p.Side <> PortSide.Auto -> p.Side
                        | _ -> chooseAutoSide uC vC
                    let tSide =
                        match findPortSpec eff.Nodes[vi] a.Target.PortId with
                        | Some p when p.Side <> PortSide.Auto -> p.Side
                        | _ -> chooseAutoSide vC uC
                    srcSides[e] <- sSide
                    dstSides[e] <- tSide
                    let sSortKey = if sSide = PortSide.North || sSide = PortSide.South then vC.X else vC.Y
                    let tSortKey = if tSide = PortSide.North || tSide = PortSide.South then uC.X else uC.Y
                    sideRequests[ui * 4 + sideToIdx sSide].Add(struct(e, true, sSortKey, a.Source.PortId))
                    sideRequests[vi * 4 + sideToIdx tSide].Add(struct(e, false, tSortKey, a.Target.PortId))

            let srcAnchors = Array.zeroCreate<Point> m
            let dstAnchors = Array.zeroCreate<Point> m
            let resolvedNamedPorts = Array.init n (fun _ -> Dictionary<string, PositionedPort>(StringComparer.Ordinal))

            for ni = 0 to n - 1 do
                let b = positionedNodes[ni].Bounds
                let x0, y0, w, h = Bounds.x b, Bounds.y b, Bounds.width b, Bounds.height b
                // Seed declared explicit non-Auto ports first so unused ports still have valid anchors
                for p in positionedNodes[ni].Ports do
                    resolvedNamedPorts[ni][p.Id] <- p

                for si = 0 to 3 do
                    let reqs = sideRequests[ni * 4 + si]
                    if reqs.Count > 0 then
                        let side = idxToSide si
                        reqs.Sort(Comparison<struct(int * bool * float * string option)>(fun a b ->
                            let struct(ea, _, ka, pa) = a
                            let struct(eb, _, kb, pb) = b
                            // Explicit port order wins first if both name explicit ports on this node
                            let oa = findPortSpec eff.Nodes[ni] pa |> Option.map (fun sp -> sp.Order)
                            let ob = findPortSpec eff.Nodes[ni] pb |> Option.map (fun sp -> sp.Order)
                            match oa, ob with
                            | Some x, Some y when x <> y -> compare x y
                            | _ ->
                                let c = compare ka kb
                                if c <> 0 then c else compare ea eb))

                        // Group requests sharing the exact same named port ID so one port has one consistent anchor!
                        let distinctSlots = ResizeArray<ResizeArray<struct(int * bool * string option)>>()
                        let portSlotIdx = Dictionary<string, int>(StringComparer.Ordinal)
                        for r in reqs do
                            let struct(e, isSrc, _, pidOpt) = r
                            match pidOpt with
                            | Some pid ->
                                match portSlotIdx.TryGetValue pid with
                                | true, existing -> distinctSlots[existing].Add(struct(e, isSrc, pidOpt))
                                | _ ->
                                    let idx = distinctSlots.Count
                                    portSlotIdx.Add(pid, idx)
                                    let bucket = ResizeArray<struct(int * bool * string option)>()
                                    bucket.Add(struct(e, isSrc, pidOpt))
                                    distinctSlots.Add bucket
                            | None ->
                                let bucket = ResizeArray<struct(int * bool * string option)>()
                                bucket.Add(struct(e, isSrc, None))
                                distinctSlots.Add bucket

                        let slotCount = distinctSlots.Count
                        for k = 0 to slotCount - 1 do
                            let t = float (k + 1) / float (slotCount + 1)
                            let pt =
                                match side with
                                | PortSide.North -> Point.create (x0 + w * t) y0
                                | PortSide.South -> Point.create (x0 + w * t) (y0 + h)
                                | PortSide.West -> Point.create x0 (y0 + h * t)
                                | PortSide.East
                                | PortSide.Auto -> Point.create (x0 + w) (y0 + h * t)
                            for item in distinctSlots[k] do
                                let struct(e, isSrc, pidOpt) = item
                                if isSrc then srcAnchors[e] <- pt else dstAnchors[e] <- pt
                                pidOpt |> Option.iter (fun pid ->
                                    resolvedNamedPorts[ni][pid] <- { Id = pid; Side = side; Anchor = pt })

            let updatedNodes =
                positionedNodes
                |> Array.mapi (fun ni pn ->
                    let ports =
                        resolvedNamedPorts[ni].Values
                        |> Seq.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))
                        |> Seq.toList
                    { pn with Ports = ports })

            let allObstacles =
                updatedNodes
                |> Array.map (fun pn ->
                    let b = pn.Bounds
                    struct(Bounds.x b - margin, Bounds.y b - margin, Bounds.x b + Bounds.width b + margin, Bounds.y b + Bounds.height b + margin))

            let rawChains = Array.zeroCreate<Point[]> m
            let sideDir = function
                | PortSide.East -> 0
                | PortSide.South -> 1
                | PortSide.West -> 2
                | PortSide.North -> 3
                | PortSide.Auto -> 0

            let ws = createWorkspace ()
            let localObs = ResizeArray<struct(float * float * float * float)>(min n 64)
            let fullBuf = ResizeArray<Point>(16)

            for e = 0 to m - 1 do
                SimdMath.check token e
                let a = eff.Associations[e]
                let ui = eff.ReversedIdMap[a.Source.NodeId]
                let vi = eff.ReversedIdMap[a.Target.NodeId]
                if ui = vi then
                    let b = updatedNodes[ui].Bounds
                    let x1 = Bounds.x b + Bounds.width b
                    let y0 = Bounds.y b
                    let pStart =
                        a.Source.PortId
                        |> Option.bind (fun pid -> updatedNodes[ui].Ports |> List.tryFind (fun p -> p.Id = pid))
                        |> Option.map (fun p -> p.Anchor)
                        |> Option.defaultValue (Point.create (x1 - min 14. (Bounds.width b * 0.3)) y0)
                    let pEnd =
                        a.Target.PortId
                        |> Option.bind (fun pid -> updatedNodes[ui].Ports |> List.tryFind (fun p -> p.Id = pid))
                        |> Option.map (fun p -> p.Anchor)
                        |> Option.defaultValue (Point.create x1 (y0 + min 14. (Bounds.height b * 0.35)))
                    let topY = y0 - stubLen - float (e % 3) * 6.
                    let rightX = x1 + stubLen + float (e % 3) * 6.
                    fullBuf.Clear()
                    fullBuf.Add pStart
                    fullBuf.Add(Point.create pStart.X topY)
                    fullBuf.Add(Point.create rightX topY)
                    fullBuf.Add(Point.create rightX pEnd.Y)
                    fullBuf.Add pEnd
                    rawChains[e] <- simplifyOrthogonal fullBuf
                else
                    let pStart, pEnd = srcAnchors[e], dstAnchors[e]
                    let nxS, nyS = sideNormal srcSides[e]
                    let nxT, nyT = sideNormal dstSides[e]
                    let stubA = Point.create (pStart.X + nxS * stubLen) (pStart.Y + nyS * stubLen)
                    let stubB = Point.create (pEnd.X + nxT * stubLen) (pEnd.Y + nyT * stubLen)

                    let cMinX = min stubA.X stubB.X - 64.
                    let cMaxX = max stubA.X stubB.X + 64.
                    let cMinY = min stubA.Y stubB.Y - 64.
                    let cMaxY = max stubA.Y stubB.Y + 64.

                    localObs.Clear()
                    for ni = 0 to n - 1 do
                        let struct(ox0, oy0, ox1, oy1) = allObstacles[ni]
                        if ox1 >= cMinX && ox0 <= cMaxX && oy1 >= cMinY && oy0 <= cMaxY then
                            if ni = ui || ni = vi then
                                let b = updatedNodes[ni].Bounds
                                localObs.Add(struct(Bounds.x b - 2., Bounds.y b - 2., Bounds.x b + Bounds.width b + 2., Bounds.y b + Bounds.height b + 2.))
                            else
                                localObs.Add(allObstacles[ni])

                    let midRoute = aStarOrthogonal ws stubA (sideDir srcSides[e]) stubB config.BendPenalty localObs
                    fullBuf.Clear()
                    fullBuf.Add pStart
                    for k = 0 to midRoute.Length - 1 do fullBuf.Add midRoute[k]
                    fullBuf.Add pEnd
                    rawChains[e] <- simplifyOrthogonal fullBuf

            let nudged = Array.zeroCreate<bool> m
            // Parallel channel nudging for overlapping interior collinear segments across distinct associations
            if spacing > 0. && m > 1 then
                let hBuckets = Dictionary<int, ResizeArray<struct(int * int * float * float)>>()
                let vBuckets = Dictionary<int, ResizeArray<struct(int * int * float * float)>>()
                for e = 0 to m - 1 do
                    let pts = rawChains[e]
                    if pts.Length >= 4 then
                        for s = 1 to pts.Length - 3 do
                            let a, b = pts[s], pts[s + 1]
                            if abs (a.Y - b.Y) <= 1e-4 && abs (a.X - b.X) > 1. then
                                let key = int (Math.Round(a.Y * 2.))
                                let list =
                                    match hBuckets.TryGetValue key with
                                    | true, l -> l
                                    | _ -> let l = ResizeArray() in hBuckets.Add(key, l); l
                                list.Add(struct(e, s, min a.X b.X, max a.X b.X))
                            elif abs (a.X - b.X) <= 1e-4 && abs (a.Y - b.Y) > 1. then
                                let key = int (Math.Round(a.X * 2.))
                                let list =
                                    match vBuckets.TryGetValue key with
                                    | true, l -> l
                                    | _ -> let l = ResizeArray() in vBuckets.Add(key, l); l
                                list.Add(struct(e, s, min a.Y b.Y, max a.Y b.Y))

                for kv in hBuckets do
                    let segs = kv.Value
                    if segs.Count > 1 then
                        let active = ResizeArray<struct(int * int * float * float)>()
                        for k = 0 to segs.Count - 1 do
                            let item = segs[k]
                            let struct(_, _, lo, hi) = item
                            let mutable overlaps = active.Count = 0
                            let mutable ai = 0
                            while not overlaps && ai < active.Count do
                                let struct(_, _, olo, ohi) = active[ai]
                                if max lo olo < min hi ohi - 1e-3 then overlaps <- true
                                ai <- ai + 1
                            if overlaps then active.Add item
                        if active.Count > 1 then
                            for rank = 0 to active.Count - 1 do
                                let struct(e, s, _, _) = active[rank]
                                let delta = (float rank - float (active.Count - 1) * 0.5) * spacing
                                if abs delta > 1e-6 then
                                    let pts = if nudged[e] then rawChains[e] else Array.copy rawChains[e]
                                    pts[s] <- Point.create pts[s].X (pts[s].Y + delta)
                                    pts[s + 1] <- Point.create pts[s + 1].X (pts[s + 1].Y + delta)
                                    rawChains[e] <- pts
                                    nudged[e] <- true

                for kv in vBuckets do
                    let segs = kv.Value
                    if segs.Count > 1 then
                        let active = ResizeArray<struct(int * int * float * float)>()
                        for k = 0 to segs.Count - 1 do
                            let item = segs[k]
                            let struct(_, _, lo, hi) = item
                            let mutable overlaps = active.Count = 0
                            let mutable ai = 0
                            while not overlaps && ai < active.Count do
                                let struct(_, _, olo, ohi) = active[ai]
                                if max lo olo < min hi ohi - 1e-3 then overlaps <- true
                                ai <- ai + 1
                            if overlaps then active.Add item
                        if active.Count > 1 then
                            for rank = 0 to active.Count - 1 do
                                let struct(e, s, _, _) = active[rank]
                                let delta = (float rank - float (active.Count - 1) * 0.5) * spacing
                                if abs delta > 1e-6 then
                                    let pts = if nudged[e] then rawChains[e] else Array.copy rawChains[e]
                                    pts[s] <- Point.create (pts[s].X + delta) pts[s].Y
                                    pts[s + 1] <- Point.create (pts[s + 1].X + delta) pts[s + 1].Y
                                    rawChains[e] <- pts
                                    nudged[e] <- true

            let mutable routed = []
            for e = m - 1 downto 0 do
                let a = eff.Associations[e]
                let pts =
                    if nudged[e] then
                        fullBuf.Clear()
                        let rc = rawChains[e]
                        for k = 0 to rc.Length - 1 do fullBuf.Add rc[k]
                        simplifyOrthogonal fullBuf
                    else rawChains[e]
                let path = buildPathWithCorners config.CornerRadius pts
                let rev = match reversedFlags with Some f -> f[e] | None -> false
                let sLbl, mLbl, tLbl = GraphKernels.placeLabelsOnWaypoints a pts
                let mutable wpList = []
                for k = pts.Length - 1 downto 0 do wpList <- pts[k] :: wpList
                routed <-
                    { Id = a.Id
                      Source = a.Source
                      Target = a.Target
                      Kind = a.Kind
                      Waypoints = wpList
                      Path = path
                      ReversedInCycle = rev
                      SourceLabel = sLbl
                      CenterLabel = mLbl
                      TargetLabel = tLbl } :: routed

            updatedNodes, routed

    /// <summary>Re-routes all associations of an existing <c>DiagramLayout2D</c> with obstacle-avoiding orthogonal paths and parallel channel separation.</summary>
    let routeWithCancellation
        (token: System.Threading.CancellationToken)
        (config: OrthogonalRouterConfig)
        (graph: DiagramGraph)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.ObstacleMargin && Double.IsFinite config.PortStubLength
                && Double.IsFinite config.BendPenalty && Double.IsFinite config.ChannelSpacing
                && Double.IsFinite config.CornerRadius)
           || config.ObstacleMargin < 0. || config.PortStubLength <= 0.
           || config.BendPenalty < 0. || config.ChannelSpacing < 0. || config.CornerRadius < 0. then
            Error [ InvalidObservation "OrthogonalRouter requires finite nonnegative margins/penalties and positive PortStubLength." ]
        else
            let eff = DiagramGraph.effective graph
            let byId = Dictionary<string, PositionedNode>(StringComparer.Ordinal)
            for pn in layout.Nodes do byId[pn.Id] <- pn
            let missing = eff.Nodes |> Array.tryFind (fun nd -> not (byId.ContainsKey nd.Id))
            match missing with
            | Some nd -> Error [ MissingEndpoint nd.Id ]
            | None ->
                let orderedNodes = eff.Nodes |> Array.map (fun nd -> byId[nd.Id])
                let revFlags =
                    let map = Dictionary<string, bool>(StringComparer.Ordinal)
                    for ra in layout.Associations do map[ra.Id] <- ra.ReversedInCycle
                    eff.Associations |> Array.map (fun a -> match map.TryGetValue a.Id with true, r -> r | _ -> false)
                let updatedNodes, routed = routeOverNodes token config eff orderedNodes (Some revFlags)
                let nList = Array.toList updatedNodes
                Ok
                    { Bounds = GraphKernels.computeDiagramBounds layout.Bounds nList layout.Groups routed
                      Nodes = nList
                      Groups = layout.Groups
                      Associations = routed }

    /// <summary>Re-routes all associations of an existing <c>DiagramLayout2D</c> orthogonally without cancellation.</summary>
    let route config graph layout = routeWithCancellation System.Threading.CancellationToken.None config graph layout

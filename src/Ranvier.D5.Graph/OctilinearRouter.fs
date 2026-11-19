namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Configuration for 45-degree metro-map schematic octilinear routing (Nöllenburg &amp; Wolff 2011; Bast, Brosi &amp; Storandt <c>LOOM</c> 2020).</summary>
type OctilinearRouterConfig =
    { /// <summary>Clearance margin around non-endpoint node obstacles in layout units.</summary>
      ObstacleMargin: float
      /// <summary>Perpendicular stub distance extending outward from each port before turning.</summary>
      PortStubLength: float
      /// <summary>Base octilinear lattice spacing in layout units.</summary>
      GridSpacing: float
      /// <summary>Path-cost penalty for a gentle 45-degree metro bend (135-degree interior angle).</summary>
      Bend45Penalty: float
      /// <summary>Path-cost penalty for a 90-degree orthogonal bend.</summary>
      Bend90Penalty: float
      /// <summary>Lateral track spacing between parallel overlapping octilinear metro trunks.</summary>
      TrackSeparation: float
      /// <summary>Optional quadratic fillet radius at bends (0 produces crisp metro corners).</summary>
      CornerFilletRadius: float }

/// <summary>Default configuration values for <c>OctilinearRouterConfig</c>.</summary>
[<RequireQualifiedAccess>]
module OctilinearRouterConfig =
    /// <summary>Default metro-map router settings: 10-unit obstacle margin, 14-unit port stubs, 24-unit grid spacing, 10-unit 45-degree bend penalty, 32-unit 90-degree bend penalty, 7-unit track separation and 4-unit corner fillets.</summary>
    let defaultValue =
        { ObstacleMargin = 10.
          PortStubLength = 14.
          GridSpacing = 24.
          Bend45Penalty = 10.
          Bend90Penalty = 32.
          TrackSeparation = 7.
          CornerFilletRadius = 4. }

/// <summary>Obstacle-avoiding 45-degree octilinear metro-map router with state-space A* and parallel metro-trunk track nudging.</summary>
[<RequireQualifiedAccess>]
module OctilinearRouter =
    let private invSqrt2 = 0.7071067811865475244
    let private sqrt2Minus1 = 0.4142135623730950488

    // 8 compass directions clockwise from East (0 deg):
    // 0=E(0), 1=SE(45), 2=S(90), 3=SW(135), 4=W(180), 5=NW(225), 6=N(270), 7=NE(315)
    let private sideNormal = function
        | PortSide.North -> 0., -1.
        | PortSide.South -> 0., 1.
        | PortSide.West -> -1., 0.
        | PortSide.East
        | PortSide.Auto -> 1., 0.

    let private sideOutOctant = function
        | PortSide.East -> 0
        | PortSide.South -> 2
        | PortSide.West -> 4
        | PortSide.North -> 6
        | PortSide.Auto -> 0

    let private chooseAutoSide (fromCenter: Point) (toCenter: Point) =
        let dx = toCenter.X - fromCenter.X
        let dy = toCenter.Y - fromCenter.Y
        if abs dx >= abs dy then
            if dx >= 0. then PortSide.East else PortSide.West
        else
            if dy >= 0. then PortSide.South else PortSide.North

    /// <summary>Classifies an exact octilinear vector <c>(dx, dy)</c> into octant <c>0..7</c>, or <c>-1</c> if zero/non-octilinear.</summary>
    let private classifyOctant (dx: float) (dy: float) : int =
        let ax, ay = abs dx, abs dy
        if ax <= 1e-6 && ay <= 1e-6 then -1
        elif ay <= 1e-5 * max 1. ax then
            if dx >= 0. then 0 else 4
        elif ax <= 1e-5 * max 1. ay then
            if dy >= 0. then 2 else 6
        elif abs (ax - ay) <= 1e-4 * max 1. (max ax ay) then
            if dx >= 0. then (if dy >= 0. then 1 else 7)
            else (if dy >= 0. then 3 else 5)
        else -1

    let inline private turnSteps (d1: int) (d2: int) : int =
        let diff = abs (d2 - d1)
        if diff <= 4 then diff else 8 - diff

    let inline private turnCost (bend45: float) (bend90: float) (steps: int) : float =
        match steps with
        | 0 -> 0.
        | 1 -> bend45
        | 2 -> bend90
        | _ -> infinity

    let inline private octHeuristic (dx: float) (dy: float) : float =
        let ax, ay = abs dx, abs dy
        let mn = min ax ay
        let mx = max ax ay
        sqrt2Minus1 * mn + mx

    /// <summary>Liang-Barsky exact intersection test of arbitrary 2D segment <c>(ax, ay) -&gt; (bx, by)</c> with open rectangle <c>(rx0, ry0)..(rx1, ry1)</c>.</summary>
    let private segmentHitsRect (ax: float) (ay: float) (bx: float) (by: float) (rx0: float) (ry0: float) (rx1: float) (ry1: float) : bool =
        let eps = 1e-5
        let xMin, xMax = rx0 + eps, rx1 - eps
        let yMin, yMax = ry0 + eps, ry1 - eps
        if xMin >= xMax || yMin >= yMax then false
        else
            let dx = bx - ax
            let dy = by - ay
            let mutable t0 = 0.
            let mutable t1 = 1.
            let inline clip p q =
                if abs p <= 1e-12 then
                    q >= 0.
                else
                    let r = q / p
                    if p < 0. then
                        if r > t1 then false
                        else (if r > t0 then t0 <- r); true
                    else
                        if r < t0 then false
                        else (if r < t1 then t1 <- r); true
            if clip (-dx) (ax - xMin)
               && clip dx (xMax - ax)
               && clip (-dy) (ay - yMin)
               && clip dy (yMax - ay) then
                t0 < t1 - 1e-7
            else
                false

    let private simplifyOctilinear (pts: ResizeArray<Point>) : Point[] =
        if pts.Count <= 2 then pts.ToArray()
        else
            let out = ResizeArray<Point>(pts.Count)
            for i = 0 to pts.Count - 1 do
                let p = pts[i]
                if out.Count = 0 then
                    out.Add p
                else
                    let prev = out[out.Count - 1]
                    if Double.Hypot(p.X - prev.X, p.Y - prev.Y) > 1e-5 then
                        if out.Count >= 2 then
                            let prev2 = out[out.Count - 2]
                            let d1 = classifyOctant (prev.X - prev2.X) (prev.Y - prev2.Y)
                            let d2 = classifyOctant (p.X - prev.X) (p.Y - prev.Y)
                            if d1 >= 0 && d1 = d2 then
                                out[out.Count - 1] <- p
                            else
                                out.Add p
                        else
                            out.Add p
            out.ToArray()

    let private buildPathWithFillets (filletRadius: float) (pts: Point[]) : Path =
        if pts.Length = 0 then Path.empty
        elif pts.Length = 1 then Path.ofCommands [ MoveTo pts[0]; LineTo pts[0] ]
        elif filletRadius <= 1e-6 || pts.Length = 2 then
            let cmds = ResizeArray<PathCommand>(pts.Length)
            cmds.Add(MoveTo pts[0])
            for i = 1 to pts.Length - 1 do cmds.Add(LineTo pts[i])
            Path.ofCommands(List.ofSeq cmds)
        else
            let cmds = ResizeArray<PathCommand>(pts.Length * 2)
            cmds.Add(MoveTo pts[0])
            for i = 1 to pts.Length - 2 do
                let a, b, c = pts[i - 1], pts[i], pts[i + 1]
                let d1 = Double.Hypot(b.X - a.X, b.Y - a.Y)
                let d2 = Double.Hypot(c.X - b.X, c.Y - b.Y)
                let r = min filletRadius (0.42 * min d1 d2)
                if r <= 0.25 || d1 <= 1e-6 || d2 <= 1e-6 then
                    cmds.Add(LineTo b)
                else
                    let pre = Point.create (b.X - (b.X - a.X) * (r / d1)) (b.Y - (b.Y - a.Y) * (r / d1))
                    let post = Point.create (b.X + (c.X - b.X) * (r / d2)) (b.Y + (c.Y - b.Y) * (r / d2))
                    cmds.Add(LineTo pre)
                    cmds.Add(QuadraticTo(b, post))
            cmds.Add(LineTo pts[pts.Length - 1])
            Path.ofCommands(List.ofSeq cmds)

    let private sortDedup (vals: ResizeArray<float>) : float[] =
        let arr = vals.ToArray()
        Array.Sort arr
        let out = ResizeArray<float>(arr.Length)
        for i = 0 to arr.Length - 1 do
            if out.Count = 0 || abs (arr[i] - out[out.Count - 1]) > 1e-4 then
                out.Add arr[i]
        out.ToArray()

    type private OctAStarWorkspace =
        { mutable GScore: float[]
          mutable Parent: int[]
          mutable Stamp: int[]
          mutable CurStamp: int
          RawXs: ResizeArray<float>
          RawYs: ResizeArray<float>
          DiagPlus: ResizeArray<float>
          DiagMinus: ResizeArray<float>
          Verts: ResizeArray<Point>
          RevPts: ResizeArray<Point> }

    let private createWorkspace () =
        let initCap = 8192
        { GScore = Array.zeroCreate<float> initCap
          Parent = Array.zeroCreate<int> initCap
          Stamp = Array.zeroCreate<int> initCap
          CurStamp = 0
          RawXs = ResizeArray<float>(64)
          RawYs = ResizeArray<float>(64)
          DiagPlus = ResizeArray<float>(32)
          DiagMinus = ResizeArray<float>(32)
          Verts = ResizeArray<Point>(1024)
          RevPts = ResizeArray<Point>(64) }

    /// <summary>Evaluates a fast-path octilinear candidate between <c>stubA</c> and <c>stubB</c>; returns <c>infinity</c> if blocked or containing &gt;= 135-degree hairpins.</summary>
    let private evaluateCandidate
        (dirA: int)
        (inDirB: int)
        (bend45: float)
        (bend90: float)
        (obstacles: ResizeArray<struct(float * float * float * float)>)
        (cand: Point[])
        : float =
        let obsCount = obstacles.Count
        let inline clearSeg x0 y0 x1 y1 =
            let mutable ok = true
            let mutable k = 0
            while ok && k < obsCount do
                let struct(rx0, ry0, rx1, ry1) = obstacles[k]
                if segmentHitsRect x0 y0 x1 y1 rx0 ry0 rx1 ry1 then ok <- false
                k <- k + 1
            ok

        // Deduplicate tiny zero-length steps first
        let buf = ResizeArray<Point>(cand.Length)
        for i = 0 to cand.Length - 1 do
            if buf.Count = 0 || Double.Hypot(cand[i].X - buf[buf.Count - 1].X, cand[i].Y - buf[buf.Count - 1].Y) > 1e-5 then
                buf.Add cand[i]

        if buf.Count = 1 then
            let ts = turnSteps dirA inDirB
            if ts >= 3 then infinity else turnCost bend45 bend90 ts
        else
            let mutable cost = 0.
            let mutable prevDir = dirA
            let mutable valid = true
            let mutable s = 0
            while valid && s < buf.Count - 1 do
                let a, b = buf[s], buf[s + 1]
                let d = classifyOctant (b.X - a.X) (b.Y - a.Y)
                if d < 0 then
                    valid <- false
                else
                    let ts = turnSteps prevDir d
                    if ts >= 3 || not (clearSeg a.X a.Y b.X b.Y) then
                        valid <- false
                    else
                        cost <- cost + Double.Hypot(b.X - a.X, b.Y - a.Y) + turnCost bend45 bend90 ts
                        prevDir <- d
                s <- s + 1
            if not valid then infinity
            else
                let finalTs = turnSteps prevDir inDirB
                if finalTs >= 3 then infinity
                else cost + turnCost bend45 bend90 finalTs

    /// <summary>Routes an exact octilinear chain from <c>stubA</c> to <c>stubB</c> via direct metro candidates or 8-octant line-arrangement State-Space A*.</summary>
    let private routeStubToStub
        (ws: OctAStarWorkspace)
        (stubA: Point)
        (dirA: int)
        (stubB: Point)
        (inDirB: int)
        (config: OctilinearRouterConfig)
        (obstacles: ResizeArray<struct(float * float * float * float)>)
        : Point[] =
        let dx = stubB.X - stubA.X
        let dy = stubB.Y - stubA.Y
        let adx, ady = abs dx, abs dy
        let sx = if dx >= 0. then 1. else -1.
        let sy = if dy >= 0. then 1. else -1.
        let dMin = min adx ady

        // Construct direct 0-bend, 1-bend (45-degree knee), 90-degree L, and symmetric 2-bend 45-degree metro candidates!
        let kneeDiagFirst = Point.create (stubA.X + sx * dMin) (stubA.Y + sy * dMin)
        let kneeCardFirst = Point.create (stubB.X - sx * dMin) (stubB.Y - sy * dMin)
        let candidates = ResizeArray<Point[]>(8)
        if classifyOctant dx dy >= 0 then
            candidates.Add [| stubA; stubB |]
        candidates.Add [| stubA; kneeDiagFirst; stubB |]
        candidates.Add [| stubA; kneeCardFirst; stubB |]
        if adx >= ady && ady > 1e-5 then
            let mx = 0.5 * (stubA.X + stubB.X)
            let p1 = Point.create (mx - 0.5 * sx * ady) stubA.Y
            let p2 = Point.create (mx + 0.5 * sx * ady) stubB.Y
            candidates.Add [| stubA; p1; p2; stubB |]
        elif ady > adx && adx > 1e-5 then
            let my = 0.5 * (stubA.Y + stubB.Y)
            let p1 = Point.create stubA.X (my - 0.5 * sy * adx)
            let p2 = Point.create stubB.X (my + 0.5 * sy * adx)
            candidates.Add [| stubA; p1; p2; stubB |]
        candidates.Add [| stubA; Point.create stubB.X stubA.Y; stubB |]
        candidates.Add [| stubA; Point.create stubA.X stubB.Y; stubB |]

        let mutable bestCand : Point[] option = None
        let mutable bestCandCost = infinity
        for i = 0 to candidates.Count - 1 do
            let c = candidates[i]
            let sc = evaluateCandidate dirA inDirB config.Bend45Penalty config.Bend90Penalty obstacles c
            if sc < bestCandCost then
                bestCandCost <- sc
                bestCand <- Some c

        match bestCand with
        | Some winner -> winner
        | None ->
            // Build exact octilinear line arrangement over corridor + obstacle corners + 45-degree diagonals.
            let pad = max 48. (config.GridSpacing * 2.5)
            let mutable cMinX = min stubA.X stubB.X - pad
            let mutable cMaxX = max stubA.X stubB.X + pad
            let mutable cMinY = min stubA.Y stubB.Y - pad
            let mutable cMaxY = max stubA.Y stubB.Y + pad
            let obsCount = obstacles.Count
            let limitObs = min obsCount 24
            for k = 0 to limitObs - 1 do
                let struct(rx0, ry0, rx1, ry1) = obstacles[k]
                if rx0 - 8. < cMinX then cMinX <- rx0 - 8.
                if rx1 + 8. > cMaxX then cMaxX <- rx1 + 8.
                if ry0 - 8. < cMinY then cMinY <- ry0 - 8.
                if ry1 + 8. > cMaxY then cMaxY <- ry1 + 8.

            let spanMax = max (cMaxX - cMinX) (cMaxY - cMinY)
            let step = max (max 12. config.GridSpacing) (spanMax / 16.)

            let rawXs = ws.RawXs
            let rawYs = ws.RawYs
            let dPlus = ws.DiagPlus   // y - x = c
            let dMinus = ws.DiagMinus // y + x = c
            rawXs.Clear(); rawYs.Clear(); dPlus.Clear(); dMinus.Clear()

            rawXs.Add stubA.X; rawXs.Add stubB.X; rawXs.Add (0.5 * (stubA.X + stubB.X))
            rawYs.Add stubA.Y; rawYs.Add stubB.Y; rawYs.Add (0.5 * (stubA.Y + stubB.Y))
            dPlus.Add (stubA.Y - stubA.X); dPlus.Add (stubB.Y - stubB.X)
            dMinus.Add (stubA.Y + stubA.X); dMinus.Add (stubB.Y + stubB.X)

            // Uniform lattice anchored at stubA so (stubA.X + i*step, stubA.Y + j*step) forms exact 45-degree squares!
            let kLeft = int (floor ((cMinX - stubA.X) / step))
            let kRight = int (ceil ((cMaxX - stubA.X) / step))
            for k = kLeft to kRight do
                let xv = stubA.X + float k * step
                if xv >= cMinX && xv <= cMaxX then rawXs.Add xv

            let mTop = int (floor ((cMinY - stubA.Y) / step))
            let mBot = int (ceil ((cMaxY - stubA.Y) / step))
            for m = mTop to mBot do
                let yv = stubA.Y + float m * step
                if yv >= cMinY && yv <= cMaxY then rawYs.Add yv

            for k = 0 to limitObs - 1 do
                let struct(rx0, ry0, rx1, ry1) = obstacles[k]
                rawXs.Add rx0; rawXs.Add rx1
                rawYs.Add ry0; rawYs.Add ry1
                if k < 8 then
                    dPlus.Add (ry0 - rx0); dPlus.Add (ry1 - rx1)
                    dPlus.Add (ry0 - rx1); dPlus.Add (ry1 - rx0)
                    dMinus.Add (ry0 + rx0); dMinus.Add (ry1 + rx1)
                    dMinus.Add (ry0 + rx1); dMinus.Add (ry1 + rx0)

            let xs = sortDedup rawXs
            let ys = sortDedup rawYs
            let cp = sortDedup dPlus
            let cm = sortDedup dMinus

            // Deduplicate 2D vertices quantized at 1/1024 unit (~0.001 px) while keeping exact float coordinates!
            let vertMap = Dictionary<int64, int>()
            let verts = ws.Verts
            verts.Clear()

            let inline packKey (x: float) (y: float) : int64 =
                let qx = int64 (Math.Round(x * 1024.))
                let qy = int64 (Math.Round(y * 1024.))
                (qx <<< 32) ^^^ (qy &&& 0xFFFFFFFFL)

            let inline pointInsideObstacle (x: float) (y: float) =
                let mutable hit = false
                let mutable k = 0
                while not hit && k < obsCount do
                    let struct(rx0, ry0, rx1, ry1) = obstacles[k]
                    if x > rx0 + 1e-4 && x < rx1 - 1e-4 && y > ry0 + 1e-4 && y < ry1 - 1e-4 then
                        hit <- true
                    k <- k + 1
                hit

            let addVertex (x: float) (y: float) (forceKeep: bool) : int =
                if not forceKeep && (x < cMinX || x > cMaxX || y < cMinY || y > cMaxY || pointInsideObstacle x y) then
                    -1
                else
                    let key = packKey x y
                    match vertMap.TryGetValue key with
                    | true, existing -> existing
                    | _ ->
                        let idx = verts.Count
                        verts.Add(Point.create x y)
                        vertMap.Add(key, idx)
                        idx

            let startIdx = addVertex stubA.X stubA.Y true
            let goalIdx = addVertex stubB.X stubB.Y true

            for ix = 0 to xs.Length - 1 do
                let xv = xs[ix]
                for iy = 0 to ys.Length - 1 do
                    addVertex xv ys[iy] false |> ignore
                for ip = 0 to cp.Length - 1 do
                    addVertex xv (xv + cp[ip]) false |> ignore
                for im = 0 to cm.Length - 1 do
                    addVertex xv (cm[im] - xv) false |> ignore

            for iy = 0 to ys.Length - 1 do
                let yv = ys[iy]
                for ip = 0 to cp.Length - 1 do
                    addVertex (yv - cp[ip]) yv false |> ignore
                for im = 0 to cm.Length - 1 do
                    addVertex (cm[im] - yv) yv false |> ignore

            // Also add pairwise +-45 diagonal intersections between cp and cm so pure diagonal detours connect directly!
            for ip = 0 to cp.Length - 1 do
                let cPlus = cp[ip]
                for im = 0 to cm.Length - 1 do
                    let cMin = cm[im]
                    addVertex (0.5 * (cMin - cPlus)) (0.5 * (cMin + cPlus)) false |> ignore

            let vCount = verts.Count
            let vArr = verts.ToArray()
            let adj = Array.create (vCount * 8) -1

            // Link consecutive collinear vertices along each of the 4 exact octilinear line families in O(V log V)!
            let linkFamily (keyFn: Point -> int64) (sortCoord: Point -> float) (fwdDir: int) (revDir: int) =
                let buckets = Dictionary<int64, ResizeArray<int>>()
                for vi = 0 to vCount - 1 do
                    let k = keyFn vArr[vi]
                    match buckets.TryGetValue k with
                    | true, list -> list.Add vi
                    | _ ->
                        let list = ResizeArray<int>(8)
                        list.Add vi
                        buckets.Add(k, list)
                for kv in buckets do
                    let list = kv.Value
                    if list.Count >= 2 then
                        let arr = list.ToArray()
                        Array.sortInPlaceWith (fun a b -> compare (sortCoord vArr[a]) (sortCoord vArr[b])) arr
                        for i = 0 to arr.Length - 2 do
                            let u = arr[i]
                            let v = arr[i + 1]
                            let pu, pv = vArr[u], vArr[v]
                            if classifyOctant (pv.X - pu.X) (pv.Y - pu.Y) = fwdDir then
                                adj[u * 8 + fwdDir] <- v
                                adj[v * 8 + revDir] <- u

            // 0: Horizontal (E=0, W=4)
            linkFamily (fun p -> int64 (Math.Round(p.Y * 512.))) (fun p -> p.X) 0 4
            // 1: Vertical (S=2, N=6)
            linkFamily (fun p -> int64 (Math.Round(p.X * 512.))) (fun p -> p.Y) 2 6
            // 2: +45 deg SE-NW (y - x = const, SE=1, NW=5)
            linkFamily (fun p -> int64 (Math.Round((p.Y - p.X) * 512.))) (fun p -> p.X) 1 5
            // 3: -45 deg NE-SW (y + x = const, NE=7, SW=3)
            linkFamily (fun p -> int64 (Math.Round((p.Y + p.X) * 512.))) (fun p -> p.X) 7 3

            // State-Space A* over state = v * 8 + incomingDir
            let stateCount = vCount * 8
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
            let pq = System.Collections.Generic.PriorityQueue<int, float>(256)
#endif

            let bend45 = config.Bend45Penalty
            let bend90 = config.Bend90Penalty
            let inline clearSeg x0 y0 x1 y1 =
                let mutable ok = true
                let mutable k = 0
                while ok && k < obsCount do
                    let struct(rx0, ry0, rx1, ry1) = obstacles[k]
                    if segmentHitsRect x0 y0 x1 y1 rx0 ry0 rx1 ry1 then ok <- false
                    k <- k + 1
                ok

            // Seed startIdx with incoming direction = dirA (the direction stubA just traveled from pStart!)
            let initState = startIdx * 8 + dirA
            stampArr[initState] <- stamp
            gScore[initState] <- 0.
            parent[initState] <- -1
            pq.Enqueue(initState, octHeuristic (stubB.X - stubA.X) (stubB.Y - stubA.Y))

            let mutable bestGoalState = -1
            let mutable bestGoalTotal = infinity

            while pq.Count > 0 do
                let cur = pq.Dequeue()
                if stampArr[cur] = stamp then
                    let curG = gScore[cur]
                    let v = cur >>> 3
                    let inDir = cur &&& 7
                    let pv = vArr[v]
                    if curG + octHeuristic (stubB.X - pv.X) (stubB.Y - pv.Y) < bestGoalTotal - 1e-9 then
                        if v = goalIdx then
                            let exitSteps = turnSteps inDir inDirB
                            let exitPenalty =
                                if exitSteps <= 2 then turnCost bend45 bend90 exitSteps
                                else 2500. + float exitSteps * 200.
                            let total = curG + exitPenalty
                            if total < bestGoalTotal then
                                bestGoalTotal <- total
                                bestGoalState <- cur
                        else
                            let adjBase = v * 8
                            for nd = 0 to 7 do
                                let ts = turnSteps inDir nd
                                // Strictly forbid >= 135-degree sharp hairpins!
                                if ts <= 2 then
                                    let uNext = adj[adjBase + nd]
                                    if uNext >= 0 then
                                        let nextState = uNext * 8 + nd
                                        let pNext = vArr[uNext]
                                        let stepLen = Double.Hypot(pNext.X - pv.X, pNext.Y - pv.Y)
                                        let bCost = turnCost bend45 bend90 ts
                                        let minTent = curG + stepLen + bCost
                                        let prevG = if stampArr[nextState] = stamp then gScore[nextState] else infinity
                                        if minTent < prevG - 1e-9 && minTent < bestGoalTotal - 1e-9 then
                                            let blocked = not (clearSeg pv.X pv.Y pNext.X pNext.Y)
                                            let obsPenalty = if blocked then 12000. + stepLen * 30. else 0.
                                            let tentative = minTent + obsPenalty
                                            if tentative < prevG - 1e-9 && tentative < bestGoalTotal - 1e-9 then
                                                stampArr[nextState] <- stamp
                                                gScore[nextState] <- tentative
                                                parent[nextState] <- cur
                                                let h = octHeuristic (stubB.X - pNext.X) (stubB.Y - pNext.Y)
                                                pq.Enqueue(nextState, tentative + h)

            if bestGoalState < 0 then
                // Guaranteed strictly octilinear 1-bend fallback
                if Double.Hypot(kneeDiagFirst.X - stubA.X, kneeDiagFirst.Y - stubA.Y) > 1e-5
                   && Double.Hypot(stubB.X - kneeDiagFirst.X, stubB.Y - kneeDiagFirst.Y) > 1e-5 then
                    [| stubA; kneeDiagFirst; stubB |]
                else
                    [| stubA; stubB |]
            else
                let revPts = ws.RevPts
                revPts.Clear()
                let mutable st = bestGoalState
                while st >= 0 do
                    let vi = st >>> 3
                    revPts.Add vArr[vi]
                    st <- parent[st]
                let cnt = revPts.Count
                let out = Array.zeroCreate<Point> cnt
                for i = 0 to cnt - 1 do
                    out[i] <- revPts[cnt - 1 - i]
                if cnt > 0 then
                    out[0] <- stubA
                    out[cnt - 1] <- stubB
                out

    /// <summary>Applies LOOM-style parallel metro-trunk ordering and exact angle-preserving line-intersection nudging along shared octilinear corridors.</summary>
    let private nudgeParallelMetroTrunks
        (spacing: float)
        (rawChains: Point[][])
        (allObstacles: struct(float * float * float * float)[])
        : unit =
        let m = rawChains.Length
        if spacing > 0. && m > 1 then
            // Canonical octilinear line families:
            // 0: Horizontal, u=(1,0), n=(0,1), lineCoord = y, proj = x
            // 1: Vertical,   u=(0,1), n=(-1,0), lineCoord = x, proj = y
            // 2: +45 SE-NW,  u=(s,s), n=(-s,s), lineCoord = (y-x)*s, proj = (x+y)*s
            // 3: -45 NE-SW,  u=(s,-s), n=(s,s), lineCoord = (x+y)*s, proj = (x-y)*s
            let familyVectors = function
                | 0 -> struct(1., 0., 0., 1.)
                | 1 -> struct(0., 1., -1., 0.)
                | 2 -> struct(invSqrt2, invSqrt2, -invSqrt2, invSqrt2)
                | _ -> struct(invSqrt2, -invSqrt2, invSqrt2, invSqrt2)

            let buckets = Dictionary<int64, ResizeArray<struct(int * int * float * float * float)>>()
            for e = 0 to m - 1 do
                let pts = rawChains[e]
                if pts.Length >= 4 then
                    for s = 1 to pts.Length - 3 do
                        let a, b = pts[s], pts[s + 1]
                        let oct = classifyOctant (b.X - a.X) (b.Y - a.Y)
                        if oct >= 0 && Double.Hypot(b.X - a.X, b.Y - a.Y) > 2.0 then
                            let fam =
                                match oct with
                                | 0 | 4 -> 0
                                | 2 | 6 -> 1
                                | 1 | 5 -> 2
                                | _ -> 3
                            let struct(ux, uy, nx, ny) = familyVectors fam
                            let lineC = a.X * nx + a.Y * ny
                            let pa = a.X * ux + a.Y * uy
                            let pb = b.X * ux + b.Y * uy
                            // Compute signed turn bias at entry & exit so diverging metro lines order cleanly without crossing!
                            let pPrev = pts[s - 1]
                            let pNext = pts[s + 2]
                            let turnEntry = (pPrev.X - a.X) * nx + (pPrev.Y - a.Y) * ny
                            let turnExit = (pNext.X - b.X) * nx + (pNext.Y - b.Y) * ny
                            let turnSig = sign turnEntry + sign turnExit
                            let qLine = int64 (Math.Round(lineC * 2.0))
                            let key = (int64 fam <<< 48) ||| (qLine &&& 0xFFFFFFFFFFFFL)
                            let list =
                                match buckets.TryGetValue key with
                                | true, l -> l
                                | _ ->
                                    let l = ResizeArray<struct(int * int * float * float * float)>()
                                    buckets.Add(key, l)
                                    l
                            list.Add(struct(e, s, min pa pb, max pa pb, float turnSig))

            let inline clearAll x0 y0 x1 y1 =
                let mutable ok = true
                let mutable k = 0
                while ok && k < allObstacles.Length do
                    let struct(rx0, ry0, rx1, ry1) = allObstacles[k]
                    if segmentHitsRect x0 y0 x1 y1 rx0 ry0 rx1 ry1 then ok <- false
                    k <- k + 1
                ok

            let nudgedCopy = Array.zeroCreate<bool> m
            for kv in buckets do
                let segs = kv.Value
                if segs.Count > 1 then
                    let fam = int (kv.Key >>> 48)
                    let struct(_, _, nx, ny) = familyVectors fam
                    let active = ResizeArray<struct(int * int * float * float * float)>()
                    for k = 0 to segs.Count - 1 do
                        let item = segs[k]
                        let struct(eCur, _, lo, hi, _) = item
                        let mutable overlaps = (active.Count = 0)
                        let mutable sameEdge = false
                        for ai = 0 to active.Count - 1 do
                            let struct(eAct, _, olo, ohi, _) = active[ai]
                            if eAct = eCur then sameEdge <- true
                            if max lo olo < min hi ohi - 1e-2 then overlaps <- true
                        if overlaps && not sameEdge then
                            active.Add item

                    if active.Count > 1 then
                        let actArr = active.ToArray()
                        Array.sortInPlaceWith (fun a b ->
                            let struct(ea, _, _, _, ta) = a
                            let struct(eb, _, _, _, tb) = b
                            let c = compare ta tb
                            if c <> 0 then c else compare ea eb) actArr

                        for rank = 0 to actArr.Length - 1 do
                            let struct(e, s, _, _, _) = actArr[rank]
                            let delta = (float rank - float (actArr.Length - 1) * 0.5) * spacing
                            if abs delta > 1e-5 then
                                let pts = if nudgedCopy[e] then rawChains[e] else Array.copy rawChains[e]
                                let pPrev = pts[s - 1]
                                let pA = pts[s]
                                let pB = pts[s + 1]
                                let pNext = pts[s + 2]

                                let dPrevX, dPrevY = pA.X - pPrev.X, pA.Y - pPrev.Y
                                let dNextX, dNextY = pNext.X - pB.X, pNext.Y - pB.Y
                                let lenPrev = Double.Hypot(dPrevX, dPrevY)
                                let lenNext = Double.Hypot(dNextX, dNextY)

                                if lenPrev > 1e-4 && lenNext > 1e-4 then
                                    let uPrevX, uPrevY = dPrevX / lenPrev, dPrevY / lenPrev
                                    let uNextX, uNextY = dNextX / lenNext, dNextY / lenNext
                                    let denomPrev = uPrevX * nx + uPrevY * ny
                                    let denomNext = uNextX * nx + uNextY * ny
                                    // Slide endpoints strictly along their adjacent octilinear segment rays!
                                    if abs denomPrev > 0.25 && abs denomNext > 0.25 then
                                        let slideA = delta / denomPrev
                                        let slideB = delta / denomNext
                                        if abs slideA < 0.42 * lenPrev && abs slideB < 0.42 * lenNext then
                                            let newA = Point.create (pA.X + uPrevX * slideA) (pA.Y + uPrevY * slideA)
                                            let newB = Point.create (pB.X + uNextX * slideB) (pB.Y + uNextY * slideB)
                                            if classifyOctant (newB.X - newA.X) (newB.Y - newA.Y) >= 0
                                               && clearAll pA.X pA.Y newA.X newA.Y
                                               && clearAll newA.X newA.Y newB.X newB.Y
                                               && clearAll pB.X pB.Y newB.X newB.Y then
                                                pts[s] <- newA
                                                pts[s + 1] <- newB
                                                rawChains[e] <- pts
                                                nudgedCopy[e] <- true

    /// <summary>Routes all associations over <c>positionedNodes</c> using strictly octilinear 45-degree schematic paths.</summary>
    let internal routeOverNodes
        (token: System.Threading.CancellationToken)
        (config: OctilinearRouterConfig)
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
            let trackSep = max 0. config.TrackSeparation

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
                for p in positionedNodes[ni].Ports do
                    resolvedNamedPorts[ni][p.Id] <- p

                for si = 0 to 3 do
                    let reqs = sideRequests[ni * 4 + si]
                    if reqs.Count > 0 then
                        let side = idxToSide si
                        reqs.Sort(Comparison<struct(int * bool * float * string option)>(fun a b ->
                            let struct(ea, _, ka, pa) = a
                            let struct(eb, _, kb, pb) = b
                            let oa = findPortSpec eff.Nodes[ni] pa |> Option.map (fun sp -> sp.Order)
                            let ob = findPortSpec eff.Nodes[ni] pb |> Option.map (fun sp -> sp.Order)
                            match oa, ob with
                            | Some x, Some y when x <> y -> compare x y
                            | _ ->
                                let c = compare ka kb
                                if c <> 0 then c else compare ea eb))

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
            let ws = createWorkspace ()
            let localObs = ResizeArray<struct(float * float * float * float)>(min n 64)
            let fullBuf = ResizeArray<Point>(24)

            for e = 0 to m - 1 do
                SimdMath.check token e
                let a = eff.Associations[e]
                let ui = eff.ReversedIdMap[a.Source.NodeId]
                let vi = eff.ReversedIdMap[a.Target.NodeId]
                if ui = vi then
                    // Chamfered 45-degree octilinear self-loop around top-right corner
                    let b = updatedNodes[ui].Bounds
                    let x1 = Bounds.x b + Bounds.width b
                    let y0 = Bounds.y b
                    let pStart =
                        a.Source.PortId
                        |> Option.bind (fun pid -> updatedNodes[ui].Ports |> List.tryFind (fun p -> p.Id = pid))
                        |> Option.map (fun p -> p.Anchor)
                        |> Option.defaultValue (Point.create (x1 - min 16. (Bounds.width b * 0.3)) y0)
                    let pEnd =
                        a.Target.PortId
                        |> Option.bind (fun pid -> updatedNodes[ui].Ports |> List.tryFind (fun p -> p.Id = pid))
                        |> Option.map (fun p -> p.Anchor)
                        |> Option.defaultValue (Point.create x1 (y0 + min 16. (Bounds.height b * 0.35)))
                    let outDist = stubLen + float (e % 3) * 6.
                    let topY = y0 - outDist
                    let rightX = x1 + outDist
                    let cham = min 8. (outDist * 0.45)
                    fullBuf.Clear()
                    fullBuf.Add pStart
                    fullBuf.Add(Point.create pStart.X (topY + cham))
                    fullBuf.Add(Point.create (pStart.X + cham) topY)
                    fullBuf.Add(Point.create (rightX - cham) topY)
                    fullBuf.Add(Point.create rightX (topY + cham))
                    fullBuf.Add(Point.create rightX (pEnd.Y - cham))
                    fullBuf.Add(Point.create (rightX - cham) pEnd.Y)
                    fullBuf.Add pEnd
                    rawChains[e] <- simplifyOctilinear fullBuf
                else
                    let pStart, pEnd = srcAnchors[e], dstAnchors[e]
                    let nxS, nyS = sideNormal srcSides[e]
                    let nxT, nyT = sideNormal dstSides[e]
                    let stubA = Point.create (pStart.X + nxS * stubLen) (pStart.Y + nyS * stubLen)
                    let stubB = Point.create (pEnd.X + nxT * stubLen) (pEnd.Y + nyT * stubLen)

                    let cMinX = min stubA.X stubB.X - 72.
                    let cMaxX = max stubA.X stubB.X + 72.
                    let cMinY = min stubA.Y stubB.Y - 72.
                    let cMaxY = max stubA.Y stubB.Y + 72.

                    localObs.Clear()
                    for ni = 0 to n - 1 do
                        let struct(ox0, oy0, ox1, oy1) = allObstacles[ni]
                        if ox1 >= cMinX && ox0 <= cMaxX && oy1 >= cMinY && oy0 <= cMaxY then
                            if ni = ui || ni = vi then
                                let b = updatedNodes[ni].Bounds
                                localObs.Add(struct(Bounds.x b - 2., Bounds.y b - 2., Bounds.x b + Bounds.width b + 2., Bounds.y b + Bounds.height b + 2.))
                            else
                                localObs.Add allObstacles[ni]

                    let dirA = sideOutOctant srcSides[e]
                    // Incoming direction from stubB into pEnd is opposite of dstSides[e]'s outward normal!
                    let inDirB = (sideOutOctant dstSides[e] + 4) % 8
                    let midRoute = routeStubToStub ws stubA dirA stubB inDirB config localObs

                    fullBuf.Clear()
                    fullBuf.Add pStart
                    for k = 0 to midRoute.Length - 1 do fullBuf.Add midRoute[k]
                    fullBuf.Add pEnd
                    rawChains[e] <- simplifyOctilinear fullBuf

            // Parallel metro-trunk track ordering & angle-preserving normal nudging
            let tightBoxes =
                updatedNodes
                |> Array.map (fun pn ->
                    let b = pn.Bounds
                    struct(Bounds.x b - 1., Bounds.y b - 1., Bounds.x b + Bounds.width b + 1., Bounds.y b + Bounds.height b + 1.))
            nudgeParallelMetroTrunks trackSep rawChains tightBoxes

            let mutable routed = []
            for e = m - 1 downto 0 do
                let a = eff.Associations[e]
                fullBuf.Clear()
                let rc = rawChains[e]
                for k = 0 to rc.Length - 1 do fullBuf.Add rc[k]
                let pts = simplifyOctilinear fullBuf
                let path = buildPathWithFillets config.CornerFilletRadius pts
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

    /// <summary>Re-routes all associations of an existing <c>DiagramLayout2D</c> with obstacle-avoiding 45-degree octilinear metro paths and cooperative cancellation.</summary>
    let routeWithCancellation
        (token: System.Threading.CancellationToken)
        (config: OctilinearRouterConfig)
        (graph: DiagramGraph)
        (layout: DiagramLayout2D)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.ObstacleMargin && Double.IsFinite config.PortStubLength
                && Double.IsFinite config.GridSpacing && Double.IsFinite config.Bend45Penalty
                && Double.IsFinite config.Bend90Penalty && Double.IsFinite config.TrackSeparation
                && Double.IsFinite config.CornerFilletRadius)
           || config.ObstacleMargin < 0. || config.PortStubLength <= 0.
           || config.GridSpacing <= 0. || config.Bend45Penalty < 0. || config.Bend90Penalty < 0.
           || config.TrackSeparation < 0. || config.CornerFilletRadius < 0. then
            Error [ InvalidObservation "OctilinearRouter requires positive PortStubLength/GridSpacing and finite nonnegative margins, penalties, separation and fillet radius." ]
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

    /// <summary>Re-routes all associations of an existing <c>DiagramLayout2D</c> with 45-degree octilinear metro paths without cancellation.</summary>
    let route config graph layout =
        routeWithCancellation System.Threading.CancellationToken.None config graph layout

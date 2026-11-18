namespace Ranvier.D5

open System

/// <summary>An opaque seed for the deterministic per-ID initial positions and jitter of a force simulation.</summary>
type Seed = private Seed of uint32
/// <summary>Constructors for <c>Seed</c>.</summary>
[<RequireQualifiedAccess>]
module Seed =
    /// <summary>Wraps a 32-bit seed value.</summary>
    let create value = Seed value

/// <summary>Force simulation settings: cooling and velocity decay in [0,1], centering, optional axis, radial, many-body and collision forces,
/// initial placement radius and a positive step budget. <c>Force.initialize</c> reports invalid values as <c>InvalidObservation</c> errors.</summary>
type ForceConfig = {
    AlphaDecay:float; AlphaMin:float; VelocityDecay:float; InitialAlpha:float
    Center:Point; CenterStrength:float; X: (float*float) option; Y: (float*float) option
    Radial:(Point*float*float) option; InitialRadius:float; MaxSteps:int
    ManyBody:ManyBodyConfig option; Collision:CollisionConfig option
}
/// <summary>Constructors for <c>ForceConfig</c>.</summary>
[<RequireQualifiedAccess>]
module ForceConfig =
    /// <summary>Creates d3-like defaults around a center point: alpha decay 0.0228, minimum alpha 0.001, velocity decay 0.4, center strength 0.05,
    /// initial radius 100, 1000 steps and no optional forces.</summary>
    let create center = {AlphaDecay=0.0228;AlphaMin=0.001;VelocityDecay=0.4;InitialAlpha=1.;Center=center;CenterStrength=0.05;X=None;Y=None;Radial=None;InitialRadius=100.;MaxSteps=1000;ManyBody=None;Collision=None}

/// <summary>A simulated node's position and per-tick velocity in scene units, with its pin, radius and mass.</summary>
type ForceNodeState = { Id:string; Position:Point; Velocity:Point; Pinned:Point option; Radius:float; Mass:float }
/// <summary>An immutable view of a simulation: nodes in ordinal ID order, current alpha, steps taken in the current budget,
/// and completion flags.</summary>
type ForceSnapshot = { Nodes:ForceNodeState list; Alpha:float; Iteration:int; Converged:bool; Finished:bool }
[<Struct>]
type private ForceEdge =
    { A: int
      B: int
      Target: float
      K: float
      ShareA: float
      ShareB: float }

/// <summary>Opaque, immutable force-simulation state; every operation returns a new value.</summary>
type ForceState = private {
    Config:ForceConfig; Seed:Seed; Graph:GraphData; Values:ForceNodeState[]
    Edges:ForceEdge[]; Alpha:float; Iteration:int
}

/// <summary>A deterministic, immutable force-directed simulation over <c>GraphData</c>.</summary>
[<RequireQualifiedAccess>]
module Force =
    let private zero=Point.create 0. 0.
    let private hash (Seed seed) (id:string) =
        let mutable value=2166136261u ^^^ seed
        for ch in id do value<-(value ^^^ uint32 ch)*16777619u
        value
    let private generator seed id =
        Ranvier.Anime.Utils.createSeededRandom (Some(float(hash seed id))) (Some 0.) (Some 1.) (Some 12)
    let private initialPosition config seed id =
        let random=generator seed id
        let angle=random None None None*2.*System.Math.PI
        let radius=sqrt(random None None None)*config.InitialRadius
        Point.create (config.Center.X+cos angle*radius) (config.Center.Y+sin angle*radius)

    let private validate config =
        let errors=ResizeArray<LayoutError>()
        let finite name value = if not(Double.IsFinite value) then errors.Add(InvalidObservation(name+" must be finite."))
        let range name lo hi value = finite name value;if value<lo || value>hi then errors.Add(InvalidObservation(name+" is out of range."))
        let strength name value = finite name value;if value<0. then errors.Add(InvalidObservation(name+" must be nonnegative."))
        range "Alpha decay" 0. 1. config.AlphaDecay
        range "Velocity decay" 0. 1. config.VelocityDecay
        range "Initial alpha" 0. 1. config.InitialAlpha
        range "Alpha minimum" System.Double.Epsilon 1. config.AlphaMin
        strength "Initial radius" config.InitialRadius
        strength "Center strength" config.CenterStrength
        finite "Center X" config.Center.X;finite "Center Y" config.Center.Y
        for axis in [config.X;config.Y] do axis |> Option.iter(fun (target,k) -> finite "Axis target" target;strength "Axis strength" k)
        config.Radial |> Option.iter(fun (center,radius,k) -> finite "Radial X" center.X;finite "Radial Y" center.Y;strength "Radial radius" radius;strength "Radial strength" k)
        config.ManyBody |> Option.iter(fun body ->
            finite "Many-body strength" body.Strength
            strength "Theta" body.Theta
            range "Minimum distance" System.Double.Epsilon System.Double.MaxValue body.MinDistance
            if System.Double.IsNaN body.MaxDistance || body.MaxDistance<body.MinDistance then errors.Add(InvalidObservation "Maximum distance must be at least the minimum distance."))
        config.Collision |> Option.iter(fun collision ->
            range "Collision strength" 0. 1. collision.Strength
            if collision.Iterations<=0 then errors.Add(InvalidObservation "Collision iterations must be positive."))
        if config.MaxSteps<=0 then errors.Add(InvalidObservation "Step budget must be positive.")
        List.ofSeq errors

    /// <summary>Initializes deterministic per-ID positions using the toolkit's seeded generator. All state storage is private and owned by this value.</summary>
    let initialize config seed graph =
        match validate config with
        | _::_ as errors -> Error errors
        | [] ->
            try
                let values=GraphData.nodes graph |> List.map(fun node ->
                    let position=node.Pinned |> Option.orElse node.Position |> Option.defaultWith(fun () -> initialPosition config seed node.Id)
                    {Id=node.Id;Position=position;Velocity=zero;Pinned=node.Pinned;Radius=node.Radius;Mass=node.Mass}) |> List.toArray
                let indices=System.Collections.Generic.Dictionary<string,int>(values.Length, StringComparer.Ordinal)
                values |> Array.iteri(fun i node -> indices.Add(node.Id,i))
                let edgeBuf = ResizeArray<ForceEdge>()
                for edge in GraphData.links graph do
                    let a = indices[edge.Source]
                    let b = indices[edge.Target]
                    if a <> b && edge.Strength > 0. then
                        let mA = values[a].Mass
                        let mB = values[b].Mass
                        let scale = max mA mB
                        let ma, mb = mA / scale, mB / scale
                        let shareA = mb / (ma + mb)
                        let shareB = ma / (ma + mb)
                        edgeBuf.Add { A = a; B = b; Target = edge.Distance; K = edge.Strength; ShareA = shareA; ShareB = shareB }
                Ok {Config=config;Seed=seed;Graph=graph;Values=values;Edges=edgeBuf.ToArray();Alpha=(if values.Length=0 then 0. else config.InitialAlpha);Iteration=0}
            with :? System.ArgumentException as error -> Error [InvalidObservation error.Message]

    /// <summary>Returns true when the graph is empty, alpha has cooled below <c>AlphaMin</c>, or the step budget is exhausted.</summary>
    let isFinished state = state.Values.Length=0 || state.Alpha<state.Config.AlphaMin || state.Iteration>=state.Config.MaxSteps
    /// <summary>Copies the node sequence into an immutable public snapshot. Finished includes budget exhaustion; Converged means cooling reached the threshold.</summary>
    let snapshot state =
        {Nodes=Array.toList state.Values;Alpha=state.Alpha;Iteration=state.Iteration
         Converged=state.Values.Length=0 || state.Alpha<state.Config.AlphaMin;Finished=isFinished state}

    let private direction state iteration id =
        let random=generator state.Seed (id+"/"+string iteration)
        let angle=random None None None*2.*System.Math.PI
        cos angle,sin angle

    /// <summary>Advances unit-time steps on copied state. Cancellation is checked every 64 nodes/edges and before each tick; cancellation never changes the input.</summary>
    let stepWithCancellation (token:System.Threading.CancellationToken) count state =
        if count<0 then invalidArg "count" "Step count must be nonnegative."
        token.ThrowIfCancellationRequested()
        if count=0 || isFinished state then state else
        let srcNodes = state.Values
        let n = srcNodes.Length
        let px = Array.zeroCreate<float> n
        let py = Array.zeroCreate<float> n
        let velX = Array.zeroCreate<float> n
        let velY = Array.zeroCreate<float> n
        let vx = Array.zeroCreate<float> n
        let vy = Array.zeroCreate<float> n
        let isPinned = Array.zeroCreate<bool> n
        let mutable maxCoordAbs = 0.
        for i = 0 to n - 1 do
            let nd = srcNodes[i]
            match nd.Pinned with
            | Some pin ->
                isPinned[i] <- true
                px[i] <- pin.X
                py[i] <- pin.Y
                velX[i] <- 0.
                velY[i] <- 0.
            | None ->
                px[i] <- nd.Position.X
                py[i] <- nd.Position.Y
                velX[i] <- nd.Velocity.X
                velY[i] <- nd.Velocity.Y
            let m = max (abs px[i]) (abs py[i])
            if m > maxCoordAbs then maxCoordAbs <- m
        let config = state.Config
        let xForce = config.X |> Option.filter(fun (_,k) -> k>0.)
        let yForce = config.Y |> Option.filter(fun (_,k) -> k>0.)
        let radialForce = config.Radial |> Option.filter(fun (_,_,k) -> k>0.)
        let body = config.ManyBody |> Option.filter(fun settings -> settings.Strength<>0.)
        let collision = config.Collision |> Option.filter(fun settings -> settings.Strength>0.)
        let spatial = body.IsSome || collision.IsSome
        let needTree = (match body with Some b when b.Theta > 0. -> true | _ -> false) || collision.IsSome
        let positions = if needTree then Array.zeroCreate<Point> n else [||]
        let nodeIds = if needTree then Array.init n id else [||]
        let masses = if spatial then Array.init n (fun i -> srcNodes[i].Mass) else [||]
        let radii = if collision.IsSome then Array.init n (fun i -> srcNodes[i].Radius) else [||]
        let edges = state.Edges
        let alphaKeep = 1. - config.AlphaDecay
        let velKeep = 1. - config.VelocityDecay
        let hasX = xForce.IsSome
        let xTarget, xK = match xForce with Some(t, k) -> t, k | None -> 0., 0.
        let hasY = yForce.IsSome
        let yTarget, yK = match yForce with Some(t, k) -> t, k | None -> 0., 0.
        let hasRadial = radialForce.IsSome
        let rCenter, rRadius, rK = match radialForce with Some(c, r, k) -> c, r, k | None -> zero, 0., 0.
        let mutable alpha, iteration, remaining = state.Alpha, state.Iteration, count
        let inline check i = if (i &&& 63) = 0 then token.ThrowIfCancellationRequested()
        while remaining > 0 && alpha >= config.AlphaMin && iteration < config.MaxSteps do
            token.ThrowIfCancellationRequested()
            alpha <- alpha * alphaKeep
            let centerX, centerY =
                if config.CenterStrength = 0. then 0., 0.
                elif maxCoordAbs < 1e200 then
#if !FABLE_COMPILER
                    let meanX = SimdNumeric.sumSpan (ReadOnlySpan<float>(px, 0, n)) / float n
                    let meanY = SimdNumeric.sumSpan (ReadOnlySpan<float>(py, 0, n)) / float n
#else
                    let mutable sumX = 0.
                    let mutable sumY = 0.
                    for i = 0 to n - 1 do
                        check i
                        sumX <- sumX + px[i]
                        sumY <- sumY + py[i]
                    let meanX = sumX / float n
                    let meanY = sumY / float n
#endif
                    (config.Center.X - meanX) * config.CenterStrength * alpha,
                    (config.Center.Y - meanY) * config.CenterStrength * alpha
                else
                    let mutable meanX, meanY = 0., 0.
                    for i = 0 to n - 1 do
                        check i
                        let weight = 1. / float(i + 1)
                        meanX <- meanX * (1. - weight) + px[i] * weight
                        meanY <- meanY * (1. - weight) + py[i] * weight
                    (config.Center.X - meanX) * config.CenterStrength * alpha,
                    (config.Center.Y - meanY) * config.CenterStrength * alpha
            let xKAlpha = xK * alpha
            let yKAlpha = yK * alpha
            let rKAlpha = rK * alpha
            for i = 0 to n - 1 do
                check i
                let mutable vxi = velX[i] + centerX
                let mutable vyi = velY[i] + centerY
                let pxi = px[i]
                let pyi = py[i]
                if hasX then vxi <- vxi + (xTarget - pxi) * xKAlpha
                if hasY then vyi <- vyi + (yTarget - pyi) * yKAlpha
                if hasRadial then
                    let dx, dy = pxi - rCenter.X, pyi - rCenter.Y
                    let distance = Double.Hypot(dx, dy)
                    Check.finite "radial distance" distance
                    let ux, uy = if distance = 0. then direction state iteration srcNodes[i].Id else dx / distance, dy / distance
                    let force = (rRadius - distance) * rKAlpha
                    vxi <- vxi + ux * force
                    vyi <- vyi + uy * force
                vx[i] <- vxi
                vy[i] <- vyi
            let fastLinks = maxCoordAbs < 1e140
            for edgeIndex = 0 to edges.Length - 1 do
                check edgeIndex
                let e = edges[edgeIndex]
                let a = e.A
                let b = e.B
                let dx = px[b] - px[a]
                let dy = py[b] - py[a]
                let d2 = dx * dx + dy * dy
                let distance =
                    if fastLinks && d2 > 1e-280 then sqrt d2
                    else
                        let h = Double.Hypot(dx, dy)
                        Check.finite "link distance" h
                        h
                let ux, uy =
                    if distance = 0. then direction state iteration (srcNodes[a].Id + "/" + srcNodes[b].Id)
                    else dx / distance, dy / distance
                let force = (distance - e.Target) * e.K * alpha
                let fa = force * e.ShareA
                let fb = force * e.ShareB
                vx[a] <- vx[a] + ux * fa
                vy[a] <- vy[a] + uy * fa
                vx[b] <- vx[b] - ux * fb
                vy[b] <- vy[b] - uy * fb
            if spatial then
                let jitter i j =
                    let a, b = min i j, max i j
                    let x, y = direction state iteration (srcNodes[a].Id + "/" + srcNodes[b].Id)
                    if i <= j then x, y else -x, -y
                body |> Option.iter(fun settings ->
                    ForceKernels.manyBody token settings alpha px py positions nodeIds masses maxCoordAbs jitter vx vy)
                collision |> Option.iter(fun settings ->
                    ForceKernels.collision token settings px py positions nodeIds masses radii isPinned maxCoordAbs jitter vx vy)
            let mutable nextMaxAbs = 0.
            for i = 0 to n - 1 do
                check i
                if isPinned[i] then
                    let m = max (abs px[i]) (abs py[i])
                    if m > nextMaxAbs then nextMaxAbs <- m
                else
                    let nvx = vx[i] * velKeep
                    let nvy = vy[i] * velKeep
                    Check.finite "x" nvx
                    Check.finite "y" nvy
                    let npx = px[i] + nvx
                    let npy = py[i] + nvy
                    Check.finite "x" npx
                    Check.finite "y" npy
                    velX[i] <- nvx
                    velY[i] <- nvy
                    px[i] <- npx
                    py[i] <- npy
                    let m = max (abs npx) (abs npy)
                    if m > nextMaxAbs then nextMaxAbs <- m
            maxCoordAbs <- nextMaxAbs
            iteration <- iteration + 1
            remaining <- remaining - 1
        token.ThrowIfCancellationRequested()
        let outNodes = Array.zeroCreate<ForceNodeState> n
        for i = 0 to n - 1 do
            let nd = srcNodes[i]
            match nd.Pinned with
            | Some position -> outNodes[i] <- { nd with Position = position; Velocity = zero }
            | None -> outNodes[i] <- { nd with Position = { Point.X = px[i]; Y = py[i] }; Velocity = { Point.X = velX[i]; Y = velY[i] } }
        { state with Values = outNodes; Alpha = alpha; Iteration = iteration }

    /// <summary>Advances up to <c>count</c> ticks without cancellation, stopping early once finished. Throws <c>ArgumentException</c> for a negative count.</summary>
    let step count state = stepWithCancellation System.Threading.CancellationToken.None count state

    /// <summary>Starts a fresh iteration budget and raises alpha to at least the requested value.</summary>
    let reheat alpha (state:ForceState) =
        Check.finite "alpha" alpha
        if alpha<0. || alpha>1. then invalidArg "alpha" "Alpha must be between zero and one."
        {state with Alpha=(if state.Values.Length=0 then 0. else max state.Alpha alpha);Iteration=0}

    /// <summary>Replaces topology, retaining surviving positions and velocities. New graph pins are authoritative; new IDs use deterministic initialization.</summary>
    let updateGraph graph state =
        initialize state.Config state.Seed graph |> Result.map(fun fresh ->
            let previous=System.Collections.Generic.Dictionary<string,ForceNodeState>(StringComparer.Ordinal)
            for node in state.Values do previous.Add(node.Id,node)
            let values=fresh.Values |> Array.map(fun node ->
                match previous.TryGetValue node.Id,node.Pinned with
                | (true,old),None -> {node with Position=old.Position;Velocity=old.Velocity}
                | _ -> node)
            {fresh with Values=values;Alpha=(if values.Length=0 then 0. else max fresh.Alpha state.Alpha)})

    /// <summary>Pins or unpins a stable ID and reheats. A new pin takes effect immediately with zero velocity.</summary>
    let pin id (position:Point option) (state:ForceState) =
        match state.Values |> Array.tryFindIndex(fun node -> node.Id=id) with
        | None -> Error [MissingEndpoint id]
        | Some index ->
            try
                position |> Option.iter(fun p -> Point.create p.X p.Y |> ignore)
                let values=Array.copy state.Values
                let node=values[index]
                values[index]<-{node with Pinned=position;Position=Option.defaultValue node.Position position;Velocity=zero}
                Ok(reheat state.Config.InitialAlpha {state with Values=values})
            with :? System.ArgumentException as error -> Error [InvalidObservation error.Message]

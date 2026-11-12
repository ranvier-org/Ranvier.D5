namespace Ranvier.D5

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
/// <summary>Opaque, immutable force-simulation state; every operation returns a new value.</summary>
type ForceState = private {
    Config:ForceConfig; Seed:Seed; Graph:GraphData; Values:ForceNodeState[]
    Edges:struct(int*int*float*float)[]; Alpha:float; Iteration:int
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
        let finite name value = if not(System.Double.IsFinite value) then errors.Add(InvalidObservation(name+" must be finite."))
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
                let indices=System.Collections.Generic.Dictionary<string,int>(System.StringComparer.Ordinal)
                values |> Array.iteri(fun i node -> indices.Add(node.Id,i))
                let edges=GraphData.links graph |> List.map(fun edge -> struct(indices[edge.Source],indices[edge.Target],edge.Distance,edge.Strength)) |> List.toArray
                Ok {Config=config;Seed=seed;Graph=graph;Values=values;Edges=edges;Alpha=(if values.Length=0 then 0. else config.InitialAlpha);Iteration=0}
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
        let nodes=Array.copy state.Values
        let vx,vy=Array.zeroCreate nodes.Length,Array.zeroCreate nodes.Length
        let config=state.Config
        let xForce=config.X |> Option.filter(fun (_,k) -> k>0.)
        let yForce=config.Y |> Option.filter(fun (_,k) -> k>0.)
        let radialForce=config.Radial |> Option.filter(fun (_,_,k) -> k>0.)
        let body=config.ManyBody |> Option.filter(fun settings -> settings.Strength<>0.)
        let collision=config.Collision |> Option.filter(fun settings -> settings.Strength>0.)
        let spatial=body.IsSome || collision.IsSome
        let positions=if spatial then Array.zeroCreate nodes.Length else [||]
        let masses=if spatial then nodes |> Array.map(fun node -> node.Mass) else [||]
        let radii=if collision.IsSome then nodes |> Array.map(fun node -> node.Radius) else [||]
        let pinned=if collision.IsSome then nodes |> Array.map(fun node -> node.Pinned.IsSome) else [||]
        let mutable alpha,iteration,remaining=state.Alpha,state.Iteration,count
        let check i=if i%64=0 then token.ThrowIfCancellationRequested()
        while remaining>0 && alpha>=config.AlphaMin && iteration<config.MaxSteps do
            token.ThrowIfCancellationRequested()
            alpha<-alpha*(1.-config.AlphaDecay)
            let mutable meanX,meanY=0.,0.
            for i in 0..nodes.Length-1 do
                check i
                let weight=1./float(i+1)
                meanX<-meanX*(1.-weight)+nodes[i].Position.X*weight
                meanY<-meanY*(1.-weight)+nodes[i].Position.Y*weight
            let centerX,centerY=if config.CenterStrength=0. then 0.,0. else (config.Center.X-meanX)*config.CenterStrength*alpha,(config.Center.Y-meanY)*config.CenterStrength*alpha
            for i in 0..nodes.Length-1 do
                check i
                let node=nodes[i]
                vx[i]<-node.Velocity.X+centerX;vy[i]<-node.Velocity.Y+centerY
                xForce |> Option.iter(fun (target,k) -> vx[i]<-vx[i]+(target-node.Position.X)*k*alpha)
                yForce |> Option.iter(fun (target,k) -> vy[i]<-vy[i]+(target-node.Position.Y)*k*alpha)
                radialForce |> Option.iter(fun (center,radius,k) ->
                    let dx,dy=node.Position.X-center.X,node.Position.Y-center.Y
                    let distance=System.Double.Hypot(dx,dy)
                    Check.finite "radial distance" distance
                    let ux,uy=if distance=0. then direction state iteration node.Id else dx/distance,dy/distance
                    let force=(radius-distance)*k*alpha
                    vx[i]<-vx[i]+ux*force;vy[i]<-vy[i]+uy*force)
            for edgeIndex in 0..state.Edges.Length-1 do
                check edgeIndex
                let struct(a,b,target,k)=state.Edges[edgeIndex]
                if a<>b && k>0. then
                    let dx,dy=nodes[b].Position.X-nodes[a].Position.X,nodes[b].Position.Y-nodes[a].Position.Y
                    let distance=System.Double.Hypot(dx,dy)
                    Check.finite "link distance" distance
                    let ux,uy=if distance=0. then direction state iteration (nodes[a].Id+"/"+nodes[b].Id) else dx/distance,dy/distance
                    let force=(distance-target)*k*alpha
                    let scale=max nodes[a].Mass nodes[b].Mass
                    let ma,mb=nodes[a].Mass/scale,nodes[b].Mass/scale
                    let shareA,shareB=mb/(ma+mb),ma/(ma+mb)
                    vx[a]<-vx[a]+ux*force*shareA;vy[a]<-vy[a]+uy*force*shareA
                    vx[b]<-vx[b]-ux*force*shareB;vy[b]<-vy[b]-uy*force*shareB
            if spatial then
                for i in 0..nodes.Length-1 do
                    check i
                    positions[i]<-nodes[i].Position
                let jitter i j =
                    let a,b=min i j,max i j
                    let x,y=direction state iteration (nodes[a].Id+"/"+nodes[b].Id)
                    if i<=j then x,y else -x,-y
                body |> Option.iter(fun settings -> ForceKernels.manyBody token settings alpha positions masses jitter vx vy)
                collision |> Option.iter(fun settings -> ForceKernels.collision token settings positions masses radii pinned jitter vx vy)
            for i in 0..nodes.Length-1 do
                check i
                let node=nodes[i]
                match node.Pinned with
                | Some position -> nodes[i]<-{node with Position=position;Velocity=zero}
                | None ->
                    let velocity=Point.create (vx[i]*(1.-config.VelocityDecay)) (vy[i]*(1.-config.VelocityDecay))
                    let position=Point.create (node.Position.X+velocity.X) (node.Position.Y+velocity.Y)
                    nodes[i]<-{node with Position=position;Velocity=velocity}
            iteration<-iteration+1;remaining<-remaining-1
        token.ThrowIfCancellationRequested()
        {state with Values=nodes;Alpha=alpha;Iteration=iteration}

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
            let previous=System.Collections.Generic.Dictionary<string,ForceNodeState>(System.StringComparer.Ordinal)
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

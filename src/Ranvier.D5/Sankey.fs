namespace Ranvier.D5

/// <summary>A directed link between two node IDs; Value must be finite and nonnegative.</summary>
type FlowLink = { Id:string; Source:string; Target:string; Value:float }
/// <summary>A validated flow graph with unique node and link IDs, created by FlowGraph.create.</summary>
type FlowGraph = private { NodeIds:string list; Edges:FlowLink list }

/// <summary>Validation of and access to flow graphs.</summary>
[<RequireQualifiedAccess>]
module FlowGraph =
    /// <summary>Validates nodes and links and returns every error found: empty or duplicate IDs, missing endpoints and negative or non-finite values.
    /// Cycles are not checked here.</summary>
    let create (nodes:string list) (links:FlowLink list) : Result<FlowGraph,LayoutError list> =
        let errors=ResizeArray<LayoutError>()
        let nodeIds=System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
        let edgeIds=System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
        let identity (seen:System.Collections.Generic.HashSet<string>) id =
            if System.String.IsNullOrWhiteSpace id then errors.Add(InvalidObservation "Flow ID must not be empty.")
            elif not(seen.Add id) then errors.Add(DuplicateId id)
        for id in nodes do identity nodeIds id
        for edge in links do
            identity edgeIds edge.Id
            if not(nodeIds.Contains edge.Source) then errors.Add(MissingEndpoint edge.Source)
            if not(nodeIds.Contains edge.Target) then errors.Add(MissingEndpoint edge.Target)
            if not(System.Double.IsFinite edge.Value) || edge.Value<0. then errors.Add(InvalidObservation "Flow values must be finite and nonnegative.")
        if errors.Count>0 then Error(List.ofSeq errors) else
        Ok {NodeIds=nodes |> List.sortWith(fun a b -> System.StringComparer.Ordinal.Compare(a,b))
            Edges=links |> List.sortWith(fun a b -> System.StringComparer.Ordinal.Compare(a.Id,b.Id))}
    /// <summary>Returns node IDs in ordinal order.</summary>
    let nodes graph = graph.NodeIds
    /// <summary>Returns links in ordinal ID order.</summary>
    let links graph = graph.Edges

/// <summary>Column assignment for unstaged layouts. Left uses depth from sources; Right uses distance to sinks; Center moves pure sources next to
/// their nearest target; Justify moves sinks to the last column.</summary>
[<RequireQualifiedAccess>]
type SankeyAlignment = Left | Right | Center | Justify

/// <summary>Sankey settings: node width and vertical padding in layout units, 0 to 256 relaxation iterations, unstaged alignment, and node and link
/// budgets.</summary>
type SankeyConfig =
    { Bounds:Bounds; NodeWidth:float; NodePadding:float; Iterations:int
      Alignment:SankeyAlignment; MaxNodes:int; MaxLinks:int }

/// <summary>Default Sankey configuration.</summary>
[<RequireQualifiedAccess>]
module SankeyConfig =
    /// <summary>Creates a configuration with 24-unit nodes, 8-unit padding, 6 iterations, Justify alignment and budgets of 100,000 nodes and
    /// 1,000,000 links.</summary>
    let create bounds =
        {Bounds=bounds;NodeWidth=24.;NodePadding=8.;Iterations=6
         Alignment=SankeyAlignment.Justify;MaxNodes=100000;MaxLinks=1000000}

/// <summary>A positioned node with its column Layer, total Inflow and Outflow, Value as the larger of the two, and Imbalance as outflow minus
/// inflow.</summary>
type SankeyNode =
    { Id:string; Layer:int; Bounds:Bounds; Inflow:float; Outflow:float; Value:float; Imbalance:float }
/// <summary>A positioned link band of the given Width; Start lies on the source node's right edge and Finish on the target node's left
/// edge.</summary>
type SankeyLink =
    { Id:string; Source:string; Target:string; Value:float; Width:float; Start:Point; Finish:Point }
/// <summary>Positioned nodes and links, each in ordinal ID order.</summary>
type SankeyLayout = { Nodes:SankeyNode list; Links:SankeyLink list }

/// <summary>Sankey layout of acyclic flow graphs within configured bounds.</summary>
[<RequireQualifiedAccess>]
module Sankey =
    let private layoutCore (token:System.Threading.CancellationToken) (stages:Map<string,int> option) (config:SankeyConfig) (graph:FlowGraph) : Result<SankeyLayout,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let width,height=Bounds.width config.Bounds,Bounds.height config.Bounds
        let n,m=graph.NodeIds.Length,graph.Edges.Length
        let finiteNonnegative value=System.Double.IsFinite value && value>=0.
        if config.MaxNodes<=0 || config.MaxLinks<=0 || config.Iterations<0 || config.Iterations>256
           || not(finiteNonnegative config.NodeWidth && finiteNonnegative config.NodePadding) then
            Error [InvalidObservation "Require finite nonnegative sizes, 0..256 iterations and positive budgets."]
        elif n>config.MaxNodes || m>config.MaxLinks then Error [BudgetExceeded "Sankey graph exceeds configured budget."]
        elif config.NodeWidth>width then Error [InvalidObservation "Node width exceeds viewport."]
        elif stages |> Option.exists(fun stages ->
            stages.Count<>n || graph.NodeIds |> List.exists(fun id -> not(stages.ContainsKey id))
            || stages |> Map.exists(fun _ stage -> stage<0 || stage>=config.MaxNodes)) then
            Error [InvalidTopology "Stages must cover exactly the graph nodes and fit the node budget."]
        elif stages |> Option.exists(fun stages ->
            graph.Edges |> List.exists(fun edge -> stages[edge.Source]>=stages[edge.Target])) then
            Error [InvalidTopology "Every staged flow must advance to a later stage."]
        elif n=0 then Ok {Nodes=[];Links=[]}
        else
            let check index=if index%64=0 then token.ThrowIfCancellationRequested()
            let nodes=List.toArray graph.NodeIds
            let edges=List.toArray graph.Edges
            let index=System.Collections.Generic.Dictionary<string,int>(System.StringComparer.Ordinal)
            for i in 0..n-1 do check i;index.Add(nodes[i],i)
            let outgoing=Array.init n (fun _ -> ResizeArray<int>())
            let incoming=Array.init n (fun _ -> ResizeArray<int>())
            let sources,targets=Array.zeroCreate<int> m,Array.zeroCreate<int> m
            let inflow,outflow=Array.zeroCreate<float> n,Array.zeroCreate<float> n
            for e in 0..m-1 do
                check e
                let s,t=index[edges[e].Source],index[edges[e].Target]
                sources[e]<-s;targets[e]<-t
                outgoing[s].Add e;incoming[t].Add e
                outflow[s]<-outflow[s]+edges[e].Value
                inflow[t]<-inflow[t]+edges[e].Value
            if Array.exists (System.Double.IsFinite >> not) inflow || Array.exists (System.Double.IsFinite >> not) outflow then
                Error [InvalidObservation "A node flow total exceeds the finite numeric range."]
            else
                let remaining=incoming |> Array.map(fun links -> links.Count)
                let depth,reverseDepth=Array.zeroCreate<int> n,Array.zeroCreate<int> n
                let order=ResizeArray<int>(n)
                let queue=System.Collections.Generic.Queue<int>()
                for i in 0..n-1 do if remaining[i]=0 then queue.Enqueue i
                while queue.Count>0 do
                    check order.Count
                    let i=queue.Dequeue()
                    order.Add i
                    for e in outgoing[i] do
                        check e
                        let t=targets[e]
                        depth[t]<-max depth[t] (depth[i]+1)
                        remaining[t]<-remaining[t]-1
                        if remaining[t]=0 then queue.Enqueue t
                if order.Count<>n then Error [InvalidTopology "Sankey requires an acyclic graph, including zero-valued edges."]
                else
                    for p in n-1 .. -1 .. 0 do
                        check p
                        let i=order[p]
                        for e in outgoing[i] do
                            check e
                            reverseDepth[i]<-max reverseDepth[i] (reverseDepth[targets[e]]+1)
                    let count=1+(match stages with Some stages -> stages |> Map.values |> Seq.max | None -> Array.max depth)
                    if config.NodeWidth*float count>width then Error [InvalidObservation "Node columns cannot fit the viewport width."]
                    else
                        let layer=Array.init n (fun i ->
                            match stages with
                            | Some stages -> stages[nodes[i]]
                            | None ->
                             match config.Alignment with
                             | SankeyAlignment.Left -> depth[i]
                             | SankeyAlignment.Right -> count-1-reverseDepth[i]
                             | SankeyAlignment.Justify -> if outgoing[i].Count=0 then count-1 else depth[i]
                             | SankeyAlignment.Center ->
                                 if incoming[i].Count>0 then depth[i]
                                 elif outgoing[i].Count>0 then outgoing[i] |> Seq.map(fun e -> depth[targets[e]]-1) |> Seq.min
                                 else 0)
                        let columns=Array.init count (fun _ -> ResizeArray<int>())
                        for i in 0..n-1 do check i;columns[layer[i]].Add i
                        let paddingFits=columns |> Array.forall(fun column ->
                            let needed=config.NodePadding*float(max 0 (column.Count-1))
                            needed<height || (needed=height && column |> Seq.forall(fun i -> inflow[i]=0. && outflow[i]=0.)))
                        if not paddingFits then Error [InvalidObservation "Viewport must fit requested padding and leave height for positive flows."]
                        else
                            let values=Array.init n (fun i -> max inflow[i] outflow[i])
                            let largest=Array.max values
                            let normalized=values |> Array.map(fun value -> if largest=0. then 0. else value/largest)
                            let mutable scale=height
                            for column in columns do
                                token.ThrowIfCancellationRequested()
                                let total=column |> Seq.sumBy(fun i -> normalized[i])
                                if total>0. then scale<-min scale ((height-config.NodePadding*float(column.Count-1))/total)
                            if largest=0. then scale<-0.
                            let sizes=normalized |> Array.map(fun value -> value*scale)
                            let y=Array.zeroCreate<float> n
                            let x i=if count=1 then 0. else (width-config.NodeWidth)*(float layer[i]/float(count-1))
                            for column in columns do
                                token.ThrowIfCancellationRequested()
                                let free=max 0. (height-(column |> Seq.sumBy(fun i -> sizes[i]))-config.NodePadding*float(max 0 (column.Count-1)))
                                let gap=free/float(column.Count+1)
                                let mutable cursor=gap
                                for i in column do
                                    check i
                                    y[i]<-cursor
                                    cursor<-cursor+sizes[i]+config.NodePadding+gap
                            let center i=y[i]+sizes[i]/2.
                            let resolve (column:ResizeArray<int>) =
                                let sorted=column |> Seq.sortWith(fun a b -> let c=compare y[a] y[b] in if c=0 then compare a b else c) |> Seq.toArray
                                let mutable cursor=0.
                                for i in sorted do
                                    check i
                                    y[i]<-max y[i] cursor
                                    cursor<-y[i]+sizes[i]+config.NodePadding
                                if sorted.Length>0 then
                                    let last=sorted[sorted.Length-1]
                                    if y[last]+sizes[last]>height then
                                        y[last]<-height-sizes[last]
                                        for p in sorted.Length-2 .. -1 .. 0 do
                                            check p
                                            let i,next=sorted[p],sorted[p+1]
                                            y[i]<-min y[i] (y[next]-config.NodePadding-sizes[i])
                                for i in sorted do y[i]<-max 0. (min (height-sizes[i]) y[i])
                            let relax alpha (column:ResizeArray<int>) (links:ResizeArray<int> array) (other:int array) =
                                for i in column do
                                    check i
                                    let mutable total,mean=0.,0.
                                    for e in links[i] do
                                        check e
                                        let weight=if largest=0. then 0. else edges[e].Value/largest
                                        if weight>0. then
                                            let next=total+weight
                                            mean<-mean+(center other[e]-mean)*(weight/next)
                                            total<-next
                                    if total>0. then y[i]<-max 0. (min (height-sizes[i]) (y[i]+alpha*(mean-center i)))
                                resolve column
                            for iteration in 0..config.Iterations-1 do
                                token.ThrowIfCancellationRequested()
                                let alpha=0.99**float iteration
                                for c in count-1 .. -1 .. 0 do relax alpha columns[c] outgoing targets
                                for c in 0..count-1 do relax alpha columns[c] incoming sources
                            let widths=edges |> Array.map(fun edge -> if largest=0. then 0. else (edge.Value/largest)*scale)
                            let startY,finishY=Array.zeroCreate<float> m,Array.zeroCreate<float> m
                            let stack i (links:ResizeArray<int>) (other:int array) (output:float array) =
                                let sorted=links |> Seq.sortWith(fun a b -> let c=compare (center other[a]) (center other[b]) in if c=0 then compare a b else c)
                                let mutable cursor=y[i]
                                for e in sorted do
                                    check e
                                    output[e]<-cursor+widths[e]/2.
                                    cursor<-cursor+widths[e]
                            for i in 0..n-1 do
                                check i
                                stack i outgoing[i] targets startY
                                stack i incoming[i] sources finishY
                            let left,top=Bounds.x config.Bounds,Bounds.y config.Bounds
                            let resultNodes=
                                [for i in 0..n-1 do
                                    check i
                                    yield {Id=nodes[i];Layer=layer[i];Bounds=Bounds.create (left+x i) (top+y[i]) config.NodeWidth sizes[i]
                                           Inflow=inflow[i];Outflow=outflow[i];Value=values[i];Imbalance=outflow[i]-inflow[i]}]
                            let resultLinks=
                                [for e in 0..m-1 do
                                    check e
                                    yield {Id=edges[e].Id;Source=edges[e].Source;Target=edges[e].Target;Value=edges[e].Value;Width=widths[e]
                                           Start=Point.create (left+x sources[e]+config.NodeWidth) (top+startY[e])
                                           Finish=Point.create (left+x targets[e]) (top+finishY[e])}]
                            token.ThrowIfCancellationRequested()
                            Ok {Nodes=resultNodes;Links=resultLinks}
    /// <summary>Assigns columns by alignment, sizes nodes by their larger flow and relaxes vertical positions. Returns Error for invalid settings,
    /// exceeded budgets, a cycle (including zero-valued links) or bounds too small for the columns and padding; cancellation raises
    /// OperationCanceledException.</summary>
    let layoutWithCancellation token config graph = layoutCore token None config graph
    /// <summary>Runs <c>layoutWithCancellation</c> without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph

    /// <summary>Like <c>layoutWithCancellation</c>, but takes each node's column from <c>stages</c>, which must map every node ID to a nonnegative
    /// stage below MaxNodes. Returns InvalidTopology when stages miss a node or a link does not advance to a later stage.</summary>
    let layoutStagedWithCancellation token config stages graph = layoutCore token (Some stages) config graph
    /// <summary>Runs <c>layoutStagedWithCancellation</c> without cancellation.</summary>
    let layoutStaged config stages graph = layoutStagedWithCancellation System.Threading.CancellationToken.None config stages graph

    /// <summary>Moves a node's top edge toward <c>top</c>, clamped between its column neighbours plus padding and the bounds, and shifts its link
    /// endpoints. Returns MissingEndpoint for an unknown ID and InvalidObservation for non-finite input or no feasible slot.</summary>
    let moveNode (config:SankeyConfig) (id:string) (top:float) (layout:SankeyLayout) : Result<SankeyLayout,LayoutError list> =
        if not(System.Double.IsFinite top && System.Double.IsFinite config.NodePadding) || config.NodePadding<0. then
            Error [InvalidObservation "Node position and padding must be finite; padding must be nonnegative."]
        else
            match layout.Nodes |> List.tryFind(fun node -> node.Id=id) with
            | None -> Error [MissingEndpoint id]
            | Some node ->
                let column=layout.Nodes |> List.filter(fun other -> other.Layer=node.Layer)
                           |> List.sortWith(fun a b ->
                               let c=compare (Bounds.y a.Bounds) (Bounds.y b.Bounds)
                               if c=0 then System.StringComparer.Ordinal.Compare(a.Id,b.Id) else c)
                           |> List.toArray
                let index=column |> Array.findIndex(fun other -> other.Id=id)
                let size=Bounds.height node.Bounds
                let lower=if index=0 then Bounds.y config.Bounds else
                              Bounds.y column[index-1].Bounds+Bounds.height column[index-1].Bounds+config.NodePadding
                let upper=if index=column.Length-1 then Bounds.y config.Bounds+Bounds.height config.Bounds-size else
                              Bounds.y column[index+1].Bounds-config.NodePadding-size
                let tolerance=1e-10*max 1. (Bounds.height config.Bounds)
                if lower>upper+tolerance then Error [InvalidObservation "Node has no feasible slot under the requested bounds and padding."]
                else
                    let next=if lower>upper then Bounds.y node.Bounds else max lower (min upper top)
                    let delta=next-Bounds.y node.Bounds
                    let moved={node with Bounds=Bounds.create (Bounds.x node.Bounds) next (Bounds.width node.Bounds) size}
                    let nodes=layout.Nodes |> List.map(fun other -> if other.Id=id then moved else other)
                    let links=layout.Links |> List.map(fun edge ->
                        if edge.Source=id then {edge with Start=Point.create edge.Start.X (edge.Start.Y+delta)}
                        elif edge.Target=id then {edge with Finish=Point.create edge.Finish.X (edge.Finish.Y+delta)}
                        else edge)
                    Ok {Nodes=nodes;Links=links}

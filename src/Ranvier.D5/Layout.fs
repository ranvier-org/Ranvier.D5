namespace Ranvier.D5

open System
open System.Collections.Generic

/// <summary>A pie slice retaining its original observation.</summary>
type PieSlice<'T> = { Data:'T; Value:float; StartAngle:float; EndAngle:float }
/// <summary>A stacked series with independent lower and upper boundaries per observation.</summary>
type StackLayer<'Key> = { Key:'Key; Values:(float*float) list }

/// <summary>Pie and stack layouts over in-memory observations; invalid input throws <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module Layout =
    /// <summary>Allocates an angular interval proportionally to nonnegative finite weights, preserving input order.</summary>
    let pie value start finish data =
        Check.interval "angles" (start,finish)
        let weighted = data |> List.map (fun item ->
            let weight = value item
            Check.finite "value" weight
            if weight<0. then invalidArg "value" "Pie weights must be nonnegative."
            item,weight)
        let maximum = weighted |> List.fold (fun m (_,weight) -> max m weight) 0.
        let total = if maximum=0. then 0. else weighted |> Seq.map (fun (_,weight) -> weight/maximum) |> Data.sum
        let mutable accumulated = 0.
        weighted |> List.map (fun (item,weight) ->
            let first = if total=0. then start else Check.lerp start finish (accumulated/total)
            accumulated <- accumulated + (if maximum=0. then 0. else weight/maximum)
            let last = if total=0. then start else Check.lerp start finish (accumulated/total)
            {Data=item;Value=weight;StartAngle=first;EndAngle=last})
    /// <summary>Stacks equal-length finite series with separate positive and negative baselines.</summary>
    let stack series =
        Check.unique "series" fst series
        let length = match series with [] -> 0 | (_,values)::_ -> List.length values
        let positive,negative = Array.zeroCreate length,Array.zeroCreate length
        series |> List.map (fun (key,values) ->
            if List.length values<>length then invalidArg "series" "Series lengths must match."
            let boundaries = values |> List.mapi (fun i value ->
                Check.finite "value" value
                let baseline = if value>=0. then positive else negative
                let first = baseline[i]
                let last = first+value
                Check.finite "value" last
                baseline[i] <- last
                first,last)
            {Key=key;Values=boundaries})

/// <summary>An immutable hierarchy node with its own nonnegative weight and ordered children.</summary>
type HierarchyNode<'Id when 'Id:comparison> = private { Identity:'Id; Weight:float; Children:HierarchyNode<'Id> list }
/// <summary>A hierarchy node exposed with its parent, depth and aggregated subtree weight.</summary>
type HierarchyItem<'Id> = { Id:'Id; Parent:'Id option; Depth:int; Value:float }
/// <summary>A hierarchy rectangle in scene coordinates.</summary>
type HierarchyBox<'Id> = { Id:'Id; Parent:'Id option; Depth:int; Bounds:Bounds }
/// <summary>A hierarchy node positioned in scene coordinates.</summary>
type HierarchyPoint<'Id> = { Id:'Id; Parent:'Id option; Depth:int; Point:Point }

/// <summary>Construction, traversal and simple layouts for <c>HierarchyNode</c> trees.</summary>
[<RequireQualifiedAccess>]
module Hierarchy =
    /// <summary>Creates an immutable node; weights must be finite and nonnegative.</summary>
    let node id weight children =
        Check.finite "weight" weight
        if weight<0. then invalidArg "weight" "Hierarchy weight must be nonnegative."
        {Identity=id;Weight=weight;Children=children}
    /// <summary>Returns the node's identity.</summary>
    let id node = node.Identity
    /// <summary>Returns the node's own weight, excluding its descendants.</summary>
    let weight node = node.Weight
    /// <summary>Returns the node's children in their original order.</summary>
    let children node = node.Children
    let private build virtualRoot key parent weightOf data =
        let rows=List.toArray data
        let ids=Array.map key rows
        let parents=Array.map parent rows
        let lookup=Dictionary<string,int>(StringComparer.Ordinal)
        let errors=ResizeArray<LayoutError>()
        for i in 0..ids.Length-1 do
            if String.IsNullOrEmpty ids[i] then errors.Add(InvalidObservation "Hierarchy IDs must be nonempty.")
#if FABLE_COMPILER
            elif lookup.ContainsKey ids[i] then errors.Add(DuplicateId ids[i])
            else lookup.Add(ids[i],i)
#else
            elif not(lookup.TryAdd(ids[i],i)) then errors.Add(DuplicateId ids[i])
#endif
        virtualRoot |> Option.iter(fun key ->
            if String.IsNullOrEmpty key then errors.Add(InvalidObservation "Virtual root ID must be nonempty.")
            elif lookup.ContainsKey key then errors.Add(DuplicateId key))
        let children=Array.init rows.Length (fun _ -> ResizeArray<int>())
        let parentIndex=Array.create rows.Length -1
        let roots=ResizeArray<int>()
        for i in 0..rows.Length-1 do
            match parents[i] with
            | None -> roots.Add i
            | Some key when String.IsNullOrEmpty key -> errors.Add(InvalidObservation "Parent IDs must be nonempty; use None for roots.")
            | Some key ->
                match lookup.TryGetValue key with
                | true,index -> parentIndex[i]<-index;children[index].Add i
                | _ -> errors.Add(MissingEndpoint key)
        if virtualRoot.IsNone && roots.Count<>1 then errors.Add(InvalidTopology "A hierarchy requires exactly one root; use forest for multiple roots.")
        if errors.Count>0 then Error(List.ofSeq errors) else
        let remaining=children |> Array.map(fun xs -> xs.Count)
        let ready=Queue<int>()
        for i in 0..remaining.Length-1 do if remaining[i]=0 then ready.Enqueue i
        let built=Array.zeroCreate<HierarchyNode<string>> rows.Length
        let mutable count=0
        while ready.Count>0 do
            let i=ready.Dequeue()
            let childNodes=children[i] |> Seq.map(fun index -> built[index]) |> Seq.toList
            let ownWeight=weightOf rows[i] childNodes.IsEmpty
            if not(Double.IsFinite ownWeight) || ownWeight<0. then errors.Add(InvalidObservation("Invalid hierarchy weight: "+ids[i]))
            else built[i]<-node ids[i] ownWeight childNodes
            count<-count+1
            let p=parentIndex[i]
            if p>=0 then
                remaining[p]<-remaining[p]-1
                if remaining[p]=0 then ready.Enqueue p
        if count<>rows.Length then errors.Add(InvalidTopology "Hierarchy contains a cycle.")
        if errors.Count>0 then Error(List.ofSeq errors) else
        match virtualRoot with
        | Some key -> Ok(node key 0. (roots |> Seq.map(fun i -> built[i]) |> Seq.toList))
        | None -> Ok built[roots[0]]

    /// <summary>Builds an ordered hierarchy from a parent table. Leaves have unit weight; branches have zero own weight. Rejects duplicate IDs, missing parents, cycles and multiple roots.</summary>
    let stratify key parent data = build None key parent (fun _ leaf -> if leaf then 1. else 0.) data
    /// <summary>Builds a hierarchy with explicit nonnegative own weights, retaining source sibling order.</summary>
    let stratifyWeighted key parent weight data = build None key parent (fun item _ -> weight item) data
    /// <summary>Joins parentless roots beneath an explicit, collision-checked virtual root. An empty forest is valid.</summary>
    let forest virtualRoot key parent data = build (Some virtualRoot) key parent (fun _ leaf -> if leaf then 1. else 0.) data

    let private flatten root =
        let result = ResizeArray<_>()
        let pending = Stack<_>()
        let seen = HashSet<_>()
        pending.Push(root,None,0)
        while pending.Count>0 do
            let current,parent,depth = pending.Pop()
            if not (seen.Add current.Identity) then invalidArg "root" "Hierarchy identities must be unique."
            result.Add(current,parent,depth)
            for child in List.rev current.Children do pending.Push(child,Some current.Identity,depth+1)
        result.ToArray()
    let private totals nodes =
        let values = Dictionary<_,float>()
        for current,_,_ in Array.rev nodes do
            values[current.Identity] <- Data.sum (seq {yield current.Weight;for child in current.Children -> values[child.Identity]})
        values
    /// <summary>Returns preorder items with aggregate weights; rejects duplicate identities throughout the hierarchy.</summary>
    let descendants root =
        let nodes = flatten root
        let values = totals nodes
        nodes |> Array.map (fun (node,parent,depth) -> {Id=node.Identity;Parent=parent;Depth=depth;Value=values[node.Identity]}) |> Array.toList
    /// <summary>Creates an alternating slice-and-dice treemap. Zero-weight siblings share space equally.</summary>
    let treemap bounds root =
        let nodes = flatten root
        let values = totals nodes
        let boxes = Dictionary<_,Bounds>()
        boxes[root.Identity] <- bounds
        let result = ResizeArray<_>()
        for current,parent,depth in nodes do
            let box = boxes[current.Identity]
            result.Add({Id=current.Identity;Parent=parent;Depth=depth;Bounds=box})
            let total = current.Children |> Seq.map (fun child -> values[child.Identity]) |> Data.sum
            let mutable accumulated = 0.
            let count = List.length current.Children
            for i,child in List.indexed current.Children do
                let fraction = if total=0. then 1./float count else values[child.Identity]/total
                let next = if i=count-1 then 1. else min 1. (accumulated+fraction)
                let childBox =
                    if depth%2=0 then Bounds.create (Bounds.x box+Bounds.width box*accumulated) (Bounds.y box) (Bounds.width box*(next-accumulated)) (Bounds.height box)
                    else Bounds.create (Bounds.x box) (Bounds.y box+Bounds.height box*accumulated) (Bounds.width box) (Bounds.height box*(next-accumulated))
                boxes[child.Identity] <- childBox
                accumulated <- next
        List.ofSeq result
    /// <summary>Positions leaves evenly and parents at their children's mean; depth controls the vertical coordinate.</summary>
    let tree bounds root =
        let nodes = flatten root
        let leaves = nodes |> Array.filter (fun (node,_,_) -> List.isEmpty node.Children)
        let positions = Dictionary<_,float>()
        for i,(node,_,_) in Array.indexed leaves do
            positions[node.Identity] <- if leaves.Length=1 then 0.5 else float i/float(leaves.Length-1)
        for node,_,_ in Array.rev nodes do
            if not (List.isEmpty node.Children) then positions[node.Identity] <- node.Children |> Seq.map (fun child -> positions[child.Identity]) |> Data.mean |> Option.get
        let depthMax = nodes |> Array.maxBy (fun (_,_,depth) -> depth) |> fun (_,_,depth) -> depth
        nodes |> Array.map (fun (node,parent,depth) ->
            let point = Point.create (Bounds.x bounds+positions[node.Identity]*Bounds.width bounds) (Bounds.y bounds+(if depthMax=0 then 0.5 else float depth/float depthMax)*Bounds.height bounds)
            {Id=node.Identity;Parent=parent;Depth=depth;Point=point}) |> Array.toList

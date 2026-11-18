namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Effective flattened view after collapsing any <c>Collapsed = true</c> groups into macro-nodes.</summary>
type internal EffectiveDiagram =
    { Nodes: DiagramNode[]
      Groups: DiagramGroup[]
      GroupDepth: int[]
      Associations: Association[]
      ReversedIdMap: Dictionary<string, int>
      Constraints: LayoutConstraint list }

/// <summary>A validated, immutable compound, port-aware and association-machine graph.</summary>
type DiagramGraph =
    private
        { NodeItems: DiagramNode[]
          GroupItems: DiagramGroup[]
          AssociationItems: Association[]
          ConstraintItems: LayoutConstraint list }

/// <summary>Validation, construction, promotion and structural queries for <c>DiagramGraph</c>.</summary>
[<RequireQualifiedAccess>]
module DiagramGraph =
    let private nonEmpty kind (errors: ResizeArray<LayoutError>) (seen: HashSet<string>) (value: string) =
        if String.IsNullOrWhiteSpace value then
            errors.Add(InvalidObservation(kind + " ID must not be empty."))
            false
        elif seen.Contains value then
            errors.Add(DuplicateId value)
            false
        else
            seen.Add value |> ignore
            true

    let private positiveSize label (errors: ResizeArray<LayoutError>) (size: TextSize) =
        if not (Double.IsFinite size.Width && Double.IsFinite size.Height) || size.Width <= 0. || size.Height <= 0. then
            errors.Add(InvalidObservation(label + " width and height must be finite and positive."))

    let private nonnegSize label (errors: ResizeArray<LayoutError>) (size: TextSize) =
        if not (Double.IsFinite size.Width && Double.IsFinite size.Height) || size.Width < 0. || size.Height < 0. then
            errors.Add(InvalidObservation(label + " width and height must be finite and nonnegative."))

    /// <summary>Validates nodes, nested compound groups, ports, associations and constraints, sorting entities deterministically by ordinal ID.</summary>
    let createWithConstraints
        (nodes: DiagramNode list)
        (groups: DiagramGroup list)
        (associations: Association list)
        (constraints: LayoutConstraint list)
        : Result<DiagramGraph, LayoutError list> =
        let errors = ResizeArray<LayoutError>()
        let allEntityIds = HashSet<string>(StringComparer.Ordinal)
        let nodePortSets = Dictionary<string, HashSet<string>>(StringComparer.Ordinal)
        let groupIds = HashSet<string>(StringComparer.Ordinal)

        for g in groups do
            if nonEmpty "Group" errors allEntityIds g.Id then
                groupIds.Add g.Id |> ignore
            let p = g.Padding
            if not (Double.IsFinite p.Top && Double.IsFinite p.Right && Double.IsFinite p.Bottom && Double.IsFinite p.Left)
               || p.Top < 0. || p.Right < 0. || p.Bottom < 0. || p.Left < 0. then
                errors.Add(InvalidObservation("Group padding must be finite and nonnegative: " + g.Id))
            positiveSize ("Group minimum size (" + g.Id + ")") errors g.MinSize

        for n in nodes do
            if nonEmpty "Node" errors allEntityIds n.Id then
                let ports = HashSet<string>(StringComparer.Ordinal)
                for port in n.Ports do
                    if String.IsNullOrWhiteSpace port.Id then
                        errors.Add(InvalidObservation("Port ID must not be empty on node " + n.Id))
                    elif ports.Contains port.Id then
                        errors.Add(DuplicateId(n.Id + ":" + port.Id))
                    else
                        ports.Add port.Id |> ignore
                nodePortSets.Add(n.Id, ports)
            positiveSize ("Node size (" + n.Id + ")") errors n.Size
            if not (Double.IsFinite n.DepthSize) || n.DepthSize <= 0. then
                errors.Add(InvalidObservation("Node depth size must be finite and positive: " + n.Id))
            if not (Double.IsFinite n.Mass) || n.Mass <= 0. then
                errors.Add(InvalidObservation("Node mass must be finite and positive: " + n.Id))
            n.Pinned |> Option.iter (fun pt ->
                if not (Double.IsFinite pt.X && Double.IsFinite pt.Y) then
                    errors.Add(InvalidObservation("Pinned position must be finite: " + n.Id)))
            n.ParentGroup |> Option.iter (fun gid ->
                if not (groupIds.Contains gid) then
                    errors.Add(MissingEndpoint gid))

        let groupMap = Dictionary<string, DiagramGroup>(StringComparer.Ordinal)
        for g in groups do
            if not (groupMap.ContainsKey g.Id) then groupMap.Add(g.Id, g)
            g.ParentGroup |> Option.iter (fun pid ->
                if pid = g.Id then errors.Add(InvalidTopology("Group cannot be its own parent: " + g.Id))
                elif not (groupIds.Contains pid) then errors.Add(MissingEndpoint pid))

        // Detect cycles in compound group nesting hierarchy via 3-color DFS.
        if errors.Count = 0 && groupMap.Count > 0 then
            let state = Dictionary<string, int>(StringComparer.Ordinal)
            let mutable hasCycle = false
            for gid in groupMap.Keys do
                if not (state.ContainsKey gid) then
                    let trail = ResizeArray<string>()
                    let mutable cur = Some gid
                    while cur.IsSome && not hasCycle do
                        let id = cur.Value
                        match state.TryGetValue id with
                        | true, 1 -> hasCycle <- true
                        | true, 2 -> cur <- None
                        | _ ->
                            state[id] <- 1
                            trail.Add id
                            cur <- groupMap[id].ParentGroup
                    for visited in trail do state[visited] <- 2
            if hasCycle then
                errors.Add(InvalidTopology "Compound group hierarchy contains a cycle.")

        let assocIds = HashSet<string>(StringComparer.Ordinal)
        let checkEndpoint (ep: Endpoint) =
            if not (allEntityIds.Contains ep.NodeId) then
                errors.Add(MissingEndpoint ep.NodeId)
            else
                match ep.PortId with
                | None -> ()
                | Some pid ->
                    match nodePortSets.TryGetValue ep.NodeId with
                    | true, ports when ports.Contains pid -> ()
                    | _ -> errors.Add(MissingEndpoint(ep.NodeId + ":" + pid))

        for a in associations do
            nonEmpty "Association" errors assocIds a.Id |> ignore
            checkEndpoint a.Source
            checkEndpoint a.Target
            if not (Double.IsFinite a.Weight) || a.Weight <= 0. then
                errors.Add(InvalidObservation("Association weight must be finite and positive: " + a.Id))
            if a.MinLength < 0 || a.MinLength > 1024 then
                errors.Add(InvalidObservation("Association MinLength must be in [0,1024]: " + a.Id))
            a.SourceLabel |> Option.iter (nonnegSize ("Source label (" + a.Id + ")") errors)
            a.CenterLabel |> Option.iter (nonnegSize ("Center label (" + a.Id + ")") errors)
            a.TargetLabel |> Option.iter (nonnegSize ("Target label (" + a.Id + ")") errors)

        for c in constraints do
            match c with
            | LayoutConstraint.AlignHorizontal ids
            | LayoutConstraint.AlignVertical ids ->
                for id in ids do
                    if not (allEntityIds.Contains id) then errors.Add(MissingEndpoint id)
            | LayoutConstraint.OrderAlong(_, before, after, gap) ->
                if not (allEntityIds.Contains before) then errors.Add(MissingEndpoint before)
                if not (allEntityIds.Contains after) then errors.Add(MissingEndpoint after)
                if before = after then errors.Add(InvalidTopology("OrderAlong constraint cannot order an entity before itself: " + before))
                if not (Double.IsFinite gap) then errors.Add(InvalidObservation "OrderAlong minGap must be finite.")
            | LayoutConstraint.FixedPosition(id, pt) ->
                if not (allEntityIds.Contains id) then errors.Add(MissingEndpoint id)
                if not (Double.IsFinite pt.X && Double.IsFinite pt.Y) then
                    errors.Add(InvalidObservation("FixedPosition must be finite: " + id))

        if errors.Count > 0 then
            Error(List.ofSeq errors)
        else
            let sortedNodes =
                nodes
                |> List.map (fun n ->
                    let sortedPorts =
                        n.Ports
                        |> List.sortWith (fun a b ->
                            let c = compare a.Order b.Order
                            if c <> 0 then c else StringComparer.Ordinal.Compare(a.Id, b.Id))
                    { n with Ports = sortedPorts })
                |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))
                |> List.toArray
            let sortedGroups =
                groups
                |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))
                |> List.toArray
            let sortedAssocs =
                associations
                |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id))
                |> List.toArray
            Ok
                { NodeItems = sortedNodes
                  GroupItems = sortedGroups
                  AssociationItems = sortedAssocs
                  ConstraintItems = constraints }

    /// <summary>Validates nodes, nested compound groups and associations without extra constraints.</summary>
    let create nodes groups associations = createWithConstraints nodes groups associations []

    /// <summary>Validates a flat node-and-association diagram with no compound groups or constraints.</summary>
    let simple nodes associations = createWithConstraints nodes [] associations []

    /// <summary>Promotes a validated <c>Ranvier.D5.GraphData</c> value into a <c>DiagramGraph</c>, mapping node radii to circular bounds.</summary>
    let ofGraphData (graph: GraphData) : Result<DiagramGraph, LayoutError list> =
        let nodes =
            GraphData.nodes graph
            |> List.map (fun n ->
                let d = max 8. (2. * n.Radius)
                { Id = n.Id
                  Size = { Width = d; Height = d }
                  DepthSize = d
                  ParentGroup = None
                  Ports = []
                  Kind = NodeKind.Standard
                  Pinned = n.Pinned |> Option.orElse n.Position
                  Mass = n.Mass })
        let assocs =
            GraphData.links graph
            |> List.map (fun e ->
                { Association.create e.Id e.Source e.Target with
                    Weight = if e.Strength > 0. then e.Strength else 1. })
        simple nodes assocs

    /// <summary>Creates an n-ary hyperedge association hub connecting multiple participating endpoints through a diamond hub node.</summary>
    let hyperAssociation hubId hubSize (participants: (string * Endpoint * AssociationKind) list) =
        let hub = { DiagramNode.sized hubId hubSize.Width hubSize.Height with Kind = NodeKind.AssociationHub }
        let spokes =
            participants
            |> List.map (fun (assocId, ep, kind) ->
                Association.betweenPorts assocId (Endpoint.node hubId) ep kind)
        hub, spokes

    /// <summary>Returns nodes in ordinal ID order.</summary>
    let nodes (graph: DiagramGraph) = Array.toList graph.NodeItems
    /// <summary>Returns compound groups in ordinal ID order.</summary>
    let groups (graph: DiagramGraph) = Array.toList graph.GroupItems
    /// <summary>Returns associations in ordinal ID order.</summary>
    let associations (graph: DiagramGraph) = Array.toList graph.AssociationItems
    /// <summary>Returns layout constraints in declaration order.</summary>
    let constraints (graph: DiagramGraph) = graph.ConstraintItems
    /// <summary>Returns the total count of declared nodes.</summary>
    let nodeCount (graph: DiagramGraph) = graph.NodeItems.Length
    /// <summary>Returns the total count of declared groups.</summary>
    let groupCount (graph: DiagramGraph) = graph.GroupItems.Length
    /// <summary>Returns the total count of declared associations.</summary>
    let associationCount (graph: DiagramGraph) = graph.AssociationItems.Length

    let internal nodeArray (graph: DiagramGraph) = graph.NodeItems
    let internal groupArray (graph: DiagramGraph) = graph.GroupItems
    let internal associationArray (graph: DiagramGraph) = graph.AssociationItems

    /// <summary>Resolves collapsed groups and group-targeted endpoints into concrete visible nodes, visible expanded groups and remapped associations.</summary>
    let internal effective (graph: DiagramGraph) : EffectiveDiagram =
        let gLen = graph.GroupItems.Length
        if gLen = 0 then
            let nodes = graph.NodeItems
            let nLen = nodes.Length
#if FABLE_COMPILER
            let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
#else
            let nodeIdx = Dictionary<string, int>(nLen, StringComparer.Ordinal)
#endif
            for i = 0 to nLen - 1 do nodeIdx.Add(nodes[i].Id, i)
            let finalConstraints =
                if graph.ConstraintItems.IsEmpty then []
                else
                    graph.ConstraintItems
                    |> List.choose (fun c ->
                        match c with
                        | LayoutConstraint.AlignHorizontal ids ->
                            let mapped = ids |> List.filter nodeIdx.ContainsKey |> List.distinct
                            if mapped.Length >= 2 then Some(LayoutConstraint.AlignHorizontal mapped) else None
                        | LayoutConstraint.AlignVertical ids ->
                            let mapped = ids |> List.filter nodeIdx.ContainsKey |> List.distinct
                            if mapped.Length >= 2 then Some(LayoutConstraint.AlignVertical mapped) else None
                        | LayoutConstraint.OrderAlong(ax, b, a, gap) ->
                            if b <> a && nodeIdx.ContainsKey b && nodeIdx.ContainsKey a then
                                Some(LayoutConstraint.OrderAlong(ax, b, a, gap))
                            else None
                        | LayoutConstraint.FixedPosition(id, pt) ->
                            if nodeIdx.ContainsKey id then Some(LayoutConstraint.FixedPosition(id, pt)) else None)
            { Nodes = nodes
              Groups = [||]
              GroupDepth = [||]
              Associations = graph.AssociationItems
              ReversedIdMap = nodeIdx
              Constraints = finalConstraints }
        else
#if FABLE_COMPILER
            let groupLookup = Dictionary<string, DiagramGroup>(StringComparer.Ordinal)
#else
            let groupLookup = Dictionary<string, DiagramGroup>(gLen, StringComparer.Ordinal)
#endif
            let mutable anyCollapsed = false
            for i = 0 to gLen - 1 do
                let g = graph.GroupItems[i]
                groupLookup.Add(g.Id, g)
                if g.Collapsed then anyCollapsed <- true

            let mutable anyGroupEndpoint = false
            if not anyCollapsed then
                let assocs = graph.AssociationItems
                let mutable e = 0
                while e < assocs.Length && not anyGroupEndpoint do
                    let a = assocs[e]
                    if groupLookup.ContainsKey a.Source.NodeId || groupLookup.ContainsKey a.Target.NodeId then
                        anyGroupEndpoint <- true
                    e <- e + 1
                if not anyGroupEndpoint && not graph.ConstraintItems.IsEmpty then
                    for c in graph.ConstraintItems do
                        match c with
                        | LayoutConstraint.AlignHorizontal ids
                        | LayoutConstraint.AlignVertical ids ->
                            for id in ids do
                                if groupLookup.ContainsKey id then anyGroupEndpoint <- true
                        | LayoutConstraint.OrderAlong(_, b, a, _) ->
                            if groupLookup.ContainsKey b || groupLookup.ContainsKey a then anyGroupEndpoint <- true
                        | LayoutConstraint.FixedPosition(id, _) ->
                            if groupLookup.ContainsKey id then anyGroupEndpoint <- true

            let rec groupDepth (gid: string) =
                match groupLookup[gid].ParentGroup with
                | None -> 0
                | Some pid -> 1 + groupDepth pid

            if not anyCollapsed && not anyGroupEndpoint then
                let nodes = graph.NodeItems
                let nLen = nodes.Length
#if FABLE_COMPILER
                let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
#else
                let nodeIdx = Dictionary<string, int>(nLen, StringComparer.Ordinal)
#endif
                for i = 0 to nLen - 1 do nodeIdx.Add(nodes[i].Id, i)

                let depthsRaw = Array.init gLen (fun i -> groupDepth graph.GroupItems[i].Id)
                let order = Array.init gLen id
                Array.Sort(order, Comparer<int>.Create(fun a b ->
                    let c = compare depthsRaw[a] depthsRaw[b]
                    if c <> 0 then c else StringComparer.Ordinal.Compare(graph.GroupItems[a].Id, graph.GroupItems[b].Id)))
                let finalGroups = Array.init gLen (fun i -> graph.GroupItems[order[i]])
                let finalDepths = Array.init gLen (fun i -> depthsRaw[order[i]])
                let finalConstraints =
                    if graph.ConstraintItems.IsEmpty then []
                    else
                        graph.ConstraintItems
                        |> List.choose (fun c ->
                            match c with
                            | LayoutConstraint.AlignHorizontal ids ->
                                let mapped = ids |> List.filter nodeIdx.ContainsKey |> List.distinct
                                if mapped.Length >= 2 then Some(LayoutConstraint.AlignHorizontal mapped) else None
                            | LayoutConstraint.AlignVertical ids ->
                                let mapped = ids |> List.filter nodeIdx.ContainsKey |> List.distinct
                                if mapped.Length >= 2 then Some(LayoutConstraint.AlignVertical mapped) else None
                            | LayoutConstraint.OrderAlong(ax, b, a, gap) ->
                                if b <> a && nodeIdx.ContainsKey b && nodeIdx.ContainsKey a then
                                    Some(LayoutConstraint.OrderAlong(ax, b, a, gap))
                                else None
                            | LayoutConstraint.FixedPosition(id, pt) ->
                                if nodeIdx.ContainsKey id then Some(LayoutConstraint.FixedPosition(id, pt)) else None)
                { Nodes = nodes
                  Groups = finalGroups
                  GroupDepth = finalDepths
                  Associations = graph.AssociationItems
                  ReversedIdMap = nodeIdx
                  Constraints = finalConstraints }
            else
                // Find outermost collapsed ancestor for any group ID, plus depth.
                let rec outermostCollapsed (gid: string) (acc: string option) =
                    let g = groupLookup[gid]
                    let nextAcc = if g.Collapsed then Some g.Id else acc
                    match g.ParentGroup with
                    | None -> nextAcc
                    | Some pid -> outermostCollapsed pid nextAcc

                // Visible expanded groups: groups that are NOT collapsed AND have no collapsed ancestor.
                let visibleGroups = ResizeArray<DiagramGroup * int>()
                let collapsedMacroNodes = ResizeArray<DiagramNode>()
                for g in graph.GroupItems do
                    match outermostCollapsed g.Id None with
                    | None -> visibleGroups.Add(g, groupDepth g.Id)
                    | Some topCollapsed when topCollapsed = g.Id ->
                        collapsedMacroNodes.Add
                            { Id = g.Id
                              Size = g.MinSize
                              DepthSize = min g.MinSize.Width g.MinSize.Height
                              ParentGroup = g.ParentGroup
                              Ports = []
                              Kind = NodeKind.Standard
                              Pinned = None
                              Mass = 1. }
                    | Some _ -> ()

                let visibleNodeList = ResizeArray<DiagramNode>(graph.NodeItems.Length + collapsedMacroNodes.Count)
                let nodeRemap = Dictionary<string, string>(StringComparer.Ordinal)
                for m in collapsedMacroNodes do
                    visibleNodeList.Add m
                    nodeRemap[m.Id] <- m.Id

                for n in graph.NodeItems do
                    match n.ParentGroup with
                    | None ->
                        visibleNodeList.Add n
                        nodeRemap[n.Id] <- n.Id
                    | Some gid ->
                        match outermostCollapsed gid None with
                        | None ->
                            visibleNodeList.Add n
                            nodeRemap[n.Id] <- n.Id
                        | Some macroId ->
                            nodeRemap[n.Id] <- macroId

                let firstMemberInGroup = Dictionary<string, string>(StringComparer.Ordinal)
                for n in visibleNodeList do
                    let mutable cur = n.ParentGroup
                    while cur.IsSome do
                        let gid = cur.Value
                        if not (firstMemberInGroup.ContainsKey gid) then
                            firstMemberInGroup.Add(gid, n.Id)
                        cur <- if groupLookup.ContainsKey gid then groupLookup[gid].ParentGroup else None

                let resolveEntityId (id: string) =
                    match nodeRemap.TryGetValue id with
                    | true, mapped -> Some(mapped, mapped <> id)
                    | _ ->
                        if groupLookup.ContainsKey id then
                            match outermostCollapsed id None with
                            | Some macroId -> Some(macroId, true)
                            | None ->
                                match firstMemberInGroup.TryGetValue id with
                                | true, rep -> Some(rep, true)
                                | _ -> None
                        else None

                visibleNodeList.Sort(Comparison<DiagramNode>(fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id)))
                let finalNodes = visibleNodeList.ToArray()
                let nodeIdx = Dictionary<string, int>(StringComparer.Ordinal)
                for i = 0 to finalNodes.Length - 1 do nodeIdx.Add(finalNodes[i].Id, i)

                let finalAssocs = ResizeArray<Association>(graph.AssociationItems.Length)
                for a in graph.AssociationItems do
                    match resolveEntityId a.Source.NodeId, resolveEntityId a.Target.NodeId with
                    | Some(sId, sChanged), Some(tId, tChanged) ->
                        if sId = tId && (sChanged || tChanged) && a.Source.NodeId <> a.Target.NodeId then
                            ()
                        else
                            let srcEp = if sChanged then Endpoint.node sId else a.Source
                            let tgtEp = if tChanged then Endpoint.node tId else a.Target
                            finalAssocs.Add { a with Source = srcEp; Target = tgtEp }
                    | _ -> ()

                visibleGroups.Sort(Comparison<DiagramGroup * int>(fun (a, da) (b, db) ->
                    let c = compare da db
                    if c <> 0 then c else StringComparer.Ordinal.Compare(a.Id, b.Id)))

                let finalGroups = visibleGroups |> Seq.map fst |> Seq.toArray
                let finalDepths = visibleGroups |> Seq.map snd |> Seq.toArray

                let finalConstraints =
                    graph.ConstraintItems
                    |> List.choose (fun c ->
                        match c with
                        | LayoutConstraint.AlignHorizontal ids ->
                            let mapped = ids |> List.choose (resolveEntityId >> Option.map fst) |> List.distinct
                            if mapped.Length >= 2 then Some(LayoutConstraint.AlignHorizontal mapped) else None
                        | LayoutConstraint.AlignVertical ids ->
                            let mapped = ids |> List.choose (resolveEntityId >> Option.map fst) |> List.distinct
                            if mapped.Length >= 2 then Some(LayoutConstraint.AlignVertical mapped) else None
                        | LayoutConstraint.OrderAlong(ax, b, a, gap) ->
                            match resolveEntityId b, resolveEntityId a with
                            | Some(bm, _), Some(am, _) when bm <> am -> Some(LayoutConstraint.OrderAlong(ax, bm, am, gap))
                            | _ -> None
                        | LayoutConstraint.FixedPosition(id, pt) ->
                            resolveEntityId id |> Option.map (fun (m, _) -> LayoutConstraint.FixedPosition(m, pt)))

                { Nodes = finalNodes
                  Groups = finalGroups
                  GroupDepth = finalDepths
                  Associations = finalAssocs.ToArray()
                  ReversedIdMap = nodeIdx
                  Constraints = finalConstraints }

    /// <summary>Returns an equivalent <c>DiagramGraph</c> in which every <c>Collapsed = true</c> group has been replaced by a macro-node and internal collapsed associations removed.</summary>
    let resolveCollapsed (graph: DiagramGraph) : DiagramGraph =
        let eff = effective graph
        { NodeItems = eff.Nodes
          GroupItems = eff.Groups
          AssociationItems = eff.Associations
          ConstraintItems = eff.Constraints }


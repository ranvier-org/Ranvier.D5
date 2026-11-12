namespace Ranvier.D5

/// <summary>A force-graph node: optional initial position, optional fixed pin position, nonnegative collision radius
/// and positive mass, in scene units.</summary>
type GraphNode = { Id:string; Position:Point option; Pinned:Point option; Radius:float; Mass:float }
/// <summary>A force-graph link between node IDs with a nonnegative target distance in scene units and nonnegative spring strength.</summary>
type GraphLink = { Id:string; Source:string; Target:string; Distance:float; Strength:float }

/// <summary>Constructors for <c>GraphNode</c>.</summary>
[<RequireQualifiedAccess>]
module GraphNode =
    /// <summary>Creates an unpinned node without an initial position, with radius 4 and mass 1.</summary>
    let create id = {Id=id;Position=None;Pinned=None;Radius=4.;Mass=1.}
/// <summary>Constructors for <c>GraphLink</c>.</summary>
[<RequireQualifiedAccess>]
module GraphLink =
    /// <summary>Creates a link with target distance 30 and strength 0.1.</summary>
    let create id source target = {Id=id;Source=source;Target=target;Distance=30.;Strength=0.1}

/// <summary>A validated, immutable graph whose nodes and links are each sorted by ordinal ID.</summary>
type GraphData = private { NodeData:GraphNode[]; LinkData:GraphLink[] }

/// <summary>Validation and access for <c>GraphData</c>; invalid input yields typed <c>LayoutError</c> results.</summary>
[<RequireQualifiedAccess>]
module GraphData =
    /// <summary>Validates node/link IDs, endpoints and physical parameters, then captures deterministic ordinal ID order. Self-links and parallel links are retained.</summary>
    let create (nodes:GraphNode list) (links:GraphLink list) =
        let errors=ResizeArray<LayoutError>()
        let nodeIds=System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
        let linkIds=System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
        let id kind (seen:System.Collections.Generic.HashSet<string>) value =
            if System.String.IsNullOrWhiteSpace value then errors.Add(InvalidObservation(kind+" ID must not be empty."))
            elif not(seen.Add value) then errors.Add(DuplicateId value)
        let point name p =
            if not(System.Double.IsFinite p.X && System.Double.IsFinite p.Y) then errors.Add(InvalidObservation(name+" must be finite."))
        let nonnegative name value =
            if not(System.Double.IsFinite value) || value<0. then errors.Add(InvalidObservation(name+" must be finite and nonnegative."))
        for node in nodes do
            id "Node" nodeIds node.Id
            node.Position |> Option.iter(point "Position")
            node.Pinned |> Option.iter(point "Pin")
            nonnegative "Radius" node.Radius
            if not(System.Double.IsFinite node.Mass) || node.Mass<=0. then errors.Add(InvalidObservation "Mass must be finite and positive.")
        for link in links do
            id "Link" linkIds link.Id
            if not(nodeIds.Contains link.Source) then errors.Add(MissingEndpoint link.Source)
            if not(nodeIds.Contains link.Target) then errors.Add(MissingEndpoint link.Target)
            nonnegative "Link distance" link.Distance
            nonnegative "Link strength" link.Strength
        if errors.Count>0 then Error(List.ofSeq errors) else
        Ok {NodeData=nodes |> List.sortWith(fun a b -> System.StringComparer.Ordinal.Compare(a.Id,b.Id)) |> List.toArray
            LinkData=links |> List.sortWith(fun a b -> System.StringComparer.Ordinal.Compare(a.Id,b.Id)) |> List.toArray}
    /// <summary>Returns the nodes in ordinal ID order.</summary>
    let nodes graph = Array.toList graph.NodeData
    /// <summary>Returns the links in ordinal ID order.</summary>
    let links graph = Array.toList graph.LinkData

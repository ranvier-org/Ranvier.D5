namespace Ranvier.D5.Graph

open System
open Ranvier.D5

/// <summary>Primary flow axis for hierarchical, layered and flow-constrained diagram layouts.</summary>
[<RequireQualifiedAccess>]
type LayoutDirection =
    /// <summary>Ranks progress from top (sources) to bottom (sinks).</summary>
    | TopDown
    /// <summary>Ranks progress from bottom (sources) to top (sinks).</summary>
    | BottomUp
    /// <summary>Ranks progress from left (sources) to right (sinks).</summary>
    | LeftRight
    /// <summary>Ranks progress from right (sources) to left (sinks).</summary>
    | RightLeft

/// <summary>Cardinal boundary side of a rectangular diagram node on which a port sits.</summary>
[<RequireQualifiedAccess>]
type PortSide =
    /// <summary>Top horizontal boundary of the node.</summary>
    | North
    /// <summary>Bottom horizontal boundary of the node.</summary>
    | South
    /// <summary>Right vertical boundary of the node.</summary>
    | East
    /// <summary>Left vertical boundary of the node.</summary>
    | West
    /// <summary>Side chosen automatically by the layout or router from relative endpoint geometry.</summary>
    | Auto

/// <summary>A named connection port on a diagram node with side constraint and relative ordering key.</summary>
type DiagramPort =
    { /// <summary>Unique port identifier within the owning node.</summary>
      Id: string
      /// <summary>Boundary side constraint for the port.</summary>
      Side: PortSide
      /// <summary>Relative sort key among ports sharing the same boundary side.</summary>
      Order: int }

/// <summary>Constructors for <c>DiagramPort</c>.</summary>
[<RequireQualifiedAccess>]
module DiagramPort =
    /// <summary>Creates a port on the given boundary side with order 0.</summary>
    let create id side = { Id = id; Side = side; Order = 0 }
    /// <summary>Creates a port on the given boundary side with an explicit side order.</summary>
    let ordered id side order = { Id = id; Side = side; Order = order }

/// <summary>Semantic role of a diagram node in flowcharts, state machines and association machines.</summary>
[<RequireQualifiedAccess>]
type NodeKind =
    /// <summary>A standard rectangular entity, class, process or state node.</summary>
    | Standard
    /// <summary>An initial state pseudostate biased toward source layers.</summary>
    | InitialState
    /// <summary>A final/accepting state biased toward sink layers.</summary>
    | FinalState
    /// <summary>A diamond decision, choice or merge pseudostate.</summary>
    | Choice
    /// <summary>A compact fork, join or junction bar/point.</summary>
    | Junction
    /// <summary>An n-ary association diamond or hyperedge hub linking three or more participating nodes.</summary>
    | AssociationHub

/// <summary>An immutable node specification supporting heterogeneous 2D/3D dimensions, nested group membership, ports and pins.</summary>
type DiagramNode =
    { /// <summary>Globally unique identifier across nodes and groups.</summary>
      Id: string
      /// <summary>Positive finite width and height in layout units.</summary>
      Size: TextSize
      /// <summary>Positive finite Z-extent used by 3D engines; defaults to the smaller 2D dimension.</summary>
      DepthSize: float
      /// <summary>Optional enclosing <c>DiagramGroup</c> identifier.</summary>
      ParentGroup: string option
      /// <summary>Explicit connection ports declared on this node.</summary>
      Ports: DiagramPort list
      /// <summary>Semantic role used by ranking and scene rendering.</summary>
      Kind: NodeKind
      /// <summary>Optional fixed center coordinate in 2D layout space.</summary>
      Pinned: Point option
      /// <summary>Positive finite mass weight for force and inertial solvers.</summary>
      Mass: float }

/// <summary>Constructors and fluent updaters for <c>DiagramNode</c>.</summary>
[<RequireQualifiedAccess>]
module DiagramNode =
    /// <summary>Creates a standard node of size 80 by 40, depth 40, unit mass, no parent group and no explicit ports.</summary>
    let create id =
        { Id = id
          Size = { Width = 80.; Height = 40. }
          DepthSize = 40.
          ParentGroup = None
          Ports = []
          Kind = NodeKind.Standard
          Pinned = None
          Mass = 1. }
    /// <summary>Creates a standard node with explicit width and height and matching depth.</summary>
    let sized id width height =
        let depth = min width height
        { create id with Size = { Width = width; Height = height }; DepthSize = if depth > 0. then depth else 1. }
    /// <summary>Assigns the enclosing compound group identifier.</summary>
    let inGroup groupId (node: DiagramNode) = { node with ParentGroup = Some groupId }
    /// <summary>Replaces the node's explicit port list.</summary>
    let withPorts ports (node: DiagramNode) = { node with Ports = ports }
    /// <summary>Sets the node's semantic kind.</summary>
    let withKind kind (node: DiagramNode) = { node with Kind = kind }
    /// <summary>Pins the node center at a fixed 2D coordinate.</summary>
    let withPin point (node: DiagramNode) = { node with Pinned = Some point }
    /// <summary>Sets the node's 3D depth extent.</summary>
    let withDepth depth (node: DiagramNode) = { node with DepthSize = depth }

/// <summary>Nonnegative interior inset around a compound group's child bounds, supporting top title headers.</summary>
type GroupPadding =
    { /// <summary>Top inset in layout units (often larger to accommodate group title banners).</summary>
      Top: float
      /// <summary>Right inset in layout units.</summary>
      Right: float
      /// <summary>Bottom inset in layout units.</summary>
      Bottom: float
      /// <summary>Left inset in layout units.</summary>
      Left: float }

/// <summary>Constructors for <c>GroupPadding</c>.</summary>
[<RequireQualifiedAccess>]
module GroupPadding =
    /// <summary>Uniform nonnegative inset on all four sides.</summary>
    let uniform pad = { Top = pad; Right = pad; Bottom = pad; Left = pad }
    /// <summary>Inset with an enlarged top header margin and uniform side/bottom padding.</summary>
    let titled headerTop pad = { Top = headerTop; Right = pad; Bottom = pad; Left = pad }

/// <summary>A compound cluster, subgraph or composite state that can nest inside another group and optionally collapse into a macro-node.</summary>
type DiagramGroup =
    { /// <summary>Globally unique identifier across nodes and groups.</summary>
      Id: string
      /// <summary>Optional enclosing parent <c>DiagramGroup</c> identifier.</summary>
      ParentGroup: string option
      /// <summary>Interior padding around member nodes and child groups.</summary>
      Padding: GroupPadding
      /// <summary>When true, member descendants collapse into a single macro-node of <c>MinSize</c> with incident associations remapped.</summary>
      Collapsed: bool
      /// <summary>Minimum width and height when the group is empty or collapsed.</summary>
      MinSize: TextSize }

/// <summary>Constructors and fluent updaters for <c>DiagramGroup</c>.</summary>
[<RequireQualifiedAccess>]
module DiagramGroup =
    /// <summary>Creates an expanded group with 28-unit header padding, 16-unit side padding and 96 by 56 minimum size.</summary>
    let create id =
        { Id = id
          ParentGroup = None
          Padding = GroupPadding.titled 28. 16.
          Collapsed = false
          MinSize = { Width = 96.; Height = 56. } }
    /// <summary>Nests this group inside an outer parent group.</summary>
    let inGroup parentId (group: DiagramGroup) = { group with ParentGroup = Some parentId }
    /// <summary>Marks whether the group is collapsed into a single macro-node.</summary>
    let withCollapsed collapsed (group: DiagramGroup) = { group with Collapsed = collapsed }
    /// <summary>Sets custom interior padding on the group.</summary>
    let withPadding padding (group: DiagramGroup) = { group with Padding = padding }

/// <summary>An association endpoint addressing a node (or group) and an optional port on that node.</summary>
type Endpoint =
    { /// <summary>Target node or group identifier.</summary>
      NodeId: string
      /// <summary>Optional port identifier declared on the endpoint node.</summary>
      PortId: string option }

/// <summary>Constructors for <c>Endpoint</c>.</summary>
[<RequireQualifiedAccess>]
module Endpoint =
    /// <summary>Addresses a node or group center/boundary without fixing a named port.</summary>
    let node nodeId = { NodeId = nodeId; PortId = None }
    /// <summary>Addresses a specific named port on a node.</summary>
    let port nodeId portId = { NodeId = nodeId; PortId = Some portId }

/// <summary>Semantic relationship kind for flowcharts, state machines, ER diagrams and UML class/architecture associations.</summary>
[<RequireQualifiedAccess>]
type AssociationKind =
    /// <summary>Directed one-way flow or link from source to target.</summary>
    | Directed
    /// <summary>Two-way symmetric channel with markers at both ends.</summary>
    | Bidirectional
    /// <summary>Undirected structural association with no arrowhead.</summary>
    | Undirected
    /// <summary>Generalization / inheritance relationship pointing to the supertype target.</summary>
    | Inheritance
    /// <summary>Whole-part composition relationship with filled diamond at source.</summary>
    | Composition
    /// <summary>Shared aggregation relationship with hollow diamond at source.</summary>
    | Aggregation
    /// <summary>Dashed dependency or realization association.</summary>
    | Dependency
    /// <summary>State-machine transition from source state to target state.</summary>
    | Transition

/// <summary>A typed, weighted association between two endpoints with optional multiplicity/guard label boxes and rank span constraint.</summary>
type Association =
    { /// <summary>Unique association identifier.</summary>
      Id: string
      /// <summary>Origin endpoint.</summary>
      Source: Endpoint
      /// <summary>Destination endpoint.</summary>
      Target: Endpoint
      /// <summary>Semantic relationship classification.</summary>
      Kind: AssociationKind
      /// <summary>Positive finite attraction/cut weight.</summary>
      Weight: float
      /// <summary>Minimum rank difference in layered layouts (0 allows same-rank horizontal associations; default 1).</summary>
      MinLength: int
      /// <summary>Optional measured size for a source-end role or ER multiplicity label.</summary>
      SourceLabel: TextSize option
      /// <summary>Optional measured size for a midpoint transition guard or association name label.</summary>
      CenterLabel: TextSize option
      /// <summary>Optional measured size for a target-end role or ER multiplicity label.</summary>
      TargetLabel: TextSize option }

/// <summary>Constructors and fluent updaters for <c>Association</c>.</summary>
[<RequireQualifiedAccess>]
module Association =
    /// <summary>Creates a directed association of unit weight and minimum rank span 1 between two node IDs.</summary>
    let create id sourceId targetId =
        { Id = id
          Source = Endpoint.node sourceId
          Target = Endpoint.node targetId
          Kind = AssociationKind.Directed
          Weight = 1.
          MinLength = 1
          SourceLabel = None
          CenterLabel = None
          TargetLabel = None }
    /// <summary>Creates an association between explicit port/node endpoints with a given semantic kind.</summary>
    let betweenPorts id source target kind =
        { create id source.NodeId target.NodeId with Source = source; Target = target; Kind = kind }
    /// <summary>Creates a state-machine transition with an optional guard/action center label box.</summary>
    let transition id sourceId targetId labelSize =
        { create id sourceId targetId with Kind = AssociationKind.Transition; CenterLabel = labelSize }
    /// <summary>Creates an entity-relationship or UML binary association with optional source, center and target label boxes.</summary>
    let labeled id sourceId targetId kind sourceLabel centerLabel targetLabel =
        { create id sourceId targetId with
            Kind = kind
            SourceLabel = sourceLabel
            CenterLabel = centerLabel
            TargetLabel = targetLabel }

/// <summary>Coordinate axis in 2D constrained layouts.</summary>
[<RequireQualifiedAccess>]
type LayoutAxis =
    /// <summary>Horizontal X coordinate axis.</summary>
    | Horizontal
    /// <summary>Vertical Y coordinate axis.</summary>
    | Vertical

/// <summary>Declarative geometric constraint enforced by constraint-aware layout engines (<c>Cola</c>, <c>FCose</c>, <c>Sugiyama</c>).</summary>
[<RequireQualifiedAccess>]
type LayoutConstraint =
    /// <summary>Aligns the Y centers of all listed nodes along a horizontal rail.</summary>
    | AlignHorizontal of nodeIds: string list
    /// <summary>Aligns the X centers of all listed nodes along a vertical rail.</summary>
    | AlignVertical of nodeIds: string list
    /// <summary>Requires <c>before</c> center plus <c>minGap</c> not to exceed <c>after</c> center along <c>axis</c>.</summary>
    | OrderAlong of axis: LayoutAxis * before: string * after: string * minGap: float
    /// <summary>Fixes a node's center at an exact 2D scene point.</summary>
    | FixedPosition of nodeId: string * position: Point

/// <summary>A resolved port anchor along a positioned node's boundary.</summary>
type PositionedPort =
    { /// <summary>Port identifier.</summary>
      Id: string
      /// <summary>Resolved cardinal side.</summary>
      Side: PortSide
      /// <summary>Exact boundary anchor point in layout coordinates.</summary>
      Anchor: Point }

/// <summary>A node positioned by a 2D layout engine with its bounding box, resolved port anchors and optional rank.</summary>
type PositionedNode =
    { /// <summary>Node identifier (or collapsed group identifier when representing a collapsed macro-node).</summary>
      Id: string
      /// <summary>Semantic node kind.</summary>
      Kind: NodeKind
      /// <summary>Axis-aligned bounding rectangle.</summary>
      Bounds: Bounds
      /// <summary>Center point of <c>Bounds</c>.</summary>
      Center: Point
      /// <summary>Enclosing visible group identifier, if any.</summary>
      ParentGroup: string option
      /// <summary>Assigned layer/rank index when produced by a ranked or radial engine.</summary>
      Layer: int option
      /// <summary>Resolved named ports in ordinal ID order.</summary>
      Ports: PositionedPort list }

/// <summary>A compound group positioned around its member nodes and nested child groups.</summary>
type PositionedGroup =
    { /// <summary>Group identifier.</summary>
      Id: string
      /// <summary>Enclosing parent group identifier, if any.</summary>
      ParentGroup: string option
      /// <summary>Outer padded group bounding rectangle.</summary>
      Bounds: Bounds
      /// <summary>Inner content rectangle excluding <c>GroupPadding</c>.</summary>
      InnerBounds: Bounds
      /// <summary>Whether the group was laid out as a collapsed macro-node.</summary>
      Collapsed: bool
      /// <summary>Nesting depth from root groups (0 at top-level groups).</summary>
      Depth: int }

/// <summary>A non-overlapping anchor rectangle placed for an association label.</summary>
type PositionedLabel =
    { /// <summary>Axis-aligned label box.</summary>
      Bounds: Bounds
      /// <summary>Center point of the label box.</summary>
      Center: Point }

/// <summary>A routed 2D association with waypoints, serialized path, cycle-reversal metadata and positioned labels.</summary>
type RoutedAssociation =
    { /// <summary>Association identifier.</summary>
      Id: string
      /// <summary>Resolved source endpoint.</summary>
      Source: Endpoint
      /// <summary>Resolved target endpoint.</summary>
      Target: Endpoint
      /// <summary>Semantic association kind.</summary>
      Kind: AssociationKind
      /// <summary>Ordered polyline/control waypoints from source boundary to target boundary.</summary>
      Waypoints: Point list
      /// <summary>Drawable SVG-compatible <c>Path</c> following the true source-to-target direction.</summary>
      Path: Path
      /// <summary>True when a cyclic back-edge was temporarily reversed during layered ranking.</summary>
      ReversedInCycle: bool
      /// <summary>Positioned source multiplicity/role label box, if requested.</summary>
      SourceLabel: PositionedLabel option
      /// <summary>Positioned center guard/name label box, if requested.</summary>
      CenterLabel: PositionedLabel option
      /// <summary>Positioned target multiplicity/role label box, if requested.</summary>
      TargetLabel: PositionedLabel option }

/// <summary>Complete, immutable 2D compound diagram layout output shared across all 2D engines.</summary>
type DiagramLayout2D =
    { /// <summary>Enclosing bounding box of all positioned nodes, groups, routes and labels.</summary>
      Bounds: Bounds
      /// <summary>Positioned visible nodes in ordinal ID order.</summary>
      Nodes: PositionedNode list
      /// <summary>Positioned visible groups ordered outermost-first (by ascending depth, then ordinal ID).</summary>
      Groups: PositionedGroup list
      /// <summary>Routed visible associations in ordinal ID order.</summary>
      Associations: RoutedAssociation list }

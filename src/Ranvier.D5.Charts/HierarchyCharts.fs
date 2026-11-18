namespace Ranvier.D5.Charts
open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Hierarchy chart specification: labels and colors by node ID, Padding in pixels, collapsed node IDs drawn as leaves with their subtree total, an optional Focus root ID, and selected IDs to outline.</summary>
type HierarchySpec =
    { Frame:ChartFrame; Label:string -> string; Color:string -> Paint; Padding:float
      Collapsed:Set<string>; Focus:string option; Selected:Set<string> }
/// <summary>Defaults for hierarchy specifications.</summary>
[<RequireQualifiedAccess>]
module HierarchySpec =
    /// <summary>Creates a specification with identity labels, category10 colors hashed from the node ID, 2 px padding, and nothing collapsed, focused or selected.</summary>
    let create frame =
        let palette=Array.init 10 (fun i -> Chromatic.categorical Chromatic.category10 i |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip)
        {Frame=frame;Label=id;Padding=2.;Collapsed=Set.empty;Focus=None;Selected=Set.empty
         Color=fun key ->
            let mutable hash=0u
            for c in key do hash<-hash*31u+uint32 c
            palette[int(hash%10u)]}

/// <summary>Tree, partition, treemap and pack charts of a string-keyed hierarchy; every node is a hit target keyed by its ID, and an unknown Focus, negative padding or layout failure is a typed error.</summary>
[<RequireQualifiedAccess>]
module HierarchyChart =
    let private unwrap = function Ok value -> value | Error errors -> invalidArg "hierarchy" (sprintf "%A" errors)
    let private prepare policy spec root =
        let items=Hierarchy.descendants root
        let totals=Dictionary<string,float>(StringComparer.Ordinal)
        let nodes=Dictionary<string,HierarchyNode<string>>(StringComparer.Ordinal)
        let pending=Stack<HierarchyNode<string>>()
        pending.Push root
        while pending.Count>0 do
            let node=pending.Pop()
            nodes.Add(Hierarchy.id node,node)
            for child in Hierarchy.children node do pending.Push child
        for item in List.rev items do
            let children=Hierarchy.children nodes[item.Id]
            totals[item.Id]<-
                if policy=HierarchySumPolicy.IncludeOwnWeight || children.IsEmpty then item.Value
                else children |> Seq.map(fun child -> totals[Hierarchy.id child]) |> Data.sum
        let focused=
            match spec.Focus with
            | None -> root
            | Some key ->
                match nodes.TryGetValue key with
                | true,node -> node
                | _ -> invalidArg "focus" "Focus ID does not exist in the hierarchy."
        let ordered=ResizeArray<HierarchyNode<string>>()
        pending.Push focused
        while pending.Count>0 do
            let node=pending.Pop()
            ordered.Add node
            if not(spec.Collapsed.Contains(Hierarchy.id node)) then
                for child in Hierarchy.children node |> List.rev do pending.Push child
        let built=Dictionary<string,HierarchyNode<string>>(StringComparer.Ordinal)
        for i in ordered.Count-1 .. -1 .. 0 do
            let node=ordered[i]
            let key=Hierarchy.id node
            built[key]<-
                if spec.Collapsed.Contains key then Hierarchy.node key totals[key] []
                else Hierarchy.node key (Hierarchy.weight node) (Hierarchy.children node |> List.map(fun child -> built[Hierarchy.id child]))
        built[Hierarchy.id focused]

    let private run policy spec root build =
        match ChartLayout.plot spec.Frame with
        | Error errors -> Error errors
        | Ok bounds ->
            try
                if not(Double.IsFinite spec.Padding) || spec.Padding<0. then invalidArg "padding" "Padding must be finite and nonnegative."
                let visible=if spec.Focus.IsNone && spec.Collapsed.IsEmpty then root else prepare policy spec root
                Ok(build bounds visible)
            with :? ArgumentException as error -> Error [InvalidObservation error.Message]

    let private entry spec id geometry mark =
        let key=Recipe.key2 "node" id
        let label=spec.Label id
        let mark=mark key |> Mark.withFill(spec.Color id) |> Mark.withLabel(Some label)
        let mark=if not spec.Selected.IsEmpty && spec.Selected.Contains id then mark |> Mark.withStroke (Paint.rgb 20 20 20) 2. else mark
        let ids=[id]
        struct(mark,Recipe.semantic key ids label,Recipe.hit key ids geometry)

    let private output (spec:HierarchySpec) bounds (entries:struct(Mark*SemanticMark*HitTarget) array) (extras:Mark list) =
        let mutable marks=[]
        let mutable semantics=[]
        let mutable hits=[]
        for i=entries.Length-1 downto 0 do
            let struct(m,s,h)=entries[i]
            marks<-m::marks
            semantics<-s::semantics
            hits<-h::hits
        let allMarks=if extras.IsEmpty then marks else extras@marks
        Recipe.output spec.Frame bounds allMarks semantics hits []

    let private points radial spec root =
        run HierarchySumPolicy.IncludeOwnWeight spec root (fun bounds visible ->
            let cx=Bounds.x bounds+Bounds.width bounds*0.5
            let cy=Bounds.y bounds+Bounds.height bounds*0.5
            let radius=max 0. (min (Bounds.width bounds) (Bounds.height bounds)*0.5-5.)
            let layoutBounds=if radial then Bounds.create 0. 0. (2.*Math.PI) radius else Bounds.create (Bounds.x bounds+min 5. (Bounds.width bounds*0.5)) (Bounds.y bounds+min 5. (Bounds.height bounds*0.5)) (max 0. (Bounds.width bounds-10.)) (max 0. (Bounds.height bounds-10.))
            let positions=(if radial then Tree.layout else Cluster.layout) (TreeConfig.create layoutBounds) visible |> unwrap |> List.toArray
            let lookup=Dictionary<string,Point>(positions.Length,StringComparer.Ordinal)
            for i=0 to positions.Length-1 do
                let item=positions[i]
                lookup[item.Id]<-item.Point
            let circleSymbol=Shape.symbol Symbol.Circle (Math.PI*16.)
            let inline position (point:Point)=if radial then let p=Curves.radialPoint point.X point.Y in Point.create (cx+p.X) (cy+p.Y) else point
            let entries=Array.zeroCreate<struct(Mark*SemanticMark*HitTarget)> positions.Length
            let linkTranslate=if radial then Some(Transform.translate cx cy) else None
            let linkStroke=Paint.rgb 145 160 180
            let mutable linksRev=[]
            for i=0 to positions.Length-1 do
                let item=positions[i]
                let p=position item.Point
                entries[i]<-entry spec item.Id (HitPoint(p,5.)) (fun key -> Mark.path key circleSymbol |> Mark.withTransform(Transform.translate p.X p.Y))
                match item.Parent with
                | Some parent ->
                    let from=lookup[parent]
                    let path=if radial then Curves.linkRadial (from.X,from.Y) (item.Point.X,item.Point.Y) else Curves.linkVertical from item.Point
                    let mark=Mark.path (Recipe.key2 "link" item.Id) path |> Mark.withFill Paint.none |> Mark.withStroke linkStroke 1.
                    let mark=match linkTranslate with Some tr -> Mark.withTransform tr mark | None -> mark
                    linksRev<-mark::linksRev
                | None -> ()
            output spec bounds entries (List.rev linksRev))

    /// <summary>A top-down cluster layout with leaves aligned, node circles and vertical links.</summary>
    let dendrogram spec root = points false spec root
    /// <summary>A tidy tree laid out in polar coordinates around the plot center with radial links.</summary>
    let radialTree spec root = points true spec root
    /// <summary>A rectangular partition with one rectangle per node, depth increasing down the plot.</summary>
    let icicle spec root =
        run HierarchySumPolicy.IncludeOwnWeight spec root (fun bounds visible ->
            let cells=Partition.layout {Bounds=bounds;Padding=spec.Padding} visible |> unwrap |> List.toArray
            let entries=Array.init cells.Length (fun i -> let cell=cells[i] in entry spec cell.Id (HitBounds cell.Bounds) (fun key -> Shape.rectangle cell.Bounds |> Mark.path key))
            output spec bounds entries [])
    /// <summary>A treemap with one rectangle per node, nested with the spec padding.</summary>
    let treemap spec root =
        run HierarchySumPolicy.IncludeOwnWeight spec root (fun bounds visible ->
            let cells=Treemap.layout {TreemapConfig.create bounds with Padding=spec.Padding} visible |> unwrap |> List.toArray
            let entries=Array.init cells.Length (fun i -> let cell=cells[i] in entry spec cell.Id (HitBounds cell.Bounds) (fun key -> Shape.rectangle cell.Bounds |> Mark.path key))
            output spec bounds entries [])
    /// <summary>Circle packing with one circle per node; collapsed totals count leaf weights only.</summary>
    let packedBubbles spec root =
        run HierarchySumPolicy.LeavesOnly spec root (fun bounds visible ->
            let circles=Pack.layout {PackConfig.create bounds with Padding=spec.Padding} visible |> unwrap |> List.toArray
            let entries=Array.init circles.Length (fun i -> let circle=circles[i] in entry spec circle.Id (HitPoint(circle.Center,circle.Radius)) (fun key -> Shape.symbol Symbol.Circle (Math.PI*circle.Radius*circle.Radius) |> Mark.path key |> Mark.withTransform(Transform.translate circle.Center.X circle.Center.Y)))
            output spec bounds entries [])
    /// <summary>A polar partition with one annular sector per node, depth increasing outward; padding shrinks each sector.</summary>
    let sunburst spec root =
        run HierarchySumPolicy.IncludeOwnWeight spec root (fun bounds visible ->
            let center=Point.create (Bounds.x bounds+Bounds.width bounds*0.5) (Bounds.y bounds+Bounds.height bounds*0.5)
            let centerTr=Transform.translate center.X center.Y
            let radius=min (Bounds.width bounds) (Bounds.height bounds)*0.5
            let cells=Partition.layout {Bounds=Bounds.create 0. 0. (2.*Math.PI) radius;Padding=0.} visible |> unwrap |> List.toArray
            let entries=Array.init cells.Length (fun i ->
                let cell=cells[i]
                let inner=Bounds.y cell.Bounds
                let outer=inner+Bounds.height cell.Bounds
                let start=Bounds.x cell.Bounds
                let finish=start+Bounds.width cell.Bounds
                let gap=if outer=0. then 0. else min ((finish-start)*0.5) (spec.Padding/outer*0.5)
                let inner=inner+min (spec.Padding*0.5) ((outer-inner)*0.5)
                let outer=max inner (outer-spec.Padding*0.5)
                entry spec cell.Id (HitSector(center,inner,outer,start+gap,finish-gap)) (fun key -> Shape.arc inner outer (start+gap) (finish-gap) |> Mark.path key |> Mark.withTransform centerTr))
            output spec bounds entries [])

namespace Ranvier.D5.Charts

open System

open Ranvier.D5

/// <summary>Network chart specification: labels and colors by node ID, selected node IDs to outline, link stroke paint and width in pixels, and whether links merge into one path mark.</summary>
type NetworkSpec =
    { Frame:ChartFrame; Label:string -> string; Color:string -> Paint
      Selected:Set<string>; LinkColor:Paint; LinkWidth:float; AggregateLinks:bool }

/// <summary>Defaults for network specifications.</summary>
[<RequireQualifiedAccess>]
module NetworkSpec =
    /// <summary>Creates a specification with identity labels, blue nodes, separate grey 1 px links and no selection.</summary>
    let create frame =
        {Frame=frame;Label=id;Color=fun _ -> Paint.rgb 40 120 200
         Selected=Set.empty;LinkColor=Paint.rgb 160 170 185;LinkWidth=1.;AggregateLinks=false}

/// <summary>Charts of force simulation snapshots; node positions are used as screen coordinates unchanged.</summary>
[<RequireQualifiedAccess>]
module NetworkChart =
    let private unitCircle=Shape.symbol Symbol.Circle System.Math.PI
    let private build (spec:NetworkSpec) (links:GraphLink list) (snapshot:ForceSnapshot) =
        let nodes=snapshot.Nodes |> List.sortWith(fun a b -> StringComparer.Ordinal.Compare(a.Id,b.Id))
        let captured=nodes |> List.map(fun node ->
            {GraphNode.create node.Id with Position=Some node.Position;Pinned=node.Pinned;Radius=node.Radius;Mass=node.Mass})
        match GraphData.create captured [] with
        | Error errors -> Error errors
        | Ok _ ->
            Recipe.run spec.Frame (fun (node:ForceNodeState) -> node.Id) nodes (fun bounds ->
                if not(Double.IsFinite spec.LinkWidth) || spec.LinkWidth<0. then invalidArg "spec" "Link width must be finite and nonnegative."
                let positions=System.Collections.Generic.Dictionary<string,Point>(nodes.Length,StringComparer.Ordinal)
                for node in nodes do positions.Add(node.Id,node.Position)
                let aggregate =
                    if spec.AggregateLinks && not links.IsEmpty then
                        let key=Recipe.key ["links"]
                        let geometry=links |> List.collect(fun link -> [MoveTo positions[link.Source];LineTo positions[link.Target]]) |> Path.ofCommands
                        let mark=geometry |> Mark.path key |> Mark.withFill Paint.none |> Mark.withStroke spec.LinkColor spec.LinkWidth
                        [mark,Recipe.semantic key (links |> List.collect(fun link -> [link.Source;link.Target]) |> List.distinct |> List.sort) "Network links"]
                    else []
                let detailedLinks=if spec.AggregateLinks then [] else links
                let linkEntries=aggregate @ (detailedLinks |> List.map(fun link ->
                    let key=Recipe.key ["link";link.Id]
                    let label=spec.Label link.Source+" → "+spec.Label link.Target
                    let mark=Path.ofCommands [MoveTo positions[link.Source];LineTo positions[link.Target]]
                             |> Mark.path key |> Mark.withFill Paint.none |> Mark.withStroke spec.LinkColor spec.LinkWidth |> Mark.withLabel(Some label)
                    mark,Recipe.semantic key [link.Source;link.Target] label))
                let nodeEntries=nodes |> List.map(fun node ->
                    let key=Recipe.key ["node";node.Id]
                    let label=spec.Label node.Id
                    let selected=spec.Selected.Contains node.Id
                    let geometry,transform=
                        if selected then Shape.symbol Symbol.Circle (System.Math.PI*node.Radius*node.Radius),Transform.translate node.Position.X node.Position.Y
                        else unitCircle,Transform.create node.Radius 0. 0. node.Radius node.Position.X node.Position.Y
                    let mark=geometry |> Mark.path key
                             |> Mark.withTransform transform
                             |> Mark.withFill(spec.Color node.Id) |> Mark.withLabel(Some label)
                    let mark=if selected then mark |> Mark.withStroke (Paint.rgb 20 20 20) 2. else mark
                    mark,Recipe.semantic key [node.Id] label,Recipe.hit key [node.Id] (HitPoint(node.Position,node.Radius)))
                let nodeMarks,nodeSemantics,hits=List.unzip3 nodeEntries
                let linkMarks,linkSemantics=List.unzip linkEntries
                Recipe.output spec.Frame bounds (linkMarks@nodeMarks) (linkSemantics@nodeSemantics) hits [])

    /// <summary>Draws the graph's links as straight lines and the snapshot's nodes as circles with disc hit targets; snapshot node IDs that differ from the graph's are InvalidTopology.</summary>
    let network (spec:NetworkSpec) (graph:GraphData) (snapshot:ForceSnapshot) : Result<ChartOutput,LayoutError list> =
        let expected=System.Collections.Generic.HashSet<string>(GraphData.nodes graph |> Seq.map(fun node -> node.Id),StringComparer.Ordinal)
        if not(expected.SetEquals(snapshot.Nodes |> Seq.map(fun node -> node.Id))) then Error [InvalidTopology "Snapshot nodes do not match the graph."]
        else build spec (GraphData.links graph) snapshot

    /// <summary>Draws only the snapshot's nodes as circles with disc hit targets, without links.</summary>
    let bubbles (spec:NetworkSpec) (snapshot:ForceSnapshot) : Result<ChartOutput,LayoutError list> =
        build spec [] snapshot

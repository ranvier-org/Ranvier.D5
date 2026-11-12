namespace Ranvier.D5.Charts

open Ranvier.D5

type NetworkSpec =
    { Frame:ChartFrame; Label:string -> string; Color:string -> Paint
      Selected:Set<string>; LinkColor:Paint; LinkWidth:float; AggregateLinks:bool }

[<RequireQualifiedAccess>]
module NetworkSpec =
    let create frame =
        {Frame=frame;Label=id;Color=fun _ -> Paint.rgb 40 120 200
         Selected=Set.empty;LinkColor=Paint.rgb 160 170 185;LinkWidth=1.;AggregateLinks=false}

[<RequireQualifiedAccess>]
module NetworkChart =
    let private build (spec:NetworkSpec) links (snapshot:ForceSnapshot) =
        let nodes=snapshot.Nodes |> List.sortWith(fun a b -> System.StringComparer.Ordinal.Compare(a.Id,b.Id))
        let captured=nodes |> List.map(fun node ->
            {GraphNode.create node.Id with Position=Some node.Position;Pinned=node.Pinned;Radius=node.Radius;Mass=node.Mass})
        match GraphData.create captured links with
        | Error errors -> Error errors
        | Ok _ ->
            Recipe.run spec.Frame (fun (node:ForceNodeState) -> node.Id) nodes (fun bounds ->
                if not(System.Double.IsFinite spec.LinkWidth) || spec.LinkWidth<0. then invalidArg "spec" "Link width must be finite and nonnegative."
                let positions=nodes |> List.map(fun node -> node.Id,node.Position) |> Map.ofList
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
                    let mark=Shape.symbol Symbol.Circle (System.Math.PI*node.Radius*node.Radius) |> Mark.path key
                             |> Mark.withTransform(Transform.translate node.Position.X node.Position.Y)
                             |> Mark.withFill(spec.Color node.Id) |> Mark.withLabel(Some label)
                    let mark=if spec.Selected.Contains node.Id then mark |> Mark.withStroke (Paint.rgb 20 20 20) 2. else mark
                    mark,Recipe.semantic key [node.Id] label,Recipe.hit key [node.Id] (HitPoint(node.Position,node.Radius)))
                let nodeMarks,nodeSemantics,hits=List.unzip3 nodeEntries
                let linkMarks,linkSemantics=List.unzip linkEntries
                Recipe.output spec.Frame bounds (linkMarks@nodeMarks) (linkSemantics@nodeSemantics) hits [])

    let network (spec:NetworkSpec) (graph:GraphData) (snapshot:ForceSnapshot) : Result<ChartOutput,LayoutError list> =
        let expected=GraphData.nodes graph |> List.map(fun node -> node.Id) |> Set.ofList
        let actual=snapshot.Nodes |> List.map(fun node -> node.Id) |> Set.ofList
        if expected<>actual then Error [InvalidTopology "Snapshot nodes do not match the graph."]
        else build spec (GraphData.links graph) snapshot

    let bubbles (spec:NetworkSpec) (snapshot:ForceSnapshot) : Result<ChartOutput,LayoutError list> =
        build spec [] snapshot

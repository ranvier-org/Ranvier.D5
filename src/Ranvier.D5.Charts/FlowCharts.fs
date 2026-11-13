namespace Ranvier.D5.Charts

open System

open Ranvier.D5

/// <summary>Chord diagram specification: layout options, labels and colors by domain ID, selected group IDs to outline, and whether to draw group labels.</summary>
type ChordSpec =
    { Frame:ChartFrame; Layout:ChordConfig; Label:string->string; Color:string->Paint; Selected:Set<string>; ShowLabels:bool }

/// <summary>Defaults for chord specifications.</summary>
[<RequireQualifiedAccess>]
module ChordSpec =
    /// <summary>Creates a specification with the default chord layout, identity labels, one blue color, no selection and labels shown.</summary>
    let create frame = {Frame=frame;Layout=ChordConfig.defaultValue;Label=id;Color=(fun _ -> Paint.rgb 40 120 200);Selected=Set.empty;ShowLabels=true}

/// <summary>Sankey and alluvial specification: labels and colors by node ID, selected node IDs to outline, link opacity in [0,1], and whether to draw node labels.</summary>
type FlowSpec =
    { Frame:ChartFrame; Label:string->string; Color:string->Paint; Selected:Set<string>; LinkOpacity:float; ShowLabels:bool }

/// <summary>Defaults for flow specifications.</summary>
[<RequireQualifiedAccess>]
module FlowSpec =
    /// <summary>Creates a specification with identity labels, one blue color, no selection, 0.4 link opacity and labels shown.</summary>
    let create frame = {Frame=frame;Label=id;Color=(fun _ -> Paint.rgb 40 120 200);Selected=Set.empty;LinkOpacity=0.4;ShowLabels=true}

/// <summary>Chord, Sankey and alluvial flow charts.</summary>
[<RequireQualifiedAccess>]
module FlowChart =
    /// <summary>Draws a chord diagram of a square flow matrix with one ID per row, centered in the plot area; group arcs are hit targets and ribbons are semantics carrying both endpoint IDs, and an ID count that differs from the matrix size is InvalidTopology.</summary>
    let chord (spec:ChordSpec) (ids:string list) (matrix:float list list) : Result<ChartOutput,LayoutError list> =
        if ids.Length<>matrix.Length then Error [InvalidTopology "Chord domain IDs must match the matrix dimension."]
        else
            match Chord.layout spec.Layout matrix with
            | Error errors -> Error errors
            | Ok layout ->
                Recipe.run spec.Frame id ids (fun bounds ->
                    let outer=min (Bounds.width bounds) (Bounds.height bounds)/2.-24.
                    if not ids.IsEmpty && outer<=12. then invalidArg "frame" "Chord frame is too small for group bands and labels."
                    let inner=max 0. (outer-12.)
                    let center=Point.create (Bounds.x bounds+Bounds.width bounds/2.) (Bounds.y bounds+Bounds.height bounds/2.)
                    let transform=Transform.translate center.X center.Y
                    let domains=List.toArray ids
                    let flows=layout.Flows |> List.map(fun flow ->
                        let a,b=domains[flow.Source.Index],domains[flow.Target.Index]
                        let domainPair=
                            if spec.Layout.Mode=ChordMode.Directed || StringComparer.Ordinal.Compare(a,b)<=0 then [a;b] else [b;a]
                        let key=Recipe.key ("flow"::domainPair)
                        let label=spec.Label a+" → "+spec.Label b
                        let ribbon={RibbonSpec.create (max 0. (inner-3.)) flow with
                                        ArrowHead=(if spec.Layout.Mode=ChordMode.Directed then Some(min 10. (max 0. (inner-3.))) else None)}
                        let mark=Ribbon.path ribbon |> Mark.path key |> Mark.withFill(spec.Color a)
                                 |> Mark.withOpacity 0.45 |> Mark.withTransform transform |> Mark.withLabel(Some label)
                        mark,Recipe.semantic key domainPair label)
                    let groups=layout.Groups |> List.map(fun group ->
                        let id=domains[group.Index]
                        let key=Recipe.key ["group";id]
                        let label=spec.Label id
                        let mark=Shape.arc inner outer group.StartAngle group.EndAngle |> Mark.path key
                                 |> Mark.withFill(spec.Color id) |> Mark.withTransform transform |> Mark.withLabel(Some label)
                        let mark=if spec.Selected.Contains id then mark |> Mark.withStroke (Paint.rgb 20 20 20) 2. else mark
                        mark,Recipe.semantic key [id] label,Recipe.hit key [id] (HitSector(center,inner,outer,group.StartAngle,group.EndAngle)))
                    let labels=
                        if not spec.ShowLabels then [] else
                        layout.Groups |> List.map(fun group ->
                            let id=domains[group.Index]
                            let angle=group.StartAngle+(group.EndAngle-group.StartAngle)/2.
                            let position=Point.create (center.X+(outer+12.)*sin angle) (center.Y-(outer+12.)*cos angle)
                            Mark.text (Recipe.key ["label";id]) position (spec.Label id) |> Mark.withFontSize 12.
                            |> fun mark -> Mark.withTextStyle {Mark.textStyle mark with Anchor="middle";Baseline="middle"} mark)
                    let flowMarks,flowSemantics=List.unzip flows
                    let marks,semantics,hits=List.unzip3 groups
                    Recipe.output spec.Frame bounds (flowMarks@marks@labels) (flowSemantics@semantics) hits [])
    /// <summary>Renders a precomputed Sankey layout in its own screen coordinates; nodes are hit targets, links are semantics with source and target IDs, and an invalid graph, flow values or opacity are typed errors.</summary>
    let sankey (spec:FlowSpec) (layout:SankeyLayout) : Result<ChartOutput,LayoutError list> =
        let graph=FlowGraph.create (layout.Nodes |> List.map(fun node -> node.Id))
                      (layout.Links |> List.map(fun edge -> {FlowLink.Id=edge.Id;Source=edge.Source;Target=edge.Target;Value=edge.Value}))
        match graph with
        | Error errors -> Error errors
        | Ok _ ->
            Recipe.run spec.Frame (fun (node:SankeyNode) -> node.Id) layout.Nodes (fun bounds ->
                let finiteNonnegative value=Double.IsFinite value && value>=0.
                if not(finiteNonnegative spec.LinkOpacity) || spec.LinkOpacity>1. then invalidArg "spec" "Link opacity must be in [0,1]."
                for node in layout.Nodes do
                    if not(finiteNonnegative node.Value && finiteNonnegative node.Inflow && finiteNonnegative node.Outflow && Double.IsFinite node.Imbalance) then
                        invalidArg "layout" "Node flow values must be finite and nonnegative."
                let links=layout.Links |> List.map(fun edge ->
                    if not(finiteNonnegative edge.Width) then invalidArg "layout" "Link width must be finite and nonnegative."
                    let key=Recipe.key ["link";edge.Id]
                    let label=spec.Label edge.Source+" → "+spec.Label edge.Target
                    let mark=Shape.linkHorizontal edge.Start edge.Finish |> Mark.path key |> Mark.withFill Paint.none
                             |> Mark.withStroke (spec.Color edge.Source) edge.Width |> Mark.withOpacity spec.LinkOpacity |> Mark.withLabel(Some label)
                    mark,Recipe.semantic key [edge.Source;edge.Target] label)
                let nodes=layout.Nodes |> List.map(fun node ->
                    let key=Recipe.key ["node";node.Id]
                    let label=spec.Label node.Id
                    let mark=Shape.rectangle node.Bounds |> Mark.path key |> Mark.withFill(spec.Color node.Id) |> Mark.withLabel(Some label)
                    let mark=if spec.Selected.Contains node.Id then mark |> Mark.withStroke (Paint.rgb 20 20 20) 2. else mark
                    mark,Recipe.semantic key [node.Id] label,Recipe.hit key [node.Id] (HitBounds node.Bounds))
                let labels=
                    if not spec.ShowLabels then [] else
                    layout.Nodes |> List.map(fun node ->
                        let right=Bounds.x node.Bounds>Bounds.x bounds+Bounds.width bounds/2.
                        let x=if right then Bounds.x node.Bounds-6. else Bounds.x node.Bounds+Bounds.width node.Bounds+6.
                        Mark.text (Recipe.key ["label";node.Id]) (Point.create x (Bounds.y node.Bounds+Bounds.height node.Bounds/2.)) (spec.Label node.Id)
                        |> Mark.withFontSize 12.
                        |> fun mark -> Mark.withTextStyle {Mark.textStyle mark with Anchor=(if right then "end" else "start");Baseline="middle"} mark)
                let linkMarks,linkSemantics=List.unzip links
                let marks,semantics,hits=List.unzip3 nodes
                Recipe.output spec.Frame bounds (linkMarks@marks@labels) (linkSemantics@semantics) hits [])
    /// <summary>Lays out a staged flow graph inside the frame's plot area, overriding the config bounds, then renders it as sankey.</summary>
    let alluvial (spec:FlowSpec) (config:SankeyConfig) (stages:Map<string,int>) (graph:FlowGraph) : Result<ChartOutput,LayoutError list> =
        match ChartLayout.plot spec.Frame with
        | Error errors -> Error errors
        | Ok bounds ->
            Sankey.layoutStaged {config with Bounds=bounds} stages graph |> Result.bind(sankey spec)

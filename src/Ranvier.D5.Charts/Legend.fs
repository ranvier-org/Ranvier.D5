namespace Ranvier.D5.Charts

open System
open Ranvier.D5

/// <summary>One categorical legend entry; Id is the semantic ID shared with the chart series, and an invisible entry is dimmed rather than removed.</summary>
type LegendItem = {Id:string;Label:string;Color:Paint;Visible:bool}
/// <summary>A horizontal gradient legend: Color is sampled at normalized positions in [0,1], and axis ticks over Domain are labelled with Format.</summary>
type ContinuousLegendSpec = {Frame:ChartFrame;Domain:float*float;Color:float->Paint;Format:float->string}

/// <summary>Categorical and continuous legend charts.</summary>
[<RequireQualifiedAccess>]
module Legend =
    /// <summary>Visibility changes opacity without removing category identity; applications filter source series using the same IDs.</summary>
    let categorical frame items =
        Recipe.run frame (fun item -> item.Id) items (fun bounds ->
            let row=if items.IsEmpty then 20. else min 24. (Bounds.height bounds/float items.Length)
            let entries=items |> List.mapi (fun index item ->
                let y=Bounds.y bounds+float index*row
                let id=Recipe.key ["legend";item.Id]
                let box=Bounds.create (Bounds.x bounds) y (min 16. row) (min 16. row)
                let opacity=if item.Visible then 1. else 0.3
                let mark=Recipe.rectangle id item.Label item.Color box |> Mark.withOpacity opacity
                let text=Mark.text (Recipe.key ["legend-label";item.Id]) (Point.create (Bounds.x bounds+22.) (y+min 14. row)) item.Label |> Mark.withOpacity opacity
                [mark;text],Recipe.semantic id [item.Id] item.Label,Recipe.hit id [item.Id] (HitBounds(Bounds.create (Bounds.x bounds) y (Bounds.width bounds) row)))
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output frame bounds (List.concat marks) semantics hits [])

    /// <summary>Draws a gradient bar up to 20 px tall across the plot width with a tick axis over Domain; it has no hit targets, and an invalid frame is a typed error.</summary>
    let continuous spec =
        Recipe.run spec.Frame id [] (fun bounds ->
            let scale=Scale.linear spec.Domain (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
            let gradient=LinearGradient("legend-gradient",Point.create (Bounds.x bounds) 0.,Point.create (Bounds.x bounds+Bounds.width bounds) 0.,
                            [for i in 0..32 -> let t=float i/32. in {Offset=t;Color=spec.Color t;Opacity=1.}])
            let box=Bounds.create (Bounds.x bounds) (Bounds.y bounds) (Bounds.width bounds) (min 20. (Bounds.height bounds))
            let mark=Shape.rectangle box |> Mark.path "gradient" |> Mark.withFillDefinition "legend-gradient" |> Mark.withLabel(Some spec.Frame.Title)
            let axes=Axis.values "legend-axis" AxisOrientation.Bottom (Bounds.y box+Bounds.height box) (Bounds.x box,Bounds.x box+Bounds.width box)
                        (Scale.ticks 5 scale |> List.map (fun v -> v.ToString("R",Globalization.CultureInfo.InvariantCulture),Scale.map scale v,spec.Format v))
            let scene=Scene.compose spec.Frame.Viewport spec.Frame.Title [gradient] (List.map Leaf (mark::axes))
            {Scene=scene;PlotBounds=bounds;Semantics=[{MarkPath=["gradient"];DataIds=[];Description=spec.Frame.Title}];HitTargets=[];Description=spec.Frame.Title})

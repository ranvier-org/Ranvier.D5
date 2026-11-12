namespace Ranvier.D5

open System



/// <summary>A finite viewport and ordered, uniquely keyed marks.</summary>
type Scene = private { Width:float; Height:float; Title:string; Marks:Mark list; Nodes:SceneNode list; Definitions:SceneDefinition list; Composed:bool }

[<RequireQualifiedAccess>]
module Scene =
    /// <summary>Creates a scene with positive viewport dimensions; duplicate mark keys reject the scene.</summary>
    let create width height title marks =
        Check.positive "width" width
        Check.positive "height" height
        if isNull title then nullArg "title"
        Check.unique "marks" Mark.key marks
        {Width=width;Height=height;Title=title;Marks=marks;Nodes=List.map Leaf marks;Definitions=[];Composed=false}
    let width scene = scene.Width
    let height scene = scene.Height
    let title scene = scene.Title
    let marks scene = scene.Marks
    let nodes scene = scene.Nodes
    let isComposed scene = scene.Composed
    let definitions scene = scene.Definitions
    /// <summary>Creates a composed scene. Keys are unique among siblings; moving between parents changes identity.</summary>
    let compose (viewport:Viewport) title definitions nodes =
        Check.positive "width" viewport.Width
        Check.positive "height" viewport.Height
        if isNull title then nullArg "title"
        Check.unique "definitions" SceneDefinition.key definitions
        definitions |> List.iter SceneDefinition.validate
        let clips = definitions |> List.choose (function RectClip(key,_) -> Some key | _ -> None) |> Set.ofList
        let gradients = definitions |> List.choose (function LinearGradient(key,_,_,_) | RadialGradient(key,_,_,_) -> Some key | _ -> None) |> Set.ofList
        let rec validate nodes =
            Check.unique "nodes" SceneNode.key nodes
            for node in nodes do
                if String.IsNullOrEmpty(SceneNode.key node) then invalidArg "nodes" "Empty node key."
                match node with
                | Leaf mark -> Mark.fillDefinition mark |> Option.iter (fun key -> if not(Set.contains key gradients) then invalidArg "fillDefinition" "Unknown gradient definition.")
                | Group(group,children) ->
                    group.Clip |> Option.iter (fun clip -> if not(Set.contains clip clips) then invalidArg "clip" "Unknown clip definition.")
                    validate children
        validate nodes
        {Width=viewport.Width;Height=viewport.Height;Title=title;Definitions=definitions;Nodes=nodes;Composed=true
         Marks=SceneNode.paths [] nodes |> List.map snd}

/// <summary>The side on which an axis places ticks and labels.</summary>
[<RequireQualifiedAccess>]
type AxisOrientation = Bottom | Top | Left | Right

[<RequireQualifiedAccess>]
module Axis =
    /// <summary>Builds explicitly bounded axes with caller-supplied semantic tick keys.</summary>
    let private labelStyle orientation mark =
        let style=Mark.textStyle mark
        let anchor=match orientation with AxisOrientation.Left -> "end" | AxisOrientation.Right -> "start" | _ -> "middle"
        Mark.withTextStyle {style with Anchor=anchor;Baseline=(if orientation=AxisOrientation.Left || orientation=AxisOrientation.Right then "middle" else "alphabetic")} mark

    let values prefix orientation origin (start,finish) (positions:(string * float * string) list) =
        Check.finite "origin" origin
        Check.finite "start" start
        Check.finite "finish" finish
        Check.unique "positions" (fun (key,_,_) -> key) positions
        let horizontal = orientation=AxisOrientation.Bottom || orientation=AxisOrientation.Top
        let direction = if orientation=AxisOrientation.Top || orientation=AxisOrientation.Left then -1. else 1.
        let point along across = if horizontal then Point.create along across else Point.create across along
        let stroke mark = mark |> Mark.withFill Paint.none |> Mark.withStroke Paint.currentColor 1.
        let domain = Mark.path (prefix+"/domain") (Path.ofCommands [MoveTo(point start origin);LineTo(point finish origin)]) |> stroke
        domain :: (positions |> List.collect (fun (id,position,label) ->
            let key = prefix+"/value/"+string id.Length+":"+id
            [Mark.path (key+"/tick") (Path.ofCommands [MoveTo(point position origin);LineTo(point position (origin+direction*6.))]) |> stroke
             Mark.text (key+"/label") (point position (origin+direction*(if horizontal then 18. else 9.))) label |> labelStyle orientation]))

    let private build prefix orientation origin positions =
        Check.finite "origin" origin
        let horizontal = orientation=AxisOrientation.Bottom || orientation=AxisOrientation.Top
        let direction = if orientation=AxisOrientation.Top || orientation=AxisOrientation.Left then -1. else 1.
        let point along across = if horizontal then Point.create along across else Point.create across along
        let stroke path = path |> Mark.withFill Paint.none |> Mark.withStroke Paint.currentColor 1.
        let domain =
            match positions with
            | [] -> []
            | _ ->
                let first,_ = List.head positions
                let last,_ = List.last positions
                [Mark.path (prefix+"/domain") (Path.ofCommands [MoveTo(point first origin);LineTo(point last origin)]) |> stroke]
        domain @ (positions |> List.mapi (fun i (position,label) ->
            let key = prefix+"/"+string i
            [Mark.path (key+"/tick") (Path.ofCommands [MoveTo(point position origin);LineTo(point position (origin+direction*6.))]) |> stroke
             Mark.text (key+"/label") (point position (origin+direction*(if horizontal then 18. else 9.))) label |> labelStyle orientation]) |> List.concat)
    /// <summary>Builds composable numeric tick and label marks using a caller-supplied formatter.</summary>
    let numeric prefix orientation origin count scale format =
        Scale.ticks count scale |> List.map (fun value -> Scale.map scale value,format value) |> build prefix orientation origin
    /// <summary>Builds category ticks centered within known bands, preserving supplied category order.</summary>
    let categorical prefix orientation origin categories scale format =
        categories |> List.choose (fun key -> Scale.bandPosition scale key |> Option.map (fun x -> x+Scale.bandwidth scale/2.,format key))
        |> build prefix orientation origin

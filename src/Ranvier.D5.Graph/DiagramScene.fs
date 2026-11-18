namespace Ranvier.D5.Graph

open System
open Ranvier.D5

/// <summary>Visual styling and label lookup callbacks for rendering a <c>DiagramLayout2D</c> into a retained <c>Ranvier.D5.Scene</c>.</summary>
type DiagramTheme =
    { /// <summary>Viewport width and height for the generated scene.</summary>
      Viewport: Viewport
      /// <summary>Accessible scene title.</summary>
      Title: string
      /// <summary>Fill paint for a node ID.</summary>
      NodeFill: string -> Paint
      /// <summary>Stroke paint for a node ID.</summary>
      NodeStroke: string -> Paint
      /// <summary>Display text for a node ID.</summary>
      NodeLabel: string -> string
      /// <summary>Display header text for a compound group ID.</summary>
      GroupLabel: string -> string
      /// <summary>Stroke paint for an association ID.</summary>
      AssociationStroke: string -> Paint
      /// <summary>Optional <c>(sourceText, centerText, targetText)</c> strings for an association ID.</summary>
      AssociationLabels: string -> string option * string option * string option }

/// <summary>Constructors for <c>DiagramTheme</c>.</summary>
[<RequireQualifiedAccess>]
module DiagramTheme =
    /// <summary>Creates a clean light-surface Mermaid/ELK-inspired diagram theme using IDs as default node and group captions.</summary>
    let create width height title =
        { Viewport = { Width = width; Height = height }
          Title = title
          NodeFill = fun _ -> Paint.rgb 243 247 253
          NodeStroke = fun _ -> Paint.rgb 58 96 156
          NodeLabel = id
          GroupLabel = id
          AssociationStroke = fun _ -> Paint.rgb 71 85 105
          AssociationLabels = fun _ -> None, None, None }

/// <summary>Pure renderer converting any <c>DiagramLayout2D</c> (compound groups, state/UML/ER shapes, markers and labels) into a <c>Ranvier.D5.Scene</c>.</summary>
[<RequireQualifiedAccess>]
module DiagramScene =
    let private markerPath (fromPt: Point) (tipPt: Point) (kind: AssociationKind) (atSource: bool) : (Path * Paint * Paint) option =
        let dx = tipPt.X - fromPt.X
        let dy = tipPt.Y - fromPt.Y
        let len = Double.Hypot(dx, dy)
        if len <= 1e-6 then None
        else
            let ux, uy = dx / len, dy / len
            let nx, ny = -uy, ux
            let arrowFilled size =
                let bx = tipPt.X - ux * size
                let by = tipPt.Y - uy * size
                let w = size * 0.55
                let p1 = Point.create (bx + nx * w) (by + ny * w)
                let p2 = Point.create (bx - nx * w) (by - ny * w)
                Path.ofCommands [ MoveTo tipPt; LineTo p1; LineTo p2; Close ]
            let diamond size =
                let midX, midY = tipPt.X - ux * (size * 0.5), tipPt.Y - uy * (size * 0.5)
                let backX, backY = tipPt.X - ux * size, tipPt.Y - uy * size
                let w = size * 0.42
                Path.ofCommands
                    [ MoveTo tipPt
                      LineTo(Point.create (midX + nx * w) (midY + ny * w))
                      LineTo(Point.create backX backY)
                      LineTo(Point.create (midX - nx * w) (midY - ny * w))
                      Close ]
            match atSource, kind with
            | false, (AssociationKind.Directed | AssociationKind.Transition | AssociationKind.Bidirectional) ->
                Some(arrowFilled 9., Paint.currentColor, Paint.currentColor)
            | false, AssociationKind.Dependency ->
                let bx, by = tipPt.X - ux * 9., tipPt.Y - uy * 9.
                let w = 4.8
                let openV =
                    Path.ofCommands
                        [ MoveTo(Point.create (bx + nx * w) (by + ny * w))
                          LineTo tipPt
                          LineTo(Point.create (bx - nx * w) (by - ny * w)) ]
                Some(openV, Paint.none, Paint.currentColor)
            | false, AssociationKind.Inheritance ->
                Some(arrowFilled 11., Paint.rgb 255 255 255, Paint.currentColor)
            | true, AssociationKind.Bidirectional ->
                Some(arrowFilled 9., Paint.currentColor, Paint.currentColor)
            | true, AssociationKind.Composition ->
                Some(diamond 12., Paint.currentColor, Paint.currentColor)
            | true, AssociationKind.Aggregation ->
                Some(diamond 12., Paint.rgb 255 255 255, Paint.currentColor)
            | _ -> None

    let private nodeShapePath (node: PositionedNode) : Path =
        let b = node.Bounds
        let x, y, w, h = Bounds.x b, Bounds.y b, Bounds.width b, Bounds.height b
        let cx, cy = node.Center.X, node.Center.Y
        match node.Kind with
        | NodeKind.InitialState
        | NodeKind.Junction ->
            let r = max 4. (min w h * 0.45)
            Shape.arc 0. r 0. (2. * Math.PI)
        | NodeKind.FinalState ->
            let rOuter = max 6. (min w h * 0.48)
            let rInner = rOuter * 0.62
            Path.concat [ Shape.arc (rOuter * 0.86) rOuter 0. (2. * Math.PI); Shape.arc 0. rInner 0. (2. * Math.PI) ]
        | NodeKind.Choice
        | NodeKind.AssociationHub ->
            Path.ofCommands
                [ MoveTo(Point.create cx y)
                  LineTo(Point.create (x + w) cy)
                  LineTo(Point.create cx (y + h))
                  LineTo(Point.create x cy)
                  Close ]
        | NodeKind.Standard ->
            Shape.rectangle b

    /// <summary>Builds a structured <c>Ranvier.D5.Scene</c> with separate keyed groups for compound containers, associations, nodes and labels.</summary>
    let render (theme: DiagramTheme) (layout: DiagramLayout2D) : Scene =
        let centerStyle = { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "central" }
        let headerStyle = { Family = "sans-serif"; Weight = 600; Anchor = "start"; Baseline = "hanging" }

        let groupMarks =
            layout.Groups
            |> List.collect (fun g ->
                let shade = max 224 (248 - g.Depth * 6)
                let boxMark =
                    Mark.path ("group:" + g.Id + ":box") (Shape.rectangle g.Bounds)
                    |> Mark.withFill (Paint.rgb shade (min 255 (shade + 2)) 255)
                    |> Mark.withStroke (Paint.rgb 148 163 184) 1.25
                    |> Mark.withOpacity 0.88
                let titleText = theme.GroupLabel g.Id
                if String.IsNullOrEmpty titleText then
                    [ Leaf boxMark ]
                else
                    let tx = Bounds.x g.Bounds + 10.
                    let ty = Bounds.y g.Bounds + 6.
                    let lblMark =
                        Mark.text ("group:" + g.Id + ":title") (Point.create tx ty) titleText
                        |> Mark.withFill (Paint.rgb 51 65 85)
                        |> Mark.withFontSize 11.
                        |> Mark.withTextStyle headerStyle
                    [ Leaf boxMark; Leaf lblMark ])

        let assocMarks =
            layout.Associations
            |> List.collect (fun a ->
                let stroke = theme.AssociationStroke a.Id
                let lineMark =
                    Mark.path ("assoc:" + a.Id + ":path") a.Path
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke stroke 1.6
                let wps = List.toArray a.Waypoints
                let endMarker =
                    if wps.Length >= 2 then
                        markerPath wps[wps.Length - 2] wps[wps.Length - 1] a.Kind false
                        |> Option.map (fun (mp, fillP, _) ->
                            let actualFill = if Paint.toCss fillP = "currentColor" then stroke else fillP
                            Leaf(
                                Mark.path ("assoc:" + a.Id + ":target-marker") mp
                                |> Mark.withFill actualFill
                                |> Mark.withStroke stroke 1.4
                            ))
                    else None
                let startMarker =
                    if wps.Length >= 2 then
                        markerPath wps[1] wps[0] a.Kind true
                        |> Option.map (fun (mp, fillP, _) ->
                            let actualFill = if Paint.toCss fillP = "currentColor" then stroke else fillP
                            Leaf(
                                Mark.path ("assoc:" + a.Id + ":source-marker") mp
                                |> Mark.withFill actualFill
                                |> Mark.withStroke stroke 1.4
                            ))
                    else None

                let sTxt, cTxt, tTxt = theme.AssociationLabels a.Id
                let renderAssocLabel suffix (boxOpt: PositionedLabel option) (txtOpt: string option) =
                    match boxOpt, txtOpt with
                    | Some b, Some txt when not (String.IsNullOrEmpty txt) ->
                        [ Leaf(
                            Mark.path ("assoc:" + a.Id + ":" + suffix + ":bg") (Shape.rectangle b.Bounds)
                            |> Mark.withFill (Paint.rgb 255 255 255)
                            |> Mark.withOpacity 0.88
                          )
                          Leaf(
                            Mark.text ("assoc:" + a.Id + ":" + suffix + ":txt") b.Center txt
                            |> Mark.withFill (Paint.rgb 30 41 59)
                            |> Mark.withFontSize 10.5
                            |> Mark.withTextStyle centerStyle
                          ) ]
                    | _ -> []

                [ yield Leaf lineMark
                  yield! Option.toList startMarker
                  yield! Option.toList endMarker
                  yield! renderAssocLabel "src" a.SourceLabel sTxt
                  yield! renderAssocLabel "mid" a.CenterLabel cTxt
                  yield! renderAssocLabel "tgt" a.TargetLabel tTxt ])

        let nodeMarks =
            layout.Nodes
            |> List.collect (fun n ->
                let shape = nodeShapePath n
                let useCenteredTransform =
                    match n.Kind with
                    | NodeKind.InitialState | NodeKind.FinalState | NodeKind.Junction -> true
                    | _ -> false
                let fill =
                    match n.Kind with
                    | NodeKind.InitialState | NodeKind.FinalState | NodeKind.Junction -> theme.NodeStroke n.Id
                    | _ -> theme.NodeFill n.Id
                let baseMark =
                    Mark.path ("node:" + n.Id + ":shape") shape
                    |> Mark.withFill fill
                    |> Mark.withStroke (theme.NodeStroke n.Id) 1.5
                    |> fun m -> if useCenteredTransform then Mark.withTransform (Transform.translate n.Center.X n.Center.Y) m else m
                let caption =
                    match n.Kind with
                    | NodeKind.InitialState | NodeKind.FinalState | NodeKind.Junction -> ""
                    | _ -> theme.NodeLabel n.Id
                let portDots =
                    n.Ports
                    |> List.map (fun p ->
                        Leaf(
                            Mark.path ("node:" + n.Id + ":port:" + p.Id) (Shape.arc 0. 2.5 0. (2. * Math.PI))
                            |> Mark.withFill (theme.NodeStroke n.Id)
                            |> Mark.withTransform (Transform.translate p.Anchor.X p.Anchor.Y)
                        ))
                if String.IsNullOrEmpty caption then
                    Leaf baseMark :: portDots
                else
                    let txtMark =
                        Mark.text ("node:" + n.Id + ":label") n.Center caption
                        |> Mark.withFill (Paint.rgb 15 23 42)
                        |> Mark.withFontSize 12.
                        |> Mark.withTextStyle centerStyle
                    Leaf baseMark :: Leaf txtMark :: portDots)

        Scene.compose
            theme.Viewport
            theme.Title
            []
            [ Group(GroupSpec.create "groups", groupMarks)
              Group(GroupSpec.create "associations", assocMarks)
              Group(GroupSpec.create "nodes", nodeMarks) ]

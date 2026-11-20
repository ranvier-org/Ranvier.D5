namespace Ranvier.D5.Wgpu

open System
open System.Collections.Generic
open System.Globalization
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Orientation of a scalar colorbar legend overlay.</summary>
[<RequireQualifiedAccess; Struct>]
type LegendOrientation3D =
    /// <summary>Vertical gradient bar running bottom-to-top with ticks beside the bar.</summary>
    | Vertical
    /// <summary>Horizontal gradient bar running left-to-right with ticks below the bar.</summary>
    | Horizontal

/// <summary>Specification for rendering a 2D SVG scalar colorbar legend overlay in <c>Legend3D.colorbar</c>.</summary>
type ColorbarSpec3D =
    {
        /// <summary>Target gradient bar rectangle in logical viewport/scene coordinates.</summary>
        Bounds: Bounds
        /// <summary>Title displayed next to the colorbar.</summary>
        Title: string
        /// <summary>Numeric domain represented across the gradient bar.</summary>
        Domain: float * float
        /// <summary>Colormap ramp sampled across 33 evenly spaced stops.</summary>
        Ramp: ColorRamp
        /// <summary>Target number of nice scale tick labels (<c>0</c> omits ticks).</summary>
        TickCount: int
        /// <summary>Optional custom tick value formatter; defaults to invariant numeric formatting.</summary>
        FormatTick: (float -> string) option
        /// <summary>Gradient direction and tick placement (<c>Vertical</c> or <c>Horizontal</c>).</summary>
        Orientation: LegendOrientation3D
        /// <summary>Paint color for tick marks, border, tick labels, and title.</summary>
        TextColor: Paint
        /// <summary>Tick label font size in scene units (title renders 1px larger).</summary>
        FontSize: float
    }

/// <summary>Convenience constructors for <c>ColorbarSpec3D</c>.</summary>
[<RequireQualifiedAccess>]
module ColorbarSpec3D =
    /// <summary>Creates a vertical bottom-to-top <c>ColorbarSpec3D</c> with 5 ticks and dark-theme light text styling.</summary>
    let vertical (bounds: Bounds) (title: string) (domain: float * float) (ramp: ColorRamp) : ColorbarSpec3D =
        if isNull (box ramp) then nullArg (nameof ramp)
        if isNull title then nullArg (nameof title)

        {
            Bounds = bounds
            Title = title
            Domain = domain
            Ramp = ramp
            TickCount = 5
            FormatTick = None
            Orientation = LegendOrientation3D.Vertical
            TextColor = Paint.rgb 226 232 240
            FontSize = 11.0
        }

    /// <summary>Creates a horizontal left-to-right <c>ColorbarSpec3D</c> with 5 ticks and dark-theme light text styling.</summary>
    let horizontal (bounds: Bounds) (title: string) (domain: float * float) (ramp: ColorRamp) : ColorbarSpec3D =
        { vertical bounds title domain ramp with
            Orientation = LegendOrientation3D.Horizontal
        }

/// <summary>Categorical legend swatch entry pairing an identifier, label, fill color, and visibility flag.</summary>
type LegendSwatch3D =
    {
        /// <summary>Unique swatch identifier used to derive stable SVG mark keys.</summary>
        Id: string
        /// <summary>Display label rendered beside the swatch box.</summary>
        Label: string
        /// <summary>Swatch fill color.</summary>
        Color: RgbaColor
        /// <summary>When false, renders the swatch and label dimmed to indicate a hidden series.</summary>
        Visible: bool
    }

/// <summary>2D SVG legend overlay builders (<c>colorbar</c>, <c>categorical</c>, and <c>combine</c>) producing composed <c>Ranvier.D5.Scene</c>s for <c>PlotPass3D</c>.</summary>
[<RequireQualifiedAccess>]
module Legend3D =
    let private formatTickDefault (value: float) : string =
        let clean = if abs value < 1e-12 then 0.0 else value
        if abs clean >= 1e5 || (clean <> 0.0 && abs clean < 1e-3) then
            clean.ToString ("G3", CultureInfo.InvariantCulture)
        else
            clean.ToString ("G4", CultureInfo.InvariantCulture)

    let private solidOpaquePaint (color: RgbaColor) : Paint =
        ColorConv3D.toPaint (RgbaColor (color.R, color.G, color.B, 1.0f))

    /// <summary>
    /// Builds a composed 2D SVG <c>Scene</c> containing a 33-stop <c>LinearGradient</c>, gradient bar, border stroke,
    /// nice scale ticks, tick labels, and title for <c>spec</c>.
    /// </summary>
    let colorbar (viewportWidth: float) (viewportHeight: float) (spec: ColorbarSpec3D) : Scene =
        Check3D.positive (nameof viewportWidth) viewportWidth
        Check3D.positive (nameof viewportHeight) viewportHeight

        if isNull (box spec) then nullArg (nameof spec)
        if isNull (box spec.Ramp) then nullArg "spec.Ramp"
        if isNull spec.Title then nullArg "spec.Title"
        if isNull (box spec.TextColor) then nullArg "spec.TextColor"

        Check3D.interval "spec.Domain" spec.Domain
        Check3D.positive "spec.FontSize" spec.FontSize

        if spec.TickCount < 0 then
            raise (ArgumentOutOfRangeException ("spec.TickCount", "Colorbar tick count must be nonnegative."))

        let bx = Bounds.x spec.Bounds
        let by = Bounds.y spec.Bounds
        let bw = Bounds.width spec.Bounds
        let bh = Bounds.height spec.Bounds
        Check3D.finite "spec.Bounds.X" bx
        Check3D.finite "spec.Bounds.Y" by
        Check3D.positive "spec.Bounds.Width" bw
        Check3D.positive "spec.Bounds.Height" bh

        let sampledStops = ColorRamp.stops 33 spec.Ramp
        let stops =
            [
                for i = 0 to sampledStops.Length - 1 do
                    let c = sampledStops.Span[i]
                    let offset = float i / float (max 1 (sampledStops.Length - 1))
                    {
                        Offset = offset
                        Color = solidOpaquePaint c
                        Opacity = Math.Clamp (float c.A, 0.0, 1.0)
                    }
            ]

        let gradKey = "legend3d/colorbar/grad"
        let isVertical = (spec.Orientation = LegendOrientation3D.Vertical)

        let startPt, endPt =
            if isVertical then
                Point.create bx (by + bh), Point.create bx by
            else
                Point.create bx by, Point.create (bx + bw) by

        let rectPath =
            Path.ofCommands
                [
                    MoveTo (Point.create bx by)
                    LineTo (Point.create (bx + bw) by)
                    LineTo (Point.create (bx + bw) (by + bh))
                    LineTo (Point.create bx (by + bh))
                    Close
                ]

        let barMark =
            Mark.path "legend3d/colorbar/bar" rectPath
            |> Mark.withFillDefinition gradKey
            |> Mark.withStroke Paint.none 0.0

        let borderMark =
            Mark.path "legend3d/colorbar/border" rectPath
            |> Mark.withFill Paint.none
            |> Mark.withStroke spec.TextColor 1.0

        let marks = ResizeArray<Mark>()
        marks.Add barMark
        marks.Add borderMark

        if spec.TickCount > 0 then
            let d0, d1 = spec.Domain
            let tickScale = Scale.linear spec.Domain (0.0, 1.0)
            let tickValues =
                if d0 = d1 then [ d0 ]
                else Scale.ticks spec.TickCount tickScale

            let fmt =
                match spec.FormatTick with
                | Some f -> f
                | None -> formatTickDefault

            let tickOnRight = bx + bw + 44.0 <= viewportWidth

            let mutable idx = 0

            for v in tickValues do
                let tOpt =
                    if d0 = d1 then Some 0.5
                    else Scale.tryMap tickScale v

                match tOpt with
                | Some t when Check3D.isFinite t && t >= -1e-5 && t <= 1.0 + 1e-5 ->
                    let tc = Math.Clamp (t, 0.0, 1.0)
                    let text = fmt v

                    if isVertical then
                        let ty = (by + bh) - tc * bh
                        let tickX0, tickX1, lblX, anchor =
                            if tickOnRight then bx + bw, bx + bw + 4.0, bx + bw + 7.0, "start"
                            else bx - 4.0, bx, bx - 7.0, "end"

                        let tickMark =
                            Mark.path $"legend3d/colorbar/tick/{idx}" (Path.ofCommands [ MoveTo (Point.create tickX0 ty); LineTo (Point.create tickX1 ty) ])
                            |> Mark.withFill Paint.none
                            |> Mark.withStroke spec.TextColor 1.0

                        let lblMark = Mark.text $"legend3d/colorbar/label/{idx}" (Point.create lblX ty) text
                        let style = Mark.textStyle lblMark

                        let styledLbl =
                            lblMark
                            |> Mark.withFill spec.TextColor
                            |> Mark.withFontSize spec.FontSize
                            |> Mark.withTextStyle { style with Anchor = anchor; Baseline = "middle" }

                        marks.Add tickMark
                        marks.Add styledLbl
                    else
                        let tx = bx + tc * bw
                        let tickY0, tickY1, lblY = by + bh, by + bh + 4.0, by + bh + 14.0

                        let tickMark =
                            Mark.path $"legend3d/colorbar/tick/{idx}" (Path.ofCommands [ MoveTo (Point.create tx tickY0); LineTo (Point.create tx tickY1) ])
                            |> Mark.withFill Paint.none
                            |> Mark.withStroke spec.TextColor 1.0

                        let lblMark = Mark.text $"legend3d/colorbar/label/{idx}" (Point.create tx lblY) text
                        let style = Mark.textStyle lblMark

                        let styledLbl =
                            lblMark
                            |> Mark.withFill spec.TextColor
                            |> Mark.withFontSize spec.FontSize
                            |> Mark.withTextStyle { style with Anchor = "middle"; Baseline = "middle" }

                        marks.Add tickMark
                        marks.Add styledLbl

                    idx <- idx + 1
                | _ -> ()

        if not (String.IsNullOrWhiteSpace spec.Title) then
            let titlePt = Point.create (bx + bw * 0.5) (max 12.0 (by - 12.0))
            let titleMark = Mark.text "legend3d/colorbar/title" titlePt spec.Title
            let titleStyle = Mark.textStyle titleMark

            let styledTitle =
                titleMark
                |> Mark.withFill spec.TextColor
                |> Mark.withFontSize (spec.FontSize + 1.0)
                |> Mark.withTextStyle { titleStyle with Anchor = "middle"; Baseline = "middle"; Weight = 600 }

            marks.Add styledTitle

        Scene.compose
            { Width = viewportWidth; Height = viewportHeight }
            "Legend3D.Colorbar"
            [ SceneDefinition.LinearGradient (gradKey, startPt, endPt, stops) ]
            (marks |> Seq.map Leaf |> Seq.toList)

    /// <summary>
    /// Builds a composed 2D SVG <c>Scene</c> of categorical swatch boxes and labels starting at <c>origin</c>,
    /// dimming items whose <c>Visible = false</c>.
    /// </summary>
    let categorical
        (viewportWidth: float)
        (viewportHeight: float)
        (origin: Point)
        (title: string option)
        (items: LegendSwatch3D list)
        : Scene =
        Check3D.positive (nameof viewportWidth) viewportWidth
        Check3D.positive (nameof viewportHeight) viewportHeight
        Point.create origin.X origin.Y |> ignore

        if isNull (box items) then
            nullArg (nameof items)

        let marks = ResizeArray<Mark>()
        let seenIds = HashSet<string>(StringComparer.Ordinal)

        let mutable rowY = origin.Y

        match title with
        | Some t when not (String.IsNullOrWhiteSpace t) ->
            let tMark = Mark.text "legend3d/categorical/title" (Point.create origin.X rowY) t
            let tStyle = Mark.textStyle tMark

            let styledTitle =
                tMark
                |> Mark.withFill (Paint.rgb 241 245 249)
                |> Mark.withFontSize 12.0
                |> Mark.withTextStyle { tStyle with Anchor = "start"; Baseline = "middle"; Weight = 600 }

            marks.Add styledTitle
            rowY <- rowY + 20.0
        | _ -> ()

        let swatchSize = 12.0
        let rowStep = 20.0
        let textColor = Paint.rgb 226 232 240
        let borderColor = Paint.rgb 148 163 184

        for item in items do
            if String.IsNullOrEmpty item.Id then
                invalidArg (nameof items) "LegendSwatch3D.Id must be non-empty."

            if not (seenIds.Add item.Id) then
                invalidArg (nameof items) $"Duplicate LegendSwatch3D.Id: '{item.Id}'."

            if isNull item.Label then
                nullArg "item.Label"

            let boxY = rowY - swatchSize * 0.5

            let boxPath =
                Path.ofCommands
                    [
                        MoveTo (Point.create origin.X boxY)
                        LineTo (Point.create (origin.X + swatchSize) boxY)
                        LineTo (Point.create (origin.X + swatchSize) (boxY + swatchSize))
                        LineTo (Point.create origin.X (boxY + swatchSize))
                        Close
                    ]

            let boxOpacity =
                Math.Clamp (float item.Color.A, 0.0, 1.0) * (if item.Visible then 1.0 else 0.3)

            let boxMark =
                Mark.path $"legend3d/swatch/{item.Id}/box" boxPath
                |> Mark.withFill (solidOpaquePaint item.Color)
                |> Mark.withStroke borderColor 0.75
                |> Mark.withOpacity boxOpacity

            let lblMark = Mark.text $"legend3d/swatch/{item.Id}/label" (Point.create (origin.X + swatchSize + 8.0) rowY) item.Label
            let lblStyle = Mark.textStyle lblMark

            let styledLbl =
                lblMark
                |> Mark.withFill textColor
                |> Mark.withFontSize 11.0
                |> Mark.withOpacity (if item.Visible then 1.0 else 0.45)
                |> Mark.withTextStyle { lblStyle with Anchor = "start"; Baseline = "middle" }

            marks.Add boxMark
            marks.Add styledLbl
            rowY <- rowY + rowStep

        Scene.compose
            { Width = viewportWidth; Height = viewportHeight }
            "Legend3D.Categorical"
            []
            (marks |> Seq.map Leaf |> Seq.toList)

    /// <summary>
    /// Combines multiple passive SVG <c>Scene</c> overlays into a single composed <c>Scene</c>, automatically deduplicating
    /// colliding definition keys and grouping colliding top-level nodes cleanly.
    /// </summary>
    let combine (viewportWidth: float) (viewportHeight: float) (scenes: Scene list) : Scene =
        Check3D.positive (nameof viewportWidth) viewportWidth
        Check3D.positive (nameof viewportHeight) viewportHeight

        if isNull (box scenes) then
            nullArg (nameof scenes)

        let combinedDefs = ResizeArray<SceneDefinition>()
        let usedDefKeys = HashSet<string>(StringComparer.Ordinal)
        let combinedNodes = ResizeArray<SceneNode>()
        let usedTopKeys = HashSet<string>(StringComparer.Ordinal)

        let uniqueKey (used: HashSet<string>) (baseKey: string) (tag: int) : string =
            if used.Add baseKey then
                baseKey
            else
                let mutable suffix = tag
                let mutable candidate = $"{baseKey}#{suffix}"
                while not (used.Add candidate) do
                    suffix <- suffix + 1
                    candidate <- $"{baseKey}#{suffix}"
                candidate

        let rewriteDef (newKey: string) (def: SceneDefinition) : SceneDefinition =
            match def with
            | SceneDefinition.RectClip (_, b) -> SceneDefinition.RectClip (newKey, b)
            | SceneDefinition.LinearGradient (_, a, b, stops) -> SceneDefinition.LinearGradient (newKey, a, b, stops)
            | SceneDefinition.RadialGradient (_, c, r, stops) -> SceneDefinition.RadialGradient (newKey, c, r, stops)

        let rec rewriteNode (remap: Dictionary<string, string>) (node: SceneNode) : SceneNode =
            match node with
            | Leaf mark ->
                match Mark.fillDefinition mark with
                | Some k ->
                    match remap.TryGetValue k with
                    | true, nextK -> Leaf (Mark.withFillDefinition nextK mark)
                    | _ -> node
                | None -> node
            | Group (spec, children) ->
                let nextSpec =
                    match spec.Clip with
                    | Some c ->
                        match remap.TryGetValue c with
                        | true, nextC -> { spec with Clip = Some nextC }
                        | _ -> spec
                    | None -> spec

                let nextChildren = children |> List.map (rewriteNode remap)
                Group (nextSpec, nextChildren)

        scenes
        |> List.iteri (fun sIdx sc ->
            if isNull (box sc) then
                nullArg "scenes"

            let remap = Dictionary<string, string>(StringComparer.Ordinal)

            for def in Scene.definitions sc do
                let origKey = SceneDefinition.key def
                let finalKey = uniqueKey usedDefKeys origKey sIdx

                if finalKey <> origKey then
                    remap[origKey] <- finalKey

                combinedDefs.Add (rewriteDef finalKey def)

            let rewritten = Scene.nodes sc |> List.map (rewriteNode remap)
            let collidesTop = rewritten |> List.exists (fun n -> usedTopKeys.Contains (SceneNode.key n))

            if not collidesTop then
                for n in rewritten do
                    usedTopKeys.Add (SceneNode.key n) |> ignore
                    combinedNodes.Add n
            elif not (List.isEmpty rewritten) then
                let groupKey = uniqueKey usedTopKeys $"legend3d/layer/{sIdx}" sIdx
                combinedNodes.Add (Group (GroupSpec.create groupKey, rewritten)))

        Scene.compose
            { Width = viewportWidth; Height = viewportHeight }
            "Legend3D.Combined"
            (Seq.toList combinedDefs)
            (Seq.toList combinedNodes)

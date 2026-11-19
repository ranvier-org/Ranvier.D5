namespace Ranvier.D5.Charts

open System
open Ranvier.D5

/// <summary>Specification for Lex et al. (2014) UpSet set-intersection charts.</summary>
type UpSetSpec<'Id when 'Id: comparison> =
    { /// <summary>Viewport, margins, and accessible chart title.</summary>
      Frame: ChartFrame
      /// <summary>Exclusive partition vs inclusive subset intersection mode.</summary>
      Mode: UpSetMode
      /// <summary>Column sort criterion.</summary>
      Sort: UpSetSort
      /// <summary>Maximum number of non-empty intersections to display.</summary>
      MaxIntersections: int
      /// <summary>Fill paint for intersection and marginal set cardinality bars.</summary>
      BarColor: Paint
      /// <summary>Fill and connector paint for participating sets in the combination matrix.</summary>
      ActiveDotColor: Paint
      /// <summary>Fill paint for non-participating matrix slots.</summary>
      InactiveDotColor: Paint }

/// <summary>Default constructors for <c>UpSetSpec</c>.</summary>
[<RequireQualifiedAccess>]
module UpSetSpec =
    /// <summary>Creates an exclusive cardinality-sorted UpSet chart specification showing up to 20 intersections.</summary>
    let create (frame: ChartFrame) : UpSetSpec<'Id> =
        { Frame = frame
          Mode = UpSetMode.Exclusive
          Sort = UpSetSort.ByCardinality
          MaxIntersections = 20
          BarColor = Paint.rgb 45 95 155
          ActiveDotColor = Paint.rgb 30 35 45
          InactiveDotColor = Paint.rgb 215 220 228 }

/// <summary>Specification for Wilkinson (2012) area-proportional circular Euler and Venn charts.</summary>
type EulerSpec =
    { /// <summary>Viewport, margins, and accessible chart title.</summary>
      Frame: ChartFrame
      /// <summary>Circle fill and stroke color mapped from set name.</summary>
      Color: string -> Paint
      /// <summary>Circle fill opacity in <c>[0, 1]</c>.</summary>
      Opacity: float }

/// <summary>Default constructors for <c>EulerSpec</c>.</summary>
[<RequireQualifiedAccess>]
module EulerSpec =
    /// <summary>Creates an Euler chart specification with categorical colors hashed from set names and 0.45 fill opacity.</summary>
    let create (frame: ChartFrame) : EulerSpec =
        let palette =
            Array.init 10 (fun i ->
                Chromatic.categorical Chromatic.category10 i
                |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip)
        { Frame = frame
          Opacity = 0.45
          Color = fun name ->
              let mutable h = 0u
              for c in name do
                  h <- h * 31u + uint32 c
              palette[int (h % 10u)] }

/// <summary>Lex et al. (2014) UpSet combination-matrix and cardinality bar charts.</summary>
[<RequireQualifiedAccess>]
module UpSetChart =
    let private toList (buf: ResizeArray<'T>) : 'T list =
        let mutable acc = []
        for i = buf.Count - 1 downto 0 do
            acc <- buf[i] :: acc
        acc

    /// <summary>Computes UpSet set intersections and renders vertical intersection bars, marginal set bars, and the connected dot matrix.</summary>
    let build (spec: UpSetSpec<'Id>) (sets: (string * 'Id list) list) : Result<ChartOutput, LayoutError list> =
        match ChartLayout.plot spec.Frame, UpSet.compute spec.Mode spec.Sort spec.MaxIntersections sets with
        | Error frameErrs, Error algErrs -> Error(frameErrs @ algErrs)
        | Error frameErrs, Ok _ -> Error frameErrs
        | Ok _, Error algErrs -> Error algErrs
        | Ok bounds, Ok summary ->
            try
                if summary.Sets.IsEmpty then
                    Ok(Recipe.output spec.Frame bounds [] [] [] [])
                else
                    let bx, by = Bounds.x bounds, Bounds.y bounds
                    let bw, bh = Bounds.width bounds, Bounds.height bounds

                    // Layout zones: left marginal set-size strip + right vertical intersection bars above combination matrix.
                    let leftWidth = bw * 0.22
                    let matrixLeft = bx + bw * 0.28
                    let matrixWidth = max 1. (bw * 0.72)
                    let barTop = by
                    let barHeight = max 1. (bh * 0.56)
                    let matrixTop = by + bh * 0.62
                    let matrixHeight = max 1. (bh * 0.38)

                    let setNames = summary.Sets |> List.map (fun s -> s.Name)
                    let rowScale = Scale.band setNames (matrixTop, matrixTop + matrixHeight) 0.2 0.1
                    let rowBand = Scale.bandwidth rowScale
                    let maxSetCount = summary.Sets |> List.fold (fun m s -> max m s.Count) 1
                    let setSizeScale = Scale.linear (0., float (max 1 maxSetCount)) (0., max 1. (leftWidth * 0.72))

                    let marks = ResizeArray<Mark>()
                    let semantics = ResizeArray<SemanticMark>()
                    let hits = ResizeArray<HitTarget>()

                    // 1. Marginal horizontal set-size bars & set row labels
                    let setBarRight = bx + leftWidth * 0.72
                    for setSum in summary.Sets do
                        let rowY = Scale.bandPosition rowScale setSum.Name |> Option.defaultValue matrixTop
                        let wBar = Scale.map setSizeScale (float setSum.Count)
                        let rect = Bounds.create (max bx (setBarRight - wBar)) rowY (max 0. wBar) (max 1. rowBand)
                        let barKey = Recipe.key [ "set-bar"; setSum.Name ]
                        let desc = sprintf "%s: %d items" setSum.Name setSum.Count
                        marks.Add(Recipe.rectangle barKey desc spec.BarColor rect)
                        semantics.Add(Recipe.semantic barKey [ setSum.Name ] desc)
                        hits.Add(Recipe.hit barKey [ setSum.Name ] (HitBounds rect))

                        let lblKey = Recipe.key [ "set-label"; setSum.Name ]
                        let lblPt = Point.create (setBarRight + 6.) (rowY + rowBand * 0.5)
                        let lblMark =
                            Mark.text lblKey lblPt setSum.Name
                            |> Mark.withFontSize 11.
                            |> fun m -> Mark.withTextStyle { Mark.textStyle m with Anchor = "start"; Baseline = "middle" } m
                        marks.Add lblMark

                    // 2. Intersection columns: vertical cardinality bars + combination matrix dots and connectors
                    let axes =
                        if summary.Intersections.IsEmpty then
                            []
                        else
                            let interIds = summary.Intersections |> List.map (fun inter -> inter.Id)
                            let colScale = Scale.band interIds (matrixLeft, matrixLeft + matrixWidth) 0.25 0.1
                            let colBand = Scale.bandwidth colScale
                            let maxInterCount = summary.Intersections |> List.fold (fun m i -> max m i.Count) 1
                            let yScale =
                                Scale.linear (0., float (max 1 maxInterCount)) (barTop + barHeight, barTop)
                                |> Scale.nice 4
                            let dotRadius = max 2. (min (min colBand rowBand * 0.32) 6.)
                            let dotArea = Math.PI * dotRadius * dotRadius
                            let dotPath = Shape.symbol Symbol.Circle dotArea

                            for inter in summary.Intersections do
                                let colX = Scale.bandPosition colScale inter.Id |> Option.defaultValue matrixLeft
                                let colCenterX = colX + colBand * 0.5
                                let yTop = Scale.map yScale (float inter.Count)
                                let yBot = Scale.map yScale 0.
                                let barRect =
                                    Bounds.create
                                        colX
                                        (min yTop yBot)
                                        (max 1. colBand)
                                        (max 0. (abs (yBot - yTop)))
                                let barKey = Recipe.key [ "bar"; inter.Id ]
                                let desc =
                                    sprintf
                                        "%s: %d items (degree %d, deviation %s)"
                                        inter.Id
                                        inter.Count
                                        inter.Degree
                                        (Recipe.number inter.Deviation)
                                marks.Add(Recipe.rectangle barKey desc spec.BarColor barRect)
                                semantics.Add(Recipe.semantic barKey [ inter.Id ] desc)
                                hits.Add(Recipe.hit barKey [ inter.Id ] (HitBounds barRect))

                                let activeSet = Set.ofList inter.Sets
                                let mutable minActiveY = Double.PositiveInfinity
                                let mutable maxActiveY = Double.NegativeInfinity

                                // Inactive background dots first, then connector line, then active foreground dots.
                                for setSum in summary.Sets do
                                    let rowY = Scale.bandPosition rowScale setSum.Name |> Option.defaultValue matrixTop
                                    let cy = rowY + rowBand * 0.5
                                    if Set.contains setSum.Name activeSet then
                                        if cy < minActiveY then minActiveY <- cy
                                        if cy > maxActiveY then maxActiveY <- cy
                                    else
                                        let dotKey = Recipe.key [ "dot"; inter.Id; setSum.Name ]
                                        let dotMark =
                                            Mark.path dotKey dotPath
                                            |> Mark.withTransform (Transform.translate colCenterX cy)
                                            |> Mark.withFill spec.InactiveDotColor
                                        marks.Add dotMark

                                if Double.IsFinite minActiveY && Double.IsFinite maxActiveY && maxActiveY > minActiveY then
                                    let linkKey = Recipe.key [ "link"; inter.Id ]
                                    let linkPath =
                                        Path.ofCommands
                                            [ MoveTo(Point.create colCenterX minActiveY)
                                              LineTo(Point.create colCenterX maxActiveY) ]
                                    let linkMark =
                                        Mark.path linkKey linkPath
                                        |> Mark.withFill Paint.none
                                        |> Mark.withStroke spec.ActiveDotColor 2.
                                    marks.Add linkMark

                                for setSum in summary.Sets do
                                    if Set.contains setSum.Name activeSet then
                                        let rowY = Scale.bandPosition rowScale setSum.Name |> Option.defaultValue matrixTop
                                        let cy = rowY + rowBand * 0.5
                                        let dotKey = Recipe.key [ "dot"; inter.Id; setSum.Name ]
                                        let dotMark =
                                            Mark.path dotKey dotPath
                                            |> Mark.withTransform (Transform.translate colCenterX cy)
                                            |> Mark.withFill spec.ActiveDotColor
                                        marks.Add dotMark

                            Axis.numeric "y" AxisOrientation.Left matrixLeft 4 yScale Recipe.number

                    Ok(
                        Recipe.output
                            spec.Frame
                            bounds
                            (toList marks)
                            (toList semantics)
                            (toList hits)
                            axes
                    )
            with :? ArgumentException as error ->
                Error [ InvalidObservation error.Message ]

/// <summary>Wilkinson (2012) area-proportional circular Euler and Venn charts.</summary>
[<RequireQualifiedAccess>]
module EulerChart =
    let private toList (buf: ResizeArray<'T>) : 'T list =
        let mutable acc = []
        for i = buf.Count - 1 downto 0 do
            acc <- buf[i] :: acc
        acc

    /// <summary>Computes an area-proportional circular Euler/Venn layout and renders translucent circles, labels, semantics, and disc hit targets.</summary>
    let build
        (spec: EulerSpec)
        (sets: (string * float) list)
        (pairwiseOverlaps: (string * string * float) list)
        : Result<ChartOutput, LayoutError list> =
        if not (Double.IsFinite spec.Opacity) || spec.Opacity < 0. || spec.Opacity > 1. then
            Error [ InvalidObservation "EulerSpec Opacity must be finite and in [0, 1]." ]
        else
            match ChartLayout.plot spec.Frame with
            | Error frameErrs -> Error frameErrs
            | Ok bounds ->
                match EulerVenn.layout bounds sets pairwiseOverlaps with
                | Error layoutErrs -> Error layoutErrs
                | Ok eulerLayout ->
                    try
                        let circleMarks = ResizeArray<Mark>(eulerLayout.Circles.Length)
                        let labelMarks = ResizeArray<Mark>(eulerLayout.Circles.Length)
                        let semantics = ResizeArray<SemanticMark>(eulerLayout.Circles.Length)
                        let hits = ResizeArray<HitTarget>(eulerLayout.Circles.Length)

                        for circle in eulerLayout.Circles do
                            let key = Recipe.key [ "euler"; circle.SetName ]
                            let desc = sprintf "%s: %s" circle.SetName (Recipe.number circle.Size)
                            let paint = spec.Color circle.SetName
                            let area = Math.PI * circle.Radius * circle.Radius
                            let cMark =
                                Shape.symbol Symbol.Circle area
                                |> Mark.path key
                                |> Mark.withTransform (Transform.translate circle.Center.X circle.Center.Y)
                                |> Mark.withFill paint
                                |> Mark.withOpacity spec.Opacity
                                |> Mark.withStroke paint 1.5
                                |> Mark.withLabel (Some desc)
                            circleMarks.Add cMark
                            semantics.Add(Recipe.semantic key [ circle.SetName ] desc)
                            hits.Add(Recipe.hit key [ circle.SetName ] (HitPoint(circle.Center, circle.Radius)))

                            let lblKey = Recipe.key [ "euler-label"; circle.SetName ]
                            let lblMark =
                                Mark.text lblKey circle.Center circle.SetName
                                |> Mark.withFontSize 12.
                                |> fun m -> Mark.withTextStyle { Mark.textStyle m with Anchor = "middle"; Baseline = "middle" } m
                            labelMarks.Add lblMark

                        let allMarks = toList circleMarks @ toList labelMarks
                        Ok(
                            Recipe.output
                                spec.Frame
                                bounds
                                allMarks
                                (toList semantics)
                                (toList hits)
                                []
                        )
                    with :? ArgumentException as error ->
                        Error [ InvalidObservation error.Message ]

/// <summary>Set-visualization chart recipes forwarding to <c>UpSetChart</c> and <c>EulerChart</c>.</summary>
[<RequireQualifiedAccess>]
module SetCharts =
    /// <summary>Builds an UpSet set-intersection chart.</summary>
    let upSet (spec: UpSetSpec<'Id>) (sets: (string * 'Id list) list) : Result<ChartOutput, LayoutError list> =
        UpSetChart.build spec sets

    /// <summary>Builds a circular area-proportional Euler/Venn chart.</summary>
    let euler
        (spec: EulerSpec)
        (sets: (string * float) list)
        (pairwiseOverlaps: (string * string * float) list)
        : Result<ChartOutput, LayoutError list> =
        EulerChart.build spec sets pairwiseOverlaps

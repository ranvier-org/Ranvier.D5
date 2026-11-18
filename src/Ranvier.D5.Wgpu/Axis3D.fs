namespace Ranvier.D5.Wgpu

open System
open System.Collections.Generic
open System.Globalization
open System.Numerics
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Logical-pixel margin space reserved around the 3D axis cuboid for tick labels and titles.</summary>
type Protrusions =
    {
        /// <summary>Left margin in logical pixels.</summary>
        Left: float
        /// <summary>Right margin in logical pixels.</summary>
        Right: float
        /// <summary>Top margin in logical pixels.</summary>
        Top: float
        /// <summary>Bottom margin in logical pixels.</summary>
        Bottom: float
    }

/// <summary>Default margin values for <c>Protrusions</c>.</summary>
[<RequireQualifiedAccess>]
module Protrusions =
    /// <summary>Standard 3D axis margins matching Makie-style tick/title clearance.</summary>
    let defaultValue: Protrusions =
        {
            Left = 36.0
            Right = 36.0
            Top = 28.0
            Bottom = 32.0
        }

/// <summary>Identifies one of the three data-space coordinate axes.</summary>
[<RequireQualifiedAccess; Struct>]
type AxisId3D =
    /// <summary>Horizontal X data axis.</summary>
    | X
    /// <summary>Y data axis (depth under <c>ZUp</c>, vertical under <c>YUp</c>).</summary>
    | Y
    /// <summary>Z data axis (vertical under <c>ZUp</c>, depth under <c>YUp</c>).</summary>
    | Z

/// <summary>Per-axis title, tick density, gridline, background-panel, and formatter configuration.</summary>
type AxisConfig3D =
    {
        /// <summary>Display title placed outside the axis silhouette spine.</summary>
        Title: string
        /// <summary>Target number of nice tick marks along the axis (<c>0</c> omits ticks).</summary>
        TickCount: int
        /// <summary>Outward 3D tick hairline length in world units.</summary>
        TickLengthWorld: float32
        /// <summary>When true, draws major gridlines across the two adjacent back walls.</summary>
        ShowGrid: bool
        /// <summary>When true, emits the background panel quad on the perpendicular back wall.</summary>
        ShowPanel: bool
        /// <summary>Optional custom tick label formatter; defaults to culture-invariant numeric or UTC time formatting.</summary>
        FormatTick: (float -> string) option
    }

/// <summary>Colors and typography sizes shared across 3D gridlines, spines, panels, and SVG overlay labels.</summary>
type AxisTheme3D =
    {
        /// <summary>RGBA color for back-wall gridline segments.</summary>
        GridColor: RgbaColor
        /// <summary>RGBA color for cuboid spine edges.</summary>
        SpineColor: RgbaColor
        /// <summary>RGBA color for outward tick hairline segments.</summary>
        TickColor: RgbaColor
        /// <summary>RGBA color for unlit back-wall panel quads.</summary>
        PanelColor: RgbaColor
        /// <summary>CSS paint fill for projected tick labels in the SVG overlay.</summary>
        LabelColor: Paint
        /// <summary>CSS paint fill for projected axis titles in the SVG overlay.</summary>
        TitleColor: Paint
        /// <summary>Font size in scene units for tick labels.</summary>
        FontSize: float
        /// <summary>Font size in scene units for axis titles.</summary>
        TitleFontSize: float
    }

/// <summary>Complete immutable specification required to frame a 3D coordinate cuboid from a camera pose.</summary>
type Axis3DSpec =
    {
        /// <summary>Data-to-world coordinate scale and box aspect policy.</summary>
        Scale: Scale3D
        /// <summary>Configuration for the X data axis and constant-X back panel.</summary>
        X: AxisConfig3D
        /// <summary>Configuration for the Y data axis and constant-Y back panel.</summary>
        Y: AxisConfig3D
        /// <summary>Configuration for the Z data axis and constant-Z back panel.</summary>
        Z: AxisConfig3D
        /// <summary>Colors and typography settings for 3D geometry and 2D labels.</summary>
        Theme: AxisTheme3D
        /// <summary>Logical-pixel margins reserved around the plot cuboid.</summary>
        Protrusions: Protrusions
        /// <summary>When true, also draws the 3 frontmost cuboid edges meeting at the camera-facing corner.</summary>
        ShowFrontSpines: bool
    }

/// <summary>Projected 2D screen-space label descriptor carrying a stable semantic key for DOM reconciliation.</summary>
type AxisLabel2D =
    {
        /// <summary>Stable semantic mark key (for example <c>"axis/x/tick/0"</c> or <c>"axis/x/title"</c>).</summary>
        Key: string
        /// <summary>Data axis owning this label.</summary>
        Axis: AxisId3D
        /// <summary>True for the axis title; false for numeric/category/time tick labels.</summary>
        IsTitle: bool
        /// <summary>Projected 2D viewport position in logical/pixel scene coordinates.</summary>
        ScreenPosition: Point
        /// <summary>SVG <c>text-anchor</c> alignment (<c>"start"</c>, <c>"middle"</c>, or <c>"end"</c>).</summary>
        Anchor: string
        /// <summary>Formatted label string.</summary>
        Text: string
    }

/// <summary>Immutable camera-classified 3D axis frame holding wall quads, line vertices, projected labels, and active back-wall planes.</summary>
type AxisFrame3D =
    private
        {
            Spec: Axis3DSpec
            Lines: LineVertex3D array
            Panels: Instance3D array
            LabelsList: AxisLabel2D list
            BackWalls: struct (float32 * float32 * float32)
        }

/// <summary>Camera-aware 3D axis framing, back-wall panel classification, silhouette tick placement, and SVG overlay construction.</summary>
[<RequireQualifiedAccess>]
module Axis3D =
    [<Struct>]
    type private WorldAxis =
        | WorldX
        | WorldY
        | WorldZ

    let private defaultAxisConfig (title: string) : AxisConfig3D =
        {
            Title = title
            TickCount = 5
            TickLengthWorld = 0.05f
            ShowGrid = true
            ShowPanel = true
            FormatTick = None
        }

    let private defaultTheme: AxisTheme3D =
        {
            GridColor = RgbaColor (0.35f, 0.42f, 0.56f, 0.28f)
            SpineColor = RgbaColor (0.58f, 0.65f, 0.78f, 0.90f)
            TickColor = RgbaColor (0.72f, 0.78f, 0.88f, 0.95f)
            PanelColor = RgbaColor (0.06f, 0.09f, 0.14f, 0.55f)
            LabelColor = Paint.rgb 203 213 225
            TitleColor = Paint.rgb 241 245 249
            FontSize = 11.0
            TitleFontSize = 13.0
        }

    /// <summary>Creates a default <c>Axis3DSpec</c> with titles <c>"X"</c>, <c>"Y"</c>, <c>"Z"</c>, 5 ticks, grids and panels enabled, and front spines off.</summary>
    let defaultSpec (scale: Scale3D) : Axis3DSpec =
        {
            Scale = scale
            X = defaultAxisConfig "X"
            Y = defaultAxisConfig "Y"
            Z = defaultAxisConfig "Z"
            Theme = defaultTheme
            Protrusions = Protrusions.defaultValue
            ShowFrontSpines = false
        }

    let private validateConfig (name: string) (cfg: AxisConfig3D) =
        if isNull cfg.Title then
            nullArg $"{name}.Title"

        if cfg.TickCount < 0 then
            raise (ArgumentOutOfRangeException ($"{name}.TickCount", "TickCount must be nonnegative."))

        Check3D.nonNegative32 $"{name}.TickLengthWorld" cfg.TickLengthWorld

    let private validateSpec (spec: Axis3DSpec) =
        validateConfig "X" spec.X
        validateConfig "Y" spec.Y
        validateConfig "Z" spec.Z
        Check3D.positive "Theme.FontSize" spec.Theme.FontSize
        Check3D.positive "Theme.TitleFontSize" spec.Theme.TitleFontSize
        Check3D.nonNegative "Protrusions.Left" spec.Protrusions.Left
        Check3D.nonNegative "Protrusions.Right" spec.Protrusions.Right
        Check3D.nonNegative "Protrusions.Top" spec.Protrusions.Top
        Check3D.nonNegative "Protrusions.Bottom" spec.Protrusions.Bottom

    let private classifyBackSign (isOrtho: bool) (viewDirComp: float32) (camPosComp: float32) (halfExt: float32) =
        if isOrtho then
            // Dot(+1, viewDir) = viewDirComp, Dot(-1, viewDir) = -viewDirComp.
            if viewDirComp <= 0.0f then
                struct (-1.0f, Vector3.UnitX, viewDirComp)
            else
                struct (1.0f, -Vector3.UnitX, -viewDirComp)
        else
            let dotNeg = -halfExt - camPosComp
            let dotPos = camPosComp - halfExt

            if dotNeg <= dotPos then
                struct (-1.0f, Vector3.UnitX, dotNeg)
            else
                struct (1.0f, -Vector3.UnitX, dotPos)

    let private axisPrefix =
        function
        | AxisId3D.X -> "x"
        | AxisId3D.Y -> "y"
        | AxisId3D.Z -> "z"

    let private formatDefaultTicks (axisScale: AxisScale3D) (ticks: float list) : string list =
        match axisScale with
        | AxisScale3D.TimeUtc _ ->
            let pattern =
                match ticks with
                | first :: rest when not (List.isEmpty rest) ->
                    let spanMs = abs (List.last rest - first)

                    if spanMs >= 2.0 * 86_400_000.0 then "yyyy-MM-dd"
                    elif spanMs >= 3_600_000.0 then "MM-dd HH:mm"
                    else "HH:mm:ss"
                | _ -> "yyyy-MM-dd"

            ticks
            |> List.map (fun ms ->
                let clamped = Math.Clamp (Math.Round ms, -62135596800000.0, 253402300799999.0)
                let dt = DateTimeOffset.FromUnixTimeMilliseconds (int64 clamped)
                Format.time CultureInfo.InvariantCulture TimeZoneInfo.Utc pattern dt)
        | _ ->
            let sorted = ticks |> List.distinct |> List.sort

            let minStep =
                match sorted with
                | _ :: _ :: _ ->
                    sorted
                    |> List.pairwise
                    |> List.map (fun (a, b) -> b - a)
                    |> List.filter (fun d -> d > 0.0)
                    |> function
                        | [] -> 0.0
                        | ds -> List.min ds
                | _ -> 0.0

            let allIntegers =
                ticks
                |> List.forall (fun v -> abs (v - Math.Round v) <= 1e-9 * max 1.0 (abs v))

            let precision =
                if allIntegers then
                    0
                elif minStep > 0.0 then
                    int (ceil (-Math.Log10 minStep + 1e-9)) |> max 0 |> min 6
                else
                    2

            ticks
            |> List.map (fun v ->
                let clean = if abs v < 1e-12 then 0.0 else v

                if abs clean >= 1e6 || (clean <> 0.0 && abs clean < 1e-4) then
                    Format.si CultureInfo.InvariantCulture 2 clean
                else
                    Format.number CultureInfo.InvariantCulture precision clean)

    let private worldPointForAxis (worldAxis: WorldAxis) (hx: float32) (hy: float32) (hz: float32) (tWorld: float32) (uSign: float32) (vSign: float32) =
        let c = tWorld - 0.5f

        match worldAxis with
        | WorldX -> Vector3 (c * 2.0f * hx, uSign * hy, vSign * hz)
        | WorldY -> Vector3 (uSign * hx, c * 2.0f * hy, vSign * hz)
        | WorldZ -> Vector3 (uSign * hx, vSign * hy, c * 2.0f * hz)

    let private outwardNormalForAxis (worldAxis: WorldAxis) (uSign: float32) (vSign: float32) =
        let raw =
            match worldAxis with
            | WorldX -> Vector3 (0.0f, uSign, vSign)
            | WorldY -> Vector3 (uSign, 0.0f, vSign)
            | WorldZ -> Vector3 (uSign, vSign, 0.0f)

        Vector3.Normalize raw

    let private selectSilhouetteEdge
        (viewMat: Matrix4x4)
        (isVertical: bool)
        (worldAxis: WorldAxis)
        (hx: float32)
        (hy: float32)
        (hz: float32)
        (backU: float32)
        (backV: float32)
        : struct (float32 * float32 * float32 * float32) =
        // Silhouette edges lie at (backU, -backV) and (-backU, backV), shared by 1 back face and 1 front face.
        let u1, v1 = backU, -backV
        let u2, v2 = -backU, backV

        let n1 = outwardNormalForAxis worldAxis u1 v1
        let n2 = outwardNormalForAxis worldAxis u2 v2
        let vn1 = Vector3.TransformNormal (n1, viewMat)
        let vn2 = Vector3.TransformNormal (n2, viewMat)

        let m1 = worldPointForAxis worldAxis hx hy hz 0.5f u1 v1
        let m2 = worldPointForAxis worldAxis hx hy hz 0.5f u2 v2
        let vm1 = Vector3.Transform (m1, viewMat)
        let vm2 = Vector3.Transform (m2, viewMat)

        let chooseFirst =
            if isVertical then
                if abs (vn1.X - vn2.X) > 1e-4f then vn1.X < vn2.X
                elif abs (vm1.X - vm2.X) > 1e-4f then vm1.X < vm2.X
                else vn1.Y < vn2.Y
            else
                if abs (vn1.Y - vn2.Y) > 1e-4f then vn1.Y < vn2.Y
                elif abs (vm1.Y - vm2.Y) > 1e-4f then vm1.Y < vm2.Y
                else vn1.X < vn2.X

        if chooseFirst then
            struct (u1, v1, u2, v2)
        else
            struct (u2, v2, u1, v1)

    let private makePanelInstance
        (center: Vector3)
        (u: Vector3)
        (spanU: float32)
        (v: Vector3)
        (spanV: float32)
        (inwardNormal: Vector3)
        (thickness: float32)
        (color: RgbaColor)
        : Instance3D =
        let c = center - inwardNormal * (thickness * 0.5f)
        let uS = u * spanU
        let vS = v * spanV
        let nS = inwardNormal * thickness

        let m =
            Matrix4x4 (
                uS.X,
                uS.Y,
                uS.Z,
                0.0f,
                vS.X,
                vS.Y,
                vS.Z,
                0.0f,
                nS.X,
                nS.Y,
                nS.Z,
                0.0f,
                c.X,
                c.Y,
                c.Z,
                1.0f
            )

        Instance3D.fromMatrix m color 0.5f 0u (InstanceFlags.Unlit ||| InstanceFlags.IgnoreSectionPlane) 0.0f

    let private addSegment (verts: ResizeArray<LineVertex3D>) (a: Vector3) (b: Vector3) (color: RgbaColor) =
        verts.Add (LineVertex3D (a, color))
        verts.Add (LineVertex3D (b, color))

    let private projectLabel
        (width: int)
        (height: int)
        (camera: Camera3D)
        (viewMat: Matrix4x4)
        (anchorWorld: Vector3)
        (outwardDir: Vector3)
        (tickLen: float32)
        (padPx: float32)
        (isVertical: bool)
        : struct (Point * string) voption =
        match Camera3D.projectToScreen width height camera anchorWorld with
        | ValueNone -> ValueNone
        | ValueSome sBase ->
            let tipWorld = anchorWorld + outwardDir * (max 0.02f tickLen)

            let fallbackDir2D () =
                let vn = Vector3.TransformNormal (outwardDir, viewMat)
                let v2 = Vector2 (vn.X, -vn.Y)
                if v2.LengthSquared () > 1e-4f then Vector2.Normalize v2 else Vector2.UnitY

            let dir2D =
                match Camera3D.projectToScreen width height camera tipWorld with
                | ValueSome sTip ->
                    let d = sTip - sBase
                    if d.LengthSquared () > 1e-4f then Vector2.Normalize d else fallbackDir2D ()
                | ValueNone -> fallbackDir2D ()

            let pos = sBase + dir2D * padPx

            if Check3D.isFinite32 pos.X && Check3D.isFinite32 pos.Y then
                let anchor =
                    if isVertical then
                        if dir2D.X < -0.15f then "end"
                        elif dir2D.X > 0.15f then "start"
                        else "middle"
                    else
                        if dir2D.X < -0.30f then "end"
                        elif dir2D.X > 0.30f then "start"
                        else "middle"

                ValueSome struct (Point.create (float pos.X) (float pos.Y), anchor)
            else
                ValueNone

    /// <summary>Classifies the 6 cuboid faces and outer silhouette tick spines from <c>camera</c>, producing panel instances, line vertices, and projected labels.</summary>
    let build (spec: Axis3DSpec) (viewportWidth: int) (viewportHeight: int) (camera: Camera3D) : AxisFrame3D =
        validateSpec spec

        if viewportWidth <= 0 then
            raise (ArgumentOutOfRangeException (nameof viewportWidth, "Viewport width must be positive."))

        if viewportHeight <= 0 then
            raise (ArgumentOutOfRangeException (nameof viewportHeight, "Viewport height must be positive."))

        Check3D.vector3 "camera.Position" camera.Position
        Check3D.vector3 "camera.Target" camera.Target
        Check3D.vector3 "camera.Up" camera.Up

        let ext = Scale3D.worldExtents spec.Scale
        let hx, hy, hz = ext.X * 0.5f, ext.Y * 0.5f, ext.Z * 0.5f
        let isOrtho = camera.FovYDeg < 0.0f

        let rawViewDir = camera.Target - camera.Position

        if rawViewDir.LengthSquared () <= 1e-12f then
            invalidArg "camera" "Camera Position and Target must differ."

        let viewDir = Vector3.Normalize rawViewDir

        // Classify back sign per world axis: -1.0f means the -halfExt face is the active back wall.
        let struct (bxSign, _, dotX) = classifyBackSign isOrtho viewDir.X camera.Position.X hx
        let struct (bySign, _, dotY) = classifyBackSign isOrtho viewDir.Y camera.Position.Y hy
        let struct (bzSign, _, dotZ) = classifyBackSign isOrtho viewDir.Z camera.Position.Z hz

        let worldBackX = bxSign * hx
        let worldFloorY = bySign * hy
        let worldBackZ = bzSign * hz

        // Build up to 3 back-facing wall panels with local +Z aligned with inward box-interior normal.
        let panels = ResizeArray<Instance3D>(3)
        let minPosExt = [ ext.X; ext.Y; ext.Z ] |> List.filter (fun v -> v > 0.0f) |> function [] -> 1.0f | xs -> List.min xs
        let thickness = max 1e-4f (min 0.004f (0.01f * minPosExt))
        let panelColor = spec.Theme.PanelColor

        let upAxis = spec.Scale.UpAxis
        let showPanelWorldX = spec.X.ShowPanel

        let showPanelWorldY =
            match upAxis with
            | UpAxis.ZUp -> spec.Z.ShowPanel
            | UpAxis.YUp -> spec.Y.ShowPanel

        let showPanelWorldZ =
            match upAxis with
            | UpAxis.ZUp -> spec.Y.ShowPanel
            | UpAxis.YUp -> spec.Z.ShowPanel

        let addPanel show spanU spanV dotVal center u v nIn =
            if show && spanU > 1e-6f && spanV > 1e-6f && dotVal < -1e-6f then
                panels.Add (makePanelInstance center u spanU v spanV nIn thickness panelColor)

        if bxSign < 0.0f then
            addPanel showPanelWorldX ext.Y ext.Z dotX (Vector3 (worldBackX, 0.0f, 0.0f)) Vector3.UnitY Vector3.UnitZ (Vector3 (-bxSign, 0.0f, 0.0f))
        else
            addPanel showPanelWorldX ext.Z ext.Y dotX (Vector3 (worldBackX, 0.0f, 0.0f)) Vector3.UnitZ Vector3.UnitY (Vector3 (-bxSign, 0.0f, 0.0f))

        if bySign < 0.0f then
            addPanel showPanelWorldY ext.Z ext.X dotY (Vector3 (0.0f, worldFloorY, 0.0f)) Vector3.UnitZ Vector3.UnitX (Vector3 (0.0f, -bySign, 0.0f))
        else
            addPanel showPanelWorldY ext.X ext.Z dotY (Vector3 (0.0f, worldFloorY, 0.0f)) Vector3.UnitX Vector3.UnitZ (Vector3 (0.0f, -bySign, 0.0f))

        if bzSign < 0.0f then
            addPanel showPanelWorldZ ext.X ext.Y dotZ (Vector3 (0.0f, 0.0f, worldBackZ)) Vector3.UnitX Vector3.UnitY (Vector3 (0.0f, 0.0f, -bzSign))
        else
            addPanel showPanelWorldZ ext.Y ext.X dotZ (Vector3 (0.0f, 0.0f, worldBackZ)) Vector3.UnitY Vector3.UnitX (Vector3 (0.0f, 0.0f, -bzSign))

        let viewMat = Camera3D.viewMatrix camera
        let lines = ResizeArray<LineVertex3D>(128)
        let labelsAcc = ResizeArray<AxisLabel2D>(32)

        let dataAxes =
            match upAxis with
            | UpAxis.ZUp ->
                [
                    AxisId3D.X, WorldX, false, spec.Scale.XScale, spec.X, bySign, bzSign, true
                    AxisId3D.Y, WorldZ, false, spec.Scale.YScale, spec.Y, bxSign, bySign, false
                    AxisId3D.Z, WorldY, true, spec.Scale.ZScale, spec.Z, bxSign, bzSign, true
                ]
            | UpAxis.YUp ->
                [
                    AxisId3D.X, WorldX, false, spec.Scale.XScale, spec.X, bySign, bzSign, true
                    AxisId3D.Y, WorldY, true, spec.Scale.YScale, spec.Y, bxSign, bzSign, true
                    AxisId3D.Z, WorldZ, false, spec.Scale.ZScale, spec.Z, bxSign, bySign, true
                ]

        for (axisId, worldAxis, isVertical, axisScale, cfg, backU, backV, positiveDir) in dataAxes do
            let struct (tickU, tickV, oppU, oppV) = selectSilhouetteEdge viewMat isVertical worldAxis hx hy hz backU backV
            let outwardDir = outwardNormalForAxis worldAxis tickU tickV

            let p0 sU sV = worldPointForAxis worldAxis hx hy hz 0.0f sU sV
            let p1 sU sV = worldPointForAxis worldAxis hx hy hz 1.0f sU sV

            // Outer silhouette tick spine (spine 1), interior back-corner spine (spine 2), opposite silhouette spine (spine 3).
            addSegment lines (p0 tickU tickV) (p1 tickU tickV) spec.Theme.SpineColor
            addSegment lines (p0 backU backV) (p1 backU backV) spec.Theme.SpineColor
            addSegment lines (p0 oppU oppV) (p1 oppU oppV) spec.Theme.SpineColor

            if spec.ShowFrontSpines then
                addSegment lines (p0 (-backU) (-backV)) (p1 (-backU) (-backV)) spec.Theme.SpineColor

            let rawTicks = AxisScale3D.ticks cfg.TickCount axisScale

            let validTicks =
                rawTicks
                |> List.choose (fun v ->
                    match AxisScale3D.tryMapNormalized axisScale v with
                    | ValueSome t when t >= -1e-5 && t <= 1.0 + 1e-5 -> Some (v, Math.Clamp (t, 0.0, 1.0))
                    | _ -> None)

            let formattedTicks =
                match cfg.FormatTick with
                | Some fmt -> validTicks |> List.map (fst >> fmt)
                | None -> formatDefaultTicks axisScale (validTicks |> List.map fst)

            let prefix = axisPrefix axisId

            validTicks
            |> List.zip formattedTicks
            |> List.iteri (fun idx (text, (_, tNorm)) ->
                let tWorld = if positiveDir then float32 tNorm else 1.0f - float32 tNorm

                // Back-wall gridlines on the two back walls containing this axis.
                if cfg.ShowGrid && tNorm > 1e-4 && tNorm < 1.0 - 1e-4 then
                    let gCorner = worldPointForAxis worldAxis hx hy hz tWorld backU backV
                    let gEdgeU = worldPointForAxis worldAxis hx hy hz tWorld (-backU) backV
                    let gEdgeV = worldPointForAxis worldAxis hx hy hz tWorld backU (-backV)
                    addSegment lines gCorner gEdgeU spec.Theme.GridColor
                    addSegment lines gCorner gEdgeV spec.Theme.GridColor

                // Outward tick hairline on the single selected outer silhouette spine.
                let tickStart = worldPointForAxis worldAxis hx hy hz tWorld tickU tickV
                let tickEnd = tickStart + outwardDir * cfg.TickLengthWorld

                if cfg.TickLengthWorld > 0.0f then
                    addSegment lines tickStart tickEnd spec.Theme.TickColor

                match projectLabel viewportWidth viewportHeight camera viewMat tickEnd outwardDir cfg.TickLengthWorld 8.0f isVertical with
                | ValueSome struct (screenPt, anchor) ->
                    labelsAcc.Add
                        {
                            Key = $"axis/{prefix}/tick/{idx}"
                            Axis = axisId
                            IsTitle = false
                            ScreenPosition = screenPt
                            Anchor = anchor
                            Text = text
                        }
                | ValueNone -> ())

            if not (String.IsNullOrEmpty cfg.Title) then
                let midSpine = worldPointForAxis worldAxis hx hy hz 0.5f tickU tickV
                let midTickTip = midSpine + outwardDir * cfg.TickLengthWorld
                let titlePad = float32 (max 20.0 (min spec.Protrusions.Bottom 30.0))

                match projectLabel viewportWidth viewportHeight camera viewMat midTickTip outwardDir cfg.TickLengthWorld titlePad isVertical with
                | ValueSome struct (screenPt, anchor) ->
                    labelsAcc.Add
                        {
                            Key = $"axis/{prefix}/title"
                            Axis = axisId
                            IsTitle = true
                            ScreenPosition = screenPt
                            Anchor = anchor
                            Text = cfg.Title
                        }
                | ValueNone -> ()

        {
            Spec = spec
            Lines = lines.ToArray ()
            Panels = panels.ToArray ()
            LabelsList = Seq.toList labelsAcc
            BackWalls = struct (worldBackX, worldFloorY, worldBackZ)
        }

    /// <summary>Returns the contiguous line-segment endpoints for back-wall gridlines, cuboid spines, and outward tick hairlines.</summary>
    let lineVertices (frame: AxisFrame3D) : ReadOnlyMemory<LineVertex3D> =
        ReadOnlyMemory<LineVertex3D>(frame.Lines)

    /// <summary>
    /// Returns up to 3 unlit, section-exempt <c>Instance3D</c> transforms for the camera-backfacing cuboid walls,
    /// designed for <c>Primitives.unitBox()</c> with local <c>+Z</c> aligned to each wall's inward box-interior normal.
    /// </summary>
    let panelInstances (frame: AxisFrame3D) : ReadOnlyMemory<Instance3D> =
        ReadOnlyMemory<Instance3D>(frame.Panels)

    /// <summary>Returns the projected 2D tick and title label descriptors with stable semantic keys.</summary>
    let labels (frame: AxisFrame3D) : IReadOnlyList<AxisLabel2D> =
        frame.LabelsList :> IReadOnlyList<AxisLabel2D>

    /// <summary>Builds a passive <c>Ranvier.D5.Scene</c> of SVG text marks with stable keys (<c>"axis/x/tick/0"</c>, <c>"axis/x/title"</c>, etc.).</summary>
    let overlayScene (viewportWidth: float) (viewportHeight: float) (frame: AxisFrame3D) : Scene =
        let theme = frame.Spec.Theme

        let marks =
            frame.LabelsList
            |> List.map (fun lbl ->
                let baseMark = Mark.text lbl.Key lbl.ScreenPosition lbl.Text
                let currentStyle = Mark.textStyle baseMark

                let updatedStyle =
                    { currentStyle with
                        Anchor = lbl.Anchor
                        Baseline = "middle"
                        Weight = if lbl.IsTitle then 600 else 400
                    }

                baseMark
                |> Mark.withFill (if lbl.IsTitle then theme.TitleColor else theme.LabelColor)
                |> Mark.withFontSize (if lbl.IsTitle then theme.TitleFontSize else theme.FontSize)
                |> Mark.withTextStyle updatedStyle)

        Scene.create viewportWidth viewportHeight "Axis3D" marks

    /// <summary>Returns the active camera-backfacing world wall coordinates <c>struct (worldBackX, worldFloorY, worldBackZ)</c> for hover spikes and wall projections.</summary>
    let backWallPlanes (frame: AxisFrame3D) : struct (float32 * float32 * float32) =
        frame.BackWalls

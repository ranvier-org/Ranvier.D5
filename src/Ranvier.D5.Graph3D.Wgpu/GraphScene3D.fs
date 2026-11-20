namespace Ranvier.D5.Graph3D.Wgpu

open System
open System.Collections.Generic
open System.Numerics
open Ranvier.D5
open Ranvier.D5.Graph
open Ranvier.D5.Graph3D
open Ranvier.D5.Wgpu
open Ranvier.Wgpu

/// <summary>Visual styling configuration mapping 3D diagram nodes, edges, and compound groups to GPU marks.</summary>
[<NoEquality; NoComparison>]
type GraphTheme3D =
    {
        /// <summary>Unit glyph mesh primitive used to render positioned graph nodes.</summary>
        NodeShape: GlyphShape3D
        /// <summary>Callback computing base RGBA color for each positioned node.</summary>
        NodeColor: PositionedNode3D -> RgbaColor
        /// <summary>Highlight RGBA color applied to selected node IDs.</summary>
        NodeSelectedColor: RgbaColor
        /// <summary>Multiplier applied to normalized world node radii.</summary>
        NodeRadiusScale: float32
        /// <summary>Surface shading mode for node glyphs.</summary>
        NodeShading: ShadingMode
        /// <summary>Additional emissive glow boost applied to selected nodes when lit.</summary>
        SelectedEmissive: float32
        /// <summary>Color assigned to spanning-tree and DAG backbone edges.</summary>
        BackboneEdgeColor: RgbaColor
        /// <summary>Color assigned to secondary and cross-link edges.</summary>
        SecondaryEdgeColor: RgbaColor
        /// <summary>World-space cylinder radius for extruded backbone edge tubes.</summary>
        BackboneTubeRadius: float32
        /// <summary>World-space cylinder radius for extruded secondary edge tubes.</summary>
        SecondaryTubeRadius: float32
        /// <summary>When true in reactive bindings, extrudes edges as shaded cylindrical tubes via <c>Line3D.tubes</c> instead of hairlines.</summary>
        UseEdgeTubes: bool
        /// <summary>Radial cross-section side count when <c>UseEdgeTubes = true</c> (at least 3).</summary>
        TubeSides: int
        /// <summary>Optional converter turning each compound <c>PositionedGroup3D</c> into a translucent container <c>BoxDatum3D</c>.</summary>
        GroupBox: PositionedGroup3D -> BoxDatum3D voption
        /// <summary>When true, emits 12-edge wireframes around each compound group cuboid.</summary>
        ShowGroupWireframe: bool
        /// <summary>Wireframe color for compound group cuboids.</summary>
        GroupWireframeColor: RgbaColor
        /// <summary>When true, exempts emitted group wireframe edges from active <c>SectionPlane</c> clipping.</summary>
        IgnoreGroupSectionPlane: bool
    }

/// <summary>Preset themes and pipeable modifiers for <c>GraphTheme3D</c>.</summary>
[<RequireQualifiedAccess>]
module GraphTheme3D =
    let private defaultDarkNodeColor (nd: PositionedNode3D) : RgbaColor =
        match nd.Kind with
        | NodeKind.InitialState -> RgbaColor (0.32f, 0.94f, 0.64f, 1.0f)
        | NodeKind.FinalState -> RgbaColor (0.98f, 0.45f, 0.56f, 1.0f)
        | NodeKind.Choice -> RgbaColor (0.98f, 0.78f, 0.28f, 1.0f)
        | NodeKind.Junction -> RgbaColor (0.76f, 0.54f, 0.98f, 1.0f)
        | NodeKind.AssociationHub -> RgbaColor (0.35f, 0.74f, 0.98f, 1.0f)
        | NodeKind.Standard ->
            match nd.Layer with
            | Some depth ->
                let palette = ColorRamp.tableau10
                palette[abs depth % palette.Length]
            | None -> RgbaColor (0.28f, 0.84f, 0.96f, 1.0f)

    let private defaultLightNodeColor (nd: PositionedNode3D) : RgbaColor =
        match nd.Kind with
        | NodeKind.InitialState -> RgbaColor (0.06f, 0.62f, 0.38f, 1.0f)
        | NodeKind.FinalState -> RgbaColor (0.78f, 0.16f, 0.28f, 1.0f)
        | NodeKind.Choice -> RgbaColor (0.82f, 0.48f, 0.04f, 1.0f)
        | NodeKind.Junction -> RgbaColor (0.48f, 0.24f, 0.78f, 1.0f)
        | NodeKind.AssociationHub -> RgbaColor (0.12f, 0.44f, 0.76f, 1.0f)
        | NodeKind.Standard ->
            match nd.Layer with
            | Some depth ->
                let palette = ColorRamp.okabeIto
                palette[abs depth % palette.Length]
            | None -> RgbaColor (0.12f, 0.45f, 0.78f, 1.0f)

    let private defaultDarkGroupBox (g: PositionedGroup3D) : BoxDatum3D voption =
        let depthTint = float32 (min 4 (max 0 g.Depth)) * 0.06f
        let col = RgbaColor (0.20f + depthTint, 0.34f + depthTint, 0.54f, 0.14f)
        ValueSome (BoxDatum3D.create g.Bounds col |> BoxDatum3D.withShading ShadingMode.Unlit)

    let private defaultLightGroupBox (g: PositionedGroup3D) : BoxDatum3D voption =
        let depthTint = float32 (min 4 (max 0 g.Depth)) * 0.05f
        let col = RgbaColor (0.18f + depthTint, 0.30f + depthTint, 0.48f, 0.12f)
        ValueSome (BoxDatum3D.create g.Bounds col |> BoxDatum3D.withShading ShadingMode.Unlit)

    /// <summary>High-contrast dark-backdrop theme pairing glowing cyan/amber nodes with translucent slate group boxes.</summary>
    let darkDefault: GraphTheme3D =
        {
            NodeShape = GlyphShape3D.Sphere
            NodeColor = defaultDarkNodeColor
            NodeSelectedColor = RgbaColor (1.0f, 0.92f, 0.42f, 1.0f)
            NodeRadiusScale = 1.0f
            NodeShading = ShadingMode.Lit (0.28f, 0.08f)
            SelectedEmissive = 0.32f
            BackboneEdgeColor = RgbaColor (0.35f, 0.85f, 0.98f, 0.92f)
            SecondaryEdgeColor = RgbaColor (0.55f, 0.62f, 0.82f, 0.38f)
            BackboneTubeRadius = 0.014f
            SecondaryTubeRadius = 0.007f
            UseEdgeTubes = false
            TubeSides = 6
            GroupBox = defaultDarkGroupBox
            ShowGroupWireframe = true
            GroupWireframeColor = RgbaColor (0.55f, 0.68f, 0.88f, 0.42f)
            IgnoreGroupSectionPlane = false
        }

    /// <summary>Clean light-backdrop theme pairing crisp categorical node colors with slate edges and wireframes.</summary>
    let lightDefault: GraphTheme3D =
        {
            NodeShape = GlyphShape3D.Sphere
            NodeColor = defaultLightNodeColor
            NodeSelectedColor = RgbaColor (0.95f, 0.32f, 0.08f, 1.0f)
            NodeRadiusScale = 1.0f
            NodeShading = ShadingMode.Lit (0.34f, 0.02f)
            SelectedEmissive = 0.24f
            BackboneEdgeColor = RgbaColor (0.12f, 0.24f, 0.42f, 0.88f)
            SecondaryEdgeColor = RgbaColor (0.42f, 0.48f, 0.58f, 0.40f)
            BackboneTubeRadius = 0.014f
            SecondaryTubeRadius = 0.007f
            UseEdgeTubes = false
            TubeSides = 6
            GroupBox = defaultLightGroupBox
            ShowGroupWireframe = true
            GroupWireframeColor = RgbaColor (0.25f, 0.34f, 0.46f, 0.48f)
            IgnoreGroupSectionPlane = false
        }

    /// <summary>Configures extruded cylindrical edge tubes with explicit radial side count (<c>&gt;= 3</c>).</summary>
    let withEdgeTubes (enabled: bool) (sides: int) (theme: GraphTheme3D) : GraphTheme3D =
        if sides < 3 then
            raise (ArgumentOutOfRangeException (nameof sides, "Tube cross-section requires at least 3 sides."))

        { theme with
            UseEdgeTubes = enabled
            TubeSides = sides
        }

    /// <summary>Sets the glyph primitive shape and radius multiplier on <c>theme</c>.</summary>
    let withNodeShape (shape: GlyphShape3D) (radiusScale: float32) (theme: GraphTheme3D) : GraphTheme3D =
        if not (Double.IsFinite (float radiusScale)) || radiusScale <= 0.0f then
            raise (ArgumentOutOfRangeException (nameof radiusScale, "NodeRadiusScale must be finite and positive."))

        { theme with
            NodeShape = shape
            NodeRadiusScale = radiusScale
        }

/// <summary>Pure GPU batch adapters bridging all 12 <c>Ranvier.D5.Graph3D</c> layout engines and spatial trees into <c>Ranvier.D5.Wgpu</c>.</summary>
[<RequireQualifiedAccess>]
module GraphScene3D =
    let inline private isFinitePoint3 (p: Point3) =
        Double.IsFinite p.X && Double.IsFinite p.Y && Double.IsFinite p.Z

    let inline private isFiniteBounds3 (b: Bounds3) =
        Double.IsFinite (Bounds3.minX b)
        && Double.IsFinite (Bounds3.minY b)
        && Double.IsFinite (Bounds3.minZ b)
        && Double.IsFinite (Bounds3.maxX b)
        && Double.IsFinite (Bounds3.maxY b)
        && Double.IsFinite (Bounds3.maxZ b)

    let private expandInterval (lo: float) (hi: float) (paddingFraction: float) (fallbackHalfSpan: float) : float * float =
        let span = hi - lo

        if span <= 1e-9 then
            let mid = 0.5 * (lo + hi)
            let hs = max 1e-4 fallbackHalfSpan * (1.0 + paddingFraction * 2.0)
            mid - hs, mid + hs
        else
            let pad = span * paddingFraction
            lo - pad, hi + pad

    let private computeDiagramExtents (layout: DiagramLayout3D) : struct (bool * float * float * float * float * float * float) =
        let mutable hasAny = false
        let mutable minX, minY, minZ = Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity
        let mutable maxX, maxY, maxZ = Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity

        let inline includePt (x: float) (y: float) (z: float) =
            if Double.IsFinite x && Double.IsFinite y && Double.IsFinite z then
                hasAny <- true
                if x < minX then minX <- x
                if y < minY then minY <- y
                if z < minZ then minZ <- z
                if x > maxX then maxX <- x
                if y > maxY then maxY <- y
                if z > maxZ then maxZ <- z

        let inline includeBounds (b: Bounds3) =
            if isFiniteBounds3 b then
                includePt (Bounds3.minX b) (Bounds3.minY b) (Bounds3.minZ b)
                includePt (Bounds3.maxX b) (Bounds3.maxY b) (Bounds3.maxZ b)

        if isFiniteBounds3 layout.Bounds && (Bounds3.sizeX layout.Bounds > 0.0 || Bounds3.sizeY layout.Bounds > 0.0 || Bounds3.sizeZ layout.Bounds > 0.0) then
            includeBounds layout.Bounds

        for nd in layout.Nodes do
            includeBounds nd.Bounds
            includePt nd.Center.X nd.Center.Y nd.Center.Z

        for g in layout.Groups do
            includeBounds g.Bounds

        for a in layout.Associations do
            let pts = Point3Chain.asSpan a.Points

            for i = 0 to pts.Length - 1 do
                let p = pts[i]
                includePt p.X p.Y p.Z

        struct (hasAny, minX, maxX, minY, maxY, minZ, maxZ)

    let private layoutMaxSpan (layout: DiagramLayout3D) : float =
        let struct (hasAny, minX, maxX, minY, maxY, minZ, maxZ) = computeDiagramExtents layout

        if not hasAny then
            2.0
        else
            let maxS = max (maxX - minX) (max (maxY - minY) (maxZ - minZ))
            if maxS > 1e-6 then maxS else 2.0

    let private boostEmissive (shading: ShadingMode) (extra: float32) : ShadingMode =
        match shading with
        | ShadingMode.Unlit -> ShadingMode.Unlit
        | ShadingMode.ExactUnlit -> ShadingMode.ExactUnlit
        | ShadingMode.Lit (roughness, em) -> ShadingMode.Lit (roughness, max 0.0f (em + extra))

    let private validateShrinkFactor (shrinkFactor: float) =
        if not (Double.IsFinite shrinkFactor) || shrinkFactor <= 0.0 || shrinkFactor > 1.0 then
            raise (ArgumentOutOfRangeException (nameof shrinkFactor, "shrinkFactor must be finite and lie in (0, 1]."))

    let private shrinkVerticesToWorld (scale: Scale3D) (shrinkFactor: float) (centroid: Point3) (vertices: Point3 list) : Vector3 array voption =
        if vertices.IsEmpty || not (isFinitePoint3 centroid) then
            ValueNone
        else
            let out = ResizeArray<Vector3>(vertices.Length)
            let mutable ok = true

            for v in vertices do
                if ok && isFinitePoint3 v then
                    let p = Point3.lerp centroid v shrinkFactor

                    match Scale3D.tryMap scale p with
                    | ValueSome wp -> out.Add wp
                    | ValueNone -> ok <- false
                else
                    ok <- false

            if ok && out.Count >= 4 then ValueSome (out.ToArray ()) else ValueNone

    let private buildCellsMeshCore
        (scale: Scale3D)
        (shrinkFactor: float)
        (cells: seq<struct (Point3 * Point3 list * int list list * RgbaColor)>)
        : MeshData =
        let parts = ResizeArray<MeshData>()

        for struct (centroid, verts, faces, color) in cells do
            if not faces.IsEmpty then
                match shrinkVerticesToWorld scale shrinkFactor centroid verts with
                | ValueSome worldVerts ->
                    let faceArrs = ResizeArray<int array>(faces.Length)

                    for f in faces do
                        if f.Length >= 3 then
                            let arr = Array.ofList f
                            let mutable indicesValid = true
                            let mutable k = 0

                            while indicesValid && k < arr.Length do
                                let idx = arr[k]

                                if idx < 0 || idx >= worldVerts.Length then
                                    indicesValid <- false

                                k <- k + 1

                            if indicesValid then
                                faceArrs.Add arr

                    if faceArrs.Count >= 4 then
                        let cellMesh = Primitives.convexPolyhedron worldVerts faceArrs color

                        if cellMesh.Vertices.Length > 0 && cellMesh.Indices.Length > 0 then
                            parts.Add cellMesh
                | ValueNone -> ()

        if parts.Count = 0 then MeshData.empty else MeshData.merge parts

    let inline private quantizeWorldPoint (v: Vector3) : struct (int64 * int64 * int64) =
        struct (
            int64 (MathF.Round (v.X * 10000.0f)),
            int64 (MathF.Round (v.Y * 10000.0f)),
            int64 (MathF.Round (v.Z * 10000.0f))
        )

    let private canonicalWorldEdge (a: Vector3) (b: Vector3) =
        let qa = quantizeWorldPoint a
        let qb = quantizeWorldPoint b
        if qa <= qb then struct (qa, qb) else struct (qb, qa)

    let private packCellsWireframeCore
        (scale: Scale3D)
        (shrinkFactor: float)
        (ignoreSectionPlane: bool)
        (cells: seq<struct (Point3 * Point3 list * int list list * RgbaColor)>)
        : ReadOnlyMemory<LineVertex3D> =
        let globalSeen =
            HashSet<struct (struct (int64 * int64 * int64) * struct (int64 * int64 * int64))>()

        let out = ResizeArray<LineVertex3D>()

        for struct (centroid, verts, faces, color) in cells do
            if Double.IsFinite (float color.R)
               && Double.IsFinite (float color.G)
               && Double.IsFinite (float color.B)
               && Double.IsFinite (float color.A) then
                match shrinkVerticesToWorld scale shrinkFactor centroid verts with
                | ValueSome worldVerts ->
                    let localSeen = HashSet<struct (int * int)>()

                    for f in faces do
                        let len = f.Length

                        if len >= 2 then
                            let idxArr = Array.ofList f

                            for k = 0 to len - 1 do
                                let i0 = idxArr[k]
                                let i1 = idxArr[(k + 1) % len]

                                if i0 <> i1 && i0 >= 0 && i0 < worldVerts.Length && i1 >= 0 && i1 < worldVerts.Length then
                                    let pair = if i0 < i1 then struct (i0, i1) else struct (i1, i0)

                                    if localSeen.Add pair then
                                        let w0 = worldVerts[i0]
                                        let w1 = worldVerts[i1]
                                        let edgeKey = canonicalWorldEdge w0 w1

                                        if globalSeen.Add edgeKey then
                                            out.Add (LineVertex3D (w0, color, ignoreSectionPlane))
                                            out.Add (LineVertex3D (w1, color, ignoreSectionPlane))
                | ValueNone -> ()

        if out.Count = 0 then ReadOnlyMemory<LineVertex3D>.Empty else ReadOnlyMemory<LineVertex3D>(out.ToArray ())

    /// <summary>Derives a padded <c>Scale3D</c> fitting every node, group, and association polyline in <c>layout</c>.</summary>
    let scaleOfLayout (aspect: BoxAspect) (upAxis: UpAxis) (paddingFraction: float) (layout: DiagramLayout3D) : Scale3D =
        if isNull (box layout) then
            nullArg (nameof layout)

        if not (Double.IsFinite paddingFraction) || paddingFraction < 0.0 then
            raise (ArgumentOutOfRangeException (nameof paddingFraction, "paddingFraction must be finite and nonnegative."))

        let struct (hasAny, minX, maxX, minY, maxY, minZ, maxZ) = computeDiagramExtents layout

        if not hasAny then
            Scale3D.create aspect upAxis (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0))
        else
            let maxSpan = max 2e-3 (max (maxX - minX) (max (maxY - minY) (maxZ - minZ)))
            let zeroFallbackHalf = 0.05 * maxSpan
            let xLo, xHi = expandInterval minX maxX paddingFraction zeroFallbackHalf
            let yLo, yHi = expandInterval minY maxY paddingFraction zeroFallbackHalf
            let zLo, zHi = expandInterval minZ maxZ paddingFraction zeroFallbackHalf
            Scale3D.create aspect upAxis (AxisScale3D.linear (xLo, xHi)) (AxisScale3D.linear (yLo, yHi)) (AxisScale3D.linear (zLo, zHi))

    /// <summary>Packs positioned diagram nodes into <c>GlyphDatum3D</c> payloads, attenuating radii by <c>Radius</c> and <c>ScaleFactor</c>.</summary>
    let packNodes (theme: GraphTheme3D) (selectedIds: Set<string>) (layout: DiagramLayout3D) : ReadOnlyMemory<GlyphDatum3D> =
        if isNull (box theme) then
            nullArg (nameof theme)

        if isNull (box layout) then
            nullArg (nameof layout)

        if layout.Nodes.IsEmpty then
            ReadOnlyMemory<GlyphDatum3D>.Empty
        else
            let worldPerData = 2.0 / layoutMaxSpan layout
            let out = ResizeArray<GlyphDatum3D>(layout.Nodes.Length)

            for nd in layout.Nodes do
                if isFinitePoint3 nd.Center then
                    let isSel = Set.contains nd.Id selectedIds

                    let baseWorldR =
                        if Double.IsFinite nd.Radius && nd.Radius > 0.0 then
                            nd.Radius * worldPerData
                        else
                            0.032

                    let atten =
                        if Double.IsFinite nd.ScaleFactor then
                            sqrt (Math.Clamp (nd.ScaleFactor, 0.02, 1.0))
                        else
                            1.0

                    let shapeMult =
                        match theme.NodeShape with
                        | GlyphShape3D.Sphere -> 1.0f
                        | _ -> 2.0f

                    let selMult = if isSel then 1.2f else 1.0f
                    let r = max 0.006f (float32 (baseWorldR * atten) * theme.NodeRadiusScale * shapeMult * selMult)
                    let col = if isSel then theme.NodeSelectedColor else theme.NodeColor nd
                    let sh = if isSel then boostEmissive theme.NodeShading theme.SelectedEmissive else theme.NodeShading

                    let datum =
                        GlyphDatum3D.sphere nd.Center r col
                        |> GlyphDatum3D.withShading sh
                        |> GlyphDatum3D.withSelected isSel

                    out.Add datum

            if out.Count = 0 then ReadOnlyMemory<GlyphDatum3D>.Empty else ReadOnlyMemory<GlyphDatum3D>(out.ToArray ())

    /// <summary>Packs compound diagram groups into <c>BoxDatum3D</c> items using <c>theme.GroupBox</c>.</summary>
    let packGroups (theme: GraphTheme3D) (layout: DiagramLayout3D) : ReadOnlyMemory<BoxDatum3D> =
        if isNull (box theme) then
            nullArg (nameof theme)

        if isNull (box layout) then
            nullArg (nameof layout)

        if layout.Groups.IsEmpty then
            ReadOnlyMemory<BoxDatum3D>.Empty
        else
            let out = ResizeArray<BoxDatum3D>(layout.Groups.Length)

            for g in layout.Groups do
                if isFiniteBounds3 g.Bounds then
                    match theme.GroupBox g with
                    | ValueSome b -> out.Add b
                    | ValueNone -> ()

            if out.Count = 0 then ReadOnlyMemory<BoxDatum3D>.Empty else ReadOnlyMemory<BoxDatum3D>(out.ToArray ())

    /// <summary>Emits 12-edge wireframe cuboids for compound groups in <c>layout</c> when <c>theme.ShowGroupWireframe</c> is enabled.</summary>
    let packGroupWireframe (scale: Scale3D) (theme: GraphTheme3D) (layout: DiagramLayout3D) : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box theme) then
            nullArg (nameof theme)

        if isNull (box layout) then
            nullArg (nameof layout)

        if not theme.ShowGroupWireframe || layout.Groups.IsEmpty then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let boxes =
                [|
                    for g in layout.Groups do
                        if isFiniteBounds3 g.Bounds then
                            yield struct (g.Bounds, theme.GroupWireframeColor)
                |]

            Line3D.boxesWireframe scale (ReadOnlySpan<struct (Bounds3 * RgbaColor)>(boxes)) theme.IgnoreGroupSectionPlane

    /// <summary>Packs diagram association polylines directly from <c>Point3Chain.asSpan</c> into paired <c>LineVertex3D</c> segments.</summary>
    let packAssociationLines (scale: Scale3D) (theme: GraphTheme3D) (layout: DiagramLayout3D) : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box theme) then
            nullArg (nameof theme)

        if isNull (box layout) then
            nullArg (nameof layout)

        if layout.Associations.IsEmpty then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let out = ResizeArray<LineVertex3D>(layout.Associations.Length * 4)

            for assoc in layout.Associations do
                let pts = Point3Chain.asSpan assoc.Points

                if pts.Length >= 2 then
                    let col = if assoc.IsBackbone then theme.BackboneEdgeColor else theme.SecondaryEdgeColor
                    let mutable prevWorld: Vector3 voption = ValueNone

                    for i = 0 to pts.Length - 1 do
                        let p = pts[i]

                        if isFinitePoint3 p then
                            match Scale3D.tryMap scale p with
                            | ValueSome w ->
                                match prevWorld with
                                | ValueSome prevW ->
                                    out.Add (LineVertex3D (prevW, col))
                                    out.Add (LineVertex3D (w, col))
                                | ValueNone -> ()

                                prevWorld <- ValueSome w
                            | ValueNone -> prevWorld <- ValueNone
                        else
                            prevWorld <- ValueNone

            if out.Count = 0 then ReadOnlyMemory<LineVertex3D>.Empty else ReadOnlyMemory<LineVertex3D>(out.ToArray ())

    /// <summary>Extrudes diagram association polylines into closed, capped cylindrical tubes via <c>Line3D.tubes</c>.</summary>
    let buildAssociationTubes
        (scale: Scale3D)
        (sides: int)
        (theme: GraphTheme3D)
        (layout: DiagramLayout3D)
        : Result<MeshData, LayoutError list> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box theme) then
            nullArg (nameof theme)

        if isNull (box layout) then
            nullArg (nameof layout)

        if sides < 3 then
            Error [ LayoutError.InvalidObservation "Tube cross-section requires at least 3 sides." ]
        else
            let chains = ResizeArray<struct (Point3 * float32 * RgbaColor) array>()

            for assoc in layout.Associations do
                let pts = Point3Chain.asSpan assoc.Points

                if pts.Length >= 2 then
                    let rad = if assoc.IsBackbone then theme.BackboneTubeRadius else theme.SecondaryTubeRadius
                    let col = if assoc.IsBackbone then theme.BackboneEdgeColor else theme.SecondaryEdgeColor
                    let filtered = ResizeArray<struct (Point3 * float32 * RgbaColor)>(pts.Length)
                    let mutable prevWorld: Vector3 voption = ValueNone
                    let mutable totalLen = 0.0f

                    for i = 0 to pts.Length - 1 do
                        let p = pts[i]

                        if isFinitePoint3 p then
                            match Scale3D.tryMap scale p with
                            | ValueSome w ->
                                match prevWorld with
                                | ValueNone ->
                                    filtered.Add (struct (p, rad, col))
                                    prevWorld <- ValueSome w
                                | ValueSome prevW ->
                                    let d = w - prevW
                                    let distSq = d.LengthSquared ()

                                    if distSq > 1e-10f then
                                        filtered.Add (struct (p, rad, col))
                                        totalLen <- totalLen + sqrt distSq
                                        prevWorld <- ValueSome w
                            | ValueNone -> ()

                    if filtered.Count >= 2 && totalLen > 1e-5f then
                        chains.Add (filtered.ToArray ())

            if chains.Count = 0 then
                Ok MeshData.empty
            else
                Line3D.tubes scale sides (chains |> Seq.map (fun arr -> arr :> IReadOnlyList<_>))

    /// <summary>
    /// Creates an <c>UpAxis.YUp</c> <c>Scale3D</c> matching <c>CityBuilding.Bounds</c> X-Z ground footprint and <c>+Y</c> height directly without axis swapping.
    /// </summary>
    let scaleOfCodeCity (buildings: IReadOnlyList<CityBuilding<'Id>>) : Scale3D =
        if isNull (box buildings) then
            nullArg (nameof buildings)

        if buildings.Count = 0 then
            Scale3D.create BoxAspect.ChartDefault UpAxis.YUp (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (0.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0))
        else
            let mutable hasAny = false
            let mutable minX, minY, minZ = Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity
            let mutable maxX, maxY, maxZ = Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity

            for i = 0 to buildings.Count - 1 do
                let b = buildings[i].Bounds

                if isFiniteBounds3 b then
                    hasAny <- true
                    minX <- min minX (Bounds3.minX b)
                    maxX <- max maxX (Bounds3.maxX b)
                    minY <- min minY (Bounds3.minY b)
                    maxY <- max maxY (Bounds3.maxY b)
                    minZ <- min minZ (Bounds3.minZ b)
                    maxZ <- max maxZ (Bounds3.maxZ b)

            if not hasAny then
                Scale3D.create BoxAspect.ChartDefault UpAxis.YUp (AxisScale3D.linear (-1.0, 1.0)) (AxisScale3D.linear (0.0, 1.0)) (AxisScale3D.linear (-1.0, 1.0))
            else
                let xLo, xHi = expandInterval minX maxX 0.05 0.5
                let yHi = if maxY - minY <= 1e-9 then minY + 1.0 else maxY + (maxY - minY) * 0.06
                let zLo, zHi = expandInterval minZ maxZ 0.05 0.5
                Scale3D.create BoxAspect.ChartDefault UpAxis.YUp (AxisScale3D.linear (xLo, xHi)) (AxisScale3D.linear (minY, yHi)) (AxisScale3D.linear (zLo, zHi))

    /// <summary>Separates 2.5D <c>CodeCity</c> leaf buildings from district plinths into opaque/translucent <c>BoxDatum3D</c> slices.</summary>
    let packCodeCity
        (colorOf: CityBuilding<'Id> -> RgbaColor)
        (shading: ShadingMode)
        (selectedIds: Set<'Id>)
        (buildings: IReadOnlyList<CityBuilding<'Id>>)
        : struct (ReadOnlyMemory<BoxDatum3D> * ReadOnlyMemory<BoxDatum3D>) =
        if isNull (box colorOf) then
            nullArg (nameof colorOf)

        if isNull (box buildings) then
            nullArg (nameof buildings)

        let leafBuf = ResizeArray<BoxDatum3D>()
        let districtBuf = ResizeArray<BoxDatum3D>()

        for i = 0 to buildings.Count - 1 do
            let b = buildings[i]

            if isFiniteBounds3 b.Bounds then
                let isSel = Set.contains b.Id selectedIds

                let datum =
                    BoxDatum3D.create b.Bounds (colorOf b)
                    |> BoxDatum3D.withShading shading
                    |> BoxDatum3D.withSelected isSel

                if b.IsDistrict then districtBuf.Add datum else leafBuf.Add datum

        let leafMem = if leafBuf.Count = 0 then ReadOnlyMemory<BoxDatum3D>.Empty else ReadOnlyMemory<BoxDatum3D>(leafBuf.ToArray ())
        let distMem = if districtBuf.Count = 0 then ReadOnlyMemory<BoxDatum3D>.Empty else ReadOnlyMemory<BoxDatum3D>(districtBuf.ToArray ())
        struct (leafMem, distMem)

    /// <summary>Packs 3D <c>Treemap3D</c> cuboids into <c>BoxDatum3D</c> slices, optionally filtering to leaf nodes.</summary>
    let packTreemapCuboids
        (colorOf: HierarchyCuboid<'Id> -> RgbaColor)
        (shading: ShadingMode)
        (leavesOnly: bool)
        (cuboids: IReadOnlyList<HierarchyCuboid<'Id>>)
        : ReadOnlyMemory<BoxDatum3D> =
        if isNull (box colorOf) then
            nullArg (nameof colorOf)

        if isNull (box cuboids) then
            nullArg (nameof cuboids)

        let out = ResizeArray<BoxDatum3D>(cuboids.Count)

        for i = 0 to cuboids.Count - 1 do
            let c = cuboids[i]

            if (not leavesOnly || c.IsLeaf) && isFiniteBounds3 c.Bounds then
                out.Add (BoxDatum3D.create c.Bounds (colorOf c) |> BoxDatum3D.withShading shading)

        if out.Count = 0 then ReadOnlyMemory<BoxDatum3D>.Empty else ReadOnlyMemory<BoxDatum3D>(out.ToArray ())

    /// <summary>Packs 3D <c>Treemap3D</c> cuboid wireframes via <c>Line3D.boxesWireframe</c>.</summary>
    let packTreemapWireframe
        (scale: Scale3D)
        (colorOf: HierarchyCuboid<'Id> -> RgbaColor)
        (ignoreSectionPlane: bool)
        (cuboids: IReadOnlyList<HierarchyCuboid<'Id>>)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box colorOf) then
            nullArg (nameof colorOf)

        if isNull (box cuboids) then
            nullArg (nameof cuboids)

        if cuboids.Count = 0 then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let pairs =
                [|
                    for i = 0 to cuboids.Count - 1 do
                        let c = cuboids[i]

                        if isFiniteBounds3 c.Bounds then
                            yield struct (c.Bounds, colorOf c)
                |]

            Line3D.boxesWireframe scale (ReadOnlySpan<struct (Bounds3 * RgbaColor)>(pairs)) ignoreSectionPlane

    /// <summary>Packs 3D tangent-packed hierarchy spheres into spherical <c>GlyphDatum3D</c> payloads.</summary>
    let packSpheres
        (colorOf: PackedSphere3D<'Id> -> RgbaColor)
        (shading: ShadingMode)
        (leavesOnly: bool)
        (spheres: IReadOnlyList<PackedSphere3D<'Id>>)
        : ReadOnlyMemory<GlyphDatum3D> =
        if isNull (box colorOf) then
            nullArg (nameof colorOf)

        if isNull (box spheres) then
            nullArg (nameof spheres)

        if spheres.Count = 0 then
            ReadOnlyMemory<GlyphDatum3D>.Empty
        else
            let mutable hasAny = false
            let mutable minX, minY, minZ = Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity
            let mutable maxX, maxY, maxZ = Double.NegativeInfinity, Double.NegativeInfinity, Double.NegativeInfinity

            for i = 0 to spheres.Count - 1 do
                let s = spheres[i]

                if isFinitePoint3 s.Center && Double.IsFinite s.Radius && s.Radius >= 0.0 then
                    hasAny <- true
                    minX <- min minX (s.Center.X - s.Radius)
                    maxX <- max maxX (s.Center.X + s.Radius)
                    minY <- min minY (s.Center.Y - s.Radius)
                    maxY <- max maxY (s.Center.Y + s.Radius)
                    minZ <- min minZ (s.Center.Z - s.Radius)
                    maxZ <- max maxZ (s.Center.Z + s.Radius)

            if not hasAny then
                ReadOnlyMemory<GlyphDatum3D>.Empty
            else
                let maxSpan = max (maxX - minX) (max (maxY - minY) (maxZ - minZ))
                let worldPerData = if maxSpan > 1e-6 then 2.0 / maxSpan else 1.0
                let out = ResizeArray<GlyphDatum3D>(spheres.Count)

                for i = 0 to spheres.Count - 1 do
                    let s = spheres[i]

                    if (not leavesOnly || s.IsLeaf) && isFinitePoint3 s.Center && Double.IsFinite s.Radius && s.Radius > 0.0 then
                        let wr = max 1e-4f (float32 (s.Radius * worldPerData))
                        out.Add (GlyphDatum3D.sphere s.Center wr (colorOf s) |> GlyphDatum3D.withShading shading)

                if out.Count = 0 then ReadOnlyMemory<GlyphDatum3D>.Empty else ReadOnlyMemory<GlyphDatum3D>(out.ToArray ())

    /// <summary>
    /// Builds merged outward-wound convex polyhedron meshes for <c>VoronoiTreemap3DCell</c> partitions,
    /// shrinking cell vertices toward each cell centroid by <c>shrinkFactor</c> in <c>(0, 1]</c>.
    /// </summary>
    let buildVoronoiCellMeshes
        (scale: Scale3D)
        (shrinkFactor: float)
        (colorOf: VoronoiTreemap3DCell<'Id> -> RgbaColor)
        (cells: IReadOnlyList<VoronoiTreemap3DCell<'Id>>)
        : MeshData =
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box cells) then nullArg (nameof cells)
        validateShrinkFactor shrinkFactor

        seq {
            for i = 0 to cells.Count - 1 do
                let c = cells[i]
                yield struct (c.Centroid, c.Vertices, c.Faces, colorOf c)
        }
        |> buildCellsMeshCore scale shrinkFactor

    /// <summary>Emits deduplicated polygonal face edges across <c>VoronoiTreemap3DCell</c> partitions into <c>LineVertex3D</c> segments.</summary>
    let packVoronoiWireframe
        (scale: Scale3D)
        (shrinkFactor: float)
        (colorOf: VoronoiTreemap3DCell<'Id> -> RgbaColor)
        (ignoreSectionPlane: bool)
        (cells: IReadOnlyList<VoronoiTreemap3DCell<'Id>>)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box cells) then nullArg (nameof cells)
        validateShrinkFactor shrinkFactor

        seq {
            for i = 0 to cells.Count - 1 do
                let c = cells[i]
                yield struct (c.Centroid, c.Vertices, c.Faces, colorOf c)
        }
        |> packCellsWireframeCore scale shrinkFactor ignoreSectionPlane

    /// <summary>
    /// Builds merged outward-wound convex polyhedron meshes for generic 3D <c>PolyhedronCell3D</c> Voronoi/Laguerre cells,
    /// shrinking cell vertices toward each cell centroid by <c>shrinkFactor</c> in <c>(0, 1]</c>.
    /// </summary>
    let buildPolyhedronCellMeshes
        (scale: Scale3D)
        (shrinkFactor: float)
        (colorOf: PolyhedronCell3D -> RgbaColor)
        (cells: IReadOnlyList<PolyhedronCell3D>)
        : MeshData =
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box cells) then nullArg (nameof cells)
        validateShrinkFactor shrinkFactor

        seq {
            for i = 0 to cells.Count - 1 do
                let c = cells[i]
                yield struct (c.Centroid, c.Vertices, c.Faces, colorOf c)
        }
        |> buildCellsMeshCore scale shrinkFactor

    /// <summary>Emits deduplicated polygonal face edges across <c>PolyhedronCell3D</c> Voronoi/Laguerre cells into <c>LineVertex3D</c> segments.</summary>
    let packPolyhedronWireframe
        (scale: Scale3D)
        (shrinkFactor: float)
        (colorOf: PolyhedronCell3D -> RgbaColor)
        (ignoreSectionPlane: bool)
        (cells: IReadOnlyList<PolyhedronCell3D>)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box cells) then nullArg (nameof cells)
        validateShrinkFactor shrinkFactor

        seq {
            for i = 0 to cells.Count - 1 do
                let c = cells[i]
                yield struct (c.Centroid, c.Vertices, c.Faces, colorOf c)
        }
        |> packCellsWireframeCore scale shrinkFactor ignoreSectionPlane

    /// <summary>Packs 3D manifold embeddings (<c>Umap3D</c>, <c>Tsne3D</c>, <c>Isomap3D</c>, <c>Sammon3D</c>) into SDF <c>ScatterDatum3D</c> discs.</summary>
    let packEmbeddedScatter
        (colorOf: EmbeddedPoint3D<'Id> -> RgbaColor)
        (radiusPx: EmbeddedPoint3D<'Id> -> float32)
        (selectedIds: Set<'Id>)
        (points: IReadOnlyList<EmbeddedPoint3D<'Id>>)
        : ReadOnlyMemory<ScatterDatum3D> =
        if isNull (box colorOf) then nullArg (nameof colorOf)
        if isNull (box radiusPx) then nullArg (nameof radiusPx)
        if isNull (box points) then nullArg (nameof points)

        let out = ResizeArray<ScatterDatum3D>(points.Count)

        for i = 0 to points.Count - 1 do
            let pt = points[i]

            if isFinitePoint3 pt.Position then
                let r = max 1.0f (radiusPx pt)
                let col = colorOf pt

                let datum =
                    { ScatterDatum3D.create pt.Position r col with
                        Selected = Set.contains pt.Id selectedIds
                    }

                out.Add datum

        if out.Count = 0 then ReadOnlyMemory<ScatterDatum3D>.Empty else ReadOnlyMemory<ScatterDatum3D>(out.ToArray ())

    /// <summary>
    /// Packs <c>Dbscan3D.clusterItems</c> outputs into SDF <c>ScatterDatum3D</c> discs, coloring clusters via
    /// <c>ColorRamp.tableau10</c> and rendering noise outliers (<c>ClusterId = None</c>) as muted slate hollow rings.
    /// </summary>
    let packClusteredScatter
        (radiusPx: ClusteredPoint3D<'T, 'Id> -> float32)
        (selectedIds: Set<'Id>)
        (clusters: IReadOnlyList<ClusteredPoint3D<'T, 'Id>>)
        : ReadOnlyMemory<ScatterDatum3D> =
        if isNull (box radiusPx) then nullArg (nameof radiusPx)
        if isNull (box clusters) then nullArg (nameof clusters)

        let palette = ColorRamp.tableau10
        let noiseColor = RgbaColor (0.50f, 0.58f, 0.68f, 0.72f)
        let out = ResizeArray<ScatterDatum3D>(clusters.Count)

        for i = 0 to clusters.Count - 1 do
            let pt = clusters[i]

            if isFinitePoint3 pt.Position then
                let r = max 1.0f (radiusPx pt)
                let isSel = Set.contains pt.Id selectedIds

                let datum =
                    match pt.ClusterId, pt.IsCore with
                    | Some cid, true ->
                        let baseCol = palette[abs cid % palette.Length]
                        { ScatterDatum3D.create pt.Position r baseCol with Selected = isSel }
                    | Some cid, false ->
                        let baseCol = palette[abs cid % palette.Length]
                        let col = RgbaColor (baseCol.R * 0.90f, baseCol.G * 0.90f, baseCol.B * 0.90f, 0.90f)
                        { (ScatterDatum3D.create pt.Position r col |> ScatterDatum3D.withHollow 0.35f) with Selected = isSel }
                    | None, _ ->
                        { (ScatterDatum3D.create pt.Position r noiseColor
                           |> ScatterDatum3D.withRings 0.0f 1.0f 1.8f
                           |> ScatterDatum3D.withHollow 0.75f) with
                            Selected = isSel }

                out.Add datum

        if out.Count = 0 then ReadOnlyMemory<ScatterDatum3D>.Empty else ReadOnlyMemory<ScatterDatum3D>(out.ToArray ())

    /// <summary>Packs spatial <c>Octree</c> cell extents into depth-colored 3D wireframe bounding boxes.</summary>
    let packOctreeWireframe
        (scale: Scale3D)
        (colorOfDepth: int -> RgbaColor)
        (cells: IReadOnlyList<OctCell<'T>>)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box colorOfDepth) then nullArg (nameof colorOfDepth)
        if isNull (box cells) then nullArg (nameof cells)

        if cells.Count = 0 then
            ReadOnlyMemory<LineVertex3D>.Empty
        else
            let pairs =
                [|
                    for i = 0 to cells.Count - 1 do
                        let c = cells[i]

                        if isFiniteBounds3 c.Extent then
                            yield struct (c.Extent, colorOfDepth c.Depth)
                |]

            Line3D.boxesWireframe scale (ReadOnlySpan<struct (Bounds3 * RgbaColor)>(pairs)) false

    /// <summary>Visits an <c>Octree</c> down to <c>maxDepth</c> and emits depth-colored wireframe box vertices in one call.</summary>
    let packOctreeTree
        (scale: Scale3D)
        (colorOfDepth: int -> RgbaColor)
        (maxDepth: int)
        (octree: Octree<'T>)
        : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then nullArg (nameof scale)
        if isNull (box colorOfDepth) then nullArg (nameof colorOfDepth)
        if isNull (box octree) then nullArg (nameof octree)

        let cells = ResizeArray<OctCell<'T>>()
        Octree.visit (fun cell -> cells.Add cell; cell.Depth < maxDepth) octree
        packOctreeWireframe scale colorOfDepth cells

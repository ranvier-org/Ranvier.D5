module Ranvier.D5.Gallery.SpatialOrganisationGallery

open System
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.Graph3D

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors

let private panelCard (id: string) (x: float) (y: float) (w: float) (h: float) (caption: string) (innerNodes: SceneNode list) =
    let border =
        Shape.rectangle (Bounds.create 0. 0. w h)
        |> Mark.path (id + ":card")
        |> Mark.withFill (Paint.rgb 250 252 255)
        |> Mark.withStroke (Paint.rgb 203 213 225) 1.25
    let sep =
        Path.ofCommands [ MoveTo(Point.create 0. 24.); LineTo(Point.create w 24.) ]
        |> Mark.path (id + ":sep")
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.
    let header =
        Mark.text (id + ":caption") (Point.create 10. 16.5) caption
        |> Mark.withFill (Paint.rgb 30 41 59)
        |> Mark.withFontSize 12.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "start"; Baseline = "alphabetic" }
    let bodyGroup =
        Group({ GroupSpec.create (id + ":body") with Transform = Transform.translate 0. 24. }, innerNodes)
    Group(
        { GroupSpec.create id with Transform = Transform.translate x y; Clip = Some ("clip:" + id) },
        [ Leaf border; Leaf sep; Leaf header; bodyGroup ]
    ),
    RectClip("clip:" + id, Bounds.create 0. 0. w h)

let private embedSubScene (prefix: string) (x: float) (y: float) (sub: Scene) : SceneNode * SceneDefinition list =
    let rename k = prefix + ":" + k
    let mapDef = function
        | RectClip(k, b) -> RectClip(rename k, b)
        | LinearGradient(k, a, b, s) -> LinearGradient(rename k, a, b, s)
        | RadialGradient(k, c, r, s) -> RadialGradient(rename k, c, r, s)
    let rec mapNode = function
        | Leaf m ->
            let m2 =
                match Mark.fillDefinition m with
                | Some k -> Mark.withFillDefinition (rename k) m
                | None -> m
            Leaf m2
        | Group(g, children) ->
            Group({ g with Clip = Option.map rename g.Clip }, List.map mapNode children)
    let grp =
        Group(
            { GroupSpec.create prefix with Transform = Transform.translate x y },
            Scene.nodes sub |> List.map mapNode
        )
    grp, (Scene.definitions sub |> List.map mapDef)

let private closedPolyPath (pts: Point list) : Path =
    match pts with
    | p0 :: rest when not rest.IsEmpty ->
        Path.ofCommands (MoveTo p0 :: (rest |> List.map LineTo) @ [ Close ])
    | _ -> Path.empty

let private spectrumPaint (t: float) : Paint =
    let u = Math.Clamp(t, 0., 1.)
    let stops =
        [| 30, 64, 175
           14, 116, 144
           16, 185, 129
           245, 158, 11
           225, 29, 72 |]
    let pos = u * float (stops.Length - 1)
    let idx = min (stops.Length - 2) (int (floor pos))
    let f = pos - float idx
    let r0, g0, b0 = stops[idx]
    let r1, g1, b1 = stops[idx + 1]
    let lerpI a b = int (Math.Round(float a + (float b - float a) * f))
    Paint.rgb (lerpI r0 r1) (lerpI g0 g1) (lerpI b0 b1)

let private discMark (key: string) (pt: Point) (radius: float) (fill: Paint) (stroke: Paint) (sw: float) =
    Shape.arc 0. radius 0. (2. * Math.PI)
    |> Mark.path key
    |> Mark.withFill fill
    |> Mark.withStroke stroke sw
    |> Mark.withTransform (Transform.translate pt.X pt.Y)

let private subLabel (key: string) (x: float) (y: float) (txt: string) =
    Mark.text key (Point.create x y) txt
    |> Mark.withFill (Paint.rgb 51 65 85)
    |> Mark.withFontSize 10.5
    |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }

// ===========================================================================
// PLATE 1: spatial-indexing
// Space-Filling 2D/3D Curves + Packed Hilbert RTree2D/RTree3D + Bvh3D + Spatial3D
// ===========================================================================

let private morton2DCurve (order: int) (bounds: Bounds) : Point list =
    let side = 1 <<< order
    let bx, by = Bounds.x bounds, Bounds.y bounds
    let sx, sy = Bounds.width bounds / float side, Bounds.height bounds / float side
    let cells =
        [ for iy in 0 .. side - 1 do
            for ix in 0 .. side - 1 do
                let code = Morton2D.encode (uint16 ix) (uint16 iy)
                let dx, dy = Morton2D.decode code
                yield Point.create (bx + (float dx + 0.5) * sx) (by + (float dy + 0.5) * sy) ]
    Morton2D.orderBy bounds id cells

let private morton3DCurve (order: int) (bounds: Bounds3) : Point3 list =
    let side = 1 <<< order
    let bx, by, bz = Bounds3.minX bounds, Bounds3.minY bounds, Bounds3.minZ bounds
    let sx = Bounds3.sizeX bounds / float side
    let sy = Bounds3.sizeY bounds / float side
    let sz = Bounds3.sizeZ bounds / float side
    let cells =
        [ for iz in 0 .. side - 1 do
            for iy in 0 .. side - 1 do
                for ix in 0 .. side - 1 do
                    let code = Morton3D.encode (uint16 ix) (uint16 iy) (uint16 iz)
                    let dx, dy, dz = Morton3D.decode code
                    yield Point3.create (bx + (float dx + 0.5) * sx) (by + (float dy + 0.5) * sy) (bz + (float dz + 0.5) * sz) ]
    Morton3D.orderBy bounds id cells

let private rtree2DDataA =
    [ for i in 0 .. 44 ->
        let col = i % 9
        let row = i / 9
        let jx = sin (float (i * 7 + 3) * 0.9) * 8.
        let jy = cos (float (i * 11 + 5) * 0.8) * 6.
        let x = 14. + float col * 38.5 + jx
        let y = 10. + float row * 38.0 + jy
        let w = 18. + float ((i * 5) % 11)
        let h = 14. + float ((i * 3) % 9)
        i, Bounds.create (max 8. (min 344. x)) (max 6. (min 184. y)) w h ]

let private rtree2DDataB =
    [ for j in 0 .. 6 ->
        let x = 44. + float j * 45.
        let y = 28. + float ((j * 3) % 4) * 38.
        j, Bounds.create x y 26. 22. ]

let private rtree2DA = RTree2D.build snd rtree2DDataA
let private rtree2DB = RTree2D.build snd rtree2DDataB
let private rtree2DJoined = RTree2D.spatialJoin rtree2DA rtree2DB

let private boxes3DData =
    [ for i in 0 .. 11 ->
        let ix = (i % 3) - 1
        let iy = ((i / 3) % 2)
        let iz = (i / 6)
        let cx = float ix * 34. + sin (float i * 1.3) * 5.
        let cy = (float iy - 0.5) * 34. + cos (float i * 0.9) * 4.
        let cz = (float iz - 0.5) * 36.
        i, Bounds3.create (cx - 11.) (cy - 10.) (cz - 11.) 22. 20. 22. ]

let private bvh3DIndex = Bvh3D.build snd boxes3DData
let private rtree3DIndex = RTree3D.build snd boxes3DData

let private cloud3DData =
    let golden = Math.PI * (3. - sqrt 5.)
    [ for i in 0 .. 45 ->
        let t = (float i + 0.5) / 46.
        let y = 1. - 2. * t
        let r = sqrt (max 0. (1. - y * y))
        let shell = 24. + 22. * float (i % 3) * 0.5
        let th = float i * golden
        i, Point3.create (shell * r * cos th) (shell * y) (shell * r * sin th) ]

let private kd3DIndex = Spatial3D.create snd cloud3DData

let private cubeWireframe (prefix: string) (vp: Viewport) (cam: Camera3D) (b: Bounds3) : SceneNode list =
    let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
    let x1, y1, z1 = Bounds3.maxX b, Bounds3.maxY b, Bounds3.maxZ b
    let corners =
        [| Point3.create x0 y0 z0; Point3.create x1 y0 z0; Point3.create x1 y1 z0; Point3.create x0 y1 z0
           Point3.create x0 y0 z1; Point3.create x1 y0 z1; Point3.create x1 y1 z1; Point3.create x0 y1 z1 |]
    let edges = [| 0,1; 1,2; 2,3; 3,0; 4,5; 5,6; 6,7; 7,4; 0,4; 1,5; 2,6; 3,7 |]
    let cmds = ResizeArray<PathCommand>()
    for a, c in edges do
        match Camera3D.projectPoint vp cam (corners[a]), Camera3D.projectPoint vp cam (corners[c]) with
        | Some(pa, _, _), Some(pc, _, _) ->
            cmds.Add(MoveTo pa)
            cmds.Add(LineTo pc)
        | _ -> ()
    if cmds.Count = 0 then []
    else
        [ Leaf(
            Mark.path (prefix + ":wire") (Path.ofCommands (List.ofSeq cmds))
            |> Mark.withFill Paint.none
            |> Mark.withStroke (Paint.rgb 148 163 184) 0.9
            |> Mark.withOpacity 0.55
          ) ]

let private renderSpaceFilling2DCard (phase: float) : SceneNode list * Point =
    let bMorton = Bounds.create 12. 14. 108. 108.
    let bHilbert = Bounds.create 136. 14. 108. 108.
    let bMoore = Bounds.create 260. 14. 108. 108.

    let ptsMorton = morton2DCurve 3 bMorton |> List.toArray
    let ptsHilbert = Hilbert2D.curve 3 bHilbert |> List.toArray
    let ptsMoore = Moore2D.curve 3 bMoore |> List.toArray

    let drawCurve (prefix: string) (box: Bounds) (pts: Point[]) (closeLoop: bool) (title: string) (subtitle: string) =
        let frame =
            Shape.rectangle box
            |> Mark.path (prefix + ":box")
            |> Mark.withFill (Paint.rgb 241 245 249)
            |> Mark.withStroke (Paint.rgb 148 163 184) 1.
        let n = pts.Length
        let segs =
            [ for i in 0 .. n - 2 ->
                let t = float i / float (max 1 (n - 2))
                let p = Path.ofCommands [ MoveTo (pts[i]); LineTo (pts[i + 1]) ]
                Leaf(
                    Mark.path (sprintf "%s:seg:%d" prefix i) p
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (spectrumPaint t) 1.75
                ) ]
        let loopMark =
            if closeLoop && n >= 2 then
                let p = Path.ofCommands [ MoveTo (pts[n - 1]); LineTo (pts[0]) ]
                [ Leaf(
                    Mark.path (prefix + ":close") p
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (Paint.rgb 15 23 42) 1.6
                  ) ]
            else []
        let cursorIdx = min (n - 1) (max 0 (int (Math.Round((0.25 + 0.55 * abs (sin (phase + 0.4))) * float (n - 1)))))
        let startDot = Leaf(discMark (prefix + ":start") (pts[0]) 4.8 (Paint.rgb 29 78 216) (Paint.rgb 255 255 255) 1.1)
        let endDot = Leaf(discMark (prefix + ":end") (pts[n - 1]) 4.2 (Paint.rgb 225 29 72) (Paint.rgb 255 255 255) 1.1)
        let curDot = Leaf(discMark (prefix + ":cur") (pts[cursorIdx]) 3.8 (Paint.rgb 245 158 11) (Paint.rgb 15 23 42) 1.2)
        let cx = Bounds.x box + Bounds.width box * 0.5
        let lbl1 = Leaf(subLabel (prefix + ":lbl1") cx 140. title)
        let lbl2 =
            Leaf(
                Mark.text (prefix + ":lbl2") (Point.create cx 155.) subtitle
                |> Mark.withFill (Paint.rgb 100 116 139)
                |> Mark.withFontSize 9.5
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }
            )
        [ Leaf frame ] @ segs @ loopMark @ [ startDot; endDot; curDot; lbl1; lbl2 ]

    let footer =
        Mark.text "sf2d:footer" (Point.create 190. 188.) "64-cell order-3 lattice • Blue = index 0 • Rose = index 63 • Amber = phase cursor"
        |> Mark.withFill (Paint.rgb 71 85 105)
        |> Mark.withFontSize 10.
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }

    let nodes =
        drawCurve "morton2d" bMorton ptsMorton false "Morton2D (Z)" "BMI2 / SWAR interleave"
        @ drawCurve "hilbert2d" bHilbert ptsHilbert false "Hilbert2D" "Unit-step Gray code"
        @ drawCurve "moore2d" bMoore ptsMoore true "Moore2D Loop" "Start-end adjacent"
        @ [ Leaf footer ]
    nodes, (ptsHilbert[0])

let private renderSpaceFilling3DCard (phase: float) : SceneNode list * Point =
    let cube = Bounds3.create -44. -44. -44. 88. 88. 88.
    let vp : Viewport = { Width = 182.; Height = 192. }
    let cam = Camera3D.orbit (Point3.create 0. 0. 0.) (0.62 + 0.35 * phase) 0.36 225.

    let hilbertPts = Hilbert3D.curve 2 cube |> List.toArray
    let mortonPts = morton3DCurve 2 cube |> List.toArray

    let renderCubeCurve (prefix: string) (ox: float) (pts3: Point3[]) (label: string) =
        let proj =
            pts3
            |> Array.map (fun p -> Camera3D.projectPoint vp cam p |> Option.get)
        let n = proj.Length
        let segs =
            [ for i in 0 .. n - 2 ->
                let pA, dA, _ = proj[i]
                let pB, dB, _ = proj[i + 1]
                let meanD = 0.5 * (dA + dB)
                let t = float i / float (max 1 (n - 2))
                let mark =
                    Path.ofCommands [ MoveTo pA; LineTo pB ]
                    |> Mark.path (sprintf "%s:s:%d" prefix i)
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (spectrumPaint t) 1.9
                    |> Mark.withOpacity 0.9
                meanD, Leaf mark ]
            |> List.sortByDescending fst
            |> List.map snd

        let beads =
            [ for i in 0 .. n - 1 ->
                let pt2, d, pScale = proj[i]
                let t = float i / float (max 1 (n - 1))
                let rad = if i = 0 then 5.2 elif i = n - 1 then 4.5 else max 1.8 (min 3.2 (2.2 * pScale))
                let fill = if i = 0 then Paint.rgb 29 78 216 elif i = n - 1 then Paint.rgb 225 29 72 else spectrumPaint t
                d, Leaf(discMark (sprintf "%s:v:%d" prefix i) pt2 rad fill (Paint.rgb 255 255 255) 0.8) ]
            |> List.sortByDescending fst
            |> List.map snd

        let wire = cubeWireframe prefix vp cam cube
        let cap = Leaf(subLabel (prefix + ":cap") (vp.Width * 0.5) 204. label)
        let p0, _, _ = proj[0]
        Group({ GroupSpec.create (prefix + ":grp") with Transform = Transform.translate ox 4. }, wire @ segs @ beads @ [ cap ]),
        Point.create (ox + p0.X) (4. + p0.Y)

    let gH, probeH = renderCubeCurve "hilbert3d" 6. hilbertPts "Hilbert3D (64-pt continuous)"
    let gM, _ = renderCubeCurve "morton3d" 192. mortonPts "Morton3D (30-bit Z-order)"
    [ gH; gM ], probeH

let private renderRTree2DCard (phase: float) : SceneNode list * Point =
    let qBox = Bounds.create (24. + 18. * sin phase) 14. 136. 96.
    let qPt = Point.create (272. + 12. * cos phase) 136.
    let winHits = RTree2D.search qBox rtree2DA |> List.map fst |> Set.ofList
    let knnHits = RTree2D.kNearest 6 qPt infinity rtree2DA |> List.map fst |> Set.ofList

    let boxesMarks =
        rtree2DDataA
        |> List.map (fun (idx, b) ->
            let isWin = Set.contains idx winHits
            let isKnn = Set.contains idx knnHits
            let fill, stroke, sw =
                if isWin then Paint.rgb 167 243 208, Paint.rgb 5 150 105, 1.5
                elif isKnn then Paint.rgb 253 230 138, Paint.rgb 217 119 6, 1.5
                else Paint.rgb 241 245 249, Paint.rgb 148 163 184, 1.0
            Leaf(
                Shape.rectangle b
                |> Mark.path (sprintf "rt2:a:%d" idx)
                |> Mark.withFill fill
                |> Mark.withStroke stroke sw
            ))

    let overlayMarks =
        rtree2DDataB
        |> List.map (fun (j, b) ->
            Leaf(
                Shape.rectangle b
                |> Mark.path (sprintf "rt2:b:%d" j)
                |> Mark.withFill (Paint.rgb 237 233 254)
                |> Mark.withStroke (Paint.rgb 124 58 237) 1.2
                |> Mark.withOpacity 0.62
            ))

    let joinLinks =
        rtree2DJoined
        |> List.mapi (fun k ((_, ba), (_, bb)) ->
            let ca = Point.create (Bounds.x ba + Bounds.width ba * 0.5) (Bounds.y ba + Bounds.height ba * 0.5)
            let cb = Point.create (Bounds.x bb + Bounds.width bb * 0.5) (Bounds.y bb + Bounds.height bb * 0.5)
            Leaf(
                Path.ofCommands [ MoveTo ca; LineTo cb ]
                |> Mark.path (sprintf "rt2:join:%d" k)
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 124 58 237) 1.6
            ))

    let knnRays =
        rtree2DDataA
        |> List.filter (fun (idx, _) -> Set.contains idx knnHits)
        |> List.map (fun (idx, b) ->
            let cb = Point.create (Bounds.x b + Bounds.width b * 0.5) (Bounds.y b + Bounds.height b * 0.5)
            Leaf(
                Path.ofCommands [ MoveTo qPt; LineTo cb ]
                |> Mark.path (sprintf "rt2:ray:%d" idx)
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 217 119 6) 1.5
            ))

    let winFrame =
        Leaf(
            Shape.rectangle qBox
            |> Mark.path "rt2:qbox"
            |> Mark.withFill (Paint.rgb 16 185 129)
            |> Mark.withOpacity 0.10
        )
    let winStroke =
        Leaf(
            Shape.rectangle qBox
            |> Mark.path "rt2:qbox:stroke"
            |> Mark.withFill Paint.none
            |> Mark.withStroke (Paint.rgb 4 120 87) 2.0
        )
    let queryHub = Leaf(discMark "rt2:qpt" qPt 5.6 (Paint.rgb 220 38 38) (Paint.rgb 255 255 255) 1.4)
    let legend =
        Leaf(
            Mark.text "rt2:legend" (Point.create 190. 217.) "Emerald: search window • Amber: kNearest(6) • Violet: spatialJoin"
            |> Mark.withFill (Paint.rgb 51 65 85)
            |> Mark.withFontSize 10.
            |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }
        )
    winFrame :: boxesMarks @ overlayMarks @ joinLinks @ knnRays @ [ winStroke; queryHub; legend ], qPt

let private render3DIndexingCard (phase: float) : SceneNode list * Point =
    let vp : Viewport = { Width = 182.; Height = 196. }
    let camL = Camera3D.orbit (Point3.create 0. 0. 0.) (0.58 + 0.28 * phase) 0.34 230.
    let rayOrigin = Point3.create -72. (-18. + 6. * sin phase) -64.
    let rayDir = Point3.create 1.0 0.26 0.90
    let closestHit = Bvh3D.raycastClosest rayOrigin rayDir 260. bvh3DIndex
    let rtreeHits = RTree3D.raycast rayOrigin rayDir 260. rtree3DIndex |> List.map (fst >> fst) |> Set.ofList
    let closestId = closestHit |> Option.map (fst >> fst)

    let boxNodes =
        boxes3DData
        |> List.choose (fun (idx, b) ->
            let c = Bounds3.center b
            Camera3D.projectPoint vp camL c
            |> Option.map (fun (_, d, _) ->
                let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
                let x1, y1, z1 = Bounds3.maxX b, Bounds3.maxY b, Bounds3.maxZ b
                let cs =
                    [| Point3.create x0 y0 z0; Point3.create x1 y0 z0; Point3.create x1 y1 z0; Point3.create x0 y1 z0
                       Point3.create x0 y0 z1; Point3.create x1 y0 z1; Point3.create x1 y1 z1; Point3.create x0 y1 z1 |]
                let edges = [| 0,1; 1,2; 2,3; 3,0; 4,5; 5,6; 6,7; 7,4; 0,4; 1,5; 2,6; 3,7 |]
                let cmds = ResizeArray<PathCommand>()
                for a, e in edges do
                    match Camera3D.projectPoint vp camL (cs[a]), Camera3D.projectPoint vp camL (cs[e]) with
                    | Some(pa, _, _), Some(pe, _, _) -> cmds.Add(MoveTo pa); cmds.Add(LineTo pe)
                    | _ -> ()
                let isClosest = (closestId = Some idx)
                let isPierce = Set.contains idx rtreeHits
                let col, sw, op =
                    if isClosest then Paint.rgb 225 29 72, 2.2, 1.0
                    elif isPierce then Paint.rgb 217 119 6, 1.4, 0.85
                    else Paint.rgb 100 116 139, 0.9, 0.45
                d, Leaf(
                    Mark.path (sprintf "bvh:box:%d" idx) (Path.ofCommands (List.ofSeq cmds))
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke col sw
                    |> Mark.withOpacity op
                )))
        |> List.sortByDescending fst
        |> List.map snd

    let rayEnd = Point3.create (rayOrigin.X + rayDir.X * 150.) (rayOrigin.Y + rayDir.Y * 150.) (rayOrigin.Z + rayDir.Z * 150.)
    let rayMarks =
        match Camera3D.projectPoint vp camL rayOrigin, Camera3D.projectPoint vp camL rayEnd with
        | Some(p0, _, _), Some(p1, _, _) ->
            let line =
                Leaf(
                    Path.ofCommands [ MoveTo p0; LineTo p1 ]
                    |> Mark.path "bvh:ray"
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (Paint.rgb 220 38 38) 2.1
                )
            let hitDot =
                match closestHit with
                | Some(_, tHit) ->
                    let hp3 = Point3.create (rayOrigin.X + rayDir.X * tHit) (rayOrigin.Y + rayDir.Y * tHit) (rayOrigin.Z + rayDir.Z * tHit)
                    match Camera3D.projectPoint vp camL hp3 with
                    | Some(hp2, _, _) -> [ Leaf(discMark "bvh:hitpt" hp2 5.0 (Paint.rgb 225 29 72) (Paint.rgb 255 255 255) 1.3) ]
                    | None -> []
                | None -> []
            line :: hitDot
        | _ -> []

    let leftGrp =
        Group(
            { GroupSpec.create "bvh3d:grp" with Transform = Transform.translate 6. 4. },
            boxNodes @ rayMarks @ [ Leaf(subLabel "bvh3d:lbl" 91. 212. "Bvh3D + RTree3D Raycast") ]
        )

    // Right sub-scene: Spatial3D k-d point cloud
    let camR = Camera3D.orbit (Point3.create 0. 0. 0.) (-0.45 + 0.32 * phase) 0.32 210.
    let q3 = Point3.create (8. * cos phase) 6. -4.
    let knn3 = Spatial3D.kNearest 8 q3 infinity kd3DIndex |> List.map fst |> Set.ofList
    let rad3 = Spatial3D.withinRadius q3 34. kd3DIndex |> List.map fst |> Set.ofList
    let env3 = Bounds3.create -48. -48. -48. 96. 96. 96.
    let q2, _, _ = Camera3D.projectPoint vp camR q3 |> Option.get

    let knnLinks =
        cloud3DData
        |> List.filter (fun (idx, _) -> Set.contains idx knn3)
        |> List.choose (fun (idx, pt) ->
            Camera3D.projectPoint vp camR pt
            |> Option.map (fun (p2, _, _) ->
                Leaf(
                    Path.ofCommands [ MoveTo q2; LineTo p2 ]
                    |> Mark.path (sprintf "kd3:lnk:%d" idx)
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (Paint.rgb 13 148 136) 1.5
                )))

    let cloudDots =
        cloud3DData
        |> List.choose (fun (idx, pt) ->
            Camera3D.projectPoint vp camR pt
            |> Option.map (fun (p2, d, _) ->
                let isK = Set.contains idx knn3
                let isR = Set.contains idx rad3
                let fill, rad =
                    if isK then Paint.rgb 16 185 129, 3.8
                    elif isR then Paint.rgb 59 130 246, 3.1
                    else Paint.rgb 148 163 184, 2.3
                d, Leaf(discMark (sprintf "kd3:pt:%d" idx) p2 rad fill (Paint.rgb 255 255 255) 0.7)))
        |> List.sortByDescending fst
        |> List.map snd

    let qSphere = Leaf(discMark "kd3:query" q2 5.6 (Paint.rgb 29 78 216) (Paint.rgb 255 255 255) 1.4)
    let rightGrp =
        Group(
            { GroupSpec.create "kd3d:grp" with Transform = Transform.translate 192. 4. },
            cubeWireframe "kd3d" vp camR env3 @ knnLinks @ cloudDots @ [ qSphere; Leaf(subLabel "kd3d:lbl" 91. 212. "Spatial3D kNearest(8)") ]
        )

    [ leftGrp; rightGrp ], Point.create (192. + q2.X) (4. + q2.Y)

let probesSpatialIndexing (phase: float) : (string * Point) list =
    let _, p1 = renderSpaceFilling2DCard phase
    let _, p2 = renderSpaceFilling3DCard phase
    let _, p3 = renderRTree2DCard phase
    let _, p4 = render3DIndexingCard phase
    [ "space-filling-2d:hilbert-start", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "space-filling-3d:hilbert3d-start", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "rtree2d:query-hub", Point.create (14. + p3.X) (304. + 24. + p3.Y)
      "spatial3d:query-sphere", Point.create (406. + p4.X) (304. + 24. + p4.Y) ]

let sceneSpatialIndexing (phase: float) : Scene =
    let n1, _ = renderSpaceFilling2DCard phase
    let n2, _ = renderSpaceFilling3DCard phase
    let n3, _ = renderRTree2DCard phase
    let n4, _ = render3DIndexingCard phase

    let p1, c1 = panelCard "p-sf2d" 14. 42. 380. 252. "Space-Filling 2D: Morton Z, Hilbert & Moore" n1
    let p2, c2 = panelCard "p-sf3d" 406. 42. 380. 252. "Space-Filling 3D: Hilbert3D & Morton3D" n2
    let p3, c3 = panelCard "p-rt2d" 14. 304. 380. 254. "Packed Hilbert RTree2D: Window, k-NN & Join" n3
    let p4, c4 = panelCard "p-ix3d" 406. 304. 380. 254. "3D Raycast & k-NN: Bvh3D, RTree3D & Spatial3D" n4

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "bg-spatial-idx"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "title-spatial-idx" (Point.create 16. 27.) "Ranvier.D5 — Space-Filling Curves, Packed Hilbert R-Trees & 3D BVH/k-d Indexing"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 16.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose { Width = 800.; Height = 570. } "Spatial Indexing & Space-Filling Curves" [ c1; c2; c3; c4 ] [ Leaf pageBg; Leaf title; p1; p2; p3; p4 ]

// ===========================================================================
// PLATE 2: seriation-sets
// Matrix Seriation + Dendrogram OLO + Dbscan2D/3D + Chart.upset + Chart.euler
// ===========================================================================

let private seriationData =
    let n = 12
    let perm = [| 7; 2; 10; 0; 5; 11; 3; 8; 1; 9; 4; 6 |]
    let nodes = [ for i in 0 .. n - 1 -> sprintf "N%02d" (perm[i]) ]
    let bandEdges =
        [ for i in 0 .. n - 1 do
            for j in i + 1 .. min (n - 1) (i + 2) do
                let w = if j = i + 1 then 1.0 else 0.55
                yield sprintf "N%02d" i, sprintf "N%02d" j, w ]
    let rcmOrder = Seriation.reverseCuthillMcKee nodes bandEdges |> ok

    let fiedlerEdges =
        [ for i in 0 .. n - 1 do
            for j in i + 1 .. n - 1 do
                let d = abs (i - j)
                if d <= 3 then
                    yield sprintf "N%02d" i, sprintf "N%02d" j, 1.0 / float d ]
    let fiedlerOrder = Seriation.spectralFiedler nodes fiedlerEdges |> ok

    let trueIdx (id: string) = Int32.Parse(id.Substring 1)
    let distFn (a: string) (b: string) =
        let ia, ib = trueIdx a, trueIdx b
        let ga, gb = ia / 4, ib / 4
        let baseD = abs (float (ia - ib)) * 0.12
        if ga = gb then baseD else 1.2 + baseD * 0.5
    let dendro = Seriation.hierarchicalCluster Linkage.Ward nodes distFn |> ok |> Option.get
    let oloOrder = Seriation.optimalLeafOrder dendro distFn

    let rows = [ for i in 0 .. n - 1 -> sprintf "R%02d" (perm[i]) ]
    let cols = [ for j in 0 .. n - 1 -> sprintf "C%02d" (perm[(j * 5 + 3) % n]) ]
    let biEntries =
        [ for r in 0 .. n - 1 do
            for c in 0 .. n - 1 do
                if r / 4 = c / 4 then
                    let w = if (r + c) % 2 = 0 then 1.0 else 0.72
                    yield sprintf "R%02d" r, sprintf "C%02d" c, w ]
    let biRows, biCols = Seriation.bicluster rows cols biEntries |> ok
    let _ = Seriation.barycenterOrder 8 rows cols biEntries |> ok
    rcmOrder, fiedlerOrder, dendro, oloOrder, distFn, biRows, biCols

let private renderSeriationCard (phase: float) : SceneNode list * Point =
    let rcmOrder, fiedlerOrder, dendro, oloOrder, distFn, biRows, biCols = seriationData
    let trueIdx (id: string) = Int32.Parse(id.Substring 1)
    let pulse = 0.92 + 0.08 * cos phase

    let drawMiniMatrix (prefix: string) (ox: float) (oy: float) (size: float) (rIds: string list) (cIds: string list) (cellVal: string -> string -> float) (hue: int * int * int) (caption: string) =
        let rArr = List.toArray rIds
        let cArr = List.toArray cIds
        let nr = rArr.Length
        let nc = cArr.Length
        let cw = size / float nc
        let ch = size / float nr
        let hr, hg, hb = hue
        let cells =
            [ for r in 0 .. nr - 1 do
                for c in 0 .. nc - 1 do
                    let v = Math.Clamp(cellVal (rArr[r]) (cArr[c]) * pulse, 0., 1.)
                    let cr = int (Math.Round(248. + (float hr - 248.) * v))
                    let cg = int (Math.Round(250. + (float hg - 250.) * v))
                    let cb = int (Math.Round(253. + (float hb - 253.) * v))
                    let rect = Bounds.create (ox + float c * cw) (oy + float r * ch) (cw + 0.25) (ch + 0.25)
                    yield Leaf(
                        Shape.rectangle rect
                        |> Mark.path (sprintf "%s:%d:%d" prefix r c)
                        |> Mark.withFill (Paint.rgb cr cg cb)
                    ) ]
        let border =
            Leaf(
                Shape.rectangle (Bounds.create ox oy size size)
                |> Mark.path (prefix + ":border")
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 100 116 139) 1.0
            )
        let lbl = Leaf(subLabel (prefix + ":lbl") (ox + size * 0.5) (oy - 5.) caption)
        let diag0Center = Point.create (ox + cw * 0.5) (oy + ch * 0.5)
        cells @ [ border; lbl ], diag0Center

    let m1, probeDiag =
        drawMiniMatrix "ser:rcm" 22. 18. 82. rcmOrder rcmOrder
            (fun a b ->
                let d = abs (trueIdx a - trueIdx b)
                if d = 0 then 1.0 elif d = 1 then 0.82 elif d = 2 then 0.48 else 0.0)
            (29, 78, 216) "RCM Bandwidth"

    let m2, _ =
        drawMiniMatrix "ser:fiedler" 214. 18. 82. fiedlerOrder fiedlerOrder
            (fun a b ->
                let d = abs (float (trueIdx a - trueIdx b))
                max 0. (1. - d / 5.5))
            (13, 148, 136) "Spectral Fiedler v₂"

    let m3, _ =
        drawMiniMatrix "ser:olo" 50. 124. 82. oloOrder oloOrder
            (fun a b -> max 0. (1. - distFn a b / 1.6))
            (109, 40, 217) "Ward + OLO"

    // Render mini dendrogram tree brackets along left edge of OLO matrix (x in 14..48, y in 124..206)
    let oloIndex =
        oloOrder |> List.mapi (fun idx id -> id, idx) |> Map.ofList
    let cellH = 82. / float oloOrder.Length
    let rec maxDendroH = function
        | Dendrogram.Leaf _ -> 1e-6
        | Dendrogram.Merge(l, r, h, _) -> max h (max (maxDendroH l) (maxDendroH r))
    let maxH = max 1e-3 (maxDendroH dendro)
    let dendroCmds = ResizeArray<PathCommand>()
    let rec layoutDendro = function
        | Dendrogram.Leaf id ->
            let row = Map.find id oloIndex
            let y = 124. + (float row + 0.5) * cellH
            48., y
        | Dendrogram.Merge(l, r, h, _) ->
            let xl, yl = layoutDendro l
            let xr, yr = layoutDendro r
            let xMerge = 48. - 32. * Math.Clamp(h / maxH, 0.06, 1.0)
            dendroCmds.Add(MoveTo(Point.create xl yl))
            dendroCmds.Add(LineTo(Point.create xMerge yl))
            dendroCmds.Add(LineTo(Point.create xMerge yr))
            dendroCmds.Add(LineTo(Point.create xr yr))
            xMerge, 0.5 * (yl + yr)
    layoutDendro dendro |> ignore
    let dendroMark =
        Leaf(
            Mark.path "ser:dendro" (Path.ofCommands (List.ofSeq dendroCmds))
            |> Mark.withFill Paint.none
            |> Mark.withStroke (Paint.rgb 71 85 105) 1.15
        )

    let m4, _ =
        drawMiniMatrix "ser:bi" 214. 124. 82. biRows biCols
            (fun r c -> if trueIdx r / 4 = trueIdx c / 4 then 0.92 else 0.04)
            (5, 150, 105) "Dhillon Bicluster"

    m1 @ m2 @ m3 @ [ dendroMark ] @ m4, probeDiag

let private dbscanData =
    let pts2D =
        [ for i in 0 .. 25 ->
            let t = Math.PI * float i / 25.
            sprintf "A%d" i, Point.create (34. + 64. * (0.5 + 0.5 * cos t)) (88. - 46. * sin t + float (i % 3 - 1) * 2.5)
          for i in 0 .. 25 ->
            let t = Math.PI * float i / 25.
            sprintf "B%d" i, Point.create (64. + 64. * (0.5 - 0.5 * cos t)) (76. + 46. * sin t + float (i % 3 - 1) * 2.5)
          yield "N0", Point.create 18. 22.
          yield "N1", Point.create 156. 20.
          yield "N2", Point.create 18. 166.
          yield "N3", Point.create 160. 164.
          yield "N4", Point.create 92. 18. ]
    let assign2D = Dbscan2D.cluster 20. 4 fst snd pts2D |> ok

    let pts3D =
        let centers = [| Point3.create -28. -18. -16.; Point3.create 28. -12. 18.; Point3.create 0. 28. -8. |]
        [ for c in 0 .. 2 do
            let ctr = centers[c]
            for k in 0 .. 11 do
                let a = float k * 2.1
                let zOff = float (k % 4 - 1) * 5.5
                yield sprintf "P%d_%d" c k, Point3.create (ctr.X + 9. * cos a) (ctr.Y + 9. * sin a) (ctr.Z + zOff)
          yield "Out0", Point3.create -46. 40. 38.
          yield "Out1", Point3.create 45. 42. -40.
          yield "Out2", Point3.create -44. -42. 40.
          yield "Out3", Point3.create 44. -40. -38. ]
    let assign3D = Dbscan3D.cluster 22. 4 fst snd pts3D |> ok
    pts2D, assign2D, pts3D, assign3D

let private renderDbscanCard (phase: float) : SceneNode list * Point =
    let pts2D, assign2D, pts3D, assign3D = dbscanData
    let map2D = assign2D |> List.map (fun a -> a.Id, a) |> Map.ofList
    let palette = [| Paint.rgb 37 99 235; Paint.rgb 16 185 129; Paint.rgb 245 158 11; Paint.rgb 139 92 246 |]

    let marks2D =
        pts2D
        |> List.map (fun (id, pt) ->
            let a = Map.find id map2D
            match a.ClusterId with
            | Some cid ->
                let fill = palette[cid % palette.Length]
                let rad = if id = "A12" then 5.4 elif a.IsCore then 4.2 else 3.2
                let rim = if a.IsCore then Paint.rgb 15 23 42 else Paint.rgb 255 255 255
                Leaf(discMark ("db2:" + id) pt rad fill rim 1.0)
            | None ->
                Leaf(
                    Shapes.symbol SymbolSpec.Times 34.
                    |> Mark.path ("db2:" + id)
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (Paint.rgb 190 24 93) 1.7
                    |> Mark.withTransform (Transform.translate pt.X pt.Y)
                ))

    let probePt = pts2D |> List.find (fun (id, _) -> id = "A12") |> snd

    let vp3 : Viewport = { Width = 180.; Height = 188. }
    let cam3 = Camera3D.orbit (Point3.create 0. 0. 0.) (0.52 + 0.38 * phase) 0.32 215.
    let map3D = assign3D |> List.map (fun a -> a.Id, a) |> Map.ofList
    let cube = Bounds3.create -50. -50. -50. 100. 100. 100.

    let marks3D =
        pts3D
        |> List.choose (fun (id, p3) ->
            Camera3D.projectPoint vp3 cam3 p3
            |> Option.map (fun (p2, d, _) ->
                let a = Map.find id map3D
                match a.ClusterId with
                | Some cid ->
                    let fill = palette[cid % palette.Length]
                    d, Leaf(discMark ("db3:" + id) p2 (if a.IsCore then 4.0 else 3.0) fill (Paint.rgb 255 255 255) 0.85)
                | None ->
                    d, Leaf(
                        Shapes.symbol SymbolSpec.Times 30.
                        |> Mark.path ("db3:" + id)
                        |> Mark.withFill Paint.none
                        |> Mark.withStroke (Paint.rgb 190 24 93) 1.6
                        |> Mark.withTransform (Transform.translate p2.X p2.Y)
                    )))
        |> List.sortByDescending fst
        |> List.map snd

    let leftFrame =
        Leaf(
            Shape.rectangle (Bounds.create 8. 8. 176. 184.)
            |> Mark.path "db2:frame"
            |> Mark.withFill (Paint.rgb 241 245 249)
            |> Mark.withStroke (Paint.rgb 203 213 225) 1.
        )
    let grp3D =
        Group(
            { GroupSpec.create "db3:grp" with Transform = Transform.translate 192. 6. },
            cubeWireframe "db3" vp3 cam3 cube @ marks3D
        )

    [ leftFrame ] @ marks2D @ [ grp3D; Leaf(subLabel "db2:lbl" 96. 210. "Dbscan2D (arcs + noise)"); Leaf(subLabel "db3:lbl" 282. 210. "Dbscan3D (3 clouds + noise)") ],
    probePt

let private buildUpSetAndEuler (phase: float) =
    let sets =
        [ "SIMD", [ for i in 1 .. 24 -> sprintf "M%02d" i ]
          "Fable", [ for i in 9 .. 34 -> sprintf "M%02d" i ]
          "Graph", [ for i in 15 .. 40 -> sprintf "M%02d" i ]
          "3D", [ for i in 18 .. 30 -> sprintf "M%02d" i ] @ [ for i in 38 .. 45 -> sprintf "M%02d" i ] ]
    let uFrame =
        { Viewport = { Width = 364.; Height = 216. }
          Margin = { Top = 8.; Right = 10.; Bottom = 10.; Left = 10. }
          Title = "UpSet Intersections" }
    let barBlue = if phase >= 0.5 then Paint.rgb 29 78 216 else Paint.rgb 37 99 235
    let uSpec =
        { UpSetSpec.create uFrame with
            MaxIntersections = 7
            BarColor = barBlue
            ActiveDotColor = Paint.rgb 15 23 42
            InactiveDotColor = Paint.rgb 203 213 225 }
    let upsetOut = Chart.upset uSpec sets |> ok

    let barCenter =
        upsetOut.HitTargets
        |> List.pick (fun h ->
            match h.MarkPath, h.Geometry with
            | [ "plot"; k ], HitBounds b when k.StartsWith "3:bar" && Bounds.height b >= 24. ->
                Some(Point.create (Bounds.x b + Bounds.width b * 0.5) (Bounds.y b + Bounds.height b * 0.5))
            | _ -> None)

    let eSets =
        [ "SIMD", 44. + 2. * sin phase
          "Graph", 36.
          "3D", 28.
          "Geo", 18. ]
    let eOverlaps =
        [ "SIMD", "Graph", 16.
          "Graph", "3D", 13.
          "SIMD", "3D", 10.
          "3D", "Geo", 6.5 ]
    let eFrame =
        { Viewport = { Width = 364.; Height = 216. }
          Margin = { Top = 8.; Right = 10.; Bottom = 10.; Left = 10. }
          Title = "Area-Proportional Euler" }
    let eSpec =
        { EulerSpec.create eFrame with
            Opacity = 0.52
            Color = function
                | "SIMD" -> Paint.rgb 37 99 235
                | "Graph" -> Paint.rgb 16 185 129
                | "3D" -> Paint.rgb 245 158 11
                | _ -> Paint.rgb 139 92 246 }
    let eulerOut = Chart.euler eSpec eSets eOverlaps |> ok
    let eulerGeom = EulerVenn.layout eulerOut.PlotBounds eSets eOverlaps |> ok
    let simdCircle = eulerGeom.Circles |> List.find (fun c -> c.SetName = "SIMD")
    let eulerProbe = Point.create simdCircle.Center.X (simdCircle.Center.Y + simdCircle.Radius * 0.45)
    upsetOut, barCenter, eulerOut, eulerGeom, eulerProbe

let probesSeriationSets (phase: float) : (string * Point) list =
    let _, p1 = renderSeriationCard phase
    let _, p2 = renderDbscanCard phase
    let _, barCenter, _, _, eulerProbe = buildUpSetAndEuler phase
    [ "seriation:rcm-diag", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "dbscan2d:core-point", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "upset:top-bar", Point.create (14. + 8. + barCenter.X) (304. + 24. + 4. + barCenter.Y)
      "euler:simd-disc", Point.create (406. + 8. + eulerProbe.X) (304. + 24. + 4. + eulerProbe.Y) ]

let sceneSeriationSets (phase: float) : Scene =
    let n1, _ = renderSeriationCard phase
    let n2, _ = renderDbscanCard phase
    let upsetOut, _, eulerOut, eulerGeom, _ = buildUpSetAndEuler phase

    let uGrp, uDefs = embedSubScene "upset" 8. 4. upsetOut.Scene
    let eGrp, eDefs = embedSubScene "euler" 8. 4. eulerOut.Scene
    let stressBadge =
        Leaf(
            Mark.text "euler:stress" (Point.create 366. 216.) (sprintf "Wilkinson stress = %.4f" eulerGeom.Stress)
            |> Mark.withFill (Paint.rgb 71 85 105)
            |> Mark.withFontSize 9.8
            |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "end"; Baseline = "alphabetic" }
        )

    let p1, c1 = panelCard "p-seriation" 14. 42. 380. 252. "Matrix Seriation: RCM, Fiedler, OLO & Bicluster" n1
    let p2, c2 = panelCard "p-dbscan" 406. 42. 380. 252. "Density Clustering: Dbscan2D & Dbscan3D" n2
    let p3, c3 = panelCard "p-upset" 14. 304. 380. 254. "UpSet Intersection Algebra (Chart.upset)" [ uGrp ]
    let p4, c4 = panelCard "p-euler" 406. 304. 380. 254. "Area-Proportional Euler/Venn (Chart.euler)" [ eGrp; stressBadge ]

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "bg-seriation"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "title-seriation" (Point.create 16. 27.) "Ranvier.D5 — Matrix Seriation, Density Clustering & Set Intersection Algebra"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 16.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose { Width = 800.; Height = 570. } "Seriation, Clustering & Set Algebra" ([ c1; c2; c3; c4 ] @ uDefs @ eDefs) [ Leaf pageBg; Leaf title; p1; p2; p3; p4 ]

// ===========================================================================
// PLATE 3: voronoi-treemaps
// PowerDiagram + L-BFGS Cvt2D + VoronoiTreemap + Ordered Treemaps + GosperMap
// ===========================================================================

let private cvt2DSeedPoints =
    let mutable rng = RandomState.create 19UL
    [ for _ in 0 .. 25 ->
        let struct(u, r1) = RandomState.nextFloat rng
        let struct(v, r2) = RandomState.nextFloat r1
        rng <- r2
        Point.create (206. + u * 156.) (16. + v * 172.) ]

let private cvt2DLbfgs =
    Cvt2D.relax CvtSolver.Lbfgs 28 (Bounds.create 198. 10. 170. 184.) cvt2DSeedPoints |> ok

let private cvt2DLloydStep =
    Cvt2D.relax CvtSolver.Lloyd 2 (Bounds.create 198. 10. 170. 184.) cvt2DSeedPoints |> ok

let private voronoiTreemapCached =
    let cx, cy, rad = 190., 110., 98.
    let octagon =
        [ for k in 0 .. 9 ->
            let a = -Math.PI * 0.5 + float k * 2. * Math.PI / 10.
            Point.create (cx + rad * cos a) (cy + rad * sin a) ]
    let root =
        Hierarchy.node "root" 0.
            [ Hierarchy.node "Core" 0.
                [ Hierarchy.node "Geometry" 28. []
                  Hierarchy.node "Simd" 22. []
                  Hierarchy.node "Scale" 16. []
                  Hierarchy.node "Scene" 14. [] ]
              Hierarchy.node "Graph" 0.
                [ Hierarchy.node "Sugiyama" 26. []
                  Hierarchy.node "Manifold" 20. []
                  Hierarchy.node "Bundle" 15. [] ]
              Hierarchy.node "Charts" 0.
                [ Hierarchy.node "Cartesian" 22. []
                  Hierarchy.node "UpSet" 16. []
                  Hierarchy.node "Density" 14. [] ]
              Hierarchy.node "Geo" 0.
                [ Hierarchy.node "Proj" 19. []
                  Hierarchy.node "Sphere" 15. []
                  Hierarchy.node "Path" 11. [] ] ]
    VoronoiTreemap.layoutInPolygon 65 1e-3 11u octagon root |> ok

let private renderPowerAndCvtCard (phase: float) : SceneNode list * Point =
    let pBounds = Bounds.create 12. 10. 170. 184.
    let powerSites =
        [ for i in 0 .. 11 ->
            let col = i % 3
            let row = i / 3
            let px = 36. + float col * 56. + sin (float (i * 5 + 1)) * 7.
            let py = 30. + float row * 45. + cos (float (i * 3 + 2)) * 6.
            let baseW = if i = 4 then 560. elif i % 3 = 0 then 280. elif i % 3 = 1 then 40. else -90.
            { PowerSite.Id = sprintf "S%d" i
              Position = Point.create px py
              Weight = baseW + 45. * sin (phase + float i * 0.7) } ]
    let pCells = PowerDiagram.clipBounds pBounds powerSites |> ok
    let pPalette =
        [| Paint.rgb 96 165 250; Paint.rgb 52 211 153; Paint.rgb 251 191 36
           Paint.rgb 167 139 250; Paint.rgb 56 189 248; Paint.rgb 251 113 133 |]

    let powerMarks =
        pCells
        |> List.filter (fun c -> c.Vertices.Length >= 3)
        |> List.mapi (fun idx c ->
            let poly =
                Leaf(
                    closedPolyPath c.Vertices
                    |> Mark.path ("pow:cell:" + c.Id)
                    |> Mark.withFill (pPalette[idx % pPalette.Length])
                    |> Mark.withOpacity 0.72
                    |> Mark.withStroke (Paint.rgb 30 41 59) 1.15
                )
            let wRad = sqrt (max 12. (c.Weight + 160.)) * 0.42
            let ring =
                Leaf(
                    Shape.arc 0. wRad 0. (2. * Math.PI)
                    |> Mark.path ("pow:ring:" + c.Id)
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (Paint.rgb 15 23 42) 0.85
                    |> Mark.withOpacity 0.45
                    |> Mark.withTransform (Transform.translate c.Site.X c.Site.Y)
                )
            let dot = Leaf(discMark ("pow:site:" + c.Id) c.Site 2.7 (Paint.rgb 15 23 42) (Paint.rgb 255 255 255) 0.7)
            [ poly; ring; dot ])
        |> List.concat

    let bestPowerCell = pCells |> List.maxBy (fun c -> c.Area)

    let cvtCells, cvtSites = cvt2DLbfgs
    let _, lloydSites = cvt2DLloydStep
    let cvtSitesArr = List.toArray cvtSites
    let lloydArr = List.toArray lloydSites

    let cvtPolys =
        cvtCells
        |> List.filter (fun c -> c.Vertices.Length >= 3)
        |> List.mapi (fun idx c ->
            let shade = if idx % 3 = 0 then Paint.rgb 167 243 208 elif idx % 3 = 1 then Paint.rgb 191 219 254 else Paint.rgb 224 231 255
            Leaf(
                closedPolyPath c.Vertices
                |> Mark.path (sprintf "cvt2:cell:%d" idx)
                |> Mark.withFill shade
                |> Mark.withOpacity 0.78
                |> Mark.withStroke (Paint.rgb 51 65 85) 1.0
            ))

    let cvtTrails =
        [ for i in 0 .. cvtSitesArr.Length - 1 ->
            let a = lloydArr[i]
            let b = cvtSitesArr[i]
            Leaf(
                Path.ofCommands [ MoveTo a; LineTo b ]
                |> Mark.path (sprintf "cvt2:trail:%d" i)
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 100 116 139) 0.9
                |> Mark.withOpacity 0.55
            ) ]

    let cvtDots =
        cvtSitesArr
        |> Array.mapi (fun i pt ->
            Leaf(discMark (sprintf "cvt2:pt:%d" i) pt 2.8 (Paint.rgb 5 150 105) (Paint.rgb 255 255 255) 0.8))
        |> Array.toList

    powerMarks @ cvtPolys @ cvtTrails @ cvtDots
    @ [ Leaf(subLabel "pow:lbl" 97. 210. "Aurenhammer PowerDiagram")
        Leaf(subLabel "cvt2:lbl" 283. 210. "L-BFGS Cvt2D (hex blue-noise)") ],
    bestPowerCell.Centroid

let private renderVoronoiTreemapCard () : SceneNode list * Point =
    let cells = voronoiTreemapCached
    let groupColor = function
        | Some "Core" -> Paint.rgb 96 165 250
        | Some "Graph" -> Paint.rgb 52 211 153
        | Some "Charts" -> Paint.rgb 251 191 36
        | Some "Geo" -> Paint.rgb 167 139 250
        | _ -> Paint.rgb 148 163 184

    let leaves = cells |> List.filter (fun c -> c.Depth = 2 && c.Polygon.Length >= 3)
    let branches = cells |> List.filter (fun c -> c.Depth = 1 && c.Polygon.Length >= 3)

    let leafMarks =
        leaves
        |> List.collect (fun c ->
            let poly =
                Leaf(
                    closedPolyPath c.Polygon
                    |> Mark.path ("vt2:leaf:" + c.Id)
                    |> Mark.withFill (groupColor c.Parent)
                    |> Mark.withOpacity 0.78
                    |> Mark.withStroke (Paint.rgb 255 255 255) 1.3
                )
            if c.Area >= 950. then
                let lbl =
                    Leaf(
                        Mark.text ("vt2:lbl:" + c.Id) c.Centroid c.Id
                        |> Mark.withFill (Paint.rgb 15 23 42)
                        |> Mark.withFontSize 9.2
                        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "central" }
                    )
                [ poly; lbl ]
            else [ poly ])

    let branchMarks =
        branches
        |> List.map (fun b ->
            Leaf(
                closedPolyPath b.Polygon
                |> Mark.path ("vt2:branch:" + b.Id)
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 15 23 42) 2.4
            ))

    let largestLeaf = leaves |> List.maxBy (fun c -> c.Area)
    let probe = Point.create largestLeaf.Centroid.X (largestLeaf.Centroid.Y + 8.)
    leafMarks @ branchMarks, probe

let private renderOrderedTreemapsCard (phase: float) : SceneNode list * Point =
    let baseWeights = [| 24.; 18.; 15.; 22.; 12.; 19.; 14.; 11.; 20.; 13.; 16.; 10.; 17.; 9.; 14.; 12. |]
    let makeRoot (factorFn: int -> float) =
        Hierarchy.node "root" 0.
            [ for i in 0 .. 15 -> Hierarchy.node (sprintf "I%02d" i) (baseWeights[i] * factorFn i) [] ]

    let baseRoot = makeRoot (fun _ -> 1.0)
    let evolvedRoot = makeRoot (fun i -> 1.0 + 0.28 * sin (phase + float i * 0.65))

    let b1 = Bounds.create 12. 18. 170. 84.
    let b2 = Bounds.create 198. 18. 170. 84.
    let b3 = Bounds.create 12. 124. 170. 84.
    let b4 = Bounds.create 198. 124. 170. 84.

    let stripBoxes = Treemap.layout { TreemapConfig.create b1 with Tiling = TreemapTiling.Strip; Padding = 1.2 } baseRoot |> ok
    let hilbBoxes = Treemap.layout { TreemapConfig.create b2 with Tiling = TreemapTiling.Hilbert; Padding = 1.2 } baseRoot |> ok
    let mooreBoxes = Treemap.layout { TreemapConfig.create b3 with Tiling = TreemapTiling.Moore; Padding = 1.2 } baseRoot |> ok
    let _, initState = Treemap.resquarify { TreemapConfig.create b4 with Padding = 1.2 } baseRoot None |> ok
    let localBoxes, _ = Treemap.evolveLocalMoves 6 { TreemapConfig.create b4 with Padding = 1.2 } evolvedRoot initState |> ok

    let drawPanel (prefix: string) (boxes: HierarchyBox<string> list) (boxFrame: Bounds) (title: string) =
        let leaves = boxes |> List.filter (fun b -> b.Depth = 1) |> List.toArray
        let rects =
            leaves
            |> Array.mapi (fun i b ->
                let t = float i / float (max 1 (leaves.Length - 1))
                Leaf(
                    Shape.rectangle b.Bounds
                    |> Mark.path (sprintf "%s:r:%d" prefix i)
                    |> Mark.withFill (spectrumPaint t)
                    |> Mark.withOpacity 0.84
                    |> Mark.withStroke (Paint.rgb 255 255 255) 0.9
                ))
            |> Array.toList
        let centers =
            leaves
            |> Array.map (fun b ->
                Some(Point.create (Bounds.x b.Bounds + Bounds.width b.Bounds * 0.5) (Bounds.y b.Bounds + Bounds.height b.Bounds * 0.5)))
            |> Array.toList
        let spine =
            Leaf(
                Shape.line Curve.Linear centers
                |> Mark.path (prefix + ":spine")
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 15 23 42) 1.25
                |> Mark.withOpacity 0.72
            )
        let lbl = Leaf(subLabel (prefix + ":lbl") (Bounds.x boxFrame + Bounds.width boxFrame * 0.5) (Bounds.y boxFrame - 4.) title)
        let firstB = leaves[0].Bounds
        let firstCenter = Point.create (Bounds.x firstB + Bounds.width firstB * 0.5) (Bounds.y firstB + Bounds.height firstB * 0.5)
        rects @ [ spine; lbl ], firstCenter

    let m1, _ = drawPanel "tm:strip" stripBoxes b1 "TreemapTiling.Strip"
    let m2, hilbFirst = drawPanel "tm:hilb" hilbBoxes b2 "TreemapTiling.Hilbert"
    let m3, _ = drawPanel "tm:moore" mooreBoxes b3 "TreemapTiling.Moore"
    let m4, _ = drawPanel "tm:local" localBoxes b4 "Sondag evolveLocalMoves"
    m1 @ m2 @ m3 @ m4, hilbFirst

let private gosperHierarchy =
    Hierarchy.node "root" 0.
        [ Hierarchy.node "Engine" 0.
            [ Hierarchy.node "G1" 28. []
              Hierarchy.node "G2" 22. []
              Hierarchy.node "G3" 18. []
              Hierarchy.node "G4" 14. [] ]
          Hierarchy.node "Charts" 0.
            [ Hierarchy.node "C1" 24. []
              Hierarchy.node "C2" 19. []
              Hierarchy.node "C3" 15. [] ]
          Hierarchy.node "Graph" 0.
            [ Hierarchy.node "N1" 26. []
              Hierarchy.node "N2" 20. []
              Hierarchy.node "N3" 16. []
              Hierarchy.node "N4" 12. [] ]
          Hierarchy.node "Geo" 0.
            [ Hierarchy.node "S1" 21. []
              Hierarchy.node "S2" 17. []
              Hierarchy.node "S3" 13. [] ] ]

let private renderGosperCard (phase: float) : SceneNode list * Point =
    let gBounds = Bounds.create 14. 6. 352. 214.
    let smooth = if phase >= 0.5 then 1 else 0
    let regions =
        GosperMap.layout
            { GosperMapConfig.create gBounds with Order = 3; SmoothIterations = smooth }
            gosperHierarchy
        |> ok
    let curvePts = GosperMap.curve 3 gBounds |> ok

    let branchFill = function
        | Some "Engine" -> Paint.rgb 96 165 250
        | Some "Charts" -> Paint.rgb 52 211 153
        | Some "Graph" -> Paint.rgb 251 191 36
        | Some "Geo" -> Paint.rgb 167 139 250
        | _ -> Paint.rgb 148 163 184

    let leafMarks =
        regions
        |> List.filter (fun r -> r.Depth = 2 && not r.Boundary.IsEmpty)
        |> List.map (fun r ->
            let p = Path.concat (r.Boundary |> List.map closedPolyPath)
            Leaf(
                Mark.path ("gosper:leaf:" + r.Id) p
                |> Mark.withFill (branchFill r.Parent)
                |> Mark.withOpacity 0.80
                |> Mark.withStroke (Paint.rgb 255 255 255) 1.0
            ))

    let spine =
        Leaf(
            Shape.line Curve.Linear (curvePts |> List.map Some)
            |> Mark.path "gosper:curve"
            |> Mark.withFill Paint.none
            |> Mark.withStroke (Paint.rgb 30 41 59) 0.55
            |> Mark.withOpacity 0.22
        )

    let groupMarks =
        regions
        |> List.filter (fun r -> r.Depth = 1 && not r.Boundary.IsEmpty)
        |> List.collect (fun r ->
            let p = Path.concat (r.Boundary |> List.map closedPolyPath)
            let border =
                Leaf(
                    Mark.path ("gosper:grp:" + r.Id) p
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (Paint.rgb 15 23 42) 2.2
                )
            let lbl =
                Leaf(
                    Mark.text ("gosper:lbl:" + r.Id) r.Centroid r.Id
                    |> Mark.withFill (Paint.rgb 15 23 42)
                    |> Mark.withFontSize 10.5
                    |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "middle"; Baseline = "central" }
                )
            [ border; lbl ])

    leafMarks @ [ spine ] @ groupMarks, curvePts.Head

let probesVoronoiTreemaps (phase: float) : (string * Point) list =
    let _, p1 = renderPowerAndCvtCard phase
    let _, p2 = renderVoronoiTreemapCard ()
    let _, p3 = renderOrderedTreemapsCard phase
    let _, p4 = renderGosperCard phase
    [ "power-diagram:cell", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "voronoi-treemap:leaf", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "treemap:hilbert-first", Point.create (14. + p3.X) (304. + 24. + p3.Y)
      "gosper-map:hex0", Point.create (406. + p4.X) (304. + 24. + p4.Y) ]

let sceneVoronoiTreemaps (phase: float) : Scene =
    let n1, _ = renderPowerAndCvtCard phase
    let n2, _ = renderVoronoiTreemapCard ()
    let n3, _ = renderOrderedTreemapsCard phase
    let n4, _ = renderGosperCard phase

    let p1, c1 = panelCard "p-pow-cvt" 14. 42. 380. 252. "Aurenhammer PowerDiagram & L-BFGS Cvt2D" n1
    let p2, c2 = panelCard "p-vtreemap" 406. 42. 380. 252. "Nocaj-Brandes Polygonal VoronoiTreemap" n2
    let p3, c3 = panelCard "p-ordered-tm" 14. 304. 380. 254. "Ordered & Stable: Strip, Hilbert, Moore & LocalMoves" n3
    let p4, c4 = panelCard "p-gosper" 406. 304. 380. 254. "Auber Hexagonal Peano-Gosper GosperMap" n4

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "bg-vtreemap"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "title-vtreemap" (Point.create 16. 27.) "Ranvier.D5 — Power Diagrams, L-BFGS CVT, Polygonal Voronoi & Gosper Treemaps"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 16.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose { Width = 800.; Height = 570. } "Voronoi, CVT & Space-Filling Treemaps" [ c1; c2; c3; c4 ] [ Leaf pageBg; Leaf title; p1; p2; p3; p4 ]

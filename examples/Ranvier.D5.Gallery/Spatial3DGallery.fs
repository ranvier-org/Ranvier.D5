module Ranvier.D5.Gallery.Spatial3DGallery

open System
open Ranvier.D5
open Ranvier.D5.Geo
open Ranvier.D5.Graph
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

let private discMark (key: string) (pt: Point) (radius: float) (fill: Paint) (stroke: Paint) (sw: float) =
    Shape.arc 0. radius 0. (2. * Math.PI)
    |> Mark.path key
    |> Mark.withFill fill
    |> Mark.withStroke stroke sw
    |> Mark.withTransform (Transform.translate pt.X pt.Y)

let private subLabel (key: string) (x: float) (y: float) (txt: string) =
    Mark.text key (Point.create x y) txt
    |> Mark.withFill (Paint.rgb 51 65 85)
    |> Mark.withFontSize 10.0
    |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }

let private spectrumRgb (t: float) : int * int * int =
    let u = Math.Clamp(t, 0., 1.)
    let stops =
        [| 37, 99, 235
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
    lerpI r0 r1, lerpI g0 g1, lerpI b0 b1

let private spectrumPaint (t: float) : Paint =
    let r, g, b = spectrumRgb t
    Paint.rgb r g b

// ---------------------------------------------------------------------------
// Painter's-Algorithm Lambertian-Shaded 3D Cuboid & Convex Polyhedron Helpers
// ---------------------------------------------------------------------------

type private ProjectedFace3D =
    { Key: string
      Depth: float
      Centroid2D: Point
      Path: Path
      Fill: Paint
      Stroke: Paint
      StrokeWidth: float
      Opacity: float }

let private lightDir =
    let lx, ly, lz = 0.44, 0.78, 0.44
    let inv = 1. / sqrt (lx * lx + ly * ly + lz * lz)
    lx * inv, ly * inv, lz * inv

let private shadeRgb (baseR: int, baseG: int, baseB: int) (nx: float) (ny: float) (nz: float) : Paint =
    let lx, ly, lz = lightDir
    let diff = max 0. (nx * lx + ny * ly + nz * lz)
    let factor = 0.48 + 0.56 * diff
    let clampCh c = min 236 (max 14 (int (Math.Round(float c * factor))))
    Paint.rgb (clampCh baseR) (clampCh baseG) (clampCh baseB)

let private projectPolyhedronFaces
    (keyPrefix: string)
    (vp: Viewport)
    (cam: Camera3D)
    (verts: Point3[])
    (faces: int list list)
    (interiorCentroid: Point3)
    (baseColor: int * int * int)
    (strokeColor: Paint)
    (strokeW: float)
    (opacity: float)
    : ProjectedFace3D list =
    faces
    |> List.mapi (fun fi idxs -> fi, idxs)
    |> List.choose (fun (fi, idxs) ->
        let idxArr = List.toArray idxs
        if idxArr.Length < 3 then None
        else
            let mutable fcx, fcy, fcz = 0., 0., 0.
            for k in 0 .. idxArr.Length - 1 do
                let v = verts[idxArr[k]]
                fcx <- fcx + v.X
                fcy <- fcy + v.Y
                fcz <- fcz + v.Z
            let invM = 1. / float idxArr.Length
            fcx <- fcx * invM
            fcy <- fcy * invM
            fcz <- fcz * invM

            let v0 = verts[idxArr[0]]
            let mutable nx, ny, nz = 0., 0., 0.
            for k in 1 .. idxArr.Length - 2 do
                let v1 = verts[idxArr[k]]
                let v2 = verts[idxArr[k + 1]]
                let ux, uy, uz = v1.X - v0.X, v1.Y - v0.Y, v1.Z - v0.Z
                let wx, wy, wz = v2.X - v0.X, v2.Y - v0.Y, v2.Z - v0.Z
                nx <- nx + (uy * wz - uz * wy)
                ny <- ny + (uz * wx - ux * wz)
                nz <- nz + (ux * wy - uy * wx)
            let nLen = sqrt (nx * nx + ny * ny + nz * nz)
            if nLen <= 1e-12 then None
            else
                let mutable unx, uny, unz = nx / nLen, ny / nLen, nz / nLen
                // Enforce strict outward orientation from convex cell centroid
                let outDot = (fcx - interiorCentroid.X) * unx + (fcy - interiorCentroid.Y) * uny + (fcz - interiorCentroid.Z) * unz
                if outDot < 0. then
                    unx <- -unx
                    uny <- -uny
                    unz <- -unz

                let toEyeX = cam.Eye.X - fcx
                let toEyeY = cam.Eye.Y - fcy
                let toEyeZ = cam.Eye.Z - fcz
                let frontDot = unx * toEyeX + uny * toEyeY + unz * toEyeZ
                if frontDot <= 0. then None
                else
                    let pts2D = ResizeArray<Point>(idxArr.Length)
                    let mutable depthSum = 0.
                    let mutable allVisible = true
                    for k in 0 .. idxArr.Length - 1 do
                        match Camera3D.projectPoint vp cam (verts[idxArr[k]]) with
                        | Some(p2, d, _) ->
                            pts2D.Add p2
                            depthSum <- depthSum + d
                        | None ->
                            allVisible <- false
                    if not allVisible || pts2D.Count < 3 then None
                    else
                        let polyPts = List.ofSeq pts2D
                        if abs (Polygon.area polyPts) < 0.35 then None
                        else
                            let mutable sx, sy = 0., 0.
                            for k in 0 .. pts2D.Count - 1 do
                                sx <- sx + (pts2D[k]).X
                                sy <- sy + (pts2D[k]).Y
                            let c2d = Point.create (sx / float pts2D.Count) (sy / float pts2D.Count)
                            Some
                                { Key = sprintf "%s:f%d" keyPrefix fi
                                  Depth = depthSum / float pts2D.Count
                                  Centroid2D = c2d
                                  Path = closedPolyPath polyPts
                                  Fill = shadeRgb baseColor unx uny unz
                                  Stroke = strokeColor
                                  StrokeWidth = strokeW
                                  Opacity = opacity })

let private cuboidFaces =
    [ [ 0; 3; 2; 1 ]
      [ 4; 5; 6; 7 ]
      [ 0; 1; 5; 4 ]
      [ 3; 7; 6; 2 ]
      [ 0; 4; 7; 3 ]
      [ 1; 2; 6; 5 ] ]

let private projectCuboidFaces
    (keyPrefix: string)
    (vp: Viewport)
    (cam: Camera3D)
    (b: Bounds3)
    (baseColor: int * int * int)
    (strokeColor: Paint)
    (strokeW: float)
    (opacity: float)
    : ProjectedFace3D list =
    let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
    let x1, y1, z1 = Bounds3.maxX b, Bounds3.maxY b, Bounds3.maxZ b
    if x1 - x0 <= 1e-6 || y1 - y0 <= 1e-6 || z1 - z0 <= 1e-6 then []
    else
        let verts =
            [| Point3.create x0 y0 z0
               Point3.create x1 y0 z0
               Point3.create x1 y1 z0
               Point3.create x0 y1 z0
               Point3.create x0 y0 z1
               Point3.create x1 y0 z1
               Point3.create x1 y1 z1
               Point3.create x0 y1 z1 |]
        projectPolyhedronFaces keyPrefix vp cam verts cuboidFaces (Bounds3.center b) baseColor strokeColor strokeW opacity

let private renderSortedFaces (faces: ProjectedFace3D list) : SceneNode list * Point =
    let sorted = faces |> List.sortByDescending (fun f -> f.Depth)
    let frontCentroid =
        match sorted with
        | [] -> Point.create 60. 60.
        | _ -> (List.last sorted).Centroid2D
    let marks =
        sorted
        |> List.map (fun f ->
            Leaf(
                Mark.path f.Key f.Path
                |> Mark.withFill f.Fill
                |> Mark.withStroke f.Stroke f.StrokeWidth
                |> Mark.withOpacity f.Opacity
            ))
    marks, frontCentroid

let private cubeWireframe (key: string) (vp: Viewport) (cam: Camera3D) (b: Bounds3) : SceneNode list =
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
            Mark.path key (Path.ofCommands (List.ofSeq cmds))
            |> Mark.withFill Paint.none
            |> Mark.withStroke (Paint.rgb 148 163 184) 0.85
            |> Mark.withOpacity 0.50
          ) ]

let private frontmostNode2D (vp: Viewport) (cam: Camera3D) (layout: DiagramLayout3D) : Point =
    layout.Nodes
    |> List.choose (fun n ->
        Camera3D.projectPoint vp cam n.Center
        |> Option.map (fun (pt2, depth, _) -> depth, pt2))
    |> List.sortBy fst
    |> List.head
    |> snd

// ===========================================================================
// PLATE 6: hierarchies-3d
// Treemap3D (CubeHilbert, CubeSquarified, StepTree) + CodeCity + Pack3D
// + Voronoi3D / Cvt3D + VoronoiTreemap3D
// ===========================================================================

let private treemap3DTree =
    Hierarchy.node "repo" 0.
        [ Hierarchy.node "Core" 0.
            [ Hierarchy.node "Scale" 26. []
              Hierarchy.node "Path" 20. []
              Hierarchy.node "Scene" 18. []
              Hierarchy.node "Simd" 14. [] ]
          Hierarchy.node "Graph" 0.
            [ Hierarchy.node "Sugiy" 24. []
              Hierarchy.node "Cola" 19. []
              Hierarchy.node "Loom" 15. [] ]
          Hierarchy.node "Spatial" 0.
            [ Hierarchy.node "RTree" 22. []
              Hierarchy.node "Voron" 17. []
              Hierarchy.node "Bvh3D" 13. [] ] ]

let private renderTreemap3DCard (phase: float) : SceneNode list * Point =
    let cubeBounds = Bounds3.create -44. -40. -44. 88. 80. 88.
    let stepBounds = Bounds3.create -46. -26. -46. 92. 20. 92.

    let hilbertCuboids =
        Treemap3D.layout
            { Treemap3DConfig.create cubeBounds with
                Tiling = Treemap3DTiling.CubeHilbert
                Padding = 2.2
                ExplodeFactor = 0.16 }
            treemap3DTree
        |> ok
    let _ = Treemap3D.toDiagramLayout hilbertCuboids

    let squarifiedCuboids =
        Treemap3D.layout
            { Treemap3DConfig.create cubeBounds with
                Tiling = Treemap3DTiling.CubeSquarified
                Padding = 2.2
                ExplodeFactor = 0.15 }
            treemap3DTree
        |> ok

    let stepCuboids =
        Treemap3D.layout
            { Treemap3DConfig.create stepBounds with
                Tiling = Treemap3DTiling.StepTree
                Padding = 3.2
                StepHeight = 14.5
                ExplodeFactor = 0. }
            treemap3DTree
        |> ok

    let vp : Viewport = { Width = 118.; Height = 178. }
    let camCube = Camera3D.orbit (Point3.create 0. 0. 0.) (0.68 + 0.32 * phase) 0.42 222.
    let camStep = Camera3D.orbit (Point3.create 0. 6. 0.) (0.72 + 0.32 * phase) 0.48 228.

    let branchColor = function
        | Some "Core" -> 37, 99, 235
        | Some "Graph" -> 16, 185, 129
        | Some "Spatial" -> 234, 88, 12
        | _ -> 100, 116, 139

    let hilbertLeaves = hilbertCuboids |> List.filter (fun c -> c.IsLeaf)
    let hFaces =
        hilbertLeaves
        |> List.mapi (fun idx c ->
            let col = spectrumRgb (float idx / float (max 1 (hilbertLeaves.Length - 1)))
            projectCuboidFaces ("tm3d:h:" + c.Id) vp camCube c.Bounds col (Paint.rgb 15 23 42) 0.65 1.0)
        |> List.concat
    let hMarks, hProbeLocal = renderSortedFaces hFaces
    let hGroup =
        Group({ GroupSpec.create "tm3d:hilbert" with Transform = Transform.translate 4. 4. },
              cubeWireframe "tm3d:h:wire" vp camCube cubeBounds @ hMarks)

    let sqLeaves = squarifiedCuboids |> List.filter (fun c -> c.IsLeaf)
    let sqFaces =
        sqLeaves
        |> List.collect (fun c ->
            projectCuboidFaces ("tm3d:sq:" + c.Id) vp camCube c.Bounds (branchColor c.Parent) (Paint.rgb 15 23 42) 0.65 1.0)
    let sqMarks, _ = renderSortedFaces sqFaces
    let sqGroup =
        Group({ GroupSpec.create "tm3d:squarified" with Transform = Transform.translate 131. 4. },
              cubeWireframe "tm3d:sq:wire" vp camCube cubeBounds @ sqMarks)

    let stFaces =
        stepCuboids
        |> List.collect (fun c ->
            let col =
                if c.Depth = 0 then (148, 163, 184)
                elif not c.IsLeaf then
                    match c.Id with
                    | "Core" -> (96, 165, 250)
                    | "Graph" -> (52, 211, 153)
                    | _ -> (251, 146, 60)
                else branchColor c.Parent
            projectCuboidFaces ("tm3d:st:" + c.Id) vp camStep c.Bounds col (Paint.rgb 30 41 59) 0.65 1.0)
    let stMarks, _ = renderSortedFaces stFaces
    let stGroup =
        Group({ GroupSpec.create "tm3d:steptree" with Transform = Transform.translate 258. 4. }, stMarks)

    let divs =
        Path.ofCommands
            [ MoveTo(Point.create 126. 10.); LineTo(Point.create 126. 192.)
              MoveTo(Point.create 253. 10.); LineTo(Point.create 253. 192.) ]
        |> Mark.path "tm3d:divs"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.

    let lbl1 = subLabel "tm3d:lbl1" 63. 198. "CubeHilbert"
    let lbl2 = subLabel "tm3d:lbl2" 190. 198. "CubeSquarified"
    let lbl3 = subLabel "tm3d:lbl3" 317. 198. "Bladh StepTree"
    let note =
        Mark.text "tm3d:note" (Point.create 190. 214.) "Exploded 3D volumetric cuboids (0.15) vs 2.5D tiered +Y plinths"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.2
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    [ hGroup; sqGroup; stGroup; Leaf divs; Leaf lbl1; Leaf lbl2; Leaf lbl3; Leaf note ],
    Point.create (4. + hProbeLocal.X) (4. + hProbeLocal.Y)

let private codeCityTreeAndHeights =
    let tree =
        Hierarchy.node "Ranvier.D5" 0.
            [ Hierarchy.node "D5.Core" 0.
                [ Hierarchy.node "Scale" 34. []
                  Hierarchy.node "Contour" 28. []
                  Hierarchy.node "PowerVor" 42. []
                  Hierarchy.node "Gosper" 26. []
                  Hierarchy.node "RTree" 30. [] ]
              Hierarchy.node "D5.Graph" 0.
                [ Hierarchy.node "Sugiyama" 46. []
                  Hierarchy.node "Manifold" 52. []
                  Hierarchy.node "Bundling" 36. []
                  Hierarchy.node "Metro" 32. []
                  Hierarchy.node "Seriate" 28. [] ]
              Hierarchy.node "D5.3D" 0.
                [ Hierarchy.node "H3Ball" 32. []
                  Hierarchy.node "Pack3D" 36. []
                  Hierarchy.node "SphereS2" 26. []
                  Hierarchy.node "Kde3D" 30. [] ] ]
    let heights =
        Map.ofList
            [ "Scale", 38.; "Contour", 52.; "PowerVor", 86.; "Gosper", 46.; "RTree", 62.
              "Sugiyama", 94.; "Manifold", 100.; "Bundling", 72.; "Metro", 68.; "Seriate", 54.
              "H3Ball", 64.; "Pack3D", 78.; "SphereS2", 50.; "Kde3D", 66. ]
    tree, (fun id -> Map.tryFind id heights |> Option.defaultValue 20.)

let private renderCodeCityCard (phase: float) : SceneNode list * Point =
    let tree, heightFn = codeCityTreeAndHeights
    let cfg =
        { CodeCityConfig.create (Bounds.create -82. -68. 164. 136.) 72. heightFn with
            DistrictPlinthHeight = 6.5
            StreetPadding = 3.2 }
    let buildings = CodeCity.layout cfg tree |> ok
    let _ = CodeCity.toDiagramLayout buildings

    let vp : Viewport = { Width = 368.; Height = 190. }
    let cam = Camera3D.orbit (Point3.create 0. 24. 0.) (0.66 + 0.34 * phase) 0.44 265.

    let districtFaces =
        buildings
        |> List.filter (fun b -> b.IsDistrict)
        |> List.collect (fun b ->
            let baseCol =
                if b.Depth = 0 then (180, 194, 212)
                else (148, 168, 194)
            projectCuboidFaces ("city:dist:" + b.Id) vp cam b.Bounds baseCol (Paint.rgb 71 85 105) 0.65 1.0)

    let towerFaces =
        buildings
        |> List.filter (fun b -> not b.IsDistrict)
        |> List.collect (fun b ->
            let baseCol =
                match b.Parent with
                | Some "D5.Core" ->
                    if b.HeightMetric >= 75. then (29, 78, 216) else (59, 130, 246)
                | Some "D5.Graph" ->
                    if b.HeightMetric >= 85. then (225, 29, 72)
                    elif b.HeightMetric >= 65. then (245, 158, 11)
                    else (16, 185, 129)
                | _ ->
                    if b.HeightMetric >= 70. then (124, 58, 237) else (14, 116, 144)
            projectCuboidFaces ("city:bld:" + b.Id) vp cam b.Bounds baseCol (Paint.rgb 15 23 42) 0.65 1.0)

    let allMarks, _ = renderSortedFaces (districtFaces @ towerFaces)
    let _, towerFrontPt = renderSortedFaces towerFaces
    let cityGrp = Group({ GroupSpec.create "codecity:grp" with Transform = Transform.translate 6. 2. }, allMarks)

    let footer =
        Mark.text "codecity:footer" (Point.create 190. 208.) "X-Z lot area = module weight • +Y height = complexity • Slate plinths = packages"
        |> Mark.withFill (Paint.rgb 71 85 105)
        |> Mark.withFontSize 9.3
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }

    [ cityGrp; Leaf footer ], Point.create (6. + towerFrontPt.X) (2. + towerFrontPt.Y)

let private pack3DHierarchy =
    Hierarchy.node "World" 0.
        [ Hierarchy.node "P0" 0.
            [ Hierarchy.node "L0_0" 28. []
              Hierarchy.node "L0_1" 19. []
              Hierarchy.node "L0_2" 14. []
              Hierarchy.node "L0_3" 11. [] ]
          Hierarchy.node "P1" 0.
            [ Hierarchy.node "L1_0" 24. []
              Hierarchy.node "L1_1" 18. []
              Hierarchy.node "L1_2" 13. []
              Hierarchy.node "L1_3" 9. [] ]
          Hierarchy.node "P2" 0.
            [ Hierarchy.node "L2_0" 22. []
              Hierarchy.node "L2_1" 16. []
              Hierarchy.node "L2_2" 12. []
              Hierarchy.node "L2_3" 10. [] ]
          Hierarchy.node "P3" 0.
            [ Hierarchy.node "L3_0" 20. []
              Hierarchy.node "L3_1" 15. []
              Hierarchy.node "L3_2" 11. [] ] ]

let private renderPack3DCard (phase: float) : SceneNode list * Point =
    let bounds3 = Bounds3.create -68. -68. -68. 136. 136. 136.
    let spheres =
        Pack3D.layout { Pack3DConfig.create bounds3 with Padding = 2.0 } pack3DHierarchy |> ok
    let _ = Pack3D.toDiagramLayout spheres

    let vp : Viewport = { Width = 368.; Height = 190. }
    let cam = Camera3D.orbit (Point3.create 0. 0. 0.) (0.52 + 0.42 * phase) 0.28 242.

    let rootSphere = spheres |> List.find (fun s -> s.Depth = 0)
    let ringSteps = 48
    let rootRings =
        [ 0; 1; 2 ]
        |> List.choose (fun plane ->
            let pts =
                [ for k in 0 .. ringSteps ->
                    let a = float k * 2. * Math.PI / float ringSteps
                    let r = rootSphere.Radius
                    let w =
                        match plane with
                        | 0 -> Point3.create (rootSphere.Center.X + r * cos a) (rootSphere.Center.Y + r * sin a) rootSphere.Center.Z
                        | 1 -> Point3.create (rootSphere.Center.X + r * cos a) rootSphere.Center.Y (rootSphere.Center.Z + r * sin a)
                        | _ -> Point3.create rootSphere.Center.X (rootSphere.Center.Y + r * cos a) (rootSphere.Center.Z + r * sin a)
                    Camera3D.projectPoint vp cam w |> Option.map (fun (p, _, _) -> p) ]
            let p = Shape.line Curve.Linear pts
            if List.isEmpty (Path.commands p) then None
            else
                Some(
                    Leaf(
                        Mark.path (sprintf "pack3d:ring:%d" plane) p
                        |> Mark.withFill Paint.none
                        |> Mark.withStroke (Paint.rgb 148 163 184) 0.95
                        |> Mark.withOpacity 0.45
                    )
                ))

    let paletteFor (pid: string) =
        match pid with
        | "P0" -> (37, 99, 235), (96, 165, 250)
        | "P1" -> (16, 185, 129), (52, 211, 153)
        | "P2" -> (234, 88, 12), (251, 146, 60)
        | _ -> (190, 24, 93), (244, 114, 182)

    let projectedParents =
        spheres
        |> List.filter (fun s -> s.Depth = 1)
        |> List.choose (fun s ->
            Camera3D.projectPoint vp cam s.Center
            |> Option.map (fun (pt2, depth, pScale) -> s, pt2, depth, max 6. (s.Radius * pScale)))
        |> List.sortByDescending (fun (_, _, d, _) -> d)

    let parentMarks =
        projectedParents
        |> List.collect (fun (s, pt2, _, sr) ->
            let (dr, dg, db), (lr, lg, lb) = paletteFor s.Id
            let shell =
                Shape.arc 0. sr 0. (2. * Math.PI)
                |> Mark.path ("pack3d:par:" + s.Id)
                |> Mark.withFill (Paint.rgb lr lg lb)
                |> Mark.withStroke (Paint.rgb dr dg db) 1.35
                |> Mark.withOpacity 0.22
                |> Mark.withTransform (Transform.translate pt2.X pt2.Y)
            [ Leaf shell ])

    let projectedLeaves =
        spheres
        |> List.filter (fun s -> s.IsLeaf)
        |> List.choose (fun s ->
            Camera3D.projectPoint vp cam s.Center
            |> Option.map (fun (pt2, depth, pScale) -> s, pt2, depth, max 4.2 (s.Radius * pScale)))
        |> List.sortByDescending (fun (_, _, d, _) -> d)

    let leafMarks =
        projectedLeaves
        |> List.collect (fun (s, pt2, _, sr) ->
            let (dr, dg, db), (lr, lg, lb) = paletteFor (Option.defaultValue "P0" s.Parent)
            let body =
                Shape.arc 0. sr 0. (2. * Math.PI)
                |> Mark.path ("pack3d:leaf:" + s.Id)
                |> Mark.withFill (Paint.rgb dr dg db)
                |> Mark.withStroke (Paint.rgb 15 23 42) 0.85
                |> Mark.withTransform (Transform.translate pt2.X pt2.Y)
            let midHi =
                Shape.arc 0. (sr * 0.62) 0. (2. * Math.PI)
                |> Mark.path ("pack3d:hi:" + s.Id)
                |> Mark.withFill (Paint.rgb lr lg lb)
                |> Mark.withOpacity 0.55
                |> Mark.withTransform (Transform.translate (pt2.X - sr * 0.16) (pt2.Y - sr * 0.16))
            let spec =
                Shape.arc 0. (max 1.3 (sr * 0.20)) 0. (2. * Math.PI)
                |> Mark.path ("pack3d:spec:" + s.Id)
                |> Mark.withFill (Paint.rgb 240 249 255)
                |> Mark.withOpacity 0.80
                |> Mark.withTransform (Transform.translate (pt2.X - sr * 0.34) (pt2.Y - sr * 0.34))
            [ Leaf body; Leaf midHi; Leaf spec ])

    let _, frontPt, _, frontRad = List.last projectedLeaves
    let probeLocal = Point.create (frontPt.X + frontRad * 0.16) (frontPt.Y + frontRad * 0.16)
    let grp = Group({ GroupSpec.create "pack3d:grp" with Transform = Transform.translate 6. 2. }, rootRings @ parentMarks @ leafMarks)

    let footer =
        Mark.text "pack3d:footer" (Point.create 190. 208.) "Front-chain 3D tangent sphere placement + exact Welzl minimum enclosing balls"
        |> Mark.withFill (Paint.rgb 71 85 105)
        |> Mark.withFontSize 9.3
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }

    [ grp; Leaf footer ], Point.create (6. + probeLocal.X) (2. + probeLocal.Y)

let private voronoi3DPrecomputed =
    let cube = Bounds3.create -42. -40. -42. 84. 80. 84.
    let rawSeeds =
        Point3.create 0. 0. 0.
        :: [ for i in 0 .. 9 ->
               let z = -0.78 + 1.56 * (float i / 9.)
               let r = sqrt (max 0. (1. - z * z))
               let phi = float i * 2.3999632297
               Point3.create (26. * r * cos phi) (24. * z) (26. * r * sin phi) ]
    let _ = Voronoi3D.clipBounds cube rawSeeds |> ok
    let cvtCells, _ = Cvt3D.relax 5 cube rawSeeds |> ok

    let vtHierarchy =
        Hierarchy.node "Root3D" 0.
            [ Hierarchy.node "G0" 0.
                [ Hierarchy.node "V0_0" 30. []
                  Hierarchy.node "V0_1" 20. []
                  Hierarchy.node "V0_2" 14. [] ]
              Hierarchy.node "G1" 0.
                [ Hierarchy.node "V1_0" 26. []
                  Hierarchy.node "V1_1" 18. []
                  Hierarchy.node "V1_2" 12. [] ]
              Hierarchy.node "G2" 0.
                [ Hierarchy.node "V2_0" 24. []
                  Hierarchy.node "V2_1" 16. []
                  Hierarchy.node "V2_2" 11. [] ] ]
    let vtCells = VoronoiTreemap3D.layout 45 0.035 cube vtHierarchy |> ok
    cube, cvtCells, vtCells

let private renderVoronoi3DCard (phase: float) : SceneNode list * Point =
    let cube, cvtCells, vtCells = voronoi3DPrecomputed
    let vp : Viewport = { Width = 178.; Height = 180. }
    let cam = Camera3D.orbit (Point3.create 0. 0. 0.) (0.64 + 0.36 * phase) 0.38 214.

    // Left: Cvt3D / Voronoi3D convex polyhedral cells shrunk slightly toward cell centroids to reveal interior facets
    let cvtFaces =
        cvtCells
        |> List.collect (fun cell ->
            let vArr =
                cell.Vertices
                |> List.map (fun v -> Point3.lerp cell.Centroid v 0.83)
                |> List.toArray
            let col = spectrumRgb (float cell.Site / float (max 1 (cvtCells.Length - 1)))
            projectPolyhedronFaces (sprintf "cvt3d:c%d" cell.Site) vp cam vArr cell.Faces cell.Centroid col (Paint.rgb 15 23 42) 0.65 1.0)
    let cvtMarks, cvtFront = renderSortedFaces cvtFaces
    let leftGrp =
        Group({ GroupSpec.create "v3d:left" with Transform = Transform.translate 6. 6. },
              cubeWireframe "cvt3d:wire" vp cam cube @ cvtMarks)

    // Right: Volumetric VoronoiTreemap3D 2-level polyhedral hierarchy
    let vtLeaves = vtCells |> List.filter (fun c -> c.Depth = 2)
    let vtFaces =
        vtLeaves
        |> List.collect (fun cell ->
            let vArr =
                cell.Vertices
                |> List.map (fun v -> Point3.lerp cell.Centroid v 0.85)
                |> List.toArray
            let baseCol =
                match cell.Parent with
                | Some "G0" -> (37, 99, 235)
                | Some "G1" -> (16, 185, 129)
                | _ -> (225, 29, 72)
            projectPolyhedronFaces ("vt3d:" + cell.Id) vp cam vArr cell.Faces cell.Centroid baseCol (Paint.rgb 15 23 42) 0.70 1.0)
    let vtMarks, _ = renderSortedFaces vtFaces
    let rightGrp =
        Group({ GroupSpec.create "v3d:right" with Transform = Transform.translate 196. 6. },
              cubeWireframe "vt3d:wire" vp cam cube @ vtMarks)

    let div =
        Path.ofCommands [ MoveTo(Point.create 190. 10.); LineTo(Point.create 190. 192.) ]
        |> Mark.path "v3d:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.
    let lblL = subLabel "v3d:lblL" 95. 198. "Cvt3D / Voronoi3D Polyhedra"
    let lblR = subLabel "v3d:lblR" 285. 198. "VoronoiTreemap3D (2-Level)"
    let footer =
        Mark.text "v3d:footer" (Point.create 190. 214.) "Exact 3D half-space convex polyhedron clipping + Laguerre power weights"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.2
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    [ leftGrp; rightGrp; Leaf div; Leaf lblL; Leaf lblR; Leaf footer ],
    Point.create (6. + cvtFront.X) (6. + cvtFront.Y)

let probesHierarchies3D (phase: float) : (string * Point) list =
    let _, p1 = renderTreemap3DCard phase
    let _, p2 = renderCodeCityCard phase
    let _, p3 = renderPack3DCard phase
    let _, p4 = renderVoronoi3DCard phase
    [ "hier3d:treemap3d", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "hier3d:codecity", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "hier3d:pack3d", Point.create (14. + p3.X) (304. + 24. + p3.Y)
      "hier3d:voronoi3d", Point.create (406. + p4.X) (304. + 24. + p4.Y) ]

let sceneHierarchies3D (phase: float) : Scene =
    let n1, _ = renderTreemap3DCard phase
    let n2, _ = renderCodeCityCard phase
    let n3, _ = renderPack3DCard phase
    let n4, _ = renderVoronoi3DCard phase

    let c1, clip1 = panelCard "c-tm3d" 14. 42. 380. 252. "Treemap3D: CubeHilbert, CubeSquarified & Bladh StepTree" n1
    let c2, clip2 = panelCard "c-codecity" 406. 42. 380. 252. "Wettel-Lanza 2.5D Software CodeCity" n2
    let c3, clip3 = panelCard "c-pack3d" 14. 304. 380. 254. "Pack3D Tangent-Sphere Welzl Hierarchy" n3
    let c4, clip4 = panelCard "c-voronoi3d" 406. 304. 380. 254. "Rycroft Voronoi3D / Cvt3D & Volumetric VoronoiTreemap3D" n4

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "hier3d:bg"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "hier3d:title" (Point.create 16. 27.) "Ranvier.D5.Graph3D — 3D Treemaps, StepTree, CodeCity, Pack3D & Volumetric 3D Voronoi"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 16.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose
        { Width = 800.; Height = 570. }
        "3D Volumetric Hierarchies and Polyhedral Voronoi Layouts"
        [ clip1; clip2; clip3; clip4 ]
        [ Leaf pageBg; Leaf title; c1; c2; c3; c4 ]

// ===========================================================================
// PLATE 7: spherical-manifolds-3d
// SphericalFibonacci + SphericalVoronoi CVT + SphericalFibonacci3D
// + SphericalStress3D + HierarchicalBundle3D + KdeBundle3D
// + Umap3D / Tsne3D / Isomap3D / Sammon3D
// ===========================================================================

let private sphericalGeoPrecomputed =
    let fibLattice = SphericalFibonacci.points 150 |> ok
    let observations =
        [ for i in 0 .. 239 ->
            let lon = -180. + 360. * ((float (i * 37 % 240)) / 240.)
            let lat = 68. * sin (float i * 0.27) * cos (float i * 0.11)
            let w = 1.0 + 2.5 * abs (cos (lon * Math.PI / 90.))
            GeoPosition.create lon lat |> ok, w ]
    let fibBins = SphericalFibonacci.aggregate 64 fst snd observations |> ok

    let seedPts =
        SphericalFibonacci.points 38
        |> ok
        |> List.mapi (fun idx pos ->
            let dLon = 9. * sin (float (idx * 5 + 2))
            let dLat = Math.Clamp(pos.Latitude + 5. * cos (float (idx * 3 + 1)), -82., 82.)
            sprintf "SV%d" idx, (GeoPosition.create (pos.Longitude + dLon) dLat |> ok))
    let _ = SphericalVoronoi.compute seedPts |> ok
    let cvtCells = SphericalVoronoi.relaxCvt 8 seedPts |> ok

    let orientedPolys =
        cvtCells
        |> List.choose (fun cell ->
            if cell.Vertices.Length < 3 then None
            else
                let revV = List.rev cell.Vertices
                let cwRing = revV @ [ revV.Head ]
                let ccwRing = cell.Vertices @ [ cell.Vertices.Head ]
                let cwGeom = GeoGeometry.Polygon [ cwRing ]
                let areaCw = GeoMeasure.area GeoStreamConfig.defaultValue cwGeom |> ok
                let geom = if areaCw <= 2. * Math.PI then cwGeom else GeoGeometry.Polygon [ ccwRing ]
                Some(cell, geom))

    let graticule =
        GeoGeometry.MultiLineString
            (Graticule.create
                { GraticuleConfig.defaultValue with
                    MinorLongitudeStep = 30.
                    MinorLatitudeStep = 30. }
             |> ok).Lines
    fibLattice, fibBins, orientedPolys, graticule

let private renderSphericalGeoCard (phase: float) : SceneNode list * Point =
    let fibLattice, fibBins, orientedPolys, graticule = sphericalGeoPrecomputed
    let leftBounds = Bounds.create 8. 8. 172. 172.
    let rightBounds = Bounds.create 198. 8. 172. 172.

    let mkOrtho (bounds: Bounds) (rotLon: float) (rotLat: float) =
        Projection.create
            { ProjectionConfig.defaultValue ProjectionKind.Orthographic with
                Scale = 74.
                Translate = Point.create (Bounds.x bounds + Bounds.width bounds * 0.5) (Bounds.y bounds + Bounds.height bounds * 0.5)
                Rotation = GeoRotation.create rotLon rotLat 0. |> ok }
        |> ok

    let orthoL = mkOrtho leftBounds (-20. + 22. * phase) -18.
    let orthoR = mkOrtho rightBounds (-15. - 20. * phase) -16.

    let optsL = { GeoPathOptions.defaultValue with ClipExtent = Some leftBounds; Precision = 0.8 }
    let optsR = { GeoPathOptions.defaultValue with ClipExtent = Some rightBounds; Precision = 0.8 }

    let oceanL =
        GeoPath.project optsL orthoL GeoGeometry.Sphere
        |> ok
        |> List.mapi (fun i p ->
            Leaf(
                Mark.path (sprintf "sgeo:oceanL:%d" i) p
                |> Mark.withFill (Paint.rgb 219 234 254)
                |> Mark.withStroke (Paint.rgb 59 130 246) 1.2
            ))
    let gratL =
        GeoPath.project optsL orthoL graticule
        |> ok
        |> List.mapi (fun i p ->
            Leaf(
                Mark.path (sprintf "sgeo:gratL:%d" i) p
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 147 197 253) 0.7
            ))

    let latticeDots =
        fibLattice
        |> List.mapi (fun i pos -> i, pos)
        |> List.choose (fun (i, pos) ->
            Projection.project orthoL pos
            |> Option.map (fun pt ->
                Leaf(discMark (sprintf "sgeo:fib:%d" i) pt 1.8 (Paint.rgb 100 116 139) Paint.none 0.)))

    let maxBinW =
        match fibBins with
        | [] -> 1.
        | _ -> fibBins |> List.map (fun b -> b.TotalWeight) |> List.max |> max 1.

    let binDots =
        fibBins
        |> List.choose (fun bin ->
            Projection.project orthoL bin.Center
            |> Option.map (fun pt ->
                let t = bin.TotalWeight / maxBinW
                let rad = 3.0 + 4.4 * t
                Leaf(discMark (sprintf "sgeo:bin:%d" bin.CellIndex) pt rad (spectrumPaint t) (Paint.rgb 15 23 42) 0.8)))

    let oceanR =
        GeoPath.project optsR orthoR GeoGeometry.Sphere
        |> ok
        |> List.mapi (fun i p ->
            Leaf(
                Mark.path (sprintf "sgeo:oceanR:%d" i) p
                |> Mark.withFill (Paint.rgb 30 64 175)
                |> Mark.withStroke (Paint.rgb 30 41 59) 1.2
            ))

    let voronoiMarks =
        orientedPolys
        |> List.mapi (fun idx (cell, geom) ->
            let paths = GeoPath.project optsR orthoR geom |> ok
            let latT = (cell.Site.Latitude + 85.) / 170.
            let areaT = Math.Clamp((cell.Steradians - 0.18) / 0.30, 0., 1.)
            let r, g, b = spectrumRgb (0.65 * latT + 0.35 * areaT)
            let clampC c = min 228 (max 18 c)
            paths
            |> List.mapi (fun pi p ->
                Leaf(
                    Mark.path (sprintf "sgeo:vcell:%s:%d_%d" cell.Id idx pi) p
                    |> Mark.withFill (Paint.rgb (clampC r) (clampC g) (clampC b))
                    |> Mark.withStroke (Paint.rgb 226 232 240) 0.95
                )))
        |> List.concat

    let voronoiSites =
        orientedPolys
        |> List.choose (fun (cell, _) ->
            Projection.project orthoR cell.Site
            |> Option.map (fun pt ->
                Leaf(discMark ("sgeo:vsite:" + cell.Id) pt 2.1 (Paint.rgb 15 23 42) (Paint.rgb 226 232 240) 0.6)))

    let div =
        Path.ofCommands [ MoveTo(Point.create 190. 10.); LineTo(Point.create 190. 192.) ]
        |> Mark.path "sgeo:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.
    let lblL = subLabel "sgeo:lblL" 94. 198. "SphericalFibonacci + Aggregate"
    let lblR = subLabel "sgeo:lblR" 284. 198. "SphericalVoronoi Lloyd CVT"
    let footer =
        Mark.text "sgeo:footer" (Point.create 190. 214.) "O(1) Keinert equal-area lattice binning | Exact S^2 convex hull dual & Girard steradians"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.1
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    // Center of right orthographic globe (284, 94) is strictly inside the clipped visible S^2 Voronoi tiling
    let rightCenter = Point.create 284. 94.
    oceanL @ gratL @ latticeDots @ binDots @ oceanR @ voronoiMarks @ voronoiSites @ [ Leaf div; Leaf lblL; Leaf lblR; Leaf footer ],
    rightCenter

let private sphericalGraphLayouts =
    // Polyhedral geodesic prism / cage graph (20 nodes, 34 edges)
    let nodes = [ for i in 0 .. 19 -> DiagramNode.sized (sprintf "G%d" i) 10. 10. ]
    let ringAssocs =
        [ for r in 0 .. 1 do
            for k in 0 .. 9 do
                let u = r * 10 + k
                let v = r * 10 + ((k + 1) % 10)
                yield Association.create (sprintf "sr_%d_%d" r k) (sprintf "G%d" u) (sprintf "G%d" v)
          for k in 0 .. 9 do
            yield Association.create (sprintf "rung_%d" k) (sprintf "G%d" k) (sprintf "G%d" (10 + k))
          for k in 0 .. 4 do
            yield Association.create (sprintf "diag_%d" k) (sprintf "G%d" (2 * k)) (sprintf "G%d" (10 + (2 * k + 3) % 10)) ]
    let graph = DiagramGraph.simple nodes ringAssocs |> ok
    let stressLayout =
        SphericalStress3D.layout
            { SphericalLayoutConfig.create (Point3.create 0. 0. 0.) 68. with
                Iterations = 50
                ArcSegments = 14
                Seed = 42UL }
            graph
        |> ok
    let fibLayout =
        SphericalFibonacci3D.layout (Point3.create 0. 0. 0.) 42. 12 graph |> ok
    stressLayout, fibLayout

let private renderSphericalStressCard (phase: float) : SceneNode list * SceneDefinition list * Point =
    let stressLayout, fibLayout = sphericalGraphLayouts
    let vpL : Viewport = { Width = 224.; Height = 184. }
    let camL = Camera3D.orbit (Point3.create 0. 0. 0.) (0.45 + 0.48 * phase) 0.30 205.
    let themeL =
        { Scene3DTheme.create vpL.Width vpL.Height "SphericalStress3D" with
            ReferenceSphereRadius = Some 68.
            NodeFill = fun id ->
                let idx = Int32.Parse(id.Substring 1)
                if idx < 10 then Paint.rgb 37 99 235 else Paint.rgb 16 185 129
            AssociationStroke = fun aid _ ->
                if aid.StartsWith "diag_" then Paint.rgb 245 158 11 else Paint.rgb 51 65 85
            NodeLabel = fun id -> if id = "G0" || id = "G5" || id = "G12" then Some id else None }
    let subL = Scene3D.render themeL camL stressLayout
    let grpL, defsL = embedSubScene "sph-stress" 4. 4. subL

    let vpR : Viewport = { Width = 142.; Height = 184. }
    let camR = Camera3D.orbit (Point3.create 0. 0. 0.) (-0.35 + 0.48 * phase) 0.34 186.
    let themeR =
        { Scene3DTheme.create vpR.Width vpR.Height "SphericalFibonacci3D" with
            ReferenceSphereRadius = Some 42.
            NodeFill = fun id ->
                let idx = Int32.Parse(id.Substring 1)
                spectrumPaint (float idx / 19.)
            AssociationStroke = fun _ _ -> Paint.rgb 79 70 229
            NodeLabel = fun _ -> None }
    let subR = Scene3D.render themeR camR fibLayout
    let grpR, defsR = embedSubScene "sph-fib3d" 232. 4. subR

    let div =
        Path.ofCommands [ MoveTo(Point.create 230. 10.); LineTo(Point.create 230. 192.) ]
        |> Mark.path "sph3d:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.
    let lblL = subLabel "sph3d:lblL" 116. 198. "SphericalStress3D (Riemannian SGD)"
    let lblR = subLabel "sph3d:lblR" 303. 198. "SphericalFibonacci3D"
    let footer =
        Mark.text "sph3d:footer" (Point.create 190. 214.) "Tangent-space exponential map exp_u + slerped great-circle Point3Chain arcs"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.1
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let frontPt = frontmostNode2D vpL camL stressLayout
    [ grpL; grpR; Leaf div; Leaf lblL; Leaf lblR; Leaf footer ],
    (defsL @ defsR),
    Point.create (4. + frontPt.X) (4. + frontPt.Y)

let private bundling3DPrecomputed =
    // Left: 3D Hierarchical Edge Bundling over 4 spatial clusters on a 3D ring/cage
    let hebNodes3D =
        [ for c in 0 .. 3 do
            let baseAng = float c * (Math.PI * 0.5)
            for k in 0 .. 3 do
                let nid = sprintf "B%d_%d" c k
                let ang = baseAng + (float k - 1.5) * 0.22
                let y = (float (k % 2) - 0.5) * 34. + (if c % 2 = 0 then 12. else -12.)
                let r = 54.
                let pt = Point3.create (r * cos ang) y (r * sin ang)
                yield
                    { Id = nid
                      Kind = NodeKind.Standard
                      Center = pt
                      Bounds = Bounds3.create (pt.X - 4.) (pt.Y - 4.) (pt.Z - 4.) 8. 8. 8.
                      Radius = 4.6
                      ScaleFactor = 1.0
                      ParentGroup = None
                      Layer = Some c } ]
    let hebMap = hebNodes3D |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
    let hebPairs =
        [ "B0_0","B2_1"; "B0_1","B2_2"; "B0_2","B1_1"; "B0_3","B3_2"
          "B1_0","B3_0"; "B1_2","B3_3"; "B1_3","B2_0"; "B2_3","B0_1"
          "B3_1","B0_3"; "B1_1","B2_3"; "B0_0","B1_3"; "B2_0","B3_2" ]
    let hebAssocs3D =
        hebPairs
        |> List.mapi (fun idx (u, v) ->
            let pu, pv = Map.find u hebMap, Map.find v hebMap
            { Id = sprintf "hb3_%d" idx
              Source = Endpoint.node u
              Target = Endpoint.node v
              Kind = AssociationKind.Directed
              IsBackbone = true
              Points = Point3Chain.ofList [ pu; pv ]
              Waypoints = [ pu; pv ] })
    let rawHeb3D : DiagramLayout3D =
        { Bounds = Bounds3.create -64. -48. -64. 128. 96. 128.
          Nodes = hebNodes3D
          Groups = []
          Associations = hebAssocs3D }
    let hebTree =
        Hierarchy.node "root3d" 0.
            [ for c in 0 .. 3 ->
                Hierarchy.node (sprintf "Cluster%d" c) 0.
                    [ for k in 0 .. 3 -> Hierarchy.node (sprintf "B%d_%d" c k) 1. [] ] ]
    let hebBundled3D = HierarchicalBundle3D.bundle 0.86 22 hebTree rawHeb3D |> ok

    // Right: Volumetric CUBu KdeBundle3D inside a 3D cube
    let kdeCube = Bounds3.create -54. -48. -54. 108. 96. 108.
    let kdeHubs =
        [| (-42., 32., -38.)
           (-40., -34., 36.)
           (42., 34., 38.)
           (40., -32., -36.) |]
    let kdeNodes3D =
        [ for h in 0 .. 3 do
            let hx, hy, hz = kdeHubs[h]
            for s in 0 .. 3 do
                let nid = sprintf "K%d_%d" h s
                let pt =
                    Point3.create
                        (hx + 8. * sin (float (h * 5 + s)))
                        (hy + 8. * cos (float (h * 3 + s * 2)))
                        (hz + 8. * sin (float (h * 7 + s * 3)))
                yield
                    { Id = nid
                      Kind = NodeKind.Standard
                      Center = pt
                      Bounds = Bounds3.create (pt.X - 4.) (pt.Y - 4.) (pt.Z - 4.) 8. 8. 8.
                      Radius = 4.2
                      ScaleFactor = 1.0
                      ParentGroup = None
                      Layer = Some h } ]
    let kdeMap = kdeNodes3D |> List.map (fun n -> n.Id, n.Center) |> Map.ofList
    let kdePairs =
        [ "K0_0","K2_0"; "K0_1","K2_1"; "K0_2","K2_2"; "K0_3","K2_3"
          "K1_0","K3_0"; "K1_1","K3_1"; "K1_2","K3_2"; "K1_3","K3_3"
          "K0_0","K3_1"; "K0_2","K3_3"; "K1_0","K2_1"; "K1_2","K2_3"
          "K0_1","K1_2"; "K2_0","K3_2" ]
    let kdeAssocs3D =
        kdePairs
        |> List.mapi (fun idx (u, v) ->
            let pu, pv = Map.find u kdeMap, Map.find v kdeMap
            { Id = sprintf "kb3_%d" idx
              Source = Endpoint.node u
              Target = Endpoint.node v
              Kind = AssociationKind.Directed
              IsBackbone = true
              Points = Point3Chain.ofList [ pu; pv ]
              Waypoints = [ pu; pv ] })
    let rawKde3D : DiagramLayout3D =
        { Bounds = kdeCube
          Nodes = kdeNodes3D
          Groups = []
          Associations = kdeAssocs3D }
    let kdeBundled3D =
        KdeBundle3D.bundle
            { KdeBundle3DConfig.defaultValue with
                GridSize = 36
                Iterations = 8
                InitialKernelRadius = 4.8
                StepSize = 1.65
                SamplePointsPerEdge = 20 }
            rawKde3D
        |> ok
    hebBundled3D, kdeCube, kdeBundled3D

let private renderBundling3DCard (phase: float) : SceneNode list * SceneDefinition list * Point =
    let hebBundled3D, kdeCube, kdeBundled3D = bundling3DPrecomputed
    let vp : Viewport = { Width = 180.; Height = 184. }
    let camL = Camera3D.orbit (Point3.create 0. 0. 0.) (0.58 + 0.42 * phase) 0.34 198.
    let camR = Camera3D.orbit (Point3.create 0. 0. 0.) (-0.52 + 0.42 * phase) 0.34 205.

    let commColor (id: string) =
        if id.StartsWith "B0" then Paint.rgb 37 99 235
        elif id.StartsWith "B1" then Paint.rgb 16 185 129
        elif id.StartsWith "B2" then Paint.rgb 245 158 11
        else Paint.rgb 225 29 72

    let themeHeb =
        { Scene3DTheme.create vp.Width vp.Height "HierarchicalBundle3D" with
            ReferenceSphereRadius = Some 58.
            NodeFill = commColor
            AssociationStroke = fun aid _ ->
                let idx = Int32.Parse(aid.Substring 4)
                if idx % 3 = 0 then Paint.rgb 37 99 235
                elif idx % 3 = 1 then Paint.rgb 16 185 129
                else Paint.rgb 225 29 72
            NodeLabel = fun _ -> None }
    let subHeb = Scene3D.render themeHeb camL hebBundled3D
    let grpHeb, defsHeb = embedSubScene "heb3d" 4. 4. subHeb

    let themeKde =
        { Scene3DTheme.create vp.Width vp.Height "KdeBundle3D" with
            NodeFill = fun id ->
                if id.StartsWith "K0" || id.StartsWith "K2" then Paint.rgb 14 116 144
                else Paint.rgb 124 58 237
            AssociationStroke = fun aid _ ->
                let idx = Int32.Parse(aid.Substring 4)
                if idx < 8 then Paint.rgb 13 148 136 else Paint.rgb 190 24 93
            NodeLabel = fun _ -> None }
    let subKde = Scene3D.render themeKde camR kdeBundled3D
    let grpKdeSub, defsKde = embedSubScene "kde3d:sub" 0. 0. subKde
    let grpKde =
        Group({ GroupSpec.create "kde3d" with Transform = Transform.translate 194. 4. },
              cubeWireframe "kde3d:wire" vp camR kdeCube @ [ grpKdeSub ])

    let div =
        Path.ofCommands [ MoveTo(Point.create 190. 10.); LineTo(Point.create 190. 192.) ]
        |> Mark.path "b3d:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.
    let lblL = subLabel "b3d:lblL" 94. 198. "HierarchicalBundle3D (Splines)"
    let lblR = subLabel "b3d:lblR" 284. 198. "CUBu KdeBundle3D (36^3 Grid)"
    let footer =
        Mark.text "b3d:footer" (Point.create 190. 214.) "3D LCA clamped B-splines vs trilinear voxel splatting & gradient advection"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.1
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let frontPt = frontmostNode2D vp camL hebBundled3D
    [ grpHeb; grpKde; Leaf div; Leaf lblL; Leaf lblR; Leaf footer ],
    (defsHeb @ defsKde),
    Point.create (4. + frontPt.X) (4. + frontPt.Y)

let private manifold3DPrecomputed =
    let cube = Bounds3.create -38. -32. -38. 76. 64. 76.
    // Shared 36-point R^10 clustered dataset for Umap3D & Tsne3D
    let protos =
        [| [|  3.0;  2.6; -2.2;  1.9;  2.4; -1.8;  2.1;  1.5; -2.0;  2.2 |]
           [| -2.8;  2.5;  2.7; -2.1;  1.6;  2.3; -1.9;  2.4;  1.8; -2.1 |]
           [|  0.4; -3.1;  0.6;  2.6; -2.5; -2.2;  2.3; -2.0;  2.5;  1.9 |] |]
    let clusterRows =
        [ for c in 0 .. 2 do
            let p = protos[c]
            for i in 0 .. 11 do
                let feats =
                    [ for d in 0 .. 9 ->
                        p[d] * 0.65 + 0.95 * sin (float (c * 31 + i * 11 + d * 7)) + 0.65 * cos (float (c * 17 + i * 23 + d * 5)) ]
                yield sprintf "M%d_%d" c i, feats ]
    let clusterDs = FeatureDataset.ofRows clusterRows |> ok
    let knn3D = NnDescent.buildGraph DistanceMetric.Euclidean 3 42u clusterDs |> ok

    let umapPts =
        Umap3D.embed { Umap3DConfig.create cube with Neighbors = 8; Epochs = 140; Seed = 42u } clusterDs |> ok
    let umapDiag = Umap3D.toDiagramLayout 4.2 umapPts knn3D

    let tsnePts =
        Tsne3D.embed { Tsne3DConfig.create cube with Perplexity = 8.; Iterations = 180; LearningRate = 110.; Seed = 42u } clusterDs |> ok

    // Isomap3D on a 3D S-manifold ribbon embedded in R^6
    let isoRows =
        [ for i in 0 .. 35 ->
            let u = float i / 35.
            let ang = (u - 0.5) * 2.6 * Math.PI
            let x = sin ang
            let y = (float ((i * 5) % 9) / 8. - 0.5) * 2.0
            let z = sign ang |> float |> fun sg -> sg * (1. - cos ang)
            sprintf "IS%d" i, [ x * 3.0; y * 2.4; z * 2.5; 0.6 * cos ang; 0.6 * sin ang; u * 1.5 ] ]
    let isoDs = FeatureDataset.ofRows isoRows |> ok
    let isoPts = Isomap3D.embed cube DistanceMetric.Euclidean 6 isoDs |> ok

    // Sammon3D on a 3D torus-knot curve embedded in R^6
    let samRows =
        [ for i in 0 .. 35 ->
            let t = 2. * Math.PI * float i / 36.
            let r = 2.4 + 0.85 * cos (3. * t)
            let x = r * cos (2. * t)
            let y = r * sin (2. * t)
            let z = 1.4 * sin (3. * t)
            sprintf "SA%d" i, [ x; y; z; 0.8 * cos t; 0.8 * sin t; 0.5 * cos (2. * t) ] ]
    let samDs = FeatureDataset.ofRows samRows |> ok
    let samPts = Sammon3D.embed cube DistanceMetric.Euclidean 140 42u samDs |> ok

    cube, umapDiag, tsnePts, isoPts, samPts

let private renderPointCloud3D
    (prefix: string)
    (vp: Viewport)
    (cam: Camera3D)
    (cube: Bounds3)
    (pts: EmbeddedPoint3D<string> list)
    (colorFn: int -> EmbeddedPoint3D<string> -> Paint)
    (connectSequential: bool)
    : SceneNode list =
    let wire = cubeWireframe (prefix + ":wire") vp cam cube
    let seqLine =
        if connectSequential then
            let p2s =
                pts
                |> List.map (fun p -> Camera3D.projectPoint vp cam p.Position |> Option.map (fun (pt2, _, _) -> pt2))
            let closed = if pts.Length > 2 then p2s @ [ p2s.Head ] else p2s
            [ Leaf(
                Mark.path (prefix + ":curve") (Shape.line Curve.Linear closed)
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 100 116 139) 1.15
                |> Mark.withOpacity 0.62
              ) ]
        else []
    let projected =
        pts
        |> List.mapi (fun idx p ->
            Camera3D.projectPoint vp cam p.Position
            |> Option.map (fun (pt2, depth, pScale) -> idx, p, pt2, depth, pScale))
        |> List.choose id
        |> List.sortByDescending (fun (_, _, _, d, _) -> d)
    let dots =
        projected
        |> List.map (fun (idx, p, pt2, _, pScale) ->
            let rad = max 2.5 (min 5.5 (3.9 * pScale * 1.15))
            Leaf(discMark (sprintf "%s:pt:%s" prefix p.Id) pt2 rad (colorFn idx p) (Paint.rgb 255 255 255) 0.75))
    wire @ seqLine @ dots

let private renderManifold3DCard (phase: float) : SceneNode list * SceneDefinition list * Point =
    let cube, umapDiag, tsnePts, isoPts, samPts = manifold3DPrecomputed
    let vp : Viewport = { Width = 178.; Height = 92. }
    let cam = Camera3D.orbit (Point3.create 0. 0. 0.) (0.62 + 0.44 * phase) 0.36 168.

    let clusterColor (id: string) =
        if id.StartsWith "M0" then Paint.rgb 37 99 235
        elif id.StartsWith "M1" then Paint.rgb 16 185 129
        else Paint.rgb 225 29 72

    // 1. Umap3D + Umap3D.toDiagramLayout
    let umapTheme =
        { Scene3DTheme.create vp.Width vp.Height "Umap3D" with
            NodeFill = clusterColor
            AssociationStroke = fun _ isBackbone -> if isBackbone then Paint.rgb 71 85 105 else Paint.rgb 148 163 184
            NodeLabel = fun _ -> None }
    let subUmap = Scene3D.render umapTheme cam umapDiag
    let grpUmapSub, defsUmap = embedSubScene "m3d:umap:sub" 0. 0. subUmap
    let grpUmap =
        Group({ GroupSpec.create "m3d:umap" with Transform = Transform.translate 6. 4. },
              cubeWireframe "m3d:umap:wire" vp cam cube @ [ grpUmapSub ])

    // 2. Tsne3D
    let grpTsne =
        Group({ GroupSpec.create "m3d:tsne" with Transform = Transform.translate 196. 4. },
              renderPointCloud3D "m3d:tsne" vp cam cube tsnePts (fun _ p -> clusterColor p.Id) false)

    // 3. Isomap3D
    let grpIso =
        Group({ GroupSpec.create "m3d:iso" with Transform = Transform.translate 6. 114. },
              renderPointCloud3D "m3d:iso" vp cam cube isoPts (fun idx _ -> spectrumPaint (float idx / 35.)) false)

    // 4. Sammon3D
    let grpSam =
        Group({ GroupSpec.create "m3d:sam" with Transform = Transform.translate 196. 114. },
              renderPointCloud3D "m3d:sam" vp cam cube samPts (fun idx _ -> spectrumPaint (float idx / 35.)) true)

    let divs =
        Path.ofCommands
            [ MoveTo(Point.create 190. 6.); LineTo(Point.create 190. 220.)
              MoveTo(Point.create 8. 110.); LineTo(Point.create 372. 110.) ]
        |> Mark.path "m3d:divs"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.

    let lbl1 = subLabel "m3d:lbl1" 95. 105. "Umap3D + k-NN Skeleton"
    let lbl2 = subLabel "m3d:lbl2" 285. 105. "Octree Barnes-Hut Tsne3D"
    let lbl3 = subLabel "m3d:lbl3" 95. 219. "Geodesic Isomap3D"
    let lbl4 = subLabel "m3d:lbl4" 285. 219. "Sammon3D Torus-Knot"

    let frontUmap = frontmostNode2D vp cam umapDiag
    [ grpUmap; grpTsne; grpIso; grpSam; Leaf divs; Leaf lbl1; Leaf lbl2; Leaf lbl3; Leaf lbl4 ],
    defsUmap,
    Point.create (6. + frontUmap.X) (4. + frontUmap.Y)

let probesSphericalManifolds3D (phase: float) : (string * Point) list =
    let _, p1 = renderSphericalGeoCard phase
    let _, _, p2 = renderSphericalStressCard phase
    let _, _, p3 = renderBundling3DCard phase
    let _, _, p4 = renderManifold3DCard phase
    [ "sph3d:voronoi-cvt", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "sph3d:stress3d", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "sph3d:heb3d", Point.create (14. + p3.X) (304. + 24. + p3.Y)
      "sph3d:umap3d", Point.create (406. + p4.X) (304. + 24. + p4.Y) ]

let sceneSphericalManifolds3D (phase: float) : Scene =
    let n1, _ = renderSphericalGeoCard phase
    let n2, defs2, _ = renderSphericalStressCard phase
    let n3, defs3, _ = renderBundling3DCard phase
    let n4, defs4, _ = renderManifold3DCard phase

    let c1, clip1 = panelCard "c-sgeo" 14. 42. 380. 252. "Keinert SphericalFibonacci & SphericalVoronoi CVT" n1
    let c2, clip2 = panelCard "c-sstress" 406. 42. 380. 252. "SphericalFibonacci3D & Riemannian SphericalStress3D" n2
    let c3, clip3 = panelCard "c-b3d" 14. 304. 380. 254. "3D Bundling: HierarchicalBundle3D & CUBu KdeBundle3D" n3
    let c4, clip4 = panelCard "c-m3d" 406. 304. 380. 254. "3D Manifolds: Umap3D, Tsne3D, Isomap3D & Sammon3D" n4

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "sphman3d:bg"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "sphman3d:title" (Point.create 16. 27.) "Ranvier.D5.Geo & Graph3D — Spherical Voronoi, Geodesic Stress, 3D Bundling & 3D Manifolds"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 16.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose
        { Width = 800.; Height = 570. }
        "Spherical Tessellations, Geodesic Layouts, 3D Bundling and 3D Manifolds"
        ([ clip1; clip2; clip3; clip4 ] @ defs2 @ defs3 @ defs4)
        [ Leaf pageBg; Leaf title; c1; c2; c3; c4 ]

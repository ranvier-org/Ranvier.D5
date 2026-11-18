namespace Ranvier.D5.Graph3D

open System
open Ranvier.D5
open Ranvier.D5.Graph

/// <summary>Projection lens model for pure CPU 3D-to-2D SVG fallback rendering.</summary>
[<RequireQualifiedAccess>]
type ProjectionLens3D =
    /// <summary>Perspective pinhole camera with vertical field-of-view in radians.</summary>
    | Perspective of fovYRadians: float
    /// <summary>Parallel orthographic projection with world-units-to-pixels scale.</summary>
    | Orthographic of scale: float

/// <summary>Immutable 3D viewing camera supporting orbit/turntable spherical angles and pure CPU projection.</summary>
type Camera3D =
    { /// <summary>Camera eye position in 3D world units.</summary>
      Eye: Point3
      /// <summary>Look-at target position in 3D world units.</summary>
      Target: Point3
      /// <summary>Up vector in 3D world units.</summary>
      Up: Point3
      /// <summary>Perspective or orthographic projection lens.</summary>
      Lens: ProjectionLens3D
      /// <summary>Positive near-clip plane distance.</summary>
      Near: float }

/// <summary>Constructors and pure 3D-to-2D projection helpers for <c>Camera3D</c>.</summary>
[<RequireQualifiedAccess>]
module Camera3D =
    /// <summary>Creates a perspective orbit camera around <c>target</c> at spherical <c>(azimuth, elevation, distance)</c>.</summary>
    let orbit (target: Point3) (azimuthRadians: float) (elevationRadians: float) (distance: float) : Camera3D =
        if not (Double.IsFinite azimuthRadians && Double.IsFinite elevationRadians && Double.IsFinite distance) || distance <= 0. then
            invalidArg "distance" "Orbit camera requires finite angles and positive distance."
        let clampedElev = max (-Math.PI * 0.495) (min (Math.PI * 0.495) elevationRadians)
        let ce = cos clampedElev
        let eye =
            Point3.create
                (target.X + distance * ce * cos azimuthRadians)
                (target.Y + distance * sin clampedElev)
                (target.Z + distance * ce * sin azimuthRadians)
        { Eye = eye
          Target = target
          Up = Point3.create 0. 1. 0.
          Lens = ProjectionLens3D.Perspective(Math.PI / 4.)
          Near = max 0.1 (distance * 0.01) }

    /// <summary>Fits an orbit camera automatically around a <c>Bounds3</c> volume.</summary>
    let fitBounds (azimuthRadians: float) (elevationRadians: float) (bounds: Bounds3) : Camera3D =
        let c = Bounds3.center bounds
        let sx, sy, sz = Bounds3.sizeX bounds, Bounds3.sizeY bounds, Bounds3.sizeZ bounds
        let radius = max 50. (0.5 * sqrt (sx * sx + sy * sy + sz * sz))
        orbit c azimuthRadians elevationRadians (radius * 2.75)

    let internal basis (cam: Camera3D) =
        let fxRaw, fyRaw, fzRaw = cam.Target.X - cam.Eye.X, cam.Target.Y - cam.Eye.Y, cam.Target.Z - cam.Eye.Z
        let fLen = sqrt (fxRaw * fxRaw + fyRaw * fyRaw + fzRaw * fzRaw)
        let fx, fy, fz = if fLen <= 1e-12 then 0., 0., -1. else fxRaw / fLen, fyRaw / fLen, fzRaw / fLen
        // right = normalize(forward x up)
        let rx0 = fy * cam.Up.Z - fz * cam.Up.Y
        let ry0 = fz * cam.Up.X - fx * cam.Up.Z
        let rz0 = fx * cam.Up.Y - fy * cam.Up.X
        let rLen = sqrt (rx0 * rx0 + ry0 * ry0 + rz0 * rz0)
        let rx, ry, rz = if rLen <= 1e-12 then 1., 0., 0. else rx0 / rLen, ry0 / rLen, rz0 / rLen
        // trueUp = right x forward
        let ux = ry * fz - rz * fy
        let uy = rz * fx - rx * fz
        let uz = rx * fy - ry * fx
        (rx, ry, rz), (ux, uy, uz), (fx, fy, fz)

    /// <summary>Projects a 3D world point to <c>(screenPoint, cameraDepth, perspectiveScale)</c>, or <c>None</c> if behind the near plane.</summary>
    let projectPoint (viewport: Viewport) (cam: Camera3D) (world: Point3) : (Point * float * float) option =
        let (rx, ry, rz), (ux, uy, uz), (fx, fy, fz) = basis cam
        let dx, dy, dz = world.X - cam.Eye.X, world.Y - cam.Eye.Y, world.Z - cam.Eye.Z
        let depth = dx * fx + dy * fy + dz * fz
        if depth <= cam.Near then None
        else
            let vx = dx * rx + dy * ry + dz * rz
            let vy = dx * ux + dy * uy + dz * uz
            let halfH = viewport.Height * 0.5
            let halfW = viewport.Width * 0.5
            match cam.Lens with
            | ProjectionLens3D.Perspective fovY ->
                let focal = halfH / max 1e-4 (tan (fovY * 0.5))
                let pScale = focal / depth
                let sx = halfW + vx * pScale
                let sy = halfH - vy * pScale
                if Double.IsFinite sx && Double.IsFinite sy then Some(Point.create sx sy, depth, pScale) else None
            | ProjectionLens3D.Orthographic scale ->
                let sx = halfW + vx * scale
                let sy = halfH - vy * scale
                if Double.IsFinite sx && Double.IsFinite sy then Some(Point.create sx sy, depth, scale) else None

/// <summary>Styling options for pure CPU fallback projection of <c>DiagramLayout3D</c> into a 2D SVG <c>Ranvier.D5.Scene</c>.</summary>
type Scene3DTheme =
    { /// <summary>2D SVG viewport dimensions.</summary>
      Viewport: Viewport
      /// <summary>Accessible scene title.</summary>
      Title: string
      /// <summary>Optional bounding sphere guide radius (e.g. <c>Some 300.</c> to draw H3/Walrus unit-ball meridians).</summary>
      ReferenceSphereRadius: float option
      /// <summary>Fill paint for a node ID.</summary>
      NodeFill: string -> Paint
      /// <summary>Stroke paint for backbone vs. cross-link associations (<c>assocId -&gt; isBackbone -&gt; Paint</c>).</summary>
      AssociationStroke: string -> bool -> Paint
      /// <summary>Optional text label callback (<c>None</c> omits label).</summary>
      NodeLabel: string -> string option }

/// <summary>Constructors for <c>Scene3DTheme</c>.</summary>
[<RequireQualifiedAccess>]
module Scene3DTheme =
    /// <summary>Creates a clean depth-cued 3D projection theme.</summary>
    let create width height title =
        { Viewport = { Width = width; Height = height }
          Title = title
          ReferenceSphereRadius = None
          NodeFill = fun _ -> Paint.rgb 59 130 246
          AssociationStroke = fun _ isBackbone -> if isBackbone then Paint.rgb 71 85 105 else Paint.rgb 148 163 184
          NodeLabel = fun id -> Some id }

/// <summary>Pure CPU painter's-algorithm projection of any <c>DiagramLayout3D</c> into a retained <c>Ranvier.D5.Scene</c> (SVG fallback when not rendering on <c>Ranvier.D5.Wgpu</c>).</summary>
[<RequireQualifiedAccess>]
module Scene3D =
    let private circleGuide (vp: Viewport) (cam: Camera3D) (key: string) (radius: float) (plane: int) : SceneNode option =
        let steps = 48
        let pts =
            [ for k in 0 .. steps ->
                let a = float k * 2. * Math.PI / float steps
                let wPt =
                    match plane with
                    | 0 -> Point3.create (radius * cos a) (radius * sin a) 0.
                    | 1 -> Point3.create (radius * cos a) 0. (radius * sin a)
                    | _ -> Point3.create 0. (radius * cos a) (radius * sin a)
                Camera3D.projectPoint vp cam wPt |> Option.map (fun (p, _, _) -> p) ]
        let path = Shape.line Curve.Linear pts
        if List.isEmpty (Path.commands path) then None
        else
            Some(
                Leaf(
                    Mark.path key path
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke (Paint.rgb 203 213 225) 1.
                    |> Mark.withOpacity 0.45
                )
            )

    /// <summary>Projects a 3D Euclidean or H3/Walrus layout through <c>camera</c> into depth-sorted SVG marks.</summary>
    let render (theme: Scene3DTheme) (camera: Camera3D) (layout: DiagramLayout3D) : Scene =
        let vp = theme.Viewport
        let sphereGuides =
            match theme.ReferenceSphereRadius with
            | Some r when r > 0. ->
                [ 0; 1; 2 ] |> List.choose (fun pl -> circleGuide vp camera (sprintf "guide:sphere:%d" pl) r pl)
            | _ -> []

        // 12 wireframe edges of each 3D compound group cuboid
        let groupWireframes =
            layout.Groups
            |> List.collect (fun g ->
                let b = g.Bounds
                let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
                let x1, y1, z1 = Bounds3.maxX b, Bounds3.maxY b, Bounds3.maxZ b
                let corners =
                    [| Point3.create x0 y0 z0; Point3.create x1 y0 z0; Point3.create x1 y1 z0; Point3.create x0 y1 z0
                       Point3.create x0 y0 z1; Point3.create x1 y0 z1; Point3.create x1 y1 z1; Point3.create x0 y1 z1 |]
                let edges = [| 0,1; 1,2; 2,3; 3,0; 4,5; 5,6; 6,7; 7,4; 0,4; 1,5; 2,6; 3,7 |]
                let cmds = ResizeArray<PathCommand>()
                for a, c in edges do
                    match Camera3D.projectPoint vp camera corners[a], Camera3D.projectPoint vp camera corners[c] with
                    | Some(pa, _, _), Some(pc, _, _) ->
                        cmds.Add(MoveTo pa)
                        cmds.Add(LineTo pc)
                    | _ -> ()
                if cmds.Count = 0 then []
                else
                    [ Leaf(
                        Mark.path ("group3d:" + g.Id) (Path.ofCommands (List.ofSeq cmds))
                        |> Mark.withFill Paint.none
                        |> Mark.withStroke (Paint.rgb 148 163 184) 1.
                        |> Mark.withOpacity 0.42
                      ) ])

        // Project associations and compute mean camera depth
        let projectedEdges =
            layout.Associations
            |> List.choose (fun a ->
                let pts2D = ResizeArray<Point>(a.Points.Length)
                let mutable depthSum = 0.
                for k = 0 to a.Points.Length - 1 do
                    match Camera3D.projectPoint vp camera a.Points[k] with
                    | Some(p2, d, _) ->
                        pts2D.Add p2
                        depthSum <- depthSum + d
                    | None -> ()
                if pts2D.Count < 2 then None
                else
                    let meanD = depthSum / float pts2D.Count
                    let cmds = ResizeArray<PathCommand>(pts2D.Count)
                    cmds.Add(MoveTo pts2D[0])
                    for k = 1 to pts2D.Count - 1 do cmds.Add(LineTo pts2D[k])
                    let op = if a.IsBackbone then 0.72 else 0.28
                    let sw = if a.IsBackbone then 1.35 else 0.85
                    let mark =
                        Mark.path ("assoc3d:" + a.Id) (Path.ofCommands (List.ofSeq cmds))
                        |> Mark.withFill Paint.none
                        |> Mark.withStroke (theme.AssociationStroke a.Id a.IsBackbone) sw
                        |> Mark.withOpacity op
                    Some(meanD, Leaf mark))
            |> List.sortByDescending fst
            |> List.map snd

        // Project nodes sorted back-to-front (painter's order) with depth + hyperbolic scale attenuation!
        let rawNodes =
            layout.Nodes
            |> List.choose (fun n ->
                Camera3D.projectPoint vp camera n.Center
                |> Option.map (fun (pt2, depth, pScale) -> n, pt2, depth, pScale))
        let minD, maxD =
            match rawNodes with
            | [] -> 1., 2.
            | _ ->
                let ds = rawNodes |> List.map (fun (_, _, d, _) -> d)
                List.min ds, max (List.min ds + 1e-3) (List.max ds)

        let labelStyle = { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }
        let sortedNodes =
            rawNodes
            |> List.sortByDescending (fun (_, _, d, _) -> d)
            |> List.collect (fun (n, pt2, depth, pScale) ->
                let depthT = (depth - minD) / (maxD - minD)
                let fogOpacity = max 0.22 (min 1. ((1. - 0.58 * depthT) * (0.35 + 0.65 * min 1. n.ScaleFactor)))
                let screenRad = max 1.8 (min 28. (n.Radius * pScale))
                let circle =
                    Mark.path ("node3d:" + n.Id + ":disc") (Shape.arc 0. screenRad 0. (2. * Math.PI))
                    |> Mark.withFill (theme.NodeFill n.Id)
                    |> Mark.withStroke (Paint.rgb 255 255 255) 1.1
                    |> Mark.withOpacity fogOpacity
                    |> Mark.withTransform (Transform.translate pt2.X pt2.Y)
                match theme.NodeLabel n.Id with
                | Some txt when not (String.IsNullOrEmpty txt) && screenRad >= 4.2 && n.ScaleFactor >= 0.2 ->
                    let lbl =
                        Mark.text ("node3d:" + n.Id + ":label") (Point.create pt2.X (pt2.Y - screenRad - 4.)) txt
                        |> Mark.withFill (Paint.rgb 15 23 42)
                        |> Mark.withFontSize (max 8.5 (min 12. (9. + screenRad * 0.2)))
                        |> Mark.withOpacity fogOpacity
                        |> Mark.withTextStyle labelStyle
                    [ Leaf circle; Leaf lbl ]
                | _ -> [ Leaf circle ])

        Scene.compose
            vp
            theme.Title
            []
            [ Group(GroupSpec.create "guides", sphereGuides)
              Group(GroupSpec.create "groups3d", groupWireframes)
              Group(GroupSpec.create "associations3d", projectedEdges)
              Group(GroupSpec.create "nodes3d", sortedNodes) ]

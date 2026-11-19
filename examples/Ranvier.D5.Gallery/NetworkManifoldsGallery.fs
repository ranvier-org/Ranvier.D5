module Ranvier.D5.Gallery.NetworkManifoldsGallery

open System
open Ranvier.D5
open Ranvier.D5.Graph

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

let private roundedRect (b: Bounds) (radius: float) : Path =
    let x, y, w, h = Bounds.x b, Bounds.y b, Bounds.width b, Bounds.height b
    let r = max 0. (min radius (min (w * 0.5) (h * 0.5)))
    if r <= 0.1 then
        Shape.rectangle b
    else
        Path.ofCommands
            [ MoveTo(Point.create (x + r) y)
              LineTo(Point.create (x + w - r) y)
              QuadraticTo(Point.create (x + w) y, Point.create (x + w) (y + r))
              LineTo(Point.create (x + w) (y + h - r))
              QuadraticTo(Point.create (x + w) (y + h), Point.create (x + w - r) (y + h))
              LineTo(Point.create (x + r) (y + h))
              QuadraticTo(Point.create x (y + h), Point.create x (y + h - r))
              LineTo(Point.create x (y + r))
              QuadraticTo(Point.create x y, Point.create (x + r) y)
              Close ]

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
    |> Mark.withFontSize 10.2
    |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }

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

let private pinnedLayout (bounds: Bounds) (graph: DiagramGraph) : DiagramLayout2D =
    CircularLayout.layout { CircularConfig.create bounds with OrderByCommunities = false; SiftingSweeps = 0 } graph |> ok

// ===========================================================================
// PLATE 4: bundling-routing
// Leiden + Holten HEB + Gansner MINGLE + Selassie Divided FDEB + Hurter KDE2D
// + LOOM OctilinearRouter + CircularLayout / ArcLayout / HivePlot / BioFabric
// ===========================================================================

let private leidenHebData =
    let nodes =
        [ for c in 0 .. 3 do
            for k in 0 .. 3 do
                yield DiagramNode.sized (sprintf "H%d_%d" c k) 8. 8. ]
    let intra =
        [ for c in 0 .. 3 do
            for a in 0 .. 2 do
                for b in a + 1 .. 3 do
                    yield
                        { Association.create (sprintf "in_%d_%d_%d" c a b) (sprintf "H%d_%d" c a) (sprintf "H%d_%d" c b) with
                            Weight = 3.5 } ]
    let interPairs =
        [ 0,0, 1,1; 0,1, 1,2; 0,2, 2,0; 0,3, 2,2; 0,1, 3,1; 0,3, 3,3
          1,0, 2,1; 1,2, 2,3; 1,3, 3,0; 1,1, 3,2
          2,0, 3,1; 2,2, 3,3; 2,1, 0,0; 3,0, 0,2 ]
    let inter =
        interPairs
        |> List.mapi (fun idx (c1, k1, c2, k2) ->
            Association.create (sprintf "xr_%d" idx) (sprintf "H%d_%d" c1 k1) (sprintf "H%d_%d" c2 k2))
    let graph = DiagramGraph.simple nodes (intra @ inter) |> ok
    let partition = Leiden.detect { LeidenConfig.defaultValue with Resolution = 1.05 } graph |> ok
    let groupedPreview = Leiden.applyGroups graph partition |> ok
    let hierarchy = Leiden.toHierarchy "heb-root" graph partition |> ok
    let orderedIds =
        nodes
        |> List.map (fun nd -> nd.Id, Map.tryFind nd.Id partition.Communities |> Option.defaultValue 0)
        |> List.sortBy (fun (id, comm) -> comm, id)
    let cx, cy, rad = 94., 94., 66.
    let total = orderedIds.Length
    let pinnedNodes =
        orderedIds
        |> List.mapi (fun idx (nid, comm) ->
            let sectorGap = float comm * 0.14
            let ang = -Math.PI * 0.5 + (2. * Math.PI * float idx / float total) + sectorGap * 0.12
            let pt = Point.create (cx + rad * cos ang) (cy + rad * sin ang)
            DiagramNode.sized nid 7. 7. |> DiagramNode.withPin pt)
    let crossGraph = DiagramGraph.simple pinnedNodes inter |> ok
    let baseRing = pinnedLayout (Bounds.create 14. 14. 160. 160.) crossGraph
    crossGraph, partition, hierarchy, baseRing, DiagramGraph.groups groupedPreview |> List.length

let private mingleData =
    let west =
        [ "W0", Point.create 208. 26.
          "W1", Point.create 216. 54.
          "W2", Point.create 206. 86.
          "W3", Point.create 214. 118.
          "W4", Point.create 208. 150. ]
    let northEast =
        [ "NE0", Point.create 340. 22.
          "NE1", Point.create 358. 40.
          "NE2", Point.create 348. 62.
          "NE3", Point.create 362. 80. ]
    let southEast =
        [ "SE0", Point.create 338. 116.
          "SE1", Point.create 360. 132.
          "SE2", Point.create 344. 152.
          "SE3", Point.create 360. 168. ]
    let allStations = west @ northEast @ southEast
    let nodes =
        allStations
        |> List.map (fun (id, pt) -> DiagramNode.sized id 6. 6. |> DiagramNode.withPin pt)
    let pairs =
        [ "W0","NE0"; "W0","NE1"; "W1","NE0"; "W1","NE2"; "W2","NE1"; "W2","NE3"
          "W1","SE0"; "W2","SE0"; "W2","SE1"; "W3","SE1"; "W3","SE2"; "W4","SE2"; "W4","SE3"
          "W0","SE1"; "W4","NE3"; "NE2","SE0"; "NE3","SE1" ]
    let assocs =
        pairs |> List.mapi (fun i (u, v) -> Association.create (sprintf "mg_%d" i) u v)
    let graph = DiagramGraph.simple nodes assocs |> ok
    let baseLayout = pinnedLayout (Bounds.create 198. 14. 172. 166.) graph
    let bundled =
        MingleBundle.bundle
            { MingleConfig.defaultValue with Curvature = 0.88; Neighbors = 8 }
            graph
            baseLayout
        |> ok
    bundled

let private renderHebMingleCard (phase: float) : SceneNode list * Point =
    let _, partition, hierarchy, baseRing, groupCount = leidenHebData
    let beta = Math.Clamp(0.84 + 0.11 * sin (phase * 1.4), 0.70, 0.96)
    let hebLayout = HierarchicalBundle.bundle beta hierarchy baseRing |> ok

    let commColors =
        [| Paint.rgb 37 99 235
           Paint.rgb 16 185 129
           Paint.rgb 245 158 11
           Paint.rgb 225 29 72 |]

    let ringGuide =
        Shape.arc 65.5 66.5 0. (2. * Math.PI)
        |> Mark.path "heb:ring-guide"
        |> Mark.withFill (Paint.rgb 226 232 240)
        |> Mark.withTransform (Transform.translate 94. 94.)

    let hebChords =
        hebLayout.Associations
        |> List.map (fun ra ->
            let cid = Map.tryFind ra.Source.NodeId partition.Communities |> Option.defaultValue 0
            let stroke = commColors[cid % commColors.Length]
            Leaf(
                Mark.path ("heb:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke stroke 1.45
                |> Mark.withOpacity 0.68
            ))

    let hebNodes =
        hebLayout.Nodes
        |> List.map (fun pn ->
            let cid = Map.tryFind pn.Id partition.Communities |> Option.defaultValue 0
            let fill = commColors[cid % commColors.Length]
            Leaf(discMark ("heb:node:" + pn.Id) pn.Center 5.0 fill (Paint.rgb 255 255 255) 1.2))

    let divider =
        Path.ofCommands [ MoveTo(Point.create 188. 10.); LineTo(Point.create 188. 184.) ]
        |> Mark.path "heb-mingle:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.

    let mingleEdges =
        mingleData.Associations
        |> List.mapi (fun idx ra ->
            let stroke =
                if idx % 3 = 0 then Paint.rgb 79 70 229
                elif idx % 3 = 1 then Paint.rgb 14 116 144
                else Paint.rgb 109 40 217
            Leaf(
                Mark.path ("mingle:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke stroke 1.55
                |> Mark.withOpacity 0.74
            ))

    let mingleNodes =
        mingleData.Nodes
        |> List.map (fun pn ->
            let fill =
                if pn.Id.StartsWith "W" then Paint.rgb 30 64 175
                elif pn.Id.StartsWith "NE" then Paint.rgb 13 148 136
                else Paint.rgb 124 58 237
            Leaf(discMark ("mingle:node:" + pn.Id) pn.Center 4.5 fill (Paint.rgb 255 255 255) 1.1))

    let lblLeft =
        subLabel "heb:lbl" 94. 196. (sprintf "Leiden (%d grp) + HEB (beta=%.2f)" groupCount beta)
    let lblRight =
        subLabel "mingle:lbl" 284. 196. "Gansner MINGLE Ink-Saving Trunks"
    let subNote =
        Mark.text "heb-mingle:note" (Point.create 190. 214.) "Modularity communities -> LCA B-spline chords | Multilevel agglomerative bundling"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.3
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let probeNode = hebLayout.Nodes.Head.Center
    [ Leaf ringGuide ] @ hebChords @ hebNodes @ [ Leaf divider ] @ mingleEdges @ mingleNodes @ [ Leaf lblLeft; Leaf lblRight; Leaf subNote ],
    probeNode

let private dividedAndKdeData =
    // Left: Selassie DividedForceBundle with opposing Eastbound (Blue) & Westbound (Rose) corridors
    let leftBank =
        [ for i in 0 .. 4 -> sprintf "DL%d" i, Point.create (22. + float (i % 2) * 8.) (26. + float i * 33.) ]
    let rightBank =
        [ for i in 0 .. 4 -> sprintf "DR%d" i, Point.create (164. - float (i % 2) * 8.) (26. + float i * 33.) ]
    let dNodes =
        (leftBank @ rightBank)
        |> List.map (fun (id, pt) -> DiagramNode.sized id 6. 6. |> DiagramNode.withPin pt)
    let eastbound =
        [ 0,0; 0,2; 1,1; 1,3; 2,2; 2,4; 3,1; 3,4; 4,3 ]
        |> List.mapi (fun k (i, j) -> Association.create (sprintf "east_%d" k) (sprintf "DL%d" i) (sprintf "DR%d" j))
    let westbound =
        [ 0,1; 1,0; 1,2; 2,1; 2,3; 3,2; 3,4; 4,0; 4,2 ]
        |> List.mapi (fun k (j, i) -> Association.create (sprintf "west_%d" k) (sprintf "DR%d" j) (sprintf "DL%d" i))
    let dGraph = DiagramGraph.simple dNodes (eastbound @ westbound) |> ok
    let dBase = pinnedLayout (Bounds.create 14. 16. 166. 162.) dGraph

    // Right: Hurter KDE 2D density-advected cross-country highway graph
    let hubs =
        [ 208., 38.; 208., 146.; 358., 40.; 358., 144.; 282., 88. ]
    let kStations =
        [ for h in 0 .. hubs.Length - 1 do
            let hx, hy = hubs[h]
            let count = if h = 4 then 2 else 4
            for s in 0 .. count - 1 do
                let ang = float (h * 3 + s) * 1.65
                let pt = Point.create (hx + 12. * cos ang) (hy + 14. * sin ang)
                yield sprintf "K%d_%d" h s, pt ]
    let kNodes =
        kStations
        |> List.map (fun (id, pt) -> DiagramNode.sized id 5. 5. |> DiagramNode.withPin pt)
    let kPairs =
        [ "K0_0","K2_0"; "K0_1","K2_1"; "K0_2","K2_2"; "K0_3","K2_3"
          "K0_0","K3_0"; "K0_1","K3_1"; "K0_2","K3_2"; "K0_3","K3_3"
          "K1_0","K3_0"; "K1_1","K3_1"; "K1_2","K3_2"; "K1_3","K3_3"
          "K1_0","K2_0"; "K1_1","K2_1"; "K1_2","K2_2"; "K1_3","K2_3"
          "K0_1","K4_0"; "K1_2","K4_1"; "K4_0","K2_2"; "K4_1","K3_1" ]
    let kAssocs =
        kPairs |> List.mapi (fun i (u, v) -> Association.create (sprintf "kde_%d" i) u v)
    let kGraph = DiagramGraph.simple kNodes kAssocs |> ok
    let kBase = pinnedLayout (Bounds.create 196. 16. 172. 162.) kGraph
    let kdeBundled =
        KdeBundle2D.bundle
            { KdeBundleConfig.defaultValue with
                GridSize = 96
                Iterations = 18
                InitialKernelRadius = 46.
                KernelDecay = 0.93
                StepSize = 5.2
                SampleStep = 12. }
            kGraph
            kBase
        |> ok
    (dGraph, dBase), kdeBundled

let private renderDividedKdeCard (phase: float) : SceneNode list * Point =
    let (dGraph, dBase), kdeBundled = dividedAndKdeData
    let laneWidth = 11. + 3. * sin (phase * 1.2)
    let divBundled =
        DividedForceBundle.bundle
            { DividedBundleConfig.defaultValue with
                Cycles = 4
                IterationsInitial = 36
                LaneWidth = laneWidth
                DirectionalSeparation = true }
            dGraph
            dBase
        |> ok

    let divEdges =
        divBundled.Associations
        |> List.map (fun ra ->
            let isEast = ra.Id.StartsWith "east_"
            let col = if isEast then Paint.rgb 37 99 235 else Paint.rgb 225 29 72
            Leaf(
                Mark.path ("fdeb:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke col 1.45
                |> Mark.withOpacity 0.72
            ))

    let divNodes =
        divBundled.Nodes
        |> List.map (fun pn ->
            let fill = if pn.Id.StartsWith "DL" then Paint.rgb 29 78 216 else Paint.rgb 190 18 60
            Leaf(discMark ("fdeb:node:" + pn.Id) pn.Center 4.8 fill (Paint.rgb 255 255 255) 1.15))

    let divider =
        Path.ofCommands [ MoveTo(Point.create 188. 10.); LineTo(Point.create 188. 184.) ]
        |> Mark.path "fdeb-kde:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.

    let kdeEdges =
        kdeBundled.Associations
        |> List.mapi (fun idx ra ->
            let col =
                if idx % 3 = 0 then Paint.rgb 5 150 105
                elif idx % 3 = 1 then Paint.rgb 14 116 144
                else Paint.rgb 217 119 6
            Leaf(
                Mark.path ("kde2d:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke col 1.5
                |> Mark.withOpacity 0.74
            ))

    let kdeNodes =
        kdeBundled.Nodes
        |> List.map (fun pn ->
            Leaf(discMark ("kde2d:node:" + pn.Id) pn.Center 3.8 (Paint.rgb 15 118 110) (Paint.rgb 255 255 255) 1.0))

    let lblLeft = subLabel "fdeb:lbl" 94. 196. "DividedForceBundle (L->R / R->L)"
    let lblRight = subLabel "kde2d:lbl" 284. 196. "KdeBundle2D Gradient Highways"
    let subNote =
        Mark.text "fdeb-kde:note" (Point.create 190. 214.) "Blue = Eastbound lane • Rose = Westbound lane | Separable KDE advection"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.3
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let probePt = (divBundled.Nodes |> List.find (fun n -> n.Id = "DL2")).Center
    divEdges @ divNodes @ [ Leaf divider ] @ kdeEdges @ kdeNodes @ [ Leaf lblLeft; Leaf lblRight; Leaf subNote ],
    probePt

let private renderMetroRouterCard (phase: float) : SceneNode list * Point =
    let stations =
        [ "RedW", Point.create 32. 34., 12., 12.
          "GrnW", Point.create 32. 98., 12., 12.
          "BluW", Point.create 32. 162., 12., 12.
          "HubW", Point.create 136. 98., 18., 22.
          "HubE", Point.create 244. 98., 18., 22.
          "RedE", Point.create 346. 34., 12., 12.
          "GrnE", Point.create 346. 98., 12., 12.
          "BluE", Point.create 346. 162., 12., 12. ]
    let obstacles =
        [ "NorthPark", Point.create 84. 38., 34., 22.
          "SouthBasin", Point.create 294. 158., 36., 22. ]
    let nodes =
        [ for id, pt, w, h in stations ->
            DiagramNode.sized id w h |> DiagramNode.withPin pt
          for id, pt, w, h in obstacles ->
            DiagramNode.sized id w h |> DiagramNode.withPin pt ]
    let assocs =
        [ Association.create "red_1" "RedW" "HubW"
          Association.create "red_2" "HubW" "HubE"
          Association.create "red_3" "HubE" "RedE"
          Association.create "grn_1" "GrnW" "HubW"
          Association.create "grn_2" "HubW" "HubE"
          Association.create "grn_3" "HubE" "GrnE"
          Association.create "blu_1" "BluW" "HubW"
          Association.create "blu_2" "HubW" "HubE"
          Association.create "blu_3" "HubE" "BluE" ]
    let graph = DiagramGraph.simple nodes assocs |> ok
    let baseLayout = pinnedLayout (Bounds.create 12. 12. 356. 176.) graph
    let cfg =
        { OctilinearRouterConfig.defaultValue with
            ObstacleMargin = 8.
            PortStubLength = 12.
            GridSpacing = 18.
            TrackSeparation = 6.5 + 1.2 * sin phase
            CornerFilletRadius = 4.5 }
    let routed = OctilinearRouter.route cfg graph baseLayout |> ok

    let gridBg =
        Shape.rectangle (Bounds.create 10. 10. 360. 178.)
        |> Mark.path "metro:bg"
        |> Mark.withFill (Paint.rgb 241 245 249)
        |> Mark.withStroke (Paint.rgb 203 213 225) 1.

    let obstacleIds = obstacles |> List.map (fun (id, _, _, _) -> id) |> Set.ofList

    let obstacleMarks =
        routed.Nodes
        |> List.filter (fun pn -> Set.contains pn.Id obstacleIds)
        |> List.collect (fun pn ->
            let rect =
                Shape.rectangle pn.Bounds
                |> Mark.path ("metro:obs:" + pn.Id)
                |> Mark.withFill (Paint.rgb 226 232 240)
                |> Mark.withStroke (Paint.rgb 148 163 184) 1.1
            let lbl =
                Mark.text ("metro:obs-lbl:" + pn.Id) (Point.create pn.Center.X (pn.Center.Y + 3.)) (if pn.Id = "NorthPark" then "Park" else "Basin")
                |> Mark.withFill (Paint.rgb 71 85 105)
                |> Mark.withFontSize 8.8
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }
            [ Leaf rect; Leaf lbl ])

    let lineColor (aid: string) =
        if aid.StartsWith "red_" then Paint.rgb 225 29 72
        elif aid.StartsWith "grn_" then Paint.rgb 16 185 129
        else Paint.rgb 37 99 235

    let trackCasings =
        routed.Associations
        |> List.map (fun ra ->
            Leaf(
                Mark.path ("metro:casing:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 255 255 255) 4.8
            ))

    let trackLines =
        routed.Associations
        |> List.map (fun ra ->
            Leaf(
                Mark.path ("metro:line:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke (lineColor ra.Id) 2.9
            ))

    let stationMarks =
        routed.Nodes
        |> List.filter (fun pn -> not (Set.contains pn.Id obstacleIds))
        |> List.collect (fun pn ->
            let isHub = pn.Id.StartsWith "Hub"
            if isHub then
                let pill =
                    roundedRect pn.Bounds 6.
                    |> Mark.path ("metro:hub:" + pn.Id)
                    |> Mark.withFill (Paint.rgb 30 41 59)
                    |> Mark.withStroke (Paint.rgb 255 255 255) 2.0
                let core =
                    discMark ("metro:hub-dot:" + pn.Id) pn.Center 4.2 (Paint.rgb 250 204 21) (Paint.rgb 30 41 59) 1.0
                let lbl =
                    Mark.text ("metro:lbl:" + pn.Id) (Point.create pn.Center.X (Bounds.y pn.Bounds - 5.)) pn.Id
                    |> Mark.withFill (Paint.rgb 15 23 42)
                    |> Mark.withFontSize 9.2
                    |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "middle"; Baseline = "alphabetic" }
                [ Leaf pill; Leaf core; Leaf lbl ]
            else
                let ringColor =
                    if pn.Id.StartsWith "Red" then Paint.rgb 225 29 72
                    elif pn.Id.StartsWith "Grn" then Paint.rgb 16 185 129
                    else Paint.rgb 37 99 235
                let dot = discMark ("metro:st:" + pn.Id) pn.Center 5.6 ringColor (Paint.rgb 255 255 255) 1.8
                [ Leaf dot ])

    let footer =
        Mark.text "metro:footer" (Point.create 190. 208.) "State-space 8-compass A* • 45° bends • Obstacle detours • Parallel trunk separation"
        |> Mark.withFill (Paint.rgb 71 85 105)
        |> Mark.withFontSize 9.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }

    let hubWCenter = (routed.Nodes |> List.find (fun n -> n.Id = "HubW")).Center
    [ Leaf gridBg ] @ obstacleMarks @ trackCasings @ trackLines @ stationMarks @ [ Leaf footer ],
    hubWCenter

let private analyticalQuartetData =
    // 1. CircularLayout
    let cNodes = [ for i in 0 .. 9 -> DiagramNode.sized (sprintf "C%d" i) 7. 7. ]
    let cPairs = [ 0,1; 1,2; 2,3; 3,0; 4,5; 5,6; 6,4; 7,8; 8,9; 9,7; 0,5; 2,7; 4,9; 1,8 ]
    let cAssocs = cPairs |> List.mapi (fun i (u, v) -> Association.create (sprintf "ce_%d" i) (sprintf "C%d" u) (sprintf "C%d" v))
    let cGraph = DiagramGraph.simple cNodes cAssocs |> ok
    let circLayout =
        CircularLayout.layout
            { CircularConfig.create (Bounds.create 38. 10. 108. 82.) with OrderByCommunities = true; SiftingSweeps = 4 }
            cGraph
        |> ok

    // 2. ArcLayout (with forward + backward hemisphere arcs)
    let aNodes = [ for i in 0 .. 6 -> DiagramNode.sized (sprintf "A%d" i) 6. 6. ]
    let aPairs = [ 0,2; 0,4; 1,3; 1,5; 2,6; 3,5; 5,1; 6,2; 4,0 ]
    let aAssocs = aPairs |> List.mapi (fun i (u, v) -> Association.create (sprintf "ae_%d" i) (sprintf "A%d" u) (sprintf "A%d" v))
    let aGraph = DiagramGraph.simple aNodes aAssocs |> ok
    let arcLayout =
        ArcLayout.layout
            { ArcConfig.create (Bounds.create 200. 10. 166. 82.) with SiftingSweeps = 4; BidirectionalSplitHemispheres = true }
            aGraph
        |> ok

    // 3. HivePlot (3 radial axes: Sources, Intermediaries, Sinks)
    let hNodes =
        [ for i in 0 .. 2 -> DiagramNode.sized (sprintf "S%d" i) 5. 5.
          for i in 0 .. 3 -> DiagramNode.sized (sprintf "M%d" i) 5. 5.
          for i in 0 .. 2 -> DiagramNode.sized (sprintf "T%d" i) 5. 5. ]
    let hAssocs =
        [ Association.create "hv0" "S0" "M0"
          Association.create "hv1" "S0" "M2"
          Association.create "hv2" "S1" "M1"
          Association.create "hv3" "S1" "M3"
          Association.create "hv4" "S2" "M2"
          Association.create "hv5" "S2" "M3"
          Association.create "hv6" "M0" "T0"
          Association.create "hv7" "M1" "T0"
          Association.create "hv8" "M1" "T1"
          Association.create "hv9" "M2" "T1"
          Association.create "hv10" "M3" "T2"
          Association.create "hv11" "S0" "T2" ]
    let hGraph = DiagramGraph.simple hNodes hAssocs |> ok
    // Shift HivePlot center to (94., 174.) so top radial axis (rOuter=60 -> y=114) and lower axes (y=204) fit cleanly!
    let hiveLayout =
        HivePlot.layout
            { HiveConfig.create (Bounds.create 34. 126. 120. 96.) with
                AxisRule = HiveAxisRule.SourceIntermediarySink
                InnerRadius = 13.
                SplitAxes = false }
            hGraph
        |> ok

    // 4. BioFabric (6 horizontal node strands + 9 vertical shadow-sorted edge bars)
    let bNodes = [ for i in 0 .. 5 -> DiagramNode.sized (sprintf "B%d" i) 7. 4. ]
    let bPairs = [ 0,1; 0,2; 0,3; 0,5; 1,2; 1,4; 2,3; 2,5; 3,4 ]
    let bAssocs = bPairs |> List.mapi (fun i (u, v) -> Association.create (sprintf "bf_%d" i) (sprintf "B%d" u) (sprintf "B%d" v))
    let bGraph = DiagramGraph.simple bNodes bAssocs |> ok
    let bioLayout = BioFabric.layout (Bounds.create 198. 116. 168. 82.) bGraph |> ok

    circLayout, arcLayout, hiveLayout, bioLayout

let private renderAnalyticalQuartetCard (phase: float) : SceneNode list * Point =
    let circLayout, arcLayout, hiveLayout, bioLayout = analyticalQuartetData
    let pulse = 0.72 + 0.16 * abs (sin phase)

    let crossDivs =
        Path.ofCommands
            [ MoveTo(Point.create 188. 6.); LineTo(Point.create 188. 218.)
              MoveTo(Point.create 8. 109.); LineTo(Point.create 372. 109.) ]
        |> Mark.path "quartet:divs"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.

    // 1. Circular
    let circMarks =
        [ for ra in circLayout.Associations ->
            Leaf(
                Mark.path ("q-circ:e:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 79 70 229) 1.15
                |> Mark.withOpacity pulse
            )
          for idx, pn in List.indexed circLayout.Nodes ->
            let col = spectrumPaint (float idx / float (max 1 (circLayout.Nodes.Length - 1)))
            Leaf(discMark ("q-circ:n:" + pn.Id) pn.Center 4.4 col (Paint.rgb 255 255 255) 1.0)
          yield Leaf(subLabel "q-circ:lbl" 94. 103. "CircularLayout") ]

    // 2. Arc
    let arcMarks =
        [ for idx, ra in List.indexed arcLayout.Associations ->
            let col = if idx < 6 then Paint.rgb 14 116 144 else Paint.rgb 225 29 72
            Leaf(
                Mark.path ("q-arc:e:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke col 1.35
                |> Mark.withOpacity 0.78
            )
          for pn in arcLayout.Nodes ->
            Leaf(discMark ("q-arc:n:" + pn.Id) pn.Center 4.0 (Paint.rgb 15 23 42) (Paint.rgb 255 255 255) 1.0)
          yield Leaf(subLabel "q-arc:lbl" 284. 103. "ArcLayout (Split Hemi)") ]

    // 3. HivePlot
    let hiveMarks =
        [ for ra in hiveLayout.Associations ->
            let col =
                if ra.Source.NodeId.StartsWith "S" && ra.Target.NodeId.StartsWith "M" then Paint.rgb 37 99 235
                elif ra.Source.NodeId.StartsWith "M" then Paint.rgb 16 185 129
                else Paint.rgb 245 158 11
            Leaf(
                Mark.path ("q-hive:e:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke col 1.25
                |> Mark.withOpacity 0.76
            )
          for pn in hiveLayout.Nodes ->
            let col =
                if pn.Id.StartsWith "S" then Paint.rgb 37 99 235
                elif pn.Id.StartsWith "M" then Paint.rgb 16 185 129
                else Paint.rgb 245 158 11
            Leaf(discMark ("q-hive:n:" + pn.Id) pn.Center 3.8 col (Paint.rgb 255 255 255) 0.9)
          yield Leaf(subLabel "q-hive:lbl" 94. 218. "Krzywinski HivePlot") ]

    // 4. BioFabric
    let bioStrands =
        bioLayout.Rows
        |> List.mapi (fun r row ->
            let col = spectrumPaint (float r / float (max 1 (bioLayout.Rows.Length - 1)))
            let x0 = max 204. (row.XStart - 10.)
            let x1 = min 368. (row.XEnd + 4.)
            Leaf(
                Mark.path ("q-bio:row:" + row.NodeId) (Path.ofCommands [ MoveTo(Point.create x0 row.Y); LineTo(Point.create x1 row.Y) ])
                |> Mark.withFill Paint.none
                |> Mark.withStroke col 2.1
            ))
    let bioCols =
        bioLayout.Columns
        |> List.collect (fun col ->
            let bar =
                Mark.path ("q-bio:col:" + col.AssociationId) (Path.ofCommands [ MoveTo(Point.create col.X col.YTop); LineTo(Point.create col.X col.YBottom) ])
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 51 65 85) 1.35
            let gTop =
                Shape.rectangle (Bounds.create (col.X - 2.3) (col.YTop - 2.3) 4.6 4.6)
                |> Mark.path ("q-bio:gt:" + col.AssociationId)
                |> Mark.withFill (Paint.rgb 30 64 175)
                |> Mark.withStroke (Paint.rgb 255 255 255) 0.7
            let gBot =
                Shape.rectangle (Bounds.create (col.X - 2.3) (col.YBottom - 2.3) 4.6 4.6)
                |> Mark.path ("q-bio:gb:" + col.AssociationId)
                |> Mark.withFill (Paint.rgb 225 29 72)
                |> Mark.withStroke (Paint.rgb 255 255 255) 0.7
            [ Leaf bar; Leaf gTop; Leaf gBot ])

    let probePt = circLayout.Nodes.Head.Center
    [ Leaf crossDivs ] @ circMarks @ arcMarks @ hiveMarks @ bioStrands @ bioCols @ [ Leaf(subLabel "q-bio:lbl" 284. 218. "Longabaugh BioFabric") ],
    probePt

let probesBundlingRouting (phase: float) : (string * Point) list =
    let _, p1 = renderHebMingleCard phase
    let _, p2 = renderDividedKdeCard phase
    let _, p3 = renderMetroRouterCard phase
    let _, p4 = renderAnalyticalQuartetCard phase
    [ "bundling:leiden-heb", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "bundling:divided-fdeb", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "bundling:metro-hub", Point.create (14. + p3.X) (304. + 24. + p3.Y)
      "bundling:circular", Point.create (406. + p4.X) (304. + 24. + p4.Y) ]

let sceneBundlingRouting (phase: float) : Scene =
    let n1, _ = renderHebMingleCard phase
    let n2, _ = renderDividedKdeCard phase
    let n3, _ = renderMetroRouterCard phase
    let n4, _ = renderAnalyticalQuartetCard phase

    let c1, clip1 = panelCard "c-heb-mingle" 14. 42. 380. 252. "Leiden Communities + Holten HEB & Gansner MINGLE" n1
    let c2, clip2 = panelCard "c-fdeb-kde" 406. 42. 380. 252. "Selassie Divided FDEB & Hurter KDE Bundling" n2
    let c3, clip3 = panelCard "c-metro" 14. 304. 380. 254. "Octilinear 45° Schematic Metro Router (LOOM)" n3
    let c4, clip4 = panelCard "c-quartet" 406. 304. 380. 254. "Analytical Quartet: Circular, Arc, HivePlot & BioFabric" n4

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "bundling:bg"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "bundling:title" (Point.create 16. 27.) "Ranvier.D5.Graph — Community Detection, Edge Bundling, 45° Metro Routing & Analytical Layouts"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 16.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose
        { Width = 800.; Height = 570. }
        "Network Bundling, Metro Routing and Analytical Layouts"
        [ clip1; clip2; clip3; clip4 ]
        [ Leaf pageBg; Leaf title; c1; c2; c3; c4 ]

// ===========================================================================
// PLATE 5: cluster-manifolds-2d
// PRISM Overlap Removal + GMap Voronoi Countries + Collins BubbleSets
// + BallTree / NnDescent + Umap2D + Barnes-Hut Tsne2D + Isomap2D + Sammon2D
// ===========================================================================

let private renderPrismGMapCard (phase: float) : SceneNode list * Point =
    // Deliberately overlapping initial cluster boxes around 3 country centers; PRISM separates them cleanly!
    let rawSpecs =
        [ "D5", "Core", Point.create 96. 58.
          "SIMD", "Core", Point.create 106. 64.
          "Scale", "Core", Point.create 92. 72.
          "Scene", "Core", Point.create 110. 76.
          "Path", "Core", Point.create 102. 52.
          "Cola", "Graph", Point.create 258. 56.
          "Sugiy", "Graph", Point.create 268. 64.
          "LOOM", "Graph", Point.create 252. 70.
          "Leiden", "Graph", Point.create 266. 76.
          "HEB", "Graph", Point.create 260. 50.
          "Globe", "Geo", Point.create 180. 136.
          "Voron", "Geo", Point.create 192. 142.
          "FibS2", "Geo", Point.create 174. 148.
          "Clip", "Geo", Point.create 190. 152. ]
    let nodes =
        rawSpecs
        |> List.map (fun (id, _, _) -> DiagramNode.sized id 34. 16.)
    let assocs =
        [ Association.create "gm0" "D5" "SIMD"
          Association.create "gm1" "D5" "Scale"
          Association.create "gm2" "Scale" "Scene"
          Association.create "gm3" "Scene" "Path"
          Association.create "gm4" "Cola" "Sugiy"
          Association.create "gm5" "Sugiy" "LOOM"
          Association.create "gm6" "Cola" "Leiden"
          Association.create "gm7" "Leiden" "HEB"
          Association.create "gm8" "Globe" "Voron"
          Association.create "gm9" "Voron" "FibS2"
          Association.create "gm10" "Globe" "Clip"
          Association.create "gm11" "Scene" "Cola"
          Association.create "gm12" "Path" "Globe"
          Association.create "gm13" "LOOM" "Voron" ]
    let graph = DiagramGraph.simple nodes assocs |> ok
    // Build initial unpinned overlapping layout by pinning a temporary graph, then clearing pins on `graph`!
    let pinnedInitNodes =
        rawSpecs
        |> List.map (fun (id, _, pt) -> DiagramNode.sized id 34. 16. |> DiagramNode.withPin pt)
    let initLayout = pinnedLayout (Bounds.create 24. 16. 332. 172.) (DiagramGraph.simple pinnedInitNodes assocs |> ok)

    let pad = 6.5 + 1.5 * sin phase
    let polished = PrismOverlap.polishLayout pad 45 graph initLayout |> ok
    let assignments = rawSpecs |> List.map (fun (id, grp, _) -> id, grp)
    let countries = GMap.compute 11. 2 assignments polished |> ok

    let seaBg =
        Shape.rectangle (Bounds.create 8. 8. 364. 184.)
        |> Mark.path "gmap:sea"
        |> Mark.withFill (Paint.rgb 224 242 254)
        |> Mark.withStroke (Paint.rgb 186 230 253) 1.

    let countryStyle = function
        | "Core" -> Paint.rgb 191 219 254, Paint.rgb 59 130 246
        | "Graph" -> Paint.rgb 167 243 208, Paint.rgb 16 185 129
        | _ -> Paint.rgb 253 230 138, Paint.rgb 217 119 6

    let countryMarks =
        countries
        |> List.collect (fun reg ->
            let fill, stroke = countryStyle reg.GroupId
            reg.Polygons
            |> List.mapi (fun pi poly ->
                Leaf(
                    Mark.path (sprintf "gmap:reg:%s:%d" reg.GroupId pi) (closedPolyPath poly)
                    |> Mark.withFill fill
                    |> Mark.withStroke stroke 1.5
                    |> Mark.withOpacity 0.88
                )))

    let edgeMarks =
        polished.Associations
        |> List.map (fun ra ->
            Leaf(
                Mark.path ("gmap:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 71 85 105) 1.15
                |> Mark.withOpacity 0.65
            ))

    let nodeBadgeFill grp =
        match grp with
        | "Core" -> Paint.rgb 30 64 175
        | "Graph" -> Paint.rgb 6 95 70
        | _ -> Paint.rgb 146 64 14

    let grpById = Map.ofList assignments
    let nodeMarks =
        polished.Nodes
        |> List.collect (fun pn ->
            let grp = Map.tryFind pn.Id grpById |> Option.defaultValue "Core"
            let boxFill = nodeBadgeFill grp
            let rect =
                roundedRect pn.Bounds 3.5
                |> Mark.path ("gmap:node:" + pn.Id)
                |> Mark.withFill boxFill
                |> Mark.withStroke (Paint.rgb 255 255 255) 1.15
            let txt =
                Mark.text ("gmap:lbl:" + pn.Id) (Point.create pn.Center.X (pn.Center.Y + 3.)) pn.Id
                |> Mark.withFill (Paint.rgb 240 249 255)
                |> Mark.withFontSize 8.3
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }
            [ Leaf rect; Leaf txt ])

    let footer =
        Mark.text "gmap:footer" (Point.create 190. 208.) "PRISM Delaunay proximity stress overlap removal -> GMap Voronoi countries"
        |> Mark.withFill (Paint.rgb 71 85 105)
        |> Mark.withFontSize 9.4
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }

    let d5Center = (polished.Nodes |> List.find (fun n -> n.Id = "D5")).Center
    [ Leaf seaBg ] @ countryMarks @ edgeMarks @ nodeMarks @ [ Leaf footer ],
    Point.create (d5Center.X - 12.) d5Center.Y

let private renderBubbleSetsCard (phase: float) : SceneNode list * Point =
    // Multi-set layout with shared intersection hubs and non-member obstacles right in the corridor!
    let specs =
        [ "A1", Point.create 56. 48.
          "A2", Point.create 58. 142.
          "AB", Point.create 174. 96.
          "B1", Point.create 288. 44.
          "BC", Point.create 268. 122.
          "C1", Point.create 156. 162.
          "C2", Point.create 318. 160.
          // Non-member obstacles forcing smooth BubbleSets contour detours!
          "Obs1", Point.create 112. 94.
          "Obs2", Point.create 228. 78.
          "Obs3", Point.create 222. 154. ]
    let nodes =
        specs
        |> List.map (fun (id, pt) ->
            let w, h = if id.StartsWith "Obs" then 26., 18. else 28., 18.
            DiagramNode.sized id w h |> DiagramNode.withPin pt)
    let assocs =
        [ Association.create "bs0" "A1" "AB"
          Association.create "bs1" "A2" "AB"
          Association.create "bs2" "AB" "B1"
          Association.create "bs3" "AB" "BC"
          Association.create "bs4" "B1" "BC"
          Association.create "bs5" "C1" "BC"
          Association.create "bs6" "BC" "C2"
          Association.create "bs7" "A2" "C1" ]
    let graph = DiagramGraph.simple nodes assocs |> ok
    let layout = pinnedLayout (Bounds.create 20. 18. 340. 168.) graph

    let sets =
        [ "SetBlue", [ "A1"; "A2"; "AB" ]
          "SetTeal", [ "AB"; "B1"; "BC" ]
          "SetRose", [ "C1"; "BC"; "C2" ] ]
    let cfg =
        { BubbleSetConfig.defaultValue with
            RadiusMember = 26. + 2.5 * sin phase
            RadiusNonMember = 21.
            NegativeWeight = 0.95
            Threshold = 0.24
            GridStep = 4.5 }
    let regions = BubbleSets.compute cfg sets layout |> ok

    let setStyle = function
        | "SetBlue" -> Paint.rgb 59 130 246, Paint.rgb 29 78 216
        | "SetTeal" -> Paint.rgb 16 185 129, Paint.rgb 4 120 87
        | _ -> Paint.rgb 244 63 94, Paint.rgb 190 18 60

    let contourFills =
        regions
        |> List.collect (fun reg ->
            let fill, stroke = setStyle reg.GroupId
            reg.Polygons
            |> List.mapi (fun pi poly ->
                let closed =
                    if poly.Length >= 3 then
                        let optPts = (poly @ [ poly.Head ]) |> List.map Some
                        Curves.line (CurveSpec.CatmullRom(0.5, CurveBoundary.Open)) optPts
                    else closedPolyPath poly
                let fillMark =
                    Mark.path (sprintf "bset:fill:%s:%d" reg.GroupId pi) closed
                    |> Mark.withFill fill
                    |> Mark.withStroke Paint.none 0.
                    |> Mark.withOpacity 0.24
                let strokeMark =
                    Mark.path (sprintf "bset:stroke:%s:%d" reg.GroupId pi) closed
                    |> Mark.withFill Paint.none
                    |> Mark.withStroke stroke 1.75
                    |> Mark.withOpacity 0.88
                [ Leaf fillMark; Leaf strokeMark ])
            |> List.concat)

    let edgeMarks =
        layout.Associations
        |> List.map (fun ra ->
            Leaf(
                Mark.path ("bset:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 100 116 139) 1.15
                |> Mark.withOpacity 0.55
            ))

    let nodeMarks =
        layout.Nodes
        |> List.collect (fun pn ->
            let isObs = pn.Id.StartsWith "Obs"
            let fill, stroke =
                if isObs then Paint.rgb 51 65 85, Paint.rgb 15 23 42
                elif pn.Id = "AB" || pn.Id = "BC" then Paint.rgb 124 58 237, Paint.rgb 255 255 255
                elif pn.Id.StartsWith "A" then Paint.rgb 29 78 216, Paint.rgb 255 255 255
                elif pn.Id.StartsWith "B" then Paint.rgb 5 150 105, Paint.rgb 255 255 255
                else Paint.rgb 225 29 72, Paint.rgb 255 255 255
            let rect =
                roundedRect pn.Bounds (if isObs then 2. else 5.)
                |> Mark.path ("bset:node:" + pn.Id)
                |> Mark.withFill fill
                |> Mark.withStroke stroke 1.2
            let txt =
                Mark.text ("bset:lbl:" + pn.Id) (Point.create pn.Center.X (pn.Center.Y + 3.)) pn.Id
                |> Mark.withFill (Paint.rgb 240 249 255)
                |> Mark.withFontSize 8.2
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }
            [ Leaf rect; Leaf txt ])

    let footer =
        Mark.text "bset:footer" (Point.create 190. 208.) "Marching-Squares potential field • Purple = 2-set hubs • Slate = obstacles"
        |> Mark.withFill (Paint.rgb 71 85 105)
        |> Mark.withFontSize 9.4
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "middle"; Baseline = "alphabetic" }

    let abCenter = (layout.Nodes |> List.find (fun n -> n.Id = "AB")).Center
    contourFills @ edgeMarks @ nodeMarks @ [ Leaf footer ],
    Point.create (abCenter.X - 10.) abCenter.Y

let private umapTsneData =
    // 48 points in R^12 across 4 distinct Gaussian-like clusters
    let prototypes =
        [| [| 3.2;  2.8; -2.4;  1.8;  2.5; -1.9;  2.2;  0.8; -2.1;  1.9;  2.4; -1.4 |]
           [| -3.0; 2.6;  2.9; -2.2;  1.7;  2.4; -1.8;  2.5;  1.6; -2.3;  0.9;  2.2 |]
           [| 2.5; -3.1;  2.4;  2.7; -2.5;  1.8; -2.3; -1.7;  2.8;  2.0; -2.2;  1.6 |]
           [| -2.8; -2.7; -2.5; -2.0; -2.4; -2.2;  2.6; -2.1; -1.9; -2.4;  2.1; -2.5 |] |]
    let rows =
        [ for c in 0 .. 3 do
            let proto = prototypes[c]
            for i in 0 .. 11 do
                let pid = sprintf "C%d_%d" c i
                let feats =
                    [ for d in 0 .. 11 ->
                        let j1 = sin (float (c * 37 + i * 13 + d * 7) * 0.91) * 0.55
                        let j2 = cos (float (c * 19 + i * 29 + d * 11) * 0.73) * 0.35
                        proto[d] + j1 + j2 ]
                yield pid, feats ]
    let dataset = FeatureDataset.ofRows rows |> ok
    let ballTree = BallTree.build DistanceMetric.Euclidean dataset
    let queryFeats = snd rows.Head
    let btNeighbors = BallTree.kNearest 5 queryFeats ballTree |> ok
    let knnLinks = NnDescent.buildGraph DistanceMetric.Euclidean 4 42u dataset |> ok

    let umapBounds = Bounds.create 18. 18. 154. 150.
    let tsneBounds = Bounds.create 208. 18. 154. 150.
    let umapPts =
        Umap2D.embed
            { UmapConfig.create umapBounds with Neighbors = 10; Epochs = 160; MinDist = 0.16; Seed = 42u }
            dataset
        |> ok
    let tsnePts =
        Tsne2D.embed
            { TsneConfig.create tsneBounds with Perplexity = 11.; Iterations = 220; LearningRate = 120.; Seed = 42u }
            dataset
        |> ok
    umapPts, tsnePts, btNeighbors, knnLinks

let private renderUmapTsneCard (phase: float) : SceneNode list * Point =
    let umapPts, tsnePts, btNeighbors, knnLinks = umapTsneData
    let umapMap = umapPts |> List.map (fun p -> p.Id, p.Position) |> Map.ofList
    let clusterColors =
        [| Paint.rgb 37 99 235
           Paint.rgb 16 185 129
           Paint.rgb 245 158 11
           Paint.rgb 225 29 72 |]
    let colorOf (id: string) =
        if id.StartsWith "C0" then clusterColors[0]
        elif id.StartsWith "C1" then clusterColors[1]
        elif id.StartsWith "C2" then clusterColors[2]
        else clusterColors[3]

    let boxL =
        Shape.rectangle (Bounds.create 12. 12. 166. 164.)
        |> Mark.path "umap2d:box"
        |> Mark.withFill (Paint.rgb 241 245 249)
        |> Mark.withStroke (Paint.rgb 203 213 225) 1.
    let boxR =
        Shape.rectangle (Bounds.create 202. 12. 166. 164.)
        |> Mark.path "tsne2d:box"
        |> Mark.withFill (Paint.rgb 241 245 249)
        |> Mark.withStroke (Paint.rgb 203 213 225) 1.

    let knnEdges =
        knnLinks
        |> List.choose (fun l ->
            if String.CompareOrdinal(l.Source, l.Target) < 0 then
                match Map.tryFind l.Source umapMap, Map.tryFind l.Target umapMap with
                | Some a, Some b ->
                    Some(
                        Leaf(
                            Mark.path (sprintf "umap2d:knn:%s:%s" l.Source l.Target) (Path.ofCommands [ MoveTo a; LineTo b ])
                            |> Mark.withFill Paint.none
                            |> Mark.withStroke (Paint.rgb 148 163 184) 0.85
                            |> Mark.withOpacity 0.45
                        )
                    )
                | _ -> None
            else None)

    // Highlight BallTree k-NN star from C0_0
    let qCenter = Map.find "C0_0" umapMap
    let btRays =
        btNeighbors
        |> List.choose (fun (nid, _) ->
            if nid = "C0_0" then None
            else
                Map.tryFind nid umapMap
                |> Option.map (fun pt ->
                    Leaf(
                        Mark.path ("umap2d:bt:" + nid) (Path.ofCommands [ MoveTo qCenter; LineTo pt ])
                        |> Mark.withFill Paint.none
                        |> Mark.withStroke (Paint.rgb 15 23 42) 1.35
                        |> Mark.withOpacity (0.55 + 0.35 * abs (sin phase))
                    )))

    let umapDots =
        umapPts
        |> List.map (fun p ->
            let r = if p.Id = "C0_0" then 5.2 else 3.8
            Leaf(discMark ("umap2d:pt:" + p.Id) p.Position r (colorOf p.Id) (Paint.rgb 255 255 255) 0.9))

    let tsneDots =
        tsnePts
        |> List.map (fun p ->
            let r = if p.Id = "C0_0" then 5.0 else 3.8
            Leaf(discMark ("tsne2d:pt:" + p.Id) p.Position r (colorOf p.Id) (Paint.rgb 255 255 255) 0.9))

    let lblL = subLabel "umap2d:lbl" 95. 192. "Umap2D + BallTree / NnDescent"
    let lblR = subLabel "tsne2d:lbl" 285. 192. "Barnes-Hut Tsne2D (theta=0.5)"
    let footer =
        Mark.text "manifold2d:footer" (Point.create 190. 210.) "48 observations in R^12 • Fuzzy simplicial SGD vs Quadtree Student-t KL"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.2
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    [ Leaf boxL; Leaf boxR ] @ knnEdges @ btRays @ umapDots @ tsneDots @ [ Leaf lblL; Leaf lblR; Leaf footer ],
    qCenter

let private isomapSammonData =
    // Left: 3D Swiss-Roll manifold unrolled by Isomap2D
    let swissSpecs =
        [ for i in 0 .. 47 ->
            let u = float i / 47.
            let t = 1.45 * Math.PI + u * 2.55 * Math.PI
            let h = -5.5 + 11.0 * (float ((i * 7) % 12) / 11.)
            let x = t * cos t
            let z = t * sin t
            sprintf "SW%d" i, u, [ x; h; z; 0.2 * cos (2. * t) ] ]
    let swissDs =
        swissSpecs |> List.map (fun (id, _, feats) -> id, feats) |> FeatureDataset.ofRows |> ok
    let swissKnn = NnDescent.buildGraph DistanceMetric.Euclidean 4 17u swissDs |> ok
    let isoPts =
        Isomap2D.embed (Bounds.create 18. 18. 154. 150.) DistanceMetric.Euclidean 7 swissDs |> ok

    // Right: Nonlinear dual-ring / twisted helix in R^6 mapped by Sammon2D
    let helixSpecs =
        [ for s in 0 .. 1 do
            for k in 0 .. 20 do
                let u = float k / 20.
                let ang = u * 2.1 * Math.PI + float s * Math.PI
                let rad = 3.0 + 0.6 * cos (2. * ang)
                let z = (u - 0.5) * 5.0
                let w1 = 1.4 * sin ang
                let w2 = 1.4 * cos ang
                let w3 = if s = 0 then -1.6 else 1.6
                yield sprintf "SM%d_%d" s k, s, u, [ rad * cos ang; rad * sin ang; z; w1; w2; w3 ] ]
    let sammonDs =
        helixSpecs |> List.map (fun (id, _, _, feats) -> id, feats) |> FeatureDataset.ofRows |> ok
    let sammonPts =
        Sammon2D.embed (Bounds.create 208. 18. 154. 150.) DistanceMetric.Euclidean 160 42u sammonDs |> ok

    let tMap = swissSpecs |> List.map (fun (id, u, _) -> id, u) |> Map.ofList
    let sMap = helixSpecs |> List.map (fun (id, s, u, _) -> id, (s, u)) |> Map.ofList
    (isoPts, swissKnn, tMap), (sammonPts, sMap)

let private renderIsomapSammonCard (phase: float) : SceneNode list * Point =
    let (isoPts, swissKnn, tMap), (sammonPts, sMap) = isomapSammonData
    let isoMap = isoPts |> List.map (fun p -> p.Id, p.Position) |> Map.ofList
    let samMap = sammonPts |> List.map (fun p -> p.Id, p.Position) |> Map.ofList

    let boxL =
        Shape.rectangle (Bounds.create 12. 12. 166. 164.)
        |> Mark.path "iso2d:box"
        |> Mark.withFill (Paint.rgb 241 245 249)
        |> Mark.withStroke (Paint.rgb 203 213 225) 1.
    let boxR =
        Shape.rectangle (Bounds.create 202. 12. 166. 164.)
        |> Mark.path "sam2d:box"
        |> Mark.withFill (Paint.rgb 241 245 249)
        |> Mark.withStroke (Paint.rgb 203 213 240) 1.

    let isoEdges =
        swissKnn
        |> List.choose (fun l ->
            if String.CompareOrdinal(l.Source, l.Target) < 0 then
                match Map.tryFind l.Source isoMap, Map.tryFind l.Target isoMap with
                | Some a, Some b ->
                    Some(
                        Leaf(
                            Mark.path (sprintf "iso2d:e:%s:%s" l.Source l.Target) (Path.ofCommands [ MoveTo a; LineTo b ])
                            |> Mark.withFill Paint.none
                            |> Mark.withStroke (Paint.rgb 148 163 184) 0.9
                            |> Mark.withOpacity 0.52
                        )
                    )
                | _ -> None
            else None)

    let cursorIdx = int (Math.Round((0.2 + 0.6 * abs (sin phase)) * 47.))
    let cursorId = sprintf "SW%d" (Math.Clamp(cursorIdx, 0, 47))

    let isoDots =
        isoPts
        |> List.map (fun p ->
            let u = Map.tryFind p.Id tMap |> Option.defaultValue 0.5
            let isCur = (p.Id = cursorId)
            let r = if isCur then 5.4 else 3.9
            let stroke = if isCur then Paint.rgb 15 23 42 else Paint.rgb 255 255 255
            Leaf(discMark ("iso2d:pt:" + p.Id) p.Position r (spectrumPaint u) stroke (if isCur then 1.4 else 0.9)))

    let strandChains =
        [ for s in 0 .. 1 ->
            let pts = [ for k in 0 .. 20 -> Map.tryFind (sprintf "SM%d_%d" s k) samMap ]
            let col = if s = 0 then Paint.rgb 37 99 235 else Paint.rgb 225 29 72
            Leaf(
                Mark.path (sprintf "sam2d:strand:%d" s) (Shape.line Curve.Linear pts)
                |> Mark.withFill Paint.none
                |> Mark.withStroke col 1.3
                |> Mark.withOpacity 0.55
            ) ]

    let samDots =
        sammonPts
        |> List.map (fun p ->
            let s, u = Map.tryFind p.Id sMap |> Option.defaultValue (0, 0.5)
            let col =
                if s = 0 then
                    let g = int (Math.Round(90. + 95. * u))
                    Paint.rgb 29 g 216
                else
                    let g = int (Math.Round(40. + 115. * u))
                    Paint.rgb 225 g 72
            Leaf(discMark ("sam2d:pt:" + p.Id) p.Position 3.8 col (Paint.rgb 255 255 255) 0.9))

    let lblL = subLabel "iso2d:lbl" 95. 192. "Isomap2D Swiss-Roll Unrolling"
    let lblR = subLabel "sam2d:lbl" 285. 192. "Sammon2D Nonlinear Stress"
    let footer =
        Mark.text "iso-sam:footer" (Point.create 190. 210.) "k-NN Dijkstra geodesics + classical MDS | Shuffled-pair normalized stress SGD"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.2
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let probePt = Map.find "SW24" isoMap
    [ Leaf boxL; Leaf boxR ] @ isoEdges @ isoDots @ strandChains @ samDots @ [ Leaf lblL; Leaf lblR; Leaf footer ],
    probePt

let probesClusterManifolds2D (phase: float) : (string * Point) list =
    let _, p1 = renderPrismGMapCard phase
    let _, p2 = renderBubbleSetsCard phase
    let _, p3 = renderUmapTsneCard phase
    let _, p4 = renderIsomapSammonCard phase
    [ "clusters:prism-gmap", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "clusters:bubblesets", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "clusters:umap2d", Point.create (14. + p3.X) (304. + 24. + p3.Y)
      "clusters:isomap2d", Point.create (406. + p4.X) (304. + 24. + p4.Y) ]

let sceneClusterManifolds2D (phase: float) : Scene =
    let n1, _ = renderPrismGMapCard phase
    let n2, _ = renderBubbleSetsCard phase
    let n3, _ = renderUmapTsneCard phase
    let n4, _ = renderIsomapSammonCard phase

    let c1, clip1 = panelCard "c-prism-gmap" 14. 42. 380. 252. "PRISM Overlap Removal & GMap Voronoi Countries" n1
    let c2, clip2 = panelCard "c-bubblesets" 406. 42. 380. 252. "Collins BubbleSets Isocontour Regions" n2
    let c3, clip3 = panelCard "c-umap-tsne" 14. 304. 380. 254. "BallTree / NnDescent + Umap2D & Barnes-Hut Tsne2D" n3
    let c4, clip4 = panelCard "c-iso-sammon" 406. 304. 380. 254. "Geodesic Isomap2D & Sammon2D Stress Mapping" n4

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "clusters2d:bg"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "clusters2d:title" (Point.create 16. 27.) "Ranvier.D5.Graph — PRISM, GMap, BubbleSets & 2D Nonlinear Manifold Embeddings"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 16.5
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose
        { Width = 800.; Height = 570. }
        "Cluster Contours and 2D Manifold Embeddings"
        [ clip1; clip2; clip3; clip4 ]
        [ Leaf pageBg; Leaf title; c1; c2; c3; c4 ]

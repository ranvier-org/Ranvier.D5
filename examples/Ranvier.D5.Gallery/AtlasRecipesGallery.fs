module Ranvier.D5.Gallery.AtlasRecipesGallery

open System
open Ranvier.D5
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
        |> Mark.withFontSize 12.2
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

let private discMark (key: string) (pt: Point) (radius: float) (fill: Paint) (stroke: Paint) (sw: float) =
    Shape.arc 0. radius 0. (2. * Math.PI)
    |> Mark.path key
    |> Mark.withFill fill
    |> Mark.withStroke stroke sw
    |> Mark.withTransform (Transform.translate pt.X pt.Y)

let private ringOutline (key: string) (pt: Point) (radius: float) (stroke: Paint) (sw: float) (opacity: float) =
    Shape.arc 0. radius 0. (2. * Math.PI)
    |> Mark.path key
    |> Mark.withFill Paint.none
    |> Mark.withStroke stroke sw
    |> Mark.withOpacity opacity
    |> Mark.withTransform (Transform.translate pt.X pt.Y)

let private subLabel (key: string) (x: float) (y: float) (txt: string) =
    Mark.text key (Point.create x y) txt
    |> Mark.withFill (Paint.rgb 51 65 85)
    |> Mark.withFontSize 10.0
    |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }

let private frontmostNode2D (vp: Viewport) (cam: Camera3D) (layout: DiagramLayout3D) : Point =
    layout.Nodes
    |> List.choose (fun n ->
        Camera3D.projectPoint vp cam n.Center
        |> Option.map (fun (pt2, depth, _) -> depth, pt2))
    |> List.sortBy fst
    |> List.head
    |> snd

// ===========================================================================
// Shared 36-Package Ecosystem Dependency Graph
// Foundations (5, reach >= 9) <- Libraries (9, reach 4..8)
// <- Components (11, reach 1..3) <- Applications (11, reach = 0)
// ===========================================================================

let private foundationIds = Set.ofList [ "libc"; "libm"; "zlib"; "crypto"; "pthread" ]
let private libraryIds = Set.ofList [ "tls"; "http"; "xml"; "sqlite"; "pcre"; "zstd"; "ssh"; "png"; "cares" ]
let private componentIds = Set.ofList [ "curl"; "gitcore"; "avcodec"; "avformat"; "webkit"; "archive"; "cairo"; "harfbuzz"; "grpc"; "systemd"; "python" ]

let private tierColor (id: string) : Paint =
    if Set.contains id foundationIds then Paint.rgb 190 18 60
    elif Set.contains id libraryIds then Paint.rgb 14 116 144
    elif Set.contains id componentIds then Paint.rgb 5 150 105
    else Paint.rgb 79 70 229

let private ecosystemGraph =
    let allIds =
        [ "libc"; "libm"; "zlib"; "crypto"; "pthread"
          "tls"; "http"; "xml"; "sqlite"; "pcre"; "zstd"; "ssh"; "png"; "cares"
          "curl"; "gitcore"; "avcodec"; "avformat"; "webkit"; "archive"; "cairo"; "harfbuzz"; "grpc"; "systemd"; "python"
          "git"; "ffmpeg"; "nginx"; "browser"; "pkgmgr"; "editor"; "player"; "ci"; "backup"; "proxy"; "shell" ]
    let nodes =
        allIds |> List.map (fun id -> DiagramNode.sized id 18. 12.)
    let depPairs =
        [ // Applications -> Components (reach of apps = 0; reach of components = 2..3)
          "git", "gitcore"; "git", "curl"
          "ffmpeg", "avcodec"; "ffmpeg", "avformat"
          "nginx", "grpc"; "nginx", "systemd"
          "browser", "curl"; "browser", "webkit"; "browser", "cairo"; "browser", "avcodec"
          "pkgmgr", "archive"; "pkgmgr", "python"
          "editor", "gitcore"; "editor", "webkit"; "editor", "harfbuzz"
          "player", "avcodec"; "player", "avformat"; "player", "cairo"; "player", "harfbuzz"
          "ci", "curl"; "ci", "gitcore"; "ci", "grpc"; "ci", "python"
          "backup", "archive"; "backup", "systemd"
          "proxy", "grpc"; "proxy", "systemd"
          "shell", "python"
          // Components -> Libraries (reach of libraries = 6..8)
          "curl", "tls"; "curl", "http"
          "gitcore", "xml"; "gitcore", "pcre"; "gitcore", "ssh"
          "avcodec", "png"
          "avformat", "cares"
          "webkit", "http"; "webkit", "xml"; "webkit", "sqlite"
          "archive", "zstd"; "archive", "ssh"
          "cairo", "png"
          "harfbuzz", "png"
          "grpc", "tls"; "grpc", "cares"
          "systemd", "pcre"; "systemd", "zstd"
          "python", "sqlite"
          // Libraries -> Foundations (reach of foundations = 15..28 >= 9)
          "tls", "libc"; "tls", "crypto"; "tls", "pthread"
          "http", "libc"; "http", "zlib"
          "xml", "zlib"
          "sqlite", "libc"; "sqlite", "libm"; "sqlite", "pthread"
          "pcre", "libc"
          "zstd", "libc"; "zstd", "crypto"
          "ssh", "zlib"; "ssh", "crypto"
          "png", "libm"; "png", "zlib"
          "cares", "libm"; "cares", "pthread" ]
    let assocs =
        depPairs |> List.mapi (fun idx (u, v) -> Association.create (sprintf "dep_%02d" idx) u v)
    DiagramGraph.simple nodes assocs |> ok

// ===========================================================================
// CARD 1: PoincareDisk — 2D Hyperbolic DAG & Mobius Focus
// ===========================================================================

let private leftDiskBounds = Bounds.create 6. 4. 176. 176.
let private rightDiskBounds = Bounds.create 198. 4. 176. 176.

let private leftDiskState =
    PoincareDisk.buildState
        { PoincareDiskConfig.create leftDiskBounds with
            RingLayering = PoincareRingLayering.LogTransitiveReach
            CenterBias = PoincareCenterBias.FoundationsInner
            Padding = 8. }
        ecosystemGraph
    |> ok

let private rightBaseDiskState =
    PoincareDisk.buildState
        { PoincareDiskConfig.create rightDiskBounds with
            RingLayering = PoincareRingLayering.LogTransitiveReach
            CenterBias = PoincareCenterBias.FoundationsInner
            Padding = 8. }
        ecosystemGraph
    |> ok

let private renderDiskSubpanel (prefix: string) (vpBounds: Bounds) (state: PoincareDiskState) (highlightId: string option) : SceneNode list =
    let proj = PoincareDisk.project state
    let dc = PoincareDisk.diskCenter state
    let dr = PoincareDisk.diskRadius state
    let rings = PoincareDisk.ringRadii state
    let coordById =
        PoincareDisk.nodes state
        |> List.map (fun nc -> nc.Id, nc)
        |> Map.ofList

    let backdrop =
        discMark (prefix + ":bg") dc dr (Paint.rgb 241 245 249) (Paint.rgb 148 163 184) 1.3

    let ringMarks =
        rings
        |> List.mapi (fun idx r ->
            if r > 0.5 && r < dr - 0.5 then
                Some(Leaf(ringOutline (sprintf "%s:ring:%d" prefix idx) dc r (Paint.rgb 203 213 225) 0.85 0.8))
            else
                None)
        |> List.choose id

    let edgeMarks =
        proj.Associations
        |> List.map (fun ra ->
            let isFocusEdge =
                match highlightId with
                | Some hid -> ra.Source.NodeId = hid || ra.Target.NodeId = hid
                | None -> false
            let stroke = if isFocusEdge then Paint.rgb 14 116 144 else Paint.rgb 100 116 139
            let sw = if isFocusEdge then 1.35 else 0.85
            let op = if isFocusEdge then 0.78 else 0.36
            Leaf(
                Mark.path (prefix + ":edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke stroke sw
                |> Mark.withOpacity op
            ))

    let nodeRadii =
        proj.Nodes
        |> List.map (fun pn ->
            let baseBoost = if highlightId = Some pn.Id then 13.5 elif Set.contains pn.Id foundationIds then 9.2 else 6.8
            let r =
                if highlightId = Some pn.Id then 13.5
                elif Set.contains pn.Id foundationIds then Math.Clamp(min (Bounds.width pn.Bounds) (Bounds.height pn.Bounds) * 0.72, 4.0, baseBoost)
                else Math.Clamp(min (Bounds.width pn.Bounds) (Bounds.height pn.Bounds) * 0.46, 2.3, baseBoost)
            pn, r)

    // Build synthetic host layout with circular disc bounds so ConstellationLayout.inlaySubgraphs inscribes internal constellations + rim gauges!
    let miniPkgGraph =
        let mGroups = [ DiagramGroup.create "core"; DiagramGroup.create "io"; DiagramGroup.create "codec" ]
        let mNodes =
            [ for gi, g in List.indexed mGroups do
                for k in 0 .. 4 ->
                    DiagramNode.sized (sprintf "m%d_%d" gi k) 6. 6. |> DiagramNode.inGroup g.Id ]
        let mEdges =
            [ for gi in 0 .. 2 do
                for k in 1 .. 4 do
                    yield Association.create (sprintf "me_%d_%d" gi k) (sprintf "m%d_%d" gi k) (sprintf "m%d_0" gi)
              yield Association.create "mx_0" "m1_2" "m0_0"
              yield Association.create "mx_1" "m2_3" "m1_0" ]
        DiagramGraph.create mNodes mGroups mEdges |> ok

    let inlayHostLayout =
        { proj with
            Nodes =
                nodeRadii
                |> List.map (fun (pn, r) ->
                    { pn with Bounds = Bounds.create (pn.Center.X - r) (pn.Center.Y - r) (2. * r) (2. * r) }) }

    let inlaySubMap =
        nodeRadii
        |> List.choose (fun (pn, r) -> if r >= 6.8 then Some(pn.Id, miniPkgGraph) else None)
        |> Map.ofList

    let rimShareMap =
        Map.ofList
            [ "libc", [ "verified", 0.68; "progress", 0.18 ]
              "zlib", [ "verified", 0.76; "progress", 0.12 ]
              "crypto", [ "verified", 0.54; "progress", 0.24 ]
              "tls", [ "verified", 0.62; "progress", 0.22 ]
              "http", [ "verified", 0.48; "progress", 0.20 ]
              "sqlite", [ "verified", 0.58; "progress", 0.16 ] ]

    let inlays =
        ConstellationLayout.inlaySubgraphs
            { ConstellationConfig.create vpBounds with ClusterMode = ConstellationClusterMode.ExplicitGroups; BundleBeta = 0.82 }
            rimShareMap
            inlaySubMap
            inlayHostLayout
        |> ok

    let inlayByHost = inlays |> List.map (fun il -> il.HostNodeId, il) |> Map.ofList

    let obstacles =
        nodeRadii
        |> List.map (fun (pn, r) ->
            Bounds.create (pn.Center.X - r - 2.) (pn.Center.Y - r - 2.) (2. * r + 4.) (2. * r + 4.))

    let labelCandidates =
        nodeRadii
        |> List.map (fun (pn, r) ->
            let reach =
                coordById
                |> Map.tryFind pn.Id
                |> Option.map (fun nc -> float nc.Reach)
                |> Option.defaultValue 0.
            let boost = if highlightId = Some pn.Id then 500. else 0.
            let wEst = max 20. (float pn.Id.Length * 5.2 + 4.)
            let hEst = 10.0
            { LabelCandidate.Id = pn.Id
              Anchor = pn.Center
              Width = wEst
              Height = hEst
              Priority = boost + reach * 4. + r * 10.
              OffsetCandidates =
                [ Point.create 0. -(r + 8.5)
                  Point.create 0. (r + 8.5)
                  Point.create (r + wEst * 0.5 + 3.2) 0.
                  Point.create -(r + wEst * 0.5 + 3.2) 0. ] })

    let culled = LabelCull.select vpBounds 1.4 obstacles labelCandidates

    let focusHalo =
        match highlightId with
        | Some hid ->
            match nodeRadii |> List.tryFind (fun (pn, _) -> pn.Id = hid) with
            | Some(pn, r) ->
                [ Leaf(ringOutline (prefix + ":focus-halo") pn.Center (r + 4.8) (Paint.rgb 245 158 11) 1.6 0.90) ]
            | None -> []
        | None -> []

    let nodeMarks =
        nodeRadii
        |> List.collect (fun (pn, r) ->
            let strokeCol = tierColor pn.Id
            match Map.tryFind pn.Id inlayByHost with
            | Some il when not il.Nodes.IsEmpty ->
                // Dark slate disc interior revealing the inlaid internal call-graph constellation + rim verification gauge!
                let bgDisc = discMark (prefix + ":nd:" + pn.Id) pn.Center r (Paint.rgb 15 23 42) strokeCol 1.15
                let gaugeMarks =
                    il.RimGauge
                    |> List.mapi (fun gi seg ->
                        let gCol =
                            match seg.Key with
                            | Some "verified" -> Paint.rgb 16 185 129
                            | Some "progress" -> Paint.rgb 245 158 11
                            | _ -> Paint.rgb 148 163 184
                        Leaf(
                            Mark.path (sprintf "%s:rg:%s:%d" prefix pn.Id gi) seg.Path
                            |> Mark.withFill Paint.none
                            |> Mark.withStroke gCol (max 1.1 (min 1.9 seg.StrokeWidth))
                        ))
                let subDiscRims =
                    il.Clusters
                    |> List.filter (fun c -> c.IsLeafCluster)
                    |> List.mapi (fun ci c ->
                        Leaf(ringOutline (sprintf "%s:ilc:%s:%d" prefix pn.Id ci) c.Center c.Radius (Paint.rgb 148 163 184) 0.55 0.55))
                let subEdges =
                    il.Associations
                    |> List.mapi (fun ei ea ->
                        Leaf(
                            Mark.path (sprintf "%s:ile:%s:%d" prefix pn.Id ei) ea.Path
                            |> Mark.withFill Paint.none
                            |> Mark.withStroke (Paint.rgb 125 211 252) 0.55
                            |> Mark.withOpacity 0.65
                        ))
                let subDots =
                    il.Nodes
                    |> List.mapi (fun ni sn ->
                        let dotCol = if ni % 3 = 0 then Paint.rgb 52 211 153 elif ni % 4 = 1 then Paint.rgb 251 191 36 else Paint.rgb 226 232 240
                        let dr0 = max 0.65 (min 1.35 (r * 0.09))
                        Leaf(discMark (sprintf "%s:ild:%s:%d" prefix pn.Id ni) sn.Center dr0 dotCol Paint.none 0.))
                [ Leaf bgDisc ] @ gaugeMarks @ subDiscRims @ subEdges @ subDots
            | Some il ->
                let baseDisc = Leaf(discMark (prefix + ":nd:" + pn.Id) pn.Center r strokeCol (Paint.rgb 255 255 255) 0.95)
                let gaugeMarks =
                    il.RimGauge
                    |> List.mapi (fun gi seg ->
                        let gCol =
                            match seg.Key with
                            | Some "verified" -> Paint.rgb 16 185 129
                            | Some "progress" -> Paint.rgb 245 158 11
                            | _ -> Paint.rgb 148 163 184
                        Leaf(
                            Mark.path (sprintf "%s:rg:%s:%d" prefix pn.Id gi) seg.Path
                            |> Mark.withFill Paint.none
                            |> Mark.withStroke gCol 1.15
                        ))
                baseDisc :: gaugeMarks
            | None ->
                [ Leaf(discMark (prefix + ":nd:" + pn.Id) pn.Center r strokeCol (Paint.rgb 255 255 255) 0.95) ])

    let labelMarks =
        culled
        |> List.collect (fun pl ->
            let pill =
                roundedRect pl.Bounds 2.5
                |> Mark.path (prefix + ":lbl-bg:" + pl.Id)
                |> Mark.withFill (Paint.rgb 255 255 255)
                |> Mark.withOpacity 0.82
            let txt =
                Mark.text (prefix + ":lbl:" + pl.Id) (Point.create pl.Center.X (pl.Center.Y + 3.1)) pl.Id
                |> Mark.withFill (Paint.rgb 15 23 42)
                |> Mark.withFontSize 8.3
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }
            [ Leaf pill; Leaf txt ])

    [ Leaf backdrop ] @ ringMarks @ edgeMarks @ focusHalo @ nodeMarks @ labelMarks

let private renderPoincareDiskCard (phase: float) : SceneNode list * Point =
    let leftMarks = renderDiskSubpanel "pd-canon" leftDiskBounds leftDiskState None

    // At phase = 0, focus "tls" exactly; for animated phases smoothly pan Mobius focus toward "http"
    let baseCoords =
        PoincareDisk.nodes rightBaseDiskState
        |> List.map (fun nc -> nc.Id, nc.DiskPosition)
        |> Map.ofList
    let tlsPt = baseCoords["tls"]
    let httpPt = baseCoords["http"]
    let rightState =
        if abs (sin phase) <= 1e-6 then
            PoincareDisk.focusNode "tls" rightBaseDiskState |> ok
        else
            let w = 0.45 * abs (sin phase)
            let fx = tlsPt.X + (httpPt.X - tlsPt.X) * w
            let fy = tlsPt.Y + (httpPt.Y - tlsPt.Y) * w
            let fPt = Hyperbolic2D.ofDisk (Math.Clamp(fx, -0.92, 0.92)) (Math.Clamp(fy, -0.92, 0.92)) |> ok
            PoincareDisk.focusPoint fPt rightBaseDiskState

    let rightMarks = renderDiskSubpanel "pd-focus" rightDiskBounds rightState (Some "tls")

    let divider =
        Path.ofCommands [ MoveTo(Point.create 190. 8.); LineTo(Point.create 190. 184.) ]
        |> Mark.path "pd:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.

    let lblL = subLabel "pd:lblL" 94. 195. "Canonical 36-Pkg Disk"
    let lblR = subLabel "pd:lblR" 286. 195. "Mobius Focus ('tls' -> Origin)"
    let footer =
        Mark.text "pd:footer" (Point.create 190. 213.) "Conformal sizing • Orthogonal geodesic arcs • LabelCull non-overlap"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.1
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let leftProj = PoincareDisk.project leftDiskState
    let probePt = (leftProj.Nodes |> List.find (fun n -> n.Id = "libc")).Center
    leftMarks @ [ Leaf divider ] @ rightMarks @ [ Leaf lblL; Leaf lblR; Leaf footer ], probePt

// ===========================================================================
// CARD 2: ConstellationLayout — Modularity + Welzl Pack + LCA/Geodesics
// ===========================================================================

let private constellationGraph =
    let groups =
        [ DiagramGroup.create "Runtime"
          DiagramGroup.create "Compiler"
          DiagramGroup.create "Protocol"
          DiagramGroup.create "Memory" |> DiagramGroup.inGroup "Runtime"
          DiagramGroup.create "Arena" |> DiagramGroup.inGroup "Runtime"
          DiagramGroup.create "Thread" |> DiagramGroup.inGroup "Runtime"
          DiagramGroup.create "Lexer" |> DiagramGroup.inGroup "Compiler"
          DiagramGroup.create "Parser" |> DiagramGroup.inGroup "Compiler"
          DiagramGroup.create "Codegen" |> DiagramGroup.inGroup "Compiler"
          DiagramGroup.create "Socket" |> DiagramGroup.inGroup "Protocol"
          DiagramGroup.create "Tls" |> DiagramGroup.inGroup "Protocol"
          DiagramGroup.create "Http" |> DiagramGroup.inGroup "Protocol" ]
    let leafSpecs =
        [| "Memory", 15; "Arena", 11; "Thread", 10
           "Lexer", 11; "Parser", 16; "Codegen", 12
           "Socket", 10; "Tls", 12; "Http", 11 |]
    let nodes =
        [ for m in 0 .. leafSpecs.Length - 1 do
            let modName, fnCount = leafSpecs[m]
            for f in 0 .. fnCount - 1 ->
                DiagramNode.sized (sprintf "%s_%d" modName f) 14. 12.
                |> DiagramNode.inGroup modName ]
    let intra =
        [ for m in 0 .. leafSpecs.Length - 1 do
            let modName, fnCount = leafSpecs[m]
            for f in 1 .. fnCount - 1 do
                yield Association.create (sprintf "in_%d_%d_a" m f) (sprintf "%s_%d" modName f) (sprintf "%s_%d" modName ((f - 1) / 3))
                if f >= 4 && f % 2 = 0 then
                    yield Association.create (sprintf "in_%d_%d_b" m f) (sprintf "%s_%d" modName f) (sprintf "%s_%d" modName ((f - 2) / 3)) ]
    let inter =
        [ // Intra-subsystem calls (violet LCA curves)
          Association.create "sib_0" "Arena_8" "Memory_0"
          Association.create "sib_1" "Thread_7" "Memory_1"
          Association.create "sib_2" "Parser_12" "Lexer_0"
          Association.create "sib_3" "Codegen_9" "Parser_0"
          Association.create "sib_4" "Http_9" "Tls_0"
          Association.create "sib_5" "Tls_9" "Socket_0"
          // Cross-subsystem calls (amber 2-level LCA highways)
          Association.create "xsub_0" "Parser_14" "Memory_0"
          Association.create "xsub_1" "Codegen_10" "Arena_0"
          Association.create "xsub_2" "Http_8" "Memory_0"
          Association.create "xsub_3" "Tls_8" "Thread_0"
          Association.create "xsub_4" "Http_6" "Parser_1"
          Association.create "xsub_5" "Codegen_7" "Socket_0" ]
    DiagramGraph.create nodes groups (intra @ inter) |> ok

let private renderConstellationCard (phase: float) : SceneNode list * Point =
    let constBounds = Bounds.create 14. 6. 352. 176.
    let beta = Math.Clamp(0.84 + 0.10 * sin phase, 0.70, 0.96)
    let out =
        ConstellationLayout.compute
            { ConstellationConfig.create constBounds with
                ClusterMode = ConstellationClusterMode.ExplicitGroups
                ClusterPadding = 7.5
                BundleBeta = beta }
            constellationGraph
        |> ok

    let outerDisc =
        discMark "const:outer" out.OuterCenter out.OuterRadius (Paint.rgb 15 23 42) (Paint.rgb 100 116 139) 1.2

    // Outer verification progress rim gauge around the constellation disc (matching &Atlas project drill-down)
    let outerGaugeMarks =
        Hyperbolic2D.rimGaugeArcs out.OuterCenter out.OuterRadius 3.6 2.3 [ "verified", 0.58; "progress", 0.21 ]
        |> List.mapi (fun idx seg ->
            let col =
                match seg.Key with
                | Some "verified" -> Paint.rgb 16 185 129
                | Some "progress" -> Paint.rgb 245 158 11
                | _ -> Paint.rgb 148 163 184
            Leaf(
                Mark.path (sprintf "const:gauge:%d" idx) seg.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke col seg.StrokeWidth
                |> Mark.withOpacity (if seg.Key.IsSome then 0.95 else 0.45)
            ))

    let subsysOf (m: string) =
        match m with
        | "Memory" | "Arena" | "Thread" | "Runtime" -> "Runtime"
        | "Lexer" | "Parser" | "Codegen" | "Compiler" -> "Compiler"
        | _ -> "Protocol"

    let discPalette (cid: string) (isLeaf: bool) =
        if not isLeaf then
            Paint.rgb 30 41 59, Paint.rgb 100 116 139, 0.55
        else
            match subsysOf cid with
            | "Runtime" -> Paint.rgb 12 74 110, Paint.rgb 56 189 248, 0.72
            | "Compiler" -> Paint.rgb 49 46 129, Paint.rgb 129 140 248, 0.72
            | _ -> Paint.rgb 19 78 74, Paint.rgb 45 212 191, 0.72

    let clusterMarks =
        out.Clusters
        |> List.filter (fun c -> c.Depth > 0)
        |> List.sortBy (fun c -> c.Depth)
        |> List.collect (fun cd ->
            let fill, stroke, op = discPalette cd.Id cd.IsLeafCluster
            let mainDisc =
                discMark ("const:disc:" + cd.Id) cd.Center cd.Radius fill stroke (if cd.IsLeafCluster then 1.1 else 0.9)
                |> Mark.withOpacity op
            let ringGuides =
                cd.InnerRingRadii
                |> List.mapi (fun ri r ->
                    Leaf(ringOutline (sprintf "const:ir:%s:%d" cd.Id ri) cd.Center r stroke 0.55 0.34))
            let sectorBands =
                cd.SectorArcs
                |> List.mapi (fun si sa ->
                    let sCol =
                        match sa.Key with
                        | Some k ->
                            match subsysOf k with
                            | "Runtime" -> Paint.rgb 56 189 248
                            | "Compiler" -> Paint.rgb 129 140 248
                            | _ -> Paint.rgb 45 212 191
                        | None -> stroke
                    Leaf(
                        Mark.path (sprintf "const:sec:%s:%d" cd.Id si) sa.Path
                        |> Mark.withFill Paint.none
                        |> Mark.withStroke sCol (max 1.0 sa.StrokeWidth)
                        |> Mark.withOpacity 0.68
                    ))
            [ Leaf mainDisc ] @ ringGuides @ sectorBands)

    let modOf (nid: string) = nid.Split('_')[0]

    let edgeMarks =
        out.Layout.Associations
        |> List.map (fun ra ->
            let mU = modOf ra.Source.NodeId
            let mV = modOf ra.Target.NodeId
            let stroke, sw, op =
                if mU = mV then
                    (if subsysOf mU = "Compiler" then Paint.rgb 165 180 252 elif subsysOf mU = "Runtime" then Paint.rgb 125 211 252 else Paint.rgb 94 234 212), 0.85, 0.58
                elif subsysOf mU = subsysOf mV then
                    Paint.rgb 192 132 252, 1.35, 0.78
                else
                    Paint.rgb 251 191 36, 1.45, 0.84
            Leaf(
                Mark.path ("const:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke stroke sw
                |> Mark.withOpacity op
            ))

    let nodeFillFor (nid: string) =
        let h = abs (nid.GetHashCode())
        if h % 5 = 0 then Paint.rgb 52 211 153
        elif h % 7 = 0 then Paint.rgb 251 191 36
        else
            match subsysOf (modOf nid) with
            | "Runtime" -> Paint.rgb 186 230 253
            | "Compiler" -> Paint.rgb 199 210 254
            | _ -> Paint.rgb 153 246 228

    let nodeRadii =
        out.Layout.Nodes
        |> List.map (fun pn ->
            let r = Math.Clamp(min (Bounds.width pn.Bounds) (Bounds.height pn.Bounds) * 0.34, 1.4, 3.2)
            pn, r)

    let nodeMarks =
        nodeRadii
        |> List.map (fun (pn, r) ->
            Leaf(discMark ("const:nd:" + pn.Id) pn.Center r (nodeFillFor pn.Id) Paint.none 0.))

    let obstacles =
        nodeRadii
        |> List.map (fun (pn, r) -> Bounds.create (pn.Center.X - r - 1.) (pn.Center.Y - r - 1.) (2. * r + 2.) (2. * r + 2.))

    let clusterLabelCands =
        out.Clusters
        |> List.filter (fun c -> c.IsLeafCluster)
        |> List.map (fun cd ->
            let wEst = float cd.Id.Length * 5.6 + 6.
            { LabelCandidate.Id = cd.Id
              Anchor = Point.create cd.Center.X (cd.Center.Y - cd.Radius - 5.)
              Width = wEst
              Height = 11.
              Priority = 100. + cd.Radius
              OffsetCandidates =
                [ Point.create 0. 0.
                  Point.create 0. (2. * cd.Radius + 10.) ] })

    let clusterLabels =
        LabelCull.select constBounds 1.5 obstacles clusterLabelCands
        |> List.collect (fun pl ->
            let bg =
                roundedRect pl.Bounds 2.8
                |> Mark.path ("const:clbl-bg:" + pl.Id)
                |> Mark.withFill (Paint.rgb 15 23 42)
                |> Mark.withOpacity 0.84
            let txt =
                Mark.text ("const:clbl:" + pl.Id) (Point.create pl.Center.X (pl.Center.Y + 3.1)) pl.Id
                |> Mark.withFill (Paint.rgb 241 245 249)
                |> Mark.withFontSize 8.2
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "middle"; Baseline = "alphabetic" }
            [ Leaf bg; Leaf txt ])

    let subHead =
        subLabel "const:sub" 190. 195. (sprintf "108 Fns • Golden-Angle Rings • Sector Arcs • LCA beta=%.2f" beta)
    let footer =
        Mark.text "const:footer" (Point.create 190. 213.) "Staggered sub-rings + geodesic fans inside discs • Rim verification gauge"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.0
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let probePt = (out.Layout.Nodes |> List.find (fun n -> n.Id = "Parser_0")).Center
    [ Leaf outerDisc ] @ outerGaugeMarks @ clusterMarks @ edgeMarks @ nodeMarks @ clusterLabels @ [ Leaf subHead; Leaf footer ], probePt

// ===========================================================================
// CARD 3: IsotonicLayers — Log-Reach Tiers, PAVA & Disk Unrolling
// ===========================================================================

let private isoViewport = Bounds.create 62. 6. 308. 180.

let private refDiskForIso =
    PoincareDisk.layout
        { PoincareDiskConfig.create isoViewport with
            RingLayering = PoincareRingLayering.LogTransitiveReach
            CenterBias = PoincareCenterBias.FoundationsInner }
        ecosystemGraph
    |> ok

let private isoPrecomputed =
    let tiers =
        [ "applications", 0
          "components", 1
          "libraries", 4
          "foundations", 9 ]
    IsotonicLayers.compute
        { IsotonicLayersConfig.create isoViewport with
            Stratification = LayerStratification.CustomReachTiers tiers
            FoundationsAtBottom = true
            NodeGap = 4.5
            BandPadding = 5.
            ReferenceLayout = Some refDiskForIso }
        ecosystemGraph
    |> ok

let private renderIsotonicCard (phase: float) : SceneNode list * Point =
    // At phase = 0.0 -> tMorph = 1.0 (full crisp 4-tier isotonic stack);
    // at phase = 0.75 -> tMorph ~ 0.65 (mid-morph unrolling from PoincareDisk!)
    let tMorph = Math.Clamp(1.0 - 0.52 * abs (sin phase), 0.35, 1.0)
    let morphed = IsotonicLayers.interpolate tMorph refDiskForIso isoPrecomputed.Layout |> ok

    let bandStyle (lbl: string) =
        let key = lbl.ToLowerInvariant()
        if key.Contains "found" then Paint.rgb 255 241 242, Paint.rgb 225 29 72, "Foundations", "reach >= 9"
        elif key.Contains "lib" then Paint.rgb 236 254 255, Paint.rgb 14 116 144, "Libraries", "reach 4..8"
        elif key.Contains "comp" then Paint.rgb 236 253 245, Paint.rgb 5 150 105, "Components", "reach 1..3"
        else Paint.rgb 238 242 255, Paint.rgb 79 70 229, "Applications", "reach = 0"

    let bandMarks =
        isoPrecomputed.Bands
        |> List.mapi (fun bIdx band -> bIdx, band)
        |> List.collect (fun (bIdx, band) ->
            let bgFill, accent, shortTitle, subRule = bandStyle band.Label
            let by = Bounds.y band.Bounds
            let bh = Bounds.height band.Bounds
            let midY = by + bh * 0.5
            let bandOp = 0.38 + 0.54 * tMorph
            let stripe =
                roundedRect band.Bounds 4.0
                |> Mark.path (sprintf "iso:band:%d" bIdx)
                |> Mark.withFill bgFill
                |> Mark.withStroke accent 0.85
                |> Mark.withOpacity bandOp
            let bar =
                Shape.rectangle (Bounds.create (Bounds.x band.Bounds) by 3.2 bh)
                |> Mark.path (sprintf "iso:bar:%d" bIdx)
                |> Mark.withFill accent
                |> Mark.withOpacity (min 1. (bandOp + 0.15))
            let tMain =
                Mark.text (sprintf "iso:bt:%d" bIdx) (Point.create 56. (midY - 1.5)) shortTitle
                |> Mark.withFill (Paint.rgb 30 41 59)
                |> Mark.withFontSize 8.8
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "end"; Baseline = "alphabetic" }
            let tSub =
                Mark.text (sprintf "iso:bs:%d" bIdx) (Point.create 56. (midY + 8.5)) subRule
                |> Mark.withFill (Paint.rgb 100 116 139)
                |> Mark.withFontSize 7.6
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 500; Anchor = "end"; Baseline = "alphabetic" }
            [ Leaf stripe; Leaf bar; Leaf tMain; Leaf tSub ])

    let edgeMarks =
        morphed.Associations
        |> List.map (fun ra ->
            Leaf(
                Mark.path ("iso:edge:" + ra.Id) ra.Path
                |> Mark.withFill Paint.none
                |> Mark.withStroke (Paint.rgb 100 116 139) 0.9
                |> Mark.withOpacity 0.34
            ))

    let pillMarks =
        morphed.Nodes
        |> List.map (fun pn ->
            let pill =
                roundedRect pn.Bounds 3.5
                |> Mark.path ("iso:nd:" + pn.Id)
                |> Mark.withFill (tierColor pn.Id)
                |> Mark.withStroke (Paint.rgb 255 255 255) 1.0
            Leaf pill)

    let obstacles =
        morphed.Nodes
        |> List.map (fun pn -> pn.Bounds)

    let labelCands =
        morphed.Nodes
        |> List.map (fun pn ->
            let reach = Map.tryFind pn.Id isoPrecomputed.ReachByNode |> Option.defaultValue 0
            let wEst = max 18. (float pn.Id.Length * 5.0 + 4.)
            let halfH = Bounds.height pn.Bounds * 0.5
            { LabelCandidate.Id = pn.Id
              Anchor = pn.Center
              Width = wEst
              Height = 9.0
              Priority = float (reach * 5 + 10)
              OffsetCandidates =
                [ Point.create 0. -(halfH + 6.5)
                  Point.create 0. (halfH + 6.5) ] })

    let placedLabels =
        LabelCull.select isoViewport 1.2 obstacles labelCands
        |> List.map (fun pl ->
            Leaf(
                Mark.text ("iso:lbl:" + pl.Id) (Point.create pl.Center.X (pl.Center.Y + 3.0)) pl.Id
                |> Mark.withFill (Paint.rgb 15 23 42)
                |> Mark.withFontSize 7.9
                |> Mark.withTextStyle { Family = "sans-serif"; Weight = 600; Anchor = "middle"; Baseline = "alphabetic" }
            ))

    let subHead =
        subLabel "iso:sub" 190. 199. (sprintf "4 Reach Strata • Weighted PAVA Box Separation • Unroll t=%.2f" tMorph)
    let footer =
        Mark.text "iso:footer" (Point.create 190. 215.) "Unrolled from PoincareDisk angles with zero horizontal overlap"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.0
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let probePt = (morphed.Nodes |> List.find (fun n -> n.Id = "libc")).Center
    bandMarks @ edgeMarks @ pillMarks @ placedLabels @ [ Leaf subHead; Leaf footer ], probePt

// ===========================================================================
// CARD 4: PoincareBallLift — 3D Conformal Ball <-> 2D Disk Morph
// ===========================================================================

let private ballLiftSourceState =
    PoincareDisk.buildState
        { PoincareDiskConfig.create (Bounds.create -80. -80. 160. 160.) with
            RingLayering = PoincareRingLayering.LogTransitiveReach
            CenterBias = PoincareCenterBias.FoundationsInner
            Padding = 6. }
        ecosystemGraph
    |> ok

let private renderPoincareBallCard (phase: float) : SceneNode list * SceneDefinition list * Point =
    let vpSub : Viewport = { Width = 182.; Height = 186. }
    let ballRad = 68.
    let origin3 = Point3.create 0. 0. 0.

    let keyLabels = Set.ofList [ "libc"; "zlib"; "crypto"; "tls"; "http"; "sqlite" ]

    let mkTheme title =
        { Scene3DTheme.create vpSub.Width vpSub.Height title with
            ReferenceSphereRadius = Some ballRad
            NodeFill = tierColor
            AssociationStroke = fun _ isBackbone ->
                if isBackbone then Paint.rgb 14 116 144 else Paint.rgb 100 116 139
            NodeLabel = fun id -> if Set.contains id keyLabels then Some id else None }

    // Left: Full 3D Poincare Ball Globe (fold = 0.0) with quaternion yaw/pitch orbit
    let liftCfgL =
        { PoincareBallLiftConfig.create origin3 ballRad with
            RadialBoost = 1.65
            ArcSegments = 14
            Yaw = 0.36 + 0.42 * phase
            Pitch = -0.24 }
    let ball3D = PoincareBallLift.lift liftCfgL ballLiftSourceState
    let camL = Camera3D.orbit origin3 0.46 0.26 202.
    let sceneL = Scene3D.render (mkTheme "3D Poincare Ball") camL ball3D
    let grpL, defsL = embedSubScene "pbl-globe" 4. 2. sceneL

    // Right: Mid-unfold (fold = 0.45 + 0.25 * sin phase) collapsing smoothly onto Z = 0 disk
    let foldVal = Math.Clamp(0.45 + 0.25 * sin phase, 0.15, 0.85)
    let liftCfgR =
        { PoincareBallLiftConfig.create origin3 ballRad with
            RadialBoost = 1.65
            ArcSegments = 14
            Yaw = 0.32
            Pitch = -0.20 }
    let morphed3D = PoincareBallLift.morph foldVal liftCfgR ballLiftSourceState
    let camR = Camera3D.orbit origin3 (0.82 + 0.25 * phase) 0.42 204.
    let sceneR = Scene3D.render (mkTheme "Poincare Unfold") camR morphed3D
    let grpR, defsR = embedSubScene "pbl-morph" 194. 2. sceneR

    // Highlighted Z = 0 target disk rim inside right sub-panel so the 2D target plane stands out
    let diskPlanePts =
        [ for k in 0 .. 48 ->
            let a = 2. * Math.PI * float k / 48.
            let wPt = Point3.create (ballRad * cos a) (ballRad * sin a) 0.
            Camera3D.projectPoint vpSub camR wPt
            |> Option.map (fun (p, _, _) -> Point.create (194. + p.X) (2. + p.Y)) ]
    let diskPlaneRing =
        Shape.line Curve.Linear diskPlanePts
        |> Mark.path "pbl:z0-ring"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 245 158 11) 1.2
        |> Mark.withOpacity 0.72

    let divider =
        Path.ofCommands [ MoveTo(Point.create 190. 8.); LineTo(Point.create 190. 186.) ]
        |> Mark.path "pbl:div"
        |> Mark.withFill Paint.none
        |> Mark.withStroke (Paint.rgb 226 232 240) 1.

    let lblL = subLabel "pbl:lblL" 95. 199. "3D Conformal Ball (fold = 0.00)"
    let lblR = subLabel "pbl:lblR" 285. 199. (sprintf "Ball -> Disk Unfold (fold = %.2f)" foldVal)
    let footer =
        Mark.text "pbl:footer" (Point.create 190. 215.) "FNV-1a spherical lift • Quaternion SLERP • Inward H^3 Poincare geodesics"
        |> Mark.withFill (Paint.rgb 100 116 139)
        |> Mark.withFontSize 9.0
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 400; Anchor = "middle"; Baseline = "alphabetic" }

    let frontLocal = frontmostNode2D vpSub camL ball3D
    let probePt = Point.create (4. + frontLocal.X) (2. + frontLocal.Y)
    [ grpL; Leaf diskPlaneRing; grpR; Leaf divider; Leaf lblL; Leaf lblR; Leaf footer ], (defsL @ defsR), probePt

// ===========================================================================
// Public Scene & Pixel Probe Entrypoints
// ===========================================================================

let probesAtlasRecipes (phase: float) : (string * Point) list =
    let _, p1 = renderPoincareDiskCard phase
    let _, p2 = renderConstellationCard phase
    let _, p3 = renderIsotonicCard phase
    let _, _, p4 = renderPoincareBallCard phase
    [ "atlas:poincare-disk", Point.create (14. + p1.X) (42. + 24. + p1.Y)
      "atlas:constellation", Point.create (406. + p2.X) (42. + 24. + p2.Y)
      "atlas:isotonic-layers", Point.create (14. + p3.X) (304. + 24. + p3.Y)
      "atlas:poincare-ball-3d", Point.create (406. + p4.X) (304. + 24. + p4.Y) ]

let sceneAtlasRecipes (phase: float) : Scene =
    let n1, _ = renderPoincareDiskCard phase
    let n2, _ = renderConstellationCard phase
    let n3, _ = renderIsotonicCard phase
    let n4, defs4, _ = renderPoincareBallCard phase

    let c1, clip1 = panelCard "c-poincare-disk" 14. 42. 380. 252. "PoincareDisk: 2D Hyperbolic DAG & Mobius Focus" n1
    let c2, clip2 = panelCard "c-constellation" 406. 42. 380. 252. "ConstellationLayout: Modularity + Welzl Pack + LCA/Geodesics" n2
    let c3, clip3 = panelCard "c-isotonic" 14. 304. 380. 254. "IsotonicLayers: Log-Reach Tiers, PAVA & Disk Unrolling" n3
    let c4, clip4 = panelCard "c-poincare-ball" 406. 304. 380. 254. "PoincareBallLift: 3D Conformal Ball <-> 2D Disk Morph" n4

    let pageBg =
        Shape.rectangle (Bounds.create 0. 0. 800. 570.)
        |> Mark.path "atlas:bg"
        |> Mark.withFill (Paint.rgb 255 255 255)
    let title =
        Mark.text "atlas:title" (Point.create 16. 27.) "Ranvier.D5.Graph & Graph3D — Poincare Disk, Modularity Constellations, PAVA Layers & 3D Ball Lift"
        |> Mark.withFill (Paint.rgb 15 23 42)
        |> Mark.withFontSize 15.8
        |> Mark.withTextStyle { Family = "sans-serif"; Weight = 700; Anchor = "start"; Baseline = "alphabetic" }

    Scene.compose
        { Viewport.Width = 800.; Height = 570. }
        "Hyperbolic Poincare Disk, Constellation, Isotonic PAVA and 3D Ball Lift Recipes"
        ([ clip1; clip2; clip3; clip4 ] @ defs4)
        [ Leaf pageBg; Leaf title; c1; c2; c3; c4 ]

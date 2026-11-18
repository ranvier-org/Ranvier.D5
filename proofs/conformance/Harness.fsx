// Shared conformance & differential execution harness against the real compiled
// Ranvier.D5, Ranvier.D5.Charts, Ranvier.D5.Geo, and Ranvier.D5.View assemblies.
#r "../../src/Ranvier.D5.View/bin/Release/net10.0/Ranvier.dll"
#r "../../src/Ranvier.D5.View/bin/Release/net10.0/Ranvier.Anime.dll"
#r "../../src/Ranvier.D5.View/bin/Release/net10.0/Ranvier.Css.dll"
#r "../../src/Ranvier.D5.View/bin/Release/net10.0/Ranvier.Dom.dll"
#r "../../src/Ranvier.D5.View/bin/Release/net10.0/Ranvier.View.dll"
#r "../../src/Ranvier.D5.View/bin/Release/net10.0/Ranvier.D5.dll"
#r "../../src/Ranvier.D5.View/bin/Release/net10.0/Ranvier.D5.View.dll"
#r "../../src/Ranvier.D5.Charts/bin/Release/net10.0/Ranvier.D5.Charts.dll"
#r "../../src/Ranvier.D5.Geo/bin/Release/net10.0/Ranvier.D5.Geo.dll"

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Reflection
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.Geo
open Ranvier.D5.View

let fmtBool (b: bool) = if b then "true" else "false"

let fmtInts (xs: seq<int>) =
    "[" + String.Join(";", xs |> Seq.map string) + "]"

let fmtOptInt (v: int option) =
    match v with
    | Some x -> sprintf "Some(%d)" x
    | None -> "None"

let fmtOptIntList (xs: seq<int option>) =
    "[" + String.Join(";", xs |> Seq.map fmtOptInt) + "]"

// ---------------------------------------------------------------------------
// 1. Data.bin + Data.join
// ---------------------------------------------------------------------------

let runBinAndJoinFSharp (edges: int list) (values: int list) (prevKeys: int list) (nextKeys: int list) : string =
    let tagged = values |> List.mapi (fun i v -> i, float v)
    let bins = Data.bin snd (edges |> List.map float) tagged
    let assigned = Array.create values.Length None

    bins
    |> List.iteri (fun binIdx b ->
        for (origIdx, _) in b.Values do
            assigned[origIdx] <- Some binIdx)

    let joined = Data.join id prevKeys nextKeys
    let enterKeys = joined.Enter |> List.map (fun (k, _, _) -> k)
    let updateKeys = joined.Update |> List.map (fun (k, _, _, _, _) -> k)
    let exitKeys = joined.Exit |> List.map (fun (k, _, _) -> k)

    sprintf
        "bins=%s;enter=%s;update=%s;exit=%s"
        (fmtOptIntList assigned)
        (fmtInts enterKeys)
        (fmtInts updateKeys)
        (fmtInts exitKeys)

// ---------------------------------------------------------------------------
// 2. Scale.linear (clamp + map) + Scale.nice
// ---------------------------------------------------------------------------

let runScaleAndNiceFSharp
    (domLo: int)
    (domHi: int)
    (rngLo: int)
    (rngHi: int)
    (clamped: bool)
    (queries: int list)
    (niceLo: int)
    (niceHi: int)
    (niceCount: int)
    : string =
    let scale =
        Scale.linear (float domLo, float domHi) (float rngLo, float rngHi)
        |> Scale.clamp clamped

    let mapped =
        queries
        |> List.map (fun q -> Scale.map scale (float q) |> Math.Round |> int)

    let ns =
        Scale.linear (float niceLo, float niceHi) (0., 100.)
        |> Scale.nice niceCount

    let nLo = Scale.invert ns 0. |> Math.Round |> int
    let nHi = Scale.invert ns 100. |> Math.Round |> int

    sprintf "mapped=%s;nlo=%d;nhi=%d" (fmtInts mapped) nLo nHi

// ---------------------------------------------------------------------------
// 3. Spatial & Quadtree within + nearest
// ---------------------------------------------------------------------------

let runSpatialFSharp
    (pts: (int * int) list)
    (qMinX: int, qMaxX: int, qMinY: int, qMaxY: int)
    (qx: int, qy: int)
    : string =
    let items = pts |> List.mapi (fun i (x, y) -> i, Point.create (float x) (float y))
    let sIdx = Spatial.create snd items
    let qTree = Quadtree.build snd (fun _ -> 1.) items

    let bounds =
        Bounds.create (float qMinX) (float qMinY) (float (qMaxX - qMinX)) (float (qMaxY - qMinY))

    let extent =
        { QuadExtent.Min = Point.create (float qMinX) (float qMinY)
          Max = Point.create (float qMaxX) (float qMaxY) }

    let sHits = Spatial.within bounds sIdx |> List.map fst
    let qHits = Quadtree.within extent qTree |> List.map fst

    if sHits <> qHits then
        failwithf "Spatial.within (%A) <> Quadtree.within (%A)" sHits qHits

    let queryPt = Point.create (float qx) (float qy)
    let sNear = Spatial.nearest queryPt Double.PositiveInfinity sIdx |> Option.map fst
    let qNear = Quadtree.nearest queryPt Double.PositiveInfinity qTree |> Option.map fst

    if sNear <> qNear then
        failwithf "Spatial.nearest (%A) <> Quadtree.nearest (%A)" sNear qNear

    sprintf "hits=%s;near=%s" (fmtInts sHits) (fmtOptInt sNear)

// ---------------------------------------------------------------------------
// 4. Sampling.minMax + Selection.reconcile + Recipe.key
// ---------------------------------------------------------------------------

let private recipeKeyMethod =
    let asm = typeof<SelectionState>.Assembly
    let recipeType = asm.GetType("Ranvier.D5.Charts.Recipe", true)
    recipeType.GetMethod("key", BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public)

let private decodeLengthPrefixed (encoded: string) : int list =
    let out = ResizeArray<int>()
    let mutable pos = 0

    while pos < encoded.Length do
        let colon = encoded.IndexOf(':', pos)
        let len = Int32.Parse(encoded.Substring(pos, colon - pos))
        out.Add len

        for i = 0 to len - 1 do
            out.Add(int encoded[colon + 1 + i] - int 'a')

        pos <- colon + 1 + len

    out |> Seq.toList

let runSamplingSelectionFSharp
    (target: int)
    (ys: int list)
    (sourceIds: int list)
    (visibleIds: int list)
    (initAvail: int list, initSel: int list, initFocus: int option, initHover: int option)
    (keyParts: int list list)
    : string =
    let samples =
        ys
        |> List.mapi (fun i y -> { SeriesSample.Id = string i; X = float i; Y = Some(float y) })

    let kept =
        match Sampling.minMax (SamplingConfig.create target) samples with
        | Ok out -> out |> List.map (fun s -> int s.Id)
        | Error errs -> failwithf "Sampling.minMax failed: %A" errs

    let st0: SelectionState =
        { Available = initAvail |> List.map string
          Selected = initSel |> List.map string |> Set.ofList
          Focused = initFocus |> Option.map string
          Hovered = initHover |> Option.map string }

    let st1 =
        Selection.reconcile
            (sourceIds |> List.map string |> Set.ofList)
            (visibleIds |> List.map string)
            st0

    let avail1 = st1.Available |> List.map int
    let selSet = st1.Selected
    // Preserve initSel order for surviving IDs to match list filter semantics on distinct inputs
    let sel1 =
        initSel
        |> List.filter (fun id -> Set.contains (string id) selSet)

    let foc1 = st1.Focused |> Option.map int
    let hov1 = st1.Hovered |> Option.map int

    let strParts =
        keyParts
        |> List.map (fun tokList ->
            String(tokList |> List.map (fun t -> char (int 'a' + t)) |> List.toArray))

    let rawKey = recipeKeyMethod.Invoke(null, [| box strParts |]) :?> string
    let keyStream = decodeLengthPrefixed rawKey

    sprintf
        "mm=%s;avail=%s;sel=%s;foc=%s;hov=%s;key=%s"
        (fmtInts kept)
        (fmtInts avail1)
        (fmtInts sel1)
        (fmtOptInt foc1)
        (fmtOptInt hov1)
        (fmtInts keyStream)

// ---------------------------------------------------------------------------
// 5. Contour.compute + Sankey.moveNode + GeoMeasure.bounds pole invariance
// ---------------------------------------------------------------------------

let runContourSankeyGeoFSharp
    (saddlePos: bool)
    (code: int)
    (loB: int, hiB: int, y0: int, targetY: int, linkY: int)
    (ringStart: int * int)
    (ringRest: (int * int) list)
    : string =
    let a, b, c, d =
        if code = 5 then
            if saddlePos then 20., 0., 20., 0. else 6., 0., 6., 0.
        elif code = 10 then
            if saddlePos then 0., 6., 0., 6. else 0., 20., 0., 20.
        else
            let bit k = if (code &&& (1 <<< k)) <> 0 then 10. else 0.
            bit 0, bit 1, bit 2, bit 3

    let grid =
        match DensityGrid.create (Bounds.create 0. 0. 2. 2.) 2 2 [ Some a; Some b; Some d; Some c ] with
        | Ok g -> g
        | Error e -> failwithf "DensityGrid failed: %A" e

    let shells =
        match Contour.compute (ContourConfig.create [ 5. ]) grid with
        | Ok polys -> polys.Length
        | Error e -> failwithf "Contour.compute failed: %A" e

    let skCfg =
        { SankeyConfig.create (Bounds.create 0. (float loB) 200. (float (hiB - loB) + 10.)) with
            NodeWidth = 20.
            NodePadding = 8. }

    let nodeMid: SankeyNode =
        { Id = "mid"
          Layer = 0
          Bounds = Bounds.create 0. (float y0) 20. 10.
          Inflow = 0.
          Outflow = 10.
          Value = 10.
          Imbalance = 10. }

    let nodeSink: SankeyNode =
        { Id = "sink"
          Layer = 1
          Bounds = Bounds.create 180. (float loB) 20. 10.
          Inflow = 10.
          Outflow = 0.
          Value = 10.
          Imbalance = -10. }

    let edge0: SankeyLink =
        { Id = "e0"
          Source = "mid"
          Target = "sink"
          Value = 10.
          Width = 4.
          Start = Point.create 20. (float linkY)
          Finish = Point.create 180. (float loB + 5.) }

    let movedY, movedLinkY =
        match Sankey.moveNode skCfg "mid" (float targetY) { Nodes = [ nodeMid; nodeSink ]; Links = [ edge0 ] } with
        | Ok moved ->
            let mNode = moved.Nodes |> List.find (fun n -> n.Id = "mid")
            let mLink = moved.Links.Head
            Bounds.y mNode.Bounds |> Math.Round |> int, mLink.Start.Y |> Math.Round |> int
        | Error e -> failwithf "Sankey.moveNode failed: %A" e

    // Exercise real GeoMeasure.bounds on both raw dummy-pole longitudes and canonical (0, +-90) poles
    let mkPos (lon: int, lat: int) =
        match GeoPosition.create (float lon) (float lat) with
        | Ok p -> p
        | Error e -> failwithf "GeoPosition.create failed: %A" e
    let rawRing = (ringStart :: ringRest @ [ ringStart ]) |> List.map mkPos

    let canonRing =
        (ringStart :: ringRest @ [ ringStart ])
        |> List.map (fun (lon, lat) -> if abs lat = 90 then mkPos (0, lat) else mkPos (lon, lat))

    let bRaw = GeoMeasure.bounds GeoStreamConfig.defaultValue (GeoGeometry.Polygon [ rawRing ])
    let bCanon = GeoMeasure.bounds GeoStreamConfig.defaultValue (GeoGeometry.Polygon [ canonRing ])

    if bRaw <> bCanon then
        failwithf "GeoMeasure.bounds pole canonicalization mismatch: raw=%A vs canon=%A" bRaw bCanon

    // Compute canonical seamWinding directly over canonicalized ring points
    let canonPts =
        (ringStart :: ringRest @ [ ringStart ])
        |> List.map (fun (lon, lat) -> if abs lat = 90 then (0, lat) else (lon, lat))

    let mutable winding = 0

    for i = 0 to canonPts.Length - 2 do
        let lonA, _ = canonPts[i]
        let lonB, _ = canonPts[i + 1]
        let delta = lonB - lonA

        if delta > 180 then
            winding <- winding + 1
        elif delta < -180 then
            winding <- winding - 1

    sprintf "shells=%d;y=%d;linky=%d;winding=%d" shells movedY movedLinkY winding

// ---------------------------------------------------------------------------
// 6. Transition + Gesture + ReactiveForce / ReactiveLayout + InputAdapter
// ---------------------------------------------------------------------------

type GestureEvtSpec =
    | GEvDown of pid: int * x: int * target: int option
    | GEvMove of pid: int * x: int
    | GEvUp of pid: int * x: int
    | GEvCancel
    | GEvNodeExit of key: int

type DomTagSpec =
    | DPlain of id: int
    | DMark of id: int * key: int
    | DGroup of id: int * grp: int

type FakeMatrixProxy() =
    inherit DispatchProxy()

    override _.Invoke(targetMethod: MethodInfo, _args: obj[]) =
        match targetMethod.Name with
        | "get_a"
        | "get_d" -> box 1.0
        | "get_b"
        | "get_c"
        | "get_e"
        | "get_f" -> box 0.0
        | _ -> null

type FakeElementProxy() =
    inherit DispatchProxy()
    member val Attrs = Dictionary<string, string>(StringComparer.Ordinal) with get
    member val Parent: Ranvier.Dom.Element = null with get, set
    member val Captured = HashSet<int>() with get
    member val Listeners = Dictionary<string, Ranvier.Dom.Event -> unit>(StringComparer.Ordinal) with get

    override this.Invoke(targetMethod: MethodInfo, args: obj[]) =
        match targetMethod.Name with
        | "hasAttribute" -> box (this.Attrs.ContainsKey(args[0] :?> string))
        | "getAttribute" ->
            match this.Attrs.TryGetValue(args[0] :?> string) with
            | true, v -> box v
            | _ -> box ""
        | "get_parentElement" -> box this.Parent
        | "getScreenCTM" ->
            let m = DispatchProxy.Create<Ranvier.Dom.SVGMatrix, FakeMatrixProxy>()
            box m
        | "addEventListener" ->
            let name = args[0] :?> string
            let cb = args[1] :?> (Ranvier.Dom.Event -> unit)
            this.Listeners[name] <- cb
            null
        | "removeEventListener" ->
            let name = args[0] :?> string
            this.Listeners.Remove name |> ignore
            null
        | "setPointerCapture" ->
            this.Captured.Add(args[0] :?> int) |> ignore
            null
        | "hasPointerCapture" -> box (this.Captured.Contains(args[0] :?> int))
        | "releasePointerCapture" ->
            this.Captured.Remove(args[0] :?> int) |> ignore
            null
        | _ ->
            if targetMethod.ReturnType = typeof<Void> then
                null
            elif targetMethod.ReturnType.IsValueType then
                Activator.CreateInstance targetMethod.ReturnType
            else
                null

type FakePointerEventProxy() =
    inherit DispatchProxy()
    member val Target: Ranvier.Dom.EventTarget = null with get, set

    override this.Invoke(targetMethod: MethodInfo, _args: obj[]) =
        match targetMethod.Name with
        | "get_type" -> box "pointerdown"
        | "get_button" -> box 0s
        | "get_pointerId" -> box 1
        | "get_clientX"
        | "get_clientY" -> box 0.0
        | "get_target" -> box this.Target
        | _ -> null

let runTransitionGestureWorkerFSharp
    (segFrom: int, segTarget: int, segStarted: int, segDur: int)
    (retargetT: int, nextVal: int, sampleT: int)
    (evs: GestureEvtSpec list)
    (staleWorker: bool, syncVal: int, workerVal: int, reqOk: bool)
    (rootId: int, domChain: DomTagSpec list)
    : string =
    // 1. Transition (exact dyadic duration 8 with multiples of 64)
    let interp (a: int) (b: int) (t: float) =
        int (Math.Round(float a + t * float (b - a)))

    let seg0 =
        Transition.start (float segStarted) (float (max 0 segDur)) id interp segFrom segTarget

    let seg1 = Transition.retarget (float retargetT) nextVal seg0
    let sampled = Transition.sample (float sampleT) seg1

    // 2. Gesture (coordinate x mapped to exact dyadic 0.125 * x so threshold 1.0 == |dx| >= 8)
    let gCfg =
        { GestureConfig.Mode = GestureMode.Lasso
          MinZoom = 0.5
          MaxZoom = 4.0
          WheelSensitivity = 0.01
          MaxLassoPoints = 1000 }

    let toPt (x: int) = Point.create (float x * 0.125) 0.0
    let fromPt (p: Point) = int (Math.Round(p.X * 8.0))

    let mutable gState = Gesture.empty
    let allCmds = ResizeArray<GestureCommand>()

    for ev in evs do
        let fEv =
            match ev with
            | GEvDown(pid, x, tgt) -> InputEvent.Down(pid, toPt x, tgt |> Option.map (fun k -> [ string k ]))
            | GEvMove(pid, x) -> InputEvent.Move(pid, toPt x)
            | GEvUp(pid, x) -> InputEvent.Up(pid, toPt x)
            | GEvCancel -> InputEvent.Cancel
            | GEvNodeExit k -> InputEvent.NodeExit [ string k ]

        let nextState, cmds = Gesture.update gCfg fEv gState
        gState <- nextState
        allCmds.AddRange cmds

    let releases =
        allCmds
        |> Seq.sumBy (function
            | GestureCommand.Release _ -> 1
            | _ -> 0)

    let lassos =
        allCmds
        |> Seq.choose (function
            | GestureCommand.SelectLasso pts -> Some(pts |> List.map fromPt)
            | _ -> None)
        |> Seq.toList

    // 3. ReactiveForce & ReactiveLayout
    let okRes =
        function
        | Ok v -> v
        | Error e -> failwithf "%A" e

    let forceCur =
        use graph = new Graph()
        let posts = new BlockingCollection<unit -> unit>()
        let frames = ResizeArray<unit -> unit>()

        let disp =
            { HostDispatcher.Post = posts.Add
              RequestFrame = frames.Add }

        let fCfg =
            { ForceConfig.create (Point.create (float workerVal) 0.) with
                CenterStrength = 1.0
                AlphaDecay = 0.0
                VelocityDecay = 0.0
                MaxSteps = 1 }

        let gInit =
            GraphData.create [ { GraphNode.create "a" with Position = Some(Point.create 0. 0.) } ] []
            |> okRes

        let initForce = Force.initialize fCfg (Seed.create 1u) gInit |> okRes
        use force = graph.Run(fun () -> ReactiveForce.create disp { MaxSteps = 1; Budget = TimeSpan.FromSeconds 1. } initForce)

        let gSync =
            GraphData.create [ { GraphNode.create "b" with Position = Some(Point.create (float syncVal) 0.) } ] []
            |> okRes

        if staleWorker then
            let mutable fired = false

            let _obs =
                graph.Run(fun () ->
                    Api.createEffect (fun () ->
                        match force.State with
                        | WorkState.Ready _ when not fired ->
                            fired <- true
                            Api.untrack (fun () -> force.Dispatch(ForceMessage.ReplaceGraph gSync) |> okRes)
                        | _ -> ()))

            let p0 = posts.Take()
            p0 ()

            for f in frames.ToArray() do
                f ()

            let snap = graph.Run(fun () -> force.Snapshot)
            snap.Nodes.Head.Position.X |> Math.Round |> int
        else
            let p0 = posts.Take()
            p0 ()

            for f in frames.ToArray() do
                f ()

            frames.Clear()
            graph.Run(fun () -> force.Dispatch(ForceMessage.ReplaceGraph gSync) |> okRes)
            let p1 = posts.Take()
            p1 ()

            for f in frames.ToArray() do
                f ()

            let snap = graph.Run(fun () -> force.Snapshot)
            snap.Nodes.Head.Position.X |> Math.Round |> int

    let frameQueuedAfter =
        use graph = new Graph()
        let posts = new BlockingCollection<unit -> unit>()
        let frames = ResizeArray<unit -> unit>()
        let mutable shouldThrow = false

        let disp =
            { HostDispatcher.Post = posts.Add
              RequestFrame =
                fun cb ->
                    if shouldThrow then
                        shouldThrow <- false
                        invalidOp "transient frame error"

                    frames.Add cb }

        let mutable curIn = 1
        use layout = graph.Run(fun () -> ReactiveLayout.create disp (fun () -> curIn) (fun _ v -> Ok v))
        let p0 = posts.Take()
        p0 ()

        for f in frames.ToArray() do
            f ()

        frames.Clear()
        curIn <- 2
        shouldThrow <- not reqOk

        let threw =
            try
                layout.Retry()
                false
            with :? InvalidOperationException ->
                true

        if not reqOk && not threw then
            failwith "Expected Retry() to throw when reqOk = false"

        frames.Count > 0

    // 4. InputAdapter.attach DOM ancestor walk
    let markPath =
        use graph = new Graph()
        let rootSvg = DispatchProxy.Create<Ranvier.Dom.SVGGraphicsElement, FakeElementProxy>()
        let rootImpl = (box rootSvg) :?> FakeElementProxy

        let mkElem (spec: DomTagSpec) =
            let id =
                match spec with
                | DPlain i
                | DMark(i, _)
                | DGroup(i, _) -> i

            if id = rootId then
                rootSvg :> Ranvier.Dom.Element, rootImpl
            else
                let el = DispatchProxy.Create<Ranvier.Dom.SVGGraphicsElement, FakeElementProxy>()
                let impl = (box el) :?> FakeElementProxy

                match spec with
                | DPlain _ -> ()
                | DMark(_, k) -> impl.Attrs["data-d5-key"] <- string k
                | DGroup(_, g) -> impl.Attrs["data-d5-group"] <- string g

                el :> Ranvier.Dom.Element, impl

        let built = domChain |> List.map mkElem

        for i = 0 to built.Length - 2 do
            let (_, childImpl) = built[i]
            let (parentEl, _) = built[i + 1]
            childImpl.Parent <- parentEl

        let mutable capturedTarget: string list option = None
        let mutable curGS = Gesture.empty

        use _owned =
            graph.Run(fun () ->
                Api.createRoot (fun _ ->
                    InputAdapter.attach
                        rootSvg
                        (fun () -> gCfg)
                        (fun () -> curGS)
                        (fun s ->
                            curGS <- s
                            capturedTarget <- s.Target)
                        ignore))

        match built with
        | [] -> None
        | (headEl, _) :: _ ->
            let pev = DispatchProxy.Create<Ranvier.Dom.PointerEvent, FakePointerEventProxy>()
            let pevImpl = (box pev) :?> FakePointerEventProxy
            pevImpl.Target <- headEl
            rootImpl.Listeners["pointerdown"] (pev :> Ranvier.Dom.Event)
            capturedTarget |> Option.map (List.map int)

    let fmtLassoList (ls: int list list) =
        "[" + String.Join("|", ls |> List.map fmtInts) + "]"

    let fmtMarkPath =
        match markPath with
        | None -> "None"
        | Some ks -> "Some(" + fmtInts ks + ")"

    sprintf
        "samp=%d;ptrs=%d;rel=%d;lassos=%s;fcur=%d;fq=%s;mpath=%s"
        sampled
        gState.Pointers.Count
        releases
        (fmtLassoList lassos)
        forceCur
        (fmtBool frameQueuedAfter)
        fmtMarkPath

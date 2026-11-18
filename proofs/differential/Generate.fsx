// Generates seeded random differential test programs across all 6 Ranvier.D5 runners,
// writes S-expressions to <work>/programs.txt, and writes live F# observations to <work>/fsharp.txt.
#load "../conformance/Harness.fsx"

open System
open System.IO
open System.Text
open Harness

let private sexpInts (xs: int list) =
    "(" + String.Join(" ", xs |> List.map string) + ")"

let private sexpBool (b: bool) = if b then "1" else "0"

let private sexpOptInt (v: int option) =
    match v with
    | None -> "(none)"
    | Some x -> sprintf "(some %d)" x

let generate (count: int) (seed: int) (workDir: string) =
    let rng = Random seed
    let progSb = StringBuilder()
    let fsSb = StringBuilder()

    for idx = 0 to count - 1 do
        let kind = idx % 6

        let prog, obs =
            match kind with
            | 0 ->
                // 1. Data.bin + Data.join
                let edgeCount = rng.Next(2, 7)
                let mutable cur = rng.Next(-40, 10)

                let edges =
                    [ for _ = 1 to edgeCount do
                          cur <- cur + rng.Next(1, 20)
                          yield cur ]

                let valCount = rng.Next(0, 14)

                let values =
                    [ for _ = 1 to valCount do
                          if rng.Next(3) = 0 then
                              yield edges[rng.Next(edges.Length)]
                          else
                              yield rng.Next(edges.Head - 10, List.last edges + 11) ]

                let mkUnique n =
                    let s = System.Collections.Generic.HashSet<int>()
                    let out = ResizeArray<int>()

                    for _ = 1 to n do
                        let k = rng.Next(1, 15)

                        if s.Add k then
                            out.Add k

                    out |> Seq.toList

                let prevKeys = mkUnique (rng.Next(0, 10))
                let nextKeys = mkUnique (rng.Next(0, 10))

                let p =
                    sprintf
                        "(bin-join %s %s %s %s)"
                        (sexpInts edges)
                        (sexpInts values)
                        (sexpInts prevKeys)
                        (sexpInts nextKeys)

                p, runBinAndJoinFSharp edges values prevKeys nextKeys

            | 1 ->
                // 2. Scale.linear + Scale.nice
                let domLo = rng.Next(-30, 20)
                let domStep = rng.Next(1, 12)
                let domHi = if rng.Next(2) = 0 then domLo + domStep else domLo - domStep
                let mult = (if rng.Next(2) = 0 then 1 else -1) * rng.Next(0, 15)
                let rngLo = rng.Next(-200, 200)
                let rngHi = rngLo + mult * (domHi - domLo)
                let clamped = rng.Next(2) = 0
                let qCount = rng.Next(1, 10)

                let queries =
                    [ for _ = 1 to qCount do
                          yield rng.Next(-50, 51) ]

                let niceCounts = [| 1; 2; 5; 10 |]
                let niceCount = niceCounts[rng.Next(niceCounts.Length)]
                let niceSpan = rng.Next(niceCount, 1200)
                let baseLo = rng.Next(-500, 500)
                let niceLo, niceHi = if rng.Next(2) = 0 then baseLo, baseLo + niceSpan else baseLo + niceSpan, baseLo

                let p =
                    sprintf
                        "(scale-nice %d %d %d %d %s %s %d %d %d)"
                        domLo
                        domHi
                        rngLo
                        rngHi
                        (sexpBool clamped)
                        (sexpInts queries)
                        niceLo
                        niceHi
                        niceCount

                p, runScaleAndNiceFSharp domLo domHi rngLo rngHi clamped queries niceLo niceHi niceCount

            | 2 ->
                // 3. Spatial & Quadtree within + nearest
                let ptCount = rng.Next(1, 16)

                let pts =
                    [ for _ = 1 to ptCount do
                          yield rng.Next(-25, 26), rng.Next(-25, 26) ]

                let x1, x2 = rng.Next(-30, 31), rng.Next(-30, 31)
                let y1, y2 = rng.Next(-30, 31), rng.Next(-30, 31)
                let qMinX, qMaxX = min x1 x2, max x1 x2
                let qMinY, qMaxY = min y1 y2, max y1 y2
                let qx, qy = rng.Next(-30, 31), rng.Next(-30, 31)

                let sPts =
                    "("
                    + String.Join(" ", pts |> List.map (fun (x, y) -> sprintf "(%d %d)" x y))
                    + ")"

                let p =
                    sprintf "(spatial %s (%d %d %d %d) (%d %d))" sPts qMinX qMaxX qMinY qMaxY qx qy

                p, runSpatialFSharp pts (qMinX, qMaxX, qMinY, qMaxY) (qx, qy)

            | 3 ->
                // 4. Sampling.minMax + Selection.reconcile + Recipe.key
                let target = 2 * rng.Next(2, 6)
                let nPts = rng.Next(2, 24)

                let ys =
                    [ for _ = 1 to nPts do
                          yield rng.Next(-50, 51) ]

                let mkIds maxN =
                    [ for _ = 1 to rng.Next(0, maxN) do
                          yield rng.Next(1, 12) ]

                let sourceIds = mkIds 10 |> List.distinct
                let visibleIds = mkIds 12
                let initAvail = mkIds 8 |> List.distinct
                let initSel = mkIds 8 |> List.distinct |> List.sort
                let initFocus = if rng.Next(3) = 0 then None else Some(rng.Next(1, 12))
                let initHover = if rng.Next(3) = 0 then None else Some(rng.Next(1, 12))

                let keyParts =
                    [ for _ = 1 to rng.Next(1, 5) do
                          yield
                              [ for _ = 1 to rng.Next(0, 5) do
                                    yield rng.Next(0, 5) ] ]

                let sKeyParts =
                    "(" + String.Join(" ", keyParts |> List.map sexpInts) + ")"

                let p =
                    sprintf
                        "(sampling-sel %d %s %s %s (%s %s %s %s) %s)"
                        target
                        (sexpInts ys)
                        (sexpInts sourceIds)
                        (sexpInts visibleIds)
                        (sexpInts initAvail)
                        (sexpInts initSel)
                        (sexpOptInt initFocus)
                        (sexpOptInt initHover)
                        sKeyParts

                p,
                runSamplingSelectionFSharp
                    target
                    ys
                    sourceIds
                    visibleIds
                    (initAvail, initSel, initFocus, initHover)
                    keyParts

            | 4 ->
                // 5. Contour + Sankey.moveNode + GeoMeasure pole canonicalization
                let saddlePos = rng.Next(2) = 0
                let code = rng.Next(0, 16)
                let loB = rng.Next(-20, 30)
                let hiB = loB + rng.Next(0, 60)
                let y0 = rng.Next(loB, hiB + 1)
                let targetY = rng.Next(loB - 40, hiB + 41)
                let linkY = rng.Next(-20, 100)

                // Safe non-antipodal spherical ring (all in northern hemisphere lat in [20, 90] or southern [-90, -20])
                let northHemi = rng.Next(2) = 0

                let mkSph () =
                    let isPole = rng.Next(4) = 0
                    let lat = if isPole then (if northHemi then 90 else -90) else (if northHemi then rng.Next(20, 85) else rng.Next(-85, -19))
                    let lon = rng.Next(-175, 176)
                    lon, lat

                let rec genSafeRing () =
                    let r0 = mkSph ()
                    let rest = [ for _ = 1 to rng.Next(2, 6) do yield mkSph () ]
                    let full = r0 :: rest @ [ r0 ]

                    let ok =
                        [ 0 .. full.Length - 2 ]
                        |> List.forall (fun i ->
                            let lonA, latA = full[i]
                            let lonB, latB = full[i + 1]
                            abs (lonB - lonA) <> 180 && not (latA = -latB && abs latA = 90))

                    if ok then r0, rest else genSafeRing ()

                let (sLon, sLat), ringRest = genSafeRing ()

                let sRest =
                    "("
                    + String.Join(" ", ringRest |> List.map (fun (lo, la) -> sprintf "(%d %d)" lo la))
                    + ")"

                let p =
                    sprintf
                        "(contour-sankey-geo %s %d (%d %d %d %d %d) (%d %d) %s)"
                        (sexpBool saddlePos)
                        code
                        loB
                        hiB
                        y0
                        targetY
                        linkY
                        sLon
                        sLat
                        sRest

                p, runContourSankeyGeoFSharp saddlePos code (loB, hiB, y0, targetY, linkY) (sLon, sLat) ringRest

            | _ ->
                // 6. Transition + Gesture + ReactiveForce / ReactiveLayout + InputAdapter
                let segFrom = 64 * rng.Next(-4, 5)
                let segTarget = 64 * rng.Next(-4, 5)
                let segStarted = rng.Next(0, 10)
                let segDur = 8
                let retargetT = rng.Next(0, 22)
                let nextVal = 64 * rng.Next(-4, 5)
                let sampleT = rng.Next(retargetT, retargetT + 14)

                let evCount = rng.Next(1, 9)

                let evs =
                    [ for _ = 1 to evCount do
                          match rng.Next(6) with
                          | 0
                          | 1 ->
                              let pid = rng.Next(1, 4)
                              let x = rng.Next(-20, 21)
                              let tgt = if rng.Next(2) = 0 then None else Some(rng.Next(1, 8))
                              yield GEvDown(pid, x, tgt)
                          | 2
                          | 3 -> yield GEvMove(rng.Next(1, 4), rng.Next(-20, 21))
                          | 4 -> yield GEvUp(rng.Next(1, 4), rng.Next(-20, 21))
                          | _ ->
                              if rng.Next(2) = 0 then
                                  yield GEvCancel
                              else
                                  yield GEvNodeExit(rng.Next(1, 8)) ]

                let fmtEv =
                    function
                    | GEvDown(pid, x, tgt) -> sprintf "(down %d %d %s)" pid x (sexpOptInt tgt)
                    | GEvMove(pid, x) -> sprintf "(move %d %d)" pid x
                    | GEvUp(pid, x) -> sprintf "(up %d %d)" pid x
                    | GEvCancel -> "(cancel)"
                    | GEvNodeExit k -> sprintf "(exit %d)" k

                let sEvs = "(" + String.Join(" ", evs |> List.map fmtEv) + ")"

                let staleWorker = rng.Next(2) = 0
                let syncVal = rng.Next(1, 80)
                let workerVal = rng.Next(1, 80)
                let reqOk = rng.Next(2) = 0

                let chainLen = rng.Next(1, 6)
                let rootPos = rng.Next(0, chainLen + 1) // chainLen means root is absent!
                let rootId = 100

                let domChain =
                    [ for i = 0 to chainLen - 1 do
                          let nid = if i = rootPos then rootId else i + 1

                          if nid = rootId then
                              yield DPlain nid
                          else
                              match rng.Next(3) with
                              | 0 -> yield DPlain nid
                              | 1 -> yield DMark(nid, rng.Next(1, 15))
                              | _ -> yield DGroup(nid, rng.Next(1, 15)) ]

                let fmtDom =
                    function
                    | DPlain nid -> sprintf "(plain %d)" nid
                    | DMark(nid, k) -> sprintf "(mark %d %d)" nid k
                    | DGroup(nid, g) -> sprintf "(group %d %d)" nid g

                let sDom = "(" + String.Join(" ", domChain |> List.map fmtDom) + ")"

                let p =
                    sprintf
                        "(trans-gesture-worker (%d %d %d %d) (%d %d %d) %s (%s %d %d %s) (%d %s))"
                        segFrom
                        segTarget
                        segStarted
                        segDur
                        retargetT
                        nextVal
                        sampleT
                        sEvs
                        (sexpBool staleWorker)
                        syncVal
                        workerVal
                        (sexpBool reqOk)
                        rootId
                        sDom

                p,
                runTransitionGestureWorkerFSharp
                    (segFrom, segTarget, segStarted, segDur)
                    (retargetT, nextVal, sampleT)
                    evs
                    (staleWorker, syncVal, workerVal, reqOk)
                    (rootId, domChain)

        progSb.Append(prog).Append('\n') |> ignore
        fsSb.Append(sprintf "%d\tok\t%s\n" idx obs) |> ignore

    File.WriteAllText(Path.Combine(workDir, "programs.txt"), progSb.ToString(), UTF8Encoding false)
    File.WriteAllText(Path.Combine(workDir, "fsharp.txt"), fsSb.ToString(), UTF8Encoding false)
    printfn "Generate.fsx: ran %d F# differential programs (seed %d)" count seed

let argv = fsi.CommandLineArgs[1..]
let count = if argv.Length >= 1 then int argv[0] else 400
let seed = if argv.Length >= 2 then int argv[1] else 1
let workDir = if argv.Length >= 3 then argv[2] else __SOURCE_DIRECTORY__
generate count seed workDir

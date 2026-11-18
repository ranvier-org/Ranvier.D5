// Generates theories/Conformance/Expected.v by running 10 deterministic conformance scenarios
// directly against the real Ranvier.D5, Ranvier.D5.Charts, Ranvier.D5.Geo, and Ranvier.D5.View assemblies.
#load "Harness.fsx"

open System
open System.IO
open System.Text
open Harness

let private parseFields (obs: string) : Map<string, string> =
    let items = ResizeArray<string>()
    let mutable depth = 0
    let mutable start = 0

    for i = 0 to obs.Length - 1 do
        match obs[i] with
        | '['
        | '(' -> depth <- depth + 1
        | ']'
        | ')' -> depth <- depth - 1
        | ';' when depth = 0 ->
            items.Add(obs.Substring(start, i - start))
            start <- i + 1
        | _ -> ()

    items.Add(obs.Substring start)

    items
    |> Seq.map (fun kv ->
        let eq = kv.IndexOf '='
        kv.Substring(0, eq), kv.Substring(eq + 1))
    |> Map.ofSeq

let private parseInts (s: string) : int list =
    let inner = s.TrimStart('[').TrimEnd(']')

    if String.IsNullOrWhiteSpace inner then
        []
    else
        inner.Split ';' |> Array.map int |> Array.toList

let private parseOptInt (s: string) : int option =
    if s = "None" then
        None
    else
        Some(int (s.Substring(5, s.Length - 6)))

let private parseOptInts (s: string) : int option list =
    let inner = s.TrimStart('[').TrimEnd(']')

    if String.IsNullOrWhiteSpace inner then
        []
    else
        inner.Split ';' |> Array.map parseOptInt |> Array.toList

let private coqZ (x: int) =
    if x < 0 then sprintf "(%d)%%Z" x else sprintf "%d%%Z" x

let private coqZList (xs: int list) =
    "[" + String.Join("; ", xs |> List.map coqZ) + "]"

let private coqNatList (xs: int list) =
    "[" + String.Join("; ", xs |> List.map (sprintf "%d%%nat")) + "]"

let private coqOptNat (v: int option) =
    match v with
    | None -> "None"
    | Some x -> sprintf "(Some %d%%nat)" x

let private coqOptNatList (xs: int option list) =
    "[" + String.Join("; ", xs |> List.map coqOptNat) + "]"

let private emitBinJoin (name: string) (obs: string) (line: string -> unit) =
    let f = parseFields obs

    line (
        sprintf
            "Definition exp_%s : list (option nat) * join_model :=\n  (%s,\n   {| jm_enter := %s; jm_update := %s; jm_exit := %s |})."
            name
            (coqOptNatList (parseOptInts f["bins"]))
            (coqNatList (parseInts f["enter"]))
            (coqNatList (parseInts f["update"]))
            (coqNatList (parseInts f["exit"]))
    )

    line ""

let private emitScaleNice (name: string) (obs: string) (line: string -> unit) =
    let f = parseFields obs

    line (
        sprintf
            "Definition exp_%s : list Z * (Z * Z) :=\n  (%s, (%s, %s))."
            name
            (coqZList (parseInts f["mapped"]))
            (coqZ (int f["nlo"]))
            (coqZ (int f["nhi"]))
    )

    line ""

let private emitSpatial (name: string) (obs: string) (line: string -> unit) =
    let f = parseFields obs

    line (
        sprintf
            "Definition exp_%s : list nat * option nat :=\n  (%s, %s)."
            name
            (coqNatList (parseInts f["hits"]))
            (coqOptNat (parseOptInt f["near"]))
    )

    line ""

let private emitSamplingSel (name: string) (obs: string) (line: string -> unit) =
    let f = parseFields obs

    line (
        sprintf
            "Definition exp_%s : list nat * sel_state * list nat :=\n  (%s,\n   {| sel_available := %s; sel_selected := %s; sel_focused := %s; sel_hovered := %s |},\n   %s)."
            name
            (coqNatList (parseInts f["mm"]))
            (coqNatList (parseInts f["avail"]))
            (coqNatList (parseInts f["sel"]))
            (coqOptNat (parseOptInt f["foc"]))
            (coqOptNat (parseOptInt f["hov"]))
            (coqNatList (parseInts f["key"]))
    )

    line ""

let private emitContourSankeyGeo (name: string) (obs: string) (line: string -> unit) =
    let f = parseFields obs

    line (
        sprintf
            "Definition exp_%s : nat * (Z * Z) * Z :=\n  (%d%%nat, (%s, %s), %s)."
            name
            (int f["shells"])
            (coqZ (int f["y"]))
            (coqZ (int f["linky"]))
            (coqZ (int f["winding"]))
    )

    line ""

let private emitTransGestureWorker (name: string) (obs: string) (line: string -> unit) =
    let f = parseFields obs
    let lasInner = f["lassos"].TrimStart('[').TrimEnd(']')

    let coqLassos =
        if String.IsNullOrWhiteSpace lasInner then
            "[]"
        else
            let parts =
                lasInner.Split '|'
                |> Array.map (parseInts >> coqZList)
                |> String.concat "; "

            "[" + parts + "]"

    let mpathStr = f["mpath"]

    let coqMpath =
        if mpathStr = "None" then
            "None"
        else
            let inner = mpathStr.Substring(5, mpathStr.Length - 6)
            sprintf "(Some %s)" (coqNatList (parseInts inner))

    line (
        sprintf
            "Definition exp_%s : Z * (nat * nat * list (list Z)) * (nat * bool) * option (list nat) :=\n  (%s, (%d%%nat, %d%%nat, %s), (%d%%nat, %s), %s)."
            name
            (coqZ (int f["samp"]))
            (int f["ptrs"])
            (int f["rel"])
            coqLassos
            (int f["fcur"])
            f["fq"]
            coqMpath
    )

    line ""

let emitExpected (outPath: string) =
    let sb = StringBuilder()
    let line (s: string) = sb.AppendLine s |> ignore

    line "(* Generated by proofs/conformance/Scenarios.fsx from real F# runs; do not edit by hand. *)"
    line "From Stdlib Require Import List ZArith."
    line "From RanvierD5.Pure Require Import DataScale Spatial SamplingSelection GeometryGeo TransitionWorker."
    line "From RanvierD5 Require Import Extract."
    line "Import ListNotations."
    line ""

    // S01: Data.bin interior + inclusive final edge + out-of-range, and Data.join Enter/Update/Exit
    emitBinJoin
        "S01"
        (runBinAndJoinFSharp [ 0; 10; 20; 30 ] [ -5; 0; 9; 10; 20; 30; 31 ] [ 1; 2; 3; 4 ] [ 3; 5; 1; 6 ])
        line

    // S02: Clamped reversed-range Scale.linear + Scale.nice on [101, 199] count=2 (Issue #64)
    emitScaleNice
        "S02"
        (runScaleAndNiceFSharp 0 10 1000 0 true [ -5; 0; 3; 10; 15 ] 101 199 2)
        line

    // S03: Unclamped extrapolating Scale.linear + Scale.nice on reversed domain [88, 12] count=5
    emitScaleNice
        "S03"
        (runScaleAndNiceFSharp -10 10 -200 200 false [ -15; -10; 0; 5; 12 ] 88 12 5)
        line

    // S04: Spatial & Quadtree within + nearest with exact tie-breaking toward earlier ordinal
    emitSpatial
        "S04"
        (runSpatialFSharp [ (10, 10); (-10, 10); (10, -10); (3, 4); (-3, -4); (25, 25) ] (-12, 12, -5, 12) (0, 0))
        line

    // S05: Sampling.minMax + Selection.reconcile hidden-ID survival + Recipe.key length-prefixed encoding
    emitSamplingSel
        "S05"
        (runSamplingSelectionFSharp
            6
            [ 10; 50; 2; 30; 80; -5; 40; 12; 90; 20 ]
            [ 1; 2; 3; 4; 5 ]
            [ 1; 3; 5; 3; 99 ]
            ([ 1; 2; 3; 4; 5 ], [ 2; 3; 99 ], Some 2, Some 3)
            [ [ 1; 2 ]; []; [ 0; 3; 4 ] ])
        line

    // S06: Contour separated saddle (code=5, saddlePos=false), Sankey upper clamp, and North Pole dummy longitude (-170, 90) (Issue #66)
    emitContourSankeyGeo
        "S06"
        (runContourSankeyGeoFSharp false 5 (10, 50, 20, 90, 25) (20, 80) [ (-170, 90); (10, 80) ])
        line

    // S07: Contour merged saddle (code=5, saddlePos=true), Sankey lower clamp, and antimeridian South Pole cap
    emitContourSankeyGeo
        "S07"
        (runContourSankeyGeoFSharp true 5 (10, 50, 30, -20, 35) (-150, -60) [ (-30, -60); (90, -60); (170, -60) ])
        line

    // S08: Transition C0 retargeting, Lasso sub-pixel accumulation (Issue #62), ReactiveForce stale worker fence (Issue #65), and <svg> outer HTML isolation (Issue #67)
    emitTransGestureWorker
        "S08"
        (runTransitionGestureWorkerFSharp
            (0, 64, 0, 8)
            (4, 128, 6)
            [ GEvDown(1, 0, Some 7); GEvMove(1, 5); GEvMove(1, 10); GEvMove(1, 15); GEvUp(1, 15) ]
            (true, 42, 99, true)
            (100, [ DMark(1, 7); DGroup(2, 3); DPlain 100; DGroup(3, 88) ]))
        line

    // S09: Two-pointer pinch cancel release balance, fresh ReactiveForce worker + transient RequestFrame rollback (Issue #65), and detached DOM chain (Issue #67)
    emitTransGestureWorker
        "S09"
        (runTransitionGestureWorkerFSharp
            (64, 0, 10, 8)
            (12, -64, 20)
            [ GEvDown(1, 0, Some 5); GEvDown(2, 16, None); GEvUp(1, 8) ]
            (false, 42, 99, false)
            (100, [ DMark(1, 7); DGroup(2, 3); DPlain 50 ]))
        line

    // S10: Duplicate observations on exact interior & final bin boundaries
    emitBinJoin
        "S10"
        (runBinAndJoinFSharp [ -20; 0; 20 ] [ -20; -20; 0; 0; 20; 20 ] [ 10; 20 ] [ 20; 30 ])
        line

    let text = sb.ToString().Replace("\r\n", "\n")
    let dir = Path.GetDirectoryName outPath

    if not (String.IsNullOrEmpty dir) then
        Directory.CreateDirectory dir |> ignore

    File.WriteAllText(outPath, text, UTF8Encoding false)

let argv = fsi.CommandLineArgs[1..]

let target =
    if argv.Length >= 1 then
        argv[0]
    else
        Path.Combine(__SOURCE_DIRECTORY__, "..", "theories", "Conformance", "Expected.v")

emitExpected target

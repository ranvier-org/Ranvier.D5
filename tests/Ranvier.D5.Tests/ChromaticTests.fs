module Ranvier.D5.Tests.ChromaticTests

open Expecto
open Ranvier.Anime
open Ranvier.D5

let tests=testList "Chromatic toolkit integration" [
    testCase "sequential endpoints and cyclic wrap" <| fun _ ->
        let ramp=Chromatic.sequential ColorInterpolation.Lab GamutPolicy.Clip Chromatic.blues
        Expect.equal (ramp 0.) (List.head Chromatic.blues) "start"
        Expect.equal (ramp 1.) (List.last Chromatic.blues) "end"
        let cycle=Chromatic.cyclic ColorInterpolation.Hcl GamutPolicy.Clip Chromatic.category10
        Expect.equal (cycle -0.25) (cycle 0.75) "negative wrap"
        Expect.equal (cycle 0.) (cycle 1.) "seam"
    testCase "diverging center and deterministic category assignment" <| fun _ ->
        let ramp=Chromatic.diverging ColorInterpolation.Srgb GamutPolicy.Clip Chromatic.redBlue
        Expect.equal (ramp 0.5) Chromatic.redBlue[5] "neutral center"
        Expect.equal (Chromatic.categorical Chromatic.category10 -1) (List.last Chromatic.category10) "negative category wraps"
        Expect.throws (fun () -> Chromatic.sequential ColorInterpolation.Lab GamutPolicy.Clip [] |> ignore) "empty rejected"
        Expect.throws (fun () -> ramp nan |> ignore) "nonfinite rejected"
    testCase "paint serializes alpha through CSS and clips explicitly" <| fun _ ->
        let color={Red=1.2;Green=0.;Blue=0.;Alpha=0.5}
        Expect.stringContains (Chromatic.paint GamutPolicy.Clip color |> Paint.toCss) "0.5" "alpha retained"
        Expect.throws (fun () -> Chromatic.paint GamutPolicy.Reject color |> ignore) "gamut rejection"
]

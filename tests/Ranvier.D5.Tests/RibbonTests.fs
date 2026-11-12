module Ranvier.D5.Tests.RibbonTests

open System
open Expecto
open Ranvier.D5

let private flow =
    {Id=struct(0,1);Source={Index=0;StartAngle=0.;EndAngle=Math.PI/2.;Value=1.}
     Target={Index=1;StartAngle=Math.PI;EndAngle=3.*Math.PI/2.;Value=1.}}
let private point expected actual =
    Expect.floatClose Accuracy.high actual.X expected.X "x"
    Expect.floatClose Accuracy.high actual.Y expected.Y "y"
let tests=testList "Ribbon" [
    testCase "bidirectional ribbon connects two circular arcs through quadratic controls" <| fun _ ->
        let commands=Ribbon.path {RibbonSpec.create 10. flow with TargetRadius=12.} |> Path.commands
        match commands with
        | [MoveTo first;ArcTo(10.,10.,0.,false,true,sourceEnd);QuadraticTo(control,targetStart);ArcTo(12.,12.,0.,false,true,targetEnd);QuadraticTo(back,last);Close] ->
            point (Point.create 0. -10.) first
            point (Point.create 10. 0.) sourceEnd
            point (Point.create 0. 12.) targetStart
            point (Point.create -12. 0.) targetEnd
            point (Point.create 0. 0.) control
            point control back
            point first last
        | _ -> failtestf "Unexpected ribbon commands: %A" commands
    testCase "arrow ribbon points at target midpoint and padding trims source arc" <| fun _ ->
        let spec={RibbonSpec.create 10. flow with ArrowHead=Some 3.;PadAngle=0.2}
        match Ribbon.path spec |> Path.commands with
        | [MoveTo start;ArcTo _;QuadraticTo(_,inner);LineTo tip;LineTo _;QuadraticTo _;Close] ->
            point (Point.create (10.*sin 0.1) (-10.*cos 0.1)) start
            Expect.floatClose Accuracy.high (Double.Hypot(inner.X,inner.Y)) 7. "arrow inset"
            point (Point.create (-sqrt 50.) (sqrt 50.)) tip
        | other -> failtestf "Unexpected arrow commands: %A" other
    testCase "full circle uses two arcs and degenerate radii are finite" <| fun _ ->
        let whole={flow.Source with EndAngle=2.*Math.PI}
        let spec={RibbonSpec.create 10. flow with Source=whole;Target=whole}
        let arcs=Ribbon.path spec |> Path.commands |> List.filter(function ArcTo _ -> true | _ -> false)
        Expect.equal arcs.Length 2 "full circle cannot use coincident-endpoint single arc"
        Expect.isEmpty (Ribbon.path (RibbonSpec.create 0. flow) |> Path.commands) "zero radii"
        for invalid in [{spec with SourceRadius= -1.};{spec with PadAngle=nan};{spec with ArrowHead=Some 11.};{spec with Source={whole with EndAngle= -1.}}] do
            Expect.throwsT<ArgumentException> (fun () -> Ribbon.path invalid |> ignore) "invalid geometry rejected"
]

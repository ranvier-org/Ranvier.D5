module Ranvier.D5.Tests.GraticuleTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok x -> x | Error e -> failtestf "Unexpected error: %A" e
let private xy (p: GeoPosition) = p.Longitude,p.Latitude

let tests = testList "Graticule" [
    testCase "default grid retains equator and avoids repeated major meridians" <| fun _ ->
        let grid=Graticule.create GraticuleConfig.defaultValue |> ok
        Expect.equal grid.Lines.Length 53 "4 major meridians, equator, 32 minor meridians, 16 minor parallels"
        let meridians=grid.Lines |> List.filter(fun l -> l |> List.forall(fun p -> p.Longitude=l.Head.Longitude))
        Expect.equal meridians.Length 36 "Every ten degrees once"
        Expect.equal (meridians |> List.map(fun l -> l.Head.Longitude) |> Set.ofList |> Set.count) 36 "No duplicate major meridian"
        let equator=grid.Lines |> List.filter(List.forall(fun p -> p.Latitude=0.))
        Expect.equal equator.Length 1 "Equator occurs once"
        Expect.equal equator.Head.Length 145 "360 degrees sampled every 2.5 degrees, both endpoints"
    testCase "custom outline samples all four edges and closes clockwise" <| fun _ ->
        let extent={West = -10.;South = -5.;East=10.;North=5.}
        let grid=Graticule.create {GraticuleConfig.defaultValue with MajorExtent=extent;MinorExtent=extent;Precision=10.} |> ok
        Expect.equal (List.map xy grid.Outline) [(-10.,-5.);(-10.,5.);(0.,5.);(10.,5.);(10.,-5.);(0.,-5.);(-10.,-5.)] "Clockwise closed perimeter"
        Expect.isTrue (grid.Lines |> List.concat |> List.forall(fun p -> p.Longitude >= -10. && p.Longitude<=10. && p.Latitude >= -5. && p.Latitude<=5.)) "Clipped to requested extents"
    testCase "sub-microdegree grids retain distinct minor lines" <| fun _ ->
        let extent={West = -1e-8;South = -1e-8;East=1e-8;North=1e-8}
        let config={GraticuleConfig.defaultValue with MajorExtent=extent;MinorExtent=extent;MajorLongitudeStep=1e-8;MajorLatitudeStep=1e-8;MinorLongitudeStep=5e-9;MinorLatitudeStep=5e-9;Precision=1e-8}
        let grid=Graticule.create config |> ok
        Expect.equal grid.Lines.Length 8 "Four major and four distinct minor lines"
    testCase "invalid ranges and steps are typed errors" <| fun _ ->
        let d=GraticuleConfig.defaultValue
        for config in [ {d with Precision=0.};{d with MinorLatitudeStep=nan};{d with MajorLongitudeStep=infinity};{d with MaxPoints=0};{d with MaxWork=0L};{d with MajorExtent={d.MajorExtent with West=181.}};{d with MinorExtent={d.MinorExtent with North=91.}} ] do
            Expect.isError (Graticule.create config) "Reject invalid config"
    testCase "tiny steps and output budgets fail before unbounded enumeration" <| fun _ ->
        let d=GraticuleConfig.defaultValue
        for config in [{d with MinorLongitudeStep=Double.Epsilon};{d with Precision=Double.Epsilon};{d with MaxPoints=2};{d with MaxWork=2L}] do
            match Graticule.create config with
            | Error [BudgetExceeded _] -> ()
            | result -> failtestf "Expected bounded refusal, got %A" result
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> Graticule.createWithCancellation d cancelled.Token |> ignore) "Cancellation propagates"
]

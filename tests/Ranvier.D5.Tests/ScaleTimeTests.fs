module Ranvier.D5.Tests.ScaleTimeTests

open System
open System.Globalization
open Expecto
open Ranvier.D5

let tests = testList "Scale and time expansion" [
    testCase "piecewise mapping and inversion retain knots and direction" <| fun _ ->
        let s=Scale.piecewise [0.;5.;10.] [0.;20.;100.]
        DataScaleTests.near 60. (Scale.piecewiseMap s 7.5)
        DataScaleTests.near 7.5 (Scale.piecewiseInvert s 60.)
        let reversed=Scale.piecewise [10.;5.;0.] [100.;20.;0.]
        DataScaleTests.near 60. (Scale.piecewiseMap reversed 7.5)
        Expect.throws (fun () -> Scale.piecewise [0.;1.;1.] [0.;1.;2.] |> ignore) "duplicate knots"
    testCase "log ticks include minor ticks and preserve negative direction" <| fun _ ->
        let positive=Scale.logTicks 10. 10 (Scale.log 10. (1.,1000.) (0.,1.))
        Expect.equal positive.Length 28 "D3 decimal ticks"
        Expect.equal (List.head positive,List.last positive) (1.,1000.) "endpoints"
        let negative=Scale.logTicks 10. 10 (Scale.log 10. (-1.,-1000.) (0.,1.))
        Expect.equal negative (List.map (~-) positive) "signed direction"
    testCase "threshold inversion represents unbounded ends and repeated outputs" <| fun _ ->
        Expect.equal (Scale.invertExtent [10.;20.] ["a";"b";"a"] "a") [(None,Some 10.);(Some 20.,None)] "all intervals"
    testCase "UTC interval multiples and reversed domains" <| fun _ ->
        let a=DateTimeOffset(2024,1,1,0,2,0,TimeSpan.Zero)
        let b=a.AddMinutes 20.
        let spec={TimeIntervalSpec.Unit=TimeInterval.Minute;Step=5}
        let ticks=TimeTicks.utc spec (a,b)
        Expect.equal (ticks |> List.map (fun t -> t.Minute)) [5;10;15;20] "aligned multiples"
        Expect.equal (TimeTicks.utc spec (b,a)) (List.rev ticks) "reversed"
    testCase "explicit timezone preserves both fold instants" <| fun _ ->
        let zone=TimeZoneInfo.FindSystemTimeZoneById "America/New_York"
        let a=DateTimeOffset(2024,11,3,4,0,0,TimeSpan.Zero)
        let ticks=TimeTicks.inZone zone {Unit=TimeInterval.Hour;Step=1} (a,a.AddHours 4.)
        Expect.equal (ticks |> List.filter (fun t -> (TimeZoneInfo.ConvertTime(t,zone)).Hour=1) |> List.length) 2 "both 1 AM offsets"
    testCase "DST gaps omit missing wall-clock hours" <| fun _ ->
        let zone=TimeZoneInfo.FindSystemTimeZoneById "America/New_York"
        let a=DateTimeOffset(2024,3,10,5,0,0,TimeSpan.Zero)
        let ticks=TimeTicks.inZone zone {Unit=TimeInterval.Hour;Step=1} (a,a.AddHours 4.)
        Expect.equal ticks.Length 5 "five real instants"
        Expect.isFalse (ticks |> List.exists (fun t -> (TimeZoneInfo.ConvertTime(t,zone)).Hour=2)) "missing 2 AM"
    testCase "empty and overflowing time ranges are bounded" <| fun _ ->
        Expect.throws (fun () -> TimeTicks.utc {Unit=TimeInterval.Second;Step=1} (DateTimeOffset.MinValue,DateTimeOffset.MaxValue) |> ignore) "bounded generation"
        let last=DateTimeOffset.MaxValue
        Expect.equal (TimeTicks.utc {Unit=TimeInterval.Year;Step=1} (last,last)) [] "no aligned endpoint"
    testCase "formatters never use ambient culture" <| fun _ ->
        Expect.equal (Format.percent CultureInfo.InvariantCulture 1 0.125) "12.5%" "percent"
        Expect.equal (Format.si CultureInfo.InvariantCulture 1 1500.) "1.5k" "SI"
]

namespace Ranvier.D5

open System

/// <summary>A tick interval of Step multiples of Unit; tick generation requires Step in [1,10000].</summary>
type TimeIntervalSpec = { Unit:TimeInterval; Step:int }

/// <summary>Calendar-aligned tick generation for instant domains, in UTC or an explicit timezone.</summary>
[<RequireQualifiedAccess>]
module TimeTicks =
    let private duration = function
        | TimeInterval.Second -> 1.
        | TimeInterval.Minute -> 60.
        | TimeInterval.Hour -> 3600.
        | TimeInterval.Day -> 86400.
        | TimeInterval.Week -> 604800.
        | TimeInterval.Month -> 2629800.
        | TimeInterval.Year -> 31557600.

    /// <summary>Chooses the standard interval, from one second to 1,000 years, whose length is closest on a log scale to the domain span divided by
    /// <c>count</c>. Raises ArgumentException unless <c>count</c> is in [1,10000].</summary>
    let choose count (first:DateTimeOffset,last:DateTimeOffset) =
        if count<1 || count>10000 then invalidArg "count" "Expected a tick count in [1,10000]."
        let target=max 1. (abs((last-first).TotalSeconds)/float count)
        [for unit,steps in [TimeInterval.Second,[1;5;15;30];TimeInterval.Minute,[1;5;15;30];TimeInterval.Hour,[1;3;6;12];TimeInterval.Day,[1;2];TimeInterval.Week,[1;2];TimeInterval.Month,[1;3;6];TimeInterval.Year,[1;2;5;10;20;50;100;200;500;1000]] do
            for step in steps -> {Unit=unit;Step=step}]
        |> List.minBy (fun spec -> abs(log(duration spec.Unit*float spec.Step/target)))

    let private floorTime spec (value:DateTime) =
        let at y m d h minute second=DateTime(y,m,d,h,minute,second,DateTimeKind.Unspecified)
        let fixedFloor ticks = DateTime(value.Ticks/(ticks*int64 spec.Step)*(ticks*int64 spec.Step),DateTimeKind.Unspecified)
        match spec.Unit with
        | TimeInterval.Second -> fixedFloor TimeSpan.TicksPerSecond
        | TimeInterval.Minute -> fixedFloor TimeSpan.TicksPerMinute
        | TimeInterval.Hour -> fixedFloor TimeSpan.TicksPerHour
        | TimeInterval.Day -> fixedFloor TimeSpan.TicksPerDay
        | TimeInterval.Week ->
            let days=(value.Date-DateTime(1,1,1)).Days
            DateTime(1,1,1).AddDays(float(days/(7*spec.Step)*(7*spec.Step)))
        | TimeInterval.Month ->
            let month=((value.Year-1)*12+value.Month-1)/spec.Step*spec.Step
            at (month/12+1) (month%12+1) 1 0 0 0
        | TimeInterval.Year -> at (max 1 (value.Year/spec.Step*spec.Step)) 1 1 0 0 0

    let private advance spec (value:DateTime) =
        match spec.Unit with
        | TimeInterval.Second -> value.AddSeconds(float spec.Step)
        | TimeInterval.Minute -> value.AddMinutes(float spec.Step)
        | TimeInterval.Hour -> value.AddHours(float spec.Step)
        | TimeInterval.Day -> value.AddDays(float spec.Step)
        | TimeInterval.Week -> value.AddDays(float spec.Step*7.)
        | TimeInterval.Month -> value.AddMonths spec.Step
        | TimeInterval.Year -> value.AddYears spec.Step

    /// <summary>Calendar ticks in an explicit timezone. Invalid local times are skipped; folds emit both offsets in instant order.</summary>
    let inZone (zone:TimeZoneInfo) spec (first:DateTimeOffset,last:DateTimeOffset) =
        if isNull zone then nullArg "zone"
        if spec.Step<1 || spec.Step>10000 then invalidArg "spec" "Step must be in [1,10000]."
        let lo,hi=min first last,max first last
        let localA,localB=TimeZoneInfo.ConvertTime(lo,zone).DateTime,TimeZoneInfo.ConvertTime(hi,zone).DateTime
        let mutable start,finish=min localA localB,max localA localB
        // Include a complete fold around either endpoint, then filter by actual instant.
        if Zone.isAmbiguousTime zone start then start<-start.Date
        if Zone.isAmbiguousTime zone finish then finish<-finish.Date.AddDays(1.).AddTicks(-1L)
        let result=ResizeArray<DateTimeOffset>()
        let mutable current=floorTime spec start
        let mutable finished=false
        let mutable iterations=0
        while current<=finish && not finished do
            iterations<-iterations+1
            if iterations>200000 || result.Count>10000 then invalidArg "spec" "Tick generation exceeds capacity."
            if not(Zone.isInvalidTime zone current) then
                let offsets=if Zone.isAmbiguousTime zone current then Zone.ambiguousOffsets zone current else [|Zone.utcOffset zone current|]
                for offset in offsets do
                    try
                        let instant=DateTimeOffset(current,offset).ToUniversalTime()
                        if instant>=lo && instant<=hi then result.Add instant
                    with :? ArgumentOutOfRangeException -> ()
            try current<-advance spec current
            with :? ArgumentOutOfRangeException -> finished<-true
        if result.Count>10000 then invalidArg "spec" "Tick generation exceeds capacity."
        let ordered=result |> Seq.distinct |> Seq.sort |> Seq.toList
        if first>last then List.rev ordered else ordered

    /// <summary>Calendar ticks in UTC; equivalent to <c>inZone TimeZoneInfo.Utc</c>.</summary>
    let utc spec domain = inZone TimeZoneInfo.Utc spec domain

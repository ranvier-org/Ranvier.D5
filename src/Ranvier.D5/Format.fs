namespace Ranvier.D5

open System
open System.Globalization

/// <summary>Culture- and zone-explicit formatting; nothing reads the machine culture or timezone.</summary>
[<RequireQualifiedAccess>]
module Format =
    let private digits precision =
        if precision<0 || precision>15 then invalidArg "precision" "Precision must be in [0,15]."
        "F"+string precision
    /// <summary>Formats a finite value with fixed-point precision of 0 to 15 decimal digits in the culture. Raises ArgumentNullException for a null
    /// culture and ArgumentException for a non-finite value or out-of-range precision.</summary>
    let number (culture:CultureInfo) precision (value:float) =
        if isNull culture then nullArg "culture"
        Check.finite "value" value
        value.ToString(digits precision,culture)
    /// <summary>Formats the value multiplied by 100 as <c>number</c> does, followed by the culture's percent symbol; fails as <c>number</c>
    /// does.</summary>
    let percent culture precision value = number culture precision (value*100.)+culture.NumberFormat.PercentSymbol
    /// <summary>Formats a finite value scaled by the largest SI prefix from y (10^-24) to Y (10^24) not exceeding its magnitude, as <c>number</c>
    /// does; zero has no prefix.</summary>
    let si culture precision value =
        Check.finite "value" value
        let prefixes=[|"y";"z";"a";"f";"p";"n";"µ";"m";"";"k";"M";"G";"T";"P";"E";"Z";"Y"|]
        let exponent=if value=0. then 0 else int(Math.Clamp(floor(Math.Log10(abs value)/3.),-8.,8.))
        number culture precision (value/Math.Pow(1000.,float exponent))+prefixes[exponent+8]
    /// <summary>Converts the instant to the zone and formats it with a .NET date and time pattern in the culture. Raises ArgumentNullException for a
    /// null culture or zone and FormatException for an invalid pattern.</summary>
    let time (culture:CultureInfo) (zone:TimeZoneInfo) pattern (instant:DateTimeOffset) =
        if isNull culture then nullArg "culture"
        if isNull zone then nullArg "zone"
        TimeZoneInfo.ConvertTime(instant,zone).ToString(pattern,culture)

/// <summary>How a wall-clock time inside a daylight-saving fold resolves.</summary>
[<RequireQualifiedAccess>]
type AmbiguousTime =
    /// <summary>The first occurrence, with the larger UTC offset.</summary>
    | Earlier
    /// <summary>The second occurrence, with the smaller UTC offset.</summary>
    | Later
    /// <summary>A typed error.</summary>
    | Reject

/// <summary>How a wall-clock time skipped by a daylight-saving gap resolves.</summary>
[<RequireQualifiedAccess>]
type InvalidTime =
    /// <summary>A typed error.</summary>
    | Reject
    /// <summary>Moves forward by the gap, as the clock reading would after the transition.</summary>
    | ShiftForward

/// <summary>Explicit policies for local times in folds and gaps.</summary>
type LocalTimePolicy = { Ambiguous:AmbiguousTime; Invalid:InvalidTime }

/// <summary>Strict, culture- and zone-explicit parsing; nothing reads the machine culture or timezone.</summary>
[<RequireQualifiedAccess>]
module Parse =
    let private styles=NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint ||| NumberStyles.AllowExponent

    /// <summary>Parses a finite number with an optional sign, decimal point and exponent; whitespace, group separators and non-finite words are rejected.</summary>
    let number (culture:CultureInfo) (text:string) : Result<float,LayoutError list> =
        if isNull culture then nullArg "culture"
        match Double.TryParse(text,styles,culture) with
        | true,value when Double.IsFinite value -> Ok value
        | _ -> Error [InvalidObservation(sprintf "'%s' is not a finite number." text)]

    /// <summary>Parses an exact pattern. A pattern with an offset (K, z, zz, zzz) keeps that offset; otherwise the wall-clock time is placed in the zone using the policy.</summary>
    let time (culture:CultureInfo) (zone:TimeZoneInfo) (pattern:string) (policy:LocalTimePolicy) (text:string) : Result<DateTimeOffset,LayoutError list> =
        if isNull culture then nullArg "culture"
        if isNull zone then nullArg "zone"
        let fail message=Error [InvalidObservation(sprintf "'%s' %s" text message)]
        if pattern.Contains 'K' || pattern.Contains 'z' then
            // AssumeUniversal keeps a quoted literal such as 'Z' from falling back to the machine offset.
            match DateTimeOffset.TryParseExact(text,pattern,culture,DateTimeStyles.AssumeUniversal) with
            | true,value -> Ok value
            | _ -> fail ("does not match the pattern "+pattern+".")
        else
            match DateTime.TryParseExact(text,pattern,culture,DateTimeStyles.None) with
            | false,_ -> fail ("does not match the pattern "+pattern+".")
            | true,parsed ->
                let local=DateTime.SpecifyKind(parsed,DateTimeKind.Unspecified)
                if zone.IsInvalidTime local then
                    match policy.Invalid with
                    | InvalidTime.Reject -> fail ("does not exist in "+zone.Id+".")
                    | InvalidTime.ShiftForward ->
                        // Read the instant with the offset in force before the gap, then show it in the zone.
                        let before=zone.GetUtcOffset(local.AddHours -6.)
                        let instant=DateTimeOffset(local.Ticks-before.Ticks,TimeSpan.Zero)
                        Ok(TimeZoneInfo.ConvertTime(instant,zone))
                elif zone.IsAmbiguousTime local then
                    let offsets=zone.GetAmbiguousTimeOffsets local
                    match policy.Ambiguous with
                    | AmbiguousTime.Reject -> fail ("is ambiguous in "+zone.Id+".")
                    | AmbiguousTime.Earlier -> Ok(DateTimeOffset(local,Array.max offsets))
                    | AmbiguousTime.Later -> Ok(DateTimeOffset(local,Array.min offsets))
                else Ok(DateTimeOffset(local,zone.GetUtcOffset local))

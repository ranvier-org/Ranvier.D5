namespace Ranvier.D5

open System
open System.Globalization

[<RequireQualifiedAccess>]
module Format =
    let private digits precision =
        if precision<0 || precision>15 then invalidArg "precision" "Precision must be in [0,15]."
        "F"+string precision
    let number (culture:CultureInfo) precision (value:float) =
        if isNull culture then nullArg "culture"
        Check.finite "value" value
        value.ToString(digits precision,culture)
    let percent culture precision value = number culture precision (value*100.)+culture.NumberFormat.PercentSymbol
    let si culture precision value =
        Check.finite "value" value
        let prefixes=[|"y";"z";"a";"f";"p";"n";"µ";"m";"";"k";"M";"G";"T";"P";"E";"Z";"Y"|]
        let exponent=if value=0. then 0 else int(Math.Clamp(floor(Math.Log10(abs value)/3.),-8.,8.))
        number culture precision (value/Math.Pow(1000.,float exponent))+prefixes[exponent+8]
    let time (culture:CultureInfo) (zone:TimeZoneInfo) pattern (instant:DateTimeOffset) =
        if isNull culture then nullArg "culture"
        if isNull zone then nullArg "zone"
        TimeZoneInfo.ConvertTime(instant,zone).ToString(pattern,culture)

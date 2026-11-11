namespace Ranvier.D5

open System

/// <summary>An immutable numeric mapping with a reversible domain transform.</summary>
type Continuous = private {
    Domain: float * float
    Range: float * float
    Forward: float -> float
    Backward: float -> float
    Accepts: float -> bool
    Clamped: bool
}

/// <summary>Immutable category positions and positive band width.</summary>
type Band<'Key when 'Key: comparison> = private { Positions: Map<'Key,float>; Width: float }

/// <summary>UTC calendar units used to align ticks.</summary>
[<RequireQualifiedAccess>]
type TimeInterval = Second | Minute | Hour | Day | Week | Month | Year

/// <summary>A UTC time mapping; inversion rounds to the nearest millisecond.</summary>
type TimeScale = private { Numeric: Continuous; First: DateTimeOffset; Last: DateTimeOffset }

/// <summary>Immutable scales. Constructors reject invalid configuration; tryMap omits invalid observations.</summary>
[<RequireQualifiedAccess>]
module Scale =
    let private create forward backward accepts domain range =
        Check.interval "domain" domain
        Check.interval "range" range
        let a,b = domain
        if not (accepts a && accepts b) then invalidArg "domain" "Domain is outside the transform."
        Check.interval "domain" (forward a, forward b)
        { Domain=domain; Range=range; Forward=forward; Backward=backward; Accepts=accepts; Clamped=false }

    /// <summary>Maps a finite numeric domain to a finite numeric range.</summary>
    let linear domain range = create id id (fun _ -> true) domain range

    /// <summary>Returns a copy that clamps mapping and inversion to the configured intervals.</summary>
    let clamp enabled scale = { scale with Clamped=enabled }

    let private normalize a b value = if a = b then 0.5 else (value-a)/(b-a)

    /// <summary>Returns None for non-finite or transform-invalid observations, including arithmetic overflow.</summary>
    let tryMap scale value =
        if not (Double.IsFinite value) || not (scale.Accepts value) then None
        else
            let a,b = scale.Domain
            let c,d = scale.Range
            let t = normalize (scale.Forward a) (scale.Forward b) (scale.Forward value)
            let result = Check.lerp c d (if scale.Clamped then Math.Clamp(t,0.,1.) else t)
            if Double.IsFinite result then Some result else None

    /// <summary>Maps an observation; rejects invalid observations with ArgumentException.</summary>
    let map scale value =
        tryMap scale value |> Option.defaultWith (fun () -> invalidArg "value" "Observation cannot be mapped to a finite value.")

    /// <summary>Inverts a range value; a constant range maps to the transformed domain midpoint.</summary>
    let invert scale value =
        Check.finite "value" value
        let a,b = scale.Domain
        let c,d = scale.Range
        let t = normalize c d value
        let result = scale.Backward (Check.lerp (scale.Forward a) (scale.Forward b) (if scale.Clamped then Math.Clamp(t,0.,1.) else t))
        Check.finite "value" result
        result

    /// <summary>Creates a logarithmic scale; domain endpoints must be nonzero and have the same sign.</summary>
    let log basis ((a,b) as domain) range =
        Check.interval "domain" domain
        Check.positive "basis" basis
        if basis = 1. then invalidArg "basis" "Logarithm base cannot be one."
        if a = 0. || b = 0. || Math.Sign a <> Math.Sign b then invalidArg "domain" "Log domain must have one nonzero sign."
        let sign = if a < 0. then -1. else 1.
        create (fun x -> sign * Math.Log(abs x, basis)) (fun x -> sign * Math.Pow(basis,sign*x))
            (fun x -> x*sign > 0.) domain range

    /// <summary>Creates a sign-preserving power scale with a positive exponent.</summary>
    let power exponent domain range =
        Check.positive "exponent" exponent
        let signedPow power x = if x < 0. then -Math.Pow(-x,power) else Math.Pow(x,power)
        create (signedPow exponent) (signedPow (1./exponent)) (fun _ -> true) domain range

    /// <summary>Creates a symmetric logarithmic scale with a positive linear-region constant.</summary>
    let symlog constant domain range =
        Check.positive "constant" constant
        let forward (x: float) = float (Math.Sign x) * Double.LogP1(abs x/constant)
        let backward (x: float) = float (Math.Sign x) * Double.ExpM1(abs x) * constant
        create forward backward (fun _ -> true) domain range

    /// <summary>Returns approximately count nice numeric ticks in domain order, bounded to 10,001 ticks.</summary>
    let ticks count scale =
        if count <= 0 then []
        elif count > 10000 then invalidArg "count" "Tick count cannot exceed 10,000."
        else
            let a,b = scale.Domain
            if a=b then [a]
            else
                let lo,hi = min a b,max a b
                let raw = (hi-lo)/float count
                if raw = 0. then [a;b]
                else
                    let unit = Math.Pow(10.,floor (Math.Log10 raw))
                    let error = raw/unit
                    let factor = if error >= sqrt 50. then 10. elif error >= sqrt 10. then 5. elif error >= sqrt 2. then 2. else 1.
                    let step = factor*unit
                    Check.positive "domain" step
                    let first,last = ceil (lo/step), floor (hi/step)
                    let number = last-first
                    if not (Double.IsFinite number) || number > 10000. then invalidArg "domain" "Tick range exceeds the bounded capacity."
                    let values = [for i in 0..int number -> (first+float i)*step]
                    if a>b then List.rev values else values

    /// <summary>Expands a numeric domain to nice tick boundaries while preserving direction.</summary>
    let nice count scale =
        if count <= 0 then scale
        else
            let values = ticks count scale
            match values with
            | first::second::_ ->
                let step = abs (second-first)
                let a,b = scale.Domain
                let lo = floor (min a b/step)*step
                let hi = ceil (max a b/step)*step
                let low = if scale.Accepts lo && Double.IsFinite(scale.Forward lo) then lo else min a b
                let high = if scale.Accepts hi && Double.IsFinite(scale.Forward hi) then hi else max a b
                let domain = if a <= b then low,high else high,low
                let candidate = create scale.Forward scale.Backward scale.Accepts domain scale.Range
                { candidate with Clamped = scale.Clamped }
            | _ -> scale

    let private categories domain =
        Check.unique "domain" id domain
        domain |> List.toArray

    /// <summary>Creates evenly spaced bands; inner padding is in [0,1], outer padding is nonnegative.</summary>
    let band domain (start,finish) inner outer =
        Check.interval "range" (start,finish)
        Check.finite "inner" inner
        Check.finite "outer" outer
        if inner<0. || inner>1. || outer<0. then invalidArg "padding" "Invalid band padding."
        let keys = categories domain
        let lo,hi = min start finish,max start finish
        let step = (hi-lo)/max 1. (float keys.Length-inner+2.*outer)
        let width = step*(1.-inner)
        let offset = lo + ((hi-lo)-step*(float keys.Length-inner))/2.
        let positions =
            keys |> Array.mapi (fun i key ->
                let index = if finish<start then keys.Length-1-i else i
                key, offset+float index*step) |> Map.ofArray
        { Positions=positions; Width=width }

    /// <summary>Creates centered point positions; padding is the number of half-steps outside the endpoints.</summary>
    let point domain range padding = band domain range 1. padding

    /// <summary>Returns the left edge of a category band, or None for an unknown category.</summary>
    let bandPosition band key = Map.tryFind key band.Positions

    /// <summary>Returns the positive width of a band; point scales have zero width.</summary>
    let bandwidth band = band.Width

    /// <summary>Creates a fixed ordinal domain, cycling the range; unknown categories return None.</summary>
    let ordinal domain range =
        let keys = categories domain
        let values = List.toArray range
        if values.Length=0 then invalidArg "range" "An ordinal range cannot be empty."
        let mapping = keys |> Array.mapi (fun i key -> key,values[i%values.Length]) |> Map.ofArray
        fun key -> Map.tryFind key mapping

    /// <summary>Creates right-inclusive threshold selection with exactly one more output than boundaries.</summary>
    let threshold boundaries range =
        let edges,values = List.toArray boundaries,List.toArray range
        if values.Length <> edges.Length+1 then invalidArg "range" "Expected one output per threshold interval."
        edges |> Array.iter (Check.finite "boundaries")
        for i in 1..edges.Length-1 do
            if edges[i]<=edges[i-1] then invalidArg "boundaries" "Thresholds must increase strictly."
        fun value -> if Double.IsFinite value then Some values[Data.bisectRight edges value] else None

    /// <summary>Divides an ascending domain into equal intervals; finite outliers clamp to the first or last output.</summary>
    let quantize (a,b) range =
        Check.interval "domain" (a,b)
        if a>=b then invalidArg "domain" "Quantize domain must increase."
        if List.isEmpty range then invalidArg "range" "Range cannot be empty."
        let n = List.length range
        threshold [for i in 1..n-1 -> Check.lerp a b (float i/float n)] range

    /// <summary>Divides finite samples at empirical quantiles; repeated quantiles select the rightmost interval.</summary>
    let quantile samples range =
        let values = List.toArray range
        if values.Length=0 then invalidArg "range" "Range cannot be empty."
        let sample = samples |> Seq.filter Double.IsFinite |> Seq.toArray
        if sample.Length=0 then invalidArg "samples" "At least one finite sample is required."
        let edges = [|for i in 1..values.Length-1 -> Data.quantile (float i/float values.Length) sample |> Option.get|]
        fun value -> if Double.IsFinite value then Some values[Data.bisectRight edges value] else None

    /// <summary>Maps a numeric domain through a caller-supplied interpolator; values extrapolate.</summary>
    let sequential domain interpolate =
        let scale = linear domain (0.,1.)
        fun value -> tryMap scale value |> Option.map interpolate

    /// <summary>Maps an ascending three-point domain to progress 0, 0.5 and 1 for a caller-supplied interpolator.</summary>
    let diverging (a,middle,b) interpolate =
        Check.interval "domain" (a,b)
        Check.finite "middle" middle
        if not (a<middle && middle<b) then invalidArg "domain" "Diverging domain must increase strictly."
        let lower,upper = linear (a,middle) (0.,0.5),linear (middle,b) (0.5,1.)
        fun value -> tryMap (if value<=middle then lower else upper) value |> Option.map interpolate

    /// <summary>Creates a UTC scale using millisecond precision and immutable endpoint values.</summary>
    let utcTime (first:DateTimeOffset,last:DateTimeOffset) range =
        { Numeric=linear (float(first.ToUnixTimeMilliseconds()),float(last.ToUnixTimeMilliseconds())) range
          First=first.ToUniversalTime(); Last=last.ToUniversalTime() }

    /// <summary>Maps an instant to the configured numeric range.</summary>
    let timeMap scale (value:DateTimeOffset) = map scale.Numeric (float(value.ToUnixTimeMilliseconds()))

    /// <summary>Inverts to a UTC instant, rounded to the nearest millisecond.</summary>
    let timeInvert scale value =
        let milliseconds = invert scale.Numeric value |> Math.Round
        if milliseconds < -62135596800000. || milliseconds > 253402300799999. then invalidArg "value" "Instant exceeds DateTimeOffset range."
        DateTimeOffset.FromUnixTimeMilliseconds(int64 milliseconds)

    /// <summary>Produces UTC calendar-aligned ticks, including exact endpoints and preserving domain direction.</summary>
    let timeTicks interval scale =
        let first,last = min scale.First scale.Last,max scale.First scale.Last
        let at y m d h minute second = DateTimeOffset(y,m,d,h,minute,second,TimeSpan.Zero)
        let floorTime =
            match interval with
            | TimeInterval.Second -> at first.Year first.Month first.Day first.Hour first.Minute first.Second
            | TimeInterval.Minute -> at first.Year first.Month first.Day first.Hour first.Minute 0
            | TimeInterval.Hour -> at first.Year first.Month first.Day first.Hour 0 0
            | TimeInterval.Day -> at first.Year first.Month first.Day 0 0 0
            | TimeInterval.Week -> (at first.Year first.Month first.Day 0 0 0).AddDays(-float ((int first.DayOfWeek+6)%7))
            | TimeInterval.Month -> at first.Year first.Month 1 0 0 0
            | TimeInterval.Year -> at first.Year 1 1 0 0 0
        let advance (value:DateTimeOffset) =
            match interval with
            | TimeInterval.Second -> value.AddSeconds 1.
            | TimeInterval.Minute -> value.AddMinutes 1.
            | TimeInterval.Hour -> value.AddHours 1.
            | TimeInterval.Day -> value.AddDays 1.
            | TimeInterval.Week -> value.AddDays 7.
            | TimeInterval.Month -> value.AddMonths 1
            | TimeInterval.Year -> value.AddYears 1
        let result = ResizeArray<_>()
        let mutable current = floorTime
        let mutable finished = false
        while current<=last && not finished do
            if current>=first then result.Add current
            if result.Count>10000 then invalidArg "interval" "Time tick count exceeds 10,000."
            try current <- advance current
            with :? ArgumentOutOfRangeException -> finished <- true
        let values = List.ofSeq result
        if scale.First>scale.Last then List.rev values else values


namespace Ranvier.D5

open System

/// <summary>A series observation with a nonempty unique ID, finite X, and a finite Y or <c>None</c> to mark a gap.</summary>
type SeriesSample = { Id:string; X:float; Y:float option }
/// <summary>Downsampling budgets: a nonnegative maximum output count and a positive maximum input count.</summary>
type SamplingConfig = { TargetCount:int; MaxInput:int }

/// <summary>Constructors for <c>SamplingConfig</c>.</summary>
[<RequireQualifiedAccess>]
module SamplingConfig =
    /// <summary>Creates a configuration with the given output count and an input budget of one million samples.</summary>
    let create targetCount = {TargetCount=targetCount;MaxInput=1000000}

/// <summary>Gap-aware series downsampling that keeps run endpoints and one sample per gap, returning typed <c>LayoutError</c> results.
/// Input with at most <c>TargetCount</c> samples is returned unchanged.</summary>
[<RequireQualifiedAccess>]
module Sampling =
    exception private SamplingAbort of LayoutError
    type private Segment = Run of SeriesSample array | Gap of SeriesSample
    type private AreaScore = Approximate of float | Exact of System.Numerics.BigInteger*int

    let private areaScore (a:Point) (b:Point) (c:Point) =
        let left,right=(a.X-c.X)*(b.Y-a.Y),(a.X-b.X)*(c.Y-a.Y)
        let area=abs(left-right)
        let bound=(abs left+abs right)*1e-14+16.*System.Double.Epsilon
        if Double.IsFinite area && Double.IsFinite bound && area>bound then Approximate area
        elif area=0. && Predicates.orient2d a b c=0 then Approximate 0.
        else
            let values,exponent=ExactDyadic.integers [|a.X;a.Y;b.X;b.Y;c.X;c.Y|]
            let determinant=(values[0]-values[4])*(values[3]-values[1])-(values[0]-values[2])*(values[5]-values[1])
            Exact(System.Numerics.BigInteger.Abs determinant,2*exponent)

    let private compareArea first second =
        match first,second with
        | Approximate a,Approximate b -> compare a b
        | _ ->
            let exact = function
                | Exact(value,exponent) -> value,exponent
                | Approximate value -> let values,exponent=ExactDyadic.integers [|value|] in values[0],exponent
            let a,ae=exact first
            let b,be=exact second
            let exponent=min ae be
            compare (a <<< (ae-exponent)) (b <<< (be-exponent))

    let private minMaxRun (token:System.Threading.CancellationToken) target (samples:SeriesSample array) =
        if target>=samples.Length then Array.toList samples
        else
            let output=ResizeArray<SeriesSample>()
            output.Add samples[0]
            let buckets=(target-2)/2
            let interior=samples.Length-2
            for bucket in 0..buckets-1 do
                token.ThrowIfCancellationRequested()
                let first=1+int(int64 bucket*int64 interior/int64 buckets)
                let last=1+int(int64(bucket+1)*int64 interior/int64 buckets)
                let mutable lowest,highest=first,first
                for i in first+1..last-1 do
                    if i%64=0 then token.ThrowIfCancellationRequested()
                    if samples[i].Y.Value<samples[lowest].Y.Value then lowest<-i
                    if samples[i].Y.Value>samples[highest].Y.Value then highest<-i
                output.Add samples[min lowest highest]
                if lowest<>highest then output.Add samples[max lowest highest]
            output.Add samples[samples.Length-1]
            List.ofSeq output

    let private lttbRun (token:System.Threading.CancellationToken) target (samples:SeriesSample array) =
        if target>=samples.Length then Array.toList samples
        elif target=2 then [samples[0];samples[samples.Length-1]]
        else
            let output=ResizeArray<SeriesSample>()
            output.Add samples[0]
            let buckets=target-2
            let interior=samples.Length-2
            let mutable anchor=0
            for bucket in 0..buckets-1 do
                token.ThrowIfCancellationRequested()
                let averageFirst=1+int(int64(bucket+1)*int64 interior/int64 buckets)
                let averageLast=min samples.Length (1+int(int64(bucket+2)*int64 interior/int64 buckets))
                let mean project=seq {for i in averageFirst..averageLast-1 -> project samples[i]} |> Data.mean |> Option.get
                let average=Point.create (mean(fun p -> p.X)) (mean(fun p -> p.Y.Value))
                let a=Point.create samples[anchor].X samples[anchor].Y.Value
                let first=1+int(int64 bucket*int64 interior/int64 buckets)
                let last=1+int(int64(bucket+1)*int64 interior/int64 buckets)
                let mutable selected=first
                let mutable largest=Approximate -1.
                for i in first..last-1 do
                    if i%64=0 then token.ThrowIfCancellationRequested()
                    let score=areaScore a (Point.create samples[i].X samples[i].Y.Value) average
                    if compareArea score largest>0 then selected<-i;largest<-score
                output.Add samples[selected]
                anchor<-selected
            output.Add samples[samples.Length-1]
            List.ofSeq output

    let private reduce (token:System.Threading.CancellationToken) minimum reduceRun (config:SamplingConfig) (samples:SeriesSample list) =
        token.ThrowIfCancellationRequested()
        if config.TargetCount<0 || config.MaxInput<=0 then Error [InvalidObservation "Sampling requires a nonnegative output budget and positive input budget."]
        elif samples.Length>config.MaxInput then Error [BudgetExceeded "Sampling input exceeds its budget."]
        else
          try
            let seen=System.Collections.Generic.HashSet<string>(StringComparer.Ordinal)
            let mutable previous=System.Double.NegativeInfinity
            let mutable index=0
            for sample in samples do
                if index%64=0 then token.ThrowIfCancellationRequested()
                index<-index+1
                if System.String.IsNullOrWhiteSpace sample.Id then raise(SamplingAbort(InvalidObservation "Sample IDs must not be empty."))
                if not(seen.Add sample.Id) then raise(SamplingAbort(DuplicateId sample.Id))
                if not(Double.IsFinite sample.X) || sample.Y |> Option.exists(Double.IsFinite >> not) then
                    raise(SamplingAbort(InvalidObservation "Sample coordinates must be finite; use None for missing Y."))
                if sample.X<previous then raise(SamplingAbort(InvalidObservation "Sampling requires nondecreasing X in original order."))
                previous<-sample.X
            if samples.Length<=config.TargetCount then Ok samples
            else
                let segments=ResizeArray<Segment>()
                let current=ResizeArray<SeriesSample>()
                let mutable missing=false
                let flush () =
                    if current.Count>0 then segments.Add(Run(current.ToArray()));current.Clear()
                index<-0
                for sample in samples do
                    if index%64=0 then token.ThrowIfCancellationRequested()
                    index<-index+1
                    match sample.Y with
                    | Some _ -> current.Add sample;missing<-false
                    | None ->
                        flush()
                        if not missing then segments.Add(Gap sample)
                        missing<-true
                flush()
                let budgets=segments |> Seq.map(function Run values -> min minimum values.Length | Gap _ -> 1) |> Seq.toArray
                let required=Array.sum budgets
                if required>config.TargetCount then Error [BudgetExceeded "Output budget cannot preserve every run endpoint, gap and required extrema."]
                else
                    let capacities=segments |> Seq.mapi(fun i segment -> match segment with Run values -> values.Length-budgets[i] | Gap _ -> 0) |> Seq.toArray
                    let capacity=Array.sum capacities
                    let available=min (config.TargetCount-required) capacity
                    if capacity>0 then
                        let remainders=ResizeArray<int64*int>()
                        let mutable assigned=0
                        for i in 0..segments.Count-1 do
                            if i%4096=0 then token.ThrowIfCancellationRequested()
                            let weighted=int64 available*int64 capacities[i]
                            let extra=int(weighted/int64 capacity)
                            budgets[i]<-budgets[i]+extra
                            assigned<-assigned+extra
                            if capacities[i]>extra then remainders.Add(weighted%int64 capacity,i)
                        let order=remainders |> Seq.sortBy(fun (remainder,i) -> -remainder,i) |> Seq.toArray
                        for i in 0..available-assigned-1 do
                            let _,segment=order[i]
                            budgets[segment]<-budgets[segment]+1
                    let output=ResizeArray<SeriesSample>()
                    for i in 0..segments.Count-1 do
                        token.ThrowIfCancellationRequested()
                        match segments[i] with
                        | Gap sample -> output.Add sample
                        | Run values -> output.AddRange(reduceRun token budgets[i] values)
                    Ok(List.ofSeq output)
          with
          | SamplingAbort error -> Error [error]
          | :? System.ArgumentException -> Error [InvalidObservation "Sampling geometry exceeds the finite numeric range."]

    /// <summary>Keeps each bucket's minimum and maximum Y in original order. Samples must have nondecreasing X;
    /// returns <c>BudgetExceeded</c> when the input exceeds <c>MaxInput</c> or the target cannot hold every run's endpoints and extrema.
    /// Cancellation throws <c>OperationCanceledException</c>.</summary>
    let minMaxWithCancellation token config samples = reduce token 4 minMaxRun config samples
    /// <summary>Runs <c>minMaxWithCancellation</c> without cancellation.</summary>
    let minMax config samples = minMaxWithCancellation System.Threading.CancellationToken.None config samples
    /// <summary>Largest-Triangle-Three-Buckets downsampling per run, with exact area ties resolved toward the earlier sample.
    /// Samples must have nondecreasing X; returns <c>BudgetExceeded</c> as for <c>minMax</c>.
    /// Cancellation throws <c>OperationCanceledException</c>.</summary>
    let lttbWithCancellation token config samples = reduce token 2 lttbRun config samples
    /// <summary>Runs <c>lttbWithCancellation</c> without cancellation.</summary>
    let lttb config samples = lttbWithCancellation System.Threading.CancellationToken.None config samples

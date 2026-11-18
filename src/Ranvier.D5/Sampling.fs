namespace Ranvier.D5

open System
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif

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
    type private Segment = Run of items:SeriesSample array * xs:float array * ys:float array | Gap of SeriesSample
    type private AreaScore = Approximate of float | Exact of System.Numerics.BigInteger*int

    let private areaScoreCoords (ax:float) (ay:float) (bx:float) (by:float) (cx:float) (cy:float) =
        let left,right=(ax-cx)*(by-ay),(ax-bx)*(cy-ay)
        let area=abs(left-right)
        let bound=(abs left+abs right)*1e-14+16.*System.Double.Epsilon
        if Double.IsFinite area && Double.IsFinite bound && area>bound then Approximate area
        elif area=0. && Predicates.orient2d (Point.create ax ay) (Point.create bx by) (Point.create cx cy)=0 then Approximate 0.
        else
            let values,exponent=ExactDyadic.integers [|ax;ay;bx;by;cx;cy|]
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

    let private minMaxRun (token:System.Threading.CancellationToken) target (samples:SeriesSample array) (_xs:float array) (ys:float array) =
        if target>=samples.Length then Array.toList samples
        else
            let buckets=(target-2)/2
            let interior=samples.Length-2
            let output=ResizeArray<SeriesSample>(2*buckets+2)
            output.Add samples[0]
            for bucket = 0 to buckets-1 do
                token.ThrowIfCancellationRequested()
                let first=1+int(int64 bucket*int64 interior/int64 buckets)
                let last=1+int(int64(bucket+1)*int64 interior/int64 buckets)
                let mutable lowest=first
                let mutable highest=first
                let mutable lowVal=ys[first]
                let mutable highVal=lowVal
                for i = first+1 to last-1 do
                    if i &&& 63 = 0 then token.ThrowIfCancellationRequested()
                    let y=ys[i]
                    if y<lowVal then lowest<-i;lowVal<-y
                    if y>highVal then highest<-i;highVal<-y
                if lowest<=highest then
                    output.Add samples[lowest]
                    if lowest<>highest then output.Add samples[highest]
                else
                    output.Add samples[highest]
                    output.Add samples[lowest]
            output.Add samples[samples.Length-1]
            Data.resizeToList output

    let private spanMean (arr:float array) (startIdx:int) (len:int) : float =
#if !FABLE_COMPILER
        let rawSum = SimdNumeric.sumSpan (ReadOnlySpan<float>(arr, startIdx, len))
        if Double.IsFinite rawSum then
            let m = rawSum / float len
            if Double.IsFinite m then m
            else
                let denom = float len
                let mutable total = 0.
                let mutable corr = 0.
                for i = startIdx to startIdx + len - 1 do
                    let v = arr[i] / denom
                    let next = total + v
                    corr <- corr + (if abs total >= abs v then (total - next) + v else (v - next) + total)
                    total <- next
                total + corr
        else
#endif
            let denom = float len
            let mutable total = 0.
            let mutable corr = 0.
            for i = startIdx to startIdx + len - 1 do
                let v = arr[i] / denom
                let next = total + v
                corr <- corr + (if abs total >= abs v then (total - next) + v else (v - next) + total)
                total <- next
            total + corr

    let private lttbRun (token:System.Threading.CancellationToken) target (samples:SeriesSample array) (xs:float array) (ys:float array) =
        if target>=samples.Length then Array.toList samples
        elif target=2 then [samples[0];samples[samples.Length-1]]
        else
            let buckets=target-2
            let interior=samples.Length-2
            let output=ResizeArray<SeriesSample>(target)
            output.Add samples[0]
            let mutable anchor=0
            for bucket = 0 to buckets-1 do
                token.ThrowIfCancellationRequested()
                let averageFirst=1+int(int64(bucket+1)*int64 interior/int64 buckets)
                let averageLast=min samples.Length (1+int(int64(bucket+2)*int64 interior/int64 buckets))
                let avgLen=averageLast-averageFirst
                let avgX=spanMean xs averageFirst avgLen
                let avgY=spanMean ys averageFirst avgLen
                Check.finite "averageX" avgX
                Check.finite "averageY" avgY
                let ax=xs[anchor]
                let ay=ys[anchor]
                let dxAc=ax-avgX
                let dyCa=avgY-ay
                let first=1+int(int64 bucket*int64 interior/int64 buckets)
                let last=1+int(int64(bucket+1)*int64 interior/int64 buckets)
                let mutable selected=first
                let mutable bestApprox= -1.
                let mutable hasExact=false
                let mutable largest=Approximate -1.
                let mutable i=first
#if !FABLE_COMPILER
                let rx = &MemoryMarshal.GetArrayDataReference xs
                let ry = &MemoryMarshal.GetArrayDataReference ys
                if Double.IsFinite dxAc && Double.IsFinite dyCa then
                    if Vector512.IsHardwareAccelerated && last - i >= 8 then
                        let vAx = Vector512.Create ax
                        let vAy = Vector512.Create ay
                        let vDxAc = Vector512.Create dxAc
                        let vDyCa = Vector512.Create dyCa
                        let vScale = Vector512.Create 1e-14
                        let vEps = Vector512.Create(16. * Double.Epsilon)
                        let vInf = Vector512.Create Double.PositiveInfinity
                        while not hasExact && i <= last - 8 do
                            if i &&& 63 = 0 then token.ThrowIfCancellationRequested()
                            let vx = Vector512.LoadUnsafe(&rx, unativeint i)
                            let vy = Vector512.LoadUnsafe(&ry, unativeint i)
                            let left = vDxAc * (vy - vAy)
                            let term2 = (vx - vAx) * vDyCa
                            let area = Vector512.Abs(left + term2)
                            let bound = Vector512.FusedMultiplyAdd(Vector512.Abs left + Vector512.Abs term2, vScale, vEps)
                            let valid =
                                Vector512.LessThan(area, vInf)
                                &&& Vector512.LessThan(bound, vInf)
                                &&& Vector512.GreaterThan(area, bound)
                            if valid = Vector512<float>.AllBitsSet then
                                for lane = 0 to 7 do
                                    let aVal = area.GetElement lane
                                    if aVal > bestApprox then
                                        bestApprox <- aVal
                                        selected <- i + lane
                                largest <- Approximate bestApprox
                                i <- i + 8
                            else
                                for lane = 0 to 7 do
                                    let idx = i + lane
                                    let score = areaScoreCoords ax ay xs[idx] ys[idx] avgX avgY
                                    if hasExact then
                                        if compareArea score largest > 0 then
                                            selected <- idx
                                            largest <- score
                                    else
                                        match score with
                                        | Approximate aVal ->
                                            if aVal > bestApprox then
                                                bestApprox <- aVal
                                                selected <- idx
                                                largest <- score
                                        | Exact _ ->
                                            hasExact <- true
                                            if compareArea score largest > 0 then
                                                selected <- idx
                                                largest <- score
                                i <- i + 8
                    elif Vector256.IsHardwareAccelerated && last - i >= 4 then
                        let vAx = Vector256.Create ax
                        let vAy = Vector256.Create ay
                        let vDxAc = Vector256.Create dxAc
                        let vDyCa = Vector256.Create dyCa
                        let vScale = Vector256.Create 1e-14
                        let vEps = Vector256.Create(16. * Double.Epsilon)
                        let vInf = Vector256.Create Double.PositiveInfinity
                        while not hasExact && i <= last - 4 do
                            if i &&& 63 = 0 then token.ThrowIfCancellationRequested()
                            let vx = Vector256.LoadUnsafe(&rx, unativeint i)
                            let vy = Vector256.LoadUnsafe(&ry, unativeint i)
                            let left = vDxAc * (vy - vAy)
                            let term2 = (vx - vAx) * vDyCa
                            let area = Vector256.Abs(left + term2)
                            let bound = Vector256.FusedMultiplyAdd(Vector256.Abs left + Vector256.Abs term2, vScale, vEps)
                            let valid =
                                Vector256.LessThan(area, vInf)
                                &&& Vector256.LessThan(bound, vInf)
                                &&& Vector256.GreaterThan(area, bound)
                            if valid = Vector256<float>.AllBitsSet then
                                for lane = 0 to 3 do
                                    let aVal = area.GetElement lane
                                    if aVal > bestApprox then
                                        bestApprox <- aVal
                                        selected <- i + lane
                                largest <- Approximate bestApprox
                                i <- i + 4
                            else
                                for lane = 0 to 3 do
                                    let idx = i + lane
                                    let score = areaScoreCoords ax ay xs[idx] ys[idx] avgX avgY
                                    if hasExact then
                                        if compareArea score largest > 0 then
                                            selected <- idx
                                            largest <- score
                                    else
                                        match score with
                                        | Approximate aVal ->
                                            if aVal > bestApprox then
                                                bestApprox <- aVal
                                                selected <- idx
                                                largest <- score
                                        | Exact _ ->
                                            hasExact <- true
                                            if compareArea score largest > 0 then
                                                selected <- idx
                                                largest <- score
                                i <- i + 4
#endif
                while i < last do
                    if i &&& 63 = 0 then token.ThrowIfCancellationRequested()
                    let bx = xs[i]
                    let by = ys[i]
                    let left = dxAc * (by - ay)
                    let term2 = (bx - ax) * dyCa
                    let area = abs (left + term2)
                    let bound = (abs left + abs term2) * 1e-14 + 16. * Double.Epsilon
                    if not hasExact && Double.IsFinite area && Double.IsFinite bound && area > bound then
                        if area > bestApprox then
                            bestApprox <- area
                            selected <- i
                            largest <- Approximate area
                    else
                        let score = areaScoreCoords ax ay bx by avgX avgY
                        match score with
                        | Approximate aVal when not hasExact ->
                            if aVal > bestApprox then
                                bestApprox <- aVal
                                selected <- i
                                largest <- score
                        | _ ->
                            hasExact <- true
                            if compareArea score largest > 0 then
                                selected <- i
                                largest <- score
                    i <- i + 1
                output.Add samples[selected]
                anchor <- selected
            output.Add samples[samples.Length-1]
            Data.resizeToList output

    let private reduce (token:System.Threading.CancellationToken) minimum reduceRun (config:SamplingConfig) (samples:SeriesSample list) =
        token.ThrowIfCancellationRequested()
        if config.TargetCount<0 || config.MaxInput<=0 then Error [InvalidObservation "Sampling requires a nonnegative output budget and positive input budget."]
        else
          try
            let mutable totalCount=0
            let mutable cur=samples
            while not cur.IsEmpty do
                if totalCount &&& 63 = 0 then token.ThrowIfCancellationRequested()
                totalCount<-totalCount+1
                if totalCount>config.MaxInput then raise(SamplingAbort(BudgetExceeded "Sampling input exceeds its budget."))
                cur<-cur.Tail
            let seen=System.Collections.Generic.HashSet<string>(totalCount, StringComparer.Ordinal)
            if totalCount<=config.TargetCount then
                let mutable previous=System.Double.NegativeInfinity
                let mutable index=0
                for sample in samples do
                    if index &&& 63 = 0 then token.ThrowIfCancellationRequested()
                    index<-index+1
                    if System.String.IsNullOrWhiteSpace sample.Id then raise(SamplingAbort(InvalidObservation "Sample IDs must not be empty."))
                    if not(seen.Add sample.Id) then raise(SamplingAbort(DuplicateId sample.Id))
                    if not(Double.IsFinite sample.X) || (match sample.Y with Some y -> not(Double.IsFinite y) | None -> false) then
                        raise(SamplingAbort(InvalidObservation "Sample coordinates must be finite; use None for missing Y."))
                    if sample.X<previous then raise(SamplingAbort(InvalidObservation "Sampling requires nondecreasing X in original order."))
                    previous<-sample.X
                Ok samples
            else
                // Validate and partition into contiguous SoA runs in a single pass
                let segments=ResizeArray<Segment>()
                let mutable bufItems=Array.zeroCreate<SeriesSample> totalCount
                let mutable bufXs=Array.zeroCreate<float> totalCount
                let mutable bufYs=Array.zeroCreate<float> totalCount
                let mutable runLen=0
                let mutable missing=false
                let mutable previous=System.Double.NegativeInfinity
                let mutable index=0
                let flushRun remainingCap =
                    if runLen>0 then
                        if runLen=bufItems.Length then
                            segments.Add(Run(bufItems,bufXs,bufYs))
                        else
                            segments.Add(Run(Array.sub bufItems 0 runLen,Array.sub bufXs 0 runLen,Array.sub bufYs 0 runLen))
                        runLen<-0
                        if remainingCap>0 then
                            bufItems<-Array.zeroCreate<SeriesSample> remainingCap
                            bufXs<-Array.zeroCreate<float> remainingCap
                            bufYs<-Array.zeroCreate<float> remainingCap
                for sample in samples do
                    if index &&& 63 = 0 then token.ThrowIfCancellationRequested()
                    index<-index+1
                    if System.String.IsNullOrWhiteSpace sample.Id then raise(SamplingAbort(InvalidObservation "Sample IDs must not be empty."))
                    if not(seen.Add sample.Id) then raise(SamplingAbort(DuplicateId sample.Id))
                    let x=sample.X
                    if not(Double.IsFinite x) then raise(SamplingAbort(InvalidObservation "Sample coordinates must be finite; use None for missing Y."))
                    if x<previous then raise(SamplingAbort(InvalidObservation "Sampling requires nondecreasing X in original order."))
                    previous<-x
                    match sample.Y with
                    | Some y ->
                        if not(Double.IsFinite y) then raise(SamplingAbort(InvalidObservation "Sample coordinates must be finite; use None for missing Y."))
                        bufItems[runLen]<-sample
                        bufXs[runLen]<-x
                        bufYs[runLen]<-y
                        runLen<-runLen+1
                        missing<-false
                    | None ->
                        flushRun (totalCount-index)
                        if not missing then segments.Add(Gap sample)
                        missing<-true
                flushRun 0
                if segments.Count=1 then
                    match segments[0] with
                    | Run(values,xs,ys) ->
                        if min minimum values.Length > config.TargetCount then
                            Error [BudgetExceeded "Output budget cannot preserve every run endpoint, gap and required extrema."]
                        else
                            Ok(reduceRun token (min config.TargetCount values.Length) values xs ys)
                    | Gap sample ->
                        if 1 > config.TargetCount then
                            Error [BudgetExceeded "Output budget cannot preserve every run endpoint, gap and required extrema."]
                        else Ok [sample]
                else
                    let segCount=segments.Count
                    let budgets=Array.zeroCreate<int> segCount
                    let mutable required=0
                    for i = 0 to segCount-1 do
                        let b = match segments[i] with Run(values,_,_) -> min minimum values.Length | Gap _ -> 1
                        budgets[i]<-b
                        required<-required+b
                    if required>config.TargetCount then Error [BudgetExceeded "Output budget cannot preserve every run endpoint, gap and required extrema."]
                    else
                        let capacities=Array.zeroCreate<int> segCount
                        let mutable capacity=0
                        for i = 0 to segCount-1 do
                            let cap = match segments[i] with Run(values,_,_) -> values.Length-budgets[i] | Gap _ -> 0
                            capacities[i]<-cap
                            capacity<-capacity+cap
                        let available=min (config.TargetCount-required) capacity
                        if capacity>0 then
                            let remainders=ResizeArray<struct(int64*int)>()
                            let mutable assigned=0
                            for i = 0 to segCount-1 do
                                if i &&& 4095 = 0 then token.ThrowIfCancellationRequested()
                                let weighted=int64 available*int64 capacities[i]
                                let extra=int(weighted/int64 capacity)
                                budgets[i]<-budgets[i]+extra
                                assigned<-assigned+extra
                                if capacities[i]>extra then remainders.Add(struct(weighted%int64 capacity,i))
                            let order=remainders.ToArray()
                            Array.Sort(order, System.Comparison(fun struct(ra,ia) struct(rb,ib) ->
                                let c = compare rb ra
                                if c <> 0 then c else compare ia ib))
                            for i = 0 to available-assigned-1 do
                                let struct(_,segment)=order[i]
                                budgets[segment]<-budgets[segment]+1
                        let output=ResizeArray<SeriesSample>(config.TargetCount)
                        for i = 0 to segCount-1 do
                            token.ThrowIfCancellationRequested()
                            match segments[i] with
                            | Gap sample -> output.Add sample
                            | Run(values,xs,ys) -> output.AddRange(reduceRun token budgets[i] values xs ys)
                        Ok(Data.resizeToList output)
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

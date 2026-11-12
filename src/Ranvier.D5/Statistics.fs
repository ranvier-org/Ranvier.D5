namespace Ranvier.D5

open System

[<RequireQualifiedAccess>]
type BinRule = Sturges | Scott | FreedmanDiaconis | Count of int

[<RequireQualifiedAccess>]
module Statistics =
    let private finite values = values |> Seq.filter Double.IsFinite |> Seq.toArray

    /// <summary>Sample variance of finite observations; missing/nonfinite observations are ignored, as in Data summaries. Unrepresentable results reject.</summary>
    let variance values =
        let data=finite values
        if data.Length<2 then None
        else
            let magnitude=data |> Array.map abs |> Array.max
            if magnitude=0. then Some 0.
            else
                let mutable mean=0.
                let mutable squared=0.
                for i in 0..data.Length-1 do
                    let value=data[i]/magnitude
                    let delta=value-mean
                    mean<-mean+delta/float(i+1)
                    squared<-squared+delta*(value-mean)
                let result=(squared/float(data.Length-1)*magnitude)*magnitude
                Check.finite "values" result
                Some result

    let cumulativeSum values =
        let mutable total=0.
        let mutable correction=0.
        [for value in values do
            if Double.IsFinite value then
                let next=total+value
                Check.finite "values" next
                correction<-correction+(if abs total>=abs value then (total-next)+value else (value-next)+total)
                total<-next
            let result=total+correction
            Check.finite "values" result
            yield result]

    let rollup key reduce values = values |> Seq.groupBy key |> Seq.map (fun (key,items) -> key,reduce items) |> Seq.toList

    /// <summary>Insertion before equal values in an ascending array.</summary>
    let bisectLeft value (values:float array) =
        Check.finite "value" value
        let mutable lo=0
        let mutable hi=values.Length
        while lo<hi do
            let mid=lo+(hi-lo)/2
            if values[mid]<value then lo<-mid+1 else hi<-mid
        lo

    /// <summary>Nearest ascending-array observation; exact distance ties choose the right observation.</summary>
    let bisectNearest value (values:float array) =
        let i=bisectLeft value values
        if values.Length=0 then None
        elif i=0 then Some 0
        elif i=values.Length then Some(i-1)
        else
            let magnitude=max (abs value) (max (abs values[i-1]) (abs values[i]))
            let left=if magnitude=0. then 0. else value/magnitude-values[i-1]/magnitude
            let right=if magnitude=0. then 0. else values[i]/magnitude-value/magnitude
            Some(if left<right then i-1 else i)

    /// <summary>Bounded equal-width bins. Zero spread yields one closed bin; zero IQR falls back to Sturges. Nonfinite observations are missing.</summary>
    let histogram rule values =
        let data=finite values
        match rule with BinRule.Count n when n<1 || n>10000 -> invalidArg "rule" "Bin count must be in [1,10000]." | _ -> ()
        if data.Length=0 then []
        else
            let lo,hi=Array.min data,Array.max data
            if lo=hi then [{Lower=lo;Upper=hi;Values=Array.toList data}]
            else
                let magnitude=max (abs lo) (abs hi)
                let scaled=data |> Array.map (fun x -> x/magnitude)
                let span=hi/magnitude-lo/magnitude
                let sturges()=ceil(Math.Log2(float data.Length))+1.
                let count =
                    match rule with
                    | BinRule.Count n -> float n
                    | BinRule.Sturges -> sturges()
                    | BinRule.Scott ->
                        let sd=variance scaled |> Option.map sqrt |> Option.defaultValue 0.
                        if sd=0. then sturges() else ceil(span/(3.49*sd*Math.Pow(float data.Length,-1./3.)))
                    | BinRule.FreedmanDiaconis ->
                        Array.Sort scaled
                        let q p =
                            let index=p*float(scaled.Length-1)
                            let lower=int(floor index)
                            Check.lerp scaled[lower] scaled[min (lower+1) (scaled.Length-1)] (index-float lower)
                        let iqr=q 0.75-q 0.25
                        if iqr=0. then sturges() else ceil(span/(2.*iqr*Math.Pow(float data.Length,-1./3.)))
                let count=int(min 10000. (max 1. count))
                let edges=[for i in 0..count -> Check.lerp lo hi (float i/float count)] |> List.distinct
                Data.bin id edges data

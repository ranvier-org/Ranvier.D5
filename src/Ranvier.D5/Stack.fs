namespace Ranvier.D5

open System

/// <summary>Series stacking order: input, reversed, by ascending or descending sum, by index of peak value, or inside-out by peak index.</summary>
[<RequireQualifiedAccess>]
type StackOrder = Input | Reverse | Ascending | Descending | Appearance | InsideOut
/// <summary>Stack baseline: zero, normalized to [0,1] per observation, centered, minimized wiggle, or separate positive and negative stacks.
/// Only <c>Diverging</c> accepts negative values.</summary>
[<RequireQualifiedAccess>]
type StackOffset = Zero | Expand | Silhouette | Wiggle | Diverging
/// <summary>Stack layout settings.</summary>
type StackConfig = { Order:StackOrder; Offset:StackOffset }
/// <summary>A named series of observations; all series in one stack must be the same length, with nonempty unique keys and finite values.</summary>
type Series = { Key:string; Values:float list }

/// <summary>d3-style stack layout returning typed <c>LayoutError</c> results instead of throwing.</summary>
[<RequireQualifiedAccess>]
module Stack =
    /// <summary>Returns layers in stack order. Negative observations require Diverging; all buffers stay private.</summary>
    let layout config (series:Series list) : Result<StackLayer<string> list,LayoutError list> =
        let errors=series |> List.countBy (fun s -> s.Key) |> List.choose (fun (key,n) -> if n>1 then Some(DuplicateId key) else None)
        let lengths=series |> List.map (fun s -> s.Values.Length) |> List.distinct
        let errors=if lengths.Length>1 then InvalidTopology "Series lengths differ."::errors else errors
        let invalid=series |> List.exists (fun s -> String.IsNullOrEmpty s.Key || s.Values |> List.exists (fun x -> not(Double.IsFinite x) || (x<0. && config.Offset<>StackOffset.Diverging)))
        let errors=if invalid then InvalidObservation "Invalid stack values or key."::errors else errors
        if not(List.isEmpty errors) then Error errors else
        try
            let sum s=Data.sum s.Values
            let appearance s=if s.Values.IsEmpty then 0 else s.Values |> List.indexed |> List.maxBy (fun (_,v) -> v) |> fst
            let ordered =
                match config.Order with
                | StackOrder.Input -> series
                | StackOrder.Reverse -> List.rev series
                | StackOrder.Ascending -> List.sortBy sum series
                | StackOrder.Descending -> List.sortBy (sum >> (~-)) series
                | StackOrder.Appearance -> List.sortBy appearance series
                | StackOrder.InsideOut ->
                    let mutable top,bottom=0.,0.
                    let mutable tops,bottoms=[],[]
                    for s in List.sortBy appearance series do
                        if top<bottom then top<-top+sum s;tops<-s::tops
                        else bottom<-bottom+sum s;bottoms<-s::bottoms
                    bottoms @ List.rev tops
            let n=ordered.Length
            let m=lengths |> List.tryHead |> Option.defaultValue 0
            let values=ordered |> List.map (fun s -> List.toArray s.Values) |> List.toArray
            let baseline=Array.zeroCreate m
            for j in 0..m-1 do
                let total=values |> Seq.map (fun row -> row[j]) |> Data.sum
                if config.Offset=StackOffset.Expand && total<>0. then
                    for row in values do row[j]<-row[j]/total
                if config.Offset=StackOffset.Silhouette then baseline[j]<- -total/2.
            if config.Offset=StackOffset.Wiggle then
                for j in 1..m-1 do
                    let mutable total,weighted,preceding=0.,0.,0.
                    for i in 0..n-1 do
                        let delta=values[i][j]-values[i][j-1]
                        weighted<-weighted+(preceding+delta/2.)*values[i][j]
                        total<-total+values[i][j]
                        preceding<-preceding+delta
                    baseline[j]<-baseline[j-1]-(if total=0. then 0. else weighted/total)
            let negative=Array.zeroCreate m
            let layers =
                ordered |> List.mapi (fun i s ->
                    let cells=[for j in 0..m-1 do
                                let value=values[i][j]
                                let lo,hi=
                                    if value<0. then
                                        let hi=negative[j]
                                        negative[j]<-hi+value
                                        negative[j],hi
                                    else
                                        let lo=baseline[j]
                                        baseline[j]<-lo+value
                                        lo,baseline[j]
                                Check.finite "stack" lo
                                Check.finite "stack" hi
                                yield lo,hi]
                    {StackLayer.Key=s.Key;Values=cells})
            Ok layers
        with :? ArgumentException as error -> Error [InvalidObservation error.Message]

namespace Ranvier.D5

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
type HierarchySumPolicy = IncludeOwnWeight | LeavesOnly
[<RequireQualifiedAccess>]
type TreemapTiling = Slice | Dice | SliceDice | Binary | Squarified
type TreemapConfig = { Bounds:Bounds; Padding:float; Tiling:TreemapTiling; Ratio:float; SumPolicy:HierarchySumPolicy }
[<RequireQualifiedAccess>]
module TreemapConfig =
    let create bounds = {Bounds=bounds;Padding=0.;Tiling=TreemapTiling.Squarified;Ratio=(1.+sqrt 5.)/2.;SumPolicy=HierarchySumPolicy.IncludeOwnWeight}

type private TileRow = { First:int; Last:int; Dice:bool }
type TreemapState<'Id> = private { Config:TreemapConfig; Topology:('Id * 'Id option * bool) list; Rows:TileRow list list }

module internal HierarchyWeights =
    let values policy (index:HierarchyIndex<'Id>) =
        let values=index.Items |> Array.map(fun item -> item.Value)
        if policy=HierarchySumPolicy.LeavesOnly then
            for i in index.Postorder do
                if index.Children[i].Length>0 then values[i]<-index.Children[i] |> Seq.map(fun child -> values[child]) |> Data.sum
        values

[<RequireQualifiedAccess>]
module Treemap =
    let private inset padding bounds =
        let x=min (padding/2.) (Bounds.width bounds/2.)
        let y=min (padding/2.) (Bounds.height bounds/2.)
        Bounds.create (Bounds.x bounds+x) (Bounds.y bounds+y) (max 0. (Bounds.width bounds-2.*x)) (max 0. (Bounds.height bounds-2.*y))

    let private stripe dice (weights:float[]) first last bounds (cells:Bounds[]) =
        let total=weights[first..last-1] |> Data.sum
        let mutable fraction=0.
        for i in first..last-1 do
            let next=if i=last-1 then 1. else min 1. (fraction+(if total=0. then 1./float(last-first) else weights[i]/total))
            if dice then
                let x=Check.lerp (Bounds.x bounds) (Bounds.x bounds+Bounds.width bounds) fraction
                let x1=Check.lerp (Bounds.x bounds) (Bounds.x bounds+Bounds.width bounds) next
                cells[i]<-Bounds.create x (Bounds.y bounds) (max 0. (x1-x)) (Bounds.height bounds)
            else
                let y=Check.lerp (Bounds.y bounds) (Bounds.y bounds+Bounds.height bounds) fraction
                let y1=Check.lerp (Bounds.y bounds) (Bounds.y bounds+Bounds.height bounds) next
                cells[i]<-Bounds.create (Bounds.x bounds) y (Bounds.width bounds) (max 0. (y1-y))
            fraction<-next

    let private binary (weights:float[]) bounds (cells:Bounds[]) =
        let prefix=Array.zeroCreate (weights.Length+1)
        for i in 0..weights.Length-1 do prefix[i+1]<-prefix[i]+weights[i]
        let pending=Stack<int*int*Bounds>()
        pending.Push(0,weights.Length,bounds)
        while pending.Count>0 do
            let first,last,box=pending.Pop()
            if last-first=1 then cells[first]<-box else
            let total=prefix[last]-prefix[first]
            let mutable lo=first+1
            let mutable hi=last-1
            let target=prefix[first]+total/2.
            while lo<hi do
                let middle=(lo+hi)/2
                if prefix[middle]<target then lo<-middle+1 else hi<-middle
            let split=if lo>first+1 && abs(prefix[lo-1]-target)<abs(prefix[lo]-target) then lo-1 else lo
            let fraction=if total=0. then float(split-first)/float(last-first) else (prefix[split]-prefix[first])/total
            let x,y,w,h=Bounds.x box,Bounds.y box,Bounds.width box,Bounds.height box
            if w>=h then
                let cut=Check.lerp x (x+w) fraction
                pending.Push(split,last,Bounds.create cut y (max 0. (x+w-cut)) h)
                pending.Push(first,split,Bounds.create x y (max 0. (cut-x)) h)
            else
                let cut=Check.lerp y (y+h) fraction
                pending.Push(split,last,Bounds.create x cut w (max 0. (y+h-cut)))
                pending.Push(first,split,Bounds.create x y w (max 0. (cut-y)))

    // Row selection follows d3-hierarchy 3.1.2 (ISC); retained rows are immutable values.
    let private squarify ratio (weights:float[]) bounds retained cells =
        let result=ResizeArray<TileRow>()
        let mutable remaining=bounds
        let mutable total=Data.sum weights
        let mutable first=0
        let saved=retained |> Option.map List.toArray
        while first<weights.Length do
            let w,h=Bounds.width remaining,Bounds.height remaining
            let mutable last=first+1
            let mutable sum=weights[first]
            let mutable dice=w<h
            match saved with
            | Some rows ->
                let row=rows[result.Count]
                last<-row.Last;dice<-row.Dice
                sum<-weights[first..last-1] |> Data.sum
            | None ->
                while sum=0. && last<weights.Length do sum<-weights[last];last<-last+1
                let mutable smallest=sum
                let mutable largest=sum
                let alpha=if w=0. || h=0. || total=0. then 0. else max (h/w) (w/h)/(total*ratio)
                let quality sum smallest largest =
                    let beta=sum*sum*alpha
                    if beta=0. || smallest=0. then Double.PositiveInfinity else max (largest/beta) (beta/smallest)
                let mutable best=quality sum smallest largest
                let mutable extending=true
                while extending && last<weights.Length do
                    let next=sum+weights[last]
                    let low,high=min smallest weights[last],max largest weights[last]
                    let score=quality next low high
                    if score>best then extending<-false else
                        sum<-next;smallest<-low;largest<-high;best<-score;last<-last+1
            let fraction=if last=weights.Length then 1. elif total=0. then float(last-first)/float(weights.Length-first) else min 1. (sum/total)
            let x,y=Bounds.x remaining,Bounds.y remaining
            let rowBox,nextBox=
                if dice then
                    let cut=Check.lerp y (y+h) fraction
                    Bounds.create x y w (max 0. (cut-y)),Bounds.create x cut w (max 0. (y+h-cut))
                else
                    let cut=Check.lerp x (x+w) fraction
                    Bounds.create x y (max 0. (cut-x)) h,Bounds.create cut y (max 0. (x+w-cut)) h
            stripe dice weights first last rowBox cells
            result.Add {First=first;Last=last;Dice=dice}
            first<-last;remaining<-nextBox;total<-max 0. (total-sum)
        List.ofSeq result

    let private compute config root state =
        HierarchyIndex.protect(fun () ->
            Check.finite "padding" config.Padding
            Check.finite "ratio" config.Ratio
            if config.Padding<0. then invalidArg "padding" "Padding must be nonnegative."
            if config.Ratio<1. then invalidArg "ratio" "Aspect ratio must be at least one."
            let index=HierarchyIndex.create root
            let values=HierarchyWeights.values config.SumPolicy index
            let topology=index.Items |> Array.mapi(fun i item -> item.Id,item.Parent,values[i]>(index.Children[i] |> Seq.map(fun child -> values[child]) |> Data.sum)) |> Array.toList
            let retained=state |> Option.filter(fun state -> state.Config=config && state.Topology=topology) |> Option.map(fun state -> List.toArray state.Rows)
            let boxes=Array.create index.Items.Length config.Bounds
            let rows=Array.create index.Items.Length []
            for i in 0..index.Items.Length-1 do
                boxes[i]<-inset config.Padding boxes[i]
                let children=index.Children[i]
                if children.Length>0 then
                    let childValues=children |> Array.map(fun child -> values[child])
                    let own=max 0. (values[i]-Data.sum childValues)
                    let weights=if own>0. then Array.append childValues [|own|] else childValues
                    if Array.forall ((=) 0.) weights then Array.fill weights 0 weights.Length 1.
                    let scale=Array.max weights
                    for j in 0..weights.Length-1 do weights[j]<-weights[j]/scale
                    let cells=Array.create weights.Length boxes[i]
                    match config.Tiling with
                    | TreemapTiling.Slice -> stripe false weights 0 weights.Length boxes[i] cells
                    | TreemapTiling.Dice -> stripe true weights 0 weights.Length boxes[i] cells
                    | TreemapTiling.SliceDice -> stripe (index.Items[i].Depth%2=1) weights 0 weights.Length boxes[i] cells
                    | TreemapTiling.Binary -> binary weights boxes[i] cells
                    | TreemapTiling.Squarified -> rows[i]<-squarify config.Ratio weights boxes[i] (retained |> Option.map(fun rows -> rows[i])) cells
                    for j in 0..children.Length-1 do boxes[children[j]]<-cells[j]
            let result=index.Items |> Array.mapi(fun i item -> {Id=item.Id;Parent=item.Parent;Depth=item.Depth;Bounds=boxes[i]}) |> Array.toList
            result,{Config=config;Topology=topology;Rows=Array.toList rows})

    let layout config root = compute config root None |> Result.map fst
    let resquarify config root state = compute {config with Tiling=TreemapTiling.Squarified} root state

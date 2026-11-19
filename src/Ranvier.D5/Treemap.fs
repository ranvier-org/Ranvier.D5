namespace Ranvier.D5

open System
open System.Collections.Generic

/// <summary>How a node's value is aggregated: <c>IncludeOwnWeight</c> adds a branch's own weight to its children's sum,
/// <c>LeavesOnly</c> sums leaves only.</summary>
[<RequireQualifiedAccess>]
type HierarchySumPolicy = IncludeOwnWeight | LeavesOnly
/// <summary>Treemap tiling: <c>Slice</c> stacks vertically, <c>Dice</c> horizontally, <c>SliceDice</c> alternates by depth,
/// <c>Binary</c> splits by weight halves, <c>Squarified</c> targets the configured aspect ratio, <c>Strip</c> forms ordered horizontal strips,
/// and <c>Hilbert</c> / <c>Moore</c> preserve child order along continuous space-filling curves.</summary>
[<RequireQualifiedAccess>]
type TreemapTiling = Slice | Dice | SliceDice | Binary | Squarified | Strip | Hilbert | Moore
/// <summary>Treemap settings; padding is in scene units and must be finite and nonnegative, and <c>Ratio</c> must be finite and at least 1.</summary>
type TreemapConfig = { Bounds:Bounds; Padding:float; Tiling:TreemapTiling; Ratio:float; SumPolicy:HierarchySumPolicy }
/// <summary>Constructors for <c>TreemapConfig</c>.</summary>
[<RequireQualifiedAccess>]
module TreemapConfig =
    /// <summary>Creates a squarified configuration with zero padding, golden-ratio aspect target and <c>IncludeOwnWeight</c> sums.</summary>
    let create bounds = {Bounds=bounds;Padding=0.;Tiling=TreemapTiling.Squarified;Ratio=(1.+sqrt 5.)/2.;SumPolicy=HierarchySumPolicy.IncludeOwnWeight}

type private TileRow = { First:int; Last:int; Dice:bool }
/// <summary>Opaque squarified row decisions retained by <c>Treemap.resquarify</c> for stable relayout of an unchanged topology.</summary>
type TreemapState<'Id> = private { Config:TreemapConfig; Topology:('Id * 'Id option * bool) list; Rows:TileRow list list }

module internal HierarchyWeights =
    let values policy (index:HierarchyIndex<'Id>) =
        let values=index.Items |> Array.map(fun item -> item.Value)
        if policy=HierarchySumPolicy.LeavesOnly then
            for i in index.Postorder do
                if index.Children[i].Length>0 then values[i]<-index.Children[i] |> Seq.map(fun child -> values[child]) |> Data.sum
        values

/// <summary>Treemap layouts of a hierarchy; invalid configuration or duplicate identities yield <c>Error [InvalidObservation]</c>.</summary>
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

    // Bederson, Shneiderman & Wattenberg (2002): ordered horizontal strips improving mean strip aspect ratio.
    let private strip (weights:float[]) bounds (cells:Bounds[]) =
        let result=ResizeArray<TileRow>()
        let mutable remaining=bounds
        let mutable total=Data.sum weights
        let mutable first=0
        let avgAspect lo hi rowSum remW remH remTotal =
            let frac=if hi=weights.Length then 1. elif remTotal=0. then float(hi-lo)/float(weights.Length-lo) else min 1. (rowSum/remTotal)
            let stripH=remH*frac
            if remW<=0. || stripH<=0. || rowSum<=0. then Double.PositiveInfinity
            else
                let mutable sumAr=0.
                let mutable posCount=0
                for k in lo..hi-1 do
                    let wk=weights[k]
                    if wk>0. then
                        let itemW=remW*(wk/rowSum)
                        sumAr<-sumAr+max (itemW/stripH) (stripH/itemW)
                        posCount<-posCount+1
                if posCount=0 then Double.PositiveInfinity else sumAr/float posCount
        while first<weights.Length do
            let w,h=Bounds.width remaining,Bounds.height remaining
            let mutable last=first+1
            let mutable sum=weights[first]
            while sum=0. && last<weights.Length do sum<-weights[last];last<-last+1
            let mutable best=avgAspect first last sum w h total
            let mutable extending=true
            while extending && last<weights.Length do
                let nextSum=sum+weights[last]
                let score=avgAspect first (last+1) nextSum w h total
                if score>best then extending<-false
                else sum<-nextSum;best<-score;last<-last+1
            let fraction=if last=weights.Length then 1. elif total=0. then float(last-first)/float(weights.Length-first) else min 1. (sum/total)
            let x,y=Bounds.x remaining,Bounds.y remaining
            let cut=Check.lerp y (y+h) fraction
            let rowBox=Bounds.create x y w (max 0. (cut-y))
            let nextBox=Bounds.create x cut w (max 0. (y+h-cut))
            stripe true weights first last rowBox cells
            result.Add {First=first;Last=last;Dice=true}
            first<-last;remaining<-nextBox;total<-max 0. (total-sum)
        List.ofSeq result

    // Tak & Cockburn (2013): spatially ordered 4-quadrant Hilbert and closed-loop Moore curve subdivision.
    let private spaceFilling isMoore (weights:float[]) bounds (cells:Bounds[]) =
        let nTotal=weights.Length
        if nTotal>0 then
            let prefix=Array.zeroCreate<float> (nTotal+1)
            for i=0 to nTotal-1 do prefix[i+1]<-prefix[i]+weights[i]
            let weightFrac lo mid hi =
                let span=prefix[hi]-prefix[lo]
                if span<=0. then float(mid-lo)/float(hi-lo) else Math.Clamp((prefix[mid]-prefix[lo])/span,0.,1.)
            let bestSplit lo hi minMid maxMid =
                let target=prefix[lo]+(prefix[hi]-prefix[lo])*0.5
                let mutable bestIdx=minMid
                let mutable bestDiff=abs(prefix[minMid]-target)
                for m=minMid+1 to maxMid do
                    let d=abs(prefix[m]-target)
                    if d<bestDiff then bestDiff<-d;bestIdx<-m
                bestIdx
            let splitX (b:Bounds) (firstSide:int) (fracFirst:float) =
                let x,y,w,h=Bounds.x b,Bounds.y b,Bounds.width b,Bounds.height b
                if firstSide=0 then
                    let cut=Check.lerp x (x+w) fracFirst
                    struct(Bounds.create x y (max 0. (cut-x)) h,Bounds.create cut y (max 0. (x+w-cut)) h)
                else
                    let cut=Check.lerp x (x+w) (1.-fracFirst)
                    struct(Bounds.create cut y (max 0. (x+w-cut)) h,Bounds.create x y (max 0. (cut-x)) h)
            let splitY (b:Bounds) (firstSide:int) (fracFirst:float) =
                let x,y,w,h=Bounds.x b,Bounds.y b,Bounds.width b,Bounds.height b
                if firstSide=0 then
                    let cut=Check.lerp y (y+h) fracFirst
                    struct(Bounds.create x y w (max 0. (cut-y)),Bounds.create x cut w (max 0. (y+h-cut)))
                else
                    let cut=Check.lerp y (y+h) (1.-fracFirst)
                    struct(Bounds.create x cut w (max 0. (y+h-cut)),Bounds.create x y w (max 0. (cut-y)))
            let rec subHilbert (sx:int) (sy:int) (ex:int) (ey:int) (first:int) (last:int) (box:Bounds) =
                let cnt=last-first
                if cnt=1 then cells[first]<-box
                elif cnt>1 then
                    let xAlong=(sx<>ex)
                    let sAlong=if xAlong then sx else sy
                    let sInto=if xAlong then sy else sx
                    let inline splitAlong b frac = if xAlong then splitX b sAlong frac else splitY b sAlong frac
                    let inline splitIntoS b frac = if xAlong then splitY b sInto frac else splitX b sInto frac
                    let inline splitIntoNotS b frac = if xAlong then splitY b (1-sInto) frac else splitX b (1-sInto) frac
                    if cnt=2 then
                        let struct(b0,b1)=splitAlong box (weightFrac first (first+1) last)
                        cells[first]<-b0;cells[first+1]<-b1
                    elif cnt=3 then
                        let s2=bestSplit first last (first+1) (first+2)
                        let struct(b01,b23)=splitAlong box (weightFrac first s2 last)
                        if s2=first+1 then
                            cells[first]<-b01
                            let struct(b2,b3)=splitIntoNotS b23 (weightFrac (first+1) (first+2) last)
                            cells[first+1]<-b2;cells[first+2]<-b3
                        else
                            let struct(b0,b1)=splitIntoS b01 (weightFrac first (first+1) (first+2))
                            cells[first]<-b0;cells[first+1]<-b1
                            cells[first+2]<-b23
                    else
                        let s2=bestSplit first last (first+2) (last-2)
                        let s1=bestSplit first s2 (first+1) (s2-1)
                        let s3=bestSplit s2 last (s2+1) (last-1)
                        let struct(b01,b23)=splitAlong box (weightFrac first s2 last)
                        let struct(b0,b1)=splitIntoS b01 (weightFrac first s1 s2)
                        let struct(b2,b3)=splitIntoNotS b23 (weightFrac s2 s3 last)
                        let sx',sy'=if xAlong then sx,1-sy else 1-sx,sy
                        let ex',ey'=if xAlong then ex,1-ey else 1-ex,ey
                        subHilbert sx sy sx' sy' first s1 b0
                        subHilbert sx sy ex ey s1 s2 b1
                        subHilbert sx sy ex ey s2 s3 b2
                        subHilbert ex' ey' ex ey s3 last b3
            if not isMoore then
                subHilbert 0 0 1 0 0 nTotal bounds
            else
                // Symmetric closed-loop Moore root: BL(0,1) -> TL(0,0) -> TR(1,0) -> BR(1,1)
                if nTotal=1 then cells[0]<-bounds
                elif nTotal=2 then
                    let struct(b0,b1)=splitX bounds 0 (weightFrac 0 1 2)
                    cells[0]<-b0;cells[1]<-b1
                elif nTotal=3 then
                    let s2=bestSplit 0 3 1 2
                    let struct(b01,b23)=splitX bounds 0 (weightFrac 0 s2 3)
                    if s2=1 then
                        cells[0]<-b01
                        let struct(b2,b3)=splitY b23 0 (weightFrac 1 2 3)
                        cells[1]<-b2;cells[2]<-b3
                    else
                        let struct(b0,b1)=splitY b01 1 (weightFrac 0 1 2)
                        cells[0]<-b0;cells[1]<-b1
                        cells[2]<-b23
                else
                    let s2=bestSplit 0 nTotal 2 (nTotal-2)
                    let s1=bestSplit 0 s2 1 (s2-1)
                    let s3=bestSplit s2 nTotal (s2+1) (nTotal-1)
                    let struct(b01,b23)=splitX bounds 0 (weightFrac 0 s2 nTotal)
                    let struct(b0,b1)=splitY b01 1 (weightFrac 0 s1 s2)
                    let struct(b2,b3)=splitY b23 0 (weightFrac s2 s3 nTotal)
                    subHilbert 1 1 1 0 0 s1 b0
                    subHilbert 1 1 1 0 s1 s2 b1
                    subHilbert 0 0 0 1 s2 s3 b2
                    subHilbert 0 0 0 1 s3 nTotal b3

    let private evaluateRows (weights:float[]) (box:Bounds) (rows:TileRow array) (outCells:Bounds[] voption) : float =
        let mutable remaining=box
        let mutable total=Data.sum weights
        let mutable worst=1.
        for r=0 to rows.Length-1 do
            let row=rows[r]
            let first,last,dice=row.First,row.Last,row.Dice
            let sum=weights[first..last-1] |> Data.sum
            let fraction=if last=weights.Length then 1. elif total=0. then float(last-first)/float(weights.Length-first) else min 1. (sum/total)
            let x,y=Bounds.x remaining,Bounds.y remaining
            let w,h=Bounds.width remaining,Bounds.height remaining
            let rowBox,nextBox=
                if dice then
                    let cut=Check.lerp y (y+h) fraction
                    Bounds.create x y w (max 0. (cut-y)),Bounds.create x cut w (max 0. (y+h-cut))
                else
                    let cut=Check.lerp x (x+w) fraction
                    Bounds.create x y (max 0. (cut-x)) h,Bounds.create cut y (max 0. (x+w-cut)) h
            match outCells with
            | ValueSome cells -> stripe dice weights first last rowBox cells
            | ValueNone -> ()
            let rw,rh=Bounds.width rowBox,Bounds.height rowBox
            for k=first to last-1 do
                let wk=weights[k]
                if wk>0. then
                    let fracItem=if sum=0. then 1./float(last-first) else wk/sum
                    let cw,ch=if dice then rw*fracItem,rh else rw,rh*fracItem
                    let ar=if cw<=0. || ch<=0. then Double.PositiveInfinity else max (cw/ch) (ch/cw)
                    if ar>worst then worst<-ar
            remaining<-nextBox
            total<-max 0. (total-sum)
        worst

    let private validRowPartition (n:int) (rows:TileRow list) =
        if rows.IsEmpty then false
        else
            let arr=List.toArray rows
            let mutable ok=(arr[0].First=0 && arr[arr.Length-1].Last=n)
            for r=0 to arr.Length-1 do
                if arr[r].First>=arr[r].Last then ok<-false
                if r>0 && arr[r].First<>arr[r-1].Last then ok<-false
            ok

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
                    | TreemapTiling.Strip -> rows[i]<-strip weights boxes[i] cells
                    | TreemapTiling.Hilbert -> spaceFilling false weights boxes[i] cells
                    | TreemapTiling.Moore -> spaceFilling true weights boxes[i] cells
                    for j in 0..children.Length-1 do boxes[children[j]]<-cells[j]
            let result=index.Items |> Array.mapi(fun i item -> {Id=item.Id;Parent=item.Parent;Depth=item.Depth;Bounds=boxes[i]}) |> Array.toList
            result,{Config=config;Topology=topology;Rows=Array.toList rows})

    /// <summary>Returns one rectangle per node in preorder, each inset by half the padding;
    /// all-zero siblings share their parent's space equally.</summary>
    let layout config root = compute config root None |> Result.map fst
    /// <summary>Lays out with squarified tiling, reusing the row decisions in <c>state</c> when the configuration and topology are unchanged.
    /// Returns the rectangles and the state to pass to the next call.</summary>
    let resquarify config root state = compute {config with Tiling=TreemapTiling.Squarified} root state

    /// <summary>Evaluates <c>previous</c> row topology with updated hierarchy weights and greedily applies up to <c>maxMoves</c>
    /// local row-boundary shifts, row splits/merges, or orientation flips per internal node that strictly improve worst aspect ratio (Sondag et al. 2018).</summary>
    let evolveLocalMoves (maxMoves:int) (config:TreemapConfig) (root:HierarchyNode<'Id>) (previous:TreemapState<'Id>) : Result<HierarchyBox<'Id> list * TreemapState<'Id>, LayoutError list> =
        HierarchyIndex.protect(fun () ->
            if maxMoves<0 then invalidArg "maxMoves" "maxMoves must be nonnegative."
            Check.finite "padding" config.Padding
            Check.finite "ratio" config.Ratio
            if config.Padding<0. then invalidArg "padding" "Padding must be nonnegative."
            if config.Ratio<1. then invalidArg "ratio" "Aspect ratio must be at least one."
            let sqConfig={config with Tiling=TreemapTiling.Squarified}
            let index=HierarchyIndex.create root
            let values=HierarchyWeights.values sqConfig.SumPolicy index
            let topology=index.Items |> Array.mapi(fun i item -> item.Id,item.Parent,values[i]>(index.Children[i] |> Seq.map(fun child -> values[child]) |> Data.sum)) |> Array.toList
            let prevRowsOpt =
                if previous.Topology=topology && previous.Rows.Length=index.Items.Length then
                    Some(List.toArray previous.Rows)
                else None
            let boxes=Array.create index.Items.Length sqConfig.Bounds
            let outRows=Array.create index.Items.Length []
            for i in 0..index.Items.Length-1 do
                boxes[i]<-inset sqConfig.Padding boxes[i]
                let children=index.Children[i]
                if children.Length>0 then
                    let childValues=children |> Array.map(fun child -> values[child])
                    let own=max 0. (values[i]-Data.sum childValues)
                    let weights=if own>0. then Array.append childValues [|own|] else childValues
                    if Array.forall ((=) 0.) weights then Array.fill weights 0 weights.Length 1.
                    let scale=Array.max weights
                    for j in 0..weights.Length-1 do weights[j]<-weights[j]/scale
                    let cells=Array.create weights.Length boxes[i]
                    let seedRows =
                        match prevRowsOpt with
                        | Some pRows when validRowPartition weights.Length pRows[i] -> List.toArray pRows[i]
                        | _ -> squarify sqConfig.Ratio weights boxes[i] None cells |> List.toArray
                    let mutable curRows=seedRows
                    let mutable curWorst=evaluateRows weights boxes[i] curRows ValueNone
                    let mutable moveStep=0
                    let mutable improving=true
                    while moveStep<maxMoves && improving do
                        let mutable bestCand=curRows
                        let mutable bestWorst=curWorst
                        let inline tryCandidate (cand:TileRow array) =
                            let wScore=evaluateRows weights boxes[i] cand ValueNone
                            if wScore<bestWorst*(1.-1e-12) then
                                bestWorst<-wScore
                                bestCand<-cand
                        let rLen=curRows.Length
                        // 1. Single-row orientation flips
                        for r=0 to rLen-1 do
                            let cand=Array.copy curRows
                            cand[r]<-{cand[r] with Dice=not cand[r].Dice}
                            tryCandidate cand
                        // 2. Adjacent row-boundary shifts (+1 / -1, including merge when a row shrinks to 0)
                        for r=0 to rLen-2 do
                            let leftRow=curRows[r]
                            let rightRow=curRows[r+1]
                            // Shift boundary right by 1 (leftRow absorbs rightRow.First)
                            if rightRow.Last-rightRow.First>1 then
                                let cand=Array.copy curRows
                                cand[r]<-{leftRow with Last=leftRow.Last+1}
                                cand[r+1]<-{rightRow with First=rightRow.First+1}
                                tryCandidate cand
                            else
                                // Merge rightRow into leftRow
                                let cand=Array.zeroCreate<TileRow> (rLen-1)
                                for k=0 to r-1 do cand[k]<-curRows[k]
                                cand[r]<-{leftRow with Last=rightRow.Last}
                                for k=r+2 to rLen-1 do cand[k-1]<-curRows[k]
                                tryCandidate cand
                            // Shift boundary left by 1 (rightRow absorbs leftRow.Last - 1)
                            if leftRow.Last-leftRow.First>1 then
                                let cand=Array.copy curRows
                                cand[r]<-{leftRow with Last=leftRow.Last-1}
                                cand[r+1]<-{rightRow with First=rightRow.First-1}
                                tryCandidate cand
                            else
                                // Merge leftRow into rightRow
                                let cand=Array.zeroCreate<TileRow> (rLen-1)
                                for k=0 to r-1 do cand[k]<-curRows[k]
                                cand[r]<-{rightRow with First=leftRow.First}
                                for k=r+2 to rLen-1 do cand[k-1]<-curRows[k]
                                tryCandidate cand
                        // 3. Row boundary split (peeling first or last item of a multi-item row into a new adjacent row)
                        for r=0 to rLen-1 do
                            let row=curRows[r]
                            let span=row.Last-row.First
                            if span>=2 then
                                for splitAt in [row.First+1; row.Last-1] do
                                    for d1 in [false; true] do
                                        for d2 in [false; true] do
                                            let cand=Array.zeroCreate<TileRow> (rLen+1)
                                            for k=0 to r-1 do cand[k]<-curRows[k]
                                            cand[r]<-{First=row.First;Last=splitAt;Dice=d1}
                                            cand[r+1]<-{First=splitAt;Last=row.Last;Dice=d2}
                                            for k=r+1 to rLen-1 do cand[k+1]<-curRows[k]
                                            tryCandidate cand
                        if bestWorst<curWorst*(1.-1e-12) then
                            curRows<-bestCand
                            curWorst<-bestWorst
                            moveStep<-moveStep+1
                        else
                            improving<-false
                    evaluateRows weights boxes[i] curRows (ValueSome cells) |> ignore
                    outRows[i]<-Array.toList curRows
                    for j in 0..children.Length-1 do boxes[children[j]]<-cells[j]
            let result=index.Items |> Array.mapi(fun i item -> {Id=item.Id;Parent=item.Parent;Depth=item.Depth;Bounds=boxes[i]}) |> Array.toList
            result,{Config=sqConfig;Topology=topology;Rows=Array.toList outRows})

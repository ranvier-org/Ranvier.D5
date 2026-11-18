namespace Ranvier.D5

open System

/// <summary>How matrix cells become chord arcs. Undirected sizes each group by its row sum; Directed adds incoming flow so each flow appears at both
/// ends; Transposed is Undirected over the transposed matrix.</summary>
[<RequireQualifiedAccess>]
type ChordMode = Undirected | Directed | Transposed
/// <summary>Sort order for groups, subgroups or flows by value; ties keep input order.</summary>
[<RequireQualifiedAccess>]
type ChordOrder = Input | Ascending | Descending
/// <summary>Chord layout settings. PadAngle is the gap between groups in radians, at most 2π divided by the group count; MaxCells bounds the matrix
/// cell count.</summary>
type ChordConfig =
    { Mode:ChordMode; PadAngle:float; GroupOrder:ChordOrder
      SubgroupOrder:ChordOrder; FlowOrder:ChordOrder; MaxCells:int }
/// <summary>Default chord configuration.</summary>
[<RequireQualifiedAccess>]
module ChordConfig =
    /// <summary>Undirected mode, no padding, input order throughout and a budget of 1,000,000 cells.</summary>
    let defaultValue={Mode=ChordMode.Undirected;PadAngle=0.;GroupOrder=ChordOrder.Input
                      SubgroupOrder=ChordOrder.Input;FlowOrder=ChordOrder.Input;MaxCells=1000000}
/// <summary>An angular span of a group or subgroup in radians, measured clockwise from twelve o'clock, with its group index and value.</summary>
type ChordArc = { Index:int; StartAngle:float; EndAngle:float; Value:float }
/// <summary>A ribbon between two subgroup arcs. Id is the (source, target) group pair; in undirected modes it lists the smaller index first and
/// Source is the side with the larger value.</summary>
type ChordFlow = { Id:struct(int*int); Source:ChordArc; Target:ChordArc }
/// <summary>Group arcs in matrix index order and flows in the configured flow order.</summary>
type ChordLayout = { Groups:ChordArc list; Flows:ChordFlow list }

/// <summary>Chord diagram layout from a square, nonnegative flow matrix.</summary>
[<RequireQualifiedAccess>]
module Chord =
    /// <summary>Lays out groups and flows around one full turn; an all-zero matrix yields evenly spaced empty groups. Returns Error for a non-square,
    /// negative or non-finite matrix, invalid padding or an exceeded cell budget; cancellation raises OperationCanceledException.</summary>
    let layoutWithCancellation (token:System.Threading.CancellationToken) (config:ChordConfig) (matrix:float list list) : Result<ChordLayout,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let n=matrix.Length
        let tau=2.*System.Math.PI
        if config.MaxCells<=0 then Error [InvalidObservation "MaxCells must be positive."]
        elif int64 n*int64 n>int64 config.MaxCells then Error [BudgetExceeded "Chord matrix exceeds MaxCells."]
        elif not(Double.IsFinite config.PadAngle) || config.PadAngle<0. || (n>0 && config.PadAngle>tau/float n) then
            Error [InvalidObservation "Chord padding must be finite, nonnegative and fit the circle."]
        elif matrix |> List.exists(fun row -> token.ThrowIfCancellationRequested();row.Length<>n) then Error [InvalidTopology "Chord matrix must be square."]
        elif matrix |> List.exists(fun row ->
            let mutable index=0
            row |> List.exists(fun value ->
                if index%64=0 then token.ThrowIfCancellationRequested()
                index<-index+1
                not(Double.IsFinite value) || value<0.)) then
            Error [InvalidObservation "Chord values must be finite and nonnegative."]
        elif n=0 then Ok {Groups=[];Flows=[]}
        else
            let inline check i=if (i &&& 63)=0 then token.ThrowIfCancellationRequested()
            let flat=Array.zeroCreate<float>(n*n)
            let transposed=config.Mode=ChordMode.Transposed
            let directed=config.Mode=ChordMode.Directed
            let mutable largest=0.
            let mutable rIdx=0
            for row in matrix do
                check rIdx
                let mutable cIdx=0
                let rowBase=rIdx*n
                for v in row do
                    if transposed then flat[cIdx*n+rIdx]<-v
                    else flat[rowBase+cIdx]<-v
                    if v>largest then largest<-v
                    cIdx<-cIdx+1
                rIdx<-rIdx+1
            let sums=Array.zeroCreate<float> n
            let mutable scaledTotal=0.
            for i=0 to n-1 do
                check i
                let rowBase=i*n
                let mutable rowSum=0.
                for j=0 to n-1 do
                    check j
                    let outgoing=flat[rowBase+j]
                    let incoming=if directed then flat[j*n+i] else 0.
                    rowSum<-rowSum+outgoing+incoming
                    if largest>0. then scaledTotal<-scaledTotal+outgoing/largest+incoming/largest
                sums[i]<-rowSum
            if sums |> Array.exists(fun sum -> not(Double.IsFinite sum)) then
                Error [InvalidObservation "A group flow total exceeds the finite numeric range."]
            else
                let sortArray (order:ChordOrder) (weight:'T -> 'W) (arr:'T array) =
                    if order=ChordOrder.Input || arr.Length<=1 then arr
                    else
                        let indexed=Array.init arr.Length (fun idx -> struct(idx,arr[idx]))
                        let mutable comparisons=0
                        Array.sortInPlaceWith(fun struct(ia,a) struct(ib,b) ->
                            check comparisons
                            comparisons<-comparisons+1
                            let comparison=
                                match order with
                                | ChordOrder.Input -> 0
                                | ChordOrder.Ascending -> compare (weight a) (weight b)
                                | ChordOrder.Descending -> compare (weight b) (weight a)
                            if comparison=0 then compare ia ib else comparison) indexed
                        Array.init indexed.Length (fun k -> let struct(_,v)=indexed[k] in v)
                let groups=Array.zeroCreate<ChordArc> n
                let cellCount=n*n
                let hasSource=Array.zeroCreate<bool> cellCount
                let sources=Array.zeroCreate<ChordArc> cellCount
                let hasTarget=Array.zeroCreate<bool> cellCount
                let targets=Array.zeroCreate<ChordArc> cellCount
                let space=max 0. (tau-config.PadAngle*float n)
                let gap=if scaledTotal=0. || space=0. then tau/float n else config.PadAngle
                let mutable angle=0.
                let groupIndices =
                    if config.GroupOrder=ChordOrder.Input then Array.init n id
                    else sortArray config.GroupOrder (fun i -> sums[i]) (Array.init n id)
                let entriesBuf=ResizeArray<struct(int*int*bool*float)>(2*n)
                for gIdx=0 to groupIndices.Length-1 do
                    let i=groupIndices[gIdx]
                    token.ThrowIfCancellationRequested()
                    let start=angle
                    entriesBuf.Clear()
                    let rowBase=i*n
                    if directed then
                        for j=n-1 downto 0 do
                            let v=flat[j*n+i]
                            if v>0. then entriesBuf.Add(struct(j,i,false,v))
                        for j=0 to n-1 do
                            let v=flat[rowBase+j]
                            if v>0. then entriesBuf.Add(struct(i,j,true,v))
                    else
                        for j=0 to n-1 do
                            let vij=flat[rowBase+j]
                            if vij>0. || flat[j*n+i]>0. then
                                entriesBuf.Add(struct(min i j,max i j,i<j,vij))
                    if config.SubgroupOrder=ChordOrder.Input then
                        for index=0 to entriesBuf.Count-1 do
                            check index
                            let struct(a,b,outgoing,v)=entriesBuf[index]
                            let next=if scaledTotal=0. then angle else angle+space*(v/largest)/scaledTotal
                            let arc={Index=i;StartAngle=angle;EndAngle=next;Value=v}
                            angle<-next
                            let slot=a*n+b
                            if outgoing then
                                hasSource[slot]<-true
                                sources[slot]<-arc
                            else
                                hasTarget[slot]<-true
                                targets[slot]<-arc
                            if not directed && a=b then
                                hasSource[slot]<-true
                                sources[slot]<-arc
                    else
                        let sortedEntries=sortArray config.SubgroupOrder (fun struct(_,_,outgoing,v) -> if directed && not outgoing then -v else v) (entriesBuf.ToArray())
                        for index=0 to sortedEntries.Length-1 do
                            check index
                            let struct(a,b,outgoing,v)=sortedEntries[index]
                            let next=if scaledTotal=0. then angle else angle+space*(v/largest)/scaledTotal
                            let arc={Index=i;StartAngle=angle;EndAngle=next;Value=v}
                            angle<-next
                            let slot=a*n+b
                            if outgoing then
                                hasSource[slot]<-true
                                sources[slot]<-arc
                            else
                                hasTarget[slot]<-true
                                targets[slot]<-arc
                            if not directed && a=b then
                                hasSource[slot]<-true
                                sources[slot]<-arc
                    groups[i]<-{Index=i;StartAngle=start;EndAngle=angle;Value=sums[i]}
                    angle<-angle+gap
                let flowBuf=ResizeArray<ChordFlow>()
                for slot=0 to cellCount-1 do
                    check slot
                    if hasSource[slot] && hasTarget[slot] then
                        let s0=sources[slot]
                        let t0=targets[slot]
                        let source,target=if not directed && s0.Value<t0.Value then t0,s0 else s0,t0
                        flowBuf.Add {Id=struct(slot/n,slot%n);Source=source;Target=target}
                let flows =
                    if config.FlowOrder=ChordOrder.Input then
                        let mutable fl=[]
                        for k=flowBuf.Count-1 downto 0 do
                            fl<-flowBuf[k]::fl
                        fl
                    else
                        let sortedFlows=
                            sortArray config.FlowOrder (fun flow ->
                                let total=flow.Source.Value+flow.Target.Value
                                if System.Double.IsPositiveInfinity total then struct(1,flow.Source.Value*0.5+flow.Target.Value*0.5)
                                else struct(0,total)) (flowBuf.ToArray())
                        let mutable fl=[]
                        for k=sortedFlows.Length-1 downto 0 do
                            fl<-sortedFlows[k]::fl
                        fl
                let mutable groupList=[]
                for k=n-1 downto 0 do
                    groupList<-groups[k]::groupList
                token.ThrowIfCancellationRequested()
                Ok {Groups=groupList;Flows=flows}
    /// <summary>Runs <c>layoutWithCancellation</c> without cancellation.</summary>
    let layout config matrix=layoutWithCancellation System.Threading.CancellationToken.None config matrix

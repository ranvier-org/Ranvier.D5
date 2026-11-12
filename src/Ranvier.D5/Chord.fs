namespace Ranvier.D5

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
        elif not(System.Double.IsFinite config.PadAngle) || config.PadAngle<0. || (n>0 && config.PadAngle>tau/float n) then
            Error [InvalidObservation "Chord padding must be finite, nonnegative and fit the circle."]
        elif matrix |> List.exists(fun row -> token.ThrowIfCancellationRequested();row.Length<>n) then Error [InvalidTopology "Chord matrix must be square."]
        elif matrix |> List.exists(fun row ->
            let mutable index=0
            row |> List.exists(fun value ->
                if index%64=0 then token.ThrowIfCancellationRequested()
                index<-index+1
                not(System.Double.IsFinite value) || value<0.)) then
            Error [InvalidObservation "Chord values must be finite and nonnegative."]
        elif n=0 then Ok {Groups=[];Flows=[]}
        else
            let check i=if i%64=0 then token.ThrowIfCancellationRequested()
            let rows=matrix |> List.mapi(fun i row -> check i;List.toArray row) |> List.toArray
            let value i j=if config.Mode=ChordMode.Transposed then rows[j][i] else rows[i][j]
            let directed=config.Mode=ChordMode.Directed
            let mutable largest=0.
            for i in 0..n-1 do
                check i
                for j in 0..n-1 do
                    check j
                    largest<-max largest (value i j)
            let sums=Array.zeroCreate<float> n
            let mutable scaledTotal=0.
            for i in 0..n-1 do
                check i
                for j in 0..n-1 do
                    check j
                    let outgoing=value i j
                    let incoming=if directed then value j i else 0.
                    sums[i]<-sums[i]+outgoing+incoming
                    if largest>0. then scaledTotal<-scaledTotal+outgoing/largest+incoming/largest
            if sums |> Array.exists(fun sum -> not(System.Double.IsFinite sum)) then
                Error [InvalidObservation "A group flow total exceeds the finite numeric range."]
            else
                let sort order weight items =
                    let mutable comparisons=0
                    if order=ChordOrder.Input then items else
                    items |> List.mapi(fun index item -> index,item)
                    |> List.sortWith(fun (ia,a) (ib,b) ->
                        check comparisons
                        comparisons<-comparisons+1
                        let comparison=
                            match order with
                            | ChordOrder.Input -> 0
                            | ChordOrder.Ascending -> compare (weight a) (weight b)
                            | ChordOrder.Descending -> compare (weight b) (weight a)
                        if comparison=0 then compare ia ib else comparison)
                    |> List.map snd
                let groups=Array.zeroCreate<ChordArc> n
                let sources=Array.create<ChordArc option> (n*n) None
                let targets=Array.create<ChordArc option> (n*n) None
                let space=max 0. (tau-config.PadAngle*float n)
                let gap=if scaledTotal=0. || space=0. then tau/float n else config.PadAngle
                let mutable angle=0.
                for i in sort config.GroupOrder (fun i -> sums[i]) [0..n-1] do
                    token.ThrowIfCancellationRequested()
                    let start=angle
                    let entries=
                        if directed then
                            [for j in n-1 .. -1 .. 0 do if value j i>0. then yield j,i,false,value j i
                             for j in 0..n-1 do if value i j>0. then yield i,j,true,value i j]
                        else
                            [for j in 0..n-1 do
                                if value i j>0. || value j i>0. then yield min i j,max i j,i<j,value i j]
                    let entries=entries |> sort config.SubgroupOrder (fun (_,_,outgoing,v) -> if directed && not outgoing then -v else v)
                    for index,(a,b,outgoing,v) in List.indexed entries do
                        check index
                        let next=if scaledTotal=0. then angle else angle+space*(v/largest)/scaledTotal
                        let arc={Index=i;StartAngle=angle;EndAngle=next;Value=v}
                        angle<-next
                        let slot=a*n+b
                        if outgoing then sources[slot]<-Some arc else targets[slot]<-Some arc
                        if not directed && a=b then sources[slot]<-Some arc
                    groups[i]<-{Index=i;StartAngle=start;EndAngle=angle;Value=sums[i]}
                    angle<-angle+gap
                let flows=
                    [for slot in 0..sources.Length-1 do
                        check slot
                        match sources[slot],targets[slot] with
                        | Some source,Some target ->
                            let source,target=if not directed && source.Value<target.Value then target,source else source,target
                            yield {Id=struct(slot/n,slot%n);Source=source;Target=target}
                        | _ -> ()]
                    |> sort config.FlowOrder (fun flow ->
                        let total=flow.Source.Value+flow.Target.Value
                        if System.Double.IsPositiveInfinity total then 1,flow.Source.Value/2.+flow.Target.Value/2.
                        else 0,total)
                token.ThrowIfCancellationRequested()
                Ok {Groups=Array.toList groups;Flows=flows}
    /// <summary>Runs <c>layoutWithCancellation</c> without cancellation.</summary>
    let layout config matrix=layoutWithCancellation System.Threading.CancellationToken.None config matrix

namespace Ranvier.D5.Charts

open System

open Ranvier.D5

/// <summary>Count, extent, mean and median of the non-missing values; each statistic is None when there are no values.</summary>
type ObservationSummary = { Count:int; Minimum:float option; Maximum:float option; Mean:float option; Median:float option }
/// <summary>One observation with a globally unique, nonblank ID; a None value is missing and excluded from density and membership.</summary>
type DistributionObservation = { Id:string; Value:float option }
/// <summary>A group of observations drawn as one violin; the group ID is its category.</summary>
type ViolinGroup = { Id:string; Observations:DistributionObservation list }
/// <summary>Violin width scaling: EachGroup scales every violin to its own peak density, SharedPeak scales all violins by the largest peak.</summary>
type ViolinWidth = EachGroup | SharedPeak
/// <summary>Violin specification: Domain and Bandwidth are in data units, Resolution (2 to 4096) is the KDE query count, and the Max fields are budgets whose excess is a BudgetExceeded error.</summary>
type ViolinSpec =
    { Frame:ChartFrame; Domain:float*float; Bandwidth:float; Resolution:int; Width:ViolinWidth
      Color:string -> Paint; Selected:Set<string>; MaxGroups:int; MaxSamples:int; MaxVertices:int; MaxWork:int64 }
/// <summary>The aggregate violin chart and an observation summary for every group, keyed by group ID.</summary>
type ViolinOutput = { Aggregate:AggregateChartOutput; Summaries:Map<string,ObservationSummary> }

/// <summary>Defaults for violin specifications.</summary>
[<RequireQualifiedAccess>]
module ViolinSpec =
    /// <summary>Creates a specification with 128 query samples, per-group width, ChartSpec.color fills, no selection and large default budgets.</summary>
    let create frame domain bandwidth =
        {Frame=frame;Domain=domain;Bandwidth=bandwidth;Resolution=128;Width=EachGroup;Color=ChartSpec.color;Selected=Set.empty
         MaxGroups=1000;MaxSamples=1000000;MaxVertices=1000000;MaxWork=100000000L}

module internal DistributionSummary =
    let ofValues values =
        let extent=Data.extent values
        {Count=List.length values;Minimum=Option.map fst extent;Maximum=Option.map snd extent;Mean=Data.mean values;Median=Data.quantile 0.5 values}

/// <summary>Kernel density violin charts.</summary>
[<RequireQualifiedAccess>]
module ViolinChart =
    exception private ViolinAbort of LayoutError list
    /// <summary>As build, but checks the token throughout and throws OperationCanceledException when it is cancelled.</summary>
    let buildWithCancellation (token:System.Threading.CancellationToken) (spec:ViolinSpec) (groups:ViolinGroup list) : Result<ViolinOutput,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let low,high=spec.Domain
        if not(Double.IsFinite low && Double.IsFinite high && low<high && Double.IsFinite spec.Bandwidth) ||
           spec.Bandwidth<=0. || spec.Resolution<2 || spec.Resolution>4096 ||
           spec.MaxGroups<1 || spec.MaxSamples<1 || spec.MaxVertices<1 || spec.MaxWork<1L then
            Error [InvalidObservation "Violin requires a finite increasing domain, positive bandwidth and budgets, and resolution 2..4096."]
        elif groups.Length>spec.MaxGroups then Error [BudgetExceeded "Violin groups exceed MaxGroups."]
        else
          try
            Recipe.run spec.Frame (fun (group:ViolinGroup) -> group.Id) groups (fun bounds ->
                let ids=System.Collections.Generic.HashSet<string>(StringComparer.Ordinal)
                let mutable count=0
                let rows=groups |> List.map(fun group ->
                    let values=group.Observations |> List.choose(fun sample ->
                        if count%64=0 then token.ThrowIfCancellationRequested()
                        count<-count+1
                        if count>spec.MaxSamples then raise(ViolinAbort [BudgetExceeded "Violin observations exceed MaxSamples."])
                        if System.String.IsNullOrWhiteSpace sample.Id || not(ids.Add sample.Id) then invalidArg "groups" "Observation IDs must be globally unique and nonblank."
                        match sample.Value with
                        | Some value when not(Double.IsFinite value) -> invalidArg "groups" "Violin observations must be finite or missing."
                        | value -> value)
                    group,values)
                let work=int64 count * (int64 spec.Resolution+int64(ceil(System.Math.Log2(float(max 2 count))))+4L)
                if work>spec.MaxWork then raise(ViolinAbort [BudgetExceeded "Violin KDE and summary work exceeds MaxWork."])
                let populated=rows |> List.filter(fun (_,values) -> not values.IsEmpty)
                if int64 populated.Length*(2L*int64 spec.Resolution+1L)>int64 spec.MaxVertices then
                    raise(ViolinAbort [BudgetExceeded "Violin query geometry exceeds MaxVertices."])
                let queryScale=Scale.linear (0.,float(spec.Resolution-1)) spec.Domain
                let queries=List.init spec.Resolution (float >> Scale.map queryScale)
                if queries |> List.pairwise |> List.exists(fun (a,b) -> a>=b) then invalidArg "Domain" "Distinct violin query samples cannot be represented at this resolution."
                let results=rows |> List.map(fun (group,values) ->
                    token.ThrowIfCancellationRequested()
                    let summary=DistributionSummary.ofValues values
                    let density=
                        if values.IsEmpty then []
                        else
                            Density.kde1dWithCancellation token spec.Bandwidth (values |> List.map(fun value -> value,1.)) queries
                            |> function Ok result -> result | Error errors -> raise(ViolinAbort errors)
                    group,summary,density,(density |> List.fold max 0.))
                let peak=results |> List.fold(fun current (_,_,_,value) -> max current value) 0.
                let bands=Scale.band (groups |> List.map(fun group -> group.Id)) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds) 0.25 0.15
                let y=Scale.linear spec.Domain (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
                let screenQueries=queries |> List.map(Scale.map y)
                if screenQueries |> List.pairwise |> List.exists(fun (a,b) -> a<=b) then invalidArg "Frame" "Distinct violin queries collapse in the viewport."
                let entries=results |> List.choose(fun (group,summary,density,groupPeak) ->
                    token.ThrowIfCancellationRequested()
                    if groupPeak=0. then None
                    else
                        let center=(Scale.bandPosition bands group.Id |> Option.get)+Scale.bandwidth bands/2.
                        let denominator=if spec.Width=EachGroup then groupPeak else peak
                        let samples=List.zip screenQueries density |> List.map(fun (screen,value) -> screen,(value/denominator)*(Scale.bandwidth bands/2.))
                        let left=samples |> List.map(fun (y,width) -> Point.create (center-width) y)
                        let right=samples |> List.rev |> List.map(fun (y,width) -> Point.create (center+width) y)
                        let polygon=left@right@[left.Head]
                        let anchorY=samples |> List.maxBy snd |> fst
                        if not(samples |> List.exists(fun (_,width) -> center-width<center && center+width>center)) then
                            invalidArg "frame" "Violin width collapses at this coordinate precision."
                        let key=Recipe.key ["violin";group.Id]
                        let label=sprintf "%s: %d observations, median %s" group.Id summary.Count (summary.Median |> Option.map Recipe.number |> Option.defaultValue "missing")
                        let path=Path.ofCommands(MoveTo polygon.Head::(polygon.Tail |> List.map LineTo)@[Close])
                        let mark=Mark.path key path |> Mark.withFill(spec.Color group.Id) |> Mark.withLabel(Some label)
                        let mark=if spec.Selected.Contains key then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                        let members=group.Observations |> List.choose(fun sample -> sample.Value |> Option.map(fun _ -> sample.Id)) |> List.sortWith (fun a b -> StringComparer.Ordinal.Compare(a,b))
                        let geometry=HitRegion([{Outer=polygon;Holes=[]}],[],Point.create center anchorY)
                        let caption=Mark.text (Recipe.key ["violin-label";group.Id]) (Point.create center (Bounds.y bounds+Bounds.height bounds+18.)) group.Id |> Mark.withFontSize 12.
                        let caption=Mark.withTextStyle {Mark.textStyle caption with Anchor="middle"} caption
                        Some(key,members,mark,Recipe.semantic key [key] label,Recipe.hit key [key] geometry,caption))
                token.ThrowIfCancellationRequested()
                let chart=Recipe.output spec.Frame bounds (entries |> List.map(fun (_,_,m,_,_,_) -> m))
                              (entries |> List.map(fun (_,_,_,s,_,_) -> s)) (entries |> List.map(fun (_,_,_,_,h,_) -> h)) (entries |> List.map(fun (_,_,_,_,_,label) -> label))
                let output={Aggregate={Chart=chart;Membership=entries |> List.map(fun (key,members,_,_,_,_) -> key,members) |> Map.ofList}
                            Summaries=results |> List.map(fun (group,summary,_,_) -> group.Id,summary) |> Map.ofList}
                token.ThrowIfCancellationRequested()
                output)
          with ViolinAbort errors -> Error errors
    /// <summary>Draws one KDE violin per group with values, keyed by a semantic mark key whose Membership lists the non-missing observation IDs; empty groups keep their band but draw nothing, and invalid input or exceeded budgets are typed errors.</summary>
    let build spec groups = buildWithCancellation System.Threading.CancellationToken.None spec groups

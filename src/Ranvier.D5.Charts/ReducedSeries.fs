namespace Ranvier.D5.Charts

open System

open Ranvier.D5

/// <summary>Downsampling algorithm for reduced series: MinMax keeps bucket extremes, Lttb uses largest-triangle-three-buckets.</summary>
[<RequireQualifiedAccess>]
type SeriesReduction = MinMax | Lttb
/// <summary>Reduced line chart specification for one series: Key names the series, Selected holds sample IDs to outline, and PointRadius is in pixels.</summary>
type ReducedSeriesSpec =
    { Frame:ChartFrame; Key:string; Sampling:SamplingConfig; Reduction:SeriesReduction
      XDomain:DomainPolicy; YDomain:DomainPolicy; Color:Paint; Selected:Set<string>; PointRadius:float }
/// <summary>The reduced chart with the full source samples, the visible reduced samples, and a summary of every source Y value.</summary>
type ReducedSeriesOutput = { Chart:ChartOutput; Source:SeriesSample list; Visible:SeriesSample list; Summary:ObservationSummary }

/// <summary>Defaults for reduced series specifications.</summary>
[<RequireQualifiedAccess>]
module ReducedSeriesSpec =
    /// <summary>Creates a MinMax specification targeting the given sample count, with key "series", automatic domains, 2 px points and no selection.</summary>
    let create frame target =
        {Frame=frame;Key="series";Sampling=SamplingConfig.create target;Reduction=SeriesReduction.MinMax
         XDomain=Auto;YDomain=Auto;Color=Paint.rgb 45 110 180;Selected=Set.empty;PointRadius=2.}

/// <summary>Line charts of downsampled series.</summary>
[<RequireQualifiedAccess>]
module ReducedSeriesChart =
    /// <summary>As build, but checks the token throughout and throws OperationCanceledException when it is cancelled.</summary>
    let buildWithCancellation (token:System.Threading.CancellationToken) (spec:ReducedSeriesSpec) (source:SeriesSample list) : Result<ReducedSeriesOutput,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let reduce=match spec.Reduction with SeriesReduction.MinMax -> Sampling.minMaxWithCancellation | SeriesReduction.Lttb -> Sampling.lttbWithCancellation
        reduce token spec.Sampling source
        |> Result.bind(fun visible ->
            Recipe.run spec.Frame id [] (fun bounds ->
                if System.String.IsNullOrWhiteSpace spec.Key then invalidArg "Key" "Series key must be nonblank."
                let pointArea=System.Math.PI*spec.PointRadius*spec.PointRadius
                if not(Double.IsFinite spec.PointRadius && Double.IsFinite pointArea) || spec.PointRadius<=0. || pointArea<=0. then
                    invalidArg "PointRadius" "Point radius must have a finite positive representable area."
                let valBuf=ResizeArray<float>()
                let mutable firstSample:SeriesSample voption=ValueNone
                let mutable lastSample:SeriesSample voption=ValueNone
                for sample in source do
                    if firstSample.IsNone then firstSample<-ValueSome sample
                    lastSample<-ValueSome sample
                    match sample.Y with
                    | Some yv -> valBuf.Add yv
                    | None -> ()
                let valuesArr=valBuf.ToArray()
                let summary=DistributionSummary.ofArray valuesArr
                token.ThrowIfCancellationRequested()
                let xExtent =
                    match firstSample,lastSample with
                    | ValueSome f,ValueSome l -> [Some f.X;Some l.X]
                    | _ -> []
                let x=Scale.linear (Recipe.domain spec.XDomain xExtent) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
                let y=Scale.linear (Recipe.domain spec.YDomain [summary.Minimum;summary.Maximum]) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
                let visArr=List.toArray visible
                let mutable linePts=[]
                let circleSymbol=Shape.symbol Symbol.Circle pointArea
                let hasSelected=not spec.Selected.IsEmpty
                let mutable marks=[]
                let mutable semantics=[]
                let mutable hits=[]
                for index=visArr.Length-1 downto 0 do
                    if (index &&& 63)=0 then token.ThrowIfCancellationRequested()
                    let sample=visArr[index]
                    match sample.Y with
                    | None -> linePts<-None::linePts
                    | Some yVal ->
                        let pt=Point.create (Scale.map x sample.X) (Scale.map y yVal)
                        linePts<-Some pt::linePts
                        let id=Recipe.key3 "sample" spec.Key sample.Id
                        let text=String.Concat(sample.Id,": ",Recipe.number sample.X,", ",Recipe.number yVal)
                        let mark=Mark.path id circleSymbol
                                 |> Mark.withTransform(Transform.translate pt.X pt.Y) |> Mark.withFill spec.Color |> Mark.withLabel(Some text)
                        let mark=if hasSelected && spec.Selected.Contains sample.Id then Mark.withStroke (Paint.rgb 230 140 20) 2. mark else mark
                        let ids=[sample.Id]
                        marks<-mark::marks
                        semantics<-Recipe.semantic id ids text::semantics
                        hits<-Recipe.hit id ids (HitPoint(pt,spec.PointRadius))::hits
                let key=Recipe.key2 "series" spec.Key
                let label=sprintf "%s: %d original measurements, %d visible samples" spec.Key summary.Count visArr.Length
                let line=Shape.line Curve.Linear linePts |> Mark.path key |> Mark.withFill Paint.none |> Mark.withStroke spec.Color 1.5 |> Mark.withLabel(Some label)
                let chart=Recipe.output spec.Frame bounds (line::marks) (Recipe.semantic key [] label::semantics) hits (Recipe.axes bounds x y)
                let output={Chart=chart;Source=source;Visible=visible;Summary=summary}
                token.ThrowIfCancellationRequested()
                output))
    /// <summary>Reduces the source and draws a line through the visible samples with a hit point per sample with a Y; domains span the full source so reduction does not move the axes, and sampling errors, a blank Key or an invalid PointRadius are typed errors.</summary>
    let build spec source = buildWithCancellation System.Threading.CancellationToken.None spec source

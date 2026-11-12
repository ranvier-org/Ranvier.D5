namespace Ranvier.D5.Charts

open Ranvier.D5

[<RequireQualifiedAccess>]
type SeriesReduction = MinMax | Lttb
type ReducedSeriesSpec =
    { Frame:ChartFrame; Key:string; Sampling:SamplingConfig; Reduction:SeriesReduction
      XDomain:DomainPolicy; YDomain:DomainPolicy; Color:Paint; Selected:Set<string>; PointRadius:float }
type ReducedSeriesOutput = { Chart:ChartOutput; Source:SeriesSample list; Visible:SeriesSample list; Summary:ObservationSummary }

[<RequireQualifiedAccess>]
module ReducedSeriesSpec =
    let create frame target =
        {Frame=frame;Key="series";Sampling=SamplingConfig.create target;Reduction=SeriesReduction.MinMax
         XDomain=Auto;YDomain=Auto;Color=Paint.rgb 45 110 180;Selected=Set.empty;PointRadius=2.}

[<RequireQualifiedAccess>]
module ReducedSeriesChart =
    let buildWithCancellation (token:System.Threading.CancellationToken) (spec:ReducedSeriesSpec) (source:SeriesSample list) : Result<ReducedSeriesOutput,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let reduce=match spec.Reduction with SeriesReduction.MinMax -> Sampling.minMaxWithCancellation | SeriesReduction.Lttb -> Sampling.lttbWithCancellation
        reduce token spec.Sampling source
        |> Result.bind(fun visible ->
            Recipe.run spec.Frame id [] (fun bounds ->
                if System.String.IsNullOrWhiteSpace spec.Key then invalidArg "Key" "Series key must be nonblank."
                let pointArea=System.Math.PI*spec.PointRadius*spec.PointRadius
                if not(System.Double.IsFinite spec.PointRadius && System.Double.IsFinite pointArea) || spec.PointRadius<=0. || pointArea<=0. then
                    invalidArg "PointRadius" "Point radius must have a finite positive representable area."
                let values=source |> List.choose(fun sample -> sample.Y)
                let summary=DistributionSummary.ofValues values
                token.ThrowIfCancellationRequested()
                let xExtent=match source with [] -> [] | first::_ -> [Some first.X;Some (List.last source).X]
                let x=Scale.linear (Recipe.domain spec.XDomain xExtent) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
                let y=Scale.linear (Recipe.domain spec.YDomain [summary.Minimum;summary.Maximum]) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
                let projected=visible |> List.mapi(fun index sample ->
                    if index%64=0 then token.ThrowIfCancellationRequested()
                    sample,sample.Y |> Option.map(fun value -> Point.create (Scale.map x sample.X) (Scale.map y value)))
                let key=Recipe.key ["series";spec.Key]
                let label=sprintf "%s: %d original measurements, %d visible samples" spec.Key summary.Count visible.Length
                let line=Shape.line Curve.Linear (List.map snd projected) |> Mark.path key |> Mark.withFill Paint.none |> Mark.withStroke spec.Color 1.5 |> Mark.withLabel(Some label)
                let points=projected |> List.choose(fun (sample,point) ->
                    token.ThrowIfCancellationRequested()
                    point |> Option.map(fun point ->
                        let id=Recipe.key ["sample";spec.Key;sample.Id]
                        let text=sample.Id+": "+Recipe.number sample.X+", "+(sample.Y |> Option.map Recipe.number |> Option.defaultValue "missing")
                        let mark=Mark.path id (Shape.symbol Symbol.Circle pointArea)
                                 |> Mark.withTransform(Transform.translate point.X point.Y) |> Mark.withFill spec.Color |> Mark.withLabel(Some text)
                        let mark=if spec.Selected.Contains sample.Id then Mark.withStroke (Paint.rgb 230 140 20) 2. mark else mark
                        mark,Recipe.semantic id [sample.Id] text,Recipe.hit id [sample.Id] (HitPoint(point,spec.PointRadius))))
                let marks,semantics,hits=List.unzip3 points
                let chart=Recipe.output spec.Frame bounds (line::marks) (Recipe.semantic key [] label::semantics) hits (Recipe.axes bounds x y)
                let output={Chart=chart;Source=source;Visible=visible;Summary=summary}
                token.ThrowIfCancellationRequested()
                output))
    let build spec source = buildWithCancellation System.Threading.CancellationToken.None spec source

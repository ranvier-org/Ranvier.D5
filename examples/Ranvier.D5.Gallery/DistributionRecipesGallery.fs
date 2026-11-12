module Ranvier.D5.Gallery.DistributionRecipesGallery

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

type DistributionView =
    { Parameter:float; Chart:ChartOutput; SelectionDomain:Set<string>; SourceCount:int; MeasuredCount:int; VisibleCount:int; GapCount:int; Detail:string }

let mount kind (host:WebCoreHost) (graph:Graph) =
    if not(List.contains kind ["violin";"minmax";"lttb"]) then invalidArg "kind" "Unknown distribution recipe."
    let random=Ranvier.Anime.Utils.createSeededRandom (Some 91.) (Some 0.) (Some 1.) (Some 12)
    let groups : ViolinGroup list =
        [for name in ["Compact";"Wide";"Two modes"] do
            let observations=
                [for i in 0..999 do
                    let a,b,c=random None None None,random None None None,random None None None
                    let value=if name="Compact" then 0.5*(a+b+c-1.5) elif name="Wide" then 2.*(a+b+c-1.5) else (if i%2=0 then -1.4 else 1.4)+0.4*(a+b-1.)
                    yield {DistributionObservation.Id=name+string i;Value=Some value}]
            yield {Id=name;Observations=observations}]
    let series=[for i in 0..99999 ->
                    {SeriesSample.Id=string i;X=float i/1000.
                     Y=if i%20000>=9000 && i%20000<10000 then None else Some(sin(float i*0.001)+0.25*sin(float i*0.013)+(if i%997=0 then 1.5 else 0.))}]
    let seriesDomain=lazy (series |> List.choose(fun sample -> sample.Y |> Option.map(fun _ -> sample.Id)) |> Set.ofList)
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let frame={ChartFrame.create 800. 570. ("Reactive "+kind) with Margin={Top=80.;Right=24.;Bottom=60.;Left=48.}}
        let bounds=ChartLayout.plot frame |> Result.defaultWith(fun e -> failwithf "%A" e)
        let empty={Scene=Scene.create 800. 570. frame.Title [];PlotBounds=bounds;Semantics=[];HitTargets=[];Description=frame.Title}
        let parameter=Api.createSignal (if kind="violin" then 0.25 else 500.)
        let selected=Api.createSignal Set.empty
        let worker=ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> parameter.Value) (fun token value ->
            if kind="violin" then
                let spec={ViolinSpec.create frame (-4.,4.) value with Width=SharedPeak;Color=(fun id -> if id="Compact" then Paint.rgb 45 110 180 elif id="Wide" then Paint.rgb 35 155 135 else Paint.rgb 210 135 45)}
                ViolinChart.buildWithCancellation token spec groups |> Result.map(fun output ->
                    {Parameter=value;Chart=output.Aggregate.Chart;SelectionDomain=output.Aggregate.Chart.Semantics |> List.collect(fun item -> item.DataIds) |> Set.ofList;SourceCount=3000;MeasuredCount=output.Summaries |> Map.toSeq |> Seq.sumBy(fun (_,s) -> s.Count)
                     VisibleCount=output.Aggregate.Chart.HitTargets.Length;GapCount=0;Detail=sprintf "3,000 original observations | shared peak widths | Gaussian bandwidth %.2f" value})
            else
                let spec={ReducedSeriesSpec.create frame (int value) with Reduction=if kind="minmax" then SeriesReduction.MinMax else SeriesReduction.Lttb}
                ReducedSeriesChart.buildWithCancellation token spec series |> Result.map(fun output ->
                    {Parameter=value;Chart=output.Chart;SelectionDomain=seriesDomain.Value;SourceCount=output.Source.Length;MeasuredCount=output.Summary.Count;VisibleCount=output.Visible.Length
                     GapCount=output.Visible |> List.filter(fun p -> p.Y.IsNone) |> List.length
                     Detail=sprintf "100,000 source rows | %d visible | 95,000 measured | original mean %s" output.Visible.Length (output.Summary.Mean |> Option.map(Format.number Globalization.CultureInfo.InvariantCulture 4) |> Option.defaultValue "missing")}))
        let chart()=worker.LastGood |> Option.map(fun output -> output.Chart) |> Option.defaultValue empty
        let scene()=
            let output=chart()
            let selectedKeys=output.Semantics |> List.filter(fun semantic -> semantic.DataIds |> List.exists selected.Value.Contains) |> List.map(fun semantic -> List.last semantic.MarkPath) |> Set.ofList
            let rec highlight = function
                | Leaf mark when selectedKeys.Contains(Mark.key mark) -> Leaf(Mark.withStroke (Paint.rgb 230 140 20) 2.5 mark)
                | Leaf mark -> Leaf mark
                | Group(group,children) -> Group(group,List.map highlight children)
            let detail=worker.LastGood |> Option.map(fun output -> output.Detail) |> Option.defaultValue "Computing…"
            let help=if kind="violin" then "Arrows focus; Enter selects; + / - changes bandwidth. Statistics use original samples." else "Arrows focus; Enter selects; + / - changes the visible budget. Gaps remain explicit."
            Scene.compose frame.Viewport frame.Title (Scene.definitions output.Scene)
                (List.map highlight (Scene.nodes output.Scene) @
                 [Leaf(Mark.text "title" (Point.create 24. 28.) (if kind="violin" then "Violin distributions / 3,000 observations" else "100,000 source rows / "+kind+" reduction") |> Mark.withFontSize 22.)
                  Leaf(Mark.text "help" (Point.create 24. 54.) help |> Mark.withFontSize 13.)
                  Leaf(Mark.text "status" (Point.create 24. 548.) detail |> Mark.withFontSize 13.)])
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render Renderer.passive scene) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
        host.Cycle()
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        svg.setAttribute("tabindex","0")
        let input=ChartInteraction.attachWithSelectionDomain svg (fun () -> {Mode=GestureMode.Rectangle;MinZoom=1.;MaxZoom=1.;WheelSensitivity=0.01;MaxLassoPoints=4}) chart
                      (fun () -> worker.LastGood |> Option.map(fun output -> output.SelectionDomain) |> Option.defaultValue Set.empty) ignore
        Api.createEffect(fun () -> selected.Value<-Set.union input.Selection.Selected (Set.ofList(Option.toList input.Selection.Focused))) |> ignore
        let key (event:Ranvier.Dom.Event)=
            let value=(event :?> Ranvier.Dom.KeyboardEvent).key
            if value="+" || value="-" then
                event.preventDefault()
                graph.Run(fun () ->
                    let step=if kind="violin" then 0.05 else 100.
                    let lower,upper=if kind="violin" then 0.05,1.5 else 100.,2000.
                    parameter.Value<-max lower (min upper (parameter.Value+(if value="+" then step else -step))))
        svg.addEventListener("keydown",key)
        Api.onCleanup(fun () -> svg.removeEventListener("keydown",key))
        owner,parameter,worker,input))

namespace Ranvier.D5.Examples

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

[<RequireQualifiedAccess>]
module LinkedCharts =
    /// <summary>Drag a rectangle in the scatter plot; the line below follows the shared selection signal. Escape clears it.</summary>
    let mount (host:WebCoreHost) (graph:Graph) =
        graph.Run(fun () -> Api.createRoot(fun owner ->
            let selected=Api.createSignal Set.empty<string>
            let observations=[0..60] |> List.map(fun i -> string i,float i/6.,sin(float i/6.))
            let frame title=ChartFrame.create 800. 275. title
            let x=Encoding.create "Time" (fun (_,x,_) -> Some x)
            let y=Encoding.create "Value" (fun (_,_,y) -> Some y)
            let key (key,_,_)=key
            let scatter=Reactive.derive (fun () -> selected.Value) (fun selected ->
                let spec=ChartSpec.scatter (frame "Brush the scatter plot; arrows and Enter select points") key x y
                Chart.scatter {spec with Color=fun row -> if Set.contains (key row) selected then Paint.rgb 220 70 40 else Paint.rgb 40 110 190} observations
                |> Result.defaultWith(fun errors -> failwithf "%A" errors))
            let line=Reactive.derive (fun () -> selected.Value) (fun selected ->
                let spec=ChartSpec.line (frame "Linked line: selected observations") (fun (id,_,_) -> id) (Encoding.create "Time" (fun (_,x,_) -> Some x)) (Encoding.create "Value" (fun (_,_,y) -> y))
                let rows=observations |> List.map(fun (id,x,y) -> id,x,(if selected.IsEmpty || Set.contains id selected then Some y else None))
                let spec={spec with XDomain=DomainPolicy.Fixed(0.,10.);YDomain=DomainPolicy.Fixed(-1.,1.)}
                Chart.line spec rows |> Result.defaultWith(fun errors -> failwithf "%A" errors))
            let mountChart read =
                let container=host.Document.createElement "div"
                host.Document.body.appendChild container |> ignore
                let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                                (fun context -> Program.run (Component.render Renderer.passive (fun () -> (read()).Scene)) context.Backend)
                Api.onCleanup(fun () -> mounted.Dispose();container.remove())
                container
            let scatterContainer=mountChart(fun () -> scatter.Value)
            mountChart(fun () -> line.Value) |> ignore
            host.Cycle()
            let svg=scatterContainer.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
            svg.setAttribute("tabindex","0")
            let config={Mode=GestureMode.Rectangle;MinZoom=0.25;MaxZoom=8.;WheelSensitivity=0.002;MaxLassoPoints=256}
            let interaction=ChartInteraction.attach svg (fun () -> config) (fun () -> scatter.Value) ignore
            Api.createEffect(fun () ->
                let next=interaction.Selection.Selected
                Api.untrack(fun () -> if selected.Value<>next then selected.Value<-next)) |> ignore
            owner,selected,(fun () -> scatter.Value),(fun () -> line.Value),interaction))

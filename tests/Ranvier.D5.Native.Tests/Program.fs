module Ranvier.D5.Native.Tests.Program

open System
open Expecto
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

// Native WebCore must be created on the process main thread on macOS.
let acceptance () =
        use host = WebCoreHost.Create(320, 200)
        use graph = new Graph()
        use canvasService = Canvas.forHost host
        let canvas = unbox<Ranvier.Dom.HTMLCanvasElement>(host.Document.createElement "canvas")
        let context =
            match canvasService.GetContext(canvas,Ranvier.Dom.CanvasContextType.TwoD,Ranvier.Dom.Canvas2DSettings.Default) with
            | Ranvier.Dom.CanvasContextOutcome.Context value -> value
            | _ -> failwith "Native 2D measurement context unavailable."
        use textCache = TextMeasurement.native 32 context
        let textRequest = {TextMeasureRequest.Text="Measured label";FontFamily="sans-serif";FontWeight=400;FontSize=12.;DeviceScale=1.;FontGeneration=0L}
        let measured = textCache.Measure textRequest
        Expect.isGreaterThan measured.Width 0. "native width"
        Expect.isGreaterThan (textCache.Measure {textRequest with FontSize=24.}).Width measured.Width "native font size changes"
        Expect.equal context.font "10px sans-serif" "measurement restores host font"
        let ownerThread=Environment.CurrentManagedThreadId
        let worker=graph.Run(fun () -> ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> 42) (fun token value ->
            token.ThrowIfCancellationRequested()
            Expect.notEqual Environment.CurrentManagedThreadId ownerThread "kernel is off the host thread"
            Ok value))
        let timeout=Diagnostics.Stopwatch.StartNew()
        while graph.Run(fun () -> worker.LastGood.IsNone) && timeout.Elapsed.TotalSeconds<5. do
            host.Cycle()
            host.Frame()
            Threading.Thread.Yield() |> ignore
        Expect.equal (graph.Run(fun () -> worker.LastGood)) (Some 42) "native owner/frame publication"
        (worker :> IDisposable).Dispose()
        let container = host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let box key x color =
            Shape.rectangle (Bounds.create x 10. 60. 60.) |> Mark.path key |> Mark.withFill color
        let make marks = Scene.create 300. 180. "Test scene" marks
        let source = graph.Run(fun () -> Api.createSignal (make [box "a" 10. (Paint.rgb 255 0 0); box "b" 90. (Paint.rgb 0 0 255)]))
        let events = ResizeArray<string * string>()
        let view = Renderer.create (fun key name _ -> events.Add(key, name))
        let mounted =
            Mounting.mount graph None (fun errors -> raise (AggregateException errors)) container None
                (fun context -> Program.run (Component.render view (fun () -> source.Value)) context.Backend)
        host.Cycle()
        host.Frame()
        let original = container.querySelector "[data-d5-key='a']"
        Expect.isNotNull original "mark exists"
        Expect.equal original.namespaceURI "http://www.w3.org/2000/svg" "native SVG namespace"
        let struct(width, height) = host.PaintSize
        let before = Array.zeroCreate<byte> (width * height * 4)
        host.Paint before
        let updated=box "a" 20. (Paint.rgb 0 255 0)
        let updated=updated |> Mark.withStroke (Paint.rgb 1 2 3) 2. |> Mark.withOpacity 0.75 |> Mark.withFontSize 18. |> Mark.withTransform(Transform.translate 1. 2.)
        let updated=updated |> Mark.withTextStyle {Mark.textStyle updated with Family="serif";Weight=700;Anchor="middle";Baseline="central"}
        graph.Run(fun () -> source.Value <- make [box "b" 90. (Paint.rgb 0 0 255); updated])
        host.Cycle()
        host.Frame()
        let retained = container.querySelector "[data-d5-key='a']"
        Expect.isTrue (Object.ReferenceEquals(original, retained)) "key retained"
        Expect.equal (retained.getAttribute "fill") "#00ff00" "paint updated"
        Expect.equal (retained.getAttribute "stroke") "#010203" "stroke updated"
        Expect.equal (retained.getAttribute "stroke-width") "2" "stroke width updated"
        Expect.isFalse (retained.hasAttribute "opacity") "wrapper does not create an opacity group"
        Expect.equal ((retained.querySelector "path").getAttribute "opacity") "0.75" "path opacity updated"
        Expect.equal ((retained.querySelector "text").getAttribute "opacity") "0.75" "text opacity ready for geometry changes"
        Expect.equal (retained.getAttribute "font-size") "18" "font size updated"
        Expect.equal (retained.getAttribute "font-family") "serif" "font family updated"
        Expect.equal (retained.getAttribute "font-weight") "700" "font weight updated"
        Expect.equal (retained.getAttribute "text-anchor") "middle" "text anchor updated"
        Expect.equal (retained.getAttribute "dominant-baseline") "central" "text baseline updated"
        Expect.equal (retained.getAttribute "transform") (Transform.toSvg(Transform.translate 1. 2.)) "transform updated"
        let after = Array.zeroCreate<byte> before.Length
        host.Paint after
        Expect.isFalse (before = after) "native pixels changed"
        retained.setAttribute("opacity","0.75")
        (retained.querySelector "path").setAttribute("opacity","1")
        (retained.querySelector "text").setAttribute("opacity","1")
        host.Cycle()
        host.Frame()
        let reference=Array.zeroCreate<byte> after.Length
        host.Paint reference
        let maxDifference=Array.map2(fun a b -> abs(int a-int b)) after reference |> Array.max
        Expect.isLessThanOrEqual maxDifference 1 "visible-element opacity preserves filled/stroked pixel compositing"
        retained.removeAttribute "opacity"
        (retained.querySelector "path").setAttribute("opacity","0.75")
        (retained.querySelector "text").setAttribute("opacity","0.75")
        host.Cycle()
        graph.Run(fun () -> source.Value <- make [box "b" 90. (Paint.rgb 0 0 255); updated |> Mark.withStroke Paint.none 0.])
        host.Cycle()
        Expect.equal ((retained.querySelector "path").getAttribute "opacity") "1" "removing stroke disables combined opacity"
        Expect.equal ((retained.querySelector "path").getAttribute "fill-opacity") "0.75" "fill-only alpha updates after style change"
        Expect.equal ((retained.querySelector "path").getAttribute "stroke-opacity") "1" "unused stroke alpha resets"
        graph.Run(fun () -> source.Value <- make [box "b" 90. (Paint.rgb 0 0 255); updated])
        host.Cycle()
        Expect.equal ((retained.querySelector "path").getAttribute "opacity") "0.75" "adding stroke restores combined opacity"
        Expect.equal ((retained.querySelector "path").getAttribute "fill-opacity") "1" "fill alpha resets for combined paint"
        host.Click(40, 40) |> ignore
        Expect.isTrue (events.Contains ("a", "click")) "retained mark receives pointer event"
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        let commands=ResizeArray<GestureCommand>()
        let config={Mode=GestureMode.Pan;MinZoom=0.1;MaxZoom=10.;WheelSensitivity=0.01;MaxLassoPoints=128}
        let inputOwner,input,state=graph.Run(fun () ->
            Api.createRoot(fun owner ->
                let state=Api.createSignal Gesture.empty
                let input=InputAdapter.attach svg (fun () -> config) (fun () -> state.Value) (fun value -> state.Value<-value) commands.Add
                owner,input,state))
        host.Mouse(1,40,40,0,1,0) |> ignore
        host.Mouse(0,60,50,0,0,0) |> ignore
        Expect.isTrue (svg.hasPointerCapture 1) "native pointer capture"
        host.Mouse(2,60,50,0,1,0) |> ignore
        Expect.isFalse (svg.hasPointerCapture 1) "pointer release"
        Expect.isTrue (commands |> Seq.exists(function GestureCommand.ZoomChanged _ -> true | _ -> false)) "native event enters signal graph"
        Expect.isEmpty (graph.Run(fun () -> state.Value.Pointers)) "pointer state released"
        inputOwner.Dispose()
        let count=commands.Count
        input.Send(InputEvent.Wheel(Point.create 40. 40.,1.))
        host.Click(40,40) |> ignore
        Expect.equal commands.Count count "disposed listeners and explicit input remain inert"
        graph.Run(fun () -> source.Value <- make [Mark.text "a" (Point.create 10. 30.) "<script>&\"Hello\"" |> Mark.withLabel (Some "<unsafe>&\"label\"")])
        host.Cycle()
        Expect.equal ((container.querySelector "text").textContent) "<script>&\"Hello\"" "same key can change geometry kind"
        let composed () =
            let clip = RectClip("plot",Bounds.create 0. 0. 30. 30.)
            let stops = [{Offset=0.;Color=Paint.rgb 255 0 0;Opacity=1.};{Offset=1.;Color=Paint.rgb 0 0 255;Opacity=1.}]
            let rect = Shape.rectangle(Bounds.create 0. 0. 80. 80.) |> Mark.path "same" |> Mark.withFillDefinition "paint"
            Scene.compose {Width=300.;Height=180.} "Composed"
                [clip;RectClip(string(char 0xD800),Bounds.create 0. 0. 1. 1.);RectClip(string(char 0xD801),Bounds.create 0. 0. 1. 1.);LinearGradient("paint",Point.create 0. 0.,Point.create 30. 0.,stops)]
                [Group({GroupSpec.create "plot" with Clip=Some "plot";Transform=Transform.translate 10. 10.},[Leaf rect])]
        graph.Run(fun () -> source.Value <- composed())
        host.Cycle()
        host.Frame()
        let group = container.querySelector "[data-d5-group='plot']"
        let clipPaths=container.querySelectorAll "clipPath"
        let clipIds=[for i in 0u..clipPaths.length-1u -> (clipPaths.item i :?> Ranvier.Dom.Element).getAttribute "id"]
        Expect.equal (Set.ofList clipIds).Count 3 "all UTF-16 definition keys have unique DOM IDs"
        let clipId = (container.querySelector "clipPath").getAttribute "id"
        Expect.equal (group.getAttribute "clip-path") ("url(#"+clipId+")") "clip reference scoped to mount"
        let clipped = Array.zeroCreate<byte> before.Length
        host.Paint clipped
        let pixel x y = let i=(y*width+x)*4 in clipped[i],clipped[i+1],clipped[i+2]
        Expect.notEqual (pixel 25 25) (pixel 65 25) "clipped plot differs from background"
        Expect.equal (pixel 65 25) (pixel 110 25) "outside clip is background"
        let secondContainer = host.Document.createElement "div"
        host.Document.body.appendChild secondContainer |> ignore
        let second = Mounting.mount graph None (fun errors -> raise(AggregateException errors)) secondContainer None
                        (fun context -> Program.run (Component.render Renderer.passive composed) context.Backend)
        host.Cycle()
        Expect.notEqual ((secondContainer.querySelector "clipPath").getAttribute "id") clipId "mount-local definition identity"
        let retainedGroup = container.querySelector "[data-d5-group='plot']"
        graph.Run(fun () -> source.Value <- composed())
        host.Cycle()
        Expect.isTrue (Object.ReferenceEquals(retainedGroup,container.querySelector "[data-d5-group='plot']")) "group retained"
        second.Dispose()
        graph.Run(fun () -> source.Value <- make [])
        host.Cycle()
        Expect.isNull (container.querySelector "[data-d5-key='a']") "exit removed"
        mounted.Dispose()
        graph.Run(fun () -> source.Value <- make [box "after-dispose" 10. (Paint.rgb 0 0 0)])
        host.Cycle()
        Expect.isNull (container.querySelector "svg") "disposed mount cannot resurrect scene"
        let linkedOwner,selected,readScatter,readLine,interaction=Ranvier.D5.Examples.LinkedCharts.mount host graph
        host.Cycle()
        let chart=graph.Run readScatter
        let point=match chart.HitTargets[10].Geometry with Ranvier.D5.Charts.HitPoint(point,_) -> point | _ -> failwith "scatter target"
        graph.Run(fun () ->
            interaction.Send(InputEvent.Down(1,Point.create (point.X-2.) (point.Y-2.),None))
            interaction.Send(InputEvent.Up(1,Point.create (point.X+2.) (point.Y+2.))))
        Expect.equal (graph.Run(fun () -> selected.Value)) (set ["10"]) "brush updates shared signal"
        Expect.equal (graph.Run(fun () -> (readLine()).HitTargets.Length)) 1 "linked line follows selection"
        let scopedChart=graph.Run(fun () -> Api.createSignal {chart with Semantics=[{chart.Semantics.Head with MarkPath=["A";"p"]};{chart.Semantics.Head with MarkPath=["B";"p"]}]})
        let scopedOwner,scopedInput=graph.Run(fun () -> Api.createRoot(fun owner ->
            let svg=host.Document.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
            owner,ChartInteraction.attach svg (fun () -> {config with Mode=GestureMode.Drag}) (fun () -> scopedChart.Value) ignore))
        scopedInput.Send(InputEvent.Down(1,point,Some ["A";"p"]))
        graph.Run(fun () -> scopedChart.Value<-{scopedChart.Value with Semantics=[scopedChart.Value.Semantics[1]]})
        Expect.isEmpty (graph.Run(fun () -> scopedInput.Gesture.Pointers)) "removed scoped target cancels despite identical sibling leaf"
        scopedOwner.Dispose()
        let seriesOwner,seriesInput,seriesData,seriesBudget=graph.Run(fun () -> Api.createRoot(fun owner ->
            let data=Api.createSignal [for i in 0..4 -> {SeriesSample.Id=string i;X=float i;Y=Some(if i=2 then 10. else 0.)}]
            let budget=Api.createSignal 5
            let frame=Ranvier.D5.Charts.ChartFrame.create 300. 180. "Selection reduction"
            let chart=Reactive.derive (fun () -> data.Value,budget.Value) (fun (values,count) ->
                let spec={Ranvier.D5.Charts.ReducedSeriesSpec.create frame count with Reduction=Ranvier.D5.Charts.SeriesReduction.Lttb}
                Ranvier.D5.Charts.ReducedSeriesChart.build spec values |> Result.defaultWith(fun e -> failwithf "%A" e))
            let domain=Reactive.derive (fun () -> data.Value) (List.choose(fun sample -> sample.Y |> Option.map(fun _ -> sample.Id)) >> Set.ofList)
            let svg=host.Document.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
            let input=ChartInteraction.attachWithSelectionDomain svg (fun () -> config) (fun () -> chart.Value.Chart) (fun () -> domain.Value) ignore
            owner,input,data,budget))
        for _ in 1..3 do seriesInput.Send(InputEvent.Key "ArrowRight")
        seriesInput.Send(InputEvent.Key "Enter")
        Expect.equal (graph.Run(fun () -> seriesInput.Selection.Selected)) (set ["2"]) "keyboard selects original sample"
        graph.Run(fun () -> seriesBudget.Value<-2)
        Expect.equal (graph.Run(fun () -> seriesInput.Selection.Selected)) (set ["2"]) "sampling does not delete source selection"
        Expect.equal (graph.Run(fun () -> seriesInput.Selection.Focused)) None "hidden sample leaves keyboard targets"
        graph.Run(fun () -> seriesBudget.Value<-5)
        Expect.equal (graph.Run(fun () -> seriesInput.Selection.Selected)) (set ["2"]) "visible sample regains selection"
        graph.Run(fun () -> seriesData.Value<-seriesData.Value |> List.map(fun sample -> if sample.Id="2" then {sample with Y=None} else sample))
        Expect.isEmpty (graph.Run(fun () -> seriesInput.Selection.Selected)) "missing measurement is no longer selectable"
        seriesOwner.Dispose()
        linkedOwner.Dispose()
        host.Cycle()
        Expect.equal (host.Document.querySelectorAll("svg").length) 0u "linked owner removes both views"
        let forceOwner,force,networkInput,networkSvg=graph.Run(fun () -> Api.createRoot(fun owner ->
            let topology=GraphData.create [{GraphNode.create "node" with Position=Some(Point.create 100. 100.);Radius=12.}] [] |> Result.defaultWith(fun e -> failwithf "%A" e)
            let initial=Force.initialize {ForceConfig.create(Point.create 100. 100.) with MaxSteps=1} (Seed.create 1u) topology |> Result.defaultWith(fun e -> failwithf "%A" e) |> Force.step 1
            let force=ReactiveForce.create (Ranvier.D5.Examples.HostDispatcher.forHost host) ForceChunk.defaultValue initial
            let spec=Ranvier.D5.Charts.NetworkSpec.create(Ranvier.D5.Charts.ChartFrame.create 300. 180. "Drag")
            let chart=Reactive.derive (fun () -> force.Snapshot) (fun snapshot -> Ranvier.D5.Charts.NetworkChart.network spec topology snapshot |> Result.defaultWith(fun e -> failwithf "%A" e))
            let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                            (fun context -> Program.run (Component.render Renderer.passive (fun () -> chart.Value.Scene)) context.Backend)
            Api.onCleanup(fun () -> mounted.Dispose())
            host.Cycle()
            let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
            owner,force,NetworkInteraction.attach svg (fun () -> chart.Value) force,svg))
        host.Cycle()
        host.Frame()
        host.Paint before
        let nodeBounds=(networkSvg.querySelector "[data-d5-key='4:node4:node']").getBoundingClientRect()
        Expect.floatClose Accuracy.medium nodeBounds.width 24. "shared circle geometry preserves native radius"
        Expect.floatClose Accuracy.medium nodeBounds.height 24. "shared circle geometry preserves native height"
        host.Mouse(1,108,108,0,1,0) |> ignore
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes.Head.Pinned)) (Some(Point.create 100. 100.)) "native drag starts pin"
        Expect.isTrue (networkSvg.hasPointerCapture 1) "network captures pointer"
        host.Mouse(0,128,118,0,0,0) |> ignore
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes.Head.Position)) (Point.create 120. 110.) "native drag delta maps into plot"
        host.Mouse(2,138,118,0,1,0) |> ignore
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes.Head.Position)) (Point.create 130. 110.) "release applies final movement"
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes.Head.Pinned)) None "release unpins after final movement"
        Expect.isFalse (networkSvg.hasPointerCapture 1) "network releases capture"
        let target=Some ["plot";"4:node4:node"]
        networkInput.Send(InputEvent.Down(1,Point.create 130. 110.,target))
        networkInput.Send InputEvent.Cancel
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes.Head.Pinned)) None "cancel unpins"
        networkInput.Send(InputEvent.Down(1,Point.create 130. 110.,target))
        networkInput.Send InputEvent.FocusLost
        Expect.equal (graph.Run(fun () -> force.Snapshot.Nodes.Head.Pinned)) None "focus loss unpins"
        networkInput.Send(InputEvent.Key "ArrowRight")
        networkInput.Send(InputEvent.Key "Enter")
        Expect.equal (graph.Run(fun () -> networkInput.Selection.Selected)) (set ["node"]) "keyboard selects semantic node"
        networkInput.Send(InputEvent.Down(1,Point.create 130. 110.,target))
        forceOwner.Dispose()
        host.Cycle()
        let flowOwner,flowState,flowInput,flowSvg=graph.Run(fun () -> Api.createRoot(fun owner ->
            let frame=Ranvier.D5.Charts.ChartFrame.create 300. 180. "Flow drag"
            let bounds=Ranvier.D5.Charts.ChartLayout.plot frame |> Result.defaultWith(fun e -> failwithf "%A" e)
            let config={SankeyConfig.create bounds with NodeWidth=18.;NodePadding=Bounds.height bounds/2.;Iterations=0}
            let topology=FlowGraph.create ["a";"b";"spacer"] [{FlowLink.Id="ab";Source="a";Target="b";Value=1.}]
                         |> Result.defaultWith(fun e -> failwithf "%A" e)
            let initial=Sankey.layout config topology |> Result.defaultWith(fun e -> failwithf "%A" e)
            let state=Api.createSignal initial
            let spec={Ranvier.D5.Charts.FlowSpec.create frame with ShowLabels=false}
            let chart=Reactive.derive (fun () -> state.Value) (fun layout -> Ranvier.D5.Charts.FlowChart.sankey spec layout |> Result.defaultWith(fun e -> failwithf "%A" e))
            let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                            (fun context -> Program.run (Component.render Renderer.passive (fun () -> chart.Value.Scene)) context.Backend)
            Api.onCleanup(fun () -> mounted.Dispose())
            host.Cycle()
            let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
            let input=FlowInteraction.attach svg (fun () -> chart.Value) (fun () -> config) (fun () -> state.Value)
                          (fun next -> state.Value<-next) (fun errors -> failwithf "%A" errors)
            owner,state,input,svg))
        host.Cycle()
        host.Frame()
        host.Paint before
        let flowPath=flowSvg.querySelector "[data-d5-key='4:link2:ab'] path"
        Expect.equal (flowPath.getAttribute "opacity") "1" "single stroke avoids element opacity layer"
        Expect.floatClose Accuracy.high (Double.Parse(flowPath.getAttribute "stroke-opacity",Globalization.CultureInfo.InvariantCulture)) 0.4 "link alpha applies directly to stroke paint"
        let paintOnly=Array.zeroCreate<byte> before.Length
        host.Paint paintOnly
        flowPath.setAttribute("stroke-opacity","1")
        flowPath.setAttribute("opacity","0.4")
        host.Cycle()
        host.Frame()
        let elementOpacity=Array.zeroCreate<byte> before.Length
        host.Paint elementOpacity
        Expect.isLessThanOrEqual (Array.map2(fun a b -> abs(int a-int b)) paintOnly elementOpacity |> Array.max) 1 "stroke opacity preserves element-opacity pixels"
        flowPath.setAttribute("stroke-opacity","0.4")
        flowPath.setAttribute("opacity","1")
        host.Cycle()
        let initialFlow=graph.Run(fun () -> flowState.Value)
        let flowBox=(flowSvg.querySelector "[data-d5-key='4:node1:a']").getBoundingClientRect()
        let fx,fy=int(flowBox.x+flowBox.width/2.),int(flowBox.y+flowBox.height/2.)
        host.Mouse(1,fx,fy,0,1,0) |> ignore
        Expect.isTrue (flowSvg.hasPointerCapture 1) "flow captures pointer"
        host.Mouse(0,fx,fy+5,0,0,0) |> ignore
        host.Mouse(2,fx,fy+10,0,1,0) |> ignore
        let movedFlow=graph.Run(fun () -> flowState.Value)
        Expect.floatClose Accuracy.high (Bounds.y movedFlow.Nodes.Head.Bounds) (Bounds.y initialFlow.Nodes.Head.Bounds+10.) "native final-up flow delta"
        Expect.equal movedFlow.Nodes.Tail initialFlow.Nodes.Tail "drag leaves unrelated nodes"
        Expect.floatClose Accuracy.high movedFlow.Links.Head.Start.Y (initialFlow.Links.Head.Start.Y+10.) "incident source endpoint moves"
        Expect.equal movedFlow.Links.Head.Finish initialFlow.Links.Head.Finish "nonincident endpoint fixed"
        Expect.isFalse (flowSvg.hasPointerCapture 1) "flow releases pointer"
        let flowTarget=Some ["plot";"4:node1:a"]
        for ending in [InputEvent.Cancel;InputEvent.FocusLost] do
            flowInput.Send(InputEvent.Down(1,Point.create 50. 50.,flowTarget))
            flowInput.Send ending
            flowInput.Send(InputEvent.Move(1,Point.create 50. 80.))
            Expect.equal (graph.Run(fun () -> flowState.Value)) movedFlow "cancel/focus loss prevents late movement"
        flowInput.Send(InputEvent.Key "ArrowRight")
        flowInput.Send(InputEvent.Key "Enter")
        Expect.equal (graph.Run(fun () -> flowInput.Selection.Selected)) (set ["a"]) "flow keyboard selects domain"
        flowInput.Send(InputEvent.Down(1,Point.create 50. 50.,flowTarget))
        graph.Run(fun () -> flowState.Value<-{Nodes=[];Links=[]})
        Expect.isEmpty (graph.Run(fun () -> flowInput.Gesture.Pointers)) "exiting flow node cancels drag"
        graph.Run(fun () -> flowState.Value<-movedFlow)
        flowInput.Send(InputEvent.Down(1,Point.create 50. 50.,flowTarget))
        flowOwner.Dispose()
        flowInput.Send(InputEvent.Move(1,Point.create 50. 80.))
        Expect.equal (graph.Run(fun () -> flowState.Value)) movedFlow "disposed flow input does not publish"
        host.Cycle()

[<EntryPoint>]
let main _ =
    acceptance ()
    printfn "Native scene acceptance passed: namespace, identity, reorder, paint, events, escaping and disposal."
    0

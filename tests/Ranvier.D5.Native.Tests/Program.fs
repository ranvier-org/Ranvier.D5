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
        let moving = updated |> Mark.withTransform(Transform.translate 3. 4.)
        graph.Run(fun () -> source.Value <- make [box "b" 90. (Paint.rgb 0 0 255); moving])
        host.Cycle()
        Expect.equal (retained.getAttribute "transform") (Transform.toSvg(Transform.translate 3. 4.)) "movement updates independently of appearance"
        Expect.equal (retained.getAttribute "fill") "#00ff00" "movement preserves cached paint"
        graph.Run(fun () -> source.Value <- make [box "b" 90. (Paint.rgb 0 0 255); moving |> Mark.withFill(Paint.rgb 7 8 9)])
        host.Cycle()
        Expect.equal (retained.getAttribute "fill") "#070809" "paint change at fixed transform invalidates appearance"
        Expect.equal (retained.getAttribute "transform") (Transform.toSvg(Transform.translate 3. 4.)) "paint update preserves movement"
        graph.Run(fun () -> source.Value <- make [box "b" 90. (Paint.rgb 0 0 255); updated])
        host.Cycle()
        host.Click(40, 40) |> ignore
        Expect.isTrue (events.Contains ("a", "click")) "retained mark receives pointer event"
        container.setAttribute("data-d5-group","leaked-html-ancestor")
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        let commands=ResizeArray<GestureCommand>()
        let config={Mode=GestureMode.Drag;MinZoom=0.1;MaxZoom=10.;WheelSensitivity=0.01;MaxLassoPoints=128}
        let inputOwner,input,state=graph.Run(fun () ->
            Api.createRoot(fun owner ->
                let state=Api.createSignal Gesture.empty
                let input=InputAdapter.attach svg (fun () -> config) (fun () -> state.Value) (fun value -> state.Value<-value) commands.Add
                owner,input,state))
        host.Mouse(1,40,40,0,1,0) |> ignore
        Expect.equal (graph.Run(fun () -> state.Value.Target)) (Some ["a"]) "InputAdapter stops group walk at host svg"
        host.Mouse(0,60,50,0,0,0) |> ignore
        Expect.isTrue (svg.hasPointerCapture 1) "native pointer capture"
        host.Mouse(2,60,50,0,1,0) |> ignore
        Expect.isFalse (svg.hasPointerCapture 1) "pointer release"
        Expect.isTrue (commands |> Seq.exists(function GestureCommand.DragBy(Some ["a"],_) -> true | _ -> false)) "native event enters signal graph without leaked HTML group"
        container.removeAttribute "data-d5-group"
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
        let retainedLeaf=container.querySelector "[data-d5-key='same']"
        let nested mark=Scene.compose {Width=300.;Height=180.} "Nested update" [] [Group(GroupSpec.create "plot",[Leaf mark])]
        let nestedText=Mark.text "same" (Point.create 20. 30.) "First" |> Mark.withFill(Paint.rgb 10 20 30)
        graph.Run(fun () -> source.Value<-nested nestedText)
        host.Cycle()
        Expect.isTrue (Object.ReferenceEquals(retainedLeaf,container.querySelector "[data-d5-key='same']")) "composed leaf retains its DOM identity across geometry kinds"
        Expect.equal (retainedLeaf.getAttribute "fill") "#0a141e" "composed leaf replaces a scoped definition with solid paint"
        Expect.equal ((retainedLeaf.querySelector "text").textContent) "First" "composed text remains reactive"
        let moved=nestedText |> Mark.withTransform(Transform.translate 5. 6.)
        graph.Run(fun () -> source.Value<-nested moved)
        host.Cycle()
        Expect.equal (retainedLeaf.getAttribute "transform") (Transform.toSvg(Transform.translate 5. 6.)) "composed movement is independent of style"
        graph.Run(fun () -> source.Value<-nested (moved |> Mark.withFill(Paint.rgb 30 40 50) |> Mark.withFontSize 19.))
        host.Cycle()
        Expect.equal (retainedLeaf.getAttribute "fill") "#1e2832" "composed style invalidates the appearance memo"
        Expect.equal (retainedLeaf.getAttribute "font-size") "19" "composed typography remains reactive"
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

// Mark events are heard on the svg root. A mark has no listener of its own: in WebCore, a wheel listener that
// comes or goes with a mark makes the next page update restyle the whole document.
let markEvents () =
        use host = WebCoreHost.Create(320, 200)
        use graph = new Graph()
        let mount view (read:unit -> Scene) =
            let container = host.Document.createElement "div"
            host.Document.body.appendChild container |> ignore
            let mounted =
                Mounting.mount graph None (fun errors -> raise (AggregateException errors)) container None
                    (fun context -> Program.run (Component.render view read) context.Backend)
            host.Cycle()
            host.Frame()
            container,mounted
        let unmount (container:Ranvier.Dom.Element,mounted:Mounting.Mounted) =
            mounted.Dispose()
            host.Document.body.removeChild container |> ignore
            host.Cycle()
        let box key x y = Shape.rectangle (Bounds.create x y 40. 40.) |> Mark.path key |> Mark.withFill (Paint.rgb 255 0 0)
        let flat = Scene.create 300. 180. "Flat" [box "a" 10. 10.; box "b" 90. 10.]
        let grouped =
            Scene.compose {Width=300.;Height=180.} "Grouped" []
                [Group(GroupSpec.create "outer",
                       [Group({GroupSpec.create "inner" with Transform=Transform.translate 100. 60.},[Leaf(box "leaf" 0. 0.)])
                        Leaf(box "sibling" 10. 10.)])]
        let centre (container:Ranvier.Dom.Element) selector =
            let bounds=(container.querySelector selector).getBoundingClientRect()
            int(bounds.x+bounds.width/2.),int(bounds.y+bounds.height/2.)
        let clickAndWheel (x,y) =
            host.Click(x,y) |> ignore
            host.Wheel(x,y,0.f,1.f,0) |> ignore
        let send (target:Ranvier.Dom.Element) name bubbles =
            let event = host.Document.createEvent "Event"
            event.initEvent(name,bubbles,false)
            target.dispatchEvent event |> ignore

        let scopedEvents = ResizeArray<string list * string>()
        let scene = graph.Run(fun () -> Api.createSignal flat)
        let scoped = mount (Renderer.createScoped (fun path name _ -> scopedEvents.Add(path,name))) (fun () -> scene.Value)
        let markA = (fst scoped).querySelector "[data-d5-key='a']"
        clickAndWheel (centre (fst scoped) "[data-d5-key='a']")
        Expect.contains scopedEvents (["a"],"click") "flat mark click carries the mark key alone"
        Expect.contains scopedEvents (["a"],"wheel") "flat mark wheel carries the mark key alone"
        send (markA.querySelector "path") "pointercancel" true
        Expect.contains scopedEvents (["a"],"pointercancel") "an event that bubbles from inside a mark reaches dispatch"
        scopedEvents.Clear()
        // Only a listener on the mark itself hears an event that does not bubble.
        for name in ["wheel";"pointercancel"] do send markA name false
        Expect.isEmpty scopedEvents "a mark has no listener of its own"
        let background=(fst scoped).querySelector("svg").getBoundingClientRect()
        clickAndWheel (int background.x+250,int background.y+150)
        Expect.isEmpty scopedEvents "an event outside every mark is not dispatched"
        graph.Run(fun () -> scene.Value <- grouped)
        host.Cycle()
        host.Frame()
        clickAndWheel (centre (fst scoped) "[data-d5-key='leaf']")
        Expect.contains scopedEvents (["outer";"inner";"leaf"],"click") "nested mark click carries its group keys, outermost first"
        Expect.contains scopedEvents (["outer";"inner";"leaf"],"wheel") "nested mark wheel carries its group keys, outermost first"
        clickAndWheel (centre (fst scoped) "[data-d5-key='sibling']")
        Expect.contains scopedEvents (["outer";"sibling"],"click") "a mark directly under a group carries that group alone"
        unmount scoped
        let count = scopedEvents.Count
        send markA "wheel" true
        Expect.equal scopedEvents.Count count "a disposed mount dispatches nothing"

        let leafEvents = ResizeArray<string * string>()
        let leafOnly = mount (Renderer.create (fun key name _ -> leafEvents.Add(key,name))) (fun () -> grouped)
        clickAndWheel (centre (fst leafOnly) "[data-d5-key='leaf']")
        Expect.contains leafEvents ("leaf","click") "create drops the group scope of a click"
        Expect.contains leafEvents ("leaf","wheel") "create drops the group scope of a wheel"
        unmount leafOnly

let graphLayoutsNative () =
        use host = WebCoreHost.Create(420, 320)
        use graph = new Graph()
        let container = host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let diagram =
            Ranvier.D5.Graph.DiagramGraph.create
                [ Ranvier.D5.Graph.DiagramNode.sized "n1" 80. 32. |> Ranvier.D5.Graph.DiagramNode.inGroup "g1"
                  Ranvier.D5.Graph.DiagramNode.sized "n2" 80. 32. |> Ranvier.D5.Graph.DiagramNode.inGroup "g1" ]
                [ { Ranvier.D5.Graph.DiagramGroup.create "g1" with Padding = Ranvier.D5.Graph.GroupPadding.titled 24. 10. } ]
                [ { Ranvier.D5.Graph.Association.create "e1" "n1" "n2" with Kind = Ranvier.D5.Graph.AssociationKind.Composition } ]
            |> Result.defaultWith (fun errors -> failwithf "%A" errors)
        let sugLayout =
            Ranvier.D5.Graph.Sugiyama.layout
                { Ranvier.D5.Graph.SugiyamaConfig.create (Bounds.create 10. 10. 360. 260.) with Routing = Ranvier.D5.Graph.SugiyamaRouting.Orthogonal }
                diagram
            |> Result.defaultWith (fun errors -> failwithf "%A" errors)
        let sugScene =
            Ranvier.D5.Graph.DiagramScene.render
                { Ranvier.D5.Graph.DiagramTheme.create 400. 300. "Native Diagram" with
                    NodeFill = fun _ -> Paint.rgb 37 99 235
                    NodeLabel = fun _ -> "" }
                sugLayout
        let h3State =
            Ranvier.D5.Graph3D.H3.buildState
                { Ranvier.D5.Graph3D.H3Config.defaultValue with BallRadius = 90.; RootId = Some "n1" }
                diagram
            |> Result.defaultWith (fun errors -> failwithf "%A" errors)
        let cam = Ranvier.D5.Graph3D.Camera3D.orbit (Point3.create 0. 0. 0.) 0.4 0.3 260.
        let theme3D = { Ranvier.D5.Graph3D.Scene3DTheme.create 400. 300. "Native H3" with ReferenceSphereRadius = Some 90. }
        let clicks = ResizeArray<string list * string>()
        let activeScene = graph.Run(fun () -> Api.createSignal sugScene)
        let mounted =
            Mounting.mount graph None (fun errors -> raise (AggregateException errors)) container None
                (fun context -> Program.run (Component.render (Renderer.createScoped (fun path name _ -> clicks.Add(path, name))) (fun () -> activeScene.Value)) context.Backend)
        host.Cycle()
        host.Frame()
        let struct(w, h) = host.PaintSize
        let pixels2D = Array.zeroCreate<byte> (w * h * 4)
        host.Paint pixels2D
        let n1 = sugLayout.Nodes |> List.find (fun n -> n.Id = "n1")
        let px, py = int (Math.Round n1.Center.X) + 8, int (Math.Round n1.Center.Y) + 8
        let idx = (py * w + px) * 4
        Expect.equal (int pixels2D[idx], int pixels2D[idx + 1], int pixels2D[idx + 2]) (37, 99, 235) "Sugiyama node fills native pixels"
        host.Click(px, py) |> ignore
        Expect.contains clicks (["nodes"; "node:n1:shape"], "click") "DiagramScene scoped click dispatches node key"
        graph.Run(fun () -> activeScene.Value <- Ranvier.D5.Graph3D.Scene3D.render theme3D cam (Ranvier.D5.Graph3D.H3.project h3State))
        host.Cycle()
        host.Frame()
        let pixels3DBefore = Array.zeroCreate<byte> (w * h * 4)
        host.Paint pixels3DBefore
        let boosted = Ranvier.D5.Graph3D.H3.focusNode "n2" h3State |> Result.defaultWith (fun errors -> failwithf "%A" errors)
        graph.Run(fun () -> activeScene.Value <- Ranvier.D5.Graph3D.Scene3D.render theme3D cam (Ranvier.D5.Graph3D.H3.project boosted))
        host.Cycle()
        host.Frame()
        let pixels3DAfter = Array.zeroCreate<byte> (w * h * 4)
        host.Paint pixels3DAfter
        Expect.isFalse (pixels3DBefore = pixels3DAfter) "H3 Lorentz focus boost updates native WebCore pixels"
        mounted.Dispose()
        host.Cycle()

let spatialOrganisationNative () =
        use host = WebCoreHost.Create(420, 320)
        use graph = new Graph()
        let container = host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let ok = function Ok v -> v | Error e -> failwithf "%A" e
        let closedPoly (pts: Point list) =
            match pts with
            | p0 :: rest when not rest.IsEmpty -> Path.ofCommands (MoveTo p0 :: (rest |> List.map LineTo) @ [ Close ])
            | _ -> Path.empty
        let tree =
            Hierarchy.node "root" 0.
                [ Hierarchy.node "gA" 0. [ Hierarchy.node "a1" 30. []; Hierarchy.node "a2" 20. [] ]
                  Hierarchy.node "gB" 0. [ Hierarchy.node "b1" 25. []; Hierarchy.node "b2" 15. [] ] ]
        let gosperRegions = GosperMap.layout (GosperMapConfig.create (Bounds.create 20. 20. 360. 260.)) tree |> ok
        let gosperLeaf = gosperRegions |> List.find (fun r -> r.Id = "a1")
        let gosperMarks =
            gosperRegions
            |> List.filter (fun r -> r.Depth = 2 && not r.Boundary.IsEmpty)
            |> List.map (fun r ->
                let fill = if r.Id = "a1" then Paint.rgb 16 185 129 else Paint.rgb 59 130 246
                Mark.path ("gosper:" + r.Id) (Path.concat (r.Boundary |> List.map closedPoly))
                |> Mark.withFill fill
                |> Mark.withStroke (Paint.rgb 15 23 42) 1.0)
        let gosperScene = Scene.create 400. 300. "Native GosperMap" gosperMarks

        let cuboids =
            Ranvier.D5.Graph3D.Treemap3D.layout
                { Ranvier.D5.Graph3D.Treemap3DConfig.create (Bounds3.create -60. -60. -60. 120. 120. 120.) with
                    Tiling = Ranvier.D5.Graph3D.Treemap3DTiling.CubeHilbert
                    ExplodeFactor = 0.12 }
                tree
            |> ok
        let tmDiag3D = Ranvier.D5.Graph3D.Treemap3D.toDiagramLayout cuboids
        let camTm = Ranvier.D5.Graph3D.Camera3D.orbit (Point3.create 0. 0. 0.) 0.55 0.35 280.
        let themeTm =
            { Ranvier.D5.Graph3D.Scene3DTheme.create 400. 300. "Native Treemap3D" with
                NodeFill = fun _ -> Paint.rgb 225 29 72
                NodeLabel = fun _ -> None }
        let tmScene = Ranvier.D5.Graph3D.Scene3D.render themeTm camTm tmDiag3D

        let sphGraph =
            Ranvier.D5.Graph.DiagramGraph.simple
                [ for i in 0 .. 5 -> Ranvier.D5.Graph.DiagramNode.sized (sprintf "s%d" i) 12. 12. ]
                [ for i in 0 .. 5 -> Ranvier.D5.Graph.Association.create (sprintf "se%d" i) (sprintf "s%d" i) (sprintf "s%d" ((i + 1) % 6)) ]
            |> ok
        let sphLayout =
            Ranvier.D5.Graph3D.SphericalStress3D.layout
                (Ranvier.D5.Graph3D.SphericalLayoutConfig.create (Point3.create 0. 0. 0.) 75.)
                sphGraph
            |> ok
        let camSph = Ranvier.D5.Graph3D.Camera3D.orbit (Point3.create 0. 0. 0.) 0.4 0.3 240.
        let themeSph =
            { Ranvier.D5.Graph3D.Scene3DTheme.create 400. 300. "Native SphericalStress3D" with
                ReferenceSphereRadius = Some 75.
                NodeFill = fun _ -> Paint.rgb 37 99 235
                NodeLabel = fun _ -> None }
        let sphScene = Ranvier.D5.Graph3D.Scene3D.render themeSph camSph sphLayout

        let activeScene = graph.Run(fun () -> Api.createSignal gosperScene)
        let mounted =
            Mounting.mount graph None (fun errors -> raise (AggregateException errors)) container None
                (fun context -> Program.run (Component.render Renderer.passive (fun () -> activeScene.Value)) context.Backend)
        host.Cycle()
        host.Frame()
        let struct(w, h) = host.PaintSize
        let pixelsGosper = Array.zeroCreate<byte> (w * h * 4)
        host.Paint pixelsGosper
        let gx, gy = int (Math.Round gosperLeaf.Centroid.X) + 8, int (Math.Round gosperLeaf.Centroid.Y) + 8
        let gIdx = (gy * w + gx) * 4
        Expect.isLessThan (int pixelsGosper[gIdx]) 240 "GosperMap territory paints native WebCore pixels"

        graph.Run(fun () -> activeScene.Value <- tmScene)
        host.Cycle()
        host.Frame()
        let pixelsTm = Array.zeroCreate<byte> (w * h * 4)
        host.Paint pixelsTm
        let tmFront =
            tmDiag3D.Nodes
            |> List.choose (fun n -> Ranvier.D5.Graph3D.Camera3D.projectPoint { Width = 400.; Height = 300. } camTm n.Center |> Option.map (fun (pt, d, _) -> d, pt))
            |> List.sortBy fst
            |> List.head
            |> snd
        let tx, ty = int (Math.Round tmFront.X) + 8, int (Math.Round tmFront.Y) + 8
        let tIdx = (ty * w + tx) * 4
        Expect.equal (int pixelsTm[tIdx], int pixelsTm[tIdx + 1], int pixelsTm[tIdx + 2]) (225, 29, 72) "Treemap3D frontmost node paints native WebCore pixels"

        graph.Run(fun () -> activeScene.Value <- sphScene)
        host.Cycle()
        host.Frame()
        let pixelsSph = Array.zeroCreate<byte> (w * h * 4)
        host.Paint pixelsSph
        Expect.isFalse (pixelsTm = pixelsSph) "SphericalStress3D updates native WebCore pixels"
        let sphFront =
            sphLayout.Nodes
            |> List.choose (fun n -> Ranvier.D5.Graph3D.Camera3D.projectPoint { Width = 400.; Height = 300. } camSph n.Center |> Option.map (fun (pt, d, _) -> d, pt))
            |> List.sortBy fst
            |> List.head
            |> snd
        let sx, sy = int (Math.Round sphFront.X) + 8, int (Math.Round sphFront.Y) + 8
        let sIdx = (sy * w + sx) * 4
        Expect.equal (int pixelsSph[sIdx], int pixelsSph[sIdx + 1], int pixelsSph[sIdx + 2]) (37, 99, 235) "SphericalStress3D frontmost node paints native WebCore pixels"

        mounted.Dispose()
        host.Cycle()

[<EntryPoint>]
let main _ =
    acceptance ()
    markEvents ()
    graphLayoutsNative ()
    spatialOrganisationNative ()
    printfn "Native scene acceptance passed: namespace, identity, reorder, paint, events, escaping, graph layouts, spatial organisation and disposal."
    0

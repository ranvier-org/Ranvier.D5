module Ranvier.D5.Gallery.Program

open System
open System.Diagnostics
open System.IO
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore
open Ranvier.View.WebCore.Sdl

let mount (host: WebCoreHost) graph read =
    let container = host.Document.createElement "div"
    host.Document.body.appendChild container |> ignore
    Mounting.mount graph None (fun errors -> raise (AggregateException errors)) container None
        (fun context -> Program.run (Component.render Renderer.passive read) context.Backend)

let capture kind path =
    use host = WebCoreHost.Create(820, 590)
    use graph = new Graph()
    let mounted:IDisposable =
        if kind="network" || kind="bubbles" then
            let owner,force,input=NetworkGallery.mount (kind="bubbles") host graph
            let timer=Stopwatch.StartNew()
            while graph.Run(fun () -> force.Snapshot.Iteration<40 && not force.Snapshot.Finished) && timer.Elapsed.TotalSeconds<15. do
                host.Cycle()
                host.Frame()
                System.Threading.Thread.Yield() |> ignore
            if graph.Run(fun () -> force.Snapshot.Iteration<40) then failwith "Network worker did not make progress."
            host.Cycle()
            let struct(w,h)=host.PaintSize
            host.Paint(Array.zeroCreate<byte>(w*h*4))
            let node=graph.Run(fun () -> force.Snapshot.Nodes.Head)
            let x,y=int(Math.Round node.Position.X)+8,int(Math.Round node.Position.Y)+8
            host.Mouse(1,x,y,0,1,0) |> ignore
            if graph.Run(fun () -> force.Snapshot.Nodes |> List.find(fun item -> item.Id=node.Id) |> fun item -> item.Pinned.IsNone) then
                failwith "Dense native network drag did not pin the node."
            host.Mouse(0,x+10,y+5,0,0,0) |> ignore
            host.Mouse(2,x+10,y+5,0,1,0) |> ignore
            if graph.Run(fun () -> force.Snapshot.Nodes |> List.find(fun item -> item.Id=node.Id) |> fun item -> item.Pinned.IsSome) then
                failwith "Dense native network release did not unpin the node."
            input.Send(InputEvent.Key "ArrowRight")
            input.Send(InputEvent.Key "Enter")
            if graph.Run(fun () -> input.Selection.Selected.IsEmpty) then failwith "Network keyboard selection failed."
            owner :> IDisposable
        elif kind="flow-sankey" || kind="flow-alluvial" || kind="flow-chord" then
            let owner,state,input=FlowGallery.mount (kind.Substring 5) host graph
            host.Cycle()
            host.Frame()
            let struct(w,h)=host.PaintSize
            host.Paint(Array.zeroCreate<byte>(w*h*4))
            if kind<>"flow-chord" then
                let node=graph.Run(fun () -> state.Value.Nodes |> List.find(fun node -> node.Id="Grid"))
                let x,y=int(Bounds.x node.Bounds+Bounds.width node.Bounds/2.)+8,int(Bounds.y node.Bounds+Bounds.height node.Bounds/2.)+8
                host.Mouse(1,x,y,0,1,0) |> ignore
                host.Mouse(0,x,y+5,0,0,0) |> ignore
                host.Mouse(2,x,y+10,0,1,0) |> ignore
                let after=graph.Run(fun () -> state.Value.Nodes |> List.find(fun node -> node.Id="Grid"))
                if abs(Bounds.y after.Bounds-Bounds.y node.Bounds-10.)>0.01 then failwith "Native flow drag did not apply the final movement."
            input.Send(InputEvent.Key "ArrowRight")
            input.Send(InputEvent.Key "Enter")
            if graph.Run(fun () -> input.Selection.Selected.IsEmpty) then failwith "Flow keyboard selection failed."
            owner :> IDisposable
        elif kind.StartsWith("density-recipe-") then
            let recipe=kind.Substring("density-recipe-".Length)
            let owner,bandwidth,worker,input=DensityRecipesGallery.mount recipe host graph
            graph.Run(fun () -> bandwidth.Value<-2.5)
            let timer=Stopwatch.StartNew()
            let ready()=graph.Run(fun () -> worker.LastGood |> Option.exists(fun (width,output) -> width=2.5 && not output.Chart.HitTargets.IsEmpty))
            while not(ready()) && timer.Elapsed.TotalSeconds<15. do
                host.Cycle()
                host.Frame()
                System.Threading.Thread.Yield() |> ignore
            if not(ready()) then failwith "Density recipe worker failed to publish."
            host.Cycle()
            let struct(w,h)=host.PaintSize
            host.Paint(Array.zeroCreate<byte>(w*h*4))
            let output=graph.Run(fun () -> worker.LastGood.Value |> snd)
            if recipe="isobands" || recipe="contours" then
                host.Mouse(0,408,308,0,0,0) |> ignore
                if graph.Run(fun () -> input.Selection.Hovered.IsSome) then failwith "Density hole must not be picked."
                host.Mouse(1,405,305,0,1,0) |> ignore
                host.Mouse(0,411,311,0,0,0) |> ignore
                host.Mouse(2,411,311,0,1,0) |> ignore
                if graph.Run(fun () -> not input.Selection.Selected.IsEmpty) then failwith "Brush in a density hole must not select a region."
                let anchor=
                    output.Chart.HitTargets |> List.choose(fun target ->
                        match target.Geometry with
                        | Ranvier.D5.Charts.HitRegion(_,_,anchor) -> Some anchor
                        | _ -> None) |> List.last
                host.Mouse(0,int(Math.Round anchor.X)+8,int(Math.Round anchor.Y)+8,0,0,0) |> ignore
                if graph.Run(fun () -> input.Selection.Hovered.IsNone) then failwith "Density filled region must be picked."
                let x,y=int(Math.Round anchor.X)+8,int(Math.Round anchor.Y)+8
                host.Mouse(1,x-3,y-3,0,1,0) |> ignore
                host.Mouse(0,x+3,y+3,0,0,0) |> ignore
                host.Mouse(2,x+3,y+3,0,1,0) |> ignore
                if graph.Run(fun () -> input.Selection.Selected.IsEmpty) then failwith "Brush around a density anchor must select a region."
            input.Send(InputEvent.Key "ArrowRight")
            input.Send(InputEvent.Key "Enter")
            if graph.Run(fun () -> input.Selection.Selected.IsEmpty) then failwith "Density keyboard selection failed."
            if recipe="heatmap" || recipe="hexbin" then
                if (output.Membership |> Map.toList |> List.sumBy(fun (_,members) -> members.Length))<>100000 then failwith "Density recipe lost source membership."
            owner :> IDisposable
        elif kind="reduction" then
            let owner,target,worker=ReductionGallery.mount host graph
            graph.Run(fun () -> target.Value<-250)
            let timer=Stopwatch.StartNew()
            let ready ()=graph.Run(fun () -> worker.LastGood |> Option.exists(fun (count,bins,minmax,lttb) ->
                count=250 && minmax.Length<=count && lttb.Length<=count &&
                minmax.Head.Id="0" && (List.last minmax).Id="99999" && lttb.Head.Id="0" && (List.last lttb).Id="99999" &&
                (bins |> List.sumBy(fun bin -> bin.Members.Length))=100000))
            while not(ready()) && timer.Elapsed.TotalSeconds<10. do
                host.Cycle()
                host.Frame()
                System.Threading.Thread.Yield() |> ignore
            if not(ready()) then failwith "Reduction worker lost source membership, endpoints or output bounds."
            owner :> IDisposable
        elif kind="contour" then
            let owner,multiplier,worker=ContourGallery.mount host graph
            graph.Run(fun () -> multiplier.Value<-1.25)
            let timer=Stopwatch.StartNew()
            let ready ()=graph.Run(fun () -> worker.LastGood |> Option.exists(fun (factor,polygons) ->
                factor=1.25 && not polygons.IsEmpty && polygons |> List.forall(fun p -> p.Outer.Head=List.last p.Outer)))
            while not(ready()) && timer.Elapsed.TotalSeconds<10. do
                host.Cycle()
                host.Frame()
                System.Threading.Thread.Yield() |> ignore
            if not(ready()) then failwith "Contour worker failed to publish the latest closed polygons."
            owner :> IDisposable
        elif kind="density" then
            let owner,bandwidth,worker=DensityGallery.mount host graph
            graph.Run(fun () -> bandwidth.Value<-3.)
            let timer=Stopwatch.StartNew()
            let ready ()=graph.Run(fun () -> worker.LastGood |> Option.exists(fun (width,grid) ->
                width=3. && DensityGrid.columns grid=32 && DensityGrid.rows grid=24 &&
                abs((DensityGrid.values grid |> List.choose id |> List.sum)-100000.)<0.001))
            while not(ready()) && timer.Elapsed.TotalSeconds<10. do
                host.Cycle()
                host.Frame()
                System.Threading.Thread.Yield() |> ignore
            if not(ready()) then failwith "Density worker failed to publish the latest bounded, mass-conserving grid."
            owner :> IDisposable
        elif kind="hierarchy-explorer" then
            let owner,tick,state,worker=HierarchyExplorer.mount host graph
            graph.Run(fun () -> state.Value<-{state.Value with Collapsed=set ["Signals"];Selected=set ["Scene"]})
            let timer=Stopwatch.StartNew()
            let mutable ready=false
            while not ready && timer.Elapsed.TotalSeconds<5. do
                host.Cycle()
                host.Frame()
                graph.Run(fun () -> tick timer.Elapsed.TotalMilliseconds)
                ready<-graph.Run(fun () -> worker.LastGood |> Option.exists(fun output -> output.Semantics |> List.exists(fun semantic -> semantic.DataIds=["State"]) |> not))
                if not ready then System.Threading.Thread.Yield() |> ignore
            if not ready then failwith "Hierarchy worker failed to publish the collapsed state."
            graph.Run(fun () -> tick (timer.Elapsed.TotalMilliseconds+500.))
            host.Cycle()
            host.Frame()
            let selectedBounds=graph.Run(fun () -> worker.LastGood.Value.HitTargets |> List.pick(fun hit ->
                match hit.DataIds,hit.Geometry with ["Scene"],Ranvier.D5.Charts.HitBounds bounds -> Some bounds | _ -> None))
            let clickX=int(Bounds.x selectedBounds+Bounds.width selectedBounds/2.)+8
            let clickY=int(Bounds.y selectedBounds+Bounds.height selectedBounds/2.)+8
            host.Click(clickX,clickY) |> ignore
            if graph.Run(fun () -> state.Value.Selected.Contains "Scene") then failwith "Native transition mark did not receive selection click."
            host.Click(clickX,clickY) |> ignore
            if graph.Run(fun () -> not(state.Value.Selected.Contains "Scene")) then failwith "Native transition selection did not toggle back."
            owner :> IDisposable
        elif kind="spatial" then
            let owner,selected=SpatialGallery.mount host graph
            host.Cycle()
            host.Frame()
            host.Click(48,118) |> ignore
            if graph.Run(fun () -> selected.Value<>Some 0) then failwith "Native nearest-site picking failed."
            owner :> IDisposable
        elif kind="linked" then
            let owner,_,_,_,_=Ranvier.D5.Examples.LinkedCharts.mount host graph
            owner :> IDisposable
        else
            let mounted=mount host graph (if kind="hierarchy" then HierarchyGallery.scene elif kind="tiling" then TilingGallery.scene elif kind="hierarchy-charts" then HierarchyChartsGallery.scene elif kind="planar" then PlanarGallery.scene elif kind="quadtree" then (fun () -> QuadtreeGallery.scene(Point.create 410. 265.)) elif kind="sankey" then (fun () -> SankeyGallery.scene 0.5) elif kind="chord" then (fun () -> ChordGallery.scene 0.5) elif kind="force" then (fun () -> ForceGallery.initial() |> Force.step 200 |> ForceGallery.scene) else fun () -> Scenes.scene 0.)
            {new IDisposable with member _.Dispose()=mounted.Dispose()}
    host.Cycle()
    host.Frame()
    let struct(width, height) = host.PaintSize
    let pixels = Array.zeroCreate<byte> (width * height * 4)
    host.Paint pixels
    // BMP stores bottom-up BGRA; Paint supplies top-down RGBA.
    for offset in 0 .. 4 .. pixels.Length - 4 do
        let red = pixels[offset]
        pixels[offset] <- pixels[offset + 2]
        pixels[offset + 2] <- red
    use stream = File.Create path
    use writer = new BinaryWriter(stream)
    writer.Write([|byte 'B'; byte 'M'|])
    writer.Write(54 + pixels.Length)
    writer.Write(0)
    writer.Write(54)
    writer.Write(40)
    writer.Write(width)
    writer.Write(height)
    writer.Write(1s)
    writer.Write(32s)
    writer.Write(0)
    writer.Write(pixels.Length)
    for _ in 1..4 do writer.Write(0)
    for row in height-1 .. -1 .. 0 do writer.Write(pixels, row * width * 4, width * 4)
    mounted.Dispose()

[<EntryPoint>]
let main args =
    if args.Length = 2 && args[0] = "--capture" then
        capture "foundation" args[1]
    elif args.Length=2 && args[0]="--capture-linked" then
        capture "linked" args[1]
    elif args.Length=2 && args[0]="--capture-hierarchy" then
        capture "hierarchy" args[1]
    elif args.Length=2 && args[0]="--capture-tiling" then
        capture "tiling" args[1]
    elif args.Length=2 && args[0]="--capture-hierarchy-charts" then
        capture "hierarchy-charts" args[1]
    elif args.Length=2 && args[0]="--capture-hierarchy-explorer" then
        capture "hierarchy-explorer" args[1]
    elif args.Length=2 && args[0]="--capture-planar" then
        capture "planar" args[1]
    elif args.Length=2 && args[0]="--capture-quadtree" then
        capture "quadtree" args[1]
    elif args.Length=2 && args[0]="--capture-spatial" then
        capture "spatial" args[1]
    elif args.Length=2 && args[0]="--capture-force" then
        capture "force" args[1]
    elif args.Length=2 && args[0]="--capture-network" then
        capture "network" args[1]
    elif args.Length=2 && args[0]="--capture-bubbles" then
        capture "bubbles" args[1]
    elif Array.contains "--network" args || Array.contains "--bubbles" args then
        use window=Window.Create("Ranvier.D5 interactive network",820,590)
        let owner,_,_=NetworkGallery.mount (Array.contains "--bubbles" args) window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif args.Length=2 && (args[0]="--capture-flow-sankey" || args[0]="--capture-flow-alluvial" || args[0]="--capture-flow-chord") then
        capture (args[0].Substring 10) args[1]
    elif args |> Array.exists(fun arg -> arg="--flow-sankey" || arg="--flow-alluvial" || arg="--flow-chord") then
        let kind=args |> Array.find(fun arg -> arg.StartsWith "--flow-") |> fun arg -> arg.Substring 7
        use window=Window.Create("Ranvier.D5 interactive flow recipes",820,590)
        let owner,_,_=FlowGallery.mount kind window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif args.Length=3 && args[0]="--capture-density-recipe" then
        capture ("density-recipe-"+args[1]) args[2]
    elif args.Length=2 && args[0]="--density-recipe" then
        use window=Window.Create("Ranvier.D5 density recipes",820,590)
        let owner,_,_,_=DensityRecipesGallery.mount args[1] window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif args.Length=2 && args[0]="--capture-reduction" then
        capture "reduction" args[1]
    elif Array.contains "--reduction" args then
        use window=Window.Create("Ranvier.D5 hexbin and sampling",820,590)
        let owner,_,_=ReductionGallery.mount window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif args.Length=2 && args[0]="--capture-contour" then
        capture "contour" args[1]
    elif Array.contains "--contour" args then
        use window=Window.Create("Ranvier.D5 contour topology",820,590)
        let owner,_,_=ContourGallery.mount window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif args.Length=2 && args[0]="--capture-density" then
        capture "density" args[1]
    elif Array.contains "--density" args then
        use window=Window.Create("Ranvier.D5 density foundations",820,590)
        let owner,_,_=DensityGallery.mount window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif args.Length=2 && args[0]="--capture-sankey" then
        capture "sankey" args[1]
    elif Array.contains "--sankey" args then
        use window=Window.Create("Ranvier.D5 Sankey foundations",820,590)
        let phase=window.Graph.Run(fun () -> Api.createSignal 0.)
        let scene=window.Graph.Run(fun () -> Reactive.derive (fun () -> phase.Value) SankeyGallery.scene)
        let mounted=mount window.Host window.Graph (fun () -> scene.Value)
        let timer=Stopwatch.StartNew()
        while window.Step() do window.Graph.Run(fun () -> phase.Value<-timer.Elapsed.TotalSeconds)
        mounted.Dispose()
    elif args.Length=2 && args[0]="--capture-chord" then
        capture "chord" args[1]
    elif Array.contains "--chord" args then
        use window=Window.Create("Ranvier.D5 chord foundations",820,590)
        let phase=window.Graph.Run(fun () -> Api.createSignal 0.)
        let scene=window.Graph.Run(fun () -> Reactive.derive (fun () -> phase.Value) ChordGallery.scene)
        let mounted=mount window.Host window.Graph (fun () -> scene.Value)
        let timer=Stopwatch.StartNew()
        while window.Step() do window.Graph.Run(fun () -> phase.Value<-timer.Elapsed.TotalSeconds)
        mounted.Dispose()
    elif Array.contains "--force" args then
        use window=Window.Create("Ranvier.D5 force state",820,590)
        let state=window.Graph.Run(fun () -> Api.createSignal(ForceGallery.initial()))
        let scene=window.Graph.Run(fun () -> Reactive.derive (fun () -> state.Value) ForceGallery.scene)
        let mounted=mount window.Host window.Graph (fun () -> scene.Value)
        while window.Step() do
            window.Graph.Run(fun () -> if not(Force.isFinished state.Value) then state.Value<-Force.step 1 state.Value)
        mounted.Dispose()
    elif Array.contains "--spatial" args then
        use window=Window.Create("Ranvier.D5 spatial picking",820,590)
        let owner,_=SpatialGallery.mount window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif Array.contains "--quadtree" args then
        use window=Window.Create("Ranvier.D5 quadtree",820,590)
        let query=window.Graph.Run(fun () -> Api.createSignal(Point.create 410. 265.))
        let scene=window.Graph.Run(fun () -> Reactive.derive (fun () -> query.Value) QuadtreeGallery.scene)
        let mounted=mount window.Host window.Graph (fun () -> scene.Value)
        let clock=Stopwatch.StartNew()
        while window.Step() do
            let t=clock.Elapsed.TotalSeconds
            window.Graph.Run(fun () -> query.Value<-Point.create (400.+180.*sin t) (265.+100.*cos(t*0.7)))
        mounted.Dispose()
    elif Array.contains "--planar" args then
        use window = Window.Create("Ranvier.D5 planar geometry",820,590)
        let mounted=mount window.Host window.Graph PlanarGallery.scene
        while window.Step() do ()
        mounted.Dispose()
    elif Array.contains "--hierarchy-charts" args then
        use window = Window.Create("Ranvier.D5 hierarchy chart family",820,590)
        let mounted=mount window.Host window.Graph HierarchyChartsGallery.scene
        while window.Step() do ()
        mounted.Dispose()
    elif Array.contains "--hierarchy-explorer" args then
        use window = Window.Create("Ranvier.D5 hierarchy explorer",820,590)
        let owner,tick,_,_=HierarchyExplorer.mount window.Host window.Graph
        let timer=Stopwatch.StartNew()
        while window.Step() do window.Graph.Run(fun () -> tick timer.Elapsed.TotalMilliseconds)
        owner.Dispose()
    elif Array.contains "--tiling" args then
        use window = Window.Create("Ranvier.D5 tiling layouts",820,590)
        let mounted=mount window.Host window.Graph TilingGallery.scene
        while window.Step() do ()
        mounted.Dispose()
    elif Array.contains "--hierarchy" args then
        use window = Window.Create("Ranvier.D5 hierarchy layouts",820,590)
        let mounted=mount window.Host window.Graph HierarchyGallery.scene
        while window.Step() do ()
        mounted.Dispose()
    elif Array.contains "--linked" args then
        use window = Window.Create("Ranvier.D5 linked selection",820,590)
        let owner,_,_,_,_=Ranvier.D5.Examples.LinkedCharts.mount window.Host window.Graph
        while window.Step() do ()
        owner.Dispose()
    elif Array.contains "--chart" args then
        use window = Window.Create("Ranvier.D5 reactive chart",820,590)
        let source,scene=window.Graph.Run ReactiveChart.create
        let mounted=mount window.Host window.Graph (fun () -> scene.Value)
        let clock=Stopwatch.StartNew()
        while window.Step() do
            window.Graph.Run(fun () -> source.Value <- source.Value |> List.map (fun point -> {point with Y=Some(sin(point.X+clock.Elapsed.TotalSeconds))}))
        mounted.Dispose()
    else
        use window = Window.Create("Ranvier.D5 — native F# visualization", 820, 590)
        let clock = Stopwatch.StartNew()
        let time = window.Graph.Run(fun () -> Api.createSignal 0.)
        let scene = window.Graph.Run(fun () -> Reactive.derive (fun () -> time.Value) Scenes.scene)
        let mounted = mount window.Host window.Graph (fun () -> scene.Value)
        while window.Step() do
            window.Graph.Run(fun () -> time.Value <- clock.Elapsed.TotalSeconds)
        mounted.Dispose()
    0

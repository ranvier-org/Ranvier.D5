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
        if kind="linked" then
            let owner,_,_,_,_=Ranvier.D5.Examples.LinkedCharts.mount host graph
            owner :> IDisposable
        else
            let mounted=mount host graph (if kind="hierarchy" then HierarchyGallery.scene elif kind="tiling" then TilingGallery.scene else fun () -> Scenes.scene 0.)
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

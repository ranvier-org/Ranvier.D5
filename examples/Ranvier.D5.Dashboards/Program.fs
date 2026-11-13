module Ranvier.D5.Dashboards.Program

open System
open System.Diagnostics
open System.IO
open Ranvier
open Ranvier.D5
open Ranvier.View.WebCore
open Ranvier.View.WebCore.Sdl
open Ranvier.D5.Dashboards.Kit

let rec private ids prefix nodes =
    nodes |> List.collect(function
        | Group(spec,children) -> (if spec.Key.StartsWith(prefix:string) then [spec.Key.Substring prefix.Length] else [])@ids prefix children
        | Leaf _ -> [])

/// <summary>Pure soak: runs each model for twenty simulated seconds, clicking every control and hovering every item it exposes,
/// and composes every frame's scene so duplicate keys or invalid geometry fail here rather than on screen.</summary>
let soak () =
    for index,dashboard in List.indexed Gallery.all do
        let mutable live=dashboard.Start()
        let mutable frames=0
        let compose () =
            let scene=try Shell.scene Gallery.all index None (live.View()) with error -> failwithf "%s: scene failed to compose: %s" dashboard.Id error.Message
            // Serialise every mark as the renderer does, so non-finite geometry fails here with its path.
            for path,mark in SceneNode.paths [] (Scene.nodes scene) do
                try
                    match Mark.geometry mark with
                    | PathGeometry geometry ->
                        let svg=Path.toSvg geometry
                        if svg.Contains "NaN" || svg.Contains "Infinity" then failwithf "non-finite path %s" (svg.Substring(0,min 200 svg.Length))
                        let numbers=Text.RegularExpressions.Regex.Matches(svg,@"-?[0-9.]+(E[-+]?[0-9]+)?") |> Seq.map(fun m -> float m.Value)
                        if numbers |> Seq.exists(fun v -> abs v>1e5) then failwithf "extreme coordinate in %s" (svg.Substring(0,min 300 svg.Length))
                    | TextGeometry _ -> ()
                    Transform.toSvg (Mark.transform mark) |> ignore
                    let style=Mark.style mark
                    for value in [style.Opacity;style.StrokeWidth;style.FontSize] do
                        if not(Double.IsFinite value) then failwith "non-finite style"
                with error -> failwithf "%s: mark %s failed to serialise: %s" dashboard.Id (String.concat "/" path) error.Message
        compose()
        for frame in 0..1199 do
            live<-live.Tick 16.7
            if frame%20=0 then
                let view=live.View()
                let controls=ids "b:" view
                let items=ids "h:" view
                if not controls.IsEmpty then live<-live.Send(Input.Click(controls[(frame/20)%controls.Length]))
                if not items.IsEmpty then
                    let item=items[(frame/20*7)%items.Length]
                    live<-live.Send(Input.Hover(Some item))
                    if frame%60=0 then live<-live.Send(Input.Pick item)
                live<-live.Send(Input.Pointer(Some(pt (40.+float((frame*37)%960)) (120.+float((frame*53)%660)))))
                for key in ["ArrowRight";"ArrowLeft";"ArrowUp";"ArrowDown";"+";"-"] do
                    if (frame/20)%6=List.findIndex ((=) key) ["ArrowRight";"ArrowLeft";"ArrowUp";"ArrowDown";"+";"-"] then live<-live.Send(Input.Key key)
            if frame%5=0 then
                compose()
                frames<-frames+1
        live<-live.Send(Input.Hover None)
        live<-live.Send(Input.Pointer None)
        compose()
        printfn "soak %-10s %d scenes composed" dashboard.Id frames

let private snapshot (host:WebCoreHost) =
    host.Cycle()
    host.Frame()
    let struct(width,height)=host.PaintSize
    let pixels=Array.zeroCreate<byte>(width*height*4)
    host.Paint pixels
    width,height,pixels

let private writeBitmap path (width,height,source:byte[]) =
    let pixels=Array.copy source
    // BMP stores bottom-up BGRA; Paint supplies top-down RGBA.
    for offset in 0 .. 4 .. pixels.Length-4 do
        let red=pixels[offset]
        pixels[offset]<-pixels[offset+2]
        pixels[offset+2]<-red
    use stream=File.Create path
    use writer=new BinaryWriter(stream)
    writer.Write([|byte 'B';byte 'M'|])
    writer.Write(54+pixels.Length)
    writer.Write(0)
    writer.Write(54)
    writer.Write(40)
    writer.Write(width:int)
    writer.Write(height:int)
    writer.Write(1s)
    writer.Write(32s)
    writer.Write(0)
    writer.Write(pixels.Length)
    for _ in 1..4 do writer.Write(0)
    for row in height-1 .. -1 .. 0 do writer.Write(pixels,row*width*4,width*4)

/// <summary>Content pixels (right of the sidebar) that differ between two paints, and distinct coarse colours in one paint.</summary>
let private changed (width,height,a:byte[]) (_,_,b:byte[]) =
    let mutable count=0
    for y in 0..height-1 do
        for x in int Shell.Sidebar..width-1 do
            let i=(y*width+x)*4
            if abs(int a[i]-int b[i])+abs(int a[i+1]-int b[i+1])+abs(int a[i+2]-int b[i+2])>24 then count<-count+1
    count
let private palette (width,height,pixels:byte[]) =
    seq { for y in 0..2..height-1 do
            for x in int Shell.Sidebar..2..width-1 do
                let i=(y*width+x)*4
                yield (int pixels[i]/32,int pixels[i+1]/32,int pixels[i+2]/32) } |> Set.ofSeq |> Set.count
let private painted (width,height,pixels:byte[]) =
    let r,g,b=Theme.background
    let mutable count=0
    for y in 0..height-1 do
        for x in int Shell.Sidebar..width-1 do
            let i=(y*width+x)*4
            if abs(int pixels[i]-r)+abs(int pixels[i+1]-g)+abs(int pixels[i+2]-b)>30 then count<-count+1
    float count/float(height*(width-int Shell.Sidebar))

/// <summary>Native checks for every dashboard: sidebar click navigation, continuous animation in painted pixels, native hover reaching a
/// data item, a native click on a control changing the view, and keyboard navigation; writes one bitmap per dashboard.</summary>
let capture directory (only:Set<string>) =
    Directory.CreateDirectory(directory:string) |> ignore
    use host=WebCoreHost.Create(int Shell.Width,int Shell.Height)
    use graph=new Graph()
    let app=Shell.mount host graph Gallery.all
    let run seconds = for _ in 1..int(seconds*1000./50.) do (app.Advance 50.;host.Cycle())
    for index,dashboard in List.indexed Gallery.all |> List.filter(fun (_,d) -> only.IsEmpty || only.Contains d.Id) do
        let y=100+index*64+28
        host.Click(110,y) |> ignore
        if app.Active()<>index then failwithf "Sidebar click at y=%d did not open %s." y dashboard.Id
        run 2.
        let first=snapshot host
        run 1.
        let second=snapshot host
        let moved=changed first second
        if moved<1500 then writeBitmap (Path.Combine(directory,dashboard.Id+"-frozen.bmp")) second
        if moved<1500 then failwithf "%s: only %d content pixels changed in one second; the dashboard is not animating." dashboard.Id moved
        let coverage=painted second
        if coverage<0.25 then failwithf "%s: only %.0f%% of the content area is painted." dashboard.Id (coverage*100.)
        let colours=palette second
        if colours<12 then failwithf "%s: only %d distinct colours painted." dashboard.Id colours
        // Native hover: sweep the content until a data item reports hover.
        let hoverPoint=
            seq { for y in 130..20..780 do
                    for x in 250..20..1270 -> x,y }
            |> Seq.tryFind(fun (x,y) -> host.Mouse(0,x,y,0,0,0) |> ignore; (app.Hovered()).IsSome)
        match hoverPoint with
        | None -> failwithf "%s: native hover never reached a data item." dashboard.Id
        | Some _ -> ()
        // Native clicks on toolbar controls: every distinct control is found by native hover, and at least one must change the view.
        let controls=
            seq { for x in 250..8..1270 -> x,97 }
            |> Seq.choose(fun (x,y) -> host.Mouse(0,x,y,0,0,0) |> ignore; app.Control() |> Option.filter(fun key -> not(key.StartsWith "b:nav:")) |> Option.map(fun key -> key,(x,y)))
            |> Seq.distinctBy fst |> Seq.toList
        if controls.IsEmpty then failwithf "%s: no native control found in the toolbar." dashboard.Id
        let effective=
            controls |> List.filter(fun (_,(x,y)) ->
                let before=(app.Current()).View()
                host.Click(x,y) |> ignore
                (app.Current()).View()<>before)
        if effective.IsEmpty then failwithf "%s: none of %d native controls changed the view." dashboard.Id controls.Length
        let controlPoint=sprintf "%d of %d controls changed the view" effective.Length controls.Length
        app.Reset()
        run 2.
        let x,y=hoverPoint.Value
        host.Mouse(0,x,y,0,0,0) |> ignore
        run 0.3
        let final=snapshot host
        writeBitmap (Path.Combine(directory,dashboard.Id+".bmp")) final
        printfn "capture %-10s moved=%d painted=%.0f%% colours=%d hover=%A controls: %s" dashboard.Id moved (coverage*100.) colours (app.Hovered()) controlPoint
    app.Select(Gallery.all.Length-1)
    app.Key "PageDown"
    if app.Active()<>0 then failwith "PageDown from the last dashboard did not wrap to the first."
    app.Key "2"
    if app.Active()<>1 then failwith "Digit key did not select the second dashboard."
    app.Owner.Dispose()

[<EntryPoint>]
let main args =
    match args with
    | [|"--soak"|] -> soak()
    | [|"--capture";directory|] -> soak();capture directory Set.empty
    | [|"--capture";directory;ids|] -> capture directory (Set.ofArray(ids.Split ','))
    | _ ->
        use window=Window.Create("Ranvier.D5 live dashboards",int Shell.Width,int Shell.Height)
        let app=Shell.mount window.Host window.Graph Gallery.all
        match Array.tryFindIndex ((=) "--dashboard") args with
        | Some index when index+1<args.Length ->
            Gallery.all |> List.tryFindIndex(fun dashboard -> dashboard.Id=args[index+1]) |> Option.iter app.Select
        | _ -> ()
        let clock=Stopwatch.StartNew()
        let mutable last=0.
        while window.Step() do
            let now=clock.Elapsed.TotalMilliseconds
            app.Advance(min 100. (now-last))
            last<-now
        app.Owner.Dispose()
    0

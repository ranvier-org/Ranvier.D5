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

/// <summary>Window sizes the soak lays every dashboard out in: the default window, a large monitor, the narrowest full sidebar,
/// a rail layout and a narrow portrait window.</summary>
let windows=[1280.,800.;1920.,1080.;1000.,720.;990.,900.;664.,860.]
let private contentOf (width,height) : Viewport = {Width=width-Shell.sidebarWidth width;Height=height}

/// <summary>Text that spills out of its card, or a card that spills out of the page, for one laid-out page. Text extents are estimated
/// from the font size (<c>Kit.textWidth</c>), so the tolerance absorbs a few pixels of font variance.</summary>
let overflow (viewport:Viewport) (page:Page) =
    let tolerance=4.
    let problems=ResizeArray()
    let rec walk (toPage:Point -> Point) (card:(string*Bounds) option) path nodes =
        for node in nodes do
            match node with
            | Group(spec,children) ->
                let local=Transform.apply spec.Transform
                let toPage'=local >> toPage
                let path=path@[spec.Key]
                let card=
                    let body=children |> List.tryPick(function Group(inner,_) when inner.Key="body" -> inner.Clip | _ -> None)
                    match body with
                    | Some key ->
                        let size=key.Substring(5).Split 'x'
                        let origin=toPage' (pt 0. 0.)
                        let bounds=box origin.X origin.Y (float size[0]) (float size[1])
                        if origin.X<Margin-tolerance || origin.X+bw bounds>viewport.Width-Margin+tolerance then
                            problems.Add(sprintf "card %s spans x %.0f–%.0f outside the %.0f-wide page" (String.concat "/" path) origin.X (origin.X+bw bounds) viewport.Width)
                        Some(String.concat "/" path,bounds)
                    | None -> card
                walk toPage' card path children
            | Leaf mark ->
                match Mark.geometry mark,card with
                | TextGeometry(point,value),Some(name,bounds) when (Mark.style mark).Opacity>0.05 && value<>"" ->
                    let p=toPage (Transform.apply (Mark.transform mark) point)
                    let size=(Mark.style mark).FontSize
                    let w=textWidth size value*(if (Mark.textStyle mark).Weight>=600 then 1.06 else 1.)
                    let left=match (Mark.textStyle mark).Anchor with "end" -> p.X-w | "middle" -> p.X-w/2. | _ -> p.X
                    let x0,x1=bx bounds,bx bounds+bw bounds
                    let y0,y1=by bounds,by bounds+bh bounds
                    if left<x0-tolerance || left+w>x1+tolerance || p.Y-size*0.75<y0-tolerance || p.Y+size*0.2>y1+tolerance then
                        problems.Add(sprintf "text \"%s\" (%s) at x %.0f–%.0f, y %.0f overflows card %s (x %.0f–%.0f, y %.0f–%.0f)"
                                        value (String.concat "/" (path@[Mark.key mark])) left (left+w) p.Y name x0 x1 y0 y1)
                | TextGeometry(point,value),None when (Mark.style mark).Opacity>0.05 && value<>"" && (path |> List.forall(fun key -> not(key.StartsWith "x:"))) ->
                    let p=toPage (Transform.apply (Mark.transform mark) point)
                    let w=textWidth (Mark.style mark).FontSize value
                    let left=match (Mark.textStyle mark).Anchor with "end" -> p.X-w | "middle" -> p.X-w/2. | _ -> p.X
                    if left < -tolerance || left+w>viewport.Width+tolerance then
                        problems.Add(sprintf "text \"%s\" (%s) at x %.0f–%.0f overflows the %.0f-wide page" value (String.concat "/" (path@[Mark.key mark])) left (left+w) viewport.Width)
                | _ -> ()
    walk id None [] page.Nodes
    List.ofSeq problems |> List.distinct

/// <summary>Pure soak: runs each model for twenty simulated seconds, clicking every control and hovering every item it exposes,
/// and composes scenes at several window sizes so duplicate keys, invalid geometry or text spilling out of a card fail here
/// rather than on screen.</summary>
let soak () =
    let failures=ResizeArray()
    for index,dashboard in List.indexed Gallery.all do
        let mutable live=dashboard.Start()
        let mutable frames=0
        let compose window check =
            let viewport=contentOf window
            live<-live.Send(Input.Viewport viewport)
            let page=live.View viewport
            let scene=try (let eased=Transitions.ease 16.7 (Transitions.Memory()) (Shell.nodes window Gallery.all index None page 0.) in Shell.compose window dashboard eased) with error -> failwithf "%s: scene failed to compose at %.0fx%.0f: %s" dashboard.Id viewport.Width viewport.Height error.Message
            if check then
                for problem in overflow viewport page do failures.Add(sprintf "%s at %.0fx%.0f: %s" dashboard.Id viewport.Width viewport.Height problem)
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
                with error -> failwithf "%s: mark %s failed to serialise at %.0fx%.0f: %s" dashboard.Id (String.concat "/" path) viewport.Width viewport.Height error.Message
        for window in windows do compose window true
        for frame in 0..1199 do
            live<-live.Tick 16.7
            if frame%20=0 then
                let view=(live.View (contentOf windows.Head)).Nodes
                let controls=ids "b:" view
                let items=ids "h:" view
                if not controls.IsEmpty then live<-live.Send(Input.Click(controls[(frame/20)%controls.Length]))
                if not items.IsEmpty then
                    let item=items[(frame/20*7)%items.Length]
                    live<-live.Send(Input.Hover(Some item))
                    if frame%60=0 then live<-live.Send(Input.Pick item)
                live<-live.Send(Input.Pointer(Some(pt (40.+float((frame*37)%960)) (120.+float((frame*53)%660)))))
                if frame%40=0 then live<-live.Send(Input.Drag(float(frame%7)-3.,float(frame%5)-2.))
                for key in ["ArrowRight";"ArrowLeft";"ArrowUp";"ArrowDown";"+";"-"] do
                    if (frame/20)%6=List.findIndex ((=) key) ["ArrowRight";"ArrowLeft";"ArrowUp";"ArrowDown";"+";"-"] then live<-live.Send(Input.Key key)
            if frame%5=0 then
                compose windows[(frame/5)%windows.Length] (frame%100=0)
                frames<-frames+1
        live<-live.Send(Input.Hover None)
        live<-live.Send(Input.Pointer None)
        for window in windows do compose window true
        printfn "soak %-10s %d scenes composed" dashboard.Id frames
    let distinct=failures |> Seq.distinct |> Seq.toList
    if not distinct.IsEmpty then
        for failure in distinct |> List.truncate 60 do eprintfn "  %s" failure
        failwithf "%d layout problems (first %d shown)." distinct.Length (min 60 distinct.Length)

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
let private side=int(Shell.sidebarWidth Shell.Width)
let private changed (width,height,a:byte[]) (_,_,b:byte[]) =
    let mutable count=0
    for y in 0..height-1 do
        for x in side..width-1 do
            let i=(y*width+x)*4
            if abs(int a[i]-int b[i])+abs(int a[i+1]-int b[i+1])+abs(int a[i+2]-int b[i+2])>24 then count<-count+1
    count
let private palette (width,height,pixels:byte[]) =
    seq { for y in 0..2..height-1 do
            for x in side..2..width-1 do
                let i=(y*width+x)*4
                yield (int pixels[i]/32,int pixels[i+1]/32,int pixels[i+2]/32) } |> Set.ofSeq |> Set.count
let private painted (width,height,pixels:byte[]) =
    let r,g,b=Theme.background
    let mutable count=0
    for y in 0..height-1 do
        for x in side..width-1 do
            let i=(y*width+x)*4
            if abs(int pixels[i]-r)+abs(int pixels[i+1]-g)+abs(int pixels[i+2]-b)>30 then count<-count+1
    float count/float(height*(width-side))

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
            seq { for y in [97;115;133;151] do
                    for x in 250..8..1270 -> x,y }
            |> Seq.choose(fun (x,y) -> host.Mouse(0,x,y,0,0,0) |> ignore; app.Control() |> Option.filter(fun key -> not(key.StartsWith "b:nav:")) |> Option.map(fun key -> key,(x,y)))
            |> Seq.distinctBy fst |> Seq.toList
        if controls.IsEmpty then failwithf "%s: no native control found in the toolbar." dashboard.Id
        let effective=
            controls |> List.filter(fun (_,(x,y)) ->
                let before=app.Page()
                host.Click(x,y) |> ignore
                app.Page()<>before)
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
    // Dragging: a press on empty globe space turns the flight tracker's globe, and releasing the button ends the drag.
    match Gallery.all |> List.tryFindIndex(fun d -> d.Id="flights") with
    | Some index when only.IsEmpty || only.Contains "flights" ->
        app.Select index
        run 0.5
        let x,y=
            seq { for y in 300..10..600 do
                    for x in 450..10..700 -> x,y }
            |> Seq.find(fun (x,y) -> host.Mouse(0,x,y,0,0,0) |> ignore; (app.Hovered()).IsNone && (app.Control()).IsNone)
        let before=app.Page()
        host.Mouse(1,x,y,0,1,0) |> ignore
        if not(app.Dragging()) then failwith "flights: a press on empty globe space did not start a drag."
        for k in 1..6 do
            host.Mouse(0,x+k*12,y+k*6,0,0,0) |> ignore
            run 0.05
        host.Mouse(2,x+72,y+36,0,1,0) |> ignore
        if app.Dragging() then failwith "flights: releasing the button did not end the drag."
        if app.Page()=before then failwith "flights: dragging did not turn the globe."
        printfn "drag    flights released after turning the globe"
    | _ -> ()
    // Cursor: controls show a pointer and empty page space the default arrow, so controls read as buttons rather than text.
    app.Select 0
    run 0.5
    host.Mouse(0,700,792,0,0,0) |> ignore
    host.Cycle()
    let plain=host.Cursor
    host.Mouse(0,110,128,0,0,0) |> ignore
    host.Cycle()
    if host.Cursor=plain then failwithf "A sidebar button shows the same cursor (%d) as empty space." plain
    // Resizing re-lays every page for the new window, and a page taller than the window scrolls with the wheel.
    for width,height in [1680,1000;980,760;720,900] do
        host.Resize(width,height)
        run 0.2
        if app.Window()<>(float width,float height) then failwithf "After resizing to %dx%d the shell is laid out for %A." width height (app.Window())
        for index,dashboard in List.indexed Gallery.all |> List.filter(fun (_,d) -> only.IsEmpty || only.Contains d.Id) do
            app.Select index
            run 1.
            writeBitmap (Path.Combine(directory,sprintf "%s-%dx%d.bmp" dashboard.Id width height)) (snapshot host)
            let page=app.Page()
            if page.Height>float height+1. then
                host.Wheel(width/2,height/2,0.f,240.f,0) |> ignore
                host.Cycle()
                if app.Scroll()<=0. then failwithf "%s at %dx%d: the wheel did not scroll a %.0f-pixel page." dashboard.Id width height page.Height
            printfn "resize  %-10s %dx%d page height %.0f" dashboard.Id width height page.Height
    host.Resize(int Shell.Width,int Shell.Height)
    run 0.2
    app.Select(Gallery.all.Length-1)
    app.Key "PageDown"
    if app.Active()<>0 then failwith "PageDown from the last dashboard did not wrap to the first."
    app.Key "2"
    if app.Active()<>1 then failwith "Digit key did not select the second dashboard."
    app.Owner.Dispose()

#nowarn "9"
/// <summary>Whether the left mouse button is held, from SDL's global mouse state.</summary>
let private leftButtonHeld () =
    let flags=SDL.SDL3.SDL_GetGlobalMouseState(NativeInterop.NativePtr.nullPtr,NativeInterop.NativePtr.nullPtr)
    flags.HasFlag SDL.SDL_MouseButtonFlags.SDL_BUTTON_LMASK

[<EntryPoint>]
let main args =
    match args with
    | [|"--soak"|] -> soak()
    | [|"--frame-cost"|] ->
        // Frame cost per dashboard: model tick, view, transitions and scene composition, without native rendering.
        for index,dashboard in List.indexed Gallery.all do
            let mutable live=dashboard.Start()
            let memory=Transitions.Memory()
            let window=Shell.Width,Shell.Height
            let viewport:Viewport={Width=Shell.Width-Shell.sidebarWidth Shell.Width;Height=Shell.Height}
            let clock=Stopwatch()
            let mutable view,ease,compose=0.,0.,0.
            let mutable marks=0
            for frame in 0..299 do
                live<-live.Tick 16.7
                if frame=150 then (view<-0.;ease<-0.;compose<-0.)
                clock.Restart()
                let nodes=Shell.nodes window Gallery.all index None (live.View viewport) 0.
                view<-view+clock.Elapsed.TotalMilliseconds
                clock.Restart()
                let eased=Transitions.ease 16.7 memory nodes
                ease<-ease+clock.Elapsed.TotalMilliseconds
                clock.Restart()
                let scene=Shell.compose window dashboard eased
                compose<-compose+clock.Elapsed.TotalMilliseconds
                marks<-List.length (Scene.marks scene)
            printfn "%-10s marks=%5d view=%6.2f ms ease=%6.2f ms compose=%6.2f ms per frame" dashboard.Id marks (view/150.) (ease/150.) (compose/150.)
    | [|"--native-probe"|] ->
        use host=WebCoreHost.Create(1280,800)
        use graph=new Graph()
        let app=Shell.mount host graph Gallery.all
        let clock=Stopwatch()
        for index,dashboard in List.indexed Gallery.all do
            app.Select index
            let struct(w,h)=host.PaintSize
            let pixels=Array.zeroCreate<byte>(w*h*4)
            let frame()=app.Advance 16.7;host.Cycle();host.Frame();host.Paint pixels
            for _ in 1..60 do frame()
            clock.Restart()
            for _ in 1..120 do frame()
            let idle=clock.Elapsed.TotalMilliseconds/120.
            // Sweep the pointer across the content so hover emphasis changes many times.
            clock.Restart()
            for k in 0..119 do
                host.Mouse(0,300+(k*7)%900,200+(k*13)%500,0,0,0) |> ignore
                frame()
            printfn "%-10s idle %.2f ms  hovering %.2f ms per painted frame" dashboard.Id idle (clock.Elapsed.TotalMilliseconds/120.)
        app.Owner.Dispose()
    | [|"--capture";directory|] -> soak();capture directory Set.empty
    | [|"--capture";directory;ids|] -> capture directory (Set.ofArray(ids.Split ','))
    | [|"--bench";id|] ->
        // Real window: three seconds idle, then three seconds with a synthetic pointer sweeping the content; reports frame times.
        use window=Window.Create("Ranvier.D5 live dashboards (bench)",int Shell.Width,int Shell.Height)
        let app=Shell.mount window.Host window.Graph Gallery.all
        Gallery.all |> List.tryFindIndex(fun d -> d.Id=id) |> Option.iter app.Select
        let clock=Stopwatch.StartNew()
        let mutable last=0.
        let idle,hover=ResizeArray(),ResizeArray()
        let mutable k=0
        while window.Step() && clock.Elapsed.TotalSeconds<7. do
            let now=clock.Elapsed.TotalMilliseconds
            app.Advance(min 100. (now-last))
            if now>4000. then
                k<-k+1
                window.Host.Mouse(0,300+(k*7)%900,200+(k*13)%500,0,0,0) |> ignore
            if now>1000. && now<=4000. then idle.Add(now-last) elif now>4000. then hover.Add(now-last)
            last<-now
        let report (xs:ResizeArray<float>) = if xs.Count=0 then "n/a" else sprintf "%.1f ms avg, %.1f ms p95 over %d frames" (Seq.average xs) (Seq.sort xs |> Seq.item (xs.Count*95/100)) xs.Count
        printfn "bench %s idle: %s" id (report idle)
        printfn "bench %s hover: %s" id (report hover)
        app.Owner.Dispose()
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
            // The native button state is the source of truth for a drag: if the left button is up, the drag is over, even when
            // no release event reached the page.
            if app.Dragging() && not(leftButtonHeld()) then app.Release()
            let now=clock.Elapsed.TotalMilliseconds
            app.Advance(min 100. (now-last))
            last<-now
        app.Owner.Dispose()
    0

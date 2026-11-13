/// <summary>The native gallery shell: a sidebar of dashboards, one retained scene and native input routed to the active dashboard.</summary>
module Ranvier.D5.Dashboards.Shell

open System
open Ranvier
open Ranvier.D5
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore
open Ranvier.D5.Dashboards.Kit

/// <summary>The window size the gallery opens at.</summary>
let Width,Height=1280.,800.

/// <summary>Sidebar width for a window width: a full sidebar, or a numbered rail below 1000 pixels.</summary>
let sidebarWidth windowWidth = if windowWidth<1000. then 64. else 232.
/// <summary>Height of one sidebar entry: 64 pixels, compressed in short windows.</summary>
let entryHeight windowHeight = clamp 40. 64. ((windowHeight-150.)/10.)

/// <summary>Pure sidebar for a window size: brand, one navigation entry per dashboard and the keyboard hint, or a numbered rail.</summary>
let sidebar (windowWidth:float,windowHeight:float) (dashboards:Dashboard list) active (hovered:string option) =
    let width=sidebarWidth windowWidth
    let rail=width<100.
    let step=entryHeight windowHeight
    let entry index (dashboard:Dashboard) =
        let selected=index=active
        let key="b:nav:"+dashboard.Id
        let over=hovered=Some key
        let h=step-8.
        at key (if rail then 8. else 12.) (100.+float index*step)
            ([Leaf(rrect "bg" 0. 0. (width-(if rail then 16. else 24.)) h 8. (if selected then Theme.raised elif over then (23,33,54) else Theme.panel)
                   |> Mark.withStroke (paint (if selected then Theme.edge else Theme.panel)) 1.)
              Leaf(rrect "marker" 0. (h/2.-15.) 3. 30. 1.5 (if selected then Theme.accent else Theme.panel))
              Leaf(label "index" (if rail then (width-16.)/2. else 22.) (h/2.+6.) (if rail then 14. else 12.) (if selected then Theme.accent else Theme.faint) 700 "middle" (string((index+1)%10)))]
             @ (if rail then [] else
                 [Leaf(label "name" 40. (if h>=50. then 25. else h/2.+5.) 13.5 (if selected || over then Theme.text else (190,200,216)) 600 "start" dashboard.Title)]
                 @ (if h>=50. then [Leaf(text "summary" 40. 43. 10.5 Theme.faint dashboard.Summary)] else [])))
    group "sidebar"
        ([Leaf(rect "bg" 0. 0. width windowHeight Theme.panel)
          Leaf(rect "edge" (width-1.) 0. 1. windowHeight Theme.edge)
          Leaf(rrect "logo" (if rail then 19. else 20.) 22. 26. 26. 6. Theme.accent)
          Leaf(label "logo-mark" (if rail then 32. else 33.) 40. 14. Theme.background 800 "middle" "D5")]
         @ (if rail then [] else
             [Leaf(label "brand" 56. 34. 16. Theme.text 800 "start" "Ranvier.D5")
              Leaf(text "tagline" 56. 50. 11. Theme.muted "Live dashboards")
              Leaf(label "section" 20. 86. 10. Theme.faint 700 "start" "GALLERY")])
         @ List.mapi entry dashboards
         @ (if rail || windowHeight<100.+10.*step+30. then [] else [Leaf(text "keys" 20. (windowHeight-18.) 10.5 Theme.faint "PgUp / PgDn or 1–0 to switch")]))

/// <summary>Outlines the background of the hovered control so every button shows native hover feedback.</summary>
let rec highlight (hovered:string option) nodes =
    match hovered with
    | None -> nodes
    | Some key ->
        nodes |> List.map(function
            | Group(spec,children) when spec.Key=key ->
                Group(spec,children |> List.map(function Leaf mark when Mark.key mark="bg" -> Leaf(Mark.withStroke (paint Theme.accent) 1.5 mark) | node -> node))
            | Group(spec,children) -> Group(spec,highlight hovered children)
            | node -> node)

/// <summary>One rectangular clip definition for every distinct <c>clip-WxH</c> key used in the nodes.</summary>
let clips (nodes:SceneNode list) =
    let rec keys nodes =
        nodes |> List.collect(function
            | Group(spec,children) -> (match spec.Clip with Some key when key.StartsWith "clip-" -> [key] | _ -> [])@keys children
            | Leaf _ -> [])
    keys nodes |> List.distinct |> List.map(fun key ->
        let size=key.Substring(5).Split 'x'
        RectClip(key,box 0. 0. (float size[0]) (float size[1])))

/// <summary>Largest scroll offset for a page in a window.</summary>
let maxScroll (windowHeight:float) (page:Page) = max 0. (page.Height-windowHeight)

/// <summary>The window's top-level nodes for a window size, the active dashboard, its laid-out page and the page's scroll offset.</summary>
let nodes (windowWidth:float,windowHeight:float) (dashboards:Dashboard list) active hovered (page:Page) scroll =
    let dashboard=List.item active dashboards
    let side=sidebarWidth windowWidth
    let content=at ("content:"+dashboard.Id) side (-scroll) (highlight hovered page.Nodes)
    let limit=maxScroll windowHeight page
    let scrollbar=
        if limit<=0. then [] else
        let thumb=max 40. (windowHeight*windowHeight/page.Height)
        [Leaf(rrect "scrollbar" (windowWidth-7.) (4.+(windowHeight-8.-thumb)*scroll/limit) 4. thumb 2. Theme.edge)]
    [Leaf(rect "page" 0. 0. windowWidth windowHeight Theme.background)
     content
     sidebar (windowWidth,windowHeight) dashboards active hovered]
    @ scrollbar

/// <summary>Composes window nodes into a scene with the clip definitions they use.</summary>
let compose (windowWidth:float,windowHeight:float) (dashboard:Dashboard) (nodes:SceneNode list) =
    Scene.compose {Width=windowWidth;Height=windowHeight} ("Ranvier.D5 dashboards: "+dashboard.Title) (clips nodes) nodes

/// <summary>The whole window scene, without transitions.</summary>
let scene window (dashboards:Dashboard list) active hovered page scroll =
    compose window (List.item active dashboards) (nodes window dashboards active hovered page scroll)

/// <summary>An application-surface preflight in the spirit of Tailwind's base layer: nothing but a writable field is selectable or
/// shows a text cursor, text never
/// takes the pointer (so a label over a chart or a button hits whatever is underneath), controls (<c>b:</c>) and data items
/// (<c>h:</c>) show a pointer cursor, and overlays (<c>x:</c>) let the pointer through.</summary>
let private stylesheet =
    String.concat ""
        ["*{user-select:none;-webkit-user-select:none;-webkit-touch-callout:none;-webkit-tap-highlight-color:transparent;cursor:default}"
         "input,textarea,[contenteditable]{user-select:text;-webkit-user-select:text;cursor:text}"
         "svg{outline:none}"
         "text{pointer-events:none}"
         "[data-d5-group^='b:'],[data-d5-group^='b:'] *,[data-d5-group^='h:'],[data-d5-group^='h:'] *{cursor:pointer}"
         "[data-d5-group^='x:']{pointer-events:none}"]

/// <summary>A mounted gallery: its owner, navigation, the frame step, input entry points and readers used by native checks.</summary>
type App =
    { Owner:IDisposable
      Active:unit -> int
      Select:int -> unit
      Advance:float -> unit
      Reset:unit -> unit
      Key:string -> unit
      Send:Input -> unit
      Current:unit -> Live
      Hovered:unit -> string option
      Control:unit -> string option
      /// <summary>The window size the shell is laid out for.</summary>
      Window:unit -> float*float
      /// <summary>The content viewport the active dashboard is laid out in.</summary>
      Content:unit -> Viewport
      Scroll:unit -> float
      /// <summary>The active dashboard's page at the current size.</summary>
      Page:unit -> Page }

/// <summary>Mounts the gallery into the host. Each dashboard keeps its own immutable <c>Live</c> in a signal; only the active one
/// advances. The layout follows the host viewport, the page scrolls with the wheel, and a press on empty space drags.</summary>
let mount (host:WebCoreHost) (graph:Graph) (dashboards:Dashboard list) =
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let count=List.length dashboards
        let active=Api.createSignal 0
        let lives=dashboards |> List.map(fun dashboard -> Api.createSignal(dashboard.Start())) |> List.toArray
        let control=Api.createSignal None
        let item=Api.createSignal None
        let window=Api.createSignal (Width,Height)
        let scroll=Api.createSignal 0.
        let content()=let w,h=window.Value in ({Width=w-sidebarWidth w;Height=h}:Viewport)
        let page()=lives[active.Value].Value.View(content())
        let scrollTo value =
            let next=clamp 0. (maxScroll (snd window.Value) (page())) value
            if next<>scroll.Value then scroll.Value<-next
        let send input =
            let index=active.Value
            lives[index].Value<-lives[index].Value.Send input
        let select index =
            let index=((index%count)+count)%count
            if index<>active.Value then
                send(Input.Hover None)
                send(Input.Pointer None)
                item.Value<-None
                control.Value<-None
                active.Value<-index
                scroll.Value<-0.
        let key (name:string) =
            match name with
            | "PageDown" | "]" -> select(active.Value+1)
            | "PageUp" | "[" -> select(active.Value-1)
            | digit when digit.Length=1 && Char.IsDigit digit[0] -> select((int digit[0]-int '0'+9)%10)
            | other -> send(Input.Key other)
        let svg:Ranvier.Dom.SVGGraphicsElement option ref=ref None
        let dragging:Point option ref=ref None
        let plot (event:Ranvier.Dom.Event) =
            match event,svg.Value with
            | (:? Ranvier.Dom.PointerEvent as pointer),Some element -> InputAdapter.clientToPlot element (Point.create pointer.clientX pointer.clientY)
            | _ -> None
        let dispatch (path:string list) (name:string) (event:Ranvier.Dom.Event) =
            graph.Run(fun () -> Api.untrack(fun () ->
                let button=path |> List.tryFindBack(fun key -> key.StartsWith "b:")
                let picked=path |> List.tryFindBack(fun key -> key.StartsWith "h:") |> Option.map(fun key -> key.Substring 2)
                let side=sidebarWidth (fst window.Value)
                match name with
                | "click" ->
                    match button with
                    | Some key when key.StartsWith "b:nav:" -> dashboards |> List.tryFindIndex(fun dashboard -> "b:nav:"+dashboard.Id=key) |> Option.iter select
                    | Some key -> send(Input.Click(key.Substring 2))
                    | None -> picked |> Option.iter(fun id -> send(Input.Pick id))
                | "pointerdown" ->
                    // A press on empty content (no control, no data item) starts a drag.
                    dragging.Value<-plot event |> Option.filter(fun point -> button.IsNone && picked.IsNone && point.X>=side)
                | "pointerup" | "pointercancel" -> dragging.Value<-None
                | "pointermove" ->
                    if control.Value<>button then control.Value<-button
                    if item.Value<>picked then
                        item.Value<-picked
                        send(Input.Hover picked)
                    plot event |> Option.iter(fun point ->
                        match event,dragging.Value with
                        | (:? Ranvier.Dom.PointerEvent as pointer),Some start when pointer.buttons<>0us ->
                            send(Input.Drag(point.X-start.X,point.Y-start.Y))
                            dragging.Value<-Some point
                        | _ -> dragging.Value<-None
                        send(Input.Pointer(if point.X>=side then Some(Point.create (point.X-side) (point.Y+scroll.Value)) else None)))
                | "wheel" ->
                    match event with
                    | :? Ranvier.Dom.WheelEvent as wheel -> scrollTo(scroll.Value+wheel.deltaY)
                    | _ -> ()
                | _ -> ()))
        let style=host.Document.createElement "style"
        style.textContent<-stylesheet
        host.Document.head.appendChild style |> ignore
        let container=host.Document.createElement "div"
        container.setAttribute("style","width:100vw;height:100vh;overflow:hidden")
        host.Document.body.setAttribute("style","margin:0;overflow:hidden;background:#0a0f1c")
        host.Document.body.appendChild container |> ignore
        // Each frame eases every mark's style toward the view (Kit.Transitions); the eased scene is what the window shows.
        let memory=ref Transitions.empty
        let raw()=nodes window.Value dashboards active.Value control.Value (page()) scroll.Value
        let shown=Api.createSignal(compose window.Value dashboards.Head (raw()))
        let present dt =
            let eased,next=Transitions.ease dt memory.Value (raw())
            memory.Value<-next
            shown.Value<-compose window.Value (List.item active.Value dashboards) eased
        let view()=shown.Value
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render (Renderer.createScoped dispatch) view) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove();style.remove())
        host.Cycle()
        let element=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        svg.Value<-Some element
        element.setAttribute("tabindex","0")
        let onKey (event:Ranvier.Dom.Event) =
            match event with
            | :? Ranvier.Dom.KeyboardEvent as keyboard ->
                if List.contains keyboard.key ["ArrowLeft";"ArrowRight";"ArrowUp";"ArrowDown";" ";"PageUp";"PageDown"] then event.preventDefault()
                graph.Run(fun () -> Api.untrack(fun () -> key keyboard.key))
            | _ -> ()
        host.Document.addEventListener("keydown",onKey)
        Api.onCleanup(fun () -> host.Document.removeEventListener("keydown",onKey))
        let run f=graph.Run(fun () -> Api.untrack f)
        // The layout follows the container, which fills the host viewport.
        let measure()=
            let size=float container.clientWidth,float container.clientHeight
            if fst size>=320. && snd size>=240. && size<>window.Value then
                window.Value<-size
                scrollTo scroll.Value
        {Owner=owner :> IDisposable
         Active=(fun () -> run(fun () -> active.Value))
         Select=(fun index -> run(fun () -> select index))
         Reset=(fun () -> run(fun () -> let index=active.Value in lives[index].Value<-(List.item index dashboards).Start()))
         Advance=(fun dt -> run(fun () -> measure();let index=active.Value in lives[index].Value<-lives[index].Value.Tick dt;scrollTo scroll.Value;present dt))
         Key=(fun name -> run(fun () -> key name))
         Send=(fun input -> run(fun () -> send input))
         Current=(fun () -> run(fun () -> lives[active.Value].Value))
         Hovered=(fun () -> run(fun () -> item.Value))
         Control=(fun () -> run(fun () -> control.Value))
         Window=(fun () -> run(fun () -> window.Value))
         Content=(fun () -> run content)
         Scroll=(fun () -> run(fun () -> scroll.Value))
         Page=(fun () -> run page)}))

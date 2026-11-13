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

let Width,Height=1280.,800.
let Sidebar=232.

/// <summary>Pure sidebar: brand, one navigation entry per dashboard and the keyboard hint.</summary>
let sidebar (dashboards:Dashboard list) active (hovered:string option) =
    let entry index (dashboard:Dashboard) =
        let selected=index=active
        let key="b:nav:"+dashboard.Id
        let over=hovered=Some key
        at key 12. (100.+float index*64.)
            [Leaf(rrect "bg" 0. 0. (Sidebar-24.) 56. 8. (if selected then Theme.raised elif over then (23,33,54) else Theme.panel)
                  |> Mark.withStroke (paint (if selected then Theme.edge else Theme.panel)) 1.)
             Leaf(rrect "marker" 0. 13. 3. 30. 1.5 (if selected then Theme.accent else Theme.panel))
             Leaf(label "index" 22. 34. 12. (if selected then Theme.accent else Theme.faint) 700 "middle" (string((index+1)%10)))
             Leaf(label "name" 40. 25. 13.5 (if selected || over then Theme.text else (190,200,216)) 600 "start" dashboard.Title)
             Leaf(text "summary" 40. 43. 10.5 Theme.faint dashboard.Summary)]
    group "sidebar"
        ([Leaf(rect "bg" 0. 0. Sidebar Height Theme.panel)
          Leaf(rect "edge" (Sidebar-1.) 0. 1. Height Theme.edge)
          Leaf(rrect "logo" 20. 22. 26. 26. 6. Theme.accent)
          Leaf(label "logo-mark" 33. 40. 14. Theme.background 800 "middle" "D5")
          Leaf(label "brand" 56. 34. 16. Theme.text 800 "start" "Ranvier.D5")
          Leaf(text "tagline" 56. 50. 11. Theme.muted "Live dashboards")
          Leaf(label "section" 20. 86. 10. Theme.faint 700 "start" "GALLERY")]
         @ List.mapi entry dashboards
         @ [Leaf(text "keys" 20. 782. 10.5 Theme.faint "PgUp / PgDn or 1–0 to switch")])

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

/// <summary>The whole window scene for an active dashboard and its current content nodes.</summary>
let scene (dashboards:Dashboard list) active hovered (content:SceneNode list) =
    let dashboard=List.item active dashboards
    Scene.compose {Width=Width;Height=Height} ("Ranvier.D5 dashboards: "+dashboard.Title) []
        [Leaf(rect "page" 0. 0. Width Height Theme.background)
         sidebar dashboards active hovered
         at ("content:"+dashboard.Id) Sidebar 0. (highlight hovered content)]

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
      Control:unit -> string option }

/// <summary>Mounts the gallery into the host. Each dashboard keeps its own immutable <c>Live</c> in a signal; only the active one advances.</summary>
let mount (host:WebCoreHost) (graph:Graph) (dashboards:Dashboard list) =
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let count=List.length dashboards
        let active=Api.createSignal 0
        let lives=dashboards |> List.map(fun dashboard -> Api.createSignal(dashboard.Start())) |> List.toArray
        let control=Api.createSignal None
        let item=Api.createSignal None
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
        let key (name:string) =
            match name with
            | "PageDown" | "]" -> select(active.Value+1)
            | "PageUp" | "[" -> select(active.Value-1)
            | digit when digit.Length=1 && Char.IsDigit digit[0] -> select((int digit[0]-int '0'+9)%10)
            | other -> send(Input.Key other)
        let svg:Ranvier.Dom.SVGGraphicsElement option ref=ref None
        let dispatch (path:string list) (name:string) (event:Ranvier.Dom.Event) =
            graph.Run(fun () -> Api.untrack(fun () ->
                let button=path |> List.tryFindBack(fun key -> key.StartsWith "b:")
                let picked=path |> List.tryFindBack(fun key -> key.StartsWith "h:") |> Option.map(fun key -> key.Substring 2)
                match name with
                | "click" ->
                    match button with
                    | Some key when key.StartsWith "b:nav:" -> dashboards |> List.tryFindIndex(fun dashboard -> "b:nav:"+dashboard.Id=key) |> Option.iter select
                    | Some key -> send(Input.Click(key.Substring 2))
                    | None -> picked |> Option.iter(fun id -> send(Input.Pick id))
                | "pointermove" ->
                    if control.Value<>button then control.Value<-button
                    if item.Value<>picked then
                        item.Value<-picked
                        send(Input.Hover picked)
                    match event,svg.Value with
                    | (:? Ranvier.Dom.PointerEvent as pointer),Some element ->
                        InputAdapter.clientToPlot element (Point.create pointer.clientX pointer.clientY) |> Option.iter(fun point ->
                            send(Input.Pointer(if point.X>=Sidebar then Some(Point.create (point.X-Sidebar) point.Y) else None)))
                    | _ -> ()
                | _ -> ()))
        let container=host.Document.createElement "div"
        host.Document.body.setAttribute("style","margin:0;overflow:hidden;background:#0a0f1c")
        host.Document.body.appendChild container |> ignore
        let view()=scene dashboards active.Value control.Value (lives[active.Value].Value.View())
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render (Renderer.createScoped dispatch) view) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
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
        {Owner=owner :> IDisposable
         Active=(fun () -> run(fun () -> active.Value))
         Select=(fun index -> run(fun () -> select index))
         Reset=(fun () -> run(fun () -> let index=active.Value in lives[index].Value<-(List.item index dashboards).Start()))
         Advance=(fun dt -> run(fun () -> let index=active.Value in lives[index].Value<-lives[index].Value.Tick dt))
         Key=(fun name -> run(fun () -> key name))
         Send=(fun input -> run(fun () -> send input))
         Current=(fun () -> run(fun () -> lives[active.Value].Value))
         Hovered=(fun () -> run(fun () -> item.Value))
         Control=(fun () -> run(fun () -> control.Value))}))

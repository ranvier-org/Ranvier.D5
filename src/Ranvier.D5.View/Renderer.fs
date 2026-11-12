namespace Ranvier.D5.View

open System
open System.Globalization
open System.Text
open Ranvier.D5
open Ranvier.View
open Ranvier.View.Sugar

/// <summary>Retained keyed SVG components that render D5 scenes through Ranvier.View.</summary>
[<RequireQualifiedAccess>]
module Renderer =
    let private number (value: float) = value.ToString("G17", CultureInfo.InvariantCulture)

    type private RenderMark = { Mark:Mark; Namespace:string; Path:string list }
    type private RenderNode = { Node:SceneNode; Namespace:string; Path:string list }
    type private Appearance = { Fill:string option; Stroke:string option; StrokeWidth:string option; Opacity:string option; FillOpacity:string option; StrokeOpacity:string option; FontSize:string option; Family:string option; Weight:string option; Anchor:string option; Baseline:string option }
    type private RenderRow = { Read:unit -> RenderMark; Appearance:unit -> Appearance; Transform:unit -> string option }
    let private definitionId prefix (key:string) = prefix + (key |> Seq.map(fun code -> (int code).ToString("X4",CultureInfo.InvariantCulture)) |> String.concat "")

    let private row (dispatch: string list -> string -> Ranvier.Dom.Event -> unit) =
        Component.define (fun _ (read: unit -> RenderMark) ->
            let mutable cached=ValueNone
            let mutable cachedTransform=ValueNone
            let transform () =
                let current=Mark.transform(read().Mark)
                match cachedTransform with
                | ValueSome struct(previous,value) when previous=current -> value
                | _ ->
                    let value=Some(Transform.toSvg current)
                    cachedTransform<-ValueSome struct(current,value)
                    value
            let appearance () =
                let item=read()
                let value=
                    match cached with
                    | ValueSome struct(mark,scope,value) when scope=item.Namespace && Mark.style mark=Mark.style item.Mark && Mark.textStyle mark=Mark.textStyle item.Mark && Mark.fillDefinition mark=Mark.fillDefinition item.Mark -> value
                    | _ ->
                        let style,text=Mark.style item.Mark,Mark.textStyle item.Mark
                        let fill=(Mark.fillDefinition item.Mark).IsSome
                        let hasFill=fill || not(String.Equals(Paint.toCss style.Fill,"none",StringComparison.OrdinalIgnoreCase))
                        let hasStroke=style.StrokeWidth>0. && not(String.Equals(Paint.toCss style.Stroke,"none",StringComparison.OrdinalIgnoreCase))
                        let alpha=number style.Opacity
                        // Keep option wrappers with cached serialization instead of allocating one per attribute read.
                        {Fill=Some(Mark.fillDefinition item.Mark |> Option.map(fun key -> "url(#"+definitionId item.Namespace key+")") |> Option.defaultWith(fun () -> Paint.toCss style.Fill))
                         Stroke=Some(Paint.toCss style.Stroke);StrokeWidth=Some(number style.StrokeWidth)
                         Opacity=Some(if hasFill && hasStroke then alpha else "1")
                         FillOpacity=Some(if hasFill && not hasStroke then alpha else "1")
                         StrokeOpacity=Some(if hasStroke && not hasFill then alpha else "1")
                         FontSize=Some(number style.FontSize)
                         Family=Some text.Family;Weight=Some(string text.Weight);Anchor=Some text.Anchor;Baseline=Some text.Baseline}
                cached<-ValueSome struct(item.Mark,item.Namespace,value)
                value
            {Read=read;Appearance=appearance;Transform=transform}) (fun t ->
            let attribute name read = t.Attrs.attribute(name, fun (row:RenderRow) -> Some(read (row.Read()).Mark))
            let appearance name read = t.Attrs.attribute(name,fun (row:RenderRow) -> read(row.Appearance()))
            let path =
                t.element (HtmlTag "path")
                    [|
                        appearance "opacity" (fun style -> style.Opacity)
                        appearance "fill-opacity" (fun style -> style.FillOpacity)
                        appearance "stroke-opacity" (fun style -> style.StrokeOpacity)
                        attribute "d" (fun mark -> match Mark.geometry mark with PathGeometry path -> Path.toSvg path | _ -> "")
                        attribute "display" (fun mark -> match Mark.geometry mark with PathGeometry _ -> "inline" | _ -> "none")
                    |]
                    [||]
            let text =
                t.element (HtmlTag "text")
                    [|
                        appearance "opacity" (fun style -> style.Opacity)
                        appearance "fill-opacity" (fun style -> style.FillOpacity)
                        appearance "stroke-opacity" (fun style -> style.StrokeOpacity)
                        attribute "x" (fun mark -> match Mark.geometry mark with TextGeometry(point, _) -> number point.X | _ -> "0")
                        attribute "y" (fun mark -> match Mark.geometry mark with TextGeometry(point, _) -> number point.Y | _ -> "0")
                        attribute "display" (fun mark -> match Mark.geometry mark with TextGeometry _ -> "inline" | _ -> "none")
                    |]
                    [| t.text(fun row -> match Mark.geometry (row.Read()).Mark with TextGeometry(_, text) -> text | _ -> "") |]
            t.element (HtmlTag "g") ([|
                attribute "data-d5-key" Mark.key
                t.Attrs.attribute("transform",fun (row:RenderRow) -> row.Transform())
                appearance "fill" (fun style -> style.Fill)
                appearance "stroke" (fun style -> style.Stroke)
                appearance "stroke-width" (fun style -> style.StrokeWidth)
                appearance "font-size" (fun style -> style.FontSize)
                appearance "font-family" (fun style -> style.Family)
                appearance "font-weight" (fun style -> style.Weight)
                appearance "text-anchor" (fun style -> style.Anchor)
                appearance "dominant-baseline" (fun style -> style.Baseline)
                t.Attrs.attribute("aria-label", fun row -> Mark.label (row.Read()).Mark)
                for event in ["click"; "pointerdown"; "pointermove"; "pointerup"; "pointercancel"; "wheel"; "keydown"] do
                    t.Events.on(event, fun row payload -> dispatch (row.Read()).Path event payload)
            |]) [| path; text |])

    let private definitionRow =
        let stopRow =
            Component.define (fun _ (read:unit -> int * GradientStop) -> read) (fun t ->
                t.element (HtmlTag "stop")
                    [|t.Attrs.attribute("offset", fun read -> Some(number (snd(read())).Offset))
                      t.Attrs.attribute("stop-color", fun read -> Some(Paint.toCss (snd(read())).Color))
                      t.Attrs.attribute("stop-opacity", fun read -> Some(number (snd(read())).Opacity))|] [||])
        let make tag attributes children =
            Component.define (fun _ (read:unit -> string * SceneDefinition) -> read) (fun t ->
                let attr name f = t.Attrs.attribute(name, fun read -> Some(f(snd(read()))))
                t.element (HtmlTag tag)
                    (Array.append [|t.Attrs.attribute("id", fun read -> let ns,d=read() in Some(definitionId ns (SceneDefinition.key d)))|] (attributes attr))
                    (children t))
        let clip =
            make "clipPath" (fun attr -> [|attr "clipPathUnits" (fun _ -> "userSpaceOnUse")|]) (fun t ->
                let rect f read = match snd(read()) with RectClip(_,b) -> Some(number(f b)) | _ -> None
                [|t.element (HtmlTag "rect")
                    [|t.Attrs.attribute("x", rect Bounds.x);t.Attrs.attribute("y", rect Bounds.y)
                      t.Attrs.attribute("width",rect Bounds.width);t.Attrs.attribute("height",rect Bounds.height)|] [||]|])
        let stops (t:TemplateBuilder<unit -> string * SceneDefinition>) =
            [|t.each fst (fun read ->
                match snd(read()) with
                | LinearGradient(_,_,_,s) | RadialGradient(_,_,_,s) -> s |> List.indexed |> List.toArray
                | _ -> [||]) stopRow|]
        let linear = make "linearGradient" (fun attr ->
            [|attr "gradientUnits" (fun _ -> "userSpaceOnUse")
              for name, f in ["x1",(fun (a:Point) _ -> a.X);"y1",(fun a _ -> a.Y);"x2",(fun _ b -> b.X);"y2",(fun _ b -> b.Y)] do
                  attr name (function LinearGradient(_,a,b,_) -> number(f a b) | _ -> "0")|]) stops
        let radial = make "radialGradient" (fun attr ->
            [|attr "gradientUnits" (fun _ -> "userSpaceOnUse")
              attr "cx" (function RadialGradient(_,p,_,_) -> number p.X | _ -> "0")
              attr "cy" (function RadialGradient(_,p,_,_) -> number p.Y | _ -> "0")
              attr "r" (function RadialGradient(_,_,r,_) -> number r | _ -> "0")|]) stops
        Component.define (fun _ (read:unit -> string * SceneDefinition) -> read) (fun t ->
            t.matchWith (fun read -> read()) (fun ((_,d) as value) ->
                match d with
                | RectClip _ -> t.arm clip value
                | LinearGradient _ -> t.arm linear value
                | RadialGradient _ -> t.arm radial value))

    /// <summary>Creates retained keyed scenes with mount-local definitions and scoped group identities.</summary>
    let createScoped dispatch =
        let marks = row dispatch
        let mutable groupDefinition : Component<unit -> RenderNode> option = None
        let leaf =
            Component.define (fun _ (read:unit -> RenderNode) ->
                let mutable cached=ValueNone
                fun () ->
                    let item=read()
                    match cached with
                    | ValueSome struct(previous,value) when Object.ReferenceEquals(previous,item) -> value
                    | _ ->
                        let value=
                            match item.Node with
                            | Leaf mark -> {Mark=mark;Namespace=item.Namespace;Path=item.Path}
                            | _ -> invalidOp "Expected a leaf."
                        cached<-ValueSome struct(item,value)
                        value) (fun t -> t.render marks id)
        let node =
            Component.define (fun _ (read:unit -> RenderNode) -> read) (fun t ->
                t.matchWith (fun read -> read()) (fun item ->
                    match item.Node with
                    | Leaf _ -> t.arm leaf item
                    | Group _ -> t.arm groupDefinition.Value item))
        let group =
            Component.define (fun _ (read:unit -> RenderNode) -> read) (fun t ->
                let spec read = match (read()).Node with Group(g,_) -> g | _ -> invalidOp "Expected a group."
                t.element (HtmlTag "g")
                    [|t.Attrs.attribute("data-d5-group", fun read -> Some((spec read).Key))
                      t.Attrs.attribute("transform",fun read -> Some(Transform.toSvg (spec read).Transform))
                      t.Attrs.attribute("clip-path",fun read -> (spec read).Clip |> Option.map (fun key -> "url(#"+definitionId (read()).Namespace key+")"))|]
                    [|t.each (fun item -> SceneNode.key item.Node) (fun read ->
                        let item=read()
                        match item.Node with
                        | Group(_,children) -> children |> List.map (fun child -> {item with Node=child;Path=item.Path@[SceneNode.key child]}) |> List.toArray
                        | _ -> [||]) node|])
        groupDefinition <- Some group
        let flat =
            Component.define (fun _ (read:unit -> string * Scene) -> read) (fun t ->
                t.each (fun item -> Mark.key item.Mark) (fun read ->
                    let ns,scene=read()
                    Scene.marks scene |> List.map (fun mark -> {Mark=mark;Namespace=ns;Path=[Mark.key mark]}) |> List.toArray) marks)
        let composed =
            Component.define (fun _ (read:unit -> string * Scene) -> read) (fun t ->
                t.each (fun item -> SceneNode.key item.Node) (fun read ->
                    let ns,scene=read()
                    Scene.nodes scene |> List.map (fun n -> {Node=n;Namespace=ns;Path=[SceneNode.key n]}) |> List.toArray) node)
        Component.define (fun _ (read:unit -> Scene) -> "d5-"+Guid.NewGuid().ToString("N")+"-",read) (fun t ->
            svg<string * (unit -> Scene)>(
                t.Attrs.attribute("role","img"),
                t.Attrs.attribute("aria-label", fun (_,read) -> Some(Scene.title(read()))),
                t.Attrs.attribute("width",fun (_,read) -> Some(number(Scene.width(read())))),
                t.Attrs.attribute("height",fun (_,read) -> Some(number(Scene.height(read())))),
                t.Attrs.attribute("viewBox",fun (_,read) -> let s=read() in Some("0 0 "+number(Scene.width s)+" "+number(Scene.height s)))
            ).Run
                [|t.element (HtmlTag "defs") [||]
                    [|t.each (snd >> SceneDefinition.key) (fun (ns,read) -> Scene.definitions(read()) |> List.map (fun d -> ns,d) |> List.toArray) definitionRow|]
                  t.matchWith (fun (ns,read) -> ns,read()) (fun ((_,scene) as value) ->
                    if Scene.isComposed scene
                    then t.arm composed value else t.arm flat value)|])

    /// <summary>A reusable scene component that forwards each mark event to dispatch with the leaf mark key and event name; group scopes are dropped.</summary>
    let create dispatch = createScoped (fun path name event -> dispatch (List.last path) name event)

    /// <summary>A shared renderer component that ignores mark events.</summary>
    let passive = create (fun _ _ _ -> ())

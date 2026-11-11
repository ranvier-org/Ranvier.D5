namespace Ranvier.D5.View

open System.Globalization
open Ranvier.D5
open Ranvier.View
open Ranvier.View.Sugar

[<RequireQualifiedAccess>]
module Renderer =
    let private number (value: float) = value.ToString("G17", CultureInfo.InvariantCulture)

    let private row (dispatch: string -> string -> Ranvier.Dom.Event -> unit) =
        Component.define (fun _ (read: unit -> Mark) -> read) (fun t ->
            let attribute name read = t.Attrs.attribute(name, fun (mark: unit -> Mark) -> Some(read (mark ())))
            let path =
                t.element (HtmlTag "path")
                    [|
                        attribute "d" (fun mark -> match Mark.geometry mark with PathGeometry path -> Path.toSvg path | _ -> "")
                        attribute "display" (fun mark -> match Mark.geometry mark with PathGeometry _ -> "inline" | _ -> "none")
                    |]
                    [||]
            let text =
                t.element (HtmlTag "text")
                    [|
                        attribute "x" (fun mark -> match Mark.geometry mark with TextGeometry(point, _) -> number point.X | _ -> "0")
                        attribute "y" (fun mark -> match Mark.geometry mark with TextGeometry(point, _) -> number point.Y | _ -> "0")
                        attribute "display" (fun mark -> match Mark.geometry mark with TextGeometry _ -> "inline" | _ -> "none")
                    |]
                    [| t.text(fun read -> match Mark.geometry (read ()) with TextGeometry(_, text) -> text | _ -> "") |]
            t.element (HtmlTag "g") ([|
                attribute "data-d5-key" Mark.key
                attribute "transform" (Mark.transform >> Transform.toSvg)
                attribute "fill" (Mark.style >> fun style -> Paint.toCss style.Fill)
                attribute "stroke" (Mark.style >> fun style -> Paint.toCss style.Stroke)
                attribute "stroke-width" (Mark.style >> fun style -> number style.StrokeWidth)
                attribute "opacity" (Mark.style >> fun style -> number style.Opacity)
                attribute "font-size" (Mark.style >> fun style -> number style.FontSize)
                t.Attrs.attribute("aria-label", fun read -> Mark.label (read ()))
                for event in ["click"; "pointerdown"; "pointermove"; "pointerup"; "pointercancel"; "wheel"; "keydown"] do
                    t.Events.on(event, fun read payload -> dispatch (Mark.key (read ())) event payload)
            |]) [| path; text |])

    /// <summary>Creates a reusable keyed scene component; construct once per event policy, then render with a signal-backed reader.</summary>
    let create dispatch =
        let marks = row dispatch
        Component.define (fun _ (read: unit -> Scene) -> read) (fun t ->
            svg<unit -> Scene>(
                t.Attrs.attribute("role", "img"),
                t.Attrs.attribute("aria-label", fun read -> Some(Scene.title (read ()))),
                t.Attrs.attribute("width", fun read -> Some(number (Scene.width (read ())))),
                t.Attrs.attribute("height", fun read -> Some(number (Scene.height (read ())))),
                t.Attrs.attribute("viewBox", fun read -> let scene = read () in Some("0 0 " + number (Scene.width scene) + " " + number (Scene.height scene)))
            ).Run [| t.each Mark.key (fun read -> Scene.marks (read ()) |> List.toArray) marks |])

    /// <summary>A reusable scene component without event handlers.</summary>
    let passive = create (fun _ _ _ -> ())

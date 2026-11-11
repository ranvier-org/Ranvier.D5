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
        graph.Run(fun () -> source.Value <- make [box "b" 90. (Paint.rgb 0 0 255); box "a" 20. (Paint.rgb 0 255 0)])
        host.Cycle()
        host.Frame()
        let retained = container.querySelector "[data-d5-key='a']"
        Expect.isTrue (Object.ReferenceEquals(original, retained)) "key retained"
        Expect.equal (retained.getAttribute "fill") "#00ff00" "paint updated"
        let after = Array.zeroCreate<byte> before.Length
        host.Paint after
        Expect.isFalse (before = after) "native pixels changed"
        host.Click(40, 40) |> ignore
        Expect.isTrue (events.Contains ("a", "click")) "retained mark receives pointer event"
        graph.Run(fun () -> source.Value <- make [Mark.text "a" (Point.create 10. 30.) "<script>&\"Hello\"" |> Mark.withLabel (Some "<unsafe>&\"label\"")])
        host.Cycle()
        Expect.equal ((container.querySelector "text").textContent) "<script>&\"Hello\"" "same key can change geometry kind"
        graph.Run(fun () -> source.Value <- make [])
        host.Cycle()
        Expect.isNull (container.querySelector "[data-d5-key='a']") "exit removed"
        mounted.Dispose()
        graph.Run(fun () -> source.Value <- make [box "after-dispose" 10. (Paint.rgb 0 0 0)])
        host.Cycle()
        Expect.isNull (container.querySelector "svg") "disposed mount cannot resurrect scene"

[<EntryPoint>]
let main _ =
    acceptance ()
    printfn "Native scene acceptance passed: namespace, identity, reorder, paint, events, escaping and disposal."
    0

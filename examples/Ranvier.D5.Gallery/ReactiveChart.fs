module Ranvier.D5.Gallery.ReactiveChart

open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts

type Observation = { Id:string; X:float; Y:float option }

let create () =
    let source=Api.createSignal [for i in 0..100 -> {Id=string i;X=float i/10.;Y=Some(sin(float i/10.))}]
    let spec=ChartSpec.line (ChartFrame.create 800. 570. "Reactive chart recipe") (fun d -> d.Id)
                (Encoding.create "Time" (fun d -> Some d.X)) (Encoding.create "Value" (fun d -> d.Y))
    let output=Reactive.derive (fun () -> source.Value) (Chart.line {spec with Curve=CurveSpec.MonotoneX})
    let scene=Reactive.derive (fun () -> output.Value) (Result.map (fun output -> output.Scene) >> Result.defaultWith (fun errors -> failwithf "%A" errors))
    source,scene

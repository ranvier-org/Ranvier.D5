module Ranvier.D5.Gallery.Scenes

open System
open Ranvier.D5

let blue = Paint.rgb 45 120 210
let orange = Paint.rgb 235 135 45
let green = Paint.rgb 50 155 110
let palette = [|blue; orange; green; Paint.rgb 150 90 195|]
let title key x y value = Mark.text key (Point.create x y) value |> Mark.withFontSize 22.

let scene phase =
    let x = Scale.linear (0., 12.) (50., 430.)
    let y = Scale.linear (-1.5, 1.5) (240., 60.)
    let points = [for i in 0..120 -> let v = float i / 10. in Some(Point.create (Scale.map x v) (Scale.map y (sin (v + phase))))]
    let line = Shape.line Curve.Linear points |> Mark.path "series" |> Mark.withFill Paint.none |> Mark.withStroke blue 3.
    let axes = Axis.numeric "x" AxisOrientation.Bottom 240. 6 x (sprintf "%.0f")
               @ Axis.numeric "y" AxisOrientation.Left 50. 4 y (sprintf "%.1f")
    let slices = Layout.pie id 0. (2. * Math.PI) [2.;3.;4.;2.]
    let donut = slices |> List.mapi(fun i slice ->
        Shape.arc 45. 85. slice.StartAngle slice.EndAngle
        |> Mark.path ("slice/" + string i)
        |> Mark.withFill palette[i]
        |> Mark.withTransform (Transform.translate 620. 155.)
        |> Mark.withLabel (Some(sprintf "Category %d: %.0f" (i + 1) slice.Value)))
    let hierarchy = Hierarchy.node "root" 0. [
        Hierarchy.node "Alpha" 3. []; Hierarchy.node "Beta" 2. []
        Hierarchy.node "Gamma" 4. []; Hierarchy.node "Delta" 1. []]
    let tiles = Hierarchy.treemap (Bounds.create 50. 340. 380. 180.) hierarchy
                |> List.filter(fun item -> item.Parent.IsSome)
                |> List.mapi(fun i item ->
                    Shape.rectangle item.Bounds |> Mark.path ("tile/" + item.Id)
                    |> Mark.withFill palette[i] |> Mark.withStroke (Paint.rgb 255 255 255) 3.)
    let bands = Scale.band ["A";"B";"C";"D"] (500.,750.) 0.2 0.1
    let bars = [25.;85.;55.;110.] |> List.mapi(fun i value ->
        let key = [|"A";"B";"C";"D"|][i]
        let left = Scale.bandPosition bands key |> Option.get
        Shape.rectangle (Bounds.create left (520. - value) (Scale.bandwidth bands) value)
        |> Mark.path ("bar/" + key) |> Mark.withFill palette[i] |> Mark.withLabel (Some key))
    Scene.create 800. 570. "Ranvier.D5 native visualization foundations"
        ([title "heading/line" 50. 32. "Signal-driven line"; title "heading/radial" 505. 32. "Radial composition"
          title "heading/tree" 50. 310. "Hierarchy layout"; title "heading/bars" 500. 310. "Categorical scales"]
         @ axes @ [line] @ donut @ tiles @ bars)

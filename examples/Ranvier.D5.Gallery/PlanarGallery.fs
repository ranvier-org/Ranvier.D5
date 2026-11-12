module Ranvier.D5.Gallery.PlanarGallery
open System
open Ranvier.D5

let private line points = Shape.line Curve.Linear ((points@[List.head points]) |> List.map Some)
let private dot id color size point = Shape.symbol Symbol.Circle size |> Mark.path id |> Mark.withFill color |> Mark.withTransform(Transform.translate point.X point.Y)
let scene() =
    let samples=[for i in 0..39 -> let angle=float i*2.399963229728653 in let radius=25.+float(i%11)*12. in Point.create (200.+cos angle*radius) (260.+sin angle*radius)]
    let hull=Polygon.hull samples
    let centroid=Polygon.centroid hull |> Option.get
    let ring=[Point.create 450. 130.;Point.create 740. 130.;Point.create 650. 250.;Point.create 740. 410.;Point.create 450. 410.;Point.create 530. 260.]
    let probes=[for y in 0..8 do for x in 0..8 -> Point.create (450.+float x*36.) (130.+float y*35.)]
    Scene.create 800. 570. "Robust planar geometry" (
        [Mark.text "hull-title" (Point.create 25. 35.) "Convex hull and area centroid" |> Mark.withFontSize 18.
         Mark.text "contains-title" (Point.create 430. 35.) "Concave polygon: boundary included" |> Mark.withFontSize 18.
         line hull |> Mark.path "hull" |> Mark.withFill(Paint.rgb 60 130 210) |> Mark.withOpacity 0.2
         line hull |> Mark.path "outline" |> Mark.withFill Paint.none |> Mark.withStroke (Paint.rgb 40 100 180) 2.
         line ring |> Mark.path "ring" |> Mark.withFill Paint.none |> Mark.withStroke (Paint.rgb 40 100 180) 2.]
        @(samples |> List.mapi(fun i point -> dot ("sample/"+string i) (Paint.rgb 40 100 180) 35. point))
        @[Shape.symbol Symbol.Diamond 100. |> Mark.path "centroid" |> Mark.withFill(Paint.rgb 220 70 45) |> Mark.withTransform(Transform.translate centroid.X centroid.Y)
          Mark.text "centroid-label" (Point.create 25. 465.) "Red diamond: area centroid" |> Mark.withFontSize 13.
          Mark.text "area" (Point.create 25. 490.) (sprintf "Area: %.2f scene units²" (Polygon.area hull)) |> Mark.withFontSize 14.
          Mark.text "legend" (Point.create 430. 490.) "Blue: inside or on edge · Gray: outside" |> Mark.withFontSize 14.]
        @(probes |> List.mapi(fun i point -> dot ("probe/"+string i) (if Polygon.contains point ring then Paint.rgb 40 100 180 else Paint.rgb 190 195 205) 35. point)))

module Ranvier.D5.Gallery.GeoKernelsGallery

open System
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private p longitude latitude=GeoPosition.create longitude latitude |> ok
let private screen (position:GeoPosition)=Point.create (40.+2.*(position.Longitude+180.)) (120.+2.*(90.-position.Latitude))
let private label key x y text size=Mark.text key (Point.create x y) text |> Mark.withFontSize size |> Mark.withFill(Paint.rgb 31 44 65)
let private grid=Graticule.create GraticuleConfig.defaultValue |> ok

let scene phase =
    let start=p -120. 30.
    let finish=p 40. (35.+10.*sin phase)
    let samples=[for i in 0..180 -> Geodesic.interpolate (float i/180.) start finish |> ok]
    let extent=GeoMeasure.bounds GeoStreamConfig.defaultValue (GeoGeometry.LineString[start;finish]) |> ok |> Option.get
    let route=Shape.line Curve.Linear (samples |> List.map(screen >> Some)) |> Mark.path "route" |> Mark.withFill Paint.none |> Mark.withStroke(Paint.rgb 224 105 52) 3.
    let gridMarks=grid.Lines |> List.mapi(fun index line ->
        Shape.line Curve.Linear (line |> List.map(screen >> Some)) |> Mark.path("grid/"+string index)
        |> Mark.withFill Paint.none |> Mark.withStroke(Paint.rgb 218 226 233) 1.)
    let upper=Shape.line Curve.Linear [Some(Point.create 40. (120.+2.*(90.-extent.North)));Some(Point.create 760. (120.+2.*(90.-extent.North)))]
              |> Mark.path "upper-bound" |> Mark.withFill Paint.none |> Mark.withStroke(Paint.rgb 36 143 135) 2.
    let ends=[start;finish] |> List.mapi(fun index point ->
        let position=screen point
        Shape.symbol Symbol.Circle 80. |> Mark.path ("endpoint/"+string index) |> Mark.withTransform(Transform.translate position.X position.Y) |> Mark.withFill(Paint.rgb 224 105 52))
    Scene.create 800. 570. "Spherical geometry foundations"
        ([label "title" 40. 42. "Spherical geometry, native F#" 25.
          label "subtitle" 40. 72. "Shortest great-circle arc and its true northern bound" 17.]
         @gridMarks@[upper;route]@ends@
         [label "bound-label" 40. 520. (sprintf "Northern bound: %.2f degrees  |  Angular length: %.3f radians" extent.North (Geodesic.distance start finish)) 16.
          label "help" 40. 550. "Ranvier signals move the endpoint. Coordinates shown on a longitude / latitude grid." 14.])

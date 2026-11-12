module Ranvier.D5.Gallery.TilingGallery
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private root = Hierarchy.node "Toolkit" 0. [for i,name in List.indexed ["Signals";"View";"Anime";"CSS";"WebCore";"D5"] -> Hierarchy.node name (float(i+1)) []]
let private color i = Chromatic.categorical Chromatic.category10 i |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip
let private panel x y title tiling =
    let boxes=Treemap.layout {TreemapConfig.create(Bounds.create 0. 28. 240. 205.) with Tiling=tiling;Padding=3.} root |> ok |> List.tail
    let marks=boxes |> List.mapi(fun i cell ->
        [Shape.rectangle cell.Bounds |> Mark.path cell.Id |> Mark.withFill(color i) |> Mark.withOpacity 0.8 |> Leaf
         if Bounds.height cell.Bounds>=18. && Bounds.width cell.Bounds>=float cell.Id.Length*5.+8. then
             Mark.text ("label/"+cell.Id) (Point.create (Bounds.x cell.Bounds+4.) (Bounds.y cell.Bounds+14.)) cell.Id |> Mark.withFontSize 10. |> Leaf]) |> List.concat
    Group({GroupSpec.create title with Transform=Transform.translate x y},Leaf(Mark.text "title" (Point.create 0. 15.) title |> Mark.withFontSize 16.)::marks)
let scene() =
    let circles=Pack.layout {PackConfig.create(Bounds.create 0. 28. 240. 205.) with Padding=4.} root |> ok |> List.tail
    let bubbles=circles |> List.mapi(fun i circle ->
        [Shape.symbol Symbol.Circle (System.Math.PI*circle.Radius*circle.Radius) |> Mark.path circle.Id |> Mark.withTransform(Transform.translate circle.Center.X circle.Center.Y) |> Mark.withFill(color i) |> Mark.withOpacity 0.8 |> Leaf
         Mark.text ("label/"+circle.Id) (Point.create (circle.Center.X-15.) (circle.Center.Y+3.)) circle.Id |> Mark.withFontSize 10. |> Leaf]) |> List.concat
    Scene.compose {Width=800.;Height=570.} "Native tiling and packing" []
        [panel 20. 15. "Squarified" TreemapTiling.Squarified;panel 280. 15. "Binary" TreemapTiling.Binary;panel 540. 15. "Slice-dice" TreemapTiling.SliceDice
         panel 20. 290. "Slice" TreemapTiling.Slice;panel 280. 290. "Dice" TreemapTiling.Dice
         Group({GroupSpec.create "pack" with Transform=Transform.translate 540. 290.},Leaf(Mark.text "title" (Point.create 0. 15.) "Circle packing" |> Mark.withFontSize 16.)::bubbles)]

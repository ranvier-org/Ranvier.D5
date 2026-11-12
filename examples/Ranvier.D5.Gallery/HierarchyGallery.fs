module Ranvier.D5.Gallery.HierarchyGallery

open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let root =
    Hierarchy.node "Toolkit" 0.
        [Hierarchy.node "Signals" 0. [Hierarchy.node "State" 2. [];Hierarchy.node "Effects" 1. []]
         Hierarchy.node "View" 0. [Hierarchy.node "Scene" 0. [Hierarchy.node "Shapes" 2. [];Hierarchy.node "Labels" 1. []];Hierarchy.node "Input" 1. []]
         Hierarchy.node "Compute" 0. [Hierarchy.node "Layout" 3. [];Hierarchy.node "Workers" 2. []]]

let private color depth =
    Chromatic.categorical Chromatic.category10 depth |> Chromatic.paint Ranvier.Anime.GamutPolicy.Clip

let private points title x layout =
    let items:HierarchyPoint<string> list=layout (TreeConfig.create(Bounds.create 20. 45. 350. 170.)) root |> ok
    let positions=items |> List.map(fun item -> item.Id,item.Point) |> Map.ofList
    let edges=items |> List.choose(fun item -> item.Parent |> Option.map(fun parent ->
        Curves.linkVertical positions[parent] item.Point |> Mark.path("edge/"+item.Id) |> Mark.withStroke (Paint.rgb 150 160 175) 1.5 |> Mark.withFill Paint.none |> Leaf))
    let nodes=items |> List.mapi(fun i item ->
        [Shape.symbol Symbol.Circle 55. |> Mark.path item.Id |> Mark.withFill(color item.Depth) |> Mark.withTransform(Transform.translate item.Point.X item.Point.Y) |> Leaf
         Mark.text ("label/"+item.Id) (Point.create item.Point.X (item.Point.Y+(if title="Cluster" && i%2=1 then 27. else 15.))) item.Id
         |> Mark.withFontSize 10. |> fun mark -> Mark.withTextStyle {Mark.textStyle mark with Anchor="middle"} mark |> Leaf]) |> List.concat
    Group({GroupSpec.create title with Transform=Transform.translate x 0.},
          Leaf(Mark.text "title" (Point.create 20. 22.) title |> Mark.withFontSize 16.)::edges@nodes)

let scene() =
    let boxes=Partition.layout {Bounds=Bounds.create 20. 325. 760. 210.;Padding=2.} root |> ok
    let cells=boxes |> List.collect(fun item ->
        [Shape.rectangle item.Bounds |> Mark.path item.Id |> Mark.withFill(color item.Depth) |> Mark.withOpacity 0.8 |> Leaf
         Mark.text ("label/"+item.Id) (Point.create (Bounds.x item.Bounds+5.) (Bounds.y item.Bounds+18.)) item.Id |> Mark.withFontSize 11. |> Leaf])
    Scene.compose {Width=800.;Height=570.} "Native hierarchy layouts" []
        [points "Tidy tree" 0. Tree.layout;points "Cluster" 400. Cluster.layout
         Group(GroupSpec.create "partition",Leaf(Mark.text "title" (Point.create 20. 300.) "Weighted partition" |> Mark.withFontSize 16.)::cells)]

module Ranvier.D5.Gallery.ForceGallery

open Ranvier.D5

let private nodes=[for i in 0..15 -> GraphNode.create(string i)]
let private links=[for i in 0..15 do for jump in [1;4] -> {GraphLink.create (string i+"/"+string jump) (string i) (string((i+jump)%16)) with Distance=85.;Strength=0.08}]
let private graph=GraphData.create nodes links |> Result.defaultWith(fun errors -> failwithf "%A" errors)
let initial()=Force.initialize {ForceConfig.create(Point.create 190. 260.) with InitialRadius=150.;CenterStrength=0.02} (Seed.create 42u) graph |> Result.defaultWith(fun errors -> failwithf "%A" errors)
let private before=initial() |> Force.snapshot
let scene state =
    let snapshot=Force.snapshot state
    let marks prefix offset color (snapshot:ForceSnapshot) =
        let positions=snapshot.Nodes |> List.map(fun node -> node.Id,node.Position) |> Map.ofList
        [for link in links ->
            Path.ofCommands [MoveTo positions[link.Source];LineTo positions[link.Target]] |> Mark.path(prefix+"/link/"+link.Id)
            |> Mark.withFill Paint.none |> Mark.withStroke (Paint.rgb 185 195 210) 1. |> Mark.withTransform(Transform.translate offset 0.)]
        @[for node in snapshot.Nodes ->
            Shape.symbol Symbol.Circle 90. |> Mark.path(prefix+"/node/"+node.Id) |> Mark.withFill color
            |> Mark.withTransform(Transform.translate (node.Position.X+offset) node.Position.Y)]
    Scene.create 800. 570. "Deterministic immutable force stepping" (
        [Mark.text "initial-title" (Point.create 25. 35.) "Seeded initial positions" |> Mark.withFontSize 22.
         Mark.text "current-title" (Point.create 425. 35.) "Link and center forces" |> Mark.withFontSize 22.]
        @marks "initial" 0. (Paint.rgb 35 110 190) before
        @marks "current" 400. (Paint.rgb 225 110 45) snapshot
        @[Mark.text "help" (Point.create 25. 505.) "Each step returns new state; the initial snapshot remains unchanged." |> Mark.withFontSize 15.
          Mark.text "status" (Point.create 25. 535.) (sprintf "16 nodes / 32 links | step %d | alpha %.4f | finished %b" snapshot.Iteration snapshot.Alpha snapshot.Finished) |> Mark.withFontSize 14.])

module Ranvier.D5.Gallery.SankeyGallery

open Ranvier.D5

let scene phase =
    let renewable=30.+10.*sin phase
    let edge id source target value : FlowLink = {Id=id;Source=source;Target=target;Value=value}
    let graph=FlowGraph.create ["Solar";"Wind";"Grid";"Homes";"Industry";"Losses"]
                  [edge "solar-grid" "Solar" "Grid" renewable
                   edge "wind-grid" "Wind" "Grid" 40.
                   edge "grid-homes" "Grid" "Homes" 25.
                   edge "grid-industry" "Grid" "Industry" 30.
                   edge "grid-losses" "Grid" "Losses" 5.]
              |> Result.defaultWith(fun errors -> failwithf "%A" errors)
    let layout=Sankey.layout {SankeyConfig.create(Bounds.create 45. 110. 700. 330.) with NodeWidth=18.;NodePadding=32.} graph
               |> Result.defaultWith(fun errors -> failwithf "%A" errors)
    let color id = if id="Solar" then Paint.rgb 230 155 40 elif id="Wind" then Paint.rgb 40 165 125 else Paint.rgb 65 120 190
    let links=[for link in layout.Links ->
                   Shape.linkHorizontal link.Start link.Finish |> Mark.path("link/"+link.Id)
                   |> Mark.withFill Paint.none |> Mark.withStroke (color link.Source) link.Width |> Mark.withOpacity 0.4]
    let nodes=[for node in layout.Nodes do
                   let bounds=node.Bounds
                   yield Shape.rectangle bounds
                         |> Mark.path("node/"+node.Id) |> Mark.withFill (color node.Id)
                   yield Mark.text ("label/"+node.Id) (Point.create (if node.Layer=2 then Bounds.x bounds+Bounds.width bounds else Bounds.x bounds) (Bounds.y bounds-10.)) (sprintf "%s: %.0f" node.Id node.Value)
                         |> Mark.withFontSize 16.
                         |> fun mark -> Mark.withTextStyle {Mark.textStyle mark with Anchor=(if node.Layer=2 then "end" else "start")} mark]
    Scene.create 800. 570. "Sankey flow with explicit imbalance"
        ([Mark.text "title" (Point.create 40. 40.) "Energy flow: immutable DAG layout" |> Mark.withFontSize 24.]
         @links@nodes
         @[Mark.text "imbalance" (Point.create 40. 495.) (sprintf "Grid input %.1f, output 60; imbalance %.1f (out − in)." (renewable+40.) (20.-renewable)) |> Mark.withFontSize 17.
           Mark.text "help" (Point.create 40. 535.) "Node height = max(inflow, outflow); a Ranvier signal changes solar input." |> Mark.withFontSize 15.])

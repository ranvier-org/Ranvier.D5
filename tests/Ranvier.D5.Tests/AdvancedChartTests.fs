module Ranvier.D5.Tests.AdvancedChartTests

open System
open Expecto
open Ranvier.D5
open Ranvier.D5.Charts

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private frame=ChartFrame.create 400. 300. "Advanced"
let private pathOf (output:ChartOutput) (fragment:string) =
    let rec leaves nodes=nodes |> List.collect(function Leaf mark -> [mark] | Group(_,children) -> leaves children)
    leaves (Scene.nodes output.Scene) |> List.filter(fun mark -> (Mark.key mark).Contains fragment)
let private commands mark=match Mark.geometry mark with PathGeometry path -> Path.commands path | _ -> []
let private center = function HitPoint(point,_) -> point | HitBounds bounds -> Point.create (Bounds.x bounds+Bounds.width bounds/2.) (Bounds.y bounds+Bounds.height bounds/2.) | HitRegion(_,_,point) -> point | HitSector(point,_,_,_,_) -> point

/// Every semantic and hit target must name a mark in the scene, so interaction can highlight it.
let private addressable (output:ChartOutput) =
    let paths=SceneNode.paths [] (Scene.nodes output.Scene) |> List.map fst |> Set.ofList
    for item in output.Semantics do Expect.isTrue (paths.Contains item.MarkPath) (sprintf "semantic %A names a scene mark" item.MarkPath)
    for hit in output.HitTargets do Expect.isTrue (paths.Contains hit.MarkPath) (sprintf "hit target %A names a scene mark" hit.MarkPath)

type private Sample = { Id:string; T:float; X:float option; Y:float option }

/// A caller-written layout: a golden-angle sunflower built only from public scene, path and hit-test APIs.
let sunflower (frame:ChartFrame) (items:(string*float) list) : Result<ChartOutput,LayoutError list> =
    ChartLayout.plot frame |> Result.map(fun bounds ->
        let centre=Point.create (Bounds.x bounds+Bounds.width bounds/2.) (Bounds.y bounds+Bounds.height bounds/2.)
        let spacing=min (Bounds.width bounds) (Bounds.height bounds)/2./sqrt(float(max 1 items.Length))
        let placed=items |> List.mapi(fun index (id,size) ->
            let angle=float index*2.399963229728653
            let point=Point.create (centre.X+spacing*sqrt(float index)*cos angle) (centre.Y+spacing*sqrt(float index)*sin angle)
            let radius=max 1. (sqrt size)
            id,point,radius)
        let marks=placed |> List.map(fun (id,point,radius) ->
            Shape.symbol Symbol.Circle (Math.PI*radius*radius) |> Mark.path ("seed/"+id) |> Mark.withTransform(Transform.translate point.X point.Y) |> Mark.withLabel(Some id))
        {Scene=Scene.compose frame.Viewport frame.Title [] [Group(GroupSpec.create "plot",List.map Leaf marks)]
         PlotBounds=bounds
         Semantics=placed |> List.map(fun (id,_,_) -> {MarkPath=["plot";"seed/"+id];DataIds=[id];Description=id})
         HitTargets=placed |> List.map(fun (id,point,radius) -> {MarkPath=["plot";"seed/"+id];DataIds=[id];Geometry=HitPoint(point,radius);Clip=Some bounds})
         Description=frame.Title})

[<Tests>]
let tests = testList "advanced chart recipes" [
    testCase "connected scatter joins points in order and breaks at missing values" <| fun _ ->
        let rows=[{Id="c";T=3.;X=Some 3.;Y=Some 1.};{Id="a";T=1.;X=Some 1.;Y=Some 2.};{Id="gap";T=2.;X=None;Y=Some 0.};{Id="d";T=4.;X=Some 4.;Y=Some 4.}]
        let spec={Frame=frame;Key=(fun row -> row.Id);Order=(fun row -> row.T);X=Encoding.create "X" (fun row -> row.X);Y=Encoding.create "Y" (fun row -> row.Y)
                  Color=Paint.rgb 0 0 0;XDomain=Auto;YDomain=Auto}
        let output=Chart.connectedScatter spec rows |> ok
        addressable output
        let trail=pathOf output "trail" |> List.head |> commands
        Expect.equal (trail |> List.filter(function MoveTo _ -> true | _ -> false) |> List.length) 2 "gap splits the trail"
        Expect.equal (output.HitTargets |> List.map(fun hit -> hit.DataIds.Head)) ["a";"c";"d"] "points in order, missing omitted"
        Expect.equal (Chart.connectedScatter spec (List.rev rows) |> ok).Semantics output.Semantics "input order does not change identity"
    testCase "slope lines pick along their length and skip incomplete items" <| fun _ ->
        let spec={Frame=frame;Key=fst;Before=Encoding.create "2020" (snd >> fst);After=Encoding.create "2025" (snd >> snd);Color=(fun _ -> Paint.rgb 0 0 0);Domain=Auto}
        let output=Chart.slope spec ["up",(Some 1.,Some 5.);"down",(Some 4.,Some 2.);"missing",(None,Some 3.)] |> ok
        addressable output
        Expect.equal (output.HitTargets |> List.map(fun hit -> hit.DataIds.Head)) ["up";"down"] "complete items only"
        for hit in output.HitTargets do
            Expect.equal (HitTest.at (center hit.Geometry) output |> List.map(fun target -> target.DataIds.Head) |> List.contains hit.DataIds.Head) true "midpoint picks its line"
    testCase "bump ranks per period with deterministic tie breaking" <| fun _ ->
        let rows=["a1","A","p1",10.;"b1","B","p1",10.;"c1","C","p1",3.;"a2","A","p2",1.;"b2","B","p2",8.;"c2","C","p2",9.]
        let spec={Frame=frame;Key=(fun (id,_,_,_) -> id);Series=(fun (_,series,_,_) -> series);Period=(fun (_,_,period,_) -> period);Value=(fun (_,_,_,value) -> value);Descending=true;Color=(fun _ -> Paint.rgb 0 0 0)}
        let output=Chart.bump spec rows |> ok
        addressable output
        let rank id=
            let description=(output.Semantics |> List.find(fun semantic -> semantic.DataIds=[id])).Description
            let after=description.Substring(description.IndexOf "rank "+5)
            int(after.Substring(0,after.IndexOf ' '))
        Expect.equal [rank "a1";rank "b1";rank "c1"] [1;2;3] "ties break by ordinal series ID"
        Expect.equal [rank "c2";rank "b2";rank "a2"] [1;2;3] "descending values"
        let y id=output.HitTargets |> List.find(fun hit -> hit.DataIds=[id]) |> fun hit -> (center hit.Geometry).Y
        Expect.isLessThan (y "c2") (y "a2") "rank 1 is drawn above rank 3"
        for hit in output.HitTargets do
            match hit.Geometry with
            | HitPoint(point,radius) ->
                Expect.isTrue (point.Y-radius>=Bounds.y output.PlotBounds && point.Y+radius<=Bounds.y output.PlotBounds+Bounds.height output.PlotBounds) "rank dots stay inside the plot clip"
            | _ -> ()
        Expect.isError (Chart.bump spec (rows@["dup","A","p1",2.])) "one value per series and period"
    testCase "horizon folds values into bands and mirrors negatives" <| fun _ ->
        let rows=[for index in 0..9 -> sprintf "s%d" index,"s",float index,Some(float index-4.)]
        let spec={Frame=frame;Key=(fun (id,_,_,_) -> id);Series=(fun (_,series,_,_) -> series);X=Encoding.create "X" (fun (_,_,x,_) -> x);Y=Encoding.create "Y" (fun (_,_,_,y) -> y)
                  Bands=2;PositiveColor=(fun band -> Paint.rgb 0 0 (100+band*50));NegativeColor=(fun band -> Paint.rgb (100+band*50) 0 0);Extent=Some 4.}
        let output=Chart.horizon spec rows |> ok
        addressable output
        Expect.equal (pathOf output "horizon").Length 4 "two bands, positive and negative"
        Expect.equal output.HitTargets.Head.DataIds.Length 10 "row target carries its data IDs"
        let top=Bounds.y output.PlotBounds
        let bottom=Bounds.y output.PlotBounds+Bounds.height output.PlotBounds
        for mark in pathOf output "horizon" do
            for command in commands mark do
                match command with
                | MoveTo point | LineTo point -> Expect.isTrue (point.Y>=top-1e-9 && point.Y<=bottom+1e-9) "bands stay inside the row"
                | _ -> ()
        Expect.isError (Chart.horizon {spec with Bands=0} rows) "band count"
    testCase "beeswarm circles never overlap and keep their x positions" <| fun _ ->
        let random=Random 7
        let rows=[for index in 0..299 -> sprintf "b%d" index,Some(Math.Round(random.NextDouble()*20.,1))]
        let spec={Frame=frame;Key=fst;X=Encoding.create "Value" snd;Radius=3.;Color=(fun _ -> Paint.rgb 0 0 0);XDomain=Auto}
        let output=Chart.beeswarm spec rows |> ok
        addressable output
        let points=output.HitTargets |> List.map(fun hit -> center hit.Geometry) |> List.toArray
        for i in 0..points.Length-1 do
            for j in i+1..points.Length-1 do
                Expect.isGreaterThanOrEqual (Double.Hypot(points[i].X-points[j].X,points[i].Y-points[j].Y)) (6.-1e-9) "no overlap"
        let x=Scale.linear (0.,20.) (Bounds.x output.PlotBounds,Bounds.x output.PlotBounds+Bounds.width output.PlotBounds)
        let byId=rows |> Map.ofList
        for hit in output.HitTargets do
            let expected=Scale.map (Scale.linear (rows |> List.choose snd |> List.min,rows |> List.choose snd |> List.max) (Bounds.x output.PlotBounds,Bounds.x output.PlotBounds+Bounds.width output.PlotBounds)) byId[hit.DataIds.Head].Value
            Expect.floatClose Accuracy.high (center hit.Geometry).X expected "x is the data position"
        ignore x
        Expect.equal (Chart.beeswarm spec rows |> ok).HitTargets output.HitTargets "deterministic"
        Expect.isError (Chart.beeswarm {spec with Radius=0.} rows) "positive radius"
    testCase "adjacency matrices mirror undirected links and reject bad topology" <| fun _ ->
        let nodes=["a","Alpha";"b","Beta";"c","Gamma"]
        let spec={Frame=frame;Nodes=nodes;Links=["ab","a","b",2.;"bc","b","c",1.];Directed=false;Order=MatrixOrder.Degree;Color=(fun t -> Paint.rgb (int(255.*t)) 0 0)}
        let output=Chart.adjacencyMatrix spec |> ok
        addressable output
        Expect.equal output.HitTargets.Length 4 "two cells per undirected link"
        Expect.equal (Chart.adjacencyMatrix {spec with Directed=true} |> ok).HitTargets.Length 2 "one cell per directed link"
        for hit in output.HitTargets do
            Expect.equal (HitTest.at (center hit.Geometry) output |> List.map(fun target -> target.DataIds)) [hit.DataIds] "cell picks its link"
        Expect.isError (Chart.adjacencyMatrix {spec with Links=["x","a","z",1.]}) "unknown endpoint"
        Expect.isError (Chart.adjacencyMatrix {spec with Nodes=nodes@["a","Again"]}) "duplicate node"
        Expect.isError (Chart.adjacencyMatrix {spec with Links=["ab","a","b",-1.]}) "negative weight"
    testCase "linked small multiples share data IDs so one selection reaches every cell" <| fun _ ->
        let groups=["2024",[{Id="a";T=1.;X=Some 1.;Y=Some 2.};{Id="b";T=2.;X=Some 2.;Y=Some 1.}];"2025",[{Id="a";T=1.;X=Some 2.;Y=Some 3.};{Id="b";T=2.;X=Some 3.;Y=Some 2.}]]
        let build cellFrame domain data=
            Chart.connectedScatter {Frame=cellFrame;Key=(fun row -> row.Id);Order=(fun row -> row.T);X=Encoding.create "X" (fun row -> row.X);Y=Encoding.create "Y" (fun row -> row.Y)
                                    Color=Paint.rgb 0 0 0;XDomain=Fixed domain;YDomain=Fixed domain} data
        let facets=Facet.build {Frame=ChartFrame.create 600. 300. "Small multiples";Columns=2;Gap=10.;Domains=FacetDomains.Shared
                                Domain=(fun rows -> 0.,4.);Build=build} groups |> ok
        let selected=Set.ofList ["a"]
        let reached=facets.Semantics |> List.filter(fun semantic -> semantic.DataIds |> List.exists selected.Contains) |> List.map(fun semantic -> semantic.MarkPath.Head) |> List.distinct
        Expect.equal reached.Length 2 "selection reaches both cells"
    testCase "caller-written layouts compose with picking and facets through public APIs" <| fun _ ->
        let items=[for index in 0..40 -> sprintf "seed%d" index,float(index%7+1)*4.]
        let output=sunflower frame items |> ok
        for hit in output.HitTargets do
            Expect.equal (HitTest.at (center hit.Geometry) output |> List.map(fun target -> target.DataIds) |> List.last) hit.DataIds "custom marks pick"
        let facets=Facet.build {Frame=ChartFrame.create 600. 300. "Custom facets";Columns=2;Gap=10.;Domains=FacetDomains.Independent;Domain=(fun _ -> ());Build=(fun cellFrame () data -> sunflower cellFrame data)} ["left",items;"right",List.take 10 items] |> ok
        Expect.equal facets.HitTargets.Length 51 "custom layout inside facets"
]

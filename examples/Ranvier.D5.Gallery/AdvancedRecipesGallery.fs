module Ranvier.D5.Gallery.AdvancedRecipesGallery

open System
open Ranvier.D5
open Ranvier.D5.Charts

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors

/// <summary>One gallery datum per recipe, so a single facet grid can compose different chart kinds.</summary>
type Panel =
    | Trail of id:string * t:float * x:float * y:float
    | Slope of id:string * before:float * after:float
    | Rank of id:string * series:string * period:string * value:float
    | Wave of id:string * series:string * x:float * y:float
    | Bee of id:string * value:float
    | Matrix of unit

let private series=["North";"East";"South";"West"]
let private seriesColor name=
    match name with
    | "North" -> Paint.rgb 52 112 190
    | "East" -> Paint.rgb 225 112 45
    | "South" -> Paint.rgb 43 150 100
    | _ -> Paint.rgb 155 80 180

/// <summary>Deterministic data for a phase; the phase shifts every recipe without changing identities.</summary>
let groups phase =
    let wave t=sin(t+phase*Math.PI)
    ["Connected scatter",[for i in 0..11 -> let t=float i*0.5 in Trail(sprintf "t%d" i,t,cos(t+phase)*(1.+t/6.),wave t*(1.+t/6.))]
     "Slope",[for name in series -> let index=float(List.findIndex ((=) name) series) in Slope(name,10.+index*3.,10.+((index*7.+phase*5.)%12.))]
     "Bump ranks",[for period in ["Q1";"Q2";"Q3";"Q4"] do
                     for name in series -> Rank(name+period,name,period,sin(float(name.Length*7+period.Length*3+int(period[1])-48)+phase*2.))]
     "Horizon",[for name in ["Alpha";"Beta";"Gamma"] do
                  for i in 0..60 -> Wave(sprintf "%s%d" name i,name,float i,3.*sin(float i/6.+float name.Length+phase*3.))]
     "Beeswarm",[for i in 0..119 -> Bee(sprintf "b%d" i,Math.Round(10.+6.*sin(float i*1.7)+3.*cos(float i*0.37+phase*2.),1))]
     "Adjacency matrix",[Matrix()]]

let private matrixNodes=["a","Ada";"b","Bo";"c","Cy";"d","Di";"e","Ed";"f","Fa"]
let private matrixLinks phase=
    [for i,(source,_) in List.indexed matrixNodes do
        for j,(target,_) in List.indexed matrixNodes do
            if i<j && (i*j+int(phase*3.))%3<>0 then yield sprintf "%s-%s" source target,source,target,float(i+j+1)]

let private build phase (frame:ChartFrame) () (data:Panel list) : Result<ChartOutput,LayoutError list> =
    let frame={frame with Margin={Top=8.;Right=12.;Bottom=26.;Left=30.}}
    match data with
    | Trail _::_ ->
        let rows=data |> List.choose(function Trail(id,t,x,y) -> Some(id,t,x,y) | _ -> None)
        Chart.connectedScatter {Frame=frame;Key=(fun (id,_,_,_) -> id);Order=(fun (_,t,_,_) -> t);X=Encoding.create "x" (fun (_,_,x,_) -> Some x);Y=Encoding.create "y" (fun (_,_,_,y) -> Some y)
                                Color=Paint.rgb 31 44 65;XDomain=Fixed(-3.,3.);YDomain=Fixed(-3.,3.)} rows
    | Slope _::_ ->
        let rows=data |> List.choose(function Slope(id,a,b) -> Some(id,a,b) | _ -> None)
        Chart.slope {Frame=frame;Key=(fun (id,_,_) -> id);Before=Encoding.create "Before" (fun (_,a,_) -> Some a);After=Encoding.create "After" (fun (_,_,b) -> Some b)
                     Color=(fun (id,_,_) -> seriesColor id);Domain=Fixed(8.,24.)} rows
    | Rank _::_ ->
        let rows=data |> List.choose(function Rank(id,s,p,v) -> Some(id,s,p,v) | _ -> None)
        Chart.bump {Frame=frame;Key=(fun (id,_,_,_) -> id);Series=(fun (_,s,_,_) -> s);Period=(fun (_,_,p,_) -> p);Value=(fun (_,_,_,v) -> v);Descending=true;Color=seriesColor} rows
    | Wave _::_ ->
        let rows=data |> List.choose(function Wave(id,s,x,y) -> Some(id,s,x,y) | _ -> None)
        Chart.horizon {Frame=frame;Key=(fun (id,_,_,_) -> id);Series=(fun (_,s,_,_) -> s);X=Encoding.create "x" (fun (_,_,x,_) -> x);Y=Encoding.create "y" (fun (_,_,_,y) -> Some y)
                       Bands=3;PositiveColor=(fun band -> Paint.rgb (150-band*45) (190-band*40) 235);NegativeColor=(fun band -> Paint.rgb 240 (170-band*45) (150-band*45));Extent=Some 3.} rows
    | Bee _::_ ->
        let rows=data |> List.choose(function Bee(id,v) -> Some(id,v) | _ -> None)
        Chart.beeswarm {Frame=frame;Key=fst;X=Encoding.create "Value" (snd >> Some);Radius=3.5;Color=(fun (_,v) -> if v>10. then Paint.rgb 43 150 100 else Paint.rgb 52 112 190);XDomain=Fixed(0.,20.)} rows
    | _ ->
        Chart.adjacencyMatrix {Frame=frame;Nodes=matrixNodes;Links=matrixLinks phase;Directed=false;Order=MatrixOrder.Degree;Color=(fun t -> Paint.rgb (int(240.-150.*t)) (int(200.-120.*t)) 230)}

/// <summary>The six recipes in a facet grid for a phase.</summary>
let chart phase =
    Facet.build {Frame={ChartFrame.create 800. 570. "Advanced recipes" with Margin={Top=70.;Right=10.;Bottom=10.;Left=10.}};Columns=3;Gap=24.
                 Domains=FacetDomains.Independent;Domain=(fun _ -> ());Build=build phase} (groups phase) |> ok

let scene phase =
    let output=chart phase
    Scene.compose {Width=800.;Height=570.} "Advanced recipes" (Scene.definitions output.Scene)
        (Scene.nodes output.Scene@
         [Leaf(Mark.text "title" (Point.create 14. 30.) "Connected scatter, slope, bump, horizon, beeswarm and adjacency matrix" |> Mark.withFontSize 20.)
          Leaf(Mark.text "subtitle" (Point.create 14. 52.) (sprintf "Six pure recipes composed with Facet; one Ranvier signal moves the data (phase %.2f)." phase) |> Mark.withFontSize 13.)])

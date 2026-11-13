/// <summary>Revenue overview: KPIs that count to new values, regional bars re-ranked with keyed mark transitions, a tweened category
/// donut, monthly revenue against target and a live order feed.</summary>
module Ranvier.D5.Dashboards.Sales

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type Order = { Id:int; Region:string; Category:string; Amount:float; At:float }
type State =
    { Now:float; Quarter:string; Metric:string; Sorted:bool; Bars:KeyedTransitionState; BarTargets:Mark list
      Orders:Order list; OrderId:int; NextOrder:float; Today:float; TodayCount:int; Rng:RandomState
      Shown:Map<string,float>; Hover:string option; Pointer:Point option }

let regions=["North America";"Europe";"Asia Pacific";"Latin America";"Middle East";"Africa";"Oceania";"Nordics"]
let categories=["Hardware";"Software";"Services";"Support";"Training"]
let private regionBase=[1.32;1.05;1.18;0.52;0.41;0.27;0.33;0.38]
let private categoryShare=[0.34;0.27;0.2;0.12;0.07]
let private categoryPrice=[1800.;650.;4200.;900.;350.]
let private categoryMargin=[0.22;0.71;0.38;0.55;0.62]
let private quarters=["Q1";"Q2";"Q3";"Q4"]
let private seasonal=[0.86;0.97;1.03;1.24]

/// <summary>Deterministic revenue in millions for a region, quarter index and category.</summary>
let revenue region quarter category =
    let r=List.findIndex ((=) region) regions
    let c=List.findIndex ((=) category) categories
    regionBase[r]*categoryShare[c]*seasonal[quarter]*(1.+0.28*noise (r*13+c) (float quarter*0.9+0.3))*10.
let private quarterIndexes quarter = if quarter="FY" then [0..3] else [List.findIndex ((=) quarter) quarters]
let measure metric quarter region category =
    let c=List.findIndex ((=) category) categories
    let value=quarterIndexes quarter |> List.sumBy(fun q -> revenue region q category)
    match metric with
    | "Units" -> value*1e6/categoryPrice[c]
    | "Margin" -> value*categoryMargin[c]
    | _ -> value
let private format metric value = match metric with "Units" -> si 1 value | _ -> "$"+num 2 value+"M"

let private barLeft,barRight,barTop,barStep=128.,440.,48.,30.

/// <summary>Target marks for the regional bars: positions follow the rank when sorted, widths follow the metric.</summary>
let barMarks quarter metric sorted =
    let totals=regions |> List.map(fun region -> region,categories |> List.sumBy(measure metric quarter region))
    let order=if sorted then totals |> List.sortByDescending snd else totals
    let maximum=totals |> List.map snd |> List.max
    order |> List.mapi(fun rank (region,value) ->
        let y=barTop+float rank*barStep
        let width=(barRight-barLeft)*value/maximum
        let color=ramp [Theme.blue;Theme.accent;Theme.teal] (float rank/7.)
        [rrect ("h:"+region) barLeft y width 20. 4. color
         label ("name:"+region) (barLeft-10.) (y+14.5) 11.5 Theme.text 600 "end" region
         label ("value:"+region) (barLeft+width+8.) (y+14.5) 11. Theme.muted 600 "start" (format metric value)]) |> List.concat

let private targets state =
    let totals=categories |> List.map(fun c -> "cat:"+c,regions |> List.sumBy(fun r -> measure state.Metric state.Quarter r c))
    let total=totals |> List.sumBy snd
    let revenueTotal=regions |> List.sumBy(fun r -> categories |> List.sumBy(fun c -> measure "Revenue" state.Quarter r c))
    let quota=if state.Quarter="FY" then 245. else 62.
    Map.ofList (totals@["total",total;"revenue",revenueTotal;"quota",revenueTotal/quota;"today",state.Today;"count",float state.TodayCount
                        "avg",(if state.TodayCount=0 then 0. else state.Today/float state.TodayCount)])

let private retarget state =
    let marks=barMarks state.Quarter state.Metric state.Sorted
    {state with BarTargets=marks;Bars=KeyedTransition.retarget (state.Now*1000.) 750. outCubic false marks state.Bars}

let rec private arrive until state =
    if state.NextOrder>until then state else
    let at=state.NextOrder
    let pick,rng=uniform state.Rng
    let category,rng=uniform rng
    let amount,rng=draw (Distribution.logNormal 7.6 0.7 |> ok) rng
    let gap,rng=draw (Distribution.exponential 2.4 |> ok) rng
    let region=regions[min 7 (int(pick*8.))]
    let category=categories[min 4 (int(category*5.))]
    let order={Id=state.OrderId+1;Region=region;Category=category;Amount=Math.Round amount;At=at}
    arrive until {state with Orders=(order::state.Orders) |> List.truncate 7;OrderId=order.Id;NextOrder=at+gap;Today=state.Today+order.Amount;TodayCount=state.TodayCount+1;Rng=rng}

let init () =
    let marks=barMarks "Q4" "Revenue" true
    let state={Now=0.;Quarter="Q4";Metric="Revenue";Sorted=true;Bars=KeyedTransition.start 0. marks;BarTargets=marks;Orders=[];OrderId=4200;NextOrder=0.2
               Today=1_284_000.;TodayCount=1630;Rng=RandomState.create 99UL;Shown=Map.empty;Hover=None;Pointer=None}
    let state=arrive 4. {state with Now=4.}
    {state with Shown=targets state |> Map.map(fun key value -> if key.StartsWith "cat:" then value else 0.)}

let tick dt state =
    let now=state.Now+dt/1000.
    let state=arrive now {state with Now=now}
    {state with Shown=approachMap dt 420. 0. state.Shown (targets state)}

let input message state =
    match message with
    | Input.Click id when id.StartsWith "quarter:" -> retarget {state with Quarter=id.Substring 8}
    | Input.Click id when id.StartsWith "metric:" -> retarget {state with Metric=id.Substring 7}
    | Input.Click "sort" | Input.Key "s" -> retarget {state with Sorted=not state.Sorted}
    | Input.Key "ArrowRight" | Input.Key "ArrowLeft" ->
        let all=quarters@["FY"]
        let index=List.findIndex ((=) state.Quarter) all
        let next=(index+(if message=Input.Key "ArrowRight" then 1 else 4))%5
        retarget {state with Quarter=all[next]}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private shown key state = Map.tryFind key state.Shown |> Option.defaultValue 0.
let private categoryColor index = [|Theme.accent;Theme.violet;Theme.teal;Theme.warn;Theme.pink|][index]

let private donut state =
    let values=categories |> List.map(fun c -> c,max 0.0001 (shown ("cat:"+c) state))
    let total=values |> List.sumBy snd
    let cx,cy,inner,outer=150.,166.,74.,118.
    let angles=values |> List.scan(fun start (_,v) -> start+v/total*tau) 0.
    let hovered=state.Hover |> Option.filter(fun h -> List.contains h categories)
    let slices=
        values |> List.mapi(fun index (category,value) ->
            let start,finish=angles[index],angles[index+1]
            let over=hovered=Some category
            let middle=(start+finish)/2.
            let offset=if over then Curves.radialPoint middle 8. else pt 0. 0.
            hot category
                [Leaf(fill "slice" (Shapes.arc {InnerRadius=inner;OuterRadius=(if over then outer+6. else outer);StartAngle=start;EndAngle=finish;PadAngle=0.025;CornerRadius=4.}) (categoryColor index)
                      |> Mark.withTransform (Transform.translate (cx+offset.X) (cy+offset.Y)) |> fade (emphasis hovered category))])
    let centerTitle,centerValue=
        match hovered with
        | Some category -> category,shown ("cat:"+category) state
        | None -> "Total "+state.Metric.ToLowerInvariant(),shown "total" state
    let rows=
        values |> List.mapi(fun index (category,value) ->
            hot category
                [at "row" 300. (70.+float index*36.)
                    [Leaf(rrect "swatch" 0. 0. 12. 12. 3. (categoryColor index))
                     Leaf(label "name" 20. 11. 12. Theme.text 600 "start" category |> fade (emphasis hovered category))
                     Leaf(label "share" 168. 11. 12. Theme.muted 700 "end" (percent 1 (value/total)))
                     Leaf(rrect "track" 20. 18. 148. 4. 2. Theme.raised)
                     Leaf(rrect "level" 20. 18. (148.*value/total) 4. 2. (categoryColor index))]])
    card "mix" 540. 244. 484. 300. (sprintf "%s mix by category" state.Metric)
        [group "slices" slices
         Leaf(label "center-title" cx (cy-6.) 11. Theme.muted 600 "middle" centerTitle)
         Leaf(label "center-value" cx (cy+18.) 20. Theme.text 700 "middle" (format state.Metric centerValue))
         group "rows" rows]

let private bars state =
    let nodes=KeyedTransition.sample (state.Now*1000.) state.Bars
    let hovered=state.Hover |> Option.filter(fun h -> List.contains h regions)
    let tip=
        match hovered,state.Pointer with
        | Some region,Some p ->
            let parts=categories |> List.map(fun c -> c,measure state.Metric state.Quarter region c) |> List.sortByDescending snd
            [tooltip "tip" (pt (p.X-24.) (p.Y-244.)) ((region+" · "+state.Quarter)::(parts |> List.truncate 3 |> List.map(fun (c,v) -> sprintf "%s %s" c (format state.Metric v))))]
        | _ -> []
    card "regions" 24. 244. 500. 300. (sprintf "%s by region · %s" state.Metric state.Quarter)
        ([group "bars" nodes
          Leaf(text "hint" 330. 24. 10.5 Theme.faint (if state.Sorted then "ranked · keyed transitions" else "fixed order"))]@tip)

let private monthly state =
    let x0,x1,y0,y1=56.,620.,44.,176.
    let months=["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"]
    let values=[for m in 0..11 -> (regions |> List.sumBy(fun r -> categories |> List.sumBy(fun c -> revenue r (m/3) c)))/3.*(1.+0.08*noise (70+m) 0.5)]
    let current=11
    let live=values[current]*(0.72+0.28*clamp01((shown "today" state-1_284_000.)/600_000.))
    let values=values |> List.mapi(fun i v -> if i=current then live else v)
    let maximum=List.max values*1.15
    let xs=Scale.band months (x0,x1) 0.28 0.1
    let ys=Scale.linear (0.,maximum) (y1,y0)
    let selected=quarterIndexes state.Quarter
    let bars=
        months |> List.mapi(fun i month ->
            let x=Scale.bandPosition xs month |> Option.get
            let w=Scale.bandwidth xs
            let active=List.contains (i/3) selected
            let top=Scale.map ys values[i]
            group month
                [Leaf(rrect "bar" x top w (y1-top) 3. (if active then Theme.accent else Theme.edge) |> fade (if i=current then 0.65+0.35*pulse 1.2 state.Now else 1.))
                 Leaf(label "month" (x+w/2.) (y1+14.) 10. (if active then Theme.text else Theme.faint) 600 "middle" month)])
    let target=[for i in 0..11 -> let x=(Scale.bandPosition xs months[i] |> Option.get)+Scale.bandwidth xs/2. in pt x (Scale.map ys (values |> List.averageBy id |> fun a -> a*(0.9+0.02*float i)))]
    card "monthly" 24. 560. 640. 216. "Monthly revenue vs target ($M)"
        [yGrid "grid" ys (Scale.ticks 3 ys) x0 x1 (num 0)
         group "bars" bars
         Leaf(stroke "target" (smooth target) Theme.warn 1.8 |> fade 0.9)
         legend "legend" 420. 24. ["Selected",Theme.accent;"Target",Theme.warn]]

let private feed state =
    let rows=
        state.Orders |> List.mapi(fun index order ->
            let age=state.Now-order.At
            let enter=outCubic(clamp01(age/0.5))
            at (sprintf "o%d" order.Id) (16.+(1.-enter)*40.) (48.+float index*24.)
                [Leaf(dot "dot" (pt 4. -4.) 3.5 (categoryColor (List.findIndex ((=) order.Category) categories)) |> fade enter)
                 Leaf(label "region" 16. 0. 11.5 Theme.text 600 "start" order.Region |> fade enter)
                 Leaf(label "category" 128. 0. 11. Theme.muted 400 "start" order.Category |> fade enter)
                 Leaf(label "amount" 312. 0. 11.5 Theme.good 700 "end" ("$"+grouped order.Amount) |> fade enter)])
    card "orders" 680. 560. 344. 216. "Live orders" [group "rows" (rows |> List.truncate 7)]

let view state =
    let tile=(1000.-48.)/4.
    let quota=shown "quota" state
    [header "Revenue overview" "Eight regions × five categories. Changing quarter or metric re-ranks bars with keyed mark transitions and tweens the donut; orders stream in live." "Pick a quarter (←/→) and metric · Sort toggles ranking (S) · hover bars, slices or rows"
     group "controls"
        (segmented "quarter" 24. 84. 46. [for q in quarters@["FY"] -> q,q] state.Quarter
         @ segmented "metric" 296. 84. 72. ["Revenue","Revenue";"Units","Units";"Margin","Margin"] state.Metric
         @ [button "sort" 536. 84. 92. (if state.Sorted then "Ranked" else "Fixed order") state.Sorted])
     kpi "revenue" 24. 124. tile 104. (sprintf "Revenue · %s" state.Quarter) ("$"+num 1 (shown "revenue" state)+"M") (sprintf "%s of quota" (percent 0 quota)) (if quota>=1. then Theme.good else Theme.warn)
         [for q in 0..3 -> regions |> List.sumBy(fun r -> categories |> List.sumBy(revenue r q))] Theme.accent
     kpi "today" (24.+tile+16.) 124. tile 104. "Revenue today" ("$"+grouped (shown "today" state)) (sprintf "%s orders" (grouped (shown "count" state))) Theme.good
         (state.Orders |> List.rev |> List.map(fun o -> o.Amount)) Theme.good
     kpi "avg" (24.+2.*(tile+16.)) 124. tile 104. "Average order" ("$"+grouped (shown "avg" state)) "rolling, today" Theme.muted
         (state.Orders |> List.rev |> List.scan(fun (sum,n) o -> sum+o.Amount,n+1.) (0.,0.) |> List.tail |> List.map(fun (s,n) -> s/n)) Theme.violet
     at "quota" (24.+3.*(tile+16.)) 124.
        [Leaf(rrect "bg" 0. 0. tile 104. 10. Theme.panel |> Mark.withStroke (paint Theme.edge) 1.)
         Leaf(label "title" 16. 24. 11. Theme.muted 700 "start" "QUOTA ATTAINMENT")
         gauge "gauge" (tile-64.) 60. 40. 9. (quota/1.2) (if quota>=1. then Theme.good else Theme.warn) (percent 0 quota) ""
         Leaf(label "note" 16. 62. 12. Theme.text 600 "start" (if quota>=1. then "Above plan" else "Below plan"))
         Leaf(label "sub" 16. 82. 11. Theme.muted 400 "start" (if state.Quarter="FY" then "$245M annual quota" else "$62M quarterly quota"))]
     bars state
     donut state
     monthly state
     feed state]

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="sales";Title="Revenue overview";Summary="Keyed re-ranking, donut, orders";Start=fun () -> Live.start model}

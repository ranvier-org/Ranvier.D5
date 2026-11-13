/// <summary>Revenue overview: KPIs that count to new values, regional bars re-ranked with keyed mark transitions, a tweened category
/// donut, monthly revenue against target and a live order feed.</summary>
module Ranvier.D5.Dashboards.Sales

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type Order = { Id:int; Region:string; Category:string; Amount:float; At:float }

/// <summary>A streaming sparkline: target values, displayed values easing toward them, a decaying value range and a scroll offset in
/// points that jumps by one when a value arrives and eases back to zero, so the line slides left instead of jumping.</summary>
type Trend = { Targets:float list; Display:float list; Range:Extent; Shift:float }

type State =
    { Now:float; Quarter:string; Metric:string; Sorted:bool; Bars:KeyedTransitionState
      Orders:Order list; OrderId:int; NextOrder:float; Today:float; TodayCount:int; Rng:RandomState
      Recent:float list; Spend:Trend; Average:Trend; Slots:Map<string,float>
      Shown:Map<string,float>; Hover:string option; Pointer:Point option; Focus:Map<string,float> }

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

// ---------------------------------------------------------------- streaming sparklines

/// <summary>Intervals visible across a sparkline; a few more points are kept off the left edge while the line scrolls.</summary>
let private span=20
let private kept=span+6
/// <summary>Orders averaged by the rolling average-order trend.</summary>
let private rollingWindow=10

let private trendOf (values:float list) = {Targets=values;Display=values;Range=Extent.ofRange (List.min values,List.max values);Shift=0.}

/// <summary>Appends a value; it enters at the previous displayed level and grows toward its target while the line slides left.</summary>
let private pushTrend value (trend:Trend) =
    let from=List.tryLast trend.Display |> Option.defaultValue value
    {trend with Targets=push kept value trend.Targets;Display=push kept from trend.Display;Shift=min 4. (trend.Shift+1.)}

let private tickTrend dt (trend:Trend) =
    let shown=List.map2 (fun current target -> approach dt 300. current target) trend.Display trend.Targets
    let visible=shown |> List.skip (max 0 (shown.Length-span-2))
    {trend with Display=shown;Shift=approach dt 260. trend.Shift 0.;Range=Extent.update dt 2500. (List.min visible,List.max visible) trend.Range}

/// <summary>Area-and-line sparkline of a streaming trend, clipped to its box: the newest point sits on the right edge once settled and
/// enters from beyond it.</summary>
let private trendMarks key (trend:Trend) x y w h color =
    let count=trend.Display.Length
    if count<2 then group key [] else
    let step=w/float span
    let low,high=Extent.padded 0.06 trend.Range
    let points=trend.Display |> List.mapi(fun index value -> pt (w-(float(count-1-index)-trend.Shift)*step) (h-(value-low)/(high-low)*h))
    at key x y
        [clipped "clip" w h
            [Leaf(fill "area" (smoothArea (points |> List.map(fun p -> pt p.X h,p))) color |> fade 0.16)
             Leaf(stroke "line" (smooth points) color 1.6)]]

/// <summary>A KPI tile like <c>Kit.kpi</c> whose sparkline is a streaming trend. When the headline (sized from <c>settled</c>, the
/// value it is counting toward, so the layout does not flip mid count) would run into the sparkline, the sparkline drops below it,
/// beside the delta line.</summary>
let private trendKpi key (r:Bounds) (title:string) (settled:string) (value:string) (delta:string) deltaColor trend color =
    let w,h=bw r,bh r
    let wide=16.+textWidth 27. settled*1.06+12.>w*0.52
    let sx,sy=if wide then max (w*0.45) (16.+textWidth 12. delta*1.06+14.),68. else w*0.52,36.
    let sh=if wide then h-sy-10. else h-54.
    at key (bx r) (by r)
        [Leaf(rrect "bg" 0. 0. w h 10. Theme.panel |> Mark.withStroke (paint Theme.edge) 1.)
         Leaf(label "title" 16. 24. 11. Theme.muted 700 "start" (title.ToUpperInvariant()))
         Leaf(label "value" 16. 58. 27. Theme.text 700 "start" value)
         Leaf(label "delta" 16. (h-14.) 12. deltaColor 600 "start" delta)
         trendMarks "trend" trend sx sy (w-14.-sx) sh color]

// ---------------------------------------------------------------- simulation

/// <summary>Keyed transition targets for the regional bars. Each region carries a one-point path at (share of the largest bar, rank),
/// which the transition interpolates, and its value label, which crossfades when the text changes; the view turns both into card
/// geometry, so the bars follow the card size.</summary>
let barMarks quarter metric sorted =
    let totals=regions |> List.map(fun region -> region,categories |> List.sumBy(measure metric quarter region))
    let order=if sorted then totals |> List.sortByDescending snd else totals
    let maximum=totals |> List.map snd |> List.max
    order |> List.mapi(fun rank (region,value) ->
        [Mark.path region (Path.ofCommands [MoveTo(pt (value/maximum) (float rank))])
         label ("value:"+region) 0. 0. 11. Theme.muted 600 "start" (format metric value)]) |> List.concat

let private targets state =
    let totals=categories |> List.map(fun c -> "cat:"+c,regions |> List.sumBy(fun r -> measure state.Metric state.Quarter r c))
    let total=totals |> List.sumBy snd
    let revenueTotal=regions |> List.sumBy(fun r -> categories |> List.sumBy(fun c -> measure "Revenue" state.Quarter r c))
    let quota=if state.Quarter="FY" then 245. else 62.
    Map.ofList (totals@["total",total;"revenue",revenueTotal;"quota",revenueTotal/quota;"today",state.Today;"count",float state.TodayCount
                        "avg",(if state.TodayCount=0 then 0. else state.Today/float state.TodayCount)])

let private retarget state =
    {state with Bars=KeyedTransition.retarget (state.Now*1000.) 750. outCubic false (barMarks state.Quarter state.Metric state.Sorted) state.Bars}

let private orderAmount rng = draw (Distribution.logNormal 7.6 0.7 |> ok) rng

let rec private arrive until state =
    if state.NextOrder>until then state else
    let at=state.NextOrder
    let pick,rng=uniform state.Rng
    let category,rng=uniform rng
    let amount,rng=orderAmount rng
    let gap,rng=draw (Distribution.exponential 2.4 |> ok) rng
    let region=regions[min 7 (int(pick*8.))]
    let category=categories[min 4 (int(category*5.))]
    let order={Id=state.OrderId+1;Region=region;Category=category;Amount=Math.Round amount;At=at}
    let recent=push rollingWindow order.Amount state.Recent
    arrive until {state with Orders=(order::state.Orders) |> List.truncate 20;OrderId=order.Id;NextOrder=at+gap;Today=state.Today+order.Amount
                             TodayCount=state.TodayCount+1;Rng=rng;Recent=recent;Spend=pushTrend order.Amount state.Spend
                             Average=pushTrend (List.average recent) state.Average}

/// <summary>Order amounts from before the dashboard opened, so both order trends start full; drawn from their own random state so
/// the live order stream is unchanged.</summary>
let private history () =
    let rec take n rng acc = if n=0 then List.rev acc else let amount,rng=orderAmount rng in take (n-1) rng (Math.Round amount::acc)
    let amounts=take (kept+rollingWindow-1) (RandomState.create 98UL) []
    let averages=[for k in rollingWindow-1..amounts.Length-1 -> amounts[k-rollingWindow+1..k] |> List.average]
    amounts |> List.skip (rollingWindow-1),averages,amounts |> List.skip (amounts.Length-rollingWindow)

let init () =
    let amounts,averages,recent=history ()
    let marks=barMarks "Q4" "Revenue" true
    let state={Now=0.;Quarter="Q4";Metric="Revenue";Sorted=true;Bars=KeyedTransition.start 0. marks;Orders=[];OrderId=4200;NextOrder=0.2
               Today=1_284_000.;TodayCount=1630;Rng=RandomState.create 99UL;Recent=recent;Spend=trendOf amounts;Average=trendOf averages;Slots=Map.empty
               Shown=Map.empty;Hover=None;Pointer=None;Focus=Map.empty}
    let state=arrive 4. {state with Now=4.}
    {state with Shown=targets state |> Map.map(fun key value -> if key.StartsWith "cat:" then value else 0.)}

let tick dt state =
    let now=state.Now+dt/1000.
    let state=arrive now {state with Now=now}
    {state with Shown=approachMap dt 420. 0. state.Shown (targets state)
                Spend=tickTrend dt state.Spend;Average=tickTrend dt state.Average
                Slots=approachMap dt 140. 0. state.Slots (state.Orders |> List.mapi(fun index order -> string order.Id,float index) |> Map.ofList)
                Focus=Focus.tick dt (state.Hover |> Option.filter(fun h -> List.contains h categories)) state.Focus}

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
/// <summary>A page point in card-local coordinates, held inside the card so a tooltip anchored on it stays in the card.</summary>
let private local (r:Bounds) (p:Point) = pt (clamp 0. (bw r) (p.X-bx r)) (clamp 0. (bh r) (p.Y-by r))

// ---------------------------------------------------------------- cards

let private donut state (r:Bounds) =
    let w,h=bw r,bh r
    let values=categories |> List.map(fun c -> c,max 0.0001 (shown ("cat:"+c) state))
    let total=values |> List.sumBy snd
    // The donut and its legend are centred together; the radius follows the space left beside the legend and the card height.
    let legendWidth,gap=168.,40.
    let outer=min ((w-legendWidth-gap-32.)/2.) ((h-64.)/2.) |> max 40.
    let inner=outer*74./118.
    let left=max 16. ((w-(2.*outer+gap+legendWidth))/2.)
    let cx,cy=left+outer,48.+(h-64.)/2.
    let legendX=cx+outer+gap
    let angles=values |> List.scan(fun start (_,v) -> start+v/total*tau) 0.
    let hovered=state.Hover |> Option.filter(fun h -> List.contains h categories)
    let any=Focus.any state.Focus
    let slices=
        values |> List.mapi(fun index (category,_) ->
            let start,finish=angles[index],angles[index+1]
            let level=Focus.level category state.Focus
            let offset=Curves.radialPoint ((start+finish)/2.) (8.*level)
            hot category
                [Leaf(fill "slice" (Shapes.arc {InnerRadius=inner;OuterRadius=outer+6.*level;StartAngle=start;EndAngle=finish;PadAngle=0.025;CornerRadius=4.}) (categoryColor index)
                      |> Mark.withTransform (Transform.translate (cx+offset.X) (cy+offset.Y)) |> fade (emphasis hovered category))])
    // The centre read-out crossfades between the total and each category with the eased hover levels.
    let centre=
        [Leaf(label "total-title" cx (cy-6.) 11. Theme.muted 600 "middle" ("Total "+state.Metric.ToLowerInvariant()) |> fade (1.-any))
         Leaf(label "total-value" cx (cy+18.) 20. Theme.text 700 "middle" (format state.Metric (shown "total" state)) |> fade (1.-any))]
        @ [for category,value in values do
             let level=Focus.level category state.Focus
             if level>0.004 then
                 yield Leaf(label ("title:"+category) cx (cy-6.) 11. Theme.muted 600 "middle" category |> fade level)
                 yield Leaf(label ("value:"+category) cx (cy+18.) 20. Theme.text 700 "middle" (format state.Metric value) |> fade level)]
    let rows=
        values |> List.mapi(fun index (category,value) ->
            let level=Focus.level category state.Focus
            hot category
                [at "row" (legendX+4.*level) (cy-96.+float index*36.)
                    [Leaf(rrect "hit" -6. -6. (legendWidth+12.) 32. 6. Theme.raised |> fade (0.01+0.5*level))
                     Leaf(rrect "swatch" 0. 0. 12. 12. 3. (categoryColor index))
                     Leaf(label "name" 20. 11. 12. Theme.text 600 "start" category |> fade (emphasis hovered category))
                     Leaf(label "share" legendWidth 11. 12. Theme.muted 700 "end" (percent 1 (value/total)))
                     Leaf(rrect "track" 20. 18. (legendWidth-20.) 4. 2. Theme.raised)
                     Leaf(rrect "level" 20. 18. ((legendWidth-20.)*value/total) 4. 2. (categoryColor index))]])
    panel "mix" r (sprintf "%s mix by category" state.Metric)
        [group "slices" slices
         overlay "centre" centre
         group "rows" rows]

let private bars state (r:Bounds) =
    let w,h=bw r,bh r
    let barLeft,barRight,barTop=128.,w-60.,48.
    let step=(h-barTop-42.)/7.
    let barHeight=min 26. (step*2./3.)
    let sampled=KeyedTransition.sample (state.Now*1000.) state.Bars
    let layers key = sampled |> List.tryPick(function Group(spec,children) when spec.Key=key -> Some children | _ -> None) |> Option.defaultValue []
    let hovered=state.Hover |> Option.filter(fun h -> List.contains h regions)
    let rows=
        regions |> List.choose(fun region ->
            let carrier=
                layers region |> List.tryPick(function
                    | Leaf mark -> (match Mark.geometry mark with PathGeometry path -> (match Path.commands path with MoveTo p::_ -> Some p | _ -> None) | _ -> None)
                    | _ -> None)
            carrier |> Option.map(fun p ->
                let share,rank=p.X,p.Y
                let y=barTop+rank*step
                let width=(barRight-barLeft)*share
                let mid=y+barHeight/2.+4.
                let opacity=match hovered with Some other when other<>region -> 0.45 | _ -> 1.
                let values=
                    layers ("value:"+region) |> List.choose(function
                        | Leaf mark ->
                            match Mark.geometry mark with
                            | TextGeometry(_,text) -> Some(Leaf(label ("v"+Mark.key mark) (barLeft+width+8.) mid 11. Theme.muted 600 "start" text |> fade ((Mark.style mark).Opacity*opacity)))
                            | _ -> None
                        | _ -> None)
                hot region
                    ([Leaf(rect "hit" 12. (y-(step-barHeight)/2.) (w-24.) step Theme.panel |> fade 0.01)
                      Leaf(rrect "bar" barLeft y width barHeight 4. (ramp [Theme.blue;Theme.accent;Theme.teal] (rank/7.)) |> fade opacity)
                      Leaf(label "name" (barLeft-10.) mid 11.5 Theme.text 600 "end" region |> fade opacity)]@values)))
    let tip=
        match hovered,state.Pointer with
        | Some region,Some p ->
            let parts=categories |> List.map(fun c -> c,measure state.Metric state.Quarter region c) |> List.sortByDescending snd
            [tooltip "tip" w h (local r p) ((region+" · "+state.Quarter)::(parts |> List.truncate 3 |> List.map(fun (c,v) -> sprintf "%s %s" c (format state.Metric v))))]
        | _ -> []
    panel "regions" r (sprintf "%s by region · %s" state.Metric state.Quarter)
        ([group "bars" rows
          Leaf(label "hint" (w-16.) 24. 10.5 Theme.faint 400 "end" (if state.Sorted then "ranked · keyed transitions" else "fixed order"))]@tip)

let private monthly state (r:Bounds) =
    let w,h=bw r,bh r
    let x0,x1,y0,y1=56.,w-20.,44.,h-40.
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
            let bw=Scale.bandwidth xs
            let active=List.contains (i/3) selected
            let top=Scale.map ys values[i]
            group month
                [Leaf(rrect "bar" x top bw (y1-top) 3. (if active then Theme.accent else Theme.edge) |> fade (if i=current then 0.65+0.35*pulse 1.2 state.Now else 1.))
                 Leaf(label "month" (x+bw/2.) (y1+14.) 10. (if active then Theme.text else Theme.faint) 600 "middle" month)])
    let target=[for i in 0..11 -> let x=(Scale.bandPosition xs months[i] |> Option.get)+Scale.bandwidth xs/2. in pt x (Scale.map ys (values |> List.averageBy id |> fun a -> a*(0.9+0.02*float i)))]
    panel "monthly" r "Monthly revenue vs target ($M)"
        [yGrid "grid" ys (Scale.ticks 3 ys) x0 x1 (num 0)
         group "bars" bars
         Leaf(stroke "target" (smooth target) Theme.warn 1.8 |> fade 0.9)
         legend "legend" (w-220.) 24. ["Selected",Theme.accent;"Target",Theme.warn]]

let private feed state (r:Bounds) =
    let w,h=bw r,bh r
    // As many rows as the card holds; older rows ease down as a new order pushes in at the top.
    let capacity=int((h-48.-16.)/24.)+1
    let categoryX=max 128. ((w-32.)*0.4)
    let rows=
        state.Orders |> List.truncate (capacity+1) |> List.mapi(fun index order ->
            let age=state.Now-order.At
            let enter=outCubic(clamp01(age/0.5))
            let slot=Map.tryFind (string order.Id) state.Slots |> Option.defaultValue 0.
            let y=48.+slot*24.
            let leaving=clamp01(1.-(slot-float(capacity-1))*1.6)
            // The region and category slide in from the right; the amount column fades in place so it never leaves the card.
            at (sprintf "o%d" order.Id) 16. y
                [at "slide" ((1.-enter)*40.) 0.
                    [Leaf(dot "dot" (pt 4. -4.) 3.5 (categoryColor (List.findIndex ((=) order.Category) categories)) |> fade (enter*leaving))
                     Leaf(label "region" 16. 0. 11.5 Theme.text 600 "start" order.Region |> fade (enter*leaving))
                     Leaf(label "category" categoryX 0. 11. Theme.muted 400 "start" order.Category |> fade (enter*leaving))]
                 Leaf(label "amount" (w-32.) 0. 11.5 Theme.good 700 "end" ("$"+grouped order.Amount) |> fade (enter*leaving))])
    panel "orders" r "Live orders" [group "rows" rows]

let private quotaTile state (r:Bounds) =
    let w,h=bw r,bh r
    let quota=shown "quota" state
    let sub=
        match state.Quarter="FY",w>=236. with
        | true,true -> "$245M annual quota"
        | false,true -> "$62M quarterly quota"
        | true,false -> "$245M quota"
        | false,false -> "$62M quota"
    at "quota" (bx r) (by r)
        [Leaf(rrect "bg" 0. 0. w h 10. Theme.panel |> Mark.withStroke (paint Theme.edge) 1.)
         Leaf(label "title" 16. 24. 11. Theme.muted 700 "start" "QUOTA ATTAINMENT")
         gauge "gauge" (w-56.) (h*0.58) 40. 9. (quota/1.2) (if quota>=1. then Theme.good else Theme.warn) (percent 0 quota) ""
         Leaf(label "note" 16. (h/2.+10.) 12. Theme.text 600 "start" (if quota>=1. then "Above plan" else "Below plan"))
         Leaf(label "sub" 16. (h/2.+30.) 11. Theme.muted 400 "start" sub)]

let view viewport state =
    let quota=shown "quota" state
    page viewport "Revenue overview"
        "Eight regions × five categories. Quarter and metric changes re-rank bars with keyed transitions and tween the donut; orders stream in live."
        "Pick a quarter (←/→) and metric · Sort toggles ranking (S) · hover bars, slices or legend rows"
        [segmentedTool "quarter" 46. [for q in quarters@["FY"] -> q,q] state.Quarter
         segmentedTool "metric" 72. ["Revenue","Revenue";"Units","Units";"Margin","Margin"] state.Metric
         buttonTool "sort" 92. (if state.Sorted then "Ranked" else "Fixed order") state.Sorted]
        []
        [["revenue";"today";"avg";"quota"] |> List.map(fun key -> item 238. 180. (cell key 104.))
         [item 500. 360. (cell "regions" 300.);item 484. 380. (cell "mix" 300.)]
         [item 640. 440. (cell "monthly" 216.);item 344. 300. (cell "orders" 216.)]]
        (fun cells ->
            let tile key = let r=cells[key] in bx r,by r,bw r,bh r
            [(let x,y,w,h=tile "revenue" in
              kpi "revenue" x y w h (sprintf "Revenue · %s" state.Quarter) ("$"+num 1 (shown "revenue" state)+"M") (sprintf "%s of quota" (percent 0 quota)) (if quota>=1. then Theme.good else Theme.warn)
                  [for q in 0..3 -> regions |> List.sumBy(fun r -> categories |> List.sumBy(revenue r q))] Theme.accent)
             trendKpi "today" cells["today"] "Revenue today" ("$"+grouped state.Today) ("$"+grouped (shown "today" state)) (sprintf "%s orders" (grouped (shown "count" state))) Theme.good state.Spend Theme.good
             trendKpi "avg" cells["avg"] "Average order" ("$"+grouped (state.Today/float state.TodayCount)) ("$"+grouped (shown "avg" state)) "rolling, today" Theme.muted state.Average Theme.violet
             quotaTile state cells["quota"]
             bars state cells["regions"]
             donut state cells["mix"]
             monthly state cells["monthly"]
             feed state cells["orders"]])

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="sales";Title="Revenue overview";Summary="Keyed re-ranking, donut, orders";Start=fun () -> Live.start model}

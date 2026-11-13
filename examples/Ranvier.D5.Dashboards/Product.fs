/// <summary>Product analytics: a conversion funnel with users flowing and dropping out, a cohort retention heatmap that reveals
/// diagonally, live active users and a self-sorting traffic-source race.</summary>
module Ranvier.D5.Dashboards.Product

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type State =
    { Now:float; Segment:string; Range:string; Live:bool; Revealed:float; Active:float list; NextSample:float
      Axis:Extent; Shown:Map<string,float>; Hover:string option; Pointer:Point option }

let stages=["Visit";"Product view";"Add to cart";"Checkout";"Purchase"]
let private sources=["Organic search";"Paid search";"Social";"Email";"Referral";"Direct"]
let private sourceColors=[|Theme.accent;Theme.violet;Theme.pink;Theme.warn;Theme.teal;Theme.blue|]

/// <summary>Stage counts for a segment and range; mobile brings more visits and converts less.</summary>
let funnel segment range (now:float) =
    let scale=if range="30d" then 4.1 else 1.
    let visits,rates=
        match segment with
        | "Mobile" -> 98_000.,[0.58;0.31;0.52;0.61]
        | "Desktop" -> 54_000.,[0.66;0.42;0.63;0.74]
        | _ -> 152_000.,[0.61;0.35;0.56;0.66]
    let wobble i = 1.+0.03*fbm (60+i) (now/9.)
    rates |> List.mapi(fun i r -> r*wobble i) |> List.scan (*) (visits*scale*wobble 9)

/// <summary>Share of a weekly cohort still active after <c>week</c> weeks.</summary>
let retention segment cohort week =
    let plateau=match segment with "Mobile" -> 0.31 | "Desktop" -> 0.42 | _ -> 0.36
    if week=0 then 1. else plateau+(1.-plateau)*exp(-float week/0.9)*0.55+0.04*noise (cohort*7+week) 0.3-0.006*float week+0.012*float cohort

let private sourceShare segment (now:float) =
    let bias=match segment with "Mobile" -> [|0.8;0.9;1.9;0.7;0.8;0.8|] | "Desktop" -> [|1.2;1.1;0.5;1.3;1.1;1.2|] | _ -> [|1.;1.;1.;1.;1.;1.|]
    sources |> List.mapi(fun i s -> s,[|0.31;0.18;0.16;0.11;0.09;0.15|][i]*bias[i]*(1.+0.35*fbm (80+i) (now/7.)))

let private targets state =
    let counts=funnel state.Segment state.Range state.Now
    let shares=sourceShare state.Segment state.Now
    let total=shares |> List.sumBy snd
    let ranked=shares |> List.sortByDescending snd |> List.mapi(fun rank (s,_) -> "rank:"+s,float rank)
    Map.ofList ((stages |> List.mapi(fun i s -> "stage:"+s,counts[i]))@(shares |> List.map(fun (s,v) -> "share:"+s,v/total))@ranked
                @["active",List.last state.Active;"conversion",List.last counts/counts.Head])

/// <summary>The active-users axis range for the current samples, before the decaying extent smooths it.</summary>
let private activeRange (values:float list) = 0.,List.max values*1.2

let init () =
    let active=[for k in 0..89 -> 1840.+260.*fbm 5 (float k/14.)]
    let state={Now=30.;Segment="All";Range="7d";Live=true;Revealed= -10.;Active=active;NextSample=31.
               Axis=Extent.ofRange (activeRange active);Shown=Map.empty;Hover=None;Pointer=None}
    {state with Shown=targets state}

let tick dt state =
    let now=state.Now+dt/1000.
    let sample=state.Live && now>=state.NextSample
    let factor=match state.Segment with "Mobile" -> 0.64 | "Desktop" -> 0.36 | _ -> 1.
    let state={state with Now=now
                          Active=(if sample then push 90 (factor*(1840.+260.*fbm 5 (now/2.)+90.*noise 6 (now*1.7))) state.Active else state.Active)
                          NextSample=(if sample then state.NextSample+0.5 else state.NextSample)}
    {state with Shown=approachMap dt 420. 0. state.Shown (targets state);Axis=Extent.update dt 1500. (activeRange state.Active) state.Axis}

let input message state =
    match message with
    | Input.Click id when id.StartsWith "segment:" -> {state with Segment=id.Substring 8;Revealed=state.Now}
    | Input.Click id when id.StartsWith "range:" -> {state with Range=id.Substring 6;Revealed=state.Now}
    | Input.Click "live" | Input.Key " " -> {state with Live=not state.Live}
    | Input.Key "ArrowRight" -> {state with Segment=(match state.Segment with "All" -> "Mobile" | "Mobile" -> "Desktop" | _ -> "All");Revealed=state.Now}
    | Input.Key "ArrowLeft" -> {state with Segment=(match state.Segment with "All" -> "Desktop" | "Desktop" -> "Mobile" | _ -> "All");Revealed=state.Now}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private shown key state = Map.tryFind key state.Shown |> Option.defaultValue 0.
/// <summary>A page point in card-local coordinates, held inside the card so a tooltip anchored on it stays in the card.</summary>
let private local (r:Bounds) (p:Point) = pt (clamp 0. (bw r) (p.X-bx r)) (clamp 0. (bh r) (p.Y-by r))

let private funnelCard state (r:Bounds) =
    let w,h=bw r,bh r
    let counts=stages |> List.map(fun s -> shown ("stage:"+s) state)
    let top=max 1. counts.Head
    // Stage names hold the left column and conversion rates the right; the funnel is centred between them.
    let full=min 520. (w-170.)
    let cx=(w+84.)/2.
    let y0=46.
    let step=(h-y0-34.-34.)/4.
    let height=min 48. (step*0.63)
    let width i = full*sqrt(counts[i]/top)
    let focus=state.Hover |> Option.filter(fun h -> h.StartsWith "stage:") |> Option.map(fun h -> h.Substring 6)
    let bands=
        stages |> List.mapi(fun i stage ->
            let w=width i
            let y=y0+float i*step
            let mid=y+height/2.+5.
            let connector=
                if i<stages.Length-1 then
                    let next=width (i+1)
                    [Leaf(fill "neck" (polygon [pt (cx-w/2.) (y+height);pt (cx+w/2.) (y+height);pt (cx+next/2.) (y+step);pt (cx-next/2.) (y+step)]) Theme.accent |> fade 0.08)]
                else []
            let rate=if i=0 then 1. else counts[i]/counts[i-1]
            hot ("stage:"+stage)
                ([Leaf(rrect "band" (cx-w/2.) y w height 6. (ramp [Theme.accent;Theme.violet;Theme.pink] (float i/4.)) |> fade (0.28*emphasis focus stage+0.1))
                  Leaf(label "name" 16. mid 11.5 Theme.text 600 "start" stage)
                  Leaf(label "count" cx mid 13. Theme.text 700 "middle" (si 1 counts[i]))
                  Leaf(label "rate" (cx+w/2.+10.) mid 11. (if i=0 then Theme.muted elif rate>0.5 then Theme.good else Theme.warn) 600 "start" (if i=0 then "100%" else percent 0 rate))]@connector))
    // Each particle walks down the funnel and survives each step with that step's conversion rate; drop-outs peel away and fade.
    let rates=[for i in 1..4 -> counts[i]/max 1. counts[i-1]]
    let particles=
        [for p in 0..69 do
            let travel=(state.Now*0.16+float p/70.)
            let cycle=int(floor travel)
            let progress=travel-float cycle
            let depth=progress*4.6
            let lane=hash (p*13+cycle) 7-0.5
            let survived=[for i in 0..3 -> hash (p*31+cycle) (i+1)<rates[i]] |> List.takeWhile id |> List.length
            let stageAt=int(min 4. depth)
            let dropped=stageAt>survived
            let y=y0+height/2.+depth*step
            if y<y0+float(stages.Length)*step then
                let w=width (min 4 stageAt)
                if dropped then
                    let since=depth-float(survived+1)
                    let side=if lane>=0. then 1. else -1.
                    yield Leaf(dot (sprintf "p%d" p) (pt (cx+side*(w/2.+since*70.)) (y0+float(survived+1)*step-6.+since*14.)) 2.4 Theme.bad |> fade (0.8-since*1.4))
                else
                    yield Leaf(dot (sprintf "p%d" p) (pt (cx+lane*(w-16.)) y) 2.6 Theme.text |> fade 0.85)]
    let tip=
        match focus,state.Pointer with
        | Some stage,Some p ->
            let i=List.findIndex ((=) stage) stages
            [tooltip "tip" w h (local r p) [stage;sprintf "%s users" (grouped counts[i]);(if i=0 then "Top of funnel" else sprintf "%s from %s" (percent 1 (counts[i]/counts[i-1])) stages[i-1]);sprintf "%s of visits" (percent 1 (counts[i]/top))]]
        | _ -> []
    panel "funnel" r (sprintf "Conversion funnel · %s · %s" state.Segment state.Range)
        ([group "bands" bands;overlay "particles" particles]@tip)

let private cohorts state (r:Bounds) =
    let w,h=bw r,bh r
    // Cells share the space right of the row labels and below the column labels, wide cells capped so the grid stays square-ish and centred.
    let y0=52.
    let cellH=(h-y0-12.)/10.
    let cellW=min ((w-96.-48.)/10.) (cellH*1.9)
    let x0=96.+max 0. ((w-96.-48.)-10.*cellW)/2.
    let reveal=state.Now-state.Revealed
    let focus=state.Hover |> Option.filter(fun h -> h.StartsWith "cell:")
    let cells=
        [for c in 0..9 do
            for wk in 0..9-c do
                let value=retention state.Segment c wk
                let appear=outCubic(clamp01((reveal-float(c+wk)*0.07)/0.45))
                let id=sprintf "cell:%d:%d" c wk
                let x,y=x0+float wk*cellW,y0+float c*cellH
                yield hot id
                    [Leaf(rrect "cell" (x+(1.-appear)*6.) y (cellW-3.) (cellH-3.) 3. (ramp Theme.cool value)
                          |> fade (appear*(if focus.IsNone || focus=Some id then 1. else 0.55)))
                     Leaf(label "v" (x+cellW/2.-1.5) (y+cellH/2.+2.) 8.5 (if value>0.6 then Theme.background else Theme.text) 600 "middle" (num 0 (value*100.)) |> fade (if wk>0 then appear else 0.))]]
    let rows=[for c in 0..9 -> Leaf(label (sprintf "r%d" c) (x0-8.) (y0+float c*cellH+cellH/2.+3.) 10. Theme.muted 400 "end" (sprintf "Week %d" (c+1)))]
    let columns=[for wk in 0..9 -> Leaf(label (sprintf "w%d" wk) (x0+float wk*cellW+cellW/2.-1.5) (y0-8.) 9.5 Theme.faint 400 "middle" (sprintf "W%d" wk))]
    let tip=
        match focus,state.Pointer with
        | Some id,Some p ->
            let parts=id.Split ':'
            let c,wk=int parts[1],int parts[2]
            [tooltip "tip" w h (local r p) [sprintf "Cohort week %d" (c+1);sprintf "Week %d retention" wk;percent 1 (retention state.Segment c wk)]]
        | _ -> []
    panel "cohorts" r (sprintf "Weekly retention · %s" state.Segment)
        ([group "cells" cells;group "rows" rows;group "columns" columns]@tip)

let private activeCard state (r:Bounds) =
    let x0,x1,y0,y1=48.,bw r-24.,44.,bh r-26.
    let values=state.Active
    let xs=Scale.linear (0.,float(values.Length-1)) (x0,x1)
    let ys=Scale.linear (state.Axis.Low,state.Axis.High) (y1,y0)
    let offset=if state.Live then (state.NextSample-state.Now)/0.5 else 0.
    let points=values |> List.mapi(fun i v -> pt (Scale.map xs (float i+offset-1.) |> clamp x0 x1) (Scale.map ys v))
    let last=List.last points
    panel "active-users" r "Active users · last 45 s"
        [yGrid "grid" ys (Scale.ticks 3 ys) x0 x1 (si 1)
         Leaf(fill "area" (smoothArea (points |> List.map(fun p -> pt p.X y1,p))) Theme.accent |> fade 0.18)
         Leaf(stroke "line" (smooth points) Theme.accent 2.)
         Leaf(dot "head" last (3.5+2.*pulse 0.8 state.Now) Theme.accent)
         Leaf(ring "head-ring" last (6.+10.*((state.Now*1.2)%1.)) Theme.accent 1.5 |> fade (1.-(state.Now*1.2)%1.))]

let private sourcesCard state (r:Bounds) =
    let w,h=bw r,bh r
    let barMax=w-194.
    let step=max 22. ((h-44.-32.)/5.)
    let rows=
        sources |> List.mapi(fun i source ->
            let share=shown ("share:"+source) state
            let y=44.+shown ("rank:"+source) state*step
            at ("src:"+source) 16. y
                [Leaf(label "name" 0. 12. 11. Theme.text 600 "start" source)
                 Leaf(rrect "bar" 118. 2. (barMax*share/0.45) 13. 3. sourceColors[i] |> fade 0.85)
                 Leaf(label "pct" (124.+barMax*share/0.45) 13. 10.5 Theme.muted 600 "start" (percent 1 share))])
    panel "sources" r "Traffic sources · live share" rows

let view viewport state =
    let counts=funnel state.Segment state.Range state.Now
    page viewport "Product analytics"
        "Users stream through a five-step funnel and drop out at each step's real conversion rate; cohorts reveal diagonally when the segment changes."
        "Segment (←/→) · range · pause live data (Space) · hover funnel stages and cohort cells"
        [segmentedTool "segment" 72. ["All","All users";"Mobile","Mobile";"Desktop","Desktop"] state.Segment
         segmentedTool "range" 48. ["7d","7 d";"30d","30 d"] state.Range
         buttonTool "live" 70. (if state.Live then "Live" else "Paused") state.Live]
        []
        [["active";"visits";"conversion";"orders"] |> List.map(fun key -> item 238. 180. (cell key 104.))
         [item 500. 380. (cell "funnel" 330.);item 484. 400. (cell "cohorts" 330.)]
         [item 500. 340. (cell "active-users" 186.);item 484. 380. (cell "sources" 186.)]]
        (fun cells ->
            let tile key = let r=cells[key] in bx r,by r,bw r,bh r
            [(let x,y,w,h=tile "active" in
              kpi "active" x y w h "Active now" (grouped (shown "active" state)) "● streaming" Theme.good (state.Active |> List.skip 60) Theme.accent)
             (let x,y,w,h=tile "visits" in
              kpi "visits" x y w h (sprintf "Visits · %s" state.Range) (si 1 (shown "stage:Visit" state)) (sprintf "%s · %s" state.Segment state.Range) Theme.muted [for k in 0..20 -> counts.Head*(1.+0.05*noise 3 (float k/3.))] Theme.violet)
             (let x,y,w,h=tile "conversion" in
              kpi "conversion" x y w h "Conversion" (percent 2 (shown "conversion" state)) "visit → purchase" Theme.good [for k in 0..20 -> 0.03+0.004*noise 4 (float k/3.)] Theme.good)
             (let x,y,w,h=tile "orders" in
              kpi "orders" x y w h "Purchases" (grouped (shown "stage:Purchase" state)) (sprintf "$%s revenue" (si 1 (shown "stage:Purchase" state*86.))) Theme.good [for k in 0..20 -> List.last counts*(1.+0.06*noise 8 (float k/3.))] Theme.pink)
             funnelCard state cells["funnel"]
             cohorts state cells["cohorts"]
             activeCard state cells["active-users"]
             sourcesCard state cells["sources"]])

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="product";Title="Product analytics";Summary="Funnel flow, cohorts, live users";Start=fun () -> Live.start model}

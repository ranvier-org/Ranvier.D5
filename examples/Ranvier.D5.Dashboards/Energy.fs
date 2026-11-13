/// <summary>Power grid mix: a 24-hour stacked generation profile with a sweeping clock, a live Sankey of flows from sources to sectors,
/// capacity rings and grid-status gauges with a streaming frequency trace.</summary>
module Ranvier.D5.Dashboards.Energy

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type State =
    { Now:float; Hour:float; Day:int; Playing:bool; Speed:float; Scenario:string; Frequency:float list; FrequencyAxis:Extent
      Hover:string option; Pointer:Point option; Shown:Map<string,float> }

let sources=["Nuclear";"Coal";"Gas";"Hydro";"Wind";"Solar";"Imports"]
let private capacity=Map.ofList ["Nuclear",7.;"Coal",6.;"Gas",16.;"Hydro",5.;"Wind",14.;"Solar",13.;"Imports",4.]
let private colors=Map.ofList ["Nuclear",Theme.violet;"Coal",(140,130,124);"Gas",Theme.orange;"Hydro",Theme.blue;"Wind",Theme.teal;"Solar",Theme.warn;"Imports",Theme.pink]
let sectors=["Residential";"Commercial";"Industrial";"EV charging";"Storage"]
let private bump centre width (hour:float) = let z=(hour-centre)/width in exp(-(z*z))

/// <summary>Demand in GW for an hour, day and scenario.</summary>
let demand scenario day hour =
    let heat=if scenario="Heatwave" then 1. else 0.
    24.+5.*bump 8.5 2.2 hour+7.*bump 19. 2.8 hour-4.*bump 3.5 3. hour+heat*7.*bump 15.5 3.5 hour+0.7*fbm (day*7+3) (hour/3.)

/// <summary>Generation by source in GW; gas balances what the other sources leave unmet.</summary>
let generation scenario day hour =
    let windy=scenario="Windy"
    let solar=13.*(max 0. (sin(Math.PI*(hour-6.)/13.)))**1.3*(0.72+0.28*(0.5+0.5*fbm (day*11+5) (hour/2.)))*(if scenario="Heatwave" then 1.1 else 1.)
    let wind=(if windy then 10.5 else 3.2)+(if windy then 2.6 else 1.6)*fbm (day*13+9) ((float day*24.+hour)/4.)
    let nuclear=6.6
    let hydro=2.2+2.2*bump 19. 3. hour+0.9*bump 8. 2. hour
    let imports=1.4+0.6*fbm (day*17+1) (hour/5.)
    let coal=if windy then 1.2 else 3.4
    let need=demand scenario day hour
    let gas=max 0.4 (need-(nuclear+coal+hydro+wind+solar+imports))
    Map.ofList ["Nuclear",nuclear;"Coal",coal;"Gas",gas;"Hydro",hydro;"Wind",wind;"Solar",max 0. solar;"Imports",imports]

let private carbon (mix:Map<string,float>) =
    let total=mix |> Map.toSeq |> Seq.sumBy snd
    (mix["Coal"]*910.+mix["Gas"]*420.+mix["Imports"]*260.)/total
let private renewable (mix:Map<string,float>) = (mix["Hydro"]+mix["Wind"]+mix["Solar"])/(mix |> Map.toSeq |> Seq.sumBy snd)

let private targets state =
    let mix=generation state.Scenario state.Day state.Hour
    let supply=mix |> Map.toSeq |> Seq.sumBy snd
    Map.ofList ([for KeyValue(k,v) in mix -> k,v]@["carbon",carbon mix;"renewable",renewable mix;"demand",demand state.Scenario state.Day state.Hour;"supply",supply;"price",38.+5.5*mix["Gas"]+(if state.Scenario="Heatwave" then 25. else 0.)])

/// <summary>How long the frequency axis takes to relax halfway toward the visible range after an extreme scrolls out, in milliseconds.</summary>
let private frequencyHalfLife=6000.
let private range (values:float list) = List.min values,List.max values

let init () =
    let state={Now=0.;Hour=13.25;Day=3;Playing=true;Speed=1.;Scenario="Calm";Frequency=[];FrequencyAxis=Extent.ofRange (50.,50.)
               Hover=None;Pointer=None;Shown=Map.empty}
    let shown=targets state
    // The history starts at the opening imbalance, so the live trace continues it instead of stepping away from 50 Hz.
    let imbalance=(shown["supply"]-shown["demand"])*0.01
    let frequency=[for i in 0..119 -> 50.+imbalance+0.02*fbm 5 (float i/12.)]
    {state with Shown=shown;Frequency=frequency;FrequencyAxis=Extent.ofRange (range frequency)}

let tick dt state =
    let now=state.Now+dt/1000.
    let hour,day=
        if state.Playing then
            let next=state.Hour+dt/1000./3.*state.Speed
            if next>=24. then next-24.,state.Day+1 else next,state.Day
        else state.Hour,state.Day
    let state={state with Now=now;Hour=hour;Day=day}
    let shown=approachMap dt 300. 0. state.Shown (targets state)
    let imbalance=(shown["supply"]-shown["demand"])*0.01
    let hz=50.+imbalance+0.018*fbm 77 (now*2.6)+0.006*noise 78 (now*11.)
    let frequency=push 120 hz state.Frequency
    {state with Shown=shown;Frequency=frequency;FrequencyAxis=Extent.update dt frequencyHalfLife (range frequency) state.FrequencyAxis}

let input message state =
    match message with
    | Input.Click "play" | Input.Key " " -> {state with Playing=not state.Playing}
    | Input.Click id when id.StartsWith "speed:" -> {state with Speed=float(id.Substring 6)}
    | Input.Click id when id.StartsWith "scenario:" -> {state with Scenario=id.Substring 9}
    | Input.Key "ArrowRight" -> {state with Hour=(state.Hour+1.)%24.}
    | Input.Key "ArrowLeft" -> {state with Hour=(state.Hour+23.)%24.}
    | Input.Pick id when id.StartsWith "src:" -> state
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private shown key state = Map.tryFind key state.Shown |> Option.defaultValue 0.
let private hoveredSource state = state.Hover |> Option.filter(fun h -> h.StartsWith "src:") |> Option.map(fun h -> h.Substring 4)
let private clock (hour:float) = sprintf "%02d:%02d" (int hour) (int((hour-floor hour)*60.))

let private profile state (r:Bounds) =
    let w,h=bw r,bh r
    let x0,x1,y0,y1=48.,w-20.,44.,h-40.
    let xs=Scale.linear (0.,24.) (x0,x1)
    let ys=Scale.linear (0.,46.) (y1,y0)
    let hours=[for i in 0..96 -> float i/4.]
    let split=state.Hour
    let past=(hours |> List.filter(fun h -> h<split))@[split]
    let future=split::(hours |> List.filter(fun h -> h>split))
    let layers (times:float list) =
        let mixes=times |> List.map(generation state.Scenario state.Day)
        Stack.layout {Order=StackOrder.Input;Offset=StackOffset.Zero} [for s in sources -> {Key=s;Values=mixes |> List.map(fun m -> m[s])}] |> ok
        |> List.map(fun layer -> layer.Key,List.zip times layer.Values |> List.map(fun (t,(low,high)) -> pt (Scale.map xs t) (Scale.map ys low),pt (Scale.map xs t) (Scale.map ys high)))
    let focus=hoveredSource state
    // Each source's stacked band is its own hoverable shape; everything drawn over the bands lives in overlays.
    let areas name times opacity =
        group name [for key,pairs in layers times ->
                      hot ("src:"+key) [Leaf(fill "area" (smoothArea pairs) colors[key] |> fade (opacity*emphasis focus key))]]
    let demandLine=smooth [for h in hours -> pt (Scale.map xs h) (Scale.map ys (demand state.Scenario state.Day h))]
    let x=Scale.map xs state.Hour
    let total=generation state.Scenario state.Day state.Hour |> Map.toSeq |> Seq.sumBy snd
    let tag=clamp 30. (w-30.) x
    let inspect=
        match state.Pointer with
        | Some p when p.X-bx r>=x0 && p.X-bx r<=x1 && p.Y-by r>=y0 && p.Y-by r<=y1 ->
            let local=pt (p.X-bx r) (p.Y-by r)
            let hour=Scale.invert xs local.X
            let mix=generation state.Scenario state.Day hour
            let marker=
                match focus with
                | Some s when List.contains s sources ->
                    let below=sources |> List.takeWhile ((<>) s) |> List.sumBy(fun k -> mix[k])
                    [Leaf(ring "focus" (pt local.X (Scale.map ys (below+mix[s]/2.))) 4.5 colors[s] 2.)]
                | _ -> []
            [overlay "inspect" (Leaf(stroke "line" (segment (pt local.X y0) (pt local.X y1)) Theme.text 1. |> fade 0.5)::marker)
             tooltip "tip" w h local
                ((sprintf "%s · demand %.1f GW" (clock hour) (demand state.Scenario state.Day hour))::
                 (sources |> List.rev |> List.map(fun s -> sprintf "%s%-8s %5.1f GW" (if focus=Some s then "▸ " else "") s mix[s])))]
        | _ -> []
    panel "profile" r (sprintf "Generation profile · day %d · %s" state.Day state.Scenario)
        ([yGrid "grid" ys [0.;10.;20.;30.;40.] x0 x1 (fun v -> num 0 v+" GW")
          areas "past" past 0.92
          areas "forecast" future 0.32
          xLabels "hours" (y1+16.) [for h in 0..4..24 -> Scale.map xs (float h),sprintf "%02d:00" (h%24)]
          legend "legend" (w-150.) 24. ["Demand",Theme.text;"Forecast",Theme.faint]
          overlay "now"
            [Leaf(stroke "demand" demandLine Theme.text 1.6 |> fade 0.85)
             Leaf(stroke "now" (segment (pt x y0) (pt x y1)) Theme.text 1.5)
             Leaf(dot "now-dot" (pt x (Scale.map ys total)) (4.+2.*pulse 1. state.Now) Theme.text)
             Leaf(rrect "now-tag" (tag-26.) (y0-20.) 52. 18. 9. Theme.text)
             Leaf(label "now-label" tag (y0-7.) 11. Theme.background 700 "middle" (clock state.Hour))]]@inspect)

let private rings state (r:Bounds) =
    let w,h=bw r,bh r
    // The rings and the legend column sit side by side, centred together; the rings size from the space left beside the legend.
    let rowWidth=82.
    let radius=max 40. (min ((w-56.-rowWidth)/2.) ((h-60.)/2.))
    let left=max 12. ((w-(2.*radius+20.+rowWidth))/2.)
    let cx,cy=left+radius,(32.+h-12.)/2.
    let step=radius*0.105
    let thickness=step*0.72
    let rowStep=clamp 28. 44. (2.*radius/7.)
    let rowX,rowY=cx+radius+20.,cy-3.*rowStep-2.
    let focus=hoveredSource state
    let items=
        sources |> List.mapi(fun index s ->
            let outer=radius-float index*step
            let ratio=clamp01(shown s state/capacity[s])
            hot ("src:"+s)
                [Leaf(fill "track" (arcPath (outer-thickness) outer 0. tau 0.) Theme.raised |> Mark.withTransform (Transform.translate cx cy))
                 Leaf(fill "value" (if ratio<0.01 then Path.empty else arcPath (outer-thickness) outer 0. (ratio*tau) (thickness/2.)) colors[s]
                      |> Mark.withTransform (Transform.translate cx cy) |> fade (emphasis focus s))
                 at "row" rowX (rowY+float index*rowStep)
                    [Leaf(rrect "swatch" 0. -9. 10. 10. 2. colors[s])
                     Leaf(label "name" 16. 0. 11.5 Theme.text 600 "start" s |> fade (emphasis focus s))
                     Leaf(label "value" rowWidth 0. 11. Theme.muted 600 "end" (num 1 (shown s state)))
                     Leaf(label "cf" rowWidth 13. 9.5 Theme.faint 400 "end" (percent 0 ratio))]])
    panel "rings" r "Output vs capacity (GW)"
        [group "items" items
         Leaf(label "center" cx (cy+5.) 14. Theme.text 700 "middle" (num 1 (shown "supply" state)))
         Leaf(label "center-sub" cx (cy+20.) 9.5 Theme.muted 400 "middle" "GW total")]

let private flows state (r:Bounds) =
    let w,h=bw r,bh r
    let mix=sources |> List.map(fun s -> s,max 0.05 (shown s state))
    let supply=mix |> List.sumBy snd
    let hour=state.Hour
    let weights=["Residential",0.3+0.08*bump 19. 3. hour;"Commercial",0.24+0.08*bump 13. 4. hour;"Industrial",0.3;"EV charging",0.04+0.05*bump 22. 3. hour]
    let need=shown "demand" state
    let weightTotal=weights |> List.sumBy snd
    let consumers=(weights |> List.map(fun (k,w) -> k,need*w/weightTotal))@["Storage",max 0.05 (supply-need)]
    let links=
        [for s,v in mix -> {FlowLink.Id="in:"+s;Source=s;Target="Grid";Value=v}]
        @[for k,v in consumers -> {FlowLink.Id="out:"+k;Source="Grid";Target=k;Value=max 0.05 v}]
    let graph=FlowGraph.create (sources@["Grid"]@sectors) links |> ok
    let layout=Sankey.layout {SankeyConfig.create (box 16. 40. (w-40.) (h-56.)) with NodeWidth=14.;NodePadding=7.;Iterations=0} graph |> ok
    let focus=hoveredSource state
    let colorOf (id:string) = Map.tryFind id colors |> Option.defaultValue Theme.accent
    let ribbons=
        layout.Links |> List.map(fun link ->
            let source=if link.Source="Grid" then link.Target else link.Source
            let color=if link.Source="Grid" then ramp [Theme.accent;Theme.teal] (float(List.findIndex ((=) link.Target) sectors)/4.) else colorOf link.Source
            let opacity=match focus with Some f -> (if link.Source=f then 0.75 else 0.12) | None -> 0.42
            hot ("src:"+source) [Leaf(stroke "ribbon" (Shape.linkHorizontal link.Start link.Finish) color (max 1. link.Width) |> fade opacity)])
    let nodes=
        layout.Nodes |> List.map(fun node ->
            let b=node.Bounds
            let left=node.Layer=0
            let color=if node.Id="Grid" then Theme.text elif List.contains node.Id sources then colorOf node.Id else Theme.accent
            group node.Id
                [Leaf(rect "bar" (Bounds.x b) (Bounds.y b) (Bounds.width b) (Bounds.height b) color)
                 if node.Id<>"Grid" then
                     Leaf(label "name" (if left then Bounds.x b+20. else Bounds.x b-6.) (Bounds.y b+Bounds.height b/2.+4.) 10.5 Theme.text 600 (if left then "start" else "end")
                            (sprintf "%s %.1f" node.Id node.Value))
                 else Leaf(label "name" (Bounds.x b+7.) (Bounds.y b-6.) 10.5 Theme.text 700 "middle" "Grid")])
    panel "flows" r (sprintf "Energy flow at %s (GW)" (clock state.Hour)) [group "ribbons" ribbons;group "nodes" nodes]

let private status state (r:Bounds) =
    let w,h=bw r,bh r
    let hz=List.last state.Frequency
    let intensity=shown "carbon" state
    let share=shown "renewable" state
    // From the foot up: the price row, the frequency trace (taller in a stretched card) and its title; the gauges share what is left.
    let traceHeight=clamp 38. 110. (h-260.)
    let titleY=h-54.-traceHeight
    let traceTop,traceBottom=titleY+8.,titleY+8.+traceHeight
    let room=titleY-64.
    let radius=clamp 36. 90. (min (w/4.-24.) ((room-18.)/1.78))
    let cy=40.+(room-(radius*1.78+18.))/2.+radius
    // The trace's axis remembers recent extremes and relaxes slowly, so a quiet stretch does not magnify noise into swings.
    let axis=state.FrequencyAxis
    let tx0,tx1=16.,w-62.
    let xs=Scale.linear (0.,float(state.Frequency.Length-1)) (tx0,tx1)
    let ys=Scale.linear (Extent.padded 0.12 axis) (traceBottom,traceTop)
    let points=state.Frequency |> List.mapi(fun index value -> pt (Scale.map xs (float index)) (Scale.map ys value))
    let lowY,highY=Scale.map ys axis.Low,Scale.map ys axis.High
    panel "status" r "Grid status"
        [gauge "carbon" (w/4.) cy radius 11. (intensity/600.) (ramp [Theme.good;Theme.warn;Theme.bad] (intensity/500.)) (num 0 intensity) "g CO₂ / kWh"
         gauge "renewable" (w*0.75) cy radius 11. share Theme.teal (percent 0 share) "renewable share"
         Leaf(label "freq-title" 16. titleY 11. Theme.muted 700 "start" "FREQUENCY")
         Leaf(label "freq" (w-16.) titleY 12. (if abs(hz-50.)>0.03 then Theme.warn else Theme.good) 700 "end" (num 3 hz+" Hz"))
         group "freq-trace"
            [Leaf(stroke "high-line" (segment (pt tx0 highY) (pt tx1 highY)) Theme.grid 1.)
             Leaf(stroke "low-line" (segment (pt tx0 lowY) (pt tx1 lowY)) Theme.grid 1.)
             Leaf(fill "area" (smoothArea (points |> List.map(fun p -> pt p.X traceBottom,p))) Theme.good |> fade 0.16)
             Leaf(stroke "line" (smooth points) Theme.good 1.6)
             Leaf(label "high" (w-16.) (highY+3.5) 9.5 Theme.faint 400 "end" (num 3 axis.High))
             Leaf(label "low" (w-16.) (lowY+3.5) 9.5 Theme.faint 400 "end" (num 3 axis.Low))]
         Leaf(label "price-title" 16. (h-16.) 11. Theme.muted 700 "start" "WHOLESALE PRICE")
         Leaf(label "price" (w-16.) (h-14.) 16. Theme.text 700 "end" ("$"+num 0 (shown "price" state)+" / MWh"))]

let private clockTool state =
    tool 124. (fun x y ->
        [at "clock" x y
            [Leaf(rrect "bg" 0. 0. 124. 26. 13. Theme.raised |> Mark.withStroke (paint Theme.edge) 1.)
             Leaf(label "time" 62. 17.5 12.5 Theme.text 700 "middle" (sprintf "Day %d · %s" state.Day (clock state.Hour)))]])

let view viewport state =
    page viewport "Power grid mix" "A simulated day on a national grid: gas balances demand after renewables, flows re-route every frame, and frequency wobbles with imbalance." "Play/pause (Space) · speed · scenario · ←/→ step an hour · move over the profile to inspect · hover a source to trace it"
        [buttonTool "play" 74. (if state.Playing then "Pause" else "Play") (not state.Playing)
         segmentedTool "speed" 44. ["1","1×";"4","4×";"16","16×"] (string(int state.Speed))
         segmentedTool "scenario" 84. ["Calm","Calm";"Windy","Windy";"Heatwave","Heatwave"] state.Scenario]
        [clockTool state]
        [[item 640. 440. (cell "profile" 330.);item 344. 290. (cell "rings" 330.)]
         [item 640. 440. (cell "flows" 298.);item 344. 290. (cell "status" 298.)]]
        (fun cells -> [profile state cells["profile"];rings state cells["rings"];flows state cells["flows"];status state cells["status"]])

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="energy";Title="Power grid";Summary="Generation mix, live Sankey";Start=fun () -> Live.start model}

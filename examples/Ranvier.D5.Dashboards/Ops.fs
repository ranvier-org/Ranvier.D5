/// <summary>Cluster operations: streaming host CPU, latency percentiles, stacked service throughput and host health.</summary>
module Ranvier.D5.Dashboards.Ops

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

let hosts=[|"api-1";"api-2";"api-3";"worker-1";"worker-2";"db-1"|]
let services=[|"checkout";"search";"catalog";"auth";"media"|]

type Sample = { T:float; Cpu:float[]; Traffic:float[]; P50:float; P90:float; P99:float; Errors:float }
type State =
    { Now:float; Next:float; Samples:Sample list; Window:float; Paused:bool; Spike:float
      Hover:string option; Pointer:Point option; Rng:RandomState; Shown:Map<string,float> }

let private interval=0.5
let private capacity=280
let private hostBase=[|38.;44.;35.;58.;52.;63.|]
let private serviceBase=[|420.;610.;380.;260.;190.|]

/// <summary>Spike envelope in [0,1] from the remaining spike seconds: one second in, two seconds out.</summary>
let spikeLevel remaining = if remaining<=0. then 0. else min (clamp01((8.-remaining)/1.)) (clamp01(remaining/2.))

let private measure (now:float) spike (rng:RandomState) =
    let jitter,rng=uniform rng
    let cpu=hosts |> Array.mapi(fun index _ ->
        let burst=if index<3 then spike*36. elif index=5 then spike*24. else spike*10.
        clamp 2. 99. (hostBase[index]+20.*fbm (11+index) (now/7.)+burst+6.*(jitter-0.5)))
    let traffic=services |> Array.mapi(fun index _ ->
        max 10. (serviceBase[index]*(1.+0.22*sin(now/18.+float index*1.3)+0.12*fbm (31+index) (now/5.))*(1.+0.8*spike)))
    let p50=38.+9.*fbm 51 (now/6.)+spike*46.
    let p90=p50*1.75+6.*abs(fbm 52 (now/4.))
    let p99=p90*1.6+18.*abs(fbm 53 (now/3.))+spike*120.
    let errors=clamp 0. 0.2 (0.004+0.003*fbm 54 (now/5.)+spike*0.035)
    {T=now;Cpu=cpu;Traffic=traffic;P50=p50;P90=p90;P99=p99;Errors=errors},rng

let private trim samples = let count=List.length samples in if count>capacity then List.skip (count-capacity) samples else samples

let private targets state =
    let last=List.last state.Samples
    let rps=Array.sum last.Traffic
    let latency=state.Samples |> List.filter(fun s -> s.T>=state.Now-state.Window) |> List.map(fun s -> s.P99) |> List.fold max 100.
    let throughput=state.Samples |> List.filter(fun s -> s.T>=state.Now-state.Window) |> List.map(fun s -> Array.sum s.Traffic) |> List.fold max 1000.
    Map.ofList ["rps",rps;"errors",last.Errors*100.;"p99",last.P99;"healthy",float(last.Cpu |> Array.filter(fun c -> c<85.) |> Array.length)
                "latencyMax",latency*1.12;"throughputMax",throughput*1.1]

let init () =
    let rec fill t rng acc = if t>130. then List.rev acc,rng else let sample,rng=measure t 0. rng in fill (t+interval) rng (sample::acc)
    let samples,rng=fill 0. (RandomState.create 7UL) []
    let state={Now=130.;Next=130.+interval;Samples=trim samples;Window=60.;Paused=false;Spike=0.;Hover=None;Pointer=None;Rng=rng;Shown=Map.empty}
    {state with Shown=targets state}

let tick dt state =
    let state=
        if state.Paused then state else
        let now=state.Now+dt/1000.
        let spike=max 0. (state.Spike-dt/1000.)
        let rec advance next samples rng =
            if next>now then next,samples,rng
            else let sample,rng=measure next (spikeLevel spike) rng in advance (next+interval) (sample::samples) rng
        let next,fresh,rng=advance state.Next [] state.Rng
        {state with Now=now;Next=next;Spike=spike;Rng=rng;Samples=trim(state.Samples@List.rev fresh)}
    {state with Shown=approachMap dt 380. 0. state.Shown (targets state)}

let input message state =
    match message with
    | Input.Click id when id.StartsWith "window:" -> {state with Window=float(id.Substring 7)}
    | Input.Click "pause" | Input.Key " " | Input.Key "p" -> {state with Paused=not state.Paused}
    | Input.Click "spike" | Input.Key "s" -> {state with Spike=8.}
    | Input.Key "ArrowLeft" -> {state with Window=max 30. (state.Window/2.)}
    | Input.Key "ArrowRight" -> {state with Window=min 120. (state.Window*2.)}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | Input.Pick id -> {state with Hover=Some id}
    | _ -> state

let private lerpSample a b f =
    let mixArray (x:float[]) (y:float[]) = Array.map2 (fun u v -> lerp u v f) x y
    {T=lerp a.T b.T f;Cpu=mixArray a.Cpu b.Cpu;Traffic=mixArray a.Traffic b.Traffic;P50=lerp a.P50 b.P50 f;P90=lerp a.P90 b.P90 f;P99=lerp a.P99 b.P99 f;Errors=lerp a.Errors b.Errors f}

/// <summary>Samples inside the window, with an interpolated sample exactly at the window start so lines enter at the left edge.</summary>
let windowed start (samples:Sample list) =
    let inside=samples |> List.filter(fun s -> s.T>=start)
    match samples |> List.filter(fun s -> s.T<start) |> List.tryLast,inside with
    | Some before,first::_ -> lerpSample before first ((start-before.T)/(first.T-before.T))::inside
    | _ -> inside

let private hostColor index = Theme.series[index%Theme.series.Length]
let private shown key state = Map.tryFind key state.Shown |> Option.defaultValue 0.

let private cpuCard state rows start (r:Bounds) =
    let w,h=bw r,bh r
    // The right margin holds the host labels at the line ends, so they stay inside the card.
    let x0,x1,y0,y1=48.,w-72.,48.,h-28.
    let xs=Scale.linear (start,state.Now) (x0,x1)
    let ys=Scale.linear (0.,100.) (y1,y0)
    let px (s:Sample)=Scale.map xs s.T
    let ends=dodge 11. [for index,host in Array.indexed hosts -> host,Scale.map ys (List.last rows).Cpu[index]]
    let lines=
        hosts |> Array.toList |> List.mapi(fun index host ->
            let points=rows |> List.map(fun s -> pt (px s) (Scale.map ys s.Cpu[index]))
            let opacity=emphasis state.Hover host
            let last=List.last points
            hot host
                [Leaf(stroke "line" (smooth points) (hostColor index) (if state.Hover=Some host then 2.6 else 1.6) |> fade opacity)
                 Leaf(dot "head" last 3.2 (hostColor index) |> fade opacity)
                 Leaf(label "name" (last.X+8.) (ends[host]+4.) 10. (hostColor index) 600 "start" host |> fade opacity)])
    let breaches=
        [for s in rows do
            for index in 0..hosts.Length-1 do
                if s.Cpu[index]>85. then yield pt (px s) (Scale.map ys s.Cpu[index])]
        |> List.mapi(fun index point -> Leaf(dot (sprintf "b%d" index) point 2.4 Theme.bad |> fade 0.85))
    let threshold=Scale.map ys 85.
    let crosshair=
        match state.Pointer with
        | Some p when p.X-bx r>=x0 && p.X-bx r<=x1 && p.Y-by r>=y0 && p.Y-by r<=y1 && not rows.IsEmpty ->
            let local=pt (p.X-bx r) (p.Y-by r)
            let time=Scale.invert xs local.X
            let nearest=rows |> List.minBy(fun s -> abs(s.T-time))
            let x=px nearest
            let ranked=hosts |> Array.mapi(fun index host -> host,nearest.Cpu[index]) |> Array.sortByDescending snd |> Array.toList
            [overlay "crosshair"
                [Leaf(stroke "cursor" (segment (pt x y0) (pt x y1)) Theme.muted 1. |> fade 0.6)
                 group "cursor-dots" [for index in 0..hosts.Length-1 -> Leaf(ring (sprintf "d%d" index) (pt x (Scale.map ys nearest.Cpu[index])) 4. (hostColor index) 2.)]]
             tooltip "tip" w h (pt x local.Y) ((sprintf "%.1fs ago" (state.Now-nearest.T))::(ranked |> List.map(fun (host,cpu) -> sprintf "%-9s %5.1f%%" host cpu)))]
        | _ -> []
    panel "cpu" r "CPU utilisation by host"
        ([yGrid "grid" ys [0.;25.;50.;75.;100.] x0 x1 (fun v -> num 0 v+"%")
          Leaf(stroke "threshold" (segment (pt x0 threshold) (pt x1 threshold)) Theme.bad 1. |> fade 0.55)
          Leaf(label "threshold-label" (x1-4.) (threshold-5.) 10. Theme.bad 600 "end" "alert 85%")
          xLabels "time" (h-10.) [for k in 0..4 -> x0+(x1-x0)*float k/4.,(if k=4 then "now" else sprintf "-%.0fs" (state.Window*(1.-float k/4.)))]
          group "breaches" breaches
          group "lines" lines]@crosshair)

let private latencyCard state rows (r:Bounds) =
    let x0,x1,y0,y1=44.,bw r-18.,48.,bh r-28.
    let start=state.Now-state.Window
    let xs=Scale.linear (start,state.Now) (x0,x1)
    let ys=Scale.linear (0.,max 100. (shown "latencyMax" state)) (y1,y0)
    let at value (s:Sample)=pt (Scale.map xs s.T) (Scale.map ys value)
    let band=rows |> List.map(fun s -> at s.P90 s,at s.P99 s)
    let ticks=Scale.ticks 4 ys
    panel "latency" r "Latency percentiles (ms)"
        [yGrid "grid" ys ticks x0 x1 (num 0)
         Leaf(fill "p90-p99" (smoothArea band) Theme.violet |> fade 0.28)
         Leaf(stroke "p99" (smooth (rows |> List.map(fun s -> at s.P99 s))) Theme.violet 1.4)
         Leaf(stroke "p90" (smooth (rows |> List.map(fun s -> at s.P90 s))) Theme.violet 1. |> fade 0.7)
         Leaf(stroke "p50" (smooth (rows |> List.map(fun s -> at s.P50 s))) Theme.teal 2.)
         legend "legend" 44. (bh r-8.) ["p50",Theme.teal;"p90–p99 band",Theme.violet]]

let private throughputCard state rows (r:Bounds) =
    let x0,x1,y0,y1=56.,bw r-20.,44.,bh r-30.
    let start=state.Now-state.Window
    let xs=Scale.linear (start,state.Now) (x0,x1)
    let ys=Scale.linear (0.,max 1000. (shown "throughputMax" state)) (y1,y0)
    let layers=
        Stack.layout {Order=StackOrder.Input;Offset=StackOffset.Zero}
            [for index,name in Array.indexed services -> {Key=name;Values=rows |> List.map(fun s -> s.Traffic[index])}] |> ok
    let areas=
        layers |> List.mapi(fun index layer ->
            let pairs=List.zip rows layer.Values |> List.map(fun (s,(low,high)) -> pt (Scale.map xs s.T) (Scale.map ys low),pt (Scale.map xs s.T) (Scale.map ys high))
            hot layer.Key [Leaf(fill "area" (smoothArea pairs) Theme.series[(index+2)%Theme.series.Length] |> fade (0.82*emphasis state.Hover layer.Key))])
    panel "throughput" r "Throughput by service (req/s)"
        [yGrid "grid" ys (Scale.ticks 3 ys) x0 x1 (si 0)
         group "areas" areas
         legend "legend" 56. (bh r-10.) [for index,name in Array.indexed services -> name,Theme.series[(index+2)%Theme.series.Length]]]

let private healthCard state (r:Bounds) =
    let last=List.last state.Samples
    // Two columns of tiles that widen with the card; rows share the height below the title.
    let tw=(bw r-42.)/2.
    let step=(bh r-48.)/3.
    let tiles=
        hosts |> Array.toList |> List.mapi(fun index host ->
            let cpu=last.Cpu[index]
            let hotHost=cpu>85.
            let color=if hotHost then Theme.bad elif cpu>70. then Theme.warn else Theme.good
            let x,y=16.+float(index%2)*(tw+10.),40.+float(index/2)*step
            hot host
                [at "tile" x y
                    [Leaf(rrect "bg" 0. 0. tw (step-8.) 6. Theme.raised
                          |> Mark.withStroke (paint (if hotHost then Theme.bad elif state.Hover=Some host then Theme.accent else Theme.edge)) (if hotHost then 1.+2.*pulse 0.8 state.Now else 1.))
                     Leaf(dot "status" (pt 12. 16.) 4. color |> fade (if hotHost then 0.4+0.6*pulse 0.8 state.Now else 1.))
                     Leaf(label "name" 22. 20. 11.5 Theme.text 600 "start" host)
                     Leaf(label "cpu" (tw-10.) 20. 11.5 color 700 "end" (num 0 cpu+"%"))
                     Leaf(rrect "track" 10. (step-24.) (tw-20.) 6. 3. Theme.edge)
                     Leaf(rrect "level" 10. (step-24.) ((tw-20.)*cpu/100.) 6. 3. color)]])
    panel "health" r "Host health" tiles

let view viewport state =
    let start=state.Now-state.Window
    let rows=windowed start state.Samples
    let last=List.last state.Samples
    let earlier=state.Samples |> List.tryFind(fun s -> s.T>=state.Now-30.) |> Option.defaultValue last
    let rpsChange=(Array.sum last.Traffic-Array.sum earlier.Traffic)/Array.sum earlier.Traffic
    let recent=state.Samples |> List.skip (max 0 (state.Samples.Length-60))
    let errors=shown "errors" state
    let live=not state.Paused
    let kpis=["rps";"errors";"p99";"healthy"] |> List.map(fun key -> item 238. 180. (cell key 104.))
    page viewport "Cluster operations" "Six hosts and five services sampled twice a second. The window scrolls continuously and axes rescale as load changes." "Hover a host or a service to isolate it · move over the CPU chart for a crosshair · Space pauses · S injects a spike · ←/→ change the window"
        [segmentedTool "window" 54. ["30","30 s";"60","60 s";"120","120 s"] (string(int state.Window))
         buttonTool "pause" 86. (if state.Paused then "Resume" else "Pause") state.Paused
         buttonTool "spike" 110. (if state.Spike>0. then "Spiking…" else "Inject spike") (state.Spike>0.)]
        [statusTool "status" 104. (if live then "LIVE" else "PAUSED") (if live then Theme.good else Theme.warn) (if live then 0.35+0.65*pulse 1.4 state.Now else 1.)]
        [kpis
         [item 640. 400. (cell "cpu" 300.);item 344. 300. (cell "latency" 300.)]
         [item 640. 400. (cell "throughput" 216.);item 344. 320. (cell "health" 216.)]]
        (fun cells ->
            let tile key = let r=cells[key] in bx r,by r,bw r,bh r
            [(let x,y,w,h=tile "rps" in kpi "rps" x y w h "Requests / s" (grouped (shown "rps" state)) (sprintf "%s vs 30 s ago" (percent 1 rpsChange)) (if rpsChange>=0. then Theme.good else Theme.bad)
                                          (recent |> List.map(fun s -> Array.sum s.Traffic)) Theme.accent)
             (let x,y,w,h=tile "errors" in kpi "errors" x y w h "Error rate" (num 2 errors+"%") (if errors>1. then "above 1% budget" else "within budget") (if errors>1. then Theme.bad else Theme.good)
                                             (recent |> List.map(fun s -> s.Errors)) Theme.bad)
             (let x,y,w,h=tile "p99" in kpi "p99" x y w h "p99 latency" (num 0 (shown "p99" state)+" ms") (sprintf "p50 %s ms" (num 0 last.P50)) Theme.muted
                                          (recent |> List.map(fun s -> s.P99)) Theme.violet)
             (let x,y,w,h=tile "healthy" in kpi "healthy" x y w h "Healthy hosts" (sprintf "%.0f / %d" (Math.Round(shown "healthy" state)) hosts.Length)
                                              (if last.Cpu |> Array.exists(fun c -> c>85.) then "CPU alert active" else "all hosts nominal") (if last.Cpu |> Array.exists(fun c -> c>85.) then Theme.bad else Theme.good)
                                              (recent |> List.map(fun s -> s.Cpu |> Array.average)) Theme.good)
             cpuCard state rows start cells["cpu"]
             latencyCard state rows cells["latency"]
             throughputCard state rows cells["throughput"]
             healthCard state cells["health"]])

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="ops";Title="Cluster operations";Summary="Streaming CPU, latency, traffic";Start=fun () -> Live.start model}

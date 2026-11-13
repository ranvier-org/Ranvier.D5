/// <summary>Emergency department flow: a stochastic patient simulation where every patient is a token moving from arrival through
/// triage, waiting and treatment bays to discharge or admission, with queue history, occupancy gauges, a wait-time histogram and
/// ward bed grids.</summary>
module Ranvier.D5.Dashboards.Hospital

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type Stage =
    | Arriving
    | Triage of finish:float * station:int
    | Waiting
    | Treating of finish:float * bay:int
    | Leaving of started:float * admitted:bool

type Patient = { Id:int; Acuity:int; Arrived:float; Stage:Stage; Position:Point }
type State =
    { Now:float; Clock:float; Rate:float; Staff:int; Paused:bool; Patients:Patient list; NextId:int; NextArrival:float
      Waits:float list; History:(float*float*float) list; NextSample:float; Wards:float[]; Rng:RandomState
      Shown:Map<string,float>; Hover:string option; Pointer:Point option }

let private acuityColor = function 1 -> Theme.bad | 2 -> Theme.orange | 3 -> Theme.warn | 4 -> Theme.good | _ -> Theme.blue
let private acuityName = function 1 -> "Resuscitation" | 2 -> "Emergent" | 3 -> "Urgent" | 4 -> "Less urgent" | _ -> "Non-urgent"
let private admitChance = function 1 -> 0.85 | 2 -> 0.55 | 3 -> 0.3 | 4 -> 0.08 | _ -> 0.02
let private treatMinutes = function 1 -> 110. | 2 -> 75. | 3 -> 48. | 4 -> 28. | _ -> 18.
let wards=["ICU",12;"Medical",24;"Surgical",20;"Observation",16]
let private entrance=pt 24. 210.
let private bays state = state.Staff*2
let private minutesPerSecond=1.6

let private slot stage index =
    match stage with
    | Arriving -> pt 62. (330.-float index*20.)
    | Triage(_,station) -> pt 132. (150.+float station*100.)
    | Waiting -> pt (206.+float(index%5)*26.) (64.+float(index/5)*26.)
    | Treating(_,bay) -> pt (384.+float(bay%4)*44.) (78.+float(bay/4)*64.)
    | Leaving(_,true) -> pt 612. 70.
    | Leaving(_,false) -> pt 612. 350.

/// <summary>Ward discharges in proportion to occupancy, balancing admissions near 70% occupancy at the default arrival rate.</summary>
let private discharge minutes (wards:float[]) = wards |> Array.map(fun occupied -> max 0. (occupied-minutes*occupied*0.0016))

let private arrive state =
    let acuity,rng=uniform state.Rng
    let gap,rng=draw (Distribution.exponential (state.Rate/60.) |> ok) rng
    let acuity=if acuity<0.03 then 1 elif acuity<0.18 then 2 elif acuity<0.55 then 3 elif acuity<0.85 then 4 else 5
    {state with Patients=state.Patients@[{Id=state.NextId;Acuity=acuity;Arrived=state.Clock;Stage=Arriving;Position=entrance}]
                NextId=state.NextId+1;NextArrival=state.Clock+gap;Rng=rng}

/// <summary>Advances the department to the current clock: arrivals, triage, prioritised bay assignment, completions and ward flow.</summary>
let rec private settle state =
    if state.NextArrival<=state.Clock then settle (arrive state) else
    let free stations used = stations |> List.filter(fun s -> not(List.contains s used))
    // Triage completions move to the waiting room.
    let patients=state.Patients |> List.map(fun p -> match p.Stage with Triage(finish,_) when finish<=state.Clock -> {p with Stage=Waiting} | _ -> p)
    // Free triage stations take the earliest arrivals.
    let usedStations=patients |> List.choose(fun p -> match p.Stage with Triage(_,s) -> Some s | _ -> None)
    let rng=state.Rng
    let patients,rng=
        free [0;1] usedStations |> List.fold(fun (patients,rng) station ->
            match patients |> List.tryFind(fun p -> p.Stage=Arriving) with
            | Some next ->
                let minutes,rng=draw (Distribution.uniform 4. 9. |> ok) rng
                patients |> List.map(fun p -> if p.Id=next.Id then {p with Stage=Triage(state.Clock+minutes,station)} else p),rng
            | None -> patients,rng) (patients,rng)
    // Treatment completions leave, some admitted.
    let patients,rng,admitted=
        patients |> List.fold(fun (done',rng,admitted) p ->
            match p.Stage with
            | Treating(finish,_) when finish<=state.Clock ->
                let roll,rng=uniform rng
                let admit=roll<admitChance p.Acuity
                {p with Stage=Leaving(state.Clock,admit)}::done',rng,(if admit then p.Acuity::admitted else admitted)
            | _ -> p::done',rng,admitted) ([],rng,[])
    let patients=List.rev patients
    // Free bays take the most acute, longest-waiting patients.
    let usedBays=patients |> List.choose(fun p -> match p.Stage with Treating(_,b) -> Some b | _ -> None)
    let patients,rng,waits=
        free [0..bays state-1] usedBays |> List.fold(fun (patients,rng,waits) bay ->
            match patients |> List.filter(fun p -> p.Stage=Waiting) |> List.sortBy(fun p -> p.Acuity,p.Arrived) |> List.tryHead with
            | Some next ->
                let minutes,rng=draw (Distribution.logNormal (log (treatMinutes next.Acuity)) 0.35 |> ok) rng
                patients |> List.map(fun p -> if p.Id=next.Id then {p with Stage=Treating(state.Clock+minutes,bay)} else p),rng,(state.Clock-next.Arrived)::waits
            | None -> patients,rng,waits) (patients,rng,[])
    let patients=patients |> List.filter(fun p -> match p.Stage with Leaving(started,_) -> state.Clock-started<5. | _ -> true)
    let wardsNext=
        state.Wards |> Array.mapi(fun i occupied ->
            let capacity=float(snd (List.item i wards))
            let incoming=admitted |> List.filter(fun a -> (if a=1 then 0 elif a=2 then 1 elif a=3 then 2 else 3)=i) |> List.length |> float
            min capacity (occupied+incoming))
    {state with Patients=patients;Rng=rng;Waits=(waits@state.Waits) |> List.truncate 160;Wards=wardsNext}

let private counts state =
    let count f=state.Patients |> List.filter(fun p -> f p.Stage) |> List.length |> float
    count (function Waiting -> true | _ -> false),count (function Treating _ -> true | _ -> false),count (function Arriving | Triage _ -> true | _ -> false)

let private targets state =
    let waiting,treating,arriving=counts state
    let median=match state.Waits |> List.sort with [] -> 0. | sorted -> sorted[sorted.Length/2]
    Map.ofList ["waiting",waiting;"treating",treating;"arriving",arriving;"occupancy",treating/float(bays state);"median",median
                "wards",(state.Wards |> Array.sum)/float(wards |> List.sumBy snd)]

let private place dt state =
    let indexed stage = state.Patients |> List.filter(fun p -> p.Stage=stage) |> List.mapi(fun i p -> p.Id,i) |> Map.ofList
    let arriving,waiting=indexed Arriving,indexed Waiting
    {state with Patients=state.Patients |> List.map(fun p ->
                    let target=
                        match p.Stage with
                        | Arriving -> slot Arriving arriving[p.Id]
                        | Waiting -> slot Waiting waiting[p.Id]
                        | other -> slot other 0
                    {p with Position=pt (approach dt 260. p.Position.X target.X) (approach dt 260. p.Position.Y target.Y)})}

let init () =
    let state={Now=0.;Clock=0.;Rate=14.;Staff=6;Paused=false;Patients=[];NextId=1;NextArrival=0.5;Waits=[];History=[];NextSample=0.
               Wards=[|9.;17.;13.;8.|];Rng=RandomState.create 911UL;Shown=Map.empty;Hover=None;Pointer=None}
    // Warm up three simulated hours so queues, waits and history are populated.
    let rec warm state = if state.Clock>=420. then state else warm (settle {state with Clock=state.Clock+1.;Wards=discharge 1. state.Wards;History=(if int state.Clock%2=0 then push 120 (counts state) state.History else state.History)})
    let state=warm state
    let rec snap n state = if n=0 then state else snap (n-1) (place 1000. state)
    let state=snap 4 {state with NextSample=state.Clock+2.}
    {state with Shown=targets state}

let tick dt state =
    let now=state.Now+dt/1000.
    if state.Paused then {state with Now=now} |> place dt else
    let clock=state.Clock+dt/1000.*minutesPerSecond
    let state=settle {state with Now=now;Clock=clock}
    let state={state with Wards=discharge (dt/1000.*minutesPerSecond) state.Wards}
    let sample=clock>=state.NextSample
    let state=if sample then {state with History=push 120 (counts state) state.History;NextSample=state.NextSample+2.} else state
    {place dt state with Shown=approachMap dt 400. 0. state.Shown (targets state)}

let input message state =
    match message with
    | Input.Click "arrivals:-" | Input.Key "ArrowDown" -> {state with Rate=max 4. (state.Rate-2.)}
    | Input.Click "arrivals:+" | Input.Key "ArrowUp" -> {state with Rate=min 40. (state.Rate+2.)}
    | Input.Click "staff:-" | Input.Key "ArrowLeft" -> {state with Staff=max 3 (state.Staff-1)}
    | Input.Click "staff:+" | Input.Key "ArrowRight" -> {state with Staff=min 8 (state.Staff+1)}
    | Input.Click "surge" | Input.Key "s" ->
        // A mass-casualty surge: eight arrivals at once with high acuity.
        let surge=[for k in 0..7 -> {Id=state.NextId+k;Acuity=(if k<2 then 1 elif k<5 then 2 else 3);Arrived=state.Clock;Stage=Arriving;Position=pt 24. (180.+float k*8.)}]
        {state with Patients=state.Patients@surge;NextId=state.NextId+8}
    | Input.Click "pause" | Input.Key " " -> {state with Paused=not state.Paused}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private shown key state = Map.tryFind key state.Shown |> Option.defaultValue 0.
let private clock (minutes:float) = let m=int(minutes+8.*60.)%(24*60) in sprintf "%02d:%02d" (m/60) (m%60)

let private floorCard state =
    let zone key x w (title:string) =
        group key
            [Leaf(rrect "bg" x 40. w 340. 8. Theme.raised |> fade 0.6)
             Leaf(label "title" (x+w/2.) 58. 10. Theme.muted 700 "middle" (title.ToUpperInvariant()))]
    let bayMarks=
        [for b in 0..15 ->
            let p=slot (Treating(0.,b)) 0
            let open'=b<bays state
            Leaf(rrect (sprintf "bay%d" b) (p.X-17.) (p.Y-22.) 34. 44. 5. (if open' then Theme.edge else Theme.panel) |> fade (if open' then 0.9 else 0.35))]
    let stations=[for s in 0..1 -> let p=slot (Triage(0.,s)) 0 in Leaf(rrect (sprintf "station%d" s) (p.X-20.) (p.Y-20.) 40. 40. 8. Theme.edge)]
    let tokens=
        state.Patients |> List.map(fun p ->
            let fresh=match p.Stage with Leaving(started,_) -> 1.-(state.Clock-started)/5. | _ -> 1.
            let progress=match p.Stage with Treating(finish,_) -> Some(clamp01(1.-(finish-state.Clock)/treatMinutes p.Acuity)) | _ -> None
            hot (sprintf "pt:%d" p.Id)
                ([Leaf(dot "token" p.Position 7. (acuityColor p.Acuity) |> Mark.withStroke (paint Theme.background) 1.5 |> fade fresh)]
                 @ (match progress with
                    | Some f -> [Leaf(fill "progress" (arcPath 9. 11. 0. (max 0.05 f*tau) 0.) (acuityColor p.Acuity) |> Mark.withTransform (Transform.translate p.Position.X p.Position.Y) |> fade 0.8)]
                    | None -> [])
                 @ (if p.Acuity=1 && fresh=1. then [Leaf(ring "alarm" p.Position (9.+6.*pulse 0.7 state.Now) Theme.bad 1.5 |> fade 0.6)] else [])))
    let tip=
        match state.Hover with
        | Some id when id.StartsWith "pt:" ->
            state.Patients |> List.tryFind(fun p -> sprintf "pt:%d" p.Id=id) |> Option.map(fun p ->
                let stage=match p.Stage with Arriving -> "Awaiting triage" | Triage _ -> "In triage" | Waiting -> "Waiting for a bay" | Treating(_,b) -> sprintf "Bay %d" (b+1) | Leaving(_,a) -> (if a then "Admitted" else "Discharged")
                tooltip "tip" p.Position [sprintf "Patient %d · ESI %d" p.Id p.Acuity;acuityName p.Acuity;stage;sprintf "In department %.0f min" (state.Clock-p.Arrived)]) |> Option.toList
        | _ -> []
    card "floor" 24. 124. 640. 400. (sprintf "Department floor · %s" (clock state.Clock))
        ([zone "z-arrive" 40. 50. "In"
          zone "z-triage" 98. 68. "Triage"
          zone "z-wait" 184. 152. "Waiting"
          zone "z-treat" 354. 186. "Treatment"
          zone "z-exit" 556. 72. "Out"
          Leaf(label "admit" 612. 98. 10. Theme.violet 600 "middle" "admit ↑")
          Leaf(label "discharge" 612. 330. 10. Theme.good 600 "middle" "home ↓")
          group "bays" bayMarks
          group "stations" stations
          group "tokens" tokens
          legend "legend" 18. 392. [for a in 1..5 -> sprintf "ESI %d" a,acuityColor a]]@tip)

let private history state =
    let x0,x1,y0,y1=48.,620.,44.,196.
    let rows=state.History
    let maximum=rows |> List.map(fun (w,t,a) -> w+t+a) |> List.fold max 10.
    let xs=Scale.linear (0.,119.) (x0,x1)
    let ys=Scale.linear (0.,maximum*1.15) (y1,y0)
    let offset=float(120-rows.Length)
    let layers=Stack.layout {Order=StackOrder.Input;Offset=StackOffset.Zero} [{Key="treating";Values=rows |> List.map(fun (_,t,_) -> t)};{Key="waiting";Values=rows |> List.map(fun (w,_,_) -> w)};{Key="arriving";Values=rows |> List.map(fun (_,_,a) -> a)}] |> ok
    let colors=Map.ofList ["treating",Theme.accent;"waiting",Theme.warn;"arriving",Theme.violet]
    let areas=layers |> List.map(fun layer ->
        Leaf(fill layer.Key (smoothArea (layer.Values |> List.mapi(fun i (lo,hi) -> pt (Scale.map xs (float i+offset)) (Scale.map ys lo),pt (Scale.map xs (float i+offset)) (Scale.map ys hi)))) colors[layer.Key] |> fade 0.7))
    let capacity=Scale.map ys (float(bays state))
    card "history" 24. 540. 640. 236. "Census · last four hours"
        [yGrid "grid" ys (Scale.ticks 4 ys) x0 x1 (num 0)
         group "areas" areas
         Leaf(stroke "capacity" (segment (pt x0 capacity) (pt x1 capacity)) Theme.text 1.2 |> fade 0.6)
         Leaf(label "capacity-label" (x1-4.) (capacity-5.) 10. Theme.text 600 "end" (sprintf "%d bays" (bays state)))
         legend "legend" 330. 24. ["Treating",Theme.accent;"Waiting",Theme.warn;"Arriving/triage",Theme.violet]]

let private occupancy state =
    let ed=shown "occupancy" state
    let wardsShare=shown "wards" state
    card "occupancy" 680. 124. 344. 196. "Occupancy"
        [gauge "ed" 92. 104. 60. 11. ed (ramp [Theme.good;Theme.warn;Theme.bad] ed) (percent 0 ed) "ED bays"
         gauge "wards" 252. 104. 60. 11. wardsShare (ramp [Theme.good;Theme.warn;Theme.bad] wardsShare) (percent 0 wardsShare) "inpatient beds"
         Leaf(label "waiting" 92. 186. 11.5 Theme.text 600 "middle" (sprintf "%.0f waiting" (shown "waiting" state)))
         Leaf(label "rate" 252. 186. 11.5 Theme.text 600 "middle" (sprintf "%.0f arrivals / h" state.Rate))]

let private waits state =
    let x0,x1,y0,y1=40.,320.,48.,160.
    let edges=[for k in 0..12 -> float k*20.]
    let bins=Data.bin id edges (state.Waits |> List.map(fun w -> clamp 0. 239.9 w))
    let maximum=bins |> List.map(fun b -> float b.Values.Length) |> List.fold max 4.
    let xs=Scale.linear (0.,240.) (x0,x1)
    let ys=Scale.linear (0.,maximum*1.15) (y1,y0)
    let median=shown "median" state
    let bars=
        bins |> List.mapi(fun i b ->
            let x=Scale.map xs b.Lower
            let w=Scale.map xs b.Upper-x-2.
            let top=Scale.map ys (float b.Values.Length)
            Leaf(rrect (sprintf "b%d" i) x top w (y1-top) 2. (if b.Lower>=120. then Theme.bad elif b.Lower>=60. then Theme.warn else Theme.teal) |> fade 0.85))
    let m=Scale.map xs (min 240. median)
    card "waits" 680. 336. 344. 214. "Door-to-bay wait (min)"
        [group "bars" bars
         Leaf(stroke "median" (segment (pt m y0) (pt m y1)) Theme.text 1.5)
         Leaf(label "median-label" (m+4.) (y0+8.) 10.5 Theme.text 700 "start" (sprintf "median %.0f" median))
         xLabels "axis" (y1+16.) [for t in [0.;60.;120.;180.;240.] -> Scale.map xs t,num 0 t]
         Leaf(text "count" 16. 200. 10.5 Theme.faint (sprintf "last %d patients seen" state.Waits.Length))]

let private wardsCard state =
    let focus=state.Hover |> Option.filter(fun h -> h.StartsWith "ward:") |> Option.map(fun h -> h.Substring 5)
    let blocks=
        wards |> List.mapi(fun i (name,capacity) ->
            let occupied=state.Wards[i]
            let x=16.+float(i%2)*162.
            let y=40.+float(i/2)*86.
            hot ("ward:"+name)
                [at "ward" x y
                    ([Leaf(label "name" 0. 10. 11. Theme.text 700 "start" name |> fade (emphasis focus name))
                      Leaf(label "beds" 150. 10. 10.5 Theme.muted 600 "end" (sprintf "%.0f/%d" occupied capacity))]
                     @ [for bed in 0..capacity-1 ->
                            let filled=float bed<occupied
                            Leaf(rrect (sprintf "bed%d" bed) (float(bed%12)*12.6) (18.+float(bed/12)*12.6) 10. 10. 2. (if filled then (if occupied/float capacity>0.9 then Theme.bad else Theme.violet) else Theme.raised)
                                 |> fade (emphasis focus name))])])
    card "wards" 680. 566. 344. 210. "Inpatient wards" blocks

let view state =
    [header "Emergency department" "A stochastic simulation: patients arrive, are triaged, wait by acuity for a bay, are treated and leave home or for a ward. Staffing and arrivals change the queue live." "Arrivals −/+ (↓/↑) · staff −/+ (←/→) · mass-casualty surge (S) · pause (Space) · hover patients and wards"
     group "controls"
        [button "arrivals:-" 24. 84. 30. "−" false
         at "rate" 58. 84. [Leaf(label "value" 46. 18. 12. Theme.text 700 "middle" (sprintf "%.0f / h" state.Rate))]
         button "arrivals:+" 150. 84. 30. "+" false
         button "staff:-" 200. 84. 30. "−" false
         at "staff" 234. 84. [Leaf(label "value" 46. 18. 12. Theme.text 700 "middle" (sprintf "%d staff" state.Staff))]
         button "staff:+" 326. 84. 30. "+" false
         button "surge" 376. 84. 120. "Surge (8 arrivals)" false
         button "pause" 502. 84. 74. (if state.Paused then "Resume" else "Pause") state.Paused]
     floorCard state
     history state
     occupancy state
     waits state
     wardsCard state]

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="hospital";Title="Emergency department";Summary="Patient flow simulation";Start=fun () -> Live.start model}

/// <summary>Market share race: a bar-chart race of twelve fictional companies over 25 years with sliding ranks and a rescaling axis,
/// a progressively drawn share history and a year-in-review panel with a concentration gauge.</summary>
module Ranvier.D5.Dashboards.Race

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type State =
    { Now:float; Year:float; Playing:bool; Speed:float; Ranks:Map<string,float>; Scale:float; Hover:string option; Pointer:Point option }

let companies=["Altair Systems";"Borealis";"Cobalt Labs";"Driftwood";"Ember Grid";"Fathom";"Galena";"Halcyon";"Ion Works";"Juniper";"Kestrel";"Lumen"]
let firstYear,lastYear=2000.,2025.
let private colorOf name = Theme.series[List.findIndex ((=) name) companies%Theme.series.Length]

/// <summary>Revenue in $B: logistic adoption with its own midpoint and ceiling, a decline after some peaks, and deterministic shocks.</summary>
let revenue name (year:float) =
    let i=List.findIndex ((=) name) companies
    let ceiling=[|86.;64.;120.;40.;95.;58.;72.;105.;48.;66.;80.;140.|][i]
    let midpoint=[|2004.;2002.;2012.;2001.;2015.;2008.;2006.;2018.;2003.;2010.;2014.;2020.|][i]
    let rate=[|0.45;0.6;0.38;0.7;0.42;0.5;0.48;0.55;0.65;0.4;0.5;0.62|][i]
    let decline=[|2016.;2009.;3000.;2007.;3000.;2017.;2013.;3000.;2011.;3000.;3000.;3000.|][i]
    let grown=ceiling/(1.+exp(-rate*(year-midpoint)))
    let faded=grown*exp(-0.12*max 0. (year-decline))
    max 0.2 (faded*(1.+0.06*noise (i*19+3) (year*0.9)))

let private standings (year:float) =
    companies |> List.map(fun c -> c,revenue c year) |> List.sortByDescending snd

let private rankTargets year = standings year |> List.mapi(fun rank (name,_) -> name,float rank) |> Map.ofList

let init () =
    let year=2003.
    {Now=0.;Year=year;Playing=true;Speed=1.;Ranks=rankTargets year;Scale=standings year |> List.head |> snd;Hover=None;Pointer=None}

let tick dt state =
    // Playback holds on the final year for a moment, then loops.
    let year=if state.Playing then (let next=state.Year+dt/1000./1.6*state.Speed in if next>lastYear+2. then firstYear else next) else state.Year
    let shown=min lastYear year
    let leader=standings shown |> List.head |> snd
    {state with Now=state.Now+dt/1000.;Year=year;Ranks=approachMap dt 260. 12. state.Ranks (rankTargets shown)
                Scale=approach dt 300. state.Scale leader}

let input message state =
    match message with
    | Input.Click "play" | Input.Key " " -> {state with Playing=not state.Playing}
    | Input.Click "restart" | Input.Key "r" -> {state with Year=firstYear;Playing=true}
    | Input.Click id when id.StartsWith "speed:" -> {state with Speed=float(id.Substring 6)}
    | Input.Key "ArrowRight" -> {state with Playing=false;Year=min lastYear (floor state.Year+1.)}
    | Input.Key "ArrowLeft" -> {state with Playing=false;Year=max firstYear (ceil state.Year-1.)}
    | Input.Pick id when id.StartsWith "year:" -> {state with Year=float(id.Substring 5);Playing=false}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private hoveredCompany state = state.Hover |> Option.filter(fun h -> h.StartsWith "co:") |> Option.map(fun h -> h.Substring 3)

let private race state =
    let x0,x1,top,slot=150.,560.,52.,46.
    let xs=Scale.linear (0.,state.Scale*1.08) (x0,x1)
    let focus=hoveredCompany state
    let bars=
        companies |> List.choose(fun name ->
            let rank=Map.tryFind name state.Ranks |> Option.defaultValue 12.
            if rank>10.6 then None else
            let value=revenue name (min lastYear state.Year)
            let y=top+rank*slot
            let opacity=clamp01(10.6-rank)*emphasis focus name
            let w=Scale.map xs value-x0
            Some(hot ("co:"+name)
                    [Leaf(rrect "bar" x0 y w 36. 5. (colorOf name) |> fade (0.88*opacity))
                     Leaf(label "name" (x0-10.) (y+23.) 12. Theme.text 600 "end" name |> fade opacity)
                     Leaf(label "value" (x0+w+8.) (y+23.) 12. Theme.text 700 "start" ("$"+num 1 value+"B") |> fade opacity)
                     Leaf(label "initial" (x0+w-10.) (y+23.) 12. Theme.background 800 "end" (name.Substring(0,1)) |> fade (if w>30. then opacity else 0.))]))
    let ticks=Scale.ticks 5 xs |> List.filter(fun t -> t>0.)
    let grid=
        [for t in ticks ->
            let x=Scale.map xs t
            group (sprintf "t%.3f" t)
                [Leaf(stroke "line" (segment (pt x 44.) (pt x 520.)) Theme.grid 1.)
                 Leaf(label "label" x 38. 10. Theme.faint 400 "middle" ("$"+num 0 t+"B"))]]
    let timeline=
        [for y in 2000..5..2025 ->
            let x=40.+(float y-firstYear)/(lastYear-firstYear)*540.
            hot (sprintf "year:%d" y)
                [Leaf(dot "hit" (pt x 600.) 12. Theme.panel |> fade 0.01)
                 Leaf(dot "tick" (pt x 600.) 3.5 (if (min lastYear state.Year)>=float y then Theme.accent else Theme.edge))
                 Leaf(label "label" x 624. 10. Theme.muted 400 "middle" (string y))]]
    let progress=40.+((min lastYear state.Year)-firstYear)/(lastYear-firstYear)*540.
    card "race" 24. 124. 640. 652. "Revenue by company ($B)"
        ([group "grid" grid
          group "bars" bars
          Leaf(label "year" 560. 500. 64. Theme.text 800 "end" (string(int (min lastYear state.Year))) |> fade 0.9)
          Leaf(label "sub" 560. 522. 12. Theme.muted 400 "end" (sprintf "Total market $%sB" (num 0 (companies |> List.sumBy(fun c -> revenue c (min lastYear state.Year))))))
          Leaf(rrect "track" 40. 597. 540. 6. 3. Theme.raised)
          Leaf(rrect "progress" 40. 597. (progress-40.) 6. 3. Theme.accent)
          Leaf(dot "playhead" (pt progress 600.) (6.+1.5*pulse 1. state.Now) Theme.text)]
         @ timeline)

let private history state =
    let x0,x1,y0,y1=40.,320.,44.,262.
    let xs=Scale.linear (firstYear,lastYear) (x0,x1)
    let leader=standings (min lastYear state.Year) |> List.head |> fst
    let focus=hoveredCompany state |> Option.defaultValue leader
    let share company year = revenue company year/(companies |> List.sumBy(fun c -> revenue c year))
    let ys=Scale.linear (0.,0.45) (y1,y0)
    let years=[for k in 0..int(((min lastYear state.Year)-firstYear)*4.) -> firstYear+float k/4.]@[(min lastYear state.Year)]
    let lines=
        companies |> List.map(fun c ->
            let points=years |> List.map(fun y -> pt (Scale.map xs y) (Scale.map ys (share c y)))
            Leaf(stroke c (smooth points) (if c=focus then colorOf c else Theme.edge) (if c=focus then 2.4 else 1.) |> fade (if c=focus then 1. else 0.7)))
    let head=pt (Scale.map xs (min lastYear state.Year)) (Scale.map ys (share focus (min lastYear state.Year)))
    card "history" 680. 124. 344. 300. (sprintf "Market share · %s" focus)
        [yGrid "grid" ys [0.;0.15;0.3;0.45] x0 x1 (fun v -> percent 0 v)
         group "lines" lines
         Leaf(dot "head" head 4.5 (colorOf focus))
         Leaf(label "share" (head.X-6.) (head.Y-10.) 12. (colorOf focus) 700 "end" (percent 1 (share focus (min lastYear state.Year))))
         xLabels "years" (y1+16.) [for y in [2000.;2010.;2020.] -> Scale.map xs y,string(int y)]]

let private review state =
    let now=standings (min lastYear state.Year)
    let before=standings (max firstYear ((min lastYear state.Year)-1.))
    let growth=now |> List.map(fun (c,v) -> c,v/(before |> List.find(fun (b,_) -> b=c) |> snd)-1.) |> List.sortByDescending snd
    let total=now |> List.sumBy snd
    let hhi=now |> List.sumBy(fun (_,v) -> (v/total)*(v/total))
    let leader,leaderValue=now.Head
    let gainer,gain=growth.Head
    let loser,loss=List.last growth
    let row index (title:string) (value:string) color =
        at (sprintf "row%d" index) 16. (52.+float index*42.)
            [Leaf(label "title" 0. 0. 10.5 Theme.muted 700 "start" (title.ToUpperInvariant()))
             Leaf(label "value" 0. 20. 15. color 700 "start" value)]
    card "review" 680. 440. 344. 336. (sprintf "Year in review · %d" (int (min lastYear state.Year)))
        [row 0 "Leader" (sprintf "%s · %s" leader (percent 1 (leaderValue/total))) (colorOf leader)
         row 1 "Fastest growth" (sprintf "%s %s" gainer (signed 1 (gain*100.)+"%")) Theme.good
         row 2 "Steepest decline" (sprintf "%s %s" loser (signed 1 (loss*100.)+"%")) Theme.bad
         row 3 "Top-3 share" (percent 1 ((now |> List.truncate 3 |> List.sumBy snd)/total)) Theme.text
         gauge "hhi" 272. 112. 46. 9. (hhi/0.25) (ramp [Theme.good;Theme.warn;Theme.bad] (hhi/0.25)) (num 0 (hhi*10000.)) "HHI"
         Leaf(text "hhi-note" 226. 196. 10.5 Theme.faint (if hhi>0.15 then "Concentrated" elif hhi>0.1 then "Moderate" else "Competitive"))]

let view state =
    [header "Market share race" "Twelve fictional companies, twenty-five years. Bars slide between ranks, the axis rescales to the leader and history draws as time advances." "Play/pause (Space) · restart (R) · speed · ←/→ step a year · click a year on the timeline · hover a company to follow it"
     group "controls"
        ([button "play" 24. 84. 74. (if state.Playing then "Pause" else "Play") (not state.Playing)
          button "restart" 104. 84. 74. "Restart" false]
         @ segmented "speed" 190. 84. 44. ["1","1×";"2","2×";"4","4×"] (string(int state.Speed)))
     race state
     history state
     review state]

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="race";Title="Market share race";Summary="Bar race, share history";Start=fun () -> Live.start model}

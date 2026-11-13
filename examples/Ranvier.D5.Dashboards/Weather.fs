/// <summary>Climate station: a radial year of daily temperature ranges revealed by a sweeping hand, a monthly wind rose that tweens
/// between months, precipitation and humidity, and live current conditions with a gusting wind compass.</summary>
module Ranvier.D5.Dashboards.Weather

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type City = { Id:string; Name:string; Mean:float; Swing:float; Range:float; Wind:float; Prevailing:float; Rain:float list; Seed:int }
type State = { Now:float; Day:float; Playing:bool; City:string; Fahrenheit:bool; Rose:Map<string,float>; Hover:string option; Pointer:Point option }

let cities=
    [ {Id="harbour";Name="Harbour";Mean=12.;Swing=7.5;Range=7.;Wind=7.5;Prevailing=240.;Seed=3
       Rain=[96.;72.;70.;58.;55.;52.;48.;61.;74.;98.;110.;104.]}
      {Id="mesa";Name="Mesa";Mean=22.;Swing=11.;Range=15.;Wind=4.5;Prevailing=200.;Seed=7
       Rain=[18.;16.;14.;6.;3.;2.;24.;30.;14.;9.;11.;20.]}
      {Id="summit";Name="Summit";Mean=3.;Swing=10.;Range=9.;Wind=9.;Prevailing=300.;Seed=11
       Rain=[140.;128.;122.;110.;118.;132.;150.;146.;118.;104.;126.;142.]} ]
let private city id = cities |> List.find(fun c -> c.Id=id)
let private months=[|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|]
let private monthStarts=[|0;31;59;90;120;151;181;212;243;273;304;334;365|]
let monthOf (day:float) = (monthStarts |> Array.findIndexBack(fun start -> float start<=day%365.)) |> min 11
let private directions=[|"N";"NNE";"NE";"ENE";"E";"ESE";"SE";"SSE";"S";"SSW";"SW";"WSW";"W";"WNW";"NW";"NNW"|]
let private speedBins=[|"0–3";"3–6";"6–9";"9+"|]
let private speedColors=[|Theme.teal;Theme.accent;Theme.violet;Theme.pink|]

/// <summary>Daily minimum and maximum in °C for a city and day of year.</summary>
let temperature (c:City) (day:float) =
    let seasonal=c.Mean+c.Swing*cos(tau*(day-200.)/365.)
    let weather=2.6*fbm c.Seed (day/5.)
    let range=c.Range*(0.75+0.35*(0.5+0.5*fbm (c.Seed+5) (day/9.)))
    seasonal+weather-range/2.,seasonal+weather+range/2.

/// <summary>Share of hours per direction and speed bin for a month; the prevailing wind swings seasonally.</summary>
let rose (c:City) month =
    [for d in 0..15 do
        let angle=float d*22.5
        let prevailing=c.Prevailing+25.*sin(tau*float month/12.)
        let delta=abs(((angle-prevailing+540.)%360.)-180.)
        let weight=0.25+exp(-(delta/45.)*(delta/45.))*1.6+0.2*(0.5+0.5*noise (c.Seed*31+d) (float month*0.7))
        for b in 0..3 do
            let speedShare=[|0.38;0.32;0.2;0.1|][b]*(if b>=2 then c.Wind/7. else 1.)*(1.+0.3*sin(tau*float month/12.+float b))
            yield sprintf "%s|%d" directions[d] b,weight*speedShare]
    |> fun rows -> let total=rows |> List.sumBy snd in rows |> List.map(fun (k,v) -> k,v/total) |> Map.ofList

let init () =
    let c=city "harbour"
    {Now=0.;Day=96.;Playing=true;City=c.Id;Fahrenheit=false;Rose=rose c (monthOf 96.);Hover=None;Pointer=None}

let tick dt state =
    let day=if state.Playing then (state.Day+dt/1000.*9.)%365. else state.Day
    let target=rose (city state.City) (monthOf day)
    {state with Now=state.Now+dt/1000.;Day=day;Rose=approachMap dt 380. 0. state.Rose target}

let input message state =
    match message with
    | Input.Click id when id.StartsWith "city:" -> {state with City=id.Substring 5}
    | Input.Click "units" | Input.Key "u" -> {state with Fahrenheit=not state.Fahrenheit}
    | Input.Click "play" | Input.Key " " -> {state with Playing=not state.Playing}
    | Input.Key "ArrowRight" -> {state with Day=(state.Day+7.)%365.}
    | Input.Key "ArrowLeft" -> {state with Day=(state.Day+358.)%365.}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private unit state (celsius:float) = if state.Fahrenheit then celsius*1.8+32. else celsius
let private degrees state celsius = sprintf "%.0f°%s" (unit state celsius) (if state.Fahrenheit then "F" else "C")
let private dateOf (day:float) = let m=monthOf day in sprintf "%s %d" months[m] (int(day%365.)-monthStarts[m]+1)
let private warmth=[ (59,130,246);(56,189,248);(45,212,191);(250,204,21);(251,146,60);(239,68,68) ]

let private radial state (r:Bounds) =
    let c=city state.City
    let w,h=bw r,bh r
    // The ring is centred below the title and scaled from the shorter side; k is the scale against the 520x652 design. Month
    // names keep a fixed gap outside the ticks, so the hand tip never runs into them on a small ring.
    let k=max 0.6 (min ((w/2.-28.)/246.) (((h-44.)/2.-30.)/246.))
    let cx,cy=w/2.,44.+(h-44.)/2.
    let labelRadius=246.*k+16.
    let inner,outer=78.*k,236.*k
    let low,high=c.Mean-c.Swing-c.Range-6.,c.Mean+c.Swing+c.Range+6.
    let rs=Scale.linear (low,high) (inner,outer)
    let today=state.Day
    let local=state.Pointer |> Option.map(fun p -> pt (p.X-bx r) (p.Y-by r))
    let hoverDay=
        match local with
        | Some p ->
            let dx,dy=p.X-cx,p.Y-cy
            let distance=sqrt(dx*dx+dy*dy)
            if distance>70.*k && distance<245.*k then Some(((atan2 dx (-dy)+tau)%tau)/tau*365.) else None
        | None -> None
    let bars=
        [for d in 0..364 ->
            let lo,hi=temperature c (float d)
            let mean=(lo+hi)/2.
            let start,finish=float d/365.*tau,float(d+1)/365.*tau
            let revealed=float d<=today
            let distance=today-float d
            let fresh=if revealed && distance<12. then 1.-distance/12. else 0.
            let hovered=hoverDay |> Option.exists(fun h -> int h=d)
            Leaf(fill (sprintf "d%d" d) (Shape.arc (Scale.map rs lo) (Scale.map rs hi+(if hovered then 6. else fresh*4.)) start finish) (ramp warmth ((mean-low)/(high-low)))
                 |> fade (if hovered then 1. elif revealed then 0.55+0.45*fresh else 0.1))]
    let rings=
        [for i,t in List.indexed (Scale.ticks 5 rs) do
            let radius=Scale.map rs t
            if radius>=inner && radius<=outer then
                yield Leaf(stroke (sprintf "ring%d" i) (circlePath radius) Theme.grid 1. |> fade 0.8)
                yield Leaf(label (sprintf "ringl%d" i) 4. (-radius-3.) 9.5 Theme.faint 400 "start" (degrees state t))]
    let monthMarks=
        [for m in 0..11 ->
            let angle=float monthStarts[m]/365.*tau
            let mid=(float monthStarts[m]+float(monthStarts[m+1]-monthStarts[m])/2.)/365.*tau
            let p=Curves.radialPoint mid labelRadius
            group months[m]
                [Leaf(stroke "tick" (segment (Curves.radialPoint angle (70.*k)) (Curves.radialPoint angle (246.*k))) Theme.grid 1.)
                 Leaf(label "name" p.X (p.Y+4.) 11. (if monthOf today=m then Theme.text else Theme.muted) (if monthOf today=m then 700 else 400) "middle" months[m])]]
    let hand=Curves.radialPoint (today/365.*tau) (240.*k)
    let lo,hi=temperature c today
    let tip=
        match hoverDay,local with
        | Some d,Some p ->
            let lo,hi=temperature c d
            [tooltip "tip" w h p [dateOf d;sprintf "High %s" (degrees state hi);sprintf "Low %s" (degrees state lo);(if d<=today then "Observed" else "Last year")]]
        | _ -> []
    panel "year" r (sprintf "%s · daily temperature range" c.Name)
        ([at "chart" cx cy
            [group "rings" rings
             group "months" monthMarks
             group "bars" bars
             Leaf(stroke "hand" (segment (pt 0. 0.) hand) Theme.text 1.5 |> fade 0.8)
             Leaf(dot "hand-tip" hand (4.+1.5*pulse 1. state.Now) Theme.text)
             // The readout keeps fixed type sizes, so the hub never shrinks below what its three lines need.
             Leaf(dot "hub" (pt 0. 0.) (max 54. (64.*k)) Theme.panel |> Mark.withStroke (paint Theme.edge) 1.)
             Leaf(label "date" 0. -14. 13. Theme.muted 600 "middle" (dateOf today))
             Leaf(label "high" 0. 12. 22. Theme.text 700 "middle" (degrees state hi))
             Leaf(label "low" 0. 32. 12. Theme.muted 400 "middle" ("low "+degrees state lo))]]@tip)

let private windRose state (r:Bounds) =
    let c=city state.City
    let w,h=bw r,bh r
    // The rose (sized from the shorter side, s being the scale against the 464x336 design) and the speed key beside it are
    // centred together, so a wide card does not open a gap between them.
    let top,bottom=36.,h-14.
    let outer=max 43. (min ((w-16.-112.-42.)/2.) ((bottom-top)/2.))
    let s=outer/143.
    let left=max 12. ((w-(2.*outer+30.+112.))/2.)
    let cx,cy=left+outer,(top+bottom)/2.
    let keyX=cx+outer+30.
    let centred path = Mark.withTransform (Transform.translate cx cy) path
    let maximum=directions |> Array.map(fun d -> [0..3] |> List.sumBy(fun b -> Map.tryFind (sprintf "%s|%d" d b) state.Rose |> Option.defaultValue 0.)) |> Array.max
    let rs=Scale.sqrt (0.,maximum) (14.*s,128.*s)
    let focus=state.Hover |> Option.filter(fun h -> h.StartsWith "dir:") |> Option.map(fun h -> h.Substring 4)
    let petals=
        directions |> Array.toList |> List.mapi(fun d name ->
            let start,finish=(float d-0.5)*tau/16.,(float d+0.5)*tau/16.
            let cumulative=[0..3] |> List.scan(fun total b -> total+(Map.tryFind (sprintf "%s|%d" name b) state.Rose |> Option.defaultValue 0.)) 0.
            hot ("dir:"+name)
                [for b in 0..3 ->
                    Leaf(fill (sprintf "b%d" b) (Shapes.arc {InnerRadius=Scale.map rs cumulative[b];OuterRadius=Scale.map rs cumulative[b+1];StartAngle=start;EndAngle=finish;PadAngle=0.04;CornerRadius=1.5}) speedColors[b]
                         |> centred |> fade (emphasis focus name))])
    let compass=
        [for name,angle in ["N",0.;"E",90.;"S",180.;"W",270.] ->
            let p=Curves.radialPoint (angle/180.*Math.PI) (142.*s)
            Leaf(label ("c"+name) (cx+p.X) (cy+p.Y+4.) 11. Theme.muted 700 "middle" name)]
    let tip=
        match focus with
        | Some name ->
            let share=[0..3] |> List.sumBy(fun b -> Map.tryFind (sprintf "%s|%d" name b) state.Rose |> Option.defaultValue 0.)
            let strong=Map.tryFind (sprintf "%s|3" name) state.Rose |> Option.defaultValue 0.
            // Follow the pointer while it is over the card; otherwise (keyboard or a stale hover) point at the petal itself.
            let petal=let d=Array.findIndex ((=) name) directions in Curves.radialPoint (float d*tau/16.) (Scale.map rs share)
            let anchor=
                match state.Pointer with
                | Some p when p.X>=bx r && p.X<=bx r+w && p.Y>=by r && p.Y<=by r+h -> pt (p.X-bx r) (p.Y-by r)
                | _ -> pt (cx+petal.X) (cy+petal.Y)
            [tooltip "tip" w h anchor [sprintf "From %s" name;sprintf "%s of hours" (percent 1 share);sprintf "%s above 9 m/s" (percent 1 strong)]]
        | None -> []
    panel "rose" r (sprintf "Wind rose · %s" months[monthOf state.Day])
        ([Leaf(stroke "r1" (circlePath (50.*s)) Theme.grid 1. |> centred)
          Leaf(stroke "r2" (circlePath (90.*s)) Theme.grid 1. |> centred)
          Leaf(stroke "r3" (circlePath (130.*s)) Theme.grid 1. |> centred)
          group "petals" petals
          group "compass" compass
          at "key" keyX (max 52. (60.+(h-336.)/2.))
            ([Leaf(label "key-title" 0. 0. 11. Theme.muted 700 "start" "SPEED (M/S)")]
             @ [for b in 0..3 -> at (sprintf "k%d" b) 0. (14.+float b*24.) [Leaf(rrect "swatch" 0. 0. 14. 14. 3. speedColors[b]);Leaf(text "name" 22. 11. 11.5 Theme.text speedBins[b])]]
             @ [Leaf(text "prevailing" 0. 124. 11. Theme.muted (sprintf "Prevailing ≈ %.0f°" (c.Prevailing+25.*sin(tau*float(monthOf state.Day)/12.))))])]@tip)

let private precipitation state (r:Bounds) =
    let c=city state.City
    let w,h=bw r,bh r
    // The legend shares the title row when there is room beside the title, otherwise it takes a row above the plot.
    let legendInTitle=w>=400.
    let x0,x1,y0,y1=48.,w-24.,(if legendInTitle then 40. else 58.),h-30.
    let xs=Scale.band (Array.toList months) (x0,x1) 0.3 0.1
    let ys=Scale.linear (0.,160.) (y1,y0)
    let current=monthOf state.Day
    let bars=
        months |> Array.toList |> List.mapi(fun m name ->
            let x=Scale.bandPosition xs name |> Option.get
            let w=Scale.bandwidth xs
            let top=Scale.map ys c.Rain[m]
            let drops=
                if m=current then
                    [for k in 0..5 ->
                        let f=(state.Now*0.9+float k/6.)%1.
                        Leaf(stroke (sprintf "drop%d" k) (segment (pt (x+w*(0.2+0.6*float((k*3)%5)/4.)) (top-34.+f*30.)) (pt (x+w*(0.2+0.6*float((k*3)%5)/4.)) (top-28.+f*30.))) Theme.accent 1.5 |> fade (1.-f))]
                else []
            group name
                ([Leaf(rrect "bar" x top w (y1-top) 3. (if m=current then Theme.accent else Theme.blue) |> fade (if m=current then 1. else 0.45))
                  Leaf(label "month" (x+w/2.) (y1+14.) 9.5 (if m=current then Theme.text else Theme.faint) 600 "middle" (name.Substring(0,1)))]@drops))
    let humidity=[for m in 0..11 -> let x=(Scale.bandPosition xs months[m] |> Option.get)+Scale.bandwidth xs/2. in pt x (Scale.map ys (40.+c.Rain[m]*0.6))]
    panel "rain" r "Precipitation (mm) and humidity"
        [yGrid "grid" ys [0.;80.;160.] x0 x1 (num 0)
         group "bars" bars
         Leaf(stroke "humidity" (smooth humidity) Theme.warn 1.6 |> fade 0.85)
         (if legendInTitle then legend "legend" (w-164.) 24. ["Rain",Theme.blue;"Humidity",Theme.warn]
          else legend "legend" x0 46. ["Rain",Theme.blue;"Humidity",Theme.warn])]

let private conditions state (r:Bounds) =
    let c=city state.City
    let w,h=bw r,bh r
    let lo,hi=temperature c state.Day
    let hour=(state.Now*2.)%24.
    let current=lerp lo hi (0.5+0.5*sin(tau*(hour-9.)/24.))
    let gust=c.Wind*(1.+0.35*fbm (c.Seed+90) (state.Now*0.8))
    let direction=c.Prevailing+25.*sin(tau*float(monthOf state.Day)/12.)+18.*fbm (c.Seed+91) (state.Now*0.5)
    // Rows centre vertically in a stretched card. The compass sits between the clock and the wind reading, shrinking (or
    // dropping out) as the card narrows; both widths are estimated from the widest text so it never jiggles.
    let mid=(h-104.)/2.
    let textEnd=120.+textWidth 11.5 "Local 00:00 · Sep 30"
    let windLeft=w-18.-textWidth 16. "00.0 m/s"*1.06
    let compassX=if w>=464. then w-144. else (textEnd+windLeft)/2.
    let compassR=min 32. ((windLeft-textEnd)/2.-10.)
    let arrow=polygon [pt 0. -30.;pt 7. 6.;pt 0. 0.;pt -7. 6.]
    let compass=
        if compassR<14. then [] else
        [Leaf(ring "compass" (pt compassX (58.+mid)) compassR Theme.edge 2.)
         Leaf(fill "arrow" arrow Theme.accent
              |> Mark.withTransform (Transform.scale (compassR/32.) (compassR/32.) |> Transform.thenApply (Transform.rotate ((direction+180.)/180.*Math.PI)) |> Transform.thenApply (Transform.translate compassX (58.+mid))))]
    panel "now" r "Current conditions"
        ([Leaf(label "temp" 16. (76.+mid) 34. Theme.text 700 "start" (degrees state current))
          Leaf(text "feels" 120. (58.+mid) 11.5 Theme.muted (sprintf "Feels like %s" (degrees state (current-gust*0.35))))
          Leaf(text "clock" 120. (78.+mid) 11.5 Theme.muted (sprintf "Local %02d:%02d · %s" (int hour) (int((hour%1.)*60.)) (dateOf state.Day)))]
         @ compass
         @ [Leaf(label "wind" (w-18.) (52.+mid) 16. Theme.text 700 "end" (sprintf "%.1f m/s" gust))
            Leaf(label "wind-dir" (w-18.) (72.+mid) 11. Theme.muted 400 "end" (sprintf "from %s" directions[int(Math.Round(((direction%360.)+360.)%360./22.5))%16]))])

let view viewport state =
    page viewport "Climate station" "A year of daily highs and lows swept by a clock hand, with the month's wind rose tweening as the season turns." "Choose a station · °C/°F (U) · play/pause the year (Space) · ←/→ step a week · hover the year ring and the rose petals"
        [segmentedTool "city" 86. [for c in cities -> c.Id,c.Name] state.City
         buttonTool "units" 64. (if state.Fahrenheit then "°F" else "°C") state.Fahrenheit
         buttonTool "play" 90. (if state.Playing then "Pause year" else "Play year") (not state.Playing)]
        []
        [[item 520. 430. (cell "year" 652.); item 464. 360. (column [cell "rose" 336.;cell "rain" 180.;cell "now" 104.])]]
        (fun cells -> [radial state cells["year"];windRose state cells["rose"];precipitation state cells["rain"];conditions state cells["now"]])

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="weather";Title="Climate station";Summary="Radial year, wind rose";Start=fun () -> Live.start model}

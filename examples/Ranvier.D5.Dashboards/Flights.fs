/// <summary>Global flight tracker: an orthographic globe with a moving day/night terminator, great-circle routes and planes in flight,
/// a departures board and on-time performance.</summary>
module Ranvier.D5.Dashboards.Flights

open System
open Ranvier.D5
open Ranvier.D5.Geo
open Ranvier.D5.Dashboards.Kit

type Hub = { Code:string; City:string; Region:string; Position:GeoPosition }
type Flight = { Code:string; Route:int; From:Hub; To:Hub; Departs:float; Duration:float; Delayed:bool }
/// <summary>Tracker state. <c>Held</c> is the time until which auto-rotation waits after a hand drag. <c>Radius</c> is the globe
/// radius the last view laid out: input carries no viewport, so a drag reads it to turn pixels into degrees.</summary>
type State =
    { Now:float; Lambda:float; Phi:float; TargetLambda:float; TargetPhi:float; Region:string; Auto:bool; Night:bool; Held:float
      Radius:float ref; Hover:string option; Pointer:Point option; Airborne:float list; NextSample:float; Shown:Map<string,float> }

let private geo longitude latitude = GeoPosition.create longitude latitude |> ok
let hubs=
    [ "JFK","New York","Americas",-73.8,40.6; "SFO","San Francisco","Americas",-122.4,37.6; "MEX","Mexico City","Americas",-99.1,19.4
      "GRU","São Paulo","Americas",-46.5,-23.4; "LHR","London","EMEA",-0.45,51.47; "FRA","Frankfurt","EMEA",8.6,50.0
      "IST","Istanbul","EMEA",28.8,41.3; "JNB","Johannesburg","EMEA",28.2,-26.1; "DXB","Dubai","APAC",55.4,25.25
      "DEL","Delhi","APAC",77.1,28.6; "SIN","Singapore","APAC",104.0,1.36; "HND","Tokyo","APAC",139.8,35.55; "SYD","Sydney","APAC",151.2,-33.9 ]
    |> List.map(fun (code,city,region,longitude,latitude) -> {Code=code;City=city;Region=region;Position=geo longitude latitude})
let private hub code = hubs |> List.find(fun h -> h.Code=code)
let routes=
    ["JFK","LHR";"JFK","FRA";"SFO","HND";"SFO","JFK";"LHR","DXB";"LHR","SIN";"FRA","DEL";"DXB","SIN";"DXB","JNB";"SIN","SYD"
     "HND","SYD";"GRU","JFK";"GRU","LHR";"MEX","SFO";"MEX","JFK";"IST","DXB";"IST","FRA";"DEL","SIN";"HND","SIN";"JNB","LHR";"JFK","IST";"MEX","GRU"]
    |> List.map(fun (a,b) -> hub a,hub b) |> List.toArray
let private regionCenters=Map.ofList ["All",(20.,25.);"Americas",(-80.,15.);"EMEA",(18.,25.);"APAC",(118.,10.)]
let private airlines=[|"RV";"DX";"AN";"SK";"QL"|]
/// <summary>Seconds a hand drag holds auto-rotation before it resumes.</summary>
let private holdSeconds=2.
/// <summary>Latitude limit of the camera: the globe never tips further than this toward a pole.</summary>
let private tilt=70.

/// <summary>Flights whose scheduled window overlaps [start, finish]; even legs fly outbound, odd legs return.</summary>
let flightsBetween start finish =
    [for index,(a,b) in Array.indexed routes do
        let period=7.+float((index*7)%9)
        let offset=float((index*23)%70)/10.
        let duration=9.+Geodesic.distance a.Position b.Position/Math.PI*26.
        let first=int(floor((start-duration-offset-2.)/period))
        let last=int(floor((finish-offset)/period))
        for leg in first..last do
            let delayed=(index*31+leg*17)%6=0
            let departs=offset+float leg*period+(if delayed then 2.5 else 0.)
            let from,destination=if leg%2=0 then a,b else b,a
            if departs+duration>=start && departs<=finish then
                yield {Code=sprintf "%s%d" airlines[(index+leg)%airlines.Length] (100+(index*37+abs leg*11)%900);Route=index;From=from;To=destination
                       Departs=departs;Duration=duration;Delayed=delayed}]

let airborne now = flightsBetween now now |> List.filter(fun f -> now>=f.Departs && now<f.Departs+f.Duration)

let private continents=
    [ [-168.,66.;-162.,70.;-140.,70.;-120.,74.;-95.,72.;-80.,73.;-62.,60.;-55.,52.;-66.,44.;-76.,35.;-81.,25.;-97.,26.;-97.,20.;-87.,21.;-83.,10.;-78.,8.;-90.,14.;-105.,20.;-112.,28.;-117.,32.;-124.,40.;-124.,48.;-135.,58.;-152.,58.;-165.,54.]
      [-78.,8.;-60.,10.;-50.,0.;-35.,-6.;-39.,-14.;-48.,-26.;-58.,-38.;-65.,-41.;-68.,-52.;-74.,-52.;-72.,-40.;-71.,-18.;-77.,-12.;-81.,-5.]
      [-17.,21.;-6.,35.;10.,37.;32.,31.;43.,12.;51.,12.;40.,-5.;40.,-15.;35.,-24.;20.,-35.;17.,-29.;12.,-17.;13.,-5.;9.,4.;-8.,4.;-17.,15.]
      [-10.,36.;-9.,43.;-2.,48.;-5.,54.;5.,60.;10.,63.;25.,71.;45.,68.;70.,73.;100.,77.;140.,73.;170.,69.;179.,66.;160.,60.;142.,54.;135.,43.;122.,40.;121.,31.;110.,20.;106.,10.;100.,13.;98.,8.;92.,22.;80.,15.;77.,8.;72.,21.;66.,25.;57.,25.;56.,27.;48.,30.;35.,36.;28.,37.;26.,40.;23.,36.;12.,44.;16.,38.;8.,44.;3.,43.]
      [114.,-22.;122.,-17.;131.,-11.;137.,-12.;142.,-11.;146.,-19.;153.,-26.;150.,-37.;141.,-38.;131.,-31.;116.,-35.]
      [-55.,60.;-44.,60.;-20.,70.;-20.,80.;-60.,82.;-72.,77.]
      [-6.,50.;2.,51.;0.,58.;-6.,58.]
      [130.,31.;141.,36.;142.,43.;140.,40.;135.,34.] ]
/// <summary>Spherical rings need consistent winding: a ring enclosing more than a hemisphere is reversed to its small region.</summary>
let private polygonOf (ring:GeoPosition list) =
    let closed=ring@[ring.Head]
    let polygon=GeoGeometry.Polygon [closed]
    match GeoMeasure.area GeoStreamConfig.defaultValue polygon with
    | Ok area when area>2.*Math.PI -> GeoGeometry.Polygon [List.rev closed]
    | _ -> polygon
let land=GeoGeometry.MultiPolygon [for ring in continents -> match polygonOf (ring |> List.map(fun (x,y) -> geo x y)) with GeoGeometry.Polygon rings -> rings | _ -> []]
let private graticule=GeoGeometry.MultiLineString (Graticule.create GraticuleConfig.defaultValue |> ok).Lines

/// <summary>A small circle of angular radius <c>distance</c> degrees around a centre.</summary>
let circle (longitude:float) (latitude:float) (distance:float) =
    let rad=Math.PI/180.
    let lat1,lon1,d=latitude*rad,longitude*rad,distance*rad
    [for i in 0..96 ->
        let bearing=float i/96.*tau
        let lat2=asin(sin lat1*cos d+cos lat1*sin d*cos bearing)
        let lon2=lon1+atan2 (sin bearing*sin d*cos lat1) (cos d-sin lat1*sin lat2)
        let lon=((lon2/rad+540.)%360.)-180.
        geo lon (lat2/rad)]
    |> polygonOf

/// <summary>The sub-solar longitude sweeps the globe every two minutes; latitude follows a slow seasonal swing.</summary>
let sun now = (180.-now*3.) % 360. - 180.,18.*sin(now/90.)

/// <summary>An orthographic projection of the given radius centred at (cx, cy), rotated by (lambda, phi) degrees.</summary>
let projection radius cx cy lambda phi =
    Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with
                           Scale=radius;Translate=pt cx cy;Rotation=GeoRotation.create lambda phi 0. |> ok} |> ok
let private draw projection geometry = GeoPath.project GeoPathOptions.defaultValue projection geometry |> ok |> Path.concat

let init () =
    let now=240.
    {Now=now;Lambda=40.;Phi= -20.;TargetLambda=40.;TargetPhi= -20.;Region="All";Auto=true;Night=true;Held=0.;Radius=ref 280.
     Hover=None;Pointer=None;Airborne=[for k in 0..59 -> float(List.length(airborne (now-60.+float k)))];NextSample=now+1.;Shown=Map.empty}

let private ontime now (code:string) = 0.78+0.16*(0.5+0.5*fbm (stableHash code%500) (now/25.))
/// <summary>Whether auto-rotation is on but waiting out the hold after a hand drag.</summary>
let private held state = state.Auto && state.Now<state.Held

let tick dt state =
    let now=state.Now+dt/1000.
    let target=if state.Auto && now>=state.Held then state.TargetLambda-dt/1000.*5. else state.TargetLambda
    let sample=now>=state.NextSample
    let targets=Map.ofList ([for h in hubs -> h.Code,ontime now h.Code]@["airborne",float(List.length(airborne now))])
    {state with Now=now;TargetLambda=target;Lambda=approach dt 650. state.Lambda target;Phi=approach dt 650. state.Phi state.TargetPhi
                Airborne=(if sample then push 60 (float(List.length(airborne now))) state.Airborne else state.Airborne)
                NextSample=(if sample then state.NextSample+1. else state.NextSample);Shown=approachMap dt 500. 0. state.Shown targets}

let private focus region state =
    let longitude,latitude=regionCenters[region]
    // Keep the rotation continuous: choose the equivalent target nearest the current angle.
    let target= -longitude
    let turns=Math.Round((state.Lambda-target)/360.)
    {state with Region=region;TargetLambda=target+turns*360.;TargetPhi= -latitude;Auto=(region="All");Held=0.}

/// <summary>Turns the globe by hand: a pixel of pointer travel is one radius-length of arc, so the surface under the pointer
/// follows it. The camera jumps straight to the new angle (no easing lag) and auto-rotation holds for a moment.</summary>
let private turn dx dy state =
    let degrees=180./(Math.PI*max 40. state.Radius.Value)
    let lambda=state.Lambda+dx*degrees
    let phi=clamp -tilt tilt (state.Phi-dy*degrees)
    {state with Lambda=lambda;TargetLambda=lambda;Phi=phi;TargetPhi=phi;Held=state.Now+holdSeconds}

let input message state =
    match message with
    | Input.Click id when id.StartsWith "region:" -> focus (id.Substring 7) state
    | Input.Click "auto" | Input.Key " " -> {state with Auto=not state.Auto;TargetLambda=state.Lambda;Held=0.}
    | Input.Click "night" | Input.Key "n" -> {state with Night=not state.Night}
    | Input.Drag(dx,dy) -> turn dx dy state
    | Input.Key "ArrowLeft" -> {state with Auto=false;TargetLambda=state.TargetLambda+25.}
    | Input.Key "ArrowRight" -> {state with Auto=false;TargetLambda=state.TargetLambda-25.}
    | Input.Key "ArrowUp" -> {state with TargetPhi=clamp -tilt tilt (state.TargetPhi-15.)}
    | Input.Key "ArrowDown" -> {state with TargetPhi=clamp -tilt tilt (state.TargetPhi+15.)}
    | Input.Pick id when id.StartsWith "hub:" -> focus (hub (id.Substring 4)).Region {state with Auto=false}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private inRegion state (h:Hub) = state.Region="All" || h.Region=state.Region
let private hoveredHub state = state.Hover |> Option.filter(fun id -> id.StartsWith "hub:") |> Option.map(fun id -> id.Substring 4)
let private planeShape=polygon [pt 7. 0.;pt -5. -4.5;pt -2.5 0.;pt -5. 4.5]

let private globe state (r:Bounds) =
    let w,h=bw r,bh r
    // The globe is centred between the title and the legend and sized from the shorter side; the side margin leaves room for a
    // hub code drawn at the limb.
    let radius=max 60. (min (w/2.-40.) ((h-76.)/2.))
    let cx,cy=w/2.,(h+12.)/2.
    state.Radius.Value<-radius
    let projection=projection radius cx cy state.Lambda state.Phi
    let project position = Projection.project projection position
    let focusHub=hoveredHub state
    let routeOpacity (a:Hub,b:Hub) =
        match focusHub with
        | Some code -> if a.Code=code || b.Code=code then 0.95 else 0.08
        | None -> if inRegion state a || inRegion state b then 0.38 else 0.08
    let longitude,latitude=sun state.Now
    let night=circle (longitude+180.) (-latitude) 89.5
    let flights=airborne state.Now
    let planes=
        flights |> List.choose(fun f ->
            let progress=(state.Now-f.Departs)/f.Duration
            let at p = Geodesic.interpolate (clamp01 p) f.From.Position f.To.Position |> ok |> project
            match at progress,at (progress+0.01) with
            | Some point,ahead ->
                let heading=match ahead with Some next -> atan2 (next.Y-point.Y) (next.X-point.X) | None -> 0.
                let trail=[for k in 0..7 -> at (progress-float k*0.012)] |> List.choose id
                let relevant=inRegion state f.From || inRegion state f.To
                let highlighted=match focusHub with Some code -> f.From.Code=code || f.To.Code=code | None -> relevant
                let hovered=state.Hover=Some("fl:"+f.Code)
                let color=if f.Delayed then Theme.warn else Theme.text
                Some(hot ("fl:"+f.Code)
                        [Leaf(stroke "trail" (polyline trail) Theme.accent 1.6 |> fade (if highlighted then 0.55 else 0.12))
                         Leaf(dot "hit" point 7. Theme.background |> fade 0.01)
                         Leaf(fill "plane" planeShape (if hovered then Theme.accent else color)
                              |> Mark.withTransform (Transform.rotate heading |> Transform.thenApply (Transform.scale (if hovered then 1.5 else 1.) (if hovered then 1.5 else 1.)) |> Transform.thenApply (Transform.translate point.X point.Y))
                              |> fade (if highlighted then 1. else 0.3))])
            | None,_ -> None)
    let departures now (h:Hub) = flightsBetween (now-1.6) now |> List.filter(fun f -> f.From.Code=h.Code && now>=f.Departs && now-f.Departs<1.6)
    let hubMarks=
        hubs |> List.choose(fun h ->
            project h.Position |> Option.map(fun point ->
                let size=3.5+float(routes |> Array.filter(fun (a,b) -> a.Code=h.Code || b.Code=h.Code) |> Array.length)*0.9
                let active=inRegion state h
                let pulses=departures state.Now h |> List.mapi(fun i f ->
                    let age=(state.Now-f.Departs)/1.6
                    Leaf(ring (sprintf "pulse%d" i) point (size+18.*outCubic age) Theme.accent 2. |> fade (0.9*(1.-age))))
                let over=focusHub=Some h.Code
                hot ("hub:"+h.Code)
                    (pulses@
                     [Leaf(dot "halo" point (size+4.) Theme.accent |> fade (if over then 0.45 else 0.15))
                      Leaf(dot "dot" point size (if active then Theme.accent else Theme.faint) |> Mark.withStroke (paint Theme.background) 1.5)
                      Leaf(label "code" (point.X+size+5.) (point.Y+4.) 11. (if active then Theme.text else Theme.faint) 700 "start" h.Code)])))
    let tip=
        match state.Hover with
        | Some id when id.StartsWith "fl:" ->
            flights |> List.tryFind(fun f -> "fl:"+f.Code=id) |> Option.bind(fun f ->
                let progress=(state.Now-f.Departs)/f.Duration
                Geodesic.interpolate (clamp01 progress) f.From.Position f.To.Position |> ok |> project |> Option.map(fun point ->
                    tooltip "tip" w h point
                        [sprintf "%s  %s → %s" f.Code f.From.Code f.To.Code
                         sprintf "%s to %s" f.From.City f.To.City
                         sprintf "%.0f%% complete · FL%.0f" (progress*100.) (Math.Round(350.*sin(Math.PI*clamp01 progress)/10.)*10.)
                         (if f.Delayed then "Departed 25 min late" else "On schedule")])) |> Option.toList
        | Some id when id.StartsWith "hub:" ->
            let target=hub (id.Substring 4)
            project target.Position |> Option.map(fun point ->
                let out=flights |> List.filter(fun f -> f.From.Code=target.Code) |> List.length
                let incoming=flights |> List.filter(fun f -> f.To.Code=target.Code) |> List.length
                tooltip "tip" w h point [sprintf "%s · %s" target.Code target.City;sprintf "%d outbound, %d inbound airborne" out incoming;sprintf "On-time %s" (percent 0 (ontime state.Now target.Code));"Click to focus region"]) |> Option.toList
        | _ -> []
    // The sea disc is a plain shape outside every data item, so a press on it starts a drag that turns the globe.
    panel "globe" r "Live network"
        ([Leaf(dot "glow" (pt cx cy) (radius+10.) Theme.accent |> fade 0.06)
          Leaf(fill "ocean" (draw projection GeoGeometry.Sphere) (14,32,58))
          Leaf(stroke "graticule" (draw projection graticule) (40,70,105) 0.7 |> fade 0.7)
          Leaf(fill "land" (draw projection land) (36,66,92) |> Mark.withStroke (paint (60,96,128)) 0.8)]
         @ (if state.Night then [Leaf(fill "night" (draw projection night) (2,6,16) |> fade 0.5)] else [])
         @ [Leaf(stroke "rim" (draw projection GeoGeometry.Sphere) Theme.accent 1.2 |> fade 0.35)
            group "routes" [for index,route in Array.indexed routes ->
                              let a,b=route
                              Leaf(stroke (sprintf "r%d" index) (draw projection (GeoGeometry.LineString [a.Position;b.Position])) Theme.accent 1.1 |> fade (routeOpacity route))]
            group "hubs" hubMarks
            group "planes" planes
            legend "legend" 18. (h-16.) ["Hub",Theme.accent;"Airborne",Theme.text;"Delayed",Theme.warn]]
         @ tip)

/// <summary>Headline count beside its trend: the number and its caption form a block on the left, vertically centred on the
/// sparkline that fills the rest of the card.</summary>
let private airborneCard state (r:Bounds) =
    let w,h=bw r,bh r
    let count=sprintf "%.0f" (Map.tryFind "airborne" state.Shown |> Option.defaultValue 0.)
    let perHour=routes |> Array.mapi(fun index _ -> 3600./(7.+float((index*7)%9))) |> Array.sum
    let caption=sprintf "%s legs scheduled per hour" (grouped perHour)
    let captionSize=11.
    let lines=if w>=440. then [caption] else wrap 112. captionSize caption
    let blockW=max (textWidth 40. "000"*1.06) (lines |> List.map(textWidth captionSize) |> List.max)
    let y0,y1=40.,h-16.
    let blockH=29.+19.+14.*float(lines.Length-1)+3.
    let top=y0+max 0. ((y1-y0-blockH)/2.)
    let baseline=top+29.
    let sparkX=16.+blockW+20.
    panel "airborne" r "Aircraft airborne"
        ([Leaf(label "count" 16. baseline 40. Theme.text 700 "start" count)]
         @ (lines |> List.mapi(fun index line -> Leaf(label (sprintf "sub%d" index) 16. (baseline+19.+14.*float index) captionSize Theme.muted 400 "start" line)))
         @ [spark "trend" state.Airborne sparkX y0 (w-16.-sparkX) (y1-y0) Theme.accent])

let private board state (r:Bounds) =
    let w,h=bw r,bh r
    let capacity=clamp 1. 16. (floor((h-52.)/27.)) |> int
    let upcoming=flightsBetween state.Now (state.Now+30.) |> List.filter(fun f -> f.Departs>state.Now-1.5 && inRegion state f.From) |> List.sortBy(fun f -> f.Departs) |> List.truncate capacity
    // The status pill hugs the right edge and the countdown sits just before it; a wide board adds the city pair between.
    let pillX=w-16.-88.
    let etaX=pillX-44.
    let rows=
        upcoming |> List.mapi(fun index f ->
            let wait=f.Departs-state.Now
            let status,color=
                if wait<0. then "Departed",Theme.good
                elif wait<3. then "Boarding",Theme.accent
                elif f.Delayed then "Delayed",Theme.warn
                else "On time",Theme.muted
            let cities=sprintf "%s → %s" f.From.City f.To.City
            at (sprintf "row-%s-%.0f" f.Code f.Departs) 0. (44.+float index*27.)
                ([Leaf(label "code" 16. 14. 12. Theme.text 700 "start" f.Code)
                  Leaf(label "route" 76. 14. 12. Theme.muted 400 "start" (sprintf "%s → %s" f.From.Code f.To.Code))]
                 @ (if 152.+textWidth 11. cities<=etaX-12. then [Leaf(label "cities" 152. 14. 11. Theme.faint 400 "start" cities)] else [])
                 @ [Leaf(label "eta" etaX 14. 11. Theme.faint 400 "start" (if wait<0. then "now" else sprintf "in %.0fs" wait))
                    Leaf(rrect "pill" pillX 1. 88. 18. 9. color |> fade (if status="Boarding" then 0.25+0.35*pulse 0.7 state.Now else 0.2))
                    Leaf(label "status" (pillX+44.) 14. 10.5 color 700 "middle" status)]))
    panel "board" r (sprintf "Departures · %s" (if state.Region="All" then "all hubs" else state.Region)) rows

let private performance state (r:Bounds) =
    let w,h=bw r,bh r
    let shown code=Map.tryFind code state.Shown |> Option.defaultValue 0.
    let capacity=clamp 1. 13. (floor((h-42.)/25.)) |> int
    let ranked=hubs |> List.filter(inRegion state) |> List.sortByDescending(fun h -> ontime state.Now h.Code) |> List.truncate capacity
    let track=w-124.
    let bars=
        ranked |> List.mapi(fun index h ->
            let value=shown h.Code
            let color=if value>0.9 then Theme.good elif value>0.84 then Theme.accent else Theme.warn
            hot ("hub:"+h.Code)
                [at "bar" 16. (44.+float index*25.)
                    [Leaf(label "code" 0. 13. 11.5 Theme.text 700 "start" h.Code)
                     Leaf(rrect "track" 42. 3. track 12. 6. Theme.raised)
                     Leaf(rrect "value" 42. 3. (track*clamp01((value-0.6)/0.4)) 12. 6. color |> fade (emphasis (hoveredHub state) h.Code))
                     Leaf(label "pct" (w-32.) 13. 11. Theme.muted 600 "end" (percent 1 value))]])
    panel "ontime" r "On-time departures" bars

let view viewport state =
    let status,color,opacity=
        if held state then "HELD",Theme.warn,1.
        elif state.Auto then "ROTATING",Theme.good,0.35+0.65*pulse 1.4 state.Now
        else "FIXED VIEW",Theme.muted,1.
    page viewport "Global flight tracker" "Twenty-two great-circle routes on an orthographic globe; planes fly scheduled legs while the day/night terminator sweeps." "Choose a region to fly there · drag the globe to turn it · hover planes and hubs · click a hub to focus · arrows rotate · Space toggles auto-rotate · N toggles night"
        [segmentedTool "region" 84. ["All","All hubs";"Americas","Americas";"EMEA","EMEA";"APAC","Asia-Pacific"] state.Region
         buttonTool "auto" 104. "Auto-rotate" state.Auto
         buttonTool "night" 92. "Day/night" state.Night]
        [statusTool "camera" 112. status color opacity]
        [[item 640. 420. (cell "globe" 652.); item 344. 290. (column [cell "airborne" 128.;cell "board" 300.;cell "ontime" 192.])]]
        (fun cells -> [globe state cells["globe"];airborneCard state cells["airborne"];board state cells["board"];performance state cells["ontime"]])

let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="flights";Title="Flight tracker";Summary="Globe, routes, departures";Start=fun () -> Live.start model}

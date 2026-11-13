/// <summary>Security operations: a force-directed service topology with packets flowing along every edge, a simulated credential-stuffing
/// attack that can be blocked at the edge, a threat gauge, streaming event bars, top sources and an alert feed.</summary>
module Ranvier.D5.Dashboards.Security

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type Alert = { Id:int; At:float; Severity:string; Text:string }
type State =
    { Now:float; Force:ForceState; Attack:float option; Blocked:bool; Labels:bool; Selected:string option
      Hover:string option; Pointer:Point option; Events:(float*float*float) list; NextSecond:float
      Alerts:Alert list; AlertId:int; NextAlert:float; Rng:RandomState; Shown:Map<string,float>
      MeshX:Extent; MeshY:Extent; EventAxis:Extent }

let private tiers=
    ["Internet","edge";"CDN","edge";"WAF","edge";"LB","edge";"API gateway","edge"
     "auth","service";"users","service";"orders","service";"payments","service";"search","service";"catalog","service"
     "inventory","service";"notify","service";"media","service";"analytics","service"
     "postgres","data";"redis","data";"kafka","data";"object store","data";"elastic","data"
     "VPN","internal";"bastion","internal";"admin","internal"]
let private tierOf id = tiers |> List.find(fun (n,_) -> n=id) |> snd
let private links=
    ["Internet","CDN";"Internet","WAF";"CDN","media";"WAF","LB";"LB","API gateway";"API gateway","auth";"API gateway","users";"API gateway","orders"
     "API gateway","search";"API gateway","catalog";"orders","payments";"orders","inventory";"orders","kafka";"payments","postgres";"auth","redis"
     "auth","postgres";"users","postgres";"search","elastic";"catalog","elastic";"catalog","redis";"inventory","postgres";"kafka","notify";"kafka","analytics"
     "media","object store";"analytics","object store";"VPN","bastion";"bastion","admin";"admin","postgres";"admin","kafka";"Internet","VPN"]
let private attackPath=["Internet";"WAF";"LB";"API gateway";"auth"]
let private onAttackPath (a,b) = List.pairwise attackPath |> List.contains (a,b)
let private tierColor = function "edge" -> Theme.accent | "service" -> Theme.violet | "data" -> Theme.teal | _ -> Theme.warn
let private radiusOf id = match tierOf id with "edge" -> (if id="Internet" then 15. else 11.) | "data" -> 10. | "internal" -> 8. | _ -> 8.5
let private cx,cy=320.,236.
let private sources=["203.0.113.24";"198.51.100.7";"203.0.113.88";"192.0.2.140";"198.51.100.61";"203.0.113.5"]

let private graph=
    GraphData.create [for id,_ in tiers -> {GraphNode.create id with Radius=radiusOf id+6.}]
                     [for a,b in links -> {GraphLink.create (a+">"+b) a b with Distance=(if tierOf a="edge" then 62. else 52.);Strength=0.35}] |> ok

let private initialForce () =
    let config={ForceConfig.create (pt cx cy) with InitialRadius=150.;CenterStrength=0.05;MaxSteps=100000;AlphaMin=0.0005;X=Some(cx,0.05);Y=Some(cy,0.09)
                                                   ManyBody=Some(ManyBodyConfig.create -120.);Collision=Some(CollisionConfig.create 0.8)}
    Force.initialize config (Seed.create 7u) graph |> ok |> Force.step 260

/// <summary>The simulation-space extent of the node positions on each axis.</summary>
let private meshRange (force:ForceState) =
    let nodes=(Force.snapshot force).Nodes
    let xs,ys=nodes |> List.map(fun n -> n.Position.X),nodes |> List.map(fun n -> n.Position.Y)
    (List.min xs,List.max xs),(List.min ys,List.max ys)
/// <summary>Half-lives in milliseconds for the mesh fit to settle after the layout contracts, and for the events axis to relax.</summary>
let private meshHalfLife,eventHalfLife=3000.,4000.
let private eventPeak (events:(float*float*float) list) = events |> List.map(fun (a,b,c) -> a+b+c) |> List.fold max 1200.

let private attackLevel state = match state.Attack with Some start -> clamp01((state.Now-start)/4.) | None -> 0.

let private targets state =
    let attack=attackLevel state
    let threat=0.14+0.06*(0.5+0.5*fbm 3 (state.Now/8.))+attack*(if state.Blocked then 0.22 else 0.72)
    Map.ofList (("threat",threat)::(sources |> List.mapi(fun i ip -> ip,(if i=0 || i=2 then attack*(if state.Blocked then 120. else 900.) else 0.)+60.+50.*(0.5+0.5*fbm (i+40) (state.Now/6.)))))

let private sample state =
    let attack=attackLevel state
    let allowed=760.+140.*fbm 11 (state.Now/7.)
    let malicious=attack*(520.+80.*fbm 12 state.Now)
    let blocked=24.+(if state.Blocked then malicious else malicious*0.15)
    let flagged=10.+(if state.Blocked then 0. else malicious*0.85)
    allowed,blocked,flagged

let private routine=[|"low","Unusual login location for user 4417";"low","Certificate for media expires in 12 days";"medium","Rate limit exceeded on search"
                      "low","New device enrolled via VPN";"medium","Port scan from 192.0.2.140 dropped";"low","Admin session started on bastion"|]

let private raise' severity text state =
    {state with Alerts=({Id=state.AlertId+1;At=state.Now;Severity=severity;Text=text}::state.Alerts) |> List.truncate 8;AlertId=state.AlertId+1}

let init () =
    let force=initialForce()
    let meshX,meshY=meshRange force
    let state={Now=0.;Force=force;Attack=Some -3.;Blocked=false;Labels=true;Selected=None;Hover=None;Pointer=None
               Events=[];NextSecond=1.;Alerts=[];AlertId=0;NextAlert=1.5;Rng=RandomState.create 31337UL;Shown=Map.empty
               MeshX=Extent.ofRange meshX;MeshY=Extent.ofRange meshY;EventAxis=Extent.ofRange (0.,1200.)}
    let state={state with Events=[for k in 0..59 -> sample {state with Now=float(k-60)}]}
    let state={state with EventAxis=Extent.ofRange (0.,eventPeak state.Events)}
    let state=raise' "low" "Admin session started on bastion" state |> raise' "medium" "Rate limit exceeded on search" |> raise' "critical" "Credential stuffing detected against auth"
    {state with Shown=targets state}

let tick dt state =
    let now=state.Now+dt/1000.
    let state={state with Now=now;Force=(if Force.isFinished state.Force then state.Force else Force.step 1 state.Force)}
    let state=
        if now>=state.NextSecond then {state with Events=push 60 (sample state) state.Events |> List.map id;NextSecond=state.NextSecond+1.} else state
    let state=
        if now>=state.NextAlert then
            let pick,rng=uniform state.Rng
            let gap,rng=draw (Distribution.exponential 0.3 |> ok) rng
            let severity,text=
                if attackLevel state>0.5 && not state.Blocked && pick<0.6 then "high",sprintf "Failed logins on auth: %.0f / s from 203.0.113.0/24" (500.+pick*300.)
                else routine[min (routine.Length-1) (int(pick*float routine.Length))]
            raise' severity text {state with NextAlert=now+1.2+gap;Rng=rng}
        else state
    let meshX,meshY=meshRange state.Force
    {state with Shown=approachMap dt 450. 0. state.Shown (targets state)
                MeshX=Extent.update dt meshHalfLife meshX state.MeshX;MeshY=Extent.update dt meshHalfLife meshY state.MeshY
                EventAxis=Extent.update dt eventHalfLife (0.,eventPeak state.Events) state.EventAxis}

let input message state =
    match message with
    | Input.Click "attack" | Input.Key "a" ->
        match state.Attack with
        | Some _ -> {state with Attack=None;Blocked=false} |> raise' "low" "Attack traffic subsided"
        | None -> {state with Attack=Some state.Now;Blocked=false;Force=Force.reheat 0.25 state.Force} |> raise' "critical" "Credential stuffing detected against auth"
    | Input.Click "block" | Input.Key "b" when state.Attack.IsSome && not state.Blocked ->
        {state with Blocked=true} |> raise' "medium" "WAF rule 942100 now blocking 203.0.113.0/24"
    | Input.Click "reheat" | Input.Key "r" -> {state with Force=Force.reheat 0.8 state.Force}
    | Input.Click "labels" | Input.Key "l" -> {state with Labels=not state.Labels}
    | Input.Pick id when id.StartsWith "node:" -> let node=id.Substring 5 in {state with Selected=(if state.Selected=Some node then None else Some node)}
    | Input.Key "Escape" -> {state with Selected=None}
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private shown key state = Map.tryFind key state.Shown |> Option.defaultValue 0.
let private severityColor = function "critical" -> Theme.bad | "high" -> Theme.orange | "medium" -> Theme.warn | _ -> Theme.accent

let private topology state (r:Bounds) =
    let w,h=bw r,bh r
    // The mesh is fitted into the card between the title and the legend, with room for the widest label beside an edge node. The fit
    // follows the decaying extent of the simulated positions, so the mesh stays centred without zooming on every jitter, and its
    // aspect may stretch by at most a quarter to use a wide or tall card.
    let left,right,top,bottom=44.,w-44.,54.,h-56.
    let spanX,spanY=max 40. (state.MeshX.High-state.MeshX.Low),max 40. (state.MeshY.High-state.MeshY.Low)
    let fitX,fitY=(right-left)/spanX,(bottom-top)/spanY
    let fit=min fitX fitY
    let sx,sy=min fitX (fit*1.25),min fitY (fit*1.25)
    let ox=(left+right)/2.-sx*(state.MeshX.Low+state.MeshX.High)/2.
    let oy=(top+bottom)/2.-sy*(state.MeshY.Low+state.MeshY.High)/2.
    let nodes=(Force.snapshot state.Force).Nodes |> List.map(fun n -> n.Id,pt (ox+sx*n.Position.X) (oy+sy*n.Position.Y)) |> Map.ofList
    let focus=state.Hover |> Option.filter(fun h -> h.StartsWith "node:") |> Option.map(fun h -> h.Substring 5) |> Option.orElse state.Selected
    let neighbours=match focus with Some f -> links |> List.collect(fun (a,b) -> if a=f then [b] elif b=f then [a] else []) |> Set.ofList |> Set.add f | None -> Set.empty
    let attack=attackLevel state
    let edges=
        links |> List.mapi(fun index (a,b) ->
            let p,q=nodes[a],nodes[b]
            let hostile=onAttackPath (a,b) && attack>0.
            let lit=focus.IsNone || (neighbours.Contains a && neighbours.Contains b)
            let rate=0.35+0.25*float(index%3)
            let count=if hostile then 3+int(attack*5.) else 2
            let stopAt=if hostile && state.Blocked && a<>"Internet" then 0. else 1.
            let packets=
                [for k in 0..count-1 do
                    let f=(state.Now*rate+float k/float count+float index*0.137)%1.
                    let hostilePacket=hostile && k%2=0
                    if not(hostilePacket && stopAt=0.) && not(hostilePacket && state.Blocked && a="Internet" && f>0.8) then
                        yield Leaf(dot (sprintf "p%d" k) (pt (lerp p.X q.X f) (lerp p.Y q.Y f)) (if hostilePacket then 2.6 else 1.9) (if hostilePacket then Theme.bad else Theme.accent)
                                   |> fade (if lit then 0.95 else 0.2))]
            group (sprintf "e%d" index)
                (Leaf(stroke "line" (segment p q) (if hostile && not state.Blocked then Theme.bad else Theme.edge) (if hostile then 1.8 else 1.2) |> fade (if lit then 0.9 else 0.25))
                 ::packets))
    let nodeMarks=
        tiers |> List.map(fun (id,tier) ->
            let p=nodes[id]
            let r=radiusOf id
            let lit=focus.IsNone || neighbours.Contains id
            let underAttack=attack>0. && id="auth" && not state.Blocked
            let shield=state.Blocked && id="WAF"
            let selected=state.Selected=Some id
            hot ("node:"+id)
                ([if underAttack then
                      for k in 0..1 do
                          let phase=((state.Now*0.9+float k*0.5)%1.)
                          yield Leaf(ring (sprintf "alarm%d" k) p (r+4.+22.*phase) Theme.bad 2. |> fade (1.-phase))
                  if shield then yield Leaf(ring "shield" p (r+6.+1.5*pulse 1.2 state.Now) Theme.good 2.5)
                  yield Leaf(dot "halo" p (r+5.) (tierColor tier) |> fade (if selected then 0.5 elif lit then 0.12 else 0.))
                  yield Leaf(dot "core" p r (if underAttack then mix (tierColor tier) Theme.bad (0.5+0.5*pulse 0.5 state.Now) else tierColor tier)
                             |> Mark.withStroke (paint Theme.background) 2. |> fade (if lit then 1. else 0.3))
                  if state.Labels || selected || state.Hover=Some("node:"+id) then
                      yield Leaf(label "name" p.X (p.Y+r+13.) 10.5 Theme.text 600 "middle" id |> fade (if lit then 0.95 else 0.3))]))
    let tip=
        match state.Hover with
        | Some id when id.StartsWith "node:" ->
            let node=id.Substring 5
            let degree=links |> List.filter(fun (a,b) -> a=node || b=node) |> List.length
            [tooltip "tip" w h nodes[node] [node;sprintf "Tier: %s · %d connection%s" (tierOf node) degree (if degree=1 then "" else "s")
                                            (if node="auth" && attack>0. && not state.Blocked then "Under attack" else "Healthy");"Click to pin neighbours"]]
        | _ -> []
    panel "topology" r "Service topology · live traffic"
        ([group "edges" edges;group "nodes" nodeMarks;legend "legend" 18. (h-16.) ["Edge",Theme.accent;"Service",Theme.violet;"Data",Theme.teal;"Internal",Theme.warn;"Hostile",Theme.bad]]@tip)

/// <summary>The first of several phrasings, longest first, that fits a width at a font size; the shortest when none does.</summary>
let private fitting width size (choices:string list) =
    choices |> List.tryFind(fun choice -> textWidth size choice<=width) |> Option.defaultValue (List.last choices)

let private threat state (r:Bounds) =
    let w,h=bw r,bh r
    let level=shown "threat" state
    let status,color=if level>0.7 then "Critical",Theme.bad elif level>0.4 then "Elevated",Theme.orange elif level>0.25 then "Guarded",Theme.warn else "Low",Theme.good
    // The gauge sizes from the card height (and never takes more than a sixth of its width); the status text sits beside it.
    let radius=clamp 36. 80. (min ((h-62.)/1.78) (w*0.17))
    let cx=34.+radius
    let cy=36.+(h-44.-(radius*1.78+18.))/2.+radius
    let tx=cx+radius+26.
    let room=w-tx-8.
    let detail=
        match state.Attack,state.Blocked with
        | Some _,false -> ["Active credential stuffing";"Credential stuffing"]
        | Some _,true -> ["Attack contained at WAF";"Contained at WAF"]
        | _ -> ["No active incidents";"No incidents"]
    panel "threat" r "Threat level"
        [gauge "gauge" cx cy radius 11. level color (percent 0 level) "risk score"
         Leaf(label "status" tx (cy-18.) 22. color 700 "start" status)
         Leaf(text "detail" tx (cy+6.) 11.5 Theme.muted (fitting room 11.5 detail))
         Leaf(text "mttr" tx (cy+26.) 11. Theme.faint (fitting room 11. [sprintf "Alerts in feed: %d" state.Alerts.Length;sprintf "%d alerts" state.Alerts.Length]))]

let private feed state (r:Bounds) =
    let w,h=bw r,bh r
    // As many rows as the card holds, and alert text cut to the width left after the slide-in offset.
    let capacity=max 1 (int((h-59.)/26.)+1)
    let chars=max 8 (int((w-128.)/textWidth 10.5 "M"))
    let rows=
        state.Alerts |> List.truncate capacity |> List.mapi(fun index alert ->
            let age=state.Now-alert.At
            let enter=outCubic(clamp01(age/0.45))
            at (sprintf "a%d" alert.Id) (16.+(1.-enter)*30.) (46.+float index*26.)
                [Leaf(rrect "pill" 0. -12. 58. 17. 8.5 (severityColor alert.Severity) |> fade (0.22*enter))
                 Leaf(label "severity" 29. 0. 9.5 (severityColor alert.Severity) 700 "middle" (alert.Severity.ToUpperInvariant()) |> fade enter)
                 Leaf(text "text" 66. 0. 10.5 Theme.text (if alert.Text.Length>chars then alert.Text.Substring(0,chars-1)+"…" else alert.Text) |> fade enter)])
    panel "alerts" r "Alert feed" [group "rows" rows]

let private eventBars state (r:Bounds) =
    let w,h=bw r,bh r
    let x0,x1,y0,y1=56.,w-20.,44.,h-26.
    let events=state.Events
    let ys=Scale.linear (0.,state.EventAxis.High*1.1) (y1,y0)
    let slot=(x1-x0)/60.
    let shift=(state.NextSecond-state.Now)*slot
    let bars=
        events |> List.mapi(fun i (allowed,blocked,flagged) ->
            let x=x0+float i*slot+shift-slot
            let opacity=if x<x0 then clamp01((x-x0+slot)/slot) else 1.
            let segments=[allowed,Theme.accent;blocked,Theme.good;flagged,Theme.bad]
            let tops=segments |> List.scan(fun total (v,_) -> total+v) 0.
            group (sprintf "s%d" i)
                [for k,(value,color) in List.indexed segments ->
                    Leaf(rect (sprintf "k%d" k) (max x0 x) (Scale.map ys (tops[k]+value)) (slot*0.78) (Scale.map ys tops[k]-Scale.map ys (tops[k]+value)) color |> fade (opacity*0.9))])
    panel "events" r "Events per second · last minute"
        [yGrid "grid" ys (Scale.ticks 3 ys) x0 x1 (si 1)
         group "bars" bars
         legend "legend" (w-220.) 24. ["Allowed",Theme.accent;"Blocked",Theme.good;"Flagged",Theme.bad]]

let private top state (r:Bounds) =
    let w,h=bw r,bh r
    let ranked=sources |> List.map(fun ip -> ip,shown ip state) |> List.sortByDescending snd
    let maximum=ranked |> List.map snd |> List.max
    // Rows spread over a taller card; bars take the width between the address column and the value column.
    let step=clamp 24. 40. ((h-52.)/6.)
    let barX,valueX=104.,w-32.
    let barMax=max 40. (valueX-barX-44.)
    let rows=
        ranked |> List.mapi(fun index (ip,value) ->
            let hostile=ip.StartsWith "203.0.113" && state.Attack.IsSome
            at ("ip:"+ip) 16. (44.+float index*step)
                [Leaf(label "ip" 0. 12. 11. (if hostile then Theme.bad else Theme.text) 600 "start" ip)
                 Leaf(rrect "bar" barX 2. (barMax*value/maximum) 12. 3. (if hostile then (if state.Blocked then Theme.good else Theme.bad) else Theme.accent) |> fade 0.85)
                 Leaf(label "value" valueX 12. 10.5 Theme.muted 600 "end" (num 0 value))])
    panel "sources" r "Top sources · req/s" rows

let view viewport state =
    page viewport "Security operations" "A force-directed service mesh with live packet flow. Launch a simulated credential-stuffing attack, watch it propagate, then block it at the WAF." "Simulate attack (A) · block at WAF (B) · re-layout (R) · labels (L) · hover nodes, click to pin neighbours, Escape clears"
        [buttonTool "attack" 136. (if state.Attack.IsSome then "Stop attack" else "Simulate attack") state.Attack.IsSome
         buttonTool "block" 112. (if state.Blocked then "Blocked" else "Block at WAF") state.Blocked
         buttonTool "reheat" 90. "Re-layout" false
         buttonTool "labels" 70. "Labels" state.Labels]
        []
        [[item 640. 440. (cell "topology" 440.);item 344. 290. (column [cell "threat" 170.;cell "alerts" 254.])]
         [item 640. 440. (cell "events" 188.);item 344. 290. (cell "sources" 188.)]]
        (fun cells -> [topology state cells["topology"];threat state cells["threat"];feed state cells["alerts"];eventBars state cells["events"];top state cells["sources"]])


let model = {Init=init;Tick=tick;Input=input;View=view}
let dashboard = {Id="security";Title="Security operations";Summary="Force graph, attack, alerts";Start=fun () -> Live.start model}

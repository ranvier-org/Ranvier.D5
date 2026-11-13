/// <summary>Trading desk: live candlesticks with moving averages and Bollinger bands, order-book depth, a trade tape,
/// session performance and a live treemap market map.</summary>
module Ranvier.D5.Dashboards.Markets

open System
open Ranvier.D5
open Ranvier.D5.Dashboards.Kit

type Candle = { Start:float; Open:float; High:float; Low:float; Close:float; Volume:float }
type Ticker = { Name:string; Price:float; Session:float; Volatility:float; Candles:Candle list; Current:Candle; History:float list; Flash:float }
type Trade = { Id:int; Symbol:string; Price:float; Size:float; Buy:bool; At:float }
type State =
    { Now:float; NextTick:float; NextSample:float; Tickers:Ticker[]; Selected:int; Sma:bool; Bands:bool
      Trades:Trade list; TradeId:int; Rng:RandomState; Low:float; High:float; Hover:string option; Pointer:Point option }

let private names=[|"NOVA";"ORBT";"KRTX";"ZENH"|]
let private starts=[|182.4;64.9;311.2;27.35|]
let private vols=[|0.0045;0.0065;0.0035;0.009|]
let private candleSeconds=2.
let private visible=60
let private step=0.1
let private sectors=
    ["Compute",["NOVA",9.;"QBIT",5.;"FABX",4.;"LUMA",3.]
     "Network",["ORBT",6.;"MESH",4.;"WAVE",3.]
     "Energy",["KRTX",7.;"SOLR",4.;"GRID",3.5;"VOLT",2.]
     "Health",["ZENH",3.;"CURA",4.5;"GENO",3.]]

let private open' start price = {Start=start;Open=price;High=price;Low=price;Close=price;Volume=0.}

let private move (ticker:Ticker) now (rng:RandomState) =
    let shock,rng=draw (Distribution.normal 0. (ticker.Volatility*sqrt step) |> ok) rng
    let size,rng=draw (Distribution.logNormal 3. 0.6 |> ok) rng
    let reversion=0.02*(log ticker.Session-log ticker.Price)*step
    let price=ticker.Price*exp(shock+reversion)
    let current=ticker.Current
    let current={current with High=max current.High price;Low=min current.Low price;Close=price;Volume=current.Volume+size}
    let candles,current=
        if now>=current.Start+candleSeconds then
            let closed=ticker.Candles@[current]
            (if closed.Length>visible+30 then List.tail closed else closed),open' (current.Start+candleSeconds) price
        else ticker.Candles,current
    let flash=clamp -1. 1. (shock/(ticker.Volatility*sqrt step)/2.)
    {ticker with Price=price;Current=current;Candles=candles;Flash=if abs flash>abs ticker.Flash*0.6 then flash else ticker.Flash},size,shock>=0.,rng

let private extent state =
    let ticker=state.Tickers[state.Selected]
    let candles=(ticker.Candles |> List.filter(fun c -> c.Start>=state.Now-float visible*candleSeconds-candleSeconds))@[ticker.Current]
    let low,high=candles |> List.map(fun c -> c.Low) |> List.min,candles |> List.map(fun c -> c.High) |> List.max
    let pad=(high-low)*0.12+ticker.Price*0.001
    low-pad,high+pad

let rec private simulate until state =
    if state.NextTick>until then state else
    let now=state.NextTick
    let tickers,rng,trades,tradeId=
        state.Tickers |> Array.indexed |> Array.fold(fun (done',rng,trades,tradeId) (index,ticker) ->
            let next,size,buy,rng=move ticker now rng
            let trades,tradeId=
                if index=state.Selected && size>24. then ({Id=tradeId+1;Symbol=ticker.Name;Price=next.Price;Size=Math.Round size;Buy=buy;At=now}::trades |> List.truncate 9),tradeId+1
                else trades,tradeId
            next::done',rng,trades,tradeId) ([],state.Rng,state.Trades,state.TradeId)
    let tickers=tickers |> List.rev |> List.toArray
    let sample=now>=state.NextSample
    let tickers=if sample then tickers |> Array.map(fun t -> {t with History=push 240 t.Price t.History}) else tickers
    simulate until {state with Tickers=tickers;Rng=rng;Trades=trades;TradeId=tradeId;NextTick=now+step;NextSample=(if sample then state.NextSample+1. else state.NextSample)}

let init () =
    let tickers=names |> Array.mapi(fun index name ->
        {Name=name;Price=starts[index];Session=starts[index];Volatility=vols[index];Candles=[];Current=open' 0. starts[index];History=[starts[index]];Flash=0.})
    let state={Now=0.;NextTick=step;NextSample=1.;Tickers=tickers;Selected=0;Sma=true;Bands=true;Trades=[];TradeId=0;Rng=RandomState.create 2026UL
               Low=0.;High=1.;Hover=None;Pointer=None}
    let warmed=simulate 150. state
    let low,high=extent {warmed with Now=150.}
    {warmed with Now=150.;Low=low;High=high}

let tick dt state =
    let now=state.Now+dt/1000.
    let state=simulate now {state with Now=now}
    let low,high=extent state
    {state with Low=approach dt 450. state.Low low;High=approach dt 450. state.High high
                Tickers=state.Tickers |> Array.map(fun t -> {t with Flash=approach dt 260. t.Flash 0.})}

let input message state =
    let select index={state with Selected=((index%names.Length)+names.Length)%names.Length;Trades=[]}
    match message with
    | Input.Click id when id.StartsWith "symbol:" -> names |> Array.tryFindIndex ((=) (id.Substring 7)) |> Option.map select |> Option.defaultValue state
    | Input.Pick id when id.StartsWith "tile:" -> names |> Array.tryFindIndex ((=) (id.Substring 5)) |> Option.map select |> Option.defaultValue state
    | Input.Click "sma" | Input.Key "m" -> {state with Sma=not state.Sma}
    | Input.Click "bands" | Input.Key "b" -> {state with Bands=not state.Bands}
    | Input.Key "ArrowDown" | Input.Key "ArrowRight" -> select(state.Selected+1)
    | Input.Key "ArrowUp" | Input.Key "ArrowLeft" -> select(state.Selected-1)
    | Input.Hover id -> {state with Hover=id}
    | Input.Pointer point -> {state with Pointer=point}
    | _ -> state

let private up=Theme.good
let private down=Theme.bad
let private change (ticker:Ticker) = ticker.Price/ticker.Session-1.

let private tickerTiles state =
    names |> Array.toList |> List.mapi(fun index name ->
        let ticker=state.Tickers[index]
        let delta=change ticker
        let selected=index=state.Selected
        let flashColor=if ticker.Flash>=0. then up else down
        let background=mix (if selected then Theme.raised else Theme.panel) flashColor (abs ticker.Flash*0.32)
        hot ("tile:"+name)
            [at "tile" (24.+float index*252.) 124.
                [Leaf(rrect "bg" 0. 0. 240. 58. 10. background |> Mark.withStroke (paint (if selected then Theme.accent else Theme.edge)) (if selected then 1.5 else 1.))
                 Leaf(label "name" 14. 22. 13. Theme.text 700 "start" name)
                 Leaf(label "price" 14. 46. 19. Theme.text 700 "start" (num 2 ticker.Price))
                 Leaf(label "change" 226. 22. 12. (if delta>=0. then up else down) 700 "end" (signed 2 (delta*100.)+"%"))
                 spark "spark" (ticker.History |> List.skip (max 0 (ticker.History.Length-60))) 120. 28. 106. 22. (if delta>=0. then up else down)]])

let private movingAverage period (closes:float[]) index =
    let first=max 0 (index-period+1)
    let window=closes[first..index]
    let mean=Array.average window
    mean,sqrt(window |> Array.averageBy(fun c -> (c-mean)*(c-mean)))

let private priceCard state =
    let ticker=state.Tickers[state.Selected]
    let x0,x1,y0,y1,v0,v1=56.,628.,40.,300.,314.,384.
    let span=float visible*candleSeconds
    let xs=Scale.linear (state.Now-span,state.Now+candleSeconds) (x0,x1)
    let ys=Scale.linear (state.Low,state.High) (y1,y0)
    let all=ticker.Candles@[ticker.Current]
    let shown=all |> List.filter(fun c -> c.Start>=state.Now-span-candleSeconds*1.5)
    let volumeMax=shown |> List.map(fun c -> c.Volume) |> List.fold max 1.
    let width=(Scale.map xs candleSeconds-Scale.map xs 0.)*0.68
    let closes=all |> List.map(fun c -> c.Close) |> List.toArray
    let offset=all.Length-shown.Length
    let center (c:Candle)=Scale.map xs (c.Start+candleSeconds/2.)
    let clipX x=clamp x0 x1 x
    let candles=
        shown |> List.map(fun c ->
            let color=if c.Close>=c.Open then up else down
            let x=center c
            let top,bottom=Scale.map ys (max c.Open c.Close),Scale.map ys (min c.Open c.Close)
            let alpha=if x<x0+width then clamp01((x-x0)/width) else 1.
            hot (sprintf "c%.0f" (c.Start*10.))
                [Leaf(stroke "wick" (segment (pt x (Scale.map ys c.High)) (pt x (Scale.map ys c.Low))) color 1.2 |> fade alpha)
                 Leaf(rect "body" (x-width/2.) top width (max 1.2 (bottom-top)) color |> fade (alpha*(if state.Hover=Some(sprintf "c%.0f" (c.Start*10.)) then 1. else 0.9)))
                 Leaf(rect "volume" (x-width/2.) (v1-(v1-v0)*c.Volume/volumeMax) width ((v1-v0)*c.Volume/volumeMax) color |> fade (alpha*0.45))])
    let overlay period color =
        let points=shown |> List.mapi(fun i c -> pt (clipX(center c)) (Scale.map ys (fst(movingAverage period closes (offset+i)))))
        Leaf(stroke (sprintf "sma%d" period) (smooth points) color 1.6)
    let bands=
        let pairs=shown |> List.mapi(fun i c ->
            let mean,deviation=movingAverage 20 closes (offset+i)
            pt (clipX(center c)) (Scale.map ys (mean-2.*deviation)),pt (clipX(center c)) (Scale.map ys (mean+2.*deviation)))
        [Leaf(fill "band" (smoothArea pairs) Theme.violet |> fade 0.13)
         Leaf(stroke "band-top" (smooth (pairs |> List.map snd)) Theme.violet 0.8 |> fade 0.6)
         Leaf(stroke "band-bottom" (smooth (pairs |> List.map fst)) Theme.violet 0.8 |> fade 0.6)]
    let last=Scale.map ys ticker.Price
    let lastColor=if ticker.Price>=ticker.Current.Open then up else down
    let hovered=
        match state.Hover with
        | Some id when id.StartsWith "c" ->
            shown |> List.tryFind(fun c -> sprintf "c%.0f" (c.Start*10.)=id) |> Option.map(fun c ->
                legacyT "tip" (pt (center c) (Scale.map ys c.High))
                    [sprintf "%s · %.0fs ago" ticker.Name (state.Now-c.Start)
                     sprintf "O %s  H %s" (num 2 c.Open) (num 2 c.High)
                     sprintf "L %s  C %s" (num 2 c.Low) (num 2 c.Close)
                     sprintf "Vol %s" (grouped c.Volume)]) |> Option.toList
        | _ -> []
    let cross=
        match state.Pointer with
        | Some p when p.X-24.>=x0 && p.X-24.<=x1 && p.Y-198.>=y0 && p.Y-198.<=y1 ->
            let y=p.Y-198.
            [Leaf(stroke "cross" (segment (pt x0 y) (pt x1 y)) Theme.muted 1. |> fade 0.5)
             Leaf(rrect "cross-bg" (x1+2.) (y-9.) 54. 18. 4. Theme.edge)
             Leaf(label "cross-price" (x1+29.) (y+4.) 10.5 Theme.text 600 "middle" (num 2 (Scale.invert ys y)))]
        | _ -> []
    card "price" 24. 198. 700. 400. (sprintf "%s · 2 s candles" ticker.Name)
        ([yGrid "grid" ys (Scale.ticks 5 ys) x0 x1 (num 1)
          group "candles" candles]
         @ (if state.Bands then [group "bands" bands] else [])
         @ (if state.Sma then [overlay 9 Theme.warn;overlay 21 Theme.accent] else [])
         @ [Leaf(stroke "last" (segment (pt x0 last) (pt x1 last)) lastColor 1. |> fade 0.7)
            Leaf(rrect "last-bg" (x1+2.) (last-9.) 54. 18. 4. lastColor)
            Leaf(label "last-price" (x1+29.) (last+4.) 10.5 Theme.background 700 "middle" (num 2 ticker.Price))
            legend "legend" 330. 24. ["SMA 9",Theme.warn;"SMA 21",Theme.accent;"Bollinger 20,2",Theme.violet]]
         @ cross @ hovered)

let private depthCard state =
    let ticker=state.Tickers[state.Selected]
    let price=ticker.Price
    let levels side =
        [for level in 1..16 ->
            let offset=float level*price*0.00045
            let size=30.+90.*(0.5+0.5*noise (side*100+level) (state.Now*0.9+float level*0.37))
            (if side=0 then price-offset else price+offset),size]
    let cumulative (rows:(float*float) list) = rows |> List.scan(fun (_,total) (p,s) -> p,total+s) (price,0.) |> List.tail
    let bids,asks=cumulative (levels 0),cumulative (levels 1)
    let maxDepth=max (snd(List.last bids)) (snd(List.last asks))
    let x0,x1,y0,y1=16.,268.,40.,214.
    let xs=Scale.linear (price*(1.-0.0075),price*(1.+0.0075)) (x0,x1)
    let ys=Scale.linear (0.,maxDepth*1.08) (y1,y0)
    let stepArea (rows:(float*float) list) =
        let points=rows |> List.collect(fun (p,total) -> [pt (Scale.map xs p) (Scale.map ys total)])
        let edge=pt (Scale.map xs price) y1
        polygon (edge::(pt (Scale.map xs price) (Scale.map ys (snd rows.Head)))::points@[pt (List.last points).X y1])
    card "depth" 740. 198. 284. 230. "Order book depth"
        [Leaf(fill "bids" (stepArea bids) up |> fade 0.35)
         Leaf(fill "asks" (stepArea asks) down |> fade 0.35)
         Leaf(stroke "mid" (segment (pt (Scale.map xs price) y0) (pt (Scale.map xs price) y1)) Theme.text 1. |> fade 0.5)
         Leaf(label "mid-price" (Scale.map xs price) (y0-4.) 11. Theme.text 700 "middle" (num 2 price))
         Leaf(label "bid-total" x0 (y1+4.) 10. up 600 "start" ("bids "+grouped (snd(List.last bids))))
         Leaf(label "ask-total" x1 (y1+4.) 10. down 600 "end" ("asks "+grouped (snd(List.last asks))))]

let private tapeCard state =
    let rows=
        state.Trades |> List.mapi(fun index trade ->
            let age=state.Now-trade.At
            let slide=outCubic(clamp01(age/0.35))
            let y=50.+float index*18.-(1.-slide)*8.
            at (sprintf "t%d" trade.Id) 0. y
                [Leaf(label "time" 16. 0. 11. Theme.faint 400 "start" (sprintf "%.1fs" age) |> fade slide)
                 Leaf(label "price" 110. 0. 11.5 (if trade.Buy then up else down) 700 "end" (num 2 trade.Price) |> fade slide)
                 Leaf(rrect "size-bar" 124. -9. (min 120. (trade.Size/2.)) 10. 2. (if trade.Buy then up else down) |> fade (0.35*slide))
                 Leaf(label "size" 268. 0. 11. Theme.muted 400 "end" (grouped trade.Size) |> fade slide)])
    card "tape" 740. 444. 284. 154. "Trade tape" [group "rows" (rows |> List.truncate 6)]

let private sessionCard state =
    let x0,x1,y0,y1=48.,560.,40.,140.
    let series=state.Tickers |> Array.map(fun t -> t.History |> List.map(fun price -> price/t.Session-1.))
    let extreme=series |> Array.collect List.toArray |> Array.fold(fun m v -> max m (abs v)) 0.01
    let count=series[0].Length
    let xs=Scale.linear (0.,float(max 1 (count-1))) (x0,x1)
    let ys=Scale.linear (-extreme*1.1,extreme*1.1) (y1,y0)
    let ends=dodge 12. [for index,values in Array.indexed series -> names[index],Scale.map ys (List.last values)]
    let lines=
        series |> Array.toList |> List.mapi(fun index values ->
            let color=Theme.series[index]
            let points=values |> List.mapi(fun i v -> pt (Scale.map xs (float i)) (Scale.map ys v))
            group names[index]
                [Leaf(stroke "line" (smooth points) color (if index=state.Selected then 2.4 else 1.2) |> fade (if index=state.Selected then 1. else 0.55))
                 Leaf(label "name" (x1+8.) (ends[names[index]]+4.) 10. color 700 "start" (sprintf "%s %s%%" names[index] (signed 1 ((List.last values)*100.))))])
    let zero=Scale.map ys 0.
    let cap=sectors |> List.collect snd |> List.map(fun (name,weight) -> name,weight*(1.+0.25*noise (stableHash name%997) (state.Now/40.)))
    let root=Hierarchy.node "market" 0. [for sector,members in sectors -> Hierarchy.node sector 0. [for name,_ in members -> Hierarchy.node name (cap |> List.find(fun (n,_) -> n=name) |> snd) []]]
    let tiles=Treemap.layout {TreemapConfig.create (box 0. 0. 320. 118.) with Tiling=TreemapTiling.Binary;Padding=1.5} root |> ok |> List.filter(fun b -> b.Depth=2)
    let moveOf (name:string) =
        match Array.tryFindIndex ((=) name) names with
        | Some index -> change state.Tickers[index]
        | None -> 0.012*fbm (stableHash name%991) (state.Now/12.)
    let heat=
        tiles |> List.map(fun tile ->
            let delta=moveOf tile.Id
            let color=if delta>=0. then mix (52,58,72) up (clamp01(delta/0.012)) else mix (52,58,72) down (clamp01(-delta/0.012))
            let b=tile.Bounds
            hot ("map:"+tile.Id)
                (Leaf(rect "cell" (Bounds.x b) (Bounds.y b) (Bounds.width b) (Bounds.height b) color |> fade (emphasis (state.Hover |> Option.filter(fun h -> h.StartsWith "map:")) ("map:"+tile.Id)))
                 ::(if Bounds.width b>34. && Bounds.height b>26. then
                        [Leaf(label "name" (Bounds.x b+4.) (Bounds.y b+13.) 10. Theme.text 700 "start" tile.Id)
                         Leaf(label "move" (Bounds.x b+4.) (Bounds.y b+25.) 9.5 Theme.text 400 "start" (signed 2 (delta*100.)+"%") |> fade 0.85)]
                    else [])))
    card "session" 24. 614. 1000. 162. "Session performance · market map"
        [yGrid "grid" ys [-extreme;0.;extreme] x0 x1 (fun v -> signed 1 (v*100.)+"%")
         Leaf(stroke "zero" (segment (pt x0 zero) (pt x1 zero)) Theme.muted 1. |> fade 0.5)
         group "lines" lines
         at "map" 664. 32. heat]

let view state =
    let ticker=state.Tickers[state.Selected]
    let delta=change ticker
    [header "Trading desk" "Four simulated instruments tick ten times a second; candles, averages, bands and the depth book update live." "Click a ticker or use ↑/↓ to switch · hover candles for OHLC · move over the chart for a price cursor · M toggles averages · B toggles bands"
     group "controls"
        (segmented "symbol" 24. 84. 66. [for name in names -> name,name] ticker.Name
         @ [button "sma" 316. 84. 82. "Averages" state.Sma
            button "bands" 404. 84. 70. "Bands" state.Bands
            at "quote" 820. 84.
                [Leaf(rrect "bg" 0. 0. 204. 26. 13. (mix Theme.raised (if ticker.Flash>=0. then up else down) (abs ticker.Flash*0.4)) |> Mark.withStroke (paint Theme.edge) 1.)
                 Leaf(label "symbol" 14. 17.5 11.5 Theme.muted 700 "start" ticker.Name)
                 Leaf(label "price" 110. 17.5 12.5 Theme.text 700 "end" (num 2 ticker.Price))
                 Leaf(label "delta" 190. 17.5 11.5 (if delta>=0. then up else down) 700 "end" ((if delta>=0. then "▲ " else "▼ ")+num 2 (abs delta*100.)+"%"))]])
     group "tickers" (tickerTiles state)
     priceCard state
     depthCard state
     tapeCard state
     sessionCard state]
    @ (match state.Hover with
       | Some id when id.StartsWith "map:" ->
           match state.Pointer with
           | Some p -> [legacyT "map-tip" p [id.Substring 4;"Live market-cap weight";"Colour: session move"]]
           | None -> []
       | _ -> [])

let model = {Init=init;Tick=tick;Input=input;View=legacy view}
let dashboard = {Id="markets";Title="Trading desk";Summary="Candles, depth book, market map";Start=fun () -> Live.start model}

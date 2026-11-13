/// <summary>Shared vocabulary for the dashboard gallery: theme, pure model contract, smoothing and widget marks.</summary>
module Ranvier.D5.Dashboards.Kit

open System
open System.Globalization
open Ranvier.D5

/// <summary>An sRGB colour as integer channels.</summary>
type Rgb = int*int*int

let inv = CultureInfo.InvariantCulture
let clamp lo hi (value:float) = Math.Clamp(value,lo,hi)
let clamp01 value = clamp 0. 1. value
let lerp a b t = a+(b-a)*t
let tau = 2.*Math.PI
let ok = function Ok value -> value | Error errors -> failwithf "%A" errors

let paint ((r,g,b):Rgb) = Paint.rgb r g b
let mix ((r1,g1,b1):Rgb) ((r2,g2,b2):Rgb) t : Rgb =
    let t=clamp01 t
    let channel a b = int(Math.Round(lerp (float a) (float b) t)) |> max 0 |> min 255
    channel r1 r2,channel g1 g2,channel b1 b2
/// <summary>Piecewise-linear colour ramp over evenly spaced stops.</summary>
let ramp (stops:Rgb list) t =
    let colors=List.toArray stops
    let position=clamp01 t*float(colors.Length-1)
    let index=min (colors.Length-2) (int position)
    mix colors[index] colors[index+1] (position-float index)

[<RequireQualifiedAccess>]
module Theme =
    let background:Rgb=10,15,28
    let panel:Rgb=18,26,43
    let raised:Rgb=26,36,57
    let edge:Rgb=36,50,76
    let grid:Rgb=34,46,68
    let text:Rgb=228,234,243
    let muted:Rgb=134,150,176
    let faint:Rgb=86,102,128
    let accent:Rgb=56,189,248
    let good:Rgb=52,211,153
    let warn:Rgb=251,191,36
    let bad:Rgb=248,113,113
    let violet:Rgb=167,139,250
    let pink:Rgb=244,114,182
    let orange:Rgb=251,146,60
    let teal:Rgb=45,212,191
    let blue:Rgb=96,165,250
    let lime:Rgb=163,230,53
    let series=[|accent;violet;good;warn;pink;orange;teal;blue;lime;bad|]
    let heat=[ (20,29,48);(30,64,175);(6,182,212);(250,204,21);(239,68,68) ]
    let cool=[ (20,29,48);(37,99,235);(56,189,248);(186,230,253) ]

/// <summary>Input delivered to a dashboard: a control button, a picked data item, hover, the pointer in page coordinates, a drag
/// that started on empty space (the pointer movement in pixels), or a key.</summary>
[<RequireQualifiedAccess>]
type Input =
    | Click of string
    | Pick of string
    | Hover of string option
    | Pointer of Point option
    | Drag of dx:float * dy:float
    | Key of string

/// <summary>A laid-out dashboard page: its nodes in page coordinates and its total height, which may exceed the viewport and scroll.</summary>
type Page = { Height:float; Nodes:SceneNode list }

/// <summary>A pure dashboard: initial state, a frame step in milliseconds, input handling and a view of the state laid out for a
/// content viewport.</summary>
type Model<'S> = { Init:unit -> 'S; Tick:float -> 'S -> 'S; Input:Input -> 'S -> 'S; View:Viewport -> 'S -> Page }

/// <summary>An immutable running dashboard with its state type hidden; every step returns a new value.</summary>
type Live = { Tick:float -> Live; Send:Input -> Live; View:Viewport -> Page }

[<RequireQualifiedAccess>]
module Live =
    let rec private wrap (model:Model<'S>) (state:'S) =
        {Tick=(fun dt -> wrap model (model.Tick dt state));Send=(fun input -> wrap model (model.Input input state));View=(fun viewport -> model.View viewport state)}
    /// <summary>Starts a model from its initial state.</summary>
    let start (model:Model<'S>) = wrap model (model.Init())

/// <summary>A gallery entry: identity, sidebar text and a factory for a fresh running instance.</summary>
type Dashboard = { Id:string; Title:string; Summary:string; Start:unit -> Live }

// ---------------------------------------------------------------- motion

/// <summary>Frame-rate independent exponential approach of a displayed value toward its target; tau is in milliseconds.</summary>
let approach dt tau current target = if tau<=0. then target else target+(current-target)*exp(-dt/tau)
/// <summary>Approaches every target in a map; new keys enter from <c>start</c> and missing keys are dropped.</summary>
let approachMap dt tau start (current:Map<string,float>) (targets:Map<string,float>) =
    targets |> Map.map(fun key target -> approach dt tau (Map.tryFind key current |> Option.defaultValue start) target)
let ease name = Ranvier.Anime.Ease.named name
let outCubic = ease "outCubic"
let inOutSine = ease "inOutSine"
let pulse period (seconds:float) = 0.5+0.5*sin(tau*seconds/period)
/// <summary>Opacity of an item given the hovered item: full when nothing or this item is hovered, dimmed otherwise.</summary>
let emphasis (hovered:string option) id = match hovered with None -> 1. | Some other when other=id -> 1. | _ -> 0.22

let hash (seed:int) (index:int) =
    let mutable h=uint32 seed*374761393u+uint32 index*668265263u
    h<-(h^^^(h>>>13))*1274126177u
    float(h^^^(h>>>16))/4294967295.
/// <summary>A process-independent string hash (String.GetHashCode is randomised per process).</summary>
let stableHash (value:string) = value |> Seq.fold(fun hash c -> (hash*31+int c) &&& 0x7fffffff) 7
/// <summary>Deterministic smooth value noise in [-1,1].</summary>
let noise seed (t:float) =
    let index=int(floor t)
    let f=t-floor t
    let s=f*f*(3.-2.*f)
    lerp (hash seed index) (hash seed (index+1)) s*2.-1.
/// <summary>Three octaves of value noise, roughly in [-1,1].</summary>
let fbm seed t = noise seed t*0.6+noise (seed+17) (t*2.3)*0.3+noise (seed+41) (t*5.1)*0.1

/// <summary>Uniform float in [0,1) and the next explicit random state.</summary>
let uniform (state:RandomState) = let struct(value,next)=RandomState.nextFloat state in value,next
/// <summary>One draw from a validated distribution and the next state.</summary>
let draw distribution (state:RandomState) = let struct(value,next)=Distribution.sample distribution state in value,next

/// <summary>Appends a value and keeps the most recent <c>count</c> values.</summary>
let push count (value:'T) (values:'T list) =
    let next=values@[value]
    if next.Length>count then List.skip (next.Length-count) next else next

// ---------------------------------------------------------------- formatting

let num digits (value:float) = Format.number inv digits value
let si digits (value:float) = Format.si inv digits value
let percent digits (value:float) = Format.percent inv digits value
let grouped (value:float) = Math.Round(value).ToString("N0",inv)
let money (value:float) = "$"+si 1 value
let signed digits (value:float) = (if value>=0. then "+" else "")+num digits value

// ---------------------------------------------------------------- geometry and marks

let pt x y = Point.create x y
let box x y w h = Bounds.create x y (max 0. w) (max 0. h)
let segment (a:Point) (b:Point) = Path.ofCommands [MoveTo a;LineTo b]
let polyline (points:Point list) =
    match points with
    | [] | [_] -> Path.empty
    | first::rest -> Path.ofCommands (MoveTo first::(rest |> List.map LineTo))
let polygon (points:Point list) =
    match points with
    | [] | [_] -> Path.empty
    | first::rest -> Path.ofCommands (MoveTo first::(rest |> List.map LineTo)@[Close])
/// <summary>A rectangle with circular corners; the radius clamps to half the shorter side.</summary>
let roundRect x y w h r =
    let w,h=max 0. w,max 0. h
    let r=min r (min (w/2.) (h/2.)) |> max 0.
    if r<=0.01 then Shape.rectangle (box x y w h) else
    Path.ofCommands [MoveTo(pt (x+r) y);LineTo(pt (x+w-r) y);ArcTo(r,r,0.,false,true,pt (x+w) (y+r));LineTo(pt (x+w) (y+h-r))
                     ArcTo(r,r,0.,false,true,pt (x+w-r) (y+h));LineTo(pt (x+r) (y+h));ArcTo(r,r,0.,false,true,pt x (y+h-r))
                     LineTo(pt x (y+r));ArcTo(r,r,0.,false,true,pt (x+r) y);Close]
let circlePath radius = if radius<=0. then Path.empty else Shape.arc 0. radius 0. tau
let arcPath inner outer start finish corner =
    Shapes.arc {InnerRadius=inner;OuterRadius=outer;StartAngle=start;EndAngle=finish;PadAngle=0.;CornerRadius=corner}
let smooth (points:Point list) = Curves.line CurveSpec.MonotoneX (points |> List.map Some)
let smoothArea (pairs:(Point*Point) list) = Curves.area CurveSpec.MonotoneX (pairs |> List.map Some)

let fill key path color = Mark.path key path |> Mark.withFill (paint color)
let stroke key path color width = Mark.path key path |> Mark.withFill Paint.none |> Mark.withStroke (paint color) width
let rect key x y w h color = fill key (Shape.rectangle (box x y w h)) color
let rrect key x y w h r color = fill key (roundRect x y w h r) color
/// <summary>A filled circle drawn once at the origin and moved by its transform, so moving dots keep a cached path.</summary>
let dot key (center:Point) radius color =
    fill key (circlePath radius) color |> Mark.withTransform (Transform.translate center.X center.Y)
let ring key (center:Point) radius color width =
    stroke key (circlePath radius) color width |> Mark.withTransform (Transform.translate center.X center.Y)
let fade opacity mark = Mark.withOpacity (clamp01 opacity) mark
let label key x y size color weight anchor (value:string) =
    Mark.text key (pt x y) value |> Mark.withFill (paint color) |> Mark.withFontSize size
    |> Mark.withTextStyle {Family="Segoe UI, Inter, sans-serif";Weight=weight;Anchor=anchor;Baseline="alphabetic"}
let text key x y size color value = label key x y size color 400 "start" value
let caption key x y value = label key x y 11. Theme.muted 400 "start" value

let group key children = Group(GroupSpec.create key,children)
let at key x y children = Group({GroupSpec.create key with Transform=Transform.translate x y},children)
/// <summary>Marks the children as a hoverable, pickable data item reported as <c>Input.Hover</c> and <c>Input.Pick</c>.</summary>
let hot id children = Group(GroupSpec.create ("h:"+id),children)
/// <summary>A non-interactive layer (crosshairs, cursors, tooltips): the pointer passes through it to the items underneath.</summary>
let overlay key children = Group(GroupSpec.create ("x:"+key),children)

// ---------------------------------------------------------------- widgets

let bx (bounds:Bounds) = Bounds.x bounds
let by (bounds:Bounds) = Bounds.y bounds
let bw (bounds:Bounds) = Bounds.width bounds
let bh (bounds:Bounds) = Bounds.height bounds

/// <summary>The key of a rectangular clip from the origin to (w, h); the shell defines one clip per distinct size it finds.</summary>
let clipKey w h = sprintf "clip-%dx%d" (int(Math.Round(w:float))) (int(Math.Round(h:float)))
/// <summary>Clips the children to the rectangle from the local origin to (w, h).</summary>
let clipped key w h children = Group({GroupSpec.create key with Clip=Some(clipKey w h)},children)

/// <summary>A dashboard card with an uppercase title; children use card-local coordinates and are clipped to the card.</summary>
let card key x y w h (title:string) children =
    at key x y [Leaf(rrect "bg" 0. 0. w h 10. Theme.panel |> Mark.withStroke (paint Theme.edge) 1.)
                Leaf(label "title" 16. 24. 11. Theme.muted 700 "start" (title.ToUpperInvariant()))
                clipped "body" w h children]
/// <summary>A card filling laid-out bounds.</summary>
let panel key (bounds:Bounds) title children = card key (bx bounds) (by bounds) (bw bounds) (bh bounds) title children

/// <summary>A clickable control reported as <c>Input.Click id</c>.</summary>
let button id x y w (caption:string) active =
    at ("b:"+id) x y
        [Leaf(rrect "bg" 0. 0. w 26. 6. (if active then Theme.accent else Theme.raised) |> Mark.withStroke (paint (if active then Theme.accent else Theme.edge)) 1.)
         Leaf(label "text" (w/2.) 17.5 12. (if active then Theme.background else Theme.text) 600 "middle" caption)]

/// <summary>A row of buttons whose ids are <c>prefix:option</c>; the selected option is highlighted.</summary>
let segmented prefix x y width (options:(string*string) list) selected =
    options |> List.mapi(fun index (id,caption) -> button (prefix+":"+id) (x+float index*(width+6.)) y width caption (id=selected))

/// <summary>Area-and-line sparkline scaled to its own extent.</summary>
let spark key (values:float list) x y w h color =
    if values.Length<2 then group key [] else
    let low,high=List.min values,List.max values
    let span=if high-low<1e-9 then 1. else high-low
    let points=values |> List.mapi(fun index value -> pt (x+w*float index/float(values.Length-1)) (y+h-(value-low)/span*h))
    group key [Leaf(fill "area" (smoothArea (points |> List.map(fun p -> pt p.X (y+h),p))) color |> fade 0.16)
               Leaf(stroke "line" (smooth points) color 1.6)]

/// <summary>A 270-degree gauge with rounded caps, centred at (cx, cy).</summary>
let gauge key cx cy radius thickness value color (valueText:string) (caption:string) =
    let first,last= -2.35,2.35
    let amount=clamp01 value
    at key cx cy
        [Leaf(fill "track" (arcPath (radius-thickness) radius first last (thickness/2.)) Theme.raised)
         Leaf(fill "value" (if amount<0.004 then Path.empty else arcPath (radius-thickness) radius first (first+(last-first)*amount) (thickness/2.)) color)
         Leaf(label "reading" 0. 8. (radius*0.36) Theme.text 700 "middle" valueText)
         Leaf(label "caption" 0. (radius*0.78+14.) 11. Theme.muted 400 "middle" caption)]

/// <summary>A KPI tile: title, headline value, coloured delta and a trend sparkline.</summary>
let kpi key x y w h (title:string) (value:string) (delta:string) deltaColor (trend:float list) color =
    at key x y
        [Leaf(rrect "bg" 0. 0. w h 10. Theme.panel |> Mark.withStroke (paint Theme.edge) 1.)
         Leaf(label "title" 16. 24. 11. Theme.muted 700 "start" (title.ToUpperInvariant()))
         Leaf(label "value" 16. 58. 27. Theme.text 700 "start" value)
         Leaf(label "delta" 16. (h-14.) 12. deltaColor 600 "start" delta)
         spark "trend" trend (w*0.52) 36. (w*0.48-14.) (h-54.) color]

/// <summary>Horizontal gridlines with left-aligned tick labels for a linear scale.</summary>
let yGrid key (scale:Continuous) (ticks:float list) x0 x1 (format:float -> string) =
    group key [for index,tick in List.indexed ticks do
                 let y=Scale.map scale tick
                 yield Leaf(stroke (sprintf "g%d" index) (segment (pt x0 y) (pt x1 y)) Theme.grid 1.)
                 yield Leaf(label (sprintf "l%d" index) (x0-6.) (y+4.) 10. Theme.faint 400 "end" (format tick))]

/// <summary>Bottom tick labels at arbitrary positions.</summary>
let xLabels key y (ticks:(float*string) list) =
    group key [for index,(x,value) in List.indexed ticks -> Leaf(label (sprintf "x%d" index) x y 10. Theme.faint 400 "middle" value)]

/// <summary>A compact legend row of coloured swatches.</summary>
let legend key x y (items:(string*Rgb) list) =
    let offsets=items |> List.scan(fun cursor (name:string,_) -> cursor+28.+float name.Length*6.2) x
    group key [for index,((name,color),cursor) in List.indexed (List.zip items (List.truncate items.Length offsets)) do
                 yield Leaf(rrect (sprintf "s%d" index) cursor (y-9.) 10. 10. 2. color)
                 yield Leaf(text (sprintf "t%d" index) (cursor+15.) y 11. Theme.muted name)]

/// <summary>Spreads label positions upward-sorted so neighbours sit at least <c>gap</c> apart, keeping their order.</summary>
let dodge gap (positions:(string*float) list) =
    positions |> List.sortBy snd
    |> List.fold(fun placed (key,y) -> match placed with (_,previous)::_ when y<previous+gap -> (key,previous+gap)::placed | _ -> (key,y)::placed) []
    |> Map.ofList

/// <summary>A tooltip box anchored near a point, kept inside the area from the local origin to (limitW, limitH), usually its card.</summary>
let tooltip key limitW limitH (anchor:Point) (lines:string list) =
    let width=12.+(lines |> List.map(fun line -> float line.Length*6.6) |> List.fold max 60.)
    let height=10.+18.*float lines.Length
    let x=if anchor.X+16.+width>limitW-8. then max 8. (anchor.X-16.-width) else anchor.X+16.
    let y=clamp 8. (max 8. (limitH-height-8.)) (anchor.Y-height/2.)
    Group({GroupSpec.create ("x:"+key) with Transform=Transform.translate x y},
        Leaf(rrect "bg" 0. 0. width height 6. Theme.raised |> Mark.withStroke (paint Theme.edge) 1. |> fade 0.96)::
         (lines |> List.mapi(fun index line -> Leaf(label (sprintf "t%d" index) 8. (19.+18.*float index) 12. (if index=0 then Theme.text else Theme.muted) (if index=0 then 700 else 400) "start" line))))

// ---------------------------------------------------------------- responsive page layout

/// <summary>Outer page margin and the gap between cards.</summary>
let Margin,Gap=24.,16.

/// <summary>Approximate rendered width of a string in the UI font; bold text runs about 6% wider.</summary>
let textWidth size (value:string) = float value.Length*size*0.53

/// <summary>Greedy word wrap of a string into lines no wider than <c>width</c> at a font size.</summary>
let wrap width size (value:string) =
    value.Split ' ' |> Array.fold(fun (lines:string list) word ->
        match lines with
        | current::rest when textWidth size (current+" "+word)<=width -> (current+" "+word)::rest
        | _ -> word::lines) [] |> List.rev

/// <summary>A toolbar entry: its width and a function drawing it at (x, y).</summary>
type Tool = { Width:float; Draw:float -> float -> SceneNode list }
let tool width draw = {Width=width;Draw=draw}
let buttonTool id width caption active = tool width (fun x y -> [button id x y width caption active])
let segmentedTool prefix width (options:(string*string) list) selected =
    tool (float options.Length*(width+6.)-6.) (fun x y -> segmented prefix x y width options selected)
/// <summary>A non-interactive status pill with a coloured dot.</summary>
let statusTool key width (caption:string) color dotOpacity =
    tool width (fun x y ->
        [at key x y
            [Leaf(rrect "bg" 0. 0. width 26. 13. Theme.raised |> Mark.withStroke (paint Theme.edge) 1.)
             Leaf(dot "dot" (pt 16. 13.) 4.5 color |> fade dotOpacity)
             Leaf(label "text" 30. 17.5 11.5 Theme.text 700 "start" caption)]])

/// <summary>A pane of the page grid: a card cell with its design height, or a vertical stack of panes.</summary>
[<RequireQualifiedAccess>]
type Pane =
    | Cell of key:string * height:float
    | Column of Pane list

/// <summary>A row entry: its width share at full size (normally its design width), the narrowest width it may take, and its pane.</summary>
type Item = { Grow:float; Min:float; Pane:Pane }
let item grow minWidth pane = {Grow=grow;Min=minWidth;Pane=pane}
let cell key height = Pane.Cell(key,height)
let column panes = Pane.Column panes

let rec private natural = function
    | Pane.Cell(_,height) -> height
    | Pane.Column panes -> (panes |> List.sumBy natural)+Gap*float(max 0 (panes.Length-1))

let rec private place x y w h pane =
    match pane with
    | Pane.Cell(key,_) -> [key,box x y w h]
    | Pane.Column panes ->
        let inner=panes |> List.sumBy natural
        let spare=h-Gap*float(panes.Length-1)
        panes |> List.fold(fun (top,acc) child ->
            let height=spare*natural child/inner
            top+height+Gap,acc@place x top w height child) (y,[]) |> snd

/// <summary>Splits a row into the fewest balanced lines in which every item gets at least its minimum width.</summary>
let private lines width (row:Item list) =
    let fits (line:Item list) =
        let total=line |> List.sumBy(fun i -> i.Grow)
        let room=width-Gap*float(line.Length-1)
        line |> List.forall(fun i -> room*i.Grow/total>=i.Min-0.5)
    let split k =
        let n=row.Length
        [for line in 0..k-1 -> row |> List.skip (line*n/k) |> List.take ((line+1)*n/k-line*n/k)]
    [1..row.Length] |> List.map split |> List.tryFind(List.forall fits) |> Option.defaultWith(fun () -> split row.Length)

/// <summary>Lays rows of items out from <c>top</c>: rows wrap into balanced lines when items would fall below their minimum width,
/// and line heights stretch (up to double) to fill a taller viewport. Returns each cell's bounds and the bottom edge.</summary>
let layout (viewport:Viewport) top bottomReserve (rows:Item list list) =
    let width=viewport.Width-2.*Margin
    let all=rows |> List.collect(lines width)
    let heights=all |> List.map(fun line -> line |> List.map(fun i -> natural i.Pane) |> List.max)
    let fixedGaps=Gap*float(max 0 (all.Length-1))
    let room=viewport.Height-top-bottomReserve-fixedGaps
    let stretch=clamp 1. 2. (room/List.sum heights)
    let cells,bottom=
        List.zip all heights |> List.fold(fun (acc,y) (line,height) ->
            let height=height*stretch
            let total=line |> List.sumBy(fun i -> i.Grow)
            let room=width-Gap*float(line.Length-1)
            let placed,_=line |> List.fold(fun (acc,x) i -> let w=room*i.Grow/total in acc@place x y w height i.Pane,x+w+Gap) ([],Margin)
            acc@placed,y+height+Gap) ([],top)
    Map.ofList cells,bottom-Gap

/// <summary>A whole dashboard page for a viewport: a wrapped title and summary, a toolbar that wraps (right-hand tools align right
/// when they fit), a responsive card grid, and the help line at the foot. <c>draw</c> receives each cell's bounds.</summary>
let page (viewport:Viewport) (title:string) (summary:string) (help:string) (left:Tool list) (right:Tool list) (rows:Item list list)
         (draw:Map<string,Bounds> -> SceneNode list) =
    let summaryLines=wrap (viewport.Width-2.*Margin) 13. summary
    let toolbarY=62.+18.*float(summaryLines.Length-1)+22.
    let width=viewport.Width
    let (leftEnd,leftLine),leftPlaced=
        left |> List.fold(fun ((x,line),acc) t ->
            if x>Margin && x+t.Width>width-Margin then (Margin+t.Width+10.,line+1),acc@[t,Margin,line+1]
            else (x+t.Width+10.,line),acc@[t,x,line]) ((Margin,0),[])
    let rightWidth=(right |> List.sumBy(fun t -> t.Width))+10.*float(max 0 (right.Length-1))
    let rightPlaced,lastLine=
        if right.IsEmpty then [],leftLine
        elif width-Margin-rightWidth>=leftEnd+6. then
            right |> List.fold(fun (acc,x) t -> acc@[t,x,leftLine],x+t.Width+10.) ([],width-Margin-rightWidth) |> fst,leftLine
        else
            right |> List.fold(fun (acc,x) t -> acc@[t,x,leftLine+1],x+t.Width+10.) ([],width-Margin-rightWidth) |> fst,leftLine+1
    let tools=leftPlaced@rightPlaced |> List.collect(fun (t,x,line) -> t.Draw x (toolbarY+34.*float line))
    let top=toolbarY+34.*float(lastLine+1)+6.
    let helpLines=wrap (viewport.Width-2.*Margin) 11. help
    let reserve=16.+16.*float helpLines.Length
    let cells,bottom=layout viewport top reserve rows
    let height=max viewport.Height (bottom+reserve)
    let helpTop=height-10.-16.*float(helpLines.Length-1)
    {Height=height
     Nodes=
        [group "header"
            ([Leaf(label "title" Margin 40. 24. Theme.text 700 "start" title)]
             @ (summaryLines |> List.mapi(fun index line -> Leaf(text (sprintf "summary%d" index) Margin (62.+18.*float index) 13. Theme.muted line)))
             @ (helpLines |> List.mapi(fun index line -> Leaf(text (sprintf "help%d" index) Margin (helpTop+16.*float index) 11. Theme.faint line))))
         group "controls" tools]
        @ draw cells}

// ---------------------------------------------------------------- reusable behaviours

/// <summary>A decaying axis extent: it widens at once to take in new values and relaxes toward the current range with a half-life,
/// so an axis remembers recent extremes instead of jumping to whatever is on screen.</summary>
type Extent = { Low:float; High:float }

[<RequireQualifiedAccess>]
module Extent =
    /// <summary>An extent that starts at a range.</summary>
    let ofRange (low,high) = {Low=low;High=high}
    /// <summary>Takes in the current range at once, then relaxes toward it with a half-life in milliseconds.</summary>
    let update dt halfLife (low,high) extent =
        let keep=Math.Pow(0.5,dt/halfLife)
        {Low=min low (low+(extent.Low-low)*keep);High=max high (high+(extent.High-high)*keep)}
    /// <summary>The extent with fractional padding on both sides, as a scale domain.</summary>
    let padded fraction extent =
        let span=max 1e-9 (extent.High-extent.Low)
        extent.Low-span*fraction,extent.High+span*fraction

/// <summary>Scene-wide style transitions: every mark's opacity, fill, stroke and stroke width ease toward the values its view asks for,
/// so hover emphasis, highlights and colour changes animate without any dashboard code. Marks are matched by their key path; a new
/// mark fades in. Geometry and transforms are untouched, so streaming data never lags.</summary>
[<RequireQualifiedAccess>]
module Transitions =
    /// <summary>The time constant in milliseconds: a change is about 95% complete after 200 ms.</summary>
    let tau=67.
    /// <summary>What each mark showed last frame, by key path.</summary>
    type Memory = Map<string,struct(float*Rgb option*Rgb option*float)>
    let empty:Memory=Map.empty
    let private rgbOf (paint:Paint) =
        let css=Paint.toCss paint
        if css.Length=7 && css[0]='#' then
            let channel i=Convert.ToInt32(css.Substring(i,2),16)
            Some(channel 1,channel 3,channel 5)
        else None
    let private towards dt (current:Rgb) (target:Rgb) =
        let (r1,g1,b1),(r2,g2,b2)=current,target
        let channel a b=int(Math.Round(approach dt tau (float a) (float b)))
        channel r1 r2,channel g1 g2,channel b1 b2
    let private close (a:float) b=abs(a-b)<0.002
    /// <summary>Eases one frame of nodes from the memory over <c>dt</c> milliseconds; returns the shown nodes and the next memory.</summary>
    let ease dt (memory:Memory) (nodes:SceneNode list) =
        let next=Collections.Generic.Dictionary<string,struct(float*Rgb option*Rgb option*float)>()
        let rec walk prefix nodes =
            nodes |> List.map(function
                | Group(spec,children) -> Group(spec,walk (prefix+spec.Key+"/") children)
                | Leaf mark ->
                    let key=prefix+Mark.key mark
                    let style=Mark.style mark
                    let fill=if (Mark.fillDefinition mark).IsSome then None else rgbOf style.Fill
                    let stroke=rgbOf style.Stroke
                    let struct(opacity0,fill0,stroke0,width0)=
                        match memory.TryFind key with
                        | Some previous -> previous
                        | None -> struct(0.,fill,stroke,style.StrokeWidth)
                    let opacity=approach dt tau opacity0 style.Opacity
                    let fill'=match fill0,fill with Some a,Some b -> Some(towards dt a b) | _ -> fill
                    let stroke'=match stroke0,stroke with Some a,Some b -> Some(towards dt a b) | _ -> stroke
                    let width=approach dt tau width0 style.StrokeWidth
                    next[key]<-struct(opacity,fill',stroke',width)
                    let mutable shown=mark
                    if not(close opacity style.Opacity) then shown<-Mark.withOpacity (clamp01 opacity) shown
                    if fill'<>fill then shown<-Mark.withFill (paint fill'.Value) shown
                    if stroke'<>stroke || not(close width style.StrokeWidth) then
                        shown<-Mark.withStroke (match stroke' with Some c -> paint c | None -> style.Stroke) (max 0. width) shown
                    Leaf shown)
        let shown=walk "" nodes
        shown,(next |> Seq.map(fun pair -> pair.Key,pair.Value) |> Map.ofSeq : Memory)

/// <summary>Eased hover levels per item, for geometric hover effects (lifting a slice, growing a bar) that the scene-wide style
/// transitions cannot provide. Keep the map in dashboard state and tick it every frame.</summary>
[<RequireQualifiedAccess>]
module Focus =
    /// <summary>Eases every level toward 1 for the hovered key and 0 for the rest, with the transition time constant; settled
    /// zero levels are dropped.</summary>
    let tick dt (hovered:string option) (levels:Map<string,float>) =
        let keys=match hovered with Some key when not(levels.ContainsKey key) -> levels.Add(key,0.) | _ -> levels
        keys |> Map.toList |> List.choose(fun (key,value) ->
            let next=approach dt Transitions.tau value (if Some key=hovered then 1. else 0.)
            if next<0.002 && Some key<>hovered then None else Some(key,next)) |> Map.ofList
    /// <summary>The eased level of one key in [0,1].</summary>
    let level key (levels:Map<string,float>) = Map.tryFind key levels |> Option.defaultValue 0.
    /// <summary>The largest level: how strongly anything is hovered, for dimming the rest smoothly.</summary>
    let any (levels:Map<string,float>) = if levels.IsEmpty then 0. else levels |> Map.toSeq |> Seq.map snd |> Seq.max

// ---------------------------------------------------------------- transitional fixed-size pages (removed once every page is responsive)

let legacyT key anchor lines = tooltip key 1048. 800. anchor lines
let header (title:string) (summary:string) (help:string) =
    group "header"
        [Leaf(label "title" 24. 40. 24. Theme.text 700 "start" title)
         Leaf(text "summary" 24. 62. 13. Theme.muted summary)
         Leaf(text "help" 24. 790. 11. Theme.faint help)]
let legacy (view:'S -> SceneNode list) = fun (_:Ranvier.D5.Viewport) state -> {Height=800.;Nodes=view state}

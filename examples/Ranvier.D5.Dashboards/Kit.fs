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

/// <summary>Input delivered to a dashboard: a control button, a picked data item, hover, the pointer in content coordinates, or a key.</summary>
[<RequireQualifiedAccess>]
type Input =
    | Click of string
    | Pick of string
    | Hover of string option
    | Pointer of Point option
    | Key of string

/// <summary>A pure dashboard: initial state, a frame step in milliseconds, input handling and a scene view in content coordinates.</summary>
type Model<'S> = { Init:unit -> 'S; Tick:float -> 'S -> 'S; Input:Input -> 'S -> 'S; View:'S -> SceneNode list }

/// <summary>An immutable running dashboard with its state type hidden; every step returns a new value.</summary>
type Live = { Tick:float -> Live; Send:Input -> Live; View:unit -> SceneNode list }

[<RequireQualifiedAccess>]
module Live =
    let rec private wrap (model:Model<'S>) (state:'S) =
        {Tick=(fun dt -> wrap model (model.Tick dt state));Send=(fun input -> wrap model (model.Input input state));View=(fun () -> model.View state)}
    /// <summary>Starts a model from its initial state.</summary>
    let start (model:Model<'S>) = wrap model (model.Init())

/// <summary>A gallery entry: identity, sidebar text and a factory for a fresh running instance.</summary>
type Dashboard = { Id:string; Title:string; Summary:string; Start:unit -> Live }

/// <summary>The content area every dashboard draws into.</summary>
let Width,Height=1048.,800.

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

// ---------------------------------------------------------------- widgets

/// <summary>A dashboard card with an uppercase title; children use card-local coordinates.</summary>
let card key x y w h (title:string) children =
    at key x y ([Leaf(rrect "bg" 0. 0. w h 10. Theme.panel |> Mark.withStroke (paint Theme.edge) 1.)
                 Leaf(label "title" 16. 24. 11. Theme.muted 700 "start" (title.ToUpperInvariant()))]@children)

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

/// <summary>A tooltip box anchored near a point, kept inside the content area.</summary>
let tooltip key (anchor:Point) (lines:string list) =
    let width=12.+(lines |> List.map(fun line -> float line.Length*6.6) |> List.fold max 60.)
    let height=10.+18.*float lines.Length
    let x=if anchor.X+16.+width>Width-8. then anchor.X-16.-width else anchor.X+16.
    let y=clamp 8. (Height-height-8.) (anchor.Y-height/2.)
    at key x y
        (Leaf(rrect "bg" 0. 0. width height 6. Theme.raised |> Mark.withStroke (paint Theme.edge) 1. |> fade 0.96)::
         (lines |> List.mapi(fun index line -> Leaf(label (sprintf "t%d" index) 8. (19.+18.*float index) 12. (if index=0 then Theme.text else Theme.muted) (if index=0 then 700 else 400) "start" line))))

/// <summary>The dashboard header: title, summary and help line.</summary>
let header (title:string) (summary:string) (help:string) =
    group "header"
        [Leaf(label "title" 24. 40. 24. Theme.text 700 "start" title)
         Leaf(text "summary" 24. 62. 13. Theme.muted summary)
         Leaf(text "help" 24. 790. 11. Theme.faint help)]

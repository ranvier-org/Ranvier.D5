namespace Ranvier.D5

open System
open Ranvier

type private MarkSegment = {Layer:int64;From:Mark;Target:Mark;Started:float;Duration:float;Ease:float->float;Exiting:bool}
type KeyedTransitionState = private {Order:string list;Segments:Map<string,MarkSegment list>;Generation:int64}

[<RequireQualifiedAccess>]
module KeyedTransition =
    let private lerp a b t =
        let value=Ranvier.Anime.Utils.lerp a b t
        if Double.IsFinite value then value else Check.lerp a b t
    let private point (a:Point) (b:Point) t=Point.create (lerp a.X b.X t) (lerp a.Y b.Y t)
    let private command t a b =
        match a,b with
        | MoveTo a,MoveTo b -> Some(MoveTo(point a b t))
        | LineTo a,LineTo b -> Some(LineTo(point a b t))
        | CubicTo(a,b,c),CubicTo(d,e,f) -> Some(CubicTo(point a d t,point b e t,point c f t))
        | QuadraticTo(a,b),QuadraticTo(c,d) -> Some(QuadraticTo(point a c t,point b d t))
        | ArcTo(a,b,c,d,e,f),ArcTo(g,h,i,j,k,l) when d=j && e=k -> Some(ArcTo(lerp a g t,lerp b h t,lerp c i t,d,e,point f l t))
        | Close,Close -> Some Close
        | _ -> None
    let private geometry t first last =
        match first,last with
        | TextGeometry(a,text),TextGeometry(b,other) when text=other -> Some(TextGeometry(point a b t,text))
        | PathGeometry a,PathGeometry b ->
            let a,b=Path.commands a,Path.commands b
            if a.Length<>b.Length then None else
            let values=List.map2 (command t) a b
            if List.exists Option.isNone values then None else Some(PathGeometry(Path.ofCommands(List.choose id values)))
        | _ -> None
    let private compatible a b =
        let x,y=Mark.style a,Mark.style b
        x.Fill=y.Fill && x.Stroke=y.Stroke && Mark.textStyle a=Mark.textStyle b &&
        Mark.fillDefinition a=Mark.fillDefinition b && (geometry 0. (Mark.geometry a) (Mark.geometry b)).IsSome
    let private interpolate a b t =
        let x,y=Mark.style a,Mark.style b
        let aa,ab,ac,ad,ae,af=Transform.coefficients(Mark.transform a)
        let ba,bb,bc,bd,be,bf=Transform.coefficients(Mark.transform b)
        let matrix=Transform.create (lerp aa ba t) (lerp ab bb t) (lerp ac bc t) (lerp ad bd t) (lerp ae be t) (lerp af bf t)
        let style={y with Opacity=lerp x.Opacity y.Opacity t;StrokeWidth=lerp x.StrokeWidth y.StrokeWidth t;FontSize=lerp x.FontSize y.FontSize t}
        Mark.reframe (Mark.key b) (geometry t (Mark.geometry a) (Mark.geometry b) |> Option.defaultValue(Mark.geometry b)) style matrix b
    let private sampleSegment now segment =
        let progress=if segment.Duration=0. then 1. else Math.Clamp((now-segment.Started)/segment.Duration,0.,1.)
        if progress=1. then if segment.Exiting then None else Some segment.Target
        elif progress=0. then Some segment.From
        else
            let amount=segment.Ease progress
            Check.finite "ease" amount
            // Clamp scene geometry to keep opacity, font sizes and arc radii valid under overshooting easings.
            Some(interpolate segment.From segment.Target (Math.Clamp(amount,0.,1.)))
    let private validate now marks =
        Check.finite "now" now
        Data.join Mark.key [] marks |> ignore
    let start now marks =
        validate now marks
        {Order=List.map Mark.key marks;Generation=0L
         Segments=marks |> List.map(fun mark -> Mark.key mark,[{Layer=0L;From=mark;Target=mark;Started=now;Duration=0.;Ease=id;Exiting=false}]) |> Map.ofList}
    /// <summary>Samples into scoped groups, preserving domain keys while crossfade layers have private identities.</summary>
    let sample now state =
        Check.finite "now" now
        state.Order |> List.choose(fun key ->
            let children=state.Segments[key] |> List.choose(fun segment ->
                sampleSegment now segment |> Option.map(fun mark ->
                    Mark.reframe (string segment.Layer) (Mark.geometry mark) (Mark.style mark) (Mark.transform mark) mark |> Leaf))
            if children.IsEmpty then None else Some(Group(GroupSpec.create key,children)))
    /// <summary>Retargets from current samples. At most eight crossfade layers per key survive repeated topology changes; the faintest old layers are discarded first.</summary>
    let retarget now duration ease reduced marks state =
        validate now marks
        Check.finite "duration" duration
        if duration<0. then invalidArg "duration" "Duration must be nonnegative."
        if reduced || duration=0. then start now marks else
        let generation=state.Generation+1L
        let targets=marks |> List.map(fun mark -> Mark.key mark,mark) |> Map.ofList
        let order=(List.map Mark.key marks)@(state.Order |> List.filter(fun key -> not(Map.containsKey key targets)))
        let segments=order |> List.choose(fun key ->
            let current=Map.tryFind key state.Segments |> Option.defaultValue [] |> List.choose(fun s -> sampleSegment now s |> Option.map(fun m -> s.Layer,m))
            let segment layer source target exiting={Layer=layer;From=source;Target=target;Started=now;Duration=duration;Ease=ease;Exiting=exiting}
            let next=
                match Map.tryFind key targets,current with
                | Some target,[(layer,source)] when compatible source target -> [segment layer source target false]
                | destination,_ ->
                    let outgoing=current |> List.sortByDescending(fun (_,mark) -> (Mark.style mark).Opacity) |> List.truncate (if destination.IsSome then 7 else 8)
                                         |> List.map(fun (layer,mark) -> segment layer mark (Mark.withOpacity 0. mark) true)
                    match destination with
                    | Some target -> outgoing@[segment generation (Mark.withOpacity 0. target) target false]
                    | None -> outgoing
            if next.IsEmpty then None else Some(key,next)) |> Map.ofList
        {Order=order |> List.filter(fun key -> Map.containsKey key segments);Segments=segments;Generation=generation}

    /// <summary>Uses the host's existing frame clock and Anime easing; no timer or completion callback is created. Read reduced motion from the toolkit's motion-policy signal.</summary>
    let animate clock targets duration ease reduced =
        let state=Api.createMemo(fun previous ->
            let now,marks,motion=clock(),targets(),reduced()
            match previous with
            | ValueNone -> marks,motion,start now marks
            | ValueSome(old,wasReduced,state) ->
                marks,motion,(if old<>marks || motion<>wasReduced then retarget now duration ease motion marks state else state))
        Api.createMemo(fun _ -> let _,_,current=state.Value in sample (clock()) current)

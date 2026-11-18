namespace Ranvier.D5

open System
open Ranvier

type private MarkSegment = {Layer:int64;LayerKey:string;From:Mark;Target:Mark;Started:float;Duration:float;Ease:float->float;Exiting:bool}
/// <summary>Opaque, immutable per-key mark transition state, including crossfade layers for exiting and incompatible marks.</summary>
type KeyedTransitionState = private {
    Entries:struct(string*GroupSpec*MarkSegment list) array
    ByKey:Map<string,MarkSegment list>
    Settled:SceneNode list voption
    Generation:int64
}

/// <summary>Key-matched mark transitions that interpolate compatible marks and crossfade the rest;
/// invalid input throws <c>ArgumentException</c>.</summary>
[<RequireQualifiedAccess>]
module KeyedTransition =
    let private lerp a b t =
        let value=Ranvier.Anime.Utils.lerp a b t
        if Double.IsFinite value then value else Check.lerp a b t
    let private point (a:Point) (b:Point) t=Point.create (lerp a.X b.X t) (lerp a.Y b.Y t)
    let private command t a b =
        match a,b with
        | MoveTo a,MoveTo b -> ValueSome(MoveTo(point a b t))
        | LineTo a,LineTo b -> ValueSome(LineTo(point a b t))
        | CubicTo(a,b,c),CubicTo(d,e,f) -> ValueSome(CubicTo(point a d t,point b e t,point c f t))
        | QuadraticTo(a,b),QuadraticTo(c,d) -> ValueSome(QuadraticTo(point a c t,point b d t))
        | ArcTo(a,b,c,d,e,f),ArcTo(g,h,i,j,k,l) when d=j && e=k -> ValueSome(ArcTo(lerp a g t,lerp b h t,lerp c i t,d,e,point f l t))
        | Close,Close -> ValueSome Close
        | _ -> ValueNone
    let private geometryCompatible first last =
        if Object.ReferenceEquals(first, last) then true
        else
            match first,last with
            | TextGeometry(_,text),TextGeometry(_,other) -> text=other
            | PathGeometry a,PathGeometry b ->
                if Object.ReferenceEquals(a, b) then true
                else
                    let mutable ca,cb=Path.commands a,Path.commands b
                    let mutable ok=true
                    while ok && not ca.IsEmpty && not cb.IsEmpty do
                        let sameKind =
                            match ca.Head,cb.Head with
                            | MoveTo _,MoveTo _ | LineTo _,LineTo _ | CubicTo _,CubicTo _
                            | QuadraticTo _,QuadraticTo _ | Close,Close -> true
                            | ArcTo(_,_,_,d,e,_),ArcTo(_,_,_,j,k,_) -> d=j && e=k
                            | _ -> false
                        if not sameKind then ok<-false
                        ca<-ca.Tail
                        cb<-cb.Tail
                    ok && ca.IsEmpty && cb.IsEmpty
            | _ -> false
    let private geometry t first last =
        if Object.ReferenceEquals(first, last) then Some first
        else
            match first,last with
            | TextGeometry(a,text),TextGeometry(b,other) when text=other ->
                if a=b then Some last else Some(TextGeometry(point a b t,text))
            | PathGeometry a,PathGeometry b ->
                if Object.ReferenceEquals(a, b) || a=b then Some last
                else
                    let mutable ca,cb=Path.commands a,Path.commands b
                    let mutable rev=[]
                    let mutable ok=true
                    while ok && not ca.IsEmpty && not cb.IsEmpty do
                        match command t ca.Head cb.Head with
                        | ValueSome cmd -> rev<-cmd::rev
                        | ValueNone -> ok<-false
                        ca<-ca.Tail
                        cb<-cb.Tail
                    if ok && ca.IsEmpty && cb.IsEmpty then Some(PathGeometry(Path.ofCommands(List.rev rev)))
                    else None
            | _ -> None
    let private compatible a b =
        let x,y=Mark.style a,Mark.style b
        x.Fill=y.Fill && x.Stroke=y.Stroke && Mark.textStyle a=Mark.textStyle b &&
        Mark.fillDefinition a=Mark.fillDefinition b && geometryCompatible (Mark.geometry a) (Mark.geometry b)
    let private interpolate keyOverride a b t =
        let x,y=Mark.style a,Mark.style b
        let ta,tb=Mark.transform a,Mark.transform b
        let matrix =
            if Object.ReferenceEquals(ta, tb) || ta=tb then tb
            else
                let aa,ab,ac,ad,ae,af=Transform.coefficients ta
                let ba,bb,bc,bd,be,bf=Transform.coefficients tb
                Transform.create (lerp aa ba t) (lerp ab bb t) (lerp ac bc t) (lerp ad bd t) (lerp ae be t) (lerp af bf t)
        let style =
            if Object.ReferenceEquals(x, y) || (x.Opacity=y.Opacity && x.StrokeWidth=y.StrokeWidth && x.FontSize=y.FontSize) then y
            else {y with Opacity=lerp x.Opacity y.Opacity t;StrokeWidth=lerp x.StrokeWidth y.StrokeWidth t;FontSize=lerp x.FontSize y.FontSize t}
        let targetKey=match keyOverride with ValueSome k -> k | ValueNone -> Mark.key b
        Mark.reframe targetKey (geometry t (Mark.geometry a) (Mark.geometry b) |> Option.defaultValue(Mark.geometry b)) style matrix b
    let private sampleSegment rawKey now segment =
        let progress=if segment.Duration=0. then 1. else Math.Clamp((now-segment.Started)/segment.Duration,0.,1.)
        if progress=1. then
            if segment.Exiting then ValueNone
            else
                let m=segment.Target
                match rawKey with
                | ValueSome k -> ValueSome(Mark.reframe k (Mark.geometry m) (Mark.style m) (Mark.transform m) m)
                | ValueNone -> ValueSome m
        elif progress=0. then
            let m=segment.From
            match rawKey with
            | ValueSome k -> ValueSome(Mark.reframe k (Mark.geometry m) (Mark.style m) (Mark.transform m) m)
            | ValueNone -> ValueSome m
        else
            let amount=segment.Ease progress
            Check.finite "ease" amount
            // Clamp scene geometry to keep opacity, font sizes and arc radii valid under overshooting easings.
            ValueSome(interpolate rawKey segment.From segment.Target (Math.Clamp(amount,0.,1.)))
    let private validate now marks =
        Check.finite "now" now
        Data.join Mark.key [] marks |> ignore
    /// <summary>Creates a settled state holding <c>marks</c> in order with no animation pending. Throws <c>ArgumentException</c> for a non-finite time or duplicate mark keys.</summary>
    let start now marks =
        validate now marks
        let arr=marks |> List.toArray
        let entries=Array.zeroCreate<struct(string*GroupSpec*MarkSegment list)> arr.Length
        let mutable settledRev=[]
        let mutable mapPairs=[]
        for i=0 to arr.Length-1 do
            let mark=arr[i]
            let key=Mark.key mark
            let group=GroupSpec.create key
            let seg={Layer=0L;LayerKey="0";From=mark;Target=mark;Started=now;Duration=0.;Ease=id;Exiting=false}
            let segs=[seg]
            entries[i]<-struct(key,group,segs)
            mapPairs<-(key,segs)::mapPairs
            let leaf=Leaf(Mark.reframe "0" (Mark.geometry mark) (Mark.style mark) (Mark.transform mark) mark)
            settledRev<-SceneNode.Group(group,[leaf])::settledRev
        {Entries=entries;ByKey=Map.ofList mapPairs;Settled=ValueSome(List.rev settledRev);Generation=0L}
    /// <summary>Samples into scoped groups, preserving domain keys while crossfade layers have private identities.</summary>
    let sample now state =
        Check.finite "now" now
        // Proven in TransitionWorker.v (settled_keyed_transition_fast_path_eq)
        match state.Settled with
        | ValueSome nodes -> nodes
        | ValueNone ->
            let entries=state.Entries
            let mutable out=[]
            for i=entries.Length-1 downto 0 do
                let struct(_,group,segs)=entries[i]
                match segs with
                | [single] ->
                    match sampleSegment (ValueSome single.LayerKey) now single with
                    | ValueSome mark -> out<-SceneNode.Group(group,[Leaf mark])::out
                    | ValueNone -> ()
                | _ ->
                    let mutable childrenRev=[]
                    for segment in segs do
                        match sampleSegment (ValueSome segment.LayerKey) now segment with
                        | ValueSome mark -> childrenRev<-Leaf mark::childrenRev
                        | ValueNone -> ()
                    if not childrenRev.IsEmpty then
                        out<-SceneNode.Group(group,List.rev childrenRev)::out
            out
    /// <summary>Retargets from current samples. At most eight crossfade layers per key survive repeated topology changes; the faintest old layers are discarded first.</summary>
    let retarget now duration ease reduced marks state =
        validate now marks
        Check.finite "duration" duration
        if duration<0. then invalidArg "duration" "Duration must be nonnegative."
        if reduced || duration=0. then start now marks else
        let generation=state.Generation+1L
        let genKey=string generation
        let targets=marks |> List.map(fun mark -> Mark.key mark,mark) |> Map.ofList
        let prevOrder=[for i in 0..state.Entries.Length-1 -> let struct(k,_,_)=state.Entries[i] in k]
        let order=(List.map Mark.key marks)@(prevOrder |> List.filter(fun key -> not(Map.containsKey key targets)))
        let built=ResizeArray<struct(string*GroupSpec*MarkSegment list)>()
        let mutable mapPairs=[]
        for key in order do
            let current =
                match Map.tryFind key state.ByKey with
                | None -> []
                | Some segs ->
                    segs |> List.choose(fun s ->
                        match sampleSegment ValueNone now s with
                        | ValueSome m -> Some(s.Layer,s.LayerKey,m)
                        | ValueNone -> None)
            let mkSegment layer layerKey source target exiting={Layer=layer;LayerKey=layerKey;From=source;Target=target;Started=now;Duration=duration;Ease=ease;Exiting=exiting}
            let next=
                match Map.tryFind key targets,current with
                | Some target,[(layer,layerKey,source)] when compatible source target -> [mkSegment layer layerKey source target false]
                | destination,_ ->
                    let outgoing=current |> List.sortByDescending(fun (_,_,mark) -> (Mark.style mark).Opacity) |> List.truncate (if destination.IsSome then 7 else 8)
                                         |> List.map(fun (layer,layerKey,mark) -> mkSegment layer layerKey mark (Mark.withOpacity 0. mark) true)
                    match destination with
                    | Some target -> outgoing@[mkSegment generation genKey (Mark.withOpacity 0. target) target false]
                    | None -> outgoing
            if not next.IsEmpty then
                built.Add(struct(key,GroupSpec.create key,next))
                mapPairs<-(key,next)::mapPairs
        {Entries=built.ToArray();ByKey=Map.ofList mapPairs;Settled=ValueNone;Generation=generation}

    /// <summary>Uses the host's existing frame clock and Anime easing; no timer or completion callback is created. Read reduced motion from the toolkit's motion-policy signal.</summary>
    let animate clock targets duration ease reduced =
        let state=Api.createMemo(fun previous ->
            let now,marks,motion=clock(),targets(),reduced()
            match previous with
            | ValueNone -> marks,motion,start now marks
            | ValueSome(old,wasReduced,state) ->
                marks,motion,(if old<>marks || motion<>wasReduced then retarget now duration ease motion marks state else state))
        Api.createMemo(fun _ -> let _,_,current=state.Value in sample (clock()) current)

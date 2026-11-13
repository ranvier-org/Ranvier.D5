namespace Ranvier.D5.Charts

open System
open Ranvier.D5

/// <summary>Points joined in Order (for example time); missing coordinates break the line.</summary>
type ConnectedScatterSpec<'T> = { Frame:ChartFrame; Key:'T->string; Order:'T->float; X:Encoding<'T,float option>; Y:Encoding<'T,float option>; Color:Paint; XDomain:DomainPolicy; YDomain:DomainPolicy }
/// <summary>One line per item from a Before value on the left axis to an After value on the right.</summary>
type SlopeSpec<'T> = { Frame:ChartFrame; Key:'T->string; Before:Encoding<'T,float option>; After:Encoding<'T,float option>; Color:'T->Paint; Domain:DomainPolicy }
/// <summary>Ranks of each series within each period, in period order; ties break by ordinal series ID.</summary>
type BumpSpec<'T> = { Frame:ChartFrame; Key:'T->string; Series:'T->string; Period:'T->string; Value:'T->float; Descending:bool; Color:string->Paint }
/// <summary>Each series in its own row, folded into Bands layers of equal height; negative values mirror in NegativeColor.</summary>
type HorizonSpec<'T> = { Frame:ChartFrame; Key:'T->string; Series:'T->string; X:Encoding<'T,float>; Y:Encoding<'T,float option>; Bands:int; PositiveColor:int->Paint; NegativeColor:int->Paint; Extent:float option }
/// <summary>Circles of one radius placed along X and dodged vertically from the centre line without overlap.</summary>
type BeeswarmSpec<'T> = { Frame:ChartFrame; Key:'T->string; X:Encoding<'T,float option>; Radius:float; Color:'T->Paint; XDomain:DomainPolicy }
/// <summary>Node ordering for adjacency matrices.</summary>
[<RequireQualifiedAccess>]
type MatrixOrder = Input | Label | Degree
/// <summary>A network as a matrix of cells; undirected links fill both cells.</summary>
type AdjacencyMatrixSpec = { Frame:ChartFrame; Nodes:(string*string) list; Links:(string*string*string*float) list; Directed:bool; Order:MatrixOrder; Color:float->Paint }

module internal AdvancedCharts =
    let private dot id label color (center:Point) radius dataIds =
        let mark=Shape.symbol Symbol.Circle (Math.PI*radius*radius) |> Mark.path id |> Mark.withTransform(Transform.translate center.X center.Y) |> Mark.withFill color |> Mark.withLabel(Some label)
        mark,Recipe.semantic id dataIds label,Recipe.hit id dataIds (HitPoint(center,radius))

    let connectedScatter (spec:ConnectedScatterSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let ordered=data |> List.sortBy(fun datum -> spec.Order datum,spec.Key datum)
            if ordered |> List.exists(fun datum -> not(Double.IsFinite(spec.Order datum))) then invalidArg "data" "Order values must be finite."
            let x=Scale.linear (Recipe.domain spec.XDomain (ordered |> List.map spec.X.Read)) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
            let y=Scale.linear (Recipe.domain spec.YDomain (ordered |> List.map spec.Y.Read)) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let position datum=match spec.X.Read datum,spec.Y.Read datum with Some a,Some b -> Some(Point.create (Scale.map x a) (Scale.map y b)) | _ -> None
            let path=Curves.line CurveSpec.Linear (ordered |> List.map position) |> Mark.path (Recipe.key ["trail"]) |> Mark.withFill Paint.none |> Mark.withStroke spec.Color 1.5
            let dots=ordered |> List.choose(fun datum ->
                position datum |> Option.map(fun center ->
                    let id=spec.Key datum
                    let label=String.Concat [|id;": ";spec.X.Label;" ";Recipe.number (spec.X.Read datum).Value;", ";spec.Y.Label;" ";Recipe.number (spec.Y.Read datum).Value|]
                    dot (Recipe.key ["point";id]) label spec.Color center 3.5 [id]))
            let marks,semantics,hits=List.unzip3 dots
            Recipe.output spec.Frame bounds (path::marks) semantics hits (Recipe.axes bounds x y))

    let slope (spec:SlopeSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let y=Scale.linear (Recipe.domain spec.Domain (data |> List.collect(fun datum -> [spec.Before.Read datum;spec.After.Read datum]))) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let left,right=Bounds.x bounds+Bounds.width bounds*0.2,Bounds.x bounds+Bounds.width bounds*0.8
            let entries=data |> List.choose(fun datum ->
                match spec.Before.Read datum,spec.After.Read datum with
                | Some before,Some after ->
                    let id=spec.Key datum
                    let a,b=Point.create left (Scale.map y before),Point.create right (Scale.map y after)
                    let label=sprintf "%s: %s %s → %s %s" id spec.Before.Label (Recipe.number before) spec.After.Label (Recipe.number after)
                    let markId=Recipe.key ["slope";id]
                    let line=Path.ofCommands [MoveTo a;LineTo b] |> Mark.path markId |> Mark.withFill Paint.none |> Mark.withStroke (spec.Color datum) 2. |> Mark.withLabel(Some label)
                    let reach=4.
                    let length=Double.Hypot(b.X-a.X,b.Y-a.Y)
                    let nx,ny= -(b.Y-a.Y)/length*reach,(b.X-a.X)/length*reach
                    let band={Outer=[Point.create (a.X+nx) (a.Y+ny);Point.create (b.X+nx) (b.Y+ny);Point.create (b.X-nx) (b.Y-ny);Point.create (a.X-nx) (a.Y-ny)];Holes=[]}
                    Some(line,Recipe.semantic markId [id] label,Recipe.hit markId [id] (HitRegion([band],[],Point.create ((a.X+b.X)/2.) ((a.Y+b.Y)/2.))))
                | _ -> None)
            let marks,semantics,hits=List.unzip3 entries
            let axis key x label=
                [Path.ofCommands [MoveTo(Point.create x (Bounds.y bounds));LineTo(Point.create x (Bounds.y bounds+Bounds.height bounds))] |> Mark.path (Recipe.key [key;"line"]) |> Mark.withFill Paint.none |> Mark.withStroke (Paint.rgb 160 160 160) 1.
                 Mark.text (Recipe.key [key;"label"]) (Point.create x (Bounds.y bounds+Bounds.height bounds+18.)) label |> Mark.withFontSize 12.]
            Recipe.output spec.Frame bounds (axis "before" left spec.Before.Label@axis "after" right spec.After.Label@marks) semantics hits [])

    /// Ranks every series within a period; ties break by ordinal series ID so ranks are deterministic.
    let ranks descending (rows:(string*string*float) list) =
        rows |> List.groupBy(fun (_,period,_) -> period) |> List.collect(fun (period,members) ->
            members
            |> List.sortWith(fun (a,_,va) (b,_,vb) ->
                let byValue=if descending then compare vb va else compare va vb
                if byValue<>0 then byValue else String.CompareOrdinal(a,b))
            |> List.mapi(fun index (series,_,_) -> (series,period),index+1))
        |> Map.ofList

    let bump (spec:BumpSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            if data |> List.exists(fun datum -> not(Double.IsFinite(spec.Value datum))) then invalidArg "data" "Bump values must be finite."
            if data |> List.countBy(fun datum -> spec.Series datum,spec.Period datum) |> List.exists(fun (_,count) -> count>1) then invalidArg "data" "Each series has at most one value per period."
            let periods=data |> List.map spec.Period |> List.distinct
            let rank=ranks spec.Descending (data |> List.map(fun datum -> spec.Series datum,spec.Period datum,spec.Value datum))
            let largest=rank |> Map.values |> Seq.fold max 1
            let x=Scale.point periods (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds) 0.5
            // Inset by the dot radius so first and last ranks are not half clipped by the plot.
            let inset=min 8. (Bounds.height bounds/4.)
            let y=Scale.linear (1.,max 2. (float largest)) (Bounds.y bounds+inset,Bounds.y bounds+Bounds.height bounds-inset)
            let position series period=Map.tryFind (series,period) rank |> Option.map(fun value -> Point.create (Scale.bandPosition x period |> Option.get) (Scale.map y (float value)))
            let series=data |> List.map spec.Series |> List.distinct
            let lines=series |> List.map(fun name ->
                Curves.line CurveSpec.BumpX (periods |> List.map(position name)) |> Mark.path (Recipe.key ["series";name]) |> Mark.withFill Paint.none |> Mark.withStroke (spec.Color name) 2.5)
            let dots=data |> List.map(fun datum ->
                let id=spec.Key datum
                let value=rank[spec.Series datum,spec.Period datum]
                let label=String.Concat [|spec.Series datum;", ";spec.Period datum;": rank ";string value;" (";Recipe.number(spec.Value datum);")"|]
                dot (Recipe.key ["rank";id]) label (spec.Color(spec.Series datum)) (position (spec.Series datum) (spec.Period datum)).Value 5. [id])
            let marks,semantics,hits=List.unzip3 dots
            let labels=periods |> List.map(fun period -> Mark.text (Recipe.key ["period";period]) (Point.create (Scale.bandPosition x period |> Option.get) (Bounds.y bounds+Bounds.height bounds+18.)) period |> Mark.withFontSize 12.)
            Recipe.output spec.Frame bounds (lines@marks@labels) semantics hits [])

    let horizon (spec:HorizonSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            if spec.Bands<1 || spec.Bands>8 then invalidArg "spec" "Horizon charts use 1 to 8 bands."
            if data |> List.exists(fun datum -> not(Double.IsFinite(spec.X.Read datum)) || spec.Y.Read datum |> Option.exists(Double.IsFinite >> not)) then invalidArg "data" "Horizon values must be finite."
            let series=data |> List.map spec.Series |> List.distinct
            let extent=
                match spec.Extent with
                | Some value when Double.IsFinite value && value>0. -> value
                | Some _ -> invalidArg "spec" "Horizon extent must be positive and finite."
                | None -> data |> List.choose spec.Y.Read |> List.fold(fun state value -> max state (abs value)) 0. |> fun value -> if value=0. then 1. else value
            let step=extent/float spec.Bands
            let rows=Scale.band series (Bounds.y bounds,Bounds.y bounds+Bounds.height bounds) 0.1 0.
            let x=Scale.linear (Recipe.domain Auto (data |> List.map(spec.X.Read >> Some))) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
            let height=Scale.bandwidth rows
            let entries=series |> List.map(fun name ->
                let top=Scale.bandPosition rows name |> Option.get
                let points=data |> List.filter(fun datum -> spec.Series datum=name) |> List.sortBy spec.X.Read
                let layer sign band =
                    // Each band shows the part of |value| between band*step and (band+1)*step, rescaled to the row height.
                    points |> List.map(fun datum ->
                        spec.Y.Read datum |> Option.map(fun value ->
                            let part=Math.Clamp((sign*value)-float band*step,0.,step)/step
                            let px=Scale.map x (spec.X.Read datum)
                            Point.create px (top+height),Point.create px (top+height-part*height)))
                let areas=
                    [for band in 0..spec.Bands-1 do
                        yield Curves.area CurveSpec.Linear (layer 1. band) |> Mark.path (Recipe.key ["horizon";name;"positive";string band]) |> Mark.withFill(spec.PositiveColor band)
                        yield Curves.area CurveSpec.Linear (layer -1. band) |> Mark.path (Recipe.key ["horizon";name;"negative";string band]) |> Mark.withFill(spec.NegativeColor band)]
                let row=Bounds.create (Bounds.x bounds) top (Bounds.width bounds) height
                let ids=points |> List.map spec.Key
                let label=sprintf "%s: %d values, %d bands of %s" name ids.Length spec.Bands (Recipe.number step)
                let markId=Recipe.key ["row";name]
                // An unfilled row outline carries the row's label and is the mark that semantics and picking name.
                let outline=Recipe.rectangle markId label Paint.none row
                areas@[outline],Recipe.semantic markId ids label,Recipe.hit markId ids (HitBounds row))
            let areas,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds (List.concat areas) semantics hits [])

    /// Greedy dodge in X order: each circle takes the smallest |offset| that clears every placed neighbour within one diameter.
    let swarm radius (positions:(string*float) list) =
        let placed=ResizeArray<struct(float*float)>()
        let diameter=2.*radius
        // Circles arrive in x order, so only a sliding window of the last diameter can collide.
        let mutable window=0
        positions |> List.sortBy(fun (id,x) -> x,id) |> List.map(fun (id,x) ->
            while window<placed.Count && (let struct(px,_)=placed[window] in x-px>=diameter) do window<-window+1
            let neighbours=[for index in window..placed.Count-1 -> placed[index]]
            let clear y=neighbours |> List.forall(fun (struct(px,py)) -> Double.Hypot(px-x,py-y)>=diameter*(1.-1e-12))
            let candidates=
                0.::(neighbours |> List.collect(fun (struct(px,py)) ->
                    let dy=sqrt(max 0. (diameter*diameter-(px-x)*(px-x)))
                    [py+dy;py-dy]))
            let y=candidates |> List.filter clear |> List.sortBy(fun y -> abs y,y) |> List.head
            placed.Add(struct(x,y))
            id,x,y)

    let beeswarm (spec:BeeswarmSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            if not(Double.IsFinite spec.Radius && spec.Radius>0.) then invalidArg "spec" "Beeswarm radius must be positive and finite."
            if List.length data>20000 then invalidArg "data" "Beeswarm is limited to 20,000 marks."
            let x=Scale.linear (Recipe.domain spec.XDomain (data |> List.map spec.X.Read)) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
            let byId=data |> List.map(fun datum -> spec.Key datum,datum) |> Map.ofList
            let centre=Bounds.y bounds+Bounds.height bounds/2.
            let placed=swarm spec.Radius (data |> List.choose(fun datum -> spec.X.Read datum |> Option.map(fun value -> spec.Key datum,Scale.map x value)))
            let marks,semantics,hits=
                placed |> List.map(fun (id,px,offset) ->
                    let datum=byId[id]
                    dot (Recipe.key ["bee";id]) (String.Concat(id,": ",spec.X.Label," ",Recipe.number (spec.X.Read datum).Value)) (spec.Color datum) (Point.create px (centre+offset)) spec.Radius [id])
                |> List.unzip3
            Recipe.output spec.Frame bounds marks semantics hits (Recipe.axes bounds x (Scale.linear (0.,1.) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)) |> List.filter(fun mark -> (Mark.key mark).StartsWith "x/")))

    let adjacencyMatrix (spec:AdjacencyMatrixSpec) =
        let errors=
            ChartLayout.validateIds fst spec.Nodes
            @ ChartLayout.validateIds (fun (id,_,_,_) -> id) spec.Links
            @ (let known=spec.Nodes |> List.map fst |> Set.ofList
               spec.Links |> List.collect(fun (id,source,target,_) ->
                   [if not(known.Contains source) then MissingEndpoint(id+": "+source)
                    if not(known.Contains target) then MissingEndpoint(id+": "+target)]))
            @ (spec.Links |> List.choose(fun (id,_,_,weight) -> if Double.IsFinite weight && weight>=0. then None else Some(InvalidObservation(id+": weight must be finite and nonnegative."))))
        if not errors.IsEmpty then Error errors
        else
            Recipe.run spec.Frame fst spec.Nodes (fun bounds ->
                let degree=spec.Links |> List.collect(fun (_,source,target,_) -> [source;target]) |> List.countBy id |> Map.ofList
                let order=
                    match spec.Order with
                    | MatrixOrder.Input -> spec.Nodes
                    | MatrixOrder.Label -> spec.Nodes |> List.sortWith(fun (a,la) (b,lb) -> let c=String.CompareOrdinal(la,lb) in if c<>0 then c else String.CompareOrdinal(a,b))
                    | MatrixOrder.Degree -> spec.Nodes |> List.sortWith(fun (a,_) (b,_) -> let c=compare (Map.tryFind b degree |> Option.defaultValue 0) (Map.tryFind a degree |> Option.defaultValue 0) in if c<>0 then c else String.CompareOrdinal(a,b))
                let ids=order |> List.map fst
                let size=min (Bounds.width bounds) (Bounds.height bounds)
                let columns=Scale.band ids (Bounds.x bounds,Bounds.x bounds+size) 0.05 0.
                let rows=Scale.band ids (Bounds.y bounds,Bounds.y bounds+size) 0.05 0.
                let largest=spec.Links |> List.fold(fun state (_,_,_,weight) -> max state weight) 0.
                let labels=Map.ofList spec.Nodes
                let cells=spec.Links |> List.collect(fun (id,source,target,weight) ->
                    let cell suffix row column=
                        let box=Bounds.create (Scale.bandPosition columns column |> Option.get) (Scale.bandPosition rows row |> Option.get) (Scale.bandwidth columns) (Scale.bandwidth rows)
                        let label=String.Concat(labels[row]," → ",labels[column],": "+Recipe.number weight)
                        let markId=Recipe.key ["cell";id;suffix]
                        Recipe.rectangle markId label (spec.Color(if largest=0. then 0. else weight/largest)) box,Recipe.semantic markId [id] label,Recipe.hit markId [id] (HitBounds box)
                    if spec.Directed || source=target then [cell "forward" source target]
                    else [cell "forward" source target;cell "reverse" target source])
                let marks,semantics,hits=List.unzip3 cells
                Recipe.output spec.Frame bounds marks semantics hits [])

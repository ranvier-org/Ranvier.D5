namespace Ranvier.D5.Charts

open System
open Ranvier.D5

module internal Cartesian =
    let bar (spec:BarSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.choose (fun d -> spec.Value.Read d |> Option.map (fun value -> spec.Key d,spec.Category.Read d,spec.Series d,value))
            let categories=rows |> List.map (fun (_,category,_,_) -> category) |> List.distinct
            let series=rows |> List.map (fun (_,_,series,_) -> series) |> List.distinct
            if rows |> List.exists (fun (_,_,_,v) -> not(Double.IsFinite v)) then invalidArg "data" "Non-finite value."
            let duplicates=rows |> List.countBy (fun (_,c,s,_) -> c,(if spec.Mode=BarMode.Single then "" else s)) |> List.exists (fun (_,n) -> n>1)
            if duplicates then invalidArg "data" "Duplicate category/series cell."
            let stacked=spec.Mode=BarMode.Stacked || spec.Mode=BarMode.Normalized
            let cells=rows |> List.map (fun (id,c,s,v) -> (c,s),(id,v)) |> Map.ofList
            let intervals =
                if stacked then
                    let layers=series |> List.map (fun s -> {Series.Key=s;Values=categories |> List.map (fun c -> Map.tryFind(c,s) cells |> Option.map snd |> Option.defaultValue 0.)})
                    Stack.layout {Order=StackOrder.Input;Offset=(if spec.Mode=BarMode.Normalized then StackOffset.Expand else StackOffset.Diverging)} layers |> Recipe.unwrap
                    |> List.collect (fun layer -> List.zip categories layer.Values |> List.map (fun (c,v) -> (c,layer.Key),v)) |> Map.ofList
                else rows |> List.map (fun (_,c,s,v) -> (c,s),(min 0. v,max 0. v)) |> Map.ofList
            let values=intervals |> Map.toList |> List.collect (fun (_, (a,b)) -> [Some a;Some b])
            let vertical=spec.Orientation=Orientation.Vertical
            let categoryRange=if vertical then Bounds.x bounds,Bounds.x bounds+Bounds.width bounds else Bounds.y bounds,Bounds.y bounds+Bounds.height bounds
            let valueRange=if vertical then Bounds.y bounds+Bounds.height bounds,Bounds.y bounds else Bounds.x bounds,Bounds.x bounds+Bounds.width bounds
            let bands=Scale.band categories categoryRange 0.15 0.1
            let sub=Scale.band series (0.,Scale.bandwidth bands) 0.1 0.
            let scale=Scale.linear (Recipe.domain spec.Domain values) valueRange
            let marks,semantics,hits =
                rows |> List.map (fun (id,c,s,value) ->
                    let lower,upper=intervals[c,s]
                    let start=Scale.bandPosition bands c |> Option.get
                    let start=if spec.Mode=BarMode.Grouped then start+(Scale.bandPosition sub s |> Option.get) else start
                    let width=if spec.Mode=BarMode.Grouped then Scale.bandwidth sub else Scale.bandwidth bands
                    let a,b=Scale.map scale lower,Scale.map scale upper
                    let box=if vertical then Bounds.create start (min a b) width (abs(b-a)) else Bounds.create (min a b) start (abs(b-a)) width
                    let markId=Recipe.key ["bar";id]
                    let description=c+", "+s+": "+Recipe.number value
                    Recipe.rectangle markId description (spec.Color s) box,Recipe.semantic markId [id] description,Recipe.hit markId [id] (HitBounds box)) |> List.unzip3
            let ticks=categories |> List.map (fun c -> c,(Scale.bandPosition bands c |> Option.get)+Scale.bandwidth bands/2.,c)
            let axes=Axis.values "category" (if vertical then AxisOrientation.Bottom else AxisOrientation.Left) (if vertical then Bounds.y bounds+Bounds.height bounds else Bounds.x bounds) categoryRange ticks
            let valueTicks=Scale.ticks 5 scale |> List.map (fun v -> v.ToString("R",Globalization.CultureInfo.InvariantCulture),Scale.map scale v,Recipe.number v)
            let valueAxis=Axis.values "value" (if vertical then AxisOrientation.Left else AxisOrientation.Bottom) (if vertical then Bounds.x bounds else Bounds.y bounds+Bounds.height bounds) valueRange valueTicks
            Recipe.output spec.Frame bounds marks semantics hits (axes@valueAxis))

    let line (spec:LineSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.map (fun d -> spec.Key d,spec.Series d,spec.X.Read d,spec.Y.Read d)
            let x=Scale.linear (Recipe.domain spec.XDomain (rows |> List.map (fun (_,_,x,_) -> x))) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
            let groups=rows |> List.groupBy (fun (_,s,_,_) -> s)
            let stacked =
                match spec.Kind with
                | LineKind.StackedArea config ->
                    let xs=rows |> List.choose (fun (_,_,x,_) -> x) |> List.distinct |> List.sort
                    let series=groups |> List.map (fun (s,rows) ->
                        let entries=rows |> List.choose (fun (_,_,x,y) -> match x,y with Some x,Some y -> Some(x,y) | _ -> None)
                        if entries |> List.countBy fst |> List.exists (fun (_,n) -> n>1) then invalidArg "data" "Duplicate X in stacked series."
                        let values=Map.ofList entries
                        {Series.Key=s;Values=xs |> List.map (fun x -> Map.tryFind x values |> Option.defaultValue 0.)})
                    let layers=Stack.layout config series |> Recipe.unwrap
                    Some(xs,layers)
                | _ -> None
            let ys=
                match stacked,spec.Kind with
                | Some(_,layers),_ -> layers |> List.collect (fun l -> l.Values |> List.collect (fun (a,b) -> [Some a;Some b]))
                | _,LineKind.Area baseline -> Some baseline::(rows |> List.map (fun (_,_,_,y) -> y))
                | _ -> rows |> List.map (fun (_,_,_,y) -> y)
            let y=Scale.linear (Recipe.domain spec.YDomain ys) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let marks,semantics=groups |> List.map (fun (series,rows) ->
                let markId=Recipe.key ["line";series]
                let point (id,_,a,b)=match a,b with Some a,Some b -> Some(id,Point.create (Scale.map x a) (Scale.map y b)) | _ -> None
                let points=rows |> List.map (point >> Option.map snd)
                let geometry=
                    match spec.Kind,stacked with
                    | LineKind.Line,_ -> Curves.line spec.Curve points
                    | LineKind.Area baseline,_ -> points |> List.map (Option.map (fun p -> Point.create p.X (Scale.map y baseline),p)) |> Curves.area spec.Curve
                    | _,Some(xs,layers) ->
                        let layer=layers |> List.find (fun l -> l.Key=series)
                        let observed=rows |> List.choose(fun (_,_,x,y) -> match x,y with Some x,Some _ -> Some x | _ -> None) |> Set.ofList
                        List.map2 (fun xv (a,b) -> if Set.contains xv observed then Some(Point.create (Scale.map x xv) (Scale.map y a),Point.create (Scale.map x xv) (Scale.map y b)) else None) xs layer.Values |> Curves.area spec.Curve
                    | _ -> invalidOp "Missing stacked layout."
                let mark=Mark.path markId geometry |> Mark.withStroke (spec.Color series) 1.5 |> Mark.withFill (if spec.Kind=LineKind.Line then Paint.none else spec.Color series) |> Mark.withLabel(Some series)
                mark,Recipe.semantic markId (rows |> List.choose (point >> Option.map fst)) series) |> List.unzip
            let hits=rows |> List.choose (fun (id,series,a,b) ->
                match a,b with
                | Some a,Some b ->
                    let value =
                        match stacked with
                        | None -> b
                        | Some(xs,layers) ->
                            let index=List.findIndex ((=) a) xs
                            (layers |> List.find (fun layer -> layer.Key=series)).Values[index] |> snd
                    Some(Recipe.hit (Recipe.key ["line";series]) [id] (HitPoint(Point.create (Scale.map x a) (Scale.map y value),5.)))
                | _ -> None)
            Recipe.output spec.Frame bounds marks semantics hits (Recipe.axes bounds x y))

    let scatter (spec:ScatterSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.choose (fun d -> match spec.X.Read d,spec.Y.Read d with Some x,Some y -> Some(d,x,y) | _ -> None)
            let x=Scale.linear (Recipe.domain spec.XDomain (rows |> List.map (fun (_,x,_) -> Some x))) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
            let y=Scale.linear (Recipe.domain spec.YDomain (rows |> List.map (fun (_,_,y) -> Some y))) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let marks,semantics,hits=rows |> List.map (fun (d,a,b) ->
                let id=spec.Key d
                let markId=Recipe.key ["point";id]
                let area=spec.Area.Read d
                if not(Double.IsFinite area) || area<0. then invalidArg "area" "Invalid bubble area."
                let center=Point.create (Scale.map x a) (Scale.map y b)
                let label=spec.X.Label+": "+Recipe.number a+", "+spec.Y.Label+": "+Recipe.number b
                let mark=Shape.symbol Symbol.Circle area |> Mark.path markId |> Mark.withTransform(Transform.translate center.X center.Y) |> Mark.withFill(spec.Color d) |> Mark.withLabel(Some label)
                mark,Recipe.semantic markId [id] label,Recipe.hit markId [id] (HitPoint(center,sqrt(area/Math.PI)))) |> List.unzip3
            Recipe.output spec.Frame bounds marks semantics hits (Recipe.axes bounds x y))

    let heatmap (spec:HeatmapSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.choose (fun d -> spec.Value.Read d |> Option.map (fun value -> d,spec.Column.Read d,spec.Row.Read d,value))
            if rows |> List.countBy (fun (_,x,y,_) -> x,y) |> List.exists (fun (_,n) -> n>1) then invalidArg "data" "Duplicate heatmap cell."
            let columns=rows |> List.map (fun (_,x,_,_) -> x) |> List.distinct
            let categories=rows |> List.map (fun (_,_,y,_) -> y) |> List.distinct
            let x=Scale.band columns (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds) 0. 0.
            let y=Scale.band categories (Bounds.y bounds,Bounds.y bounds+Bounds.height bounds) 0. 0.
            let marks,semantics,hits=rows |> List.map (fun (d,column,row,value) ->
                if not(Double.IsFinite value) then invalidArg "data" "Non-finite heatmap value."
                let id=spec.Key d
                let markId=Recipe.key ["cell";id]
                let box=Bounds.create (Scale.bandPosition x column |> Option.get) (Scale.bandPosition y row |> Option.get) (Scale.bandwidth x) (Scale.bandwidth y)
                let label=column+", "+row+": "+Recipe.number value
                Recipe.rectangle markId label (spec.Color value) box,Recipe.semantic markId [id] label,Recipe.hit markId [id] (HitBounds box)) |> List.unzip3
            Recipe.output spec.Frame bounds marks semantics hits [])

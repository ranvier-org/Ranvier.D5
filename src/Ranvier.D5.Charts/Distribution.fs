namespace Ranvier.D5.Charts

open System
open Ranvier.D5

type HistogramSpec<'T> = {Frame:ChartFrame;Key:'T->string;Value:Encoding<'T,float option>;Bins:BinRule;Color:Paint}
[<RequireQualifiedAccess>]
type WhiskerPolicy = Extent | Tukey of multiplier:float
type BoxPlotSpec<'T> = {Frame:ChartFrame;Key:'T->string;Samples:Encoding<'T,float list>;Whiskers:WhiskerPolicy;Color:Paint}
type IntervalObservation = {Lower:float;Upper:float;Estimate:float}
type ErrorBarSpec<'T> = {Frame:ChartFrame;Key:'T->string;X:Encoding<'T,float>;Interval:Encoding<'T,IntervalObservation>;Color:Paint}
type WaterfallSpec<'T> = {Frame:ChartFrame;Key:'T->string;Value:Encoding<'T,float>;IsTotal:'T->bool;PositiveColor:Paint;NegativeColor:Paint}
type BulletSpec<'T> = {Frame:ChartFrame;Key:'T->string;Measure:Encoding<'T,float>;Target:Encoding<'T,float>;Ranges:float list;Color:Paint}
type LollipopSpec<'T> = {Frame:ChartFrame;Key:'T->string;Category:Encoding<'T,string>;Value:Encoding<'T,float option>;Color:Paint}
type Ohlc = {Open:float;High:float;Low:float;Close:float}
type OhlcSpec<'T> = {Frame:ChartFrame;Key:'T->string;X:Encoding<'T,float>;Values:Encoding<'T,Ohlc>;Candlestick:bool;UpColor:Paint;DownColor:Paint}

module internal Distribution =
    let private stroke id label color commands = Path.ofCommands commands |> Mark.path id |> Mark.withFill Paint.none |> Mark.withStroke color 1.5 |> Mark.withLabel(Some label)
    let private numeric bounds xs ys =
        Scale.linear (Recipe.domain Auto (List.map Some xs)) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds),
        Scale.linear (Recipe.domain IncludeZero (List.map Some ys)) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)

    let histogram (spec:HistogramSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.choose (fun d -> spec.Value.Read d |> Option.map (fun v -> spec.Key d,v))
            if rows |> List.exists (snd >> Double.IsFinite >> not) then invalidArg "data" "Non-finite histogram value."
            let bins=Statistics.histogram spec.Bins (List.map snd rows)
            let x,y=numeric bounds (bins |> List.collect (fun b -> [b.Lower;b.Upper])) (bins |> List.map (fun b -> float b.Values.Length))
            let memberships=
                match bins with
                | [] -> [||]
                | [bin] when bin.Lower=bin.Upper -> [|List.map fst rows|]
                | first::_ -> Data.bin snd (first.Lower::(bins |> List.map (fun b -> b.Upper))) rows |> List.map (fun b -> List.map fst b.Values) |> List.toArray
            let marks,semantics,hits=bins |> List.mapi (fun index bin ->
                let ids=memberships[index]
                let id=Recipe.key ["bin";bin.Lower.ToString("R",Globalization.CultureInfo.InvariantCulture);bin.Upper.ToString("R",Globalization.CultureInfo.InvariantCulture)]
                let left,right=Scale.map x bin.Lower,Scale.map x bin.Upper
                let top=Scale.map y (float ids.Length)
                let width=max 1. (right-left)
                let box=Bounds.create (if left=right then left-0.5 else left) top width (Scale.map y 0.-top)
                let label=Recipe.number bin.Lower+"–"+Recipe.number bin.Upper+": "+string ids.Length
                Recipe.rectangle id label spec.Color box,Recipe.semantic id ids label,Recipe.hit id ids (HitBounds box)) |> List.unzip3
            Recipe.output spec.Frame bounds marks semantics hits (Recipe.axes bounds x y))

    let boxPlot (spec:BoxPlotSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            match spec.Whiskers with WhiskerPolicy.Tukey k when not(Double.IsFinite k) || k<0. -> invalidArg "whiskers" "Invalid Tukey multiplier." | _ -> ()
            let rows=data |> List.choose (fun d ->
                let values=spec.Samples.Read d
                if values |> List.exists (Double.IsFinite >> not) then invalidArg "data" "Non-finite box observation."
                if values.IsEmpty then None else Some(spec.Key d,values))
            let bands=Scale.band (List.map fst rows) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds) 0.35 0.2
            let y=Scale.linear (Recipe.domain Auto (rows |> List.collect (snd >> List.map Some))) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let entries=rows |> List.collect (fun (dataId,values) ->
                let q p=Data.quantile p values |> Option.get
                let q1,median,q3=q 0.25,q 0.5,q 0.75
                let low,high=
                    match spec.Whiskers with
                    | WhiskerPolicy.Extent -> List.min values,List.max values
                    | WhiskerPolicy.Tukey k ->
                        let filtered=values |> List.filter (fun v -> v>=q1-k*(q3-q1) && v<=q3+k*(q3-q1))
                        if filtered.IsEmpty then q1,q3 else List.min filtered,List.max filtered
                let left=Scale.bandPosition bands dataId |> Option.get
                let width=Scale.bandwidth bands
                let center=left+width/2.
                let box=Bounds.create left (Scale.map y q3) width (Scale.map y q1-Scale.map y q3)
                let label=dataId+": median "+Recipe.number median
                let id=Recipe.key ["box";dataId]
                let body=Recipe.rectangle id label spec.Color box |> Mark.withOpacity 0.4
                let stemId=Recipe.key ["whisker";dataId]
                let stem=stroke stemId label spec.Color [MoveTo(Point.create center (Scale.map y low));LineTo(Point.create center (Scale.map y high));MoveTo(Point.create left (Scale.map y median));LineTo(Point.create (left+width) (Scale.map y median))]
                let outliers=values |> List.indexed |> List.choose (fun (i,v) ->
                    if v>=low && v<=high then None else
                    let id=Recipe.key ["outlier";dataId;string i]
                    let point=Point.create center (Scale.map y v)
                    let mark=Shape.symbol Symbol.Circle 12. |> Mark.path id |> Mark.withTransform(Transform.translate point.X point.Y) |> Mark.withFill spec.Color |> Mark.withLabel(Some(dataId+": "+Recipe.number v))
                    Some(mark,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitPoint(point,3.))))
                (body,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitBounds box))::(stem,Recipe.semantic stemId [dataId] label,Recipe.hit stemId [dataId] (HitBounds(Bounds.create (center-3.) (Scale.map y high) 6. (Scale.map y low-Scale.map y high))))::outliers)
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics hits [])

    let errorBars (spec:ErrorBarSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.map (fun d -> spec.Key d,spec.X.Read d,spec.Interval.Read d)
            for _,_,v in rows do
                if not(Double.IsFinite v.Lower && Double.IsFinite v.Upper && Double.IsFinite v.Estimate) || v.Lower>v.Estimate || v.Estimate>v.Upper then invalidArg "data" "Invalid explicit interval."
            let x,y=numeric bounds (rows |> List.map (fun (_,x,_) -> x)) (rows |> List.collect (fun (_,_,v) -> [v.Lower;v.Upper]))
            let entries=rows |> List.map (fun (dataId,xv,value) ->
                let id=Recipe.key ["interval";dataId]
                let center=Scale.map x xv
                let lo,hi=Scale.map y value.Lower,Scale.map y value.Upper
                let label=spec.Interval.Label+": "+Recipe.number value.Lower+"–"+Recipe.number value.Upper
                let commands=[MoveTo(Point.create center lo);LineTo(Point.create center hi);MoveTo(Point.create (center-4.) lo);LineTo(Point.create (center+4.) lo);MoveTo(Point.create (center-4.) hi);LineTo(Point.create (center+4.) hi);MoveTo(Point.create (center-3.) (Scale.map y value.Estimate));LineTo(Point.create (center+3.) (Scale.map y value.Estimate))]
                stroke id label spec.Color commands,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitBounds(Bounds.create (center-4.) hi 8. (lo-hi))))
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics hits (Recipe.axes bounds x y))

    let waterfall (spec:WaterfallSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let _,rows=data |> List.mapFold (fun total d ->
                let value=spec.Value.Read d
                let next=if spec.IsTotal d then value else total+value
                (spec.Key d,(if spec.IsTotal d then 0. else total),next),next) 0. |> fun (rows,total) -> total,rows
            let bands=Scale.band (rows |> List.map (fun (id,_,_) -> id)) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds) 0.2 0.1
            let y=Scale.linear (Recipe.domain IncludeZero (rows |> List.collect (fun (_,a,b) -> [Some a;Some b]))) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let marks,semantics,hits=rows |> List.map (fun (dataId,a,b) ->
                let id=Recipe.key ["waterfall";dataId]
                let box=Bounds.create (Scale.bandPosition bands dataId |> Option.get) (min (Scale.map y a) (Scale.map y b)) (Scale.bandwidth bands) (abs(Scale.map y a-Scale.map y b))
                let label=dataId+": "+Recipe.number a+" → "+Recipe.number b
                Recipe.rectangle id label (if b>=a then spec.PositiveColor else spec.NegativeColor) box,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitBounds box)) |> List.unzip3
            Recipe.output spec.Frame bounds marks semantics hits [])

    let lollipop (spec:LollipopSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.choose (fun d -> spec.Value.Read d |> Option.map (fun v -> spec.Key d,spec.Category.Read d,v))
            let bands=Scale.band (rows |> List.map (fun (id,_,_) -> id)) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds) 0.2 0.1
            let y=Scale.linear (Recipe.domain IncludeZero (rows |> List.map (fun (_,_,v) -> Some v))) (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let entries=rows |> List.collect (fun (dataId,category,value) ->
                let center=(Scale.bandPosition bands dataId |> Option.get)+Scale.bandwidth bands/2.
                let point=Point.create center (Scale.map y value)
                let label=category+": "+Recipe.number value
                let stemId,pointId=Recipe.key ["stem";dataId],Recipe.key ["head";dataId]
                let stem=stroke stemId label spec.Color [MoveTo(Point.create center (Scale.map y 0.));LineTo point]
                let head=Shape.symbol Symbol.Circle 40. |> Mark.path pointId |> Mark.withFill spec.Color |> Mark.withTransform(Transform.translate point.X point.Y) |> Mark.withLabel(Some label)
                [stem,Recipe.semantic stemId [dataId] label,Recipe.hit stemId [dataId] (HitBounds(Bounds.create (center-2.) (min point.Y (Scale.map y 0.)) 4. (abs(point.Y-Scale.map y 0.))))
                 head,Recipe.semantic pointId [dataId] label,Recipe.hit pointId [dataId] (HitPoint(point,4.))])
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics hits [])

    let bullet (spec:BulletSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            if spec.Ranges |> List.exists (fun v -> not(Double.IsFinite v) || v<0.) then invalidArg "ranges" "Invalid qualitative range."
            if spec.Ranges |> List.pairwise |> List.exists (fun (a,b) -> a>=b) then invalidArg "ranges" "Ranges must increase."
            let rows=data |> List.map (fun d -> spec.Key d,spec.Measure.Read d,spec.Target.Read d)
            let x=Scale.linear (Recipe.domain IncludeZero ((spec.Ranges @ (rows |> List.collect (fun (_,a,b) -> [a;b]))) |> List.map Some)) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds)
            let bands=Scale.band (rows |> List.map (fun (id,_,_) -> id)) (Bounds.y bounds,Bounds.y bounds+Bounds.height bounds) 0.3 0.1
            let entries=rows |> List.collect (fun (dataId,value,target) ->
                let top=Scale.bandPosition bands dataId |> Option.get
                let h=Scale.bandwidth bands
                let label=dataId+": "+Recipe.number value+", target "+Recipe.number target
                let rectangle role height offset value color opacity =
                    let id=Recipe.key [role;dataId]
                    let box=Bounds.create (min (Scale.map x 0.) (Scale.map x value)) (top+offset) (abs(Scale.map x value-Scale.map x 0.)) height
                    Recipe.rectangle id label color box |> Mark.withOpacity opacity,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitBounds box)
                let ranges=spec.Ranges |> List.indexed |> List.rev |> List.map (fun (i,value) -> rectangle ("range"+string i) h 0. value (Paint.rgb 120 120 120) (0.15+0.5/float(i+1)))
                let id=Recipe.key ["target";dataId]
                let marker=stroke id label spec.Color [MoveTo(Point.create (Scale.map x target) top);LineTo(Point.create (Scale.map x target) (top+h))]
                ranges @ [rectangle "measure" (h/3.) (h/3.) value spec.Color 1.;marker,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitBounds(Bounds.create (Scale.map x target-2.) top 4. h))])
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics hits [])

    let ohlc (spec:OhlcSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.map (fun d -> spec.Key d,spec.X.Read d,spec.Values.Read d)
            for _,_,v in rows do
                if [v.Open;v.High;v.Low;v.Close] |> List.exists (Double.IsFinite >> not) then invalidArg "data" "Non-finite OHLC."
                if v.Low>min v.Open v.Close || v.High<max v.Open v.Close then invalidArg "data" "OHLC low/high do not contain open/close."
            let x,y=numeric bounds (rows |> List.map (fun (_,x,_) -> x)) (rows |> List.collect (fun (_,_,v) -> [v.Low;v.High]))
            let half=min 6. (Bounds.width bounds/float(max 1 rows.Length)/3.)
            let entries=rows |> List.collect (fun (dataId,xv,v) ->
                let center=Scale.map x xv
                let color=if v.Close>=v.Open then spec.UpColor else spec.DownColor
                let label=dataId+": O "+Recipe.number v.Open+", H "+Recipe.number v.High+", L "+Recipe.number v.Low+", C "+Recipe.number v.Close
                let id=Recipe.key ["wick";dataId]
                let commands=[MoveTo(Point.create center (Scale.map y v.Low));LineTo(Point.create center (Scale.map y v.High))]
                let commands=if spec.Candlestick then commands else commands@[MoveTo(Point.create (center-half) (Scale.map y v.Open));LineTo(Point.create center (Scale.map y v.Open));MoveTo(Point.create center (Scale.map y v.Close));LineTo(Point.create (center+half) (Scale.map y v.Close))]
                let wick=stroke id label color commands
                let box=Bounds.create (center-half) (Scale.map y v.High) (2.*half) (Scale.map y v.Low-Scale.map y v.High)
                let main=wick,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitBounds box)
                if not spec.Candlestick then [main] else
                let bodyId=Recipe.key ["body";dataId]
                let body=Bounds.create (center-half) (min (Scale.map y v.Open) (Scale.map y v.Close)) (2.*half) (max 1. (abs(Scale.map y v.Open-Scale.map y v.Close)))
                [main;Recipe.rectangle bodyId label color body,Recipe.semantic bodyId [dataId] label,Recipe.hit bodyId [dataId] (HitBounds body)])
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics hits (Recipe.axes bounds x y))

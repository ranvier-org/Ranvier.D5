namespace Ranvier.D5.Charts

open System
open Ranvier.D5

/// <summary>Pie or donut specification: values must be finite and nonnegative, InnerRatio is in [0,1), and MeasureLabel sizes slice labels in pixels so overlapping ones are skipped.</summary>
type PieSpec<'T> = {Frame:ChartFrame;Key:'T->string;Value:Encoding<'T,float option>;Label:'T->string;Color:string->Paint;InnerRatio:float;MeasureLabel:string->TextSize}
/// <summary>How a radial bar's value maps to its outer radius: proportional to Radius length or to annular Area.</summary>
[<RequireQualifiedAccess>]
type RadiusEncoding = Radius | Area
/// <summary>Radial bar specification with one equal-angle sector per datum scaled to the largest value; InnerRatio and Padding are fractions in [0,1).</summary>
type RadialBarSpec<'T> = {Frame:ChartFrame;Key:'T->string;Value:Encoding<'T,float option>;Color:string->Paint;Encoding:RadiusEncoding;InnerRatio:float;Padding:float}
/// <summary>Radar specification with at least three dimensions and exactly one domain policy per dimension.</summary>
type RadarSpec<'T> = {Frame:ChartFrame;Key:'T->string;Dimensions:Encoding<'T,float option> list;Domains:DomainPolicy list;Color:string->Paint}
/// <summary>Parallel coordinates specification with at least two dimensions and exactly one domain policy per dimension.</summary>
type ParallelSpec<'T> = {Frame:ChartFrame;Key:'T->string;Dimensions:Encoding<'T,float option> list;Domains:DomainPolicy list;Color:string->Paint}
/// <summary>Calendar heatmap specification with one square per date in weekday rows and week columns; dates must be unique.</summary>
type CalendarSpec<'T> = {Frame:ChartFrame;Key:'T->string;Date:Encoding<'T,DateOnly>;Value:Encoding<'T,float option>;Color:float->Paint;WeekStartsOn:DayOfWeek}

module internal Radial =
    let private geometry bounds =
        Point.create (Bounds.x bounds+Bounds.width bounds/2.) (Bounds.y bounds+Bounds.height bounds/2.),min (Bounds.width bounds) (Bounds.height bounds)/2.
    let private ratio name x = if not(Double.IsFinite x) || x<0. || x>=1. then invalidArg name "Expected ratio in [0,1)."
    let pie (spec:PieSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            ratio "innerRatio" spec.InnerRatio
            let rows=data |> List.choose (fun d -> spec.Value.Read d |> Option.map (fun value -> d,value))
            if rows |> List.exists (fun (_,v) -> not(Double.IsFinite v) || v<0.) then invalidArg "data" "Pie values must be finite and nonnegative."
            let center,radius=geometry bounds
            let outer=radius*0.8
            let inner=outer*spec.InnerRatio
            let slices=Layout.pie snd 0. (2.*Math.PI) rows
            let entries=slices |> List.map (fun slice ->
                let d,value=slice.Data
                let dataId=spec.Key d
                let id=Recipe.key ["slice";dataId]
                let label=spec.Label d+": "+Recipe.number value
                let mark=Shape.arc inner outer slice.StartAngle slice.EndAngle |> Mark.path id |> Mark.withFill(spec.Color dataId) |> Mark.withTransform(Transform.translate center.X center.Y) |> Mark.withLabel(Some label)
                mark,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitSector(center,inner,outer,slice.StartAngle,slice.EndAngle)))
            let marks,semantics,hits=List.unzip3 entries
            let requests=slices |> List.choose (fun slice ->
                if slice.Value=0. then None else
                let d,_=slice.Data
                let text=spec.Label d
                let size=spec.MeasureLabel text
                let point=Curves.radialPoint ((slice.StartAngle+slice.EndAngle)/2.) (outer*0.7+inner*0.3)
                Some{LabelRequest.Id=Recipe.key ["label";spec.Key d];Text=text;Position=Point.create (center.X+point.X-size.Width/2.) (center.Y+point.Y-size.Height/2.);Priority=0})
            let labels=LabelLayout.place bounds spec.MeasureLabel requests |> List.map (fun label -> Mark.text label.Id (Point.create (Bounds.x label.Bounds) (Bounds.y label.Bounds+Bounds.height label.Bounds)) label.Text)
            Recipe.output spec.Frame bounds (marks@labels) semantics hits [])

    let radialBar (spec:RadialBarSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            ratio "innerRatio" spec.InnerRatio
            ratio "padding" spec.Padding
            let rows=data |> List.choose (fun d -> spec.Value.Read d |> Option.map (fun v -> spec.Key d,v))
            if rows |> List.exists (fun (_,v) -> not(Double.IsFinite v) || v<0.) then invalidArg "data" "Radial values must be finite and nonnegative."
            let center,outer=geometry bounds
            let inner=outer*spec.InnerRatio
            let maximum=rows |> List.map snd |> List.fold max 0.
            let entries=rows |> List.mapi (fun index (dataId,value) ->
                let fraction=if maximum=0. then 0. else value/maximum
                let radius=match spec.Encoding with RadiusEncoding.Radius -> inner+(outer-inner)*fraction | RadiusEncoding.Area -> sqrt(inner*inner+(outer*outer-inner*inner)*fraction)
                let step=2.*Math.PI/float rows.Length
                let first=float index*step+step*spec.Padding/2.
                let last=float(index+1)*step-step*spec.Padding/2.
                let id=Recipe.key ["radial-bar";dataId]
                let label=dataId+": "+Recipe.number value
                let mark=Shape.arc inner radius first last |> Mark.path id |> Mark.withFill(spec.Color dataId) |> Mark.withTransform(Transform.translate center.X center.Y) |> Mark.withLabel(Some label)
                mark,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitSector(center,inner,radius,first,last)))
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics hits [])

    let private dimensionScales dimensions domains data range =
        if List.length dimensions<>List.length domains then invalidArg "domains" "One domain policy per dimension required."
        List.map2 (fun (encoding:Encoding<'T,float option>) policy -> Scale.linear (Recipe.domain policy (List.map encoding.Read data)) range) dimensions domains

    let radar (spec:RadarSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            if spec.Dimensions.Length<3 then invalidArg "dimensions" "Radar requires at least three dimensions."
            let center,radius=geometry bounds
            let scales=dimensionScales spec.Dimensions spec.Domains data (0.,radius*0.8)
            let entries=data |> List.map (fun d ->
                let dataId=spec.Key d
                let id=Recipe.key ["radar";dataId]
                let points=List.zip spec.Dimensions scales |> List.mapi (fun i (dimension,scale) -> dimension.Read d |> Option.map (fun v -> Curves.radialPoint (float i*2.*Math.PI/float scales.Length) (Scale.map scale v)))
                let complete=List.forall Option.isSome points
                let path=Curves.line CurveSpec.Linear (if complete then points@[List.head points] else points)
                let mark=Mark.path id path |> Mark.withTransform(Transform.translate center.X center.Y) |> Mark.withStroke(spec.Color dataId) 1.5 |> Mark.withFill(if complete then spec.Color dataId else Paint.none) |> Mark.withOpacity 0.5 |> Mark.withLabel(Some dataId)
                let hits=points |> List.choose (fun point -> point) |> List.map (fun point -> Recipe.hit id [dataId] (HitPoint(Point.create (center.X+point.X) (center.Y+point.Y),5.)))
                mark,Recipe.semantic id [dataId] dataId,hits)
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics (List.concat hits) [])

    let parallelCoordinates (spec:ParallelSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            if spec.Dimensions.Length<2 then invalidArg "dimensions" "Parallel coordinates require at least two dimensions."
            let scales=dimensionScales spec.Dimensions spec.Domains data (Bounds.y bounds+Bounds.height bounds,Bounds.y bounds)
            let position i=Bounds.x bounds+float i*Bounds.width bounds/float(scales.Length-1)
            let entries=data |> List.map (fun d ->
                let dataId=spec.Key d
                let id=Recipe.key ["parallel";dataId]
                let points=List.zip spec.Dimensions scales |> List.mapi (fun i (dimension,scale) -> dimension.Read d |> Option.map (fun v -> Point.create (position i) (Scale.map scale v)))
                let mark=Curves.line CurveSpec.Linear points |> Mark.path id |> Mark.withStroke(spec.Color dataId) 1.5 |> Mark.withFill Paint.none |> Mark.withLabel(Some dataId)
                mark,Recipe.semantic id [dataId] dataId,(points |> List.choose (fun x -> x) |> List.map (fun point -> Recipe.hit id [dataId] (HitPoint(point,5.)))))
            let marks,semantics,hits=List.unzip3 entries
            let axes=List.zip spec.Dimensions scales |> List.mapi (fun i (dimension,scale) ->
                Axis.values (Recipe.key ["dimension";string i;dimension.Label]) AxisOrientation.Left (position i) (Bounds.y bounds,Bounds.y bounds+Bounds.height bounds)
                    (Scale.ticks 4 scale |> List.map (fun value -> value.ToString("R",Globalization.CultureInfo.InvariantCulture),Scale.map scale value,Recipe.number value))) |> List.concat
            Recipe.output spec.Frame bounds marks semantics (List.concat hits) axes)

    let calendar (spec:CalendarSpec<'T>) data =
        Recipe.run spec.Frame spec.Key data (fun bounds ->
            let rows=data |> List.choose (fun d -> spec.Value.Read d |> Option.map (fun value -> spec.Key d,spec.Date.Read d,value))
            if int spec.WeekStartsOn<0 || int spec.WeekStartsOn>6 then invalidArg "weekStartsOn" "Invalid weekday."
            if rows |> List.countBy (fun (_,date,_) -> date) |> List.exists (fun (_,n) -> n>1) then invalidArg "data" "Duplicate calendar date."
            let weekday (date:DateOnly)=(int date.DayOfWeek-int spec.WeekStartsOn+7)%7
            let first=rows |> List.map (fun (_,date,_) -> date.DayNumber-weekday date) |> List.fold min Int32.MaxValue
            let last=rows |> List.map (fun (_,date,_) -> date.DayNumber) |> List.fold max 0
            let weeks=if rows.IsEmpty then 1 else (last-first)/7+1
            let cell=min (Bounds.width bounds/float weeks) (Bounds.height bounds/7.)
            let entries=rows |> List.map (fun (dataId,date,value) ->
                if not(Double.IsFinite value) then invalidArg "data" "Non-finite calendar value."
                let id=Recipe.key ["day";dataId]
                let box=Bounds.create (Bounds.x bounds+float((date.DayNumber-first)/7)*cell) (Bounds.y bounds+float(weekday date)*cell) cell cell
                let label=date.ToString("yyyy-MM-dd",Globalization.CultureInfo.InvariantCulture)+": "+Recipe.number value
                Recipe.rectangle id label (spec.Color value) box,Recipe.semantic id [dataId] label,Recipe.hit id [dataId] (HitBounds box))
            let marks,semantics,hits=List.unzip3 entries
            Recipe.output spec.Frame bounds marks semantics hits [])

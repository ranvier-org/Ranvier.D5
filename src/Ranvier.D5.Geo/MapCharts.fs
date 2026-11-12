namespace Ranvier.D5.Geo

open System
open System.Globalization
open Ranvier.D5
open Ranvier.D5.Charts

/// <summary>A geographic feature joined to data by a stable, non-empty identifier.</summary>
type MapFeature = { Id:string; Name:string; Geometry:GeoGeometry }

/// <summary>The projection a map draws with: one projection or a composite of insets.</summary>
[<RequireQualifiedAccess>]
type MapProjection =
    | Single of Projection
    | Composite of CompositeProjection

/// <summary>Non-interactive context drawn beneath a map's data marks, in order.</summary>
[<RequireQualifiedAccess>]
type MapLayer =
    /// <summary>The projected world outline.</summary>
    | Sphere of fill:Paint
    /// <summary>Stroked lines such as a graticule or coastlines.</summary>
    | Lines of key:string * geometry:GeoGeometry * stroke:Paint * width:float
    /// <summary>Filled context polygons such as land.</summary>
    | Fill of key:string * geometry:GeoGeometry * fill:Paint

/// <summary>Frame, projection, path options and background shared by map recipes. The plot bounds always clip the map.</summary>
type MapFrame = { Frame:ChartFrame; Projection:MapProjection; Options:GeoPathOptions; Background:MapLayer list }

/// <summary>Regions coloured by a value joined on feature ID; features without a value use Missing.</summary>
type ChoroplethSpec<'T> = {
    Map:MapFrame; Key:'T->string; Value:Encoding<'T,float option>; Domain:DomainPolicy
    /// <summary>Maps the value's position in the domain, in [0,1], to a fill.</summary>
    Color:float->Paint
    Missing:Paint; Stroke:Paint; Selected:Set<string> }

/// <summary>Circles at geographic positions whose area encodes Size; the largest datum has MaxRadius.</summary>
type BubbleMapSpec<'T> = { Map:MapFrame; Key:'T->string; Position:'T->GeoPosition; Size:Encoding<'T,float>; MaxRadius:float; Color:'T->Paint; Selected:Set<string> }

/// <summary>Shortest great-circle routes between two positions, split at seams and clipped like any line.</summary>
type RouteMapSpec<'T> = { Map:MapFrame; Key:'T->string; Origin:'T->GeoPosition; Destination:'T->GeoPosition; Width:float; Color:'T->Paint; Selected:Set<string> }

/// <summary>Feature construction from GeoJSON documents.</summary>
module MapFeature =
    let private text = function
        | GeoId.Text value -> value
        | GeoId.Number value -> value

    /// <summary>Converts a document's features, requiring unique non-empty IDs and a geometry for each.</summary>
    let ofDocument (name:GeoFeature -> string option) document : Result<MapFeature list,LayoutError list> =
        let features =
            match document with
            | GeoDocument.FeatureCollection(features,_) -> features
            | GeoDocument.Feature feature -> [feature]
            | GeoDocument.Geometry _ -> []
        let converted=features |> List.mapi(fun index feature ->
            match feature.Id,feature.Geometry with
            | Some id,Some geometry when not(String.IsNullOrEmpty(text id)) ->
                Ok {Id=text id;Name=name feature |> Option.defaultValue (text id);Geometry=geometry}
            | Some _,Some _ | None,_ -> Error(InvalidObservation(sprintf "Feature %d has no stable ID." index))
            | _,None -> Error(InvalidObservation(sprintf "Feature %d has no geometry." index)))
        let errors=converted |> List.choose(function Error error -> Some error | Ok _ -> None)
        let valid=converted |> List.choose(function Ok feature -> Some feature | Error _ -> None)
        let duplicates=ChartLayout.validateIds (fun (feature:MapFeature) -> feature.Id) valid
        match errors@duplicates with
        | [] -> Ok valid
        | problems -> Error problems

/// <summary>Map frames and the pure map recipes.</summary>
module MapFrame =
    /// <summary>A frame with no background, default path options and the given projection.</summary>
    let create frame projection = {Frame=frame;Projection=projection;Options=GeoPathOptions.defaultValue;Background=[]}

/// <summary>Choropleth, bubble and route recipes returning ordinary chart outputs with semantic IDs and region-aware picking.</summary>
module MapChart =
    exception private Rejected of LayoutError list

    let private key parts=parts |> List.map(fun (part:string) -> string part.Length+":"+part) |> String.concat ""
    let private number (value:float)=value.ToString("G6",CultureInfo.InvariantCulture)
    let private ok = function Ok value -> value | Error errors -> raise(Rejected errors)

    let private paths (map:MapFrame) bounds geometry =
        let options={map.Options with ClipExtent=Some bounds}
        match map.Projection with
        | MapProjection.Single projection -> GeoPath.project options projection geometry
        | MapProjection.Composite composite -> CompositeProjection.paths options composite geometry
        |> ok

    let private projectPoint (map:MapFrame) position =
        match map.Projection with
        | MapProjection.Single projection -> Projection.project projection position
        | MapProjection.Composite composite -> CompositeProjection.project composite position

    /// Concatenates paths so one feature, even with several members, is one keyed mark.
    let private combine (paths:Path list) =
        match paths with
        | [] -> None
        | [path] -> Some path
        | _ -> Some(Path.ofCommands(paths |> List.collect Path.commands))

    let private rings (path:Path) =
        let rings=ResizeArray<Point list>()
        let mutable current=[]
        for command in Path.commands path do
            match command with
            | MoveTo point -> (if current.Length>2 then rings.Add(List.rev current)); current<-[point]
            | LineTo point -> current<-point::current
            | Close -> (if current.Length>2 then rings.Add(List.rev current)); current<-[]
            | _ -> ()
        if current.Length>2 then rings.Add(List.rev current)
        List.ofSeq rings

    /// Exteriors share the largest ring's orientation; each opposite ring is a hole of the exterior containing it.
    let private hitPolygons (paths:Path list) =
        paths |> List.collect(fun path ->
            let rings=rings path |> List.filter(fun ring -> Polygon.area ring<>0.)
            match rings with
            | [] -> []
            | _ ->
                let sign=rings |> List.maxBy(fun ring -> abs(Polygon.area ring)) |> Polygon.area |> Math.Sign
                let exteriors,holes=rings |> List.partition(fun ring -> Math.Sign(Polygon.area ring)=sign)
                exteriors |> List.map(fun exterior ->
                    {Outer=exterior;Holes=holes |> List.filter(fun hole -> Polygon.contains hole.Head exterior)}))

    /// An interior point for selection and labels: the midpoint of the widest inside span on several scanlines,
    /// so concave slivers at a limb or seam still anchor inside their fill rather than on an edge.
    let private anchor (polygons:HitPolygon list) =
        let candidates=polygons |> List.collect(fun polygon ->
            let rings=polygon.Outer::polygon.Holes
            let ys=polygon.Outer |> List.map(fun point -> point.Y)
            let top,bottom=List.min ys,List.max ys
            [for step in 1..7 do
                let y=top+(bottom-top)*float step/8.
                let crossings=
                    rings |> List.collect(fun ring ->
                        (ring@[ring.Head]) |> List.pairwise |> List.choose(fun (a,b) ->
                            if (a.Y<=y)<>(b.Y<=y) then Some(a.X+(y-a.Y)*(b.X-a.X)/(b.Y-a.Y)) else None))
                    |> List.sort
                for pair in crossings |> List.chunkBySize 2 do
                    match pair with
                    | [left;right] when right>left -> yield right-left,Point.create ((left+right)/2.) y
                    | _ -> ()])
        match candidates with
        | [] -> (polygons |> List.maxBy(fun polygon -> abs(Polygon.area polygon.Outer))).Outer.Head
        | _ -> candidates |> List.maxBy fst |> snd

    let private run (map:MapFrame) build =
        match ChartLayout.plot map.Frame with
        | Error errors -> Error errors
        | Ok bounds ->
            try
                let background=map.Background |> List.mapi(fun index layer ->
                    let prefix=key ["background";string index]
                    let marks name style geometry=
                        paths map bounds geometry |> List.mapi(fun part path -> Mark.path (key [prefix;name;string part]) path |> style)
                    match layer with
                    | MapLayer.Sphere fill -> marks "sphere" (Mark.withFill fill) GeoGeometry.Sphere
                    | MapLayer.Fill(name,geometry,fill) -> marks name (Mark.withFill fill) geometry
                    | MapLayer.Lines(name,geometry,stroke,width) -> marks name (Mark.withFill Paint.none >> Mark.withStroke stroke width) geometry)
                let (marks:Mark list),(semantics:SemanticMark list),(hits:HitTarget list)=build bounds
                let scene=Scene.compose map.Frame.Viewport map.Frame.Title [RectClip("plot-clip",bounds)]
                            [Group({GroupSpec.create "plot" with Clip=Some "plot-clip"},List.map Leaf (List.concat background@marks))]
                Ok {Scene=scene;PlotBounds=bounds;Semantics=semantics;HitTargets=hits |> List.map(fun (hit:HitTarget) -> {hit with Clip=Some bounds});Description=map.Frame.Title}
            with
            | Rejected errors -> Error errors
            | :? ArgumentException as error -> Error [InvalidObservation error.Message]

    let private ids key data = match ChartLayout.validateIds key data with [] -> Ok() | errors -> Error errors

    /// <summary>Colours each feature by the datum sharing its ID; data without a matching feature is an error, never discarded.</summary>
    let choropleth (spec:ChoroplethSpec<'T>) (features:MapFeature list) (data:'T list) : Result<ChartOutput,LayoutError list> =
        let featureIds=features |> List.map(fun feature -> feature.Id) |> Set.ofList
        let unmatched=data |> List.choose(fun datum -> if featureIds.Contains(spec.Key datum) then None else Some(InvalidObservation(sprintf "Data key %s has no feature." (spec.Key datum))))
        match ids (fun (feature:MapFeature) -> feature.Id) features,ids spec.Key data,unmatched with
        | Error errors,_,_ | _,Error errors,_ -> Error errors
        | _,_,(_::_ as errors) -> Error errors
        | Ok(),Ok(),[] ->
            let values=data |> List.map(fun datum -> spec.Key datum,spec.Value.Read datum) |> Map.ofList
            match ChartLayout.domain spec.Domain (Map.values values |> List.ofSeq) with
            | Error errors -> Error errors
            | Ok(low,high) ->
                run spec.Map (fun bounds ->
                    features |> List.choose(fun feature ->
                        let projected=paths spec.Map bounds feature.Geometry
                        combine projected |> Option.map(fun path ->
                            let id=key ["feature";feature.Id]
                            let value=Map.tryFind feature.Id values |> Option.flatten
                            let fill=value |> Option.map(fun value -> spec.Color(Math.Clamp((value-low)/(high-low),0.,1.))) |> Option.defaultValue spec.Missing
                            let label=feature.Name+": "+(value |> Option.map number |> Option.defaultValue "no data")
                            let width=if spec.Selected.Contains feature.Id then 2.5 else 0.75
                            let mark=Mark.path id path |> Mark.withFill fill |> Mark.withStroke spec.Stroke width |> Mark.withLabel(Some label)
                            let polygons=hitPolygons projected
                            let hit=if polygons.IsEmpty then [] else [{MarkPath=["plot";id];DataIds=[feature.Id];Geometry=HitRegion(polygons,[],anchor polygons);Clip=None}]
                            mark,{MarkPath=["plot";id];DataIds=[feature.Id];Description=label},hit))
                    |> List.unzip3 |> fun (marks,semantics,hits) -> marks,semantics,List.concat hits)

    /// <summary>Draws one circle per visible datum, largest first so smaller circles stay pickable on top.</summary>
    let bubbles (spec:BubbleMapSpec<'T>) (data:'T list) : Result<ChartOutput,LayoutError list> =
        if not(Double.IsFinite spec.MaxRadius && spec.MaxRadius>0.) then Error [InvalidObservation "Bubble radius must be positive and finite."]
        elif data |> List.exists(fun datum -> let size=spec.Size.Read datum in not(Double.IsFinite size) || size<0.) then
            Error [InvalidObservation "Bubble sizes must be finite and nonnegative."]
        else
            match ids spec.Key data with
            | Error errors -> Error errors
            | Ok() ->
                let largest=data |> List.fold(fun state datum -> max state (spec.Size.Read datum)) 0.
                run spec.Map (fun bounds ->
                    data
                    |> List.sortBy(fun datum -> -spec.Size.Read datum,spec.Key datum)
                    |> List.choose(fun datum ->
                        let radius=if largest=0. then 0. else spec.MaxRadius*sqrt(spec.Size.Read datum/largest)
                        let position=spec.Position datum
                        match projectPoint spec.Map position with
                        | Some center when radius>0. ->
                            let options={spec.Map with Options={spec.Map.Options with PointRadius=radius}}
                            paths options bounds (GeoGeometry.Point(Some position)) |> combine |> Option.map(fun path ->
                                let id=key ["bubble";spec.Key datum]
                                let label=sprintf "%s: %s %s" (spec.Key datum) spec.Size.Label (number(spec.Size.Read datum))
                                let mark=Mark.path id path |> Mark.withFill(spec.Color datum) |> Mark.withOpacity 0.85 |> Mark.withLabel(Some label)
                                let mark=if spec.Selected.Contains(spec.Key datum) then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                                mark,{MarkPath=["plot";id];DataIds=[spec.Key datum];Description=label},
                                {MarkPath=["plot";id];DataIds=[spec.Key datum];Geometry=HitPoint(center,radius);Clip=None})
                        | _ -> None)
                    |> List.unzip3)

    /// <summary>Draws each route as its shortest great-circle arc; picking uses a band around every projected segment.</summary>
    let routes (spec:RouteMapSpec<'T>) (data:'T list) : Result<ChartOutput,LayoutError list> =
        if not(Double.IsFinite spec.Width && spec.Width>0.) then Error [InvalidObservation "Route width must be positive and finite."]
        else
            match ids spec.Key data with
            | Error errors -> Error errors
            | Ok() ->
                let reach=max (spec.Width/2.) 3.
                let band (a:Point) (b:Point) =
                    let length=Double.Hypot(b.X-a.X,b.Y-a.Y)
                    if length=0. then None
                    else
                        let nx,ny= -(b.Y-a.Y)/length*reach,(b.X-a.X)/length*reach
                        Some {Outer=[Point.create (a.X+nx) (a.Y+ny);Point.create (b.X+nx) (b.Y+ny);Point.create (b.X-nx) (b.Y-ny);Point.create (a.X-nx) (a.Y-ny)];Holes=[]}
                run spec.Map (fun bounds ->
                    data |> List.choose(fun datum ->
                        let projected=paths spec.Map bounds (GeoGeometry.LineString[spec.Origin datum;spec.Destination datum])
                        combine projected |> Option.map(fun path ->
                            let id=key ["route";spec.Key datum]
                            let label=spec.Key datum
                            let width=if spec.Selected.Contains(spec.Key datum) then spec.Width*2. else spec.Width
                            let mark=Mark.path id path |> Mark.withFill Paint.none |> Mark.withStroke (spec.Color datum) width |> Mark.withLabel(Some label)
                            let vertices=projected |> List.collect(fun path ->
                                Path.commands path |> List.choose(function MoveTo point -> Some(true,point) | LineTo point -> Some(false,point) | _ -> None))
                            let segments=vertices |> List.pairwise |> List.choose(fun ((_,a),(start,b)) -> if start then None else band a b)
                            let hit=
                                match segments with
                                | [] -> []
                                | _ -> [{MarkPath=["plot";id];DataIds=[spec.Key datum];Geometry=HitRegion(segments,[],(snd vertices.Head));Clip=None}]
                            mark,{MarkPath=["plot";id];DataIds=[spec.Key datum];Description=label},hit))
                    |> List.unzip3 |> fun (marks,semantics,hits) -> marks,semantics,List.concat hits)

    /// <summary>Stacks map outputs drawn in one frame; later layers paint and pick above earlier ones.</summary>
    /// <remarks>Each layer's nodes, semantics and hit targets are scoped under its own group key, so identical local keys never collide.</remarks>
    let overlay (layers:(string*ChartOutput) list) : Result<ChartOutput,LayoutError list> =
        match layers with
        | [] -> Error [InvalidObservation "An overlay needs at least one layer."]
        | (_,first)::_ ->
            match ChartLayout.validateIds fst layers with
            | _::_ as errors -> Error errors
            | [] ->
                if layers |> List.exists(fun (_,output) -> output.PlotBounds<>first.PlotBounds || Scene.width output.Scene<>Scene.width first.Scene || Scene.height output.Scene<>Scene.height first.Scene) then
                    Error [InvalidObservation "Overlay layers must share one frame."]
                else
                    try
                        let definitions=layers |> List.collect(fun (_,output) -> Scene.definitions output.Scene) |> List.distinctBy SceneDefinition.key
                        let nodes=layers |> List.map(fun (scope,output) -> Group(GroupSpec.create scope,Scene.nodes output.Scene))
                        let scene=Scene.compose {Width=Scene.width first.Scene;Height=Scene.height first.Scene} (Scene.title first.Scene) definitions nodes
                        Ok {Scene=scene;PlotBounds=first.PlotBounds
                            Semantics=layers |> List.collect(fun (scope,output) -> output.Semantics |> List.map(fun semantic -> {semantic with MarkPath=scope::semantic.MarkPath}))
                            HitTargets=layers |> List.collect(fun (scope,output) -> output.HitTargets |> List.map(fun hit -> {hit with MarkPath=scope::hit.MarkPath}))
                            Description=first.Description}
                    with :? ArgumentException as error -> Error [InvalidObservation error.Message]

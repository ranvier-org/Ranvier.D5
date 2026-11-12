module Ranvier.D5.Gallery.MapGallery

open System
open System.IO
open Ranvier
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.Geo
open Ranvier.D5.View
open Ranvier.View
open Ranvier.View.Dom
open Ranvier.View.WebCore

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private p longitude latitude=GeoPosition.create longitude latitude |> ok

/// <summary>Synthetic regions shipped with the gallery; values are a deterministic function of the region number.</summary>
let regionsPath=Path.Combine(AppContext.BaseDirectory,"data","regions.geojson")
type RegionValue = { Region:string; Value:float }
let values=[for index in 1..24 -> {Region=sprintf "R%02d" index;Value=float((index*37)%24)}]
/// <summary>Blue to amber sequential fill for a domain position in [0,1].</summary>
let fill (t:float)=int(50.+170.*t),int(110.+40.*t),int(200.-150.*t)
type City = { Name:string; Position:GeoPosition; Population:float }
let cities=[
    {Name="Aster";Position=p -100. 35.;Population=9.}
    {Name="Brook";Position=p -60. -5.;Population=4.}
    {Name="Cedar";Position=p -135. 5.;Population=2.5}
    {Name="Delta";Position=p -20. 30.;Population=6.}
    {Name="Ember";Position=p 30. -40.;Population=3.}]
let private flights=[ "Aster-Brook","Aster","Brook";"Aster-Delta","Aster","Delta";"Cedar-Aster","Cedar","Aster" ]
let private city name=cities |> List.find(fun item -> item.Name=name)

let frame={ChartFrame.create 800. 570. "Reactive regional globe" with Margin={Top=70.;Right=20.;Bottom=40.;Left=20.}}
let private graticule=GeoGeometry.MultiLineString (Graticule.create GraticuleConfig.defaultValue |> ok).Lines

/// <summary>An orthographic globe centred on the negated rotation longitude, fitted to the plot.</summary>
let projection rotation=
    let config={ProjectionConfig.defaultValue ProjectionKind.Orthographic with Rotation=GeoRotation.create rotation -15. 0. |> ok}
    let bounds=ChartLayout.plot frame |> ok
    ProjectionFit.extent GeoPathOptions.defaultValue bounds GeoGeometry.Sphere config |> ok

/// <summary>Pure composition: regions, cities and routes over one globe; no signals are created here.</summary>
let chart (features:MapFeature list) rotation (selected:Set<string>) =
    let map={MapFrame.create frame (MapProjection.Single(projection rotation)) with
                Background=[MapLayer.Sphere(Paint.rgb 226 236 245);MapLayer.Lines("graticule",graticule,Paint.rgb 176 192 207,0.6)]}
    let regions={Map=map;Key=(fun (row:RegionValue) -> row.Region);Value=Encoding.create "Index" (fun row -> Some row.Value);Domain=Fixed(0.,23.)
                 Color=(fun t -> let r,g,b=fill t in Paint.rgb r g b);Missing=Paint.rgb 205 205 205;Stroke=Paint.rgb 255 255 255;Selected=selected}
    let bubbles={Map={map with Background=[]};Key=(fun (row:City) -> row.Name);Position=(fun row -> row.Position);Size=Encoding.create "Population (millions)" (fun row -> row.Population)
                 MaxRadius=16.;Color=(fun _ -> Paint.rgb 30 40 60);Selected=selected}
    let routes={Map={map with Background=[]};Key=(fun (name,_,_) -> name);Origin=(fun (_,a,_) -> (city a).Position);Destination=(fun (_,_,b) -> (city b).Position)
                Width=2.;Color=(fun _ -> Paint.rgb 214 72 48);Selected=selected}
    MapChart.choropleth regions features values |> Result.bind(fun regions ->
    MapChart.routes routes flights |> Result.bind(fun routes ->
    MapChart.bubbles bubbles cities |> Result.bind(fun bubbles ->
    MapChart.overlay ["regions",regions;"routes",routes;"cities",bubbles])))

/// <summary>Loads regions on the owned worker, rotates with a signal and selects through native input.</summary>
let mount (host:WebCoreHost) (graph:Graph) =
    graph.Run(fun () -> Api.createRoot(fun owner ->
        let rotation=Api.createSignal 100.
        let selected=Api.createSignal Set.empty
        let status=Api.createSignal "Loading regions…"
        let path=Api.createSignal regionsPath
        let loader=ReactiveLayout.create (Ranvier.D5.Examples.HostDispatcher.forHost host) (fun () -> path.Value) (fun token file ->
            token.ThrowIfCancellationRequested()
            GeoJson.parseWithCancellation GeoJsonConfig.defaultValue token (File.ReadAllText file)
            |> Result.bind(MapFeature.ofDocument(fun feature ->
                feature.Properties |> Option.bind(Map.tryFind "name") |> Option.bind(function GeoValue.String name -> Some name | _ -> None))))
        let empty={Scene=Scene.create 800. 570. frame.Title [];PlotBounds=ChartLayout.plot frame |> ok;Semantics=[];HitTargets=[];Description=frame.Title}
        let output=Reactive.derive (fun () -> loader.LastGood,rotation.Value,selected.Value) (fun (features,angle,chosen) ->
            match features with
            | None -> empty
            | Some features -> chart features angle chosen |> Result.defaultValue empty)
        let scene()=
            let chart=output.Value
            Scene.compose frame.Viewport frame.Title (Scene.definitions chart.Scene)
                (Scene.nodes chart.Scene @
                 [Leaf(Mark.text "title" (Point.create 20. 30.) "Regions, cities and routes on a reactive globe" |> Mark.withFontSize 22.)
                  Leaf(Mark.text "help" (Point.create 20. 54.) "Hover or arrows explore; Enter selects; + / - rotates. Regions load on the owned worker." |> Mark.withFontSize 13.)
                  Leaf(Mark.text "status" (Point.create 20. 556.) status.Value |> Mark.withFontSize 13.)])
        let container=host.Document.createElement "div"
        host.Document.body.appendChild container |> ignore
        let mounted=Mounting.mount graph None (fun errors -> raise(AggregateException errors)) container None
                        (fun context -> Program.run (Component.render Renderer.passive scene) context.Backend)
        Api.onCleanup(fun () -> mounted.Dispose();container.remove())
        host.Cycle()
        let svg=container.querySelector "svg" :?> Ranvier.Dom.SVGGraphicsElement
        svg.setAttribute("tabindex","0")
        let input=ChartInteraction.attach svg (fun () -> {Mode=GestureMode.Rectangle;MinZoom=1.;MaxZoom=1.;WheelSensitivity=0.01;MaxLassoPoints=4}) (fun () -> output.Value) ignore
        Api.createEffect(fun () ->
            let state=input.Selection
            selected.Value<-state.Selected
            let active=state.Hovered |> Option.orElse state.Focused
            let description=
                active |> Option.bind(fun id -> output.Value.Semantics |> List.tryFind(fun semantic -> semantic.DataIds=[id]))
                |> Option.map(fun semantic -> semantic.Description) |> Option.defaultValue "Nothing active"
            status.Value<-sprintf "%s | %d selected | rotation %.0f°" description state.Selected.Count rotation.Value) |> ignore
        let key (event:Ranvier.Dom.Event)=
            let value=(event :?> Ranvier.Dom.KeyboardEvent).key
            if value="+" || value="-" then
                event.preventDefault()
                graph.Run(fun () -> rotation.Value<-rotation.Value+(if value="+" then 10. else -10.))
        svg.addEventListener("keydown",key)
        Api.onCleanup(fun () -> svg.removeEventListener("keydown",key))
        owner,rotation,loader,input,(fun () -> output.Value)))

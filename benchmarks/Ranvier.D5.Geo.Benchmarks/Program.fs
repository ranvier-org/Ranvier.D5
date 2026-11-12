module Ranvier.D5.Geo.Benchmarks.Program

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open Ranvier.D5.Geo

type Measurement = { Name:string; MedianMs:float; P95Ms:float; MedianBytes:int64; LimitMs:float; LimitBytes:int64 }

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private measure name limit bytes action =
    for _ in 1..3 do action()
    let samples=Array.init 60 (fun _ ->
        let before=GC.GetAllocatedBytesForCurrentThread()
        let watch=Stopwatch.StartNew()
        action()
        watch.Stop()
        watch.Elapsed.TotalMilliseconds,GC.GetAllocatedBytesForCurrentThread()-before)
    let times=samples |> Array.map fst |> Array.sort
    let allocated=samples |> Array.map snd |> Array.sort
    let result={Name=name;MedianMs=(times[29]+times[30])/2.;P95Ms=times[56];MedianBytes=allocated[30];LimitMs=limit;LimitBytes=bytes}
    printfn "%s: median %.3f ms, p95 %.3f ms, %d bytes" name result.MedianMs result.P95Ms result.MedianBytes
    result

[<EntryPoint>]
let main args =
    let p x y=GeoPosition.create x y |> ok
    let config=GeoStreamConfig.defaultValue
    let points=[for i in 0..99999 -> p (float((i*7919)%36000)/100.-180.) (float(i%170)-85.)]
    let geometry=GeoGeometry.MultiPoint points
    let line=GeoGeometry.LineString[for i in 0..99999 -> p (float i/1000.) 0.]
    let ring=[p 0. 0.;p 0. 90.;p 90. 0.;p 0. 0.]
    let polygons=GeoGeometry.MultiPolygon(List.replicate 10000 [ring])
    let document=GeoDocument.Geometry(GeoGeometry.MultiPoint(List.take 10000 points))
    let json=GeoJson.serialize document |> ok
    let projections =
        [ProjectionKind.Equirectangular;ProjectionKind.Mercator;ProjectionKind.Orthographic]
        |> List.map (fun kind -> kind,Projection.create (ProjectionConfig.defaultValue kind) |> ok)
    let projectionResults =
        projections |> List.map (fun (kind,projection) ->
            measure (sprintf "geo-project-%A-100000" kind) 50. 4000000L (fun () ->
                let mutable count=0
                let mutable checksum=0.
                for point in points do
                    match Projection.project projection point with
                    | Some screen -> count<-count+1; checksum<-checksum+screen.X+screen.Y
                    | None -> ()
                if count=0 || not(Double.IsFinite checksum) then failwith "Projection output"))
    let pointProjection=Projection.create (ProjectionConfig.defaultValue ProjectionKind.Equirectangular) |> ok
    let pointGeometry=GeoGeometry.MultiPoint(List.take 10000 points)
    let clippedPointGeometry=GeoGeometry.MultiPoint(List.replicate 1000 (p 0. 0.))
    let clippedPointOptions={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Ranvier.D5.Bounds.create 0. -3. 3. 6.)}
    let seamLine=GeoGeometry.LineString[for index in 0..99999 -> p (if index%2=0 then 170. else -170.) 10.]
    let hemisphereProjection=Projection.create (ProjectionConfig.defaultValue ProjectionKind.Orthographic) |> ok
    let hemisphereLine=GeoGeometry.LineString[for index in 0..9999 -> p (if index%2=0 then 0. else 120.) 0.]
    let rectangleLine=GeoGeometry.LineString[for index in 0..9999 -> p (if index%2=0 then -60. else 60.) 0.]
    let rectangleOptions={GeoPathOptions.defaultValue with ClipExtent=Some(Ranvier.D5.Bounds.create -0.5 -0.5 1. 1.)}
    let spheres=GeoGeometry.Collection(List.replicate 10000 GeoGeometry.Sphere)
    let clippedSpheres=GeoGeometry.Collection(List.replicate 1000 GeoGeometry.Sphere)
    let sphereOptions={GeoPathOptions.defaultValue with ClipExtent=Some(Ranvier.D5.Bounds.create -0.25 -0.25 0.5 0.5);Precision=0.01}
    let results=[|
        measure "geo-sphere-boundaries-10000" 50. 6000000L (fun () ->
            let paths=GeoPath.project GeoPathOptions.defaultValue pointProjection spheres |> ok
            if paths.Length<>10000 then failwith "World boundary count")
        measure "geo-clipped-sphere-boundaries-1000" 50. 2000000L (fun () ->
            let paths=GeoPath.project sphereOptions hemisphereProjection clippedSpheres |> ok
            if paths.Length<>1000 then failwith "Clipped globe count")
        measure "geo-rectangle-line-path-10000" 100. 5000000L (fun () ->
            let paths=GeoPath.project rectangleOptions pointProjection rectangleLine |> ok
            if paths.Length<>9999 then failwith "Rectangle fragment count")
        measure "geo-hemisphere-path-10000" 100. 6000000L (fun () ->
            let paths=GeoPath.project GeoPathOptions.defaultValue hemisphereProjection hemisphereLine |> ok
            let commands=paths |> List.sumBy(fun path -> Ranvier.D5.Path.commands path |> List.length)
            if paths.Length<>5000 || commands<>44996 then failwithf "Hemisphere output: %d paths, %d commands" paths.Length commands)
        measure "geo-seam-path-100000" 250. 40000000L (fun () ->
            let paths=GeoPath.project GeoPathOptions.defaultValue pointProjection seamLine |> ok
            if paths.Length<>100000 || (paths |> List.sumBy(fun path -> Ranvier.D5.Path.commands path |> List.length))<>299998 then failwith "Seam fragment count")
        measure "geo-line-path-100000" 100. 16000000L (fun () ->
            let paths=GeoPath.project GeoPathOptions.defaultValue pointProjection line |> ok
            if paths.Length<>1 || (Ranvier.D5.Path.commands paths.Head |> List.length)<>100000 then failwith "Line path count")
        measure "geo-clipped-point-paths-1000" 50. 4000000L (fun () ->
            let paths=GeoPath.project clippedPointOptions pointProjection clippedPointGeometry |> ok
            if paths.Length<>1000 then failwith "Clipped point path count")
        measure "geo-point-paths-10000" 50. 8000000L (fun () ->
            let paths=GeoPath.project GeoPathOptions.defaultValue pointProjection pointGeometry |> ok
            if paths.Length<>10000 then failwith "Point path count")
        measure "geo-stream-100000" 10. 65536L (fun () ->
            let count=GeoStream.fold config (fun count event -> match event with GeoStreamEvent.Point _ -> count+1 | _ -> count) 0 geometry |> ok
            if count<>100000 then failwith "Stream count")
        measure "geo-length-100000" 50. 65536L (fun () ->
            let value=GeoMeasure.length config line |> ok
            if abs(value-99.999*Math.PI/180.)>1e-10 then failwith "Line length")
        measure "geo-area-10000-octants" 25. 65536L (fun () ->
            let value=GeoMeasure.area config polygons |> ok
            if abs(value-5000.*Math.PI)>1e-8 then failwith "Polygon area")
        measure "geo-centroid-10000-octants" 25. 65536L (fun () ->
            let value=GeoMeasure.centroid config polygons |> ok |> Option.get
            if Geodesic.distance value (p 45. (atan(1./sqrt 2.)*180./Math.PI))>1e-12 then failwith "Polygon centroid")
        measure "geo-bounds-100000" 100. 8000000L (fun () ->
            let value=GeoMeasure.bounds config geometry |> ok |> Option.get
            if value.South <> -85. || value.North<>84. then failwith "Spherical bounds")
        measure "geo-graticule-default" 5. 256000L (fun () ->
            let value=Graticule.create GraticuleConfig.defaultValue |> ok
            if value.Lines.Length<>53 then failwith "Graticule lines")
        measure "geojson-read-10000" 100. 8000000L (fun () -> GeoJson.parse json |> ok |> ignore)
        measure "geojson-write-10000" 100. 8000000L (fun () -> GeoJson.serialize document |> ok |> ignore)
    |]
    let results=Array.append results (List.toArray projectionResults)
    let output=if args.Length=0 then "artifacts/geography-benchmarks.json" else args[0]
    let directory=Path.GetDirectoryName(Path.GetFullPath output)
    Directory.CreateDirectory directory |> ignore
    File.WriteAllText(output,JsonSerializer.Serialize(results,JsonSerializerOptions(WriteIndented=true)))
    if results |> Array.exists(fun value -> value.P95Ms>value.LimitMs || value.MedianBytes>value.LimitBytes) then 1 else 0

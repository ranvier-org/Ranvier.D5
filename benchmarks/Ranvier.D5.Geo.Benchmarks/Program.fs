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
        // Collect outside the timed region so one sample's garbage is not charged to the next.
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()
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
    let polygonProjection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=100.} |> ok
    let globeProjection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with Scale=100.} |> ok
    let polygonOptions={GeoPathOptions.defaultValue with Precision=0.1}
    let outer=[p -10. -10.;p -10. 10.;p 10. 10.;p 10. -10.;p -10. -10.]
    let hole=[p -3. -3.;p 3. -3.;p 3. 3.;p -3. 3.;p -3. -3.]
    let seamOuter=[p 170. -20.;p 170. 20.;p -170. 20.;p -170. -20.;p 170. -20.]
    let seamHole=[p 175. -5.;p -175. -5.;p -175. 5.;p 175. 5.;p 175. -5.]
    let mercatorProjection=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Mercator with Scale=100.} |> ok
    let polarRing=[p -30. 80.;p -30. 89.;p 30. 89.;p 30. 80.;p -30. 80.]
    let polarLine=GeoGeometry.LineString[for index in 0..9999 -> p 25. (if index%2=0 then -89. else 89.)]
    let familyProjection kind=Projection.create {ProjectionConfig.defaultValue kind with Scale=100.} |> ok
    let wideRing=[p -75. -35.;p -75. 45.;p 75. 45.;p 75. -35.;p -75. -35.]
    let equatorialPoleRing=[p 80. -5.;p 80. 5.;p 100. 5.;p 100. -5.;p 80. -5.]
    let polygonCases=[
        "geo-azimuthal-equal-area-seam-holes-1000",550.,58000000L,familyProjection ProjectionKind.AzimuthalEqualArea,GeoGeometry.MultiPolygon(List.replicate 1000 [seamOuter;seamHole])
        "geo-gnomonic-cap-polygons-1000",60.,21000000L,familyProjection ProjectionKind.Gnomonic,GeoGeometry.MultiPolygon(List.replicate 1000 [wideRing])
        "geo-conic-seam-holes-1000",50.,11000000L,familyProjection(ProjectionKind.ConicEqualArea(29.5,45.5)),GeoGeometry.MultiPolygon(List.replicate 1000 [seamOuter;seamHole])
        "geo-transverse-mercator-pole-polygons-1000",250.,36000000L,familyProjection ProjectionKind.TransverseMercator,GeoGeometry.MultiPolygon(List.replicate 1000 [equatorialPoleRing])
        "geo-mercator-polar-polygons-1000",75.,12000000L,mercatorProjection,GeoGeometry.MultiPolygon(List.replicate 1000 [polarRing])
        "geo-polygons-holes-1000",50.,5000000L,polygonProjection,GeoGeometry.MultiPolygon(List.replicate 1000 [outer;hole])
        "geo-polygons-seam-holes-1000",75.,9000000L,polygonProjection,GeoGeometry.MultiPolygon(List.replicate 1000 [seamOuter;seamHole])
        "geo-polygons-globe-complements-1000",100.,20000000L,globeProjection,GeoGeometry.MultiPolygon(List.replicate 1000 [List.rev outer])
    ]
    let polygonResults=polygonCases |> List.map(fun (name,limit,bytes,projection,geometry) ->
        measure name limit bytes (fun () ->
            let paths=GeoPath.project polygonOptions projection geometry |> ok
            if paths.Length<>1000 then failwith "Polygon path ownership"))
    let denseRing=[for index in 0..99999 do
                       let angle= -float index*2.*Math.PI/100000.
                       yield p (10.*cos angle) (10.*sin angle)
                   yield p 10. 0.]
    let densePolygon=GeoGeometry.Polygon[denseRing]
    let denseClipOptions={polygonOptions with ClipExtent=Some(Ranvier.D5.Bounds.create -5. -20. 10. 40.)}
    let concaveRing=[p -40. -40.;p -40. 40.;p -20. 40.;p -20. -20.;p 20. -20.;p 20. 40.;p 40. 40.;p 40. -40.;p -40. -40.]
    let concavePolygons=GeoGeometry.MultiPolygon(List.replicate 1000 [concaveRing])
    let radians=100.*Math.PI/180.
    let concaveOptions={polygonOptions with ClipExtent=Some(Ranvier.D5.Bounds.create (-50.*radians) (-15.*radians) (100.*radians) (10.*radians))}
    let planarProjection=IdentityProjection.create IdentityProjectionConfig.defaultValue |> ok
    let planarLine=PlanarGeometry.LineString[for index in 0..99999 -> Ranvier.D5.Point.create (float index) (float(index%2))]
    let planarPolygon=PlanarGeometry.Polygon[denseRing |> List.map(fun point -> Ranvier.D5.Point.create (point.Longitude*10.) (point.Latitude*10.))]
    let planarPoints=PlanarGeometry.MultiPoint(List.replicate 1000 (Ranvier.D5.Point.create 0. 0.))
    let states=GeoGeometry.MultiPolygon[for index in 0..999 ->
                                            let longitude,latitude=(if index%3=0 then -150.+float(index%7),62. elif index%3=1 then -157.+float(index%5)*0.3,20. else -120.+float(index%50),30.+float(index%15))
                                            [[p longitude latitude;p longitude (latitude+0.5);p (longitude+0.5) (latitude+0.5);p (longitude+0.5) latitude;p longitude latitude]]]
    let usa=CompositeProjection.albersUsa 1070. (Ranvier.D5.Point.create 480. 250.) |> ok
    let fitConfig=ProjectionConfig.defaultValue(ProjectionKind.ConicConformal(20.,50.))
    let results=[|
        measure "albers-usa-paths-1000" 100. 13000000L (fun () ->
            let paths=CompositeProjection.paths polygonOptions usa states |> ok
            if paths.Length<>1000 then failwith "Albers USA inset ownership")
        measure "projection-fit-sphere-conic" 25. 2000000L (fun () ->
            let fitted=ProjectionFit.size polygonOptions 960. 500. GeoGeometry.Sphere fitConfig |> ok
            if not(Double.IsFinite (Projection.config fitted).Scale) then failwith "Fitted scale")
        measure "identity-line-100000" 50. 12000000L (fun () ->
            let paths=PlanarPath.project GeoPathOptions.defaultValue planarProjection planarLine |> ok
            if paths.Length<>1 || (Ranvier.D5.Path.commands paths.Head |> List.length)<>100000 then failwith "Identity line output")
        measure "identity-clipped-polygon-100000" 50. 8000000L (fun () ->
            let paths=PlanarPath.project denseClipOptions planarProjection planarPolygon |> ok
            if paths.Length<>1 then failwith "Identity clipped polygon output")
        measure "identity-clipped-point-paths-1000" 50. 2000000L (fun () ->
            let paths=PlanarPath.project clippedPointOptions planarProjection planarPoints |> ok
            if paths.Length<>1000 then failwith "Identity clipped point output")
        measure "geo-mercator-line-100000" 100. 16000000L (fun () ->
            let paths=GeoPath.project GeoPathOptions.defaultValue mercatorProjection line |> ok
            if paths.Length<>1 || (Ranvier.D5.Path.commands paths.Head |> List.length)<>100000 then failwith "Mercator ordinary line count")
        measure "geo-mercator-polar-line-10000" 150. 16000000L (fun () ->
            let paths=GeoPath.project polygonOptions mercatorProjection polarLine |> ok
            if paths.Length<>9999 then failwith "Mercator polar fragment count")
        measure "geo-rectangle-polygon-100000" 150. 28000000L (fun () ->
            let paths=GeoPath.project denseClipOptions polygonProjection densePolygon |> ok
            if paths.Length<>1 then failwith "Dense clipped polygon count")
        measure "geo-rectangle-concave-polygons-1000" 100. 12000000L (fun () ->
            let paths=GeoPath.project concaveOptions polygonProjection concavePolygons |> ok
            let rings=paths |> List.sumBy(fun path -> Ranvier.D5.Path.commands path |> List.sumBy(function Ranvier.D5.PathCommand.Close -> 1 | _ -> 0))
            if paths.Length<>1000 || rings<>2000 then failwith "Disconnected rectangle polygon components")
        measure "geo-polygon-100000" 150. 32000000L (fun () ->
            let paths=GeoPath.project polygonOptions polygonProjection densePolygon |> ok
            if paths.Length<>1 || (Ranvier.D5.Path.commands paths.Head |> List.length)<>100001 then failwith "Dense polygon closure")
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
    let results=Array.append results (List.toArray (projectionResults@polygonResults))
    let output=if args.Length=0 then "artifacts/geography-benchmarks.json" else args[0]
    let directory=Path.GetDirectoryName(Path.GetFullPath output)
    Directory.CreateDirectory directory |> ignore
    File.WriteAllText(output,JsonSerializer.Serialize(results,JsonSerializerOptions(WriteIndented=true)))
    let failures=results |> Array.filter(fun value -> value.P95Ms>value.LimitMs || value.MedianBytes>value.LimitBytes)
    for value in failures do
        printfn "FAIL %s: p95 %.3f ms (limit %.3f); allocation %d bytes (limit %d)" value.Name value.P95Ms value.LimitMs value.MedianBytes value.LimitBytes
    if failures.Length>0 then 1 else 0

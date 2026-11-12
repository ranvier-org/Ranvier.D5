module Ranvier.D5.Tests.GeoPathTests

open System
open System.Threading
open System.Text.Json
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private p longitude latitude = GeoPosition.create longitude latitude |> ok
let private projection kind = Projection.create (ProjectionConfig.defaultValue kind) |> ok

[<Tests>]
let tests = testList "geographic paths" [
    testCase "sphere emits the complete equirectangular boundary" <| fun _ ->
        let paths=GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Equirectangular) GeoGeometry.Sphere |> ok
        Expect.equal paths.Length 1 "one world boundary"
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.equal vertices.Length 4 "rectangle corners"
        Expect.isLessThan (abs(Polygon.area vertices-2.*Math.PI*Math.PI)) 1e-12 "world screen area and winding"
        Expect.equal (Path.commands paths.Head |> List.last) Close "closed boundary"
    testCase "orthographic sphere uses its projection radius independently of point symbols" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with Scale=20.;Translate={X=100.;Y=80.}} |> ok
        let paths=GeoPath.project {GeoPathOptions.defaultValue with PointRadius=0.} projected GeoGeometry.Sphere |> ok
        match Path.commands paths.Head with
        | [MoveTo start;ArcTo(rx,ry,_,false,true,middle);ArcTo(_,_,_,false,true,finish);Close] ->
            Expect.equal (rx,ry) (20.,20.) "projection scale"
            Expect.equal start {X=120.;Y=80.} "translated disk"
            Expect.equal middle {X=80.;Y=80.} "opposite limb"
            Expect.equal finish start "closed circle"
        | commands -> failtestf "Unexpected globe boundary %A" commands
    testCase "mercator sphere has the finite canonical world square" <| fun _ ->
        let paths=GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Mercator) GeoGeometry.Sphere |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.equal vertices.Length 4 "finite world corners"
        Expect.isTrue (vertices |> List.forall(fun point -> abs(abs point.X-Math.PI)<1e-12 && abs(abs point.Y-Math.PI)<1e-12)) "canonical Mercator extent"
    testCase "sphere clipping preserves an enclosed viewport for every projection" <| fun _ ->
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.Mercator;ProjectionKind.Orthographic] do
            let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -0.1 -0.1 0.2 0.2);Precision=0.001}
            let paths=GeoPath.project options (projection kind) GeoGeometry.Sphere |> ok
            let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            Expect.isLessThan (abs(Polygon.area vertices-0.04)) 1e-12 "viewport survives without visible input vertices"
    testCase "sphere clipping can omit a disjoint or empty viewport" <| fun _ ->
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.Mercator;ProjectionKind.Orthographic] do
            for extent in [Bounds.create 10. 10. 1. 1.;Bounds.create 0. 0. 0. 1.] do
                let options={GeoPathOptions.defaultValue with ClipExtent=Some extent;MaxOutputPoints=1}
                Expect.equal (GeoPath.project options (projection kind) GeoGeometry.Sphere |> ok) [] "no phantom output"
    testCase "sphere boundary ignores globe rotation but respects projection center" <| fun _ ->
        let rotation=GeoRotation.create 120. 35. 20. |> ok
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.Mercator;ProjectionKind.Orthographic] do
            let config={ProjectionConfig.defaultValue kind with Center=p 20. 10.;Scale=30.;Translate={X=50.;Y=60.}}
            let unrotated=Projection.create config |> ok
            let rotated=Projection.create {config with Rotation=rotation} |> ok
            let paths projection=GeoPath.project GeoPathOptions.defaultValue projection GeoGeometry.Sphere |> ok |> List.map Path.commands
            Expect.equal (paths rotated) (paths unrotated) "the full sphere has no orientation"
            let origin=Projection.project unrotated (p 0. 0.) |> Option.get
            match paths unrotated |> List.head |> List.head with
            | MoveTo point ->
                let expectedX=if kind=ProjectionKind.Orthographic then origin.X+30. else origin.X-30.*Math.PI
                Expect.equal point.X expectedX "center shifts the projection frame"
            | _ -> failtest "expected boundary start"
    testCase "sphere overflow and clipped precision exhaustion return typed errors" <| fun _ ->
        for kind in [ProjectionKind.Equirectangular;ProjectionKind.Mercator;ProjectionKind.Orthographic] do
            let projected=Projection.create {ProjectionConfig.defaultValue kind with Scale=Double.MaxValue;Translate={X=Double.MaxValue;Y=0.}} |> ok
            Expect.isError (GeoPath.project GeoPathOptions.defaultValue projected GeoGeometry.Sphere) "unrepresentable frame"
        let options={GeoPathOptions.defaultValue with Precision=1e-20;ClipExtent=Some(Bounds.create 0. -2. 2. 4.);MaxBufferedPoints=100}
        match GeoPath.project options (projection ProjectionKind.Orthographic) GeoGeometry.Sphere with
        | Error [BudgetExceeded _] -> ()
        | other -> failtestf "Expected bounded circular clipping, got %A" other
    testCase "sphere output remains budgeted and preserves collection ownership" <| fun _ ->
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=3} (projection ProjectionKind.Equirectangular) GeoGeometry.Sphere) "rectangle requires four endpoints"
        let paths=GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Equirectangular) (GeoGeometry.Collection[GeoGeometry.Sphere;GeoGeometry.Point(Some(p 0. 0.))]) |> ok
        Expect.equal paths.Length 2 "sphere and point are independent owned paths"
    testCase "point paths use existing circle commands and omit hidden points" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.}
        let paths=GeoPath.project options (projection ProjectionKind.Orthographic) (GeoGeometry.MultiPoint[p 0. 0.;p 180. 0.;p 90. 0.]) |> ok
        Expect.equal paths.Length 2 "visible points only"
        match Path.commands paths.Head with
        | [MoveTo start;ArcTo(rx,ry,_,false,true,middle);ArcTo(_,_,_,false,true,finish);Close] ->
            Expect.equal (rx,ry) (2.,2.) "radius in screen units"
            Expect.equal start {X=2.;Y=0.} "right edge"
            Expect.equal middle {X= -2.;Y=0.} "left edge"
            Expect.equal finish start "closed circle"
        | commands -> failtestf "Unexpected point path %A" commands
    testCase "validates positive budgets precision and finite point radius" <| fun _ ->
        let options=GeoPathOptions.defaultValue
        let empty=GeoGeometry.Collection []
        let invalid=[{options with MaxOutputPoints=0};{options with MaxWork=0L};{options with MaxSubdivisionDepth=0};{options with MaxSubdivisionDepth=33};{options with Precision=0.};{options with Precision=nan};{options with PointRadius= -1.};{options with PointRadius=infinity}]
        for value in invalid do
            Expect.isError (GeoPath.project value (projection ProjectionKind.Equirectangular) empty) "invalid policy"
    testCase "bounds all emitted point endpoints and returns no partial success" <| fun _ ->
        let geometry=GeoGeometry.MultiPoint[p 0. 0.;p 10. 0.]
        let projected=projection ProjectionKind.Equirectangular
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=5} projected geometry) "each circle has three emitted endpoints"
        Expect.equal (GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=6} projected geometry |> ok |> List.length) 2 "exact output budget"
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with MaxWork=1L} projected geometry) "bounded work"
    testCase "input budgets cancellation and malformed geometry retain stream contracts" <| fun _ ->
        let options={GeoPathOptions.defaultValue with InputLimits={GeoStreamConfig.defaultValue with MaxPoints=1}}
        Expect.isError (GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.MultiPoint[p 0. 0.;p 1. 0.])) "input point budget"
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> GeoPath.projectWithCancellation GeoPathOptions.defaultValue cancelled.Token (projection ProjectionKind.Equirectangular) (GeoGeometry.Collection []) |> ignore) "pre-cancelled empty input"
    testCase "clips partial point circles within a bounded viewport" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Bounds.create 0. -3. 3. 6.)}
        let paths=GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.Point(Some(p 0. 0.))) |> ok
        Expect.equal paths.Length 1 "half disk"
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isTrue (vertices |> List.forall(fun point -> point.X>=0. && point.X<=3. && point.Y>= -3. && point.Y<=3.)) "inside clip"
        Expect.isLessThan (abs(abs(Polygon.area vertices)-2.*Math.PI)) 0.06 "half circle within requested approximation"
        Expect.equal (Path.commands paths.Head |> List.last) Close "closed clipped symbol"
    testCase "circle clipping preserves a viewport enclosed by the symbol" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=10.;Precision=0.01;ClipExtent=Some(Bounds.create -1. -1. 2. 2.)}
        let paths=GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.Point(Some(p 0. 0.))) |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isLessThan (abs(abs(Polygon.area vertices)-4.)) 1e-12 "whole viewport"
    testCase "clipped paths own their vertices across reused working buffers" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Bounds.create 0. -3. 3. 6.)}
        let positions=[p 0. 0.;p 30. 20.;p -10. -10.]
        let projected=projection ProjectionKind.Equirectangular
        let combined=GeoPath.project options projected (GeoGeometry.MultiPoint positions) |> ok
        let independent=positions |> List.collect(fun point -> GeoPath.project options projected (GeoGeometry.Point(Some point)) |> ok)
        Expect.equal combined independent "later clips never mutate previous output"
    testCase "point clipping bounds tessellation buffers and subdivision precision" <| fun _ ->
        let options={GeoPathOptions.defaultValue with PointRadius=2.;Precision=0.01;ClipExtent=Some(Bounds.create 0. -3. 3. 6.)}
        for limited in [{options with MaxBufferedPoints=8};{options with MaxSubdivisionDepth=1};{options with Precision=1e-300}] do
            match GeoPath.project limited (projection ProjectionKind.Equirectangular) (GeoGeometry.Point(Some(p 0. 0.))) with
            | Error [BudgetExceeded _] -> ()
            | result -> failtestf "Expected bounded tessellation failure, got %A" result
    testCase "projects a continuous equatorial line without point symbols" <| fun _ ->
        let paths=GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString[p 0. 0.;p 10. 0.]) |> ok
        Expect.equal paths.Length 1 "one connected line"
        match Path.commands paths.Head with
        | [MoveTo first;LineTo last] ->
            Expect.equal first {X=0.;Y=0.} "first endpoint"
            Expect.isLessThan (abs(last.X-Math.PI/18.)) 1e-12 "last endpoint"
            Expect.equal last.Y 0. "equator"
        | commands -> failtestf "Expected a line, got %A" commands
    testCase "resamples projected great-circle curvature in screen units" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=100.} |> ok
        let geometry=GeoGeometry.LineString[p -45. 60.;p 45. 60.]
        let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=0.1} projected geometry |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isGreaterThan vertices.Length 2 "curved arc is subdivided"
        let north=atan(tan(Math.PI/3.)/cos(Math.PI/4.))
        Expect.isLessThan (abs((vertices |> List.minBy(fun point -> point.Y)).Y + 100.*north)) 1e-10 "great-circle midpoint"
        Expect.isFalse (Path.commands paths.Head |> List.contains Close) "line remains open"
    testCase "resampling detects symmetric curvature that a midpoint misses" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=10000.} |> ok
        let first,last=p -5. -5.,p 5. 5.
        let options={GeoPathOptions.defaultValue with Precision=0.01}
        let paths=GeoPath.project options projected (GeoGeometry.LineString[first;last]) |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isGreaterThan vertices.Length 2 "symmetric arc still has curvature"
        let segments=List.pairwise vertices
        let distance (point:Point) (a:Point,b:Point) =
            let dx,dy=b.X-a.X,b.Y-a.Y
            let parameter=Math.Clamp(((point.X-a.X)*dx+(point.Y-a.Y)*dy)/(dx*dx+dy*dy),0.,1.)
            Double.Hypot(point.X-(a.X+parameter*dx),point.Y-(a.Y+parameter*dy))
        for index=0 to 100 do
            let point=Geodesic.interpolate (float index/100.) first last |> ok |> Projection.project projected |> Option.get
            Expect.isLessThan (segments |> List.map(distance point) |> List.min) 0.0101 "dense reference samples meet screen tolerance"
    testCase "line resampling preserves tiny and nearly antipodal unique arcs" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular
        let options={GeoPathOptions.defaultValue with Precision=1e-5;MaxOutputPoints=10000}
        let paths=GeoPath.project options projected (GeoGeometry.LineString[p 0. 0.;p 179.999 0.001]) |> ok
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isGreaterThan vertices.Length 2 "nearly antipodal arc is unique and subdivided"
        Expect.isTrue (vertices |> List.forall(fun point -> Double.IsFinite point.X && Double.IsFinite point.Y)) "finite output"
        Expect.equal vertices.Head (Projection.project projected (p 0. 0.) |> Option.get) "first endpoint"
        Expect.equal (List.last vertices) (Projection.project projected (p 179.999 0.001) |> Option.get) "last endpoint"
        let tiny=GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString[p 0. 0.;p 1e-200 1e-200]) |> ok
        match Path.commands tiny.Head with
        | [MoveTo _;LineTo point] -> Expect.isTrue (point.X>0. && point.Y<0.) "tiny endpoint survives"
        | commands -> failtestf "Unexpected tiny arc %A" commands
    testCase "resampling rejects unmet precision and exhausted output or buffer budgets" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=100.} |> ok
        let geometry=GeoGeometry.LineString[p -45. 60.;p 45. 60.]
        for options in [{GeoPathOptions.defaultValue with Precision=1e-10;MaxSubdivisionDepth=1};{GeoPathOptions.defaultValue with MaxOutputPoints=2};{GeoPathOptions.defaultValue with MaxBufferedPoints=2}] do
            match GeoPath.project options projected geometry with
            | Error [BudgetExceeded _] -> ()
            | result -> failtestf "Expected bounded resampling failure, got %A" result
    testCase "seam stream endpoints and topology match pinned reference fixtures" <| fun _ ->
        use json=JsonDocument.Parse(System.IO.File.ReadAllText(System.IO.Path.Combine(AppContext.BaseDirectory,"fixtures","seam-streams.json")))
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            let kind=if item.GetProperty("kind").GetString()="Mercator" then ProjectionKind.Mercator else ProjectionKind.Equirectangular
            let angles=item.GetProperty("rotate")
            let rotation=GeoRotation.create (angles[0].GetDouble()) (angles[1].GetDouble()) (angles[2].GetDouble()) |> ok
            let projected=Projection.create {ProjectionConfig.defaultValue kind with Scale=item.GetProperty("scale").GetDouble();Rotation=rotation} |> ok
            let positions=item.GetProperty("points").EnumerateArray() |> Seq.map(fun value -> p (value[0].GetDouble()) (value[1].GetDouble())) |> Seq.toList
            let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=item.GetProperty("precision").GetDouble()} projected (GeoGeometry.LineString positions) |> ok
            let expected=item.GetProperty("fragments").EnumerateArray() |> Seq.toArray
            Expect.equal paths.Length expected.Length (item.GetProperty("name").GetString())
            for index=0 to paths.Length-1 do
                let vertices=Path.commands paths[index] |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
                let reference=expected[index]
                for actual,target in [vertices.Head,reference[0];List.last vertices,reference[reference.GetArrayLength()-1]] do
                    Expect.isLessThan (Double.Hypot(actual.X-target[0].GetDouble(),actual.Y-target[1].GetDouble())) 1e-9 "fragment endpoint"
    testCase "splits antimeridian arcs without a cross-map connector" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular
        for first,last,sign in [p 170. 10.,p -170. 10.,1.;p -170. 10.,p 170. 10.,-1.] do
            let paths=GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString[first;last]) |> ok
            Expect.equal paths.Length 2 "two seam fragments"
            let vertices path=Path.commands path |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            let before,after=vertices paths[0],vertices paths[1]
            let latitude=atan(tan(Math.PI/18.)/cos(Math.PI/18.))
            Expect.isLessThan (abs((List.last before).X-sign*Math.PI)) 1e-12 "outgoing seam"
            Expect.isLessThan (abs(after.Head.X+sign*Math.PI)) 1e-12 "incoming seam"
            Expect.isLessThan (abs(after.Head.Y+latitude)) 1e-12 "great-circle intersection latitude"
            for fragment in [before;after] do
                Expect.isTrue (List.pairwise fragment |> List.forall(fun (a,b) -> abs(a.X-b.X)<Math.PI)) "no world-spanning segment"
    testCase "seam endpoints do not create phantom fragments or consume phantom output" <| fun _ ->
        let options={GeoPathOptions.defaultValue with MaxOutputPoints=2}
        let projected=projection ProjectionKind.Equirectangular
        for positions in [[p 180. 10.;p 170. 15.];[p 170. 15.;p 180. 10.]] do
            let paths=GeoPath.project options projected (GeoGeometry.LineString positions) |> ok
            Expect.equal paths.Length 1 "one nonempty fragment"
            let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            Expect.equal vertices.Length 2 "only real endpoints count"
            Expect.isTrue (vertices |> List.exists(fun point -> point.X=Math.PI)) "positive seam representation"
    testCase "seam touches remain connected while seam-vertex crossings split" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular
        let touches=GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=3} projected (GeoGeometry.LineString[p 170. 10.;p 180. 10.;p 170. 15.]) |> ok
        Expect.equal touches.Length 1 "touching the seam does not break the stroke"
        let crossing=GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=4} projected (GeoGeometry.LineString[p 170. 10.;p 180. 10.;p -170. 10.]) |> ok
        Expect.equal crossing.Length 2 "a true crossing at an existing vertex splits"
    testCase "runs along the seam preserve the incident side" <| fun _ ->
        let paths=GeoPath.project {GeoPathOptions.defaultValue with MaxOutputPoints=4} (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString[p 170. 10.;p 180. 10.;p 180. 20.;p 170. 25.]) |> ok
        Expect.equal paths.Length 1 "one connected seam run"
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isTrue (vertices |> List.forall(fun point -> point.X>0.)) "run stays on positive seam"
    testCase "pole-crossing lines follow the map boundary and preserve pole aliases" <| fun _ ->
        let projected=projection ProjectionKind.Equirectangular
        let paths=GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString[p -90. 60.;p 90. 60.]) |> ok
        Expect.equal paths.Length 2 "pole crossing split"
        for path in paths do
            let vertices=Path.commands path |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            Expect.isTrue (vertices |> List.exists(fun point -> point.Y= -Math.PI/2.)) "north boundary included"
            Expect.isTrue (vertices |> List.forall(fun point -> abs point.X<=Math.PI && abs point.Y<=Math.PI/2.)) "bounded world"
        let alias=GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString[p 123. 90.;p 170. 80.]) |> ok
        let vertices=Path.commands alias.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.isTrue (vertices |> List.forall(fun point -> abs(point.X-170.*Math.PI/180.)<1e-12)) "pole representation follows the incident meridian"
    testCase "preclipping is applied after spherical rotation" <| fun _ ->
        let rotation=GeoRotation.create 180. 0. 0. |> ok
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Rotation=rotation} |> ok
        let paths=GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString[p -10. 10.;p 10. 10.]) |> ok
        Expect.equal paths.Length 2 "rotation moves the arc across the seam"
    testCase "clips orthographic line entry and exit at the exact limb" <| fun _ ->
        let projected=projection ProjectionKind.Orthographic
        for positions,expectedFirst,expectedLast in [
            [p 0. 0.;p 120. 0.],{X=0.;Y=0.},{X=1.;Y=0.}
            [p 120. 0.;p 0. 0.],{X=1.;Y=0.},{X=0.;Y=0.}
        ] do
            let paths=GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString positions) |> ok
            Expect.equal paths.Length 1 "one visible fragment"
            let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            Expect.isLessThan (Point.distanceSquared vertices.Head expectedFirst) 1e-24 "first endpoint"
            Expect.isLessThan (Point.distanceSquared (List.last vertices) expectedLast) 1e-24 "last endpoint"
            Expect.isTrue (vertices |> List.forall(fun point -> Double.Hypot(point.X,point.Y)<=1.+1e-14)) "inside the projected globe"
    testCase "hidden orthographic arcs and boundary-only touches produce no fragment" <| fun _ ->
        let projected=projection ProjectionKind.Orthographic
        for positions in [[p 120. 0.;p 170. 20.];[p 90. 0.;p 100. 0.];[p 100. 0.;p 90. 0.]] do
            Expect.equal (GeoPath.project GeoPathOptions.defaultValue projected (GeoGeometry.LineString positions) |> ok) [] "no visible line extent"
    testCase "orthographic reentry separates visible fragments without a connector" <| fun _ ->
        let paths=GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Orthographic) (GeoGeometry.LineString[p -20. 0.;p 120. 0.;p 160. 20.;p 20. 20.]) |> ok
        Expect.equal paths.Length 2 "two visible runs"
        for path in paths do
            let vertices=Path.commands path |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            Expect.isTrue (vertices |> List.forall(fun point -> Double.Hypot(point.X,point.Y)<=1.+1e-14)) "hidden segments omitted"
    testCase "orthographic clipping respects rotation and keeps pole vertices connected" <| fun _ ->
        let rotation=GeoRotation.create 180. 0. 0. |> ok
        let rotated=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Orthographic with Rotation=rotation} |> ok
        Expect.equal (GeoPath.project GeoPathOptions.defaultValue rotated (GeoGeometry.LineString[p 0. 0.;p 60. 0.]) |> ok) [] "rotated hidden hemisphere"
        let paths=GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Orthographic) (GeoGeometry.LineString[p -90. 60.;p 0. 90.;p 90. 60.]) |> ok
        Expect.equal paths.Length 1 "pole aliases do not split a continuous visible arc"
    testCase "orthographic near-limb and oblique crossings remain bounded" <| fun _ ->
        let projected=projection ProjectionKind.Orthographic
        for start,finish in [
            p (90.-1e-10) 20.,p (90.+1e-10) 20.
            p -30. -40.,p 120. 60.
            p 30. 40.,p -120. -60.
        ] do
            for first,last in [start,finish;finish,start] do
                let paths=GeoPath.project {GeoPathOptions.defaultValue with Precision=0.001} projected (GeoGeometry.LineString[first;last]) |> ok
                Expect.equal paths.Length 1 "one visible portion"
                let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
                Expect.isTrue (vertices |> List.forall(fun point -> Double.IsFinite point.X && Double.IsFinite point.Y && Double.Hypot(point.X,point.Y)<=1.+1e-14)) "finite inside-disk vertices"
                let boundary=if first= start then List.last vertices else vertices.Head
                Expect.isLessThan (abs(Double.Hypot(boundary.X,boundary.Y)-1.)) 1e-12 "intersection on limb"
    testCase "orthographic clipping rejects an ambiguous antipodal route" <| fun _ ->
        Expect.isError (GeoPath.project GeoPathOptions.defaultValue (projection ProjectionKind.Orthographic) (GeoGeometry.LineString[p 0. 0.;p 180. 0.])) "route needs a waypoint"
    testCase "hemisphere and rectangle fragments match pinned stream references" <| fun _ ->
        use json=JsonDocument.Parse(System.IO.File.ReadAllText(System.IO.Path.Combine(AppContext.BaseDirectory,"fixtures","clipped-streams.json")))
        for item in json.RootElement.GetProperty("cases").EnumerateArray() do
            let kind=if item.GetProperty("kind").GetString()="Orthographic" then ProjectionKind.Orthographic else ProjectionKind.Equirectangular
            let projected=Projection.create {ProjectionConfig.defaultValue kind with Scale=item.GetProperty("scale").GetDouble()} |> ok
            let positions=item.GetProperty("points").EnumerateArray() |> Seq.map(fun value -> p (value[0].GetDouble()) (value[1].GetDouble())) |> Seq.toList
            let extent =
                match item.TryGetProperty("clip") with
                | true,value ->
                    let lower,upper=value[0],value[1]
                    Some(Bounds.create (lower[0].GetDouble()) (lower[1].GetDouble()) (upper[0].GetDouble()-lower[0].GetDouble()) (upper[1].GetDouble()-lower[1].GetDouble()))
                | _ -> None
            let options={GeoPathOptions.defaultValue with Precision=item.GetProperty("precision").GetDouble();ClipExtent=extent}
            let paths=GeoPath.project options projected (GeoGeometry.LineString positions) |> ok
            let expected=item.GetProperty("fragments").EnumerateArray() |> Seq.toArray
            Expect.equal paths.Length expected.Length (item.GetProperty("name").GetString())
            for index=0 to paths.Length-1 do
                let vertices=Path.commands paths[index] |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
                let reference=expected[index]
                for actual,target in [vertices.Head,reference[0];List.last vertices,reference[reference.GetArrayLength()-1]] do
                    Expect.isLessThan (Double.Hypot(actual.X-target[0].GetDouble(),actual.Y-target[1].GetDouble())) (item.GetProperty("tolerance").GetDouble()) "fragment endpoint within combined precision"
    testCase "rectangle clips line crossings with exact boundary endpoints" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -0.5 -0.25 1. 0.5)}
        let projected=projection ProjectionKind.Equirectangular
        for positions in [[p -60. 0.;p 60. 0.];[p 60. 0.;p -60. 0.]] do
            let paths=GeoPath.project options projected (GeoGeometry.LineString positions) |> ok
            Expect.equal paths.Length 1 "one crossing"
            let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
            Expect.equal (abs vertices.Head.X) 0.5 "entry boundary"
            Expect.equal (abs (List.last vertices).X) 0.5 "exit boundary"
            Expect.isTrue (vertices |> List.forall(fun point -> abs point.X<=0.5 && abs point.Y<=0.25)) "inside extent"
    testCase "rectangle separates exits and reentries without an outside connector" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -0.5 -0.5 1. 1.)}
        let paths=GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString[p 0. 0.;p 60. 0.;p 60. 10.;p 0. 10.]) |> ok
        Expect.equal paths.Length 2 "outside run breaks the path"
    testCase "rectangle omits invisible lines and zero-area extents" <| fun _ ->
        for extent,positions in [
            Bounds.create -0.5 -0.5 1. 1.,[p 50. 0.;p 60. 0.]
            Bounds.create 0. -1. 0. 2.,[p -60. 0.;p 60. 0.]
            Bounds.create -1. 0. 2. 0.,[p -60. 0.;p 60. 0.]
        ] do
            let options={GeoPathOptions.defaultValue with ClipExtent=Some extent;MaxOutputPoints=1}
            Expect.equal (GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString positions) |> ok) [] "no output or phantom budget charges"
    testCase "rectangle retains a line along its boundary" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create 0. -1. 1. 2.)}
        let paths=GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString[p 0. -30.;p 0. 30.]) |> ok
        Expect.equal paths.Length 1 "boundary stroke retained"
        Expect.isTrue (Path.commands paths.Head |> List.forall(function MoveTo point | LineTo point -> point.X=0. | _ -> false)) "boundary coordinates"
    testCase "rectangle clipping follows hemisphere clipping and resampling" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -0.5 -0.5 1. 1.);Precision=0.001}
        let paths=GeoPath.project options (projection ProjectionKind.Orthographic) (GeoGeometry.LineString[p 0. 0.;p 120. 0.]) |> ok
        Expect.equal paths.Length 1 "visible part intersects viewport"
        match Path.commands paths.Head |> List.last with
        | LineTo point -> Expect.equal point {X=0.5;Y=0.} "screen clip after hemisphere clip"
        | _ -> failtest "expected line endpoint"
    testCase "rectangle duplicate vertices do not break a visible stroke" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -1. -1. 2. 2.)}
        let paths=GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString[p -10. 0.;p 0. 0.;p 0. 0.;p 10. 0.]) |> ok
        Expect.equal paths.Length 1 "duplicate vertex keeps the same stroke"
    testCase "rectangle corner-only touches do not consume output budget" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create 0. 0. 1. 1.);MaxOutputPoints=1}
        for positions in [[p -10. 10.;p 0. 0.];[p 0. 0.;p -10. 10.]] do
            Expect.equal (GeoPath.project options (projection ProjectionKind.Equirectangular) (GeoGeometry.LineString positions) |> ok) [] "zero-length corner touch"
    testCase "rectangle clipping remains finite at large representable screen scales" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -1e149 -1e149 2e149 2e149);Precision=1e140}
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Scale=1e150} |> ok
        let paths=GeoPath.project options projected (GeoGeometry.LineString[p -60. 0.;p 60. 0.]) |> ok
        Expect.equal paths.Length 1 "crossing remains connected"
        let vertices=Path.commands paths.Head |> List.choose(function MoveTo point | LineTo point -> Some point | _ -> None)
        Expect.equal vertices.Head.X -1e149 "left boundary"
        Expect.equal (List.last vertices).X 1e149 "right boundary"
    testCase "rectangle line clipping still enforces output and work budgets" <| fun _ ->
        let options={GeoPathOptions.defaultValue with ClipExtent=Some(Bounds.create -0.5 -0.5 1. 1.)}
        let geometry=GeoGeometry.LineString[p -60. 0.;p 60. 0.]
        Expect.isError (GeoPath.project {options with MaxOutputPoints=1} (projection ProjectionKind.Equirectangular) geometry) "visible line requires two endpoints"
        Expect.isError (GeoPath.project {options with MaxWork=10L} (projection ProjectionKind.Equirectangular) geometry) "clipping consumes bounded work"
    testCase "rejects point-radius overflow instead of emitting nonfinite path commands" <| fun _ ->
        let projected=Projection.create {ProjectionConfig.defaultValue ProjectionKind.Equirectangular with Translate={X=Double.MaxValue;Y=0.}} |> ok
        Expect.isError (GeoPath.project {GeoPathOptions.defaultValue with PointRadius=Double.MaxValue} projected (GeoGeometry.Point(Some(p 0. 0.)))) "overflow is explicit"
]

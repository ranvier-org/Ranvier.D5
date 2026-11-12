module Ranvier.D5.Tests.GeoJsonTests

open System
open System.Threading
open Expecto
open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failtestf "Expected valid GeoJSON, received %A" errors
let private parse = GeoJson.parse >> ok
let private roundtrip source = source |> GeoJson.serialize |> ok |> parse
let private feature = """{"type":"Feature","id":1234567890123456789012345678901234567890,"geometry":null,"properties":{"nested":[true,null,{"value":1e400}],"missing":null},"vendor":{"revision":2}}"""

let tests = testList "GeoJSON interchange" [
    testCase "foreign metadata conversion stops at a small work budget" <| fun _ ->
        let metadata=Map.ofList[for i in 0..9999 -> string i,GeoValue.Null]
        let document=GeoDocument.Feature {Id=None;Geometry=None;Properties=None;ForeignMembers=metadata}
        let before=GC.GetAllocatedBytesForCurrentThread()
        let result=GeoJson.serializeWithCancellation {GeoJsonConfig.defaultValue with MaxWork=1L} CancellationToken.None document
        let allocated=GC.GetAllocatedBytesForCurrentThread()-before
        Expect.isError result "Work budget"
        Expect.isLessThan allocated 65536L "Do not copy a large foreign map before checking its budget"

    testCase "validates two dimensional bounding boxes without rejecting dateline wrapping" <| fun _ ->
        for bbox in ["null";"[]";"[0,0,1]";"[0,0,0,1,1,1]";"[0,20,10,10]";"[0,-91,10,10]";"[0,0,1e400,10]";"[0,0,\"10\",10]"] do
            for body in [ "\"type\":\"Point\",\"coordinates\":[0,0]"; "\"type\":\"Feature\",\"geometry\":null,\"properties\":null"; "\"type\":\"FeatureCollection\",\"features\":[]"] do
                Expect.isError (GeoJson.parse ("{"+body+",\"bbox\":"+bbox+"}")) ("Invalid bbox "+bbox)
        let source="""{"type":"Feature","geometry":null,"properties":null,"bbox":[170,-10,-170,10]}"""
        let value=parse source
        Expect.equal (roundtrip value) value "Valid wrapped metadata survives"
        let bad=GeoDocument.Feature {Id=None;Geometry=None;Properties=None;ForeignMembers=Map.ofList["bbox",GeoValue.Array[GeoValue.Number "0"]]}
        Expect.isError (GeoJson.serialize bad) "Manual metadata is validated on output"

    testCase "retains exact numeric IDs nulls nested properties and foreign members" <| fun _ ->
        let value = parse feature
        match value with
        | GeoDocument.Feature item ->
            Expect.equal item.Id (Some(GeoId.Number "1234567890123456789012345678901234567890")) "numeric identity is lossless"
            Expect.isNone item.Geometry "null geometry"
            let properties = item.Properties.Value
            Expect.equal properties.["nested"] (GeoValue.Array [GeoValue.Boolean true; GeoValue.Null;GeoValue.Object(Map.ofList["value",GeoValue.Number "1e400"])]) "detached immutable properties"
            Expect.equal properties.["missing"] GeoValue.Null "explicit null"
            Expect.isFalse (properties.ContainsKey "absent") "absent is not null"
            Expect.equal item.ForeignMembers.["vendor"] (GeoValue.Object(Map.ofList["revision",GeoValue.Number "2"])) "foreign metadata"
        | _ -> failtest "expected feature"
        Expect.equal (roundtrip value) value "numeric and property representation roundtrip"
        let text = parse (feature.Replace("1234567890123456789012345678901234567890","\"1234567890123456789012345678901234567890\""))
        Expect.notEqual text value "string and numeric identity differ"
    testCase "converts RFC exterior and hole winding reversibly" <| fun _ ->
        let value = parse """{"type":"Polygon","coordinates":[[[0,0],[20,0],[20,20],[0,0]],[[1,1],[1,2],[2,1],[1,1]]]}"""
        match value with
        | GeoDocument.Geometry(GeoGeometry.Polygon [outer;hole]) ->
            Expect.equal (outer |> List.map(fun p -> p.Longitude,p.Latitude)) [(0.,0.);(20.,20.);(20.,0.);(0.,0.)] "exterior converted"
            Expect.equal (hole |> List.map(fun p -> p.Longitude,p.Latitude)) [(1.,1.);(2.,1.);(1.,2.);(1.,1.)] "hole converted"
        | _ -> failtest "expected polygon"
        Expect.equal (roundtrip value) value "winding remains stable across interchange"
    testCase "supports all geometry kinds and permitted empties" <| fun _ ->
        for source in [
            """{"type":"Point","coordinates":[180,90]}"""
            """{"type":"Point","coordinates":[]}"""
            """{"type":"MultiPoint","coordinates":[[0,0],[20,30]]}"""
            """{"type":"LineString","coordinates":[[170,10],[-170,20]]}"""
            """{"type":"MultiLineString","coordinates":[[],[[0,0],[1,1]]]}"""
            """{"type":"Polygon","coordinates":[]}"""
            """{"type":"MultiPolygon","coordinates":[[],[[[0,0],[1,0],[0,1],[0,0]]]]}"""
            """{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[]},{"type":"GeometryCollection","geometries":[]}]}"""
            """{"type":"FeatureCollection","features":[],"vendor":true}"""
            """{"type":"Feature","geometry":null,"properties":null}"""
        ] do
            let value = parse source
            Expect.equal (roundtrip value) value source
        let collection = parse ("{\"type\":\"FeatureCollection\",\"features\":["+feature+","+feature+"]}")
        match collection with
        | GeoDocument.FeatureCollection(items,_) -> Expect.equal items.Length 2 "generic interchange preserves duplicate IDs for later join validation"
        | _ -> failtest "expected collection"
    testCase "rejects malformed structure coordinates duplicate members and ring topology" <| fun _ ->
        for source in [
            "null";"{}";"[]";"{broken}";"{\"type\":\"Sphere\"}"
            """{"type":"Point","type":"Point","coordinates":[0,0]}"""
            """{"type":"Point","coordinates":[0,0,5]}"""
            """{"type":"Point","coordinates":[0,91]}"""
            """{"type":"Point","coordinates":[1e400,0]}"""
            """{"type":"Point","coordinates":["0",0]}"""
            """{"type":"LineString","coordinates":[[0,0]]}"""
            """{"type":"Polygon","coordinates":[[]]}"""
            """{"type":"Polygon","coordinates":[[[0,0],[1,0],[0,1],[2,2]]]}"""
            """{"type":"Feature","geometry":null}"""
            """{"type":"Feature","properties":{}}"""
            """{"type":"Feature","geometry":null,"properties":[],"id":null}"""
            """{"type":"GeometryCollection","geometries":[null]}"""
        ] do Expect.isError (GeoJson.parse source) source
    testCase "enforces parse write and nesting budgets and cancellation" <| fun _ ->
        let config = GeoJsonConfig.defaultValue
        let points = """{"type":"MultiPoint","coordinates":[[0,0],[1,1],[2,2]]}"""
        let document = parse points
        for limited in [{config with MaxPoints=2};{config with MaxJsonBytes=10};{config with MaxWork=2L};{config with MaxDepth=1}] do
            Expect.isError (GeoJson.parseWithCancellation limited CancellationToken.None points) "parse budget"
            Expect.isError (GeoJson.serializeWithCancellation limited CancellationToken.None document) "write budget"
        let manyFeatures = "{\"type\":\"FeatureCollection\",\"features\":["+feature+","+feature+"]}"
        Expect.isError (GeoJson.parseWithCancellation {config with MaxFeatures=1} CancellationToken.None manyFeatures) "feature limit"
        use cancelled = new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throwsT<OperationCanceledException> (fun () -> GeoJson.parseWithCancellation config cancelled.Token points |> ignore) "cancel parse"
        Expect.throwsT<OperationCanceledException> (fun () -> GeoJson.serializeWithCancellation config cancelled.Token document |> ignore) "cancel write"
    testCase "small work budgets stop empty geometry conversion before allocating the full output" <| fun _ ->
        let config = {GeoJsonConfig.defaultValue with MaxWork=2L}
        let document = GeoDocument.Geometry(GeoGeometry.MultiLineString(List.replicate 100000 []))
        let before = GC.GetAllocatedBytesForCurrentThread()
        let result = GeoJson.serializeWithCancellation config CancellationToken.None document
        let allocated = GC.GetAllocatedBytesForCurrentThread()-before
        Expect.isError result "work budget"
        Expect.isLessThan allocated 65536L "conversion stops before creating 100,000 empty output arrays"
    testCase "validates manually constructed JSON numbers and reserved foreign fields" <| fun _ ->
        let feature = {Id=None;Geometry=None;Properties=Some(Map.ofList["x",GeoValue.Number "0,1"]);ForeignMembers=Map.empty}
        Expect.isError (GeoJson.serialize(GeoDocument.Feature feature)) "number cannot inject JSON"
        Expect.isError (GeoJson.serialize(GeoDocument.Feature {feature with Properties=None;ForeignMembers=Map.ofList["type",GeoValue.String "Other"]})) "reserved member collision"
        Expect.isError (GeoJson.serialize(GeoDocument.Geometry GeoGeometry.Sphere)) "sphere has no RFC representation"
]

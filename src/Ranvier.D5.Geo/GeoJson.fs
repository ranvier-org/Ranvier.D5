namespace Ranvier.D5.Geo

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open Ranvier.D5

/// <summary>Bounded RFC 7946 interchange with explicit conversion to spherical ring winding.</summary>
/// <remarks>Input RFC edges use coordinate interpolation; the resulting geometry uses great-circle edges.
/// Parsing reverses polygon rings and serialization reverses them back, preserving complement orientation.</remarks>
module GeoJson =
    exception private Rejected of LayoutError
    let private reject message = raise(Rejected(InvalidObservation message))
    let private budget message = raise(Rejected(BudgetExceeded message))
    let private configValid config =
        config.MaxJsonBytes>0 && config.MaxDepth>0 && config.MaxDepth<=128 &&
        config.MaxPoints>0 && config.MaxFeatures>0 && config.MaxWork>0L

    type private Context(config:GeoJsonConfig, cancellation:CancellationToken) =
        let mutable work=0L
        let mutable points=0
        let mutable features=0
        member _.Visit depth =
            cancellation.ThrowIfCancellationRequested()
            if depth>config.MaxDepth then budget "GeoJSON nesting limit exceeded."
            if work>=config.MaxWork then budget "GeoJSON traversal work limit exceeded."
            work<-work+1L
        member this.Point() =
            this.Visit 0
            if points>=config.MaxPoints then budget "GeoJSON point limit exceeded."
            points<-points+1
        member this.Feature() =
            this.Visit 0
            if features>=config.MaxFeatures then budget "GeoJSON feature limit exceeded."
            features<-features+1

    let private protect config (cancellation:CancellationToken) action =
        cancellation.ThrowIfCancellationRequested()
        if not(configValid config) then
            Error [InvalidObservation "GeoJSON budgets must be positive and MaxDepth must be at most 128."]
        else
            try Ok(action(Context(config,cancellation)))
            with
            | Rejected error -> Error [error]
            | :? JsonException as error -> Error [InvalidObservation("Invalid JSON or excessive nesting: "+error.Message)]

    let rec private readValue (context:Context) depth (value:JsonElement) =
        context.Visit depth
        match value.ValueKind with
        | JsonValueKind.Null -> GeoValue.Null
        | JsonValueKind.True -> GeoValue.Boolean true
        | JsonValueKind.False -> GeoValue.Boolean false
        | JsonValueKind.Number -> GeoValue.Number(value.GetRawText())
        | JsonValueKind.String -> GeoValue.String(value.GetString())
        | JsonValueKind.Array ->
            value.EnumerateArray() |> Seq.map(readValue context (depth+1)) |> Seq.toList |> GeoValue.Array
        | JsonValueKind.Object ->
            let mutable properties=Map.empty
            for property in value.EnumerateObject() do
                if properties.ContainsKey property.Name then reject("Duplicate JSON member: "+property.Name)
                properties<-properties.Add(property.Name,readValue context (depth+1) property.Value)
            GeoValue.Object properties
        | _ -> reject "Unsupported JSON value."

    let private asObject = function GeoValue.Object values -> values | _ -> reject "Expected a geographic object."
    let private asArray = function GeoValue.Array values -> values | _ -> reject "Expected a geographic coordinate or object array."
    let private required name (values:Map<string,GeoValue>) =
        values.TryFind name |> Option.defaultWith(fun () -> reject("Missing GeoJSON member: "+name))
    let private kind values =
        match required "type" values with
        | GeoValue.String value -> value
        | _ -> reject "GeoJSON type must be a string."
    let private coordinateNumber = function
        | GeoValue.Number value ->
#if FABLE_COMPILER
            // The value is already a JSON number lexeme, and Fable parses invariantly without NumberStyles.
            match Double.TryParse value with
#else
            match Double.TryParse(value,NumberStyles.Float,CultureInfo.InvariantCulture) with
#endif
            | true,number when Double.IsFinite number -> number
            | _ -> reject "Coordinates require finite representable numbers."
        | _ -> reject "Coordinates require JSON numbers."
    let private readPoint (context:Context) value =
        context.Point()
        match asArray value with
        | [longitude;latitude] ->
            match GeoPosition.create (coordinateNumber longitude) (coordinateNumber latitude) with
            | Ok point -> point
            | Error errors -> raise(Rejected(List.head errors))
        | _ -> reject "Geographic positions require exactly two coordinates."
    let private validateLine positions =
        match positions with
        | [_] -> reject "A nonempty line requires at least two positions."
        | _ -> positions
    let private validateRing positions =
        if List.length positions<4 || List.head positions<>List.last positions then
            reject "A polygon ring requires at least four positions and identical first and last coordinates."
        positions
    let private readLine context value = asArray value |> List.map(readPoint context) |> validateLine
    let private readPolygon context value =
        asArray value |> List.map(fun ring -> asArray ring |> List.map(readPoint context) |> validateRing |> List.rev)

    let private validateBbox (values:Map<string,GeoValue>) =
        match values.TryFind "bbox" with
        | None -> ()
        | Some(GeoValue.Array [west;south;east;north]) ->
            coordinateNumber west |> ignore
            coordinateNumber east |> ignore
            let lower,upper=coordinateNumber south,coordinateNumber north
            if lower < -90. || upper>90. || lower>upper then
                reject "Bounding box latitudes must be ordered within [-90,90]."
        | _ -> reject "Two-dimensional GeoJSON bounding boxes require four finite numbers."

    let rec private readGeometry (context:Context) depth value =
        context.Visit depth
        let values=asObject value
        validateBbox values
        let coordinates()=required "coordinates" values
        match kind values with
        | "Point" ->
            match asArray(coordinates()) with
            | [] -> GeoGeometry.Point None
            | _ -> GeoGeometry.Point(Some(readPoint context (coordinates())))
        | "MultiPoint" -> GeoGeometry.MultiPoint(asArray(coordinates()) |> List.map(readPoint context))
        | "LineString" -> GeoGeometry.LineString(readLine context (coordinates()))
        | "MultiLineString" -> GeoGeometry.MultiLineString(asArray(coordinates()) |> List.map(readLine context))
        | "Polygon" -> GeoGeometry.Polygon(readPolygon context (coordinates()))
        | "MultiPolygon" -> GeoGeometry.MultiPolygon(asArray(coordinates()) |> List.map(readPolygon context))
        | "GeometryCollection" ->
            GeoGeometry.Collection(required "geometries" values |> asArray |> List.map(readGeometry context (depth+1)))
        | _ -> reject "Expected an RFC 7946 geometry type."

    let private featureReserved=Set.ofList ["type";"id";"geometry";"properties"]
    let private collectionReserved=Set.ofList ["type";"features"]
    let private foreign reserved values = values |> Map.filter(fun key _ -> not(Set.contains key reserved))
    let private readFeature (context:Context) depth value =
        context.Feature()
        let values=asObject value
        validateBbox values
        if kind values<>"Feature" then reject "Feature collections require Feature objects."
        let id=
            match values.TryFind "id" with
            | None -> None
            | Some(GeoValue.String id) -> Some(GeoId.Text id)
            | Some(GeoValue.Number id) -> Some(GeoId.Number id)
            | _ -> reject "Feature identifiers require a string or number."
        let geometry=
            match required "geometry" values with
            | GeoValue.Null -> None
            | value -> Some(readGeometry context (depth+1) value)
        let properties=
            match required "properties" values with
            | GeoValue.Null -> None
            | GeoValue.Object values -> Some values
            | _ -> reject "Feature properties require an object or null."
        { Id=id;Geometry=geometry;Properties=properties;ForeignMembers=foreign featureReserved values }

    let private readDocument context value =
        let values=asObject value
        validateBbox values
        match kind values with
        | "Feature" -> GeoDocument.Feature(readFeature context 1 value)
        | "FeatureCollection" ->
            GeoDocument.FeatureCollection(required "features" values |> asArray |> List.map(readFeature context 2),
                                          foreign collectionReserved values)
        | _ -> GeoDocument.Geometry(readGeometry context 1 value)

    /// <summary>Parses immutable geometry and features, rejecting invalid input with typed errors.</summary>
    let parseWithCancellation config cancellation (json:string) =
        protect config cancellation (fun context ->
            if isNull json then reject "GeoJSON input cannot be null."
            if Encoding.UTF8.GetByteCount json>config.MaxJsonBytes then budget "GeoJSON input size limit exceeded."
            use document=JsonDocument.Parse(json,JsonDocumentOptions(MaxDepth=config.MaxDepth))
            document.RootElement |> readValue context 1 |> readDocument context)

    /// <summary>Parses RFC 7946 using the default budgets.</summary>
    let parse json = parseWithCancellation GeoJsonConfig.defaultValue CancellationToken.None json

    let private number (value:float) = GeoValue.Number(value.ToString("R",CultureInfo.InvariantCulture))
    let private writePoint (context:Context) (position:GeoPosition) =
        context.Point()
        GeoValue.Array [number position.Longitude;number position.Latitude]
    let private objectValue kind name value =
        GeoValue.Object(Map.ofList ["type",GeoValue.String kind;name,value])
    let private writeLine (context:Context) positions =
        context.Visit 0
        validateLine positions |> List.map(writePoint context) |> GeoValue.Array
    let private writePolygon (context:Context) rings =
        context.Visit 0
        rings
        |> List.map(fun ring ->
            context.Visit 0
            match ring with
            | first :: _ :: _ :: _ :: _ ->
                let mutable last=first
                let mutable reversed=[]
                for point in ring do
                    reversed<-writePoint context point :: reversed
                    last<-point
                if last<>first then reject "Polygon ring coordinates must close."
                GeoValue.Array reversed
            | _ -> reject "A polygon ring requires at least four positions.")
        |> GeoValue.Array

    let rec private writeGeometry (context:Context) depth geometry =
        context.Visit depth
        match geometry with
        | GeoGeometry.Point position ->
            objectValue "Point" "coordinates" (position |> Option.map(writePoint context) |> Option.defaultValue(GeoValue.Array []))
        | GeoGeometry.MultiPoint positions ->
            objectValue "MultiPoint" "coordinates" (positions |> List.map(writePoint context) |> GeoValue.Array)
        | GeoGeometry.LineString positions -> objectValue "LineString" "coordinates" (writeLine context positions)
        | GeoGeometry.MultiLineString lines ->
            objectValue "MultiLineString" "coordinates" (lines |> List.map(writeLine context) |> GeoValue.Array)
        | GeoGeometry.Polygon rings -> objectValue "Polygon" "coordinates" (writePolygon context rings)
        | GeoGeometry.MultiPolygon polygons ->
            objectValue "MultiPolygon" "coordinates" (polygons |> List.map(writePolygon context) |> GeoValue.Array)
        | GeoGeometry.Collection geometries ->
            objectValue "GeometryCollection" "geometries" (geometries |> List.map(writeGeometry context (depth+1)) |> GeoValue.Array)
        | GeoGeometry.Sphere -> reject "Sphere has no RFC 7946 representation."

    let private mergeForeign (context:Context) depth reserved required foreign =
        validateBbox foreign
        Map.fold(fun values key value ->
            context.Visit depth
            if Set.contains key reserved then reject "Foreign members cannot replace reserved GeoJSON fields."
            Map.add key value values) required foreign |> GeoValue.Object

    let private writeFeature (context:Context) depth (feature:GeoFeature) =
        context.Feature()
        let fields=Map.ofList [
            "type",GeoValue.String "Feature"
            "geometry",feature.Geometry |> Option.map(writeGeometry context (depth+1)) |> Option.defaultValue GeoValue.Null
            "properties",feature.Properties |> Option.map GeoValue.Object |> Option.defaultValue GeoValue.Null ]
        let fields=
            match feature.Id with
            | None -> fields
            | Some(GeoId.Text value) -> fields.Add("id",GeoValue.String value)
            | Some(GeoId.Number value) -> fields.Add("id",GeoValue.Number value)
        mergeForeign context depth featureReserved fields feature.ForeignMembers

    let private writeDocument context = function
        | GeoDocument.Geometry geometry -> writeGeometry context 1 geometry
        | GeoDocument.Feature feature -> writeFeature context 1 feature
        | GeoDocument.FeatureCollection(features,foreign) ->
            let fields=Map.ofList [
                "type",GeoValue.String "FeatureCollection"
                "features",features |> List.map(writeFeature context 2) |> GeoValue.Array ]
            mergeForeign context 1 collectionReserved fields foreign

    let rec private emitValue (context:Context) config depth (writer:Utf8JsonWriter) value =
        context.Visit depth
        let checkSize() =
            if writer.BytesCommitted+int64 writer.BytesPending>int64 config.MaxJsonBytes then budget "GeoJSON output size limit exceeded."
        match value with
        | GeoValue.Null -> writer.WriteNullValue()
        | GeoValue.Boolean value -> writer.WriteBooleanValue value
        | GeoValue.String value ->
            if isNull value then reject "JSON string payloads cannot be null; use GeoValue.Null."
            if Encoding.UTF8.GetByteCount value>config.MaxJsonBytes then budget "GeoJSON string exceeds output size limit."
            writer.WriteStringValue value
        | GeoValue.Number value ->
            if isNull value || value.Length>config.MaxJsonBytes then reject "Invalid JSON numeric lexeme."
            use number=JsonDocument.Parse(value)
            if number.RootElement.ValueKind<>JsonValueKind.Number || number.RootElement.GetRawText()<>value then
                reject "Number payloads require one exact JSON numeric lexeme."
            writer.WriteRawValue(value,false)
        | GeoValue.Array values ->
            writer.WriteStartArray()
            for value in values do emitValue context config (depth+1) writer value
            writer.WriteEndArray()
        | GeoValue.Object values ->
            writer.WriteStartObject()
            for KeyValue(key,value) in values do
                if isNull key then reject "JSON member names cannot be null."
                if Encoding.UTF8.GetByteCount key>config.MaxJsonBytes then budget "GeoJSON member name exceeds output size limit."
                writer.WritePropertyName key
                checkSize()
                emitValue context config (depth+1) writer value
            writer.WriteEndObject()
        checkSize()

    /// <summary>Serializes bounded RFC 7946 values; rejects Sphere and invalid manually constructed values.</summary>
    let serializeWithCancellation config cancellation document =
        protect config cancellation (fun context ->
            let value=writeDocument context document
#if FABLE_COMPILER
            // Fable has no MemoryStream; the Fable Utf8JsonWriter (FableJson.fs) writes into a string.
            use writer=new Utf8JsonWriter(JsonWriterOptions(MaxDepth=config.MaxDepth))
            emitValue context config 1 writer value
            writer.ToString())
#else
            use output=new MemoryStream()
            use writer=new Utf8JsonWriter(output,JsonWriterOptions(MaxDepth=config.MaxDepth))
            emitValue context config 1 writer value
            writer.Flush()
            Encoding.UTF8.GetString(output.GetBuffer(),0,int output.Length))
#endif

    /// <summary>Serializes RFC 7946 using the default budgets.</summary>
    let serialize document = serializeWithCancellation GeoJsonConfig.defaultValue CancellationToken.None document

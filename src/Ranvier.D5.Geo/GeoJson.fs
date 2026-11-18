namespace Ranvier.D5.Geo

open System
#if !FABLE_COMPILER
open System.Buffers
open System.Buffers.Text
#endif
open System.Collections.Generic
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
        member _.CheckDepth depth =
            if depth>config.MaxDepth then budget "GeoJSON nesting limit exceeded."
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

    // Single-pass RFC 8259 Section 6 JSON number validator (proven in Pure/GeometryGeo.v: rfc8259_number_scanner_boundaries_sound).
    let private isValidJsonNumber (s:string) =
        if isNull s || s.Length=0 then false
        else
            let len=s.Length
            let mutable i=if s[0]='-' then 1 else 0
            if i>=len then false
            else
                let mutable ok=true
                if s[i]='0' then
                    i<-i+1
                elif s[i]>='1' && s[i]<='9' then
                    i<-i+1
                    while i<len && s[i]>='0' && s[i]<='9' do
                        i<-i+1
                else
                    ok<-false
                if ok && i<len && s[i]='.' then
                    i<-i+1
                    let start=i
                    while i<len && s[i]>='0' && s[i]<='9' do
                        i<-i+1
                    if i=start then ok<-false
                if ok && i<len && (s[i]='e' || s[i]='E') then
                    i<-i+1
                    if i<len && (s[i]='+' || s[i]='-') then i<-i+1
                    let start=i
                    while i<len && s[i]>='0' && s[i]<='9' do
                        i<-i+1
                    if i=start then ok<-false
                ok && i=len

    let rec private readValue (context:Context) depth (value:JsonElement) =
        context.Visit depth
        match value.ValueKind with
        | JsonValueKind.Null -> GeoValue.Null
        | JsonValueKind.True -> GeoValue.Boolean true
        | JsonValueKind.False -> GeoValue.Boolean false
        | JsonValueKind.Number -> GeoValue.Number(value.GetRawText())
        | JsonValueKind.String -> GeoValue.String(value.GetString())
        | JsonValueKind.Array ->
            let mutable rev=[]
            for item in value.EnumerateArray() do
                rev<-readValue context (depth+1) item::rev
            GeoValue.Array(List.rev rev)
        | JsonValueKind.Object ->
            let mutable properties=Map.empty
            for property in value.EnumerateObject() do
                if properties.ContainsKey property.Name then reject("Duplicate JSON member: "+property.Name)
                properties<-properties.Add(property.Name,readValue context (depth+1) property.Value)
            GeoValue.Object properties
        | _ -> reject "Unsupported JSON value."

    let private coordinateNumber = function
        | GeoValue.Number value ->
#if FABLE_COMPILER
            match Double.TryParse value with
#else
            match Double.TryParse(value,NumberStyles.Float,CultureInfo.InvariantCulture) with
#endif
            | true,number when Double.IsFinite number -> number
            | _ -> reject "Coordinates require finite representable numbers."
        | _ -> reject "Coordinates require JSON numbers."

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

    let private featureReserved=Set.ofList ["type";"id";"geometry";"properties"]
    let private collectionReserved=Set.ofList ["type";"features"]

#if FABLE_COMPILER
    let private asObject = function GeoValue.Object values -> values | _ -> reject "Expected a geographic object."
    let private asArray = function GeoValue.Array values -> values | _ -> reject "Expected a geographic coordinate or object array."
    let private required name (values:Map<string,GeoValue>) =
        values.TryFind name |> Option.defaultWith(fun () -> reject("Missing GeoJSON member: "+name))
    let private kind values =
        match required "type" values with
        | GeoValue.String value -> value
        | _ -> reject "GeoJSON type must be a string."
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
#else
    let rec private validateSkippedElement (context:Context) depth (value:JsonElement) =
        context.Visit depth
        match value.ValueKind with
        | JsonValueKind.Array ->
            let mutable e=value.EnumerateArray()
            while e.MoveNext() do
                validateSkippedElement context (depth+1) e.Current
        | JsonValueKind.Object ->
            let seen=HashSet<string>(StringComparer.Ordinal)
            let mutable e=value.EnumerateObject()
            while e.MoveNext() do
                let prop=e.Current
                if not(seen.Add prop.Name) then reject("Duplicate JSON member: "+prop.Name)
                validateSkippedElement context (depth+1) prop.Value
        | JsonValueKind.Null | JsonValueKind.True | JsonValueKind.False | JsonValueKind.Number | JsonValueKind.String -> ()
        | _ -> reject "Unsupported JSON value."

    let private coordElement (el:JsonElement) =
        if el.ValueKind<>JsonValueKind.Number then reject "Coordinates require JSON numbers."
        match el.TryGetDouble() with
        | true,number when Double.IsFinite number -> number
        | _ -> reject "Coordinates require finite representable numbers."

    let private validateBboxElement (el:JsonElement) =
        if el.ValueKind<>JsonValueKind.Array || el.GetArrayLength()<>4 then
            reject "Two-dimensional GeoJSON bounding boxes require four finite numbers."
        let mutable e=el.EnumerateArray()
        e.MoveNext() |> ignore
        let west=coordElement e.Current
        e.MoveNext() |> ignore
        let south=coordElement e.Current
        e.MoveNext() |> ignore
        let east=coordElement e.Current
        e.MoveNext() |> ignore
        let north=coordElement e.Current
        ignore west
        ignore east
        if south < -90. || north>90. || south>north then
            reject "Bounding box latitudes must be ordered within [-90,90]."

    let private readPointElement (context:Context) (el:JsonElement) =
        context.Point()
        if el.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
        if el.GetArrayLength()<>2 then reject "Geographic positions require exactly two coordinates."
        let mutable e=el.EnumerateArray()
        e.MoveNext() |> ignore
        let lon=coordElement e.Current
        e.MoveNext() |> ignore
        let lat=coordElement e.Current
        match GeoPosition.create lon lat with
        | Ok point -> point
        | Error errors -> raise(Rejected errors.Head)

    let private readPointsElement (context:Context) (el:JsonElement) =
        if el.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
        let mutable rev=[]
        let mutable e=el.EnumerateArray()
        while e.MoveNext() do
            rev<-readPointElement context e.Current::rev
        List.rev rev

    let private readLineElement (context:Context) (el:JsonElement) =
        let pts=readPointsElement context el
        match pts with
        | [_] -> reject "A nonempty line requires at least two positions."
        | _ -> pts

    let private readRingReversedElement (context:Context) (el:JsonElement) =
        if el.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
        let mutable rev=[]
        let mutable count=0
        let mutable first=Unchecked.defaultof<GeoPosition>
        let mutable last=Unchecked.defaultof<GeoPosition>
        let mutable e=el.EnumerateArray()
        while e.MoveNext() do
            let pt=readPointElement context e.Current
            if count=0 then first<-pt
            last<-pt
            rev<-pt::rev
            count<-count+1
        if count<4 || first<>last then
            reject "A polygon ring requires at least four positions and identical first and last coordinates."
        rev

    let private readPolygonElement (context:Context) (el:JsonElement) =
        if el.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
        let mutable revRings=[]
        let mutable e=el.EnumerateArray()
        while e.MoveNext() do
            revRings<-readRingReversedElement context e.Current::revRings
        List.rev revRings

    let rec private readGeometryElement (context:Context) depth (el:JsonElement) =
        context.Visit depth
        if el.ValueKind<>JsonValueKind.Object then reject "Expected a geographic object."
        let mutable hasType=false
        let mutable kindText=""
        let mutable hasCoords=false
        let mutable coordsEl=Unchecked.defaultof<JsonElement>
        let mutable hasGeoms=false
        let mutable geomsEl=Unchecked.defaultof<JsonElement>
        let mutable hasBbox=false
        let mutable extraKeys:HashSet<string> voption=ValueNone
        let mutable e=el.EnumerateObject()
        while e.MoveNext() do
            let prop=e.Current
            let name=prop.Name
            if name="type" then
                if hasType then reject "Duplicate JSON member: type"
                hasType<-true
                if prop.Value.ValueKind<>JsonValueKind.String then reject "GeoJSON type must be a string."
                kindText<-prop.Value.GetString()
            elif name="coordinates" then
                if hasCoords then reject "Duplicate JSON member: coordinates"
                hasCoords<-true
                coordsEl<-prop.Value
            elif name="geometries" then
                if hasGeoms then reject "Duplicate JSON member: geometries"
                hasGeoms<-true
                geomsEl<-prop.Value
            elif name="bbox" then
                if hasBbox then reject "Duplicate JSON member: bbox"
                hasBbox<-true
                validateBboxElement prop.Value
            else
                let set =
                    match extraKeys with
                    | ValueSome s -> s
                    | ValueNone ->
                        let s=HashSet<string>(StringComparer.Ordinal)
                        extraKeys<-ValueSome s
                        s
                if not(set.Add name) then reject("Duplicate JSON member: "+name)
                validateSkippedElement context (depth+1) prop.Value
        if not hasType then reject "Missing GeoJSON member: type"
        let inline requireCoords () =
            if not hasCoords then reject "Missing GeoJSON member: coordinates"
            coordsEl
        match kindText with
        | "Point" ->
            let c=requireCoords()
            if c.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
            if c.GetArrayLength()=0 then GeoGeometry.Point None
            else GeoGeometry.Point(Some(readPointElement context c))
        | "MultiPoint" -> GeoGeometry.MultiPoint(readPointsElement context (requireCoords()))
        | "LineString" -> GeoGeometry.LineString(readLineElement context (requireCoords()))
        | "MultiLineString" ->
            let c=requireCoords()
            if c.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
            let mutable rev=[]
            let mutable ae=c.EnumerateArray()
            while ae.MoveNext() do rev<-readLineElement context ae.Current::rev
            GeoGeometry.MultiLineString(List.rev rev)
        | "Polygon" -> GeoGeometry.Polygon(readPolygonElement context (requireCoords()))
        | "MultiPolygon" ->
            let c=requireCoords()
            if c.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
            let mutable rev=[]
            let mutable ae=c.EnumerateArray()
            while ae.MoveNext() do rev<-readPolygonElement context ae.Current::rev
            GeoGeometry.MultiPolygon(List.rev rev)
        | "GeometryCollection" ->
            if not hasGeoms then reject "Missing GeoJSON member: geometries"
            if geomsEl.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
            let mutable rev=[]
            let mutable ae=geomsEl.EnumerateArray()
            while ae.MoveNext() do rev<-readGeometryElement context (depth+1) ae.Current::rev
            GeoGeometry.Collection(List.rev rev)
        | _ -> reject "Expected an RFC 7946 geometry type."

    let private readPropertiesMap (context:Context) depth (el:JsonElement) =
        context.Visit depth
        let mutable properties=Map.empty
        let mutable e=el.EnumerateObject()
        while e.MoveNext() do
            let prop=e.Current
            if properties.ContainsKey prop.Name then reject("Duplicate JSON member: "+prop.Name)
            properties<-properties.Add(prop.Name,readValue context (depth+1) prop.Value)
        properties

    let private readFeatureElement (context:Context) depth (el:JsonElement) =
        context.Feature()
        if el.ValueKind<>JsonValueKind.Object then reject "Expected a geographic object."
        let mutable hasType=false
        let mutable kindText=""
        let mutable hasId=false
        let mutable idVal:GeoId option=None
        let mutable hasGeom=false
        let mutable geomEl=Unchecked.defaultof<JsonElement>
        let mutable hasProps=false
        let mutable propsEl=Unchecked.defaultof<JsonElement>
        let mutable foreign=Map.empty
        let mutable e=el.EnumerateObject()
        while e.MoveNext() do
            let prop=e.Current
            let name=prop.Name
            if name="type" then
                if hasType then reject "Duplicate JSON member: type"
                hasType<-true
                if prop.Value.ValueKind<>JsonValueKind.String then reject "GeoJSON type must be a string."
                kindText<-prop.Value.GetString()
            elif name="id" then
                if hasId then reject "Duplicate JSON member: id"
                hasId<-true
                match prop.Value.ValueKind with
                | JsonValueKind.String -> idVal<-Some(GeoId.Text(prop.Value.GetString()))
                | JsonValueKind.Number -> idVal<-Some(GeoId.Number(prop.Value.GetRawText()))
                | _ -> reject "Feature identifiers require a string or number."
            elif name="geometry" then
                if hasGeom then reject "Duplicate JSON member: geometry"
                hasGeom<-true
                geomEl<-prop.Value
            elif name="properties" then
                if hasProps then reject "Duplicate JSON member: properties"
                hasProps<-true
                propsEl<-prop.Value
            else
                if foreign.ContainsKey name then reject("Duplicate JSON member: "+name)
                if name="bbox" then validateBboxElement prop.Value
                foreign<-foreign.Add(name,readValue context (depth+1) prop.Value)
        if not hasType then reject "Missing GeoJSON member: type"
        if kindText<>"Feature" then reject "Feature collections require Feature objects."
        if not hasGeom then reject "Missing GeoJSON member: geometry"
        if not hasProps then reject "Missing GeoJSON member: properties"
        let geometry =
            if geomEl.ValueKind=JsonValueKind.Null then None
            else Some(readGeometryElement context (depth+1) geomEl)
        let properties =
            match propsEl.ValueKind with
            | JsonValueKind.Null -> None
            | JsonValueKind.Object -> Some(readPropertiesMap context (depth+1) propsEl)
            | _ -> reject "Feature properties require an object or null."
        { Id=idVal; Geometry=geometry; Properties=properties; ForeignMembers=foreign }

    let private readDocumentElement (context:Context) (el:JsonElement) =
        if el.ValueKind<>JsonValueKind.Object then
            context.Visit 1
            reject "Expected a geographic object."
        let mutable hasType=false
        let mutable kindText=""
        let mutable hasFeatures=false
        let mutable featuresEl=Unchecked.defaultof<JsonElement>
        let mutable e=el.EnumerateObject()
        while e.MoveNext() do
            let prop=e.Current
            if prop.Name="type" then
                if hasType then reject "Duplicate JSON member: type"
                hasType<-true
                if prop.Value.ValueKind<>JsonValueKind.String then reject "GeoJSON type must be a string."
                kindText<-prop.Value.GetString()
            elif prop.Name="features" then
                if hasFeatures then reject "Duplicate JSON member: features"
                hasFeatures<-true
                featuresEl<-prop.Value
        if not hasType then reject "Missing GeoJSON member: type"
        match kindText with
        | "Feature" ->
            context.Visit 1
            GeoDocument.Feature(readFeatureElement context 1 el)
        | "FeatureCollection" ->
            context.Visit 1
            if not hasFeatures then reject "Missing GeoJSON member: features"
            if featuresEl.ValueKind<>JsonValueKind.Array then reject "Expected a geographic coordinate or object array."
            let mutable foreign=Map.empty
            let mutable hasTypeSeen=false
            let mutable hasFeaturesSeen=false
            let mutable pe=el.EnumerateObject()
            while pe.MoveNext() do
                let prop=pe.Current
                let name=prop.Name
                if name="type" then
                    if hasTypeSeen then reject "Duplicate JSON member: type"
                    hasTypeSeen<-true
                elif name="features" then
                    if hasFeaturesSeen then reject "Duplicate JSON member: features"
                    hasFeaturesSeen<-true
                else
                    if foreign.ContainsKey name then reject("Duplicate JSON member: "+name)
                    if name="bbox" then validateBboxElement prop.Value
                    foreign<-foreign.Add(name,readValue context 2 prop.Value)
            let mutable revFeatures=[]
            let mutable fe=featuresEl.EnumerateArray()
            while fe.MoveNext() do
                revFeatures<-readFeatureElement context 2 fe.Current::revFeatures
            GeoDocument.FeatureCollection(List.rev revFeatures,foreign)
        | _ ->
            GeoDocument.Geometry(readGeometryElement context 1 el)
#endif

    /// <summary>Parses immutable geometry and features, rejecting invalid input with typed errors.</summary>
    let parseWithCancellation config cancellation (json:string) =
        protect config cancellation (fun context ->
            if isNull json then reject "GeoJSON input cannot be null."
            if Encoding.UTF8.GetByteCount json>config.MaxJsonBytes then budget "GeoJSON input size limit exceeded."
            use document=JsonDocument.Parse(json,JsonDocumentOptions(MaxDepth=config.MaxDepth))
#if FABLE_COMPILER
            document.RootElement |> readValue context 1 |> readDocument context)
#else
            readDocumentElement context document.RootElement)
#endif

    /// <summary>Parses RFC 7946 using the default budgets.</summary>
    let parse json = parseWithCancellation GeoJsonConfig.defaultValue CancellationToken.None json

    let rec private emitValue (context:Context) config depth (writer:Utf8JsonWriter) value =
        context.Visit depth
        let inline checkSize() =
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
            if not(isValidJsonNumber value) then
                reject "Number payloads require one exact JSON numeric lexeme."
            writer.WriteRawValue(value,true)
        | GeoValue.Array values ->
            writer.WriteStartArray()
            for item in values do emitValue context config (depth+1) writer item
            writer.WriteEndArray()
        | GeoValue.Object values ->
            writer.WriteStartObject()
            for KeyValue(key,item) in values do
                if isNull key then reject "JSON member names cannot be null."
                if Encoding.UTF8.GetByteCount key>config.MaxJsonBytes then budget "GeoJSON member name exceeds output size limit."
                writer.WritePropertyName key
                checkSize()
                emitValue context config (depth+1) writer item
            writer.WriteEndObject()
        checkSize()

    let private emitDocument (context:Context) (config:GeoJsonConfig) (writer:Utf8JsonWriter) (document:GeoDocument) =
#if !FABLE_COMPILER
        let coordScratch=Array.zeroCreate<byte> 32
#endif
        let ringScratch=ResizeArray<GeoPosition>(16)
        let inline checkSize () =
            if writer.BytesCommitted+int64 writer.BytesPending>int64 config.MaxJsonBytes then budget "GeoJSON output size limit exceeded."
        let inline writeProperty (key:string) =
            if isNull key then reject "JSON member names cannot be null."
            if Encoding.UTF8.GetByteCount key>config.MaxJsonBytes then budget "GeoJSON member name exceeds output size limit."
            writer.WritePropertyName key
            checkSize()
        let emitPoint depth (position:GeoPosition) =
            context.Point()
            context.CheckDepth depth
            writer.WriteStartArray()
#if FABLE_COMPILER
            writer.WriteRawValue(position.Longitude.ToString("R",CultureInfo.InvariantCulture),true)
            writer.WriteRawValue(position.Latitude.ToString("R",CultureInfo.InvariantCulture),true)
#else
            let mutable written=0
            Utf8Formatter.TryFormat(position.Longitude,Span<byte>(coordScratch),&written,StandardFormat('R')) |> ignore
            writer.WriteRawValue(ReadOnlySpan<byte>(coordScratch,0,written),true)
            Utf8Formatter.TryFormat(position.Latitude,Span<byte>(coordScratch),&written,StandardFormat('R')) |> ignore
            writer.WriteRawValue(ReadOnlySpan<byte>(coordScratch,0,written),true)
#endif
            writer.WriteEndArray()
            checkSize()
        let emitLine depth (positions:GeoPosition list) =
            context.Visit 0
            context.CheckDepth depth
            match positions with
            | [_] -> reject "A nonempty line requires at least two positions."
            | _ -> ()
            writer.WriteStartArray()
            for pt in positions do emitPoint (depth+1) pt
            writer.WriteEndArray()
            checkSize()
        let emitPolygon depth (rings:GeoPosition list list) =
            context.Visit 0
            context.CheckDepth depth
            writer.WriteStartArray()
            for ring in rings do
                context.Visit 0
                context.CheckDepth(depth+1)
                ringScratch.Clear()
                for pt in ring do ringScratch.Add pt
                if ringScratch.Count<4 then reject "A polygon ring requires at least four positions."
                if ringScratch[0]<>ringScratch[ringScratch.Count-1] then reject "Polygon ring coordinates must close."
                writer.WriteStartArray()
                for i=ringScratch.Count-1 downto 0 do
                    emitPoint (depth+2) ringScratch[i]
                writer.WriteEndArray()
                checkSize()
            writer.WriteEndArray()
            checkSize()
        let rec emitGeometry depth geometry =
            context.Visit depth
            context.CheckDepth(depth+1)
            match geometry with
            | GeoGeometry.Sphere -> reject "Sphere has no RFC 7946 representation."
            | GeoGeometry.Point position ->
                writer.WriteStartObject()
                writer.WritePropertyName "type"
                writer.WriteStringValue "Point"
                writer.WritePropertyName "coordinates"
                checkSize()
                match position with
                | Some pt -> emitPoint (depth+1) pt
                | None ->
                    writer.WriteStartArray()
                    writer.WriteEndArray()
                    checkSize()
                writer.WriteEndObject()
                checkSize()
            | GeoGeometry.MultiPoint positions ->
                writer.WriteStartObject()
                writer.WritePropertyName "type"
                writer.WriteStringValue "MultiPoint"
                writer.WritePropertyName "coordinates"
                checkSize()
                writer.WriteStartArray()
                for pt in positions do emitPoint (depth+2) pt
                writer.WriteEndArray()
                writer.WriteEndObject()
                checkSize()
            | GeoGeometry.LineString positions ->
                writer.WriteStartObject()
                writer.WritePropertyName "type"
                writer.WriteStringValue "LineString"
                writer.WritePropertyName "coordinates"
                checkSize()
                emitLine (depth+1) positions
                writer.WriteEndObject()
                checkSize()
            | GeoGeometry.MultiLineString lines ->
                writer.WriteStartObject()
                writer.WritePropertyName "type"
                writer.WriteStringValue "MultiLineString"
                writer.WritePropertyName "coordinates"
                checkSize()
                writer.WriteStartArray()
                for line in lines do emitLine (depth+2) line
                writer.WriteEndArray()
                writer.WriteEndObject()
                checkSize()
            | GeoGeometry.Polygon rings ->
                writer.WriteStartObject()
                writer.WritePropertyName "type"
                writer.WriteStringValue "Polygon"
                writer.WritePropertyName "coordinates"
                checkSize()
                emitPolygon (depth+1) rings
                writer.WriteEndObject()
                checkSize()
            | GeoGeometry.MultiPolygon polygons ->
                writer.WriteStartObject()
                writer.WritePropertyName "type"
                writer.WriteStringValue "MultiPolygon"
                writer.WritePropertyName "coordinates"
                checkSize()
                writer.WriteStartArray()
                for poly in polygons do emitPolygon (depth+2) poly
                writer.WriteEndArray()
                writer.WriteEndObject()
                checkSize()
            | GeoGeometry.Collection geometries ->
                writer.WriteStartObject()
                writer.WritePropertyName "type"
                writer.WriteStringValue "GeometryCollection"
                writer.WritePropertyName "geometries"
                checkSize()
                writer.WriteStartArray()
                for child in geometries do emitGeometry (depth+2) child
                writer.WriteEndArray()
                writer.WriteEndObject()
                checkSize()
        let emitForeign depth reserved (foreign:Map<string,GeoValue>) =
            validateBbox foreign
            for KeyValue(key,value) in foreign do
                context.Visit depth
                if Set.contains key reserved then reject "Foreign members cannot replace reserved GeoJSON fields."
                writeProperty key
                emitValue context config (depth+1) writer value
        let emitFeature depth (feature:GeoFeature) =
            context.Feature()
            context.CheckDepth(depth+1)
            validateBbox feature.ForeignMembers
            for KeyValue(key,_) in feature.ForeignMembers do
                context.Visit depth
                if Set.contains key featureReserved then reject "Foreign members cannot replace reserved GeoJSON fields."
            writer.WriteStartObject()
            writer.WritePropertyName "type"
            writer.WriteStringValue "Feature"
            checkSize()
            match feature.Id with
            | None -> ()
            | Some(GeoId.Text value) ->
                if isNull value then reject "JSON string payloads cannot be null; use GeoValue.Null."
                if Encoding.UTF8.GetByteCount value>config.MaxJsonBytes then budget "GeoJSON string exceeds output size limit."
                writer.WritePropertyName "id"
                writer.WriteStringValue value
                checkSize()
            | Some(GeoId.Number value) ->
                if isNull value || value.Length>config.MaxJsonBytes then reject "Invalid JSON numeric lexeme."
                if not(isValidJsonNumber value) then reject "Number payloads require one exact JSON numeric lexeme."
                writer.WritePropertyName "id"
                writer.WriteRawValue(value,true)
                checkSize()
            writer.WritePropertyName "geometry"
            checkSize()
            match feature.Geometry with
            | None -> writer.WriteNullValue()
            | Some geom -> emitGeometry (depth+1) geom
            writer.WritePropertyName "properties"
            checkSize()
            match feature.Properties with
            | None -> writer.WriteNullValue()
            | Some props -> emitValue context config (depth+1) writer (GeoValue.Object props)
            for KeyValue(key,value) in feature.ForeignMembers do
                writeProperty key
                emitValue context config (depth+1) writer value
            writer.WriteEndObject()
            checkSize()
        match document with
        | GeoDocument.Geometry geometry -> emitGeometry 1 geometry
        | GeoDocument.Feature feature -> emitFeature 1 feature
        | GeoDocument.FeatureCollection(features,foreign) ->
            context.CheckDepth 2
            validateBbox foreign
            writer.WriteStartObject()
            writer.WritePropertyName "type"
            writer.WriteStringValue "FeatureCollection"
            writer.WritePropertyName "features"
            checkSize()
            writer.WriteStartArray()
            for feature in features do emitFeature 3 feature
            writer.WriteEndArray()
            emitForeign 1 collectionReserved foreign
            writer.WriteEndObject()
            checkSize()

    /// <summary>Serializes bounded RFC 7946 values; rejects Sphere and invalid manually constructed values.</summary>
    let serializeWithCancellation config cancellation document =
        protect config cancellation (fun context ->
#if FABLE_COMPILER
            // Fable has no MemoryStream; the Fable Utf8JsonWriter (FableJson.fs) writes into a string.
            use writer=new Utf8JsonWriter(JsonWriterOptions(MaxDepth=config.MaxDepth+4))
            emitDocument context config writer document
            writer.ToString())
#else
            use output=new MemoryStream()
            use writer=new Utf8JsonWriter(output,JsonWriterOptions(MaxDepth=config.MaxDepth+4))
            emitDocument context config writer document
            writer.Flush()
            Encoding.UTF8.GetString(output.GetBuffer(),0,int output.Length))
#endif

    /// <summary>Serializes RFC 7946 using the default budgets.</summary>
    let serialize document = serializeWithCancellation GeoJsonConfig.defaultValue CancellationToken.None document

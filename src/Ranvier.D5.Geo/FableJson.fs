/// <summary>The part of System.Text.Json that GeoJson uses, for Fable, which has no System.Text.Json. Compiled only under Fable (see the
/// project file); .NET builds use the real namespace. Reading follows RFC 8259 strictly, as JsonDocument does by default. Writing escapes
/// as Utf8JsonWriter's default encoder does, so both platforms produce the same text.</summary>
namespace System.Text.Json

open System
open System.Text

type JsonException(message: string) =
    inherit Exception(message)

type JsonValueKind =
    | Undefined = 0
    | Object = 1
    | Array = 2
    | String = 3
    | Number = 4
    | True = 5
    | False = 6
    | Null = 7

[<AllowNullLiteral>]
type JsonElement(kind: JsonValueKind, raw: string, text: string, items: JsonElement list, members: JsonProperty list) =
    member _.ValueKind = kind
    /// <summary>The value's JSON text; exact for numbers, which is all GeoJson asks for.</summary>
    member _.GetRawText () = raw
    member _.GetString () =
        match kind with
        | JsonValueKind.String -> text
        | JsonValueKind.Null -> null
        | _ -> invalidOp "The element is not a string."
    member _.EnumerateArray () : JsonElement seq =
        if kind <> JsonValueKind.Array then invalidOp "The element is not an array."
        items :> _
    member _.EnumerateObject () : JsonProperty seq =
        if kind <> JsonValueKind.Object then invalidOp "The element is not an object."
        members :> _

and JsonProperty(name: string, value: JsonElement) =
    member _.Name = name
    member _.Value = value

type JsonDocumentOptions() =
    member val MaxDepth = 0 with get, set

type JsonDocument private (root: JsonElement) =
    member _.RootElement = root
    interface IDisposable with
        member _.Dispose () = ()

    static member private ParseCore (json: string, maxDepth: int) =
        let limit = if maxDepth <= 0 then 64 else maxDepth
        let mutable i = 0
        let fail what = raise (JsonException (sprintf "%s at position %d." what i))
        let peek () = if i < json.Length then json[i] else '\000'
        let skipSpace () =
            while i < json.Length && (json[i] = ' ' || json[i] = '\t' || json[i] = '\n' || json[i] = '\r') do
                i <- i + 1
        let expect (literal: string) =
            if i + literal.Length <= json.Length && json.Substring (i, literal.Length) = literal then i <- i + literal.Length
            else fail ("Expected '" + literal + "'")
        let hex () =
            if i + 4 > json.Length then fail "Truncated \\u escape"
            let mutable v = 0
            for k in 0 .. 3 do
                let c = json[i + k]
                let d =
                    if c >= '0' && c <= '9' then int c - int '0'
                    elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
                    elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
                    else fail "Invalid \\u escape"
                v <- v * 16 + d
            i <- i + 4
            char v
        let readString () =
            i <- i + 1
            let builder = StringBuilder ()
            let mutable closed = false
            while not closed do
                if i >= json.Length then fail "Unterminated string"
                let c = json[i]
                if c = '"' then
                    i <- i + 1
                    closed <- true
                elif c = '\\' then
                    i <- i + 1
                    let e = peek ()
                    i <- i + 1
                    match e with
                    | '"' -> builder.Append '"' |> ignore
                    | '\\' -> builder.Append '\\' |> ignore
                    | '/' -> builder.Append '/' |> ignore
                    | 'b' -> builder.Append '\b' |> ignore
                    | 'f' -> builder.Append '\012' |> ignore
                    | 'n' -> builder.Append '\n' |> ignore
                    | 'r' -> builder.Append '\r' |> ignore
                    | 't' -> builder.Append '\t' |> ignore
                    | 'u' -> builder.Append (hex ()) |> ignore
                    | _ -> fail "Invalid escape"
                elif c < ' ' then fail "Control character in string"
                else
                    builder.Append c |> ignore
                    i <- i + 1
            builder.ToString ()
        let readNumber () =
            let start = i
            let digits () =
                let first = i
                while i < json.Length && json[i] >= '0' && json[i] <= '9' do
                    i <- i + 1
                if i = first then fail "Expected a digit"
            if peek () = '-' then i <- i + 1
            if peek () = '0' then i <- i + 1 else digits ()
            if peek () = '.' then
                i <- i + 1
                digits ()
            if peek () = 'e' || peek () = 'E' then
                i <- i + 1
                if peek () = '+' || peek () = '-' then i <- i + 1
                digits ()
            json.Substring (start, i - start)
        let rec readValue depth : JsonElement =
            skipSpace ()
            let start = i
            let raw () = json.Substring (start, i - start)
            match peek () with
            | '{' ->
                if depth >= limit then fail "The maximum configured depth is exceeded"
                i <- i + 1
                let members = ResizeArray<JsonProperty> ()
                skipSpace ()
                if peek () = '}' then i <- i + 1
                else
                    let mutable more = true
                    while more do
                        skipSpace ()
                        if peek () <> '"' then fail "Expected a property name"
                        let name = readString ()
                        skipSpace ()
                        expect ":"
                        let value = readValue (depth + 1)
                        members.Add (JsonProperty (name, value))
                        skipSpace ()
                        match peek () with
                        | ',' -> i <- i + 1
                        | '}' ->
                            i <- i + 1
                            more <- false
                        | _ -> fail "Expected ',' or '}'"
                JsonElement (JsonValueKind.Object, raw (), null, [], List.ofSeq members)
            | '[' ->
                if depth >= limit then fail "The maximum configured depth is exceeded"
                i <- i + 1
                let items = ResizeArray<JsonElement> ()
                skipSpace ()
                if peek () = ']' then i <- i + 1
                else
                    let mutable more = true
                    while more do
                        items.Add (readValue (depth + 1))
                        skipSpace ()
                        match peek () with
                        | ',' -> i <- i + 1
                        | ']' ->
                            i <- i + 1
                            more <- false
                        | _ -> fail "Expected ',' or ']'"
                JsonElement (JsonValueKind.Array, raw (), null, List.ofSeq items, [])
            | '"' ->
                let text = readString ()
                JsonElement (JsonValueKind.String, raw (), text, [], [])
            | 't' ->
                expect "true"
                JsonElement (JsonValueKind.True, "true", null, [], [])
            | 'f' ->
                expect "false"
                JsonElement (JsonValueKind.False, "false", null, [], [])
            | 'n' ->
                expect "null"
                JsonElement (JsonValueKind.Null, "null", null, [], [])
            | c when c = '-' || (c >= '0' && c <= '9') ->
                let text = readNumber ()
                JsonElement (JsonValueKind.Number, text, null, [], [])
            | _ -> fail "Unexpected character"
        if isNull json then nullArg "json"
        let root = readValue 0
        skipSpace ()
        if i <> json.Length then fail "Unexpected data after the JSON value"
        new JsonDocument (root)

    static member Parse (json: string) = JsonDocument.ParseCore (json, 0)
    static member Parse (json: string, options: JsonDocumentOptions) = JsonDocument.ParseCore (json, options.MaxDepth)

type JsonWriterOptions() =
    member val MaxDepth = 0 with get, set

/// <summary>Writes compact JSON into a string. BytesPending counts UTF-8 bytes written so far; BytesCommitted stays 0.</summary>
type Utf8JsonWriter(options: JsonWriterOptions) =
    let output = StringBuilder ()
    let mutable bytes = 0L
    // One entry per open container: true until its first element is written.
    let first = Collections.Generic.Stack<bool> ()
    let mutable afterName = false
    let counter = Ranvier.D5.FableCompat.Utf8Counter ()
    let append (text: string) =
        output.Append text |> ignore
        bytes <- bytes + int64 (counter.GetByteCount text)
    let separate () =
        if afterName then afterName <- false
        elif first.Count > 0 then
            if first.Peek () then
                first.Pop () |> ignore
                first.Push false
            else append ","
    /// <summary>Escapes as JavaScriptEncoder.Default: quotes, backslash, control characters, HTML-sensitive characters and
    /// everything outside printable ASCII.</summary>
    let quote (text: string) =
        let builder = StringBuilder ()
        builder.Append '"' |> ignore
        for c in text do
            match c with
            | '"' -> builder.Append "\\u0022" |> ignore
            | '\\' -> builder.Append "\\\\" |> ignore
            | '\b' -> builder.Append "\\b" |> ignore
            | '\012' -> builder.Append "\\f" |> ignore
            | '\n' -> builder.Append "\\n" |> ignore
            | '\r' -> builder.Append "\\r" |> ignore
            | '\t' -> builder.Append "\\t" |> ignore
            | c when c < ' ' || c > '~' || c = '<' || c = '>' || c = '&' || c = '\'' || c = '+' || c = '`' ->
                builder.Append("\\u").Append((int c).ToString "X4") |> ignore
            | c -> builder.Append c |> ignore
        builder.Append '"' |> ignore
        builder.ToString ()
    let value (text: string) =
        separate ()
        append text
    let start (bracket: string) =
        separate ()
        if options.MaxDepth > 0 && first.Count >= options.MaxDepth then invalidOp "The maximum configured depth is exceeded."
        append bracket
        first.Push true
    let finish (bracket: string) =
        first.Pop () |> ignore
        append bracket

    member _.BytesCommitted = 0L
    member _.BytesPending = int bytes
    member _.WriteNullValue () = value "null"
    member _.WriteBooleanValue (b: bool) = value (if b then "true" else "false")
    member _.WriteStringValue (text: string) = value (quote text)
    member _.WriteRawValue (json: string, _skipValidation: bool) = value json
    member _.WriteStartArray () = start "["
    member _.WriteEndArray () = finish "]"
    member _.WriteStartObject () = start "{"
    member _.WriteEndObject () = finish "}"
    member _.WritePropertyName (name: string) =
        separate ()
        append (quote name)
        append ":"
        afterName <- true
    member _.Flush () = ()
    override _.ToString () = output.ToString ()
    interface IDisposable with
        member _.Dispose () = ()

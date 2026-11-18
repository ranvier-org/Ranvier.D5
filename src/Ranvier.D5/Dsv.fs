namespace Ranvier.D5

open System
#if !FABLE_COMPILER
open System.Buffers
#endif
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Threading

/// <summary>Delimiter and budgets for delimiter-separated values.</summary>
type DsvConfig = {
    /// <summary>Field delimiter; it cannot be a quote, carriage return or line feed.</summary>
    Delimiter: char
    /// <summary>Maximum input characters.</summary>
    MaxCharacters: int
    /// <summary>Maximum records, including the header.</summary>
    MaxRows: int
    /// <summary>Maximum fields in a record.</summary>
    MaxColumns: int
}

/// <summary>Comma and tab presets with ten-million-character, one-million-row and ten-thousand-column budgets.</summary>
[<RequireQualifiedAccess>]
module DsvConfig =
    let private create delimiter={Delimiter=delimiter;MaxCharacters=10_000_000;MaxRows=1_000_000;MaxColumns=10_000}
    /// <summary>Comma-separated values.</summary>
    let csv=create ','
    /// <summary>Tab-separated values.</summary>
    let tsv=create '\t'

/// <summary>A header and rows of equal width; every cell is the field's exact text.</summary>
type DsvTable = { Header:string list; Rows:string list list }

/// <summary>One data record with its 1-based source line for error messages.</summary>
type DsvRow = private { Line:int; Columns:Map<string,int>; Values:string array }

/// <summary>Typed access to a record's named fields; errors name the line and column.</summary>
[<RequireQualifiedAccess>]
module DsvRow =
    let private fail row name message=Error [InvalidObservation(sprintf "Line %d, column %s: %s" row.Line name message)]
    /// <summary>The 1-based source line where the record starts.</summary>
    let line row=row.Line
    /// <summary>The exact field text.</summary>
    let text name row : Result<string,LayoutError list> =
        match Map.tryFind name row.Columns with
        | Some index -> Ok row.Values[index]
        | None -> fail row name "unknown column."
    /// <summary>None for an empty field, otherwise the converted value.</summary>
    let optional (convert:string -> DsvRow -> Result<'T,LayoutError list>) name row : Result<'T option,LayoutError list> =
        text name row |> Result.bind(fun value -> if value="" then Ok None else convert name row |> Result.map Some)
    /// <summary>A finite number parsed strictly in the given culture.</summary>
    let number culture name row =
        text name row |> Result.bind(fun value ->
            match Parse.number culture value with
            | Ok number -> Ok number
            | Error _ -> fail row name (sprintf "'%s' is not a finite number." value))
    /// <summary>A whole number within the 32-bit range, in the given culture.</summary>
    let integer (culture:CultureInfo) name row =
        text name row |> Result.bind(fun value ->
#if FABLE_COMPILER
            // Fable parses invariant integers and has no NumberStyles; AllowLeadingSign rejects surrounding whitespace.
            ignore culture
            match (if isNull value || value.Trim()<>value then false,0 else Int32.TryParse value) with
#else
            match Int32.TryParse(value,NumberStyles.AllowLeadingSign,culture) with
#endif
            | true,number -> Ok number
            | _ -> fail row name (sprintf "'%s' is not an integer." value))
    /// <summary>true or false, case-insensitively.</summary>
    let boolean name row =
        text name row |> Result.bind(fun value ->
            if String.Equals(value,"true",StringComparison.OrdinalIgnoreCase) then Ok true
            elif String.Equals(value,"false",StringComparison.OrdinalIgnoreCase) then Ok false
            else fail row name (sprintf "'%s' is not true or false." value))
    /// <summary>A time in an exact pattern, placed in an explicit zone by the policy.</summary>
    let time culture zone pattern policy name row =
        text name row |> Result.bind(fun value ->
            match Parse.time culture zone pattern policy value with
            | Ok time -> Ok time
            | Error _ -> fail row name (sprintf "'%s' is not a valid %s time in %s." value pattern (Zone.id zone)))

/// <summary>Strict RFC 4180 parsing and formatting for comma, tab and other single-character delimiters.</summary>
/// <remarks>Quotes are recognised only at the start of a field and escape themselves by doubling. Fields may contain delimiters and line breaks inside quotes. CRLF, LF and CR end records; a leading byte-order mark and one trailing line break are ignored. Errors name the line and column and never return a partial table.</remarks>
[<RequireQualifiedAccess>]
module Dsv =
    exception private Rejected of LayoutError

#if !FABLE_COMPILER
    let private csvSpecial = SearchValues.Create ",\"\r\n"
    let private tsvSpecial = SearchValues.Create "\t\"\r\n"
    let private quoteSpecial = SearchValues.Create "\"\r\n"
    let inline private specialFor delimiter =
        if delimiter = ',' then csvSpecial
        elif delimiter = '\t' then tsvSpecial
        else SearchValues.Create([| delimiter; '"'; '\r'; '\n' |])
#endif

    let private validConfig config =
        config.Delimiter<>'"' && config.Delimiter<>'\r' && config.Delimiter<>'\n' &&
        config.MaxCharacters>=0 && config.MaxRows>=1 && config.MaxColumns>=1

    /// <summary>Parses every record, checking cancellation per record.</summary>
    let parseRowsWithCancellation (config:DsvConfig) (token:CancellationToken) (text:string) : Result<string list list,LayoutError list> =
        if isNull text then nullArg "text"
        if not(validConfig config) then Error [InvalidObservation "DSV delimiter cannot be a quote or line break, and budgets must be positive."]
        elif text.Length>config.MaxCharacters then Error [BudgetExceeded "DSV input exceeds its character budget."]
        else
            try
                let start=if text.Length>0 && text[0]='\uFEFF' then 1 else 0
                if start>=text.Length then Ok []
                else
#if !FABLE_COMPILER
                    let special = specialFor config.Delimiter
                    let rows = ResizeArray<string list>()
                    let mutable fieldBuf = Array.zeroCreate<string> 16
                    let mutable fieldCount = 0
                    let mutable sb: StringBuilder = null
                    let mutable index = start
                    let mutable line = 1
                    let mutable column = 1
                    let fail message = raise(Rejected(InvalidObservation(sprintf "Line %d, column %d: %s" line column message)))
                    let pushField (value: string) =
                        if fieldCount >= config.MaxColumns then raise(Rejected(BudgetExceeded "DSV record exceeds its column budget."))
                        if fieldCount = fieldBuf.Length then
                            let nextCap = min config.MaxColumns (fieldBuf.Length * 2)
                            let grown = Array.zeroCreate<string> (max (fieldCount + 1) nextCap)
                            Array.Copy(fieldBuf, grown, fieldCount)
                            fieldBuf <- grown
                        fieldBuf[fieldCount] <- value
                        fieldCount <- fieldCount + 1
                    let emitRecord () =
                        token.ThrowIfCancellationRequested()
                        if rows.Count >= config.MaxRows then raise(Rejected(BudgetExceeded "DSV input exceeds its row budget."))
                        let mutable rowList = []
                        for j = fieldCount - 1 downto 0 do
                            rowList <- fieldBuf[j] :: rowList
                        rows.Add rowList
                        fieldCount <- 0
                    let newline () =
                        if text[index] = '\r' && index + 1 < text.Length && text[index + 1] = '\n' then index <- index + 1
                        index <- index + 1
                        line <- line + 1
                        column <- 1
                    let mutable trailingDelimiter = false
                    while index < text.Length do
                        let pos = text.AsSpan(index).IndexOfAny special
                        if pos < 0 then
                            let fld = text.Substring index
                            pushField fld
                            emitRecord ()
                            trailingDelimiter <- false
                            index <- text.Length
                        else
                            let stopIdx = index + pos
                            let stopChar = text[stopIdx]
                            if stopChar = '"' then
                                if pos > 0 then
                                    column <- column + pos
                                    fail "a quote may only start a field."
                                index <- index + 1
                                column <- column + 1
                                // Fast path for unescaped single-line quoted fields (e.g. "q123, 0")
                                let qPos = text.AsSpan(index).IndexOfAny quoteSpecial
                                let mutable valStr: string = null
                                if qPos >= 0 && text[index + qPos] = '"' && (index + qPos + 1 >= text.Length || text[index + qPos + 1] <> '"') then
                                    valStr <- if qPos = 0 then "" else text.Substring(index, qPos)
                                    index <- index + qPos + 1
                                    column <- column + qPos + 1
                                else
                                    if isNull sb then sb <- StringBuilder() else sb.Clear() |> ignore
                                    let mutable closed = false
                                    while not closed do
                                        if index >= text.Length then fail "unterminated quoted field."
                                        let subPos = text.AsSpan(index).IndexOfAny quoteSpecial
                                        if subPos < 0 then
                                            fail "unterminated quoted field."
                                        if subPos > 0 then
                                            sb.Append(text, index, subPos) |> ignore
                                            index <- index + subPos
                                            column <- column + subPos
                                        let q = text[index]
                                        if q = '"' then
                                            if index + 1 < text.Length && text[index + 1] = '"' then
                                                sb.Append '"' |> ignore
                                                index <- index + 2
                                                column <- column + 2
                                            else
                                                index <- index + 1
                                                column <- column + 1
                                                closed <- true
                                        else
                                            if q = '\r' && index + 1 < text.Length && text[index + 1] = '\n' then
                                                sb.Append "\r\n" |> ignore
                                            else
                                                sb.Append q |> ignore
                                            newline ()
                                    valStr <- sb.ToString()
                                if index >= text.Length then
                                    pushField valStr
                                    emitRecord ()
                                    trailingDelimiter <- false
                                else
                                    let after = text[index]
                                    if after = config.Delimiter then
                                        pushField valStr
                                        index <- index + 1
                                        column <- column + 1
                                        trailingDelimiter <- true
                                    elif after = '\r' || after = '\n' then
                                        pushField valStr
                                        emitRecord ()
                                        newline ()
                                        trailingDelimiter <- false
                                    else
                                        fail "a closing quote must end its field."
                            elif stopChar = config.Delimiter then
                                let fld = if pos = 0 then "" else text.Substring(index, pos)
                                pushField fld
                                index <- stopIdx + 1
                                column <- column + pos + 1
                                trailingDelimiter <- true
                            else
                                // '\r' or '\n'
                                let fld = if pos = 0 then "" else text.Substring(index, pos)
                                pushField fld
                                emitRecord ()
                                index <- stopIdx
                                newline ()
                                trailingDelimiter <- false
                    if trailingDelimiter then
                        pushField ""
                        emitRecord ()
                    Ok(Data.resizeToList rows)
#else
                    let rows=ResizeArray<string list>()
                    let fields=ResizeArray<string>()
                    let field=StringBuilder()
                    let mutable index=start
                    let mutable line=1
                    let mutable column=1
                    let fail message=raise(Rejected(InvalidObservation(sprintf "Line %d, column %d: %s" line column message)))
                    let endField () =
                        if fields.Count>=config.MaxColumns then raise(Rejected(BudgetExceeded "DSV record exceeds its column budget."))
                        fields.Add(field.ToString())
                        field.Clear() |> ignore
                    let endRecord () =
                        token.ThrowIfCancellationRequested()
                        endField()
                        if rows.Count>=config.MaxRows then raise(Rejected(BudgetExceeded "DSV input exceeds its row budget."))
                        rows.Add(Data.resizeToList fields)
                        fields.Clear()
                    let newline () =
                        if text[index]='\r' && index+1<text.Length && text[index+1]='\n' then index<-index+1
                        index<-index+1
                        line<-line+1
                        column<-1
                    let mutable atFieldStart=true
                    while index<text.Length do
                        let c=text[index]
                        if atFieldStart && c='"' then
                            index<-index+1
                            column<-column+1
                            let mutable closed=false
                            while not closed do
                                if index>=text.Length then fail "unterminated quoted field."
                                let q=text[index]
                                if q='"' then
                                    if index+1<text.Length && text[index+1]='"' then
                                        field.Append '"' |> ignore
                                        index<-index+2
                                        column<-column+2
                                    else
                                        index<-index+1
                                        column<-column+1
                                        closed<-true
                                elif q='\r' || q='\n' then
                                    field.Append(if q='\r' && index+1<text.Length && text[index+1]='\n' then "\r\n" else string q) |> ignore
                                    newline()
                                else
                                    field.Append q |> ignore
                                    index<-index+1
                                    column<-column+1
                            if index<text.Length && text[index]<>config.Delimiter && text[index]<>'\r' && text[index]<>'\n' then
                                fail "a closing quote must end its field."
                            atFieldStart<-false
                        elif c=config.Delimiter then
                            endField()
                            index<-index+1
                            column<-column+1
                            atFieldStart<-true
                        elif c='\r' || c='\n' then
                            endRecord()
                            newline()
                            atFieldStart<-true
                            if index>=text.Length then field.Clear() |> ignore
                        elif c='"' then fail "a quote may only start a field."
                        else
                            field.Append c |> ignore
                            index<-index+1
                            column<-column+1
                            atFieldStart<-false
                    let last=text[text.Length-1]
                    if not(last='\n' || last='\r') || fields.Count>0 || field.Length>0 then endRecord()
                    Ok(Data.resizeToList rows)
#endif
            with Rejected error -> Error [error]

    /// <summary>Parses every record without cancellation.</summary>
    let parseRows config text = parseRowsWithCancellation config CancellationToken.None text

    /// <summary>Parses a header and rows; header names must be non-empty and unique, and every row must match its width.</summary>
    let parseWithCancellation config token text : Result<DsvTable,LayoutError list> =
        parseRowsWithCancellation config token text |> Result.bind(fun rows ->
            match rows with
            | [] -> Error [InvalidObservation "DSV input has no header."]
            | header::dataRows ->
                let errors = ResizeArray<LayoutError>()
                let headerWidth = List.length header
                for name in header do
                    if String.IsNullOrEmpty name then
                        errors.Add(InvalidObservation "DSV header names cannot be empty.")
                let seen = Dictionary<string, int>(headerWidth, StringComparer.Ordinal)
                let order = ResizeArray<string>(headerWidth)
                for name in header do
                    if name <> "" then
                        match seen.TryGetValue name with
                        | true, c -> seen[name] <- c + 1
                        | false, _ ->
                            seen.Add(name, 1)
                            order.Add name
                for name in order do
                    if seen[name] > 1 then errors.Add(DuplicateId name)
                let mutable rowIndex = 0
                let mutable widthErrCount = 0
                for row in dataRows do
                    if widthErrCount < 100 then
                        let rowLen = List.length row
                        if rowLen <> headerWidth then
                            errors.Add(InvalidObservation(sprintf "Record %d has %d fields; the header has %d." (rowIndex + 2) rowLen headerWidth))
                            widthErrCount <- widthErrCount + 1
                    rowIndex <- rowIndex + 1
                if errors.Count = 0 then Ok { Header = header; Rows = dataRows }
                else Error(Data.resizeToList errors))

    /// <summary>Parses a header and rows without cancellation.</summary>
    let parse config text = parseWithCancellation config CancellationToken.None text

    /// <summary>Converts each row, collecting up to 100 errors instead of stopping at the first or dropping rows.</summary>
    let convert (map:DsvRow -> Result<'T,LayoutError list>) (table:DsvTable) : Result<'T list,LayoutError list> =
        let columns=table.Header |> List.mapi(fun index name -> name,index) |> Map.ofList
        let results=table.Rows |> List.mapi(fun index row -> map {Line=index+2;Columns=columns;Values=List.toArray row})
        match results |> List.collect(function Error errors -> errors | Ok _ -> []) |> List.truncate 100 with
        | [] -> Ok(results |> List.choose(function Ok value -> Some value | Error _ -> None))
        | errors -> Error errors

    let private quoteIfNeeded delimiter (value:string) =
        if isNull value then nullArg "value"
#if !FABLE_COMPILER
        let special = specialFor delimiter
        if value.AsSpan().IndexOfAny special >= 0 then "\""+value.Replace("\"","\"\"")+"\"" else value
#else
        if value.IndexOfAny([|delimiter;'"';'\r';'\n'|])>=0 then "\""+value.Replace("\"","\"\"")+"\"" else value
#endif

    /// <summary>Formats records with LF line ends, quoting only fields that need it.</summary>
    let formatRows (config:DsvConfig) (rows:string list list) =
        if not(validConfig config) then invalidArg "config" "DSV delimiter cannot be a quote or line break."
        let builder=StringBuilder()
        for row in rows do
#if FABLE_COMPILER
            builder.Append(String.Join(string config.Delimiter,row |> List.map(quoteIfNeeded config.Delimiter))).Append('\n') |> ignore
#else
            let mutable first = true
            for cell in row do
                if not first then builder.Append config.Delimiter |> ignore
                first <- false
                builder.Append(quoteIfNeeded config.Delimiter cell) |> ignore
            builder.Append '\n' |> ignore
#endif
        builder.ToString()

    /// <summary>Formats the header and rows; parsing the result returns an equal table.</summary>
    let format config (table:DsvTable) = formatRows config (table.Header::table.Rows)

    /// <summary>Reads text asynchronously within the character budget, then parses it with cancellation.</summary>
    let readAsync (config:DsvConfig) (reader:TextReader) (token:CancellationToken) =
#if FABLE_COMPILER
        // Fable implements no TextReader reads; JavaScript callers read the text themselves and call parse.
        ignore (config,reader,token)
        task { return Error [InvalidObservation "Dsv.readAsync is not available under Fable: TextReader cannot be read."] }
#else
        task {
            let buffer=Array.zeroCreate<char> 65536
            let builder=StringBuilder()
            let mutable finished=false
            let mutable exceeded=false
            while not finished && not exceeded do
                let! count=reader.ReadAsync(Memory(buffer),token)
                if count=0 then finished<-true
                elif builder.Length+count>config.MaxCharacters then exceeded<-true
                else builder.Append(buffer,0,count) |> ignore
            return
                if exceeded then Error [BudgetExceeded "DSV input exceeds its character budget."]
                else parseWithCancellation config token (builder.ToString())
        }
#endif

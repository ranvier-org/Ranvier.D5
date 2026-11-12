module Ranvier.D5.Tests.DsvTests

open System
open System.Globalization
open System.IO
open System.Threading
open Expecto
open Ranvier.D5

let private ok = function Ok value -> value | Error errors -> failtestf "%A" errors
let private invariant=CultureInfo.InvariantCulture
let private errorText = function Error errors -> sprintf "%A" errors | Ok value -> failtestf "Expected an error, got %A" value

[<Tests>]
let tests = testList "delimiter-separated values" [
    testCase "parses RFC 4180 quoting, embedded breaks, BOM and every line ending" <| fun _ ->
        let text="﻿name,note,score\r\n\"Smith, J\",\"said \"\"hi\"\"\",3\n\"two\r\nlines\",,4\rlast,\"x\ny\",5\n"
        let table=Dsv.parse DsvConfig.csv text |> ok
        Expect.equal table.Header ["name";"note";"score"] "BOM stripped from the first header"
        Expect.equal table.Rows [["Smith, J";"said \"hi\"";"3"];["two\r\nlines";"";"4"];["last";"x\ny";"5"]] "exact field text"
    testCase "a trailing line break is optional and adds no record" <| fun _ ->
        for text in ["a,b\n1,2";"a,b\n1,2\n";"a,b\r\n1,2\r\n"] do
            Expect.equal (Dsv.parse DsvConfig.csv text |> ok).Rows [["1";"2"]] "one data record"
        Expect.equal (Dsv.parseRows DsvConfig.csv "a,\n,") (Ok [["a";""];["";""]]) "trailing empty fields kept"
        Expect.equal (Dsv.parseRows DsvConfig.csv "") (Ok []) "empty input has no records"
    testCase "tab-separated values use the same rules" <| fun _ ->
        let table=Dsv.parse DsvConfig.tsv "a\tb\n\"x\ty\"\t2\n" |> ok
        Expect.equal table.Rows [["x\ty";"2"]] "quoted tab"
    testCase "malformed input reports line and column without a partial table" <| fun _ ->
        Expect.stringContains (errorText(Dsv.parseRows DsvConfig.csv "a,b\n1,\"open")) "Line 2" "unterminated quote"
        Expect.stringContains (errorText(Dsv.parseRows DsvConfig.csv "a,b\n1,x\"y")) "Line 2, column 4" "quote inside a field"
        Expect.stringContains (errorText(Dsv.parseRows DsvConfig.csv "a,b\n\"q\"x,1")) "closing quote" "text after a closing quote"
        Expect.stringContains (errorText(Dsv.parse DsvConfig.csv "a,b\n1\n2,3,4")) "Record 2" "ragged record"
        Expect.stringContains (errorText(Dsv.parse DsvConfig.csv "a,a\n1,2")) "DuplicateId" "duplicate header"
        Expect.stringContains (errorText(Dsv.parse DsvConfig.csv "a,\n1,2")) "empty" "empty header name"
        Expect.isError (Dsv.parse DsvConfig.csv "") "no header"
    testCase "budgets and invalid delimiters are typed errors" <| fun _ ->
        Expect.stringContains (errorText(Dsv.parseRows {DsvConfig.csv with MaxCharacters=3} "a,b,c")) "character budget" "characters"
        Expect.stringContains (errorText(Dsv.parseRows {DsvConfig.csv with MaxRows=1} "a\nb")) "row budget" "rows"
        Expect.stringContains (errorText(Dsv.parseRows {DsvConfig.csv with MaxColumns=2} "a,b,c")) "column budget" "columns"
        Expect.isError (Dsv.parseRows {DsvConfig.csv with Delimiter='"'} "a") "quote delimiter"
    testCase "formatting quotes only where needed and round-trips arbitrary text" <| fun _ ->
        Expect.equal (Dsv.format DsvConfig.csv {Header=["a";"b"];Rows=[["plain";"x,y"];["q\"";"line\nbreak"]]}) "a,b\nplain,\"x,y\"\n\"q\"\"\",\"line\nbreak\"\n" "minimal quoting"
        let random=Random 42
        let alphabet=[|"a";"b";",";"\"";"\n";"\r\n";" ";"\t";"é";""|]
        for _ in 1..200 do
            let width=random.Next(1,5)
            let cell()=String.Join("",Array.init (random.Next(0,6)) (fun _ -> alphabet[random.Next alphabet.Length]))
            let header=[for column in 1..width -> sprintf "c%d" column]
            let table={Header=header;Rows=[for _ in 1..random.Next(0,5) -> [for _ in 1..width -> cell()]]}
            for config in [DsvConfig.csv;DsvConfig.tsv] do
                Expect.equal (Dsv.parse config (Dsv.format config table) |> ok) table "parse inverts format"
    testCase "typed conversion names the line and column and collects every error" <| fun _ ->
        let table=Dsv.parse DsvConfig.csv "id,value,flag,count\na,1.5,true,3\nb,oops,maybe,\nc,-2e3,FALSE,7\n" |> ok
        let convert row =
            match DsvRow.text "id" row,DsvRow.number invariant "value" row,DsvRow.boolean "flag" row,DsvRow.optional (DsvRow.integer invariant) "count" row with
            | Ok id,Ok value,Ok flag,Ok count -> Ok(id,value,flag,count)
            | a,b,c,d -> Error([a |> Result.map ignore;b |> Result.map ignore;c |> Result.map ignore;d |> Result.map ignore] |> List.collect(function Error e -> e | Ok() -> []))
        let message=errorText(Dsv.convert convert table)
        Expect.stringContains message "Line 3, column value" "number error located"
        Expect.stringContains message "Line 3, column flag" "boolean error located"
        let valid=Dsv.parse DsvConfig.csv "id,value,flag,count\na,1.5,true,3\nc,-2e3,FALSE,\n" |> ok
        Expect.equal (Dsv.convert convert valid |> ok) [("a",1.5,true,Some 3);("c",-2000.,false,None)] "converted rows"
        Expect.isError (DsvRow.text "missing" (Dsv.convert (fun row -> Ok row) valid |> ok).Head) "unknown column"
    testCase "asynchronous reading honours budgets and cancellation" <| fun _ ->
        let table=(Dsv.readAsync DsvConfig.csv (new StringReader("a,b\n1,2\n")) CancellationToken.None).Result |> ok
        Expect.equal table.Rows [["1";"2"]] "read"
        let large=(Dsv.readAsync {DsvConfig.csv with MaxCharacters=4} (new StringReader("a,b\n1,2\n")) CancellationToken.None).Result
        Expect.isError large "budget"
        use cancelled=new CancellationTokenSource()
        cancelled.Cancel()
        Expect.throws (fun () -> (Dsv.readAsync DsvConfig.csv (new StringReader("a\n1")) cancelled.Token).Result |> ignore) "cancelled read"
    testCase "number parsing is strict and culture-explicit" <| fun _ ->
        Expect.equal (Parse.number invariant "-1.25e2") (Ok -125.) "sign and exponent"
        Expect.equal (Parse.number (CultureInfo.GetCultureInfo "de-DE") "3,5") (Ok 3.5) "culture decimal comma"
        for text in [" 1";"1,000";"NaN";"Infinity";"1e400";"";"0x10"] do
            Expect.isError (Parse.number invariant text) (sprintf "rejects %A" text)
    testCase "local times resolve daylight-saving gaps and folds by explicit policy" <| fun _ ->
        let zone=TimeZoneInfo.FindSystemTimeZoneById "America/New_York"
        let parse ambiguous invalid text=Parse.time invariant zone "yyyy-MM-dd HH:mm" {Ambiguous=ambiguous;Invalid=invalid} text
        Expect.equal (parse AmbiguousTime.Reject InvalidTime.Reject "2026-07-04 12:00") (Ok(DateTimeOffset(2026,7,4,12,0,0,TimeSpan.FromHours -4.))) "summer offset"
        Expect.isError (parse AmbiguousTime.Reject InvalidTime.Reject "2026-03-08 02:30") "gap rejected"
        Expect.equal ((parse AmbiguousTime.Reject InvalidTime.ShiftForward "2026-03-08 02:30" |> ok).UtcDateTime) (DateTime(2026,3,8,7,30,0)) "gap shifted forward by the hour"
        Expect.isError (parse AmbiguousTime.Reject InvalidTime.Reject "2026-11-01 01:30") "fold rejected"
        Expect.equal ((parse AmbiguousTime.Earlier InvalidTime.Reject "2026-11-01 01:30" |> ok).UtcDateTime) (DateTime(2026,11,1,5,30,0)) "first occurrence"
        Expect.equal ((parse AmbiguousTime.Later InvalidTime.Reject "2026-11-01 01:30" |> ok).UtcDateTime) (DateTime(2026,11,1,6,30,0)) "second occurrence"
        Expect.equal (Parse.time invariant zone "yyyy-MM-ddTHH:mmzzz" {Ambiguous=AmbiguousTime.Reject;Invalid=InvalidTime.Reject} "2026-01-02T03:04+05:30") (Ok(DateTimeOffset(2026,1,2,3,4,0,TimeSpan(5,30,0)))) "explicit offset wins"
        Expect.isError (parse AmbiguousTime.Earlier InvalidTime.Reject "2026-13-01 00:00") "pattern mismatch"
]

module Ranvier.D5.Tests.FixtureTests

open System
open System.IO
open System.Text.Json
open Expecto

let tests = testList "reference fixture provenance" [
    testCase "rejects absent provenance and invalid tolerance" <| fun _ ->
        let valid = """{"seed":42,"source":{"d3-array":"3.2.4"},"coordinates":"SVG x-right y-down; radians clockwise","tolerance":1e-9}"""
        Expect.isEmpty (FixtureManifest.validate valid) "complete provenance"
        for field in ["seed";"source";"coordinates";"tolerance"] do
            let node = System.Text.Json.Nodes.JsonNode.Parse valid
            node.AsObject().Remove field |> ignore
            Expect.isNonEmpty (FixtureManifest.validate (node.ToJsonString())) ("missing " + field)
        Expect.isNonEmpty (FixtureManifest.validate (valid.Replace("1e-9", "-1"))) "negative tolerance"
    testCase "checked-in reference files carry valid provenance" <| fun _ ->
        let directory = Path.Combine(AppContext.BaseDirectory, "fixtures")
        let manifest = File.ReadAllText(Path.Combine(directory, "manifest.json"))
        Expect.isEmpty (FixtureManifest.validate manifest) "manifest"
        use json = JsonDocument.Parse manifest
        for file in json.RootElement.GetProperty("files").EnumerateArray() do
            let path = Path.Combine(directory, file.GetString())
            use fixture = JsonDocument.Parse(File.ReadAllText path)
            let populated=
                match fixture.RootElement.ValueKind with
                | JsonValueKind.Array -> fixture.RootElement.GetArrayLength()>0
                | JsonValueKind.Object -> fixture.RootElement.EnumerateObject() |> Seq.isEmpty |> not
                | _ -> false
            Expect.isTrue populated "fixture cases exist"
]

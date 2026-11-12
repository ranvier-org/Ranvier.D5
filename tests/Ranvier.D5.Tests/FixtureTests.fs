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
    testCase "spherical references preserve units winding and independent polar bounds" <| fun _ ->
        let path = Path.Combine(AppContext.BaseDirectory, "fixtures", "spherical.json")
        Expect.isTrue (File.Exists path) "geographic fixtures are shipped with the tests"
        let content = File.ReadAllText path
        Expect.isEmpty (FixtureManifest.validate content) "geographic provenance"
        use json = JsonDocument.Parse content
        let root = json.RootElement
        Expect.equal (root.GetProperty("source").GetProperty("d3-geo").GetString()) "3.1.1" "pinned reference"
        let coordinates = root.GetProperty("coordinates").GetString()
        for unit in ["degrees"; "radians"; "steradians"; "clockwise"] do
            Expect.stringContains coordinates unit "explicit units and ring convention"
        let geometries = root.GetProperty("geometries").EnumerateArray() |> Seq.toList
        let area name =
            geometries |> List.find (fun item -> item.GetProperty("name").GetString() = name)
            |> fun item -> item.GetProperty("area").GetDouble()
        Expect.isLessThan (abs(area "octant" - Math.PI / 2.)) 1e-12 "independent octant area"
        Expect.isLessThan (abs(area "octant" + area "octant-complement" - 4. * Math.PI)) 1e-12 "orientation selects spherical complement"
        Expect.isLessThan (area "square-with-hole") (area "small-square") "hole subtracts area"
        let independent = root.GetProperty("independentExpectations").EnumerateArray() |> Seq.exactlyOne
        Expect.equal (independent.GetProperty("name").GetString()) "north-cap" "known conservative reference bound"
        let bounds = independent.GetProperty("bounds")
        Expect.equal (bounds.[0].[1].GetDouble(), bounds.[1].[1].GetDouble()) (80.,90.) "hand-derived tight polar extent"
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

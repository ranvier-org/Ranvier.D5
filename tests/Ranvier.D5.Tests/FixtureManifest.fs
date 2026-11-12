module Ranvier.D5.Tests.FixtureManifest

open System
open System.Text.Json

let validate (text: string) =
    try
        use document = JsonDocument.Parse text
        let root = document.RootElement
        let property name =
            match root.TryGetProperty(name: string) with true, value -> Some value | _ -> None
        let isValid name predicate = property name |> Option.exists predicate
        [
            if not (isValid "seed" (fun v -> v.ValueKind = JsonValueKind.Number && fst(v.TryGetInt64()))) then "seed"
            if not (isValid "source" (fun v -> v.ValueKind = JsonValueKind.Object && Seq.isEmpty(v.EnumerateObject()) |> not)) then "source"
            elif property "source" |> Option.exists(fun v -> v.EnumerateObject() |> Seq.exists(fun p -> p.Value.ValueKind <> JsonValueKind.String || String.IsNullOrWhiteSpace(p.Value.GetString()))) then "version"
            if not (isValid "coordinates" (fun v -> v.ValueKind = JsonValueKind.String && not(String.IsNullOrWhiteSpace(v.GetString())))) then "coordinates"
            if not (isValid "tolerance" (fun v -> v.ValueKind = JsonValueKind.Number && Double.IsFinite(v.GetDouble()) && v.GetDouble() > 0.)) then "tolerance"
        ]
    with :? JsonException | :? InvalidOperationException -> ["invalid JSON manifest"]

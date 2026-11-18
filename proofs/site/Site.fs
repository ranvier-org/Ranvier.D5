/// <summary>The Navigator site of the Rocq D5 proofs. Run through <c>proofs/site.sh</c>, which compiles the proofs and captures their facts first.</summary>
module Ranvier.D5.Proofs.Site

open System
open Nacara.Core
open Nacara.Plugins

/// <summary>The site's path on its host, ending in a slash. <c>PROOFS_BASE_URL</c> overrides the root path.</summary>
let private baseUrl =
    match Environment.GetEnvironmentVariable "PROOFS_BASE_URL" with
    | null
    | "" -> "/"
    | value -> value

[<EntryPoint>]
let main args =
    let options =
        Rocq.defaults
        |> Rocq.project "../_CoqProject"
        |> Rocq.facts (Some ".work/facts.json")
        |> Rocq.config (Some "navigator.json")
        |> Rocq.home (Some ("Ranvier.D5 repository", "https://github.com/ranvier-org/Ranvier.D5"))

    let site =
        Site.create "Ranvier.D5 proofs"
        |> Site.baseUrl baseUrl
        |> Site.output "output"
        |> Site.noStaticFiles
        |> Rocq.register options
        |> LinkValidator.register
        |> Site.collection (
            Rocq.collection "proofs" options
            |> Collection.layout (Rocq.reader options)
        )

    Nacara.run site args

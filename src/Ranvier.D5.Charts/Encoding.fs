namespace Ranvier.D5.Charts

type Encoding<'T,'V> = { Read:'T -> 'V; Label:string }

[<RequireQualifiedAccess>]
module Encoding =
    let create label read = {Read=read;Label=label}
    let map project encoding = {Read=encoding.Read >> project;Label=encoding.Label}

namespace Ranvier.D5.Charts

/// <summary>A named accessor that reads one channel value from a datum; Label names the channel in descriptions.</summary>
type Encoding<'T,'V> = { Read:'T -> 'V; Label:string }

/// <summary>Constructors and combinators for encodings.</summary>
[<RequireQualifiedAccess>]
module Encoding =
    /// <summary>Creates an encoding from a channel label and a reader function.</summary>
    let create label read = {Read=read;Label=label}
    /// <summary>Applies a projection after the reader and keeps the label.</summary>
    let map project encoding = {Read=encoding.Read >> project;Label=encoding.Label}

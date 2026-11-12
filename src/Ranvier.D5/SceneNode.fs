namespace Ranvier.D5

type GroupSpec = { Key:string; Transform:Transform; Clip:string option }

[<RequireQualifiedAccess>]
module GroupSpec =
    let create key = {Key=key;Transform=Transform.identity;Clip=None}

type SceneNode = Leaf of Mark | Group of GroupSpec * SceneNode list

[<RequireQualifiedAccess>]
module SceneNode =
    let key = function Leaf mark -> Mark.key mark | Group(group,_) -> group.Key
    /// <summary>Structured identities remain unambiguous even when keys contain separators.</summary>
    let rec paths prefix nodes =
        nodes |> List.collect (fun node ->
            let path = prefix @ [key node]
            match node with
            | Leaf mark -> [path,mark]
            | Group(_,children) -> paths path children)

namespace Ranvier.D5

/// <summary>A scene group's key, transform applied to its children, and optional clip definition key.</summary>
type GroupSpec = { Key:string; Transform:Transform; Clip:string option }

/// <summary>Constructors for <c>GroupSpec</c>.</summary>
[<RequireQualifiedAccess>]
module GroupSpec =
    /// <summary>Creates a group with an identity transform and no clip; the key is validated by <c>Scene.compose</c>.</summary>
    let create key = {Key=key;Transform=Transform.identity;Clip=None}

/// <summary>A composed scene tree node: a <c>Leaf</c> mark or a <c>Group</c> of ordered child nodes.</summary>
type SceneNode = Leaf of Mark | Group of GroupSpec * SceneNode list

/// <summary>Helpers for <c>SceneNode</c> trees.</summary>
[<RequireQualifiedAccess>]
module SceneNode =
    /// <summary>Returns the mark key of a leaf or the group key of a group.</summary>
    let key = function Leaf mark -> Mark.key mark | Group(group,_) -> group.Key
    /// <summary>Structured identities remain unambiguous even when keys contain separators.</summary>
    let rec paths prefix nodes =
        nodes |> List.collect (fun node ->
            let path = prefix @ [key node]
            match node with
            | Leaf mark -> [path,mark]
            | Group(_,children) -> paths path children)

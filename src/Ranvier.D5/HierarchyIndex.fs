namespace Ranvier.D5

open System
open System.Collections.Generic

type internal HierarchyIndex<'Id when 'Id:comparison> =
    {Items:HierarchyItem<'Id> array;Parents:int array;Children:int array array;Postorder:int array}

module internal HierarchyIndex =
    let create root =
        let items=Hierarchy.descendants root |> List.toArray
        let lookup=Dictionary<_,int>()
        for i in 0..items.Length-1 do lookup.Add(items[i].Id,i)
        let parents=items |> Array.map(fun item -> item.Parent |> Option.map(fun id -> lookup[id]) |> Option.defaultValue -1)
        let children=Array.init items.Length (fun _ -> ResizeArray<int>())
        for i in 1..items.Length-1 do children[parents[i]].Add i
        let post=ResizeArray<int>()
        let pending=Stack<int*bool>()
        pending.Push(0,false)
        while pending.Count>0 do
            let i,visited=pending.Pop()
            if visited then post.Add i else
            pending.Push(i,true)
            for j in children[i].Count-1 .. -1 .. 0 do pending.Push(children[i][j],false)
        {Items=items;Parents=parents;Children=children |> Array.map(fun xs -> xs.ToArray());Postorder=post.ToArray()}
    let protect action =
        try Ok(action()) with :? ArgumentException as error -> Error [InvalidObservation error.Message]

type TreeConfig<'Id> = {Bounds:Bounds;Separation:HierarchyItem<'Id>->HierarchyItem<'Id>->float}

[<RequireQualifiedAccess>]
module TreeConfig =
    let create bounds = {Bounds=bounds;Separation=fun a b -> if a.Parent=b.Parent then 1. else 2.}

module internal TreeCoordinates =
    let separation config index a b =
        let value=config.Separation index.Items[a] index.Items[b]
        Check.positive "separation" value
        value
    let points bounds index (xs:float array) (ys:float array) =
        index.Items |> Array.mapi(fun i item ->
            {Id=item.Id;Parent=item.Parent;Depth=item.Depth
             Point=Point.create (Check.lerp (Bounds.x bounds) (Bounds.x bounds+Bounds.width bounds) xs[i])
                                (Check.lerp (Bounds.y bounds) (Bounds.y bounds+Bounds.height bounds) ys[i])}) |> Array.toList

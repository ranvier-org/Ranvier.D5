namespace Ranvier.D5

open System
open System.Collections.Generic

module internal Check =
    let finite name value =
        if not (Double.IsFinite value) then invalidArg name "Expected a finite number."
    let positive name value =
        finite name value
        if value <= 0. then invalidArg name "Expected a positive number."
    let interval name (a, b) =
        finite name a
        finite name b
        finite name (b - a)
    let lerp a b t =
        let value = Ranvier.Anime.Utils.lerp a b t
        if Double.IsFinite value then value else a * (1. - t) + b * t
    let unique name key values =
        let seen = HashSet<_>()
        for value in values do
            if not (seen.Add (key value)) then invalidArg name "Keys must be unique."

/// <summary>A numeric interval and the observations assigned to it.</summary>
type Bin<'T> = { Lower: float; Upper: float; Values: 'T list }

/// <summary>Keyed changes with source and target indices; lists follow their respective input order.</summary>
type Join<'Key, 'T> = {
    /// <summary>Keys only in the next input, with their index in next and their value.</summary>
    Enter: ('Key * int * 'T) list
    /// <summary>Keys in both inputs, with the previous index, next index, previous value and next value.</summary>
    Update: ('Key * int * int * 'T * 'T) list
    /// <summary>Keys only in the previous input, with their index in previous and their value.</summary>
    Exit: ('Key * int * 'T) list
}

/// <summary>Pure data transforms. Numeric summaries omit NaN and infinite observations.</summary>
[<RequireQualifiedAccess>]
module Data =
    /// <summary>Returns the finite minimum and maximum, or None when no finite observations exist.</summary>
    let extent values =
        values |> Seq.filter Double.IsFinite |> Seq.fold (fun state value ->
            match state with
            | None -> Some (value, value)
            | Some (lo, hi) -> Some (min lo value, max hi value)) None

    /// <summary>Sums finite observations with compensated accumulation; rejects arithmetic overflow.</summary>
    let sum values =
        let mutable total = 0.
        let mutable correction = 0.
        for value in values do
            if Double.IsFinite value then
                let next = total + value
                Check.finite "values" next
                correction <- correction + (if abs total >= abs value then (total-next)+value else (value-next)+total)
                total <- next
        let result = total + correction
        Check.finite "values" result
        result

    /// <summary>Returns the mean of finite observations, or None for an empty sample.</summary>
    let mean values =
        let finite = values |> Seq.filter Double.IsFinite |> Seq.toArray
        if finite.Length = 0 then None
        else Some (finite |> Seq.map (fun x -> x / float finite.Length) |> sum)

    /// <summary>Computes a linearly interpolated quantile from a sorted copy; probability must be in [0,1].</summary>
    let quantile probability values =
        Check.finite "probability" probability
        if probability < 0. || probability > 1. then invalidArg "probability" "Expected a probability in [0,1]."
        let ordered = values |> Seq.filter Double.IsFinite |> Seq.sort |> Seq.toArray
        if ordered.Length = 0 then None
        else
            let position = probability * float (ordered.Length - 1)
            let lower = int (floor position)
            let upper = min (lower+1) (ordered.Length-1)
            Some (Check.lerp ordered[lower] ordered[upper] (position-float lower))

    /// <summary>Groups observations by an ordered key, preserving observation order within each group.</summary>
    let groupBy key values =
        values |> Seq.fold (fun groups value ->
            let k = key value
            Map.change k (fun previous -> Some (value :: Option.defaultValue [] previous)) groups) Map.empty
        |> Map.map (fun _ -> List.rev)

    /// <summary>Returns the insertion index to the right of equal values in an ascending array.</summary>
    let bisectRight (ordered: float array) value =
        let mutable low, high = 0, ordered.Length
        while low < high do
            let middle = low + (high-low)/2
            if value < ordered[middle] then high <- middle else low <- middle+1
        low

    /// <summary>Bins finite observations within strictly increasing boundaries. The final upper edge is inclusive.</summary>
    let bin project boundaries values =
        let edges = List.toArray boundaries
        if edges.Length < 2 then invalidArg "boundaries" "At least two boundaries are required."
        edges |> Array.iter (Check.finite "boundaries")
        for i in 1..edges.Length-1 do
            if edges[i] <= edges[i-1] then invalidArg "boundaries" "Boundaries must increase strictly."
        let buckets = Array.init (edges.Length-1) (fun _ -> ResizeArray<_>())
        for value in values do
            let x = project value
            if Double.IsFinite x && x >= edges[0] && x <= edges[edges.Length-1] then
                let index = min (buckets.Length-1) (bisectRight edges x - 1)
                buckets[index].Add value
        [for i in 0..buckets.Length-1 -> { Lower=edges[i]; Upper=edges[i+1]; Values=List.ofSeq buckets[i] }]

    /// <summary>Computes a keyed join in O(n) expected time; duplicate keys in either input reject the join.</summary>
    let join key previous next =
        Check.unique "previous" key previous
        Check.unique "next" key next
        let old = Dictionary<_,_>()
        previous |> List.iteri (fun i value -> old.Add(key value, (i,value)))
        let retained = HashSet<_>()
        let enter, update = ResizeArray<_>(), ResizeArray<_>()
        next |> List.iteri (fun i value ->
            let k = key value
            match old.TryGetValue k with
            | true, (oldIndex,oldValue) ->
                retained.Add k |> ignore
                update.Add(k,oldIndex,i,oldValue,value)
            | _ -> enter.Add(k,i,value))
        let exits = previous |> List.mapi (fun i value -> key value,i,value) |> List.filter (fun (k,_,_) -> not (retained.Contains k))
        { Enter=List.ofSeq enter; Update=List.ofSeq update; Exit=exits }


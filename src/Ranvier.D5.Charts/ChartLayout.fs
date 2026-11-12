namespace Ranvier.D5.Charts

open System
open Ranvier.D5

/// <summary>Validation of frames, data IDs and scale domains shared by chart recipes.</summary>
[<RequireQualifiedAccess>]
module ChartLayout =
    /// <summary>Returns the plot area in screen pixels, or an InvalidObservation error when a size or margin is negative or non-finite or the margins leave no area.</summary>
    let plot frame =
        let v,m=frame.Viewport,frame.Margin
        let values=[v.Width;v.Height;m.Top;m.Right;m.Bottom;m.Left]
        if values |> List.exists (fun x -> not(Double.IsFinite x) || x<0.) then Error [InvalidObservation "Invalid viewport or margin."]
        elif v.Width<=m.Left+m.Right || v.Height<=m.Top+m.Bottom then Error [InvalidObservation "Margins leave no plot area."]
        else Ok(Bounds.create m.Left m.Top (v.Width-m.Left-m.Right) (v.Height-m.Top-m.Bottom))

    /// <summary>Returns one error per empty ID (InvalidObservation) and per repeated ID (DuplicateId); an empty list means every key is valid and unique.</summary>
    let validateIds key data =
        data |> List.countBy key |> List.choose (fun (id,count) ->
            if String.IsNullOrEmpty id then Some(InvalidObservation "Empty data ID.")
            elif count>1 then Some(DuplicateId id) else None)

    /// <summary>Resolves a domain from the present values (missing ones are ignored; no values gives [0,1] before the policy); a zero-width domain is widened by 1 or 1% of its magnitude, and non-finite or inverted domains are typed errors.</summary>
    let domain policy values =
        let values=List.choose id values
        if values |> List.exists (Double.IsFinite >> not) then Error [InvalidObservation "Non-finite observation."]
        else
            let a,b =
                match policy with
                | Fixed(a,b) -> a,b
                | _ ->
                    let a,b=if List.isEmpty values then 0.,1. else List.min values,List.max values
                    if policy=IncludeZero then min 0. a,max 0. b else a,b
            if not(Double.IsFinite a && Double.IsFinite b) || a>b then Error [InvalidObservation "Invalid domain."]
            elif a=b then
                let delta=if a=0. then 1. else abs a*0.01
                let lo,hi=a-delta,b+delta
                if Double.IsFinite lo && Double.IsFinite hi then Ok(lo,hi) else Error [InvalidObservation "Constant domain cannot be expanded finitely."]
            else Ok(a,b)

/// <summary>Greedy, deterministic label placement within a plot area.</summary>
[<RequireQualifiedAccess>]
module LabelLayout =
    /// <summary>Places high-priority labels first; equal priority keeps input order. Overlapping or overflowing labels are skipped.</summary>
    let place plot measure (requests:LabelRequest list) =
        let overlaps a b = Bounds.x a < Bounds.x b+Bounds.width b && Bounds.x b < Bounds.x a+Bounds.width a && Bounds.y a < Bounds.y b+Bounds.height b && Bounds.y b < Bounds.y a+Bounds.height a
        let inside b = Bounds.x b>=Bounds.x plot && Bounds.y b>=Bounds.y plot && Bounds.x b+Bounds.width b<=Bounds.x plot+Bounds.width plot && Bounds.y b+Bounds.height b<=Bounds.y plot+Bounds.height plot
        requests |> List.indexed |> List.sortBy (fun (i,r) -> -(int64 r.Priority),i)
        |> List.fold (fun (placed:PlacedLabel list) (_,request) ->
            let size:TextSize=measure request.Text
            if not(Double.IsFinite size.Width && Double.IsFinite size.Height) || size.Width<0. || size.Height<0. then invalidArg "measure" "Invalid text bounds."
            let bounds=Bounds.create request.Position.X request.Position.Y size.Width size.Height
            if inside bounds && not(List.exists (fun (p:PlacedLabel) -> overlaps p.Bounds bounds) placed)
            then {Id=request.Id;Text=request.Text;Bounds=bounds}::placed else placed) [] |> List.rev

    /// <summary>Runs at most two measurement/layout feedback passes; callers apply the skip policy to the final bounds.</summary>
    let refine initial pass equivalent =
        let first=pass initial
        if equivalent initial first then first else pass first

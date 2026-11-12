namespace Ranvier.D5.Charts

open System
open Ranvier.D5

module internal Recipe =
    let key parts=parts |> List.map (fun (part:string) -> string part.Length+":"+part) |> String.concat ""
    let number value=Format.number Globalization.CultureInfo.InvariantCulture 3 value
    let unwrap = function Ok value -> value | Error errors -> raise(ArgumentException(sprintf "%A" errors))
    let run frame key data build =
        let errors=ChartLayout.validateIds key data
        match ChartLayout.plot frame,errors with
        | Error e,_ -> Error(e@errors)
        | _,_::_ -> Error errors
        | Ok bounds,[] ->
            try Ok(build bounds)
            with :? ArgumentException as error -> Error [InvalidObservation error.Message]
    let domain policy values=ChartLayout.domain policy values |> unwrap
    let output frame bounds marks semantics hits axes =
        let scene=Scene.compose frame.Viewport frame.Title [RectClip("plot-clip",bounds)]
                    [Group({GroupSpec.create "plot" with Clip=Some "plot-clip"},List.map Leaf marks)
                     Group(GroupSpec.create "axes",List.map Leaf axes)]
        // A recipe may narrow a target's clip to its own extent so picking rejects it cheaply; disjoint extents keep the plot clip.
        let clip (hit:HitTarget)=
            match hit.Clip with
            | None -> bounds
            | Some own ->
                let left,top=max (Bounds.x own) (Bounds.x bounds),max (Bounds.y own) (Bounds.y bounds)
                let right=min (Bounds.x own+Bounds.width own) (Bounds.x bounds+Bounds.width bounds)
                let bottom=min (Bounds.y own+Bounds.height own) (Bounds.y bounds+Bounds.height bounds)
                if right>=left && bottom>=top then Bounds.create left top (right-left) (bottom-top) else bounds
        {Scene=scene;PlotBounds=bounds;Semantics=semantics;HitTargets=hits |> List.map(fun hit -> {hit with Clip=Some(clip hit)});Description=frame.Title}
    let semantic id dataIds description = {MarkPath=["plot";id];DataIds=dataIds;Description=description}
    let hit id dataIds geometry = {MarkPath=["plot";id];DataIds=dataIds;Geometry=geometry;Clip=None}
    let rectangle id label color bounds = Mark.path id (Shape.rectangle bounds) |> Mark.withFill color |> Mark.withLabel(Some label)
    let axes bounds x y =
        let label value=number value
        let numeric prefix orient origin ends scale =
            Scale.ticks 5 scale |> List.map (fun value -> value.ToString("R",Globalization.CultureInfo.InvariantCulture),Scale.map scale value,label value)
            |> Axis.values prefix orient origin ends
        numeric "x" AxisOrientation.Bottom (Bounds.y bounds+Bounds.height bounds) (Bounds.x bounds,Bounds.x bounds+Bounds.width bounds) x
        @ numeric "y" AxisOrientation.Left (Bounds.x bounds) (Bounds.y bounds,Bounds.y bounds+Bounds.height bounds) y

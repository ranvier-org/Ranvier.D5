namespace Ranvier.D5.Geo

open System
open Ranvier.D5

/// <summary>An inset: a projection drawn only within its screen rectangle.</summary>
type CompositePart = { Projection: Projection; Clip: Bounds }

/// <summary>Several projections drawn side by side, each inside its own inset rectangle.</summary>
/// <remarks>Point projection uses the first part whose inset contains the projected point; inversion selects a part by screen region.</remarks>
type CompositeProjection = private { Parts: CompositePart list; Select: Point -> CompositePart }

/// <summary>Composite projections, including the D3 Albers USA arrangement.</summary>
module CompositeProjection =
    /// <summary>Returns the insets in drawing and point-lookup order.</summary>
    let parts composite = composite.Parts

    /// <summary>Lower 48 Albers with Alaska (0.35 scale) and Hawaii insets, matching d3-geo geoAlbersUsa; D3 uses scale 1070 and translate (480,250).</summary>
    let albersUsa (scale:float) (translate:Point) : Result<CompositeProjection,LayoutError list> =
        if not(Double.IsFinite scale && scale>0. && Double.IsFinite translate.X && Double.IsFinite translate.Y) then
            Error [InvalidObservation "Albers USA requires positive finite scale and finite translation."]
        else
            let epsilon=1e-6
            let x,y,k=translate.X,translate.Y,scale
            let part longitude latitude rotate parallels partScale (offsetX,offsetY) (left,top,right,bottom) =
                let center=GeoPosition.create longitude latitude
                let rotation=GeoRotation.create rotate 0. 0.
                match center,rotation with
                | Ok center,Ok rotation ->
                    Projection.create {ProjectionConfig.defaultValue (ProjectionKind.ConicEqualArea parallels) with
                                        Scale=partScale;Translate=Point.create (x+offsetX*k) (y+offsetY*k);Center=center;Rotation=rotation}
                    |> Result.map(fun projection -> {Projection=projection;Clip=Bounds.create left top (right-left) (bottom-top)})
                | Error errors,_ | _,Error errors -> Error errors
            let lower48=part -0.6 38.7 96. (29.5,45.5) k (0.,0.) (x-0.455*k,y-0.238*k,x+0.455*k,y+0.238*k)
            let alaska=part -2. 58.5 154. (55.,65.) (0.35*k) (-0.307,0.201) (x-0.425*k+epsilon,y+0.120*k+epsilon,x-0.214*k-epsilon,y+0.234*k-epsilon)
            let hawaii=part -3. 19.9 157. (8.,18.) k (-0.205,0.212) (x-0.214*k+epsilon,y+0.166*k+epsilon,x-0.115*k-epsilon,y+0.234*k-epsilon)
            match lower48,alaska,hawaii with
            | Ok lower48,Ok alaska,Ok hawaii ->
                let select (point:Point) =
                    let u,v=(point.X-x)/k,(point.Y-y)/k
                    if v>=0.120 && v<0.234 && u>= -0.425 && u< -0.214 then alaska
                    elif v>=0.166 && v<0.234 && u>= -0.214 && u< -0.115 then hawaii
                    else lower48
                Ok {Parts=[lower48;alaska;hawaii];Select=select}
            | Error errors,_,_ | _,Error errors,_ | _,_,Error errors -> Error errors

    /// <summary>Projects with the first inset whose rectangle contains the projected point, or None.</summary>
    let project composite position =
        composite.Parts |> List.tryPick(fun part ->
            Projection.project part.Projection position |> Option.filter(fun point -> Bounds.contains point part.Clip))

    /// <summary>Inverts with the inset selected by screen region.</summary>
    let tryInvert composite point = Projection.tryInvert (composite.Select point).Projection point

    /// <summary>Projects geometry through every inset, each clipped to its rectangle intersected with any ClipExtent.</summary>
    /// <remarks>Each inset receives the configured budgets; the result is all-or-nothing.</remarks>
    let paths (options:GeoPathOptions) composite geometry : Result<Path list,LayoutError list> =
        let intersect (part:CompositePart) =
            match options.ClipExtent with
            | None -> Some part.Clip
            | Some clip ->
                let left=max (Bounds.x clip) (Bounds.x part.Clip)
                let top=max (Bounds.y clip) (Bounds.y part.Clip)
                let right=min (Bounds.x clip+Bounds.width clip) (Bounds.x part.Clip+Bounds.width part.Clip)
                let bottom=min (Bounds.y clip+Bounds.height clip) (Bounds.y part.Clip+Bounds.height part.Clip)
                if right<left || bottom<top then None else Some(Bounds.create left top (right-left) (bottom-top))
        let rec run parts collected =
            match parts with
            | [] -> Ok(List.rev collected |> List.concat)
            | part::rest ->
                match intersect part with
                | None -> run rest collected
                | Some clip ->
                    match GeoPath.project {options with ClipExtent=Some clip} part.Projection geometry with
                    | Ok paths -> run rest (paths::collected)
                    | Error errors -> Error errors
        run composite.Parts []

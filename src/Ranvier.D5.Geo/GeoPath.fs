namespace Ranvier.D5.Geo

open System
open System.Threading
open Ranvier.D5

/// <summary>Bounds input traversal, projection work, subdivision and screen-space output.</summary>
type GeoPathOptions = {
    /// <summary>Independent input traversal limits, including input vertices and nesting.</summary>
    InputLimits: GeoStreamConfig
    /// <summary>Positive screen-space error tolerance for adaptive subdivision.</summary>
    Precision: float
    /// <summary>Nonnegative finite point-symbol radius in screen units; zero omits point symbols.</summary>
    PointRadius: float
    /// <summary>Optional rectangular output clipping extent in screen coordinates.</summary>
    ClipExtent: Bounds option
    /// <summary>Maximum emitted path endpoints, counting repeated closing and arc endpoints.</summary>
    MaxOutputPoints: int
    /// <summary>Maximum projection and output work units, in addition to input traversal work.</summary>
    MaxWork: int64
    /// <summary>Adaptive subdivision depth in [1,32].</summary>
    MaxSubdivisionDepth: int
}

/// <summary>Default policies for bounded geographic path generation.</summary>
module GeoPathOptions =
    /// <summary>Uses half-unit precision, radius 4.5, no rectangular clip, one million output endpoints and ten million work units.</summary>
    let defaultValue = {
        InputLimits=GeoStreamConfig.defaultValue; Precision=0.5; PointRadius=4.5
        ClipExtent=None; MaxOutputPoints=1_000_000; MaxWork=10_000_000L; MaxSubdivisionDepth=16
    }

/// <summary>Builds existing toolkit paths from validated geographic streams.</summary>
module GeoPath =
    exception private Rejected of LayoutError

    /// <summary>Projects geometry with cooperative cancellation and atomic result publication.</summary>
    let projectWithCancellation (options:GeoPathOptions) (token:CancellationToken) projection geometry : Result<Path list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let validExtent =
            match options.ClipExtent with
            | None -> true
            | Some extent ->
                Double.IsFinite (Bounds.x extent) && Double.IsFinite (Bounds.y extent) &&
                Double.IsFinite (Bounds.width extent) && Double.IsFinite (Bounds.height extent) &&
                (Bounds.width extent) >= 0. && (Bounds.height extent) >= 0. &&
                Double.IsFinite((Bounds.x extent)+(Bounds.width extent)) && Double.IsFinite((Bounds.y extent)+(Bounds.height extent))
        if options.MaxOutputPoints < 1 || options.MaxWork < 1L ||
           options.MaxSubdivisionDepth < 1 || options.MaxSubdivisionDepth > 32 ||
           not(Double.IsFinite options.Precision) || options.Precision <= 0. ||
           not(Double.IsFinite options.PointRadius) || options.PointRadius < 0. || not validExtent then
            Error [InvalidObservation "Geographic paths require positive finite precision, nonnegative finite radius and clip extent, positive budgets, and subdivision depth in [1,32]."]
        else
            try
                let paths=ResizeArray<Path>()
                let mutable work=0L
                let mutable emitted=0
                let spend amount =
                    token.ThrowIfCancellationRequested()
                    if amount > options.MaxWork-work then raise(Rejected(BudgetExceeded "Geographic projection exceeds its work budget."))
                    work <- work+amount
                let reserve count =
                    spend(int64 count)
                    if count > options.MaxOutputPoints-emitted then raise(Rejected(BudgetExceeded "Geographic projection exceeds its output-point budget."))
                    emitted <- emitted+count
                let circle (point:Point) =
                    let radius=options.PointRadius
                    if radius > 0. then
                        let left,right=point.X-radius,point.X+radius
                        let top,bottom=point.Y-radius,point.Y+radius
                        if not(Double.IsFinite left && Double.IsFinite right && Double.IsFinite top && Double.IsFinite bottom) then
                            raise(Rejected(InvalidObservation "Projected point radius overflows screen coordinates."))
                        let visible =
                            match options.ClipExtent with
                            | None -> true
                            | Some clip ->
                                if (Bounds.width clip)=0. || (Bounds.height clip)=0. || right < (Bounds.x clip) || left > (Bounds.x clip)+(Bounds.width clip) || bottom < (Bounds.y clip) || top > (Bounds.y clip)+(Bounds.height clip) then false
                                elif left >= (Bounds.x clip) && right <= (Bounds.x clip)+(Bounds.width clip) && top >= (Bounds.y clip) && bottom <= (Bounds.y clip)+(Bounds.height clip) then true
                                else raise(Rejected(InvalidObservation "Partial point clipping is not yet implemented."))
                        if visible then
                            reserve 3
                            let start={X=right;Y=point.Y}
                            paths.Add(Path.ofCommands [
                                MoveTo start
                                ArcTo(radius,radius,0.,false,true,{X=left;Y=point.Y})
                                ArcTo(radius,radius,0.,false,true,start)
                                Close])
                let folder () event =
                    spend 1L
                    match event with
                    | GeoStreamEvent.Point position -> Projection.project projection position |> Option.iter circle
                    | _ -> raise(Rejected(InvalidObservation "Geographic line and polygon path projection is not yet implemented."))
                match GeoStream.foldWithCancellation options.InputLimits token folder () geometry with
                | Error errors -> Error errors
                | Ok () ->
                    let mutable result=[]
                    for index=paths.Count-1 downto 0 do
                        spend 1L
                        result <- paths[index]::result
                    token.ThrowIfCancellationRequested()
                    Ok result
            with Rejected error -> Error [error]

    /// <summary>Projects geometry with the configured budgets and no cancellation token.</summary>
    let project options projection geometry = projectWithCancellation options CancellationToken.None projection geometry

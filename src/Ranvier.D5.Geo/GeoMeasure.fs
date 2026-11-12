namespace Ranvier.D5.Geo

open System.Threading
open Ranvier.D5

/// <summary>Spherical measurements in angular radians and steradians, using bounded geographic traversal.</summary>
module GeoMeasure =
    [<Struct>]
    type private LengthState =
        { Sum: float; Correction: float
          First: GeoPosition voption; Previous: GeoPosition voption
          InLine: bool; InPolygon: bool }

    let private addLength value state =
        let adjusted=value-state.Correction
        let total=state.Sum+adjusted
        {state with Sum=total;Correction=(total-state.Sum)-adjusted}

    let private lengthEvent state event =
        match event with
        | GeoStreamEvent.LineStart -> {state with InLine=true;First=ValueNone;Previous=ValueNone}
        | GeoStreamEvent.Point position when state.InLine ->
            match state.Previous with
            | ValueNone -> {state with First=ValueSome position;Previous=ValueSome position}
            | ValueSome previous ->
                {addLength (Geodesic.distance previous position) state with Previous=ValueSome position}
        | GeoStreamEvent.LineEnd ->
            let closed =
                match state.InPolygon,state.First,state.Previous with
                | true,ValueSome first,ValueSome previous -> addLength (Geodesic.distance previous first) state
                | _ -> state
            {closed with InLine=false;First=ValueNone;Previous=ValueNone}
        | GeoStreamEvent.PolygonStart -> {state with InPolygon=true}
        | GeoStreamEvent.PolygonEnd -> {state with InPolygon=false}
        | _ -> state
    /// <summary>Measures line and polygon-ring length in radians; points and the sphere contribute zero.</summary>
    let lengthWithCancellation (config: GeoStreamConfig) (token: CancellationToken) (geometry: GeoGeometry) : Result<float,LayoutError list> =
        let initial={Sum=0.;Correction=0.;First=ValueNone;Previous=ValueNone;InLine=false;InPolygon=false}
        GeoStream.foldWithCancellation config token lengthEvent initial geometry |> Result.map(fun state -> state.Sum)

    /// <summary>Measures spherical boundary length in radians without cancellation.</summary>
    let length config geometry = lengthWithCancellation config CancellationToken.None geometry

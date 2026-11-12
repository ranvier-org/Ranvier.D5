namespace Ranvier.D5.Geo

open System
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

    exception private AmbiguousArc of string

    [<Struct>]
    type private AreaState =
        { Total: float; TotalCorrection: float
          HalfPolygon: float; PolygonCorrection: float
          First: GeoPosition voption; Previous: GeoPosition voption
          InPolygon: bool }

    let private addArea value state =
        let adjusted=value-state.TotalCorrection
        let total=state.Total+adjusted
        {state with Total=total;TotalCorrection=(total-state.Total)-adjusted}

    let private areaEdge from destination state =
        let a,b=GeoPosition.normalize from,GeoPosition.normalize destination
        if a.Latitude = -b.Latitude && (abs(a.Longitude-b.Longitude)=180. || abs a.Latitude=90.) then
            raise(AmbiguousArc "Spherical area requires an explicit intermediate point on antipodal edges.")
        let delta=b.Longitude-a.Longitude
        let struct(sin0,cos0)=GeoMath.sinCosDegrees (a.Latitude/2.+45.)
        let struct(sin1,cos1)=GeoMath.sinCosDegrees (b.Latitude/2.+45.)
        let struct(sinDelta,cosDelta)=GeoMath.sinCosDegrees (abs delta)
        let sign=if delta < 0. then -1. else 1.
        let k=sin0*sin1
        let excess=Math.Atan2(k*sign*sinDelta,cos0*cos1+k*cosDelta)
        let adjusted=excess-state.PolygonCorrection
        let total=state.HalfPolygon+adjusted
        {state with HalfPolygon=total;PolygonCorrection=(total-state.HalfPolygon)-adjusted}

    let private areaEvent state event =
        match event with
        | GeoStreamEvent.Sphere -> addArea (4.*Math.PI) state
        | GeoStreamEvent.PolygonStart ->
            {state with InPolygon=true;HalfPolygon=0.;PolygonCorrection=0.;First=ValueNone;Previous=ValueNone}
        | GeoStreamEvent.LineStart when state.InPolygon ->
            {state with First=ValueNone;Previous=ValueNone}
        | GeoStreamEvent.Point position when state.InPolygon ->
            match state.Previous with
            | ValueNone -> {state with First=ValueSome position;Previous=ValueSome position}
            | ValueSome previous -> {areaEdge previous position state with Previous=ValueSome position}
        | GeoStreamEvent.LineEnd when state.InPolygon ->
            match state.First,state.Previous with
            | ValueSome first,ValueSome previous -> areaEdge previous first state
            | _ -> state
        | GeoStreamEvent.PolygonEnd ->
            let half=if state.HalfPolygon < 0. then 2.*Math.PI+state.HalfPolygon else state.HalfPolygon
            {addArea (2.*half) state with InPolygon=false;First=ValueNone;Previous=ValueNone}
        | _ -> state

    /// <summary>Measures spherical polygon area in steradians, preserving complement winding and holes.</summary>
    /// <remarks>Small clockwise exteriors select the small region; reversal selects its complement.
    /// Collections sum members without computing their union. Antipodal polygon edges are rejected.</remarks>
    let areaWithCancellation (config: GeoStreamConfig) (token: CancellationToken) (geometry: GeoGeometry) : Result<float,LayoutError list> =
        let initial={Total=0.;TotalCorrection=0.;HalfPolygon=0.;PolygonCorrection=0.;First=ValueNone;Previous=ValueNone;InPolygon=false}
        try GeoStream.foldWithCancellation config token areaEvent initial geometry |> Result.map(fun state -> state.Total)
        with AmbiguousArc message -> Error [InvalidObservation message]

    /// <summary>Measures spherical polygon area without cancellation; collections sum their members.</summary>
    let area config geometry = areaWithCancellation config CancellationToken.None geometry

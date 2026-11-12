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

    [<Struct>]
    type private VectorSum =
        { X: float; Y: float; Z: float
          CX: float; CY: float; CZ: float; Weight: float; Scale: float }

    let private emptyVector = {X=0.;Y=0.;Z=0.;CX=0.;CY=0.;CZ=0.;Weight=0.;Scale=0.}

    let private addVector x y z weight sum =
        let inline sumAxis value total correction =
            let adjusted=value-correction
            let next=total+adjusted
            struct(next,(next-total)-adjusted)
        let struct(x,cx)=sumAxis x sum.X sum.CX
        let struct(y,cy)=sumAxis y sum.Y sum.CY
        let struct(z,cz)=sumAxis z sum.Z sum.CZ
        {X=x;Y=y;Z=z;CX=cx;CY=cy;CZ=cz;Weight=sum.Weight+weight;Scale=sum.Scale}

    let private addWeightedVector x y z bound weight sum =
        if weight=0. then sum else
        let scale=max sum.Scale weight
        let resized =
            if scale=sum.Scale then sum else
            let factor=sum.Scale/scale
            {X=sum.X*factor;Y=sum.Y*factor;Z=sum.Z*factor
             CX=sum.CX*factor;CY=sum.CY*factor;CZ=sum.CZ*factor
             Weight=sum.Weight*factor;Scale=scale}
        let factor=weight/scale
        addVector (x*factor) (y*factor) (z*factor) (bound*factor) resized

    let private directionOf sum =
        let norm=Double.Hypot(Double.Hypot(sum.X,sum.Y),sum.Z)
        if norm <= 32.*2.2204460492503131e-16*sum.Weight then None
        else Some(GeoMath.position struct(sum.X,sum.Y,sum.Z))

    /// <summary>Returns an area-weighted, then length-weighted, then point centroid; ambiguous results are None.</summary>
    /// <remarks>Line weights use angular length times the sum of endpoint unit vectors.
    /// Polygon weights use oriented great-circle normals and full arc angles.
    /// Near-cancelling sums below a relative floating-point error threshold fall back in dimension.
    /// Antipodal edges require an explicit intermediate route point.</remarks>
    let centroidWithCancellation (config: GeoStreamConfig) (token: CancellationToken) (geometry: GeoGeometry) : Result<GeoPosition option,LayoutError list> =
        let mutable points=emptyVector
        let mutable lines=emptyVector
        let mutable polygons=emptyVector
        let mutable inLine=false
        let mutable inPolygon=false
        let mutable first=ValueNone
        let mutable previous=ValueNone
        let mutable firstVector=struct(0.,0.,0.)
        let mutable previousVector=struct(0.,0.,0.)
        let edge from fromVector destination destinationVector =
            let a,b=GeoPosition.normalize from,GeoPosition.normalize destination
            if a.Latitude = -b.Latitude && (abs(a.Longitude-b.Longitude)=180. || abs a.Latitude=90.) then
                raise(AmbiguousArc "Spherical centroid requires an explicit intermediate point on antipodal edges.")
            let struct(x0,y0,z0)=fromVector
            let struct(x1,y1,z1)=destinationVector
            let angle=Geodesic.distance a b
            lines <- addWeightedVector (x0+x1) (y0+y1) (z0+z1) 2. angle lines
            if inPolygon then
                let cx,cy,cz=y0*z1-z0*y1,z0*x1-x0*z1,x0*y1-y0*x1
                let magnitude=Double.Hypot(Double.Hypot(cx,cy),cz)
                if magnitude>0. then
                    polygons <- addWeightedVector (-cx/magnitude) (-cy/magnitude) (-cz/magnitude) 1. angle polygons
                elif angle>0. then
                    raise(AmbiguousArc "The great-circle normal is not representable.")
        let folder () event =
            match event with
            | GeoStreamEvent.PolygonStart -> inPolygon <- true
            | GeoStreamEvent.PolygonEnd -> inPolygon <- false
            | GeoStreamEvent.LineStart ->
                inLine <- true
                first <- ValueNone
                previous <- ValueNone
            | GeoStreamEvent.Point position ->
                let struct(x,y,z) as vector=GeoMath.vector position
                points <- addVector x y z 1. points
                if inLine then
                    match previous with
                    | ValueSome prior -> edge prior previousVector position vector
                    | ValueNone -> first <- ValueSome position;firstVector <- vector
                    previous <- ValueSome position
                    previousVector <- vector
            | GeoStreamEvent.LineEnd ->
                if inPolygon then
                    match previous,first with
                    | ValueSome prior,ValueSome start -> edge prior previousVector start firstVector
                    | _ -> ()
                inLine <- false
                first <- ValueNone
                previous <- ValueNone
            | GeoStreamEvent.Sphere -> ()
        try
            GeoStream.foldWithCancellation config token folder () geometry
            |> Result.map(fun () ->
                match directionOf polygons with
                | Some position -> Some position
                | None -> if lines.Weight>0. then directionOf lines else directionOf points)
        with AmbiguousArc message -> Error [InvalidObservation message]

    /// <summary>Computes a spherical centroid without cancellation.</summary>
    let centroid config geometry = centroidWithCancellation config CancellationToken.None geometry

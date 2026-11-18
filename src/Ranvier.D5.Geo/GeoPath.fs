namespace Ranvier.D5.Geo

open System
open System.Threading
open Ranvier.D5

/// <summary>Builds existing toolkit paths from validated geographic streams.</summary>
/// <remarks>Mercator streams are clipped after rotation to latitudes +/-atan(sinh(pi)), about 85.051129 degrees, before projection. Point-symbol centers outside the band are omitted; symbols centered inside retain their configured radius. Direct Projection.project remains a mathematical point mapping. Exact antipodal edges require a waypoint.</remarks>
module GeoPath =
    exception private Rejected of LayoutError

    let private resampler (options:GeoPathOptions) projection spend (screen:GeoPosition -> Point) emit =
        let cylindrical=
            match Projection.kind projection with
            | ProjectionKind.Equirectangular | ProjectionKind.Mercator -> true
            | _ -> false
        let cartesianFast=Projection.supportsCartesianResample projection
        let scale=Projection.scale projection
        let screenCartesian seamLonRad x y z =
            match Projection.projectRotatedCartesianValue projection seamLonRad x y z with
            | ValueSome value -> value
            | ValueNone -> raise(Rejected(InvalidObservation "Projected line span exceeds representable screen coordinates."))
        let rec subdivideCartesian depth seamLonRad ax ay az tx ty tz angle (a:Point) (b:Point) =
            spend 1L
            let half=angle*0.5
            let struct(sm,cm)=Math.SinCos half
            let mx3=cm*ax+sm*tx
            let my3=cm*ay+sm*ty
            let mz3=cm*az+sm*tz
            let middle=screenCartesian seamLonRad mx3 my3 mz3
            let dx,dy=b.X-a.X,b.Y-a.Y
            let mx,my=middle.X-a.X,middle.Y-a.Y
            let length=Double.Hypot(dx,dy)
            if not(Double.IsFinite length && Double.IsFinite mx && Double.IsFinite my) then
                raise(Rejected(InvalidObservation "Projected line span exceeds representable screen coordinates."))
            let error,parameter =
                if length=0. then Double.Hypot(mx,my),0.5
                else
                    let ux,uy=dx/length,dy/length
                    abs(ux*my-uy*mx),(ux*mx+uy*my)/length
            let inline checkQuarter amount s c =
                spend 1L
                let sample=screenCartesian seamLonRad (c*ax+s*tx) (c*ay+s*ty) (c*az+s*tz)
                let qx,qy=sample.X-a.X,sample.Y-a.Y
                if not(Double.IsFinite qx && Double.IsFinite qy) then
                    raise(Rejected(InvalidObservation "Projected line span exceeds representable screen coordinates."))
                if length=0. then struct(Double.Hypot(qx,qy)>options.Precision,false)
                else
                    let ux,uy=dx/length,dy/length
                    struct(abs(ux*qy-uy*qx)>options.Precision,abs((ux*qx+uy*qy)/length-amount)>0.3)
            let mutable subdivide=error>options.Precision || abs(parameter-0.5)>0.3 || angle>Math.PI/6.
            if not subdivide then
                let cq=sqrt((1.+cm)*0.5)
                let sq=sm/(2.*cq)
                let struct(firstDeviates,firstUneven)=checkQuarter 0.25 sq cq
                let struct(secondDeviates,secondUneven)=checkQuarter 0.75 (sm*cq+cm*sq) (cm*cq-sm*sq)
                subdivide<-firstDeviates || firstUneven || secondDeviates || secondUneven
            if subdivide then
                if depth>=options.MaxSubdivisionDepth then
                    let cq=sqrt((1.+cm)*0.5)
                    let sq=sm/(2.*cq)
                    let struct(firstDeviates,_)=checkQuarter 0.25 sq cq
                    let struct(secondDeviates,_)=checkQuarter 0.75 (sm*cq+cm*sq) (cm*cq-sm*sq)
                    if error>options.Precision || parameter<0. || parameter>1. || firstDeviates || secondDeviates then
                        raise(Rejected(BudgetExceeded "Projected line cannot meet precision within its subdivision depth."))
                    emit b
                else
                    subdivideCartesian (depth+1) seamLonRad ax ay az tx ty tz half a middle
                    subdivideCartesian (depth+1) seamLonRad mx3 my3 mz3 (cm*tx-sm*ax) (cm*ty-sm*ay) (cm*tz-sm*az) half middle b
            else emit b
        // Halves stay on the parent's great circle, so recursion rotates the parent frame rather than re-solving each arc.
        let rec subdivideArc depth (first:GeoPosition) (last:GeoPosition) (arc:GeoArc) (a:Point) (b:Point) =
            spend 1L
            // On a preclipped arc, latitude changes by at most arc length and these maps expand by at most sec(latitude).
            let withinPrecision =
                if cylindrical && arc.Angle<=Math.PI/6. then
                    let latitudeLimit=min (abs first.Latitude) (abs last.Latitude)*(Math.PI/180.)+arc.Angle
                    latitudeLimit<Math.PI/2. && scale*arc.Angle/cos(latitudeLimit)*(1.+1e-12)<=options.Precision
                else false
            if withinPrecision then emit b
            else
                let signed (value:GeoPosition) =
                    if first.Longitude=last.Longitude && abs first.Longitude=180. then
                        match GeoPosition.create first.Longitude value.Latitude with
                        | Ok signed -> signed
                        | Error errors -> raise(Rejected errors.Head)
                    else value
                let interpolate amount=signed(GeoArc.atFraction amount arc)
                let struct(halfway,firstHalf,secondHalf)=GeoArc.bisect arc
                let midpoint=signed halfway
                let middle=screen midpoint
                let dx,dy=b.X-a.X,b.Y-a.Y
                let mx,my=middle.X-a.X,middle.Y-a.Y
                let length=Double.Hypot(dx,dy)
                if not(Double.IsFinite length && Double.IsFinite mx && Double.IsFinite my) then
                    raise(Rejected(InvalidObservation "Projected line span exceeds representable screen coordinates."))
                let error,parameter =
                    if length=0. then Double.Hypot(mx,my),0.5
                    else
                        let ux,uy=dx/length,dy/length
                        abs(ux*my-uy*mx),(ux*mx+uy*my)/length
                // Returns (screen deviation exceeds precision, sample spacing is uneven) for a quarter sample.
                let quarter amount =
                    spend 1L
                    let sample=interpolate amount |> screen
                    let qx,qy=sample.X-a.X,sample.Y-a.Y
                    if not(Double.IsFinite qx && Double.IsFinite qy) then
                        raise(Rejected(InvalidObservation "Projected line span exceeds representable screen coordinates."))
                    if length=0. then struct(Double.Hypot(qx,qy)>options.Precision,false)
                    else
                        let ux,uy=dx/length,dy/length
                        struct(abs(ux*qy-uy*qx)>options.Precision,abs((ux*qx+uy*qy)/length-amount)>0.3)
                let mutable subdivide=error>options.Precision || abs(parameter-0.5)>0.3 || arc.Angle>Math.PI/6.
                if not subdivide then
                    let struct(firstDeviates,firstUneven)=quarter 0.25
                    let struct(secondDeviates,secondUneven)=quarter 0.75
                    subdivide<-firstDeviates || firstUneven || secondDeviates || secondUneven
                if subdivide then
                    if depth>=options.MaxSubdivisionDepth then
                        // Spacing and arc-length rules only guard folds the deviation test could miss. At the depth limit a
                        // chord whose midpoint lies on it and whose samples stay within precision is accurate, as near a
                        // stretched conformal pole; otherwise the requested screen precision is genuinely unmet.
                        let struct(firstDeviates,_)=quarter 0.25
                        let struct(secondDeviates,_)=quarter 0.75
                        if error>options.Precision || parameter<0. || parameter>1. || firstDeviates || secondDeviates then
                            raise(Rejected(BudgetExceeded "Projected line cannot meet precision within its subdivision depth."))
                        emit b
                    else
                        subdivideArc (depth+1) first midpoint firstHalf a middle
                        subdivideArc (depth+1) midpoint last secondHalf middle b
                else emit b
        // The precision test is monotone in arc length, so a cheap upper bound that passes lets short
        // cylindrical arcs skip solving their great circle.
        let shortEnough (first:GeoPosition) (last:GeoPosition) =
            cylindrical &&
            let bound=GeoMath.distanceBound first last*(Math.PI/180.)
            let latitudeLimit=min (abs first.Latitude) (abs last.Latitude)*(Math.PI/180.)+bound
            bound<=Math.PI/6. && latitudeLimit<Math.PI/2. && scale*bound/cos(latitudeLimit)*(1.+1e-12)<=options.Precision
        fun depth (first:GeoPosition) (last:GeoPosition) (a:Point) (b:Point) ->
            if shortEnough first last then
                spend 1L
                emit b
            else
            match GeoArc.create first last with
            | Ok arc ->
                if cartesianFast then
                    let seamLonRad =
                        if first.Longitude=last.Longitude && abs first.Longitude=180. then first.Longitude*(Math.PI/180.)
                        else 0.
                    subdivideCartesian depth seamLonRad arc.X arc.Y arc.Z arc.TangentX arc.TangentY arc.TangentZ arc.Angle a b
                else
                    subdivideArc depth first last arc a b
            | Error errors -> raise(Rejected errors.Head)

    /// <summary>Projects geometry with cooperative cancellation and atomic result publication.</summary>
    /// <remarks>Sphere emits the complete equirectangular rectangle, the projected clip circle of orthographic and azimuthal kinds, or the canonical Mercator square (raw coordinates bounded by pi), intersected with ClipExtent. PointRadius affects point symbols only.</remarks>
    let projectWithCancellation (options:GeoPathOptions) (token:CancellationToken) projection geometry : Result<Path list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not(ProjectedPath.validOptions options) then
            Error [InvalidObservation "Geographic paths require positive finite precision, nonnegative finite radius and clip extent, positive budgets, and subdivision depth in [1,32]."]
        else
            try
                let emptyViewport=options.ClipExtent |> Option.exists(fun clip -> Bounds.width clip=0. || Bounds.height clip=0.)
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
                let circle=ProjectedPath.circle options (fun () -> options.MaxWork-work) spend reserve paths.Add
                let polygonScreen point =
                    match Projection.projectRotatedValue projection point with
                    | ValueSome value -> value
                    | ValueNone -> raise(Rejected(InvalidObservation "Polygon contains an undefined projected vertex."))
                let makeSampler emit =
                    let segment=resampler options projection spend polygonScreen emit
                    fun first last -> segment 0 first last (polygonScreen first) (polygonScreen last)
                // Intersects rejoined rings with the viewport and publishes them as one compound path.
                let addRings rings =
                    let rings =
                        match options.ClipExtent with
                        | None -> rings
                        | Some clip ->
                            match GeoRectangle.polygons options.MaxBufferedPoints spend clip rings with
                            | Ok clipped -> clipped
                            | Error error -> raise(Rejected error)
                    match rings with
                    | [] -> ()
                    | [ring] when ring.Length>0 ->
                        reserve ring.Length
                        spend(int64 ring.Length)
                        let mutable owned=[Close]
                        spend 1L
                        for index=ring.Length-1 downto 1 do
                            spend 1L
                            owned<-LineTo ring[index]::owned
                        spend 1L
                        owned<-MoveTo ring[0]::owned
                        paths.Add(Path.ofCommands owned)
                    | _ ->
                        let ringArr=List.toArray rings
                        let mutable hasAny=false
                        for r=0 to ringArr.Length-1 do
                            let ring=ringArr[r]
                            if ring.Length>0 then
                                hasAny<-true
                                reserve ring.Length
                                spend(int64 ring.Length)
                        if hasAny then
                            let mutable owned=[]
                            for r=ringArr.Length-1 downto 0 do
                                let ring=ringArr[r]
                                if ring.Length>0 then
                                    spend 1L
                                    owned<-Close::owned
                                    for index=ring.Length-1 downto 1 do
                                        spend 1L
                                        owned<-LineTo ring[index]::owned
                                    spend 1L
                                    owned<-MoveTo ring[0]::owned
                            paths.Add(Path.ofCommands owned)
                let sphere () =
                    let origin =
                        match GeoPosition.create 0. 0. |> Result.bind(fun value ->
                            match Projection.projectRotatedValue projection value with
                            | ValueSome point -> Ok point
                            | ValueNone -> Error [InvalidObservation "Projection boundary exceeds representable screen coordinates."]) with
                        | Ok point -> point
                        | Error errors -> raise(Rejected errors.Head)
                    let scale=Projection.scale projection
                    match Projection.kind projection with
                    | _ when (Projection.clipCircle projection).IsSome -> circle (scale*Projection.clipPlaneRadius projection) origin
                    | _ when Projection.conic projection ->
                        match GeoPolygon.outline projection options.Precision options.MaxSubdivisionDepth options.MaxBufferedPoints spend makeSampler with
                        | Ok rings -> addRings rings
                        | Error error -> raise(Rejected error)
                    | _ ->
                        let halfWidth=scale*Math.PI
                        let halfHeight=if Projection.mercatorBand projection then halfWidth else halfWidth/2.
                        let mutable left=origin.X-halfWidth
                        let mutable right=origin.X+halfWidth
                        let mutable top=origin.Y-halfHeight
                        let mutable bottom=origin.Y+halfHeight
                        if not(Double.IsFinite left && Double.IsFinite right && Double.IsFinite top && Double.IsFinite bottom) then
                            raise(Rejected(InvalidObservation "Projection boundary exceeds representable screen coordinates."))
                        match options.ClipExtent with
                        | Some clip ->
                            left<-max left (Bounds.x clip)
                            right<-min right (Bounds.x clip+Bounds.width clip)
                            top<-max top (Bounds.y clip)
                            bottom<-min bottom (Bounds.y clip+Bounds.height clip)
                        | None -> ()
                        if left<right && top<bottom then
                            reserve 4
                            paths.Add(Path.ofCommands [
                                MoveTo {X=left;Y=top};LineTo {X=right;Y=top}
                                LineTo {X=right;Y=bottom};LineTo {X=left;Y=bottom};Close])
                let lineInput=ResizeArray<GeoPosition>()
                let lineCommands=ResizeArray<PathCommand>()
                let mutable inLine=false
                let mutable inPolygon=false
                let polygonInput=ResizeArray<GeoPosition array>()
                let mutable polygonInputCount=0
                let finishLine () =
                    if lineInput.Count>0 then
                        lineCommands.Clear()
                        let mutable pending=ValueNone
                        let mutable tail=ValueNone
                        let mutable outputTail=ValueNone
                        let flushOutput () =
                            if lineCommands.Count>1 then
                                let mutable commands=[]
                                for index=lineCommands.Count-1 downto 0 do
                                    spend 1L
                                    commands<-lineCommands[index]::commands
                                paths.Add(Path.ofCommands commands)
                            lineCommands.Clear()
                            pending<-ValueNone
                            outputTail<-ValueNone
                        let startFragment point =
                            lineCommands.Clear()
                            pending<-if options.ClipExtent.IsNone then ValueSome point else ValueNone
                            outputTail<-ValueNone
                            tail<-ValueSome point
                        let emitLine point =
                            let count=if pending.IsSome then 2 else 1
                            reserve count
                            if count>options.MaxBufferedPoints-lineCommands.Count then
                                raise(Rejected(BudgetExceeded "Projected line exceeds its buffer budget."))
                            match pending with
                            | ValueSome first -> lineCommands.Add(MoveTo first); pending<-ValueNone
                            | ValueNone -> ()
                            lineCommands.Add(LineTo point)
                            outputTail<-ValueSome point
                        let clipLine=GeoRectangle.clipSegment spend
                        let addLine point =
                            match options.ClipExtent with
                            | None -> emitLine point
                            | Some clip ->
                                match tail with
                                | ValueNone -> raise(Rejected(InvalidObservation "Projected line fragment has no starting point."))
                                | ValueSome previous when previous=point -> ()
                                | ValueSome previous ->
                                    match clipLine clip previous point with
                                    | ValueNone -> flushOutput()
                                    | ValueSome(struct(first,last)) ->
                                        match outputTail with
                                        | ValueSome current when current=first -> ()
                                        | _ -> flushOutput(); pending<-ValueSome first
                                        emitLine last
                                        if last<>point then flushOutput()
                            tail<-ValueSome point
                        let flush () =
                            flushOutput()
                            tail<-ValueNone
                        let screen position =
                            match Projection.projectRotatedValue projection position with
                            | ValueSome value -> value
                            | ValueNone -> raise(Rejected(InvalidObservation "This line requires spherical preclipping before projection."))
                        let segment=resampler options projection spend screen addLine
                        let sink = {
                            Start=fun point -> startFragment(screen point)
                            Arc=fun first last ->
                                match tail with
                                | ValueSome previous -> segment 0 first last previous (screen last)
                                | ValueNone -> raise(Rejected(InvalidObservation "Projected line fragment has no starting point."))
                            Boundary=fun point -> addLine(screen point)
                            Finish=flush
                        }
                        match GeoPreclip.line projection spend sink lineInput with
                        | Ok () -> ()
                        | Error error -> raise(Rejected error)
                let finishPolygon () =
                    match GeoPolygon.project projection options.Precision options.MaxSubdivisionDepth options.MaxBufferedPoints spend makeSampler (polygonInput.ToArray()) with
                    | Error error -> raise(Rejected error)
                    | Ok rings -> addRings rings
                    polygonInput.Clear()
                    polygonInputCount<-0
                let folder () event =
                    spend 1L
                    match event with
                    | GeoStreamEvent.Point _ when emptyViewport -> ()
                    | GeoStreamEvent.Point position ->
                        if inLine then
                            if lineInput.Count>=options.MaxBufferedPoints then
                                raise(Rejected(BudgetExceeded "Input line exceeds its projection buffer budget."))
                            lineInput.Add position
                        else
                            let rotated=Projection.rotate projection position
                            let insideCap =
                                match Projection.clipCircle projection with
                                | ValueSome struct(_,_,cosine) -> let struct(x,_,_)=GeoMath.vector rotated in x>=cosine
                                | ValueNone -> true
                            if insideCap && (not(Projection.mercatorBand projection) || abs rotated.Latitude<=GeoPreclip.mercatorLatitude) then
                                match Projection.projectRotatedValue projection rotated with
                                | ValueSome point -> circle options.PointRadius point
                                | ValueNone -> ()
                    | GeoStreamEvent.Sphere -> if not emptyViewport then sphere()
                    | GeoStreamEvent.LineStart -> lineInput.Clear(); inLine<-true
                    | GeoStreamEvent.LineEnd ->
                        if emptyViewport then ()
                        elif inPolygon then
                            if lineInput.Count>options.MaxBufferedPoints-polygonInputCount then
                                raise(Rejected(BudgetExceeded "Polygon input exceeds its vertex buffer budget."))
                            spend(int64 lineInput.Count)
                            polygonInputCount<-polygonInputCount+lineInput.Count
                            polygonInput.Add(lineInput.ToArray())
                        else finishLine()
                        inLine<-false
                    | GeoStreamEvent.PolygonStart -> inPolygon<-true;polygonInput.Clear();polygonInputCount<-0
                    | GeoStreamEvent.PolygonEnd ->
                        if polygonInput.Count>0 then finishPolygon()
                        inPolygon<-false
                match GeoStream.foldWithCancellation options.InputLimits token folder () geometry with
                | Error errors -> Error errors
                | Ok () ->
                    let mutable result=[]
                    for index=paths.Count-1 downto 0 do
                        spend 1L
                        result <- paths[index]::result
                    token.ThrowIfCancellationRequested()
                    Ok result
            with
            | Rejected error | ProjectedPath.Rejected error -> Error [error]

    /// <summary>Projects geometry with the configured budgets and no cancellation token.</summary>
    let project options projection geometry = projectWithCancellation options CancellationToken.None projection geometry

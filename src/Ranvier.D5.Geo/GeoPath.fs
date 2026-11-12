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
    /// <summary>Maximum vertices retained in any intermediate clipping buffer.</summary>
    MaxBufferedPoints: int
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
        ClipExtent=None; MaxOutputPoints=1_000_000; MaxBufferedPoints=1_000_000; MaxWork=10_000_000L; MaxSubdivisionDepth=16
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
        if options.MaxOutputPoints < 1 || options.MaxBufferedPoints < 1 || options.MaxWork < 1L ||
           options.MaxSubdivisionDepth < 1 || options.MaxSubdivisionDepth > 32 ||
           not(Double.IsFinite options.Precision) || options.Precision <= 0. ||
           not(Double.IsFinite options.PointRadius) || options.PointRadius < 0. || not validExtent then
            Error [InvalidObservation "Geographic paths require positive finite precision, nonnegative finite radius and clip extent, positive budgets, and subdivision depth in [1,32]."]
        else
            try
                let paths=ResizeArray<Path>()
                let clipBufferA=ResizeArray<Point>()
                let clipBufferB=ResizeArray<Point>()
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
                let clippedCircle (point:Point) clip =
                    let radius=options.PointRadius
                    let angle=4.*asin(sqrt(min 0.5 (options.Precision/radius/2.)))
                    let required=max 4. (4.*ceil((2.*Math.PI/angle)/4.))
                    if not(Double.IsFinite required) || required > float options.MaxBufferedPoints ||
                       required > float(options.MaxWork-work) || required > 4.*pown 2. options.MaxSubdivisionDepth then
                        raise(Rejected(BudgetExceeded "Clipped point precision exceeds its buffer, work or subdivision budget."))
                    let count=int required
                    clipBufferA.Clear()
                    clipBufferB.Clear()
                    let mutable vertices=clipBufferA
                    for index=0 to count-1 do
                        spend 1L
                        let struct(sine,cosine)=GeoMath.sinCosDegrees(float index*360./float count)
                        vertices.Add {X=point.X+radius*cosine;Y=point.Y+radius*sine}
                    let left,top=Bounds.x clip,Bounds.y clip
                    let right,bottom=left+Bounds.width clip,top+Bounds.height clip
                    for axis,boundary,keepGreater in [true,left,true;true,right,false;false,top,true;false,bottom,false] do
                        if vertices.Count>0 then
                            let output=if Object.ReferenceEquals(vertices,clipBufferA) then clipBufferB else clipBufferA
                            output.Clear()
                            let coordinate (value:Point)=if axis then value.X else value.Y
                            let inside value=if keepGreater then coordinate value>=boundary else coordinate value<=boundary
                            let add value =
                                spend 1L
                                if output.Count=0 || output[output.Count-1]<>value then
                                    if output.Count>=options.MaxBufferedPoints then raise(Rejected(BudgetExceeded "Clipped point exceeds its buffer budget."))
                                    output.Add value
                            let intersection (a:Point) (b:Point) =
                                let ca,cb=coordinate a,coordinate b
                                let da,db=ca-boundary,cb-boundary
                                let t =
                                    if Double.IsFinite da && Double.IsFinite db && Double.IsFinite(da-db) then da/(da-db)
                                    else
                                        let scale=max (abs boundary) (max (abs ca) (abs cb))
                                        let sa,sb=ca/scale-boundary/scale,cb/scale-boundary/scale
                                        sa/(sa-sb)
                                let mix x y=(1.-t)*x+t*y
                                if axis then {X=boundary;Y=mix a.Y b.Y}
                                else {X=mix a.X b.X;Y=boundary}
                            let mutable previous=vertices[vertices.Count-1]
                            let mutable previousInside=inside previous
                            for current in vertices do
                                spend 1L
                                let currentInside=inside current
                                if currentInside<>previousInside then add(intersection previous current)
                                if currentInside then add current
                                previous<-current
                                previousInside<-currentInside
                            if output.Count>1 && output[0]=output[output.Count-1] then output.RemoveAt(output.Count-1)
                            vertices<-output
                    if vertices.Count>=3 then
                        reserve vertices.Count
                        let mutable commands=[Close]
                        for index=vertices.Count-1 downto 1 do
                            spend 1L
                            commands<-LineTo vertices[index]::commands
                        paths.Add(Path.ofCommands(MoveTo vertices[0]::commands))
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
                                else
                                    clippedCircle point clip
                                    false
                        if visible then
                            reserve 3
                            let start={X=right;Y=point.Y}
                            paths.Add(Path.ofCommands [
                                MoveTo start
                                ArcTo(radius,radius,0.,false,true,{X=left;Y=point.Y})
                                ArcTo(radius,radius,0.,false,true,start)
                                Close])
                let lineInput=ResizeArray<GeoPosition>()
                let lineCommands=ResizeArray<PathCommand>()
                let mutable inLine=false
                let finishLine () =
                    if lineInput.Count>0 then
                        if options.ClipExtent.IsSome then
                            raise(Rejected(InvalidObservation "Rectangular line clipping is not yet implemented."))
                        lineCommands.Clear()
                        let add command =
                            reserve 1
                            if lineCommands.Count>=options.MaxBufferedPoints then
                                raise(Rejected(BudgetExceeded "Projected line exceeds its buffer budget."))
                            lineCommands.Add command
                        let screen position =
                            match Projection.projectValue projection position with
                            | ValueSome value -> value
                            | ValueNone -> raise(Rejected(InvalidObservation "This line requires spherical preclipping before projection."))
                        let rec segment depth (first:GeoPosition) (last:GeoPosition) (a:Point) (b:Point) =
                            spend 1L
                            let start,finish=GeoPosition.normalize first,GeoPosition.normalize last
                            if abs(start.Longitude-finish.Longitude)>=180. then
                                raise(Rejected(InvalidObservation "Antimeridian and pole-crossing line preclipping is not yet implemented."))
                            let arc =
                                match GeoArc.create first last with
                                | Ok value -> value
                                | Error errors -> raise(Rejected errors.Head)
                            let interpolate amount = GeoArc.atFraction amount arc
                            let midpoint=interpolate 0.5
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
                            let mutable subdivide=error>options.Precision || abs(parameter-0.5)>0.3 || arc.Angle>Math.PI/6.
                            if not subdivide then
                                let quarterExceeds amount =
                                    spend 1L
                                    let sample=interpolate amount |> screen
                                    let qx,qy=sample.X-a.X,sample.Y-a.Y
                                    if not(Double.IsFinite qx && Double.IsFinite qy) then
                                        raise(Rejected(InvalidObservation "Projected line span exceeds representable screen coordinates."))
                                    if length=0. then Double.Hypot(qx,qy)>options.Precision
                                    else
                                        let ux,uy=dx/length,dy/length
                                        abs(ux*qy-uy*qx)>options.Precision || abs((ux*qx+uy*qy)/length-amount)>0.3
                                subdivide<-quarterExceeds 0.25 || quarterExceeds 0.75
                            if subdivide then
                                if depth>=options.MaxSubdivisionDepth then
                                    raise(Rejected(BudgetExceeded "Projected line cannot meet precision within its subdivision depth."))
                                segment (depth+1) first midpoint a middle
                                segment (depth+1) midpoint last middle b
                            else add(LineTo b)
                        let mutable previous=lineInput[0]
                        let mutable previousScreen=screen previous
                        add(MoveTo previousScreen)
                        for index=1 to lineInput.Count-1 do
                            spend 1L
                            let current=lineInput[index]
                            let currentScreen=screen current
                            segment 0 previous current previousScreen currentScreen
                            previous<-current
                            previousScreen<-currentScreen
                        let mutable commands=[]
                        for index=lineCommands.Count-1 downto 0 do
                            spend 1L
                            commands<-lineCommands[index]::commands
                        paths.Add(Path.ofCommands commands)
                let folder () event =
                    spend 1L
                    match event with
                    | GeoStreamEvent.Point position ->
                        if inLine then
                            if lineInput.Count>=options.MaxBufferedPoints then
                                raise(Rejected(BudgetExceeded "Input line exceeds its projection buffer budget."))
                            lineInput.Add position
                        else
                            match Projection.projectValue projection position with
                            | ValueSome point -> circle point
                            | ValueNone -> ()
                    | GeoStreamEvent.LineStart -> lineInput.Clear(); inLine<-true
                    | GeoStreamEvent.LineEnd -> finishLine(); inLine<-false
                    | _ -> raise(Rejected(InvalidObservation "Geographic polygon path projection is not yet implemented."))
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

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
    /// <summary>Optional rectangular output extent, intersected with the projection boundary. Mercator retains its finite canonical world even with an explicit extent.</summary>
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

module internal ProjectedPath =
    exception Rejected of LayoutError

    let validOptions (options:GeoPathOptions) =
        let validExtent =
            match options.ClipExtent with
            | None -> true
            | Some extent ->
                Double.IsFinite (Bounds.x extent) && Double.IsFinite (Bounds.y extent) &&
                Double.IsFinite (Bounds.width extent) && Double.IsFinite (Bounds.height extent) &&
                Bounds.width extent>=0. && Bounds.height extent>=0. &&
                Double.IsFinite(Bounds.x extent+Bounds.width extent) && Double.IsFinite(Bounds.y extent+Bounds.height extent)
        options.MaxOutputPoints>=1 && options.MaxBufferedPoints>=1 && options.MaxWork>=1L &&
        options.MaxSubdivisionDepth>=1 && options.MaxSubdivisionDepth<=32 &&
        Double.IsFinite options.Precision && options.Precision>0. &&
        Double.IsFinite options.PointRadius && options.PointRadius>=0. && validExtent

    let circle (options:GeoPathOptions) (remainingWork:unit -> int64) spend reserve addPath =
        let clipBufferA=ResizeArray<Point>()
        let clipBufferB=ResizeArray<Point>()
        let clippedCircle (point:Point) radius clip =
            let angle=4.*asin(sqrt(min 0.5 (options.Precision/radius/2.)))
            let required=max 4. (4.*ceil((2.*Math.PI/angle)/4.))
            if not(Double.IsFinite required) || required > float options.MaxBufferedPoints ||
               required > float(remainingWork()) || required > 4.*pown 2. options.MaxSubdivisionDepth then
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
                addPath(Path.ofCommands(MoveTo vertices[0]::commands))
        let circle radius (point:Point) =
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
                            clippedCircle point radius clip
                            false
                if visible then
                    reserve 3
                    let start={X=right;Y=point.Y}
                    addPath(Path.ofCommands [
                        MoveTo start
                        ArcTo(radius,radius,0.,false,true,{X=left;Y=point.Y})
                        ArcTo(radius,radius,0.,false,true,start)
                        Close])
        circle


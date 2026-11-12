namespace Ranvier.D5.Charts

open Ranvier.D5

type Margin = { Top:float; Right:float; Bottom:float; Left:float }
type ChartFrame = { Viewport:Viewport; Margin:Margin; Title:string }
type SemanticMark = { MarkPath:string list; DataIds:string list; Description:string }
/// A polygon with boundary-inclusive outer ring and boundary-exclusive holes; winding order is immaterial.
type HitPolygon = { Outer:Point list; Holes:Point list list }
type HitGeometry = HitBounds of Bounds | HitPoint of Point * radius:float | HitSector of center:Point * inner:float * outer:float * start:float * finish:float | HitRegion of included:HitPolygon list * excluded:HitPolygon list * anchor:Point
type HitTarget = { MarkPath:string list; DataIds:string list; Geometry:HitGeometry; Clip:Bounds option }
type ChartOutput = { Scene:Scene; PlotBounds:Bounds; Semantics:SemanticMark list; HitTargets:HitTarget list; Description:string }
type DomainPolicy = Auto | IncludeZero | Fixed of float * float
type LabelRequest = { Id:string; Text:string; Position:Point; Priority:int }
type PlacedLabel = { Id:string; Text:string; Bounds:Bounds }

[<RequireQualifiedAccess>]
module ChartFrame =
    let create width height title =
        {Viewport={Width=width;Height=height};Margin={Top=20.;Right=20.;Bottom=30.;Left=40.};Title=title}

[<RequireQualifiedAccess>]
module HitTest =
    let private containsPolygon point (polygon:HitPolygon) =
        let mutable left,right,top,bottom=infinity,-infinity,infinity,-infinity
        for vertex in polygon.Outer do
            left<-min left vertex.X;right<-max right vertex.X
            top<-min top vertex.Y;bottom<-max bottom vertex.Y
        point.X>=left && point.X<=right && point.Y>=top && point.Y<=bottom &&
        Polygon.contains point polygon.Outer &&
        not (polygon.Holes |> List.exists (Polygon.contains point))
    let rec private containsAny point polygons =
        match polygons with
        | [] -> false
        | polygon::rest -> containsPolygon point polygon || containsAny point rest
    let visibleAt point target = target.Clip |> Option.forall (Bounds.contains point)
    let contains point geometry =
            match geometry with
            | HitRegion(included,excluded,_) -> containsAny point included && not (containsAny point excluded)
            | HitBounds bounds -> Bounds.contains point bounds
            | HitPoint(center,radius) -> System.Double.Hypot(point.X-center.X,point.Y-center.Y)<=radius
            | HitSector(center,inner,outer,start,finish) ->
                let radius=System.Double.Hypot(point.X-center.X,point.Y-center.Y)
                let tau=2.*System.Math.PI
                let normalize x=(x%tau+tau)%tau
                let angle=normalize(atan2 (point.X-center.X) (center.Y-point.Y))
                let sweep=abs(finish-start)
                let position=if finish>=start then normalize(angle-start) else normalize(start-angle)
                sweep>0. && outer>inner && radius>=inner && radius<=outer && (sweep>=tau || position<=sweep)
    let at point output =
        output.HitTargets |> List.filter (fun target -> visibleAt point target && contains point target.Geometry)

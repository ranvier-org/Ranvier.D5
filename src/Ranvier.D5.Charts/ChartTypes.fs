namespace Ranvier.D5.Charts

open System

open Ranvier.D5

/// <summary>Space in screen pixels reserved on each side between the viewport edge and the plot area.</summary>
type Margin = { Top:float; Right:float; Bottom:float; Left:float }
/// <summary>The viewport, margins and accessible title of one chart; the plot area is the viewport inset by the margins.</summary>
type ChartFrame = { Viewport:Viewport; Margin:Margin; Title:string }
/// <summary>An accessible description of one mark: its path in the scene, the source data IDs it represents, and its text.</summary>
type SemanticMark = { MarkPath:string list; DataIds:string list; Description:string }
/// A polygon with boundary-inclusive outer ring and boundary-exclusive holes; winding order is immaterial.
type HitPolygon = { Outer:Point list; Holes:Point list list }
/// <summary>Pick geometry in screen pixels (y down): a box, a disc, an annular sector with angles in radians clockwise from 12 o'clock, or included polygons minus excluded ones with an interior anchor point.</summary>
type HitGeometry = HitBounds of Bounds | HitPoint of Point * radius:float | HitSector of center:Point * inner:float * outer:float * start:float * finish:float | HitRegion of included:HitPolygon list * excluded:HitPolygon list * anchor:Point
/// <summary>A pickable region tied to a mark path and the data IDs it selects; when Clip is set, points outside it never hit.</summary>
type HitTarget = { MarkPath:string list; DataIds:string list; Geometry:HitGeometry; Clip:Bounds option }
/// <summary>A built chart: the scene to render, the plot area in screen pixels, accessible semantics, hit targets in draw order, and a description.</summary>
type ChartOutput = { Scene:Scene; PlotBounds:Bounds; Semantics:SemanticMark list; HitTargets:HitTarget list; Description:string }
/// <summary>How a numeric scale domain is chosen: Auto uses the data extent, IncludeZero widens it to include zero, and Fixed uses the given lower and upper bounds.</summary>
type DomainPolicy = Auto | IncludeZero | Fixed of float * float
/// <summary>A candidate label whose Position is its top-left corner in screen pixels; higher Priority places first.</summary>
type LabelRequest = { Id:string; Text:string; Position:Point; Priority:int }
/// <summary>A label accepted by LabelLayout.place with its final screen bounds.</summary>
type PlacedLabel = { Id:string; Text:string; Bounds:Bounds }

/// <summary>Constructors for chart frames.</summary>
[<RequireQualifiedAccess>]
module ChartFrame =
    /// <summary>Creates a frame of the given viewport size in pixels with margins of 20 top, 20 right, 30 bottom and 40 left.</summary>
    let create width height title =
        {Viewport={Width=width;Height=height};Margin={Top=20.;Right=20.;Bottom=30.;Left=40.};Title=title}

/// <summary>Pure point picking against chart hit targets in screen pixels.</summary>
[<RequireQualifiedAccess>]
module HitTest =
    let private containsPolygon (point:Point) (polygon:HitPolygon) =
        let mutable left,right,top,bottom=infinity,-infinity,infinity,-infinity
        for vertex in polygon.Outer do
            left<-min left vertex.X;right<-max right vertex.X
            top<-min top vertex.Y;bottom<-max bottom vertex.Y
        point.X>=left && point.X<=right && point.Y>=top && point.Y<=bottom &&
        Polygon.contains point polygon.Outer &&
        not (polygon.Holes |> List.exists (Polygon.contains point))
    let rec private containsAny (point:Point) polygons =
        match polygons with
        | [] -> false
        | polygon::rest -> containsPolygon point polygon || containsAny point rest
    /// <summary>True when the target has no clip or its clip bounds contain the point.</summary>
    let visibleAt (point:Point) target = target.Clip |> Option.forall (Bounds.contains point)
    /// <summary>True when the point lies inside the geometry; disc and sector boundaries are inclusive and an empty sector sweep never hits.</summary>
    let contains (point:Point) geometry =
            match geometry with
            | HitRegion(included,excluded,_) -> containsAny point included && not (containsAny point excluded)
            | HitBounds bounds -> Bounds.contains point bounds
            | HitPoint(center,radius) -> Double.Hypot(point.X-center.X,point.Y-center.Y)<=radius
            | HitSector(center,inner,outer,start,finish) ->
                let radius=Double.Hypot(point.X-center.X,point.Y-center.Y)
                let tau=2.*System.Math.PI
                let normalize x=(x%tau+tau)%tau
                let angle=normalize(atan2 (point.X-center.X) (center.Y-point.Y))
                let sweep=abs(finish-start)
                let position=if finish>=start then normalize(angle-start) else normalize(start-angle)
                sweep>0. && outer>inner && radius>=inner && radius<=outer && (sweep>=tau || position<=sweep)
    /// <summary>Returns every hit target of the output that is visible at and contains the point, in output order.</summary>
    let at (point:Point) output =
        output.HitTargets |> List.filter (fun target -> visibleAt point target && contains point target.Geometry)

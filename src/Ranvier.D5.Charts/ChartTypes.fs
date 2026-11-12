namespace Ranvier.D5.Charts

open Ranvier.D5

type Margin = { Top:float; Right:float; Bottom:float; Left:float }
type ChartFrame = { Viewport:Viewport; Margin:Margin; Title:string }
type SemanticMark = { MarkPath:string list; DataIds:string list; Description:string }
type HitGeometry = HitBounds of Bounds | HitPoint of Point * radius:float
type HitTarget = { MarkPath:string list; DataIds:string list; Geometry:HitGeometry }
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
    let at point output =
        output.HitTargets |> List.filter (fun target ->
            match target.Geometry with
            | HitBounds bounds -> Bounds.contains point bounds
            | HitPoint(center,radius) -> System.Double.Hypot(point.X-center.X,point.Y-center.Y)<=radius)

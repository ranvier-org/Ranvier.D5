namespace Ranvier.D5.Charts

open System
open Ranvier.D5

/// <summary>Whether facet cells share one domain computed over all groups or compute one domain per group.</summary>
[<RequireQualifiedAccess>]
type FacetDomains = Shared | Independent
/// <summary>Facet grid specification: 1 to 1000 Columns, a nonnegative Gap in pixels, a domain function, and a builder called with each cell's frame, domain and group data.</summary>
type FacetSpec<'T,'Domain> = {Frame:ChartFrame;Columns:int;Gap:float;Domains:FacetDomains;Domain:'T list->'Domain;Build:ChartFrame->'Domain->'T list->Result<ChartOutput,LayoutError list>}

/// <summary>Small-multiple composition of chart builders.</summary>
[<RequireQualifiedAccess>]
module Facet =
    /// <summary>Builds chart values in a grid. Shared domains are computed once over all input; each cell namespaces definitions and semantic paths.</summary>
    let build spec groups =
        Recipe.run spec.Frame fst groups (fun bounds ->
            if spec.Columns<1 || spec.Columns>1000 || not(Double.IsFinite spec.Gap) || spec.Gap<0. then invalidArg "spec" "Invalid facet grid."
            let rows=max 1 ((List.length groups+spec.Columns-1)/spec.Columns)
            let width=(Bounds.width bounds-float(spec.Columns-1)*spec.Gap)/float spec.Columns
            let height=(Bounds.height bounds-float(rows-1)*spec.Gap)/float rows
            if width<=0. || height<=0. then invalidArg "spec" "Facet gaps leave no usable cells."
            let shared=if spec.Domains=FacetDomains.Shared then Some(spec.Domain(groups |> List.collect snd)) else None
            let cells=groups |> List.mapi (fun index (groupId,data) ->
                let frame={spec.Frame with Viewport={Width=width;Height=height};Title=groupId}
                let domain=shared |> Option.defaultWith (fun () -> spec.Domain data)
                let output=spec.Build frame domain data |> Recipe.unwrap
                let x=Bounds.x bounds+float(index%spec.Columns)*(width+spec.Gap)
                let y=Bounds.y bounds+float(index/spec.Columns)*(height+spec.Gap)
                let scope=Recipe.key ["facet";groupId]
                let rename key=Recipe.key [scope;key]
                let definition = function
                    | RectClip(key,bounds) -> RectClip(rename key,bounds)
                    | LinearGradient(key,a,b,stops) -> LinearGradient(rename key,a,b,stops)
                    | RadialGradient(key,center,radius,stops) -> RadialGradient(rename key,center,radius,stops)
                let rec node = function
                    | Leaf mark -> Leaf(Mark.fillDefinition mark |> Option.map (fun key -> Mark.withFillDefinition (rename key) mark) |> Option.defaultValue mark)
                    | Group(group,children) -> Group({group with Clip=Option.map rename group.Clip},List.map node children)
                let translate point=Point.create (point.X+x) (point.Y+y)
                let geometry = function
                    | HitRegion(included,excluded,anchor) ->
                        let polygon (p:HitPolygon)={Outer=List.map translate p.Outer;Holes=List.map (List.map translate) p.Holes}
                        HitRegion(List.map polygon included,List.map polygon excluded,translate anchor)
                    | HitPoint(center,radius) -> HitPoint(translate center,radius)
                    | HitBounds b -> HitBounds(Bounds.create (Bounds.x b+x) (Bounds.y b+y) (Bounds.width b) (Bounds.height b))
                    | HitSector(center,inner,outer,start,finish) -> HitSector(translate center,inner,outer,start,finish)
                let semantic (s:SemanticMark)={s with MarkPath=scope::s.MarkPath}
                let hit (h:HitTarget)={h with MarkPath=scope::h.MarkPath;Geometry=geometry h.Geometry;Clip=h.Clip |> Option.map(fun b -> Bounds.create (Bounds.x b+x) (Bounds.y b+y) (Bounds.width b) (Bounds.height b))}
                Group({GroupSpec.create scope with Transform=Transform.translate x y},Scene.nodes output.Scene |> List.map node),
                (Scene.definitions output.Scene |> List.map definition),(List.map semantic output.Semantics),(List.map hit output.HitTargets))
            let scene=Scene.compose spec.Frame.Viewport spec.Frame.Title (cells |> List.collect (fun (_,d,_,_) -> d)) (cells |> List.map (fun (n,_,_,_) -> n))
            {Scene=scene;PlotBounds=bounds;Semantics=cells |> List.collect (fun (_,_,s,_) -> s);HitTargets=cells |> List.collect (fun (_,_,_,h) -> h);Description=spec.Frame.Title})

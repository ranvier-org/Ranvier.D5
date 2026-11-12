namespace Ranvier.D5

/// <summary>Scene viewport dimensions in scene units; <c>Scene.compose</c> requires both to be finite and positive.</summary>
type Viewport = { Width:float; Height:float }
/// <summary>A gradient color stop; offset and opacity must lie in [0,1], and offsets must be nondecreasing within a gradient.</summary>
type GradientStop = { Offset:float; Color:Paint; Opacity:float }
/// <summary>A keyed, reusable scene resource that groups reference as a clip and marks reference as a gradient fill.</summary>
type SceneDefinition =
    /// <summary>A rectangular clip region in scene coordinates.</summary>
    | RectClip of key:string * bounds:Bounds
    /// <summary>A linear gradient from <c>start</c> to <c>finish</c> in scene coordinates; both points must be finite.</summary>
    | LinearGradient of key:string * start:Point * finish:Point * stops:GradientStop list
    /// <summary>A radial gradient about a finite center with a finite positive radius in scene units.</summary>
    | RadialGradient of key:string * center:Point * radius:float * stops:GradientStop list

/// <summary>Helpers for <c>SceneDefinition</c> values.</summary>
[<RequireQualifiedAccess>]
module SceneDefinition =
    /// <summary>Returns the definition's key, which must be nonempty and unique within a composed scene.</summary>
    let key = function RectClip(k,_) | LinearGradient(k,_,_,_) | RadialGradient(k,_,_,_) -> k
    let internal validate definition =
        if System.String.IsNullOrEmpty(key definition) then invalidArg "definitions" "Empty definition key."
        let stops =
            match definition with
            | RectClip _ -> []
            | LinearGradient(_,a,b,stops) ->
                Point.create a.X a.Y |> ignore
                Point.create b.X b.Y |> ignore
                stops
            | RadialGradient(_,center,radius,stops) ->
                Point.create center.X center.Y |> ignore
                Check.positive "radius" radius
                stops
        let mutable previous = -1.
        for stop in stops do
            if not(System.Double.IsFinite stop.Offset) || stop.Offset<previous || stop.Offset>1. || stop.Offset<0. then
                invalidArg "stops" "Offsets must be ordered in [0,1]."
            if not(System.Double.IsFinite stop.Opacity) || stop.Opacity<0. || stop.Opacity>1. then
                invalidArg "stops" "Opacity must be in [0,1]."
            previous <- stop.Offset

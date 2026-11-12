namespace Ranvier.D5

type Viewport = { Width:float; Height:float }
type GradientStop = { Offset:float; Color:Paint; Opacity:float }
type SceneDefinition =
    | RectClip of key:string * bounds:Bounds
    | LinearGradient of key:string * start:Point * finish:Point * stops:GradientStop list
    | RadialGradient of key:string * center:Point * radius:float * stops:GradientStop list

[<RequireQualifiedAccess>]
module SceneDefinition =
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

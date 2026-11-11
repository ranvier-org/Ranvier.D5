namespace Ranvier.D5

open System

/// <summary>A serialized CSS paint value.</summary>
type Paint = private Paint of string

[<RequireQualifiedAccess>]
module Paint =
    /// <summary>Uses a typed Ranvier.Css value as SVG paint. The caller chooses a color-compatible CSS value.</summary>
    let ofCss value = Paint (Ranvier.Css.Value.render value)
    let private tokens text = Ranvier.Css.Value.unsafeFromTokens<unit> text |> ofCss
    let none = tokens "none"
    let currentColor = tokens "currentColor"
    /// <summary>Creates an opaque sRGB paint from integer channels in [0,255].</summary>
    let rgb red green blue =
        if List.exists (fun value -> value<0 || value>255) [red;green;blue] then invalidArg "channels" "Expected channels in [0,255]."
        tokens (sprintf "#%02x%02x%02x" red green blue)
    let toCss (Paint value) = value

/// <summary>Presentation shared by path and text marks.</summary>
type MarkStyle = { Fill:Paint; Stroke:Paint; StrokeWidth:float; Opacity:float; FontSize:float }

/// <summary>Renderable geometry; labels remain text and are escaped by the renderer.</summary>
type Geometry = PathGeometry of Path | TextGeometry of Point * string

/// <summary>An immutable visual item with a stable identity within its scene.</summary>
type Mark = private { Identity:string; Geometry:Geometry; Style:MarkStyle; Matrix:Transform; Label:string option }

[<RequireQualifiedAccess>]
module Mark =
    let private defaults = { Fill=Paint.currentColor; Stroke=Paint.none; StrokeWidth=1.; Opacity=1.; FontSize=12. }
    let private create key geometry =
        if String.IsNullOrEmpty key then invalidArg "key" "A mark requires a nonempty key."
        { Identity=key;Geometry=geometry;Style=defaults;Matrix=Transform.identity;Label=None }
    let path key geometry = create key (PathGeometry geometry)
    /// <summary>Creates a text mark at a finite scene coordinate.</summary>
    let text key point value =
        Point.create point.X point.Y |> ignore
        if isNull value then nullArg "value"
        create key (TextGeometry(point,value))
    let key mark = mark.Identity
    let geometry mark = mark.Geometry
    let style mark = mark.Style
    let transform mark = mark.Matrix
    let label mark = mark.Label
    let withFill fill mark = { mark with Style={mark.Style with Fill=fill} }
    /// <summary>Sets stroke paint and a nonnegative width in scene units.</summary>
    let withStroke stroke width mark =
        Check.finite "width" width
        if width<0. then invalidArg "width" "Stroke width must be nonnegative."
        { mark with Style={mark.Style with Stroke=stroke;StrokeWidth=width} }
    /// <summary>Sets opacity in [0,1].</summary>
    let withOpacity opacity mark =
        Check.finite "opacity" opacity
        if opacity<0. || opacity>1. then invalidArg "opacity" "Opacity must be in [0,1]."
        { mark with Style={mark.Style with Opacity=opacity} }
    let withFontSize size mark =
        Check.positive "size" size
        { mark with Style={mark.Style with FontSize=size} }
    let withTransform matrix mark = { mark with Matrix=matrix }
    let withLabel label mark = { mark with Label=label }

/// <summary>A finite viewport and ordered, uniquely keyed marks.</summary>
type Scene = private { Width:float; Height:float; Title:string; Marks:Mark list }

[<RequireQualifiedAccess>]
module Scene =
    /// <summary>Creates a scene with positive viewport dimensions; duplicate mark keys reject the scene.</summary>
    let create width height title marks =
        Check.positive "width" width
        Check.positive "height" height
        if isNull title then nullArg "title"
        Check.unique "marks" Mark.key marks
        {Width=width;Height=height;Title=title;Marks=marks}
    let width scene = scene.Width
    let height scene = scene.Height
    let title scene = scene.Title
    let marks scene = scene.Marks

/// <summary>The side on which an axis places ticks and labels.</summary>
[<RequireQualifiedAccess>]
type AxisOrientation = Bottom | Top | Left | Right

[<RequireQualifiedAccess>]
module Axis =
    let private build prefix orientation origin positions =
        Check.finite "origin" origin
        let horizontal = orientation=AxisOrientation.Bottom || orientation=AxisOrientation.Top
        let direction = if orientation=AxisOrientation.Top || orientation=AxisOrientation.Left then -1. else 1.
        let point along across = if horizontal then Point.create along across else Point.create across along
        let stroke path = path |> Mark.withFill Paint.none |> Mark.withStroke Paint.currentColor 1.
        let domain =
            match positions with
            | [] -> []
            | _ ->
                let first,_ = List.head positions
                let last,_ = List.last positions
                [Mark.path (prefix+"/domain") (Path.ofCommands [MoveTo(point first origin);LineTo(point last origin)]) |> stroke]
        domain @ (positions |> List.mapi (fun i (position,label) ->
            let key = prefix+"/"+string i
            [Mark.path (key+"/tick") (Path.ofCommands [MoveTo(point position origin);LineTo(point position (origin+direction*6.))]) |> stroke
             Mark.text (key+"/label") (point position (origin+direction*18.)) label]) |> List.concat)
    /// <summary>Builds composable numeric tick and label marks using a caller-supplied formatter.</summary>
    let numeric prefix orientation origin count scale format =
        Scale.ticks count scale |> List.map (fun value -> Scale.map scale value,format value) |> build prefix orientation origin
    /// <summary>Builds category ticks centered within known bands, preserving supplied category order.</summary>
    let categorical prefix orientation origin categories scale format =
        categories |> List.choose (fun key -> Scale.bandPosition scale key |> Option.map (fun x -> x+Scale.bandwidth scale/2.,format key))
        |> build prefix orientation origin

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
type TextStyle = { Family:string; Weight:int; Anchor:string; Baseline:string }

type Mark = private { Identity:string; Geometry:Geometry; Style:MarkStyle; Matrix:Transform; Label:string option; TextStyle:TextStyle; FillDefinition:string option }

[<RequireQualifiedAccess>]
module Mark =
    let private defaults = { Fill=Paint.currentColor; Stroke=Paint.none; StrokeWidth=1.; Opacity=1.; FontSize=12. }
    let private defaultText = {Family="sans-serif";Weight=400;Anchor="start";Baseline="alphabetic"}
    let private create key geometry =
        if String.IsNullOrEmpty key then invalidArg "key" "A mark requires a nonempty key."
        { Identity=key;Geometry=geometry;Style=defaults;Matrix=Transform.identity;Label=None;TextStyle=defaultText;FillDefinition=None }
    let path key geometry = create key (PathGeometry geometry)
    /// <summary>Creates a text mark at a finite scene coordinate.</summary>
    let text key point value =
        Point.create point.X point.Y |> ignore
        if isNull value then nullArg "value"
        create key (TextGeometry(point,value))
    let internal reframe key geometry style matrix mark = {mark with Identity=key;Geometry=geometry;Style=style;Matrix=matrix}
    let key mark = mark.Identity
    let geometry mark = mark.Geometry
    let style mark = mark.Style
    let transform mark = mark.Matrix
    let label mark = mark.Label
    let withFill fill mark = { mark with Style={mark.Style with Fill=fill};FillDefinition=None }
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
    let textStyle mark = mark.TextStyle
    let fillDefinition mark = mark.FillDefinition
    let withFillDefinition key mark =
        if String.IsNullOrEmpty key then invalidArg "key" "Empty definition key."
        {mark with FillDefinition=Some key}
    let withTextStyle style mark =
        if String.IsNullOrWhiteSpace style.Family then invalidArg "style" "Empty font family."
        if style.Weight<1 || style.Weight>1000 then invalidArg "style" "Font weight must be in [1,1000]."
        if not(List.contains style.Anchor ["start";"middle";"end"]) then invalidArg "style" "Unknown text anchor."
        if not(List.contains style.Baseline ["alphabetic";"middle";"central";"hanging";"text-before-edge";"text-after-edge"]) then invalidArg "style" "Unknown baseline."
        {mark with TextStyle=style}

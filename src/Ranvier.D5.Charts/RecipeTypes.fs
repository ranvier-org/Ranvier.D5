namespace Ranvier.D5.Charts

open Ranvier.D5

/// <summary>Bar direction: Vertical places categories along X with values rising up the screen; Horizontal places categories along Y with values along X.</summary>
[<RequireQualifiedAccess>]
type Orientation = Vertical | Horizontal
/// <summary>Bar arrangement: Single allows one bar per category, Grouped places series side by side, Stacked stacks series with diverging signs, and Normalized stacks series to fill [0,1].</summary>
[<RequireQualifiedAccess>]
type BarMode = Single | Grouped | Stacked | Normalized
/// <summary>Line rendering: Line strokes each series, Area fills to a baseline in data units, and StackedArea stacks series at shared X values with the given stack configuration.</summary>
[<RequireQualifiedAccess>]
type LineKind = Line | Area of baseline:float | StackedArea of StackConfig

/// <summary>Bar chart specification; Key gives each datum's unique semantic ID and missing values are skipped.</summary>
type BarSpec<'T> = { Frame:ChartFrame; Key:'T->string; Category:Encoding<'T,string>; Value:Encoding<'T,float option>; Series:'T->string; Color:string->Paint; Mode:BarMode; Orientation:Orientation; Domain:DomainPolicy }
/// <summary>Line or area chart specification with one path per series; Key gives each datum's unique semantic ID.</summary>
type LineSpec<'T> = { Frame:ChartFrame; Key:'T->string; X:Encoding<'T,float option>; Y:Encoding<'T,float option>; Series:'T->string; Color:string->Paint; Curve:CurveSpec; Kind:LineKind; XDomain:DomainPolicy; YDomain:DomainPolicy }
/// <summary>Scatter or bubble chart specification; Area is each circle's area in square pixels.</summary>
type ScatterSpec<'T> = { Frame:ChartFrame; Key:'T->string; X:Encoding<'T,float option>; Y:Encoding<'T,float option>; Area:Encoding<'T,float>; Color:'T->Paint; XDomain:DomainPolicy; YDomain:DomainPolicy }
/// <summary>Categorical heatmap specification with one cell per column and row pair; Color maps the cell value to paint.</summary>
type HeatmapSpec<'T> = { Frame:ChartFrame; Key:'T->string; Column:Encoding<'T,string>; Row:Encoding<'T,string>; Value:Encoding<'T,float option>; Color:float->Paint }

/// <summary>Default specifications for the Cartesian chart recipes.</summary>
[<RequireQualifiedAccess>]
module ChartSpec =
    /// <summary>Deterministically picks one of five palette colors from a hash of the key.</summary>
    let color (key:string) =
        let colors=[|Paint.rgb 52 112 190;Paint.rgb 225 112 45;Paint.rgb 43 150 100;Paint.rgb 155 80 180;Paint.rgb 195 65 85|]
        let hash=key |> Seq.fold (fun state c -> (state*31L+int64 c)%2147483647L) 0L
        colors[int(hash%int64 colors.Length)]
    /// <summary>A single-series vertical bar chart with a zero-including value domain and palette colors.</summary>
    let bar frame key category value = {Frame=frame;Key=key;Category=category;Value=value;Series=(fun _ -> "series");Color=color;Mode=BarMode.Single;Orientation=Orientation.Vertical;Domain=IncludeZero}
    /// <summary>A single-series linear line chart with automatic X and Y domains.</summary>
    let line frame key x y = {Frame=frame;Key=key;X=x;Y=y;Series=(fun _ -> "series");Color=color;Curve=CurveSpec.Linear;Kind=LineKind.Line;XDomain=Auto;YDomain=Auto}
    /// <summary>A scatter chart of 30 square-pixel circles in one color with automatic domains.</summary>
    let scatter frame key x y = {Frame=frame;Key=key;X=x;Y=y;Area=Encoding.create "Area" (fun _ -> 30.);Color=(fun _ -> color "series");XDomain=Auto;YDomain=Auto}
    /// <summary>A heatmap specification built from its fields.</summary>
    let heatmap frame key column row value color = {Frame=frame;Key=key;Column=column;Row=row;Value=value;Color=color}

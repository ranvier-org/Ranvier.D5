namespace Ranvier.D5.Charts

open Ranvier.D5

[<RequireQualifiedAccess>]
type Orientation = Vertical | Horizontal
[<RequireQualifiedAccess>]
type BarMode = Single | Grouped | Stacked | Normalized
[<RequireQualifiedAccess>]
type LineKind = Line | Area of baseline:float | StackedArea of StackConfig

type BarSpec<'T> = { Frame:ChartFrame; Key:'T->string; Category:Encoding<'T,string>; Value:Encoding<'T,float option>; Series:'T->string; Color:string->Paint; Mode:BarMode; Orientation:Orientation; Domain:DomainPolicy }
type LineSpec<'T> = { Frame:ChartFrame; Key:'T->string; X:Encoding<'T,float option>; Y:Encoding<'T,float option>; Series:'T->string; Color:string->Paint; Curve:CurveSpec; Kind:LineKind; XDomain:DomainPolicy; YDomain:DomainPolicy }
type ScatterSpec<'T> = { Frame:ChartFrame; Key:'T->string; X:Encoding<'T,float option>; Y:Encoding<'T,float option>; Area:Encoding<'T,float>; Color:'T->Paint; XDomain:DomainPolicy; YDomain:DomainPolicy }
type HeatmapSpec<'T> = { Frame:ChartFrame; Key:'T->string; Column:Encoding<'T,string>; Row:Encoding<'T,string>; Value:Encoding<'T,float option>; Color:float->Paint }

[<RequireQualifiedAccess>]
module ChartSpec =
    let color (key:string) =
        let colors=[|Paint.rgb 52 112 190;Paint.rgb 225 112 45;Paint.rgb 43 150 100;Paint.rgb 155 80 180;Paint.rgb 195 65 85|]
        let hash=key |> Seq.fold (fun state c -> (state*31L+int64 c)%2147483647L) 0L
        colors[int(hash%int64 colors.Length)]
    let bar frame key category value = {Frame=frame;Key=key;Category=category;Value=value;Series=(fun _ -> "series");Color=color;Mode=BarMode.Single;Orientation=Orientation.Vertical;Domain=IncludeZero}
    let line frame key x y = {Frame=frame;Key=key;X=x;Y=y;Series=(fun _ -> "series");Color=color;Curve=CurveSpec.Linear;Kind=LineKind.Line;XDomain=Auto;YDomain=Auto}
    let scatter frame key x y = {Frame=frame;Key=key;X=x;Y=y;Area=Encoding.create "Area" (fun _ -> 30.);Color=(fun _ -> color "series");XDomain=Auto;YDomain=Auto}
    let heatmap frame key column row value color = {Frame=frame;Key=key;Column=column;Row=row;Value=value;Color=color}

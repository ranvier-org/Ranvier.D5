namespace Ranvier.D5

open System

type ArcSpec = { InnerRadius:float; OuterRadius:float; StartAngle:float; EndAngle:float; PadAngle:float; CornerRadius:float }

[<RequireQualifiedAccess>]
type SymbolSpec = Circle | Square | Diamond | Triangle | Cross | Star | Wye | Plus | Times | Asterisk | Diamond2 | Square2 | Triangle2 | TriangleDown

[<RequireQualifiedAccess>]
module Shapes =
    /// <summary>Circular corner fillets with angular padding. Corners clamp to ring thickness and available angular span; complete rings ignore padding and corners.</summary>
    let arc spec =
        [spec.InnerRadius;spec.OuterRadius;spec.StartAngle;spec.EndAngle;spec.PadAngle;spec.CornerRadius] |> List.iter (Check.finite "spec")
        if spec.InnerRadius<0. || spec.OuterRadius<spec.InnerRadius || spec.PadAngle<0. || spec.CornerRadius<0. then invalidArg "spec" "Invalid arc radii or padding."
        let span=abs(spec.EndAngle-spec.StartAngle)
        Check.finite "span" span
        if span>=2.*Math.PI then Shape.arc spec.InnerRadius spec.OuterRadius spec.StartAngle spec.EndAngle
        elif spec.OuterRadius=0. || span<=spec.PadAngle then Path.empty
        else
            let sign=if spec.EndAngle>=spec.StartAngle then 1. else -1.
            let first=spec.StartAngle+sign*spec.PadAngle/2.
            let last=spec.EndAngle-sign*spec.PadAngle/2.
            let angle=span-spec.PadAngle
            let halfSin=sin(min Math.PI angle/2.)
            let thickness=(spec.OuterRadius-spec.InnerRadius)/2.
            let outerCorner=min spec.CornerRadius (min thickness (spec.OuterRadius*halfSin/(1.+halfSin)))
            let innerCorner=if spec.InnerRadius=0. then 0. else min spec.CornerRadius (min thickness (if halfSin>=1. then thickness else spec.InnerRadius*halfSin/(1.-halfSin)))
            if outerCorner=0. && innerCorner=0. then Shape.arc spec.InnerRadius spec.OuterRadius first last
            else
                let r0,r1=spec.InnerRadius,spec.OuterRadius
                let outerOffset=if outerCorner=0. then 0. else asin(Math.Clamp(outerCorner/(r1-outerCorner),0.,1.))
                let innerOffset=if innerCorner=0. then 0. else asin(Math.Clamp(innerCorner/(r0+innerCorner),0.,1.))
                let outerTangent=sqrt(max 0. ((r1-outerCorner)*(r1-outerCorner)-outerCorner*outerCorner))
                let innerTangent=sqrt(max 0. ((r0+innerCorner)*(r0+innerCorner)-innerCorner*innerCorner))
                let at angle radius=Curves.radialPoint angle radius
                let cw=sign>0.
                [yield MoveTo(at first outerTangent)
                 if outerCorner>0. then yield ArcTo(outerCorner,outerCorner,0.,false,cw,at (first+sign*outerOffset) r1)
                 yield ArcTo(r1,r1,0.,angle-2.*outerOffset>Math.PI,cw,at (last-sign*outerOffset) r1)
                 if outerCorner>0. then yield ArcTo(outerCorner,outerCorner,0.,false,cw,at last outerTangent)
                 yield LineTo(at last innerTangent)
                 if r0>0. then
                     if innerCorner>0. then yield ArcTo(innerCorner,innerCorner,0.,false,cw,at (last-sign*innerOffset) r0)
                     yield ArcTo(r0,r0,0.,angle-2.*innerOffset>Math.PI,not cw,at (first+sign*innerOffset) r0)
                     if innerCorner>0. then yield ArcTo(innerCorner,innerCorner,0.,false,cw,at first innerTangent)
                 yield Close] |> Path.ofCommands

    /// <summary>Filled symbols use area in square units. Stroke-oriented variants use d3-shape's optical size adjustment and require a stroke.</summary>
    let symbol kind size =
        Check.finite "size" size
        if size<0. then invalidArg "size" "Negative size."
        let polygon points=match points with [] -> Path.empty | first::rest -> Path.ofCommands(MoveTo first::(List.map LineTo rest)@[Close])
        let p x y=Point.create x y
        match kind with
        | SymbolSpec.Circle -> Shape.symbol Symbol.Circle size
        | SymbolSpec.Square -> Shape.symbol Symbol.Square size
        | SymbolSpec.Diamond -> Shape.symbol Symbol.Diamond size
        | SymbolSpec.Triangle -> Shape.symbol Symbol.Triangle size
        | SymbolSpec.TriangleDown ->
            let r=sqrt(size/sqrt 3.)
            polygon [p 0. (2.*r/sqrt 3.);p (-r) (-r/sqrt 3.);p r (-r/sqrt 3.)]
        | SymbolSpec.Cross ->
            let r=sqrt(size/5.)/2.
            [-3.,-1.;-1.,-1.;-1.,-3.;1.,-3.;1.,-1.;3.,-1.;3.,1.;1.,1.;1.,3.;-1.,3.;-1.,1.;-3.,1.]
            |> List.map (fun (x,y) -> p (x*r) (y*r)) |> polygon
        | SymbolSpec.Star ->
            let r=sqrt(size*0.8908130915292852)
            let inner=r*sin(Math.PI/10.)/sin(7.*Math.PI/10.)
            [for i in 0..9 -> Curves.radialPoint (float i*Math.PI/5.) (if i%2=0 then r else inner)] |> polygon
        | SymbolSpec.Wye ->
            let k=1./sqrt 12.
            let r=sqrt(size/((k/2.+1.)*3.))
            [for turn in 0..2 do
                let angle=float turn*2.*Math.PI/3.
                for x,y in [r/2.,r*k;r/2.,r*k+r;-r/2.,r*k+r] do
                    yield p (cos angle*x-sin angle*y) (sin angle*x+cos angle*y)] |> polygon
        | SymbolSpec.Diamond2 ->
            let r=sqrt size*0.62625
            polygon [p 0. (-r);p r 0.;p 0. r;p (-r) 0.]
        | SymbolSpec.Square2 ->
            let r=sqrt size*0.4431
            polygon [p r r;p r (-r);p (-r) (-r);p (-r) r]
        | SymbolSpec.Triangle2 ->
            let s=sqrt size*0.6824
            polygon [p 0. (-s);p (s*sqrt 3./2.) (s/2.);p (-s*sqrt 3./2.) (s/2.)]
        | SymbolSpec.Plus ->
            let r=sqrt(size-min (size/7.) 2.)*0.87559
            Path.ofCommands [MoveTo(p (-r) 0.);LineTo(p r 0.);MoveTo(p 0. r);LineTo(p 0. (-r))]
        | SymbolSpec.Times ->
            let r=sqrt(size-min (size/6.) 1.7)*0.6189
            Path.ofCommands [MoveTo(p (-r) (-r));LineTo(p r r);MoveTo(p (-r) r);LineTo(p r (-r))]
        | SymbolSpec.Asterisk ->
            let r=sqrt(size+min (size/28.) 0.75)*0.59436
            let t=r/2.
            let u=t*sqrt 3.
            Path.ofCommands [MoveTo(p 0. r);LineTo(p 0. (-r));MoveTo(p (-u) (-t));LineTo(p u t);MoveTo(p (-u) t);LineTo(p u (-t))]

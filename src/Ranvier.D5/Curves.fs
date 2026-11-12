namespace Ranvier.D5

open System

/// <summary>Spline end handling: <c>Clamped</c> passes through both endpoints, <c>Open</c> omits the end segments,
/// <c>Closed</c> wraps into a loop.</summary>
[<RequireQualifiedAccess>]
type CurveBoundary = Clamped | Open | Closed

/// <summary>A curve interpolation for <c>Curves.line</c> and <c>Curves.area</c>, following d3-shape's curve family.</summary>
[<RequireQualifiedAccess>]
type CurveSpec =
    | Linear | StepBefore | StepAfter | StepMiddle | MonotoneX | MonotoneY | Natural | BumpX | BumpY
    /// <summary>A uniform cubic B-spline with the given boundary handling.</summary>
    | Basis of CurveBoundary
    /// <summary>A cardinal spline; tension must be in [0,1], where 1 yields straight segments.</summary>
    | Cardinal of tension:float * CurveBoundary
    /// <summary>A Catmull–Rom spline; alpha must be in [0,1], where 0.5 is centripetal.</summary>
    | CatmullRom of alpha:float * CurveBoundary
    /// <summary>A clamped B-spline straightened toward the chord between the endpoints; beta must be in [0,1], where 0 is a straight line.</summary>
    | Bundle of beta:float

/// <summary>Pure spline kernels. Formulas adapted from d3-shape 3.2.0; see tools/reference-fixtures/licenses/d3-shape.txt.</summary>
[<RequireQualifiedAccess>]
module Curves =
    let private p x y=Point.create x y
    let private blend t (a:Point) (b:Point)=p (Check.lerp a.X b.X t) (Check.lerp a.Y b.Y t)
    let private weighted (a:Point) (b:Point) (c:Point)=p (a.X/6.+b.X*(2./3.)+c.X/6.) (a.Y/6.+b.Y*(2./3.)+c.Y/6.)
    let private linear (points:Point array) =
        if points.Length=0 then []
        elif points.Length=1 then [MoveTo points[0];Close]
        else MoveTo points[0]::(points |> Array.skip 1 |> Array.map LineTo |> Array.toList)

    let private runs points =
        let output=ResizeArray<_>()
        let mutable current=[]
        for point in points do
            match point with
            | Some value -> current<-value::current
            | None -> if not(List.isEmpty current) then output.Add(List.rev current); current<-[]
        if not(List.isEmpty current) then output.Add(List.rev current)
        List.ofSeq output

    let private basis boundary (points:Point array) =
        let n=points.Length
        let at i=points[(i+n)%n]
        let cubic a b c=CubicTo(blend (1./3.) a b,blend (2./3.) a b,weighted a b c)
        match boundary with
        | CurveBoundary.Clamped when n<3 -> linear points
        | CurveBoundary.Open when n<3 -> []
        | CurveBoundary.Closed when n<2 -> linear points
        | CurveBoundary.Closed when n=2 -> [MoveTo(blend (2./3.) points[0] points[1]);LineTo(blend (1./3.) points[0] points[1]);Close]
        | CurveBoundary.Clamped ->
            [yield MoveTo points[0]
             yield LineTo(blend (1./6.) points[0] points[1])
             for i in 0..n-3 do yield cubic points[i] points[i+1] points[i+2]
             yield cubic points[n-2] points[n-1] points[n-1]
             yield LineTo points[n-1]]
        | CurveBoundary.Open ->
            [yield MoveTo(weighted points[0] points[1] points[2])
             for i in 1..n-3 do yield cubic points[i] points[i+1] points[i+2]
             if n=3 then yield Close]
        | CurveBoundary.Closed ->
            [yield MoveTo(weighted points[0] points[1] points[2])
             for i in 1..n do yield cubic (at i) (at(i+1)) (at(i+2))]

    let private cardinalControls tension (a:Point) (b:Point) (c:Point) (d:Point) =
        let k=(1.-tension)/6.
        p (b.X+k*(c.X-a.X)) (b.Y+k*(c.Y-a.Y)),p (c.X+k*(b.X-d.X)) (c.Y+k*(b.Y-d.Y))

    let private catmullControls alpha (a:Point) (b:Point) (c:Point) (d:Point) =
        if alpha=0. then cardinalControls 0. a b c d else
        let distance (x:Point) (y:Point)=Math.Pow(Double.Hypot(x.X-y.X,x.Y-y.Y),alpha)
        let u,v,w=distance a b,distance b c,distance c d
        let control (previous:Point) (center:Point) (next:Point) left right =
            if left<=1e-12 then center else
            let denominator=3.*left*(left+right)
            let coefficient=2.*left*left+3.*left*right+right*right
            p ((center.X*coefficient-previous.X*right*right+next.X*left*left)/denominator)
              ((center.Y*coefficient-previous.Y*right*right+next.Y*left*left)/denominator)
        control a b c u v,control d c b w v

    let private interpolating cardinal boundary controls (points:Point array) =
        let n=points.Length
        let at i=points[(i+n)%n]
        match boundary with
        | CurveBoundary.Open when n<3 -> []
        | CurveBoundary.Open ->
            [yield MoveTo points[1]
             for i in 1..n-3 do
                 let a,b=controls points[i-1] points[i] points[i+1] points[i+2]
                 yield CubicTo(a,b,points[i+1])
             if n=3 then yield Close]
        | _ when n<3 ->
            if boundary=CurveBoundary.Closed && n=2 then [MoveTo points[1];LineTo points[0];Close] else linear points
        | CurveBoundary.Closed ->
            [yield MoveTo points[1]
             for i in 1..n do
                 let a,b=controls (at(i-1)) (at i) (at(i+1)) (at(i+2))
                 yield CubicTo(a,b,at(i+1))]
        | CurveBoundary.Clamped ->
            [yield MoveTo points[0]
             for i in 0..n-2 do
                 let before=if i=0 then points[if cardinal then 1 else 0] else points[i-1]
                 let after=if i=n-2 then points[if cardinal then n-2 else n-1] else points[i+2]
                 let a,b=controls before points[i] points[i+1] after
                 yield CubicTo(a,b,points[i+1])]

    let private natural (points:Point array) =
        let n=points.Length-1
        if n<2 then linear points else
        let solve project =
            let x=Array.map project points
            let a,b,r=Array.zeroCreate n,Array.zeroCreate n,Array.zeroCreate n
            b[0]<-2.;r[0]<-x[0]+2.*x[1]
            for i in 1..n-2 do a[i]<-1.;b[i]<-4.;r[i]<-4.*x[i]+2.*x[i+1]
            a[n-1]<-2.;b[n-1]<-7.;r[n-1]<-8.*x[n-1]+x[n]
            for i in 1..n-1 do
                let m=a[i]/b[i-1]
                b[i]<-b[i]-m;r[i]<-r[i]-m*r[i-1]
            a[n-1]<-r[n-1]/b[n-1]
            for i in n-2.. -1 ..0 do a[i]<-(r[i]-a[i+1])/b[i]
            b[n-1]<-(x[n]+a[n-1])/2.
            for i in 0..n-2 do b[i]<-2.*x[i+1]-a[i+1]
            a,b
        let ax,bx=solve(fun p -> p.X)
        let ay,by=solve(fun p -> p.Y)
        [yield MoveTo points[0]
         for i in 0..n-1 do yield CubicTo(p ax[i] ay[i],p bx[i] by[i],points[i+1])]

    let private monotone swap (points:Point array) =
        let reflect p=if swap then {X=p.Y;Y=p.X} else p
        let points=points |> Array.map reflect |> Array.toList |> List.fold (fun acc p -> if List.tryHead acc=Some p then acc else p::acc) [] |> List.rev |> List.toArray
        let n=points.Length
        if n<3 then linear (Array.map reflect points) else
        let slopes=Array.init (n-1) (fun i -> (points[i+1].Y-points[i].Y)/(points[i+1].X-points[i].X))
        let tangents=Array.zeroCreate n
        let sign x=if x<0. then -1. else 1.
        for i in 1..n-2 do
            let h0,h1=points[i].X-points[i-1].X,points[i+1].X-points[i].X
            let a,b=slopes[i-1],slopes[i]
            let average=(a*h1+b*h0)/(h0+h1)
            let tangent=(sign a+sign b)*min (abs a) (min (abs b) (abs average/2.))
            tangents[i]<-if Double.IsFinite tangent then tangent else 0.
        tangents[0]<-if Double.IsFinite slopes[0] then (3.*slopes[0]-tangents[1])/2. else tangents[1]
        tangents[n-1]<-if Double.IsFinite slopes[n-2] then (3.*slopes[n-2]-tangents[n-2])/2. else tangents[n-2]
        [yield MoveTo(reflect points[0])
         for i in 0..n-2 do
             let a,b=points[i],points[i+1]
             let dx=(b.X-a.X)/3.
             yield CubicTo(reflect(p (a.X+dx) (a.Y+dx*tangents[i])),reflect(p (b.X-dx) (b.Y-dx*tangents[i+1])),reflect b)]

    let private segment curve (points:Point array) =
        match curve with
        | CurveSpec.Linear -> linear points
        | CurveSpec.Natural -> natural points
        | CurveSpec.MonotoneX -> monotone false points
        | CurveSpec.MonotoneY -> monotone true points
        | CurveSpec.Basis boundary -> basis boundary points
        | CurveSpec.Cardinal(tension,boundary) ->
            Check.finite "tension" tension
            if tension<0. || tension>1. then invalidArg "tension" "Expected [0,1]."
            interpolating true boundary (cardinalControls tension) points
        | CurveSpec.CatmullRom(alpha,boundary) ->
            Check.finite "alpha" alpha
            if alpha<0. || alpha>1. then invalidArg "alpha" "Expected [0,1]."
            interpolating false boundary (catmullControls alpha) points
        | CurveSpec.Bundle beta ->
            Check.finite "beta" beta
            if beta<0. || beta>1. then invalidArg "beta" "Expected [0,1]."
            let n=points.Length
            if n<2 then [] else
            points |> Array.mapi (fun i point -> blend beta (blend (float i/float(n-1)) points[0] points[n-1]) point) |> basis CurveBoundary.Clamped
        | _ when points.Length<2 -> linear points
        | CurveSpec.BumpX | CurveSpec.BumpY ->
            [yield MoveTo points[0]
             for a,b in Array.pairwise points do
                 if curve=CurveSpec.BumpX then
                     let mid=Check.lerp a.X b.X 0.5
                     yield CubicTo(p mid a.Y,p mid b.Y,b)
                 else
                     let mid=Check.lerp a.Y b.Y 0.5
                     yield CubicTo(p a.X mid,p b.X mid,b)]
        | CurveSpec.StepBefore | CurveSpec.StepAfter | CurveSpec.StepMiddle ->
            let t=if curve=CurveSpec.StepBefore then 0. elif curve=CurveSpec.StepAfter then 1. else 0.5
            [yield MoveTo points[0]
             for a,b in Array.pairwise points do
                 let x=Check.lerp a.X b.X t
                 yield LineTo(p x a.Y)
                 yield LineTo(p x b.Y)
                 yield LineTo b]

    let private validate curve =
        match curve with
        | CurveSpec.Cardinal(value,_) | CurveSpec.CatmullRom(value,_) | CurveSpec.Bundle value ->
            if not(Double.IsFinite value) || value<0. || value>1. then invalidArg "curve" "Curve parameter must be in [0,1]."
        | _ -> ()
    /// <summary>Builds a path through the points, where each <c>None</c> breaks the line into separate subpaths.
    /// Throws <c>ArgumentException</c> when a curve parameter is outside [0,1].</summary>
    let line curve points =
        validate curve
        runs points |> List.collect (fun run -> segment curve (List.toArray run)) |> Path.ofCommands
    /// <summary>Builds a closed area per run of (lower, upper) point pairs: the curve along the upper points, then back along the lower points.
    /// Each <c>None</c> starts a new run; throws <c>ArgumentException</c> for an invalid curve parameter.</summary>
    let area curve points =
        validate curve
        let reverse=match curve with CurveSpec.StepBefore -> CurveSpec.StepAfter | CurveSpec.StepAfter -> CurveSpec.StepBefore | other -> other
        runs points |> List.collect (fun run ->
            let lo,hi=List.unzip run
            let top=segment curve (List.toArray hi) |> List.filter ((<>) Close)
            let bottom=segment reverse (lo |> List.rev |> List.toArray) |> List.filter ((<>) Close)
            match top,bottom with
            | [],_ | _,[] -> []
            | _,MoveTo first::rest -> top @ (LineTo first::rest) @ [Close]
            | _ -> invalidOp "Expected initial move.") |> Path.ofCommands

    /// <summary>Converts a polar coordinate to a point about the origin, with angle in radians clockwise from 12 o'clock.
    /// Throws <c>ArgumentException</c> for a non-finite angle or a negative or non-finite radius.</summary>
    let radialPoint angle radius =
        Check.finite "angle" angle
        Check.finite "radius" radius
        if radius<0. then invalidArg "radius" "Negative radius."
        p (radius*sin angle) (-radius*cos angle)
    /// <summary>Like <c>line</c> over (angle, radius) pairs converted with <c>radialPoint</c>; <c>None</c> breaks the line.</summary>
    let radialLine curve points = points |> List.map (Option.map (fun (angle,radius) -> radialPoint angle radius)) |> line curve
    /// <summary>Like <c>area</c> over (angle, inner radius, outer radius) triples converted with <c>radialPoint</c>;
    /// <c>None</c> breaks the area.</summary>
    let radialArea curve points = points |> List.map (Option.map (fun (angle,inner,outer) -> radialPoint angle inner,radialPoint angle outer)) |> area curve
    /// <summary>Builds a cubic link from <c>a</c> to <c>b</c> whose tangents are vertical at both ends.</summary>
    let linkVertical a b = line CurveSpec.BumpY [Some a;Some b]
    /// <summary>Builds a cubic link between two (angle, radius) polar points with control points at the mean radius, about the origin.</summary>
    let linkRadial (angleA,radiusA) (angleB,radiusB) =
        let middle=Check.lerp radiusA radiusB 0.5
        Path.ofCommands [MoveTo(radialPoint angleA radiusA);CubicTo(radialPoint angleA middle,radialPoint angleB middle,radialPoint angleB radiusB)]

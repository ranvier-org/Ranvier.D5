namespace Ranvier.D5

open System

/// <summary>One chord ribbon: source and target arcs and radii, PadAngle in radians trimmed from each arc, and an optional ArrowHead radial length
/// that replaces the target arc with an arrow tip.</summary>
type RibbonSpec =
    { Source:ChordArc; Target:ChordArc; SourceRadius:float; TargetRadius:float
      PadAngle:float; ArrowHead:float option }
/// <summary>RibbonSpec construction.</summary>
[<RequireQualifiedAccess>]
module RibbonSpec =
    /// <summary>Creates an unpadded ribbon without an arrow head for a chord flow, using one radius at both ends.</summary>
    let create radius (flow:ChordFlow) =
        {Source=flow.Source;Target=flow.Target;SourceRadius=radius;TargetRadius=radius;PadAngle=0.;ArrowHead=None}
/// <summary>SVG path generation for chord ribbons.</summary>
[<RequireQualifiedAccess>]
module Ribbon =
    /// <summary>Builds a closed ribbon path around the origin, with zero angle at twelve o'clock and angles increasing clockwise; zero radii give an
    /// empty path. Raises ArgumentException for negative or non-finite sizes, an arrow head longer than the target radius, or an arc that descends or
    /// exceeds one turn.</summary>
    let path (spec:RibbonSpec) =
        let tau=2.*System.Math.PI
        let nonnegative name value=
            Check.finite name value
            if value<0. then invalidArg name "Value must be nonnegative."
        nonnegative "SourceRadius" spec.SourceRadius
        nonnegative "TargetRadius" spec.TargetRadius
        nonnegative "PadAngle" spec.PadAngle
        spec.ArrowHead |> Option.iter(fun radius ->
            nonnegative "ArrowHead" radius
            if radius>spec.TargetRadius then invalidArg "ArrowHead" "Arrow head must fit the target radius.")
        let angles arc =
            Check.finite "StartAngle" arc.StartAngle
            Check.finite "EndAngle" arc.EndAngle
            let span=arc.EndAngle-arc.StartAngle
            if not(Double.IsFinite span) || span<0. || span>tau+1e-12 then invalidArg "arc" "Require an ascending arc of at most one turn."
            if span<=spec.PadAngle then
                let middle=arc.StartAngle+span/2.
                middle,middle
            else arc.StartAngle+spec.PadAngle/2.,arc.EndAngle-spec.PadAngle/2.
        let s0,s1=angles spec.Source
        let t0,t1=angles spec.Target
        if spec.SourceRadius=0. && spec.TargetRadius=0. then Path.empty else
        let inline at radius angle =
#if !FABLE_COMPILER
            let struct(s, c) = Math.SinCos angle
            Point.create (radius * s) (-radius * c)
#else
            Point.create (radius*sin angle) (-radius*cos angle)
#endif
        let inline prependArc radius first last tail =
            if first=last then tail
            elif radius=0. then LineTo(at radius last) :: tail
            elif last-first>=tau-1e-12 then
                ArcTo(radius,radius,0.,false,true,at radius (first+(last-first)/2.))
                :: ArcTo(radius,radius,0.,false,true,at radius last)
                :: tail
            else ArcTo(radius,radius,0.,last-first>System.Math.PI,true,at radius last) :: tail
        let origin = { Point.X = 0.; Y = 0. }
        let start = at spec.SourceRadius s0
        let closing = QuadraticTo(origin, start) :: [ Close ]
        let afterTarget =
            if s0<>t0 || s1<>t1 || spec.SourceRadius<>spec.TargetRadius then
                match spec.ArrowHead with
                | Some head ->
                    let inner = spec.TargetRadius - head
                    QuadraticTo(origin, at inner t0)
                    :: LineTo(at spec.TargetRadius (t0 + (t1 - t0) / 2.))
                    :: LineTo(at inner t1)
                    :: closing
                | None ->
                    QuadraticTo(origin, at spec.TargetRadius t0)
                    :: prependArc spec.TargetRadius t0 t1 closing
            else closing
        let cmds = MoveTo start :: prependArc spec.SourceRadius s0 s1 afterTarget
        Path.ofCommands cmds

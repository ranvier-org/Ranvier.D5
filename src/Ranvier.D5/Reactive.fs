namespace Ranvier.D5

open System.Collections.Generic
open Ranvier

/// <summary>An immutable animation segment evaluated against an injected millisecond clock.</summary>
type Transition<'T> = private {
    Started: float
    Duration: float
    Ease: float -> float
    Interpolate: 'T -> 'T -> float -> 'T
    From: 'T
    Target: 'T
}

[<RequireQualifiedAccess>]
module Transition =
    /// <summary>Starts a segment using an Anime easing and an application-specific interpolator.</summary>
    let start now duration ease interpolate source target =
        Check.finite "now" now
        Check.finite "duration" duration
        if duration < 0. then invalidArg "duration" "Duration must be nonnegative."
        { Started = now; Duration = duration; Ease = ease; Interpolate = interpolate; From = source; Target = target }

    /// <summary>Samples a segment, preserving exact endpoints and allowing easing overshoot between them.</summary>
    let sample now segment =
        Check.finite "now" now
        if now < segment.Started then segment.From
        elif segment.Duration = 0. || now - segment.Started >= segment.Duration then segment.Target
        elif now = segment.Started then segment.From
        else
            let amount = segment.Ease ((now - segment.Started) / segment.Duration)
            Check.finite "ease" amount
            segment.Interpolate segment.From segment.Target amount

    /// <summary>Redirects a segment from its value at the supplied time, retaining duration and easing.</summary>
    let retarget now target segment =
        start now segment.Duration segment.Ease segment.Interpolate (sample now segment) target

[<RequireQualifiedAccess>]
module Reactive =
    /// <summary>Derives an owned Ranvier memo from reactive inputs in the current graph.</summary>
    let derive read project = Api.createMemo (fun _ -> project (read ()))

    /// <summary>Animates target changes using a caller-owned clock signal, with no private timer or scheduler.</summary>
    /// <remarks>Read on each frame to observe target changes at that frame. Disposing the enclosing Ranvier owner releases the memos.</remarks>
    let animate clock target duration ease interpolate =
        Check.finite "duration" duration
        if duration < 0. then invalidArg "duration" "Duration must be nonnegative."
        let state = Api.createMemo (fun previous ->
            let now, destination = clock (), target ()
            Check.finite "clock" now
            let segment =
                match previous with
                | ValueNone -> Transition.start now duration ease interpolate destination destination
                | ValueSome (old, _) when EqualityComparer<_>.Default.Equals(old.Target, destination) -> old
                | ValueSome (old, _) -> Transition.retarget now destination old
            segment, Transition.sample now segment)
        Api.createMemo (fun _ -> snd state.Value)

/// <summary>A uniform scale and translation for reversible pan and zoom.</summary>
type Zoom = private { Scale: float; X: float; Y: float }

[<RequireQualifiedAccess>]
module Zoom =
    /// <summary>The unchanged viewport.</summary>
    let identity = { Scale = 1.; X = 0.; Y = 0. }

    /// <summary>Creates a finite viewport with a strictly positive scale.</summary>
    let create scale x y =
        Check.positive "scale" scale
        Check.finite "x" x
        Check.finite "y" y
        { Scale = scale; X = x; Y = y }

    /// <summary>Maps a data-space point into viewport coordinates.</summary>
    let apply (point: Point) zoom = Point.create (point.X * zoom.Scale + zoom.X) (point.Y * zoom.Scale + zoom.Y)

    /// <summary>Maps a viewport point back into data coordinates.</summary>
    let invert (point: Point) zoom = Point.create ((point.X - zoom.X) / zoom.Scale) ((point.Y - zoom.Y) / zoom.Scale)

    /// <summary>Translates the viewport by a screen-space displacement.</summary>
    let pan dx dy zoom = create zoom.Scale (zoom.X + dx) (zoom.Y + dy)

    /// <summary>Multiplies the scale while keeping the data point under the screen-space anchor fixed.</summary>
    let zoomAt anchor factor zoom =
        Check.positive "factor" factor
        let point = invert anchor zoom
        let scale = zoom.Scale * factor
        create scale (anchor.X - point.X * scale) (anchor.Y - point.Y * scale)

    /// <summary>Converts the viewport into an affine scene transform.</summary>
    let transform zoom = Transform.create zoom.Scale 0. 0. zoom.Scale zoom.X zoom.Y

[<RequireQualifiedAccess>]
module Brush =
    /// <summary>Normalizes a rectangular selection drawn in either direction.</summary>
    let between (first: Point) (last: Point) =
        Bounds.create (min first.X last.X) (min first.Y last.Y) (abs (last.X - first.X)) (abs (last.Y - first.Y))

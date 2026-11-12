namespace Ranvier.D5.Geo

open System
open System.Threading
open Ranvier.D5

/// <summary>A nonempty ordered rectangle in canonical degrees: longitude [-180,180], latitude [-90,90].</summary>
type GeoExtent = { West: float; South: float; East: float; North: float }

/// <summary>Major/minor grid spacing and bounded sampling, all angles in degrees.</summary>
type GraticuleConfig =
    { MajorExtent: GeoExtent; MinorExtent: GeoExtent
      MajorLongitudeStep: float; MajorLatitudeStep: float
      MinorLongitudeStep: float; MinorLatitudeStep: float
      Precision: float; MaxPoints: int; MaxWork: int64 }

/// <summary>Conventional ten-degree grid with ninety-degree major meridians.</summary>
module GraticuleConfig =
    /// <summary>Ten-degree minor lines to ±80 degrees, ninety-degree major meridians to the poles, 2.5-degree sampling, one million points and two million work units.</summary>
    let defaultValue =
        { MajorExtent={West = -180.; South = -89.999999; East=180.; North=89.999999}
          MinorExtent={West = -180.; South = -80.000001; East=180.; North=80.000001}
          MajorLongitudeStep=90.; MajorLatitudeStep=360.
          MinorLongitudeStep=10.; MinorLatitudeStep=10.
          Precision=2.5; MaxPoints=1_000_000; MaxWork=2_000_000L }

/// <summary>Immutable grid lines and the closed clockwise outline of the major extent.</summary>
type Graticule = { Lines: GeoPosition list list; Outline: GeoPosition list }

/// <summary>Pure graticule generation; parallels use precision in (0,90] degrees, meridians use ninety-degree segments.</summary>
module Graticule =
    exception private Rejected of LayoutError
    /// <summary>Generates a bounded grid and outline, observing cancellation during generation.</summary>
    let createWithCancellation (config: GraticuleConfig) (token: CancellationToken) : Result<Graticule,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let validExtent e =
            [e.West;e.South;e.East;e.North] |> List.forall Double.IsFinite
            && e.West >= -180. && e.East <= 180. && e.West < e.East
            && e.South >= -90. && e.North <= 90. && e.South < e.North
        let validStep x = Double.IsFinite x && x > 0.
        if not(validExtent config.MajorExtent && validExtent config.MinorExtent)
           || not([config.MajorLongitudeStep;config.MajorLatitudeStep;config.MinorLongitudeStep;config.MinorLatitudeStep;config.Precision] |> List.forall validStep)
           || config.Precision > 90. || config.MaxPoints < 1 || config.MaxWork < 1L then
            Error [InvalidObservation "Graticules require ordered canonical extents, positive finite steps, precision in (0,90], and positive budgets."]
        else
            try
                let mutable work=0L
                let mutable points=0
                let visit () =
                    token.ThrowIfCancellationRequested()
                    if work >= config.MaxWork then raise(Rejected(BudgetExceeded "Graticule work budget exceeded."))
                    work <- work+1L
                let count (lo: float) (hi: float) (step: float) =
                    let n=max 0. (Math.Ceiling((hi-lo)/step))
                    if not(Double.IsFinite n) || n > float (min (int64 (Int32.MaxValue-1)) (config.MaxWork-work)) then
                        raise(Rejected(BudgetExceeded "Graticule sampling exceeds its work budget."))
                    int n
                let point longitude latitude =
                    visit()
                    if points >= config.MaxPoints then raise(Rejected(BudgetExceeded "Graticule point budget exceeded."))
                    points <- points+1
                    match GeoPosition.create longitude latitude with
                    | Ok p -> p
                    | Error e -> raise(Rejected e.Head)
                let sample lo hi step reverse skipFirst make =
                    let n=count lo hi step
                    if n+1-(if skipFirst then 1 else 0) > config.MaxPoints-points then
                        raise(Rejected(BudgetExceeded "Graticule line exceeds its point budget."))
                    [ for index in (if skipFirst then 1 else 0)..n do
                        let i=if reverse then n-index else index
                        let value=if i=n then hi else min hi (lo+float i*step)
                        yield make value ]
                let meridian e longitude reverse skip =
                    sample e.South e.North 90. reverse skip (point longitude)
                let latitudeLine e latitude reverse skip =
                    sample e.West e.East config.Precision reverse skip (fun longitude -> point longitude latitude)
                let lines=ResizeArray<GeoPosition list>()
                let grid (lo: float) (hi: float) (step: float) includeLine make =
                    let first=Math.Ceiling(lo/step)*step
                    let n=count first hi step
                    for i in 0..n-1 do
                        visit()
                        let value=first+float i*step
                        if value < hi && includeLine value then lines.Add(make value)
                let major=config.MajorExtent
                let minor=config.MinorExtent
                grid major.West major.East config.MajorLongitudeStep (fun _ -> true) (fun x -> meridian major x false false)
                grid major.South major.North config.MajorLatitudeStep (fun _ -> true) (fun y -> latitudeLine major y false false)
                let notMajor step value =
                    let remainder=abs(value % step)
                    let tolerance=min 1e-6 (step*1e-10)
                    remainder > tolerance && abs(step-remainder) > tolerance
                grid minor.West minor.East config.MinorLongitudeStep (notMajor config.MajorLongitudeStep) (fun x -> meridian minor x false false)
                grid minor.South minor.North config.MinorLatitudeStep (notMajor config.MajorLatitudeStep) (fun y -> latitudeLine minor y false false)
                let outline =
                    [ yield! meridian major major.West false false
                      yield! latitudeLine major major.North false true
                      yield! meridian major major.East true true
                      yield! latitudeLine major major.South true true ]
                Ok {Lines=List.ofSeq lines;Outline=outline}
            with Rejected error -> Error [error]

    /// <summary>Generates a bounded immutable grid and outline without cancellation.</summary>
    let create config = createWithCancellation config CancellationToken.None

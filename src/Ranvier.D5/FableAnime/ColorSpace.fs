// Copied from Ranvier.Anime 0.1.0-rc.2 (src/Ranvier.Anime/ColorSpace.fs): that package ships no F# sources for Fable.
// Compiled only under Fable; .NET builds reference the package.
namespace Ranvier.Anime

open System
// Not in the original: brings D5's Fable stand-ins (Double.IsFinite) into scope.
open Ranvier.D5

/// <summary>Normalized sRGB channels and straight alpha. Conversion may preserve out-of-gamut channels.</summary>
type Rgba =
    {
        Red: float
        Green: float
        Blue: float
        Alpha: float
    }

/// <summary>CIE Lab using the D50 reference white.</summary>
type LabColor =
    {
        Lightness: float
        A: float
        B: float
        Alpha: float
    }

/// <summary>Polar D50 Lab. Hue is degrees; achromatic colors use zero hue and zero chroma.</summary>
type HclColor =
    {
        Hue: float
        Chroma: float
        Lightness: float
        Alpha: float
    }

[<RequireQualifiedAccess>]
type ColorInterpolation =
    | Srgb
    | Lab
    | Hcl

[<RequireQualifiedAccess>]
type GamutPolicy =
    | Preserve
    | Clip
    | Reject

/// <summary>Pure color conversion and interpolation without CSS parsing or a scheduler. D50 matrices follow d3-color; see THIRD-PARTY-NOTICES.md.</summary>
[<RequireQualifiedAccess>]
module ColorSpace =
    let private finite values =
        if values |> List.exists (Double.IsFinite >> not) then
            invalidArg "color" "Color components must be finite."

    let private alpha value =
        if
            not (Double.IsFinite value)
            || value < 0.
            || value > 1.
        then
            invalidArg "alpha" "Alpha must be in [0,1]."

    let rgba red green blue opacity =
        finite [ red; green; blue ]
        alpha opacity

        if List.exists (fun x -> x < 0. || x > 1.) [ red; green; blue ] then
            invalidArg "channels" "Expected normalized sRGB in [0,1]."

        {
            Red = red
            Green = green
            Blue = blue
            Alpha = opacity
        }

    let applyGamut policy (color: Rgba) =
        finite [ color.Red; color.Green; color.Blue ]
        alpha color.Alpha

        let clip x =
            Math.Clamp (x, 0., 1.)

        match policy with
        | GamutPolicy.Preserve -> color
        | GamutPolicy.Clip ->
            { color with
                Red = clip color.Red
                Green = clip color.Green
                Blue = clip color.Blue
            }
        | GamutPolicy.Reject -> rgba color.Red color.Green color.Blue color.Alpha

    let toLab (color: Rgba) =
        applyGamut GamutPolicy.Preserve color |> ignore

        let linear x =
            if x <= 0.04045 then
                x / 12.92
            else
                Math.Pow ((x + 0.055) / 1.055, 2.4)

        let f x =
            if x > 216. / 24389. then
                Math.Pow (x, 1. / 3.)
            else
                x / (108. / 841.) + 4. / 29.

        let r, g, b = linear color.Red, linear color.Green, linear color.Blue
        let y = f (0.2225045 * r + 0.7168786 * g + 0.0606169 * b)

        let x, z =
            if r = g && g = b then
                y, y
            else
                f (
                    (0.4360747 * r + 0.3850649 * g + 0.1430804 * b)
                    / 0.96422
                ),
                f (
                    (0.0139322 * r + 0.0971045 * g + 0.7141733 * b)
                    / 0.82521
                )

        {
            Lightness = 116. * y - 16.
            A = 500. * (x - y)
            B = 200. * (y - z)
            Alpha = color.Alpha
        }

    let fromLab (color: LabColor) =
        finite [ color.Lightness; color.A; color.B ]
        alpha color.Alpha

        let inverse t =
            if t > 6. / 29. then
                t * t * t
            else
                (108. / 841.) * (t - 4. / 29.)

        let gamma x =
            if x <= 0.0031308 then
                12.92 * x
            else
                1.055 * Math.Pow (x, 1. / 2.4) - 0.055

        let y = (color.Lightness + 16.) / 116.

        let x, z =
            0.96422 * inverse (y + color.A / 500.), 0.82521 * inverse (y - color.B / 200.)

        let y = inverse y

        {
            Red = gamma (3.1338561 * x - 1.6168667 * y - 0.4906146 * z)
            Green = gamma (-0.9787684 * x + 1.9161415 * y + 0.0334540 * z)
            Blue = gamma (0.0719453 * x - 0.2289914 * y + 1.4052427 * z)
            Alpha = color.Alpha
        }
        |> applyGamut GamutPolicy.Preserve

    let toHcl color =
        let lab = toLab color
        let chroma = sqrt (lab.A * lab.A + lab.B * lab.B)

        let hue =
            if chroma < 1e-7 then
                0.
            else
                (atan2 lab.B lab.A * 180. / Math.PI + 360.) % 360.

        {
            Hue = hue
            Chroma = (if chroma < 1e-7 then 0. else chroma)
            Lightness = lab.Lightness
            Alpha = lab.Alpha
        }

    let fromHcl (color: HclColor) =
        finite [ color.Hue; color.Chroma; color.Lightness ]

        if color.Chroma < 0. then
            invalidArg "color" "Negative chroma."

        let angle = (color.Hue % 360.) * Math.PI / 180.

        fromLab
            {
                Lightness = color.Lightness
                A = cos angle * color.Chroma
                B = sin angle * color.Chroma
                Alpha = color.Alpha
            }

    /// <summary>Interpolates straight alpha and color at progress in [0,1]. HCL takes the shortest hue path and borrows hue from a chromatic endpoint.</summary>
    let interpolate space gamut (first: Rgba) (last: Rgba) progress =
        if
            not (Double.IsFinite progress)
            || progress < 0.
            || progress > 1.
        then
            invalidArg "progress" "Expected progress in [0,1]."

        applyGamut GamutPolicy.Preserve first |> ignore
        applyGamut GamutPolicy.Preserve last |> ignore

        let lerp a b =
            let result = Utils.lerp a b progress

            if Double.IsFinite result then
                result
            else
                a * (1. - progress) + b * progress

        let opacity = lerp first.Alpha last.Alpha

        let result =
            match space with
            | ColorInterpolation.Srgb ->
                {
                    Red = lerp first.Red last.Red
                    Green = lerp first.Green last.Green
                    Blue = lerp first.Blue last.Blue
                    Alpha = opacity
                }
            | ColorInterpolation.Lab ->
                let a, b = toLab first, toLab last

                fromLab
                    {
                        Lightness = lerp a.Lightness b.Lightness
                        A = lerp a.A b.A
                        B = lerp a.B b.B
                        Alpha = opacity
                    }
            | ColorInterpolation.Hcl ->
                let a, b = toHcl first, toHcl last
                let h0 = if a.Chroma = 0. then b.Hue else a.Hue
                let h1 = if b.Chroma = 0. then h0 else b.Hue
                let delta = (h1 - h0 + 540.) % 360. - 180.

                fromHcl
                    {
                        Hue = h0 + progress * delta
                        Chroma = lerp a.Chroma b.Chroma
                        Lightness = lerp a.Lightness b.Lightness
                        Alpha = opacity
                    }

        applyGamut
            gamut
            (if progress = 0. then first
             elif progress = 1. then last
             else result)

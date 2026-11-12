namespace Ranvier.D5

open System
open System.Globalization
open Ranvier.Anime

/// <summary>Palette values and scale policy; color conversion and interpolation are provided by Anime.</summary>
[<RequireQualifiedAccess>]
module Chromatic =
    let private rgb value =
        ColorSpace.rgba (float ((value >>> 16) &&& 255)/255.) (float ((value >>> 8) &&& 255)/255.) (float (value &&& 255)/255.) 1.

    /// <summary>D3 Category10 categorical values. See the packaged d3-scale-chromatic license.</summary>
    let category10 = [0x1f77b4;0xff7f0e;0x2ca02c;0xd62728;0x9467bd;0x8c564b;0xe377c2;0x7f7f7f;0xbcbd22;0x17becf] |> List.map rgb
    /// <summary>ColorBrewer Blues nine-color sequential palette, distributed by d3-scale-chromatic.</summary>
    let blues = [0xf7fbff;0xdeebf7;0xc6dbef;0x9ecae1;0x6baed6;0x4292c6;0x2171b5;0x08519c;0x08306b] |> List.map rgb
    /// <summary>ColorBrewer RdBu eleven-color diverging palette, distributed by d3-scale-chromatic.</summary>
    let redBlue = [0x67001f;0xb2182b;0xd6604d;0xf4a582;0xfddbc7;0xf7f7f7;0xd1e5f0;0x92c5de;0x4393c3;0x2166ac;0x053061] |> List.map rgb

    let private colors palette =
        if List.isEmpty palette then invalidArg "palette" "At least one color is required."
        palette |> List.map (ColorSpace.applyGamut GamutPolicy.Preserve) |> List.toArray

    /// <summary>Wraps integer categories, including negative indices, over an immutable palette.</summary>
    let categorical palette =
        let values=colors palette
        fun index -> values[((index % values.Length)+values.Length)%values.Length]

    /// <summary>Clamps progress to [0,1] and interpolates adjacent palette stops in the requested color space.</summary>
    let sequential space gamut palette =
        let values=colors palette |> Array.map (ColorSpace.applyGamut gamut)
        fun progress ->
            Check.finite "progress" progress
            let position=Math.Clamp(progress,0.,1.)*float(values.Length-1)
            let index=min (values.Length-1) (int position)
            if index=values.Length-1 then values[index]
            else ColorSpace.interpolate space gamut values[index] values[index+1] (position-float index)

    /// <summary>Uses an odd stop count so progress 0.5 maps exactly to the central neutral color.</summary>
    let diverging space gamut palette =
        if List.length palette<3 || List.length palette%2=0 then invalidArg "palette" "Diverging palettes require an odd number of at least three stops."
        sequential space gamut palette

    /// <summary>Wraps finite progress around the palette, interpolating the final stop back to the first.</summary>
    let cyclic space gamut palette =
        let values=colors palette |> Array.map (ColorSpace.applyGamut gamut)
        fun progress ->
            Check.finite "progress" progress
            let position=(progress-floor progress)*float values.Length
            let index=min (values.Length-1) (int position)
            ColorSpace.interpolate space gamut values[index] values[(index+1)%values.Length] (position-float index)

    /// <summary>Serializes straight-alpha sRGB through Ranvier.Css. Preserve retains out-of-gamut CSS channels.</summary>
    let paint gamut color =
        let color=ColorSpace.applyGamut gamut color
        let number (value:float)=value.ToString("R",CultureInfo.InvariantCulture)
        let css="rgba("+number(color.Red*255.)+","+number(color.Green*255.)+","+number(color.Blue*255.)+","+number color.Alpha+")"
        Ranvier.Css.Value.unsafeFromTokens<unit> css |> Paint.ofCss

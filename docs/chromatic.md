# Palette policy

D5 requires Ranvier.Anime 0.1.0 or later for public sRGB, D50 Lab and HCL conversion.
The upstream normal/Traced packages passed the six color-contract jobs and full
Anime CI before publication. The original source snapshot remains recorded in
kit.lock.json; packageCommit identifies the deliberate compatible upgrade.

Chromatic exposes Category10 and ColorBrewer Blues/RdBu values from
d3-scale-chromatic 3.1.0 (its license is included in the package). Sequential and
diverging functions interpolate piecewise between these stops; they do not claim
D3's RGB-basis spline interpolation. Cyclic accepts caller-supplied stops and
wraps the final stop back to the first. Category10 demonstrates the API but is
categorical, not a perceptually uniform cyclic scale.

Choose Anime's Srgb, Lab or Hcl interpolation and Preserve, Clip or Reject gamut
policy explicitly. No CSS strings are parsed, no easing/color engine is copied,
and alpha remains straight. Palette construction validates values once; sampling
rejects nonfinite progress. Serialize with Chromatic.paint through Ranvier.Css.

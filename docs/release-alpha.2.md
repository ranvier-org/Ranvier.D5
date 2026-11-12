# Alpha.2 validation and scope

Tasks 0–4 of the D3 expansion plan deliver three packages: core, Charts and View,
each normal and Traced. Anime rc.2 supplies reusable public sRGB/D50 Lab/HCL
conversion; other toolkit dependency versions remain pinned.

Implemented batches: coverage/provenance; composed retained scenes and layout
contracts; statistics; scale/time/format; curves/shapes/stacks; palettes;
Cartesian charts; distribution charts; radial/legend/facet/calendar composition;
owned gestures and selection; interruptible keyed transitions; bounded workers.

The final review found and regression-tested small-domain tick identity,
stacked-area missingness, clipped picking, scoped drag exit identity, and narrow
log ticks. Native linked-chart testing additionally caught unchanged-selection
feedback. Axis label anchors and explicit fill replacement are covered.

Run the linked example with:

```sh
dotnet run --project examples/Ranvier.D5.Gallery -c Release -- --linked
```

Drag a rectangle in the upper scatter plot; the lower line follows its shared
Ranvier selection signal. Keyboard arrows/Enter select points; Escape clears.
The native suite checks shared selection, node-exit cancellation, real pointer
capture/release, disposal, and off-thread kernel publication through host frames.

## Performance gate

`python tools/compare-benchmarks.py --traced false` runs three fresh-process
baseline/current pairs on the same machine. Baseline is 52ef102; its harness alone
is changed from 15 to 60 samples to match current sampling. Pair order alternates.
CI runs this for all Windows/Linux/macOS normal/Traced jobs, alongside absolute
guards for new workloads. Evidence is retained as JSON and logs.

A failure requires median runtime growth above both 20% and 2 ms, or managed
allocation growth above both 10% and 64 KiB, using medians across the three
processes. No automatic baseline reset is permitted.

Initial matched Windows normal evidence: native 500-mark update/paint
5.923 → 7.112 ms, 1,788,632 → 1,918,600 bytes (+7.3%); pure established cases
retain identical allocations. These are machine-specific observations, not
cross-platform frame-rate promises. New-case samples measured approximately
0.46 ms for 500 transition marks and 0.23 ms for a 1,000-request worker burst.
Use each CI artifact's comparison.json for that runner's matched results.

## Deliberate policies

- Pure chart functions return typed errors; invalid numeric geometry is rejected.
  Missing stacked observations create geometry gaps; zero placeholders serve
  stack baseline calculation only, never visible observations.
- Palette values include Category10 and ColorBrewer Blues/RdBu with provenance.
  Stop interpolation is piecewise Anime interpolation, not D3's RGB-basis spline.
  Cyclic ramps accept supplied stops; no full palette-catalog claim.
- Arc padding/corner policy is documented in the shape contracts; D3 padRadius
  semantics are not claimed.
- Transition easing is clamped to valid scene geometry; affine coefficients
  interpolate directly. At most eight crossfade layers per domain key survive
  rapid incompatible retargeting, dropping faint old layers first.
- Brush/lasso use representative hit centers within each target's plot clip.
  Drag targets carry full scoped paths. Applications decide how zoom commands
  change scales or group transforms.
- Background inputs must be immutable/owned snapshots. Kernels check cancellation
  cooperatively. One running plus one latest pending request is enforced;
  signals/native objects remain on the host thread.
- Text measurement uses a bounded owner-thread cache with explicit font-generation
  and device-scale keys. Label placement is bounded, not a global optimum solver.

See interaction.md, transitions.md, background-layout.md and chromatic.md for API
ownership details. Tasks 5–11 remain future work; this release does not claim all
D3 algorithms or million-mark native SVG responsiveness.

# Foundation contracts

The core is immutable data-to-geometry code. It does not own a DOM, timer, thread, event loop or reactive runtime. Temporary arrays, dictionaries and loops are confined to algorithm implementations. Public scenes, marks, layouts and transitions are persistent values.

## Data and scales

Missing observations are explicit options. Statistical summaries ignore NaN and infinities; geometry, domains and layout weights reject them. Keyed joins reject duplicate keys in either input and retain input order. Bins are left-inclusive/right-exclusive except for the inclusive final upper edge. Quantiles use linear interpolation between sorted observations.

Continuous scales support reversed domains and ranges. Constant domains map to the range midpoint; constant ranges invert to the domain midpoint. `tryMap` returns None for observations outside a transform's valid mathematical domain or a nonfinite result; `map` rejects these observations. Clamping is opt-in. Log domains must have a single nonzero sign. Numeric ticks use 1/2/5 steps, including on transformed scales; specialized logarithmic tick selection is not provided. `nice` preserves a valid transform domain if rounding an endpoint would leave it. Time scales use UTC Unix milliseconds and explicit calendar tick intervals.

Categorical scales have a fixed domain. Unknown categories return None; new data cannot silently grow the scale. Threshold boundaries must strictly increase; empirical quantile boundaries may repeat. Band ranges can reverse while bandwidth remains nonnegative.

## Geometry and layouts

Coordinates use an SVG-compatible, downward-positive Y axis. Arc angles are radians, zero at twelve o'clock, positive clockwise. A span of at least one full turn produces a complete circle/annulus. SVG serialization is culture-independent. Missing points split line/area runs; they are never bridged. Catmull–Rom is the uniform cardinal form with configurable tension, not a centripetal spline. Paths validate finite controls and arc radii.

Pie layout preserves source order and supports zero weights. Stack separates positive and negative totals. Hierarchy IDs must be globally unique. The tree uses leaf spacing and mean child positions; the treemap is alternating slice-and-dice, not squarified. These layouts produce data/geometry, not renderer-specific objects. The immutable spatial index is a balanced k-d tree with exact nearest and inclusive rectangle queries; nearest ties prefer the earliest original item. Distances outside finite double representation are rejected.

## Signals, animation and interaction

Create reactive values inside a Ranvier graph/owner. `Reactive.derive` returns an ordinary Ranvier memo; writes, batching, dependency tracking and disposal retain their existing semantics. `Reactive.animate` reads a caller-supplied clock and target. It is lazy: observe it each frame to record target changes at that frame. It owns no scheduler. Retargeting starts from the prior segment sampled at the current clock. Duration is milliseconds, finite and nonnegative. A zero duration completes immediately; times before a segment start return its source. Anime easing may overshoot between exact endpoints. Use `Transition` directly for pure, replayable animation state.

Zoom and brush are pure coordinate operations. Convert device/client coordinates into scene coordinates before applying them. Pointer capture, drag gesture policy, keyboard commands and focus navigation remain application responsibilities; View supplies native events and lifetime management.

## Native rendering and limits

`Renderer.create` constructs a reusable View component. Its prop is `unit -> Scene`, usually reading a signal or memo. Each mark key owns a retained SVG group; reordering keeps that identity. Both path and text slots exist under a group so a geometry-kind change does not replace the keyed group. View owns keyed reconciliation, attribute updates and event cleanup. Mark labels and text go through escaped attribute/text APIs, never raw markup. Dispose the mount before disposing its host.

The current backend is native WebCore SVG. This is not a GPU instancing renderer and it does not virtualize millions of visible marks. Aggregate, downsample or window large inputs before constructing the scene. Hit testing uses native SVG events or the core spatial index. D5 does not move scene construction or native host calls to arbitrary threads. Pure layouts can run on a worker, with publication handed back to the graph/host thread by the application.

The foundation supports composing Cartesian, radial, hierarchical and linked interactive views. It is not full D3 API parity: geographic projections/clipping, contours, triangulation, force simulation, Sankey/chord layouts, rich legend policies and specialized chart types remain extensions. These can reuse the scales, paths, marks, spatial queries and reactive renderer without adding another rendering/runtime stack.

## Performance gates

The benchmark executable warms each operation three times and records fifteen samples, median, nearest-rank p95 (the maximum for 15 samples), and managed bytes allocated on the calling thread. Native allocations/GPU memory are not counted. CI gates p95 and median managed allocation with generous shared-runner headroom; these gates detect large regressions, not a guarantee of a 16 ms frame on all hardware. The native case includes scene construction, signal publication, keyed updates, layout and CPU paint/copy for 500 marks at 800×600.

`windows-baseline.json` records one local Release run. CI artifacts contain per-platform/variant measurements and native gallery captures. Compare matching platforms and variants; Traced adds diagnostic work. Absolute thresholds are visible in the benchmark source and failure exits are mandatory in CI.

# D3 coverage and toolkit ownership

Baseline: alpha.1 / 52ef102. This ledger separates algorithms from native infrastructure. Tasks refer to `superpowers/plans/2026-11-12-d3-expansion.md`. Expansion status below tracks development beyond that baseline; release notes identify shipped revisions.

| D3 module | Baseline and owner | Expansion |
|---|---|---|
| array | Partial: Data summaries, grouping, bins, joins; ordinary collections use F# Seq/Map/Set | Task 2 statistics, cumulative summaries, bisectors and bin rules |
| axis | Partial: numeric/category marks, positional keys | Task 1 semantic keys, endpoints and label layout |
| brush | Coordinate rectangle only | Task 4 owned gestures/selection |
| chord | Absent | Task 8 |
| color | CSS paint strings; Anime internal color decomposition | Task 2 shared public color math upstream, not a second CSS parser |
| contour | Absent | Task 9 |
| delaunay | Absent; k-d lookup already present | Task 6 |
| dispatch | Native View events/Ranvier effects replace infrastructure | Task 4 typed input messages; no global dispatcher |
| drag | Native pointer events available | Task 4 |
| dsv | Absent | Task 11 optional adapter |
| ease | Ranvier.Anime.Ease | Reuse; no new easing implementation |
| fetch | .NET/toolkit async IO | Reuse; no network engine |
| force | Absent | Task 7 |
| format | Caller-provided labels | Task 2 explicit culture/SI/percent/time formatting |
| geo | Optional immutable coordinates, GeoJSON, bounded streams, graticules, spherical distance/interpolation/rotation and measurements | Task 10 J1 kernels and J2 bounded [projection paths](geographic-projections.md) (identity, equirectangular, Mercator, orthographic; preclipping, resampling, rejoining, viewport clipping) and J3 azimuthal, gnomonic, stereographic, conic, transverse Mercator, fitting and Albers USA, and J4 [map charts](map-charts.md) (choropleth, bubbles, routes, overlays, reactive globe) implemented; TopoJSON remains a separate adapter |
| hierarchy | Parent-table/forest validation; iterative tidy tree, cluster, partition, packing and five treemap modes; caller-owned resquarify; six chart recipes with signal-driven collapse/focus/selection | Task 5 delivered; packing uses conservative enclosure, not exact D3 coordinates |
| interpolate | Anime scalar/color facilities; pure D5 Transition | Task 2 public reusable color interpolation, task 4 topology-aware scene transition |
| path | Immutable SVG commands | Task 2 shapes use existing paths |
| polygon | Absent | Task 6 |
| quadtree | Absent; immutable k-d tree is a different algorithm | Task 6 |
| random | Anime seeded closure available | Task 11 [explicit-state distributions](random.md) implemented: immutable xoshiro256** state and the d3-random distribution set |
| scale | Partial continuous/categorical/threshold/quantile/UTC | Task 2 ticks, piecewise, inversion extents |
| scale-chromatic | Absent beyond user colors | Task 2 palettes with upstream attribution |
| selection | View keyed regions plus immutable Scene/Data.join | Reuse; task 1 groups/definitions, no mutable selection DSL |
| shape | Partial paths/line/area/arc/link/symbol/pie/stack | Task 2 extended curves, symbols, arc policies, orders/offsets |
| time | Explicit basic UTC calendar ticks | Task 2 interval selection/multiples and explicit zones |
| time-format | Caller formatting | Task 2 explicit formatting; full parser utility later task 11 |
| timer | Host frame clock/Anime | Reuse; no private timer |
| transition | Pure sampled/retargetable D5 transitions | Task 4 owned keyed lifecycle |
| zoom | Pure viewport math | Task 4 input adapter, extent clamp, capture, pinch |

Sankey and hexbin are ecosystem extensions (tasks 8/9), not core-module parity. Charts are pure compositions over these modules, not a separate rendering engine.

## Compatibility audit

Current package cohort: Ranvier 0.1.1-preview.2, Anime 0.1.0-rc.2, CSS 0.1.1-alpha.2, View 0.1.49-rc.3, Dom 0.1.0-rc.1, View.WebCore 0.1.0-rc.8, WebCore managed/native 0.1.1-rc.8 and SDL 0.1.0-rc.12. Anime supplies public perceptual interpolation and retains exact core/CSS dependencies; the newer View and native hosts remain compatible with them. Source snapshot revisions are reference material, not an API compatibility guarantee.

| Need | Audited symbol/source | Decision |
|---|---|---|
| Signal/memo/effect/cleanup | Ranvier.Api.createSignal/createMemo/createEffect/createRoot/onCleanup, Graph.Run, Owner.Dispose; pinned assembly/XML | Use existing graph ownership; worker result publication must return to owner thread |
| Numeric interpolation/easing | Anime.Utils.lerp, Ease.named/create, Spring.Ease/SettlingDuration; pinned assembly/XML and existing compiled D5 tests | Reuse directly |
| Seeded randomness | Anime.Utils.createSeededRandom; pinned XML | Stateful seeded closure, not an immutable RNG state; no randomized kernels in tasks 0–4 |
| General color conversion | Anime.ValueInterpolation is explicitly `module internal` in pinned source; XML includes internals and is not proof of visibility | Add a reusable public Anime color module/package if required; never call internals or parse arbitrary CSS again in D5 |
| Typed CSS paint | Css.Value.render and Value.unsafeFromTokens; existing compiled Paint.ofCss | Keep serialization here; transparent/gradient paints use explicit typed definitions |
| Keyed DOM updates | View.TemplateBuilder.each, Component.define, Attrs.attribute, Events.on; existing renderer/native tests | Extend retained templates, preserve disposal |
| Pointer capture | Dom.Element.setPointerCapture/releasePointerCapture; pinned contract | Native acceptance must verify actual delivery/cancellation, not just interface presence |
| Measurement | Dom.CanvasRenderingContext2D.measureText; WebCore Generated/Objects.fs adapter | Inject/cache native measurements; pure label placement consumes bounds |
| Frame scheduling | Existing WebCore frame integration and host-thread graph execution | Adapter consumes frame scheduling/post functions; no standalone runtime |
| Persistence | Snapshot Ranvier.Persistence PersistCodec module / Persist.fs and versioned codec Types.fs | Optional consumer adapter only; not added as a mandatory chart dependency |

## Completed expansion through task 4

Tasks 0–4 are implemented: pinned fixture provenance; scene groups, clips,
gradients and scoped identity; native text measurement and pure label placement;
statistics, scale/time/format additions, extended curves/arcs/symbols/stacks;
shared Anime perceptual colors and D5 palette policy; Cartesian, distribution,
radial, calendar, legend and facet recipes; owned gestures, selection, keyed
transitions and bounded background layouts.

The table above records baseline and planned ownership, not exhaustive parity.
See [release validation](release-alpha.2.md) for tested scope and deliberate
policies for alpha.2. The current hierarchy milestone adds task 5 construction,
layouts and recipes, with [native snapshots and explicit limitations](hierarchy-charts.md).
Task 6 now includes robust planar predicates/polygon operations and an immutable quadtree with aggregate traversal; see [planar contracts](planar.md) and [quadtree contracts](quadtree.md). Task 6 also adds [Delaunay triangulation, clipped Voronoi and native picking](spatial.md), with explicit extreme-input fallback and degeneracy policies. Task 7 now includes [deterministic immutable force state, collision and Barnes–Hut forces](force.md); [network/bubble recipes and owned worker/drag integration](network.md) are implemented. Task 8 now includes [chord/directed/transposed layouts and ribbon paths](chord.md); [DAG Sankey layouts](sankey.md) are also available; [chord, Sankey, staged alluvial and owned drag recipes](flow-charts.md) now compose these kernels. Task 9 now includes [density grids, sampled Gaussian blur and analytical 1D KDE](density.md); [contour topology and native worker composition](contours.md) are implemented; [hexbin and min/max/LTTB sampling](reduction.md) provide bounded aggregates and original-ID reduction; [heatmap, hexbin, contour and isoband recipes](density-charts.md) now expose aggregate membership and polygon-aware selection; [violin and reduced-series recipes](distribution-series.md) preserve original statistics with bounded visible geometry; tasks 10–11 remain open. Full locale/time parsers, all D3 palette tables, geographic pipelines and additional network recipes are not claimed by this milestone.

Reference fixtures: `tests/fixtures/manifest.json`, exact npm lockfile and copied licenses in `tools/reference-fixtures/`. Generation is development-only. Numeric tolerance and coordinate conventions travel with the fixtures. Native ownership, topology errors and missing-data behavior are tested as D5 contracts rather than copied blindly from JavaScript.

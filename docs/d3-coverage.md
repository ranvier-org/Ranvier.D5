# D3 coverage and toolkit ownership

Baseline: alpha.1 / 52ef102. This ledger separates algorithms from native infrastructure. Tasks refer to `superpowers/plans/2026-11-12-d3-expansion.md`; only tasks 0–4 are in the current implementation batch.

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
| geo | Absent | Task 10 separate package |
| hierarchy | Partial simple tree and slice-and-dice treemap | Task 5 |
| interpolate | Anime scalar/color facilities; pure D5 Transition | Task 2 public reusable color interpolation, task 4 topology-aware scene transition |
| path | Immutable SVG commands | Task 2 shapes use existing paths |
| polygon | Absent | Task 6 |
| quadtree | Absent; immutable k-d tree is a different algorithm | Task 6 |
| random | Anime seeded closure available | Task 11 explicit-state distributions; no hidden random use now |
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

Current package cohort is unchanged: Ranvier 0.1.1-preview.2, Anime 0.1.0-rc.1, CSS 0.1.1-alpha.2, View 0.1.49-rc.2, Dom 0.1.0-rc.1, WebCore managed 0.1.0-rc.6 and SDL 0.1.0-rc.10. Source snapshot revisions are reference material, not an API compatibility guarantee.

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

Reference fixtures: `tests/fixtures/manifest.json`, exact npm lockfile and copied licenses in `tools/reference-fixtures/`. Generation is development-only. Numeric tolerance and coordinate conventions travel with the fixtures. Native ownership, topology errors and missing-data behavior are tested as D5 contracts rather than copied blindly from JavaScript.

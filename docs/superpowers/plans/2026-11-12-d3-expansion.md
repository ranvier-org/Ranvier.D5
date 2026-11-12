# Ranvier.D5 algorithm and chart expansion implementation plan

> **For agentic workers:** Use superpowers:executing-plans to implement this plan task by task. Each numbered task has its own red/green test cycle and reviewable commit. Do not start implementation merely because this planning document exists.

**Goal:** Expand the released foundation into a functional native visualization toolkit covering the major D3 algorithm families and reusable chart compositions.

**Architecture:** Pure data → layout → geometry → scene functions, with Ranvier-owned reactive adapters around them. View owns retained rendering and event lifetimes; WebCore owns native layout/text/painting; Anime owns easing, timing integration and reusable interpolation; CSS owns presentation tokens. Heavy algorithms publish immutable, versioned results back to the host thread.

**Tech stack:** F#, .NET 10, existing compatible Ranvier package cohort, Expecto, native WebCore/SDL, normal and Traced builds.

**Spec:** `docs/design.md`, `docs/contracts.md`, and the scope/contracts below. Baseline verified against local and public main `52ef102dd473f0e08549b2c1144110dc0cbad0be`, release `v0.1.0-alpha.1`, on 2026-11-12.

## Scope and ordering decisions

This is an implementation roadmap, not a promise of method-for-method JavaScript compatibility. D3 supplies algorithms rather than a fixed catalog of chart classes. D5 chart types will be small compositions over reusable algorithms. Distinguish existing capabilities, improved implementations and new families in the coverage ledger.

The current release already has summaries/binning/joins, basic scales and UTC ticks, paths, lines/areas/arcs, pie/diverging stack, simple tree/slice-and-dice treemap, a k-d index, signals, transitions, zoom/brush maths and native keyed SVG. Do not rebuild these. Specifically, current CatmullRom is uniform/cardinal, ticks are numeric even for log scales, scenes are flat path/text lists, axis keys are positional, and tree/treemap algorithms are deliberately basic.

Prioritize useful interactive Cartesian charts, then hierarchy, spatial/network algorithms, flows, contours and geography. Geography comes last among major families because clipping, spherical topology and resampling make it a larger correctness commitment. Sankey and hexbin are ecosystem extensions, not implied by the core D3 API index.

## Global constraints

- Remain native .NET 10 first; browser/Fable is a separate qualification effort.
- Keep pure algorithms usable without a graph, host, timer or window. Chart builders return values and never create signals.
- Use immutable records/DUs, total matching, explicit options for missing observations, and stable domain IDs. Private mutable arrays/builders are allowed within a kernel or isolated worker; never expose a mutable shared buffer as an immutable result.
- Preserve existing public entry points through wrappers. New validated configurations reject programmer errors at construction. New data-dependent layouts return `Result<_, LayoutError list>` for duplicate IDs, invalid topology or unsupported inputs. Do not silently discard bad graph edges.
- No replacement reactive engine, DOM reconciler, CSS parser, easing library, global event dispatcher or animation timer.
- Keep the existing exact compatible package cohort until a separately tested coordinated toolkit upgrade. Snapshot APIs are not proof that a pinned NuGet API is public or available.
- Explicit seeds, clocks, culture, timezone and ordering; no hidden randomness, ambient locale or worker reads of signals.
- Baseline plus new tests run normal/Traced on Windows/Linux/macOS. Native tests execute on the process main thread on macOS.
- No arbitrary paths silently morphed. Interpolate compatible topology; cross-fade incompatible topology or return an explicit unsupported result.
- Preserve source licenses and attribution for ported algorithms, palette tables and fixture generators. Pin reference versions; JavaScript is permitted only in offline test-fixture tooling.

## Shared proposed interfaces

These names are planned APIs, not APIs present in alpha.1. Define each type in its owning task before consumers land.

```fsharp
type LayoutError =
    | DuplicateId of string
    | MissingEndpoint of string
    | InvalidTopology of string
    | InvalidObservation of string
    | BudgetExceeded of string

// Task 1, in Ranvier.D5.Charts:
// ChartSpec<'T> = immutable viewport, semantic key projection, encodings,
// domain policies, theme, accessibility and label policies.
// ChartOutput = immutable scene plus semantic mark-ID/data-ID mapping,
// plot bounds, hit-test geometry and accessibility description.
val Chart.build : ChartSpec<'T> -> 'T list -> Result<ChartOutput, LayoutError list>

// Task 4, Ranvier.D5.View: explicit owned boundary around pure kernels.
type WorkState<'T> = Idle | Running of generation:int64
                  | Ready of generation:int64 * value:'T
                  | Failed of generation:int64 * errors:LayoutError list
// OwnedLayout<'T> exposes a read-only state memo and explicit Dispose.
val ReactiveLayout.create : HostDispatcher ->
    (unit -> 'Input) ->
    (System.Threading.CancellationToken -> 'Input -> Result<'Output, LayoutError list>) ->
    OwnedLayout<'Output>
```

`HostDispatcher` is a narrow injected post-to-owner-thread function, adapted to the existing host integration. It is not a new dispatcher runtime. Snapshot inputs in a graph effect; workers receive owned copies/immutable values. Use one running computation and one latest pending request. Increment generation for data, configuration, viewport and disposal changes. Only the current generation can publish.

Keep `Scene.create` working. Task 1 adds a separate richer scene constructor with keyed nodes, groups and definitions. Keep leaf key identity stable; scope nested keys with a structured key path, not concatenation vulnerable to collisions. Moving a node across parent groups may remount and must be documented.

## Packages and file ownership

| Package | Responsibility and additions |
|---|---|
| `Ranvier.D5` | Existing core; split additional algorithms into focused files/modules instead of expanding `Geometry.fs` and `Layout.fs` indefinitely. |
| `Ranvier.D5.Charts` (new, task 1/3) | Immutable encodings/specifications, chart recipes, labels, legends, faceting and semantic selection mappings. Depends on core, not View/WebCore. |
| `Ranvier.D5.View` | Keyed scene nodes, native text-measure adapter, pointer/keyboard adaptation, owned reactive layout runner. |
| `Ranvier.D5.Geo` (new, task 10) | Geographic types, projection/clipping pipeline and GeoJSON adapter. Keeps specialized geographic surface optional. |
| `Ranvier.D5.IO` (optional, task 11) | DSV and serialization conveniences only if the toolkit lacks suitable public adapters. No networking subsystem. |

Create matching Traced package variants only where dependency identities require them. Put shared color-space/interpolation additions in Anime/CSS when reusable across the kit; D5 owns visualization palettes and scale-to-color policy. Persistence stores chart configuration/selection/viewport values, never View nodes or graph owners.

## Review focus

1. Reorder/filter/stream updates must preserve semantic IDs, focus and selection; exiting IDs must not survive in tooltip or hit-test state.
2. Empty/all-missing/constant/mixed-sign/extreme data must have documented finite output or a typed error; no hidden loops or fabricated zeroes.
3. Stale workers, canceled drags and disposed owners must never publish or resurrect marks; test publication races as well as cancellation requests.
4. Screen coordinates, DPI, SVG viewBox, nested transforms and geographic units must remain explicit; test resizing during interaction.
5. Text measurement/font changes, color gamut and topology changes must not create unstable layout feedback or misleading transitions.

## Task 0 — Freeze coverage and reference fixtures (P0; PR A1)

**Files:** add `docs/d3-coverage.md`, `tests/fixtures/manifest.json`, `tools/reference-fixtures/`; update test project files and `kit.lock.json` only if a deliberate dependency upgrade is accepted.

**Deliverable:** A per-family ledger: present, partial, planned, toolkit replacement or extension. Audit public pinned Anime color/interpolation/random facilities, CSS color values, View pointer capture/text/native scheduling, and Persistence codecs. Record exact symbol/version before using an adapter. No upstream upgrades bundled into algorithm work.

- [ ] Add fixture-schema tests rejecting missing seed, source version, coordinate convention or tolerance; see them fail.
- [ ] Pin D3 reference packages and generate small deterministic fixtures; keep upstream licenses. Include empty, duplicate, missing and adversarial numeric cases. Use geometric invariants rather than expecting identical SVG strings.
- [ ] Enumerate all core D3 modules. Assign selection/join/events to View plus D5 joins; ease/timer/transition to Anime plus Ranvier ownership; collection operations to F#; fetch to existing .NET/toolkit IO. Track additional algorithms explicitly rather than claiming complete parity.
- [ ] Run the fixture tests and existing suite; commit the ledger and fixtures.

**Acceptance:** Every listed family has an owner and task or explicit toolkit replacement. No runtime JavaScript dependency.

## Task 1 — Scene composition, identity and layout contracts (P0; PR A2–A3)

**Files:** add core `SceneNode.fs`, `SceneDefinitions.fs`; modify `Scene.fs`, `Scene.fs`'s Axis module and View `Renderer.fs`; add `SceneCompositionTests.fs` and native acceptance cases. Add Charts `ChartTypes.fs`, `Encoding.fs`, `ChartLayout.fs` and its project.

**Interfaces:** `SceneNode = Leaf of Mark | Group of GroupSpec * SceneNode list`; immutable `GroupSpec` carries key, transform and optional clip reference. Definitions initially support rectangular clipping and linear/radial gradients. `Scene.compose : Viewport -> string -> SceneDefinition list -> SceneNode list -> Scene`. Define `ChartOutput` and semantic ID mapping here; recipes arrive in task 3.

- [ ] Red tests: two charts with identical local keys coexist; reorder retains node identity; changing a tick set preserves surviving value keys; clipped marks cannot paint outside the plot; escaped labels remain text.
- [ ] Add groups/definitions through existing View keyed regions. Per-mount namespace definition IDs to prevent cross-chart SVG collisions. Do not replace the root on each update. Extend text style with font family/weight, anchor and baseline alignment; keep CSS serialization at the existing boundary.
- [ ] Add value/category-keyed axis entry points and explicit domain-line endpoints. Preserve old helpers as wrappers; no index-based transition identity in new charts.
- [ ] Implement a pure label-placement pass consuming measured text bounds. Cache native measurement by text/font/style/device scale/font generation. Publish measurement changes in a batch; cap feedback to two layout passes, then use a documented ellipsis/skip policy.
- [ ] Verify nested transforms, duplicate keys, multiple chart mounts, delayed font load, DPI changes and disposal. Commit renderer and chart-contract changes as separate PRs.

**Acceptance:** Two independently reactive charts render together with clipping, stable keys and bounded label layout. CPU snapshot pixels and DOM identity both verified.

## Task 2 — Scale, shape, statistics and color completion (P0; PR B1–B4)

**Files:** add `Statistics.fs`, `Curves.fs`, `Stack.fs`, `TimeTicks.fs`, `Format.fs`, `Chromatic.fs`; modify `Scale.fs`, `Geometry.fs`; add corresponding Expecto suites. Shared color/interpolation extensions go into Anime/CSS only after task 0's API audit.

**Interfaces:** `Statistics.variance : float seq -> float option`; `Statistics.histogram : BinRule -> float seq -> Bin<float> list`; `Scale.logTicks`, piecewise continuous scales and `Scale.invertExtent`; `TimeTicks.choose : int -> DateTimeOffset * DateTimeOffset -> TimeIntervalSpec`; `CurveSpec` as a new DU to avoid changing the meaning of existing `Curve.CatmullRom`; `Stack.layout : StackConfig -> Series list -> Result<StackLayer<string> list, LayoutError list>`; `Palette.sample : Palette -> float -> Paint`.

- [ ] B1: red/green sample variance, cumulative summaries, rollups, bisect-left/nearest and Sturges/Scott/Freedman–Diaconis bins. Pin sample-vs-population semantics. Cover singleton, zero-IQR, all-missing and extreme values. Use F# built-ins for ordinary collection work.
- [ ] B2: red/green log-specific ticks, sqrt convenience, piecewise mapping/inversion, threshold extent inversion, UTC interval multiples/automatic selection, explicit culture/SI/percentage/time formatters. Local timezone ticks use an explicit `TimeZoneInfo`; specify DST gaps/folds and do not infer machine timezone.
- [ ] B3: add monotone X/Y, basis, natural, bump X/Y, step-middle, cardinal/open/closed, centripetal/chordal Catmull–Rom and bundle curves; radial lines/areas/links, vertical links, arc padding/corners and remaining symbols. Extend via new configuration APIs and retain legacy semantics. Add stack order policies and zero/expand/silhouette/wiggle/diverging offsets; reject negative inputs where an offset assumes nonnegative data.
- [ ] B4: reuse public Anime interpolation for scalar/color/transform transitions; add missing reusable perceptual color math upstream rather than cloning it. Provide categorical/sequential/diverging/cyclic palette values, explicit sRGB/Lab/HCL interpolation and gamut policy. Preserve palette attribution. Add OKLab only as a separately identified extension, not a D3 compatibility requirement.
- [ ] Each PR: write numeric/reference/property failures first, implement, run normal/Traced tests and benchmarks, commit.

**Acceptance:** Curves respect gaps and area boundary reversal; monotone interpolation does not overshoot a monotone segment; stack totals/order/offset invariants hold. Color tests cover alpha, hue wrap, achromatic colors and out-of-gamut values. Numeric and geographic curves must not share implicit coordinate assumptions.

## Task 3 — Common chart recipes and composition (P0; PR C1–C3)

**Files:** Charts `Cartesian.fs`, `Distribution.fs`, `Radial.fs`, `Legend.fs`, `Facet.fs`; add chart recipe tests and gallery scenes.

**Interfaces:** concrete immutable `BarSpec<'T>`, `LineSpec<'T>`, `ScatterSpec<'T>`, `HistogramSpec<'T>`, `HeatmapSpec<'T>` plus `Chart.bar`, `Chart.line`, `Chart.scatter`, `Chart.histogram`, `Chart.heatmap`, each returning `Result<ChartOutput, LayoutError list>`. Add corresponding recipe specs as they land; do not build a universal dynamic property bag. Shared `Encoding<'T,'V>` holds accessor and label; keys are supplied separately.

- [ ] C1: implement horizontal/vertical/grouped/stacked/100% bars, line/step/area/stacked area/streamgraph, scatter/bubble and grid heatmap. Reuse task 2 stacks/curves/scales; no chart-specific duplicate geometry kernels.
- [ ] C2: implement histogram, box plot, error bars, waterfall, bullet, lollipop and OHLC/candlestick. Define quartile/whisker policy explicitly; require caller-supplied interval semantics for error bars; validate OHLC ordering. Violin waits for density in task 9.
- [ ] C3: implement pie/donut, radial bar, radar, parallel coordinates, calendar heatmap, categorical/continuous legends and shared/independent-domain facets. Pie/donut labels obey collision policy; radial area encodings document whether radius or area represents value.
- [ ] Red/green fixtures per recipe: empty, one item, all missing, negative/zero values, duplicate semantic keys, reorder and size change. Include legends that hide/show a series without changing remaining keys.
- [ ] Add examples showing `Api.createSignal` → `Reactive.derive` → `Chart.*` → `Renderer.create`, with no signals created inside chart builders. Verify source edits update only relevant derived geometry.

**Acceptance:** Each recipe can render standalone, compose in a facet, return semantic selection IDs and provide accessible labels. Ordinary line/bar/scatter use is complete before advanced algorithm work starts.

## Task 4 — Owned gestures, transitions and background layouts (P0; PR D1–D3)

**Files:** core `Interaction.fs`; View `InputAdapter.fs`, `ReactiveLayout.fs`, `ChartInteraction.fs`; Charts `Selection.fs`; extend `Reactive.fs` through compatible adapters. Add `InteractionTests.fs`, `ReactiveLayoutTests.fs`, native tests and controlled worker race fixtures.

**Interfaces:** `Gesture.update : GestureConfig -> InputEvent -> GestureState -> GestureState * GestureCommand list`; `Selection.update : SelectionMessage -> SelectionState -> SelectionState`; `ReactiveLayout.create` defined above. Commands include capture/release pointer and semantic selection; effects execute only in the owned View adapter.

- [ ] D1: red/green wheel/pinch zoom, pan, drag, rectangular brush, lasso, hover/tooltip and keyboard selection. Convert client/device coordinates to plot coordinates using existing native geometry APIs; capture pointer through toolkit support. Clear gestures on cancel, focus loss, node exit and disposal. Clamp zoom extents and preserve the anchor.
- [ ] D2: keyed enter/update/exit transitions with Anime easing and frame integration. Snapshot current interpolation on interruption; compatible path topology can interpolate, incompatible paths cross-fade. Reduced motion completes immediately. No stale completion callback can remove a newly re-entered key.
- [ ] D3: owned background layout adapter with one running and one replaceable pending request, generation validation, cancellation, errors, and last-good-result presentation. Publish at most once per host frame; never read/write signals on the worker. Cancellation is cooperative and checked inside kernels, not merely before launch.
- [ ] Race tests: A finishes after B is requested, worker completes after disposal, rapid 100-request burst, fault/retry, resize during drag, and animation exit/re-entry. Native capture/lost-capture tests run on all platforms.

**Acceptance:** Linked scatter/brush/line views work with signals and optional Persistence state. Queue size is bounded at two requests, publications are owner-thread-only, and expensive layouts cannot monopolize the UI loop. Initial synchronous recipes from task 3 remain valid.

## Task 5 — Hierarchy algorithms and chart family (P1; PR E1–E3)

**Depends on:** tasks 1–4.

**Files:** `HierarchyBuild.fs`, `Tree.fs`, `Partition.fs`, `Pack.fs`, `Treemap.fs`; Charts `HierarchyCharts.fs`; corresponding fixtures/tests.

**Interfaces:** `Hierarchy.stratify : ('T -> string) -> ('T -> string option) -> 'T list -> Result<HierarchyNode<string>, LayoutError list>`; `Tree.layout`, `Cluster.layout`, `Partition.layout`, `Pack.layout`, `Treemap.layout` consume immutable configs and hierarchy values. `Treemap.resquarify : TreemapConfig -> Hierarchy -> TreemapState option -> Result<TreemapResult * TreemapState, LayoutError list>` makes retained layout state explicit.

- [ ] E1: validate parent tables, roots, cycles and disconnected forests; provide an explicit virtual-root constructor for forests. Add tidy tree (Buchheim/Reingold–Tilford family), cluster and partition with configurable separation. No uncontrolled recursion on deep chains.
- [ ] E2: add circle packing and binary/slice/dice/slice-dice/squarified/resquarified tiling with stable ordering, padding and sum policies. Preserve current simple APIs as wrappers.
- [ ] E3: recipes for dendrogram, radial tree, icicle, sunburst, packed bubbles and treemap drill-down. Focus/selection use domain IDs through collapse/expand; transitions preserve identity and use task 4 cancellation.
- [ ] Test containment, nonoverlap, area/weight proportions, zero weights, repeated labels with distinct IDs, invalid parents, a 50,000-node chain and 10,000-node wide tree.

**Acceptance:** Finite bounded output, deterministic ordering and no stack overflow. Incremental treemap state belongs to the caller; changing layout configuration invalidates incompatible retained state.

## Task 6 — Robust planar geometry and spatial algorithms (P1; PR F1–F3)

**Files:** `Polygon.fs`, `Predicates.fs`, `Quadtree.fs`, `Delaunay.fs`, `Voronoi.fs`; chart hit-test adapters and tests.

**Interfaces:** `Polygon.area`, `centroid`, `contains`, `hull`; `Delaunay.build : Point list -> Triangulation`; `Voronoi.clip : Bounds -> Triangulation -> VoronoiCell list`; private-array immutable triangulation with copying enumeration/accessors; `Quadtree.build`, `within`, `nearest` and aggregate traversal. Keep the existing k-d index; do not redirect its semantics unnecessarily.

- [ ] F1: robust orientation/incircle predicates with adaptive precision fallback, polygon winding/containment/centroid/hull; specify boundary inclusion and ring orientation. Port a proven permissively licensed implementation with attribution rather than inventing epsilon-only topology.
- [ ] F2: build a quadtree supporting coincident-point buckets, finite bounds expansion, aggregate mass and extent traversal. Bound subdivision depth; zero-width domains cannot recurse indefinitely. Mutable build state remains private.
- [ ] F3: Delaunay triangulation, clipped Voronoi cells, adjacency and nearest-site lookup. Map duplicate coordinates back to original semantic IDs; define empty/singleton/collinear behavior. Add Voronoi overlays and robust scatter picking.
- [ ] Reference/property tests verify orientation, adjacency symmetry, empty circumcircle within defined degeneracy rules, clipping containment and deterministic cocircular tie breaking. Include huge/tiny mixed coordinates.

**Acceptance:** Geometry retains topology under supported numeric inputs. Quadtree aggregates are reusable by force; triangulation is reusable by mesh/Voronoi charts without rendering dependencies.

## Task 7 — Force simulation and network charts (P1; PR G1–G3)

**Depends on:** tasks 4 and 6; basic graph validation can land independently.

**Files:** `GraphData.fs`, `Force.fs`, `ForceKernels.fs`; Charts `Network.fs`; worker and native drag fixtures.

**Interfaces:** `Force.initialize : ForceConfig -> Seed -> GraphData -> Result<ForceState, LayoutError list>`; `Force.step : int -> ForceState -> ForceState`; `Force.snapshot : ForceState -> ForceSnapshot`. Config includes alpha/cooling, velocity decay, forces, pinned positions and deterministic ordering. State storage is private; stepping never mutates an earlier public state.

- [ ] G1: deterministic seeded initialization, explicit fixed-step integration, center/X/Y/radial/link forces and stable-ID topology updates. Retain positions for surviving IDs; initialize only additions; reject unknown endpoints and nonfinite values.
- [ ] G2: collision and Barnes–Hut many-body kernels using task 6 aggregate traversal; handle coincident points with deterministic jitter. Benchmark against exact small-N pairwise forces and test approximation error at declared theta settings.
- [ ] G3: force-directed network and bubble-cluster recipes; drag pins/unpins nodes and reheats through explicit messages. Run chunks in task 4's worker adapter, publish latest immutable snapshots, stop on convergence/iteration budget/cancellation. Anime animates presentation changes; it does not drive the physics equations.
- [ ] Verify same seed/config/input reproduces trajectories, pinned nodes stay fixed, cooling terminates, topology changes do not resurrect exited IDs, and cancellation is checked at bounded inner-loop intervals.

**Acceptance:** A 1,000-node/2,000-edge interactive example stays responsive while simulating. Do not claim arbitrary force layouts converge to a unique optimum. Solver quality and scheduling latency are tested separately.

## Task 8 — Flow and relationship charts (P1; PR H1–H3)

**Files:** `Chord.fs`, `Ribbon.fs`, `Sankey.fs`; Charts `FlowCharts.fs`; reference fixtures and interaction tests.

**Interfaces:** `Chord.layout : ChordConfig -> float list list -> Result<ChordLayout, LayoutError list>`; `Ribbon.path : RibbonSpec -> Path`; `Sankey.layout : SankeyConfig -> FlowGraph -> Result<SankeyLayout, LayoutError list>`.

- [x] H1: square-matrix chord/directed-chord/transposed modes, group/subgroup ordering, padding, ribbon and arrow-ribbon geometry. Cover asymmetric matrices, zeros, nonnegative validation, angular conservation and stable source/target IDs.
- [x] H2: DAG Sankey validation, node layers/alignment, iterative relaxation, node padding and link stacking. Detect cycles and return typed errors; do not silently break edges. Flow imbalance is represented explicitly; node size follows documented max(inflow,outflow) policy rather than inventing conservation.
- [x] H3: chord, Sankey and staged alluvial compositions; drag repositions nodes and recomputes incident links without restarting unrelated layout. Stable edge IDs distinguish parallel links.

**Acceptance:** Ribbon/link widths and ordering are deterministic; feasible nodes do not overlap; infeasible viewport/padding is reported. Circular Sankey is a separate extension, not part of this DAG implementation.

## Task 9 — Density, contours and large-data charts (P1; PR I1–I3)

**Files:** `Density.fs`, `Contour.fs`, `Hexbin.fs`, `Sampling.fs`; Charts `DensityCharts.fs`; cancellation/performance fixtures.

**Interfaces:** `Density.grid : DensityConfig -> WeightedPoint list -> DensityGrid`; `Contour.compute : ContourConfig -> DensityGrid -> ContourPolygon list`; `Hexbin.aggregate : HexbinConfig -> Point list -> HexBin list`; `Sampling.minMax` and `Sampling.lttb` return samples retaining original IDs.

- [x] I1: uniform grid aggregation, explicit kernel bandwidth and separable Gaussian blur; 1D KDE supports violin/density curves. Define normalization and boundary handling; missing grid cells are distinct from measured zero.
- [x] I2: marching squares with a specified asymptotic-decider saddle rule, ring stitching, hole classification and optional interpolation/smoothing. Validate thresholds and dimensions; define equal-threshold classification and NaN masking. Cancellation checks occur per grid strip.
- [ ] I3: density heatmaps, contour/isoband maps, violin, hexbin and large time-series recipes. Min/max envelopes preserve extrema; LTTB is opt-in visual reduction, never used for statistics. Picking/selection retain original semantic IDs or expose aggregate membership explicitly.
- [ ] Test constant/empty/all-missing grids, diagonal saddles, nested holes, boundary rings, threshold monotonicity, weighted conservation and downsampling endpoints/gaps. Reuse definitions/gradients from task 1.

**Acceptance:** 100,000 input points can be aggregated off-thread into bounded visible output. Contour output is topologically valid before it is converted to paths. Hexbin is labeled an ecosystem extension.

## Task 10 — Geographic pipeline and map charts (P2; PR J1–J4)

**Files:** new Geo project: `GeoTypes.fs`, `Spherical.fs`, `GeoJson.fs`, `Projection.fs`, `Clip.fs`, `Resample.fs`, `GeoPath.fs`; Charts geographic adapters kept in a separate optional project if needed to avoid a core dependency cycle.

**Interfaces:** `Projection.project : Projection -> GeoPoint -> Point option`; `Projection.tryInvert : Projection -> Point -> GeoPoint option`; `GeoPath.project : GeoPathOptions -> Projection -> GeoGeometry -> Result<Path list, GeoError list>`; spherical area/centroid/distance/bounds functions. Geographic degrees/radians use distinct types or F# units of measure.

- [ ] J1: typed GeoJSON geometry/feature collections, spherical distance/area/centroid/bounds, rotation, graticule and great-circle interpolation. Explicit longitude normalization, ring convention and hemisphere semantics; map RFC 7946 input into the internal convention rather than assuming it matches D3.
- [ ] J2: streaming geometry pipeline: rotation → preclip/antimeridian or cap clipping → projection/adaptive resampling → viewport clipping → path. Implement identity/equirectangular/Mercator/orthographic first. `project` alone is not a complete polygon projection API.
- [ ] J3: conic conformal/equal-area/equidistant, azimuthal equal-area/equidistant, gnomonic/stereographic, transverse Mercator and common composites. Add fit-to-bounds using streamed clipped geometry; specify projection singularities and maximum resampling depth/output vertices.
- [ ] J4: choropleth, geographic points/bubbles, routes and orthographic globe compositions with signal-driven projection/selection. Join external values by stable feature ID. Map loading uses existing IO and publishes through task 4; no new networking engine. TopoJSON decoding and additional projections are separate optional adapters.
- [ ] Test antimeridian crossings, poles, holes, multipolygons, hemisphere-spanning rings, hidden hemisphere, projection singularities, inverse roundtrips where defined and bounded adaptive subdivision. Pixel screenshots alone are insufficient.

**Acceptance:** World polygons can cross seams without spurious full-screen triangles, holes remain holes, and visible output stays within clipping bounds. Geographic transforms never silently use planar area/centroid operations.

## Task 11 — Utility coverage, advanced recipes and public API stabilization (P2; PR K1–K3)

**Files:** optional IO project, `Random.fs` if the toolkit lacks explicit-state generators, formatting/time modules, recipe modules, coverage ledger, docs and gallery.

- [ ] K1: complete explicit-state random distribution helpers needed by reference fixtures/simulation (uniform, normal, lognormal, exponential, gamma/beta, Poisson/binomial and related distributions). Reuse seeded toolkit primitives where available; test distribution properties with deterministic statistical tolerances rather than flaky random assertions.
- [ ] K2: strict CSV/TSV parse/format with quoted delimiters/newlines/BOM and explicit schema conversion if existing toolkit adapters are insufficient. Use .NET/toolkit async IO. Complete requested number/time format parsing and timezone interval policies; reuse F# Map/Set/Seq instead of cloning D3 collection wrappers.
- [ ] K3: compose connected scatter, slope/bump/rank charts, horizon, beeswarm, network adjacency matrix and linked small multiples using prior modules. Add extension examples for a caller-written layout. Do not add 3D, raster-tile services or circular Sankey under a vague parity claim.
- [ ] Reconcile every D3 core module in the ledger. List exact algorithms still absent; remove a parity claim unless its fixtures and contracts are complete. Audit public naming, invalid-input behavior, XML docs, package dependencies and allocation ownership before a stable release.

**Acceptance:** Each supported chart has a minimal pure example, a signal-driven example, keyboard/accessibility behavior and a documented complexity/visible-output limit. Specialized extensions are visibly distinguished from core coverage.

## Verification and performance contract

Every task uses this sequence: add the named failing tests → run the focused test to observe failure → implement → run the complete affected suite → benchmark if hot paths changed → commit. Each algorithm PR contains fixture provenance, complexity expectations and an executable example. Do not skip invariants because a reference snapshot matches.

Baseline commands (prefix with `rtk.exe proxy` on the current Windows workspace):

```text
dotnet run --project tests/Ranvier.D5.Tests -c Release
dotnet run --project tests/Ranvier.D5.Native.Tests -c Release
dotnet run --project benchmarks/Ranvier.D5.Benchmarks -c Release
dotnet run --project examples/Ranvier.D5.Gallery -c Release -- --capture artifacts/gallery.bmp
```

Repeat with `-p:RanvierTrace=true`. New chart/Geo test executables follow the same layout. Add every new package to pack-and-consume CI, not only project-reference builds. Preserve the current six platform/variant jobs and native main-thread rule.

### Proposed gates, not measured claims

- Preserve alpha.1 absolute guards while collecting matched before/after evidence. Existing normal CI 500-mark update+paint medians were approximately 6.26 ms macOS, 7.89 ms Linux and 10.53 ms Windows; compare only equivalent runtime/platform/variant.
- For established cases, flag >20% median regression with >2 ms absolute growth, or >10% managed allocation growth with >64 KiB absolute growth. Confirm with three fresh-process paired runs before declaring failure; use the median of process results. Do not automatically rebaseline regressions.
- Expand sampling from 15 to at least 60 measured iterations after warm-up for tail analysis. Keep hard timeouts and complexity-size tests; p95 of 15 samples is currently the maximum and too noisy for tight gates.
- Add fixtures: 100,000-point line reduction; 10,000-node hierarchy; 100,000-site spatial construction; 1,000-node/2,000-edge force; 256×256 and 1024×1024 contour grids; seam-heavy 100,000-vertex geography. Track runtime, managed allocation, output count, peak native/process memory and cancellation latency separately.
- Target host-thread adapter overhead below 4 ms p95 for the 500-visible-mark fixture on a documented reference machine; target a 16.7 ms complete interactive frame there. These are optimization targets, not guarantees on shared runners or million-mark SVG scenes.
- Worker chunks target 4–8 ms with cancellation checkpoints inside kernels. Hard assertions: one running + one pending, no stale publications, no publication after disposal, and bounded intermediate/output allocations. Measure cooperative cancellation; do not promise a hard real-time deadline.
- Add a native dense-scene benchmark before implementing a second renderer. First aggregate, reduce, cache and clip. Consider a Canvas/GPU adapter only if measured SVG work remains the bottleneck; that is a separate design using existing WebCore surfaces, not a dependency of every chart type.

## PR and release order

1. A1–A3 establish coverage and composition. B1–B4 complete shared algorithms.
2. C1–C3 and D1–D3 deliver an interactive Cartesian release. This is the first useful expansion milestone.
3. E1–E3 deliver hierarchy. F1–F3 then G1–G3 deliver spatial/network capability.
4. H1–H3 and I1–I3 deliver flow/density. Their independent kernels may be scheduled separately after their prerequisites, without sharing mutable implementation state.
5. J1–J4 deliver geography; K1–K3 close utility coverage and stabilize APIs.

Keep PRs bounded by independently testable interfaces rather than entire priority tiers. Review, watch CI with `gh pr checks --watch` / `gh run watch --compact --exit-status`, fix failures, merge green PRs, and release completed capability milestones. Every release includes normal/Traced packages, native gallery captures and measured benchmark artifacts. NuGet publication remains dependent on configuring the new repository's trusted-publishing policy; GitHub release assets already work.

Do not tie publication of common charts to finishing geography. Do not change all existing configurations just to resemble D3's mutable fluent API.

## References

Primary upstream references, checked 2026-11-12. Pin implementation/test fixture revisions during task 0 rather than depending on moving documentation:

- D3 module coverage: https://d3js.org/api
- Shape/curve families: https://d3js.org/d3-shape/curve
- Force simulation semantics: https://d3js.org/d3-force/simulation
- Geographic pipeline: https://d3js.org/d3-geo
- DAG Sankey extension: https://github.com/d3/d3-sankey
- Local baseline: `src/Ranvier.D5/{Scene,Scale,Geometry,Layout,Spatial,Reactive}.fs`, `src/Ranvier.D5.View/Renderer.fs`, `docs/contracts.md`.

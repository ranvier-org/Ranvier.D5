# Ranvier.D5 foundation

Build reusable visualization primitives that can be composed into chart families, following D3's modular approach (https://d3js.org/api), with F# functions, immutable records, discriminated unions and explicit optional values. This is a native .NET 10 foundation, not a JavaScript wrapper or a claim of complete D3 algorithm parity.

## Architecture

`Ranvier.D5` contains pure data transformations, statistics, numeric and categorical scales, UTC intervals, paths, shapes, axes, hierarchy and spatial primitives, immutable scenes, interaction state and signal-based composition. `Ranvier.D5.View` translates scenes into keyed Ranvier.View SVG templates. The native example and integration tests mount that component through WebCore. Applications can supply new layouts as ordinary functions from data to marks; specialized contour, triangulation, geographic clipping and advanced force algorithms can build on these contracts later.

Reuse Ranvier signals/memos/effects, owners and batching. Reuse Ranvier.Anime easing and interpolation; time is an injected signal so D5 creates no competing scheduler. Reuse Ranvier.Css typed values for presentation. Reuse View's keyed regions and event/lifetime machinery rather than creating another reconciler. WebCore remains the layout, SVG, text and native rendering engine. Persistence and other tools are available in the snapshot workspace but become dependencies only when a feature needs them.

All public algorithm configuration is immutable. Mutation is confined to local arrays/builders and owned host integration. A caller can share a scale or scene safely without a hidden domain update. Unknown categories return None. Invalid configuration throws ArgumentException at construction; non-finite data has documented omission or rejection behavior. Missing line points break segments. Duplicated scene/data keys reject before mutation. Formatting is invariant. UTC time is explicit. Paths and transformed coordinates must remain finite.

## Completion criteria

- Eight pinned source snapshots, reproducible manifest and editor workspace; original checkouts preserved, including uncommitted changes.
- Data summaries, grouping, binning and keyed joins; continuous/log/power/symlog, band/point/ordinal, threshold/quantile/quantize and UTC time scales with ticks and inversion where meaningful.
- Immutable path commands and SVG serialization; line/area/arc/link/symbol, pie and diverging stack layout, axes, hierarchy traversal/layout and spatial queries.
- Immutable keyed scenes, affine/zoom/brush interaction primitives, signal-derived scenes and Anime-driven interpolation with explicit owner disposal and cancellation behavior.
- View component retains element identity across reorder, updates only affected attributes and removes exited marks. Native tests verify real pixels, reactive updates, event delivery and disposal.
- Runnable gallery demonstrates shared primitives across Cartesian, radial and hierarchical examples; no generated HTML/CSS authoring files are required.
- Property/edge tests and cross-platform CI. Performance gates check bounded allocations, large data joins/scales/spatial lookup, and retained rendering work. Publish source and versioned packages after review and passing CI; NuGet publication depends on available authorized publisher configuration.

## Important boundaries

Safe numeric ranges are enforced at API boundaries. Empty/singleton datasets are valid. Degenerate domains use midpoint mapping and constant ticks. Descending domains/ranges work. Caller data is never mutated. Selection/zoom operates in explicit scene coordinates. Animation is deterministic under an injected clock, with no ambient timers. Rendering and signal graph mutation follow the host's owner thread. Browser/Fable support is a future adapter unless tested in this release.

The dependency cohort deliberately uses Ranvier preview.2 and CSS alpha.2 because the released Anime and View packages require those exact versions. Source snapshots record upstream commits independently from package pins.

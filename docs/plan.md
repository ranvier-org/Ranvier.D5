# Ranvier.D5 Foundation Implementation Plan

> **For agentic workers:** Use superpowers:executing-plans to implement task by task.

**Goal:** Deliver tested, reusable F# visualization foundations with a native reactive renderer.

**Architecture:** Immutable data-to-scene functions form the core. Ranvier owns reactivity and lifetimes, Anime owns easing, CSS owns typed presentation values, View owns keyed rendering, and WebCore paints native SVG.

**Tech Stack:** F#, .NET 10, Ranvier preview.2, Anime rc.1, CSS alpha.2, View rc.2, WebCore backend rc.6/native rc.7, Expecto.

**Spec:** docs/design.md

## Global Constraints

- Preserve existing worktrees and uncommitted files. Snapshot committed upstream revisions only.
- .NET 10 native first; no JavaScript runtime or D3 runtime dependency.
- Immutable public configuration; local mutation is permitted for measured algorithms.
- Exact compatible Ranvier package cohort, normal and Traced variants.
- Unit tests precede implementation; full Release and native acceptance before publication.

## Review Focus

- Empty, singleton, descending, repeated and non-finite input: explicit outcomes without infinite loops.
- Duplicate keys and reordered rows: reject ambiguity and preserve retained identity.
- Floating-point overflow and degenerate geometry: finite output or early rejection.
- Disposal and changing animation targets: no detached subscriptions or stale completion writes.
- Coordinate transforms, SVG namespaces and culture: identical scene meaning in native output and serialization.

## Tasks

### 1. Data and scales
- [ ] Add failing tests for Data summaries/bin/join and Scale mapping/inversion/ticks, empty/descending/NaN input.
- [ ] Implement Data.fs and Scale.fs with immutable captured state; returns options for missing observations/categories.
- [ ] Run all core tests, record edge cases and commit.

### 2. Geometry, scene and layouts
- [ ] Add failing tests for affine inversion, path serialization, gap splitting, full-circle arcs, pie/stack, axes, hierarchy and nearest/range search.
- [ ] Implement Point/Transform, Path/Shape, Scene/Mark, Axis, Layout and Spatial modules. Layouts produce reusable immutable geometry; scenes reject duplicate keys.
- [ ] Run tests with culture changes, empty/degenerate data and randomized invariants; commit.

### 3. Reactivity, animation and interaction
- [ ] Add failing tests for signal-derived scenes, one batched recomputation, explicit disposal, clock-driven interpolation, zoom anchoring and brush normalization.
- [ ] Implement Reactive, Transition and Interaction using Ranvier and Anime APIs. Expose pure samplers and owned effects; no independent timer.
- [ ] Verify lifetime and target-change edges; commit.

### 4. View/native integration and examples
- [ ] Add failing native tests for SVG namespace, keyed identity/reorder/exit, changed pixels, click dispatch, disposal and escaped labels.
- [ ] Implement Ranvier.D5.View scene component using existing View keyed regions and typed CSS values; mount through WebCore in tests and the native gallery.
- [ ] Render Cartesian, radial and hierarchy examples; inspect captured output and test native behavior; commit.

### 5. Performance, documentation, review and release
- [ ] Add measured gates for large joins/scales/spatial lookup and retained native rendering; record baseline and thresholds.
- [ ] Document API contracts, example composition, package compatibility and remaining specialized algorithms.
- [ ] Fresh whole-branch review; reproduce and fix material findings with tests.
- [ ] Run Windows/Linux/macOS CI, normal/Traced and native package acceptance, then publish a versioned GitHub release with packages and available NuGet publication.
- [ ] Preserve the snapshot workspace and clean completed temporary branches.

# Alpha.3 hierarchy milestone

This milestone adds task 5 of the D3 expansion plan to the normal and Traced
core, Charts and View packages. Toolkit dependencies remain pinned.

- Validated parent tables and explicit forests; iterative tidy tree, cluster and partition.
- Circle packing and slice/dice/slice-dice/binary/squarified treemaps, with caller-owned resquarify state.
- Dendrogram, radial tree, icicle, sunburst, packed-bubble and treemap recipes.
- Signal-driven selection, collapse and drill-down in a native explorer, with owned workers and keyed transitions.
- Six checked-in snapshots from working native views, reproduction commands, and cross-platform capture artifacts.

See the [native gallery](../examples/Ranvier.D5.Gallery/README.md),
[layout contracts](hierarchy.md), [tiling limitations](tiling.md) and
[interactive explorer](hierarchy-charts.md).

## Validation and performance

The local normal and Traced suites pass 120 tests. The final review added failing-then-passing regressions
for representable tiny packing radii and leaf-only weights across collapse. The release gate requires all
six Windows/Linux/macOS normal/Traced CI jobs, including native contracts,
captures, paired regression checks, packing and packed-consumer checks.
Native explorer capture verifies worker publication and real click selection
through the transition groups. Deep tests use 50,000-node chains; wide tests use
10,000 children. Coordinate fixtures pin d3-hierarchy 3.1.2 and include its license.

Development-machine medians (milliseconds; allocated bytes):

| Workload | Median | Allocation |
| --- | ---: | ---: |
| Parent-table construction, 10,000 | 0.86 | 2,728,168 |
| Tidy tree, 10,000 | 7.29 | 10,633,304 |
| Squarified treemap, 10,000 siblings | 7.50 | 17,535,280 |
| Circle pack, 10,000 siblings | 35.81 | 12,570,480 |
| Full treemap chart, 10,000 nodes | 33.96 | 40,938,704 |

These are observations, not frame-rate guarantees. Large layouts run through the
owned worker. The chart allocation gate caught a redundant tree copy and palette
conversion; fixing those reduced the measured allocation from 67.00 MB to
40.94 MB. Existing paired gates retain baseline 52ef102 and the alpha.2 thresholds.

Across the first six CI jobs, hierarchy workload p95s peaked at 6.70 ms for
construction, 25.04 ms for tidy layout, 32.49 ms for treemap, 66.00 ms for packing
and 89.23 ms for the full chart. Final absolute gates are respectively
20/60/75/150/180 ms, with allocation limits of 4/12/20/15/48 MB. These leave
runner-noise headroom while catching substantial regressions.

The final candidate exposed a paired-harness defect on Linux: unchanged scale
mapping compared roughly 0.90 ms baseline with 5.33 ms current. Three warmup calls
could leave the two processes at different asynchronous JIT tiers. The paired
harness now pins `DOTNET_TieredCompilation=0` (and its legacy alias) identically in
both benchmark subprocesses, recording those settings in comparison.json.
A local controlled run measured 1.630 ms baseline and 1.638 ms current for scale
mapping. The original baseline revision, three alternating process pairs, 60
samples and regression thresholds remain unchanged. Default-runtime absolute
benchmarks still run separately in every CI job. Paired subprocesses use
`--baseline-only` to run exactly the same five workloads; the harness rejects
mismatched workload sets. This avoids candidate-only layout/worker workloads
changing GC pressure or being rerun under a different optimization mode. The
full default-runtime suite still applies every new-workload absolute gate.
This isolates steady optimized
throughput; it does not measure application startup or dynamic PGO benefits.
See [.NET compilation settings](https://learn.microsoft.com/en-us/dotnet/core/runtime-config/compilation).

The controlled gate also exposed avoidable native attribute work. Renderer rows
retain an owner-local appearance cache: geometry-only updates reuse serialized
style strings, while fill, stroke, opacity, font, transform and definition changes
invalidate it. Native tests verify style, font and transform updates. The cache
is read inside the existing attribute bindings rather than adding a reactive
memo per mark: the additional graph bookkeeping regressed macOS Traced CI.
A local three-pair Traced comparison after removing that memo measured 6.830 ms
baseline versus 6.759 ms current, with allocation reduced from 2,171,384 to
1,974,056 bytes. The original regression thresholds remain unchanged.

Packing uses a conservative enclosing circle, not D3's minimum-circle output;
fitted padding is approximate. Cancellation occurs around the pure hierarchy
kernel; kernels do not poll a token during traversal. Tasks 6–11 remain open;
this milestone makes no claim of full D3 parity.

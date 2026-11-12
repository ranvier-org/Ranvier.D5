# Performance gates and evidence

CI runs normal and Traced workloads on Windows, Linux and macOS. Absolute workload
limits remain in the benchmark source. Each process uses three warmup calls and
60 measured calls; it reports median time, p95 time and median allocated bytes.
These are observed warmed workloads under the default runtime, not a guarantee
that asynchronous runtime tier promotion has finished or a cold-start measurement.

The paired comparison runs the alpha.1 baseline and current implementation in
three fresh-process pairs, alternating execution order. Both sides disable tiered
compilation to compare fully optimized code at the same compilation stage. This
runtime setting applies only to the paired comparison, not the absolute suite.

The comparison retains each baseline/current pair when calculating changes.
Timing fails when the median within-pair increase exceeds 2 ms and the median
within-pair ratio exceeds 1.20. Allocation fails when the median within-pair
increase exceeds 65,536 bytes and the median within-pair ratio exceeds 1.10.
For a zero baseline, a positive current value exceeds the relative limit;
the absolute limit still applies. Undefined infinite ratios are recorded as null
in JSON, never nonstandard Infinity values. Missing or invalid measurements fail
validation rather than becoming a passing comparison.

The earlier comparator independently took the median of each side, discarding
the pairing when applying limits. With runner-speed drift it could report a
regression absent from the typical matched pair, or miss a regression present in
two of three matched pairs. Regression tests cover both failure modes, deadband
boundaries, outliers, zero baselines and malformed evidence. The numeric limits,
workloads and sample counts have not changed.

The benchmark also writes a sibling `.stages.json` diagnostic file. `Samples`
contains retained-500-mark timings; `NetworkSamples` contains the 1,000-node,
2,000-link network timings (empty for baseline-only runs). Columns separately
record scene construction, signal updates, host cycling, painting, and generation
0/1/2 collection counts. The first three rows are warmups. These diagnostics do
not replace or relax the total-time and allocation gates.

Runtime diagnostics accompany these phases in RuntimeSamples and
NetworkRuntimeSamples, with columns in RuntimeColumns: process-wide GC pause
time, total JIT compilation time, calling-thread JIT compilation time, current
managed heap size, and the graph trace sequence count (zero in normal builds).
The trace count grows with retained history because these workloads do not
checkpoint their diagnostic logs. These counters help distinguish runtime and
history costs; overlapping durations must not be subtracted from wall-clock
timings. The probes remain inside the measured action, and gates still evaluate
the whole action without adjustment.

```sh
python -m unittest discover -s tools -p test_benchmark_comparison.py
python tools/compare-benchmarks.py --traced true
dotnet run --project benchmarks/Ranvier.D5.Benchmarks -c Release -- artifacts/benchmarks.json
```

Preserve failed-run artifacts. A passing repeat alone is not proof of a code fix;
use matched evidence and the phase diagnostics to distinguish renderer work,
collection, compilation and host variability. Run the amended head through all
cross-platform checks before merging or releasing.

The retained renderer caches transform serialization separately from paint/text
appearance and reuses the composed-leaf adapter for repeated reads of the same
immutable node. Caches remain local to the mounted row and always read the reactive
source before checking their keys. This preserves dependency tracking, keyed DOM
identity and live event paths.

Three alternating full-workload process pairs on Windows Traced reduced network
update allocation by 1,104,880 bytes (about 8.3 percent) and flow update allocation
by 528,880 bytes. The paired network median-time ratio was 0.981; this is an
allocation improvement, not evidence of a large latency improvement or a macOS
tail-latency fix. The network allocation gate is tightened from 32 MB to 13 MB;
the 100 ms p95 gate and paired comparison tolerances remain unchanged.

## Native dependency update

The alpha.6 candidate updates View rc.2 to rc.3, View.WebCore rc.6 to rc.8,
WebCore native rc.7 to rc.8, and SDL rc.10 to rc.12. Anime rc.2 remains the
latest published native package and retains its exact Ranvier/CSS dependencies.

Three alternating Windows Traced full-suite process pairs measured a network
median-time ratio of 0.9025 (-2.563 ms), flow ratio 0.9730 (-0.294 ms), and
retained-500 ratio 1.1968 (+1.790 ms). Allocations were unchanged. All six
absolute suites passed. Because every full-suite retained pair was slower, a
focused follow-up ran three pairs under each runtime policy: default tiering
measured ratio 1.0208 (+0.207 ms), and fully optimized code measured 1.0155
(+0.081 ms), again with unchanged allocations. Both passed the existing paired
tolerance. These measurements support compatibility within the performance
budget, not a universal speedup or a proven explanation for runner variability.
The native release also targets hover repainting; these scene-update workloads
do not independently establish its pointer-motion improvement.

The unbounded many-body force path skips nearest/farthest cell-distance cutoff
calculations when MaxDistance is positive infinity. Finite cutoffs retain their
existing exact traversal checks. Three alternating Windows Traced focused pairs
measured median-time ratios of 0.7346 under default tiering and 0.8405 with fully
optimized code, with unchanged allocations and bit-identical output positions
and velocities for the 1,000-node/2,000-link fixture. The default-runtime samples
remain noisy; this is measured kernel headroom, not proof that hosted-runner tail
variation is eliminated. The 25 ms spatial-force p95 gate is unchanged.

Force passes also reuse one local traversal stack across all node queries instead
of allocating a stack per query. Against the original force kernel, three further
alternating Windows Traced pairs measured median-time ratios of 0.6774 under
default tiering and 0.7924 with fully optimized code. Default timings remained
noisy, with one pair slower. All twelve processes produced bit-identical positions
and velocities. Allocation fell from 1,690,016 to 913,912 bytes per step (45.9
percent), independently enforced by a regression test that failed before reuse.
The fixture's allocation gate is tightened from 8 MB to 1 MB; its 25 ms p95 gate,
workload and sampling policy remain unchanged.

Retained rendering also caches the option wrappers used by appearance and
transform attributes. Three further alternating Windows Traced pairs reduced
allocation by 180,000 bytes per 500-mark update. Default-tiering median time
ratio was 0.9002 (-0.966 ms); fully optimized ratio was 0.9422 (-0.334 ms).
The focused allocations were 1,886,448 and 1,854,280 bytes respectively. The
retained allocation gate tightens from 32 MB to 2 MB, with unchanged timing
limits and paired tolerances. These local results do not establish the macOS
result; all platform checks remain release requirements.

Hexbin region picking scanned every target and rebuilt each hexagon's bounds
per query. The recipe now narrows each target's hit clip to its hexagon bounds
intersected with the plot, so a non-matching target is rejected by four
comparisons; recipes that set no clip keep the plot bounds. Three alternating
same-machine runs of the 100-query workload measured medians of 9.6–14.5 ms
before and 1.9–2.2 ms after. Picking semantics are unchanged and covered by a
test; the 100 ms p95 gate is unchanged.

Geographic resampling previously rebuilt each half of a subdivided arc from its
rounded degree endpoints. Halves now rotate the parent's great-circle frame,
removing most trigonometry from the recursion while producing the same vertex
counts and passing the pinned D3 fixtures. In two alternating runs the
10,000-vertex Mercator polar workload measured 49.6–50.9 ms before and
34.0–34.1 ms after; the equirectangular variant measured 46.5–46.9 and
19.1–20.5 ms. The 150 ms p95 gate is unchanged.

Short geographic arcs no longer solve their great circle when a cheap bound
already settles them: the distance along a meridian and then a parallel
(|Δlatitude| + |Δlongitude|) never undercuts the great-circle distance, and both
the Mercator band test and the cylindrical precision test are monotone in arc
length. Two alternating same-machine runs measured the 100,000-vertex Mercator
line at 28.1–28.6 ms before and 13.6 ms after, the equirectangular line at
21.3–22.0 and 9.9–10.8 ms, and the seam-heavy line at 85.7–87.7 and
53.2–65.1 ms. Outputs and gates are unchanged; this follows a merged-main
Windows Traced p95 tail of 111.6 ms against the 100 ms Mercator line gate.

Both benchmark harnesses now run a full collection between samples, outside the
timed region, so a sample is not charged for garbage left by the previous one.
The paired comparison applies the same change to the alpha.1 baseline harness, so
both sides measure alike. Allocation-heavy workloads on macOS runners, whose
small gen0 budget made p95 tails rotate between unrelated gates (for example
treemap at a 22.6 ms median and 89.4 ms p95), motivated the change. A local
paired run after it measured join 0.848 to 0.848 ms and line 2.081 to 2.047 ms.
Gates and workloads are unchanged.

Traced builds retain every trace event in a per-graph log that grows by doubling.
Every native workload used to share one graph, so the network workload inherited
about 750,000 events from earlier workloads and its samples straddled the 2^20
and 2^21 buffer doublings. On macOS Traced runners the worst network samples
(121–332 ms) landed exactly at those crossings, with heap growth from 140 to
380 MB, while GC and JIT time did not explain them. The network and flow paint
workloads now run in their own graphs, as an application would; their log
starts at the mount's 140,000 events. The local Traced network p95 measured
36.9 ms. Gates, workloads and normal builds are unchanged.

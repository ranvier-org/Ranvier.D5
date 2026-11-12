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

```sh
python -m unittest discover -s tools -p test_benchmark_comparison.py
python tools/compare-benchmarks.py --traced true
dotnet run --project benchmarks/Ranvier.D5.Benchmarks -c Release -- artifacts/benchmarks.json
```

Preserve failed-run artifacts. A passing repeat alone is not proof of a code fix;
use matched evidence and the phase diagnostics to distinguish renderer work,
collection, compilation and host variability. Run the amended head through all
cross-platform checks before merging or releasing.

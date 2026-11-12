"""Comparison of matched fresh-process benchmark samples."""
import math
import statistics


def _paired(before, after, relative_limit, absolute_limit):
    delta = statistics.median(b - a for a, b in zip(before, after))
    relative_votes = sum(b > a * relative_limit for a, b in zip(before, after))
    ratios = [b / a if a else (1.0 if b == 0 else math.inf) for a, b in zip(before, after)]
    ratio = statistics.median(ratios)
    return delta, ratio if math.isfinite(ratio) else None, relative_votes > len(before) // 2 and delta > absolute_limit


def compare_samples(samples):
    if set(samples) != {"baseline", "current"}:
        raise ValueError("Expected baseline and current process samples")
    count = len(samples["baseline"])
    if count < 3 or count % 2 == 0 or len(samples["current"]) != count:
        raise ValueError("Expected an odd number of at least three matched process pairs")
    names = set(samples["baseline"][0])
    if not names:
        raise ValueError("Expected measured workloads")
    for runs in samples.values():
        for sample in runs:
            if set(sample) != names:
                raise ValueError("All pairs must measure the same workloads")
            for value in sample.values():
                time, allocation = value.get("MedianMs"), value.get("MedianBytes")
                if isinstance(time, bool) or not isinstance(time, (int, float)) or not math.isfinite(time) or time < 0:
                    raise ValueError("Elapsed times must be finite and nonnegative")
                if isinstance(allocation, bool) or not isinstance(allocation, int) or not 0 <= allocation <= 2**63 - 1:
                    raise ValueError("Allocated bytes must be nonnegative signed 64-bit integers")
    rows = []
    for name in samples["baseline"][0]:
        times = {side: [sample[name]["MedianMs"] for sample in samples[side]] for side in samples}
        allocations = {side: [sample[name]["MedianBytes"] for sample in samples[side]] for side in samples}
        time_delta, time_ratio, time_fail = _paired(times["baseline"], times["current"], 1.2, 2)
        byte_delta, byte_ratio, byte_fail = _paired(allocations["baseline"], allocations["current"], 1.1, 65536)
        rows.append(dict(name=name,
                         baselineMs=statistics.median(times["baseline"]), currentMs=statistics.median(times["current"]),
                         baselineBytes=statistics.median(allocations["baseline"]), currentBytes=statistics.median(allocations["current"]),
                         pairedTimeDeltaMs=time_delta, pairedTimeRatio=time_ratio,
                         pairedAllocationDeltaBytes=byte_delta, pairedAllocationRatio=byte_ratio,
                         timeRegression=time_fail, allocationRegression=byte_fail))
    return rows

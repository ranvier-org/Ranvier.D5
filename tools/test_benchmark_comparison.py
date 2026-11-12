import json
import math
import unittest

from benchmark_comparison import compare_samples


def samples(before, after, before_bytes=None, after_bytes=None):
    def side(times, allocations):
        return [{"native": {"MedianMs": t, "MedianBytes": b}} for t, b in zip(times, allocations or [1000] * len(times))]
    return {"baseline": side(before, before_bytes), "current": side(after, after_bytes)}


class PairedComparisonTests(unittest.TestCase):
    def test_preserves_matched_pairs_during_runner_speed_drift(self):
        row = compare_samples(samples([8.77735, 7.07965, 5.50805], [10.22395, 9.6759, 6.33135]))[0]
        self.assertFalse(row["timeRegression"])

    def test_detects_paired_regression_hidden_by_equal_separate_medians(self):
        row = compare_samples(samples([10, 20, 30], [20, 10, 40]))[0]
        self.assertTrue(row["timeRegression"])

    def test_allocation_comparison_also_preserves_pairs(self):
        row = compare_samples(samples([10]*3, [10]*3, [100000, 200000, 300000], [180000, 100000, 380000]))[0]
        self.assertTrue(row["allocationRegression"])

    def test_one_slow_pair_does_not_override_the_other_two(self):
        self.assertFalse(compare_samples(samples([10]*3, [10, 10, 100]))[0]["timeRegression"])

    def test_time_gate_keeps_relative_and_absolute_deadbands(self):
        for before, after, expected in [(10, 12, False), (20, 24, False), (1, 2, False), (10, 12.1, True)]:
            with self.subTest(before=before, after=after):
                self.assertEqual(compare_samples(samples([before]*3, [after]*3))[0]["timeRegression"], expected)

    def test_allocation_gate_keeps_both_deadbands(self):
        for before, after, expected in [(1000000, 1100000, False), (1000, 66536, False), (1000, 66537, True)]:
            with self.subTest(before=before, after=after):
                row = compare_samples(samples([10]*3, [10]*3, [before]*3, [after]*3))[0]
                self.assertEqual(row["allocationRegression"], expected)

    def test_zero_baseline_is_finite_json_and_detects_large_regression(self):
        rows = compare_samples(samples([0]*3, [3]*3, [0]*3, [70000]*3))
        self.assertTrue(rows[0]["timeRegression"])
        self.assertTrue(rows[0]["allocationRegression"])
        json.dumps(rows, allow_nan=False)

    def test_rejects_unmatched_pairs_and_missing_workloads(self):
        for data in [samples([10]*3, [10]*2), samples([10], [10]), {"baseline":[{"a":{"MedianMs":10,"MedianBytes":1}}]*3,"current":[{"b":{"MedianMs":10,"MedianBytes":1}}]*3}]:
            with self.subTest(data=data), self.assertRaises(ValueError):
                compare_samples(data)

    def test_rejects_invalid_measurements(self):
        for value in [math.nan, math.inf, -1]:
            with self.subTest(value=value), self.assertRaises(ValueError):
                compare_samples(samples([10]*3, [10, value, 10]))
        with self.assertRaises(ValueError):
            compare_samples(samples([10]*3, [10]*3, [1000]*3, [1000, -1, 1000]))


if __name__ == "__main__":
    unittest.main()

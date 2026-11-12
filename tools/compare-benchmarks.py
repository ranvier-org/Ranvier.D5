"""Three fresh-process paired runs against alpha.1; only harness sampling and between-sample collection change."""
import argparse
import io
import json
import os
import pathlib
import platform
from benchmark_comparison import compare_samples
import subprocess
import zipfile

parser = argparse.ArgumentParser()
parser.add_argument("--traced", choices=["true", "false"], default="false")
args = parser.parse_args()
root = pathlib.Path(__file__).resolve().parents[1]
out = root / "artifacts" / ("comparison-" + args.traced)
out.mkdir(parents=True, exist_ok=True)
baseline = out / "baseline"
baseline.mkdir(exist_ok=True)
revision = "52ef102"
archive = subprocess.check_output(["git", "archive", "--format=zip", revision], cwd=root)
with zipfile.ZipFile(io.BytesIO(archive)) as source:
    for name in source.namelist():
        if not (baseline / name).resolve().is_relative_to(baseline.resolve()):
            raise ValueError("Archive path escaped baseline directory")
    source.extractall(baseline)
program = baseline / "benchmarks/Ranvier.D5.Benchmarks/Program.fs"
text = program.read_text(encoding="utf-8")
for old, new in [("Array.init 15", "Array.init 60"), ("MedianMs=times[7]", "MedianMs=(times[29]+times[30])/2."),
                 ("P95Ms=times[14]", "P95Ms=times[56]"), ("MedianBytes=allocations[7]", "MedianBytes=allocations[30]"),
                 # Mirror the current harness: collect between samples, outside the timed region.
                 ("            let allocated = GC.GetAllocatedBytesForCurrentThread()",
                  "            GC.Collect()\n            GC.WaitForPendingFinalizers()\n            GC.Collect()\n"
                  "            let allocated = GC.GetAllocatedBytesForCurrentThread()")]:
    if text.count(old) != 1:
        raise ValueError("Unexpected baseline harness: " + old)
    text = text.replace(old, new)
program.write_text(text, encoding="utf-8")
project = "benchmarks/Ranvier.D5.Benchmarks"
variant = "-p:RanvierTrace=" + args.traced

# Pin the same optimization stage in both subprocesses. Three warmup calls do
# not guarantee that asynchronous tier promotion has completed on either side.
benchmark_settings = {"DOTNET_TieredCompilation": "0", "COMPlus_TieredCompilation": "0"}
benchmark_environment = dict(os.environ, **benchmark_settings)


def run(command, cwd, log, environment=None):
    with log.open("w", encoding="utf-8") as stream:
        result = subprocess.run(command, cwd=cwd, stdout=stream, stderr=subprocess.STDOUT, timeout=300, env=environment)
    if result.returncode:
        print(log.read_text(encoding="utf-8"))
        raise SystemExit(result.returncode)

for label, directory in [("baseline", baseline), ("current", root)]:
    run(["dotnet", "build", project, "-c", "Release", variant], directory, out / (label + "-build.log"))

samples = {"baseline": [], "current": []}
for pair in range(3):
    # Alternate ordering to reduce systematic thermal/order bias.
    ordering = [("baseline", baseline), ("current", root)]
    if pair % 2:
        ordering.reverse()
    for label, directory in ordering:
        target = out / (label + "-" + str(pair) + ".json")
        run(["dotnet", "run", "--project", project, "-c", "Release", variant, "--no-build", "--", str(target), "--baseline-only"],
            directory, out / (label + "-" + str(pair) + ".log"), benchmark_environment)
        samples[label].append({item["Name"]: item for item in json.loads(target.read_text())})

expected_names = set(samples["baseline"][0])
rows = compare_samples(samples)
failed = any(row["timeRegression"] or row["allocationRegression"] for row in rows)
for row in rows:
    print(f"{row['name']}: paired change {row['pairedTimeDeltaMs']:.3f} ms, ratio {row['pairedTimeRatio']}; "
          f"allocation change {row['pairedAllocationDeltaBytes']} bytes; "
          f"median levels {row['baselineMs']:.3f} -> {row['currentMs']:.3f} ms")
report = dict(baseline=revision, current=subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip(),
              platform=platform.platform(), traced=args.traced, pairs=3, samplesPerProcess=60,
              runtimeSettings=benchmark_settings, optimizationMode="fully-optimized-without-tiering",
              comparisonEstimator="median-within-pair-changes",
              thresholds=dict(timeRatio=1.2, timeDeltaMs=2, allocationRatio=1.1, allocationDeltaBytes=65536),
              workloads=sorted(expected_names),
              passed=not failed, results=rows)
(out / "comparison.json").write_text(json.dumps(report, indent=2), encoding="utf-8")
raise SystemExit(1 if failed else 0)

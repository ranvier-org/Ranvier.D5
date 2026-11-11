"""Recreate the committed kit snapshot beside an existing D5 checkout."""
import json
import pathlib
import subprocess
import sys

repository = pathlib.Path(__file__).resolve().parents[1]
destination = pathlib.Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else repository.parent / "snapshots"
destination.mkdir(parents=True, exist_ok=True)
manifest = json.loads((repository / "kit.lock.json").read_text(encoding="utf-8-sig"))
for item in manifest["repositories"]:
    target = destination / item["name"]
    if target.exists():
        raise SystemExit(f"Refusing to replace existing snapshot: {target}")
    subprocess.run(["git", "init", str(target)], check=True)
    subprocess.run(["git", "-C", str(target), "remote", "add", "origin", item["url"]], check=True)
    subprocess.run(["git", "-C", str(target), "fetch", "--depth", "1", "origin", item["commit"]], check=True)
    subprocess.run(["git", "-C", str(target), "checkout", "--detach", "FETCH_HEAD"], check=True)
print(f"Kit revisions restored to {destination}")

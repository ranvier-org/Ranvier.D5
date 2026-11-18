#!/usr/bin/env python3
"""Checks theorem documentation comments in proofs/theories/. Run from proofs/.

  guarantees.py comments        fails on a Theorem or Corollary with no (**md comment and bold title directly above
"""
import re
import sys
from pathlib import Path

DECLARATION = re.compile(r"^\s*(?:Theorem|Corollary)\s+([A-Za-z0-9_']+)")
TITLE = re.compile(r"^\*\*(.+)\*\*\s*$")


def source_files():
    lines = Path("_CoqProject").read_text(encoding="utf-8").splitlines()
    return [Path(line) for line in lines if line.startswith("theories/") and line.endswith(".v")]


def title_above(lines, i):
    """Bold title of the (**md comment ending on the line directly above line i, or None."""
    k = i - 1
    if k < 0 or not lines[k].rstrip().endswith("*)"):
        return None
    while k >= 0 and not lines[k].lstrip().startswith("(*"):
        k -= 1
    if k < 0 or not lines[k].lstrip().startswith("(**md"):
        return None
    first = lines[k].lstrip()[len("(**md"):].strip() or lines[k + 1].strip()
    match = TITLE.match(first)
    return match.group(1) if match else None


def check_comments():
    bad = []
    for path in source_files():
        lines = path.read_text(encoding="utf-8").split("\n")
        for i, line in enumerate(lines):
            match = DECLARATION.match(line)
            if match and title_above(lines, i) is None:
                bad.append(f"{path}: {match.group(1)}")
    for entry in bad:
        print(f"site.sh: no (**md comment with a bold title directly above {entry}", file=sys.stderr)
    return not bad


def main():
    if len(sys.argv) < 2 or sys.argv[1] != "comments":
        sys.exit("guarantees.py: expected command 'comments'")
    sys.exit(0 if check_comments() else 1)


main()

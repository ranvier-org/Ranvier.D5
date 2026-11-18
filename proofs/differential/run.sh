#!/usr/bin/env bash
# Differential conformance check for Ranvier.D5:
#   bash proofs/differential/run.sh [count] [seed]
#
# Generates randomized programs across all 6 verified D5/Charts/Geo/View runners, executes
# them against the real F# assemblies (Generate.fsx -> Harness.fsx) and against the OCaml code
# extracted from Rocq (differential/engine.ml + driver.ml), and verifies exact agreement.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(uname -s)" != "Linux" ]; then
  distro="${RANVIER_WSL_DISTRO:-Ubuntu}"
  wsl_here="$(MSYS_NO_PATHCONV=1 wsl.exe -d "$distro" -- wslpath -u "$(cygpath -m "$here")" | tr -d '\r')"
  exec env MSYS_NO_PATHCONV=1 wsl.exe -d "$distro" -- bash "$wsl_here/$(basename "${BASH_SOURCE[0]}")" "$@"
fi

count="${1:-400}"
seed="${2:-1}"

if ! command -v ocamlopt >/dev/null 2>&1; then
  export PATH="$HOME/.local/bin:$PATH"
  if command -v opam >/dev/null 2>&1; then
    eval "$(opam env --switch=ranvier-proofs --set-switch)"
  fi
fi
command -v ocamlopt >/dev/null 2>&1 || { echo "run.sh: no ocamlopt found" >&2; exit 1; }

find_dotnet() {
  if [ -n "${RANVIER_DOTNET:-}" ]; then echo "$RANVIER_DOTNET"; return 0; fi
  if command -v dotnet >/dev/null 2>&1; then command -v dotnet; return 0; fi
  if command -v dotnet.exe >/dev/null 2>&1; then command -v dotnet.exe; return 0; fi
  if [ -x "/mnt/c/Program Files/dotnet/dotnet.exe" ]; then echo "/mnt/c/Program Files/dotnet/dotnet.exe"; return 0; fi
  return 1
}
dotnet_cmd="$(find_dotnet)" || { echo "run.sh: no dotnet found" >&2; exit 1; }

cd "$here/.."
work="$(mktemp -d -p differential .run.XXXXXX)"
trap 'rm -rf "$work"' EXIT

"$dotnet_cmd" build ../src/Ranvier.D5.Charts/Ranvier.D5.Charts.fsproj -c Release -p:CopyLocalLockFileAssemblies=true --nologo -v q >"$work/build.log" 2>&1 \
  || { cat "$work/build.log" >&2; echo "run.sh: dotnet build of Ranvier.D5.Charts failed" >&2; exit 1; }
"$dotnet_cmd" build ../src/Ranvier.D5.Geo/Ranvier.D5.Geo.fsproj -c Release -p:CopyLocalLockFileAssemblies=true --nologo -v q >>"$work/build.log" 2>&1 \
  || { cat "$work/build.log" >&2; echo "run.sh: dotnet build of Ranvier.D5.Geo failed" >&2; exit 1; }
"$dotnet_cmd" build ../src/Ranvier.D5.View/Ranvier.D5.View.fsproj -c Release -p:CopyLocalLockFileAssemblies=true --nologo -v q >>"$work/build.log" 2>&1 \
  || { cat "$work/build.log" >&2; echo "run.sh: dotnet build of Ranvier.D5.View failed" >&2; exit 1; }

cp differential/engine.ml differential/engine.mli differential/driver.ml "$work/"
(cd "$work" && ocamlopt -w -a engine.mli engine.ml driver.ml -o driver) \
  || { echo "run.sh: building OCaml driver failed" >&2; exit 1; }

"$dotnet_cmd" fsi --exec differential/Generate.fsx -- "$count" "$seed" "$work" >"$work/fsi.log" 2>&1 \
  || { cat "$work/fsi.log" >&2; echo "run.sh: Generate.fsx failed at seed $seed" >&2; exit 1; }

tr -d '\r' <"$work/programs.txt" >"$work/programs_lf.txt"
"$work/driver" <"$work/programs_lf.txt" >"$work/ocaml.txt" \
  || { echo "run.sh: OCaml driver failed at seed $seed" >&2; exit 1; }

tr -d '\r' <"$work/fsharp.txt" >"$work/fsharp_lf.txt"

if ! diff -u "$work/fsharp_lf.txt" "$work/ocaml.txt" >"$work/diff.txt"; then
  first_idx="$(awk '/^-[0-9]+\tok\t/{ sub(/^-/,""); print $1; exit }' "$work/diff.txt")"
  if [ -n "$first_idx" ]; then
    prog="$(sed -n "$((first_idx + 1))p" "$work/programs_lf.txt")"
    fs_line="$(sed -n "$((first_idx + 1))p" "$work/fsharp_lf.txt")"
    ml_line="$(sed -n "$((first_idx + 1))p" "$work/ocaml.txt")"
    printf 'run.sh: mismatch at seed %s, program %s\nprogram: %s\nF#:      %s\nRocq:    %s\n' \
      "$seed" "$first_idx" "$prog" "$fs_line" "$ml_line" >&2
  else
    head -40 "$work/diff.txt" >&2
  fi
  exit 1
fi

tr -d '\r' <"$work/fsi.log"
echo "differential: $count/$count matched (seed $seed)"

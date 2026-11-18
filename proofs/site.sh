#!/usr/bin/env bash
# Builds the Navigator site of the proofs into site/output. Run as `bash proofs/site.sh` from any directory and any shell.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(uname -s)" != "Linux" ]; then
  distro="${RANVIER_WSL_DISTRO:-Ubuntu}"
  wsl_here="$(MSYS_NO_PATHCONV=1 wsl.exe -d "$distro" -- wslpath -u "$(cygpath -m "$here")" | tr -d '\r')"
  exec env MSYS_NO_PATHCONV=1 WSLENV="PROOFS_BASE_URL:RANVIER_DOTNET" wsl.exe -d "$distro" -- bash "$wsl_here/$(basename "${BASH_SOURCE[0]}")" "$@"
fi

cd "$here"

find_dotnet() {
  if [ -n "${RANVIER_DOTNET:-}" ]; then echo "$RANVIER_DOTNET"; return 0; fi
  if command -v dotnet >/dev/null 2>&1; then command -v dotnet; return 0; fi
  if command -v dotnet.exe >/dev/null 2>&1; then command -v dotnet.exe; return 0; fi
  if [ -x "/mnt/c/Program Files/dotnet/dotnet.exe" ]; then echo "/mnt/c/Program Files/dotnet/dotnet.exe"; return 0; fi
  return 1
}
dotnet_cmd="$(find_dotnet)" || { echo "site.sh: no dotnet found" >&2; exit 1; }

case "$dotnet_cmd" in
  *.exe)
    rocq="wsl.exe -d ${WSL_DISTRO_NAME:-Ubuntu} -- bash $here/build.sh --run rocq"
    export WSLENV="${WSLENV:+$WSLENV:}PROOFS_BASE_URL"
    ;;
  *) rocq="bash $here/build.sh --run rocq" ;;
esac

log="$(mktemp)"
trap 'rm -f "$log"' EXIT
bash ./build.sh >"$log" 2>&1 || { cat "$log" >&2; echo "site.sh: build failed" >&2; exit 1; }

python3 docs/guarantees.py comments

cd site
mkdir -p .work
"$dotnet_cmd" build -c Release --nologo -v q
"$dotnet_cmd" run -c Release --no-build -- rocq-capture --rocq "$rocq" --out .work/facts.json
"$dotnet_cmd" run -c Release --no-build -- build --strict

echo "site.sh: wrote $here/site/output/index.html"

#!/usr/bin/env bash
# Builds the Rocq project. Run as `bash proofs/build.sh` from any directory and any shell.
#
#   build.sh                 generate Makefile.rocq from _CoqProject and build in parallel
#   build.sh --run CMD ARGS  run CMD in the project directory with the toolchain environment
#
# The toolchain lives in WSL (see VERSIONS.md). Outside Linux this script re-executes itself in
# the WSL distribution named by RANVIER_WSL_DISTRO (default: Ubuntu).
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(uname -s)" != "Linux" ]; then
  distro="${RANVIER_WSL_DISTRO:-Ubuntu}"
  wsl_here="$(MSYS_NO_PATHCONV=1 wsl.exe -d "$distro" -- wslpath -u "$(cygpath -m "$here")" | tr -d '\r')"
  exec env MSYS_NO_PATHCONV=1 wsl.exe -d "$distro" -- bash "$wsl_here/$(basename "${BASH_SOURCE[0]}")" "$@"
fi

export PATH="$HOME/.local/bin:$PATH"
gmp="$HOME/.local/gmp"
if [ -d "$gmp/lib" ]; then
  export CPATH="$gmp/include${CPATH:+:$CPATH}"
  export LIBRARY_PATH="$gmp/lib${LIBRARY_PATH:+:$LIBRARY_PATH}"
  export LD_LIBRARY_PATH="$gmp/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
fi
export OPAMYES=1
eval "$(opam env --switch=ranvier-proofs --set-switch)"

cd "$here"

if [ "${1:-}" = "--run" ]; then
  shift
  exec "$@"
fi

rocq makefile -f _CoqProject -o Makefile.rocq
set +e
make -f Makefile.rocq -j"$(nproc)" 2>&1 \
  | { grep -v -E "(modification time .* in the future|[Cc]lock skew detected)" || true; }
exit "${PIPESTATUS[0]}"

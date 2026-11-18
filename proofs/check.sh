#!/usr/bin/env bash
# Audits the Ranvier.D5 Rocq project. Run as `bash proofs/check.sh` from any directory and any shell.
#
# Exits 0 when:
#   1. `build.sh` compiles all theories cleanly;
#   2. every `Print Assumptions` in `theories/Audit.v` is `Closed under the global context` (or allowlisted);
#   3. no file under `theories/` contains `Admitted`, `admit`, `give_up`, `Admit Obligations`, or a top-level
#      `Axiom`, `Parameter`, or `Conjecture`;
#   4. every `exp_NAME` in `theories/Conformance/Expected.v` has a matching `Example check_NAME` in `Check.v`;
#   5. regenerating `theories/Conformance/Expected.v` from real F# runs leaves it byte-for-byte unchanged;
#   6. extracted `engine.ml` and `engine.mli` match the committed copies in `differential/`;
#   7. `differential/run.sh 400 1` exits 0.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ "$(uname -s)" != "Linux" ]; then
  distro="${RANVIER_WSL_DISTRO:-Ubuntu}"
  wsl_here="$(MSYS_NO_PATHCONV=1 wsl.exe -d "$distro" -- wslpath -u "$(cygpath -m "$here")" | tr -d '\r')"
  exec env MSYS_NO_PATHCONV=1 wsl.exe -d "$distro" -- bash "$wsl_here/$(basename "${BASH_SOURCE[0]}")" "$@"
fi

cd "$here"

ALLOWED_ASSUMPTIONS=()

fail=0
note() { echo "check.sh: $*" >&2; fail=1; }

is_allowed() {
  local n="$1" a
  for a in "${ALLOWED_ASSUMPTIONS[@]+"${ALLOWED_ASSUMPTIONS[@]}"}"; do [ "$a" = "$n" ] && return 0; done
  return 1
}

# 1. Build.
bash ./build.sh || { echo "check.sh: build failed" >&2; exit 1; }

# 2. Assumptions.
out="$(mktemp)"
trap 'rm -f "$out"' EXIT
bash ./build.sh --run rocq compile -Q theories RanvierD5 theories/Audit.v >"$out" 2>&1 \
  || { cat "$out" >&2; echo "check.sh: Audit.v failed to compile" >&2; exit 1; }

expected="$(grep -cE '^[[:space:]]*Print Assumptions ' theories/Audit.v || true)"
closed="$(grep -c '^Closed under the global context' "$out" || true)"
opened="$(grep -c '^Axioms:' "$out" || true)"
if [ "$((closed + opened))" -ne "$expected" ]; then
  note "Audit.v has $expected Print Assumptions but output has $((closed + opened)) results"
fi
assumption_names="$(awk '/^Axioms:/{a=1; next} /^Closed under/{a=0; next}
              a && /^[^[:space:]]/{ n=$1; print n }' "$out")" \
  || { echo "check.sh: could not read assumptions from Audit.v output" >&2; exit 1; }
while IFS= read -r name; do
  [ -n "$name" ] || continue
  is_allowed "$name" || note "assumption not allowlisted: $name"
done <<<"$assumption_names"

# 3. Forbidden incomplete proof or axiom constructs.
scan_awk="$(cat <<'AWK'
function strip_prefix(s,   again) {
  sub(/^[[:space:]]+/, "", s)
  again = 1
  while (again) {
    again = 0
    if (match(s, /^#\[[^]]*\][[:space:]]*/)) { s = substr(s, RLENGTH + 1); again = 1 }
    else if (match(s, /^(Local|Global|Program|Polymorphic|Monomorphic|Private|Cumulative|NonCumulative)[[:space:]]+/)) {
      s = substr(s, RLENGTH + 1); again = 1 }
  }
  return s
}
function ident_after(s,   t) {
  t = s
  sub(/^[A-Za-z]+/, "", t)
  sub(/^[[:space:]]+/, "", t)
  sub(/[^A-Za-z0-9_'.].*$/, "", t)
  return t
}
{
  s = strip_prefix($0)
  if (s ~ /^(Lemma|Theorem|Corollary|Proposition|Fact|Remark|Definition|Example|Instance|Fixpoint|CoFixpoint|Record|Inductive|CoInductive|Variant|Structure|Class)([^A-Za-z0-9_']|$)/) {
    decl = ident_after(s)
  }
  if (s ~ /^(Parameters?|Conjectures?)([^A-Za-z0-9_']|$)/) {
    printf "%s:%d\t%s\n", file, NR, ident_after(s)
  } else if ($0 ~ /(^|[^A-Za-z0-9_'])Axioms?([^A-Za-z0-9_']|$)/) {
    t = $0; sub(/^.*Axioms?[[:space:]]+/, "", t); sub(/[^A-Za-z0-9_'.].*$/, "", t)
    printf "%s:%d\t%s\n", file, NR, t
  } else if ($0 ~ /(^|[^A-Za-z0-9_'])(Admitted|admit|give_up)([^A-Za-z0-9_']|$)/ ||
             $0 ~ /(^|[^A-Za-z0-9_'])Admit[[:space:]]+Obligations([^A-Za-z0-9_']|$)/) {
    printf "%s:%d\t%s\n", file, NR, decl
  }
}
AWK
)"

v_files="$(find theories -name '*.v' | sort)"
[ -n "$v_files" ] || { echo "check.sh: no .v files found under theories" >&2; exit 1; }

scan_hits=""
while IFS= read -r f; do
  file_hits="$(awk -v file="$f" "$scan_awk" "$f")"
  [ -z "$file_hits" ] || scan_hits+="$file_hits"$'\n'
done <<<"$v_files"

while IFS=$'\t' read -r loc name; do
  [ -n "$loc" ] || continue
  note "$loc: Admitted/admit/give_up/Admit Obligations/Axiom/Parameter/Conjecture in '${name:-<unknown>}'"
done <<<"$scan_hits"

# 4. Scenario coverage in Check.v.
exp_names="$(sed -n 's/^Definition exp_\([A-Za-z0-9_]*\) .*/\1/p' theories/Conformance/Expected.v)"
[ -n "$exp_names" ] || note "theories/Conformance/Expected.v defines no scenario"
while IFS= read -r name; do
  [ -n "$name" ] || continue
  grep -qE "^Example check_$name( |$)" theories/Conformance/Check.v \
    || note "exp_$name has no Example check_$name in theories/Conformance/Check.v"
  grep -qE "exp_$name\.\$" theories/Conformance/Check.v \
    || note "no Example in theories/Conformance/Check.v is stated over exp_$name"
done <<<"$exp_names"

# 5. Conformance regeneration check.
find_dotnet() {
  if [ -n "${RANVIER_DOTNET:-}" ]; then echo "$RANVIER_DOTNET"; return 0; fi
  if command -v dotnet >/dev/null 2>&1; then command -v dotnet; return 0; fi
  if command -v dotnet.exe >/dev/null 2>&1; then command -v dotnet.exe; return 0; fi
  if [ -x "/mnt/c/Program Files/dotnet/dotnet.exe" ]; then echo "/mnt/c/Program Files/dotnet/dotnet.exe"; return 0; fi
  return 1
}

if dotnet_cmd="$(find_dotnet)"; then
  regen="$(mktemp -p conformance .Expected.XXXXXX)"
  trap 'rm -f "$out" "$regen"' EXIT
  if ! "$dotnet_cmd" build ../src/Ranvier.D5.Charts/Ranvier.D5.Charts.fsproj -c Release -p:CopyLocalLockFileAssemblies=true --nologo -v q >"$out" 2>&1; then
    cat "$out" >&2
    note "dotnet build of Ranvier.D5.Charts failed"
  elif ! "$dotnet_cmd" build ../src/Ranvier.D5.Geo/Ranvier.D5.Geo.fsproj -c Release -p:CopyLocalLockFileAssemblies=true --nologo -v q >"$out" 2>&1; then
    cat "$out" >&2
    note "dotnet build of Ranvier.D5.Geo failed"
  elif ! "$dotnet_cmd" build ../src/Ranvier.D5.View/Ranvier.D5.View.fsproj -c Release -p:CopyLocalLockFileAssemblies=true --nologo -v q >"$out" 2>&1; then
    cat "$out" >&2
    note "dotnet build of Ranvier.D5.View failed"
  elif ! "$dotnet_cmd" fsi --exec conformance/Scenarios.fsx -- "$regen" >"$out" 2>&1; then
    cat "$out" >&2
    note "conformance/Scenarios.fsx failed"
  elif ! tr -d '\r' <"$regen" | diff -u <(tr -d '\r' <theories/Conformance/Expected.v) - >&2; then
    note "theories/Conformance/Expected.v differs from a fresh F# run"
  fi
else
  note "no dotnet found; set RANVIER_DOTNET or install the .NET SDK"
fi

# 6. Extraction freshness check.
for f in engine.ml engine.mli; do
  if [ ! -f "$f" ]; then
    note "$f was not generated by build.sh"
  elif [ ! -f "differential/$f" ]; then
    note "differential/$f is missing"
  elif ! tr -d '\r' <"differential/$f" | diff -u - <(tr -d '\r' <"$f") >&2; then
    note "differential/$f differs from freshly extracted $f"
  fi
done

# 7. Differential check.
if ! bash ./differential/run.sh 400 1; then
  note "differential/run.sh 400 1 failed"
fi

[ "$fail" -eq 0 ] || exit 1
scenarios="$(grep -cE '^\s*Example\s+check_S' theories/Conformance/Check.v)"
echo "check.sh: OK ($expected theorems closed, $scenarios conformance scenarios, 400 differential programs)"

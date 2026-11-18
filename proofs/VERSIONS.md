# Toolchain

Platform: WSL, Ubuntu 24.04 LTS (distribution `Ubuntu`), opam 2.3.0 switch `ranvier-proofs`.
`build.sh` and `check.sh` re-execute themselves in that distribution when started from Git Bash or PowerShell;
set `RANVIER_WSL_DISTRO` to use another distribution name.

| Component | Version |
|---|---|
| OCaml | 4.14.2 (`ocaml-base-compiler`) |
| Rocq (`rocq-core`, `rocq-runtime`) | 9.1.0 |
| `rocq-stdlib` | 9.2.0 |
| stdpp (`rocq-stdpp`) | 1.13.0 |
| zarith | 1.14 |

`Ranvier.D5` models pure numerical, spatial, geometric, topological, downsampling, selection, streaming-clip,
gesture, and worker-revision state machines above the reactive core (`ranvier-org/Ranvier`) and retained view
layer (`ranvier-org/Ranvier.View`), which already verify reactive graph, ownership, and keyed DOM invariants.

## Reproducing the switch

```sh
opam init --bare --disable-sandboxing --no-setup
opam switch create ranvier-proofs ocaml-base-compiler.4.14.2
eval "$(opam env --switch=ranvier-proofs)"
opam repo add rocq-released https://rocq-prover.org/opam/released
opam update
opam install -j16 rocq-core.9.1.0 rocq-stdlib.9.2.0 rocq-stdpp.1.13.0 zarith.1.14
```

## Differential check

`differential/run.sh 400` compiles the committed `differential/engine.ml` with `ocamlopt` alone (no Rocq or
stdpp needed) and compares it against the compiled F# assemblies (`Ranvier.D5`, `Ranvier.D5.Charts`,
`Ranvier.D5.Geo`, `Ranvier.D5.View`).

## Navigator site

`site.sh` renders the theories into `site/output/` with `Nacara.Plugin.Rocq` and `FSharp.Data.Rocq` at the
version pinned in `site/Ranvier.D5.Proofs.Site.fsproj`. It checks that every `Theorem` and `Corollary` carries a
`(**md **...** *)` guarantee comment (`docs/guarantees.py comments`), captures types and assumptions from the
compiled `.vo` files (`rocq-capture`), and builds the static pages under `--strict`.

## Commands

- Build: `bash proofs/build.sh`
- Audit: `bash proofs/check.sh`
- Navigator site: `bash proofs/site.sh`
- Differential check: `bash proofs/differential/run.sh [count] [seed]`
- Run a command in the toolchain environment: `bash proofs/build.sh --run CMD ARGS`

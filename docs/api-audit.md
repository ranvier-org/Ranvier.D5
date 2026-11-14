# Public API audit

This audit closes task 11's stabilisation checks for the alpha line. It records
how each property was checked so the checks can be repeated before a stable
release. `tools/audit-api-docs.fsx` and `tools/audit-mutable-signatures.fsx` use
reflection over built assemblies; pass the output directory and assembly names,
for example `dotnet fsi tools/audit-api-docs.fsx <bin> Ranvier.D5 Ranvier.D5.Charts`.

## XML documentation

Every exported type and public module function or value now has an XML
`<summary>`. The reflection pass compares exported types and module members with
the generated XML files. Union cases that compile to nested classes count as
documented when the case or its union type is documented.

| Package | Undocumented before | After |
|---|---:|---:|
| Ranvier.D5 | 289 | 0 |
| Ranvier.D5.Charts | 155 | 0 |
| Ranvier.D5.View | 29 | 0 |
| Ranvier.D5.Geo | 1 | 0 |

About 470 summaries were added across 66 files without code changes, except that
`WorkState` now lists one case per line so each case can carry its own summary.
Unions and records declared on a single line document their cases and fields in
the type summary rather than being reformatted. The review also corrected four
older summaries that described a neighbouring declaration (`Piecewise`,
`TextStyle`, an axis helper and `Renderer.create`).

Summaries state the contract: units and coordinate conventions, whether
failures are typed `Result` errors or argument exceptions, ownership and
disposal for View resources, and invariants such as stable semantic IDs. No
summary uses `<paramref>`, which would oblige every parameter to be documented.

## Invalid input

D5 has two failure channels, as `docs/contracts.md` specifies:

- Construction-time programmer errors (non-finite scale domains, negative
  dimensions, duplicate mark keys, invalid budgets) throw `ArgumentException`
  immediately. Scale, Mark, Geometry, Format and Scene concentrate these checks.
- Data-dependent work (layouts, recipes, parsing, projection, fitting) returns
  `Result<_, LayoutError list>`. Chart recipes raise argument errors only inside
  `Recipe.run`, which converts them to `InvalidObservation`; tests cover empty,
  missing, duplicate, non-finite and malformed inputs for every recipe family.

A search of library code finds no untyped `failwith` except two that guard
constant defaults (the geographic origin and identity rotation) and cannot fail.

## Allocation ownership

A reflection pass lists every public member whose signature exposes an array,
`ResizeArray`, `Dictionary` or `HashSet`. The only results are `Delaunay`
accessors, all of which return copies (`Array.copy`), the bisect functions in
`Statistics` and `Data`, which only read the caller's array, and
`Distribution.samples`, which returns a new array for each call that the caller
owns. Every other
public result is an immutable record, union, list, map or set, and kernels keep
mutable working buffers private.

## Package dependencies

| Package | Depends on |
|---|---|
| Ranvier.D5 | Ranvier 0.2.0, Ranvier.Anime 0.1.0, Ranvier.Css 0.1.1 (minimum versions) |
| Ranvier.D5.Charts | Ranvier.D5 |
| Ranvier.D5.View | Ranvier.D5, Ranvier.D5.Charts, Ranvier.View 0.1.49 (minimum) |
| Ranvier.D5.Geo | Ranvier.D5.Charts |

Core, Charts and View do not depend on Geo. Traced variants mirror these
identities with the `.Traced` suffix, and the release preparation script
verifies every packed dependency's identity and version.

## Naming

Public modules follow the existing convention: a type `X` with a
`[<RequireQualifiedAccess>] module X` of functions, `create`/`defaultValue`
constructors, `WithCancellation` variants that take an explicit token, and
`try`-prefixed functions returning options. New task 10–11 APIs
(`Projection`, `ProjectionFit`, `CompositeProjection`, `MapChart`,
`RandomState`, `Distribution`, `Dsv`, `DsvRow`, `Parse`) follow the same
pattern. Breaking renames of earlier alpha APIs were not made; the alpha
version line documents any future renames in release notes.

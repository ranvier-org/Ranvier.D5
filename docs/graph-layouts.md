# Compound Diagram, Association-Machine & 3D Graph Layout Engines

`Ranvier.D5.Graph` and `Ranvier.D5.Graph3D` extend `Ranvier.D5` with pure, deterministic, cancellable layout and routing engines for compound grouped diagrams, port-constrained flowcharts, state/ER/UML association machines, scalable metric/modularity networks, and 3D Euclidean & $\mathbb{H}^3$ hyperbolic spaces (including Tamara Munzner's **H3** and CAIDA's **Walrus**).

See [graph-layout-research.md](graph-layout-research.md) for the academic literature survey, Mermaid/ELK/CoLa/fCoSE/H3/Walrus prior art, and package boundary rationale.

---

## 1. Unified Compound, Port & Association Machine IR (`Ranvier.D5.Graph`)

All 2D and 3D engines operate over the same validated, immutable `DiagramGraph` value:

- **`DiagramNode`**: heterogeneous `Size` (`Width`, `Height`) and 3D `DepthSize`, optional `ParentGroup` membership, explicit `Ports` (`PortSide.North | South | East | West | Auto` with side `Order`), `NodeKind` (`Standard`, `InitialState`, `FinalState`, `Choice`, `Junction`, `AssociationHub`), optional `Pinned` coordinate, and `Mass`.
- **`DiagramGroup`**: arbitrarily nested compound clusters/subgraphs (`ParentGroup`), directional `GroupPadding` (supporting top title banners via `GroupPadding.titled`), `MinSize`, and `Collapsed = true` macro-node collapse (`DiagramGraph.resolveCollapsed`).
- **`Association`**: `Source` and `Target` `Endpoint`s (node/group ID + optional port ID), `AssociationKind` (`Directed`, `Bidirectional`, `Undirected`, `Inheritance`, `Composition`, `Aggregation`, `Dependency`, `Transition`), positive `Weight`, layered `MinLength`, and optional `SourceLabel`, `CenterLabel`, and `TargetLabel` boxes for ER multiplicities and state transition guards.
- **`LayoutConstraint`**: `AlignHorizontal`, `AlignVertical`, `OrderAlong(axis, before, after, minGap)`, and `FixedPosition`.
- **N-ary Hyper-Associations**: `DiagramGraph.hyperAssociation` constructs a diamond `AssociationHub` node and spoke associations for ternary or $n$-ary relationships.
- **Spanning Forest Extraction**: `SpanningTree.extract` (`Bfs`, `MaxWeight`, `MinDepth`) partitions associations into `TreeAssociationIds` (backbone) and `CrossAssociationIds` (cross-links, back-edges, self-loops).

---

## 2. 2D Layout & Routing Engines (`Ranvier.D5.Graph`)

| Module | Algorithm & Lineage | Primary Use Cases |
|---|---|---|
| `Sugiyama` | Eades–Lin–Smyth cycle breaking + tight-tree rank optimization + Forster compound group contiguity + port-aware barycenter/median sweeps + 4-pass Brandes–Köpf coordinates (`TopDown`, `BottomUp`, `LeftRight`, `RightLeft`) | Mermaid/ELK flowcharts, `stateDiagram-v2` composite state machines, `classDiagram`, C4 architecture |
| `OrthogonalRouter` | Wybrow–Marriott–Stuckey obstacle-avoiding local Hanan-grid A* router with port stubs, bend penalties, parallel channel/bus nudging, corner fillets, and label placement | Standalone or integrated port-to-port orthogonal wiring for ER, UML, and architecture diagrams |
| `Cola` | Dwyer–Koren–Marriott IPSep-CoLa stress descent coupled with Hildreth Dual Coordinate Ascent QP separation projection | Interactive diagrams requiring strict non-overlap, alignment rails, directed flow tiers, and group containment |
| `FCose` | Balcı & Dogrusoz (2022) PivotMDS algebraic skeleton + Quadtree Barnes–Hut compound spring embedder + constraint polish | Compound biological/software cluster networks and organic mindmaps |
| `PivotMds` | Brandes & Pich (2006) $O(k(|V|+|E|))$ k-center landmark double-centered SVD/power scaling (SIMD-accelerated on .NET 10) | Instant deterministic global unfolding of multi-thousand-node networks |
| `Stress2D` | Zheng, Pawar & Goodman (2019) Fisher–Yates Stochastic Gradient Descent (SGD) Stress (exact APSP or sparse pivot+edge mode) | High-fidelity metric distance preservation free from poor local minima |
| `ModularityForce2D` | Heavy-Edge Multilevel ($FM^3$/Walshaw) + Jacomy `ForceAtlas2`, Noack `LinLog` modularity energy, or `FruchtermanReingold` | Unsupervised community/cluster discovery and large network visualization |
| `RadialTree` | Size-aware concentric 360° rings and `@mermaid-js/layout-tidy-tree` bidirectional left/right mindmap | Organizational hierarchies, taxonomies, and balanced mindmaps |
| `BoxPack` | Recursive bottom-up Skyline aspect-ratio rectangle packer (`elk.box`) | Treeless block architectures and disconnected component packing |
| `DiagramScene` | Pure renderer into `Ranvier.D5.Scene` (`Scene.compose`) with UML/ER/State markers, nested group headers, ports, and labels | Retained native SVG rendering through `Ranvier.D5.View` |

Every engine exposes `layout` and `layoutWithCancellation` (`CancellationToken -> Config -> DiagramGraph -> Result<DiagramLayout2D, LayoutError list>`), plugging directly into `Ranvier.D5.View.ReactiveLayout.create`.

---

## 3. 3D Euclidean & $\mathbb{H}^3$ Hyperbolic Engines (`Ranvier.D5.Graph3D`)

`Ranvier.D5.Graph3D` shares `Ranvier.D5.Point3` and `Ranvier.D5.Bounds3` (with `Ranvier.D5.Wgpu`) and produces `DiagramLayout3D` values carrying stable `Id`s, `ScaleFactor` fisheye attenuation, `IsBackbone` flags, and contiguous O(1)-indexed `Points: Point3Chain` waypoint buffers ready for zero-copy GPU instancing or pure CPU `Scene3D` SVG fallback projection.

| Module | Algorithm & Lineage | Primary Use Cases |
|---|---|---|
| `Hyperbolic3D` & `HyperbolicTransform` | Exact $(3+1)$D Minkowski upper-sheet hyperboloid ($\langle p,q\rangle_M = -1$), Beltrami–Klein straight-chord ball, Poincaré conformal ball, Minkowski Gram–Schmidt orthonormalization, and 3-tier SIMD (`Vector512`/`Vector256`/`Vector128`) batch $O^+(3,1)$ Lorentz boosts | Hyperbolic geometry foundations and drift-free interactive focus+context navigation |
| `H3` | Tamara Munzner (1997/1998) bottom-up hyperbolic hemisphere area synthesis ($2\pi\sinh^2 r_p$) + top-down concentric spherical-cap band packing + `H3.focusNode` $O(N)$ SIMD Lorentz focus boosts | Exploring massive directed call graphs, file systems, and web/taxonomy trees in a 3D Klein or Poincaré sphere |
| `Walrus` | CAIDA Walrus (`H3GraphLayout`) explicit or extracted spanning-tree backbone + subtree-weighted 3D Fibonacci cone allocation + toggleable cross-links | Internet AS topologies, phylogenetic trees, and large directed graphs with domain-specific spanning trees |
| `Octree` & `Force3D` | Immutable 3D `Octree<'T>` (`within`, `nearest`, stack-reused `visitIndexed`) + 3D Barnes–Hut `ForceAtlas2` / `LinLog` / `FruchtermanReingold` + 3D compound group cohesion & sphere collision | 3D clustered networks and spatial particle/graph layouts |
| `Stress3D` | 3D PivotMDS + 3D Stochastic Gradient Descent (SGD) Stress with optional `FixedZ` stratum pins | Metric 3D graph embeddings and Z-pinned multi-layer networks |
| `ConeTree3D` | Robertson–Mackinlay–Card (1991) `VerticalCone` / `HorizontalCam` + Carrière–Kazman (1995) `SphericalBalloon` trees | 3D hierarchical org charts and isotropic 3D subtree exploration |
| `Layered3D` | Hong & Nikolov (2005) 3D stratified compound architecture planes (`StratumAxis.Y` or `Z`) with intra-plane barycenter + riser routing | Multi-tier system architecture planes (UI / API / Data strata) in 3D |
| `Camera3D` & `Scene3D` | Orbit/turntable perspective & orthographic camera + painter's depth-sorted SVG projection with sphere guides and cuboid wireframes | Immediate rendering in `Ranvier.D5.View` without requiring a GPU context |

---

## 4. Measured Performance

All figures below are **measured here** on Windows (.NET 10 Release) via `dotnet run --project benchmarks/Ranvier.D5.Graph.Benchmarks -c Release -- artifacts/graph-benchmarks.json`, using the shared `Ranvier.D5.BenchmarkHarness` (`benchmarks/Shared/Harness.fs`: 3 warm-up runs followed by 60 timed samples with `GC.Collect()` outside the timed window and `GC.GetAllocatedBytesForCurrentThread()` allocation accounting):

| Contract | Median (ms) | p95 (ms) | Median Managed Allocation (bytes) | Gate Limit (p95 ms / bytes) |
|---|---:|---:|---:|---:|
| `sugiyama-compound-spline-250-nodes` | 9.481 | 16.318 | 14,582,808 | 120 / 32,000,000 |
| `sugiyama-compound-orthogonal-120-nodes` | 17.491 | 27.002 | 23,772,224 | 160 / 40,000,000 |
| `cola-constrained-nonoverlap-120-nodes` | 6.723 | 16.815 | 8,044,712 | 150 / 32,000,000 |
| `fcose-compound-300-nodes-600-edges` | 10.153 | 19.833 | 11,323,840 | 200 / 48,000,000 |
| `pivotmds-2d-2000-nodes-4000-edges` | 4.205 | 5.297 | 12,271,456 | 120 / 32,000,000 |
| `stress-sgd-2d-500-nodes-1000-edges` | 5.983 | 7.585 | 5,645,264 | 120 / 28,000,000 |
| `modularity-forceatlas2-500-nodes-1000-edges` | 28.997 | 38.165 | 25,149,576 | 220 / 48,000,000 |
| `radial-mindmap-1000-nodes` | 2.564 | 14.081 | 7,116,760 | 80 / 24,000,000 |
| `boxpack-compound-300-nodes` | 0.664 | 0.862 | 1,369,088 | 50 / 16,000,000 |
| `h3-munzner-5000-nodes-klein` | 13.835 | 19.807 | 34,592,784 | 150 / 48,000,000 |
| `h3-lorentz-focus-boost-project-10000-nodes` | 1.910 | 2.851 | 9,600,696 | 60 / 24,000,000 |
| `walrus-caida-5000-nodes-crosslinks` | 13.194 | 16.205 | 34,553,480 | 150 / 48,000,000 |
| `octree-3d-build-10000` | 3.396 | 4.247 | 4,033,696 | 60 / 16,000,000 |
| `octree-3d-nearest-200-in-10000` | 1.528 | 1.706 | 299,280 | 40 / 4,000,000 |
| `force3d-octree-400-nodes-800-edges` | 15.063 | 27.537 | 6,244,080 | 250 / 48,000,000 |
| `stress-sgd-3d-500-nodes-1000-edges` | 6.177 | 14.272 | 5,042,432 | 150 / 32,000,000 |
| `conetree-balloon-3d-2000-nodes` | 3.160 | 3.678 | 10,499,568 | 80 / 24,000,000 |
| `layered-3d-stratified-250-nodes` | 3.098 | 3.973 | 11,404,328 | 150 / 36,000,000 |


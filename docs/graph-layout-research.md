# Graph, Compound Grouping, Association-Machine & 3D Layout Engines: Research & Package Architecture

This document surveys the academic literature and industrial prior art behind modern diagrammatic, compound-grouping, association-machine, and 3D hyperbolic/Euclidean graph layout engines, and records the architectural decision for packaging these engines in the `Ranvier.D5` repository.

---

## 1. Problem Scope: Beyond Basic Node-Link Force Simulation

`Ranvier.D5` provides foundational D3-like primitives (`Tree`, `Cluster`, `Treemap`, `Pack`, `Chord`, `Sankey`, and basic 2D point-mass `Force` with `Quadtree` Barnes–Hut). However, rich diagramming systems (such as **Mermaid**, **ELK**, **Dunnart/WebCoLa**, and **Cytoscape**) and large-scale 3D topology explorers (such as **Munzner's H3** and **CAIDA's Walrus**) require capabilities that flat point-particle simulations cannot express:

1. **Heterogeneous Sized Nodes & Explicit Ports**: Nodes are rectangles, diamonds, or state capsules with distinct widths, heights (and 3D depths), plus directional connection ports (`North`, `South`, `East`, `West`, `Top`, `Bottom`, `Auto`) with fixed or ordered port constraints.
2. **Compound Grouping (Nested Subgraphs & Composite States)**: Nodes belong to arbitrarily nested parent groups/clusters (`subgraph` in Mermaid flowcharts, composite/concurrent states in `stateDiagram-v2`, system/container boundaries in C4 architecture diagrams) with header/padding insets and collapsible macro-node semantics.
3. **Association Machines**: Edges are typed semantic associations—state transitions with `[guard] / effect` center labels, ER crow's-foot relationships with `Source`/`Target` cardinalities (`1`, `0..*`), UML class inheritance/composition/aggregation/dependency links, bidirectional channels, self-loops, parallel multi-edges, and $n$-ary association hubs—routed cleanly around obstacles with orthogonal bends or splines and non-overlapping label anchors.
4. **Hard & Soft Geometric Constraints**: Alignment rails (`AlignHorizontal`, `AlignVertical`), directional/tier ordering (`OrderAlongAxis`), non-overlap separation, and group containment.
5. **3D Euclidean & 3D Hyperbolic ($\mathbb{H}^3$) Spaces**: Exponentially branching hierarchies and internet-scale directed graphs crowd polynomially ($O(r^2)$ or $O(r^3)$) in Euclidean space, whereas hyperbolic 3-space $\mathbb{H}^3$ expands exponentially ($A(r) = 4\pi\sinh^2 r$), enabling interactive focus+context exploration of tens of thousands of nodes inside a unit Klein or Poincaré ball via $O(N)$ Lorentz boosts.

---

## 2. Prior Art & Academic Foundations

### 2.1 Mermaid & Industrial Diagramming Prior Art

Modern Mermaid (`mermaid-js`) separates diagram parsing from pluggable layout and routing engines:

| Engine in Prior Art | Diagram Families Served | Core Algorithmic Lineage |
|---|---|---|
| **ELK Layered** (`elk.layered`) & **Dagre** | Flowcharts, `stateDiagram-v2`, `erDiagram`, `classDiagram`, C4/Architecture | Sugiyama–Tagawa–Toda (1981), Gansner et al. DOT Network Simplex (1993), Brandes & Köpf (2001), Forster Compound Ordering (2004), Orthogonal/Spline port routing |
| **ELK Orthogonal / Libavoid** | Architecture, ER, UML Class, State machines | Wybrow, Marriott & Stuckey (2009) visibility/Hanan A* orthogonal connector routing with bend penalties and segment nudging |
| **CoSE-Bilkent / fCoSE** (`cose`, `fcose`) | Mindmaps, compound biological/architecture networks | Balcı & Dogrusoz (2022) spectral/PivotMDS skeleton + compound spring embedder + constraint projection |
| **WebCoLa / IPSep-CoLa** (`cola`) | Interactive architecture, state/workflow diagrams | Dwyer, Koren & Marriott (2006) stress majorization with incremental VPSC separation constraints |
| **ELK Stress** (`elk.stress`) | Undirected metric networks, ER overview | Gansner, Koren & North (2004); superseded in speed/quality by PivotMDS (Brandes & Pich 2006) + SGD Stress (Zheng, Pawar & Goodman 2019) |
| **Tidy Tree & ELK Box** (`tidy-tree`, `elk.box`) | Bidirectional mindmaps, radial org charts, disconnected component packing | Reingold–Tilford / Buchheim radial & split trees; Skyline/Shelf aspect-ratio rectangle packing |
| **Munzner H3 & CAIDA Walrus** | Large directed call graphs, web/AS internet topologies, taxonomies in $\mathbb{H}^3$ | Munzner (1997, 1998) Klein-ball hemisphere packing; Hyun / CAIDA (2000–2004) Minkowski $\mathbb{R}^{3,1}$ spanning-tree + cross-link lens |

### 2.2 Primary Academic Literature

1. **Layered & Compound Hierarchical Drawing (`Sugiyama`)**
   - K. Sugiyama, S. Tagawa, and M. Toda, *"Methods for Visual Understanding of Hierarchical System Structures,"* IEEE Trans. Systems, Man, and Cybernetics, 11(2):109–125, 1981.
   - P. Eades, X. Lin, and W. F. Smyth, *"A Fast and Effective Heuristic for the Feedback Arc Set Problem,"* Information Processing Letters, 47(6):319–323, 1993 — linear-time $O(|V|+|E|)$ source/sink/degree-delta cycle breaking.
   - E. R. Gansner, E. Koutsofios, S. C. North, and K.-P. Vo, *"A Technique for Drawing Directed Graphs,"* IEEE Trans. Software Engineering, 19(3):214–230, 1993 — feasible tight-tree construction and network-simplex cut-value rank optimization.
   - U. Brandes and B. Köpf, *"Fast and Simple Horizontal Coordinate Assignment,"* Proc. Graph Drawing (GD 2001), LNCS 2265, pp. 31–44, 2002 — $O(|V|+|E|)$ type-1 conflict detection, four-direction vertical alignment and horizontal block compaction, aligned width shifting, and median-two averaging.
   - M. Forster, *"Applying Crossing Reduction Strategies to Layered Compound Graphs,"* Proc. Graph Drawing (GD 2004), LNCS 3383, pp. 276–284, 2004 — hierarchical layer ordering that prevents sibling compound clusters from interleaving.

2. **Obstacle-Avoiding Orthogonal & Port Association Routing (`OrthogonalRouter`)**
   - M. Wybrow, K. Marriott, and P. J. Stuckey, *"Orthogonal Connector Routing,"* Proc. Graph Drawing (GD 2009), LNCS 5849, pp. 219–231, 2010 — port-stub generation along cardinal faces, obstacle-inflated Hanan/visibility grid graph construction, A* shortest-path search with explicit bend penalties, and post-route channel nudging to separate shared overlapping collinear segments into distinct parallel tracks.

3. **Constrained & Compound Force/Optimization Layouts (`Cola`, `FCose`)**
   - T. Dwyer, Y. Koren, and K. Marriott, *"IPSep-CoLa: An Incremental Procedure for Separation Constraint Layout of Graphs,"* IEEE Trans. Visualization and Computer Graphics (TVCG), 12(5):821–828, 2006 — couples gradient stress descent with 1D Variable Placement with Separation Constraints ($x_u + g \le x_v$) to guarantee node non-overlap, alignment rails, directed flow tiers, and nested cluster containment.
   - H. Balcı and U. Dogrusoz, *"fCoSE: A Fast Compound Graph Layout Algorithm with Constraint Support,"* IEEE TVCG, 28(12):4582–4593, 2022 — accelerates compound spring embedding via algebraic/spectral skeleton initialization followed by compound bounding-box spring/repulsion polish and iterative constraint projection.

4. **Scalable Metric Scaling, Stochastic Stress & Modularity Clustering (`PivotMds`, `Stress2D`/`Stress3D`, `ModularityForce2D`/`Force3D`)**
   - U. Brandes and C. Pich, *"Eigensolver Methods for Progressive Multidimensional Scaling of Large Data,"* Proc. Graph Drawing (GD 2006), LNCS 4372, pp. 42–53, 2007 — **PivotMDS** approximates classical MDS in $O(k(|V|+|E|) + k^2|V|)$ time and $O(k|V|)$ memory using $k \ll |V|$ k-center landmarks and double-centered rectangular SVD/power iteration in 2D or 3D.
   - J. X. Zheng, S. Pawar, and D. F. M. Goodman, *"Graph Drawing by Stochastic Gradient Descent,"* IEEE TVCG, 25(9):2738–2748, 2019 — minimizes metric stress $\sum_{i<j} w_{ij}(\|x_i - x_j\| - d_{ij})^2$ via Fisher–Yates shuffled pairwise updates with step clamping $\mu_{ij} = \min(\eta(t) w_{ij}, 1)$ and exponential/Robbins–Monro annealing between $\eta_{\max} = 1/w_{\min}$ and $\eta_{\min} = \varepsilon / w_{\max}$ in arbitrary dimension ($\mathbb{R}^2$ or $\mathbb{R}^3$).
   - S. Hachul and M. Jünger, *"Drawing Large Graphs with a Potential-Field-Based Multilevel Algorithm,"* Proc. Graph Drawing (GD 2004), LNCS 3383, pp. 285–295, 2004 ($FM^3$), and C. Walshaw, *"A Multilevel Algorithm for Force-Directed Graph-Drawing,"* JGAA, 7(3):253–285, 2003 (2D and 3D coarsening/prolongation).
   - A. Noack, *"Modularity Clustering Is Force-Directed Layout,"* Physical Review E, 79(2):026102, 2009 (**LinLog** energy model), and M. Jacomy, T. Venturini, S. Heymann, and M. Bastian, *"ForceAtlas2, a Continuous Graph Layout Algorithm for Handy Network Visualization,"* PLOS ONE, 9(6):e98679, 2014 (degree-scaled repulsion and adaptive swing/traction speed regulation).

5. **3D Hyperbolic ($\mathbb{H}^3$) & 3D Hierarchical/Stratified Layouts (`H3`, `Walrus`, `ConeTree3D`, `Layered3D`)**
   - T. Munzner, *"H3: Laying Out Large Directed Graphs in 3D Hyperbolic Space,"* Proc. IEEE Symp. Information Visualization (InfoVis 1997), pp. 2–10, 1997; and T. Munzner, *"Exploring Large Graphs in 3D Hyperbolic Space,"* IEEE Computer Graphics and Applications, 18(4):18–23, 1998 — extracts a domain spanning tree, computes bottom-up hyperbolic hemisphere areas $H_p = 2\pi\sinh^2 r_p$, packs sorted child subtrees into concentric spherical-cap bands on parent hemispheres using hyperbolic right-triangle trigonometry ($\sinh r_\varphi = \sinh r_p \sin\varphi$, $\tan\delta\theta = \tanh r_c / \sinh r_\varphi$), embeds coordinates in the Beltrami–Klein unit ball (where geodesics are straight Euclidean segments), and navigates focus+context via 4×4 projective/Lorentz transformations.
   - Y. Hyun (CAIDA), *"Walrus — Graph Visualization Tool,"* CAIDA (`https://github.com/CAIDA/walrus`, `H3GraphLayout` / `H3Transform`), 2000–2004 — represents $\mathbb{H}^3$ points on the upper sheet of the $(3+1)$D Minkowski hyperboloid ($\langle p, q\rangle_M = x_1x_2 + y_1y_2 + z_1z_2 - w_1w_2 = -1, w \ge 1$), separates structural **spanning-tree backbone edges** from **non-tree cross-links**, applies $O^+(3,1)$ Lorentz boosts + $SO(3)$ rotations to bring any focus node isometrically to the origin $(0,0,0,1)$, and renormalizes hyperboloid vectors across deep levels to prevent floating-point drift off the sheet.
   - G. G. Robertson, J. D. Mackinlay, and S. K. Card, *"Cone Trees: Animated 3D Visualizations of Hierarchical Information,"* Proc. ACM CHI 1991, pp. 189–194, 1991; and J. Carrière and R. Kazman, *"Interacting with Huge Hierarchies: Beyond Cone Trees,"* Proc. InfoVis 1995, pp. 74–81, 1995 (3D spherical balloon trees).
   - S.-H. Hong and N. S. Nikolov, *"Layered Drawings of Directed Graphs in Three Dimensions,"* Proc. APVis / GD, 2005 — stratified 2.5D/3D planes for multi-tier system architectures and temporal state machines.

---

## 3. Architectural Decision: Separate Packages Inside `Ranvier.D5`

### Question
Should the compound, association-machine, and 3D layout engines live directly inside the `Ranvier.D5` core package, or as separate packages within the `Ranvier.D5` repository?

### Decision
**Live as two layered expansion packages within the `Ranvier.D5` repository:**

1. **`Ranvier.D5.Graph`** (`src/Ranvier.D5.Graph`, depending only on `Ranvier.D5`):
   - Owns the unified immutable **Compound, Port & Association Machine IR** (`DiagramGraph`, `DiagramNode`, `GroupSpec`, `PortSpec`, `Association`, `LayoutConstraint`), shared CSR structural & spectral/pivot/coarsening kernels (`GraphKernels`), **all 2D diagrammatic & scalable network layout engines** (`Sugiyama`, `OrthogonalRouter`, `Cola`, `FCose`, `PivotMds`, `Stress2D`, `ModularityForce2D`, `RadialTree`, `BoxPack`), and the pure `DiagramScene` bridge to `Ranvier.D5.Scene`.
2. **`Ranvier.D5.Graph3D`** (`src/Ranvier.D5.Graph3D`, depending on `Ranvier.D5.Graph` and `Ranvier.D5`):
   - Owns **3D Euclidean & $\mathbb{H}^3$ Hyperbolic geometry** (`Point3`, `Bounds3`, `Quaternion`, `Octree`, `MinkowskiPoint`, `HyperbolicTransform` $O^+(3,1)$ boosts, Klein & Poincaré unit-ball models), **all 3D layout engines** (`H3`, `Walrus`, `Force3D`, `Stress3D`, `ConeTree3D`, `Layered3D`), and the pure `Camera3D` / `Scene3D` bridge projecting 3D & hyperbolic scenes into `Ranvier.D5.Scene` (while exposing flat immutable 3D coordinates directly usable by `Ranvier.Wgpu` GPU instancing).

### Rationale

1. **Preserves `Ranvier.D5` Core Lean Footprint & Fable Bundle Size**:
   `Ranvier.D5` already compiles 44 modules (**from documentation**: `src/Ranvier.D5/Ranvier.D5.fsproj`) for scales, statistics, curves, basic hierarchy, quadtree, Delaunay/Voronoi, basic force, Sankey, chord, and contours, and packs all `.fs` files for `fickl`/Fable JavaScript compilation. Following the exact precedent established by `Ranvier.D5.Geo`, keeping specialized diagramming, constraint solvers, obstacle routing, and 3D hyperbolic/Euclidean coordinate systems in opt-in packages prevents binary, JIT, AOT, and JS bundle bloat for standard chart consumers.
2. **Avoids Shallow Micro-Package Fragmentation**:
   Splitting every algorithm into its own NuGet package (`Sugiyama`, `Cola`, `FCose`, `H3`, `Walrus`, ...) would create shallow seams, duplicate CSR/shortest-path/coarsening infrastructure, multiply `.Traced` package variants, and force callers to convert between incompatible graph types when switching layout engines (the exact anti-pattern Mermaid solved by unifying behind ELK/diagram specs).
3. **Clean 2D vs. 3D Coordinate Seam with Zero IR Duplication**:
   2D flowchart/state/ER/architecture consumers install `Ranvier.D5.Graph` without pulling 3D vectors, `Octree`, or 4×4 Minkowski matrices. 3D consumers install `Ranvier.D5.Graph3D` and pass the *exact same* `DiagramGraph` (with groups, associations, weights, and roots) directly into `H3`, `Walrus`, `Force3D`, `Stress3D`, `ConeTree3D`, or `Layered3D`.
4. **Zero Runtime Coupling to `Ranvier.View` or `Ranvier.Wgpu`**:
   Every layout engine is a pure, deterministic, cancellable data-to-geometry function (`CancellationToken -> Config -> Graph -> Result<Layout, LayoutError list>`). Any engine plugs directly into `Ranvier.D5.View.ReactiveLayout.create` for background worker execution, projects to `Ranvier.D5.Scene` for retained SVG rendering, or feeds `Ranvier.Wgpu` 3D vertex/instance buffers without dragging native windowing or GPU runtimes into layout packages.

---

## 4. Performance Engineering Principles

- **Dense Integer CSR & Structure-of-Arrays (SoA)**: String IDs are validated and mapped once at boundary entry to dense `0 .. n-1` ordinal indices. Inner loops operate exclusively over flat `int[]` CSR adjacency tables and contiguous `float[]` coordinate/velocity/Minkowski arrays (`xs`, `ys`, `zs`, `ws`), eliminating dictionary lookups, F# list allocations, and pointer chasing.
- **SIMD & `TensorPrimitives` Acceleration on .NET 10 (`#if !FABLE_COMPILER`)**: Contiguous vector dot products, norms, double-centering reductions, SoA velocity updates, and batch 4×4 Minkowski $O^+(3,1)$ Lorentz boosts + Klein/Poincaré ball projections use `System.Numerics.Tensors.TensorPrimitives` and `Vector128`/`Vector256`/`Vector512` intrinsics with exact scalar fallbacks under `#if FABLE_COMPILER`.
- **Cooperative Cancellation & Sub-Quadratic Scaling**: Iterative loops check `CancellationToken` every 64 elements via bitmask (`i &&& 63 = 0`). Large-graph engines achieve $O(V + E)$ (`H3`, `Walrus`, Brandes–Köpf coordinate assignment), $O(k(V + E))$ (`PivotMds`, sparse `Stress2D`/`Stress3D`), and $O(V \log V + E)$ (`Quadtree`/`Octree` Barnes–Hut + multilevel coarsening).

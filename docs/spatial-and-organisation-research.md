# High-Value 2D & 3D Layouts and Data Organisation: Academic Research & Architecture

> **Scope & Design Baseline**: Primary-source academic literature survey, industrial prior-art review, mathematical invariants, and dual-target (`.NET 10` `Vector512`/`Vector256`/`Vector128` SIMD + `Fickl`/Fable JavaScript) architecture across `Ranvier.D5`, `Ranvier.D5.Charts`, `Ranvier.D5.Geo`, `Ranvier.D5.Graph`, `Ranvier.D5.Graph3D`, and `Ranvier.D5.Wgpu`.

---

## 0. Baseline Audit of Existing `Ranvier.D5*` Capabilities & Package DAG

### 0.1 Project Reference DAG (Architectural Constraint)
Inspecting the `.fsproj` manifests establishes the exact dependency hierarchy:
- **`Ranvier.D5`** *(zero project dependencies; owns `Point`, `Bounds`, `Point3`, `Bounds3`, `HierarchyItem<'Id>`, `HierarchyIndex<'Id>`, `GraphData`, `ExactDyadic` predicates, `SimdNumeric`, `FableCompat`)*
- **`Ranvier.D5.Charts`** $\rightarrow$ `Ranvier.D5` *(does **not** reference `Ranvier.D5.Graph`)*
- **`Ranvier.D5.Geo`** $\rightarrow$ `Ranvier.D5.Charts` $\rightarrow$ `Ranvier.D5`
- **`Ranvier.D5.Graph`** $\rightarrow$ `Ranvier.D5` *(exposes `CsrGraph`, `SimdMath`, `BhQuadtree2D`, VPSC, `PivotMds` internals to `Ranvier.D5.Graph3D` via `InternalsVisibleTo`)*
- **`Ranvier.D5.Graph3D`** $\rightarrow$ `Ranvier.D5`, `Ranvier.D5.Graph`
- **`Ranvier.D5.Wgpu`** $\rightarrow$ `Ranvier.D5`, `Ranvier.Wgpu` *(does **not** reference `Ranvier.D5.Graph` or `Ranvier.D5.Graph3D`)*

**Critical Placement Corollary**:
1. Any **3D spatial acceleration structure** over `Point3` / `Bounds3` (`Bvh3D`, `RTree3D`, `KDTree3D`) placed in **`Ranvier.D5`** immediately accelerates **both** `Ranvier.D5.Graph3D` (3D node/edge/group picking & collision) **and** `Ranvier.D5.Wgpu` (`Probe3D.fs` ray-picking & frustum culling over `Scatter3D`, `Line3D`, `SurfaceMesh`, `IsoSurface3D`) without coupling `Ranvier.D5.Wgpu` to `Ranvier.D5.Graph3D`.
2. Any **matrix seriation / permutation / space-filling curve** primitive placed in **`Ranvier.D5`** immediately upgrades `Ranvier.D5.Charts` (`AdjacencyMatrixSpec`, which currently only supports `MatrixOrder = Input | Label | Degree`, plus heatmaps/dendrograms) **and** `Ranvier.D5.Graph` / `Ranvier.D5.Graph3D`.

### 0.2 Existing Modules vs. Unsolved Gaps

| Package | Already Implemented | Key Missing Capabilities |
| :--- | :--- | :--- |
| **`Ranvier.D5`** | `Spatial` (2D point k-d tree), `Quadtree` (2D point-region), `Delaunay` & `Voronoi` (unweighted 2D, viewport-box clip only), `Treemap` (`Slice`/`Dice`/`SliceDice`/`Binary`/`Squarified`/`resquarify`), `Pack` (Wang 2D circle pack), `Partition` (2D icicle/sunburst), `Tree`/`Cluster` (2D Reingold-Tilford/Buchheim), `Force`, `Chord`, `Sankey`, `Hexbin`, `Density` (2D KDE), `Contour` (Marching Squares), `Sampling` (`MinMax`/`LTTB`), `Curves` (includes single-spline `CurveSpec.Bundle`) | **2D/3D Space-filling curves** (Morton, Hilbert, Moore, Gosper); **Static packed R-tree / Flatbush** (2D/3D AABB & segment indexing); **3D BVH / KDTree3D / metric BallTree**; **Weighted Voronoi (Power Diagrams)**, **Voronoi Treemaps** (Nocaj-Brandes), **Hilbert/Moore/Gosper/Local-Move Stable Treemaps**, **2D/3D Centroidal Voronoi Tessellation (CVT)**; **Matrix Seriation & Optimal Leaf Ordering** |
| **`Ranvier.D5.Charts`** | Cartesian, Radial, Hierarchy, Network, Flow, Density, Contour, `ConnectedScatter`, `Slope`, `Bump`, `Horizon`, `Beeswarm`, `AdjacencyMatrix` (`Input`/`Label`/`Degree` only) | **Seriated/Bi-clustered Matrix & Heatmap**, **UpSet** set-intersection layout, **Area-Proportional Euler/Venn**, **Parallel Sets / Alluvial** |
| **`Ranvier.D5.Geo`** | Projections, spherical clipping, spherical area/length, graticules, GeoJSON | **Spherical Fibonacci** ($O(1)$ forward & inverse binning), **Spherical Delaunay/Voronoi & Spherical CVT**, **Icosahedral Geodesic / Hexagonal DGGS** |
| **`Ranvier.D5.Graph`** | `Sugiyama`, `OrthogonalRouter` (90° rectilinear A*), `Cola` (IPSep-CoLa), `FCose`, `PivotMds`, `Stress2D` (SGD), `ModularityForce2D` (FM³ + FA2/LinLog/FR), `RadialTree`, `BoxPack`, VPSC overlap removal | **Edge Bundling** (Holten HEB LCA router, FDEB, Selassie Divided, MINGLE, KDEEB); **Octilinear Metro-Map Router** (0°/45°/90°/135°); **PRISM** isotropic proximity-stress overlap removal; **GMap / BubbleSets / KelpFusion** cluster hulls; **Leiden** community detection; **Circular crossing-minimized / Arc / Hive / BioFabric** layouts |
| **`Ranvier.D5.Graph3D`** | `Octree` (3D point-region Barnes-Hut), `Hyperbolic3D` (Klein/Poincaré/Minkowski), `H3` (Munzner), `Walrus`, `Force3D`, `Stress3D`, `ConeTree3D` (Cone & Balloon), `Layered3D` | **3D Treemaps** (3D Squarified Cube, StepTree, 2.5D CodeCity, 3D Voronoi Treemap), **3D Sphere Packing**, **3D Edge Bundling** (3D KDEEB / 3D FDEB), **Spherical Geodesic Graph Layout** ($S^2$ positive-curvature counterpart to $H^3$), **2D/3D Manifold Embeddings** (UMAP, Barnes-Hut t-SNE, Isomap, Sammon) |

---

## 1. Space-Filling Curves (2D & 3D) and Spatially Ordered / Temporally Stable 2D Treemaps

### 1.1 Primary Academic Papers & Flagship Prior Art
1. **Morton, G. M. (1966).** *"A Computer Oriented Geodetic Data Base and a New Technique in File Sequencing."* Technical Report, IBM Ottawa, Canada.
2. **Skilling, J. (2004).** *"Programming the Hilbert curve."* *AIP Conference Proceedings*, Vol. 707, pp. 381–387. DOI: `10.1063/1.1751381` *(definitive arbitrary-dimension $D \in \{2, 3\}$ Gray-code state-transposition algorithm without recursive tables)*.
3. **Bederson, B. B., Shneiderman, B., & Wattenberg, M. (2002).** *"Ordered and Quantum Treemaps: Making Effective Use of 2D Space to Display Hierarchies."* *ACM Transactions on Graphics (TOG)*, 21(4), 833–854. *(Introduces Strip, Pivot-by-Middle, and Pivot-by-Split-Size treemaps).*
4. **Tak, S., & Cockburn, A. (2013).** *"Enhanced Spatial Stability with Hilbert and Moore Treemaps."* *IEEE Transactions on Visualization and Computer Graphics (TVCG)*, 19(1), 141–148. DOI: `10.1109/TVCG.2012.108`.
5. **Auber, D., Huet, C., Lambert, A., Renoust, B., Sallaberry, A., & Saulnier, A. (2013).** *"GosperMap: Using a Gosper Curve for Laying Out Hierarchical Data."* *IEEE TVCG*, 19(11), 1820–1832. DOI: `10.1109/TVCG.2013.91`.
6. **Sondag, M., Speckmann, B., & Verbeek, K. (2018).** *"Stable Treemaps via Local Moves."* *IEEE TVCG*, 24(1), 729–738. DOI: `10.1109/TVCG.2017.2745140`.
- **Flagship Prior Art**: `libmorton` (Baert), Google `S2Geometry` (Hilbert curve on cube faces), `Flatbush` (16-bit 2D Hilbert LUT), `Tulip` (`GosperMap` plugin by LaBRI), HPI `webgl-treemaps` (Limberger/Trapp Hilbert & Moore treemaps).

### 1.2 Mathematical Formulation, Invariants & Complexity
- **2D & 3D Morton (Z-Order) Bijection**:
  Quantize finite normalized coordinates $\hat{u} = \lfloor (2^b - 1) \cdot \text{clamp}_0^1((u - u_{\min})/\Delta u) \rfloor \in [0, 2^b - 1]$ ($b = 16$ for 32-bit 2D keys; $b = 26$ for 52-bit IEEE-754 exact-integer 2D keys; $b = 10$ for 30-bit 3D keys or $b = 21$ for 63-bit 3D keys):
  $$M_2(x, y) = \sum_{k=0}^{b-1} \bigl(x_k 2^{2k} + y_k 2^{2k+1}\bigr), \qquad M_3(x, y, z) = \sum_{k=0}^{b-1} \bigl(x_k 2^{3k} + y_k 2^{3k+1} + z_k 2^{3k+2}\bigr)$$
- **2D & 3D Hilbert / Moore Curve (Skilling 2004)**:
  Maps $D$-dimensional $b$-bit integer coordinates $\mathbf{x} \in [0, 2^b - 1]^D$ to a continuous 1D index $H_D(\mathbf{x}) \in [0, 2^{Db} - 1]$ satisfying the **Hölder-$1/D$ locality invariant**:
  $$\|\mathbf{x}_1 - \mathbf{x}_2\|_2 \le c_D \cdot |H_D(\mathbf{x}_1) - H_D(\mathbf{x}_2)|^{1/D} \quad (c_2 = 2, \; c_3 = \sqrt{6})$$
  Moore's curve is the closed-loop variant ($H_{\text{Moore}}(0) \sim H_{\text{Moore}}(2^{2b}-1)$) formed by four rotated $(b-1)$-order Hilbert sub-quadrants, keeping both ends of a linear sequence adjacent near the center/boundary.
- **Hilbert & Moore Treemaps (Tak & Cockburn 2013)**:
  Recursively partitions a rectangle $R$ with quadrant orientation state $Q \in \{U, R, D, L\}$ (for Hilbert) or symmetric ring state (for Moore) into 4 sub-quadrants (or adaptive $1$–$4$ greedy lookahead sub-boxes weighted by child sums $w_i$), placing ordered children sequentially along the curve's quadrant traversal order so item $i$'s center stays anchored to interval $[W_{i-1}/W, W_i/W]$ along the fractal curve. Minimizes Tak & Cockburn's **Location Drift** metric across time-series updates.
- **GosperMap (Auber et al. 2013)**:
  Lays leaves in traversal order along an order-$k$ hexagonal Peano-Gosper curve ($N_{\text{hex}} = 7^k$ regular hexagons, $k = \lceil \log_7(\sum w_i / w_{\min}) \rceil$), assigning each leaf a contiguous run of hexagons proportional to $w_i$, and extracting parent boundaries as topological unions of child hexagonal dual regions followed by corner-chamfer/chaikin smoothing with guaranteed nested containment ($R_{\text{child}} \subseteq R_{\text{parent}}$) and cartographic fractal coastlines.
- **Local-Move Stable Treemaps (Sondag et al. 2018)**:
  Maintains a **sliceable** or general **rectangulation** topology represented by maximal horizontal and vertical segments $S$. When weights $w^{(t)} \to w^{(t+1)}$ change: (1) recompute exact zero-area-error segment positions for the current topology via linear-system/one-dimensional sweeps, then (2) greedily apply at most $K$ local *segment-flip* and *T-junction slide* moves that strictly decrease worst/mean aspect ratio $\max(w/h, h/w)$.
- **Complexity**:
  - Morton/Hilbert encode & decode: $O(1)$ per point ($b$-bit fixed word ops).
  - Strip / Hilbert / Moore Treemaps: $O(N)$ time, $O(N)$ auxiliary space (or $O(N \log N)$ if sorting children).
  - GosperMap: $O(M)$ time and space where $M = 7^k$ is the hexagonal resolution grid.
  - Sondag Local-Move update: $O(N \cdot K)$ per dynamic frame ($K \approx 4$–$8$ local moves).

### 1.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- **Critical Fable JS Constraint**: JavaScript numbers are IEEE-754 binary64 (`53` bits of exact integer precision) and JS bitwise operators (`|`, `&`, `^`, `>>>`) truncate to **32-bit signed/unsigned ints** (`uint64` in Fable compiles to slow boxed `BigInt` or helper objects!).
  - **Dual-Target Design**: Use `uint32` 16-bit-per-axis keys ($65536 \times 65536$ resolution in 2D, $1024^3$ 10-bit-per-axis in 3D) for fast single-word `uint32` operations everywhere, and `struct(uint32 * uint32)` (`Hi`, `Lo`) when 64-bit precision ($2^{32}$ per axis in 2D, $2^{21}$ per axis in 3D) is needed—completely avoiding `uint64` `BigInt` overhead in Fable JS!
  - On **`.NET 10`** (`#if !FABLE_COMPILER`), accelerate batch Morton encoding via `System.Runtime.Intrinsics.X86.Bmi2.ParallelBitDeposit` (`PDEP` — 1 hardware instruction!) and `Vector256<uint32>` LUT-free magic-number bit-dilation shifts (`(x ^^^ (x <<< 8)) &&& 0x00FF00FFu` etc.), processing 8–16 points per SIMD instruction.
- **Flat SoA Treemap Layout**: Operates directly on `Ranvier.D5`'s existing `HierarchyIndex<'Id>` (`Parents: int[]`, `Children: int[][]`, `Postorder: int[]`), writing `Bounds[]` in-place with zero intermediate tuple or list allocations.

### 1.4 Visualization Value & Package Placement
- **Why High-Value**: `Ranvier.D5`'s `Treemap.Squarified` sorts nodes by weight, causing catastrophic layout jumps when weights fluctuate slightly (`resquarify` freezes row topology, causing aspect ratios to degrade unboundedly into thin slivers over time). **Hilbert/Moore Treemaps** and **Sondag Local-Move Treemaps** preserve item order and temporal mental maps simultaneously; **GosperMap** delivers organic map-like hierarchical territories; and **2D/3D Morton & Hilbert curves** are foundational building blocks for `Flatbush` R-trees, `LBVH`, spatial point-cloud sorting (accelerating GPU cache locality in `Marks3D`), and 1D spatial sparklines.
- **Recommended Placement**:
  - `Ranvier.D5/SpaceFilling.fs` (`Morton2D`, `Morton3D`, `Hilbert2D`, `Hilbert3D`, `Moore2D`).
  - `Ranvier.D5/Treemap.fs` (extend `TreemapTiling` with `Strip | PivotMiddle | Hilbert | Moore` + `Treemap.localMove` stateful updater; `GosperMap.fs` in `Ranvier.D5`).

---

## 2. Static Packed Spatial Indexes & Bounding Volume Hierarchies (2D & 3D)

### 2.1 Primary Academic Papers & Flagship Prior Art
1. **Guttman, A. (1984).** *"R-Trees: A Dynamic Index Structure for Spatial Searching."* *Proceedings of ACM SIGMOD*, pp. 47–57.
2. **Beckmann, N., Kriegel, H.-P., Schneider, R., & Seeger, B. (1990).** *"The R\*-tree: An Efficient and Robust Access Method for Points and Rectangles."* *Proceedings of ACM SIGMOD*, pp. 322–331.
3. **Kamel, I., & Faloutsos, C. (1994).** *"Hilbert R-tree: An Improved R-tree Using Fractals."* *Proceedings of the 20th International Conference on Very Large Data Bases (VLDB)*, pp. 500–509.
4. **Leutenegger, S. T., Edgington, J. M., & López, M. A. (1997).** *"STR: A Simple and Efficient Algorithm for R-Tree Packing."* *Proceedings of IEEE ICDE*, pp. 497–506.
5. **Karras, T. (2012).** *"Maximizing Parallelism in the Construction of BVHs, Octrees, and k-d Trees."* *Proceedings of ACM SIGGRAPH / Eurographics High Performance Graphics (HPG)*, pp. 33–37.
6. **Wald, I. (2007).** *"On fast Construction of SAH-based Bounding Volume Hierarchies."* *IEEE Symposium on Interactive Ray Tracing (RT)*, pp. 33–40.
7. **Omohundro, S. M. (1989).** *"Five Balltree Construction Algorithms."* Technical Report TR-89-063, International Computer Science Institute (ICSI), Berkeley.
- **Flagship Prior Art**: `Flatbush` & `KDBush` (Vladimir Agafonkin / Mapbox), `Boost.Geometry.Index` (`rtree<..., rstar<16>>` & packing constructor), Intel `Embree` (SAH BVH & Morton LBVH), `SciPy.spatial` (`cKDTree`, `BallTree`).

### 2.2 Mathematical Formulation, Invariants & Complexity
- **Packed Hilbert R-Tree (`Flatbush` 2D & 3D)**:
  Given $N$ axis-aligned bounding boxes (AABBs) $B_i = [x_{\min,i}, x_{\max,i}] \times [y_{\min,i}, y_{\max,i}]$ ($\times [z_{\min,i}, z_{\max,i}]$ in 3D) and branching factor $M = 16$:
  1. Compute center $(c_{x,i}, c_{y,i})$ and 16-bit Hilbert index $h_i = H_D(c_i)$.
  2. Sort item indices $0 \dots N-1$ by $h_i$ (via 2-pass 8-bit LSD radix sort in **$O(N)$ time**!).
  3. Pack bottom-up level-by-level into a single contiguous array of total capacity $N_{\text{total}} = \sum_{\ell=0}^{L} \lceil N / M^\ell \rceil$: each parent node at level $\ell+1$ stores the tight union AABB $B_P = \bigcup_{k=0}^{M-1} B_{\text{child}_k}$ and child offset `pos`.
  - **Invariant**: Every subtree node's AABB encloses all descendant AABBs; 100% leaf fill (except final node per level), zero pointer overhead, minimal perimeter/area overlap via Hilbert locality.
  - **Complexity**: Build **$O(N)$** (with radix sort on 32-bit Hilbert keys) or $O(N \log N)$; window/intersection query $O(\log_M N + K)$; distance-priority $k$-NN via min-heap on `minDistSq(q, B_node)` in $O(k \log_M N)$.
- **Karras (2012) Linear Bounding Volume Hierarchy (`LBVH` 3D) + SAH Refinement**:
  Given $N$ 3D primitives (`Bounds3`: nodes, cylinders, triangles, text labels):
  1. Compute 30-bit (or 60-bit `struct(uint32*uint32)`) 3D Morton codes of centroids and radix-sort in $O(N)$.
  2. Define longest common prefix $\delta(i, j) = \operatorname{clz}(k_i \oplus k_j)$ (with index tie-breaker $\operatorname{clz}(i \oplus j)$ when $k_i = k_j$, and $\delta(i, j) = -1$ for $j \notin [0, N-1]$).
  3. For each internal node $i \in [0, N-2]$, determine direction $d = \operatorname{sign}(\delta(i, i+1) - \delta(i, i-1))$, upper bound $l_{\max}$ by exponential search, range end $j$, and exact binary-radix split index $\gamma = i + s \cdot d$ via binary search for the largest $s$ satisfying $\delta(i, i + s \cdot d) > \delta(i, j)$. Left child covers $[\min(i,j), \gamma]$, right child covers $[\gamma+1, \max(i,j)]$.
  4. Single bottom-up postorder pass computes exact enclosing `Bounds3` (optionally weighted by Surface Area Heuristic $C_{\text{SAH}} = C_{\text{trav}} + \frac{SA(B_L)}{SA(B_P)} N_L + \frac{SA(B_R)}{SA(B_P)} N_R$ via Wald's 12-bin sweep for static meshes).
  - **Ray-AABB Slab Test (Branchless IEEE-754)**: $t_{\text{enter}} = \max_{a \in \{x,y,z\}} \min(t_{0,a}, t_{1,a})$, $t_{\text{exit}} = \min_{a \in \{x,y,z\}} \max(t_{0,a}, t_{1,a})$, hit iff $t_{\text{exit}} \ge \max(0, t_{\text{enter}})$ and $t_{\text{enter}} \le t_{\max}$.
  - **Complexity**: Build **$O(N)$** time and $2N - 1$ flat nodes; ray/frustum query $O(\log N)$.
- **Metric `BallTree` (Omohundro 1989)**:
  For $N$ points in $\mathbb{R}^D$ (or any metric space $(X, d)$): recursively finds two farthest pivot points $(p_1, p_2)$ in $O(D \cdot |S|)$ (2-sweep diameter heuristic: pick $p_1$ farthest from centroid/seed, $p_2$ farthest from $p_1$), projects points onto $(p_2 - p_1)$ (or generalized hyperplane $d(x, p_1) - d(x, p_2)$), partitions around median via `Quickselect` (already in `Ranvier.D5/Data.fs`!), and stores enclosing ball $(c_v, r_v = \max_{x \in S_v} d(c_v, x))$.
  - **Pruning Invariant**: Prune subtree $v$ during $k$-NN whenever $d(q, c_v) - r_v \ge d_k^{\text{best}}$. Unlike k-d trees (which degrade to $O(N)$ above $D \approx 15$), BallTree adapts to intrinsic manifold dimension $d_{\text{int}} \ll D$.

### 2.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- **`Flatbush` $M = 16$ Wide-SIMD Node Layout**:
  Store SoA arrays `MinX: float[]`, `MinY: float[]`, `MaxX: float[]`, `MaxY: float[]` (plus `MinZ`, `MaxZ` in 3D), `Indices: int[]`, and `LevelBounds: int[]`. Because every internal node's $M = 16$ children sit at contiguous indices `[pos .. pos + 15]`:
  - On **.NET 10 AVX-512** (`Vector512<double>`, 8 lanes $\times 2$) or **AVX2 / ARM64 AdvSIMD** (`Vector256<double>` $\times 4$ / `Vector128<double>` $\times 8$), a **single branchless SIMD test intersects a query box or ray against 8 or 16 child AABBs simultaneously**:
    ```fsharp
    // 8 child AABBs tested in parallel with Vector512<double>:
    let mX0 = Vector512.LessThanOrEqual(vQueryMinX, Vector512.LoadUnsafe(&rMaxX, pos))
    let mX1 = Vector512.GreaterThanOrEqual(vQueryMaxX, Vector512.LoadUnsafe(&rMinX, pos))
    let mY0 = Vector512.LessThanOrEqual(vQueryMinY, Vector512.LoadUnsafe(&rMaxY, pos))
    let mY1 = Vector512.GreaterThanOrEqual(vQueryMaxY, Vector512.LoadUnsafe(&rMinY, pos))
    let mutable hitMask = (mX0 &&& mX1 &&& mY0 &&& mY1).ExtractMostSignificantBits()
    while hitMask <> 0UL do
        let lane = BitOperations.TrailingZeroCount(hitMask)
        stack[sp] <- int pos + lane; sp <- sp + 1
        hitMask <- hitMask &&& (hitMask - 1UL)
    ```
  - Under `#if FABLE_COMPILER`, the exact same flat SoA arrays compile to V8 `Float64Array` / JS arrays with zero object-per-node allocations and explicit `int[]` traversal stack (`stack[64]`).

### 2.4 Visualization Value & Package Placement
- **Why High-Value**:
  1. `Ranvier.D5`'s existing `Spatial` (2D k-d tree) and `Quadtree` **only index 0-area points**, NOT finite rectangles, text labels, polygons, or routed edge segments! Hit-testing 50,000 chart marks (`HitBounds`, `HitRegion`), culling viewport marks on pan/zoom, label-collision placement, and obstacle-aware edge routing all require an AABB spatial index (`RTree2D`).
  2. `Ranvier.D5.Graph3D` (`Octree`) only indexes points; `Bvh3D` / `RTree3D` gives sub-millisecond 3D ray picking, frustum culling, and box-overlap queries for `PositionedNode3D`, `RoutedAssociation3D`, `SurfaceMesh` triangles, and `IsoSurface3D` meshes!
  3. `KDTree3D` completes exact $O(\log N)$ 3D point nearest-neighbor symmetry with 2D `Spatial`, and `BallTree` unlocks high-dimensional $k$-NN for manifold layouts (UMAP/t-SNE/Isomap).
- **Recommended Placement**: **`Ranvier.D5`** (`RTree2D.fs`, `Bvh3D.fs`, `Spatial3D.fs`, `BallTree.fs`) so `Ranvier.D5.Charts`, `Ranvier.D5.Geo`, `Ranvier.D5.Graph`, `Ranvier.D5.Graph3D`, and `Ranvier.D5.Wgpu` all share them with zero duplicate code.

---

## 3. Weighted Voronoi (Power Diagrams), Nocaj-Brandes Voronoi Treemaps, & Centroidal Voronoi Tessellation (CVT) in 2D & 3D

### 3.1 Primary Academic Papers & Flagship Prior Art
1. **Aurenhammer, F. (1987).** *"Power Diagrams: Properties, Algorithms and Applications."* *SIAM Journal on Computing*, 16(1), 78–96. DOI: `10.1137/0216006`.
2. **Du, Q., Faber, V., & Gunzburger, M. (1999).** *"Centroidal Voronoi Tessellations: Applications and Algorithms."* *SIAM Review*, 41(4), 637–676. DOI: `10.1137/S0036144599352836`.
3. **Liu, Y., Wang, W., Lévy, B., Sun, F., Yan, D.-M., Lu, L., & Yang, C. (2009).** *"On Centroidal Voronoi Tessellation — Energy Smoothness and Fast Computation."* *ACM Transactions on Graphics (TOG)*, 28(4), Article 101. *(Quasi-Newton L-BFGS CVT).*
4. **Balzer, M., Deussen, O., & Lewiner, C. (2005).** *"Voronoi Treemaps for the Visualization of Software Metrics."* *Proceedings of ACM Symposium on Software Visualization (SoftVis)*, pp. 157–166; and **Balzer, M., & Deussen, O. (2005)**, *"Voronoi Treemaps"*, *IEEE InfoVis*, pp. 49–56.
5. **Nocaj, A., & Brandes, U. (2012).** *"Computing Voronoi Treemaps: Faster, Simpler, and Resolution-independent."* *Computer Graphics Forum (EuroVis)*, 31(3), 855–864. DOI: `10.1111/j.1467-8659.2012.03078.x`.
6. **Rycroft, C. H. (2009).** *"Voro++: A three-dimensional Voronoi cell library in C++."* *Chaos: An Interdisciplinary Journal of Nonlinear Science*, 19(4), 041111. DOI: `10.1063/1.3215722`.
- **Flagship Prior Art**: `CGAL` (`Regular_triangulation_2`, `Regular_triangulation_3`), `Voro++` (Rycroft — cell-by-cell 3D radical/Laguerre Voronoi), `Geogram` (Bruno Lévy — L-BFGS 2D/3D CVT), `JVoroTreemap` (Nocaj) & `d3-voronoi-treemap` (Kcnarf).

### 3.2 Mathematical Formulation, Invariants & Complexity
- **2D & 3D Power Diagram (Laguerre-Voronoi)**:
  Given sites $s_i \in \mathbb{R}^d$ ($d \in \{2, 3\}$) with scalar weights $w_i \in \mathbb{R}$, the **power distance** is $\pi(x, s_i) = \|x - s_i\|^2 - w_i$. The power cell is convex (possibly empty if $w_i$ is too small relative to neighbors):
  $$\operatorname{Pow}(s_i, w_i) = \bigl\{ x \in \Omega \;\big|\; \langle 2(s_j - s_i), x \rangle \le \|s_j\|^2 - \|s_i\|^2 - (w_j - w_i) \;\;\forall j \neq i \bigr\}$$
  Notice every bisector is a **linear hyperplane** (straight line in 2D, flat plane in 3D), even when weights differ! (By contrast, multiplicatively weighted Voronoi cells have curved Apollonian circle arcs and non-convex disconnected regions.)
  - **Duality Invariant (Aurenhammer 1987)**: Lifting $(x_i, y_i, w_i) \mapsto \hat{s}_i = (x_i, y_i, x_i^2 + y_i^2 - w_i) \in \mathbb{R}^3$ maps the 2D regular triangulation to the lower convex hull in $\mathbb{R}^3$. The exact weighted `inPowerCircle(a, b, c, d)` predicate is a $4 \times 4$ determinant (reducible to $3 \times 3$), which seamlessly extends `Ranvier.D5/Predicates.fs` (`ExactDyadic` / adaptive Shewchuk expansion)!
- **Nocaj & Brandes (2012) Resolution-Independent Voronoi Treemap**:
  Recursively subdivides any convex parent polygon $\Omega_P$ (starting from a rectangle, circle, regular $k$-gon, or arbitrary convex hull) among $m$ children with target area fractions $\alpha_i = v_i / \sum_j v_j$ ($A_i^* = \alpha_i \operatorname{Area}(\Omega_P)$):
  1. Compute exact clipped power diagram $\{\Omega_i = \operatorname{Pow}(s_i, w_i) \cap \Omega_P\}$ in $O(m \log m)$ using lifted convex hull / Bowyer-Watson regular triangulation + Sutherland-Hodgman convex polygon clipping (directly generalizing `Voronoi.fs` from 4-vertex rectangles to $k$-vertex convex polygons!).
  2. **Position Adaptation (Centroidal step)**: Move $s_i \leftarrow c(\Omega_i)$ (exact polygon centroid via shoelace formula) and bound $w_i \le r_{\text{inscribed}}(\Omega_i)^2$ to prevent site jumping/cell starvation.
  3. **Weight Adaptation**: Update $w_i \leftarrow w_i \cdot (A_i^* / \operatorname{Area}(\Omega_i))$ (clamped to prevent empty cells: for adjacent $(i, j)$, $w_i - w_j < \|s_i - s_j\|^2$) until relative area error $\sum_i |\operatorname{Area}(\Omega_i) - A_i^*| / \operatorname{Area}(\Omega_P) < \varepsilon$ (typically $\varepsilon = 10^{-3}$ in $15$–$35$ iterations).
  - **Total Complexity**: $O(I \cdot N \log d_{\max})$ where $d_{\max}$ is max branching degree of a single parent node (often $d_{\max} \le 50$, making each parent solve sub-millisecond!).
- **2D & 3D Centroidal Voronoi Tessellation (Lloyd & Liu et al. L-BFGS)**:
  Minimizes $F(\mathbf{S}) = \sum_{i=1}^n \int_{\Omega_i} \rho(x) \|x - s_i\|^2 \, dx$. By Liu et al. (2009) Theorem 1, $F(\mathbf{S})$ is **$C^2$-continuous** almost everywhere with exact analytical gradient:
  $$\nabla_{s_i} F(\mathbf{S}) = 2 m_i (s_i - c_i), \qquad m_i = \int_{\Omega_i} \rho(x)\,dx, \quad c_i = \frac{1}{m_i}\int_{\Omega_i} x \rho(x)\,dx$$
  - **3D Radical Voronoi (`Voro++` Cell-by-Cell Method, Rycroft 2009)**: Instead of building a fragile global 3D tetrahedral mesh with degenerate 5-point co-spherical flips, initialize cell $i$ as the bounding box `Bounds3` (or parent convex polyhedron) stored in a tiny reusable flat vertex/plane table (`maxVerts = 128`, 3-valent vertex adjacency `ed[v * 3 + 0..2]`). Query neighbors in increasing distance order via `Spatial3D` / spatial grid, half-space clip against bisector plane $\pi(x, s_i) = \pi(x, s_j)$, and **terminate early** as soon as $\|s_j - s_i\| - \frac{w_j - w_i}{\|s_j - s_i\|} > 2 R_{\max}(\text{cell}_i)$!
  - **Complexity**: 2D CVT $O(I \cdot N \log N)$; 3D Voro++ cell-by-cell expected **$O(N)$** for quasi-uniform / bounded-degree distributions!

### 3.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- **Zero-Allocation Polygon & Polyhedron Clipping Arenas**:
  - In 2D (`PowerDiagram.fs` / `VoronoiTreemap.fs`), reuse double-buffered flat `Point[]` ring arenas (`curPoly`, `nextPoly` — already proven in `Voronoi.fs` lines 71–103!) for arbitrary convex parent polygons. Polygon area & centroid (`shoelace`) vectorizes across vertices via `Vector256<double>` cross-products $x_k y_{k+1} - x_{k+1} y_k$.
  - In 3D (`Voronoi3D.fs`), Rycroft's cell-by-cell clipper operates on a **single pre-allocated thread-local/workspace struct arena** (~8 KB: `vx: float[256]`, `vy: float[256]`, `vz: float[256]`, `nbr: int[256 * 3]`), requiring **zero heap allocations per cell** on both .NET 10 and Fable JS, and trivially parallelizing across cells if desired!
  - L-BFGS history (`m = 7` vector pairs `s_hist[7][d*N]`, `y_hist[7][d*N]`) reuses `SimdMath.dot` and `SimdMath.axpy` directly, converging $5\times$–$10\times$ faster than plain Lloyd's iteration.

### 3.4 Visualization Value & Package Placement
- **Why High-Value**:
  1. **Voronoi Treemaps** eliminate the "nested grid of boxes" ambiguity of rectangular treemaps (where aligned corners hide hierarchy depth), achieve near-isotropic $\approx 1.0$ aspect ratios, fit arbitrary circular/map boundaries, and are one of the most requested enterprise visualization types missing from `Ranvier.D5`.
  2. **2D & 3D CVT** provides blue-noise point relaxation, non-overlapping glyph/stipple placement weighted by `DensityGrid` / `VolumeGrid`, uniform 3D spatial sampling, and **3D Volumetric Voronoi Treemaps** (sliced or rendered semi-transparently in `Ranvier.D5.Wgpu`).
- **Recommended Placement**:
  - `Ranvier.D5`: `PowerDiagram.fs`, `VoronoiTreemap.fs`, `Cvt2D.fs`.
  - `Ranvier.D5` or `Ranvier.D5.Graph3D`: `Voronoi3D.fs` (Voro++ radical polyhedral clipper + `Cvt3D` + `VoronoiTreemap3D`; placing `Voronoi3D` in `Ranvier.D5` lets `Ranvier.D5.Wgpu` render 3D Voronoi/foam meshes directly!).

---

## 4. 3D Hierarchical Space-Filling Layouts (3D Cube Treemaps, StepTree, 2.5D CodeCity, & 3D Sphere Packing)

### 4.1 Primary Academic Papers & Flagship Prior Art
1. **Rekimoto, J., & Green, M. (1993).** *"The Information Cube: Using Transparency in 3D Information Visualization."* *Proceedings of the 3rd Annual Workshop on Information Technologies & Systems (WITS)*, pp. 125–132.
2. **Bladh, T., Carr, D. A., & Scholl, J. (2004).** *"Extending Tree-Maps to Three Dimensions: A Comparative Study."* *Proceedings of the 6th Asia-Pacific Conference on Computer-Human Interaction (APCHI 2004)*, LNCS 3101, pp. 50–59. *(StepTree).*
3. **Wettel, R., & Lanza, M. (2007 / 2008).** *"Visualizing Software Systems as Cities"* (*IEEE VISSOFT 2007*, pp. 92–99) and *"CodeCity: 3D Visualization of Large-Scale Software"* (*ACM/IEEE ICSE 2008*, pp. 921–922).
4. **Schulz, H.-J., Hadlak, S., & Schumann, H. (2011).** *"The Design Space of Implicit Hierarchy Visualization: A Survey."* *IEEE TVCG*, 17(4), 393–411. DOI: `10.1109/TVCG.2010.79`.
5. **Limberger, D., Scheibel, W., Trapp, M., & Döllner, J. (2017 / 2023).** *"Mixed-Projection Treemaps"* & *"Interactive Rendering of Complex 2.5D/3D Treemaps."* Hasso Plattner Institute (*CGF* / *VINCI*).
6. **Wang, W., Wang, H., Dai, G., & Wang, H. (2006).** *"Visualization of Large Hierarchical Data by Circle Packing."* *ACM CHI*, pp. 517–520 — generalized to **3D Front-Surface Tangent Sphere Packing** + **Welzl (1991)** *"Smallest enclosing disks (balls and ellipsoids)"*, *New Results and New Trends in Computer Science*, LNCS 555, pp. 359–370.
- **Flagship Prior Art**: `CodeCity`, HPI `webgl-treemaps` (`treemap-3d`), `Sonargraph` 3D Architecture City, `Tulip` 3D hierarchy views.

### 4.2 Mathematical Formulation, Invariants & Complexity
- **3D Squarified Cube Treemap (`CubeTreemap3D`)**:
  Given `Bounds3` $B = [x_0, x_1] \times [y_0, y_1] \times [z_0, z_1]$ with extents $(W, H, D)$ and children weights $w_0 \ge w_1 \ge \dots \ge w_{m-1}$ summing to $V_{\text{sum}}$:
  1. Scale weights to target volumes $v_i = (W H D) \cdot w_i / V_{\text{sum}}$.
  2. Choose slicing normal along the longest extent (say $W = \max(W, H, D)$), cutting a planar slab of cross-section $(H, D)$ and thickness $t = \sum_{j=a}^{b} v_j / (H D)$.
  3. Within the slab $(H, D)$, apply 2D squarification (`Treemap.squarify`) to partition $[a..b]$ into cuboids $(t, h_j, d_j)$, greedily extending $b$ while the worst **3D cuboid aspect ratio**
     $$\rho_3(t, h_j, d_j) = \frac{\max(t, h_j, d_j)}{\min(t, h_j, d_j)} \ge 1$$
     decreases! Inset child cuboids by `Padding3D` (or exploded view factor $\lambda \in [0, 1]$ around parent centroid so interior cubes remain visible!).
  - **Invariant**: Disjoint `Bounds3` cuboids strictly nested inside parent `Bounds3`, volume-proportional at zero padding, deterministic $O(N \log d_{\max})$ time and $O(N)$ space.
- **StepTree (Bladh et al. 2004) & 2.5D CodeCity (Wettel & Lanza 2007)**:
  Solves volumetric occlusion by separating XY footprint from Z elevation:
  - **StepTree**: Computes nested padded 2D treemap footprints $(x_i, y_i, w_i, d_i)$ in XY and stacks Z-slabs monotonically by hierarchy depth: $z_i \in [d_{\text{depth}} \cdot \Delta z, (d_{\text{depth}} + 1) \cdot \Delta z]$.
  - **CodeCity (2.5D Bivariate Treemap / Street-District Packing)**: Internal package nodes form stepped district plinths of height $\Delta z_{\text{pkg}}$ with street margins; leaf nodes become 3D buildings whose XY base area encodes metric 1 ($A_i \propto \text{Weight}_i$, via squarified treemap or modified strip/bin shelf packer preserving aspect ratios when leaf dimensions are pre-specified) and whose Z height encodes an **independent metric 2** ($h_i = \text{Scale}_Z(\text{HeightMetric}_i)$)!
- **3D Nested Sphere Packing (`Pack3D`)**:
  Postorder bottom-up over `HierarchyIndex<'Id>`: leaf $i$ gets sphere radius $r_i \propto v_i^{1/3}$; internal node packs child spheres $\{S_1, \dots, S_m\}$ tangentially around origin using 3D advancing-front tangent triples (given mutually tangent spheres $(c_a, r_a), (c_b, r_b), (c_c, r_c)$, solve quadratic intersection of $\|x - c_k\|^2 = (r_k + r_{\text{new}})^2$ for $k \in \{a,b,c\}$ closest to cluster centroid and verify collision-freeness via local spatial grid), then computes exact **Welzl (1991)** smallest enclosing ball $(C_P, R_P)$ in expected $O(m)$ time!

### 4.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- Operates directly on `HierarchyIndex<'Id>` using flat `Bounds3[]` (or `Cx[], Cy[], Cz[], Radius[]` for `Pack3D`), emitting `DiagramLayout3D` / `PositionedNode3D` or direct GPU instance buffers (`Marks3D` cuboid/sphere instancing in `Ranvier.D5.Wgpu`!). Zero recursion on call stack via explicit `Stack<int>` / `Postorder: int[]`.

### 4.4 Visualization Value & Package Placement
- **Why High-Value**: `Ranvier.D5.Graph3D` has node-link 3D trees (`ConeTree3D`, `H3`, `Walrus`, `Layered3D`), yet **zero 3D space-filling or 2.5D architectural hierarchy layouts**. `CodeCity` / `StepTree` / `CubeTreemap3D` + `Pack3D` turn `Ranvier.D5.Wgpu` into a first-class engine for software architecture telemetry, memory/bundle profilers, financial risk cubes, and biological ontologies.
- **Recommended Placement**: **`Ranvier.D5.Graph3D`** (`Treemap3D.fs` covering `CubeSquarified`, `CubeHilbert`, `StepTree`, and `CodeCity`; `Pack3D.fs` for 3D nested sphere packing).

---

## 5. Edge Bundling in 2D, Geo, and 3D (HEB, FDEB, Divided Edge Bundling, MINGLE, & KDEEB)

### 5.1 Primary Academic Papers & Flagship Prior Art
1. **Holten, D. (2006).** *"Hierarchical Edge Bundles: Visualization of Adjacency Relations in Hierarchical Data."* *IEEE TVCG*, 12(5), 741–748. DOI: `10.1109/TVCG.2006.147`.
2. **Holten, D., & van Wijk, J. J. (2009).** *"Force-Directed Edge Bundling for Graph Visualization."* *Computer Graphics Forum (EuroVis)*, 28(3), 983–990. DOI: `10.1111/j.1467-8659.2009.01450.x`.
3. **Selassie, D., Heller, B., & Heer, J. (2011).** *"Divided Edge Bundling for Directional Network Data."* *IEEE TVCG*, 17(12), 2354–2363. DOI: `10.1109/TVCG.2011.190`.
4. **Gansner, E. R., Hu, Y., North, S. C., & Scheidegger, C. (2011).** *"Multilevel Agglomerative Edge Bundling for Visualizing Large Graphs."* *IEEE Pacific Visualization Symposium (PacificVis)*, pp. 187–194. *(MINGLE).*
5. **Hurter, C., Ersoy, O., & Telea, A. (2012).** *"Graph Bundling by Kernel Density Estimation."* *Computer Graphics Forum (EuroVis)*, 31(3), 865–874. *(KDEEB; extended to 3D/GPU in **van der Zwan, M., Codreanu, V., & Telea, A. 2016**, "CUBu", IEEE TVCG 22(12), 2550–2563).*
- **Flagship Prior Art**: Graphviz `mingle`, Deck.gl / `philogb/mingle`, D3 `d3-hierarchy` (`node.path` + `curveBundle`), `Tulip` & `Gephi` edge-bundling plugins, `Cytoscape` (`EdgeBundling`).

### 5.2 Mathematical Formulation, Invariants & Complexity
- **1. Hierarchical Edge Bundling (Holten 2006)**:
  Given node positions laid out by `Tree`, `Cluster`, `RadialTree`, `Treemap`, `Pack`, or `ConeTree3D` (in 2D or 3D!), and cross-hierarchy adjacency links $(u, v)$:
  1. Compute unique tree path $P(u, v) = (u = p_0, p_1, \dots, p_{\text{LCA}}, \dots, p_k = v)$ via depth-aligned parent pointers in `HierarchyIndex<'Id>` (removing `p_LCA` when $k=2$? Holten removes LCA only when $|P| = 3$ and siblings share parent if configured, or retains full control polygon).
  2. Straighten control points with bundling strength $\beta \in [0, 1]$ (note `Curves.fs` already implements 2D `CurveSpec.Bundle beta` for 2D paths, while 3D uniform cubic B-spline evaluation + $\beta$-straightening extends it directly to `Point3Chain` in 3D!).
  - **Complexity**: $O(E \cdot H)$ time ($H$ = tree height), $O(E \cdot H)$ output points.
- **2. Force-Directed & Divided Edge Bundling (Holten & van Wijk 2009; Selassie et al. 2011)**:
  Subdivide each edge $P \in E$ into $S_c$ segments ($S_0 = 1$, doubling over $c = 0 \dots C-1$ cycles, typically $C=6 \Rightarrow 32$ segments).
  - **Pairwise Compatibility** $C_e(P, Q) = C_a(P, Q) \cdot C_s(P, Q) \cdot C_p(P, Q) \cdot C_v(P, Q) \in [0, 1]$:
    - Angle: $C_a = |\cos \alpha|$ (FDEB) vs. **Selassie Directional**: $C_d = \frac{1 + \cos\alpha}{2}$ for same-lane attraction and **normal lane-separation offset** $\delta_{\perp}$ when $\cos\alpha < 0$, splitting opposing $u \to v$ and $v \to u$ traffic into right-hand parallel lanes!
    - Scale: $C_s = \frac{2}{l_{\text{avg}} \min(|P|,|Q|)^{-1} + \max(|P|,|Q|) l_{\text{avg}}^{-1}}$; Position: $C_p = \frac{l_{\text{avg}}}{l_{\text{avg}} + \|P_m - Q_m\|}$; Visibility: $C_v$.
  - **Force Update**: On internal subdivision point $p_i$ ($1 \le i < S_c$), spring force $k_P (\|p_{i-1} - p_i\| \hat{u}_{i-} + \|p_{i+1} - p_i\| \hat{u}_{i+})$ + electrostatic attraction/lane-offset over compatible pairs $(P, Q)$ with $C_e(P, Q) \ge \tau$.
  - **Complexity**: $O(E^2 + I \cdot |E_{\text{compat}}| \cdot S)$ — accelerated to $O(E \log E + I \cdot k E S)$ by indexing edge midpoints/lengths in `RTree2D` / `Spatial`!
- **3. MINGLE (Gansner et al. 2011)**:
  Models each edge $e = (u, v)$ as a 4D vector $(u_x, u_y, v_x, v_y) \in \mathbb{R}^4$ (or 6D in 3D). Uses `BallTree` / 4D k-d tree to find $k$ nearest neighbor edges, greedily merging bundles into a coarsened routing tree whenever **ink saved** $\Delta \text{ink} > 0$ subject to maximum turning angle $\theta \le \theta_{\max}$ (computed analytically via golden-section search on meet-points $(m_u, m_v)$).
  - **Complexity**: **$O(E \log E)$** — scales to $10^5+$ edges without grids or pairwise $O(E^2)$ compatibility!
- **4. KDEEB in 2D & 3D (Hurter et al. 2012)**:
  1. Discretize edges into sample points $\{x_{e,s}\}$.
  2. Accumulate point weights into a 2D `DensityGrid` ($G \times G$, e.g. $256^2$) or 3D `VolumeGrid` ($G^3$, e.g. $96^3$) via bilinear/trilinear splatting.
  3. Apply separable 1D Gaussian/box-filter passes along X, Y (, Z) with bandwidth $h_t = \lambda^t h_0$ ($\lambda \approx 0.75$).
  4. Compute finite-difference gradient $\mathbf{g}(x) = \nabla \rho(x)$, advect interior sample points $x_{e,s} \leftarrow x_{e,s} + \eta_t \frac{\mathbf{g}(x_{e,s})}{\|\mathbf{g}(x_{e,s})\| + \varepsilon}$, and resample polylines to uniform arc-length spacing + Laplacian smoothing.
  - **Complexity**: **$O\bigl(I \cdot (E \cdot S + G^d)\bigr)$** — strictly **linear in $|E|$**, zero pairwise edge comparisons, natively supports **obstacle avoidance** (subtract repulsive node mask from $\rho$!) and works identically in **2D, Projected Geo, and 3D (`Point3Chain`)**!

### 5.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- **Flat Polyline Arena**: Store all $E$ subdivided polylines in contiguous SoA arrays `px: float[E * stride]`, `py: float[E * stride]` (`pz: float[E * stride]` in 3D).
- **KDEEB Separable Convolution & Gradient**: Row/column/slice 3-pass box-blur (approximating Gaussian to $<1\%$ error in $O(1)$ per cell independent of kernel radius $h_t$!) vectorizes with `Vector512<float>` / `Vector256<float>` across columns/slices in `.NET 10` and runs at $>60\text{ fps}$ for 10,000 edges even in single-threaded Fable JS!
- **FDEB/Divided Compatibility CSR**: Pre-filter compatible edge pairs $(P, Q)$ with $C_e \ge \tau$ into a static CSR adjacency table `(compatRowPtr: int[], compatColIdx: int[], compatWeight: float32[])` once before subdivision cycles, turning inner force loops into contiguous SIMD stream gathers.

### 5.4 Visualization Value & Package Placement
- **Why High-Value**: Dense node-link graphs, origin-destination geographic flow maps (`Ranvier.D5.Geo`), and **especially 3D network layouts** (`Force3D`, `Stress3D`, `SphericalLayout3D` — where unbundled 3D lines create an impenetrable volumetric "furball") become dramatically legible once edges coalesce into arterial bundles. Moreover, Holten HEB bridges `Ranvier.D5`'s hierarchy layouts and `Curves.Bundle` in ~150 lines of clean code!
- **Recommended Placement**:
  - `Ranvier.D5.Graph/EdgeBundling.fs` (`HierarchicalBundle`, `DividedForceBundle`, `MingleBundle`, `KdeBundle2D`).
  - `Ranvier.D5.Graph3D/EdgeBundling3D.fs` (`HierarchicalBundle3D`, `KdeBundle3D`, `ForceBundle3D` producing `Point3Chain` routes on `DiagramLayout3D`).

---

## 6. Matrix Seriation, Bandwidth Reduction, Optimal Leaf Ordering, Bi-Clustering, & Leiden Community Organization

### 6.1 Primary Academic Papers & Flagship Prior Art
1. **Cuthill, E., & McKee, J. (1969).** *"Reducing the bandwidth of sparse symmetric matrices."* *Proceedings of the 24th ACM National Conference*, pp. 157–172; **George, A. (1971)**, *Computer Implementation of the Finite Element Method*, Stanford CS-TR-71-208 *(Reverse Cuthill-McKee, RCM)*.
2. **Atkins, J. E., Boman, E. G., & Hendrickson, B. (1998).** *"A Spectral Algorithm for Seriation and the Consecutive Ones Problem."* *SIAM Journal on Computing*, 28(1), 297–310. DOI: `10.1137/S0097539795285771` *(Fiedler eigenvector exact pre-R-matrix seriation; see also Barnard, Pothen & Simon 1995)*.
3. **Bar-Joseph, Z., Gifford, D. K., & Jaakkola, T. S. (2001).** *"Fast optimal leaf ordering for hierarchical clustering."* *Bioinformatics*, 17(Suppl 1), S22–S29. DOI: `10.1093/bioinformatics/17.suppl_1.S22`.
4. **Dhillon, I. S. (2001).** *"Co-clustering documents and words using Bipartite Spectral Graph Partitioning."* *Proceedings of ACM SIGKDD*, pp. 269–274; and **Mäkinen, E., & Siirtola, H. (2005)**, *"The Barycenter Heuristic and the Reorderedable Matrix"*, *Informatica*, 29(3), 357–363.
5. **Behrisch, M., Bach, B., Henry Riche, N., Schreck, T., & Fekete, J.-D. (2016).** *"Matrix Reordering Methods for Table and Network Visualization."* *Computer Graphics Forum (EuroVis)*, 35(3), 693–716.
6. **Traag, V. A., Waltman, L., & van Eck, N. J. (2019).** *"From Louvain to Leiden: guaranteeing well-connected communities."* *Scientific Reports*, 9, Article 5233. DOI: `10.1038/s41598-019-41695-z`.
- **Flagship Prior Art**: `Reorder.js` (Jean-Daniel Fekete), `SciPy` (`scipy.sparse.csgraph.reverse_cuthill_mckee`, `scipy.cluster.hierarchy.optimal_leaf_ordering`), R `seriation` (Hahsler et al.), `leidenalg` / `igraph`.

### 6.2 Mathematical Formulation, Invariants & Complexity
- **Reverse Cuthill-McKee (`RCM`)**:
  Minimizes symmetric matrix **bandwidth** $\beta(\pi) = \max_{(i,j) \in E} |\pi(i) - \pi(j)|$ and profile/envelope:
  1. Find a **pseudo-peripheral root** $r$ per connected component via Gibbs-Poole-Stockmeyer (1976) iterative eccentricity level-set narrowing (typically converges in $2$–$4$ BFS sweeps).
  2. Run BFS from $r$, enqueuing unvisited neighbors of each popped vertex in **ascending degree order** (deterministic ordinal ID tie-break).
  3. Reverse the resulting array per component.
  - **Complexity**: **$O(V + E \log d_{\max})$** time, $O(V)$ scratch space.
- **Spectral Fiedler Seriation (Atkins et al. 1998)**:
  Minimizes weighted quadratic 2-sum $\sum_{(i,j) \in E} w_{ij} (x_i - x_j)^2 = \mathbf{x}^T L \mathbf{x}$ subject to $\mathbf{x} \perp \mathbf{1}, \|\mathbf{x}\|_2 = 1$, where $L = D - W$ is the graph Laplacian.
  - **Exactness Invariant (Atkins et al. Theorem 3.3)**: If similarity matrix $S$ is pre-Robinson ($S_{\pi(i),\pi(j)}$ decreases monotonically away from the diagonal under some unknown permutation $\pi$), sorting vertices by the Fiedler eigenvector $\mathbf{v}_2$ ($L \mathbf{v}_2 = \lambda_2 \mathbf{v}_2$) recovers $\pi$ (up to reversal) **exactly** in polynomial time!
  - **Complexity**: Computed via Lanczos tridiagonalization or orthogonalized power iteration on $(\lambda_{\max} I - L)$ in $O(k(V + E))$ per component.
- **Optimal Leaf Ordering (`OLO`, Bar-Joseph et al. 2001)**:
  Given a binary dendrogram $T$ with $n$ leaves (which admits $2^{n-1}$ valid linear leaf orders via left/right child flips) and pairwise similarity/distance matrix $D(u, w)$:
  - Bottom-up DP computes for every internal node $v$ and pair $u \in L(v_{\ell}), w \in L(v_r)$:
    $$M(v, u, w) = \min_{m \in L(v_\ell), \, k \in L(v_r)} \bigl( M(v_\ell, u, m) + D(m, k) + M(v_r, k, w) \bigr)$$
    and symmetrically $M(v, w, u)$. Pre-sorting candidate $m$ by $M(v_\ell, u, m)$ and pruning whenever $M(v_\ell, u, m) + \min_{k} M(v_r, k, w) + D_{\min}(v_\ell, v_r) \ge \text{best}$ reduces empirical runtime from $O(n^3)$ (after Demaine's two-stage split $T(v, u, k) = \min_m (M(v_\ell, u, m) + D(m, k))$ which already lowers naive $O(n^4)$ to **strict worst-case $O(n^3)$**!) down to **$O(n^2 \log n)$ typical**!
- **Two-Way Bi-Clustering (Dhillon 2001 & Alternating Barycenter)**:
  For rectangular $R \times C$ data tables / bipartite graphs $A \in \mathbb{R}_{\ge 0}^{R \times C}$:
  - **Spectral Co-clustering**: Compute second left/right singular vectors $(\mathbf{u}_2, \mathbf{v}_2)$ of normalized matrix $A_n = D_R^{-1/2} A D_C^{-1/2}$ via alternating Lanczos $A_n \mathbf{v}, A_n^T \mathbf{u}$ in $O(k \cdot \operatorname{nnz}(A))$, sorting rows by $D_R^{-1/2} \mathbf{u}_2$ and columns by $D_C^{-1/2} \mathbf{v}_2$.
  - **Barycentric Crossing/Band Polish**: Alternate row barycenters $b_r = \frac{\sum_c \pi_C(c) A_{rc}}{\sum_c A_{rc}}$ and column barycenters $b_c$ for $8$–$16$ sweeps.
- **Leiden Community Detection (Traag et al. 2019)**:
  Optimizes Modularity $Q = \frac{1}{2m} \sum_{c} \bigl(E_c - \gamma \frac{K_c^2}{2m}\bigr)$ or Constant Potts Model (CPM) in three phases per level:
  1. **Fast Local Moving**: Maintain queue of only vertices whose neighborhood community changed (unlike Louvain's full sweeps!).
  2. **Refinement Phase**: Within each partition $P \in \mathcal{P}$, split and merge locally into refined sub-communities $\mathcal{P}_{\text{refined}}$ using $\theta$-boltzmann / deterministic best-connected moves **restricted inside $P$**, mathematically **guaranteeing every community is $\gamma$-connected** (fixing Louvain's notorious disconnected/badly-connected community bug!).
  3. **Aggregate CSR Network** on $\mathcal{P}_{\text{refined}}$ while initializing next-level partition from $\mathcal{P}$.
  - **Complexity**: Empirical **$O(V + E)$**!

### 6.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- **CSR SpMV + `SimdMath` Lanczos**: Fiedler & Dhillon spectral seriation use flat `CsrGraph` (`RowPtr: int[]`, `ColIdx: int[]`, `Weights: float[]`) + `SimdMath.dot` / `SimdMath.axpy` (`Vector512`/`Vector256`/`Vector128`) with full Gram-Schmidt re-orthogonalization against the small Krylov basis ($m \le 32$), requiring zero external BLAS/LAPACK!
- **OLO Two-Stage $O(n^3)$ Flat Arena**: Because disjoint sibling subtrees $L(v_\ell) \times L(v_r)$ partition all unordered leaf pairs $\binom{n}{2}$ **exactly once across the entire tree**, the entire DP table for all internal nodes fits inside a **single flat $n \times n$ `float[]` array** (`dp[u * n + w]`) plus one $n$-element scratch row! Zero per-node allocations!

### 6.4 Visualization Value & Package Placement
- **Why High-Value**: Right now, `Ranvier.D5.Charts`'s `AdjacencyMatrixSpec` only offers `Input | Label | Degree` — producing unreadable salt-and-pepper noise on structured networks! Adding `RCM` (diagonal band), `Spectral` (smooth gradient/Robinson blocks), `OptimalLeafOrder` (cluster blocks + dendrograms), `Bicluster` (rectangular heatmaps), and `Leiden` (automatic community discovery, chord diagram sector ordering, hierarchical synthesis for `HierarchicalBundle` and `Treemap`, and automatic cluster hulls!) transforms data organization across the entire `Ranvier.D5` stack.
- **Recommended Placement**:
  - **`Ranvier.D5/Seriation.fs`**: `ReverseCuthillMcKee`, `SpectralFiedler`, `OptimalLeafOrder`, `Bicluster`, `HierarchicalCluster` (agglomerative Ward/UPGMA Lance-Williams O(N²) chain) — directly consumable by `Ranvier.D5.Charts` (`MatrixOrder` extended!) AND `Ranvier.D5.Graph`.
  - **`Ranvier.D5.Graph/Community.fs`**: `Leiden` (deterministic seed, Modularity & CPM quality functions, multi-level hierarchy output as `HierarchyItem<string>`!).

---

## 7. Proximity-Stress Overlap Removal (PRISM), Cluster Map Metaphors (GMap, BubbleSets, KelpFusion), & Set Visualizations (UpSet, Area-Proportional Euler/Venn)

### 7.1 Primary Academic Papers & Flagship Prior Art
1. **Gansner, E. R., & Hu, Y. (2008 / 2010).** *"Efficient, Proximity-Preserving Node Overlap Removal."* *JGAA*, 14(1), 53–74 (preliminary *GD 2008*, LNCS 5417). *(PRISM).*
2. **Gansner, E. R., Hu, Y., & Kobourov, S. (2010).** *"GMap: Visualizing Graphs and Clusters as Maps."* *IEEE Pacific Visualization Symposium (PacificVis)*, pp. 201–208.
3. **Collins, C., Penn, G., & Carpendale, S. (2009).** *"Bubble Sets: Revealing Set Relations with Isocontours over Existing Visualizations."* *IEEE TVCG*, 15(6), 1009–1016.
4. **Meulemans, W., Henry Riche, N., Speckmann, B., Alper, B., & Dwyer, T. (2013).** *"KelpFusion: A Hybrid Set Visualization Technique."* *IEEE TVCG*, 19(11), 1846–1858.
5. **Lex, A., Gehlenborg, N., Strobelt, H., Vuillemot, R., & Pfister, H. (2014).** *"UpSet: Visualization of Intersecting Sets."* *IEEE TVCG*, 20(12), 1983–1992.
6. **Wilkinson, L. (2012).** *"Exact and Approximate Area-Proportional Circular Venn and Euler Diagrams."* *IEEE TVCG*, 18(2), 321–331; and **Larsson, J., & Gustafsson, P. (2018)**, *"A Case Study in Fitting Area-Proportional Euler Diagrams with Ellipses Using eulerr"*, *SetVR*.
- **Flagship Prior Art**: Graphviz (`overlap=prism`, `gvmap`), `Tulip` (`BubbleSets`), `UpSetR` / `@upsetjs/bundle`, `venn.js` (Ben Frederickson) & R `eulerr`.

### 7.2 Mathematical Formulation, Invariants & Complexity
- **PRISM Proximity-Stress Overlap Removal (Gansner & Hu 2010)**:
  Given overlapping boxes $B_i = (x_i \pm w_i/2, y_i \pm h_i/2)$ from any 2D layout (`Force`, `Stress2D`, `UMAP`, `t-SNE`, text labels):
  1. Build exact `Delaunay` triangulation $DT(X^{(t)})$ of box centers in $O(V \log V)$.
  2. For each Delaunay edge $(i, j) \in E_{DT}$ ($|E_{DT}| \le 3V - 6$), compute exact ray-box overlap factor:
     $$t_{ij} = \max\!\left(\frac{(w_i + w_j)/2 + \text{pad}}{|x_i - x_j|},\; \text{if } \text{overlap}(B_i, B_j) \text{ then } \min\!\left(\frac{(w_i + w_j)/2 + \text{pad}}{|x_i - x_j|},\; \frac{(h_i + h_j)/2 + \text{pad}}{|y_i - y_j|}\right) \text{ else } 1\right)$$
     and clamped per-iteration expansion $s_{ij} = \min(s_{\max}, \max(1, t_{ij}))$ ($s_{\max} = 1.5$), setting target edge length $d_{ij}^* = s_{ij} \|x_i - x_j\|$ and weight $w_{ij} = (d_{ij}^*)^{-2}$.
  3. Minimize sparse proximity stress $\sum_{(i,j) \in E_{DT}} w_{ij}\bigl(\|x_i - x_j\| - d_{ij}^*\bigr)^2$ in $O(V)$ via conjugate gradient or SGD, and rebuild $DT$ for $t = 1 \dots T$ until maximum pairwise overlap $= 0$ (plus final scanline sweep guarantee).
  - **Why Superior to VPSC Alone**: VPSC separates X then Y independently along orthogonal axes, shearing circular/organic layouts into artificial rectilinear bands. PRISM expands **isotropically** in all radial directions, preserving rotational neighborhoods and aspect ratio! (Generalizes cleanly to 3D `Bounds3` via 3D k-NN / Gabriel edges!).
- **GMap Cartographic Cluster Overlay (Gansner, Hu & Kobourov 2010)**:
  Given positioned nodes with cluster IDs $c_i$:
  1. Sample perimeter points around each node box $B_i$ (tagged with $c_i$) + uniform/perturbed background "ocean" guard points outside a margin radius $R_{\text{coast}}$ from all nodes (tagged `Ocean = -1`).
  2. Compute exact `Delaunay` + `Voronoi.clip` over all real + guard points in $O(M \log M)$.
  3. Extract boundary half-edges where `cluster(siteA) <> cluster(siteB)` and chain directed half-edges into closed country polygons (with interior enclave holes supported automatically!) smoothed via Chaikin corner-cutting that preserves shared borders without gaps or overlaps!
- **BubbleSets Isocontour Overlays (Collins et al. 2009)**:
  For overlapping sets $S_1, \dots, S_k$ over fixed node positions (scatterplots, maps, networks):
  1. Connect members of $S_m$ into a virtual routing tree (MST ordered by distance, routing virtual edges via A* around non-member obstacle boxes $B_j, j \notin S_m$).
  2. Accumulate piecewise quadratic potential $E_m(p) = \sum_{e \in S_m} \max\!\bigl(0, 1 - \frac{\operatorname{dist}(p, e)}{R_1}\bigr)^2 - \lambda \sum_{o \notin S_m} \max\!\bigl(0, 1 - \frac{\operatorname{dist}(p, o)}{R_0}\bigr)^2$ on a local grid and extract exact threshold contour $E_m(p) = \tau$ via `Ranvier.D5`'s existing `Contour` (Marching Squares)!
- **UpSet Set-Intersection Algebra (Lex et al. 2014)**:
  Represents $M$ sets over $N$ items as bitvectors ($M \le 64 \Rightarrow$ single `struct(uint32 * uint32)` per item; $M > 64 \Rightarrow$ `uint32[]` words per item). Hash/radix-groups identical membership masks to extract non-empty **exclusive intersections** (`Exact`), **inclusive intersections** (`Subset`), cardinalities, intersection degrees, and **deviation from statistical independence** $\Delta(I) = \frac{|I|}{N} - \prod_{s \in S_I} \frac{|s|}{N} \prod_{s \notin S_I}\bigl(1 - \frac{|s|}{N}\bigr)$, laying out the coordinated combination matrix, degree/cardinality marginal bars, and attribute summary boxplots.
- **Area-Proportional Circular/Elliptical Euler & Venn Layout (Wilkinson 2012 / Larsson 2018)**:
  Finds circle parameters $(x_i, y_i, r_i)$ ($r_i = \sqrt{|S_i|/\pi}$) or 5-DOF ellipses $(x_i, y_i, a_i, b_i, \phi_i)$ minimizing normalized region-area stress $\operatorname{Stress} = \frac{\sum_{I \neq \emptyset} (\hat{A}_I - \beta A_I)^2}{\sum_{I \neq \emptyset} A_I^2}$:
  1. Initialize centers via constrained 2D MDS on pairwise Jaccard/lens target distances $d_{ij}^*$ (solved via 1D Brent root-finding on exact two-circle lens area $L(r_i, r_j, d) = |S_i \cap S_j|$).
  2. Refine via Levenberg-Marquardt / L-BFGS using exact analytical circle-arc arrangement integration (or adaptive quadtree cell decomposition for ellipses).

### 7.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- **Zero-New-Geometry Reuse**: `PRISM` and `GMap` reuse `Ranvier.D5/Delaunay.fs` + `Voronoi.fs` + `Stress2D.fs` directly! `BubbleSets` reuses `Ranvier.D5/Contour.fs` directly, vectorizing grid potential splatting with `Vector256<float>`!
- **UpSet Bitset SIMD**: On `.NET 10`, intersection popcounts over item bitplanes use `Vector256<uint64>` bitwise `&&&` / `~~~` + `BitOperations.PopCount`; on Fable JS, 32-bit SWAR `popcnt32` processes 32 items per integer instruction with zero heap allocations.

### 7.4 Visualization Value & Package Placement
- **Why High-Value**:
  1. `PRISM` solves non-distorting label and node overlap removal for scatterplots, UMAP/t-SNE embeddings, and force layouts.
  2. `GMap` + `BubbleSets` turn flat clustered networks or geographic scatterplots into immediately intuitive "countries & regions" or overlapping set contours without moving nodes.
  3. `UpSet` + `EulerVenn` close a major chart-type gap in `Ranvier.D5.Charts` (set & multi-cohort analytics for bioinformatics, A/B feature cohorts, and security permissions).
- **Recommended Placement**:
  - `Ranvier.D5.Graph`: `PrismOverlap.fs`, `ClusterRegions.fs` (`GMap`, `BubbleSets`, `KelpFusion`).
  - `Ranvier.D5` (`SetAlgebra.fs` — UpSet bitmask engine + Euler/Venn optimizer) + `Ranvier.D5.Charts` (`SetCharts.fs` — `Chart.upset`, `Chart.euler`).

---

## 8. Schematic Octilinear (Metro-Map) Routing & Analytical Network Layouts (Circular, Arc, Hive, BioFabric)

### 8.1 Primary Academic Papers & Flagship Prior Art
1. **Nöllenburg, M., & Wolff, A. (2011).** *"Drawing and Labeling High-Quality Metro Maps by Mixed-Integer Programming."* *IEEE TVCG*, 17(5), 626–641.
2. **Stott, J., Rodgers, P., Martinez Ovando, J. C., & Walker, S. G. (2011).** *"Automatic Metro Map Layout Using Multicriteria Optimization."* *IEEE TVCG*, 17(1), 101–114.
3. **Bast, H., Brosi, P., & Storandt, S. (2020 / 2021).** *"Metro Maps on Octilinear Grid Graphs"* (*Computer Graphics Forum / EuroVis 2020*, 39(3), 357–367) & *"Efficient Generation of Geographically Accurate Transit Maps"* (*ACM TSAS*, `LOOM` suite).
4. **Baur, M., & Brandes, U. (2005).** *"Crossing Reduction in Circular Layouts."* *Graph-Theoretic Concepts in Computer Science (WG 2004)*, LNCS 3353, pp. 332–343; **Gansner, E. R., & Koren, Y. (2006)**, *"Improved Circular Layouts"*, *GD 2006*, LNCS 4372, pp. 386–398.
5. **Wattenberg, M. (2002).** *"Arc Diagrams: Visualizing Structure in Strings."* *IEEE InfoVis*, pp. 110–116.
6. **Krzywinski, M., Birol, I., Jones, S. J. M., & Marra, M. A. (2012).** *"Hive Plots—rational approach to visualizing networks."* *Briefings in Bioinformatics*, 13(5), 627–644.
7. **Longabaugh, W. J. R. (2012).** *"Combing the hairball with BioFabric: a new approach for visualization of large networks."* *BMC Bioinformatics*, 13, Article 275.
- **Flagship Prior Art**: `LOOM` (`ad-freiburg/loom`), Eclipse `ELK`, Graphviz `circo`, `Circos` & `d3-hive` (Krzywinski/Bostock), `BioFabric` / `Cytoscape`.

### 8.2 Mathematical Formulation, Invariants & Complexity
- **Octilinear Schematic Router (`OctilinearRouter`, $k \cdot 45^\circ$)**:
  Restricts every routed edge segment to the 8 compass octants $\Theta_8 = \{0^\circ, 45^\circ, 90^\circ, 135^\circ, 180^\circ, 225^\circ, 270^\circ, 315^\circ\}$:
  1. **Octilinear Visibility / Port-Extended Grid Graph**: Through every port and inflated obstacle corner $(x \pm m, y \pm m)$, emit horizontal ($y = c$), vertical ($x = c$), and $\pm 45^\circ$ diagonal ($x - y = c$, $x + y = c$) guide lines clipped against inflated node AABBs (`RTree2D` accelerated).
  2. **Bend-Penalized State-Space A\***: States $(v, d_{\text{in}} \in \Theta_8 \cup \{\bot\})$ minimize
     $$J(P) = \sum_{e \in P} \operatorname{len}(e) + \sum_{\text{bends}} \psi(\Delta \theta) + c_{\text{cong}} \cdot \text{sharedPenalty}(e)$$
     where $\psi(0^\circ) = 0$, $\psi(45^\circ) = \beta_{45}$ (gentle obtuse metro bend, $135^\circ$ interior angle), $\psi(90^\circ) = \beta_{90} \gg \beta_{45}$, and $\psi(\ge 135^\circ) = +\infty$ (forbids sharp acute hairpins!), guided by the **exact admissible octilinear heuristic**:
     $$h_{\text{oct}}(\Delta x, \Delta y) = (\sqrt{2} - 1)\min(|\Delta x|, |\Delta y|) + \max(|\Delta x|, |\Delta y|)$$
  3. **Topological Metro-Trunk Line Ordering (Bast, Brosi & Storandt 2020)**: When multiple colored transit/flow lines share edges along a trunk corridor, order parallel offset tracks across shared segments via dynamic programming / sifting to eliminate avoidable cross-overs at junctions!
- **Circular & Arc Crossing Reduction (Baur & Brandes 2005)**:
  Minimizes chord crossings $\sum_{(u,v),(w,z) \in E} [\pi(u) < \pi(w) < \pi(v) < \pi(z)]$ in two deterministic phases:
  1. **Connectivity Greedy Insertion**: Append next vertex maximizing placed neighbors (tie-break minimum unplaced neighbors) to the front or back of a deque minimizing immediate crossings.
  2. **Circular Sifting ($O(V E)$ per sweep)**: For each vertex $u$, sweep $u$ across all $V$ circular positions maintaining exact delta-crossings incrementally when $u$ swaps past neighbor $v$:
     $$\Delta \operatorname{cr}(u, v) = \sum_{x \in N(u) \setminus \{v\}} \bigl|\{y \in N(v) \setminus \{u, x\} : y \text{ strictly between } v \text{ and } x \text{ clockwise}\}\bigr| - (\text{symmetric})$$
     Updating prefix sums takes $O(\deg(u) \cdot \deg(v))$ per adjacent swap $\Rightarrow$ **$O\!\left(\sum_u \sum_v \deg(u)\deg(v)\right) = O(E^2)$** worst-case (or $O(V E)$ on bounded-degree graphs), reducing crossings by $40\%$–$80\%$ over naive/degree circular and arc layouts!
- **Hive Plots (Krzywinski 2012) & BioFabric (Longabaugh 2012)**:
  - **Hive Plots**: Deterministically partitions nodes onto $K \in [2, 6]$ radial axes (with optional duplicated split-axes to show intra-group edges cleanly!) by structural rule (e.g., source / intermediary / sink, $k$-core shell, or Leiden community) and radial coordinate $r_v$ by quantitative metric (degree, PageRank, clustering coefficient), drawing quadratic/cubic Bézier curves between axes. **Invariant**: Two structurally identical networks produce identical perceptual signatures ("network barcodes").
  - **BioFabric**: Maps nodes $0 \dots V-1$ (ordered by BFS degree / RCM) to horizontal y-lines $y_v$ and **every edge $e = (u, v)$ to its own unique vertical column $x_e$**, sorted lexicographically by $(\min(y_u, y_v), \max(y_u, y_v))$ (grouping node wedges into clean triangular "shadow" sails!). Zero edge overlap at arbitrary density!

### 8.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- `OctilinearRouter` uses a flat binary 4-ary heap over integer state IDs `v * 8 + dir`, storing `gScore: float[]` and `parent: int[]` in reusable pool arrays (matching `OrthogonalRouter.fs` style, zero closure/tuple allocations).
- Circular sifting uses CSR adjacency slices + flat `pos: int[]` array, executing inner neighbor-pair sweeps in L1 cache.

### 8.4 Visualization Value & Package Placement
- **Why High-Value**:
  1. `OrthogonalRouter` only does $90^\circ$ boxy UML/circuit wires; `OctilinearRouter` unlocks clean $45^\circ$ subway/transit maps, CI/CD Git-branch graphs, data-lineage pipelines, and modern architecture diagrams!
  2. Crossing-minimized `Circular` & `Arc` layouts, `HivePlot`, and `BioFabric` give `Ranvier.D5.Graph` and `Ranvier.D5.Charts` deterministic, hairball-free analytical network views.
- **Recommended Placement**: **`Ranvier.D5.Graph`** (`OctilinearRouter.fs`, `AnalyticalLayouts.fs`: `CircularLayout`, `ArcLayout`, `HivePlot`, `BioFabric`) + thin recipe wrappers in `Ranvier.D5.Charts/Network.fs`.

---

## 9. 3D Spherical / Geodesic Distributions, Spherical Voronoi / CVT, Discrete Global Grids, & Riemannian Spherical Graph Layouts

### 9.1 Primary Academic Papers & Flagship Prior Art
1. **Saff, E. B., & Kuijlaars, A. B. J. (1997).** *"Distributing Many Points on a Sphere."* *The Mathematical Intelligencer*, 19(1), 5–11.
2. **Keinert, B., Innmann, M., Sänger, M., & Stamminger, M. (2015).** *"Spherical Fibonacci Mapping."* *ACM Transactions on Graphics (TOG)*, 34(6), Article 193 (SIGGRAPH Asia). DOI: `10.1145/2816795.2818131`.
3. **Renka, R. J. (1997).** *"Algorithm 772: STRIPACK: Delaunay Triangulation and Voronoi Diagram on the Surface of a Sphere."* *ACM Transactions on Mathematical Software (TOMS)*, 23(3), 416–434.
4. **Du, Q., Gunzburger, M. D., & Ju, L. (2003).** *"Constrained Centroidal Voronoi Tessellations for Surfaces."* *SIAM Journal on Scientific Computing*, 24(5), 1488–1506.
5. **Górski, K. M., Hivon, E., Banday, A. J., Wandelt, B. D., Hansen, F. K., Reinecke, M., & Bartelmann, M. (2005).** *"HEALPix: A Framework for High-Resolution Discretization and Fast Analysis of Data Distributed on the Sphere."* *The Astrophysical Journal*, 622(2), 759–771; and **Sahr, K., White, D., & Kimerling, A. J. (2003)**, *"Geodesic Discrete Global Grid Systems"*, *CaGIS*, 30(2), 121–134.
6. **Kobourov, S. G., & Wampler, K. (2005).** *"Non-Euclidean Spring Embedders."* *IEEE TVCG*, 11(6), 757–767; **Perry, S., Yin, M., Gray, R., & Kobourov, S. (2020)**, *"Drawing Graphs on the Sphere" / Spherical Geodesic MDS*.
- **Flagship Prior Art**: `SciPy.spatial.SphericalVoronoi`, `d3-geo-voronoi` (Philippe Rivière / Fil), Uber `H3` (`h3geo.org`), `HEALPix`, CGAL `Triangulation_on_sphere_2`.

### 9.2 Mathematical Formulation, Invariants & Complexity
- **Spherical Fibonacci Lattice (`SF`) & $O(1)$ Inverse Lookup (Keinert et al. 2015)**:
  - **Forward ($O(1)$)**: For $i \in [0, N-1]$ and golden ratio $\Phi = \frac{1+\sqrt{5}}{2}$:
    $$z_i = 1 - \frac{2i + 1}{N}, \quad r_i = \sqrt{\max(0, 1 - z_i^2)}, \quad \phi_i = 2\pi \operatorname{frac}(i (\Phi - 1)), \quad \mathbf{p}_i = (r_i \cos\phi_i, r_i \sin\phi_i, z_i)$$
  - **Inverse `inverseSF(q, N)` ($O(1)$, zero tables, zero tree)**:
    Given unit vector $\mathbf{q} = (x, y, z) \in S^2$, compute azimuth $\phi = \min(\operatorname{atan2}(y, x) \bmod 2\pi, \pi\text{-safe})$ and local Fibonacci zone number:
    $$k = \max\!\left(2,\; \left\lfloor \frac{\ln\bigl(N \pi \sqrt{5} (1 - z^2)\bigr)}{\ln(\Phi^2)} \right\rfloor\right), \qquad F_k = \operatorname{round}\!\left(\frac{\Phi^k}{\sqrt{5}}\right), \quad F_{k+1} = \operatorname{round}\!\left(\frac{\Phi^{k+1}}{\sqrt{5}}\right)$$
    Construct the $2 \times 2$ local lattice basis matrix $B = \begin{pmatrix} 2\pi(\operatorname{frac}(F_k(\Phi-1)) - \dots) & \dots \\ -2 F_k / N & -2 F_{k+1} / N \end{pmatrix}$, compute continuous grid coordinates $\mathbf{c} = \lfloor B^{-1} (\phi, z - (1 - 1/N))^T \rfloor$, and evaluate Euclidean dot product $\langle \mathbf{q}, \mathbf{p}_j \rangle$ at the **4 corners** $j = F_k (c_0 + \delta_0) + F_{k+1} (c_1 + \delta_1)$ ($\delta_0, \delta_1 \in \{0, 1\}$, clamped to $[0, N-1]$). Returns exact/near-exact spherical Voronoi bin in **~35 floating-point ops**!
- **Spherical Delaunay, Spherical Voronoi & Spherical CVT**:
  - **3D Convex Hull Equivalence**: For $N \ge 4$ non-coplanar sites on $S^2$, the 3D convex hull $\operatorname{CH}(P)$ in $\mathbb{R}^3$ ($O(N \log N)$ incremental/divide-and-conquer with exact `orient3d` predicate!) **is** the exact Spherical Delaunay triangulation! For each triangular hull face $(a, b, c)$ oriented outward, its normalized plane normal $\mathbf{v}_f = \frac{(b - a) \times (c - a)}{\|(b - a) \times (c - a)\|}$ (or exact circumcenter ray) is the **exact Spherical Voronoi vertex** on $S^2$, joined by great-circle arcs!
  - **Spherical CVT**: Iteratively replaces each site $\mathbf{s}_i \leftarrow \frac{\mathbf{c}(\Omega_i)}{\|\mathbf{c}(\Omega_i)\|}$ where $\mathbf{c}(\Omega_i)$ is the spherical surface-area-weighted centroid decomposed into spherical triangles from $\mathbf{s}_i$.
- **Spherical Geodesic Stress Layout (`SphericalStress3D`)**:
  Places graph vertices on $S^2$ ($\|\mathbf{x}_i\| = 1$, scaled by radius $R$) minimizing geodesic stress:
  $$\mathcal{E}_{S^2}(\mathbf{X}) = \sum_{i < j} w_{ij} \Bigl(\arccos\bigl(\operatorname{clamp}_{-1}^1\langle \mathbf{x}_i, \mathbf{x}_j \rangle\bigr) - \theta_{ij}^*\Bigr)^2, \qquad \theta_{ij}^* = \min\!\left(\pi - \varepsilon,\; \alpha \cdot d_G(i, j)\right)$$
  Using **Riemannian Stochastic Gradient Descent** (generalizing `Stress2D`/`Stress3D` Zheng-Pawar-Goodman 2019 to $S^2$!): for pair $(i, j)$ at angular distance $\theta = \arccos\langle \mathbf{x}_i, \mathbf{x}_j \rangle$, rotate $\mathbf{x}_i$ and $\mathbf{x}_j$ symmetrically along their connecting great-circle geodesic via exact tangent exponential map $\exp_{\mathbf{x}_i}(\delta \hat{\mathbf{t}}_{ij}) = \mathbf{x}_i \cos\delta + \hat{\mathbf{t}}_{ij} \sin\delta$!

### 9.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- `SphericalFibonacci` batch binning (`binPoints: Point3[] -> int -> int[]`) has zero branches except pole guard $1 - z^2 \le \varepsilon$, vectorizing over `Vector256<double>` / `SimdNumeric`!
- `SphericalStress3D` reuses `Ranvier.D5.Graph`'s k-center BFS landmark pivots (`GraphKernels`) + Fisher-Yates shuffled pair schedule on flat `xs[], ys[], zs[]` arrays with zero heap allocations per epoch.

### 9.4 Visualization Value & Package Placement
- **Why High-Value**:
  1. `SphericalFibonacci` gives `Ranvier.D5.Geo` and `Ranvier.D5.Wgpu` an **equal-area pole-distortion-free 3D globe hexbinning equivalent** (where lat-lon grids pinch 100x at the poles!) in $O(1)$ per datum!
  2. `SphericalVoronoi` + `SphericalCvt` power global weather/sensor/tectonic tessellations, procedural globe cells, and geodesic dome meshes.
  3. `SphericalStress3D` completes **all three constant-curvature 3D geometries** in `Ranvier.D5.Graph3D`: Euclidean $\mathbb{R}^3$ (`Force3D`/`Stress3D`), Negative-Curvature Hyperbolic $\mathbb{H}^3$ (`Hyperbolic3D`/`H3`/`Walrus`), and Positive-Curvature Spherical $\mathbb{S}^2$ (`SphericalStress3D` — eliminating periphery boundary distortion on cyclic/global/planetary networks!).
- **Recommended Placement**:
  - `Ranvier.D5.Geo`: `SphericalFibonacci.fs`, `SphericalVoronoi.fs` (exposing `GeoPolygon` spherical cells + `Point3` duals).
  - `Ranvier.D5.Graph3D`: `SphericalLayout3D.fs` (`SphericalStress3D`, `SphericalFibonacciNodes`).

---

## 10. High-Dimensional Neighborhood & Manifold Embeddings in 2D & 3D (NN-Descent, UMAP, Barnes-Hut t-SNE, Isomap, Sammon)

### 10.1 Primary Academic Papers & Flagship Prior Art
1. **Sammon, J. W. (1969).** *"A Nonlinear Mapping for Data Structure Analysis."* *IEEE Transactions on Computers*, C-18(5), 401–409.
2. **Tenenbaum, J. B., de Silva, V., & Langford, J. C. (2000).** *"A Global Geometric Framework for Nonlinear Dimensionality Reduction."* *Science*, 290(5500), 2319–2323. *(Isomap).*
3. **van der Maaten, L., & Hinton, G. (2008).** *"Visualizing Data using t-SNE."* *Journal of Machine Learning Research (JMLR)*, 9, 2579–2605; **van der Maaten, L. (2014)**, *"Accelerating t-SNE using Tree-Based Algorithms"*, *JMLR*, 15, 3221–3245.
4. **Dong, W., Moses, C., & Li, K. (2011).** *"Efficient K-Nearest Neighbor Graph Construction for Generic Similarity Measures."* *Proceedings of the 20th International Conference on World Wide Web (WWW)*, pp. 577–586. *(NN-Descent).*
5. **McInnes, L., Healy, J., & Melville, J. (2018).** *"UMAP: Uniform Manifold Approximation and Projection for Dimension Reduction."* *Journal of Open Source Software (JOSS)*, 3(29), 861 / `arXiv:1802.03426`.
- **Flagship Prior Art**: `umap-learn` & `pynndescent` (Leland McInnes), `umap-js` (Google PAIR), `openTSNE` (Poličar et al.), `scikit-learn` (`TSNE`, `Isomap`), Apple `Embedding Atlas` / `Deck.gl` point-cloud viewers.

### 10.2 Mathematical Formulation, Invariants & Complexity
- **1. Exact (`BallTree`) & Approximate (`NN-Descent`) $k$-NN Graph**:
  - For $N \le 4{,}096$ or $D \le 16$: exact `BallTree` $k$-NN in $O(N \log N)$.
  - For large $N$ or high $D$ (e.g. $D = 384$–$1536$ LLM/bio embeddings!): **Dong et al. (2011) `NN-Descent`** initializes each point with $K$ deterministic random neighbors in a flat bounded max-heap `idx[N * K]`, `dist[N * K]`, `isNew[N * K]`, and iteratively tests *neighbors-of-neighbors* (local join over forward + reverse `new` lists against `new` $\cup$ `old`), terminating when updates $< \delta K N$ ($\delta = 0.001$, typically $6$–$12$ iterations, empirical **$O(N^{1.14} D)$** with $>99\%$ recall!).
- **2. UMAP in 2D & 3D (McInnes, Healy & Melville 2018)**:
  1. **Local Metric Calibration**: For each $i$, set $\rho_i = \min \{ d(x_i, x_{i_j}) > 0 \}$ (ensuring local connectivity) and binary-search ($64$ iterations) for smooth scale $\sigma_i > 0$ satisfying:
     $$\sum_{j=1}^{k} \exp\!\left(-\frac{\max(0, d(x_i, x_{i_j}) - \rho_i)}{\sigma_i}\right) = \log_2(k)$$
  2. **Fuzzy Simplicial Set Union**: Symmetrize directed membership strengths $p_{i|j}$ via probabilistic t-conorm:
     $$w_{ij} = p_{i|j} + p_{j|i} - p_{i|j} p_{j|i}$$
  3. **Curve Fit $(a, b)$ from $(\text{minDist}, \text{spread})$**: Fit $(1 + a d^{2b})^{-1}$ to piecewise exponential decay via Gauss-Newton ($20$ steps on $300$ grid samples).
  4. **Spectral Initialization + Negative-Sampling SGD ($d \in \{2, 3\}$)**: Initialize $Y \in \mathbb{R}^{N \times d}$ from normalized Laplacian eigenvectors (with deterministic PCA fallback if disconnected). Schedule each directed edge $e$ every $1 / w_e$ epochs (`epochOfNextSample[e]`), applying attractive gradient $\frac{-2 a b \|y_i - y_j\|^{2(b-1)}}{1 + a \|y_i - y_j\|^{2b}} (y_i - y_j)$ and $m = 5$ uniform negative repulsive samples $\frac{2 b}{(\varepsilon + \|y_i - y_k\|^2)(1 + a \|y_i - y_k\|^{2b})}(y_i - y_k)$!
  - **Complexity**: **$O(N^{1.14} D + k N \cdot \text{Epochs})$** time, **$O(k N + D N)$** memory.
- **3. Barnes-Hut t-SNE in 2D & 3D (van der Maaten 2014)**:
  1. Binary-search $\beta_i = (2\sigma_i^2)^{-1}$ on $K = \lfloor 3 \cdot \text{Perplexity} \rfloor$ nearest neighbors so Shannon entropy $2^{H(P_i)} = \text{Perplexity}$; symmetrize $P_{ij} = \frac{p_{j|i} + p_{i|j}}{2N}$.
  2. Minimize KL divergence $KL(P \| Q)$ with Student-$t$ kernel $q_{ij} = \frac{(1 + \|y_i - y_j\|^2)^{-1}}{Z}$ using early exaggeration ($\times 4$ on $P_{ij}$ for first $25\%$ iterations) + momentum + adaptive Jacobs delta-bar-delta gains:
     - **Attractive term**: Exact sparse CSR sweep over $3\cdot\text{Perp}\cdot N$ edges in $O(k N)$.
     - **Repulsive term & $Z$**: Approximated via Barnes-Hut ($\theta = 0.5$) directly on `BhQuadtree2D` (for 2D) and `BhOctreeWorkspace` (for 3D) in **$O(N \log N)$**!
- **4. Isomap (Tenenbaum et al. 2000) & Sammon Mapping (1969)**:
  - **Isomap**: Build $k$-NN CSR graph weighted by Euclidean/metric distances $\to$ Landmark/Dijkstra geodesic shortest paths $\to$ double-centered `PivotMds` ($O(p \cdot k N \log N)$ for $p$ landmarks, or $O(N^2 \log N)$ exact!).
  - **Sammon**: Weighted Stress minimization $\frac{1}{\sum_{i<j} d_{ij}^*} \sum_{i<j} \frac{(\|y_i - y_j\| - d_{ij}^*)^2}{d_{ij}^*}$ emphasizing preservation of **small local distances** ($w_{ij} = 1 / d_{ij}^*$ vs. Stress's $w_{ij} = (d_{ij}^*)^{-2}$), solved immediately by plugging Sammon weights into `Stress2D` / `Stress3D` SGD!

### 10.3 .NET 10 SIMD + Flat SoA + Fable JS Mapping
- **Row-Major Contiguous Feature Matrix**: Accept `features: float[]` (or `float32[]`) of shape `N * D` so Euclidean (`SimdMath.sumSquaresDiff`), Cosine (`SimdMath.dot`), and Manhattan distance kernels run at full **AVX-512 / AVX2 / ARM64 AdvSIMD** memory bandwidth (`16` `float32`s or `8` `float`s per instruction on `.NET 10`)!
- **Zero-Allocation Arena Reuse**: Barnes-Hut t-SNE reuses `BhQuadtree2D` (`Ranvier.D5.Graph`) and `BhOctreeWorkspace` (`Ranvier.D5.Graph3D`) **directly without allocating a single node object per frame**! UMAP SGD runs on pre-allocated `head: int[]`, `tail: int[]`, `epochsPerSample: float[]`, `epochOfNextSample: float[]`, and `Xoshiro256**` deterministic PRNG (`Ranvier.D5/Random.fs`), guaranteeing **bit-exact cross-run determinism**!

### 10.4 Visualization Value & Package Placement
- **Why High-Value**: Modern interactive data visualization increasingly centers on exploring high-dimensional embeddings (vector databases, LLM/RAG chunks, telemetry traces, single-cell omics, financial factor spaces) in **2D scatterplots (`Ranvier.D5.Charts`) and 3D GPU point clouds (`Ranvier.D5.Wgpu` `Scatter3D`)**. Having native, deterministic, zero-Python-dependency F#/.NET 10 SIMD + Fable JS `UMAP`, `t-SNE`, `Isomap`, `PCA`, and `Sammon` (paired with `BallTree`/`NN-Descent`) is a massive differentiator.
- **Recommended Placement**:
  - Either a focused new package **`Ranvier.D5.Manifold`** (referencing `Ranvier.D5`, `Ranvier.D5.Graph`, `Ranvier.D5.Graph3D` with `InternalsVisibleTo` for Barnes-Hut & `SimdMath` reuse) **OR** place `Pca` + `BallTree` + `NnDescent` in `Ranvier.D5`, `Isomap` + `Sammon` + `Umap2D` + `Tsne2D` in `Ranvier.D5.Graph`, and `Umap3D` + `Tsne3D` in `Ranvier.D5.Graph3D`. A dedicated `Ranvier.D5.Manifold` (or `Ranvier.D5` + `Graph`/`Graph3D` split if keeping package count fixed) avoids bloating core chart/diagram consumers while sharing 100% of kernels.

---

## 11. Master Prioritization & Package Placement Matrix

| Rank | Algorithm Family | Dim | Asymptotic Time | Reuse of Existing `Ranvier.D5*` Kernels | Recommended Package & Module | Value / Effort |
| :---: | :--- | :---: | :--- | :--- | :--- | :---: |
| **1** | **Packed Hilbert R-Tree (`Flatbush`) + 3D `LBVH` (Karras 2012) + `SpaceFilling` (Morton/Hilbert/Moore 2D & 3D)** | **2D & 3D** | $O(N)$ build, $O(\log N)$ query | `Bounds`, `Bounds3`, `Point`, `Point3`, `SimdNumeric` (`Vector512`/`256`) | **`Ranvier.D5`** (`SpaceFilling.fs`, `RTree2D.fs`, `Bvh3D.fs`, `Spatial3D.fs`) | **Highest** (Unlocks fast 2D/3D AABB & ray picking everywhere) |
| **2** | **Matrix Seriation (`RCM`, `SpectralFiedler`, `OptimalLeafOrder`, `Bicluster`) + `Leiden` Community Detection** | **Data Org / 2D** | $O(V+E)$ RCM/Leiden; $O(n^3)$ / $O(n^2)$ OLO | `CsrGraph`, `SimdMath`, upgrades `AdjacencyMatrixSpec` & `Chord` | **`Ranvier.D5`** (`Seriation.fs`) + **`Ranvier.D5.Graph`** (`Community.fs`) | **Highest** (Transforms noisy matrices & discovers graph hierarchy) |
| **3** | **Power Diagrams, Nocaj-Brandes `VoronoiTreemap`, & 2D/3D `CVT` (`Voro++` radical clipper + L-BFGS)** | **2D & 3D** | $O(I \cdot N \log d_{\max})$ treemap; $O(I \cdot N)$ 3D CVT | `ExactDyadic` (`Predicates.fs`), `Voronoi.fs` polygon clip arena, `SimdMath` | **`Ranvier.D5`** (`PowerDiagram.fs`, `VoronoiTreemap.fs`, `Cvt2D.fs`, `Voronoi3D.fs`) | **Very High** (Flagship organic 2D/3D treemaps & blue-noise relaxation) |
| **4** | **3D Hierarchical Space-Filling Layouts (`CubeTreemap3D` Squarified/Hilbert, `StepTree`, 2.5D `CodeCity`, `Pack3D`)** | **2.5D & 3D** | $O(N \log d_{\max})$ | `HierarchyIndex<'Id>`, `Bounds3`, `Treemap` row squarifier, `Marks3D` cuboids/spheres | **`Ranvier.D5.Graph3D`** (`Treemap3D.fs`, `Pack3D.fs`) | **Very High** (Fills complete gap in 3D space-filling hierarchies) |
| **5** | **Edge Bundling in 2D & 3D (`Holten HEB`, `Selassie Divided FDEB`, `MINGLE`, `KDEEB` 2D & 3D)** | **2D, Geo & 3D** | $O(E \cdot H)$ HEB; $O(E \log E)$ MINGLE; $O(I(ES + G^d))$ KDEEB | `HierarchyIndex`, `Curves.Bundle`, `DensityGrid`, `VolumeGrid`, `Point3Chain` | **`Ranvier.D5.Graph`** (`EdgeBundling.fs`) + **`Ranvier.D5.Graph3D`** (`EdgeBundling3D.fs`) | **Very High** (Eliminates 2D/Geo/3D hairballs; linear-time KDEEB) |
| **6** | **3D Spherical Fibonacci (`inverseSF` $O(1)$), `SphericalVoronoi`/`CVT`, & `SphericalStress3D` Geodesic Layout** | **3D / $S^2$** | $O(1)$ SF bin; $O(N \log N)$ Voronoi; $O(I \cdot k V)$ Stress | `Point3`, `Quaternion`, `ExactDyadic` `orient3d`, `Stress3D` SGD | **`Ranvier.D5.Geo`** (`SphericalFibonacci.fs`, `SphericalVoronoi.fs`) + **`Ranvier.D5.Graph3D`** (`SphericalLayout3D.fs`) | **High** (Pole-free $O(1)$ globe binning + $S^2$ graph geometry) |
| **7** | **Schematic `OctilinearRouter` ($45^\circ$ Metro-Map) + Crossing-Minimized `Circular`, `Arc`, `HivePlot`, & `BioFabric`** | **2D** | $O(V \log V)$ A*; $O(V E)$ sifting; $O(V+E \log E)$ BioFabric | `DiagramGraph`, `RTree2D`, mirrors `OrthogonalRouter.fs` port contracts | **`Ranvier.D5.Graph`** (`OctilinearRouter.fs`, `AnalyticalLayouts.fs`) | **High** (Subway/Git/pipeline 45° routing + rational network views) |
| **8** | **`PRISM` Overlap Removal + `GMap` / `BubbleSets` Cluster Hulls + `UpSet` / `EulerVenn` Set Layouts** | **2D** | $O(T \cdot V \log V)$ PRISM; $O(M \log M)$ GMap; $O(N \cdot \lceil M/64 \rceil)$ UpSet | `Delaunay`, `Voronoi`, `Contour` (Marching Squares), `Stress2D` | **`Ranvier.D5.Graph`** (`PrismOverlap.fs`, `ClusterRegions.fs`) + **`Ranvier.D5`**/`Charts` (`SetLayout.fs`, `SetCharts.fs`) | **High** (Isotropic label/node de-overlap, map hulls & set analytics) |
| **9** | **Spatially Stable 2D Treemaps (`Hilbert`, `Moore`, `Strip`, `GosperMap`, Sondag `LocalMoves`)** | **2D** | $O(N)$ Hilbert/Moore/Strip; $O(N \cdot K)$ LocalMoves | `HierarchyIndex<'Id>`, `SpaceFilling.fs`, `Treemap.fs` | **`Ranvier.D5`** (`Treemap.fs` extensions, `GosperMap.fs`) | **Medium-High** (Solves dynamic treemap jitter & cartographic trees) |
| **10** | **High-D Manifold Embeddings (`BallTree`, `NN-Descent`, `UMAP` 2D/3D, Barnes-Hut `t-SNE` 2D/3D, `Isomap`, `Sammon`)** | **High-D $\to$ 2D & 3D** | $O(N^{1.14} D + k N E)$ UMAP; $O(I \cdot N \log N)$ BH t-SNE | `BhQuadtree2D`, `BhOctreeWorkspace`, `PivotMds`, `Stress2D/3D`, `SimdMath`, `Random` | **`Ranvier.D5.Manifold`** *(or `Ranvier.D5` `BallTree`/`NnDescent` + `Graph`/`Graph3D`)* | **High** (Native SIMD 2D/3D embedding engine for AI/data exploration) |

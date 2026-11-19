# `Ranvier.D5.Wgpu` Design Specification

**Date:** 2026-11-18  
**Prior-Art Survey:** [`docs/3d-prior-art.md`](../../3d-prior-art.md)  
**Repositories Affected:** `C:/wk/ranvier-release/Ranvier.Wgpu` and `C:/wk/ranvier-release/Ranvier.D5`

Following repository figure-attribution rules (`C:/wk/ranvier-release/AGENTS.md`, "Label every figure"), every numeric figure in this specification carries one of three explicit labels: **measured here**, **from documentation** (naming the file or URL), or **general knowledge** (not checked).

---

## 1. Intent and Why

`Ranvier.D5` provides immutable scalar scales, statistics, palettes, 2D geometry, layouts, and retained SVG rendering through `Ranvier.D5.View`. SVG excels at crisp 2D charts and typography, yet lacks true 3D depth testing, hardware instancing, shaded/deforming surface meshes, and live half-space section cuts. Conversely, `Ranvier.Wgpu` (with `Ranvier.Wgpu.Sdl` on native desktop and `Fable.Ranvier.Wgpu` in the browser) provides a zero-allocation `ScenePass` 3D engine with `4×` MSAA (**from documentation**: `Ranvier.Wgpu/README.md`), `InstanceBatch`, `LineBatch`, `DiscBatch`, `SectionPlane`, and `1×1` `RGBA32Uint` GPU picking (**from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu/WgpuContext.fs`), while leaving coordinate bounds, 3D axis framing, multi-dimensional channel encoding, surface/isosurface tessellation, and incremental `Ranvier` reactive state synchronization to every consumer.

`Ranvier.D5.Wgpu` closes that gap: it turns `Ranvier.Wgpu` into a high-performance 3D and multi-dimensional visualization backend for `Ranvier.D5`. A caller can express a bounded 3-axis plot—such as a live wave surface with independent 4th-dimension color, camera-aware back-wall grids, outer-silhouette ticks, crisp projected labels, and pointer probing—or stream dynamic `Ranvier` `KeyedCollection` and `Projection` datasets into GPU batches with `O(changed)` partial buffer updates and zero steady-state GC churn.

---

## 2. Goals, Non-Goals, and Anti-Goals

### Goals
1. **Upstream `Ranvier.Wgpu` (+ `Fable.Ranvier.Wgpu`) primitives for dynamic data:**
   - In-place `MeshHandle.UpdateVertices(ReadOnlySpan<Vertex3D>)` and `MeshHandle.UpdateVertexRange(startVertex, ReadOnlySpan<Vertex3D>)` so deforming surfaces update GPU vertex buffers without reallocating `MeshHandle` or index buffers.
   - Sub-range writes `UpdateRange(startIndex, ReadOnlySpan<_>)` on `InstanceBatch`, `LineBatch`, and `DiscBatch` across both `.NET` (`wgpuQueueWriteBuffer` with byte offset) and browser (`WebGPU` `queue.writeBuffer` / `WebGL2` `gl.bufferSubData`).
   - Orthographic and continuous dolly-zoom `perspectiveness` in `[0.0, 1.0]` (**from documentation**: `docs.makie.org/stable/reference/blocks/axis3`) on `Camera3D` without breaking existing `Camera3D` struct record fields, preserving exact compatibility with `Camera3D.projectToScreen`, `Camera3D.screenRay`, `InstanceBatch`, `LineBatch`, `DiscBatch`, and `Pick` / `PickAsync`.
2. **Shared 3D spatial value types in core `Ranvier.D5`:**
   - Minimal `[<Struct>] Point3` and `[<Struct>] Bounds3` in `src/Ranvier.D5/Geometry3D.fs` (aligned with sibling `Ranvier.D5.Graph3D` so both packages interoperate with zero cross-dependency).
3. **Bounded 3-axis coordinate system and Makie-style view-aware `Axis3D`:**
   - `Scale3D` + `BoxAspect` decoupling data domains (`Ranvier.D5.Scale` continuous, log, symlog, piecewise, UTC `TimeScale`, and categorical `Band<'Key>`) from world-space cuboid proportions (`Cube`, `ChartDefault`, `Data` with `Auto` extreme-ratio guard, and `Manual`).
   - Camera-aware `Axis3D` classifying the `6` cuboid faces (**general knowledge**: not checked) per camera pose to draw only back-facing panels and gridlines, selecting outer-silhouette edges for tick hairlines, and emitting projected 2D screen-space label descriptors (`AxisLabel2D`) with stable semantic keys and fixed layout `Protrusions`.
4. **Multi-dimensional encodings (`4`D–`7`D+) over 3D marks:**
   - Allocation-free `ColorRamp` mapping scalar domains directly into `RgbaColor` structs (with `LowClip`, `HighClip`, and `Missing` policies—no CSS string formatting), plus an optional `Shaders.composeScene` + `UserUniformSlots` (`16` `vec4<f32>` = `256` bytes; **from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu/README.md`) GPU scalar-LUT shader builder for zero-upload `colorrange` scrubbing.
   - Anisotropic `Vector3` glyph scale, `Quaternion` / direction-vector `(u, v, w)` orientation, `DiscInstance3D` concentric ring-progress channels, `Unlit` vs. studio-lit shading control, wall shadow/contour projections (`XY`, `XZ`, `YZ`), and live WGSL `SectionPlane` slicing.
5. **Complete texture-free 3D mark & topology family:**
   - `Surface3D` (regular, rectilinear, curvilinear `StructuredGrid`, and `Delaunay`/`Mesh3D` surfaces with independent `4`th-dimension `color` grids, `None`/`NaN` hole omission, analytical/finite-difference normals, cached index topology reuse, and optional wireframes/wall contours).
   - `Scatter3D` (`DiscBatch` anti-aliased SDF billboards with back-to-front translucent sorting when needed).
   - `Glyph3D` / `MeshScatter3D`, `Bar3D` (baseline-anchored 3D columns), and `Arrows3D` / `Cones3D` (`InstanceBatch` over unit `Primitives`).
   - `Segments3D`, `Line3D`, `Tube3D` (`MeshBuilder.AddTube`), and `Ribbon3D` (`Band3D` ruled strips).
   - `IsoSurface3D` (bounded, cancellable 3D marching cubes with boundary caps and secondary scalar interpolation) and `VolumeSlices3D` (`3`-plane orthogonal volume cross-sections).
6. **First-class `Ranvier` reactivity and mutation (`KeyedCollection`, `Projection`, `Lookup`, `Editable`):**
   - Keyed GPU batch sinks driven by `Projection<'K, 'T>.NewValueReader()` (`ProjectionDelta<'K>`) applying `O(changed)` slot updates via `UpdateRange`, maintaining exact `Key <-> PickId` (`uint32`) maps, and retiring exited keys immediately.
   - Split per-channel memos (`position` vs. `scale`/`rotation` vs. `color`/`flags`) so palette, range, or `createSelector` (`Lookup<'K, bool>`) selection updates never recompute unchanged geometry.
   - Incremental reactive domain extents via `Projection.tryMinBy` / `Projection.tryMaxBy` feeding `Scale.nice` memos, plus `Editable<'T>` / `createDraft` camera, section-plane, and domain controls.
7. **Dual-target core + native SDL host integration:**
   - `Ranvier.D5.Wgpu` compiles cleanly on `.NET 10` (`IsAotCompatible=true`) and packs `fable/` sources for `Fickl` (`Fable.Ranvier.Wgpu`).
   - `Ranvier.D5.Wgpu.Sdl` mounts a zero-copy `WgpuSdlSurface` + stable-keyed `Ranvier.D5.View` `Renderer.passive` label overlay + `Ranvier.View.Dsl` `tag.on*` pointer/wheel/keyboard turntable/arcball + throttled `1×1` GPU picking + idle-resting frame loop.

### Non-Goals
- **GPU 3D-texture volume ray-marching (`:mip` / `:absorption`) or GPU font-atlas textures:** `Ranvier.Wgpu` intentionally exposes `0` texture/sampler/storage bindings (**from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu/README.md`). Volumetric fields are represented via `IsoSurface3D` and `VolumeSlices3D`; typography uses crisp projected DOM/SVG overlays.
- **Graph, compound-port, and hyperbolic (`H3` / `Walrus` / `Force3D`) layout solvers:** Owned by sibling `Ranvier.D5.Graph` and `Ranvier.D5.Graph3D`. `Ranvier.D5.Wgpu` consumes their `Point3` and `Bounds3` outputs directly without duplicating layout math.
- **Spherical geographic projections:** Owned by `Ranvier.D5.Geo`.

### Anti-Goals
- Allocating new GPU `MeshHandle`s, index arrays, CSS color strings, or per-datum heap closures on every frame of a live wave/simulation update or single-key `KeyedCollection` mutation.
- Attaching `wheel` listeners to overlay marks or keying projected axis labels by visible-list position (which triggers multi-millisecond WebCore full-document style/layout rebuilds during drag, **from documentation**: `AstroShowcase.Web/measure/README.md`).
- Defaulting quantitative 3-axis charts to high-foreshortening perspective with front-grid occlusion, or letting directional studio normals silently corrupt quantitative perceptual luminance palettes without an explicit `ShadingMode`.
- Leaving stale `PickId` entries for removed keys, or disposing `WgpuContext` / `Window` before `WgpuSdlSurface` / `Graph` (`0xC0000005` COM teardown fault, **from documentation**: `ranvier-cohort-check` skill).

---

## 3. Constraints and Cohort Compatibility

- **Runtime & SDK floor:** `net10.0`, `FSharp.Core` `10.0.100` (**from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu/Ranvier.Wgpu.fsproj` and `Ranvier.D5` commit `5568243`), `TreatWarningsAsErrors=true`, `WarnOn=3390` XML documentation on all public symbols, `IsAotCompatible=true` with trim/AOT analyzers enabled, and `.Traced` package twins matching `$(RanvierPackageSuffix)` (**from documentation**: `Ranvier.D5/Directory.Build.props`).
- **`Ranvier.D5` public API audits:** Both `Ranvier.D5.Wgpu` and `Ranvier.D5.Wgpu.Sdl` must pass `tools/audit-api-docs.fsx` (`0` undocumented public types or module members, avoiding `<paramref>` tags; **from documentation**: `Ranvier.D5/docs/api-audit.md`) and `tools/audit-mutable-signatures.fsx` (`0` public signatures exposing raw arrays, `ResizeArray`, `Dictionary`, or `HashSet`—public inputs and outputs use `ReadOnlySpan<'T>`, `ReadOnlyMemory<'T>`, `IReadOnlyList<'T>`, or opaque immutable wrapper structs/records over private backing arrays, matching `DensityGrid` in `Ranvier.D5/src/Ranvier.D5/Density.fs`). Construction-time programmer errors raise `ArgumentException`; data-dependent operations return `Result<_, LayoutError list>`.
- **Browser / `Fickl` parity:** `Ranvier.D5.Wgpu` packs `<None Include="$(MSBuildProjectFile);*.fs" Pack="true" PackagePath="fable/" />` (matching `Ranvier.D5.fsproj`), avoids `.NET`-only reflection or unguarded `Span` constructs unsupported by `Fickl`, and avoids polymorphic JS `setItem` array-write bottlenecks on inner loops where applicable (**from documentation**: `AstroShowcase.Web/measure/README.md`).
- **Explicit purity vs. ownership:** Pure geometry, scale, color-ramp, surface-mesh, marching-cubes, and axis-frame builders take immutable inputs and create no signals, timers, or GPU handles; reactive projections and GPU batch sinks live in dedicated owned modules (`Reactive3D`, `PlotStage3D`) attached to `Graph.Current` / `Api.getOwner()` with deterministic `onCleanup` / `IDisposable` release.
- **Workspace isolation:** Existing uncommitted edits in `C:/wk/ranvier-release/Ranvier.D5` (`main`) and `C:/wk/ranvier-release/Ranvier.Wgpu` (`perf/lazy-offscreen-targets`) must remain untouched; implementation uses dedicated git worktrees branched from `origin/main`.

---

## 4. Architecture and Decided Design

### 4.1 Package Ownership and Dependency Graph

| Repository | Project / Package | Dependencies | Role |
| :--- | :--- | :--- | :--- |
| `Ranvier.Wgpu` | `Ranvier.Wgpu` (`+ Fable.Ranvier.Wgpu`) | `FSharp.Core 10.0.100` | Adds `MeshHandle.UpdateVertices` / `UpdateVertexRange` / `UpdateMesh`, batch `UpdateRange` / `Resize`, and `Camera3D` orthographic / `perspectiveness` helpers |
| `Ranvier.D5` | `Ranvier.D5` (`+ .Traced`) | Existing cohort (`Ranvier`, `Anime`, `Css`) | Cherry-picks `Geometry3D.fs` (`[<Struct>] Point3`, `[<Struct>] Bounds3`) unmodified from `feat/layout-engines` (`09862250d1fd36e1d94d0533359b1e64710722b7`) |
| `Ranvier.D5` | `Ranvier.D5.Wgpu` (`+ .Traced`) | `Ranvier.D5`, `Ranvier.Wgpu` | `.NET` 3D scales/axes/encodings/meshes + `Ranvier` `Projection`/`KeyedCollection` GPU batch sinks + `PlotCamera3D` + `Probe3D` |
| `Ranvier.D5` | `Fable.Ranvier.D5.Wgpu` (`+ .Traced`) | `Ranvier.D5`, `Fable.Ranvier.Wgpu` | Browser/`Fickl` twin compiling the shared `Ranvier.D5.Wgpu` sources + `fable/` package payload without `Ranvier.Wgpu` assembly/type collisions |
| `Ranvier.D5` | `Ranvier.D5.Wgpu.Sdl` (`+ .Traced`) | `Ranvier.D5.Wgpu`, `Ranvier.D5.View`, `Ranvier.Wgpu.Sdl`, `Ranvier.View.WebCore.Sdl` (`>= 0.2.0`) | Native `Ranvier.View.Dsl` interactive 3D plot component (`WgpuSdlSurface` + SVG label overlay + pointer/wheel/key controller + idle-resting loop) |

*(**Builder call:** Shipping `Fable.Ranvier.D5.Wgpu` as a dedicated browser twin alongside `Ranvier.D5.Wgpu`—mirroring `Ranvier.Wgpu` / `Fable.Ranvier.Wgpu`, `Ranvier.Keyboard.Dom` / `Fable.Ranvier.Keyboard.Dom`, and `Ranvier.View.Anime` / `Fable.Ranvier.View.Anime`—avoids NuGet `.nuspec` dependency baking and duplicate `Ranvier.Wgpu.*` type collisions under `dotnet fickl`. Keeping `Ranvier.D5.Wgpu` free of `Ranvier.View.WebCore.Sdl` and isolating native DOM/SDL mounting in `Ranvier.D5.Wgpu.Sdl` lets headless offscreen tests and benchmarks run without SDL/WebCore. In `Ranvier.D5/Directory.Build.props`, `RanvierViewVersion` / `RanvierViewWebCore*Version` floors align to `0.2.0` to match `Ranvier.Wgpu.Sdl` `0.2.1` (**from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu.Sdl/Ranvier.Wgpu.Sdl.fsproj`).)*

### 4.2 Upstream `Ranvier.Wgpu` Additions

1. **Dynamic `MeshHandle` vertex updates (`Batches.fs` in both `Ranvier.Wgpu` and `Fable.Ranvier.Wgpu`):**
   - `member UpdateVertices : vertices: ReadOnlySpan<Vertex3D> -> unit` (and `Vertex3D array` overload): grows `vertexBuffer` if `vertices.Length > vertexCapacity`, updates `vertexCount` / `VertexBytes`, and writes via `wgpuQueueWriteBuffer` (`.NET`) or `backend.WriteBuffer` (`Fable`). Index buffer remains untouched when topology is unchanged.
   - `member UpdateVertexRange : startVertex: int * vertices: ReadOnlySpan<Vertex3D> -> unit`: validates bounds `[startVertex, startVertex + vertices.Length) <= vertexCount` and writes only that byte slice at `startVertex * sizeof<Vertex3D>`.
   - `member UpdateMesh : mesh: MeshData -> unit`: updates both vertex and index buffers in-place (reusing capacity when large enough) for variable-topology surfaces such as `IsoSurface3D` or `None`-masked grids whose active triangle count changed.
2. **Sub-range batch writes (`Batches.fs` + `Backend.fs` / `WebGpuBackend.fs` / `WebGl2Backend.fs`):**
   - `InstanceBatch.UpdateRange(startIndex: int, instances: ReadOnlySpan<Instance3D>)`
   - `LineBatch.UpdateRange(startVertex: int, vertices: ReadOnlySpan<LineVertex3D>)` (requiring even `startVertex` and even `vertices.Length`)
   - `DiscBatch.UpdateRange(startIndex: int, discs: ReadOnlySpan<DiscInstance3D>)`
   - Also expose `Resize(count: int)` (retaining existing buffer contents up to `min oldCount count`, growing capacity if needed) so `ProjectionDelta` additions/removals adjust active count without re-uploading untouched prefix slots.
3. **Orthographic and continuous `perspectiveness` on `Camera3D` (`Types.fs`):**
   - Preserve `[<Struct>] Camera3D`'s exact `6` fields (`Position`, `Target`, `Up`, `FovYDeg`, `NearPlane`, `FarPlane`) for `100%` source/binary compatibility (**from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu/Types.fs`).
   - Convention (**Builder call**): when `cam.FovYDeg < 0.0f`, `-cam.FovYDeg` encodes the orthographic view half-height `orthoHalfHeight` in world units; `Camera3D.projMatrix` emits the right-handed WebGPU `[0, 1]` orthographic projection matrix:
     `Matrix4x4(1 / (orthoHalfHeight * safeAspect), 0, 0, 0,  0, 1 / orthoHalfHeight, 0, 0,  0, 0, 1 / (zn - zf), 0,  0, 0, zn / (zn - zf), 1)`.
   - Add `Camera3D.orthographic (target: Vector3) (yawDeg: float32) (pitchDeg: float32) (halfHeight: float32) (distance: float32) : Camera3D` and `Camera3D.orbitPerspectiveness (target: Vector3) (yawDeg: float32) (pitchDeg: float32) (fitRadius: float32) (perspectiveness: float32) : Camera3D` (at `0.0f` produces exact orthographic framing of `fitRadius`; for `(0.0f, 1.0f]` uses dolly-zoom `fovYDeg = perspectiveness * 45.0f` and `distance = fitRadius / sin(fovYRad * 0.5f)` so the bounding sphere of `fitRadius` stays invariant under rotation).

### 4.3 Shared `Geometry3D` in `Ranvier.D5`

In `Ranvier.D5/src/Ranvier.D5/Geometry3D.fs` (`namespace Ranvier.D5`, cherry-picked unmodified from sibling `feat/layout-engines` commit `09862250d1fd36e1d94d0533359b1e64710722b7` so both branches share identical blob hashes):
- `[<Struct>] type Point3` declared with explicit `val X: float`, `val Y: float`, `val Z: float` and `internal new` (avoiding F# record-field label shadowing of `Point.X` / `Point.Y` across `Ranvier.D5`), validated finite via `Point3.create`, with `distanceSquared`, `distance`, and `lerp`.
- `[<Struct>] type Bounds3 = private { MinX: float; MinY: float; MinZ: float; SizeX: float; SizeY: float; SizeZ: float }` validated finite and nonnegative via `Bounds3.create` and `Bounds3.ofExtrema`, with `minX/Y/Z`, `maxX/Y/Z`, `sizeX/Y/Z`, `minPoint`, `maxPoint`, `center`, `contains`, and `union` (half-diagonal bounding-sphere radius is computed in `Ranvier.D5.Wgpu` from `Bounds3.sizeX/Y/Z` so `Geometry3D.fs` needs no divergent edit).

### 4.4 3D Coordinate Framing: `Scale3D`, `BoxAspect`, and `Axis3D`

- **World coordinate convention (**Builder call**):** Data coordinates use standard scientific `(X, Y, Z)` where `X` and `Y` form the horizontal floor plane and `Z` is vertical height/elevation (mapping `(dataX, dataY, dataZ)` to `Ranvier.Wgpu`'s Y-up world space as `Vector3(worldX, worldZ, -worldY)` so right-handed orientation is preserved, `Camera3D` turntable orbits around `Vector3.UnitY`, and studio sky/ground hemisphere lighting aligns naturally with `+Z` data elevation). An explicit `UpAxis = ZUp | YUp` option on `Scale3D` allows direct `YUp` scenes when desired.
- **`BoxAspect`:**
  - `ChartDefault`: world extents `(2.0, 2.0, 4.0 / 3.0)` centered at `Vector3.Zero` (matching Makie `Axis3`'s `(1, 1, 2/3)` aspect ratio; **from documentation**: `docs.makie.org/stable/reference/blocks/axis3`).
  - `Cube`: `(2.0, 2.0, 2.0)`.
  - `Data`: proportional to domain spans normalized to max extent `2.0`.
  - `Auto`: `Data` unless max/min span ratio exceeds `4.0` (**from documentation**: `plotly.com/javascript/reference/layout/scene/`), in which case falls back to `ChartDefault`.
  - `Manual of wx: float * wy: float * wz: float`.
- **`Scale3D`:** Pairs three `AxisScale` values (`Continuous`, `Piecewise`, `TimeUtc`, or categorical `Band<string>`) with `BoxAspect` and `UpAxis`, exposing pure `tryMap : Point3 -> Vector3 voption`, `map : Point3 -> Vector3`, `invert : Vector3 -> Point3`, and `worldBounds : Bounds3`.
- **Camera-aware `Axis3D`:**
  - Given `Scale3D`, axis titles/tick configs, `Camera3D`, and viewport `(widthPx, heightPx)`:
    1. Classifies the `6` cuboid faces (**general knowledge**: not checked) using view direction (`cam.Position - cam.Target` for orthographic; face center to `cam.Position` for perspective) so only back-facing walls emit background quads (`InstanceFlags.Unlit ||| InstanceFlags.IgnoreSectionPlane`) and major/minor grid segments (`LineVertex3D[]`).
    2. For each of `X`, `Y`, `Z`, picks the outer silhouette edge (adjacent to one front-facing and one back-facing cuboid face) with outward screen-projected normal, emits `3`D tick hairlines along that spine, and projects tick and title anchors through `Camera3D.projectToScreen` into `AxisLabel2D` records carrying stable semantic keys (`"x/tick/0"`, `"x/title"`, etc. — **not** visible-order indices).

### 4.5 Multi-Dimensional Channel Encodings (`4`D–`7`D+)

- **`ColorRamp` (allocation-free GPU palette sampler):**
  - Wraps `Ranvier.D5.Scale` (`Continuous` or `Piecewise`) + a pre-sampled `RgbaColor[]` lookup table (`256` entries **Builder call**, built once from `Ranvier.D5.Chromatic` in `Srgb`, `Lab`, or `Hcl` via `Ranvier.Anime.ColorSpace`) + `ColorClipPolicy`:
    - `LowClip: RgbaColor voption` (`ValueNone` clamps to `lut[0]`)
    - `HighClip: RgbaColor voption` (`ValueNone` clamps to `lut[last]`)
    - `Missing: MissingColorPolicy` (`Omit` drops the mark/triangle; `Fallback of RgbaColor` renders a sentinel color).
  - Evaluates `ColorRamp.map : float -> RgbaColor` in `O(1)` arithmetic with `0` heap or string allocations.
- **`ShadingMode`:**
  - `Unlit` (`InstanceFlags.Unlit`): preserves exact perceptual `Lab`/`Hcl` colormap luminance without normal-lighting distortion (**from documentation**: `VAD-marks.pdf` in `docs/3d-prior-art.md`).
  - `Lit of roughness: float32 * emissive: float32`: uses `Ranvier.Wgpu` hemisphere + key/rim lighting for 3D surface shape perception (combinable with surface wireframe/contour lines).
- **GPU Uniform Scalar-LUT Shader (`ColorShader`):**
  - Optional `Shaders.composeScene` (`.NET` and `Fable` WebGPU) and `#if FABLE_COMPILER` `GlslShaders.composeScene` (browser WebGL2 fallback, **Builder call**) storing normalized scalar `t` in vertex/instance color channel and up to `13` palette stops + `[cmin, cmax, mode, flags]` in `WgpuContext.SetUserUniforms` (`16` `vec4<f32>` slots; **from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu/README.md`), allowing live `colorrange` / threshold scrubbing with `0` vertex-buffer uploads.
- **Additional quantitative/categorical channels:**
  - **Size / Anisotropic Scale (`5`th–`6`th dim):** `SizeChannel` mapping scalars to pixel radii (`DiscBatch`) or `Vector3(sx, sy, sz)` (`InstanceBatch`).
  - **Orientation (`6`th–`7`th dim):** `Orientation.alignYToVector (dir: Vector3)` (constructs a twist-free `Quaternion` aligning unit `+Y` cone/cylinder/arrow meshes to `(u, v, w)`) or explicit `Quaternion`.
  - **Radial ring fractions:** Exposes `DiscInstance3D`'s `verifiedFrac`, `inProgressFrac`, and `portalHollow` as optional secondary `[0, 1]` scalar channels on `Scatter3D`.
  - **Wall Projections & Section Cuts:** Pure builders projecting `Point3` clouds, trajectories, or `Ranvier.D5.Contour` 2D isolines onto active back/floor walls, plus reactive `SectionPlane` binding (`ScenePass.withSection`).

### 4.6 Core 3D Primitives and Mesh/Batch Kernels

1. **`Surface3D` (`SurfaceGrid` -> `MeshData` / `SurfaceBatch`):**
   - Supports `UniformGrid` `(nx, ny, xDomain, yDomain)`, `RectilinearGrid` `(xs: float[], ys: float[])`, and `CurvilinearGrid` `(points: Point3[])` of dimensions `nx × ny`.
   - Accepts `heights: float[]` (for uniform/rectilinear) and optional independent `scalars: float[] voption` (`4`th dimension `surfacecolor`).
   - Computes smooth central-difference world-space vertex normals in `O(nx * ny)` into a reusable `Vertex3D[]` scratch buffer, omitting the `2` cell triangles (**general knowledge**: not checked) whenever any of a cell's `4` corners is non-finite or `Missing = Omit`.
   - `SurfaceBatch` caches the uploaded `MeshHandle`, `InstanceBatch` (`doubleSided = true`), and active mask stamp: when `heights` or `scalars` update with the same valid-cell mask (e.g., a live `60` Hz wave plane), it calls `MeshHandle.UpdateVertices(ReadOnlySpan<Vertex3D>)` directly with `0` index-buffer or `MeshHandle` allocations.
2. **`Scatter3D` (`DiscBatch`):**
   - Packs `DiscInstance3D` spans with `PickId` assignment; when any mark has `alpha < 1.0f` and `SortTranslucent = true`, sorts indices back-to-front against `Camera3D.Position` into a reusable buffer.
3. **`Glyph3D` / `Bar3D` / `Arrows3D` (`InstanceBatch`):**
   - `Glyph3D`: instances any `MeshHandle` (`unitIcosphere`, `unitBox`, `unitCylinder`, `unitCone`, or custom `MeshData`) with position, `Vector3` scale, `Quaternion` rotation, `RgbaColor`, `ShadingMode`, and `PickId`.
   - `Bar3D`: specialized `unitBox` instancing anchoring each column cleanly from `Scale.map zScale baseline` to `Scale.map zScale value` with footprint `(dx, dy)` derived from `Band` widths or explicit numeric widths.
   - `Arrows3D`: composite unit arrow mesh (cylinder shaft + cone tip via `MeshBuilder`) or separate shaft/tip batches oriented along `(u, v, w)` and scaled/colored by vector magnitude or an independent scalar channel.
4. **`Segments3D`, `Line3D`, `Tube3D`, and `Ribbon3D`:**
   - `Segments3D` / `Line3D`: converts disconnected pairs or `Point3 option` polylines (`None` breaks runs, matching `Ranvier.D5` contract) into `LineVertex3D` spans.
   - `Tube3D`: wraps `MeshBuilder.AddTube` with per-point world positions, radii, and `ColorRamp` vertex colors for variable-width 3D curves and streamtubes.
   - `Ribbon3D`: builds double-sided triangle strips between paired `lower[i]` and `upper[i]` 3D curves for confidence bands and waterfall curtains.
5. **`IsoSurface3D` and `VolumeSlices3D`:**
   - `VolumeGrid`: uniform or rectilinear `(nx, ny, nz)` `float32[]` / `float[]` scalar volume + optional secondary `colorVolume`.
   - `IsoSurface3D`: pure, `CancellationToken`-checked 3D marching-cubes kernel with configurable vertex/triangle work budgets (matching `Ranvier.D5.ContourConfig`), optional boundary `Caps` on the `6` volume faces, smooth gradient normals, and optional secondary scalar interpolation onto extracted `Vertex3D.Color`.
   - `VolumeSlices3D`: extracts up to `3` orthogonal cross-section grids (`X = ix`, `Y = iy`, `Z = iz`) as colored `Surface3D` / `MeshHandle` planes inside the plot box.

### 4.7 Ranvier Reactive Projections, Collections, and Primitives (`Reactive3D`)

All stateful/reactive adapters build directly on `Ranvier` core (`Ranvier/src/Ranvier/Api.fs`, `Collections.fs`, `Projections.fs`, `Deltas.fs`, `Combinators.fs`):

1. **Keyed Incremental Batch Sinks (`KeyedInstanceSink<'K>`, `KeyedDiscSink<'K>`, `KeyedSegmentSink<'K>`):**
   - Accept any `Projection<'K, 'Mark>` (including `KeyedCollection<'K, 'Datum>.Rows |> Projection.map ...`, `Projection.filter`, `Projection.sortByDescending`, or `Ranvier.D5.Graph3D` node/edge projections) inside an active `Ranvier` `Owner`.
   - Attach `let reader = projection.NewValueReader()` inside `Api.createEffect`:
     - Maintain a dense CPU shadow array (`Instance3D[]` / `DiscInstance3D[]`), a `Dictionary<'K, int>` key-to-slot map, a slot-to-key `'K[]` table, and a monotonic or slot-indexed non-zero `PickId` table (`PickId = uint32 (slot + 1)` encoded with a `uint8` batch prefix in the high bits so up to `255` pickable layers share one `ScenePass` without `PickId` collisions — **Builder call**).
     - **Fast path (`not delta.IsReset && not delta.OrderChanged`):** iterate `delta.Changes`; for each `KeyChange.Changed` or `KeyChange.Replaced`, write the updated struct at `slot`, coalesce contiguous dirty slot runs, and call `batch.UpdateRange(runStart, span)`—touching **only** changed items on CPU and GPU.
     - **Membership / order path (`delta.OrderChanged` or `delta.IsReset`):** for unordered opaque batches, apply `KeyChange.Removed` via `O(1)` swap-remove with the last active slot (updating the moved key's `PickId` slot entry and calling `UpdateRange(removedSlot, 1)` + `Resize(newCount)`); for ordered/translucent batches (`SortView`), apply `delta.Positional` (`RemoveAt`, `InsertAt`, `Move`) and upload the minimal affected window `[minSlot..maxSlot]`.
2. **Split-Channel Reactive Arrays & Grids (`ChannelMemo`):**
   - For dense columnar arrays or `(nx, ny)` simulation grids where keys are fixed indices `0..N-1`: separate `positions: Memo<Vector3[]>`, `transforms: Memo<...>` and `colors: Memo<RgbaColor[]>` (or `heights: Signal<float[]>` + `scalars: Signal<float[]>`) combine in an owning effect that re-packs only the invalidated fields into a pre-allocated array before `UpdateInstances` / `UpdateVertices`.
   - Selection/hover highlights bind `Lookup<'K, bool>` from `Api.createSelector` (or multi-select `Lookup`) so changing the hovered or selected key wakes only the old and new key rows (`2` rows total; **from documentation**: `Ranvier/src/Ranvier/Api.fs`).
3. **Incremental Reactive Auto-Domains (`ReactiveDomain`):**
   - `ReactiveDomain.ofProjection (read: 'T -> float option) (rows: Projection<'K, 'T>) : Memo<(float * float) option>` uses `Projection.tryMinBy` and `Projection.tryMaxBy` (`O(1)` per non-extremum edit; **from documentation**: `Ranvier/src/Ranvier/Combinators.fs`) piped through `Scale.nice` with structural cutoff so steady-state interior mutations never invalidate `Scale3D` or unchanged marks.
4. **Interactive `Editable<'T>` State & Background Volume Workers:**
   - Camera pose (`Editable<PlotCamera3D>`), active section cut (`Editable<SectionPlane>`), and zoomed axis/color domains (`Editable<float * float>`) use `Api.createEditable` / `Api.createDraft` so local pointer drags override upstream defaults cleanly while `Reset()` restores the seed.
   - Expensive `IsoSurface3D` marching-cubes recomputations provide an optional `ReactiveIsoSurface.create` helper (matching `Ranvier.D5.View.ReactiveLayout`) publishing generational results back to the graph thread.

### 4.8 Camera, Picking, Probing, and Native SDL View (`Ranvier.D5.Wgpu.Sdl`)

- **`PlotCamera3D`:** Immutable value type supporting `Turntable` (`AzimuthRad`, `ElevationRad` clamped to `[-89°, +89°]`, `Zoom`, `Target: Vector3`, `Perspectiveness` defaulting to `0.0f` orthographic) and `Arcball` (`Rotation: Quaternion`, `Zoom`, `Target`, `Perspectiveness`), pure pointer/wheel/key reducer (`DragOrbit`, `DragPan` with optional `X`/`Y`/`Z` axis lock, `WheelZoom`, `FocusAt`, `Reset`), and `toCamera3D (fitRadius: float32) : Camera3D`.
- **`Probe3D`:** Maps a `PickHit` (`PickId`, `WorldPosition`) back to:
  1. `PickedKey: (uint8 * 'K) voption` via the stage's `PickId` registry,
  2. `DataPoint: Point3` via `Scale3D.invert hit.WorldPosition`, and
  3. `HoverSpikes: LineVertex3D[]` (`3` orthogonal hairline drop-segments from `hit.WorldPosition` to the active back/floor walls).
- **`Ranvier.D5.Wgpu.Sdl` (`PlotView3D`):**
  - Built with `Ranvier.View.Dsl`: mounts a container `div` holding a `<canvas>` (bound via `WgpuSdlSurface.lend`) and a layered pointer-transparent `Ranvier.D5.View` `Renderer.passive` SVG overlay (`0` per-mark DOM listeners; **from documentation**: `AstroShowcase.Web/measure/README.md`) rendering `AxisLabel2D` ticks, axis titles, and optional hover readout.
  - Binds `div.onPointerdown`, `div.onPointermove`, `div.onPointerup`, `div.onPointercancel`, `div.onWheel`, and `div.onKeydown` (`Ranvier.View.Dsl` `tag.on*` attributes + `NodeRef`), coalescing GPU picking to pointer moves and resting the `RequestAnimationFrame` render loop whenever no reactive dirty flag or camera glide is active.
  - Documents and enforces safe disposal order (`mounted` and `Graph` disposed before `WgpuContext` and `Window`).

### 4.9 Cross-Repository Delivery and Release Sequence (Decided)

Per owner confirmation, upstream `Ranvier.Wgpu` additions and downstream `Ranvier.D5` packages are pushed, merged, and published to `nuget.org` following `C:/wk/ranvier-release/RELEASE-RUNBOOK.md` and `C:/wk/ranvier-release/AGENTS.md` (with merges, tags/releases, and workflow dispatches executed as coordinator steps):

1. **Stage 1 — Upstream `Ranvier.Wgpu` (`C:/wk/ranvier-release/_wt/wgpu-d5-primitives` from `origin/main`):**
   - Land `MeshHandle` dynamic vertex/mesh updates, batch `UpdateRange`/`Resize`, and `Camera3D` orthographic + `orbitPerspectiveness` across `Ranvier.Wgpu` and `Fable.Ranvier.Wgpu` with full `.NET` and `Fickl`/browser tests.
   - Open a `feat(wgpu): ...` PR (leaving `<Version>` untouched for EasyBuild.ShipIt), watch CI to green, squash-merge, close and reopen the `release/main` ShipIt PR so `GITHUB_TOKEN`-held checks run, squash-merge `release/main` keeping its exact `chore: release ...` title, watch `Publish NuGet`, and verify the new versions of `Ranvier.Wgpu`, `Ranvier.Wgpu.Sdl` (`+ .Traced`), and `Fable.Ranvier.Wgpu` in `https://api.nuget.org/v3-flatcontainer/<lowercase-id>/index.json`.
2. **Stage 2 — Downstream `Ranvier.D5` (`C:/wk/ranvier-release/_wt/d5-wgpu` on `feat/d5-wgpu` from `origin/main`):**
   - Cherry-pick `09862250d1fd36e1d94d0533359b1e64710722b7` (`Geometry3D.fs`), set `RanvierWgpuVersion` to the newly indexed `nuget.org` version (passing `--no-http-cache` if needed immediately after publish) and `RanvierView*Version` floors to `0.2.0` in `Directory.Build.props`, build and verify `Ranvier.D5.Wgpu`, `Fable.Ranvier.D5.Wgpu`, and `Ranvier.D5.Wgpu.Sdl` (`+ .Traced` twins), extend `ci.yml` pack/consumer/`Fickl` checks, merge to `main`, and run `Ranvier.D5`'s version-bump, GitHub Release artifact attachment (`14` `.nupkg`s across `7` package IDs, untraced and `.Traced`; **from documentation**: `RELEASE-RUNBOOK.md` `8` existing + `6` new here), and `publish.yml` dispatch (confirming NuGet Trusted Publishing policy coverage on `nuget.org` for the new package IDs before dispatch).

---

## 5. Verification, Performance Gates, and Example Scope

1. **Unit & Property Tests (`Ranvier.Wgpu.Tests` + `Ranvier.D5.Wgpu.Tests`):**
   - `MeshHandle.UpdateVertices` / `UpdateVertexRange`, `InstanceBatch`/`LineBatch`/`DiscBatch.UpdateRange`, and `Camera3D` orthographic + `perspectiveness` rendering, `projectToScreen`, `screenRay`, and `1×1` GPU `Pick` accuracy on offscreen `WgpuContext`.
   - `Point3` / `Bounds3`, `Scale3D` (`invert (map p) ≈ p` across linear/log/symlog/reversed/aspect modes), `Axis3D` back-wall & silhouette spine invariants across all `8` view octants (**general knowledge**: not checked), `ColorRamp` clip/missing policies (`0` allocations after setup), `Surface3D` analytical normal orientation & `None`-cell hole omission, `IsoSurface3D` closed-shell topology & budget cancellation, and `KeyedInstanceSink` / `KeyedDiscSink` `ProjectionDelta` add/change/remove/reset/PickId-retirement invariants (both untraced and `.Traced`).
2. **Browser / `Fickl` Compilation Gate:**
   - Compile `Fable.Ranvier.Wgpu` + `Ranvier.D5.Wgpu` through `fickl` under `FABLE_COMPILER` and pass packing/headless checks.
3. **Native Integration & Gallery Flagship (`Ranvier.D5.Wgpu.Native.Tests` + example):**
   - Hidden-window (`Window.Create(hidden = true)`) hardware pixel, resize, `WgpuSdlSurface` + SVG axis-label overlay, turntable drag, `1×1` GPU pick + `Scale.invert` hover spike, and clean teardown tests.
   - Live flagship wave-plane 3-axis demo (`z(x, y, t)` wave surface with independent 4th-dimension phase/curvature color, `Axis3D` bounds/grids/ticks, live `SectionPlane` slider, and interactive `KeyedCollection` probe markers) with `--capture` support.
4. **Performance Benchmarks:**
   - Zero-allocation steady-state gate on live `(128 × 128)` (**Builder call**) `Surface3D` frame updates (`MeshHandle.UpdateVertices`) and single-row `KeyedCollection` (`10,000` items, **Builder call**) `ProjectionDelta` `UpdateRange` updates, reporting every measured timing and allocation figure with explicit benchmark attribution.

---

## 6. Left to the Builder

- Internal module/file decomposition within `src/Ranvier.D5.Wgpu/` (keeping files focused by responsibility: coordinate/axis framing, color/channel encodings, surface/mesh/volume kernels, `Ranvier` reactive sinks, camera/picking).
- Exact marching-cubes lookup table representation and gradient-normal smoothing details inside `IsoSurface3D`.
- Exact default theme colors, tick lengths, and `Protrusions` pixel defaults (following `Ranvier.D5` / Makie `Axis3` conventions) when callers do not supply custom styling.
- Exact `PickId` bit-packing split (e.g., `8`-bit layer ID + `24`-bit `1`-based slot index vs. dense table handle) inside `PlotStage3D`.

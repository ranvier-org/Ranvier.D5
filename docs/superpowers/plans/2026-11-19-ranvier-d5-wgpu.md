# `Ranvier.D5.Wgpu` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build and publish upstream `Ranvier.Wgpu` dynamic-buffer/orthographic primitives and the `Ranvier.D5.Wgpu` (`+ Fable.Ranvier.D5.Wgpu` and `Ranvier.D5.Wgpu.Sdl`) 3D and multi-dimensional visualization packages powered by `Ranvier` projections, collections, and reactive primitives.

**Architecture:** Stage 1 adds in-place `MeshHandle` vertex/mesh updates, sub-range `UpdateRange`/`Resize` batch writes, and source-compatible orthographic + continuous `perspectiveness` camera projection across `Ranvier.Wgpu` and `Fable.Ranvier.Wgpu`, then releases them to `nuget.org` via ShipIt. Stage 2 builds `Ranvier.D5.Wgpu`, `Fable.Ranvier.D5.Wgpu`, and `Ranvier.D5.Wgpu.Sdl` in `Ranvier.D5`—combining `Scale3D` + Makie-style camera-aware `Axis3D`, allocation-free multi-dimensional `ColorRamp`/`Channel3D` encodings, `Surface3D`/`Scatter3D`/`Glyph3D`/`Bar3D`/`Arrows3D`/`Line3D`/`Tube3D`/`Ribbon3D`/`IsoSurface3D`/`VolumeSlices3D` kernels, `ProjectionDelta`-driven incremental GPU batch sinks, and `Ranvier.View.Dsl` zero-copy SDL + SVG overlay mounting.

**Tech Stack:** F# (`net10.0`, `FSharp.Core 10.0.100`), `Ranvier` `0.2.0`, `Ranvier.D5`, `Ranvier.Wgpu` / `Ranvier.Wgpu.Sdl` / `Fable.Ranvier.Wgpu`, `Ranvier.View` / `Ranvier.View.Dsl` / `Ranvier.View.WebCore.Sdl` (`>= 0.2.0`), `Fickl`, Expecto, Node + headless Chrome.

**Spec:** `C:/wk/ranvier-release/_wt/d5-wgpu/docs/superpowers/specs/2026-11-18-ranvier-d5-wgpu-design.md` (with prior art in `C:/wk/ranvier-release/_wt/d5-wgpu/docs/3d-prior-art.md`).

## Global Constraints

- **Target & analyzer floor:** `TargetFramework=net10.0`, `FSharp.Core` `10.0.100` (**from documentation**: `Ranvier.Wgpu/src/Ranvier.Wgpu/Ranvier.Wgpu.fsproj`), `TreatWarningsAsErrors=true`, `WarnOn=3390` XML docs on all public types/members, `IsAotCompatible=true` with trim/AOT analyzers enabled on core/SDL libraries, and `.Traced` twins via `$(RanvierPackageSuffix)` (**from documentation**: `Ranvier.D5/Directory.Build.props`).
- **`Ranvier.D5` public API audits:** `Ranvier.D5.Wgpu` and `Ranvier.D5.Wgpu.Sdl` must report `0` issues under `dotnet fsi --exec tools/audit-api-docs.fsx` (avoid `<paramref>` tags in XML summaries) and `0` issues under `dotnet fsi --exec tools/audit-mutable-signatures.fsx` (no `array`, `ResizeArray`, `Dictionary`, or `HashSet` in public `Ranvier.D5.*` member signatures—use `ReadOnlySpan<'T>`, `ReadOnlyMemory<'T>`, `IReadOnlyList<'T>`, or opaque immutable wrapper structs/records over private arrays; **from documentation**: `Ranvier.D5/docs/api-audit.md`).
- **Error channels:** Construction-time invalid configuration raises `ArgumentException` / `ArgumentOutOfRangeException`; data-dependent grid/mesh/volume builders return `Result<_, LayoutError list>`; never use untyped `failwith` on caller inputs (**from documentation**: `Ranvier.D5/docs/api-audit.md`).
- **`Ranvier.Wgpu` formatting & bindings:** All touched `Ranvier.Wgpu` and `Fable.Ranvier.Wgpu` files must pass `dotnet tool run fantomas --check` (**from documentation**: `Ranvier.Wgpu/.github/workflows/ci.yml`).
- **Workspace & shell safety:** Never touch dirty parent checkouts `C:/wk/ranvier-release/Ranvier.D5` or `C:/wk/ranvier-release/Ranvier.Wgpu`, and never touch sibling worktree `C:/wk/ranvier-release/_wt/d5-graph-layouts`. Use absolute paths; write multi-line scripts to files instead of heredocs; always pass `--exec` to `dotnet fsi`; run test/capture windows hidden (`Window.Create(..., hidden = true)`); label every figure in reports and commit notes (**measured here**, **from documentation**, or **general knowledge**) per `C:/wk/ranvier-release/AGENTS.md`.
- **Coordinator-only steps:** Merging PRs, closing/reopening ShipIt `release/main`, tagging/creating GitHub releases, and dispatching `publish.yml` must be run by the coordinator session, never by a subagent (**from documentation**: `C:/wk/ranvier-release/AGENTS.md`).

## Review Focus

1. **Antiparallel (`-UnitY`) and zero-length `(0, 0, 0)` vectors in `Orientation3D.alignYToVector`:** Naive `Cross(UnitY, v)` + `1 + dot` quaternion math produces `NaN` at `180°` (`-UnitY`) or zero norm; `-UnitY` must produce a valid unit quaternion rotating `+UnitY` to `-UnitY` (around `UnitX`), and near-zero vectors must return `Quaternion.Identity` (pinned in Task 6).
2. **Constant single-value domains (`lo = hi`) and extreme span ratios (`> 4×`) in `Scale3D` / `BoxAspect.Auto`:** Constant domains must map cleanly to the world-axis midpoint (`0.0f`) without `NaN` division, and `BoxAspect.Auto` must fall back from proportional `Data` extents to `ChartDefault` whenever the max-to-min span ratio exceeds `4.0` (**from documentation**: `plotly.com/javascript/reference/layout/scene/`, pinned in Task 5).
3. **Missing (`None`/`NaN`) cells, neighbor normals, and hole-mask changes in `Surface3D` / `SurfaceBatch`:** Missing grid samples must omit their `2` cell triangles without poisoning adjacent finite vertex normals with `NaN` (using one-sided finite differences at hole/grid boundaries), and changing which cells are `None`/`NaN` between frames must update the index buffer via `MeshHandle.UpdateMesh` rather than reusing stale indices (pinned in Task 7).
4. **Deleted-key `PickId` retirement and tail-swap re-indexing in `Reactive3D` sinks:** When `KeyedCollection.Remove` deletes a middle slot and swaps the tail key into that slot, `sink.TryResolvePick` / `Probe3D.resolveKeyed` must immediately return `ValueNone` for the retired slot's `PickId` (when count shrank) and resolve the swapped slot's `PickId` to the surviving tail key, never the deleted key (pinned in Task 10).
5. **Out-of-range / odd-vertex sub-buffer writes and disposal ordering:** `UpdateRange` past active item count or odd `startVertex`/`Length` on `LineBatch` must throw `ArgumentOutOfRangeException` / `ArgumentException` before native calls, and `PlotView3D` unmount must dispose `WgpuSdlSurface` cleanly before `WgpuContext`/`Window` teardown without throwing on repeated `Dispose()` (pinned in Tasks 1 and 11).

---

### Task 1: Upstream `Ranvier.Wgpu` Dynamic Buffers and Orthographic/`Perspectiveness` Camera (`.NET`)

**Files:**
- Worktree setup: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives` (branch `feat/d5-primitives` from `origin/main` of `C:/wk/ranvier-release/Ranvier.Wgpu`)
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Ranvier.Wgpu/Types.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Ranvier.Wgpu/Batches.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Ranvier.Wgpu/WgpuContext.fs`
- Test: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Ranvier.Wgpu.Tests/Program.fs`

**Interfaces:**
- Produces in `namespace Ranvier.Wgpu`:
  - `Camera3D.orthographic : target: Vector3 -> yawDeg: float32 -> pitchDeg: float32 -> halfHeight: float32 -> distance: float32 -> Camera3D` (sets `FovYDeg = -abs halfHeight`, `NearPlane = max 0.01f (distance - halfHeight * 8.0f)`, `FarPlane = distance + halfHeight * 8.0f`; throws `ArgumentOutOfRangeException` if `halfHeight <= 0.0f` or `distance <= 0.0f` or non-finite).
  - `Camera3D.orbitPerspectiveness : target: Vector3 -> yawDeg: float32 -> pitchDeg: float32 -> fitRadius: float32 -> perspectiveness: float32 -> Camera3D` (clamps `perspectiveness` to `[0.0f, 1.0f]`; at `<= 1e-4f` delegates to `orthographic target yawDeg pitchDeg fitRadius (max 4.0f (fitRadius * 4.0f))`; otherwise sets `fovYDeg = max 0.5f (perspectiveness * 45.0f)` and dolly-zoom `distance = fitRadius / MathF.Sin(fovYDeg * MathF.PI / 360.0f)` with `NearPlane = max 0.05f (distance - fitRadius * 2.5f)` and `FarPlane = distance + fitRadius * 2.5f`).
  - `Camera3D.projMatrix`: when `cam.FovYDeg < 0.0f`, treats `halfH = -cam.FovYDeg` and `halfW = halfH * max 0.05f aspect` and returns right-handed WebGPU `[0, 1]` orthographic `Matrix4x4(1.0f / halfW, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f / halfH, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f / (zn - zf), 0.0f, 0.0f, 0.0f, zn / (zn - zf), 1.0f)`.
  - `MeshHandle`:
    - `member VertexCount : int`
    - `member UpdateVertices : vertices: ReadOnlySpan<Vertex3D> -> unit` (and `Vertex3D array` overload)
    - `member UpdateVertexRange : startVertex: int * vertices: ReadOnlySpan<Vertex3D> -> unit` (throws `ArgumentOutOfRangeException` if `startVertex < 0 || startVertex + vertices.Length > this.VertexCount`)
    - `member UpdateMesh : mesh: MeshData -> unit` (updates both vertex and index buffers, growing GPU buffers by doubling when capacity is exceeded, updating `VertexCount`, `IndexCount`, `TriangleCount`)
  - `InstanceBatch`:
    - `member Resize : count: int -> unit` (throws `ArgumentOutOfRangeException` if `count < 0`; when growing past capacity, allocates new GPU buffer, copies existing prefix bytes via a retained CPU shadow `Instance3D array` or `wgpuCommandEncoderCopyBufferToBuffer`, and updates `Count`)
    - `member UpdateRange : startIndex: int * instances: ReadOnlySpan<Instance3D> -> unit` (throws `ArgumentOutOfRangeException` if `startIndex < 0 || startIndex + instances.Length > this.Count`)
  - `LineBatch`:
    - `member Resize : vertexCount: int -> unit` (throws `ArgumentException` if odd, `ArgumentOutOfRangeException` if `< 0`)
    - `member UpdateRange : startVertex: int * vertices: ReadOnlySpan<LineVertex3D> -> unit` (throws `ArgumentException` if `startVertex` or `vertices.Length` is odd; `ArgumentOutOfRangeException` if out of `[0, VertexCount]`)
  - `DiscBatch`:
    - `member Resize : count: int -> unit`
    - `member UpdateRange : startIndex: int * discs: ReadOnlySpan<DiscInstance3D> -> unit`

- [ ] **Step 1: Create worktree `C:/wk/ranvier-release/_wt/wgpu-d5-primitives` and stage `wgpu_native`**

Run:
```powershell
git -C 'C:/wk/ranvier-release/Ranvier.Wgpu' fetch origin
git -C 'C:/wk/ranvier-release/Ranvier.Wgpu' worktree add -b feat/d5-primitives 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives' origin/main
python 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/scripts/fetch_wgpu_native.py' --rid win-x64
```
Expected: Worktree created at `origin/main` and `runtimes/win-x64/native/wgpu_native.dll` verified.

- [ ] **Step 2: Write failing `.NET` tests in `tests/Ranvier.Wgpu.Tests/Program.fs`**

Add Expecto test cases covering:
1. `"Camera3D.orthographic preserves parallel screen size across depth and supports projectToScreen, screenRay, DiscBatch, and Pick"` — two spheres of radius `0.35f` at `( -0.8f, 0.0f, +1.2f )` and `( +0.8f, 0.0f, -1.2f )` under `Camera3D.orthographic Vector3.Zero 0.0f 0.0f 2.0f 6.0f` subtend equal pixel widths (`<= 1` px difference), `screenRay` returns parallel rays `(0, 0, -1)`, and `ctx.Pick` hits both spheres' `PickId`s with accurate `WorldPosition.X`.
2. `"Camera3D.orbitPerspectiveness transitions smoothly from 0 orthographic to 1 perspective while keeping fitRadius framed"` — `projectToScreen` of `(fitRadius * 0.95f, 0.0f, 0.0f)` stays inside `[0, width]` at `perspectiveness = 0.0f`, `0.5f`, and `1.0f`.
3. `"MeshHandle.UpdateVertices, UpdateVertexRange, and UpdateMesh update rendered pixels in place"` — upload a flat `2x2` quad mesh, render, then call `UpdateVertices` to raise height/change vertex color to green and verify center pixel turns green without recreating `InstanceBatch`; test `UpdateVertexRange` and `UpdateMesh` growing triangle count.
4. `"InstanceBatch, LineBatch, and DiscBatch Resize and UpdateRange mutate only target slots and reject invalid ranges"` — upload `3` colored instances, `UpdateRange(1, singleInstanceSpan)` changes only slot `1`'s rendered pixel and `PickId`, `Resize(2)` hides slot `2` while keeping slots `0..1` intact, `Resize(5)` preserves slots `0..1`, and out-of-bounds `UpdateRange` or odd `LineBatch.UpdateRange(1, ...)` throws `ArgumentOutOfRangeException` / `ArgumentException`.

- [ ] **Step 3: Run `Ranvier.Wgpu.Tests` to verify compilation/test failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Ranvier.Wgpu.Tests' -c Release`
Expected: FAIL (`Camera3D.orthographic`, `UpdateVertices`, `UpdateRange`, `Resize` not defined).

- [ ] **Step 4: Implement `Camera3D` orthographic/`perspectiveness` and `Batches.fs` dynamic updates**

Implement the exact signatures in `Types.fs`, `Batches.fs`, and optional curried module wrappers in `WgpuContext.fs`. In `Batches.fs`, retain a backing CPU array (`Instance3D[]`, `LineVertex3D[]`, `DiscInstance3D[]`) inside `InstanceBatch`, `LineBatch`, and `DiscBatch` so `Resize` growing capacity re-uploads existing prefix items cleanly in `1` `wgpuQueueWriteBuffer` call without GPU-to-GPU encoder overhead. Format edited files with `dotnet tool restore` and `dotnet tool run fantomas`.

- [ ] **Step 5: Run `Ranvier.Wgpu.Tests` and `fantomas --check`**

Run:
```powershell
dotnet build 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/Ranvier.Wgpu.sln' -c Release
dotnet run --project 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Ranvier.Wgpu.Tests' -c Release --no-build
dotnet tool run fantomas --check 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Ranvier.Wgpu/Types.fs' 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Ranvier.Wgpu/Batches.fs' 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Ranvier.Wgpu/WgpuContext.fs' 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Ranvier.Wgpu.Tests/Program.fs'
```
Expected: `0` warnings, `0` errors, all Expecto tests PASS, Fantomas check clean.

- [ ] **Step 6: Commit Task 1**

```powershell
git -C 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives' add src/Ranvier.Wgpu/Types.fs src/Ranvier.Wgpu/Batches.fs src/Ranvier.Wgpu/WgpuContext.fs tests/Ranvier.Wgpu.Tests/Program.fs
git -C 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives' commit -m "feat(wgpu): add orthographic camera projection, dynamic mesh updates, and sub-range batch writes"
```

---

### Task 2: `Fable.Ranvier.Wgpu` Browser WebGPU & WebGL2 Parity for Dynamic Buffers and Orthographic Camera

**Files:**
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Fable.Ranvier.Wgpu/Backend.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Fable.Ranvier.Wgpu/WebGpu.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Fable.Ranvier.Wgpu/WebGpuBackend.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Fable.Ranvier.Wgpu/WebGl2.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Fable.Ranvier.Wgpu/WebGl2Backend.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Fable.Ranvier.Wgpu/Batches.fs`
- Modify: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/src/Fable.Ranvier.Wgpu/WgpuContext.fs`
- Test: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Fable.Ranvier.Wgpu.Tests/PackingChecks.fs`
- Test: `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Fable.Ranvier.Wgpu.Tests/BrowserChecks.fs`

**Interfaces:**
- Consumes: Shared `Types.fs` (`Camera3D.orthographic`, `Camera3D.orbitPerspectiveness`) from Task 1.
- Produces:
  - `ISceneBackend.WriteBufferAt : buffer: IBackendBuffer * dstByteOffset: int * data: JS.ArrayBuffer * byteLength: int -> unit` and `WriteIndices : buffer: IBackendBuffer * indices: uint32 array -> unit` (with reallocation support in `MeshHandle.UpdateMesh`).
  - Matching public members on `Fable.Ranvier.Wgpu`'s `MeshHandle` (`VertexCount`, `UpdateVertices`, `UpdateVertexRange`, `UpdateMesh`), `InstanceBatch` (`Resize`, `UpdateRange`), `LineBatch` (`Resize`, `UpdateRange`), and `DiscBatch` (`Resize`, `UpdateRange`).

- [ ] **Step 1: Write failing browser/Node tests in `PackingChecks.fs` and `BrowserChecks.fs`**

Add checks in `PackingChecks.fs` verifying `Camera3D.orthographic` and `Camera3D.orbitPerspectiveness` matrices match `.NET` bit-for-bit within float32 tolerance, and checks in `BrowserChecks.fs` running on both `WebGPU` and `WebGL2` backends verifying `MeshHandle.UpdateVertices`, `InstanceBatch.UpdateRange`, `InstanceBatch.Resize`, and orthographic `PickAsync`.

- [ ] **Step 2: Run Fickl compile in `tests/Fable.Ranvier.Wgpu.Tests` to verify failure**

Run (in `C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Fable.Ranvier.Wgpu.Tests`):
```powershell
npm ci
npm run compile
```
Expected: FAIL (`UpdateVertices`, `UpdateRange`, `Resize` missing on `Fable.Ranvier.Wgpu` batch types).

- [ ] **Step 3: Implement `WriteBufferAt` on WebGPU & WebGL2 backends and mirror `Batches.fs` methods**

In `WebGpuBackend.fs`, call `queue.writeBuffer(buf, dstByteOffset, data, 0, byteLength)`. In `WebGl2Backend.fs`, bind `glBuf.Target` (`ARRAY_BUFFER`), and call `gl.bufferSubData(glBuf.Target, dstByteOffset, JS.Constructors.Uint8Array.Create(data, 0, byteLength))`; note on `MeshHandle.UpdateVertices` / `UpdateMesh`: if `MeshHandle`'s `vertexBuffer` or `indexBuffer` is reallocated when growing capacity, ensure `WebGl2Backend`'s cached `VertexArrayMesh` check invalidates and rebuilds any bound VAO referencing the old mesh buffer! Format all modified files with `fantomas`.

- [ ] **Step 4: Run full Node + headless Chrome WebGPU/WebGL2 + Fantomas + pack suite**

Run:
```powershell
Push-Location 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives/tests/Fable.Ranvier.Wgpu.Tests'
npm run compile
npm test
npm run golden
npm run test:browser
Pop-Location
Push-Location 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives'
dotnet tool run fantomas --check src/Ranvier.Wgpu/Native.fs src/Ranvier.Wgpu/Types.fs src/Ranvier.Wgpu/Primitives.fs src/Ranvier.Wgpu/Shaders.fs src/Ranvier.Wgpu/Batches.fs src/Ranvier.Wgpu/ScenePass.fs src/Ranvier.Wgpu/WgpuContext.fs src/Ranvier.Wgpu.Sdl/WgpuSdlSurface.fs tests/Ranvier.Wgpu.Tests/Program.fs src/Fable.Ranvier.Wgpu tests/Shared tests/Fable.Ranvier.Wgpu.Golden/Program.fs tests/Fable.Ranvier.Wgpu.Tests/PackingChecks.fs tests/Fable.Ranvier.Wgpu.Tests/BrowserChecks.fs samples/FableRanvierWgpuWaves/App.fs
dotnet pack src/Ranvier.Wgpu/Ranvier.Wgpu.fsproj -c Release -o artifacts/nupkg
dotnet pack src/Ranvier.Wgpu.Sdl/Ranvier.Wgpu.Sdl.fsproj -c Release -o artifacts/nupkg
dotnet pack src/Ranvier.Wgpu.Sdl/Ranvier.Wgpu.Sdl.fsproj -c Release -p:RanvierTrace=true -o artifacts/nupkg
dotnet pack src/Fable.Ranvier.Wgpu/Fable.Ranvier.Wgpu.fsproj -c Release -o artifacts/nupkg
Pop-Location
```
Expected: All Node, Chrome WebGPU, Chrome WebGL2, Fantomas, and `dotnet pack` steps PASS with `0` warnings/errors.

- [ ] **Step 5: Commit Task 2**

```powershell
git -C 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives' add src/Fable.Ranvier.Wgpu tests/Fable.Ranvier.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives' commit -m "feat(fable): add dynamic mesh updates and sub-range batch writes to Fable.Ranvier.Wgpu"
```

---

### Task 3 (Coordinator Step): Upstream `Ranvier.Wgpu` PR, ShipIt Release, and `nuget.org` Verification

**Files:**
- Remote repository: `ranvier-org/Ranvier.Wgpu` (follow `C:/wk/ranvier-release/RELEASE-RUNBOOK.md`, "Ranvier.Wgpu", and `ranvier-shipit-release` skill; **do not hand-edit `<Version>`**)

**Interfaces:**
- Consumes: Commits from Tasks 1–2 on `feat/d5-primitives`.
- Produces: Published and indexed `nuget.org` packages `Ranvier.Wgpu`, `Ranvier.Wgpu.Sdl`, `Ranvier.Wgpu.Sdl.Traced`, and `Fable.Ranvier.Wgpu` (expected `0.3.0` / `0.2.0` per ShipIt minor bump).

- [ ] **Step 1: Open tracking issue and PR on `ranvier-org/Ranvier.Wgpu`, confirm push, and watch CI**

Run (coordinator session):
```powershell
git -C 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives' push -u origin feat/d5-primitives
git -C 'C:/wk/ranvier-release/_wt/wgpu-d5-primitives' ls-remote origin feat/d5-primitives
gh pr create -R ranvier-org/Ranvier.Wgpu --head feat/d5-primitives --base main --title "feat(wgpu): add orthographic projection, dynamic mesh updates, and sub-range batch writes" --body "..."
gh pr checks -R ranvier-org/Ranvier.Wgpu --watch
```
Expected: All PR checks (`Test`, `Browser package`, `Packages`, `Conventional commits`) PASS.

- [ ] **Step 2: Squash-merge feature PR, unblock and merge ShipIt `release/main` PR, and watch `Publish NuGet`**

Run (coordinator session):
```powershell
gh pr merge -R ranvier-org/Ranvier.Wgpu --squash --delete-branch
# Wait for main CI + ShipIt workflow, then inspect release/main PR:
gh pr list -R ranvier-org/Ranvier.Wgpu --head release/main
# Close & reopen release/main to trigger GITHUB_TOKEN-held checks:
gh pr close <release-pr> -R ranvier-org/Ranvier.Wgpu
gh pr reopen <release-pr> -R ranvier-org/Ranvier.Wgpu
gh pr checks <release-pr> -R ranvier-org/Ranvier.Wgpu --watch
# Squash-merge preserving exact "chore: release ..." title:
gh pr merge <release-pr> -R ranvier-org/Ranvier.Wgpu --squash
```
Watch the release commit `CI` run and triggered `Publish NuGet` run with `gh run watch <id> -R ranvier-org/Ranvier.Wgpu --compact --exit-status`.

- [ ] **Step 3: Confirm `nuget.org` flat-container indexing and clean up `wgpu-d5-primitives` worktree**

Poll `https://api.nuget.org/v3-flatcontainer/<id>/index.json` inside one bounded PowerShell script for `ranvier.wgpu`, `ranvier.wgpu.sdl`, `ranvier.wgpu.sdl.traced`, and `fable.ranvier.wgpu` until all new versions appear, then remove `C:/wk/ranvier-release/_wt/wgpu-d5-primitives` (`git worktree remove` + `git worktree prune` + `git branch -d feat/d5-primitives`).

---

### Task 4: `Ranvier.D5` `Geometry3D` Cherry-Pick, Cohort Floor Alignment, and Project Scaffolding

**Files:**
- Modify (via cherry-pick `09862250d1fd36e1d94d0533359b1e64710722b7`): `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5/Geometry3D.fs`, `src/Ranvier.D5/Ranvier.D5.fsproj`, `tests/Ranvier.D5.Tests/*`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/Directory.Build.props`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Contracts3D.fs`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu/Fable.Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Ranvier.D5.Wgpu.Tests.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Program.fs`

**Interfaces:**
- Consumes: Published `Ranvier.Wgpu` and `Fable.Ranvier.Wgpu` versions from Task 3; commit `09862250d1fd36e1d94d0533359b1e64710722b7` (`Point3`, `Bounds3`).
- Produces:
  - `Directory.Build.props` properties: `RanvierViewVersion` (`0.2.0`), `RanvierViewWebCoreVersion` (`0.2.0`), `RanvierViewWebCoreSdlVersion` (`0.2.0`), `RanvierWgpuVersion` (`<published>`), `FableRanvierWgpuVersion` (`<published>`).
  - `Ranvier.D5.Wgpu.fsproj` (`PackageId = Ranvier.D5.Wgpu$(RanvierPackageSuffix)`, `IsAotCompatible = true`, references `../Ranvier.D5/Ranvier.D5.fsproj` and `Ranvier.Wgpu` `$(RanvierWgpuVersion)` — note `Ranvier.Wgpu` itself has no `.Traced` suffix because it is `Ranvier`-graph-free).
  - `Fable.Ranvier.D5.Wgpu.fsproj` (`PackageId = Fable.Ranvier.D5.Wgpu$(RanvierPackageSuffix)`, `<DefineConstants>$(DefineConstants);FABLE_COMPILER</DefineConstants>`, compiles `../Ranvier.D5.Wgpu/*.fs` via `<Compile Include="..." />` + packs `<None Include="$(MSBuildProjectFile)" Pack="true" PackagePath="fable/" />` and linked `*.fs` into `fable/`, referencing `../Ranvier.D5/Ranvier.D5.fsproj` and `Fable.Ranvier.Wgpu` `$(FableRanvierWgpuVersion)`).
  - `namespace Ranvier.D5.Wgpu` initial `Contracts3D.fs` (`internal Check3D` validation + `Vector3` / `Point3` conversion helpers `Point3.toVector3f`, `Bounds3.boundingRadius`).

- [ ] **Step 1: Cherry-pick `09862250d1fd36e1d94d0533359b1e64710722b7` onto `feat/d5-wgpu`**

Run:
```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' cherry-pick 09862250d1fd36e1d94d0533359b1e64710722b7
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Tests' -c Release
```
Expected: Clean cherry-pick with identical `Geometry3D.fs` blob hash and all `Ranvier.D5.Tests` passing.

- [ ] **Step 2: Scaffold `Ranvier.D5.Wgpu`, `Fable.Ranvier.D5.Wgpu`, and `Ranvier.D5.Wgpu.Tests` with initial `Contracts3D` smoke test**

Update `Directory.Build.props`, create the three `.fsproj` files, `Contracts3D.fs`, and `tests/Ranvier.D5.Wgpu.Tests/Program.fs` testing `Bounds3` bounding-sphere radius and `Ranvier.Wgpu` offscreen context creation (`dotnet restore --no-http-cache` if freshly published). Ensure `wgpu_native.dll` resolves cleanly from the restored `Ranvier.Wgpu` NuGet package in `Ranvier.D5.Wgpu.Tests`.

- [ ] **Step 3: Verify build, tests, and API audit scripts on `Ranvier.D5.Wgpu`**

Run:
```powershell
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-api-docs.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-mutable-signatures.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
```
Expected: Tests PASS, `Fable.Ranvier.D5.Wgpu` compiles with `0` warnings, `0` undocumented members, `0` mutable-signature violations.

- [ ] **Step 4: Commit Task 4**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add Directory.Build.props src/Ranvier.D5.Wgpu src/Fable.Ranvier.D5.Wgpu tests/Ranvier.D5.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(wgpu): scaffold Ranvier.D5.Wgpu and Fable.Ranvier.D5.Wgpu packages"
```

---

### Task 5: `Scale3D`, `BoxAspect`, and Camera-Aware `Axis3D` Framing

**Files:**
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Scale3D.fs`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Axis3D.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Ranvier.D5.Wgpu.fsproj`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu/Fable.Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/ScaleAxisTests.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Ranvier.D5.Wgpu.Tests.fsproj`, `Program.fs`

**Interfaces:**
- Consumes: `Ranvier.D5.Scale`, `Ranvier.D5.Format`, `Ranvier.D5.Point3`, `Ranvier.D5.Bounds3`, `Ranvier.Wgpu.Camera3D`, `LineVertex3D`, `Instance3D`.
- Produces in `namespace Ranvier.D5.Wgpu`:
  - `[<RequireQualifiedAccess; Struct>] type UpAxis = ZUp | YUp`
  - `[<RequireQualifiedAccess>] type BoxAspect = ChartDefault | Cube | Data | Auto | Manual of widthX: float * widthY: float * widthZ: float`
  - `[<RequireQualifiedAccess>] type AxisScale3D = Continuous of Continuous | Piecewise of Piecewise | TimeUtc of TimeScale | Categorical of Band<string>` (with `[<RequireQualifiedAccess>] module AxisScale3D`: `linear`, `log`, `symlog`, `piecewise`, `timeUtc`, `band`, `point`, `domainSpan`, `tryMapNormalized`, `invertNormalized`, `ticks`).
  - `type Scale3D = private ...` with `[<RequireQualifiedAccess>] module Scale3D`:
    - `create : aspect: BoxAspect -> upAxis: UpAxis -> x: AxisScale3D -> y: AxisScale3D -> z: AxisScale3D -> Scale3D`
    - `cartesian : xDomain: (float * float) -> yDomain: (float * float) -> zDomain: (float * float) -> Scale3D` (`ChartDefault` `(2.0, 2.0, 4.0 / 3.0)`, `ZUp`, linear scales)
    - `worldExtents : Scale3D -> Vector3`
    - `worldBounds : Scale3D -> Bounds3`
    - `fitRadius : Scale3D -> float32`
    - `tryMap : Scale3D -> Point3 -> Vector3 voption`
    - `map : Scale3D -> Point3 -> Vector3`
    - `tryMapCategory : Scale3D -> xKey: string option -> yKey: string option -> zVal: float -> Vector3 voption`
    - `invert : Scale3D -> Vector3 -> Point3` (for `ZUp`: maps `Vector3(wx, wy, wz)` back via `dataX = invertX wx`, `dataY = invertY (-wz)`, `dataZ = invertZ wy`)
  - `type Protrusions = { Left: float; Right: float; Top: float; Bottom: float }` (`Protrusions.defaultValue = { Left = 36.; Right = 36.; Top = 28.; Bottom = 32. }`)
  - `[<RequireQualifiedAccess; Struct>] type AxisId3D = X | Y | Z`
  - `type AxisConfig3D = { Title: string; TickCount: int; TickLengthWorld: float32; ShowGrid: bool; ShowPanel: bool; FormatTick: (float -> string) option }`
  - `type AxisTheme3D = { GridColor: RgbaColor; SpineColor: RgbaColor; TickColor: RgbaColor; PanelColor: RgbaColor; LabelColor: Paint; TitleColor: Paint; FontSize: float; TitleFontSize: float }`
  - `type Axis3DSpec = { Scale: Scale3D; X: AxisConfig3D; Y: AxisConfig3D; Z: AxisConfig3D; Theme: AxisTheme3D; Protrusions: Protrusions; ShowFrontSpines: bool }`
  - `type AxisLabel2D = { Key: string; Axis: AxisId3D; IsTitle: bool; ScreenPosition: Point; Anchor: string; Text: string }`
  - `type AxisFrame3D = private ...` with `[<RequireQualifiedAccess>] module Axis3D`:
    - `defaultSpec : Scale3D -> Axis3DSpec`
    - `build : spec: Axis3DSpec -> viewportWidth: int -> viewportHeight: int -> camera: Camera3D -> AxisFrame3D`
    - `lineVertices : AxisFrame3D -> ReadOnlyMemory<LineVertex3D>` (back-wall grids + silhouette spines + outward tick hairlines)
    - `panelInstances : AxisFrame3D -> ReadOnlyMemory<Instance3D>` (up to `3` thin back-facing wall quads with `InstanceFlags.Unlit ||| InstanceFlags.IgnoreSectionPlane`)
    - `labels : AxisFrame3D -> IReadOnlyList<AxisLabel2D>`
    - `overlayScene : viewportWidth: float -> viewportHeight: float -> frame: AxisFrame3D -> Scene` (builds a `Ranvier.D5.Scene` with stable keys `"axis/x/tick/0"`, `"axis/x/title"`, etc.)
    - `backWallPlanes : AxisFrame3D -> struct (float32 * float32 * float32)` (returns the active back-facing `(worldBackX, worldFloorY, worldBackZ)` coordinates used by `Probe3D` hover spikes and `WallProjection3D`)

- [ ] **Step 1: Write failing `Scale3D` and `Axis3D` tests in `ScaleAxisTests.fs`**

Include Expecto tests pinning:
1. Round-trip `Scale3D.invert scale (Scale3D.map scale p)` within `1e-5` across `ZUp` and `YUp`, linear, `log 10.0`, `symlog`, and reversed `(10.0, -10.0)` domains.
2. Review Focus #2: constant domain `(5.0, 5.0)` maps to `0.0f` without `NaN`, and `BoxAspect.Auto` on `(0., 1.) × (0., 1.) × (0., 100.)` (`100× > 4×`) falls back to `ChartDefault` `(2.0f, 4.0f / 3.0f, 2.0f)` world extents whereas `(0., 2.) × (0., 1.) × (0., 1.)` (`2× <= 4×`) uses proportional `Data` extents.
3. All `8` camera octants (`(±1, ±1, ±1)` view directions, both orthographic and perspective) emit exactly `3` back-facing panel instances whose normals have negative dot product with the camera-to-wall vector (never occluding the box interior), `1` outer silhouette tick spine per axis, and stable `AxisLabel2D.Key` strings across rotation.

- [ ] **Step 2: Run `Ranvier.D5.Wgpu.Tests` to confirm failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release`
Expected: FAIL (`Scale3D` / `Axis3D` undefined).

- [ ] **Step 3: Implement `Scale3D.fs` and `Axis3D.fs`**

Implement `Scale3D` and `Axis3D`. Ensure every public type and module function carries a clean XML `<summary>` (no `<paramref>`) and exposes `ReadOnlyMemory<_>` / `IReadOnlyList<_>` (no raw arrays in public signatures).

- [ ] **Step 4: Run tests and API audits**

Run:
```powershell
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-api-docs.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-mutable-signatures.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
```
Expected: All tests PASS, `0` audit issues.

- [ ] **Step 5: Commit Task 5**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add src/Ranvier.D5.Wgpu src/Fable.Ranvier.D5.Wgpu tests/Ranvier.D5.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(d5-wgpu): add Scale3D, BoxAspect, and camera-aware Axis3D framing"
```

---

### Task 6: Multi-Dimensional Channel Encodings (`ColorRamp`, `ColorShader`, `Orientation3D`, `WallProjection3D`)

**Files:**
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Channels3D.fs`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/ColorShader.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Ranvier.D5.Wgpu.fsproj`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu/Fable.Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/ChannelTests.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Ranvier.D5.Wgpu.Tests.fsproj`, `Program.fs`

**Interfaces:**
- Consumes: `Scale3D`, `AxisFrame3D` from Task 5; `Ranvier.D5.Chromatic`, `Ranvier.D5.Contour`, `Ranvier.Wgpu.Shaders` (and `GlslShaders` under `#if FABLE_COMPILER`).
- Produces in `namespace Ranvier.D5.Wgpu`:
  - `[<RequireQualifiedAccess; Struct>] type MissingColorPolicy = Omit | Fallback of RgbaColor`
  - `[<Struct>] type ColorClipPolicy = { LowClip: RgbaColor voption; HighClip: RgbaColor voption; Missing: MissingColorPolicy }` (`ColorClipPolicy.clampAndOmit`, `ColorClipPolicy.withClips`)
  - `[<RequireQualifiedAccess; Struct>] type ShadingMode = Unlit | Lit of roughness: float32 * emissive: float32`
  - `type ColorRamp = private ...` with `[<RequireQualifiedAccess>] module ColorRamp`:
    - `sequential : scale: Continuous -> palette: Ranvier.Anime.SrgbColor list -> clip: ColorClipPolicy -> ColorRamp` (samples `256` `RgbaColor` entries in `ColorSpace.Lab` with `GamutPolicy.Clip` under `.NET` and `FableAnime` under `FABLE_COMPILER`, `alpha = 1.0f`)
    - `diverging : scale: Continuous -> palette: Ranvier.Anime.SrgbColor list -> clip: ColorClipPolicy -> ColorRamp`
    - `viridis : domain: (float * float) -> ColorRamp` and `turbo : domain: (float * float) -> ColorRamp` and `coolWarm : domain: (float * float) -> ColorRamp` (built-in perceptually uniform palettes alongside `Chromatic.blues` and `Chromatic.redBlue`)
    - `tryMap : ColorRamp -> float -> RgbaColor voption` (`ValueNone` when non-finite/unaccepted and `Missing = Omit`; `O(1)`, `0` allocations)
    - `mapOrFallback : ColorRamp -> float -> RgbaColor -> RgbaColor`
    - `stops : count: int -> ColorRamp -> ReadOnlyMemory<RgbaColor>`
  - `[<RequireQualifiedAccess>] module Orientation3D`:
    - `alignYToVector : direction: Vector3 -> Quaternion` (rotates `(0, 1, 0)` onto `Normalize direction`; handles `direction.Y < -0.99999f` via `Quaternion(1.0f, 0.0f, 0.0f, 0.0f)` and `LengthSquared < 1e-12f` via `Quaternion.Identity`)
    - `anisotropicMatrix : position: Vector3 -> direction: Vector3 -> radiusXZ: float32 -> lengthY: float32 -> Matrix4x4`
  - `[<RequireQualifiedAccess; Struct>] type WallPlane = XY | XZ | YZ`
  - `[<RequireQualifiedAccess>] module WallProjection3D`:
    - `projectPoint : scale: Scale3D -> activeBackWalls: struct (float32 * float32 * float32) -> plane: WallPlane -> point: Point3 -> Vector3`
    - `projectContours : scale: Scale3D -> activeBackWalls: struct (float32 * float32 * float32) -> plane: WallPlane -> color: RgbaColor -> contours: ContourPolygon list -> ReadOnlyMemory<LineVertex3D>`
  - `[<RequireQualifiedAccess>] module ColorShader`:
    - `wgsl : unit -> string` (via `Shaders.composeScene`: interprets `in.vertexColor.x` when `user.slots[0].w > 0.5` as raw scalar mapped against `[cmin = slots[0].x, cmax = slots[0].y, stopCount = slots[0].z]` across `user.slots[3..15]`, with `lowClip` in `slots[1]` and `highClip` in `slots[2]`)
    - `#if FABLE_COMPILER` `glsl : unit -> string` (matching `GlslShaders.composeScene`)
    - `packUniforms : cmin: float32 -> cmax: float32 -> ramp: ColorRamp -> ReadOnlyMemory<Vector4>` (`16` `Vector4`s ready for `ctx.SetUserUniforms(mem.ToArray())` or slot loop)

- [ ] **Step 1: Write failing `ChannelTests.fs` tests**

Pin:
1. `ColorRamp.tryMap` accuracy at endpoints/midpoint, `LowClip`/`HighClip` overrides, `NaN` returning `ValueNone` under `Omit` vs sentinel color under `Fallback`, and `GC.GetAllocatedBytesForCurrentThread()` showing `0` bytes allocated across `10,000` warmed `ColorRamp.tryMap` calls.
2. Review Focus #1: `Orientation3D.alignYToVector` at `Vector3.UnitY`, `-Vector3.UnitY` (verifying `Vector3.Transform(Vector3.UnitY, q)` equals `(0.0f, -1.0f, 0.0f)` within `1e-5f` with `0` `NaN`s!), `Vector3.Zero` (`Quaternion.Identity`), and arbitrary diagonal vectors.
3. `WgpuContext.ValidateWgsl (ColorShader.wgsl ()) = Ok ()`, plus offscreen `WgpuContext` render confirming changing `cmin`/`cmax` via `ColorShader.packUniforms` shifts rendered quad color without re-uploading `MeshHandle`.

- [ ] **Step 2: Run tests to verify failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release`
Expected: FAIL (`ColorRamp`, `Orientation3D`, `ColorShader` undefined).

- [ ] **Step 3: Implement `Channels3D.fs` and `ColorShader.fs`**

Implement the exact modules and types, keeping all working arrays private behind `ReadOnlyMemory<_>` accessors.

- [ ] **Step 4: Run tests, Fickl build, and API audits**

Run:
```powershell
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-api-docs.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-mutable-signatures.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
```
Expected: All tests PASS, `0` audit issues.

- [ ] **Step 5: Commit Task 6**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add src/Ranvier.D5.Wgpu src/Fable.Ranvier.D5.Wgpu tests/Ranvier.D5.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(d5-wgpu): add allocation-free ColorRamp, ColorShader, Orientation3D, and wall projections"
```

---

### Task 7: `Surface3D` and Zero-Allocation Deforming `SurfaceBatch`

**Files:**
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Surface3D.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Ranvier.D5.Wgpu.fsproj`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu/Fable.Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/SurfaceTests.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Ranvier.D5.Wgpu.Tests.fsproj`, `Program.fs`

**Interfaces:**
- Consumes: `Scale3D`, `ColorRamp`, `ShadingMode` from Tasks 5–6; `MeshHandle.UpdateVertices` / `UpdateMesh` from Task 1; `Ranvier.D5.Delaunay`.
- Produces in `namespace Ranvier.D5.Wgpu`:
  - `type SurfaceGrid = private ...` with `[<RequireQualifiedAccess>] module SurfaceGrid`:
    - `uniform : columns: int -> rows: int -> xDomain: (float * float) -> yDomain: (float * float) -> heights: ReadOnlySpan<float> -> scalars: ReadOnlySpan<float> -> Result<SurfaceGrid, LayoutError list>` (`scalars` may be empty `ReadOnlySpan.Empty` to color by `heights`; `NaN`/`Infinity` marks missing samples without error unless `columns < 2 || rows < 2` or length mismatches `columns * rows`)
    - `fromFunction : columns: int -> rows: int -> xDomain: (float * float) -> yDomain: (float * float) -> evalHeight: (float -> float -> float) -> evalScalar: (float -> float -> float) voption -> Result<SurfaceGrid, LayoutError list>`
    - `rectilinear : xs: ReadOnlySpan<float> -> ys: ReadOnlySpan<float> -> heights: ReadOnlySpan<float> -> scalars: ReadOnlySpan<float> -> Result<SurfaceGrid, LayoutError list>`
    - `curvilinear : columns: int -> rows: int -> points: ReadOnlySpan<Point3> -> scalars: ReadOnlySpan<float> -> Result<SurfaceGrid, LayoutError list>`
  - `type SurfaceSpec = { Scale: Scale3D; Color: ColorRamp; Shading: ShadingMode; Opacity: float32; Wireframe: RgbaColor voption; PickId: uint32 }`
  - `type SurfaceMesh = private ...` (`vertices : ReadOnlyMemory<Vertex3D>`, `indices : ReadOnlyMemory<uint32>`, `wireframe : ReadOnlyMemory<LineVertex3D>`, `toMeshData : SurfaceMesh -> MeshData` internal/GPU helper)
  - `[<RequireQualifiedAccess>] module Surface3D`:
    - `build : spec: SurfaceSpec -> grid: SurfaceGrid -> SurfaceMesh`
    - `triangulatePoints : spec: SurfaceSpec -> points: IReadOnlyList<Point3> -> scalars: ReadOnlySpan<float> -> Result<SurfaceMesh, LayoutError list>` (uses `Ranvier.D5.Delaunay` over `(X, Y)` lifted by `Z`)
  - `[<Sealed>] type SurfaceBatch` (implements `IDisposable`):
    - `static member Create : context: WgpuContext * spec: SurfaceSpec * initialGrid: SurfaceGrid -> SurfaceBatch`
    - `member Batch : InstanceBatch`
    - `member WireframeBatch : LineBatch option`
    - `member Update : spec: SurfaceSpec * heights: ReadOnlySpan<float> * scalars: ReadOnlySpan<float> -> unit` (computes positions, finite-difference world normals, and `ColorRamp` colors directly into pre-allocated scratch `Vertex3D[]`; computes a rolling `uint64` validity-mask hash + active triangle count—if identical to previous frame, calls `meshHandle.UpdateVertices(ReadOnlySpan<Vertex3D>(scratchVertices))` with `0` heap allocations; if hole mask changed, rebuilds active `uint32[]` indices and calls `meshHandle.UpdateMesh`)
    - `member UpdateGrid : spec: SurfaceSpec * grid: SurfaceGrid -> unit`

- [ ] **Step 1: Write failing `SurfaceTests.fs` tests**

Pin:
1. Flat horizontal plane `z = 0` under `ZUp` produces upward `+UnitY` world normals; tilted plane `z = x` produces analytical upward-left normals with `0` `NaN`s.
2. Review Focus #3: `(5 × 5)` grid with center sample `(2, 2)` set to `Double.NaN` (`Missing = Omit`) omits the `8` triangles (`4` cells × `2` triangles; **general knowledge**: not checked) touching `(2, 2)`, keeps all adjacent vertex normals finite (`Vector3` components finite), and calling `SurfaceBatch.Update` transitioning from `0` `NaN`s to `1` `NaN` and back updates `MeshHandle.TriangleCount` accurately via `UpdateMesh`, whereas updating a finite wave `sin(x + t)` across `20` frames allocates `0` bytes per frame after `1` warmup call (`GC.GetAllocatedBytesForCurrentThread()`).
3. Independent `4`th-dimension `scalars` grid colors a constant-height `z = 1` plane with distinct left-vs-right pixels on offscreen `WgpuContext` (`ShadingMode.Unlit` matching `ColorRamp` sRGB tone-mapped output without normal-direction darkening).

- [ ] **Step 2: Run tests to verify failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release`
Expected: FAIL (`SurfaceGrid`, `Surface3D`, `SurfaceBatch` undefined).

- [ ] **Step 3: Implement `Surface3D.fs`**

Implement `SurfaceGrid`, `Surface3D`, and `SurfaceBatch`. Use central differences `(p[x+1] - p[x-1])` where both neighbors are finite and one-sided differences `(p[x+1] - p[x])` or `(p[x] - p[x-1])` next to grid edges or `NaN` holes so a single missing cell never contaminates neighbor normals.

- [ ] **Step 4: Run tests, Fickl build, and API audits**

Run:
```powershell
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-api-docs.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-mutable-signatures.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
```
Expected: All tests PASS (`0` bytes allocated on warmed `SurfaceBatch.Update`), `0` audit issues.

- [ ] **Step 5: Commit Task 7**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add src/Ranvier.D5.Wgpu src/Fable.Ranvier.D5.Wgpu tests/Ranvier.D5.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(d5-wgpu): add Surface3D and zero-allocation deforming SurfaceBatch"
```

---

### Task 8: Discrete, Vector, and Curve 3D Primitives (`Scatter3D`, `Glyph3D`, `Bar3D`, `Arrows3D`, `Line3D`, `Tube3D`, `Ribbon3D`)

**Files:**
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Marks3D.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Ranvier.D5.Wgpu.fsproj`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu/Fable.Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Marks3DTests.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Ranvier.D5.Wgpu.Tests.fsproj`, `Program.fs`

**Interfaces:**
- Consumes: `Scale3D`, `ColorRamp`, `ShadingMode`, `Orientation3D` from Tasks 5–6.
- Produces in `namespace Ranvier.D5.Wgpu`:
  - `[<Struct>] type ScatterDatum3D = { Position: Point3; RadiusPx: float32; Color: RgbaColor; RingPrimary: float32; RingSecondary: float32; RingThicknessPx: float32; PortalHollow: float32; PickId: uint32; Selected: bool }` (`ScatterDatum3D.create` simple constructor + `withRings` / `withHollow`)
  - `[<RequireQualifiedAccess>] module Scatter3D`:
    - `pack : scale: Scale3D -> cameraPos: Vector3 voption -> items: ReadOnlySpan<ScatterDatum3D> -> ReadOnlyMemory<DiscInstance3D>` (when `cameraPos.IsSome` and any item has `Color.A < 0.999f`, sorts translucent discs back-to-front by squared distance to `cameraPos.Value`)
  - `[<RequireQualifiedAccess; Struct>] type GlyphShape3D = Sphere | Box | Cylinder | Cone | Torus`
  - `[<Struct>] type GlyphDatum3D = { Position: Point3; Scale: Vector3; Rotation: Quaternion; Color: RgbaColor; Shading: ShadingMode; PickId: uint32; Selected: bool }`
  - `[<Struct>] type BarDatum3D = { X: float; Y: float; Z0: float; Z1: float; WidthX: float; WidthY: float; Color: RgbaColor; Shading: ShadingMode; PickId: uint32; Selected: bool }`
  - `[<Struct>] type ArrowDatum3D = { Origin: Point3; Vector: Point3; ShaftRadius: float32; LengthScale: float32; Color: RgbaColor; Shading: ShadingMode; PickId: uint32; Selected: bool }`
  - `[<RequireQualifiedAccess>] module Glyph3D`:
    - `unitMesh : GlyphShape3D -> MeshData`
    - `unitArrowMesh : segments: int -> MeshData` (cylinder shaft `[0.0f..0.72f]` + cone head `[0.72f..1.0f]` along `+Y`, centered/anchored at `y = 0.0f` so scaling by `(radius, length, radius)` places tail at `Origin` and tip at `Origin + dir * length`)
    - `packGlyphs : scale: Scale3D -> items: ReadOnlySpan<GlyphDatum3D> -> ReadOnlyMemory<Instance3D>`
    - `packBars : scale: Scale3D -> items: ReadOnlySpan<BarDatum3D> -> ReadOnlyMemory<Instance3D>`
    - `packArrows : scale: Scale3D -> items: ReadOnlySpan<ArrowDatum3D> -> ReadOnlyMemory<Instance3D>`
  - `[<RequireQualifiedAccess>] module Line3D`:
    - `segments : scale: Scale3D -> pairs: ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)> -> ReadOnlyMemory<LineVertex3D>`
    - `polyline : scale: Scale3D -> points: IReadOnlyList<struct (Point3 * RgbaColor) voption> -> ReadOnlyMemory<LineVertex3D>` (`ValueNone` breaks segment runs cleanly)
    - `tube : scale: Scale3D -> sides: int -> points: ReadOnlySpan<struct (Point3 * float32 * RgbaColor)> -> Result<MeshData, LayoutError list>`
    - `ribbon : scale: Scale3D -> samples: ReadOnlySpan<struct (Point3 * Point3 * RgbaColor)> -> Result<MeshData, LayoutError list>` (double-sided ruled strip between `lower` and `upper` `Point3` curves)

- [ ] **Step 1: Write failing `Marks3DTests.fs` tests**

Pin:
1. `Scatter3D.pack` renders and picks (`WgpuContext.Pick`) billboard discs, and sorts two overlapping translucent discs (`alpha = 0.5f`) far-to-near when `cameraPos` is supplied.
2. `Glyph3D.packBars` anchors positive (`0..5`) and negative (`0..-3`) columns on opposite sides of the `z = 0` plane; `Glyph3D.packArrows` orients `unitArrowMesh` from `(0, 0, 0)` toward `(+1, 0, 0)` so offscreen pixels appear on the `+X` half of the viewport and not `-X`.
3. `Line3D.polyline` splits runs across `ValueNone` gaps without bridging across the gap; `Line3D.tube` and `Line3D.ribbon` produce valid non-empty `MeshData` that render cleanly on `WgpuContext`.

- [ ] **Step 2: Run tests to verify failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release`
Expected: FAIL (`Marks3D` types/modules undefined).

- [ ] **Step 3: Implement `Marks3D.fs`**

Implement `Scatter3D`, `Glyph3D`, and `Line3D` with zero mutable types in public signatures and complete XML `<summary>` comments.

- [ ] **Step 4: Run tests, Fickl build, and API audits**

Run:
```powershell
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-api-docs.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-mutable-signatures.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
```
Expected: All tests PASS, `0` audit issues.

- [ ] **Step 5: Commit Task 8**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add src/Ranvier.D5.Wgpu src/Fable.Ranvier.D5.Wgpu tests/Ranvier.D5.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(d5-wgpu): add Scatter3D, Glyph3D, Bar3D, Arrows3D, Line3D, Tube3D, and Ribbon3D"
```

---

### Task 9: Volumetric Scalar Fields (`VolumeGrid`, `IsoSurface3D` Marching Cubes, and `VolumeSlices3D`)

**Files:**
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Volume3D.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Ranvier.D5.Wgpu.fsproj`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu/Fable.Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/VolumeTests.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Ranvier.D5.Wgpu.Tests.fsproj`, `Program.fs`

**Interfaces:**
- Consumes: `Scale3D`, `ColorRamp` from Tasks 5–6; `Ranvier.D5.LayoutError`.
- Produces in `namespace Ranvier.D5.Wgpu`:
  - `type VolumeGrid = private ...` with `[<RequireQualifiedAccess>] module VolumeGrid`:
    - `uniformWithBudget : maxVoxels: int -> nx: int -> ny: int -> nz: int -> bounds: Bounds3 -> values: ReadOnlySpan<float32> -> secondaryScalars: ReadOnlySpan<float32> -> Result<VolumeGrid, LayoutError list>` (`nx, ny, nz >= 2`, default budget in `VolumeGrid.uniform` = `4,194,304` voxels = `160^3` range)
    - `fromFunction : nx: int -> ny: int -> nz: int -> bounds: Bounds3 -> eval: (float -> float -> float -> float32) -> evalColorScalar: (float -> float -> float -> float32) voption -> Result<VolumeGrid, LayoutError list>`
    - `dimensions : VolumeGrid -> struct (int * int * int)`
    - `bounds : VolumeGrid -> Bounds3`
  - `type IsoSurfaceConfig = { Threshold: float32; CapBoundaries: bool; MaxTriangles: int; MaxVertices: int }` (`IsoSurfaceConfig.create threshold` with `CapBoundaries = true`, `MaxTriangles = 1_000_000`, `MaxVertices = 1_000_000`)
  - `[<RequireQualifiedAccess>] module IsoSurface3D`:
    - `computeWithCancellation : token: System.Threading.CancellationToken -> scale: Scale3D -> color: ColorRamp -> config: IsoSurfaceConfig -> volume: VolumeGrid -> Result<MeshData, LayoutError list>`
    - `compute : scale: Scale3D -> color: ColorRamp -> config: IsoSurfaceConfig -> volume: VolumeGrid -> Result<MeshData, LayoutError list>`
  - `type SliceIndices3D = { XIndex: int option; YIndex: int option; ZIndex: int option }`
  - `[<RequireQualifiedAccess>] module VolumeSlices3D`:
    - `extract : scale: Scale3D -> color: ColorRamp -> slices: SliceIndices3D -> volume: VolumeGrid -> Result<MeshData, LayoutError list>`

- [ ] **Step 1: Write failing `VolumeTests.fs` tests**

Pin:
1. Spherical distance field `f(x, y, z) = 1.0f - (x^2 + y^2 + z^2)` inside `[-1.5, +1.5]^3` at `Threshold = 0.0f` extracts a closed non-empty `MeshData` whose vertices all lie within `0.08f` of world radius `1.0` and carry outward normals (`dot(pos, normal) > 0.8f`).
2. Clipped sphere inside `[0.0, +1.5]^3` with `CapBoundaries = true` generates sealing triangles on the `x = 0`, `y = 0`, and `z = 0` boundary faces (higher triangle count than `CapBoundaries = false`).
3. Secondary `colorScalar` volume colors extracted isosurface vertices independently of the primary iso-threshold field; `MaxTriangles = 10` returns `Error [BudgetExceeded ...]`; pre-canceled `CancellationToken` raises `OperationCanceledException`.
4. `VolumeSlices3D.extract` with `{ XIndex = Some 8; YIndex = Some 8; ZIndex = Some 8 }` over `(16, 16, 16)` builds `3` orthogonal cross-section planes (`3 * 15 * 15 * 2 = 1350` triangles; **general knowledge**: not checked) and renders cleanly on `WgpuContext`.

- [ ] **Step 2: Run tests to verify failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release`
Expected: FAIL (`VolumeGrid`, `IsoSurface3D`, `VolumeSlices3D` undefined).

- [ ] **Step 3: Implement `Volume3D.fs`**

Implement `VolumeGrid`, standard `256`-case marching-cubes `IsoSurface3D` with shared-edge vertex deduplication, central-difference scalar-gradient normals, optional boundary face padding/capping, and `VolumeSlices3D`.

- [ ] **Step 4: Run tests, Fickl build, and API audits**

Run:
```powershell
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-api-docs.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-mutable-signatures.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
```
Expected: All tests PASS, `0` audit issues.

- [ ] **Step 5: Commit Task 9**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add src/Ranvier.D5.Wgpu src/Fable.Ranvier.D5.Wgpu tests/Ranvier.D5.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(d5-wgpu): add VolumeGrid, IsoSurface3D marching cubes, and VolumeSlices3D"
```

---

### Task 10: `Ranvier` Reactive Projections, Collections, Incremental GPU Sinks, `PlotCamera3D`, and `Probe3D`

**Files:**
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Interaction3D.fs`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Reactive3D.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/Ranvier.D5.Wgpu.fsproj`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu/Fable.Ranvier.D5.Wgpu.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Reactive3DTests.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests/Ranvier.D5.Wgpu.Tests.fsproj`, `Program.fs`

**Interfaces:**
- Consumes: `Ranvier` (`Api`, `KeyedCollection`, `Projection`, `ProjectionReader`, `ProjectionDelta`, `KeyChange`, `Lookup`, `Editable`, `Memo`, `Signal`), `Ranvier.Wgpu` (`UpdateRange`, `Resize`, `PickHit`), and Tasks 5–9.
- Produces in `namespace Ranvier.D5.Wgpu`:
  - `[<RequireQualifiedAccess; Struct>] type OrbitMode3D = Turntable | Arcball`
  - `[<RequireQualifiedAccess; Struct>] type AxisLock3D = Free | LockX | LockY | LockZ`
  - `[<Struct>] type PlotCamera3D = { Mode: OrbitMode3D; AzimuthRad: float32; ElevationRad: float32; Arcball: Quaternion; Zoom: float32; PanOffset: Vector3; Perspectiveness: float32 }` with `[<RequireQualifiedAccess>] module PlotCamera3D`:
    - `defaultValue : PlotCamera3D` (`Turntable`, `AzimuthRad = 1.275f * MathF.PI`, `ElevationRad = MathF.PI / 8.0f`, `Zoom = 1.0f`, `Perspectiveness = 0.0f` — matching Makie `Axis3`; **from documentation**: `docs.makie.org/stable/reference/blocks/axis3`)
    - `orbitBy : deltaPixelsX: float32 -> deltaPixelsY: float32 -> PlotCamera3D -> PlotCamera3D`
    - `panBy : fitRadius: float32 -> lockAxis: AxisLock3D -> deltaNormX: float32 -> deltaNormY: float32 -> PlotCamera3D -> PlotCamera3D`
    - `zoomBy : factor: float32 -> PlotCamera3D -> PlotCamera3D`
    - `focusOn : worldTarget: Vector3 -> PlotCamera3D -> PlotCamera3D`
    - `toCamera3D : scale: Scale3D -> PlotCamera3D -> Camera3D`
  - `[<Struct>] type PickedMark<'K> = { LayerId: byte; Key: 'K; WorldPosition: Vector3; DataPoint: Point3 }`
  - `type IPickResolver` (internal/public non-generic lookup for multi-layer `ScenePass`es) and `[<Sealed>] type KeyedInstanceSink<'K when 'K: equality>` / `[<Sealed>] type KeyedDiscSink<'K when 'K: equality>` / `[<Sealed>] type KeyedSegmentSink<'K when 'K: equality>`:
    - `member Batch : InstanceBatch` (or `DiscBatch` / `LineBatch`)
    - `member LayerId : byte`
    - `member ActiveCount : int`
    - `member LastUpdatedSlots : int` (diagnostic counter verifying `O(changed)` partial updates in tests/benchmarks)
    - `member TryResolvePick : pickId: uint32 -> 'K voption` (encodes `PickId = (uint32 layerId <<< 24) ||| uint32 (slot + 1)`; checks `layerId` match and `0 <= slot < activeCount`)
  - `[<RequireQualifiedAccess>] module Probe3D`:
    - `resolveKeyed : scale: Scale3D -> sink: KeyedInstanceSink<'K> -> hit: PickHit -> PickedMark<'K> voption` (and `resolveDisc : ... -> KeyedDiscSink<'K> -> ...`)
    - `invertHit : scale: Scale3D -> hit: PickHit -> Point3`
    - `hoverSpikes : scale: Scale3D -> axisFrame: AxisFrame3D -> color: RgbaColor -> worldPos: Vector3 -> ReadOnlyMemory<LineVertex3D>` (`3` orthogonal hairline drop-segments from `worldPos` onto `Axis3D.backWallPlanes axisFrame`)
  - `[<RequireQualifiedAccess>] module Reactive3D`:
    - `domainExtent : read: ('T -> float option) -> rows: Projection<'K, 'T> -> Memo<(float * float) option>` (built on `Projection.tryMinBy` and `Projection.tryMaxBy`)
    - `niceContinuousScale : tickCount: int -> fallbackDomain: (float * float) -> range: (float * float) -> read: ('T -> float option) -> rows: Projection<'K, 'T> -> Memo<Continuous>`
    - `editableCamera : seed: (unit -> PlotCamera3D) -> Editable<PlotCamera3D>` (`Api.createEditable`)
    - `editableSection : seed: (unit -> SectionPlane) -> Editable<SectionPlane>`
    - `bindInstances : context: WgpuContext -> layerId: byte -> mesh: MeshHandle -> blendMode: BlendMode -> doubleSided: bool -> preserveOrder: bool -> scale: (unit -> Scale3D) -> rows: Projection<'K, GlyphDatum3D> -> KeyedInstanceSink<'K>` (attaches `rows.NewValueReader()` inside `Api.createEffect`, coalescing `KeyChange.Changed` runs into `batch.UpdateRange`, handling `KeyChange.Removed` via `O(1)` tail-swap when `not preserveOrder` and `delta.Positional` when `preserveOrder`, and disposing GPU `InstanceBatch` via `Api.onCleanup`)
    - `bindBars : context: WgpuContext -> layerId: byte -> scale: (unit -> Scale3D) -> rows: Projection<'K, BarDatum3D> -> KeyedInstanceSink<'K>`
    - `bindDiscs : context: WgpuContext -> layerId: byte -> preserveOrder: bool -> scale: (unit -> Scale3D) -> rows: Projection<'K, ScatterDatum3D> -> KeyedDiscSink<'K>`
    - `bindSegments : context: WgpuContext -> scale: (unit -> Scale3D) -> rows: Projection<'K, struct (Point3 * Point3 * RgbaColor)> -> KeyedSegmentSink<'K>`
    - `bindSurface : context: WgpuContext -> spec: (unit -> SurfaceSpec) -> grid: (unit -> SurfaceGrid) -> SurfaceBatch` (attaches reactive `SurfaceBatch.UpdateGrid` effect + `Api.onCleanup` disposal)
    - `asyncIsoSurface : scale: (unit -> Scale3D) -> color: (unit -> ColorRamp) -> config: (unit -> IsoSurfaceConfig) -> volume: (unit -> VolumeGrid) -> AsyncMemo<Result<MeshData, LayoutError list>>` (uses `Api.createAsyncWithPolicy` to compute `IsoSurface3D.computeWithCancellation` off-thread on `.NET` with automatic `CancellationToken` cancellation of stale generations when threshold/volume signals move)

- [ ] **Step 1: Write failing `Reactive3DTests.fs` tests**

Pin:
1. `KeyedCollection<string, BarDatum3D>` with `100` items bound via `Reactive3D.bindBars`: mutating `1` key (`collection.AddOrUpdate`) sets `sink.LastUpdatedSlots = 1` (only `1` GPU slot written via `UpdateRange`!) and updates only that bar's pixel on `WgpuContext`.
2. `Api.createSelector` hover/selection integration: `collection.Rows |> Projection.mapByKey (fun k -> let d = collection.Rows.Get k in { d with Selected = hoveredSelector.Get k })` — changing `hoveredKey.Value` from `"k1"` to `"k2"` sets `sink.LastUpdatedSlots = 2` (only `"k1"` and `"k2"` re-evaluated and uploaded, not the other `98` items!).
3. Review Focus #4: `collection.Remove "k1"` on a `3`-item unordered `KeyedInstanceSink` swaps `"k3"` into slot `0` and shrinks `ActiveCount` to `2`: `sink.TryResolvePick ((1u <<< 24) ||| 3u)` immediately returns `ValueNone` (old slot `2` retired), `sink.TryResolvePick ((1u <<< 24) ||| 1u)` returns `ValueSome "k3"` (never `"k1"`!), and GPU `ctx.Pick` at `"k1"`'s old position returns `ValueNone` while picking at `"k3"`'s position returns `"k3"`.
4. `Reactive3D.niceContinuousScale` over `KeyedCollection`: interior edits inside `[0.0, 100.0]` do not wake a downstream scale observer (`runs = 1`), whereas inserting `145.0` widens the nice domain cleanly (`runs = 2`).
5. `Editable<PlotCamera3D>` + `Probe3D.hoverSpikes` + `Reactive3D.asyncIsoSurface`: local `orbitBy` overrides upstream camera seed until `Reset()`, `ctx.Pick` on a `SurfaceBatch` inverted via `Probe3D.invertHit` recovers `(dataX, dataY, dataZ)` within `0.05` and emits `6` `LineVertex3D` endpoints (`3` spikes), and `Reactive3D.asyncIsoSurface` cancels an superseded threshold run and settles on the latest threshold's `Ok mesh`.

- [ ] **Step 2: Run tests (normal and `-p:RanvierTrace=true`) to verify failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release`
Expected: FAIL (`Interaction3D` / `Reactive3D` undefined).

- [ ] **Step 3: Implement `Interaction3D.fs` and `Reactive3D.fs`**

Implement `PlotCamera3D`, `Probe3D`, `KeyedInstanceSink`, `KeyedDiscSink`, `KeyedSegmentSink`, and `Reactive3D`. Remember `Ranvier` purity rules (`Api.fs`): `rows.NewValueReader()` and `Api.onCleanup` must be created in an owning scope (`bind*` called inside component/root/owning scope, never inside a pure `createMemo`). Avoid `as this` on hot classes per `fsharp-jit-hot-paths` skill.

- [ ] **Step 4: Run tests (normal + `.Traced`), Fickl build, and API audits**

Run:
```powershell
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release
dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Tests' -c Release -p:RanvierTrace=true
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release
dotnet build 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Fable.Ranvier.D5.Wgpu' -c Release -p:RanvierTrace=true
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-api-docs.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
dotnet fsi --exec 'C:/wk/ranvier-release/_wt/d5-wgpu/tools/audit-mutable-signatures.fsx' 'C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu/bin/Release/net10.0' Ranvier.D5.Wgpu
```
Expected: Both untraced and `.Traced` suites PASS, `0` audit issues.

- [ ] **Step 5: Commit Task 10**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add src/Ranvier.D5.Wgpu src/Fable.Ranvier.D5.Wgpu tests/Ranvier.D5.Wgpu.Tests
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(d5-wgpu): add Ranvier ProjectionDelta GPU batch sinks, Reactive3D, PlotCamera3D, and Probe3D"
```

---

### Task 11: `Ranvier.D5.Wgpu.Sdl` Native Host View, Wave-Plane Flagship Example, Benchmarks, `Fickl` Smoke, and CI Wiring

**Files:**
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu.Sdl/Ranvier.D5.Wgpu.Sdl.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/src/Ranvier.D5.Wgpu.Sdl/PlotView3D.fs`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Native.Tests/Ranvier.D5.Wgpu.Native.Tests.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Native.Tests/Program.fs`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/benchmarks/Ranvier.D5.Wgpu.Benchmarks/Ranvier.D5.Wgpu.Benchmarks.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/benchmarks/Ranvier.D5.Wgpu.Benchmarks/Program.fs`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/examples/Ranvier.D5.Wgpu.WaveDemo/Ranvier.D5.Wgpu.WaveDemo.fsproj`
- Create: `C:/wk/ranvier-release/_wt/d5-wgpu/examples/Ranvier.D5.Wgpu.WaveDemo/Program.fs`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/.github/workflows/ci.yml`
- Modify: `C:/wk/ranvier-release/_wt/d5-wgpu/README.md`

**Interfaces:**
- Consumes: `Ranvier.D5.Wgpu` (Tasks 4–10), `Ranvier.D5.View` (`Renderer.passive`), `Ranvier.View.Dsl`, `Ranvier.Wgpu.Sdl` (`WgpuSdlSurface`), `Ranvier.View.WebCore.Sdl` (`Window`).
- Produces in `namespace Ranvier.D5.Wgpu.Sdl`:
  - `type PlotPass3D = { AxisSpec: Axis3DSpec; ClearColor: RgbaColor; Lighting: LightingRig voption; SectionPlane: SectionPlane voption; ExtraBatches: ReadOnlyMemory<InstanceBatch>; ExtraLines: ReadOnlyMemory<LineBatch>; ExtraDiscs: ReadOnlyMemory<DiscBatch>; ExtraOverlayMarks: Mark list }`
  - `type PlotViewConfig3D = { Width: int; Height: int; Camera: Editable<PlotCamera3D>; OnHoverHit: (PickHit voption -> unit) option; OnClickHit: (PickHit voption -> unit) option }`
  - `[<RequireQualifiedAccess>] module PlotView3D`:
    - `create : window: Window -> context: WgpuContext -> config: PlotViewConfig3D -> readScene: (unit -> PlotPass3D) -> ViewNode<unit>` (or `Ranvier.View.Dsl` element returning the mounted interactive container: binds `<canvas>` via `WgpuSdlSurface.lend`, owns internal `LineBatch`/`InstanceBatch` for `Axis3D` + hover spikes, mounts pointer-transparent `Renderer.passive` SVG overlay for `Axis3D.overlayScene` + `ExtraOverlayMarks`, wires `div.onPointerdown`/`onPointermove`/`onPointerup`/`onPointercancel`/`onWheel`/`onKeydown` via `NodeRef`, schedules `window.RequestAnimationFrame` only while dirty, and guards `Dispose()` idempotently).

- [ ] **Step 1: Write failing native SDL integration tests in `tests/Ranvier.D5.Wgpu.Native.Tests/Program.fs`**

Pin (using `Window.Create("D5WgpuTest", 480, 360, hidden = true)` on main thread for macOS compatibility):
1. Mount `PlotView3D.create` displaying a live `3`-axis wave `SurfaceBatch` + `KeyedCollection` probe spheres + `Axis3D` bounds: verify `window.ReadPixelsInto` has non-backdrop wave pixels AND the DOM holds `[data-d5-key='axis/x/title']` SVG text with `0` `wheel` listeners on marks.
2. Simulate pointer drag (`orbitBy`) and verify `[data-d5-key='axis/x/title']` retains DOM node reference identity (`Object.ReferenceEquals`) while changing projected `x`/`y` attributes and rendered pixels.
3. Verify `ctx.Pick` / hover probe on a `KeyedCollection` sphere updates hover spikes and `Probe3D` readout, and verify Review Focus #5: disposing `mounted.Dispose()`, then `graph.Dispose()`, then `wgpu.Dispose()`, then `win.Dispose()` (plus calling `mounted.Dispose()` twice) exits with `0` WebGPU/COM errors.

- [ ] **Step 2: Run `Ranvier.D5.Wgpu.Native.Tests` to confirm failure**

Run: `dotnet run --project 'C:/wk/ranvier-release/_wt/d5-wgpu/tests/Ranvier.D5.Wgpu.Native.Tests' -c Release`
Expected: FAIL (`PlotView3D` undefined).

- [ ] **Step 3: Implement `PlotView3D.fs`, `Ranvier.D5.Wgpu.WaveDemo` (`--capture <path.bmp>`), `Ranvier.D5.Wgpu.Benchmarks`, `README.md`, and `.github/workflows/ci.yml`**

In `WaveDemo`: build a live clock-driven `(96 × 96)` `SurfaceBatch` wave plane with an independent `4`th-dimension phase-velocity `ColorRamp`, `Axis3D` bounded frame, interactive `Editable<SectionPlane>` + `Editable<PlotCamera3D>`, and `KeyedCollection` extrema markers, supporting `--capture artifacts/wgpu-wave.bmp` in a hidden window (`AGENTS.md`: never pop a visible window unless requested!).
In `Benchmarks`: measure and gate steady-state `(128 × 128)` `SurfaceBatch.Update` (`0` managed bytes allocated on warmed step) and `1`-of-`10,000` `KeyedCollection` `ProjectionDelta` `UpdateRange` mutation.
In `.github/workflows/ci.yml`: add `mesa-vulkan-drivers libvulkan1` to the Ubuntu apt step, run `Ranvier.D5.Wgpu.Tests`, `Ranvier.D5.Wgpu.Native.Tests`, `Ranvier.D5.Wgpu.Benchmarks`, `WaveDemo -- --capture`, pack `Ranvier.D5.Wgpu`, `Fable.Ranvier.D5.Wgpu`, and `Ranvier.D5.Wgpu.Sdl` (untraced + `.Traced`), extend the `.NET` packed consumer check, and verify both direct `dotnet fickl src/Fable.Ranvier.D5.Wgpu` and packed-nupkg `Fable.Ranvier.D5.Wgpu` consumer execution under Node (`Scale3D` + `ColorRamp` + `Surface3D` pure JS smoke).

- [ ] **Step 4: Run full local verification matrix (untraced, `.Traced`, native, benchmarks, Fickl, API audits, pack)**

Run:
```powershell
Push-Location 'C:/wk/ranvier-release/_wt/d5-wgpu'
dotnet run --project tests/Ranvier.D5.Tests -c Release
dotnet run --project tests/Ranvier.D5.Wgpu.Tests -c Release
dotnet run --project tests/Ranvier.D5.Wgpu.Tests -c Release -p:RanvierTrace=true
dotnet run --project tests/Ranvier.D5.Native.Tests -c Release
dotnet run --project tests/Ranvier.D5.Wgpu.Native.Tests -c Release
dotnet run --project tests/Ranvier.D5.Wgpu.Native.Tests -c Release -p:RanvierTrace=true
dotnet run --project benchmarks/Ranvier.D5.Wgpu.Benchmarks -c Release -- artifacts/wgpu-benchmarks.json
dotnet run --project examples/Ranvier.D5.Wgpu.WaveDemo -c Release -- --capture artifacts/wgpu-wave.bmp
dotnet tool restore
dotnet fickl src/Fable.Ranvier.D5.Wgpu -o artifacts/fable/d5-wgpu --noPackageCache
dotnet fsi --exec tools/audit-api-docs.fsx src/Ranvier.D5.Wgpu/bin/Release/net10.0 Ranvier.D5.Wgpu
dotnet fsi --exec tools/audit-api-docs.fsx src/Ranvier.D5.Wgpu.Sdl/bin/Release/net10.0 Ranvier.D5.Wgpu.Sdl
dotnet fsi --exec tools/audit-mutable-signatures.fsx src/Ranvier.D5.Wgpu/bin/Release/net10.0 Ranvier.D5.Wgpu
dotnet fsi --exec tools/audit-mutable-signatures.fsx src/Ranvier.D5.Wgpu.Sdl/bin/Release/net10.0 Ranvier.D5.Wgpu.Sdl
for ($t in @('false', 'true')) {
    dotnet pack src/Ranvier.D5 -c Release -p:RanvierTrace=$t -o artifacts/packages
    dotnet pack src/Ranvier.D5.View -c Release -p:RanvierTrace=$t -o artifacts/packages
    dotnet pack src/Ranvier.D5.Charts -c Release -p:RanvierTrace=$t -o artifacts/packages
    dotnet pack src/Ranvier.D5.Geo -c Release -p:RanvierTrace=$t -o artifacts/packages
    dotnet pack src/Ranvier.D5.Wgpu -c Release -p:RanvierTrace=$t -o artifacts/packages
    dotnet pack src/Fable.Ranvier.D5.Wgpu -c Release -p:RanvierTrace=$t -o artifacts/packages
    dotnet pack src/Ranvier.D5.Wgpu.Sdl -c Release -p:RanvierTrace=$t -o artifacts/packages
}
Pop-Location
```
Expected: All test suites, benchmarks, hidden-window wave capture, Fickl JS compile, API audits (`0` undocumented, `0` mutable-signature violations), and `14` `.nupkg` packs PASS with `0` warnings/errors.

- [ ] **Step 5: Commit Task 11**

```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' add src/Ranvier.D5.Wgpu.Sdl tests/Ranvier.D5.Wgpu.Native.Tests benchmarks/Ranvier.D5.Wgpu.Benchmarks examples/Ranvier.D5.Wgpu.WaveDemo .github/workflows/ci.yml README.md
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' commit -m "feat(d5-wgpu): add Ranvier.D5.Wgpu.Sdl view, wave-plane demo, benchmarks, and CI gates"
```

---

### Task 12 (Coordinator Step): `Ranvier.D5` PR, Cross-Platform CI, Release Preparation, and `nuget.org` Publication

**Files:**
- Remote repository: `ranvier-org/Ranvier.D5` (follow `C:/wk/ranvier-release/RELEASE-RUNBOOK.md`, "Ranvier.D5")

**Interfaces:**
- Consumes: Branch `feat/d5-wgpu` (Tasks 4–11) and sibling coordination status on `ranvier-org/Ranvier.D5`.
- Produces: Merged `main`, GitHub Release `v<next>` holding all `14` `.nupkg`s (or `18` if merged after `Ranvier.D5.Graph` / `Ranvier.D5.Graph3D`), and published packages verified on `nuget.org`.

- [ ] **Step 1: Push `feat/d5-wgpu`, open PR on `ranvier-org/Ranvier.D5`, and watch `Foundation CI`**

Run (coordinator session):
```powershell
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' push -u origin feat/d5-wgpu
git -C 'C:/wk/ranvier-release/_wt/d5-wgpu' ls-remote origin feat/d5-wgpu
gh pr create -R ranvier-org/Ranvier.D5 --head feat/d5-wgpu --base main --title "feat(wgpu): add Ranvier.D5.Wgpu, Fable.Ranvier.D5.Wgpu, and Ranvier.D5.Wgpu.Sdl" --body "..."
gh pr checks -R ranvier-org/Ranvier.D5 --watch
```
Expected: All `6` matrix cells (`windows-latest`, `ubuntu-24.04`, `macos-latest` × `traced: false/true`) PASS. Squash-merge once green.

- [ ] **Step 2: Prepare release version PR (`0.2.0`), attach CI `.nupkg` artifacts to GitHub Release, and dispatch `publish.yml`**

Follow `RELEASE-RUNBOOK.md` ("Ranvier.D5" steps `1`–`5`):
1. Open and merge `"Prepare version 0.2.0"` updating `<Version>` in `Directory.Build.props`, `README.md`, consumer `[0.2.0]` pin in `.github/workflows/ci.yml`, and default tag `v0.2.0` in `.github/workflows/publish.yml`.
2. Wait for `Foundation CI` on `main` to pass and download `evidence-windows-latest-false` and `evidence-windows-latest-true`.
3. Confirm with the owner that `nuget.org` Trusted Publishing covers `Ranvier.D5.Wgpu`, `Fable.Ranvier.D5.Wgpu`, `Ranvier.D5.Wgpu.Sdl`, and their `.Traced` twins under `publish.yml` (if first-time package IDs require owner policy registration on `nuget.org`).
4. Create GitHub Release `v0.2.0` at `<main-sha>` with all packed `.nupkg` files attached and dispatch `gh workflow run publish.yml -R ranvier-org/Ranvier.D5 -f tag=v0.2.0`.

- [ ] **Step 3: Verify `nuget.org` flat-container index and clean up worktree**

Confirm every published package ID (`ranvier.d5.wgpu`, `ranvier.d5.wgpu.traced`, `fable.ranvier.d5.wgpu`, `fable.ranvier.d5.wgpu.traced`, `ranvier.d5.wgpu.sdl`, `ranvier.d5.wgpu.sdl.traced`, plus core cohort) lists `0.2.0` at `https://api.nuget.org/v3-flatcontainer/<id>/index.json`, then remove `C:/wk/ranvier-release/_wt/d5-wgpu` (`git worktree remove` + `git worktree prune` + `git branch -d feat/d5-wgpu`), leaving pre-existing worktrees untouched.

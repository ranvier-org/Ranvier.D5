namespace Ranvier.D5.Wgpu

open System
open System.Collections.Generic
open System.Numerics
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Validated immutable 2D height and optional 4th-dimension scalar grid supporting uniform, rectilinear, and curvilinear layouts.</summary>
type SurfaceGrid =
    private
        {
            Columns: int
            Rows: int
            IsCurvilinear: bool
            Xs: float array
            Ys: float array
            Heights: float array
            Scalars: float array
            CurviPoints: Point3 array
        }

/// <summary>Appearance, coordinate scale, shading, opacity, wireframe, and pick configuration for a 3D surface.</summary>
type SurfaceSpec =
    {
        /// <summary>3D data-to-world coordinate mapping.</summary>
        Scale: Scale3D
        /// <summary>Scalar-to-color lookup ramp.</summary>
        Color: ColorRamp
        /// <summary>Unlit perceptual palette shading or directional studio lighting.</summary>
        Shading: ShadingMode
        /// <summary>Surface alpha multiplier in <c>[0.0, 1.0]</c>.</summary>
        Opacity: float32
        /// <summary>Optional overlay grid wireframe line color.</summary>
        Wireframe: RgbaColor voption
        /// <summary>Hardware picking identifier (<c>0u</c> disables picking).</summary>
        PickId: uint32
    }

/// <summary>Immutable CPU-side triangulated 3D surface mesh with optional wireframe line-list vertices.</summary>
type SurfaceMesh =
    private
        {
            VerticesArr: Vertex3D array
            IndicesArr: uint32 array
            WireframeArr: LineVertex3D array
        }

/// <summary>Read-only accessors and GPU upload helpers for <c>SurfaceMesh</c>.</summary>
[<RequireQualifiedAccess>]
module SurfaceMesh =
    /// <summary>Returns a read-only view over the surface mesh vertices.</summary>
    let vertices (mesh: SurfaceMesh) : ReadOnlyMemory<Vertex3D> =
        ReadOnlyMemory<Vertex3D>(mesh.VerticesArr)

    /// <summary>Returns a read-only view over the active CCW triangle indices.</summary>
    let indices (mesh: SurfaceMesh) : ReadOnlyMemory<uint32> =
        ReadOnlyMemory<uint32>(mesh.IndicesArr)

    /// <summary>Returns a read-only view over paired <c>LineList</c> wireframe endpoints (empty when wireframe is disabled).</summary>
    let wireframe (mesh: SurfaceMesh) : ReadOnlyMemory<LineVertex3D> =
        ReadOnlyMemory<LineVertex3D>(mesh.WireframeArr)

    /// <summary>Materializes an immutable <c>MeshData</c> copy ready for <c>WgpuContext.UploadMesh</c>.</summary>
    let toMeshData (mesh: SurfaceMesh) : MeshData =
        {
            Vertices = Array.copy mesh.VerticesArr
            Indices = Array.copy mesh.IndicesArr
        }

module private SurfaceKernel =
    let copySpan (span: ReadOnlySpan<'T>) : 'T array =
        if span.Length = 0 then
            [||]
        else
            let arr = Array.zeroCreate<'T> span.Length

            for i = 0 to span.Length - 1 do
                arr[i] <- span[i]

            arr

    let validateGridDims (columns: int) (rows: int) : Result<int, LayoutError list> =
        if columns < 2 || rows < 2 then
            Error [ LayoutError.InvalidObservation "Surface grid requires at least 2 columns and 2 rows." ]
        else
            let count64 = int64 columns * int64 rows
            let maxIndices64 = int64 (columns - 1) * int64 (rows - 1) * 6L

            if count64 > int64 Int32.MaxValue then
                Error [ LayoutError.BudgetExceeded "Surface grid vertex count exceeds 32-bit capacity." ]
            elif maxIndices64 > int64 Int32.MaxValue then
                Error [ LayoutError.BudgetExceeded "Surface grid index count exceeds 32-bit capacity." ]
            else
                Ok (int count64)

    let isFiniteDomain (a: float, b: float) =
        Check3D.isFinite a && Check3D.isFinite b && Check3D.isFinite (b - a)

    let sampleLinearAxis (count: int) (a: float, b: float) : float array =
        let arr = Array.zeroCreate<float> count
        let denom = float (count - 1)
        let span = b - a

        for i = 0 to count - 1 do
            arr[i] <- if i = count - 1 then b else a + span * (float i / denom)

        arr

    let isMonotoneFiniteAxis (values: ReadOnlySpan<float>) =
        if values.Length < 2 then
            false
        elif not (Check3D.isFinite values[0] && Check3D.isFinite values[1]) then
            false
        else
            let asc = values[1] > values[0]
            let desc = values[1] < values[0]

            if not asc && not desc then
                false
            else
                let mutable ok = true

                for i = 1 to values.Length - 1 do
                    let v = values[i]

                    if not (Check3D.isFinite v) then
                        ok <- false
                    elif asc && v <= values[i - 1] then
                        ok <- false
                    elif desc && v >= values[i - 1] then
                        ok <- false

                ok

    let validateSpec (spec: SurfaceSpec) =
        if isNull (box spec) then
            nullArg (nameof spec)

        if isNull (box spec.Scale) then
            nullArg "spec.Scale"

        if isNull (box spec.Color) then
            nullArg "spec.Color"

        Check3D.finite32 "spec.Opacity" spec.Opacity

        if spec.Opacity < 0.0f || spec.Opacity > 1.0f then
            raise (ArgumentOutOfRangeException ("spec.Opacity", "Surface opacity must be within [0, 1]."))

        match spec.Shading with
        | ShadingMode.Unlit -> ()
        | ShadingMode.Lit (roughness, emissive) ->
            Check3D.nonNegative32 "spec.Shading.roughness" roughness
            Check3D.nonNegative32 "spec.Shading.emissive" emissive

    let shouldFlipGridWinding (scale: Scale3D) (isCurvilinear: bool) (xs: float array) (ys: float array) : bool =
        let sX = AxisCoordMapper.axisDir scale.XScale
        let sY = AxisCoordMapper.axisDir scale.YScale

        let gX =
            if isCurvilinear then 1.0
            elif xs[xs.Length - 1] < xs[0] then -1.0
            else 1.0

        let gY =
            if isCurvilinear then 1.0
            elif ys[ys.Length - 1] < ys[0] then -1.0
            else 1.0

        (sX * gX) * (sY * gY) < 0.0

    let inline normalizedToWorldFast (upAxis: UpAxis) (dx: float) (dy: float) (dz: float) (tx: float) (ty: float) (tz: float) : struct (bool * Vector3) =
        if Check3D.isFinite tx && Check3D.isFinite ty && Check3D.isFinite tz then
            let wx = float32 ((tx - 0.5) * dx)
            let wy = float32 ((ty - 0.5) * dy)
            let wz = float32 ((tz - 0.5) * dz)

            if Check3D.isFinite32 wx && Check3D.isFinite32 wy && Check3D.isFinite32 wz then
                match upAxis with
                | UpAxis.ZUp -> struct (true, Vector3 (wx, wz, -wy))
                | UpAxis.YUp -> struct (true, Vector3 (wx, wy, wz))
            else
                struct (false, Vector3.Zero)
        else
            struct (false, Vector3.Zero)

    let inline safeNormalizeCross (u: Vector3) (v: Vector3) (flip: bool) : Vector3 =
        let ux = float u.X
        let uy = float u.Y
        let uz = float u.Z
        let vx = float v.X
        let vy = float v.Y
        let vz = float v.Z
        let cx = uy * vz - uz * vy
        let cy = uz * vx - ux * vz
        let cz = ux * vy - uy * vx
        let lenSq = cx * cx + cy * cy + cz * cz

        if lenSq > 1e-28 && Check3D.isFinite lenSq then
            let inv = (if flip then -1.0 else 1.0) / sqrt lenSq
            Vector3 (float32 (cx * inv), float32 (cy * inv), float32 (cz * inv))
        else
            Vector3.UnitY

    let inline isBitSet (mask: uint64 array) (idx: int) =
        (mask[idx >>> 6] &&& (1UL <<< (idx &&& 63))) <> 0UL

    let inline setBit (mask: uint64 array) (idx: int) =
        let w = idx >>> 6
        mask[w] <- mask[w] ||| (1UL <<< (idx &&& 63))

    let inline isCellValid (cols: int) (mask: uint64 array) (c: int) (r: int) =
        let i00 = r * cols + c
        let i01 = i00 + cols
        isBitSet mask i00
        && isBitSet mask (i00 + 1)
        && isBitSet mask i01
        && isBitSet mask (i01 + 1)

    let fillRectilinearVertices
        (cols: int)
        (rows: int)
        (txBuf: float array)
        (tyBuf: float array)
        (zMapper: AxisCoordMapper)
        (upAxis: UpAxis)
        (dx: float)
        (dy: float)
        (dz: float)
        (ramp: ColorRamp)
        (opacity: float32)
        (heights: ReadOnlySpan<float>)
        (scalars: ReadOnlySpan<float>)
        (vertices: Vertex3D array)
        (mask: uint64 array)
        (maskWords: int)
        =
        for w = 0 to maskWords - 1 do
            mask[w] <- 0UL

        let useHeightsForColor = scalars.Length = 0

        for r = 0 to rows - 1 do
            let ty = tyBuf[r]
            let rowBase = r * cols

            for c = 0 to cols - 1 do
                let idx = rowBase + c
                let zVal = heights[idx]
                let tz = AxisCoordMapper.evalNormalized zMapper zVal
                let struct (posOk, worldPos) = normalizedToWorldFast upAxis dx dy dz txBuf[c] ty tz

                if posOk then
                    let s = if useHeightsForColor then zVal else scalars[idx]

                    match ColorRamp.tryMap ramp s with
                    | ValueSome col ->
                        setBit mask idx
                        vertices[idx] <- Vertex3D (worldPos, Vector3.UnitY, Vector4 (col.R, col.G, col.B, col.A * opacity))
                    | ValueNone ->
                        vertices[idx] <- Vertex3D (Vector3.Zero, Vector3.UnitY, Vector4.Zero)
                else
                    vertices[idx] <- Vertex3D (Vector3.Zero, Vector3.UnitY, Vector4.Zero)

    let fillCurvilinearVertices
        (count: int)
        (curviPoints: Point3 array)
        (xMapper: AxisCoordMapper)
        (yMapper: AxisCoordMapper)
        (zMapper: AxisCoordMapper)
        (upAxis: UpAxis)
        (dx: float)
        (dy: float)
        (dz: float)
        (ramp: ColorRamp)
        (opacity: float32)
        (heights: ReadOnlySpan<float>)
        (scalars: ReadOnlySpan<float>)
        (vertices: Vertex3D array)
        (mask: uint64 array)
        (maskWords: int)
        =
        for w = 0 to maskWords - 1 do
            mask[w] <- 0UL

        let hasHeightOverride = heights.Length > 0
        let useZForColor = scalars.Length = 0

        for idx = 0 to count - 1 do
            let pt = curviPoints[idx]
            let zVal = if hasHeightOverride then heights[idx] else pt.Z
            let tx = AxisCoordMapper.evalNormalized xMapper pt.X
            let ty = AxisCoordMapper.evalNormalized yMapper pt.Y
            let tz = AxisCoordMapper.evalNormalized zMapper zVal
            let struct (posOk, worldPos) = normalizedToWorldFast upAxis dx dy dz tx ty tz

            if posOk then
                let s = if useZForColor then zVal else scalars[idx]

                match ColorRamp.tryMap ramp s with
                | ValueSome col ->
                    setBit mask idx
                    vertices[idx] <- Vertex3D (worldPos, Vector3.UnitY, Vector4 (col.R, col.G, col.B, col.A * opacity))
                | ValueNone ->
                    vertices[idx] <- Vertex3D (Vector3.Zero, Vector3.UnitY, Vector4.Zero)
            else
                vertices[idx] <- Vertex3D (Vector3.Zero, Vector3.UnitY, Vector4.Zero)

    let computeGridNormals (cols: int) (rows: int) (flipWinding: bool) (vertices: Vertex3D array) (mask: uint64 array) =
        for r = 0 to rows - 1 do
            let rowBase = r * cols

            for c = 0 to cols - 1 do
                let idx = rowBase + c

                if isBitSet mask idx then
                    let p = vertices[idx].Position
                    let hasLeft = c > 0 && isBitSet mask (idx - 1)
                    let hasRight = c < cols - 1 && isBitSet mask (idx + 1)
                    let hasDown = r > 0 && isBitSet mask (idx - cols)
                    let hasUp = r < rows - 1 && isBitSet mask (idx + cols)

                    if (hasLeft || hasRight) && (hasDown || hasUp) then
                        let tangentU =
                            if hasLeft && hasRight then
                                vertices[idx + 1].Position - vertices[idx - 1].Position
                            elif hasRight then
                                vertices[idx + 1].Position - p
                            else
                                p - vertices[idx - 1].Position

                        let tangentV =
                            if hasDown && hasUp then
                                vertices[idx + cols].Position - vertices[idx - cols].Position
                            elif hasUp then
                                vertices[idx + cols].Position - p
                            else
                                p - vertices[idx - cols].Position

                        vertices[idx].Normal <- safeNormalizeCross tangentU tangentV flipWinding
                    else
                        vertices[idx].Normal <- Vector3.UnitY

    let buildActiveIndices (cols: int) (rows: int) (flipWinding: bool) (mask: uint64 array) (indices: uint32 array) : int =
        let mutable outIdx = 0

        for r = 0 to rows - 2 do
            let row0 = r * cols
            let row1 = row0 + cols

            for c = 0 to cols - 2 do
                let i00 = row0 + c
                let i10 = i00 + 1
                let i01 = row1 + c
                let i11 = i01 + 1

                if
                    isBitSet mask i00
                    && isBitSet mask i10
                    && isBitSet mask i01
                    && isBitSet mask i11
                then
                    let v00 = uint32 i00
                    let v10 = uint32 i10
                    let v11 = uint32 i11
                    let v01 = uint32 i01

                    if not flipWinding then
                        indices[outIdx] <- v00
                        indices[outIdx + 1] <- v10
                        indices[outIdx + 2] <- v11
                        indices[outIdx + 3] <- v00
                        indices[outIdx + 4] <- v11
                        indices[outIdx + 5] <- v01
                    else
                        indices[outIdx] <- v00
                        indices[outIdx + 1] <- v11
                        indices[outIdx + 2] <- v10
                        indices[outIdx + 3] <- v00
                        indices[outIdx + 4] <- v01
                        indices[outIdx + 5] <- v11

                    outIdx <- outIdx + 6

        outIdx

    let buildActiveWireframe
        (cols: int)
        (rows: int)
        (wireColor: RgbaColor)
        (vertices: Vertex3D array)
        (mask: uint64 array)
        (lines: LineVertex3D array)
        : int =
        let colorVec = wireColor.ToVector4 ()
        let mutable outIdx = 0

        for r = 0 to rows - 1 do
            let rowBase = r * cols

            for c = 0 to cols - 2 do
                let active =
                    (r > 0 && isCellValid cols mask c (r - 1))
                    || (r < rows - 1 && isCellValid cols mask c r)

                if active then
                    let idxA = rowBase + c
                    let idxB = idxA + 1
                    lines[outIdx] <- LineVertex3D (vertices[idxA].Position, colorVec)
                    lines[outIdx + 1] <- LineVertex3D (vertices[idxB].Position, colorVec)
                    outIdx <- outIdx + 2

        for r = 0 to rows - 2 do
            let rowBase = r * cols

            for c = 0 to cols - 1 do
                let active =
                    (c > 0 && isCellValid cols mask (c - 1) r)
                    || (c < cols - 1 && isCellValid cols mask c r)

                if active then
                    let idxA = rowBase + c
                    let idxB = idxA + cols
                    lines[outIdx] <- LineVertex3D (vertices[idxA].Position, colorVec)
                    lines[outIdx + 1] <- LineVertex3D (vertices[idxB].Position, colorVec)
                    outIdx <- outIdx + 2

        outIdx

    let buildInstance (spec: SurfaceSpec) : Instance3D =
        let lutFlag = if spec.Color.IsScalarPassthrough then InstanceFlags.UseColorLut else InstanceFlags.None

        match spec.Shading with
        | ShadingMode.Unlit ->
            Instance3D.fromMatrix Matrix4x4.Identity RgbaColor.white 0.5f spec.PickId (InstanceFlags.Unlit ||| lutFlag) 0.0f
        | ShadingMode.Lit (r, e) ->
            Instance3D.fromMatrix Matrix4x4.Identity RgbaColor.white r spec.PickId lutFlag e

/// <summary>Validating constructors and dimension queries for <c>SurfaceGrid</c>.</summary>
[<RequireQualifiedAccess>]
module SurfaceGrid =
    /// <summary>Creates a uniformly spaced height grid over <c>xDomain</c> and <c>yDomain</c>; non-finite height/scalar samples mark omitted vertices unless dimensions or lengths are invalid.</summary>
    let uniform
        (columns: int)
        (rows: int)
        (xDomain: float * float)
        (yDomain: float * float)
        (heights: ReadOnlySpan<float>)
        (scalars: ReadOnlySpan<float>)
        : Result<SurfaceGrid, LayoutError list> =
        match SurfaceKernel.validateGridDims columns rows with
        | Error errs -> Error errs
        | Ok count ->
            if not (SurfaceKernel.isFiniteDomain xDomain && SurfaceKernel.isFiniteDomain yDomain) then
                Error [ LayoutError.InvalidObservation "Surface grid domain endpoints and spans must be finite." ]
            elif heights.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected {count} height samples for {columns}x{rows} grid, got {heights.Length}." ]
            elif scalars.Length <> 0 && scalars.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected 0 or {count} scalar samples for {columns}x{rows} grid, got {scalars.Length}." ]
            else
                Ok
                    {
                        Columns = columns
                        Rows = rows
                        IsCurvilinear = false
                        Xs = SurfaceKernel.sampleLinearAxis columns xDomain
                        Ys = SurfaceKernel.sampleLinearAxis rows yDomain
                        Heights = SurfaceKernel.copySpan heights
                        Scalars = SurfaceKernel.copySpan scalars
                        CurviPoints = [||]
                    }

    /// <summary>Evaluates a uniform grid from analytical height and optional 4th-dimension scalar functions.</summary>
    let fromFunction
        (columns: int)
        (rows: int)
        (xDomain: float * float)
        (yDomain: float * float)
        (evalHeight: float -> float -> float)
        (evalScalar: (float -> float -> float) voption)
        : Result<SurfaceGrid, LayoutError list> =
        if isNull (box evalHeight) then
            nullArg (nameof evalHeight)

        match evalScalar with
        | ValueSome f when isNull (box f) -> nullArg (nameof evalScalar)
        | _ -> ()

        match SurfaceKernel.validateGridDims columns rows with
        | Error errs -> Error errs
        | Ok count ->
            if not (SurfaceKernel.isFiniteDomain xDomain && SurfaceKernel.isFiniteDomain yDomain) then
                Error [ LayoutError.InvalidObservation "Surface grid domain endpoints and spans must be finite." ]
            else
                let xs = SurfaceKernel.sampleLinearAxis columns xDomain
                let ys = SurfaceKernel.sampleLinearAxis rows yDomain
                let heights = Array.zeroCreate<float> count

                let scalars =
                    match evalScalar with
                    | ValueSome _ -> Array.zeroCreate<float> count
                    | ValueNone -> [||]

                for r = 0 to rows - 1 do
                    let y = ys[r]
                    let rowBase = r * columns

                    for c = 0 to columns - 1 do
                        let x = xs[c]
                        let idx = rowBase + c
                        heights[idx] <- evalHeight x y

                        match evalScalar with
                        | ValueSome f -> scalars[idx] <- f x y
                        | ValueNone -> ()

                Ok
                    {
                        Columns = columns
                        Rows = rows
                        IsCurvilinear = false
                        Xs = xs
                        Ys = ys
                        Heights = heights
                        Scalars = scalars
                        CurviPoints = [||]
                    }

    /// <summary>Creates a rectilinear grid with explicit strictly monotone finite <c>xs</c> and <c>ys</c> axis coordinate arrays.</summary>
    let rectilinear
        (xs: ReadOnlySpan<float>)
        (ys: ReadOnlySpan<float>)
        (heights: ReadOnlySpan<float>)
        (scalars: ReadOnlySpan<float>)
        : Result<SurfaceGrid, LayoutError list> =
        match SurfaceKernel.validateGridDims xs.Length ys.Length with
        | Error errs -> Error errs
        | Ok count ->
            if not (SurfaceKernel.isMonotoneFiniteAxis xs && SurfaceKernel.isMonotoneFiniteAxis ys) then
                Error [ LayoutError.InvalidObservation "Rectilinear grid axis coordinates must be finite and strictly monotone." ]
            elif heights.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected {count} height samples for {xs.Length}x{ys.Length} grid, got {heights.Length}." ]
            elif scalars.Length <> 0 && scalars.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected 0 or {count} scalar samples for {xs.Length}x{ys.Length} grid, got {scalars.Length}." ]
            else
                Ok
                    {
                        Columns = xs.Length
                        Rows = ys.Length
                        IsCurvilinear = false
                        Xs = SurfaceKernel.copySpan xs
                        Ys = SurfaceKernel.copySpan ys
                        Heights = SurfaceKernel.copySpan heights
                        Scalars = SurfaceKernel.copySpan scalars
                        CurviPoints = [||]
                    }

    /// <summary>Creates a structured curvilinear surface grid from <c>columns * rows</c> 3D points and optional 4th-dimension scalars.</summary>
    let curvilinear
        (columns: int)
        (rows: int)
        (points: ReadOnlySpan<Point3>)
        (scalars: ReadOnlySpan<float>)
        : Result<SurfaceGrid, LayoutError list> =
        match SurfaceKernel.validateGridDims columns rows with
        | Error errs -> Error errs
        | Ok count ->
            if points.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected {count} points for {columns}x{rows} curvilinear grid, got {points.Length}." ]
            elif scalars.Length <> 0 && scalars.Length <> count then
                Error [ LayoutError.InvalidObservation $"Expected 0 or {count} scalar samples for {columns}x{rows} curvilinear grid, got {scalars.Length}." ]
            else
                Ok
                    {
                        Columns = columns
                        Rows = rows
                        IsCurvilinear = true
                        Xs = [||]
                        Ys = [||]
                        Heights = [||]
                        Scalars = SurfaceKernel.copySpan scalars
                        CurviPoints = SurfaceKernel.copySpan points
                    }

    /// <summary>Returns the grid column count.</summary>
    let columns (grid: SurfaceGrid) : int = grid.Columns

    /// <summary>Returns the grid row count.</summary>
    let rows (grid: SurfaceGrid) : int = grid.Rows

/// <summary>Static structured-grid and Delaunay 3D surface mesh builders with smooth finite-difference normals and hole omission.</summary>
[<RequireQualifiedAccess>]
module Surface3D =
    /// <summary>Tessellates a structured <c>SurfaceGrid</c> into a <c>SurfaceMesh</c> with finite-difference world normals, hole omission, and optional wireframe.</summary>
    let build (spec: SurfaceSpec) (grid: SurfaceGrid) : SurfaceMesh =
        SurfaceKernel.validateSpec spec

        if isNull (box grid) then
            nullArg (nameof grid)

        let cols = grid.Columns
        let rows = grid.Rows
        let vertexCount = cols * rows
        let maxIndices = (cols - 1) * (rows - 1) * 6
        let maskWords = (vertexCount + 63) >>> 6

        let vertices = Array.zeroCreate<Vertex3D> vertexCount
        let mask = Array.zeroCreate<uint64> maskWords
        let indicesScratch = Array.zeroCreate<uint32> maxIndices

        let struct (dx, dy, dz) = spec.Scale.DataExtents
        let upAxis = spec.Scale.UpAxis
        let xMapper = AxisCoordMapper.classify spec.Scale.XScale
        let yMapper = AxisCoordMapper.classify spec.Scale.YScale
        let zMapper = AxisCoordMapper.classify spec.Scale.ZScale
        let flipWinding = SurfaceKernel.shouldFlipGridWinding spec.Scale grid.IsCurvilinear grid.Xs grid.Ys

        if grid.IsCurvilinear then
            SurfaceKernel.fillCurvilinearVertices
                vertexCount
                grid.CurviPoints
                xMapper
                yMapper
                zMapper
                upAxis
                dx
                dy
                dz
                spec.Color
                spec.Opacity
                ReadOnlySpan<float>.Empty
                (ReadOnlySpan<float>(grid.Scalars))
                vertices
                mask
                maskWords
        else
            let txBuf = Array.zeroCreate<float> cols
            let tyBuf = Array.zeroCreate<float> rows
            AxisCoordMapper.mapArray xMapper grid.Xs txBuf
            AxisCoordMapper.mapArray yMapper grid.Ys tyBuf

            SurfaceKernel.fillRectilinearVertices
                cols
                rows
                txBuf
                tyBuf
                zMapper
                upAxis
                dx
                dy
                dz
                spec.Color
                spec.Opacity
                (ReadOnlySpan<float>(grid.Heights))
                (ReadOnlySpan<float>(grid.Scalars))
                vertices
                mask
                maskWords

        SurfaceKernel.computeGridNormals cols rows flipWinding vertices mask
        let activeIndices = SurfaceKernel.buildActiveIndices cols rows flipWinding mask indicesScratch

        let indices =
            if activeIndices = maxIndices then
                indicesScratch
            elif activeIndices = 0 then
                [||]
            else
                Array.sub indicesScratch 0 activeIndices

        let wireframe =
            match spec.Wireframe with
            | ValueNone -> [||]
            | ValueSome wireColor ->
                let maxLines = ((cols - 1) * rows + cols * (rows - 1)) * 2
                let lineScratch = Array.zeroCreate<LineVertex3D> maxLines
                let activeLines = SurfaceKernel.buildActiveWireframe cols rows wireColor vertices mask lineScratch

                if activeLines = maxLines then
                    lineScratch
                elif activeLines = 0 then
                    [||]
                else
                    Array.sub lineScratch 0 activeLines

        {
            VerticesArr = vertices
            IndicesArr = indices
            WireframeArr = wireframe
        }

    /// <summary>Triangulates scattered 3D points via 2D <c>Ranvier.D5.Delaunay</c> over <c>(X, Y)</c> lifted by <c>Z</c> with area-weighted smooth normals.</summary>
    let triangulatePoints (spec: SurfaceSpec) (points: IReadOnlyList<Point3>) (scalars: ReadOnlySpan<float>) : Result<SurfaceMesh, LayoutError list> =
        SurfaceKernel.validateSpec spec

        if isNull (box points) then
            nullArg (nameof points)

        let count = points.Count

        if count < 3 then
            Error [ LayoutError.InvalidObservation "At least three points are required for surface triangulation." ]
        elif scalars.Length <> 0 && scalars.Length <> count then
            Error [ LayoutError.InvalidObservation $"Expected 0 or {count} scalar samples, got {scalars.Length}." ]
        else
            let mutable allFinite = true
            let pts2D = ResizeArray<Point>(count)

            for i = 0 to count - 1 do
                let p = points[i]

                if not (Check3D.isFinite p.X && Check3D.isFinite p.Y && Check3D.isFinite p.Z) then
                    allFinite <- false
                else
                    pts2D.Add { X = p.X; Y = p.Y }

            if not allFinite then
                Error [ LayoutError.InvalidObservation "Point coordinates must be finite." ]
            else
                let tri = Delaunay.build (Seq.toList pts2D)
                let rawTris = Delaunay.triangles tri

                if rawTris.Length = 0 then
                    Error [ LayoutError.InvalidTopology "Points are collinear or yield fewer than three unique (X, Y) sites." ]
                else
                    let sites = Delaunay.sites tri
                    let siteCount = sites.Length
                    let vertices = Array.zeroCreate<Vertex3D> siteCount
                    let valid = Array.zeroCreate<bool> siteCount
                    let accumX = Array.zeroCreate<float> siteCount
                    let accumY = Array.zeroCreate<float> siteCount
                    let accumZ = Array.zeroCreate<float> siteCount

                    let struct (dx, dy, dz) = spec.Scale.DataExtents
                    let upAxis = spec.Scale.UpAxis
                    let xMapper = AxisCoordMapper.classify spec.Scale.XScale
                    let yMapper = AxisCoordMapper.classify spec.Scale.YScale
                    let zMapper = AxisCoordMapper.classify spec.Scale.ZScale
                    let flipWinding = (AxisCoordMapper.axisDir spec.Scale.XScale * AxisCoordMapper.axisDir spec.Scale.YScale) < 0.0
                    let useZForColor = scalars.Length = 0

                    for s = 0 to siteCount - 1 do
                        let origIdx = (Delaunay.inputIndices s tri)[0]
                        let pt = points[origIdx]
                        let tx = AxisCoordMapper.evalNormalized xMapper pt.X
                        let ty = AxisCoordMapper.evalNormalized yMapper pt.Y
                        let tz = AxisCoordMapper.evalNormalized zMapper pt.Z
                        let struct (posOk, worldPos) = SurfaceKernel.normalizedToWorldFast upAxis dx dy dz tx ty tz

                        if posOk then
                            let scalar = if useZForColor then pt.Z else scalars[origIdx]

                            match ColorRamp.tryMap spec.Color scalar with
                            | ValueSome col ->
                                valid[s] <- true
                                vertices[s] <- Vertex3D (worldPos, Vector3.UnitY, Vector4 (col.R, col.G, col.B, col.A * spec.Opacity))
                            | ValueNone ->
                                vertices[s] <- Vertex3D (Vector3.Zero, Vector3.UnitY, Vector4.Zero)
                        else
                            vertices[s] <- Vertex3D (Vector3.Zero, Vector3.UnitY, Vector4.Zero)

                    let triCount = rawTris.Length / 3
                    let activeIndices = ResizeArray<uint32>(rawTris.Length)
                    let edgeSet = if spec.Wireframe.IsSome then Some (HashSet<uint64>()) else None

                    let addEdge (lines: ResizeArray<LineVertex3D>) (seen: HashSet<uint64>) (colorVec: Vector4) (a: int) (b: int) =
                        let lo = uint32 (min a b)
                        let hi = uint32 (max a b)
                        let key = (uint64 lo <<< 32) ||| uint64 hi

                        if seen.Add key then
                            lines.Add (LineVertex3D (vertices[a].Position, colorVec))
                            lines.Add (LineVertex3D (vertices[b].Position, colorVec))

                    let wireLines = if spec.Wireframe.IsSome then Some (ResizeArray<LineVertex3D>(rawTris.Length * 2)) else None

                    for t = 0 to triCount - 1 do
                        let bIdx = t * 3
                        let i0 = rawTris[bIdx]
                        let i1 = if flipWinding then rawTris[bIdx + 2] else rawTris[bIdx + 1]
                        let i2 = if flipWinding then rawTris[bIdx + 1] else rawTris[bIdx + 2]

                        if valid[i0] && valid[i1] && valid[i2] then
                            activeIndices.Add (uint32 i0)
                            activeIndices.Add (uint32 i1)
                            activeIndices.Add (uint32 i2)

                            let p0 = vertices[i0].Position
                            let p1 = vertices[i1].Position
                            let p2 = vertices[i2].Position
                            let ux = float (p1.X - p0.X)
                            let uy = float (p1.Y - p0.Y)
                            let uz = float (p1.Z - p0.Z)
                            let vx = float (p2.X - p0.X)
                            let vy = float (p2.Y - p0.Y)
                            let vz = float (p2.Z - p0.Z)
                            let cx = uy * vz - uz * vy
                            let cy = uz * vx - ux * vz
                            let cz = ux * vy - uy * vx

                            accumX[i0] <- accumX[i0] + cx
                            accumY[i0] <- accumY[i0] + cy
                            accumZ[i0] <- accumZ[i0] + cz
                            accumX[i1] <- accumX[i1] + cx
                            accumY[i1] <- accumY[i1] + cy
                            accumZ[i1] <- accumZ[i1] + cz
                            accumX[i2] <- accumX[i2] + cx
                            accumY[i2] <- accumY[i2] + cy
                            accumZ[i2] <- accumZ[i2] + cz

                            match struct (spec.Wireframe, wireLines, edgeSet) with
                            | struct (ValueSome wc, Some lines, Some seen) ->
                                let cVec = wc.ToVector4 ()
                                addEdge lines seen cVec i0 i1
                                addEdge lines seen cVec i1 i2
                                addEdge lines seen cVec i2 i0
                            | _ -> ()

                    for s = 0 to siteCount - 1 do
                        if valid[s] then
                            let nx = accumX[s]
                            let ny = accumY[s]
                            let nz = accumZ[s]
                            let lenSq = nx * nx + ny * ny + nz * nz

                            if lenSq > 1e-28 && Check3D.isFinite lenSq then
                                let inv = 1.0 / sqrt lenSq
                                vertices[s].Normal <- Vector3 (float32 (nx * inv), float32 (ny * inv), float32 (nz * inv))

                    let wireArr =
                        match wireLines with
                        | Some lines when lines.Count > 0 -> lines.ToArray ()
                        | _ -> [||]

                    Ok
                        {
                            VerticesArr = vertices
                            IndicesArr = activeIndices.ToArray ()
                            WireframeArr = wireArr
                        }

/// <summary>Dynamic deforming GPU surface batch reusing vertex/index buffers and skipping index uploads when the hole mask is unchanged.</summary>
[<Sealed>]
type SurfaceBatch private
    (
        context: WgpuContext,
        meshHandle: MeshHandle,
        initOpaqueBatchOpt: InstanceBatch option,
        initAlphaBatchOpt: InstanceBatch option,
        initActiveBatch: InstanceBatch,
        initActiveIsAlpha: bool,
        initWireframeBatch: LineBatch option,
        initCols: int,
        initRows: int,
        initCurvilinear: bool,
        initXs: float array,
        initYs: float array,
        initCurviPoints: Point3 array,
        initVertices: Vertex3D array,
        initCurrMask: uint64 array,
        initNextMask: uint64 array,
        initIndices: uint32 array,
        initLines: LineVertex3D array,
        initTxBuf: float array,
        initTyBuf: float array,
        initScale: Scale3D,
        initXMapper: AxisCoordMapper,
        initYMapper: AxisCoordMapper,
        initZMapper: AxisCoordMapper,
        initFlipWinding: bool,
        initShading: ShadingMode,
        initPickId: uint32,
        initUseColorLut: bool,
        initHasWireframe: bool
    ) =
    let mutable disposed = false
    let mutable opaqueBatchOpt = initOpaqueBatchOpt
    let mutable alphaBatchOpt = initAlphaBatchOpt
    let mutable activeBatch = initActiveBatch
    let mutable activeIsAlpha = initActiveIsAlpha
    let mutable wireframeBatch = initWireframeBatch
    let mutable cols = initCols
    let mutable rows = initRows
    let mutable vertexCount = initCols * initRows
    let mutable maxIndices = (initCols - 1) * (initRows - 1) * 6
    let mutable maxLineVertices = ((initCols - 1) * initRows + initCols * (initRows - 1)) * 2
    let mutable activeMaskWords = (initCols * initRows + 63) >>> 6
    let mutable isCurvilinear = initCurvilinear
    let mutable xs = initXs
    let mutable ys = initYs
    let mutable curviPoints = initCurviPoints
    let mutable scratchVertices = initVertices
    let mutable currMask = initCurrMask
    let mutable nextMask = initNextMask
    let mutable scratchIndices = initIndices
    let mutable scratchLines = initLines
    let mutable txBuf = initTxBuf
    let mutable tyBuf = initTyBuf
    let mutable lastScale = initScale
    let mutable xMapper = initXMapper
    let mutable yMapper = initYMapper
    let mutable zMapper = initZMapper
    let mutable flipWinding = initFlipWinding
    let mutable lastShading = initShading
    let mutable lastPickId = initPickId
    let mutable lastUseColorLut = initUseColorLut
    let mutable lastHasWireframe = initHasWireframe

    let checkAlive () =
        if disposed then
            raise (ObjectDisposedException (nameof SurfaceBatch))

    let refreshScaleCaches (spec: SurfaceSpec) (forceAxisMap: bool) : bool =
        let scaleChanged = not (Object.ReferenceEquals (spec.Scale, lastScale))

        if scaleChanged || forceAxisMap then
            lastScale <- spec.Scale
            xMapper <- AxisCoordMapper.classify spec.Scale.XScale
            yMapper <- AxisCoordMapper.classify spec.Scale.YScale
            zMapper <- AxisCoordMapper.classify spec.Scale.ZScale

            if not isCurvilinear then
                AxisCoordMapper.mapArray xMapper xs txBuf
                AxisCoordMapper.mapArray yMapper ys tyBuf

            let newFlip = SurfaceKernel.shouldFlipGridWinding spec.Scale isCurvilinear xs ys
            let flipChanged = newFlip <> flipWinding
            flipWinding <- newFlip
            flipChanged
        else
            false

    let applyData (spec: SurfaceSpec) (heights: ReadOnlySpan<float>) (scalars: ReadOnlySpan<float>) (forceRebuildTopology: bool) =
        let struct (dx, dy, dz) = spec.Scale.DataExtents
        let upAxis = spec.Scale.UpAxis

        if isCurvilinear then
            SurfaceKernel.fillCurvilinearVertices
                vertexCount
                curviPoints
                xMapper
                yMapper
                zMapper
                upAxis
                dx
                dy
                dz
                spec.Color
                spec.Opacity
                heights
                scalars
                scratchVertices
                nextMask
                activeMaskWords
        else
            SurfaceKernel.fillRectilinearVertices
                cols
                rows
                txBuf
                tyBuf
                zMapper
                upAxis
                dx
                dy
                dz
                spec.Color
                spec.Opacity
                heights
                scalars
                scratchVertices
                nextMask
                activeMaskWords

        SurfaceKernel.computeGridNormals cols rows flipWinding scratchVertices nextMask

        let mutable maskChanged = forceRebuildTopology

        for w = 0 to activeMaskWords - 1 do
            if nextMask[w] <> currMask[w] then
                maskChanged <- true
                currMask[w] <- nextMask[w]

        if not maskChanged then
            meshHandle.UpdateVertices (ReadOnlySpan<Vertex3D>(scratchVertices))
        else
            let activeIndexCount = SurfaceKernel.buildActiveIndices cols rows flipWinding currMask scratchIndices

            let activeIndices =
                if activeIndexCount = scratchIndices.Length then
                    scratchIndices
                elif activeIndexCount = 0 then
                    [||]
                else
                    Array.sub scratchIndices 0 activeIndexCount

            meshHandle.UpdateMesh
                {
                    Vertices = scratchVertices
                    Indices = activeIndices
                }

        match spec.Wireframe with
        | ValueSome wireColor ->
            if scratchLines.Length < maxLineVertices then
                scratchLines <- Array.zeroCreate<LineVertex3D> maxLineVertices

            let lb =
                match wireframeBatch with
                | Some existing -> existing
                | None ->
                    let created = context.CreateLineBatch ()
                    wireframeBatch <- Some created
                    created

            let activeLineCount =
                SurfaceKernel.buildActiveWireframe cols rows wireColor scratchVertices currMask scratchLines

            if activeLineCount = scratchLines.Length then
                lb.UpdateVertices (ReadOnlySpan<LineVertex3D>(scratchLines))
            elif activeLineCount = 0 then
                lb.Resize 0
            else
#if FABLE_COMPILER
                scratchLines <- Array.sub scratchLines 0 activeLineCount
                lb.UpdateVertices (ReadOnlySpan<LineVertex3D>(scratchLines))
#else
                lb.UpdateVertices (ReadOnlySpan<LineVertex3D>(scratchLines, 0, activeLineCount))
#endif

            lastHasWireframe <- true
        | ValueNone ->
            match wireframeBatch with
            | Some lb when lastHasWireframe ->
                lb.Resize 0
                lastHasWireframe <- false
            | _ -> ()

        let wantsAlpha = spec.Opacity < 0.999f
        let useColorLut = spec.Color.IsScalarPassthrough
        let instanceChanged = spec.Shading <> lastShading || spec.PickId <> lastPickId || useColorLut <> lastUseColorLut

        if wantsAlpha <> activeIsAlpha then
            activeBatch.Resize 0

            let nextBatch =
                if wantsAlpha then
                    match alphaBatchOpt with
                    | Some b -> b
                    | None ->
                        let b = context.CreateBatch (meshHandle, BlendMode.AlphaBlend, true)
                        alphaBatchOpt <- Some b
                        b
                else
                    match opaqueBatchOpt with
                    | Some b -> b
                    | None ->
                        let b = context.CreateBatch (meshHandle, BlendMode.Opaque, true)
                        opaqueBatchOpt <- Some b
                        b

            lastShading <- spec.Shading
            lastPickId <- spec.PickId
            lastUseColorLut <- useColorLut
            nextBatch.SetSingle (SurfaceKernel.buildInstance spec)
            activeBatch <- nextBatch
            activeIsAlpha <- wantsAlpha
        elif instanceChanged then
            lastShading <- spec.Shading
            lastPickId <- spec.PickId
            lastUseColorLut <- useColorLut
            activeBatch.SetSingle (SurfaceKernel.buildInstance spec)

    /// <summary>Uploads an initial <c>SurfaceGrid</c> and creates the GPU <c>InstanceBatch</c> and optional wireframe <c>LineBatch</c>.</summary>
    static member Create(context: WgpuContext, spec: SurfaceSpec, initialGrid: SurfaceGrid) : SurfaceBatch =
        if isNull (box context) then
            nullArg (nameof context)

        SurfaceKernel.validateSpec spec

        if isNull (box initialGrid) then
            nullArg (nameof initialGrid)

        let cols = initialGrid.Columns
        let rows = initialGrid.Rows
        let vertexCount = cols * rows
        let maxIndices = (cols - 1) * (rows - 1) * 6
        let maxLines = ((cols - 1) * rows + cols * (rows - 1)) * 2
        let maskWords = (vertexCount + 63) >>> 6

        let vertices = Array.zeroCreate<Vertex3D> vertexCount
        let currMask = Array.zeroCreate<uint64> maskWords
        let nextMask = Array.zeroCreate<uint64> maskWords
        let indicesScratch = Array.zeroCreate<uint32> maxIndices
        let mutable lineScratch = if spec.Wireframe.IsSome then Array.zeroCreate<LineVertex3D> maxLines else [||]

        let xMapper = AxisCoordMapper.classify spec.Scale.XScale
        let yMapper = AxisCoordMapper.classify spec.Scale.YScale
        let zMapper = AxisCoordMapper.classify spec.Scale.ZScale
        let flipWinding = SurfaceKernel.shouldFlipGridWinding spec.Scale initialGrid.IsCurvilinear initialGrid.Xs initialGrid.Ys

        let txBuf, tyBuf =
            if initialGrid.IsCurvilinear then
                [||], [||]
            else
                let tx = Array.zeroCreate<float> cols
                let ty = Array.zeroCreate<float> rows
                AxisCoordMapper.mapArray xMapper initialGrid.Xs tx
                AxisCoordMapper.mapArray yMapper initialGrid.Ys ty
                tx, ty

        let struct (dx, dy, dz) = spec.Scale.DataExtents

        if initialGrid.IsCurvilinear then
            SurfaceKernel.fillCurvilinearVertices
                vertexCount
                initialGrid.CurviPoints
                xMapper
                yMapper
                zMapper
                spec.Scale.UpAxis
                dx
                dy
                dz
                spec.Color
                spec.Opacity
                ReadOnlySpan<float>.Empty
                (ReadOnlySpan<float>(initialGrid.Scalars))
                vertices
                currMask
                maskWords
        else
            SurfaceKernel.fillRectilinearVertices
                cols
                rows
                txBuf
                tyBuf
                zMapper
                spec.Scale.UpAxis
                dx
                dy
                dz
                spec.Color
                spec.Opacity
                (ReadOnlySpan<float>(initialGrid.Heights))
                (ReadOnlySpan<float>(initialGrid.Scalars))
                vertices
                currMask
                maskWords

        SurfaceKernel.computeGridNormals cols rows flipWinding vertices currMask
        let activeIndexCount = SurfaceKernel.buildActiveIndices cols rows flipWinding currMask indicesScratch

        let initialIndices =
            if activeIndexCount = maxIndices then
                indicesScratch
            elif activeIndexCount = 0 then
                [||]
            else
                Array.sub indicesScratch 0 activeIndexCount

        let meshHandle =
            context.UploadMesh
                {
                    Vertices = vertices
                    Indices = initialIndices
                }

        let isAlpha = spec.Opacity < 0.999f

        let blendMode =
            if isAlpha then
                BlendMode.AlphaBlend
            else
                BlendMode.Opaque

        let initialBatch = context.CreateBatch (meshHandle, blendMode, true)
        initialBatch.SetSingle (SurfaceKernel.buildInstance spec)
        let opaqueOpt = if isAlpha then None else Some initialBatch
        let alphaOpt = if isAlpha then Some initialBatch else None

        let wireframeBatch, hasWireframe =
            match spec.Wireframe with
            | ValueSome wireColor ->
                let lb = context.CreateLineBatch ()
                let activeLines = SurfaceKernel.buildActiveWireframe cols rows wireColor vertices currMask lineScratch

                if activeLines = lineScratch.Length then
                    lb.UpdateVertices (ReadOnlySpan<LineVertex3D>(lineScratch))
                elif activeLines > 0 then
#if FABLE_COMPILER
                    lineScratch <- Array.sub lineScratch 0 activeLines
                    lb.UpdateVertices (ReadOnlySpan<LineVertex3D>(lineScratch))
#else
                    lb.UpdateVertices (ReadOnlySpan<LineVertex3D>(lineScratch, 0, activeLines))
#endif

                Some lb, activeLines > 0
            | ValueNone -> None, false

        new SurfaceBatch (
            context,
            meshHandle,
            opaqueOpt,
            alphaOpt,
            initialBatch,
            isAlpha,
            wireframeBatch,
            cols,
            rows,
            initialGrid.IsCurvilinear,
            initialGrid.Xs,
            initialGrid.Ys,
            initialGrid.CurviPoints,
            vertices,
            currMask,
            nextMask,
            indicesScratch,
            lineScratch,
            txBuf,
            tyBuf,
            spec.Scale,
            xMapper,
            yMapper,
            zMapper,
            flipWinding,
            spec.Shading,
            spec.PickId,
            spec.Color.IsScalarPassthrough,
            hasWireframe
        )

    /// <summary>The underlying double-sided GPU <c>InstanceBatch</c> rendering the surface mesh.</summary>
    member _.Batch: InstanceBatch = activeBatch

    /// <summary>Optional <c>LineBatch</c> rendering surface grid wireframe segments when enabled at creation or via <c>Update</c>/<c>UpdateGrid</c>.</summary>
    member _.WireframeBatch: LineBatch option = wireframeBatch

    /// <summary>Updates heights and optional 4th-dimension scalars in place with zero heap allocations when the valid-vertex hole mask is unchanged.</summary>
    member _.Update(spec: SurfaceSpec, heights: ReadOnlySpan<float>, scalars: ReadOnlySpan<float>) : unit =
        checkAlive ()
        SurfaceKernel.validateSpec spec

        if heights.Length <> vertexCount then
            invalidArg (nameof heights) $"Expected {vertexCount} height samples for {cols}x{rows} surface, got {heights.Length}."

        if scalars.Length <> 0 && scalars.Length <> vertexCount then
            invalidArg (nameof scalars) $"Expected 0 or {vertexCount} scalar samples for {cols}x{rows} surface, got {scalars.Length}."

        let flipChanged = refreshScaleCaches spec false
        applyData spec heights scalars flipChanged

    /// <summary>Updates the surface from a complete <c>SurfaceGrid</c>, resizing scratch and index buffers when grid dimensions change.</summary>
    member _.UpdateGrid(spec: SurfaceSpec, grid: SurfaceGrid) : unit =
        checkAlive ()
        SurfaceKernel.validateSpec spec

        if isNull (box grid) then
            nullArg (nameof grid)

        let newCols = grid.Columns
        let newRows = grid.Rows
        let newVertexCount = newCols * newRows
        let newMaxIndices = (newCols - 1) * (newRows - 1) * 6
        let newMaxLines = ((newCols - 1) * newRows + newCols * (newRows - 1)) * 2
        let newMaskWords = (newVertexCount + 63) >>> 6

        let topologyChanged =
            newCols <> cols
            || newRows <> rows
            || grid.IsCurvilinear <> isCurvilinear

        cols <- newCols
        rows <- newRows
        vertexCount <- newVertexCount
        maxIndices <- newMaxIndices
        maxLineVertices <- newMaxLines
        activeMaskWords <- newMaskWords
        isCurvilinear <- grid.IsCurvilinear
        xs <- grid.Xs
        ys <- grid.Ys
        curviPoints <- grid.CurviPoints

        if scratchVertices.Length <> newVertexCount then
            scratchVertices <- Array.zeroCreate<Vertex3D> newVertexCount

        if currMask.Length < newMaskWords then
            currMask <- Array.zeroCreate<uint64> newMaskWords
            nextMask <- Array.zeroCreate<uint64> newMaskWords

        if scratchIndices.Length < newMaxIndices then
            scratchIndices <- Array.zeroCreate<uint32> newMaxIndices

        if wireframeBatch.IsSome && scratchLines.Length <> newMaxLines then
            scratchLines <- Array.zeroCreate<LineVertex3D> newMaxLines

        if not isCurvilinear then
            if txBuf.Length <> newCols then
                txBuf <- Array.zeroCreate<float> newCols

            if tyBuf.Length <> newRows then
                tyBuf <- Array.zeroCreate<float> newRows

        let flipChanged = refreshScaleCaches spec true
        let heightsSpan = if isCurvilinear then ReadOnlySpan<float>.Empty else ReadOnlySpan<float>(grid.Heights)
        let scalarsSpan = ReadOnlySpan<float>(grid.Scalars)
        applyData spec heightsSpan scalarsSpan (topologyChanged || flipChanged)

    /// <summary>Releases GPU mesh, instance, and wireframe batch resources.</summary>
    member _.Dispose() =
        if not disposed then
            disposed <- true

            match opaqueBatchOpt with
            | Some b -> b.Dispose ()
            | None -> ()

            match alphaBatchOpt with
            | Some b -> b.Dispose ()
            | None -> ()

            match wireframeBatch with
            | Some lb -> lb.Dispose ()
            | None -> ()

            meshHandle.Dispose ()

    interface IDisposable with
        member this.Dispose() =
            this.Dispose ()

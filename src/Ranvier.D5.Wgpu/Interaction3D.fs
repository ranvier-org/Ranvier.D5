namespace Ranvier.D5.Wgpu

open System
open System.Collections.Generic
open System.Numerics
#if FABLE_COMPILER
open Ranvier.D5.Wgpu.FableCompat3D
#endif
open Ranvier
open Ranvier.D5
open Ranvier.Wgpu

/// <summary>Camera rotation behavior for interactive 3D plot controls.</summary>
[<RequireQualifiedAccess; Struct>]
type OrbitMode3D =
    /// <summary>Constrained azimuth/elevation orbit keeping world <c>+Y</c> upright.</summary>
    | Turntable
    /// <summary>Quaternion trackball supporting unconstrained full-sphere rotation.</summary>
    | Arcball

/// <summary>Optional world-axis constraint applied while panning a <c>PlotCamera3D</c>.</summary>
[<RequireQualifiedAccess; Struct>]
type AxisLock3D =
    /// <summary>Pans freely across both screen-aligned camera axes.</summary>
    | Free
    /// <summary>Restricts pan translation to world <c>X</c>.</summary>
    | LockX
    /// <summary>Restricts pan translation to world <c>Y</c>.</summary>
    | LockY
    /// <summary>Restricts pan translation to world <c>Z</c>.</summary>
    | LockZ

/// <summary>Immutable interactive 3D plot camera pose supporting turntable, arcball, zoom, pan, and orthographic-to-perspective transitions.</summary>
[<Struct>]
type PlotCamera3D =
    {
        /// <summary>Selected rotation kinematics (<c>Turntable</c> or <c>Arcball</c>).</summary>
        Mode: OrbitMode3D
        /// <summary>Horizontal turntable azimuth angle in radians.</summary>
        AzimuthRad: float32
        /// <summary>Vertical turntable elevation angle in radians, clamped away from gimbal poles.</summary>
        ElevationRad: float32
        /// <summary>Orientation quaternion used when <c>Mode = OrbitMode3D.Arcball</c>.</summary>
        Arcball: Quaternion
        /// <summary>Positive magnification multiplier (<c>1.0f</c> frames <c>Scale3D.fitRadius</c>).</summary>
        Zoom: float32
        /// <summary>World-space focus/pan target offset relative to the centered plot box origin.</summary>
        PanOffset: Vector3
        /// <summary>Projection blend factor in <c>[0.0, 1.0]</c> (<c>0.0f</c> orthographic, <c>1.0f</c> 45-degree perspective).</summary>
        Perspectiveness: float32
    }

/// <summary>Pure reducers and <c>Camera3D</c> conversion for <c>PlotCamera3D</c>.</summary>
[<RequireQualifiedAccess>]
module PlotCamera3D =
    [<Literal>]
    let private MaxPitchRad = 1.483529864f // 85 degrees in radians

    [<Literal>]
    let private OrbitSensitivityRadPerPx = 0.006f

    [<Literal>]
    let private MinZoom = 0.05f

    [<Literal>]
    let private MaxZoom = 100.0f

    let private clampZoom (zoom: float32) : float32 =
        if Check3D.isFinite32 zoom && zoom > 0.0f then
            Math.Clamp (zoom, MinZoom, MaxZoom)
        else
            1.0f

    let private clampElevation (elevRad: float32) : float32 =
        if Check3D.isFinite32 elevRad then
            Math.Clamp (elevRad, -MaxPitchRad, MaxPitchRad)
        else
            0.0f

    let private normalizeQuat (q: Quaternion) : Quaternion =
        if
            Check3D.isFinite32 q.X
            && Check3D.isFinite32 q.Y
            && Check3D.isFinite32 q.Z
            && Check3D.isFinite32 q.W
        then
            let lenSq = q.LengthSquared ()
            if lenSq > 1e-12f then Quaternion.Normalize q else Quaternion.Identity
        else
            Quaternion.Identity

    let private cameraBasis (camera: PlotCamera3D) : struct (Vector3 * Vector3) =
        match camera.Mode with
        | OrbitMode3D.Turntable ->
            let yaw = if Check3D.isFinite32 camera.AzimuthRad then camera.AzimuthRad else 0.0f
            let pitch = clampElevation camera.ElevationRad
            let cp = MathF.Cos pitch
            let offsetDir = Vector3 (cp * MathF.Sin yaw, MathF.Sin pitch, cp * MathF.Cos yaw)
            let rightCross = Vector3.Cross (Vector3.UnitY, offsetDir)

            let right =
                if rightCross.LengthSquared () > 1e-12f then
                    Vector3.Normalize rightCross
                else
                    Vector3.UnitX

            let upCross = Vector3.Cross (offsetDir, right)

            let up =
                if upCross.LengthSquared () > 1e-12f then
                    Vector3.Normalize upCross
                else
                    Vector3.UnitY

            struct (right, up)
        | OrbitMode3D.Arcball ->
            let m = Orientation3D.matrixFromQuaternion (normalizeQuat camera.Arcball)
            let right = Vector3.Normalize (Vector3.Transform (Vector3.UnitX, m))
            let up = Vector3.Normalize (Vector3.Transform (Vector3.UnitY, m))
            struct (right, up)

    /// <summary>Default camera matching Makie <c>Axis3</c> (<c>Turntable</c>, <c>AzimuthRad = 1.275 * pi</c>, <c>ElevationRad = pi / 8</c>, orthographic).</summary>
    let defaultValue: PlotCamera3D =
        {
            Mode = OrbitMode3D.Turntable
            AzimuthRad = 1.275f * MathF.PI
            ElevationRad = MathF.PI / 8.0f
            Arcball = Quaternion.Identity
            Zoom = 1.0f
            PanOffset = Vector3.Zero
            Perspectiveness = 0.0f
        }

    /// <summary>Applies pointer drag deltas in logical pixels according to <c>camera.Mode</c>.</summary>
    let orbitBy (deltaPixelsX: float32) (deltaPixelsY: float32) (camera: PlotCamera3D) : PlotCamera3D =
        Check3D.finite32 (nameof deltaPixelsX) deltaPixelsX
        Check3D.finite32 (nameof deltaPixelsY) deltaPixelsY

        match camera.Mode with
        | OrbitMode3D.Turntable ->
            let az = if Check3D.isFinite32 camera.AzimuthRad then camera.AzimuthRad else 0.0f
            let el = clampElevation camera.ElevationRad

            { camera with
                AzimuthRad = az - deltaPixelsX * OrbitSensitivityRadPerPx
                ElevationRad = clampElevation (el + deltaPixelsY * OrbitSensitivityRadPerPx)
            }
        | OrbitMode3D.Arcball ->
            let q0 = normalizeQuat camera.Arcball
            let yawQ = Quaternion.CreateFromAxisAngle (Vector3.UnitY, -deltaPixelsX * OrbitSensitivityRadPerPx)
            let pitchQ = Quaternion.CreateFromAxisAngle (Vector3.UnitX, deltaPixelsY * OrbitSensitivityRadPerPx)
            let qNext = normalizeQuat (q0 * yawQ * pitchQ)
            { camera with Arcball = qNext }

    /// <summary>Pans the camera target by normalized viewport deltas scaled to <c>fitRadius</c>, optionally constrained along one world axis.</summary>
    let panBy (fitRadius: float32) (lockAxis: AxisLock3D) (deltaNormX: float32) (deltaNormY: float32) (camera: PlotCamera3D) : PlotCamera3D =
        Check3D.positive32 (nameof fitRadius) fitRadius
        Check3D.finite32 (nameof deltaNormX) deltaNormX
        Check3D.finite32 (nameof deltaNormY) deltaNormY

        let struct (right, camUp) = cameraBasis camera
        let worldSpan = 2.0f * fitRadius / clampZoom camera.Zoom
        let rawDelta = (-right * deltaNormX + camUp * deltaNormY) * worldSpan

        let lockedDelta =
            match lockAxis with
            | AxisLock3D.Free -> rawDelta
            | AxisLock3D.LockX -> Vector3 (rawDelta.X, 0.0f, 0.0f)
            | AxisLock3D.LockY -> Vector3 (0.0f, rawDelta.Y, 0.0f)
            | AxisLock3D.LockZ -> Vector3 (0.0f, 0.0f, rawDelta.Z)

        { camera with PanOffset = camera.PanOffset + lockedDelta }

    /// <summary>Multiplies camera magnification by positive <c>factor</c>, clamped to supported precision bounds.</summary>
    let zoomBy (factor: float32) (camera: PlotCamera3D) : PlotCamera3D =
        Check3D.positive32 (nameof factor) factor
        { camera with Zoom = clampZoom (clampZoom camera.Zoom * factor) }

    /// <summary>Moves the camera focus target to <c>worldTarget</c>.</summary>
    let focusOn (worldTarget: Vector3) (camera: PlotCamera3D) : PlotCamera3D =
        Check3D.vector3 (nameof worldTarget) worldTarget
        { camera with PanOffset = worldTarget }

    let private buildPose (camera: PlotCamera3D) (target: Vector3) (dist: float32) (fovYDeg: float32) (nearP: float32) (farP: float32) : Camera3D =
        match camera.Mode with
        | OrbitMode3D.Turntable ->
            let yawDeg = camera.AzimuthRad * 180.0f / MathF.PI
            let pitchDeg = clampElevation camera.ElevationRad * 180.0f / MathF.PI

            let baseCam =
                if fovYDeg < 0.0f then
                    Camera3D.orthographic target yawDeg pitchDeg (-fovYDeg) dist
                else
                    Camera3D.orbit target yawDeg pitchDeg dist fovYDeg

            { baseCam with NearPlane = nearP; FarPlane = farP }
        | OrbitMode3D.Arcball ->
            let m = Orientation3D.matrixFromQuaternion (normalizeQuat camera.Arcball)
            let offsetDir = Vector3.Normalize (Vector3.Transform (Vector3.UnitZ, m))
            let camUp = Vector3.Normalize (Vector3.Transform (Vector3.UnitY, m))

            {
                Position = target + offsetDir * dist
                Target = target
                Up = camUp
                FovYDeg = fovYDeg
                NearPlane = nearP
                FarPlane = farP
            }

    /// <summary>Converts an interactive <c>PlotCamera3D</c> into a <c>Ranvier.Wgpu.Camera3D</c> framing <c>scale</c> with zoom-safe clipping planes.</summary>
    let toCamera3D (scale: Scale3D) (camera: PlotCamera3D) : Camera3D =
        if isNull (box scale) then
            nullArg (nameof scale)

        if camera.Perspectiveness <> camera.Perspectiveness then
            raise (ArgumentOutOfRangeException ("Perspectiveness", "Camera perspectiveness cannot be NaN."))

        Check3D.vector3 "PanOffset" camera.PanOffset

        let boxRadius = max 1e-4f (Scale3D.fitRadius scale)
        let zoom = clampZoom camera.Zoom
        let viewRadius = boxRadius / zoom
        let p = Math.Clamp (camera.Perspectiveness, 0.0f, 1.0f)
        let target = camera.PanOffset
        let panDist = target.Length ()

        if p <= 1e-4f then
            let dist = max 4.0f (boxRadius * 4.0f + panDist)
            let boxClearance = boxRadius * 4.0f + panDist
            buildPose camera target dist (-abs viewRadius) (max 0.01f (dist - boxClearance)) (dist + boxClearance)
        else
            let fovYDeg = max 0.5f (p * 45.0f)
            let dist = viewRadius / MathF.Sin (fovYDeg * MathF.PI / 360.0f)
            let boxMargin = boxRadius * 2.5f + panDist
            let nearFloor = max 1e-4f (min 0.01f (dist * 0.05f))
            let nearP = max nearFloor (dist - boxMargin)
            buildPose camera target dist fovYDeg nearP (max (nearP + 0.1f) (dist + boxMargin))

/// <summary>Resolved keyed GPU picking result pairing layer/key identity with world and inverted data coordinates.</summary>
[<Struct>]
type PickedMark<'K> =
    {
        /// <summary>Layer prefix byte assigned to the picked sink.</summary>
        LayerId: byte
        /// <summary>Stable key identifying the picked item within the sink's projection.</summary>
        Key: 'K
        /// <summary>Picked 3D world-space intersection coordinate.</summary>
        WorldPosition: Vector3
        /// <summary>World coordinate inverted back through <c>Scale3D</c> into data coordinates.</summary>
        DataPoint: Point3
    }

/// <summary>Non-generic GPU picking resolver enabling multi-layer <c>ScenePass</c> lookups across heterogeneous keyed sinks.</summary>
type IPickResolver =
    /// <summary>Layer prefix byte identifying this sink's <c>PickId</c> namespace (<c>1uy .. 255uy</c>).</summary>
    abstract LayerId: byte
    /// <summary>Number of currently active GPU slots tracked by this sink.</summary>
    abstract ActiveCount: int
    /// <summary>Returns true when <c>pickId</c> belongs to this layer and addresses a currently active slot.</summary>
    abstract ContainsPick: pickId: uint32 -> bool
    /// <summary>Resolves <c>pickId</c> to its boxed key when active, or <c>ValueNone</c> when foreign or retired.</summary>
    abstract TryResolveBoxedKey: pickId: uint32 -> obj voption

module internal PickEncoding =
    [<Literal>]
    let MaxSlotsPerLayer = 0x00FFFFFE

    let validateLayerId (layerId: byte) =
        if layerId = 0uy then
            raise (ArgumentOutOfRangeException (nameof layerId, "Pick layerId must be in [1 .. 255]."))

    let inline encode (layerId: byte) (slot: int) : uint32 =
        if layerId = 0uy then
            0u
        else
            (uint32 layerId <<< 24) ||| uint32 (slot + 1)

    let inline tryDecodeSlot (layerId: byte) (activeCount: int) (pickId: uint32) : int voption =
        if layerId = 0uy || (pickId >>> 24) <> uint32 layerId then
            ValueNone
        else
            let slot = int (pickId &&& 0x00FFFFFFu) - 1

            if slot >= 0 && slot < activeCount then
                ValueSome slot
            else
                ValueNone

module internal SlotSort =
    let private insertionSort (arr: int array) (lo: int) (hi: int) =
        for i = lo + 1 to hi do
            let x = arr[i]
            let mutable j = i - 1

            while j >= lo && arr[j] > x do
                arr[j + 1] <- arr[j]
                j <- j - 1

            arr[j + 1] <- x

    let rec private qsort (arr: int array) (lo: int) (hi: int) =
        if hi - lo <= 16 then
            insertionSort arr lo hi
        else
            let mid = lo + ((hi - lo) >>> 1)

            if arr[mid] < arr[lo] then
                let t = arr[lo]
                arr[lo] <- arr[mid]
                arr[mid] <- t

            if arr[hi] < arr[lo] then
                let t = arr[lo]
                arr[lo] <- arr[hi]
                arr[hi] <- t

            if arr[hi] < arr[mid] then
                let t = arr[mid]
                arr[mid] <- arr[hi]
                arr[hi] <- t

            let pivot = arr[mid]
            let mutable i = lo
            let mutable j = hi

            while i <= j do
                while arr[i] < pivot do
                    i <- i + 1

                while arr[j] > pivot do
                    j <- j - 1

                if i <= j then
                    let t = arr[i]
                    arr[i] <- arr[j]
                    arr[j] <- t
                    i <- i + 1
                    j <- j - 1

            if lo < j then
                qsort arr lo j

            if i < hi then
                qsort arr i hi

    let sortPrefix (arr: int array) (count: int) =
        if count > 1 then
            qsort arr 0 (count - 1)

type internal IKeyedSinkSync<'K when 'K: equality> =
    abstract ActiveCount: int
    abstract LastUpdatedSlots: int
    abstract TryResolvePick: pickId: uint32 -> 'K voption

[<Sealed>]
type internal KeyedSlotSync<'K, 'Datum, 'Gpu when 'K: equality>
    (
        layerId: byte,
        slotsPerItem: int,
        preserveOrder: bool,
        readScale: unit -> Scale3D,
        rows: Projection<'K, 'Datum>,
        packSlot: Scale3D -> uint32 -> 'Datum -> 'Gpu array -> int -> bool,
        rewritePickId: 'Gpu array -> int -> uint32 -> unit,
        uploadFull: 'Gpu array -> int -> unit,
        uploadRange: int -> 'Gpu array -> int -> int -> unit,
        resizeBatch: int -> unit
    ) =
    let keyToSlot = Dictionary<'K, int>(HashIdentity.Structural)
    let mutable hasNullKey = false
    let mutable nullSlot = -1
    let mutable slotToKey = Array.zeroCreate<'K> 16
    let mutable shadow = Array.zeroCreate<'Gpu> (16 * slotsPerItem)
    let mutable dirtyTag = Array.zeroCreate<int> 16
    let mutable dirtySlots = Array.zeroCreate<int> 16
    let mutable dirtyGen = 1
    let mutable dirtyCount = 0
    let mutable activeCount = 0
    let mutable prevBatchCount = 0
    let mutable lastUpdatedSlots = 0
    let mutable initialized = false
    let mutable lastScale = Unchecked.defaultof<Scale3D>
    let mutable previousKeyCount = 0
    let tempMoved = Array.zeroCreate<'Gpu> slotsPerItem

    let ensureCapacity (neededItems: int) =
        if neededItems > slotToKey.Length then
            let newCap = max neededItems (max 16 (slotToKey.Length * 2))
            let nextKeys = Array.zeroCreate<'K> newCap
            Array.Copy (slotToKey, nextKeys, min activeCount slotToKey.Length)
            slotToKey <- nextKeys

            let nextShadow = Array.zeroCreate<'Gpu> (newCap * slotsPerItem)
            Array.Copy (shadow, nextShadow, min (activeCount * slotsPerItem) shadow.Length)
            shadow <- nextShadow

            let nextTag = Array.zeroCreate<int> newCap
            Array.Copy (dirtyTag, nextTag, dirtyTag.Length)
            dirtyTag <- nextTag

            let nextDirty = Array.zeroCreate<int> newCap
            Array.Copy (dirtySlots, nextDirty, min dirtyCount dirtySlots.Length)
            dirtySlots <- nextDirty

    let tryFindSlot (key: 'K) : int voption =
        if isNull (box key) then
            if hasNullKey then ValueSome nullSlot else ValueNone
        else
            match keyToSlot.TryGetValue key with
            | true, slot -> ValueSome slot
            | _ -> ValueNone

    let setSlotMapping (key: 'K) (slot: int) =
        if isNull (box key) then
            hasNullKey <- true
            nullSlot <- slot
        else
            keyToSlot[key] <- slot

        slotToKey[slot] <- key

    let removeSlotMapping (key: 'K) =
        if isNull (box key) then
            hasNullKey <- false
            nullSlot <- -1
        else
            keyToSlot.Remove key |> ignore

    let clearMappings () =
        hasNullKey <- false
        nullSlot <- -1
        keyToSlot.Clear ()

    let markDirty (slot: int) =
        if dirtyTag[slot] <> dirtyGen then
            dirtyTag[slot] <- dirtyGen
            dirtySlots[dirtyCount] <- slot
            dirtyCount <- dirtyCount + 1

    let nextDirtyGen () =
        if dirtyGen = Int32.MaxValue then
            for i = 0 to dirtyTag.Length - 1 do
                dirtyTag[i] <- 0

            dirtyGen <- 1
        else
            dirtyGen <- dirtyGen + 1

        dirtyCount <- 0

    let tryReadRow (key: 'K) : 'Datum option =
        Api.untrack (fun () ->
            try
                rows.TryGet key
            with NotReadyException _ ->
                None)

    let writeSlot (curScale: Scale3D) (slot: int) (datum: 'Datum) : bool =
        if layerId <> 0uy && slot >= PickEncoding.MaxSlotsPerLayer then
            raise (InvalidOperationException $"Keyed GPU layer {layerId} exceeded maximum pickable slot capacity ({PickEncoding.MaxSlotsPerLayer}).")

        let pickId = PickEncoding.encode layerId slot
        packSlot curScale pickId datum shadow (slot * slotsPerItem)

    let copySlot (srcSlot: int) (dstSlot: int) =
        slotToKey[dstSlot] <- slotToKey[srcSlot]
        let srcBase = srcSlot * slotsPerItem
        let dstBase = dstSlot * slotsPerItem

        for k = 0 to slotsPerItem - 1 do
            shadow[dstBase + k] <- shadow[srcBase + k]

    let shiftSlotsLeft (startSlot: int) (endInclusive: int) =
        for s = startSlot to endInclusive do
            copySlot (s + 1) s

    let shiftSlotsRight (startSlot: int) (endInclusive: int) =
        for s = endInclusive downto startSlot do
            copySlot s (s + 1)

    let removeAtSlot (slot: int) =
        let oldKey = slotToKey[slot]
        removeSlotMapping oldKey
        let lastSlot = activeCount - 1
        activeCount <- lastSlot

        if slot < lastSlot then
            if preserveOrder then
                shiftSlotsLeft slot (lastSlot - 1)

                for s = slot to lastSlot - 1 do
                    setSlotMapping slotToKey[s] s

                    if layerId <> 0uy then
                        rewritePickId shadow (s * slotsPerItem) (PickEncoding.encode layerId s)

                    markDirty s
            else
                copySlot lastSlot slot

                if layerId <> 0uy then
                    rewritePickId shadow (slot * slotsPerItem) (PickEncoding.encode layerId slot)

                setSlotMapping slotToKey[slot] slot
                markDirty slot

        slotToKey[lastSlot] <- Unchecked.defaultof<'K>

    let rebuildFull (keys: 'K array) (curScale: Scale3D) =
        nextDirtyGen ()
        clearMappings ()
        ensureCapacity keys.Length
        let prevActive = activeCount
        let mutable nextCount = 0

        for i = 0 to keys.Length - 1 do
            let key = keys[i]

            match tryReadRow key with
            | Some datum ->
                if writeSlot curScale nextCount datum then
                    setSlotMapping key nextCount
                    nextCount <- nextCount + 1
            | None -> ()

        for i = nextCount to prevActive - 1 do
            slotToKey[i] <- Unchecked.defaultof<'K>

        activeCount <- nextCount
        uploadFull shadow (nextCount * slotsPerItem)
        prevBatchCount <- nextCount
        lastUpdatedSlots <- nextCount
        previousKeyCount <- keys.Length

    let flushIncremental () =
        if activeCount > prevBatchCount then
            resizeBatch (activeCount * slotsPerItem)

        let mutable validDirty = 0

        for i = 0 to dirtyCount - 1 do
            let s = dirtySlots[i]

            if s >= 0 && s < activeCount then
                dirtySlots[validDirty] <- s
                validDirty <- validDirty + 1

        if validDirty = 0 then
            lastUpdatedSlots <- 0
        else
            SlotSort.sortPrefix dirtySlots validDirty
            let mutable totalUploaded = 0
            let mutable runStart = dirtySlots[0]
            let mutable runEnd = runStart

            for i = 1 to validDirty - 1 do
                let s = dirtySlots[i]

                if s = runEnd + 1 then
                    runEnd <- s
                else
                    let runLen = runEnd - runStart + 1
                    uploadRange (runStart * slotsPerItem) shadow (runStart * slotsPerItem) (runLen * slotsPerItem)
                    totalUploaded <- totalUploaded + runLen
                    runStart <- s
                    runEnd <- s

            let lastLen = runEnd - runStart + 1
            uploadRange (runStart * slotsPerItem) shadow (runStart * slotsPerItem) (lastLen * slotsPerItem)
            lastUpdatedSlots <- totalUploaded + lastLen

        if activeCount < prevBatchCount then
            resizeBatch (activeCount * slotsPerItem)

        prevBatchCount <- activeCount
        nextDirtyGen ()

    let applyUpserts (curScale: Scale3D) (delta: ProjectionDelta<'K>) (skipAdded: bool) =
        for kvp in delta.Changes do
            let ch = kvp.Value

            if ch <> KeyChange.Removed && not (skipAdded && ch = KeyChange.Added) then
                let key = kvp.Key

                match tryReadRow key, tryFindSlot key with
                | Some datum, ValueSome slot ->
                    if writeSlot curScale slot datum then
                        markDirty slot
                    else
                        removeAtSlot slot
                | Some datum, ValueNone ->
                    let slot = activeCount
                    ensureCapacity (slot + 1)

                    if writeSlot curScale slot datum then
                        setSlotMapping key slot
                        activeCount <- slot + 1
                        markDirty slot
                | None, ValueSome slot -> removeAtSlot slot
                | None, ValueNone -> ()

    let tryApplyPositional (curScale: Scale3D) (delta: ProjectionDelta<'K>) : bool =
        if activeCount <> previousKeyCount then
            false
        else
            let edits = delta.Positional
            let maxCount = max activeCount delta.Keys.Length
            ensureCapacity maxCount
            let mutable curCount = activeCount
            let mutable minTouched = Int32.MaxValue
            let mutable maxTouched = -1
            let mutable ok = true
            let mutable ei = 0

            while ok && ei < edits.Count do
                match edits[ei] with
                | PositionalChange.RemoveAt idx ->
                    if idx < 0 || idx >= curCount then
                        ok <- false
                    else
                        removeSlotMapping slotToKey[idx]
                        shiftSlotsLeft idx (curCount - 2)

                        if idx < curCount - 1 then
                            minTouched <- min minTouched idx
                            maxTouched <- max maxTouched (curCount - 2)

                        curCount <- curCount - 1
                        slotToKey[curCount] <- Unchecked.defaultof<'K>
                | PositionalChange.InsertAt (idx, key) ->
                    if idx < 0 || idx > curCount then
                        ok <- false
                    else
                        match tryReadRow key with
                        | None -> ok <- false
                        | Some datum ->
                            shiftSlotsRight idx (curCount - 1)

                            if not (writeSlot curScale idx datum) then
                                ok <- false
                            else
                                slotToKey[idx] <- key
                                minTouched <- min minTouched idx
                                maxTouched <- max maxTouched curCount
                                curCount <- curCount + 1
                | PositionalChange.Move (oldIdx, newIdx) ->
                    if oldIdx < 0 || oldIdx >= curCount || newIdx < 0 || newIdx >= curCount then
                        ok <- false
                    elif oldIdx <> newIdx then
                        let movedKey = slotToKey[oldIdx]
                        let oldBase = oldIdx * slotsPerItem

                        for k = 0 to slotsPerItem - 1 do
                            tempMoved[k] <- shadow[oldBase + k]

                        if oldIdx < newIdx then
                            shiftSlotsLeft oldIdx (newIdx - 1)
                        else
                            shiftSlotsRight newIdx (oldIdx - 1)

                        slotToKey[newIdx] <- movedKey
                        let newBase = newIdx * slotsPerItem

                        for k = 0 to slotsPerItem - 1 do
                            shadow[newBase + k] <- tempMoved[k]

                        minTouched <- min minTouched (min oldIdx newIdx)
                        maxTouched <- max maxTouched (max oldIdx newIdx)

                ei <- ei + 1

            if not ok then
                false
            else
                activeCount <- curCount

                if maxTouched >= minTouched && curCount > 0 then
                    let hi = min (curCount - 1) maxTouched

                    for s = max 0 minTouched to hi do
                        setSlotMapping slotToKey[s] s

                        if layerId <> 0uy then
                            rewritePickId shadow (s * slotsPerItem) (PickEncoding.encode layerId s)

                        markDirty s

                applyUpserts curScale delta true
                previousKeyCount <- delta.Keys.Length
                flushIncremental ()
                true

    member _.ActiveCount = activeCount
    member _.LastUpdatedSlots = lastUpdatedSlots

    member _.TryResolvePick(pickId: uint32) : 'K voption =
        match PickEncoding.tryDecodeSlot layerId activeCount pickId with
        | ValueSome slot -> ValueSome slotToKey[slot]
        | ValueNone -> ValueNone

    interface IKeyedSinkSync<'K> with
        member _.ActiveCount = activeCount
        member _.LastUpdatedSlots = lastUpdatedSlots

        member _.TryResolvePick(pickId: uint32) =
            match PickEncoding.tryDecodeSlot layerId activeCount pickId with
            | ValueSome slot -> ValueSome slotToKey[slot]
            | ValueNone -> ValueNone

    member _.Sync(delta: ProjectionDelta<'K>) =
        let curScale = readScale ()

        if isNull (box curScale) then
            nullArg "scale"

        let scaleChanged = not (obj.ReferenceEquals (curScale, lastScale))

        if not initialized || delta.IsReset || scaleChanged then
            initialized <- true
            lastScale <- curScale
            rebuildFull delta.Keys curScale
        elif preserveOrder && delta.OrderChanged then
            if not (tryApplyPositional curScale delta) then
                rebuildFull delta.Keys curScale
        elif delta.Changes.Count > 0 then
            ensureCapacity (activeCount + delta.Changes.Count)

            if not preserveOrder then
                for kvp in delta.Changes do
                    if kvp.Value = KeyChange.Removed then
                        match tryFindSlot kvp.Key with
                        | ValueSome slot -> removeAtSlot slot
                        | ValueNone -> ()

            applyUpserts curScale delta false
            previousKeyCount <- delta.Keys.Length
            flushIncremental ()
        else
            previousKeyCount <- delta.Keys.Length
            lastUpdatedSlots <- 0

/// <summary>Incremental keyed <c>InstanceBatch</c> sink tracking slot-to-key mappings and retired <c>PickId</c>s across <c>ProjectionDelta</c> updates.</summary>
[<Sealed>]
type KeyedInstanceSink<'K when 'K: equality> internal (layerId: byte, batch: InstanceBatch, sync: IKeyedSinkSync<'K>) =
    /// <summary>Underlying GPU <c>InstanceBatch</c> kept synchronized with the source <c>Projection</c>.</summary>
    member _.Batch: InstanceBatch = batch
    /// <summary>Layer prefix byte encoded into the high 8 bits of each instance <c>PickId</c>.</summary>
    member _.LayerId: byte = layerId
    /// <summary>Number of currently live GPU instances in <c>Batch</c>.</summary>
    member _.ActiveCount: int = sync.ActiveCount
    /// <summary>Diagnostic count of GPU instance slots written on the latest reactive pass.</summary>
    member _.LastUpdatedSlots: int = sync.LastUpdatedSlots

    /// <summary>Resolves a GPU <c>PickId</c> to its active key, returning <c>ValueNone</c> if foreign, zero, or retired after removal/re-index.</summary>
    member _.TryResolvePick(pickId: uint32) : 'K voption =
        sync.TryResolvePick pickId

    interface IPickResolver with
        member _.LayerId = layerId
        member _.ActiveCount = sync.ActiveCount
        member _.ContainsPick(pickId: uint32) = (sync.TryResolvePick pickId).IsSome

        member _.TryResolveBoxedKey(pickId: uint32) =
            match sync.TryResolvePick pickId with
            | ValueSome k -> ValueSome (box k)
            | ValueNone -> ValueNone

/// <summary>Incremental keyed <c>DiscBatch</c> sink tracking slot-to-key mappings and retired <c>PickId</c>s across <c>ProjectionDelta</c> updates.</summary>
[<Sealed>]
type KeyedDiscSink<'K when 'K: equality> internal (layerId: byte, batch: DiscBatch, sync: IKeyedSinkSync<'K>) =
    /// <summary>Underlying GPU <c>DiscBatch</c> kept synchronized with the source <c>Projection</c>.</summary>
    member _.Batch: DiscBatch = batch
    /// <summary>Layer prefix byte encoded into the high 8 bits of each disc <c>PickId</c>.</summary>
    member _.LayerId: byte = layerId
    /// <summary>Number of currently live GPU disc instances in <c>Batch</c>.</summary>
    member _.ActiveCount: int = sync.ActiveCount
    /// <summary>Diagnostic count of GPU disc slots written on the latest reactive pass.</summary>
    member _.LastUpdatedSlots: int = sync.LastUpdatedSlots

    /// <summary>Resolves a GPU <c>PickId</c> to its active key, returning <c>ValueNone</c> if foreign, zero, or retired after removal/re-index.</summary>
    member _.TryResolvePick(pickId: uint32) : 'K voption =
        sync.TryResolvePick pickId

    interface IPickResolver with
        member _.LayerId = layerId
        member _.ActiveCount = sync.ActiveCount
        member _.ContainsPick(pickId: uint32) = (sync.TryResolvePick pickId).IsSome

        member _.TryResolveBoxedKey(pickId: uint32) =
            match sync.TryResolvePick pickId with
            | ValueSome k -> ValueSome (box k)
            | ValueNone -> ValueNone

/// <summary>Incremental keyed <c>LineBatch</c> sink synchronizing independent 3D segments across <c>ProjectionDelta</c> updates.</summary>
[<Sealed>]
type KeyedSegmentSink<'K when 'K: equality> internal (batch: LineBatch, sync: IKeyedSinkSync<'K>) =
    /// <summary>Underlying GPU <c>LineBatch</c> holding 2 vertices per active segment.</summary>
    member _.Batch: LineBatch = batch
    /// <summary>Number of currently live 3D line segments in <c>Batch</c>.</summary>
    member _.ActiveCount: int = sync.ActiveCount
    /// <summary>Diagnostic count of segment slots updated on the latest reactive pass.</summary>
    member _.LastUpdatedSlots: int = sync.LastUpdatedSlots

/// <summary>Helpers resolving 3D GPU picking hits back to keyed marks, data coordinates, and axis back-wall hover spikes.</summary>
[<RequireQualifiedAccess>]
module Probe3D =
    let private resolveWith
        (scale: Scale3D)
        (layerId: byte)
        (tryResolve: uint32 -> 'K voption)
        (hit: PickHit)
        : PickedMark<'K> voption =
        if isNull (box scale) then
            nullArg (nameof scale)

        if hit.PickId = 0u then
            ValueNone
        else
            match tryResolve hit.PickId with
            | ValueSome key ->
                ValueSome
                    {
                        LayerId = layerId
                        Key = key
                        WorldPosition = hit.WorldPosition
                        DataPoint = Scale3D.invert scale hit.WorldPosition
                    }
            | ValueNone -> ValueNone

    /// <summary>Resolves a GPU <c>PickHit</c> against a keyed <c>KeyedInstanceSink</c>, returning <c>ValueNone</c> for unmatched or retired IDs.</summary>
    let resolveKeyed (scale: Scale3D) (sink: KeyedInstanceSink<'K>) (hit: PickHit) : PickedMark<'K> voption =
        if isNull (box sink) then
            nullArg (nameof sink)

        resolveWith scale sink.LayerId sink.TryResolvePick hit

    /// <summary>Resolves a GPU <c>PickHit</c> against a keyed <c>KeyedDiscSink</c>, returning <c>ValueNone</c> for unmatched or retired IDs.</summary>
    let resolveDisc (scale: Scale3D) (sink: KeyedDiscSink<'K>) (hit: PickHit) : PickedMark<'K> voption =
        if isNull (box sink) then
            nullArg (nameof sink)

        resolveWith scale sink.LayerId sink.TryResolvePick hit

    /// <summary>Inverts a GPU <c>PickHit.WorldPosition</c> through <c>scale</c> back into data coordinates.</summary>
    let invertHit (scale: Scale3D) (hit: PickHit) : Point3 =
        if isNull (box scale) then
            nullArg (nameof scale)

        Scale3D.invert scale hit.WorldPosition

    /// <summary>Emits 3 orthogonal hairline drop-segments (6 <c>LineVertex3D</c> endpoints) from <c>worldPos</c> onto <c>Axis3D.backWallPlanes axisFrame</c>.</summary>
    let hoverSpikes (scale: Scale3D) (axisFrame: AxisFrame3D) (color: RgbaColor) (worldPos: Vector3) : ReadOnlyMemory<LineVertex3D> =
        if isNull (box scale) then
            nullArg (nameof scale)

        if isNull (box axisFrame) then
            nullArg (nameof axisFrame)

        if not (MarksKernel.isFiniteColor color) then
            invalidArg (nameof color) "Hover spike color channels must be finite."

        Check3D.vector3 (nameof worldPos) worldPos
        let struct (bx, fy, bz) = Axis3D.backWallPlanes axisFrame
        let colV4 = color.ToVector4 ()

        let vertices =
            [|
                LineVertex3D (worldPos, colV4)
                LineVertex3D (Vector3 (bx, worldPos.Y, worldPos.Z), colV4)
                LineVertex3D (worldPos, colV4)
                LineVertex3D (Vector3 (worldPos.X, fy, worldPos.Z), colV4)
                LineVertex3D (worldPos, colV4)
                LineVertex3D (Vector3 (worldPos.X, worldPos.Y, bz), colV4)
            |]

        ReadOnlyMemory<LineVertex3D>(vertices)

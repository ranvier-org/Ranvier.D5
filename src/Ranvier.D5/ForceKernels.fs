namespace Ranvier.D5

open System
#if !FABLE_COMPILER
open System.Runtime.InteropServices
open System.Runtime.Intrinsics
#endif

/// <summary>Many-body force settings: finite strength (negative repels, positive attracts), nonnegative Barnes–Hut theta (0 is exact pairwise),
/// positive minimum softening distance and a maximum interaction distance no smaller than it.</summary>
type ManyBodyConfig = { Strength:float; Theta:float; MinDistance:float; MaxDistance:float }
/// <summary>Constructors for <c>ManyBodyConfig</c>.</summary>
[<RequireQualifiedAccess>]
module ManyBodyConfig =
    /// <summary>Creates a configuration with theta 0.8, minimum distance 1 and unbounded maximum distance.</summary>
    let create strength = {Strength=strength;Theta=0.8;MinDistance=1.;MaxDistance=infinity}

/// <summary>Collision force settings: strength in [0,1] scales overlap correction, applied over a positive number of iterations per tick.</summary>
type CollisionConfig = { Strength:float; Iterations:int }
/// <summary>Constructors for <c>CollisionConfig</c>.</summary>
[<RequireQualifiedAccess>]
module CollisionConfig =
    /// <summary>Creates a configuration with one iteration per tick.</summary>
    let create strength = {Strength=strength;Iterations=1}

module internal ForceKernels =
    let inline private check (token:System.Threading.CancellationToken) i = if (i &&& 63) = 0 then token.ThrowIfCancellationRequested()
    let private near (p:Point) (extent:QuadExtent) =
        Double.Hypot(p.X-max extent.Min.X (min extent.Max.X p.X),p.Y-max extent.Min.Y (min extent.Max.Y p.Y))
    let inline private nearSq (px: float) (py: float) (extent: QuadExtent) =
        let dx = px - max extent.Min.X (min extent.Max.X px)
        let dy = py - max extent.Min.Y (min extent.Max.Y py)
        dx * dx + dy * dy
    let private far (p:Point) (extent:QuadExtent) =
        Double.Hypot(max (abs(p.X-extent.Min.X)) (abs(p.X-extent.Max.X)),max (abs(p.Y-extent.Min.Y)) (abs(p.Y-extent.Max.Y)))

    let manyBody (token: System.Threading.CancellationToken) (config: ManyBodyConfig) alpha (px: float[]) (py: float[]) (positions: Point[]) (nodeIds: int[]) (masses: float[]) maxCoordAbs jitter (vx: float[]) (vy: float[]) =
        let n = px.Length
        let inline addFallback i srcX srcY mass sourceIndex =
            let dx, dy = srcX - px[i], srcY - py[i]
            let actual = Double.Hypot(dx, dy)
            Check.finite "many-body distance" actual
            if actual <= config.MaxDistance then
                let ux, uy, distance =
                    if actual = 0. then
                        let x, y = jitter i sourceIndex
                        x, y, config.MinDistance
                    else dx / actual, dy / actual, actual
                let softened = max distance config.MinDistance
                let magnitude = (config.Strength * alpha / softened) * mass * (distance / softened)
                vx[i] <- vx[i] + ux * magnitude
                vy[i] <- vy[i] + uy * magnitude
        let bounded = Double.IsFinite config.MaxDistance
        let strengthAlpha = config.Strength * alpha
        let minDist = config.MinDistance
        let fastSq = not bounded && maxCoordAbs < 1e140 && minDist < 1e140 && abs strengthAlpha < 1e140
        if config.Strength <> 0. && n > 0 then
            if config.Theta = 0. then
                if fastSq then
                    // Proven in Spatial.v (many_body_rational_form_cancels_sqrt):
                    // ux * magnitude = dx * (strength * alpha * mass / max(distSq, minDist^2))
                    let minDistSq = minDist * minDist
                    let inline addCoincidentOrScalar i j accX accY =
                        if i = j then struct(accX, accY)
                        else
                            let dx = px[j] - px[i]
                            let dy = py[j] - py[i]
                            let d2 = dx * dx + dy * dy
                            if d2 = 0. then
                                let jx, jy = jitter i j
                                let mag = (strengthAlpha / minDist) * masses[j]
                                struct(accX + jx * mag, accY + jy * mag)
                            else
                                let denom = max d2 minDistSq
                                let f = (strengthAlpha * masses[j]) / denom
                                struct(accX + dx * f, accY + dy * f)
#if !FABLE_COMPILER
                    let rPx = &MemoryMarshal.GetArrayDataReference px
                    let rPy = &MemoryMarshal.GetArrayDataReference py
                    let rMass = &MemoryMarshal.GetArrayDataReference masses
                    if Vector512.IsHardwareAccelerated && n >= 8 then
                        let vMinSq = Vector512.Create minDistSq
                        let vSA = Vector512.Create strengthAlpha
                        for i = 0 to n - 1 do
                            check token i
                            let pxi = px[i]
                            let pyi = py[i]
                            let vPxi = Vector512.Create pxi
                            let vPyi = Vector512.Create pyi
                            let mutable vAccX = Vector512<float>.Zero
                            let mutable vAccY = Vector512<float>.Zero
                            let mutable sAccX = 0.
                            let mutable sAccY = 0.
                            let mutable j = 0
                            while j <= n - 8 do
                                if (j &&& 511) = 0 then token.ThrowIfCancellationRequested()
                                let vDx = Vector512.LoadUnsafe(&rPx, unativeint j) - vPxi
                                let vDy = Vector512.LoadUnsafe(&rPy, unativeint j) - vPyi
                                let vD2 = Vector512.FusedMultiplyAdd(vDx, vDx, vDy * vDy)
                                if Vector512.GreaterThan(vD2, Vector512<float>.Zero) = Vector512<float>.AllBitsSet then
                                    let vDenom = Vector512.Max(vD2, vMinSq)
                                    let vF = (vSA * Vector512.LoadUnsafe(&rMass, unativeint j)) / vDenom
                                    vAccX <- Vector512.FusedMultiplyAdd(vDx, vF, vAccX)
                                    vAccY <- Vector512.FusedMultiplyAdd(vDy, vF, vAccY)
                                else
                                    for lane = 0 to 7 do
                                        let struct(nx, ny) = addCoincidentOrScalar i (j + lane) sAccX sAccY
                                        sAccX <- nx; sAccY <- ny
                                j <- j + 8
                            sAccX <- sAccX + Vector512.Sum vAccX
                            sAccY <- sAccY + Vector512.Sum vAccY
                            while j < n do
                                let struct(nx, ny) = addCoincidentOrScalar i j sAccX sAccY
                                sAccX <- nx; sAccY <- ny
                                j <- j + 1
                            vx[i] <- vx[i] + sAccX
                            vy[i] <- vy[i] + sAccY
                    elif Vector256.IsHardwareAccelerated && n >= 4 then
                        let vMinSq = Vector256.Create minDistSq
                        let vSA = Vector256.Create strengthAlpha
                        for i = 0 to n - 1 do
                            check token i
                            let pxi = px[i]
                            let pyi = py[i]
                            let vPxi = Vector256.Create pxi
                            let vPyi = Vector256.Create pyi
                            let mutable vAccX = Vector256<float>.Zero
                            let mutable vAccY = Vector256<float>.Zero
                            let mutable sAccX = 0.
                            let mutable sAccY = 0.
                            let mutable j = 0
                            while j <= n - 4 do
                                if (j &&& 511) = 0 then token.ThrowIfCancellationRequested()
                                let vDx = Vector256.LoadUnsafe(&rPx, unativeint j) - vPxi
                                let vDy = Vector256.LoadUnsafe(&rPy, unativeint j) - vPyi
                                let vD2 = Vector256.FusedMultiplyAdd(vDx, vDx, vDy * vDy)
                                if Vector256.GreaterThan(vD2, Vector256<float>.Zero) = Vector256<float>.AllBitsSet then
                                    let vDenom = Vector256.Max(vD2, vMinSq)
                                    let vF = (vSA * Vector256.LoadUnsafe(&rMass, unativeint j)) / vDenom
                                    vAccX <- Vector256.FusedMultiplyAdd(vDx, vF, vAccX)
                                    vAccY <- Vector256.FusedMultiplyAdd(vDy, vF, vAccY)
                                else
                                    for lane = 0 to 3 do
                                        let struct(nx, ny) = addCoincidentOrScalar i (j + lane) sAccX sAccY
                                        sAccX <- nx; sAccY <- ny
                                j <- j + 4
                            sAccX <- sAccX + Vector256.Sum vAccX
                            sAccY <- sAccY + Vector256.Sum vAccY
                            while j < n do
                                let struct(nx, ny) = addCoincidentOrScalar i j sAccX sAccY
                                sAccX <- nx; sAccY <- ny
                                j <- j + 1
                            vx[i] <- vx[i] + sAccX
                            vy[i] <- vy[i] + sAccY
                    elif Vector128.IsHardwareAccelerated && n >= 2 then
                        let vMinSq = Vector128.Create minDistSq
                        let vSA = Vector128.Create strengthAlpha
                        for i = 0 to n - 1 do
                            check token i
                            let pxi = px[i]
                            let pyi = py[i]
                            let vPxi = Vector128.Create pxi
                            let vPyi = Vector128.Create pyi
                            let mutable vAccX = Vector128<float>.Zero
                            let mutable vAccY = Vector128<float>.Zero
                            let mutable sAccX = 0.
                            let mutable sAccY = 0.
                            let mutable j = 0
                            while j <= n - 2 do
                                if (j &&& 511) = 0 then token.ThrowIfCancellationRequested()
                                let vDx = Vector128.LoadUnsafe(&rPx, unativeint j) - vPxi
                                let vDy = Vector128.LoadUnsafe(&rPy, unativeint j) - vPyi
                                let vD2 = Vector128.FusedMultiplyAdd(vDx, vDx, vDy * vDy)
                                if Vector128.GreaterThan(vD2, Vector128<float>.Zero) = Vector128<float>.AllBitsSet then
                                    let vDenom = Vector128.Max(vD2, vMinSq)
                                    let vF = (vSA * Vector128.LoadUnsafe(&rMass, unativeint j)) / vDenom
                                    vAccX <- Vector128.FusedMultiplyAdd(vDx, vF, vAccX)
                                    vAccY <- Vector128.FusedMultiplyAdd(vDy, vF, vAccY)
                                else
                                    for lane = 0 to 1 do
                                        let struct(nx, ny) = addCoincidentOrScalar i (j + lane) sAccX sAccY
                                        sAccX <- nx; sAccY <- ny
                                j <- j + 2
                            sAccX <- sAccX + Vector128.Sum vAccX
                            sAccY <- sAccY + Vector128.Sum vAccY
                            while j < n do
                                let struct(nx, ny) = addCoincidentOrScalar i j sAccX sAccY
                                sAccX <- nx; sAccY <- ny
                                j <- j + 1
                            vx[i] <- vx[i] + sAccX
                            vy[i] <- vy[i] + sAccY
                    else
                        for i = 0 to n - 1 do
                            check token i
                            let mutable sAccX = 0.
                            let mutable sAccY = 0.
                            for j = 0 to n - 1 do
                                check token j
                                let struct(nx, ny) = addCoincidentOrScalar i j sAccX sAccY
                                sAccX <- nx; sAccY <- ny
                            vx[i] <- vx[i] + sAccX
                            vy[i] <- vy[i] + sAccY
#else
                    for i = 0 to n - 1 do
                        check token i
                        let mutable sAccX = 0.
                        let mutable sAccY = 0.
                        for j = 0 to n - 1 do
                            check token j
                            let struct(nx, ny) = addCoincidentOrScalar i j sAccX sAccY
                            sAccX <- nx; sAccY <- ny
                        vx[i] <- vx[i] + sAccX
                        vy[i] <- vy[i] + sAccY
#endif
                else
                    for i = 0 to n - 1 do
                        check token i
                        for j = 0 to n - 1 do
                            check token j
                            if i <> j then addFallback i px[j] py[j] masses[j] j
            else
                for i = 0 to n - 1 do
                    positions[i] <- { Point.X = px[i]; Y = py[i] }
                let tree = Quadtree.buildWithCancellation token (fun i -> positions[i]) (fun i -> masses[i]) nodeIds
                if fastSq && config.Theta < 1e140 then
                    let thetaSq = config.Theta * config.Theta
                    let minDistSq = minDist * minDist
                    Quadtree.visitIndexed n (fun i cell ->
                        token.ThrowIfCancellationRequested()
                        let pxi = px[i]
                        let pyi = py[i]
                        let e = cell.Extent
                        let contains = pxi >= e.Min.X && pxi <= e.Max.X && pyi >= e.Min.Y && pyi <= e.Max.Y
                        let cdx = cell.Center.X - pxi
                        let cdy = cell.Center.Y - pyi
                        let d2 = cdx * cdx + cdy * cdy
                        let size = max (e.Max.X - e.Min.X) (e.Max.Y - e.Min.Y)
                        if not contains && size * size < thetaSq * d2 then
                            let denom = max d2 minDistSq
                            let f = (strengthAlpha * cell.Mass) / denom
                            vx[i] <- vx[i] + cdx * f
                            vy[i] <- vy[i] + cdy * f
                            false
                        elif not cell.Items.IsEmpty then
                            let mutable idx = 0
                            let mutable accX = vx[i]
                            let mutable accY = vy[i]
                            for item in cell.Items do
                                check token idx
                                idx <- idx + 1
                                let j = item.Value
                                if j <> i then
                                    let dx = item.Position.X - pxi
                                    let dy = item.Position.Y - pyi
                                    let id2 = dx * dx + dy * dy
                                    if id2 = 0. then
                                        let jx, jy = jitter i j
                                        let mag = (strengthAlpha / minDist) * item.Mass
                                        accX <- accX + jx * mag
                                        accY <- accY + jy * mag
                                    else
                                        let denom = max id2 minDistSq
                                        let f = (strengthAlpha * item.Mass) / denom
                                        accX <- accX + dx * f
                                        accY <- accY + dy * f
                            vx[i] <- accX
                            vy[i] <- accY
                            false
                        else true) tree
                else
                    Quadtree.visitIndexed n (fun i cell ->
                        let point = positions[i]
                        token.ThrowIfCancellationRequested()
                        if bounded && near point cell.Extent > config.MaxDistance then false else
                        let e = cell.Extent
                        let contains = point.X >= e.Min.X && point.X <= e.Max.X && point.Y >= e.Min.Y && point.Y <= e.Max.Y
                        let distance = Double.Hypot(cell.Center.X - point.X, cell.Center.Y - point.Y)
                        let size = max (e.Max.X - e.Min.X) (e.Max.Y - e.Min.Y)
                        if not contains && Double.IsFinite distance && size < config.Theta * distance && (not bounded || far point e <= config.MaxDistance) then
                            addFallback i cell.Center.X cell.Center.Y cell.Mass i
                            false
                        elif not cell.Items.IsEmpty then
                            cell.Items |> List.iteri(fun index item ->
                                check token index
                                if item.Value <> i then addFallback i item.Position.X item.Position.Y item.Mass item.Value)
                            false
                        else true) tree

    let collision (token: System.Threading.CancellationToken) (config: CollisionConfig) (px: float[]) (py: float[]) (predicted: Point[]) (nodeIds: int[]) (masses: float[]) (radii: float[]) (pinned: bool[]) maxCoordAbs jitter (vx: float[]) (vy: float[]) =
        let n = px.Length
        if config.Strength > 0. && n > 0 then
            let mutable maxRadius = radii[0]
            for i = 1 to n - 1 do
                if radii[i] > maxRadius then maxRadius <- radii[i]
            for _ = 1 to config.Iterations do
                let mutable predMaxAbs = maxRadius
                for i = 0 to n - 1 do
                    check token i
                    if pinned[i] then
                        let pxi, pyi = px[i], py[i]
                        predicted[i] <- { Point.X = pxi; Y = pyi }
                        let m = max (abs pxi) (abs pyi)
                        if m > predMaxAbs then predMaxAbs <- m
                    else
                        let pxi = px[i] + vx[i]
                        let pyi = py[i] + vy[i]
                        Check.finite "x" pxi
                        Check.finite "y" pyi
                        predicted[i] <- { Point.X = pxi; Y = pyi }
                        let m = max (abs pxi) (abs pyi)
                        if m > predMaxAbs then predMaxAbs <- m
                let _ = maxCoordAbs
                let fastSq = predMaxAbs < 1e140
                let tree = Quadtree.buildWithCancellation token (fun i -> predicted[i]) (fun _ -> 1.) nodeIds
                if fastSq then
                    Quadtree.visitIndexed n (fun i cell ->
                        token.ThrowIfCancellationRequested()
                        let pi = predicted[i]
                        let ri = radii[i]
                        let limitR = ri + maxRadius
                        if nearSq pi.X pi.Y cell.Extent > limitR * limitR then false
                        else
                            let mutable idx = 0
                            for item in cell.Items do
                                check token idx
                                idx <- idx + 1
                                let j = item.Value
                                if j > i && not (pinned[i] && pinned[j]) then
                                    let pj = predicted[j]
                                    let dx = pj.X - pi.X
                                    let dy = pj.Y - pi.Y
                                    let sumR = ri + radii[j]
                                    let d2 = dx * dx + dy * dy
                                    if d2 < sumR * sumR then
                                        let distance = sqrt d2
                                        let overlap = sumR - distance
                                        if overlap > 0. then
                                            let ux, uy = if distance = 0. then jitter i j else dx / distance, dy / distance
                                            let mi, mj = masses[i], masses[j]
                                            let scale = max mi mj
                                            let a, b = mi / scale, mj / scale
                                            let shareA, shareB =
                                                if pinned[i] then 0., 1.
                                                elif pinned[j] then 1., 0.
                                                else b / (a + b), a / (a + b)
                                            let correction = overlap * config.Strength
                                            vx[i] <- vx[i] - ux * correction * shareA
                                            vy[i] <- vy[i] - uy * correction * shareA
                                            vx[j] <- vx[j] + ux * correction * shareB
                                            vy[j] <- vy[j] + uy * correction * shareB
                            true) tree
                else
                    Quadtree.visitIndexed n (fun i cell ->
                        token.ThrowIfCancellationRequested()
                        if near predicted[i] cell.Extent > radii[i] + maxRadius then false else
                        cell.Items |> List.iteri(fun index item ->
                            check token index
                            let j = item.Value
                            if j > i && not (pinned[i] && pinned[j]) then
                                let dx, dy = predicted[j].X - predicted[i].X, predicted[j].Y - predicted[i].Y
                                let distance = Double.Hypot(dx, dy)
                                Check.finite "collision distance" distance
                                let overlap = radii[i] + radii[j] - distance
                                if overlap > 0. then
                                    let ux, uy = if distance = 0. then jitter i j else dx / distance, dy / distance
                                    let scale = max masses[i] masses[j]
                                    let a, b = masses[i] / scale, masses[j] / scale
                                    let shareA, shareB = if pinned[i] then 0., 1. elif pinned[j] then 1., 0. else b / (a + b), a / (a + b)
                                    let correction = overlap * config.Strength
                                    vx[i] <- vx[i] - ux * correction * shareA; vy[i] <- vy[i] - uy * correction * shareA
                                    vx[j] <- vx[j] + ux * correction * shareB; vy[j] <- vy[j] + uy * correction * shareB)
                        true) tree

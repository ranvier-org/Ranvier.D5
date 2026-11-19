module Ranvier.D5.Tests.SpatialAndOrganisationTests

open System
open Expecto
open Ranvier.D5
open Ranvier.D5.Charts
open Ranvier.D5.Geo

let private ok =
    function
    | Ok v -> v
    | Error errs -> failwithf "Unexpected error: %A" errs

let private viewport = Bounds.create 0. 0. 400. 300.
let private cube = Bounds3.create 0. 0. 0. 100. 100. 100.

let private boxArea (b: Bounds) = Bounds.width b * Bounds.height b

let private boxContainsBox (tol: float) (outer: Bounds) (inner: Bounds) =
    Bounds.x inner >= Bounds.x outer - tol
    && Bounds.y inner >= Bounds.y outer - tol
    && Bounds.x inner + Bounds.width inner <= Bounds.x outer + Bounds.width outer + tol
    && Bounds.y inner + Bounds.height inner <= Bounds.y outer + Bounds.height outer + tol

let private boxesIntersect2D (a: Bounds) (b: Bounds) =
    let ax0, ay0 = Bounds.x a, Bounds.y a
    let ax1, ay1 = ax0 + Bounds.width a, ay0 + Bounds.height a
    let bx0, by0 = Bounds.x b, Bounds.y b
    let bx1, by1 = bx0 + Bounds.width b, by0 + Bounds.height b
    ax1 >= bx0 && ax0 <= bx1 && ay1 >= by0 && ay0 <= by1

let private boxContainsBox2DExact (outer: Bounds) (inner: Bounds) =
    let ox0, oy0 = Bounds.x outer, Bounds.y outer
    let ox1, oy1 = ox0 + Bounds.width outer, oy0 + Bounds.height outer
    let ix0, iy0 = Bounds.x inner, Bounds.y inner
    let ix1, iy1 = ix0 + Bounds.width inner, iy0 + Bounds.height inner
    ix0 >= ox0 && ix1 <= ox1 && iy0 >= oy0 && iy1 <= oy1

let private boxDist2D (p: Point) (b: Bounds) =
    let x0, y0 = Bounds.x b, Bounds.y b
    let x1, y1 = x0 + Bounds.width b, y0 + Bounds.height b
    let dx = if p.X < x0 then x0 - p.X elif p.X > x1 then p.X - x1 else 0.
    let dy = if p.Y < y0 then y0 - p.Y elif p.Y > y1 then p.Y - y1 else 0.
    Double.Hypot(dx, dy)

let private boxesIntersect3D (a: Bounds3) (b: Bounds3) =
    Bounds3.maxX a >= Bounds3.minX b && Bounds3.minX a <= Bounds3.maxX b
    && Bounds3.maxY a >= Bounds3.minY b && Bounds3.minY a <= Bounds3.maxY b
    && Bounds3.maxZ a >= Bounds3.minZ b && Bounds3.minZ a <= Bounds3.maxZ b

let private boxContainsBox3DExact (outer: Bounds3) (inner: Bounds3) =
    Bounds3.minX inner >= Bounds3.minX outer && Bounds3.maxX inner <= Bounds3.maxX outer
    && Bounds3.minY inner >= Bounds3.minY outer && Bounds3.maxY inner <= Bounds3.maxY outer
    && Bounds3.minZ inner >= Bounds3.minZ outer && Bounds3.maxZ inner <= Bounds3.maxZ outer

let private boxDist3D (p: Point3) (b: Bounds3) =
    let x0, y0, z0 = Bounds3.minX b, Bounds3.minY b, Bounds3.minZ b
    let x1, y1, z1 = Bounds3.maxX b, Bounds3.maxY b, Bounds3.maxZ b
    let dx = if p.X < x0 then x0 - p.X elif p.X > x1 then p.X - x1 else 0.
    let dy = if p.Y < y0 then y0 - p.Y elif p.Y > y1 then p.Y - y1 else 0.
    let dz = if p.Z < z0 then z0 - p.Z elif p.Z > z1 then p.Z - z1 else 0.
    sqrt (dx * dx + dy * dy + dz * dz)

let private bruteSlabRay3D (origin: Point3) (dir: Point3) (maxT: float) (b: Bounds3) : float option =
    let mutable tEnter = 0.
    let mutable tExit = maxT
    let mutable hit = true
    let inline axisSlab o d lo hi =
        if hit then
            if d = 0. then
                if o < lo || o > hi then hit <- false
            else
                let t1 = (lo - o) / d
                let t2 = (hi - o) / d
                tEnter <- max tEnter (min t1 t2)
                tExit <- min tExit (max t1 t2)
    axisSlab origin.X dir.X (Bounds3.minX b) (Bounds3.maxX b)
    axisSlab origin.Y dir.Y (Bounds3.minY b) (Bounds3.maxY b)
    axisSlab origin.Z dir.Z (Bounds3.minZ b) (Bounds3.maxZ b)
    if hit && tEnter <= tExit then Some tEnter else None

let private cvtEnergy2D (cells: VoronoiCell list) (sites: Point array) : float =
    let mutable total = 0.
    for cell in cells do
        let verts = List.toArray cell.Vertices
        let m = verts.Length
        if m >= 3 then
            for idx in cell.InputIndices do
                let s = sites[idx]
                for k in 0 .. m - 1 do
                    let p = verts[k]
                    let q = verts[(k + 1) % m]
                    let ux, uy = p.X - s.X, p.Y - s.Y
                    let vx, vy = q.X - s.X, q.Y - s.Y
                    let absTri = 0.5 * abs (ux * vy - vx * uy)
                    let quad = (ux * ux + uy * uy) + (ux * vx + uy * vy) + (vx * vx + vy * vy)
                    total <- total + (absTri / 6.) * quad
    total

let private geoToUnitVec (pos: GeoPosition) : float * float * float =
    let lon = pos.Longitude * (Math.PI / 180.)
    let lat = pos.Latitude * (Math.PI / 180.)
    let cl = cos lat
    cl * cos lon, cl * sin lon, sin lat

let private pointInsideConvexPolygon (tol: float) (poly: Point list) (pt: Point) : bool =
    let arr = List.toArray poly
    let n = arr.Length
    if n < 3 then false
    else
        // Normalize orientation to positive signed area
        let mutable signed2 = 0.
        for i in 1 .. n - 2 do
            let ux, uy = arr[i].X - arr[0].X, arr[i].Y - arr[0].Y
            let vx, vy = arr[i + 1].X - arr[0].X, arr[i + 1].Y - arr[0].Y
            signed2 <- signed2 + (ux * vy - uy * vx)
        let verts = if signed2 >= 0. then arr else Array.rev arr
        let mutable ok = true
        for i in 0 .. n - 1 do
            let a = verts[i]
            let b = verts[(i + 1) % n]
            let ex, ey = b.X - a.X, b.Y - a.Y
            let len = max 1. (Double.Hypot(ex, ey))
            let cross = ex * (pt.Y - a.Y) - ey * (pt.X - a.X)
            if cross < -tol * len then ok <- false
        ok

let tests =
    testList "SpatialAndOrganisation" [
        testCase "Morton2D, Morton3D, Hilbert2D, Moore2D and Hilbert3D satisfy bijections, unit lattice steps and stable ordering" <| fun _ ->
            // Morton2D bijection on [0..255] x [0..255]
            let seen2D = Array.zeroCreate<bool> 65536
            for x in 0us .. 255us do
                for y in 0us .. 255us do
                    let code = Morton2D.encode x y
                    Expect.isTrue (code < 65536u) "Morton2D [0..255]^2 fits in 16 bits"
                    Expect.isFalse (seen2D[int code]) "Morton2D injective"
                    seen2D[int code] <- true
                    Expect.equal (Morton2D.decode code) (x, y) "Morton2D decode(encode(x,y)) = (x,y)"

            // Morton3D bijection on [0..15]^3
            let seen3D = Array.zeroCreate<bool> 4096
            for x in 0us .. 15us do
                for y in 0us .. 15us do
                    for z in 0us .. 15us do
                        let code = Morton3D.encode x y z
                        Expect.isTrue (code < 4096u) "Morton3D [0..15]^3 fits in 12 bits"
                        Expect.isFalse (seen3D[int code]) "Morton3D injective"
                        seen3D[int code] <- true
                        Expect.equal (Morton3D.decode code) (x, y, z) "Morton3D decode(encode(x,y,z)) = (x,y,z)"
            Expect.equal (Morton3D.decode (Morton3D.encode 1023us 511us 777us)) (1023us, 511us, 777us) "Morton3D 10-bit max"

            // Hilbert2D & Moore2D bijection + unit Manhattan step for order = 4 (16 x 16 = 256)
            for x in 0 .. 15 do
                for y in 0 .. 15 do
                    let hIdx = Hilbert2D.encodeOrder 4 x y
                    Expect.equal (Hilbert2D.decodeOrder 4 hIdx) (x, y) "Hilbert2D order-4 bijection"
                    let mIdx = Moore2D.encodeOrder 4 x y
                    Expect.equal (Moore2D.decodeOrder 4 mIdx) (x, y) "Moore2D order-4 bijection"

            for idx in 0u .. 254u do
                let hx0, hy0 = Hilbert2D.decodeOrder 4 idx
                let hx1, hy1 = Hilbert2D.decodeOrder 4 (idx + 1u)
                Expect.equal (abs (hx1 - hx0) + abs (hy1 - hy0)) 1 "Hilbert2D consecutive Manhattan step = 1"

                let mx0, my0 = Moore2D.decodeOrder 4 idx
                let mx1, my1 = Moore2D.decodeOrder 4 (idx + 1u)
                Expect.equal (abs (mx1 - mx0) + abs (my1 - my0)) 1 "Moore2D consecutive Manhattan step = 1"

            // Closed-loop endpoint adjacency for Moore2D
            let mStartX, mStartY = Moore2D.decodeOrder 4 0u
            let mEndX, mEndY = Moore2D.decodeOrder 4 255u
            Expect.equal (abs (mEndX - mStartX) + abs (mEndY - mStartY)) 1 "Moore2D closed-loop start/end lattice adjacency"

            // Hilbert3D bijection + unit 3D Manhattan step for order = 3 (8 x 8 x 8 = 512)
            for x in 0 .. 7 do
                for y in 0 .. 7 do
                    for z in 0 .. 7 do
                        let h3 = Hilbert3D.encodeOrder 3 x y z
                        Expect.equal (Hilbert3D.decodeOrder 3 h3) (x, y, z) "Hilbert3D order-3 bijection"

            for idx in 0u .. 510u do
                let x0, y0, z0 = Hilbert3D.decodeOrder 3 idx
                let x1, y1, z1 = Hilbert3D.decodeOrder 3 (idx + 1u)
                Expect.equal (abs (x1 - x0) + abs (y1 - y0) + abs (z1 - z0)) 1 "Hilbert3D consecutive 3D Manhattan step = 1"

            // Stable orderBy tie-breaking + curve lengths
            let dup2D = [ ("first", Point.create 50. 50.); ("second", Point.create 50. 50.); ("origin", Point.create 1. 1.); ("third", Point.create 50. 50.) ]
            Expect.equal (Morton2D.orderBy viewport snd dup2D |> List.map fst) [ "origin"; "first"; "second"; "third" ] "Morton2D.orderBy stability"
            Expect.equal (Hilbert2D.orderBy viewport snd dup2D |> List.map fst) [ "origin"; "first"; "second"; "third" ] "Hilbert2D.orderBy stability"
            let mooreTies = Moore2D.orderBy viewport snd [ ("a", Point.create 50. 50.); ("b", Point.create 50. 50.); ("c", Point.create 50. 50.) ] |> List.map fst
            Expect.equal mooreTies [ "a"; "b"; "c" ] "Moore2D.orderBy tie stability"

            let dup3D = [ ("p1", Point3.create 50. 50. 50.); ("p2", Point3.create 50. 50. 50.); ("p0", Point3.create 1. 1. 1.); ("p3", Point3.create 50. 50. 50.) ]
            Expect.equal (Morton3D.orderBy cube snd dup3D |> List.map fst) [ "p0"; "p1"; "p2"; "p3" ] "Morton3D.orderBy stability"
            Expect.equal (Hilbert3D.orderBy cube snd dup3D |> List.map fst) [ "p0"; "p1"; "p2"; "p3" ] "Hilbert3D.orderBy stability"

            let hCurve2 = Hilbert2D.curve 4 viewport
            let mCurve2 = Moore2D.curve 4 viewport
            let hCurve3 = Hilbert3D.curve 3 cube
            Expect.equal hCurve2.Length 256 "Hilbert2D.curve order 4 length"
            Expect.equal mCurve2.Length 256 "Moore2D.curve order 4 length"
            Expect.equal hCurve3.Length 512 "Hilbert3D.curve order 3 length"
            Expect.isTrue (hCurve2 |> List.forall (fun p -> Bounds.contains p viewport)) "Hilbert2D.curve inside bounds"
            Expect.isTrue (hCurve3 |> List.forall (fun p -> Bounds3.contains p cube)) "Hilbert3D.curve inside bounds3"

        testCase "RTree2D, RTree3D, Bvh3D and Spatial3D match brute-force reference queries on 150 items plus empty and singleton cases" <| fun _ ->
            // Empty & singleton edge cases
            let emptyR2 = RTree2D.build snd ([] : (int * Bounds) list)
            Expect.equal (RTree2D.count emptyR2) 0 "empty RTree2D count"
            Expect.isNone (RTree2D.bounds emptyR2) "empty RTree2D bounds"
            Expect.isEmpty (RTree2D.search viewport emptyR2) "empty RTree2D search"
            Expect.isNone (RTree2D.nearest (Point.create 0. 0.) infinity emptyR2) "empty RTree2D nearest"

            let singleBox2 = Bounds.create 10. 20. 5. 6.
            let singleR2 = RTree2D.build snd [ (7, singleBox2) ]
            Expect.equal (RTree2D.search viewport singleR2 |> List.map fst) [ 7 ] "singleton RTree2D search"
            Expect.equal (RTree2D.nearest (Point.create 0. 0.) infinity singleR2 |> Option.map fst) (Some 7) "singleton RTree2D nearest"

            let emptyBvh = Bvh3D.build snd ([] : (int * Bounds3) list)
            Expect.equal (Bvh3D.count emptyBvh) 0 "empty Bvh3D count"
            Expect.isEmpty (Bvh3D.raycast (Point3.create 0. 0. 0.) (Point3.create 1. 0. 0.) 100. emptyBvh) "empty Bvh3D raycast"
            Expect.isNone (Bvh3D.raycastClosest (Point3.create 0. 0. 0.) (Point3.create 1. 0. 0.) 100. emptyBvh) "empty Bvh3D raycastClosest"

            let singleBox3 = Bounds3.create 10. -5. -5. 8. 10. 10.
            let singleBvh = Bvh3D.build snd [ (42, singleBox3) ]
            Expect.equal (Bvh3D.raycastClosest (Point3.create 0. 0. 0.) (Point3.create 1. 0. 0.) 100. singleBvh) (Some(42, 10.) |> Option.map (fun (id, t) -> (id, singleBox3), t)) "singleton Bvh3D raycastClosest"

            let emptySp3 = Spatial3D.create snd ([] : (int * Point3) list)
            Expect.equal (Spatial3D.count emptySp3) 0 "empty Spatial3D count"
            Expect.isNone (Spatial3D.nearest (Point3.create 0. 0. 0.) infinity emptySp3) "empty Spatial3D nearest"
            let singleSp3 = Spatial3D.create snd [ (9, Point3.create 3. 4. 5.) ]
            Expect.equal (Spatial3D.nearest (Point3.create 0. 0. 0.) 10. singleSp3 |> Option.map fst) (Some 9) "singleton Spatial3D nearest"

            // 150 deterministic 2D items + 80 join items
            let items2D =
                [| for i in 0 .. 149 ->
                    let x = sin (float i * 1.73 + 0.2) * 180. + 200.
                    let y = cos (float i * 2.19 + 0.5) * 130. + 150.
                    let w = 6. + float ((i * 7) % 19)
                    let h = 5. + float ((i * 11) % 17)
                    i, Bounds.create x y w h |]
            let right2D =
                [| for j in 0 .. 79 ->
                    let x = cos (float j * 1.31) * 170. + 200.
                    let y = sin (float j * 1.97) * 120. + 150.
                    j, Bounds.create x y 14. 12. |]

            let rt2 = RTree2D.build snd items2D
            let rt2Right = RTree2D.build snd right2D
            Expect.equal (RTree2D.count rt2) 150 "RTree2D 150 count"

            for qx, qy, qw, qh in [ (80., 60., 120., 90.); (190., 110., 150., 130.); (0., 0., 50., 50.); (350., 250., 80., 60.) ] do
                let q = Bounds.create qx qy qw qh
                let expSearch = items2D |> Array.filter (fun (_, b) -> boxesIntersect2D b q) |> Array.map fst |> Array.toList
                let expWithin = items2D |> Array.filter (fun (_, b) -> boxContainsBox2DExact q b) |> Array.map fst |> Array.toList
                Expect.equal (RTree2D.search q rt2 |> List.map fst) expSearch "RTree2D.search matches brute force"
                Expect.equal (RTree2D.within q rt2 |> List.map fst) expWithin "RTree2D.within matches brute force"

            for px, py, k, maxD in [ (210., 145., 8, infinity); (95., 80., 12, 90.); (380., 280., 5, 45.); (-500., -500., 5, 20.) ] do
                let pt = Point.create px py
                let expKnn =
                    items2D
                    |> Array.choose (fun (idx, b) ->
                        let d = boxDist2D pt b
                        if d <= maxD then Some(d, idx) else None)
                    |> Array.sort
                    |> Array.truncate k
                    |> Array.map snd
                    |> Array.toList
                Expect.equal (RTree2D.kNearest k pt maxD rt2 |> List.map fst) expKnn "RTree2D.kNearest matches brute force"
                Expect.equal (RTree2D.nearest pt maxD rt2 |> Option.map fst) (List.tryHead expKnn) "RTree2D.nearest matches brute force"

            let expJoin2 =
                [ for i in 0 .. items2D.Length - 1 do
                    for j in 0 .. right2D.Length - 1 do
                        if boxesIntersect2D (snd (items2D[i])) (snd (right2D[j])) then
                            yield fst (items2D[i]), fst (right2D[j]) ]
            let actJoin2 = RTree2D.spatialJoin rt2 rt2Right |> List.map (fun ((ia, _), (ib, _)) -> ia, ib)
            Expect.equal actJoin2 expJoin2 "RTree2D.spatialJoin matches brute force"

            // 150 deterministic 3D items + 80 join items
            let items3D =
                [| for i in 0 .. 149 ->
                    let x = sin (float i * 1.41 + 0.3) * 90.
                    let y = cos (float i * 1.87 + 0.7) * 90.
                    let z = sin (float i * 2.53 + 1.1) * 90.
                    let sx = 8. + float ((i * 5) % 15)
                    let sy = 7. + float ((i * 9) % 13)
                    let sz = 6. + float ((i * 13) % 17)
                    i, Bounds3.create x y z sx sy sz |]
            let right3D =
                [| for j in 0 .. 79 ->
                    let x = cos (float j * 1.63) * 85.
                    let y = sin (float j * 2.09) * 85.
                    let z = cos (float j * 0.93 + 0.4) * 85.
                    j, Bounds3.create x y z 15. 14. 16. |]

            let rt3 = RTree3D.build snd items3D
            let rt3Right = RTree3D.build snd right3D
            let bvh3 = Bvh3D.build snd items3D

            for qx, qy, qz, s in [ (-60., -50., -40., 95.); (10., -20., -30., 80.); (-200., -200., -200., 30.) ] do
                let q3 = Bounds3.create qx qy qz s s s
                let expS3 = items3D |> Array.filter (fun (_, b) -> boxesIntersect3D b q3) |> Array.map fst |> Array.toList
                let expW3 = items3D |> Array.filter (fun (_, b) -> boxContainsBox3DExact q3 b) |> Array.map fst |> Array.toList
                Expect.equal (RTree3D.search q3 rt3 |> List.map fst) expS3 "RTree3D.search matches brute force"
                Expect.equal (Bvh3D.search q3 bvh3 |> List.map fst) expS3 "Bvh3D.search matches brute force"
                Expect.equal (RTree3D.within q3 rt3 |> List.map fst) expW3 "RTree3D.within matches brute force"

            for px, py, pz, k, maxD in [ (12., -18., 25., 10, infinity); (-75., 60., -40., 6, 65.); (500., 500., 500., 4, 30.) ] do
                let pt3 = Point3.create px py pz
                let expKnn3 =
                    items3D
                    |> Array.choose (fun (idx, b) ->
                        let d = boxDist3D pt3 b
                        if d <= maxD then Some(d, idx) else None)
                    |> Array.sort
                    |> Array.truncate k
                    |> Array.map snd
                    |> Array.toList
                Expect.equal (RTree3D.kNearest k pt3 maxD rt3 |> List.map fst) expKnn3 "RTree3D.kNearest matches brute force"
                Expect.equal (RTree3D.nearest pt3 maxD rt3 |> Option.map fst) (List.tryHead expKnn3) "RTree3D.nearest matches brute force"

            let expJoin3 =
                [ for i in 0 .. items3D.Length - 1 do
                    for j in 0 .. right3D.Length - 1 do
                        if boxesIntersect3D (snd (items3D[i])) (snd (right3D[j])) then
                            yield fst (items3D[i]), fst (right3D[j]) ]
            let actJoin3 = RTree3D.spatialJoin rt3 rt3Right |> List.map (fun ((ia, _), (ib, _)) -> ia, ib)
            Expect.equal actJoin3 expJoin3 "RTree3D.spatialJoin matches brute force"

            // Raycast & raycastClosest across 16 oblique and axis-parallel rays
            for r in 0 .. 15 do
                let origin = Point3.create (-140. + float r * 11.) (-90. + float ((r * 17) % 160)) (-110. + float ((r * 23) % 180))
                let dir =
                    if r % 4 = 0 then Point3.create 1. 0. 0.
                    elif r % 4 = 1 then Point3.create 1. 0.35 0.25
                    elif r % 4 = 2 then Point3.create 0.8 -0.45 0.55
                    else Point3.create 0. 0.6 0.8
                let maxT = if r % 3 = 0 then 120. else 500.
                let expHits =
                    items3D
                    |> Array.choose (fun (idx, b) -> bruteSlabRay3D origin dir maxT b |> Option.map (fun t -> t, idx))
                    |> Array.sort
                    |> Array.toList
                let rtHits = RTree3D.raycast origin dir maxT rt3 |> List.map (fun ((idx, _), t) -> t, idx)
                let bvhHits = Bvh3D.raycast origin dir maxT bvh3 |> List.map (fun ((idx, _), t) -> t, idx)
                Expect.equal (rtHits |> List.map snd) (expHits |> List.map snd) "RTree3D.raycast hit order matches brute force"
                Expect.equal (bvhHits |> List.map snd) (expHits |> List.map snd) "Bvh3D.raycast hit order matches brute force"
                let closest = Bvh3D.raycastClosest origin dir maxT bvh3 |> Option.map (fun ((idx, _), t) -> t, idx)
                match closest, List.tryHead expHits with
                | None, None -> ()
                | Some(tc, ic), Some(te, ie) ->
                    Expect.equal ic ie "Bvh3D.raycastClosest closest index"
                    Expect.isTrue (abs (tc - te) < 1e-10) "Bvh3D.raycastClosest tEnter"
                | a, b -> failwithf "Raycast closest mismatch: %A vs %A" a b

            // Spatial3D point k-d tree against brute force on 150 3D points
            let pts3D =
                [| for i in 0 .. 149 ->
                    let p = Point3.create (sin (float i * 1.29) * 100.) (cos (float i * 2.03) * 100.) (sin (float i * 0.77 + 0.9) * 100.)
                    i, p |]
            let sp3 = Spatial3D.create snd pts3D
            let spBox = Bounds3.create -45. -35. -55. 95. 85. 110.
            let expSpWithin = pts3D |> Array.filter (fun (_, p) -> Bounds3.contains p spBox) |> Array.map fst |> Array.toList
            Expect.equal (Spatial3D.within spBox sp3 |> List.map fst) expSpWithin "Spatial3D.within matches brute force"

            let center3 = Point3.create 15. -10. 20.
            let rad3 = 65.
            let expSpRad = pts3D |> Array.filter (fun (_, p) -> Point3.distance p center3 <= rad3) |> Array.map fst |> Array.toList
            Expect.equal (Spatial3D.withinRadius center3 rad3 sp3 |> List.map fst) expSpRad "Spatial3D.withinRadius matches brute force"

            let expSpKnn =
                pts3D
                |> Array.choose (fun (idx, p) ->
                    let d = Point3.distance p center3
                    if d <= 90. then Some(d, idx) else None)
                |> Array.sort
                |> Array.truncate 12
                |> Array.map snd
                |> Array.toList
            Expect.equal (Spatial3D.kNearest 12 center3 90. sp3 |> List.map fst) expSpKnn "Spatial3D.kNearest matches brute force"
            Expect.equal (Spatial3D.nearest center3 90. sp3 |> Option.map fst) (List.tryHead expSpKnn) "Spatial3D.nearest matches brute force"

        testCase "Seriation, AdjacencyMatrix orderings and Dbscan2D/3D recover structure, reduce bandwidth and isolate noise" <| fun _ ->
            // 1. Reverse Cuthill-McKee bandwidth reduction on a shuffled 5x5 grid (25 nodes)
            let gridId r c = sprintf "g_%d_%d" r c
            let canonicalGrid = [ for r in 0 .. 4 do for c in 0 .. 4 -> gridId r c ]
            let shuffledGrid =
                canonicalGrid
                |> List.mapi (fun idx id -> ((idx * 17 + 11) % 25), id)
                |> List.sortBy fst
                |> List.map snd
            let gridEdges =
                [ for r in 0 .. 4 do
                    for c in 0 .. 4 do
                        if c + 1 < 5 then yield gridId r c, gridId r (c + 1), 1.
                        if r + 1 < 5 then yield gridId r c, gridId (r + 1) c, 1. ]
            let bandwidth (order: string list) =
                let pos = order |> List.mapi (fun i id -> id, i) |> Map.ofList
                gridEdges |> List.fold (fun m (u, v, _) -> max m (abs (pos[u] - pos[v]))) 0
            let initBw = bandwidth shuffledGrid
            let rcmOrder = Seriation.reverseCuthillMcKee shuffledGrid gridEdges |> ok
            let rcmBw = bandwidth rcmOrder
            Expect.equal rcmOrder.Length 25 "RCM preserves all 25 grid vertices"
            Expect.isTrue (rcmBw < initBw) (sprintf "RCM strictly reduces bandwidth (%d < %d)" rcmBw initBw)
            Expect.isTrue (rcmBw <= 8) "RCM achieves compact grid bandwidth"

            // 2. Spectral Fiedler recovers monotonic ordering on a 12-vertex path Laplacian
            let pathCanonical = [ for i in 0 .. 11 -> sprintf "p%02d" i ]
            let pathShuffled = [ "p07"; "p02"; "p11"; "p00"; "p09"; "p04"; "p01"; "p10"; "p05"; "p08"; "p03"; "p06" ]
            let pathEdges = [ for i in 0 .. 10 -> sprintf "p%02d" i, sprintf "p%02d" (i + 1), 1. ]
            let fiedlerOrder = Seriation.spectralFiedler pathShuffled pathEdges |> ok
            Expect.isTrue
                (fiedlerOrder = pathCanonical || fiedlerOrder = List.rev pathCanonical)
                (sprintf "Fiedler recovers exact path order: %A" fiedlerOrder)

            // 3. Hierarchical clustering (Single, Complete, Average, Ward) + Optimal Leaf Ordering + n=1,2 edge cases
            Expect.equal (Seriation.hierarchicalCluster Linkage.Ward ([] : string list) (fun _ _ -> 0.) |> ok) None "n=0 dendrogram"
            let tree1 = Seriation.hierarchicalCluster Linkage.Ward [ "only" ] (fun _ _ -> 0.) |> ok |> Option.get
            Expect.equal (Seriation.optimalLeafOrder tree1 (fun _ _ -> 0.)) [ "only" ] "n=1 OLO"
            let tree2 = Seriation.hierarchicalCluster Linkage.Complete [ "x"; "y" ] (fun a b -> if a = b then 0. else 3.) |> ok |> Option.get
            Expect.equal (Seriation.optimalLeafOrder tree2 (fun a b -> if a = b then 0. else 3.) |> Set.ofList) (Set.ofList [ "x"; "y" ]) "n=2 OLO"

            let pts1D =
                [ ("i0", 0.); ("i1", 100.); ("i2", 2.); ("i3", 103.); ("i4", 5.); ("i5", 98.)
                  ("i6", 50.); ("i7", 52.); ("i8", 48.); ("i9", 7.); ("i10", 105.); ("i11", 54.) ]
            let valMap = Map.ofList pts1D
            let dist1D a b = abs (valMap[a] - valMap[b])
            let ids1D = List.map fst pts1D
            let rec rawLeaves = function
                | Dendrogram.Leaf x -> [ x ]
                | Dendrogram.Merge(l, r, _, _) -> rawLeaves l @ rawLeaves r
            let pathCost (ord: string list) =
                ord |> List.pairwise |> List.sumBy (fun (a, b) -> dist1D a b)

            for linkage in [ Linkage.Single; Linkage.Complete; Linkage.Average; Linkage.Ward ] do
                let dTree = Seriation.hierarchicalCluster linkage ids1D dist1D |> ok |> Option.get
                let unopt = rawLeaves dTree
                let opt = Seriation.optimalLeafOrder dTree dist1D
                Expect.equal (Set.ofList opt) (Set.ofList ids1D) "OLO is a permutation"
                Expect.isTrue (pathCost opt <= pathCost unopt + 1e-9) (sprintf "OLO improves/matches leaf path for %A" linkage)

            // 4. Barycenter & Bicluster on shuffled two-block bipartite matrix
            let rows = [ "rA1"; "rB1"; "rA2"; "rB2"; "rA3"; "rB3" ]
            let cols = [ "cB1"; "cA1"; "cB2"; "cA2"; "cB3"; "cA3" ]
            let entries =
                [ for r in [ "rA1"; "rA2"; "rA3" ] do
                    for c in [ "cA1"; "cA2"; "cA3" ] -> r, c, 5.
                  for r in [ "rB1"; "rB2"; "rB3" ] do
                    for c in [ "cB1"; "cB2"; "cB3" ] -> r, c, 5. ]
            let isContiguousBlock (ord: string list) (prefix: string) =
                let indices = ord |> List.mapi (fun idx s -> idx, s) |> List.choose (fun (idx, s) -> if s.StartsWith prefix then Some idx else None)
                List.max indices - List.min indices = indices.Length - 1
            let baryR, baryC = Seriation.barycenterOrder 12 rows cols entries |> ok
            Expect.isTrue (isContiguousBlock baryR "rA" && isContiguousBlock baryC "cA") "barycenterOrder groups blocks"
            let biR, biC = Seriation.bicluster rows cols entries |> ok
            Expect.isTrue (isContiguousBlock biR "rA" && isContiguousBlock biC "cA") "bicluster groups blocks"

            // 5. AdjacencyMatrix chart with all 4 Wave 2 MatrixOrder variants
            let frame = ChartFrame.create 420. 420. "Matrix Seriation"
            let mNodes = [ for id in pathShuffled -> id, id.ToUpperInvariant() ]
            let mLinks = pathEdges |> List.mapi (fun idx (u, v, w) -> sprintf "e%d" idx, u, v, w)
            for ord in [ MatrixOrder.ReverseCuthillMcKee; MatrixOrder.SpectralFiedler; MatrixOrder.OptimalLeafOrder; MatrixOrder.Barycenter ] do
                let outUndir =
                    Chart.adjacencyMatrix
                        { Frame = frame
                          Nodes = mNodes
                          Links = mLinks
                          Directed = false
                          Order = ord
                          Color = fun t -> Paint.rgb (int (200. * t)) 60 120 }
                    |> ok
                Expect.isTrue (Scene.isComposed outUndir.Scene) "AdjacencyMatrix composed"
                Expect.equal outUndir.HitTargets.Length (mLinks.Length * 2) "undirected cells"

            // 6. Dbscan2D separating two concentric non-convex rings + noise outliers
            let ringPts =
                [ for k in 0 .. 35 ->
                    let a = 2. * Math.PI * float k / 36.
                    sprintf "inner_%d" k, Point.create (10. * cos a) (10. * sin a)
                  for k in 0 .. 35 ->
                    let a = 2. * Math.PI * float k / 36.
                    sprintf "outer_%d" k, Point.create (40. * cos a) (40. * sin a)
                  yield "noise_1", Point.create 200. 200.
                  yield "noise_2", Point.create -200. -180. ]
            let db2 = Dbscan2D.cluster 8.0 3 fst snd ringPts |> ok
            let db2Map = db2 |> List.map (fun a -> a.Id, a) |> Map.ofList
            let innerCid = db2Map["inner_0"].ClusterId
            let outerCid = db2Map["outer_0"].ClusterId
            Expect.isSome innerCid "inner ring forms cluster"
            Expect.isSome outerCid "outer ring forms cluster"
            Expect.notEqual innerCid outerCid "concentric rings separated into distinct clusters"
            for k in 0 .. 35 do
                Expect.equal (db2Map[sprintf "inner_%d" k].ClusterId) innerCid "all inner ring members match"
                Expect.equal (db2Map[sprintf "outer_%d" k].ClusterId) outerCid "all outer ring members match"
            Expect.isNone (db2Map["noise_1"].ClusterId) "2D outlier 1 is noise"
            Expect.isNone (db2Map["noise_2"].ClusterId) "2D outlier 2 is noise"

            // 7. Dbscan3D separating two 3D clusters + noise outliers
            let cl3Pts =
                [ for k in 0 .. 15 ->
                    let a = float k * 1.1
                    sprintf "cA_%d" k, Point3.create (-50. + 3. * cos a) (3. * sin a) (float (k % 4))
                  for k in 0 .. 15 ->
                    let a = float k * 1.1
                    sprintf "cB_%d" k, Point3.create (50. + 3. * cos a) (3. * sin a) (float (k % 4))
                  yield "out3_1", Point3.create 0. 250. 0.
                  yield "out3_2", Point3.create 0. -250. 200. ]
            let db3 = Dbscan3D.cluster 8.0 4 fst snd cl3Pts |> ok
            let db3Map = db3 |> List.map (fun a -> a.Id, a) |> Map.ofList
            Expect.isSome (db3Map["cA_0"].ClusterId) "3D cluster A detected"
            Expect.isSome (db3Map["cB_0"].ClusterId) "3D cluster B detected"
            Expect.notEqual (db3Map["cA_0"].ClusterId) (db3Map["cB_0"].ClusterId) "3D clusters distinct"
            Expect.isNone (db3Map["out3_1"].ClusterId) "3D outlier 1 is noise"
            Expect.isNone (db3Map["out3_2"].ClusterId) "3D outlier 2 is noise"

        testCase "PowerDiagram, VoronoiTreemap, Cvt2D, Voronoi3D and Cvt3D conserve area/volume and reduce CVT energy" <| fun _ ->
            // 1. PowerDiagram positive weights partition Bounds into convex polygons summing to Bounds area within 1e-5
            let pBounds = Bounds.create 0. 0. 200. 150.
            let pSites =
                [ for i in 0 .. 11 ->
                    let x = 25. + float (i % 4) * 50. + float ((i * 3) % 7)
                    let y = 25. + float (i / 4) * 50. + float ((i * 5) % 6)
                    let w = 10. + float ((i * 17) % 80)
                    { PowerSite.Id = sprintf "s%d" i; Position = Point.create x y; Weight = w } ]
            let pCells = PowerDiagram.clipBounds pBounds pSites |> ok
            let totalPowerArea = pCells |> List.sumBy (fun (c: PowerCell<string>) -> c.Area)
            Expect.isTrue (abs (totalPowerArea - 30000.) <= 1e-5) "PowerDiagram cell areas sum to Bounds area within 1e-5"
            for cell in pCells do
                if cell.Vertices.Length >= 3 then
                    for v in cell.Vertices do
                        Expect.isTrue (Bounds.contains v pBounds) "PowerCell vertex inside Bounds"
                    Expect.equal (Polygon.hull cell.Vertices).Length cell.Vertices.Length "PowerCell polygon is convex"

            // 2. VoronoiTreemap nested leaf polygon areas proportional to weights and nested inside parent polygons
            let vtRoot =
                Hierarchy.node "root" 0.
                    [ Hierarchy.node "branchA" 0.
                        [ Hierarchy.node "a1" 1. []
                          Hierarchy.node "a2" 2. []
                          Hierarchy.node "a3" 3. [] ]
                      Hierarchy.node "branchB" 0.
                        [ Hierarchy.node "b1" 2. []
                          Hierarchy.node "b2" 2. []
                          Hierarchy.node "b3" 4. [] ] ]
            let vtBounds = Bounds.create 0. 0. 200. 200.
            let vtCfg = { VoronoiTreemapConfig.create vtBounds with MaxIterations = 120; AreaTolerance = 1e-3 }
            let vtCells = VoronoiTreemap.layout vtCfg vtRoot |> ok
            let vtById = vtCells |> List.map (fun (c: VoronoiTreemapCell<string>) -> c.Id, c) |> Map.ofList
            let vtLeaves = [ "a1", 1.; "a2", 2.; "a3", 3.; "b1", 2.; "b2", 2.; "b3", 4. ]
            let leafAreaSum = vtLeaves |> List.sumBy (fun (id, _) -> vtById[id].Area)
            Expect.isTrue (abs (leafAreaSum - 40000.) <= 1e-2) "VoronoiTreemap leaves conserve root area"
            for id, w in vtLeaves do
                let cell = vtById[id]
                let expectedArea = (w / 14.) * 40000.
                Expect.isTrue (abs (cell.Area - expectedArea) / expectedArea < 0.03) (sprintf "VoronoiTreemap proportional area for %s" id)
                let parentPoly = vtById[cell.Parent.Value].Polygon
                for v in cell.Polygon do
                    Expect.isTrue (pointInsideConvexPolygon 1e-5 parentPoly v) (sprintf "leaf %s vertex inside parent polygon" id)

            // 3. Cvt2D Lloyd & Lbfgs strictly decrease CVT energy
            let cvtBounds = Bounds.create 0. 0. 100. 100.
            let clustered2D =
                [ for i in 0 .. 15 ->
                    Point.create (12. + float (i % 4) * 5.5 + float (i % 3)) (15. + float (i / 4) * 6.0) ]
            let cells0, sites0 = Cvt2D.relax CvtSolver.Lloyd 0 cvtBounds clustered2D |> ok
            let e0 = cvtEnergy2D cells0 (List.toArray sites0)
            for solver in [ CvtSolver.Lloyd; CvtSolver.Lbfgs ] do
                let cellsR, sitesR = Cvt2D.relax solver 15 cvtBounds clustered2D |> ok
                let eR = cvtEnergy2D cellsR (List.toArray sitesR)
                Expect.isTrue (eR < e0) (sprintf "Cvt2D %A decreases CVT energy (%f < %f)" solver eR e0)
                Expect.isTrue (abs ((cellsR |> List.sumBy (fun c -> Polygon.area c.Vertices)) - 10000.) <= 1e-5) "Cvt2D conserves area"

            // 4. Voronoi3D clipBounds & clipBoundsWeighted conserve Bounds3 volume within 1e-4
            let box3 = Bounds3.create 0. 0. 0. 10. 10. 10.
            let sites3 =
                [ for i in 0 .. 17 ->
                    let x = 1.5 + float (i % 3) * 3.2 + 0.17 * float (i % 5)
                    let y = 1.5 + float ((i / 3) % 3) * 3.1 + 0.13 * float (i % 4)
                    let z = 2.0 + float (i / 9) * 5.2 + 0.19 * float (i % 3)
                    Point3.create x y z ]
            let cells3Unweighted = Voronoi3D.clipBounds box3 sites3 |> ok
            let volUnweighted = cells3Unweighted |> List.sumBy (fun (c: PolyhedronCell3D) -> c.Volume)
            Expect.isTrue (abs (volUnweighted - 1000.) <= 1e-4) "Voronoi3D.clipBounds conserves volume within 1e-4"

            let weightedSites3 = sites3 |> List.mapi (fun i p -> p, 0.2 + 0.15 * float (i % 5))
            let cells3Weighted = Voronoi3D.clipBoundsWeighted box3 weightedSites3 |> ok
            let volWeighted = cells3Weighted |> List.sumBy (fun (c: PolyhedronCell3D) -> c.Volume)
            Expect.isTrue (abs (volWeighted - 1000.) <= 1e-4) "Voronoi3D.clipBoundsWeighted conserves volume within 1e-4"

            // 5. Cvt3D relaxation reduces site-to-centroid drift and conserves cuboid volume
            let clustered3D =
                [ for i in 0 .. 11 ->
                    Point3.create (1.5 + float (i % 3) * 1.1) (1.5 + float ((i / 3) % 2) * 1.2) (1.5 + float (i / 6) * 1.3) ]
            let init3Cells, init3Pts = Cvt3D.relax 0 box3 clustered3D |> ok
            let initDrift =
                List.zip init3Cells init3Pts
                |> List.sumBy (fun (c, p) -> Point3.distanceSquared c.Centroid p)
            let rel3Cells, rel3Pts = Cvt3D.relax 10 box3 clustered3D |> ok
            let relDrift =
                List.zip rel3Cells rel3Pts
                |> List.sumBy (fun (c, p) -> Point3.distanceSquared c.Centroid p)
            Expect.isTrue (relDrift < initDrift) "Cvt3D relaxation reduces centroidal residual"
            Expect.isTrue (abs ((rel3Cells |> List.sumBy (fun c -> c.Volume)) - 1000.) <= 1e-4) "Cvt3D conserves volume"

        testCase "Treemap Strip/Hilbert/Moore, evolveLocalMoves and GosperMap satisfy area, containment and closed-ring invariants" <| fun _ ->
            let hTree =
                Hierarchy.node "root" 0.
                    [ Hierarchy.node "grp1" 0.
                        [ for i in 1 .. 5 -> Hierarchy.node (sprintf "u%d" i) (float i * 2.) [] ]
                      Hierarchy.node "grp2" 0.
                        [ for i in 1 .. 5 -> Hierarchy.node (sprintf "v%d" i) (float (6 - i) * 1.5) [] ] ]
            let leafIds = Set.ofList [ for i in 1 .. 5 do yield sprintf "u%d" i; yield sprintf "v%d" i ]

            for tiling in [ TreemapTiling.Strip; TreemapTiling.Hilbert; TreemapTiling.Moore ] do
                let cfg =
                    { TreemapConfig.create viewport with
                        Tiling = tiling
                        SumPolicy = HierarchySumPolicy.LeavesOnly }
                let boxes = Treemap.layout cfg hTree |> ok
                for b in boxes do
                    Expect.isTrue (boxContainsBox 1e-6 viewport b.Bounds) (sprintf "%A node inside viewport" tiling)
                let leaves = boxes |> List.filter (fun b -> leafIds.Contains b.Id)
                let sumA = leaves |> List.sumBy (fun b -> boxArea b.Bounds)
                Expect.isTrue (abs (sumA - boxArea viewport) <= 1e-5) (sprintf "%A exact area conservation" tiling)

            // Treemap.evolveLocalMoves improves or preserves worst aspect ratio after dynamic weight change
            let baseCfg = { TreemapConfig.create viewport with SumPolicy = HierarchySumPolicy.LeavesOnly }
            let initTree =
                Hierarchy.node "r" 0.
                    [ for i in 1 .. 8 -> Hierarchy.node (sprintf "k%d" i) 1. [] ]
            let _, state0 = Treemap.resquarify baseCfg initTree None |> ok
            let skewedTree =
                Hierarchy.node "r" 0.
                    [ Hierarchy.node "k1" 25. []
                      Hierarchy.node "k2" 1. []
                      Hierarchy.node "k3" 18. []
                      Hierarchy.node "k4" 2. []
                      Hierarchy.node "k5" 14. []
                      Hierarchy.node "k6" 1. []
                      Hierarchy.node "k7" 9. []
                      Hierarchy.node "k8" 3. [] ]
            let retainedBoxes, _ = Treemap.resquarify baseCfg skewedTree (Some state0) |> ok
            let evolvedBoxes, _ = Treemap.evolveLocalMoves 5 baseCfg skewedTree state0 |> ok
            let worstAspect (bs: HierarchyBox<string> list) =
                bs
                |> List.tail
                |> List.fold (fun m b ->
                    let w, h = Bounds.width b.Bounds, Bounds.height b.Bounds
                    max m (max (w / h) (h / w))) 1.
            Expect.isTrue (abs ((evolvedBoxes |> List.tail |> List.sumBy (fun b -> boxArea b.Bounds)) - boxArea viewport) <= 1e-5) "evolveLocalMoves conserves area"
            Expect.isTrue (worstAspect evolvedBoxes <= worstAspect retainedBoxes + 1e-9) "evolveLocalMoves improves or matches retained worst aspect ratio"

            // GosperMap produces non-empty closed regions for both leaves and internal nodes
            let gCurve = GosperMap.curve 2 viewport |> ok
            Expect.equal gCurve.Length 49 "GosperMap order 2 has 49 hex centers"
            let gRegions = GosperMap.layout { GosperMapConfig.create viewport with Order = 3; SmoothIterations = 1 } hTree |> ok
            Expect.equal gRegions.Length 13 "GosperMap emits root + 2 internal branches + 10 leaves"
            for reg in gRegions do
                Expect.isNonEmpty reg.Boundary (sprintf "GosperRegion %s has closed boundary rings" reg.Id)
                Expect.isTrue (reg.Boundary |> List.forall (fun ring -> ring.Length >= 3)) "every boundary ring has >= 3 vertices"
                Expect.isTrue (reg.Area > 0.) (sprintf "GosperRegion %s has positive area" reg.Id)
            let rootReg = gRegions |> List.find (fun r -> r.Id = "root")
            let leafAreaGosper = gRegions |> List.filter (fun r -> leafIds.Contains r.Id) |> List.sumBy (fun r -> r.Area)
            Expect.isTrue (abs (leafAreaGosper - rootReg.Area) <= 1e-4) "GosperMap leaf areas sum to root island area"

        testCase "UpSet, EulerVenn, Chart.upset and Chart.euler satisfy set-algebraic and area-proportional invariants" <| fun _ ->
            let sets =
                [ "Python", [ 1; 2; 3; 4; 5; 6 ]
                  "FSharp", [ 4; 5; 6; 7; 8 ]
                  "Rust", [ 5; 6; 8; 9; 10 ]
                  "SQL", [ 1; 4; 6; 10; 11 ] ]
            let excl = UpSet.compute UpSetMode.Exclusive UpSetSort.ByCardinality 100 sets |> ok
            Expect.equal excl.TotalItems 11 "union cardinality = 11"
            Expect.equal (excl.Intersections |> List.sumBy (fun i -> i.Count)) excl.TotalItems "Exclusive intersection counts partition the union"

            let incl = UpSet.compute UpSetMode.Inclusive UpSetSort.ByDegree 100 sets |> ok
            for a in incl.Intersections do
                let setA = Set.ofList a.Sets
                for b in incl.Intersections do
                    let setB = Set.ofList b.Sets
                    if Set.isSubset setA setB then
                        Expect.isTrue (b.Count <= a.Count) "Inclusive superset monotonicity |B| <= |A| when A subset B"

            let frame = ChartFrame.create 640. 420. "UpSet & Euler"
            let upsetChart = Chart.upset (UpSetSpec.create frame) sets |> ok
            Expect.isTrue (Scene.isComposed upsetChart.Scene) "Chart.upset composed scene"
            Expect.isNonEmpty (Scene.marks upsetChart.Scene) "Chart.upset non-empty marks"
            Expect.isNonEmpty upsetChart.HitTargets "Chart.upset non-empty hit targets"

            // EulerVenn & Chart.euler
            let eulerSets = [ "A", 100.; "B", 60.; "C", 40. ]
            let eulerOverlaps = [ "A", "B", 25.; "B", "C", 15.; "A", "C", 0. ]
            let eulerOut = EulerVenn.layout (Bounds.create 0. 0. 500. 400.) eulerSets eulerOverlaps |> ok
            let cMap = eulerOut.Circles |> List.map (fun c -> c.SetName, c) |> Map.ofList
            let cA, cB, cC = cMap["A"], cMap["B"], cMap["C"]
            let areaRatioAB = (cA.Radius * cA.Radius) / (cB.Radius * cB.Radius)
            let areaRatioBC = (cB.Radius * cB.Radius) / (cC.Radius * cC.Radius)
            Expect.isTrue (abs (areaRatioAB - (100. / 60.)) <= 1e-5) "Euler circle areas match A/B size ratio"
            Expect.isTrue (abs (areaRatioBC - (60. / 40.)) <= 1e-5) "Euler circle areas match B/C size ratio"
            let dAB = sqrt (Point.distanceSquared cA.Center cB.Center)
            let dBC = sqrt (Point.distanceSquared cB.Center cC.Center)
            let dAC = sqrt (Point.distanceSquared cA.Center cC.Center)
            Expect.isTrue (dAB < cA.Radius + cB.Radius) "intersecting sets A & B overlap"
            Expect.isTrue (dBC < cB.Radius + cC.Radius) "intersecting sets B & C overlap"
            Expect.isTrue (dAC >= cA.Radius + cC.Radius - 1e-3) "disjoint sets A & C do not overlap"

            let eulerChart = Chart.euler (EulerSpec.create frame) eulerSets eulerOverlaps |> ok
            Expect.isTrue (Scene.isComposed eulerChart.Scene) "Chart.euler composed scene"
            Expect.equal eulerChart.HitTargets.Length 3 "Chart.euler 3 disc hit targets"

        testCase "SphericalFibonacci and SphericalVoronoi satisfy unit norm, exact nearestCell, steradian conservation and CVT relaxation" <| fun _ ->
            let nFib = 250
            let fibPts = SphericalFibonacci.points nFib |> ok
            Expect.equal fibPts.Length nFib "SphericalFibonacci.points count"
            let fibVecs = fibPts |> List.map geoToUnitVec |> List.toArray
            for vx, vy, vz in fibVecs do
                let norm = sqrt (vx * vx + vy * vy + vz * vz)
                Expect.isTrue (abs (norm - 1.) <= 1e-12) "SphericalFibonacci point lies on unit sphere S^2"

            // nearestCell matches brute-force dot-product argmax across 200 query points on S^2
            let queries =
                [ for i in 0 .. 199 ->
                    let lon = -180. + 360. * ((float i * 0.61803398875) % 1.0)
                    let z = -0.995 + 1.99 * (float i / 199.)
                    let lat = asin z * (180. / Math.PI)
                    GeoPosition.create lon lat |> ok ]
            for q in queries do
                let qx, qy, qz = geoToUnitVec q
                let mutable bestIdx = 0
                let mutable bestDot = Double.NegativeInfinity
                for i in 0 .. nFib - 1 do
                    let sx, sy, sz = fibVecs[i]
                    let d = qx * sx + qy * sy + qz * sz
                    if d > bestDot then
                        bestDot <- d
                        bestIdx <- i
                let actualIdx = SphericalFibonacci.nearestCell nFib q |> ok
                Expect.equal actualIdx bestIdx "SphericalFibonacci.nearestCell matches brute-force dot-product argmax"

            // aggregate conserves sample count and total weight
            let weightedSamples = queries |> List.mapi (fun idx pos -> idx, pos, 1. + float (idx % 5) * 0.5)
            let bins = SphericalFibonacci.aggregate 64 (fun (_, p, _) -> p) (fun (_, _, w) -> w) weightedSamples |> ok
            Expect.equal (bins |> List.sumBy (fun b -> b.Members.Length)) 200 "SphericalFibonacci.aggregate conserves item count"
            let expectedW = weightedSamples |> List.sumBy (fun (_, _, w) -> w)
            let actualW = bins |> List.sumBy (fun b -> b.TotalWeight)
            Expect.isTrue (abs (actualW - expectedW) <= 1e-9) "SphericalFibonacci.aggregate conserves total weight"

            // SphericalVoronoi.compute cell steradian areas sum to 4 * pi within 1e-5
            let vorSites =
                SphericalFibonacci.points 32
                |> ok
                |> List.mapi (fun idx p -> sprintf "sv%d" idx, p)
            let vorCells = SphericalVoronoi.compute vorSites |> ok
            Expect.equal vorCells.Length 32 "SphericalVoronoi cell count"
            let totalSteradians = vorCells |> List.sumBy (fun c -> c.Steradians)
            Expect.isTrue (abs (totalSteradians - 4. * Math.PI) <= 1e-5) "SphericalVoronoi steradians sum to 4*pi within 1e-5"

            // SphericalVoronoi.relaxCvt increases minimum site separation on bunched sites spanning S^2
            let base16 = SphericalFibonacci.points 16 |> ok |> List.toArray
            let p1 = base16[3]
            let bunchedPos =
                GeoPosition.create (p1.Longitude + 5.0) (max -80. (min 80. (p1.Latitude + 4.0))) |> ok
            base16[4] <- bunchedPos
            let unevenSites = [ for i in 0 .. 15 -> sprintf "u%d" i, base16[i] ]
            let minSeparation (positions: GeoPosition list) =
                let arr = List.toArray positions
                let mutable m = Double.PositiveInfinity
                for i in 0 .. arr.Length - 2 do
                    for j in i + 1 .. arr.Length - 1 do
                        let d = Geodesic.distance (arr[i]) (arr[j])
                        if d < m then m <- d
                m
            let sepBefore = minSeparation (List.map snd unevenSites)
            let relaxedCells = SphericalVoronoi.relaxCvt 12 unevenSites |> ok
            let sepAfter = minSeparation (relaxedCells |> List.map (fun c -> c.Site))
            Expect.isTrue (sepAfter > sepBefore) (sprintf "SphericalVoronoi.relaxCvt increases min separation (%f > %f)" sepAfter sepBefore)
            let relaxedSteradians = relaxedCells |> List.sumBy (fun c -> c.Steradians)
            Expect.isTrue (abs (relaxedSteradians - 4. * Math.PI) <= 1e-5) "SphericalVoronoi.relaxCvt preserves 4*pi steradians"
    ]

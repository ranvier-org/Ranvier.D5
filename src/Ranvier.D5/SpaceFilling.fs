namespace Ranvier.D5

open System
open System.Collections.Generic
#if !FABLE_COMPILER
open System.Runtime.Intrinsics.X86
#endif

module internal SpaceFillingBits =
    let inline part1By1Swar (n: uint32) : uint32 =
        let mutable v = n &&& 0x0000FFFFu
        v <- (v ^^^ (v <<< 8)) &&& 0x00FF00FFu
        v <- (v ^^^ (v <<< 4)) &&& 0x0F0F0F0Fu
        v <- (v ^^^ (v <<< 2)) &&& 0x33333333u
        v <- (v ^^^ (v <<< 1)) &&& 0x55555555u
        v

    let inline compact1By1Swar (n: uint32) : uint16 =
        let mutable v = n &&& 0x55555555u
        v <- (v ^^^ (v >>> 1)) &&& 0x33333333u
        v <- (v ^^^ (v >>> 2)) &&& 0x0F0F0F0Fu
        v <- (v ^^^ (v >>> 4)) &&& 0x00FF00FFu
        v <- (v ^^^ (v >>> 8)) &&& 0x0000FFFFu
        uint16 v

    let inline part1By2Swar (n: uint32) : uint32 =
        let mutable v = n &&& 0x000003FFu
        v <- (v ^^^ (v <<< 16)) &&& 0x030000FFu
        v <- (v ^^^ (v <<< 8)) &&& 0x0300F00Fu
        v <- (v ^^^ (v <<< 4)) &&& 0x030C30C3u
        v <- (v ^^^ (v <<< 2)) &&& 0x09249249u
        v

    let inline compact1By2Swar (n: uint32) : uint16 =
        let mutable v = n &&& 0x09249249u
        v <- (v ^^^ (v >>> 2)) &&& 0x030C30C3u
        v <- (v ^^^ (v >>> 4)) &&& 0x0300F00Fu
        v <- (v ^^^ (v >>> 8)) &&& 0x030000FFu
        v <- (v ^^^ (v >>> 16)) &&& 0x000003FFu
        uint16 v

    let inline morton2DEncode (x: uint16) (y: uint16) : uint32 =
#if !FABLE_COMPILER
        if Bmi2.IsSupported then
            Bmi2.ParallelBitDeposit(uint32 x, 0x55555555u)
            ||| Bmi2.ParallelBitDeposit(uint32 y, 0xAAAAAAAAu)
        else
            part1By1Swar (uint32 x) ||| (part1By1Swar (uint32 y) <<< 1)
#else
        part1By1Swar (uint32 x) ||| (part1By1Swar (uint32 y) <<< 1)
#endif

    let inline morton2DDecode (code: uint32) : uint16 * uint16 =
#if !FABLE_COMPILER
        if Bmi2.IsSupported then
            uint16 (Bmi2.ParallelBitExtract(code, 0x55555555u)),
            uint16 (Bmi2.ParallelBitExtract(code, 0xAAAAAAAAu))
        else
            compact1By1Swar code, compact1By1Swar (code >>> 1)
#else
        compact1By1Swar code, compact1By1Swar (code >>> 1)
#endif

    let inline morton3DEncodeRaw (x: uint32) (y: uint32) (z: uint32) : uint32 =
#if !FABLE_COMPILER
        if Bmi2.IsSupported then
            Bmi2.ParallelBitDeposit(x, 0x09249249u)
            ||| Bmi2.ParallelBitDeposit(y, 0x12492492u)
            ||| Bmi2.ParallelBitDeposit(z, 0x24924924u)
        else
            part1By2Swar x ||| (part1By2Swar y <<< 1) ||| (part1By2Swar z <<< 2)
#else
        part1By2Swar x ||| (part1By2Swar y <<< 1) ||| (part1By2Swar z <<< 2)
#endif

    let inline morton3DDecodeRaw (code: uint32) : uint16 * uint16 * uint16 =
#if !FABLE_COMPILER
        if Bmi2.IsSupported then
            uint16 (Bmi2.ParallelBitExtract(code, 0x09249249u)),
            uint16 (Bmi2.ParallelBitExtract(code, 0x12492492u)),
            uint16 (Bmi2.ParallelBitExtract(code, 0x24924924u))
        else
            compact1By2Swar code, compact1By2Swar (code >>> 1), compact1By2Swar (code >>> 2)
#else
        compact1By2Swar code, compact1By2Swar (code >>> 1), compact1By2Swar (code >>> 2)
#endif

    let inline grayDecode32 (g: uint32) : uint32 =
        let mutable h = g
        h <- h ^^^ (h >>> 1)
        h <- h ^^^ (h >>> 2)
        h <- h ^^^ (h >>> 4)
        h <- h ^^^ (h >>> 8)
        h <- h ^^^ (h >>> 16)
        h

    let inline quantize16 (minVal: float) (span: float) (coord: float) : uint16 =
        if span <= 0. then
            0us
        else
            let hi = minVal + span
            let clamped = min hi (max minVal coord)
            let t = (clamped - minVal) / span
            let q = int (floor (t * 65536.))
            uint16 (min 65535 (max 0 q))

    let inline quantize10 (minVal: float) (span: float) (coord: float) : uint16 =
        if span <= 0. then
            0us
        else
            let hi = minVal + span
            let clamped = min hi (max minVal coord)
            let t = (clamped - minVal) / span
            let q = int (floor (t * 1024.))
            uint16 (min 1023 (max 0 q))

    let orderItemsByCode<'T> (encodeItem: 'T -> uint32) (items: seq<'T>) : 'T list =
        let coded =
            match box items with
            | :? ('T array) as arr ->
                let res = Array.zeroCreate<struct(uint32 * int * 'T)> arr.Length
                for i in 0 .. arr.Length - 1 do
                    let item = arr[i]
                    res[i] <- struct(encodeItem item, i, item)
                res
            | :? ('T list) as lst ->
                let n = List.length lst
                let res = Array.zeroCreate<struct(uint32 * int * 'T)> n
                let mutable i = 0
                for item in lst do
                    res[i] <- struct(encodeItem item, i, item)
                    i <- i + 1
                res
            | _ ->
                let buf = ResizeArray<struct(uint32 * int * 'T)>()
                let mutable i = 0
                for item in items do
                    buf.Add(struct(encodeItem item, i, item))
                    i <- i + 1
                buf.ToArray()
        let n = coded.Length
        if n = 0 then
            []
        else
            Array.sortInPlaceWith
                (fun struct(c1, i1, _) struct(c2, i2, _) ->
                    if c1 < c2 then -1
                    elif c1 > c2 then 1
                    elif i1 < i2 then -1
                    elif i1 > i2 then 1
                    else 0)
                coded
            let mutable acc = []
            let mutable idx = n - 1
            while idx >= 0 do
                let struct(_, _, item) = coded[idx]
                acc <- item :: acc
                idx <- idx - 1
            acc

    let hilbert2DEncodeCore (bits: int) (x: uint32) (y: uint32) : uint32 =
        let mutable x0 = x
        let mutable x1 = y
        let mutable q = 1u <<< (bits - 1)
        while q > 1u do
            let p = q - 1u
            if (x0 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            if (x1 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            else
                let t = (x0 ^^^ x1) &&& p
                x0 <- x0 ^^^ t
                x1 <- x1 ^^^ t
            q <- q >>> 1
        let g = morton2DEncode (uint16 x1) (uint16 x0)
        grayDecode32 g

    let hilbert2DDecodeCore (bits: int) (index: uint32) : uint32 * uint32 =
        let g = index ^^^ (index >>> 1)
        let raw1, raw0 = morton2DDecode g
        let mutable x0 = uint32 raw0
        let mutable x1 = uint32 raw1
        let limit = 1u <<< bits
        let mutable q = 2u
        while q < limit do
            let p = q - 1u
            if (x1 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            else
                let t = (x0 ^^^ x1) &&& p
                x0 <- x0 ^^^ t
                x1 <- x1 ^^^ t
            if (x0 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            q <- q <<< 1
        x0, x1

    let hilbert3DEncodeCore (bits: int) (x: uint32) (y: uint32) (z: uint32) : uint32 =
        let mutable x0 = x
        let mutable x1 = y
        let mutable x2 = z
        let mutable q = 1u <<< (bits - 1)
        while q > 1u do
            let p = q - 1u
            if (x0 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            if (x1 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            else
                let t = (x0 ^^^ x1) &&& p
                x0 <- x0 ^^^ t
                x1 <- x1 ^^^ t
            if (x2 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            else
                let t = (x0 ^^^ x2) &&& p
                x0 <- x0 ^^^ t
                x2 <- x2 ^^^ t
            q <- q >>> 1
        let g = morton3DEncodeRaw x2 x1 x0
        grayDecode32 g

    let hilbert3DDecodeCore (bits: int) (index: uint32) : uint32 * uint32 * uint32 =
        let g = index ^^^ (index >>> 1)
        let raw2, raw1, raw0 = morton3DDecodeRaw g
        let mutable x0 = uint32 raw0
        let mutable x1 = uint32 raw1
        let mutable x2 = uint32 raw2
        let limit = 1u <<< bits
        let mutable q = 2u
        while q < limit do
            let p = q - 1u
            if (x2 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            else
                let t = (x0 ^^^ x2) &&& p
                x0 <- x0 ^^^ t
                x2 <- x2 ^^^ t
            if (x1 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            else
                let t = (x0 ^^^ x1) &&& p
                x0 <- x0 ^^^ t
                x1 <- x1 ^^^ t
            if (x0 &&& q) <> 0u then
                x0 <- x0 ^^^ p
            q <- q <<< 1
        x0, x1, x2

/// <summary>Two-dimensional Z-order (Morton) bit-interleaved curve encoding, decoding and stable spatial ordering.</summary>
[<RequireQualifiedAccess>]
module Morton2D =
    /// <summary>Interleaves 16-bit X (even bits) and 16-bit Y (odd bits) into a 32-bit Morton code.</summary>
    let encode (x: uint16) (y: uint16) : uint32 =
        SpaceFillingBits.morton2DEncode x y

    /// <summary>De-interleaves a 32-bit Morton code back into its 16-bit <c>(x, y)</c> coordinates.</summary>
    let decode (code: uint32) : uint16 * uint16 =
        SpaceFillingBits.morton2DDecode code

    /// <summary>Quantizes a finite point clamped to <c>bounds</c> onto <c>[0, 65535]^2</c> and returns its 32-bit Morton code.</summary>
    let encodePoint (bounds: Bounds) (point: Point) : uint32 =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        let qx = SpaceFillingBits.quantize16 (Bounds.x bounds) (Bounds.width bounds) point.X
        let qy = SpaceFillingBits.quantize16 (Bounds.y bounds) (Bounds.height bounds) point.Y
        SpaceFillingBits.morton2DEncode qx qy

    /// <summary>Orders items deterministically by 2D Morton code within <c>bounds</c>, breaking code ties by original input index.</summary>
    let orderBy (bounds: Bounds) (project: 'T -> Point) (items: seq<'T>) : 'T list =
        let bx = Bounds.x bounds
        let by = Bounds.y bounds
        let bw = Bounds.width bounds
        let bh = Bounds.height bounds
        SpaceFillingBits.orderItemsByCode
            (fun item ->
                let pt = project item
                Check.finite "x" pt.X
                Check.finite "y" pt.Y
                let qx = SpaceFillingBits.quantize16 bx bw pt.X
                let qy = SpaceFillingBits.quantize16 by bh pt.Y
                SpaceFillingBits.morton2DEncode qx qy)
            items

/// <summary>Three-dimensional Z-order (Morton) bit-interleaved curve encoding (10 bits per axis into 30-bit <c>uint32</c>), decoding and stable spatial ordering.</summary>
[<RequireQualifiedAccess>]
module Morton3D =
    /// <summary>Interleaves 10-bit X, Y and Z coordinates in <c>[0, 1023]</c> into a 30-bit Morton code.</summary>
    let encode (x: uint16) (y: uint16) (z: uint16) : uint32 =
        if x > 1023us then invalidArg "x" "Morton3D coordinate must be in [0, 1023]."
        if y > 1023us then invalidArg "y" "Morton3D coordinate must be in [0, 1023]."
        if z > 1023us then invalidArg "z" "Morton3D coordinate must be in [0, 1023]."
        SpaceFillingBits.morton3DEncodeRaw (uint32 x) (uint32 y) (uint32 z)

    /// <summary>De-interleaves a 30-bit 3D Morton code back into its 10-bit <c>(x, y, z)</c> coordinates.</summary>
    let decode (code: uint32) : uint16 * uint16 * uint16 =
        if code > 0x3FFFFFFFu then invalidArg "code" "Morton3D code must fit in 30 bits."
        SpaceFillingBits.morton3DDecodeRaw code

    /// <summary>Quantizes a finite 3D point clamped to <c>bounds</c> onto <c>[0, 1023]^3</c> and returns its 30-bit Morton code.</summary>
    let encodePoint (bounds: Bounds3) (point: Point3) : uint32 =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        Check.finite "z" point.Z
        let qx = SpaceFillingBits.quantize10 (Bounds3.minX bounds) (Bounds3.sizeX bounds) point.X
        let qy = SpaceFillingBits.quantize10 (Bounds3.minY bounds) (Bounds3.sizeY bounds) point.Y
        let qz = SpaceFillingBits.quantize10 (Bounds3.minZ bounds) (Bounds3.sizeZ bounds) point.Z
        SpaceFillingBits.morton3DEncodeRaw (uint32 qx) (uint32 qy) (uint32 qz)

    /// <summary>Orders items deterministically by 30-bit 3D Morton code within <c>bounds</c>, breaking code ties by original input index.</summary>
    let orderBy (bounds: Bounds3) (project: 'T -> Point3) (items: seq<'T>) : 'T list =
        let bx = Bounds3.minX bounds
        let by = Bounds3.minY bounds
        let bz = Bounds3.minZ bounds
        let sx = Bounds3.sizeX bounds
        let sy = Bounds3.sizeY bounds
        let sz = Bounds3.sizeZ bounds
        SpaceFillingBits.orderItemsByCode
            (fun item ->
                let pt = project item
                Check.finite "x" pt.X
                Check.finite "y" pt.Y
                Check.finite "z" pt.Z
                let qx = SpaceFillingBits.quantize10 bx sx pt.X
                let qy = SpaceFillingBits.quantize10 by sy pt.Y
                let qz = SpaceFillingBits.quantize10 bz sz pt.Z
                SpaceFillingBits.morton3DEncodeRaw (uint32 qx) (uint32 qy) (uint32 qz))
            items

/// <summary>Two-dimensional Hilbert space-filling curve (Skilling 2004 Gray-code transpose) over orders 1..16.</summary>
[<RequireQualifiedAccess>]
module Hilbert2D =
    /// <summary>Maps 16-bit <c>(x, y)</c> grid coordinates bijectively onto their 32-bit Hilbert curve index.</summary>
    let encode (x: uint16) (y: uint16) : uint32 =
        SpaceFillingBits.hilbert2DEncodeCore 16 (uint32 x) (uint32 y)

    /// <summary>Maps a 32-bit Hilbert curve index bijectively onto its 16-bit <c>(x, y)</c> grid coordinates.</summary>
    let decode (index: uint32) : uint16 * uint16 =
        let x, y = SpaceFillingBits.hilbert2DDecodeCore 16 index
        uint16 x, uint16 y

    /// <summary>Maps <c>(x, y)</c> in <c>[0, 2^bits - 1]^2</c> to its Hilbert index in <c>[0, 4^bits - 1]</c> for <c>bits</c> in <c>1..16</c>.</summary>
    let encodeOrder (bits: int) (x: int) (y: int) : uint32 =
        if bits < 1 || bits > 16 then invalidArg "bits" "Hilbert2D order bits must be in [1, 16]."
        let limit = 1 <<< bits
        if x < 0 || x >= limit then invalidArg "x" "Coordinate x is outside [0, 2^bits - 1]."
        if y < 0 || y >= limit then invalidArg "y" "Coordinate y is outside [0, 2^bits - 1]."
        SpaceFillingBits.hilbert2DEncodeCore bits (uint32 x) (uint32 y)

    /// <summary>Maps a Hilbert index in <c>[0, 4^bits - 1]</c> to <c>(x, y)</c> in <c>[0, 2^bits - 1]^2</c> for <c>bits</c> in <c>1..16</c>.</summary>
    let decodeOrder (bits: int) (index: uint32) : int * int =
        if bits < 1 || bits > 16 then invalidArg "bits" "Hilbert2D order bits must be in [1, 16]."
        if bits < 16 && index >= (1u <<< (2 * bits)) then
            invalidArg "index" "Hilbert2D index exceeds 4^bits - 1."
        let x, y = SpaceFillingBits.hilbert2DDecodeCore bits index
        int x, int y

    /// <summary>Quantizes a finite point clamped to <c>bounds</c> onto <c>[0, 65535]^2</c> and returns its 32-bit Hilbert index.</summary>
    let encodePoint (bounds: Bounds) (point: Point) : uint32 =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        let qx = SpaceFillingBits.quantize16 (Bounds.x bounds) (Bounds.width bounds) point.X
        let qy = SpaceFillingBits.quantize16 (Bounds.y bounds) (Bounds.height bounds) point.Y
        SpaceFillingBits.hilbert2DEncodeCore 16 (uint32 qx) (uint32 qy)

    /// <summary>Orders items deterministically by 2D Hilbert index within <c>bounds</c>, breaking ties by original input index.</summary>
    let orderBy (bounds: Bounds) (project: 'T -> Point) (items: seq<'T>) : 'T list =
        let bx = Bounds.x bounds
        let by = Bounds.y bounds
        let bw = Bounds.width bounds
        let bh = Bounds.height bounds
        SpaceFillingBits.orderItemsByCode
            (fun item ->
                let pt = project item
                Check.finite "x" pt.X
                Check.finite "y" pt.Y
                let qx = SpaceFillingBits.quantize16 bx bw pt.X
                let qy = SpaceFillingBits.quantize16 by bh pt.Y
                SpaceFillingBits.hilbert2DEncodeCore 16 (uint32 qx) (uint32 qy))
            items

    /// <summary>Generates the <c>4^order</c> cell centers in Hilbert traversal order across <c>bounds</c> for <c>order</c> in <c>1..8</c>.</summary>
    let curve (order: int) (bounds: Bounds) : Point list =
        if order < 1 || order > 8 then invalidArg "order" "Hilbert2D curve order must be in [1, 8]."
        let side = 1 <<< order
        let total = 1 <<< (2 * order)
        let bx = Bounds.x bounds
        let by = Bounds.y bounds
        let stepX = Bounds.width bounds / float side
        let stepY = Bounds.height bounds / float side
        let mutable acc = []
        let mutable idx = total - 1
        while idx >= 0 do
            let ix, iy = SpaceFillingBits.hilbert2DDecodeCore order (uint32 idx)
            let px = bx + (float ix + 0.5) * stepX
            let py = by + (float iy + 0.5) * stepY
            acc <- Point.create px py :: acc
            idx <- idx - 1
        acc

/// <summary>Two-dimensional closed-loop symmetric Moore space-filling curve over orders 1..16 where start and end cells are lattice-adjacent.</summary>
[<RequireQualifiedAccess>]
module Moore2D =
    let private encodeCore (bits: int) (x: int) (y: int) : uint32 =
        if bits = 1 then
            match x, y with
            | 0, 0 -> 0u
            | 0, 1 -> 1u
            | 1, 1 -> 2u
            | _ -> 3u
        else
            let subBits = bits - 1
            let half = 1 <<< subBits
            let shift = 2 * subBits
            if x < half then
                if y < half then
                    let u = y
                    let v = half - 1 - x
                    SpaceFillingBits.hilbert2DEncodeCore subBits (uint32 u) (uint32 v)
                else
                    let u = y - half
                    let v = half - 1 - x
                    (1u <<< shift) ||| SpaceFillingBits.hilbert2DEncodeCore subBits (uint32 u) (uint32 v)
            else if y >= half then
                let u = (2 * half - 1) - y
                let v = x - half
                (2u <<< shift) ||| SpaceFillingBits.hilbert2DEncodeCore subBits (uint32 u) (uint32 v)
            else
                let u = half - 1 - y
                let v = x - half
                (3u <<< shift) ||| SpaceFillingBits.hilbert2DEncodeCore subBits (uint32 u) (uint32 v)

    let private decodeCore (bits: int) (index: uint32) : int * int =
        if bits = 1 then
            match index with
            | 0u -> 0, 0
            | 1u -> 0, 1
            | 2u -> 1, 1
            | _ -> 1, 0
        else
            let subBits = bits - 1
            let half = 1 <<< subBits
            let shift = 2 * subBits
            let mask = (1u <<< shift) - 1u
            let quad = index >>> shift
            let subIdx = index &&& mask
            let ru, rv = SpaceFillingBits.hilbert2DDecodeCore subBits subIdx
            let u = int ru
            let v = int rv
            match quad with
            | 0u -> half - 1 - v, u
            | 1u -> half - 1 - v, half + u
            | 2u -> half + v, (2 * half - 1) - u
            | _ -> half + v, half - 1 - u

    /// <summary>Maps 16-bit <c>(x, y)</c> grid coordinates bijectively onto their 32-bit closed-loop Moore index.</summary>
    let encode (x: uint16) (y: uint16) : uint32 =
        encodeCore 16 (int x) (int y)

    /// <summary>Maps a 32-bit Moore curve index bijectively onto its 16-bit <c>(x, y)</c> grid coordinates.</summary>
    let decode (index: uint32) : uint16 * uint16 =
        let x, y = decodeCore 16 index
        uint16 x, uint16 y

    /// <summary>Maps <c>(x, y)</c> in <c>[0, 2^bits - 1]^2</c> to its closed-loop Moore index in <c>[0, 4^bits - 1]</c> for <c>bits</c> in <c>1..16</c>.</summary>
    let encodeOrder (bits: int) (x: int) (y: int) : uint32 =
        if bits < 1 || bits > 16 then invalidArg "bits" "Moore2D order bits must be in [1, 16]."
        let limit = 1 <<< bits
        if x < 0 || x >= limit then invalidArg "x" "Coordinate x is outside [0, 2^bits - 1]."
        if y < 0 || y >= limit then invalidArg "y" "Coordinate y is outside [0, 2^bits - 1]."
        encodeCore bits x y

    /// <summary>Maps a Moore index in <c>[0, 4^bits - 1]</c> to <c>(x, y)</c> in <c>[0, 2^bits - 1]^2</c> for <c>bits</c> in <c>1..16</c>.</summary>
    let decodeOrder (bits: int) (index: uint32) : int * int =
        if bits < 1 || bits > 16 then invalidArg "bits" "Moore2D order bits must be in [1, 16]."
        if bits < 16 && index >= (1u <<< (2 * bits)) then
            invalidArg "index" "Moore2D index exceeds 4^bits - 1."
        decodeCore bits index

    /// <summary>Quantizes a finite point clamped to <c>bounds</c> onto <c>[0, 65535]^2</c> and returns its 32-bit Moore index.</summary>
    let encodePoint (bounds: Bounds) (point: Point) : uint32 =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        let qx = SpaceFillingBits.quantize16 (Bounds.x bounds) (Bounds.width bounds) point.X
        let qy = SpaceFillingBits.quantize16 (Bounds.y bounds) (Bounds.height bounds) point.Y
        encodeCore 16 (int qx) (int qy)

    /// <summary>Orders items deterministically by 2D Moore index within <c>bounds</c>, breaking ties by original input index.</summary>
    let orderBy (bounds: Bounds) (project: 'T -> Point) (items: seq<'T>) : 'T list =
        let bx = Bounds.x bounds
        let by = Bounds.y bounds
        let bw = Bounds.width bounds
        let bh = Bounds.height bounds
        SpaceFillingBits.orderItemsByCode
            (fun item ->
                let pt = project item
                Check.finite "x" pt.X
                Check.finite "y" pt.Y
                let qx = SpaceFillingBits.quantize16 bx bw pt.X
                let qy = SpaceFillingBits.quantize16 by bh pt.Y
                encodeCore 16 (int qx) (int qy))
            items

    /// <summary>Generates the <c>4^order</c> cell centers in closed-loop Moore traversal order across <c>bounds</c> for <c>order</c> in <c>1..8</c>.</summary>
    let curve (order: int) (bounds: Bounds) : Point list =
        if order < 1 || order > 8 then invalidArg "order" "Moore2D curve order must be in [1, 8]."
        let side = 1 <<< order
        let total = 1 <<< (2 * order)
        let bx = Bounds.x bounds
        let by = Bounds.y bounds
        let stepX = Bounds.width bounds / float side
        let stepY = Bounds.height bounds / float side
        let mutable acc = []
        let mutable idx = total - 1
        while idx >= 0 do
            let ix, iy = decodeCore order (uint32 idx)
            let px = bx + (float ix + 0.5) * stepX
            let py = by + (float iy + 0.5) * stepY
            acc <- Point.create px py :: acc
            idx <- idx - 1
        acc

/// <summary>Three-dimensional Hilbert space-filling curve (Skilling 2004 Gray-code transpose) over orders 1..10.</summary>
[<RequireQualifiedAccess>]
module Hilbert3D =
    /// <summary>Maps 10-bit <c>(x, y, z)</c> coordinates in <c>[0, 1023]^3</c> bijectively onto their 30-bit 3D Hilbert index.</summary>
    let encode (x: uint16) (y: uint16) (z: uint16) : uint32 =
        if x > 1023us then invalidArg "x" "Hilbert3D coordinate must be in [0, 1023]."
        if y > 1023us then invalidArg "y" "Hilbert3D coordinate must be in [0, 1023]."
        if z > 1023us then invalidArg "z" "Hilbert3D coordinate must be in [0, 1023]."
        SpaceFillingBits.hilbert3DEncodeCore 10 (uint32 x) (uint32 y) (uint32 z)

    /// <summary>Maps a 30-bit 3D Hilbert index in <c>[0, 2^30 - 1]</c> bijectively onto its 10-bit <c>(x, y, z)</c> coordinates.</summary>
    let decode (index: uint32) : uint16 * uint16 * uint16 =
        if index > 0x3FFFFFFFu then invalidArg "index" "Hilbert3D index must fit in 30 bits."
        let x, y, z = SpaceFillingBits.hilbert3DDecodeCore 10 index
        uint16 x, uint16 y, uint16 z

    /// <summary>Maps <c>(x, y, z)</c> in <c>[0, 2^bits - 1]^3</c> to its 3D Hilbert index in <c>[0, 8^bits - 1]</c> for <c>bits</c> in <c>1..10</c>.</summary>
    let encodeOrder (bits: int) (x: int) (y: int) (z: int) : uint32 =
        if bits < 1 || bits > 10 then invalidArg "bits" "Hilbert3D order bits must be in [1, 10]."
        let limit = 1 <<< bits
        if x < 0 || x >= limit then invalidArg "x" "Coordinate x is outside [0, 2^bits - 1]."
        if y < 0 || y >= limit then invalidArg "y" "Coordinate y is outside [0, 2^bits - 1]."
        if z < 0 || z >= limit then invalidArg "z" "Coordinate z is outside [0, 2^bits - 1]."
        SpaceFillingBits.hilbert3DEncodeCore bits (uint32 x) (uint32 y) (uint32 z)

    /// <summary>Maps a 3D Hilbert index in <c>[0, 8^bits - 1]</c> to <c>(x, y, z)</c> in <c>[0, 2^bits - 1]^3</c> for <c>bits</c> in <c>1..10</c>.</summary>
    let decodeOrder (bits: int) (index: uint32) : int * int * int =
        if bits < 1 || bits > 10 then invalidArg "bits" "Hilbert3D order bits must be in [1, 10]."
        if index >= (1u <<< (3 * bits)) then
            invalidArg "index" "Hilbert3D index exceeds 8^bits - 1."
        let x, y, z = SpaceFillingBits.hilbert3DDecodeCore bits index
        int x, int y, int z

    /// <summary>Quantizes a finite 3D point clamped to <c>bounds</c> onto <c>[0, 1023]^3</c> and returns its 30-bit 3D Hilbert index.</summary>
    let encodePoint (bounds: Bounds3) (point: Point3) : uint32 =
        Check.finite "x" point.X
        Check.finite "y" point.Y
        Check.finite "z" point.Z
        let qx = SpaceFillingBits.quantize10 (Bounds3.minX bounds) (Bounds3.sizeX bounds) point.X
        let qy = SpaceFillingBits.quantize10 (Bounds3.minY bounds) (Bounds3.sizeY bounds) point.Y
        let qz = SpaceFillingBits.quantize10 (Bounds3.minZ bounds) (Bounds3.sizeZ bounds) point.Z
        SpaceFillingBits.hilbert3DEncodeCore 10 (uint32 qx) (uint32 qy) (uint32 qz)

    /// <summary>Orders items deterministically by 30-bit 3D Hilbert index within <c>bounds</c>, breaking ties by original input index.</summary>
    let orderBy (bounds: Bounds3) (project: 'T -> Point3) (items: seq<'T>) : 'T list =
        let bx = Bounds3.minX bounds
        let by = Bounds3.minY bounds
        let bz = Bounds3.minZ bounds
        let sx = Bounds3.sizeX bounds
        let sy = Bounds3.sizeY bounds
        let sz = Bounds3.sizeZ bounds
        SpaceFillingBits.orderItemsByCode
            (fun item ->
                let pt = project item
                Check.finite "x" pt.X
                Check.finite "y" pt.Y
                Check.finite "z" pt.Z
                let qx = SpaceFillingBits.quantize10 bx sx pt.X
                let qy = SpaceFillingBits.quantize10 by sy pt.Y
                let qz = SpaceFillingBits.quantize10 bz sz pt.Z
                SpaceFillingBits.hilbert3DEncodeCore 10 (uint32 qx) (uint32 qy) (uint32 qz))
            items

    /// <summary>Generates the <c>8^order</c> cell centers in 3D Hilbert traversal order across <c>bounds</c> for <c>order</c> in <c>1..5</c>.</summary>
    let curve (order: int) (bounds: Bounds3) : Point3 list =
        if order < 1 || order > 5 then invalidArg "order" "Hilbert3D curve order must be in [1, 5]."
        let side = 1 <<< order
        let total = 1 <<< (3 * order)
        let bx = Bounds3.minX bounds
        let by = Bounds3.minY bounds
        let bz = Bounds3.minZ bounds
        let stepX = Bounds3.sizeX bounds / float side
        let stepY = Bounds3.sizeY bounds / float side
        let stepZ = Bounds3.sizeZ bounds / float side
        let mutable acc = []
        let mutable idx = total - 1
        while idx >= 0 do
            let ix, iy, iz = SpaceFillingBits.hilbert3DDecodeCore order (uint32 idx)
            let px = bx + (float ix + 0.5) * stepX
            let py = by + (float iy + 0.5) * stepY
            let pz = bz + (float iz + 0.5) * stepZ
            acc <- Point3.create px py pz :: acc
            idx <- idx - 1
        acc

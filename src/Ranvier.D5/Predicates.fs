namespace Ranvier.D5
open System
open System.Numerics

module internal ExactDyadic =
    let integers (values:float[]) =
        let parts=values |> Array.map(fun value ->
            Check.finite "coordinate" value
            let bits=BitConverter.DoubleToUInt64Bits value
            let fraction=bits &&& 0x000fffffffffffffUL
            let exponent=int((bits >>> 52) &&& 0x7ffUL)
            let mantissa=BigInteger(if exponent=0 then fraction else fraction ||| 0x0010000000000000UL)
            (if bits >>> 63=0UL then mantissa else -mantissa),(if exponent=0 then -1074 else exponent-1075))
        let nonzero=parts |> Array.filter(fun (mantissa,_) -> not mantissa.IsZero)
        let exponent=if nonzero.Length=0 then 0 else nonzero |> Array.minBy snd |> snd
        parts |> Array.map(fun (mantissa,power) -> if mantissa.IsZero then mantissa else mantissa <<< (power-exponent)),exponent
    let ratio (numerator:BigInteger) (denominator:BigInteger) exponent =
        if denominator.IsZero then invalidArg "denominator" "Division by zero."
        if numerator.IsZero then 0. else
        let n,d=BigInteger.Abs numerator,BigInteger.Abs denominator
        let ns=max 0 (int(n.GetBitLength())-53)
        let ds=max 0 (int(d.GetBitLength())-53)
        let result=Math.ScaleB(float(n >>> ns)/float(d >>> ds),exponent+ns-ds)*float(numerator.Sign*denominator.Sign)
        Check.finite "result" result
        result

/// <summary>Exact geometric predicates that are not affected by floating-point rounding.</summary>
[<RequireQualifiedAccess>]
module Predicates =
    // Floating filters: robust-predicates 3.0.2 / Shewchuk (Unlicense).
    // Exact dyadic fallback also handles exponent ranges unsafe for expansion arithmetic.
    let private epsilon=1.1102230246251565e-16
    let inline private safe value =
        let av = abs value
        av = 0. || (av >= 1e-140 && av <= 1e140)
    let inline private integerCoordinate value = abs value<=16777216. && Double.IsInteger value
    let inline private integerPoint (p:Point) = integerCoordinate p.X && integerCoordinate p.Y

#if !FABLE_COMPILER
    let inline private decomposeFinite (value: float) : struct(int64 * int) =
        Check.finite "coordinate" value
        let bits = BitConverter.DoubleToUInt64Bits value
        let frac = bits &&& 0x000fffffffffffffUL
        let exp = int ((bits >>> 52) &&& 0x7ffUL)
        let mag = int64 (if exp = 0 then frac else frac ||| 0x0010000000000000UL)
        let signedMag = if (bits >>> 63) = 0UL then mag else -mag
        let pow2 = if exp = 0 then -1074 else exp - 1075
        struct(signedMag, pow2)

    let private orient2dExact (a: Point) (b: Point) (c: Point) : int =
        let struct(m0, e0) = decomposeFinite a.X
        let struct(m1, e1) = decomposeFinite a.Y
        let struct(m2, e2) = decomposeFinite b.X
        let struct(m3, e3) = decomposeFinite b.Y
        let struct(m4, e4) = decomposeFinite c.X
        let struct(m5, e5) = decomposeFinite c.Y
        let mutable minE = Int32.MaxValue
        let mutable maxE = Int32.MinValue
        let inline track m e =
            if m <> 0L then
                if e < minE then minE <- e
                if e > maxE then maxE <- e
        track m0 e0; track m1 e1; track m2 e2; track m3 e3; track m4 e4; track m5 e5
        if minE = Int32.MaxValue then 0
        elif maxE - minE <= 9 then
            // Proven overflow-free in GeometryGeo.v (int128_orient2d_no_overflow):
            // |v_k| <= (2^53 - 1) * 2^9 < 2^62 and |det| < 2^127.
            let inline scale m e = if m = 0L then 0L else m <<< (e - minE)
            let v0 = scale m0 e0
            let v1 = scale m1 e1
            let v2 = scale m2 e2
            let v3 = scale m3 e3
            let v4 = scale m4 e4
            let v5 = scale m5 e5
            let p1 = Math.BigMul(v0 - v4, v3 - v5)
            let p2 = Math.BigMul(v1 - v5, v2 - v4)
            if p1 > p2 then 1 elif p1 < p2 then -1 else 0
        else
            let v, _ = ExactDyadic.integers [| a.X; a.Y; b.X; b.Y; c.X; c.Y |]
            ((v[0] - v[4]) * (v[3] - v[5]) - (v[1] - v[5]) * (v[2] - v[4])).Sign
#endif

    /// <summary>Returns the exact sign of the orientation of a, b, c: positive when they turn counterclockwise in y-up coordinates, negative when
    /// clockwise, zero when collinear. Raises ArgumentException for non-finite coordinates.</summary>
    let orient2d (a:Point) (b:Point) (c:Point) =
        let acx,acy,bcx,bcy=a.X-c.X,a.Y-c.Y,b.X-c.X,b.Y-c.Y
        let left,right=acx*bcy,acy*bcx
        let determinant=left-right
        let bound=(3.+16.*epsilon)*epsilon*(abs left+abs right)
        if safe acx && safe acy && safe bcx && safe bcy && (abs determinant>bound || (left=0. && right=0.)) then sign determinant else
        // Integer coordinates bounded by 2^24 keep both products and their difference below 2^53.
        if integerPoint a && integerPoint b && integerPoint c then sign determinant else
#if !FABLE_COMPILER
        orient2dExact a b c
#else
        let v,_=ExactDyadic.integers [|a.X;a.Y;b.X;b.Y;c.X;c.Y|]
        ((v[0]-v[4])*(v[3]-v[5])-(v[1]-v[5])*(v[2]-v[4])).Sign
#endif
    /// <summary>Returns the exact sign of the incircle test: positive when d lies inside the circle through counterclockwise a, b, c (y-up), negative
    /// outside, zero on it. Raises ArgumentException for non-finite coordinates.</summary>
    let incircle (a:Point) (b:Point) (c:Point) (d:Point) =
        let ax,ay,bx,by,cx,cy=a.X-d.X,a.Y-d.Y,b.X-d.X,b.Y-d.Y,c.X-d.X,c.Y-d.Y
        let al,bl,cl=ax*ax+ay*ay,bx*bx+by*by,cx*cx+cy*cy
        let bc,ca,ab=bx*cy-cx*by,cx*ay-ax*cy,ax*by-bx*ay
        let determinant=al*bc+bl*ca+cl*ab
        let permanent=(abs(bx*cy)+abs(cx*by))*al+(abs(cx*ay)+abs(ax*cy))*bl+(abs(ax*by)+abs(bx*ay))*cl
        let bound=(10.+96.*epsilon)*epsilon*permanent
        if safe ax && safe ay && safe bx && safe by && safe cx && safe cy && abs determinant>bound then sign determinant else
        // Exact integer differences <= 2^10 bound every fourth-degree intermediate below 2^44.
        if integerPoint a && integerPoint b && integerPoint c && integerPoint d &&
           max (max (abs ax) (abs ay)) (max (max (abs bx) (abs by)) (max (abs cx) (abs cy)))<=1024. then sign determinant else
        let v,_=ExactDyadic.integers [|a.X;a.Y;b.X;b.Y;c.X;c.Y;d.X;d.Y|]
        let ax,ay,bx,by,cx,cy=v[0]-v[6],v[1]-v[7],v[2]-v[6],v[3]-v[7],v[4]-v[6],v[5]-v[7]
        ((ax*ax+ay*ay)*(bx*cy-cx*by)+(bx*bx+by*by)*(cx*ay-ax*cy)+(cx*cx+cy*cy)*(ax*by-bx*ay)).Sign

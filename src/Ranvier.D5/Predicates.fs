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

[<RequireQualifiedAccess>]
module Predicates =
    // Floating filters: robust-predicates 3.0.2 / Shewchuk (Unlicense).
    // Exact dyadic fallback also handles exponent ranges unsafe for expansion arithmetic.
    let private epsilon=1.1102230246251565e-16
    let private safe value = value=0. || (abs value>=1e-50 && abs value<=1e50)
    let private integerCoordinate value = abs value<=16777216. && Double.IsInteger value
    let private integerPoint (p:Point) = integerCoordinate p.X && integerCoordinate p.Y
    let orient2d (a:Point) (b:Point) (c:Point) =
        let acx,acy,bcx,bcy=a.X-c.X,a.Y-c.Y,b.X-c.X,b.Y-c.Y
        let left,right=acx*bcy,acy*bcx
        let determinant=left-right
        let bound=(3.+16.*epsilon)*epsilon*(abs left+abs right)
        if safe acx && safe acy && safe bcx && safe bcy && (abs determinant>bound || (left=0. && right=0.)) then sign determinant else
        // Integer coordinates bounded by 2^24 keep both products and their difference below 2^53.
        if integerPoint a && integerPoint b && integerPoint c then sign determinant else
        let v,_=ExactDyadic.integers [|a.X;a.Y;b.X;b.Y;c.X;c.Y|]
        ((v[0]-v[4])*(v[3]-v[5])-(v[1]-v[5])*(v[2]-v[4])).Sign
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

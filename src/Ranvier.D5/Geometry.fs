namespace Ranvier.D5

open System
open System.Globalization
open System.Text

/// <summary>A point in an explicitly chosen coordinate system.</summary>
[<Struct>]
type Point = { X: float; Y: float }

[<RequireQualifiedAccess>]
module Point =
    /// <summary>Creates a point with finite coordinates.</summary>
    let create x y =
        Check.finite "x" x
        Check.finite "y" y
        { X=x; Y=y }
    /// <summary>Returns squared Euclidean distance; very large separations may overflow to infinity.</summary>
    let distanceSquared a b = (a.X-b.X)*(a.X-b.X)+(a.Y-b.Y)*(a.Y-b.Y)

/// <summary>A finite axis-aligned rectangle with nonnegative dimensions.</summary>
type Bounds = private { Left:float; Top:float; Width:float; Height:float }

[<RequireQualifiedAccess>]
module Bounds =
    /// <summary>Creates a rectangle; coordinates and its far edges must be finite.</summary>
    let create x y width height =
        [x;y;width;height;x+width;y+height] |> List.iter (Check.finite "bounds")
        if width<0. || height<0. then invalidArg "bounds" "Dimensions must be nonnegative."
        { Left=x; Top=y; Width=width; Height=height }
    let x bounds = bounds.Left
    let y bounds = bounds.Top
    let width bounds = bounds.Width
    let height bounds = bounds.Height
    /// <summary>Tests inclusive edges; non-finite points are outside.</summary>
    let contains point bounds =
        point.X>=bounds.Left && point.X<=bounds.Left+bounds.Width && point.Y>=bounds.Top && point.Y<=bounds.Top+bounds.Height
    /// <summary>Returns the enclosing rectangle of two bounds.</summary>
    let union a b =
        let left,top = min a.Left b.Left,min a.Top b.Top
        create left top (max (a.Left+a.Width) (b.Left+b.Width)-left) (max (a.Top+a.Height) (b.Top+b.Height)-top)

/// <summary>An immutable affine matrix using SVG's a,b,c,d,e,f convention.</summary>
type Transform = private { A:float; B:float; C:float; D:float; E:float; F:float }

module internal Number =
    let format (value:float) = if value=0. then "0" else value.ToString("G17",CultureInfo.InvariantCulture)

[<RequireQualifiedAccess>]
module Transform =
    /// <summary>Creates a finite affine matrix.</summary>
    let create a b c d e f =
        [a;b;c;d;e;f] |> List.iter (Check.finite "matrix")
        { A=a; B=b; C=c; D=d; E=e; F=f }
    let internal coefficients matrix = matrix.A,matrix.B,matrix.C,matrix.D,matrix.E,matrix.F
    let identity = create 1. 0. 0. 1. 0. 0.
    let translate x y = create 1. 0. 0. 1. x y
    let scale x y = create x 0. 0. y 0. 0.
    /// <summary>Rotates by an angle in radians.</summary>
    let rotate angle =
        Check.finite "angle" angle
        create (cos angle) (sin angle) (-sin angle) (cos angle) 0. 0.
    /// <summary>Composes matrices in pipeline order: first |> thenApply next.</summary>
    let thenApply next first =
        create (next.A*first.A+next.C*first.B) (next.B*first.A+next.D*first.B)
            (next.A*first.C+next.C*first.D) (next.B*first.C+next.D*first.D)
            (next.A*first.E+next.C*first.F+next.E) (next.B*first.E+next.D*first.F+next.F)
    /// <summary>Transforms a point, rejecting overflow.</summary>
    let apply matrix point = Point.create (matrix.A*point.X+matrix.C*point.Y+matrix.E) (matrix.B*point.X+matrix.D*point.Y+matrix.F)
    /// <summary>Returns None for singular matrices or inverses that overflow.</summary>
    let tryInverse matrix =
        let row1 = max (abs matrix.A) (abs matrix.C)
        let row2 = max (abs matrix.B) (abs matrix.D)
        if row1 = 0. || row2 = 0. then None
        else
            let a,c = matrix.A / row1, matrix.C / row1
            let b,d = matrix.B / row2, matrix.D / row2
            let determinant = a*d-b*c
            let rawDeterminant = matrix.A*matrix.D-matrix.B*matrix.C
            let rawUsable = rawDeterminant <> 0. && Double.IsFinite rawDeterminant
            let losesCoefficient =
                [matrix.A,a; matrix.B,b; matrix.C,c; matrix.D,d]
                |> List.exists(fun (original, normalized) -> original <> 0. && abs normalized < 2.2250738585072014e-308)
            if determinant = 0. && not rawUsable then None
            else
                let divide numerator row =
                    let value = numerator / determinant
                    if Double.IsFinite value then value / row else (numerator / row) / determinant
                let ia,ib,ic,id =
                    if rawUsable && (determinant = 0. || losesCoefficient) then
                        matrix.D/rawDeterminant, -matrix.B/rawDeterminant, -matrix.C/rawDeterminant, matrix.A/rawDeterminant
                    else divide d row1, divide -b row1, divide -c row2, divide a row2
                let values = [ia; ib; ic; id; -(ia*matrix.E+ic*matrix.F); -(ib*matrix.E+id*matrix.F)]
                if List.forall Double.IsFinite values then Some (create values[0] values[1] values[2] values[3] values[4] values[5]) else None
    /// <summary>Serializes a culture-independent SVG matrix.</summary>
    let toSvg matrix = "matrix(" + ([matrix.A;matrix.B;matrix.C;matrix.D;matrix.E;matrix.F] |> List.map Number.format |> String.concat " ") + ")"

/// <summary>Immutable SVG path instructions; arc rotation is in degrees.</summary>
type PathCommand =
    | MoveTo of Point
    | LineTo of Point
    | CubicTo of Point * Point * Point
    | QuadraticTo of Point * Point
    | ArcTo of radiusX:float * radiusY:float * rotation:float * large:bool * clockwise:bool * destination:Point
    | Close

/// <summary>A validated immutable path; each drawable subpath starts with MoveTo.</summary>
type Path = private Path of PathCommand list

[<RequireQualifiedAccess>]
module Path =
    let empty = Path []
    /// <summary>Validates finite coordinates, nonnegative radii and a leading MoveTo.</summary>
    let ofCommands commands =
        let validate point = Point.create point.X point.Y |> ignore
        let mutable hasPoint = false
        for command in commands do
            match command with
            | MoveTo point -> validate point; hasPoint <- true
            | _ when not hasPoint -> invalidArg "commands" "A path must start with MoveTo."
            | LineTo point -> validate point
            | CubicTo (a,b,c) -> List.iter validate [a;b;c]
            | QuadraticTo (a,b) -> List.iter validate [a;b]
            | ArcTo (rx,ry,rotation,_,_,point) ->
                List.iter (Check.finite "arc") [rx;ry;rotation]
                if rx<0. || ry<0. then invalidArg "commands" "Arc radii must be nonnegative."
                validate point
            | Close -> ()
        Path commands
    let commands (Path commands) = commands
    /// <summary>Concatenates independent subpaths.</summary>
    let concat paths = paths |> List.collect commands |> ofCommands
    /// <summary>Serializes a validated path using invariant round-trippable coordinates.</summary>
    let toSvg (Path commands) =
        let output = StringBuilder()
        let add (text:string) = output.Append text |> ignore
        let point p = add(Number.format p.X); add ","; add(Number.format p.Y)
        for command in commands do
            match command with
            | MoveTo p -> add "M"; point p
            | LineTo p -> add "L"; point p
            | CubicTo (a,b,c) -> add "C"; point a; add " "; point b; add " "; point c
            | QuadraticTo (a,b) -> add "Q"; point a; add " "; point b
            | ArcTo (rx,ry,rotation,large,clockwise,p) ->
                add "A"; add(Number.format rx); add ","; add(Number.format ry); add " "; add(Number.format rotation)
                add(if large then " 1," else " 0,"); add(if clockwise then "1 " else "0 "); point p
            | Close -> add "Z"
        output.ToString()

/// <summary>Supported interpolation of line segments. CatmullRom tension is in [0,1].</summary>
[<RequireQualifiedAccess>]
type Curve = Linear | StepBefore | StepAfter | CatmullRom of tension:float

/// <summary>Area-normalized point glyphs.</summary>
[<RequireQualifiedAccess>]
type Symbol = Circle | Square | Diamond | Triangle

[<RequireQualifiedAccess>]
module Shape =
    let private validateCurve curve =
        match curve with
        | Curve.CatmullRom tension ->
            Check.finite "tension" tension
            if tension<0. || tension>1. then invalidArg "tension" "Tension must be in [0,1]."
        | _ -> ()
    let private runs values =
        let segments = ResizeArray<_>()
        let mutable current = []
        for value in values do
            match value with
            | Some point -> current <- point::current
            | None -> if not (List.isEmpty current) then segments.Add(List.rev current); current <- []
        if not (List.isEmpty current) then segments.Add(List.rev current)
        List.ofSeq segments

    let private segment curve points =
        let points = Array.ofList points
        if points.Length=0 then []
        else
            let commands = ResizeArray<PathCommand>()
            commands.Add(MoveTo points[0])
            for i in 1..points.Length-1 do
                let a,b = points[i-1],points[i]
                match curve with
                | Curve.Linear -> commands.Add(LineTo b)
                | Curve.StepBefore -> commands.Add(LineTo(Point.create a.X b.Y)); commands.Add(LineTo b)
                | Curve.StepAfter -> commands.Add(LineTo(Point.create b.X a.Y)); commands.Add(LineTo b)
                | Curve.CatmullRom tension ->
                    Check.finite "tension" tension
                    if tension<0. || tension>1. then invalidArg "tension" "Tension must be in [0,1]."
                    let before,after = points[max 0 (i-2)],points[min (points.Length-1) (i+1)]
                    let factor = (1.-tension)/6.
                    commands.Add(CubicTo(Point.create (a.X+(b.X-before.X)*factor) (a.Y+(b.Y-before.Y)*factor),
                                         Point.create (b.X-(after.X-a.X)*factor) (b.Y-(after.Y-a.Y)*factor),b))
            List.ofSeq commands

    /// <summary>Creates a line; None observations break the line into independent subpaths.</summary>
    let line curve points =
        validateCurve curve
        runs points |> List.collect (segment curve) |> Path.ofCommands
    /// <summary>Creates closed areas from lower/upper point pairs; None observations split areas.</summary>
    let area curve points =
        validateCurve curve
        runs points |> List.collect (fun run ->
            let lower,upper = List.unzip run
            let top = segment curve upper
            let reverseCurve = match curve with Curve.StepBefore -> Curve.StepAfter | Curve.StepAfter -> Curve.StepBefore | other -> other
            let bottom = segment reverseCurve (List.rev lower)
            top @ (match bottom with MoveTo p::rest -> LineTo p::rest | _ -> bottom) @ [Close]) |> Path.ofCommands
    /// <summary>Creates a circular sector or annulus, with zero radians at twelve o'clock and positive angles clockwise.</summary>
    let arc inner outer start finish =
        [inner;outer;start;finish;finish-start] |> List.iter (Check.finite "arc")
        if inner<0. || outer<inner then invalidArg "radii" "Require 0 <= inner <= outer."
        let span = finish-start
        if outer=0. || span=0. then Path.empty
        else
            let clockwise = span>0.
            let full = abs span >= 2.*Math.PI-1e-12
            let finish = if full then start + (if clockwise then 2.*Math.PI else -2.*Math.PI) else finish
            let point radius angle = Point.create (radius*sin angle) (-radius*cos angle)
            let ring radius fromAngle toAngle direction =
                let first,last = point radius fromAngle,point radius toAngle
                if full then [MoveTo first;ArcTo(radius,radius,0.,false,direction,point radius ((fromAngle+toAngle)/2.));ArcTo(radius,radius,0.,false,direction,first)]
                else [MoveTo first;ArcTo(radius,radius,0.,abs(toAngle-fromAngle)>Math.PI,direction,last)]
            let outside = ring outer start finish clockwise
            let inside = if inner=0. then (if full then [] else [LineTo(Point.create 0. 0.)]) else ring inner finish start (not clockwise)
            let inside = match inside with MoveTo p::rest when not full -> LineTo p::rest | _ -> inside
            outside @ (if full then [Close] else []) @ inside @ [Close] |> Path.ofCommands
    /// <summary>Creates a rectangular path with nonnegative dimensions.</summary>
    let rectangle bounds =
        let x,y,w,h = Bounds.x bounds,Bounds.y bounds,Bounds.width bounds,Bounds.height bounds
        Path.ofCommands [MoveTo(Point.create x y);LineTo(Point.create (x+w) y);LineTo(Point.create (x+w) (y+h));LineTo(Point.create x (y+h));Close]
    /// <summary>Creates a centered symbol with the requested nonnegative area.</summary>
    let symbol kind size =
        Check.finite "size" size
        if size<0. then invalidArg "size" "Area must be nonnegative."
        if size=0. then Path.empty
        else
            match kind with
            | Symbol.Circle -> arc 0. (sqrt(size/Math.PI)) 0. (2.*Math.PI)
            | Symbol.Square -> let side = sqrt size in rectangle (Bounds.create (-side/2.) (-side/2.) side side)
            | Symbol.Diamond ->
                let radius = sqrt(size/2.)
                Path.ofCommands [MoveTo(Point.create 0. -radius);LineTo(Point.create radius 0.);LineTo(Point.create 0. radius);LineTo(Point.create -radius 0.);Close]
            | Symbol.Triangle ->
                let side = sqrt(4.*size/sqrt 3.)
                let height = side*sqrt 3./2.
                Path.ofCommands [MoveTo(Point.create 0. (-2.*height/3.));LineTo(Point.create (side/2.) (height/3.));LineTo(Point.create (-side/2.) (height/3.));Close]
    /// <summary>Creates a cubic link with horizontal endpoint tangents.</summary>
    let linkHorizontal source target =
        let middle = Check.lerp source.X target.X 0.5
        Path.ofCommands [MoveTo source;CubicTo(Point.create middle source.Y,Point.create middle target.Y,target)]

namespace Ranvier.D5

open System
open System.Globalization
open System.Text

/// <summary>A point in an explicitly chosen coordinate system.</summary>
[<Struct>]
type Point = { X: float; Y: float }

/// <summary>Point construction and distance.</summary>
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

/// <summary>Construction, accessors and tests for Bounds.</summary>
[<RequireQualifiedAccess>]
module Bounds =
    /// <summary>Creates a rectangle; coordinates and its far edges must be finite.</summary>
    let create x y width height =
        // Validate each coordinate directly; rectangles are created in hot recipe loops.
        Check.finite "bounds" x; Check.finite "bounds" y; Check.finite "bounds" width; Check.finite "bounds" height
        Check.finite "bounds" (x+width); Check.finite "bounds" (y+height)
        if width<0. || height<0. then invalidArg "bounds" "Dimensions must be nonnegative."
        { Left=x; Top=y; Width=width; Height=height }
    /// <summary>Returns the left edge.</summary>
    let x bounds = bounds.Left
    /// <summary>Returns the top edge.</summary>
    let y bounds = bounds.Top
    /// <summary>Returns the nonnegative width.</summary>
    let width bounds = bounds.Width
    /// <summary>Returns the nonnegative height.</summary>
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

#if !FABLE_COMPILER
type internal NumberScratch() =
    [<ThreadStatic; DefaultValue>]
    static val mutable private buf : char array
    static member Get() =
        let mutable b = NumberScratch.buf
        if isNull b then
            b <- Array.zeroCreate<char> 32
            NumberScratch.buf <- b
        b
#endif

module internal Number =
    let format (value:float) = if value=0. then "0" else value.ToString("G17",CultureInfo.InvariantCulture)
#if !FABLE_COMPILER
    let getBuffer () = NumberScratch.Get()
    let inline appendWith (buf:char array) (output:StringBuilder) (value:float) =
        if value=0. then output.Append '0' |> ignore
        else
            let mutable written = 0
            if value.TryFormat(Span<char>(buf), &written, "G17", CultureInfo.InvariantCulture) then
                output.Append(buf, 0, written) |> ignore
            else
                output.Append(value.ToString("G17",CultureInfo.InvariantCulture)) |> ignore
#else
    let inline getBuffer () : char array = [||]
    let inline appendWith (_:char array) (output:StringBuilder) (value:float) =
        output.Append(format value) |> ignore
#endif

/// <summary>Affine transform construction, composition, application, inversion and SVG serialization.</summary>
[<RequireQualifiedAccess>]
module Transform =
    /// <summary>Creates a finite affine matrix.</summary>
    let create a b c d e f =
        Check.finite "matrix" a; Check.finite "matrix" b; Check.finite "matrix" c
        Check.finite "matrix" d; Check.finite "matrix" e; Check.finite "matrix" f
        { A=a; B=b; C=c; D=d; E=e; F=f }
    let internal coefficients matrix = matrix.A,matrix.B,matrix.C,matrix.D,matrix.E,matrix.F
    /// <summary>The identity transform.</summary>
    let identity = create 1. 0. 0. 1. 0. 0.
    /// <summary>Creates a translation; raises ArgumentException for non-finite offsets.</summary>
    let translate x y = create 1. 0. 0. 1. x y
    /// <summary>Creates an axis-aligned scale; raises ArgumentException for non-finite factors.</summary>
    let scale x y = create x 0. 0. y 0. 0.
    /// <summary>Rotates by an angle in radians.</summary>
    let rotate angle =
        Check.finite "angle" angle
#if !FABLE_COMPILER
        let struct(s, c) = Math.SinCos angle
        create c s -s c 0. 0.
#else
        create (cos angle) (sin angle) (-sin angle) (cos angle) 0. 0.
#endif
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
            let inline subnormalLoss orig norm = orig <> 0. && abs norm < 2.2250738585072014e-308
            let losesCoefficient =
                subnormalLoss matrix.A a || subnormalLoss matrix.B b || subnormalLoss matrix.C c || subnormalLoss matrix.D d
            if determinant = 0. && not rawUsable then None
            else
                let divide numerator row =
                    let value = numerator / determinant
                    if Double.IsFinite value then value / row else (numerator / row) / determinant
                let ia,ib,ic,id =
                    if rawUsable && (determinant = 0. || losesCoefficient) then
                        matrix.D/rawDeterminant, -matrix.B/rawDeterminant, -matrix.C/rawDeterminant, matrix.A/rawDeterminant
                    else divide d row1, divide -b row1, divide -c row2, divide a row2
                let ie = -(ia*matrix.E+ic*matrix.F)
                let iff = -(ib*matrix.E+id*matrix.F)
                if Double.IsFinite ia && Double.IsFinite ib && Double.IsFinite ic && Double.IsFinite id && Double.IsFinite ie && Double.IsFinite iff then
                    Some { A=ia; B=ib; C=ic; D=id; E=ie; F=iff }
                else None
    let private identitySvg = "matrix(1 0 0 1 0 0)"
    /// <summary>Serializes a culture-independent SVG matrix.</summary>
    let toSvg matrix =
        if Object.ReferenceEquals(matrix, identity) ||
           (matrix.A = 1. && matrix.B = 0. && matrix.C = 0. && matrix.D = 1. && matrix.E = 0. && matrix.F = 0.) then
            identitySvg
        else
            let buf = Number.getBuffer ()
            let sb = StringBuilder(64)
            sb.Append "matrix(" |> ignore
            Number.appendWith buf sb matrix.A; sb.Append ' ' |> ignore
            Number.appendWith buf sb matrix.B; sb.Append ' ' |> ignore
            Number.appendWith buf sb matrix.C; sb.Append ' ' |> ignore
            Number.appendWith buf sb matrix.D; sb.Append ' ' |> ignore
            Number.appendWith buf sb matrix.E; sb.Append ' ' |> ignore
            Number.appendWith buf sb matrix.F; sb.Append ')' |> ignore
            sb.ToString()

/// <summary>Immutable SVG path instructions; arc rotation is in degrees.</summary>
type PathCommand =
    /// <summary>Starts a new subpath at the point.</summary>
    | MoveTo of Point
    /// <summary>Draws a straight segment to the point.</summary>
    | LineTo of Point
    /// <summary>Draws a cubic Bezier segment through two control points to the final point.</summary>
    | CubicTo of Point * Point * Point
    /// <summary>Draws a quadratic Bezier segment through one control point to the final point.</summary>
    | QuadraticTo of Point * Point
    /// <summary>Draws an elliptical arc to the destination with SVG large-arc and sweep flags; radii must be nonnegative and rotation is in
    /// degrees.</summary>
    | ArcTo of radiusX:float * radiusY:float * rotation:float * large:bool * clockwise:bool * destination:Point
    /// <summary>Closes the current subpath with a straight segment to its start.</summary>
    | Close

/// <summary>A validated immutable path; each drawable subpath starts with MoveTo.</summary>
type Path = private Path of PathCommand list

/// <summary>Construction, inspection and SVG serialization of paths.</summary>
[<RequireQualifiedAccess>]
module Path =
    /// <summary>A path with no commands; it serializes to an empty string.</summary>
    let empty = Path []
    /// <summary>Validates finite coordinates, nonnegative radii and a leading MoveTo.</summary>
    let ofCommands commands =
        let inline validate (point:Point) = Check.finite "x" point.X; Check.finite "y" point.Y
        let mutable hasPoint = false
        for command in commands do
            match command with
            | MoveTo point -> validate point; hasPoint <- true
            | _ when not hasPoint -> invalidArg "commands" "A path must start with MoveTo."
            | LineTo point -> validate point
            | CubicTo (a,b,c) -> validate a; validate b; validate c
            | QuadraticTo (a,b) -> validate a; validate b
            | ArcTo (rx,ry,rotation,_,_,point) ->
                Check.finite "arc" rx; Check.finite "arc" ry; Check.finite "arc" rotation
                if rx<0. || ry<0. then invalidArg "commands" "Arc radii must be nonnegative."
                validate point
            | Close -> ()
        Path commands
    /// <summary>Returns the commands in drawing order.</summary>
    let commands (Path commands) = commands
    /// <summary>Concatenates independent subpaths.</summary>
    let concat paths = paths |> List.collect commands |> ofCommands
    /// <summary>Serializes a validated path using invariant round-trippable coordinates.</summary>
    let toSvg (Path commands) =
        match commands with
        | [] -> ""
        | _ ->
            let buf = Number.getBuffer ()
            let output = StringBuilder(128)
            let inline point (p:Point) =
                Number.appendWith buf output p.X
                output.Append ',' |> ignore
                Number.appendWith buf output p.Y
            for command in commands do
                match command with
                | MoveTo p -> output.Append 'M' |> ignore; point p
                | LineTo p -> output.Append 'L' |> ignore; point p
                | CubicTo (a,b,c) ->
                    output.Append 'C' |> ignore; point a
                    output.Append ' ' |> ignore; point b
                    output.Append ' ' |> ignore; point c
                | QuadraticTo (a,b) ->
                    output.Append 'Q' |> ignore; point a
                    output.Append ' ' |> ignore; point b
                | ArcTo (rx,ry,rotation,large,clockwise,p) ->
                    output.Append 'A' |> ignore
                    Number.appendWith buf output rx
                    output.Append ',' |> ignore
                    Number.appendWith buf output ry
                    output.Append ' ' |> ignore
                    Number.appendWith buf output rotation
                    output.Append(if large then " 1," else " 0,") |> ignore
                    output.Append(if clockwise then "1 " else "0 ") |> ignore
                    point p
                | Close -> output.Append 'Z' |> ignore
            output.ToString()

/// <summary>Supported interpolation of line segments. CatmullRom tension is in [0,1].</summary>
[<RequireQualifiedAccess>]
type Curve = Linear | StepBefore | StepAfter | CatmullRom of tension:float

/// <summary>Area-normalized point glyphs.</summary>
[<RequireQualifiedAccess>]
type Symbol = Circle | Square | Diamond | Triangle

/// <summary>Path generators for lines, areas, arcs, rectangles, symbols and links.</summary>
[<RequireQualifiedAccess>]
module Shape =
    let private validateCurve curve =
        match curve with
        | Curve.CatmullRom tension ->
            Check.finite "tension" tension
            if tension<0. || tension>1. then invalidArg "tension" "Tension must be in [0,1]."
        | _ -> ()

    let private runsArrays (values: 'T option list) : 'T array list =
        let segments = ResizeArray<'T array>()
        let current = ResizeArray<'T>()
        for value in values do
            match value with
            | Some item -> current.Add item
            | None ->
                if current.Count > 0 then
                    segments.Add(current.ToArray())
                    current.Clear()
        if current.Count > 0 then segments.Add(current.ToArray())
        Data.resizeToList segments

    let private segmentArray curve (points: Point array) =
        let n = points.Length
        if n = 0 then []
        else
            match curve with
            | Curve.Linear ->
                let mutable acc = []
                for i = n - 1 downto 1 do
                    acc <- LineTo points[i] :: acc
                MoveTo points[0] :: acc
            | Curve.StepBefore ->
                let mutable acc = []
                for i = n - 1 downto 1 do
                    let a, b = points[i - 1], points[i]
                    acc <- LineTo(Point.create a.X b.Y) :: LineTo b :: acc
                MoveTo points[0] :: acc
            | Curve.StepAfter ->
                let mutable acc = []
                for i = n - 1 downto 1 do
                    let a, b = points[i - 1], points[i]
                    acc <- LineTo(Point.create b.X a.Y) :: LineTo b :: acc
                MoveTo points[0] :: acc
            | Curve.CatmullRom tension ->
                let factor = (1. - tension) / 6.
                let mutable acc = []
                for i = n - 1 downto 1 do
                    let a, b = points[i - 1], points[i]
                    let before, after = points[max 0 (i - 2)], points[min (n - 1) (i + 1)]
                    acc <-
                        CubicTo(
                            Point.create (a.X + (b.X - before.X) * factor) (a.Y + (b.Y - before.Y) * factor),
                            Point.create (b.X - (after.X - a.X) * factor) (b.Y - (after.Y - a.Y) * factor),
                            b) :: acc
                MoveTo points[0] :: acc

    /// <summary>Creates a line; None observations break the line into independent subpaths.</summary>
    let line curve points =
        validateCurve curve
        match curve with
        | Curve.Linear ->
            let mutable cmdCount = 0
            for v in points do
                match v with
                | Some _ -> cmdCount <- cmdCount + 1
                | None -> ()
            if cmdCount = 0 then Path.empty
            else
                let arr = Array.zeroCreate<PathCommand> cmdCount
                let mutable w = 0
                let mutable inRun = false
                for v in points do
                    match v with
                    | Some pt ->
                        Check.finite "x" pt.X
                        Check.finite "y" pt.Y
                        arr[w] <- if inRun then LineTo pt else (inRun <- true; MoveTo pt)
                        w <- w + 1
                    | None ->
                        inRun <- false
                let mutable cmds = []
                for i = cmdCount - 1 downto 0 do
                    cmds <- arr[i] :: cmds
                Path cmds
        | _ ->
            match runsArrays points with
            | [] -> Path.empty
            | [single] -> segmentArray curve single |> Path.ofCommands
            | many -> many |> List.collect (segmentArray curve) |> Path.ofCommands

    /// <summary>Creates closed areas from lower/upper point pairs; None observations split areas.</summary>
    let area curve points =
        validateCurve curve
        let reverseCurve = match curve with Curve.StepBefore -> Curve.StepAfter | Curve.StepAfter -> Curve.StepBefore | other -> other
        runsArrays points |> List.collect (fun (run: (Point * Point) array) ->
            let n = run.Length
            let upper = Array.zeroCreate<Point> n
            let lowerRev = Array.zeroCreate<Point> n
            for i = 0 to n - 1 do
                let lo, hi = run[i]
                upper[i] <- hi
                lowerRev[n - 1 - i] <- lo
            let top = segmentArray curve upper
            let bottom = segmentArray reverseCurve lowerRev
            top @ (match bottom with MoveTo p::rest -> LineTo p::rest | _ -> bottom) @ [Close]) |> Path.ofCommands

    /// <summary>Creates a circular sector or annulus, with zero radians at twelve o'clock and positive angles clockwise.</summary>
    let arc inner outer start finish =
        Check.finite "arc" inner; Check.finite "arc" outer; Check.finite "arc" start; Check.finite "arc" finish; Check.finite "arc" (finish-start)
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

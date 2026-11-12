module Ranvier.D5.Tests.CurveStackTests

open System
open System.IO
open System.Text.Json
open System.Text.RegularExpressions
open System.Globalization
open Expecto
open Ranvier.D5

let tests = testList "Curves and stacks" [
    testCase "spline control geometry matches pinned D3 fixtures" <| fun _ ->
        use doc=JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","curves.json")))
        let numberPattern= @"-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?"
        let numbers text=Regex.Matches(text,numberPattern) |> Seq.map (fun m -> Double.Parse(m.Value,CultureInfo.InvariantCulture)) |> Seq.toList
        for item in doc.RootElement.EnumerateArray() do
            let name=item.GetProperty("name").GetString()
            let curve =
                match name with
                | "curveLinear" -> Some CurveSpec.Linear
                | "curveBasis" -> Some(CurveSpec.Basis CurveBoundary.Clamped)
                | "curveBasisOpen" -> Some(CurveSpec.Basis CurveBoundary.Open)
                | "curveBasisClosed" -> Some(CurveSpec.Basis CurveBoundary.Closed)
                | "curveCardinal" -> Some(CurveSpec.Cardinal(0.,CurveBoundary.Clamped))
                | "curveCardinalOpen" -> Some(CurveSpec.Cardinal(0.,CurveBoundary.Open))
                | "curveCardinalClosed" -> Some(CurveSpec.Cardinal(0.,CurveBoundary.Closed))
                | "curveCatmullRom" -> Some(CurveSpec.CatmullRom(0.5,CurveBoundary.Clamped))
                | "curveCatmullRomOpen" -> Some(CurveSpec.CatmullRom(0.5,CurveBoundary.Open))
                | "curveCatmullRomClosed" -> Some(CurveSpec.CatmullRom(0.5,CurveBoundary.Closed))
                | "curveNatural" -> Some CurveSpec.Natural
                | "curveMonotoneX" -> Some CurveSpec.MonotoneX
                | "curveMonotoneY" -> Some CurveSpec.MonotoneY
                | "curveBumpX" -> Some CurveSpec.BumpX
                | "curveBumpY" -> Some CurveSpec.BumpY
                | "curveBundle" -> Some(CurveSpec.Bundle 0.85)
                | _ -> None
            curve |> Option.iter (fun curve ->
                let points=item.GetProperty("points").EnumerateArray() |> Seq.map (fun point -> Some(Point.create (point[0].GetDouble()) (point[1].GetDouble()))) |> Seq.toList
                let expected=if item.GetProperty("path").ValueKind=JsonValueKind.Null then "" else item.GetProperty("path").GetString()
                let actual=Curves.line curve points |> Path.toSvg
                let expectedNumbers,actualNumbers=numbers expected,numbers actual
                Expect.equal actualNumbers.Length expectedNumbers.Length name
                List.iter2 (fun expected actual -> Expect.isLessThanOrEqual (abs(expected-actual)) 1e-9 name) expectedNumbers actualNumbers)

    testCase "stack offsets match pinned D3 fixtures" <| fun _ ->
        use doc=JsonDocument.Parse(File.ReadAllText(Path.Combine(AppContext.BaseDirectory,"fixtures","stacks.json")))
        for item in doc.RootElement.EnumerateArray() do
            let offset=
                match item.GetProperty("name").GetString() with
                | "stackOffsetNone" -> StackOffset.Zero
                | "stackOffsetExpand" -> StackOffset.Expand
                | "stackOffsetSilhouette" -> StackOffset.Silhouette
                | "stackOffsetWiggle" -> StackOffset.Wiggle
                | _ -> StackOffset.Diverging
            let rows=item.GetProperty("values").EnumerateArray() |> Seq.map (fun row -> row.EnumerateArray() |> Seq.map (fun v -> v.GetDouble()) |> Seq.toArray) |> Seq.toArray
            let series=[for i in 0..2 -> {Series.Key=string i;Values=rows |> Array.map (fun row -> row[i]) |> Array.toList}]
            let actual=Stack.layout {Order=StackOrder.Input;Offset=offset} series |> Result.defaultWith (fun e -> failwithf "%A" e)
            for i,layer in List.indexed actual do
                for j,(lo,hi) in List.indexed layer.Values do
                    let expected=(item.GetProperty("layers").[i]).[j]
                    DataScaleTests.near (expected[0].GetDouble()) lo
                    DataScaleTests.near (expected[1].GetDouble()) hi
    testCase "monotone controls stay within each monotone interval" <| fun _ ->
        let points=[Point.create 0. 0.;Point.create 1. 1.;Point.create 2. 1.;Point.create 3. 4.]
        let commands=Curves.line CurveSpec.MonotoneX (List.map Some points) |> Path.commands
        let mutable previous=List.head points
        for command in commands do
            match command with
            | CubicTo(a,b,last) ->
                for control in [a;b] do
                    Expect.isGreaterThanOrEqual control.Y previous.Y "no undershoot"
                    Expect.isLessThanOrEqual control.Y last.Y "no overshoot"
                previous<-last
            | _ -> ()
    testCase "filled added symbols preserve requested area" <| fun _ ->
        for kind in [SymbolSpec.Star;SymbolSpec.Cross;SymbolSpec.Wye;SymbolSpec.TriangleDown] do
            let vertices=Shapes.symbol kind 100. |> Path.commands |> List.choose (function MoveTo p | LineTo p -> Some p | _ -> None)
            let closed=vertices @ [List.head vertices]
            let area=List.pairwise closed |> List.sumBy (fun (a,b) -> a.X*b.Y-a.Y*b.X) |> abs |> fun x -> x/2.
            DataScaleTests.near 100. area
    testCase "rounded arcs stay finite for thin tiny and full rings" <| fun _ ->
        for inner,outer,finish in [0.,10.,1.;9.9,10.,0.001;5.,10.,-1.;5.,10.,2.*Math.PI] do
            let path=Shapes.arc {InnerRadius=inner;OuterRadius=outer;StartAngle=0.;EndAngle=finish;PadAngle=0.;CornerRadius=100.}
            let text=Path.toSvg path
            Expect.isFalse (text.Contains "NaN" || text.Contains "Infinity") "finite corners"
        Expect.equal (Shapes.arc {InnerRadius=0.;OuterRadius=10.;StartAngle=0.;EndAngle=0.1;PadAngle=0.2;CornerRadius=2.} |> Path.commands) [] "padding collapses wedge"
    testCase "new curves respect missing runs" <| fun _ ->
        for curve in [CurveSpec.Natural;CurveSpec.Basis CurveBoundary.Clamped;CurveSpec.CatmullRom(0.5,CurveBoundary.Clamped);CurveSpec.BumpY] do
            let path=Curves.line curve [Some(Point.create 0. 0.);Some(Point.create 1. 1.);None;Some(Point.create 4. 3.);Some(Point.create 5. 4.)]
            Expect.equal (Path.commands path |> List.filter (function MoveTo _ -> true | _ -> false) |> List.length) 2 "two subpaths"
    testCase "expand stack conserves one and rejects negative values" <| fun _ ->
        let config={StackConfig.Order=StackOrder.Input;Offset=StackOffset.Expand}
        let data=[{Series.Key="a";Values=[1.;2.]};{Series.Key="b";Values=[3.;2.]}]
        let layers=Stack.layout config data |> Result.defaultWith (fun e -> failwithf "%A" e)
        Expect.equal (List.last layers).Values [(0.25,1.);(0.5,1.)] "normalized totals"
        Expect.isError (Stack.layout config [{Key="negative";Values=[-1.]}]) "invalid negative"
    testCase "wiggle and silhouette preserve thickness" <| fun _ ->
        let data=[{Series.Key="a";Values=[1.;2.;3.]};{Series.Key="b";Values=[3.;4.;1.]}]
        for offset in [StackOffset.Wiggle;StackOffset.Silhouette] do
            let layers=Stack.layout {Order=StackOrder.Input;Offset=offset} data |> Result.defaultWith (fun e -> failwithf "%A" e)
            for source,layer in List.zip data layers do
                List.iter2 (fun expected (lo,hi) -> DataScaleTests.near expected (hi-lo)) source.Values layer.Values
]

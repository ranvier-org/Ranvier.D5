module Ranvier.D5.Tests.QuadtreeTests

open System
open Expecto
open Ranvier.D5

let private p x y = Point.create x y
let private extent x0 y0 x1 y1 = {Min=p x0 y0;Max=p x1 y1}
let tests = testList "Quadtree" [
    testCase "empty and inclusive query boundaries" <| fun _ ->
        let empty=Quadtree.build id (fun _ -> 1.) []
        Expect.equal (Quadtree.nearest (p 0. 0.) infinity empty) None "empty"
        let data=[p 0. 0.;p 2. 2.;p 1. 1.;p -1. 0.]
        let tree=Quadtree.build id (fun _ -> 1.) data
        Expect.equal (Quadtree.within (extent 0. 0. 1. 1.) tree) [p 0. 0.;p 1. 1.] "inclusive and input ordered"
        Expect.equal (Quadtree.nearest (p 1. 0.) 1. tree) (Some(p 0. 0.)) "input order resolves ties"
        Expect.equal (Quadtree.nearest (p 1. 0.) 0.5 tree) None "radius excludes"
    testCase "coincident buckets terminate and retain all identities" <| fun _ ->
        let tree=Quadtree.build (fun _ -> p 3. 4.) (fun _ -> 1.) [0..99999]
        let cells=ResizeArray<_>()
        Quadtree.visit (fun cell -> cells.Add cell;true) tree
        Expect.equal cells.Count 1 "no meaningless subdivision"
        Expect.equal cells[0].Count 100000 "all records retained"
        Expect.equal cells[0].Mass 100000. "mass"
        Expect.equal (Quadtree.nearest (p 3. 4.) 0. tree) (Some 0) "first coincident identity"
    testCase "mass summaries and pruning support aggregate consumers" <| fun _ ->
        let data=[for i in 0..99 -> p (float i) 0.,float(i+1)]
        let tree=Quadtree.build fst snd data
        let cells=ResizeArray<_>()
        Quadtree.visit (fun cell -> cells.Add cell;false) tree
        Expect.equal cells.Count 1 "prune root"
        Expect.equal cells[0].Mass 5050. "total mass"
        Expect.floatClose Accuracy.high cells[0].Center.X 66. "weighted centroid"
        let mutable leafCount=0
        Quadtree.visit (fun cell -> leafCount<-leafCount+cell.Items.Length;true) tree
        Expect.equal leafCount 100 "every item occurs in exactly one leaf"
    testCase "100k grid queries agree with exhaustive search" <| fun _ ->
        let data=[|for i in 0..99999 -> p (float(i%1000)) (float(i/1000))|]
        let tree=Quadtree.build id (fun _ -> 1.) data
        for i in 0..49 do
            let q=p (float(i*19)+0.2) (float(i%100)+0.3)
            let expected=data |> Array.minBy(fun v -> (v.X-q.X)**2.+(v.Y-q.Y)**2.)
            Expect.equal (Quadtree.nearest q infinity tree) (Some expected) "nearest matches brute force"
        Expect.equal (Quadtree.within (extent 19. 20. 21. 22.) tree |> List.length) 9 "range query"
    testCase "extreme and subnormal coordinates preserve nearest ordering" <| fun _ ->
        let huge=Quadtree.build id (fun _ -> 1.) [p 1e308 0.;p 9e307 0.]
        Expect.equal (Quadtree.nearest (p -1e308 0.) infinity huge) (Some(p 9e307 0.)) "overflowing differences"
        Expect.equal (Quadtree.nearest (p -1e308 0.) Double.MaxValue huge) None "finite radius cannot reach"
        let tiny=Quadtree.build id (fun _ -> 1.) [p Double.Epsilon Double.Epsilon;p Double.Epsilon 0.]
        Expect.equal (Quadtree.nearest (p 0. 0.) infinity tiny) (Some(p Double.Epsilon 0.)) "rounded hypot tie is not true tie"
        Expect.equal (Quadtree.nearest (p 0. 0.) 0. tiny) None "positive subnormal distance"
        let extremes=Quadtree.build id (fun _ -> 1.) [p -Double.MaxValue 0.;p Double.MaxValue 0.]
        Quadtree.visit (fun c -> Expect.isTrue (Double.IsFinite c.Center.X) "finite center";true) extremes
    testCase "position and mass are captured once and input arrays are not retained" <| fun _ ->
        let mutable calls=0
        let source=[|p 1. 1.;p 2. 2.|]
        let tree=Quadtree.build (fun v -> calls<-calls+1;v) (fun _ -> 1.) source
        source[0]<-p 999. 999.
        Expect.equal calls 2 "accessor evaluated once"
        Expect.equal (Quadtree.nearest (p 1. 1.) 0. tree) (Some(p 1. 1.)) "snapshot retained"
    testCase "skewed exponents stay bounded without losing points" <| fun _ ->
        let data=[for i in -1000..10..1000 -> p (Math.ScaleB(1.,i)) 0.]
        let tree=Quadtree.build id (fun _ -> 1.) data
        let mutable count=0
        Quadtree.visit (fun cell ->
            Expect.isLessThanOrEqual cell.Depth 64 "bounded depth"
            count<-count+cell.Items.Length
            true) tree
        Expect.equal count data.Length "all exponent bands retained"
        for point in data do Expect.equal (Quadtree.nearest point 0. tree) (Some point) "exact lookup at every scale"
    testCase "cancellation interrupts coordinate capture at a bounded checkpoint" <| fun _ ->
        use source=new System.Threading.CancellationTokenSource()
        let mutable calls=0
        Expect.throwsT<OperationCanceledException> (fun () ->
            Quadtree.buildWithCancellation source.Token (fun i ->
                calls<-calls+1
                if calls=100 then source.Cancel()
                p (float i) 0.) (fun _ -> 1.) (seq{0..99999}) |> ignore) "cancelled during build"
        Expect.isLessThanOrEqual calls 164 "at most64 further accessor calls"
    testCase "invalid masses and inverted extents are rejected" <| fun _ ->
        Expect.throws (fun () -> Quadtree.build id (fun _ -> -1.) [p 0. 0.] |> ignore) "negative mass"
        Expect.throws (fun () -> Quadtree.build id (fun _ -> Double.NaN) [p 0. 0.] |> ignore) "nonfinite mass"
        Expect.throws (fun () -> Quadtree.build id (fun _ -> Double.MaxValue) [p 0. 0.;p 1. 1.] |> ignore) "mass overflow"
        let tree=Quadtree.build id (fun _ -> 0.) [p 0. 0.;p 2. 2.]
        Quadtree.visit (fun c -> Expect.equal c.Center (p 1. 1.) "zero mass uses extent center";false) tree
        Expect.throws (fun () -> Quadtree.within (extent 2. 0. 1. 1.) tree |> ignore) "inverted range"
]

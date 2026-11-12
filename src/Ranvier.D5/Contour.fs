namespace Ranvier.D5

type ContourPolygon = { Threshold:float; Outer:Point list; Holes:Point list list }
type ContourConfig =
    { Thresholds:float list; Interpolate:bool; MaxCells:int; MaxVertices:int; MaxPolygons:int; MaxWork:int64 }

[<RequireQualifiedAccess>]
module ContourConfig =
    let create thresholds =
        {Thresholds=thresholds;Interpolate=true;MaxCells=1048576;MaxVertices=2000000;MaxPolygons=250000;MaxWork=100000000L}

[<RequireQualifiedAccess>]
module Contour =
    exception private ContourAbort of LayoutError
    type private Edge = (struct (int*int*bool))

    let private saddle a b c d threshold =
        let aa,bb,cc,dd=a-threshold,b-threshold,c-threshold,d-threshold
        let left,right=aa*cc,bb*dd
        let determinant=left-right
        let bound=(abs left+abs right)*1e-14+16.*System.Double.Epsilon
        if System.Double.IsFinite determinant && System.Double.IsFinite bound && abs determinant>bound then sign determinant
        else
            let values,_=ExactDyadic.integers [|a;b;c;d;threshold|]
            ((values[0]-values[4])*(values[2]-values[4])-(values[1]-values[4])*(values[3]-values[4])).Sign

    let private fraction a b threshold =
        let numerator,denominator=threshold-a,b-a
        if System.Double.IsFinite numerator && System.Double.IsFinite denominator then numerator/denominator
        else
            let values,_=ExactDyadic.integers [|a;b;threshold|]
            ExactDyadic.ratio (values[2]-values[0]) (values[1]-values[0]) 0

    let private signedArea (points:Point list) = Polygon.area points

    let private simplify (token:System.Threading.CancellationToken) (ring:Point list) =
        let points=ring |> List.take (ring.Length-1) |> List.toArray
        let kept=ResizeArray<Point>()
        for i in 0..points.Length-1 do
            if i%4096=0 then token.ThrowIfCancellationRequested()
            let previous,current,next=points[(i+points.Length-1)%points.Length],points[i],points[(i+1)%points.Length]
            let between=current.X>=min previous.X next.X && current.X<=max previous.X next.X && current.Y>=min previous.Y next.Y && current.Y<=max previous.Y next.Y
            if not between || Predicates.orient2d previous current next<>0 then kept.Add current
        if kept.Count<3 then [] else List.ofSeq kept @ [kept[0]]

    let computeWithCancellation (token:System.Threading.CancellationToken) (config:ContourConfig) (grid:DensityGrid) : Result<ContourPolygon list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let columns,rows=DensityGrid.columns grid,DensityGrid.rows grid
        let cellWork=(int64 columns+1L)*(int64 rows+1L)
        if config.MaxCells<=0 || config.MaxVertices<=0 || config.MaxPolygons<=0 || config.MaxWork<=0L then
            Error [InvalidObservation "Contour budgets must be positive."]
        elif config.Thresholds |> List.exists(System.Double.IsFinite >> not) then
            Error [InvalidObservation "Contour thresholds must be finite."]
        elif Set.count(Set.ofList config.Thresholds)<>config.Thresholds.Length then
            Error [InvalidObservation "Contour thresholds must be unique."]
        elif int64 columns*int64 rows>int64 config.MaxCells || int64 config.Thresholds.Length>config.MaxWork/cellWork then
            Error [BudgetExceeded "Contour grid or level count exceeds the work budget."]
        else
          try
            let values,observed=DensityGrid.copySamples grid
            let sample x y=
                if x<0 || x>=columns || y<0 || y>=rows then struct(0.,false)
                else let index=y*columns+x in struct(values[index],observed[index])
            let mutable work=cellWork*int64 config.Thresholds.Length
            let mutable vertices=0
            let charge amount =
                if amount>config.MaxWork-work then raise(ContourAbort(BudgetExceeded "Contour topology work exceeds its budget."))
                work<-work+amount
            let output=ResizeArray<ContourPolygon>()
            for threshold in List.sort config.Thresholds do
                token.ThrowIfCancellationRequested()
                let points=System.Collections.Generic.Dictionary<Edge,Point>()
                let links=System.Collections.Generic.Dictionary<Edge,Edge>()
                let crossing (struct(x,y,horizontal) as key:Edge) =
                    match points.TryGetValue key with
                    | true,point -> point
                    | _ ->
                        let x2,y2=if horizontal then x+1,y else x,y+1
                        let t=
                            match sample x y,sample x2 y2 with
                            | struct(a,true),struct(b,true) when config.Interpolate -> max 0. (min 1. (fraction a b threshold))
                            | _ -> 0.5
                        let point=Point.create (max 0. (min (float columns) (float x+0.5+(if horizontal then t else 0.))))
                                               (max 0. (min (float rows) (float y+0.5+(if horizontal then 0. else t))))
                        points.Add(key,point)
                        point
                let add first last =
                    if links.Count>=config.MaxVertices-vertices then raise(ContourAbort(BudgetExceeded "Contour vertices exceed the output budget."))
                    crossing first |> ignore
                    crossing last |> ignore
                    if not(links.TryAdd(first,last)) then raise(ContourAbort(InvalidTopology "A contour edge has multiple outgoing segments."))
                for y in -1..rows-1 do
                    token.ThrowIfCancellationRequested()
                    for x in -1..columns-1 do
                        if (x+1)%4096=0 then token.ThrowIfCancellationRequested()
                        let a,b,c,d=sample x y,sample (x+1) y,sample (x+1) (y+1),sample x (y+1)
                        let high (struct(value,present)) = present && value>=threshold
                        let code=(if high a then 1 else 0)+(if high b then 2 else 0)+(if high c then 4 else 0)+(if high d then 8 else 0)
                        if code<>0 && code<>15 then
                            let top,right,bottom,left=struct(x,y,true),struct(x+1,y,false),struct(x,y+1,true),struct(x,y,false)
                            let decision ()=
                                match a,b,c,d with
                                | struct(a,true),struct(b,true),struct(c,true),struct(d,true) -> saddle a b c d threshold
                                | _ -> 0
                            match code with
                            | 1 -> add top left
                            | 2 -> add right top
                            | 3 -> add right left
                            | 4 -> add bottom right
                            | 5 when decision()>0 -> add top right;add bottom left
                            | 5 -> add top left;add bottom right
                            | 6 -> add bottom top
                            | 7 -> add bottom left
                            | 8 -> add left bottom
                            | 9 -> add top bottom
                            | 10 when decision()<0 -> add left top;add right bottom
                            | 10 -> add right top;add left bottom
                            | 11 -> add right bottom
                            | 12 -> add left right
                            | 13 -> add top right
                            | 14 -> add left top
                            | _ -> ()
                let rings=ResizeArray<Point list*float>()
                let split (ring:ResizeArray<Point>) =
                    let path=ResizeArray<Point>()
                    let positions=System.Collections.Generic.Dictionary<Point,int>()
                    for point in ring do
                        if path.Count%4096=0 then token.ThrowIfCancellationRequested()
                        match positions.TryGetValue point with
                        | true,index ->
                            let count=path.Count-index
                            if count>=3 then
                                let cycle=([for i in index..path.Count-1 -> path[i]] @ [point]) |> simplify token
                                let area=if cycle.IsEmpty then 0. else signedArea cycle
                                if area<>0. then
                                    vertices<-vertices+cycle.Length
                                    if vertices>config.MaxVertices then raise(ContourAbort(BudgetExceeded "Closed contour vertices exceed the output budget."))
                                    rings.Add(cycle,area)
                            for i in index+1..path.Count-1 do positions.Remove path[i] |> ignore
                            path.RemoveRange(index+1,path.Count-index-1)
                        | _ -> positions.Add(point,path.Count);path.Add point
                let starts=links.Keys |> Seq.toArray |> Array.sort
                for start in starts do
                    if links.ContainsKey start then
                        let ring=ResizeArray<Point>()
                        let mutable current=start
                        let mutable closed=false
                        while not closed do
                            if ring.Count%4096=0 then token.ThrowIfCancellationRequested()
                            charge 1L
                            ring.Add points[current]
                            match links.TryGetValue current with
                            | false,_ -> raise(ContourAbort(InvalidTopology "Contour stitching encountered an open ring."))
                            | true,next ->
                                links.Remove current |> ignore
                                current<-next
                                closed<-current=start
                        ring.Add points[start]
                        split ring
                let shells=rings |> Seq.filter(fun (_,area) -> area>0.) |> Seq.toArray
                if output.Count+shells.Length>config.MaxPolygons then raise(ContourAbort(BudgetExceeded "Contour polygon count exceeds its budget."))
                let holes=Array.init shells.Length (fun _ -> ResizeArray<Point list>())
                let bounds ring =
                    let xs=ring |> List.map(fun p -> p.X)
                    let ys=ring |> List.map(fun p -> p.Y)
                    List.min xs,List.min ys,List.max xs,List.max ys
                let boxes=shells |> Array.map(fst >> bounds)
                let shellArrays=shells |> Array.map(fst >> List.toArray)
                let candidates=[|0..shells.Length-1|] |> Array.sortBy(fun i -> snd shells[i])
                for hole,area in rings do
                    if area<0. then
                        token.ThrowIfCancellationRequested()
                        let hx,hy,hx2,hy2=bounds hole
                        let mutable parent = -1
                        let mutable index=0
                        while parent<0 && index<candidates.Length do
                            token.ThrowIfCancellationRequested()
                            charge 1L
                            let candidate=candidates[index]
                            let x,y,x2,y2=boxes[candidate]
                            if x<=hx && y<=hy && x2>=hx2 && y2>=hy2 then
                                let shell=fst shells[candidate]
                                let contains point=
                                    token.ThrowIfCancellationRequested()
                                    charge(int64 shell.Length)
                                    Polygon.containsArrayWithCancellation token point shellArrays[candidate]
                                if hole |> List.forall contains then parent<-candidate
                            index<-index+1
                        if parent<0 then raise(ContourAbort(InvalidTopology "Contour hole has no enclosing shell."))
                        holes[parent].Add hole
                let extent=DensityGrid.bounds grid
                let mapped=System.Collections.Generic.Dictionary<Point,Point>()
                let world point =
                    let result=Point.create (Bounds.x extent+Bounds.width extent*(point.X/float columns))
                                            (Bounds.y extent+Bounds.height extent*(point.Y/float rows))
                    match mapped.TryGetValue result with
                    | true,prior when prior<>point -> raise(ContourAbort(InvalidObservation "Contour coordinates collapse at this extent's numeric precision."))
                    | _ -> mapped[result]<-point
                    result
                for i in 0..shells.Length-1 do
                    token.ThrowIfCancellationRequested()
                    output.Add {Threshold=threshold;Outer=fst shells[i] |> List.map world;Holes=holes[i] |> Seq.map(List.map world) |> List.ofSeq}
            token.ThrowIfCancellationRequested()
            Ok(List.ofSeq output)
          with ContourAbort error -> Error [error]
    let compute config grid = computeWithCancellation System.Threading.CancellationToken.None config grid

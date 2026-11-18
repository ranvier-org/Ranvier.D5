namespace Ranvier.D5.Charts

open Ranvier.D5

/// <summary>Filled contour and isoband charts over a density grid.</summary>
[<RequireQualifiedAccess>]
module ContourChart =
    exception private CompositionAbort of LayoutError

    let private rings (polygons:HitPolygon list) =
        polygons |> List.collect(fun polygon -> polygon.Outer::polygon.Holes)

    let private interior tick included excluded =
        let boundaries=rings included @ rings excluded
        let yBuf=ResizeArray<float>()
        let edgeBuf=ResizeArray<struct(Point*Point)>()
        let mutable vertexCount=0
        for b in boundaries do
            match b with
            | [] -> ()
            | first::rest ->
                yBuf.Add first.Y
                vertexCount<-vertexCount+1
                let mutable prev=first
                for cur in rest do
                    yBuf.Add cur.Y
                    vertexCount<-vertexCount+1
                    edgeBuf.Add(struct(prev,cur))
                    prev<-cur
        tick (int64 vertexCount * int64(ceil(System.Math.Log2(float(max 2 vertexCount)))))
        yBuf.Sort()
        let ys =
            if yBuf.Count=0 then [||]
            else
                let dedup=ResizeArray<float>(yBuf.Count)
                let mutable lastY=yBuf[0]
                dedup.Add lastY
                for i=1 to yBuf.Count-1 do
                    let v=yBuf[i]
                    if v<>lastY then
                        dedup.Add v
                        lastY<-v
                dedup.ToArray()
        let edges=edgeBuf.ToArray()
        let geometry=HitRegion(included,excluded,Point.create 0. 0.)
        let xsBuf=ResizeArray<float>(32)
        let mutable answer=None
        let mutable row=0
        while answer.IsNone && row<ys.Length-1 do
            let y=ys[row]+(ys[row+1]-ys[row])*0.5
            tick (int64 edges.Length)
            if y>ys[row] && y<ys[row+1] then
                xsBuf.Clear()
                for eIdx=0 to edges.Length-1 do
                    let struct(a,b)=edges[eIdx]
                    if (a.Y<=y && b.Y>y) || (b.Y<=y && a.Y>y) then
                        xsBuf.Add(a.X+(b.X-a.X)*((y-a.Y)/(b.Y-a.Y)))
                if xsBuf.Count>=2 then
                    xsBuf.Sort()
                    let mutable writeIdx=1
                    for readIdx=1 to xsBuf.Count-1 do
                        let v=xsBuf[readIdx]
                        if v<>xsBuf[writeIdx-1] then
                            xsBuf[writeIdx]<-v
                            writeIdx<-writeIdx+1
                    let mutable column=0
                    while answer.IsNone && column<writeIdx-1 do
                        let x0=xsBuf[column]
                        let x1=xsBuf[column+1]
                        let x=x0+(x1-x0)*0.5
                        tick (int64 vertexCount)
                        if x>x0 && x<x1 then
                            let point=Point.create x y
                            if HitTest.contains point geometry then answer<-Some point
                        column<-column+1
            row<-row+1
        answer

    let private build (token:System.Threading.CancellationToken) bands (spec:DensityChartSpec) (config:ContourConfig) (grid:DensityGrid) =
        token.ThrowIfCancellationRequested()
        if spec.MaxMarks<1 then Error [InvalidObservation "Mark budget must be positive."]
        else
          Contour.computeWithCancellation token config grid
          |> Result.bind(fun polygons ->
            try
              Recipe.run spec.Frame id [] (fun bounds ->
                let mutable work=0L
                let tick amount =
                    token.ThrowIfCancellationRequested()
                    if amount>config.MaxWork-work then raise(CompositionAbort(BudgetExceeded "Contour composition exceeds the work budget."))
                    work<-work+amount
                let extent=DensityGrid.bounds grid
                let number (value:float)=value.ToString("R",System.Globalization.CultureInfo.InvariantCulture)
                let scope=Recipe.key [(if bands then "isoband" else "contour");number(Bounds.x extent);number(Bounds.y extent);number(Bounds.width extent);number(Bounds.height extent)]
                let bx=Bounds.x bounds
                let by=Bounds.y bounds
                let bw=Bounds.width bounds
                let bh=Bounds.height bounds
                let ex=Bounds.x extent
                let ey=Bounds.y extent
                let ew=Bounds.width extent
                let eh=Bounds.height extent
                let inline project (p:Point)=
                    Point.create (bx+(p.X-ex)/ew*bw) (by+(p.Y-ey)/eh*bh)
                let ring (points:Point list) =
                    let arr=List.toArray points
                    tick (int64 arr.Length)
                    let projected=Array.zeroCreate<Point> arr.Length
                    for index=0 to arr.Length-1 do
                        if (index &&& 63)=0 then token.ThrowIfCancellationRequested()
                        let p=project arr[index]
                        if index>0 && p=projected[index-1] then
                            invalidArg "frame" "Contour vertices collapse at the requested coordinate precision."
                        projected[index]<-p
                    Array.toList projected
                let groups=polygons |> List.groupBy(fun p -> p.Threshold) |> List.map(fun (level,polys) ->
                    level,polys |> List.map(fun p -> {HitPolygon.Outer=ring p.Outer;Holes=List.map ring p.Holes})) |> Map.ofList
                let levels=config.Thresholds |> List.sort |> List.toArray
                let marks,semantics,hits,membership=ResizeArray(),ResizeArray(),ResizeArray(),ResizeArray()
                let columns,rows=DensityGrid.columns grid,DensityGrid.rows grid
                // Single-pass compaction of non-empty valued cells (proven in SamplingSelection.v: contour_compact_nonempty_cells_membership_eq)
                let mutable populatedCells:struct(float*string list) array voption=ValueNone
                let getPopulatedCells () =
                    match populatedCells with
                    | ValueSome arr -> arr
                    | ValueNone ->
                        let buf=ResizeArray<struct(float*string list)>()
                        for row=0 to rows-1 do
                            for column=0 to columns-1 do
                                if (column &&& 63)=0 then token.ThrowIfCancellationRequested()
                                let source=DensityGrid.members column row grid
                                if not source.IsEmpty then
                                    match DensityGrid.value column row grid with
                                    | Some value -> buf.Add(struct(value,source))
                                    | None -> ()
                        let arr=buf.ToArray()
                        populatedCells<-ValueSome arr
                        arr
                for index in 0..levels.Length-1 do
                    token.ThrowIfCancellationRequested()
                    let level=levels[index]
                    let upper=if bands && index+1<levels.Length then Some levels[index+1] else None
                    let included=Map.tryFind level groups |> Option.defaultValue []
                    let excluded=upper |> Option.bind(fun value -> Map.tryFind value groups) |> Option.defaultValue []
                    if not included.IsEmpty then
                        match interior tick included excluded with
                        | None when included=excluded -> ()
                        | None -> invalidArg "frame" "Contour band has no representable interior selection point."
                        | Some anchor ->
                            if marks.Count>=spec.MaxMarks then raise(CompositionAbort(BudgetExceeded "Contour levels exceed the visible mark budget."))
                            let key=Recipe.key3 scope (number level) (upper |> Option.map number |> Option.defaultValue "unbounded")
                            let mutable cmdsRev=[]
                            for r in rings included do
                                match r with
                                | [] -> ()
                                | first::rest ->
                                    cmdsRev<-MoveTo first::cmdsRev
                                    for p in rest do cmdsRev<-LineTo p::cmdsRev
                                    cmdsRev<-Close::cmdsRev
                            for r in rings excluded do
                                let revRing=List.rev r
                                match revRing with
                                | [] -> ()
                                | first::rest ->
                                    cmdsRev<-MoveTo first::cmdsRev
                                    for p in rest do cmdsRev<-LineTo p::cmdsRev
                                    cmdsRev<-Close::cmdsRev
                            let combinedPath=Path.ofCommands(List.rev cmdsRev)
                            let label=
                                match upper with
                                | Some limit -> sprintf "Density %s to %s (upper excluded)" (Recipe.number level) (Recipe.number limit)
                                | None -> sprintf "Density at least %s" (Recipe.number level)
                            let mark=Mark.path key combinedPath |> Mark.withFill(spec.Color level) |> Mark.withLabel(Some label)
                            let mark=if spec.Selected.Contains key then Mark.withStroke (Paint.rgb 20 20 20) 2. mark else mark
                            marks.Add mark
                            semantics.Add(Recipe.semantic key [key] label)
                            hits.Add(Recipe.hit key [key] (HitRegion(included,excluded,anchor)))
                            tick (int64 columns*int64 rows)
                            let compact=getPopulatedCells()
                            let members =
                                if compact.Length=0 then []
                                else
                                    let out=ResizeArray<string>()
                                    match upper with
                                    | Some limit ->
                                        for cIdx=0 to compact.Length-1 do
                                            if (cIdx &&& 63)=0 then token.ThrowIfCancellationRequested()
                                            let struct(v,src)=compact[cIdx]
                                            if v>=level && v<limit then
                                                for s in src do out.Add s
                                    | None ->
                                        for cIdx=0 to compact.Length-1 do
                                            if (cIdx &&& 63)=0 then token.ThrowIfCancellationRequested()
                                            let struct(v,src)=compact[cIdx]
                                            if v>=level then
                                                for s in src do out.Add s
                                    List.ofSeq out
                            membership.Add(key,members)
                token.ThrowIfCancellationRequested()
                {Chart=Recipe.output spec.Frame bounds (List.ofSeq marks) (List.ofSeq semantics) (List.ofSeq hits) []
                 Membership=Map.ofSeq membership})
            with CompositionAbort error -> Error [error])

    /// <summary>As contours, but checks the token throughout and throws OperationCanceledException when it is cancelled.</summary>
    let contoursWithCancellation token spec config grid = build token false spec config grid
    /// <summary>As isobands, but checks the token throughout and throws OperationCanceledException when it is cancelled.</summary>
    let isobandsWithCancellation token spec config grid = build token true spec config grid
    /// <summary>Draws one filled region per threshold covering density at or above it, stretched to the plot area; Membership lists observations in qualifying grid cells, and exceeding MaxMarks or the config work budget is BudgetExceeded.</summary>
    let contours spec config grid = contoursWithCancellation System.Threading.CancellationToken.None spec config grid
    /// <summary>Like contours, but each band excludes the next higher threshold, so its region and Membership cover values in [level, next level).</summary>
    let isobands spec config grid = isobandsWithCancellation System.Threading.CancellationToken.None spec config grid

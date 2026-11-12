namespace Ranvier.D5.Geo

open System.Threading
open Ranvier.D5

/// <summary>Builds bounded toolkit paths from planar straight-edge geometry.</summary>
module PlanarPath =
    let private orientForClip spend (rings:Point array list) =
        match rings with
        | outer::_ when outer.Length>2 ->
            let mutable extreme=0
            for index=1 to outer.Length-1 do
                spend 1L
                let a,b=outer[index],outer[extreme]
                if a.X<b.X || (a.X=b.X && a.Y<b.Y) then extreme<-index
            let mutable before=(extreme+outer.Length-1)%outer.Length
            let mutable after=(extreme+1)%outer.Length
            while before<>extreme && outer[before]=outer[extreme] do
                spend 1L
                before<-(before+outer.Length-1)%outer.Length
            while after<>extreme && outer[after]=outer[extreme] do
                spend 1L
                after<-(after+1)%outer.Length
            // At the lexicographic extreme of a simple ring, the exact turn gives its winding.
            if Predicates.orient2d outer[before] outer[extreme] outer[after]<0 then
                for ring in rings do
                    for index=0 to ring.Length/2-1 do
                        spend 1L
                        let other=ring.Length-1-index
                        let value=ring[index]
                        ring[index]<-ring[other]
                        ring[other]<-value
        | _ -> ()

    /// <summary>Transforms and clips planar geometry using the shared path budgets and cancellation token.</summary>
    let projectWithCancellation (options:GeoPathOptions) (token:CancellationToken) projection geometry : Result<Path list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        let limits=options.InputLimits
        if not(ProjectedPath.validOptions options) || limits.MaxPoints<1 || limits.MaxWork<1L || limits.MaxDepth<1 || limits.MaxDepth>128 then
            Error [InvalidObservation "Planar paths require valid finite options, positive budgets and input depth in [1,128]."]
        else
            try
                let reject error = raise(ProjectedPath.Rejected error)
                let paths=ResizeArray<Path>()
                let mutable work=0L
                let mutable inputWork=0L
                let mutable inputCount=0
                let mutable emitted=0
                let spend amount =
                    token.ThrowIfCancellationRequested()
                    if amount>options.MaxWork-work then reject(BudgetExceeded "Planar path exceeds its work budget.")
                    work<-work+amount
                let visit () =
                    token.ThrowIfCancellationRequested()
                    if inputWork>=limits.MaxWork then reject(BudgetExceeded "Planar input exceeds its work budget.")
                    inputWork<-inputWork+1L
                    spend 1L
                let reserve count =
                    spend(int64 count)
                    if count>options.MaxOutputPoints-emitted then reject(BudgetExceeded "Planar path exceeds its output-point budget.")
                    emitted<-emitted+count
                let checkBuffer count =
                    if count>options.MaxBufferedPoints then reject(BudgetExceeded "Planar path exceeds its vertex buffer budget.")
                let emptyViewport=options.ClipExtent |> Option.exists(fun clip -> Bounds.width clip=0. || Bounds.height clip=0.)
                let input (point:Point) =
                    visit()
                    if inputCount>=limits.MaxPoints then reject(BudgetExceeded "Planar input exceeds its point budget.")
                    inputCount<-inputCount+1
                    if not(System.Double.IsFinite point.X && System.Double.IsFinite point.Y) then
                        reject(InvalidObservation "Planar input coordinates must be finite.")
                let project point =
                    spend 1L
                    match IdentityProjection.projectValue projection point with
                    | ValueSome value -> value
                    | ValueNone -> reject(InvalidObservation "Planar projection exceeds representable screen coordinates.")
                let circle=ProjectedPath.circle options (fun () -> options.MaxWork-work) spend reserve paths.Add
                let publish (commands:ResizeArray<PathCommand>) =
                    if commands.Count>0 then
                        let mutable owned=[]
                        for index=commands.Count-1 downto 0 do
                            spend 1L
                            owned<-commands[index]::owned
                        paths.Add(Path.ofCommands owned)
                let traverseLine ring (positions:Point list) emit =
                    visit()
                    let mutable count=0
                    let first=List.tryHead positions
                    let mutable remaining=positions
                    while not remaining.IsEmpty do
                        let value=remaining.Head
                        remaining<-remaining.Tail
                        input value
                        count<-count+1
                        if ring && remaining.IsEmpty then
                            if first<>Some value then reject(InvalidObservation "Planar polygon rings must close at their first coordinate.")
                        elif not emptyViewport then emit(project value)
                    if (ring && count<4) || (not ring && count=1) then
                        reject(InvalidObservation "Lines require zero or at least two coordinates; polygon rings require at least four closed coordinates.")
                let line positions =
                    let commands=ResizeArray<PathCommand>()
                    let mutable previous=ValueNone
                    let mutable outputTail=ValueNone
                    let flush () =
                        publish commands
                        commands.Clear()
                        outputTail<-ValueNone
                    let segment first last =
                        match outputTail with
                        | ValueSome tail when tail=first ->
                            checkBuffer(commands.Count+1)
                            reserve 1
                        | _ ->
                            flush()
                            checkBuffer 2
                            reserve 2
                            commands.Add(MoveTo first)
                        commands.Add(LineTo last)
                        outputTail<-ValueSome last
                    traverseLine false positions (fun point ->
                        match previous with
                        | ValueSome before ->
                            match options.ClipExtent with
                            | None -> segment before point
                            | Some _ when before=point -> ()
                            | Some clip ->
                                match GeoRectangle.clipSegment spend clip before point with
                                | ValueNone -> flush()
                                | ValueSome(struct(first,last)) ->
                                    segment first last
                                    if last<>point then flush()
                        | ValueNone -> ()
                        previous<-ValueSome point)
                    flush()
                let polygon sourceRings =
                    visit()
                    let rings=ResizeArray<Point array>()
                    let working=ResizeArray<Point>()
                    let mutable buffered=0
                    for ring in sourceRings do
                        working.Clear()
                        traverseLine true ring (fun point ->
                            checkBuffer(buffered+working.Count+1)
                            working.Add point)
                        if not emptyViewport then
                            spend(int64 working.Count)
                            buffered<-buffered+working.Count
                            rings.Add(working.ToArray())
                    let mutable ownedRings=[]
                    for index=rings.Count-1 downto 0 do
                        spend 1L
                        ownedRings<-rings[index]::ownedRings
                    let visible =
                        match options.ClipExtent with
                        | None -> ownedRings
                        | Some clip ->
                            orientForClip spend ownedRings
                            match GeoRectangle.polygons options.MaxBufferedPoints spend clip ownedRings with
                            | Ok value -> value
                            | Error error -> reject error
                    let commands=ResizeArray<PathCommand>()
                    for ring in visible do
                        if ring.Length>0 then
                            reserve ring.Length
                            commands.Add(MoveTo ring[0])
                            for index=1 to ring.Length-1 do
                                spend 1L
                                commands.Add(LineTo ring[index])
                            spend 1L
                            commands.Add Close
                    publish commands
                let point value =
                    input value
                    if not emptyViewport then circle options.PointRadius (project value)
                let rec walk depth value =
                    visit()
                    if depth>limits.MaxDepth then reject(BudgetExceeded "Planar input exceeds its depth budget.")
                    match value with
                    | PlanarGeometry.Point value -> Option.iter point value
                    | PlanarGeometry.MultiPoint values -> List.iter point values
                    | PlanarGeometry.LineString values -> line values
                    | PlanarGeometry.MultiLineString values -> List.iter line values
                    | PlanarGeometry.Polygon values -> polygon values
                    | PlanarGeometry.MultiPolygon values -> List.iter polygon values
                    | PlanarGeometry.Collection values -> for child in values do walk (depth+1) child
                walk 1 geometry
                let mutable result=[]
                for index=paths.Count-1 downto 0 do
                    spend 1L
                    result<-paths[index]::result
                token.ThrowIfCancellationRequested()
                Ok result
            with ProjectedPath.Rejected error -> Error [error]
    /// <summary>Transforms and clips planar geometry without a cancellation token.</summary>
    let project options projection geometry = projectWithCancellation options CancellationToken.None projection geometry

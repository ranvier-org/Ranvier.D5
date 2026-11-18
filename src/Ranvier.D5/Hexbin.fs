namespace Ranvier.D5

open System

/// <summary>Hexagonal binning settings: bounds that must contain every point, the hexagon circumradius in layout units, and point and bin
/// budgets.</summary>
type HexbinConfig = { Bounds:Bounds; Radius:float; MaxPoints:int; MaxBins:int }
/// <summary>One occupied hexagon: its axial lattice key, center, circumradius, total member weight and member IDs in ordinal order.</summary>
type HexBin = { Key:struct(int64*int64); Center:Point; Radius:float; Weight:float; Members:string list }

/// <summary>Default hexbin configuration.</summary>
[<RequireQualifiedAccess>]
module HexbinConfig =
    /// <summary>Creates a configuration with budgets of 1,000,000 points and 1,000,000 bins.</summary>
    let create bounds radius = {Bounds=bounds;Radius=radius;MaxPoints=1000000;MaxBins=1000000}

/// <summary>Hexagonal binning of weighted points on a pointy-top lattice anchored at the bounds origin.</summary>
[<RequireQualifiedAccess>]
module Hexbin =
    exception private HexbinAbort of LayoutError
    let private unitCorners=Array.init 6 (fun i ->
        let angle= -System.Math.PI/2.+float i*System.Math.PI/3.
        Point.create (cos angle) (sin angle))
    let private validateCorners (center:Point) radius =
        Check.finite "radius" radius
        if radius<=0. then invalidArg "radius" "Hexagon radius must be positive."
        let c0 = Point.create (center.X+radius*unitCorners[0].X) (center.Y+radius*unitCorners[0].Y)
        let c1 = Point.create (center.X+radius*unitCorners[1].X) (center.Y+radius*unitCorners[1].Y)
        let c2 = Point.create (center.X+radius*unitCorners[2].X) (center.Y+radius*unitCorners[2].Y)
        let c3 = Point.create (center.X+radius*unitCorners[3].X) (center.Y+radius*unitCorners[3].Y)
        let c4 = Point.create (center.X+radius*unitCorners[4].X) (center.Y+radius*unitCorners[4].Y)
        let c5 = Point.create (center.X+radius*unitCorners[5].X) (center.Y+radius*unitCorners[5].Y)
        if c0=c1 || c0=c2 || c0=c3 || c0=c4 || c0=c5 ||
           c1=c2 || c1=c3 || c1=c4 || c1=c5 ||
           c2=c3 || c2=c4 || c2=c5 ||
           c3=c4 || c3=c5 || c4=c5 then
            invalidArg "radius" "Hexagon corners collapse at this coordinate precision."
        struct(c0, c1, c2, c3, c4, c5)
    let private vertices center radius =
        let struct(c0, c1, c2, c3, c4, c5) = validateCorners center radius
        [| c0; c1; c2; c3; c4; c5 |]

#if !FABLE_COMPILER
    [<Struct>]
    type private AxialKeyComparer =
        interface System.Collections.Generic.IEqualityComparer<struct(int64*int64)> with
            member _.Equals(struct(aq, ar), struct(bq, br)) = aq = bq && ar = br
            member _.GetHashCode(struct(q, r)) =
                let h = uint64 q * 0x9E3779B97F4A7C15UL ^^^ (uint64 r * 0xBF58476D1CE4E5B9UL)
                int (h ^^^ (h >>> 32))
        interface System.Collections.Generic.IComparer<struct(int64*int64)> with
            member _.Compare(struct(aq, ar), struct(bq, br)) =
                let c = aq.CompareTo bq
                if c <> 0 then c else ar.CompareTo br
#endif

    /// <summary>Assigns each point to its nearest hexagon center and returns occupied bins sorted by key. Returns Error for invalid settings, the
    /// first invalid point (empty or duplicate ID, invalid weight, outside the bounds) or an exceeded budget; cancellation raises
    /// OperationCanceledException.</summary>
    let aggregateWithCancellation (token:System.Threading.CancellationToken) (config:HexbinConfig) (points:WeightedPoint list) : Result<HexBin list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not(Double.IsFinite config.Radius) || config.Radius<=0. || Bounds.width config.Bounds<=0. || Bounds.height config.Bounds<=0.
           || config.MaxPoints<=0 || config.MaxBins<=0 then
            Error [InvalidObservation "Hexbin requires positive finite radius, bounds and budgets."]
        else
          try
            let originX = Bounds.x config.Bounds
            let originY = Bounds.y config.Bounds
            let maxX = originX + Bounds.width config.Bounds
            let maxY = originY + Bounds.height config.Bounds
            let radius = config.Radius
            let invRadius = 1. / radius
            let sqrt3 = sqrt 3.
            let invSqrt3 = 1. / sqrt3
            let seen=System.Collections.Generic.HashSet<string>(StringComparer.Ordinal)
#if !FABLE_COMPILER
            let groups=System.Collections.Generic.Dictionary<struct(int64*int64),ResizeArray<WeightedPoint>>(AxialKeyComparer())
#else
            let groups=System.Collections.Generic.Dictionary<struct(int64*int64),ResizeArray<WeightedPoint>>()
#endif
            let mutable index=0
            for point in points do
                if (index &&& 63)=0 then token.ThrowIfCancellationRequested()
                index<-index+1
                if index>config.MaxPoints then raise(HexbinAbort(BudgetExceeded "Hexbin input exceeds its point budget."))
                if System.String.IsNullOrWhiteSpace point.Id then raise(HexbinAbort(InvalidObservation "Point IDs must not be empty."))
                if not(seen.Add point.Id) then raise(HexbinAbort(DuplicateId point.Id))
                if not(Double.IsFinite point.Weight) || point.Weight<0. then raise(HexbinAbort(InvalidObservation "Hexbin weights must be finite and nonnegative."))
                let px = point.Position.X
                let py = point.Position.Y
                if not(px>=originX && px<=maxX && py>=originY && py<=maxY) then
                    raise(HexbinAbort(InvalidObservation("Point lies outside hexbin bounds: "+point.Id)))
                let x=(px-originX)*invRadius
                let y=(py-originY)*invRadius
                let q=x*invSqrt3-y*(1./3.)
                let r=y*(2./3.)
                let third= -q-r
                if not(Double.IsFinite q && Double.IsFinite r && Double.IsFinite third)
                   || max (abs q) (max (abs r) (abs third))>1125899906842624. then
                    raise(HexbinAbort(BudgetExceeded "Hexbin lattice coordinates exceed the exact rounding range."))
                let mutable rq,rr=System.Math.Round q,System.Math.Round r
                let rt=System.Math.Round third
                let dq,dr,dt=abs(rq-q),abs(rr-r),abs(rt-third)
                if dq>=dr && dq>=dt then rq<- -rt-rr
                elif dr>=dt then rr<- -rq-rt
                let key=struct(int64 rq,int64 rr)
                match groups.TryGetValue key with
                | true,members -> members.Add point
                | _ ->
                    if groups.Count>=config.MaxBins then raise(HexbinAbort(BudgetExceeded "Hexbin output exceeds its bin budget."))
                    let members=ResizeArray<WeightedPoint>(4)
                    members.Add point
                    groups.Add(key,members)
            let binCount = groups.Count
            if binCount = 0 then Ok []
            else
                let keys = Array.zeroCreate<struct(int64*int64)> binCount
                let mutable kIdx = 0
                for kv in groups do
                    keys[kIdx] <- kv.Key
                    kIdx <- kIdx + 1
#if !FABLE_COMPILER
                Array.Sort(keys, AxialKeyComparer())
                let pointIdComparer =
                    { new System.Collections.Generic.IComparer<WeightedPoint> with
                        member _.Compare(a, b) = StringComparer.Ordinal.Compare(a.Id, b.Id) }
#else
                Array.sortInPlace keys
#endif
                let bins = Array.zeroCreate<HexBin> binCount
                for b = 0 to binCount - 1 do
                    if (b &&& 63) = 0 then token.ThrowIfCancellationRequested()
                    let key = keys[b]
                    let struct(q,r) = key
                    let center=Point.create (originX+(sqrt3*(float q+float r*0.5))*radius)
                                            (originY+(1.5*float r)*radius)
                    validateCorners center radius |> ignore
                    let memberList = groups[key]
#if !FABLE_COMPILER
                    let span = System.Runtime.InteropServices.CollectionsMarshal.AsSpan(memberList)
                    MemoryExtensions.Sort(span, pointIdComparer)
                    let mutable weight = 0.
                    let mutable memberIds = []
                    for i = span.Length - 1 downto 0 do
                        if (i &&& 63) = 0 then token.ThrowIfCancellationRequested()
                        let item = span[i]
                        weight <- weight + item.Weight
                        memberIds <- item.Id :: memberIds
                    // Forward sum order matches ascending ID order!
                    if span.Length > 2 then
                        weight <- 0.
                        for i = 0 to span.Length - 1 do
                            weight <- weight + span[i].Weight
#else
                    let members = memberList.ToArray()
                    Array.sortInPlaceWith(fun (a:WeightedPoint) b -> StringComparer.Ordinal.Compare(a.Id,b.Id)) members
                    let mutable weight = 0.
                    for i = 0 to members.Length - 1 do
                        if (i &&& 63) = 0 then token.ThrowIfCancellationRequested()
                        weight <- weight + members[i].Weight
                    let mutable memberIds = []
                    for i = members.Length - 1 downto 0 do
                        memberIds <- members[i].Id :: memberIds
#endif
                    if not(Double.IsFinite weight) then raise(HexbinAbort(InvalidObservation "Hexbin weight exceeds the finite numeric range."))
                    bins[b] <- {Key=key;Center=center;Radius=radius;Weight=weight;Members=memberIds}
                let mutable result = []
                for b = binCount - 1 downto 0 do
                    result <- bins[b] :: result
                Ok result
          with
          | HexbinAbort error -> Error [error]
          | :? System.ArgumentException -> Error [InvalidObservation "Hexbin geometry exceeds the finite coordinate range."]
    /// <summary>Runs <c>aggregateWithCancellation</c> without cancellation.</summary>
    let aggregate config points = aggregateWithCancellation System.Threading.CancellationToken.None config points
    /// <summary>Returns the six corners starting at the top and proceeding clockwise in y-down coordinates. Raises ArgumentException for a
    /// nonpositive radius or corners that collapse at coordinate precision.</summary>
    let polygon (bin:HexBin) =
        let struct(c0, c1, c2, c3, c4, c5) = validateCorners bin.Center bin.Radius
        [ c0; c1; c2; c3; c4; c5 ]
    /// <summary>Returns the hexagon as a closed path; raises as <c>polygon</c> does.</summary>
    let path (bin:HexBin) =
        let struct(c0, c1, c2, c3, c4, c5) = validateCorners bin.Center bin.Radius
        Path.ofCommands [ MoveTo c0; LineTo c1; LineTo c2; LineTo c3; LineTo c4; LineTo c5; Close ]

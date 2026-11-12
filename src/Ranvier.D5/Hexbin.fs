namespace Ranvier.D5

type HexbinConfig = { Bounds:Bounds; Radius:float; MaxPoints:int; MaxBins:int }
type HexBin = { Key:struct(int64*int64); Center:Point; Radius:float; Weight:float; Members:string list }

[<RequireQualifiedAccess>]
module HexbinConfig =
    let create bounds radius = {Bounds=bounds;Radius=radius;MaxPoints=1000000;MaxBins=1000000}

[<RequireQualifiedAccess>]
module Hexbin =
    exception private HexbinAbort of LayoutError
    let private vertices center radius =
        Check.finite "radius" radius
        if radius<=0. then invalidArg "radius" "Hexagon radius must be positive."
        let points=Array.init 6 (fun i ->
            let angle= -System.Math.PI/2.+float i*System.Math.PI/3.
            Point.create (center.X+radius*cos angle) (center.Y+radius*sin angle))
        for i in 0..5 do
            for j in 0..i-1 do
                if points[i]=points[j] then invalidArg "radius" "Hexagon corners collapse at this coordinate precision."
        points

    let aggregateWithCancellation (token:System.Threading.CancellationToken) (config:HexbinConfig) (points:WeightedPoint list) : Result<HexBin list,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not(System.Double.IsFinite config.Radius) || config.Radius<=0. || Bounds.width config.Bounds<=0. || Bounds.height config.Bounds<=0.
           || config.MaxPoints<=0 || config.MaxBins<=0 then
            Error [InvalidObservation "Hexbin requires positive finite radius, bounds and budgets."]
        elif points.Length>config.MaxPoints then Error [BudgetExceeded "Hexbin input exceeds its point budget."]
        else
          try
            let seen=System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
            let groups=System.Collections.Generic.Dictionary<struct(int64*int64),ResizeArray<WeightedPoint>>()
            let mutable index=0
            for point in points do
                if index%64=0 then token.ThrowIfCancellationRequested()
                index<-index+1
                if System.String.IsNullOrWhiteSpace point.Id then raise(HexbinAbort(InvalidObservation "Point IDs must not be empty."))
                if not(seen.Add point.Id) then raise(HexbinAbort(DuplicateId point.Id))
                if not(System.Double.IsFinite point.Weight) || point.Weight<0. then raise(HexbinAbort(InvalidObservation "Hexbin weights must be finite and nonnegative."))
                if not(Bounds.contains point.Position config.Bounds) then raise(HexbinAbort(InvalidObservation("Point lies outside hexbin bounds: "+point.Id)))
                let x=(point.Position.X-Bounds.x config.Bounds)/config.Radius
                let y=(point.Position.Y-Bounds.y config.Bounds)/config.Radius
                let q,r=x/sqrt 3.-y/3.,y*(2./3.)
                let third= -q-r
                if not(System.Double.IsFinite q && System.Double.IsFinite r && System.Double.IsFinite third)
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
                    let members=ResizeArray<WeightedPoint>()
                    members.Add point
                    groups.Add(key,members)
            let output=ResizeArray<HexBin>()
            for key in groups.Keys |> Seq.sort do
                token.ThrowIfCancellationRequested()
                let struct(q,r)=key
                let center=Point.create (Bounds.x config.Bounds+(sqrt 3.*(float q+float r/2.))*config.Radius)
                                        (Bounds.y config.Bounds+(1.5*float r)*config.Radius)
                vertices center config.Radius |> ignore
                let members=groups[key].ToArray()
                Array.sortInPlaceWith(fun (a:WeightedPoint) b -> System.StringComparer.Ordinal.Compare(a.Id,b.Id)) members
                let mutable weight=0.
                for i in 0..members.Length-1 do
                    if i%64=0 then token.ThrowIfCancellationRequested()
                    weight<-weight+members[i].Weight
                if not(System.Double.IsFinite weight) then raise(HexbinAbort(InvalidObservation "Hexbin weight exceeds the finite numeric range."))
                output.Add {Key=key;Center=center;Radius=config.Radius;Weight=weight;Members=members |> Array.map(fun p -> p.Id) |> Array.toList}
            Ok(List.ofSeq output)
          with
          | HexbinAbort error -> Error [error]
          | :? System.ArgumentException -> Error [InvalidObservation "Hexbin geometry exceeds the finite coordinate range."]
    let aggregate config points = aggregateWithCancellation System.Threading.CancellationToken.None config points
    let path (bin:HexBin) =
        let points=vertices bin.Center bin.Radius |> Array.toList
        Path.ofCommands(MoveTo points.Head::(points.Tail |> List.map LineTo) @ [Close])

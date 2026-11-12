namespace Ranvier.D5

type ManyBodyConfig = { Strength:float; Theta:float; MinDistance:float; MaxDistance:float }
[<RequireQualifiedAccess>]
module ManyBodyConfig =
    let create strength = {Strength=strength;Theta=0.8;MinDistance=1.;MaxDistance=infinity}

type CollisionConfig = { Strength:float; Iterations:int }
[<RequireQualifiedAccess>]
module CollisionConfig =
    let create strength = {Strength=strength;Iterations=1}

module internal ForceKernels =
    let private check (token:System.Threading.CancellationToken) i = if i%64=0 then token.ThrowIfCancellationRequested()
    let private near (p:Point) (extent:QuadExtent) =
        System.Double.Hypot(p.X-max extent.Min.X (min extent.Max.X p.X),p.Y-max extent.Min.Y (min extent.Max.Y p.Y))
    let private far (p:Point) (extent:QuadExtent) =
        System.Double.Hypot(max (abs(p.X-extent.Min.X)) (abs(p.X-extent.Max.X)),max (abs(p.Y-extent.Min.Y)) (abs(p.Y-extent.Max.Y)))

    let manyBody token (config:ManyBodyConfig) alpha (positions:Point[]) (masses:float[]) jitter (vx:float[]) (vy:float[]) =
        let add i source mass sourceIndex =
            let dx,dy=source.X-positions[i].X,source.Y-positions[i].Y
            let actual=System.Double.Hypot(dx,dy)
            Check.finite "many-body distance" actual
            if actual<=config.MaxDistance then
                let ux,uy,distance=if actual=0. then let x,y=jitter i sourceIndex in x,y,config.MinDistance else dx/actual,dy/actual,actual
                let softened=max distance config.MinDistance
                let magnitude=(config.Strength*alpha/softened)*mass*(distance/softened)
                vx[i]<-vx[i]+ux*magnitude;vy[i]<-vy[i]+uy*magnitude
        let bounded=System.Double.IsFinite config.MaxDistance
        if config.Strength<>0. then
            if config.Theta=0. then
                for i in 0..positions.Length-1 do
                    check token i
                    for j in 0..positions.Length-1 do
                        check token j
                        if i<>j then add i positions[j] masses[j] j
            else
                let tree=Quadtree.buildWithCancellation token (fun i -> positions[i]) (fun i -> masses[i]) (seq{0..positions.Length-1})
                for i in 0..positions.Length-1 do
                    check token i
                    let point=positions[i]
                    Quadtree.visit (fun cell ->
                        token.ThrowIfCancellationRequested()
                        if bounded && near point cell.Extent>config.MaxDistance then false else
                        let e=cell.Extent
                        let contains=point.X>=e.Min.X && point.X<=e.Max.X && point.Y>=e.Min.Y && point.Y<=e.Max.Y
                        let distance=System.Double.Hypot(cell.Center.X-point.X,cell.Center.Y-point.Y)
                        let size=max (e.Max.X-e.Min.X) (e.Max.Y-e.Min.Y)
                        if not contains && System.Double.IsFinite distance && size<config.Theta*distance && (not bounded || far point e<=config.MaxDistance) then
                            add i cell.Center cell.Mass i
                            false
                        elif not cell.Items.IsEmpty then
                            cell.Items |> List.iteri(fun index item ->
                                check token index
                                if item.Value<>i then add i item.Position item.Mass item.Value)
                            false
                        else true) tree

    let collision token (config:CollisionConfig) (positions:Point[]) (masses:float[]) (radii:float[]) (pinned:bool[]) jitter (vx:float[]) (vy:float[]) =
        if config.Strength>0. && positions.Length>0 then
            let maxRadius=Array.max radii
            let predicted=Array.zeroCreate<Point> positions.Length
            for _ in 1..config.Iterations do
                for i in 0..positions.Length-1 do
                    check token i
                    predicted[i]<-if pinned[i] then positions[i] else Point.create (positions[i].X+vx[i]) (positions[i].Y+vy[i])
                let tree=Quadtree.buildWithCancellation token (fun i -> predicted[i]) (fun _ -> 1.) (seq{0..positions.Length-1})
                for i in 0..positions.Length-1 do
                    check token i
                    Quadtree.visit(fun cell ->
                        token.ThrowIfCancellationRequested()
                        if near predicted[i] cell.Extent>radii[i]+maxRadius then false else
                        cell.Items |> List.iteri(fun index item ->
                            check token index
                            let j=item.Value
                            if j>i && not(pinned[i] && pinned[j]) then
                                let dx,dy=predicted[j].X-predicted[i].X,predicted[j].Y-predicted[i].Y
                                let distance=System.Double.Hypot(dx,dy)
                                Check.finite "collision distance" distance
                                let overlap=radii[i]+radii[j]-distance
                                if overlap>0. then
                                    let ux,uy=if distance=0. then jitter i j else dx/distance,dy/distance
                                    let scale=max masses[i] masses[j]
                                    let a,b=masses[i]/scale,masses[j]/scale
                                    let shareA,shareB=if pinned[i] then 0.,1. elif pinned[j] then 1.,0. else b/(a+b),a/(a+b)
                                    let correction=overlap*config.Strength
                                    vx[i]<-vx[i]-ux*correction*shareA;vy[i]<-vy[i]-uy*correction*shareA
                                    vx[j]<-vx[j]+ux*correction*shareB;vy[j]<-vy[j]+uy*correction*shareB)
                        true) tree

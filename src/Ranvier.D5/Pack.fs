namespace Ranvier.D5
open System

type HierarchyCircle<'Id> = { Id:'Id; Parent:'Id option; Depth:int; Center:Point; Radius:float }
type PackConfig = { Bounds:Bounds; Padding:float; SumPolicy:HierarchySumPolicy }
[<RequireQualifiedAccess>]
module PackConfig =
    let create bounds = {Bounds=bounds;Padding=0.;SumPolicy=HierarchySumPolicy.LeavesOnly}

[<RequireQualifiedAccess>]
module Pack =
    // Front-chain placement adapted from d3-hierarchy 3.1.2 (ISC).
    let private siblings (r:float[]) (x:float[]) (y:float[]) =
        let n=r.Length
        let place b a c =
            let dx,dy=x[b]-x[a],y[b]-y[a]
            let d2=dx*dx+dy*dy
            if d2=0. then x[c]<-x[a]+r[c];y[c]<-y[a] else
            let a2,b2=(r[a]+r[c])**2.,(r[b]+r[c])**2.
            if a2>b2 then
                let along=(d2+b2-a2)/(2.*d2)
                let perpendicular=sqrt(max 0. (b2/d2-along*along))
                x[c]<-x[b]-along*dx-perpendicular*dy
                y[c]<-y[b]-along*dy+perpendicular*dx
            else
                let along=(d2+a2-b2)/(2.*d2)
                let perpendicular=sqrt(max 0. (a2/d2-along*along))
                x[c]<-x[a]+along*dx-perpendicular*dy
                y[c]<-y[a]+along*dy+perpendicular*dx
        let intersects a b =
            let radius=(r[a]+r[b])*(1.-1e-12)
            let dx,dy=x[a]-x[b],y[a]-y[b]
            radius>0. && radius*radius>dx*dx+dy*dy
        if n=0 then 0.
        elif n=1 then r[0]
        else
            x[0]<- -r[1];x[1]<-r[0]
            if n>2 then
                place 1 0 2
                let next=Array.zeroCreate n
                let previous=Array.zeroCreate n
                next[0]<-1;previous[1]<-0;next[1]<-2;previous[2]<-1;next[2]<-0;previous[0]<-2
                let score a =
                    let b=next[a]
                    let radius=r[a]+r[b]
                    let dx=(x[a]*r[b]+x[b]*r[a])/radius
                    let dy=(y[a]*r[b]+y[b]*r[a])/radius
                    dx*dx+dy*dy
                let mutable a,b,i=0,1,3
                while i<n do
                    place a b i
                    let mutable j,k=next[b],previous[a]
                    let mutable sj,sk=r[b],r[a]
                    let mutable searching,retry=true,false
                    while searching do
                        if sj<=sk then
                            if intersects j i then b<-j;next[a]<-b;previous[b]<-a;retry<-true else sj<-sj+r[j];j<-next[j]
                        else
                            if intersects k i then a<-k;next[a]<-b;previous[b]<-a;retry<-true else sk<-sk+r[k];k<-previous[k]
                        searching<-not retry && j<>next[k]
                    if not retry then
                        previous[i]<-a;next[i]<-b;next[a]<-i;previous[b]<-i;b<-i
                        let mutable best=score a
                        let mutable c=next[i]
                        while c<>b do
                            let candidate=score c
                            if candidate<best then a<-c;best<-candidate
                            c<-next[c]
                        b<-next[a];i<-i+1
            // A conservative linear enclosure avoids recursive minimum-circle solvers.
            let mutable left,right,top,bottom=Double.PositiveInfinity,Double.NegativeInfinity,Double.PositiveInfinity,Double.NegativeInfinity
            for i in 0..n-1 do
                left<-min left (x[i]-r[i]);right<-max right (x[i]+r[i])
                top<-min top (y[i]-r[i]);bottom<-max bottom (y[i]+r[i])
            let cx,cy=(left+right)/2.,(top+bottom)/2.
            let mutable radius=0.
            for i in 0..n-1 do
                x[i]<-x[i]-cx;y[i]<-y[i]-cy
                radius<-max radius (sqrt(x[i]*x[i]+y[i]*y[i])+r[i])
            radius

    let layout config root =
        HierarchyIndex.protect(fun () ->
            Check.finite "padding" config.Padding
            if config.Padding<0. then invalidArg "padding" "Padding must be nonnegative."
            let index=HierarchyIndex.create root
            let values=HierarchyWeights.values config.SumPolicy index
            let maximum=Array.max values
            let radii=Array.zeroCreate values.Length
            let x,y=Array.zeroCreate values.Length,Array.zeroCreate values.Length
            let own=values |> Array.mapi(fun i total -> max 0. (total-(index.Children[i] |> Seq.map(fun child -> values[child]) |> Data.sum)))
            let leafRadius value=if maximum=0. then 1. else sqrt(value/maximum)
            let pack padding =
                for i in index.Postorder do
                    let children=index.Children[i]
                    if children.Length=0 then radii[i]<-leafRadius values[i] else
                    let positive=children |> Array.filter(fun child -> radii[child]>0.)
                    let ownRadius=if config.SumPolicy=HierarchySumPolicy.IncludeOwnWeight && own[i]>0. then leafRadius own[i] else 0.
                    let sizes=positive |> Array.map(fun child -> radii[child]+padding/2.)
                    let sizes=if ownRadius>0. then Array.append sizes [|ownRadius+padding/2.|] else sizes
                    let cx,cy=Array.zeroCreate sizes.Length,Array.zeroCreate sizes.Length
                    radii[i]<-siblings sizes cx cy+padding/2.
                    for j in 0..positive.Length-1 do x[positive[j]]<-cx[j];y[positive[j]]<-cy[j]
            pack 0.
            let diameter=min (Bounds.width config.Bounds) (Bounds.height config.Bounds)
            if config.Padding>0. && diameter>0. then pack (min 1. (config.Padding/diameter)*2.*radii[0])
            let scale=if radii[0]=0. then 0. else diameter/2./radii[0]
            x[0]<-Bounds.x config.Bounds+Bounds.width config.Bounds/2.
            y[0]<-Bounds.y config.Bounds+Bounds.height config.Bounds/2.
            for i in 1..values.Length-1 do
                x[i]<-x[index.Parents[i]]+x[i]*scale
                y[i]<-y[index.Parents[i]]+y[i]*scale
            index.Items |> Array.mapi(fun i item -> {Id=item.Id;Parent=item.Parent;Depth=item.Depth;Center=Point.create x[i] y[i];Radius=radii[i]*scale}) |> Array.toList)

namespace Ranvier.D5

open System

/// <summary>Iterative Buchheim tidy-tree layout, adapted from d3-hierarchy 3.1.2; see packaged license.</summary>
[<RequireQualifiedAccess>]
module Tree =
    /// <summary>Positions every node in preorder within <c>config.Bounds</c>, with depth increasing downward from the root at the top edge.
    /// Returns <c>Error [InvalidObservation]</c> for duplicate identities or a non-positive separation.</summary>
    let layout config root =
        HierarchyIndex.protect(fun () ->
            let index=HierarchyIndex.create root
            let n=index.Items.Length
            let children=Array.append index.Children [|[|0|]|]
            let parent=Array.append index.Parents [|-1|]
            parent[0]<-n
            let number=Array.zeroCreate(n+1)
            for xs in children do for i in 0..xs.Length-1 do number[xs[i]]<-i
            let z,m,c,s=Array.zeroCreate(n+1),Array.zeroCreate(n+1),Array.zeroCreate(n+1),Array.zeroCreate(n+1)
            let thread=Array.create(n+1) -1
            let ancestor=Array.init(n+1) id
            let defaults=Array.create(n+1) -1
            let sep=TreeCoordinates.separation config index
            let left i=if children[i].Length>0 then children[i][0] else thread[i]
            let right i=if children[i].Length>0 then Array.last children[i] else thread[i]
            let move a b shift =
                let change=shift/float(number[b]-number[a])
                c[b]<-c[b]-change
                s[b]<-s[b]+shift
                c[a]<-c[a]+change
                z[b]<-z[b]+shift
                m[b]<-m[b]+shift
            let apportion v w initial =
                let mutable result=initial
                if w>=0 then
                    let mutable vip,vop,vim,vom=v,v,w,children[parent[v]][0]
                    let mutable sip,sop,sim,som=m[vip],m[vop],m[vim],m[vom]
                    let mutable running=true
                    while running do
                        vim<-right vim
                        vip<-left vip
                        if vim<0 || vip<0 then running<-false else
                        vom<-left vom
                        vop<-right vop
                        ancestor[vop]<-v
                        let shift=z[vim]+sim-z[vip]-sip+sep vim vip
                        if shift>0. then
                            let a=if parent[ancestor[vim]]=parent[v] then ancestor[vim] else result
                            move a v shift
                            sip<-sip+shift
                            sop<-sop+shift
                        sim<-sim+m[vim]
                        sip<-sip+m[vip]
                        som<-som+m[vom]
                        sop<-sop+m[vop]
                    if vim>=0 && right vop<0 then
                        thread[vop]<-vim
                        m[vop]<-m[vop]+sim-sop
                    if vip>=0 && left vom<0 then
                        thread[vom]<-vip
                        m[vom]<-m[vom]+sip-som
                        result<-v
                result
            for v in index.Postorder do
                let siblings=children[parent[v]]
                let w=if number[v]>0 then siblings[number[v]-1] else -1
                if children[v].Length>0 then
                    let mutable shift,change=0.,0.
                    for i in children[v].Length-1 .. -1 .. 0 do
                        let child=children[v][i]
                        z[child]<-z[child]+shift
                        m[child]<-m[child]+shift
                        change<-change+c[child]
                        shift<-shift+s[child]+change
                    let midpoint=(z[children[v][0]]+z[Array.last children[v]])/2.
                    if w>=0 then z[v]<-z[w]+sep v w;m[v]<-z[v]-midpoint else z[v]<-midpoint
                elif w>=0 then z[v]<-z[w]+sep v w
                defaults[parent[v]]<-apportion v w (if defaults[parent[v]]<0 then siblings[0] else defaults[parent[v]])
            m[n]<- -z[0]
            let xs=Array.zeroCreate n
            for v in 0..n-1 do xs[v]<-z[v]+m[parent[v]];m[v]<-m[v]+m[parent[v]]
            let low=Array.minBy(fun i -> xs[i]) [|0..n-1|]
            let high=Array.maxBy(fun i -> xs[i]) [|0..n-1|]
            let padding=if low=high then 1. else sep low high/2.
            let first,last=xs[low]-padding,xs[high]+padding
            let depth=index.Items |> Array.maxBy(fun item -> item.Depth) |> fun item -> max 1 item.Depth
            TreeCoordinates.points config.Bounds index (xs |> Array.map(fun x -> (x-first)/(last-first))) (index.Items |> Array.map(fun item -> float item.Depth/float depth)))

/// <summary>Dendrogram layout that aligns all leaves on the bottom edge of the bounds.</summary>
[<RequireQualifiedAccess>]
module Cluster =
    /// <summary>Positions every node in preorder: leaves are spaced by separation along the bottom edge and parents sit at their children's mean x,
    /// one level per subtree height. Returns <c>Error [InvalidObservation]</c> for duplicate identities or a non-positive separation.</summary>
    let layout config root =
        HierarchyIndex.protect(fun () ->
            let index=HierarchyIndex.create root
            let xs,heights=Array.zeroCreate index.Items.Length,Array.zeroCreate index.Items.Length
            let mutable previous= -1
            for i in index.Postorder do
                let children=index.Children[i]
                if children.Length=0 then
                    xs[i]<-if previous<0 then 0. else xs[previous]+TreeCoordinates.separation config index i previous
                    previous<-i
                else
                    xs[i]<-children |> Seq.map(fun child -> xs[child]) |> Data.mean |> Option.get
                    heights[i]<-1.+(children |> Array.map(fun child -> heights[child]) |> Array.max)
            let leaves=index.Postorder |> Array.filter(fun i -> index.Children[i].Length=0)
            let left,right=leaves[0],Array.last leaves
            let first=xs[left]-TreeCoordinates.separation config index left right/2.
            let last=xs[right]+TreeCoordinates.separation config index right left/2.
            TreeCoordinates.points config.Bounds index (xs |> Array.map(fun x -> (x-first)/(last-first)))
                (heights |> Array.map(fun h -> if heights[0]=0. then 0. else 1.-h/heights[0])))

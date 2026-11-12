namespace Ranvier.D5

/// <summary>Partition (icicle) settings: target bounds in scene coordinates and a finite nonnegative padding in scene units.</summary>
type PartitionConfig = {Bounds:Bounds;Padding:float}

/// <summary>Adjacency (icicle) layout of a hierarchy; invalid configuration or identities yield <c>Error [InvalidObservation]</c>.</summary>
[<RequireQualifiedAccess>]
module Partition =
    /// <summary>Partitions each depth into a horizontal band. Own branch weight reserves unallocated space; all-zero siblings share equally.</summary>
    let layout config root =
        HierarchyIndex.protect(fun () ->
            Check.finite "padding" config.Padding
            if config.Padding<0. then invalidArg "padding" "Padding must be nonnegative."
            let index=HierarchyIndex.create root
            let depth=index.Items |> Array.maxBy(fun item -> item.Depth) |> fun item -> item.Depth+1
            let row=Bounds.height config.Bounds/float depth
            let spans=Array.create index.Items.Length (0.,0.)
            spans[0]<-Bounds.x config.Bounds,Bounds.x config.Bounds+Bounds.width config.Bounds
            index.Items |> Array.mapi(fun i item ->
                let first,last=spans[i]
                let children=index.Children[i]
                let total=item.Value
                let mutable accumulated=0.
                for child in children do
                    let fraction=if total=0. then 1./float children.Length else index.Items[child].Value/total
                    let next=min 1. (accumulated+fraction)
                    spans[child]<-Check.lerp first last accumulated,Check.lerp first last next
                    accumulated<-next
                let paddingX=min (config.Padding/2.) ((last-first)/2.)
                let paddingY=min (config.Padding/2.) (row/2.)
                {Id=item.Id;Parent=item.Parent;Depth=item.Depth
                 Bounds=Bounds.create (first+paddingX) (Bounds.y config.Bounds+float item.Depth*row+paddingY) (max 0. (last-first-2.*paddingX)) (max 0. (row-2.*paddingY))}) |> Array.toList)

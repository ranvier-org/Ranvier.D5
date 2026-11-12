module Ranvier.D5.Gallery.HierarchyChartsGallery
open Ranvier.D5
open Ranvier.D5.Charts

let root=Hierarchy.node "Toolkit" 0. [
    Hierarchy.node "Signals" 0. [Hierarchy.node "State" 2. [];Hierarchy.node "Effects" 1. []]
    Hierarchy.node "View" 0. [Hierarchy.node "Scene" 3. [];Hierarchy.node "Input" 1. []]
    Hierarchy.node "Compute" 0. [Hierarchy.node "Layout" 3. [];Hierarchy.node "Workers" 2. []]]
let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let scene() =
    let kinds=["Dendrogram",HierarchyChart.dendrogram;"Radial tree",HierarchyChart.radialTree;"Icicle",HierarchyChart.icicle;"Sunburst",HierarchyChart.sunburst;"Packed bubbles",HierarchyChart.packedBubbles;"Treemap",HierarchyChart.treemap]
    let frame={ChartFrame.create 800. 570. "Hierarchy chart family" with Margin={Left=10.;Right=10.;Top=0.;Bottom=0.}}
    Facet.build {Frame=frame;Columns=3;Gap=10.;Domains=FacetDomains.Independent;Domain=ignore;Build=fun frame () charts ->
        let output=(List.head charts) (HierarchySpec.create {frame with Margin={Left=8.;Right=8.;Top=32.;Bottom=10.}}) root |> ok
        let title=Mark.text "panel-title" (Point.create 8. 20.) frame.Title |> Mark.withFontSize 16.
        Ok{output with Scene=Scene.compose frame.Viewport frame.Title (Scene.definitions output.Scene) (Leaf title::Scene.nodes output.Scene)}}
        (kinds |> List.map(fun (title,chart) -> title,[chart])) |> ok |> fun output -> output.Scene

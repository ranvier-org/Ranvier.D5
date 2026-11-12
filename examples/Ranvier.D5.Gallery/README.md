# Native examples

Every image below comes from a working native WebCore view.

## Planar geometry

![Native hull, centroid and containment](../../docs/images/planar.png)

Run `--planar`; capture with `--capture-planar artifacts/planar.bmp`.
[Source](PlanarGallery.fs) · [Numeric and boundary contracts](../../docs/planar.md).

## Hierarchy chart family and explorer

![Native hierarchy charts](../../docs/images/hierarchy-charts.png)

![Native reactive hierarchy explorer](../../docs/images/hierarchy-explorer.png)

Run `--hierarchy-charts` or `--hierarchy-explorer`. The explorer supports selection,
collapse, drill-down and transitions using Ranvier signals and the owned worker.
[Source](HierarchyExplorer.fs) · [Interaction and capture instructions](../../docs/hierarchy-charts.md).

## Hierarchy layouts

![Native hierarchy layout gallery](../../docs/images/hierarchy.png)

Run with `--hierarchy`; capture with `--capture-hierarchy artifacts/hierarchy.bmp`.
The example composes pure tidy-tree, cluster and partition outputs into retained
SVG marks. [Source](HierarchyGallery.fs) · [Contracts](../../docs/hierarchy.md).

## Treemaps and packing

![Native tiling and circle packing](../../docs/images/tiling.png)

Run with `--tiling`; capture with `--capture-tiling artifacts/tiling.bmp`.
[Source](TilingGallery.fs) · [Contracts and limitations](../../docs/tiling.md).

## Linked selection

![Native linked scatter and line views](../../docs/images/linked.png)

Run with `--linked`; capture with `--capture-linked artifacts/linked.bmp`.
Brush the upper scatter plot and the lower line updates through a shared Ranvier
signal. Arrow keys and Enter select, Escape clears.
[Source](../Shared/LinkedCharts.fs).

## Immutable force state

![Native initial and evolved force snapshots](../../docs/images/force.png)

Run `--force` to advance immutable solver state through a Ranvier signal.
Capture with `--capture-force artifacts/force.bmp`.
[Source](ForceGallery.fs) and [solver contracts](../../docs/force.md).

## Delaunay and Voronoi picking

![Native mesh and Voronoi picking](../../docs/images/spatial.png)

Run `--spatial`; move or click in either plot to select the nearest original site.
Focus the chart for arrow-key selection and Escape to clear.
Capture with `--capture-spatial artifacts/spatial.bmp`; capture checks a real native click.
[Source](SpatialGallery.fs) and [contracts](../../docs/spatial.md).

## Quadtree search

![Native quadtree search](../../docs/images/quadtree.png)

Run `--quadtree` for a signal-driven moving query over a retained immutable index.
Capture with `--capture-quadtree artifacts/quadtree.bmp`.
[Source](QuadtreeGallery.fs) and [contracts](../../docs/quadtree.md).

## Networks and bubble clusters

![Native worker-driven network](../../docs/images/network.png)
![Native bubble cluster](../../docs/images/bubbles.png)

Run `--network` for 1,000 nodes and 2,000 edges, or `--bubbles` for variable-radius
collision layout. Drag pins/unpins a node; arrows focus and Enter selects.
Capture with `--capture-network artifacts/network.bmp` or
`--capture-bubbles artifacts/bubbles.bmp`.
[Source](NetworkGallery.fs) and [contracts and measured limits](../../docs/network.md).

## Chords and directed ribbons

![Native chord and directed ribbon view](../../docs/images/chord.png)

Run `--chord` for a signal-driven matrix; capture with
`--capture-chord artifacts/chord.bmp`.
[Source](ChordGallery.fs) and [layout contracts](../../docs/chord.md).

## Sankey flows

![Native energy-flow Sankey view](../../docs/images/sankey.png)

Run `--sankey` for a signal-driven DAG layout; capture with
`--capture-sankey artifacts/sankey.bmp`.
[Source](SankeyGallery.fs) and [flow contracts](../../docs/sankey.md).

## Interactive flow recipes

![Native Sankey recipe](../../docs/images/flow-sankey.png)
![Native staged alluvial recipe](../../docs/images/flow-alluvial.png)
![Native chord recipe](../../docs/images/flow-chord.png)

Run `--flow-sankey`, `--flow-alluvial`, or `--flow-chord`. Capture with the matching
`--capture-flow-* artifacts/flow-*.bmp` option. Flow nodes drag within their column
slots; arrows focus and Enter selects. Capture checks native drag and keyboard input.
[Source](FlowGallery.fs) and [recipe and interaction contracts](../../docs/flow-charts.md).

## Foundation scene

![Native foundation gallery](../../docs/images/gallery.png)

Run without arguments; capture with `--capture artifacts/gallery.bmp`.
The scene combines line, bars, donut and treemap primitives.
[Source](Scenes.fs).

Prefix each invocation with
`dotnet run --project examples/Ranvier.D5.Gallery -c Release --`.
Convert captures to checked-in lossless PNGs with
`python tools/document-snapshots.py` (Pillow required).

## Reactive density

![Native density view](../../docs/images/density.png)

Run --density; Left/Right adjusts bandwidth. Capture with --capture-density artifacts/density.bmp. The example aggregates 100,000 observations in the owned worker and paints 768 cells. [Source](DensityGallery.fs) and [density contracts](../../docs/density.md).

## Density contours

![Native contour view](../../docs/images/contour.png)

Run --contour; Left/Right adjusts levels. Capture with --capture-contour artifacts/contour.bmp. Density and topology run through the owned worker. [Source](ContourGallery.fs) and [topology contracts](../../docs/contours.md).

## Hexbin and line reduction

![Native reduction view](../../docs/images/reduction.png)

Run --reduction; Left/Right adjusts the visible sample budget. Capture with --capture-reduction artifacts/reduction.bmp. [Source](ReductionGallery.fs) and [membership, gap and sampling contracts](../../docs/reduction.md).

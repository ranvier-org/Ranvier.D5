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

## Foundation scene

![Native foundation gallery](../../docs/images/gallery.png)

Run without arguments; capture with `--capture artifacts/gallery.bmp`.
The scene combines line, bars, donut and treemap primitives.
[Source](Scenes.fs).

Prefix each invocation with
`dotnet run --project examples/Ranvier.D5.Gallery -c Release --`.
Convert captures to checked-in lossless PNGs with
`python tools/document-snapshots.py` (Pillow required).

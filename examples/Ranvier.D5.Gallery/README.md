# Native examples

Every image below comes from a working native WebCore view.

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

## Foundation scene

![Native foundation gallery](../../docs/images/gallery.png)

Run without arguments; capture with `--capture artifacts/gallery.bmp`.
The scene combines line, bars, donut and treemap primitives.
[Source](Scenes.fs).

Prefix each invocation with
`dotnet run --project examples/Ranvier.D5.Gallery -c Release --`.
Convert captures to checked-in lossless PNGs with
`python tools/document-snapshots.py` (Pillow required).

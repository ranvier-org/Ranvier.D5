"""Convert real native gallery captures to lossless documentation images."""
from pathlib import Path
from PIL import Image
root=Path(__file__).resolve().parents[1]
out=root/"docs"/"images"
out.mkdir(parents=True,exist_ok=True)
for name in ["gallery","linked","hierarchy","tiling","hierarchy-charts","hierarchy-explorer","planar","quadtree","spatial","force","network","bubbles","chord","sankey","flow-sankey","flow-alluvial","flow-chord","geographic-kernels","projections","projection-families","map","advanced","graph-layouts","graph-3d","spatial-indexing","seriation-sets","voronoi-treemaps","bundling-routing","cluster-manifolds-2d","hierarchies-3d","spherical-manifolds-3d","atlas-recipes"]:
    source=root/"artifacts"/(name+".bmp")
    if source.exists():
        with Image.open(source) as native:
            native.convert("RGB").save(out/(name+".png"),optimize=True)
            print(f"{name}: {native.width}x{native.height}, native capture -> PNG")
dashboards=out/"dashboards"
dashboards.mkdir(parents=True,exist_ok=True)
for source in sorted((root/"artifacts"/"dashboards").glob("*.bmp")):
    # Only the default-size capture is documented; <id>-<W>x<H> resize captures and <id>-frozen failures stay in artifacts.
    if "-" in source.stem:
        continue
    with Image.open(source) as native:
        native.convert("RGB").save(dashboards/(source.stem+".png"),optimize=True)
        print(f"dashboards/{source.stem}: {native.width}x{native.height}, native capture -> PNG")

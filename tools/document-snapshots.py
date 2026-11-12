"""Convert real native gallery captures to lossless documentation images."""
from pathlib import Path
from PIL import Image
root=Path(__file__).resolve().parents[1]
out=root/"docs"/"images"
out.mkdir(parents=True,exist_ok=True)
for name in ["gallery","linked","hierarchy","tiling","hierarchy-charts","hierarchy-explorer","planar","quadtree"]:
    source=root/"artifacts"/(name+".bmp")
    if source.exists():
        with Image.open(source) as native:
            native.convert("RGB").save(out/(name+".png"),optimize=True)
            print(f"{name}: {native.width}x{native.height}, native capture -> PNG")

"""Independent 80-digit unit-vector spherical interpolation fixtures."""
import json
from pathlib import Path
import mpmath as mp

assert mp.__version__ == "1.3.0", "Install the pinned precision-requirements.txt"
mp.mp.dps = 80
root = Path(__file__).resolve().parents[2]
source = json.loads((root / "tests/fixtures/spherical.json").read_text())

def vector(point):
    longitude, latitude = [mp.mpf(str(value)) * mp.pi / 180 for value in point]
    return [mp.cos(latitude)*mp.cos(longitude), mp.cos(latitude)*mp.sin(longitude), mp.sin(latitude)]

def sample(pair):
    start, finish = vector(pair["from"]), vector(pair["to"])
    cross = [start[1]*finish[2]-start[2]*finish[1], start[2]*finish[0]-start[0]*finish[2], start[0]*finish[1]-start[1]*finish[0]]
    angle = mp.atan2(mp.sqrt(sum(value*value for value in cross)), sum(a*b for a,b in zip(start,finish)))
    samples = []
    for value in [0, 0.25, 0.5, 0.75, 1]:
        t = mp.mpf(str(value))
        if angle == 0:
            point = start
        else:
            a, b = mp.sin((1-t)*angle)/mp.sin(angle), mp.sin(t*angle)/mp.sin(angle)
            point = [a*x+b*y for x,y in zip(start,finish)]
        assert abs(sum(x*x for x in point)-1) < mp.mpf("1e-65")
        longitude = mp.atan2(point[1],point[0])*180/mp.pi
        if longitude >= 180:
            longitude -= 360
        latitude = mp.atan2(point[2],mp.sqrt(point[0]**2+point[1]**2))*180/mp.pi
        samples.append({"t":value,"point":[float(longitude),float(latitude)]})
    return {"name":pair["name"],"from":pair["from"],"to":pair["to"],"distance":float(angle),"samples":samples}

output = {"schema":1,"seed":42,"source":{"mpmath":"1.3.0"},"precisionDigits":80,
          "coordinates":"Longitude/latitude degrees; angular distance radians; input JSON decimals interpreted exactly; unit-vector slerp at 80 decimal digits.",
          "tolerance":1e-10,"pairs":[sample(pair) for pair in source["pairs"]]}
(root / "tests/fixtures/spherical-precision.json").write_text(json.dumps(output,indent=2)+"\n")
print(f"Generated {len(output['pairs'])} independent spherical interpolation cases")

# Reference fixtures

Run `npm ci` then `npm run generate` here. Dependencies are exact-version locked; Node/D3 are development-only and never enter a D5 package. Fixture metadata records versions, seed, coordinate conventions and numeric tolerance. Each reference package's copyright/license is copied into `licenses/` during generation. Handwritten invariant tests remain authoritative for D5-specific contracts (typed errors, immutable state, invalid inputs and ownership).

Generate spherical D3 references with `node spherical.mjs`. For independent near-antipodal interpolation references, install `python -m pip install -r precision-requirements.txt` and run `python spherical-precision.py`. The latter pins mpmath 1.3.0 at 80 decimal digits; its license is included in `licenses/mpmath.txt`. D3 fixtures remain unchanged so differences caused by its near-antipodal numerical conditioning remain visible.

# Reference fixtures

Run `npm ci` then `npm run generate` here. Dependencies are exact-version locked; Node/D3 are development-only and never enter a D5 package. Fixture metadata records versions, seed, coordinate conventions and numeric tolerance. Each reference package's copyright/license is copied into `licenses/` during generation. Handwritten invariant tests remain authoritative for D5-specific contracts (typed errors, immutable state, invalid inputs and ownership).

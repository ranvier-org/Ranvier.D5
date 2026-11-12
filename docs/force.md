# Deterministic force state

![Working native initial and evolved force states](images/force.png)

`GraphData.create nodes links` validates IDs, endpoints, finite positive masses,
nonnegative radii and link parameters. Nodes and links are captured in ordinal
ID order. Parallel links have distinct IDs; self-links remain in the graph but
exert no spring force. Missing endpoints and invalid observations return typed
layout errors.

`Force.initialize config seed graph` creates private state. Explicit positions
are honored; pins override positions. Missing positions use Ranvier.Anime's
seeded generator with an explicit seed and stable per-ID hash. Changing input
order does not change initialization. No global random state is consumed.

`Force.step count state` advances unit-time ticks and returns new state, leaving
previous snapshots intact. Each tick cools alpha, adds center/axis/radial/link
velocity contributions, applies velocity decay, and advances positions. Center
force moves the collective mean toward the configured center. Link corrections
are divided inversely by endpoint mass. Pins restore their fixed position and
zero velocity after forces. Coincident radial/link directions use deterministic
seeded perturbations. Disabled forces do no arithmetic.

This is an explicit fixed-step solver, not a variable-frame-time integrator or
a guarantee of a unique optimum. Anime is reused for seeded utilities and later
presentation animation, not for physics equations. Large strengths or extreme
active-force coordinates can overflow; stepping throws instead of publishing
nonfinite state. Callers retain the unchanged input state on failure.

`snapshot` returns immutable node records, alpha and iteration. `Converged`
means alpha cooled below its threshold (or topology is empty); `Finished` also
includes the iteration budget. `reheat` raises alpha and starts a fresh budget.
`pin id position` pins or unpins an ID and reheats. `updateGraph` replaces the
topology, retains surviving positions/velocities, deterministically initializes
new IDs and removes exited IDs. Pins in the replacement graph are authoritative;
initial-position fields do not teleport surviving nodes.

`stepWithCancellation token count state` checks cancellation before stepping,
before every tick and every 64 nodes/edges. Cancellation discards private work
without modifying input. Cancellation latency is cooperative, not real-time.
Same seed/config/input reproduces results; cross-platform bitwise transcendental
math identity is not promised.

```fsharp
let graph = GraphData.create [GraphNode.create "a"; GraphNode.create "b"]
                            [GraphLink.create "ab" "a" "b"]
let run = graph |> Result.bind (Force.initialize (ForceConfig.create(Point.create 0. 0.)) (Seed.create 42u))
let snapshot = run |> Result.map (Force.step 100 >> Force.snapshot)
```

The native example publishes successive immutable states through a Ranvier
signal, rendering initial and current positions side by side:

```sh
dotnet run --project examples/Ranvier.D5.Gallery -c Release -- --force
dotnet run --project examples/Ranvier.D5.Gallery -c Release -- --capture-force artifacts/force.bmp
python tools/document-snapshots.py
```

The local 1,000-node/2,000-edge benchmark takes about 0.42 ms for ten steps,
allocating 1.46 MB. Its p95 time/allocation gates are 8 ms / 3 MB across normal
and Traced CI. This excludes rendering and the upcoming collision/many-body
kernels; it is not a complete interactive-frame claim.

This is task 7 G1. Collision, Barnes–Hut many-body forces and the bounded worker/
drag network recipes follow in G2/G3. They are not claimed by this batch.

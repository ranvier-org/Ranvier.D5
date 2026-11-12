# Explicit-state randomness

`RandomState` is an immutable xoshiro256** state seeded through splitmix64.
`RandomState.nextUInt64`, `nextFloat` and `split` return a value together with
the following state; earlier states remain valid, so any sequence can be stored,
branched and replayed. Ranvier.Anime's seeded random is a stateful closure, which
cannot be stored or branched, so D5 owns this small generator. It is not
cryptographically secure.

`Distribution` values follow d3-random and are validated when created; sampling
is then total. `Distribution.sample` draws one value; `Distribution.samples`
draws a bounded count (at most 100,000,000) in order.

| Constructor | Method |
|---|---|
| `uniform`, `integer` | Linear transform of one draw; integers are half-open |
| `normal`, `logNormal` | Box–Muller with two draws per sample and no cached spare |
| `exponential`, `pareto`, `logistic`, `weibull`, `cauchy` | Inverse transforms; Weibull shape 0 gives Gumbel, as in D3 |
| `gamma`, `beta` | Marsaglia–Tsang gamma (boosted below shape 1); beta from two gammas |
| `poisson` | Multiplication below mean 10, Hörmann's PTRS above |
| `binomial` | d3-random beta splitting followed by geometric counting |
| `geometric`, `bernoulli` | Trials to first success (at least 1); single comparison |
| `irwinHall`, `bates` | Sum or mean of uniform draws |

Tests check the xoshiro256** output against an independent reimplementation of
the reference algorithm, replay and split behaviour, and analytic moments of
every distribution with 200,000 seeded draws. Seeds make these checks
deterministic, so a six-standard-error band cannot flake. Gates cover 100,000
normal and Poisson(250) samples.

---
name: hypergraph-paclet-baseline
description: Baseline state of the Hypergraph Paclet merge project as of 2026-09-17
metadata: 
  node_type: memory
  type: project
  originSessionId: e3c3d21a-5f51-4cdd-ac11-634f3a2bc73c
  modified: 2026-09-17T19:09:24.700Z
---

The project merges two upstream paclets into one: `WolframInstitute/Hypergraph` (v1.2.3) and
`Wolfram/Multicomputation` (v0.1.9), cloned to `reference/Hypergraph` and
`reference/Multicomputation`. Both are installed locally at exactly those versions, so upstream
behaviour can be diffed against the clones.

Multicomputation already depends on Hypergraph **and on SetReplace** (see its
`Kernel/MulticomputationLoader.m`). SetReplace is what causes the four `::shdw` warnings on load
(`HypergraphToGraph`, `IsomorphicHypergraphQ`, `RandomHypergraph`, `ToPatternRules`).

Two upstream gaps matter most because labels are the project's headline feature:
- The `Hypergraph` constructor silently drops edge tags (`{1,2} -> "x"`). Upstream documents this
  in `docs/Symbols/EdgeListTagged.md` under "Possible Issues". Root cause is
  `Kernel/Hypergraph.m:213`, which rebuilds stored edges from the tag-stripping `he["EdgeList"]`
  accessor instead of `he["EdgeListTagged"]`.
- `IsomorphicHypergraphQ` takes no options and compares skeleton + symmetry only, so labelled
  isomorphism does not exist yet. `CanonicalHypergraph`'s `"Annotations" -> True` carries labels
  along passively but does not use them to refine the canonical vertex ordering.

The user's own `src/*.wl` files are exported research notebooks, not packages: `HypergraphAlgebras.wl`
and `HypergraphMinicourse.wl` are 100% commented out, `Chemoids.wl` has 2 live definitions,
`PlexAlgebra.wl` has ~15 plus a top-level `PacletInstall` and a `CloudGet` of a private cloud
`EinsteinSummation`. The WFR `ResourceFunction["EinsteinSummation"]` accepts the index-list form
(`{{1,2},{2,3}} -> {1,3}`) that `PlexProduct` uses, but **not** the string form (`"ij,jk->ik"`) that
`BM`/`Blades`/`TriForce`/`Fish`/`Boat` use — those need a string-to-index-list parser.

See [[wl-dev-toolchain]].

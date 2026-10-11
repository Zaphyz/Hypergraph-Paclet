---
name: causal-dependence-is-an-option
description: In the Hypergraph paclet, causal dependence between rewrite events is chosen by option, not fixed — the user's own design call
metadata:
  type: project
---

The user asked for the causality machinery (2026-09-30) with one design point they specified
themselves: **causal dependence must be an option, not a fixed meaning.** Their framing was that
the reference paclets only ever ask whether two rewrite events "shared some elements in common",
which is the weakest form — superposition of matching patterns — and that there should also be a
reading by **edge superposition**, where only whole shared edges count.

Implemented in `src/Causality.wl` as `"CausalDependence"` on `HypergraphCausalGraph` and on every
property of `HypergraphRewritingSystem`, with `$CausalDependenceTypes = {"Elements", "Edges",
"Vertices"}`. `"Elements"` is the default and the weakest; `"Edges"` the strictest; `"Vertices"`
exists so the three form a checkable identity — `"Elements"` is exactly `"Edges"` ∪ `"Vertices"`,
asserted over 120 generated systems.

**Why:** the user's point is that whether two events that meet only at a vertex are causally
connected is a fact about what the hypergraph is *standing for*, not about the rewriting. Two
compositions in disjoint parts of a chain share no arrow but do share the object between them.
The package should not decide that for the modeller.

**How to apply:** when extending the causal layer (causal invariance, branchial structure,
token-event graphs), keep the dependence reading a parameter threaded through, never hard-coded.
The uniform definition that makes this cheap is "two events are related when what they *touch*
overlaps", where touching = match ∪ creations, and the option only picks which tokens count.

See [[where-things-stand]] and [[keep-docs-in-sync]].

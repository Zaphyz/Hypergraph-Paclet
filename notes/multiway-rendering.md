---
name: multiway-rendering
description: How the Hypergraph paclet draws multiway graphs like the reference notebooks, and the three traps that took the longest
metadata:
  type: project
---

On 2026-09-30 the user asked for the multiway/causal graphs to render like the reference
notebooks, so they could check the causality engine by eye. What the references actually do:
states are framed hypergraph plots inset into graph vertices; **events are `HighlightRule`** —
the host with the match in red, an arrow, the result with the creations in red, both halves on
one layout. Colours come from `WolframPhysicsProjectStyleData`; those values are now copied into
`src/Causality.wl` rather than fetched, so the package loads offline.

Three things cost real time and will again:

1. **`Inset`'s size argument from `VertexShapeFunction` is a radius, not a diameter**, and the
   layout puts vertices one unit apart. A panel drawn at that size comes out half the width it
   has room for. `panelShapeFunction` uses `1.84 × Min[size]`.
2. **WL lays out *none* of a graph's vertices when given `VertexCoordinates` for only some.**
   The fix in `HypergraphPlot` is to take the automatic layout, write the given positions over
   it, and settle the rest by ~24 rounds of averaging against their neighbours.
3. **The reference's `VertexSize -> 64` blows the layout up** if copied literally — it is a
   scaled fraction, so vertices end up 64 graph units apart. Size insets in graph coordinates
   instead.

Option handling follows what the user chose: a registry `$HypergraphRewritingOptions` in three
groups (evolution, causality, drawing), joined with `Options[Graph]`; options stored on the
object as a third canonical argument; `sys["Options"]` reports what is in force.

See [[causal-dependence-is-an-option]] and [[rule-symbols-must-stay-clear]].

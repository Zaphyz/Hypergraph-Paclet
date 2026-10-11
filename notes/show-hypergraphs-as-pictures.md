---
name: show-hypergraphs-as-pictures
description: "Standing instruction for the Hypergraph Paclet — every notebook example should render its hypergraphs as diagrams, not as expressions"
metadata:
  node_type: memory
  type: feedback
  originSessionId: ea96dfe3-1c47-4909-b9c0-15b5daedc390
  modified: 2026-09-30T00:57:27.888Z
---

Given at the end of the 2026-09-30 session, as "a very deep instruction for all further work":
**in as many instances as possible, every notebook example should render its hypergraphs as
graphic elements.** Small hypergraphs illustrating each piece of functionality should be presented
in graphical form.

**Why:** the point of hypergraphs is that they are easily renderable as diagrams, so the point of
this package is to be visual and easy for a human to read. The user reviews the correctness of the
system by looking at it. An example whose output is `Hypergraph[{Vertex[1], ...}, {Edge[...]}]` or
a count or a list of vertex identifiers gives them nothing to check; the same example drawn does.
This is not a presentation preference — it is what makes review possible at all.

**How to apply:**
- Default to showing a hypergraph, a rule or an event as a picture. `HypergraphPlot`,
  `HypergraphRulePlot`, `HypergraphEventPlot`, and the graph-valued properties of
  `HypergraphRewritingSystem`, all draw. A summary box or an `InputForm` is the exception now,
  used when the point being made is about the expression rather than the object.
- Where a cell must show structure — a key list, a count, an association of tokens — put the
  picture beside it or in the cell before, so the number can be checked against the thing.
- Prefer several small hypergraphs drawn over one large one: four or five vertices is enough to
  make a point and is legible at notebook width.
- `AspectRatio -> Automatic` is usually wanted for a chain-shaped hypergraph; the squared default
  leaves it a thin band with unreadable labels. Panels inside a graph take it through
  `"HypergraphPlotOptions"`.
- This applies to `Documentation/build_docs.wls` examples as much as to the notebooks — see
  [[keep-docs-in-sync]].

Not yet applied in full: `RewritingBasics.nb` and `CausalityBasics.nb` still have many cells whose
output is a count, a list of pairs, or an association. Those are the cells to convert when work
resumes. See [[multiway-rendering]] for what is already drawable and
[[causal-dependence-is-an-option]] for the contrast that most wants a picture.

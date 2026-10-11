---
name: new-package-design
description: Design decisions and WL traps in the ground-up rewrite (src/Hypergraph.wl)
metadata: 
  node_type: memory
  type: project
  originSessionId: 10479b59-77e7-4aef-9db2-6f32fa61072a
  modified: 2026-09-18T01:33:25.621Z
---

The ground-up rewrite lives in `src/Hypergraph.wl`, context `WolframInstitute`Hypergraphs``
(plural, deliberately distinct from the installed upstream `WolframInstitute`Hypergraph``).
Checks are in `tests/BasicExamples.wls`.

Objects are `Vertex[id, lab]`, `Edge[{vertices}, sym, lab]` with sym one of "Directed" (default),
"Unordered", "Cyclic", and `Hypergraph[vertexList, edgeMultiset]`. Each constructor normalizes to
a canonical form via a single rewrite rule whose guard inspects **only the pattern variables** —
never the object expression itself, which would re-enter.

Three traps worth remembering:

- **`VertexQ` and `EdgeQ` are protected System symbols.** So are `Cyclic`, `VertexList`,
  `EdgeList`, `VertexCount`, `EdgeCount`, `VertexIndex`. `Vertex`, `Edge`, `Hypergraph`,
  `VertexID`, `VertexLabel`, `EdgeLabel`, `EdgeSymmetry` are free. The predicates are therefore
  named `VertexObjectQ` / `EdgeObjectQ`, and list accessors are UpValues on `Hypergraph`.
- **In `f[Edge[...]] := ...` the left-hand side's *arguments* are evaluated** even though the
  left-hand side as a whole is not. A pattern like `EdgeObjectQ[Edge[args___]]` therefore runs the
  `Edge` constructor on the pattern, and if the fallback does `Message[Edge::spec, Edge[args]]`
  it rebuilds the object and recurses forever. Match on `_Edge` / `_Vertex` / `_Hypergraph` and
  destructure with `Part`; wrap objects in `HoldForm` inside messages.
- **`assoc[Key[k]]` is not a lookup.** Direct application looks up the literal key `Key[k]`. Use
  `Lookup[assoc, Key[k]]`, which is also what keeps list-valued vertex identifiers working.

`src/HypergraphPlot.wl` (loaded from the end of `src/Hypergraph.wl`) ports the reference's
plotting and gives `Hypergraph` a `MakeBoxes` so it draws itself like a `Graph`. Three things
about the reference that are not apparent from reading it:

- **The reference does not actually use the layout it asks for.** It passes
  `GraphLayout -> Automatic`, inherited from `Options[Graph]` via `AbsoluteOptions`, *ahead* of
  its own explicit `GraphLayout -> {"SpringEmbedding", "EdgeWeighted" -> True}`, and WL takes the
  first setting of a repeated option. Using SpringEmbedding for real flings each large edge's
  helper vertex far out along its heavier spokes and wrecks the plot range. Matching the
  reference means using `Automatic`; that reproduces its plot ranges to the digit.
- **`LightDarkSwitched` only resolves under a live front end.** The reference wraps every colour
  in it on 14.3+. Rendered from a plain kernel the whole plot comes out as a pink error box, so
  this package uses the reference's pre-14.3 (light-mode) palette on an explicit white background.
- In the reference's `makeEdge`, the primitive passed in must be the *bare* `FilledCurve`;
  `makeEdge` adds the arrows itself from the `lines` argument. Passing the arrowed version both
  double-draws them and makes `RegionCentroid` fail, which leaves every label and badge at a
  symbolic position.

A hypergraph with an arity-zero edge lays that edge out as an isolated point and stretches the
plot range enormously (a 362x2889 image for three edges). The reference does exactly the same,
so it is inherited, not introduced.

**Labelled isomorphism is up to renaming of labels.** Labels match under a one-to-one
correspondence, not identically: `a,b` on a path matches `x,y`, but not `x,x`. Vertex labels and
edge labels are corresponded independently, and `None` is fixed (an unlabelled element only
matches an unlabelled one). The canonical form therefore replaces each label with a placeholder
`\[FormalL][i]`, ranked by the sorted keys of the elements carrying it — vertex indices for a
vertex label, label-free edges for an edge label. Vertex-label key lists are disjoint so that
ranking is unambiguous; two edge labels can tie, but exchanging them permutes edges within one
group and leaves the canonical form identical. Because label *values* are not invariant, no
invariant used to prune the search may mention them — `vertexInvariant` and `coarseInvariants`
use label multiplicities only.

`CanonicalHypergraph` picks a representative that depends on `Method`: Automatic pins each
vertex-invariant class to a fixed block of indices and so minimizes over a subset of what
"BruteForce" sees. Both are functions of the isomorphism class alone, so isomorphism verdicts
always agree (verified over 300 random pairs), but the representatives differ — never mix methods
in one comparison. "BruteForce" exists to cross-check the refinement.

See [[hypergraph-paclet-baseline]], [[wl-dev-toolchain]].

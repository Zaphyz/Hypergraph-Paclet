---
name: where-things-stand
description: "State of the Hypergraph Paclet rewrite at the end of 2026-09-18, and what comes next"
metadata: 
  node_type: memory
  type: project
  originSessionId: 10479b59-77e7-4aef-9db2-6f32fa61072a
  modified: 2026-09-18T21:36:48.551Z
---

As of 2026-09-18 the first milestone is complete and `tests/BasicExamples.wls` passes from a
clean kernel. Files that matter:

- `src/Hypergraph.wl` — core objects, isomorphism and `SimpleHypergraphQ`; loads the other four
  src files at its end, so one `Get` of this file brings in the whole package. It is the entry
  point despite the name, which is why `PacletInfo.wl` has to pair the context with its file.
- `src/HypergraphPlot.wl` — plotting and the default graphical display.
- `src/Hypermatrix.wl` — `ArrayObject`, `GenerateSymbolicArray`, `Hypermatrix` and their
  accessors. **Unified on 2026-09-24**: there is one array head, `ArrayObject[entries, symmetry]`,
  whatever the entries are. `NumericalArray` and `SymbolicArray` are gone, as is `ArrayName`.
  `SymbolicArray` became the *function* `GenerateSymbolicArray[name, dims, sym]`, returning an
  `ArrayObject` — it sits beside `ZeroArray`/`OneArray`/`IdentityArray`, which also build entries
  from a spec. The user picked the `Generate...` prefix over keeping the bare name.
  `ArrayDomain` is **computed, not declared**: it infers the narrowest of
  Boolean/Integer/Rational/Real/Complex/`FiniteField[p,n]`, else `"Expression"`. The `"Label"`
  domain added the day before was deleted — a computed domain made it unnecessary.
  Symmetry is still declared and verified; it is the one thing an array is told rather than asked.
  A `Hypermatrix` sorts by order, then dimensions, then symmetry, then WL canonical order.
  Naming was researched before the refactor: `ArrayObject` is free and pairs with the existing
  `ArrayObjectQ`; the old `NumericalArray` was one letter from the built-in **`NumericArray`**.
- `src/ArrayAlgebra.wl` — `ZeroArray`, `OneArray`, `IdentityArray`, `PartialIdentityArray`;
  `ArrayMultiply` (a native Einstein summation, no cloud dependency, taking either `"ij,jk->ik"`
  or `{{1,2},{2,3}} -> {1,3}`, and handling indices shared by three or more arrays, which a
  pairwise `TensorContract` cannot); `ArrayAdd` and `ArrayTimes` (entry by entry, variadic rather
  than taking a list, since a list of arrays is itself an array; a 0-array spreads over any shape,
  so scalar multiplication is `ArrayTimes` and needs no name of its own; `Plus` and `Times` on
  array objects are UpValues onto them; entries of any kind may be combined and the result is
  always an `ArrayObject`); and `FindArrayEquations`.
- `tests/BasicExamples.wls`, `tests/HypermatrixExamples.wls`, `tests/ArrayAlgebraExamples.wls` —
  the examples as assertions plus several thousand randomized checks each.
- `notebooks/HypergraphBasics.nb` — the review notebook, built by a generator kept in the session
  scratchpad (not in the project). The generator evaluates each cell in the kernel and boxes the
  result by hand; `NotebookEvaluate` from a script formats in OutputForm and loses every graphic.

The project is a git repository, branch `main`, with `reference/` gitignored — the two upstream
clones and the four moved research `.wl` files stay on disk but out of history (their contents
remain in the initial commit). `Documentation/` is tracked, notebooks and all: they are what a
reader opens, and a paclet is installed rather than built. Only the search index is ignored.
Four commits so far: `e553df5` (hypergraph core, isomorphism, plotting) on 2026-09-18,
`9415f05` (arrays, hypermatrices, array algebra, documentation) just after midnight on
2026-09-19, `7732451` (README and LICENSE) on 2026-09-21, and `9dcc839` (the drawing layer, the
adjacency bridge, the array unification, and the paclet) on 2026-09-25 — 62 files, +24093/-6624.
Working tree clean; `main` is **one commit ahead of `origin/main`**, so `9dcc839` still needs
pushing from GitHub Desktop.

Published 2026-09-21 to `https://github.com/Zaphyz/Hypergraph-Paclet`, **private** (the
unauthenticated GitHub API returns 404 for it). The user pushed the first two commits
themselves from GitHub Desktop. `git push` from the Bash tool fails — GitHub Desktop's
credentials are not reachable from that shell, so pushes are the user's to make from the
Desktop app.

Licensing: MIT, `Copyright (c) 2026 Carlos Zapata-Carratalá`, chosen because both upstream
paclets are MIT. `LICENSE` reproduces the upstream MIT notice for
`WolframInstitute/Hypergraph` (© 2023 Nikolay Murzin), since the drawing code in
`src/HypergraphPlot.wl` reuses parts of its implementation.

All four commits are authored `Carlos Zapata Carratala <c.zapata.carratala@gmail.com>`, a
personal address. Raised twice and left as-is; harmless while the repo is private, but it
becomes visible in the history if it is ever made public. Rewriting it means a force-push.

`.gitattributes` decides line endings for the repo, because the git bundled with GitHub Desktop
sets `core.autocrlf=true` system-wide: everything is `text=auto eol=lf`, with `*.nb` and `*.cdf`
marked `-text` so notebooks are never rewritten (the upstream paclets do the same, via LFS).

`PacletInfo.wl` is real as of 2026-09-24. `"Root" -> "src"` (not the usual `Kernel/`) and the
context is paired with its file, `{{"WolframInstitute`Hypergraphs`", "Hypergraph.wl"}}`, because
the entry file is singular while the context is plural — so no files had to move. It declares all
46 symbols, so once the directory is registered any of them loads the package on first use.
`PacletDirectoryLoad[dir]` is per-session; `PacletDirectoryAdd[dir]` is permanent but writes to
the Wolfram user configuration, so it was left for the user to run rather than done here. Their
`$UserBaseDirectory/Kernel/init.m` is the untouched default stub.

Both notebooks load themselves: an `InitializationCell -> True` cell doing `PacletDirectoryLoad` +
`Needs`, with the notebook options `InitializationCellEvaluation -> True` and
`InitializationCellWarning -> False`. **Unverified from here** — a headless `UsingFrontEnd` does not
evaluate on open, so neither this nor `NotebookDynamicExpression` could be tested with a marker
file; both mechanisms produced nothing. The options are written into the .nb and the mechanism is
the documented one, but only opening it in a real front end confirms it.

Repo size was measured on 2026-09-21 and is a non-issue: `notebooks/legacy/Chemoids.nb` is 30 MB,
`notebooks/legacy/` 39.7 MB in 4 files, and `.git` 14.7 MB. GitHub warns at 50 MB per file and
blocks at 100 MB, so no LFS and no history surgery were needed. An earlier note here overstated
this as a decision to settle before adding a remote.

`FindArrayEquations[ops, args]` follows the two-stage strategy of `FindTernaryCloneEquationsPairs`
in `reference/ArrayAlgebra.wl`: a term generator builds every nesting, a screen on random integer
arrays proposes candidates (sound, since identities agree on any arrays), and only the survivors
are verified on symbolic arrays. Verification expands each term's entries once and groups terms by
that expansion, rather than expanding a difference per pair, which is exact for polynomials and far
quicker than `FullSimplify`. Known results, all pinned by assertions: matrix multiplication on 3
arguments gives associativity alone; `Fish` on 5 gives two twisted ternary associativities (it gave
five before the redundancy filter, the other three being consequences); `BM` on 5 gives nothing.

Superseded 2026-09-24 by the unification: `ArrayMultiply` now accepts any mixture of entries and
always returns an `ArrayObject`; `ArrayMultiply::mixed` is gone, its stated reason (an ambiguous
result kind) having dissolved. `ArrayAdd`/`ArrayTimes` likewise always return an `ArrayObject`.
Contracting every index gives the 0-array (so `Normal` is needed to get the scalar —
the user's own `trace[u_List] := Normal @ ArrayMultiply[{u}, "pp->"]` already has it). An empty
group of index labels names a 0-array, so `",ij->ij"` scales a matrix; `StringSplit` drops empty
segments, so the spec parser splits the character list by hand. `FindArrayEquations` defaults to
`"Order" -> Automatic`, taking the smallest order from 1 to 5 on which every operation actually
evaluates — without it, matrix operations silently failed against the old default of 3 and the
empty result read as "no identities". A nesting that cannot evaluate is dropped (so a scalar-valued
operation such as a trace can sit at the top of a term); if no nesting that *uses an operation*
evaluates, `FindArrayEquations::eval` fires and `$Failed` is returned.

Settled on 2026-09-18: unary operations are the exception to "a term uses exactly the given
arguments" — every term is offered wrapped in up to `"UnaryDepth"` (default 2) nested unary
operations, which is what brings `perp[perp[a]] == a` within reach. That floods the result with
consequences (115 equations for `{perp, dot}` on 2 arguments), so `"Reduce" -> True` (default) drops
every equation derivable from the others: each is read as a schema over its arguments and turned
into a rewrite rule that fires only when it strictly decreases the term order (unary weight, then
`LeafCount`, then canonical order), which makes the rewriting terminate whatever the rules' shape —
including permutative ones such as the cyclic property of the trace. Ordering by size alone picks
the wrong direction for `perp[dot[u,v]] == dot[perp[v],perp[u]]`; the unary-weight measure (total
size of the arguments unary operations are applied to) pushes them towards the leaves, where doubled
ones cancel. Result: `{perp, dot}` on 2 arguments gives exactly involutivity and the
anti-homomorphism. What survives is a basis, not a canonical set. An identity is still verified at
one dimension only (`"Dimension"`, default 2).

Documentation is 40 reference pages plus the guide, all generated by `Documentation/build_docs.wls`,
which evaluates every example at build time; coverage is checked by comparing the page filenames
against `Names["WolframInstitute`Hypergraphs`*"]`.

`src/ArrayGraphics.wl` draws arrays, added 2026-09-24, split into two families and renamed
the same day. Every picture is one or the other, at every order:

- **Flat**, `ArrayGraphics` / `HypermatrixGraphics`: always a `Graphics`. Order 1 a row of cells, 2 a grid,
  and above that a nesting — the outer block's cells hold `Inset`s of the inner picture. Blocks are
  at most 2, so a 3-array is a row of matrices and a block of 3 is refused.
- **Spatial**, `ArrayGraphics3D` / `HypermatrixGraphics3D`: always a `Graphics3D`. Order 1 a
  line of cubes, 2 a plane, 3 a lattice; entry (i,j,k) at point {i,j,k}, a missing index standing
  at 1. Blocks are at most 3. Above that the blocks are **tiled, not inset** — a `Graphics3D`
  cannot contain another `Graphics3D`, so `prims3D` translates each sub-lattice by
  `(t - 1) (innerExtent + $blockGap)` and everything lands in one picture. `extent3D` computes the
  extent a block occupies.

`ArrayNesting[n]` is the flat default (blocks ≤ 2), `ArrayNesting[n, 3]` the spatial one. The rule:
blocks of the largest size allowed, remainder **off the front** — so flat 3 → {1,2} (a row of
matrices, the familiar reading) rather than {2,1}. Options are shared: `"Framed"`, `"Indices"`,
`"Gnomon"`, `"PlotTheme"`, `"MaxEntries"`, `"Nesting"`.

Naming, settled 2026-09-24: the drawing functions are `...Graphics`, not `...Plot` or `...Form`.
`ArrayPlot` and `ArrayPlot3D` are System symbols so "Plot" was never available for arrays, and a
`...Form` suffix in WL means a display wrapper like `MatrixForm` rather than something returning a
`Graphics`. `Cubix3D` was dropped as redundant (it was `ArrayGraphics3D` at order 3) and because it
silently swallowed a `"Nesting"` option passed to it (it prepended its own, and the first wins).
The cubix lineage is recorded in the docs and the notebook, not in a symbol name.

Cubes have translucent faces and `EdgeForm[None]`. The dashed wireframe they had first was removed
at the user's request; an outline on every cube of a lattice reads as a mesh rather than a set of
cells.

Four traps that cost time there, worth remembering:
- `Text` inside `Graphics` renders in **TraditionalForm** by default, so `a[1,2]` printed as
  `a(1,2)`. `BaseStyle -> {FormatType -> StandardForm}` on the `Text` does nothing; the option has
  to go on the enclosing `Graphics`/`Graphics3D`.
- Row-index labels sit outside the cells, so the `PlotRange` must reserve a left margin or they are
  silently clipped away.
- Cells must be sized to the widest entry (a character count of `ToString[e, InputForm]` is a good
  enough measure), or symbolic entries overflow each other.
- A nested picture's own opaque `Background` covers the outer cell it sits in, leaving only the
  rounded corners showing. Inner pictures are drawn with `Background -> None` and outer cells are
  outlined rather than filled.

`src/Adjacency.wl` bridges the two halves, added 2026-09-24. `AdjacencyHypermatrix[hg]` groups the
edges by **arity and symmetry type together** — an array holds one order and one symmetry — and
makes one array per group, which a `Hypermatrix` then sorts by exactly those keys. The symmetry
correspondence, written down in `$edgeToArraySymmetry`, is the whole content of the function:
`"Directed"`→`"Rigid"`, `"Cyclic"`→`"Cyclic"`, `"Unordered"`→`"Symmetric"`.

The trick that makes it work: counts are kept against `arrayCanonicalIndex[tuple, sym]`, the same
private function a `SymbolicArray` uses to decide which of its entries are the same entry. Every
tuple of an orbit then reads the same count, so each array carries its declared symmetry *by
construction* rather than being claimed to — and `NumericalArray` would reject it otherwise.

Indices run over `VertexList[hg]` order. By default entries are edge counts, so identical copies of
an edge come out as an integer multiple (confirmed at every symmetry type, and over the whole orbit
in each case). With `"Labeled" -> True` (added 2026-09-24) the entries are the edge labels instead,
0 where there is no edge and `None` for an edge carrying no label of its own -- distinguishable.
That needs `SimpleHypergraphQ[hg]`, also added then: no repeated edge **once labels are set aside**,
i.e. `DuplicateFreeQ` on the skeleton's edges, since one entry cannot hold two labels. A vertex
repeated inside one edge is allowed; edges of one arity in different symmetry types go to different
arrays and never collide. Labelled arrays carry the value domain `"Label"`, whose test admits
anything and which is therefore deliberately **kept out of `$ArrayDomains`** so inference can never
land on it. Vertex labels have no slot -- entries answer to edges.
Arity 0 gives a 0-array holding the count of empty edges; a hypergraph with no edges gives an empty
`Hypermatrix[{}]` (which is valid). Labels play no part — it is the adjacency of the skeleton.
Verified invertible: reading every canonical entry back out recovers the edge multiset exactly, on
300 random hypergraphs plus repeated-vertex, mixed-symmetry and arity-0 cases.

The index numbering is a **choice**, and this was checked rather than assumed (2026-09-24). It is
made by `makeHypergraph`, which ends with `Sort @ Values[canonicalVertex]`, so `VertexList` is
sorted -- not insertion order. Consequences: `AdjacencyHypermatrix` is a function of the Hypergraph
expression (reordering the input edges changes nothing), but **not** an isomorphism invariant,
because the sort is on the vertex identifiers. Two isomorphic hypergraphs named differently give
hypermatrices differing by the simultaneous permutation of all indices that the renaming induces
(verified explicitly at orders 2 and 3). `AdjacencyHypermatrix[CanonicalHypergraph[hg]]` **is** the
invariant -- pinned by 200 randomly-renamed hypergraphs. Left as a composition rather than given its
own name, so it stays clear which of the two is meant. The inverse
direction (hypermatrix → hypergraph) is **not** implemented; the round-trip reader lives only in
`tests/HypermatrixExamples.wls` §10.

Arrays still display as summary boxes by default; `ArrayGraphics` has to be called. Whether a small
array should draw itself the way a `Hypergraph` does was deliberately left alone, not decided.

Next up: rewriting rules, then the causal machinery, then plex diagrams. Further prototypes are in
`reference/ArrayAlgebra.wl` (ZigZag, Blades, TriForce, Boat, plex diagrams, Lie brackets) and
`reference/PlexAlgebra.wl`. Known rough edge worth fixing when the hypergraph drawing code is next
touched: an edge of arity zero lays out as an isolated point and stretches the plot range badly
(inherited from the reference, which does the same).

See [[new-package-design]], [[hypergraph-paclet-baseline]], [[wl-dev-toolchain]].

`src/Rewriting.wl` is the core of rewriting, added 2026-09-26. `HypergraphRule[input, output]`
(both sides anything `Hypergraph` accepts), `HypergraphRuleQ`, `HypergraphRuleMatches[rule, hg]`
for the matches alone, and `HypergraphRuleApply[rule, hg]` — also written `rule[hg]` — giving one
**event** per match: an association keyed `"Hypergraph"`, `"MatchVertices"`, `"MatchEdges"`,
`"MatchEdgePositions"`, `"NewVertices"`, `"NewEdges"`, `"DeletedVertices"`, `"RuleVertexMap"`,
`"Rule"`. Those key names and the `rule[hg]` syntax are deliberately the reference paclet's, so
rules written for it read the same; the matcher underneath is ours, the reference's resting on
`ResourceFunction["MultiReplace"]` and its own annotation system.

Semantics settled there: symmetry types must agree, and a pattern edge is matched over the
orderings its symmetry allows (1 for directed, k rotations for cyclic, k! permutations for
unordered — all genuinely distinct matches, all reported). Labels constrain and their absence does
not: an unlabelled pattern is a wildcard, `"Labeled" -> False` ignores labels. `"DistinctVertices"
-> True` (default, matching the reference) stops two pattern vertices landing on one. A destroyed
vertex is dropped **only if nothing still standing uses it** — reaching into unmatched edges to
strike a vertex out would not be local; this is a deliberate difference from the reference, which
does strike it out. New vertices are `\[FormalV][k]` chosen to miss what is there, deterministic
rather than `Unique`, so results reproduce across sessions.

`tests/RewritingExamples.wls` is the fourth suite. Its randomized section builds rules **out of
edges the hypergraph actually has**, renamed — a rule drawn at random matched in only 21 of 200
tries, and invariants that never fire prove nothing; built this way all 200 match, giving ~311
events over which the invariants are checked (binding carries input edges onto matched ones, edge
counts add up, nothing destroyed survives, kept edges remain).

Trap, cost an hour: `Documentation/build_docs.wls` evaluates examples in its own kernel, so an
example that **assigns to a helper** silently breaks the build. An example did `path = Hypergraph[...]`,
clobbering the `path` helper; `source[...]` then produced a `Missing[...]`, which `Export` cannot
write, so two pages vanished with no error at all — the build still reported success. There is now
a `ClearAll` and a comment before the rewriting section. Symptom to recognise: page count below
symbol count with no failed examples.

`notebooks/RewritingBasics.nb` is the third review notebook, added 2026-09-26: the rule object,
matching and what it turns on, events, labels, six worked examples, scope and limits. Its examples
are pinned as §5 of `tests/RewritingExamples.wls`, so the claim it makes about that is true.

The clobbering trap bit a **second** time, in the notebook generator rather than the doc builder:
an example named its rule `sub`, and the generator's own subsection helper was also `sub`, so four
subsection headers silently became `Missing[...]` and rendered as empty red boxes. The helper is
now `subsection`. Rule for any of these generators: **no helper may share a name with a variable
an example assigns to**, since both live in one kernel. Symptom: headers render as small empty
boxes, and nothing is reported wrong.

Labels became pattern variables in rewriting on 2026-09-26, after reading `reference/Chemoids.wl`.
A label on a rule's **input** is read three ways: a bare `Symbol` is a **variable** (matches
anything, binds, and every later occurrence must agree), `None` is a wildcard binding nothing, and
anything else — string, number, composite — is a literal. `Verbatim[f]` forces a symbol to be
literal; `"LabelPatterns" -> False` reads every label literally. The **output's** labels are
expressions in those variables, written out with the bindings substituted, and the bindings are
reported on the event as `"LabelBindings"`. Vertex labels bind exactly as edge labels do. Where
the output labels a vertex that label wins (relabelling is legitimate); where it does not, the
host's label stays — resolved inside `applyMatch` rather than left to the constructor, which would
otherwise warn `Hypergraph::vlbl` about a disagreement that is not one.

The symbol-is-a-variable convention was chosen by reading Chemoids, not invented: every label it
writes in a rule is a bare symbol meant as a variable (`x`, `y`, `p`, `q`), and every label in a
state is one too (`f`, `g`, `h`). Requiring `x_` would have made those notebooks unwriteable here
without editing every rule. Existing string-labelled rules were unaffected, which is why the
change broke only two key-list assertions.

Three Chemoids rules now reproduce and are pinned in `tests/RewritingExamples.wls` §6: line
composition (`{f g}` from edges labelled f and g), the unit rule (input is **one labelled vertex
with no edges** — binds the vertex label, creates a vertex carrying it and an edge labelled
`Subscript["1", p]`), and the dagger (reverses the edge, labels it `(f†)`, and twice gives
`((f†)†)` unreduced — deliberately not simplified, since whether the daggers cancel is a fact
about the algebra, not about rewriting).

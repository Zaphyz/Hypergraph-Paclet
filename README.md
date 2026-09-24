# Hypergraphs

A Wolfram Language package for hypergraph rewriting and the algebra around it.

Hypergraphs here are multisets of hyperedges, where a hyperedge is a set of vertices
together with a symmetry type. Vertices and edges carry labels independently. On top of
that sit hypermatrices — multidimensional arrays with a contraction calculus — and, in
time, rewrite rules, causal graphs, and the plex diagrams that connect the two halves.

> **Status: early.** The object model, isomorphism and the array algebra are implemented,
> documented and tested. Rewriting itself is not written yet. See [Roadmap](#roadmap).

## Loading

The two notebooks in `notebooks/` load the package themselves: they carry an initialization
cell that the front end evaluates on opening, so every symbol is ready before you read
anything. Nothing needs installing and nothing is copied — the repository itself is what
gets loaded, so an edit to `src/` is picked up next time.

Elsewhere, point the kernel at the repository and open the context:

```wolfram
PacletDirectoryLoad["<path to this repository>"]
Needs["WolframInstitute`Hypergraphs`"]
```

`PacletDirectoryLoad` lasts for the session. `PacletDirectoryAdd`, run once on the same
directory, registers it permanently — and because `PacletInfo.wl` declares the package's
symbols, any one of them then loads the package on first use, in any notebook, with no
`Needs` at all.

A plain `Get` of the entry file still works, and pulls in the other five source files:

```wolfram
Get[FileNameJoin[{"<path to this repository>", "src", "Hypergraph.wl"}]]
```

Everything lives in the context `` WolframInstitute`Hypergraphs` `` — plural, and
deliberately distinct from the upstream `` WolframInstitute`Hypergraph` `` so that both
can be loaded into one kernel.

Developed and tested against Wolfram Language 15.0; `PacletInfo.wl` claims 14.0+, which is
a conservative guess rather than a tested floor.  The test suites run under `wolframscript`.

## A quick tour

### Hypergraphs

A hypergraph is given as a list of edges. Each edge is a list of vertices, a symmetry type
— `"Directed"` (the default), `"Unordered"` or `"Cyclic"` — and an optional label.

```wolfram
Hypergraph[{Edge[{1, 2, 3}, "Unordered"], Edge[{3, 4}, "Directed"]}]
```

It draws itself, the way a `Graph` does; `HypergraphPlot` is needed only to pass options.
The three symmetry types are drawn distinctly, a cyclic edge carrying a circular arrow.

The standard graph functions work directly on it:

```wolfram
VertexList[%]
(* {Vertex[1], Vertex[2], Vertex[3], Vertex[4]} *)
```

### Isomorphism

Two hypergraphs are isomorphic when they are equal up to renaming the vertices and
reordering each edge according to its symmetry type, the hypergraph itself being an
unordered multiset of edges.

```wolfram
IsomorphicHypergraphQ[
  Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}],
  Hypergraph[{Edge[{"a", "b"}], Edge[{"b", "c"}]}]]
(* True *)

FindHypergraphIsomorphism[
  Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}],
  Hypergraph[{Edge[{"a", "b"}], Edge[{"b", "c"}]}]]
(* <|1 -> "a", 2 -> "b", 3 -> "c"|> *)
```

Labels are matched up to a one-to-one correspondence rather than identically, with vertex
labels and edge labels corresponded independently, on top of the isomorphism of the
underlying skeleton:

```wolfram
IsomorphicHypergraphQ[
  Hypergraph[{Edge[{1, 2}, "Directed", "x"], Edge[{2, 3}, "Directed", "y"]}],
  Hypergraph[{Edge[{1, 2}, "Directed", "p"], Edge[{2, 3}, "Directed", "q"]}]]
(* True:  x <-> p,  y <-> q *)

IsomorphicHypergraphQ[
  Hypergraph[{Edge[{1, 2}, "Directed", "x"], Edge[{2, 3}, "Directed", "y"]}],
  Hypergraph[{Edge[{1, 2}, "Directed", "p"], Edge[{2, 3}, "Directed", "p"]}]]
(* False:  two distinct labels cannot become one *)
```

`"Labeled" -> False` compares the skeletons instead, and `HypergraphSkeleton` strips the
labels outright.

### Arrays and contraction

There is one array object, `ArrayObject`, whatever its entries are: numbers, elements of a
finite field, symbolic expressions, labels. Nothing about the structure of an array depends
on what sits in it, so what it is made of is read off the entries by `ArrayDomain` rather
than declared alongside them. A scalar is the array of order zero. Entries may also be
generated rather than given — `GenerateSymbolicArray[a, {2, 2}]` builds them from a name,
as `ZeroArray` and `IdentityArray` build theirs from a shape.

`ArrayMultiply` is a native Einstein summation — no cloud dependency — taking either a
string or a rule between lists of index labels. An index may occur in any number of the
arrays and in the output as well, so diagonals and the three-way contractions of the
ternary products are expressible, which no sequence of pairwise contractions would give.

```wolfram
a = {{1, 2}, {3, 4}}; b = {{0, 1}, {1, 0}};

Normal @ ArrayMultiply[{a, b}, "ij,jk->ik"]   (* {{2, 1}, {4, 3}} — the matrix product *)
Normal @ ArrayMultiply[{a}, "ij->ji"]         (* the transpose *)
Normal @ ArrayMultiply[{a}, "ii->"]           (* 5 — the trace, as the 0-array *)
Normal @ ArrayMultiply[{x, y, z}, "ijp,ipk,pjk->ijk"]   (* a ternary product *)
```

Arrays add and multiply entry by entry too, and `Plus` and `Times` on array objects are
those same operations, so ordinary arithmetic notation works. A 0-array spreads over any
shape, which is why scalar multiplication needs no name of its own:

```wolfram
Normal[2 ArrayObject[a]]        (* {{2, 4}, {6, 8}} *)
Normal @ ArrayTimes[a, b]       (* {{0, 2}, {3, 0}} *)
```

### Drawing them

Two families, and an array of any order may be drawn in either. `ArrayGraphics` gives plane
pictures throughout — a row of cells, a grid, and above that a grid whose cells hold grids.
`ArrayGraphics3D` gives cubes arranged in space throughout — a line, a plane, a lattice, and
above that those lattices tiled through space, all in one picture you can turn and look at.

```wolfram
ArrayGraphics[{{1, 2, 3}, {4, 5, 6}}]                      (* a grid of cells *)
ArrayGraphics[Array[#1 + #2 + #3 &, {2, 2, 2}]]            (* a row of matrices *)
ArrayGraphics3D[Array[#1 + #2 + #3 &, {3, 3, 3}]]          (* a lattice of cubes *)
ArrayGraphics3D[Array[1 &, {2, 2, 2, 2}]]                  (* a line of lattices *)
```

Which indices go outside and which inside is a choice rather than a fact about the array, and
`"Nesting"` makes it. `HypermatrixGraphics` and `HypermatrixGraphics3D` draw every array of a
hypermatrix at once.

### From a hypergraph to a hypermatrix

This is where the two halves meet. An edge has an arity and a symmetry type; an array has an
order and a symmetry; and the two line up one for one — `"Directed"` with `"Rigid"`,
`"Cyclic"` with `"Cyclic"`, `"Unordered"` with `"Symmetric"`. Since an array holds one
symmetry and one order, the edges are gathered by arity and symmetry type together and each
group becomes one array.

```wolfram
h = Hypergraph[{Edge[{1, 2, 3}, "Unordered"], Edge[{3, 4}, "Directed"], Edge[{1, 2}, "Directed"]}];
AdjacencyHypermatrix[h]["Symmetries"]
(* {"Rigid", "Symmetric"} *)
```

Identical copies of an edge share an entry, so a repeated edge comes out as an integer multiple.
With `"Labeled" -> True` the entries are the edge labels themselves rather than counts, which
needs the hypergraph to be simple -- one entry cannot hold two labels, and `SimpleHypergraphQ`
tests for that.

The entry at an index tuple counts the edges sitting on it, looked up against the canonical
index of its orbit — so every tuple of an orbit reads the same count, and each array carries
its declared symmetry by construction. Nothing is lost: the canonical entries name exactly
the edges that went in, multiplicities and all, which the test suite checks by round-tripping
300 random hypergraphs.

### Searching for equational identities

`FindArrayEquations` takes a set of array operations and a list of generic arguments, forms
every way of nesting the operations over those arguments, and returns the equations that
hold between them. The search screens candidates on random integer arrays first — sound,
since identities agree on any arrays — and verifies only the survivors symbolically.

```wolfram
dot[u_List, w_List] := Normal @ ArrayMultiply[{u, w}, "ij,jk->ik"];
perp[u_List] := Normal @ ArrayMultiply[{u}, "ij->ji"];

FindArrayEquations[{dot}, {a1, a2, a3}, "Order" -> 2, "Dimension" -> 3]
(* {dot[a1, dot[a2, a3]] == dot[dot[a1, a2], a3]} *)

FindArrayEquations[{perp, dot}, {a1, a2}]
(* {a1 == perp[perp[a1]], dot[perp[a1], perp[a2]] == perp[dot[a2, a1]]} *)
```

Operations of arity one may be nested freely, which is what brings a law such as the
involutivity of a transpose within reach, and an equation that follows from the others is
left out rather than reported alongside them.

## Documentation

`Documentation/` holds a reference page for every exported symbol — 46 of them — and a
guide that groups them by subject. Open them in the front end:

```wolfram
NotebookOpen["Documentation/English/Guides/Hypergraphs.nb"]
```

They are generated by `Documentation/build_docs.wls`, which **evaluates every example at
build time**, so no printed output can drift away from what the code actually does. Rebuild
with:

```bash
wolframscript -file Documentation/build_docs.wls
```

The two notebooks in `notebooks/` are the discursive counterpart: worked examples with the
reasoning behind each design choice.

## Repository layout

| Path | |
| --- | --- |
| `src/Hypergraph.wl` | `Vertex`, `Edge`, `Hypergraph`, accessors, skeletons, canonical form and isomorphism. Loads the other five at its end. |
| `src/HypergraphPlot.wl` | `HypergraphPlot`, `HypergraphEmbedding`, and the default graphical display. |
| `src/Hypermatrix.wl` | `ArrayObject`, `GenerateSymbolicArray`, `Hypermatrix` and their accessors. |
| `src/ArrayAlgebra.wl` | Special arrays, `ArrayMultiply`, `ArrayAdd`, `ArrayTimes`, `FindArrayEquations`. |
| `src/ArrayGraphics.wl` | `ArrayGraphics`, `ArrayGraphics3D`, `HypermatrixGraphics`, `HypermatrixGraphics3D`, `ArrayNesting`. |
| `src/Adjacency.wl` | `AdjacencyHypermatrix`: the correspondence between edge symmetry types and array symmetries, counting or labelled. |
| `PacletInfo.wl` | The paclet's description of itself: kernel root, context, and the symbols that load it. |
| `Documentation/` | Reference pages and guide, with the script that generates them. |
| `notebooks/` | `HypergraphBasics.nb` and `HypermatrixBasics.nb`, the review notebooks. |
| `notebooks/legacy/` | Earlier research notebooks, kept for reference; not part of the package. |
| `tests/` | The examples as assertions, plus several thousand randomized checks. |
| `reference/` | Not in version control. Clones of the two upstream paclets, for comparison. |

## Tests

Each suite runs from a clean kernel and prints a line per check:

```bash
wolframscript -file tests/BasicExamples.wls
wolframscript -file tests/HypermatrixExamples.wls
wolframscript -file tests/ArrayAlgebraExamples.wls
```

Every example in the documentation appears here as an assertion, so the two cannot drift
apart silently.

## Roadmap

The plan, in the order it is being built:

- [x] **Hypermatrices** — definitions, arbitrary contraction by index specification,
      symmetry properties, and the drawing of arrays of any order, flat or as cubes in space.
- [x] **Hypergraphs** — definitions, properties, isomorphism, labels, labelled isomorphism,
      default graphical display. *A visual editor is still to come.*
- [x] **Hypergraphs to hypermatrices** — `AdjacencyHypermatrix`, with the edge symmetry types
      corresponding to the array symmetries. *The passage back is still to come.*
- [ ] **Rewriting rules** — definitions, rule application, a visual interface for editing a rule.
- [ ] **Rewriting systems and causality** — generations of states, causal graphs, causal
      dependence of events, conditions for dependence and for overlapping patterns.
- [ ] **Plex diagrams** — hypermatrix operations specified by labelled hypergraphs, and the
      equivalence between a plex product and a hypergraph rewriting procedure.

Prototypes for much of the unbuilt half exist in research files kept outside version control.

## Relation to the upstream paclets

This package merges and rewrites, from the ground up, functionality spread across two
existing paclets:

- [WolframInstitute/Hypergraph](https://github.com/WolframInstitute/Hypergraph)
- [WolframInstitute/Multicomputation](https://github.com/WolframInstitute/Multicomputation)

The strongest debt is to the first, whose graphical style the plotting code follows closely.
What is new here is the treatment of labels — labelled isomorphism up to a one-to-one
correspondence of labels — and the explicit connection between hypergraphs and hypermatrices.

## License

MIT. See [LICENSE](LICENSE), which also carries the notice for the upstream code this
package builds on.

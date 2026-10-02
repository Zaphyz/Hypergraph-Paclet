HYPERGRAPH PACLET

A powerful, user-friendly Wolfram Language package for hypergraph rewriting and related algebraic functionality.

Scope: This package includes a formalization of hypergraphs as multisets of hyperedges, which in turn are formalized as a set of vertices with a symmetry type. All elements of a hypergraph, both edges and vertices, can be independently labelled from a collection of dictionaries, which are sets of symbols that can be assembled into expressions (typically just character strings). All the basic functionality of basic graph theory is extended to hypergraphs: vertex and edge manipulation functions, simplicity, connectedness, isomorphism, labelled isomorphism, etc. A graphical interface is provided to visually define and edit hypergraph objects. An explicit connection between hypergraphs and hypermatrices (multidimensional arrays) is built via conversion functions that allow to interchangeably operate with hypergraphs or their adjacency array expressions. A major feature is the ability to define rewrite rules and rewrite systems. Labelled rewrite systems are implemented with full flexibility to do patern-matching on labels and underlying hypergraphs (sometimes called skeletons). There are functions to probe the properties of the causal graphs that result from application of rewrite rules to an initial state.

Terms to be used in the naming of main functions and objects: vertex, edge (short for hyperedge), hypergraph, skeleton, dictionary, label, rewrite rule, causal graph, rewrite event, adjacency hypermatrix, hypermatrix, hypermatrix operation, plex diagram.

Guiding principle for all development: this system is meant to be visual and easy for a human to read. The whole point of hypergraphs is that they are easily renderable as diagrams, and that is the point of this package. Therefore, in as many instances as possible, hypergraphs must be presented as graphic elements rather than as expressions. Every notebook example and every documentation example illustrating a piece of functionality should use a small hypergraph and show it drawn. This is not a matter of presentation: it is what makes the correctness of the system reviewable at all. An example whose output is a count, a list of vertex identifiers, or a raw Hypergraph expression gives the reader nothing to check; the same example drawn does. Where a cell must show structure — a list of keys, a count, an association — the picture belongs beside it or in the cell before, so that the number can be checked against the thing it describes. Prefer several small hypergraphs drawn over one large one.

A content index to follow when developing the project:

1. Hypermatrices

definitions, arbitrary operations with hypermatrices via contraction index specification, symmetry properties of hypermatrices, rendering of 1D, 2D and 3D hypermatrices

2. Hypergraphs

definitions, properties of hypergraphs, isomorphisms check functions, labels, labelled isomorphism, visual interface to define and edit hypergraph objects

3. Hypergraph Rewriting Rules

definitions, rule application, visual interface to define and edit a hypergraph rewrite rule

4. Hypergraph Rewriting Systems & Causality

definitions, generations of states, causal graph, causal dependence of rewriting events, conditions for causal dependence and overapping of patterns

5. Plex Diagrams: Equivalence between Hypergraph Rewriting and Hypermatrix Algebra

definitions, specifications of hypermatrix operations via plex diagrams (labelled hypergraphs), equivalence between a plex product and a hypergraph rewriting procedure


This package represents an update and extension of what is largely already possible to do with two Wolfram Language paclets available online:

https://github.com/WolframInstitute/Hypergraph

https://github.com/WolframInstitute/Multicomputation
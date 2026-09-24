(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- adjacency*)


(* ::Text:: *)
(*The bridge between the two halves of the package. A hypergraph is a multiset of hyperedges, each*)
(*with an arity and a symmetry type; an array has an order and a symmetry. The two vocabularies*)
(*line up exactly, and AdjacencyHypermatrix is what that correspondence looks like when written*)
(*down:*)
(**)
(*    an edge of arity k        <->  an array of order k*)
(*    "Directed"                <->  "Rigid"*)
(*    "Cyclic"                  <->  "Cyclic"*)
(*    "Unordered"               <->  "Symmetric"*)
(**)
(*An array carries one symmetry, so the edges are gathered by arity and symmetry type together,*)
(*and each group becomes one array. A Hypermatrix then holds those arrays in its own canonical*)
(*order, which is by order, then dimensions, then symmetry -- the same three things the grouping*)
(*was keyed on, so the hypermatrix comes out sorted by exactly what distinguishes its parts.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[AdjacencyHypermatrix];


(* ::Section:: *)
(*Usage*)


AdjacencyHypermatrix::usage =
    "AdjacencyHypermatrix[hg] gives the adjacency hypermatrix of the hypergraph hg: one array for " <>
    "each arity and symmetry type of edge it has, held in a Hypermatrix.\n" <>
    "An edge of arity k contributes to an array of order k, whose indices run over the vertices in " <>
    "the order VertexList[hg] gives them. The entry at an index tuple is the number of edges of " <>
    "that group sitting on it.\n" <>
    "An edge symmetry of \"Directed\", \"Cyclic\" or \"Unordered\" gives an array symmetry of " <>
    "\"Rigid\", \"Cyclic\" or \"Symmetric\" respectively.
" <>
    "Identical copies of an edge come out as an integer multiple in the entry they share.
" <>
    "With \"Labeled\" -> True the entries are the edge labels themselves rather than counts, and 0 " <>
    "where there is no edge. That needs the hypergraph to be simple, since one entry cannot hold " <>
    "two labels.";


(* ::Section:: *)
(*Messages*)


AdjacencyHypermatrix::hg = "`1` is not a hypergraph.";
AdjacencyHypermatrix::sym = "`1` is not a recognized edge symmetry type.";
AdjacencyHypermatrix::nonsimple =
    "Labels can be the entries of a hypermatrix only when no edge is repeated, since one entry \ncannot hold two labels. The given hypergraph has a repeated edge, labels aside; \nSimpleHypergraphQ tests for this.";


Begin["`Private`"];


(* ::Section:: *)
(*The correspondence*)


(* The one place the two vocabularies are tied together. Directed means no symmetry is claimed of
   the vertex order, which is rigid; cyclic means invariance under rotation, which is cyclic; and
   unordered means invariance under every permutation, which is symmetric. *)
$edgeToArraySymmetry = <|
    "Directed" -> "Rigid",
    "Cyclic" -> "Cyclic",
    "Unordered" -> "Symmetric"
|>;

arraySymmetryForEdge[s_] := Lookup[$edgeToArraySymmetry, s, $Failed];


(* ::Section:: *)
(*AdjacencyHypermatrix*)


Options[AdjacencyHypermatrix] = {"Labeled" -> False};

(* One array from one group of edges: those of arity k sharing a symmetry type.

   The entry at an index tuple is looked up against the canonical index of that tuple -- the least
   one in its orbit under the symmetry, which is the same arrayCanonicalIndex a symbolic array uses
   to decide which of its entries are the same entry. Every tuple of an orbit therefore reads the
   same value, which is what makes the array carry its declared symmetry by construction rather
   than by luck.

   Counting, the value is how many edges lie there, so identical copies of an edge come out as an
   integer multiple. Labelling, it is the edge's own label, and 0 where there is no edge; nothing
   declares a value domain, ArrayDomain reading it off the entries when asked. *)
adjacencyArray[k_Integer, edgeSym_String, pairs_List, n_Integer, labeledQ_] := Module[
    {sym, values, data},
    sym = arraySymmetryForEdge[edgeSym];

    values = If[ labeledQ,
        (* simplicity has already been checked, so no canonical index occurs twice here *)
        Association[Rule @@@ pairs],
        Counts[First /@ pairs]
    ];

    data = If[ k === 0,
        (* an edge of arity zero has no vertices to index, so its group is a single value *)
        If[labeledQ, Last @ First[pairs], Length[pairs]],
        Array[Lookup[values, Key[arrayCanonicalIndex[{##}, sym]], 0] &, ConstantArray[n, k]]
    ];

    ArrayObject[data, sym]
];

AdjacencyHypermatrix[hg_Hypergraph ? HypergraphQ, OptionsPattern[]] := Module[{
    labeledQ = TrueQ @ OptionValue["Labeled"], vertices, index, n, edges, groups
},
    edges = EdgeList[hg];

    If[ ! AllTrue[edges, KeyExistsQ[$edgeToArraySymmetry, EdgeSymmetry[#]] &],
        Message[AdjacencyHypermatrix::sym,
            EdgeSymmetry @ SelectFirst[edges, ! KeyExistsQ[$edgeToArraySymmetry, EdgeSymmetry[#]] &]];
        Return[$Failed, Module]
    ];

    (* A label can be an entry only if no two edges want the same entry, which is exactly what
       simplicity says. Counting has no such trouble: two edges in one place is a 2. *)
    If[ labeledQ && ! SimpleHypergraphQ[hg],
        Message[AdjacencyHypermatrix::nonsimple]; Return[$Failed, Module]
    ];

    vertices = VertexID /@ VertexList[hg];
    n = Length[vertices];
    index = AssociationThread[vertices -> Range[n]];

    (* Arity and symmetry together, because an array holds one order and one symmetry. *)
    groups = GroupBy[
        edges,
        {Length[EdgeVertices[#]], EdgeSymmetry[#]} &,
        Function[es,
            Function[e,
                {arrayCanonicalIndex[
                    Lookup[index, VertexID /@ EdgeVertices[e]],
                    arraySymmetryForEdge[EdgeSymmetry[e]]],
                 EdgeLabel[e]}
            ] /@ es
        ]
    ];

    Hypermatrix @ KeyValueMap[
        adjacencyArray[#1[[1]], #1[[2]], #2, n, labeledQ] &,
        groups
    ]
];

AdjacencyHypermatrix[hg_, ___] := (Message[AdjacencyHypermatrix::hg, HoldForm[hg]]; $Failed);


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];

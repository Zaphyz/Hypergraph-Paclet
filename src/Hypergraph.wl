(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs`*)


(* ::Text:: *)
(*Core objects -- Vertex, Edge, Hypergraph -- and isomorphism testing.*)
(*Ground-up rewrite. Isomorphism is an exhaustive search, so small cases only for now.*)
(**)
(*Load with:  Get["<project>/src/Hypergraph.wl"]*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect["WolframInstitute`Hypergraphs`*"];
ClearAll["WolframInstitute`Hypergraphs`*", "WolframInstitute`Hypergraphs`Private`*"];


(* ::Section:: *)
(*Usage*)


Vertex::usage =
    "Vertex[id] represents an unlabelled vertex whose unique identifier is the expression id.\n" <>
    "Vertex[id, lab] represents a vertex with identifier id carrying the label lab.\n" <>
    "A label of None means unlabelled, so Vertex[id, None] normalizes to Vertex[id].";

Edge::usage =
    "Edge[{v1, v2, ...}] represents a directed, unlabelled hyperedge on the given vertices.\n" <>
    "Edge[{v1, v2, ...}, sym] gives the hyperedge the symmetry type sym, one of \"Directed\" " <>
    "(the default), \"Unordered\" or \"Cyclic\".\n" <>
    "Edge[{v1, v2, ...}, sym, lab] additionally labels the hyperedge with lab.\n" <>
    "Vertices may be given as bare identifiers or as Vertex objects. The stored vertex list is " <>
    "put into the canonical order of its symmetry type, so two Edge objects are identical " <>
    "exactly when they are the same hyperedge.";

Hypergraph::usage =
    "Hypergraph[{edge1, edge2, ...}] builds a hypergraph from a list of Edge objects; its vertex " <>
    "set is the union of the vertices of those edges.\n" <>
    "Hypergraph[{v1, v2, ...}, {{...}, {...}, ...}] builds a hypergraph on an explicit vertex " <>
    "list, with edges given as lists of vertex identifiers, directed and unlabelled by default.\n" <>
    "Hypergraph[{v1, v2, ...}] builds an edgeless hypergraph.\n" <>
    "A Hypergraph is a multiset of edges: edge order is not significant, and repeated edges count.";

HypergraphQ::usage = "HypergraphQ[expr] gives True if expr is a valid Hypergraph object.";
EdgeObjectQ::usage = "EdgeObjectQ[expr] gives True if expr is a valid Edge object.";
VertexObjectQ::usage = "VertexObjectQ[expr] gives True if expr is a valid Vertex object.";

VertexID::usage = "VertexID[v] gives the identifier of the vertex v.";
VertexLabel::usage = "VertexLabel[v] gives the label of the vertex v, or None if it is unlabelled.";
EdgeVertices::usage = "EdgeVertices[e] gives the list of Vertex objects of the hyperedge e.";
EdgeSymmetry::usage = "EdgeSymmetry[e] gives the symmetry type of the hyperedge e.";
EdgeLabel::usage = "EdgeLabel[e] gives the label of the hyperedge e, or None if it is unlabelled.";

HypergraphSkeleton::usage =
    "HypergraphSkeleton[hg] gives the underlying unlabelled hypergraph of hg, dropping every " <>
    "vertex and edge label but keeping all edge symmetry types.";

CanonicalHypergraph::usage =
    "CanonicalHypergraph[hg] gives a canonical representative of the isomorphism class of hg, " <>
    "with vertex identifiers renamed to 1, 2, ... and labels replaced by the placeholders " <>
    "\[FormalL][1], \[FormalL][2], ..., since labels count only up to renaming. Two hypergraphs " <>
    "are isomorphic exactly when their canonical forms are identical.\n" <>
    "CanonicalHypergraph[hg, \"Labeled\" -> False] canonicalizes the skeleton instead.\n" <>
    "Which representative is chosen depends on Method: Automatic narrows the search using vertex " <>
    "invariants, while \"BruteForce\" searches all orderings of the vertices. Isomorphic " <>
    "hypergraphs give identical results under either setting, but the two settings pick " <>
    "different representatives of the same class, so do not mix them in one comparison.";

IsomorphicHypergraphQ::usage =
    "IsomorphicHypergraphQ[hg1, hg2] gives True if hg1 and hg2 are isomorphic as labelled " <>
    "hypergraphs: some renaming of vertex identifiers carries one to the other, preserving edge " <>
    "symmetry types and edge multiplicities, and carrying the labels of hg1 to those of hg2 by a " <>
    "one-to-one correspondence.\n" <>
    "Labels are therefore significant only up to renaming: what must agree is which elements " <>
    "share a label, not what the labels are. Vertex labels and edge labels are put in " <>
    "correspondence independently of each other, and an unlabelled element can only correspond " <>
    "to an unlabelled one.\n" <>
    "IsomorphicHypergraphQ[hg1, hg2, \"Labeled\" -> False] compares the skeletons instead, " <>
    "ignoring labels altogether.";

FindHypergraphIsomorphism::usage =
    "FindHypergraphIsomorphism[hg1, hg2] gives an association of vertex identifiers of hg1 to " <>
    "those of hg2 realizing an isomorphism, or an empty association if there is none.\n" <>
    "FindHypergraphIsomorphism[hg1, hg2, \"Labels\" -> True] gives instead an association with " <>
    "keys \"Vertices\", \"VertexLabels\" and \"EdgeLabels\", holding the vertex renaming and the " <>
    "two label correspondences that go with it.";


(* ::Section:: *)
(*Messages*)


Edge::sym = "`1` is not a valid edge symmetry type; expected \"Directed\", \"Unordered\" or \"Cyclic\".";
Edge::spec = "`1` is not a valid hyperedge specification.";
Hypergraph::spec = "`1` is not a valid hypergraph specification.";
Hypergraph::vlbl = "Vertex `1` was given more than one label (`2`); using `3`.";
CanonicalHypergraph::large =
    "Canonicalizing a hypergraph on `1` vertices requires searching `2` vertex orderings and may be slow.";


Begin["`Private`"];


(* ::Section:: *)
(*Vertices*)


(* Canonical form: Vertex[id] when unlabelled, Vertex[id, lab] otherwise.

   Note on style: Vertex, Edge and Hypergraph all carry constructor rules, and the arguments of
   a definition's left-hand side are evaluated even though the left-hand side as a whole is not.
   Writing f[Vertex[id_, ___]] := ... would therefore run the constructor on the pattern itself.
   Every definition below matches on _Vertex, _Edge or _Hypergraph and takes the object apart
   with Part instead. *)

Vertex[id_, None] := Vertex[id];

VertexObjectQ[v_Vertex] := Length[v] === 1 || (Length[v] === 2 && v[[2]] =!= None);
VertexObjectQ[_] := False;

VertexID[v_Vertex] := First[v];

VertexLabel[v_Vertex] := If[Length[v] === 2, v[[2]], None];

(* Bare identifiers are promoted to vertices. *)
toVertex[v_Vertex] /; VertexObjectQ[v] := v;
toVertex[id_] := Vertex[id];

stripVertexLabel[v_Vertex] := Vertex[VertexID[v]];


(* ::Section:: *)
(*Edges*)


$SymmetryTypes = {"Directed", "Unordered", "Cyclic"};

normalizeSymmetry[s_String] := Switch[ToLowerCase[s],
    "directed" | "ordered", "Directed",
    "unordered" | "undirected", "Unordered",
    "cyclic", "Cyclic",
    _, $Failed
];
normalizeSymmetry[_] := $Failed;

(* The canonical vertex order of an edge: the least element of its symmetry orbit. *)
canonicalOrder[vs_List, "Directed"] := vs;
canonicalOrder[vs_List, "Unordered"] := Sort[vs];
canonicalOrder[{}, "Cyclic"] := {};
canonicalOrder[vs_List, "Cyclic"] := First @ Sort @ NestList[RotateLeft, vs, Length[vs] - 1];

(* A single rewrite rule brings any Edge into canonical form. Its guard inspects only the
   pattern variables, never the Edge expression itself, so there is no re-entrancy. *)
canonicalEdgeArgsQ[vs_List, sym_String] :=
    AllTrue[vs, VertexObjectQ] && MemberQ[$SymmetryTypes, sym] && vs === canonicalOrder[vs, sym];
canonicalEdgeArgsQ[vs_List, sym_String, lab_] := lab =!= None && canonicalEdgeArgsQ[vs, sym];
canonicalEdgeArgsQ[___] := False;

Edge[args___] /; ! canonicalEdgeArgsQ[args] := makeEdge[args];

makeEdge[vs_List] := makeEdge[vs, "Directed", None];
makeEdge[vs_List, sym_] := makeEdge[vs, sym, None];
makeEdge[vs_List, sym_, lab_] := Module[{s = normalizeSymmetry[sym], nvs},
    If[ s === $Failed, Message[Edge::sym, sym]; Return[$Failed, Module]];
    nvs = canonicalOrder[toVertex /@ vs, s];
    If[lab === None, Edge[nvs, s], Edge[nvs, s, lab]]
];
makeEdge[args___] := (Message[Edge::spec, HoldForm[Edge[args]]]; $Failed);

EdgeObjectQ[e_Edge] := canonicalEdgeArgsQ @@ e;
EdgeObjectQ[_] := False;

EdgeVertices[e_Edge] := First[e];
EdgeSymmetry[e_Edge] := e[[2]];
EdgeLabel[e_Edge] := If[Length[e] === 3, e[[3]], None];

edgeIDs[e_Edge] := VertexID /@ EdgeVertices[e];

stripEdgeLabel[e_Edge] := Edge[EdgeVertices[e], EdgeSymmetry[e]];

(* Rebuild an edge over new vertices; the Edge constructor re-canonicalizes the order. *)
remapEdge[e_Edge, map_Association] :=
    Edge[Lookup[map, Key @ VertexID[#]] & /@ EdgeVertices[e], EdgeSymmetry[e], EdgeLabel[e]];

toEdge[e_Edge] /; EdgeObjectQ[e] := e;
toEdge[vs_List] := Edge[vs, "Directed"];
toEdge[spec_] := (Message[Edge::spec, spec]; $Failed);


(* ::Section:: *)
(*Hypergraphs*)


(* Canonical form: Hypergraph[sorted vertex list, sorted edge multiset], where every vertex
   occurring in an edge also appears in the vertex list, carrying the same label. *)
canonicalHypergraphArgsQ[vs_List, es_List] :=
    AllTrue[vs, VertexObjectQ] && AllTrue[es, EdgeObjectQ] &&
    vs === Sort[vs] && es === Sort[es] &&
    vs === Union[vs, Catenate[EdgeVertices /@ es]];
canonicalHypergraphArgsQ[___] := False;

Hypergraph[args___] /; ! canonicalHypergraphArgsQ[args] := makeHypergraph[args];

makeHypergraph[] := makeHypergraph[{}, {}];

makeHypergraph[x_List] :=
    If[ x =!= {} && AnyTrue[x, MatchQ[#, _Edge] || ListQ[#] &],
        makeHypergraph[{}, x],
        makeHypergraph[x, {}]
    ];

makeHypergraph[vs_List, es_List] := Module[
    {edges, declared, incident, grouped, labelOf, canonicalVertex},

    edges = toEdge /@ es;
    If[ ! AllTrue[edges, EdgeObjectQ], Return[$Failed, Module]];

    (* Declared vertices come first, so an explicit vertex list wins on label conflicts. *)
    declared = toVertex /@ vs;
    incident = Catenate[EdgeVertices /@ edges];
    grouped = GroupBy[Join[declared, incident], VertexID];

    labelOf = Map[
        Function[group,
            With[{labels = DeleteDuplicates @ DeleteCases[VertexLabel /@ group, None]},
                Which[
                    labels === {}, None,
                    Length[labels] === 1, First[labels],
                    True,
                    Message[Hypergraph::vlbl, VertexID @ First[group], labels, First[labels]];
                    First[labels]
                ]
            ]
        ],
        grouped
    ];

    (* Lookup with an explicit Key, since a vertex identifier may itself be a list. *)
    canonicalVertex = AssociationMap[Vertex[#, Lookup[labelOf, Key[#]]] &, Keys[grouped]];

    Hypergraph[
        Sort @ Values[canonicalVertex],
        Sort[remapEdge[#, canonicalVertex] & /@ edges]
    ]
];

makeHypergraph[args___] := (Message[Hypergraph::spec, HoldForm[Hypergraph[args]]]; $Failed);

HypergraphQ[hg_Hypergraph] := canonicalHypergraphArgsQ @@ hg;
HypergraphQ[_] := False;


(* ::Subsection:: *)
(*Accessors*)


(* A Hypergraph expression that fails to canonicalize evaluates to $Failed, so any surviving
   Hypergraph object is already in canonical two-argument form and these need no guard. *)

Hypergraph /: VertexList[hg_Hypergraph] := First[hg];
Hypergraph /: EdgeList[hg_Hypergraph] := Last[hg];
Hypergraph /: VertexCount[hg_Hypergraph] := Length[First[hg]];
Hypergraph /: EdgeCount[hg_Hypergraph] := Length[Last[hg]];

(hg_Hypergraph)[prop_String] /; HypergraphQ[hg] := hypergraphProp[hg, prop];

hypergraphProp[_, "Properties"] := {
    "VertexList", "EdgeList", "VertexCount", "EdgeCount", "VertexIDs",
    "VertexLabels", "EdgeLabels", "EdgeSymmetries", "Arity", "Skeleton"
};
hypergraphProp[hg_, "VertexList"] := VertexList[hg];
hypergraphProp[hg_, "EdgeList"] := EdgeList[hg];
hypergraphProp[hg_, "VertexCount"] := VertexCount[hg];
hypergraphProp[hg_, "EdgeCount"] := EdgeCount[hg];
hypergraphProp[hg_, "VertexIDs"] := VertexID /@ VertexList[hg];
hypergraphProp[hg_, "VertexLabels"] := VertexLabel /@ VertexList[hg];
hypergraphProp[hg_, "EdgeLabels"] := EdgeLabel /@ EdgeList[hg];
hypergraphProp[hg_, "EdgeSymmetries"] := EdgeSymmetry /@ EdgeList[hg];
hypergraphProp[hg_, "Arity"] := Length[EdgeVertices[#]] & /@ EdgeList[hg];
hypergraphProp[hg_, "Skeleton"] := HypergraphSkeleton[hg];
hypergraphProp[_, prop_] := Missing["UnknownProperty", prop];

(* Vertex labels have to be stripped inside the edges as well as from the vertex list: the
   constructor takes the label of any occurrence of a vertex as authoritative, so labelled
   Vertex objects left sitting in the edges would put the labels straight back. *)

HypergraphSkeleton[hg_Hypergraph] /; HypergraphQ[hg] :=
    Hypergraph[
        stripVertexLabel /@ VertexList[hg],
        Edge[stripVertexLabel /@ EdgeVertices[#], EdgeSymmetry[#]] & /@ EdgeList[hg]
    ];


(* ::Section:: *)
(*Canonical form and isomorphism*)


(* Labels are significant only up to renaming, so nothing that depends on a label's actual value
   may be used as an isomorphism invariant. What is invariant is how often a label occurs: a
   bijection between label sets preserves multiplicities. These are deliberately coarse -- a
   coarser invariant only widens the search, it can never make it wrong. *)

labelMultiplicity[labels_List][lab_] := If[lab === None, None, Count[labels, Verbatim[lab]]];

labelProfile[labels_List] := {
    Count[labels, None],
    Sort @ Values @ Counts @ DeleteCases[labels, None]
};

(* Vertices that correspond under an isomorphism share this invariant, so vertex orderings need
   only be searched within its classes. For a directed edge the positions a vertex occupies are
   themselves invariant; under the unordered and cyclic groups only the number of occurrences is,
   since those groups act transitively on the positions of an edge. *)
vertexInvariant[hg_, labeledQ_][v_] := With[{
    id = VertexID[v],
    vMult = labelMultiplicity[VertexLabel /@ VertexList[hg]],
    eMult = labelMultiplicity[EdgeLabel /@ EdgeList[hg]]
},
    {
        If[labeledQ, vMult[VertexLabel[v]], None],
        Sort @ Cases[
            EdgeList[hg],
            e_ /; MemberQ[edgeIDs[e], id] :> {
                Length @ EdgeVertices[e],
                EdgeSymmetry[e],
                If[labeledQ, eMult[EdgeLabel[e]], None],
                If[ EdgeSymmetry[e] === "Directed",
                    Flatten @ Position[edgeIDs[e], id],
                    Count[edgeIDs[e], id]
                ]
            }
        ]
    }
];

(* Blocks of vertices that may be permuted among themselves, laid out in a canonical order.
   Method -> "BruteForce" puts every vertex in one block, i.e. searches all n! orderings.

   Because the refined search pins each invariant class to a fixed block of indices, it minimizes
   over a subset of the orderings that "BruteForce" sees, and the two therefore settle on
   different -- but equally canonical -- representatives of the same isomorphism class. Both are
   functions of the isomorphism class alone, which is all the isomorphism test relies on; the
   two settings must not be mixed within a single comparison. "BruteForce" exists to cross-check
   the refinement. *)
vertexClasses[hg_, labeledQ_, method_] :=
    If[ method === "BruteForce",
        {VertexList[hg]},
        Values @ KeySort @ GroupBy[VertexList[hg], vertexInvariant[hg, labeledQ]]
    ];

(* Canonical placeholders for labels.

   Only the pattern of which elements share a label survives a renaming, not the label values, so
   the canonical form carries \[FormalL][1], \[FormalL][2], ... in place of the originals. Each
   label is ranked by the sorted list of keys of the elements carrying it -- the vertex indices
   for a vertex label, the label-free edges for an edge label -- which is a function of the
   already-canonical vertex numbering alone.

   For vertex labels those key lists are disjoint, so the ranking is never ambiguous. Two edge
   labels can share a key list, but then exchanging them permutes edges within a single group and
   leaves the relabelled hypergraph identical, so either choice gives the same canonical form. *)

labelTokens[labels_List, keys_List] := With[{
    groups = KeyDrop[GroupBy[Transpose[{labels, keys}], First -> Last, Sort], None]
},
    AssociationThread[Keys[#] -> \[FormalL] /@ Range[Length[#]]] & @ SortBy[groups, Identity]
];

(* Rename the vertices of hg to 1, 2, ... in the given order and, when labeledQ, replace the
   labels by their canonical placeholders. Returns the relabelled hypergraph together with the
   two maps from original label to placeholder, one for vertices and one for edges: composing one
   with the inverse of the other is exactly the label correspondence between two isomorphic
   hypergraphs. *)
relabelHypergraph[hg_, ordering_List, labeledQ_] := Module[{
    index, edges, edgeKeys, vTokens, eTokens, vertexOf
},
    index = Association @ MapIndexed[VertexID[#1] -> First[#2] &, ordering];
    edges = EdgeList[hg];

    If[ ! labeledQ,
        Return[
            {
                Hypergraph[
                    Vertex /@ Values[index],
                    Edge[Lookup[index, Key @ VertexID[#]] & /@ EdgeVertices[#], EdgeSymmetry[#]] & /@ edges
                ],
                <||>, <||>
            },
            Module
        ]
    ];

    edgeKeys = Edge[Lookup[index, Key @ VertexID[#]] & /@ EdgeVertices[#], EdgeSymmetry[#]] & /@ edges;
    vTokens = labelTokens[VertexLabel /@ ordering, Values[index]];
    eTokens = labelTokens[EdgeLabel /@ edges, edgeKeys];

    vertexOf = Association @ Map[
        VertexID[#] -> Vertex[
            Lookup[index, Key @ VertexID[#]],
            Lookup[vTokens, Key @ VertexLabel[#], None]
        ] &,
        ordering
    ];

    {
        Hypergraph[
            Values[vertexOf],
            Edge[
                Lookup[vertexOf, Key @ VertexID[#]] & /@ EdgeVertices[#],
                EdgeSymmetry[#],
                Lookup[eTokens, Key @ EdgeLabel[#], None]
            ] & /@ edges
        ],
        vTokens,
        eTokens
    }
];

(* Returns {canonical hypergraph, vertex ordering, vertex-label tokens, edge-label tokens}. *)
canonicalData[hg_, labeledQ_, method_] := Module[{classes, count, orderings},
    classes = vertexClasses[hg, labeledQ, method];
    count = Times @@ (Factorial[Length[#]] & /@ classes);
    If[count > 40320, Message[CanonicalHypergraph::large, VertexCount[hg], count]];
    orderings = Catenate /@ Tuples[Permutations /@ classes];
    First @ SortBy[
        Function[ord, Join[{#[[1]], ord}, Rest[#]] & @ relabelHypergraph[hg, ord, labeledQ]] /@ orderings,
        First
    ]
];

Options[CanonicalHypergraph] = {"Labeled" -> True, Method -> Automatic};

CanonicalHypergraph[hg_Hypergraph, OptionsPattern[]] /; HypergraphQ[hg] :=
    First @ canonicalData[hg, TrueQ @ OptionValue["Labeled"], OptionValue[Method]];


(* Cheap invariants, unquestionably preserved by isomorphism, used to reject early. *)
coarseInvariants[hg_, labeledQ_] := {
    VertexCount[hg],
    EdgeCount[hg],
    Sort[Length[EdgeVertices[#]] & /@ EdgeList[hg]],
    Sort[EdgeSymmetry /@ EdgeList[hg]],
    (* How many elements carry each label, not which label, since labels may be renamed. *)
    If[labeledQ, labelProfile[VertexLabel /@ VertexList[hg]], None],
    If[labeledQ, labelProfile[EdgeLabel /@ EdgeList[hg]], None]
};

Options[IsomorphicHypergraphQ] = Options[CanonicalHypergraph];

IsomorphicHypergraphQ[hg1_Hypergraph, hg2_Hypergraph, OptionsPattern[]] /;
    HypergraphQ[hg1] && HypergraphQ[hg2] :=
    With[{labeledQ = TrueQ @ OptionValue["Labeled"], method = OptionValue[Method]},
        coarseInvariants[hg1, labeledQ] === coarseInvariants[hg2, labeledQ] &&
        First[canonicalData[hg1, labeledQ, method]] === First[canonicalData[hg2, labeledQ, method]]
    ];

(* Two labels correspond when they are given the same canonical placeholder. *)
labelCorrespondence[tokens1_Association, tokens2_Association] := With[{
    inverse2 = AssociationThread[Values[tokens2] -> Keys[tokens2]]
},
    KeySort @ Association @ KeyValueMap[
        #1 -> Lookup[inverse2, Key[#2]] &,
        tokens1
    ]
];

Options[FindHypergraphIsomorphism] = Join[{"Labels" -> False}, Options[CanonicalHypergraph]];

FindHypergraphIsomorphism[hg1_Hypergraph, hg2_Hypergraph, OptionsPattern[]] /;
    HypergraphQ[hg1] && HypergraphQ[hg2] :=
    Module[{
        labeledQ = TrueQ @ OptionValue["Labeled"], method = OptionValue[Method],
        withLabels = TrueQ @ OptionValue["Labels"], d1, d2, empty
    },
        empty = If[withLabels, <|"Vertices" -> <||>, "VertexLabels" -> <||>, "EdgeLabels" -> <||>|>, <||>];
        If[ coarseInvariants[hg1, labeledQ] =!= coarseInvariants[hg2, labeledQ],
            Return[empty, Module]
        ];
        d1 = canonicalData[hg1, labeledQ, method];
        d2 = canonicalData[hg2, labeledQ, method];
        If[First[d1] =!= First[d2], Return[empty, Module]];
        If[ withLabels,
            Return[
                <|
                    "Vertices" -> KeySort @ Association @
                        Thread[(VertexID /@ d1[[2]]) -> (VertexID /@ d2[[2]])],
                    "VertexLabels" -> labelCorrespondence[d1[[3]], d2[[3]]],
                    "EdgeLabels" -> labelCorrespondence[d1[[4]], d2[[4]]]
                |>,
                Module
            ]
        ];
        (* Both orderings realize the same canonical form, so matching them position by
           position is an isomorphism.  Sorted by key, since the search order the pairs come out
           in carries no meaning and an Association compares key-order-sensitively. *)
        KeySort @ Association @ Thread[(VertexID /@ d1[[2]]) -> (VertexID /@ d2[[2]])]
    ];


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];


(* The rest of the package lives in separate files, loaded here so that a single Get of this one
   brings in everything. *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "HypergraphPlot.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Hypermatrix.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "ArrayAlgebra.wl"}]];

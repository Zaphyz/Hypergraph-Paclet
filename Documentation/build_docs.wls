#!/usr/bin/env wolframscript

(* Builds every page under Documentation/English/ from the definitions in this file.

   This script is the source of truth for the documentation: it holds the prose, the option
   descriptions and the examples. The .nb files are its output, so editing one by hand is lost
   the next time this runs. After adding a function, adding an option, or changing a default,
   update the relevant page below and re-run:

       wolframscript -file Documentation/build_docs.wls

   Each example is evaluated here in the kernel and its result attached as a StandardForm Output
   cell. NotebookEvaluate from a script formats results in OutputForm, which loses every
   graphic, so the boxing is done by hand.

   Symbol pages use the real Reference.nb stylesheet. The guide uses the ordinary Default.nb
   styles, because the Guide* styles resolve only inside the Documentation Center's own screen
   environment and collapse to body text anywhere else. *)

$ProgressReporting = False;
$root = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{$root, "src", "Hypergraph.wl"}]];

If[ ! MemberQ[Names["WolframInstitute`Hypergraphs`*"], "Hypergraph"],
    Print["FATAL: the package did not load from ", $root]; Exit[1]
];

$symbolDir = FileNameJoin[{$root, "Documentation", "English", "ReferencePages", "Symbols"}];
$guideDir  = FileNameJoin[{$root, "Documentation", "English", "Guides"}];
Quiet[CreateDirectory[#, CreateIntermediateDirectories -> True]] & /@ {$symbolDir, $guideDir};

(* ---------- cell helpers ---------- *)

(* Code-styled inline text, for things that really are Wolfram Language: a symbol name, an option
   setting, a small expression. Export parses the contents of a BoxData cell, which is what makes
   these render with proper spacing. *)
inline[s_String] := Cell[BoxData[StyleBox[s, "InlineCode"]], "InlineFormula"];

(* File paths must NOT go through inline: Export parses them as Wolfram code, so that
   "src/Hypergraph.wl" is stored as RowBox[{"src", "/", "Hypergraph", ".", "wl"}] and comes out
   reading "src / Hypergraph . wl". A StyleBox inside TextData is text-level and is left alone. *)
path[s_String] := StyleBox[s, FontFamily -> "Courier", FontWeight -> "Plain"];

(* Prose in this file is written across several source lines with backslash continuations. A
   backslash-newline is stripped by the parser only when the newline is a bare LF; if the file has
   CRLF endings (anything that has been through a Windows editor) the carriage return survives
   into the string and shows up as a hard line break mid-sentence. Flattening whitespace here
   makes the build independent of the file's line endings.

   Two passes, and no StringTrim. One pass would not collapse the doubled space that the newline
   rule has just created, because StringReplace does not re-scan what it has already written; and
   trimming would eat the single leading or trailing space that separates a run of prose from an
   adjacent inline code fragment. *)
tidy[s_String] := StringReplace[
    StringReplace[s, {"\r\n" -> " ", "\r" -> " ", "\n" -> " "}],
    RegularExpression[" {2,}"] -> " "
];

usage[code_String, desc_String] := Cell[TextData[{inline[code], " ", tidy[desc]}], "Usage"];
note[s_String] := Cell[TextData[tidy[s]], "Notes"];
noteC[pieces_List] := Cell[TextData[Replace[pieces, s_String :> tidy[s], {1}]], "Notes"];
exText[s_String] := Cell[TextData[tidy[s]], "ExampleText"];

$failed = {};
input[s_String] := Module[{res},
    res = Check[ToExpression[s], $evalFailed];
    If[res === $evalFailed, AppendTo[$failed, s]];
    If[ res === Null,
        {Cell[s, "Input"]},
        {Cell[s, "Input"], Cell[BoxData[ToBoxes[res, StandardForm]], "Output"]}
    ]
];

seeAlso[names_List] := Cell[
    TextData @ Riffle[inline /@ names, ", "],
    "SeeAlso"
];

(* file + section, deliberately without line numbers: those go stale on the next edit *)
source[file_String, section_String] := Cell[
    TextData[{"Defined in ", path[file], ", section ", "\[OpenCurlyDoubleQuote]", section,
              "\[CloseCurlyDoubleQuote]."}],
    "Notes"
];

page[name_String, usages_List, details_List, examples_List, src_, related_List] := Module[{nb},
    nb = Notebook[
        Flatten[{
            Cell[name, "ObjectName"],
            usages,
            If[details === {}, Nothing, {Cell["Details and Options", "NotesSection"], details}],
            If[examples === {}, Nothing, {Cell["Examples", "PrimaryExamplesSection"], examples}],
            Cell["Source", "NotesSection"],
            src,
            Cell["See Also", "SeeAlsoSection"],
            seeAlso[related]
        }],
        StyleDefinitions -> "Reference.nb",
        WindowSize -> {800, 750}
    ];
    Export[FileNameJoin[{$symbolDir, name <> ".nb"}], nb, "NB"];
    name
];

$core = "src/Hypergraph.wl";
$plot = "src/HypergraphPlot.wl";


(* ================= Vertices ================= *)

page["Vertex",
  {usage["Vertex[id]", "represents an unlabelled vertex whose unique identifier is the expression id."],
   usage["Vertex[id, lab]", "represents a vertex with identifier id carrying the label lab."]},
  {note["The identifier id may be any Wolfram Language expression, and is what makes the vertex unique within a hypergraph."],
   note["The label lab may be any expression. A label of None means no label, so Vertex[id, None] normalizes to the one-argument form Vertex[id]."],
   note["Because there is exactly one spelling per vertex, structural equality (===) on two Vertex objects already means that they are the same vertex."],
   note["Wherever a vertex is expected, a bare identifier may be given instead and is promoted to a Vertex automatically."]},
  {exText["An unlabelled vertex and a labelled one:"],
   input["{Vertex[1], Vertex[1, \"a\"]}"],
   exText["A label of None is dropped:"],
   input["Vertex[1, None]"],
   exText["Identifiers are arbitrary expressions:"],
   input["{Vertex[1], Vertex[\"a\"], Vertex[{1, 2}], Vertex[u], Vertex[f[3]]}"],
   exText["Two vertices are identical exactly when they are the same vertex:"],
   input["{Vertex[1] === Vertex[1], Vertex[1] === Vertex[1, \"a\"]}"]},
  source[$core, "Vertices"],
  {"VertexID", "VertexLabel", "VertexObjectQ", "Edge", "Hypergraph"}
];

page["VertexID",
  {usage["VertexID[v]", "gives the identifier of the vertex v."]},
  {note["The identifier is the first argument of the Vertex object, and is what distinguishes one vertex from another."]},
  {input["VertexID[Vertex[x, \"a\"]]"],
   exText["Reading the identifiers of a whole hypergraph:"],
   input["VertexID /@ VertexList[Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]]"],
   exText["The same list is available as a hypergraph property:"],
   input["Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}][\"VertexIDs\"]"]},
  source[$core, "Vertices"],
  {"Vertex", "VertexLabel", "VertexObjectQ"}
];

page["VertexLabel",
  {usage["VertexLabel[v]", "gives the label of the vertex v, or None if it is unlabelled."]},
  {note["A vertex identifier denotes one vertex, so it carries at most one label."],
   note["Within a hypergraph, a label attached to any occurrence of a vertex propagates to every other occurrence of that identifier."]},
  {input["{VertexLabel[Vertex[1, \"a\"]], VertexLabel[Vertex[1]]}"],
   exText["The labels of a whole hypergraph, in the order of its vertex list:"],
   input["Hypergraph[{Vertex[1, \"a\"], Vertex[2, \"b\"], 3}, {{1, 2}, {2, 3}}][\"VertexLabels\"]"]},
  source[$core, "Vertices"],
  {"Vertex", "VertexID", "EdgeLabel", "HypergraphSkeleton"}
];

page["VertexObjectQ",
  {usage["VertexObjectQ[expr]", "gives True if expr is a valid Vertex object, and False otherwise."]},
  {note["The names VertexQ and EdgeQ are protected System` symbols \[LongDash] the graph membership tests VertexQ[g, v] and EdgeQ[g, e] \[LongDash] so the predicates of this package are named VertexObjectQ and EdgeObjectQ instead."]},
  {input["{VertexObjectQ[Vertex[1]], VertexObjectQ[Vertex[1, \"a\"]], VertexObjectQ[1], VertexObjectQ[Edge[{1, 2}]]}"]},
  source[$core, "Vertices"],
  {"Vertex", "EdgeObjectQ", "HypergraphQ"}
];


(* ================= Edges ================= *)

page["Edge",
  {usage["Edge[{v1, v2, ...}]", "represents a directed, unlabelled hyperedge on the given vertices."],
   usage["Edge[{v1, v2, ...}, sym]", "gives the hyperedge the symmetry type sym."],
   usage["Edge[{v1, v2, ...}, sym, lab]", "additionally labels the hyperedge with lab."]},
  {note["The symmetry type sym is one of \"Directed\" (the default), \"Unordered\" or \"Cyclic\"."],
   note["The symmetry types are strings rather than symbols because Cyclic is already a protected System` symbol. \"Ordered\" is accepted as a synonym for \"Directed\", and the spelling is case-insensitive on input, but the stored form is always one of the three canonical strings."],
   note["Vertices may be given as bare identifiers or as Vertex objects."],
   note["The stored vertex list is put into the canonical order of its symmetry type: an unordered edge is sorted, a cyclic edge is rotated to its least rotation, and a directed edge is left as given. Two Edge objects are therefore identical exactly when they are the same hyperedge."],
   note["The symmetry type and the label are both part of an edge's identity. The symmetry type is always shown in the stored form, even when it is the default, because it changes the mathematics of the edge; the label is omitted when absent."],
   note["An edge may repeat a vertex, and may have any arity, including zero."],
   note["An unrecognized symmetry type produces the message Edge::sym and returns $Failed."]},
  {exText["The default symmetry type is \"Directed\":"],
   input["Edge[{1, 2, 3}]"],
   exText["The three symmetry types:"],
   input["{Edge[{1, 2, 3}, \"Directed\"], Edge[{1, 2, 3}, \"Unordered\"], Edge[{1, 2, 3}, \"Cyclic\"]}"],
   exText["A labelled edge:"],
   input["Edge[{1, 2, 3}, \"Cyclic\", \"f\"]"],
   exText["The stored vertex order is the least member of the symmetry orbit:"],
   input["{Edge[{3, 2, 1}, \"Unordered\"], Edge[{3, 1, 2}, \"Cyclic\"], Edge[{3, 2, 1}]}"],
   exText["A rotation of a cyclic edge is the same edge, but a reflection is not, since the cyclic group contains rotations only:"],
   input["{Edge[{1, 2, 3}, \"Cyclic\"] === Edge[{3, 1, 2}, \"Cyclic\"],\n Edge[{1, 2, 3}, \"Cyclic\"] === Edge[{1, 3, 2}, \"Cyclic\"]}"],
   exText["The symmetry type and the label are part of the identity:"],
   input["{Edge[{1, 2}] === Edge[{1, 2}, \"Unordered\"],\n Edge[{1, 2}, \"Directed\", \"f\"] === Edge[{1, 2}]}"],
   exText["An edge of arity zero, and one that repeats a vertex:"],
   input["{Edge[{}], Edge[{1, 1, 2}, \"Unordered\"]}"],
   exText["An unrecognized symmetry type is rejected:"],
   input["Quiet @ Edge[{1, 2}, \"Sideways\"]"]},
  source[$core, "Edges"],
  {"EdgeVertices", "EdgeSymmetry", "EdgeLabel", "EdgeObjectQ", "Vertex", "Hypergraph"}
];

page["EdgeVertices",
  {usage["EdgeVertices[e]", "gives the list of Vertex objects of the hyperedge e."]},
  {note["The list is in the canonical order of the edge's symmetry type, which is not necessarily the order in which the edge was written."]},
  {input["EdgeVertices[Edge[{1, 2, 3}, \"Cyclic\", \"f\"]]"],
   exText["The arity of an edge is the length of this list:"],
   input["Length @ EdgeVertices @ Edge[{1, 2, 3}]"],
   exText["Arities of a whole hypergraph:"],
   input["Hypergraph[{Edge[{1}], Edge[{1, 2}], Edge[{1, 2, 3}]}][\"Arity\"]"]},
  source[$core, "Edges"],
  {"Edge", "EdgeSymmetry", "EdgeLabel", "VertexID"}
];

page["EdgeSymmetry",
  {usage["EdgeSymmetry[e]", "gives the symmetry type of the hyperedge e."]},
  {note["The result is one of \"Directed\", \"Unordered\" or \"Cyclic\"."],
   note["The symmetry type governs which reorderings of the vertex list denote the same hyperedge: none for \"Directed\", all of them for \"Unordered\", and the rotations for \"Cyclic\"."],
   note["It also governs how the edge is drawn: a directed edge carries arrowheads along its sides, an unordered edge carries none, and a cyclic edge is marked with a small circular arrow."]},
  {input["EdgeSymmetry /@ {Edge[{1, 2}], Edge[{1, 2}, \"Unordered\"], Edge[{1, 2}, \"Cyclic\"]}"],
   exText["The symmetry types of a whole hypergraph:"],
   input["Hypergraph[{Edge[{1, 2}], Edge[{2, 3}, \"Unordered\"]}][\"EdgeSymmetries\"]"],
   exText["Symmetry types are respected by the isomorphism test:"],
   input["IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}]}], Hypergraph[{Edge[{1, 2}, \"Unordered\"]}]]"]},
  source[$core, "Edges"],
  {"Edge", "EdgeVertices", "EdgeLabel", "HypergraphPlot"}
];

page["EdgeLabel",
  {usage["EdgeLabel[e]", "gives the label of the hyperedge e, or None if it is unlabelled."]},
  {note["Edge labels take part in labelled isomorphism, where they must correspond one to one; see IsomorphicHypergraphQ."],
   note["Edge labels and vertex labels are put in correspondence independently of one another."]},
  {input["{EdgeLabel[Edge[{1, 2}, \"Directed\", \"f\"]], EdgeLabel[Edge[{1, 2}]]}"],
   exText["The labels of a whole hypergraph, in the order of its edge list:"],
   input["Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{2, 3}]}][\"EdgeLabels\"]"]},
  source[$core, "Edges"],
  {"Edge", "VertexLabel", "IsomorphicHypergraphQ", "HypergraphSkeleton"}
];

page["EdgeObjectQ",
  {usage["EdgeObjectQ[expr]", "gives True if expr is a valid Edge object, and False otherwise."]},
  {note["An Edge expression that fails to be built evaluates to $Failed rather than remaining as an Edge, so any surviving Edge object is in canonical form."]},
  {input["{EdgeObjectQ[Edge[{1, 2}]], EdgeObjectQ[{1, 2}], EdgeObjectQ[Vertex[1]]}"]},
  source[$core, "Edges"],
  {"Edge", "VertexObjectQ", "HypergraphQ"}
];


(* ================= Hypergraphs ================= *)

page["Hypergraph",
  {usage["Hypergraph[{edge1, edge2, ...}]", "builds a hypergraph from a list of Edge objects; its vertex set is the union of the vertices of those edges."],
   usage["Hypergraph[{v1, v2, ...}, {{...}, {...}, ...}]", "builds a hypergraph on an explicit vertex list, with edges given as lists of vertex identifiers, directed and unlabelled by default."],
   usage["Hypergraph[{v1, v2, ...}]", "builds an edgeless hypergraph on the given vertices."]},
  {note["A Hypergraph is a multiset of edges: the order of the edges carries no information, and a repeated edge counts once for each occurrence."],
   note["The vertex list and the edge list are both put into a canonical order when the hypergraph is built, so two ways of writing the same hypergraph produce literally the same expression and may be compared with ===."],
   note["Vertices occurring only inside edges are absorbed into the vertex set."],
   note["A label attached to any occurrence of a vertex propagates to every other occurrence of that identifier. If two occurrences disagree, the explicit vertex list wins and the message Hypergraph::vlbl is produced."],
   note["A Hypergraph displays as a drawing of itself, the way a Graph does; see HypergraphPlot. Feeding the displayed output back to the kernel recovers the object. A hypergraph too large to draw usefully displays as a summary box instead; see HypergraphLargeQ."],
   note["VertexList, EdgeList, VertexCount and EdgeCount work directly on a Hypergraph. They are protected System` symbols, extended here with UpValues rather than redefined."],
   noteC[{"Properties are read as ", inline["hg[\"prop\"]"], ". The available properties are given by ", inline["hg[\"Properties\"]"], "."}],
   note["An invalid specification produces the message Hypergraph::spec and returns $Failed."]},
  {exText["From a list of edges; the vertex set is inferred:"],
   input["Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{1, 2, 3}, \"Unordered\"]}]"],
   exText["The picture is the object itself; InputForm shows what is stored:"],
   input["InputForm @ Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{1, 2, 3}, \"Unordered\"]}]"],
   exText["From a vertex list together with edges as plain lists of identifiers:"],
   input["Hypergraph[{1, 2, 3}, {{1}, {1, 2}, {1, 2, 3}}]"],
   exText["An edgeless hypergraph, and the empty hypergraph:"],
   input["{Hypergraph[{1, 2, 3, 4}], Hypergraph[{}]}"],
   exText["A label given on one occurrence of a vertex propagates to the others:"],
   input["InputForm @ Hypergraph[{Edge[{Vertex[1, \"a\"], 2}], Edge[{1, 3}]}]"],
   exText["Edge order carries no information, but multiplicity does:"],
   input["{Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}] === Hypergraph[{Edge[{2, 3}], Edge[{1, 2}]}],\n EdgeCount[Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}]]}"],
   exText["The standard graph functions apply directly:"],
   input["h = Hypergraph[{Vertex[1, \"a\"], Vertex[2, \"b\"], 3}, {{1, 2}, {2, 3}}];\n{VertexCount[h], EdgeCount[h], VertexList[h]}"],
   exText["The available properties:"],
   input["h[\"Properties\"]"],
   input["{h[\"VertexIDs\"], h[\"VertexLabels\"], h[\"Arity\"], h[\"EdgeSymmetries\"]}"]},
  source[$core, "Hypergraphs"],
  {"Edge", "Vertex", "HypergraphQ", "HypergraphPlot", "HypergraphSkeleton", "IsomorphicHypergraphQ"}
];

page["HypergraphQ",
  {usage["HypergraphQ[expr]", "gives True if expr is a valid Hypergraph object, and False otherwise."]},
  {note["A Hypergraph expression that fails to be built evaluates to $Failed rather than remaining as a Hypergraph, so any surviving Hypergraph object is in canonical form."]},
  {input["{HypergraphQ[Hypergraph[{Edge[{1, 2}]}]], HypergraphQ[17], HypergraphQ[Edge[{1, 2}]]}"]},
  source[$core, "Hypergraphs"],
  {"Hypergraph", "VertexObjectQ", "EdgeObjectQ", "HypergraphLargeQ"}
];

page["SimpleHypergraphQ",
  {usage["SimpleHypergraphQ[hg]", "gives True if no edge of hg is repeated, and False otherwise."]},
  {note["Labels are set aside for the test. Two edges differing only by a label are the same edge of the same skeleton, and so make the hypergraph not simple; the test is DuplicateFreeQ on the edges of HypergraphSkeleton[hg]."],
   note["A vertex repeated within a single edge is allowed. That is a property of one edge rather than a collision between two, and it obstructs nothing."],
   note["Edges are stored with their vertices in the canonical order of their symmetry type, so two spellings of one unordered or cyclic edge are one edge, and a hypergraph holding both is not simple. Two directed edges that reverse each other are genuinely two edges, and it is."],
   noteC[{"Simplicity is what ", inline["AdjacencyHypermatrix"], " needs before it will put labels in the entries of a hypermatrix: one entry cannot hold two labels."}]},
  {input["{SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}],\n SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}]}"],
   exText["Labels do not make two edges different:"],
   input["SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"],\n                                Edge[{1, 2}, \"Directed\", \"b\"]}]"],
   exText["Two spellings of one unordered edge are one edge; two opposite directed edges are two:"],
   input["{SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, \"Unordered\"], Edge[{2, 1}, \"Unordered\"]}],\n SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, \"Directed\"], Edge[{2, 1}, \"Directed\"]}]}"],
   exText["A vertex repeated inside an edge is no obstacle:"],
   input["SimpleHypergraphQ @ Hypergraph[{Edge[{1, 1, 2}, \"Unordered\"]}]"]},
  source[$core, "Hypergraphs"],
  {"HypergraphSkeleton", "AdjacencyHypermatrix", "Hypergraph", "EdgeList"}
];

page["HypergraphSkeleton",
  {usage["HypergraphSkeleton[hg]", "gives the underlying unlabelled hypergraph of hg, dropping every vertex and edge label but keeping all edge symmetry types."]},
  {note["The same hypergraph is available as the property hg[\"Skeleton\"]."],
   note["Comparing skeletons is what IsomorphicHypergraphQ[hg1, hg2, \"Labeled\" -> False] does; the two are equivalent for deciding isomorphism and for the canonical form, though the witness returned by FindHypergraphIsomorphism need not be the same one."],
   note["Labels are removed from the Vertex objects inside the edges as well as from the vertex list. Leaving them in the edges would put every label back, since the constructor treats a label on any occurrence of a vertex as authoritative."]},
  {exText["A labelled hypergraph and its skeleton:"],
   input["l = Hypergraph[{Vertex[1, \"A\"], Vertex[2, \"B\"], Vertex[3, \"C\"]}, {{1, 2}, {2, 3}}];\n{l, HypergraphSkeleton[l]}"],
   exText["Every label is gone, while the symmetry types survive:"],
   input["s = HypergraphSkeleton[Hypergraph[{Edge[{Vertex[1, \"A\"], 2}, \"Unordered\", \"f\"]}]];\n{s[\"VertexLabels\"], s[\"EdgeLabels\"], s[\"EdgeSymmetries\"]}"],
   exText["The same thing as a property:"],
   input["l[\"Skeleton\"] === HypergraphSkeleton[l]"]},
  source[$core, "Accessors"],
  {"Hypergraph", "IsomorphicHypergraphQ", "CanonicalHypergraph", "VertexLabel", "EdgeLabel"}
];


(* ================= Canonical form and isomorphism ================= *)

page["CanonicalHypergraph",
  {usage["CanonicalHypergraph[hg]", "gives a canonical representative of the isomorphism class of hg."]},
  {note["The vertices of the result are renamed to 1, 2, ... and the labels are replaced by the placeholders \[FormalL][1], \[FormalL][2], ..., since labels count only up to renaming."],
   note["Two hypergraphs are isomorphic exactly when their canonical forms are identical."],
   noteC[{"With ", inline["\"Labeled\" -> False"], " the skeleton is canonicalized instead, and the result carries no labels at all."}],
   noteC[{"The setting ", inline["Method -> Automatic"], " narrows the search by grouping vertices into classes that no isomorphism can mix up. ", inline["Method -> \"BruteForce\""], " searches every ordering of the vertices instead, and exists to cross-check the default."}],
   note["Both Method settings are functions of the isomorphism class alone, so they always agree about whether two hypergraphs are isomorphic, but they settle on different representatives of the same class. The two settings must therefore not be mixed within a single comparison."],
   note["The search is exhaustive. When it would exceed 40320 candidate orderings the message CanonicalHypergraph::large is produced and the computation proceeds."]},
  {exText["Two spellings of the same directed path have the same canonical form:"],
   input["a = Hypergraph[{{1, 2}, {2, 3}}];\nb = Hypergraph[{{10, 20}, {20, 30}}];\n{CanonicalHypergraph[a], CanonicalHypergraph[b]}"],
   input["CanonicalHypergraph[a] === CanonicalHypergraph[b]"],
   exText["Labels become numbered placeholders, which is what makes two hypergraphs differing only by a renaming of labels reduce to the same form:"],
   input["h1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];\nh2 = Hypergraph[{Edge[{4, 5}, \"Directed\", \"x\"], Edge[{5, 6}, \"Directed\", \"y\"]}];\nInputForm /@ {CanonicalHypergraph[h1], CanonicalHypergraph[h2]}"],
   exText["Canonicalizing the skeleton instead:"],
   input["InputForm @ CanonicalHypergraph[h1, \"Labeled\" -> False]"]},
  source[$core, "Canonical form and isomorphism"],
  {"IsomorphicHypergraphQ", "FindHypergraphIsomorphism", "HypergraphSkeleton", "Hypergraph"}
];

page["IsomorphicHypergraphQ",
  {usage["IsomorphicHypergraphQ[hg1, hg2]", "gives True if hg1 and hg2 are isomorphic as labelled hypergraphs."]},
  {note["Some renaming of vertex identifiers must carry hg1 onto hg2: the edge multisets must match exactly, each edge matching up to its own symmetry type, with edge multiplicities preserved."],
   note["The renaming must also carry the labels of hg1 onto those of hg2 by a one-to-one correspondence. Labels are therefore significant only up to renaming: what must agree is which elements share a label, not what the labels are called."],
   note["Vertex labels and edge labels are put in correspondence independently of one another."],
   note["An element with no label can correspond only to an element with no label. Without this, every unlabelled hypergraph would be isomorphic to a labelled one whose labels were all alike."],
   noteC[{"With ", inline["\"Labeled\" -> False"], " the skeletons are compared instead, ignoring labels altogether. Labelled isomorphism always implies isomorphism of skeletons."}],
   noteC[{"The ", inline["Method"], " option is as for CanonicalHypergraph. Both settings always agree here."}],
   note["The search is exhaustive, and is comfortable up to roughly eight vertices."]},
  {exText["The same directed path on different identifiers:"],
   input["{IsomorphicHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{10, 20}, {20, 30}}]],\n IsomorphicHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{1, 2}, {1, 3}}]]}"],
   exText["Symmetry types are respected: a directed path is not an unordered one, but an unordered path on three vertices is an unordered star:"],
   input["{IsomorphicHypergraphQ[Hypergraph[{{1, 2}, {2, 3}}],\n                       Hypergraph[{Edge[{1, 2}, \"Unordered\"], Edge[{2, 3}, \"Unordered\"]}]],\n IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}, \"Unordered\"], Edge[{2, 3}, \"Unordered\"]}],\n                       Hypergraph[{Edge[{1, 2}, \"Unordered\"], Edge[{1, 3}, \"Unordered\"]}]]}"],
   exText["Labels count only up to a one-to-one renaming. Here a, b corresponds to x, y:"],
   input["h1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];\nh2 = Hypergraph[{Edge[{4, 5}, \"Directed\", \"x\"], Edge[{5, 6}, \"Directed\", \"y\"]}];\nIsomorphicHypergraphQ[h1, h2]"],
   exText["But h3 repeats a single label where h1 has two distinct ones, and no correspondence can do that:"],
   input["h3 = Hypergraph[{Edge[{4, 5}, \"Directed\", \"x\"], Edge[{5, 6}, \"Directed\", \"x\"]}];\n{IsomorphicHypergraphQ[h1, h3], IsomorphicHypergraphQ[h1, h3, \"Labeled\" -> False]}"],
   exText["The same principle governs vertex labels. A, A, B against A, B, B is not an isomorphism, because A would have to correspond to both A and B:"],
   input["{IsomorphicHypergraphQ[\n   Hypergraph[{Vertex[1, \"A\"], Vertex[2, \"A\"], Vertex[3, \"B\"]}, {{1, 2}, {2, 3}}],\n   Hypergraph[{Vertex[1, \"A\"], Vertex[2, \"B\"], Vertex[3, \"B\"]}, {{1, 2}, {2, 3}}]],\n IsomorphicHypergraphQ[\n   Hypergraph[{Vertex[1, \"A\"], Vertex[2, \"B\"], Vertex[3, \"C\"]}, {{1, 2}, {2, 3}}],\n   Hypergraph[{Vertex[1, \"C\"], Vertex[2, \"B\"], Vertex[3, \"A\"]}, {{1, 2}, {2, 3}}]]}"],
   exText["An unlabelled edge is not a labelled one:"],
   input["{IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"]}], Hypergraph[{Edge[{1, 2}]}]],\n IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"]}], Hypergraph[{Edge[{1, 2}]}],\n                       \"Labeled\" -> False]}"]},
  source[$core, "Canonical form and isomorphism"],
  {"FindHypergraphIsomorphism", "CanonicalHypergraph", "HypergraphSkeleton", "Hypergraph"}
];

page["FindHypergraphIsomorphism",
  {usage["FindHypergraphIsomorphism[hg1, hg2]", "gives an association of vertex identifiers of hg1 to those of hg2 realizing an isomorphism, or an empty association if there is none."],
   usage["FindHypergraphIsomorphism[hg1, hg2, \"Labels\" -> True]", "gives instead an association with keys \"Vertices\", \"VertexLabels\" and \"EdgeLabels\", holding the vertex renaming and the two label correspondences that go with it."]},
  {note["The notion of isomorphism is that of IsomorphicHypergraphQ, and the options are the same, with \"Labels\" added."],
   note["The result is sorted by key. When a hypergraph has a non-trivial automorphism group there are several valid witnesses, and which one is returned is not specified."],
   note["With \"Labels\" -> True and no isomorphism, all three associations come back empty."]},
  {exText["The vertex renaming alone:"],
   input["FindHypergraphIsomorphism[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{10, 20}, {20, 30}}]]"],
   exText["An empty association when there is none:"],
   input["FindHypergraphIsomorphism[Hypergraph[{{1, 2}, {2, 3}}], Hypergraph[{{1, 2}, {1, 3}}]]"],
   exText["Asking for the label correspondences as well:"],
   input["h1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];\nh2 = Hypergraph[{Edge[{4, 5}, \"Directed\", \"x\"], Edge[{5, 6}, \"Directed\", \"y\"]}];\nFindHypergraphIsomorphism[h1, h2, \"Labels\" -> True]"],
   exText["Vertex labels and edge labels are corresponded separately:"],
   input["i1 = Hypergraph[{Edge[{Vertex[1, \"A\"], Vertex[2, \"A\"]}, \"Directed\", \"f\"],\n                 Edge[{Vertex[2, \"A\"], Vertex[3, \"B\"]}, \"Directed\", \"g\"]}];\ni2 = Hypergraph[{Edge[{Vertex[1, \"P\"], Vertex[2, \"P\"]}, \"Directed\", \"g\"],\n                 Edge[{Vertex[2, \"P\"], Vertex[3, \"Q\"]}, \"Directed\", \"f\"]}];\nFindHypergraphIsomorphism[i1, i2, \"Labels\" -> True]"],
   exText["The labels can oblige the vertices to move. Here f is on two edges and g on one, while it is q that is on two and p on one, so vertex 4 must go to vertex 2:"],
   input["k1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{1, 3}, \"Directed\", \"f\"],\n                 Edge[{1, 4}, \"Directed\", \"g\"]}];\nk2 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"p\"], Edge[{1, 3}, \"Directed\", \"q\"],\n                 Edge[{1, 4}, \"Directed\", \"q\"]}];\nFindHypergraphIsomorphism[k1, k2, \"Labels\" -> True]"]},
  source[$core, "Canonical form and isomorphism"],
  {"IsomorphicHypergraphQ", "CanonicalHypergraph", "Hypergraph"}
];


(* ================= Plotting ================= *)

page["HypergraphPlot",
  {usage["HypergraphPlot[hg]", "draws the hypergraph hg."]},
  {note["A Hypergraph displays as this plot by default, the way a Graph does, so HypergraphPlot is needed only to pass options."],
   note["Binary edges are drawn as arcs, and edges of arity three and above as filled concave polygons through their vertices. An edge of arity one is drawn as a disk and an edge of arity zero as a circle."],
   note["The symmetry type of each edge is visible: a directed edge carries arrowheads running along it, an unordered edge carries none, and a cyclic edge is marked with a small circular arrow at its centre, drawn wider so as to encircle the edge's label when it has one."],
   noteC[{"With ", inline["VertexLabels -> Automatic"], " (the default) each vertex shows its label if it has one and its identifier otherwise. ", inline["VertexLabels -> None"], " turns them off, ", inline["\"Name\""], " always shows the identifier and ", inline["\"Label\""], " always the label."}],
   noteC[{inline["EdgeLabels -> Automatic"], " (the default) shows each edge's label where it has one; ", inline["\"Name\""], " shows the edge's vertex identifiers instead."}],
   noteC[{inline["\"EdgeArrows\" -> True"], " puts arrowheads on every edge whatever its symmetry type, and ", inline["False"], " on none. ", inline["\"CyclicBadges\" -> False"], " suppresses the circular arrows."}],
   note["The layout, colour palette and edge rendering follow the WolframInstitute/Hypergraph paclet."],
   note["All Graphics options are accepted."]},
  {exText["Edges of mixed arity and symmetry type:"],
   input["HypergraphPlot[Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"], Edge[{3, 4}],\n                            Edge[{1, 4, 5}, \"Cyclic\"]}], ImageSize -> 300]"],
   exText["The three symmetry types side by side:"],
   input["Grid[{{\"Directed\", \"Unordered\", \"Cyclic\"},\n      HypergraphPlot[Hypergraph[{Edge[{1, 2, 3, 4}, #]}], ImageSize -> 150] & /@\n        {\"Directed\", \"Unordered\", \"Cyclic\"}}, Frame -> All, Alignment -> Center]"],
   exText["Labelled edges; the cyclic badge encircles its label:"],
   input["HypergraphPlot[Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{2, 3}, \"Cyclic\", \"g\"],\n                           Edge[{1, 3, 4}, \"Unordered\", \"h\"]}], ImageSize -> 280]"],
   exText["Turning the vertex labels off:"],
   input["HypergraphPlot[Hypergraph[{{1, 2, 3}, {3, 4}}], VertexLabels -> None, ImageSize -> 220]"]},
  source[$plot, "HypergraphPlot"],
  {"Hypergraph", "HypergraphEmbedding", "HypergraphLargeQ", "EdgeSymmetry"}
];

page["HypergraphEmbedding",
  {usage["HypergraphEmbedding[hg]", "gives the coordinates chosen for the vertices of hg, in the order of VertexList[hg]."]},
  {note["The coordinates are those used by HypergraphPlot, and are obtained by laying out an auxiliary graph."],
   note["Any option of HypergraphPlot may be given, and affects the embedding in the same way."]},
  {input["HypergraphEmbedding[Hypergraph[{{1, 2}, {2, 3}}]]"],
   exText["One coordinate per vertex, in the order of the vertex list:"],
   input["h = Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"], Edge[{3, 4}]}];\nThread[h[\"VertexIDs\"] -> HypergraphEmbedding[h]]"]},
  source[$plot, "HypergraphPlot"],
  {"HypergraphPlot", "Hypergraph"}
];

page["HypergraphLargeQ",
  {usage["HypergraphLargeQ[hg]", "gives True when hg is big enough that it displays as a summary box instead of a plot."]},
  {note["The thresholds are \"MaxVertices\" (32), \"MaxEdges\" (128) and \"MaxTotalElements\" (196), and are changed with SetHypergraphPlotThresholds."]},
  {input["{HypergraphLargeQ[Hypergraph[{{1, 2}}]], HypergraphLargeQ[Hypergraph[Table[Edge[{i, i + 1}], {i, 40}]]]}"],
   exText["A large hypergraph displays as a summary box:"],
   input["Hypergraph[Table[Edge[{i, i + 1}], {i, 40}]]"]},
  source[$plot, "Default formatting"],
  {"SetHypergraphPlotThresholds", "HypergraphPlot", "Hypergraph", "HypergraphQ"}
];

page["SetHypergraphPlotThresholds",
  {usage["SetHypergraphPlotThresholds[\"MaxVertices\" -> n, ...]", "sets the sizes beyond which a Hypergraph displays as a summary box rather than a plot."]},
  {note["The recognized keys are \"MaxVertices\", \"MaxEdges\" and \"MaxTotalElements\"; the defaults are 32, 128 and 196."],
   note["Only the keys given are changed, and the new thresholds apply to every subsequent display."]},
  {exText["Making the threshold small enough that a small hypergraph is summarized:"],
   input["SetHypergraphPlotThresholds[\"MaxVertices\" -> 2];\nHypergraphLargeQ[Hypergraph[{{1, 2, 3}}]]"],
   exText["Putting it back:"],
   input["SetHypergraphPlotThresholds[\"MaxVertices\" -> 32];\nHypergraphLargeQ[Hypergraph[{{1, 2, 3}}]]"]},
  source[$plot, "Default formatting"],
  {"HypergraphLargeQ", "HypergraphPlot", "Hypergraph"}
];



(* ================= Arrays and hypermatrices ================= *)

$hm = "src/Hypermatrix.wl";

(* The hypergraph pages above bind several single letters (a, h, l, s) as examples. Release them
   so that the symbolic-array examples below can use a, b and t as array names. *)
ClearAll[a, b, c, d, e, f, g, h, l, m, s, t, u, v, w, x, y, z];
page["ArrayObject",
  {usage["ArrayObject[data]", "represents the array with entries data."],
   usage["ArrayObject[data, sym]", "additionally declares the index symmetry sym."]},
  {note["The entries data must form a rectangular array. A ragged list is not one."],
   note["The entries themselves may be anything at all: numbers, elements of a finite field, symbolic expressions, labels from a dictionary. Nothing about the structure of an array depends on what sits in it, so there is one array object rather than one per kind of entry."],
   noteC[{"What the array is made of is read off the entries by ", inline["ArrayDomain"], " rather than declared alongside them, and an array whose entries answer to no recognized domain is an array of expressions."}],
   note["A scalar is accepted as the array of order zero. It has no indices and a single entry, and it is what a full contraction leaves behind and what multiplies an array entry by entry, so scalar multiplication is nothing but ArrayTimes with a 0-array."],
   note["The index symmetry sym is one of \"Rigid\" (the default), \"Cyclic\" or \"Symmetric\". The spelling is case-insensitive on input, but the stored form is always one of the three."],
   note["A declared symmetry is checked against the entries, and rejected with ArrayObject::nosym if they are not invariant under the corresponding index permutations. Only a regular array can carry a symmetry other than \"Rigid\". The symmetry is the one thing an array is told rather than asked, because it is a claim about the entries that they may or may not bear out."],
   note["An array displays as a summary box. Normal gives its entries, and ArrayGraphics draws them."]},
  {exText["A vector, a matrix and a 3-array:"],
   input["{ArrayObject[{1, 2, 3}], ArrayObject[{{1, 2}, {3, 4}}],\n ArrayObject[Array[#1 + #2 + #3 &, {2, 2, 2}]]}"],
   exText["The entries come back with Normal:"],
   input["Normal @ ArrayObject[{{1, 2}, {3, 4}}]"],
   exText["The domain is read off the entries, and it is the narrowest that contains them all:"],
   input["ArrayDomain @ ArrayObject[#] & /@ {{True, False}, {1, 2}, {1/2, 3}, {1.5, 2}, {1 + I, 2}}"],
   exText["Entries of a finite field are recognized too:"],
   input["ArrayDomain @ ArrayObject[{FiniteField[2, 1][0], FiniteField[2, 1][1]}]"],
   exText["Entries that answer to none of them are simply expressions, and make a perfectly good array:"],
   input["{ArrayDomain @ ArrayObject[{\"a\", \"b\"}], ArrayDomain @ ArrayObject[{x, y}],\n ArrayDomain @ ArrayObject[{1, \"a\"}]}"],
   exText["A declared symmetry is verified. These entries are symmetric:"],
   input["ArraySymmetry @ ArrayObject[{{1, 2}, {2, 1}}, \"Symmetric\"]"],
   exText["\[Ellipsis] and these are not, so the declaration is refused:"],
   input["Quiet @ ArrayObject[{{1, 2}, {3, 4}}, \"Symmetric\"]"],
   exText["The same entries are perfectly good as a rigid array:"],
   input["ArraySymmetry @ ArrayObject[{{1, 2}, {3, 4}}]"],
   exText["A scalar is the array of order zero, with no dimensions and one entry:"],
   input["{ArrayOrder @ ArrayObject[5], ArrayDimensions @ ArrayObject[5],\n Normal @ ArrayObject[5]}"]},
  source[$hm, "ArrayObject"],
  {"GenerateSymbolicArray", "Hypermatrix", "ArrayOrder", "ArrayDimensions", "ArraySymmetry", "ArrayDomain", "ArrayEntries"}
];

page["GenerateSymbolicArray",
  {usage["GenerateSymbolicArray[name, dims]", "represents an array of the given dimensions whose entries are the expressions name[i1, i2, ...] formed from name and the indices."],
   usage["GenerateSymbolicArray[name, dims, sym]", "additionally declares the index symmetry sym."]},
  {note["The entries are not stored. They are generated on demand by ArrayEntries or Normal, so a symbolic array of any size costs nothing to hold."],
   note["The name must be a symbol or a string. The dimensions must be a list of positive integers. The empty list gives the array of order zero, whose single entry is the bare name."],
   note["The index symmetry sym is one of \"Rigid\" (the default), \"Cyclic\" or \"Symmetric\", and only a regular array can carry one other than \"Rigid\"."],
   note["Under a symmetry, index tuples that are equivalent name the same entry: the entry at a given tuple is the name applied to the least tuple in its orbit. A symmetric or cyclic symbolic array is therefore symmetric or cyclic by construction, with no checking needed."],
   noteC[{"The generated entries answer to no numeric domain, so ", inline["ArrayDomain"], " of such an array is ", inline["\"Expression\""], "."}]},
  {exText["The entries are the name applied to the indices:"],
   input["Normal @ GenerateSymbolicArray[a, {2, 3}]"],
   exText["Under a symmetry, equivalent index tuples name one entry:"],
   input["Normal @ GenerateSymbolicArray[a, {3, 3}, \"Symmetric\"]"],
   exText["So the generated array really is symmetric:"],
   input["With[{e = Normal @ GenerateSymbolicArray[a, {3, 3}, \"Symmetric\"]}, e === Transpose[e]]"],
   exText["At order three the three symmetries give different numbers of distinct entries:"],
   input["Length @ DeleteDuplicates @ Flatten @ Normal @ GenerateSymbolicArray[a, {3, 3, 3}, #] & /@\n  {\"Rigid\", \"Cyclic\", \"Symmetric\"}"],
   exText["At order two the cyclic group is the symmetric group, so the two coincide:"],
   input["Normal[GenerateSymbolicArray[a, {3, 3}, \"Cyclic\"]] === Normal[GenerateSymbolicArray[a, {3, 3}, \"Symmetric\"]]"],
   exText["What comes back is an ordinary array, and its domain is read off those entries:"],
   input["{Head @ GenerateSymbolicArray[a, {2}], ArrayDomain @ GenerateSymbolicArray[a, {2}]}"],
   exText["An irregular array cannot carry a symmetry:"],
   input["Quiet @ GenerateSymbolicArray[a, {2, 3}, \"Symmetric\"]"],
   exText["With no indices the single entry is the name itself:"],
   input["Normal @ GenerateSymbolicArray[a, {}]"]},
  source[$hm, "GenerateSymbolicArray"],
  {"ArrayObject", "Hypermatrix", "ArraySymmetry", "ArrayEntries", "RegularArrayQ"}
];

page["Hypermatrix",
  {usage["Hypermatrix[{arr1, arr2, ...}]", "represents the given arrays as a hypermatrix, holding them in canonical order."],
   usage["Hypermatrix[arr1, arr2, ...]", "is equivalent."]},
  {note["The arrays are ordered by array order, lowest first; then by dimensions, shorter first, compared term by term; then by symmetry, taking \"Symmetric\" before \"Cyclic\" before \"Rigid\"."],
   note["Two arrays alike in all three are put in the Wolfram Language's own canonical order, so that a hypermatrix is a function of the set of arrays it was given and of nothing else. Giving the same arrays in a different order produces the identical expression."],
   note["An irregular array is necessarily \"Rigid\", since only a regular array can carry a symmetry, so the symmetry criterion separates the regular arrays among themselves."],
   note["Arrays of any kind of entry sit together freely. Raw entries given in place of an array are promoted to an ArrayObject."],
   note["Length gives the number of arrays, and Normal the entries of each of them in order."]},
  {exText["The arrays are sorted whatever order they are given in:"],
   input["hm = Hypermatrix[{\n   ArrayObject[Array[1 &, {2, 2, 2}], \"Symmetric\"],\n   ArrayObject[{{1, 2}, {3, 4}}],\n   ArrayObject[{1, 2}],\n   ArrayObject[{1, 2, 3}],\n   ArrayObject[{{1, 2}, {2, 1}}, \"Symmetric\"],\n   ArrayObject[{{1, 2}, {2, 1}}, \"Cyclic\"]}]"],
   exText["Lowest order first, then the shorter dimensions, then the symmetries:"],
   input["{hm[\"Orders\"], hm[\"Dimensions\"], hm[\"Symmetries\"]}"],
   exText["The result does not depend on the order the arrays were given in:"],
   input["Hypermatrix[{ArrayObject[{1, 2}], ArrayObject[{{1, 2}, {3, 4}}]}] ===\n Hypermatrix[{ArrayObject[{{1, 2}, {3, 4}}], ArrayObject[{1, 2}]}]"],
   exText["Raw entries are promoted, and numerical and symbolic arrays may be mixed:"],
   input["Hypermatrix[{{1, 2}, GenerateSymbolicArray[a, {2, 2}]}][\"Domains\"]"],
   exText["The number of arrays, and their entries:"],
   input["{Length[hm], Normal @ Hypermatrix[{ArrayObject[{{1, 2}, {3, 4}}], ArrayObject[{1, 2}]}]}"]},
  source[$hm, "Hypermatrix"],
  {"ArrayObject", "GenerateSymbolicArray", "HypermatrixArrays", "HypermatrixQ", "ArrayOrder"}
];

page["ArrayOrder",
  {usage["ArrayOrder[arr]", "gives the number of indices of arr."]},
  {note["An array of order n is also called an n-array. A component vector is a 1-array and a matrix is a 2-array."],
   note["A scalar is the 0-array: it has no indices, and its list of dimensions is empty."],
   note["The order is the length of ArrayDimensions[arr]."],
   note["It is the first criterion by which a Hypermatrix orders its arrays."]},
  {input["ArrayOrder @ ArrayObject[#] & /@ {5, {1, 2, 3}, {{1, 2}, {3, 4}}, Array[1 &, {2, 2, 2}]}"],
   exText["Also available as a property, and for a whole hypermatrix:"],
   input["{ArrayObject[{{1, 2}, {3, 4}}][\"Order\"],\n Hypermatrix[{{1, 2}, {{1, 2}, {3, 4}}}][\"Orders\"]}"]},
  source[$hm, "Array accessors"],
  {"ArrayDimensions", "ArraySymmetry", "ArrayObject", "GenerateSymbolicArray", "Hypermatrix"}
];

page["ArrayDimensions",
  {usage["ArrayDimensions[arr]", "gives the list of lengths of the indices of arr."]},
  {note["Dimensions[arr] gives the same list. Dimensions is a protected System` symbol, extended here with UpValues on the two array heads rather than redefined."],
   note["An array is regular when all of these lengths are equal, and irregular otherwise; see RegularArrayQ. An array of order zero has no lengths at all, so its list of dimensions is empty and it counts as regular."],
   note["Dimensions are the second criterion by which a Hypermatrix orders its arrays, compared term by term among arrays of equal order."]},
  {input["ArrayDimensions @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}]"],
   exText["Dimensions works directly on an array object:"],
   input["Dimensions @ GenerateSymbolicArray[a, {2, 3, 4}]"],
   exText["Equal lengths make the array regular:"],
   input["{RegularArrayQ @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}],\n RegularArrayQ @ ArrayObject[{{1, 2}, {3, 4}}]}"]},
  source[$hm, "Array accessors"],
  {"ArrayOrder", "RegularArrayQ", "ArrayObject", "GenerateSymbolicArray"}
];

page["ArraySymmetry",
  {usage["ArraySymmetry[arr]", "gives the declared index symmetry of arr."]},
  {note["The result is one of \"Rigid\", meaning no symmetry is claimed; \"Cyclic\", meaning the entries are invariant under cyclic permutation of the indices; or \"Symmetric\", meaning they are invariant under all permutations."],
   note["Only a regular array can carry a symmetry other than \"Rigid\"."],
   noteC[{"The symmetry is the one thing an array is told rather than asked: ArraySymmetry reports what was declared, and the declaration is verified against the entries when the array is built. ", inline["GenerateSymbolicArray"], " builds entries that honour it by construction, equivalent index tuples naming the same entry, so there is nothing for the check to catch."}],
   note["A symmetric array is also cyclic, so the same entries may be declared either way. At order two the cyclic group is the symmetric group and the two coincide exactly."],
   note["Symmetry is the third criterion by which a Hypermatrix orders its arrays, taking \"Symmetric\" before \"Cyclic\" before \"Rigid\"."]},
  {input["ArraySymmetry @ ArrayObject[{{1, 2}, {2, 1}}, #] & /@ {\"Rigid\", \"Cyclic\", \"Symmetric\"}"],
   exText["The entries of the cyclic orbit of (1, 2, 3) are invariant under rotation but not under reflection, so they are cyclic and not symmetric:"],
   input["cyc = Array[If[MemberQ[{{1, 2, 3}, {2, 3, 1}, {3, 1, 2}}, {##}], 1, 0] &, {3, 3, 3}];\n{ArraySymmetry @ ArrayObject[cyc, \"Cyclic\"],\n Quiet @ ArrayObject[cyc, \"Symmetric\"]}"]},
  source[$hm, "Array accessors"],
  {"ArrayOrder", "ArrayDimensions", "RegularArrayQ", "ArrayObject", "GenerateSymbolicArray", "Hypermatrix"}
];
page["ArrayDomain",
  {usage["ArrayDomain[arr]", "gives the narrowest recognized value domain containing every entry of arr, or \"Expression\" if the entries lie in none of them."]},
  {note["The recognized domains are \"Boolean\", \"Integer\", \"Rational\", \"Real\" and \"Complex\", together with FiniteField[p, n]."],
   note["\"Boolean\" means the entries are True and False. Entries 0 and 1 are integers, and the elements of the two-element field are FiniteField[2, 1]."],
   note["It is computed from the entries, not declared: an array is not told what it is made of. An array whose entries answer to none of the recognized domains is an array of expressions, which is a perfectly good array; generated symbolic entries and labels are both of that kind."],
   note["Because it is computed rather than stored, it costs a pass over the entries to ask."]},
  {input["ArrayDomain @ ArrayObject[#] & /@ {{True, False}, {1, 2}, {1/2, 3}, {1.5, 2}, {1 + I, 2}}"],
   exText["Entries of a finite field:"],
   input["ArrayDomain @ ArrayObject[{FiniteField[2, 1][0], FiniteField[2, 1][1]}]"],
   exText["Entries belonging to no recognized domain are expressions:"],
   input["{ArrayDomain @ GenerateSymbolicArray[a, {2, 2}], ArrayDomain @ ArrayObject[{\"x\", \"y\"}],\n ArrayDomain @ ArrayObject[{1, \"x\"}]}"],
   exText["The domains of a whole hypermatrix:"],
   input["Hypermatrix[{{1, 2}, {1.5, 2.5}, GenerateSymbolicArray[a, {2}]}][\"Domains\"]"]},
  source[$hm, "Array accessors"],
  {"ArrayObject", "GenerateSymbolicArray", "ArrayEntries"}
];

page["ArrayEntries",
  {usage["ArrayEntries[arr]", "gives the entries of arr as a nested list."]},
  {note["Normal[arr] gives the same list."],
   note["The entries are whatever the array was built from, generated or given."],
   note["Normal of a Hypermatrix gives the entries of each of its arrays, in canonical order."]},
  {input["ArrayEntries @ ArrayObject[{{1, 2}, {3, 4}}]"],
   exText["Generated entries, here with a symmetry:"],
   input["ArrayEntries @ GenerateSymbolicArray[a, {2, 2}, \"Symmetric\"]"],
   exText["Normal is equivalent, and works on a hypermatrix too:"],
   input["Normal @ Hypermatrix[{ArrayObject[{{1, 2}, {3, 4}}], ArrayObject[{1, 2}]}]"]},
  source[$hm, "Array accessors"],
  {"ArrayObject", "GenerateSymbolicArray", "Hypermatrix", "ArrayDimensions"}
];

page["RegularArrayQ",
  {usage["RegularArrayQ[arr]", "gives True if every index of arr ranges over the same length, and False otherwise."]},
  {note["An array whose indices range over different lengths is called irregular."],
   note["Only a regular array can carry an index symmetry other than \"Rigid\", since permuting the indices of an irregular array would not give an array of the same shape."]},
  {input["{RegularArrayQ @ ArrayObject[{{1, 2}, {3, 4}}],\n RegularArrayQ @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}]}"],
   exText["An irregular array cannot be declared symmetric:"],
   input["Quiet @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}, \"Symmetric\"]"]},
  source[$hm, "Array accessors"],
  {"ArrayDimensions", "ArraySymmetry", "ArrayObject", "GenerateSymbolicArray"}
];

page["ArrayObjectQ",
  {usage["ArrayObjectQ[expr]", "gives True if expr is a valid ArrayObject or GenerateSymbolicArray, and False otherwise."]},
  {note["A ArrayObject or GenerateSymbolicArray expression that fails to be built evaluates to $Failed rather than remaining as one, so any surviving array object is in canonical form."],
   note["A bare nested list is not an array object, though Hypermatrix promotes one to an ArrayObject."]},
  {input["{ArrayObjectQ @ ArrayObject[{1, 2}], ArrayObjectQ @ GenerateSymbolicArray[a, {2}],\n ArrayObjectQ[{1, 2}], ArrayObjectQ[5]}"]},
  source[$hm, "Array accessors"],
  {"ArrayObject", "GenerateSymbolicArray", "HypermatrixQ", "RegularArrayQ"}
];

page["HypermatrixQ",
  {usage["HypermatrixQ[expr]", "gives True if expr is a valid Hypermatrix object, and False otherwise."]},
  {note["A Hypermatrix expression that fails to be built evaluates to $Failed, so any surviving Hypermatrix object holds valid arrays in canonical order."]},
  {input["{HypermatrixQ @ Hypermatrix[{{1, 2}}], HypermatrixQ[{ArrayObject[{1, 2}]}], HypermatrixQ[17]}"]},
  source[$hm, "Hypermatrix"],
  {"Hypermatrix", "HypermatrixArrays", "ArrayObjectQ"}
];

page["HypermatrixArrays",
  {usage["HypermatrixArrays[hm]", "gives the arrays of hm, in canonical order."]},
  {note["The same list is available as the property hm[\"Arrays\"]."],
   note["Normal[hm] gives the entries of each of those arrays instead."]},
  {input["HypermatrixArrays @ Hypermatrix[{{{1, 2}, {3, 4}}, {1, 2}}]"],
   exText["The properties of a hypermatrix read off its arrays in the same order:"],
   input["hm2 = Hypermatrix[{{{1, 2}, {3, 4}}, {1, 2}, GenerateSymbolicArray[a, {2, 2}, \"Symmetric\"]}];\n{hm2[\"Orders\"], hm2[\"Symmetries\"], hm2[\"Domains\"]}"]},
  source[$hm, "Hypermatrix"],
  {"Hypermatrix", "HypermatrixQ", "ArrayEntries"}
];


(* ================= Array algebra ================= *)

$aa = "src/ArrayAlgebra.wl";

ClearAll[a, b, c, d, e, f, g, h, l, m, s, t, u, v, w, x, y, z];

page["ArrayMultiply",
  {usage["ArrayMultiply[{arr1, arr2, ...}, spec]", "contracts the given arrays according to the index specification spec, in the manner of Einstein summation."]},
  {note["The specification may be written as a string, \"ij,jk->ik\", in which each character is an index label, or as a rule between lists of labels, {{1, 2}, {2, 3}} -> {1, 3}."],
   note["Indices named in the output part survive; every other index is summed over. With the output part omitted, the indices that occur exactly once survive, in sorted order, and the rest are summed."],
   note["An index may occur in any number of the arrays, and may occur in the output as well as in the inputs. That covers diagonals and three-way contractions, which a pairwise contraction could not express."],
   note["The arrays may be given as array objects or as plain nested lists, and an array object comes through as its entries. The result is an ArrayObject whatever went in."],
   note["Contracting every index away leaves a single entry, which comes back as the 0-array: an ArrayObject with no indices, whose Normal is the scalar itself."],
   note["An array of order zero is named by an empty group of labels, so that \",ij->ij\" multiplies a matrix by a scalar. The output part may equally be written empty, \"ij,ji->\", to ask for every index to be contracted away."],
   note["Arrays with numeric entries and arrays with symbolic ones may be contracted together: the entries of the result are simply the expressions the contraction builds, and there is only one kind of array for it to come back as."],
   note["The number of labels given for an array must equal its number of indices, and every occurrence of a label must have the same length, or the corresponding message is produced and $Failed returned."]},
  {exText["The ordinary matrix product:"],
   input["ArrayMultiply[{{{1, 2}, {3, 4}}, {{0, 1}, {1, 0}}}, \"ij,jk->ik\"]"],
   exText["Its entries, and the same contraction written with index lists:"],
   input["a = {{1, 2}, {3, 4}}; b = {{0, 1}, {1, 0}};\n\
{Normal @ ArrayMultiply[{a, b}, \"ij,jk->ik\"], Normal @ ArrayMultiply[{a, b}, {{1, 2}, {2, 3}} -> {1, 3}]}"],
   exText["Repeating an index within one array takes a diagonal, and dropping it from the output sums it:"],
   input["{Normal @ ArrayMultiply[{a}, \"ii->i\"], Normal @ ArrayMultiply[{a}, \"ii->\"]}"],
   exText["A transpose is a contraction that sums nothing:"],
   input["Normal @ ArrayMultiply[{a}, \"ij->ji\"]"],
   exText["An index may join three arrays at once. This is the Bhattacharya-Mesner product of three 3-arrays:"],
   input["x = Array[#1 + #2 + #3 &, {2, 2, 2}];\nNormal @ ArrayMultiply[{x, x, x}, \"ijp,ipk,pjk->ijk\"]"],
   exText["With the output part omitted, whatever occurs exactly once survives; here nothing does, so the result is the 0-array:"],
   input["ArrayMultiply[{{1, 2, 3}, {4, 5, 6}}, \"i,i\"]"],
   exText["An empty group of labels names an array of order zero, which is how a scalar enters a contraction:"],
   input["Normal @ ArrayMultiply[{3, a}, \",ij->ij\"]"],
   exText["Array objects are accepted as readily as raw lists:"],
   input["ArrayMultiply[{ArrayObject[a], ArrayObject[b]}, \"ij,jk->ik\"]"],
   exText["A symbolic array comes through as its list of lists of terms, and the contraction builds the corresponding expressions:"],
   input["sa = Normal @ GenerateSymbolicArray[p, {2, 2}]; sb = Normal @ GenerateSymbolicArray[q, {2, 2}];\nArrayMultiply[{sa, sb}, \"ij,jk->ik\"]"],
   exText["Numerical and symbolic arrays may not be mixed in one contraction:"],
   input["Quiet @ ArrayMultiply[{a, sb}, \"ij,jk->ik\"]"],
   exText["Contracting one array onto no indices at all gives the trace, on numerical and symbolic entries alike:"],
   input["trace[u_List] := Normal @ ArrayMultiply[{u}, \"pp->\"];\n{trace[a], trace[sa]}"]},
  source[$aa, "ArrayMultiply"],
  {"ArrayObject", "IdentityArray", "FindArrayEquations", "ArrayAdd", "ArrayTimes", "ArrayDimensions", "Hypermatrix"}
];

page["ArrayAdd",
  {usage["ArrayAdd[arr1, arr2, ...]", "adds the given arrays entry by entry."]},
  {note["The arrays are given directly, not in a list: a list of arrays is itself an array, and there would be no telling the two readings apart."],
   note["Every argument must have the same dimensions, except that an array of order zero has no shape of its own and is added to every entry."],
   note["The arrays may be given as array objects or as plain nested lists, and entries of any kind may be combined: the operation acts on one entry at a time, so a number meeting an expression has the obvious reading. The result is an ArrayObject."],
   note["A symmetry declared by every argument of order one or more is carried over to the result, since an entry-by-entry operation treats each index tuple separately. Where the arguments disagree, or where plain entries were given, nothing is claimed and the result is \"Rigid\"."],
   noteC[{inline["arr1 + arr2"], " is the same operation: Plus on an array object is ArrayAdd, so arrays may be combined in ordinary arithmetic notation."}],
   note["Arrays of different dimensions give ArrayAdd::dims and $Failed."]},
  {exText["Entry by entry:"],
   input["u = {{1, 2}, {3, 4}}; v = {{5, 6}, {7, 8}};\nNormal @ ArrayAdd[u, v]"],
   exText["Ordinary arithmetic notation does the same:"],
   input["Normal[ArrayObject[u] + ArrayObject[v]]"],
   exText["A difference is a sum with a scalar multiple, so that works too:"],
   input["Normal[ArrayObject[u] - ArrayObject[v]]"],
   exText["An array of order zero has no shape of its own and reaches every entry:"],
   input["Normal @ ArrayAdd[1, u]"],
   exText["A symmetry shared by the arguments is carried over:"],
   input["ArraySymmetry @ ArrayAdd[ArrayObject[{{1, 2}, {2, 3}}, \"Symmetric\"],\n                         ArrayObject[{{0, 1}, {1, 0}}, \"Symmetric\"]]"],
   exText["Arrays of different dimensions cannot be added:"],
   input["Quiet @ ArrayAdd[{1, 2}, {1, 2, 3}]"]},
  source[$aa, "Element-wise operations"],
  {"ArrayTimes", "ArrayMultiply", "ArrayObject", "GenerateSymbolicArray", "ZeroArray"}
];

page["ArrayTimes",
  {usage["ArrayTimes[arr1, arr2, ...]", "multiplies the given arrays entry by entry."]},
  {note["The arrays are given directly, not in a list, as with ArrayAdd."],
   note["Every argument must have the same dimensions, except that an array of order zero multiplies every entry. Scalar multiplication is therefore the same operation, with no separate name: ArrayTimes[2, arr] and ArrayTimes with the 0-array ArrayObject[2] are one and the same."],
   note["Entries of any kind may be combined, and the result is an ArrayObject, exactly as for ArrayAdd."],
   noteC[{inline["arr1 arr2"], " is the same operation, as is ", inline["2 arr"], " and ", inline["arr / 2"], "."}],
   note["This is multiplication entry by entry, which is not a contraction. For the matrix product, and for Einstein summation generally, see ArrayMultiply."]},
  {exText["Entry by entry:"],
   input["u = {{1, 2}, {3, 4}}; v = {{5, 6}, {7, 8}};\nNormal @ ArrayTimes[u, v]"],
   exText["A scalar multiplies every entry, and it is the same operation:"],
   input["{Normal @ ArrayTimes[2, u], Normal @ ArrayTimes[ArrayObject[2], u]}"],
   exText["In ordinary notation:"],
   input["Normal[2 ArrayObject[u]]"],
   exText["Entries of different kinds may be combined:"],
   input["2 GenerateSymbolicArray[c, {2}]"],
   exText["This is not the matrix product, which contracts:"],
   input["{Normal @ ArrayTimes[u, v], Normal @ ArrayMultiply[{u, v}, \"ij,jk->ik\"]}"]},
  source[$aa, "Element-wise operations"],
  {"ArrayAdd", "ArrayMultiply", "ArrayObject", "GenerateSymbolicArray", "OneArray"}
];

page["FindArrayEquations",
  {usage["FindArrayEquations[ops, args]", "searches for equational identities among all the ways the operations ops can be nested over the arguments args, and returns them as a list of equations."]},
  {note["Each operation is given as op -> arity, or as op alone when its arity can be read off the first of its definitions. Operations of different arities may be mixed."],
   note["The arguments are symbols standing for generic arrays, and may be repeated. Only those nestings that use exactly the given arguments are considered, so the number of arguments must fit the arities; with a single ternary operation, for instance, it must be odd."],
   noteC[{"With ", inline["\"Permute\" -> True"], ", the default, the right-hand side of a candidate equation ranges over every permutation of the arguments as well as every nesting. With ", inline["\"Permute\" -> False"], " the arguments keep the order given, and only the nesting varies."}],
   noteC[{inline["\"IncludeArguments\" -> True"], ", the default, also considers equations whose left-hand side is a bare argument, which is how an identity such as an absorbing element is found."}],
   note["The search is in two stages. Every term is first evaluated on random integer arrays, which is fast, and only pairs that agree there are carried forward. Terms equal as identities agree on any arrays, so nothing correct is ever discarded; what survives is verified on symbolic arrays by expanding the difference of the two sides."],
   noteC[{"The test arrays have order ", inline["\"Order\""], " and every index of length ", inline["\"Dimension\""], ". The order is ", inline["Automatic"], " by default, meaning the smallest order from 1 to 5 on which every operation actually evaluates, which is what makes a set of matrix operations and a set of 3-array operations both work without being told. The dimension is 2 by default."}],
   noteC[{"An identity is verified on symbolic arrays of that dimension, so it is proved at that dimension rather than at every dimension; raising ", inline["\"Dimension\""], " checks it more widely. ", inline["\"Trials\""], " sets of random arrays are used for the numerical screen."}],
   note["A nesting that the operations cannot evaluate is dropped rather than compared, which is what lets an operation returning a scalar, such as a trace, sit at the top of a term without its appearing anywhere it cannot. If no nesting evaluates at all, FindArrayEquations::eval is produced and $Failed returned, so that being unable to test anything is never reported as having found no equations."],
   noteC[{"An operation of arity one is the exception to the rule that a term uses exactly the given arguments: it takes a term to a term of the same shape, so it may be applied over and over at any point without consuming one. Every term is offered wrapped in up to ", inline["\"UnaryDepth\""], " nested unary operations, two by default, which is what brings a law such as the involutivity of a transpose within reach. Setting it to 0 leaves the unary operations out of the terms altogether."}],
   noteC[{"With ", inline["\"Reduce\" -> True"], ", the default, an equation that follows from the others is not reported. Each equation is read as a schema, its arguments standing for arbitrary terms, and is turned into a rewrite rule that fires only when it makes a term strictly smaller; equations whose two sides then rewrite to a common form are consequences of the ones already kept, and are dropped. Without this a single involution would be reported over again for every term it can be buried in. ", inline["\"Reduce\" -> False"], " gives every equation found."}],
   note["What survives the reduction is a basis, not a canonical set: which of several equivalent forms of a law is kept depends on the order the equations are considered in, and the reduction is by rewriting alone, so a consequence that rewriting cannot reach is still reported."],
   note["The operations must be defined so that they do not evaluate on bare symbols, which is what definitions with _List or array patterns achieve. Otherwise the terms collapse before they can be compared."]},
  {exText["Ordinary matrix multiplication on three generic arguments finds associativity, and nothing else:"],
   input["dot[u_List, w_List] := Normal @ ArrayMultiply[{u, w}, \"ij,jk->ik\"];\n\
FindArrayEquations[{dot}, {a1, a2, a3}, \"Order\" -> 2, \"Dimension\" -> 3]"],
   exText["Keeping the arguments in the order given makes no difference here, since associativity does not permute them:"],
   input["FindArrayEquations[{dot}, {a1, a2, a3}, \"Order\" -> 2, \"Dimension\" -> 3, \"Permute\" -> False]"],
   exText["The Fish product of three 3-arrays satisfies a twisted form of ternary associativity:"],
   input["Fish[u_List, w_List, t_List] := Normal @ ArrayMultiply[{u, w, t}, \"ijp,qrp,qrk->ijk\"];\n\
FindArrayEquations[{Fish}, {a1, a2, a3, a4, a5}]"],
   exText["The Bhattacharya-Mesner product, by contrast, satisfies no equation at all on five arguments:"],
   input["BM[u_List, w_List, t_List] := Normal @ ArrayMultiply[{u, w, t}, \"ijp,ipk,pjk->ijk\"];\n\
FindArrayEquations[{BM}, {a1, a2, a3, a4, a5}]"],
   exText["An arity that cannot be read off a definition must be given:"],
   input["FindArrayEquations[{dot -> 2}, {a1, a2, a3}, \"Order\" -> 2, \"Dimension\" -> 3]"],
   exText["Several operations may be searched together. A transpose and a matrix product give the law that transposing a product reverses it, and the law that transposing twice does nothing:"],
   input["perp[u_List] := Normal @ ArrayMultiply[{u}, \"ij->ji\"];\nFindArrayEquations[{perp, dot}, {a1, a2}]"],
   exText["Holding the arguments in the order given leaves only the involutivity, since the other law moves them:"],
   input["FindArrayEquations[{perp, dot}, {a1, a2}, \"Permute\" -> False]"],
   exText["Every consequence of those two laws holds as well, and without the reduction each is reported in its own right:"],
   input["Length @ FindArrayEquations[{perp, dot}, {a1, a2}, \"Reduce\" -> False]"],
   exText["The involutivity needs the transpose twice over, so it is out of reach when the unary operations may not be nested \[LongDash] and without it the remaining law is reported in each of its forms, there being nothing left to rewrite one into another:"],
   input["FindArrayEquations[{perp, dot}, {a1, a2}, \"UnaryDepth\" -> 1]"],
   exText["A trace and a matrix product give the cyclic property of the trace. The trace returns a scalar, so the nestings that would feed it back into a product simply drop out:"],
   input["trace[u_List] := Normal @ ArrayMultiply[{u}, \"pp->\"];\nFindArrayEquations[{trace, dot}, {a1, a2}]"],
   exText["Each of those inferred an order of 2 from the operations themselves. Forcing a shape they cannot take reports as much, rather than returning no equations:"],
   input["Quiet @ FindArrayEquations[{perp, dot}, {a1, a2}, \"Order\" -> 4]"]},
  source[$aa, "FindArrayEquations"],
  {"ArrayMultiply", "GenerateSymbolicArray", "ArrayObject", "IdentityArray"}
];

page["ZeroArray",
  {usage["ZeroArray[n, d]", "gives the n-array with every index of length d and every entry zero."],
   usage["ZeroArray[dims]", "gives the array of the given dimensions."],
   usage["ZeroArray[spec, dom]", "gives its entries in the value domain dom."]},
  {note["The domain defaults to \"Integer\". The zero of \"Boolean\" is False, and the zero of FiniteField[p, n] is that field's zero element."],
   note["A constant array is invariant under every permutation of its indices, so a regular one is declared \"Symmetric\". An array of unequal index lengths cannot carry a symmetry and is declared \"Rigid\"."]},
  {input["Normal @ ZeroArray[2, 3]"],
   exText["Any shape, and any domain:"],
   input["{ArrayDimensions @ ZeroArray[{2, 3, 4}], Normal @ ZeroArray[1, 3, \"Real\"]}"],
   exText["A regular constant array is symmetric:"],
   input["{ArraySymmetry @ ZeroArray[3, 2], ArraySymmetry @ ZeroArray[{2, 3}]}"]},
  source[$aa, "Special arrays"],
  {"OneArray", "IdentityArray", "PartialIdentityArray", "ArrayObject"}
];

page["OneArray",
  {usage["OneArray[n, d]", "gives the n-array with every index of length d and every entry one."],
   usage["OneArray[dims]", "gives the array of the given dimensions."],
   usage["OneArray[spec, dom]", "gives its entries in the value domain dom."]},
  {note["The domain defaults to \"Integer\". The one of \"Boolean\" is True, and the one of FiniteField[p, n] is that field's unit."],
   note["Like any constant array, a regular one is declared \"Symmetric\"."],
   note["This is the array of all ones, which is not the identity for any contraction; for that see IdentityArray."]},
  {input["Normal @ OneArray[2, 3]"],
   input["Normal @ OneArray[3, 2, FiniteField[2, 1]]"]},
  source[$aa, "Special arrays"],
  {"ZeroArray", "IdentityArray", "PartialIdentityArray", "ArrayObject"}
];

page["IdentityArray",
  {usage["IdentityArray[n, d]", "gives the n-array with every index of length d whose entry is one where all n indices agree and zero elsewhere."],
   usage["IdentityArray[n, d, dom]", "gives its entries in the value domain dom."]},
  {note["For n equal to 2 this is the ordinary identity matrix of size d."],
   note["It is the generalized Kronecker delta on all n indices, and is invariant under every permutation of them, so it is declared \"Symmetric\"."],
   note["IdentityArray[n, d] is PartialIdentityArray[n, d, Range[n]]."]},
  {exText["At order two it is the identity matrix:"],
   input["Normal @ IdentityArray[2, 3]"],
   exText["At order three it is one only on the main diagonal:"],
   input["Normal @ IdentityArray[3, 2]"],
   exText["It acts as an identity for the matrix product:"],
   input["a = {{1, 2}, {3, 4}};\nNormal @ ArrayMultiply[{Normal @ IdentityArray[2, 2], a}, \"ij,jk->ik\"]"],
   exText["Over a finite field:"],
   input["Normal @ IdentityArray[2, 2, FiniteField[2, 1]]"]},
  source[$aa, "Special arrays"],
  {"PartialIdentityArray", "ZeroArray", "OneArray", "ArrayMultiply"}
];

page["PartialIdentityArray",
  {usage["PartialIdentityArray[n, d, positions]", "gives the n-array with every index of length d whose entry is one where the indices at the given positions agree and zero elsewhere."],
   usage["PartialIdentityArray[n, d, positions, dom]", "gives its entries in the value domain dom."]},
  {note["The positions are a list of distinct index positions between 1 and n."],
   note["With every position named this is IdentityArray[n, d], which is symmetric. With only some of them the array is generally not invariant under permuting all its indices, so it is declared \"Rigid\"."],
   note["These are the arrays that act as partial units for the ternary products: each names the pair of indices it identifies."]},
  {exText["A 3-array that is one where the first two indices agree, whatever the third:"],
   input["Normal @ PartialIdentityArray[3, 2, {1, 2}]"],
   exText["The three partial identities of a 3-array, one for each pair of indices:"],
   input["Normal @ PartialIdentityArray[3, 2, #] & /@ {{1, 2}, {2, 3}, {1, 3}}"],
   exText["Naming every position gives the full identity:"],
   input["PartialIdentityArray[3, 2, {1, 2, 3}] === IdentityArray[3, 2]"],
   exText["Only a full identity is symmetric:"],
   input["{ArraySymmetry @ PartialIdentityArray[3, 2, {1, 2}],\n ArraySymmetry @ PartialIdentityArray[3, 2, {1, 2, 3}]}"]},
  source[$aa, "Special arrays"],
  {"IdentityArray", "ZeroArray", "OneArray", "ArrayMultiply", "ArraySymmetry"}
];

(* ================= Drawing arrays ================= *)

$hmp = "src/ArrayGraphics.wl";

(* The contraction pages above bind a and b as example matrices. Release them so that the
   symbolic arrays below are built from free symbols again. *)
ClearAll[a, b, c, x, y, z];

page["ArrayGraphics",
  {usage["ArrayGraphics[arr]", "draws the array arr in the plane."],
   usage["ArrayGraphics[arr, opts]", "draws it with the given options."]},
  {note["An array of order one is drawn as a row of cells and one of order two as a grid; an array of order zero is a single cell."],
   note["Only those orders have a flat picture of their own. Anything higher is drawn as a nesting of them: the first block of indices gives the outer picture, and each of its cells holds the picture of the indices that remain. A 3-array is a row of grids, a 4-array a grid of grids."],
   noteC[{"The result is always a two-dimensional ", inline["Graphics"], ". For cubes arranged in space, at every order, see ", inline["ArrayGraphics3D"], "."}],
   noteC[{inline["\"Nesting\" -> {k1, k2, ...}"], " says how to split the indices: the blocks must sum to the order of the array and each must be one or two, since a flat picture holds no more. The default, ", inline["Automatic"], ", uses ", inline["ArrayNesting"], "."}],
   note["The cells of the innermost block are sized to the widest entry, so that the entries of a symbolic array do not overflow one another."],
   noteC[{inline["\"Framed\" -> False"], " leaves the entries on the background with no cell drawn around them."}],
   noteC[{inline["\"Indices\""], " is ", inline["Automatic"], ", which labels the rows and columns; ", inline["None"], ", which labels nothing; or ", inline["\"Colored\""], ", which gives each axis a colour of its own."}],
   noteC[{inline["\"PlotTheme\""], " is ", inline["\"Light\""], " or ", inline["\"Dark\""], "."}],
   noteC[{"An array of more than ", inline["\"MaxEntries\""], " entries, 512 by default, is refused with ", inline["ArrayGraphics::big"], " rather than drawn illegibly."}],
   note["Array objects and plain nested lists are both accepted, and Graphics options are passed through."]},
  {exText["A 1-array is a row of cells, and a 2-array a grid:"],
   input["{ArrayGraphics[{1, 2, 3, 4}], ArrayGraphics[{{1, 2, 3}, {4, 5, 6}}]}"],
   exText["A 3-array has no flat picture of its own, so it is drawn as a row of matrices, the first index choosing the matrix:"],
   input["ArrayGraphics[Array[#1 + #2 + #3 &, {2, 2, 2}]]"],
   exText["A 4-array becomes a grid of grids:"],
   input["ArrayGraphics[Array[#1 + #2 + #3 + #4 &, {2, 2, 2, 2}]]"],
   exText["Which indices go outside and which inside is a choice. The same 3-array as a matrix of rows instead:"],
   input["ArrayGraphics[Array[#1 + #2 + #3 &, {2, 2, 2}], \"Nesting\" -> {2, 1}]"],
   exText["The entries of a symbolic array are wider, and the cells widen to match:"],
   input["ArrayGraphics[GenerateSymbolicArray[b, {2, 3}]]"],
   exText["A dark theme, and cells left undrawn:"],
   input["{ArrayGraphics[{{1, 2}, {3, 4}}, \"PlotTheme\" -> \"Dark\"],\n ArrayGraphics[{{1, 2}, {3, 4}}, \"Framed\" -> False]}"],
   exText["A block of three indices has no flat picture, and is refused:"],
   input["Quiet @ ArrayGraphics[Array[1 &, {2, 2, 2}], \"Nesting\" -> {3}]"]},
  source[$hmp, "The flat family"],
  {"ArrayGraphics3D", "ArrayNesting", "HypermatrixGraphics", "ArrayObject", "GenerateSymbolicArray"}
];

page["ArrayGraphics3D",
  {usage["ArrayGraphics3D[arr]", "draws the array arr as cubes arranged in space."],
   usage["ArrayGraphics3D[arr, opts]", "draws it with the given options."]},
  {note["An array of order one is drawn as a line of cubes, one of order two as a plane of them, and one of order three as a lattice. The entry at index tuple (i, j, k) sits at the point {i, j, k}, an index the array does not have standing at one."],
   note["An array of higher order is drawn by tiling: the inner block of indices is drawn as a lattice, and the outer block repeats that lattice through space, shifted by its own extent and a gap. Everything ends up in one picture, since space has no room for pictures inset into pictures."],
   noteC[{"The result is always a three-dimensional ", inline["Graphics3D"], ". For plane pictures at every order see ", inline["ArrayGraphics"], "."}],
   noteC[{inline["\"Nesting\" -> {k1, k2, ...}"], " says how to split the indices: the blocks must sum to the order of the array and each must be one, two or three, space holding no more at a time. The default uses ", inline["ArrayNesting[n, 3]"], "."}],
   note["The cubes have translucent faces and no outline, so that the entries behind them can still be read; an outline on every cube of a lattice reads as a mesh rather than as a set of cells."],
   noteC[{"A gnomon in the near corner marks as many axes as the picture uses; ", inline["\"Gnomon\" -> False"], " drops it. ", inline["\"Indices\" -> \"Colored\""], " gives each axis a colour of its own."}],
   note["The reference implementation this package grew out of drew only the order-three case, and called such a picture a cubix; the name is not kept, but the look of it is."],
   note["The other options are shared with ArrayGraphics."]},
  {exText["At order one a line of cubes, at order two a plane of them:"],
   input["{ArrayGraphics3D[{1, 2, 3, 4}], ArrayGraphics3D[{{1, 2, 3}, {4, 5, 6}}]}"],
   exText["At order three, the full lattice:"],
   input["ArrayGraphics3D[Array[#1 + #2 + #3 &, {3, 3, 3}]]"],
   exText["Above that the lattices are tiled through space. A 4-array is a line of them:"],
   input["ArrayGraphics3D[Array[#1 + #2 + #3 + #4 &, {2, 2, 2, 2}]]"],
   exText["\[Ellipsis] and a 5-array a plane of them:"],
   input["ArrayGraphics3D[Array[Mod[#1 + #2 + #3 + #4 + #5, 7] &, {2, 2, 2, 2, 2}]]"],
   exText["Colouring the axes says which index runs which way:"],
   input["ArrayGraphics3D[Array[#1 #2 #3 &, {2, 2, 2}], \"Indices\" -> \"Colored\"]"],
   exText["A block of four indices has nowhere to go, and is refused:"],
   input["Quiet @ ArrayGraphics3D[Array[1 &, {2, 2, 2, 2}], \"Nesting\" -> {4}]"]},
  source[$hmp, "The spatial family"],
  {"ArrayGraphics", "ArrayNesting", "HypermatrixGraphics3D", "ArrayObject"}
];

page["ArrayNesting",
  {usage["ArrayNesting[n]", "gives the way ArrayGraphics splits the indices of an array of order n by default: blocks of at most two."],
   usage["ArrayNesting[n, b]", "allows blocks of up to b indices."],
   usage["ArrayNesting[arr]", "gives the splitting for the order of arr."]},
  {note["The result is a list of block sizes summing to n: the first block is the outer picture, and each block after it is drawn inside the cells of the one before, or tiled by it."],
   noteC[{"A flat picture holds one or two indices and a spatial one holds three, so the two families differ only in the largest block: ", inline["ArrayGraphics"], " uses ", inline["ArrayNesting[n]"], " and ", inline["ArrayGraphics3D"], " uses ", inline["ArrayNesting[n, 3]"], "."}],
   note["The blocks are of the largest size allowed, with any remainder taken off the front. The remainder goes first so that a 3-array drawn flat comes out as a row of matrices, the first index choosing the matrix, which is the familiar reading."]},
  {exText["The flat splitting, in blocks of at most two:"],
   input["Table[ArrayNesting[n], {n, 0, 8}]"],
   exText["The spatial one, in blocks of at most three:"],
   input["Table[ArrayNesting[n, 3], {n, 0, 8}]"],
   exText["Taken from an array, it reads the order off it:"],
   input["{ArrayNesting @ GenerateSymbolicArray[b, {2, 2, 2, 2}],\n ArrayNesting[GenerateSymbolicArray[b, {2, 2, 2, 2}], 3]}"]},
  source[$hmp, "Nesting"],
  {"ArrayGraphics", "ArrayGraphics3D", "ArrayOrder"}
];

page["HypermatrixGraphics",
  {usage["HypermatrixGraphics[hm]", "draws each array of the hypermatrix hm in the plane, in canonical order."]},
  {note["Each array is drawn by ArrayGraphics, and the options are the same."],
   note["The arrays appear in the hypermatrix's own order: lowest order first, then the shorter dimensions, then symmetry."],
   noteC[{"For cubes arranged in space, see ", inline["HypermatrixGraphics3D"], "."}]},
  {input["HypermatrixGraphics @ Hypermatrix[{{1, 2}, {{1, 2}, {3, 4}}}]"],
   exText["Arrays of several orders at once:"],
   input["HypermatrixGraphics @ Hypermatrix[{{1, 2, 3}, {{1, 2}, {3, 4}}, Array[1 &, {2, 2, 2}]}]"]},
  source[$hmp, "Public entry points"],
  {"HypermatrixGraphics3D", "ArrayGraphics", "Hypermatrix", "HypermatrixArrays"}
];

page["HypermatrixGraphics3D",
  {usage["HypermatrixGraphics3D[hm]", "draws each array of the hypermatrix hm as cubes in space, in canonical order."]},
  {note["Each array is drawn by ArrayGraphics3D, and the options are the same."],
   noteC[{"For plane pictures, see ", inline["HypermatrixGraphics"], "."}]},
  {input["HypermatrixGraphics3D @ Hypermatrix[{{1, 2, 3}, {{1, 2}, {3, 4}}, Array[1 &, {2, 2, 2}]}]"]},
  source[$hmp, "Public entry points"],
  {"HypermatrixGraphics", "ArrayGraphics3D", "Hypermatrix", "HypermatrixArrays"}
];

(* ================= Adjacency ================= *)

$adj = "src/Adjacency.wl";

page["AdjacencyHypermatrix",
  {usage["AdjacencyHypermatrix[hg]", "gives the adjacency hypermatrix of the hypergraph hg: one array for each arity and symmetry type of edge it has, held in a Hypermatrix."]},
  {note["An edge of arity k contributes to an array of order k, whose indices run over the vertices in the order VertexList[hg] gives them. The entry at an index tuple is the number of edges of that group sitting on it, so a repeated edge counts twice, the hypergraph being a multiset."],
   note["The symmetry types of the two halves of the package correspond one for one, and this is where that correspondence is written down: a \"Directed\" edge gives a \"Rigid\" array, a \"Cyclic\" edge a \"Cyclic\" array, and an \"Unordered\" edge a \"Symmetric\" one."],
   note["An array carries one symmetry and one order, so the edges are gathered by arity and symmetry type together and each group becomes one array. A hypergraph with edges of the same arity but different symmetry types therefore gives several arrays of the same dimensions, one per type."],
   note["The entry is looked up against the canonical index of its tuple, the least one in its orbit under the symmetry. Every tuple of an orbit therefore reads the same count, which is what makes the array carry its declared symmetry by construction rather than by luck."],
   note["Identical copies of an edge share one entry, so the count there is an integer multiple. That holds at every symmetry type: two copies of a cyclic edge give 2 at each of its three rotations, three copies of an unordered one give 3 at each of its six permutations."],
   noteC[{"By default the entries count edges and labels play no part, so a labelled hypergraph gives the same hypermatrix as ", inline["HypergraphSkeleton"], " of it. With ", inline["\"Labeled\" -> True"], " the entries are the edge labels themselves, and 0 where there is no edge; an edge carrying no label of its own gives ", inline["None"], ", which absence does not."}],
   noteC[{"Labels as entries need the hypergraph to be simple, since one entry cannot hold two labels, and ", inline["AdjacencyHypermatrix::nonsimple"], " is given if it is not. Counting has no such trouble: two edges in one place is a 2. See ", inline["SimpleHypergraphQ"], "."}],
   note["A label fills the whole orbit of its edge, just as a count does, so the array carries its declared symmetry either way. The value domain of a labelled array is \"Label\", which is never inferred and has to be declared, so that an array of labels is always one on purpose."],
   note["A hypergraph with no edges gives an empty hypermatrix. An edge of arity zero gives an array of order zero, whose single entry is the number of such edges."],
   note["The vertices of a hypergraph are a set and have no order of their own, so the numbering of the indices is a choice. It is made by the Hypergraph constructor, which stores its vertices sorted, and not by AdjacencyHypermatrix: giving the same edges in a different order therefore changes nothing."],
   note["What that buys is determinism, not invariance. The sort is on the vertex identifiers, so renaming the vertices can permute the indices, and two isomorphic hypergraphs may well give different hypermatrices \[LongDash] differing by the simultaneous permutation of all their indices that the renaming induces. As an invariant of the isomorphism class an adjacency array is defined only up to such a permutation, which is the usual state of affairs rather than a defect."],
   noteC[{"For an invariant, compose with ", inline["CanonicalHypergraph"], ", which relabels the vertices one to n by isomorphism class: ", inline["AdjacencyHypermatrix[CanonicalHypergraph[hg]]"], " depends on the class alone. It is left as a composition rather than given a name of its own, so that it is always clear which of the two is meant."}],
   note["Nothing is lost: the canonical entries of the arrays name exactly the edges that went in, with their multiplicities, so the hypermatrix determines the edge multiset of the hypergraph."]},
  {exText["A hypergraph with edges of two arities and two symmetry types:"],
   input["h = Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"], Edge[{3, 4}, \"Directed\"],\n                Edge[{1, 2}, \"Directed\"]}]"],
   exText["Its adjacency hypermatrix has one array per group:"],
   input["hm = AdjacencyHypermatrix[h];\n{hm[\"Orders\"], hm[\"Dimensions\"], hm[\"Symmetries\"]}"],
   exText["The directed edges land where they are, in a rigid matrix:"],
   input["MatrixForm @ Normal @ First @ HypermatrixArrays[hm]"],
   exText["The unordered edge fills the whole orbit of its vertices, which is what makes that array symmetric:"],
   input["Position[Normal @ Last @ HypermatrixArrays[hm], 1]"],
   exText["A cyclic edge fills the rotations only, and a directed one the single tuple:"],
   input["Position[Normal @ First @ HypermatrixArrays @\n   AdjacencyHypermatrix @ Hypergraph[{Edge[{1, 2, 3}, \"Cyclic\"]}], 1]"],
   exText["The same arity in three symmetry types gives three arrays:"],
   input["AdjacencyHypermatrix[Hypergraph[{Edge[{1, 2}, \"Directed\"], Edge[{1, 2}, \"Unordered\"],\n                                  Edge[{1, 2}, \"Cyclic\"]}]][\"Symmetries\"]"],
   exText["A repeated edge counts twice:"],
   input["Normal @ First @ HypermatrixArrays @\n   AdjacencyHypermatrix @ Hypergraph[{Edge[{1, 2}], Edge[{1, 2}], Edge[{2, 1}]}]"],
   exText["Identical copies of an edge share an entry, and the count there is the multiple:"],
   input["Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
 Hypergraph[{Edge[{1, 2}], Edge[{1, 2}], Edge[{2, 1}]}]"],
   exText["With \"Labeled\" -> True the entries are the labels instead, and 0 where there is no edge:"],
   input["hLab = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];
Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[hLab, \"Labeled\" -> True]"],
   exText["That needs the hypergraph to be simple. A repeated edge would want one entry to hold two labels, and is refused:"],
   input["Quiet @ AdjacencyHypermatrix[
   Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{1, 2}, \"Directed\", \"b\"]}],
   \"Labeled\" -> True]"],
   exText["The vertices are a set, so the numbering of the indices comes from the hypergraph's own sorted vertex list. Two isomorphic hypergraphs whose vertices are named differently need not agree:"],
   input["hNum = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}];
hStr = Hypergraph[{Edge[{\"z\", \"a\"}], Edge[{\"a\", \"m\"}]}];
{IsomorphicHypergraphQ[hNum, hStr], AdjacencyHypermatrix[hNum] === AdjacencyHypermatrix[hStr]}"],
   exText["\[Ellipsis] but they agree once the vertices are relabelled by isomorphism class:"],
   input["AdjacencyHypermatrix[CanonicalHypergraph[hNum]] ===
 AdjacencyHypermatrix[CanonicalHypergraph[hStr]]"],
   exText["And the arrays draw like any others:"],
   input["HypermatrixGraphics @ AdjacencyHypermatrix @\n   Hypergraph[{Edge[{1, 2}, \"Directed\"], Edge[{2, 3}, \"Directed\"]}]"]},
  source[$adj, "AdjacencyHypermatrix"],
  {"Hypergraph", "Hypermatrix", "EdgeSymmetry", "ArraySymmetry", "HypergraphSkeleton", "HypermatrixGraphics"}
];

(* ================= Rewriting ================= *)

$rw = "src/Rewriting.wl";

(* Examples run in this same kernel, so a variable an example assigns to is a variable this file
   loses. An example once assigned to `path`, the helper just above, and the source cell of the
   two pages below quietly became a Missing -- which Export cannot write, so the pages vanished
   with no error. Example variables are named to miss the helpers; this clears the ones the
   earlier sections bound. *)
ClearAll[a, b, c, x, y, z, h, m, sub, chain, events, del, hl];

page["HypergraphRule",
  {usage["HypergraphRule[input, output]", "represents the rewrite rule taking the hypergraph input to the hypergraph output."]},
  {note["Either side may be given as anything Hypergraph accepts, and is converted; a side that is not a hypergraph gives HypergraphRule::hg."],
   noteC[{inline["rule[hg]"], " applies the rule, giving one event for every way the input matches. See ", inline["HypergraphRuleApply"], "."}],
   note["A vertex of the input that the output keeps is carried through the rewrite; one the output introduces is created fresh; one the output drops is destroyed. \"SharedVertices\", \"NewVertices\" and \"DeletedVertices\" report the three groups, read off the two sides by vertex identifier."],
   note["Vertices correspond between the two sides by identifier, so the same name on both sides means the same vertex, and a name on the output alone means a new one."],
   note["The shape follows the reference paclet this package grew out of, so that rules written for that one read the same here."]},
  {exText["Subdividing an edge: the two endpoints are kept, a third vertex is new."],
   input["sub = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}]"],
   input["{sub[\"SharedVertices\"], sub[\"NewVertices\"], sub[\"DeletedVertices\"]}"],
   exText["A rule that destroys a vertex, by mentioning it on the left only:"],
   input["HypergraphRule[{{1, 2}}, {{1}}][\"DeletedVertices\"]"],
   exText["The sides may carry symmetry types and labels like any hypergraph:"],
   input["HypergraphRule[{Edge[{1, 2, 3}, \"Unordered\", \"a\"]}, {Edge[{1, 2}, \"Directed\"]}][\"Input\"]"],
   exText["Something that is not a hypergraph is refused:"],
   input["Quiet @ HypergraphRule[{{1, 2}}, \"nope\"]"]},
  source[$rw, "The rule object"],
  {"HypergraphRuleApply", "HypergraphRuleMatches", "HypergraphRuleQ", "Hypergraph", "Edge"}
];

page["HypergraphRuleQ",
  {usage["HypergraphRuleQ[expr]", "gives True if expr is a valid HypergraphRule, and False otherwise."]},
  {note["A rule is valid when both of its sides are valid Hypergraph objects."]},
  {input["{HypergraphRuleQ @ HypergraphRule[{{1, 2}}, {{2, 1}}],\n HypergraphRuleQ[{{1, 2}} -> {{2, 1}}]}"]},
  source[$rw, "The rule object"],
  {"HypergraphRule", "HypergraphQ"}
];

page["HypergraphRuleApply",
  {usage["HypergraphRuleApply[rule, hg]", "applies rule to the hypergraph hg, giving a list of events, one for every way the rule's input matches."],
   usage["rule[hg]", "is equivalent."]},
  {note["Each event is an association. \"Hypergraph\" is the result; \"MatchVertices\", \"MatchEdges\" and \"MatchEdgePositions\" say what was matched; \"NewVertices\" and \"NewEdges\" what was created; \"DeletedVertices\" what was destroyed; \"RuleVertexMap\" the correspondence from the rule's vertices to the hypergraph's; and \"Rule\" the rule itself."],
   note["The result is the hypergraph with the matched edges removed and the output's edges put in their place. A vertex the output introduces becomes a new vertex of the result, and one the output drops is destroyed."],
   note["A destroyed vertex is removed only when nothing still standing uses it. A rule rewrites what it matched, and reaching into edges it did not match to strike a vertex out of them would not be local; so a vertex the rule drops but another edge still holds simply stays, and is not reported destroyed."],
   note["New vertices are named so as to miss every vertex already present, which makes the result reproducible from one session to the next rather than depending on a counter of fresh symbols."],
   noteC[{"Labels constrain matching and absence of a label does not: a pattern edge or vertex carrying no label matches whatever it lies on, and one carrying a label matches only the same label. ", inline["\"Labeled\" -> False"], " ignores labels altogether."}],
   noteC[{"Symmetry types must agree, and the pattern is matched over the orderings its symmetry allows: one for a directed edge, its rotations for a cyclic one, every permutation for an unordered one. An unordered edge of arity k therefore matches in k! ways, which are genuinely different matches and are all reported."}],
   noteC[{inline["\"DistinctVertices\" -> True"], ", the default, keeps two vertices of the input from landing on one vertex of the hypergraph, which is what the reference paclet does. ", inline["False"], " allows it."}],
   note["No match gives an empty list, which is not a failure: it says the rule does not apply."]},
  {exText["Subdividing an edge of a two-edge path: one event for each edge."],
   input["sub = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}];\nchain = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}];\nevents = sub[chain];\nLength[events]"],
   exText["What the first event did:"],
   input["KeyTake[First[events], {\"MatchEdgePositions\", \"NewVertices\", \"RuleVertexMap\"}]"],
   exText["And what it produced:"],
   input["First[events][\"Hypergraph\"]"],
   exText["The results of both events, as edge lists:"],
   input["EdgeList[#[\"Hypergraph\"]] & /@ events"],
   exText["A rule that destroys a vertex does so only when nothing else holds it:"],
   input["del = HypergraphRule[{{1, 2}}, {{1}}];\n{VertexList @ First[del[Hypergraph[{Edge[{1, 2}]}]]][\"Hypergraph\"],\n VertexList @ First[del[Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]]][\"Hypergraph\"]}"],
   exText["A label on the pattern narrows what the rule applies to:"],
   input["hl = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];\n{Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], hl],\n Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"a\"]}, {}], hl]}"],
   exText["A rule that does not apply gives no events:"],
   input["HypergraphRule[{{1, 2}, {2, 3}}, {}][Hypergraph[{Edge[{1, 2}]}]]"]},
  source[$rw, "Applying"],
  {"HypergraphRule", "HypergraphRuleMatches", "Hypergraph", "EdgeSymmetry", "EdgeLabel"}
];

page["HypergraphRuleMatches",
  {usage["HypergraphRuleMatches[rule, hg]", "gives the ways the input of rule sits inside hg, without rewriting anything."]},
  {note["Each match is an association with \"MatchEdgePositions\", the positions in EdgeList[hg] that the input's edges lie on, and \"RuleVertexMap\", the correspondence from the input's vertices to the hypergraph's."],
   note["It takes the same options as HypergraphRuleApply, and is what that function matches with; asking for the matches alone is the way to see why a rule applies as often as it does."],
   note["An edge of the hypergraph is used by at most one edge of the input, so a rule whose input has two edges wants two of them."],
   note["A vertex of the input lying in no edge constrains nothing, and is matched to each vertex not already taken in turn."]},
  {input["HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}],\n   Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]]"],
   exText["An unordered edge matches in as many ways as its vertices can be ordered:"],
   input["Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2, 3}, \"Unordered\"]}, {}],\n   Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"]}]]"],
   exText["\[Ellipsis] a cyclic one in as many ways as it can be rotated, and a directed one in one way:"],
   input["{Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2, 3}, \"Cyclic\"]}, {}],\n    Hypergraph[{Edge[{1, 2, 3}, \"Cyclic\"]}]],\n Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2, 3}, \"Directed\"]}, {}],\n    Hypergraph[{Edge[{1, 2, 3}, \"Directed\"]}]]}"],
   exText["Two vertices of the input will not land on one, unless that is allowed:"],
   input["{HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], Hypergraph[{Edge[{1, 1}]}]],\n Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], Hypergraph[{Edge[{1, 1}]}],\n    \"DistinctVertices\" -> False]}"]},
  source[$rw, "Matching"],
  {"HypergraphRuleApply", "HypergraphRule", "EdgeList"}
];

(* ================= Guide ================= *)

(* The Guide* styles are defined in Reference.nb but only resolve inside the Documentation
   Center's own screen environment; outside it they fall back to plain body text. The guide
   therefore uses the ordinary Default.nb styles, which render properly anywhere. *)

gTitle[s_]   := Cell[s, "Title"];
gSub[s_]     := Cell[s, "Subtitle"];
gSection[s_] := Cell[s, "Section"];
gText[s_]    := Cell[tidy[s], "Text"];
gTextC[p_List] := Cell[TextData[Replace[p, s_String :> tidy[s], {1}]], "Text"];

(* one line per entry, so long descriptions wrap at the window width instead of forcing a
   grid several thousand pixels wide *)
entry[name_String, desc_String] := Cell[TextData[{inline[name], " \[LongDash] ", tidy[desc]}], "Text"];

(* same shape, but for file paths, which must not be parsed as code *)
pathEntry[p_String, desc_String] := Cell[TextData[{path[tidy[p]], " \[LongDash] ", tidy[desc]}], "Text"];

guide = Notebook[
    Flatten[{
        gTitle["Hypergraphs"],
        gSub["WolframInstitute`Hypergraphs` \[LongDash] guide to the implemented functions"],

        gText["Hypergraphs as multisets of hyperedges, with independently labelled vertices and \
edges, a symmetry type on every edge, a default graphical display, and isomorphism testing that \
takes labels into account up to renaming."],

        gSection["Loading the package"],

        gTextC[{"The whole package is loaded by a single ", inline["Get"], " of the core file, \
which pulls in the plotting code itself:"}],

        Cell["Get[FileNameJoin[{\"<project>\", \"src\", \"Hypergraph.wl\"}]]", "Input"],

        gTextC[{"Everything lives in the context ", inline["WolframInstitute`Hypergraphs`"],
            ", deliberately distinct from the upstream paclet ",
            inline["WolframInstitute`Hypergraph`"],
            " (singular) so that both can be loaded into one kernel."}],

        gSection["Where the code lives"],

        gText["The package proper is the files in src/. Everything else is documentation, \
tests, or earlier research material that predates the rewrite and is kept under reference/ for \
consultation."],

        pathEntry["src/Hypergraph.wl", "Vertex, Edge and Hypergraph, their accessors and predicates, \
HypergraphSkeleton, and the canonical form and isomorphism functions. Loads the other four files \
at its end, so this is the only one that has to be read in."],
        pathEntry["src/HypergraphPlot.wl", "HypergraphPlot and HypergraphEmbedding, the summary-box \
thresholds, and the MakeBoxes definition that makes a Hypergraph draw itself."],
        pathEntry["src/Hypermatrix.wl", "ArrayObject, GenerateSymbolicArray and Hypermatrix, their \
accessors and predicates."],
        pathEntry["src/ArrayAlgebra.wl", "The special arrays, the contraction ArrayMultiply, the \
entry-by-entry ArrayAdd and ArrayTimes, and FindArrayEquations."],
        pathEntry["src/Rewriting.wl", "HypergraphRule and its application: the matcher, and the \nevents a rewrite produces."],
        pathEntry["src/Adjacency.wl", "AdjacencyHypermatrix: the correspondence between edge \nsymmetry types and array symmetries, and the passage from one to the other."],
        pathEntry["src/ArrayGraphics.wl", "The two families of pictures: ArrayGraphics and \
HypermatrixGraphics in the plane, ArrayGraphics3D and HypermatrixGraphics3D in space, with \
ArrayNesting deciding how the indices are split between them."],
        pathEntry["tests/BasicExamples.wls", "Every hypergraph example in the documentation as an \
assertion, plus several thousand randomized checks. Run it with wolframscript -file."],
        pathEntry["tests/HypermatrixExamples.wls", "The same for the array and hypermatrix objects."],
        pathEntry["tests/ArrayAlgebraExamples.wls", "The same for the special arrays, ArrayMultiply \
and FindArrayEquations."],
        pathEntry["notebooks/HypergraphBasics.nb", "Worked examples with the reasoning behind each \
design choice."],
        pathEntry["notebooks/HypermatrixBasics.nb", "The same for arrays and hypermatrices."],
        pathEntry["Documentation/English/ReferencePages/Symbols/", "One reference page per exported \
symbol."],
        pathEntry["Documentation/English/Guides/Hypergraphs.nb", "This guide."],
        pathEntry["reference/Hypergraph/, reference/Multicomputation/", "Read-only clones of the two \
upstream paclets, WolframInstitute/Hypergraph and Wolfram/Multicomputation, kept for comparison."],
        pathEntry["reference/PlexAlgebra.wl, reference/Chemoids.wl, reference/HypergraphAlgebras.wl, reference/HypergraphMinicourse.wl",
            "Earlier research notebooks exported as packages, from before the rewrite. They hold \
the prototypes this package is growing out of \[LongDash] hypermatrix contraction and plex \
diagrams in PlexAlgebra.wl in particular \[LongDash] together with many worked examples of what \
the two upstream paclets could do in combination. Mostly commented out, and not part of the \
package."],
        pathEntry["notebooks/legacy/", "The notebooks those exports came from."],

        gText["A reference page names the file and the section heading its function is defined \
under, rather than a line number, since line numbers go stale on the next edit."],

        gSection["Vertices and edges"],
        entry["Vertex", "a vertex, identified by any expression and optionally labelled"],
        entry["VertexID", "the identifier of a vertex"],
        entry["VertexLabel", "the label of a vertex, or None"],
        entry["VertexObjectQ", "test for a valid Vertex object"],
        entry["Edge", "a hyperedge, with a symmetry type and an optional label"],
        entry["EdgeVertices", "the vertices of a hyperedge"],
        entry["EdgeSymmetry", "the symmetry type of a hyperedge: \"Directed\", \"Unordered\" or \"Cyclic\""],
        entry["EdgeLabel", "the label of a hyperedge, or None"],
        entry["EdgeObjectQ", "test for a valid Edge object"],

        gSection["Hypergraphs"],
        entry["Hypergraph", "a hypergraph, as an unordered multiset of hyperedges"],
        entry["HypergraphQ", "test for a valid Hypergraph object"],
        entry["HypergraphSkeleton", "the underlying unlabelled hypergraph"],
        entry["SimpleHypergraphQ", "whether no edge is repeated, labels set aside"],
        gTextC[{"The standard graph functions ", inline["VertexList"], ", ", inline["EdgeList"],
            ", ", inline["VertexCount"], " and ", inline["EdgeCount"],
            " work directly on a Hypergraph, added as UpValues rather than redefined. Further \
properties are read as ", inline["hg[\"prop\"]"], ", and ", inline["hg[\"Properties\"]"],
            " lists them."}],

        gSection["Canonical form and isomorphism"],
        entry["CanonicalHypergraph", "a canonical representative of the isomorphism class"],
        entry["IsomorphicHypergraphQ", "test two hypergraphs for isomorphism, with or without labels"],
        entry["FindHypergraphIsomorphism", "the vertex renaming, and optionally the label correspondences"],
        gText["Labelled isomorphism matches labels up to a one-to-one correspondence rather than \
identically, with vertex labels and edge labels corresponded independently. The option \
\"Labeled\" -> False compares the skeletons instead."],

        gSection["Drawing"],
        entry["HypergraphPlot", "draw a hypergraph"],
        entry["HypergraphEmbedding", "the coordinates chosen for the vertices"],
        entry["HypergraphLargeQ", "whether a hypergraph is too big to draw, and so shown as a summary box"],
        entry["SetHypergraphPlotThresholds", "change the sizes beyond which a summary box is shown"],
        gText["A Hypergraph draws itself by default, the way a Graph does, so HypergraphPlot is \
needed only in order to pass options."],

        gSection["Arrays"],
        gText["An array is a nesting of lists, and one object holds any of them: nothing about \
the structure of an array depends on what sits in its entries, which may be numbers, elements of \
a finite field, symbolic expressions or labels. The order of an array is its number of indices, \
and an array of order n is an n-array: a vector is a 1-array and a matrix is a 2-array. A scalar \
is the 0-array. An array is regular when all of its indices range over the same length, and only \
a regular array can carry an index symmetry."],
        gTextC[{"What an array is made of is read off its entries by ", inline["ArrayDomain"],
            " rather than declared alongside them. The symmetry is the one thing it is told \
rather than asked, being a claim about the entries that they may or may not bear out."}],
        entry["ArrayObject", "an array, whatever its entries are"],
        entry["GenerateSymbolicArray", "build one whose entries are a name applied to the indices"],
        entry["ArrayOrder", "the number of indices"],
        entry["ArrayDimensions", "the lengths of the indices"],
        entry["ArraySymmetry", "the index symmetry: \"Rigid\", \"Cyclic\" or \"Symmetric\""],
        entry["ArrayDomain", "the value domain of the entries, or \"Expression\""],
        entry["ArrayEntries", "the entries, as a nested list"],
        entry["RegularArrayQ", "whether every index ranges over the same length"],
        entry["ArrayObjectQ", "test for a valid array object"],
        gTextC[{"Dimensions and Normal work directly on an array object, and further properties \
are read as ", inline["arr[\"prop\"]"], "."}],

        gSection["Special arrays"],
        gText["Constant arrays and the generalized Kronecker deltas, in any order, dimension and \
value domain. Each declares the strongest symmetry that actually holds of it."],
        entry["ZeroArray", "every entry zero"],
        entry["OneArray", "every entry one"],
        entry["IdentityArray", "one where all the indices agree, zero elsewhere"],
        entry["PartialIdentityArray", "one where the indices at chosen positions agree"],

        gSection["Contraction"],
        entry["ArrayMultiply", "contract arrays by an index specification, in the manner of Einstein summation"],
        gText["The specification is written either as a string, \"ij,jk->ik\", or as a rule \
between lists of index labels. An index may occur in any number of the arrays and in the output \
as well, so diagonals and three-way contractions are expressible, not only pairwise ones. A 0-array \
is named by an empty group of labels, and contracting every index away gives one back."],

        gSection["Drawing arrays"],
        gText["There are two families, and an array of any order may be drawn in either. The flat \
family gives plane pictures throughout: a row of cells at order one, a grid at order two, and \
above that a nesting of those, a grid whose cells hold grids. The spatial family gives cubes \
arranged in space throughout: a line of cubes, a plane of them, a lattice, and above that the same \
lattices tiled through space, block beside block, all in one picture."],
        gTextC[{"Which indices go outside and which inside is a choice rather than a fact about \
the array, and ", inline["\"Nesting\""], " makes it. A flat picture holds one or two indices at a \
time and a spatial one holds three, so that is the only difference between the two defaults."}],
        entry["ArrayGraphics", "draw an array of any order in the plane"],
        entry["ArrayGraphics3D", "draw an array of any order as cubes in space"],
        entry["ArrayNesting", "how the indices are split by default, for either family"],
        entry["HypermatrixGraphics", "draw every array of a hypermatrix, in the plane"],
        entry["HypermatrixGraphics3D", "the same, as cubes in space"],

        gSection["Adding and multiplying entry by entry"],
        entry["ArrayAdd", "add arrays entry by entry"],
        entry["ArrayTimes", "multiply arrays entry by entry"],
        gTextC[{"These are the multilinear structure of arrays, as against the contraction \
ArrayMultiply. Arrays of the same dimensions combine entry by entry, and a 0-array, having no \
shape of its own, reaches every entry \[LongDash] which is why scalar multiplication needs no name \
of its own. ", inline["Plus"], " and ", inline["Times"], " on array objects are the same \
operations, so ", inline["2 a + b"], " means what it looks like."}],

        gSection["Equational identities"],
        entry["FindArrayEquations", "search for equations between all nestings of given operations over given arguments"],
        gText["The search screens candidates on random numerical arrays first, which is fast, and \
verifies only the survivors on symbolic arrays. Matrix multiplication on three arguments returns \
associativity; the Fish product on five returns a twisted ternary associativity; the \
Bhattacharya-Mesner product on five returns nothing. An operation of arity one may be nested, so \
that the involutivity of a transpose is within reach, and an equation that follows from the others \
is not reported."],

        gSection["From hypergraphs to hypermatrices"],
        gTextC[{"The two halves of the package meet here. An edge has an arity and a symmetry \
type; an array has an order and a symmetry; and the two vocabularies line up one for one \
\[LongDash] ", inline["\"Directed\""], " with ", inline["\"Rigid\""], ", ", inline["\"Cyclic\""],
            " with ", inline["\"Cyclic\""], ", ", inline["\"Unordered\""], " with ",
            inline["\"Symmetric\""], ". An array holds one symmetry and one order, so the edges \
are gathered by arity and symmetry type together and each group becomes one array."}],
        entry["AdjacencyHypermatrix", "the adjacency hypermatrix of a hypergraph"],
        gTextC[{"Identical copies of an edge share an entry, so a repeated edge comes out as an \ninteger multiple. With ", inline["\"Labeled\" -> True"], " the entries are the edge labels \nthemselves rather than counts, which needs the hypergraph to be simple \[LongDash] one entry \ncannot hold two labels."}],
        gText["The entry at an index tuple counts the edges sitting on it, and every tuple of an \
orbit reads the same count, which is what makes each array carry its symmetry by construction. \
Nothing is lost in the passage: the canonical entries name exactly the edges that went in."],
        gTextC[{"The vertices are a set, so numbering the indices is a choice; the Hypergraph \
constructor makes it by storing its vertices sorted. That is deterministic but not invariant \
\[LongDash] renaming the vertices can permute the indices \[LongDash] so for an invariant of the \
isomorphism class, compose with ", inline["CanonicalHypergraph"], "."}],

        gSection["Hypermatrices"],
        gText["A hypermatrix is a list of arrays held in canonical order: lowest order first, \
then the shorter dimensions, then symmetry taking symmetric before cyclic before rigid. Giving \
the same arrays in a different order produces the identical expression."],
        entry["Hypermatrix", "a canonically ordered list of arrays"],
        entry["HypermatrixArrays", "the arrays, in canonical order"],
        entry["HypermatrixQ", "test for a valid hypermatrix object"],

        gSection["Messages"],
        entry["Edge::sym", "the given edge symmetry type is not one of the three recognized ones"],
        entry["Edge::spec", "the expression is not a valid hyperedge specification"],
        entry["Hypergraph::spec", "the expression is not a valid hypergraph specification"],
        entry["Hypergraph::vlbl", "one vertex identifier was given more than one label"],
        entry["CanonicalHypergraph::large", "the isomorphism search will be slow at this size"],
        entry["ArrayObject::nosym", "the entries are not invariant under the declared symmetry"],
        entry["ArrayObject::irreg", "an irregular array cannot carry a symmetry"],
        entry["ArrayObject::data", "the entries are not a rectangular array"],
        entry["GenerateSymbolicArray::name", "an array name must be a symbol or a string"],
        entry["Hypermatrix::arrays", "something given was not an array"],
        entry["ArrayMultiply::rank", "an array has a different number of indices from its specification"],
        entry["ArrayMultiply::dims", "one index label is used with two different lengths"],
        entry["ArrayMultiply::out", "an output index does not occur among the inputs"],
        entry["FindArrayEquations::arity", "the arity of an operation could not be determined"],
        entry["FindArrayEquations::eval", "no nesting evaluated on arrays of the shape used"],
        entry["FindArrayEquations::shape", "no order from 1 to 5 suits every operation"],
        entry["FindArrayEquations::depth", "\"UnaryDepth\" is not a non-negative integer"],
        entry["ArrayAdd::dims", "the arrays are not of the same dimensions"],
        entry["ArrayTimes::dims", "the same, for multiplication entry by entry"],
        entry["ArrayGraphics::nesting", "the given nesting does not account for every index"],
        entry["ArrayGraphics::block", "a block of more than two indices has no flat picture"],
        entry["ArrayGraphics3D::block", "a block of more than three indices has nowhere to go in space"],
        entry["AdjacencyHypermatrix::hg", "the argument is not a hypergraph"],
        entry["HypergraphRule::hg", "a side of a rule is not a hypergraph"],
        entry["HypergraphRuleApply::rule", "what was given is not a rewrite rule"],
        entry["AdjacencyHypermatrix::nonsimple", "labels cannot be entries unless no edge is repeated"],
        entry["ArrayGraphics::big", "the array has too many entries to be drawn legibly"],

        gSection["Rewriting"],
        gText["A rewrite rule is a pair of hypergraphs. Applying it means finding the ways its \
input sits inside a hypergraph, and for each of them replacing what was matched by the output. \
Each such way is an event, reported as an association saying not only what the new hypergraph is \
but what was matched, what was created and what was destroyed."],
        entry["HypergraphRule", "a rewrite rule, as a pair of hypergraphs"],
        entry["HypergraphRuleQ", "test for a valid rule"],
        entry["HypergraphRuleApply", "apply a rule, giving one event per match; rule[hg] is the same"],
        entry["HypergraphRuleMatches", "the ways the input sits inside a hypergraph, without rewriting"],
        gTextC[{"Symmetry types must agree between a pattern edge and the edge it lies on, and \
the pattern is matched over the orderings that symmetry allows. Labels constrain: a pattern \
carrying no label matches whatever it lies on, one carrying a label matches only the same label, \
and ", inline["\"Labeled\" -> False"], " ignores them. The shape of all this follows the reference \
paclet, so that rules written for that one read the same here."}],

        gSection["Not yet implemented"],
        gText["The objects, isomorphism, the array algebra, the drawing, the adjacency \
hypermatrix and the core of rewriting are all in place. What remains of the rewriting is the \
machinery around it: the generations of states a rule produces, the causal graph of its events, \
and the conditions under which two events depend on one another or their matches overlap. After \
that come the plex diagrams that tie rewriting to hypermatrix algebra, and a visual editor for \
hypergraphs and for rules."]
    }],
    StyleDefinitions -> "Default.nb",
    WindowSize -> {820, 750}
];

Export[FileNameJoin[{$guideDir, "Hypergraphs.nb"}], guide, "NB"];

Print["symbol pages : ", Length @ FileNames["*.nb", $symbolDir]];
Print["guide pages  : ", Length @ FileNames["*.nb", $guideDir]];
Print["failed example evaluations: ", Length[$failed]];
Scan[Print["   ", #] &, $failed];





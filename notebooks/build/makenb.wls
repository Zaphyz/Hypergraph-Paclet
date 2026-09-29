#!/usr/bin/env wolframscript
$ProgressReporting = False;
(* The repository root, from where this script sits: <root>/notebooks/build/. *)
(* Forward slashes: $root is spliced into a string that is then parsed, and a Windows
   backslash there is read as a string escape. *)
$root = StringReplace[ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]],
                      "\\" -> "/"];
target = FileNameJoin[{$root, "notebooks", "HypergraphBasics.nb"}];


title[s_]    := Cell[s, "Title"];
subtitle[s_] := Cell[s, "Subtitle"];
section[s_]  := Cell[s, "Section"];
sub[s_]      := Cell[s, "Subsection"];
text[s_]     := Cell[s, "Text"];
(* Each input cell is evaluated here in the kernel and its result attached as an Output cell
   holding real StandardForm boxes.  NotebookEvaluate from a script formats results in
   OutputForm, which loses every graphic, so the boxing is done by hand. *)
$evalLog = {};
code[s_] := code[s, s];

initCode[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    {Cell[s, "Input", InitializationCell -> True]}
];
code[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    AppendTo[$evalLog, {StringTake[StringReplace[s, "\n" -> " "], UpTo[58]],
                        Head[res], ! FreeQ[ToBoxes[res, StandardForm], GraphicsBox]}];
    (* A Sequence returned from Module would splice into Module's own argument list, so
       the cells come back as a list and the whole notebook body is flattened once at the end. *)
    If[ res === Null,
        {Cell[s, "Input"]},
        {Cell[s, "Input"], Cell[BoxData[ToBoxes[res, StandardForm]], "Output"]}
    ]
];
note[s_]     := Cell[
    TextData[{StyleBox["Design note.  ", FontWeight -> "Bold", FontSlant -> "Italic"], s}],
    "Text", CellFrame -> {{2, 0}, {0, 0}}, CellFrameColor -> GrayLevel[0.7],
    CellFrameMargins -> {{10, 0}, {6, 6}}, FontColor -> GrayLevel[0.25]
];

cells = {

title["Hypergraphs: Core Objects and Isomorphism"],
subtitle["WolframInstitute`Hypergraphs` \[LongDash] first milestone"],

text["This notebook shows the three core objects of the rewrite \[LongDash] Vertex, Edge and \
Hypergraph \[LongDash] their default graphical display, and the isomorphism test, together with \
the language design choices behind them. Everything below is live output from src/."],

text["A Hypergraph now draws itself, the way a Graph does. Where the exact stored expression is \
the point rather than the picture, the cell is repeated with InputForm."],

text["Not implemented yet: rewriting rules, the hypermatrix layer, and the causal machinery. \
Those come next."],

section["Loading the package"],

text["The cell below is an initialization cell, which the front end evaluates when this notebook \
is opened, so that every symbol is ready by the time you read this."],

initCode["PacletDirectoryLoad[ParentDirectory[NotebookDirectory[]]];\n\
Needs[\"WolframInstitute`Hypergraphs`\"]",
     "PacletDirectoryLoad[\"" <> $root <> "\"];Needs[\"WolframInstitute`Hypergraphs`\"]"],

text["The package lives in its own context, deliberately distinct from the installed upstream \
paclet WolframInstitute`Hypergraph` (singular) so that both can coexist while we develop. This \
is a one-line change if you would prefer a different name."],

code["Names[\"WolframInstitute`Hypergraphs`*\"]"],

section["1.  Vertices"],

text["A vertex is Vertex[id] or Vertex[id, lab]. The identifier id is any Wolfram Language \
expression and is what makes the vertex unique; lab is any expression and may be omitted."],

code["Vertex[1]"],
code["Vertex[1, \"a\"]"],
code["Vertex[1, None]"],

note["A label of None means \[OpenCurlyDoubleQuote]no label\[CloseCurlyDoubleQuote], so \
Vertex[id, None] collapses to the one-argument form. Keeping a single spelling per vertex means \
that ordinary structural equality (===) on vertices already means \
\[OpenCurlyDoubleQuote]the same vertex\[CloseCurlyDoubleQuote], which the isomorphism code \
relies on throughout."],

text["The two parts are read back with VertexID and VertexLabel:"],

code["{VertexID[Vertex[x, \"a\"]], VertexLabel[Vertex[x, \"a\"]], VertexLabel[Vertex[1]]}"],

text["Identifiers really are arbitrary expressions \[LongDash] integers, strings, symbols, \
lists, anything:"],

code["{Vertex[1], Vertex[\"a\"], Vertex[{1, 2}], Vertex[u], Vertex[f[3]]}"],

note["Wherever a vertex is expected you may write the bare identifier instead, and it is \
promoted to a Vertex automatically. That is what keeps the second Hypergraph constructor, which \
takes plain lists of identifiers, as light as you specified it."],

section["2.  Edges"],

text["A hyperedge is Edge[{v1, v2, \[Ellipsis]}, sym, lab]. The symmetry type sym is one of \
\"Directed\" (the default), \"Unordered\" or \"Cyclic\", and lab may be omitted."],

code["Edge[{1, 2, 3}]"],
code["Edge[{1, 2, 3}, \"Unordered\"]"],
code["Edge[{1, 2, 3}, \"Cyclic\", \"f\"]"],

note["The symmetry types are strings rather than symbols because Cyclic is already a protected \
System` symbol in the Wolfram Language. \"Ordered\" is accepted as a synonym for \"Directed\", \
and the spelling is case-insensitive on input, but the stored form is always one of the three \
canonical strings."],

note["The stored form always shows the symmetry type explicitly, even when it is the default, \
because the symmetry changes the mathematics of the edge. The label is the opposite case \
\[LongDash] it is dropped when absent, so an unlabelled edge stays short to read."],

note["Vertices and edges keep their literal form on output; only a whole Hypergraph turns into \
a picture. An edge on its own has no embedding, and its identity is exactly the expression you \
see, so printing it literally is the honest thing to do."],

sub["Canonical vertex order"],

text["Each edge stores the least member of its symmetry orbit. An unordered edge is sorted, a \
cyclic edge is rotated to its smallest rotation, and a directed edge is left alone:"],

code["{Edge[{3, 2, 1}, \"Unordered\"], Edge[{3, 1, 2}, \"Cyclic\"], Edge[{3, 2, 1}]}"],

text["The consequence is that two Edge objects are identical exactly when they are the same \
hyperedge. A rotation of a cyclic edge is the same edge:"],

code["Edge[{1, 2, 3}, \"Cyclic\"] === Edge[{3, 1, 2}, \"Cyclic\"]"],

text["\[Ellipsis] but a reflection of it is not, since the cyclic group contains rotations only:"],

code["Edge[{1, 2, 3}, \"Cyclic\"] === Edge[{1, 3, 2}, \"Cyclic\"]"],

text["The symmetry type and the label are both part of an edge's identity:"],

code["{Edge[{1, 2}] === Edge[{1, 2}, \"Unordered\"],\n \
 Edge[{1, 2}, \"Directed\", \"f\"] === Edge[{1, 2}]}"],

note["This is the pivot of the whole design. Once === on edges means \
\[OpenCurlyDoubleQuote]is the same hyperedge\[CloseCurlyDoubleQuote], a hypergraph can be \
compared as a plain multiset of edges, and the symmetry types never have to be consulted again \
during an isomorphism search \[LongDash] they have already been absorbed into the \
representation."],

text["An unrecognized symmetry type is rejected rather than silently accepted:"],

code["Edge[{1, 2}, \"Sideways\"]"],

text["The outputs in this notebook were computed in a plain kernel, so the accompanying \
message appears on the console rather than as a cell. Evaluating the cell yourself shows it:\n\n \
   Edge::sym: Sideways is not a valid edge symmetry type; expected \"Directed\", \"Unordered\" \
or \"Cyclic\"."],

section["3.  Hypergraphs"],

text["A hypergraph can be given as a list of Edge objects, in which case the vertex set is the \
union of the vertices of those edges. It displays as a drawing of itself:"],

code["Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{1, 2, 3}, \"Unordered\"]}]"],

text["The picture is the same object; InputForm shows what is actually stored:"],

code["InputForm @ Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{1, 2, 3}, \"Unordered\"]}]"],

text["A hypergraph can equally be given as a list of vertex identifiers together with edges \
written as plain lists of identifiers, which are directed and unlabelled by default:"],

code["Hypergraph[{1, 2, 3}, {{1}, {1, 2}, {1, 2, 3}}]"],

text["A list with no edges in it gives an edgeless hypergraph, and the empty list gives the \
empty hypergraph:"],

code["{Hypergraph[{1, 2, 3, 4}], Hypergraph[{}]}"],

sub["The vertex set and the propagation of labels"],

text["Vertices that occur only inside edges are absorbed into the vertex set:"],

code["Hypergraph[{Edge[{1, 2}]}][\"VertexIDs\"]"],

text["Labels may be given in the vertex list:"],

code["Hypergraph[{Vertex[1, \"a\"], Vertex[2, \"b\"], 3}, {{1, 2}, {2, 3}}]"],

text["\[Ellipsis] or on any single occurrence of a vertex inside an edge, in which case they \
propagate to every other occurrence. Here vertex 1 is labelled once and carries that label in \
both edges, as the drawing and the stored form both show:"],

code["Hypergraph[{Edge[{Vertex[1, \"a\"], 2}], Edge[{1, 3}]}]"],
code["InputForm @ Hypergraph[{Edge[{Vertex[1, \"a\"], 2}], Edge[{1, 3}]}]"],

note["A vertex identifier denotes one vertex, so it can carry only one label. The explicit \
vertex list wins if the two disagree, and the disagreement is reported rather than resolved \
silently. This keeps the invariant that the vertex set is the single source of truth about \
labels, which is what makes labelled isomorphism well defined."],

sub["A hypergraph is a multiset of edges"],

text["Edge order carries no information, so the two spellings below are the same object:"],

code["Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}] === Hypergraph[{Edge[{2, 3}], Edge[{1, 2}]}]"],

text["Multiplicity, on the other hand, does carry information. A repeated edge is kept, is drawn \
as a second arc, and a hypergraph with a doubled edge is not the same as one without:"],

code["Hypergraph[{Edge[{1, 2}], Edge[{1, 2}], Edge[{2, 3}]}]"],
code["{EdgeCount[Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}]],\n \
 Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}] === Hypergraph[{Edge[{1, 2}]}]}"],

note["Edges are sorted into a canonical order when the hypergraph is built, which is what makes \
=== on hypergraphs agree with equality of multisets. The same is done for the vertex list. So a \
Hypergraph expression is always in a normal form, and any two ways of writing the same \
hypergraph produce literally the same expression."],

section["4.  How a hypergraph is drawn"],

text["The drawing is the reference paclet's: the same spring layout of an auxiliary graph, the \
same blue palette, binary edges as arcs, and edges of arity three and above as filled concave \
polygons through their vertices. The plot is also available on its own as HypergraphPlot."],

code["HypergraphPlot[Hypergraph[{Edge[{1, 2, 3}, \"Unordered\"], Edge[{3, 4}],\n \
                            Edge[{1, 4, 5}, \"Cyclic\"]}], ImageSize -> 300]"],

sub["The three symmetry types"],

text["The symmetry type of each edge is visible in the drawing. A directed edge carries \
arrowheads running along it, from its first vertex to its last. An unordered edge carries none. \
A cyclic edge carries none either, and is marked instead by a small circular arrow at its \
centre:"],

code["Grid[{\n \
  {\"Directed\", \"Unordered\", \"Cyclic\"},\n \
  HypergraphPlot[Hypergraph[{Edge[{1, 2, 3, 4}, #]}], ImageSize -> 170] & /@\n \
    {\"Directed\", \"Unordered\", \"Cyclic\"}\n \
 }, Frame -> All, Alignment -> Center]"],

note["Directed and unordered are drawn exactly as the reference draws ordered and unordered \
edges. Cyclic is the one departure. The reference puts arrowheads on a cyclic edge too, which \
leaves it differing from a directed edge only by whether the last side happens to close the \
loop \[LongDash] easy to miss on a small picture, and on an edge of arity two there is no \
closing side to look at at all. Dropping the arrowheads and marking a cyclic edge with the badge \
alone makes the three types tell themselves apart at a glance."],

text["On an edge of arity two the badge is the whole of the distinction, since there is \
nothing else to tell a cyclic edge from the other two:"],

code["Grid[{\n \
  {\"Directed\", \"Unordered\", \"Cyclic\"},\n \
  HypergraphPlot[Hypergraph[{Edge[{1, 2}, #]}], ImageSize -> 170] & /@\n \
    {\"Directed\", \"Unordered\", \"Cyclic\"}\n \
 }, Frame -> All, Alignment -> Center]"],

text["When a cyclic edge is labelled, the circular arrow is drawn wider so that it encircles the \
label rather than colliding with it:"],

code["Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{2, 3}, \"Cyclic\", \"g\"],\n \
            Edge[{1, 3, 4}, \"Unordered\", \"h\"]}]"],

note["The badge is drawn with Offset, so its radius is a fixed number of printer's points rather \
than a fraction of the embedding. A plot-relative radius looked right on one example and wrong \
on the next, because the extent of a spring embedding has nothing to do with how big the label \
text is."],

note["Vertex labels are shown by default: a vertex's label when it has one, and otherwise its \
identifier. The reference paclet shows nothing by default. Here the identifier is the thing that \
makes a vertex what it is, and reading identifiers off the picture is what makes the isomorphism \
examples below checkable by eye, so it seemed the more useful default. VertexLabels -> None \
turns them off."],

note["The colours are the reference's light-mode colours, on an explicit white background. The \
reference wraps each colour in LightDarkSwitched on 14.3 and later, but that wrapper is only \
resolved by a live front end; rendered from a plain kernel it is an unrecognized directive and \
the entire plot comes out as an error box. A fixed palette renders identically everywhere."],

sub["Displaying as a Graph does"],

text["The picture is not merely a picture of the hypergraph: it is the hypergraph. Feeding the \
displayed output back to the kernel recovers the object, exactly as it does for a Graph:"],

code["ToExpression[ToBoxes[Hypergraph[{{1, 2}, {2, 3}}]]] === Hypergraph[{{1, 2}, {2, 3}}]"],

text["A hypergraph too large to draw usefully falls back to a summary box instead, with the \
thresholds adjustable through SetHypergraphPlotThresholds:"],

code["Hypergraph[Table[Edge[{i, i + 1}], {i, 40}]]"],

section["5.  Accessors and properties"],

code["h = Hypergraph[{Vertex[1, \"a\"], Vertex[2, \"b\"], 3}, {{1, 2}, {2, 3}}];"],
code["{VertexList[h], EdgeList[h], VertexCount[h], EdgeCount[h]}"],

note["VertexList, EdgeList, VertexCount and EdgeCount are protected System` symbols. They are \
extended with UpValues on Hypergraph rather than redefined, so the ordinary graph meanings are \
untouched and no shadowing warning is ever produced."],

text["Everything else is reached through a string property, in the style of the upstream paclet:"],

code["h[\"Properties\"]"],
code["{h[\"VertexIDs\"], h[\"VertexLabels\"], h[\"Arity\"], h[\"EdgeSymmetries\"]}"],

text["There are predicates for each of the three object types:"],

code["{VertexObjectQ[Vertex[1]], EdgeObjectQ[Edge[{1, 2}]], HypergraphQ[h], HypergraphQ[17]}"],

note["The natural names VertexQ and EdgeQ are protected System` symbols \[LongDash] they are \
the graph membership tests VertexQ[g, v] and EdgeQ[g, e] \[LongDash] so the predicates here are \
VertexObjectQ and EdgeObjectQ. Taking the System` names instead would have reproduced exactly \
the shadowing problem that SetReplace and the upstream Hypergraph paclet already suffer from, \
where four symbols collide and every load prints a warning."],

section["6.  Isomorphism"],

text["Two hypergraphs are isomorphic when some renaming of vertex identifiers carries one onto \
the other: the edge multisets must match exactly, each edge matching up to its own symmetry \
type, with edge multiplicities preserved."],

text["For labelled isomorphism the renaming must also carry the labels of the first onto the \
labels of the second, by a one-to-one correspondence. Labels are therefore significant only up \
to renaming: what has to agree is which elements share a label, not what the labels are called. \
Vertex labels and edge labels are put in correspondence independently of one another, and an \
element with no label can only correspond to an element with no label."],

code["a = Hypergraph[{{1, 2}, {2, 3}}];\n\
b = Hypergraph[{{10, 20}, {20, 30}}];\n\
c = Hypergraph[{{1, 2}, {1, 3}}];\n\
{a, b, c}"],

text["a and b are the same directed path on different identifiers, so they are isomorphic. c is \
an out-star, which as a directed hypergraph is a different object:"],

code["{IsomorphicHypergraphQ[a, b], IsomorphicHypergraphQ[a, c]}"],

text["The witnessing renaming is available too, and is an empty association when there is none:"],

code["{FindHypergraphIsomorphism[a, b], FindHypergraphIsomorphism[a, c]}"],

sub["Symmetry types are respected"],

text["The same two hypergraphs, with both edges made unordered, become isomorphic \[LongDash] \
an undirected path on three vertices is an undirected star:"],

code["au = Hypergraph[{Edge[{1, 2}, \"Unordered\"], Edge[{2, 3}, \"Unordered\"]}];\n\
cu = Hypergraph[{Edge[{1, 2}, \"Unordered\"], Edge[{1, 3}, \"Unordered\"]}];\n\
{au, cu, IsomorphicHypergraphQ[au, cu]}"],

text["A directed edge is never the same as an unordered one, so a and au are not isomorphic:"],

code["IsomorphicHypergraphQ[a, au]"],

sub["Labels count only up to renaming"],

text["The two hypergraphs below are the same directed path, one carrying edge labels a and b, the \
other x and y. They are isomorphic: on top of the vertex renaming sits the label correspondence \
a \[Rule] x, b \[Rule] y."],

code["h1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];\n\
h2 = Hypergraph[{Edge[{4, 5}, \"Directed\", \"x\"], Edge[{5, 6}, \"Directed\", \"y\"]}];\n\
{h1, h2}"],

code["IsomorphicHypergraphQ[h1, h2]"],

text["Asking for the labels as well as the vertices returns both correspondences:"],

code["FindHypergraphIsomorphism[h1, h2, \"Labels\" -> True]"],

text["h3 below is the same path again, but carries the one label x on both of its edges. It is \
not isomorphic to h1, because a correspondence would have to send both a and b to x, and that is \
not one-to-one. What distinguishes the two is not the names of the labels but how many edges \
share each of them:"],

code["h3 = Hypergraph[{Edge[{4, 5}, \"Directed\", \"x\"], Edge[{5, 6}, \"Directed\", \"x\"]}];\n\
{h1, h3}"],

code["{IsomorphicHypergraphQ[h1, h3], IsomorphicHypergraphQ[h1, h3, \"Labeled\" -> False]}"],

text["When there is no isomorphism the maps come back empty:"],

code["FindHypergraphIsomorphism[h1, h3, \"Labels\" -> True]"],

note["An unlabelled element counts as carrying no label rather than as carrying some particular \
one, so it can only ever correspond to another unlabelled element. Without that, every \
unlabelled hypergraph would be isomorphic to a labelled one with the labels all alike."],

text["Vertex labels and edge labels are corresponded independently of each other. Here both kinds \
are renamed at once, the vertices by A \[Rule] P, B \[Rule] Q and the edges by f \[Rule] g, \
g \[Rule] f:"],

code["i1 = Hypergraph[{Edge[{Vertex[1, \"A\"], Vertex[2, \"A\"]}, \"Directed\", \"f\"],\n \
                Edge[{Vertex[2, \"A\"], Vertex[3, \"B\"]}, \"Directed\", \"g\"]}];\n\
i2 = Hypergraph[{Edge[{Vertex[1, \"P\"], Vertex[2, \"P\"]}, \"Directed\", \"g\"],\n \
                Edge[{Vertex[2, \"P\"], Vertex[3, \"Q\"]}, \"Directed\", \"f\"]}];\n\
{i1, i2}"],

code["FindHypergraphIsomorphism[i1, i2, \"Labels\" -> True]"],

text["Being independent, the two can disagree. i3 has the same edge labels as i1 but shares its \
vertex labels out differently \[LongDash] P once and Q twice, against A twice and B once \
\[LongDash] so no correspondence exists:"],

code["i3 = Hypergraph[{Edge[{Vertex[1, \"P\"], Vertex[2, \"Q\"]}, \"Directed\", \"f\"],\n \
                Edge[{Vertex[2, \"Q\"], Vertex[3, \"Q\"]}, \"Directed\", \"g\"]}];\n\
{IsomorphicHypergraphQ[i1, i3], IsomorphicHypergraphQ[i1, i3, \"Labeled\" -> False]}"],

text["The labels can still force the vertices to move. In k1 the label f is on two edges and g \
on one; in k2 it is q that is on two and p on one. So f must correspond to q and g to p, which \
obliges vertex 4 to go to vertex 2:"],

code["k1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{1, 3}, \"Directed\", \"f\"],\n \
                Edge[{1, 4}, \"Directed\", \"g\"]}];\n\
k2 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"p\"], Edge[{1, 3}, \"Directed\", \"q\"],\n \
                Edge[{1, 4}, \"Directed\", \"q\"]}];\n\
{k1, k2}"],

code["FindHypergraphIsomorphism[k1, k2, \"Labels\" -> True]"],
sub["Labelled and unlabelled"],

note["IsomorphicHypergraphQ respects labels by default. This is the conservative choice: it \
distinguishes more, and it collapses to plain isomorphism automatically when nothing is \
labelled, so the unlabelled case costs nothing. The option \"Labeled\" -> False compares the \
skeletons instead."],

code["l1 = Hypergraph[{Vertex[1, \"A\"], Vertex[2, \"B\"], Vertex[3, \"C\"]}, {{1, 2}, {2, 3}}];\n\
l2 = Hypergraph[{Vertex[10, \"A\"], Vertex[20, \"B\"], Vertex[30, \"C\"]}, {{10, 20}, {20, 30}}];\n\
l3 = Hypergraph[{Vertex[1, \"C\"], Vertex[2, \"B\"], Vertex[3, \"A\"]}, {{1, 2}, {2, 3}}];\n\
{l1, l2, l3}"],

text["l1 and l2 are both the labelled path A \[Rule] B \[Rule] C, so they are isomorphic. So, \
now, is l3, the path C \[Rule] B \[Rule] A: leaving the vertices where they are and exchanging \
the labels A and C carries one to the other, and exchanging two labels is a perfectly good \
one-to-one correspondence:"],

code["{IsomorphicHypergraphQ[l1, l2],\n \
 IsomorphicHypergraphQ[l1, l3],\n \
 IsomorphicHypergraphQ[l1, l3, \"Labeled\" -> False]}"],

code["FindHypergraphIsomorphism[l1, l2]"],

text["What a renaming cannot repair is a difference in how the labels are shared out. Two \
labellings of the same path, one reading A, A, B and the other A, B, B, are still not \
isomorphic: any correspondence would have to send A to both A and B at once. Their skeletons of \
course agree. This is also the case the upstream paclet gets wrong:"],

code["m1 = Hypergraph[{Vertex[1, \"A\"], Vertex[2, \"A\"], Vertex[3, \"B\"]}, {{1, 2}, {2, 3}}];\n\
m2 = Hypergraph[{Vertex[1, \"A\"], Vertex[2, \"B\"], Vertex[3, \"B\"]}, {{1, 2}, {2, 3}}];\n\
{m1, m2}"],

code["{IsomorphicHypergraphQ[m1, m2], IsomorphicHypergraphQ[m1, m2, \"Labeled\" -> False]}"],

text["Edge labels behave the same way. Below, p followed by q along a directed path is \
isomorphic to q followed by p, by the correspondence that exchanges p and q:"],

code["t1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"p\"], Edge[{2, 3}, \"Directed\", \"q\"]}];\n\
t2 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"q\"], Edge[{2, 3}, \"Directed\", \"p\"]}];\n\
{t1, t2}"],

code["{IsomorphicHypergraphQ[t1, t2], IsomorphicHypergraphQ[t1, t2, \"Labeled\" -> False]}"],

sub["Isomorphism with labelled edges"],

text["Edge labels are treated on exactly the same footing as vertex labels. Two hypergraphs \
carrying the same edge labels on different identifiers are isomorphic, and the renaming is \
reported as usual:"],

code["e1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{2, 3}, \"Directed\", \"g\"]}];\n\
e2 = Hypergraph[{Edge[{7, 8}, \"Directed\", \"f\"], Edge[{8, 9}, \"Directed\", \"g\"]}];\n\
{e1, e2}"],

code["{IsomorphicHypergraphQ[e1, e2], FindHypergraphIsomorphism[e1, e2]}"],

text["The labels can force the renaming to be a non-trivial one. These two stars differ only in \
which arm carries f and which carries g, so they are isomorphic, but only by a map that \
exchanges vertices 2 and 3:"],

code["s1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{1, 3}, \"Directed\", \"g\"]}];\n\
s2 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"g\"], Edge[{1, 3}, \"Directed\", \"f\"]}];\n\
{s1, s2}"],

code["{IsomorphicHypergraphQ[s1, s2], FindHypergraphIsomorphism[s1, s2]}"],

text["An edge carrying a label is not the same as an edge carrying none, and the same label on \
edges of different symmetry type does not match either:"],

code["{IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"]}],\n \
                       Hypergraph[{Edge[{1, 2}]}]],\n \
 IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2, 3}, \"Cyclic\", \"f\"]}],\n \
                       Hypergraph[{Edge[{1, 2, 3}, \"Directed\", \"f\"]}]]}"],

text["Since a hypergraph is a multiset, how many times each label occurs matters too. Two copies \
of f is not the same as one f and one g, though the skeletons cannot tell them apart:"],

code["n1 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{1, 2}, \"Directed\", \"f\"]}];\n\
n2 = Hypergraph[{Edge[{1, 2}, \"Directed\", \"f\"], Edge[{1, 2}, \"Directed\", \"g\"]}];\n\
{IsomorphicHypergraphQ[n1, n2], IsomorphicHypergraphQ[n1, n2, \"Labeled\" -> False]}"],

text["Vertex labels and edge labels are checked together, but corresponded separately. Below b1 \
and b2 agree on both. So does b3, which has b1's two edge labels exchanged \[LongDash] absorbed \
by the edge-label correspondence. b4 is the one that fails: it puts a single label on both edges \
where b1 has two distinct ones, and no one-to-one correspondence can do that:"],

code["b1 = Hypergraph[{Edge[{Vertex[1, \"A\"], Vertex[2, \"B\"]}, \"Directed\", \"f\"],\n \
                Edge[{Vertex[2, \"B\"], Vertex[3, \"C\"]}, \"Cyclic\", \"g\"]}];\n\
b2 = Hypergraph[{Edge[{Vertex[5, \"A\"], Vertex[6, \"B\"]}, \"Directed\", \"f\"],\n \
                Edge[{Vertex[6, \"B\"], Vertex[7, \"C\"]}, \"Cyclic\", \"g\"]}];\n\
b3 = Hypergraph[{Edge[{Vertex[5, \"A\"], Vertex[6, \"B\"]}, \"Directed\", \"g\"],\n \
                Edge[{Vertex[6, \"B\"], Vertex[7, \"C\"]}, \"Cyclic\", \"f\"]}];\n\
b4 = Hypergraph[{Edge[{Vertex[5, \"A\"], Vertex[6, \"B\"]}, \"Directed\", \"f\"],\n \
                Edge[{Vertex[6, \"B\"], Vertex[7, \"C\"]}, \"Cyclic\", \"f\"]}];\n\
{b1, b2, b3, b4}"],

code["{IsomorphicHypergraphQ[b1, b2],\n \
 IsomorphicHypergraphQ[b1, b3],\n \
 IsomorphicHypergraphQ[b1, b4],\n \
 IsomorphicHypergraphQ[b1, b4, \"Labeled\" -> False]}"],

note["An edge's label sits alongside its symmetry type in its identity, so the isomorphism \
search never treats labels as a separate pass. The canonical form replaces each label by a \
placeholder numbered from the structure, after which two edges match when the Edge expressions \
match, which settles vertex labels, edge labels, symmetry and multiplicity in one comparison."],
sub["Skeletons"],

text["The skeleton \[LongDash] the underlying unlabelled hypergraph, with all symmetry types \
kept \[LongDash] is available on its own. The labels A, B, C give way to the bare identifiers:"],

code["{l1, HypergraphSkeleton[l1]}"],
code["InputForm @ HypergraphSkeleton[l1]"],

text["Every vertex label and every edge label is gone, while the symmetry types survive:"],

code["hs = HypergraphSkeleton[Hypergraph[{Edge[{Vertex[1, \"A\"], 2}, \"Unordered\", \"f\"]}]];\n\
{hs[\"VertexLabels\"], hs[\"EdgeLabels\"], hs[\"EdgeSymmetries\"]}"],

note["The labels have to be removed from the Vertex objects sitting inside the edges, not just \
from the vertex list. Because the constructor treats a label on any occurrence of a vertex as \
authoritative and propagates it, leaving labelled vertices inside the edges would put every \
label straight back. This was in fact a bug in the first cut of the code, caught while building \
this notebook, and there are now four assertions guarding it."],

sub["Identifiers of any kind"],

text["Since identifiers are arbitrary expressions, hypergraphs built on completely different \
kinds of identifier can be compared directly:"],

code["w1 = Hypergraph[{Edge[{\"a\", \"b\"}], Edge[{\"b\", {1, 2}}]}];\n\
w2 = Hypergraph[{Edge[{x, y}], Edge[{y, z}]}];\n\
{w1, w2}"],

code["{IsomorphicHypergraphQ[w1, w2], FindHypergraphIsomorphism[w1, w2]}"],

section["7.  Canonical form"],

text["Isomorphism is decided by putting each hypergraph into a canonical form, in which the \
vertices are renamed to 1, 2, \[Ellipsis] . Isomorphic hypergraphs have identical canonical \
forms:"],

code["{CanonicalHypergraph[a], CanonicalHypergraph[b]}"],
code["CanonicalHypergraph[a] === CanonicalHypergraph[b]"],

text["In the labelled case the labels do not travel with their vertices, because their values \
are not part of what is being canonicalized. They are replaced by placeholders \
\[FormalL][1], \[FormalL][2], \[Ellipsis] numbered from the structure, which is exactly what \
makes two hypergraphs differing only by a renaming of labels come out identical. Dropping the \
labels altogether gives the canonical form of the skeleton:"],

code["{CanonicalHypergraph[l1], CanonicalHypergraph[l1, \"Labeled\" -> False]}"],
code["InputForm @ CanonicalHypergraph[l1]"],

text["The placeholders are what let h1 and h2 above, whose edge labels are named differently, \
reduce to the same canonical form:"],

code["{CanonicalHypergraph[h1], CanonicalHypergraph[h2],\n \
 CanonicalHypergraph[h1] === CanonicalHypergraph[h2]}"],
note["Which representative of the isomorphism class is returned depends on the Method option. \
The default narrows the search by first grouping vertices into classes that no isomorphism can \
mix up; Method -> \"BruteForce\" searches every ordering of the vertices instead. Both are \
functions of the isomorphism class alone, so the two always agree about whether two hypergraphs \
are isomorphic, but they settle on different representatives of the same class. The two \
settings must therefore never be mixed within a single comparison. \"BruteForce\" exists to \
cross-check the default, and 300 random pairs were checked that way."],

code["InputForm /@ {CanonicalHypergraph[a], CanonicalHypergraph[a, Method -> \"BruteForce\"]}"],

section["8.  Scope and limits"],

text["The isomorphism search is exhaustive, as agreed for this milestone. It is comfortable up \
to roughly eight vertices, and warns when a search would exceed 40320 candidate orderings. \
Replacing it with a refinement that scales is a later job and will not change any of the \
behaviour shown above."],

text["The layout is inherited from the reference paclet along with its rough edges. The one \
worth knowing about: a hypergraph containing an edge of arity zero lays that edge out as an \
isolated point, which stretches the plot range badly. The reference produces exactly the same \
picture, so this is inherited rather than introduced, but it is worth fixing when the drawing \
code is revisited."],

text["The file tests/BasicExamples.wls runs the non-graphical examples in this notebook as \
assertions, together with some thousands of randomized checks: that a random relabelling of the \
vertices is always recognized as isomorphic to its original and has the same canonical form, \
with and without edge labels; that renaming the labels one-to-one never changes the isomorphism \
class, while collapsing two distinct labels into one always does; that the two search methods \
always agree; that labelled isomorphism always implies isomorphism of skeletons; and that every \
mapping returned by FindHypergraphIsomorphism really is a bijection carrying one hypergraph \
exactly onto the other."],

text["Still to come, in the order of the project outline: the hypermatrix layer, rewriting \
rules, and the causal machinery."]

};

cells = Flatten[cells];

nb = Notebook[cells,
    StyleDefinitions -> "Default.nb",
    WindowSize -> {900, 800},
    CellGrouping -> Automatic,
    InitializationCellEvaluation -> True,
    InitializationCellWarning -> False
];

Quiet @ CreateDirectory[DirectoryName[target]];
Export[target, nb, "NB"];
Print["wrote unevaluated notebook: ", target];
Print["cells: ", Length[cells], "  input: ", Count[cells, Cell[_, "Input", ___]],
      "  output: ", Count[cells, Cell[_, "Output", ___]]];
Print["graphics outputs: ", Count[$evalLog, {_, _, True}]];
Print["failed evaluations: ", Cases[$evalLog, {s_, $evalFailed, _} :> s]];
Print[];
Scan[Print["  ", StringPadRight[#[[1]], 60], " ", StringPadRight[ToString[#[[2]]], 12],
           If[#[[3]], "GRAPHICS", ""]] &, $evalLog];











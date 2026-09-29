#!/usr/bin/env wolframscript

(* Builds notebooks/CausalityBasics.nb. Each input cell is evaluated here in the kernel and its
   result attached as a StandardForm Output cell: NotebookEvaluate from a script formats results
   in OutputForm, which loses every graphic and every summary box. *)

$ProgressReporting = False;
(* The repository root, from where this script sits: <root>/notebooks/build/. *)
(* Forward slashes: $root is spliced into a string that is then parsed, and a Windows
   backslash there is read as a string escape. *)
$root = StringReplace[ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]],
                      "\\" -> "/"];
target = FileNameJoin[{$root, "notebooks", "CausalityBasics.nb"}];

tidy[s_String] := StringReplace[
    StringReplace[s, {"\r\n" -> " ", "\r" -> " ", "\n" -> " "}],
    RegularExpression[" {2,}"] -> " "
];

(* The helpers must not share a name with any variable an example assigns to: the examples run
   in this same kernel, and a clobbered helper fails silently -- a subsection header becomes
   Missing[...] and renders as an empty box. *)
title[s_]    := Cell[tidy[s], "Title"];
subtitle[s_] := Cell[tidy[s], "Subtitle"];
section[s_]  := Cell[tidy[s], "Section"];
subsection[s_] := Cell[tidy[s], "Subsection"];
text[s_]     := Cell[tidy[s], "Text"];
note[s_]     := Cell[
    TextData[{StyleBox["Design note.  ", FontWeight -> "Bold", FontSlant -> "Italic"], tidy[s]}],
    "Text", CellFrame -> {{2, 0}, {0, 0}}, CellFrameColor -> GrayLevel[0.7],
    CellFrameMargins -> {{10, 0}, {6, 6}}, FontColor -> GrayLevel[0.25]
];

$log = {};
code[s_] := code[s, s];

initCode[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    AppendTo[$log, {"INIT: " <> StringTake[s, UpTo[50]], Head[res]}];
    {Cell[s, "Input", InitializationCell -> True]}
];
code[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    AppendTo[$log, {StringTake[StringReplace[s, "\n" -> " "], UpTo[56]], Head[res]}];
    If[ res === Null,
        {Cell[s, "Input"]},
        {Cell[s, "Input"], Cell[BoxData[ToBoxes[res, StandardForm]], "Output"]}
    ]
];

cells = Flatten[{

title["Rewriting Systems and Causality"],
subtitle["WolframInstitute`Hypergraphs` \[LongDash] evolution, events and what depends on what"],

text["A rewriting system is a set of rules together with a state to start from. Evolving it \
means applying every rule in every way it fits, to every state reached so far, for as many \
generations as asked. What comes out is a tree of state occurrences joined by events, and the \
causal graph is the record of which event depended on which. Everything below is live output \
from src/."],

text["The design point this notebook is really about is in section 4. What \"depended on\" means \
is not fixed here: it is an option, because the answer is a fact about what the hypergraph is \
standing for rather than about the rewriting."],

section["Loading the package"],

text["The cell below is an initialization cell, which the front end evaluates when this notebook \
is opened. By the time you read this the package is already loaded."],

initCode["PacletDirectoryLoad[ParentDirectory[NotebookDirectory[]]];\n\
Needs[\"WolframInstitute`Hypergraphs`\"]",
     "PacletDirectoryLoad[\"" <> $root <> "\"];Needs[\"WolframInstitute`Hypergraphs`\"]"],

(* The examples run in this same kernel; clear the names first, and in particular the short
   symbols used below as label variables. *)
(ClearAll[a, b, c, d, e, f, g, p, q, r, s, u, x, y, z,
          subdivide, twoEdges, system, chain4, chainSys, comp, unit, arrow, catSys,
          zig, zigSys, long, pairsOf, byType]; Nothing),

section["1.  A system is rules and a state to start from"],

text["HypergraphRewritingSystem takes the rules and the initial state. The rules may be one \
HypergraphRule, a list of them, or plain arrows; the state may be anything Hypergraph accepts:"],

code["subdivide = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}];\n\
system = HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}}]]"],

code["HypergraphRewritingSystem[{{1, 2}} -> {{1, 3}, {3, 2}}, {{1, 2}}] === system"],

text["What it can be asked for:"],

code["system[\"Properties\"]"],

text["Every property takes a number of generations, which defaults to one:"],

code["{system[\"EventCount\"], system[\"EventCount\", 1], system[\"EventCount\", 3]}"],

section["2.  Evolution"],

text["Every edge of every state is a place the subdivision rule fits, so each generation has one \
more edge to work on than the last:"],

code["Table[{g, system[\"StateCount\", g], system[\"EventCount\", g]}, {g, 0, 4}]"],

text["The states, grouped by generation:"],

code["system[\"States\", 2]"],

text["An event says which rule fired, which state it acted on, which it made, and what it \
touched. \"Data\" is the event HypergraphRuleApply gave, so everything from the rewriting layer \
is still there:"],

code["KeyDrop[First @ system[\"EventList\", 1], {\"Rule\", \"Data\"}]"],

subsection["The evolution is a tree, not a graph of states"],

text["A state reached two ways appears twice. Reversing an edge, back and forth:"],

code["flip = HypergraphRewritingSystem[{{1, 2}} -> {{2, 1}}, Hypergraph[{{1, 2}}]];\n\
Table[flip[\"StateCount\", g], {g, 0, 3}]"],

text["Four occurrences, of what is really one state reversed and reversed again. StatesGraph is \
where they are merged:"],

code["{flip[\"EvolutionGraph\", 3], flip[\"StatesGraph\", 3]}"],

note["Keeping the occurrences apart is what the causal reading needs: which elements a state is \
made of depends on how it was reached, and two events can only be causally related when one lies \
on a path through the other. Merging first would throw away exactly the information the causal \
graph is built from. StatesGraph merges for display, on top of the tree, and by default up to \
canonical form \[LongDash] a rewrite names the vertices it creates after what is already there, \
so two branches reaching the same shape rarely reach it under the same names. Setting \
\"CanonicalizeStates\" -> False asks for the states as written."],

code["{VertexCount @ flip[\"StatesGraph\", 3],\n \
 VertexCount @ flip[\"StatesGraph\", 3, \"CanonicalizeStates\" -> False]}"],

section["3.  What an event touched"],

text["The causal question is about elements: which ones an event took, and which it left \
behind. An event reports both, as tokens:"],

code["KeyTake[First @ system[\"EventList\", 1],\n \
 {\"ConsumedEdges\", \"CreatedEdges\", \"MatchedVertices\", \"CreatedVertices\", \"DestroyedVertices\"}]"],

note["A token is a counter, minted when an element is created and carried unchanged for as long \
as that element survives. Elements are not followed by value, because an edge is an element of a \
multiset: two edges that look the same are still two elements, and a vertex identifier freed by \
one event may be handed out again by a later one. Following values instead of tokens would make \
a hypergraph with a repeated edge report dependences that are not there."],

text["Two identical edges give two events, and the two consume different elements even though \
nothing tells the edges apart:"],

code["HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}, {1, 2}}]][\"EventList\", 1] //\n \
 Map[#[\"ConsumedEdges\"] &]"],

note["What makes that work is \"EdgeOrigins\" on the underlying event, which says for each edge \
of the result which host edge it carries over from, or None where the rule made it. The \
Hypergraph constructor sorts the edges, so a position in the result says nothing on its own; \
without that record an edge could not be followed across a rewrite at all."],

section["4.  Causal dependence, and the option"],

text["Two events are causally related when the elements they touch overlap. What counts as an \
element is the option \"CausalDependence\":"],

code["$CausalDependenceTypes"],

text["Take a path of two edges and subdivide, twice:"],

code["twoEdges = HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}, {2, 3}}]];\n\
twoEdges[\"EventCount\", 2]"],

code["pairsOf[l_] := {#[\"From\"], #[\"To\"]} & /@ l;\n\
byType[t_] := pairsOf @ twoEdges[\"CausalEdges\", 2, \"CausalDependence\" -> t];\n\
AssociationMap[byType, $CausalDependenceTypes]"],

text["Four pairs by edges, six by elements. The two that only elements sees:"],

code["Complement[byType[\"Elements\"], byType[\"Edges\"]]"],

text["Those are the ones whose matches share no edge at all \[LongDash] they meet at the vertex \
between the two halves of the path:"],

code["Select[twoEdges[\"CausalEdges\", 2], #[\"SharedEdges\"] === {} &]"],

note["This is the design point. The default, \"Elements\", is the weakest reading: two rule \
applications are related as soon as their matched patterns are superposed anywhere, vertex or \
edge. It is what the reference paclets take. \"Edges\" is the strictest: two events are related \
only when one consumed a whole edge the other made. Neither is more correct than the other. \
Subdividing one edge of a path and then the other really does leave the second event free of \
anything the first produced, and really does have the two matches meeting at a vertex; whether \
that vertex is a channel along which one event can affect the other is a fact about what the \
hypergraph is standing for."],

text["The three readings stand in a fixed relation on every system, which is why there are three \
and not two:"],

code["{SubsetQ[byType[\"Elements\"], byType[\"Edges\"]],\n \
 SubsetQ[byType[\"Elements\"], byType[\"Vertices\"]],\n \
 Sort @ byType[\"Elements\"] === Sort @ Union[byType[\"Edges\"], byType[\"Vertices\"]]}"],

subsection["What a causal edge reports"],

text["Not just that two events are related, but which elements they had in common:"],

code["First @ twoEdges[\"CausalEdges\", 2]"],

text["And the relation as a graph, one vertex per event, numbered as in \"EventList\":"],

code["{twoEdges[\"CausalGraph\", 2],\n \
 twoEdges[\"CausalGraph\", 2, \"CausalDependence\" -> \"Edges\"]}"],

note["Dependence is only asked about between events on one path: the later one must act on the \
state the earlier produced, or on a descendant of it. Two events on different branches of a \
multiway evolution never meet, whatever elements they appear to have in common \[LongDash] they \
are two things that could have happened, not two things that did. At one generation from a \
branching state there are therefore no causal edges at all."],

code["pairsOf @ twoEdges[\"CausalEdges\", 1]"],

section["5.  The graphs"],

text["States alone, as the tree and as the merged graph; the states one step could have reached; \
the events and their dependence; and both together:"],

code["twoEdges[\"EvolutionGraph\", 2]"],

code["twoEdges[\"StatesGraph\", 2]"],

code["twoEdges[\"BranchialGraph\", 1]"],

code["twoEdges[\"EvolutionCausalGraph\", 2]"],

text["Every vertex carries the state or the event it stands for as a tooltip, so a graph can be \
read by pointing at it."],

section["6.  Systems from the reference notebooks"],

text["The rules of reference/Chemoids.wl, evolved. Section 5 of RewritingBasics.nb ran them a \
step at a time by hand; a rewriting system is what that was standing in for."],

subsection["Composition along a chain"],

code["comp = HypergraphRule[\n \
 {Edge[{1, 2}, \"Directed\", x], Edge[{2, 3}, \"Directed\", y]},\n \
 {Edge[{1, 3}, \"Directed\", Row[{\"(\", x, y, \")\"}]]}];\n\
chain4 = Hypergraph[{Edge[{1, 2}, \"Directed\", a], Edge[{2, 3}, \"Directed\", b],\n \
                    Edge[{3, 4}, \"Directed\", c], Edge[{4, 5}, \"Directed\", d]}];\n\
chainSys = HypergraphRewritingSystem[comp, chain4]"],

text["Three places to compose, then two from each, then one:"],

code["Table[chainSys[\"EventCount\", g], {g, 0, 3}]"],

text["Carried to the end, the five ways of bracketing four arrows:"],

code["DeleteDuplicates @ Select[\n \
 EdgeLabel /@ EdgeList[#[\"Data\", \"Hypergraph\"]] & /@ chainSys[\"EventList\", 3],\n \
 Length[#] === 1 &]"],

text["And here the option earns its keep. Composing the first pair and composing the last pair \
are two events that share no arrow:"],

code["Complement[\n \
 pairsOf @ chainSys[\"CausalEdges\", 2],\n \
 pairsOf @ chainSys[\"CausalEdges\", 2, \"CausalDependence\" -> \"Edges\"]]"],

code["Select[chainSys[\"CausalEdges\", 2], #[\"SharedEdges\"] === {} &]"],

note["(a b) and (c d) are composed out of disjoint pairs of arrows, so by edges they are \
independent and the causal graph leaves them unrelated. By elements they are related, because \
both matches touch the object in the middle of the chain. For a category the second reading is \
arguably the honest one \[LongDash] the composites meet at that object and can be composed \
further through it \[LongDash] while for a system where the arrows are the only carriers of \
influence the first is. The rewriting does not decide; the option lets the model decide."],

code["{chainSys[\"CausalGraph\", 2],\n \
 chainSys[\"CausalGraph\", 2, \"CausalDependence\" -> \"Edges\"]}"],

subsection["Zig-zag composition"],

code["zig = HypergraphRule[\n \
 {Edge[{1, 2}, \"Directed\", x], Edge[{3, 2}, \"Directed\", y], Edge[{3, 4}, \"Directed\", z]},\n \
 {Edge[{1, 4}, \"Directed\", Row[{\"(\", x, y, z, \")\"}]]}];\n\
long = Hypergraph[{Edge[{1, 2}, \"Directed\", a], Edge[{3, 2}, \"Directed\", b],\n \
                  Edge[{3, 4}, \"Directed\", c], Edge[{5, 4}, \"Directed\", d],\n \
                  Edge[{5, 6}, \"Directed\", e]}];\n\
zigSys = HypergraphRewritingSystem[zig, long];\n\
{zigSys[\"EventCount\", 1], zigSys[\"EventCount\", 2]}"],

text["The three bracketings of heap associativity, reached in two steps:"],

code["DeleteDuplicates @ Select[\n \
 EdgeLabel /@ EdgeList[#[\"Data\", \"Hypergraph\"]] & /@ zigSys[\"EventList\", 2],\n \
 Length[#] === 1 &]"],

text["Here each second step consumes the arrow the first one made, so the two readings agree:"],

code["pairsOf @ zigSys[\"CausalEdges\", 2] ===\n \
 pairsOf @ zigSys[\"CausalEdges\", 2, \"CausalDependence\" -> \"Edges\"]"],

code["zigSys[\"EvolutionCausalGraph\", 2]"],

subsection["Two rules at once"],

text["A system may hold several rules, and each event records which of them fired:"],

code["unit = HypergraphRule[\n \
 Hypergraph[{Vertex[1, p]}, {}],\n \
 Hypergraph[{Vertex[1, p], Vertex[2, p]}, {Edge[{1, 2}, \"Directed\", Subscript[\"1\", p]]}]];\n\
arrow = Hypergraph[{Vertex[1, r], Vertex[2, s]}, {Edge[{1, 2}, \"Directed\", f]}];\n\
catSys = HypergraphRewritingSystem[{unit, comp}, arrow];\n\
Tally[#[\"RuleIndex\"] & /@ catSys[\"EventList\", 2]]"],

text["Eight units and one composition: of the two arrows a unit can build, only the one at the \
target of f is composable with it. By edges, that composition depends on exactly one earlier \
event:"],

code["pairsOf @ catSys[\"CausalEdges\", 2, \"CausalDependence\" -> \"Edges\"]"],

code["pairsOf @ catSys[\"CausalEdges\", 2]"],

section["7.  Scope and limits"],

text["The evolution is exhaustive: every rule in every way it fits, on every state reached. That \
is what a multiway system is, and it is also how it gets expensive. The number of states can \
grow faster than expected, so \"MaxStates\" \[LongDash] 2048 by default \[LongDash] stops the \
run and says so rather than going on."],

note["A rule holds the symbols it was written with, including the ones serving as label \
variables. Giving one of those symbols a value later turns every copy of the rule into whatever \
the value is, and an expression holding itself is not a pleasant thing to evaluate. Keeping the \
variables of a rule clear of assignments is not a style preference here; x = something after the \
fact will break rules written months earlier."],

text["What is not here: the branchial structure beyond a single generation, causal invariance as \
a test rather than something to look at, and the token-event graph in the form the \
Multicomputation paclet gives it. Those are all built out of what is above and none of them \
needs a new idea."],

text["The syntax follows the reference paclets: where they write MultiwaySystem[rules, \
init][\"CausalGraph\", n], this writes HypergraphRewritingSystem[rules, init][\"CausalGraph\", \
n], with the same property names. What differs is the option: their causal dependence is fixed \
at the weakest reading, and here it is chosen."],

text["The file tests/CausalityExamples.wls runs the examples in this notebook as assertions, \
plus invariants checked over a hundred and twenty generated systems: that a causal edge always \
runs from an earlier generation to a later one, that it never reports an overlap it does not \
have, and that the three readings stand in the relation section 4 claims."],

text["Next: the plex diagrams that tie rewriting to hypermatrix algebra."]

}];

nb = Notebook[cells,
    StyleDefinitions -> "Default.nb",
    WindowSize -> {900, 800},
    InitializationCellEvaluation -> True,
    InitializationCellWarning -> False
];

Quiet @ CreateDirectory[DirectoryName[target]];
Export[target, nb, "NB"];

Print["wrote ", target];
Print["cells: ", Length[cells], "  input: ", Count[cells, Cell[_, "Input", ___]],
      "  output: ", Count[cells, Cell[_, "Output", ___]]];
Print["failed: ", Cases[$log, {s_, $evalFailed} :> s]];

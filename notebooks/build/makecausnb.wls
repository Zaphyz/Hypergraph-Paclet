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
          zig, zigSys, long, pairsOf, byType, strict, flip, labelledStart, onlyA]; Nothing),

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

text["Six graphs come out of one evolution, each answering a different question. All of them draw \
their states and events as pictures, carry the object they stand for as a tooltip, and take the \
options of Graph on top of the options of the system."],

subsection["EvolutionGraph \[LongDash] the tree of what happened"],

text["One vertex per state occurrence, one edge per event. Nothing is merged, so a state reached \
two ways appears twice:"],

code["twoEdges[\"EvolutionGraph\", 2, ImageSize -> 460, \"PanelSize\" -> 80]"],

subsection["StatesGraph \[LongDash] the same states, merged"],

text["Equal states become one vertex, which is what closes the tree into a graph:"],

code["twoEdges[\"StatesGraph\", 2, ImageSize -> 460, \"PanelSize\" -> 80]"],

text["What counts as equal is \"CanonicalizeStates\". By default it is up to canonical form, so \
two states differing only in what their vertices are called are one. Comparing them as written \
merges far less:"],

code["{VertexCount @ twoEdges[\"StatesGraph\", 2, \"StateRendering\" -> False],\n \
 VertexCount @ twoEdges[\"StatesGraph\", 2, \"CanonicalizeStates\" -> False,\n \
    \"StateRendering\" -> False]}"],

note["A rewrite names the vertices it creates after what is already there, so two branches that \
reach the same shape rarely reach it under the same names. Without canonicalizing, a states graph \
of a growing rule barely merges anything and is just the tree again."],

subsection["BranchialGraph \[LongDash] what one step could have done"],

text["The states of a single generation, joined when they came from the same state:"],

code["twoEdges[\"BranchialGraph\", 1, ImageSize -> 400, \"PanelSize\" -> 90]"],

code["twoEdges[\"BranchialGraph\", 2, ImageSize -> 460, \"PanelSize\" -> 60]"],

subsection["CausalGraph \[LongDash] which event depended on which"],

text["One vertex per event, drawn as the match highlighted in what was there and the creations \
highlighted in what came of it:"],

code["twoEdges[\"CausalGraph\", 2, ImageSize -> 540, \"PanelSize\" -> 70]"],

text["\"TransitiveReduction\" is on by default, leaving only the edges the order does not already imply. Two generations of this rule happen to have nothing implied; three have plenty:"],

code["Table[{n,
    EdgeCount @ twoEdges[\"CausalGraph\", n, \"EventRendering\" -> False],
    EdgeCount @ twoEdges[\"CausalGraph\", n, \"TransitiveReduction\" -> False,
       \"EventRendering\" -> False]}, {n, 2, 3}]"],

note["Thirty edges against forty-eight at three generations. The eighteen the reduction removes are the ones saying that an event depends on something it already depends on through another event, which is true and adds nothing to the picture."],

subsection["EvolutionCausalGraph \[LongDash] states and events together"],

text["State to event to state, with the causal edges laid over them in red. Events are the wide \
panels, states the square ones:"],

code["twoEdges[\"EvolutionCausalGraph\", 2, ImageSize -> 560, \"PanelSize\" -> 55]"],

subsection["TokenEventGraph \[LongDash] the elements themselves"],

text["An event takes the elements it consumed and gives the ones it created, so a causal \
dependence becomes a path through an element rather than something inferred:"],

code["twoEdges[\"TokenEventGraph\", 1, ImageSize -> 520, \"PanelSize\" -> 55]"],

text["Which elements appear is the same \"CausalDependence\" option. Under \"Edges\" the vertex \
tokens are gone, so the dependence that ran only through a shared vertex has nothing to run along:"],

code["twoEdges[\"TokenEventGraph\", 1, \"CausalDependence\" -> \"Edges\", ImageSize -> 440,\n \
 \"PanelSize\" -> 55]"],

note["This is section 4's contrast made visible. Two events joined in the token-event graph under \
\"Elements\" but not under \"Edges\" are exactly the pairs whose matches meet at a vertex and \
nowhere else."],

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

code["{chainSys[\"CausalGraph\", 2, ImageSize -> 340, \"PanelSize\" -> 45],\n \
 chainSys[\"CausalGraph\", 2, \"CausalDependence\" -> \"Edges\",\n \
   ImageSize -> 340, \"PanelSize\" -> 45]}"],

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

section["7.  Seeing it"],

text["Everything above has been counts and lists of numbered events. A graph of a rewriting \
system is meant to be looked at, and every graph-valued property draws its states and its events \
as pictures."],

subsection["A rule, and an event"],

text["A rule is its input beside its output, with the vertices the two sides share in the same \
places, so that what the rule does to them can be read off rather than worked out. The vertex it \
creates is highlighted:"],

code["HypergraphRulePlot[subdivide, \"PanelSize\" -> 130]"],

text["An event is the same idea applied to a particular match: what it acted on with the match \
highlighted, and what it made with the creations highlighted, both on the one layout:"],

code["HypergraphEventPlot[First @ subdivide[Hypergraph[{{1, 2}, {2, 3}}]], \"PanelSize\" -> 130]"],

text["One picture per way the rule fits:"],

code["HypergraphEventPlot[subdivide, Hypergraph[{{1, 2}, {2, 3}}], \"PanelSize\" -> 90]"],

note["The two halves share a layout because otherwise a rewrite that moved nothing would look \
like it had moved everything: two spring embeddings of nearly the same hypergraph come out \
differently, and the eye reads the difference as the event. A vertex the event created has no \
place in the old layout, so it is settled among its neighbours \[LongDash] which puts a vertex \
inserted into an edge between that edge's two ends, where it belongs."],

note["Which edges of the result to highlight is read off \"EdgeOrigins\" rather than found by \
looking for edges equal to \"NewEdges\". In a multiset two edges of equal value are still two \
edges, and a rule that creates an edge the hypergraph already had would otherwise highlight the \
wrong one."],

subsection["The graphs"],

text["The states of an evolution, each drawn:"],

code["HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}}]][\"EvolutionGraph\", 2,\n \
 ImageSize -> 420, \"PanelSize\" -> 110]"],

text["And the causal graph, whose vertices are the events:"],

code["twoEdges[\"CausalGraph\", 2, ImageSize -> 560, \"PanelSize\" -> 70]"],

text["The same relation under the stricter reading has fewer edges, and the difference is now \
something to look at rather than a list of pairs:"],

code["twoEdges[\"CausalGraph\", 2, \"CausalDependence\" -> \"Edges\", ImageSize -> 560,\n \
 \"PanelSize\" -> 70]"],

note["The colours are those of the Wolfram Physics Project, which is what the reference paclets \
draw these graphs in: states in blue, events in amber, causal edges in red, branchial edges in \
magenta. The values are copied into the source rather than fetched from the style resource \
function the reference calls, because that is a network call on a machine that has not cached it, \
and a package that cannot be loaded offline is not worth the exactness."],

subsection["Elements as vertices"],

text["The token-event graph puts the elements themselves in the picture: an event takes the ones \
it consumed and gives the ones it created, and a causal dependence is then a path rather than \
something inferred."],

code["twoEdges[\"TokenEventGraph\", 1, ImageSize -> 520, \"PanelSize\" -> 60]"],

note["Which elements appear is the same \"CausalDependence\" option as everywhere else. Under \
\"Edges\" only edge tokens are drawn, so a dependence that ran only through a shared vertex has \
nothing to run along and the two events stand apart \[LongDash] the contrast of section 4, made \
visible."],

subsection["When not to draw"],

text["Drawing a state means laying out a hypergraph, and drawing an event means laying out two. \
Past a couple of dozen that is a wait nobody asked for, so a graph falls back to plain vertices \
and says which option to raise:"],

code["Short[Quiet @ HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}}]][\"EvolutionGraph\", 4],\n \
 3]"],

text["\"StateRendering\" and \"EventRendering\" settle the question either way, and \
\"MaxDrawnVertices\" moves the line."],

subsection["Keeping track of the options"],

text["There are enough options now that they are kept in one registry, with their defaults. Each of them is shown at work at the end of this section:"],

code["$HypergraphRewritingOptions"],

text["Every graph-valued property takes those together with the options of Graph, so ImageSize, \
VertexSize, GraphLayout and the rest all work. A system can also hold the options that describe \
it, rather than having them retyped on every call:"],

code["strict = HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}, {2, 3}}],\n \
 \"CausalDependence\" -> \"Edges\", \"PanelSize\" -> 70];\n\
{Length @ strict[\"CausalEdges\", 2],\n \
 Length @ strict[\"CausalEdges\", 2, \"CausalDependence\" -> \"Elements\"]}"],

text["The held option is used without being repeated, and an option given to a property still \
wins over it. What is in force for a particular system:"],

code["FilterRules[strict[\"Options\"], {\"CausalDependence\", \"PanelSize\", \"MaxDrawnVertices\"}]"],

subsection["Every option, and what it changes"],

text["The registry, in the three groups it falls into. Four options govern the evolution, three \
govern what is made of it, and five govern how it is drawn:"],

code["Column[{\n \
 Row[{\"evolution:  \", Keys @ Association @ Take[$HypergraphRewritingOptions, 4]}],\n \
 Row[{\"causality:  \", Keys @ Association @ Take[$HypergraphRewritingOptions, {5, 7}]}],\n \
 Row[{\"drawing:    \", Keys @ Association @ Take[$HypergraphRewritingOptions, -5]}]}]"],

text["\"MaxStates\" stops an evolution that is growing faster than expected rather than running on, and gives HypergraphRewritingSystem::trunc saying which generation it stopped in. Six generations of this rule would be tens of thousands of states; the limit cuts it off at forty, against ten at three generations:"],

code["{Quiet @ HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}}]][
     \"StateCount\", 6, \"MaxStates\" -> 40],
  HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}}]][\"StateCount\", 3]}"],

text["\"Labeled\", \"LabelPatterns\" and \"DistinctVertices\" are the matching options of \
HypergraphRuleApply, passed through to every rule application the evolution makes:"],

code["labelledStart = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];\n\
onlyA = HypergraphRewritingSystem[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"a\"]}, {{2, 1}}],\n \
                                 labelledStart];\n\
{onlyA[\"EventCount\", 1], onlyA[\"EventCount\", 1, \"Labeled\" -> False]}"],

text["\"PanelSize\" is how many pixels wide each picture is drawn, and \
\"HypergraphPlotOptions\" is passed to every hypergraph inside a vertex \[LongDash] which is how \
to get the vertex names back, or to restyle what is drawn:"],

code["twoEdges[\"EvolutionGraph\", 1, ImageSize -> 380, \"PanelSize\" -> 130,\n \
 \"HypergraphPlotOptions\" -> {VertexLabels -> \"Name\"}]"],

code["twoEdges[\"EvolutionGraph\", 1, ImageSize -> 380, \"PanelSize\" -> 110,\n \
 \"HypergraphPlotOptions\" -> {EdgeStyle -> Directive[Thick, Darker[Green]],\n \
                            VertexStyle -> Darker[Green], AspectRatio -> Automatic}]"],

text["Because the options of Graph come too, the ordinary graph knobs all work:"],

code["twoEdges[\"CausalGraph\", 2, \"EventRendering\" -> False, ImageSize -> 300,\n \
 VertexLabels -> Automatic, VertexSize -> Medium, GraphLayout -> \"SpringEmbedding\"]"],

subsection["Holding options on the system"],

text["A system can carry the options that describe it, so they are not retyped on every call:"],

code["strict = HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}, {2, 3}}],\n \
 \"CausalDependence\" -> \"Edges\", \"PanelSize\" -> 70];\nstrict"],

code["{Length @ strict[\"CausalEdges\", 2],\n \
 Length @ strict[\"CausalEdges\", 2, \"CausalDependence\" -> \"Elements\"]}"],

text["Four pairs by the held option, six when the call overrides it. What is in force for a \
system, the ones it holds and the defaults for the rest:"],

code["FilterRules[strict[\"Options\"],\n \
 {\"CausalDependence\", \"PanelSize\", \"MaxDrawnVertices\", \"TransitiveReduction\"}]"],

text["The two-argument form is the same object with no options held:"],

code["HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}}]] ===\n \
 HypergraphRewritingSystem[subdivide, Hypergraph[{{1, 2}}], {}]"],

note["Options are held on the system rather than only passed to properties because there are \
twelve of them and most describe the system rather than the question being asked of it. Which \
reading of causal dependence a model uses is a fact about the model; how many generations to \
evolve is a fact about the question."],

subsection["A labelled system, drawn"],

text["Nothing about the drawing is particular to unlabelled hypergraphs. The composition rule of \
section 6, with the labels shown:"],

code["chainSys[\"EvolutionGraph\", 1, ImageSize -> 620, \"PanelSize\" -> 150,\n \
 \"HypergraphPlotOptions\" -> {EdgeLabels -> Automatic, AspectRatio -> Automatic}]"],

text["And its causal graph, where the two events that meet only at an object are the pair the \
stricter reading drops:"],

code["chainSys[\"CausalGraph\", 2, ImageSize -> 900, \"PanelSize\" -> 110,\n \
 \"HypergraphPlotOptions\" -> {EdgeLabels -> Automatic, AspectRatio -> Automatic}]"],

note["A panel is squared by default, which is the shape the insets are sized for. A hypergraph \
that lays out as a line \[LongDash] a chain of arrows is the usual case \[LongDash] then sits as \
a thin band in the middle of a lot of white, and at small panel sizes its labels become \
unreadable. AspectRatio -> Automatic in \"HypergraphPlotOptions\" lets the panel take the shape \
of what is in it, which is what makes these two legible."],

section["8.  Scope and limits"],

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

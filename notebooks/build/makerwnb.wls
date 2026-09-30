#!/usr/bin/env wolframscript

(* Builds notebooks/RewritingBasics.nb. Each input cell is evaluated here in the kernel and its
   result attached as a StandardForm Output cell: NotebookEvaluate from a script formats results
   in OutputForm, which loses every graphic and every summary box. *)

$ProgressReporting = False;
(* The repository root, from where this script sits: <root>/notebooks/build/. *)
(* Forward slashes: $root is spliced into a string that is then parsed, and a Windows
   backslash there is read as a string escape. *)
$root = StringReplace[ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]],
                      "\\" -> "/"];
target = FileNameJoin[{$root, "notebooks", "RewritingBasics.nb"}];

tidy[s_String] := StringReplace[
    StringReplace[s, {"\r\n" -> " ", "\r" -> " ", "\n" -> " "}],
    RegularExpression[" {2,}"] -> " "
];

(* The helpers must not share a name with any variable an example assigns to: the examples run
   in this same kernel, and a clobbered helper fails silently -- a subsection header becomes
   Missing[...] and renders as an empty box. The subdivision rule below is called sub, so the
   subsection helper is subsection. *)
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

title["Hypergraph Rewriting"],
subtitle["WolframInstitute`Hypergraphs` \[LongDash] rules, matches and events"],

text["A rewrite rule is a pair of hypergraphs. Applying it to a hypergraph means finding the ways \
its input sits inside that hypergraph, and for each of them replacing what was matched by the \
output. This notebook is the tour of that: what a rule is, how matching works and what it depends \
on, and what an application gives back. Everything below is live output from src/."],

text["The machinery around rewriting \[LongDash] the generations of states a rule produces, \
the causal graph of its events, the conditions under which two events depend on one another \
\[LongDash] is built on top of this, out of the events described in section 3. It is in \
CausalityBasics.nb."],

section["Loading the package"],

text["The cell below is an initialization cell, which the front end evaluates when this notebook \
is opened. By the time you read this the package is already loaded."],

initCode["PacletDirectoryLoad[ParentDirectory[NotebookDirectory[]]];\n\
Needs[\"WolframInstitute`Hypergraphs`\"]",
     "PacletDirectoryLoad[\"" <> $root <> "\"];Needs[\"WolframInstitute`Hypergraphs`\"]"],

section["1.  A rule is a pair of hypergraphs"],

text["HypergraphRule takes an input and an output. Either may be written as anything Hypergraph \
accepts, so a rule can be given as plainly as a pair of edge lists:"],

code["sub = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}]"],

text["This is the rule that subdivides an edge: where there was one edge from 1 to 2, there will \
be two, through a vertex in the middle. The two sides are hypergraphs like any other, and draw \
themselves:"],

code["{sub[\"Input\"], sub[\"Output\"]}"],

text["Vertices correspond between the two sides by name. So the rule can be read off: 1 and 2 \
appear on both sides and are kept, 3 appears only on the right and is therefore created."],

code["{sub[\"SharedVertices\"], sub[\"NewVertices\"], sub[\"DeletedVertices\"]}"],

note["A name on both sides means the same vertex, and nothing else in the rule says so. That is \
what makes a rule readable as a local surgery: the shared vertices are the seam along which the \
output is sewn in where the input was cut out."],

text["A vertex mentioned only on the left is destroyed:"],

code["HypergraphRule[{{1, 2}}, {{1}}][\"DeletedVertices\"]"],

text["Rules are hypergraphs all the way down, so a side may carry symmetry types and labels:"],

code["HypergraphRule[{Edge[{1, 2, 3}, \"Unordered\", \"triple\"]}, {Edge[{1, 2}, \"Directed\"]}][\"Input\"]"],

section["2.  Matching"],

text["Before anything is rewritten, the input has to be found. HypergraphRuleMatches gives the \
ways it sits inside a hypergraph, and nothing else:"],

code["chain = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]"],

code["HypergraphRuleMatches[sub, chain]"],

text["Two matches, because the single edge of the input can lie on either edge of the chain. Each \
match says which positions of EdgeList it used and how the input's vertices correspond to the \
hypergraph's."],

note["Asking for the matches without rewriting is the way to see why a rule applies as often as \
it does. When a rule produces more events than expected, it is almost always the matching that is \
surprising rather than the rewriting."],

subsection["What a rule knows about itself"],

text["A rule answers for its two sides and for what it does to vertices, read off by identifier:"],

code["sub[\"Properties\"]"],

code["AssociationMap[sub, {\"SharedVertices\", \"NewVertices\", \"DeletedVertices\", \"Signature\"}]"],

text["\"Signature\" is the shape of the rule: how many edges and vertices on each side. A rule \
that contracts rather than grows has it the other way round:"],

code["contract = HypergraphRule[{{1, 2}, {2, 3}}, {{1, 3}}];\n\
AssociationMap[contract, {\"SharedVertices\", \"NewVertices\", \"DeletedVertices\", \"Signature\"}]"],

note["Vertices correspond between the two sides by identifier, so a name on both sides is the \
same vertex and a name on the output alone is a new one. That is the whole of the convention, \
and these three lists are it applied."],

subsection["The matching options"],

text["Three options govern what counts as a match. \"Labeled\" -> False puts labels aside \
altogether, so a rule written for skeletons applies to a labelled hypergraph:"],

code["labelledPair = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}];\n\
{Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"c\"]}, {}], labelledPair],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"c\"]}, {}], labelledPair,\n \
    \"Labeled\" -> False]}"],

text["\"LabelPatterns\" -> False reads every label literally, so a bare symbol stops being a \
variable. Section 4 is about what that option turns on:"],

code["{Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", w]}, {}], labelledPair],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", w]}, {}], labelledPair,\n \
    \"LabelPatterns\" -> False]}"],

text["\"DistinctVertices\" -> False lets two vertices of the input fall on one vertex of the \
hypergraph, which is what the next subsection is about:"],

code["{Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}, {2, 3}}, {}], Hypergraph[{{1, 2}}]],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}, {2, 3}}, {}], Hypergraph[{{1, 2}}],\n \
    \"DistinctVertices\" -> False]}"],

text["A match on its own says only where it sits: which positions of EdgeList it used, how the \
input's vertices correspond, and what its label variables were bound to."],

code["Keys @ First @ HypergraphRuleMatches[sub, chain]"],

subsection["Symmetry decides how many ways a pattern fits"],

text["A pattern edge and the edge it lies on must have the same symmetry type, and the pattern is \
matched over the orderings that type allows. A directed edge of arity three fits one way, a \
cyclic one three ways, an unordered one all six:"],

code["Table[\n \
 Length @ HypergraphRuleMatches[\n \
   HypergraphRule[{Edge[{1, 2, 3}, s]}, {}], Hypergraph[{Edge[{1, 2, 3}, s]}]],\n \
 {s, {\"Directed\", \"Cyclic\", \"Unordered\"}}]"],

text["And the types have to agree, so a directed pattern finds nothing in an unordered edge:"],

code["HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\"]}, {}],\n \
 Hypergraph[{Edge[{1, 2}, \"Unordered\"]}]]"],

note["Those six matches of an unordered triple are genuinely six, not one reported six times: \
they differ in which vertex of the hypergraph each vertex of the rule corresponds to, and an \
output that treats its vertices differently will produce six different results. When the output \
is symmetric too, the six results coincide, and the multiplicity is then something to divide out \
rather than something wrong."],

subsection["Two vertices of a rule do not fold onto one"],

text["By default distinct vertices of the input match distinct vertices of the hypergraph, so a \
two-vertex pattern does not fit a self-loop:"],

code["{HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], Hypergraph[{Edge[{1, 1}]}]],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], Hypergraph[{Edge[{1, 1}]}],\n \
    \"DistinctVertices\" -> False]}"],

note["This follows the reference paclet, which imposes the same by default. It is worth knowing \
that it is a choice: in the Wolfram model a rule's vertices need not be distinct, and \
\"DistinctVertices\" -> False is that convention."],

subsection["An edge is used once"],

text["Each edge of the hypergraph is used by at most one edge of the input, so a rule wanting two \
edges wants two of them:"],

code["{HypergraphRuleMatches[HypergraphRule[{{1, 2}, {1, 2}}, {}], Hypergraph[{Edge[{1, 2}]}]],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}, {1, 2}}, {}],\n \
    Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}]]}"],

section["3.  Applying a rule: events"],

text["Applying a rule gives one event for every match. An event is an association: it says what \
the hypergraph became, and also what was matched, what was created and what was destroyed."],

code["events = sub[chain];\nLength[events]"],

code["Keys @ First[events]"],

text["The two results, drawn. Each has subdivided one of the two edges:"],

code["#[\"Hypergraph\"] & /@ events"],

text["The first event, in full:"],

code["First[events]"],

note["An event is reported rather than just a result because a result on its own forgets how it \
came about. Which edges were consumed and which were produced is exactly what decides whether two \
events could have happened in either order, so the causal machinery to come needs all of it. \
Returning it now costs nothing and means that machinery will not have to match the rule a second \
time."],

text["rule[hg] and HypergraphRuleApply are the same thing, the first being the reference paclet's \
way of writing it:"],

code["sub[chain] === HypergraphRuleApply[sub, chain]"],

subsection["Every key of an event"],

text["An event is an association, and each of its keys answers one question. Taking a single \
event and going through them:"],

code["oneEvent = First @ sub[chain];\nKeys[oneEvent]"],

text["What it made, and what it acted on:"],

code["{oneEvent[\"Input\"], oneEvent[\"Hypergraph\"]}"],

text["Where it matched: the positions of EdgeList it covered, those edges themselves, and the \
vertices under them:"],

code["KeyTake[oneEvent, {\"MatchEdgePositions\", \"MatchEdges\", \"MatchVertices\"}]"],

text["What it created and what it destroyed:"],

code["KeyTake[oneEvent, {\"NewVertices\", \"NewEdges\", \"DeletedVertices\"}]"],

text["How the rule's own vertex names correspond to the hypergraph's, including the names it \
minted for the vertices it created:"],

code["oneEvent[\"RuleVertexMap\"]"],

text["Where each edge of the result came from: a host edge position, or None where the rule made \
it. This is the one key that has no counterpart in the reference paclet, and the causal \
machinery could not be written without it:"],

code["Thread[EdgeList[oneEvent[\"Hypergraph\"]] -> oneEvent[\"EdgeOrigins\"]]"],

text["What the label variables were bound to, and the rule itself:"],

code["{oneEvent[\"LabelBindings\"], oneEvent[\"Rule\"]}"],

note["The reference paclet's events carry \"Hypergraph\", \"MatchVertices\", \"MatchEdges\", \
\"MatchEdgePositions\", \"NewVertices\", \"NewEdges\", \"DeletedVertices\" and \
\"RuleVertexMap\", and a rule written for it reads the same here. The three extra keys are \
\"Input\", without which an event cannot be drawn; \"LabelBindings\", which is the label \
machinery of section 4; and \"EdgeOrigins\", which is what lets an element be followed across a \
rewrite."],

subsection["A rule that consumes two edges"],

text["The input may have several edges. This one contracts a path of two edges into one:"],

code["contract = HypergraphRule[{{1, 2}, {2, 3}}, {{1, 3}}]"],

code["path3 = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{3, 4}]}]"],

code["#[\"Hypergraph\"] & /@ contract[path3]"],

text["Two matches, on the first two edges and on the last two, and the middle vertex of each \
disappears with them:"],

code["{#[\"MatchEdgePositions\"], #[\"DeletedVertices\"]} & /@ contract[path3]"],

subsection["Creating and destroying"],

text["An output that mentions fewer vertices than the input destroys the difference:"],

code["del = HypergraphRule[{{1, 2}}, {{1}}];\nFirst[del[Hypergraph[{Edge[{1, 2}]}]]][\"Hypergraph\"]"],

text["\[Ellipsis] but only when nothing else is holding them. Here vertex 2 is in another edge, \
and stays:"],

code["First[del[Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]]][\"Hypergraph\"]"],

note["A rule rewrites what it matched. Reaching into an edge it did not match, to strike a vertex \
out of it, would not be local, so a vertex the rule drops but another edge still holds simply \
stays and is not reported destroyed. The reference paclet does remove it from those other edges; \
this is a deliberate difference."],

text["An empty output deletes the matched edges outright:"],

code["First[HypergraphRule[{{1, 2}}, {}][path3]][\"Hypergraph\"]"],

text["New vertices are named so as to miss everything already there:"],

code["{#[\"NewVertices\"], VertexList[#[\"Hypergraph\"]]} & @ First[sub[chain]]"],

note["Fresh symbols would do here, and are what the reference uses, but names chosen to avoid \
what is present make a rewrite reproducible from one session to the next. That matters as soon as \
states are compared, which is the whole of what a multiway system does."],

section["4.  Labels"],

text["Labels on a rule constrain what it applies to. A pattern carrying no label matches whatever \
it lies on, and one carrying a label matches only the same label, so labels only ever narrow a \
rule."],

code["labelled = Hypergraph[{Edge[{1, 2}, \"Directed\", \"a\"], Edge[{2, 3}, \"Directed\", \"b\"]}]"],

code["{Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {{2, 1}}], labelled],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"a\"]}, {{2, 1}}], labelled]}"],

text["The labelled pattern finds the edge carrying that label, and only it:"],

code["First @ HypergraphRuleMatches[\n \
 HypergraphRule[{Edge[{1, 2}, \"Directed\", \"a\"]}, {{2, 1}}], labelled]"],

text["A label nothing carries matches nothing, and \"Labeled\" -> False puts labels aside \
altogether:"],

code["{HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"c\"]}, {}], labelled],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"c\"]}, {}], labelled,\n \
    \"Labeled\" -> False]}"],

note["An unlabelled pattern being a wildcard is what makes a rule written for skeletons work \
unchanged on a labelled hypergraph. The alternative \[LongDash] requiring a pattern to be \
unlabelled in order to match an unlabelled edge \[LongDash] would mean writing two versions of \
every rule."],

text["The output's labels are carried onto the edges it makes, so a rule can label what it \
builds:"],

code["tag = HypergraphRule[{{1, 2}}, {Edge[{1, 3}, \"Directed\", \"first\"],\n \
                          Edge[{3, 2}, \"Directed\", \"second\"]}];\n\
First[tag[Hypergraph[{Edge[{1, 2}]}]]][\"Hypergraph\"]"],

code["EdgeLabel /@ EdgeList @ First[tag[Hypergraph[{Edge[{1, 2}]}]]][\"Hypergraph\"]"],

subsection["Labels as variables"],

text["So far the labels on a rule have been literals, narrowing what it applies to. They can do \
much more than that. A label on the input written as a bare symbol is a variable: it matches \
whatever label it is laid against and remembers it, and the output can then be named in terms of \
what was found."],

code["composable = Hypergraph[{Edge[{1, 2}, \"Directed\", f], Edge[{2, 3}, \"Directed\", g]}]"],

code["#[\"LabelBindings\"] & /@ HypergraphRuleMatches[\n \
 HypergraphRule[{Edge[{1, 2}, \"Directed\", x]}, {}], composable]"],

text["One match per edge, and in each the variable x has been bound to the label it found. The \
output is an expression in those variables. This rule composes two edges and names the result \
after the two it consumed:"],

code["compose = HypergraphRule[\n \
 {Edge[{1, 2}, \"Directed\", x], Edge[{2, 3}, \"Directed\", y]},\n \
 {Edge[{1, 3}, \"Directed\", Row[{\"{\", x, y, \"}\"}]]}]"],

code["First[compose[composable]][\"Hypergraph\"]"],

code["EdgeLabel /@ EdgeList @ First[compose[composable]][\"Hypergraph\"]"],

note["This is the rule the reference notebooks write for composition, and it is the reason labels \
are worth the trouble. Without variables a rule can only say what it applies to; with them it can \
carry information out of the match and into what it builds, and a rewriting system becomes a \
calculus on the labels rather than only on the shape."],

text["A variable used twice must find the same label both times, which is how a rule asks for two \
edges that agree:"],

code["twice = HypergraphRule[{Edge[{1, 2}, \"Directed\", x], Edge[{2, 3}, \"Directed\", x]}, {}];\n\
{HypergraphRuleMatches[twice, composable],\n \
 Length @ HypergraphRuleMatches[twice,\n \
    Hypergraph[{Edge[{1, 2}, \"Directed\", f], Edge[{2, 3}, \"Directed\", f]}]]}"],

subsection["What counts as a variable"],

text["A bare symbol is a variable. A string, a number, or any composite expression is a literal \
and matches only itself, which is why every labelled rule earlier in this notebook went on \
behaving as it did:"],

code["{Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", x]}, {}], composable],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", \"f\"]}, {}], composable]}"],

text["Verbatim asks for a symbol to be read literally, for a rule that wants to match the label f \
rather than bind to it; and \"LabelPatterns\" -> False reads every label literally:"],

code["{Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", Verbatim[f]]}, {}], composable],\n \
 Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, \"Directed\", x]}, {}], composable,\n \
    \"LabelPatterns\" -> False]}"],

note["Deciding that a bare symbol is a variable and everything else is a literal is a convention, \
not a necessity, and it was chosen by reading the reference notebooks: every label they write in \
a rule is a bare symbol meant as a variable, and every label they write in a state is one too. \
Requiring something like x_ would have been more explicit and would have made those notebooks \
unwriteable in this package without editing every rule."],

subsection["Vertex labels bind as well"],

text["Nothing about this is particular to edges. A rule whose whole input is a single labelled \
vertex binds that label, and creates something named after it:"],

code["objects = Hypergraph[{Vertex[1, r], Vertex[2, s]}, {Edge[{1, 2}, \"Directed\", u]}]"],

code["unit = HypergraphRule[\n \
 Hypergraph[{Vertex[1, p]}, {}],\n \
 Hypergraph[{Vertex[1, p], Vertex[2, p]},\n \
            {Edge[{1, 2}, \"Directed\", Subscript[\"1\", p]]}]]"],

code["#[\"LabelBindings\"] & /@ unit[objects]"],

code["EdgeLabel /@ EdgeList @ First[unit[objects]][\"Hypergraph\"]"],

text["Two events, one for each vertex of the state, and each builds an edge named for the vertex \
it was found at. The vertex the rule created carries that label too:"],

code["VertexLabel /@ VertexList @ First[unit[objects]][\"Hypergraph\"]"],

note["The input of this rule has no edges at all, only a labelled vertex. A vertex of the input \
lying in no edge is matched to each vertex not already taken in turn, and its label binds like any \
other \[LongDash] which is what makes a rule of this shape mean anything."],

subsection["An involution"],

text["A rule may take an edge apart and put it back the other way round, naming the result after \
what it undid. Reversing an edge and daggering its label:"],

code["dagger = HypergraphRule[\n \
 Hypergraph[{Vertex[1, p], Vertex[2, q]}, {Edge[{1, 2}, \"Directed\", x]}],\n \
 Hypergraph[{Vertex[1, p], Vertex[2, q]},\n \
            {Edge[{2, 1}, \"Directed\", Row[{\"(\", SuperDagger[x], \")\"}]]}]];\n\
First[dagger[objects]][\"Hypergraph\"]"],

text["Applying it twice returns the edge to where it began, with the label recording that it was \
daggered twice rather than reduced:"],

code["EdgeLabel /@ EdgeList @ First[dagger[First[dagger[objects]][\"Hypergraph\"]]][\"Hypergraph\"]"],

note["That the label is not simplified is the right behaviour here: rewriting builds the \
expression, and whether the two daggers cancel is a fact about the algebra being modelled rather \
than about the rewriting. A rule asserting the involution would be a second rule, and the two \
together are what the reference notebooks use to check it."],

text["A rule may also relabel. Where the output gives a vertex a label, that label is what the \
result carries; where it leaves one unlabelled, the hypergraph's own label stays:"],

code["swap = HypergraphRule[\n \
 Hypergraph[{Vertex[1, p], Vertex[2, q]}, {Edge[{1, 2}, \"Directed\", x]}],\n \
 Hypergraph[{Vertex[1, q], Vertex[2, p]}, {Edge[{1, 2}, \"Directed\", x]}]];\n\
VertexLabel /@ VertexList @ First[swap[objects]][\"Hypergraph\"]"],

section["5.  Rules from the reference notebooks"],

text["reference/Chemoids.wl builds small algebraic structures out of labelled rules: a \
composition, a unit, a dagger, an action, a zig-zag. Section 4 showed the mechanism those rules \
need; this section runs them and looks at the laws they were written to express. There is no \
multiway system here yet, so where the reference draws two steps of an evolution graph, this \
takes the second step by hand \[LongDash] applying the rule again to each state the first step \
produced."],

(* The examples below run in this same kernel, so clear the names first: a symbol that has picked
   up a value somewhere earlier stops being a pattern variable, and the rules go quiet. *)
(ClearAll[arrow, arrowComp, arrowUnit, chain2, dag, bothDaggered, action, pointed, zigzag, long,
          a, b, c, d, e, f, g, p, q, r, s, u, w, x, y, z, shown, styled, triangle, here, doubled]; Nothing),

subsection["Composition and the unit"],

text["Composition joins two edges meeting head to tail and names the result after them. The unit \
takes a single labelled vertex and grows an identity edge at it. Both are written the way the \
reference writes them:"],

code["arrowComp = HypergraphRule[\n \
 {Edge[{1, 2}, \"Directed\", x], Edge[{2, 3}, \"Directed\", y]},\n \
 {Edge[{1, 3}, \"Directed\", Row[{\"(\", x, y, \")\"}]]}];\n\
arrowUnit = HypergraphRule[\n \
 Hypergraph[{Vertex[1, p]}, {}],\n \
 Hypergraph[{Vertex[1, p], Vertex[2, p]},\n \
            {Edge[{1, 2}, \"Directed\", Subscript[\"1\", p]]}]];\n\
arrow = Hypergraph[{Vertex[1, r], Vertex[2, s]}, {Edge[{1, 2}, \"Directed\", f]}]"],

text["Both rules against that one state. Composition finds nothing \[LongDash] a single edge has \
nothing to compose with \[LongDash] and the unit fires once at each end:"],

code["EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@ Catenate[Through[{arrowUnit, arrowComp}[arrow]]]"],

text["Composing afterwards is the unit law. Of the two states the unit produced, only one goes \
further:"],

code["EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@\n \
 Catenate[arrowComp /@ (#[\"Hypergraph\"] & /@ arrowUnit[arrow])]"],

note["The unit built at the target of f leaves s, so it follows f head to tail and the pair \
composes; the one built at the source leaves r, which f also leaves, so the two share a source \
rather than meeting, and nothing composes. The orientation is doing the work. What comes out is \
the term (f Subscript[1, s]) \[LongDash] the left side of the unit law, not the law itself. \
Rewriting builds terms; asserting that this one equals f is a further rule, or the identification \
of states that a multiway system makes."],

subsection["The dagger is an antihomomorphism"],

text["The dagger rule reverses an edge and daggers its label. Composing first and then daggering \
gives the dagger of the composite:"],

code["chain2 = Hypergraph[{Edge[{1, 2}, \"Directed\", f], Edge[{2, 3}, \"Directed\", g]}];\n\
dag = HypergraphRule[\n \
 Hypergraph[{Vertex[1, p], Vertex[2, q]}, {Edge[{1, 2}, \"Directed\", x]}],\n \
 Hypergraph[{Vertex[1, p], Vertex[2, q]}, {Edge[{2, 1}, \"Directed\", SuperDagger[x]]}]];\n\
EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@\n \
 Catenate[dag /@ (#[\"Hypergraph\"] & /@ arrowComp[chain2])]"],

text["Daggering both edges first and then composing gives the composite of the daggers, in the \
other order:"],

code["bothDaggered = Select[\n \
 #[\"Hypergraph\"] & /@ Catenate[dag /@ (#[\"Hypergraph\"] & /@ dag[chain2])],\n \
 FreeQ[EdgeLabel /@ EdgeList[#], SuperDagger[SuperDagger[_]]] &];\n\
DeleteDuplicates[EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@ Catenate[arrowComp /@ bothDaggered]]"],

note["Two routes out of the same state reach two terms, and the antihomomorphism law is the claim \
that those terms are equal. Nothing here checks that claim; what the rewriting does is produce \
both sides of it from one state, which is what makes such a law something a causal or multiway \
analysis can be asked about rather than something asserted by hand. The Select drops the states \
where the same edge was daggered twice \[LongDash] those are the involution, not the \
antihomomorphism."],

subsection["An action"],

text["Not every edge in a rule has to be binary. The action rule consumes a unary edge \
\[LongDash] a point carrying an element \[LongDash] together with an arrow leaving that vertex, \
and leaves a point at the far end carrying the element acted on:"],

code["action = HypergraphRule[\n \
 {Edge[{1}, \"Directed\", a], Edge[{1, 2}, \"Directed\", x]},\n \
 {Edge[{2}, \"Directed\", Row[{x, \"[\", a, \"]\"}]]}];\n\
pointed = Hypergraph[{Edge[{1}, \"Directed\", u], Edge[{1, 2}, \"Directed\", f],\n \
                     Edge[{2, 3}, \"Directed\", g]}]"],

text["Acting twice moves the point along the chain, one arrow at a time:"],

code["EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@\n \
 Catenate[action /@ (#[\"Hypergraph\"] & /@ action[pointed])]"],

text["Composing the two arrows first and acting once gives the other side of the associativity \
law:"],

code["EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@\n \
 Catenate[action /@ (#[\"Hypergraph\"] & /@ arrowComp[pointed])]"],

note["The two results are g[f[u]] and (f g)[u]. Which of f and g ends up outermost is fixed by \
the composition convention, not by the rewriting: arrowComp writes the composite in diagram \
order, first arrow first, so acting through the composite applies f before g and the nested form \
has g outside. Writing the composite the other way round would swap them. Both rules match on the \
same structure either way."],

subsection["Zig-zag composition"],

text["The input of a rule need not be a directed path. This one matches three edges meeting at \
two shared vertices with the middle one reversed, and replaces them by a single edge across:"],

code["zigzag = HypergraphRule[\n \
 {Edge[{1, 2}, \"Directed\", x], Edge[{3, 2}, \"Directed\", y], Edge[{3, 4}, \"Directed\", z]},\n \
 {Edge[{1, 4}, \"Directed\", Row[{\"(\", x, y, z, \")\"}]]}];\n\
long = Hypergraph[{Edge[{1, 2}, \"Directed\", a], Edge[{3, 2}, \"Directed\", b],\n \
                  Edge[{3, 4}, \"Directed\", c], Edge[{5, 4}, \"Directed\", d],\n \
                  Edge[{5, 6}, \"Directed\", e]}]"],

text["Three ways to fit into that state, each consuming a different window of three edges:"],

code["EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@ zigzag[long]"],

text["Each of the three has one further match, and the three second steps are the three \
bracketings of the whole:"],

code["EdgeLabel /@ EdgeList[#[\"Hypergraph\"]] & /@\n \
 Catenate[zigzag /@ (#[\"Hypergraph\"] & /@ zigzag[long])]"],

note["((a b c) d e), (a b (c d e)) and (a (d c b) e): the reference calls the equality of these \
three heap associativity, and the middle one having its letters reversed is the point of it. \
Three distinct terms out of one state in two steps, from a rule carrying no labels of its own \
\[LongDash] every label in the result was carried out of the match by a variable. Which of the \
three a system reaches, and whether two of the steps could have been taken in either order, is \
what the causal machinery is for; section 6 of CausalityBasics.nb runs this same rule as a \
system."],

section["6.  Pictures"],

text["A rule is a pair of hypergraphs, and a hypergraph is a thing to look at. Everything in \
this section is what the causal machinery uses to draw an event as a vertex of a graph, available \
on its own."],

subsection["A rule"],

text["HypergraphRulePlot draws a rule as its input beside its output. The vertices the two sides \
share are drawn in the same places, so what the rule does to them can be read off rather than \
worked out:"],

code["HypergraphRulePlot[sub, \"PanelSize\" -> 140]"],

text["The vertex the rule creates is highlighted on the right. A rule that destroys one \
highlights it on the left:"],

code["HypergraphRulePlot[contract, \"PanelSize\" -> 140]"],

text["\"Highlight\" -> False leaves both sides plain, and \"SharedCoordinates\" -> False lays \
them out independently, which is worth seeing once to know what the sharing is doing:"],

code["GraphicsColumn[{\n \
 HypergraphRulePlot[sub, \"PanelSize\" -> 120, \"Highlight\" -> False],\n \
 HypergraphRulePlot[sub, \"PanelSize\" -> 120, \"SharedCoordinates\" -> False]}, ImageSize -> 320]"],

note["Without the shared layout the two sides are two separate spring embeddings of nearly the \
same hypergraph, and they come out differently. The eye reads that difference as something the \
rule did, which it did not. Sharing the layout costs nothing and removes a whole class of \
misreading."],

text["Opening the summary box of a rule shows the same picture, so a rule sitting in a variable \
can be checked without drawing it explicitly:"],

code["sub"],

subsection["An event"],

text["HypergraphEventPlot is the same idea applied to a particular match: what the event acted \
on with the match highlighted, an arrow, and what it made with the creations highlighted."],

code["shown = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{3, 4}]}];
\nHypergraphEventPlot[First @ sub[shown], \"PanelSize\" -> 140]"],

text["One picture for every way the rule fits, which is the quickest way to see what a rule is \
really doing to a hypergraph:"],

code["HypergraphEventPlot[sub, shown, \"PanelSize\" -> 95]"],

text["An event carries the hypergraph it acted on under \"Input\", which is what makes the left \
half drawable at all \[LongDash] an event that knew only its result could not be drawn:"],

code["First[sub[shown]][\"Input\"] === path3"],

note["Which edges of the result to highlight is read off \"EdgeOrigins\", the record of where \
each edge of the result came from, rather than by looking for edges equal to \"NewEdges\". In a \
multiset two edges of equal value are still two edges, so a rule that creates an edge the \
hypergraph already had would otherwise highlight whichever one happened to sort first."],

code["doubled = Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}];\n\
{#[\"EdgeOrigins\"], Flatten @ Position[#[\"EdgeOrigins\"], None]} & @ First[sub[doubled]]"],

code["HypergraphEventPlot[First @ sub[doubled], \"PanelSize\" -> 140]"],

subsection["The four highlight styles"],

text["The styles are four options, named as the reference paclet names them: left for what was \
matched, right for what was made, and a vertex and an edge style on each side."],

code["HypergraphEventPlot[First @ sub[shown], \"PanelSize\" -> 140,\n \
 \"HighlightLeftEdgeStyle\" -> Directive[Thick, Blue, EdgeForm[Directive[Blue, Thick]]],\n \
 \"HighlightLeftVertexStyle\" -> Directive[PointSize[0.03], Blue],\n \
 \"HighlightRightEdgeStyle\" -> Directive[Thick, Darker[Green], EdgeForm[Directive[Darker[Green], Thick]]],\n \
 \"HighlightRightVertexStyle\" -> Directive[PointSize[0.03], Darker[Green]]]"],

text["Any option of HypergraphPlot is passed through to both sides, so the two halves can be \
labelled, sized or restyled together:"],

code["HypergraphEventPlot[First @ sub[shown], \"PanelSize\" -> 140, VertexLabels -> \"Name\",\n \
 EdgeLabels -> None]"],

subsection["Styling a hypergraph, one element at a time"],

text["All of the above rests on HypergraphPlot taking a style per element. A single directive \
styles everything:"],

code["styled = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{1, 2}], Edge[{1, 2, 3}]}];\n\
HypergraphPlot[styled, EdgeStyle -> Directive[Thick, Purple], VertexStyle -> Orange,\n \
 ImageSize -> 220]"],

text["A list of rules or an Association styles them one at a time. A key may be an element's \
position, counting from one:"],

code["HypergraphPlot[styled, EdgeStyle -> {1 -> Red, 4 -> Directive[Opacity[0.4], Green]},\n \
 ImageSize -> 220]"],

text["or the element itself \[LongDash] a vertex identifier, or an edge's list of vertex \
identifiers:"],

code["HypergraphPlot[styled, EdgeStyle -> {{2, 3} -> Red}, VertexStyle -> {2 -> Orange},\n \
 ImageSize -> 220]"],

text["or a pattern, which is how a fallback is written. Later rules win, so the general case can \
be given first and then overridden:"],

code["HypergraphPlot[styled, VertexStyle -> {_ -> Gray, 2 -> Red}, ImageSize -> 220]"],

note["Positions are what a multiset needs. This hypergraph has the edge {1, 2} twice, and only a \
position can say which of the two to colour \[LongDash] which is exactly what highlighting a \
match requires, since a match covers particular edges and not every edge that looks like them. \
Where a vertex is itself named by an integer the identifier wins over the position reading, \
since that is what anyone writing VertexStyle -> {1 -> Red} means."],

code["{HypergraphPlot[styled, EdgeStyle -> {1 -> Red}, ImageSize -> 190],\n \
 HypergraphPlot[styled, EdgeStyle -> {3 -> Red}, ImageSize -> 190]}"],

text["An empty list of rules changes nothing, so a list of highlights can be built and passed \
without checking whether it came out empty:"],

code["HypergraphPlot[styled, EdgeStyle -> {}] === HypergraphPlot[styled]"],

text["\"EdgeLineStyle\" colours the boundary lines and arrowheads rather than the fill, and the \
two label styles do what their names say:"],

code["HypergraphPlot[styled, \"EdgeLineStyle\" -> Purple, EdgeLabelStyle -> Purple,\n \
 VertexLabelStyle -> {1 -> Red}, ImageSize -> 240]"],

subsection["Placing the vertices"],

text["VertexCoordinates takes rules, an Association, or a plain list in vertex order:"],

code["HypergraphPlot[Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{3, 1}]}],\n \
 VertexCoordinates -> {{0., 0.}, {1., 0.}, {0.5, 0.9}}, ImageSize -> 200]"],

text["Handing a layout back gives the same layout, which is the property everything else relies \
on:"],

code["triangle = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{3, 1}]}];\n\
here = Thread[VertexID /@ VertexList[triangle] -> HypergraphEmbedding[triangle]];\n\
Chop[HypergraphEmbedding[triangle, VertexCoordinates -> here] - HypergraphEmbedding[triangle]]"],

text["And a different hypergraph can be drawn on it, which is how the two halves of an event come \
to line up:"],

code["{HypergraphPlot[triangle, ImageSize -> 190],\n \
 HypergraphPlot[Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}], VertexCoordinates -> here,\n \
   ImageSize -> 190]}"],

text["Placing only some of the vertices settles the rest among them rather than laying them out \
separately. A vertex a rewrite has just inserted into an edge lands between that edge's two ends:"],

code["HypergraphPlot[First[sub[triangle]][\"Hypergraph\"], VertexCoordinates -> here,\n \
 VertexLabels -> \"Name\", ImageSize -> 240]"],

note["A graph given coordinates for some of its vertices and not others lays out none of them, \
which is why this is not simply handed to the layout. The given positions are written over the \
automatic layout and the rest moved repeatedly to the middle of their neighbours until they \
settle. The helper vertex that stands for a large edge is moved the same way, to the centre of \
the vertices it serves \[LongDash] left where the automatic layout put it, it would drag the \
edge's boundary away from them."],

section["7.  Worked examples"],

subsection["Growing a path"],

text["Subdividing repeatedly. Taking the first event each time, an edge becomes a path:"],

code["NestList[First[sub[#]][\"Hypergraph\"] &, Hypergraph[{Edge[{1, 2}]}], 3]"],

note["Choosing the first event each time is a choice made by hand, and it is exactly the choice a \
multiway system refuses to make: it keeps all of them, and the branching that results is what its \
evolution graph draws. Doing it by hand here shows what there is to keep."],

subsection["A rule with no match"],

text["A rule that does not apply gives no events, which is not a failure but an answer:"],

code["contract[Hypergraph[{Edge[{1, 2}]}]]"],

subsection["Every state one step away"],

text["All the events at once, from a slightly larger hypergraph, as the states they lead to:"],

code["star = Hypergraph[{Edge[{1, 2}], Edge[{1, 3}], Edge[{1, 4}]}]"],

code["#[\"Hypergraph\"] & /@ sub[star]"],

text["Three events, one per edge. They are distinct as events but isomorphic as hypergraphs, \
which CanonicalHypergraph makes plain:"],

code["Length @ DeleteDuplicates[CanonicalHypergraph[#[\"Hypergraph\"]] & /@ sub[star]]"],

note["Distinct events with isomorphic results is the ordinary situation, not a special case, and \
it is why a multiway system has to decide whether its states are hypergraphs or isomorphism \
classes of them. The pieces for either reading are already here."],

subsection["An unordered rule"],

text["Nothing about rewriting is particular to directed edges. Here an unordered pair becomes an \
unordered triple:"],

code["grow = HypergraphRule[{Edge[{1, 2}, \"Unordered\"]},\n \
                     {Edge[{1, 2, 3}, \"Unordered\"]}];\n\
Length @ grow[Hypergraph[{Edge[{1, 2}, \"Unordered\"]}]]"],

text["Two events, from the two orderings the unordered pattern admits, and their results are the \
same hypergraph:"],

code["DeleteDuplicates[#[\"Hypergraph\"] & /@ grow[Hypergraph[{Edge[{1, 2}, \"Unordered\"]}]]]"],

subsection["A rule on a labelled chain"],

text["Rules and labels together: rewriting only the edges that carry a given label leaves the \
rest of the hypergraph alone."],

code["mixed = Hypergraph[{Edge[{1, 2}, \"Directed\", \"x\"], Edge[{2, 3}, \"Directed\", \"y\"],\n \
                   Edge[{3, 4}, \"Directed\", \"x\"]}]"],

code["onlyX = HypergraphRule[{Edge[{1, 2}, \"Directed\", \"x\"]},\n \
                      {Edge[{1, 3}, \"Directed\", \"x\"], Edge[{3, 2}, \"Directed\", \"x\"]}];\n\
#[\"MatchEdgePositions\"] & /@ onlyX[mixed]"],

code["First[onlyX[mixed]][\"Hypergraph\"]"],

section["8.  Scope and limits"],

text["What is here is the core: a rule, the matches of its input, and the events an application \
produces. Everything built on top of that \[LongDash] applying a rule over and over to collect \
the states it reaches, the graph of which event enabled which, and the conditions for two events \
to be independent \[LongDash] is in CausalityBasics.nb, and HypergraphRewritingSystem is where \
it starts."],

text["The matcher is a backtracking search over the edges of the input, and makes no attempt to \
be clever. Subhypergraph matching is hard in general, and the sizes this package is meant for are \
small; a rule with many edges against a large hypergraph will be slow, and that is the cost to \
watch."],

text["The syntax follows the reference paclet: HypergraphRule[input, output], applied by \
rule[hg], giving associations keyed \"Hypergraph\", \"MatchVertices\", \"MatchEdges\" and the \
rest. Rules written for that paclet read the same here. The matcher underneath is this package's \
own, the reference's resting on a cloud resource function and on its own annotation system."],

text["The file tests/RewritingExamples.wls runs the examples in this notebook as assertions, plus \
invariants checked over three hundred events from rules built to match: that the binding really \
carries the input's edges onto the ones the match claims, that the edge counts add up, that \
nothing reported destroyed survives, and that the edges the rule did not touch are still there."],

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

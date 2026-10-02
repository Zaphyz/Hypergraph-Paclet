#!/usr/bin/env wolframscript

(* Builds experimental/notebooks/RewritingBasics.nb. Each input cell is evaluated here and its
   result attached as a StandardForm Output cell. *)

$ProgressReporting = False;
$root = StringReplace[ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]], "\\" -> "/"];
target = FileNameJoin[{$root, "experimental", "notebooks", "RewritingBasics.nb"}];

tidy[s_String] := StringReplace[
    StringReplace[s, {"\r\n" -> " ", "\r" -> " ", "\n" -> " "}],
    RegularExpression[" {2,}"] -> " "];

title[s_]      := Cell[tidy[s], "Title"];
subtitle[s_]   := Cell[tidy[s], "Subtitle"];
section[s_]    := Cell[tidy[s], "Section"];
subsection[s_] := Cell[tidy[s], "Subsection"];
text[s_]       := Cell[tidy[s], "Text"];
note[s_] := Cell[
    TextData[{StyleBox["Design note.  ", FontWeight -> "Bold", FontSlant -> "Italic"], tidy[s]}],
    "Text", CellFrame -> {{2, 0}, {0, 0}}, CellFrameColor -> GrayLevel[0.7],
    CellFrameMargins -> {{10, 0}, {6, 6}}, FontColor -> GrayLevel[0.25]];

$log = {};
code[s_] := code[s, s];
initCode[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    AppendTo[$log, {"INIT", Head[res]}];
    {Cell[s, "Input", InitializationCell -> True]}];
code[s_, evalStr_String] := Module[{res},
    res = Check[ToExpression[evalStr], $evalFailed];
    AppendTo[$log, {StringTake[StringReplace[s, "\n" -> " "], UpTo[56]], Head[res]}];
    If[res === Null, {Cell[s, "Input"]},
        {Cell[s, "Input"], Cell[BoxData[ToBoxes[res, StandardForm]], "Output"]}]];

cells = Flatten[{

title["Rewriting Basics"],
subtitle["WolframInstitute`HypergraphRewriting \[LongDash] a second take on rewriting systems"],

text["A rewriting system is a set of rules and a state to start from. Evolving it gives states, \
events and the causal relations between them. This is a separate stack from the one in src/, \
written from the ground up rather than following the upstream paclets, to be compared against it \
and either adopted or dropped."],

text["It takes as given everything up to and including rule application \[LongDash] Hypergraph, \
HypergraphPlot, HypergraphRule, HypergraphRuleApply, and the pictures of rules and events. \
Everything above that is new. Everything below is live output."],

section["Loading"],

initCode["PacletDirectoryLoad[ParentDirectory[ParentDirectory[NotebookDirectory[]]]];\n\
Needs[\"WolframInstitute`Hypergraphs`\"];\n\
Get[FileNameJoin[{ParentDirectory[NotebookDirectory[]], \"RewritingSystems.wl\"}]]",
  "PacletDirectoryLoad[\"" <> $root <> "\"];Needs[\"WolframInstitute`Hypergraphs`\"];" <>
  "Get[\"" <> $root <> "/experimental/RewritingSystems.wl\"]"],

(ClearAll[a, b, c, f, g, x, y, sys, ev, two, chain, comp, lsys, lev, tokenOf]; Nothing),

section["1.  A system is rules and a state"],

text["RewritingSystem takes the rules and where to start. A rule may be written as a plain \
arrow, and the state as anything Hypergraph accepts:"],

code["sys = RewritingSystem[{{1, 2}} -> {{1, 3}, {3, 2}}, {{1, 2}, {2, 3}}]"],

text["It shows itself as what it is: the rules drawn, and the state they start from. Open the \
summary box above. On its own, the rule subdivides an edge:"],

code["HypergraphRulePlot[First @ sys[\"Rules\"], \"PanelSize\" -> 150]"],

text["and the state is a path of two edges:"],

code["HypergraphPlot[sys[\"InitialState\"], ImageSize -> 200]"],

note["A system is two things and nothing else, so it has two properties. There is no place to \
hang options on it: how far to evolve and how to draw belong to the asking, not to the system."],

section["2.  Evolving it"],

text["sys[n] evolves n steps. What comes back is the whole evolution, which every picture below \
is a reading of:"],

code["ev = sys[2]"],

text["The states it reached, by step. Two edges, then three, then four \[LongDash] and two ways \
to take the first step, three to take the second:"],

code["Column[Row[HypergraphPlot[#, ImageSize -> 130] & /@ #, Spacer[6]] & /@ ev[\"StatesByStep\"],\n \
 Spacings -> 1]"],

code["{ev[\"StateCount\"], ev[\"EventCount\"], ev[\"TokenCount\"]}"],

section["3.  The graphs"],

text["Five readings of the same evolution. Each draws what its vertices stand for, and carries \
the object itself as a tooltip."],

subsection["States graph \[LongDash] where it can get to"],

text["One vertex per state, one arrow per event. Equal states are one vertex:"],

code["ev[\"StatesGraph\", ImageSize -> 560]"],

text["Equal means equal up to canonical form, which is what makes it merge at all: a rewrite \
names the vertices it creates after what was already there, so two branches that reach the same \
shape almost never reach it under the same names."],

code["{VertexCount @ ev[\"StatesGraph\", \"Draw\" -> False],\n \
 VertexCount @ ev[\"StatesGraph\", \"MergeStates\" -> False, \"Draw\" -> False]}"],

subsection["Event graph \[LongDash] what it actually did"],

text["States and events both, alternating: a state, the event that acted on it, the state that \
came out. The wide panels are the events, each showing what it matched and what it made:"],

code["sys[1][\"EventGraph\", ImageSize -> 620]"],

subsection["Causal graph \[LongDash] which event needed which"],

text["One vertex per event, and an arrow wherever one event used something another made:"],

code["ev[\"CausalGraph\", ImageSize -> 620]"],

code["EdgeList @ ev[\"CausalGraph\", \"Draw\" -> False]"],

note["There is no overlap test and no ancestry test here. An arrow of the causal graph is a \
token: an edge that one event created and another consumed. The relation is read off the tokens \
rather than computed from them, which is the whole reason this stack is organised the way it is."],

subsection["Token-event graph \[LongDash] the thing the others are made of"],

text["The edges themselves as vertices, with the events between them. Each small panel is one \
hyperedge; each wide panel is one event, taking the edges above it and giving the ones below:"],

code["sys[1][\"TokenEventGraph\", ImageSize -> 620]"],

subsection["Branchial graph \[LongDash] what one step could have done instead"],

text["The states of the last step, joined when they came from the same place:"],

code["sys[1][\"BranchialGraph\", ImageSize -> 420]"],

section["4.  Tokens, which is the whole idea"],

text["Every edge of every state is a token. A token is created by exactly one event, consumed by \
at most one, and has no other history:"],

code["Dataset[KeyTake[ev[\"Tokens\"], Range[6]]]"],

text["Token 3, drawn, and the two events it joins:"],

code["HypergraphPlot[Hypergraph[{ev[\"Tokens\"][3][\"Edge\"]}], VertexLabels -> \"Name\",\n \
 ImageSize -> 150]"],

code["{ev[\"Tokens\"][3][\"CreatedBy\"],\n \
 FirstPosition[#[\"Consumed\"] & /@ ev[\"Events\"], 3][[1]]}"],

text["Event 1 made it and event 3 used it, so there is an arrow from 1 to 3 in the causal graph \
\[LongDash] and there it is:"],

code["MemberQ[EdgeList @ ev[\"CausalGraph\", \"Draw\" -> False], DirectedEdge[1, 3]]"],

note["Because a token knows both ends of its own story, nothing has to be searched for. Two \
events on different branches can never be joined, since a token created on one branch is only \
ever in the states that descend from it. The whole causal graph is one pass over the tokens."],

text["The two events of the first step, by contrast, share nothing \[LongDash] they are two \
things that could have happened, not two that did:"],

code["{#[\"Consumed\"], #[\"Created\"]} & /@ Take[ev[\"Events\"], 2]"],

section["5.  A labelled system"],

text["Nothing is particular to unlabelled hypergraphs. Composition of labelled arrows, which is \
the rule the reference notebooks are written around:"],

code["comp = HypergraphRule[\n \
 {Edge[{1, 2}, \"Directed\", x], Edge[{2, 3}, \"Directed\", y]},\n \
 {Edge[{1, 3}, \"Directed\", Row[{\"(\", x, y, \")\"}]]}];\n\
chain = Hypergraph[{Edge[{1, 2}, \"Directed\", a], Edge[{2, 3}, \"Directed\", b],\n \
                   Edge[{3, 4}, \"Directed\", c]}];\n\
lsys = RewritingSystem[comp, chain]"],

code["lev = lsys[2];\nlev[\"StatesGraph\", ImageSize -> 520]"],

text["Two ways to compose first, and each leaves one way to finish, so the two orders of \
bracketing meet at the end:"],

code["lev[\"CausalGraph\", ImageSize -> 620]"],

code["lev[\"EventGraph\", ImageSize -> 700]"],

section["6.  Options, all four of them"],

text["\"Draw\" turns the pictures off, which is what to do when a graph is only being counted:"],

code["{ev[\"CausalGraph\", \"Draw\" -> False], ev[\"StatesGraph\", \"Draw\" -> False]}"],

text["\"MaxDrawn\" is where Automatic gives up, \"PanelSize\" is how big each picture is drawn, \
and \"MergeStates\" is whether the states graph merges up to canonical form:"],

code["ev[\"StatesGraph\", \"PanelSize\" -> 150, ImageSize -> 620]"],

text["\"MaxStates\" belongs to the evolving rather than the drawing, and stops a run that is \
growing faster than expected:"],

code["Quiet @ sys[6, \"MaxStates\" -> 20][\"StateCount\"]"],

text["Any option of Graph goes through as well:"],

code["ev[\"CausalGraph\", \"Draw\" -> False, VertexLabels -> \"Name\", VertexSize -> Large,\n \
 GraphLayout -> \"SpringEmbedding\", ImageSize -> 300]"],

section["7.  What is here, and what is not"],

text["Here: a system as rules and a state; an evolution as states, events and tokens; and five \
graphs over them. Four options. Two objects. That is the whole surface."],

text["Not here, deliberately, and the thing to weigh against the other stack: causal dependence \
through a shared vertex. In this stack a causal arrow is a hyperedge passing from one event to \
another, which is the standard reading in the graph-rewriting literature and in the Wolfram \
model. The other stack also offers a weaker reading, in which two events are related when their \
matches merely touch the same vertex. That is not a thing a token can express, and adding it \
would mean giving vertices tokens too. It is a real choice, not an oversight."],

text["Also not here: a states graph that merges across steps into a genuine multiway graph with \
shared tokens, and anything about causal invariance or confluence. Both are natural next steps \
if this stack is the one kept."],

text["The evolution is a tree of state occurrences, not a graph: a state reached two ways is two \
occurrences, because which tokens it is made of depends on how it was reached. The states graph \
merges them for display, which is the right place to do it."]

}];

nb = Notebook[cells,
    StyleDefinitions -> "Default.nb",
    WindowSize -> {900, 800},
    InitializationCellEvaluation -> True,
    InitializationCellWarning -> False];

Quiet @ CreateDirectory[DirectoryName[target]];
Export[target, nb, "NB"];

Print["wrote ", target];
Print["cells: ", Length[cells], "  input: ", Count[cells, Cell[_, "Input", ___]],
      "  output: ", Count[cells, Cell[_, "Output", ___]]];
Print["failed: ", Cases[$log, {s_, $evalFailed} :> s]];

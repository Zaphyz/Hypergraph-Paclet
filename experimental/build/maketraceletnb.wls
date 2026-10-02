#!/usr/bin/env wolframscript

(* Builds experimental/notebooks/TraceletBasics.nb. *)

$ProgressReporting = False;
$root = StringReplace[ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]], "\\" -> "/"];
target = FileNameJoin[{$root, "experimental", "notebooks", "TraceletBasics.nb"}];

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

title["Tracelet Basics"],
subtitle["WolframInstitute`HypergraphRewriting \[LongDash] derivations with the context taken away"],

text["After Nicolas Behr, Tracelets and Tracelet Analysis Of Compositional Rewriting Systems, \
ACT 2019, EPTCS 323, 44\[Dash]71 (arXiv:1904.12829). A tracelet is a derivation trace stripped \
of its context: the steps, and nothing of the hypergraph they ran in except what they actually \
used. Behr's characterisation theorem says every length-n trace is a tracelet applied to some \
ambient object, so the tracelet is what two traces have in common when they are the same \
derivation happening somewhere else."],

text["In the categorical account all of this is pushouts and spans of monomorphisms over an \
adhesive category. Here it is tokens. The rewriting stack already gives every hyperedge an \
identity, with the event that made it and the events that used it, and that is the same data."],

section["Loading"],

initCode["PacletDirectoryLoad[ParentDirectory[ParentDirectory[NotebookDirectory[]]]];\n\
Needs[\"WolframInstitute`Hypergraphs`\"];\n\
Get[FileNameJoin[{ParentDirectory[NotebookDirectory[]], \"RewritingSystems.wl\"}]];\n\
Get[FileNameJoin[{ParentDirectory[NotebookDirectory[]], \"TraceletAnalysis.wl\"}]]",
  "PacletDirectoryLoad[\"" <> $root <> "\"];Needs[\"WolframInstitute`Hypergraphs`\"];" <>
  "Get[\"" <> $root <> "/experimental/RewritingSystems.wl\"];" <>
  "Get[\"" <> $root <> "/experimental/TraceletAnalysis.wl\"]"],

(ClearAll[a, b, c, x, y, sys, ts, t1, t2, comp, chain, lsys, lts]; Nothing),

section["1.  Two ways for two steps to meet"],

text["A system that subdivides an edge, started from a path of two:"],

code["sys = RewritingSystem[{{1, 2}} -> {{1, 3}, {3, 2}}, {{1, 2}, {2, 3}}];\n\
HypergraphRulePlot[First @ sys[\"Rules\"], \"PanelSize\" -> 130]"],

text["Two steps of it can happen in nine ways, but they are only two derivations. \
TraceletAnalysis says which:"],

code["ts = TraceletAnalysis[sys, 2];\nLength[ts]"],

code["Dataset[KeyTake[#, {\"Rules\", \"Causal\", \"Multiplicity\", \"Linearisations\"}] & /@ ts]"],

text["The first, four of the nine runs, is one subdivision inside the other. The second, the \
other two, is two subdivisions that never meet. Drawn, in their own minimal context:"],

code["t1 = ts[[1]]; t2 = ts[[2]];\nt1[\"Diagram\"]"],

code["t2[\"Diagram\"]"],

note["Look at what each one starts from. The first begins with a single edge \[LongDash] not the \
two-edge path the system was run on. Nothing else was needed, so nothing else is there. That is \
the whole content of the word minimal, and of Behr's characterisation theorem: the trace that \
actually ran is this tracelet with a context glued on."],

code["{HypergraphPlot[t1[\"Input\"], ImageSize -> 130],\n \
 HypergraphPlot[t2[\"Input\"], ImageSize -> 160]}"],

section["2.  The composite rule"],

text["By the concurrency theorem the whole trace is one rule, from the input interface to the \
output interface. For the causal tracelet, an edge becomes a path of three:"],

code["HypergraphRulePlot[t1[\"Composite\"], \"PanelSize\" -> 150]"],

text["And it really does: applying that one rule to the tracelet's input gives the tracelet's \
output, in one step instead of two."],

code["CanonicalHypergraph @ First[HypergraphRuleApply[t1[\"Composite\"], t1[\"Input\"]]][\"Hypergraph\"] ===\n \
 CanonicalHypergraph @ t1[\"Output\"]"],

text["The independent one composes too, into a rule that subdivides both edges of a path at once:"],

code["HypergraphRulePlot[t2[\"Composite\"], \"PanelSize\" -> 150]"],

section["3.  Overlaps, which are the causality"],

text["What glues one step to the next is a token: an edge the first made and the second used. \
That is the admissible match of the categorical account. The causal tracelet has one:"],

code["t1[\"Overlaps\"]"],

code["HypergraphPlot[Hypergraph[First[t1[\"Overlaps\"]][\"Edges\"]], VertexLabels -> \"Name\",\n \
 ImageSize -> 140]"],

text["The independent one has none, which is exactly why it is not causal:"],

code["{t2[\"Overlaps\"], t2[\"Causal\"]}"],

text["Collected over the steps, the overlaps are the causal order of the tracelet:"],

code["{t1[\"CausalOrder\"], t2[\"CausalOrder\"]}"],

text["Counting the orders in which the steps could have been taken is Behr's shift equivalence, \
counted. One means the trace was forced; two means the two steps were independent and the order \
they came in was an accident of the run:"],

code["{t1[\"Linearisations\"], t2[\"Linearisations\"]}"],

section["4.  Where this meets the causal graph"],

text["The causal order of a tracelet is the causal graph of the evolution, restricted to the \
steps of that trace and renumbered. The two are the same object seen from two distances: the \
causal graph says which of the events that happened depended on which, and the tracelet says \
what the dependence was made of and what the smallest hypergraph is in which it could happen \
at all."],

code["sys[2][\"CausalGraph\", ImageSize -> 560]"],

code["First[t1[\"Occurrences\"]]"],

text["Those two event numbers are joined in the causal graph above, and the tracelet is what \
that arrow means."],

note["A causal arrow in this stack is a token, and an overlap of a tracelet is a token, so the \
two notions are not merely compatible \[LongDash] they are the same list read twice. That is \
the reason the tracelet code is short: it computes nothing that the evolution did not already \
record."],

section["5.  Labels"],

text["Composition of labelled arrows. Two steps, and the two bracketings:"],

code["comp = HypergraphRule[\n \
 {Edge[{1, 2}, \"Directed\", x], Edge[{2, 3}, \"Directed\", y]},\n \
 {Edge[{1, 3}, \"Directed\", Row[{\"(\", x, y, \")\"}]]}];\n\
chain = Hypergraph[{Edge[{1, 2}, \"Directed\", a], Edge[{2, 3}, \"Directed\", b],\n \
                   Edge[{3, 4}, \"Directed\", c]}];\n\
lts = TraceletAnalysis[RewritingSystem[comp, chain], 2];\nLength[lts]"],

code["Column[#[\"Diagram\"] & /@ lts, Spacings -> 1.2]"],

text["Two tracelets, each with multiplicity one, each causal: the labels make the two bracketings \
different derivations, where without them they would be one."],

code["Dataset[KeyTake[#, {\"Rules\", \"Causal\", \"Multiplicity\", \"Linearisations\"}] & /@ lts]"],

section["6.  What the function gives back"],

code["Keys[t1]"],

text["\"Input\" and \"Output\" are the two interfaces; \"Composite\" is the single rule they \
make; \"Trace\" is the derivation in minimal context, which is what \"Diagram\" draws; \
\"Overlaps\" are the tokens gluing step to step; \"CausalOrder\" and \"Linearisations\" are what \
they amount to; \"Multiplicity\" and \"Occurrences\" say how often this tracelet turned up and \
where."],

text["Other ways to ask. A single explicit trace, by event number:"],

code["TraceletAnalysis[sys[2], {1, 3}][[1, \"Diagram\"]]"],

text["Only the causally connected ones, which is the filter Behr's pathway analysis puts on:"],

code["Length /@ {TraceletAnalysis[sys, 2], TraceletAnalysis[sys, 2, \"Causal\" -> True]}"],

text["And with the context left in, which is the same trace read where it happened rather than \
on its own:"],

code["TraceletAnalysis[sys[2], {1, 3}, \"Minimal\" -> False][[1, \"Diagram\"]]"],

section["7.  Scope"],

text["What is here is the extraction half of the framework: given derivations, factor each into \
a tracelet and a context, quotient by isomorphism, and report. That is Behr's characterisation \
theorem used in the direction that needs no new search, and it is why the implementation is a \
single pass over the tokens."],

text["What is not here is the generative half: FETA, the algorithm that builds minimal traces \
ending in a chosen event by composing rules along admissible matches, without running the system \
at all. That needs an enumeration of the overlaps between the output of one rule and the input \
of the next, which is a real piece of work and a natural next step."],

text["Two smaller limits. Abstraction equivalence is tested by canonicalising the two interfaces \
together, with each edge marked by the side it is on, alongside the rule sequence and the causal \
order; that is an invariant of the isomorphism class, and strong enough here, but it is not a \
proof of completeness. And overlaps are hyperedges only, as causality is everywhere in this \
stack \[LongDash] two steps meeting at a shared vertex and no shared edge count as independent."]

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

#!/usr/bin/env wolframscript
$ProgressReporting = False;

(* Assertions for tracelet analysis.
   Run with:  wolframscript -file experimental/tests/TraceletExamples.wls  *)

$root = ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]];
PacletDirectoryLoad[$root];
Needs["WolframInstitute`Hypergraphs`"];
Get[FileNameJoin[{$root, "experimental", "RewritingSystems.wl"}]];
Get[FileNameJoin[{$root, "experimental", "TraceletAnalysis.wl"}]];

p = Print;
form[x_] := ToString[x, InputForm];
$fails = 0;
check[lbl_, got_, want_] := (
   If[got =!= want, $fails++];
   p[If[got === want, "  ok   ", "  FAIL "], StringPadRight[lbl, 54], " ", form[got],
     If[got === want, "", "   (expected " <> form[want] <> ")"]]);

ClearAll[a, b, c, x, y];
bare = Sequence["Diagrams" -> False];
same[h1_, h2_] := CanonicalHypergraph[h1] === CanonicalHypergraph[h2];

(* Every run of n steps each acting on what the one before it made. The same traces the function
   looks at, enumerated here independently so the count is not taken on trust. *)
traces[ev_, n_] := Module[{evs = ev["Events"], follows},
   follows[k_] := Select[Range[Length[evs]], evs[[#, "InputState"]] === evs[[k, "OutputState"]] &];
   Nest[Catenate[Function[path, Append[path, #] & /@ follows[Last[path]]] /@ #] &,
        List /@ Range[Length[evs]], n - 1]];

sub = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}];
sys = RewritingSystem[sub, Hypergraph[{{1, 2}, {2, 3}}]];


p["=== 1. A tracelet of length one is the rule ==="];
one = TraceletAnalysis[sys, 1, bare];
check["one tracelet, however many times it fired", Length[one], 1];
check["  and it fired twice", First[one]["Multiplicity"], 2];
check["its input is the rule's input", same[First[one]["Input"], sub["Input"]], True];
check["its output is the rule's output", same[First[one]["Output"], sub["Output"]], True];
check["there is nothing to glue", First[one]["Overlaps"], {}];
check["one step is trivially causal", First[one]["Causal"], True];
check["and has one order", First[one]["Linearisations"], 1];


p[];
p["=== 2. Two steps meet in two ways, and only two ==="];
ts = TraceletAnalysis[sys, 2, bare];
check["six runs of two steps, two tracelets", {Length @ traces[sys[2], 2], Length[ts]}, {6, 2}];
check["the multiplicities account for all six",
   Total[#["Multiplicity"] & /@ ts], 6];
check["one causal, one not", #["Causal"] & /@ ts, {True, False}];
check["the causal one is the commoner", #["Multiplicity"] & /@ ts, {4, 2}];

causal = First[ts]; indep = Last[ts];
check["the causal tracelet needs one edge and nothing else",
   {EdgeCount @ causal["Input"], VertexCount @ causal["Input"]}, {1, 2}];
check["  and leaves three", EdgeCount @ causal["Output"], 3];
check["  glued by one token", Length @ causal["Overlaps"], 1];
check["  in a forced order", causal["Linearisations"], 1];
check["the independent one needs both edges",
   {EdgeCount @ indep["Input"], VertexCount @ indep["Input"]}, {2, 3}];
check["  nothing glues its steps", indep["Overlaps"], {}];
check["  and either order would have done", indep["Linearisations"], 2];

p["-- minimality is the point --"];
(* The system ran on a two-edge path, and the causal tracelet needs one edge. That difference is
   Behr's characterisation theorem: the trace is this tracelet with a context glued on. *)
check["the context really was dropped",
   EdgeCount[causal["Input"]] < EdgeCount[sys["InitialState"]], True];
check["\"Minimal\" -> False keeps it",
   EdgeCount @ First[TraceletAnalysis[sys[2], {1, 3}, bare, "Minimal" -> False]]["Input"],
   EdgeCount @ sys["InitialState"]];


p[];
p["=== 3. The concurrency theorem ==="];
(* The composite rule does in one step what the trace did in n. If this fails, the interfaces
   were computed wrongly. *)
check["every composite is a rule", HypergraphRuleQ[#["Composite"]] & /@ ts, {True, True}];
check["and applying it gives the tracelet's output",
   Table[
      MemberQ[CanonicalHypergraph[#["Hypergraph"]] & /@
                 HypergraphRuleApply[t["Composite"], t["Input"]],
              CanonicalHypergraph[t["Output"]]],
      {t, ts}],
   {True, True}];
check["the composite of a one-step tracelet is its own rule",
   same[First[HypergraphRuleApply[First[one]["Composite"], First[one]["Input"]]]["Hypergraph"],
        First[HypergraphRuleApply[sub, First[one]["Input"]]]["Hypergraph"]],
   True];

p["-- and the trace runs in its own minimal context --"];
check["each tracelet's input admits its own trace",
   Table[
      With[{re = RewritingSystem[sys["Rules"], t["Input"]][t["Length"]]},
         MemberQ[CanonicalHypergraph[#["Hypergraph"]] & /@ First[re]["States"],
                 CanonicalHypergraph[t["Output"]]]],
      {t, ts}],
   {True, True}];
check["  and gives back the same tracelet",
   Table[
      With[{re = RewritingSystem[sys["Rules"], t["Input"]][t["Length"]]},
         MemberQ[{#["Rules"], #["Causal"], EdgeCount[#["Input"]]} & /@
                    TraceletAnalysis[re, t["Length"], bare],
                 {t["Rules"], t["Causal"], EdgeCount[t["Input"]]}]],
      {t, ts}],
   {True, True}];


p[];
p["=== 4. Overlaps are the causal graph ==="];
ev = sys[2];
check["an overlap names the token that glues two steps",
   Keys @ First @ First[ts]["Overlaps"], {"From", "To", "Tokens", "Edges"}];
(* The connection to the causality machinery: a causal arrow of the evolution and an overlap of
   a tracelet are the same token, read twice. *)
check["every overlap of every trace is a causal arrow of the evolution",
   With[{arrows = Sort[List @@@ EdgeList @ ev["CausalGraph", "Draw" -> False]]},
      And @@ Table[
         With[{t = First @ TraceletAnalysis[ev, es, bare]},
            And @@ (MemberQ[arrows, {es[[#["From"]]], es[[#["To"]]]}] & /@ t["Overlaps"])],
         {es, traces[ev, 2]}]],
   True];
check["and a causal arrow is an overlap of the trace it joins",
   With[{arrows = List @@@ EdgeList @ ev["CausalGraph", "Draw" -> False]},
      And @@ Table[
         With[{t = First @ TraceletAnalysis[ev, arrow, bare]},
            Length[t["Overlaps"]] > 0],
         {arrow, arrows}]],
   True];


p[];
p["=== 5. Labels tell derivations apart ==="];
comp = HypergraphRule[
   {Edge[{1, 2}, "Directed", x], Edge[{2, 3}, "Directed", y]},
   {Edge[{1, 3}, "Directed", Row[{"(", x, y, ")"}]]}];
chain = Hypergraph[{Edge[{1, 2}, "Directed", a], Edge[{2, 3}, "Directed", b],
                    Edge[{3, 4}, "Directed", c]}];
lts = TraceletAnalysis[RewritingSystem[comp, chain], 2, bare];
check["two bracketings, two tracelets", Length[lts], 2];
check["each ran once", #["Multiplicity"] & /@ lts, {1, 1}];
check["both are causal", #["Causal"] & /@ lts, {True, True}];
check["each needs all three arrows", EdgeCount[#["Input"]] & /@ lts, {3, 3}];
check["and leaves one", EdgeCount[#["Output"]] & /@ lts, {1, 1}];
check["the outputs are the two bracketings",
   Sort[Flatten[EdgeLabel /@ EdgeList[#["Output"]] & /@ lts]],
   Sort[{Row[{"(", a, Row[{"(", b, c, ")"}], ")"}], Row[{"(", Row[{"(", a, b, ")"}], c, ")"}]}]];
check["unlabelled, the two would be one tracelet",
   Length @ TraceletAnalysis[
      RewritingSystem[HypergraphRule[{{1, 2}, {2, 3}}, {{1, 3}}],
                      Hypergraph[{{1, 2}, {2, 3}, {3, 4}}]], 2, bare],
   1];


p[];
p["=== 6. A rule that needs a vertex and no edge ==="];
(* The interfaces carry vertices as well as edges. A rule whose whole input is one labelled
   vertex would otherwise come out with an empty input and a composite that is not a rule. *)
unit = HypergraphRule[
   Hypergraph[{Vertex[1, a]}, {}],
   Hypergraph[{Vertex[1, a], Vertex[2, a]}, {Edge[{1, 2}, "Directed", b]}]];
uts = TraceletAnalysis[RewritingSystem[unit, Hypergraph[{Vertex[1, x], Vertex[2, y]}, {}]], 1, bare];
(* Two of them, because the rule binds the vertex label: firing at the vertex labelled x and at
   the one labelled y builds differently labelled edges, so they are two derivations. *)
check["one tracelet per vertex it could fire at", Length[uts], 2];
check["the vertex survives into the input interface",
   VertexCount[#["Input"]] & /@ uts, {1, 1}];
check["  with no edges at all", EdgeCount[#["Input"]] & /@ uts, {0, 0}];
check["  and the output has the edge it built", EdgeCount[#["Output"]] & /@ uts, {1, 1}];
check["the composite is still a rule", HypergraphRuleQ[First[uts]["Composite"]], True];
check["  carrying the vertex it needs on its input side",
   VertexCount @ First[uts]["Composite"]["Input"], 1];


p[];
p["=== 7. Asking in the other ways ==="];
check["a system and a length evolve first",
   Length @ TraceletAnalysis[sys, 2, bare], Length @ TraceletAnalysis[sys[2], 2, bare]];
check["one explicit trace gives one tracelet",
   Length @ TraceletAnalysis[ev, {1, 3}, bare], 1];
check["  and says where it came from", First[TraceletAnalysis[ev, {1, 3}, bare]]["Occurrences"],
   {{1, 3}}];
check["\"Causal\" -> True drops the independent one",
   Length @ TraceletAnalysis[sys, 2, bare, "Causal" -> True], 1];
check["maximal traces, when no length is given",
   #["Length"] & /@ TraceletAnalysis[ev, bare], {2, 2}];
check["a diagram is drawn by default",
   Head @ First[TraceletAnalysis[sys, 2]]["Diagram"], Row];
check["events that do not follow one another are refused",
   Quiet @ TraceletAnalysis[ev, {1, 2}, bare], $Failed];
check["a bad length is refused", Quiet @ TraceletAnalysis[ev, 0, bare], $Failed];
check["something that is not an evolution is refused", Quiet @ TraceletAnalysis[42], $Failed];


p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];

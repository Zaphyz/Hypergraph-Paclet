#!/usr/bin/env wolframscript
$ProgressReporting = False;

(* Assertions for the experimental rewriting stack.
   Run with:  wolframscript -file experimental/tests/RewritingSystemsExamples.wls  *)

$root = ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]];
PacletDirectoryLoad[$root];
Needs["WolframInstitute`Hypergraphs`"];
Get[FileNameJoin[{$root, "experimental", "RewritingSystems.wl"}]];

p = Print;
form[x_] := ToString[x, InputForm];
$fails = 0;
check[lbl_, got_, want_] := (
   If[got =!= want, $fails++];
   p[If[got === want, "  ok   ", "  FAIL "], StringPadRight[lbl, 52], " ", form[got],
     If[got === want, "", "   (expected " <> form[want] <> ")"]]);

ClearAll[a, b, c, f, g, x, y];
plain = Sequence["Draw" -> False];


p["=== 1. The system ==="];
sub = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}];
two = Hypergraph[{{1, 2}, {2, 3}}];
sys = RewritingSystem[sub, two];
check["RewritingSystemQ", RewritingSystemQ[sys], True];
check["a plain arrow builds the same system",
   RewritingSystem[{{1, 2}} -> {{1, 3}, {3, 2}}, {{1, 2}, {2, 3}}] === sys, True];
check["a list of rules too", RewritingSystem[{sub}, two] === sys, True];
check["it keeps its two parts", {sys["Rules"], sys["InitialState"]}, {{sub}, two}];
check["nothing else is a system", RewritingSystemQ /@ {sub, two, $Failed}, {False, False, False}];
check["a non-rule is refused", Quiet @ RewritingSystem[{"no"}, two], $Failed];
check["a non-hypergraph start is refused", Quiet @ RewritingSystem[sub, "no"], $Failed];
check["a negative number of steps is refused", Quiet @ sys[-1], $Failed];


p[];
p["=== 2. Evolving ==="];
ev = sys[2];
check["RewritingEvolutionQ", RewritingEvolutionQ[ev], True];
check["counts", {ev["StateCount"], ev["EventCount"], ev["TokenCount"]}, {9, 8, 18}];
check["one state per event, plus the one it started from",
   ev["StateCount"] === ev["EventCount"] + 1, True];
check["states by step", Length /@ ev["StatesByStep"], {1, 2, 6}];
check["zero steps is the initial state alone",
   {sys[0]["StateCount"], sys[0]["EventCount"]}, {1, 0}];
check["an unknown property says so", Quiet @ ev["Nope"], $Failed];
check["the properties on offer", ev["Properties"],
   {"System", "Steps", "StateCount", "EventCount", "TokenCount",
    "States", "Events", "Tokens", "StatesByStep",
    "StatesGraph", "EventGraph", "CausalGraph", "TokenEventGraph", "BranchialGraph"}];
check["\"MaxStates\" stops it", Quiet @ sys[6, "MaxStates" -> 20]["StateCount"], 20];


p[];
p["=== 3. Tokens ==="];
(* The one idea the stack is built on: an edge is a thing with an identity. Every invariant
   below is a consequence of that, and if any of them fails the causal graph is wrong. *)
tokens = ev["Tokens"];
events = ev["Events"];
check["every state's token list is as long as its edge list",
   And @@ (Length[#["Tokens"]] === EdgeCount[#["Hypergraph"]] & /@ First[ev]["States"]), True];
check["a token's edge is the edge it stands for in every state holding it",
   And @@ Catenate[
      Function[st, MapThread[tokens[#1, "Edge"] === #2 &, {st["Tokens"], EdgeList[st["Hypergraph"]]}]] /@
         First[ev]["States"]],
   True];
check["the initial tokens are the ones nobody created",
   Keys @ Select[tokens, #["CreatedBy"] === None &], Range[EdgeCount[two]]];
check["every other token names the event that made it",
   And @@ (IntegerQ[#["CreatedBy"]] & /@ Select[tokens, #["CreatedBy"] =!= None &]), True];
check["no token is created twice",
   With[{made = Catenate[#["Created"] & /@ events]}, Length[made] === Length[DeleteDuplicates[made]]],
   True];
(* A token may well be consumed twice, by two branches out of the state that held it -- what it
   may not do is be consumed twice along one branch, which would mean using something that was
   already gone. *)
branchOf[st_, states_] := If[states[[st, "Parent"]] === None, {st},
   Append[branchOf[states[[st, "Parent"]], states], st]];
check["a token is consumed at most once along any one branch",
   With[{sts = First[ev]["States"]},
      And @@ Table[
         DuplicateFreeQ @ Catenate[
            events[[#]]["Consumed"] & /@
               DeleteCases[sts[[#, "InEvent"]] & /@ branchOf[st, sts], None]],
         {st, Length[sts]}]],
   True];
check["  but may be consumed by two branches, and both arrows are real",
   With[{shared = Select[First[ev]["ConsumedBy"], Length[#] > 1 &]}, Length[shared] > 0], True];
check["an event consumes exactly what its match covered",
   And @@ (Length[#["Consumed"]] === Length[#["Match", "MatchEdgePositions"]] & /@ events), True];
check["and creates exactly what its rule builds",
   And @@ (Length[#["Created"]] === Length[#["Match", "NewEdges"]] & /@ events), True];
check["a child state is its parent's tokens, less what went, plus what came",
   And @@ Table[
      With[{e = events[[k]], st = First[ev]["States"]},
         Sort[st[[e["OutputState"], "Tokens"]]] ===
            Sort[Join[Complement[st[[e["InputState"], "Tokens"]], e["Consumed"]], e["Created"]]]],
      {k, Length[events]}],
   True];

p["-- two edges of equal value are two tokens --"];
dup = RewritingSystem[sub, Hypergraph[{{1, 2}, {1, 2}}]][1];
check["two identical edges give two events", dup["EventCount"], 2];
check["and the two events take different tokens",
   Length @ Union @ Catenate[#["Consumed"] & /@ dup["Events"]], 2];


p[];
p["=== 4. Causality ==="];
cg = ev["CausalGraph", plain];
check["one vertex per event", VertexCount[cg], ev["EventCount"]];
check["the arrows", Sort @ EdgeList[cg],
   Sort[DirectedEdge @@@ {{1, 3}, {1, 5}, {2, 7}, {2, 8}}]];
check["it is acyclic", AcyclicGraphQ[cg], True];
check["every arrow runs forward in time",
   And @@ (events[[First[#], "Step"]] < events[[Last[#], "Step"]] & /@ EdgeList[cg]), True];
(* The arrows are not computed from the events; they are the tokens, counted. *)
check["an arrow for every token with both ends, and no other",
   Sort @ DeleteDuplicates @ Flatten @ KeyValueMap[
      Function[{id, tok},
         With[{to = First /@ Position[#["Consumed"] & /@ events, id, {2}]},
            If[tok["CreatedBy"] === None || to === {},
               Nothing,
               DirectedEdge[tok["CreatedBy"], #] & /@ to]]],
      tokens],
   Sort @ EdgeList[cg]];
check["events of the first step are never related",
   EdgeList @ sys[1]["CausalGraph", plain], {}];


p[];
p["=== 5. The graphs ==="];
check["all five are graphs",
   GraphQ /@ Table[ev[gr, plain],
      {gr, {"StatesGraph", "EventGraph", "CausalGraph", "TokenEventGraph", "BranchialGraph"}}],
   {True, True, True, True, True}];
check["the event graph alternates states and events",
   {VertexCount @ ev["EventGraph", plain], EdgeCount @ ev["EventGraph", plain]},
   {ev["StateCount"] + ev["EventCount"], 2 ev["EventCount"]}];
check["  and is a tree rooted at the initial state",
   {AcyclicGraphQ @ ev["EventGraph", plain],
    Pick[VertexList @ ev["EventGraph", plain], VertexInDegree @ ev["EventGraph", plain], 0]},
   {True, {{"state", 1}}}];
check["the states graph merges equal states, up to canonical form by default",
   {VertexCount @ ev["StatesGraph", plain],
    VertexCount @ ev["StatesGraph", plain, "MergeStates" -> False]},
   {3, 9}];
check["  and the initial state is the only thing nothing leads to",
   Pick[VertexList @ ev["StatesGraph", plain], VertexInDegree @ ev["StatesGraph", plain], 0], {1}];
check["the token-event graph has a vertex per event and per token used",
   VertexCount @ ev["TokenEventGraph", plain],
   ev["EventCount"] + Length @ Union @ Catenate[Join[#["Consumed"], #["Created"]] & /@ events]];
check["the branchial graph is the states of the last step",
   VertexCount @ ev["BranchialGraph", plain], Length @ Last @ ev["StatesByStep"]];
check["options of Graph pass through",
   ImageSize /. Options[ev["CausalGraph", plain, ImageSize -> 321], ImageSize], 321];


p[];
p["=== 6. Drawn ==="];
(* The point of the stack. These only check that a picture came out; what it looks like is for
   experimental/notebooks/RewritingBasics.nb. *)
check["every graph draws", Head /@ Table[ev[gr],
      {gr, {"StatesGraph", "EventGraph", "CausalGraph", "TokenEventGraph", "BranchialGraph"}}],
   {Graph, Graph, Graph, Graph, Graph}];
check["drawing can be forced off and on",
   {Head @ ev["CausalGraph", "Draw" -> False], Head @ ev["CausalGraph", "Draw" -> True]},
   {Graph, Graph}];
check["past \"MaxDrawn\" it falls back without complaint",
   Head @ ev["EventGraph", "MaxDrawn" -> 2], Graph];
check["a system shows itself", Head @ ToBoxes[sys, StandardForm], InterpretationBox];
check["so does an evolution", Head @ ToBoxes[ev, StandardForm], InterpretationBox];


p[];
p["=== 7. Labels ==="];
comp = HypergraphRule[
   {Edge[{1, 2}, "Directed", x], Edge[{2, 3}, "Directed", y]},
   {Edge[{1, 3}, "Directed", Row[{"(", x, y, ")"}]]}];
lev = RewritingSystem[comp, Hypergraph[{Edge[{1, 2}, "Directed", a], Edge[{2, 3}, "Directed", b],
                                        Edge[{3, 4}, "Directed", c]}]][2];
check["two ways in, one way on from each", {lev["EventCount"], lev["StateCount"]}, {4, 5}];
check["the two bracketings",
   Sort @ DeleteDuplicates @ Select[
      EdgeLabel /@ EdgeList[#["Match", "Hypergraph"]] & /@ lev["Events"], Length[#] === 1 &],
   Sort[{{Row[{"(", a, Row[{"(", b, c, ")"}], ")"}]}, {Row[{"(", Row[{"(", a, b, ")"}], c, ")"}]}}]];
check["each second step depends on the first that fed it",
   Sort @ EdgeList @ lev["CausalGraph", plain], Sort[DirectedEdge @@@ {{1, 3}, {2, 4}}]];


p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];

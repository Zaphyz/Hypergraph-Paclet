#!/usr/bin/env wolframscript
$ProgressReporting = False;

(* Assertions for rewriting systems and causality.
   Run with:  wolframscript -file tests/CausalityExamples.wls  *)

Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "src", "Hypergraph.wl"}]];

p = Print;
form[x_] := ToString[x, InputForm];
$fails = 0;
check[lbl_, got_, want_] := (
   If[got =!= want, $fails++];
   p[If[got === want, "  ok   ", "  FAIL "], StringPadRight[lbl, 54], " ", form[got],
     If[got === want, "", "   (expected " <> form[want] <> ")"]]
);

(* Nothing here may assign to a symbol a rule uses as a label variable: the rule holds the symbol
   itself, so giving it a value later turns every copy of that rule into whatever the value is.
   The variables below are x, y, z, p0 and q0, and they stay clear. *)
ClearAll[a, b, c, d, e, f, g, p0, q0, r0, s0, u, x, y, z];

pairs[l_] := {#["From"], #["To"]} & /@ l;
labelsOf[hg_] := EdgeLabel /@ EdgeList[hg];


p["=== 1. The system object ==="];
sub = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}];
single = Hypergraph[{{1, 2}}];
sys = HypergraphRewritingSystem[sub, single];
check["HypergraphRewritingSystemQ", HypergraphRewritingSystemQ[sys], True];
check["one rule may be given bare", sys["Rules"], {sub}];
check["and a list of them", HypergraphRewritingSystem[{sub}, single] === sys, True];
check["the initial state is kept", sys["InitialState"], single];
check["the initial state may be anything Hypergraph takes",
   HypergraphRewritingSystem[sub, {{1, 2}}] === sys, True];
check["rules may be given as plain arrows",
   HypergraphRewritingSystem[{{1, 2}} -> {{1, 3}, {3, 2}}, single] === sys, True];
check["a non-rule is refused",
   Quiet @ HypergraphRewritingSystem[{"nonsense"}, single], $Failed];
check["a non-hypergraph initial state is refused",
   Quiet @ HypergraphRewritingSystem[sub, "nonsense"], $Failed];
check["and neither is a system", HypergraphRewritingSystemQ /@ {$Failed, sub, 7},
   {False, False, False}];
check["an unknown property says so", sys["Nope", 1], Missing["UnknownProperty", "Nope"]];
check["a negative number of generations is refused", Quiet @ sys["CausalGraph", -1], $Failed];


p[];
p["=== 2. Evolution ==="];
(* Subdividing an edge: every state has one more edge than the last, and every edge of it is a
   place the rule fits, so the count of events triples. *)
check["generation zero is the initial state alone",
   {sys["StateCount", 0], sys["EventCount", 0]}, {1, 0}];
check["states after one, two, three generations",
   Table[sys["StateCount", n], {n, 3}], {2, 4, 10}];
check["events after one, two, three generations",
   Table[sys["EventCount", n], {n, 3}], {1, 3, 9}];
check["one state per event, and the initial one",
   Table[sys["StateCount", n] === sys["EventCount", n] + 1, {n, 0, 3}],
   {True, True, True, True}];
check["\"States\" groups them by generation",
   Length /@ sys["States", 2], {1, 1, 2}];
check["\"StateList\" is the flat list", Length @ sys["StateList", 2], 4];
check["every state is a hypergraph", AllTrue[sys["StateList", 3], HypergraphQ], True];
check["the first state is the initial one", First @ sys["StateList", 2], single];
check["an event knows which state it acted on and which it made",
   {#["InputState"], #["OutputState"]} & /@ sys["EventList", 2], {{1, 2}, {2, 3}, {2, 4}}];
check["and which generation it belongs to",
   #["Generation"] & /@ sys["EventList", 2], {1, 2, 2}];
check["the keys of an event record", Keys @ First @ sys["EventList", 1],
   {"Rule", "RuleIndex", "Generation", "InputState", "ConsumedEdges", "CreatedEdges",
    "MatchedVertices", "CreatedVertices", "DestroyedVertices", "Data", "OutputState"}];
check["\"Data\" is the event HypergraphRuleApply gave",
   First[sys["EventList", 1]]["Data"] === First[sub[single]], True];

p["-- the evolution is a tree, not a graph of distinct states --"];
(* A state reached two ways appears twice, because which elements it is made of depends on how it
   was reached. Merging them is what "StatesGraph" does, for display. *)
flip = HypergraphRule[{{1, 2}}, {{2, 1}}];
flipSys = HypergraphRewritingSystem[flip, Hypergraph[{{1, 2}}]];
check["flipping an edge back and forth keeps making new occurrences",
   Table[flipSys["StateCount", n], {n, 0, 3}], {1, 2, 3, 4}];
(* Reversing a directed edge gives back something isomorphic to what was there, so up to
   canonical form there is only ever the one state; asking for equality as written sees the two. *)
check["  though up to canonical form there is only one state",
   VertexCount @ flipSys["StatesGraph", 3], 1];
check["  and two when states are compared as written",
   VertexCount @ flipSys["StatesGraph", 3, "CanonicalizeStates" -> False], 2];


p[];
p["=== 3. What an event touched ==="];
(* Elements are tracked by token, minted on creation and carried for as long as the element
   lasts, so that two edges of equal value stay two elements. *)
ev1 = First @ sys["EventList", 1];
check["it consumed the one edge there was", Length @ ev1["ConsumedEdges"], 1];
check["and made the two the rule builds", Length @ ev1["CreatedEdges"], 2];
check["no token is minted twice",
   With[{all = Catenate[#["CreatedEdges"] & /@ sys["EventList", 3]]},
      Length[all] === Length[DeleteDuplicates[all]]],
   True];
check["a vertex the rule creates gets a token",
   Length @ ev1["CreatedVertices"], 1];
check["the vertices the match covered are reported",
   Length @ ev1["MatchedVertices"], 2];
check["nothing was destroyed here", ev1["DestroyedVertices"], {}];

p["-- duplicate edges are distinct elements --"];
dupes = Hypergraph[{{1, 2}, {1, 2}}];
dupSys = HypergraphRewritingSystem[sub, dupes];
check["two identical edges give two events", dupSys["EventCount", 1], 2];
check["and the two events consume different elements",
   Union @@ (#["ConsumedEdges"] & /@ dupSys["EventList", 1]) // Length, 2];


p[];
p["=== 4. Causal dependence, and the option ==="];
(* The headline design point: what counts as a dependence is chosen, not fixed. Two events are
   related when what they touch overlaps, and the option says in what currency. *)
check["the types on offer", $CausalDependenceTypes, {"Elements", "Edges", "Vertices"}];
check["an unknown one falls back to the default with a message",
   Quiet @ pairs @ sys["CausalEdges", 2, "CausalDependence" -> "Nonsense"],
   pairs @ sys["CausalEdges", 2]];

p["-- two subdivisions that meet only at a vertex --"];
(* Subdividing one edge of a two-edge path, then the other: the second event consumes an edge the
   first never touched, but the two matches meet at the vertex between them. *)
twoEdges = Hypergraph[{{1, 2}, {2, 3}}];
meet = HypergraphRewritingSystem[sub, twoEdges];
check["eight events in two generations", meet["EventCount", 2], 8];
check["by elements", pairs @ meet["CausalEdges", 2],
   {{1, 3}, {1, 4}, {1, 5}, {2, 6}, {2, 7}, {2, 8}}];
check["by whole edges, fewer",
   pairs @ meet["CausalEdges", 2, "CausalDependence" -> "Edges"],
   {{1, 3}, {1, 5}, {2, 7}, {2, 8}}];
check["by vertices alone",
   pairs @ meet["CausalEdges", 2, "CausalDependence" -> "Vertices"],
   {{1, 3}, {1, 4}, {1, 5}, {2, 6}, {2, 7}, {2, 8}}];
check["the pairs that only share a vertex",
   Complement[pairs @ meet["CausalEdges", 2],
              pairs @ meet["CausalEdges", 2, "CausalDependence" -> "Edges"]],
   {{1, 4}, {2, 6}}];
check["  and those really do share no edge",
   Union[#["SharedEdges"] & /@ Select[meet["CausalEdges", 2], MemberQ[{{1, 4}, {2, 6}}, {#["From"], #["To"]}] &]],
   {{}}];

p["-- the three types stand in a fixed relation --"];
byType[s_, n_, t_] := pairs @ s["CausalEdges", n, "CausalDependence" -> t];
check["edges are a subset of elements",
   SubsetQ[byType[meet, 2, "Elements"], byType[meet, 2, "Edges"]], True];
check["vertices are a subset of elements",
   SubsetQ[byType[meet, 2, "Elements"], byType[meet, 2, "Vertices"]], True];
check["and elements is exactly their union",
   Sort @ byType[meet, 2, "Elements"],
   Sort @ Union[byType[meet, 2, "Edges"], byType[meet, 2, "Vertices"]]];

p["-- what a causal edge reports --"];
first = First @ meet["CausalEdges", 2];
check["the keys of a causal edge", Keys[first],
   {"From", "To", "SharedEdges", "SharedVertices"}];
check["no causal edge is empty",
   AllTrue[meet["CausalEdges", 2], #["SharedEdges"] =!= {} || #["SharedVertices"] =!= {} &], True];
check["every causal edge runs from an earlier generation to a later one",
   With[{evs = meet["EventList", 2]},
      AllTrue[meet["CausalEdges", 2],
         evs[[#["From"], "Generation"]] < evs[[#["To"], "Generation"]] &]],
   True];
check["and only between events on one path",
   With[{evs = meet["EventList", 2]},
      AllTrue[meet["CausalEdges", 2],
         evs[[#["From"], "OutputState"]] === evs[[#["To"], "InputState"]] ||
         evs[[#["From"], "Generation"]] + 1 < evs[[#["To"], "Generation"]] &]],
   True];
check["events on different branches are never related",
   pairs @ meet["CausalEdges", 1], {}];


p[];
p["=== 5. The graphs ==="];
check["the causal graph has one vertex per event",
   VertexCount @ sys["CausalGraph", 3], sys["EventCount", 3]];
check["transitive reduction is on by default, and can be turned off",
   {EdgeCount @ meet["CausalGraph", 2],
    EdgeCount @ meet["CausalGraph", 2, "TransitiveReduction" -> False]},
   {6, 6}];
check["the evolution graph is the tree",
   {VertexCount @ sys["EvolutionGraph", 2], EdgeCount @ sys["EvolutionGraph", 2]},
   {sys["StateCount", 2], sys["EventCount", 2]}];
check["  and it really is a tree", AcyclicGraphQ @ sys["EvolutionGraph", 3], True];
check["the states graph merges equal states",
   {VertexCount @ flipSys["StatesGraph", 3],
    VertexCount @ flipSys["StatesGraph", 3, "CanonicalizeStates" -> False]},
   {1, 2}];
check["the branchial graph joins the states of one generation with a common parent",
   {VertexCount @ meet["BranchialGraph", 1], EdgeCount @ meet["BranchialGraph", 1]},
   {2, 1}];
check["the evolution-causal graph carries states and events both",
   VertexCount @ sys["EvolutionCausalGraph", 2],
   sys["StateCount", 2] + sys["EventCount", 2]];
check["all of them are graphs",
   GraphQ /@ {sys["CausalGraph", 2], sys["EvolutionGraph", 2], sys["StatesGraph", 2],
              sys["BranchialGraph", 2], sys["EvolutionCausalGraph", 2]},
   {True, True, True, True, True}];

p["-- the standalone form --"];
check["HypergraphCausalGraph on a system",
   HypergraphCausalGraph[sys, 2] === sys["CausalGraph", 2], True];
check["and on rules and an initial state",
   HypergraphCausalGraph[sub, single, 2] === sys["CausalGraph", 2], True];
check["it takes the option too",
   EdgeCount @ HypergraphCausalGraph[sub, twoEdges, 2, "CausalDependence" -> "Edges"],
   EdgeCount @ meet["CausalGraph", 2, "CausalDependence" -> "Edges"]];
check["and refuses what is not a system",
   Quiet @ HypergraphCausalGraph["nonsense"], $Failed];


p[];
p["=== 6. The reference notebooks' systems ==="];
(* The rules of reference/Chemoids.wl, evolved. Each is a section of
   notebooks/CausalityBasics.nb. *)

p["-- composition along a chain --"];
comp = HypergraphRule[
   {Edge[{1, 2}, "Directed", x], Edge[{2, 3}, "Directed", y]},
   {Edge[{1, 3}, "Directed", Row[{"(", x, y, ")"}]]}];
chain4 = Hypergraph[{Edge[{1, 2}, "Directed", a], Edge[{2, 3}, "Directed", b],
                     Edge[{3, 4}, "Directed", c], Edge[{4, 5}, "Directed", d]}];
chainSys = HypergraphRewritingSystem[comp, chain4];
check["three places to compose, then two from each",
   {chainSys["EventCount", 1], chainSys["EventCount", 2]}, {3, 9}];
check["the five bracketings of four arrows",
   Sort @ DeleteDuplicates @ Select[labelsOf[#["Data", "Hypergraph"]] & /@ chainSys["EventList", 3],
                                    Length[#] === 1 &],
   Sort[{{Row[{"(", Row[{"(", Row[{"(", a, b, ")"}], c, ")"}], d, ")"}]},
         {Row[{"(", Row[{"(", a, Row[{"(", b, c, ")"}], ")"}], d, ")"}]},
         {Row[{"(", Row[{"(", a, b, ")"}], Row[{"(", c, d, ")"}], ")"}]},
         {Row[{"(", a, Row[{"(", Row[{"(", b, c, ")"}], d, ")"}], ")"}]},
         {Row[{"(", a, Row[{"(", b, Row[{"(", c, d, ")"}], ")"}], ")"}]}}]];
check["composing the two ends is independent by arrows",
   Complement[byType[chainSys, 2, "Elements"], byType[chainSys, 2, "Edges"]],
   {{1, 5}, {3, 8}}];
check["  and those two pairs are the ones that meet only at an object",
   Union[#["SharedVertices"] === {} & /@
      Select[chainSys["CausalEdges", 2], MemberQ[{{1, 5}, {3, 8}}, {#["From"], #["To"]}] &]],
   {False}];

p["-- zig-zag composition --"];
zig = HypergraphRule[
   {Edge[{1, 2}, "Directed", x], Edge[{3, 2}, "Directed", y], Edge[{3, 4}, "Directed", z]},
   {Edge[{1, 4}, "Directed", Row[{"(", x, y, z, ")"}]]}];
long = Hypergraph[{Edge[{1, 2}, "Directed", a], Edge[{3, 2}, "Directed", b],
                   Edge[{3, 4}, "Directed", c], Edge[{5, 4}, "Directed", d],
                   Edge[{5, 6}, "Directed", e]}];
zigSys = HypergraphRewritingSystem[zig, long];
check["three ways in, one way on from each",
   {zigSys["EventCount", 1], zigSys["EventCount", 2]}, {3, 6}];
check["the three bracketings of heap associativity",
   Sort @ DeleteDuplicates @ Select[labelsOf[#["Data", "Hypergraph"]] & /@ zigSys["EventList", 2],
                                    Length[#] === 1 &],
   Sort[{{Row[{"(", Row[{"(", a, b, c, ")"}], d, e, ")"}]},
         {Row[{"(", a, b, Row[{"(", c, d, e, ")"}], ")"}]},
         {Row[{"(", a, Row[{"(", d, c, b, ")"}], e, ")"}]}}]];
check["here every step consumes an arrow the last one made",
   byType[zigSys, 2, "Elements"], byType[zigSys, 2, "Edges"]];

p["-- the unit and composition together --"];
unit = HypergraphRule[
   Hypergraph[{Vertex[1, p0]}, {}],
   Hypergraph[{Vertex[1, p0], Vertex[2, p0]}, {Edge[{1, 2}, "Directed", Subscript["1", p0]]}]];
arrow = Hypergraph[{Vertex[1, r0], Vertex[2, s0]}, {Edge[{1, 2}, "Directed", f]}];
catSys = HypergraphRewritingSystem[{unit, comp}, arrow];
check["two rules, and the events say which fired",
   Tally[#["RuleIndex"] & /@ catSys["EventList", 2]], {{1, 8}, {2, 1}}];
(* Of the two units, one builds its arrow at the source of f and one at the target; only the
   second gives a composable pair, so only that one is what the composition depends on. *)
check["the one composition depends on the unit that made its arrow",
   byType[catSys, 2, "Edges"], {{2, 9}}];
check["  where by elements more is related",
   Length @ byType[catSys, 2, "Elements"], 5];


p[];
p["=== 7. Invariants over generated systems ==="];
(* Rules built out of the edges a hypergraph actually has, so that they match. *)
SeedRandom[20260930];
randomRule[hg_] := Module[{es = EdgeList[hg], chosen, ids, ren},
   chosen = RandomSample[es, RandomInteger[{1, Min[2, Length[es]]}]];
   ids = DeleteDuplicates[VertexID /@ Catenate[EdgeVertices /@ chosen]];
   ren = AssociationThread[ids -> Range[Length[ids]]];
   HypergraphRule[
      Edge[Lookup[ren, VertexID /@ EdgeVertices[#]], EdgeSymmetry[#]] & /@ chosen,
      RandomChoice[{
         {Edge[Reverse[Range[Length[ids]]], "Directed"]},
         {Edge[Range[Length[ids]], "Directed"], Edge[{1, Length[ids] + 1}, "Directed"]},
         {}
      }]
   ]
];
randomState[] := Hypergraph[
   Table[RandomSample[Range[4], RandomInteger[{1, 3}]], RandomInteger[{2, 4}]]];

$trials = 0; $causal = 0; $bad = {};
Do[
   Module[{hg = randomState[], r, s, evs, ce},
      r = randomRule[hg];
      If[! HypergraphRuleQ[r], Continue[]];
      s = HypergraphRewritingSystem[r, hg];
      If[! HypergraphRewritingSystemQ[s], Continue[]];
      $trials++;
      evs = s["EventList", 2];
      If[evs === {}, Continue[]];
      Do[
         ce = s["CausalEdges", 2, "CausalDependence" -> t];
         $causal += Length[ce];
         If[! AllTrue[ce,
               evs[[#["From"], "Generation"]] < evs[[#["To"], "Generation"]] &&
               MemberQ[Range[Length[evs]], #["From"]] && MemberQ[Range[Length[evs]], #["To"]] &&
               (#["SharedEdges"] =!= {} || #["SharedVertices"] =!= {}) &],
            AppendTo[$bad, {"order", t, hg, r}]];
         If[t === "Edges" && ! AllTrue[ce, #["SharedVertices"] === {} &],
            AppendTo[$bad, {"edges carry vertices", t, hg, r}]];
         If[t === "Vertices" && ! AllTrue[ce, #["SharedEdges"] === {} &],
            AppendTo[$bad, {"vertices carry edges", t, hg, r}]],
         {t, $CausalDependenceTypes}
      ];
      If[! SubsetQ[pairs @ s["CausalEdges", 2],
                   pairs @ s["CausalEdges", 2, "CausalDependence" -> "Edges"]],
         AppendTo[$bad, {"edges not a subset", hg, r}]];
      If[Sort @ pairs @ s["CausalEdges", 2] =!=
         Sort @ Union[pairs @ s["CausalEdges", 2, "CausalDependence" -> "Edges"],
                      pairs @ s["CausalEdges", 2, "CausalDependence" -> "Vertices"]],
         AppendTo[$bad, {"elements not the union", hg, r}]];
      If[s["StateCount", 2] =!= s["EventCount", 2] + 1,
         AppendTo[$bad, {"state count", hg, r}]]
   ],
   {120}
];
p["  systems built: ", $trials, "   causal edges checked: ", $causal];
check["every invariant held", $bad, {}];


p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];

#!/usr/bin/env wolframscript
$ProgressReporting = False;

(* Assertions for hypergraph rewriting.
   Run with:  wolframscript -file tests/RewritingExamples.wls  *)

Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "src", "Hypergraph.wl"}]];

p = Print;
form[x_] := ToString[x, InputForm];
$fails = 0;
check[lbl_, got_, want_] := (
   If[got =!= want, $fails++];
   p[If[got === want, "  ok   ", "  FAIL "], StringPadRight[lbl, 54], " ", form[got],
     If[got === want, "", "   (expected " <> form[want] <> ")"]]
);
show[lbl_, x_] := p[StringPadRight[lbl, 50], " ", form[x]];

ed[hg_] := Sort[VertexID /@ EdgeVertices[#] & /@ EdgeList[hg]];
vs[hg_] := VertexID /@ VertexList[hg];


p["=== 1. The rule object ==="];
sub = HypergraphRule[{{1, 2}}, {{1, 3}, {3, 2}}];
check["HypergraphRuleQ", HypergraphRuleQ[sub], True];
check["both sides are hypergraphs",
   {HypergraphQ @ sub["Input"], HypergraphQ @ sub["Output"]}, {True, True}];
check["the sides may be given as anything Hypergraph takes",
   HypergraphRule[{{1, 2}}, {{2, 1}}] === HypergraphRule[Hypergraph[{{1, 2}}], Hypergraph[{{2, 1}}]],
   True];
check["which vertices it keeps, creates and destroys",
   {sub["SharedVertices"], sub["NewVertices"], sub["DeletedVertices"]}, {{1, 2}, {3}, {}}];
check["a rule that destroys one", HypergraphRule[{{1, 2}}, {{1}}]["DeletedVertices"], {2}];
check["properties", sub["Properties"],
   {"Input", "Output", "SharedVertices", "NewVertices", "DeletedVertices", "Signature"}];
check["an unknown property", Head @ sub["Nonsense"], Missing];
check["a side that is not a hypergraph is refused",
   Quiet @ HypergraphRule[{{1, 2}}, "nope"], $Failed];
check["so is a malformed rule", Quiet @ HypergraphRule[1, 2, 3], $Failed];
check["HypergraphRuleQ of anything else", HypergraphRuleQ[{{1, 2}} -> {{2, 1}}], False];


p[];
p["=== 2. Matching ==="];
path2 = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}];
check["one edge matches each edge of a two-edge path",
   Length @ HypergraphRuleMatches[sub, path2], 2];
check["a match names the positions it used and the binding it made",
   Keys @ First @ HypergraphRuleMatches[sub, path2],
   {"MatchEdgePositions", "RuleVertexMap"}];
check["the binding of the first match",
   First[HypergraphRuleMatches[sub, path2]]["RuleVertexMap"], <|1 -> 1, 2 -> 2|>];
check["a two-edge pattern matches the two consecutive pairs",
   #["MatchEdgePositions"] & /@ HypergraphRuleMatches[
      HypergraphRule[{{1, 2}, {2, 3}}, {{1, 3}}],
      Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{3, 4}]}]],
   {{1, 2}, {2, 3}}];
check["a pattern with more edges than the hypergraph matches nothing",
   HypergraphRuleMatches[HypergraphRule[{{1, 2}, {2, 3}}, {}], Hypergraph[{Edge[{1, 2}]}]], {}];
check["an edge is used by at most one pattern edge",
   HypergraphRuleMatches[HypergraphRule[{{1, 2}, {1, 2}}, {}], Hypergraph[{Edge[{1, 2}]}]], {}];
check["  but a repeated edge gives it two to use",
   Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}, {1, 2}}, {}],
      Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}]],
   2];

p["-- symmetry types have to agree --"];
check["a directed pattern does not match an unordered edge",
   HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed"]}, {}],
      Hypergraph[{Edge[{1, 2}, "Unordered"]}]],
   {}];
check["an unordered pattern matches an unordered edge both ways round",
   Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Unordered"]}, {}],
      Hypergraph[{Edge[{1, 2}, "Unordered"]}]],
   2];
check["a cyclic pattern of arity three matches its three rotations",
   Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2, 3}, "Cyclic"]}, {}],
      Hypergraph[{Edge[{1, 2, 3}, "Cyclic"]}]],
   3];
check["  where an unordered one of arity three matches all six orderings",
   Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2, 3}, "Unordered"]}, {}],
      Hypergraph[{Edge[{1, 2, 3}, "Unordered"]}]],
   6];
check["a directed pattern matches one ordering only",
   Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2, 3}, "Directed"]}, {}],
      Hypergraph[{Edge[{1, 2, 3}, "Directed"]}]],
   1];

p["-- distinct vertices --"];
check["two pattern vertices do not fold onto one",
   HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], Hypergraph[{Edge[{1, 1}]}]], {}];
check["  unless the rule is told they may",
   Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], Hypergraph[{Edge[{1, 1}]}],
      "DistinctVertices" -> False],
   1];


p[];
p["=== 3. Applying ==="];
ev = sub[path2];
check["one event per match", Length[ev], 2];
check["the keys of an event", Keys @ First[ev],
   {"Hypergraph", "MatchVertices", "MatchEdges", "MatchEdgePositions", "NewVertices",
    "NewEdges", "DeletedVertices", "RuleVertexMap", "Rule"}];
check["rule[hg] and HypergraphRuleApply agree", sub[path2] === HypergraphRuleApply[sub, path2], True];
check["subdividing the first edge", ed @ ev[[1, "Hypergraph"]],
   Sort[{{1, \[FormalV][1]}, {\[FormalV][1], 2}, {2, 3}}]];
check["subdividing the second", ed @ ev[[2, "Hypergraph"]],
   Sort[{{1, 2}, {2, \[FormalV][1]}, {\[FormalV][1], 3}}]];
check["the new vertex is named so as to miss what is already there",
   #["NewVertices"] & /@ ev, {{\[FormalV][1]}, {\[FormalV][1]}}];
check["and it really is new", MemberQ[vs[path2], \[FormalV][1]], False];

p["-- what the event says changed --"];
e1 = First[ev];
check["the edges it matched", VertexID /@ EdgeVertices[#] & /@ e1["MatchEdges"], {{1, 2}}];
check["the vertices it matched", e1["MatchVertices"], {1, 2}];
check["the edges it made", VertexID /@ EdgeVertices[#] & /@ e1["NewEdges"],
   {{1, \[FormalV][1]}, {\[FormalV][1], 2}}];
check["it destroyed nothing", e1["DeletedVertices"], {}];
check["the map from the rule's vertices to the hypergraph's",
   e1["RuleVertexMap"], <|1 -> 1, 2 -> 2, 3 -> \[FormalV][1]|>];
check["the event carries the rule that made it", e1["Rule"] === sub, True];

p["-- destroying --"];
del = HypergraphRule[{{1, 2}}, {{1}}];
check["a vertex nothing else uses goes",
   vs @ First[del[Hypergraph[{Edge[{1, 2}]}]]]["Hypergraph"], {1}];
check["  and is reported",
   First[del[Hypergraph[{Edge[{1, 2}]}]]]["DeletedVertices"], {2}];
check["a vertex another edge still uses stays",
   vs @ First[del[Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]]]["Hypergraph"], {1, 2, 3}];
check["  and is not reported destroyed",
   First[del[Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]]]["DeletedVertices"], {}];
check["an empty output deletes the edge outright",
   ed @ First[HypergraphRule[{{1, 2}}, {}][Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}]]]["Hypergraph"],
   {{2, 3}}];

p["-- labels --"];
hl = Hypergraph[{Edge[{1, 2}, "Directed", "a"], Edge[{2, 3}, "Directed", "b"]}];
check["an unlabelled pattern is a wildcard on labels",
   Length @ HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], hl], 2];
check["a labelled pattern matches only that label",
   First[HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", "a"]}, {}], hl]]["MatchEdgePositions"],
   {1}];
check["a label nothing carries matches nothing",
   HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", "c"]}, {}], hl], {}];
check["\"Labeled\" -> False ignores labels altogether",
   Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", "c"]}, {}], hl,
      "Labeled" -> False],
   2];
check["the output's labels are carried onto the edges it makes",
   EdgeLabel /@ EdgeList @ First[
      HypergraphRule[{{1, 2}}, {Edge[{1, 2}, "Directed", "fresh"]}][Hypergraph[{Edge[{1, 2}]}]]]["Hypergraph"],
   {"fresh"}];

p["-- nothing to do, and nothing given --"];
check["no match gives no events, which is not a failure",
   HypergraphRule[{{1, 2}, {2, 3}}, {}][Hypergraph[{Edge[{1, 2}]}]], {}];
check["applying something that is not a rule",
   Quiet @ HypergraphRuleApply[{1, 2}, Hypergraph[{Edge[{1, 2}]}]], $Failed];
check["applying to something that is not a hypergraph",
   Quiet @ HypergraphRuleApply[sub, "nope"], $Failed];


p[];
p["=== 4. Invariants, on random rules and hypergraphs ==="];
(* These are the things that must hold of every event, whatever the rule and whatever it is
   applied to, and they are what would catch a matcher that bound inconsistently or an
   application that lost or invented an edge. *)

canonEdge[e_] := {WolframInstitute`Hypergraphs`Private`arrayCanonicalIndex[
      VertexID /@ EdgeVertices[e],
      Switch[EdgeSymmetry[e], "Directed", "Rigid", "Cyclic", "Cyclic", _, "Symmetric"]],
   EdgeSymmetry[e]};

randomHypergraph[] := Hypergraph @ Table[
   Edge[RandomInteger[{1, 4}, RandomInteger[{1, 3}]],
        RandomChoice[{"Directed", "Unordered", "Cyclic"}]],
   RandomInteger[{1, 4}]];

(* A rule built out of edges the hypergraph actually has, with their vertices renamed, so that it
   is sure to match at least once. A rule drawn at random almost never does, and invariants that
   are never exercised prove nothing. *)
randomMatchingRule[hg_] := Module[{chosen, rename, input},
   chosen = RandomSample[EdgeList[hg], RandomInteger[{1, Min[2, EdgeCount[hg]]}]];
   With[{ids = DeleteDuplicates[VertexID /@ Catenate[EdgeVertices /@ chosen]]},
      rename = AssociationThread[ids -> Range[Length[ids]]]];
   input = Edge[Lookup[rename, Key[#]] & /@ (VertexID /@ EdgeVertices[#]),
                EdgeSymmetry[#], EdgeLabel[#]] & /@ chosen;
   HypergraphRule[
      input,
      Table[Edge[RandomInteger[{1, Length[rename] + 1}, RandomInteger[{1, 2}]],
                 RandomChoice[{"Directed", "Unordered", "Cyclic"}]], RandomInteger[{0, 2}]]]
];

SeedRandom[20260926];
trials = Table[
   Module[{hg = randomHypergraph[], r, events},
      r = randomMatchingRule[hg];
      events = r[hg];
      {hg, r, events}
   ],
   200];

show["hypergraph/rule pairs tried", Length[trials]];
show["of which produced at least one event", Count[trials, {_, _, {__}}]];
show["events in all", Total[Length[#[[3]]] & /@ trials]];

check["every match names as many positions as the input has edges",
   Union @ Catenate @ Map[
      Function[t, Length[#["MatchEdgePositions"]] === EdgeCount[t[[2]]["Input"]] & /@ t[[3]]],
      trials],
   {True}];

check["the positions of a match are distinct and in range",
   Union @ Catenate @ Map[
      Function[t, With[{n = EdgeCount[t[[1]]]},
         DuplicateFreeQ[#["MatchEdgePositions"]] && AllTrue[#["MatchEdgePositions"], 1 <= # <= n &] & /@ t[[3]]]],
      trials],
   {True}];

(* The real test of the matcher: pushing the input's edges through the binding must give back
   exactly the edges the match claims to have used, as edges rather than as vertex lists, so
   that symmetry is accounted for. *)
check["the binding carries the input's edges onto the ones matched",
   Union @ Catenate @ Map[
      Function[t, Function[ev,
         Sort[canonEdge /@ ev["MatchEdges"]] === Sort[canonEdge /@ (
            Edge[Lookup[ev["RuleVertexMap"], Key[#]] & /@ (VertexID /@ EdgeVertices[#]),
                 EdgeSymmetry[#], EdgeLabel[#]] & /@ EdgeList[t[[2]]["Input"]])]
      ] /@ t[[3]]],
      trials],
   {True}];

check["the result has as many edges as it should",
   Union @ Catenate @ Map[
      Function[t, With[{r = t[[2]], hg = t[[1]]},
         EdgeCount[#["Hypergraph"]] === EdgeCount[hg] - EdgeCount[r["Input"]] + EdgeCount[r["Output"]] & /@ t[[3]]]],
      trials],
   {True}];

check["every result is a valid hypergraph",
   Union @ Catenate @ Map[Function[t, HypergraphQ[#["Hypergraph"]] & /@ t[[3]]], trials],
   {True}];

check["nothing reported destroyed survives in the result",
   Union @ Catenate @ Map[
      Function[t, Function[ev,
         Intersection[ev["DeletedVertices"], vs[ev["Hypergraph"]]] === {}] /@ t[[3]]],
      trials],
   {True}];

check["every vertex the output introduced is in the result and was not in the host",
   Union @ Catenate @ Map[
      Function[t, Function[ev,
         With[{new = ev["NewVertices"]},
            Intersection[new, vs[t[[1]]]] === {} &&
            (new === {} || SubsetQ[vs[ev["Hypergraph"]], new])]] /@ t[[3]]],
      trials],
   {True}];

check["the edges the host kept are still there",
   Union @ Catenate @ Map[
      Function[t, Function[ev,
         SubsetQ[
            Sort[canonEdge /@ EdgeList[ev["Hypergraph"]]],
            Sort[canonEdge /@ Delete[EdgeList[t[[1]]], List /@ ev["MatchEdgePositions"]]]]] /@ t[[3]]],
      trials],
   {True}];

check["matching is unaffected by the order the edges were written in",
   Union @ Table[
      With[{es = Table[Edge[RandomInteger[{1, 3}, 2], "Directed"], RandomInteger[{2, 4}]]},
         Length[sub[Hypergraph[es]]] === Length[sub[Hypergraph[RandomSample[es]]]]],
      100],
   {True}];


p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];

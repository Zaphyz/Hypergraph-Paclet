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
   {"MatchEdgePositions", "RuleVertexMap", "LabelBindings"}];
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
   {"Hypergraph", "Input", "EdgeOrigins", "MatchVertices", "MatchEdges", "MatchEdgePositions", "NewVertices",
    "NewEdges", "DeletedVertices", "RuleVertexMap", "LabelBindings", "Rule"}];
check["rule[hg] and HypergraphRuleApply agree", sub[path2] === HypergraphRuleApply[sub, path2], True];
check["subdividing the first edge", ed @ ev[[1, "Hypergraph"]],
   Sort[{{1, \[FormalV][1]}, {\[FormalV][1], 2}, {2, 3}}]];
check["subdividing the second", ed @ ev[[2, "Hypergraph"]],
   Sort[{{1, 2}, {2, \[FormalV][1]}, {\[FormalV][1], 3}}]];
check["the new vertex is named so as to miss what is already there",
   #["NewVertices"] & /@ ev, {{\[FormalV][1]}, {\[FormalV][1]}}];
check["and it really is new", MemberQ[vs[path2], \[FormalV][1]], False];

p["-- where each edge of the result came from --"];
(* "EdgeOrigins" is what lets the causal machinery follow an edge across a rewrite: the
   constructor sorts the edges, so a position in the result says nothing on its own. *)
check["one entry per edge of the result",
   Length /@ (#["EdgeOrigins"] & /@ ev), EdgeCount[#["Hypergraph"]] & /@ ev];
check["None exactly where the rule made an edge",
   Count[#["EdgeOrigins"], None] & /@ ev, Length[#["NewEdges"]] & /@ ev];
check["the kept origins are the host positions the match left alone",
   Sort @ DeleteCases[First[ev]["EdgeOrigins"], None],
   Complement[Range[EdgeCount[path2]], First[ev]["MatchEdgePositions"]]];
check["and each carried-over edge really is the host edge it points at",
   With[{host = EdgeList[path2]},
      And @@ MapThread[
         Function[{o, e}, o === None || (VertexID /@ EdgeVertices[host[[o]]] === VertexID /@ EdgeVertices[e])],
         {First[ev]["EdgeOrigins"], EdgeList[First[ev]["Hypergraph"]]}]],
   True];
(* Duplicate edges are the case this exists for: two edges of equal value are still two
   elements, and consuming one has to leave the other identifiable. *)
dupes = Hypergraph[{{1, 2}, {1, 2}, {2, 3}}];
check["with duplicate edges, consuming one leaves the other's position",
   DeleteCases[#["EdgeOrigins"], None] & /@ sub[dupes], {{2, 3}, {1, 3}, {1, 2}}];

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
p["=== 5. The worked examples of notebooks/RewritingBasics.nb ==="];

p["-- growing a path by subdividing repeatedly --"];
grown = NestList[First[sub[#]]["Hypergraph"] &, Hypergraph[{Edge[{1, 2}]}], 3];
check["each step adds an edge", EdgeCount /@ grown, {1, 2, 3, 4}];
check["  and a vertex", VertexCount /@ grown, {2, 3, 4, 5}];
check["the new vertices do not collide from one step to the next",
   DuplicateFreeQ @ Cases[vs @ Last[grown], _\[FormalV]], True];
check["and the result really is a path: no vertex in more than two edges, two in just one",
   With[{degrees = Last /@ Tally[Flatten[ed @ Last[grown]]]},
      {Max[degrees], Count[degrees, 1]}],
   {2, 2}];

p["-- every state one step away from a star --"];
star = Hypergraph[{Edge[{1, 2}], Edge[{1, 3}], Edge[{1, 4}]}];
check["one event per edge", Length @ sub[star], 3];
check["the events differ",
   Length @ DeleteDuplicates[#["MatchEdgePositions"] & /@ sub[star]], 3];
check["but their results are all isomorphic",
   Length @ DeleteDuplicates[CanonicalHypergraph[#["Hypergraph"]] & /@ sub[star]], 1];

p["-- an unordered rule --"];
grow = HypergraphRule[{Edge[{1, 2}, "Unordered"]}, {Edge[{1, 2, 3}, "Unordered"]}];
check["two events, from the two orderings the pattern admits",
   Length @ grow[Hypergraph[{Edge[{1, 2}, "Unordered"]}]], 2];
check["  whose results are the same hypergraph",
   Length @ DeleteDuplicates[#["Hypergraph"] & /@ grow[Hypergraph[{Edge[{1, 2}, "Unordered"]}]]], 1];
check["the edge produced is unordered and of arity three",
   {EdgeSymmetry[#], Length @ EdgeVertices[#]} & @
      First @ EdgeList @ First[grow[Hypergraph[{Edge[{1, 2}, "Unordered"]}]]]["Hypergraph"],
   {"Unordered", 3}];

p["-- rewriting only the edges carrying a label --"];
mixed = Hypergraph[{Edge[{1, 2}, "Directed", "x"], Edge[{2, 3}, "Directed", "y"],
                    Edge[{3, 4}, "Directed", "x"]}];
onlyX = HypergraphRule[{Edge[{1, 2}, "Directed", "x"]},
                       {Edge[{1, 3}, "Directed", "x"], Edge[{3, 2}, "Directed", "x"]}];
check["it finds the two edges labelled x and not the one labelled y",
   Length @ onlyX[mixed], 2];
check["  and the edge labelled y is untouched in every result",
   Union @ Map[Function[ev, MemberQ[EdgeLabel /@ EdgeList[ev["Hypergraph"]], "y"]], onlyX[mixed]],
   {True}];
check["each result has one edge more than it started with",
   Union[EdgeCount[#["Hypergraph"]] & /@ onlyX[mixed]], {4}];

p["-- a rule that consumes two edges --"];
contract = HypergraphRule[{{1, 2}, {2, 3}}, {{1, 3}}];
path3 = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{3, 4}]}];
check["two matches on a three-edge path",
   #["MatchEdgePositions"] & /@ contract[path3], {{1, 2}, {2, 3}}];
check["the middle vertex of each goes with them",
   #["DeletedVertices"] & /@ contract[path3], {{2}, {3}}];
check["and each result is a two-edge path",
   ed[#["Hypergraph"]] & /@ contract[path3], {{{1, 3}, {3, 4}}, {{1, 2}, {2, 4}}}];


p[];
p["=== 6. Labels as pattern variables ==="];
(* A bare symbol used as a label in a rule's input is a variable: it matches whatever label it is
   laid against and remembers it, and the output may then be named in terms of it. This is what
   the composition rules of the reference notebooks are written with. *)

p["-- a symbol binds, a string does not --"];
hlab = Hypergraph[{Edge[{1, 2}, "Directed", f], Edge[{2, 3}, "Directed", g]}];
check["a symbol matches any label and remembers it",
   #["LabelBindings"] & /@ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", x]}, {}], hlab],
   {<|x -> f|>, <|x -> g|>}];
check["a string is a literal, as before",
   HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", "f"]}, {}], hlab], {}];
check["  and matches its own literal",
   Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", "a"]}, {}],
      Hypergraph[{Edge[{1, 2}, "Directed", "a"]}]],
   1];
check["no label is still a wildcard that binds nothing",
   #["LabelBindings"] & /@ HypergraphRuleMatches[HypergraphRule[{{1, 2}}, {}], hlab],
   {<||>, <||>}];
check["Verbatim forces a symbol to be read literally",
   {Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", Verbatim[f]]}, {}], hlab],
    Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", Verbatim[zzz]]}, {}], hlab]},
   {1, 0}];
check["\"LabelPatterns\" -> False turns binding off, so a symbol is a literal",
   Length @ HypergraphRuleMatches[HypergraphRule[{Edge[{1, 2}, "Directed", x]}, {}], hlab,
      "LabelPatterns" -> False],
   0];

p["-- one variable, used twice, must find the same label --"];
same = HypergraphRule[{Edge[{1, 2}, "Directed", x], Edge[{2, 3}, "Directed", x]}, {}];
check["two edges labelled differently do not match it",
   HypergraphRuleMatches[same, hlab], {}];
check["  but two labelled the same do",
   Length @ HypergraphRuleMatches[same,
      Hypergraph[{Edge[{1, 2}, "Directed", f], Edge[{2, 3}, "Directed", f]}]],
   1];

p["-- vertex labels bind too --"];
vlab = Hypergraph[{Vertex[1, r], Vertex[2, ss]}, {Edge[{1, 2}, "Directed", ff]}];
check["a vertex label variable binds",
   Sort @ Values[First[HypergraphRuleMatches[
      HypergraphRule[Hypergraph[{Vertex[1, pp], Vertex[2, qq]}, {Edge[{1, 2}, "Directed", xx]}], {}],
      vlab]]["LabelBindings"]],
   Sort[{r, ss, ff}]];

p["-- the output is named in terms of what was found --"];
lineComp = HypergraphRule[
   {Edge[{1, 2}, "Directed", x], Edge[{2, 3}, "Directed", y]},
   {Edge[{1, 3}, "Directed", Row[{"{", x, y, "}"}]]}];
check["composing two labelled edges names the result after them",
   EdgeLabel /@ EdgeList @ First[lineComp[hlab]]["Hypergraph"],
   {Row[{"{", f, g, "}"}]}];
check["and the binding is reported with the event",
   First[lineComp[hlab]]["LabelBindings"], <|x -> f, y -> g|>];

p["-- the reference's unit rule: a free labelled vertex --"];
unit = HypergraphRule[
   Hypergraph[{Vertex[1, pp]}, {}],
   Hypergraph[{Vertex[1, pp], Vertex[2, pp]}, {Edge[{1, 2}, "Unordered", Subscript["1", pp]]}]];
unitEvents = unit[vlab];
check["one event per vertex of the state", Length[unitEvents], 2];
check["binding the vertex label each time",
   #["LabelBindings"] & /@ unitEvents, {<|pp -> r|>, <|pp -> ss|>}];
check["an identity edge named after the object it was found at",
   DeleteDuplicates @ Cases[
      Catenate[EdgeLabel /@ EdgeList[#["Hypergraph"]] & /@ unitEvents], _Subscript],
   {Subscript["1", r], Subscript["1", ss]}];
check["and the created vertex carries that label too",
   VertexLabel /@ Select[VertexList[First[unitEvents]["Hypergraph"]],
      MatchQ[VertexID[#], _\[FormalV]] &],
   {r}];

p["-- the reference's dagger rule --"];
dagger = HypergraphRule[
   Hypergraph[{Vertex[1, pp], Vertex[2, qq]}, {Edge[{1, 2}, "Directed", x]}],
   Hypergraph[{Vertex[1, pp], Vertex[2, qq]}, {Edge[{2, 1}, "Directed", Row[{"(", SuperDagger[x], ")"}]]}]];
dEvent = First @ dagger[vlab];
check["the edge is reversed", VertexID /@ EdgeVertices @ First @ EdgeList[dEvent["Hypergraph"]], {2, 1}];
check["and daggered", EdgeLabel /@ EdgeList[dEvent["Hypergraph"]], {Row[{"(", SuperDagger[ff], ")"}]}];
check["the vertex labels survive", Sort[VertexLabel /@ VertexList[dEvent["Hypergraph"]]], Sort[{r, ss}]];
check["applying it twice gives the involution unreduced",
   EdgeLabel /@ EdgeList @ First[dagger[dEvent["Hypergraph"]]]["Hypergraph"],
   {Row[{"(", SuperDagger[Row[{"(", SuperDagger[ff], ")"}]], ")"}]}];

p["-- an output label may relabel a vertex --"];
relabel = HypergraphRule[
   Hypergraph[{Vertex[1, pp], Vertex[2, qq]}, {Edge[{1, 2}, "Directed", x]}],
   Hypergraph[{Vertex[1, qq], Vertex[2, pp]}, {Edge[{1, 2}, "Directed", x]}]];
check["the output's labels win where it gives them",
   VertexLabel /@ VertexList @ First[relabel[vlab]]["Hypergraph"], {ss, r}];


p[];
p["=== 7. The reference notebooks' rules ==="];
(* The rules of reference/Chemoids.wl, and the laws they were written to express. Each of these
   is a cell of notebooks/RewritingBasics.nb section 5. *)

ClearAll[a, b, c, d, e, f, g, p0, q0, r0, s0, u, x, y, z];
labels[ev_] := EdgeLabel /@ EdgeList[ev["Hypergraph"]];
again[rule_, evs_] := Catenate[rule /@ (#["Hypergraph"] & /@ evs)];

arrowComp = HypergraphRule[
   {Edge[{1, 2}, "Directed", x], Edge[{2, 3}, "Directed", y]},
   {Edge[{1, 3}, "Directed", Row[{"(", x, y, ")"}]]}];
arrowUnit = HypergraphRule[
   Hypergraph[{Vertex[1, p0]}, {}],
   Hypergraph[{Vertex[1, p0], Vertex[2, p0]}, {Edge[{1, 2}, "Directed", Subscript["1", p0]]}]];
arrow = Hypergraph[{Vertex[1, r0], Vertex[2, s0]}, {Edge[{1, 2}, "Directed", f]}];

p["-- composition and the unit --"];
check["composition has nothing to do to a single edge", arrowComp[arrow], {}];
check["the unit fires once at each end",
   labels /@ arrowUnit[arrow], {{f, Subscript["1", r0]}, {f, Subscript["1", s0]}}];
check["only the unit at the target composes with f",
   labels /@ again[arrowComp, arrowUnit[arrow]], {{Row[{"(", f, Subscript["1", s0], ")"}]}}];

p["-- the dagger is an antihomomorphism --"];
chain2 = Hypergraph[{Edge[{1, 2}, "Directed", f], Edge[{2, 3}, "Directed", g]}];
dag = HypergraphRule[
   Hypergraph[{Vertex[1, p0], Vertex[2, q0]}, {Edge[{1, 2}, "Directed", x]}],
   Hypergraph[{Vertex[1, p0], Vertex[2, q0]}, {Edge[{2, 1}, "Directed", SuperDagger[x]]}]];
check["compose then dagger gives the dagger of the composite",
   labels /@ again[dag, arrowComp[chain2]], {{SuperDagger[Row[{"(", f, g, ")"}]]}}];
bothDaggered = Select[#["Hypergraph"] & /@ again[dag, dag[chain2]],
   FreeQ[EdgeLabel /@ EdgeList[#], SuperDagger[SuperDagger[_]]] &];
check["dagger both then compose gives the composite of the daggers, reversed",
   DeleteDuplicates[labels /@ Catenate[arrowComp /@ bothDaggered]],
   {{Row[{"(", SuperDagger[g], SuperDagger[f], ")"}]}}];

p["-- an action, and its associativity --"];
action = HypergraphRule[
   {Edge[{1}, "Directed", a], Edge[{1, 2}, "Directed", x]},
   {Edge[{2}, "Directed", Row[{x, "[", a, "]"}]]}];
pointed = Hypergraph[{Edge[{1}, "Directed", u], Edge[{1, 2}, "Directed", f],
                      Edge[{2, 3}, "Directed", g]}];
check["a rule may mix arities: one unary edge and one binary",
   Length @ HypergraphRuleMatches[action, pointed], 1];
check["acting twice walks the point along the chain",
   labels /@ again[action, action[pointed]], {{Row[{g, "[", Row[{f, "[", u, "]"}], "]"}]}}];
check["composing first and acting once gives the other side of the law",
   labels /@ again[action, arrowComp[pointed]], {{Row[{Row[{"(", f, g, ")"}], "[", u, "]"}]}}];

p["-- zig-zag composition and heap associativity --"];
zigzag = HypergraphRule[
   {Edge[{1, 2}, "Directed", x], Edge[{3, 2}, "Directed", y], Edge[{3, 4}, "Directed", z]},
   {Edge[{1, 4}, "Directed", Row[{"(", x, y, z, ")"}]]}];
long = Hypergraph[{Edge[{1, 2}, "Directed", a], Edge[{3, 2}, "Directed", b],
                   Edge[{3, 4}, "Directed", c], Edge[{5, 4}, "Directed", d],
                   Edge[{5, 6}, "Directed", e]}];
check["three windows of three edges fit", Length @ zigzag[long], 3];
check["  and each contracts a different one",
   Sort[Sort /@ (#["DeletedVertices"] & /@ zigzag[long])], {{2, 3}, {3, 4}, {4, 5}}];
check["two steps give the three bracketings of the whole",
   Sort[Flatten[labels /@ again[zigzag, zigzag[long]]]],
   Sort[{Row[{"(", Row[{"(", a, b, c, ")"}], d, e, ")"}],
         Row[{"(", a, b, Row[{"(", c, d, e, ")"}], ")"}],
         Row[{"(", a, Row[{"(", d, c, b, ")"}], e, ")"}]}]];


p[];
p["=== 8. Styling a plot, and drawing rules and events ==="];
(* Highlighting needs two things HypergraphPlot did not have: a style per element, and a layout
   that can be handed to another plot so that before and after line up. *)
styled = Hypergraph[{{1, 2}, {2, 3}, {1, 2}}];
check["a plot still comes out a plot", Head @ HypergraphPlot[styled], Graphics];
check["one directive styles everything",
   Head @ HypergraphPlot[styled, EdgeStyle -> Red, VertexStyle -> Green], Graphics];
check["a style per edge position", Head @ HypergraphPlot[styled, EdgeStyle -> {1 -> Red}], Graphics];
check["a style per edge value", Head @ HypergraphPlot[styled, EdgeStyle -> {{2, 3} -> Red}], Graphics];
check["a fallback and then an override",
   Head @ HypergraphPlot[styled, VertexStyle -> {_ -> Gray, 2 -> Red}], Graphics];
check["an empty list of highlights changes nothing",
   HypergraphPlot[styled, EdgeStyle -> {}] === HypergraphPlot[styled], True];

p["-- coordinates, given and reused --"];
emb = HypergraphEmbedding[styled];
coords = Thread[VertexID /@ VertexList[styled] -> emb];
check["handing a layout back gives the same layout",
   Chop[HypergraphEmbedding[styled, VertexCoordinates -> coords] - emb] // Flatten // Union, {0}];
check["a bare list of positions is taken in vertex order",
   HypergraphEmbedding[styled, VertexCoordinates -> {{0., 0.}, {1., 0.}, {0., 1.}}],
   {{0., 0.}, {1., 0.}, {0., 1.}}];
check["placing some leaves the rest to settle among them",
   Length @ HypergraphEmbedding[styled, VertexCoordinates -> {1 -> {0., 0.}}], 3];
(* The reason coordinates exist at all: the same layout, imposed on a different hypergraph. *)
check["another hypergraph can be drawn on the same layout",
   HypergraphEmbedding[Hypergraph[{{1, 2}, {2, 3}, {1, 3}}], VertexCoordinates -> coords], emb];

p["-- rules and events as pictures --"];
check["a rule draws", Head @ HypergraphRulePlot[sub], Graphics];
check["an event draws", Head @ HypergraphEventPlot[First @ sub[path2]], Graphics];
check["one picture per match",
   Length @ HypergraphEventPlot[sub, path2], Length @ sub[path2]];
check["an event carries what it acted on", First[sub[path2]]["Input"], path2];
check["  which is what lets it be drawn on its own",
   Head @ HypergraphEventPlot[KeyDrop[First @ sub[path2], "Rule"]], Graphics];
check["and the created edges are found by origin, not by looking alike",
   Flatten @ Position[First[sub[Hypergraph[{{1, 2}, {1, 2}}]]]["EdgeOrigins"], None],
   {2, 3}];
check["a non-event is refused", Quiet @ HypergraphEventPlot[42], $Failed];
check["a non-rule is refused", Quiet @ HypergraphRulePlot[42], $Failed];

p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];

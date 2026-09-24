#!/usr/bin/env wolframscript
$ProgressReporting = False;

(* Basic examples and checks for src/Hypergraph.wl.
   Run with:  wolframscript -file tests/BasicExamples.wls  *)

Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "src", "Hypergraph.wl"}]];

p = Print;
form[x_] := ToString[x, InputForm];
show[lbl_, x_] := p[StringPadRight[lbl, 46], " ", form[x]];
$fails = 0;
check[lbl_, got_, want_] := (
   If[got =!= want, $fails++];
   p[If[got === want, "  ok   ", "  FAIL "], StringPadRight[lbl, 52], " ", form[got],
     If[got === want, "", "   (expected " <> form[want] <> ")"]]
);

p["=== 1. Vertex objects ==="];
show["Vertex[1]", Vertex[1]];
show["Vertex[1, \"a\"]", Vertex[1, "a"]];
show["Vertex[1, None]  (label elided)", Vertex[1, None]];
show["VertexID / VertexLabel of Vertex[x, \"a\"]", {VertexID[Vertex[x, "a"]], VertexLabel[Vertex[x, "a"]]}];
show["VertexLabel[Vertex[1]]", VertexLabel[Vertex[1]]];
check["Vertex[1, None] === Vertex[1]", Vertex[1, None] === Vertex[1], True];

p[];
p["=== 2. Edge objects: defaults and symmetry canonicalization ==="];
show["Edge[{1, 2, 3}]  (directed by default)", Edge[{1, 2, 3}]];
show["Edge[{1, 2, 3}, \"Unordered\"]", Edge[{1, 2, 3}, "Unordered"]];
show["Edge[{1, 2, 3}, \"Cyclic\", \"f\"]", Edge[{1, 2, 3}, "Cyclic", "f"]];
show["Edge[{3, 1, 2}, \"Cyclic\"]  (rotated)", Edge[{3, 1, 2}, "Cyclic"]];
show["Edge[{3, 2, 1}, \"Unordered\"]  (sorted)", Edge[{3, 2, 1}, "Unordered"]];
show["accessors of Edge[{1,2},\"Cyclic\",\"f\"]",
   {EdgeVertices[#], EdgeSymmetry[#], EdgeLabel[#]} & @ Edge[{1, 2}, "Cyclic", "f"]];

p[];
p["-- two Edge objects are identical exactly when they are the same hyperedge --"];
check["unordered {1,2,3} == {3,2,1}",
   Edge[{1, 2, 3}, "Unordered"] === Edge[{3, 2, 1}, "Unordered"], True];
check["cyclic {1,2,3} == {3,1,2}  (a rotation)",
   Edge[{1, 2, 3}, "Cyclic"] === Edge[{3, 1, 2}, "Cyclic"], True];
check["cyclic {1,2,3} != {1,3,2}  (a reflection)",
   Edge[{1, 2, 3}, "Cyclic"] === Edge[{1, 3, 2}, "Cyclic"], False];
check["directed {1,2,3} != {3,2,1}",
   Edge[{1, 2, 3}] === Edge[{3, 2, 1}], False];
check["symmetry type is part of edge identity",
   Edge[{1, 2}] === Edge[{1, 2}, "Unordered"], False];
check["edge label is part of edge identity",
   Edge[{1, 2}, "Directed", "f"] === Edge[{1, 2}], False];
check["bad symmetry type is rejected",
   Quiet @ Edge[{1, 2}, "Sideways"], $Failed];

p[];
p["=== 3. Hypergraph constructors ==="];
h1 = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}], Edge[{1, 2, 3}, "Unordered"]}];
show["from a list of Edge objects", h1];
p["    VertexList  ", form @ VertexList[h1]];
p["    EdgeList    ", form @ EdgeList[h1]];

h2 = Hypergraph[{1, 2, 3}, {{1}, {1, 2}, {1, 2, 3}}];
show["from vertex list + lists of vertices", h2];

h3 = Hypergraph[{Vertex[1, "a"], Vertex[2, "b"], 3}, {{1, 2}, {2, 3}}];
show["with vertex labels", h3];

h4 = Hypergraph[{1, 2, 3, 4}];
show["edgeless (isolated vertices)", h4];

show["Hypergraph[{}] (empty)", Hypergraph[{}]];
check["HypergraphQ", HypergraphQ[h1], True];

p[];
p["-- the vertex set absorbs vertices that only occur in edges --"];
show["Hypergraph[{Edge[{1,2}]}] vertex ids", Hypergraph[{Edge[{1, 2}]}]["VertexIDs"]];
p[];
p["-- a label given anywhere propagates to every occurrence of that vertex --"];
show["Hypergraph[{Edge[{Vertex[1,\"a\"],2}],Edge[{1,3}]}]",
   Hypergraph[{Edge[{Vertex[1, "a"], 2}], Edge[{1, 3}]}]];

p[];
p["-- a hypergraph is a multiset of edges: order does not matter, multiplicity does --"];
check["edge order irrelevant",
   Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}] === Hypergraph[{Edge[{2, 3}], Edge[{1, 2}]}], True];
check["repeated edge is kept",
   EdgeCount @ Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}], 2];
check["repeated edge differs from single",
   Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}] === Hypergraph[{Edge[{1, 2}]}], False];

p[];
p["=== 4. Properties ==="];
show["h3[\"Properties\"]", h3["Properties"]];
show["h3[\"VertexIDs\"]", h3["VertexIDs"]];
show["h3[\"VertexLabels\"]", h3["VertexLabels"]];
show["h1[\"Arity\"]", h1["Arity"]];
show["h1[\"EdgeSymmetries\"]", h1["EdgeSymmetries"]];
show["h3[\"Skeleton\"]", h3["Skeleton"]];
check["skeleton drops every vertex label",
   Union[VertexLabel /@ VertexList[h3["Skeleton"]]], {None}];
check["skeleton drops labels inside the edges too",
   Union[VertexLabel /@ Catenate[EdgeVertices /@ EdgeList[h3["Skeleton"]]]], {None}];
check["skeleton drops every edge label",
   Union[EdgeLabel /@ EdgeList[HypergraphSkeleton[
      Hypergraph[{Edge[{Vertex[1, "a"], 2}, "Directed", "f"]}]]]], {None}];
check["skeleton keeps edge symmetry types",
   EdgeSymmetry /@ EdgeList[HypergraphSkeleton[h1]], EdgeSymmetry /@ EdgeList[h1]];

p[];
p["=== 5. Isomorphism: plain (skeleton) ==="];
a = Hypergraph[{{1, 2}, {2, 3}}];
b = Hypergraph[{{10, 20}, {20, 30}}];
c = Hypergraph[{{1, 2}, {1, 3}}];
show["a = path 1->2->3", a];
show["b = path 10->20->30", b];
show["c = out-star 1->2, 1->3", c];
check["a iso b  (relabelling)", IsomorphicHypergraphQ[a, b], True];
check["a iso c  (directed, so no)", IsomorphicHypergraphQ[a, c], False];
show["FindHypergraphIsomorphism[a, b]", FindHypergraphIsomorphism[a, b]];
show["FindHypergraphIsomorphism[a, c]", FindHypergraphIsomorphism[a, c]];

p[];
p["-- the same two, made undirected, ARE isomorphic --"];
au = Hypergraph[{Edge[{1, 2}, "Unordered"], Edge[{2, 3}, "Unordered"]}];
cu = Hypergraph[{Edge[{1, 2}, "Unordered"], Edge[{1, 3}, "Unordered"]}];
check["unordered path iso unordered star", IsomorphicHypergraphQ[au, cu], True];
check["directed path NOT iso unordered path", IsomorphicHypergraphQ[a, au], False];

p[];
p["=== 6. Isomorphism: labelled vs unlabelled ==="];
l1 = Hypergraph[{Vertex[1, "A"], Vertex[2, "B"], Vertex[3, "C"]}, {{1, 2}, {2, 3}}];
l2 = Hypergraph[{Vertex[10, "A"], Vertex[20, "B"], Vertex[30, "C"]}, {{10, 20}, {20, 30}}];
l3 = Hypergraph[{Vertex[1, "C"], Vertex[2, "B"], Vertex[3, "A"]}, {{1, 2}, {2, 3}}];
p["    l1 = A->B->C,  l2 = A->B->C on other ids,  l3 = C->B->A"];
check["l1 iso l2  (labelled)", IsomorphicHypergraphQ[l1, l2], True];
(* Labels count only up to renaming, so A,B,C against C,B,A is an isomorphism, by the label
   correspondence A <-> C with B fixed. *)
check["l1 iso l3  (labelled, by renaming A <-> C)", IsomorphicHypergraphQ[l1, l3], True];
check["l1 iso l3  (skeletons)", IsomorphicHypergraphQ[l1, l3, "Labeled" -> False], True];
show["FindHypergraphIsomorphism[l1, l2]", FindHypergraphIsomorphism[l1, l2]];

p[];
p["-- the case upstream got wrong: A,A,B vs A,B,B on the same skeleton --"];
m1 = Hypergraph[{Vertex[1, "A"], Vertex[2, "A"], Vertex[3, "B"]}, {{1, 2}, {2, 3}}];
m2 = Hypergraph[{Vertex[1, "A"], Vertex[2, "B"], Vertex[3, "B"]}, {{1, 2}, {2, 3}}];
check["AAB iso ABB  (labelled)", IsomorphicHypergraphQ[m1, m2], False];
check["AAB iso ABB  (skeletons)", IsomorphicHypergraphQ[m1, m2, "Labeled" -> False], True];

p[];
p["-- edge labels count too --"];
t1 = Hypergraph[{Edge[{1, 2}, "Directed", "p"], Edge[{2, 3}, "Directed", "q"]}];
t2 = Hypergraph[{Edge[{1, 2}, "Directed", "q"], Edge[{2, 3}, "Directed", "p"]}];
p["    t1 = p then q along a directed path,  t2 = q then p"];
check["t1 iso t2  (labelled, by renaming p <-> q)", IsomorphicHypergraphQ[t1, t2], True];
check["t1 iso t2  (skeletons)", IsomorphicHypergraphQ[t1, t2, "Labeled" -> False], True];

p[];
p["-- labels are significant only up to a one-to-one renaming --"];
g1 = Hypergraph[{Edge[{1, 2}, "Directed", "a"], Edge[{2, 3}, "Directed", "b"]}];
g2 = Hypergraph[{Edge[{4, 5}, "Directed", "x"], Edge[{5, 6}, "Directed", "y"]}];
g3 = Hypergraph[{Edge[{4, 5}, "Directed", "x"], Edge[{5, 6}, "Directed", "x"]}];
check["two distinct labels vs two distinct labels", IsomorphicHypergraphQ[g1, g2], True];
check["  the correspondence found",
   FindHypergraphIsomorphism[g1, g2, "Labels" -> True],
   <|"Vertices" -> <|1 -> 4, 2 -> 5, 3 -> 6|>, "VertexLabels" -> <||>,
     "EdgeLabels" -> <|"a" -> "x", "b" -> "y"|>|>];
check["two distinct labels vs one repeated label", IsomorphicHypergraphQ[g1, g3], False];
check["  their skeletons still agree", IsomorphicHypergraphQ[g1, g3, "Labeled" -> False], True];
check["no maps are reported when there is no isomorphism",
   FindHypergraphIsomorphism[g1, g3, "Labels" -> True],
   <|"Vertices" -> <||>, "VertexLabels" -> <||>, "EdgeLabels" -> <||>|>];

p[];
p["-- vertex labels and edge labels are put in correspondence independently --"];
i1 = Hypergraph[{Edge[{Vertex[1, "A"], Vertex[2, "A"]}, "Directed", "f"],
                 Edge[{Vertex[2, "A"], Vertex[3, "B"]}, "Directed", "g"]}];
i2 = Hypergraph[{Edge[{Vertex[1, "P"], Vertex[2, "P"]}, "Directed", "g"],
                 Edge[{Vertex[2, "P"], Vertex[3, "Q"]}, "Directed", "f"]}];
i3 = Hypergraph[{Edge[{Vertex[1, "P"], Vertex[2, "Q"]}, "Directed", "f"],
                 Edge[{Vertex[2, "Q"], Vertex[3, "Q"]}, "Directed", "g"]}];
check["both label kinds renamed at once", IsomorphicHypergraphQ[i1, i2], True];
check["  the two correspondences",
   Values @ KeyTake[FindHypergraphIsomorphism[i1, i2, "Labels" -> True], {"VertexLabels", "EdgeLabels"}],
   {<|"A" -> "P", "B" -> "Q"|>, <|"f" -> "g", "g" -> "f"|>}];
check["vertex-label pattern differs", IsomorphicHypergraphQ[i1, i3], False];

p[];
p["-- the vertex map can still be forced to move vertices --"];
k1 = Hypergraph[{Edge[{1, 2}, "Directed", "f"], Edge[{1, 3}, "Directed", "f"],
                 Edge[{1, 4}, "Directed", "g"]}];
k2 = Hypergraph[{Edge[{1, 2}, "Directed", "p"], Edge[{1, 3}, "Directed", "q"],
                 Edge[{1, 4}, "Directed", "q"]}];
check["f twice and g once, against q twice and p once", IsomorphicHypergraphQ[k1, k2], True];
check["  vertex 4 must go to vertex 2",
   Lookup[FindHypergraphIsomorphism[k1, k2], 4], 2];

p[];
p["-- labelled-edge isomorphism, positive cases --"];
e1 = Hypergraph[{Edge[{1, 2}, "Directed", "f"], Edge[{2, 3}, "Directed", "g"]}];
e2 = Hypergraph[{Edge[{7, 8}, "Directed", "f"], Edge[{8, 9}, "Directed", "g"]}];
check["same labelled path on other identifiers", IsomorphicHypergraphQ[e1, e2], True];
check["  its mapping", FindHypergraphIsomorphism[e1, e2], <|1 -> 7, 2 -> 8, 3 -> 9|>];

s1 = Hypergraph[{Edge[{1, 2}, "Directed", "f"], Edge[{1, 3}, "Directed", "g"]}];
s2 = Hypergraph[{Edge[{1, 2}, "Directed", "g"], Edge[{1, 3}, "Directed", "f"]}];
check["labelled star, f and g exchanged", IsomorphicHypergraphQ[s1, s2], True];
(* Exchanging f and g on the two arms is now absorbed by the label correspondence, so the
   vertices need not move at all. *)
check["  the vertices can stay put", FindHypergraphIsomorphism[s1, s2], <|1 -> 1, 2 -> 2, 3 -> 3|>];
check["  the labels do the work",
   Lookup[FindHypergraphIsomorphism[s1, s2, "Labels" -> True], "EdgeLabels"],
   <|"f" -> "g", "g" -> "f"|>];

p[];
p["-- labelled-edge isomorphism, negative cases --"];
check["labelled edge is not an unlabelled one",
   IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}, "Directed", "f"]}], Hypergraph[{Edge[{1, 2}]}]], False];
check["  but their skeletons agree",
   IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}, "Directed", "f"]}], Hypergraph[{Edge[{1, 2}]}],
      "Labeled" -> False], True];
check["same label, different symmetry type",
   IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2, 3}, "Cyclic", "f"]}],
                         Hypergraph[{Edge[{1, 2, 3}, "Directed", "f"]}]], False];
check["edge-label multiplicity: {f,f} vs {f,g}",
   IsomorphicHypergraphQ[Hypergraph[{Edge[{1, 2}, "Directed", "f"], Edge[{1, 2}, "Directed", "f"]}],
                         Hypergraph[{Edge[{1, 2}, "Directed", "f"], Edge[{1, 2}, "Directed", "g"]}]], False];

p[];
p["-- vertex labels and edge labels together --"];
b1 = Hypergraph[{Edge[{Vertex[1, "A"], Vertex[2, "B"]}, "Directed", "f"],
                 Edge[{Vertex[2, "B"], Vertex[3, "C"]}, "Cyclic", "g"]}];
b2 = Hypergraph[{Edge[{Vertex[5, "A"], Vertex[6, "B"]}, "Directed", "f"],
                 Edge[{Vertex[6, "B"], Vertex[7, "C"]}, "Cyclic", "g"]}];
b3 = Hypergraph[{Edge[{Vertex[5, "A"], Vertex[6, "B"]}, "Directed", "g"],
                 Edge[{Vertex[6, "B"], Vertex[7, "C"]}, "Cyclic", "f"]}];
check["everything matches", IsomorphicHypergraphQ[b1, b2], True];
(* b3 exchanges the two edge labels, which a one-to-one renaming absorbs. *)
check["edge labels exchanged is still an isomorphism", IsomorphicHypergraphQ[b1, b3], True];
(* What a renaming cannot absorb is a change in how many elements share a label. *)
b4 = Hypergraph[{Edge[{Vertex[5, "A"], Vertex[6, "B"]}, "Directed", "f"],
                 Edge[{Vertex[6, "B"], Vertex[7, "C"]}, "Cyclic", "f"]}];
check["one label on both edges instead of two", IsomorphicHypergraphQ[b1, b4], False];
check["  their skeletons still agree", IsomorphicHypergraphQ[b1, b4, "Labeled" -> False], True];

p[];
p["=== 7. Canonical form ==="];
show["CanonicalHypergraph[a]", CanonicalHypergraph[a]];
show["CanonicalHypergraph[b]", CanonicalHypergraph[b]];
check["canonical forms agree", CanonicalHypergraph[a] === CanonicalHypergraph[b], True];
show["CanonicalHypergraph[l1]", CanonicalHypergraph[l1]];
show["CanonicalHypergraph[l1, unlabelled]", CanonicalHypergraph[l1, "Labeled" -> False]];

p[];
p["=== 8. Higher arity and mixed symmetries ==="];
x1 = Hypergraph[{Edge[{1, 2, 3}, "Unordered"], Edge[{3, 4}, "Directed"], Edge[{1, 4, 5}, "Cyclic"]}];
x2 = Hypergraph[{Edge[{30, 20, 10}, "Unordered"], Edge[{30, 40}, "Directed"], Edge[{40, 50, 10}, "Cyclic"]}];
show["x1", x1];
show["x2 (relabelled, edges permuted)", x2];
check["x1 iso x2", IsomorphicHypergraphQ[x1, x2], True];
show["mapping", FindHypergraphIsomorphism[x1, x2]];
x3 = Hypergraph[{Edge[{1, 2, 3}, "Unordered"], Edge[{3, 4}, "Directed"], Edge[{1, 5, 4}, "Cyclic"]}];
p["    x3 differs from x1 only in the cyclic edge's orientation: {1,4,5} vs {1,5,4}"];
check["x1 iso x3", IsomorphicHypergraphQ[x1, x3], False];

p[];

p["=== 9. Vertex identifiers can be any expression ==="];
w1 = Hypergraph[{Edge[{"a", "b"}], Edge[{"b", {1, 2}}]}];
show["string and list identifiers", w1];
w2 = Hypergraph[{Edge[{x, y}], Edge[{y, z}]}];
show["symbol identifiers", w2];
check["string/list hg iso symbol hg", IsomorphicHypergraphQ[w1, w2], True];
show["mapping", FindHypergraphIsomorphism[w1, w2]];

p[];
p["=== 10. Randomized checks ==="];
SeedRandom[20260917];
randomHG[] := Module[{n = RandomInteger[{2, 5}], k = RandomInteger[{1, 4}]},
    Hypergraph @ Table[
        Edge[RandomInteger[{1, n}, RandomInteger[{1, 3}]],
             RandomChoice[{"Directed", "Unordered", "Cyclic"}],
             RandomChoice[{None, "f", "g"}]],
        k]
];
relabelRandomly[hg_] := Module[{ids = hg["VertexIDs"], new},
    new = AssociationThread[ids -> RandomSample[Range[100, 99 + Length[ids]]]];
    Hypergraph[
        Vertex[Lookup[new, Key[VertexID[#]]], VertexLabel[#]] & /@ VertexList[hg],
        Edge[Lookup[new, Key[VertexID[#]]] & /@ EdgeVertices[#],
             EdgeSymmetry[#], EdgeLabel[#]] & /@ EdgeList[hg]]
];

p["-- (a) a relabelling is always isomorphic to its original, and has the same canonical form --"];
r1 = Flatten @ Table[
    Module[{g = randomHG[], r},
        r = relabelRandomly[g];
        Table[
            {IsomorphicHypergraphQ[g, r, "Labeled" -> lab, Method -> m],
             CanonicalHypergraph[g, "Labeled" -> lab, Method -> m] ===
                 CanonicalHypergraph[r, "Labeled" -> lab, Method -> m]},
            {lab, {True, False}}, {m, {Automatic, "BruteForce"}}]
    ],
    300];
check["300 relabellings x 2 label modes x 2 methods", Union[r1], {True}];

p["-- (a2) the same, on hypergraphs whose edges all carry labels --"];
randomEdgeLabelled[] := Module[{n = RandomInteger[{2, 5}]},
    Hypergraph @ Table[
        Edge[RandomInteger[{1, n}, RandomInteger[{2, 3}]],
             RandomChoice[{"Directed", "Unordered", "Cyclic"}],
             RandomChoice[{"f", "g", "h"}]],
        RandomInteger[{1, 4}]]
];
r1b = Flatten @ Table[
    Module[{g = randomEdgeLabelled[], r},
        r = relabelRandomly[g];
        Table[{IsomorphicHypergraphQ[g, r, Method -> m],
               CanonicalHypergraph[g, Method -> m] === CanonicalHypergraph[r, Method -> m]},
              {m, {Automatic, "BruteForce"}}]
    ],
    200];
check["200 edge-labelled relabellings x 2 methods", Union[r1b], {True}];

p["-- (b) the two methods always agree on isomorphism --"];
r2 = Flatten @ Table[
    Module[{g = randomHG[], h = randomHG[]},
        Table[
            IsomorphicHypergraphQ[g, h, "Labeled" -> lab] ===
                IsomorphicHypergraphQ[g, h, "Labeled" -> lab, Method -> "BruteForce"],
            {lab, {True, False}}]
    ],
    300];
check["300 random pairs x 2 label modes", Union[r2], {True}];

p["-- (c) labelled isomorphism implies skeleton isomorphism, never the converse --"];
r3 = Flatten @ Table[
    Module[{g = randomHG[], h = randomHG[]},
        ! IsomorphicHypergraphQ[g, h] || IsomorphicHypergraphQ[g, h, "Labeled" -> False]
    ],
    300];
check["300 random pairs", Union[r3], {True}];

p["-- (c2) renaming labels one-to-one never changes the isomorphism class --"];
randomlyLabelled[] := Module[{g = randomHG[]},
    Hypergraph[
        Vertex[VertexID[#], RandomChoice[{None, "A", "B"}]] & /@ VertexList[g],
        EdgeList[g]]
];
renameLabels[hg_, vr_, er_] := Hypergraph[
    Vertex[VertexID[#], Replace[VertexLabel[#], vr]] & /@ VertexList[hg],
    Edge[EdgeVertices[#] /. v_Vertex :> Vertex[VertexID[v], Replace[VertexLabel[v], vr]],
         EdgeSymmetry[#], Replace[EdgeLabel[#], er]] & /@ EdgeList[hg]];
r5 = Flatten @ Table[
    Module[{g = randomlyLabelled[], h},
        h = renameLabels[g, {"A" -> "A1", "B" -> "B1"}, {"f" -> "f1", "g" -> "g1"}];
        {IsomorphicHypergraphQ[g, h],
         IsomorphicHypergraphQ[g, h, Method -> "BruteForce"],
         CanonicalHypergraph[g] === CanonicalHypergraph[h]}],
    200];
check["200 injective label renamings, both methods", Union[r5], {True}];

p["-- (c3) collapsing two distinct labels into one does change it --"];
r6 = Table[
    Module[{g = randomlyLabelled[]},
        If[ Length[DeleteCases[Union[VertexLabel /@ VertexList[g]], None]] < 2,
            True,
            ! IsomorphicHypergraphQ[g, renameLabels[g, {"A" -> "Z", "B" -> "Z"}, {}]]]],
    300];
check["300 label collapses are detected", Union[r6], {True}];

p["-- (d) FindHypergraphIsomorphism returns a genuine bijection whenever one is reported --"];
r4 = Flatten @ Table[
    Module[{g = randomHG[], h, iso},
        h = relabelRandomly[g];
        iso = FindHypergraphIsomorphism[g, h];
        {Sort[Keys[iso]] === Sort[g["VertexIDs"]],
         Sort[Values[iso]] === Sort[h["VertexIDs"]],
         Hypergraph[
             Vertex[Lookup[iso, Key[VertexID[#]]], VertexLabel[#]] & /@ VertexList[g],
             Edge[Lookup[iso, Key[VertexID[#]]] & /@ EdgeVertices[#],
                  EdgeSymmetry[#], EdgeLabel[#]] & /@ EdgeList[g]] === h}
    ],
    300];
check["300 mappings are bijections that carry g exactly onto h", Union[r4], {True}];


p[];
p["=== Simplicity ==="];
check["no repeated edge", SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}], True];
check["a repeated edge", SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}], Edge[{1, 2}]}], False];
check["labels are set aside: two edges differing only by a label are one edge of the skeleton",
   SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, "Directed", "a"], Edge[{1, 2}, "Directed", "b"]}],
   False];
check["a vertex repeated within one edge is allowed",
   SimpleHypergraphQ @ Hypergraph[{Edge[{1, 1, 2}, "Unordered"]}], True];
check["the same vertices under different symmetry types are different edges",
   SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, "Directed"], Edge[{1, 2}, "Unordered"]}], True];
check["two spellings of one unordered edge are one edge, so not simple",
   SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, "Unordered"], Edge[{2, 1}, "Unordered"]}], False];
check["  the directed pair, by contrast, really is two edges",
   SimpleHypergraphQ @ Hypergraph[{Edge[{1, 2}, "Directed"], Edge[{2, 1}, "Directed"]}], True];
check["an empty hypergraph is simple", SimpleHypergraphQ @ Hypergraph[{}], True];
check["anything else is not a hypergraph", SimpleHypergraphQ[{1, 2}], False];
p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];


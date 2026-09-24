#!/usr/bin/env wolframscript

(* Assertions for the array and hypermatrix objects.
   Run with:  wolframscript -file tests/HypermatrixExamples.wls  *)

$ProgressReporting = False;

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


p["=== 1. The value domain is read off the entries ==="];
check["booleans", ArrayDomain[ArrayObject[{True, False}]], "Boolean"];
check["integers", ArrayDomain[ArrayObject[{1, 2, 3}]], "Integer"];
check["rationals", ArrayDomain[ArrayObject[{1/2, 3}]], "Rational"];
check["reals", ArrayDomain[ArrayObject[{1.5, 2}]], "Real"];
check["complexes", ArrayDomain[ArrayObject[{1 + I, 2}]], "Complex"];
check["a finite field", ArrayDomain[ArrayObject[{FiniteField[2, 1][0], FiniteField[2, 1][1]}]],
   FiniteField[2, 1]];
check["the narrowest one that holds, not the first that could", ArrayDomain[ArrayObject[{1, 2}]],
   "Integer"];
check["entries answering to none of them are expressions",
   ArrayDomain[ArrayObject[{"a", "b"}]], "Expression"];
check["  symbolic entries likewise", ArrayDomain[ArrayObject[{x, y}]], "Expression"];
check["  and mixed entries too", ArrayDomain[ArrayObject[{1, "a"}]], "Expression"];
check["a domain cannot be declared, so there is nothing to reject: any entries are an array",
   {ArrayObjectQ @ ArrayObject[{1.5}], ArrayObjectQ @ ArrayObject[{"anything", f[1]}]},
   {True, True}];
check["an array of one kind of field element", ArrayDomain[ArrayObject[{FiniteField[3, 1][1]}]],
   FiniteField[3, 1]];
check["two different fields are not one domain",
   ArrayDomain[ArrayObject[{FiniteField[2, 1][1], FiniteField[3, 1][1]}]], "Expression"];


p[];
p["=== 2. Order, dimensions, regularity ==="];
v = ArrayObject[{1, 2, 3}];
m = ArrayObject[{{1, 2, 3}, {4, 5, 6}}];
t = ArrayObject[Array[#1 + #2 + #3 &, {2, 2, 2}]];
check["a 1-array is a vector", {ArrayOrder[v], ArrayDimensions[v]}, {1, {3}}];
check["a 2-array is a matrix", {ArrayOrder[m], ArrayDimensions[m]}, {2, {2, 3}}];
check["a 3-array", {ArrayOrder[t], ArrayDimensions[t]}, {3, {2, 2, 2}}];
check["unequal index lengths make it irregular", RegularArrayQ[m], False];
check["equal index lengths make it regular", RegularArrayQ[t], True];
check["Dimensions works directly", Dimensions[m], {2, 3}];
check["Normal gives the entries", Normal[v], {1, 2, 3}];
check["ArrayEntries is the same", ArrayEntries[v], Normal[v]];
check["a scalar is the array of order zero", ArrayOrder @ ArrayObject[5], 0];
check["  which has no dimensions", ArrayDimensions @ ArrayObject[5], {}];
check["  is vacuously regular", RegularArrayQ @ ArrayObject[5], True];
check["  and carries its single entry", Normal @ ArrayObject[5], 5];
check["a symbolic 0-array is its bare name", Normal @ GenerateSymbolicArray[q, {}], q];
check["a ragged list is not an array", Quiet @ ArrayObject[{{1, 2}, {3}}], $Failed];


p[];
p["=== 3. A declared symmetry is verified against the entries ==="];
sym2 = {{1, 2}, {2, 1}};
asym2 = {{1, 2}, {3, 4}};
check["symmetric entries accepted as Symmetric",
   ArraySymmetry[ArrayObject[sym2, "Symmetric"]], "Symmetric"];
check["asymmetric entries rejected as Symmetric",
   Quiet @ ArrayObject[asym2, "Symmetric"], $Failed];
check["the same entries are fine as Rigid", ArraySymmetry[ArrayObject[asym2]], "Rigid"];
check["Rigid is the default", ArraySymmetry[ArrayObject[sym2]], "Rigid"];
check["an irregular array cannot carry a symmetry",
   Quiet @ ArrayObject[{{1, 2, 3}, {4, 5, 6}}, "Symmetric"], $Failed];
check["an unrecognized symmetry is rejected",
   Quiet @ ArrayObject[sym2, "Wobbly"], $Failed];
check["spelling is lenient on input",
   ArraySymmetry[ArrayObject[sym2, "symmetric"]], "Symmetric"];

(* the indicator of the cyclic orbit of (1,2,3): invariant under rotation, not under reflection *)
cyc3 = Array[If[MemberQ[{{1, 2, 3}, {2, 3, 1}, {3, 1, 2}}, {##}], 1, 0] &, {3, 3, 3}];
check["cyclic but not symmetric entries accepted as Cyclic",
   ArraySymmetry[ArrayObject[cyc3, "Cyclic"]], "Cyclic"];
check["  and rejected as Symmetric", Quiet @ ArrayObject[cyc3, "Symmetric"], $Failed];
check["symmetric entries are also cyclic",
   ArraySymmetry[ArrayObject[sym2, "Cyclic"]], "Cyclic"];


p[];
p["=== 4. GenerateSymbolicArray builds entries from a name ==="];
check["entries are the name applied to the indices",
   Normal @ GenerateSymbolicArray[a, {2, 2}],
   {{a[1, 1], a[1, 2]}, {a[2, 1], a[2, 2]}}];
check["order, dimensions and domain",
   {ArrayOrder[#], ArrayDimensions[#], ArrayDomain[#]} & @ GenerateSymbolicArray[a, {2, 3}],
   {2, {2, 3}, "Expression"}];
check["it gives back an ordinary array, not an object of its own",
   Head @ GenerateSymbolicArray[a, {2, 3}], ArrayObject];
check["  so the generated entries are just entries",
   ArrayObjectQ @ GenerateSymbolicArray[a, {2, 3}], True];
check["a symmetric symbolic array really is symmetric",
   With[{e = Normal @ GenerateSymbolicArray[a, {3, 3}, "Symmetric"]}, e === Transpose[e]], True];
check["equivalent index tuples name one entry",
   Normal @ GenerateSymbolicArray[a, {2, 2}, "Symmetric"],
   {{a[1, 1], a[1, 2]}, {a[1, 2], a[2, 2]}}];
check["distinct entries: rigid, cyclic, symmetric at order 3",
   Length @ DeleteDuplicates @ Flatten @ Normal @ GenerateSymbolicArray[a, {3, 3, 3}, #] & /@
      {"Rigid", "Cyclic", "Symmetric"},
   {27, 11, 10}];
check["cyclic and symmetric differ at order 3",
   Normal[GenerateSymbolicArray[a, {3, 3, 3}, "Cyclic"]] =!= Normal[GenerateSymbolicArray[a, {3, 3, 3}, "Symmetric"]],
   True];
check["but coincide at order 2, where the cyclic group is the symmetric one",
   Normal[GenerateSymbolicArray[a, {3, 3}, "Cyclic"]] === Normal[GenerateSymbolicArray[a, {3, 3}, "Symmetric"]],
   True];
check["a string name is allowed",
   Normal @ GenerateSymbolicArray["A", {2}], {"A"[1], "A"[2]}];
check["a name must be a symbol or a string", Quiet @ GenerateSymbolicArray[{1, 2}, {2}], $Failed];
check["dimensions must be positive integers", Quiet @ GenerateSymbolicArray[a, {2, 0}], $Failed];
check["an irregular symbolic array cannot carry a symmetry",
   Quiet @ GenerateSymbolicArray[a, {2, 3}, "Symmetric"], $Failed];


p[];
p["=== 5. Predicates and properties ==="];
check["ArrayObjectQ",
   {ArrayObjectQ[v], ArrayObjectQ[GenerateSymbolicArray[a, {2}]], ArrayObjectQ[{1, 2}], ArrayObjectQ[5]},
   {True, True, False, False}];
check["properties", v["Properties"],
   {"Order", "Dimensions", "Symmetry", "Domain", "Entries", "RegularQ"}];
check["property access", {v["Order"], v["Dimensions"], v["Domain"], v["RegularQ"]},
   {1, {3}, "Integer", True}];
check["an unknown property", Head @ v["Nonsense"], Missing];


p[];
p["=== 6. A hypermatrix holds its arrays in canonical order ==="];
arrays = {
   ArrayObject[Array[1 &, {2, 2, 2}], "Symmetric"],
   ArrayObject[{{1, 2}, {3, 4}}],
   ArrayObject[{1, 2}],
   ArrayObject[{1, 2, 3}],
   ArrayObject[{{1, 2}, {2, 1}}, "Symmetric"],
   ArrayObject[{{1, 2}, {2, 1}}, "Cyclic"]
};
hm = Hypermatrix[arrays];
show["orders", hm["Orders"]];
show["dimensions", hm["Dimensions"]];
show["symmetries", hm["Symmetries"]];
check["lowest order first", hm["Orders"], {1, 1, 2, 2, 2, 3}];
check["shorter dimensions first, among equal order", hm["Dimensions"][[1 ;; 2]], {{2}, {3}}];
check["symmetric, then cyclic, then rigid", hm["Symmetries"][[3 ;; 5]],
   {"Symmetric", "Cyclic", "Rigid"}];
check["HypermatrixQ", HypermatrixQ[hm], True];
check["Length gives the number of arrays", Length[hm], 6];
check["properties", hm["Properties"],
   {"Arrays", "Length", "Orders", "Dimensions", "Symmetries", "Domains", "Entries"}];
check["the sequence form is equivalent",
   Hypermatrix[ArrayObject[{1, 2}], ArrayObject[{1, 2, 3}]] ===
      Hypermatrix[{ArrayObject[{1, 2}], ArrayObject[{1, 2, 3}]}], True];
check["raw entries are promoted to numerical arrays",
   HypermatrixArrays[Hypermatrix[{{1, 2}, {{1, 2}, {3, 4}}}]][[1]],
   ArrayObject[{1, 2}, "Rigid"]];
check["numerical and symbolic arrays may be mixed",
   Hypermatrix[{GenerateSymbolicArray[a, {2, 2}], ArrayObject[{1, 2}]}]["Orders"], {1, 2}];
check["a bare value is the 0-array of it, so this is a hypermatrix after all",
   {HypermatrixQ @ Hypermatrix[{"nope"}], Normal @ Hypermatrix[{"nope"}]},
   {True, {"nope"}}];
check["  a ragged list, on the other hand, is no array at all",
   Quiet @ Hypermatrix[{{{1, 2}, {3}}}], $Failed];
check["Normal gives the entries of each array in order",
   Normal @ Hypermatrix[{ArrayObject[{{1, 2}, {3, 4}}], ArrayObject[{1, 2}]}],
   {{1, 2}, {{1, 2}, {3, 4}}}];


p[];
p["=== 7. Randomized checks ==="];
SeedRandom[20260919];

randomData[] := Module[{n = RandomInteger[{1, 3}], d},
   d = If[RandomChoice[{True, False}],
      ConstantArray[RandomInteger[{1, 3}], n],            (* regular *)
      RandomInteger[{1, 3}, n]];                          (* possibly irregular *)
   Array[RandomInteger[{0, 5}] &, d]
];

p["-- (a) order and dimensions always agree with the entries --"];
r1 = Flatten @ Table[
   Module[{data = randomData[], arr},
      arr = ArrayObject[data];
      {ArrayOrder[arr] === Length[Dimensions[data]],
       ArrayDimensions[arr] === Dimensions[data],
       RegularArrayQ[arr] === (SameQ @@ Dimensions[data]),
       Normal[arr] === data}],
   300];
check["300 random arrays", Union[r1], {True}];

p["-- (b) a symmetrized array is always accepted as Symmetric --"];
r2 = Flatten @ Table[
   Module[{n = RandomInteger[{2, 3}], d = RandomInteger[{2, 3}], data, sym},
      data = Array[RandomInteger[{0, 4}] &, ConstantArray[d, n]];
      sym = Total[Transpose[data, #] & /@ Permutations[Range[n]]];
      {ArraySymmetry[ArrayObject[sym, "Symmetric"]] === "Symmetric",
       ArraySymmetry[ArrayObject[sym, "Cyclic"]] === "Cyclic"}],
   200];
check["200 symmetrized arrays", Union[r2], {True}];

p["-- (c) a cyclically symmetrized array is accepted as Cyclic --"];
r3 = Table[
   Module[{n = RandomInteger[{2, 3}], d = RandomInteger[{2, 3}], data, cyc},
      data = Array[RandomInteger[{0, 4}] &, ConstantArray[d, n]];
      cyc = Total[Transpose[data, RotateLeft[Range[n], #]] & /@ Range[0, n - 1]];
      ArraySymmetry[ArrayObject[cyc, "Cyclic"]] === "Cyclic"],
   200];
check["200 cyclically symmetrized arrays", Union[r3], {True}];

p["-- (d) the canonical order does not depend on the order given --"];
r4 = Table[
   Module[{as = Table[ArrayObject[randomData[]], RandomInteger[{2, 6}]]},
      Hypermatrix[as] === Hypermatrix[RandomSample[as]]],
   300];
check["300 shuffled sets of arrays", Union[r4], {True}];

p["-- (e) the canonical order really is sorted by the stated criteria --"];
rank[s_] := First @ FirstPosition[{"Symmetric", "Cyclic", "Rigid"}, s];
r5 = Table[
   Module[{hm2 = Hypermatrix[Table[ArrayObject[randomData[]], RandomInteger[{2, 6}]]], keys},
      keys = {ArrayOrder[#], ArrayDimensions[#], rank[ArraySymmetry[#]]} & /@ HypermatrixArrays[hm2];
      OrderedQ[keys]],
   300];
check["300 hypermatrices are ordered", Union[r5], {True}];


p[];
p["=== 8. Drawing arrays: the flat family ==="];
check["order 0 is a single cell", Head @ ArrayGraphics[ArrayObject[7]], Graphics];
check["order 1 is a row of cells", Head @ ArrayGraphics[{1, 2, 3}], Graphics];
check["order 2 is a grid", Head @ ArrayGraphics[{{1, 2}, {3, 4}}], Graphics];
check["order 3 is a row of grids, still flat", Head @ ArrayGraphics[Array[1 &, {2, 2, 2}]], Graphics];
check["order 4 is a grid of grids", Head @ ArrayGraphics[Array[1 &, {2, 2, 2, 2}]], Graphics];
check["order 5 too", Head @ ArrayGraphics[Array[1 &, {2, 2, 2, 2, 2}]], Graphics];
check["nothing it draws is three-dimensional",
   AllTrue[{0, 1, 2, 3, 4, 5},
      Head @ ArrayGraphics[If[# === 0, 7, Array[1 &, ConstantArray[2, #]]]] === Graphics &],
   True];
check["array objects draw too", Head @ ArrayGraphics[IdentityArray[2, 3]], Graphics];
check["so do symbolic arrays", Head @ ArrayGraphics[GenerateSymbolicArray[a, {2, 2}]], Graphics];

p["-- the splitting of the indices --"];
check["the flat default, in blocks of at most two", Table[ArrayNesting[n], {n, 0, 7}],
   {{}, {1}, {2}, {1, 2}, {2, 2}, {1, 2, 2}, {2, 2, 2}, {1, 2, 2, 2}}];
check["the remainder goes first, so a 3-array is a row of matrices", ArrayNesting[3], {1, 2}];
check["every flat block is one or two",
   AllTrue[Flatten @ Table[ArrayNesting[n], {n, 0, 12}], # <= 2 &], True];
check["and the blocks account for every index",
   AllTrue[Range[0, 12], Total[ArrayNesting[#]] === # &], True];
check["a block of three has no flat picture, and is refused",
   Quiet @ ArrayGraphics[Array[1 &, {2, 2, 2}], "Nesting" -> {3}], $Failed];
check["a nesting that does not account for every index is refused",
   Quiet @ ArrayGraphics[{{1, 2}, {3, 4}}, "Nesting" -> {1}], $Failed];
check["an explicit splitting is honoured",
   Head @ ArrayGraphics[Array[1 &, {2, 2, 2}], "Nesting" -> {2, 1}], Graphics];

p["-- options --"];
check["a theme", Head @ ArrayGraphics[{{1, 2}, {3, 4}}, "PlotTheme" -> "Dark"], Graphics];
check["an unknown theme is refused", Quiet @ ArrayGraphics[{1, 2}, "PlotTheme" -> "Neon"], $Failed];
check["unframed", Head @ ArrayGraphics[{{1, 2}, {3, 4}}, "Framed" -> False], Graphics];
check["indices may be dropped", Head @ ArrayGraphics[{{1, 2}, {3, 4}}, "Indices" -> None], Graphics];
check["or coloured by axis", Head @ ArrayGraphics[{{1, 2}, {3, 4}}, "Indices" -> "Colored"], Graphics];
check["an unknown index spec is refused",
   Quiet @ ArrayGraphics[{1, 2}, "Indices" -> "Rainbow"], $Failed];
check["an array too big to read is refused, not drawn",
   Quiet @ ArrayGraphics[Array[1 &, {12, 12, 12}]], $Failed];
check["  unless the limit is raised",
   Head @ ArrayGraphics[Array[1 &, {4, 4, 4}], "MaxEntries" -> 1000], Graphics];
check["a ragged list is not an array", Quiet @ ArrayGraphics[{{1, 2}, {3}}], $Failed];


p[];
p["=== 9. Drawing arrays: the spatial family ==="];
check["order 0 is a single cube", Head @ ArrayGraphics3D[ArrayObject[7]], Graphics3D];
check["order 1 is a line of cubes", Head @ ArrayGraphics3D[{1, 2, 3}], Graphics3D];
check["order 2 is a plane of cubes", Head @ ArrayGraphics3D[{{1, 2}, {3, 4}}], Graphics3D];
check["order 3 is a lattice", Head @ ArrayGraphics3D[Array[1 &, {2, 2, 2}]], Graphics3D];
check["order 4 tiles lattices through space",
   Head @ ArrayGraphics3D[Array[1 &, {2, 2, 2, 2}]], Graphics3D];
check["order 5 too", Head @ ArrayGraphics3D[Array[1 &, {2, 2, 2, 2, 2}]], Graphics3D];
check["everything it draws is three-dimensional",
   AllTrue[{0, 1, 2, 3, 4, 5},
      Head @ ArrayGraphics3D[If[# === 0, 7, Array[1 &, ConstantArray[2, #]]]] === Graphics3D &],
   True];

p["-- the splitting, and the tiling it produces --"];
check["the spatial default, in blocks of at most three", Table[ArrayNesting[n, 3], {n, 0, 7}],
   {{}, {1}, {2}, {3}, {1, 3}, {2, 3}, {3, 3}, {1, 3, 3}}];
check["a block of four is refused; space holds three",
   Quiet @ ArrayGraphics3D[Array[1 &, {2, 2, 2, 2}], "Nesting" -> {4}], $Failed];
extent = WolframInstitute`Hypergraphs`Private`extent3D;
check["a leaf block spans its own dimensions", extent[{2, 3}, {2}], {2, 3, 1}];
check["a 0-array spans one cell", extent[{}, {}], {1, 1, 1}];
check["tiling leaves a gap between the blocks",
   extent[{2, 2, 2, 2}, {1, 3}], {2 (2 + 9/10) - 9/10, 2, 2} // N];
check["and tiles in two directions when the outer block has two indices",
   extent[{2, 2, 2, 2, 2}, {2, 3}][[1 ;; 2]], {2 (2 + 9/10) - 9/10, 2 (2 + 9/10) - 9/10} // N];
check["an explicit splitting is honoured",
   Head @ ArrayGraphics3D[Array[1 &, {2, 2, 2, 2}], "Nesting" -> {3, 1}], Graphics3D];
check["options are shared with the flat family",
   Head @ ArrayGraphics3D[Array[1 &, {2, 2, 2}], "PlotTheme" -> "Dark", "Indices" -> "Colored"],
   Graphics3D];
check["an array too big to read is refused here too",
   Quiet @ ArrayGraphics3D[Array[1 &, {12, 12, 12}]], $Failed];

p["-- a hypermatrix draws each of its arrays, in either family --"];
check["one flat picture per array",
   Length @ Cases[HypermatrixGraphics[Hypermatrix[{{1, 2}, {{1, 2}, {3, 4}}}]], _Graphics, Infinity],
   2];
check["one spatial picture per array",
   Length @ Cases[HypermatrixGraphics3D[Hypermatrix[{{1, 2}, {{1, 2}, {3, 4}}}]], _Graphics3D, Infinity],
   2];
check["a non-hypermatrix is refused", Quiet @ HypermatrixGraphics[{1, 2}], $Failed];
check["  in either family", Quiet @ HypermatrixGraphics3D[{1, 2}], $Failed];

p[];
p["=== 10. The adjacency hypermatrix ==="];
hAdj = Hypergraph[{Edge[{1, 2, 3}, "Unordered"], Edge[{3, 4}, "Directed"], Edge[{1, 2}, "Directed"]}];
hmAdj = AdjacencyHypermatrix[hAdj];
show["orders", hmAdj["Orders"]];
show["symmetries", hmAdj["Symmetries"]];
check["one array per arity and symmetry", Length[hmAdj], 2];
check["an edge of arity k gives an array of order k", hmAdj["Orders"], {2, 3}];
check["the indices run over the vertices", hmAdj["Dimensions"], {{4, 4}, {4, 4, 4}}];
check["directed edges give a rigid array, unordered a symmetric one",
   hmAdj["Symmetries"], {"Rigid", "Symmetric"}];
check["the directed entries sit where the edges do",
   Normal @ First @ HypermatrixArrays[hmAdj],
   {{0, 1, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 0, 0}}];

p["-- the symmetry types correspond one for one --"];
check["cyclic gives cyclic, over the rotations only",
   Position[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 2, 3}, "Cyclic"]}]], 1],
   {{1, 2, 3}, {2, 3, 1}, {3, 1, 2}}];
check["unordered gives symmetric, over every permutation",
   Length @ Position[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 2, 3}, "Unordered"]}]], 1],
   6];
check["directed gives rigid, over the one tuple",
   Length @ Position[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 2, 3}, "Directed"]}]], 1],
   1];
check["the same arity in three symmetries gives three arrays",
   AdjacencyHypermatrix[Hypergraph[{Edge[{1, 2}, "Directed"], Edge[{1, 2}, "Unordered"],
                                    Edge[{1, 2}, "Cyclic"]}]]["Symmetries"],
   {"Symmetric", "Cyclic", "Rigid"}];

p["-- multiplicity, repeated vertices, and the degenerate arities --"];
check["a repeated edge counts twice, the hypergraph being a multiset",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 2}], Edge[{1, 2}], Edge[{2, 1}]}]],
   {{0, 2}, {1, 0}}];
check["an edge with a repeated vertex still gives a symmetric array",
   ArraySymmetry @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 1, 2}, "Unordered"]}]],
   "Symmetric"];
check["  over the three distinct tuples of its orbit",
   Length @ Position[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 1, 2}, "Unordered"]}]], _?Positive],
   3];
check["an edge of arity zero gives a 0-array holding its count",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{}], Edge[{}], Edge[{1}]}]],
   2];
check["an edge of arity one gives a vector",
   Normal @ HypermatrixArrays[AdjacencyHypermatrix[
      Hypergraph[{Edge[{}], Edge[{1}], Edge[{2}], Edge[{1, 2}]}]]][[2]],
   {1, 1}];
check["a hypergraph with no edges gives an empty hypermatrix",
   Length @ AdjacencyHypermatrix[Hypergraph[{}]], 0];
check["anything that is not a hypergraph is refused",
   Quiet @ AdjacencyHypermatrix[{1, 2}], $Failed];

p["-- labels play no part: this is the adjacency of the skeleton --"];
check["labelled and unlabelled agree",
   AdjacencyHypermatrix[Hypergraph[{Edge[{1, 2}, "Directed", "x"], Edge[{2, 3}, "Directed", "y"]}]] ===
   AdjacencyHypermatrix[Hypergraph[{Edge[{1, 2}, "Directed"], Edge[{2, 3}, "Directed"]}]],
   True];
check["and so does the skeleton itself",
   With[{hl = Hypergraph[{Edge[{1, 2}, "Directed", "x"], Edge[{2, 3}, "Directed", "y"]}]},
      AdjacencyHypermatrix[hl] === AdjacencyHypermatrix[HypergraphSkeleton[hl]]],
   True];

p[];
p["-- (h) the hypermatrix determines the edge multiset: a round trip --"];
(* Read the hypermatrix back out: every canonical index with a positive entry names that many
   edges, and the array's symmetry names their symmetry type. If this recovers exactly the edges
   that went in, nothing was lost and nothing was invented. *)
toEdgeSym = <|"Rigid" -> "Directed", "Cyclic" -> "Cyclic", "Symmetric" -> "Unordered"|>;
toArrSym = <|"Directed" -> "Rigid", "Cyclic" -> "Cyclic", "Unordered" -> "Symmetric"|>;
canonIdx = WolframInstitute`Hypergraphs`Private`arrayCanonicalIndex;

edgesOfHypergraph[hg_] := With[
   {idx = AssociationThread[VertexID /@ VertexList[hg] -> Range[VertexCount[hg]]]},
   Sort[Function[e,
      {canonIdx[Lookup[idx, VertexID /@ EdgeVertices[e]], toArrSym[EdgeSymmetry[e]]],
       EdgeSymmetry[e]}] /@ EdgeList[hg]]
];

edgesOfHypermatrix[hm_] := Sort @ Catenate @ Map[
   Function[arr,
      With[{sym = ArraySymmetry[arr], k = ArrayOrder[arr], e = Normal[arr]},
         If[ k === 0,
            ConstantArray[{{}, toEdgeSym[sym]}, e],
            Catenate @ Map[
               Function[pos, ConstantArray[{pos, toEdgeSym[sym]}, Extract[e, pos]]],
               Select[Tuples[Range /@ ArrayDimensions[arr]], # === canonIdx[#, sym] &]]
         ]]],
   HypermatrixArrays[hm]
];

roundTripsQ[hg_] := edgesOfHypergraph[hg] === edgesOfHypermatrix[AdjacencyHypermatrix[hg]];

SeedRandom[20260924];
r6 = Table[
   roundTripsQ @ Hypergraph[Table[
      Edge[RandomInteger[{1, RandomInteger[{1, 4}]}, RandomInteger[{0, 3}]],
           RandomChoice[{"Directed", "Unordered", "Cyclic"}]],
      RandomInteger[{0, 5}]]],
   300];
check["300 random hypergraphs come back unchanged", Union[r6], {True}];

p[];
p["-- (i) which order the indices run in --"];
(* The vertices of a hypergraph are a set, so the numbering of the array indices is a choice.
   The Hypergraph constructor makes it, by sorting, and these pin down what that does and does
   not buy: determinism given the object, but not invariance under renaming. *)
check["the order does not depend on how the edges were given",
   AdjacencyHypermatrix[Hypergraph[{Edge[{3, 1}], Edge[{2, 3}]}]] ===
   AdjacencyHypermatrix[Hypergraph[{Edge[{2, 3}], Edge[{3, 1}]}]],
   True];
check["the vertices come out sorted, not in order of appearance",
   VertexID /@ VertexList @ Hypergraph[{Edge[{3, 1}], Edge[{2, 3}]}],
   {1, 2, 3}];

hNumT = Hypergraph[{Edge[{1, 2}], Edge[{2, 3}]}];
hStrT = Hypergraph[{Edge[{"z", "a"}], Edge[{"a", "m"}]}];
check["renaming the vertices can permute the indices",
   VertexID /@ VertexList[hStrT], {"a", "m", "z"}];
check["  so two isomorphic hypergraphs may differ as hypermatrices",
   {IsomorphicHypergraphQ[hNumT, hStrT],
    AdjacencyHypermatrix[hNumT] === AdjacencyHypermatrix[hStrT]},
   {True, False}];
check["  and differ by exactly the permutation the renaming induces",
   With[{q = InversePermutation[{3, 1, 2}],
         mN = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[hNumT],
         mS = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[hStrT]},
      mN[[q, q]] === mS],
   True];

gNumT = Hypergraph[{Edge[{1, 2, 3}, "Unordered"], Edge[{1, 1, 2}, "Unordered"]}];
gStrT = Hypergraph[{Edge[{"z", "a", "m"}, "Unordered"], Edge[{"z", "z", "a"}, "Unordered"]}];
check["at higher order the permutation acts in every slot at once",
   With[{q = InversePermutation[{3, 1, 2}],
         aN = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[gNumT],
         aS = Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[gStrT]},
      aN[[q, q, q]] === aS],
   True];

check["composing with CanonicalHypergraph gives an isomorphism invariant",
   AdjacencyHypermatrix[CanonicalHypergraph[hNumT]] ===
   AdjacencyHypermatrix[CanonicalHypergraph[hStrT]],
   True];
check["  and likewise at higher order",
   AdjacencyHypermatrix[CanonicalHypergraph[gNumT]] ===
   AdjacencyHypermatrix[CanonicalHypergraph[gStrT]],
   True];

SeedRandom[20260925];
r7 = Table[
   Module[{n = RandomInteger[{1, 4}], hg, names, hg2},
      hg = Hypergraph[Table[
         Edge[RandomInteger[{1, n}, RandomInteger[{1, 3}]],
              RandomChoice[{"Directed", "Unordered", "Cyclic"}]],
         RandomInteger[{1, 4}]]];
      names = AssociationThread[VertexID /@ VertexList[hg] -> RandomSample[CharacterRange["a", "t"], VertexCount[hg]]];
      hg2 = Hypergraph[Edge[Lookup[names, VertexID /@ EdgeVertices[#]], EdgeSymmetry[#]] & /@ EdgeList[hg]];
      AdjacencyHypermatrix[CanonicalHypergraph[hg]] === AdjacencyHypermatrix[CanonicalHypergraph[hg2]]
   ],
   200];
check["200 random hypergraphs, renamed at random, agree after canonicalizing", Union[r7], {True}];

p[];
p["-- (j) multiplicities come out as integer multiples --"];
check["two copies of one directed edge, one of its reverse",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
      Hypergraph[{Edge[{1, 2}], Edge[{1, 2}], Edge[{2, 1}]}],
   {{0, 2}, {1, 0}}];
check["three copies of an unordered edge give 3 at every tuple of its orbit",
   {Union @ Cases[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
       Hypergraph[Table[Edge[{1, 2, 3}, "Unordered"], 3]], _?Positive, Infinity],
    Length @ Position[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
       Hypergraph[Table[Edge[{1, 2, 3}, "Unordered"], 3]], 3]},
   {{3}, 6}];
check["two copies of a cyclic edge, over its three rotations",
   {Union @ Cases[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
       Hypergraph[Table[Edge[{1, 2, 3}, "Cyclic"], 2]], _?Positive, Infinity],
    Length @ Position[Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
       Hypergraph[Table[Edge[{1, 2, 3}, "Cyclic"], 2]], 2]},
   {{2}, 3}];
check["five empty edges give the 0-array 5",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @ Hypergraph[Table[Edge[{}], 5]],
   5];
check["two spellings of one unordered edge count as two",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
      Hypergraph[{Edge[{1, 2}, "Unordered"], Edge[{2, 1}, "Unordered"]}],
   {{0, 2}, {2, 0}}];

p[];
p["-- (k) labels as entries, for a simple hypergraph --"];
hLab = Hypergraph[{Edge[{1, 2}, "Directed", "a"], Edge[{2, 3}, "Directed", "b"],
                   Edge[{1, 2, 3}, "Unordered", "c"]}];
check["counting is still the default",
   AdjacencyHypermatrix[hLab] === AdjacencyHypermatrix[hLab, "Labeled" -> False], True];
check["the counting entries are integers", AdjacencyHypermatrix[hLab]["Domains"],
   {"Integer", "Integer"}];
check["the labelled entries are labels, which answer to no numeric domain",
   AdjacencyHypermatrix[hLab, "Labeled" -> True]["Domains"], {"Expression", "Expression"}];
check["the labels sit where their edges do",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[hLab, "Labeled" -> True],
   {{0, "a", 0}, {0, 0, "b"}, {0, 0, 0}}];
check["a label fills the whole orbit of its edge, so the array stays symmetric",
   {Length @ Position[Normal @ Last @ HypermatrixArrays @
       AdjacencyHypermatrix[hLab, "Labeled" -> True], "c"],
    ArraySymmetry @ Last @ HypermatrixArrays @ AdjacencyHypermatrix[hLab, "Labeled" -> True]},
   {6, "Symmetric"}];
check["the orders and symmetries are the same either way",
   {AdjacencyHypermatrix[hLab]["Orders"], AdjacencyHypermatrix[hLab]["Symmetries"]} ===
   {AdjacencyHypermatrix[hLab, "Labeled" -> True]["Orders"],
    AdjacencyHypermatrix[hLab, "Labeled" -> True]["Symmetries"]},
   True];
check["an edge of arity zero carries its label",
   Normal @ First @ HypermatrixArrays @
      AdjacencyHypermatrix[Hypergraph[{Edge[{}, "Directed", "empty"]}], "Labeled" -> True],
   "empty"];
check["an unlabelled edge gives None, which absence does not",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 2}, "Directed", "a"], Edge[{2, 1}, "Directed"]}], "Labeled" -> True],
   {{0, "a"}, {None, 0}}];

p["-- and the simplicity it needs --"];
check["a repeated edge cannot be labelled: one entry, two labels",
   Quiet @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 2}, "Directed", "a"], Edge[{1, 2}, "Directed", "b"]}],
      "Labeled" -> True],
   $Failed];
check["  though counting it is no trouble at all",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix @
      Hypergraph[{Edge[{1, 2}, "Directed", "a"], Edge[{1, 2}, "Directed", "b"]}],
   {{0, 2}, {0, 0}}];
check["the same vertices under different symmetries go to different arrays, so that is simple",
   AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 2}, "Directed", "d"], Edge[{1, 2}, "Unordered", "u"]}],
      "Labeled" -> True]["Symmetries"],
   {"Symmetric", "Rigid"}];
check["a vertex repeated within one edge is no obstacle",
   Normal @ First @ HypermatrixArrays @ AdjacencyHypermatrix[
      Hypergraph[{Edge[{1, 1, 2}, "Unordered", "r"]}], "Labeled" -> True] // Flatten // Union,
   {0, "r"}];

p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];

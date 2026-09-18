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


p["=== 1. Numerical arrays: the domain is inferred, or declared ==="];
check["booleans", ArrayDomain[NumericalArray[{True, False}]], "Boolean"];
check["integers", ArrayDomain[NumericalArray[{1, 2, 3}]], "Integer"];
check["rationals", ArrayDomain[NumericalArray[{1/2, 3}]], "Rational"];
check["reals", ArrayDomain[NumericalArray[{1.5, 2}]], "Real"];
check["complexes", ArrayDomain[NumericalArray[{1 + I, 2}]], "Complex"];
check["a finite field", ArrayDomain[NumericalArray[{FiniteField[2, 1][0], FiniteField[2, 1][1]}]],
   FiniteField[2, 1]];
check["a wider domain may be declared", ArrayDomain[NumericalArray[{1, 2}, "Complex"]], "Complex"];
check["a domain too narrow is rejected", Quiet @ NumericalArray[{1.5}, "Integer"], $Failed];
check["an unknown domain is rejected", Quiet @ NumericalArray[{1}, "Quaternion"], $Failed];
check["entries of another field are rejected",
   Quiet @ NumericalArray[{FiniteField[3, 1][1]}, FiniteField[2, 1]], $Failed];


p[];
p["=== 2. Order, dimensions, regularity ==="];
v = NumericalArray[{1, 2, 3}];
m = NumericalArray[{{1, 2, 3}, {4, 5, 6}}];
t = NumericalArray[Array[#1 + #2 + #3 &, {2, 2, 2}]];
check["a 1-array is a vector", {ArrayOrder[v], ArrayDimensions[v]}, {1, {3}}];
check["a 2-array is a matrix", {ArrayOrder[m], ArrayDimensions[m]}, {2, {2, 3}}];
check["a 3-array", {ArrayOrder[t], ArrayDimensions[t]}, {3, {2, 2, 2}}];
check["unequal index lengths make it irregular", RegularArrayQ[m], False];
check["equal index lengths make it regular", RegularArrayQ[t], True];
check["Dimensions works directly", Dimensions[m], {2, 3}];
check["Normal gives the entries", Normal[v], {1, 2, 3}];
check["ArrayEntries is the same", ArrayEntries[v], Normal[v]];
check["a scalar is the array of order zero", ArrayOrder @ NumericalArray[5], 0];
check["  which has no dimensions", ArrayDimensions @ NumericalArray[5], {}];
check["  is vacuously regular", RegularArrayQ @ NumericalArray[5], True];
check["  and carries its single entry", Normal @ NumericalArray[5], 5];
check["a symbolic 0-array is its bare name", Normal @ SymbolicArray[q, {}], q];
check["a ragged list is not an array", Quiet @ NumericalArray[{{1, 2}, {3}}], $Failed];


p[];
p["=== 3. A declared symmetry is verified against the entries ==="];
sym2 = {{1, 2}, {2, 1}};
asym2 = {{1, 2}, {3, 4}};
check["symmetric entries accepted as Symmetric",
   ArraySymmetry[NumericalArray[sym2, Automatic, "Symmetric"]], "Symmetric"];
check["asymmetric entries rejected as Symmetric",
   Quiet @ NumericalArray[asym2, Automatic, "Symmetric"], $Failed];
check["the same entries are fine as Rigid", ArraySymmetry[NumericalArray[asym2]], "Rigid"];
check["Rigid is the default", ArraySymmetry[NumericalArray[sym2]], "Rigid"];
check["an irregular array cannot carry a symmetry",
   Quiet @ NumericalArray[{{1, 2, 3}, {4, 5, 6}}, Automatic, "Symmetric"], $Failed];
check["an unrecognized symmetry is rejected",
   Quiet @ NumericalArray[sym2, Automatic, "Wobbly"], $Failed];
check["spelling is lenient on input",
   ArraySymmetry[NumericalArray[sym2, Automatic, "symmetric"]], "Symmetric"];

(* the indicator of the cyclic orbit of (1,2,3): invariant under rotation, not under reflection *)
cyc3 = Array[If[MemberQ[{{1, 2, 3}, {2, 3, 1}, {3, 1, 2}}, {##}], 1, 0] &, {3, 3, 3}];
check["cyclic but not symmetric entries accepted as Cyclic",
   ArraySymmetry[NumericalArray[cyc3, Automatic, "Cyclic"]], "Cyclic"];
check["  and rejected as Symmetric", Quiet @ NumericalArray[cyc3, Automatic, "Symmetric"], $Failed];
check["symmetric entries are also cyclic",
   ArraySymmetry[NumericalArray[sym2, Automatic, "Cyclic"]], "Cyclic"];


p[];
p["=== 4. Symbolic arrays generate their entries from a name ==="];
check["entries are the name applied to the indices",
   Normal @ SymbolicArray[a, {2, 2}],
   {{a[1, 1], a[1, 2]}, {a[2, 1], a[2, 2]}}];
check["order, dimensions, domain and name",
   {ArrayOrder[#], ArrayDimensions[#], ArrayDomain[#], ArrayName[#]} & @ SymbolicArray[a, {2, 3}],
   {2, {2, 3}, "Symbolic", a}];
check["a numerical array has no name", ArrayName[v], None];
check["a symbolic array's domain is \"Symbolic\"", ArrayDomain[SymbolicArray[a, {2}]], "Symbolic"];
check["a symmetric symbolic array really is symmetric",
   With[{e = Normal @ SymbolicArray[a, {3, 3}, "Symmetric"]}, e === Transpose[e]], True];
check["equivalent index tuples name one entry",
   Normal @ SymbolicArray[a, {2, 2}, "Symmetric"],
   {{a[1, 1], a[1, 2]}, {a[1, 2], a[2, 2]}}];
check["distinct entries: rigid, cyclic, symmetric at order 3",
   Length @ DeleteDuplicates @ Flatten @ Normal @ SymbolicArray[a, {3, 3, 3}, #] & /@
      {"Rigid", "Cyclic", "Symmetric"},
   {27, 11, 10}];
check["cyclic and symmetric differ at order 3",
   Normal[SymbolicArray[a, {3, 3, 3}, "Cyclic"]] =!= Normal[SymbolicArray[a, {3, 3, 3}, "Symmetric"]],
   True];
check["but coincide at order 2, where the cyclic group is the symmetric one",
   Normal[SymbolicArray[a, {3, 3}, "Cyclic"]] === Normal[SymbolicArray[a, {3, 3}, "Symmetric"]],
   True];
check["a string name is allowed", ArrayName[SymbolicArray["A", {2}]], "A"];
check["a name must be a symbol or a string", Quiet @ SymbolicArray[{1, 2}, {2}], $Failed];
check["dimensions must be positive integers", Quiet @ SymbolicArray[a, {2, 0}], $Failed];
check["an irregular symbolic array cannot carry a symmetry",
   Quiet @ SymbolicArray[a, {2, 3}, "Symmetric"], $Failed];


p[];
p["=== 5. Predicates and properties ==="];
check["ArrayObjectQ",
   {ArrayObjectQ[v], ArrayObjectQ[SymbolicArray[a, {2}]], ArrayObjectQ[{1, 2}], ArrayObjectQ[5]},
   {True, True, False, False}];
check["properties", v["Properties"],
   {"Order", "Dimensions", "Symmetry", "Domain", "Name", "Entries", "RegularQ"}];
check["property access", {v["Order"], v["Dimensions"], v["Domain"], v["RegularQ"]},
   {1, {3}, "Integer", True}];
check["an unknown property", Head @ v["Nonsense"], Missing];


p[];
p["=== 6. A hypermatrix holds its arrays in canonical order ==="];
arrays = {
   NumericalArray[Array[1 &, {2, 2, 2}], Automatic, "Symmetric"],
   NumericalArray[{{1, 2}, {3, 4}}],
   NumericalArray[{1, 2}],
   NumericalArray[{1, 2, 3}],
   NumericalArray[{{1, 2}, {2, 1}}, Automatic, "Symmetric"],
   NumericalArray[{{1, 2}, {2, 1}}, Automatic, "Cyclic"]
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
   Hypermatrix[NumericalArray[{1, 2}], NumericalArray[{1, 2, 3}]] ===
      Hypermatrix[{NumericalArray[{1, 2}], NumericalArray[{1, 2, 3}]}], True];
check["raw entries are promoted to numerical arrays",
   HypermatrixArrays[Hypermatrix[{{1, 2}, {{1, 2}, {3, 4}}}]][[1]],
   NumericalArray[{1, 2}, "Integer", "Rigid"]];
check["numerical and symbolic arrays may be mixed",
   Hypermatrix[{SymbolicArray[a, {2, 2}], NumericalArray[{1, 2}]}]["Orders"], {1, 2}];
check["a non-array is rejected", Quiet @ Hypermatrix[{"nope"}], $Failed];
check["Normal gives the entries of each array in order",
   Normal @ Hypermatrix[{NumericalArray[{{1, 2}, {3, 4}}], NumericalArray[{1, 2}]}],
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
      arr = NumericalArray[data];
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
      {ArraySymmetry[NumericalArray[sym, Automatic, "Symmetric"]] === "Symmetric",
       ArraySymmetry[NumericalArray[sym, Automatic, "Cyclic"]] === "Cyclic"}],
   200];
check["200 symmetrized arrays", Union[r2], {True}];

p["-- (c) a cyclically symmetrized array is accepted as Cyclic --"];
r3 = Table[
   Module[{n = RandomInteger[{2, 3}], d = RandomInteger[{2, 3}], data, cyc},
      data = Array[RandomInteger[{0, 4}] &, ConstantArray[d, n]];
      cyc = Total[Transpose[data, RotateLeft[Range[n], #]] & /@ Range[0, n - 1]];
      ArraySymmetry[NumericalArray[cyc, Automatic, "Cyclic"]] === "Cyclic"],
   200];
check["200 cyclically symmetrized arrays", Union[r3], {True}];

p["-- (d) the canonical order does not depend on the order given --"];
r4 = Table[
   Module[{as = Table[NumericalArray[randomData[]], RandomInteger[{2, 6}]]},
      Hypermatrix[as] === Hypermatrix[RandomSample[as]]],
   300];
check["300 shuffled sets of arrays", Union[r4], {True}];

p["-- (e) the canonical order really is sorted by the stated criteria --"];
rank[s_] := First @ FirstPosition[{"Symmetric", "Cyclic", "Rigid"}, s];
r5 = Table[
   Module[{hm2 = Hypermatrix[Table[NumericalArray[randomData[]], RandomInteger[{2, 6}]]], keys},
      keys = {ArrayOrder[#], ArrayDimensions[#], rank[ArraySymmetry[#]]} & /@ HypermatrixArrays[hm2];
      OrderedQ[keys]],
   300];
check["300 hypermatrices are ordered", Union[r5], {True}];


p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];

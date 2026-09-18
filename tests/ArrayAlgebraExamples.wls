#!/usr/bin/env wolframscript
$ProgressReporting = False;
(* Assertions for the special arrays, ArrayMultiply and FindArrayEquations.
   Run with:  wolframscript -file tests/ArrayAlgebraExamples.wls  *)

Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "src", "Hypergraph.wl"}]];
p = Print;
form[x_] := ToString[x, InputForm];
$fails = 0;
check[lbl_, got_, want_] := (If[got =!= want, $fails++];
  p[If[got === want, "  ok   ", "  FAIL "], StringPadRight[lbl, 50], " ", form[got],
    If[got === want, "", "   (expected " <> form[want] <> ")"]]);
show[lbl_, x_] := p[StringPadRight[lbl, 46], " ", form[x]];

p["=== A. ArrayMultiply against known contractions ==="];
SeedRandom[11];
a = RandomInteger[{-3, 3}, {3, 3}];
b = RandomInteger[{-3, 3}, {3, 3}];
pv = RandomInteger[{-3, 3}, 3];
qv = RandomInteger[{-3, 3}, 3];
check["matrix product", Normal @ ArrayMultiply[{a, b}, "ij,jk->ik"], a . b];
check["  index-list form agrees",
   ArrayMultiply[{a, b}, {{1, 2}, {2, 3}} -> {1, 3}], ArrayMultiply[{a, b}, "ij,jk->ik"]];
check["elementwise product", Normal @ ArrayMultiply[{a, b}, "ij,ij->ij"], a * b];
check["transpose", Normal @ ArrayMultiply[{a}, "ij->ji"], Transpose[a]];
check["trace", Normal @ ArrayMultiply[{a}, "ii->"], Tr[a]];
check["  contracting every index gives a 0-array", ArrayOrder @ ArrayMultiply[{a}, "ii->"], 0];
check["diagonal", Normal @ ArrayMultiply[{a}, "ii->i"], Diagonal[a]];
check["dot of vectors", Normal @ ArrayMultiply[{pv, qv}, "i,i->"], pv . qv];
check["outer product", Normal @ ArrayMultiply[{pv, qv}, "i,j->ij"], Outer[Times, pv, qv]];
check["3-way elementwise (Einstein default output)",
   Normal @ ArrayMultiply[{pv, qv, pv}, "i,i,i"], Total[pv qv pv]];
check["matrix-vector", Normal @ ArrayMultiply[{a, pv}, "ij,j->i"], a . pv];
check["accepts array objects",
   ArrayMultiply[{NumericalArray[a], NumericalArray[b]}, "ij,jk->ik"],
   ArrayMultiply[{a, b}, "ij,jk->ik"]];

p[];
p["-- the reference's ternary products, checked against their explicit definitions --"];
SeedRandom[12];
{x, y, z} = Table[RandomInteger[{-3, 3}, {3, 3, 3}], 3];
P1 = Table[Sum[x[[i, j, q]] y[[i, q, k]] z[[q, j, k]], {q, 3}], {i, 3}, {j, 3}, {k, 3}];
check["BM    ijp,ipk,pjk->ijk", Normal @ ArrayMultiply[{x, y, z}, "ijp,ipk,pjk->ijk"], P1];
P2 = Table[Sum[x[[i, r, q]] y[[r, j, s]] z[[q, s, k]], {r, 3}, {q, 3}, {s, 3}], {i, 3}, {j, 3}, {k, 3}];
check["TriForce ipq,pjr,qrk->ijk", Normal @ ArrayMultiply[{x, y, z}, "ipq,pjr,qrk->ijk"], P2];
P3 = Table[Sum[x[[i, j, q]] y[[r, s, q]] z[[r, s, k]], {q, 3}, {r, 3}, {s, 3}], {i, 3}, {j, 3}, {k, 3}];
check["Fish  ijp,qrp,qrk->ijk", Normal @ ArrayMultiply[{x, y, z}, "ijp,qrp,qrk->ijk"], P3];

p[];
p["-- errors --"];
check["wrong array count", Quiet @ ArrayMultiply[{a}, "ij,jk->ik"], $Failed];
check["wrong rank", Quiet @ ArrayMultiply[{a, b}, "i,jk->ik"], $Failed];
check["inconsistent index length",
   Quiet @ ArrayMultiply[{RandomInteger[{1, 2}, {2, 2}], RandomInteger[{1, 2}, {3, 3}]}, "ij,jk->ik"], $Failed];
check["output index not among inputs", Quiet @ ArrayMultiply[{a}, "ij->ijk"], $Failed];

p[];
p["=== B. Special arrays ==="];
show["ZeroArray[2, 2]", Normal @ ZeroArray[2, 2]];
show["OneArray[2, 2]", Normal @ OneArray[2, 2]];
show["IdentityArray[2, 3]", Normal @ IdentityArray[2, 3]];
show["IdentityArray[3, 2]", Normal @ IdentityArray[3, 2]];
show["PartialIdentityArray[3, 2, {1, 2}]", Normal @ PartialIdentityArray[3, 2, {1, 2}]];
check["IdentityArray[2, 3] is the identity matrix", Normal @ IdentityArray[2, 3], IdentityMatrix[3]];
check["full partial identity is the identity",
   PartialIdentityArray[3, 2, {1, 2, 3}], IdentityArray[3, 2]];
check["constant arrays are symmetric", ArraySymmetry @ ZeroArray[3, 2], "Symmetric"];
check["full identity is symmetric", ArraySymmetry @ IdentityArray[3, 2], "Symmetric"];
check["partial identity is rigid", ArraySymmetry @ PartialIdentityArray[3, 2, {1, 2}], "Rigid"];
check["domains", ArrayDomain @ ZeroArray[2, 2, "Real"], "Real"];
check["GF(2) identity",
   Normal @ IdentityArray[2, 2, FiniteField[2, 1]],
   {{FiniteField[2, 1][1], FiniteField[2, 1][0]}, {FiniteField[2, 1][0], FiniteField[2, 1][1]}}];
check["irregular zero array", ArrayDimensions @ ZeroArray[{2, 3}], {2, 3}];
check["  and it is rigid, not symmetric", ArraySymmetry @ ZeroArray[{2, 3}], "Rigid"];
check["bad positions rejected", Quiet @ PartialIdentityArray[3, 2, {1, 5}], $Failed];

p[];
p["-- the identity acts as an identity for matrix multiplication --"];
check["I . a == a", ArrayMultiply[{Normal @ IdentityArray[2, 3], a}, "ij,jk->ik"],
   NumericalArray[a]];

p[];
p["-- arrays of order zero --"];
check["a scalar is the 0-array", ArrayOrder @ NumericalArray[7], 0];
check["ZeroArray[{}] is the scalar zero", Normal @ ZeroArray[{}], 0];
check["ZeroArray[0, 3] is the same", ZeroArray[0, 3], ZeroArray[{}]];
check["a 0-array contributes no index labels",
   Normal @ ArrayMultiply[{NumericalArray[3], a}, ",ij->ij"], 3 a];
check["contracting every index gives one back",
   Normal @ ArrayMultiply[{a, a}, "ij,ji->"], Tr[a . a]];
check["a symbolic 0-array is its name", Normal @ SymbolicArray[s0, {}], s0];

p[];
p["=== B2. Element-wise operations ==="];
u2 = {{1, 2}, {3, 4}};
v2 = {{5, 6}, {7, 8}};
check["ArrayAdd", Normal @ ArrayAdd[u2, v2], u2 + v2];
check["ArrayTimes", Normal @ ArrayTimes[u2, v2], u2 v2];
check["several at once", Normal @ ArrayAdd[u2, v2, u2], 2 u2 + v2];
check["one argument is itself", Normal @ ArrayAdd[u2], u2];
check["a 0-array multiplies every entry", Normal @ ArrayTimes[2, u2], 2 u2];
check["  and adds to every entry", Normal @ ArrayAdd[1, u2], 1 + u2];
check["  so scalar multiplication needs no name of its own",
   ArrayTimes[2, u2], ArrayTimes[NumericalArray[2], u2]];
check["Plus on array objects", Normal[NumericalArray[u2] + NumericalArray[v2]], u2 + v2];
check["Times on array objects", Normal[NumericalArray[u2] NumericalArray[v2]], u2 v2];
check["a difference", Normal[NumericalArray[u2] - NumericalArray[v2]], u2 - v2];
check["a quotient by a number", Normal[NumericalArray[u2] / 2], u2 / 2];
check["a numerical result is an array object", Head @ ArrayAdd[u2, v2], NumericalArray];
check["anything symbolic gives the plain entries",
   2 SymbolicArray[sc, {2}], {2 sc[1], 2 sc[2]}];
check["the two kinds may be combined, entry by entry",
   NumericalArray[{1, 2}] + SymbolicArray[sc, {2}], {1 + sc[1], 2 + sc[2]}];
check["dimensions must agree", Quiet @ ArrayAdd[{1, 2}, {1, 2, 3}], $Failed];
check["a shared symmetry is preserved",
   ArraySymmetry @ ArrayAdd[NumericalArray[{{1, 2}, {2, 3}}, "Integer", "Symmetric"],
                            NumericalArray[{{0, 1}, {1, 0}}, "Integer", "Symmetric"]],
   "Symmetric"];
check["  and is not claimed when they disagree",
   ArraySymmetry @ ArrayAdd[NumericalArray[{{1, 2}, {2, 3}}, "Integer", "Symmetric"],
                            NumericalArray[u2]],
   "Rigid"];
check["adding nothing is refused", Quiet @ ArrayAdd[], $Failed];

p[];
p["=== C. FindArrayEquations ==="];
dot[u_List, w_List] := ArrayMultiply[{u, w}, "ij,jk->ik"] // Normal;
BM[u_List, w_List, t_List] := ArrayMultiply[{u, w, t}, "ijp,ipk,pjk->ijk"] // Normal;
Fish[u_List, w_List, t_List] := ArrayMultiply[{u, w, t}, "ijp,qrp,qrk->ijk"] // Normal;

p["-- (1) binary matrix multiplication on three arguments --"];
t0 = AbsoluteTime[];
eq1 = FindArrayEquations[{dot}, {a1, a2, a3}, "Order" -> 2, "Dimension" -> 3];
p["   time: ", Round[AbsoluteTime[] - t0, 0.01], "s"];
Scan[p["   ", form[#]] &, eq1];
check["associativity found", MemberQ[eq1, HoldPattern[Equal[dot[dot[a1, a2], a3], dot[a1, dot[a2, a3]]]] |
                                          HoldPattern[Equal[dot[a1, dot[a2, a3]], dot[dot[a1, a2], a3]]]], True];
check["only associativity", Length[eq1], 1];

p[];
p["-- (2) Fish on five arguments --"];
t0 = AbsoluteTime[];
eq2 = FindArrayEquations[{Fish}, {a1, a2, a3, a4, a5}];
p["   time: ", Round[AbsoluteTime[] - t0, 0.01], "s"];
Scan[p["   ", form[#]] &, eq2];
check["Fish satisfies something", Length[eq2] > 0, True];
check["and what it satisfies is a twisted ternary associativity",
   MemberQ[eq2, HoldPattern[Equal[Fish[a1, a2, Fish[a3, a4, a5]], Fish[Fish[a1, a2, a3], a4, a5]]] |
                HoldPattern[Equal[Fish[Fish[a1, a2, a3], a4, a5], Fish[a1, a2, Fish[a3, a4, a5]]]]],
   True];

p[];
p["-- (3) BM on five arguments --"];
t0 = AbsoluteTime[];
eq3 = FindArrayEquations[{BM}, {a1, a2, a3, a4, a5}];
p["   time: ", Round[AbsoluteTime[] - t0, 0.01], "s"];
Scan[p["   ", form[#]] &, eq3];
check["BM satisfies nothing", eq3, {}];

p[];
p["-- fixed argument order --"];
eq4 = FindArrayEquations[{dot}, {a1, a2, a3}, "Order" -> 2, "Dimension" -> 3, "Permute" -> False];
show["dot, no permutation", eq4];

p[];
p["=== C2. Numerical and symbolic arrays, and several operations ==="];
trace[u_List] := Normal @ ArrayMultiply[{u}, "pp->"];
perp[u_List] := Normal @ ArrayMultiply[{u}, "ij->ji"];
sa = Normal @ SymbolicArray[sym1, {2, 2}];
sb = Normal @ SymbolicArray[sym2, {2, 2}];

p["-- a symbolic array is a list of lists of terms, and so is the result --"];
check["symbolic entries", sa,
   {{sym1[1, 1], sym1[1, 2]}, {sym1[2, 1], sym1[2, 2]}}];
check["a symbolic product is a plain list of lists", Head @ ArrayMultiply[{sa, sb}, "ij,jk->ik"], List];
check["  and agrees with Dot", ArrayMultiply[{sa, sb}, "ij,jk->ik"], sa . sb];
check["a numerical product is a NumericalArray",
   Head @ ArrayMultiply[{{{1, 2}, {3, 4}}, {{0, 1}, {1, 0}}}, "ij,jk->ik"], NumericalArray];
check["the two kinds may not be mixed",
   Quiet @ ArrayMultiply[{{{1, 2}, {3, 4}}, sb}, "ij,jk->ik"], $Failed];
check["a SymbolicArray object is accepted directly",
   ArrayMultiply[{SymbolicArray[sym1, {2, 2}], SymbolicArray[sym2, {2, 2}]}, "ij,jk->ik"],
   sa . sb];

p[];
p["-- the trace, as a contraction of one array onto no indices --"];
check["numerical", trace[{{1, 2}, {3, 4}}], Tr[{{1, 2}, {3, 4}}]];
check["symbolic", trace[sa], Tr[sa]];
check["a symbolic one is an expression, not an array", Head @ trace[sa], Plus];
check["a numerical one is the 0-array", ArrayOrder @ ArrayMultiply[{{{1, 2}, {3, 4}}}, "pp->"], 0];

p[];
p["-- the order of the test arrays is inferred from the operations --"];
(* Reported up to which of its equivalent forms the reduction keeps: the law is the same whichever
   side the transposes are collected on, and whichever way round the two arguments are named. *)
check["matrix operations give order 2, so the transpose law is found",
   MemberQ[FindArrayEquations[{perp, dot}, {a1, a2}],
      HoldPattern[Equal[dot[perp[a1], perp[a2]], perp[dot[a2, a1]]]] |
      HoldPattern[Equal[perp[dot[a2, a1]], dot[perp[a1], perp[a2]]]]],
   True];
check["and so is the involutivity of the transpose, which needs it twice over",
   MemberQ[FindArrayEquations[{perp, dot}, {a1, a2}],
      HoldPattern[Equal[a1, perp[perp[a1]]]] | HoldPattern[Equal[perp[perp[a1]], a1]]],
   True];
check["the transpose law needs the arguments permuted, the involutivity does not",
   FindArrayEquations[{perp, dot}, {a1, a2}, "Permute" -> False], {a1 == perp[perp[a1]]}];
check["and with no nesting of the unary operation, neither is within reach",
   FindArrayEquations[{perp, dot}, {a1, a2}, "Permute" -> False, "UnaryDepth" -> 0], {}];
check["trace and matrix product give the cyclic property",
   FindArrayEquations[{trace, dot}, {a1, a2}],
   {trace[dot[a1, a2]] == trace[dot[a2, a1]]}];

p[];
p["-- a shape the operations do not accept is reported, not returned as no equations --"];
wrongShape[u_List] := Normal @ ArrayMultiply[{u}, "ijkl->lkji"];
check["nothing evaluates", Quiet @ FindArrayEquations[{wrongShape}, {a1}, "Order" -> 2], $Failed];

p[];
p["=== D. Randomized checks ==="];
SeedRandom[20260920];

p["-- (a) ArrayMultiply agrees with Dot on random matrices --"];
r1 = Table[
   Module[{u = RandomInteger[{-4, 4}, {3, 4}], w = RandomInteger[{-4, 4}, {4, 2}]},
      Normal[ArrayMultiply[{u, w}, "ij,jk->ik"]] === u . w],
   200];
check["200 random matrix products", Union[r1], {True}];

p["-- (b) the result does not depend on which letters the indices are given --"];
r2 = Table[
   Module[{u = RandomInteger[{-3, 3}, {2, 2, 2}], w = RandomInteger[{-3, 3}, {2, 2, 2}],
           t = RandomInteger[{-3, 3}, {2, 2, 2}]},
      ArrayMultiply[{u, w, t}, "ijp,ipk,pjk->ijk"] ===
      ArrayMultiply[{u, w, t}, "xyz,xzw,zyw->xyw"]],
   200];
check["200 relabellings of the BM specification", Union[r2], {True}];

p["-- (c) the string and index-list forms agree --"];
r3 = Table[
   Module[{u = RandomInteger[{-3, 3}, {3, 3}], w = RandomInteger[{-3, 3}, {3, 3}]},
      ArrayMultiply[{u, w}, "ij,jk->ik"] === ArrayMultiply[{u, w}, {{1, 2}, {2, 3}} -> {1, 3}]],
   200];
check["200 pairs", Union[r3], {True}];

p["-- (d) the identity array is an identity for matrix multiplication --"];
r4 = Table[
   Module[{d = RandomInteger[{1, 4}], u},
      u = RandomInteger[{-4, 4}, {d, d}];
      Normal[ArrayMultiply[{Normal @ IdentityArray[2, d], u}, "ij,jk->ik"]] === u],
   200];
check["200 dimensions", Union[r4], {True}];

p["-- (e) every equation reported really holds, on arrays it never saw --"];
verifyOn[eq_, arrays_] := Module[{asg = AssociationThread[Keys[arrays] -> Values[arrays]]},
   (eq[[1]] /. asg) === (eq[[2]] /. asg)
];
r5 = Flatten @ Table[
   Module[{arrays},
      arrays = AssociationThread[{a1, a2, a3, a4, a5} ->
         Table[RandomInteger[{-4, 4}, {2, 2, 2}], 5]];
      verifyOn[#, arrays] & /@ eq2],
   50];
check["50 fresh sets of arrays against every Fish equation", Union[r5], {True}];

p["-- (f) the Fish equations are the same at dimension 3 --"];
check["dimension 2 and dimension 3 agree",
   Sort[eq2] === Sort[FindArrayEquations[{Fish}, {a1, a2, a3, a4, a5}, "Dimension" -> 3]], True];

p["-- (g) a missing arity is reported rather than guessed --"];
check["unknown operation", Quiet @ FindArrayEquations[{undefinedOp}, {a1, a2, a3}], $Failed];
check["explicit arity works instead",
   Length @ FindArrayEquations[{dot -> 2}, {a1, a2, a3}, "Order" -> 2, "Dimension" -> 3], 1];

p[];
p[If[$fails === 0, "=== ALL CHECKS PASSED ===", "=== " <> ToString[$fails] <> " CHECK(S) FAILED ==="]];


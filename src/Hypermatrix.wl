(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- arrays and hypermatrices*)


(* ::Text:: *)
(*Arrays come in two kinds, which are different objects: a numerical array carries explicit*)
(*entries drawn from a value domain, and a symbolic array carries only a name, its entries being*)
(*generated as expressions of the indices. A hypermatrix is a canonically ordered list of arrays.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[
    NumericalArray, SymbolicArray, Hypermatrix,
    ArrayObjectQ, RegularArrayQ, HypermatrixQ,
    ArrayOrder, ArrayDimensions, ArraySymmetry, ArrayDomain, ArrayName, ArrayEntries,
    HypermatrixArrays
];


(* ::Section:: *)
(*Usage*)


NumericalArray::usage =
    "NumericalArray[data] represents an array with the explicit entries data, whose value domain " <>
    "is inferred.\n" <>
    "NumericalArray[data, dom] declares the value domain to be dom.\n" <>
    "NumericalArray[data, dom, sym] additionally declares the index symmetry sym, one of " <>
    "\"Rigid\" (the default), \"Cyclic\" or \"Symmetric\".\n" <>
    "A scalar is accepted as the array of order zero, which has no indices and a single entry.";

SymbolicArray::usage =
    "SymbolicArray[name, dims] represents an array of the given dimensions whose entries are the " <>
    "expressions name[i1, i2, ...] formed from name and the indices.\n" <>
    "SymbolicArray[name, dims, sym] additionally declares the index symmetry sym, one of " <>
    "\"Rigid\" (the default), \"Cyclic\" or \"Symmetric\".\n" <>
    "SymbolicArray[name, {}] is the array of order zero, whose single entry is name itself.";

Hypermatrix::usage =
    "Hypermatrix[{arr1, arr2, ...}] represents the given arrays as a hypermatrix, holding them in " <>
    "canonical order.\n" <>
    "Hypermatrix[arr1, arr2, ...] is equivalent.\n" <>
    "The order is by array order, then by dimensions, then by symmetry, taking \"Symmetric\" " <>
    "before \"Cyclic\" before \"Rigid\".";

ArrayObjectQ::usage = "ArrayObjectQ[expr] gives True if expr is a valid NumericalArray or SymbolicArray.";
RegularArrayQ::usage =
    "RegularArrayQ[arr] gives True if every index of arr ranges over the same length, and False " <>
    "otherwise, in which case the array is irregular.";
HypermatrixQ::usage = "HypermatrixQ[expr] gives True if expr is a valid Hypermatrix object.";

ArrayOrder::usage = "ArrayOrder[arr] gives the number of indices of arr. An array of order n is an n-array.";
ArrayDimensions::usage = "ArrayDimensions[arr] gives the list of lengths of the indices of arr.";
ArraySymmetry::usage =
    "ArraySymmetry[arr] gives the declared index symmetry of arr: \"Rigid\", \"Cyclic\" or \"Symmetric\".";
ArrayDomain::usage =
    "ArrayDomain[arr] gives the value domain of a numerical array, or \"Symbolic\" for a symbolic one.";
ArrayName::usage =
    "ArrayName[arr] gives the name of a symbolic array, or None for a numerical one.";
ArrayEntries::usage =
    "ArrayEntries[arr] gives the entries of arr as a nested list. Normal[arr] is equivalent.";

HypermatrixArrays::usage =
    "HypermatrixArrays[hm] gives the arrays of hm, in canonical order. Normal[hm] gives their entries.";


(* ::Section:: *)
(*Messages*)


NumericalArray::data = "`1` is not a rectangular array.";
NumericalArray::dom = "`1` is not a recognized value domain.";
NumericalArray::entries = "Not every entry lies in the declared domain `1`.";
NumericalArray::sym = "`1` is not a valid array symmetry; expected \"Rigid\", \"Cyclic\" or \"Symmetric\".";
NumericalArray::irreg = "An irregular array cannot be declared `1`; only a regular array can carry a symmetry.";
NumericalArray::nosym = "The given entries are not invariant under the index permutations of `1`.";
NumericalArray::spec = "`1` is not a valid numerical array specification.";

SymbolicArray::name = "`1` is not a valid array name; expected a symbol or a string.";
SymbolicArray::dims = "`1` is not a list of positive integer index lengths.";
SymbolicArray::sym = NumericalArray::sym;
SymbolicArray::irreg = NumericalArray::irreg;
SymbolicArray::spec = "`1` is not a valid symbolic array specification.";

Hypermatrix::arrays = "`1` is not a list of arrays.";
Hypermatrix::spec = "`1` is not a valid hypermatrix specification.";


Begin["`Private`"];


(* ::Section:: *)
(*Symmetries*)


(* Canonical order, and also the order hypermatrices sort by: symmetric before cyclic before rigid,
   from the most constrained to the least. *)
$ArraySymmetries = {"Symmetric", "Cyclic", "Rigid"};

normalizeArraySymmetry[s_String] := Switch[ToLowerCase[s],
    "rigid" | "none", "Rigid",
    "cyclic", "Cyclic",
    "symmetric" | "symmetrical", "Symmetric",
    _, $Failed
];
normalizeArraySymmetry[_] := $Failed;

(* The least index tuple equivalent to idx under the symmetry, mirroring the treatment of the
   vertex order of a hyperedge. This is what makes a symbolic array's entries respect its
   symmetry automatically: equivalent index tuples name the same entry. *)
arrayCanonicalIndex[idx_List, "Rigid"] := idx;
arrayCanonicalIndex[idx_List, "Symmetric"] := Sort[idx];
arrayCanonicalIndex[{}, "Cyclic"] := {};
arrayCanonicalIndex[idx_List, "Cyclic"] := First @ Sort @ NestList[RotateLeft, idx, Length[idx] - 1];


(* ::Section:: *)
(*Array data*)


(* An array must be rectangular. A scalar counts as an array of order zero: it has no indices, so
   its list of dimensions is empty, and it is the unit of the multilinear structure -- what
   multiplies an array is a 0-array, and what a full contraction leaves behind is one. *)
arrayDataQ[data_] := ArrayQ[data] || ! ListQ[data];

dataOrder[data_] := If[ListQ[data], ArrayDepth[data], 0];
dataDims[data_] := If[ListQ[data], Dimensions[data], {}];

(* A 0-array is vacuously regular: SameQ of no arguments is True. *)
regularDataQ[data_] := SameQ @@ dataDims[data];

(* The entries of an array, as a flat list, a 0-array contributing its single value. *)
entriesOf[data_] := If[ListQ[data], Flatten[data], {data}];

(* Invariance under a generating set implies invariance under the whole group, so one transposition
   and one rotation settle "Symmetric", and the rotation alone settles "Cyclic". *)
arrayHasSymmetryQ[data_, "Rigid"] := True;
arrayHasSymmetryQ[data_, "Cyclic"] := With[{n = dataOrder[data]},
    n <= 1 || (regularDataQ[data] && data === Transpose[data, RotateLeft[Range[n]]])
];
arrayHasSymmetryQ[data_, "Symmetric"] := With[{n = dataOrder[data]},
    n <= 1 || (
        regularDataQ[data] &&
        data === Transpose[data, RotateLeft[Range[n]]] &&
        data === Transpose[data, Join[{2, 1}, Range[3, n]]]
    )
];
arrayHasSymmetryQ[___] := False;


(* ::Section:: *)
(*Value domains*)


(* The recognized domains, narrowest first, which is the order inference tries them in. A finite
   field is written FiniteField[p, n] and is not in this list because it is not a single domain.
   Adding a domain is a line here and a line in domainTestFor. *)
$ArrayDomains = {"Boolean", "Integer", "Rational", "Real", "Complex"};

domainTestFor["Boolean"] = BooleanQ;
domainTestFor["Integer"] = IntegerQ;
domainTestFor["Rational"] = (IntegerQ[#] || Head[#] === Rational) &;
domainTestFor["Real"] = (NumberQ[#] && Im[#] === 0) &;
domainTestFor["Complex"] = NumberQ;
domainTestFor[field_FiniteField] :=
    With[{f = field}, (Head[#] === FiniteFieldElement && #["Field"] === f) &];
domainTestFor[_] := None;

validArrayDomainQ[dom_] := domainTestFor[dom] =!= None;

domainHoldsQ[data_, dom_] := With[{test = domainTestFor[dom]},
    test =!= None && AllTrue[entriesOf[data], test]
];

(* The narrowest recognized domain containing every entry. *)
inferArrayDomain[data_] := With[{es = entriesOf[data]},
    Which[
        es =!= {} && AllTrue[es, Head[#] === FiniteFieldElement &] && SameQ @@ (#["Field"] & /@ es),
            First[es]["Field"],
        True,
            SelectFirst[$ArrayDomains, domainHoldsQ[data, #] &, $Failed]
    ]
];


(* ::Section:: *)
(*NumericalArray*)


(* One rewrite rule brings any NumericalArray into canonical form. Its guard looks only at the
   pattern variables, never at the expression itself, so there is no re-entrancy. *)
canonicalNumericalArgsQ[data_, dom_, sym_String] :=
    arrayDataQ[data] && validArrayDomainQ[dom] && MemberQ[$ArraySymmetries, sym] &&
    domainHoldsQ[data, dom] &&
    (sym === "Rigid" || regularDataQ[data]) &&
    arrayHasSymmetryQ[data, sym];
canonicalNumericalArgsQ[___] := False;

NumericalArray[args___] /; ! canonicalNumericalArgsQ[args] := makeNumericalArray[args];

makeNumericalArray[data_] := makeNumericalArray[data, Automatic, "Rigid"];
makeNumericalArray[data_, dom_] := makeNumericalArray[data, dom, "Rigid"];
makeNumericalArray[data_, dom_, sym_] := Module[{s, d},
    If[ ! arrayDataQ[data],
        Message[NumericalArray::data, HoldForm[data]]; Return[$Failed, Module]];
    s = normalizeArraySymmetry[sym];
    If[s === $Failed, Message[NumericalArray::sym, sym]; Return[$Failed, Module]];
    d = If[dom === Automatic, inferArrayDomain[data], dom];
    If[ d === $Failed || ! validArrayDomainQ[d],
        Message[NumericalArray::dom, dom]; Return[$Failed, Module]];
    If[ ! domainHoldsQ[data, d],
        Message[NumericalArray::entries, d]; Return[$Failed, Module]];
    If[ s =!= "Rigid" && ! regularDataQ[data],
        Message[NumericalArray::irreg, s]; Return[$Failed, Module]];
    If[ ! arrayHasSymmetryQ[data, s],
        Message[NumericalArray::nosym, s]; Return[$Failed, Module]];
    NumericalArray[data, d, s]
];
makeNumericalArray[args___] := (Message[NumericalArray::spec, HoldForm[NumericalArray[args]]]; $Failed);


(* ::Section:: *)
(*SymbolicArray*)


validArrayNameQ[name_] := MatchQ[name, _Symbol | _String];

(* An empty list of dimensions is a 0-array, whose single entry is the bare name. *)
validDimsQ[dims_] := VectorQ[dims, IntegerQ[#] && Positive[#] &];

canonicalSymbolicArgsQ[name_, dims_List, sym_String] :=
    validArrayNameQ[name] && validDimsQ[dims] && MemberQ[$ArraySymmetries, sym] &&
    (sym === "Rigid" || SameQ @@ dims);
canonicalSymbolicArgsQ[___] := False;

SymbolicArray[args___] /; ! canonicalSymbolicArgsQ[args] := makeSymbolicArray[args];

makeSymbolicArray[name_, dims_] := makeSymbolicArray[name, dims, "Rigid"];
makeSymbolicArray[name_, dims_, sym_] := Module[{s, d = Developer`ToList[dims]},
    If[ ! validArrayNameQ[name],
        Message[SymbolicArray::name, HoldForm[name]]; Return[$Failed, Module]];
    If[ ! validDimsQ[d],
        Message[SymbolicArray::dims, HoldForm[dims]]; Return[$Failed, Module]];
    s = normalizeArraySymmetry[sym];
    If[s === $Failed, Message[SymbolicArray::sym, sym]; Return[$Failed, Module]];
    If[ s =!= "Rigid" && ! SameQ @@ d,
        Message[SymbolicArray::irreg, s]; Return[$Failed, Module]];
    SymbolicArray[name, d, s]
];
makeSymbolicArray[args___] := (Message[SymbolicArray::spec, HoldForm[SymbolicArray[args]]]; $Failed);


(* ::Section:: *)
(*Array accessors*)


(* Matching on _NumericalArray and taking the object apart with Part: writing the pattern as
   NumericalArray[data_, ___] would run the constructor on the pattern itself, because the
   arguments of a definition's left-hand side are evaluated. *)

ArrayObjectQ[a_NumericalArray] := canonicalNumericalArgsQ @@ a;
ArrayObjectQ[a_SymbolicArray] := canonicalSymbolicArgsQ @@ a;
ArrayObjectQ[_] := False;

ArrayEntries[a_NumericalArray] := First[a];
ArrayEntries[a_SymbolicArray] := With[{name = First[a], dims = a[[2]], sym = a[[3]]},
    (* With no indices the single entry is the name itself; Array would give name[] instead. *)
    If[dims === {}, name, Array[name @@ arrayCanonicalIndex[{##}, sym] &, dims]]
];

ArrayDimensions[a_NumericalArray] := dataDims[First[a]];
ArrayDimensions[a_SymbolicArray] := a[[2]];

ArrayOrder[a_NumericalArray] := dataOrder[First[a]];
ArrayOrder[a_SymbolicArray] := Length[a[[2]]];

ArraySymmetry[a : _NumericalArray | _SymbolicArray] := a[[3]];

ArrayDomain[a_NumericalArray] := a[[2]];
ArrayDomain[_SymbolicArray] := "Symbolic";

ArrayName[_NumericalArray] := None;
ArrayName[a_SymbolicArray] := First[a];

RegularArrayQ[a : _NumericalArray | _SymbolicArray] := SameQ @@ ArrayDimensions[a];
RegularArrayQ[_] := False;

NumericalArray /: Normal[a_NumericalArray] := ArrayEntries[a];
SymbolicArray /: Normal[a_SymbolicArray] := ArrayEntries[a];
NumericalArray /: Dimensions[a_NumericalArray] := ArrayDimensions[a];
SymbolicArray /: Dimensions[a_SymbolicArray] := ArrayDimensions[a];

(* Two definitions rather than one with Alternatives in the head: a SubValues left-hand side may
   not have an alternation there, and SetDelayed::altno rejects it outright, which would leave the
   property interface silently missing. *)
(a_NumericalArray)[prop_String] /; ArrayObjectQ[a] := arrayProp[a, prop];
(a_SymbolicArray)[prop_String] /; ArrayObjectQ[a] := arrayProp[a, prop];

arrayProp[_, "Properties"] := {
    "Order", "Dimensions", "Symmetry", "Domain", "Name", "Entries", "RegularQ"
};
arrayProp[a_, "Order"] := ArrayOrder[a];
arrayProp[a_, "Dimensions"] := ArrayDimensions[a];
arrayProp[a_, "Symmetry"] := ArraySymmetry[a];
arrayProp[a_, "Domain"] := ArrayDomain[a];
arrayProp[a_, "Name"] := ArrayName[a];
arrayProp[a_, "Entries"] := ArrayEntries[a];
arrayProp[a_, "RegularQ"] := RegularArrayQ[a];
arrayProp[_, prop_] := Missing["UnknownProperty", prop];


(* ::Section:: *)
(*Hypermatrix*)


(* Lowest order first, then the shorter dimensions, then symmetry from the most constrained to the
   least. Two arrays alike in all three are put in the Wolfram Language's own canonical order, so
   that a hypermatrix is a function of the set of arrays it was given and of nothing else. *)
arraySortKey[a_] := {
    ArrayOrder[a],
    ArrayDimensions[a],
    First @ FirstPosition[$ArraySymmetries, ArraySymmetry[a]],
    a
};

toArray[a : _NumericalArray | _SymbolicArray] /; ArrayObjectQ[a] := a;
toArray[data_] /; arrayDataQ[data] := NumericalArray[data];
toArray[spec_] := (Message[Hypermatrix::arrays, HoldForm[spec]]; $Failed);

canonicalHypermatrixArgsQ[arrays_List] :=
    AllTrue[arrays, ArrayObjectQ] && arrays === SortBy[arrays, arraySortKey];
canonicalHypermatrixArgsQ[___] := False;

Hypermatrix[args___] /; ! canonicalHypermatrixArgsQ[args] := makeHypermatrix[args];

makeHypermatrix[arrays_List] := Module[{as = toArray /@ arrays},
    If[! AllTrue[as, ArrayObjectQ], Return[$Failed, Module]];
    Hypermatrix[SortBy[as, arraySortKey]]
];
makeHypermatrix[a_, b__] := makeHypermatrix[{a, b}];
makeHypermatrix[args___] := (Message[Hypermatrix::spec, HoldForm[Hypermatrix[args]]]; $Failed);

HypermatrixQ[hm_Hypermatrix] := canonicalHypermatrixArgsQ @@ hm;
HypermatrixQ[_] := False;

HypermatrixArrays[hm_Hypermatrix] := First[hm];

Hypermatrix /: Normal[hm_Hypermatrix] := ArrayEntries /@ HypermatrixArrays[hm];
Hypermatrix /: Length[hm_Hypermatrix] := Length[HypermatrixArrays[hm]];

(hm_Hypermatrix)[prop_String] /; HypermatrixQ[hm] := hypermatrixProp[hm, prop];

hypermatrixProp[_, "Properties"] := {
    "Arrays", "Length", "Orders", "Dimensions", "Symmetries", "Domains", "Entries"
};
hypermatrixProp[hm_, "Arrays"] := HypermatrixArrays[hm];
hypermatrixProp[hm_, "Length"] := Length[HypermatrixArrays[hm]];
hypermatrixProp[hm_, "Orders"] := ArrayOrder /@ HypermatrixArrays[hm];
hypermatrixProp[hm_, "Dimensions"] := ArrayDimensions /@ HypermatrixArrays[hm];
hypermatrixProp[hm_, "Symmetries"] := ArraySymmetry /@ HypermatrixArrays[hm];
hypermatrixProp[hm_, "Domains"] := ArrayDomain /@ HypermatrixArrays[hm];
hypermatrixProp[hm_, "Entries"] := ArrayEntries /@ HypermatrixArrays[hm];
hypermatrixProp[_, prop_] := Missing["UnknownProperty", prop];


(* ::Section:: *)
(*Formatting*)


(* A summary box rather than the entries: an array of any order prints in constant space, and the
   things one wants to see at a glance are its order, dimensions and symmetry. Normal gives the
   entries. Graphical rendering of 1-, 2- and 3-arrays is a separate job, still to come. *)

$ArrayIcon := $ArrayIcon = Graphics[
    {
        FaceForm[Hue[0.63, 0.26, 0.89]], EdgeForm[Hue[0.63, 0.7, 0.33]],
        Table[Rectangle[{i, -j}, {i + 0.82, -j + 0.82}], {i, 0, 2}, {j, 0, 2}]
    },
    ImageSize -> Dynamic[{Automatic, 3.2 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}],
    PlotRangePadding -> 0.3
];

arraySummary[a_, head_String, form_] := BoxForm`ArrangeSummaryBox[
    head, a, $ArrayIcon,
    {
        {BoxForm`SummaryItem[{"Order: ", ArrayOrder[a]}]},
        {BoxForm`SummaryItem[{"Dimensions: ", ArrayDimensions[a]}]}
    },
    {
        {BoxForm`SummaryItem[{"Symmetry: ", ArraySymmetry[a]}]},
        {BoxForm`SummaryItem[{If[ArrayName[a] === None, "Domain: ", "Name: "],
                              If[ArrayName[a] === None, ArrayDomain[a], ArrayName[a]]}]},
        {BoxForm`SummaryItem[{"Regular: ", RegularArrayQ[a]}]}
    },
    form, "Interpretable" -> Automatic
];

NumericalArray /: MakeBoxes[a_NumericalArray /; ArrayObjectQ[a], form : StandardForm | TraditionalForm] :=
    arraySummary[a, "NumericalArray", form];

SymbolicArray /: MakeBoxes[a_SymbolicArray /; ArrayObjectQ[a], form : StandardForm | TraditionalForm] :=
    arraySummary[a, "SymbolicArray", form];

Hypermatrix /: MakeBoxes[hm_Hypermatrix /; HypermatrixQ[hm], form : StandardForm | TraditionalForm] :=
    BoxForm`ArrangeSummaryBox[
        "Hypermatrix", hm, $ArrayIcon,
        {
            {BoxForm`SummaryItem[{"Arrays: ", Length[HypermatrixArrays[hm]]}]},
            {BoxForm`SummaryItem[{"Orders: ", hm["Orders"]}]}
        },
        {
            {BoxForm`SummaryItem[{"Dimensions: ", hm["Dimensions"]}]},
            {BoxForm`SummaryItem[{"Symmetries: ", hm["Symmetries"]}]},
            {BoxForm`SummaryItem[{"Domains: ", hm["Domains"]}]}
        },
        form, "Interpretable" -> Automatic
    ];


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];

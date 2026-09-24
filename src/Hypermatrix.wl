(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- arrays and hypermatrices*)


(* ::Text:: *)
(*An array is one object, ArrayObject, whatever its entries are. Nothing about the structure of an*)
(*array depends on what sits in it: the order, the dimensions and the symmetry are the same*)
(*questions whether the entries are integers, elements of a finite field, or labels from a*)
(*dictionary. So there is one head, holding entries and a symmetry, and the value domain is read*)
(*off the entries when asked for rather than declared alongside them.*)
(**)
(*Arrays whose entries are generated rather than given are made by a function like any other:*)
(*GenerateSymbolicArray builds the entries name[i, j, ...] from a name, as ZeroArray and*)
(*IdentityArray build theirs from a shape.*)
(**)
(*A hypermatrix is a canonically ordered list of arrays.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[
    ArrayObject, GenerateSymbolicArray, Hypermatrix,
    ArrayObjectQ, RegularArrayQ, HypermatrixQ,
    ArrayOrder, ArrayDimensions, ArraySymmetry, ArrayDomain, ArrayEntries,
    HypermatrixArrays
];


(* ::Section:: *)
(*Usage*)


ArrayObject::usage =
    "ArrayObject[data] represents the array with entries data.\n" <>
    "ArrayObject[data, sym] additionally declares the index symmetry sym, one of \"Rigid\" (the " <>
    "default), \"Cyclic\" or \"Symmetric\".\n" <>
    "The entries may be anything at all: numbers, elements of a finite field, symbolic " <>
    "expressions, labels. What the array is made of is read off the entries by ArrayDomain rather " <>
    "than declared.\n" <>
    "A scalar is accepted as the array of order zero, which has no indices and a single entry.";

GenerateSymbolicArray::usage =
    "GenerateSymbolicArray[name, dims] gives the array of the given dimensions whose entries are " <>
    "the expressions name[i1, i2, ...] formed from name and the indices.\n" <>
    "GenerateSymbolicArray[name, dims, sym] declares the index symmetry sym, and generates entries " <>
    "that respect it: index tuples equivalent under sym name one entry.\n" <>
    "GenerateSymbolicArray[name, {}] is the array of order zero, whose single entry is name itself.";

Hypermatrix::usage =
    "Hypermatrix[{arr1, arr2, ...}] represents the given arrays as a hypermatrix, holding them in " <>
    "canonical order.\n" <>
    "Hypermatrix[arr1, arr2, ...] is equivalent.\n" <>
    "The order is by array order, then by dimensions, then by symmetry, taking \"Symmetric\" " <>
    "before \"Cyclic\" before \"Rigid\".";

ArrayObjectQ::usage = "ArrayObjectQ[expr] gives True if expr is a valid ArrayObject.";
RegularArrayQ::usage =
    "RegularArrayQ[arr] gives True if every index of arr ranges over the same length, and False " <>
    "otherwise, in which case the array is irregular.";
HypermatrixQ::usage = "HypermatrixQ[expr] gives True if expr is a valid Hypermatrix object.";

ArrayOrder::usage = "ArrayOrder[arr] gives the number of indices of arr. An array of order n is an n-array.";
ArrayDimensions::usage = "ArrayDimensions[arr] gives the list of lengths of the indices of arr.";
ArraySymmetry::usage =
    "ArraySymmetry[arr] gives the declared index symmetry of arr: \"Rigid\", \"Cyclic\" or \"Symmetric\".";
ArrayDomain::usage =
    "ArrayDomain[arr] gives the narrowest recognized value domain containing every entry of arr, " <>
    "or \"Expression\" if the entries lie in none of them.\n" <>
    "It is computed from the entries, not declared: an array is not told what it is made of.";
ArrayEntries::usage =
    "ArrayEntries[arr] gives the entries of arr as a nested list. Normal[arr] is equivalent.";

HypermatrixArrays::usage =
    "HypermatrixArrays[hm] gives the arrays of hm, in canonical order. Normal[hm] gives their entries.";


(* ::Section:: *)
(*Messages*)


ArrayObject::data = "`1` is not a rectangular array.";
ArrayObject::sym = "`1` is not a valid array symmetry; expected \"Rigid\", \"Cyclic\" or \"Symmetric\".";
ArrayObject::irreg = "An irregular array cannot be declared `1`; only a regular array can carry a symmetry.";
ArrayObject::nosym = "The given entries are not invariant under the index permutations of `1`.";
ArrayObject::spec = "`1` is not a valid array specification.";

GenerateSymbolicArray::name = "`1` is not a valid array name; expected a symbol or a string.";
GenerateSymbolicArray::dims = "`1` is not a list of positive integer index lengths.";
GenerateSymbolicArray::sym = ArrayObject::sym;
GenerateSymbolicArray::irreg = ArrayObject::irreg;
GenerateSymbolicArray::spec = "`1` is not a valid symbolic array specification.";

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
   vertex order of a hyperedge. This is what lets generated entries respect a symmetry
   automatically: equivalent index tuples name the same entry. *)
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


(* The recognized domains, narrowest first, which is the order inference walks them in. A finite
   field is written FiniteField[p, n] and is not in this list because it is not a single domain.
   Nothing declares a domain any more: it is a question asked of the entries, and entries that
   answer to none of these are simply expressions. *)
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

(* The narrowest recognized domain containing every entry, or $Failed when none does. *)
inferArrayDomain[data_] := With[{es = entriesOf[data]},
    Which[
        es =!= {} && AllTrue[es, Head[#] === FiniteFieldElement &] && SameQ @@ (#["Field"] & /@ es),
            First[es]["Field"],
        True,
            SelectFirst[$ArrayDomains, domainHoldsQ[data, #] &, $Failed]
    ]
];


(* ::Section:: *)
(*ArrayObject*)


(* One rewrite rule brings any ArrayObject into canonical form. Its guard looks only at the
   pattern variables, never at the expression itself, so there is no re-entrancy. *)
canonicalArrayArgsQ[data_, sym_String] :=
    arrayDataQ[data] && MemberQ[$ArraySymmetries, sym] &&
    (sym === "Rigid" || regularDataQ[data]) &&
    arrayHasSymmetryQ[data, sym];
canonicalArrayArgsQ[___] := False;

ArrayObject[args___] /; ! canonicalArrayArgsQ[args] := makeArrayObject[args];

makeArrayObject[data_] := makeArrayObject[data, "Rigid"];
makeArrayObject[data_, sym_] := Module[{s},
    If[ ! arrayDataQ[data],
        Message[ArrayObject::data, HoldForm[data]]; Return[$Failed, Module]];
    s = normalizeArraySymmetry[sym];
    If[s === $Failed, Message[ArrayObject::sym, sym]; Return[$Failed, Module]];
    If[ s =!= "Rigid" && ! regularDataQ[data],
        Message[ArrayObject::irreg, s]; Return[$Failed, Module]];
    If[ ! arrayHasSymmetryQ[data, s],
        Message[ArrayObject::nosym, s]; Return[$Failed, Module]];
    ArrayObject[data, s]
];
makeArrayObject[args___] := (Message[ArrayObject::spec, HoldForm[ArrayObject[args]]]; $Failed);


(* ::Section:: *)
(*GenerateSymbolicArray*)


validArrayNameQ[name_] := MatchQ[name, _Symbol | _String];

(* An empty list of dimensions is a 0-array, whose single entry is the bare name. *)
validDimsQ[dims_] := VectorQ[dims, IntegerQ[#] && Positive[#] &];

GenerateSymbolicArray[name_, dims_] := GenerateSymbolicArray[name, dims, "Rigid"];
GenerateSymbolicArray[name_, dims_, sym_] := Module[{s, d = Developer`ToList[dims], entries},
    If[ ! validArrayNameQ[name],
        Message[GenerateSymbolicArray::name, HoldForm[name]]; Return[$Failed, Module]];
    If[ ! validDimsQ[d],
        Message[GenerateSymbolicArray::dims, HoldForm[dims]]; Return[$Failed, Module]];
    s = normalizeArraySymmetry[sym];
    If[s === $Failed, Message[GenerateSymbolicArray::sym, sym]; Return[$Failed, Module]];
    If[ s =!= "Rigid" && ! SameQ @@ d,
        Message[GenerateSymbolicArray::irreg, s]; Return[$Failed, Module]];

    (* The entry at an index tuple is the name applied to the least tuple in its orbit, so the
       generated array satisfies its declared symmetry by construction and the constructor below
       has nothing to complain about. With no indices the single entry is the name itself, where
       Array would give name[]. *)
    entries = If[ d === {},
        name,
        Array[name @@ arrayCanonicalIndex[{##}, s] &, d]
    ];
    ArrayObject[entries, s]
];
GenerateSymbolicArray[args___] :=
    (Message[GenerateSymbolicArray::spec, HoldForm[GenerateSymbolicArray[args]]]; $Failed);


(* ::Section:: *)
(*Array accessors*)


(* Matching on _ArrayObject and taking the object apart with Part: writing the pattern as
   ArrayObject[data_, ___] would run the constructor on the pattern itself, because the arguments
   of a definition's left-hand side are evaluated. *)

ArrayObjectQ[a_ArrayObject] := canonicalArrayArgsQ @@ a;
ArrayObjectQ[_] := False;

ArrayEntries[a_ArrayObject] := First[a];
ArrayDimensions[a_ArrayObject] := dataDims[First[a]];
ArrayOrder[a_ArrayObject] := dataOrder[First[a]];
ArraySymmetry[a_ArrayObject] := Last[a];

(* Computed, not stored: the array is not told what it is made of, it is asked. *)
ArrayDomain[a_ArrayObject] := With[{d = inferArrayDomain[First[a]]},
    If[d === $Failed, "Expression", d]
];

RegularArrayQ[a_ArrayObject] := SameQ @@ ArrayDimensions[a];
RegularArrayQ[_] := False;

ArrayObject /: Normal[a_ArrayObject] := ArrayEntries[a];
ArrayObject /: Dimensions[a_ArrayObject] := ArrayDimensions[a];

(a_ArrayObject)[prop_String] /; ArrayObjectQ[a] := arrayProp[a, prop];

arrayProp[_, "Properties"] := {
    "Order", "Dimensions", "Symmetry", "Domain", "Entries", "RegularQ"
};
arrayProp[a_, "Order"] := ArrayOrder[a];
arrayProp[a_, "Dimensions"] := ArrayDimensions[a];
arrayProp[a_, "Symmetry"] := ArraySymmetry[a];
arrayProp[a_, "Domain"] := ArrayDomain[a];
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

toArray[a_ArrayObject] /; ArrayObjectQ[a] := a;
toArray[data_] /; arrayDataQ[data] := ArrayObject[data];
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
   entries, and ArrayGraphics draws them. *)

$ArrayIcon := $ArrayIcon = Graphics[
    {
        FaceForm[Hue[0.63, 0.26, 0.89]], EdgeForm[Hue[0.63, 0.7, 0.33]],
        Table[Rectangle[{i, -j}, {i + 0.82, -j + 0.82}], {i, 0, 2}, {j, 0, 2}]
    },
    ImageSize -> Dynamic[{Automatic, 3.2 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}],
    PlotRangePadding -> 0.3
];

ArrayObject /: MakeBoxes[a_ArrayObject /; ArrayObjectQ[a], form : StandardForm | TraditionalForm] :=
    BoxForm`ArrangeSummaryBox[
        "ArrayObject", a, $ArrayIcon,
        {
            {BoxForm`SummaryItem[{"Order: ", ArrayOrder[a]}]},
            {BoxForm`SummaryItem[{"Dimensions: ", ArrayDimensions[a]}]}
        },
        {
            {BoxForm`SummaryItem[{"Symmetry: ", ArraySymmetry[a]}]},
            {BoxForm`SummaryItem[{"Domain: ", ArrayDomain[a]}]},
            {BoxForm`SummaryItem[{"Regular: ", RegularArrayQ[a]}]}
        },
        form, "Interpretable" -> Automatic
    ];

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

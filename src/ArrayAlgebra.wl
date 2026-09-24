(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- array algebra*)


(* ::Text:: *)
(*Special arrays, the contraction operation ArrayMultiply, and FindArrayEquations, which searches*)
(*for equational identities between nestings of a given set of array operations.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[
    ZeroArray, OneArray, IdentityArray, PartialIdentityArray,
    ArrayMultiply, ArrayAdd, ArrayTimes, FindArrayEquations,
    ArrayObject
];


(* ::Section:: *)
(*Usage*)


ZeroArray::usage =
    "ZeroArray[n, d] gives the n-array with every index of length d and every entry zero.\n" <>
    "ZeroArray[dims] gives the array of the given dimensions.\n" <>
    "ZeroArray[spec, dom] gives its entries in the value domain dom.";

OneArray::usage =
    "OneArray[n, d] gives the n-array with every index of length d and every entry one.\n" <>
    "OneArray[dims] gives the array of the given dimensions.\n" <>
    "OneArray[spec, dom] gives its entries in the value domain dom.";

IdentityArray::usage =
    "IdentityArray[n, d] gives the n-array with every index of length d whose entry is one where " <>
    "all n indices agree and zero elsewhere.\n" <>
    "IdentityArray[n, d, dom] gives its entries in the value domain dom.";

PartialIdentityArray::usage =
    "PartialIdentityArray[n, d, positions] gives the n-array with every index of length d whose " <>
    "entry is one where the indices at the given positions agree and zero elsewhere.\n" <>
    "PartialIdentityArray[n, d, positions, dom] gives its entries in the value domain dom.\n" <>
    "With positions covering every index this is IdentityArray[n, d].";

ArrayMultiply::usage =
    "ArrayMultiply[{arr1, arr2, ...}, spec] contracts the given arrays according to the index " <>
    "specification spec, in the manner of Einstein summation.\n" <>
    "The specification may be written as a string, \"ij,jk->ik\", or as a rule between lists of " <>
    "index labels, {{1, 2}, {2, 3}} -> {1, 3}.\n" <>
    "With the output part omitted, the result carries those indices that occur exactly once, in " <>
    "sorted order, and every repeated index is summed over.";

ArrayAdd::usage =
    "ArrayAdd[arr1, arr2, ...] adds the given arrays entry by entry.\n" <>
    "The arrays must have the same dimensions, except that an array of order zero -- a scalar -- " <>
    "is added to every entry.\n" <>
    "arr1 + arr2 is equivalent.";

ArrayTimes::usage =
    "ArrayTimes[arr1, arr2, ...] multiplies the given arrays entry by entry.\n" <>
    "The arrays must have the same dimensions, except that an array of order zero -- a scalar -- " <>
    "multiplies every entry, so that scalar multiplication is the same operation.\n" <>
    "arr1 arr2 is equivalent.";

FindArrayEquations::usage =
    "FindArrayEquations[ops, args] searches for equational identities among all the ways the " <>
    "operations ops can be nested over the arguments args, and returns them as a list of " <>
    "equations.\n" <>
    "Each operation is given as op -> arity, or as op alone when its arity can be read off its " <>
    "definition. The arguments are symbols, and may be repeated.\n" <>
    "Candidate equations are first screened on random numerical arrays, which is fast, and only " <>
    "the survivors are verified on symbolic arrays.\n" <>
    "An operation of arity one may be applied repeatedly, up to the option \"UnaryDepth\", so " <>
    "that a law such as the involutivity of the transpose is within reach.";


(* ::Section:: *)
(*Messages*)


ArrayMultiply::spec = "`1` is not a valid index specification.";
ArrayMultiply::count = "The specification names `1` arrays but `2` were given.";
ArrayMultiply::rank = "Array `1` has `2` indices but its specification names `3`.";
ArrayMultiply::dims = "Index `1` is used with inconsistent lengths `2`.";
ArrayMultiply::out = "The output index `1` does not occur among the inputs.";
ArrayMultiply::array = "Argument `1` is not a rectangular array.";

ArrayAdd::array = "Argument `1` is not a rectangular array.";
ArrayAdd::dims =
    "An entry-by-entry operation needs arrays of the same dimensions, apart from arrays of order \
zero, which spread over any shape; the dimensions given were `1`.";
ArrayAdd::args = "At least one array is expected.";
ArrayTimes::array = ArrayAdd::array;
ArrayTimes::dims = ArrayAdd::dims;
ArrayTimes::args = ArrayAdd::args;

ZeroArray::spec = "`1` is not a valid array shape.";
OneArray::spec = ZeroArray::spec;
IdentityArray::spec = ZeroArray::spec;
PartialIdentityArray::spec = ZeroArray::spec;
PartialIdentityArray::pos = "`1` is not a list of distinct index positions between 1 and `2`.";
ZeroArray::dom = "`1` is not a recognized value domain.";
OneArray::dom = ZeroArray::dom;
IdentityArray::dom = ZeroArray::dom;
PartialIdentityArray::dom = ZeroArray::dom;

FindArrayEquations::arity = "The arity of `1` could not be determined; give it as `1` -> n.";
FindArrayEquations::nest = "No nesting of the given operations uses exactly `1` arguments.";
FindArrayEquations::shape =
    "The given operations do not all evaluate on arrays of any order from 1 to 5 with indices of \
length `1`. Give the shape explicitly with \"Order\" and \"Dimension\".";
FindArrayEquations::depth = "\"UnaryDepth\" -> `1` is not a non-negative integer.";
FindArrayEquations::eval =
    "No nesting of the given operations evaluated on arrays of order `1` with indices of length \
`2`, so no equation could be found either way. Check that the operations accept arrays of that \
shape, or set \"Order\" and \"Dimension\" to ones they do.";


Begin["`Private`"];


(* ::Section:: *)
(*Special arrays*)


(* Zero and one in each recognized domain. Note that this package's "Boolean" is True and False;
   the two-element field, which the reference file calls boolean, is FiniteField[2, 1]. *)
domainZero["Boolean"] = False;
domainOne["Boolean"] = True;
domainZero["Integer"] = 0;      domainOne["Integer"] = 1;
domainZero["Rational"] = 0;     domainOne["Rational"] = 1;
domainZero["Real"] = 0.;        domainOne["Real"] = 1.;
domainZero["Complex"] = 0;      domainOne["Complex"] = 1;
domainZero[f_FiniteField] := f[0];
domainOne[f_FiniteField] := f[1];
domainZero[_] := $Failed;
domainOne[_] := $Failed;

(* A shape is either a list of index lengths or a pair (order, length). The empty shape, and
   equally an order of zero, give the 0-array holding a single entry. *)
shapeDims[dims_List] /; VectorQ[dims, IntegerQ[#] && Positive[#] &] := dims;
shapeDims[n_Integer, d_Integer] /; n >= 0 && d >= 1 := ConstantArray[d, n];
shapeDims[___] := $Failed;

constantArrayObject[head_Symbol, value_, dims_, dom_] := Module[{sym},
    (* a constant array is invariant under every permutation of its indices, so it is symmetric
       whenever it is regular *)
    sym = If[SameQ @@ dims, "Symmetric", "Rigid"];
    ArrayObject[If[dims === {}, value, ConstantArray[value, dims]], sym]
];

makeConstant[head_Symbol, zeroOrOne_, args___, dom_] := Module[{dims, v},
    dims = shapeDims[args];
    If[dims === $Failed, Message[MessageName[head, "spec"], {args}]; Return[$Failed, Module]];
    v = zeroOrOne[dom];
    If[v === $Failed, Message[MessageName[head, "dom"], dom]; Return[$Failed, Module]];
    constantArrayObject[head, v, dims, dom]
];

ZeroArray[args___] := makeZeroOne[ZeroArray, domainZero, args];
OneArray[args___] := makeZeroOne[OneArray, domainOne, args];

(* The domain, when given, is the last argument; everything before it is the shape. *)
makeZeroOne[head_, zeroOrOne_, dims_List] := makeConstant[head, zeroOrOne, dims, "Integer"];
makeZeroOne[head_, zeroOrOne_, dims_List, dom_] := makeConstant[head, zeroOrOne, dims, dom];
makeZeroOne[head_, zeroOrOne_, n_Integer, d_Integer] := makeConstant[head, zeroOrOne, n, d, "Integer"];
makeZeroOne[head_, zeroOrOne_, n_Integer, d_Integer, dom_] := makeConstant[head, zeroOrOne, n, d, dom];
makeZeroOne[head_, _, args___] := (Message[MessageName[head, "spec"], {args}]; $Failed);


(* The generalized identity: one where all the indices at the chosen positions agree. With every
   position chosen this is the full identity, which is symmetric; with only some of them it
   generally is not, so the symmetry is claimed only when it holds. *)
partialIdentity[head_, n_, d_, positions_, dom_] := Module[{zero, one, data, sym},
    If[ ! (IntegerQ[n] && n >= 1 && IntegerQ[d] && d >= 1),
        Message[MessageName[head, "spec"], {n, d}]; Return[$Failed, Module]];
    If[ ! (VectorQ[positions, IntegerQ] && DuplicateFreeQ[positions] &&
           AllTrue[positions, 1 <= # <= n &] && Length[positions] >= 1),
        Message[PartialIdentityArray::pos, positions, n]; Return[$Failed, Module]];
    zero = domainZero[dom]; one = domainOne[dom];
    If[ zero === $Failed,
        Message[MessageName[head, "dom"], dom]; Return[$Failed, Module]];
    data = Array[If[SameQ @@ {##}[[positions]], one, zero] &, ConstantArray[d, n]];
    sym = If[Sort[positions] === Range[n], "Symmetric", "Rigid"];
    ArrayObject[data, sym]
];

IdentityArray[n_Integer, d_Integer] := partialIdentity[IdentityArray, n, d, Range[n], "Integer"];
IdentityArray[n_Integer, d_Integer, dom_] := partialIdentity[IdentityArray, n, d, Range[n], dom];
IdentityArray[args___] := (Message[IdentityArray::spec, {args}]; $Failed);

PartialIdentityArray[n_Integer, d_Integer, positions_List] :=
    partialIdentity[PartialIdentityArray, n, d, positions, "Integer"];
PartialIdentityArray[n_Integer, d_Integer, positions_List, dom_] :=
    partialIdentity[PartialIdentityArray, n, d, positions, dom];
PartialIdentityArray[args___] := (Message[PartialIdentityArray::spec, {args}]; $Failed);


(* ::Section:: *)
(*ArrayMultiply*)


(* An index specification is read either from a string, "ij,jk->ik", where each character is an
   index label, or from a rule between lists of labels, {{1, 2}, {2, 3}} -> {1, 3}. Omitting the
   output part means the standard Einstein convention: whatever occurs exactly once survives. *)

(* The segments between the separators, the empty ones kept. StringSplit drops those, and they
   carry meaning here: an array of order zero contributes no index labels at all, and an output
   part written empty asks for every index to be contracted away. *)
splitKeepEmpty[chars_List, sep_] := Module[{bounds},
    bounds = Join[{0}, Flatten @ Position[chars, sep, 1], {Length[chars] + 1}];
    Table[Take[chars, {bounds[[k]] + 1, bounds[[k + 1]] - 1}], {k, Length[bounds] - 1}]
];

parseArraySpec[spec_String] := Module[{clean, arrow, inStr, outStr},
    clean = StringDelete[spec, Whitespace];
    arrow = StringPosition[clean, "->"];
    If[Length[arrow] > 1, Return[$Failed, Module]];
    {inStr, outStr} = If[ arrow === {},
        {clean, Automatic},
        {StringTake[clean, {1, arrow[[1, 1]] - 1}], StringDrop[clean, arrow[[1, 2]]]}
    ];
    {splitKeepEmpty[Characters[inStr], ","], Replace[outStr, s_String :> Characters[s]]}
];
parseArraySpec[Rule[ins_List, out_List]] /; AllTrue[ins, ListQ] := {ins, out};
parseArraySpec[ins_List] /; AllTrue[ins, ListQ] := {ins, Automatic};
parseArraySpec[_] := $Failed;

(* Raw nested lists and array objects are both acceptable inputs; an array object comes through as
   its entries, which is what the contraction then works with. *)
arrayDataOf[a_] := If[ArrayObjectQ[a], ArrayEntries[a], a];

ArrayMultiply[arrays_List, spec_] := Module[
    {parsed, ins, out, data, occurrences, dimOf, bad, labels, summed,
     outDims, sumTuples, outTuples, entries},

    parsed = parseArraySpec[spec];
    If[parsed === $Failed, Message[ArrayMultiply::spec, spec]; Return[$Failed, Module]];
    {ins, out} = parsed;

    data = arrayDataOf /@ arrays;
    If[ Length[ins] =!= Length[data],
        Message[ArrayMultiply::count, Length[ins], Length[data]]; Return[$Failed, Module]];

    If[ ! AllTrue[data, arrayDataQ],
        Message[ArrayMultiply::array, First @ FirstPosition[data, _ ? (! arrayDataQ[#] &)]];
        Return[$Failed, Module]];

    Do[
        If[ Length[ins[[k]]] =!= dataOrder[data[[k]]],
            Message[ArrayMultiply::rank, k, dataOrder[data[[k]]], Length[ins[[k]]]];
            Return[$Failed, Module]],
        {k, Length[data]}
    ];

    (* every occurrence of a label fixes a length; they must agree *)
    occurrences = Catenate @ MapThread[Thread[#1 -> dataDims[#2]] &, {ins, data}];
    dimOf = GroupBy[occurrences, First -> Last];
    bad = Select[dimOf, ! SameQ @@ # &];
    If[ Length[bad] > 0,
        Message[ArrayMultiply::dims, First @ Keys[bad], First @ Values[bad]];
        Return[$Failed, Module]];
    dimOf = First /@ dimOf;

    labels = Keys[dimOf];
    If[ out === Automatic,
        out = Sort @ Keys @ Select[Counts[Flatten[ins]], # === 1 &]
    ];
    If[ ! SubsetQ[labels, out],
        Message[ArrayMultiply::out, First @ Complement[out, labels]]; Return[$Failed, Module]];

    summed = DeleteCases[labels, Alternatives @@ out];
    outDims = Lookup[dimOf, out];
    outTuples = Tuples[Range /@ outDims];
    sumTuples = Tuples[Range /@ Lookup[dimOf, summed]];

    (* One pass over the free indices, summing over the contracted ones. An index may occur in any
       number of the arrays, and may occur in the output as well, so this covers diagonals and
       multi-way contractions that a pairwise TensorContract could not express. *)
    entries = Table[
        With[{outAsg = AssociationThread[out -> ot]},
            Total @ Table[
                With[{asg = Join[outAsg, AssociationThread[summed -> st]]},
                    (* Extract[x, {}] gives {} rather than x, so a 0-array's single entry, which
                       has no position to speak of, is taken directly. *)
                    Times @@ MapThread[
                        If[#2 === {}, #1, Extract[#1, Lookup[asg, #2]]] &,
                        {data, ins}
                    ]
                ],
                {st, sumTuples}
            ]
        ],
        {ot, outTuples}
    ];

    (* Contracting every index leaves a single entry, which is the 0-array. Whatever the entries
       are, the result is an array of them; Normal recovers them, so a fully contracted result
       reads as the scalar it is. *)
    With[{result = If[out === {}, First[entries], ArrayReshape[entries, outDims]]},
        ArrayObject[result]
    ]
];

ArrayMultiply[args___] := (Message[ArrayMultiply::spec, {args}]; $Failed);


(* ::Section:: *)
(*Element-wise operations*)


(* Addition and multiplication of arrays entry by entry, which is what makes an array a point of a
   vector space rather than merely a table. The arguments are given directly rather than in a list,
   because a list of arrays is itself an array and there would be no telling the two apart.
   A 0-array has a single entry and no shape of its own, so it combines with an array of any
   dimensions: this is why scalar multiplication needs no separate name, being ArrayTimes with a
   0-array. Every other argument must have the same dimensions. *)

(* The symmetry of the result. An entry-by-entry operation treats each index tuple separately, so
   it preserves any symmetry that all of its arguments share; where they disagree, or where a plain
   list was given and nothing was declared, nothing is claimed. *)
elementwiseSymmetry[arrays_List, data_List] := With[{
    syms = DeleteDuplicates @ MapThread[
        Which[
            dataOrder[#2] === 0, Nothing,
            ArrayObjectQ[#1], ArraySymmetry[#1],
            True, "Rigid"
        ] &,
        {arrays, data}
    ]
},
    If[Length[syms] === 1, First[syms], "Rigid"]
];

elementwise[head_Symbol, op_, arrays_List] := Module[{data, shapes, result, sym},
    If[ arrays === {},
        Message[MessageName[head, "args"]]; Return[$Failed, Module]];

    data = arrayDataOf /@ arrays;
    If[ ! AllTrue[data, arrayDataQ],
        Message[MessageName[head, "array"], First @ FirstPosition[data, _ ? (! arrayDataQ[#] &)]];
        Return[$Failed, Module]];

    (* Every shape but the empty one has to agree; the empty one belongs to the 0-arrays. *)
    shapes = DeleteDuplicates @ DeleteCases[dataDims /@ data, {}];
    If[ Length[shapes] > 1,
        Message[MessageName[head, "dims"], dataDims /@ data]; Return[$Failed, Module]];

    (* Plus and Times are Listable, so a single application threads over every index at once, and a
       0-array, being a bare value, spreads over the whole shape of its own accord. *)
    result = op @@ data;
    sym = elementwiseSymmetry[arrays, data];

    (* One head for every array, so there is nothing to decide here: whatever the entries turned
       out to be, the result is an array of them. *)
    ArrayObject[result, sym]
];

ArrayAdd[arrays__] := elementwise[ArrayAdd, Plus, {arrays}];
ArrayTimes[arrays__] := elementwise[ArrayTimes, Times, {arrays}];
ArrayAdd[] := (Message[ArrayAdd::args]; $Failed);
ArrayTimes[] := (Message[ArrayTimes::args]; $Failed);

(* Plus and Times on array objects are the same operations, so that the ordinary arithmetic
   notation may be used: a + b, 2 a, a - b, a / 2. Nothing here feeds an array object back into
   Plus or Times -- elementwise works on the entries -- so there is no recursion. *)
ArrayObject /: Plus[a___, b_ArrayObject, c___] := ArrayAdd[a, b, c];
ArrayObject /: Times[a___, b_ArrayObject, c___] := ArrayTimes[a, b, c];


(* ::Section:: *)
(*FindArrayEquations*)


(* The arity of an operation, read off the first of its definitions when it is not given. *)
operationArity[Rule[f_, n_Integer]] := {f, n};
operationArity[f_] := With[{
    n = Replace[
        DownValues[f],
        {{HoldPattern[Verbatim[HoldPattern][_[args___]] :> _], ___} :> Length[{args}], _ -> $Failed}
    ]
},
    If[IntegerQ[n] && n >= 1, {f, n}, $Failed]
];

Options[FindArrayEquations] = {
    "Permute" -> True,
    "Order" -> Automatic,
    "Dimension" -> 2,
    "Trials" -> 2,
    "IncludeArguments" -> True,
    "UnaryDepth" -> 2,
    "Reduce" -> True
};


(* ::Subsection:: *)
(*Terms*)


(* All the ways of cutting an ordered list into k consecutive non-empty blocks. Nesting an
   operation of arity k over a sequence of arguments is exactly choosing such a cut and then
   nesting again within each block. *)
orderedBlocks[list_List, k_Integer] := Which[
    k < 1 || Length[list] < k, {},
    k === 1, {{list}},
    True, Catenate @ Table[
        Prepend[#, Take[list, i]] & /@ orderedBlocks[Drop[list, i], k - 1],
        {i, Length[list] - k + 1}
    ]
];

(* Operations of arity one are the exception: they take a term to a term of the same shape, so
   they may be applied over and over at any point without consuming an argument. Every term is
   therefore offered wrapped in from zero to d nested unary operations. Without this, a unary
   operation could appear at most once at each place, and a law such as the involutivity of the
   transpose, which needs it twice, would never be among the candidates. *)
unaryLayers[terms_List, uops_List, d_Integer] := If[uops === {} || d < 1,
    terms,
    DeleteDuplicates @ Catenate @ NestList[
        Function[ts, Catenate @ Map[Function[t, Map[Function[u, u[t]], uops]], ts]],
        terms,
        d
    ]
];

(* Every nesting of the operations over the arguments in the given order, with the unary ones free
   to repeat. With no unary operations this is exactly Groupings[args, arities]. *)
generateTerms[arguments_List, naryOps_List, uops_List, d_Integer] := Module[{rec},
    rec[args_List] := rec[args] = unaryLayers[
        If[ Length[args] === 1,
            args,
            DeleteDuplicates @ Catenate @ Table[
                Catenate @ Map[
                    Function[blocks, Function[parts, First[op] @@ parts] /@ Tuples[rec /@ blocks]],
                    orderedBlocks[args, Last[op]]
                ],
                {op, naryOps}
            ]
        ],
        uops, d
    ];
    rec[arguments]
];


(* ::Subsection:: *)
(*Irredundancy*)


(* An equation that follows from the others is not worth reporting, and once unary operations may
   repeat there are a great many that do: knowing that a transpose is an involution, every term
   with a doubled transpose buried in it equals the term without, and each of those equalities
   would be reported as a law of its own.

   Each equation is read as a schema, its arguments standing for arbitrary terms, and is turned
   into a rewrite rule that fires only when it makes the term it acts on strictly smaller. Any
   number of such rules is then a terminating rewriting system whatever their shape, since every
   step goes down a well-founded order; and two sides that rewrite to a common form are equal by
   the rules used, which is a proof that the equation between them adds nothing. Equations are
   considered smallest first, so the simple laws are kept and their consequences are dropped. *)

(* How deeply the unary operations sit: the total size of the arguments they are applied to. A
   term is smaller when its unary operations are nearer the leaves, then when it is shorter, then
   by the Wolfram Language's own order, which makes the comparison definite.

   Size alone would not do. The law perp[dot[u, v]] == dot[perp[v], perp[u]] has the shorter side
   on the left, and used in that direction it collects transposes on the outside, where they never
   meet; used in the other direction it drives them down to the leaves, where doubled transposes
   come together and cancel. It is the second direction that reduces the consequences, and this
   measure is what picks it. *)
unaryWeight[t_] := Total @ Cases[t, _[s_] :> LeafCount[s], {0, Infinity}];

termKey[t_] := {unaryWeight[t], LeafCount[t]};

termLessQ[x_, y_] := With[{kx = termKey[x], ky = termKey[y]},
    If[kx === ky, Order[x, y] === 1, OrderedQ[{kx, ky}]]
];

equationRule[pair_List, args_List] := Module[{big, small, vars, patt, bigTerm, smallTerm},
    {big, small} = If[termLessQ @@ pair, Reverse[pair], pair];
    vars = AssociationThread[args -> Table[Unique["arg"], Length[args]]];
    patt = big /. KeyValueMap[#1 -> Pattern @@ {#2, Blank[]} &, vars];
    bigTerm = big /. Normal[vars];
    smallTerm = small /. Normal[vars];
    (* The condition is what keeps the rewriting finite even for a law that merely permutes its
       arguments, such as the cyclic property of the trace, which read in either direction would
       otherwise go round for ever. *)
    With[{p = patt, s = smallTerm, b = bigTerm},
        RuleDelayed[p, Condition[s, termLessQ[s, b]]]
    ]
];

reduceEquations[pairs_List, args_List] := Module[{kept = {}, rules = {}, normal},
    normal[t_] := Quiet @ ReplaceRepeated[t, rules, MaxIterations -> 64];
    Do[
        If[ normal[First[pair]] =!= normal[Last[pair]],
            AppendTo[kept, pair];
            AppendTo[rules, equationRule[pair, args]]
        ],
        {pair, SortBy[pairs, {unaryWeight, LeafCount, Identity}]}
    ];
    kept
];


(* ::Subsection:: *)
(*The search*)


(* Whether an operation actually ran. ArrayMultiply returns $Failed on a mismatch, and Normal of
   $Failed is still $Failed, so a failure propagates to the top of the term. *)
evaluatedQ[r_] := FreeQ[r, $Failed] && FreeQ[r, _Missing];

(* With Order -> Automatic, find the smallest order on which every operation runs. Operations are
   shape-specific -- a matrix product will not accept 3-arrays, and a ternary array product will
   not accept matrices -- so this is a search for the shape the operations are defined on rather
   than a guess between equally good answers. *)
inferTestOrder[specs_, dim_] := SelectFirst[
    Range[1, 5],
    Function[ord,
        With[{dims = ConstantArray[dim, ord]},
            AllTrue[specs, Function[s,
                evaluatedQ @ Quiet @ Apply[First[s], Table[RandomInteger[{-3, 3}, dims], Last[s]]]
            ]]
        ]
    ],
    $Failed
];

FindArrayEquations[ops_List, arguments_List, OptionsPattern[]] := Module[{
    specs, unaryOps, naryOps, depth = OptionValue["UnaryDepth"],
    order = OptionValue["Order"], dim = OptionValue["Dimension"],
    trials = OptionValue["Trials"], distinct, dims,
    leftTerms, rightTerms, numericSets, leftValues, rightValues,
    leftOK, rightOK, leftGroups, rightGroups, symbolic, symbolicValue, classify, verified
},
    specs = operationArity /@ ops;
    If[ MemberQ[specs, $Failed],
        Message[FindArrayEquations::arity, ops[[First @ FirstPosition[specs, $Failed]]]];
        Return[$Failed, Module]
    ];
    If[ ! (IntegerQ[depth] && depth >= 0),
        Message[FindArrayEquations::depth, depth]; Return[$Failed, Module]
    ];
    unaryOps = First /@ Select[specs, Last[#] === 1 &];
    naryOps = Select[specs, Last[#] >= 2 &];

    distinct = DeleteDuplicates[arguments];

    (* All nestings of the operations over the arguments. A nesting is only produced when the
       number of arguments fits the arities, so an impossible request comes back empty. *)
    leftTerms = generateTerms[arguments, naryOps, unaryOps, depth];
    If[ leftTerms === {},
        Message[FindArrayEquations::nest, Length[arguments]]; Return[{}, Module]
    ];
    rightTerms = If[ TrueQ @ OptionValue["Permute"],
        DeleteDuplicates @ Catenate[
            generateTerms[#, naryOps, unaryOps, depth] & /@ Permutations[arguments]
        ],
        leftTerms
    ];

    (* A single argument, and whatever the unary operations make of it, on both sides: a law
       relating a term to a bare argument -- the involutivity of the transpose, say -- needs the
       one on the left and the other on the right. *)
    If[ TrueQ @ OptionValue["IncludeArguments"],
        With[{argTerms = Catenate[generateTerms[{#}, naryOps, unaryOps, depth] & /@ distinct]},
            leftTerms = DeleteDuplicates @ Join[leftTerms, argTerms];
            rightTerms = DeleteDuplicates @ Join[rightTerms, argTerms]
        ]
    ];

    If[ order === Automatic,
        order = inferTestOrder[specs, dim];
        If[ order === $Failed,
            Message[FindArrayEquations::shape, dim]; Return[$Failed, Module]
        ]
    ];
    dims = ConstantArray[dim, order];

    (* The cheap screen: evaluate every term on several sets of random integer arrays. Two terms
       that are equal as identities agree on every such set, so nothing is ever wrongly discarded;
       what survives is only a candidate. *)
    numericSets = Table[
        AssociationThread[distinct -> Table[RandomInteger[{-3, 3}, dims], Length[distinct]]],
        trials
    ];
    (* arrayDataOf so that a term is judged by its entries: an operation may hand back a
       array object while a bare argument is a plain list, and those two must still compare
       equal when they hold the same numbers. *)
    leftValues = Table[Quiet @ Map[arrayDataOf, ReplaceAll[leftTerms, s]], {s, numericSets}];
    rightValues = Table[Quiet @ Map[arrayDataOf, ReplaceAll[rightTerms, s]], {s, numericSets}];

    (* A nesting the operations cannot actually evaluate -- a term that feeds a scalar back into a
       product, say, or an operation applied to the wrong shape -- is dropped rather than compared.
       If nothing at all evaluates, the operations do not fit the test arrays, and saying so is
       very different from reporting that no equations were found. *)
    leftOK = Table[AllTrue[Range[trials], evaluatedQ[leftValues[[#, i]]] &], {i, Length[leftTerms]}];
    rightOK = Table[AllTrue[Range[trials], evaluatedQ[rightValues[[#, j]]] &], {j, Length[rightTerms]}];

    (* A bare argument always evaluates, being just the array itself, so the test is whether any
       term that actually applies an operation did. *)
    If[ ! Or @@ Pick[leftOK, ! AtomQ[#] & /@ leftTerms],
        Message[FindArrayEquations::eval, order, dim]; Return[$Failed, Module]
    ];

    (* Two terms are candidates when they agree on every trial, so gathering the terms by the
       sequence of values they took settles all the pairs at once. Comparing every left term with
       every right one would be quadratic, and with unary operations free to repeat there are a
       great many terms. *)
    leftGroups = GroupBy[
        Pick[Range @ Length[leftTerms], leftOK],
        Function[i, Table[leftValues[[t, i]], {t, trials}]]
    ];
    rightGroups = GroupBy[
        Pick[Range @ Length[rightTerms], rightOK],
        Function[j, Table[rightValues[[t, j]], {t, trials}]]
    ];

    (* The expensive check, on the survivors only: evaluate on symbolic arrays and compare the
       entries as polynomials. Expanding them is exact for these and far quicker than FullSimplify,
       and two terms are equal exactly when their expanded entries agree, so grouping by that
       expansion settles a whole class of terms at once. Each term is evaluated once, however many
       pairs it takes part in. *)
    symbolic = AssociationThread[distinct -> (Normal @ GenerateSymbolicArray[#, dims] & /@ distinct)];
    symbolicValue[t_] := symbolicValue[t] = With[{v = arrayDataOf @ Quiet[t /. symbolic]},
        If[evaluatedQ[v], Expand[v], $Failed]
    ];
    classify[terms_, indices_] := GroupBy[
        Select[indices, symbolicValue[terms[[#]]] =!= $Failed &],
        symbolicValue[terms[[#]]] &
    ];

    verified = DeleteDuplicates @ Catenate @ KeyValueMap[
        Function[{signature, is},
            With[{js = Lookup[rightGroups, Key[signature], {}]},
                If[ js === {},
                    {},
                    With[{rg = classify[rightTerms, js]},
                        Catenate @ KeyValueMap[
                            Function[{value, iis},
                                Catenate @ Table[
                                    If[ leftTerms[[i]] =!= rightTerms[[j]],
                                        Sort[{leftTerms[[i]], rightTerms[[j]]}],
                                        Nothing
                                    ],
                                    {i, iis}, {j, Lookup[rg, Key[value], {}]}
                                ]
                            ],
                            classify[leftTerms, is]
                        ]
                    ]
                ]
            ]
        ],
        leftGroups
    ];

    Equal @@@ If[TrueQ @ OptionValue["Reduce"], reduceEquations[verified, distinct], verified]
];

FindArrayEquations[args___] := (Message[FindArrayEquations::arity, {args}]; $Failed);


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];

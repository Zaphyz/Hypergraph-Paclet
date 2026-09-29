(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- rewriting*)


(* ::Text:: *)
(*A rewrite rule is a pair of hypergraphs. Applying it to a hypergraph means finding the ways its*)
(*input sits inside that hypergraph, and for each of them replacing what was matched by the*)
(*output. Each such way is an event, and an event is reported as an association saying not only*)
(*what the new hypergraph is but what was matched, what was created and what was destroyed --*)
(*which is what the causal machinery will later be built out of.*)
(**)
(*The shape of all this follows the reference paclet: HypergraphRule[input, output], applied by*)
(*rule[hg], giving a list of associations keyed "Hypergraph", "MatchVertices", "MatchEdges" and*)
(*so on. The matching underneath is our own, since the reference's rests on a cloud resource*)
(*function and on its own annotation system.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[HypergraphRule, HypergraphRuleQ, HypergraphRuleApply, HypergraphRuleMatches];


(* ::Section:: *)
(*Usage*)


HypergraphRule::usage =
    "HypergraphRule[input, output] represents the rewrite rule taking the hypergraph input to the " <>
    "hypergraph output.\n" <>
    "Either side may be given as anything Hypergraph accepts, and is converted.\n" <>
    "rule[hg] applies the rule to hg, giving one association for every way the input matches.";

HypergraphRuleQ::usage = "HypergraphRuleQ[expr] gives True if expr is a valid HypergraphRule.";

HypergraphRuleApply::usage =
    "HypergraphRuleApply[rule, hg] applies rule to the hypergraph hg, giving a list of events, one " <>
    "for every way the rule's input matches.\n" <>
    "rule[hg] is equivalent.\n" <>
    "Each event is an association with \"Hypergraph\", the result; \"MatchVertices\", " <>
    "\"MatchEdges\" and \"MatchEdgePositions\", what was matched; \"NewVertices\" and " <>
    "\"NewEdges\", what was created; \"DeletedVertices\", what was destroyed; and " <>
    "\"RuleVertexMap\", the correspondence from the rule's vertices to the hypergraph's.";

HypergraphRuleMatches::usage =
    "HypergraphRuleMatches[rule, hg] gives the ways the input of rule sits inside hg, without " <>
    "rewriting anything: one association per match, with \"MatchEdgePositions\" and " <>
    "\"RuleVertexMap\".\n" <>
    "It takes the same options as HypergraphRuleApply.";


(* ::Section:: *)
(*Messages*)


HypergraphRule::spec = "`1` is not a valid rewrite rule specification.";
HypergraphRule::hg = "`1` is not a hypergraph, and cannot be a side of a rule.";
HypergraphRuleApply::rule = "`1` is not a rewrite rule.";
HypergraphRuleApply::hg = "`1` is not a hypergraph.";


Begin["`Private`"];


(* ::Section:: *)
(*The rule object*)


(* Canonical form: both sides are Hypergraph objects. The guard looks only at the pattern
   variables, never at the expression itself, so there is no re-entrancy -- the same treatment
   Vertex, Edge and Hypergraph get. *)
canonicalRuleArgsQ[lhs_, rhs_] := HypergraphQ[lhs] && HypergraphQ[rhs];
canonicalRuleArgsQ[___] := False;

HypergraphRule[args___] /; ! canonicalRuleArgsQ[args] := makeHypergraphRule[args];

makeHypergraphRule[lhs_, rhs_] := Module[{l = toHypergraph[lhs], r = toHypergraph[rhs]},
    If[l === $Failed || r === $Failed, Return[$Failed, Module]];
    HypergraphRule[l, r]
];
makeHypergraphRule[args___] := (Message[HypergraphRule::spec, HoldForm[HypergraphRule[args]]]; $Failed);

toHypergraph[hg_Hypergraph] /; HypergraphQ[hg] := hg;
toHypergraph[spec_] := With[{hg = Quiet @ Hypergraph[spec]},
    If[HypergraphQ[hg], hg, Message[HypergraphRule::hg, HoldForm[spec]]; $Failed]
];

HypergraphRuleQ[r_HypergraphRule] := canonicalRuleArgsQ @@ r;
HypergraphRuleQ[_] := False;

(r_HypergraphRule)[prop_String] /; HypergraphRuleQ[r] := ruleProp[r, prop];

ruleProp[_, "Properties"] := {
    "Input", "Output", "SharedVertices", "NewVertices", "DeletedVertices", "Signature"
};
ruleProp[r_, "Input"] := First[r];
ruleProp[r_, "Output"] := Last[r];
(* The vertices the rule keeps, creates and destroys, read off the two sides by identifier. *)
ruleProp[r_, "SharedVertices"] := Intersection[ruleVertexIDs @ First[r], ruleVertexIDs @ Last[r]];
ruleProp[r_, "NewVertices"] := Complement[ruleVertexIDs @ Last[r], ruleVertexIDs @ First[r]];
ruleProp[r_, "DeletedVertices"] := Complement[ruleVertexIDs @ First[r], ruleVertexIDs @ Last[r]];
ruleProp[r_, "Signature"] := Thread[{EdgeCount /@ {First[r], Last[r]}, VertexCount /@ {First[r], Last[r]}}];
ruleProp[_, prop_] := Missing["UnknownProperty", prop];

ruleVertexIDs[hg_] := VertexID /@ VertexList[hg];

HypergraphRule /: MakeBoxes[r_HypergraphRule /; HypergraphRuleQ[r], form : StandardForm | TraditionalForm] :=
    BoxForm`ArrangeSummaryBox[
        "HypergraphRule", r, $HypergraphRuleIcon,
        {
            {BoxForm`SummaryItem[{"Input: ", Row[{VertexCount[First[r]], " vertices, ",
                                                  EdgeCount[First[r]], " edges"}]}]},
            {BoxForm`SummaryItem[{"Output: ", Row[{VertexCount[Last[r]], " vertices, ",
                                                   EdgeCount[Last[r]], " edges"}]}]}
        },
        {
            {BoxForm`SummaryItem[{"Shared: ", r["SharedVertices"]}]},
            {BoxForm`SummaryItem[{"Created: ", r["NewVertices"]}]},
            {BoxForm`SummaryItem[{"Destroyed: ", r["DeletedVertices"]}]}
        },
        form, "Interpretable" -> Automatic
    ];

$HypergraphRuleIcon := $HypergraphRuleIcon = Graphics[
    {
        Hue[0.63, 0.7, 0.5],
        Arrowheads[0.35], Arrow[{{-0.8, 0}, {0.8, 0}}],
        AbsoluteThickness[1.4]
    },
    ImageSize -> Dynamic[{Automatic, 2.6 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}],
    PlotRangePadding -> 0.4
];


(* ::Section:: *)
(*Matching*)


Options[HypergraphRuleMatches] = {
    "Labeled" -> True,
    "LabelPatterns" -> True,
    "DistinctVertices" -> True
};

Options[HypergraphRuleApply] = Options[HypergraphRuleMatches];

(* The orderings of an edge's vertices that its symmetry type says are the same edge. A directed
   edge has only the one; a cyclic edge its rotations; an unordered edge every permutation. These
   are the same three groups the canonical vertex order of an Edge is taken over, seen from the
   other side: there we picked a representative, here we have to try them all. *)
symmetryPermutations["Directed", k_Integer] := {Range[k]};
symmetryPermutations[_, k_Integer] /; k <= 1 := {Range[k]};
symmetryPermutations["Cyclic", k_Integer] := NestList[RotateLeft, Range[k], k - 1];
symmetryPermutations["Unordered", k_Integer] := Permutations[Range[k]];
symmetryPermutations[___] := {};


(* A match is built up in two parts: which vertex of the hypergraph each vertex of the rule stands
   on, and what each label variable of the rule stands for. *)
$emptyMatchState = <|"Vertices" -> <||>, "Labels" -> <||>|>;

bindIn[state_, part_String, key_, value_] :=
    Append[state, part -> Append[state[part], key -> value]];

(* A label on the rule is read three ways.

   A bare symbol is a variable: it matches whatever label it is laid against and remembers it, and
   every later occurrence of that symbol must find the same thing. This is what lets a rule say
   "an edge labelled x followed by an edge labelled y" without knowing what x and y will be, and
   then name the result in terms of them -- which is how the composition rules in the reference
   notebooks are written, and the reason labels are worth this trouble at all.

   No label at all is a wildcard, matching anything and remembering nothing.

   Anything else -- a string, a number, a composite expression -- is a literal, and matches only
   itself. Verbatim[x] forces a symbol to be read that way, for a rule that wants to match the
   label x rather than bind it. *)
labelVariableQ[lab_] := Head[lab] === Symbol && lab =!= None;

bindLabel[$Failed, _, _] := $Failed;
bindLabel[state_, patt_, host_] := Which[
    ! $labeledQ, state,
    patt === None, state,
    MatchQ[patt, Verbatim[Verbatim][_]], If[First[patt] === host, state, $Failed],
    $labelPatternsQ && labelVariableQ[patt],
        With[{seen = Lookup[state["Labels"], Key[patt], $unbound]},
            Which[
                seen === $unbound, bindIn[state, "Labels", patt, host],
                seen === host, state,
                True, $Failed
            ]
        ],
    patt === host, state,
    True, $Failed
];

(* One vertex of the rule onto one vertex of the hypergraph: the correspondence must agree with
   whatever was settled earlier, the labels must match or bind, and with "DistinctVertices" no two
   vertices of the rule may land on the same one. *)
bindVertex[$Failed, _, _] := $Failed;
bindVertex[state_, pv_, hv_] := Module[{pid = VertexID[pv], hid = VertexID[hv], seen, next},
    seen = Lookup[state["Vertices"], Key[pid], $unbound];
    next = Which[
        seen === $unbound,
            If[ $distinctQ && MemberQ[Values[state["Vertices"]], hid],
                $Failed,
                bindIn[state, "Vertices", pid, hid]
            ],
        seen === hid, state,
        True, $Failed
    ];
    bindLabel[next, VertexLabel[pv], VertexLabel[hv]]
];

(* Whether a pattern edge could sit on a host edge at all, before any vertices are considered.
   The label is not checked here: a label variable has to be bound rather than compared, which
   only makes sense once the rest of the match is being built. *)
edgeCompatibleQ[pe_, he_] :=
    Length[EdgeVertices[pe]] === Length[EdgeVertices[he]] &&
    EdgeSymmetry[pe] === EdgeSymmetry[he];

(* Extend a match by laying one pattern edge over one host edge. Every ordering the symmetry
   allows is tried, so the result is a list of states rather than one. *)
extendMatch[state_, pe_, he_] := Module[{pvs, hvs, perms, withLabel},
    withLabel = bindLabel[state, EdgeLabel[pe], EdgeLabel[he]];
    If[withLabel === $Failed, Return[{}, Module]];
    pvs = EdgeVertices[pe];
    hvs = EdgeVertices[he];
    perms = symmetryPermutations[EdgeSymmetry[he], Length[hvs]];
    DeleteCases[
        Function[perm,
            Fold[bindVertex[#1, #2[[1]], #2[[2]]] &, withLabel, Thread[{pvs, hvs[[perm]]}]]
        ] /@ perms,
        $Failed
    ]
];

(* Lay the pattern edges over the host edges one at a time, keeping every consistent way of doing
   it. A host edge may be used once: a rule matching two edges wants two of them. *)
matchEdges[{}, _, used_, state_] := {{Reverse[used], state}};
matchEdges[{pe_, rest___}, hostEdges_, used_, state_] := Catenate @ Table[
    If[ MemberQ[used, p] || ! edgeCompatibleQ[pe, hostEdges[[p]]],
        {},
        Catenate[matchEdges[{rest}, hostEdges, Prepend[used, p], #] & /@ extendMatch[state, pe, hostEdges[[p]]]]
    ],
    {p, Length[hostEdges]}
];

(* A vertex of the rule lying in no edge constrains nothing about which edges are used, so it may
   sit on any vertex not already taken -- but its label still has to match or bind, which is what
   makes a rule whose whole input is one labelled vertex mean anything. *)
bindFreeVertices[state_, {}, _] := {state};
bindFreeVertices[state_, free_List, hostVertices_] := Catenate @ Map[
    Function[choice,
        With[{result = Fold[bindVertex[#1, #2[[1]], #2[[2]]] &, state, Thread[{free, choice}]]},
            If[result === $Failed, {}, {result}]
        ]
    ],
    Permutations[
        Select[hostVertices, ! MemberQ[Values[state["Vertices"]], VertexID[#]] &],
        {Length[free]}
    ]
];

HypergraphRuleMatches[r_HypergraphRule ? HypergraphRuleQ, hg_Hypergraph ? HypergraphQ,
                      opts : OptionsPattern[]] := Block[{
    $labeledQ = TrueQ @ OptionValue["Labeled"],
    $labelPatternsQ = TrueQ @ OptionValue["LabelPatterns"],
    $distinctQ = TrueQ @ OptionValue["DistinctVertices"],
    lhs = r["Input"], patternEdges, hostEdges, freeVertices, raw
},
    patternEdges = EdgeList[lhs];
    hostEdges = EdgeList[hg];
    freeVertices = Select[VertexList[lhs],
        ! MemberQ[VertexID /@ Catenate[EdgeVertices /@ patternEdges], VertexID[#]] &];

    raw = matchEdges[patternEdges, hostEdges, {}, $emptyMatchState];

    DeleteDuplicates @ Catenate @ Map[
        Function[pair,
            Function[state,
                <|
                    "MatchEdgePositions" -> First[pair],
                    "RuleVertexMap" -> state["Vertices"],
                    "LabelBindings" -> state["Labels"]
                |>
            ] /@ bindFreeVertices[Last[pair], freeVertices, VertexList[hg]]
        ],
        raw
    ]
];

HypergraphRuleMatches[r_, hg_, ___] := (
    If[! HypergraphRuleQ[r], Message[HypergraphRuleApply::rule, HoldForm[r]],
        Message[HypergraphRuleApply::hg, HoldForm[hg]]];
    $Failed
);


(* ::Section:: *)
(*Applying*)


(* The label a vertex of the hypergraph carries, for an output that names the vertex but gives it
   no label of its own. *)
hostLabelOf[hostVertices_, id_] :=
    With[{v = SelectFirst[hostVertices, VertexID[#] === id &, None]},
        If[v === None, None, VertexLabel[v]]
    ];

(* Names for the vertices the output creates. Fresh symbols would do, and are what the reference
   uses, but names chosen to avoid what is already there are reproducible from one session to the
   next, which the tests and the comparison of states both want. *)
newVertexNames[n_Integer, avoid_List] := Module[{k = 0, names = {}, candidate},
    While[Length[names] < n,
        k++;
        candidate = \[FormalV][k];
        If[! MemberQ[avoid, candidate] && ! MemberQ[names, candidate], AppendTo[names, candidate]]
    ];
    names
];

(* One event: what the hypergraph becomes under one match, and an account of what changed.

   The matched edges go, the output's edges arrive in their place, and the vertices follow: those
   the output shares with the input keep the vertices they were matched to, those it introduces
   get new ones, and those only the input had are destroyed. A destroyed vertex is dropped from
   the vertex list only when nothing left in the hypergraph uses it -- a rule rewrites what it
   matched, and reaching into edges it did not match to remove a vertex from them would not be
   local.

   The output's labels are expressions in whatever its input's label variables were bound to, so
   they are written out with those bindings substituted. A rule may therefore name what it builds
   after what it found: this is what turns a pair of composable edges labelled x and y into one
   labelled for their composite. *)
applyMatch[r_, hg_, match_] := Module[{
    lhs = r["Input"], rhs = r["Output"],
    pos = match["MatchEdgePositions"], vmap = match["RuleVertexMap"],
    labelRules = Normal[match["LabelBindings"]],
    hostEdges = EdgeList[hg], hostVertices = VertexList[hg],
    newIDs, deletedIDs, newNames, fullMap,
    matchEdges, matchVertexIDs, keptEdges, newEdges, outputVertices,
    deletedHostIDs, keptVertices, outputIDs, result
},
    newIDs = r["NewVertices"];
    deletedIDs = r["DeletedVertices"];

    newNames = newVertexNames[Length[newIDs],
        Join[VertexID /@ hostVertices, VertexID /@ VertexList[rhs]]];
    fullMap = Join[vmap, AssociationThread[newIDs -> newNames]];

    matchEdges = hostEdges[[pos]];
    matchVertexIDs = Lookup[vmap, Key[#]] & /@ (VertexID /@ VertexList[lhs]);

    keptEdges = Delete[hostEdges, List /@ pos];
    newEdges = Function[e,
        Edge[
            Replace[VertexID /@ EdgeVertices[e], id_ :> Lookup[fullMap, Key[id], id], 1],
            EdgeSymmetry[e],
            EdgeLabel[e] /. labelRules
        ]
    ] /@ EdgeList[rhs];

    (* The vertices the output names, carrying the labels it gives them with the bindings put in.
       Where the output labels a vertex, that label is the one that stands -- relabelling is
       something a rule is entitled to do. Where it leaves one unlabelled, whatever the hypergraph
       had is kept. The label is settled here rather than left to the constructor to merge, which
       would see two labels for one vertex and warn about a disagreement that is not one. *)
    outputVertices = Function[v,
        With[{id = Lookup[fullMap, Key[VertexID[v]], VertexID[v]],
              lab = VertexLabel[v] /. labelRules},
            Vertex[id, If[lab === None, hostLabelOf[hostVertices, id], lab]]
        ]
    ] /@ VertexList[rhs];
    outputIDs = VertexID /@ outputVertices;

    deletedHostIDs = Lookup[vmap, Key[#]] & /@ deletedIDs;
    With[{surviving = VertexID /@ Catenate[EdgeVertices /@ Join[keptEdges, newEdges]]},
        keptVertices = DeleteCases[hostVertices,
            v_ /; (MemberQ[deletedHostIDs, VertexID[v]] && ! MemberQ[surviving, VertexID[v]]) ||
                  MemberQ[outputIDs, VertexID[v]]]
    ];

    result = Hypergraph[Join[outputVertices, keptVertices], Join[keptEdges, newEdges]];

    <|
        "Hypergraph" -> result,
        "MatchVertices" -> matchVertexIDs,
        "MatchEdges" -> matchEdges,
        "MatchEdgePositions" -> pos,
        "NewVertices" -> newNames,
        "NewEdges" -> newEdges,
        "DeletedVertices" -> Complement[deletedHostIDs,
            VertexID /@ Catenate[EdgeVertices /@ Join[keptEdges, newEdges]]],
        "RuleVertexMap" -> fullMap,
        "LabelBindings" -> match["LabelBindings"],
        "Rule" -> r
    |>
];

HypergraphRuleApply[r_HypergraphRule ? HypergraphRuleQ, hg_Hypergraph ? HypergraphQ,
                    opts : OptionsPattern[]] :=
    applyMatch[r, hg, #] & /@ HypergraphRuleMatches[r, hg, opts];

HypergraphRuleApply[r_, hg_, ___] := (
    If[! HypergraphRuleQ[r], Message[HypergraphRuleApply::rule, HoldForm[r]],
        Message[HypergraphRuleApply::hg, HoldForm[hg]]];
    $Failed
);

(* rule[hg], the reference's way of writing it. *)
(r_HypergraphRule)[hg_Hypergraph, opts : OptionsPattern[HypergraphRuleApply]] /; HypergraphRuleQ[r] :=
    HypergraphRuleApply[r, hg, opts];


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];

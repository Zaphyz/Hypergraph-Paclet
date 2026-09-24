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

(* A label on the pattern is a constraint; no label is a wildcard. So an unlabelled rule matches
   a labelled hypergraph exactly as it matches the skeleton, and labels only ever narrow what a
   rule applies to. With "Labeled" -> False they are ignored altogether. *)
labelMatchQ[_, _, False] := True;
labelMatchQ[None, _, True] := True;
labelMatchQ[patt_, host_, True] := patt === host;

(* Whether a pattern edge could sit on a host edge at all, before any vertices are considered. *)
edgeCompatibleQ[pe_, he_, labeledQ_] :=
    Length[EdgeVertices[pe]] === Length[EdgeVertices[he]] &&
    EdgeSymmetry[pe] === EdgeSymmetry[he] &&
    labelMatchQ[EdgeLabel[pe], EdgeLabel[he], labeledQ];

(* Extend a partial vertex binding by laying one pattern edge over one host edge. Every ordering
   the symmetry allows is tried, so the result is a list of bindings rather than one. *)
extendBinding[binding_, pe_, he_, labeledQ_, distinctQ_] := Module[{pvs, hvs, perms},
    pvs = EdgeVertices[pe];
    hvs = EdgeVertices[he];
    perms = symmetryPermutations[EdgeSymmetry[he], Length[hvs]];
    DeleteCases[
        Function[perm, Fold[bindVertex[#1, #2[[1]], #2[[2]], labeledQ, distinctQ] &,
                            binding, Thread[{pvs, hvs[[perm]]}]]] /@ perms,
        $Failed
    ]
];

(* One vertex of the pattern onto one vertex of the hypergraph. A binding already made must agree,
   and with "DistinctVertices" no two pattern vertices may land on the same one. *)
bindVertex[$Failed, ___] := $Failed;
bindVertex[binding_, pv_, hv_, labeledQ_, distinctQ_] := With[{
    pid = VertexID[pv], hid = VertexID[hv], seen = Lookup[binding, Key[VertexID[pv]], None]
},
    Which[
        ! labelMatchQ[VertexLabel[pv], VertexLabel[hv], labeledQ], $Failed,
        seen =!= None, If[seen === hid, binding, $Failed],
        distinctQ && MemberQ[Values[binding], hid], $Failed,
        True, Append[binding, pid -> hid]
    ]
];

(* Lay the pattern edges over the host edges one at a time, keeping every consistent way of doing
   it. A host edge may be used once: a rule matching two edges wants two of them. *)
matchEdges[{}, _, used_, binding_] := {{Reverse[used], binding}};
matchEdges[{pe_, rest___}, hostEdges_, used_, binding_] := Catenate @ Table[
    If[ MemberQ[used, p] || ! edgeCompatibleQ[pe, hostEdges[[p]], $labeledQ],
        {},
        Catenate[
            matchEdges[{rest}, hostEdges, Prepend[used, p], #] & /@
                extendBinding[binding, pe, hostEdges[[p]], $labeledQ, $distinctQ]
        ]
    ],
    {p, Length[hostEdges]}
];

(* A vertex of the input that lies in no edge constrains nothing, so it may sit on any vertex not
   already taken. Rules usually have none, and this is what happens when they do. *)
bindFreeVertices[binding_, {}, _] := {binding};
bindFreeVertices[binding_, free_List, hostIDs_] := With[{
    taken = Values[binding]
},
    Function[choice, Join[binding, AssociationThread[free -> choice]]] /@
        Permutations[Complement[hostIDs, taken], {Length[free]}]
];

HypergraphRuleMatches[r_HypergraphRule ? HypergraphRuleQ, hg_Hypergraph ? HypergraphQ,
                      opts : OptionsPattern[]] := Block[{
    $labeledQ = TrueQ @ OptionValue["Labeled"],
    $distinctQ = TrueQ @ OptionValue["DistinctVertices"],
    lhs = r["Input"], patternEdges, hostEdges, hostIDs, freeIDs, raw
},
    patternEdges = EdgeList[lhs];
    hostEdges = EdgeList[hg];
    hostIDs = VertexID /@ VertexList[hg];
    freeIDs = Complement[VertexID /@ VertexList[lhs],
                         VertexID /@ Catenate[EdgeVertices /@ patternEdges]];

    raw = matchEdges[patternEdges, hostEdges, {}, <||>];

    DeleteDuplicates @ Catenate @ Map[
        Function[pair,
            Function[binding, <|"MatchEdgePositions" -> First[pair], "RuleVertexMap" -> binding|>] /@
                bindFreeVertices[Last[pair], freeIDs, hostIDs]
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
   local. *)
applyMatch[r_, hg_, match_, labeledQ_] := Module[{
    lhs = r["Input"], rhs = r["Output"],
    pos = match["MatchEdgePositions"], vmap = match["RuleVertexMap"],
    hostEdges = EdgeList[hg], hostVertices = VertexList[hg],
    sharedIDs, newIDs, deletedIDs, newNames, fullMap,
    matchEdges, matchVertexIDs, keptEdges, newEdges, deletedHostIDs, keptVertices, result
},
    sharedIDs = r["SharedVertices"];
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
            EdgeLabel[e]
        ]
    ] /@ EdgeList[rhs];

    deletedHostIDs = Lookup[vmap, Key[#]] & /@ deletedIDs;
    (* keep a destroyed vertex if anything still standing uses it *)
    With[{surviving = VertexID /@ Catenate[EdgeVertices /@ Join[keptEdges, newEdges]]},
        keptVertices = DeleteCases[hostVertices,
            v_ /; MemberQ[deletedHostIDs, VertexID[v]] && ! MemberQ[surviving, VertexID[v]]]
    ];

    (* The host's vertices go in first, so that where the output gives a label to a vertex that
       already had one, the one already there wins. *)
    result = Hypergraph[keptVertices, Join[keptEdges, newEdges]];

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
        "Rule" -> r
    |>
];

HypergraphRuleApply[r_HypergraphRule ? HypergraphRuleQ, hg_Hypergraph ? HypergraphQ,
                    opts : OptionsPattern[]] :=
    applyMatch[r, hg, #, TrueQ @ OptionValue["Labeled"]] & /@
        HypergraphRuleMatches[r, hg, opts];

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

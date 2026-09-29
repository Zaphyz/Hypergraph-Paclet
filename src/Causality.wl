(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- rewriting systems and causality*)


(* ::Text:: *)
(*A rewriting system is a set of rules together with a state to start from. Evolving it means*)
(*applying every rule in every way it fits, to every state reached so far, for as many*)
(*generations as asked. What comes out is a tree of state occurrences joined by events, and the*)
(*causal graph is the record of which event depended on which.*)
(**)
(*The design point here is that "depended on" is an option rather than a fixed meaning. Two*)
(*events are causally related when the elements they touch overlap, and "element" is what the*)
(*option chooses: anything at all, vertex or edge, which is the weakest reading and the one the*)
(*reference paclets take; or whole edges only, which is the strictest. See*)
(*$CausalDependenceTypes below.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[HypergraphRewritingSystem, HypergraphRewritingSystemQ, HypergraphCausalGraph,
          $CausalDependenceTypes];


(* ::Section:: *)
(*Usage*)


HypergraphRewritingSystem::usage =
    "HypergraphRewritingSystem[rules, init] represents the rewriting system with the given rules " <>
    "started from the hypergraph init.\n" <>
    "rules may be one HypergraphRule or a list of them; init may be anything Hypergraph accepts.\n" <>
    "sys[prop, n] gives a property of the evolution carried n generations, and sys[\"Properties\"] " <>
    "lists the properties.";

HypergraphRewritingSystemQ::usage =
    "HypergraphRewritingSystemQ[expr] gives True if expr is a valid HypergraphRewritingSystem.";

HypergraphCausalGraph::usage =
    "HypergraphCausalGraph[sys, n] gives the causal graph of the rewriting system sys evolved n " <>
    "generations: one vertex per event, and an edge from an event to a later one that depends " <>
    "on it.\n" <>
    "HypergraphCausalGraph[rules, init, n] builds the system first.\n" <>
    "The option \"CausalDependence\" chooses what counts as a dependence; " <>
    "$CausalDependenceTypes lists the values it takes.";

$CausalDependenceTypes::usage =
    "$CausalDependenceTypes is the list of values the \"CausalDependence\" option takes.\n" <>
    "\"Elements\", the default, makes two events dependent when what they touch overlaps in " <>
    "anything at all, vertex or edge; it is the weakest reading, and the one the reference " <>
    "paclets take.\n" <>
    "\"Edges\" counts only whole shared edges.\n" <>
    "\"Vertices\" counts only shared vertices, and is there for the contrast: \"Elements\" is " <>
    "exactly \"Edges\" or \"Vertices\".";


(* ::Section:: *)
(*Messages*)


HypergraphRewritingSystem::spec = "`1` is not a valid rewriting system specification.";
HypergraphRewritingSystem::rules = "`1` is not a rewrite rule or a list of them.";
HypergraphRewritingSystem::hg = "`1` is not a hypergraph, and cannot be an initial state.";
HypergraphRewritingSystem::gen = "`1` is not a non-negative integer number of generations.";
HypergraphRewritingSystem::dep =
    "`1` is not a causal dependence type; it should be one of `2`. Using \"Elements\".";
HypergraphRewritingSystem::trunc =
    "The evolution reached the limit of `1` states during generation `2` and was stopped there. " <>
    "Raise \"MaxStates\" to go further.";

HypergraphCausalGraph::sys = "`1` is not a rewriting system.";


Begin["`Private`"];


(* ::Section:: *)
(*The system object*)


(* Canonical form: a list of rules and a Hypergraph. As everywhere else in this package the guard
   reads only the pattern variables, never the expression itself, so there is no re-entrancy. *)
canonicalSystemArgsQ[rules_List, init_] := AllTrue[rules, HypergraphRuleQ] && HypergraphQ[init];
canonicalSystemArgsQ[___] := False;

HypergraphRewritingSystem[args___] /; ! canonicalSystemArgsQ[args] := makeSystem[args];

makeSystem[rules_, init_] := Module[{rs = toRuleList[rules], hg = toInitial[init]},
    If[rs === $Failed || hg === $Failed, Return[$Failed, Module]];
    HypergraphRewritingSystem[rs, hg]
];
makeSystem[args___] :=
    (Message[HypergraphRewritingSystem::spec, HoldForm[HypergraphRewritingSystem[args]]]; $Failed);

toRuleList[r_HypergraphRule] /; HypergraphRuleQ[r] := {r};
toRuleList[rs_List] := With[{converted = toSystemRule /@ rs},
    If[MemberQ[converted, $Failed], $Failed, converted]
];
toRuleList[spec : (_Rule | _RuleDelayed)] := toRuleList[{spec}];
toRuleList[spec_] := (Message[HypergraphRewritingSystem::rules, HoldForm[spec]]; $Failed);

toSystemRule[r_HypergraphRule] /; HypergraphRuleQ[r] := r;
toSystemRule[(Rule | RuleDelayed)[lhs_, rhs_]] := With[{r = Quiet @ HypergraphRule[lhs, rhs]},
    If[ HypergraphRuleQ[r], r,
        Message[HypergraphRewritingSystem::rules, HoldForm[lhs -> rhs]]; $Failed]
];
toSystemRule[spec_] := (Message[HypergraphRewritingSystem::rules, HoldForm[spec]]; $Failed);

toInitial[hg_Hypergraph] /; HypergraphQ[hg] := hg;
toInitial[spec_] := With[{hg = Quiet @ Hypergraph[spec]},
    If[HypergraphQ[hg], hg, Message[HypergraphRewritingSystem::hg, HoldForm[spec]]; $Failed]
];

HypergraphRewritingSystemQ[s_HypergraphRewritingSystem] := canonicalSystemArgsQ @@ s;
HypergraphRewritingSystemQ[_] := False;


(* ::Section:: *)
(*Options*)


$CausalDependenceTypes = {"Elements", "Edges", "Vertices"};

Options[HypergraphCausalGraph] = {
    "CausalDependence" -> "Elements",
    "TransitiveReduction" -> True,
    "CanonicalizeStates" -> True,
    "MaxStates" -> 2048,
    "Labeled" -> True,
    "LabelPatterns" -> True,
    "DistinctVertices" -> True
};

dependenceType[opts___] := With[{t = OptionValue[HypergraphCausalGraph, {opts}, "CausalDependence"]},
    If[ MemberQ[$CausalDependenceTypes, t],
        t,
        Message[HypergraphRewritingSystem::dep, t, $CausalDependenceTypes]; "Elements"
    ]
];


(* ::Section:: *)
(*Evolution*)


(* The evolution is a tree of state occurrences, not a graph of distinct states: a state reached
   two ways appears twice. That is what the causal reading needs, since which elements a state is
   made of depends on how it was reached, and two events can only be causally related when one
   lies on a path through the other. Merging equal states is something "StatesGraph" does on top
   of this, for display.

   Elements are tracked by token, not by value. An edge is an element of a multiset, so two edges
   that look the same are still two elements; and a vertex identifier freed by one event may be
   handed out again by a later one. A token is a counter, minted when an element is created and
   carried unchanged for as long as that element survives. *)

evolve[sys_, n_Integer, opts___] := Module[{
    rules = First[sys], init = Last[sys],
    maxStates = OptionValue[HypergraphCausalGraph, {opts}, "MaxStates"],
    applyOpts = FilterRules[{opts}, Options[HypergraphRuleApply]],
    edgeToken = 0, vertexToken = 0,
    mintEdges, mintVertices,
    states, events, frontier, next, truncated = False
},
    mintEdges[k_] := Table[++edgeToken, k];
    mintVertices[ids_] := AssociationMap[(++vertexToken) &, ids];

    states = {<|
        "Hypergraph" -> init,
        "EdgeTokens" -> mintEdges[EdgeCount[init]],
        "VertexTokens" -> mintVertices[VertexID /@ VertexList[init]],
        "Generation" -> 0,
        "Parent" -> None,
        "InEvent" -> None
    |>};
    events = {};
    frontier = {1};

    Do[
        next = {};
        Do[
            Do[
                Do[
                    If[ Length[states] >= maxStates, truncated = True; Break[]];
                    With[{step = extend[states[[si]], raw, ri, g, Length[events] + 1, si,
                                        mintEdges, mintVertices]},
                        AppendTo[states, First[step]];
                        AppendTo[events, Append[Last[step], "OutputState" -> Length[states]]];
                        AppendTo[next, Length[states]]
                    ],
                    {raw, HypergraphRuleApply[rules[[ri]], states[[si, "Hypergraph"]], applyOpts]}
                ];
                If[truncated, Break[]],
                {ri, Length[rules]}
            ];
            If[truncated, Break[]],
            {si, frontier}
        ];
        frontier = next;
        If[ truncated,
            Message[HypergraphRewritingSystem::trunc, maxStates, g];
            Break[]
        ],
        {g, n}
    ];

    <|"States" -> states, "Events" -> events, "Truncated" -> truncated|>
];

(* One event: the new state occurrence, and the record of what the event touched.

   Edge tokens follow "EdgeOrigins", which says for each edge of the result which host edge it
   carries over from, or None where the rule made it. Vertex tokens follow identifiers, which a
   rewrite never renames: the ones the event created are minted, the ones it destroyed are
   dropped, and the rest carry over. *)
extend[state_, ev_, ruleIndex_, generation_, eventIndex_, inputState_, mintEdges_, mintVertices_] :=
Module[{
    oldEdgeTokens = state["EdgeTokens"], oldVertexTokens = state["VertexTokens"],
    origins = ev["EdgeOrigins"], createdEdges, newEdgeTokens, createdVertices, taken = 0
},
    createdEdges = mintEdges[Count[origins, None]];
    newEdgeTokens = If[# === None, createdEdges[[++taken]], oldEdgeTokens[[#]]] & /@ origins;
    createdVertices = mintVertices[ev["NewVertices"]];

    {
        <|
            "Hypergraph" -> ev["Hypergraph"],
            "EdgeTokens" -> newEdgeTokens,
            "VertexTokens" -> Join[KeyDrop[oldVertexTokens, ev["DeletedVertices"]], createdVertices],
            "Generation" -> generation,
            "Parent" -> inputState,
            "InEvent" -> eventIndex
        |>,
        <|
            "Rule" -> ev["Rule"],
            "RuleIndex" -> ruleIndex,
            "Generation" -> generation,
            "InputState" -> inputState,
            "ConsumedEdges" -> oldEdgeTokens[[ev["MatchEdgePositions"]]],
            "CreatedEdges" -> createdEdges,
            "MatchedVertices" -> DeleteMissing @ Lookup[oldVertexTokens, ev["MatchVertices"]],
            "CreatedVertices" -> Values[createdVertices],
            "DestroyedVertices" -> DeleteMissing @ Lookup[oldVertexTokens, ev["DeletedVertices"]],
            "Data" -> ev
        |>
    }
];


(* ::Section:: *)
(*Causal dependence*)


(* What an event touches, in the currency the option asks for. Edge tokens are tagged apart from
   vertex tokens so that the two counters cannot collide.

   Touching covers what was taken and what was left behind: an event's match together with what
   it created. Along a path that comes to the same thing as the usual created-then-consumed
   reading, since an edge an event consumed is gone and can only be met again by having been
   created -- but writing it as an overlap of what two events touch makes the three settings one
   definition, and makes "Edges" and "Vertices" visibly the two halves of "Elements". *)
touchedEdges[e_] := Union[e["ConsumedEdges"], e["CreatedEdges"]];
touchedVertices[e_] := Union[e["MatchedVertices"], e["CreatedVertices"], e["DestroyedVertices"]];

touched[e_, "Edges"] := {"E", #} & /@ touchedEdges[e];
touched[e_, "Vertices"] := {"V", #} & /@ touchedVertices[e];
touched[e_, "Elements"] := Join[touched[e, "Edges"], touched[e, "Vertices"]];

(* The state occurrences on the path from the root down to a given one, the occurrence itself
   included. *)
ancestry[states_, si_Integer] := Module[{path = {}, k = si},
    While[k =!= None, AppendTo[path, k]; k = states[[k, "Parent"]]];
    path
];

(* Whether one event is causally after another is only a question worth asking when the second
   acts on a state the first produced, or on a descendant of it. In a multiway evolution two
   events on different branches never meet, whatever they have in common. *)
causalPairs[ev_, states_, type_] := Module[{lines, tok, out = {}},
    lines = ancestry[states, #["InputState"]] & /@ ev;
    tok = touched[#, type] & /@ ev;
    Do[
        If[ MemberQ[lines[[j]], ev[[i, "OutputState"]]],
            With[{shared = Intersection[tok[[i]], tok[[j]]]},
                If[ shared =!= {},
                    AppendTo[out, <|
                        "From" -> i,
                        "To" -> j,
                        "SharedEdges" -> Last /@ Cases[shared, {"E", _}],
                        "SharedVertices" -> Last /@ Cases[shared, {"V", _}]
                    |>]
                ]
            ]
        ],
        {i, Length[ev]}, {j, Length[ev]}
    ];
    out
];


(* ::Section:: *)
(*Properties*)


(s_HypergraphRewritingSystem)[args___] /; HypergraphRewritingSystemQ[s] := systemProp[s, args];

systemProp[_, "Properties"] := {
    "Rules", "RuleCount", "InitialState",
    "States", "StateList", "StateCount",
    "EventList", "EventCount",
    "EvolutionGraph", "StatesGraph", "BranchialGraph",
    "CausalGraph", "CausalEdges", "EvolutionCausalGraph"
};
systemProp[s_, "Rules"] := First[s];
systemProp[s_, "RuleCount"] := Length @ First[s];
systemProp[s_, "InitialState"] := Last[s];

systemProp[s_, prop_String, opts : OptionsPattern[HypergraphCausalGraph]] :=
    systemProp[s, prop, 1, opts];

systemProp[s_, prop_String, n_, opts : OptionsPattern[HypergraphCausalGraph]] :=
    If[ ! (IntegerQ[n] && n >= 0),
        Message[HypergraphRewritingSystem::gen, n]; $Failed,
        evolutionProp[evolve[s, n, opts], prop, n, opts]
    ];

systemProp[_, prop_, ___] := Missing["UnknownProperty", prop];

evolutionProp[e_, "StateList", ___] := #["Hypergraph"] & /@ e["States"];
evolutionProp[e_, "StateCount", ___] := Length @ e["States"];
evolutionProp[e_, "States", ___] :=
    Values @ GroupBy[e["States"], #["Generation"] &, Map[#["Hypergraph"] &]];
evolutionProp[e_, "EventList", ___] := e["Events"];
evolutionProp[e_, "EventCount", ___] := Length @ e["Events"];

evolutionProp[e_, "CausalEdges", n_, opts___] :=
    causalPairs[e["Events"], e["States"], dependenceType[opts]];


(* ::Subsection:: *)
(*Graphs*)


$stateStyle = Directive[Hue[0.58, 0.55, 0.92], EdgeForm[Hue[0.58, 0.8, 0.6]]];
$eventStyle = Directive[Hue[0.11, 0.85, 0.9], EdgeForm[Hue[0.09, 1, 0.65]]];
$causalStyle = Hue[0.02, 0.75, 0.8];

evolutionProp[e_, "CausalGraph", n_, opts___] := Module[{events = e["Events"], edges, g},
    edges = DeleteDuplicates[
        DirectedEdge[#["From"], #["To"]] & /@ evolutionProp[e, "CausalEdges", n, opts]];
    g = Graph[
        Range[Length[events]], edges,
        VertexLabels -> Table[k -> Placed[eventTooltip[events, k], Tooltip], {k, Length[events]}],
        GraphLayout -> "LayeredDigraphEmbedding",
        VertexStyle -> $eventStyle,
        EdgeStyle -> $causalStyle,
        VertexSize -> Medium
    ];
    If[ TrueQ @ OptionValue[HypergraphCausalGraph, {opts}, "TransitiveReduction"],
        Graph[TransitiveReductionGraph[g], Sequence @@ Options[g]],
        g
    ]
];

eventTooltip[events_, k_] := Column[{
    Row[{"event ", k, " \[LongDash] generation ", events[[k, "Generation"]],
         ", rule ", events[[k, "RuleIndex"]]}],
    events[[k, "Data", "Hypergraph"]]
}];

evolutionProp[e_, "EvolutionGraph", n_, opts___] :=
Module[{states = e["States"], events = e["Events"]},
    Graph[
        Range[Length[states]],
        Table[DirectedEdge[events[[k, "InputState"]], events[[k, "OutputState"]], k],
              {k, Length[events]}],
        VertexLabels -> Table[i -> Placed[states[[i, "Hypergraph"]], Tooltip], {i, Length[states]}],
        GraphLayout -> "LayeredDigraphEmbedding",
        VertexStyle -> $stateStyle,
        VertexSize -> Medium
    ]
];

(* States that are the same hypergraph are one vertex here, which is what makes this a states
   graph rather than the tree above. What "the same" means is the "CanonicalizeStates" option:
   by default it is up to canonical form, so two states differing only in what their vertices
   are called count as one, which is what usually makes a states graph close up at all -- a
   rewrite names the vertices it creates after what was already there, so two branches reaching
   the same shape rarely reach it under the same names. Setting it False asks for equality of
   the states as they stand, names and all. *)
evolutionProp[e_, "StatesGraph", n_, opts___] := Module[{
    states = e["States"], events = e["Events"], canon, distinct, index, representative
},
    canon = If[ TrueQ @ OptionValue[HypergraphCausalGraph, {opts}, "CanonicalizeStates"],
        CanonicalHypergraph[#["Hypergraph"]] & /@ states,
        #["Hypergraph"] & /@ states
    ];
    distinct = DeleteDuplicates[canon];
    index = AssociationThread[distinct -> Range[Length[distinct]]];
    representative = AssociationThread[
        Range[Length[distinct]] ->
            (states[[First @ FirstPosition[canon, #], "Hypergraph"]] & /@ distinct)];
    Graph[
        Range[Length[distinct]],
        DeleteDuplicates @ Table[
            DirectedEdge[index[canon[[events[[k, "InputState"]]]]],
                         index[canon[[events[[k, "OutputState"]]]]]],
            {k, Length[events]}
        ],
        VertexLabels -> Table[i -> Placed[representative[i], Tooltip], {i, Length[distinct]}],
        GraphLayout -> "LayeredDigraphEmbedding",
        VertexStyle -> $stateStyle,
        VertexSize -> Medium
    ]
];

(* Two state occurrences of the same generation are branchially connected when they came from the
   same state: the two ways one step could have gone. *)
evolutionProp[e_, "BranchialGraph", n_, opts___] := Module[{states = e["States"], sameGen},
    sameGen = Select[Range[Length[states]], states[[#, "Generation"]] === n &];
    Graph[
        sameGen,
        DeleteDuplicates @ Catenate @ Map[
            UndirectedEdge @@@ Subsets[#, {2}] &,
            Values @ GroupBy[sameGen, states[[#, "Parent"]] &]
        ],
        VertexLabels -> Table[i -> Placed[states[[i, "Hypergraph"]], Tooltip], {i, sameGen}],
        VertexStyle -> Directive[Hue[0.78, 0.45, 0.92], EdgeForm[Hue[0.78, 0.7, 0.6]]],
        VertexSize -> Medium
    ]
];

(* States and events together, with the causal edges laid over them: state to event to state, and
   event to event wherever the causal graph has one. *)
evolutionProp[e_, "EvolutionCausalGraph", n_, opts___] := Module[{
    states = e["States"], events = e["Events"], st, evn
},
    st[i_] := {"State", i};
    evn[k_] := {"Event", k};
    Graph[
        Join[st /@ Range[Length[states]], evn /@ Range[Length[events]]],
        Join[
            Catenate @ Table[
                {DirectedEdge[st[events[[k, "InputState"]]], evn[k]],
                 DirectedEdge[evn[k], st[events[[k, "OutputState"]]]]},
                {k, Length[events]}
            ],
            DeleteDuplicates[
                DirectedEdge[evn[#["From"]], evn[#["To"]]] & /@
                    evolutionProp[e, "CausalEdges", n, opts]
            ]
        ],
        VertexLabels -> Join[
            Table[st[i] -> Placed[states[[i, "Hypergraph"]], Tooltip], {i, Length[states]}],
            Table[evn[k] -> Placed[eventTooltip[events, k], Tooltip], {k, Length[events]}]
        ],
        VertexStyle -> {{"State", _} -> $stateStyle, {"Event", _} -> $eventStyle},
        VertexShapeFunction -> {{"Event", _} -> "Square"},
        GraphLayout -> "LayeredDigraphEmbedding",
        VertexSize -> Medium
    ]
];

evolutionProp[_, prop_, ___] := Missing["UnknownProperty", prop];


(* ::Section:: *)
(*The standalone causal graph*)


HypergraphCausalGraph[s_HypergraphRewritingSystem, n : _Integer : 1, opts : OptionsPattern[]] /;
    HypergraphRewritingSystemQ[s] := s["CausalGraph", n, opts];

HypergraphCausalGraph[rules_, init_, n : _Integer : 1, opts : OptionsPattern[]] :=
    With[{s = HypergraphRewritingSystem[rules, init]},
        If[HypergraphRewritingSystemQ[s], s["CausalGraph", n, opts], $Failed]
    ];

HypergraphCausalGraph[s_, ___] := (Message[HypergraphCausalGraph::sys, HoldForm[s]]; $Failed);


(* ::Section:: *)
(*Formatting*)


HypergraphRewritingSystem /: MakeBoxes[
    s_HypergraphRewritingSystem /; HypergraphRewritingSystemQ[s],
    form : StandardForm | TraditionalForm
] :=
    BoxForm`ArrangeSummaryBox[
        "HypergraphRewritingSystem", s, $HypergraphRewritingSystemIcon,
        {
            {BoxForm`SummaryItem[{"Rules: ", Length @ First[s]}]},
            {BoxForm`SummaryItem[{"Initial state: ",
                Row[{VertexCount @ Last[s], " vertices, ", EdgeCount @ Last[s], " edges"}]}]}
        },
        {
            {BoxForm`SummaryItem[{"Signatures: ", #["Signature"] & /@ First[s]}]}
        },
        form, "Interpretable" -> Automatic
    ];

$HypergraphRewritingSystemIcon := $HypergraphRewritingSystemIcon = Graphics[
    {
        Hue[0.58, 0.7, 0.7], AbsoluteThickness[1.2],
        Line[{{0, 1}, {-0.75, 0}}], Line[{{0, 1}, {0.75, 0}}],
        Line[{{-0.75, 0}, {-1.15, -1}}], Line[{{-0.75, 0}, {-0.35, -1}}],
        Line[{{0.75, 0}, {0.35, -1}}], Line[{{0.75, 0}, {1.15, -1}}],
        Hue[0.11, 0.85, 0.85],
        Disk[{0, 1}, 0.19], Disk[{-0.75, 0}, 0.19], Disk[{0.75, 0}, 0.19],
        Disk[{-1.15, -1}, 0.19], Disk[{-0.35, -1}, 0.19],
        Disk[{0.35, -1}, 0.19], Disk[{1.15, -1}, 0.19]
    },
    ImageSize -> Dynamic[{Automatic, 3.2 CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}],
    PlotRangePadding -> 0.3
];


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];

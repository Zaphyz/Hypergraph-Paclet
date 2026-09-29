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
          $CausalDependenceTypes, $HypergraphRewritingOptions];


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

$HypergraphRewritingOptions::usage =
    "$HypergraphRewritingOptions is the registry of options a rewriting system takes, with their " <>
    "defaults: the ones that govern the evolution, the ones that govern causal dependence, and " <>
    "the ones that govern how a graph is drawn.
" <>
    "Every graph-valued property takes these together with the options of Graph.
" <>
    "sys[\"Options\"] gives the values in force for a particular system.";

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

HypergraphRewritingSystem::undrawn =
    "There are `1` `2` to draw, more than the `3` allowed by \"MaxDrawnVertices\"; plain vertices are " <>
    "used instead. Raise that option, or set \"StateRendering\" or \"EventRendering\" to True, " <>
    "to draw them anyway.";


Begin["`Private`"];


(* ::Section:: *)
(*The system object*)


(* Canonical form: a list of rules, a Hypergraph, and a list of options. As everywhere else in
   this package the guard reads only the pattern variables, never the expression itself, so there
   is no re-entrancy.

   The options are held on the system because there are enough of them that repeating the ones
   that matter on every property call is a chore, and because they describe the system rather
   than the question being asked of it. An option given to a property still wins over one held
   here. *)
canonicalSystemArgsQ[rules_List, init_, opts_List] :=
    AllTrue[rules, HypergraphRuleQ] && HypergraphQ[init] &&
    MatchQ[opts, {(_Rule | _RuleDelayed) ...}];
canonicalSystemArgsQ[___] := False;

HypergraphRewritingSystem[args___] /; ! canonicalSystemArgsQ[args] := makeSystem[args];

makeSystem[rules_, init_, opts___] := Module[{
    rs = toRuleList[rules], hg = toInitial[init], os = Flatten[{opts}]
},
    If[rs === $Failed || hg === $Failed, Return[$Failed, Module]];
    If[ ! MatchQ[os, {(_Rule | _RuleDelayed) ...}],
        Message[HypergraphRewritingSystem::spec,
                HoldForm[HypergraphRewritingSystem[rules, init, opts]]];
        Return[$Failed, Module]
    ];
    HypergraphRewritingSystem[rs, hg, os]
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

(* The options are grouped by what they act on, and the groups are what everything else is built
   out of, so that there is one place where an option is written down and one place to look for
   it. $HypergraphRewritingOptions is the whole registry, and sys["Options"] says which values
   are actually in force for a given system. *)

$evolutionOptions = {
    "MaxStates" -> 2048,
    "Labeled" -> True,
    "LabelPatterns" -> True,
    "DistinctVertices" -> True
};

$causalOptions = {
    "CausalDependence" -> "Elements",
    "TransitiveReduction" -> True,
    "CanonicalizeStates" -> True
};

$drawingOptions = {
    "StateRendering" -> Automatic,
    "EventRendering" -> Automatic,
    "MaxDrawnVertices" -> 24,
    "PanelSize" -> 90,
    "HypergraphPlotOptions" -> {}
};

$HypergraphRewritingOptions = Join[$evolutionOptions, $causalOptions, $drawingOptions];

(* Every graph-valued property also takes the options of Graph, and what the caller passes wins
   over what the property would have set, since in WL the first setting of an option is the one
   used and the caller's are put first. *)
Options[HypergraphCausalGraph] = Join[$HypergraphRewritingOptions, Options[Graph]];
Options[HypergraphRewritingSystem] = Options[HypergraphCausalGraph];

dependenceType[o_List] := With[{t = OptionValue[HypergraphCausalGraph, o, "CausalDependence"]},
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

evolve[sys_, n_Integer, o_List] := Module[{
    rules = First[sys], init = sys[[2]],
    maxStates = OptionValue[HypergraphCausalGraph, o, "MaxStates"],
    applyOpts = FilterRules[o, Options[HypergraphRuleApply]],
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
    "Rules", "RuleCount", "InitialState", "Options",
    "States", "StateList", "StateCount",
    "EventList", "EventCount",
    "EvolutionGraph", "StatesGraph", "BranchialGraph",
    "CausalGraph", "CausalEdges", "EvolutionCausalGraph", "TokenEventGraph"
};
systemProp[s_, "Rules"] := First[s];
systemProp[s_, "RuleCount"] := Length @ First[s];
systemProp[s_, "InitialState"] := s[[2]];
(* Every option of the registry with the value actually in force: the system's own where it set
   one, the default otherwise. *)
systemProp[s_, "Options"] := Normal @ Join[
    Association[$HypergraphRewritingOptions],
    Association @ FilterRules[Last[s], $HypergraphRewritingOptions]
];

(* Options held on the system, with the ones given here put first so that they win. *)
systemOptions[s_, opts_] := Join[Flatten[{opts}], Last[s]];

systemProp[s_, prop_String, opts : OptionsPattern[HypergraphCausalGraph]] :=
    systemProp[s, prop, 1, opts];

systemProp[s_, prop_String, n_, opts : OptionsPattern[HypergraphCausalGraph]] :=
    If[ ! (IntegerQ[n] && n >= 0),
        Message[HypergraphRewritingSystem::gen, n]; $Failed,
        With[{o = systemOptions[s, {opts}]}, evolutionProp[evolve[s, n, o], prop, n, o]]
    ];

systemProp[_, prop_, ___] := Missing["UnknownProperty", prop];

evolutionProp[e_, "StateList", ___] := #["Hypergraph"] & /@ e["States"];
evolutionProp[e_, "StateCount", ___] := Length @ e["States"];
evolutionProp[e_, "States", ___] :=
    Values @ GroupBy[e["States"], #["Generation"] &, Map[#["Hypergraph"] &]];
evolutionProp[e_, "EventList", ___] := e["Events"];
evolutionProp[e_, "EventCount", ___] := Length @ e["Events"];

evolutionProp[e_, "CausalEdges", n_, o_List] :=
    causalPairs[e["Events"], e["States"], dependenceType[o]];


(* ::Subsection:: *)
(*Graphs*)


(* The colours of the Wolfram Physics Project, which is what the reference paclets draw these
   graphs in, so that a graph made here and one made there read the same way: states in blue,
   events in amber, causal edges in red, branchial edges in magenta.

   The values are copied rather than fetched. The reference asks
   ResourceFunction["WolframPhysicsProjectStyleData"] for them at load time, which is a network
   call on a machine that has not cached the function, and a package that cannot be loaded
   offline is not worth the exactness. These are that function's values. *)

$stateStyle = Directive[Opacity[0.7], Hue[0.62, 0.45, 0.87]];
$stateEdgeStyle = Hue[0.75, 0, 0.35];
$eventStyle = Directive[Hue[0.11, 1, 0.97], EdgeForm[{Hue[0.11, 1, 0.97], Opacity[1]}]];
$causalStyle = Hue[0, 1, 0.56];
$branchialStyle = Hue[0.89, 0.97, 0.71];
$tokenStyle = Directive[Opacity[0.75], Hue[0.35, 0.5, 0.75]];

$frameStyle = Directive[Opacity[0.6], Hue[0.62, 0.45, 0.6]];


(* ::Subsubsection:: *)
(*Whether to draw at all*)


(* Drawing a state means laying out a hypergraph, and drawing an event means laying out two. A
   few dozen of those is a moment; a few hundred is a wait long enough that nobody meant to ask
   for it. So past "MaxDrawnVertices" the graph falls back to numbered vertices and says which
   option to raise -- silently plain would be worse than slow, since the caller would think the
   pictures were not implemented. *)

drawQ[setting_, count_, o_List, what_String] := Which[
    setting === True, True,
    setting === False, False,
    True,
    With[{limit = OptionValue[HypergraphCausalGraph, o, "MaxDrawnVertices"]},
        If[ IntegerQ[limit] && count > limit,
            Message[HypergraphRewritingSystem::undrawn, count, what, limit];
            False,
            True
        ]
    ]
];

panelSize[o_List] := Replace[OptionValue[HypergraphCausalGraph, o, "PanelSize"],
                             Except[_ ? NumericQ] -> 90];

(* AspectRatio -> 1 is what makes a panel a known shape, which is what lets the inset below be
   sized without measuring it. The reference squares its rule plots for the same reason. *)
plotOptions[o_List] := Join[
    Replace[OptionValue[HypergraphCausalGraph, o, "HypergraphPlotOptions"], Except[_List] -> {}],
    {VertexLabels -> None, EdgeLabels -> None, AspectRatio -> 1}
];

(* A state, framed, for use as a graph vertex. The labels come off: a hypergraph drawn ninety
   pixels wide has no room for them, and the state is on the tooltip anyway. *)
statePanel[hg_, o_List] := Framed[
    HypergraphPlot[hg, Sequence @@ plotOptions[o],
        ImageSize -> panelSize[o], ImagePadding -> 7, Background -> None],
    Background -> GrayLevel[0.99], FrameStyle -> $frameStyle, FrameMargins -> 0,
    RoundingRadius -> 3
];

eventPanel[ev_, o_List] := Framed[
    HypergraphEventPlot[ev["Data"], Sequence @@ plotOptions[o],
        "PanelSize" -> panelSize[o], ImagePadding -> 7],
    Background -> GrayLevel[0.99], FrameStyle -> $frameStyle, FrameMargins -> 0,
    RoundingRadius -> 3
];

(* Insets are sized in the coordinates of the graph, so that VertexSize genuinely controls how
   big a picture is and the layout leaves room for it. Passing a pixel size the way the reference
   does makes the layout space the vertices as though each were sixty-four graph units wide.

   The width has to be given as well as the height, or a wide panel dropped into the square box a
   vertex is allotted runs off both sides of it. A state panel is square, being one squared plot;
   an event panel is the same twice over with an arrow between. *)
$statePanelAspect = 1;
$eventPanelAspect = 2.4;

(* The size a vertex shape function is handed is a radius, and the layout puts vertices a unit
   apart, so a panel drawn at that size comes out half the width it has room for. Twice it, less
   a little, fills the space between neighbours without the frames touching. *)
panelShapeFunction[panels_, aspect_ : 1] := Function[{pos, name, size},
    With[{w = 1.84 Min[Flatten[{size}]]},
        Inset[Lookup[panels, Key[name], ""], pos, Center, {w, w / aspect}]
    ]
];


(* ::Subsubsection:: *)
(*Assembling one*)


graphOptions[o_List] := FilterRules[o, Options[Graph]];

eventTooltip[events_, k_] := Column[{
    Row[{"event ", k, " \[LongDash] generation ", events[[k, "Generation"]],
         ", rule ", events[[k, "RuleIndex"]]}],
    events[[k, "Data", "Hypergraph"]]
}];

evolutionProp[e_, "CausalGraph", n_, o_List] := Module[{events = e["Events"], edges, drawn, g},
    edges = DeleteDuplicates[
        DirectedEdge[#["From"], #["To"]] & /@ evolutionProp[e, "CausalEdges", n, o]];
    drawn = drawQ[OptionValue[HypergraphCausalGraph, o, "EventRendering"], Length[events], o, "events"];
    g = Graph[
        Range[Length[events]], edges,
        Sequence @@ graphOptions[o],
        VertexShapeFunction -> If[
            drawn,
            panelShapeFunction[AssociationMap[eventPanel[events[[#]], o] &, Range[Length[events]]],
                               $eventPanelAspect],
            Automatic
        ],
        VertexSize -> If[drawn, $eventPanelAspect, Automatic],
        VertexLabels -> Table[k -> Placed[eventTooltip[events, k], Tooltip], {k, Length[events]}],
        GraphLayout -> "LayeredDigraphEmbedding",
        VertexStyle -> $eventStyle,
        EdgeStyle -> $causalStyle,
        PerformanceGoal -> "Quality",
        (* True coordinates, so that a panel a unit wide sits a unit from its neighbour rather
           than being stretched to fill whatever shape the image happens to be. *)
        AspectRatio -> Automatic,
        Background -> White
    ];
    If[ TrueQ @ OptionValue[HypergraphCausalGraph, o, "TransitiveReduction"],
        Graph[TransitiveReductionGraph[g], Sequence @@ Options[g]],
        g
    ]
];

evolutionProp[e_, "EvolutionGraph", n_, o_List] :=
Module[{states = e["States"], events = e["Events"], drawn},
    drawn = drawQ[OptionValue[HypergraphCausalGraph, o, "StateRendering"], Length[states], o, "states"];
    Graph[
        Range[Length[states]],
        Table[DirectedEdge[events[[k, "InputState"]], events[[k, "OutputState"]], k],
              {k, Length[events]}],
        Sequence @@ graphOptions[o],
        VertexShapeFunction -> If[
            drawn,
            panelShapeFunction @ AssociationMap[statePanel[states[[#, "Hypergraph"]], o] &,
                                                Range[Length[states]]],
            Automatic
        ],
        VertexSize -> If[drawn, 1, Automatic],
        VertexLabels -> Table[i -> Placed[states[[i, "Hypergraph"]], Tooltip], {i, Length[states]}],
        GraphLayout -> "LayeredDigraphEmbedding",
        VertexStyle -> $stateStyle,
        EdgeStyle -> $stateEdgeStyle,
        PerformanceGoal -> "Quality",
        (* True coordinates, so that a panel a unit wide sits a unit from its neighbour rather
           than being stretched to fill whatever shape the image happens to be. *)
        AspectRatio -> Automatic,
        Background -> White
    ]
];

(* States that are the same hypergraph are one vertex here, which is what makes this a states
   graph rather than the tree above. What "the same" means is the "CanonicalizeStates" option:
   by default it is up to canonical form, so two states differing only in what their vertices
   are called count as one, which is what usually makes a states graph close up at all -- a
   rewrite names the vertices it creates after what was already there, so two branches reaching
   the same shape rarely reach it under the same names. Setting it False asks for equality of
   the states as they stand, names and all. *)
evolutionProp[e_, "StatesGraph", n_, o_List] := Module[{
    states = e["States"], events = e["Events"], canon, distinct, index, representative, drawn
},
    canon = If[ TrueQ @ OptionValue[HypergraphCausalGraph, o, "CanonicalizeStates"],
        CanonicalHypergraph[#["Hypergraph"]] & /@ states,
        #["Hypergraph"] & /@ states
    ];
    distinct = DeleteDuplicates[canon];
    index = AssociationThread[distinct -> Range[Length[distinct]]];
    representative = AssociationThread[
        Range[Length[distinct]] ->
            (states[[First @ FirstPosition[canon, #], "Hypergraph"]] & /@ distinct)];
    drawn = drawQ[OptionValue[HypergraphCausalGraph, o, "StateRendering"], Length[distinct], o, "states"];
    Graph[
        Range[Length[distinct]],
        DeleteDuplicates @ Table[
            DirectedEdge[index[canon[[events[[k, "InputState"]]]]],
                         index[canon[[events[[k, "OutputState"]]]]]],
            {k, Length[events]}
        ],
        Sequence @@ graphOptions[o],
        VertexShapeFunction -> If[
            drawn, panelShapeFunction @ Map[statePanel[#, o] &, representative], Automatic],
        VertexSize -> If[drawn, 1, Automatic],
        VertexLabels -> Table[i -> Placed[representative[i], Tooltip], {i, Length[distinct]}],
        GraphLayout -> "LayeredDigraphEmbedding",
        VertexStyle -> $stateStyle,
        EdgeStyle -> $stateEdgeStyle,
        PerformanceGoal -> "Quality",
        (* True coordinates, so that a panel a unit wide sits a unit from its neighbour rather
           than being stretched to fill whatever shape the image happens to be. *)
        AspectRatio -> Automatic,
        Background -> White
    ]
];

(* Two state occurrences of the same generation are branchially connected when they came from the
   same state: the two ways one step could have gone. *)
evolutionProp[e_, "BranchialGraph", n_, o_List] := Module[{states = e["States"], sameGen, drawn},
    sameGen = Select[Range[Length[states]], states[[#, "Generation"]] === n &];
    drawn = drawQ[OptionValue[HypergraphCausalGraph, o, "StateRendering"], Length[sameGen], o, "states"];
    Graph[
        sameGen,
        DeleteDuplicates @ Catenate @ Map[
            UndirectedEdge @@@ Subsets[#, {2}] &,
            Values @ GroupBy[sameGen, states[[#, "Parent"]] &]
        ],
        Sequence @@ graphOptions[o],
        VertexShapeFunction -> If[
            drawn,
            panelShapeFunction @ AssociationMap[statePanel[states[[#, "Hypergraph"]], o] &, sameGen],
            Automatic
        ],
        VertexSize -> If[drawn, 1, Automatic],
        VertexLabels -> Table[i -> Placed[states[[i, "Hypergraph"]], Tooltip], {i, sameGen}],
        VertexStyle -> $stateStyle,
        EdgeStyle -> $branchialStyle,
        PerformanceGoal -> "Quality",
        (* True coordinates, so that a panel a unit wide sits a unit from its neighbour rather
           than being stretched to fill whatever shape the image happens to be. *)
        AspectRatio -> Automatic,
        Background -> White
    ]
];

(* States and events together, with the causal edges laid over them: state to event to state, and
   event to event wherever the causal graph has one. *)
evolutionProp[e_, "EvolutionCausalGraph", n_, o_List] := Module[{
    states = e["States"], events = e["Events"], st, evn, causal, drawn, panels
},
    st[i_] := {"State", i};
    evn[k_] := {"Event", k};
    causal = DeleteDuplicates[
        DirectedEdge[evn[#["From"]], evn[#["To"]]] & /@ evolutionProp[e, "CausalEdges", n, o]];
    drawn = drawQ[OptionValue[HypergraphCausalGraph, o, "StateRendering"],
                  Length[states] + Length[events], o, "states and events"];
    panels = If[ drawn,
        Join[
            AssociationMap[statePanel[states[[Last[#], "Hypergraph"]], o] &, st /@ Range[Length[states]]],
            AssociationMap[eventPanel[events[[Last[#]]], o] &, evn /@ Range[Length[events]]]
        ],
        <||>
    ];
    Graph[
        Join[st /@ Range[Length[states]], evn /@ Range[Length[events]]],
        Join[
            Catenate @ Table[
                {DirectedEdge[st[events[[k, "InputState"]]], evn[k]],
                 DirectedEdge[evn[k], st[events[[k, "OutputState"]]]]},
                {k, Length[events]}
            ],
            causal
        ],
        Sequence @@ graphOptions[o],
        VertexShapeFunction -> If[
            drawn,
            {{"State", _} -> panelShapeFunction[panels, $statePanelAspect],
             {"Event", _} -> panelShapeFunction[panels, $eventPanelAspect]},
            {{"Event", _} -> "Square"}
        ],
        VertexSize -> If[
            drawn,
            {{"State", _} -> $statePanelAspect, {"Event", _} -> $eventPanelAspect},
            Automatic
        ],
        VertexLabels -> Join[
            Table[st[i] -> Placed[states[[i, "Hypergraph"]], Tooltip], {i, Length[states]}],
            Table[evn[k] -> Placed[eventTooltip[events, k], Tooltip], {k, Length[events]}]
        ],
        VertexStyle -> {{"State", _} -> $stateStyle, {"Event", _} -> $eventStyle},
        (* The specific rules first: in a list of option rules the first that matches wins. *)
        EdgeStyle -> Join[
            # -> $causalStyle & /@ causal,
            {_ -> $stateEdgeStyle}
        ],
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality",
        (* True coordinates, so that a panel a unit wide sits a unit from its neighbour rather
           than being stretched to fill whatever shape the image happens to be. *)
        AspectRatio -> Automatic,
        Background -> White
    ]
];

(* Elements as vertices, events between them: the picture in which a causal dependence is visible
   as a path rather than inferred. An event takes the elements it consumed and gives the ones it
   created, and two events are joined through the element that passed from one to the other.

   Which elements appear is the "CausalDependence" option, the same choice as everywhere else:
   under "Edges" only edge tokens are drawn, so a dependence that runs only through a vertex has
   nothing to run along and the two events stand apart. *)
evolutionProp[e_, "TokenEventGraph", n_, o_List] := Module[{
    events = e["Events"], type = dependenceType[o], evn, tok, edges, tokens, drawn, panels
},
    evn[k_] := {"Event", k};
    tok[t_] := {"Token", t};
    edges = Catenate @ Table[
        Join[
            DirectedEdge[tok[#], evn[k]] & /@ consumed[events[[k]], type],
            DirectedEdge[evn[k], tok[#]] & /@ produced[events[[k]], type]
        ],
        {k, Length[events]}
    ];
    tokens = DeleteDuplicates @ Cases[edges, {"Token", _}, {2}];
    drawn = drawQ[OptionValue[HypergraphCausalGraph, o, "EventRendering"], Length[events], o, "events"];
    panels = If[drawn, AssociationMap[eventPanel[events[[Last[#]]], o] &, evn /@ Range[Length[events]]], <||>];
    Graph[
        Join[tokens, evn /@ Range[Length[events]]],
        DeleteDuplicates[edges],
        Sequence @@ graphOptions[o],
        VertexShapeFunction -> If[
            drawn,
            {{"Event", _} -> panelShapeFunction[panels, $eventPanelAspect], {"Token", _} -> Automatic},
            {{"Event", _} -> "Square"}
        ],
        VertexSize -> If[drawn, {{"Event", _} -> 1, {"Token", _} -> Automatic}, Automatic],
        VertexLabels -> Join[
            Table[evn[k] -> Placed[eventTooltip[events, k], Tooltip], {k, Length[events]}],
            # -> Placed[tokenName[#], Tooltip] & /@ tokens
        ],
        VertexStyle -> {{"Token", _} -> $tokenStyle, {"Event", _} -> $eventStyle},
        EdgeStyle -> $stateEdgeStyle,
        GraphLayout -> "LayeredDigraphEmbedding",
        PerformanceGoal -> "Quality",
        (* True coordinates, so that a panel a unit wide sits a unit from its neighbour rather
           than being stretched to fill whatever shape the image happens to be. *)
        AspectRatio -> Automatic,
        Background -> White
    ]
];

(* The tokens an event takes and the ones it gives, in the currency the option asks for. *)
consumed[ev_, "Edges"] := {"E", #} & /@ ev["ConsumedEdges"];
consumed[ev_, "Vertices"] := {"V", #} & /@ Union[ev["MatchedVertices"], ev["DestroyedVertices"]];
consumed[ev_, "Elements"] := Join[consumed[ev, "Edges"], consumed[ev, "Vertices"]];
produced[ev_, "Edges"] := {"E", #} & /@ ev["CreatedEdges"];
produced[ev_, "Vertices"] := {"V", #} & /@ Union[ev["MatchedVertices"], ev["CreatedVertices"]];
produced[ev_, "Elements"] := Join[produced[ev, "Edges"], produced[ev, "Vertices"]];

tokenName[{"Token", {"E", k_}}] := Row[{"edge ", k}];
tokenName[{"Token", {"V", k_}}] := Row[{"vertex ", k}];
tokenName[t_] := t;

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
                Row[{VertexCount @ s[[2]], " vertices, ", EdgeCount @ s[[2]], " edges"}]}]}
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

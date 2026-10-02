(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`HypergraphRewriting -- rewriting systems, from the ground up*)


(* ::Text:: *)
(*A second attempt at the causality layer, written without reference to how the upstream paclets*)
(*organise theirs, to be judged against src/Causality.wl and either adopted or dropped.*)
(**)
(*It takes as given everything up to and including rule application: Hypergraph, HypergraphPlot,*)
(*HypergraphRule, HypergraphRuleApply and the pictures of rules and events. Everything above that*)
(*-- the evolution, the tokens, causality, and all the graphs -- is new here.*)
(**)
(*The one idea it is built on is that a hyperedge is a thing with an identity, not a value. Every*)
(*edge of every state is a token: it is created by exactly one event, consumed by at most one,*)
(*and carries no other history. Once that is written down, nothing else has to be computed:*)
(**)
(*  - the causal graph is the tokens, read as arrows from the event that made each to the event*)
(*    that used it. There is no overlap test and no ancestry test, because a token already knows*)
(*    both ends of its own story.*)
(*  - the event graph is the states and events, which is what the evolution literally did.*)
(*  - the states graph is that with the events contracted and equal states merged.*)
(*  - the branchial graph is the states of one step that share a parent.*)
(*  - the token-event graph is the tokens and events themselves, which is the thing all four of*)
(*    the others are projections of.*)
(**)
(*Load with Get on this file; it needs WolframInstitute`Hypergraphs` on the paclet path.*)


BeginPackage["WolframInstitute`HypergraphRewriting`", {"WolframInstitute`Hypergraphs`"}];

Unprotect[RewritingSystem, RewritingSystemQ, RewritingEvolution, RewritingEvolutionQ];
ClearAll[RewritingSystem, RewritingSystemQ, RewritingEvolution, RewritingEvolutionQ];


(* ::Section:: *)
(*Usage*)


RewritingSystem::usage =
    "RewritingSystem[rules, init] is the rewriting system with the given rules started from the \
hypergraph init.\n\
rules may be one HypergraphRule, a list of them, or plain arrows lhs -> rhs; init may be \
anything Hypergraph accepts.\n\
sys[n] evolves it n steps and gives a RewritingEvolution.";

RewritingSystemQ::usage = "RewritingSystemQ[expr] is True for a valid RewritingSystem.";

RewritingEvolution::usage =
    "RewritingEvolution[...] is what a rewriting system evolved some number of steps gives \
back.\n\
ev[prop] gives one of its parts or one of its graphs; ev[\"Properties\"] lists them.\n\
Its graphs are \"StatesGraph\", \"EventGraph\", \"CausalGraph\", \"TokenEventGraph\" and \
\"BranchialGraph\".";

RewritingEvolutionQ::usage = "RewritingEvolutionQ[expr] is True for a valid RewritingEvolution.";


(* ::Section:: *)
(*Messages*)


RewritingSystem::spec = "`1` is not a rewriting system specification.";
RewritingSystem::rules = "`1` is not a rewrite rule or a list of them.";
RewritingSystem::init = "`1` is not a hypergraph, and cannot be an initial state.";
RewritingSystem::steps = "`1` is not a non-negative number of steps.";
RewritingSystem::big =
    "The evolution reached `1` states, the limit set by \"MaxStates\", during step `2`, and was \
stopped there.";
RewritingEvolution::prop = "`1` is not a property of a rewriting evolution; `2` are.";


Begin["`Private`"];


(* ::Section:: *)
(*The system*)


systemArgsQ[rules_List, init_] := AllTrue[rules, HypergraphRuleQ] && HypergraphQ[init];
systemArgsQ[___] := False;

RewritingSystem[args___] /; ! systemArgsQ[args] := buildSystem[args];

buildSystem[rules_, init_] := Module[{rs = ruleList[rules], hg = initialState[init]},
    If[rs === $Failed || hg === $Failed, Return[$Failed, Module]];
    RewritingSystem[rs, hg]
];
buildSystem[args___] := (Message[RewritingSystem::spec, HoldForm[RewritingSystem[args]]]; $Failed);

ruleList[r_HypergraphRule] /; HypergraphRuleQ[r] := {r};
ruleList[rs_List] := With[{out = asRule /@ rs}, If[MemberQ[out, $Failed], $Failed, out]];
ruleList[r : (_Rule | _RuleDelayed)] := ruleList[{r}];
ruleList[other_] := (Message[RewritingSystem::rules, HoldForm[other]]; $Failed);

asRule[r_HypergraphRule] /; HypergraphRuleQ[r] := r;
asRule[(Rule | RuleDelayed)[lhs_, rhs_]] := With[{r = Quiet @ HypergraphRule[lhs, rhs]},
    If[HypergraphRuleQ[r], r, Message[RewritingSystem::rules, HoldForm[lhs -> rhs]]; $Failed]];
asRule[other_] := (Message[RewritingSystem::rules, HoldForm[other]]; $Failed);

initialState[hg_Hypergraph] /; HypergraphQ[hg] := hg;
initialState[spec_] := With[{hg = Quiet @ Hypergraph[spec]},
    If[HypergraphQ[hg], hg, Message[RewritingSystem::init, HoldForm[spec]]; $Failed]];

RewritingSystemQ[s_RewritingSystem] := systemArgsQ @@ s;
RewritingSystemQ[_] := False;

(s_RewritingSystem ? RewritingSystemQ)["Rules"] := First[s];
(s_RewritingSystem ? RewritingSystemQ)["InitialState"] := Last[s];
(s_RewritingSystem ? RewritingSystemQ)["Properties"] := {"Rules", "InitialState"};


(* ::Section:: *)
(*Options*)


Options[RewritingSystem] = {
    "MaxStates" -> 512,
    "PanelSize" -> 80,
    "Draw" -> Automatic,
    "MaxDrawn" -> 30,
    "MergeStates" -> True
};

Options[RewritingEvolution] = Join[Options[RewritingSystem], Options[Graph]];

opt[o_List, name_] := OptionValue[RewritingEvolution, o, name];


(* ::Section:: *)
(*Evolving*)


(* A step takes each state of the frontier, finds every way every rule fits it, and records one
   event per way. The bookkeeping is all in the tokens: the edges the match covered are the ones
   this event consumed, and the edges of the result that came from nowhere are the ones it
   created. "EdgeOrigins", which the rewriting layer reports, is what says which is which --
   the Hypergraph constructor sorts its edges, so a position in the result means nothing on its
   own, and two edges of equal value are two different tokens. *)

(s_RewritingSystem ? RewritingSystemQ)[n : Except[_String], o : OptionsPattern[RewritingEvolution]] :=
    If[ ! (IntegerQ[n] && n >= 0),
        Message[RewritingSystem::steps, n]; $Failed,
        evolve[s, n, Flatten[{o}]]
    ];

evolve[sys_, n_, o_List] := Module[{
    rules = First[sys], init = Last[sys],
    maxStates = opt[o, "MaxStates"],
    applyOpts = FilterRules[o, Options[HypergraphRuleApply]],
    nextToken = 0, mint, tokens = <||>, states, events = {}, frontier, stopped = False
},
    mint[edge_, by_, step_] := (
        tokens[++nextToken] = <|"Edge" -> edge, "CreatedBy" -> by, "Step" -> step|>;
        nextToken
    );

    states = {<|
        "Hypergraph" -> init,
        "Tokens" -> (mint[#, None, 0] & /@ EdgeList[init]),
        "Step" -> 0, "Parent" -> None, "InEvent" -> None
    |>};
    frontier = {1};

    Do[
        Block[{nextFrontier = {}},
            Do[
                Do[
                    Do[
                        If[Length[states] >= maxStates, stopped = True; Break[]];
                        Block[{
                            parent = states[[si]], consumed, created, childTokens, k = 0, newTokens
                        },
                            consumed = parent["Tokens"][[raw["MatchEdgePositions"]]];
                            newTokens = Cases[
                                Thread[{raw["EdgeOrigins"], EdgeList[raw["Hypergraph"]]}],
                                {None, e_} :> e
                            ];
                            created = mint[#, Length[events] + 1, step] & /@ newTokens;
                            childTokens = If[# === None, created[[++k]], parent["Tokens"][[#]]] & /@
                                raw["EdgeOrigins"];
                            AppendTo[states, <|
                                "Hypergraph" -> raw["Hypergraph"],
                                "Tokens" -> childTokens,
                                "Step" -> step, "Parent" -> si, "InEvent" -> Length[events] + 1
                            |>];
                            AppendTo[events, <|
                                "Rule" -> ri, "Step" -> step,
                                "InputState" -> si, "OutputState" -> Length[states],
                                "Consumed" -> consumed, "Created" -> created,
                                "Match" -> raw
                            |>];
                            AppendTo[nextFrontier, Length[states]]
                        ],
                        {raw, HypergraphRuleApply[rules[[ri]], states[[si, "Hypergraph"]], applyOpts]}
                    ];
                    If[stopped, Break[]],
                    {ri, Length[rules]}
                ];
                If[stopped, Break[]],
                {si, frontier}
            ];
            frontier = nextFrontier
        ];
        If[stopped, Message[RewritingSystem::big, maxStates, step]; Break[]],
        {step, n}
    ];

    RewritingEvolution[<|
        "System" -> sys, "Steps" -> n, "Options" -> o,
        "States" -> states, "Events" -> events, "Tokens" -> tokens,
        (* Which events used each token. A list, not one event: along any single branch a token
           is consumed at most once, but two branches out of the same state may each consume it,
           and both of those are real. Keeping only one would silently lose causal arrows. *)
        "ConsumedBy" -> Merge[
            Catenate @ MapIndexed[Function[{e, k}, # -> First[k] & /@ e["Consumed"]], events],
            Identity]
    |>]
];

RewritingEvolutionQ[RewritingEvolution[d_Association]] :=
    AllTrue[{"States", "Events", "Tokens", "ConsumedBy"}, KeyExistsQ[d, #] &];
RewritingEvolutionQ[_] := False;


(* ::Section:: *)
(*Reading it*)


$properties = {
    "System", "Steps", "StateCount", "EventCount", "TokenCount",
    "States", "Events", "Tokens", "StatesByStep",
    "StatesGraph", "EventGraph", "CausalGraph", "TokenEventGraph", "BranchialGraph"
};

(e_RewritingEvolution ? RewritingEvolutionQ)[prop_String, o : OptionsPattern[RewritingEvolution]] :=
    property[First[e], prop, Join[Flatten[{o}], First[e]["Options"]]];

property[_, "Properties", _] := $properties;
property[d_, "System", _] := d["System"];
property[d_, "Steps", _] := d["Steps"];
property[d_, "StateCount", _] := Length @ d["States"];
property[d_, "EventCount", _] := Length @ d["Events"];
property[d_, "TokenCount", _] := Length @ d["Tokens"];
property[d_, "States", _] := #["Hypergraph"] & /@ d["States"];
property[d_, "Events", _] := d["Events"];
property[d_, "Tokens", _] := d["Tokens"];
property[d_, "StatesByStep", _] :=
    Values @ GroupBy[d["States"], #["Step"] &, Map[#["Hypergraph"] &]];
property[_, prop_, _] := (Message[RewritingEvolution::prop, prop, $properties]; $Failed);


(* ::Section:: *)
(*Drawing*)


(* One palette, used by every graph, so that the same kind of thing is the same colour wherever
   it appears: states blue, events amber, tokens green, and the arrows coloured by what they
   mean rather than by which graph they are in. *)

$stateColour = Hue[0.60, 0.45, 0.85];
$eventColour = Hue[0.11, 0.80, 0.95];
$tokenColour = Hue[0.33, 0.45, 0.75];
$flowArrow = GrayLevel[0.55];
$causalArrow = Hue[0.00, 0.85, 0.75];
$branchArrow = Hue[0.85, 0.65, 0.80];
$frameLine = GrayLevel[0.78];

framed[g_] := Framed[g, Background -> GrayLevel[1], FrameStyle -> $frameLine,
                     FrameMargins -> 0, RoundingRadius -> 3];

statePanel[hg_, o_List] := framed @ HypergraphPlot[hg,
    VertexLabels -> None, EdgeLabels -> Automatic, AspectRatio -> Automatic,
    ImageSize -> opt[o, "PanelSize"], ImagePadding -> 8, Background -> None];

eventPanel[ev_, o_List] := framed @ HypergraphEventPlot[ev["Match"],
    VertexLabels -> None, EdgeLabels -> Automatic, AspectRatio -> Automatic,
    "PanelSize" -> opt[o, "PanelSize"], ImagePadding -> 8];

(* A token is one hyperedge, so it is drawn as one: the smallest picture in the stack, and the
   one that makes a causal chain readable as a chain of things rather than of numbers. *)
tokenPanel[tok_, o_List] := framed @ HypergraphPlot[Hypergraph[{tok["Edge"]}],
    VertexLabels -> "Name", EdgeLabels -> Automatic, AspectRatio -> Automatic,
    ImageSize -> Round[0.7 opt[o, "PanelSize"]], ImagePadding -> 8, Background -> None];

(* Whether to draw at all. Laying out and measuring a hypergraph for every vertex of a large
   graph is a wait nobody asked for, so past "MaxDrawn" the graph comes out plain. *)
drawQ[count_, o_List] := Replace[opt[o, "Draw"], {
    True -> True, False -> False,
    _ -> TrueQ[count <= opt[o, "MaxDrawn"]]}];


(* ::Subsection:: *)
(*Panels, measured and placed*)


(* Each panel is measured rather than assumed. A state that lays out as a line and an event that
   is two of those side by side are nothing like the same shape, and insetting either into a box
   of the wrong shape shrinks it to fit and wastes most of the room. Measuring costs one
   rasterization per panel, which is part of why there is a limit on how many are drawn. *)

measure[g_] := With[{sz = Quiet @ Rasterize[g, "RasterSize"]},
    {g, If[MatchQ[sz, {_ ? Positive, _ ? Positive}], N[First[sz] / Last[sz]], 1]}];

panels[assoc_Association] := measure /@ assoc;

(* Every panel is drawn one unit high and as many units wide as it is wide, and the vertices are
   placed far enough apart for that. VertexSize is no use here: it is not what the layered layout
   spaces vertices by, so a wide panel asked for through VertexSize still lands on top of its
   neighbour. Placing them directly is both simpler and exact.

   The layer of a vertex is given by the caller, since the evolution already knows it -- a state
   or an event knows its step -- and the order within a layer is taken from the ordinary layout,
   which is good at keeping the arrows from crossing. *)

$gapAcross = 1.15;
$gapDown = 1.55;

place[vertices_List, edges_List, layerOf_, widthOf_] := Module[{base, order, wide, rows},
    base = Quiet @ Check[
        AssociationThread[vertices -> First /@ GraphEmbedding[
            Graph[vertices, edges, GraphLayout -> "LayeredDigraphEmbedding"]]],
        AssociationThread[vertices -> Range[Length[vertices]]]];
    order = Lookup[base, #, 0] &;
    wide = Max[Append[widthOf /@ vertices, 1]];
    rows = GroupBy[vertices, layerOf];
    Association @ Catenate @ KeyValueMap[
        Function[{layer, vs},
            With[{sorted = SortBy[vs, order]},
                MapIndexed[
                    #1 -> {($gapAcross wide) (First[#2] - (Length[sorted] + 1) / 2),
                           - $gapDown layer} &,
                    sorted]]],
        rows]
];

(* Drawn at ninety-five percent of its cell, so that neighbouring frames do not touch. *)
inset[measured_] := Function[{pos, name, size},
    With[{pa = Lookup[measured, Key[name], {"", 1}]},
        Inset[First[pa], pos, Center, {0.95 Last[pa], 0.95}]]];

widthOf[measured_] := Function[v, Last @ Lookup[measured, Key[v], {"", 1}]];

(* The common tail of every drawn graph: place the vertices, inset the panels, and let Graph do
   the arrows. Undrawn, it is an ordinary graph with an ordinary layout. *)
assemble[vertices_, edges_, measured_, layerOf_, styles_, edgeStyle_, labels_, draw_, o_List] :=
    If[ draw,
        Graph[vertices, edges,
            Sequence @@ graphOpts[o],
            VertexCoordinates -> Lookup[place[vertices, edges, layerOf, widthOf[measured]], vertices],
            VertexShapeFunction -> inset[measured],
            VertexStyle -> styles,
            VertexLabels -> labels,
            EdgeStyle -> edgeStyle,
            PerformanceGoal -> "Quality", AspectRatio -> Automatic, Background -> White],
        Graph[vertices, edges,
            Sequence @@ graphOpts[o],
            VertexStyle -> styles,
            VertexLabels -> labels,
            EdgeStyle -> edgeStyle,
            GraphLayout -> "LayeredDigraphEmbedding",
            PerformanceGoal -> "Quality", Background -> White]
    ];

graphOpts[o_List] := FilterRules[o, Options[Graph]];


(* ::Subsection:: *)
(*The event graph: what the evolution did*)


property[d_, "EventGraph", o_List] := Module[{
    states = d["States"], events = d["Events"], st, evn, vs, es, draw, measured
},
    st[i_] := {"state", i};
    evn[k_] := {"event", k};
    vs = Join[st /@ Range[Length[states]], evn /@ Range[Length[events]]];
    es = Catenate @ Table[
        {DirectedEdge[st[events[[k, "InputState"]]], evn[k]],
         DirectedEdge[evn[k], st[events[[k, "OutputState"]]]]},
        {k, Length[events]}];
    draw = drawQ[Length[vs], o];
    measured = If[ draw,
        panels @ AssociationMap[
            Replace[#, {{"state", i_} :> statePanel[states[[i, "Hypergraph"]], o],
                        {"event", k_} :> eventPanel[events[[k]], o]}] &, vs],
        <||>];
    assemble[vs, es, measured,
        Replace[#, {{"state", i_} :> 2 states[[i, "Step"]],
                    {"event", k_} :> 2 events[[k, "Step"]] - 1}] &,
        {{"state", _} -> $stateColour, {"event", _} -> $eventColour},
        $flowArrow, tooltips[d], draw, o]
];


(* ::Subsection:: *)
(*The states graph: the same with the events contracted*)


property[d_, "StatesGraph", o_List] := Module[{
    states = d["States"], events = d["Events"], key, index, representative, step,
    vs, es, draw, measured
},
    (* Two states are the same vertex when they are the same hypergraph. Up to canonical form by
       default: a rewrite names what it creates after what was already there, so two branches
       that reach the same shape almost never reach it under the same names, and comparing them
       as written would merge nothing. *)
    key = If[ TrueQ @ opt[o, "MergeStates"],
        CanonicalHypergraph[#["Hypergraph"]] & /@ states,
        #["Hypergraph"] & /@ states];
    index = AssociationThread[DeleteDuplicates[key] -> Range[Length @ DeleteDuplicates[key]]];
    representative = AssociationMap[
        states[[First @ FirstPosition[key, #], "Hypergraph"]] &, Keys[index]];
    step = AssociationMap[
        Min[Pick[#["Step"] & /@ states, key, #]] &, Keys[index]];
    vs = Values[index];
    es = DeleteDuplicates @ Table[
        DirectedEdge[index[key[[events[[k, "InputState"]]]]],
                     index[key[[events[[k, "OutputState"]]]]]],
        {k, Length[events]}];
    draw = drawQ[Length[vs], o];
    measured = If[ draw,
        panels @ AssociationThread[vs -> (statePanel[representative[#], o] & /@ Keys[index])],
        <||>];
    assemble[vs, DeleteCases[es, DirectedEdge[v_, v_]], measured,
        step[Keys[index][[#]]] &,
        $stateColour, $flowArrow,
        Normal @ AssociationThread[vs -> (Placed[representative[#], Tooltip] & /@ Keys[index])],
        draw, o]
];


(* ::Subsection:: *)
(*The causal graph: the tokens, read as arrows*)


(* Every token created by one event and consumed by another is one arrow of the causal graph.
   That is the whole computation. There is no overlap test and no ancestry test, because a token
   already knows both ends of its own story. *)

causalArrows[d_] := With[{consumedBy = d["ConsumedBy"]},
    Catenate @ KeyValueMap[
        Function[{id, tok},
            If[ tok["CreatedBy"] === None,
                {},
                {tok["CreatedBy"], #, id} & /@ Lookup[consumedBy, id, {}]]],
        d["Tokens"]]
];

property[d_, "CausalGraph", o_List] := Module[{
    events = d["Events"], arrows = causalArrows[d], vs, draw, measured
},
    vs = Range[Length[events]];
    draw = drawQ[Length[vs], o];
    measured = If[draw, panels @ AssociationMap[eventPanel[events[[#]], o] &, vs], <||>];
    assemble[vs, DeleteDuplicates[DirectedEdge @@@ arrows[[All, {1, 2}]]], measured,
        events[[#, "Step"]] &,
        $eventColour, $causalArrow,
        Table[k -> Placed[eventTooltip[d, k], Tooltip], {k, Length[events]}],
        draw, o]
];


(* ::Subsection:: *)
(*The token-event graph: the thing the others are projections of*)


property[d_, "TokenEventGraph", o_List] := Module[{
    events = d["Events"], tokens = d["Tokens"], tk, evn, es, used, vs, draw, measured
},
    tk[id_] := {"token", id};
    evn[k_] := {"event", k};
    es = Catenate @ Table[
        Join[
            DirectedEdge[tk[#], evn[k]] & /@ events[[k, "Consumed"]],
            DirectedEdge[evn[k], tk[#]] & /@ events[[k, "Created"]]],
        {k, Length[events]}];
    used = DeleteDuplicates @ Cases[es, {"token", _}, {2}];
    vs = Join[used, evn /@ Range[Length[events]]];
    draw = drawQ[Length[vs], o];
    measured = If[ draw,
        panels @ AssociationMap[
            Replace[#, {{"token", id_} :> tokenPanel[tokens[id], o],
                        {"event", k_} :> eventPanel[events[[k]], o]}] &, vs],
        <||>];
    assemble[vs, DeleteDuplicates[es], measured,
        Replace[#, {{"token", id_} :> 2 tokens[id, "Step"],
                    {"event", k_} :> 2 events[[k, "Step"]] - 1}] &,
        {{"token", _} -> $tokenColour, {"event", _} -> $eventColour},
        $flowArrow,
        Join[
            Table[evn[k] -> Placed[eventTooltip[d, k], Tooltip], {k, Length[events]}],
            # -> Placed[Row[{"token ", Last[#], ": ", tokens[Last[#], "Edge"]}], Tooltip] & /@ used],
        draw, o]
];


(* ::Subsection:: *)
(*The branchial graph: the states one step could have reached*)


property[d_, "BranchialGraph", o_List] := Module[{states = d["States"], here, draw, measured},
    here = Select[Range[Length[states]], states[[#, "Step"]] === d["Steps"] &];
    draw = drawQ[Length[here], o];
    measured = If[draw, panels @ AssociationMap[statePanel[states[[#, "Hypergraph"]], o] &, here], <||>];
    assemble[here,
        DeleteDuplicates @ Catenate @ Map[
            UndirectedEdge @@@ Subsets[#, {2}] &,
            Values @ GroupBy[here, states[[#, "Parent"]] &]],
        measured,
        (* All of one step, so they go in one row, grouped by where they came from. *)
        0 &,
        $stateColour, Directive[$branchArrow, Dashed],
        Table[i -> Placed[states[[i, "Hypergraph"]], Tooltip], {i, here}],
        draw, o]
];


(* ::Subsection:: *)
(*Tooltips*)


eventTooltip[d_, k_] := With[{e = d["Events"][[k]]},
    Column[{
        Row[{"event ", k, " \[LongDash] step ", e["Step"], ", rule ", e["Rule"]}],
        Row[{"consumed ", e["Consumed"], ", created ", e["Created"]}],
        e["Match", "Hypergraph"]
    }]];

tooltips[d_] := Join[
    Table[{"state", i} -> Placed[d["States"][[i, "Hypergraph"]], Tooltip],
          {i, Length @ d["States"]}],
    Table[{"event", k} -> Placed[eventTooltip[d, k], Tooltip], {k, Length @ d["Events"]}]];


(* ::Section:: *)
(*How the objects show themselves*)


(* A rewriting system is its rules and where it starts, so that is what it displays as. Reading
   a system off two vertex counts is exactly the thing this package exists not to do. *)

RewritingSystem /: MakeBoxes[s_RewritingSystem /; RewritingSystemQ[s], form : StandardForm | TraditionalForm] :=
    BoxForm`ArrangeSummaryBox[
        "RewritingSystem", s, $systemIcon,
        {{BoxForm`SummaryItem[{"Rules: ", Length @ First[s]}]},
         {BoxForm`SummaryItem[{"Start: ",
            Row[{VertexCount @ Last[s], " vertices, ", EdgeCount @ Last[s], " edges"}]}]}},
        {{Column[Prepend[
             Quiet @ Check[HypergraphRulePlot[#, "PanelSize" -> 100], ""] & /@ First[s],
             Row[{"start:  ", Quiet @ Check[HypergraphPlot[Last[s], ImageSize -> 110], ""]}]
          ], Spacings -> 0.4]}},
        form, "Interpretable" -> Automatic
    ];

RewritingEvolution /: MakeBoxes[e_RewritingEvolution /; RewritingEvolutionQ[e], form : StandardForm | TraditionalForm] :=
    BoxForm`ArrangeSummaryBox[
        "RewritingEvolution", e, $evolutionIcon,
        {{BoxForm`SummaryItem[{"Steps: ", First[e]["Steps"]}]},
         {BoxForm`SummaryItem[{"States: ", Length @ First[e]["States"]}]}},
        {{BoxForm`SummaryItem[{"Events: ", Length @ First[e]["Events"]}]},
         {BoxForm`SummaryItem[{"Tokens: ", Length @ First[e]["Tokens"]}]},
         {BoxForm`SummaryItem[{"Causal arrows: ", Length @ causalArrows[First[e]]}]}},
        form, "Interpretable" -> Automatic
    ];

$systemIcon := $systemIcon = Graphics[
    {$eventColour, Disk[{0, 0}, 0.3], $stateColour, Disk[{-1, 0.8}, 0.3], Disk[{1, 0.8}, 0.3],
     Disk[{-1, -0.8}, 0.3], Disk[{1, -0.8}, 0.3],
     GrayLevel[0.55], AbsoluteThickness[1.1],
     Line[{{-1, 0.8}, {0, 0}}], Line[{{-1, -0.8}, {0, 0}}],
     Line[{{0, 0}, {1, 0.8}}], Line[{{0, 0}, {1, -0.8}}]},
    ImageSize -> Dynamic[{Automatic, 3. CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}],
    PlotRangePadding -> 0.3];

$evolutionIcon := $evolutionIcon = Graphics[
    {$stateColour, Disk[{0, 1}, 0.28],
     $eventColour, Disk[{0, 0}, 0.28], Disk[{-0.9, -1}, 0.28], Disk[{0.9, -1}, 0.28],
     $causalArrow, AbsoluteThickness[1.3], Arrowheads[0.18],
     Arrow[{{0, 0.78}, {0, 0.28}}], Arrow[{{-0.18, -0.24}, {-0.75, -0.76}}],
     Arrow[{{0.18, -0.24}, {0.75, -0.76}}]},
    ImageSize -> Dynamic[{Automatic, 3. CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]}],
    PlotRangePadding -> 0.3];


End[];

EndPackage[];

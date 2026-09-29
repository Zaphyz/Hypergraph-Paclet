(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- pictures of rules and events*)


(* ::Text:: *)
(*A rule is drawn as its input beside its output; an event as what it acted on beside what it*)
(*made, with the match highlighted on the left and the creations highlighted on the right. Both*)
(*sides share a layout, so that reading one against the other shows what changed rather than two*)
(*unrelated drawings.*)
(**)
(*This follows HighlightRule of the reference paclet, whose four option names are kept, and is*)
(*what the causal machinery uses to draw an event as a vertex of a graph.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[HypergraphRulePlot, HypergraphEventPlot];


(* ::Section:: *)
(*Usage*)


HypergraphRulePlot::usage =
    "HypergraphRulePlot[rule] draws the rewrite rule rule as its input beside its output.\n" <>
    "Vertices the two sides share are drawn in the same places, so that what the rule does to " <>
    "them can be read off.\n" <>
    "It takes the options of HypergraphPlot, together with the highlight options of " <>
    "HypergraphEventPlot.";

HypergraphEventPlot::usage =
    "HypergraphEventPlot[event] draws a rewriting event: the hypergraph it acted on with the " <>
    "match highlighted, an arrow, and the hypergraph it made with the created vertices and " <>
    "edges highlighted.\n" <>
    "HypergraphEventPlot[rule, hg] draws one picture for every way rule matches hg.\n" <>
    "The highlight styles are the options \"HighlightLeftVertexStyle\", " <>
    "\"HighlightLeftEdgeStyle\", \"HighlightRightVertexStyle\" and \"HighlightRightEdgeStyle\".";


(* ::Section:: *)
(*Messages*)


HypergraphEventPlot::event = "`1` is not a rewriting event.";
HypergraphRulePlot::rule = "`1` is not a rewrite rule.";


Begin["`Private`"];


(* ::Section:: *)
(*The arrow between the two sides*)


(* The reference paclet's arrow, so that a rule drawn here and a rule drawn there are the same
   picture. It is a plain right-pointing arrow described as a filled curve. *)
$ruleArrow = FilledCurve[
    {{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 0}}},
    {{{-1., 0.1848}, {0.2991, 0.1848}, {-0.1531, 0.6363}, {0.109, 0.8982}, {1., 0.0034},
      {0.109, -0.8982}, {-0.1531, -0.6363}, {0.2991, -0.1848}, {-1., -0.1848}, {-1., 0.1848}}}
];

arrowPanel[size_] := Graphics[{GrayLevel[0.65], $ruleArrow},
    ImageSize -> Replace[size, {Automatic -> 18, s_ ? NumericQ :> s / 8, _ -> 18}],
    PlotRangePadding -> 0.6
];


(* ::Section:: *)
(*Options*)


$highlightOptions = {
    "HighlightLeftVertexStyle" -> Directive[PointSize[0.02], Red],
    "HighlightRightVertexStyle" -> Directive[PointSize[0.02], Red],
    "HighlightLeftEdgeStyle" -> Directive[Thick, Red, EdgeForm[Directive[Red, Thick]]],
    "HighlightRightEdgeStyle" -> Directive[Thick, Red, EdgeForm[Directive[Red, Thick]]]
};

Options[HypergraphEventPlot] = Join[
    $highlightOptions,
    {"SharedCoordinates" -> True, "PanelSize" -> Automatic},
    Options[HypergraphPlot]
];

Options[HypergraphRulePlot] = Join[
    $highlightOptions,
    {"SharedCoordinates" -> True, "PanelSize" -> Automatic, "Highlight" -> True},
    Options[HypergraphPlot]
];


(* ::Section:: *)
(*Drawing one side*)


(* The options a caller may pass straight through to the two hypergraph plots, minus the ones
   this function is itself setting. *)
sidePlotOptions[opts_List] := DeleteCases[
    FilterRules[opts, Options[HypergraphPlot]],
    (Rule | RuleDelayed)[VertexStyle | EdgeStyle | VertexCoordinates | ImageSize, _]
];

sidePlot[hg_, vertexHighlights_, edgeKeys_, vStyle_, eStyle_, coords_, size_, opts_List] :=
    HypergraphPlot[hg,
        VertexStyle -> (# -> vStyle & /@ vertexHighlights),
        EdgeStyle -> (# -> eStyle & /@ edgeKeys),
        Sequence @@ If[coords === {}, {}, {VertexCoordinates -> coords}],
        Sequence @@ If[size === Automatic, {}, {ImageSize -> size}],
        Sequence @@ sidePlotOptions[opts]
    ];

(* Coordinates for the vertices of hg, to be reused by whatever is drawn next to it. A vertex the
   other side does not have is simply not mentioned, and gets laid out on its own. *)
sharedCoordinates[hg_] := Thread[VertexID /@ VertexList[hg] -> HypergraphEmbedding[hg]];


(* ::Section:: *)
(*Events*)


eventQ[ev_Association] := AllTrue[
    {"Input", "Hypergraph", "MatchEdgePositions", "MatchVertices", "NewEdges", "NewVertices"},
    KeyExistsQ[ev, #] &
];
eventQ[_] := False;

HypergraphEventPlot[ev_Association ? eventQ, opts : OptionsPattern[]] := Block[{
    o = Flatten[{opts}], host = ev["Input"], result = ev["Hypergraph"],
    size = OptionValue["PanelSize"], coords, left, right
},
    coords = If[TrueQ @ OptionValue["SharedCoordinates"], sharedCoordinates[host], {}];
    left = sidePlot[host,
        ev["MatchVertices"], ev["MatchEdgePositions"],
        OptionValue["HighlightLeftVertexStyle"], OptionValue["HighlightLeftEdgeStyle"],
        coords, size, o];
    right = sidePlot[result,
        ev["NewVertices"], createdEdgePositions[ev],
        OptionValue["HighlightRightVertexStyle"], OptionValue["HighlightRightEdgeStyle"],
        coords, size, o];
    GraphicsRow[{left, arrowPanel[size], right},
        Sequence @@ FilterRules[o, Options[GraphicsRow]],
        Spacings -> 0
    ]
];

(* Which positions of the result the rule's new edges landed in. "EdgeOrigins" says which edges
   were carried over from the host, so the rest are the created ones. Reading it off the record
   is the only reliable way: searching the result for edges equal to "NewEdges" would colour a
   carried-over edge that happens to look the same, and in a multiset that is a real case. *)
createdEdgePositions[ev_] := If[
    KeyExistsQ[ev, "EdgeOrigins"],
    Flatten @ Position[ev["EdgeOrigins"], None, {1}, Heads -> False],
    {}
];

HypergraphEventPlot[r_HypergraphRule ? HypergraphRuleQ, hg_Hypergraph ? HypergraphQ,
                    opts : OptionsPattern[]] :=
    HypergraphEventPlot[#, opts] & /@ HypergraphRuleApply[r, hg,
        FilterRules[Flatten[{opts}], Options[HypergraphRuleApply]]];

HypergraphEventPlot[ev_, ___] := (Message[HypergraphEventPlot::event, HoldForm[ev]]; $Failed);


(* ::Section:: *)
(*Rules*)


HypergraphRulePlot[r_HypergraphRule ? HypergraphRuleQ, opts : OptionsPattern[]] := Block[{
    o = Flatten[{opts}], lhs = r["Input"], rhs = r["Output"],
    size = OptionValue["PanelSize"], highlightQ = TrueQ @ OptionValue["Highlight"],
    coords, left, right
},
    coords = If[TrueQ @ OptionValue["SharedCoordinates"], sharedCoordinates[lhs], {}];
    left = sidePlot[lhs,
        If[highlightQ, r["DeletedVertices"], {}], {},
        OptionValue["HighlightLeftVertexStyle"], OptionValue["HighlightLeftEdgeStyle"],
        coords, size, o];
    right = sidePlot[rhs,
        If[highlightQ, r["NewVertices"], {}], {},
        OptionValue["HighlightRightVertexStyle"], OptionValue["HighlightRightEdgeStyle"],
        coords, size, o];
    GraphicsRow[{left, arrowPanel[size], right},
        Sequence @@ FilterRules[o, Options[GraphicsRow]],
        Spacings -> 0
    ]
];

HypergraphRulePlot[r_, ___] := (Message[HypergraphRulePlot::rule, HoldForm[r]]; $Failed);


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];

(* ::Package:: *)

(* ::Title:: *)
(*WolframInstitute`Hypergraphs` -- plotting and default formatting*)


(* ::Text:: *)
(*Ported from the reference paclet's Kernel/HypergraphPlot.m and Kernel/Formatting.m*)
(*(reference/Hypergraph), adapted to the Vertex / Edge / Hypergraph model of this package.*)
(*The layout, the colour theme and the edge rendering are deliberately the same as there.*)
(**)
(*Loaded automatically at the end of src/Hypergraph.wl.*)


BeginPackage["WolframInstitute`Hypergraphs`"];

Unprotect[Hypergraph, HypergraphPlot, HypergraphEmbedding, HypergraphLargeQ, SetHypergraphPlotThresholds];

HypergraphPlot::usage =
    "HypergraphPlot[hg] draws the hypergraph hg.\n" <>
    "A Hypergraph object displays as this plot by default, the way a Graph does.";

HypergraphEmbedding::usage =
    "HypergraphEmbedding[hg] gives the coordinates chosen for the vertices of hg, in the order " <>
    "of VertexList[hg].";

HypergraphLargeQ::usage =
    "HypergraphLargeQ[hg] gives True when hg is big enough that it displays as a summary box " <>
    "instead of a plot.";

SetHypergraphPlotThresholds::usage =
    "SetHypergraphPlotThresholds[\"MaxVertices\" -> n, \"MaxEdges\" -> m, \"MaxTotalElements\" -> k] " <>
    "sets the sizes beyond which a Hypergraph displays as a summary box rather than a plot.";


Begin["`Private`"];


(* ::Section:: *)
(*Style*)


(* The colour theme of the reference paclet.

   The reference wraps every colour in LightDarkSwitched on 14.3+, but that wrapper is only
   resolved by a live front end: rendered from a plain kernel it is an unrecognized graphics
   directive and the whole plot comes out as an error box. These are therefore the reference's
   pre-14.3 colours, which are its light-mode colours exactly, on an explicit white background
   so that the plot reads the same way under either notebook theme. *)

$nullEdgeStyle    = Directive[Opacity[.4], Hue[0.63, 0.7, 0.5]];
$binaryEdgeStyle  = Directive[Opacity[.7], Hue[0.63, 0.7, 0.5]];
$largeEdgeStyle   = Directive[Opacity[0.1], Hue[0.63, 0.66, 0.81]];
$edgeLineStyle    = Directive[Opacity[.7], Hue[0.63, 0.7, 0.5]];
$edgeLabelStyle   = Hue[0.63, 0.7, 0.5];
$vertexStyle      = Directive[Hue[0.63, 0.26, 0.89], EdgeForm[Directive[Hue[0.63, 0.7, 0.33], Opacity[0.95]]]];
$vertexLabelStyle = Black;
$plotBackground   = White;

edgeStyleForArity[n_] := Which[n === 0, $nullEdgeStyle, n === 2, $binaryEdgeStyle, True, $largeEdgeStyle];

(* Vertices are drawn exactly as in the reference: a small disk of fixed screen radius. *)
vertexPrimitive[coord_, size_] := Disk[coord, Offset[200 {size, size}]];


(* ::Section:: *)
(*The cyclic badge*)


(* The subtle distinction asked for between the three symmetry types.  Directed and unordered
   edges are drawn exactly as in the reference -- arrowheads along the edge, or none at all.
   A cyclic edge additionally carries a small circular arrow at the point where its label would
   sit, drawn at a wider radius when there is a label so that it encircles it. *)

(* The radius is in printer's points rather than plot coordinates, so that the badge is the same
   modest size whatever the extent of the embedding, and always clears the label it encircles. *)

cyclicBadge[pos_, radius_] := With[{ts = Range[0.55 Pi, 2.2 Pi, Pi / 24]},
    {
        $edgeLineStyle,
        Opacity[1],
        AbsoluteThickness[1],
        Arrowheads[{{Small, 1}}],
        Arrow @ Line[Table[Offset[radius {Cos[t], Sin[t]}, pos], {t, ts}]]
    }
];

$cyclicBadgeRadius = 7;

$cyclicBadgeLabelledRadius = 12;


(* ::Section:: *)
(*Layout helpers (ported verbatim)*)


ConcavePolygon[points_, n_ : 1] := Block[{polygon = ConvexHullRegion[points], ordering, center},
    center = RegionCentroid[polygon];
    ordering = If[MatchQ[Dimensions[points], {_, 2}], OrderingBy[points, ArcTan @@ (# - center) &], Range[Length[points]]];
    {
        BSplineCurve[With[{from = #1, diff = #2 - #1},
            MapAt[Mean[{#, center}] &, from + # diff & /@ Range[0, 1, 1 / (n + 1)], {2 ;; -2}]]] & @@@
            Partition[points[[ordering]], 2, 1, 1],
        ordering
    }
];

makeVertexLabel[label_, style_, pos_, labelOffset_ : {0, .01}] :=
    Replace[label, {
        None | Inherited -> Nothing,
        Placed[placedLabel_, offset_] :> Text[Style[placedLabel, style], pos, offset],
        l_ :> Text[Style[l, style], pos, labelOffset]
    }];


(* ::Section:: *)
(*HypergraphPlot*)


Options[HypergraphPlot] = Join[
    {
        VertexLabels -> Automatic,
        EdgeLabels -> Automatic,
        VertexSize -> 0.01,
        "EdgeArrows" -> Automatic,
        "CyclicBadges" -> True,
        GraphLayout -> Automatic
    },
    Options[Graphics]
];

(* Default labelling: show a vertex's label when it has one, and its identifier otherwise, since
   in this package the identifier is what makes the vertex what it is.  The reference paclet
   shows nothing by default; here the identifiers are what you usually want to read off. *)

resolveVertexLabel[Automatic, v_] := With[{lab = VertexLabel[v]}, If[lab === None, VertexID[v], lab]];
resolveVertexLabel[None, _] := None;
resolveVertexLabel["Name", v_] := VertexID[v];
resolveVertexLabel["Label", v_] := VertexLabel[v];
resolveVertexLabel[spec_, _] := spec;

resolveEdgeLabel[Automatic, e_] := EdgeLabel[e];
resolveEdgeLabel[None, _] := None;
resolveEdgeLabel["Name", e_] := VertexID /@ EdgeVertices[e];
resolveEdgeLabel[spec_, _] := spec;


HypergraphPlot[hg_Hypergraph ? HypergraphQ, opts : OptionsPattern[]] := Enclose @ Block[{
    vertices = VertexList[hg], edgeObjects = EdgeList[hg],
    ids, edges, symms, labels, vertexLabelSpecs,
    arrowsOpt = OptionValue["EdgeArrows"], badgesQ = TrueQ @ OptionValue["CyclicBadges"],
    vertexSize = OptionValue[VertexSize],
    nullEdges, longEdges, ws, graph,
    vertexEmbedding, edgeEmbedding,
    allPoints, bounds, corner, center, range, size,
    vertexLabelOffsets,
    totalCounts = <||>,
    arrowsQ, makeEdge, renderEdge
},
    ids = VertexID /@ vertices;
    edges = edgeIDs /@ edgeObjects;
    symms = EdgeSymmetry /@ edgeObjects;
    labels = resolveEdgeLabel[OptionValue[EdgeLabels], #] & /@ edgeObjects;
    vertexLabelSpecs = resolveVertexLabel[OptionValue[VertexLabels], #] & /@ vertices;

    (* Only a directed edge carries arrowheads along its sides.  The reference also puts them on
       cyclic edges, but then a cyclic edge differs from a directed one only by whether the last
       side happens to close the loop, which is nearly impossible to read.  Here the circular
       badge alone marks a cyclic edge, so the two are told apart at a glance. *)
    arrowsQ[symm_] := If[arrowsOpt === Automatic, symm === "Directed", TrueQ[arrowsOpt]];

    nullEdges = \[FormalN] /@ Range[Count[edges, {}]];
    longEdges = Cases[edges, {_, _, __}];
    ws = Join[ids, nullEdges, \[FormalE] /@ Range[Length[longEdges]]];

    graph = ConfirmBy[
        Graph[
            ws,
            Join[
                Annotation[DirectedEdge[##], EdgeWeight -> 1] & @@@ Cases[edges, {_, _}],
                Catenate[
                    MapIndexed[{edge, i} |->
                        With[{clickEdges = Partition[edge, 2, 1, 1], weight = Length[edge]},
                            Join[
                                Annotation[DirectedEdge[##, edge], EdgeWeight -> 1] & @@@ clickEdges,
                                If[ DuplicateFreeQ[edge],
                                    Annotation[DirectedEdge[#, \[FormalE] @@ i], EdgeWeight -> weight] & /@ edge,
                                    {}
                                ]
                            ]
                        ],
                        Catenate @ Replace[Gather[longEdges], {edge_, ___} /; ! DuplicateFreeQ[edge] :> {edge}, {1}]
                    ]
                ]
            ],
            VertexShapeFunction -> ((Sow[#2 -> #1, "v"]; Point[#1]) &),
            EdgeShapeFunction -> ((Sow[#2 -> #1, "e"]; GraphComputation`GraphElementData["Line"][#1, None]) &),
            VertexLabels -> {_ -> Automatic, \[FormalE][_] -> None},
            (* The reference nominally asks for {"SpringEmbedding", "EdgeWeighted" -> True}, but
               it also passes GraphLayout -> Automatic inherited from Options[Graph] ahead of
               that, and the first setting of an option is the one WL uses. So what the reference
               actually draws with is the default layout, which keeps each large edge's helper
               vertex inside the edge instead of flinging it out along its heavier spokes. This
               follows the reference's real behaviour; "SpringEmbedding" is still selectable. *)
            GraphLayout -> OptionValue[GraphLayout]
        ],
        GraphQ
    ];

    {vertexEmbedding, edgeEmbedding} = First[#, {}] & /@ Reap[ConfirmMatch[GraphPlot[graph], _Graphics], {"v", "e"}][[2]];
    edgeEmbedding = Join[Merge[edgeEmbedding, Identity], Association[vertexEmbedding][[Key /@ nullEdges]]];
    vertexEmbedding = Association[vertexEmbedding];

    (* Re-order the vertices of a large edge around its centroid so the drawn boundary does not
       self-intersect. *)
    With[{vertexRearange = Association @ Catenate[
            Block[{points = Lookup[vertexEmbedding, #], center, ordering},
                center = Mean[points];
                ordering = OrderingBy[points, ArcTan @@ (# - center) &];
                ordering = First @ MaximalBy[
                    Catenate[{RotateLeft[ordering, #], RotateLeft[Reverse[ordering], #]} & /@ Range[Length[ordering]]],
                    Count[MapIndexed[#1 == #2[[1]] &, #], True] &, 1];
                Thread[Part[#, ordering] -> #]
            ] & /@ Select[edges, DuplicateFreeQ[#] && Length[#] > 3 &]
        ]
    },
        vertexEmbedding = Merge[{KeyMap[Replace[vertexRearange]] @ vertexEmbedding, vertexEmbedding}, First];
        edgeEmbedding = Association @ KeyValueMap[
            #1 -> If[ MatchQ[#2, {__Real}], #2,
                ReplacePart[#2, Thread[{{_, 1}, {_, -1}} -> Lookup[vertexEmbedding, Extract[#1, {{1}, {2}}]]]]] &
        ] @ edgeEmbedding;
    ];

    vertexEmbedding = vertexEmbedding[[Key /@ ids]];

    (* The plot range must be measured from what is actually drawn.  Every large edge also has a
       helper vertex in the layout graph, joined to each of its vertices by a spoke; neither the
       helper nor its spokes are ever drawn, and including them leaves a band of empty space on
       whichever side the helper happened to land. *)
    allPoints = DeleteDuplicates @ DeleteMissing @ Join[
        Values[vertexEmbedding],
        Catenate[If[MatchQ[#, {__Real}], {#}, Flatten[#, 1]] & /@
            Values @ KeySelect[edgeEmbedding, FreeQ[#, \[FormalE]] &]]
    ];
    bounds = CoordinateBounds[allPoints];
    (* A hypergraph whose drawing is exactly flat in one direction -- a single binary edge, say --
       would otherwise have a zero-width plot range.  The reference opens it out to a full square;
       a sixth of the other direction is enough to keep the range valid without stranding the
       drawing in the middle of a mostly empty frame. *)
    bounds = With[{diff = Abs[#2 - #1] & @@@ bounds}, {range = Max[diff]},
        MapThread[If[Chop[#1] == 0, #2 + range {-1, 1} / 6, #2] &, {diff, bounds}]];
    corner = bounds[[All, 1]];
    center = Mean /@ bounds;
    range = #2 - #1 & @@@ bounds;
    size = Min[range];
    If[size == 0, size = 1];
    vertexLabelOffsets = - 2 Normalize[Mean[Threaded[#] - Nearest[allPoints, #, 5]]] & /@ vertexEmbedding;

    makeEdge[edge_, label_, symm_, i_, initPrimitive_, lines_ : {}] := Block[{
        primitive = Chop @ If[RegionQ[initPrimitive],
            DiscretizeRegion[#, MaxCellMeasure -> 0.1] &,
            DiscretizeGraphics @* ReplaceAll[Arrow[l_] :> l]] @ initPrimitive,
        pos, labelPrimitive
    },
        (* RegionCentroid can fail on a FilledCurve of B-splines, in which case the reference
           leaves pos symbolic and every label and badge lands nowhere. Fall back to the mean of
           the edge's own vertices. *)
        pos = Replace[
            Quiet @ RegionCentroid[primitive /. Offset[r_] :> r],
            Except[{_ ? NumericQ, _ ? NumericQ}] :> Replace[
                Mean @ DeleteMissing @ Lookup[vertexEmbedding, edge],
                Except[{_ ? NumericQ, _ ? NumericQ}] :> corner
            ]
        ];
        If[ Length[edge] == 2,
            pos += With[{points = Sort[MeshCoordinates[
                    If[RegionQ[primitive], DiscretizeRegion, DiscretizeGraphics][primitive, MaxCellMeasure -> .1]]][[{1, -1}]]},
                0.03 size Normalize[
                    If[TrueQ[VectorAngle[#, pos - center] > Pi], #, - #] & [
                        Subtract @@ RotationTransform[Pi / 2, pos][points]]]
            ]
        ];
        If[Length[edge] == 1, pos += 0.03 size];
        labelPrimitive = Replace[label, {
            None | Inherited -> {},
            Placed[placedLabel_, offset_] :> Text[Replace[placedLabel, None -> ""], pos, offset],
            l_ :> Text[l, pos]
        }];
        primitive = If[ arrowsQ[symm],
            {initPrimitive,
             MapIndexed[{Arrowheads[{{Replace[#2[[1]], {1 -> Small, 2 -> Medium, _ -> Large}], .5}}],
                         $edgeLineStyle, Arrow[#1]} &, lines]},
            initPrimitive
        ] /. _EmptyRegion -> {};
        Sow[pos, "EdgeLabelPosition"];
        {
            edgeStyleForArity[Length[edge]],
            primitive,
            Opacity[1],
            Replace[labelPrimitive, Text[expr_, args___] :> Text[Style[expr, $edgeLabelStyle], args]],
            If[ badgesQ && symm === "Cyclic",
                cyclicBadge[pos, If[label === None, $cyclicBadgeRadius, $cyclicBadgeLabelledRadius]],
                Nothing
            ]
        }
    ];

    renderEdge[{edge_List, label_} -> {mult_Integer, total_Integer : 0}, i_Integer, j_Integer] := Block[{
        emb = Replace[edge, Normal[vertexEmbedding], {1}], primitive, addArrows, symm = symms[[i]]
    },
        Switch[
            Length[edge],
            0 | 1, Block[{r, dr = size 0.01},
                r = size 0.03 + If[Length[edge] > 0, (j - 1) * dr, 0];
                primitive = Switch[Length[edge],
                    0, Circle[Lookup[edgeEmbedding, \[FormalN][j]], Offset[400 r / size]],
                    1, Disk[First[emb], Offset[400 r / size]]
                ];
                makeEdge[edge, label, symm, i, primitive]
            ],
            2, Block[{points = Lookup[edgeEmbedding, DirectedEdge @@ edge], curve},
                curve = With[{c = Lookup[totalCounts, Key[#], 0] + 1},
                    AppendTo[totalCounts, # -> c];
                    GraphComputation`GraphElementData["Line"][points[[c]], None][[1]] /. BezierCurve -> BSplineCurve
                ] & @ edge;
                primitive = If[arrowsQ[symm], Arrow, Identity] @
                    If[DuplicateFreeQ[edge] && total == 1, MapAt[#[[{1, -1}]] &, curve, {1}], curve];
                {Opacity[1], Arrowheads[{{Small, .5}}], makeEdge[edge, label, symm, i, primitive]}
            ],
            _, Block[{coords = Lookup[vertexEmbedding, edge], counts = <||>, points, curves, ordering, lines = {}},
                If[ DuplicateFreeQ[edge],
                    With[{c = Lookup[totalCounts, Key[#], 0] + 1},
                        AppendTo[totalCounts, # -> c];
                        {curves, ordering} = ConcavePolygon[coords, c];
                        lines = Prepend[curves[[1]]][Insert[#[[2]], #[[1, 1, -1]], {1, 1}] & /@ Partition[curves, 2, 1]];
                        With[{part = Partition[ordering, 2, 1, 1]},
                            lines = If[symm === "Cyclic", MapAt[Reverse, {-1, 1}], Most] @ Map[
                                With[{pos = FirstPosition[part, # | Reverse[#], {1}, Heads -> False]},
                                    If[OrderedQ[Extract[part, pos]], Identity, MapAt[Reverse, 1]] @ Extract[lines, pos]
                                ] &,
                                Partition[Range[Length[ordering]], 2, 1, 1]
                            ];
                        ]
                    ] & @ Sort[edge]
                    ,
                    points = With[{c = Lookup[counts, #, 0] + 1},
                        AppendTo[counts, # -> c];
                        Lookup[edgeEmbedding, #][[c]]
                    ] & /@ (DirectedEdge[##, edge] & @@@ Partition[edge, 2, 1, 1]);
                    points = MapAt[ScalingTransform[ConstantArray[1 + Log10[j], 2], Mean[#]], #, {2 ;; -2}] & /@ points;
                    curves = Catenate[GraphComputation`GraphElementData["Line"][#, None] /. BezierCurve -> BSplineCurve & /@ points];
                    lines = Insert[#[[2]], #[[1, 1, -1]], {1, 1}] & /@
                        If[symm === "Cyclic", Identity, Most] @ Partition[curves, 2, 1, -1];
                ];
                (* The bare filled curve is what makeEdge needs; it adds the arrows itself from
                   the boundary lines, and would otherwise receive them twice. *)
                makeEdge[edge, label, symm, i, FilledCurve[curves], lines]
            ]
        ]
    ];

    Graphics[{
        Opacity[.5],
        Arrowheads[{{Medium, .5}}],
        AbsoluteThickness[Medium],
        Block[{edgesWithLabels, counts, counter = <||>},
            edgesWithLabels = Thread[{edges, labels}];
            counts = Merge[
                {Counts[edgesWithLabels], First[#] -> Length[#] & /@ GatherBy[edgesWithLabels, First /* Sort]},
                Identity];
            MapIndexed[{edgeWithLabel, i} |-> With[{j = Lookup[counter, Key[edgeWithLabel[[1]]], 0] + 1},
                    counter[edgeWithLabel[[1]]] = j;
                    renderEdge[edgeWithLabel -> counts[edgeWithLabel], i[[1]], j]
                ],
                edgesWithLabels
            ]
        ],
        Opacity[1],
        {$vertexStyle, vertexPrimitive[#, vertexSize] & /@ Values[vertexEmbedding]},
        MapThread[{coord, label, offset} |-> (
            Sow[coord, "Vertex"];
            makeVertexLabel[label, $vertexLabelStyle, coord, Take[offset, UpTo[2]]]
        ), {Values[vertexEmbedding], vertexLabelSpecs, Lookup[vertexLabelOffsets, Keys[vertexEmbedding]]}]
    },
        FilterRules[{opts}, Options[Graphics]],
        PlotRange -> bounds,
        (* Vertex labels are text, whose extent Graphics does not fold into the plot range, so
           the reference reserves room for them with PlotRangePadding -> Scaled[.2].  That pads
           in coordinate space, which leaves the drawing floating in a fifth of the frame.
           Reserving the room in screen space instead lets the drawing fill the frame while the
           labels still have somewhere to sit. *)
        PlotRangePadding -> Scaled[.02],
        (* ImagePadding -> All does not reserve room for Text primitives, and drops the outermost
           labels altogether; a fixed margin wide enough for a label of a few characters does. *)
        ImagePadding -> 20,
        Background -> $plotBackground
    ]
];

HypergraphPlot[hg_Hypergraph, opts : OptionsPattern[]] /; ! HypergraphQ[hg] := $Failed;


HypergraphEmbedding[hg_Hypergraph ? HypergraphQ, opts : OptionsPattern[HypergraphPlot]] :=
    First[Reap[HypergraphPlot[hg, opts], "Vertex"][[2]], {}];


(* ::Section:: *)
(*Default formatting*)


$HypergraphSummaryThresholds = <|
    "MaxVertices" -> 32,
    "MaxEdges" -> 128,
    "MaxTotalElements" -> 196
|>;

SetHypergraphPlotThresholds[rules___Rule] := AssociateTo[$HypergraphSummaryThresholds, {rules}];

HypergraphLargeQ[hg_Hypergraph ? HypergraphQ] := With[{
    vertexCount = VertexCount[hg], edgeCount = EdgeCount[hg]
},
    vertexCount > $HypergraphSummaryThresholds["MaxVertices"] ||
    edgeCount > $HypergraphSummaryThresholds["MaxEdges"] ||
    (vertexCount + edgeCount) > $HypergraphSummaryThresholds["MaxTotalElements"]
];
HypergraphLargeQ[___] := False;

$HypergraphIcon := $HypergraphIcon = Quiet @ HypergraphPlot[
    Hypergraph[{{1, 2, 3}, {3, 4, 5, 6}, {5, 2}, {3, 4}, {1, 2}, {6, 1}, {2}}],
    VertexLabels -> None, Background -> None
];

(* A Hypergraph displays as its plot, the way a Graph does.  The plot is wrapped in a
   NamespaceBox holding the original expression, so that the displayed graphic is still the
   hypergraph: copying it or feeding it back to the kernel recovers the object. *)

Hypergraph /: MakeBoxes[hg_Hypergraph /; HypergraphQ[hg], form : StandardForm] :=
    If[ HypergraphLargeQ[hg],
        BoxForm`ArrangeSummaryBox[
            "Hypergraph",
            hg,
            $HypergraphIcon,
            {
                {BoxForm`SummaryItem[{"Vertices: ", VertexCount[hg]}]},
                {BoxForm`SummaryItem[{"Edges: ", EdgeCount[hg]}]}
            },
            {
                {BoxForm`SummaryItem[{"Max arity: ", Max[Append[hg["Arity"], 0]]}]},
                {BoxForm`SummaryItem[{"Edge symmetries: ", CountDistinct[hg["EdgeSymmetries"]]}]}
            },
            form,
            "Interpretable" -> Automatic
        ],
        With[{plot = Quiet @ HypergraphPlot[hg]},
            If[ Head[plot] === Graphics,
                (* hypergraphBox holds its argument, so the boxes have to be computed first. *)
                With[{boxes = Block[{BoxForm`$UseTextFormattingWhenConvertingInput = False},
                        ToBoxes[plot, form]]},
                    hypergraphBox[boxes, hg]
                ],
                (* If the layout failed for any reason, fall back to the literal expression
                   rather than showing nothing. *)
                RowBox[{"Hypergraph", "[", ToBoxes[VertexList[hg], form], ",",
                        ToBoxes[EdgeList[hg], form], "]"}]
            ]
        ]
    ];

SetAttributes[hypergraphBox, HoldAllComplete];
hypergraphBox[GraphicsBox[box_, opts___], hg_] :=
    GraphicsBox[NamespaceBox["Hypergraphs", DynamicModuleBox[{Typeset`hg = HoldComplete[hg]}, box]], opts];
hypergraphBox[boxes_, _] := boxes;

PossibleHypergraphBoxQ[HoldPattern[GraphicsBox[NamespaceBox["Hypergraphs", _, ___], ___]]] := True;
PossibleHypergraphBoxQ[___] := False;

FromHypergraphBox[HoldPattern[GraphicsBox[NamespaceBox["Hypergraphs", DynamicModuleBox[vars_, ___], ___], ___]], _] :=
    Module[vars, Typeset`hg];

Unprotect[GraphicsBox];
With[{lhs = HoldPattern[MakeExpression[g_GraphicsBox ? PossibleHypergraphBoxQ, fmt_]]},
    If[ ! KeyExistsQ[FormatValues[GraphicsBox], lhs],
        PrependTo[FormatValues[GraphicsBox], lhs :> FromHypergraphBox[g, fmt]]
    ]
];
Protect[GraphicsBox];


(* Edges and vertices keep their literal form; only the hypergraph as a whole becomes a picture. *)


End[];

Protect["WolframInstitute`Hypergraphs`*"];

EndPackage[];

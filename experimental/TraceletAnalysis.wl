(* ::Package:: *)

(* ::Title:: *)
(*Tracelet analysis*)


(* ::Text:: *)
(*After Nicolas Behr, "Tracelets and Tracelet Analysis Of Compositional Rewriting Systems",*)
(*ACT 2019, EPTCS 323, 44-71 (arXiv:1904.12829), and the compositional rewriting theory around*)
(*it (Behr, Harmer and Krivine, "Fundamentals of Compositional Rewriting Theory"; Behr and Kock,*)
(*"Tracelet Hopf Algebras and Decomposition Spaces").*)
(**)
(*A tracelet is a derivation trace stripped of its context: the n steps, and nothing of the*)
(*hypergraph they ran in except what they actually used. Behr's characterisation theorem says*)
(*that every length-n derivation trace is a tracelet applied to some ambient object, so the*)
(*tracelet is what two traces have in common when they are "the same derivation happening*)
(*somewhere else". Its interfaces are the input, which the trace needs given to it, and the*)
(*output, which it leaves behind; by the concurrency theorem the pair is a single rule doing the*)
(*work of all n steps.*)
(**)
(*In adhesive categories all of that is pushouts and spans of monomorphisms. Here it is tokens.*)
(*The rewriting stack in RewritingSystems.wl already gives every hyperedge an identity, with the*)
(*event that created it and the events that consumed it, and that is exactly the data the*)
(*categorical account constructs:*)
(**)
(*  - a token consumed by the trace and created by none of its steps is an input: it had to be*)
(*    there already. These are the input interface.*)
(*  - a token created by one step and consumed by a later one is an admissible match gluing the*)
(*    two steps together. These are the overlaps, and they are the causal order.*)
(*  - a token created and never consumed is an output. These are the output interface.*)
(*  - everything else in the ambient state is context, and context is what a tracelet drops.*)
(**)
(*Load after RewritingSystems.wl.*)


BeginPackage["WolframInstitute`HypergraphRewriting`", {"WolframInstitute`Hypergraphs`"}];

Unprotect[TraceletAnalysis];
ClearAll[TraceletAnalysis];


TraceletAnalysis::usage =
    "TraceletAnalysis[ev] gives the tracelets of the derivation traces of a RewritingEvolution: \
each trace in minimal context, with the steps that remain, what they need, what they leave, and \
how they depend on one another.\n\
TraceletAnalysis[ev, n] takes the traces of n consecutive steps; TraceletAnalysis[sys, n] \
evolves the system n steps first; TraceletAnalysis[ev, {k1, k2, ...}] takes the one trace made \
of those events.\n\
Traces that differ only by where they happened give the same tracelet, and are reported once \
with a multiplicity.";

TraceletAnalysis::ev = "`1` is not a rewriting evolution or system.";
TraceletAnalysis::len = "`1` is not a positive trace length.";
TraceletAnalysis::seq = "`1` is not a sequence of events running one after another.";


Begin["`Private`"];


Options[TraceletAnalysis] = {
    "Minimal" -> True,
    "Causal" -> All,
    "Diagrams" -> True,
    "PanelSize" -> 90
};


(* ::Section:: *)
(*Which traces to look at*)


(* The events on the branch from the root down to a state, in order. The evolution is a tree, so
   there is exactly one. *)
branchEvents[states_, s_] := Module[{path = {}, k = s},
    While[states[[k, "Parent"]] =!= None,
        PrependTo[path, states[[k, "InEvent"]]];
        k = states[[k, "Parent"]]];
    path
];

(* Every run of n consecutive steps, which is what a tracelet is of. A window rather than a whole
   history: a tracelet of length two in the middle of a long derivation is as much a tracelet as
   one at the start. *)
windows[states_, n_] := DeleteDuplicates @ Table[
    With[{path = branchEvents[states, s]},
        If[Length[path] >= n, Take[path, -n], Nothing]],
    {s, Length[states]}];

maximalTraces[states_] := DeleteDuplicates @ Select[
    branchEvents[states, #] & /@ Select[Range[Length[states]], leafQ[states, #] &],
    # =!= {} &];

leafQ[states_, s_] := ! MemberQ[states[[All, "Parent"]], s];


(* ::Section:: *)
(*One tracelet*)


(* The whole construction. Everything below is read off the tokens; nothing is searched for. *)

tracelet[d_, es_List, o_List] := Module[{
    states = d["States"], events = d["Events"], steps,
    consumed, created, input, output, internal, relevant, touched,
    first, along, inHg, outHg, trace, order, overlaps, n = Length[es]
},
    steps = events[[#]] & /@ es;

    consumed = Union @@ (#["Consumed"] & /@ steps);
    created = Union @@ (#["Created"] & /@ steps);
    input = Complement[consumed, created];
    output = Complement[created, consumed];
    internal = Intersection[consumed, created];
    relevant = Union[consumed, created];

    (* A rule may need a vertex without needing an edge on it -- a rule whose whole input is one
       labelled vertex is the standard example -- so the interfaces carry the vertices the steps
       touched as well as the edges. Dropping them would make the composite rule wrong. *)
    touched = Union @@ (Join[#["Match", "MatchVertices"], #["Match", "NewVertices"]] & /@ steps);

    first = First[steps]["InputState"];
    along = Prepend[#["OutputState"] & /@ steps, first];

    inHg = restrict[states[[first]], input, touched, o];
    outHg = restrict[states[[Last[along]]], output, touched, o];
    trace = restrict[states[[#]], relevant, touched, o] & /@ along;

    (* Each internal token glues the step that made it to the step that used it. That is the
       admissible match of the categorical account, and the arrow of the causal order. *)
    order = DeleteDuplicates @ Flatten @ Table[
        With[{from = FirstPosition[#["Created"] & /@ steps, t, None, {2}],
              to = FirstPosition[#["Consumed"] & /@ steps, t, None, {2}]},
            If[from === None || to === None, Nothing, DirectedEdge[First[from], First[to]]]],
        {t, internal}];

    overlaps = KeyValueMap[
        <|"From" -> First[#1], "To" -> Last[#1],
          "Tokens" -> #2, "Edges" -> (d["Tokens"][#, "Edge"] & /@ #2)|> &,
        GroupBy[internal,
            With[{from = FirstPosition[#["Created"] & /@ steps, #2, None, {2}],
                  to = FirstPosition[#["Consumed"] & /@ steps, #2, None, {2}]},
                {First[from], First[to]}] & [Null, #] &]];

    <|
        "Length" -> n,
        "Rules" -> (#["Rule"] & /@ steps),
        "Composite" -> Quiet @ HypergraphRule[inHg, outHg],
        "Input" -> inHg,
        "Output" -> outHg,
        "Trace" -> trace,
        "Overlaps" -> overlaps,
        "CausalOrder" -> Graph[Range[n], order,
            VertexLabels -> Automatic, GraphLayout -> "LayeredDigraphEmbedding",
            VertexStyle -> Hue[0.11, 0.80, 0.95], EdgeStyle -> Hue[0.00, 0.85, 0.75],
            VertexSize -> Medium, ImageSize -> Small, Background -> White],
        "Causal" -> (n === 1 || ConnectedGraphQ @ UndirectedGraph @ Graph[Range[n], order]),
        "Linearisations" -> linearisations[n, order],
        "Events" -> es
    |>
];

(* A state with everything the trace did not touch taken away. With "Minimal" -> False the state
   is left whole, which is the same trace read in its context rather than on its own. *)
restrict[st_, keep_, touched_, o_List] := If[
    TrueQ @ OptionValue[TraceletAnalysis, o, "Minimal"],
    Quiet @ Hypergraph[
        Select[VertexList[st["Hypergraph"]], MemberQ[touched, VertexID[#]] &],
        Pick[EdgeList[st["Hypergraph"]], MemberQ[keep, #] & /@ st["Tokens"]]],
    st["Hypergraph"]
];

(* How many orders the steps could have been taken in: the linear extensions of the causal order.
   Behr's shift equivalence, counted. One means the trace is forced; n! means the steps are
   pairwise independent and the order was an accident of the run. *)
linearisations[n_, order_] := If[
    n > 7,
    Missing["TooLong"],
    With[{closure = If[order === {}, {}, EdgeList @ TransitiveClosureGraph @ Graph[Range[n], order]]},
        Count[Permutations[Range[n]],
            perm_ /; AllTrue[closure,
                Order[FirstPosition[perm, First[#]], FirstPosition[perm, Last[#]]] === 1 &]]]
];


(* ::Section:: *)
(*Telling two tracelets apart*)


(* Behr's abstraction equivalence: two tracelets are the same when an isomorphism carries one to
   the other. The invariant used here is the canonical form of the input and the output taken
   together, as one hypergraph with each edge marked by the side it is on -- together so that how
   the two interfaces share their vertices is part of the invariant, and marked so that an input
   edge is never mistaken for an output one -- along with the sequence of rules and the causal
   order. *)

abstractionKey[t_] := {
    t["Rules"],
    Sort[List @@@ EdgeList[t["CausalOrder"]]],
    Quiet @ CanonicalHypergraph @ sided[t["Input"], t["Output"]],
    labelSignature[t["Input"]], labelSignature[t["Output"]]
};

(* The labels themselves, not just the pattern of which elements share one.

   CanonicalHypergraph keeps only the pattern: a label is replaced by a placeholder ranked by
   which elements carry it, because that is all that survives a renaming of a hypergraph. For a
   tracelet that is too coarse. A label here is often something the derivation computed -- the
   composite of two arrows is labelled after the two it consumed -- and a trace that ends in
   ((a b) c) is not the same derivation as one that ends in (a (b c)), though the two outputs
   are isomorphic as labelled hypergraphs in the package's sense. So the label values go into
   the invariant as well, which can only tell more tracelets apart, never fewer. *)
labelSignature[hg_] := {
    Sort[EdgeLabel /@ EdgeList[hg]],
    Sort[VertexLabel /@ VertexList[hg]]
};

sided[inHg_, outHg_] := Quiet @ Hypergraph[
    Join[VertexList[inHg], VertexList[outHg]],
    Join[
        Edge[EdgeVertices[#], EdgeSymmetry[#], {"in", EdgeLabel[#]}] & /@ EdgeList[inHg],
        Edge[EdgeVertices[#], EdgeSymmetry[#], {"out", EdgeLabel[#]}] & /@ EdgeList[outHg]]
];


(* ::Section:: *)
(*The picture*)


(* A tracelet is a trace, so it is drawn as one: the states it passes through, in minimal
   context, with the rule that fired between each pair. *)

traceletDiagram[t_, o_List] := Module[{size = OptionValue[TraceletAnalysis, o, "PanelSize"], frames},
    frames = Riffle[
        framedPlot[#, size] & /@ t["Trace"],
        Style[#, GrayLevel[0.45], 10] & /@ (Row[{"\[LongRightArrow]  r", #}] & /@ t["Rules"])];
    Row[frames, Spacer[5]]
];

framedPlot[hg_, size_] := Framed[
    HypergraphPlot[hg, VertexLabels -> None, EdgeLabels -> Automatic,
        AspectRatio -> Automatic, ImageSize -> size, ImagePadding -> 8, Background -> None],
    Background -> White, FrameStyle -> GrayLevel[0.78], FrameMargins -> 0, RoundingRadius -> 3];


(* ::Section:: *)
(*The function*)


TraceletAnalysis[sys_RewritingSystem ? RewritingSystemQ, n_Integer, o : OptionsPattern[]] :=
    TraceletAnalysis[sys[n], n, o];

TraceletAnalysis[ev_RewritingEvolution ? RewritingEvolutionQ, o : OptionsPattern[]] :=
    report[First[ev], maximalTraces[First[ev]["States"]], Flatten[{o}]];

TraceletAnalysis[ev_RewritingEvolution ? RewritingEvolutionQ, n_Integer, o : OptionsPattern[]] :=
    If[ n < 1,
        Message[TraceletAnalysis::len, n]; $Failed,
        report[First[ev], windows[First[ev]["States"], n], Flatten[{o}]]
    ];

TraceletAnalysis[ev_RewritingEvolution ? RewritingEvolutionQ, es : {__Integer},
                 o : OptionsPattern[]] :=
    If[ ! consecutiveQ[First[ev], es],
        Message[TraceletAnalysis::seq, es]; $Failed,
        report[First[ev], {es}, Flatten[{o}]]
    ];

TraceletAnalysis[other_, ___] := (Message[TraceletAnalysis::ev, HoldForm[other]]; $Failed);

(* A trace is a run of steps each acting on what the one before it made. *)
consecutiveQ[d_, es_] := With[{evs = d["Events"]},
    AllTrue[es, 1 <= # <= Length[evs] &] &&
    AllTrue[Partition[es, 2, 1], evs[[#[[1]], "OutputState"]] === evs[[#[[2]], "InputState"]] &]
];

report[d_, traces_, o_List] := Module[{all, grouped, out},
    all = tracelet[d, #, o] & /@ traces;
    If[ OptionValue[TraceletAnalysis, o, "Causal"] === True,
        all = Select[all, TrueQ[#["Causal"]] &]];
    grouped = GroupBy[all, abstractionKey];
    out = KeyValueMap[
        Function[{key, ts},
            Join[
                KeyDrop[First[ts], "Events"],
                <|"Multiplicity" -> Length[ts], "Occurrences" -> (#["Events"] & /@ ts)|>]],
        grouped];
    out = SortBy[out, {#["Length"] &, - #["Multiplicity"] &, ! TrueQ[#["Causal"]] &}];
    If[ TrueQ @ OptionValue[TraceletAnalysis, o, "Diagrams"],
        out = Append[#, "Diagram" -> traceletDiagram[#, o]] & /@ out];
    out
];


End[];

EndPackage[];

(* ::Package:: *)

(* ::Input:: *)
(*PacletInstall["https://wolfr.am/Hypergraph.paclet"]*)
(*<<WolframInstitute`Hypergraph`*)


(* ::Chapter:: *)
(*Hypergraph Algebras*)


(* ::Input:: *)
(*OnePassRewriting[rule_,hypergraph_]:=Hypergraph[VertexList[hypergraph],Flatten/@rule[hypergraph][[All,"NewEdges"]],*)
(*VertexCoordinates->Thread[VertexList[hypergraph]->HypergraphEmbedding[hypergraph]]]*)


(* ::Input:: *)
(*HypergraphRule[Hypergraph[{1, 2, 3}, Hyperedges[{1, 2}, {2, 3}], "EdgeSymmetry" -> {All -> "Directed"}, *)
(*   VertexLabels -> {All -> Automatic}], Hypergraph[{1, 2, 3}, Hyperedges[{1, 3}],"EdgeSymmetry"->{All->"Directed"},VertexLabels -> {All -> Automatic}]]*)


(* ::Input:: *)
(*RuleToOperation[rule_]:=With[{arity=Length[EdgeList[rule[[1]]]],coarity=Length[EdgeList[rule[[2]]]]},*)
(*With[{args=Array[Slot,arity]},*)
(*HyperCompose[rule][args]*)
(*]*)
(*]*)


(* ::Input:: *)
(*RuleToOperation[WolframInstitute`Hypergraph`HypergraphRule[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], "EdgeSymmetry" -> {All -> "Directed"}, VertexLabels -> {All -> Automatic}], WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 3}]]]]*)


(* ::Input:: *)
(*SetEdgeLabel[label_,hyp_,edge_]:=Hypergraph[hyp,EdgeLabels->{edge->label}]*)
(*HyperCompose[rule_][args_]:=If[ResourceFunction["AllSameBy"][args,VertexCount],*)
(*If[Length[EdgeList[rule[[1]]]]==Length[args],*)
(*With[{vars=ToExpression/@Alphabet[],largs=MapThread[SetEdgeLabel[#1,#2,All]&,{Array[\[FormalA],Length[args]],args}]},*)
(*With[{lrule=HypergraphRule[Hypergraph[rule[[1]],EdgeLabels->Thread[EdgeList[rule[[1]]]->Table[vars[[i]],{i,Length[EdgeList[test3rule[[1]]]]}]]],rule[[2]]],lhyp=Plus@@largs},*)
(*OnePassRewriting[lrule,lhyp]*)
(*]*)
(*],*)
(*"Non-conformable number of arguments and input edges."*)
(*],*)
(*"Hypergraph arguments defined on different node sets."*)
(*]*)


(* ::Input:: *)
(*test3rule=HypergraphRule[Hypergraph[{1,2,3,4},{{1,4},{2,4},{3,4}},VertexLabels->Automatic],Hypergraph[{1,2,3,4},{{1},{2},{3}}],VertexLabels->Automatic]*)


(* ::Input:: *)
(*testlrule=HypergraphRule[Hypergraph[test3rule[[1]],EdgeLabels->Thread[EdgeList[test3rule[[1]]]->Table[vars[[i]],{i,Length[EdgeList[test3rule[[1]]]]}]]],test3rule[[2]]]*)


(* ::Input:: *)
(*hyp1=Hypergraph[{{1},{1,2},{2,3,4},{2}},VertexLabels->Automatic];*)
(*hyp2=Hypergraph[{{1,4},{4,3},{2,4}},VertexLabels->Automatic];*)
(*hyp3=Hypergraph[{{2,3},{1,2,3},{2,3,4}},VertexLabels->Automatic];*)


(* ::Input:: *)
(*HyperCompose[test3rule][{hyp1,hyp2,hyp3}]*)


(* ::Input:: *)
(*RuleToOperation[test3rule][{hyp1,hyp2,hyp3}]*)


(* ::Input:: *)
(*OnePassRewriting[test3rule,Plus@@{hyp1,hyp2,hyp3}]*)

(* ::Package:: *)

PacletInstall["https://wolfr.am/Hypergraph.paclet",ForceVersionInstall -> True]
<<WolframInstitute`Hypergraph`


CloudGet[CloudObject["https://www.wolframcloud.com/obj/nikm/EinsteinSummation"]]


IndexContract[arraylist_,indices_]:=EinsteinSummation[indices,arraylist]


BM[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ijp,ipk,pjk->ijk"]
Blades[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ipq,pjq,pqk->ijk"]
TriForce[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ipq,pjr,qrk->ijk"]
Fish[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ijp,qrp,qrk->ijk"]
Boat[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ipq,pjq,pjk->ijk"]


(* ::Input:: *)
(*IndexContract[{a,b},{{1,2},{2,3}}->{1,3}]==IndexContract[{a,b},"ij,jk->ik"]*)


RandomElement[values_][arg_]:=Switch[values,"Boolean",FiniteField[2,1][RandomInteger[]],"Integer",RandomInteger[{-2,2}],"Complex",SetPrecision[RandomComplex[{-1.5-1.5I,1.5+1.5I}],2]]


GenerateArray[entry_,order_,dimension_]:=Array[entry,ConstantArray[dimension,order]]
GenerateRandomArray[values_,order_,dimension_]:=If[order===0,RandomElement[values][_],GenerateArray[RandomElement[values][List[#]]&,order,dimension]]
GenerateRandomArrays[n_,values_,order_,dimension_]:=Table[GenerateRandomArray[values,order,dimension],n]
GenerateSymmetricArray[entry_,order_,dimension_]:=Normal@SymmetrizedArray[pos_ :> entry[pos], ConstantArray[dimension,order],Symmetric[Range[order]]]
GenerateRandomSymmetricArray[values_,order_,dimension_]:=If[order===0,RandomElement[values][_],GenerateSymmetricArray[RandomElement[values][List[#]]&,order,dimension]]
GenerateRandomSymmetricArrays[n_,values_,order_,dimension_]:=Table[GenerateRandomSymmetricArray[values,order,dimension],n]


(* ::Input:: *)
(*p=GenerateRandomArray["Integer",1,3];*)
(*q=GenerateRandomArray["Integer",1,3];*)
(*r=GenerateRandomArray["Integer",1,3];*)
(*a=GenerateRandomArray["Integer",2,3];*)
(*b=GenerateRandomArray["Integer",2,3];*)
(*c=GenerateRandomArray["Integer",2,3];*)
(*d=GenerateRandomArray["Integer",2,3];*)
(*e=GenerateRandomArray["Integer",2,3];*)
(*x=GenerateRandomArray["Integer",3,3];*)
(*y=GenerateRandomArray["Integer",3,3];*)
(*z=GenerateRandomArray["Integer",3,3];*)
(*u=GenerateRandomArray["Integer",3,3];*)
(*v=GenerateRandomArray["Integer",3,3];*)
(*w=GenerateRandomArray["Integer",3,3];*)
(*\[Alpha]=GenerateRandomSymmetricArray["Integer",3,3];*)
(*\[Beta]=GenerateRandomSymmetricArray["Integer",3,3];*)
(*\[Gamma]=GenerateRandomSymmetricArray["Integer",3,3];*)
(*\[Delta]=GenerateRandomSymmetricArray["Integer",3,3];*)
(*\[Epsilon]=GenerateRandomSymmetricArray["Integer",3,3];*)


IdentityArray[values_,order_,dimension_]:=If[values=="Boolean",Map[FiniteField[2,1],GenerateArray[KroneckerDelta,order,dimension],{order}],GenerateArray[KroneckerDelta,order,dimension]]


PartialIdentityArray[values_,order_,dimension_,indices_]:=If[Length[indices]<=order,
With[{indexlist=Table[{ind[i],dimension},{i,order}]},
With[{deltaindexlist=Map[First,indexlist[[#]]&/@indices]},
Table[KroneckerDelta@@deltaindexlist,##]&@@indexlist
]
]
,"Undefined"
]


(* ::Input:: *)
(*ID=IdentityArray["Integer",3,2];*)
(*ir=PartialIdentityArray["Integer",3,2,{1,2}];*)
(*ig=PartialIdentityArray["Integer",3,2,{2,3}];*)
(*ib=PartialIdentityArray["Integer",3,2,{1,3}];*)


(* ::Input:: *)
(*PartialIdentityArray["Integer",2,2,{1,2}]//MatrixForm*)


(* ::Input:: *)
(*ir//MatrixForm*)


(* ::Input:: *)
(*RedPartialIdentity=Table[KroneckerDelta[i,j],{i,2},{j,2},{k,2}];*)
(*PartialIdentityArray["Integer",3,2,{1,2}]==RedPartialIdentity*)


(* ::Section:: *)
(*Hypermatrix Multiplications*)


(* ::Input:: *)
(*HyProduct[name_,arrays_,contraction_]*)


(* ::Section:: *)
(*Plex Diagrams as Hypergraph Objects*)


(* ::Input:: *)
(*PlexDiagram[plexlist_List,indexlist_List,OptionsPattern[]]:=With[{indicesALL=DeleteDuplicates[Join@@plexlist]},*)
(*With[{indicesC=indexlist,indicesF=DeleteElements[indicesALL,indexlist]},*)
(*Hypergraph[indicesALL,plexlist,*)
(*VertexLabels->Automatic,*)
(*VertexSize->Thread[indicesC->0.02],*)
(*VertexStyle->Thread[indicesC->RGBColor[0.77, 0.77, 0.77]],*)
(*VertexSize->Thread[indicesF->0.02],*)
(*VertexStyle->Thread[indicesF->GrayLevel[0]],*)
(*EdgeStyle->{{__}->RGBColor[0.7,0.7,0.7,0.49]},*)
(*EdgeLabels->Thread[plexlist->Map[Subscript["#",#]&,Range[Length[plexlist]]]],*)
(*"EdgeSymmetry"-> "Directed"*)
(*]*)
(*]*)
(*]*)


(* ::Input:: *)
(*FreeIndices[plex_]:=Keys[Select[First[AbsoluteOptions[plex,{VertexStyle}]][[2]],Part[#,2]==GrayLevel[0]&]]*)
(*ContractIndices[plex_]:=Keys[Select[First[AbsoluteOptions[plex,{VertexStyle}]][[2]],Part[#,2]==RGBColor[0.77, 0.77, 0.77]&]]*)


(* ::Input:: *)
(*First[AbsoluteOptions[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {2, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Directed"}],{VertexStyle}]][[2]]*)


(* ::Input:: *)
(*plex1=PlexDiagram[{{1,2},{2,3}},{2}]*)


(* ::Input:: *)
(*plex2=PlexDiagram[{{1,2,3},{2,3,4}},{2,3}]*)


(* ::Input:: *)
(*plex3=PlexDiagram[{{1,2,4},{1,4,3},{4,2,3}},{4}]*)


(* ::Input:: *)
(*plex4=PlexDiagram[{{1,4,5},{4,2,6},{5,6,3}},{4,5,6}]*)


(* ::Input:: *)
(*plex5=PlexDiagram[{{1,2,4},{5,6,4},{5,6,3}},{4,5,6}]*)


(* ::Input:: *)
(*FreeIndices[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {2, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Directed"}]]*)


(* ::Input:: *)
(*ContractIndices[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {2, 3} -> Subscript["#", 2]}]]*)


(* ::Input:: *)
(*FreeIndices[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]]*)


(* ::Input:: *)
(*ContractIndices[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]]*)


(* ::Input:: *)
(*FreeIndices[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3, 4}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 3}, {2, 3, 4}], VertexSize -> {2 -> 0.02, 3 -> 0.02, 1 -> 0.02, 4 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 3 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 4 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 3} -> Subscript["#", 1], {2, 3, 4} -> Subscript["#", 2]}]]*)


(* ::Input:: *)
(*ContractIndices[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3, 4}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 3}, {2, 3, 4}], VertexSize -> {2 -> 0.02, 3 -> 0.02, 1 -> 0.02, 4 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 3 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 4 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 3} -> Subscript["#", 1], {2, 3, 4} -> Subscript["#", 2]}]]*)


(* ::Input:: *)
(*PlexContractions[hg_]:=PlexDiagram[EdgeList[hg],#]&/@Subsets@VertexList[hg]*)


(* ::Input:: *)
(*PlexDiagram[{{1,2,3}},{}]*)


(* ::Input:: *)
(*PlexContractions[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 3}], VertexLabels -> {All -> Automatic}, VertexSize -> {1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 3} -> Subscript["#", 1]}, "EdgeSymmetry" -> {All -> "Directed"}]]*)


(* ::Input:: *)
(*PlexContractions[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {2, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Directed"}]]*)


(* ::Input:: *)
(*PlexContractions[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}]]]*)


(* ::Section:: *)
(*Plex Diagram -> Array Product*)


(* ::Input:: *)
(*PlexProduct[plex_]:=With[{contractV=ContractIndices[plex],*)
(*freeV=DeleteElements[VertexList[plex],ContractIndices[plex]],*)
(*arrayE=DeleteElements[EdgeList[plex],ContractIndices[plex]]},*)
(*With[{args=Array[Slot,Length[arrayE]]},*)
(*Function[IndexContract[args,arrayE->freeV]]*)
(*]*)
(*]*)


(* ::Input:: *)
(*PlexDiagram[{{1,2},{2,3}},{}]*)


(* ::Input:: *)
(*a//MatrixForm*)
(*b//MatrixForm*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {2, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Symmetry"}]][a,b]//MatrixForm*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["a", 1], {2, 3} -> Subscript["a", 2]}]][a,b]==a . b*)


(* ::Input:: *)
(*Hypergraph[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, "VertexAnnotationRules" -> {"Free" -> {1 -> True, 3 -> True}}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}],"EdgeSymmetry"->"Ordered"]*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], "EdgeSymmetry" -> {Blank[] -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, "VertexAnnotationRules" -> {"Free" -> {1 -> True, 3 -> True}}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}]][a,b]==a . b*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], "EdgeSymmetry" -> {Blank[] -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, "VertexAnnotationRules" -> {"Free" -> {1 -> True, 3 -> True}}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}]][PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], "EdgeSymmetry" -> {Blank[] -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, "VertexAnnotationRules" -> {"Free" -> {1 -> True, 3 -> True}}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}]][a,b],c]==PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], "EdgeSymmetry" -> {Blank[] -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, "VertexAnnotationRules" -> {"Free" -> {1 -> True, 3 -> True}}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}]][a,PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], "EdgeSymmetry" -> {Blank[] -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, "VertexAnnotationRules" -> {"Free" -> {1 -> True, 3 -> True}}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}]][b,c]]*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}, {1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}, WolframInstitute`Hypergraph`Hyperedges[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}], VertexSize -> {{-2, 2, 1} -> 0.02, {1, 2, 0} -> 0.02, {-2, 2, 0} -> 0.02, {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> 0.02, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> 0.02, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> 0.02}, VertexStyle -> {{-2, 2, 1} -> RGBColor[0.77, 0.77, 0.77], {1, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {-2, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> GrayLevel[0], {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> GrayLevel[0], {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}} -> Subscript["#", 1], {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}} -> Subscript["#", 2], {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}} -> Subscript["#", 3]}]][PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}, {1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}, WolframInstitute`Hypergraph`Hyperedges[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}], VertexSize -> {{-2, 2, 1} -> 0.02, {1, 2, 0} -> 0.02, {-2, 2, 0} -> 0.02, {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> 0.02, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> 0.02, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> 0.02}, VertexStyle -> {{-2, 2, 1} -> RGBColor[0.77, 0.77, 0.77], {1, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {-2, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> GrayLevel[0], {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> GrayLevel[0], {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}} -> Subscript["#", 1], {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}} -> Subscript["#", 2], {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}} -> Subscript["#", 3]}]][x,y,z],u,v]==PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}, {1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}, WolframInstitute`Hypergraph`Hyperedges[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}], VertexSize -> {{-2, 2, 1} -> 0.02, {1, 2, 0} -> 0.02, {-2, 2, 0} -> 0.02, {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> 0.02, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> 0.02, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> 0.02}, VertexStyle -> {{-2, 2, 1} -> RGBColor[0.77, 0.77, 0.77], {1, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {-2, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> GrayLevel[0], {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> GrayLevel[0], {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}} -> Subscript["#", 1], {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}} -> Subscript["#", 2], {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}} -> Subscript["#", 3]}]][x,PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}, {1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}, WolframInstitute`Hypergraph`Hyperedges[{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}}, {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}}], VertexSize -> {{-2, 2, 1} -> 0.02, {1, 2, 0} -> 0.02, {-2, 2, 0} -> 0.02, {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> 0.02, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> 0.02, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> 0.02}, VertexStyle -> {{-2, 2, 1} -> RGBColor[0.77, 0.77, 0.77], {1, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {-2, 2, 0} -> RGBColor[0.77, 0.77, 0.77], {{1, 2, -2}, {1, -2, -1}, {2, 1, 0}} -> GrayLevel[0], {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}} -> GrayLevel[0], {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}} -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{{{1, 2, -2}, {1, -2, -1}, {2, 1, 0}}, {{1, -1, -2}, {0, 0, -1}, {-2, -1, -2}}, {-2, 2, 1}} -> Subscript["#", 1], {{1, 2, 0}, {-2, 2, 0}, {-2, 2, 1}} -> Subscript["#", 2], {{1, 2, 0}, {-2, 2, 0}, {{0, -1, 0}, {1, 2, 1}, {1, -2, 0}}} -> Subscript["#", 3]}]][u,z,y],v]*)


(* ::Input:: *)
(*Hypergraph[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}],"EdgeSymmetry"->"Ordered"]*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], "EdgeSymmetry" -> {All -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]][\[Alpha],\[Beta],\[Gamma]]==PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], "EdgeSymmetry" -> {All -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]][\[Alpha],\[Gamma],\[Beta]]*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], "EdgeSymmetry" -> {All -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]][ir,ir,ir]==ir*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], "EdgeSymmetry" -> {All -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]][ID,ID,ID]==ID*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], "EdgeSymmetry" -> {All -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]][\[Alpha],ig,ib]==\[Alpha]*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], "EdgeSymmetry" -> {All -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]][ID,\[Beta],BM[ID,\[Gamma],\[Delta]]]==PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], "EdgeSymmetry" -> {All -> "Ordered"}, "LayoutDimension" -> 2, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}]][ID,BM[ID,\[Beta],\[Gamma]],\[Delta]]*)


(* ::Input:: *)
(*DiamProd[a_,b_]:=Table[Sum[a[[i,p,q]]*b[[p,q,j]],{p,1,3},{q,1,3}],{i,3},{j,3}]*)
(*DiamProdContr[a_,b_]:=IndexContract[{a,b},"ipq,pqj->ij"]*)


(* ::Input:: *)
(*DiamProd[x,y]==DiamProdContr[x,y]*)


(* ::Input:: *)
(*PlexProduct[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3, 4}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 3}, {2, 3, 4}], VertexSize -> {2 -> 0.02, 3 -> 0.02, 1 -> 0.02, 4 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 3 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 4 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 3} -> Subscript["#", 1], {2, 3, 4} -> Subscript["#", 2]}]][x,y]==DiamProd[x,y]*)


(* ::Section:: *)
(*Plex Product Enumeration*)


(* ::Input:: *)
(*EnumeratePlexProducts[order_,arity_]:=With[{plexes=Flatten[PlexContractions/@EnumerateHypergraphs[All,{{arity,order}}]]},*)
(*Select[plexes,Length[FreeIndices[#]]==order&]*)
(*]*)


(* ::Input:: *)
(*EnumeratePlexProducts[2,2]*)


(* ::Input:: *)
(*{WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {1, 3}], VertexSize -> {1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {1 -> RGBColor[0.77, 0.77, 0.77], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {1, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Unordered"}],WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {1, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {1, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Unordered"}],WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {1, 3}], VertexSize -> {3 -> 0.02, 1 -> 0.02, 2 -> 0.02}, VertexStyle -> {3 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {1, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Unordered"}]}*)


(* ::Input:: *)
(*EnumeratePlexProducts[3,3]*)

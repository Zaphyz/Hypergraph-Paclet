(* ::Package:: *)

(* ::Input:: *)
(*PacletInstall["https://wolfr.am/Hypergraph.paclet",ForceVersionInstall -> True]*)
(*<<WolframInstitute`Hypergraph`*)


(* ::Input:: *)
(*PacletInstall["https://wolfr.am/Multicomputation.paclet",ForceVersionInstall -> True]*)
(*<<Wolfram`Multicomputation`*)


UnLabel[hyp_]:=Hypergraph[hyp,VertexLabels->{_->None},EdgeLabels->{_->None}]


HypergraphRuleEmb[hgIN_,hgOUT_]:=With[{hgOUTemb=Hypergraph[hgOUT,VertexCoordinates->Thread[VertexList[hgIN]->HypergraphEmbedding[hgIN]]]},
HypergraphRule[hgIN,hgOUTemb]
]


(* ::Input:: *)
(*InputHyp=Hypergraph[{1,2,3,4,5},*)
(*Hyperedges[{1,2},{2,3,4},{1,4,5},{5,1}, {3}],*)
(*EdgeLabels -> {{1, 2} -> "x", {2,3,4}->"A",{1,4,5}->"B",{5,1}->"y",{3}->"p"},*)
(*PlotTheme->"Dark",VertexLabels->Automatic];*)
(*OutputHyp=Hypergraph[{1,2,3,4,5},*)
(*Hyperedges[{1,3},{1,2,4,5}],*)
(*EdgeLabels -> {{1,3} -> "z",{1,2,4,5}->"S"},*)
(*PlotTheme->"Dark",VertexLabels->Automatic];*)
(*Rule=HypergraphRule[InputHyp,OutputHyp]*)


(* ::Chapter::Closed:: *)
(*Unit Line*)


(* ::Input:: *)
(*LineUnitInput=Hypergraph[{1},Hyperedges[],"EdgeSymmetry"->"Unordered",VertexLabels->{1->p},PlotTheme->"Dark"];*)
(*LineUnitOutput=Hypergraph[{1,2},Hyperedges[{1,2}],"EdgeSymmetry"->"Unordered",VertexLabels->{1->p,2->p},EdgeLabels->{{1,2}->Subscript["1", p]},PlotTheme->"Dark"];*)
(*LineUnitRule=HypergraphRuleEmb[LineUnitInput,LineUnitOutput]*)


(* ::Input:: *)
(*SingleLine=Hypergraph[{1,2},Hyperedges[{1,2}],VertexLabels->{1->r,2->s},EdgeLabels->{{1,2}->f},PlotTheme->"Dark"]*)


(* ::Input:: *)
(*MultiwaySystem[LineUnitRule,SingleLine]["EvolutionGraph",2,VertexSize->200]*)


(* ::Chapter:: *)
(*Line Composition*)


(* ::Input:: *)
(*LineInput=Hypergraph[{1,2,3},Hyperedges[{1,2},{2,3}],EdgeLabels -> {{1, 2} -> x, {2,3} -> y},PlotTheme->"Dark"];*)
(*LineOutput=Hypergraph[{1,2,3},Hyperedges[{1,3}],EdgeLabels -> {{1,3} -> Row[{"{",x,y,"}"}]},PlotTheme->"Dark"];*)
(*LineComp=HypergraphRuleEmb[LineInput,LineOutput]*)


(* ::Section:: *)
(*Bag Associativity:	{{h g} f} = {{f g} h} *)


(* ::Input:: *)
(*LineChain3=Hypergraph[{1,2,3,4},Hyperedges[{1,2},{2,3},{3,4}],EdgeLabels -> {{1, 2} -> f, {2,3} -> g,{3,4}->h},PlotTheme->"Dark"]*)


(* ::Input:: *)
(*MultiwaySystem[LineComp,LineChain3]["EvolutionGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*UnLLineComp=HypergraphRuleEmb[UnLabel@LineInput,UnLabel@LineOutput]*)


(* ::Input:: *)
(*MultiwaySystem[UnLLineComp,UnLabel@LineChain3]["EvolutionGraph",2,VertexSize->256]*)


(* ::Chapter::Closed:: *)
(*Line Category*)


(* ::Section:: *)
(*Neutrality of units:	{Subscript[1, s] f} = f = {Subscript[1, r] f}*)


(* ::Input:: *)
(*MultiwaySystem[{LineUnitRule,LineComp},WolframInstitute`Hypergraph`Hypergraph[{1, 2}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}], GraphLayout -> "SpringElectricalEmbedding", "LayoutDimension" -> 2, VertexLabels -> {1 -> $CellContext`r, 2 -> $CellContext`s}, EdgeLabels -> {{1, 2} -> $CellContext`f}, PlotTheme -> "Dark"]]["EvolutionGraph",2,VertexSize->200]*)


(* ::Input:: *)
(*Hypergraph[SingleLine,GraphLayout->"SpringElectricalEmbedding"]*)


(* ::Chapter::Closed:: *)
(*Unit Arrow*)


(* ::Input:: *)
(*ArrowUnitOutInput=Hypergraph[{1},Hyperedges[],"EdgeSymmetry"->"Ordered",VertexLabels->{1->p},PlotTheme->"Dark"];*)
(*ArrowUnitOutOutput=Hypergraph[{1,2},Hyperedges[{1,2}],"EdgeSymmetry"->"Ordered",VertexLabels->{1->p,2->p},EdgeLabels->{{1,2}->Subscript["1", p]},PlotTheme->"Dark"];*)
(*ArrowUnitOutRule=HypergraphRuleEmb[ArrowUnitOutInput,ArrowUnitOutOutput]*)


(* ::Input:: *)
(*ArrowUnitInInput=Hypergraph[{1},Hyperedges[],"EdgeSymmetry"->"Ordered",VertexLabels->{1->p},PlotTheme->"Dark"];*)
(*ArrowUnitInOutput=Hypergraph[{1,2},Hyperedges[{2,1}],"EdgeSymmetry"->"Ordered",VertexLabels->{1->p,2->p},EdgeLabels->{{2,1}->Subscript["1", p]},PlotTheme->"Dark"];*)
(*ArrowUnitInRule=HypergraphRuleEmb[ArrowUnitInInput,ArrowUnitInOutput]*)


(* ::Input:: *)
(*SingleArrow=Hypergraph[{1,2},Hyperedges[{1,2}],"EdgeSymmetry"->"Ordered",VertexLabels->{1->r,2->s},EdgeLabels->{{1,2}->f},PlotTheme->"Dark"]*)


(* ::Input:: *)
(*MultiwaySystem[{ArrowUnitOutRule,ArrowUnitInRule},SingleArrow]["EvolutionGraph",1,VertexSize->200]*)


(* ::Chapter:: *)
(*Arrow Composition*)


(* ::Input:: *)
(*ArrowInput=Hypergraph[{1,2,3},Hyperedges[{1,2},{2,3}],"EdgeSymmetry"->"Ordered",VertexLabels->{1->p,2->q,3->r},EdgeLabels -> {{1, 2} -> x, {2,3} -> y},PlotTheme->"Dark"];*)
(*ArrowOutput=Hypergraph[{1,2,3},Hyperedges[{1,3}],"EdgeSymmetry"->"Ordered",VertexLabels->{1->p,2->q,3->r},EdgeLabels -> {{1,3} -> Row[{"(",x,y,")"}]},PlotTheme->"Dark"];*)
(*ArrowComp=HypergraphRuleEmb[ArrowInput,ArrowOutput]*)


(* ::Input:: *)
(*ArrowChain2=Hypergraph[{1,2,3},Hyperedges[{1,2},{2,3}],"EdgeSymmetry"->"Ordered",EdgeLabels -> {{1, 2} -> f, {2,3} -> g},PlotTheme->"Dark"]*)


(* ::Input:: *)
(*ArrowChain3=Hypergraph[{1,2,3,4},Hyperedges[{1,2},{2,3},{3,4}],"EdgeSymmetry"->"Ordered",EdgeLabels -> {{1, 2} -> f, {2,3} -> g,{3,4}->h},PlotTheme->"Dark",VertexLabels->Automatic]*)


(* ::Section:: *)
(*Tuple Associativity:	(f (g h)) = ((f g) h)*)


(* ::Input:: *)
(*Hypergraph[{1, 2, 3, 4}, Hyperedges[{1, 2}, {2, 3}, {3, 4}], "EdgeSymmetry" -> {All -> "Ordered"}, EdgeLabels -> {{1, 2} -> f, {2, 3} -> g, {3, 4} -> h}, *)
(*  PlotTheme -> "Dark"]*)


(* ::Input:: *)
(*ArrowComp@ArrowChain3*)


(* ::Input:: *)
(*MultiwaySystem[ArrowComp,ArrowChain3]["EvolutionGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*UnLArrowComp=HypergraphRuleEmb[UnLabel@ArrowInput,UnLabel@ArrowOutput]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain3]["EvolutionGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain3]["StatesGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain3]["EvolutionCausalGraph",2,VertexSize->150]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain3]["CausalGraph",4,VertexSize->500]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain3]["CausalBranchialGraph",1,VertexSize->5000000]*)


(* ::Input:: *)
(*ArrowChain4=Hypergraph[Hyperedges[{1,2},{2,3},{4,3},{5,4}],"EdgeSymmetry"->"Ordered",EdgeLabels -> {{1, 2} -> f, {2,3} -> g,{4,3}->h,{5,4}->i},PlotTheme->"Dark",GraphLayout->"LayeredEmbedding"]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain4]["EvolutionGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain4]["CausalBranchialGraph",1,VertexSize->5000000]*)


(* ::Input:: *)
(*ArrowChain5=Hypergraph[Hyperedges[{1,2},{2,3},{4,3},{4,5},{5,6}],"EdgeSymmetry"->"Ordered",EdgeLabels -> {{1, 2} -> f, {2,3} -> g,{4,3}->h,{4,5}->i,{5,6}->j},PlotTheme->"Dark"]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain5]["EvolutionGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*MultiwaySystem[UnLArrowComp,UnLabel@ArrowChain5]["CausalBranchialGraph",1,VertexSize->5000000]*)


(* ::Chapter::Closed:: *)
(*Arrow Category*)


(* ::Section:: *)
(*Neutrality of units:	(Subscript[1, s] f) = f = (f Subscript[1, r])*)


(* ::Input:: *)
(*MultiwaySystem[{ArrowUnitOutRule,ArrowUnitInRule,ArrowComp},SingleArrow]["EvolutionGraph",2,VertexSize->200]*)


(* ::Input:: *)
(*MultiwaySystem[{ArrowUnitOutRule,ArrowUnitInRule,ArrowComp},ArrowChain2]["EvolutionGraph",2,VertexSize->200]*)


(* ::Chapter::Closed:: *)
(*Dagger*)


(* ::Input:: *)
(*DaggerInput=Hypergraph[{1,2},Hyperedges[{1,2}],"EdgeSymmetry"->"Ordered",EdgeLabels->{{1,2}->x},VertexLabels->{1->p,2->q},PlotTheme->"Dark"];*)
(*DaggerOutput=Hypergraph[{1,2},Hyperedges[{2,1}],"EdgeSymmetry"->"Ordered",EdgeLabels->{{2,1}->Row[{"(",SuperDagger[x],")"}]},VertexLabels->{1->p,2->q},PlotTheme->"Dark"];*)
(*DaggerRule=HypergraphRuleEmb[DaggerInput,DaggerOutput]*)


(* ::Section:: *)
(*Involutivity:	SuperDagger[(SuperDagger[f])]= f*)


(* ::Input:: *)
(*MultiwaySystem[{DaggerRule},SingleArrow]["EvolutionGraph",2,VertexSize->200]*)


(* ::Section:: *)
(*Antihomomorphism:	(f SuperDagger[g)] = (SuperDagger[g] SuperDagger[f])*)


(* ::Input:: *)
(*MultiwaySystem[{DaggerRule,ArrowComp},ArrowChain2]["EvolutionGraph",3,VertexSize->200]*)


(* ::Chapter:: *)
(*Action*)


(* ::Input:: *)
(*ActionInput=Hypergraph[{1,2},Hyperedges[{1},{1,2}],EdgeLabels -> {{1}->a,{1, 2} -> x},"EdgeSymmetry" -> "Ordered",PlotTheme->"Dark",VertexLabels->Automatic];*)
(*ActionOutput=Hypergraph[{1,2},Hyperedges[{2}],EdgeLabels -> {{2}->Row[{x,"[",a,"]"}]},"EdgeSymmetry" -> "Ordered",PlotTheme->"Dark",VertexLabels->Automatic];*)
(*ActionComp=HypergraphRuleEmb[ActionInput,ActionOutput]*)


(* ::Input:: *)
(*DotArrowChain2=Hypergraph[{1,2,3},Hyperedges[{1},{1,2},{2,3}],"EdgeSymmetry"->"Ordered",EdgeLabels -> {{1}->u,{1, 2} -> f, {2,3} -> g},PlotTheme->"Dark",GraphLayout->"SpringElectricalEmbedding"]*)


(* ::Section:: *)
(*Action Associativity:	(f g) [u] = f [g [u]]*)


(* ::Input:: *)
(*MultiwaySystem[{ActionComp,ArrowComp},DotArrowChain2]["EvolutionGraph",2,VertexSize->200]*)


(* ::Chapter:: *)
(*Zig-Zag Composition*)


(* ::Input:: *)
(*ZigZagInput=Hypergraph[{1,2,3,4},Hyperedges[{1,2},{3,2},{3,4}],EdgeLabels -> {{1, 2} -> x, {3,2} -> y, {3, 4} -> z},"EdgeSymmetry" -> "Ordered",PlotTheme->"Dark"];*)
(*ZigZagOutput=Hypergraph[{1,2,3,4},Hyperedges[{1,4}],EdgeLabels -> {{1, 4} ->Row[{"(",x,y,z,")"}]},"EdgeSymmetry" -> "Ordered",PlotTheme->"Dark"];*)
(*ZigZagComp=HypergraphRule[ZigZagInput,ZigZagOutput]*)


(* ::Input:: *)
(*LongZigZag=Hypergraph[{1,2,3,4,5,6},Hyperedges[{1,2},{3,2},{3,4},{5,4},{5,6}],EdgeLabels -> {{1, 2} -> a, {3,2} -> b, {3, 4} -> c,{5,4}->d,{5,6}->e},"EdgeSymmetry" -> "Ordered",PlotTheme->"Dark"]*)


(* ::Section:: *)
(*Heap Associativity:	((abc)de) = (a(dcb)e) = (ab(cde))*)


(* ::Input:: *)
(*MultiwaySystem[ZigZagComp,LongZigZag]["EvolutionGraph",2,VertexSize->256]*)


(* ::Chapter::Closed:: *)
(*Operad Composition*)


(* ::Input:: *)
(*Operad23Input=Hypergraph[{1, 2, 3, 4, 5, 6}, Hyperedges[{1, 2, 3,4}, { 4, 5,6}],  *)
(*      EdgeLabels -> {{1, 2, 3,4}->a, { 4, 5,6}->b},"EdgeSymmetry" -> "Ordered",PlotTheme->"Dark",VertexLabels->Automatic];*)
(*Operad23Output=Hypergraph[{1, 2, 3, 4, 5, 6}, Hyperedges[{1,2,3,5,6}],  *)
(*      EdgeLabels -> {{1,2,3,5,6} -> Row[{"(",a,b,")"}]},"EdgeSymmetry" -> "Ordered",PlotTheme->"Dark",VertexLabels->Automatic];*)
(*Operad23Comp=HypergraphRule[Operad23Input,Operad23Output]*)


(* ::Chapter:: *)
(*Fish Composition*)


(* ::Input:: *)
(*FishInput=Hypergraph[{1, 2, 3, 4, 5, 6}, Hyperedges[{1, 2, 3}, {2, 4, 5}, {5, 6, 4}],  *)
(*      EdgeLabels -> {{1, 2, 3} -> x, {2, 4, 5} -> y, {5, 6, 4} -> z},"EdgeSymmetry" -> "Unordered",PlotTheme->"Dark"];*)
(*FishOutput=Hypergraph[{1, 2, 3, 4, 5, 6}, Hyperedges[{1,3,6}],  *)
(*      EdgeLabels -> {{1, 3,6} -> Row[{"(",x,y,z,")"}]},"EdgeSymmetry" -> "Unordered",PlotTheme->"Dark"];*)
(*FishComp=HypergraphRuleEmb[FishInput,FishOutput]*)


(* ::Input:: *)
(*LongFish=Hypergraph[{1, 2, 3, 4, 5, 6, 7, 8, 9}, Hyperedges[{1, 2, 3}, {3, 4, 5}, {5, 6, 4}, {6, 7, 8}, {7, 9, 8}],  *)
(*      EdgeLabels -> {{1, 2, 3} -> a, {3, 4, 5} -> b, {5, 6, 4} -> c, {6, 7, 8} -> d, {7, 9, 8} -> e},"EdgeSymmetry" -> "Unordered",PlotTheme->"Dark"]*)


(* ::Section:: *)
(*Heap Associativity:	((abc)de)  = (ab(cde)) = (a(dcb)e)*)


(* ::Input:: *)
(*MultiwaySystem[FishComp,LongFish]["EvolutionGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*UnLFishComp=HypergraphRuleEmb[UnLabel@FishInput,UnLabel@FishOutput]*)


(* ::Input:: *)
(*MultiwaySystem[UnLFishComp,UnLabel@LongFish]["EvolutionGraph",2,VertexSize->256]*)


(* ::Input:: *)
(*MultiwaySystem[UnLFishComp,UnLabel@LongFish]["BranchialGraph",1]*)


(* ::Input:: *)
(*MultiwaySystem[UnLFishComp,UnLabel@LongFish]["EvolutionCausalGraph",2]*)


(* ::Chapter:: *)
(*Bhattacharya-Mesner Composition*)


(* ::Input:: *)
(*BMInput=Hypergraph[{1,2,3,4},Hyperedges[{1,2,4},{2,3,4},{3,1,4}],EdgeLabels->{{1,2,4}->x,{2,3,4}->y,{3,1,4}->z},PlotTheme->"Dark"];*)
(*BMOutput=Hypergraph[{1,2,3,4},Hyperedges[{1,2,3}],EdgeLabels->{{1,2,3}->Row[{"(",x,y,z,")"}]},PlotTheme->"Dark"];*)
(*BMComp=HypergraphRule[BMInput,BMOutput]*)


(* ::Input:: *)
(*TriTetroid=Hypergraph[{1, 2, 3, 4, 5, 6, 7}, Hyperedges[{1, 2, 5}, {2, 4, 5},{4, 1, 5}, {2, 3, 7}, {3, 4, 7}, {4, 2, 7}, {3, 1, 6}, *)
(*   {6, 4, 3}, {1, 4, 6}],EdgeLabels->{{1, 2, 5}->a, {2, 4, 5}->b,{4, 1, 5}->c, {2, 3, 7}->d, {3, 4, 7}->e, {4, 2, 7}->f, {3, 1, 6}->g, *)
(*   {6, 4, 3}->h, {1, 4, 6}->i},PlotTheme->"Dark"]*)


(* ::Section:: *)
(*Single-Valued Operation	((abc)(def)(ghi))*)


(* ::Input:: *)
(*UnLBMComp=HypergraphRule[UnLabel@BMInput,UnLabel@BMOutput]*)


(* ::Input:: *)
(*WolframInstitute`Hypergraph`HypergraphRule[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3, 4}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {2, 3, 4}, {3, 1, 4}], VertexLabels -> {Blank[] -> None}, EdgeLabels -> {Blank[] -> None}, "LayoutDimension" -> 2, PlotTheme -> "Dark"], WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3, 4}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 3}], VertexLabels -> {Blank[] -> None}, EdgeLabels -> {Blank[] -> None}, "LayoutDimension" -> 2, PlotTheme -> "Dark"]]*)


(* ::Input:: *)
(*MultiwaySystem[UnLBMComp,UnLabel@TriTetroid]["EvolutionGraph",4,VertexSize->256]*)


(* ::Input:: *)
(*[[a,b,c],[x,y,z],[t,w,v]]*)


(* ::Input:: *)
(*MultiwaySystem[BMComp,TriTetroid]["EvolutionGraph",1,VertexSize->256]*)


(* ::Input:: *)
(*MultiwaySystem[BMComp,TriTetroid]["EvolutionGraph",4,VertexSize->256]*)

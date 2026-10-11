(* ::Package:: *)

(* ::Input:: *)
(*PacletInstall["https://wolfr.am/Hypergraph.paclet",ForceVersionInstall -> True]*)
(*<<WolframInstitute`Hypergraph`*)


(* ::Title:: *)
(*Array Algebra*)


(* ::Text:: *)
(*(only regular arrays are considered for simplicity, i.e. all indices range over the same values)*)


(* ::Chapter:: *)
(*Generating Arrays*)


RandomElement[values_][arg_]:=Switch[values,"Boolean",FiniteField[2,1][RandomInteger[]],"Integer",RandomInteger[{-2,2}],"Complex",SetPrecision[RandomComplex[{-1.5-1.5I,1.5+1.5I}],2]]


(* ::Input:: *)
(*SetPrecision[RandomComplex[],5]*)


(* ::Input:: *)
(*RandomElement["Boolean"][{2,3,4,5}]*)


(* ::Input:: *)
(*RandomElement["Integer"][{1,2}]*)


(* ::Input:: *)
(*RandomElement["Complex"][{}]*)


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
(*r=GenerateRandomArray["Integer",1,3];*)
(*a=GenerateRandomArray["Integer",2,3];*)
(*b=GenerateRandomArray["Integer",2,3];*)
(*c=GenerateRandomArray["Integer",2,3];*)
(*d=GenerateRandomArray["Integer",2,3];*)
(*e=GenerateRandomArray["Integer",2,3];*)
(*x=GenerateRandomArray["Integer",3,3];*)
(*y=GenerateRandomArray["Integer",3,3];*)
(*z=GenerateRandomArray["Integer",3,3];*)
(*t=GenerateRandomArray["Integer",3,3];*)
(*s=GenerateRandomArray["Integer",3,3];*)
(*v1=GenerateRandomArray["Integer",1,4];*)
(*v2=GenerateRandomArray["Integer",1,4];*)
(*v3=GenerateRandomArray["Integer",1,4];*)
(*v4=GenerateRandomArray["Integer",1,4];*)
(*v5=GenerateRandomArray["Integer",1,4];*)


(* ::Input:: *)
(*SymbArray=GenerateArray[a,2,3]*)


(* ::Input:: *)
(*SymbArray//MatrixForm*)


(* ::Input:: *)
(*SymbArray[[2,3]]*)


(* ::Input:: *)
(*GenerateArray[b,0,3]*)


(* ::Input:: *)
(*GenerateRandomArray["Integer",4,2]//MatrixForm*)


(* ::Input:: *)
(*GenerateRandomArray["Integer",9,2]//CubixForm*)


(* ::Input:: *)
(*Graphics3D[{{{{Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {1, 1, 1}], {1, 1, 1}], Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {1, 1, 2}], {1, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {1, 2, 1}], {1, 2, 1}], Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {1, 2, 2}], {1, 2, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {2, 1, 1}], {2, 1, 1}], Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {2, 1, 2}], {2, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {2, 2, 1}], {2, 2, 1}], Text[Tooltip[ClickToCopy[Framed[Graphics3D[{{}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {ImageSize -> 20}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False], RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], Graphics3D[{{{{{Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {1, 1, 1}], {1, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 1, 2}], {1, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 2, 1}], {1, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {1, 2, 2}], {1, 2, 2}]}}}, {{{Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 1}], {2, 1, 1}]}, {Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 2}], {2, 1, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 2, 1}], {2, 2, 1}]}, {Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 2}], {2, 2, 2}]}}}}, {GrayLevel[0.5], {{Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}}, {}, Background -> GrayLevel[1], AxesStyle -> GrayLevel[0.], Axes -> False, Boxed -> False]], {2, 2, 2}], {2, 2, 2}]}}}, {GrayLevel[0.5], {Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}, Axes -> False, AxesStyle -> GrayLevel[0.], Background -> GrayLevel[1], Boxed -> False, ImageSize -> {169.5336413232248, 204.43058809162815`}, ViewPoint -> {1.1259228073194645`, -2.461760478675112, 2.0302790885962305`}, ViewVertical -> {-0.0645927627300946, -0.019115977122739353`, 0.9977285975762831}]*)


(* ::Input:: *)
(*Graphics3D[{{{{Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 1, 1}], {1, 1, 1}], Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {1, 1, 2}], {1, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 2, 1}], {1, 2, 1}], Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {1, 2, 2}], {1, 2, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {2, 1, 1}], {2, 1, 1}], Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 1, 2}], {2, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 1}], {2, 2, 1}], Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 2}], {2, 2, 2}]}}}, {GrayLevel[0.5], {Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}, Axes -> False, AxesStyle -> GrayLevel[0.], Background -> GrayLevel[1], Boxed -> False, ImageSize -> {114.31243881131773`, 121.93184296377773`}, ImageSizeRaw -> Automatic, ViewPoint -> {1.2237217780445635`, -2.1840866828181853`, 2.2764600527739787`}, ViewVertical -> {-0.057371162954910945`, 0.0886111535822231, 0.9944126975868868}]*)


(* ::Input:: *)
(*Graphics3D[{{{{Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 1, 1}], {1, 1, 1}], Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 1, 2}], {1, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 2, 1}], {1, 2, 1}], Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 2, 2}], {1, 2, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 1}], {2, 1, 1}], Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 2}], {2, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 1}], {2, 2, 1}], Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 2}], {2, 2, 2}]}}}, {GrayLevel[0.5], {Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}, Axes -> False, AxesStyle -> GrayLevel[0.], Background -> GrayLevel[1], Boxed -> False, ImageSize -> {132.50000000000045`, Automatic}]*)


(* ::Input:: *)
(*Graphics3D[{{{{Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 1, 1}], {1, 1, 1}], Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 1, 2}], {1, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[-1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -1], {1, 2, 1}], {1, 2, 1}], Text[Tooltip[ClickToCopy[Framed[0, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 0], {1, 2, 2}], {1, 2, 2}]}}, {{Text[Tooltip[ClickToCopy[Framed[1, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 1], {2, 1, 1}], {2, 1, 1}], Text[Tooltip[ClickToCopy[Framed[-2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], -2], {2, 1, 2}], {2, 1, 2}]}, {Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 1}], {2, 2, 1}], Text[Tooltip[ClickToCopy[Framed[2, RoundingRadius -> 5, Background -> GrayLevel[1], FrameMargins -> 1.8, FrameStyle -> GrayLevel[0.85], ContentPadding -> False, ImageSize -> 20, Alignment -> Center], 2], {2, 2, 2}], {2, 2, 2}]}}}, {GrayLevel[0.5], {Line[{{{1, 1, 1}, {2, 1, 1}}, {{1, 1, 2}, {2, 1, 2}}}], Line[{{{1, 2, 1}, {2, 2, 1}}, {{1, 2, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 2, 1}}, {{1, 1, 2}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 2, 1}}, {{2, 1, 2}, {2, 2, 2}}}]}, {Line[{{{1, 1, 1}, {1, 1, 2}}, {{1, 2, 1}, {1, 2, 2}}}], Line[{{{2, 1, 1}, {2, 1, 2}}, {{2, 2, 1}, {2, 2, 2}}}]}}}, Axes -> False, AxesStyle -> GrayLevel[0.], Background -> GrayLevel[1], Boxed -> False, ImageSize -> {152.01692131254177`, 150.06355307180988`}, ImageSizeRaw -> Automatic, ViewPoint -> {1.1524975747313269`, -2.8878453637246224`, 1.3349526191789027`}, ViewVertical -> {0.06751635426505664, -0.21526477718509424`, 0.9742189782642336}]*)


(* ::Input:: *)
(*GenerateRandomArray["Integer",2,4]//MatrixForm*)


(* ::Input:: *)
(*GenerateRandomArray["Complex",3,2]//MatrixForm*)


(* ::Input:: *)
(*GenerateRandomArray["Integer",0,4]*)


(* ::Input:: *)
(*GenerateRandomArrays[3,"Boolean",3,2]*)


(* ::Input:: *)
(*GenerateSymmetricArray[y,2,3]//MatrixForm*)


(* ::Input:: *)
(*GenerateRandomSymmetricArray["Boolean",2,3]//MatrixForm*)


(* ::Input:: *)
(*GenerateRandomSymmetricArray["Boolean",3,2]//MatrixForm*)


(* ::Input:: *)
(*CubixForm[GenerateRandomSymmetricArray["Boolean",3,2],"Framed"->False,"PlotTheme"->"Dark","Indices"->"Colored","Gnomon"->True]*)


(* ::Input:: *)
(*GenerateRandomSymmetricArray["Complex",2,2]//MatrixForm*)


(* ::Chapter:: *)
(*Special Arrays*)


ZeroArray[values_,order_,dimension_]:=If[values=="Boolean",Map[FiniteField[2,1],ConstantArray[0,ConstantArray[dimension,order]],{order}],ConstantArray[0,ConstantArray[dimension,order]]]


(* ::Input:: *)
(*ZeroArray["Integer",2,2]//MatrixForm*)


(* ::Input:: *)
(*ZeroArray["Boolean",2,2]//MatrixForm*)


OneArray[values_,order_,dimension_]:=If[values=="Boolean",Map[FiniteField[2,1],ConstantArray[1,ConstantArray[dimension,order]],{order}],ConstantArray[1,ConstantArray[dimension,order]]]


(* ::Input:: *)
(*OneArray["Integer",1,3]*)


(* ::Input:: *)
(*OneArray["Boolean",3,2]*)


IdentityArray[values_,order_,dimension_]:=If[values=="Boolean",Map[FiniteField[2,1],GenerateArray[KroneckerDelta,order,dimension],{order}],GenerateArray[KroneckerDelta,order,dimension]]


(* ::Input:: *)
(*IdentityArray["Integer",2,3]//MatrixForm*)


(* ::Input:: *)
(*IdentityArray["Boolean",2,2]//MatrixForm*)


(* ::Input:: *)
(*IdentityArray["Integer",1,3]*)


(* ::Input:: *)
(*IdentityArray["Boolean",0,2]*)


(* ::Input:: *)
(*IdentityArray["Boolean",0,0]*)


(* ::Input:: *)
(*ArrayReshape*)
(*ArrayPad*)


PartialIdentityArray[values_,order_,dimension_,indices_]:=If[Length[indices]<=order,
With[{indexlist=Table[{ind[i],dimension},{i,order}]},
With[{deltaindexlist=Map[First,indexlist[[#]]&/@indices]},
Table[KroneckerDelta@@deltaindexlist,##]&@@indexlist
]
]
,"Undefined"
]


(* ::Input:: *)
(*SymbolicDeltaProductArray[]*)


(* ::Input:: *)
(*SymbolicDeltaProductArray[{3,3,3},{{1,2,3}}]*)


(* ::Input:: *)
(*SymbolicDeltaProductArray[{3,3,3},{{1,2},{2,3},{1,3}}]*)


(* ::Input:: *)
(*Table[KroneckerDelta[i,j,k],{i,3},{j,3},{k,3}]//CubixForm*)


(* ::Input:: *)
(*Normal[SymbolicDeltaProductArray[{3,3,3},{{1,2,3}}]]//ArrayPlot3D*)


(* ::Input:: *)
(*Normal[SymbolicDeltaProductArray[{2,2,2,2},{{1,4},{2,3}}]]//MatrixForm*)


(* ::Input:: *)
(*PartialIdentityArray["Integer",2,2,{1,2}]//MatrixForm*)


(* ::Input:: *)
(*PartialIdentityArray["Integer",3,3,{1,2}]//ArrayPlot3D*)


(* ::Input:: *)
(*ir=Table[KroneckerDelta[i,j],{i,2},{j,2},{k,2}];*)


(* ::Input:: *)
(*PartialIdentityArray["Integer",3,2,{1,2}]==ir*)


(* ::Chapter:: *)
(*Contraction Operations*)


CloudGet[CloudObject["https://www.wolframcloud.com/obj/nikm/EinsteinSummation"]]


IndexContract[arraylist_,indices_]:=EinsteinSummation[indices,arraylist]


(* ::Input:: *)
(*IndexContract[{p,q},"i,i->i"]==p*q*)


(* ::Input:: *)
(*IndexContract[{a,b},"ij,ij->ij"]==a*b*)


(* ::Input:: *)
(*MatrixForm/@{a,b}*)


(* ::Input:: *)
(*IndexContract[{a,b},"ij,jk->ik"]==a . b*)


(* ::Input:: *)
(*IndexContract[{a,b},{{1,2},{2,3}}->{1,3}]==IndexContract[{a,b},"ij,jk->ik"]*)


(* ::Input:: *)
(*NaryDot[vects_]:=If[Apply[Equal,Map[Length,vects]],Total[Product[vects[[i]],{i,Length[vects]}]], "Vectors of different dimensions"]*)


(* ::Input:: *)
(*IndexContract[{p,q,r},"i,i,i"]===NaryDot[{p,q,r}]*)


(* ::Input:: *)
(*IndexContract[{p,q,r},{{1},{1},{1}}->{}]===NaryDot[{p,q,r}]*)


(* ::Input:: *)
(*P1[a_,b_,c_]:=Table[Sum[a[[i,j,p]]*b[[i,p,k]]*c[[p,j,k]],{p,1,3}],{i,3},{j,3},{k,3}];*)


(* ::Input:: *)
(*P1[x,y,z]===IndexContract[{x,y,z},"ijp,ipk,pjk->ijk"]*)


(* ::Input:: *)
(*P1[x,y,z]===IndexContract[{x,y,z},{{1,2,4},{1,4,3},{4,2,3}}->{1,2,3}]*)


(* ::Input:: *)
(*P2[a_,b_,c_]:=Table[Sum[a[[i,r,p]]*b[[r,j,q]]*c[[p,q,k]],{r,1,3},{p,1,3},{q,1,3}],{i,3},{j,3},{k,3}];*)


(* ::Input:: *)
(*P2[x,y,z]===IndexContract[{x,y,z},"irp,rjq,pqk->ijk"]*)


(* ::Section:: *)
(*ZigZag Product*)


ZigZagProduct[arraylist_]:=If[OddQ@Length[arraylist] && SameQ@@Dimensions/@arraylist && AllTrue[arraylist,MatrixQ],
With[{arity=Length[arraylist]},
With[{INindices=Table[If[OddQ[i],{i,i+1},{i+1,i}],{i,arity}],OUTindices={1,arity+1}},
IndexContract[arraylist,INindices->OUTindices]
]
],
"Undefined"
]


(* ::Input:: *)
(*ZigZagProduct[{a,b,c}]//MatrixForm*)


(* ::Input:: *)
(*ZigZagProduct[{a,b,c}]==a . Transpose[b] . c*)


(* ::Input:: *)
(*ZigZagProduct[{ZigZagProduct[{a,b,c}],d,e}]==ZigZagProduct[{a,ZigZagProduct[{d,c,b}],e}]*)


(* ::Input:: *)
(*ZigZagProduct[{ZigZagProduct[{a,b,c}],d,e}]==ZigZagProduct[{a,b,c,d,e}]*)


(* ::Section:: *)
(*Bhattacharya-Mesner Product*)


BMProduct[arraylist_]:=If[SameQ@@Dimensions/@arraylist && Length[arraylist]===Map[Length,Dimensions/@arraylist][[1]],
With[{arity=Length[arraylist]},
With[{INindices=Table[ReplacePart[Range[arity],i->arity+1],{i,arity,1,-1}],OUTindices=Range[arity]},
IndexContract[arraylist,INindices->OUTindices]
]
],
"Undefined"
]


(* ::Input:: *)
(*BMProduct[{a,b}] ==a . b*)


(* ::Input:: *)
(*BMProduct[{x,y,z}] ===P1[x,y,z]*)


(* ::Input:: *)
(*BMProduct[{a,x,p}]*)


(* ::Input:: *)
(*BMProduct[{p,q,r}]*)


(* ::Input:: *)
(*booleanarrays=Table[GenerateRandomArray["Boolean",4,2],4]*)


(* ::Input:: *)
(*BMProduct[booleanarrays]*)


(* ::Input:: *)
(*symmetricarrays=GenerateRandomSymmetricArrays[3,"Boolean",3,2];*)


(* ::Input:: *)
(*BMProduct[symmetricarrays]*)


(* ::Input:: *)
(*BMProduct[Permute[symmetricarrays,Cycles[{{1,2,3}}]]]*)


(* ::Input:: *)
(*TensorSymmetry[BMProduct[symmetricarrays]]*)


(* ::Section:: *)
(*Ternary Products*)


BM[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ijp,ipk,pjk->ijk"]
Blades[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ipq,pjq,pqk->ijk"]
TriForce[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ipq,pjr,qrk->ijk"]
Fish[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ijp,qrp,qrk->ijk"]
Boat[a_List,b_List,c_List]:=IndexContract[{a,b,c},"ipq,pjq,pjk->ijk"]


(* ::Input:: *)
(*cub=GenerateRandomArrays[9,"Integer",3,2];*)
(*\[Alpha]=GenerateArray[arra,3,2];*)
(*\[Beta]=GenerateArray[arrb,3,2];*)
(*\[Gamma]=GenerateArray[arrc,3,2];*)
(*\[Delta]=GenerateArray[arrd,3,2];*)
(*\[Epsilon]=GenerateArray[arre,3,2];*)
(*\[Phi]=GenerateArray[arrf,3,2];*)
(*\[Chi]=GenerateArray[arrx,3,2];*)
(*\[Mu]=GenerateArray[arry,3,2];*)
(*ID=IdentityArray["Integer",3,2];*)
(*ir=PartialIdentityArray["Integer",3,3,{1,2}];*)
(*ig=PartialIdentityArray["Integer",3,3,{2,3}];*)
(*ib=PartialIdentityArray["Integer",3,3,{1,3}];*)
(*AntID=Normal@LeviCivitaTensor[3];*)


(* ::Input:: *)
(*\[Alpha]*)


(* ::Input:: *)
(*TensorSymmetry[ID]*)


(* ::Text:: *)
(*Search for equations involving a given set of arrays:*)


(* ::Input:: *)
(*TriForce[ID,ID,ID]==ID*)


(* ::Input:: *)
(*BM[ID,ID,ID]==ID*)


(* ::Input:: *)
(*BM[AntID,AntID,AntID]==ZeroArray["Integer",3,3]*)


(* ::Input:: *)
(*BM[\[Alpha],ID,ID]===\[Alpha]*)


(* ::Input:: *)
(*IdEqSearch[prod_]:=With[{idc={ir,ig,ib,ID,\[Alpha]}},*)
(*With[{checks=Table[*)
(*prod[idc[[a]],idc[[b]],idc[[c]]]==idc[[d]],*)
(*{a,5},{b,5},{c,5},{d,5}]},*)
(*Position[checks,True]*)
(*]*)
(*]*)


(* ::Input:: *)
(*IdEqSearch[BM]*)


(* ::Input:: *)
(*Select[IdEqSearch[BM],ContainsAny[{5}]]*)


(* ::Input:: *)
(*BM[ir,ir,ir]==ir*)


(* ::Input:: *)
(*CubixForm/@{x,ig,ib}*)


(* ::Input:: *)
(*BM[x,ig,ib]==x*)


(* ::Input:: *)
(*BM[ig,x,ir]==x*)


(* ::Input:: *)
(*BM[ID,\[Beta],BM[ID,\[Gamma],\[Delta]]]==BM[ID,BM[ID,\[Beta],\[Gamma]],\[Delta]]*)


(* ::Input:: *)
(*BM[ir,\[Beta],BM[ir,\[Gamma],\[Delta]]]===BM[ir,BM[ir,\[Beta],\[Gamma]],\[Delta]]*)
(*BM[ig,\[Beta],BM[ig,\[Gamma],\[Delta]]]===BM[ig,BM[ig,\[Beta],\[Gamma]],\[Delta]]*)
(*BM[ib,\[Beta],BM[ib,\[Gamma],\[Delta]]]===BM[ib,BM[ib,\[Beta],\[Gamma]],\[Delta]]*)


TernaryClones[productlist_,argumentlist_,perm_]:=Switch[perm,
"Permute",Groupings[Permutations[argumentlist],Table[productlist[[i]]->3,{i,Length[productlist]}]],
"NotPermute",Groupings[{argumentlist},Table[productlist[[i]]->3,{i,Length[productlist]}]]
]


(* ::Input:: *)
(*TernaryClones[{BM},{1,2,3,4,5},"NotPermute"]*)


(* ::Input:: *)
(*TernaryClones[{BM},{1,2,3,4,5},"Permute"]*)


(* ::Input:: *)
(*TernaryClones[{BM,TriForce},{1,2,3,4,5},"NotPermute"]*)


FindTernaryCloneEquationsPairs[productlist_,arguments_]:=Block[{numarrays,symbarrays,terms,permterms,size,permsize,rules,values,permvalues,exp,permexp},
terms=TernaryClones[productlist,arguments,"NotPermute"]~Join~DeleteDuplicates[arguments];
permterms=TernaryClones[productlist,arguments,"Permute"];
size=Length@terms;
permsize=Length@permterms;
numarrays=AssociationThread[DeleteDuplicates@arguments,GenerateRandomArrays[Length[DeleteDuplicates@arguments],"Integer",3,2]];
symbarrays=AssociationThread[DeleteDuplicates@arguments,Map[GenerateArray[#,3,2]&,DeleteDuplicates@arguments]];
values=Map[ReplaceAll[numarrays],terms];
permvalues=Map[ReplaceAll[numarrays],permterms];
exp=Map[ReplaceAll[symbarrays],terms];
permexp=Map[ReplaceAll[symbarrays],permterms];
DeleteCases[
Flatten[Table[
If[values[[i]]==permvalues[[j]],
If[FullSimplify[exp[[i]]==permexp[[j]]],terms[[i]]<->permterms[[j]],Nothing],Nothing],
{i,size},{j,permsize}]],
x_<->x_]
]


(* ::Input:: *)
(*FindTernaryCloneEquationsPairs[{Fish},{a,b,c,d,e}]*)


(* ::Input:: *)
(*FindTernaryCloneEquationsPairs[{BM},{a,b,c,d,e}]*)


(* ::Input:: *)
(*FindTernaryCloneEquationsPairs[{TriForce},{a,b,c}]*)
(*FindTernaryCloneEquationsPairs[{TriForce},{a,b,c,d,e}]*)


(* ::Input:: *)
(*FindTernaryCloneEquationsPairs[{Boat},{a,b,c}]*)
(*FindTernaryCloneEquationsPairs[{Boat},{a,b,c,d,e}]*)


(* ::Input:: *)
(*FindTernaryCloneEquationsPairs[{Blades},{a,b,c}]*)
(*FindTernaryCloneEquationsPairs[{Blades},{a,b,c,d,e}]*)


(* ::Input:: *)
(*FindTernaryCloneEquationsPairs[{FishTable},{a1,a2,a3}]*)
(*FindTernaryCloneEquationsPairs[{FishTable},{a1,a2,a3,a4,a5}]*)


(* ::Input:: *)
(*FindTernaryCloneEquationsPairs[{BMTable},{a1,a2,a3,a4,a5,a6,a7}]*)


(* ::Chapter::Closed:: *)
(*Plex Diagrams*)


PlexDiagram[plexlist_List,indexlist_List,OptionsPattern[]]:=With[{indicesALL=DeleteDuplicates[Join@@plexlist]},
With[{indicesC=indexlist,indicesF=DeleteElements[indicesALL,indexlist]},
Hypergraph[indicesALL,plexlist,
VertexLabels->Automatic,
VertexSize->Thread[indicesC->0.02],
VertexStyle->Thread[indicesC->RGBColor[0.77, 0.77, 0.77]],
VertexSize->Thread[indicesF->0.02],
VertexStyle->Thread[indicesF->GrayLevel[0]],
EdgeStyle->{{__}->RGBColor[0.7,0.7,0.7,0.49]},
EdgeLabels->Thread[plexlist->Map[Subscript["#",#]&,Range[Length[plexlist]]]],
"EdgeSymmetry"-> "Directed"
]
]
]


FreeIndices[plex_]:=Keys[Select[First[AbsoluteOptions[plex,{VertexStyle}]][[2]],Part[#,2]==GrayLevel[0]&]]
ContractIndices[plex_]:=Keys[Select[First[AbsoluteOptions[plex,{VertexStyle}]][[2]],Part[#,2]==RGBColor[0.77, 0.77, 0.77]&]]


(* ::Input:: *)
(*plex1=PlexDiagram[{{1,2},{2,3}},{2}]*)


(* ::Input:: *)
(*plex2=PlexDiagram[{{1,2,3},{2,3,4}},{2,3}]*)


(* ::Input:: *)
(*plex3=PlexDiagram[{{1,2,4},{1,4,3},{4,2,3}},{4}]*)


(* ::Input:: *)
(*FreeIndices[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> "#1", {2, 3} -> "#2"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{Opacity[1], Arrowheads[{{Small, 0.5}}], {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], Line[{{0., 3.6739403974420594`*^-16}, {1., 2.4492935982947064`*^-16}}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.5, 3.176108747282852*^-16}]}}}, {Opacity[1], Arrowheads[{{Small, 0.5}}], {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], Line[{{1., 2.4492935982947064`*^-16}, {2., 0.}}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.5, 1.3391385485618224`*^-16}]}}}}, {Opacity[1], {{GrayLevel[0], Disk[{0., 3.6739403974420594`*^-16}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1., 2.4492935982947064`*^-16}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{2., 0.}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {0., 3.6739403974420594`*^-16}, ImageScaled[{1.5, 0.49999999999999983`}]], Inset[2, {1., 2.4492935982947064`*^-16}, ImageScaled[{0.5, -0.5}]], Inset[3, {2., 0.}, ImageScaled[{-0.5, 0.5000000000000002}]]}}}]], Boxed -> False, BoxID -> "Hypergraph18", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph18"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> "#1", {2, 3} -> "#2"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {207.37499999999997`, Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]]*)


(* ::Input:: *)
(*ContractIndices[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> "#1", {2, 3} -> "#2"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{Opacity[1], Arrowheads[{{Small, 0.5}}], {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], Line[{{0., 3.6739403974420594`*^-16}, {1., 2.4492935982947064`*^-16}}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.5, 3.176108747282852*^-16}]}}}, {Opacity[1], Arrowheads[{{Small, 0.5}}], {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], Line[{{1., 2.4492935982947064`*^-16}, {2., 0.}}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.5, 1.3391385485618224`*^-16}]}}}}, {Opacity[1], {{GrayLevel[0], Disk[{0., 3.6739403974420594`*^-16}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1., 2.4492935982947064`*^-16}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{2., 0.}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {0., 3.6739403974420594`*^-16}, ImageScaled[{1.5, 0.49999999999999983`}]], Inset[2, {1., 2.4492935982947064`*^-16}, ImageScaled[{0.5, -0.5}]], Inset[3, {2., 0.}, ImageScaled[{-0.5, 0.5000000000000002}]]}}}]], Boxed -> False, BoxID -> "Hypergraph18", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph18"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> "#1", {2, 3} -> "#2"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {188.25, Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]]*)


(* ::Input:: *)
(*FreeIndices[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> "#1", {1, 4, 3} -> "#2", {4, 2, 3} -> "#3"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {1.2379182674037712`, 1.1530748666634816`}, {1.5596261667601576`, 0.8124764573870834}}], BSplineCurve[{{1.5596261667601576`, 0.8124764573870834}, {1.2509730452220142`, 1.3245669387097752`}, {1.0454262759156179`, 1.8863367934217479`}}], BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {1.109368294692636, 1.4215399506721476`}, {0.9932071646426459, 1.200368505236573}}]}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.1993134428885595`, 1.2996755026601787`}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.3734718648182639, 0.9030445937495521}, {0.9709920405981166, 0.914861855513027}, {1.5596261667601579`, 0.8124764573870834}}], BSplineCurve[{{1.5596261667601576`, 0.8124764573870834}, {1.1259258655542121`, 0.9891928333847821}, {0.9932071646426459, 1.200368505236573}}], BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.8293872900687387, 1.0118348674753992`}, {0.3734718648182639, 0.9030445937495521}}]}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.9754819406773837, 0.9720195392442879}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.3734718648182639, 0.9030445937495521}, {0.7436873082613153, 1.1908115901478433`}, {0.9932071646426459, 1.200368505236573}}], BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.9116759110356538, 1.4366346400658923`}, {1.0454262759156179`, 1.8863367934217479`}}], BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {0.7567420860795584, 1.3623036621941371`}, {0.3734718648182639, 0.9030445937495521}}]}], {Opacity[1], Inset[Style["#3", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.8041441905802126, 1.3298747999373497`}]}}}, {Opacity[1], {{GrayLevel[0], Disk[{1.5596261667601576`, 0.8124764573870834}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{1.0454262759156179`, 1.8863367934217479`}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{0.9932071646426459, 1.200368505236573}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0.3734718648182639, 0.9030445937495521}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {1.5596261667601576`, 0.8124764573870834}, ImageScaled[{-0.3250748845194824, 1.0650233932636444`}]], Inset[2, {1.0454262759156179`, 1.8863367934217479`}, ImageScaled[{0.42409494341029336`, -0.49711504972300635`}]], Inset[4, {0.9932071646426459, 1.200368505236573}, ImageScaled[{0.42242499707060155`, -0.49698651892616064`}]], Inset[3, {0.3734718648182639, 0.9030445937495521}, ImageScaled[{1.401607624015636, 0.9325548431319195}]]}}}]], Boxed -> False, BoxID -> "Hypergraph20", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph20"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> "#1", {1, 4, 3} -> "#2", {4, 2, 3} -> "#3"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {143.25, Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]]*)


(* ::Input:: *)
(*ContractIndices[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> "#1", {1, 4, 3} -> "#2", {4, 2, 3} -> "#3"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {1.2379182674037712`, 1.1530748666634816`}, {1.5596261667601576`, 0.8124764573870834}}], BSplineCurve[{{1.5596261667601576`, 0.8124764573870834}, {1.2509730452220142`, 1.3245669387097752`}, {1.0454262759156179`, 1.8863367934217479`}}], BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {1.109368294692636, 1.4215399506721476`}, {0.9932071646426459, 1.200368505236573}}]}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.1993134428885595`, 1.2996755026601787`}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.3734718648182639, 0.9030445937495521}, {0.9709920405981166, 0.914861855513027}, {1.5596261667601579`, 0.8124764573870834}}], BSplineCurve[{{1.5596261667601576`, 0.8124764573870834}, {1.1259258655542121`, 0.9891928333847821}, {0.9932071646426459, 1.200368505236573}}], BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.8293872900687387, 1.0118348674753992`}, {0.3734718648182639, 0.9030445937495521}}]}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.9754819406773837, 0.9720195392442879}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.3734718648182639, 0.9030445937495521}, {0.7436873082613153, 1.1908115901478433`}, {0.9932071646426459, 1.200368505236573}}], BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.9116759110356538, 1.4366346400658923`}, {1.0454262759156179`, 1.8863367934217479`}}], BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {0.7567420860795584, 1.3623036621941371`}, {0.3734718648182639, 0.9030445937495521}}]}], {Opacity[1], Inset[Style["#3", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.8041441905802126, 1.3298747999373497`}]}}}, {Opacity[1], {{GrayLevel[0], Disk[{1.5596261667601576`, 0.8124764573870834}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{1.0454262759156179`, 1.8863367934217479`}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{0.9932071646426459, 1.200368505236573}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0.3734718648182639, 0.9030445937495521}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {1.5596261667601576`, 0.8124764573870834}, ImageScaled[{-0.3250748845194824, 1.0650233932636444`}]], Inset[2, {1.0454262759156179`, 1.8863367934217479`}, ImageScaled[{0.42409494341029336`, -0.49711504972300635`}]], Inset[4, {0.9932071646426459, 1.200368505236573}, ImageScaled[{0.42242499707060155`, -0.49698651892616064`}]], Inset[3, {0.3734718648182639, 0.9030445937495521}, ImageScaled[{1.401607624015636, 0.9325548431319195}]]}}}]], Boxed -> False, BoxID -> "Hypergraph20", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph20"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> "#1", {1, 4, 3} -> "#2", {4, 2, 3} -> "#3"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {146.625, Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]]*)


PlexToProduct[plex_]:=With[{contractV=ContractIndices[plex],
freeV=DeleteElements[VertexList[plex],ContractIndices[plex]],
arrayE=DeleteElements[EdgeList[plex],ContractIndices[plex]]},
With[{args=Array[Slot,Length[arrayE]]},
Function[IndexContract[args,arrayE->freeV]]
]
]


(* ::Input:: *)
(*PlexToProduct[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexLabels -> {All -> Automatic}, VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {2, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Directed"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{Opacity[1], Arrowheads[{{Small, 0.5}}], {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], {Arrow[Line[{{0., 3.6739403974420594`*^-16}, {1., 2.4492935982947064`*^-16}}]], {}}, {Opacity[1], Inset[Style[Subscript["#", 1], LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.5, 0.0600000000000003}]}}}, {Opacity[1], Arrowheads[{{Small, 0.5}}], {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], {Arrow[Line[{{1., 2.4492935982947064`*^-16}, {2., 0.}}]], {}}, {Opacity[1], Inset[Style[Subscript["#", 2], LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.5, 0.06000000000000012}]}}}}, {Opacity[1], {{GrayLevel[0], Disk[{0., 3.6739403974420594`*^-16}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1., 2.4492935982947064`*^-16}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{2., 0.}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {0., 3.6739403974420594`*^-16}, ImageScaled[{1.5, 0.49999999999999983`}]], Inset[2, {1., 2.4492935982947064`*^-16}, ImageScaled[{0.5, -0.5}]], Inset[3, {2., 0.}, ImageScaled[{-0.5, 0.5000000000000002}]]}}}]], BoxID -> "Hypergraph39", BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph39"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2}, {2, 3}], VertexLabels -> {All -> Automatic}, VertexSize -> {2 -> 0.02, 1 -> 0.02, 3 -> 0.02}, VertexStyle -> {2 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2} -> Subscript["#", 1], {2, 3} -> Subscript["#", 2]}, "EdgeSymmetry" -> {All -> "Directed"}]]]]]], MenuEvaluator -> Automatic]}}}, FormatType -> TraditionalForm, FrameTicks -> None, ImageMargins -> 0., ImageSize -> {131.24999999999997`, Automatic}, PlotRange -> {{0., 2.}, {-1., 1.0000000000000004`}}, PlotRangePadding -> Scaled[0.2]]][a,b]==a . b*)


(* ::Input:: *)
(*PlexToProduct[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexLabels -> {All -> Automatic}, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}, "EdgeSymmetry" -> {All -> "Directed"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{RGBColor[0.7, 0.7, 0.7], Opacity[0.49], {FilledCurve[{BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {1.2379182674037712`, 1.1530748666634816`}, {1.5596261667601576`, 0.8124764573870834}}], BSplineCurve[{{1.5596261667601576`, 0.8124764573870834}, {1.2509730452220142`, 1.3245669387097752`}, {1.0454262759156179`, 1.8863367934217479`}}], BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {1.109368294692636, 1.4215399506721476`}, {0.9932071646426459, 1.200368505236573}}]}], {{Hue[0.63, 0.7, 0.5], Opacity[0.7], Arrowheads[{{Small, 0.5}}], Arrow[BSplineCurve[{{1.5596261667601576`, 0.8124764573870834}, {1.5596261667601576`, 0.8124764573870834}, {1.2509730452220142`, 1.3245669387097752`}, {1.0454262759156179`, 1.8863367934217479`}}]]}, {Hue[0.63, 0.7, 0.5], Opacity[0.7], Arrowheads[{{Medium, 0.5}}], Arrow[BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {1.0454262759156179`, 1.8863367934217479`}, {1.109368294692636, 1.4215399506721476`}, {0.9932071646426459, 1.200368505236573}}]]}}}, {Opacity[1], Inset[Style[Subscript["#", 1], LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.199784012988236, 1.2995869391851018`}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], {FilledCurve[{BSplineCurve[{{0.3734718648182639, 0.9030445937495521}, {0.9709920405981166, 0.914861855513027}, {1.5596261667601579`, 0.8124764573870834}}], BSplineCurve[{{1.5596261667601576`, 0.8124764573870834}, {1.1259258655542121`, 0.9891928333847821}, {0.9932071646426459, 1.200368505236573}}], BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.8293872900687387, 1.0118348674753992`}, {0.3734718648182639, 0.9030445937495521}}]}], {{Hue[0.63, 0.7, 0.5], Opacity[0.7], Arrowheads[{{Small, 0.5}}], Arrow[BSplineCurve[{{1.5596261667601579`, 0.8124764573870834}, {1.5596261667601576`, 0.8124764573870834}, {1.1259258655542121`, 0.9891928333847821}, {0.9932071646426459, 1.200368505236573}}]]}, {Hue[0.63, 0.7, 0.5], Opacity[0.7], Arrowheads[{{Medium, 0.5}}], Arrow[BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.9932071646426459, 1.200368505236573}, {0.8293872900687387, 1.0118348674753992`}, {0.3734718648182639, 0.9030445937495521}}]]}}}, {Opacity[1], Inset[Style[Subscript["#", 2], LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.9755312811703947, 0.9716843921831634}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], {FilledCurve[{BSplineCurve[{{0.3734718648182639, 0.9030445937495521}, {0.7436873082613153, 1.1908115901478433`}, {0.9932071646426459, 1.200368505236573}}], BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.9116759110356538, 1.4366346400658923`}, {1.0454262759156179`, 1.8863367934217479`}}], BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {0.7567420860795584, 1.3623036621941371`}, {0.3734718648182639, 0.9030445937495521}}]}], {{Hue[0.63, 0.7, 0.5], Opacity[0.7], Arrowheads[{{Small, 0.5}}], Arrow[BSplineCurve[{{0.9932071646426459, 1.200368505236573}, {0.9932071646426459, 1.200368505236573}, {0.9116759110356538, 1.4366346400658923`}, {1.0454262759156179`, 1.8863367934217479`}}]]}, {Hue[0.63, 0.7, 0.5], Opacity[0.7], Arrowheads[{{Medium, 0.5}}], Arrow[BSplineCurve[{{1.0454262759156179`, 1.8863367934217479`}, {1.0454262759156179`, 1.8863367934217479`}, {0.7567420860795584, 1.3623036621941371`}, {0.3734718648182639, 0.9030445937495521}}]]}}}, {Opacity[1], Inset[Style[Subscript["#", 3], LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.8039084023345833, 1.3302690767923753`}]}}}, {Opacity[1], {{GrayLevel[0], Disk[{1.5596261667601576`, 0.8124764573870834}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{1.0454262759156179`, 1.8863367934217479`}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{0.9932071646426459, 1.200368505236573}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0.3734718648182639, 0.9030445937495521}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {1.5596261667601576`, 0.8124764573870834}, ImageScaled[{-0.3250748845194824, 1.0650233932636444`}]], Inset[2, {1.0454262759156179`, 1.8863367934217479`}, ImageScaled[{0.42409494341029336`, -0.49711504972300635`}]], Inset[4, {0.9932071646426459, 1.200368505236573}, ImageScaled[{0.42242499707060155`, -0.49698651892616064`}]], Inset[3, {0.3734718648182639, 0.9030445937495521}, ImageScaled[{1.401607624015636, 0.9325548431319195}]]}}}]], BoxID -> "Hypergraph41", BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph41"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {1, 4, 3}, {4, 2, 3}], VertexLabels -> {All -> Automatic}, VertexSize -> {4 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> Subscript["#", 1], {1, 4, 3} -> Subscript["#", 2], {4, 2, 3} -> Subscript["#", 3]}, "EdgeSymmetry" -> {All -> "Directed"}]]]]]], MenuEvaluator -> Automatic]}}}, FormatType -> TraditionalForm, FrameTicks -> None, ImageMargins -> 0., ImageSize -> {339.375, Automatic}, PlotRange -> {{0., 2.0793621253200887`}, {0., 1.8863367934217479`}}, PlotRangePadding -> Scaled[0.2]]][x,y,z]==BM[x,y,z]*)


(* ::Input:: *)
(*CubixForm/@{x,y,z}*)


(* ::Input:: *)
(*CubixForm@BM[x,y,z]*)


(* ::Input:: *)
(*PlexToProduct[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 4, 5, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 4, 5}, {4, 2, 5}, {4, 5, 3}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 4, 5} -> "#1", {4, 2, 5} -> "#2", {4, 5, 3} -> "#3"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{1.5119828117036656`, 0.03582031595416946}, {1.2859483021170837`, 0.671384645766534}, {1.171613268133579, 1.2155314986303463`}}], BSplineCurve[{{1.171613268133579, 1.2155314986303463`}, {1.1596275024685145`, 0.8874116513124704}, {1.0066996131093893`, 0.899928338137915}}], BSplineCurve[{{1.0066996131093893`, 0.899928338137915}, {1.2447198883610362`, 0.5924838556434262}, {1.5119828117036656`, 0.03582031595416946}}]}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.2298287444881073`, 0.7175334986232195}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{1.0066996131093893`, 0.899928338137915}, {0.9076303671845701, 1.088312088945935}, {1.171613268133579, 1.2155314986303463`}}], BSplineCurve[{{1.171613268133579, 1.2155314986303463`}, {0.6559554639072227, 1.1736357398501953`}, {0., 1.2412229417549567`}}], BSplineCurve[{{0., 1.2412229417549567`}, {0.6147270501511753, 1.0947349497270875`}, {1.0066996131093893`, 0.899928338137915}}]}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.7266487930519866, 1.1187468229898456`}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{1.0066996131093893`, 0.899928338137915}, {1.522670907048963, 1.0910539179049983`}, {2.1790652565546904`, 1.2323884653819426`}}], BSplineCurve[{{2.1790652565546904`, 1.2323884653819426`}, {1.5638993208050103`, 1.169954708028106}, {1.171613268133579, 1.2155314986303463`}}], BSplineCurve[{{1.171613268133579, 1.2155314986303463`}, {1.2708079099436853`, 1.0868396762170993`}, {1.0066996131093893`, 0.899928338137915}}]}], {Opacity[1], Inset[Style["#3", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.451924703240652, 1.1159216293575291`}]}}}, {Opacity[1], {{GrayLevel[0], Disk[{1.5119828117036656`, 0.03582031595416946}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1.0066996131093893`, 0.899928338137915}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1.171613268133579, 1.2155314986303463`}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0., 1.2412229417549567`}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{2.1790652565546904`, 1.2323884653819426`}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {1.5119828117036656`, 0.03582031595416946}, ImageScaled[{0.026682191183233395`, 1.3808917367400462`}]], Inset[4, {1.0066996131093893`, 0.899928338137915}, ImageScaled[{0.9631202708388967, 1.3862954443852833`}]], Inset[5, {1.171613268133579, 1.2155314986303463`}, ImageScaled[{0.0368797291611106, -0.3862954443852872}]], Inset[2, {0., 1.2412229417549567`}, ImageScaled[{1.4648375929781972`, 0.23715324012642125`}]], Inset[3, {2.1790652565546904`, 1.2323884653819426`}, ImageScaled[{-0.4985902208480084, 0.4469192047278339}]]}}}]], Boxed -> False, BoxID -> "Hypergraph23", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph23"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 4, 5, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 4, 5}, {4, 2, 5}, {4, 5, 3}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 4, 5} -> "#1", {4, 2, 5} -> "#2", {4, 5, 3} -> "#3"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {283.125, Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]][x,y,z]==Blades[x,y,z]*)


(* ::Input:: *)
(*PlexToProduct[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 4, 5, 2, 6, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 4, 5}, {4, 2, 6}, {3, 5, 6}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 1 -> 0.02, 2 -> 0.02, 6 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 6 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 4, 5} -> "#1", {4, 2, 6} -> "#2", {3, 5, 6} -> "#3"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{1.6489801259032233`, 0.935990408334951}, {1.7631523093210713`, 1.5412975191739178`}, {2.101097880858897, 2.064095531388159}}], BSplineCurve[{{2.101097880858897, 2.064095531388159}, {1.6518369876005468`, 1.7441924835241134`}, {1.2037188390211255`, 1.7475702657357328`}}], BSplineCurve[{{1.2037188390211255`, 1.7475702657357328`}, {1.5388075488616282`, 1.4621662027608113`}, {1.6489801259032233`, 0.935990408334951}}]}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.6511727316457425`, 1.5825753047739377`}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.8604792730985972, 0.012189538602921157`}, {1.1661582077038488`, 0.5543213826317417}, {1.6489801259032233`, 0.935990408334951}}], BSplineCurve[{{1.6489801259032233`, 0.935990408334951}, {1.131863576608835, 0.790143605092454}, {0.723300748718542, 0.95547842844577}}], BSplineCurve[{{0.723300748718542, 0.95547842844577}, {0.9347383634076785, 0.5591933876594465}, {0.8604792730985972, 0.012189538602921157`}}]}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.0775143989063254`, 0.6346149007466091}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.723300748718542, 0.95547842844577}, {0.8560509764705022, 1.4762456444118808`}, {1.2037188390211255`, 1.7475702657357328`}}], BSplineCurve[{{1.2037188390211255`, 1.7475702657357328`}, {0.754915011659328, 1.76233907005482}, {0.31875688947384495`, 2.0998521310175273`}}], BSplineCurve[{{0.31875688947384495`, 2.0998521310175273`}, {0.6348104890836821, 1.5643161107323293`}, {0.723300748718542, 0.95547842844577}}]}], {Opacity[1], Inset[Style["#3", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.7486822568920058, 1.6009884225192756`}]}}}, {Opacity[1], {{GrayLevel[0], Disk[{2.101097880858897, 2.064095531388159}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1.6489801259032233`, 0.935990408334951}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1.2037188390211255`, 1.7475702657357328`}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0.8604792730985972, 0.012189538602921157`}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0.723300748718542, 0.95547842844577}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0.31875688947384495`, 2.0998521310175273`}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {2.101097880858897, 2.064095531388159}, ImageScaled[{-0.373157706547233, 0.012562189097751808`}]], Inset[4, {1.6489801259032233`, 0.935990408334951}, ImageScaled[{-0.35505784670944485`, 1.0185326207487893`}]], Inset[5, {1.2037188390211255`, 1.7475702657357328`}, ImageScaled[{0.47568312255928397`, -0.4997043010168223}]], Inset[2, {0.8604792730985972, 0.012189538602921157`}, ImageScaled[{0.5793324604154529, 1.4968482134830912`}]], Inset[6, {0.723300748718542, 0.95547842844577}, ImageScaled[{1.2600368391012884`, 1.1498799913898892`}]], Inset[3, {0.31875688947384495`, 2.0998521310175273`}, ImageScaled[{1.3527538977763651`, -0.02231292328183565}]]}}}]], Boxed -> False, BoxID -> "Hypergraph24", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph24"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 4, 5, 2, 6, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 4, 5}, {4, 2, 6}, {3, 5, 6}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 1 -> 0.02, 2 -> 0.02, 6 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 6 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 4, 5} -> "#1", {4, 2, 6} -> "#2", {3, 5, 6} -> "#3"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {189.3273080503297, Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]][x,y,z]==TriForce[x,y,z]*)


(* ::Input:: *)
(*PlexToProduct[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 5, 6, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {5, 6, 4}, {5, 6, 3}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 6 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 6 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> "#1", {5, 6, 4} -> "#2", {5, 6, 3} -> "#3"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{3.379741574363792, 0.20326702647524203`}, {3.3488864344865013`, 0.468864201503597}, {3.721080333092927, 0.6958075844788747}}], BSplineCurve[{{3.721080333092927, 0.6958075844788747}, {3.089267000464856, 0.559422115293859}, {2.3412638382772095`, 0.5654986816362899}}], BSplineCurve[{{2.3412638382772095`, 0.5654986816362899}, {3.003932310782572, 0.4362869757929508}, {3.379741574363792, 0.20326702647524203`}}]}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {3.1476214813029166`, 0.4880770681780449}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.9150283715524979, 0.33985573780546285`}, {1.5323395926501249`, 0.5141916222364042}, {2.3412638382772095`, 0.5654986816362899}}], BSplineCurve[{{2.3412638382772095`, 0.5654986816362899}, {1.56690925759362, 0.6346686089885494}, {1.053307031326479, 0.8217636848140434}}], BSplineCurve[{{1.053307031326479, 0.8217636848140434}, {1.210350390912442, 0.5782578730308425}, {0.9150283715524979, 0.33985573780546285`}}]}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.4360962659314018`, 0.5757508275744482}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0., 0.42350803275695986`}, {0.556812993367954, 0.45502885187001674`}, {0.9150283715524979, 0.33985573780546285`}}], BSplineCurve[{{0.9150283715524979, 0.33985573780546285`}, {0.8201397511995737, 0.5545927648842877}, {1.053307031326479, 0.8217636848140434}}], BSplineCurve[{{1.053307031326479, 0.8217636848140434}, {0.5913826583114492, 0.5755058386221619}, {0., 0.42350803275695986`}}]}], {Opacity[1], Inset[Style["#3", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.6564721360349483, 0.5283809929909322}]}}}, {Opacity[1], {{GrayLevel[0], Disk[{3.721080333092927, 0.6958075844788747}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{3.379741574363792, 0.20326702647524203`}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{2.3412638382772095`, 0.5654986816362899}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1.053307031326479, 0.8217636848140434}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{0.9150283715524979, 0.33985573780546285`}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{0., 0.42350803275695986`}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {3.721080333092927, 0.6958075844788747}, ImageScaled[{-0.4972331618431739, 0.42566278912774547`}]], Inset[2, {3.379741574363792, 0.20326702647524203`}, ImageScaled[{-0.4465701607938646, 0.8224979545589052}]], Inset[4, {2.3412638382772095`, 0.5654986816362899}, ImageScaled[{-0.49970353555159874`, 0.5243483265468738}]], Inset[5, {1.053307031326479, 0.8217636848140434}, ImageScaled[{0.22418980761089796`, -0.4612121190321554}]], Inset[6, {0.9150283715524979, 0.33985573780546285`}, ImageScaled[{0.7758101923891059, 1.461212119032154}]], Inset[3, {0., 0.42350803275695986`}, ImageScaled[{1.4999755436859419`, 0.49300628639384586`}]]}}}]], Boxed -> False, BoxID -> "Hypergraph25", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph25"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 2, 4, 5, 6, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 2, 4}, {5, 6, 4}, {5, 6, 3}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 6 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 6 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 2, 4} -> "#1", {5, 6, 4} -> "#2", {5, 6, 3} -> "#3"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {297., Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]][x,y,z]==Fish[x,y,z]*)


(* ::Input:: *)
(*PlexToProduct[Graphics[DynamicNamespace["Hypergraph", DynamicModule[{$CellContext`hg = HoldComplete[WolframInstitute`Hypergraph`Hypergraph[{1, 4, 5, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 4, 5}, {4, 2, 5}, {2, 3, 4}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 6 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 6 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 4, 5} -> "#1", {4, 2, 5} -> "#2", {2, 3, 4} -> "#3"}]]}, {AbsoluteThickness[Medium], Opacity[0.5], Arrowheads[{{Medium, 0.5}}], {{RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.7445522010219944, 0.6925023684640357}, {0.868448954278848, 0.9515564452657521}, {1.3397252892472409`, 1.1620240187101634`}}], BSplineCurve[{{1.3397252892472409`, 1.1620240187101634`}, {0.6823109040233495, 1.0466865290644969`}, {0., 1.0730227036590152`}}], BSplineCurve[{{0., 1.0730227036590152`}, {0.5335176319670378, 0.9293061165029651}, {0.7445522010219944, 0.6925023684640357}}]}], {Opacity[1], Inset[Style["#1", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {0.6948652389429375, 0.9756936984794893}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{0.7445522010219944, 0.6925023684640357}, {1.3386519727639974`, 0.7704318003977642}, {1.9323224179127028`, 0.691724345006533}}], BSplineCurve[{{1.9323224179127028`, 0.691724345006533}, {1.487445244820309, 0.887812212959296}, {1.3397252892472409`, 1.1620240187101634`}}], BSplineCurve[{{1.3397252892472409`, 1.1620240187101634`}, {1.190502690597632, 0.8880067188236718}, {0.7445522010219944, 0.6925023684640357}}]}], {Opacity[1], Inset[Style["#2", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.3389021393957983`, 0.8488269878279243}]}}, {RGBColor[0.7, 0.7, 0.7], Opacity[0.49], FilledCurve[{BSplineCurve[{{1.9323224179127028`, 0.691724345006533}, {2.144323352139425, 0.9289609749982585}, {2.6781635115230205`, 1.0729723875052222`}}], BSplineCurve[{{2.6781635115230205`, 1.0729723875052222`}, {1.9961740699730597`, 1.0465358934241662`}, {1.3397252892472409`, 1.1620240187101634`}}], BSplineCurve[{{1.3397252892472409`, 1.1620240187101634`}, {1.8097137965704801`, 0.9512238827994938}, {1.9323224179127028`, 0.691724345006533}}]}], {Opacity[1], Inset[Style["#3", LineColor -> Hue[0.63, 0.7, 0.5], FrontFaceColor -> Hue[0.63, 0.7, 0.5], BackFaceColor -> Hue[0.63, 0.7, 0.5], GraphicsColor -> Hue[0.63, 0.7, 0.5], FontColor -> Hue[0.63, 0.7, 0.5]], {1.9833510811772488`, 0.9755044980994928}]}}}, {Opacity[1], {{GrayLevel[0], Disk[{0., 1.0730227036590152`}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{1.3397252892472409`, 1.1620240187101634`}, Offset[{4., 4.}, {0., 0.}]]}, {RGBColor[0.77, 0.77, 0.77], Disk[{0.7445522010219944, 0.6925023684640357}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{1.9323224179127028`, 0.691724345006533}, Offset[{4., 4.}, {0., 0.}]]}, {GrayLevel[0], Disk[{2.6781635115230205`, 1.0729723875052222`}, Offset[{4., 4.}, {0., 0.}]]}}, {Inset[1, {0., 1.0730227036590152`}, ImageScaled[{1.4290965321190314`, 0.13016269251954904`}]], Inset[4, {1.3397252892472409`, 1.1620240187101634`}, ImageScaled[{0.4972591067917081, -0.4999962437451556}]], Inset[5, {0.7445522010219944, 0.6925023684640357}, ImageScaled[{1.2851084383101625`, 1.1193583293168643`}]], Inset[2, {1.9323224179127028`, 0.691724345006533}, ImageScaled[{-0.28329888433709205`, 1.1216452829357486`}]], Inset[3, {2.6781635115230205`, 1.0729723875052222`}, ImageScaled[{-0.42896003936892924`, 0.12981998263591077`}]]}}}]], Boxed -> False, BoxID -> "Hypergraph26", AlignmentPoint -> Center, AspectRatio -> Automatic, Axes -> False, AxesLabel -> None, AxesOrigin -> Automatic, AxesStyle -> {}, Background -> None, BaseStyle -> {GraphicsHighlightColor -> RGBColor[1, 0, 0], ComponentwiseContextMenu -> {"GraphicsBox" -> {MenuItem["Draw", KernelExecute[MathLink`CallFrontEnd[FrontEnd`BoxReferenceReplace[FE`BoxReference[EvaluationNotebook[], "Hypergraph26"], ToBoxes[WolframInstitute`Hypergraph`HypergraphDraw[WolframInstitute`Hypergraph`Hypergraph[{1, 4, 5, 2, 3}, WolframInstitute`Hypergraph`Hyperedges[{1, 4, 5}, {4, 2, 5}, {2, 3, 4}], VertexSize -> {4 -> 0.02, 5 -> 0.02, 6 -> 0.02, 1 -> 0.02, 2 -> 0.02, 3 -> 0.02}, VertexStyle -> {4 -> RGBColor[0.77, 0.77, 0.77], 5 -> RGBColor[0.77, 0.77, 0.77], 6 -> RGBColor[0.77, 0.77, 0.77], 1 -> GrayLevel[0], 2 -> GrayLevel[0], 3 -> GrayLevel[0]}, VertexLabels -> {All -> Automatic}, EdgeStyle -> {{BlankSequence[]} -> RGBColor[0.7, 0.7, 0.7, 0.49]}, EdgeLabels -> {{1, 4, 5} -> "#1", {4, 2, 5} -> "#2", {2, 3, 4} -> "#3"}]]]]]], MenuEvaluator -> Automatic]}}}, BaselinePosition -> Automatic, ContentSelectable -> Automatic, Epilog -> {}, FormatType -> TraditionalForm, Frame -> False, FrameLabel -> None, FrameStyle -> {}, FrameTicks -> None, FrameTicksStyle -> {}, GridLines -> None, GridLinesStyle -> {}, ImageMargins -> 0., ImagePadding -> All, ImageSize -> {297.375, Automatic}, LabelStyle -> {}, PlotLabel -> None, PlotRange -> All, PlotRangeClipping -> False, PlotRangePadding -> Automatic, PlotRegion -> Automatic, Prolog -> {}, RotateLabel -> True, Ticks -> Automatic, TicksStyle -> {}]][x,y,z]==Boat[x,y,z]*)


(* ::Chapter::Closed:: *)
(*3D Cubix Visualization*)


(* ::Input:: *)
(*CubixForm[IdentityArray["Integer",3,3]]*)


Cubix3D[array_]:=Graphics3D[
Table[{Text[array[[i,j,k]],{i,j,k}]},{i,1,Length[array]},{j,1,Length[array]},{k,1,Length[array]}],
Axes->True,AxesLabel->{"i","j","k"},PlotRange->{{1,Length[array]},{1,Length[array]},{1,Length[array]}}, Ticks->None
]


(* ::Input:: *)
(*Cubix3DAlt[array_]:=With[{n=Length[array]},Graphics3D[{EdgeForm[Dashed],FaceForm[None],MapIndexed[With[{c=1/2 (#2-3/2)+1/2},{Cuboid[c-1/4,c+1/4],Text[#1,c]}]&,array,{ArrayDepth[array]}]},Axes->False,AxesLabel->{"i","j","k"},PlotRange->{{0,n-1},{0,n-1},{0,n-1}},Ticks->None]]*)


(* ::Input:: *)
(*Cubix3D[cub1]*)


(* ::Input:: *)
(*Cubix3D[IdentityArray["Boolean",3,3]]*)


(* ::Input:: *)
(*Cubix3D[x]*)


(* ::Input:: *)
(*Cubix3DAlt[cub1]*)


(* ::Input:: *)
(*ID3=Table[FiniteField[2,1][KroneckerDelta[i,j,k]],{i,3},{j,3},{k,3}];*)


(* ::Input:: *)
(*Cubix3D[ID]*)


(* ::Input:: *)
(*Cubix3DProduct[prod_][a_,b_,c_]:={{Cubix3D[a],Cubix3D[b],Cubix3D[c]}->Cubix3D[prod[a,b,c]]}*)


(* ::Input:: *)
(*Cubix3DProduct[BM][cub1,cub2,cub3]*)


(* ::Input:: *)
(*Cubix3DProduct[Fish][cub1,cub2,cub3]*)


(* ::Chapter:: *)
(*Boolean Multiplication Cubles*)


(* ::Text:: *)
(*IndexContract considerably slower than basic WL arithmetic with lists, define products as tables:*)


BMTable[a_List,b_List,c_List]:=If[Length[a]==Length[b]==Length[c],
With[{D=Length[a]},
Table[Sum[a[[i,j,p]]*b[[i,p,k]]*c[[p,j,k]],{p,1,D}],{i,D},{j,D},{k,D}]
],
"Cubices of different dimensions."]
BladesTable[a_List,b_List,c_List]:=If[Length[a]==Length[b]==Length[c],
With[{D=Length[a]},
Table[Sum[a[[i,p,q]]*b[[p,j,q]]*c[[p,q,k]],{p,1,D},{q,1,D}],{i,D},{j,D},{k,D}]
],
"Cubices of different dimensions."]
TriForceTable[a_List,b_List,c_List]:=If[Length[a]==Length[b]==Length[c],
With[{D=Length[a]},
Table[Sum[a[[i,p,q]]*b[[p,j,r]]*c[[q,r,k]],{p,1,D},{q,1,D},{r,1,D}],{i,D},{j,D},{k,D}]
],
"Cubices of different dimensions."]
FishTable[a_List,b_List,c_List]:=If[Length[a]==Length[b]==Length[c],
With[{D=Length[a]},
Table[Sum[a[[i,j,p]]*b[[q,r,p]]*c[[q,r,k]],{p,1,D},{q,1,D},{r,1,D}],{i,D},{j,D},{k,D}]
],
"Cubices of different dimensions."]
BoatTable[a_List,b_List,c_List]:=If[Length[a]==Length[b]==Length[c],
With[{D=Length[a]},
Table[Sum[a[[i,p,q]]*b[[p,j,q]]*c[[p,j,k]],{p,1,D},{q,1,D}],{i,D},{j,D},{k,D}]
],
"Cubices of different dimensions."]


(* ::Input:: *)
(*BooleanValues={FiniteField[2,1][0],FiniteField[2,1][1]}*)


(* ::Input:: *)
(*CubixSet=Tuples[BooleanValues,{2,2,2}];*)
(*Length[CubixSet]==Echo[2^8]*)


(* ::Input:: *)
(*cub1=RandomChoice[CubixSet];*)
(*cub2=RandomChoice[CubixSet];*)
(*cub3=RandomChoice[CubixSet];*)
(*cub4=RandomChoice[CubixSet];*)
(*cub5=RandomChoice[CubixSet];*)
(*cub6=RandomChoice[CubixSet];*)
(*cub7=RandomChoice[CubixSet];*)
(*cub1//MatrixForm*)


(* ::Input:: *)
(*MultCuble[prod_,n_,set_]:=With[{CubList=Take[set,n]},*)
(*With[{ArgList=Tuples[CubList,3]},*)
(*With[{OutList=prod@@@ArgList},*)
(*Length[OutList]*)
(*]*)
(*]*)
(*]*)
(*TIMEMultCuble[prod_,n_,set_]:=First[MultCuble[prod,n,set];//RepeatedTiming]*)


(* ::Input:: *)
(*MultCuble[BM,1,CubixSet]*)
(*TIMEMultCuble[BM,1,CubixSet]*)


(* ::Input:: *)
(*MultCuble[BM,4,CubixSet]*)
(*TIMEMultCuble[BM,4,CubixSet]*)


(* ::Input:: *)
(*MultCuble[BM,50,CubixSet]*)


(* ::Input:: *)
(*BMPMultTable[Length[CubixSet],CubixSet]*)


(* ::Chapter:: *)
(*Commutators & Lie Brackets*)


(* ::Section:: *)
(*2-Arrays*)


Bin1Comm[binopp_]:=binopp[#1,#2]+binopp[#2,#1]&
BinZ2Comm[binopp_]:=binopp[#1,#2]-binopp[#2,#1]&


(* ::Input:: *)
(*MatMult[x_,y_]:=x . y*)
(*MatComm[x_,y_]:=BinZ2Comm[MatMult][x,y]*)


(* ::Input:: *)
(*Bin1Comm[MatMult][a,b]==MatMult[a,b]+MatMult[b,a]*)


BinJacobiator[binopp_]:=binopp[#1,binopp[#2,#3]]-binopp[binopp[#1,#2],#3]-binopp[#2,binopp[#1,#3]]&


(* ::Input:: *)
(*BinJacobiator[MatComm][a,b,c]*)


(* ::Input:: *)
(*UnBinComm[unopp_,binopp_]:=unopp[#1]*BinZ2Comm[binopp][#2,#3]+unopp[#2]*BinZ2Comm[binopp][#3,#1]+unopp[#3]*BinZ2Comm[binopp][#1,#2]&*)


(* ::Input:: *)
(*UnBinComm[Tr,MatMult][a,b,c]*)


(* ::Input:: *)
(*Epsilon4D=Normal[LeviCivitaTensor[4]];*)
(*Cross4D[x_,y_,z_]:=Table[Sum[Epsilon4D[[i,p,r,q]]*x[[p]]*y[[r]]*z[[q]],{p,4},{r,4},{q,4}],{i,4}]*)


(* ::Input:: *)
(*Cross4D[v1,v1,v3]*)


(* ::Input:: *)
(*Cross4D[v1,v2,v3]==-Cross4D[v1,v3,v2]*)


TerJacobiator[teropp_]:=teropp[#1,#2,teropp[#3,#4,#5]]-teropp[teropp[#1,#2,#3],#4,#5]-teropp[#3,teropp[#1,#2,#4],#5]-teropp[#3,#4,teropp[#1,#2,#5]]&


(* ::Input:: *)
(*TerJacobiator[Cross4D][v1,v2,v3,v4,v5]*)


(* ::Input:: *)
(*TerJacobiator[UnBinComm[Tr,MatMult]][a,b,c,d,e]*)


DoubleMatMult[x_,y_,z_]:=MatMult[MatMult[x,y],z]
DoubleComm[x_,y_,z_]:=MatComm[MatComm[x,y],z]
AsymmDoubleComm[x_,y_,z_]:=TerZ2Comm[DoubleComm][x,y,z]


(* ::Input:: *)
(*TerJacobiator[DoubleMatMult][a,b,c,d,e]*)


(* ::Input:: *)
(*TerJacobiator[DoubleComm][a,b,c,d,e]*)


(* ::Input:: *)
(*AsymmDoubleComm[a,b,c]==DoubleComm[a,b,c]*)


(* ::Input:: *)
(*TerJacobiator[AsymmDoubleComm][a,b,c,d,e]*)


(* ::Section:: *)
(*3 -Arrays*)


j=Exp[2*Pi*I/3];


(* ::Input:: *)
(*Simplify[1+j+j^2]*)


Ter1Comm[ternaryopp_]:=ternaryopp[#1,#2,#3]+ternaryopp[#2,#3,#1]+ternaryopp[#3,#1,#2]+ternaryopp[#2,#1,#3]+ternaryopp[#1,#3,#2]+ternaryopp[#3,#2,#1]&
TerZ2Comm[ternaryopp_]:=ternaryopp[#1,#2,#3]+ternaryopp[#2,#3,#1]+ternaryopp[#3,#1,#2]-ternaryopp[#2,#1,#3]-ternaryopp[#1,#3,#2]-ternaryopp[#3,#2,#1]&
TerZ3Comm[ternaryopp_]:=ternaryopp[#1,#2,#3]+j*ternaryopp[#2,#3,#1]+j^2*ternaryopp[#3,#1,#2]&
TerZ2Z3Comm[ternaryopp_]:=ternaryopp[#1,#2,#3]+j*ternaryopp[#2,#3,#1]+j^2*ternaryopp[#3,#1,#2]-ternaryopp[#2,#1,#3]-j*ternaryopp[#1,#3,#2]-j^2*ternaryopp[#3,#2,#1]&
TerS3Comm[ternaryopp_]:=ternaryopp[#1,#2,#3]+j*ternaryopp[#2,#3,#1]+j^2*ternaryopp[#3,#1,#2]+ternaryopp[#2,#1,#3]\[Conjugate]+j*ternaryopp[#1,#3,#2]\[Conjugate]+j^2*ternaryopp[#3,#2,#1]\[Conjugate]&

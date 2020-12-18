(* ::Package:: *)

(* ::Title:: *)
(*TicTacToe *)


(*
	Title: TicTacToe
	Author: Soumya Sambeet Mohapatra
	Date: 20 November 2020
*)


(* ::Section:: *)
(*Package header*)


BeginPackage["TicTacToe`"]


(* ::Section:: *)
(*Function definitions*)


(* ::Subsection:: *)
(*Main game board*)


startGame[] := DynamicModule[
	{label, chance, winCode},
	label = Range[9];
	chance = 1;
	winCode = Null;
	Grid[
		Partition[
			Dynamic[
				Button[If[IntegerQ[label[[#]]],Null,label[[#]]],
					If[
						OddQ[chance],
						label[[#]]= Style["X", Bold, Red, 20],
						label[[#]]= Style["O", Bold, Blue, 20]
					];
					chance++;
					
					(* Checking rows *)
					If[
						First[checkRows[label]],
						winCode = Last[checkRows[label]]
					];
					
					(* Checking columns *)
					If[
						First[checkColumns[label]],
						winCode = Last[checkColumns[label]]
					];
					Print[winCode],
					Enabled -> Dynamic[IntegerQ[label[[#]]]],
					ImageSize -> {40,40},
					Background -> Dynamic[
						Switch[
							winCode,
							1, If[#==1 || #==2 || #==3, Green, LightBlue],
							2, If[#==4 || #==5 || #==6, Green, LightBlue],
							3, If[#==7 || #==8 || #==9, Green, LightBlue],
							4, If[#==1 || #==4 || #==7, Green, LightBlue],
							5, If[#==2 || #==5 || #==8, Green, LightBlue],
							6, If[#==3 || #==6 || #==9, Green, LightBlue],
							Null, LightBlue
						]
					],
					Appearance -> None
				]
			]& /@ Range[9], 
		3], Spacings -> {0,0}
	]
]


(* ::Subsection:: *)
(*Check Rows*)


checkRows[l_List] := Module[
	{row1, row2, row3},
	row1 = SameQ[Sequence@@l[[1;;3]]];
	row2 = SameQ[Sequence@@l[[4;;6]]];
	row3 = SameQ[Sequence@@l[[7;;9]]];
	If[row1, Return[{True, 1}]];
	If[row2, Return[{True, 2}]];
	If[row3, Return[{True, 3}], {False}]
]


(* ::Subsection:: *)
(*Check Columns*)


checkColumns[l_List] := 
	Module[
		{columnList, col1, col2, col3},
		columnList = Flatten[Transpose[Partition[l, 3]]];
		col1 = SameQ[Sequence @@ columnList[[1;;3]]];
		col2 = SameQ[Sequence @@ columnList[[4;;6]]];
		col3 = SameQ[Sequence @@ columnList[[7;;9]]];
		If[col1, Return[{True, 4}]];
		If[col2, Return[{True, 5}]];
		If[col3, Return[{True, 6}], {False}]
	]


(* ::Section:: *)
(*Package footer*)


EndPackage[]

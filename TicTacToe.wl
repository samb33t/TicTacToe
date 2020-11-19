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
	{label, chance},
	label = Table[Null, 9];
	chance = 1;
	Grid[
		Partition[
			Dynamic[
				Button[label[[#]],
					If[
						OddQ[chance],
						label[[#]]="X";
						chance++,
						label[[#]]="O";
						chance++
					], 
					Enabled -> Dynamic[label[[#]] == Null]
				]
			]& /@ Range[9],
		3]
	]
]


(* ::Subsection:: *)
(*Check Rows*)


checkRows[l_List] := 
	If[
		SameQ[Sequence@@l[[1;;3]]]||
		SameQ[Sequence@@l[[4;;6]]]||
		SameQ[Sequence@@l[[7;;9]]],
		True,
		False
	]


(* ::Subsection:: *)
(*Check Columns*)


checkColumns[l_List] := 
	Module[
		{columnList},
		columnList = Flatten[Transpose[Partition[l, 3]]];
		If[
			SameQ[Sequence @@ columnList[[1;;3]]]||
			SameQ[Sequence @@ columnList[[4;;6]]]||
			SameQ[Sequence @@ columnList[[7;;9]]],
			True,
			False
		]
	]


(* ::Section:: *)
(*Package footer*)


EndPackage[]

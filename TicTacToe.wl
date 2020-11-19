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
(*Check Rows*)


checkRows[l_List] := 
	If[
		SameQ[Sequence@@l[[1;;3]]]||
		SameQ[Sequence@@l[[4;;6]]]||
		SameQ[Sequence@@l[[7;;9]]],
		True,
		False
	]


(* ::Section:: *)
(*Package footer*)


EndPackage[]

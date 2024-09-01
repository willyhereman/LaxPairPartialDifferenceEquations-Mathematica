(* ::Package:: *)

(* ## ## ## ## ##       Function: convert2Print       ## ## ## ## ## *)

(**********************************************************************)
(* convert2Print[ddeEquationList]                   *)
(* Purpose:                                       *)
(* Input:   List of discrete differential functions                   *)
(* Output:                   *)
(* Code is in File:  cvt2prt.m                                       *)
(*                                                                    *)
(* Last Modified:                                   *)
(**********************************************************************)

convert2Print[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList},

  prtEQ = ddEQ /. int2prtRules;

  Return[prtEQ];

]; (* end Module convert2Print *)



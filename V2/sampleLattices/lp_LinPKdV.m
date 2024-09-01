(* ::Package:: *)

(**********************************************************************)
(* data file lp_LinKdV.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman  *)

(*   *)
(*** Linearized version of pKdV:  Nijhoff's Lecture 4, eq. 4.2.3  ***)

ddEQ = {(p + q)(x[0,1,0] - x[1,0,0]) - (p - q)(x[1,1,0] - x[0,0,0])};

nameINPUT = "Linearized pKdV (Lecture 4)";

ddeEquationListINPUT = ddEQ;

explicitScalars = False;


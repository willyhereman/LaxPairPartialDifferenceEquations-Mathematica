(* ::Package:: *)

(**********************************************************************)
(* data file lp_H3d0.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***  H3 eq. w/ delta=0 (ABS classification 2003)   ***)


ddEQ = {p*(x[0,0,0]*x[1,0,0] + x[0,1,0]*x[1,1,0]) - q*(x[0,0,0]*x[0,1,0] + x[1,0,0]*x[1,1,0])};

nameINPUT = "H3 eq. w/ delta=0 (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

(* weightedParametersINPUT = {}; *)


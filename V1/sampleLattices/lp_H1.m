(* ::Package:: *)

(**********************************************************************)
(* data file lp_H1.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***  H1 equation (ABS classification   ***)


ddEQ = {(x[0,0,0] - x[1,1,0])*(x[1,0,0] - x[0,1,0]) + q - p};

nameINPUT = "H1 equation (ABS classification)";
ddeEquationListINPUT = ddEQ;

(* weightedParametersINPUT = {}; *)


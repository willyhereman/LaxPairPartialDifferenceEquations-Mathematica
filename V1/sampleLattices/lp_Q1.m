(* ::Package:: *)

(**********************************************************************)
(* data file lp_Q1.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***  Q1 equation (ABS classification 2003)   ***)


ddEQ = {p*(x[0,0,0] - x[0,1,0])*(x[1,0,0] - x[1,1,0])
        - q*(x[0,0,0] - x[1,0,0])*(x[0,1,0] - x[1,1,0])
        + d^2*p*q*(p-q)};
 
nameINPUT = "Q1 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

(* weightedParametersINPUT = {}; *)


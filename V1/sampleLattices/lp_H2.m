(* ::Package:: *)

(**********************************************************************)
(* data file lp_H2.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***  H2 equation (ABS classification 2003)   ***)


ddEQ = {(x[0,0,0] - x[1,1,0])*(x[1,0,0] - x[0,1,0]) 
        + (q - p)*(x[0,0,0] + x[1,0,0] + x[0,1,0] + x[1,1,0]) + q^2 -p^2};

nameINPUT = "H2 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

(* weightedParametersINPUT = {}; *)

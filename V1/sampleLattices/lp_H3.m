(* ::Package:: *)

(**********************************************************************)
(* data file lp_H3.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***  H3 equation (ABS classification 2003)   ***)


ddEQ = {p*(x[0,0,0]*x[1,0,0] + x[0,1,0]*x[1,1,0]) - q*(x[0,0,0]*x[0,1,0] + x[1,0,0]*x[1,1,0])
         + d*(p^2 - q^2)};
 
nameINPUT = "H3 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

(* weightedParametersINPUT = {}; *)


(* ::Package:: *)

(**********************************************************************)
(* data file lp_tran.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  Q1 equation (ABS classification 2003)   ***)

ddEQ = {p*(beta1*x[0,0,0] - beta2*x[0,1,0])*(beta1*x[1,0,0] - beta2*x[1,1,0])
        - q*(alpha1*x[0,0,0] - alpha2*x[1,0,0])*(alpha1*x[0,1,0] - alpha2*x[1,1,0])};
 
nameINPUT = "Q1 equation Variation";
ddeEquationListINPUT = ddEQ;

explicitScalars = False;
(* no info for tFunc and sFunc is given *)

(* weightedParametersINPUT = {}; *)


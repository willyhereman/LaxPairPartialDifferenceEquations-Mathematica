(* ::Package:: *)

(**********************************************************************)
(* data file lp_dLatVol.m *)
(*   *)
(* Last Updated: Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  Discrete Lotka-Volterra Equation  ***)

ddEQ = {x[1,1,0] - (x[0,0,0]*(x[0,1,0] - 1))/(x[1,0,0] - 1)};
 
nameINPUT = "Discrete Lotka-Volterra equation";
ddeEquationListINPUT = ddEQ;

explicitScalars = False; 
(* no info for tFunc and sFunc is given *)

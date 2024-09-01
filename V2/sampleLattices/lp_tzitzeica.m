(* ::Package:: *)

(**********************************************************************)
(* data file lp_tzitzeica.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***    Tzitzeica Equation ***)
(***    Habibullin and Poptsova 2014   ***)


ddEQ = {(x[0,0,0]*x[1,1,1]*(p*x[1,0,0]*x[0,1,0] - x[1,0,0] - x[0,1,0]) + x[1,1,0] + x[0,0,0] - p)};

nameINPUT = "Tzitzeica Equation";
ddeEquationListINPUT = ddEQ;

explicitScalars = False;
(* no info for tFunc and sFunc is given *)

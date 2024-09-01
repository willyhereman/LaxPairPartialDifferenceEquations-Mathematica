(* ::Package:: *)

(**********************************************************************)
(* data file lp_qdsys.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***    Quotient-Difference System ***)
(* NOTE: This system is not consistent about the cube *)

ddEQ = {x[0,1,0] + y[1,1,0] - x[1,0,0] + y[1,0,0],
        x[0,1,0]*y[0,1,0] - x[0,0,0]*y[1,0,0]};

nameINPUT = "Quotient Difference System";
ddeEquationListINPUT = ddEQ;

explicitScalars = False;
(* no info for tFunc and sFunc is given *)

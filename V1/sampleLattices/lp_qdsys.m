(* ::Package:: *)

(**********************************************************************)
(* data file lp_qdsys.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***    Quotient-Difference System ***)
(* This system is not consistent about the cube *)


ddEQ = {x[0,1,0] + y[1,1,0] - x[1,0,0] + y[1,0,0],
        x[0,1,0]*y[0,1,0] - x[0,0,0]*y[1,0,0]};

nameINPUT = "Quotient Difference System";
ddeEquationListINPUT = ddEQ;



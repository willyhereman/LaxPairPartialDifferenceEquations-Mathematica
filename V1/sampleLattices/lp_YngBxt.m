(* ::Package:: *)

(**********************************************************************)
(* data file lp_YngBxts.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***    Yang Baxter ***)
(***    Papageorgiou and Tongas 2009   ***)


ddEQ = {x[1,1,0] - x[0,0,0] + (p - q)/(1 - x[1,0,0]*y[0,1,0])*x[1,0,0],
        y[1,1,0] - y[0,0,0] + (p - q)/(1 - x[1,0,0]*y[0,1,0])*y[0,1,0]};

nameINPUT = "Yang-Baxter System";
ddeEquationListINPUT = ddEQ;



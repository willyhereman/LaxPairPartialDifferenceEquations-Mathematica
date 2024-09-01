(* ::Package:: *)

(**********************************************************************)
(* data file lp_nzzA2.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***   Nijhoff/Zhang/Zhou A-2 system, also treated in our paper ***)


ddEQ = {z[0,0,0]*x[1,0,0] - y[1,0,0] - x[0,0,0], 
        z[0,0,0]*x[0,1,0] - y[0,1,0] - x[0,0,0], 
        y[0,0,0] - x[0,0,0]*z[1,1,0] + b0*x[0,0,0] + 
        (G[-p,-a]*x[1,0,0] - G[-q,-a]*x[0,1,0])/(z[0,1,0] - z[1,0,0])};

nameINPUT = "Zhang, Zhou, Nijhoff A-2";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {};
laxPairMatrixM = {};

explicitScalars = False;
(* no info for tFunc and sFunc is given *)


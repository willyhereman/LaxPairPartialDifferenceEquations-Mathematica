(* ::Package:: *)

(**********************************************************************)
(* data file lp_H _A3c.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***   Hietarinta derived:  Case A, 3 component ***)
(***   Hietarinta 2010  ***)

ddEQ = {x[1,0,0]*z[0,0,0] - y[1,0,0] - x[0,0,0], 
        x[0,1,0]*z[0,0,0] - y[0,1,0] - x[0,0,0], 
        z[1,1,0] - y[0,0,0]/x[1,0,0] - 1/x[0,0,0]*(p*x[1,0,0] - q*x[0,1,0])/(z[1,0,0] - z[0,1,0])};

nameINPUT = "Hietarinta Case A - 3 component";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {};
laxPairMatrixM = {};

explicitScalars = False;



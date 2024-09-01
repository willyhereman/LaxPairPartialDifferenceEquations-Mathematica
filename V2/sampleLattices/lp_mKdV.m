(* ::Package:: *)

(**********************************************************************)
(* data file lp_mKdV.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***   discrete modified KORTEWEG-DE VRIES EQUATION   ***)

ddEQ = {p*(x[0,0,0]*x[0,1,0] - x[1,0,0]*x[1,1,0])-q*(x[0,0,0]*x[1,0,0]-x[0,1,0]*x[1,1,0])};

nameINPUT = "Discrete Modified Korteweg-de Vries (mKdV) Equation";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{-p*x[0,0,0], k*x[0,0,0]*x[1,0,0]},
                  {k, -p*x[1,0,0]}};

laxPairMatrixM = {{-q*x[0,0,0], k*x[0,0,0]*x[0,1,0]},
                  {k, -q*x[0,1,0]}};

explicitScalars = True;

sFunc = 1/x[0,1,0];
tFunc = 1/x[1,0,0];

(* Also works: 
sFunc = 1/x[0,0,0];
tFunc = 1/x[0,0,0];
*)


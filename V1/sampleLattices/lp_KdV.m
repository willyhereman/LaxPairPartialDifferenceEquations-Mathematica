(* ::Package:: *)

(**********************************************************************)
(* data file lp_KdV.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***   KORTEWEG-DE VRIES EQUATION   ***)


ddEQ = {(x[0,0,0] - x[1,1,0])(x[1,0,0]-x[0,1,0]) - p^2 + q^2};

nameINPUT = "Korteweg-de Vries (KdV) Equation";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{x[0,0,0], p^2 - k^2 - x[0,0,0]*x[1,0,0]},
                  {1, -x[1,0,0]}};
laxPairMatrixM = {{x[0,0,0], q^2 - k^2 - x[0,0,0]*x[0,1,0]},
                  {1, -x[0,1,0]}};
explicitScalars = False;


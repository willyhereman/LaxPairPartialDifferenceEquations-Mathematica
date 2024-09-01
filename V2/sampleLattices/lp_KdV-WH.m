(* ::Package:: *)



(**********************************************************************)

(* data file lp_KdV.m *)

(*   *)

(* Last Updated: June 26, 2017 by Hereman *)

(*   *)

(***   KORTEWEG-DE VRIES EQUATION   ***)




ddEQ = {(x[0,0,0] - x[1,1,0])(x[1,0,0]-x[0,1,0]) - p^2 + q^2};


nameINPUT = "Korteweg-de Vries (KdV) Equation";

ddeEquationListINPUT = ddEQ;



laxPairMatrixL = {{x[0,0,0], p^2 - k^2 - x[0,0,0]*x[1,0,0]},
 {1, - x[1,0,0]}};


laxPairMatrixM = {{x[0,0,0], q^2 - k^2 - x[0,0,0]*x[0,1,0]},
 {1, - x[0,1,0]}};


(* Setting explicitScalars to False or True is mandatory: *)
(* Set to False if no information about the sclalars t and s is given by the user. *)
(* Set to True if specific expressions for sclalars t and s should be used. *)

explicitScalars = False;


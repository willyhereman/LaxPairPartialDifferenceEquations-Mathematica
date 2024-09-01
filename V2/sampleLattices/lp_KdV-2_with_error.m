(* ::Package:: *)


(**********************************************************************)
(* data file lp_KdV-2_with_error.m *)
(* Purposely changed matrix L for testing purposes (added parameter kkk which should be 1)  *)
(* Last Updated: June 26, 2017 by Hereman *)
(* For testing purposes: Partial info is given: the Lax matrices are given but not the functions  *)
(***   KORTEWEG-DE VRIES EQUATION   ***)

ddEQ = {(x[0,0,0] - x[1,1,0])(x[1,0,0]-x[0,1,0]) - p^2 + q^2};

nameINPUT = "Korteweg-de Vries (KdV) Equation";
ddeEquationListINPUT = ddEQ;

(* *)
laxPairMatrixL = {{x[0,0,0], p^2 - k^2 - kkk*x[0,0,0]*x[1,0,0]},
 {1, - x[1,0,0]}};
(* *)

(* *)
laxPairMatrixM = {{x[0,0,0], q^2 - k^2 - x[0,0,0]*x[0,1,0]},
 {1, - x[0,1,0]}};
(* *)

(* Setting explicitScalars to False or True is mandatory: *)
(* Set to False if no information about the sclalars t and s is given by the user. *)
(* Set to True if specific expressions for sclalars t and s should be used. *)

explicitScalars = False;

(*
tFunc = xxx;
sFunc = xxx;
*)

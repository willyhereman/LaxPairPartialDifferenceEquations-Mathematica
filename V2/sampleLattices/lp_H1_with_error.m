(* ::Package:: *)

(**********************************************************************)
(* data file lp_H1_with_error.m *)
(* Purposely included an error (in this case a parameter kkk*) for testing purposes  *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  H1 equation (ABS classification   ***)

ddEQ = {(x[0,0,0] - x[1,1,0])*(x[1,0,0] - x[0,1,0]) + q - p};

nameINPUT = "H1 equation (ABS classification)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{x[0,0,0], p - k - x[0,0,0]*x[1,0,0]},
                  {1, -x[1,0,0]}};

laxPairMatrixM = {{x[0,0,0], q - k - kkk*x[0,0,0]*x[0,1,0]},
                  {1, -x[0,1,0]}};

explicitScalars = False;
(* no info for tFunc and sFunc is given *)


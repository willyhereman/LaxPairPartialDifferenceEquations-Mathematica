(* ::Package:: *)

(**********************************************************************)
(* data file lp_todambsq.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***   Toda - Modified Boussinesq System ***)
(***   (A .4a) - (A .4c)   ***)
(*  in The lattice Gel'fand-Dikii hierarchy  *)
(* Inverse Problems Vol. 8, 1992 597 - 621  *)



ddEQ = {y[1,1,0]*(p - q + x[0,1,0] - x[1,0,0]) - (p - 1)*y[0,1,0] + (q - 1)*y[1,0,0], 
        y[1,0,0]*y[0,1,0]*(p - q - z[0,1,0] + z[1,0,0]) - (p - 1)*y[0,0,0]*y[0,1,0] + (q - 1)*y[0,0,0]*y[1,0,0], 
        y[0,0,0]*(p + q - z[0,0,0] - x[1,1,0])*(p - q + x[0,1,0] - x[1,0,0]) - (p^2 + p + 1)*y[1,0,0] + (q^2 + q + 1)*y[0,1,0]};

nameINPUT = "Toda-Modified BSQ";
ddeEquationListINPUT = ddEQ;

(*
laxPairMatrixL = {{-x[1,0,0], 1, 0},
                  {-y[1,0,0], 0, 1},
                  {-k + p - x[0,0,0]*y[1,0,0] + x[1,0,0]*z[0,0,0], -z[0,0,0], x[0,0,0]}};
laxPairMatrixM = {{-x[0,1,0], 1, 0},
                  {-y[0,1,0], 0, 1},
                  {-k + q - x[0,0,0]*y[0,1,0] + x[0,1,0]*z[0,0,0], -z[0,0,0], x[0,0,0]}};
explicitScalars = False;
*)


(* ::Package:: *)

(**********************************************************************)
(* data file lp_pKdVsys.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***    System of pKdV equations ***)
(***    Pavlos Xenitidis and Sasha Mikhailov 2009   ***)

ddEQ = {y[1,0,0] - y[0,1,0] - y[0,0,0]*((x[1,0,0] - x[0,1,0])*y[0,0,0] + p - q),
        x[1,0,0] - x[0,1,0] + x[1,1,0]*((x[1,0,0] - x[0,1,0])*y[0,0,0] + p - q)};

nameINPUT = "NLS System";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{-1, x[1,0,0]},
                  {y[0,0,0], k-p - x[1,0,0]*y[0,0,0]}};

laxPairMatrixM = {{-1, x[0,1,0]},
                  {y[0,0,0], k-q - x[0,1,0]*y[0,0,0]}};

explicitScalars = False;

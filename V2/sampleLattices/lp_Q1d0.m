(* ::Package:: *)

(**********************************************************************)
(* data file lp_Q1d0.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  Q1 eq. w/ delta = d = 0 (ABS classification 2003)   ***)

ddEQ = {p*(x[0,0,0] - x[0,1,0])*(x[1,0,0] - x[1,1,0]) - q*(x[0,0,0] - x[1,0,0])*(x[0,1,0] - x[1,1,0])};
 
nameINPUT = "Q1 eq. w/ delta = 0 (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{k*(x[0,0,0]-x[1,0,0]) + p*x[1,0,0], -p*x[0,0,0]*x[1,0,0]},
                  {p, k*(x[0,0,0]-x[1,0,0])-p*x[0,0,0]}};

laxPairMatrixM = {{k*(x[0,0,0]-x[0,1,0]) + q*x[0,1,0], -q*x[0,0,0]*x[0,1,0]},
                 {q, k*(x[0,0,0]-x[0,1,0])-q*x[0,0,0]}};

explicitScalars = True;
tFunc = 1/(x[0,0,0] - x[1,0,0]);
sFunc = 1/(x[0,0,0] - x[0,1,0]); 


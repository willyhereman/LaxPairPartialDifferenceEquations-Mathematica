(* ::Package:: *)

(**********************************************************************)
(* data file lp_H3d0.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  H3 eq. w/ delta = d = 0 (ABS classification 2003)   ***)


ddEQ = {p*(x[0,0,0]*x[1,0,0] + x[0,1,0]*x[1,1,0]) - q*(x[0,0,0]*x[0,1,0] + x[1,0,0]*x[1,1,0])};

nameINPUT = "H3 eq. w/ delta=0 (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{-k*x[0,0,0], p*x[0,0,0]*x[1,0,0]},
                  {-p, k*x[1,0,0]}};

laxPairMatrixM = {{-k*x[0,0,0], q*x[0,0,0]*x[0,1,0]},
                  {-q, k*x[0,1,0]}};

explicitScalars = True;

sFunc = 1/x[0,0,0];

tFunc = 1/x[0,0,0];

(* Also work: 
sFunc = 1/x[0,1,0];
tFunc = 1/x[1,0,0];
*)

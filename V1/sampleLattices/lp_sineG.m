(* ::Package:: *)

(**********************************************************************)
(* data file lp_sineG.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***  Discrete sine-Gordon Equation  ***)


ddEQ = {x[0,0,0]*x[1,0,0]*x[0,1,0]*x[1,1,0]
        - p*q*(x[0,0,0]*x[1,1,0] - x[1,0,0]*x[0,1,0]) - 1};
 
nameINPUT = "Discrete sine-Gordon equation";
ddeEquationListINPUT = ddEQ;

explicitScalars = True;
laxPairMatrixL = {{p, -k*x[1,0,0]},
                  {-k/x[0,0,0], (p*x[1,0,0])/x[0,0,0]}};
laxPairMatrixM = {{(q*x[0,1,0])/x[0,0,0], -1/(k*x[0,0,0])},
                  {-x[0,1,0]/k, q}};
sFunc = 1;
tFunc = 1;

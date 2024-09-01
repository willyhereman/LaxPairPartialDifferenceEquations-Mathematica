(* ::Package:: *)

(**********************************************************************)
(* data file lp_sineG.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  Discrete sine-Gordon Equation  ***)
(* This equation is not consistent around the cube but has a valid Lax pair *)

ddEQ = {x[0,0,0]*x[1,0,0]*x[0,1,0]*x[1,1,0]
        - p*q*(x[0,0,0]*x[1,1,0] - x[1,0,0]*x[0,1,0]) - 1};
 
nameINPUT = "Discrete sine-Gordon equation";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{p, -k*x[1,0,0]},
                  {-k/x[0,0,0], (p*x[1,0,0])/x[0,0,0]}};

laxPairMatrixM = {{(q*x[0,1,0])/x[0,0,0], -1/(k*x[0,0,0])},
                  {-x[0,1,0]/k, q}};

explicitScalars = True;

sFunc = 1;
tFunc = 1;

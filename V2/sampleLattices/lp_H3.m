(* ::Package:: *)

(**********************************************************************)
(* data file lp_H3.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  H3 equation with delta = d not zero (ABS classification 2003)   ***)


ddEQ = {p*(x[0,0,0]*x[1,0,0] + x[0,1,0]*x[1,1,0]) - q*(x[0,0,0]*x[0,1,0] + x[1,0,0]*x[1,1,0])
         + d*(p^2 - q^2)};
 
nameINPUT = "H3 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{-k*x[0,0,0], d*(p^2-k^2)+p*x[0,0,0]*x[1,0,0]},
                  {-p, k*x[1,0,0]}};

laxPairMatrixM = {{-k*x[0,0,0], d*(q^2-k^2)+q*x[0,0,0]*x[0,1,0]},
                  {-q, k*x[0,1,0]}};

explicitScalars = True;

sFunc = 1/Sqrt[(q^2-k^2)*(d*q+x[0,0,0]*x[0,1,0])];

tFunc = 1/Sqrt[(p^2-k^2)*(d*p+x[0,0,0]*x[1,0,0])];


(* ::Package:: *)

(**********************************************************************)
(* data file lp_Q2.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  Q2 equation (ABS classification 2003)   ***)

ddEQ = {p*(x[0,0,0] - x[0,1,0])*(x[1,0,0] - x[1,1,0])
        - q*(x[0,0,0] - x[1,0,0])*(x[0,1,0] - x[1,1,0])
        + p*q*(p-q)*(x[0,0,0] + x[1,0,0] + x[0,1,0] + x[1,1,0])
        - p*q*(p-q)*(p^2 - p*q + q^2)};
 
nameINPUT = "Q2 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{(k-p)*(k*p-x[1,0,0])+k*x[0,0,0], 
                  -p*(k*(k-p)*(k^2 - k*p + p^2 - x[0,0,0] - x[1,0,0]) + x[0,0,0]*x[1,0,0])},
                  {p, -((k-p)*(k*p-x[0,0,0])+k*x[1,0,0])}};

laxPairMatrixM = {{(k-q)*(k*q-x[0,1,0])+k*x[0,0,0], 
                  -q*(k*(k-q)*(k^2 - k*q + q^2 - x[0,0,0] - x[0,1,0]) + x[0,0,0]*x[0,1,0])},
                  {q, -((k-q)*(k*q-x[0,0,0])+k*x[0,1,0])}};

explicitScalars = True;

tFunc = 1/Sqrt[k*(k-p)*((x[0,0,0]-x[1,0,0])^2 - 2*p^2*(x[0,0,0]+x[1,0,0])+p^4)];

sFunc = 1/Sqrt[k*(k-q)*((x[0,0,0]-x[0,1,0])^2 - 2*q^2*(x[0,0,0]+x[0,1,0])+q^4)];


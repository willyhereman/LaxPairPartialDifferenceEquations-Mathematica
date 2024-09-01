(* ::Package:: *)

(**********************************************************************)
(* data file lp_A1.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman  *)
(*   *)

(***  A1 equation (ABS classification 2003)   ***)

ddEQ = {p*(x[0,0,0] + x[0,1,0])*(x[1,0,0] + x[1,1,0]) - q*(x[0,0,0] + x[1,0,0])* (x[0,1,0] + x[1,1,0])
        - delta^2*p*q*(p - q)};
 
nameINPUT = "A1 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{(k-p)*x[1,0,0] + k*x[0,0,0], -p*(delta^2*k*(k-p) + x[0,0,0]*x[1,0,0])},
                  {p, -((k-p)*x[0,0,0] + k*x[1,0,0])}};

laxPairMatrixM = {{(k-q)*x[0,1,0] + k*x[0,0,0], -q*(delta^2*k*(k-q) + x[0,0,0]*x[0,1,0])},
                  {q, -((k-q)*x[0,0,0] + k*x[0,1,0])}};

explicitScalars = True;

tFunc = 1/Sqrt[k*(k-p)*((delta*p+x[0,0,0]+x[1,0,0])*(delta*p-x[0,0,0]-x[1,0,0]))];

sFunc = 1/Sqrt[k*(k-q)*((delta*q+x[0,0,0]+x[0,1,0])*(delta*q-x[0,0,0]-x[0,1,0]))];




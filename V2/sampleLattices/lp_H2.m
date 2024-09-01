(* ::Package:: *)

(**********************************************************************)
(* data file lp_H2.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  H2 equation (ABS classification 2003)   ***)

ddEQ = {(x[0,0,0] - x[1,1,0])*(x[1,0,0] - x[0,1,0]) 
        + (q - p)*(x[0,0,0] + x[1,0,0] + x[0,1,0] + x[1,1,0]) + q^2 -p^2};

nameINPUT = "H2 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{p - k + x[0,0,0], p^2 - k^2 - x[0,0,0]*x[1,0,0] + (p -k)*(x[0,0,0]+x[1,0,0])},
                  {1, k-p-x[1,0,0]}};

laxPairMatrixM = {{q - k + x[0,0,0], q^2 - k^2 - x[0,0,0]*x[0,1,0] + (q -k)*(x[0,0,0]+x[0,1,0])},
                  {1, k-q-x[0,1,0]}};

explicitScalars = True;

sFunc = 1/Sqrt[2*(k-q)*(q+x[0,0,0]+x[0,1,0])];

tFunc = 1/Sqrt[2*(k-p)*(p+x[0,0,0]+x[1,0,0])];


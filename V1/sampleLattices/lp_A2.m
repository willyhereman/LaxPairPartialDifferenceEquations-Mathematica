(* ::Package:: *)

(**********************************************************************)
(* data file lp_A2.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***  A2 equation (ABS classification 2003)   ***)


ddEQ = {(q^2 - p^2)*(x[0,0,0]*x[1,0,0]*x[0,1,0]*x[1,1,0]+1) + q*(p^2-1)*(x[0,0,0]*x[0,1,0] + x[1,0,0]*x[1,1,0])
           - p*(q^2-1)*(x[0,0,0]*x[1,0,0] + x[0,1,0]*x[1,1,0])};
 
nameINPUT = "A2 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;


laxPairMatrixL = {{k*(p^2-1)*x[0,0,0], -(p^2 - k^2 + p*(k^2-1)*x[0,0,0]*x[1,0,0])},
                  {p*(k^2-1)+(p^2-k^2)*x[0,0,0]*x[1,0,0], -k*(p^2-1)*x[1,0,0]}};
laxPairMatrixM = {{k*(q^2-1)*x[0,0,0], -(q^2 - k^2 + q*(k^2-1)*x[0,0,0]*x[0,1,0])},
                  {q*(k^2-1)+(q^2-k^2)*x[0,0,0]*x[0,1,0], -k*(q^2-1)*x[0,1,0]}};
tFunc = 1/Sqrt[(k^2-1)*(k^2-p^2)*(p - x[0,0,0]+x[1,0,0])*(p*x[0,0,0]*x[1,0,0]-1)];
sFunc = 1/Sqrt[(k^2-1)*(k^2-q^2)*(q - x[0,0,0]+x[0,1,0])*(q*x[0,0,0]*x[0,1,0]-1)];
explicitScalars = True;

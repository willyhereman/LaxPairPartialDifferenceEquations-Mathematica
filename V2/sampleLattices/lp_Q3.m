(* ::Package:: *)

(**********************************************************************)
(* data file lp_Q3.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  Q3 equation (ABS classification 2003)   ***)

ddEQ = {(q^2 - p^2)(x[0,0,0]*x[1,1,0] + x[1,0,0]*x[0,1,0])
         + q*(p^2 - 1)*(x[0,0,0]*x[1,0,0] + x[0,1,0]*x[1,1,0])
         - p*(q^2 - 1)*(x[0,0,0]*x[0,1,0] + x[1,0,0]*x[1,1,0])
         - delta^2/(4*p*q)*(p^2 - q^2)*(p^2 - 1)*(q^2 - 1)};
 
nameINPUT = "Q3 equation (ABS classification 2003)";

ddeEquationListINPUT = ddEQ;

(*
laxPairMatrixL = {{-4*k*p*(p*(k^2 - 1)*x[0,0,0]+(p^2-k^2)*x[1,0,0]), 
                     -(p^2-1)*(delta^2*k^2-delta^2*k^4-delta^2*p^2+delta^2*k^2*p^2 - 4*k^2*p*x[0,0,0]*x[1,0,0])},
                  {-4*k^2*p*(p^2-1), 4*k*p*(p*(k^2-1)*x[1,0,0]+(p^2-k^2)*x[0,0,0])}};

laxPairMatrixM = {{-4*k*q*(q*(k^2 - 1)*x[0,0,0]+(q^2-k^2)*x[0,1,0]), 
                      -(q^2-1)*(delta^2*k^2-delta^2*k^4-delta^2*q^2+delta^2*k^2*q^2 - 4*k^2*q*x[0,0,0]*x[0,1,0])},
                  {-4*k^2*q*(q^2-1), 4*k*q*(q*(k^2-1)*x[0,1,0]+(q^2-k^2)*x[0,0,0])}};
*)

(* explicitScalars = True; *)
explicitScalars = False;

(* 
tFunc = 1/(2*k*Sqrt[p*(k^2-1)*(k^2-p^2)*(4*p^2*(x[0,0,0]^2  + x[1,0,0]^2)-4*p*(1+p^2)*x[0,0,0]*x[1,0,0]+delta^2(1-p^2)^2)]);

sFunc = 1/(2*k*Sqrt[q*(k^2-1)*(k^2-q^2)*(4*q^2*(x[0,0,0]^2  + x[0,1,0]^2)-4*q*(1+q^2)*x[0,0,0]*x[0,1,0]+delta^2(1-q^2)^2)]);
*)

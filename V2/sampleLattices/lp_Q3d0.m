(* ::Package:: *)

(**********************************************************************)
(* data file lp_Q3d0.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***  Q3 eq. w/ delta = d = 0 (ABS classification 2003)   ***)

ddEQ = {(q^2 - p^2)(x[0,0,0]*x[1,1,0] + x[1,0,0]*x[0,1,0])
         + q*(p^2 - 1)*(x[0,0,0]*x[1,0,0] + x[0,1,0]*x[1,1,0])
         - p*(q^2 - 1)*(x[0,0,0]*x[0,1,0] + x[1,0,0]*x[1,1,0])};
 
nameINPUT = "Q3 eq. w/ delta = 0 (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{(p^2 - k^2)*x[1,0,0] + p*(k^2 - 1)*x[0,0,0], -k*(p^2 - 1)*x[0,0,0]*x[1,0,0]},
                  {(p^2 - 1)*k, -((p^2 - k^2)*x[0,0,0] + p*(k^2 - 1)*x[1,0,0])}};

laxPairMatrixM = {{(q^2 - k^2)*x[0,1,0] + q*(k^2 - 1)*x[0,0,0], -k*(q^2 - 1)*x[0,0,0]*x[0,1,0]},
                  {(q^2 - 1)*k, -((q^2 - k^2)*x[0,0,0] + q*(k^2 - 1)*x[0,1,0])}};

explicitScalars = True;

(*
tFunc = 1/((k^2-1)*(p^2-k^2)(p*x[0,0,0]-x[1,0,0])*(p*x[1,0,0]-x[0,0,0]));
sFunc = 1/((k^2-1)*(q^2-k^2)(q*x[0,0,0]-x[0,1,0])*(q*x[0,1,0]-x[0,0,0]));
*)

(* These are given: *)
tFunc = 1/(p*x[0,0,0] - x[1,0,0]);
sFunc = 1/(q*x[0,0,0] - x[0,1,0]); 

(* 
tFunc = 1/(p*x[1,0,0] - x[0,0,0]);
sFunc = 1/(q*x[0,1,0] - x[0,0,0]);
*)


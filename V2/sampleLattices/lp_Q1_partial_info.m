(* ::Package:: *)

(**********************************************************************)
(* data file lp_Q1_no_info.m *)
(*   *)
(* Last Updated: On June 27, 2017 by Hereman *)
(*   *)
(***  Q1 equation (ABS classification 2003)  with partial info given  ***)
(* with partial info (tFunc and sFunc) given for testing purposes *)

ddEQ = 
{p*(x[0,0,0] - x[0,1,0])*(x[1,0,0] - x[1,1,0])
 -q*(x[0,0,0] -x[1,0,0])*(x[0,1,0] - x[1,1,0])
 + d^2*p*q*(p-q)};
 
nameINPUT = "Q1 equation (ABS classification 2003)";
ddeEquationListINPUT = ddEQ;

(* 
laxPairMatrixL = 
{{k*(x[0,0,0]-x[1,0,0]) + p*x[1,0,0], -p*(d^2*k*(p-k) + x[0,0,0]*x[1,0,0])},
{p, k*(x[0,0,0]-x[1,0,0])-p*x[0,0,0]}};
 
*)

(* 
laxPairMatrixM = 
{{k*(x[0,0,0]-x[0,1,0]) + q*x[0,1,0], -q*(d^2*k*(q-k) + x[0,0,0]*x[0,1,0])},
{q, k*(x[0,0,0]-x[0,1,0])-q*x[0,0,0]}};
*)

explicitScalars = True;
(* *) 

tFunc = 1/(d*p + (x[0,0,0] - x[1,0,0]));
sFunc = 1/(d*q + (x[0,0,0] - x[0,1,0])); 

(* *)


(*  
Also, 
tFunc = 1/(d*p - (x[0,0,0] - x[1,0,0]));

    sFunc = 1/(d*q - (x[0,0,0] - x[0,1,0])); 

*)


(* weightedParametersINPUT = {}; *)


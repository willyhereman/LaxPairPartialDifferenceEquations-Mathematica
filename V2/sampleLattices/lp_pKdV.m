(* ::Package:: *)

(**********************************************************************)
(* data file lp_Lecture4.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman  *)
(*   *)
(***  Equation given in Nijhoff, Lecture 4    ***)

ddEQ = {(x[1,1,0] - x[0,0,0])*(x[1,0,0] - x[0,1,0]) + q^2 - p^2};

nameINPUT = "pKdV Variance";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{k+p,0},{-1,p-k}};
laxPairMatrixM = {{k+q,0},{-1,q-k}};

explicitScalars = True;
tFunc = 1;
sFunc = 1; 


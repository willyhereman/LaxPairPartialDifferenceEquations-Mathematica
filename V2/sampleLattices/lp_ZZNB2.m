(* ::Package:: *)

(**********************************************************************)
(* data file lp_nzzA2.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***   Nijhoff/Zhang/Zhou B-2 system, also treated in our paper ***)

ddEQ = {x[0,0,0]*x[1,0,0] - z[1,0,0] - y[0,0,0], 
        x[0,0,0]*x[0,1,0] - z[0,1,0] - y[0,0,0], 
        y[1,1,0] + a1 + z[0,0,0] + a2*(x[1,1,0] - x[0,0,0]) - 
        x[0,0,0]*x[1,1,0] + G[-p,-q]/(x[0,1,0] - x[1,0,0])};

nameINPUT = "Zhang, Zhou, Nijhoff B-2";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {};
laxPairMatrixM = {};

explicitScalars = False;
(* no info for tFunc and sFunc is given *)

userShifts = True;
paramEquivalences = {G[-p,-q] -> G[-k,-q] - G[-k,-p], 
                     G[-p,-k]-> -G[-k,-p], 
                     G[-q,-k]-> -G[-k,-q]};

oldPE = {G[-k,-q] -> q^3 - k^3 + beta2*(k^2 - q^2) + beta1*(q - k), 
G[-p,-q] ->  q^3 - p^3 + beta2*(p^2 - q^2) + beta1*(q - p),
G[-p,-k] ->  k^3 - p^3 + beta2*(p^2 - k^2) + beta1*(k - p)};


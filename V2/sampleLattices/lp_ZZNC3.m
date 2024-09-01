(* ::Package:: *)

(**********************************************************************)
(* data file lp_NZZC3.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***   Nijhoff/Zhang/Zhou C-3 system, also treated in our paper ***)

ddEQ = {z[0,0,0]*y[1,0,0] + x[1,0,0] - x[0,0,0], 
        z[0,0,0]*y[0,1,0] + x[0,1,0] - x[0,0,0], 
        G[-a,-b]*x[1,1,0] - y[0,0,0]*z[1,1,0] +  
        z[0,0,0]*((G[-q,-b]*y[0,1,0]*z[1,0,0] - 
        G[-p,-b]*y[1,0,0]*z[0,1,0])/(z[1,0,0] - z[0,1,0]))};

nameINPUT = "Zhang, Zhou, Nijhoff C-3";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {};
laxPairMatrixM = {};

explicitScalars = False;
(* no info for tFunc and sFunc is given *)

userShifts = True;
paramEquivalences = {G[-p, -q] -> G[-k,-q] - G[-k,-p], 
                     G[-p,-k]  -> -G[-k,-p]};


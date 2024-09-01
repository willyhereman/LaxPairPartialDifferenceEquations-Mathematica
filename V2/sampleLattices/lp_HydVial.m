(* ::Package:: *)

(**********************************************************************)
(* data file lp_HydVial.m                                             *)
(*                                                                    *)
(* Last Updated:   on June 26, 2017 by Hereman                        *)
(*                                                                    *)
(***                     Hydon-Viallet Equation                     ***)


ddEQ = {x[1,1,0] + (x[1,0,0] + 1)*(x[0,1,0] - 1)/((x[0,0,0]*(x1,0,0] - 1)(x[0,1,0] + 1))};
 
nameINPUT = "Hydon-Viallet equation";
ddeEquationListINPUT = ddEQ;

explicitScalars = False; 
(* no info for tFunc and sFunc is given *)

(* 
prt2intRules = {u00 -> x[0,0,0],
               u10 -> x[1,0,0], 
               u01 -> x[0,1,0], 
               u11 -> x[1,1,0]};
int2prtRules = {x[0,0,0] -> u00,
                x[1,0,0] -> u10,
                x[0,1,0] -> u01,
                x[0,0,1] -> u001, 
                x[1,1,0] -> u11, 
                x[1,0,1] -> u101,
                x[0,1,1] -> u011, 
                x[1,1,1] -> u111};

Clear[laxPairMatrixL, laxPairMatrixM, sFunc, tFunc];

*)


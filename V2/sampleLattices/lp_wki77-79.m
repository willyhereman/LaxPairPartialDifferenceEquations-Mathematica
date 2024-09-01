(* ::Package:: *)

(**********************************************************************)
(* data file lp_wki77079.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman  *)
(*   *)
(***     ***)

ddEQ = {(x[1,1,0] - x[1,0,0] - x[0,1,0] + x[0,0,0])*(x[1,0,0]-x[0,1,0]) + (y[1,1,0] - y[1,0,0] - y[0,1,0] + y[0,0,0])*(y[1,0,0]-y[0,1,0]),
         (x[1,0,0] - x[0,1,0])*(y[1,1,0]-y[0,0,0]) - (x[1,1,0]-x[0,0,0])*(y[1,0,0]-y[0,1,0])};
 
nameINPUT = "WKI Eqs 2.77 and 2.79";
ddeEquationListINPUT = ddEQ;

explicitScalars = False; 
(* no info for tFunc and sFunc is given *)


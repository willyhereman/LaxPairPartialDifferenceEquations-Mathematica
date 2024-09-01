(* ::Package:: *)

(**********************************************************************)
(* data file lp_mKdV.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)
(***   discrete modified KORTEWEG-DE VRIES EQUATION   ***)

ddEQ = {p*(x[0,0,0]*x[0,1,0] - x[1,0,0]*x[1,1,0])-q*(x[0,0,0]*x[1,0,0]-x[0,1,0]*x[1,1,0])};

nameINPUT = "Discrete Modified Korteweg-de Vries (mKdV) Equation  (Hay Thesis)";
ddeEquationListINPUT = ddEQ;

(* 
prt2intRules = {x -> x[0,0,0], y -> y[0,0,0], z -> z[0,0,0],
               x1 -> x[1,0,0], y1 -> y[1,0,0], z1 -> z[1,0,0],
               x2 -> x[0,1,0], y2 -> y[0,1,0], z2 -> z[0,1,0],
               x3 -> x[0,0,1], y3 -> y[0,0,1], z3 -> z[0,0,1],
               x12 -> x[1,1,0], y12 -> y[1,1,0], z12 -> z[1,1,0],
               x13 -> x[1,0,1], y13 -> y[1,0,1], z13 -> z[1,0,1],
               x23 -> x[0,1,1], y23 -> y[0,1,1], z23 -> z[0,1,1]};
int2prtRules = {x[0,0,0] -> x, y[0,0,0] -> y, z[0,0,0] -> z,
                x[1,0,0] -> x1, y[1,0,0] -> y1, z[1,0,0] -> z1,
                x[0,1,0] -> x2, y[0,1,0] -> y2, z[0,1,0] -> z2,
                x[0,0,1] -> x3, y[0,0,1] -> y3, z[0,0,1] -> z3,
                x[1,1,0] -> x12, y[1,1,0] -> y12, z[1,1,0] -> z12,
                x[1,0,1] -> x13, y[1,0,1] -> y13, z[1,0,1] -> z13,
                x[0,1,1] -> x23, y[0,1,1] -> y23, z[0,1,1] -> z23,
                x[1,1,1] -> x123, y[1,1,1] -> y123, z[1,1,1] -> z123};
*)

laxPairMatrixL = {{(p/k)*x[1,0,0]/x[0,0,0], 1/x[0,0,0]},
                  {x[1,0,0], p/k}};

laxPairMatrixM = {{(q/k)*x[0,1,0]/x[0,0,0], 1/x[0,0,0]},
                  {x[0,1,0], q/k}};

explicitScalars = True;

sFunc = 1;
tFunc = 1;


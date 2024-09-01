(* ::Package:: *)

(**********************************************************************)
(* data file lp_pKdVsys.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***    System of pKdV equations ***)
(***    Zenitidis and Mikhailov 2009   ***)


ddEQ = {(x[0,0,0] - x[1,1,0])(y[1,0,0] - y[0,1,0]) - p^2 + q^2,
        (y[0,0,0] - y[1,1,0])(x[1,0,0] - x[0,1,0]) - p^2 + q^2};

nameINPUT = "System of pKdV equations";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{0, 0, x[0,0,0], -k^2+p^2 - x[0,0,0]*y[1,0,0]},
                  {0, 0, 1, -y[1,0,0]},
                  {y[0,0,0], -k^2 + p^2 - x[1,0,0]*y[0,0,0], 0, 0},
                  {1, -x[1,0,0], 0, 0}};
laxPairMatrixM = {{0, 0, x[0,0,0], -k^2+q^2 - x[0,0,0]*y[0,1,0]},
                  {0, 0, 1, -y[0,1,0]},
                  {y[0,0,0], -k^2 + q^2 - x[0,1,0]*y[0,0,0], 0, 0},
                  {1, -x[0,1,0], 0, 0}};
sFunc = 1;
tFunc = 1;

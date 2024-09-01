(* ::Package:: *)

(**********************************************************************)
(* data file lp_AtkLobNij.m *)
(*   *)
(* Last Updated: on June 26, 2017 by Hereman *)
(*   *)

(***    Atkinson-Lobb-Nijhoff System ***)
(***    Habibullin and Poptsova 2014   ***)

ddEQ = {(x[1,1,0]*y[0,0,0])*(p*x[1,0,0] - q*x[0,1,0]) - x[0,0,0]*(p*x[0,1,0]*y[1,0,0] - q*x[1,0,0]*y[0,1,0]),
         y[1,1,0]*(p*x[0,1,0]*y[1,0,0] - q*x[1,0,0]*y[0,1,0]) - (x[1,1,0]*y[0,0,0])*(p*y[0,1,0] - q*y[1,0,0])};

nameINPUT = "Atkinson-Lobb-Nijhoff System";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{p*x[1,0,0]/x[0,0,0], -k/x[0,0,0], 0},
                  {0, p*y[1,0,0]/y[0,0,0], -k*x[1,0,0]/y[0,0,0]},
                  {-k*y[1,0,0], 0, p}};

laxPairMatrixM = {{q*x[0,1,0]/x[0,0,0], -k/x[0,0,0], 0},
                  {0, q*y[0,1,0]/y[0,0,0], -k*x[0,1,0]/y[0,0,0]},
                  {-k*y[0,1,0], 0, q}};

explicitScalars = True;
sFunc = 1;
tFunc = 1;


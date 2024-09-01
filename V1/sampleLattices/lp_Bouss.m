(* ::Package:: *)

(**********************************************************************)
(* data file lp_Bouss.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***   discrete BOUSSINESQ equation ***)
(***   Tongas and Nijhoff 2005  ***)


ddEQ = {z[1,0,0] - x[0,0,0]*x[1,0,0] + y[0,0,0], 
        z[0,1,0] - x[0,0,0]*x[0,1,0] + y[0,0,0], 
        (x[0,1,0] - x[1,0,0])*(z[0,0,0] - x[0,0,0]*x[1,1,0] + y[1,1,0]) - p + q};

nameINPUT = "Discrete Boussinesq Equation";
ddeEquationListINPUT = ddEQ;

laxPairMatrixL = {{-x[1,0,0], 1, 0},
                  {-y[1,0,0], 0, 1},
                  {-k + p - x[0,0,0]*y[1,0,0] + x[1,0,0]*z[0,0,0], -z[0,0,0], x[0,0,0]}};
laxPairMatrixM = {{-x[0,1,0], 1, 0},
                  {-y[0,1,0], 0, 1},
                  {-k + q - x[0,0,0]*y[0,1,0] + x[0,1,0]*z[0,0,0], -z[0,0,0], x[0,0,0]}};
explicitScalars = False;




(* ::Package:: *)

(**********************************************************************)
(* data file lp_Hiea2.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***   discrete Hietarinta A2 equation ***)
(***   Hietarinta J. Phys. A: Math. Theor. 44, art. no. 165204, 2011 ***)

ddEQ = {x[1,0,0]*z[0,0,0]-y[1,0,0]-x[0,0,0], 
        x[0,1,0]*z[0,0,0]-y[0,1,0]-x[0,0,0],
        z[1,1,0] - b0 - y[0,0,0]/x[0,0,0] 
        - (1/x[0,0,0])*(p*x[1,0,0] - q*x[0,1,0])/(z[1,0,0]-z[0,1,0])};

nameINPUT = "Discrete Hietarinta A2 Equation";
ddeEquationListINPUT = ddEQ;

(*
laxPairMatrixL = {{-x[1,0,0], 1, 0},
                  {-y[1,0,0], 0, 1},
                  {-k + p - x[0,0,0]*y[1,0,0] + x[1,0,0]*z[0,0,0], 
                   -z[0,0,0], x[0,0,0]}};
laxPairMatrixM = {{-x[0,1,0], 1, 0},
                  {-y[0,1,0], 0, 1},
                  {-k + q - x[0,0,0]*y[0,1,0] + x[0,1,0]*z[0,0,0], 
                  -z[0,0,0], x[0,0,0]}};
explicitScalars = False;
*)

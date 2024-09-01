(* ::Package:: *)

(**********************************************************************)
(* data file lp_lsBSQ.m *)
(*   *)
(* Last Updated:  *)
(*   *)
(***   lattice Schwarzian BOUSSINESQ equation ***)
(***   Tongas and Nijhoff 2005  ***)


ddEQ = {x[0,1,0] - x[0,0,0] - y[0,0,0]*z[0,1,0], 
        x[1,0,0] - x[0,0,0] - y[0,0,0]*z[1,0,0],
        y[1,1,0]*z[0,0,0]*(y[1,0,0] - y[0,1,0]) - y[0,0,0]*(z[1,0,0]*y[0,1,0]*p - z[0,1,0]*y[1,0,0]*q)};

nameINPUT = "lattice Schwarzian Boussinesq System";
ddeEquationListINPUT = ddEQ;



laxPairMatrixL = {{y[1,0,0], -x[1,0,0], 0},
                  {-k*y[1,0,0]/z[0,0,0], p*y[0,0,0]*z[1,0,0]/z[0,0,0], k*x[0,0,0]*y[1,0,0]/z[0,0,0]},
                  {0, -1, y[1,0,0]}};
laxPairMatrixM = {{y[0,1,0], -x[0,1,0], 0},
                  {-k*y[0,1,0]/z[0,0,0], q*y[0,0,0]*z[0,1,0]/z[0,0,0], k*x[0,0,0]*y[0,1,0]/z[0,0,0]},
                  {0, -1, y[0,1,0]}};
(*
laxPairMatrixL = {{y[1,0,0], -x[1,0,0], 0},
                  {-k*y[1,0,0]/z[0,0,0], p*y[0,0,0]*z[1,0,0]/z[0,0,0], k*x*y[1,0,0]/z[0,0,0]},
                  {0, -1, y[1,0,0]}};
laxPairMatrixM = {{y[0,1,0], -x[0,1,0], 0},
                  {-k*y[0,1,0]/z[0,0,0], p*y[0,0,0]*z[0,1,0]/z[0,0,0], k*x*y[0,1,0]/z[0,0,0]},
                  {0, -1, y[0,1,0]}};
*)
tFunc = 1/y[0,0,0];
sFunc = 1/y[0,0,0];

explicitScalars = True;


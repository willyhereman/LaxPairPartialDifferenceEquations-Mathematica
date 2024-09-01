(* ::Package:: *)

(* ## ## ## ## ##         Function: genLatticeEquations         ## ## ## ## ## *)

(*******************************************************************************)
(* genLatticeEquations[ddeEquationList]                                        *)
(* Purpose:  From the given equations, generate the corresponding equations    *)
(*           on the cube.  I.e.  Assuming the equations specified correspond   *)
(*           to the front face of a cube, rotate and translate these equations *)
(*           to generate the equations on the remaining 5 faces.               *)
(* Input:   User Specified List of discrete differential functions             *)
(* Output:  List of discrete diff. functions adjusted for faces of             *)
(*          cube lattice                                                       *)
(* Code is in File:  genLatEq.m                                                *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

genLatticeEquations[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printGenLat},

  If[dbLPGenLatt, printGenLat = Print, Clear[printGenLat], Clear[printGenLat]];

  printGenLat["D: genLat, Function: genLatticeEquations, File: genLatEq.m"];
  printGenLat["D: #######################################################"];
  printGenLat["D: genLat, DDE: "];
  printGenLat[convert2Print[ddEQ]];

  (* generate corresponding equations for each face of the cube     *)
  (* First the rotational faces *)
  leftFaceEQG = ddEQ /. latFrnt2LeftRules;
  groundFaceEQG = ddEQ /. latFrnt2GrndRules;

  (* Next the shifted faces *)
  rightFaceEQG = leftFaceEQG /. latLeft2RghtRules;
  topFaceEQG = groundFaceEQG /. latGrnd2TopRules;
  backFaceEQG = frontFaceEQG /. latFrnt2BackRules; 

  printGenLat["D: genLat, front: \n", convert2Print[frontFaceEQG]];
  printGenLat["D: genLat, back: \n", convert2Print[backFaceEQG]];
  printGenLat["D: genLat, left: \n", convert2Print[leftFaceEQG]];
  printGenLat["D: genLat, right: \n", convert2Print[rightFaceEQG]];
  printGenLat["D: genLat, ground: \n", convert2Print[groundFaceEQG]];
  printGenLat["D: genLat, top: \n", convert2Print[topFaceEQG]];

  latticeEQG = Union[frontFaceEQG, leftFaceEQG, groundFaceEQG,
                         backFaceEQG, rightFaceEQG, topFaceEQG];
  printGenLat["D: genLat, DDE: \n", convert2Print[latticeEQG]];
  printGenLat["D: genLat, # of equations: ", Length[latticeEQG]];

  (* Clear all local variables not being returned. *)
  Clear[ddEQ];

  printGenLat["D: genLat, exit"];
  Clear[printGenLat];
  Return[];
]; (* end Module genLatticeEquations *)


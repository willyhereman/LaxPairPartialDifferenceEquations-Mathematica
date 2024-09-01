(* ::Package:: *)

(* ## ## ## ## ##         Function: augmentLatticeEqu         ## ## ## ## ## *)

(*******************************************************************************)
(* augmentLatticeEqu[ddeEquationList]                                          *)
(* Purpose:  Inspects the user provided equations specifically for "edge"      *)
(*           equations.  Given the nature of the transformations of the lattice*)
(*           equations, these would be subject to 2 (both a rotation and a     *)
(*           shift).  Augment the original equation (s) so that this special    *)
(*           case is handled.                                                  *)
(* Input:   User Specified List of discrete differential functions             *)
(* Output:  Global list of equations = original set + additional edge          *)
(*          equations if necessary.                                            *)
(* Code is in File:  augLatEq.m                                                *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

augmentLatticEqu[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printAugLat, eqLen, vpos, tmpEQ, newEq},

  If[dbLPAugLatt, printAugLat = Print, Clear[printAugLat], Clear[printAugLat]];

  printAugLat["D: augLat, Function: augmentLatticeEqu, File: augLatEq.m"];
  printAugLat["D: #######################################################"];
  printAugLat["D: augLat, DDE: "];
  printAugLat[convert2Print[ddEQ]];

  eqLen = Length[ddEQ];
  printAugLat["D: augLat, # of equations:"];
  printAugLat[eqLen];

  frontFaceEQG = ddEQ;
  If[Length[singleEdgeEQG] > 0, 
    mainPrint["**** System of Equations contain single edge equation - augment ***"];

    Map[
      tmpEQ = #;
      MapThread[
        vpos = Flatten[Position[Variables[tmpEQ], #1]];
        If[Length[vpos]>0,vpos = First[vpos],vpos = 0];
        If[vpos > 0, 
         (* Shift the x/x2 equation across the front face in the 1 direction  *)
          newEq = tmpEQ /. #2;
          printAugLat["D: augLat, shifted: "];
          printAugLat[convert2Print[newEq]];
          frontFaceEQG = Union[frontFaceEQG, {newEq}];
        ];
      &, {{_[1,0,0],_[0,1,0]}, {lat2ShiftRules, lat1ShiftRules}}];
    &, singleEdgeEQG];

    mainPrint["************************ Augmented System **************************"];
    mainPrint[convert2Print[ Map[# == 0 &, frontFaceEQG] ]];
  ];

  (* Clear all local variables not being returned. *)
  Clear[eqLen, newEq, vpos, tmpEQ];

  printAugLat["D: augLat, exit"];
  Clear[printAugLat];
  Return[];
]; (* end Module augmentLatticeEqu *)




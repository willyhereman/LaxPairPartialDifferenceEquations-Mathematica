(* ::Package:: *)

(* ## ## ## ## ##            Function: variableInfo             ## ## ## ## ## *)

(*******************************************************************************)
(* variableInfo[prtFunc, lpV]                                                  *)
(* Purpose:                                                                    *)
(* Input: variable construct used to store information for the L.P compuation  *)
(*                                                                             *)
(* Output:                                                                     *)
(*                                                                             *)
(* Code is in File:  varinfo.m                                                 *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

variableInfo[prtFunc_, lpV_] :=
Module[{printFunc = prtFunc, lpVars = lpV,
         printMerge, solList={{},{},{}}, tmpList={}, tmpRule={}, idx=0},

  printFunc["D: Lax Pair Variable Info:"];
  printFunc["D: Variable Index: ", lpVars[[ vIdx ]] ];
  printFunc["D: Variable Name: ", lpVars[[ vName ]] ];
  printFunc["D: Variable Used: ", lpVars[[ vUsed ]] ];
  printFunc["D: Variable Linearity: ", lpVars[[ vLinear ]] ];
  printFunc["D: Variable Constraints: ", lpVars[[ vConst ]] ];
  printFunc["D: Variable sub order: ", lpVars[[ vSub ]] ];
  printFunc["D: Solutions after initial substituions: ", lpVars[[ subs ]] ];
  printFunc["D: Common Denominator: ", lpVars[[ cmnDen ]] ];
  printFunc["\n"];

  Return[];
]; (* end Module variableInfo *)


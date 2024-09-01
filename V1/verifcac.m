(* ::Package:: *)

(* ## ## ## ## ##             Function: verifyCAC               ## ## ## ## ## *)

(*******************************************************************************)
(* verifyCAC[ddeEquationList]                                                  *)
(* Purpose:  Using the specified list of equations, systematically solves for  *)
(*           the 3-sub variables, if possible, via different routes and        *)
(*           verifies that all solutions are consistent.                       *)
(* Input:    Expanded list of equations derived from the original DDE and      *)
(*           adjusted to represent equations on all faces of the cube.         *)
(* Output:   List of rules describing the solutions for all variables,         *)
(*           2-sub and 3-sub primarily, 0-sub and 1-sub if necessary.          *)
(* Code is in File:  verifcac.m                                                *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

verifyCAC[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printVarSolve, eqUnk, eqVars, 
   faceEqs, sub3List, frontFaceSols},

  If[dbLPVerifyCAC, printCAC = Print, Clear[printCAC], Clear[printCAC]];

  printCAC["D: CAC, Function: verifyCAC, File: verifcac.m"];
  printCAC["D: ###########################################"];

  vars2Solve = sysVarsG;

  (* Beginning w/ the original system (aka front face), solve for the variables  *)
  (* referenced, eliminating the corresponding equation used as we go. Progress  *)
  (* through the faces solving for variables, reducing previously found          *)
  (* solutions, until all necessary variables (for either CAC, tetrahedron prop  *)
  (* or finding Lax Pair)                                                        *) 
  varsFound = {};

  (* Solve the initial rotational faces (front, left, bottom) for the 2-sub      *)
  (* variables and 0-sub variables                                               *)
  (*** Front Face   ***)
  faceEqs = frontFaceEQG;
  frontFaceSols = solveFrontFace[frontFaceEQG];
  mainPrint["********************** Front Face Solutions ************************"];
  mainPrint[convert2Print[frontFaceSols]];
  mainPrint["*******************************************************************\n"];
  printCAC["D: CAC, Variables Found:"];
  printCAC[convert2Print[varsFound]];
  solutionListG = frontFaceSols;

  (* Using the variables found on the Front Face, calculate the variables we     *)
  (* will find on the left face but remove any redundancy                        *)
  vars2Find = Complement[(varsFound /. latFrnt2LeftRules), varsFound];
  printCAC["D: CAC, Variables to solve for in Left Face:\n", convert2Print[vars2Find]];

  (*** Left Face   ***)
  faceSol = MapIndexed[Complement[(#1 /. latFrnt2LeftRules), solutionListG[[First[#2]]]]&, frontFaceSols];
  (* faceSol = solveFaceEqu[leftFaceEQG, vars2Find];  *)
  mainPrint["*********************** Left Face Solutions ************************"];
  mainPrint[convert2Print[faceSol]];
  mainPrint["*******************************************************************\n"];
  solutionListG = mergeSolLists[solutionListG, faceSol];
  printCAC["D: CAC, Solutions Found:"];
  printCAC[convert2Print[solutionListG]];

  (* Using the variables found on the Front Face, calculate the variables we     *)
  (* will find on the bottom face but remove any redundancy                        *)
  vars2Find = Complement[(varsFound /. latFrnt2GrndRules), varsFound];
  printCAC["D: CAC, Variables to solve for in Bottom Face:\n", convert2Print[vars2Find]];

  (*** Bottom Face   ***)
  faceSol = MapIndexed[Complement[(#1 /. latFrnt2GrndRules), solutionListG[[First[#2]]]]&, frontFaceSols];
  (*  faceSol = solveFaceEqu[groundFaceEQG, vars2Find];  *)
  mainPrint["********************** Bottom Face Solutions ************************"];
  mainPrint[convert2Print[faceSol]];
  mainPrint["*******************************************************************\n"];
  solutionListG = mergeSolLists[solutionListG, faceSol];
  printCAC["D: CAC, Front/Left/Bottom Face solutions:"];
  printCAC[convert2Print[solutionListG]];

  (* Using the translational faces, solve the 3-sub and simplify using the solutions *)
  (* found above, primarily the 2-sub and 0-sub (1-sub if necessary)                 *)
  vars123FoundG = {};
  faceEqs = rightFaceEQG;
  printCAC["D: CAC:  Computing 3-sub solutions using Right Face:"];
  faceSolR = verifyVar123[solutionListG, faceEqs];
  mainPrint["********************* Right Face 123 Solutions **********************"];
  mainPrint[convert2Print[faceSolR]];
  mainPrint["*******************************************************************\n"];
  faceEqs = backFaceEQG;
  faceSolB = verifyVar123[solutionListG, faceEqs];
  mainPrint["********************* Back Face 123 Solutions ***********************"];
  mainPrint[convert2Print[faceSolB]];
  mainPrint["*******************************************************************\n"];
  faceEqs = topFaceEQG;
  faceSolT = verifyVar123[solutionListG, faceEqs];
  mainPrint["********************** Top Face 123 Solutions ***********************"];
  mainPrint[convert2Print[faceSolT]];
  mainPrint["*******************************************************************\n"];
  faceSolT123 = x[1,1,1] /. faceSolT;
  printCAC["D: CAC:  Top Face 3-sub solutions:"];
  printCAC[convert2Print[faceSolT123]]; 

  printCAC["D: CAC:  Variables Solved for:"];
  printCAC[convert2Print[vars123FoundG]];


  mainPrint["********************* 123 Solution Comparisons **********************"];
  (* If the system is consistent about the cube, the solutions found for the 3-sub *)
  (* variables should be the same upon reduction using the solutions found         *)
  cacTest = True;
  Map[
    printCAC["D: CAC:  Current 3-sub Variable:"];
    printCAC[convert2Print[#]]; 
    (* Compare solutions from Right and Top faces *)
    mainPrint[convert2Print[#]];
    solution1 = Factor[# /. faceSolR];
    printCAC["D: CAC:  Right:"];
    printCAC[convert2Print[solution1]]; 
    solution2 = Factor[# /. faceSolT];
    printCAC["D: CAC:  Top:"];
    printCAC[convert2Print[solution2]]; 
    solDiff = Simplify[solution1 - solution2];
    mainPrint["********************* 123 Top/Right Difference *********************"];
    mainPrint[convert2Print[solDiff]];
    eqVars={};
    If[UnsameQ[solDiff,0], 
      (* reduce difference using solutions found *)
      solDiff = Factor[solDiff /. solutionListG[[2]] ];
      solDiff = Factor[solDiff /. solutionListG[[1]] ];
(***
      eqVars = Sort[Intersection[Variables[solDiff], allowedVars], Count[#1,1]>Count[#2,1]&];
      printCAC["variables:", convert2Print[eqVars]];
      Map[
        i = Flatten[Position[varsFound,#]];
        If[Length[i]>0,
          solDiff = Factor[solDiff /. solutionListG[[i[[1]]]]];
          printCAC["D: CAC:  reduced difference:"];
          printCAC[convert2Print[solDiff]];
        ];
      &, eqVars];
***)
    ];
    mainPrint["***************** 123 Top/Right Reduced Difference *****************"];
    mainPrint[convert2Print[solDiff], ": ", solDiff===0];
    cacTest = cacTest && solDiff===0;

    (* Compare solutions from Right and Back faces *)
    solution1 = Factor[# /. faceSolR];
    printCAC["D: CAC:  Right:"];
    printCAC[convert2Print[solution1]]; 
    solution2 = Factor[# /. faceSolB];
    printCAC["D: CAC:  Back:"];
    printCAC[convert2Print[solution2]]; 
    solDiff = Simplify[solution1 - solution2];
    mainPrint["********************* 123 Back/Right Difference ********************"];
    mainPrint[convert2Print[solDiff]];
    eqVars={};
    If[UnsameQ[solDiff,0], 
      (* reduce difference using solutions found *)
      solDiff = Factor[solDiff /. solutionListG[[2]] ];
      solDiff = Factor[solDiff /. solutionListG[[1]] ];
(***
      eqVars = Sort[Intersection[Variables[solDiff], allowedVars], Count[#1,1]>Count[#2,1]&];
      printCAC["variables:", convert2Print[eqVars]];
      Map[
        i = Flatten[Position[varsFound,#]];
        If[Length[i]>0,
          solDiff = Factor[solDiff /. solutionListG[[i[[1]]]]];
          printCAC["D: CAC:  reduced difference:"];
          printCAC[convert2Print[solDiff]];
        ];
      &, eqVars];
***)
    ];
    mainPrint["***************** 123 Back/Right Reduced Difference ****************"];
    mainPrint[convert2Print[solDiff], ": ", solDiff===0];
    cacTest = cacTest && solDiff===0;
    mainPrint["*******************************************************************\n"];
  &, vars123FoundG];

  If[cacTest, 
    mainPrint["************ The system is consistent about the cube. **************"],
    mainPrint["********** The system is NOT consistent about the cube. ************"]
  ];

  printCAC["D: CAC:  final solution set:"];
  printCAC[convert2Print[solutionListG]]; 

  printCAC["D: CAC, exit"];
]; (* end Module verifyCAC *)


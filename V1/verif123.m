(* ::Package:: *)

(* ## ## ## ## ##       Function: verifyVar123     ## ## ## ## ## *)

(**********************************************************************)
(* verifyVar123[solutionList, faceEQ]                                 *)
(* Purpose:                                                           *)
(* Input:  List of solutions found from the rotational faces which    *)
(*         should include all 2-sub var solutions.                    *)
(*         3-sub var solutions (if any) found.  If this list is null  *)
(*         simply solve the face equ's for 3-sub.                     *)
(*         If not null, solve for 3-sub and check consistency.        *)
(*         Equations corresponding to translational face on the cube  *)
(* Output:  List of solutions for the 3-sub variables if consistent   *)
(*          Null list if inconsistent                                 *)
(* Code is in File:  verif123.m                                       *)
(*                                                                    *)
(* Last Modified:                                                     *)
(**********************************************************************)

verifyVar123[solutionList_, faceEquList_] :=
Module[{faceEQ = faceEquList, solList = solutionList, sol3List={}, 
        print123, eqUnk, eqVars, vars2Solve},

  If[dbLPVerify123, print123 = Print, Clear[print123], Clear[print123]];

  print123["D: ver3, Function: verifyVar123, File: verif123.m"];
  print123["D: ver3, ########################################"];
  cFace = Sort[faceEQ, LeafCount[#1]<LeafCount[#2]&];
  print123["D: ver3, Sorted Face Equations"];
  print123[convert2Print[cFace]];

  print123["D: ver3, Solutions Found: "];
  print123[convert2Print[solList]];   

  eqUnk = Variables[faceEQ];
  eqVars = Sort[Intersection[eqUnk, allowedVars], Count[#1,1]<Count[#2,1]&];
  print123["D: ver3, using variables:"];
  print123[convert2Print[eqVars]];

  (* Since the form may describe several variables, loop until depleted   *)
  faceVar = eqVars;
  While[Length[Cases[faceVar, _[1,1,1]]]>0,
    cVar = First[Cases[faceVar, _[1,1,1]]];
    print123["D: ver3, current variable:"];
    print123[convert2Print[cVar]];

    (* we can only solve for linear variables, check the exponents        *)
    linEQ = Select[cFace, Exponent[#,cVar]==1&];
    If[Length[linEQ]>0,
      (* sort the equations based on LeafCount and then we only           *)
      (* need to grab the 1st equ referenced to have simplest.            *)
      linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
      equRef = linEQ[[First[First[Position[linEQ, cVar]]]]];
      print123["D: ver3, Current: variable, equation"];
      print123[convert2Print[cVar]];
      print123[convert2Print[equRef]];

      varSol = Flatten[Solve[equRef==0, cVar]];
      print123["D: ver3, current variable solution"];
      print123[convert2Print[varSol]];
      cFace = Complement[Factor[cFace /. varSol],{0}];
      If[Length[sol3List]>0, 
        sol3List = MapAll[Factor, sol3List /. varSol];
      ];
      sol3List = Union[sol3List, varSol];
      vars123FoundG = Union[vars123FoundG, {cVar}];
      print123["dbg: ver3, Variables Found:"];
      print123[convert2Print[vars123FoundG]];
      print123["dbg: ver3, Solutions Found:"];
      print123[convert2Print[sol3List]];

    ];(* If ** no linear references for current variable, move on.          *)
    faceVar = Complement[faceVar, {cVar}];
  ];  (* While *)

  print123["D: ver3:  Solution Set for this Face:"];
  print123[convert2Print[sol3List]]; 


  (* Once we have found solutions for any 3sub vars, simplify the solutions *)
  (* using the 2sub and 0sub solutions we have found                        *) 
  (* First determine, of the variable solutions found, which are used in    *)
  (* the 3sub solutions we currently have                                   *)
(*  vars = Union[Flatten[Map[Intersection[Variables[First[#] /. #], varsFound]&, sol3List]]]; *)

  (* Of these, remove any except the 2sub solutions                         *)
(***
  vars2Sub = Select[vars, Count[# /. Head[#] -> List, 1] > 1&];
  print123["D: ver3:  Variables to sub:"];
  print123[convert2Print[vars2Sub]]; 

  sol2Use = Map[solList[[ First[Flatten[Position[solList, #]]] ]] &, vars2Sub];
  print123[convert2Print[sol2Use]]; 
  redSol3List = Factor[sol3List /. sol2Use];
***)
  redSol3List = Factor[sol3List /. solList[[3]] ];

  print123["D: ver3:  Reduced:"];
  print123[convert2Print[redSol3List]]; 

  print123["D: ver3, exit"];
  Return[redSol3List];
]; (* end Module verifyVar123 *)


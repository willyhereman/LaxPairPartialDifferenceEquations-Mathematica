(* ::Package:: *)

(* ## ## ## ## ##         Function: lpSystemStatistics          ## ## ## ## ## *)

(*******************************************************************************)
(* lpSystemStatistics[ddeEquationList]                                         *)
(* Purpose:  Inspects the user provided equations and specifically identifies  *)
(*           1. any single edge equations                                      *)
(*           2. any double edge equations                                      *)
(*           3. variables referenced                                           *)
(*           4. total number of provided equations                             *)
(*           5. total number of equations incl. any augmented eqs              *)
(* Input:   User Specified List of discrete differential functions             *)
(* Output:  Global list of equations = original set + additional edge          *)
(*          equations if necessary.                                            *)
(* Code is in File:  lpsystat.m                                                *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

lpSystemStatistics[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printSystat, eqLen,
 dblSub, edgeEQ, sglEdge, dblEdge, tmpEQ, vpos},


  If[dbLPSysStat, printSystat = Print, Clear[printSystat], Clear[printSystat]];

  printSystat["D: syStat, Function: lpSystemStatistics, File: lpsystat.m"];
  printSystat["D: #######################################################"];
  printSystat["D: syStat, DDE: "];
  printSystat[convert2Print[ddEQ]];

  eqLen = Length[ddEQ];
  printSystat["D: syStat, # of equations:"];
  printSystat[eqLen];

  mainPrint["************************** DDE Statistics ***************************\n"];
  mainPrint["The DDE specified contains: ", eqLen, " equation(s)."];
  mainPrint[convert2Print[ Map[#==0 &, ddEQ] ]];

  sysUnkG = Variables[ddEQ];
  sysParamsG = Complement[sysUnkG, allowedVars];
  sysVarsG = Sort[Intersection[sysUnkG, allowedVars], Count[#1,1]<Count[#2,1]&];

  mainPrint["It references : ", convert2Print[sysVarsG], " variables.\n"];
  mainPrint["It references : ", convert2Print[sysParamsG], " parameters.\n"];

  dblSub = {x[1,1,0], y[1,1,0], z[1,1,0]};
  If[Length[Intersection[sysVarsG, dblSub]] == 0, 
    Throw[$Failed, failTag[lpEno12,lpSystemStatistics]]
  ];

  (* Check for any single-edge equations.  These will only reference x, x1 or x2 *)
  (* First determine what 2-sub variables are being used so we can eliminate     *)
  (* any equations that reference these from consideration.                      *)
  dblSub = Extract[sysVarsG, Position[sysVarsG, _[1,1,0]]];
  printSystat["D: systat, Checking equations for the dbl-sub variable: "];
  printSystat[convert2Print[dblSub]];

  (* Record which _ 12 variables appear.  Further computation will need this info *)
  Map[lpVariables[[ vUsed,var2Index[Head[#]] ]] = True; &, dblSub ];
  printSystat["D: systat, Variable information: ", lpVariables];

  (*  MonomialList will also return those equations not involving 2-sub variables*)
  (*  so intersecting the results w/ the original system will produce only those *)
  (*  equations which can be considered "edge" equations.                        *)
  edgeEQ = Intersection[Factor[ddEQ], Factor[Flatten[MonomialList[ddEQ,dblSub]]]];
  printSystat["D: systat, Equations not referencing dbl-subs: "];
  printSystat[convert2Print[edgeEQ]];

  dblEdge={};
  sglEdge={};
  If[Length[edgeEQ] > 0,
    printSystat["D: systat, Edge Equations: "];
    printSystat[convert2Print[edgeEQ]];

    (* Of these, we could have either single- or double-edge equations.        *)
    (* single edge reference only: x and x1 or x and x2 type variables         *)
    (* double edge reference x, x1 and x2 (but not x12)                        *)
    Map[
      tmpEQ = #;
      MapThread[
        vpos = Flatten[Position[Variables[tmpEQ], #1]];
        If[Length[vpos]>0,vpos= First[vpos],vpos=0];
        If[vpos > 0,
          printSystat["D: systat, equation references x1 type variables: "];
          printSystat[convert2Print[tmpEQ]];

          (* check if it also references other 1-sub, making this a 2-edge eq  *)   
          vpos = Flatten[Position[Variables[tmpEQ], #2]];
          If[Length[vpos]>0,vpos=First[vpos],vpos=0];
          If[vpos > 0,
            printSystat["D: systat, 2-edge equation: "];
            printSystat[convert2Print[tmpEQ]];
            dblEdge = Union[dblEdge, Simplify[{tmpEQ}]];
          , (* else *)
            sglEdge = Union[sglEdge, Simplify[{tmpEQ}]];
          ];
          printSystat["D: systat, dblEdge, sglEdge EQs: "];
          printSystat[Length[dblEdge], Length[sglEdge]];
        ];
      &, {{_[1,0,0],_[0,1,0]}, {_[0,1,0],_[1,0,0]}}];
    &, edgeEQ];
  ];

  singleEdgeEQG = sglEdge;
  doubleEdgeEQG = dblEdge;
  mainPrint["It contains ", Length[singleEdgeEQG], " single edge equation(s)."];
  If[Length[singleEdgeEQG]>0,
    mainPrint[convert2Print[ Map[# == 0&, singleEdgeEQG] ], "\n"];
  ];

  mainPrint["It contains ", Length[doubleEdgeEQG], " double edge equation(s)."];
  If[Length[doubleEdgeEQG]>0,
    mainPrint[convert2Print[Map[ # == 0 &, doubleEdgeEQG] ], "\n"];
  ];
  mainPrint["*********************************************************************\n"];

  (* Clear all local variables not being returned. *)
  Clear[eqLen, dblSub, edgeEQ, tmpEQ, vpos];

  printSystat["D: systat, exit"];
  Clear[printSystat];
  Return[];
]; (* end Module lpSystemStatistics *)




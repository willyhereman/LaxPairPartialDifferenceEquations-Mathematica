(* ::Package:: *)

(* ## ## ## ## ##              Function: LaxPairMenu            ## ## ## ## ## *)

(*******************************************************************************)
(* LaxPairMenu (no arguments)                                                  *)
(* Purpose: A menu to choose the DDE to calculate the corresponding Lax Pair   *)
(* Authors:                                                                    *)
(* Input:   When the program is first run, this menu is printed for            *)
(*          the user to choose the corresponding DDE they wish                 *)
(* Output:  The specifics of the selected equation                             *)
(* Adapted from:  rc_menu.m                                                    *)
(* Code is in File:  lp_menu.m                                                 *)
(* Last Modified:                                                              *)
(*******************************************************************************)

LaxPairMenu :=
Module[{choice, makeChoice, choice2, choice3, printLPMenu, ddEQ},

  (* Begin timing of run. *)
  startTime1 = AbsoluteTime[];
  startTime2 = TimeUsed[];

  If[dbLPMenu, printLPMenu = Print, Clear[printLPMenu], Clear[printLPMenu]];

  printLPMenu["D: LPMenu, Function: LaxPairMenu, File: lp_menu.m"];
  printLPMenu["D: ##############################################"];

  (* Make certain all variables defined in system files are clear prior to loading *)
  Clear[nameInput, ddEQ, ddeEquationListINPUT, laxPairMatrixL, laxPairMatrixM, sFunc, tFunc];

  Print[" "];
  Print["  *** MENU INTERFACE *** "];
  Print["-------------------------------------------"];
  Print["  Discrete Difference Equations \n"];
  Print["  Single Equations "];
  Print["   1) Discrete pKdV Equation"];
  Print["   2) Discrete Modified KdV Equation "];
  Print["   3) H1 Equation "];
  Print["   4) H2 Equation "];
  Print["   5) H3 Equation (delta = 0)"];
  Print["   6) H3 Equation (delta <> 0)"];
  Print["   7) Q1 Equation (delta = 0)"];
  Print["   8) Q1 Equation (delta <> 0) "];
  Print["   9) Q2 Equation "];
  Print["  10) Q3 Equation (delta = 0) "];
  Print["  11) Q3 Equation (delta <> 0) "];
  Print["  12) A1 Equation "];
  Print["  13) A2 Equation "];
  Print["  14) sine-Gordon Equation  "];
  Print["  15) Discrete Lotka-Volterra Equation  "];
  Print["  16) Discrete Potential Lotka-Volterra Equation  "];
  Print["  17) Hydon - Viallet Equation  "];
  Print["  18) Potential Hydon - Viallet Equation  \n"];
  Print["  Systems of Equations "];
  Print["  21) Discrete Boussinesq System"];
  Print["  22) Lattice Schwarzian Boussinesq System"];
  Print["  23) Toda/Modified Boussinesq System"];
  Print["  24) System of pKdV Equations"];
  Print["  25) System of NLS Equations"];
  Print["       ---------------  \n"];
  Print["  User Actions "];
  Print["  tt) Take Equation or System from a File"];
  Print["  qq) Exit the Program "];
  Print["-------------------------------------------"];

  choice = Input["ENTER YOUR CHOICE: "];
  makeChoice := Switch[choice,
      1, Get["sampleLattices/lp_KdV.m"];,
      2, Get["sampleLattices/lp_mKdV.m"];,
      3, Get["sampleLattices/lp_H1.m"];,
      4, Get["sampleLattices/lp_H2.m"];,
      5, Get["sampleLattices/lp_H3d0.m"];,
      6, Get["sampleLattices/lp_H3.m"];,
      7, Get["sampleLattices/lp_Q1d0.m"];,
      8, Get["sampleLattices/lp_Q1.m"];,
      9, Get["sampleLattices/lp_Q2.m"];,
      10, Get["sampleLattices/lp_Q3d0.m"];,
      11, Get["sampleLattices/lp_Q3.m"];,
      12, Get["sampleLattices/lp_A1.m"];,
      13, Get["sampleLattices/lp_A2.m"];,
      14, Get["sampleLattices/lp_sineG.m"];,
      15, Get["sampleLattices/lp_dLotVol.m"];,
      16, Get["sampleLattices/lp_dPLotVol.m"];,
      17, Get["sampleLattices/lp_HydVial.m"];,
      18, Get["sampleLattices/lp_PHydVial.m"];,
      19, Print["Invalid Selection"];,
      20, Print["Invalid Selection"];,
      21, Get["sampleLattices/lp_Bouss.m"];,
      22, Get["sampleLattices/lp_lsBSQ.m"];,
      23, Get["sampleLattices/lp_todambsq.m"];,
      24, Get["sampleLattices/lp_pKdVsys.m"];,
      25, Get["sampleLattices/lp_NLS.m"];,
      tt, Print["Make sure that you have prepared the data file for the"<>
              " system you want to test (similar to the data files we"<>
              " supplied)."];
           choice2 = Input["If your file is ready, press 1, else 2: "];
           If[choice2 === 1,
             choice3 = InputString["Enter the name of your data file: "];
             Get[choice3],
             Print["All computations are being discontinued."];
             Print["Prepare your data file first, then restart the program."];
             Abort[];
           ],
      qq, Print["All computations are being discontinued."]; Abort[],
      _,  choice = Input["Please enter a choice from the menu."];
          makeChoice
    ]; (* end Switch *)

  Check[makeChoice, Abort[]];

  printLPMenu["D: LPMenu: Menu Choice ", choice];
  printLPMenu["D: LPMenu: Menu DDE: "];
  mainPrint["********************** Start of Computation ************************"];
  If[choice > 20,
    mainPrint["Selected System of Discrete Difference Equations: ", nameINPUT];
  ,
    mainPrint["Selected Discrete Difference Equation: ", nameINPUT];
  ];
  mainPrint[convert2Print[ Map[# == 0 &, ddeEquationListINPUT] ]];
  mainPrint["********************************************************************\n"];

(*******************************************************************************)
(* ## ## ## ## ##               Start the program.               #### ## ## ## *)
(*******************************************************************************)
  printLPMenu["D: LPMenu: Calling lpDriver"];
  catchInternalError[
    lpDriver[ddeEquationListINPUT], lpDriver, failTag
  ];
  printLPMenu["D: LPMenu: Return from lpDriver"];

  (* Clear all local variables not being returned. *)
  Clear[choice, makeChoice, choice2, choice3, ddEQ];

  printLPMenu["dbg: LPMenu, exit"];
  Clear[printLPMenu];
]

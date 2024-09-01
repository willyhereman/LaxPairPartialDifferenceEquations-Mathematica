(* ::Package:: *)

(* ## ## ## Load all supporting files ## ## ## *)
Get["lpRules.m"];  Get["prtRules.m"]; Get["cvt2prt.m"];  Get["lpsystat.m"];
Get["augLatEq.m"]; Get["genLatEq.m"]; Get["verifcac.m"]; Get["verif123.m"];
Get["solvfrnt.m"]; Get["varinfo.m"]; Get["vycalclp.m"]; Get["verifylp.m"];
Get["chklinr.m"]; Get["complxpr.m"]; Get["ccorelax.m"]; Get["calclpST.m"];
Get["merglist.m"];

(* ## ## ## ## ##               Function: LPDriver              ## ## ## ## ## *)

(*******************************************************************************)
(* LPDriver[ddeEquationList]                                                   *)
(* Purpose:  Determine a variety of characteristics about the input dde        *)
(*           including variables used, parameters used, etc.                   *)
(*           This also branches based on the inputs of the user to calculate   *)
(*           1. Consistency About the Cube                                     *)
(*           2. Verify the Tetrahedron Property on the given lattice equ.      *)
(*           3. If possible, compute the corresponding Lax Pair                *)
(* Input:   List of discrete differential functions                            *)
(* Output:  Does not return anything                                           *)
(* Code is in File:  lpDriver.m                                                *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

lpDriver[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printLPD, dblVars={}},

  If[dbLPDriver, printLPD = Print, Clear[printLPD], Clear[printLPD]];
  contDriver = True;
  ret = False;

  printLPD["D: lpDriver, Function: lpDriver, File: lpDriver.m"];
  printLPD["D: ###############################################"];
  (* initialize necessary global variables  *)
  lpVariables = {{1,2,3}, supportedVars, {False, False, False}, {False, False, False}, {0,0,0}, 
                 {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}};
  vIdx=1; vName=2; vUsed=3; vLinear=4; vConst=5; vSub=6; sRule=7; subs=8; cmnDen=9;



  mainPrint["********************* Configuration Settings ***********************"];
  mainPrint["Verification of Consistency About the Cube set to ", pcCheckCAC];
  mainPrint["Computation of Lax Pair set to ", pcCompLaxPairs];
  If[pcCompLaxPairs,
    mainPrint["Note: Computed Lax Pair will automatically be verified against defining equation."];
  ];
  mainPrint["Verification of User Provided Lax Pair set to ", pcVerifyLPs];
  If[pcUserInput,
    mainPrint["The Interactive flag has been set to True.  "];
    mainPrint["This may require further input prior to completion."];
  ,
    mainPrint["The Interactive flag has been set to False."];
  ];
  mainPrint["********************************************************************\n"];

  operationG = oNone;
  If[catchInternalError[
    lpSystemStatistics[ddEQ], lpSystemStatistics, failTag] =!= Null
  ,
    Throw[$Failed, failTag[lpEfail,lpDriver]]
  ];

  (* Before any calculations, augment the original system if necessary         *)
  If[catchInternalError[
    augmentLatticEqu[ddEQ], augmentLatticEqu, failTag] =!= Null
  ,
    Throw[$Failed, failTag[lpEfail,lpDriver]]
  ];
  ddEQ = frontFaceEQG;

  mainPrint["********************************************************************\n"];

  (*  Generate the equations on the cube and keep them organized per face      *)
  (*  We will need some of these regardless of the functionality chosen.       *)
  genLatticeEquations[ddEQ];

  (* initialize our list of solutions found for this system  *)
  solutionListG = {};
  edgeConstraintsG = {};

  (****************   Check Consistency About the Cube    **********************)
  If [pcCheckCAC, 
    (* program control flagged requires computation of variable solutions *)
    mainPrint["************** Verifying Consistency About the Cube ****************"];
    operationG = oCAC;

    (* Initialize the global variables used in the CAC check workflow           *)
    varsFound = {};
    If[catchInternalError[
      verifyCAC[latticeEQG], verifyCAC, failTag] =!= Null
    ,
      Throw[$Failed, failTag[lpEfail,lpDriver]]
    ];
    printLPD["D: lpd:  Solution List: "];
    printLPD[convert2Print[solutionListG]];

    (* Reset the global variables used in the CAC check workflow                *)
    solutionListG = {};
    edgeConstraintsG = {};
    varsFound = {};
    mainPrint["*******************************************************************\n"];
  ,
    (* else  *)
    mainPrint["****************** Skipping Consistency Check *********************"];
    mainPrint["*******************************************************************\n"];
  ];

  (****************   Compute Lax Pairs     **********************)
  If [pcCompLaxPairs, 
    (* program control flagged requires computation of variable solutions *)
    mainPrint["********************** Computing Lax Pairs ************************"];
    operationG = oCLP;

    If[!pcCheckCAC,
      (* algorithm is designed for DDE's that are consistent about the cube.  If    *)
      (* the check hasn't been done, warn the user as such.                         *)
      mainPrint["*********** Warning:  Consistency check not performed. ************"];
      mainPrint["******** The Calculated Lax Pair are not guaranteed valid. ********"];
    ];

    If[catchInternalError[
      computeLaxPairs[latticeEQG], computeLaxPairs, failTag] =!= Null
    ,
      Throw[$Failed, failTag[lpEfail,lpDriver]]
    ];
    mainPrint["*******************************************************************\n"];
  ,
    (* else  *)
    mainPrint["**************** Skipping Lax Pair Computation ********************"];
    mainPrint["*******************************************************************\n"];
  ];

  (****************   Verify Lax Pairs     **********************)
  If [pcVerifyLPs, 
    (* program control flagged requires computation of variable solutions *)
    mainPrint["*************** Verifying User Provided Lax Pair *****************"];
    operationG = oVLP;

    If[catchInternalError[
      verifyLaxPairs[latticeEQG], computeLaxPairs, failTag] =!= Null
    ,
      Throw[$Failed, failTag[lpEfail,lpDriver]]
    ];
    mainPrint["********************************************************************\n"];
  ,
    (* else  *)
    mainPrint["******* Skipping Verification of User Provided Lax Pair ***********"];
    mainPrint["*******************************************************************\n"];
  ];

  printLPD["D: lpDriver, exit"];
]; (* end Module lpDriver *)

  Print["Lax Pair Software Driver of March 29, 2011 Successfully Loaded."];
(************************** End of File **********************************)

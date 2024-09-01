(* ::Package:: *)

(* Load all supporting files  *)
 Get["lpGlobals.m"]; Get["prtRules.m"]; Get["lpDriver.m"]; Get["lp_menu.m"];

(* This list of options controls both messaging of operation and a variety 
   of operating flags            *)
Options[LaxPairOps] = {Verbose -> None,
                   checkConsistencyOnCube -> True,
                   checkTetraProp -> True,
                   computeLaxPair -> True,
                   verifyUserLaxPair -> False,

                   userInteractive -> False,

                   consistencyOnCubeDebug -> False,
                   computeLaxPairDebug -> False,
                   verifyUserLaxPairDebug -> False,
                   utilitiesDebug -> False,

                   lpMenuDebug -> False,
                   lpDriverDebug -> False,
                   lpSystatDebug -> False,

                   prtVerifyCAC -> False,
                   prtVerify123 -> False,
                   prtVerifyLaxPair -> False,
                   prtCheckLinear -> False,
                   prtCheckFace -> False,

                   lpGenLatDebug -> False,
                   lpAugLatEqDebug -> False,
                   lpSolveFaceDebug -> False,
                   lpSolveFrontFaceDebug -> False,

                   compLPCoreDebug -> False,
                   compLPDebug -> False,
                   calcSTDebug -> False,
                   verifyCalcLPDebug -> False
};

Verbose::usage = "To use Verbose, type LaxPairOps[Verbose -> Option].\n"<>
    " Three options exist for Verbose:\n  None (default)  Only"<>
    " the final results are printed to the screen.\n  Minimal         A"<>
    " minimal amount of information showing the progress\n                 "<>
    " of the algorithm is printed to the screen.\n  All             All key"<>
    " steps are printed to the screen.  This setting\n                  is"<>
    " not recommended for calculations at high ranks\n                  or"<>
    " for calculations on Mathematica 6 or higher."

(* ## ## ## ## ##         Function: LaxPairOps          ## ## ## ## ## *)

(**********************************************************************)
(* LaxPairOps[Options]                                             *)
(* Purpose:  All initialization steps -- load neccessary supporting   *)
(*           files; initialize debug flags; load menu and capture     *)
(*           user input.                                              *)
(* Input:   Any changes to options found under Options[LaxPairOps]    *)
(* Output:  None                                                      *)
(* Code is in File:  LPopts.m                      *)
(* Created:                                        *)
(* Last Modified:                                  *)
(**********************************************************************)

LaxPairOptions[opts___?OptionQ] := Module[{},
  globalVerbose = Verbose /. {opts} /. Options[LaxPairOps];
  pcPrint = True;  (* used for the formal operation output *)

  (* use to allow user to specify various options during processing *)
  (* these include:                                                 *)
  (*   + edge equations, which variable to solve for                *)
  pcUserInput = userInteractive /. {opts} /. Options[LaxPairOps];

  (* establish program flow based on high-level work flow flags *)
  (* Verification of Consistency about the Cube *)
  pcCheckCAC = checkConsistencyOnCube /. {opts} /. Options[LaxPairOps];

  (* and corresponding debug if necessary *)
  cubeCon = consistencyOnCubeDebug /. {opts} /. Options[LaxPairOps];
  dbLPVerifyCAC = cubeCon;
  dbLPVerify123 = cubeCon;



  (* still need to complete verification of tetrahedron property  *)
  pcTetrahedronProp = checkTetraProp /. {opts} /. Options[LaxPairOps];



  (* Compputation of Lax Pair *)
  pcCompLaxPairs = computeLaxPair /. {opts} /. Options[LaxPairOps];

  (* and corresponding debug if necessary *)
  calcLP = computeLaxPairDebug /. {opts} /. Options[LaxPairOps];
  dbLPCompLP = calcLP;
  dbLPCompLPCore = calcLP;
  dbLPVerifyCLP = calcLP;
  dbLPCalcST = calcLP;



  (* Verification of user provided Lax Pair *)
  pcVerifyLPs = verifyUserLaxPair /. {opts} /. Options[LaxPairOps];

  (* and corresponding debug if necessary *)
  verifyLP = verifyUserLaxPairDebug /. {opts} /. Options[LaxPairOps];
  dbLPVerifyCLP = verifyLP;
  dbLPVerify = If[dbLPVerifyCLP, True, calcLP];
  


  (* also all utility files debug if necessary *)
  utilLP = utilitiesDebug /. {opts} /. Options[LaxPairOps];
  dbLPMenu = utilLP;
  dbLPDriver = utilLP;
  dbLPSysStat = utilLP;
  dbLPGenLatt = utilLP;
  dbLPAugLatt = utilLP;
  dbLPSolveFace = utilLP;
  dbLPSolveFF = utilLP;



  (* also support more specific debug flags  *)
  dbLPMenu = utilLP || (lpMenuDebug /. {opts} /. Options[LaxPairOps]);
  dbLPDriver = utilLP || (lpDriverDebug /. {opts} /. Options[LaxPairOps]);
  dbLPSysStat = utilLP || (lpSystatDebug /. {opts} /. Options[LaxPairOps]);

  dbLPGenLatt = utilLP || (lpGenLatDebug /. {opts} /. Options[LaxPairOps]);
  dbLPAugLatt = utilLP || (lpAugLatEqDebug /. {opts} /. Options[LaxPairOps]);
  dbLPSolveFace = utilLP || (lpSolveFaceDebug /. {opts} /. Options[LaxPairOps]);
  dbLPSolveFF = utilLP || (lpSolveFrontFaceDebug /. {opts} /. Options[LaxPairOps]);

(*
  dbLPVerifyCAC = prtVerifyCAC /. {opts} /. Options[LaxPairOps];
  dbLPVerify123 = prtVerify123 /. {opts} /. Options[LaxPairOps];
*)

(*
  dbLPVerifyLP = prtVerifyLaxPair/. {opts} /. Options[LaxPairOps];
*)
(*
  dbLPChkLinear = prtCheckLinear /. {opts} /. Options[LaxPairOps];
  dbLPChkFace = prtCheckFace /. {opts} /. Options[LaxPairOps];
*)

  dbLPCompLP = calcLP || (compLPDebug /. {opts} /. Options[LaxPairOps]);
  dbLPCompLPCore = calcLP || (compLPCoreDebug /. {opts} /. Options[LaxPairOps]);
  dbLPVerifyCLP = calcLP || (verifyCalcLPDebug /. {opts} /. Options[LaxPairOps]);
  dbLPCalcST = calcLP || (calcSTDebug /. {opts} /. Options[LaxPairOps]);

  mainPrint = If[pcPrint, Print, Clear[mainPrint], Clear[mainPrint]];

  (* Start the program by calling the menu function.                    *)
  ret = False;
  LaxPairMenu
] (* end Module LaxPairOps  *)

  Print["Lax Pair Option Driver of March 29, 2011 Successfully Loaded."];
(************************** End of File **********************************)

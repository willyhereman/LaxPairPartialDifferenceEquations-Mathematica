(* ::Package:: *)

(* ## ## ## ## ##       Function: verifyLaxPairs       ## ## ## ## ## *)

(**********************************************************************)
(* verifyLaxPairs[ddeEquationList]                                    *)
(* Purpose:   Verify the user provided L & M matrices satisfy         *)
(*           the defining equation L2M - M1L on the DDE               *)
(* Input:   List of discrete differential functions                   *)
(* Output:  List of discrete diff. functions adjusted for faces of    *)
(*          cube lattice                                              *)
(* Code is in File:  verifylp.m                                       *)
(*                                                                    *)
(* Last Modified:                                                     *)
(**********************************************************************)

verifyLaxPairs[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printVLP, dim, eqVars, faceSol, 
       defEQg, quotRule2, quotRule, eps, workingST},

  If[dbLPVerifyLP, printVLP = Print, Clear[printVLP], Clear[printVLP]];

  printVLP["D: verLP, Function: verifyLaxPairs, File: verifylp.m"];
  printVLP["D: #################################################"];
  printVLP["D: verLP, DDE: "];
  printVLP[convert2Print[ddEQ]];

  (* Verify that the user has provided matrices to verify *)
  If[Length[laxPairMatrixL] > 0 && Length[laxPairMatrixM] > 0,
    mainPrint["***************** User Provided Lax Pairs ********************"];
    mainPrint["L = ", convert2Print[MatrixForm[laxPairMatrixL]]];
    mainPrint["M = ", convert2Print[MatrixForm[laxPairMatrixM]]];

    dim = First[Dimensions[laxPairMatrixL]];
    If[explicitScalars, 
      (* the scalar functions are explicitly stated and not integrated into the Lax Pair *)
      mainPrint["*** User Explicitly Specified Additional Scalar Functions ***"];
      mainPrint["Scalar function associated with L: ", convert2Print[tFunc]];
      mainPrint["Scalar function associated with M: ", convert2Print[sFunc]];

      scalarL = tFunc*IdentityMatrix[dim];
      scalarM = sFunc*IdentityMatrix[dim];
    ,
      (* else, any scalar functions are assumed to be integrated into the Lax Pair   *)
      mainPrint["********** No Additional Scalar Functions Specified **********"];

      scalarL = IdentityMatrix[dim];
      scalarM = IdentityMatrix[dim];
    ];

    coreLPL = laxPairMatrixL;
    coreLPM = laxPairMatrixM;
    
    solutionListG = {};
    varsFound = {};

    printVLP["D: verLP, DDE: Calling verifyCalcLP"];
    verifyLP = verifyCalcLP[ddEQ];
    printVLP["D: verLP, DDE: Return from verifyCalcLP"];

    If[verifyLP,
      mainPrint["****** Lax Pair and scalar functions specified are valid.  ******"];
    ,
      mainPrint["**** Lax Pair and scalar functions specified are not valid.  ****"];
    ];

  , (* else *)
    mainPrint["****** Verification not complete -- No Lax Pairs provided. ******"];
  ];

  printVLP["D: verLP, exit"];
];

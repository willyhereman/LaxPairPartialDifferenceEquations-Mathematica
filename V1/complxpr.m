(* ::Package:: *)

(* ## ## ## ## ##       Function: computeLaxPairs      ## ## ## ## ## *)

(**********************************************************************)
(* computeLaxPairs[ddeEquationList]                                   *)
(* Purpose:                                                           *)
(* Input:   List of discrete differential functions                   *)
(* Output:  List of discrete diff. functions adjusted for faces of    *)
(*          cube lattice                                              *)
(* Code is in File:  complxpr.m                                       *)
(*                                                                    *)
(* Last Modified:                                                     *)
(**********************************************************************)

computeLaxPairs[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printCompLP, compLP, coreL, coreM, 
        numScalars},

  If[dbLPCompLP, printCompLP = Print, Clear[printCompLP], Clear[printCompLP]];

  printCompLP["D: cmpLP, Function: computeLaxPairs, File: complxpr.m"];
  printCompLP["D: ##################################################"];

  varsFound = {};
  coreLPL = {};
  scalarL = {};
  coreLPM = {};
  scalarM = {};
  matrixPhi = {};
  frontFaceSol = {};

  (* Reset the global variables used in the CAC check workflow                *)
  solutionListG = {};
  edgeConstraintsG = {};
  varsFound = {};

  printCompLP["D: cmpLP, Compute the Core L,M matrices"];
  If[catchInternalError[
    compCoreLaxPair[ddEQ], compCoreLaxPair, failTag] =!= Null
  ,
    Throw[$Failed, failTag[lpNoLxP, computeLaxPairs]]
  ];

  If[ Length[coreLPL]>0,
      mainPrint["*********************** Candidate Lax pair  **********************"];
      mainPrint["With respect to the matrix Phi:\n", MatrixForm[convert2Print[matrixPhi]]];
      numScalars = Length[Complement[Union[Flatten[scalarL]], {0}]];
      mainPrint["We have the following Candidate Lax pair (with ", numScalars*2,
        " unresolved scalar functions):"];

      gcdL = rationalPolyGCD[coreLPL];
      gcdsL = PolynomialGCD[Flatten[scalarL] /. List->Sequence];
      printCompLP["D: cmpLP, gcdL, gcdsL ", convert2Print[gcdL], " ", convert2Print[gcdsL]];
      If[gcdL*gcdsL != 1,
        coreL = Cancel[coreLPL/gcdL].Cancel[scalarL/gcdsL];
        mainPrint["Candidate L:\n", convert2Print[gcdL*gcdsL], MatrixForm[convert2Print[coreL]]];
      , (* else *)
        coreL = coreLPL.scalarL;
        mainPrint["Candidate L:\n", MatrixForm[convert2Print[coreL]]];
      , (* same behavior as if not equal to 1 *)
        coreL = Cancel[coreLPL/gcdL].Cancel[scalarL/gcdsL];
        mainPrint["Candidate L:\n", convert2Print[gcdL*gcdsL], MatrixForm[convert2Print[coreL]]];
      ];

      gcdM = rationalPolyGCD[Flatten[coreLPM]];
      gcdtM = PolynomialGCD[Flatten[scalarM] /. List->Sequence];
      printCompLP["D: cmpLP, gcdM, gcdsM ", convert2Print[gcdM], " ", convert2Print[gcdtM]];
      If[gcdM*gcdtM != 1,
        coreM = Cancel[coreLPM/gcdM].Cancel[scalarM/gcdtM];
        mainPrint["Candidate M:\n", convert2Print[gcdM*gcdtM], MatrixForm[convert2Print[coreM]]];
      , (* else *)
        coreM = coreLPM.scalarM;
        mainPrint["Candidate M:\n", MatrixForm[convert2Print[coreM]]];
      , (* same behavior as if not equal to 1 *)
        coreM = Cancel[coreLPM/gcdM].Cancel[scalarM/gcdtM];
        mainPrint["Candidate M:\n", convert2Print[gcdM*gcdtM], MatrixForm[convert2Print[coreM]]];
      ];
      mainPrint["*******************************************************************\n"];
  ];

  If[ Length[coreLPL]>0,
    mainPrint["************** Calculating possible scalar functions **************"];
    constantScalar = False;
    scalarFuncs = calculateScalarFunctions[coreL, coreM];
      mainPrint["******************** Computed Scalar Functions  *******************"];
      If[numScalars > 1, 
        mainPrint["The calculated scalar functions (in matrix form) are given by:"];
        mainPrint["Associated with matrix L: ", MatrixForm[convert2Print[scalarL]], "\n"];
        mainPrint["Associated with matrix M: ", MatrixForm[convert2Print[scalarM]], "\n"];
      , (* else *)
        mainPrint["The calculated scalar functions are given by:"];
        mainPrint["Associated with matrix L: ", convert2Print[gcdL*gcdsL], " = ",
               MatrixForm[convert2Print[Complement[Union[Flatten[scalarL]], {0}]]], "\n"];
        mainPrint["Associated with matrix M: ", convert2Print[gcdM*gcdtM], " = ",
               MatrixForm[convert2Print[Complement[Union[Flatten[scalarM]], {0}]]], "\n"];
      ];
      If[constantScalar, 
        mainPrint["where m, n are arbitrary functions of the parameters p, q, k"];
      ];
      mainPrint["*******************************************************************\n"];

      mainPrint["*********************** Candidate Lax pair  **********************"];
      mainPrint["With respect to the matrix Phi:\n", MatrixForm[convert2Print[matrixPhi]]];
      mainPrint["We have the following Candidate Lax pair:"];

      If[numScalars > 1, 
        mainPrint["Candidate L:\n", 
          MatrixForm[convert2Print[coreLPL]],
          MatrixForm[convert2Print[scalarL]]];
        mainPrint["Candidate M:\n", 
          MatrixForm[convert2Print[coreLPM]],
          MatrixForm[convert2Print[scalarM]]];
      ,
        mainPrint["Candidate L:\n", 
          MatrixForm[convert2Print[Complement[Union[Flatten[scalarL]], {0}]]],
          MatrixForm[convert2Print[coreLPL]]];
        mainPrint["Candidate M:\n", 
          MatrixForm[convert2Print[Complement[Union[Flatten[scalarM]], {0}]]],
          MatrixForm[convert2Print[coreLPM]]];
      ];
      mainPrint["*******************************************************************\n"];
  ];

  If[ Length[coreLPL]>0,
    (* Reset the global variables used in the CAC check workflow                *)
    solutionListG = {};
    varsFound = {};

    mainPrint["****************** Verifying Calculated Lax pair ******************"];
    verifyLP = verifyCalcLP[ddEQ];
    If[verifyLP, 
      mainPrint["******** Lax pair Satisfy Defining Equation L2.M - M1.L = 0 ********"];
    , (* else *)
      mainPrint["\n***** Lax pair Do Not Satisfy Defining Equation L2.M - M1.L = 0 *****"];
      mainPrint["\n***************** Calculated Lax pair is not valid ****************"];
    ];
    mainPrint["*******************************************************************\n"];
  ];

  printCompLP["D: cmpLP, exit"];
];


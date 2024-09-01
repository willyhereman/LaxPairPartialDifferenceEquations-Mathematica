(* ::Package:: *)

(* ## ## ## ##     Function: calculateScalarFunctions      ## ## ## ## *)

(************************************************************************)
(* calculateScalarFunctions[definingEQ]                                 *)
(* Purpose:  Given the defining equation, calculate candidate scalar    *)
(*           functions s, t (and possibly S, T)                         *)
(* Input:   Reduced (by given DDE) defining equation                    *)
(* Output:  List of scalar function pairs (if possible)                 *)
(* Code is in File:  calclpST.m                                         *)
(*                                                                      *)
(* Last Modified:                                                       *)
(************************************************************************)

calculateScalarFunctions[matrixL_, matrixM_ ] :=
Module[{printCST, matL = matrixL, matM = matrixM, matL2, detL, matM1, detM,
        tL, tL2, sM, sM1, detVars, dim, lhsEQ, lhsDetEQ, rhsEQ, rhsDetEQ, matEQ,
        stQuotRule, ratEQ, ratEQred, ratEQsol, aRat, scalarRuleL, scalarRuleM},

  If[dbLPCalcST, printCST = Print, Clear[printCST], Clear[printCST]];
  Clear[a];
  aRat = a;

  printCST["D: clcST, Function: calculateScalarFunctions, File: calclpST.m"];
  printCST["D: ##########################################################"];
  printCST["D: clcST, core Matrices: "];
  printCST[convert2Print[matL]];
  printCST[convert2Print[matM]];

  printCST["D: clcST, Calculating s,t using determinants (L2)(M)=(M1)(L)"];
  matL2 = matL /. lat2ShiftRules;
  detL = Simplify[Det[matL]];
  tL = scalarL;
  tL2 = tL /. lat2ShiftRules;
  matM1 = matM /. lat1ShiftRules;
  detM = Simplify[Det[matM]];
  sM = scalarM;
  sM1 = sM /. lat1ShiftRules;

  lhsEQ = Simplify[(matL2.tL2).(matM.sM)];
  rhsEQ = Simplify[(matM1.sM1).(matL.tL)];
  lhsDetEQ = Simplify[Det[lhsEQ]];
  rhsDetEQ = Simplify[Det[rhsEQ]];

  mainPrint["*********** Determinant equation: Det[L2.M] = Det[M1.L]  **********"];
  mainPrint[MatrixForm[convert2Print[lhsDetEQ]], " = ",
            MatrixForm[convert2Print[rhsDetEQ]], "\n" ];

  mainPrint["Note:  The scalar functions may also be determined using the original\n",
            "       matrix equation, L2.M - M1.L = 0"];
  mainPrint[MatrixForm[convert2Print[matL2]], ".", MatrixForm[convert2Print[tL2]],
       ".", MatrixForm[convert2Print[matM]], ".", MatrixForm[convert2Print[sM]], "\n",
       " - ", MatrixForm[convert2Print[matM1]], ".", MatrixForm[convert2Print[sM1]],
       ".", MatrixForm[convert2Print[matL]], ".", MatrixForm[convert2Print[tL]], " = 0\n"];
  printCST["D: clcST, Front Face Solutions: ", Flatten[MatrixForm[convert2Print[frontFaceSol]]] ];

  mainPrint["************ Matrix equation combined and simplified  ************"];
  matEQ = Simplify[(lhsEQ - rhsEQ)];

  (* Using the 2,1 entry, compute the corresponding scalar ratio, (s*t2)/(t*s1)   *)
  printCST["D: clcST, matrix entry: ", convert2Print[ If[Length[ matEQ[[2,1]] ]> 1, matEQ[[2,1]], matEQ[[1,1]]] ]];
  stQuotRule = {s[1] -> (aRat*s1[1]*t[1])/t2[1]};
  ratEQ = Simplify[If[Length[ matEQ[[2,1]] ]> 1, matEQ[[2,1]] /. stQuotRule, matEQ[[1,1]] /. stQuotRule]];
  printCST["D: clcST, in process matrix entry: ", convert2Print[ratEQ]];
  ratEQred = Simplify[ratEQ /. frontFaceSol[[3]] /. frontFaceSol[[2]] /. frontFaceSol[[1]] ];
  printCST["D: clcST, reduced matrix entry: ", convert2Print[ratEQ]];
  ratEQsol = Simplify[Solve[ratEQred == 0, aRat]];
  printCST["D: clcST, ratio: ", convert2Print[ratEQsol]];
  mainPrint["***************** Resulting scalar function ratio  ****************"];
  mainPrint["(t2 s)/(t s1) = ", convert2Print[aRat /. ratEQsol] ];

  mainPrint["********* Matrix equation combined and reduced using DDE  ********"];
  matEQ = Simplify[(lhsEQ - rhsEQ) /. frontFaceSol[[3]] /. frontFaceSol[[2]] /. frontFaceSol[[1]] ];
  mainPrint[MatrixForm[convert2Print[matEQ]], " = 0\n"];

  If[Length[Variables[Det[tL]]] > 1, 
    (* we have more than one scalar function involved => must use a different approach *)
    printCST["D: clcST, tL Determinant: ", convert2Print[Det[tL]]];

    (* For now, hard code solutions for pKdV system's scalar functions  *)
    scalarL = scalarL /. {t[1] -> n, t[2] -> n};
    scalarM = scalarM /. {s[1] -> m, s[2] -> m}; 
    constantScalar = True;
  , (* else  *)
    (* single scalar function => calculate s,t directly  *)
    detVars = Intersection[Variables[detL], sysVarsG];
    printCST["D: clcST, Variables appearing in t: ", convert2Print[detVars]];
    If[Length[detVars] > 0, 
      dim = First[Dimensions[tL]];
      scalarRuleL = {t[1] -> Power[1/detL,1/dim]};
    ,
      (* No variables appear, so our scalar function is an arbitrary constant *)
      scalarRuleL = {t[1]-> m};
      constantScalar = True;
    ];
    scalarL = scalarL /. scalarRuleL;
    printCST["D: clcST, scalar function t: ", convert2Print[scalarL]];

    detVars = Intersection[Variables[detM], sysVarsG];
    printCST["D: clcST, Variables appearing in s: ", convert2Print[detVars]];
    If[Length[detVars] > 0, 
      dim = First[Dimensions[sM]];
      scalarRuleM = {s[1] -> Power[1/detM,1/dim]};
    ,
      (* No variables appear, so our scalar function is an arbitrary constant *)
      scalarRuleM = {s[1] -> n};
      constantScalar = True;
    ];
    scalarM = scalarM /. scalarRuleM;
    printCST["D: clcST, scalar function s: ", convert2Print[scalarM]];
  ];

  Clear[matL, matL2, detL, matM, matM1, detM,
        tL, tL2, sM, sM1, detVars, dim, rhsEQ, rhsDetEQ, lhsEQ, lhsDetEQ];

  printCST["D: clcST, exit"];
  Clear[printCST];

  Return[];
];

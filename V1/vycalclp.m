(* ::Package:: *)

(* ## ## ## ## ##       Function: verifyCalcLP       ## ## ## ## ## *)

(**********************************************************************)
(* verifyCalcLP[ddeEquationList]                                      *)
(* Purpose:  Verify the just calculated L & M matrices satisfy        *)
(*           the defining equation L2M - M1L on the DDE               *)
(* Input:   List of discrete differential functions                   *)
(* Output:                                                            *)
(* Code is in File:  vycalclp.m                                       *)
(*                                                                    *)
(* Last Modified:                                                     *)
(**********************************************************************)

verifyCalcLP[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printVCLP, defEQ, faceSol, fsols,
        filterList, defEQ12, matL, matL2, matM, matM1, matS, matS2, matT, matT1,
        matM1L, matL2M, gcdL, gcdL2, gcdS, gcdS2, gcdM, gcdM1, gcdT,
        gcdT1, gcdL2M, gcdM1L, pwr,gcdL2Mmat,gcdM1Lmat,gcdL2Mred,gcdM1Lred,
       defEQg, quotRule2, quotRule, eps, tmp},

  If[dbLPVerifyCLP, printVCLP = Print, Clear[printVCLP], Clear[printVCLP]];

  printVLP["D: verCLP, Function: verifyCalcLP, File: vycalclp.m"];
  printVLP["D: #################################################"];
  printVLP["D: verCLP, DDE: "];
  printVLP[convert2Print[ddEQ]];

  (* form the defining equation from the calculated lax pair    *)
  (* Break each matrix down to scalar * matrix combinations     *)
  (* LAX Pair L core *)
  gcdL = rationalPolyGCD[Flatten[coreLPL]];
  matL = Cancel[coreLPL/gcdL];
  printVCLP["D: verCLP, Computing components of L and L2"];
  printVCLP["D: verCLP, L core: \n", convert2Print[gcdL], MatrixForm[convert2Print[matL]]];
  (* LAX Pair L2 core *)
  matL2 = matL /. lat2ShiftRules;
  gcdL2 = gcdL /. lat2ShiftRules;
  printVCLP["D: verCLP, L2 core: \n", convert2Print[gcdL2], MatrixForm[convert2Print[matL2]]];

  (* Since the scalar functions are actually matrices internally, *)
  (* we'll need to break these down to scalar * matrix combo      *)
  (* scalar function s *)
  gcdS = rationalPolyGCD[Flatten[scalarL]];
  matS = Cancel[scalarL/gcdS];
  printVCLP["D: verCLP, L scalar: \n", convert2Print[gcdS], MatrixForm[convert2Print[matS]]];
  (* scalar function s2 *)
  matS2 = matS /. lat2ShiftRules;
  gcdS2 = gcdS /. lat2ShiftRules;
  printVCLP["D: verCLP, L2 scalar: \n", convert2Print[gcdS2], MatrixForm[convert2Print[matS2]]];

  (* LAX Pair M core *)
  gcdM = rationalPolyGCD[Flatten[coreLPM]];
  matM = Cancel[coreLPM/gcdM];
  printVCLP["D: verCLP, Computing components of M and M1"];
  printVCLP["D: verCLP, M core: \n", convert2Print[gcdM], MatrixForm[convert2Print[matM]]];
  (* LAX Pair M1 core *)
  matM1 = matM /. lat1ShiftRules;
  gcdM1 = gcdM /. lat1ShiftRules;
  printVCLP["D: verCLP, M1 core: \n", convert2Print[gcdM1], MatrixForm[convert2Print[matM1]]];

  (* scalar function t *)
  gcdT = rationalPolyGCD[Flatten[scalarM]];
  matT = Cancel[scalarM/gcdT];
  printVCLP["D: verCLP, M scalar: \n", convert2Print[gcdT], MatrixForm[convert2Print[matT]]];
  (* scalar function t1 *)
  matT1 = matT /. lat1ShiftRules;
  gcdT1 = gcdT /. lat1ShiftRules;
  printVCLP["D: verCLP, M1 scalar: \n", convert2Print[gcdT1], MatrixForm[convert2Print[matT1]]];

  (* The defining equation of (s2.L2).(t.M) - (s.L).(t1.M1)  *)
  (* is now represented as (L2'*S2')*(T'*M'))*(s2.L2).(t.M) - (S'*L')*(T1'*M1')*(s.L).(T1.M1)  *)
  (* In addition to simplifying the expression, we can also isolate any roots appearing         *)
  gcdL2M = (gcdL2*gcdS2)*(gcdM*gcdT);
  printVCLP["D: verCLP, Computing GCD of scalars associated w/ L2 and M and those w/ M1 and L"];
  printVCLP["D: verCLP, L2M gcd scalars: \n", convert2Print[gcdL2M]];
  gcdM1L = (gcdM1*gcdT1)*(gcdL*gcdS);
  printVCLP["D: verCLP, M1L gcd scalars: \n", convert2Print[gcdM1L]];

  matL2M = Simplify[(matL2.matS2).(matM.matT)];
  matM1L = Simplify[(matM1.matT1).(matL.matS)];
  mainPrint["*************** Defining Equation L2.M - M1.L = 0 ******************"];
  mainPrint["(",
   convert2Print[gcdL2M], " times ", MatrixForm[convert2Print[matL2M]], " - \n",
   convert2Print[gcdM1L], " times ", MatrixForm[convert2Print[matM1L]],
   ") = 0\n"];

  (* We will verify the defining equation against the original dde     *)
  (* To do this, we will reduce the dde to a set of variable solutions *)
  fsols = frontFaceSolutions;
  printVCLP["D: verCLP, Solutions found from Front Face: \n", convert2Print[fsols]];
  If[Length[fsols]==0,
    Goto[failVerifyReturn];
  ];

  (* Reduction will be done a 2-step process:                          *)
  (* 1. reduce w/ _12 solutions (know to occur in L2.M-M1.L            *)
  (* 2. if necessary, reduce using any remaining solutions             *)
  printVCLP["D: verCLP, reduce L2M using _12 solutions\n", convert2Print[ fsols[[3]] ]];
  matL2Mred = Map[Factor, (matL2M /. fsols[[3]] )];
  printVCLP["D: verCLP, matrix L2M reduced\n", MatrixForm[convert2Print[matL2Mred]]];

  printVCLP["D: verCLP, reduce L2M using \n", convert2Print[ fsols[[2]] ], "\n", convert2Print[ fsols[[1]] ]];
  matL2Mred = Map[Factor, (matL2Mred /. fsols[[2]])];
  matL2Mred = Map[Factor, (matL2Mred /. fsols[[1]])];
  printVCLP["D: verCLP, matrix L2M reduced\n", MatrixForm[convert2Print[matL2Mred]]];

  gcdL2Mmat = PolynomialGCD[Map[Numerator[#]&, Flatten[matL2Mred]] /. List->Sequence]/
           PolynomialLCM[Complement[Map[myDenominator[#]&, Flatten[matL2Mred]],{0}] /. List->Sequence];
  printVCLP["D: verCLP, gcd of entries of the L2M matrix\n", convert2Print[gcdL2Mmat]];
  matL2Mred = Simplify[matL2Mred/gcdL2Mmat];
  printVCLP["D: verCLP, L2M matrix with gcd factored out\n", convert2Print[matL2Mred]];

  printVCLP["D: verCLP, reduce M1L using \n", convert2Print[ fsols[[3]] ]];
  matM1Lred = Map[Factor, (matM1L /. fsols[[3]] )];
  printVCLP["D: verCLP, matrix M1L reduced\n", MatrixForm[convert2Print[matM1Lred]]];

  printVCLP["D: verCLP, reduce M1L using \n", convert2Print[ fsols[[2]] ], "\n", convert2Print[ fsols[[1]] ]];
  matM1Lred = Map[Factor, (matM1Lred /. fsols[[2]] )];
  matM1Lred = Map[Factor, (matM1Lred /. fsols[[1]] )];
  printVCLP["D: verCLP, matrix L2M reduced\n", MatrixForm[convert2Print[matM1Lred]]];

  gcdM1Lmat = (PolynomialGCD[Map[Numerator[#]&, Flatten[matM1Lred]] /. List->Sequence])/
          (PolynomialLCM[Complement[Map[myDenominator[#]&, Flatten[matM1Lred]],{0}] /. List->Sequence]);
  printVCLP["D: verCLP, gcd of entries of the M1L matrix\n", convert2Print[gcdM1Lmat]];
  matM1Lred = Simplify[matM1Lred/gcdM1Lmat];
  printVCLP["D: verCLP, M1L matrix with gcd factored out\n", convert2Print[matM1Lred]];

  gcdL2Mred = Factor[Factor[Factor[gcdL2M /. fsols[[3]] ] /. fsols[[2]] ] /. fsols[[1]] ];
  gcdM1Lred = Factor[Factor[Factor[gcdM1L /. fsols[[3]] ] /. fsols[[2]] ] /. fsols[[1]] ];
  printVCLP["D: verCLP, defining equation parts (after reductions of matrices): \n",
   "(",
   convert2Print[gcdL2Mred], " times ", convert2Print[gcdL2Mmat], " times", 
                  MatrixForm[convert2Print[matL2Mred]], "\n",
   convert2Print[gcdM1Lred], " times ", convert2Print[gcdM1Lmat], " times",
                  MatrixForm[convert2Print[matM1Lred]],
   ")"];

  (* Now that we have the pieces, we'll convert our standard defining equation *)
  (* as follows           *)
  (* s2t*(L2.M) - t1s*(M1.L) =>  (s2t)/(t1s)*(L2.M) - (M1.L)                    *)
  defEQscalars = Factor[(gcdL2Mred*gcdL2Mmat)/(gcdM1Lred*gcdM1Lmat)];
  printVCLP["D: verCLP, defining eq parts (scalar clean-up): \n",
   "(",
   convert2Print[defEQscalars], " times (",  MatrixForm[convert2Print[matL2Mred]], "\n",
   " minus ", MatrixForm[convert2Print[matM1Lred]], ")",
   ")"];


  defEQscalars = Factor[Factor[Factor[defEQscalars /. fsols[[3]] ] /. fsols[[2]] ] /. fsols[[1]] ];
  mainPrint["************** Defining Equation reduced using DDE *****************"];
  mainPrint["(",
   convert2Print[defEQscalars], " times (",  MatrixForm[convert2Print[matL2Mred]], ")\n",
   " - ", MatrixForm[convert2Print[matM1Lred]], ") = 0\n"];

  pwr = First[Dimensions[coreLPL]];
  mainPrint["******* Verifying components of reduced defining equation **********"];
  mainPrint["The leading scalar coefficient should be either 1 or -1."];
  scalarsRed = Factor[PowerExpand[Power[defEQscalars,pwr]]];
  mainPrint["(", convert2Print[defEQscalars], ")^", pwr, " = ", convert2Print[scalarsRed], "\n"];

  mainPrint["The matrix difference should then be 0."];
  defEQred = Simplify[matL2Mred - matM1Lred];

  tmp = Union[Flatten[defEQred]];
  If[Length[tmp] == 1 && First[tmp] == 0,
    (* Valid Lax Pair since defining equations reduces to 0 *)
    mainPrint[MatrixForm[convert2Print[defEQred]]];
    printVCLP["D: verCLP, Specified Lax Pair satisfy defining equation."];
    tmp = True;
  ,
    (* else - given Lax Pair do not satisfy the defining equation  *)
    printVCLP["D: verCLP, Non-zero => Double-check for any negatives introduced before concluding failure."];
    defEQred = Simplify[matL2Mred + matM1Lred];
    tmp = Union[Flatten[defEQred]];
    If[Length[tmp] == 1 && First[tmp] == 0,
      mainPrint[MatrixForm[convert2Print[defEQred]]];
      printVCLP["D: verCLP, Specified Lax Pair satisfy defining equation."];
      tmp = True;
    ,
      tmp = False;
      mainPrint[MatrixForm[convert2Print[defEQred]]];
      printVCLP["D: verCLP, Specified Lax Pair do not satisfy defining equation."];
    ];
  ];
  
  printVCLP["D: verCLP, exit"];
  Clear[printVCLP];
  Return[tmp];

  Label[failVerifyReturn];
  mainPrint["********** Verification of Calculated Lax pair failed *************"];
  Return[];
];

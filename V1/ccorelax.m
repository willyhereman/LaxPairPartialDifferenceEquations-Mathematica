(* ::Package:: *)

(* ## ## ## ## ##       Function: compCoreLaxPair      ## ## ## ## ## *)

(**********************************************************************)
(* compCoreLaxPair[ddeEquationList]                                   *)
(* Purpose:                                                           *)
(* Input:   List of discrete differential functions                   *)
(* Output:  List of discrete diff. functions adjusted for faces of    *)
(*          cube lattice                                              *)
(* Code is in File:  ccorelax.m                                       *)
(*                                                                    *)
(* Last Modified:                                                     *)
(**********************************************************************)

compCoreLaxPair[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, printCLPCore, currVar, currVarPos, tmpVar,
        subRuleList={}, denGCD, dscale,
        lpLinearRules = {}, compLP = True, subFuncRules = {}, l2mRules={}, num,
        i, den, matrixRow, matrixL, matrixM, matPhi, edgeEQ, constrainedVar,
        flag, matchingDenom, tmpList},

  If[dbLPCompLPCore, printCLPCore = Print, Clear[printCLPCore], Clear[printCLPCore]];

  printCLPCore["D: cLPc, Function: compCoreLaxPairs, File: ccorelax.m"];
  printCLPCore["D: ####################################################"];

  lpLinearRules = {f[1] -> mu*f[1], g[1] -> mu*g[1],
                  f[2] -> mu*f[2], g[2] -> mu*g[2],
                  f[3] -> mu*f[3], g[3] -> mu*g[3]};
  subFuncRules = {f[1]->f[1], g[1]->g[1], f[2]->f[2], g[2]->g[2], f[3]->f[3],g[3]->g[3]};
  l2mRules = {x[1,0,0] -> x[0,1,0], y[1,0,0] -> y[0,1,0], z[1,0,0] -> z[0,1,0], p->q, t->s};

  matchingDenom[x_, y_] := If[Length[x]!=0, Denominator[Last[First[x /. subFuncRules]]]==y, False];


  (**** utility functions  ****)
  (* edgeConstraint:  based on the given list of variables of the form _[0,0,1], this *)
  (* determines if there is 1 or no subscripts and if 1, then is the variable _ 3      *)
  (* If all variables are either x or x_ 3, returns True, otherwise False              *)
  edgeConstraint[x_] := And @@ Map[If[Length[Position[# /. {Head[#] -> List},1]]>0 &&
      First[Flatten[Position[# /. {Head[#]-> List},1]]] != 3, False, True]&, x];


  (* To compute the Lax Pair, we need solutions from the Front, Bottom and Left faces *)
  (* We will compute the solutions regardless of any work done w/ CAC                 *)
  (* Solving for variables in the original equations (aka Front Face) will indicate   *)
  (* which _ 13 and _ 23 variables can be solved for.                                 *)
  faceVariables = {};
  frontFaceSol = solveFrontFace[frontFaceEQG];
  printCLPCore["D: cLPc, Solutions found (Front Face):\n", convert2Print[frontFaceSol]];
  solutionListG = frontFaceSol;
  printCLPCore["D: cLPc, Variable solved for (Front Face):\n", convert2Print[faceVariables]];
  varsFF = faceVariables;
  printCLPCore["D: cLPc, Constraints found:", convert2Print[edgeConstraintsG]];  
  variableInfo[printCLPCore, lpVariables];

  (* any _ 12 variables found indicate that the corresponding _ 13 & _ 23 will be found *)
  (* these must be considered in the lax pair computation.  Save the list.            *)
  Map[lpVariables[[ vUsed,var2Index[Head[#]] ]] = True; &, Cases[varsFF,_[1,1,0]] ];
  flag = False;
  MapThread[
    If[#3,
      printCLPCore["D: cLPc, ", #2, " may contribute to Lax pair computation."];
      flag = flag || True;
    ,
      printCLPCore["D: cLPc, ", #2, " will not be used in Lax pair computation."];
      flag = flag || False;
    ];
  &, lpVariables];
  (* If no variable can be used in Lax pair calculation, abort *)
  If[!flag, 
    Throw[$Failed, failTag[lpEno12,compCoreLaxPair]]
  ];

  (* Shift the variables found on the front face to get the list of variables         *)
  (* to find on the left face                                                         *)
  varsLF = Complement[varsFF /. latFrnt2LeftRules, varsFF];
  printCLPCore["D: cLPc, Variables to solve for (Left Face):\n",convert2Print[varsLF]];

  faceVariables = {};
  faceSol = MapIndexed[Complement[(#1 /. latFrnt2LeftRules), solutionListG[[First[#2]]]]&, frontFaceSol];
  (*  faceSol = solveFaceEqu[leftFaceEQG, varsLF];  *)
  printCLPCore["D: cLPc, Solutions found (Left Face):\n", convert2Print[faceSol]];
  solutionListG = mergeSolLists[solutionListG, faceSol];
  printCLPCore["D: cLPc, Solutions found thus far (Front/Left):\n", convert2Print[solutionListG]];
  (*  printCLPCore["D: cLPc, Variables found (Left Face):\n", convert2Print[faceVariables]];  *)
  
  (* Shift the variables found on the front face to get the list of variables   *)
  (* to find on the bottom face                                                 *)
  varsGF = Complement[varsFF /. latFrnt2GrndRules, varsFF];
  printCLPCore["D: cLPc, Variables to solve for (Bottom Face):\n", convert2Print[varsGF]];

  faceVariables = {};
  faceSol = MapIndexed[Complement[(#1 /. latFrnt2GrndRules), solutionListG[[First[#2]]]]&, frontFaceSol];
  (*  faceSol = solveFaceEqu[groundFaceEQG, varsGF];  *)
  printCLPCore["D: cLPc, Solutions found (Bottom Face):\n", convert2Print[faceSol]];
  solutionListG = mergeSolLists[solutionListG, faceSol];
  mainPrint["****** Solutions found using Front, Left & Bottom faces ***********"];
  mainPrint[convert2Print[solutionListG]];
  (*  printCLPCore["D: cLPc, Variables found (Bottom Face):\n", convert2Print[faceVariables]];  *) 
  

  (* To compute the Lax pair, we need all _ 13 solutions and _ 23 solutions.    *)
  (* We also need any _ 3 solutions that may impose additional constraints on  *)
  (* our substitutions.                                                       *)
  solList = Union[Part[solutionListG[[3]], Complement[Map[If[Part[#,2]==1,First[#]]&, 
                             Position[solutionListG[[3]],_[1,0,1]]],{Null}]],
                  Part[solutionListG[[3]], Complement[Map[If[Part[#,2]==1,First[#]]&, 
                             Position[solutionListG[[3]],_[0,1,1]]],{Null}]],
                  Part[solutionListG[[2]], Complement[Map[If[Part[#,2]==1,First[#]]&, 
                             Position[solutionListG[[2]],_[0,0,1]]],{Null}]]];

  (* We do not reduce the 2-sub against the 1-sub found.  It's essential the _ 13 and *)
  (* _ 23 vars reference _ 3 vars for our Lax Pair calculation to succeed.             *)
  (* printCLPCore["D: cLPc, Raw solutions for LP compuation:\n", convert2Print[solList]]; *)
  (* If[Length[ frontFaceSol[[2]] ]>0,  *)
  (*   tmpList = {};  *)
  (*   Map[tmpList = Union[tmpList, {Simplify[# /. frontFaceSol[[2]]]}];&, solList ];  *)
  (*   solList = tmpList;  tmpList = {};  *)
  (* ];  *)
  mainPrint["********* Solutions to use in computation of lax pair *************"];
  mainPrint[convert2Print[solList]];

  (* For each variable in the list, see if the corresponding solutions are  *)
  (* linear in the 3 variable.                                              *)
  MapThread[
    If[#3,
      currVar = #2;
      (* 1.  Do any of the _ 3 variables appear in these solutions            *)
      (* 2.  And if so, does it appear linearly                               *)
      MapThread[
        If[#3,
          v2chk = #2;
          (* We need only check the _ 13 variables as the linearity of the _ 3   *)
          (* variables will be the same for both _ 13 and _ 23 variables.       *)
          num = Numerator[currVar[1,0,1] /. solList] /. {v2chk[0,0,1] -> mu*v2chk[0,0,1]};
          den = Denominator[currVar[1,0,1] /. solList] /. {v2chk[0,0,1] -> mu*v2chk[0,0,1]};
          If[Exponent[Expand[num], mu] != 0,
            lpVariables[[ vLinear,#1 ]] = (Exponent[Expand[num],mu] <= 1 && 
                                            Exponent[Expand[den],mu] <= 1);
          ,
            lpVariables[[ vLinear,#1 ]] = True;
          ];
          printCLPCore["D: cLPc, Variable Linearity (num, den):\n", 
                 convert2Print[num], " and ", convert2Print[den] ];
          printCLPCore["D: cLPc, mu exponent (num, den):\n", 
                 Exponent[Expand[num],mu], " and ", Exponent[Expand[den],mu] ];
          printCLPCore["D: cLPc, Variable Linearity (result): ", lpVariables[[ vLinear, #1 ]] ];
        ];
      &, lpVariables];
    ];
  &, lpVariables];
  variableInfo[printCLPCore, lpVariables];
  flag = False;
  MapThread[
    currVar = #2;
    If[#3,
      If[#4,
        printCLPCore["D: cLPc, ", convert2Print[currVar[0,0,1]], " is referenced linearly."];
        flag = flag || True;
      ,
        printCLPCore["D: cLpc, ", convert2Print[currVar[0,0,1]], " does not appear linearly."];
        flag = flag || False;
      ];
    ,
      printCLPCore["D: cLpc, ", convert2Print[currVar[0,0,1]], " is not referenced."];
      flag = flag || False;
    ];
  &, lpVariables];
  (* If no variable appears or appears linearly, abort *)
  If[!flag, 
    Throw[$Failed, failTag[lpNoLin,compCoreLaxPair]]
  ];


  (* Create the substitution rules for _ 3 variables used to derive Lax pair *)
  (* Additional constraints come from edge equations involving _ 3 variables *)
  (* If the system has any, they will also show as solutions                 *)
  (*  edgeEQ = Complement[Map[If[Count[First[#],1] == 1 &&    *)
  (*     First[Flatten[Position[First[#] /. {Head[First[#]]-> List},1]]]==3, #,0]  *)
  (*     &, solList], {0}];  *)
  edgeEQ = {};
  Map[
    If[First[Flatten[Position[First[#] /. {Head[First[#]]-> List},1]]]==1, 
      edgeEQ = Union[edgeEQ, {# /. latFrnt2LeftRules}];
    ];
    If[First[Flatten[Position[First[#] /. {Head[First[#]]-> List},1]]]==2, 
      edgeEQ = Union[edgeEQ, {# /. latFrnt2GrndRules}];
    ];
  &, edgeConstraintsG];
  printCLPCore["D: cLPc, Edge equations involving _3:", convert2Print[edgeEQ]];

  (* If any _ 3 variable solutions are found, they must also have only no     *)
  (* subscript variables or _ 3 variables.                                    *)
  Map[
    var3 = Variables[First[#] /. #];
    printCLPCore["D: cLPc, solution:", convert2Print[#], " contains ", convert2Print[var3]];
    printCLPCore["D: cLPc, solution:", convert2Print[#], " is valid: ", edgeConstraint[var3]];
    If[edgeConstraint[var3],
      lpVariables[[ vConst,var2Index[Head[First[#]]] ]] = 
        Head[ var3[[ First[Flatten[Position[var3, _[0,0,1]]]] ]] ];
    ];
  &, edgeEQ];
  variableInfo[printCLPCore, lpVariables];

  MapThread[
    If[#3,
      currVar = #2;
      If[#5 == 0, 
        printCLPCore["D: cLPc, ", convert2Print[currVar[0,0,1]], " is unconstrained. "];
      , (* the conditional will not evaluate to False - Mathematica is comparing an unknown to 0 *)
        False;
      ,
        tmpVar = #5;
        printCLPCore["D: cLPc, ", convert2Print[currVar[0,0,1]], 
                   " is constrained by ", convert2Print[tmpVar[0,0,1]]];
      ];
    ];
  &, lpVariables];
  printCLPCore["D: cLPc, Constraining Edge Equations\n", convert2Print[edgeEQ]];

  printCLPCore["D: cLPc, Substitutions0\n", convert2Print[subRuleList]];

  (* Substitute scalar functions for the _ 3 variables remaining true to the edge constraints *)
  If[Count[lpVariables[[vConst]], 0] < 3,
    (* we have an existing constraints, begin substitution w/ referenced variables *)
    currVar = First[Flatten[Cases[lpVariables[[vConst]], Except[0]]]];
    currVarPos = First[Flatten[Position[lpVariables[[vConst]], currVar]]];
    lpVariables[[ sRule, var2Index[currVar] ]]= {currVar[0,0,1] -> f[1]/g[1]};
    subRuleList = {currVar[0,0,1] -> f[1]/g[1]};
    printCLPCore["D: cLPc, Substitutions1\n", convert2Print[subRuleList]];

    lpVariables[[ vSub,var2Index[currVar] ]] = 1;
    subRuleList = Union[subRuleList, Factor[edgeEQ /. subRuleList]];
    printCLPCore["D: cLPc, Substitutions2\n", convert2Print[subRuleList]];

    lpVariables[[ sRule, currVarPos ]]= Factor[edgeEQ /. lpVariables[[ sRule, var2Index[currVar] ]] ];
    lpVariables[[ vSub,currVarPos ]] = -1;
    currVar = lpVariables[[ vName,First[Flatten[Position[lpVariables[[vSub]], 0]]] ]];
    lpVariables[[ sRule, var2Index[currVar] ]]= {currVar[0,0,1] -> f[2]/g[2]};
    subRuleList = Union[subRuleList, {currVar[0,0,1] -> f[2]/g[2]} ];
    lpVariables[[ vSub,First[Flatten[Position[lpVariables[[vSub]], 0]]] ]] = 2;
    printCLPCore["D: cLPc, Substitutions3\n", convert2Print[subRuleList]];

    denVarCount = 2;
    subVarCount = 2;
  , (* no edge constraints, substitute worst case *)
    i=1;
    MapThread[
      If[#3,
        currVar = #2;
        lpVariables[[ sRule, var2Index[currVar] ]]= {currVar[0,0,1] -> f[i]/g[i]};
        subRuleList = Union[subRuleList, {currVar[0,0,1] ->f[i]/g[i]}];
        lpVariables[[vSub, #1]] = i++;
      ];
    &, lpVariables];

    denVarCount = i-1;
    subVarCount = i-1;
  ];
  printCLPCore["D: cLPc, Final substitutions:\n", convert2Print[subRuleList]];
  variableInfo[printCLPCore, lpVariables];

  (* With the substitutions calculated, form x13, y13 and z13 for subsequent computations     *)
  MapThread[
    If[#3,
      currVar = #2;
      tmpVar = {Factor[Flatten[(currVar[1,0,1] /. solList) /. subRuleList]]};
      printCLPCore["D: cLPc, Saving subbed variable,", currVar, " = ", convert2Print[tmpVar]];
      lpVariables[[ subs, #1 ]] = Factor[Flatten[(currVar[1,0,1] /. solList) /. subRuleList]];
    ];
  &, lpVariables];
  variableInfo[printCLPCore, lpVariables];


  laxPairFound = True;
  While[compLP,
    compLP = False;
    reduceByOne = False;
    (* Substitute into 13-sol and check for linearity w/ the subbed variables     *)
    (* If not linear, reduce denominator variables by one and repeat until linear *)
    MapThread[
      If[#3 && #4,
        currVar = #2;
        currSub13 = Factor[lpVariables[[subs,#1]] /. subFuncRules];
        printCLPCore["D: cLPc, Subbed with f,g:\n", currVar, ": ", convert2Print[currSub13]];
        muNSub13 = Numerator[currSub13] /. lpLinearRules /. subFuncRules;
        muDSub13 = Denominator[currSub13] /. lpLinearRules /. subFuncRules;
        printCLPCore["D: cLPc, Subbed with f,g scaled:\n", currVar, ": ", 
                   convert2Print[muNSub13], " over ", convert2Print[muDSub13]];
        If[Exponent[Expand[muNSub13],mu] > 1 || Exponent[Expand[muDSub13],mu] > 1,
          (* The equation is not linear in the subbed parameters *)
          reduceByOne = True;
          printCLPCore["D: cLPc, Not linear in f and g "];
        ,
          printCLPCore["D: cLPc, Linear in f and g "];
        ];
      ];
    &, lpVariables]; 

    printCLPCore["D: cLPc, reduceByOne: ", reduceByOne];
    If[reduceByOne,
      (* remove the freedom of a denominator and repeat the check *)
      printCLPCore["D: cLPc, Reducing the number of denominators referenced by 1"];
      If[denVarCount < 2,
        (* no more denominators to remove, so we can only conclude no lax pair possible *)
        printCLPCore["D: cLPc, No additional denominators available, no lax pair."];
        laxPairFound = False;

        Throw[$Failed, failTag[lpLnSub,compCoreLaxPair]]
      ,
        subFuncRules = Complement[subFuncRules, {g[denVarCount]->g[denVarCount]}];
        subFuncRules = Union[subFuncRules, {g[denVarCount]->g[denVarCount-1]}];
        printCLPCore["D: cLPc, substitution rules:\n", convert2Print[subFuncRules]];
        denVarCount--;
        compLP = True;
      ];
    ];
    printCLPCore["D: cLPc, compLP: ", compLP];
  ];
  printCLPCore["D: cLPc, Variables subbed = ", subVarCount, 
    " and denominators needed = ", denVarCount ];

  mainPrint["******************** Resulting substitutions **********************"];
  mainPrint[convert2Print[Flatten[ lpVariables[[sRule]] ] /. subFuncRules]];

  variableInfo[printCLPCore, lpVariables];


  i=0;
  MapThread[
    rule = #7 /. subFuncRules;
    printCLPCore["D: cLPc, denominator: ", convert2Print[rule] ];
    If[Length[rule] != 0,
      dd = Denominator[Last[First[rule]]];
      i++;
      MapIndexed[
        (* Find all matching denominators in our sub. rules and use to determine any nec. scaling factor *)
        If[Length[#1]>0,
          If[Denominator[Last[First[#1 /. subFuncRules]]]==dd,
            If[lpVariables[[ cmnDen, First[#2] ]]==0, 
              lpVariables[[ cmnDen, First[#2] ]] = i];
          ];
        ];
      &, lpVariables[[ sRule ]] ];
      variableInfo[printCLPCore, lpVariables];
    ];
  &, lpVariables];

  printCLPCore["D: cLPc, Lax Pair substitions found?: ", laxPairFound ];

  (* Now that we finally have the proper sub rules, use them to derive the lax pair *)
  printCLPCore["D: cLPc, beginning computation of Lax pair. "];
  matrixL = {};
  Map[
    printCLPCore["D: cLPc, sub order: ", #1];  
    j = Flatten[Position[lpVariables[[vSub]], #1]] /. List->Sequence;
    If[j>0,
      printCLPCore["D: cLPc, extracting information for ", 
                     convert2Print[ Transpose[lpVariables][[j]] ]];  
      matrixRow={};
      If[lpVariables[[vUsed,j]] && lpVariables[[vLinear,j]],
        currVar = lpVariables[[vName,j]];
        currSub13 = Factor[lpVariables[[ subs,j ]] /. subFuncRules];
        num = Numerator[currSub13];
        printCLPCore["D: cLPc, Pick: ", #1, " gives: ", 
           convert2Print[Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,j]] ]]];

        denGCD = PolynomialGCD[Map[Denominator,
                  Simplify[Flatten[ Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,j]]] ]
                            /. subFuncRules] ] /. List -> Sequence];
        printCLPCore["D: cLPc, Denominator GCD of sub vars: ", convert2Print[denGCD]];
        dscale = Simplify[Denominator[currSub13]/denGCD];
        printCLPCore[ "D: cLPc, Current Variable: ", convert2Print[currVar[1,0,1]], 
            " = ", convert2Print[currSub13] ];
        printCLPCore[ "D: cLPc, with numerator: ", convert2Print[num] ];
        printCLPCore[ "D: cLPc, and scaling factor: ", convert2Print[dscale] ];

        For[i=1, i <= subVarCount, i++, 
          printCLPCore["D: cLPc, Coefficient1 of ", convert2Print[f[i]], ": ", 
                           convert2Print[Coefficient[num, f[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[num/dscale, f[i]]] ];
        ];
        For[i=1, i <= denVarCount, i++, 
          printCLPCore["D: cLPc, Coefficient2 of ", convert2Print[g[i]], ": ", 
                           convert2Print[Coefficient[num, g[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[num/dscale, g[i]]] ];
        ];
        printCLPCore["D: cLPc, row:\n", convert2Print[matrixRow] ];
        matrixL = Append[matrixL, matrixRow];
      ];
    ];
  &, {1,2,3}];

  MapThread[
    matrixRow = {};
    If[#3 && #4,
      If[#6 > 0 && #6 <= denVarCount, 
        currVar = #2;
        currSub13 = Factor[lpVariables[[subs,#1]] /. subFuncRules];
        den = Denominator[currSub13];
        printCLPCore["D: cLPc, Pick: ", #1, "gives: ", 
           convert2Print[Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,#1]] ]]];
        denGCD = PolynomialGCD[Map[Denominator,
                  Simplify[Flatten[ Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,#1]]] ]
                            /. subFuncRules] ] /. List -> Sequence];
        dscale = Simplify[Denominator[currSub13]/denGCD];
        printCLPCore[ "D: cLPc, Current Variable: ", convert2Print[currVar[1,0,1]], 
            " = ", convert2Print[currSub13] ];
        printCLPCore[ "D: cLPc, with denominator: ", convert2Print[den] ];
        printCLPCore[ "D: cLPc, and scaling factor: ", convert2Print[dscale] ];
        For[i=1, i <= subVarCount, i++, 
          printCLPCore["D: cLPc, Coefficient of ", convert2Print[f[i]], ": ", 
                          convert2Print[Coefficient[den, f[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[den/dscale, f[i]]] ];
        ];
        For[i=1, i <= denVarCount, i++, 
          printCLPCore["D: cLPc, Coefficient of ", convert2Print[g[i]], ": ", 
                          convert2Print[Coefficient[den, g[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[den/dscale, g[i]]] ];
        ];
        printCLPCore["D: cLPc, row:\n", convert2Print[matrixRow] ];
        matrixL = Append[matrixL, matrixRow];
      ];
    ];
  &, lpVariables]; 

  (* define the corresponding matrix of scalar functions  *)
  sDen = Table[t[i],{i,1,denVarCount}];
  sNum = Table[t[i],{i,1,denVarCount}];
  printCLPCore["D: cLPc, scalars (den, num):\n", sDen, sNum];
  printCLPCore["D: cLPc, counts:\n", subVarCount, denVarCount];
  If[subVarCount > denVarCount, 
    sNum = PadRight[sNum, subVarCount, t[denVarCount]];
  ];
  scalars = Join[sNum, sDen];
  scalarMatL = DiagonalMatrix[scalars];
  printCLPCore["D: cLPc, scalar matrix:\n", MatrixForm[convert2Print[scalarMatL]] ];


  matPhi = Join[Table[f[i],{i,1,subVarCount}], Table[g[i],{i,1,denVarCount}]];
  printCLPCore["D: cLPc, phi:\n", matPhi];
  printCLPCore[MatrixForm[matPhi]];
  matrixPhi = matPhi;

  printCLPCore["D: cLPc, matrix L:\n", convert2Print[matrixL] ];
  printCLPCore[MatrixForm[convert2Print[matrixL]]];
  coreLPL = matrixL;
  scalarL = scalarMatL;

  matrixM = matrixL /. l2mRules;
  scalarMatM = scalarMatL /. l2mRules;
  printCLPCore["D: cLPc, matrix M:\n", convert2Print[matrixM] ];
  printCLPCore[MatrixForm[convert2Print[matrixM]]];
  coreLPM = matrixM;
  scalarM = scalarMatM;

  printCLPCore["dbg: cLPc, exit"];
  Clear[printCLPCore];
  Return[];
];

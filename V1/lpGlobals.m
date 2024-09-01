(* ::Package:: *)

sysUnkG = {};        (* unknowns used in the system  *)
sysVarsG = {};       (* unknowns classified as variables (ie. remove parameters)  *)
solutionListG = {};  (* list of solutions for vars. Found in consistency check    *)
edgeConstraintsG = {}; (* list of possible constraints due to edge equations      *)
oNone = 0; oCAC=1; oCLP=2; oVLP=3;
operationG = oNone;      (* Flag indicating current processing: 1-CAC, 2-LP, 3-VLP    *)
coreLPL = {};
coreLPM = {};
matrixPhi = {};

(* The following 3 lists of equations are necessary for both Consistency checks   *)
(* and for Lax Pair Calculations                                                  *)
frontFaceEQG = {};   (* Basic user=provided equation - may be augmented if nec.   *)
leftFaceEQG = {};    (* Front Face adjusted to Left Face *)
groundFaceEQG = {};  (* Front Face adjusted to Bottom Face *)

(* The following track specific characteristic equations within the given DDE *)
singleEdgeEQG = {};
singleEdgeSolG = {};
doubleEdgeEQG = {};

(* Front Face solutions subdivided into face, double-edge and single edge equations *)
frontFaceSolutions = {};

(* The following 3 lists are only necessary for consistency checks *)
rightFaceEQG = {};
groundFaceEQG = {};
topFaceEQG = {};

(* Currently, the only supported variables (internally) *)
supportedVars = {x,y,z};

(* Variable structure used to store computational information *)
lpVariables = {{1,2,3}, supportedVars, {False, False, False}, {False, False, False}, {0,0,0}, 
               {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}};
(* and the corresponding defines to collate that information  *)
vIdx=1; vName=2; vUsed=3; vLinear=4; vConst=5; vSub=6; sRule=7; subs=8; cmnDen=9;

(* global return value used throughout *)
retG = False;

(* Specially modified denominator function -- returns 0 for denominator of 0, instead of 1 *)
myDenominator[x_] := If[IntegerQ[Numerator[x]] && Numerator[x]==0, 0, Denominator[x]];

(* Mathematica can not handle rational polynomials exactly right w/ GCD.  This does as I expect *)
rationalPolyGCD[x_]:= Simplify[PolynomialGCD[Union[Map[Numerator[Factor[#]]&,Flatten[x]]] /. List -> Sequence]/
                               PolynomialGCD[Union[Map[myDenominator[Factor[#]]&,Flatten[x]]] /. List -> Sequence] ]

(* Given a list of solutions, subdivide the list into 2-sub variable solutions, 1-sub variables solutions and *)
(* 0-sub variable solutions returning the results as a list of these lists *)
divideSol[x_]:= If[ArrayQ[x],
                   List[FilterRules[x, {_[1,1,0],_[1,0,1], _[0,1,1]}],
                        FilterRules[x, {_[1,0,0],_[0,0,1], _[0,1,0]}],
                        FilterRules[x, {_[0,0,0]}]], 
                   {}];

(* var2Index:  Translates between x,y,z and their corresponding index value         *)
var2Index[a_] := If[Length[Flatten[Position[supportedVars,a]]]>0,
                      First[Flatten[Position[supportedVars,a]]],0];

(* List of possible error messages that may occur during processing  *)
(* Error Handling *)
Attributes[catchInternalError] = {HoldAll};
catchInternalError[code_, f_, failTag_] :=
  Catch[code, _failTag,
    Function[{value, tag},
      f::opFail = "The calculation failed due to: `1` . `2`";
      If[globalVerbose,
        Message[f::opFail, Style[First@tag, Red], Style[Last@tag, Blue]],
        Message[f::opFail, Style[First@tag, Red], " "],
        Message[f::opFail, Style[First@tag, Red], " "]
      ];
      f::opFail=.;
      value]];
lpEno12 = "The specified DDE does not reference a double-subscripted variable";
lpNoLin = "Critical variables do not appear linearly in the given equation(s)";
lpNoLxP = "No Lax Pair could be calculated";
lpLnSub = "Substitutions could not be found that would result in a separable expression";
lpEfail = "Error in computations";

(* and the corresponding error reporting function  *)
failTag;


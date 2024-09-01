(* The package was LAST UPDATED by Hereman on December 24, 2017 at 00:20 (12:20am) in Boulder *)

(* New corrections added by Hereman on November 28, 2017 at 22:40: see corrections NOV 28, 2017 and ASK TB *)

(* The package was PREVIOUSLY UPDATED by Hereman on July 3, 2017 at 00:25 (00:25am) in Boulder *)
(* Last corrections added by Hereman on July 31, 2017 at 17:00 (5:00pm) in Lokeren: see corrrection and ASK TB *)

(* Package for the computation of a Lax pair of a single partial difference equation *)
(* or a system of partial difference equations. *)
(* Also allows the user to test a given Lax pair.  *)

(* ::Package:: *)

BeginPackage["LPSolvePkg`"]
createOutputNotebook;
dbgPrint;
vrbPrint;

catchInternalError;
variableInfo;
getVariableInfo;

setPrintOptsDebug;
getPrintOptsDebug;
getPrintOptsVerbose;
showPrintOptions;
convert2Print;

mergeSolLists;
solveInitDDE;

augmentDDE;
removeDoubleRefs;
findEdgeEquations;
ddeSystemToSolutions;

compCoreLaxPair;
computeLaxPairs;
compLPDriver;

(* Consistency Around the Cube functionality  *)
solveFaceEqu;
verifyCAC;
verifyVar123;

(*  Verify Lax Pairs functionality *)
verifyLaxPairs;
verifyLPDriver;

(*  Dialog/Interface related functionality *)
LaxPairSolverUI;
initLaxPairInfo;
uiModuleFlagsCBox;
uiProcessFlagsCBox;
uiProcessBtn;
uiStatisticsBtn;

(* Global variables *)
$allowedVars::usage="List of all unknowns that the software will assume to be
 variables of the system.";
$allowedPars::usage="List of all unknowns that will be assumed to spectral 
 parameters.";
$globalVerbose::usage="Verbose flag.";

systemUnks;
systemPars;
systemVars;
systemUDefs;
dblSubSystemVars;

(***************************** Global Variables ******************************** *)
$frontFaceEQ::usage="Unshifted, original DDE.";
$leftFaceEQ::usage="Shifted (1->3, 2->2) version of current DDE.";
$groundFaceEQ::uusage="Shifted (1->1, 2->3) version of current DDE.";
$rightFaceEQ::usage="Shifted (2->12) version of current DDE.";
$topFaceEQ::usage="Shifted (1->12) version of current DDE.";
$backFaceEQ::usage="Shifted (1->13) version of current DDE.";
$latticeEQ::usage="Union of all face equations.";

$latFrnt2GrndRules::usage="Shift rules for Front to Bottom.";
$latFrnt2LeftRules::usage="Shift rules for Front to Left.";
$latGrnd2TopRules::usage="Shift rules for Bottom to Top.";
$latLeft2RghtRules::usage="Shift rules for Left to Right.";
$latFrnt2BackRules::usage="Shift rules for Front to Back.";

myDenominator::usage="myDenominator[x] returns the denominator of x.  If x
 is 0, this returns 0 (as opposed to Mathematica's Denominator which returns 1.";
rationalPolyGCD::usage="rationalPolyGCD[poly] returns the GCD of the numerator and
 denominator of the specified rational polynomial.";


(* The following variables are used as flags to determine processing to be done *)
$checkCAC::usage="Check consistency around the cube."; 
$computeLP::usage="Compute a Lax pair for the specified DDE.";
$verifyLP::usage="Verify the user provided Lax pair against the accompanying DDE.";

(* Error Codes *)
$lpFileNotFound = "Specified DDE file not found.";
$lpErrNo12 = "The specified DDE does not reference a double-subscripted variable";
$lpNotLinr = "Critical variables do not appear linearly in the given equation(s)";
$lpNoLnSub = "Substitutions could not be found that would result in a separable expression";
lpNoLxP = "No Lax pair could be calculated";
lpEfail = "Error in computations";

(* and the corresponding error reporting function  *)
failTag;



(* var2Index:  Translates between x,y,z and their corresponding index value         *)
var2Index[a_] := If[Length[Flatten[Position[$supportedVars,a]]]>0,
                      First[Flatten[Position[$supportedVars,a]]],0];

(* The following variables are used within the pre-defined .m files containing *)
(* pre-defined scalar/system DDEs                                              *)
Clear[ddeEquationListINPUT, nameINPUT, laxPairMatrixL, laxPairMatrixM];
Clear[explicitScalars, sFunc, tFunc, userShifts, paramEquivalences];
(*
ddeEquationListINPUT = "";
nameINPUT = "";
laxPairMatrixL = "";
laxPairMatrixM = "";
explicitScalars = False;
sFunc = "";
tFunc = "";
*)
(************************************************************************** *)

$ddeEquationList::usage="The current DDE equation(s).  This may contain a 
  single equation (scalar) or list of equations (system).";
$currentLPDirectory::usage="The current working directory as defined by where
  the subdirectory of pre-defined DDEs is located.";
$lpVariables::usage="Structure containing various operationally derived 
  information.";


Clear[x, x1, x2, x3, x12, x13, x23, x123, 
      y, y1, y2, y3, y12, y13, y23, y123,
      z, z1, z2, z3, z12, z13, z23, z123, p, q, k];
(* Variables used in the _3 \[Rule] _13 substitutions *)
Clear[f, g, h, F, G, H, s, s1, s2, t, t1, t2, T, T1, T2, S, S1, S2, mu, m, n];
(* f; g; mu; h; F; G; H; s; s1; s2; t; t1; t2; *) 

(* Initialization modules *)
lpSysStat=1001; lpProcDrv=1002; rmDRef=1003; 
(* Lax Pair Calculation modules *)
sys2sol=2001; augDDE=2002; findSub=2003; cmpLP=2004; ccoreLP=2005; 
cLPDriver=2006; calcST=2007;
(* Verification modules  *)
vLPDriver=3001; vLaxPair=3002;
(* Consistency Around the Cube modules *)
verCAC=4001; solFace=4002; mrgLst=4003; edgeEQs=4004; solInit=4005; ver123=4006;
(* User Interface modules *)
uiFunc=5001;

(* Process Level Flags *)
lpInit=6001; lpUI=6002; lpCAC=6003; lpComp=6004; lpVerify=6005;

$supportedVars={x,y,z};
(* Variable structure used to store computational information *)
lpVariables = {{1,2,3}, $supportedVars, {False, False, False}, {False, False, False}, {0,0,0}, 
               {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}};
(* and the corresponding defines to collate that information  *)
vIdx=1; vName=2; vUsed=3; vLinear=4; vConst=5; vSub=6; sRule=7; subs=8; cmnDen=9;

$edgeConstraints={};
$solutionList = {};  (* list of solutions for vars. Found in consistency check    *)

Begin["`Private`"]

$frontFaceEQ={};
$leftFaceEQ={};
$groundFaceEQ={};
$rightFaceEQ={};
$topFaceEQ={}
$backFaceEQ={};

$latFrnt2GrndRules = 
  {x[0,0,0] -> x[0,0,0], y[0,0,0] -> y[0,0,0], z[0,0,0] -> z[0,0,0], 
   x[1,0,0] -> x[1,0,0], y[1,0,0] -> y[1,0,0], z[1,0,0] -> z[1,0,0], 
   x[0,1,0] -> x[0,0,1], y[0,1,0] -> y[0,0,1], z[0,1,0] -> z[0,0,1], 
   x[1,1,0] -> x[1,0,1], y[1,1,0] -> y[1,0,1], z[1,1,0] -> z[1,0,1], 
   p -> p, q -> k};
$latFrnt2LeftRules = 
  {x[0,0,0] -> x[0,0,0], y[0,0,0] -> y[0,0,0], z[0,0,0] -> z[0,0,0], 
   x[1,0,0] -> x[0,0,1], y[1,0,0] -> y[0,0,1], z[1,0,0] -> z[0,0,1], 
   x[0,1,0] -> x[0,1,0], y[0,1,0] -> y[0,1,0], z[0,1,0] -> z[0,1,0], 
   x[1,1,0] -> x[0,1,1], y[1,1,0] -> y[0,1,1], z[1,1,0] -> z[0,1,1], 
   p -> k, q -> q};
$latGrnd2TopRules = 
  {x[0,0,0] -> x[0,1,0], y[0,0,0] -> y[0,1,0], z[0,0,0] -> z[0,1,0], 
   x[1,0,0] -> x[1,1,0], y[1,0,0] -> y[1,1,0], z[1,0,0] -> z[1,1,0], 
   x[0,0,1] -> x[0,1,1], y[0,0,1] -> y[0,1,1], z[0,0,1] -> z[0,1,1], 
   x[1,0,1] -> x[1,1,1], y[1,0,1] -> y[1,1,1], z[1,0,1] -> z[1,1,1], 
   p -> p, k -> k};
$latLeft2RghtRules = 
  {x[0,0,0] -> x[1,0,0], y[0,0,0] -> y[1,0,0], z[0,0,0] -> z[1,0,0], 
   x[0,1,0] -> x[1,1,0], y[0,1,0] -> y[1,1,0], z[0,1,0] -> z[1,1,0], 
   x[0,0,1] -> x[1,0,1], y[0,0,1] -> y[1,0,1], z[0,0,1] -> z[1,0,1], 
   x[0,1,1] -> x[1,1,1], y[0,1,1] -> y[1,1,1], z[0,1,1] -> z[1,1,1], 
   q -> q, k -> k};
$latFrnt2BackRules = 
  {x[0,0,0] -> x[0,0,1], y[0,0,0] -> y[0,0,1], z[0,0,0] -> z[0,0,1], 
   x[1,0,0] -> x[1,0,1], y[1,0,0] -> y[1,0,1], z[1,0,0] -> z[1,0,1], 
   x[0,1,0] -> x[0,1,1], y[0,1,0] -> y[0,1,1], z[0,1,0] -> z[0,1,1], 
   x[1,1,0] -> x[1,1,1], y[1,1,0] -> y[1,1,1], z[1,1,0] -> z[1,1,1], 
   q -> q, p -> p};

(* The following variables are used as flags to determine processing to be done *)
$checkCAC=101;
$computeLP=102;
$verifyLP=103;

(* Global processing variables - current DDE system/scalar                    *)
$ddeEquationList = Null;
(* Global processing variables - current working directory                    *)
$currentLPDirectory = "";

outputNotebook = Null;
outputNotebookTitle = "Lax Pair Output";

(* The following 2 variables are also privately defined in LPcac.m  *)
$allowedVars = {x[0,0,0],y[0,0,0],z[0,0,0],
               x[1,0,0],y[1,0,0],z[1,0,0],
               x[0,1,0],y[0,1,0],z[0,1,0],
               x[0,0,1],y[0,0,1],z[0,0,1],
               x[1,1,0],y[1,1,0],z[1,1,0],
               x[1,0,1],y[1,0,1],z[1,0,1],
               x[0,1,1],y[0,1,1],z[0,1,1],
               x[1,1,1],y[1,1,1],z[1,1,1]};
allowedPars = {p, q, k};

(************************************************************************)
(* The following rules are used to convert the variables to/from an     *)
(*  internal representation to/from a print representation              *) 
(************************************************************************)

prt2intRules = {x -> x[0,0,0], y -> y[0,0,0], z -> z[0,0,0],
               x1 -> x[1,0,0], y1 -> y[1,0,0], z1 -> z[1,0,0],
               x2 -> x[0,1,0], y2 -> y[0,1,0], z2 -> z[0,1,0],
               x3 -> x[0,0,1], y3 -> y[0,0,1], z3 -> z[0,0,1],
               x12 -> x[1,1,0], y12 -> y[1,1,0], z12 -> z[1,1,0],
               x13 -> x[1,0,1], y13 -> y[1,0,1], z13 -> z[1,0,1],
               x23 -> x[0,1,1], y23 -> y[0,1,1], z23 -> z[0,1,1]};
int2prtRules = {x[0,0,0] -> x, y[0,0,0] -> y, z[0,0,0] -> z,
                x[1,0,0] -> x1, y[1,0,0] -> y1, z[1,0,0] -> z1,
                x[0,1,0] -> x2, y[0,1,0] -> y2, z[0,1,0] -> z2,
                x[0,0,1] -> x3, y[0,0,1] -> y3, z[0,0,1] -> z3,
                x[1,1,0] -> x12, y[1,1,0] -> y12, z[1,1,0] -> z12,
                x[1,0,1] -> x13, y[1,0,1] -> y13, z[1,0,1] -> z13,
                x[0,1,1] -> x23, y[0,1,1] -> y23, z[0,1,1] -> z23,
                x[1,1,1] -> x123, y[1,1,1] -> y123, z[1,1,1] -> z123,
                t[1] -> t, t[2] -> T, s[1] -> s, s[2] -> S,
                t1[1] -> t1, t1[2] -> T1, s2[1] -> s2, s2[2] -> S2,
                t2[1] -> t2, t2[2] -> T2, s1[1] -> s1, s1[2] -> S1,
                f[1]->f, f[2] -> g, f[3]-> h, g[1] -> F, g[2] -> G, 
                g[3] -> H};


(* following variables used to shift a system from one face to another *) 
(* (in the CAC vernacular)                                             *)
(* These also exist in the LPComplp.m package                          *)
lat1ShiftRules = 
  {x[0,0,0] -> x[1,0,0], y[0,0,0] -> y[1,0,0], z[0,0,0] -> z[1,0,0], 
   x[0,1,0] -> x[1,1,0], y[0,1,0] -> y[1,1,0], z[0,1,0] -> z[1,1,0], 
   x[0,0,1] -> x[1,0,1], y[0,0,1] -> y[1,0,1], z[0,0,1] -> z[1,0,1], 
   x[0,1,1] -> x[1,1,1], y[0,1,1] -> y[1,1,1], z[0,1,1] -> z[1,1,1],
   s[1] -> s1[1], t[1] -> t1[1], s[2] -> s1[2], t[2]->t1[2], s[3]->s1[3], t[3]->t1[3]};
lat2ShiftRules = 
  {x[0,0,0] -> x[0,1,0], y[0,0,0] -> y[0,1,0], z[0,0,0] -> z[0,1,0], 
   x[1,0,0] -> x[1,1,0], y[1,0,0] -> y[1,1,0], z[1,0,0] -> z[1,1,0], 
   x[0,0,1] -> x[0,1,1], y[0,0,1] -> y[0,1,1], z[0,0,1] -> z[0,1,1], 
   x[1,0,1] -> x[1,1,1], y[1,0,1] -> y[1,1,1], z[1,0,1] -> z[1,1,1],
   s[1] -> s2[1], t[1] -> t2[1], s[2] -> s2[2], t[2]->t2[2], s[3]->s2[3], t[3]->t2[3]};
lat3ShiftRules = 
  {x[0,0,0] -> x[0,0,1], y[0,0,0] -> y[0,0,1], z[0,0,0] -> z[0,0,1],
   x[1,0,0] -> x[1,0,1], y[1,0,0] -> y[1,0,1], z[1,0,0] -> z[1,0,1], 
   x[0,1,0] -> x[0,1,1], y[0,1,0] -> y[0,1,1], z[0,1,0] -> z[0,1,1], 
   x[1,1,0] -> x[1,1,1], y[1,1,0] -> y[1,1,1], z[1,1,0] -> z[1,1,1]};


latFrnt2GrndRules = 
  {x[0,0,0] -> x[0,0,0], y[0,0,0] -> y[0,0,0], z[0,0,0] -> z[0,0,0], 
   x[1,0,0] -> x[1,0,0], y[1,0,0] -> y[1,0,0], z[1,0,0] -> z[1,0,0], 
   x[0,1,0] -> x[0,0,1], y[0,1,0] -> y[0,0,1], z[0,1,0] -> z[0,0,1], 
   x[1,1,0] -> x[1,0,1], y[1,1,0] -> y[1,0,1], z[1,1,0] -> z[1,0,1], 
   p -> p, q -> k};

latFrnt2LeftRules = 
  {x[0,0,0] -> x[0,0,0], y[0,0,0] -> y[0,0,0], z[0,0,0] -> z[0,0,0], 
   x[1,0,0] -> x[0,0,1], y[1,0,0] -> y[0,0,1], z[1,0,0] -> z[0,0,1], 
   x[0,1,0] -> x[0,1,0], y[0,1,0] -> y[0,1,0], z[0,1,0] -> z[0,1,0], 
   x[1,1,0] -> x[0,1,1], y[1,1,0] -> y[0,1,1], z[1,1,0] -> z[0,1,1], 
   p -> k, q -> q};

printOptions = {
  lpSysStat->{tag->lpInit, debug->False, verbose->True},
  lpProcDrv->{tag->lpInit, debug->False, verbose->True},
  rmDRef->{tag->lpInit, debug->False, verbose->True},
  sys2sol->{tag->lpComp, debug->False, verbose->True},
  solInit->{tag->lpComp, debug->False, verbose->True},
  findSub->{tag->lpComp, debug->False, verbose->True},
  augDDE->{tag->lpComp, debug->False, verbose->True},
  cmpLP->{tag->lpComp, debug->False, verbose->True},
  calcST->{tag->lpComp, debug->False, verbose->True},
  ccoreLP->{tag->lpComp, debug->False, verbose->True},
  cLPDriver->{tag->lpComp, debug->False, verbose->True},
  ver123->{tag->lpCAC, debug->False, verbose->True},
  mrgLst->{tag->lpCAC, debug->False, verbose->True},
  solFace->{tag->lpCAC, debug->False, verbose->True},
  verCAC->{tag->lpCAC, debug->False, verbose->True},
  edgeEQs->{tag->lpCAC, debug->False, verbose->True},
  vLaxPair->{tag->lpVerify, debug->False, verbose->True},
  vLPDriver->{tag->lpVerify, debug->False, verbose->True},
  uiFunc->{tag->lpUI, debug->False, verbose->True}
};
functionTags = {
  lpInit-> {lpSysStat, lpProcDrv, rmDRef, edgeEQs}, 
  lpComp->{sys2sol, solInit, augDDE, rmDRef, findSub, cmpLP, ccoreLP, mrgLst, 
           cLPDriver, vLaxPair, calcST,lpProcDrv}, 
  lpCAC->{lpProcDrv, verCAC, solInit, edgeEQs, sys2sol, findSub, solFace, mrgLst, ver123},
  lpVerify-> {lpProcDrv, vLPDriver, vLaxPair},
  lpUI-> {uiFunc}
};


(* Variables involved in the calculation of Lax pair and referenced in both *)
(* compCoreLaxPair (where they are derived) and computeLaxPairs (where they *)
(* are used)                                                                *)
coreLPL = {};  scalarL = {};
coreLPM = {};  scalarM = {};
matrixPhi = {};

(* Actual variables represented in the specified DDE *)
systemVars;

moduleLevelFlagsTxt = {
  "System Statistics", 
  "Augment DDE",
  "Remove Double Refs", 
  "System to Solutions",
  "Solve Initial DDE", 
  "Find Edge Equations", 
  "Compute Lax Pairs",
  "Compute Core LP",
  "Verify Lax Pairs",
  "Verify CAC",
  "Calculate Scalars"};
moduleLevelFlagsVal = {
  lpSysStat, 
  augDDE, 
  rmDRef, 
  sys2sol, 
  solInit, 
  edgeEQs, 
  cmpLP,
  ccoreLP,
  vLaxPair, 
  verCAC,
  calcST};
processLevelFlagsTxt = {
  "Initialization Functions", 
  "User Interface", 
  "CAC Computations",
  "Lax Pair Computations", 
  "Verifications Computations"};
processLevelFlagsVal = {lpInit, lpUI, lpCAC, lpComp, lpVerify};
(* CHANGED HEREMAN JUL 2, 2017 added a tag to the data file *)
scalarDDEs={
      "Select an Equation", NULL, 
      "Discrete pKdV Equation (data file: lp_KdV.m)", "sampleLattices/lp_KdV.m",
      "Discrete Modified KdV Equation (data file: lp_mKdV.m)", "sampleLattices/lp_mKdV.m",
      "H1 Equation (data file: lp_H1.m)", "sampleLattices/lp_H1.m",
      "H2 Equation (data file: lp_H2.m)", "sampleLattices/lp_H2.m",
      "H3 Equation (delta = 0) (data file: lp_H3d0.m)", "sampleLattices/lp_H3d0.m", 
      "H3 Equation (delta not 0)(data file: lp_H3.m)", "sampleLattices/lp_H3.m",
      "Q1 Equation (delta = 0) (data file: lp_Q1d0.m)", "sampleLattices/lp_Q1d0.m", 
      "Q1 Equation (delta not 0) (data file: lp_Q1.m)", "sampleLattices/lp_Q1.m",
      "Q2 Equation (data file: lp_Q2.m)", "sampleLattices/lp_Q2.m", 
      "Q3 Equation (delta = 0) (data file: lp_Q3d0.m)", "sampleLattices/lp_Q3d0.m",
      "Q3 Equation (delta not 0) (data file: lp_Q3.m)", "sampleLattices/lp_Q3.m", 
      "A1 Equation (data file: lp_A1.m)", "sampleLattices/lp_A1.m",
      "A2 Equation (data file: lp_A2.m)", "sampleLattices/lp_A2.m", 
      "sine-Gordon Equation (data file: lp_sineG.m)",  "sampleLattices/lp_sineG.m",
      "Discrete Lotka-Volterra Equation (data file: lp_dLotVol.m)",  "sampleLattices/lp_dLotVol.m", 
      "Discrete Potential Lotka-Volterra Equation (data file: lp_dPLotVol.m)", "sampleLattices/lp_dPLotVol.m",
      "Hydon - Viallet Equation (data file: lp_HydVial.m)", "sampleLattices/lp_HydVial.m", 
      "Potential Hydon - Viallet Equation (data file: lp_PHydVial.m)", "sampleLattices/lp_PHydVial.m"
      };
systemDDEs={
      "Select a System", NULL,
      "Discrete Boussinesq System (data file: lp_Bouss.m)", "sampleLattices/lp_Bouss.m", 
      "Lattice Schwarzian Boussinesq System (data file: lp_lsBSQ.m)", "sampleLattices/lp_lsBSQ.m",
      "Toda/Modified Boussinesq System (data file: lp_todambsq.m)",  "sampleLattices/lp_todambsq.m", 
      "System of pKdV Equations (data file: lp_pKdVsys.m)", "sampleLattices/lp_pKdVsys.m",
      "System of NLS Equations (data file: lp_NLS.m)",  "sampleLattices/lp_NLS.m",
      "Generalized Hietarinta A-2 (data file: lp_ZZNA2.m)", "sampleLattices/lp_ZZNA2.m",
      "Generalized Hietarinta B-2 (data file: lp_ZZNB2.m)", "sampleLattices/lp_ZZNB2.m",
      "Generalized Hietarinta C-3 (data file: lp_ZZNC3.m)", "sampleLattices/lp_ZZNC3.m",
      "Generalized Hietarinta C-4 (data file: lp_ZZNC4.m)", "sampleLattices/lp_ZZNC4.m"
};
scalarEQ=0;
systemEQ=1;
userDefEQ=2;

lpSolverOpValues = {$checkCAC, $computeLP, $verifyLP};
lpSolverOpLabels = {"Check Consistency Around the Cube", 
                    "Compute a Lax pair",
                    "Verify the User-provided Lax Pair"};

catchInternalError::usage="catchInternalError[] error handling.";
Attributes[catchInternalError] = {HoldAll};
catchInternalError[code_, f_, failTag_] :=
  Catch[code, _failTag,
    Function[{value, tag},
      f::opFail = "The calculation failed due to: `1` . `2`";
      
      vrbPrint[" ", "Failed: ", First@tag, ". (", Last@tag, ")"];
(*
      If[globalVerbose,
        Message[f::opFail, Style[First@tag, Red], Style[Last@tag, Blue]],
        Message[f::opFail, Style[First@tag, Red], " "],
        Message[f::opFail, Style[First@tag, Red], " "]
      ];
*)
      f::opFail=.;
      value]];

(***************************    Module : getVariableInfo   ********************** *)
(* getVariableInfo[ddeEquationList]                                                *)
(* Purpose:  Inspects the user provided equations and specifically identifies      *)
(*           1. variables                                                          *)
(*           2. parameters                                                         *)
(*           3. user-specified unknowns                                            *)
(* Input:   User Specified List of discrete differential functions                 *)
(******************************************************************************* *)
getVariableInfo::usage="getVariableInfo[ddEquationList] determines the variables,
 parameters, and user-specified variables used in the DDE.";
(******************************************************************************* *)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPDrivers.m                                                *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
getVariableInfo[ddeEquationList_, debug_:False] :=
  Module[{ddEQ = ddeEquationList, 
    dPrint, dblSubList = {x[1,1,0], y[1,1,0], z[1,1,0]},
    ctag="varinfo"}, 

  If[debug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  systemUnks = Variables[ddEQ];
  dPrint[ctag, "Variables: ", systemUnks];
  systemPars = Complement[systemUnks, $allowedVars];
  dPrint[ctag, "Parameters: ", systemPars];
  systemVars = Sort[Intersection[systemUnks, $allowedVars], Count[#1,1]<Count[#2,1]&];
  dPrint[ctag, "Actual Vars: ", systemVars];
  systemUDefs = Complement[systemUnks, Union[systemPars, systemVars]];
  dPrint[ctag, "User-Defines: ", systemUDefs];
  dblSubSystemVars = Intersection[systemVars, dblSubList];
  dPrint[ctag, "Dbl-sub Vars: ", dblSubSystemVars];
  
  Return[{systemUnks, systemPars, systemVars, systemUDefs, dblSubSystemVars}];
];
(***************************    Module : getVariableInfo   ********************** *)


(****************************    Module: variableInfo    ************************* *)
(* Purpose:  Used for debug messages to the output notebook.                        *)
(******************************************************************************** *)
variableInfo::usage="variableInfo[prtFunc, lpV] used for debug messages only.  Prints the 
contents of the global variable structure.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
variableInfo[debug_, lpV_] :=
Module[{lpVars = lpV,
         printMerge, solList={{},{},{}}, tmpList={}, tmpRule={}, idx=0,
         ctag=" "},

  If[debug, 
    dbgPrint["** Lax Pair Variable Info **", ctag];
    dbgPrint[ctag, "Variable Index: ", lpVars[[ vIdx ]] ];
    dbgPrint[ctag, "Variable Name: ", lpVars[[ vName ]] ];
    dbgPrint[ctag, "Variable Used: ", lpVars[[ vUsed ]] ];
    dbgPrint[ctag, "Variable Linearity: ", lpVars[[ vLinear ]] ];
    dbgPrint[ctag, "Variable Constraints: ", lpVars[[ vConst ]] ];
    dbgPrint[ctag, "Variable sub order: ", lpVars[[ vSub ]] ];
    dbgPrint[ctag, "Solutions after initial substituions: ", 
                                 convert2Print[ lpVars[[ subs ]] ] ];
    dbgPrint[ctag, "Common Denominator: ", lpVars[[ cmnDen ]] ];
  ];
  Return[];
]; 
(****************************    Module: variableInfo    ************************* *)


(**********************    Module: createOutputNotebook    *********************** *)
(* Purpose:  Used for debug messages to the output notebook.                        *)
(******************************************************************************** *)
createOutputNotebook::usage="createOutputNotebook[] creates a new notebook to 
 be used for both general & debug messages.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
createOutputNotebook[] :=
  Module[{}, 

  If[Length[Notebooks[outputNotebookTitle]] == 0,
    outputNotebook = CreateDocument[TextCell["Lax Pair Solver Output", "Section"], 
           WindowTitle->outputNotebookTitle, TaggingRules->{"NBType"-> "LPOutput"},
           PageWidth->WindowWidth];
  ];
  SelectionMove[outputNotebook, After, Notebook];
  Return[]; 
];



(*****************************    Module: dbgPrint    **************************** *)
(* Purpose:  Used for debug messages to the output notebook.                        *)
(******************************************************************************** *)
dbgPrint::usage="dbgPrint[tag, message] writes the specified message(s)
 to the output notebook, tagging it with a debug tag.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
dbgPrint[msgTag_, message__] :=
  Module[{msg=message, tag = msgTag, prefix=Null, outMsg=Null}, 

  prefix = "D>>" <> tag <> ": ";
  If [Length[Notebooks[outputNotebookTitle]] == 0, createOutputNotebook[]];

  NotebookWrite[outputNotebook, Cell[BoxData[ToBoxes[Row[{prefix, msg}]]], "Print"]];
(*  SelectionMove[outputNotebook, After, Cell];  *)
  Return;
];
(*****************************    Module: dbgPrint    **************************** *)


(*****************************    Module: vrbPrint    **************************** *)
(* Purpose:  Used for verbose messages to the output notebook.                      *)
(******************************************************************************** *)
vrbPrint::usage="vrbPrint[tag, message] writes the specified message(s) 
 to the output notebook, tagging it with a verbose tag.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
vrbPrint[msgTag_, message__] :=
  Module[{msg=message, tag=msgTag, prefix=Null, outMsg=Null}, 

  prefix = ">> ";
  If [Length[Notebooks[outputNotebookTitle]] == 0, createOutputNotebook[]];

  NotebookWrite[outputNotebook, Cell[BoxData[ToBoxes[Row[{prefix, msg}]]], "Print"]];
(*  SelectionMove[outputNotebook, After, Cell];  *)
  Return;
];
(*****************************    Module: vrbPrint    **************************** *)


(***************************  Module: setPrintOptsDebug  ************************ *)
(* Purpose:  Sets the debug print flags either by module or functionality.         *)
(******************************************************************************** *)
setPrintOptsDebug::usage="setPrintOptsDebug[ftag, cat, debugFlag]sets the Debug
 flags.  User may specify a function tag or set the options using a category tag.";
(******************************************************************************** *)
(* Output:  Null                                                                    *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
setPrintOptsDebug[ftag_, pcat_, debugFlag_, uiDebug_] := Module[{newOpts, 
  ft=ftag, pc=pcat, mods, dPrint, ctag="UI"},

  If[uiDebug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  setParam[paramList_, newRules_]:= 
      DeleteDuplicates[Join[newRules, paramList], First[#1] === First[#2] &];

  dPrint[ctag, "Set (ftag): ", ft, "Set (ptag): ", pc, "Debug: ", debugFlag];
  (* If a tag has been specified, use it, else check a category *)
  If[ Length[ft] > 0, {
    Map[
      newOpts = setParam[printOptions, 
          {#-> setParam[# /. printOptions, {debug -> debugFlag}]}];
      printOptions = newOpts;
    &, ft];
  }, (* else check for a category specified *)
    If[ Length[pc] > 0, {
      Map[
        mods = # /. functionTags;  (* grab all the modules within the process *)
        dPrint[ctag, "Modules: ``", mods];
        Map[ 
          newOpts = setParam[printOptions, 
            {#-> setParam[# /. printOptions, {debug -> debugFlag}]}];
          printOptions = newOpts;
        &, mods];
      &, pc];
    }];
  ];
  Return[];
];
(***************************  Module: setPrintOptsDebug  ************************ *)


(**************************    Module: getPrintOptsDebug   *********************** *)
(* Purpose:  Returns a 'debug' flag for the given module.  True => print.           *)
(******************************************************************************** *)
getPrintOptsDebug::usage="getPrintOptsDebug[ftag] returns the status of the 
  debug flag (True/False) to determine the amount of printed ouput.";
(******************************************************************************** *)
(* Output:  boolean:  True = Print; False = No Print                                *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
getPrintOptsDebug[ftag_] := Module[{tag = ftag, ret=False},

  If[tag != 0, {ret = debug /. (tag /. printOptions);}];
  Return[ret];
];
(**************************    Module: getPrintOptsDebug   *********************** *)


(***************************    Module: getPrintOptsVerbose   ******************** *)
(* Purpose:  Returns a 'verbose' flag for the given module.  True => print.         *)
(******************************************************************************** *)
getPrintOptsVerbose::usage="getPrintOptsVerbose[ftag] returns the status of the 
  verbose flag (True/False) to determine the amount of printed ouput.";
(******************************************************************************** *)
(* Output:  boolean:  True = Print; False = No Print                                *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
getPrintOptsVerbose[ftag_] := Module[{tag=ftag, ret=False},

  If[ tag != 0, ret = verbose /. (tag /. printOptions);];
  Return[ret];
];
(***************************    Module: getPrintOptsVerbose   ******************** *)


(***************************    Module: showPrintOptions   *********************** *)
(* Purpose:  Returns a 'verbose' flag for the given module.  True => print.         *)
(********************************************************************************* *)
showPrintOptions::usage="showPrintOptions[] dumps a debug version of the current 
 printOptions settings.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
showPrintOptions[] := Module[{modNames, procNames},

  modNames={lpSysStat->"lpSysStat", lpProcDrv->"lpProcDrv", rmDRef->"rmDRef",
    sys2sol->"sys2sol", augDDE->"augDDE", findSub->"findSub", cmpLP->"cmpLP",
    ccoreLP->"ccoreLP", cLPDriver->"cLPDriver", calcST->"calcST", 
    vLPDriver->"vLPDriver", vLaxPair->"vLaxPair", verCAC->"verCAC", 
    solFace->"solFace", mrgLst->"mrgLst", edgeEQs->"edgeEQs", solInit->"solInit",
    ver123->"ver123", uiFunc->"uiFunc"};
  procNames={lpInit->"lpInit", lpUI->"lpUI", lpCAC->"lpCAC", lpComp->"lpComp",
     lpVerify->"lpVerify"};

  Map[
    dbgPrint[" ", # /. modNames /. procNames];
    &, printOptions];
];
(***************************    Module: getPrintOptsVerbose   ******************** *)


(******************************    Module: convert2Print    ********************** *)
(* Purpose:  Replaces Mathematica's StringForm.                                     *)
(******************************************************************************** *)
convert2Print::usage="convert2Print[ddEQ] converts the specified equation(s)
 from an internal representation to a user friendly representation.";
(******************************************************************************** *)
(* Output:  True/False                                                              *)
(* Code is in package:  LPUtilities.m                                               *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
convert2Print[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList},

  Return[ddEQ /. int2prtRules];
]; 


(*************************    Function: myDenominator     ************************ *)
(* Output:  NULL                                                                  *)
(* Code is in package:  LPUtilities.m                                             *)
(* Specially modified denominator function -- returns 0 for denominator of 0,    *)
(* instead of 1                                                                   *)
(*                                                                                *)
(* Last Modified:                                                                 *)
(********************************************************************************** *)
myDenominator[x_] := If[IntegerQ[Numerator[x]] && Numerator[x]==0, 0, Denominator[x]];


(*************************    Function: rationalPolyGCD     ********************** *)
(* Output:  NULL                                                                  *)
(* Code is in package:  LPUtilities.m                                             *)
(* Mathematica can not handle rational polynomials exactly right w/ GCD.         *)
(* This does as I expect                                                          *)
(*                                                                                *)
(* Last Modified:                                                                 *)
(********************************************************************************* *)
rationalPolyGCD[x_]:= 
  Simplify[PolynomialGCD[Union[Map[Numerator[Factor[#]]&,Flatten[x]]] /. List -> Sequence]/
              PolynomialGCD[Union[Map[myDenominator[Factor[#]]&,Flatten[x]]] /. List -> Sequence] 
];


(***************************    Module: mergeSolLists    ***************************)
(* Purpose:  Merges the sorted list, solList2, into the sorted list solList1        *)
(* Input: 2 lists of solutions sorted according to subscripts                       *)
(**********************************************************************************)
mergeSolLists::usage="mergeSolLists[list1, list2] merges the specified lists 
 into a single list (pre-defined sort order).";
(**********************************************************************************)
(* Output:  Single list of solutions consistent with original sort                 *)
(* Code is in package:  LPcac.m                                                    *)
(* Last Modified:                                                                  *)
(**********************************************************************************)
mergeSolLists[solutionList1_, solutionList2_, debug_] :=
 Module[{solList1 = solutionList1, solList2 = solutionList2,
         dPrint, dStat=debug, solList={{},{},{}}, tmpList={}, tmpRule={},
         idx=0, ctag="mrgLst"},

  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: mergeSolLists, Package: LPcac.m"];
  dPrint[ctag, "#####################################################"];
  dPrint[ctag, "Solution List 1: ", convert2Print[solList1]];
  dPrint[ctag, "Solution List 2: ", convert2Print[solList2]];

  If[Length[ Flatten[solList1] ]>0, 
    If[Length[ Flatten[solList2] ]>0,
      MapIndexed[
        idx = First[#2];
        If[Length[#1] > 0,
          tmpList = Flatten[#1];
          MapIndexed[
            redRule = #1;
            newRule = Map[First[#] -> Simplify[(First[#] /. #) /. redRule] &, solList2[[idx]] ];
            solList2[[idx]] = newRule;
          &, tmpList];

          solList2[[idx]] = Select[solList2[[idx]],
             Length[Union[Flatten[# /. {Rule -> List}]]]>1 &];   
          solList[[idx]] = Complement[ Union[solList1[[idx]], solList2[[idx]]], {0}];
        ,
          solList[[idx]] = solList2[[idx]];
        ];
        dPrint[ctag, "Merged List: ", convert2Print[ solList[[idx]] ]]; 
      &, solList1];
    ,
      solList = solList1;
    ];
  ,
    solList = solList2;
  ];
  dPrint[ctag, "Merged List of Solutions: ", convert2Print[solList]]; 

  dPrint[ctag, "Exit"];

  Return[solList];
];
(***************************    Module: mergeSolLists    ***************************)


(***************************    Module: solveInitDDE    ************************* *)
(* Input: The original (possibly augmented) system of DDE.                         *)
(* Purpose: Finds a solution                                                       *)
(*        representation of the system by solving:                                 *)
(*        1. for any 2-sub variables                                               *)
(*        2. for any 1-sub variables                                               *)
(*        Note:  special care must be given to any single-edge equations           *)
(*               as to the variable to be solved for.                              *)
(**********************************************************************************)
solveInitDDE::usage="solveInitDDE[faceEquList, sglEdge, dblEdge] solves the 
 specified equation(s) for variables necessary to confirm consistency around the 
 cube."; 
(**********************************************************************************)
(* Output:  List of rules describing solutions for the variables                   *)
(*          found in that face's equations.                                        *)
(*          Also, a list of variables solved for in that face                      *)
(* Code is in package:  LPcac.m                                                    *)
(* Last Modified:                                                                  *)
(**********************************************************************************)
solveInitDDE[faceEquList_, sEdge_, dEdge_] :=
Module[{faceEQ = faceEquList, sglEdge=sEdge, dblEdge=dEdge, dStat, vStat, dPrint, 
  vPrint, equRef, printFF, vars2, vars1d, vars1s, linEQ, solList,
  varSol, varsFnd, solList2, solList1s, solList1d, dblFaceEQ, cVar, tmpList,
  sglSub, varsConstrained, edgeEQ, msg, vC, ctag="solInit",
       varList={x[0,0,0],y[0,0,0],z[0,0,0],
                x[1,0,0],y[1,0,0],z[1,0,0],
                x[0,1,0],y[0,1,0],z[0,1,0]},
       orderM={{1,1,1,1,1,1,1,1,1},
               {1,0,0,0,0,0,0,0,0},
               {0,1,0,0,0,0,0,0,0},
               {0,0,1,0,0,0,0,0,0},
               {0,0,0,1,0,0,0,0,0},
               {0,0,0,0,1,0,0,0,0},
               {0,0,0,0,0,1,0,0,0},
               {0,0,0,0,0,0,1,0,0},
               {0,0,0,0,0,0,0,1,0}} },


  dStat = getPrintOptsDebug[solInit]; 
  vStat = getPrintOptsVerbose[solInit] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: solveInitDDE ", "File: LPcac.m"];
  dPrint[ctag, "##################################################"];

  dPrint[ctag, "Solve for any _12 variables"];
  dblFaceEQ = Complement[faceEQ, sglEdge, dblEdge];
  dPrint[ctag, "Using equations: ", convert2Print[dblFaceEQ]];

  solList2={};  vars2={};
  Map[
    cVar = #;
    dPrint[ctag, "Current variable: ", convert2Print[cVar]];

    linEQ = Select[dblFaceEQ, Exponent[#,cVar]==1&];
    If[Length[linEQ]>0,
      (* sort the matching equations based on LeafCount and then we only         *)
      (* need to grab the 1st equ referenced to have simplest.                   *)
      linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
      equRef = Factor[linEQ[[First[First[Position[linEQ, cVar]]]]]];
      dPrint[ctag, "Current equation containing variable: ", convert2Print[equRef]];

      varSol = Factor[Flatten[Solve[equRef==0, cVar]]];
      If[Length[solList2]>0,
        (* extract all _ 12 solutions from the list, reduce them if necessary     *)
        (* and re-insert them into the list of solutions.                         *)
        solList2 = Union[MapAll[Factor, FilterRules[solList2,{_[1,1,0]}]/.varSol], 
                          FilterRules[solList2, Except[{_[1,1,0]}]]];
      ];

      (* now add the new solution to the list  *)
      solList2 = Union[solList2, varSol];

      (* and add the current variable to the list of variables found *)
      vars2 = Union[vars2, {cVar}];
      dblFaceEQ = Complement[Simplify[dblFaceEQ /. varSol],{0}];
    ];
  &, {x[1,1,0], y[1,1,0], z[1,1,0]}];
  If[!(Length[solList2] > 0), 
    Throw[$Failed, failTag[$lpErrNo12, solveInitDDE]]
  ];

  dPrint[ctag, " _12 solutions found: ", convert2Print[solList2]]; 
  dPrint[ctag, "Variables found: ``", convert2Print[vars2]]; 
  sglSub = PadRight[Delete[Level[#,{1}],1],3] /. {List -> Head[#]}& /@ vars2;
  dPrint[ctag, "Key variable solutions needed: ", convert2Print[sglSub]]; 

  (* The remaining equations (which do not reference _ 12 variables) are either   *)
  (* single or double edge equations.  The single edge provide additional        *)
  (* constraints for Lax pair computations so care as to which variable to solve *)
  (* for, must be taken.  Double edge can be handled easily.                     *)

  solList1s={};  vars1s={};  varSol = {};
  dPrint[ctag, "Edge equations: ", convert2Print[sglEdge]];
  If[Length[sglEdge]>0,
    (* of the _ 12 solutions found, which _ 1 variables appear in the edge equations *)
    varsConstrained = Intersection[Variables[#], sglSub]& /@ sglEdge;
    If[Length[Complement[Length[#]& /@ varsConstrained, {0,1}]] > 0,
      (* there are edge equations with more than 1 necessary variable appearing  *)
      MapIndexed[
        vC = Complement[#1, vars1s];
        edgeEQ = sglEdge[[ First[#2] ]];
        If[Length[vC] > 1, 
          dPrint[ctag, "Edge constraints of ", convert2Print[edgeEQ], 
                         " involving ``", convert2Print[vC]];
          (* If the UserInteractive flag is set, prompt the user as to which variable to solve for *)
          (* prompt when calculating LP since choice will result in different answers  *)
(*
          If[pcUserInput && operationG == oCLP,
            msg = StringJoin["The equation,\n", ToString[convert2Print[edgeEQ]], 
                    "\nintroduces constraints between ", ToString[convert2Print[vC] ],
                    "\n Solve for the following variable:"];
            dPrint[msg, ctag];  
            cVar = ChoiceDialog[msg, Map[convert2Print[#] -> #&, vC]];
          ,
            cVar = First[vC];
          ];
*)
          cVar = First[vC];

          dPrint[ctag, "Variable Selected: ", convert2Print[cVar]]; 
          varSol = Factor[Flatten[Solve[edgeEQ==0, cVar]]];
          $edgeConstraints = Union[$edgeConstraints, varSol];
        , (* else Length[vC] <=1  *)
          If[Length[vC]==1,
            dPrint[ctag, "Edge eq involving only 1 constrained var: ", convert2Print[edgeEQ]];
            cVar = First[vC];
            varSol = Factor[Flatten[Solve[edgeEQ==0, cVar]]];
          ,
            dPrint[ctag, "Edge eq. involving no constrained vars: ", convert2Print[edgeEQ]];
            cVar = Select[Reverse[MonomialList[edgeEQ, varList, orderM, Modulus->2]],
               Length[Position[solList1s,#]]==0 &,1];
            dPrint[ctag, "Current variable: ", convert2Print[cVar]];
            varSol = Factor[Flatten[Solve[edgeEQ==0, cVar]]];
          ];  (* Length[vC] \[Equal] 1 *)
        ]; (* Length[vC] > 1 *)
        solList1s = Union[solList1s, varSol];
        vars1s = Flatten[Union[vars1s, {cVar}]];

        dPrint[ctag, "Solution found: ", convert2Print[varSol]];  
      &, varsConstrained];
    ];
  ];

  dPrint[ctag, "Constraints found: ", convert2Print[$edgeConstraints]];    
  dPrint[ctag, "Single edge solutions found: ", convert2Print[solList1s]]; 

  dPrint[ctag, "Solve for remaining single subscript variables."];
  solList1d={};  vars1d={};
  If[Length[dblEdge] > 0, 
    dPrint[ctag, "Solve all double edge equations: ", convert2Print[dblEdge]];
    Map[
      cVar = #;
      dPrint[ctag, "Current: variable: ", convert2Print[cVar]];

      linEQ = Select[dblEdge, Exponent[#,cVar]==1&];
      If[Length[linEQ]>0,
        (* sort the matching equations based on LeafCount and then we only         *)
        (* need to grab the 1st equ referenced to have simplest.                   *)
        linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
        equRef = Factor[linEQ[[First[First[Position[linEQ, cVar]]]]]];
        dPrint[ctag, "Current: equation: ", convert2Print[equRef]];

        varSol = Factor[Flatten[Solve[equRef==0, cVar]]];
        If[Length[solList1d]>0,
          (* extract solutions from the list, reduce them if necessary             *)
          (* and re-insert them into the list of solutions.                        *)
          solList1d = Union[MapAll[Factor, FilterRules[solList1d,{_[1,1,0]}]/.varSol], 
                            FilterRules[solList1d, Except[{_[1,1,0]}]]];
        ];

        (* now add the new solution to the list  *)
        solList1d = Union[solList1d, varSol];

        (* and add the current variable to the list of variables found *)
        vars1d = Union[vars1d, {cVar}];
        dblEdge = Complement[Simplify[dblEdge /. varSol],{0}];
      ];
    &, {x[1,0,0], y[1,0,0], z[1,0,0], x[0,1,0], y[0,1,0], z[0,1,0]}];
  ];

  solList = {{}, Union[solList1s, solList1d], solList2};
  dPrint[ctag, "Solution Set found: ", convert2Print[solList]]; 

(*
  frontFaceSolutions = {{}, Union[solList1s, solList1d], solList2};
  solList = frontFaceSolutions;
  dPrint[ctag, "Solution Set for this Face: ",  convert2Print[frontFaceSolutions]]; 
*)

  varsFnd = Union[vars2,vars1d, vars1s];
  dPrint[ctag, "Variables Solved for: ", convert2Print[varsFnd]];

(*
  varsFound = Union[varsFound, varsFnd];
  faceVariables = varsFound;
*)

  dPrint[ctag, "Exit"];
  Return[{solList, varsFnd}];
]; 
(***************************    Module: solveInitDDE    ************************* *)


(************************    Module: findSubSolutions     *********************** *)
(* Purpose:  Finds solutions of variables from the specified list of variables.    *)
(* Input: 2 lists of solutions sorted according to subscripts                      *)
(**********************************************************************************)
findSubSolutions::usage="findSubSolutions[ddeEquationList, subscriptList, varPattern]
 find solutions for the variables specified.";
(**********************************************************************************)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPUtilities.m                                              *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
findSubSolutions[ddeEquationList_, subscriptList_, varPattern_] :=
 Module[{ddEQ = ddeEquationList, varList = subscriptList, vPtrn = varPattern, 
    dPrint, msg, dStat, vars, cVar, coefLeafEq, slist, eq, 
    equRef, varSol, solList={},
    ctag="findSub"},
               
  dStat = getPrintOptsDebug[findSub]; 
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "############ findSubSolutions - entry ############"];
  dPrint[ctag, "System to solve: ", convert2Print[ddEQ]];
  vars = Sort[Intersection[Variables[ddEQ], $allowedVars], Count[#1,1]<Count[#2,1]&];
  dPrint[ctag, "Variables Referenced:  ", convert2Print[vars]];

  varList = Intersection[varList, vars];
  dPrint[ctag, "Variables in question referenced: ", convert2Print[varList]];
  (* Find the solutions from the variables specified *)
  Map[
    cVar = #;
    dPrint[ctag, "Current variable: ", convert2Print[cVar]];

    (* Form a list of coefficients, leaf counts and the equations.  Sort by *)
    (* leaf count, select by non-zero coefficient => equation to solve.     *)
    coefLeafEq = {Flatten[Map[Coefficient[#,cVar] &, ddEQ]], 
                  Map[LeafCount[#] &, ddEQ],
                  ddEQ};
    slist = coefLeafEq[[All, Ordering @ coefLeafEq[[2]] ]];
 
    dPrint[ctag, "Parts: ", Flatten[Position[slist[[1]], _?(# != 0 || ! NumericQ[#] &)]]];
    eq = Part[Flatten[Position[slist[[1]], _?(# != 0 || ! NumericQ[#] &)]],2];
    equRef = slist[[3]][[eq]];
    dPrint[ctag, "Equation to solve: ", convert2Print[equRef]];
    varSol = Factor[Flatten[Solve[equRef==0, cVar]]];
    dPrint[ctag, "Solution: ", convert2Print[varSol]];

    If[Length[solList]>0,
      (* extract all _ 12 solutions from the list, reduce them if necessary     *)
      (* and re-insert them into the list of solutions.                         *)
      solList = Union[MapAll[Factor, FilterRules[solList, vPtrn]/.varSol], 
                          FilterRules[solList, Except[vPtrn]]];
    ];
    (* now add the new solution to the list  *)
    solList = Union[solList, varSol];
    dPrint[ctag, "Solution List: ", convert2Print[solList]];

    (* Reduce the equation list against the solution found *)
    ddEQ = Complement[Simplify[ddEQ /. varSol],{0}];
  &, varList];
  
  dPrint[ctag, "Exit"];
  Return[solList];
];
(************************    Module: findSubSolutions     *********************** *)


(***************************    Module: augmentDDE     ************************** *)
(* Purpose:  Augments the given DDE based on the existence of edge equations.      *)
(* Input:   User Specified List of discrete differential functions                 *)
(******************************************************************************* *)
augmentDDE::usage="augmentDDE[ddEquationList] augments the given DDE in the case of 
 existence of single or double edge equations.";
(******************************************************************************* *)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPUtilities.m                                              *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
augmentDDE[ddeEquationList_] :=
 Module[{ddEQ = ddeEquationList, dStat, vStat, dPrint, vPrint, augmented=False,
  dblSub, vars, edgeEQ, dblEdge, sglEdge, tmpEQ, vpos, newEQ, aDDE,
  ctag="augDDE"},
               
               
  dStat = getPrintOptsDebug[augDDE]; 
  vStat = getPrintOptsVerbose[augDDE] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: augmentDDE", "File: LPUtilities.m"];
  dPrint[ctag, "##################################################"];
  dPrint[ctag, "ddEQ: ", convert2Print[ddEQ]];

  vars = Sort[Intersection[Variables[ddEQ], $allowedVars], Count[#1,1]<Count[#2,1]&];

  (* Check for any single-edge equations.  These will only reference x, x1 or x2 *)
  dblSub = Extract[vars, Position[vars, _[1,1,0]]];
  dPrint[ctag, "Checking eqs for the dbl-sub var: ", convert2Print[dblSub]];

  edgeEQ = Intersection[Factor[ddEQ], Factor[Flatten[MonomialList[ddEQ, dblSub]]]];
  dPrint[ctag, "Equations not referencing dbl-subs: ", convert2Print[edgeEQ]];

  (* Of these, we could have either single- or double-edge equations.        *)
  (* single edge reference only: x and x1 or x and x2 type variables         *)
  (* double edge reference x, x1 and x2 (but not x12)                        *)
  dblEdge={};
  sglEdge={};
  Map[
    tmpEQ = #;
    MapThread[
      vpos = Flatten[Position[Variables[tmpEQ], #1]];
      If[Length[vpos]>0,vpos= First[vpos],vpos=0];
      If[vpos > 0,
        dPrint[ctag, "Eq references x1/x2 type variables: "];
        dPrint[ctag, "Eq: ", convert2Print[tmpEQ]];

        (* check if it also references other 1-sub, making this a 2-edge eq  *)   
        vpos = Flatten[Position[Variables[tmpEQ], #2]];
        If[Length[vpos]>0,vpos=First[vpos],vpos=0];
        If[vpos > 0,
          dPrint[ctag, "2-edge equation: ", convert2Print[tmpEQ]];
          dblEdge = Union[dblEdge, Simplify[{tmpEQ}]];
        , (* else *)
          sglEdge = Union[sglEdge, Simplify[{tmpEQ}]];
        ];
        dPrint[ctag, "dblEdge: ", Length[dblEdge], 
                     ", sglEdge: ", Length[sglEdge]];
      ];
    &, {{_[1,0,0],_[0,1,0]}, {_[0,1,0],_[1,0,0]}}];
  &, edgeEQ];
  aDDE=ddEQ;
  augmented=False;
  If[Length[sglEdge] > 0, 
    augmented=True;
    (* We have single-edge equations, so augment the given system *)
    Map[
      tmpEQ = #;
      MapThread[
        vpos = Flatten[Position[Variables[tmpEQ], #1]];
        If[Length[vpos]>0,vpos = First[vpos],vpos = 0];
        If[vpos > 0, 
         (* Shift the x/x2 equation across the front face in the 1 direction  *)
          newEQ = tmpEQ /. #2;
          dPrint[ctag, "shifted: ", convert2Print[newEQ]];
          aDDE = Union[aDDE, {newEQ}];
        ];
      &, {{_[1,0,0],_[0,1,0]}, {lat2ShiftRules, lat1ShiftRules}}];
    &, sglEdge];
    dPrint[ctag, "Augmented System: ", convert2Print[ Map[# == 0 &, aDDE]]];
  ];

  dPrint[ctag, "Augmented? ", augmented];
  dPrint[ctag, "DDE: ", convert2Print[aDDE]];
  dPrint[ctag, "Exit"];
  Return[{aDDE, augmented}];
]; 
(***************************    Module: augmentDDE     ************************** *)


(************************    Module: removeDoubleRefs     *********************** *)
(* For systems containing mulitple refs to double-subscripted variables, we        *)
(* identify the references, use the equation containing _12, _1 and _2 refs        *)
(* to find a solution for _12 and use it to remove the other references.           *)
(******************************************************************************* *)
removeDoubleRefs::usage="removeDoubleRefs[ddeEquationList, dblRefVars] redefines
the provided system by identifying the equations which reference the indicated 
double-subscripted variables, solves appropriately and modifies the equations.";
(**************************    Function: removeDoubleRefs   ********************* *)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPUtilities.m                                              *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
removeDoubleRefs[ddeEquationList_, dblRefVars_] :=
 Module[{ddEQ = ddeEquationList, dblVars = dblRefVars, dPrint, vPrint, 
         subList = {a[1,0,0], a[0,1,0], a[1,1,0]}, refList, refListPtrn,
         newEQ, dblRefEq, hasAnyVars, theEquationIdx, theEquation,
         theEquationSol, theOtherEqs, dStat, vStat, 
         ctag="rmDRef"},

  dStat = getPrintOptsDebug[rmDRef]; 
  vStat = getPrintOptsVerbose[rmDRef] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: removeDoubleRefs ", "File: LPUtilities.m"];
  dPrint[ctag, "#######################################################"];
  Map[
    (* Determine which equations reference the 12-sub variable *)
    dblRefEq = Map[Function[$x$,Length[Position[Variables[$x$],#]]], ddEQ];

    (* Determine which equations reference the 12-sub, 1-sub AND 2-sub variable *)
    refList = subList /. {a-> Head[#]};
    refListPtrn = Apply[Alternatives, refList];
    hasAnyVars = Length[DeleteCases[#,0]]& 
         /@ Map[Function[$x$,Length[Position[Variables[$x$],#]]&/@ refListPtrn], ddEQ];
    dPrint[ctag, "Number of refs of any _12, _1 or _2 variables: ``", hasAnyVars];
    theEquationIdx = Flatten[Intersection[Position[dblRefEq,1], Position[hasAnyVars, 3]]];
    dPrint[ctag, "Eq referencing everything: ", Map[convert2Print, theEquationIdx]];
    If[Length[theEquationIdx] > 0,
      theEquation = ddEQ[[First[theEquationIdx]]];
      vPrint[ctag, "Equation: ", Map[convert2Print, theEquation]];
      theEquationSol = Factor[Flatten[Solve[theEquation == 0, #]]];
      vPrint[ctag, "Corresponding Solution: ", Map[convert2Print, theEquationSol]];
      theOtherEqs =  Flatten[Intersection[Position[dblRefEq,1], 
                                          Position[hasAnyVars, n_/; n!= 3 && n != 0]]];
      dPrint[ctag, "The other equations referencing dbl sub: ", theOtherEqs];
      Map[
        newEQ = Numerator[Simplify[Together[ddEQ[[#]] /. theEquationSol]]];
        vPrint[ctag, "New Equation: ", convert2Print[newEQ]];
        ddEQ[[#]] = newEQ;
      &, theOtherEqs];
    ];
  &, dblVars];

  dPrint[ctag, "Exit"];
  Return[ddEQ];
];
(************************    Module: removeDoubleRefs     *********************** *)


(***************************    Module: findEdgeEquations   ********************* *)
(* Input: The original system of DDE.                                              *)
(* Purpose:  Finds the single edge and double edge equations in the given DDE.     *)
(******************************************************************************* *)
findEdgeEquations::usage="findEdgeEquations[ddEquationList] find the single edge and 
 double edge equations in the given DDE.";
(******************************************************************************* *)
(* Output:  Single edge equations & Double edge equations                          *)
(* Code is in package:  LPUtilities.m                                              *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
findEdgeEquations[ddeEquationList_] :=
 Module[{ddEQ = ddeEquationList, dStat, vStat, dPrint, vPrint, edgeEQ, tmpEQ,
   dblEdge={}, sglEdge={}, dblSubSystemVars, sysVars,
   vpos, dblSubList = {x[1,1,0], y[1,1,0], z[1,1,0]}, msg,
   ctag="edgeEQs"},
 
 
  dStat = getPrintOptsDebug[edgeEQs]; 
  vStat = getPrintOptsVerbose[edgeEQs] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: findEdgeEquations ", "File: LPUtilities.m"];
  dPrint[ctag, "##################################################"];
  sysVars = Sort[Intersection[Variables[ddEQ], $allowedVars], Count[#1,1]<Count[#2,1]&];
  dblSubSystemVars = Intersection[sysVars, dblSubList];
  dPrint[ctag, "The referenced double-sub variables: ", dblSubSystemVars];

(*  MonomialList will also return those equations not involving 2-sub variables    *)
(*  so intersecting the results with the original system will produce only those   *)
(*  equations which can be considered "edge" equations.                            *)
  edgeEQ = Intersection[Factor[ddEQ], 
                 Factor[Flatten[MonomialList[ddEQ,dblSubSystemVars]]]];
  dPrint[ctag, "Equations not referencing the dbl-subs: ", convert2Print[edgeEQ]];

  If[Length[edgeEQ] > 0,
    dPrint[ctag, "Edge Equations: ``", convert2Print[edgeEQ]];

    (* Of these, we could have either single- or double-edge equations.        *)
    (* single edge reference only: x and x1 or x and x2 type variables         *)
    (* double edge reference x, x1 and x2 (but not x12)                        *)
    Map[
      tmpEQ = #;
      MapThread[
        vpos = Flatten[Position[Variables[tmpEQ], #1]];
        If[Length[vpos]>0,vpos= First[vpos],vpos=0];
        If[vpos > 0,
          dPrint[ctag, "Eq. references x1 type variables: ", convert2Print[tmpEQ]];

          (* check if it also references other 1-sub, making this a 2-edge eq  *)   
          vpos = Flatten[Position[Variables[tmpEQ], #2]];
          If[Length[vpos]>0,vpos=First[vpos],vpos=0];
          If[vpos > 0,
            dPrint[ctag, "2-edge equation: ", convert2Print[tmpEQ]];
            dblEdge = Union[dblEdge, Simplify[{tmpEQ}]];
          , (* else *)
            sglEdge = Union[sglEdge, Simplify[{tmpEQ}]];
          ];
          dPrint[ctag, "dblEdge: ", Length[dblEdge], "; sglEdge: ", Length[sglEdge]];
        ];
      &, {{_[1,0,0],_[0,1,0]}, {_[0,1,0],_[1,0,0]}}];
    &, edgeEQ];
  ];

  Return[{sglEdge, dblEdge}];
];
(***************************    Module: findEdgeEquations   ********************* *)


(************************    Module : ddeSystemToSolutions   ******************** *)
(* Input: The original system of DDE.                                              *)
(* Purpose: Finds a solution representation of the system by solving:              *)
(*        1. for any 2-sub variables                                               *)
(*        2. for any 0-sub variables                                               *)
(*        2. for any 1-sub variables                                               *)
(*        Note:  special care must be given to any single-edge equations           *)
(*               as to the variable to be solved for.                              *)
(******************************************************************************* *)
ddeSystemToSolutions::usage="ddeSystemToSolutions[ddEquationList] solves the 
specified DDE, returning the corresponding list of solutions.";
(************************    Function: ddeSystemToSolutions   ******************* *)
(* Output:  List of rules describing solutions for the variables                   *)
(* Code is in package:  LPUtilities.m                                              *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
ddeSystemToSolutions[ddeEquationList_] :=
 Module[{ddEQ = ddeEquationList, dStat, vStat, dPrint, vPrint, newDDE, 
   dblList, mx, dblRefVars, cVar, coefLeafEq, vars, slist, eq, equRef, 
   varSol, solList={}, solList2={}, solList1={}, solList0={},
   dblSubList = {x[1,1,0], y[1,1,0], z[1,1,0]}, msg, 
   sglSubList = {x[1,0,0], y[1,0,0], z[1,0,0]},
   noSubList={x[0,0,0], y[0,0,0], z[0,0,0]}, ctag="sys2sol"},

               
  dStat = getPrintOptsDebug[sys2sol]; 
  vStat = getPrintOptsVerbose[sys2sol] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: ddeSystemToSolutions ", "File: LPUtilities.m"];
  dPrint[ctag, "#####################################################"];
(*
  (* Check for double references of the _12 variables *)
  mx = Max[dblList = Map[Length, DeleteCases[#, 0]& /@ 
                      Map[Function[$x$, Map[Coefficient[#, $x$]&, ddEQ]], dblSubList]]];
  If[mx > 1, 
    (* A 12-sub variable has multiple references in the given DDE *)
    dblRefVars = Extract[dblSubList, Position[dblList, mx]];
    dPrint[ctag, "The DDE contains multiple references to: ", 
                                         Map[convert2Print, dblRefVars]];
    dPrint[ctag, "Modify the DDE by removing the multiple references."];

    (* Examine equations ref-ing the _12 variable(s), solve/sub to remove ref *)
    newDDE = removeDoubleRefs[ddEQ, dblRefVars];
    dPrint[ctag, convert2Print[ Map[#==0 &, newDDE] ]];
  , (* else *)
    dPrint[ctag, "No double-subscripted multiple references."];
    newDDE = ddEQ;
  ];

  (* Augment the given system based on existence of edge equations *)
  dPrint[ctag, "Check for edge equations and augment if necessary."];
  newDDE = augmentDDE[newDDE];
  dPrint[ctag, "After augmentDDE:  ``", newDDE];
*)
(* CHANGED HEREMAN JUL 2, 2017 *)
  dPrint[ctag, "System to solve: ", convert2Print[ Map[#==0 &, ddEQ] ]];
  vars = Sort[Intersection[Variables[ddEQ], $allowedVars], Count[#1,1]<Count[#2,1]&];
  dPrint[ctag, "Variables Referenced:  ", convert2Print[vars]];

  dblSubList = Intersection[dblSubList, vars];
  dPrint[ctag, "Double-sub variables referenced: ", convert2Print[dblSubList]];
  
  (* Find the double-subscript solutions *)
  solList2 = findSubSolutions[ddEQ, dblSubList, {_[1,1,0]}];
  ddEQ = Complement[Simplify[ddEQ /. solList2],{0}];
  dPrint[ctag, "Eq List:  ", convert2Print[ddEQ]];

  (* Find the no-subscript solutions *)
  If[Length[ddEQ] > 0, 
    solList0 = findSubSolutions[ddEQ, noSubList, {_[0,0,0]}];
    ddEQ = Complement[Simplify[ddEQ /. solList0],{0}];
    dPrint[ctag, "Eq List:  ", convert2Print[ddEQ]];
  ];

  (* Find the single-subscript solutions *)
  If[Length[ddEQ] > 0, 
    solList1 = findSubSolutions[ddEQ, sglSubList, {_[1,0,0]}];
    ddEQ = Complement[Simplify[ddEQ /. solList1],{0}];
    dPrint[ctag, "Eq List: ", convert2Print[ddEQ]];
  ];

(*  solList = {{solList0}, {solList1}, {solList2}};  *)
  solList = {solList0, solList1, solList2};
  dPrint[ctag, "Solution List:  ``", convert2Print[solList]];

  dPrint[ctag, "Exit"];
  Return[solList];
]; 
(************************    Module : ddeSystemToSolutions   ******************** *)


(*************************    Module : verifyLaxPairs   ************************* *)
(* Purpose:  Initializes the global variable lpVariable.                           *)
(* Input:  Null                                                                    *)
(******************************************************************************* *)
(* was: verifyLaxPairs::usage="verifyLaxPairs[ddEQ, matL, matM, scalarL, scalarM, ddEQs] *)
(* HEREMAN, JUL 26, 2017 corrected: *)
verifyLaxPairs::usage="verifyLaxPairs[ddEQ, matL, matM, scalarL, scalarM, uShifts, pEquiv]
 verifies that the specified L and M matrices satisfy the defining equation
 (L2)M - (M1)L = 0 on the provided discrete difference equation(s), ddEQs.";
(******************************************************************************* *)
(* Output:  True/False                                                             *)
(* Code is in package:  VerifyLP.m                                                 *)
(* Last Modified:                                                                  *)
(*********************************************************************************)
verifyLaxPairs[ddeEquationList_, matrixL_, matrixM_, scalarL_, scalarM_,
  uShifts_, pEquiv_] :=
Module[{ddEQ=ddeEquationList, matL = matrixL, matM = matrixM, scL = scalarL, 
  scM = scalarM, dPrint, vPrint, dStat, vStat, solList, matL2, matM1,
  coreL, coreL2, coreS, coreS2, coreM, coreM1, coreT, coreT1,  
  matM1L, matL2M, gcdL, gcdL2, gcdS, gcdS2, gcdM, 
  gcdM1, gcdT, gcdT1, gcdL2M, gcdM1L, pwr, 
  matL2Mred, matM1Lred, gcdL2Mmat, gcdM1Lmat, gcdL2Mred, gcdM1Lred, 
  defEQscalars, scalarsRed, defEQred, defEQred2, tmp, 
  ctag="vLaxPair"},


  dStat = getPrintOptsDebug[vLaxPair]; 
  vStat = getPrintOptsVerbose[vLaxPair] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: verifyLaxPairs ", "Package: LPVerify.m"];
  dPrint[ctag, "###############################################"];
  dPrint[ctag, "DDE: "];
  dPrint[ctag, convert2Print[ddEQ]]; 

  (* form the defining equation from the calculated lax pair    *)
  (* Break each matrix down to scalar * matrix combinations     *)
  (* LAX Pair L core & L2 core*)
  gcdL = rationalPolyGCD[Flatten[matL]];
  gcdL2 = gcdL /. lat2ShiftRules;
  coreL = Cancel[matL/gcdL];
  coreL2 = coreL /. lat2ShiftRules;
  dPrint[ctag, "Computing components of L and L2"];
  dPrint[ctag, "L core decomposed: (gcd) ", convert2Print[gcdL], 
               "(L) ", MatrixForm[convert2Print[coreL]]];

  (* Since the scalar functions are actually matrices internally, *)
  (* we'll need to break these down to scalar * matrix combo      *)
  (* scalar function s & s2 *)
  gcdS = rationalPolyGCD[Flatten[scL]];
  gcdS2 = gcdS /. lat2ShiftRules;
  coreS = Cancel[scL/gcdS];
  coreS2 = coreS /. lat2ShiftRules;
  dPrint[ctag, "L scalar decomposed: (gcd) ", convert2Print[gcdS], 
               "(Lscalar) ", MatrixForm[convert2Print[coreS]]];

  (* LAX Pair M core & M1 core *)
  gcdM = rationalPolyGCD[Flatten[matM]];
  gcdM1 = gcdM /. lat1ShiftRules;
  coreM = Cancel[matM/gcdM];
  coreM1 = coreM /. lat1ShiftRules;
  dPrint[ctag, "Computing components of M and M1"];
  dPrint[ctag, "M core decomposed: (gcd) ", convert2Print[gcdM], 
               " (M) ", MatrixForm[convert2Print[matM]]];

  (* scalar function t & t1 *)
  gcdT = rationalPolyGCD[Flatten[scM]];
  gcdT1 = gcdT /. lat1ShiftRules;
  coreT = Cancel[scM/gcdT];
  coreT1 = coreT /. lat1ShiftRules;
  dPrint[ctag, "M scalar decomposed: (gcd) ", convert2Print[gcdT], 
               " (Mscalar) ", MatrixForm[convert2Print[coreT]]];

  (* The defining equation of (s2.L2).(t.M) - (s.L).(t1.M1)  *)
  (* is now represented as (L2'*S2')*(T'*M'))*(s2.L2).(t.M) - (S'*L')*(T1'*M1')*(s.L).(T1.M1)  *)
  (* In addition to simplifying the expression, we can also isolate any roots appearing         *)
  gcdL2M = (gcdL2*gcdS2)*(gcdM*gcdT);
  gcdM1L = (gcdM1*gcdT1)*(gcdL*gcdS);
  dPrint[ctag, "Computing GCD of scalars associated w/ L2 and M and those w/ M1 and L"];
  dPrint[ctag, "L2M gcd scalars: ", convert2Print[gcdL2M], 
               " and M1L gcd scalars ", convert2Print[gcdM1L]];

  matL2M = Simplify[(coreL2.coreS2).(coreM.coreT)];
  matM1L = Simplify[(coreM1.coreT1).(coreL.coreS)];
  (* vPrint[ctag, "*********************************************************************"]; *) 
  (* CHANGED HEREMAN JUL 2, 2017 used a zero matrix of proper size (dim) instead of scalar 0 *)
     vPrint[ctag, "**************** Defining Equation L2.M - M1.L = 0 ******************"];
     vPrint[ctag, "(", convert2Print[gcdL2M], " times ", MatrixForm[convert2Print[matL2M]],
               " - ", convert2Print[gcdM1L], " times ", MatrixForm[convert2Print[matM1L]],
               " ) = ", MatrixForm[IdentityMatrix[dim]-IdentityMatrix[dim]]];

  (* Verify the defining equation against the original dde     *)
  solList = ddeSystemToSolutions[ddEQ];
  dPrint[ctag, "Solutions found", convert2Print[solList]];
  If[Length[#] > 0, dPrint[ctag, "Sols: ", convert2Print[#]]] & /@ solList;

  (* Reduction will be done a 2-step process:                          *)
  (* 1. reduce w/ _12 solutions (know to occur in L2.M-M1.L            *)
  (* 2. if necessary, reduce using any remaining solutions             *)
  dPrint[ctag, "Reduce L2M using _12 solutions: ", convert2Print[ solList[[3]] ]];
  matL2Mred = Map[Factor, (matL2M /. solList[[3]] )];
  dPrint[ctag, "Matrix L2M reduced: ", MatrixForm[convert2Print[matL2Mred]]];

  dPrint[ctag, "Reduce L2M using: ", convert2Print[ solList[[2]] ],
               " and ", convert2Print[ solList[[1]] ]];
  matL2Mred = Map[Factor, (matL2Mred /. solList[[2]])];
  matL2Mred = Map[Factor, (matL2Mred /. solList[[1]])];
  dPrint[ctag, "Matrix L2M reduced: ", MatrixForm[convert2Print[matL2Mred]]];

  gcdL2Mmat = PolynomialGCD[Map[Numerator[#]&, Flatten[matL2Mred]] /. List->Sequence]/
           PolynomialLCM[Complement[Map[myDenominator[#]&, Flatten[matL2Mred]],{0}] /. List->Sequence];
  dPrint[ctag, "GCD of entries of the L2M matrix: ", MatrixForm[convert2Print[gcdL2Mmat]]];
  matL2Mred = Simplify[matL2Mred/gcdL2Mmat];
  dPrint[ctag, "L2M matrix with gcd removed: ", MatrixForm[convert2Print[matL2Mred]]];

  dPrint[ctag, "Reduce M1L using ", convert2Print[ solList[[3]] ]];
  matM1Lred = Map[Factor, (matM1L /. solList[[3]] )];
  dPrint[ctag, "Matrix M1L reduced ", MatrixForm[convert2Print[matM1Lred]]];

  dPrint[ctag, "Reduce M1L using ", convert2Print[ solList[[2]] ],
               " and ``", convert2Print[ solList[[1]] ]];
  matM1Lred = Map[Factor, (matM1Lred /. solList[[2]] )];
  matM1Lred = Map[Factor, (matM1Lred /. solList[[1]] )];
  dPrint[ctag, "Matrix L2M reduced: ", MatrixForm[convert2Print[matM1Lred]]];

  gcdM1Lmat = (PolynomialGCD[Map[Numerator[#]&, Flatten[matM1Lred]] /. List->Sequence])/
          (PolynomialLCM[Complement[Map[myDenominator[#]&, Flatten[matM1Lred]],{0}] /. List->Sequence]);
  dPrint[ctag, "GCD of entries of the M1L matrix: ", convert2Print[gcdM1Lmat]];
  matM1Lred = Simplify[matM1Lred/gcdM1Lmat];
  dPrint[ctag, "M1L matrix with gcd removed: ", MatrixForm[convert2Print[matM1Lred]]];

  gcdL2Mred = Factor[Factor[Factor[gcdL2M /. solList[[3]] ] /. solList[[2]] ] /. solList[[1]] ];
  gcdM1Lred = Factor[Factor[Factor[gcdM1L /. solList[[3]] ] /. solList[[2]] ] /. solList[[1]] ];
  If[uShifts,
    gcdL2Mred = Factor[gcdL2Mred /. pEquiv];
    gcdM1Lred = Factor[gcdM1Lred /. pEquiv];
  ];
  dPrint[ctag, "Defining equation parts (after reductions of matrices):"];
  dPrint[ctag, "( ", convert2Print[gcdL2Mred], " times ", 
         convert2Print[gcdL2Mmat],  " times ", MatrixForm[convert2Print[matL2Mred]]];
  dPrint[ctag, convert2Print[gcdM1Lred], " times ", 
         convert2Print[gcdM1Lmat], " times ", MatrixForm[convert2Print[matM1Lred]], ")"];

  (* Now that we have the pieces, we'll convert our standard defining equation *)
  (* as follows           *)
  (* s2t*(L2.M) - t1s*(M1.L) =>  (s2t)/(t1s)*(L2.M) - (M1.L)                    *)
  defEQscalars = Factor[(gcdL2Mred*gcdL2Mmat)/(gcdM1Lred*gcdM1Lmat)];
  dPrint[ctag, "Defining eq parts (scalar clean-up):"];
  dPrint[ctag, convert2Print[defEQscalars], " times ( ", 
   MatrixForm[convert2Print[matL2Mred]], " minus ", 
   MatrixForm[convert2Print[matM1Lred]], ")"];

  defEQscalars = Factor[Factor[Factor[defEQscalars /. solList[[3]] ] /. solList[[2]] ] /. solList[[1]] ];
  If[uShifts,
    defEQscalars = Factor[defEQscalars /. pEquiv];
  ];
  (* vPrint[ctag, "*********************************************************************"]; *)
  
  vPrint[ctag, "**************** Defining equation reduced using DDE ****************"];
 (* CHANGED HEREMAN JUL 2, 2017 used a zero matrix of proper size (dim) instead of scalar 0 *)
  vPrint[ctag, "(", convert2Print[defEQscalars], " times (", 
           MatrixForm[convert2Print[matL2Mred]], ") minus ", 
           MatrixForm[convert2Print[matM1Lred]], ") = ", MatrixForm[IdentityMatrix[dim]-IdentityMatrix[dim]]];

  pwr = First[Dimensions[coreL]];
  (* vPrint[ctag, "*********************************************************************"]; *)
     vPrint[ctag, "***** Verifying components of reduced defining (Lax) equation *******"];
  vPrint[ctag, "The leading scalar coefficient should be either 1 or -1 (on right hand side)."];
  (* CHANGED HEREMAN JUL 2, 2017 *)
  scalarsRed = Factor[PowerExpand[Power[defEQscalars,pwr]]];
  vPrint[ctag, "(", convert2Print[defEQscalars], ")^", pwr, " = ", 
            convert2Print[scalarsRed]];
  (* CHANGED HEREMAN JUL 2, 2017 - added line below *)
  vPrint[ctag, "Continuing with ", convert2Print[scalarsRed]];
  vPrint[ctag, "Evaluated on the partial difference equation(s), 
             the difference of the matrices, L2.M - M1.L, should reduce to the zero matrix."];
  (* vPrint[ctag, "The matrix difference should then be 0."]; *)
  defEQred = Simplify[matL2Mred - matM1Lred];
  If[uShifts,
    defEQred = Simplify[defEQred /. pEquiv];
  ];
  dPrint[ctag, "Difference: ", MatrixForm[convert2Print[defEQred]]];

  tmp = Union[Flatten[defEQred]];
  dPrint[ctag, "Union: ", convert2Print[tmp]];
  dPrint[ctag, "First If Check: ", Length[tmp] == 1 && First[tmp] == 0];
  (* CHANGED HEREMAN JUL 2, 2017 *)
  If[Length[tmp] == 1 && First[tmp] == 0,
    (* Valid Lax pair since defining equations reduces to 0 *)
    vPrint[ctag, "Lax equation evaluated on the partial difference equation(s): "];
    vPrint[ctage, MatrixForm[convert2Print[defEQred]]];
    vPrint[ctag, "Specified Lax pair satisfies the defining (Lax) equation."];
    tmp = True;
  ,
    (* else - given Lax pair does not satisfy the defining equation  *)
    vPrint[ctag, "Non-zero => Double-check for any negatives introduced before concluding failure."];
    defEQred = Simplify[matL2Mred + matM1Lred];
    dPrint[ctag, "Sum: ", MatrixForm[convert2Print[defEQred]]];
    tmp = Union[Flatten[defEQred]];
    dPrint[ctag, "Union: ", convert2Print[tmp]];
    dPrint[ctag, "Second If Check (False): ", Length[tmp] == 1 && First[tmp] == 0];
    (* CHANGED HEREMAN JUL 2, 2017 *)
    If[Length[tmp] == 1 && First[tmp] == 0,
      vPrint[ctag, "Lax equation evaluated on the partial difference equation(s):"]; 
      vPrint[ctag, MatrixForm[convert2Print[defEQred]]];
      vPrint[ctag, "Specified Lax pair satisfies the defining (Lax) equation."];
      tmp = True;
    ,
      (* else - given Lax pair still does not satisfy the defining (Lax) equation  *)
      tmp = False;
      vPrint[ctag, "Lax equation evaluated on the partial difference equation(s):"]; 
      vPrint[ctag, MatrixForm[convert2Print[defEQred]]];
      vPrint[ctag, "Specified Lax pair does not satisfy defining (Lax) equation."];
    ];
  ];

  dPrint[ctag, "Exit: ", tmp];
  Return[tmp];

(* correction: Ask TB How is below used? I took it out *)
(* 
  Label[failVerifyReturn];
  (* vPrint[ctag, "*********************************************************************"]; *)
     vPrint[ctag, "************ Verification of Calculated Lax pair failed *************"];
     Return[False];
*)
];
(*************************    Module : verifyLaxPairs   ************************* *)


(**************************    Module: verifyLPDriver    ************************ *)
(* Purpose:  Initializes the global variable lpVariable.                           *)
(* Input:  Null                                                                    *)
(******************************************************************************* *)
(* correction: HEREMAN JUL 31, 2017 arguments are missing *)
(* was: verifyLPDriver::usage="verifyLPDriver[ddEQ, matrixL, matrixM, uShifts, pEquiv] *)
(* now: *)
verifyLPDriver::usage="verifyLPDriver[ddEQ, matrixL, matrixM, expScalar, scalarT, scalarS, uShifts, pEquiv]
 verifies that the specified L and M matrices satisfy the defining equation 
 (L2)M - (M1)L = 0 on the provided discrete difference equation(s), ddEQs";
(******************************************************************************* *)
(* Output:  True/False                                                             *)
(* Code is in package:  VerifyLP.m                                                 *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
verifyLPDriver[ddeEquationList_, matrixL_, matrixM_, expScalar_, scalarT_, scalarS_,
 uShifts_, pEquiv_] :=
 (* CHANGED HEREMAN JUL 2, 2017 need dim later, therefore made global -- should be done differently later *)
 Module[{ddEQ = ddeEquationList, matL = matrixL, matM = matrixM, vPrint, dPrint, 
   dStat=False, vStat=False, 
   (* dim, *) 
   tFunc, sFunc, scalarL, scalarM, verifyLP,
   ctag="vLPDrvr"},

  dStat = getPrintOptsDebug[vLPDriver]; 
  vStat = getPrintOptsVerbose[vLPDriver] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: verifyLPDriver ", "Package: LPVerify.m"];
  dPrint[ctag, "###############################################"];
  dPrint[ctag, "DDE: ", convert2Print[ddEQ]];
  dPrint[ctag, "Matrix L: ", convert2Print[matL]];
  dPrint[ctag, "Matrix M: ", convert2Print[matM]];

  $ddeEquationList = ddEQ;
  
  (* Verify that the user has provided matrices to verify *)
  If[Length[matL] > 0 && Length[matM] > 0,
  (* vPrint[ctag, "*********************************************************************"]; *)
     vPrint[ctag, "********************** User-provided Lax Pair ***********************"];
    vPrint[ctag, "L = ", convert2Print[MatrixForm[matL]]];
    vPrint[ctag, "M = ", convert2Print[MatrixForm[matM]]];

    dim = First[Dimensions[matL]];
    If[expScalar, 
      tFunc = scalarT;  sFunc = scalarS;
      (* the scalar functions are explicitly stated and not integrated into the Lax pair *)
   (* vPrint[ctag, "*********************************************************************"]; *)
      vPrint[ctag, "******* User Explicitly Specified Additional Scalar Functions *******"];
      vPrint[ctag, "Scalar function associated with L: ", convert2Print[tFunc]];
      vPrint[ctag, "Scalar function associated with M: ", convert2Print[sFunc]];
    ,
      (* else, any scalar functions are assumed to be integrated into the Lax pair   *)
   (* vPrint[ctag, "*********************************************************************"]; *)
      vPrint[ctag, "*************** No Additional Scalar Functions Specified ************"];
      tFunc=1;  sFunc=1;
    ];
    scalarL = tFunc*IdentityMatrix[dim];
    scalarM = sFunc*IdentityMatrix[dim];

    dPrint[ctag, "Calling verifyLaxPairs"];
    (* was: verifyLP = verifyLaxPairs[ddEQ, matL, matM, scalarL, scalarM, uShifts, pEquiv]; *)
    (* correction: ASK TB: are uShifts and pEquiv defined ??? *)
    (* should they be ushift and pEquivs ??? *)
    (* changed into: *)
    (* also: Added Evaluate to assure that the returned value binds to verifyLP *)
    verifyLP = Evaluate[verifyLaxPairs[ddEQ, matL, matM, scalarL, scalarM, ushift, pEquivs]];
    dPrint[ctag, "Return from verifyLaxPairs"];

    (* CHANGED HEREMAN JUL 2, 2017 *)
    vPrint[ctag, "Lax equation evaluated on the partial difference equation(s):"]
    (* vPrint[ctag, "*** Valid Lax pair specified: ", verifyLP]; *)
  , (* else *)
 (* vPrint[ctag, "*********************************************************************"]; *)
    vPrint[ctag, "******* Verification not completed -- No Lax pairs provided. ********"];
  ];

  dPrint[ctag, "Exit"];
  Return[];
];
(**************************    Module: verifyLPDriver    ************************ *)


(**********************    Module: calculateScalarFunctions    ****************** *)
(* Purpose:  From the given equations, calculate candidate scalar functions s, t   *)
(* (and possibly S, T)                                                             *)
(* Input:  Lax matrices matrixL, matrixM                                           *)
(******************************************************************************* *)
calculateScalarFunctions::usage="calculateScalarFunctions[matrixL, matrixM, sVars]
 calculates the corresponding scalar functions necessary to satisfy the defining
 Lax equation.";
(******************************************************************************* *)
(* Output:  List of scalar function pairs (if possible)                            *)
(* Code is in package:  LPcomplp.m                                                 *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
calculateScalarFunctions[matrixL_, matrixM_, sysVars_ ] :=
Module[{dPrint, vPrint, dStat, vStat, matL = matrixL, matM = matrixM, sVars=sysVars,
  (* CHANGED HEREMAN JUL 2, 2017 need dim later, therefore made global -- should be done differently later *)
  matL2, detL, matM1, detM, tL, tL2, sM, sM1, detVars, (* dim, *) lhsEQ, lhsDetEQ, rhsEQ, 
  rhsDetEQ, matEQ, stQuotRule, ratEQ, ratEQred, ratEQsol, aRat, scalarRuleL, 
  scalarRuleM, ctag="calcST"},


  Clear[a];
  aRat = a;
  
  dStat = getPrintOptsDebug[calcST]; 
  vStat = getPrintOptsVerbose[calcST] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: calculateScalarFunctions ", "File: LPcomplp.m"];
  dPrint[ctag, "######################################################"];
  dPrint[ctag, "Core Matrices: ", convert2Print[matL], " ; ",  convert2Print[matM]];
  dPrint[ctag, "Calculating s,t using determinants (L2)(M)=(M1)(L)"];

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

(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "*********** Determinant equation: Det[L2.M] = Det[M1.L] *************"];
  vPrint[ctag, convert2Print[lhsDetEQ], " = ", convert2Print[rhsDetEQ]];
  vPrint[ctag, "Note: The scalar functions may also be determined using the matrix equation, L2.M - M1.L = 0"];
  vPrint[ctag, MatrixForm[convert2Print[matL2]], ".", 
               MatrixForm[convert2Print[tL2]], ".", MatrixForm[convert2Print[matM]], ".", 
               MatrixForm[convert2Print[sM]] ];
  (* CHANGED HEREMAN JUL 2, 2017 -- should be zero matrix of proper size instead of scalar zero *)
  dim = First[Dimensions[tL]];
  vPrint[ctag, " minus ", MatrixForm[convert2Print[matM1]], ".", 
           MatrixForm[convert2Print[sM1]], ".", MatrixForm[convert2Print[matL]],  
           ".", MatrixForm[convert2Print[tL]], " = ", MatrixForm[IdentityMatrix[dim]-IdentityMatrix[dim]]];
  vPrint[ctag, "Front Face Solutions: ", convert2Print[frontFaceSol] ];
  (* CHANGED HEREMAN JUL 2, 2017 *)
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "***** Defining (Lax) Equation all Terms Combined and Simplified *****"];
  matEQ = Simplify[(lhsEQ - rhsEQ)];
  vPrint[ctag, "Simplified: "];
  vPrint[ctag, MatrixForm[convert2Print[matEQ]] ];

  (* Using the 2,1 entry, compute the corresponding scalar ratio, (s*t2)/(t*s1)   *)
  dPrint[ctag, "Matrix entry: ", 
     convert2Print[ If[Length[ matEQ[[2,1]] ]> 1, matEQ[[2,1]], matEQ[[1,1]]] ]];
  stQuotRule = {s[1] -> (aRat*s1[1]*t[1])/t2[1]};
  ratEQ = Simplify[If[Length[ matEQ[[2,1]] ]> 1, matEQ[[2,1]] /. stQuotRule, matEQ[[1,1]] /. stQuotRule]];
  dPrint[ctag, "In process matrix entry: ", convert2Print[ratEQ]];
  ratEQred = Simplify[ratEQ /. frontFaceSol[[3]] /. frontFaceSol[[2]] /. frontFaceSol[[1]] ];
  dPrint[ctag, "Reduced matrix entry: ", convert2Print[ratEQ]];
  ratEQsol = Simplify[Solve[ratEQred == 0, aRat]];
  dPrint[ctag, "Ratio: ", convert2Print[ratEQsol]];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "****************** Resulting Scalar Function Ratio ******************"];
  vPrint[ctag, "(t2 s)/(t s1) = ", convert2Print[aRat /. ratEQsol] ];

(* vPrint[ctag, "*********************************************************************"]; *) 
 (* CHANGED HEREMAN JUL 2, 2017 -- used a zero matrix of proper size (dim) instead of scalar 0 *)
  vPrint[ctag, "** Defining (Lax) Equation Evaluated on Partial Difference Equations: ** "];
  dim = First[Dimensions[matL]];
  matEQ = Simplify[(lhsEQ - rhsEQ) /. frontFaceSol[[3]] /. frontFaceSol[[2]] /. frontFaceSol[[1]] ];
  vPrint[ctag, MatrixForm[convert2Print[matEQ]], " = ", MatrixForm[IdentityMatrix[dim]-IdentityMatrix[dim]] ];

  If[Length[Variables[Det[tL]]] > 1, 
    (* we have more than one scalar function involved => must use a different approach *)
    dPrint[ctag, "tL Determinant: ", convert2Print[Det[tL]]];

    (* For now, hard code solutions for pKdV system's scalar functions  *)
    scalarL = scalarL /. {t[1] -> n, t[2] -> n};
    scalarM = scalarM /. {s[1] -> m, s[2] -> m}; 
    constantScalar = True;
  , (* else  *)
    (* single scalar function => calculate s,t directly  *)
    dPrint[ctag, "determinant L vars: ", Variables[detL], ", System Vars: ", sVars];
    detVars = Intersection[Variables[detL], sVars];
    dPrint[ctag, "Variables appearing in t: ", convert2Print[detVars]];
    If[Length[detVars] > 0, 
      dim = First[Dimensions[tL]];
      scalarRuleL = {t[1] -> Power[1/detL,1/dim]};
    ,
      (* No variables appear, so our scalar function is an arbitrary constant *)
      scalarRuleL = {t[1]-> m};
      constantScalar = True;
    ];
    scalarL = scalarL /. scalarRuleL;
    dPrint[ctag, "Scalar function t: ", convert2Print[scalarL]];

    detVars = Intersection[Variables[detM], systemVars];
    dPrint[ctag, "Variables appearing in s: ", convert2Print[detVars]];
    If[Length[detVars] > 0, 
      dim = First[Dimensions[sM]];
      scalarRuleM = {s[1] -> Power[1/detM,1/dim]};
    ,
      (* No variables appear, so our scalar function is an arbitrary constant *)
      scalarRuleM = {s[1] -> n};
      constantScalar = True;
    ];
    scalarM = scalarM /. scalarRuleM;
    dPrint[ctag, "Scalar function s: ", convert2Print[scalarM]];
  ];


  dPrint[ctag, "Exit"];
  Return[];
];
(**********************    Module: calculateScalarFunctions    ****************** *)


(**************************    Module: compCoreLaxPair    *********************** *)
(* Purpose:  From the given equations, generate the corresponding equations        *)
(*           on the cube.  I.e.  Assuming the equations specified correspond       *)
(*           to the front face of a cube, rotate and translate these equations     *)
(*           to generate the equations on the remaining 5 faces.                   *)
(* Input:  DDE                                                                     *)
(******************************************************************************* *)
compCoreLaxPair::usage="compCoreLaxPair[ddEquationList] generates each set
 of equations per face of the cube.";
(******************************************************************************* *)
(* Output:  List of DDEs adjusted per face of cube lattice                         *)
(* Code is in package:  LPcomplp.m                                                 *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
compCoreLaxPair[ddeEquationList_] :=
Module[{ddEQ = ddeEquationList, dStat, vStat, dPrint, vPrint,
  matchingDenom, edgeConstraint, sglEdge, dblEdge, solList, lpList,
  varsF, varsLF, varsGF, reduceByOne, currSub13, muNSub13, muDSub13, sDen, sNum,
  scalars, scalarMatL, scalarMatM,
  faceSol, currVar, currVarPos, tmpVar, subRuleList={}, denGCD, dscale,
  allSolutions, denVarCount, subVarCount, v2chk, var3,
        lpLinearRules = {}, compLP = True, subFuncRules = {}, l2mRules={}, num,
        i, den, matrixRow, matrixL, matrixM, matPhi, edgeEQ, constrainedVar,
        flag, tmpList,
  ctag="ccoreLP"},


  dStat = getPrintOptsDebug[ccoreLP]; 
  vStat = getPrintOptsVerbose[ccoreLP] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: compCoreLaxPairs ", "File: LPcomplp.m", ctag];
  dPrint[ctag, "##################################################"];


  lpLinearRules = {f[1] -> mu*f[1], g[1] -> mu*g[1],
                  f[2] -> mu*f[2], g[2] -> mu*g[2],
                  f[3] -> mu*f[3], g[3] -> mu*g[3]};
  subFuncRules = {f[1]->f[1], g[1]->g[1], f[2]->f[2], g[2]->g[2], f[3]->f[3],g[3]->g[3]};
  l2mRules = {x[1,0,0] -> x[0,1,0], y[1,0,0] -> y[0,1,0], z[1,0,0] -> z[0,1,0], p->q, t->s};

  (**** utility functions  ****)
  (* edgeConstraint:  based on the given list of variables of the form _[0,0,1], this *)
  (* determines if there is 1 or no subscripts and if 1, then is the variable _ 3      *)
  (* If all variables are either x or x_ 3, returns True, otherwise False              *)
  edgeConstraint[x_] := And @@ Map[If[Length[Position[# /. {Head[#] -> List},1]]>0 &&
      First[Flatten[Position[# /. {Head[#]-> List},1]]] != 3, False, True]&, x];
  matchingDenom[x_, y_] := If[Length[x]!=0, Denominator[Last[First[x /. subFuncRules]]]==y, False];

  (* To compute the Lax pair, we need solutions from the Front, Bottom and Left faces *)
  (* We will compute the solutions regardless of any work done w/ CAC                 *)
  (* Solving for variables in the original equations (aka Front Face) will indicate   *)
  (* which _ 13 and _ 23 variables can be solved for.                                 *)
(*
  faceVariables = {};
  frontFaceSol = solveFrontFace[frontFaceEQG];
  printCLPCore["D: cLPc, Solutions found (Front Face):\n", convert2Print[frontFaceSol]];
  printCLPCore["D: cLPc, Variable solved for (Front Face):\n", convert2Print[faceVariables]];
  varsFF = faceVariables;
*)
  variableInfo[dStat, lpVariables];
  dPrint[ctag, "Constraints found: ", convert2Print[$edgeConstraints]];  
  $solutionList = ddEQ;

  {sglEdge, dblEdge} = findEdgeEquations[ddEQ];
  dPrint[ctag, "Single Edge: ", convert2Print[sglEdge], "; Double Edge: ", 
             convert2Print[dblEdge]];
   
  {solList, varsF} = solveInitDDE[ddEQ, sglEdge, dblEdge];
  frontFaceSol = solList;
(* vPrint[ctag, "*********************************************************************"]; *)  
   vPrint[ctag, "************************* System Solutions **************************"];
  vPrint[ctag, "Solutions: ", convert2Print[solList]];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "*********************************************************************"];
  allSolutions = solList;

  (* any _ 12 variables found indicate that the corresponding _ 13 & _ 23 will be found *)
  (* these must be considered in the lax pair computation.  Save the list.            *)
  Map[lpVariables[[ vUsed, var2Index[Head[#]] ]] = True; &, Cases[varsF,_[1,1,0]] ];
  flag = False;
  MapThread[
    If[#3,
      dPrint[ctag, #2, " may contribute to Lax pair computation."];
      flag = flag || True;
    ,
      dPrint[ctag, #2, " will not be used in Lax pair computation."];
      flag = flag || False;
    ];
  &, lpVariables];

  (* If any variable can be used in Lax pair calculation, continue, else abort *)
  If[flag, 
    (* Shift the variables found on the front face to get the list of variables         *)
    (* to find on the left face                                                         *)
    varsLF = Complement[varsF /. $latFrnt2LeftRules, varsF];
    dPrint[ctag, "Variables to solve for (Left Face): ", convert2Print[varsLF]];

    faceSol = MapIndexed[Complement[(#1 /. $latFrnt2LeftRules), 
                            solList[[First[#2]]]]&, solList];
    dPrint[ctag, "Solutions found (Left Face): ", convert2Print[faceSol]];
    allSolutions = mergeSolLists[allSolutions, faceSol, dStat];
    dPrint[ctag, "Found so far (Front/Left): ", convert2Print[allSolutions]];
  
    (* Shift the variables found on the front face to get the list of variables    *)
    (* to find on the bottom face                                                  *)
    varsGF = Complement[varsF /. $latFrnt2GrndRules, varsF];
    dPrint[ctag, "Variables to solve for (Bottom Face): ", 
                      convert2Print[varsGF]];

    faceSol = MapIndexed[Complement[(#1 /. $latFrnt2GrndRules), 
                          solList[[First[#2]]]]&, solList];
    dPrint[ctag, "Solutions found (Bottom Face): ", convert2Print[faceSol]];
    allSolutions = mergeSolLists[allSolutions, faceSol, dStat];
(*  vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "******* Solutions found using Front, Left and Bottom faces **********"];
    vPrint[ctag, convert2Print[allSolutions]];

    (* To compute the Lax pair, we need all _ 13 solutions and _ 23 solutions.    *)
    (* We also need any _ 3 solutions that may impose additional constraints on  *)
    (* our substitutions.                                                       *)
    lpList = Union[Part[allSolutions[[3]], Complement[Map[If[Part[#,2]==1,First[#]]&, 
                             Position[allSolutions[[3]],_[1,0,1]]],{Null}]],
                   Part[allSolutions[[3]], Complement[Map[If[Part[#,2]==1,First[#]]&, 
                             Position[allSolutions[[3]],_[0,1,1]]],{Null}]],
                   Part[allSolutions[[2]], Complement[Map[If[Part[#,2]==1,First[#]]&, 
                             Position[allSolutions[[2]],_[0,0,1]]],{Null}]]];

    (* We do not reduce the 2-sub against the 1-sub found.  It's essential the _ 13 and *)
    (* _ 23 vars reference _ 3 vars for our Lax pair calculation to succeed.             *)
 (* vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "*********** Solutions to use in computation of lax pair *************"];
    vPrint[ctag, convert2Print[lpList]];

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
            num = Numerator[currVar[1,0,1] /. lpList] /. {v2chk[0,0,1] -> mu*v2chk[0,0,1]};
            den = Denominator[currVar[1,0,1] /. lpList] /. {v2chk[0,0,1] -> mu*v2chk[0,0,1]};
            If[Exponent[Expand[num], mu] != 0,
              lpVariables[[ vLinear,#1 ]] = (Exponent[Expand[num],mu] <= 1 && 
                                              Exponent[Expand[den],mu] <= 1);
            ,
              lpVariables[[ vLinear,#1 ]] = True;
            ];
            dPrint[ctag, "Variable Linearity (num, den): ",  
                   convert2Print[num], " and ", convert2Print[den] ];
            dPrint[ctag, "mu exponent (num, den): ", 
                   Exponent[Expand[num],mu], " and ", Exponent[Expand[den],mu] ];
            dPrint[ctag, "Variable Linearity (result): ", 
                   lpVariables[[ vLinear, #1 ]] ];
          ];
        &, lpVariables];
      ];
    &, lpVariables];
    variableInfo[dStat, lpVariables];

    flag = False;
    MapThread[
      currVar = #2;
      If[#3,
        If[#4,
          dPrint[ctag, convert2Print[currVar[0,0,1]], " is referenced linearly."];
          flag = flag || True;
        ,
          dPrint[ctag, convert2Print[currVar[0,0,1]], " `` does not appear linearly."];
          flag = flag || False;
        ];
      ,
        dPrint[ctag, convert2Print[currVar[0,0,1]], " `` is not referenced."];
        flag = flag || False;
      ];
    &, lpVariables];
    (* If no variable appears or appears linearly, abort *)
  ];
  
  If[!flag, 
    Throw[$Failed, failTag[$lpNotLinr, compCoreLaxPair]]
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
      edgeEQ = Union[edgeEQ, {# /. $latFrnt2LeftRules}];
    ];
    If[First[Flatten[Position[First[#] /. {Head[First[#]]-> List},1]]]==2, 
      edgeEQ = Union[edgeEQ, {# /. $latFrnt2GrndRules}];
    ];
  &, $edgeConstraints];
  dPrint[ctag, "Edge equations involving _3: ", convert2Print[edgeEQ]];

  (* If any _ 3 variable solutions are found, they must also have only no     *)
  (* subscript variables or _ 3 variables.                                    *)
  Map[
    var3 = Variables[First[#] /. #];
    dPrint[ctag, "Solution: ", convert2Print[#], " contains ",  convert2Print[var3]];
    dPrint[ctag, "Solution: ", convert2Print[#], " is valid: ", edgeConstraint[var3]];
    If[edgeConstraint[var3],
      lpVariables[[ vConst, var2Index[Head[First[#]]] ]] = 
        Head[ var3[[ First[Flatten[Position[var3, _[0,0,1]]]] ]] ];
    ];
  &, edgeEQ];
  variableInfo[dStat, lpVariables];

  MapThread[
    If[#3,
      currVar = #2;
      If[#5 == 0, 
        dPrint[ctag, convert2Print[currVar[0,0,1]], " is unconstrained." ];
      , (* the conditional will not evaluate to False - Mathematica is comparing an unknown to 0 *)
        False;
      ,
        tmpVar = #5;
        dPrint[ctag, convert2Print[currVar[0,0,1]], " is constrained by ", 
                   convert2Print[tmpVar[0,0,1]]];
      ];
    ];
  &, lpVariables];
  
  dPrint[ctag, "Constraining Edge Equations ", convert2Print[edgeEQ]];
  dPrint[ctag, "Substitutions0: ", convert2Print[subRuleList]];

  (* Substitute scalar functions for the _ 3 variables remaining true to the edge constraints *)
  If[Count[lpVariables[[vConst]], 0] < 3,
    (* we have an existing constraints, begin substitution w/ referenced variables *)
    currVar = First[Flatten[Cases[lpVariables[[vConst]], Except[0]]]];
    currVarPos = First[Flatten[Position[lpVariables[[vConst]], currVar]]];
    lpVariables[[ sRule, var2Index[currVar] ]]= {currVar[0,0,1] -> f[1]/g[1]};
    subRuleList = {currVar[0,0,1] -> f[1]/g[1]};
    dPrint[ctag, "Substitutions1: ", convert2Print[subRuleList]];

    lpVariables[[ vSub, var2Index[currVar] ]] = 1;
    subRuleList = Union[subRuleList, Factor[edgeEQ /. subRuleList]];
    dPrint[ctag, "Substitutions2: ", convert2Print[subRuleList]];

    lpVariables[[ sRule, currVarPos ]]= Factor[edgeEQ /. lpVariables[[ sRule, var2Index[currVar] ]] ];
    lpVariables[[ vSub,currVarPos ]] = -1;
    currVar = lpVariables[[ vName,First[Flatten[Position[lpVariables[[vSub]], 0]]] ]];
    lpVariables[[ sRule, var2Index[currVar] ]]= {currVar[0,0,1] -> f[2]/g[2]};
    subRuleList = Union[subRuleList, {currVar[0,0,1] -> f[2]/g[2]} ];
    lpVariables[[ vSub,First[Flatten[Position[lpVariables[[vSub]], 0]]] ]] = 2;
    dPrint[ctag, "Substitutions3: ", convert2Print[subRuleList]];

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
  dPrint[ctag, "Final substitutions: ", convert2Print[subRuleList]];
  variableInfo[dStat, lpVariables];

  (* With the substitutions calculated, form x13, y13 and z13 for subsequent computations     *)
  MapThread[
    If[#3,
      currVar = #2;
      tmpVar = {Factor[Flatten[(currVar[1,0,1] /. solList) /. subRuleList]]};
      dPrint[ctag, "Saving subbed variable, ", currVar, " = ", 
                              convert2Print[tmpVar], "."];
      lpVariables[[ subs, #1 ]] = Factor[Flatten[(currVar[1,0,1] /. lpList) /. subRuleList]];
    ];
  &, lpVariables];
  variableInfo[dStat, lpVariables];


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
        dPrint[ctag, "Subbed with f,g: ", currVar, " : ", convert2Print[currSub13]];
        muNSub13 = Numerator[currSub13] /. lpLinearRules /. subFuncRules;
        muDSub13 = Denominator[currSub13] /. lpLinearRules /. subFuncRules;
        dPrint[ctag, "Subbed with f,g scaled: ", currVar, " : ",  
                   convert2Print[muNSub13], " over ", convert2Print[muDSub13]];
        If[Exponent[Expand[muNSub13],mu] > 1 || Exponent[Expand[muDSub13],mu] > 1,
          (* The equation is not linear in the subbed parameters *)
          reduceByOne = True;
          dPrint[ctag, "Not linear in f and g "];
        ,
          dPrint[ctag, "Linear in f and g "];
        ];
      ];
    &, lpVariables]; 

    dPrint[ctag, "ReduceByOne: ", reduceByOne];
    If[reduceByOne,
      (* remove the freedom of a denominator and repeat the check *)
      dPrint[ctag, "Reducing the number of denominators referenced by 1"];
      If[denVarCount < 2,
        (* no more denominators to remove, so we can only conclude no lax pair possible *)
        dPrint[ctag, "No additional denominators available, no lax pair."];
        laxPairFound = False;

        Throw[$Failed, failTag[$lpNoLnSub,compCoreLaxPair]]
      ,
        subFuncRules = Complement[subFuncRules, {g[denVarCount]->g[denVarCount]}];
        subFuncRules = Union[subFuncRules, {g[denVarCount]->g[denVarCount-1]}];
        dPrint[ctag, "Substitution rules: ", convert2Print[subFuncRules]];
        denVarCount--;
        compLP = True;
      ];
    ];
    dPrint[ctag, "compLP: ", compLP];
  ];
  dPrint[ctag, "Variables subbed = ", subVarCount, 
               " and denominators needed = ", denVarCount];

(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "********************* Resulting substitutions ***********************"];
  vPrint[ctag, convert2Print[Flatten[ lpVariables[[sRule]] ] /. subFuncRules]];

  variableInfo[dStat, lpVariables];

  i=0;
  MapThread[
    rule = #7 /. subFuncRules;
    dPrint[ctag, "Denominator: ", convert2Print[rule] ];
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
      variableInfo[dStat, lpVariables];
    ];
  &, lpVariables];

  dPrint[ctag, "Lax pair substitions found?: ``", laxPairFound ];

  (* Now that we finally have the proper sub rules, use them to derive the lax pair *)
  dPrint[ctag, "Beginning computation of Lax pair."];
  matrixL = {};
  Map[
    dPrint[ctag, "Sub order: ", #1];  
    j = Flatten[Position[lpVariables[[vSub]], #1]] /. List->Sequence;
    If[j>0,
      dPrint[ctag, "Extracting information for ", 
                     convert2Print[ Transpose[lpVariables][[j]] ]];  
      matrixRow={};
      If[lpVariables[[vUsed,j]] && lpVariables[[vLinear,j]],
        currVar = lpVariables[[vName,j]];
        currSub13 = Factor[lpVariables[[ subs,j ]] /. subFuncRules];
        num = Numerator[currSub13];
        dPrint[ctag, "Pick: ", #1, " gives ",
           convert2Print[Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,j]] ]]];

        denGCD = PolynomialGCD[Map[Denominator,
                  Simplify[Flatten[ Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,j]]] ]
                            /. subFuncRules] ] /. List -> Sequence];
        dPrint[ctag, "Denominator GCD of sub vars: ", convert2Print[denGCD]];
        dscale = Simplify[Denominator[currSub13]/denGCD];
        dPrint[ctag, "Current Variable: ", convert2Print[currVar[1,0,1]], " = ", 
                    convert2Print[currSub13] ];
        dPrint[ctag, "with numerator: ", convert2Print[num] ];
        dPrint[ctag, " and scaling factor: ", convert2Print[dscale] ];

        For[i=1, i <= subVarCount, i++, 
          dPrint[ctag, "Coefficient1 of ", convert2Print[f[i]], " : ", 
                           convert2Print[Coefficient[num, f[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[num/dscale, f[i]]] ];
        ];
        For[i=1, i <= denVarCount, i++, 
          dPrint[ctag, "Coefficient2 of ", convert2Print[g[i]], " : ",
                           convert2Print[Coefficient[num, g[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[num/dscale, g[i]]] ];
        ];
        dPrint[ctag, " row: ", convert2Print[matrixRow] ];
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
        dPrint[ctag, "Pick: ", #1, " gives ",  
           convert2Print[Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,#1]] ]]];
        denGCD = PolynomialGCD[Map[Denominator,
                  Simplify[Flatten[ Pick[lpVariables[[subs]], lpVariables[[cmnDen]], lpVariables[[cmnDen,#1]]] ]
                            /. subFuncRules] ] /. List -> Sequence];
        dscale = Simplify[Denominator[currSub13]/denGCD];
        dPrint[ctag, "Current Variable: ", convert2Print[currVar[1,0,1]], " = ",
            convert2Print[currSub13] ];
        dPrint[ctag, " with denominator: ", convert2Print[den] ];
        dPrint[ctag, " and scaling factor: ", convert2Print[dscale] ];
        For[i=1, i <= subVarCount, i++, 
          dPrint[ctag, "Coefficient of ", convert2Print[f[i]], " : ",
                          convert2Print[Coefficient[den, f[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[den/dscale, f[i]]] ];
        ];
        For[i=1, i <= denVarCount, i++, 
          dPrint[ctag, " Coefficient of ", convert2Print[g[i]], " : ", 
                          convert2Print[Coefficient[den, g[i]]] ];
          matrixRow = Append[matrixRow, Simplify[Coefficient[den/dscale, g[i]]] ];
        ];
        dPrint[ctag, " row: ", convert2Print[matrixRow] ];
        matrixL = Append[matrixL, matrixRow];
      ];
    ];
  &, lpVariables]; 

  (* define the corresponding matrix of scalar functions  *)
  sDen = Table[t[i],{i,1,denVarCount}];
  sNum = Table[t[i],{i,1,denVarCount}];
  dPrint[ctag, "Scalars (den, num): ", convert2Print[sDen], " , ", convert2Print[sNum]];
  dPrint[ctag, "Counts: ", subVarCount, " , ", denVarCount];
  If[subVarCount > denVarCount, 
    sNum = PadRight[sNum, subVarCount, t[denVarCount]];
  ];
  scalars = Join[sNum, sDen];
  scalarMatL = DiagonalMatrix[scalars];
  dPrint[ctag, "Scalar matrix: ", MatrixForm[convert2Print[scalarMatL]] ];

  matPhi = Join[Table[f[i],{i,1,subVarCount}], Table[g[i],{i,1,denVarCount}]];
  dPrint[ctag, " phi:  ", matPhi];
  matrixPhi = matPhi;

  dPrint[ctag, "Matrix L: ", MatrixForm[convert2Print[matrixL]] ];
  coreLPL = matrixL;
  scalarL = scalarMatL;

  matrixM = matrixL /. l2mRules;
  scalarMatM = scalarMatL /. l2mRules;
  dPrint[ctag, "Matrix M: ", MatrixForm[convert2Print[matrixM]] ];
  coreLPM = matrixM;
  scalarM = scalarMatM;

  dPrint[ctag, "Exit"];
  Return[];
];
(**************************    Module: compCoreLaxPair    *********************** *)


(**************************    Module: computeLaxPairs    *********************** *)
(* Purpose:  From the given equations, generate the corresponding equations        *)
(*           on the cube.  I.e.  Assuming the equations specified correspond       *)
(*           to the front face of a cube, rotate and translate these equations     *)
(*           to generate the equations on the remaining 5 faces.                   *)
(* Input:  DDE                                                                     *)
(******************************************************************************* *)
computeLaxPairs::usage="computeLaxPairs[ddEquationList, ushift, pEquivs] generates 
 each set of equations per face of the cube.";
(******************************************************************************* *)
(* Output:  List of DDEs adjusted per face of cube lattice                         *)
(* Code is in package:  LPcomplp.m                                                 *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
computeLaxPairs[ddeEquationList_, ushift_, pEquivs_] :=
Module[{ddEQ = ddeEquationList, dStat, vStat, dPrint, vPrint,
  compLP, coreL, coreM, numScalars, verifyLP, sUnks, sPars, sVars, sUDefs, dsubVars,
  ctag="cmpLP"},

  dStat = getPrintOptsDebug[cmpLP]; 
  vStat = getPrintOptsVerbose[cmpLP] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: computeLaxPairs ", "File: LPcomplp.m"];
  dPrint[ctag, "##########################################"];

  {sUnks, sPars, sVars, sUDefs, dsubVars} = getVariableInfo[ddEQ, dStat];
  dPrint[ctag, "Compute the Core L,M matrices"];
  catchInternalError[compCoreLaxPair[ddEQ], computeLaxPairs, failTag];

  If[ Length[coreLPL]>0,
(*  vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "************************* Candidate Lax pair ************************"];
    vPrint[ctag, "With respect to the matrix Phi: ", 
               MatrixForm[convert2Print[matrixPhi]]];
    numScalars = Length[Complement[Union[Flatten[scalarL]], {0}]];
    vPrint[ctag, "We have the following candidate Lax pair (with ", numScalars*2, " unresolved scalar functions):"];

    gcdL = rationalPolyGCD[coreLPL];
    gcdsL = PolynomialGCD[Flatten[scalarL] /. List->Sequence];
    dPrint[ctag, "gcdL ", convert2Print[gcdL], " gcdsL ", convert2Print[gcdsL]];
    If[gcdL*gcdsL != 1,
      coreL = Cancel[coreLPL/gcdL].Cancel[scalarL/gcdsL];
      vPrint[ctag, "Candidate L: ", convert2Print[gcdL*gcdsL], " times", MatrixForm[convert2Print[coreL]]];
    , (* else *)
      coreL = coreLPL.scalarL;
      vPrint[ctag, "Candidate L: ", MatrixForm[convert2Print[coreL]]];
    , (* same behavior as if not equal to 1 *)
      coreL = Cancel[coreLPL/gcdL].Cancel[scalarL/gcdsL];
      vPrint[ctag, "Candidate L: ", convert2Print[gcdL*gcdsL], " times ", MatrixForm[convert2Print[coreL]]];
    ];
    
    gcdM = rationalPolyGCD[Flatten[coreLPM]];
    gcdtM = PolynomialGCD[Flatten[scalarM] /. List->Sequence];
    dPrint[ctag, "gcdM ", convert2Print[gcdM], ", gcdsM ", convert2Print[gcdtM]];
    If[gcdM*gcdtM != 1,
      coreM = Cancel[coreLPM/gcdM].Cancel[scalarM/gcdtM];
      vPrint[ctag, "Candidate M: ", 
        convert2Print[gcdM*gcdtM], " times ", MatrixForm[convert2Print[coreM]]];
    , (* else *)
      coreM = coreLPM.scalarM;
      vPrint[ctag, "Candidate M: ", MatrixForm[convert2Print[coreM]]];
    , (* same behavior as if not equal to 1 *)
      coreM = Cancel[coreLPM/gcdM].Cancel[scalarM/gcdtM];
      vPrint[ctag, "Candidate M: ", 
               convert2Print[gcdM*gcdtM], " times ", MatrixForm[convert2Print[coreM]]];
    ];
    (* vPrint[ctag, "*********************************************************************"]; *) 
       vPrint[ctag, "*********************************************************************"];
  ];

  If[ Length[coreLPL]>0,
 (* vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "**************** Calculating possible scalar functions **************"];
    constantScalar = False;
    (* correction HEREMAN JUL 31, 2017 Ask TB: calculateScalarFunctions does not seem to return anything *)
    scalarFuncs = calculateScalarFunctions[coreL, coreM, sVars];
 (* vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "********************* Computed Scalar Functions *********************"];
    If[numScalars > 1, 
      vPrint[ctag, "The calculated scalar functions (in matrix form) are as follows: "];
      vPrint[ctag, "Associated with matrix L: ", 
                 MatrixForm[convert2Print[scalarL]]];
      vPrint[ctag, "Associated with matrix M: ", 
                 MatrixForm[convert2Print[scalarM]]];
    , (* else *)
      vPrint[ctag, "The calculated scalar functions are as follows:"];
      vPrint[ctag, "Associated with matrix L: ", convert2Print[gcdL*gcdsL], 
               " = ", MatrixForm[convert2Print[Complement[Union[Flatten[scalarL]], {0}]]]];
      vPrint[ctag, "Associated with matrix M: ", convert2Print[gcdM*gcdtM], 
               " = ", MatrixForm[convert2Print[Complement[Union[Flatten[scalarM]], {0}]]]];
    ];
    If[constantScalar, 
      vPrint[ctag, "where m, n are arbitrary functions of the parameters p, q, k"];
    ];
(*  vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "*********************************************************************"];
    vPrint[ctag, "************************ Candidate Lax pair  ************************"];
    vPrint[ctag, "With respect to the matrix Phi: ", 
                          MatrixForm[convert2Print[matrixPhi]]];
    vPrint[ctag, "We have the following candidate Lax pair:"];

    If[numScalars > 1, 
      vPrint[ctag, "Candidate L: ", 
          MatrixForm[convert2Print[coreLPL]], " ", 
          MatrixForm[convert2Print[scalarL]]];
      vPrint[ctag, "Candidate M: ", 
          MatrixForm[convert2Print[coreLPM]], " ", 
          MatrixForm[convert2Print[scalarM]]];
    ,
      vPrint[ctag, "Candidate L: ", 
          MatrixForm[convert2Print[Complement[Union[Flatten[scalarL]], {0}]]], " ",
          MatrixForm[convert2Print[coreLPL]]];
      vPrint[ctag, "Candidate M: ", 
          MatrixForm[convert2Print[Complement[Union[Flatten[scalarM]], {0}]]], " ",
          MatrixForm[convert2Print[coreLPM]]];
      ];
 (* vPrint[ctag, "*********************************************************************"]; *)   
    vPrint[ctag, "*********************************************************************"];
  ];

  If[ Length[coreLPL]>0,
    (* Reset the global variables used in the CAC check workflow                *)
    $solutionList = {};
    (* varsFound = {};  *)
 (* vPrint[ctag, "*********************************************************************"]; *)
    vPrint[ctag, "****************** Verifying the Computed Lax pair ******************"];
    (* HEREMAN, JUL 26, 2017 was: *)
    (* verifyLP = verifyLaxPairs[ddEQ, coreLPL, coreLPM, scalarL, scalarM, ushift, pEquivs]; *)
    (* correction ASK TB: ushift vs uShifts and pEquivs vs pEquiv ??? *)
    (* left unchanged for now: *)
    (* also: added Evaluate to assure that returned value binds to verifyLP *)
    verifyLP = Evaluate[verifyLaxPairs[ddEQ, coreLPL, coreLPM, scalarL, scalarM, ushift, pEquivs]];
    (* verifyLP = verifyCalcLP[ddEQ]; *)
    If[verifyLP, 
    vPrint[ctag, "* Computed Lax Pair Satisfies the Defining Equation L2.M - M1.L = 0 *"];
    , (* else *)
   (* vPrint[ctag, "*********************************************************************"]; *) 
      vPrint[ctag, "**** Lax Pair Does Not Satisfy Defining Equation L2.M - M1.L = 0 ****"];
      vPrint[ctag, "***************** Calculated Lax Pair is Not Valid ******************"];
    ];
(*  vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "*********************************************************************"];
  ];

  dPrint[ctag, "Exit"];
  Return[];
];
(**************************    Module: computeLaxPairs    *********************** *)


(**************************    Function: compLPDriver    ************************ *)
(* Purpose:  From the given equations, generate the corresponding equations        *)
(*           on the cube.  I.e.  Assuming the equations specified correspond       *)
(*           to the front face of a cube, rotate and translate these equations     *)
(*           to generate the equations on the remaining 5 faces.                   *)
(* Input:  DDE                                                                     *)
(******************************************************************************* *)
compLPDriver::usage="compLPDriver[ddEquationList, dblsub, uShift, pEquivs]
 generates each set of equations per face of the cube.";
(******************************************************************************* *)
(* Output:  List of DDEs adjusted per face of cube lattice                         *)
(* Code is in package:  LPcomplp.m                                                 *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
compLPDriver[ddeEquationList_, dblsub_, uShift_, pEquivs_] :=
Module[{ddEQ = ddeEquationList, dStat, vStat, dPrint, vPrint,
  compLP, coreL, coreM, numScalars, eqLen, 
  dblSubList= {x[1,1,0], y[1,1,0], z[1,1,0]},
  systemUnks, systemPars, systemUDefs, dblSubSystemVars, 
  ctag="cLPDriver"},


  dStat = getPrintOptsDebug[cLPDriver]; 
  vStat = getPrintOptsVerbose[cLPDriver] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: compLPDriver, ", "File: LPcomplp.m", ctag];
  dPrint[ctag, "#######################################", ctag];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "************************* Compute a Lax pair ************************"];
  (* CHANGED HEREMAN JUL 2, 2017 *)
  vPrint[ctag, "Partial difference equation(s): ", convert2Print[ Map[#==0 &, ddEQ] ]]; 
  dPrint[ctag, "# of equations: ", eqLen=Length[ddEQ]];

  If[Length[Intersection[dblsub]] == 0, 
    Throw[$Failed, failTag[$lpErrNo12, compLPDriver]]
  ];

  (* Check for any single-edge equations.  These will only reference x, x1 or x2 *)
  (* First determine what 2-sub variables are being used so we can eliminate     *)
  (* any equations that reference these from consideration.                      *)
  dPrint[ctag, "Checking equations for the dbl-sub variable: ", 
          convert2Print[dblsub]];

  (* Record which _ 12 variables appear.  Further computation will need this info *)
  Map[lpVariables[[ vUsed, var2Index[Head[#]] ]] = True; &, dblsub ];
  variableInfo[dStat, lpVariables];
  
   (* Reset the global variables used in the CAC check workflow                *)
  $solutionList = {};
  $edgeConstraints = {};
  coreLPL = {};
  scalarL = {};
  coreLPM = {};
  scalarM = {};
  matrixPhi = {};

  computeLaxPairs[ddEQ, uShift, pEquivs];

  dPrint[ctag, "Exit"];
  Return[];
];
(**************************    Function: compLPDriver    ************************ *)


(***************************    Module: verifyVar123    ****************************)
(* Input:  List of solutions found from the rotational faces which should include   *)
(*         all 2-sub var solutions and 3-sub var solutions (if any) found.          *)
(*         Solve for 3-sub and check consistency.                                   *)
(************************************************************************************)
verifyVar123::usage="verifyVar123[solList, faceEQList] checks the specified 
 face equations for consistency against the provided solution list."; 
(************************************************************************************)
(* Output:  List of solutions for the 3-sub variables if consistent                 *)
(*          Null list if inconsistent                                               *)
(* Code is in package:  LPcomplp.m                                                  *)
(* Last Modified:                                                                   *)
(************************************************************************************)
verifyVar123[solutionList_, faceEquList_, vFnd_] :=
Module[{faceEQ = faceEquList, solList = solutionList, varsFound=vFnd,
  cVar, linEQ, equRef, msg, varSol, dStat, cFace, faceVar, redSol3List,
  sol3List={}, print123, eqUnk, eqVars, vars2Solve, ctag="ver123"},

  dStat = getPrintOptsDebug[ver123]; 
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: verifyVar123, Package: LPcac.m"];
  dPrint[ctag, "#####################################################"];
  dPrint[ctag, "Face: ", convert2Print[faceEQ]];
  dPrint[ctag, "Solutions: ", convert2Print[solList]];

  cFace = Sort[faceEQ, LeafCount[#1]<LeafCount[#2]&];
  dPrint[ctag, "Sorted Face Equations: ", convert2Print[cFace]];
  dPrint[ctag, "Solutions Found: ", convert2Print[solList]];   

  eqUnk = Variables[faceEQ];
  eqVars = Sort[Intersection[eqUnk, $allowedVars], Count[#1,1]<Count[#2,1]&];
  dPrint[ctag, "Using variables: ", convert2Print[eqVars]];

  (* Since the form may describe several variables, loop until depleted   *)
  faceVar = eqVars;
  While[Length[Cases[faceVar, _[1,1,1]]]>0,
    cVar = First[Cases[faceVar, _[1,1,1]]];
    dPrint[ctag, "Current variable: ", convert2Print[cVar]];

    (* we can only solve for linear variables, check the exponents        *)
    linEQ = Select[cFace, Exponent[#,cVar]==1&];
    If[Length[linEQ]>0,
      (* sort the equations based on LeafCount and then we only           *)
      (* need to grab the 1st equ referenced to have simplest.            *)
      linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
      equRef = linEQ[[First[First[Position[linEQ, cVar]]]]];
      dPrint["Current variable: ", convert2Print[cVar], 
             " ; Current equation: ",  convert2Print[equRef]];

      varSol = Flatten[Solve[equRef==0, cVar]];
      dPrint[ctag, "Current variable solution: ", convert2Print[varSol]];
      cFace = Complement[Factor[cFace /. varSol],{0}];
      If[Length[sol3List]>0, 
        sol3List = MapAll[Factor, sol3List /. varSol];
      ];
      sol3List = Union[sol3List, varSol];
      varsFound = Union[varsFound, {cVar}];
      dPrint[ctag, "Variables Found: ", convert2Print[varsFound]];
      dPrint[ctag, "Solutions Found: ", convert2Print[sol3List]];

    ];(* If ** no linear references for current variable, move on.          *)
    faceVar = Complement[faceVar, {cVar}];
  ];  (* While *)

  dPrint[ctag, "Solution Set for this Face: ", convert2Print[sol3List]];
  (* Once we have found solutions for any 3sub vars, simplify the solutions *)
  (* using the 2sub and 0sub solutions we have found                        *) 
  (* First determine, of the variable solutions found, which are used in    *)
  (* the 3sub solutions we currently have                                   *)
(*  vars = Union[Flatten[Map[Intersection[Variables[First[#] /. #], varsFound]&, sol3List]]]; *)

  (* Of these, remove any except the 2sub solutions                         *)
(***
  vars2Sub = Select[vars, Count[# /. Head[#] -> List, 1] > 1&];
  print123["D: ver3:  Variables to sub:"];
  print123[convert2Print[vars2Sub]]; 

  sol2Use = Map[solList[[ First[Flatten[Position[solList, #]]] ]] &, vars2Sub];
  print123[convert2Print[sol2Use]]; 
  redSol3List = Factor[sol3List /. sol2Use];
***)
  redSol3List = Factor[sol3List /. solList[[3]] ];

  dPrint[ctag, "Reduced: ", convert2Print[redSol3List]]; 
  dPrint[ctag, "Exit"];
  Return[{redSol3List, varsFound}];
];
(**************************    Module: verifyVar123    *****************************)


(**************************   Module: solveFaceEqu    **************************** *)
(* solveFaceEqu[faceEquList]                                                        *)
(* Input: Subset List of equations from the original cube DDE's which correspond to *)
(*        the specified face (Fr,Bt,Le,Ri,Bk,Tp) list of variables to solve for,    *)
(*        if null, let the code decide                                              *)
(* Output:  List of rules describing solutions for the variables found in that      *)
(*          face's equations.  Also, a list of variables solved for in that face    *)
(************************************************************************************)
solveFaceEqu::usage="solveFaceEqu[faceEquList, varsToFind, varsFound] solves 
 the specified equation for the variables specified and then solves the equ 
 for any variables not already found."; 
(************************************************************************************)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPcomplp.m                                                  *)
(* Last Modified:                                                                   *)
(************************************************************************************)
solveFaceEqu[faceEquList_, varsToFind_, varsFound_] :=
 Module[{faceEQ = faceEquList, vFind = varsToFind, vFound=varsFound,
         eqUnk, eqVars, vars, vars2Solve, idx=0, vPrint, dPrint, solListM,
         solListF, solList, cVar, linEQ, equRef, varSol, faceVar, vUsed,
         newVarsFound, dStat, vStat,
         ctag="solFace"},


  dStat = getPrintOptsDebug[solFace]; 
  vStat = getPrintOptsVerbose[solFace] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: solveFaceEqu, Package: LPcac.m"];
  dPrint[ctag, "#####################################################"];
  dPrint[ctag, "DDE: ", convert2Print[faceEQ]];

  vFind = Sort[vFind, Count[#1,1]>Count[#2,1]&];
  dPrint[ctag, "Variables to solve for first: ", convert2Print[vFind]];

  (* Using the list provided, find all occurrences of a variable, sort these by  *)
  (* leafcount, solve for that variable, record, and continue with the list      *)
  solListM = {};
  Map[
    cVar = #;
    idx = Count[cVar,1]+1;

    dPrint[ctag, "Current variable: ", convert2Print[cVar]];

    linEQ = Select[faceEQ, Exponent[#,cVar]==1&];
    If[Length[linEQ]>0,
      (* sort the matching equations based on LeafCount and then we only         *)
      (* need to grab the 1st equ referenced to have simplest.                   *)
      linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
      equRef = Factor[linEQ[[First[First[Position[linEQ, cVar]]]]]];
      dPrint[ctag, "Simplest eq ref current var: ", convert2Print[equRef]];

      varSol = Flatten[Solve[equRef==0, cVar]];
      (* if any prior solutions exist, use the newly found solution to      *)
      (* reduce them.  IMPORTANT:  Only reduce those solutions involving    *)
      (* one subscript more.  ie. x1 only w/ x1, x12 w/ x12, x123 w/ x123   *)
      If[Length[ Flatten[solListM] ]>0, 
        If[Length[ solListM[[idx]] ]>0,
          solListM[[idx]] = MapAll[Factor, solListM[[idx]] /. varSol];
          solListM[[idx]] = Union[solListM[[idx]], varSol];
        , (* else *)
          solListM[[idx]] = varSol;
        ];
      , (* else *)
        solListM = {{},{},{}};
        solListM[[idx]] = varSol;
      ];
      faceEQ = Complement[Simplify[faceEQ /. varSol],{0}];
    ];
  &, vFind];
  dPrint[ctag, "Mandatory solutions found: ", convert2Print[solListM]]; 

  (* For any remaining equations, solve for whatever variables appear           *)
  eqUnk = Variables[faceEQ];
  eqVars = Intersection[eqUnk, $allowedVars];
  vars = Complement[eqVars, vFound, vFind];
  
  (* sort lexicographically and by count of 1's *)
  faceVar = Join[Sort[Select[vars, Count[#,1]==2 &]], Sort[Select[vars,Count[#,1]==1 &]],
                        Sort[Select[vars, Count[#,1]==0&]]];
  (* If any of the variables referenced are unusable, remove the _ 3 references  *)
  (* The _ 3 solutions are potentially critical to computation of Lax pair       *)
  MapThread[
    vUsed = #3;
    cVar = #2;
    If[Not[vUsed], Complement[faceVar, {cVar[1,0,0]}]];
  &, $lpVariables];

  dPrint[ctag, "Allowable variables to solve for beyond mandatory: ", 
                            convert2Print[faceVar]];

  newVarsFound = {};
  solListF = {};
  Map[
    cVar = #;
    idx = Count[cVar, 1]+1;
    dPrint[ctag, "Current variable: ", convert2Print[cVar]];

    linEQ = Select[faceEQ, Exponent[#,cVar]==1&];
    If[Length[linEQ]>0,
      (* sort the matching equations based on LeafCount and then we only         *)
      (* need to grab the 1st equ referenced to have simplest.                   *)
      linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
      equRef = Factor[linEQ[[First[First[Position[linEQ, cVar]]]]]];
      dPrint[ctag, "Current equation: ", convert2Print[equRef]];

      varSol = Flatten[Solve[equRef==0, cVar]];
      (* if any prior solutions exist, use the newly found solution to      *)
      (* reduce them.  IMPORTANT:  Only reduce those solutions involving    *)
      (* one subscript more.  ie. x1 only w/ x1, x12 w/ x12, x123 w/ x123   *)
      If[Length[ Flatten[solListF] ]>0, 
        If[Length[ solListF[[idx]] ]>0,
          solListF[[idx]] = MapAll[Factor, solListF[[idx]] /. varSol];
          solListF[[idx]] = Union[solListF[[idx]], varSol];
        , (* else *)
          solListF[[idx]] = varSol;
        ];
      , (* else *)
        solListF = {{},{},{}};
        solListF[[idx]] = varSol;
      ];
      newVarsFound = Union[newVarsFound, {cVar}];
      faceEQ = Complement[Simplify[faceEQ /. varSol],{0}];
    ];
  &, faceVar];
  dPrint[ctag, "Arbitrary solutions found: ", convert2Print[solListF]]; 

  (* combine mandatory & arbitrary lists into a list of sols sorted by var indices *)
  solList = mergeSolLists[solListM, solListF, dStat];
  dPrint[ctag, "Solution Set for this Face: ", convert2Print[solList]]; 
  newVarsFound = Union[newVarsFound, vFind];
  dPrint[ctag, "Variables Solved for: ", convert2Print[vFind]];

  vFound = Union[vFound, newVarsFound];
(*  faceVariables = varsFound;  *)

  dPrint[ctag, "Return list: ", convert2Print[solList]];
  dPrint[ctag, "Return vars: ", convert2Print[vFound]];
  dPrint[ctag, "Exit"];

  Return[{solList, vFound}];
]; 
(**************************   Module: solveFaceEqu    **************************** *)


(*****************************   Module: verifyCAC    **************************** *)
(* verifyCAC[ddeEquationList]                                                       *)
(* Purpose:  verifies that the specified DDE is consistent around the cube per the   *)
(*           ABS classificaiton.                                                    *)
(* Input:    DDE                                                                    *)
(* Output:   List of rules describing the solutions for all variables,              *)
(*           2-sub and 3-sub primarily, 0-sub and 1-sub if necessary.               *)
(******************************************************************************** *)
verifyCAC::usage="verifyCAC[ddeEquationList, userShifts, paramEquiv] Using the 
 specified list of equations, systematically solves for the 3-sub variables, if
 possible, via different routes and verifies that all solutions are consistent.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPcomplp.m                                                  *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
verifyCAC[ddeEquationList_, uShift_, pEquiv_] :=
Module[{ddEQ = ddeEquationList, solList, eqUnk, eqVars, dStat, vStat, msg,
   faceEqs, sub3List, frontFaceSols, varsFound, vars2Find, dPrint, vPrint,
   varsF, faceSol, allSolutions, v123Fnd, faceSolR, faceSolB, faceSolT, faceSolT123,
   solution1, solution2, solDiff, cacTest,
   sglEdge={}, dblEdge={}, ctag="verCAC"},


  dStat = getPrintOptsDebug[verCAC]; 
  vStat = getPrintOptsVerbose[verCAC] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: verifyCAC ", "Package: LPcac.m"];
  dPrint[ctag, "#####################################################"];
  dPrint[ctag, "DDE: ", convert2Print[ddEQ]];
  dPrint[ctag, "userShifts: ", uShift, " parameter Rules: ", pEquiv];

  (* Beginning with the original system (aka front face), solve for the variables *)
  (* referenced, eliminating the corresponding equation used as we go. Progress   *)
  (* through the faces solving for variables, reducing previously found           *)
  (* solutions, until all necessary variables (for either CAC, tetrahedron prop   *)
  (* or finding Lax pair)                                                         *) 

  {sglEdge, dblEdge} = findEdgeEquations[ddEQ];
  dPrint[ctag, "Single Edge: ", convert2Print[sglEdge], "; Double Edge: ", 
             convert2Print[dblEdge]];
   
  {solList, varsF} = solveInitDDE[ddEQ, sglEdge, dblEdge];
(* vPrint[ctag, "*********************************************************************"]; *)  
  vPrint[ctag, "************************** System Solutions **************************"];
  vPrint[ctag, "Solutions: "];
  vPrint[ctag, convert2Print[solList]];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "*********************************************************************"];
  allSolutions = solList;
  dPrint[ctag, "Variables Found: ``", convert2Print[varsF]];

  (* Using the variables found the initial system, calculate the variables we     *)
  (* will find on the shifted system but remove any redundancy                    *)
  vars2Find = Complement[(varsF /. latFrnt2LeftRules), varsF];
  dPrint[ctag, "Variables to solve for: ", convert2Print[vars2Find]];

  (*** Left Face   ***)
  faceEqs = ddEQ /. latFrnt2LeftRules;
  dPrint[ctag, "Left Face: ", convert2Print[faceEqs]];
  {faceSol, varsF} = solveFaceEqu[faceEqs, vars2Find, varsF];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "************************ Left Face Solutions ************************"];
  vPrint[ctag, "Solutions: ``", convert2Print[faceSol]];
 (* vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "*********************************************************************"];
  dPrint[ctag, "Variables found: ``", convert2Print[varsF]];

  allSolutions = mergeSolLists[allSolutions, faceSol, dStat];
  dPrint[ctag, "Solutions found thus far: ", convert2Print[allSolutions]];

  (* Using the variables found on the Front Face, calculate the variables we     *)
  (* will find on the bottom face but remove any redundancy                        *)
  vars2Find = Complement[(varsF /. latFrnt2GrndRules), varsF];
  dPrint[ctag, "Variables to solve for in Bottom Face: ", convert2Print[vars2Find]];

  (*** Bottom Face   ***)
  faceEqs = ddEQ /. latFrnt2GrndRules;
  dPrint[ctag, "Bottom Face: ", convert2Print[faceEqs]];
  (* faceSol = MapIndexed[Complement[(#1 /. latFrnt2GrndRules), solutionListG[[First[#2]]]]&, frontFaceSols]; *)
  (* faceSol = solveFaceEqu[groundFaceEQG, vars2Find];  *)

  {faceSol, varsF} = solveFaceEqu[faceEqs, vars2Find, varsF];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "********************** Bottom Face Solutions ************************"];
  vPrint[ctag, "Solutions: ", convert2Print[faceSol]];
(* vPrint[ctag, "*********************************************************************"]; *)  
   vPrint[ctag, "*********************************************************************"];
  dPrint[ctag, "Variables found: ", convert2Print[varsF]];

  allSolutions = mergeSolLists[allSolutions, faceSol, dStat];
  dPrint[ctag, "Front/Left/Bottom Face solutions: ", convert2Print[allSolutions]];

  (* Using the translational faces, solve the 3-sub and simplify using the solutions *)
  (* found above, primarily the 2-sub and 0-sub (1-sub if necessary)                 *)
  v123Fnd = {};
  faceEqs = $rightFaceEQ;
  dPrint[ctag, "Computing 3-sub solutions using Right Face:"];
  {faceSolR, v123Fnd} = verifyVar123[allSolutions, $rightFaceEQ, v123Fnd];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "********************* Right Face 123 Solutions **********************"];
   vPrint[ctag, convert2Print[faceSolR]];
   vPrint[ctag, "*********************************************************************"];
   faceEqs = $backFaceEQ;
   {faceSolB, v123Fnd} = verifyVar123[allSolutions, faceEqs, v123Fnd];
   vPrint[ctag, "********************** Back Face 123 Solutions **********************"];
   vPrint[ctag, convert2Print[faceSolB]];
   vPrint[ctag, "*********************************************************************"];
   faceEqs = $topFaceEQ;
   {faceSolT, v123Fnd} = verifyVar123[allSolutions, faceEqs, v123Fnd];
   vPrint[ctag, "********************** Top Face 123 Solutions ***********************"];
   vPrint[ctag, convert2Print[faceSolT]];
   vPrint[ctag, "*********************************************************************"];
  faceSolT123 = x[1,1,1] /. faceSolT;
  dPrint[ctag, "Top Face 3-sub solutions: ", convert2Print[faceSolT123]]; 
  dPrint[ctag, "Variables Solved for: ``", convert2Print[v123Fnd]];

(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "********************* 123 Solution Comparisons **********************"];
  (* If the system is consistent around the cube, the solutions found for the 3-sub *)
  (* variables should be the same upon reduction using the solutions found         *)
  cacTest = True;
  Map[
    dPrint[ctag, "Current 3-sub Variable: ", convert2Print[#]]; 
    (* Compare solutions from Right and Top faces *)
    vPrint[ctag, convert2Print[#]];
    solution1 = Factor[# /. faceSolR];
    dPrint[ctag, "Right: ", convert2Print[solution1]]; 
    solution2 = Factor[# /. faceSolT];
    dPrint[ctag, "CAC:  Top: ", convert2Print[solution2]]; 
    solDiff = Simplify[solution1 - solution2];
 (* vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "********************* 123 Top/Right Difference **********************"];
    vPrint[ctag, convert2Print[solDiff]];
    eqVars={};
    If[UnsameQ[solDiff,0], 
      (* reduce difference using solutions found *)
      solDiff = Factor[solDiff /. allSolutions[[2]] ];
      solDiff = Factor[solDiff /. allSolutions[[1]] ];
    ];
    If[uShift, 
      dPrint[ctag, "Reduce difference using user provided rules.", pEquiv];
      solDiff = Factor[solDiff /. pEquiv];
      dPrint[ctag, convert2Print[solDiff]];
    ];
(*  vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "****************** 123 Top/Right Reduced Difference *****************"];
    vPrint[ctag, convert2Print[solDiff], " : ", solDiff===0];
    cacTest = cacTest && solDiff===0;

    (* Compare solutions from Right and Back faces *)
    solution1 = Factor[# /. faceSolR];
    dPrint[ctag, "CAC:  Right: ", convert2Print[solution1]]; 
    solution2 = Factor[# /. faceSolB];
    dPrint[ctag, "CAC:  Back: ", convert2Print[solution2]]; 
    solDiff = Simplify[solution1 - solution2];
(*  vPrint[ctag, "*********************************************************************"]; *) 
    vPrint[ctag, "********************** 123 Back/Right Difference ********************"];
    vPrint[ctag, convert2Print[solDiff]];
    eqVars={};
    If[UnsameQ[solDiff,0], 
      (* reduce difference using solutions found *)
      solDiff = Factor[solDiff /. allSolutions[[2]] ];
      solDiff = Factor[solDiff /. allSolutions[[1]] ];
    ];
    If[uShift, 
      dPrint[ctag, "Reduce difference using user provided rules.", pEquiv];
      solDiff = Factor[solDiff /. pEquiv];
      dPrint[ctag, convert2Print[solDiff]];
    ];
 (* vPrint[ctag, "*********************************************************************"]; *)     
    vPrint[ctag, "***************** 123 Back/Right Reduced Difference *****************"];
    vPrint[ctag, convert2Print[solDiff], " : ", solDiff===0];
    cacTest = cacTest && solDiff===0;
    vPrint[ctag, "*********************************************************************"];
  &, v123Fnd];

  If[cacTest, 
    vPrint[ctag, "************ The system is CONSISTENT around the cube. **************"],
    vPrint[ctag, "********** The system is NOT consistent around the cube. ************"]
  ];
  dPrint[ctag, "CAC:  final solution set: ", convert2Print[allSolutions]]; 

  dPrint[ctag, "Exit"];
  Return[];
]; 
(*****************************   Module: verifyCAC    **************************** *)


(****************************    Module : resetlpVariable   ********************* *)
(* Purpose:  Initializes the global variable lpVariable.                           *)
(* Input:  Null                                                                    *)
(******************************************************************************* *)
resetlpVariable::usage="resetlpVariable[] initializes the global information 
 structure, lpVariables.";
(******************************************************************************* *)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPSolveUI.m                                                *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
resetlpVariable[] :=
  Module[{}, 

  lpVariables = {{1,2,3}, {x,y,z}, {False, False, False}, {False, False, False}, 
                 {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}};
  vIdx=1; vName=2; vUsed=3; vLinear=4; 
  vConst=5; vSub=6; sRule=7; subs=8; cmnDen=9;
  
  Return[];
];
(****************************    Module : resetlpVariable   ********************* *)


(***********************    Module: genLatticeEquations    ********************** *)
(* Purpose:  From the given equations, generate the corresponding equations        *)
(*           on the cube.  I.e.  Assuming the equations specified correspond       *)
(*           to the front face of a cube, rotate and translate these equations     *)
(*           to generate the equations on the remaining 5 faces.                   *)
(* Input:  DDE                                                                     *)
(******************************************************************************* *)
genLatticeEquations::usage="genLatticeEquations[ddEquationList] generates each set
 of equations per face of the cube.";
(******************************************************************************* *)
(* Output:  List of DDEs adjusted per face of cube lattice                         *)
(* Code is in package:  LPSolveUI.m                                                *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
genLatticeEquations[ddeEquationList_, debug_:False] :=
Module[{ddEQ=ddeEquationList, dPrint, msg, ctag="gLatEq"},

  If[debug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: genLatticeEquations", "File: LPDrivers.m"];
  dPrint[ctag, "##################################################"];
  dPrint[ctag, "DDE: ", convert2Print[ddEQ]]; 

  (* generate corresponding equations for each face of the cube     *)
  (* First the rotational faces *)
  $frontFaceEQ = ddEQ;
  $leftFaceEQ = ddEQ /. $latFrnt2LeftRules;
  $groundFaceEQ = ddEQ /. $latFrnt2GrndRules;

  (* Next the shifted faces *)
  $rightFaceEQ = $leftFaceEQ /. $latLeft2RghtRules;
  $topFaceEQ = $groundFaceEQ /. $latGrnd2TopRules;
  $backFaceEQ = $frontFaceEQ /. $latFrnt2BackRules; 

  dPrint[ctag, "Front: ", convert2Print[$frontFaceEQ]];
  dPrint[ctag, "Back: ", convert2Print[$backFaceEQ]];
  dPrint[ctag, "Left: ", convert2Print[$leftFaceEQ]];
  dPrint[ctag, "Right: ", convert2Print[$rightFaceEQ]];
  dPrint[ctag, "Bottom: ", convert2Print[$groundFaceEQ]];
  dPrint[ctag, "Top: ", convert2Print[$topFaceEQ]];

  $latticeEQ = Union[$frontFaceEQ, $leftFaceEQ, $groundFaceEQ,
                         $backFaceEQ, $rightFaceEQ, $topFaceEQ];
  dPrint[ctag, "Lattice Equations: ", convert2Print[$latticeEQ]];
  dPrint[ctag, "Number of equations: ", Length[$latticeEQ]];

  (* Clear all local variables not being returned. *)
  dPrint[ctag, "Exit"];
  Return[];
];
(***********************    Module: genLatticeEquations    ********************** *)


(************************    Module: lpProcessDriver    ************************* *)
(* Purpose:  Branches based on the inputs of the user to calculate                 *)
(*           1. Consistency Around the Cube                                         *)
(*           2. Verify the Tetrahedron Property on the given lattice equ.          *)
(*           3. If possible, compute the corresponding Lax pair                    *)
(* Input:  DDE, flags indicating processing to be done.                            *)
(******************************************************************************* *)
lpProcessDriver::usage="lpProcessDriver[ddEquationList, pOpts] branches to the
 appropriate processing of the specified DDE.";
(******************************************************************************* *)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPSolveUI.m                                                *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
lpProcessDriver[filePath_, pOpts_] :=
 Module[{fPath=filePath, ddEQ, dPrint, vPrint, msg, newDDE, dStat, vStat, aug=False,
   dblSubList = {x[1,1,0], y[1,1,0], z[1,1,0]}, dblList, mx, dblRefVars, sUnks, 
   sPars, sVars, sUDefs, dsubVars, err=101, sScalar, tScalar, 
   ctag="lpDriv"},

  dStat = getPrintOptsDebug[lpProcDrv]; 
  vStat = getPrintOptsVerbose[lpProcDrv] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: lpProcessDriver ", "File: LPDrivers.m"];
  dPrint[ctag, "##################################################"];

  dPrint[ctag, "Current file: ", fPath, " Process Options: ", pOpts];
  If[Check[Get[fPath], err] =!= Null, 
    Throw[$Failed, failTag[$lpFileNotFound,lpProcessDriver]]
  ];
 (* vPrint[ctag, "*********************************************************************"]; *)  
    vPrint[ctag, "********************** Start of Computations ************************"];
  (* CHANGED HEREMAN JUL 2, 2017 *)
  vPrint[ctag, "Selected System of Partial Difference Equations: "];
  vPrint[ctag, nameINPUT];
  ddEQ = ddeEquationListINPUT;
  (* CHANGE HEREMAN JUL 2, 2017 *)
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "************ Specified Partial Difference Equation(s) ***************"];
  (* CHANGE HEREMAN JUL 2, 2017 *)
  vPrint[ctag, "Partial difference equation(s): ", convert2Print[ Map[#==0 &, ddEQ] ]];

  (* initialize necessary global variables  *)
  resetlpVariable[];
  {sUnks, sPars, sVars, sUDefs, dsubVars} = getVariableInfo[ddEQ, dStat];

(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "********************** Configuration Settings ***********************"];
  vPrint[ctag, "Verification of Consistency Around the Cube set to ", 
            Count[pOpts, $checkCAC]>0];
  vPrint[ctag, "Computation of Lax pair set to ", Count[pOpts, $computeLP]>0];
  If[ Count[pOpts, $computeLP]>0 ,
    vPrint[ctag, "Note: Computed Lax pair will be verified against defining equation."];
  ];
  vPrint[ctag, "Verification of user-provided Lax pair set to ", 
            Count[pOpts, $verifyLP]>0];
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "*********************************************************************"];

  (* Check for existence of _12 variables -- no processing if no _12 vars *)
  If[Length[dsubVars] == 0, 
    vPrint[ctag, "System does not contain double-subscripted variables."];
    vPrint[ctag, "In this case, any further processing will fail.", ctag ];
  ,
    (* Record which _ 12 variables appear.  Further computation will need this info *)
    vPrint[ctag, "Double-sub vars appearing: ", convert2Print[dsubVars]];
    Map[lpVariables[[ vUsed, var2Index[Head[#]] ]] = True; &, dsubVars ];
  ];

  (* Check for double references of the _12 variables *)
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "************ Checking Possible Multiple Referencing *****************"];
  mx = Max[dblList = Map[Length, DeleteCases[#, 0]& /@ 
                      Map[Function[$x$, Map[Coefficient[#, $x$]&, ddEQ]], dblSubList]]];
  If[mx > 1, 
    (* A 12-sub variable has multiple references in the given DDE *)
    dblRefVars = Extract[dblSubList, Position[dblList, mx]];
    dPrint[ctag, "The DDE contains multiple references to: ", 
                                         Map[convert2Print, dblRefVars]];
    dPrint[ctag, "Modify the DDE by removing the multiple references."];

    (* Examine equations ref-ing the _12 variable(s), solve/sub to remove ref *)
    newDDE = removeDoubleRefs[ddEQ, dblRefVars];
    dPrint[ctag, "Modified DDE: ", convert2Print[newDDE]];
  , (* else *)
    dPrint[ctag, "No double-subscripted multiple references."];
    newDDE = ddEQ;
  ];

  (* Augment the given system based on existence of edge equations *)
(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "********************* Checking Edge Equations ***********************"];
  aug=False;
  {newDDE,aug} = augmentDDE[newDDE];

  If[aug,
    vPrint[ctag, "Resulting system after augmentation: "]; 
    (* NOV 28, 2017 done in Lima *)
    vPrint[ctag, convert2Print[ Map[#==0 &, newDDE] ]]
    (* was:  vPrint[ctag, convert2Print[newDDE]]; *)
    ,
    dPrint[ctag, "No augmentation necessary."];
    ];

(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "*********************************************************************"];
   vPrint[ctag, "**************** Generating Equations on the Cube *******************"];
  If[Length[newDDE] > 0,
    (*  Generate the equations on the cube and keep them organized per face      *)
    (*  We will need some of these regardless of the functionality chosen.       *)
    genLatticeEquations[newDDE, dStat];

    (* initialize our list of solutions found for this system  *)
    $solutionList = {};
    $edgeConstraints = {};

    (* ***************   Check Consistency Around the Cube    ********************* *)
    If[Count[pOpts, $checkCAC] > 0, 
      (* operationG = oCAC; *)
   (* vPrint[ctag, "*********************************************************************"]; *) 
      vPrint[ctag, "************** Verifying Consistency Around the Cube ****************"];
      verifyCAC[newDDE, userShifts, paramEquivalences];
      dPrint[ctag, "Solution List: ", convert2Print[$solutionList]];

      (* Reset the global variables used in the CAC check workflow                *)
      $solutionList = {};
    ,   (* else  *)
   (* vPrint[ctag, "*********************************************************************"]; *)  
      vPrint[ctag, "********************* Skipping Consistency Check ********************"];
      vPrint[ctag, "*********************************************************************"];
    ];

    (****************   Compute a Lax Pair     **********************)
    (* CHANGED HEREMAN JUL 2, 2017 *)
    If [Count[pOpts, $computeLP] > 0,
   (* vPrint[ctag, "*********************************************************************"]; *) 
      vPrint[ctag, "*********************** Computing a Lax Pair ************************"];
      (* operationG = oCLP;  *)

      If[Count[pOpts, $checkCAC] == 0,
        (* algorithm is designed for DDE's that are consistent around the cube.  If    *)
        (* the check hasn't been done, warn the user as such.                         *)
     (* vPrint[ctag, "*********************************************************************"]; *) 
        vPrint[ctag, "********** Warning:  Consistency check was not performed. ***********"];
        vPrint[ctag, "******** The computed Lax pair is not guaranteed to be valid. *******"];
      ];
      compLPDriver[newDDE, dblSubSystemVars, userShifts, paramEquivalences];
        vPrint[ctag, "*********************************************************************"];
    ,  (* else  *)
   (* vPrint[ctag, "*********************************************************************"]; *) 
      vPrint[ctag, "**************** Skipping the Lax Pair Computation ******************"];
      vPrint[ctag, "*********************************************************************"];
    ];

    $edgeConstraints = {};
    (****************   Verify the User-provided Lax Pair      **********************)
    If [Count[pOpts, $verifyLP]>0, 
      (* program control flagged requires computation of variable solutions *)
   (* vPrint[ctag, "*********************************************************************"]; *) 
      vPrint[ctag, "**************** Verifying a User-provided Lax Pair *****************"];
      (* operationG = oVLP;  *)
      If[explicitScalars,
        sScalar = sFunc; tScalar = tFunc;
      , 
        sScalar = 1; tScalar = 1;
      , 
        sScalar = 1; tScalar = 1;];
   (* correction: HEREMAN JUL 31, 2017 Ask TB: The position of tScalar and sScalar appears to have been swapped !!! *)     
   (* was: verifyLPDriver[newDDE, laxPairMatrixL, laxPairMatrixM, explicitScalars,
         sScalar, tScalar, userShifts, paramEquivalences]; *)
   (* now corrected: *)
   verifyLPDriver[newDDE, laxPairMatrixL, laxPairMatrixM, explicitScalars,
         tScalar, sScalar, userShifts, paramEquivalences]; 
   (* vPrint[ctag, "*********************************************************************"]; *) 
      vPrint[ctag, "*********************************************************************"];
    ,
      (* else  *)
   (* vPrint[ctag, "*********************************************************************"]; *) 
      vPrint[ctag, "********* Skipping Verification of User-provided Lax Pair ***********"];
      vPrint[ctag, "*********************************************************************"];
    ];
  ];

  dPrint[ctag, "Exit"];
];
(***************************    Module: lpProcessDriver    ********************** *)


(***************************    Module: lpSystemStatistics   ******************** *)
(* lpSystemStatistics[ddeEquationList]                                             *)
(* Purpose:  Inspects the user provided equations and specifically identifies      *)
(*           1. any single edge equations                                          *)
(*           2. any double edge equations                                          *)
(*           3. variables referenced                                               *)
(*           4. total number of provided equations                                 *)
(*           5. total number of equations incl. any augmented eqs                  *)
(* Input:   User Specified List of discrete differential functions                 *)
(******************************************************************************* *)
lpSystemStatistics::usage="lpSystemStatistics[ddEquationList] computes the
 statistics (variables, type of equations, etc.) of the specified DDE.";
(******************************************************************************* *)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPSolveUI.m                                                *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
lpSystemStatistics[filePath_] :=
 Module[{fPath=filePath, ddEQ, dPrint, vPrint, eqLen, dblList, mx, newEQ, varPat, 
   posPat, eqLoc, err=101, aug, sUnks, sPars, sVars, sUDefs, dsubVars,  tmpEQ,
   dblRefVars, dblRefEq, edgeEQ, sglEdge, dblEdge, newDDE, vpos, dStat, vStat, 
   ctag = "syStat"},

  dStat = getPrintOptsDebug[lpSysStat]; 
  vStat = getPrintOptsVerbose[lpSysStat] || dStat;
  If[vStat, vPrint=vrbPrint, Clear[vPrint], Clear[vPrint]];
  If[dStat, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: lpSystemStatistics; ", "File: lpsystat.m"];
  dPrint[ctag, "#######################################################"];
  dPrint[ctag, "Current file: ", fPath];
  (* reset variables used in the DDE files *)
  Clear[ddeEquationListINPUT, nameINPUT, laxPairMatrixL, laxPairMatrixM, 
            explicitScalars, sFunc, tFunc, userShifts, paramEquivalences];
  explicitScalars=False;
  userShifts=False;
  If[Check[Get[fPath], err] =!= Null, 
    Throw[$Failed, failTag[$lpFileNotFound, lpSystemStatistics]]
  ];
  dPrint[ctag, "name: ", nameINPUT, " ddEQ: ", convert2Print[ddeEquationListINPUT],
    "; matrix L: ", laxPairMatrixL, "; matrix M: ", laxPairMatrixM, "; scalars: ",
    explicitScalars];
  ddEQ = ddeEquationListINPUT;

(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "************************ DDE System Statistics **********************"];
  vPrint[ctag, "File specified: ", fPath];
  If[StringLength[nameINPUT]>0, vPrint[ctag, "DDE Name: ", nameINPUT]];
  If[Length[laxPairMatrixL]>0 && Length[laxPairMatrixM]>0, 
    vPrint[ctag, "Corresponding User-specified Lax pair"];
    vPrint[ctag, "L matrix: ", MatrixForm[convert2Print[laxPairMatrixL]]];
    vPrint[ctag, "M matrix: ", MatrixForm[convert2Print[laxPairMatrixM]]];
  ]; (* HERE *)
(* CHANGED HEREMAN JUL 2, 2017 -added a ; after the If statement above and below *)
  If[explicitScalars,
    vPrint[ctag, "L,M matrix scalars also specified."];
    vPrint[ctag, "s:  ", convert2Print[sFunc], " t: ", convert2Print[tFunc]];
  ];  (* HERE ALSO *)
  vPrint[ctag, "partial difference equation(s):: ", convert2Print[ Map[#==0 &, ddEQ] ]];
  dPrint[ctag, "Contains ", eqLen=Length[ddEQ], " equations."];

  {sUnks, sPars, sVars, sUDefs, dsubVars} = getVariableInfo[ddEQ, dStat];
 
  (* vPrint[ctag, convert2Print[ Map[#==0 &, ddEQ] ]];  *)
  vPrint[ctag, "The partial difference equation contains: ", eqLen, " equation(s)."];
  vPrint[ctag, "The partial difference equation references: "];
  vPrint[ctag, " Unknowns: ", convert2Print[sUnks]];
  vPrint[ctag, "which will be interpretted as,"];
  vPrint[ctag, " Parameters: ", convert2Print[sPars]];
  vPrint[ctag, " Variables: ", convert2Print[sVars]];
  If[Length[sUDefs] > 0, 
    vPrint[ctag, " User has defined: ", convert2Print[sUDefs] ]; 
  ];
  If[userShifts, 
    vPrint[ctag, "Additional parameter shifts specified."];
    vPrint[ctag, paramEquivalences];
  ];
  newEQ = ddEQ;
  If[Length[dsubVars] == 0, 
    vPrint[ctag, "System does not contain double-subscripted variables."];
    vPrint[ctag, "In this case, any further processing will fail."];,
  (* else *)
    vPrint[ctag, "System contains double-subscripted variable(s): ", 
               convert2Print[dsubVars]];
    (* Check for double references of the _12 variables *)
    mx = Max[dblList = Map[Length, DeleteCases[#, 0]& /@ 
                      Map[Function[$x$, Map[Coefficient[#, $x$]&, ddEQ]], dblSubList]]];
    If[mx > 1, 
      (* A 12-sub variable has multiple references in the given DDE *)
      vPrint[ctag, "The partial difference equation(s) contains multiple references to: ", 
              Map[convert2Print, dblRefVars = Extract[dblSubList, Position[dblList, mx]]]];
      vPrint[ctag, "The partial difference equation(s) will be modified to remove the multiple references."];

      (* Examine equations referencing the _12 variable(s), solve/sub to remove reference *)
      newEQ = removeDoubleRefs[ddEQ, dblRefVars];
      dPrint[ctag, convert2Print[ Map[#==0 &, newEQ] ]];
    ];
  ];

  {sglEdge, dblEdge} = findEdgeEquations[ddEQ];
  
  vPrint[ctag, "It contains ", Length[dblEdge], " double edge equations(s)."];
  If[Length[dblEdge]>0,
    vPrint[ctag, convert2Print[Map[ # == 0 &, dblEdge] ]];
  ];

  vPrint[ctag, "It contains ", Length[sglEdge], " single edge equation(s)."];
  If[Length[sglEdge]>0,
    vPrint[ctag, convert2Print[ Map[# == 0&, sglEdge] ]];
    vPrint[ctag, "The system may be augmented with additional derived equations prior to processing."];
    aug=False;
    {newDDE,aug} = augmentDDE[newEQ];
(* CHANGED HEREMAN JUL 2, 2017 *)
    If[aug,
      vPrint[ctag, "Augmented partial difference equation(s): ", convert2Print[ Map[#==0 &, ddEQ] ]];
    ,
      vPrint[ctag, "No augmentation necessary.  Partial difference equation(s): ", convert2Print[ Map[#==0 &, ddEQ] ]];
    ];
  ];

(* vPrint[ctag, "*********************************************************************"]; *) 
   vPrint[ctag, "*********************************************************************"];

  dPrint[ctag, "Exit"];
  Return[];
];
(***************************    Module: lpSystemStatistics   ******************** *)


(************************    Module: uiModuleFlagsCBox   ************************ *)
uiModuleFlagsCBox::usage="uiModuleFlagsCBox[mFlags, debug] saves the current user-specified
 flags for debug status with the Module Level functionality.";
(******************************************************************************* *)
(* Output:  NULL                                                                   *)
(* Code is in package:  LPSolveUI.m                                                *)
(* Last Modified:                                                                  *)
(******************************************************************************* *)
uiModuleFlagsCBox[mFlags_, debug_] := 
 Module[{mlf=mFlags, dPrint, ctag="mlfCBox", tcat={}},

  If[debug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: uiModuleFlagsCBox ", "File: LPSolveUI.m"];
  dPrint[ctag, "#######################################################"];
  dPrint[ctag, "Module Level Flags: ", mlf];

  (* Turn on the debug flag for the specified modules *)
  setPrintOptsDebug[mlf, tcat, True, debug];
  
  (* Turn off the debug flag for the unspecified modules *)
  setPrintOptsDebug[Complement[moduleLevelFlagsVal, mlf], tcat, False, debug];
];
(************************    Module: uiModuleFlagsCBox   ************************* *)


(************************    Module: uiProcessFlagsCBox   ************************ *)
uiProcessFlagsCBox::usage="uiProcessFlagsCBox[mFlags, debug] saves the current
 user-specified flags for processing.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPSolveUI.m                                                 *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
uiProcessFlagsCBox[pFlags_, debug_] := 
 Module[{plf=pFlags, dPrint, ctag="plfCBox", ttag=0},

  If[debug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: uiProcessFlagsCBox ", "File: LPSolveUI.m"];
  dPrint[ctag, "#######################################################"];
  
  (* Turn off the debug flag for the unspecified modules *)
  setPrintOptsDebug[ttag, Complement[processLevelFlagsVal, plf], False, debug];
  dPrint[ctag, "Process Level Flags: ", plf];

  (* Turn on the debug flag for the specified modules *)
  setPrintOptsDebug[ttag, plf, True, debug];
];
(************************    Module: uiProcessFlagsCBox   ************************ *)


(**************************    Module: uiProcessBtn    *************************** *)
(* uiProcessBtn[fileSource, eqSpec, ddeFilename, procOpts, debug]                   *)
(* Purpose:  Initiates the system statistics process invoked from the Stats btn.    *)
(******************************************************************************** *)
uiProcessBtn::usage="uiProcessBtn[fileSource, eqSpec, ddeFilename, procOpts, debug] 
 initiates computation on the specified system based on the user selected option.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPSolveUI.m                                                 *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
uiProcessBtn[fileSource_, eqSpec_, ddeFilename_, procOpts_, debug_:False] := 
 Module[{fSrc=fileSource, eq=eqSpec, ddeF=ddeFilename, ddEQ, dPrint, 
   dStat, vStat, pOpts=procOpts, matL, matM, tF, sF, expS, wd=$currentLPDirectory,
   fullPath=NULL, ctag="uiProcBtn"},

  If[debug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];
  dPrint[ctag, "Function: ddeProcessBtn ", "File: LPSolveUI.m"];
  dPrint[ctag, "#######################################################"];
  dPrint[ctag, "File Source: ", fSrc];
  dPrint[ctag, "File Index: ", eq];

  If[debug, showPrintOptions[]];
  Switch[fSrc,
    scalarEQ, {
      dPrint[ctag, "Pre-Defined Scalar Equation selected."];
      dPrint[ctag, "File Index: ", eq];
      ddeF=Null;
      If[eq > 0, ddeF=scalarDDEs[[2;; ;;2]][[eq]]; ];
      fullPath = wd <> ddeF; 
      dPrint[ctag, "Scalar selected: ", fullPath];
    },
    systemEQ, {
      dPrint[ctag, "Pre-Defined System of Equations selected."];
      dPrint[ctag, "File Index: ", eq];
      ddeF=Null;
      If[eq > 0, ddeF=systemDDEs[[2;; ;;2]][[eq]]; ];
      fullPath = wd <> ddeF;
      dPrint[ctag, "System selected: ", fullPath];
    },
    userDefEQ, {
      dPrint[ctag, "User specified equation selected."];
      fullPath = ddeF;
      dPrint[ctag, "User file: ", fullPath];
    }
  ];

  catchInternalError[lpProcessDriver[fullPath, pOpts], lpProcessDriver, failTag];
  Return;
];
(**************************    Module: uiProcessBtn    *************************** *)


(**************************    Module: uiStatisticsBtn    ************************ *)
(* uiStatisticsBtn[fileSource, eqSpec, ddeFilename, debug]                          *)
(* Purpose:  Initiates the system statistics process invoked from the Stats btn.    *)
(******************************************************************************** *)
uiStatisticsBtn::usage="uiStatisticsBtn[fileSource, eqSpec, ddeFilename, debug]
 initiates the system statistics computation from the LP Solver Dialog.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPSolveUI.m                                                 *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
uiStatisticsBtn[fileSource_, eqSpec_, ddeFilename_, debug_:False] := 
 Module[{fSrc=fileSource, eq=eqSpec, ddeF=ddeFilename, ddEQ, dPrint, eqs,
    slDDEs=scalarDDEs, syDDEs=systemDDEs, wd=$currentLPDirectory,
    fullPath="", ctag="statBtn"},

  If[debug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];
  dPrint[ctag, "Function: ddeStatBtn, File: LPSolveUI.m"];
  dPrint[ctag, "#######################################################"];
  dPrint[ctag, "File Source: ", fSrc];
  dPrint[ctag, "File Index: ", eq];

  Switch[fSrc,
    scalarEQ, {
      dPrint[ctag, "Pre-Defined Scalar Equation selected."];
      dPrint[ctag, "File Index: ", eq];
      ddeF=Null;
      If[eq > 0, ddeF=slDDEs[[2;; ;;2]][[eq]]; ];
      fullPath = wd <> ddeF; 
      dPrint[ctag, "Scalar selected: ", fullPath];
    },
    systemEQ, {
      dPrint[ctag, "Pre-Defined System of Equations selected."];
      dPrint[ctag, "File Index: ", eq];
      ddeF=Null;
      If[eq > 0, ddeF=syDDEs[[2;; ;;2]][[eq]]; ];
      fullPath = wd <> ddeF;
      dPrint[ctag, "System selected: ", fullPath];
    },
    userDefEQ, {
      dPrint[ctag, "User specified equation selected."];
      fullPath = ddeF;
      dPrint[ctag, "User file: ", fullPath];
    }
  ];

  catchInternalError[lpSystemStatistics[fullPath], lpSystemStatistics, failTag];
];
(**************************    Module: uiStatisticsBtn    ************************ *)


(**************************    Module: initLaxPairInfo    ************************ *)
initLaxPairInfo::usage="initLaxPairInfo[] initializes all the necessary operational 
 information.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPSolveUI.m                                                 *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
initLaxPairInfo[workDir_] := Module[{ctag="initLP"},

  createOutputNotebook[];

  $currentLPDirectory = workDir;
  dbgPrint[ctag, "Current working directory: ", $currentLPDirectory];

  $lpVariables = {{1,2,3},  (* Variable Index *)
                  {x,y,z},  (* Variable Name *)
                  {False, False, False}, (* Variable usage flag *)
                  {False, False, False}, (* Variable linearity flag *)
                  {0,0,0},  (* Variable Constraints *)
                  {0,0,0},  (* Variable Sub Order *)
                  {0,0,0},  (* substitution rule per variable *)
                  {0,0,0},  (* Solutions after initial sub *)
                  {0,0,0}   (* Common denominator *)
                 };

  Return[];
]
(**************************    Module: initLaxPairInfo    ************************ *)


(**************************    Module: LaxPairSolverUI    ************************ *)
(* LaxPairSolverUI[workDir, uiDebug]                                                *)
(* Purpose:  Used for debug messages to the output notebook.                        *)
(******************************************************************************** *)
LaxPairSolverUI::usage="LaxPairSolverUI[workDir, userInterfaceDebugFlag] initializes 
 the Lax Pair Solver dialog.";
(******************************************************************************** *)
(* Output:  NULL                                                                    *)
(* Code is in package:  LPSolveUI.m                                                 *)
(* Last Modified:                                                                   *)
(******************************************************************************** *)
Options[LaxPairSolverUI] = {userInterfaceDebugFlag->False, gVrb->True};
LaxPairSolverUI[workDir_, opts : OptionsPattern[]] := 
Module[{dPrint, uiDebug=OptionValue[userInterfaceDebugFlag], ctag="uiLP",
  (* Dialog Appearance Constants *)
  titleFontSize=24, subTitleFontSize=18, subsubTitleFontSize=15,
  processLevelFlags = {"Initialization Functions", "CAC Computations",
                         "Lax Pair Computations", "Verifications Computations"}
  },
  
  equationFontSize=15;
  $globalVerbose = OptionValue[gVrb];
  initLaxPairInfo[workDir];
  If[uiDebug, dPrint=dbgPrint, Clear[dPrint], Clear[dPrint]];

  dPrint[ctag, "Function: LaxPairSolveUI", "File: LPSolveUI.m"];
  dPrint[ctag, "#######################################################"];

  CreateDialog[DynamicModule[{lpOps, pdEQ=1, fileSource=0, ddeFilename="", 
    plFlags={}, slDDEs=scalarDDEs, syDDEs=systemDDEs, 
    numSL=Length[scalarDDEs[[1;; ;;2]]], numSY=Length[systemDDEs[[1;; ;;2]]], 
    mlFlags, dbg=uiDebug, dPrt=dPrint, scalarRB=0, systemRB=1, userRB=2, 
    eqSL=1, eqSys=1},

    Grid[
      {{Text[Style["Lax Pair Solver ",Bold, titleFontSize,FontFamily->"Helvetica "]], SpanFromLeft},
      {TabView[{
        "Operational Options "->
          Grid[{
            {Text[Style["Equation Specification ",Bold,subTitleFontSize,FontFamily->"Helvetica "]], SpanFromLeft},
			{RadioButton[Dynamic[fileSource], scalarRB],Text[Style["Scalar Equations ", Bold, subsubTitleFontSize]], SpanFromLeft},
			{" ", PopupMenu[Dynamic[eqSL,(eqSL=#; eqSys=1; pdEQ=eqSL)&], Thread[Table[i,{i,1,numSL}] -> slDDEs[[1;; ;;2]] ], 
                                  Enabled->Dynamic[fileSource==scalarRB]], SpanFromLeft},
			{},
			{RadioButton[Dynamic[fileSource], systemRB],Text[Style["System of Equations ", Bold, subsubTitleFontSize]], SpanFromLeft},
			{ " ",PopupMenu[Dynamic[eqSys,(eqSys=#; eqSL=1; pdEQ=eqSys)&], Thread[Table[i,{i,1,numSY}] -> syDDEs[[1;; ;;2]] ],
                                  Enabled->Dynamic[fileSource==systemRB]], SpanFromLeft},
			{},
			{RadioButton[Dynamic[fileSource], userRB],Text[Style["User Specified Equation ", Bold, subsubTitleFontSize]], SpanFromLeft},
			{ " ",InputField[Dynamic[ddeFilename],String,FieldSize->Medium, FieldHint->"Input File...", 
			          Enabled->Dynamic[fileSource==userRB]], FileNameSetter[Dynamic[ddeFilename], Enabled->Dynamic[fileSource==userRB]]},
			{},
			{Item[Button["DDE Statistics ", uiStatisticsBtn[fileSource, pdEQ, ddeFilename, dbg], 
			                ImageSize->Automatic], Alignment->Center], SpanFromLeft},
			{},
            {Text[Style["Operations ",Bold, subTitleFontSize,FontFamily->"Helvetica "]], SpanFromLeft},
			{CheckboxBar[Dynamic[lpOps], MapThread[#1->#2 &,{lpSolverOpValues, lpSolverOpLabels}], 
			                                           Appearance-> "Vertical"], SpanFromLeft},
			{Item[Button["Process Systems", uiProcessBtn[fileSource, pdEQ, ddeFilename, lpOps, dbg], 
			                ImageSize->Automatic], Alignment->Center], SpanFromLeft},
			{}
          }, Spacings->{1,Automatic},Alignment->Left, Frame->False, Dividers->False],
(**********************************************************************************************************************)
        "Advanced Directives "->
          Grid[{
            {Text[Style["Debug Output ",Bold, subTitleFontSize,FontFamily->"Helvetica "]]},
			{},
			{Text[Style["Process-Level Debug Flags ",Bold, subsubTitleFontSize,FontFamily->"Helvetica "]]},
			{CheckboxBar[Dynamic[plFlags, (plFlags=#; uiProcessFlagsCBox[#, dbg])& ],
			        MapThread[#1->#2 &,{processLevelFlagsVal, processLevelFlagsTxt}], Appearance-> "Vertical"]},
			{}, 
            {},
			{Text[Style["Module-Level Debugs Flags ",Bold, subsubTitleFontSize,FontFamily->"Helvetica "]]},
			{CheckboxBar[Dynamic[mlFlags, (mlFlags=#; uiModuleFlagsCBox[#, dbg])& ],
			        MapThread[#1->#2 &,{moduleLevelFlagsVal, moduleLevelFlagsTxt}], Appearance-> "Vertical"->{Automatic, 2}]}
          }, Frame->False, Alignment->Left, Spacings->{1,Automatic}]
		}],SpanFromLeft},
		{Item[Button["Close", DialogReturn[], ImageSize->Automatic], Alignment->Center], SpanFromLeft}
      }, Frame->False, Spacings->{1,2},Alignment->Left]
     ], 
     WindowTitle->"Lax Pair Dialog", WindowFloating->False]
];
(**************************    Module: LaxPairSolverUI    ************************ *)


End[];

Print["LaxPairSystemsPartialDifferenceEquations v2 Successfully Loaded.", Today];
Print["The package was LAST UPDATED by Hereman on December 24, 2017 at 00:20 (12:20am)."];
(* Print["The package was LAST UPDATED by Hereman on July 3, 2017 at 00:25 (12:25am)."]; *)

EndPackage[]

(* end of file *)

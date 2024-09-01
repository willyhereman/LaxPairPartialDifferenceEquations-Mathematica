(* ::Package:: *)

 


(* ## ## ## ## ##            Function: solveFrontFace            ## ## ## ## ## *)

(*******************************************************************************)
(* solveFrontFace[faceEquList]                                                 *)
(* Purpose:                                                                    *)
(* Input: The original (possibly augmented) system of DDE.  Finds a solution   *)
(*        representation of the system by solving:                             *)
(*        1. for any 2-sub variables                                           *)
(*        2. for any 1-sub variables                                           *)
(*        Note:  special care must be given to any single-edge equations       *)
(*               as to the variable to be solved for.                          *)
(* Output:  List of rules describing solutions for the variables               *)
(*          found in that face's equations.                                    *)
(*          Also, a list of variables solved for in that face                  *)
(*                                                                             *)
(* Debug Flag:  lpSolveFrontFaceDebug                                          *)
(*                                                                             *)
(* Code is in File:  solvfrnt.m                                                *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

solveFrontFace[faceEquList_] :=
Module[{faceEQ = faceEquList, printFF, vars2, vars1d, vars1s,
        varSol, varsFnd, solList2, solList1s, solList1d, dblFaceEQ, 
        sglEdge, dblEdge, cVar, tmpList,
        sglSub, varsConstrained, edgeEQ, msg, vC,
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

  (* Establish any necessary failure conditions and corresponding messages *)
  If[dbLPSolveFF, printFF = Print, Clear[printFF], Clear[printFF]];

  printFF["D: sFF, Function: solveFrontFace, File: solvfrnt.m"];
  printFF["D: #################################################"];

  printFF["D: sFF, Solve for any _12 variables"];
  dblFaceEQ = Complement[faceEQ, singleEdgeEQG, doubleEdgeEQG];
  printFF["D: sFF, Using equations\n", convert2Print[dblFaceEQ]];

  frontFaceSolutions = {};

  solList2={};  vars2={};
  Map[
    cVar = #;
    printFF["D: sFF, Current: variable\n", convert2Print[cVar]];

    linEQ = Select[dblFaceEQ, Exponent[#,cVar]==1&];
    If[Length[linEQ]>0,
      (* sort the matching equations based on LeafCount and then we only         *)
      (* need to grab the 1st equ referenced to have simplest.                   *)
      linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
      equRef = Factor[linEQ[[First[First[Position[linEQ, cVar]]]]]];
      printFF["D: sFF, Current: equation\n", convert2Print[equRef]];

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
    Throw[$Failed, failTag[lpEno12,solveFrontFace]]
  ];

  printFF["D: sFF:  _12 solutions found:\n", convert2Print[solList2]]; 
  printFF["D: sFF:  variables found:\n", convert2Print[vars2]]; 
  sglSub = PadRight[Delete[Level[#,{1}],1],3] /. {List -> Head[#]}& /@ vars2;
  printFF["D: sFF:  key variable solutions needed:\n", convert2Print[sglSub]]; 

  (* The remaining equations (which do not reference _ 12 variables) are either   *)
  (* single or double edge equations.  The single edge provide additional        *)
  (* constraints for Lax pair computations so care as to which variable to solve *)
  (* for, must be taken.  Double edge can be handled easily.                     *)

  solList1s={};  vars1s={};  varSol = {};
  printFF["D: sFF, edge equations\n", convert2Print[singleEdgeEQG]];
  If[Length[singleEdgeEQG]>0,
    sglEdge = singleEdgeEQG;

    (* of the _ 12 solutions found, which _ 1 variables appear in the edge equations *)
    varsConstrained = Intersection[Variables[#], sglSub]& /@ sglEdge;
    If[Length[Complement[Length[#]& /@ varsConstrained, {0,1}]] > 0,
      (* there are edge equations with more than 1 necessary variable appearing  *)
      MapIndexed[
        vC = Complement[#1, vars1s];
        edgeEQ = sglEdge[[ First[#2] ]];
        If[Length[vC] > 1, 
          printFF["D: sFF, Edge constraints of ", convert2Print[edgeEQ],  
                     " involving ", convert2Print[vC] ];
          (* If the UserInteractive flag is set, prompt the user as to which variable to solve for *)
          (* prompt when calculating LP since choice will result in different answers  *)
          If[pcUserInput && operationG == oCLP,
            msg = StringJoin["The equation,\n", ToString[convert2Print[edgeEQ]], 
                    "\nintroduces constraints between ", ToString[convert2Print[vC] ],
                    "\n Solve for the following variable:"];
            printFF["D: sFF, Msg:", msg];  
            cVar = ChoiceDialog[msg, Map[convert2Print[#] -> #&, vC]];
          ,
            cVar = First[vC];
          ];

          printFF["D: sFF, Variable Selected:", convert2Print[cVar]];  
          varSol = Factor[Flatten[Solve[edgeEQ==0, cVar]]];
          edgeConstraintsG = Union[edgeConstraintsG, varSol];
(*        solList1s = Union[solList1s, varSol];  *)
(*        vars1s = Flatten[Union[vars1s, {cVar}]]; *)
        , (* else Length[vC] <=1  *)
          If[Length[vC]==1,
            printFF["D: sFF, Edge equation involving only 1 constrained variables:", convert2Print[edgeEQ]];  
            cVar = First[vC];
            varSol = Factor[Flatten[Solve[edgeEQ==0, cVar]]];
(*          solList1s = Union[solList1s, varSol];  *)
(*          vars1s = Flatten[Union[vars1s, {cVar}]]; *)
          ,
            printFF["D: sFF, Edge equation involving no constrained variables:", convert2Print[edgeEQ]];  
            cVar = Select[Reverse[MonomialList[edgeEQ, varList, orderM, Modulus->2]],
               Length[Position[solList1s,#]]==0 &,1];
            printFF["D: sFF, Current: variable\n", convert2Print[cVar]];
            varSol = Factor[Flatten[Solve[edgeEQ==0, cVar]]];
(*          solList1s = Union[solList1s, varSol];  *)
(*          vars1s = Flatten[Union[vars1s, {cVar}]]; *)
          ]; (* Length[vC]==1 *)
        ]; (* Length[vC] > 1 *)
        solList1s = Union[solList1s, varSol];
        vars1s = Flatten[Union[vars1s, {cVar}]];

        printFF["D: sFF, Solution found:", convert2Print[varSol]];  
      &, varsConstrained];
    ];
  ];

  printFF["D: sFF, Constraints found:", convert2Print[edgeConstraintsG]];  
  printFF["D: sFF:  single edge solutions found:\n", convert2Print[solList1s]]; 

  printFF["D: sFF, Solve for remaining single subscript variables"];
  solList1d={};  vars1d={};
  If[Length[doubleEdgeEQG] > 0, 
    printFF["D: sFF, Solve all double edge equations\n",
             convert2Print[doubleEdgeEQG]];
    dblEdge = doubleEdgeEQG;
    Map[
      cVar = #;
      printFF["D: sFF, Current: variable\n", convert2Print[cVar]];

      linEQ = Select[dblEdge, Exponent[#,cVar]==1&];
      If[Length[linEQ]>0,
        (* sort the matching equations based on LeafCount and then we only         *)
        (* need to grab the 1st equ referenced to have simplest.                   *)
        linEQ = Sort[linEQ, LeafCount[#1] < LeafCount[#2]&];
        equRef = Factor[linEQ[[First[First[Position[linEQ, cVar]]]]]];
        printFF["D: sFF, Current: equation\n", convert2Print[equRef]];

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

  (* Simplify the 2-sub solutions using the 1-sub solutions found *)
(*  tmpList = {};
  Map[tmpList = Union[tmpList, {Factor[# /. solList1s]}];&, solList2 ];
  solList2 = tmpList;  tmpList = {};
  Map[tmpList = Union[tmpList, {Factor[# /. solList1d]}];&, solList2 ];
  solList2 = tmpList;  tmpList = {};  *)

  frontFaceSolutions = {{}, Union[solList1s, solList1d], solList2};
 

  solList = frontFaceSolutions;
  printFF["D: sFF:  Solution Set for this Face:\n", convert2Print[frontFaceSolutions]]; 

  varsFnd = Union[vars2,vars1d, vars1s];
  printFF["D: sFF:  Variables Solved for:\n", convert2Print[varsFnd]];

  varsFound = Union[varsFound, varsFnd];
  faceVariables = varsFound;

  printFF["D: sFF, exit"];
  Clear[faceEQ, printFF, vars2, vars1d, vars1s, varSol, varsFnd,
        solList2, solList1s, solList1d, dblFaceEQ, varList, orderM];
  Return[solList];
]; (* end Module solveFaceEqu *)

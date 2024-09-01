(* ::Package:: *)

(* ## ## ## ## ##            Function: mergeSolLists             ## ## ## ## ## *)

(*******************************************************************************)
(* mergeSolLists[solList1, solList2]                                           *)
(* Purpose:                                                                    *)
(* Input: 2 lists of solutions sorted according to subscripts                  *)
(*                                                                             *)
(* Output:  Single list of solutions consistent w/ original sort               *)
(*                                                                             *)
(* Code is in File:  merglist.m                                                *)
(*                                                                             *)
(* Last Modified:                                                              *)
(*******************************************************************************)

mergeSolLists[solutionList1_, solutionList2_] :=
Module[{solList1 = solutionList1, solList2 = solutionList2,
         printMerge, solList={{},{},{}}, tmpList={}, tmpRule={}, idx=0},

  dbLPMergeLists = False;
  If[dbLPMergeLists, printMerge = Print, Clear[printMerge], Clear[printMerge]];

  (* combine the provided lists into a list of solutions sorted by variable indices *)
  printMerge["D: mList:  Provided List of Solutions:\n", convert2Print[solList1], 
                                                    "\n",convert2Print[solList2] ]; 
  If[Length[ Flatten[solList1] ]>0, 
    If[Length[ Flatten[solList2] ]>0,
      MapIndexed[
        idx = First[#2];
        If[Length[#1] > 0,
          tmpList = Flatten[#1];
          MapIndexed[
            redRule = #1;
(*            printMerge["D: mList:  current rule:\n", convert2Print[redRule] ];   *)
            newRule = Map[First[#] -> Simplify[(First[#] /. #) /. redRule] &, solList2[[idx]] ];
(*            printMerge["D: mList:  reduced rule:\n", convert2Print[newRule] ];   *)
            solList2[[idx]] = newRule;
          &, tmpList];

          solList2[[idx]] = Select[solList2[[idx]],
             Length[Union[Flatten[# /. {Rule -> List}]]]>1 &];   
(*     
          printMerge["D: mList:  Index:\n", idx]; 
          printMerge["D: mList:  Reduction List (1):\n", convert2Print[ solList1[[idx]] ]]; 
          printMerge["D: mList:  Reduced List (2):\n", convert2Print[ solList2[[idx]] ]]; 
*)
          solList[[idx]] = Complement[ Union[solList1[[idx]], solList2[[idx]]], {0}];
        ,
          solList[[idx]] = solList2[[idx]];
        ];
        printMerge["D: mList:  Merged List:\n", convert2Print[ solList[[idx]] ]]; 
      &, solList1];
    ,
      solList = solList1;
    ];
  ,
    solList = solList2;
  ];
  printMerge["D: mList:  Merged List of Solutions:\n", convert2Print[solList]]; 

  printMerge["D: mList, exit"];
  Clear[printMerge, tmpList, idx];
  Return[solList];
]; (* end Module solveFaceEqu *)


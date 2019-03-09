#!/usr/bin/env wolframscript


$langs={"English","ChineseSimplified"}

$pacletBase=FileNameJoin@{DirectoryName[Directory[]],"SnakeGame"}
$trBase=FileNameJoin@{$pacletBase,"TextResources"}

$packageFiles=FileNames["*.wl",$pacletBase]



trFile[lang_String]:=FileNameJoin@{$trBase,lang,"Default.m"}
trLoad[lang_String]:=trLoad[lang]=With[
  {load=Get[trFile[lang]]},
  If[MatchQ[load,KeyValuePattern[{}]],load,{}]
]
GetTrStrings[files_List]:=Flatten[GetTrStrings/@files]
GetTrStrings[file_]:=Cases[Import[file,{"Package","HeldExpressions"}],HoldPattern@tr[text_String]:>text,Infinity]


trtexts=<|#->#&/@GetTrStrings[$packageFiles]|>

Do[
  If[!FileExistsQ@trFile[lang],
    CreateFile[trFile[lang]]
  ];
  Put[
    Normal@Merge[{trtexts,Association@@trLoad[lang]},Last],
    Echo[trFile[lang],"Update: "]
  ],
  {lang,$langs}
]
Print["Finished."]

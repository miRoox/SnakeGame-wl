(* ::Package:: *)

(* ::Title:: *)
(*Snake Game*)


(* ::Section:: *)
(*Interface*)


BeginPackage["SnakeGame`"]


Unprotect[SnakeGame,SnakeMap,$SnakeMapTemplates,$SnakeDirections]
ClearAll["`*"]


PlaySnakeGame::usage="PlaySnakeGame[] plays a new snake game.
PlaySnakeGame[map] plays a new snake game with the map.
PlaySnakeGame[game] continues a snake game."
NewSnakeGame::usage="NewSnakeGame[] creates a new snake game.
NewSnakeGame[map] creates a new snake game with the map."
SaveSnakeGame::usage="SaveSnakeGame[file,game] saves the snake game to the file."
LoadSnakeGame::usage="LoadSnakeGame[file] loads the snake game from the file."
CreateSnakeMap::usage="CreateSnakeMap[opts] creates snake game.
CreateSnakeMap[template,opts] creates snake map based on the template."
SaveSnakeMap::usage="SaveSnakeMap[file,map] saves the snake map to the file."
LoadSnakeMap::usage="LoadSnakeMap[file] loads the snake map from the file."
SnakeGame::usage="SnakeGame[\[Ellipsis]] represents a snake game archive."
SnakeMap::usage="SnakeMap[\[Ellipsis]] represents a snake map."


$SnakeMapTemplates::usage="$SnakeMapTemplates lists the template names can be used for snake map."
$SnakeDirections::usage="$SnakeDirections gives the valid direction names."
$SnakeGameRates::usage="$SnakeGameRates is a settable global variable that specifies the game rates."
$DefaultSnakeMapSize::usage="$DefaultSnakeMapSize is the default setting for the size of snake map."
$DefaultSnakeInitialDirection::usage="$DefaultSnakeInitialDirection is the default setting for the initial direction of snake map."
$gameover::usage="$gameover is an internal symbol."


Options[PlaySnakeGame]=
  {
    "SpeedLevel"->Automatic,
    "MapSize"->Automatic,
    "StartingPoint"->Automatic,
    "InitialDirection"->Automatic
  }
Options[NewSnakeGame]=
  {
    "MapSize"->Automatic,
    "StartingPoint"->Automatic,
    "InitialDirection"->Automatic
  }
Options[CreateSnakeMap]=
  {
    "MapSize"->Automatic,
    "StartingPoint"->Automatic,
    "InitialDirection"->Automatic,
    "Walls"->{}
  }


SyntaxInformation[PlaySnakeGame]={"ArgumentsPattern"->{_.,OptionsPattern[]}}
SyntaxInformation[NewSnakeGame]={"ArgumentsPattern"->{_.,OptionsPattern[]}}
SyntaxInformation[SaveSnakeGame]={"ArgumentsPattern"->{_,_,OptionsPattern[]}}
SyntaxInformation[LoadSnakeGame]={"ArgumentsPattern"->{_,OptionsPattern[]}}
SyntaxInformation[CreateSnakeMap]={"ArgumentsPattern"->{_.,OptionsPattern[]}}
SyntaxInformation[SaveSnakeMap]={"ArgumentsPattern"->{_,_,OptionsPattern[]}}
SyntaxInformation[LoadSnakeMap]={"ArgumentsPattern"->{_,OptionsPattern[]}}


SetAttributes[{PlaySnakeGame,NewSnakeGame,SaveSnakeGame,LoadSnakeGame,CreateSnakeMap,SaveSnakeMap,LoadSnakeMap},ReadProtected]
SetAttributes[{SnakeGame,SnakeMap},ReadProtected]


(* ::Subsection:: *)
(*GUI*)


BeginPackage["`GUI`"]


ExecSnake::usage="ExecSnake[game,\[Ellipsis]] execute the snake game."


EndPackage[]


(* ::Section:: *)
(*Implement*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*String Table*)


PlaySnakeGame::unkarg="Unknown argument `1` at postion `3` in `2`.
Only snake game, snake map or map template is valid."
NewSnakeGame::unkarg="Unknown argument `1` at postion `3` in `2`.
Only snake map or map template is valid."
CreateSnakeMap::unkarg="Unknown argument `1` at postion `3` in `2`.
Only map template is valid."


SnakeMap::invsiz="Size specification `1` is invalid. Use default setting instead."
SnakeMap::invpos="Postion specification `1` is invalid. Use default setting instead."


SnakeGame::invdrt="Direction specification `1` is invalid. Use default setting instead."
SnakeGame::invspd="Speed specification `1` is invalid. Use default setting instead."
SnakeGame::self="Ouroboros!"
SnakeGame::wall="Hit the wall!"


(* ::Subsection::Closed:: *)
(*General Utilities*)


archiveFileQ[file:(_String|_File)]:=StringMatchQ[FileExtension[file],"SAV",IgnoreCase->True]
mapFileQ[file:(_String|_File)]:=StringMatchQ[FileExtension[file],"MAP",IgnoreCase->True]
mapTemplateQ[temp_String]:=MemberQ[$SnakeMapTemplates,temp]


validMapSizeQ=MatchQ[{w_Integer/;w>4,l_Integer/;l>4}|(sl_Integer/;sl>4)]
validDirectionQ:=validDirectionQ=MatchQ[Alternatives@@$SnakeDirections]


SetAttributes[oppositeDirectionQ,Orderless]
oppositeDirectionQ["Up","Down"]=True
oppositeDirectionQ["Left","Right"]=True
oppositeDirectionQ[_,_]=False


(* ::Subsection::Closed:: *)
(*Variable*)


(* ::Subsubsection::Closed:: *)
(*Read-only*)


$SnakeDirections={"Up","Down","Right","Left"}
$SnakeMapTemplates={"Empty","Enclosed","Random"}


(* ::Subsubsection::Closed:: *)
(*Settable*)


(* ::Text:: *)
(*Maybe PersistentValue is better?*)


$SnakeGameRates/:Unset[$SnakeGameRates]:=$SnakeGameRates={3.0,2.0,1.0,0.6,0.3}
$SnakeGameRates=.
$iSnakeGameRates:=If[VectorQ[$SnakeGameRates,Positive],$SnakeGameRates,$SnakeGameRates=.]


$DefaultSnakeMapSize/:Unset[$DefaultSnakeMapSize]:=$DefaultSnakeMapSize={16,16}
$DefaultSnakeMapSize=.
$iDefaultSnakeMapSize:=If[validMapSizeQ[$DefaultSnakeMapSize],$DefaultSnakeMapSize,$DefaultSnakeMapSize=.]


$DefaultSnakeInitialDirection/:Unset[$DefaultSnakeInitialDirection]:=$DefaultSnakeInitialDirection="Left"
$DefaultSnakeInitialDirection=.
$iDefaultSnakeInitialDirection:=If[validDirectionQ[$SnakeDirections],$DefaultSnakeInitialDirection,$DefaultSnakeInitialDirection=.]


(* ::Subsection::Closed:: *)
(*Map*)


(* ::Subsubsection::Closed:: *)
(*Underlying*)


(* ::Text:: *)
(*SnakeMap data layout:*)
(*SnakeMap[{length, width}, walls, {init-snake, init-direction}]*)


initSnakeMap[]:=SnakeMap[{0,0},{},{{},$iDefaultSnakeInitialDirection}](*Invalid to use!*)


getMapSize[map_SnakeMap]:=map[[1]]
getWalls[map_SnakeMap]:=map[[2]]
getInitSnake[map_SnakeMap]:=map[[3,1]]
getInitDirection[map_SnakeMap]:=map[[3,2]]


setMapSize[map_SnakeMap,size_]:=ReplacePart[map,1->size]
setWalls[map_SnakeMap,walls_]:=ReplacePart[map,2->walls]
setInitSnake[map_SnakeMap,snake_]:=ReplacePart[map,{3,1}->snake]
setInitDirection[map_SnakeMap,direct_]:=ReplacePart[map,{3,2}->direct]


(*currying*)
Do[
  With[{f=f},f[arg_][map_]:=f[map,arg]],
  {f,{setMapSize,setWalls,setInitSnake,setInitDirection}}
]


(* ::Subsubsection::Closed:: *)
(*helper*)


getMapPositions[map_SnakeMap]:=Flatten[CoordinateBoundingBoxArray[{{1,1},getMapSize@map}],1]


mapPosition[Automatic,size_]:=mapPosition[Scaled[1/2],size]
mapPosition[Scaled[s_],size_]:=mapPosition[Scaled[{s,s}],size]
mapPosition[Scaled[{sx_,sy_}],{w_,l_}]:=Round@RescalingTransform[{{0,1},{0,1}},{{1,w},{1,l}}][{sx,sy}]
mapPosition[{x_Real,y_Real},{w_,l_}]:=mapPosition[Round@{x,y},{w,l}]
mapPosition[{x_Integer,y_Integer},{w_,l_}]:=Mod[{x,y},{w,l},1]
mapPosition[pos_,size_]:=(Message[SnakeMap::invpos,pos];mapPosition[Automatic,size])
mapPosition[size_][pos_]:=mapPosition[pos,size]


(* ::Subsubsection::Closed:: *)
(*Create*)


parseCreateMapOpt["MapSize",OptionsPattern[CreateSnakeMap]]:=With[{val=OptionValue["MapSize"]},
    If[ListQ[val],val,{val,val}]/;validMapSizeQ[val]
  ]
parseCreateMapOpt["MapSize",OptionsPattern[CreateSnakeMap]]:=
  $iDefaultSnakeMapSize/;OptionValue["MapSize"]===Automatic
parseCreateMapOpt["MapSize",OptionsPattern[CreateSnakeMap]]:=(
  Message[SnakeMap::invsiz,OptionValue["MapSize"]];
  $iDefaultSnakeMapSize)
parseCreateMapOpt["StartingPoint",opts:OptionsPattern[CreateSnakeMap]]:=
    mapPosition[OptionValue["StartingPoint"],parseCreateMapOpt["MapSize",opts]]
parseCreateMapOpt["InitialDirection",OptionsPattern[CreateSnakeMap]]:=
  With[{val=OptionValue["InitialDirection"]},
    val/;validDirectionQ[val]
  ]
parseCreateMapOpt["InitialDirection",OptionsPattern[CreateSnakeMap]]:=
  $iDefaultSnakeInitialDirection/;OptionValue["InitialDirection"]===Automatic
parseCreateMapOpt["InitialDirection",OptionsPattern[CreateSnakeMap]]:=(
  Message[SnakeGame::invdrt,OptionValue["InitialDirection"]];
  $iDefaultSnakeInitialDirection)
parseCreateMapOpt["Walls",opts:OptionsPattern[CreateSnakeMap]]:=
  mapPosition[parseCreateMapOpt["MapSize",opts]]/@OptionValue["Walls"]//DeleteDuplicates


parseCreateMapOpts[opts:OptionsPattern[CreateSnakeMap]]:=
  <|(#->parseCreateMapOpt[#,opts]&)/@Keys@Options[CreateSnakeMap]|>


generateInitSnake[pos_,direct_,size_]:={movedHead[pos,size,direct],pos}


generateWalls["Empty",_]:={}
generateWalls["Enclosed",{w_,l_}]:=Union[
    Thread[{1,Range[l]}],
    Thread[{w,Range[l]}],
    Thread[{Range[w],1}],
    Thread[{Range[w],l}]
  ]
generateWalls["Random",{w_,l_}]:=
  RandomVariate[DiscreteUniformDistribution[{{1,w},{1,l}}],RandomInteger[{1,w*l/2}]]//DeleteDuplicates


iCreateSnakeMap[temp_String,opts_Association]:=
  With[{size=opts["MapSize"],direct=opts["InitialDirection"]},
    initSnakeMap[]//setMapSize[size]
                  //setWalls[opts["Walls"]~Union~generateWalls[temp,size]]
                  //setInitDirection[direct]
                  //setInitSnake[generateInitSnake[opts@"StartingPoint",direct,size]]
  ]


(* ::Subsubsection::Closed:: *)
(*Application*)


CreateSnakeMap[opts:OptionsPattern[]]:=CreateSnakeMap["Empty",opts]
CreateSnakeMap[temp_String?mapTemplateQ,opts:OptionsPattern[]]:=iCreateSnakeMap[temp,parseCreateMapOpts[opts]]
CreateSnakeMap[arg_,opts:OptionsPattern[]]:=(
  Message[CreateSnakeMap::unkarg,arg,HoldForm@CreateSnakeMap[arg,opts],1];
  $Failed)


(* ::Subsection::Closed:: *)
(*Game*)


(* ::Subsubsection::Closed:: *)
(*Underlying*)


(* ::Text:: *)
(*SnakeGame data layout:*)
(*SnakeGame[map, {body, direction, growing}, bonus, score]*)


initSnakeGame[]:=SnakeGame[initSnakeMap[],{{},$iDefaultSnakeInitialDirection,False},{0,0},0](*Invalid to use!*)


getMap[game_SnakeGame]:=game[[1]]
getSnakeBody[game_SnakeGame]:=game[[2,1]]
getSnakeDirection[game_SnakeGame]:=game[[2,2]]
isGrowing[game_SnakeGame]:=game[[2,3]]
getBonus[game_SnakeGame]:=game[[3]]
getScore[game_SnakeGame]:=game[[4]]


setMap[game_SnakeGame,map_SnakeMap]:=ReplacePart[game,1->map]
setSnakeBody[game_SnakeGame,body_List]:=ReplacePart[game,{2,1}->body]
setSnakeDirection[game_SnakeGame,direct_]:=ReplacePart[game,{2,2}->direct]
setGrowing[game_SnakeGame,growing:(True|False)]:=ReplacePart[game,{2,3}->growing]
setBonus[game_SnakeGame,bonus_List]:=ReplacePart[game,3->bonus]
setScore[game_SnakeGame,score_Integer]:=ReplacePart[game,4->score]


(*currying*)
Do[
  With[{f=f},f[arg_][game_]:=f[game,arg]],
  {f,{setMap,setSnakeBody,setSnakeDirection,setGrowing,setBonus,setScore}}
]


(* ::Subsubsection::Closed:: *)
(*helper*)


getMapSize[game_SnakeGame]:=getMapSize@getMap[game]
getMapWidth[game_SnakeGame]:=getMapWidth@getMap[game]
getMapLength[game_SnakeGame]:=getMapLength@getMap[game]
getWalls[game_SnakeGame]:=getWalls@getMap[game]
getEmptyGround[game_SnakeGame]:=Complement[
    getMapPositions@getMap@game,
    getWalls@game,
    getSnakeBody@game
  ]


(* ::Subsubsection::Closed:: *)
(*Movement/Update*)


turnTo[game_SnakeGame,Inherited]:=game
turnTo[game_SnakeGame,direct_]/;oppositeDirectionQ[direct,getSnakeDirection@game]:=game
turnTo[game_SnakeGame,direct_]:=setSnakeDirection[game,direct]
turnTo[direct_][game_SnakeGame]:=turnTo[game,direct]


movedHead[head_,size_,"Up"]   :=mapPosition[head+{0, 1},size]
movedHead[head_,size_,"Down"] :=mapPosition[head+{0,-1},size]
movedHead[head_,size_,"Left"] :=mapPosition[head+{-1,0},size]
movedHead[head_,size_,"Right"]:=mapPosition[head+{ 1,0},size]
moveSnake[game_SnakeGame]:=
  With[{body=getSnakeBody@game},
    setSnakeBody[game,
      Prepend[
        If[isGrowing@game,body,Most@body],
        movedHead[First@body,getMapSize@game,getSnakeDirection@game]
      ]
    ]
  ]


validateSnake[game_SnakeGame]/;(!DuplicateFreeQ@getSnakeBody@game):=Throw[SnakeGame::self,$gameover]
validateSnake[game_SnakeGame]/;IntersectingQ[getSnakeBody@game,getWalls@game]:=Throw[SnakeGame::wall,$gameover]
validateSnake[game_SnakeGame]:=game


eatenQ[game_SnakeGame]:=getBonus@game===First@getSnakeBody@game
generateBonus[game_SnakeGame]:=setBonus[game,RandomChoice[getEmptyGround[game]]]
increaseScore[game_SnakeGame]:=setScore[game,getScore[game]+1]
maybeBonus[game_SnakeGame]/;eatenQ[game]:=setGrowing[game,True]//increaseScore//generateBonus
maybeBonus[game_SnakeGame]:=setGrowing[game,False]


update[game_SnakeGame,turnto_]:=game//turnTo[turnto]//moveSnake//validateSnake//maybeBonus


(* ::Subsubsection::Closed:: *)
(*New/Renew*)


resolveSpeed[Automatic|Inherited]:=Floor[Length@$iSnakeGameRates/2]
resolveSpeed[speed_Integer]:=speed/;0<speed<=Length@$iSnakeGameRates
resolveSpeed[speed_]:=(Message[SnakeGame::invspd,speed];resolveSpeed[Automatic])


iNewSnakeGame[map_SnakeMap]:=
  With[
    {snake=getInitSnake@map,
     direct=getInitDirection@map},
    initSnakeGame[]//setMap[map]
                   //setSnakeBody[snake]
                   //setSnakeDirection[direct]
                   //generateBonus
  ]


(* ::Subsubsection::Closed:: *)
(*Application*)


PlaySnakeGame[opts:OptionsPattern[]]:=
  PlaySnakeGame[NewSnakeGame@@FilterRules[{opts},OptionsPattern[NewSnakeGame]],opts]
PlaySnakeGame[game_SnakeGame,opts:OptionsPattern[]]:=
  ExecSnake[game,resolveSpeed@OptionValue["SpeedLevel"]]
PlaySnakeGame[map_SnakeMap,opts:OptionsPattern[]]:=
  PlaySnakeGame[NewSnakeGame[map,Sequence@@FilterRules[{opts},OptionsPattern[NewSnakeGame]]],opts]
PlaySnakeGame[game_String?archiveFileQ,opts:OptionsPattern[]]:=PlaySnakeGame[LoadSnakeGame[game],opts]
PlaySnakeGame[map_String?mapFileQ,opts:OptionsPattern[]]:=
  PlaySnakeGame[NewSnakeGame[map,Sequence@@FilterRules[{opts},OptionsPattern[NewSnakeGame]]],opts]
PlaySnakeGame[map_String?mapTemplateQ,opts:OptionsPattern[]]:=
  PlaySnakeGame[NewSnakeGame[map,Sequence@@FilterRules[{opts},OptionsPattern[NewSnakeGame]]],opts]
PlaySnakeGame[game_File?archiveFileQ,opts:OptionsPattern[]]:=PlaySnakeGame[LoadSnakeGame[game],opts]
PlaySnakeGame[map_File?mapFileQ,opts:OptionsPattern[]]:=
  PlaySnakeGame[NewSnakeGame[map,Sequence@@FilterRules[{opts},OptionsPattern[NewSnakeGame]]],opts]
PlaySnakeGame[arg_,opts:OptionsPattern[]]:=(
  Message[PlaySnakeGame::unkarg,arg,HoldForm@PlaySnakeGame[arg,opts],1];
  $Failed)


NewSnakeGame[opts:OptionsPattern[]]:=NewSnakeGame[CreateSnakeMap@@FilterRules[{opts},Options[CreateSnakeMap]],opts]
NewSnakeGame[map_SnakeMap,OptionsPattern[]]:=iNewSnakeGame[map]
NewSnakeGame[map_String?mapFileQ,opts:OptionsPattern[]]:=NewSnakeGame[LoadSnakeMap[map,opts]]
NewSnakeGame[map_String?mapTemplateQ,opts:OptionsPattern[]]:=
  NewSnakeGame[CreateSnakeMap[map,Sequence@@FilterRules[{opts},Options[CreateSnakeMap]]],opts]
NewSnakeGame[map_File,opts:OptionsPattern[]]:=NewSnakeGame[LoadSnakeMap[map,opts]]
NewSnakeGame[map_LocalObject,opts:OptionsPattern[]]:=NewSnakeGame[LoadSnakeMap[map,opts]]
NewSnakeGame[map_CloudObject,opts:OptionsPattern[]]:=NewSnakeGame[LoadSnakeMap[map,opts]]
NewSnakeGame[arg_,opts:OptionsPattern[]]:=(
  Message[NewSnakeGame::unkarg,arg,HoldForm@NewSnakeGame[arg,opts],1];
  $Failed)


(* ::Subsection::Closed:: *)
(*GUI*)


runIcon=Graphics[{Lighter@Green,Triangle[{{0.2,0.2},{0.2,0.8},{0.8,0.5}}]},PlotRange->{{0,1},{0,1}},ImageSize->32]
runHoverIcon=Graphics[{Green,Triangle[{{0.2,0.2},{0.2,0.8},{0.8,0.5}}]},PlotRange->{{0,1},{0,1}},ImageSize->32]
stopIcon=Graphics[{Lighter@Red,Rectangle[{0.2,0.2},{0.4,0.8}],Rectangle[{0.6,0.2},{0.8,0.8}]},PlotRange->{{0,1},{0,1}},ImageSize->32]
stopHoverIcon=Graphics[{Red,Rectangle[{0.2,0.2},{0.4,0.8}],Rectangle[{0.6,0.2},{0.8,0.8}]},PlotRange->{{0,1},{0,1}},ImageSize->32]
overIcon=Graphics[{Lighter@Gray,Thickness[0.15],Circle[{0.5,0.5},0.35],Line[{{0.27,0.27},{0.73,0.73}}]},PlotRange->{{0,1},{0,1}},ImageSize->32]


bonusBasePrimitive=
  (ImplicitRegion[(x^2+y^2-1)^3<=x^2*y^3,{x,y}]//Region//Show//First)/.
    {Directive[_]:>Directive[Red,EdgeForm[Yellow]]}


mapPrimitives[map_SnakeMap]:={
    {Brown,Rectangle[{1,1},getMapSize[map]+1]},
    {Gray,Rectangle/@getWalls[map]}
  }
snakePrimitives[game_SnakeGame]:={Darker@Green,Rectangle/@getSnakeBody[game]}
bonusPrimitive[game_SnakeGame]:=
  GeometricTransformation[bonusBasePrimitive,AffineTransform[{{{1/3,0},{0,1/3}},getBonus[game]+1/2}]]


SnakeMap/:Show[map_SnakeMap]:=Graphics@mapPrimitives[map]
SnakeGame/:Show[game_SnakeGame]:=
  Graphics[
    {snakePrimitives[game],bonusPrimitive[game]},
    Prolog->mapPrimitives@getMap[game],
    PlotRange->Transpose@{{1,1},1+getMapSize@game}
  ]


(* ::Subsubsection::Closed:: *)
(*Game Panel*)

showFinalScore[nb_NotebookObject]:=
  TemplateApply["Your final score is: `1`.",
    {getScore@CurrentValue[nb, {TaggingRules, "Game"}]}
  ]
whenGameOver[nb_NotebookObject][msg_,$gameover]:=
  If[CurrentValue[nb,{TaggingRules,"RunStatus"}]=!="Removed",
    removeDriverTask[nb];
    MessageDialog[
      Column[{msg, showFinalScore[nb]}, Alignment->Left],
      {
        "Quit":>NotebookClose[nb]
      },
      WindowTitle->"Game Over!"
    ]
  ]
removeDriverTask[nb_NotebookObject]:=
  With[
    {task=CurrentValue[nb,{TaggingRules,"Driver"}],
      run:=CurrentValue[EvaluationNotebook[],{TaggingRules,"RunStatus"}]},
    If[run=!="Removed",
      run="Removed";
      TaskRemove[task] (*remove it if already exists*)
    ]
  ]
setDriverTask[nb_NotebookObject,speed_Integer]:=(
  CurrentValue[nb,{TaggingRules,"Driver"}]=SessionSubmit@ScheduledTask[
    With[{
      game:=CurrentValue[nb,{TaggingRules,"Game"}],
      turnto:=CurrentValue[nb,{TaggingRules,"TurningTo"}]
    },
      Catch[
        game=update[game,turnto];
        turnto=Inherited,
        $gameover,
        whenGameOver[nb]
      ]
    ],
    $iSnakeGameRates[[speed]]
  ];
  CurrentValue[nb,{TaggingRules,"RunStatus"}]="Running";
)
resetDriverTask[nb_NotebookObject,speed_Integer]:=(
  removeDriverTask[nb];
  setDriverTask[nb,speed];
)

actionToggleRunStatus[]:=With[
    {task=CurrentValue[EvaluationNotebook[],{TaggingRules,"Driver"}],
     run:=CurrentValue[EvaluationNotebook[],{TaggingRules,"RunStatus"}]},
    Switch[run,
      "Running",
        TaskSuspend[task];
        run="Suspended";,
      "Suspended",
        TaskResume[task];
        run="Running";
    ]
  ]
actionTurnTo[direct_]:=
  If[CurrentValue[EvaluationNotebook[],{TaggingRules,"RunStatus"}]==="Running",
    CurrentValue[EvaluationNotebook[], {TaggingRules, "TurningTo"}]=direct
  ]


gameMainUi[speed_]:=
  DynamicModule[{game},
    DynamicWrapper[
      Pane[
        Graphics[{
          Dynamic[snakePrimitives[game]],
          Dynamic[bonusPrimitive[game]]
        },
          Prolog->Dynamic@Refresh[mapPrimitives@getMap[game],None],
          PlotRange->Dynamic@Refresh[Transpose@{{1,1},1+getMapSize@game},None]
        ],
        Alignment->Center
      ],
      game=CurrentValue[EvaluationNotebook[], {TaggingRules, "Game"}]
    ],
    Initialization:>(
      game=CurrentValue[EvaluationNotebook[], {TaggingRules, "Game"}];
      setDriverTask[EvaluationNotebook[],speed];
    ),
    Deinitialization:>(removeDriverTask[EvaluationNotebook[]])
  ]

runStatusToggler:=
  With[{run:=CurrentValue[EvaluationNotebook[],{TaggingRules,"RunStatus"}]},
    Button[
      Dynamic@Switch[run,
        "Running",Mouseover[stopIcon,stopHoverIcon],
        "Suspended",Mouseover[runIcon,runHoverIcon],
        _,overIcon
      ],
      actionToggleRunStatus[],
      Enabled->Dynamic[MatchQ[run,"Running"|"Suspended"]],
      Appearance->"Frameless"
    ]
  ]

scoreView:=Row@{
    "Score: ",
    Dynamic[getScore@CurrentValue[EvaluationNotebook[], {TaggingRules, "Game"}]]
  }
gameToolbar:=
  Grid[{{
    runStatusToggler,
    Null,
    scoreView
  }},
    ItemSize->{Scaled /@ {0.2, 0.6, 0.2}}
  ]


gameEventDispatch={(* TODO: maybe close? *)
  "EscapeKeyDown":>actionToggleRunStatus[],
  "LeftArrowKeyDown":>actionTurnTo["Left"],
  "RightArrowKeyDown":>actionTurnTo["Right"],
  "UpArrowKeyDown":>actionTurnTo["Up"],
  "DownArrowKeyDown":>actionTurnTo["Down"],
  {"KeyDown","A"}:>actionTurnTo["Left"],
  {"KeyDown","D"}:>actionTurnTo["Right"],
  {"KeyDown","W"}:>actionTurnTo["Up"],
  {"KeyDown","S"}:>actionTurnTo["Down"],
  {"KeyDown","a"}:>actionTurnTo["Left"],
  {"KeyDown","d"}:>actionTurnTo["Right"],
  {"KeyDown","w"}:>actionTurnTo["Up"],
  {"KeyDown","s"}:>actionTurnTo["Down"]
}


ExecSnake[game_SnakeGame,speed_Integer]:=
  CreateWindow[
    DialogNotebook[
      gameMainUi[speed],
      TaggingRules->{(*TODO: resolve untracked task object status problem*)
        "Driver"->Automatic,
        "RunStatus"->"Waiting",
        "TurningTo"->Inherited,
        "Game"->game
      },
      NotebookEventActions->gameEventDispatch,
      DockedCells->Cell[BoxData@ToBoxes[gameToolbar],"DockedCells"]
    ]
  ]


(* ::Section:: *)
(*End*)


End[]


Protect[SnakeGame,SnakeMap,$SnakeMapTemplates,$SnakeDirections]


EndPackage[]

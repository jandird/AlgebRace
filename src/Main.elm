module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing(..)
import List
import String
import Dict exposing (Dict)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)
-- questions list is formatted the following way:
-- question, list of possible answers, which answer is correct (0, 1, or 2)

questions = [("3 + x = 20", ("16", "31", "17"), 2), ("2x = 10", ("5", "6", "7"), 0), ("16 / x = 4", ("4", "5", "16"), 0), ("- 1 + x = 2", ("2", "3", "1"), 1),  ("3 - x = 20", ("-16", "-31", "-17"), 2), ("5x = 10", ("2", "3", "1"), 0), ("68 - x = 4", ("64", "-64", "-46"), 0), ("x / 2 = 16", ("-8", "8", "16"), 1) ]

-- possible pages: Main Menu, Tutorial, Play Game, Game Over
myShapes model = let 
                   playerPos = model.playerPos
                   oppPos = model.oppPos
                   page = model.currentPage
                   currQ = model.currentQuestion
                   direction = model.direction

                   blockA = model.blockA
                   blockB = model.blockB
                   blockC = model.blockC
                 in
                 [ square 300 |> filled green]
                 ++
                   [
                   if page == "Main Menu" then
                     group
                       [ 
                        circle 60 |> outlined (solid 20) darkGray
                        , circle 60 |> outlined (dashed 1) yellow
                        , if model.direction == "left"
                            then
                            group
                            [
                              redCar |> scale 0.25 |> move (sin -model.time*53 , cos -model.time*53)
                            , blueCar |> scale 0.25 |> move (sin model.time*65 , cos model.time*65)
                            ]
                          else
                            group
                            [
                              redCar |> scale 0.25 |> move (sin model.time*53 , cos model.time*53)
                            , blueCar |> scale 0.25 |> move (sin -model.time*65 , cos -model.time*65)
                            ]
                        , rect 62 12 |> filled (lightBlue) |> move (0, 5) |> notifyTap (ChangePage "Play Game")
                        , rect 62 12 |> filled (lightBlue) |> move (0, -9) |> notifyTap (ChangePage "Tutorial")
                        , rect 62 12 |> filled (lightBlue) |> move (0, -23) |> notifyTap (ChangePage "Algebra")
                        , text "ALGEB-RACE" |> centered |> sansserif |> bold |> underline |> filled white |> move (0, 20) |> notifyTap (ChangeDir)
                        , text "START" |> centered |> sansserif |> size 7 |> filled white |> move ( 0, 2) |> notifyTap (ChangePage "Play Game")
                        , text "HOW TO PLAY" |> centered |> sansserif |> size 7 |> filled white |> move ( 0, -12) |> notifyTap (ChangePage "Tutorial")
                        , text "ALGEBRA BASICS" |> centered |> sansserif |> size 6.5 |> filled white |> move ( 0, -26) |> notifyTap (ChangePage "Algebra")
                       ]
                   else if page == "Tutorial" then 
                     group 
                       [ text "How To Play" |> centered |> sansserif |> bold |> underline |> filled white |> move (0, 50)
                         , text "- Solve for x in each question" |> sansserif |> size 6 |> filled (rgb 100 0 100) |> move (-60,35)
                         , text "- Click the correct answer to make the red car move" |> sansserif |> size 6 |> filled (rgb 100 0 100) |> move (-60,25)
                         , text "- Answer fast before the blue car wins!" |> sansserif |> size 6 |> filled (rgb 100 0 100) |> move (-60,15)
                         , mainMenuButton |> move (0, -30) |> notifyTap (ChangePage "Main Menu")
                         --, 
                         
                       ]
                   else if page == "Algebra" then 
                     group
                       [ text "Adding and subtracting algebraic expressions:" |> sansserif |> size 5 |> filled (rgb 100 0 100) |> move (-70,40)
                         , text "- Isolate for x! For example:" |> sansserif |> size 4 |> filled (rgb 100 0 100) |> move (-65,33)
                         , text "x + 2 = 6" |> sansserif |> size 5 |> filled (rgb 100 0 100) |> move (-15,26)
                         , text "x + 2 - 2 = 6 - 2" |> sansserif |> size 5 |> filled (rgb 100 0 120) |> move (-22,20)
                         , text "x = 4" |> sansserif |> size 5 |> filled red |> move (-6,14)
                         , text "Multiplying and dividing algebraic expressions:" |> sansserif |> size 5 |> filled (rgb 100 0 120) |> move (-70,5)
                         , text "- Do the same, isolate for x! For example:" |> sansserif |> size 4 |> filled (rgb 100 0 100) |> move (-65,-2)
                         , text "3x = 15" |> sansserif |> size 5 |> filled (rgb 100 0 100) |> move (-15,-9)
                         , text "3x ÷ 3 = 15 ÷ 3" |> sansserif |> size 5 |> filled (rgb 100 0 120) |> move (-23,-15)
                         , text "x = 5" |> sansserif |> size 5 |> filled red |> move (-12,-21)
                         , mainMenuButton |> move (40, -35) |> notifyTap (ChangePage "Main Menu")
                         --, nextStepButton |> move (-45, -35) |> notifyTap (AddLine)
                       ]
                   else if page == "Game Over" then
                    group
                      [
                        rect 300 10 |> filled (lightGray)
                        , if model.winner == "red" then 
                          group [
                            text "You Win! :)" |> sansserif |> centered |> filled red |> move (0, 25)
                            , redCar |> scale 0.25 |> move (sin model.time*53 , 0)
                          ]
                        else 
                          group [
                            text "You Lose :(" |> sansserif |> centered |> filled blue |> move (0, 25)
                            , blueCar |> scale 0.25 |> move (sin model.time*53 , 0)
                          ]
                        , playAgainButton |> move (-40, -30) |> notifyTap (ChangePage "Play Game")
                        , mainMenuButton |> move (40, -30) |> notifyTap (ChangePage "Main Menu")
                      ]
                   else -- Play Game page
                     group
                       [ rect 20 200 |> filled (lightGray) |> move (70, 0)
                         , rect 20 200 |> filled (lightGray) |> move (-70, 0)
                         , finishLine |> move (0, 50)
                         , redCar |> rotate(degrees 90) |> scale 0.25 |> move (playerPos) 
                         , blueCar |> rotate(degrees -90) |> scale 0.25 |> move (oppPos)
                         -- , enterButton |> move (0, -30) |> notifyTap (UpdatePos)
                         , question currQ
                         , wrongAnswer blockA blockB blockC
                         ]
                 ]
                 
question (q, (a, b, c), correct) = group
                  [ text (q) |> centered |> filled (black) |> move (0, 20) 
                  , if correct == 0 then 
                  group
                    [ questionButton a|> move (0, 0) |> notifyTap (UpdatePos)
                   , questionButton b |> move (0, -15) |> notifyTap (WrongAnswer "b")
                   , questionButton c |> move (0, -30) |> notifyTap (WrongAnswer "c")
                  ] 
                  else if correct == 1  then
                  group 
                    [questionButton a |> move (0, 0) |> notifyTap (WrongAnswer "a")
                   , questionButton b |> move (0, -15) |> notifyTap (UpdatePos)
                   , questionButton c |> move (0, -30) |> notifyTap (WrongAnswer "c")
                  ]
                  else
                  group 
                    [questionButton a |> move (0, 0) |> notifyTap (WrongAnswer "a")
                   , questionButton b |> move (0, -15) |> notifyTap (WrongAnswer "b")
                   , questionButton c |> move (0, -30) |> notifyTap (UpdatePos)
                  ]
                  ]
wrongAnswer blockA blockB blockC = group
              [ if blockA == True
                  then
                    questionBlock
                else
                  group []

                ,if blockB == True
                  then
                    questionBlock |> move(0, -15)
                else
                  group []

                ,if blockC == True 
                  then
                    questionBlock |> move(0, -30)
                else
                  group []
               ]
redCar = group   
         [ roundedRect 100 25 20
            |> filled red
        , wedge 30 0.5
            |> filled red
            |> rotate (degrees 90)
            |> move (0,8)
        , circle 10
            |> filled black
            |> move(-20,-10)
        , circle 10
            |> filled black
            |> move(20,-10)
        , wedge 20 0.5
            |> filled white
            |> rotate (degrees 90)
            |> move(0,12)
        , circle 5
            |> filled white
            |> move(20,-10)
        , circle 5
            |> filled white
            |> move(-20,-10)
        ]

blueCar = group 
          [  roundedRect 100 25 20
            |> filled blue
        , wedge 30 0.5
            |> filled blue
            |> rotate (degrees 90)
            |> move (0,8)
        , circle 10
            |> filled black
            |> move(-20,-10)
        , circle 10
            |> filled black
            |> move(20,-10)
        , wedge 20 0.5
            |> filled white
            |> rotate (degrees 90)
            |> move(0,12)
        , circle 5
            |> filled white
            |> move(20,-10)
        , circle 5
            |> filled white
            |> move(-20,-10)
          ]
      
finishLine = group 
              [ rect 500 3 |> filled (black)
                , text ("FINISH!") |> sansserif |> size 10 |> centered |> filled black  |> move(0,5) 
              ]

enterButton = group 
              [ rect 30 10 |> filled (green)
                , text ("SUBMIT") |> sansserif |> size 8 |> centered |> filled white |> move (0, -2)
              ]
              
playAgainButton = group
              [ rect 62 12 |> filled (lightBlue)
              , text ("Play again") |> sansserif |> size 8 |> centered |> filled white |> move (0, -2)
              ]

mainMenuButton = group
              [ rect 62 12 |> filled (lightBlue)
              , text ("Main Menu") |> sansserif |> size 8 |> centered |> filled white |> move (0, -2)
              ]

questionButton answer = group
                      [ rect 30 10 |> filled grey
                      , text (answer) |> sansserif |> size 8 |> centered |> filled black |> move (0, -2)]

questionBlock = group
                      [ rect 30 10 |> filled green
                      , text "WRONG!" |> sansserif |> size 8 |> centered |> filled red |> move (0, -2)]                     

type Msg = Tick Float GetKeyState | UpdatePos | WrongAnswer String | ChangePage String | ChangeDir

update msg model = case msg of
                     Tick t _ -> let
                                   (oppX, oppY) = model.oppPos
                                   (playerX, playerY) = model.playerPos
                                   currentPage = model.currentPage
                                 in
                                 
                                 -- if oppPos y value has passed finish line (50), user loses
                                 if currentPage == "Play Game" then 
                                   if oppY > 50
                                     then
                                       { model | time = t
                                             , oppPos = (oppX, 50)
                                             , winner = "blue"
                                             , currentPage = "Game Over"
                                       }
                                     else if playerY > 50 
                                       then 
                                         { model | time = t
                                               , winner = "red"
                                               , currentPage = "Game Over"
                                         }
                                     else 
                                       { model | time = t
                                             , oppPos = (oppX , oppY + 0.2)
                                       }
                                  else 
                                      { model | time = t}
                                 
                     UpdatePos -> let 
                                        (x, y) = model.playerPos
                                      in 
                                      { model | playerPos = (-70, y + 35)
                                      , currentQuestion =  Maybe.withDefault ("x + 4 = 10", ("-2", "-6", "6"), 2) <| List.head <| List.drop 1 model.questions
                                      , questions = List.drop 1 model.questions
                                      , blockA = False, blockB = False, blockC = False
                                      }

                     WrongAnswer ans -> let (x, y) = model.playerPos in 
                                        if y > -50 then 
                                          if ans == "a"
                                          then {model | blockA = True, playerPos = (-70, y - 35)}
                                          else if ans == "b"
                                            then {model | blockB = True, playerPos = (-70, y - 35)}
                                          else
                                            {model | blockC = True, playerPos = (-70, y - 35)}
                                        else 
                                          if ans == "a"
                                            then {model | blockA = True, playerPos = (-70, y)}
                                          else if ans == "b"
                                            then {model | blockB = True, playerPos = (-70, y)}
                                          else
                                            {model | blockC = True, playerPos = (-70, y)}          
                                       
                     ChangePage page -> if page == "Play Game" 
                                         then
                                           { model | currentPage = page
                                           , playerPos = (-70, -50)
                                           , oppPos = (70, -50)
                                           , winner = " "
                                           , questions = questions
                                           , blockA = False
                                           , blockB = False
                                           , blockC = False}
                                         else
                                           { model | currentPage = page }

                     ChangeDir -> if model.direction == "left"
                                        then 
                                          { model | direction = "right" }
                                        else 
                                          { model | direction = "left" }
init = { time = 0
         , playerPos = (-70, -50)
         , oppPos = (70, -50)
         , winner = " "
         , currentPage = "Main Menu"
         , questions = questions
         , blockA = False
         , blockB = False
         , blockC = False
         , direction = "right"
         , currentQuestion = Maybe.withDefault ("0", ("0", "0", "0"), 0) <| List.head questions
         }
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

questions = [("2x = 10", ("5", "6", "7"), 0)]

-- possible pages: Main Menu, Tutorial, Play Game, Game Over
myShapes model = let 
                   playerPos = model.playerPos
                   oppPos = model.oppPos
                   page = model.currentPage
                 in
                 [ square 300 |> filled green]
                 ++
                   [
                   if page == "Main Menu" then
                     group
                       [ 
                        circle 60 |> outlined (solid 20) darkGray
                        , circle 60 |> outlined (dashed 1) yellow
                        , redCar |> scale 0.25 |> move (sin -model.time*53 , cos -model.time*53)
                        , blueCar |> scale 0.25 |> move (sin model.time*65 , cos model.time*65)
                        , rect 62 12 |> filled (lightBlue) |> move (0, 5)
                        , rect 62 12 |> filled (lightBlue) |> move (0, -9)
                        , rect 62 12 |> filled (lightBlue) |> move (0, -23)
                        , text "ALGEB-RACE" |> centered |> sansserif |> bold |> underline |> filled white |> move (0, 20)
                        , text "START" |> centered |> sansserif |> size 8 |> filled white |> move ( 0, 2) |> notifyTap (ChangePage "Play Game")
                        , text "HOW TO PLAY" |> centered |> sansserif |> size 8 |> filled white |> move ( 0, -12) |> notifyTap (ChangePage "Tutorial")
                        , text "HOW TO MATH" |> centered |> sansserif |> size 8 |> filled white |> move ( 0, -26)
                       ]
                   else if page == "Tutorial" then 
                     group 
                       [ text "This is the tutorial page" |> sansserif |> centered |> filled (rgb 100 0 100) |> move (0,0)
                         , mainMenuButton |> move (30, -30) |> notifyTap (ChangePage "Main Menu")
                       ]
                   else if page == "Game Over" then
                    group
                      [
                        if model.winner == "red" then 
                          text "You Win! :)" |> sansserif |> centered |> filled blue 
                        else 
                          text "You Lose :(" |> sansserif |> centered |> filled red
                        , playAgainButton |> move (-30, -30) |> notifyTap (ChangePage "Play Game")
                        , mainMenuButton |> move (30, -30) |> notifyTap (ChangePage "Main Menu")
                      ]
                   else -- Play Game page
                     group
                       [ rect 20 200 |> filled (lightGray) |> move (70, 0)
                         , rect 20 200 |> filled (lightGray) |> move (-70, 0)
                         , finishLine |> move (0, 50)
                         , redCar |> rotate(degrees 90) |> scale 0.25 |> move (playerPos) 
                         , blueCar |> rotate(degrees -90) |> scale 0.25 |> move (oppPos)
                         , enterButton |> move (0, -30) |> notifyTap (UpdatePos playerPos)
                         , group <| List.map question questions 
                       ]
                 ]
question (q, (a, b, c), correct) = group
                  [ text (q) |> centered |> filled (black) |> move (0, 20) 
                  , if correct == 0 then 
                  group
                    [text (a) |> centered |> filled (black) |> move (0, 0) |> notifyTap (ChangePage "Main Menu")
                   , text (b) |> centered |> filled (black) |> move (0, -10)
                   , text (c) |> centered |> filled (black) |> move (0, -20)
                  ] 
                  else if correct == 1 then
                  group 
                    [text (a) |> centered |> filled (black) |> move (0, 0) 
                   , text (b) |> centered |> filled (black) |> move (0, -10) |> notifyTap (ChangePage "Main Menu")
                   , text (c) |> centered |> filled (black) |> move (0, -20)
                  ]
                  else
                  group 
                    [text (a) |> centered |> filled (black) |> move (0, 0) 
                   , text (b) |> centered |> filled (black) |> move (0, -10) 
                   , text (c) |> centered |> filled (black) |> move (0, -20) |> notifyTap (ChangePage "Main Menu")
                  ]
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
              [ rect 40 10 |> filled (black)
              , text ("Play again") |> sansserif |> size 8 |> centered |> filled white |> move (0, -2)
              ]

mainMenuButton = group
              [ rect 40 10 |> filled (black)
              , text ("Main Menu") |> sansserif |> size 8 |> centered |> filled white |> move (0, -2)
              ]

type Msg = Tick Float GetKeyState | UpdatePos (Float, Float) | ChangePage String

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
                                      { model | time = t }
                                 
                     UpdatePos pos -> let 
                                        (x, y) = pos
                                      in 
                                      { model | playerPos = (-70, y + 35) 
                                      }
                                       
                     ChangePage page -> if page == "Play Game" 
                                         then
                                           { model | currentPage = page
                                           , playerPos = (-70, -50)
                                           , oppPos = (70, -50)
                                           , winner = " "}
                                         else
                                           { model | currentPage = page }

init = { time = 0
         , playerPos = (-70, -50)
         , oppPos = (70, -50)
         , winner = " "
         , currentPage = "Main Menu"
         }



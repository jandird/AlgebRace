module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing(..)
import List
import String
import Dict exposing (Dict)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

questions = [("2x = 10", (5, 8, 7))]

myShapes model = let 
                   playerPos = model.playerPos
                   oppPos = model.oppPos
                   page = model.currentPage
                 in
                   [
                   if page == "Start" then
                     group
                       [ text "ALGEB-RACE" |> sansserif |> bold |> underline |> filled (rgb 100 0 100) |> move (-50, 20)
                        , text "START" |> sansserif |> filled (rgb 100 0 100) |> move (-80, -20) |> notifyTap (ChangePage "Play game")
                        , text "TUTORIAL"  |> sansserif |> filled (rgb 100 0 100) |> move (10, -20) |> notifyTap (ChangePage "Tutorial")
                       ]
                   else if page == "Tutorial" then 
                     group 
                       [ text "This is the tutorial page" |> sansserif |> centered |> filled (rgb 100 0 100) |> move (0,0)
                       ]
                   else 
                     group
                       [ redCar |> move (playerPos)
                         , blueCar |> move (oppPos)
                         , finishLine |> move (0, 50)
                         , enterButton |> move (0, -30) |> notifyTap (UpdatePos playerPos) 
                         , if model.winner /= " "
                         then
                           group
                            [ text (model.winner ++ " wins!") |> centered |> filled red
                            ]
                         else
                           group
                            [ text "Keep going!" |> centered |> filled green 
                                                        ]
                       ]
                 ]

redCar = group 
          [ rect 10 20 |> filled (rgb 225 0 0)
          ]

blueCar = group 
          [ rect 10 20 |> filled (rgb 0 0 225) 
          ]
      
finishLine = group 
              [ rect 500 3 |> filled (black)
                , text ("FINISH!") |> size 10 |> centered |> filled black  |> move(0,5) 
              ]

enterButton = group 
              [ rect 30 10 |> filled (green)
                , text ("SUBMIT") |> size 8 |> centered |> filled white |> move (0, -2)
              ]

type Msg = Tick Float GetKeyState | UpdatePos (Float, Float) | ChangePage String

update msg model = case msg of
                     Tick t _ -> let
                                   (oppX, oppY) = model.oppPos
                                   (playerX, playerY) = model.playerPos
                                   currentPage = model.currentPage
                                 in
                                 
                                 -- if oppPos y value has passed finish line (50), user loses
                                 if currentPage == "Play game" then 
                                   if oppY > 50
                                     then
                                       { model | time = t
                                             , oppPos = (oppX, 50)
                                             , winner = "Blue car"
                                       }
                                     else if playerY > 50 
                                       then 
                                         { model | time = t
                                               , winner = "Red car" 
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
                                       
                     ChangePage page -> { model | currentPage = page }

init = { time = 0
         , playerPos = (-70, -50)
         , oppPos = (70, -50)
         , winner = " "
         , currentPage = "Start"
         }


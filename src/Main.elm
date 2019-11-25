module Main exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing(..)
import List
import String
import Dict exposing (Dict)

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

myShapes model = [  text "ALGEB-RACE" |> sansserif |> bold |> underline 
                                      |> filled (rgb 100 0 100) |> move (-50, 20),
                    text "START" |> sansserif |> filled (rgb 100 0 100) |> move (-80, -20),
                    text "TUTORIAL"  |> sansserif |> filled (rgb 100 0 100) |> move (10, -20)
                 ]

type Msg = Tick Float GetKeyState

update msg model = case msg of
                     Tick t _ -> { time = t }

init = { time = 0}

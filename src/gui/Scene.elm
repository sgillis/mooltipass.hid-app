module Scene where

-- Elm standard library
import Graphics.Input exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)

-- local source
import GuiState exposing (..)
import Navigation exposing (..)
import Layout exposing (..)
import Content exposing (..)

{-| The scene maps the window dimensions and an application state to the main
    'Element'-}
scene : (Int,Int) -> GuiState -> Element
scene dims state = layers [layer1 dims state]

{-| This is currently the only layer, might be useful to easily add layers to
    the scene though -}
layer1 : (Int, Int) -> GuiState -> Element
layer1 dims state =
    flow down [ spacer 1 heights.marginTop
              , navigation dims state
              , content dims state
              ]

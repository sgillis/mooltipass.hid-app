module LogTab where

-- Elm standard library
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Html
import Html.Attributes
import Text -- needed even when unused because of Elm bug #864
import List

-- local source
import Layout exposing (..)
import CommonState exposing (..)
import CustomGraphics exposing (..)
import Actions exposing (..)

{-| Displays the log in a screen with a clear button at the bottom in
    a 'toolbar'. -}
logTab : (Int, Int) -> List String -> Element
logTab (w,h) log =
    let toolbar = container w heights.logTabToolbar middle clearButton
        screenH = h - heights.logTabToolbar - 32 - 16
        screenW = w - 64
        screen' = container w screenH middle <| screen (screenW, screenH) log
    in container w h middle <| flow down [screen', spacer 1 16, toolbar]

{-| The screen that displays the log -}
screen : (Int, Int) -> List String -> Element
screen (w,h) log =
    let background = roundedRect w h grey
        style =
            Html.Attributes.style
                [ ("color", "white")
                , ("font-family", "DejaVu Sans Mono")
                , ("-webkit-user-select", "text")
                , ("font-size", "13px")
                , ("overflow-y", "auto")
                , ("width", toString (w - 32) ++ "px")
                , ("height", toString (h - 32) ++ "px")
                ]
        txt  = Html.div [style]
                (List.intersperse
                    (Html.br [] [])
                    (List.map Html.text
                    (List.reverse log))
                )
                |> Html.toElement (w - 32) (h - 32)
        txt' = container w h middle txt
    in layers [background, txt']

{-| A button that says 'clear' and clears the log -}
clearButton : Element
clearButton = button (message commonActions.address (SetLog [])) "clear"

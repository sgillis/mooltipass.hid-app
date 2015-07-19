module ManualTab where

import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (field, password, defaultStyle)
import Graphics.Collage exposing (collage, filled)
import Signal exposing (message)

import GuiState exposing (..)
import Actions exposing (updateInput, guiActions)
import CustomGraphics exposing (..)
import Layout exposing (heights)

manualTab : (Int, Int) -> GuiState -> Element
manualTab (w,h) s =
    let contentH = h - 32
        contentW = w - 64
        content' = container w contentH middle
            <| content (contentW, contentH) s
    in container w h middle content'

content : (Int, Int) -> GuiState -> Element
content (w,h) s =
    let saveButton = button (message guiActions.address SaveManualCredentials) "save"
        contextField = field defaultStyle
                             (updateInput ManualContext) 
                             "context"
                             (s.inputValues.manualContext)
        userField = field defaultStyle
                             (updateInput ManualUser) 
                             "user"
                             (s.inputValues.manualUser)
        passwordField = password defaultStyle
                             (updateInput ManualPassword)
                             "password"
                             (s.inputValues.manualPassword)
        content' = box (w,h) "Add credentials" <|
            container w (h-100) midTop <| flow down
                [ spacer 5 5
                , contextField
                , spacer 5 5
                , userField
                , spacer 5 5
                , passwordField
                , spacer 5 5
                , saveButton
                ]
    in content'

title : Int -> String -> Element
title w str =
    let ht    = heights.manageTitle
        bg    = collage w ht
           [filled lightGrey <| roundedRectShape Top (toFloat w) (toFloat ht) 5]
        txt   = container w heights.manageTitle middle
            <| leftAligned (whiteText str)
    in layers [bg, txt]

box : (Int, Int) -> String -> Element -> Element
box (w,h) str cont =
    let bg = roundedRect w h grey
    in layers [bg, flow down [title w str, cont]]

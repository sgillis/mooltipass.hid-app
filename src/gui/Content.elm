module Content where

-- Elm standard library
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Maybe

-- local source
import CustomGraphics exposing (..)
import Layout exposing (..)
import GuiState exposing (..)
import LogTab exposing (..)
import DeveloperTab exposing (..)
import ManageTab exposing (..)
import SettingsTab exposing (..)
import ManualTab exposing (manualTab)
import CommonState exposing (..)

{-| Renders the window the window dimensions and application state to the
    element that is below the tab navigation. -}
content : (Int, Int) -> GuiState -> Element
content (w,h) state =
    let h' = h - heights.marginTop - heights.nav
        background =
            collage w h' [filled darkGrey <| rect (toFloat w) (toFloat h)]
        withBackground e = layers [background, e]
    in case state.activeTab of
        Log       -> withBackground <| logTab (w, h') state.common.log
        Developer -> withBackground <| developerTab (w, h') state.common.importInfo
        Manage    -> withBackground <| manageTab (w, h') state
        Settings  -> withBackground <| settingsTab (w, h') state.common.settingsInfo
                                         state.selections state.stageParameters
        Manual    -> withBackground <| manualTab (w, h') state
        _         -> empty

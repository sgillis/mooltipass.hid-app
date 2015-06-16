module Actions where

-- Elm standard library
import List
import Graphics.Input.Field exposing (Content)
import Signal exposing (..)

import Byte exposing (..)
import String exposing (toInt)
import CommonState exposing (..)
import GuiState exposing (..)

{-| The channel that user inputs can 'Signal.send' actions to that affect the
    GUI directly -}
guiActions : Mailbox Action
guiActions = mailbox NoOp

{-| The channel that user inputs can 'Signal.send' actions to that affect the common state with the background. These are sent to the background first before they bubble back up to the 'GuiState'. -}
commonActions : Mailbox CommonAction
commonActions = mailbox CommonNoOp

stageStringContent : StringCmd -> Content -> Message
stageStringContent cmd content = message guiActions.address (StageStringCmd cmd content)

sendGetParameter : Parameter -> Message
sendGetParameter p = message guiActions.address (CommonAction (GetParameter (Just p)))

stageParameter : Parameter -> Byte -> Message
stageParameter p b = message guiActions.address (StageParameter p b)

stageIntContent : Parameter -> Int -> Int -> Content -> Message
stageIntContent p lo hi content = message guiActions.address (StageParameterIntField p lo hi content)

stageParseInt : Parameter -> String -> Message
stageParseInt p s = case toInt s of
  Ok i -> stageParameter p i
  _    -> message guiActions.address NoOp

stageBool : Parameter -> Bool -> Message
stageBool p b = case b of
  True  -> stageParameter p 1
  False -> stageParameter p 0

stageKeyboard : Int -> Message
stageKeyboard kb = stageParameter KeyboardLayout kb

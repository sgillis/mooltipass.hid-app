module ChromeBgMessage where

-- Elm standard library
import List
-- local source
import BackgroundState exposing (..)
import CommonState exposing (..)
import Byte exposing (..)
import DevicePacket exposing (..)

type alias ToChromeMessage = {readFile : Maybe String}

emptyToChromeMessage = {readFile = Nothing}

encode : BackgroundState -> ToChromeMessage
encode s =
    let e = emptyToChromeMessage
    in case s.mediaImport of
        MediaImportRequested p -> {e | readFile <- Just p}
        _ -> e

type alias FromChromeMessage = {readFile : List ByteArray}

decode : FromChromeMessage -> BackgroundAction
decode msg = case msg.readFile of
    [] -> SetMediaImport (MediaImportError "empty file")
    bs -> SetMediaImport (MediaImportStart (List.map OutgoingImportMedia bs))

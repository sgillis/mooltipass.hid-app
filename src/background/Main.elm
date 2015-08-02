module Background where

-- Elm standard library
import Signal exposing (..)
import Time exposing (..)
import List exposing ((::))

-- local source
import ToGuiMessage
import ToGuiMessage exposing (ToGuiMessage)
import FromGuiMessage
import FromGuiMessage exposing (FromGuiMessage)
import CommonState exposing (CommonAction)
import DeviceMessage exposing (..)
import DeviceMessage
import BackgroundState exposing (..)
import ExtensionMessage exposing (..)
import ExtensionMessage
import DevicePacket exposing (..)
import ChromeBgMessage
import ChromeBgMessage exposing (..)

port fromGUI : Signal FromGuiMessage

port toGUI : Signal ToGuiMessage
port toGUI = map (ToGuiMessage.encode << .common) (dropRepeats state)

port fromDevice : Signal FromDeviceMessage

port deviceStatus : Signal Int

port toDevice : Signal ToDeviceMessage
port toDevice = map (\(m,_,_) -> m) output

port toChrome : Signal ToChromeMessage
port toChrome = map ChromeBgMessage.encode state

port fromChrome : Signal FromChromeMessage

port debug : Signal (Bool, Bool, String)
port debug = dropRepeats <|
    map (\s -> ( s.waitingForDevice
               , s.blockSetExtRequest
               , printExtensionRequest s.extRequest
               )
    ) state

state : Signal BackgroundState
state = map (\(_,_,s) -> s) output

output : Signal (ToDeviceMessage, ToExtensionMessage, BackgroundState)
output =
    let go ias (dm,em,s) =
        let s'                   = apply ias s
            (deviceMessage, a1s) = DeviceMessage.encode s'
            s''                  = apply a1s s'
            (extMessage, a2)     = ExtensionMessage.encode s''
        in (deviceMessage, extMessage, update a2 s'')
    in foldp go (emptyToDeviceMessage, emptyToExtensionMessage, default) inputActions

port fromExtension : Signal FromExtensionMessage

port toExtension : Signal ToExtensionMessage
port toExtension = map (\(_,m,_) -> m) output

inputActions : Signal (List BackgroundAction)
inputActions = mergeMany
    [ map (\m -> [CommonAction (FromGuiMessage.decode m)]) fromGUI
    , map (\m -> [ExtensionMessage.decode m]) fromExtension
    , map (\m -> [ChromeBgMessage.decode m]) fromChrome
    , map DeviceMessage.decode fromDevice
    , map (\m -> [DeviceMessage.decodeStatus m]) (dropRepeats deviceStatus)
    , map (\_ -> []) (every second)
    ]

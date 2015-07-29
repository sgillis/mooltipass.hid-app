module ExtensionMessage where

-- Elm standard library
import Maybe

-- local source
import BackgroundState exposing (..)
import CommonState as Common
import CommonState exposing (..)
import Byte exposing (..)
import Result exposing (..)
import Util exposing (..)

type alias FromExtensionMessage =
    { ping      : Maybe ()
    , getInputs : Maybe { context : String }
    , update    : Maybe { context  : String
                        , login    : String
                        , password : String
                        }
    , getRandom : Maybe ()
    }

type alias ToExtensionMessage =
    { deviceStatus : Maybe { connected : Bool
                           , version : String
                           , state : String
                           }
    , credentials  : Maybe { context  : String
                           , login    : String
                           , password : String
                           }
    , noCredentials  : Maybe ()
    , updateComplete : Maybe ()
    , random         : Maybe String
    }

emptyToExtensionMessage =
    { deviceStatus   = Nothing
    , credentials    = Nothing
    , noCredentials  = Nothing
    , updateComplete = Nothing
    , random         = Nothing
    }

decode : FromExtensionMessage -> BackgroundAction
decode message =
    let decode' {ping, getInputs, update, getRandom} =
        Maybe.oneOf
            [ Maybe.map (\_ -> SetExtAwaitingPing True) ping
            , Maybe.map (set ExtWantsCredentials) getInputs
            , Maybe.map (set ExtWantsToWrite) update
            , Maybe.map (\_ -> SetExtRequest ExtWantsRandomNumber) getRandom
            ]
        set constructor d = SetExtRequest (constructor d)
        -- we do a bytestring conversion to check for errors but we just use
        -- the string above as ByteString is just a type alias
        errOrInputs =
            Maybe.map
                (\{context} -> byteString context)
                message.getInputs
        errOrUpdate = Maybe.map errOrUpdate' message.update
        errOrUpdate' {context, login, password} =
            let bSc = byteString context
                bSl = byteString login
                bSp = byteString password
            in bSc `andThen` (\_ -> bSl)
                   `andThen` (\_ -> bSp)
                   `andThen` (\_ -> Ok ())
    in case errOrUpdate of
        Just (Err err) -> CommonAction
                            (AppendToLog ("Extension Error: " ++ err))
        _              -> case errOrInputs of
            Just (Err err) -> CommonAction
                                (AppendToLog ("Extension Error: " ++ err))
            _              -> Maybe.withDefault NoOp (decode' message)

encode : BackgroundState -> (ToExtensionMessage, BackgroundAction)
encode s =
    let e = emptyToExtensionMessage
    in  if | s.extAwaitingPing ->
            ({e | deviceStatus <- Just <|
                { connected = s.common.deviceStatus == Unlocked
                , version   = Maybe.withDefault "unknown"
                                (Maybe.map (\v -> v.version) s.deviceVersion)
                , state     = case s.common.deviceStatus of
                    NotConnected -> "NotConnected"
                    Unlocked -> "Unlocked"
                    NoCard -> "NoCard"
                    Locked -> "Locked"
                    ManageMode -> "ManageMode"
                    UnknownCard -> "UnknownCard"
                }
            }, SetExtAwaitingPing False)
           | s.extRequest /= NoRequest -> case s.extRequest of
                ExtCredentials    c  ->
                    ({e | credentials <- Just c}, SetExtRequest NoRequest)
                ExtWriteComplete _  ->
                    ({e | updateComplete <- Just ()}, SetExtRequest NoRequest)
                ExtNoCredentials ->
                    ({e | noCredentials <- Just ()}, SetExtRequest NoRequest)
                ExtNotWritten -> (e, SetExtRequest NoRequest)
                ExtRandomNumber n -> ({e | random <- Just n}, SetExtRequest NoRequest)
                _ -> (e,NoOp)
           | otherwise -> (e, NoOp)

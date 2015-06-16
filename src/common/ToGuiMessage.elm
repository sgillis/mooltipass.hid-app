module ToGuiMessage where

import Maybe

-- local source
import Byte exposing (..)
import CommonState exposing (..)

type alias ToGuiMessage = { setLog          : (List String)
                          , setDeviceStatus : Int
                          , setImportInfo   : (Int,FileId,Int,Int)
                          , setMemInfo      : (Int, Maybe MemInfoData)
                          , getStringCmd    : Maybe Int
                          , setParameter    : Maybe (Int, Byte)
                          , getParameter    : Maybe Int
                          , settingsInfo    : SettingsInfo
                          , strCmdInfo      : StringCmdInfo
                          }

encode : CommonState -> ToGuiMessage
encode s =
    
    { setLog = s.log
    , setDeviceStatus = case s.deviceStatus of
                    NotConnected -> 0
                    Unlocked     -> 1
                    NoCard       -> 2
                    Locked       -> 3
                    ManageMode   -> 4
                    UnknownCard  -> 5
    , setImportInfo = case s.importInfo of
        NoImport           -> (0,"",0,0)
        ImportRequested id -> (1,id,0,0)
        Importing id i1 i2 -> (2,id,i1,i2)
        Imported id        -> (3,id,0,0)
        ImportError s      -> (4,s ,0,0)
    , setMemInfo = case s.memoryInfo of
        NoMemInfo               -> (0, Nothing)
        MemInfoRequest          -> (1, Nothing)
        MemInfoWaitingForUser   -> (2, Nothing)
        MemInfoWaitingForDevice -> (3, Nothing)
        MemInfo d               -> (4, Just d)
    , getStringCmd = s.getStringCmd
    , setParameter = Maybe.map (\(p,b) -> (encodeParameter p, b)) s.setParameter
    , getParameter = Maybe.map encodeParameter s.getParameter
    , settingsInfo = s.settingsInfo
    , strCmdInfo   = s.strCmdInfo
    }

decode : ToGuiMessage -> List CommonAction
decode msg=
    let setDeviceStatus =
        case msg.setDeviceStatus of
            0 -> NotConnected
            1 -> Unlocked
            2 -> NoCard
            3 -> Locked
            4 -> ManageMode
            5 -> UnknownCard
        setImportInfo = case msg.setImportInfo of
           (0,"",0,0)   -> NoImport
           (1,id,0,0)   -> ImportRequested id
           (2,id,i1,i2) -> Importing id i1 i2
           (3,id,0,0)   -> Imported id
           (4,s,0,0)    -> ImportError s
        setMemInfo = case msg.setMemInfo of
            (0, Nothing) -> NoMemInfo
            (1, Nothing) -> MemInfoRequest
            (2, Nothing) -> MemInfoWaitingForUser
            (3, Nothing) -> MemInfoWaitingForDevice
            (4, Just d)  -> MemInfo d
        getStringCmd = case msg.getStringCmd of
            Nothing -> []
            Just c  -> [GetStringCmd (Just c)]
        setParam = case msg.setParameter of
            Nothing -> []
            Just (p,b) -> [SetParameter (Just (decodeParameter p, b))]
        getParam = case msg.getParameter of
            Nothing -> []
            Just p  -> [GetParameter (Just (decodeParameter p))]
    in  [ SetLog msg.setLog
        , SetDeviceStatus setDeviceStatus
        , SetImportInfo setImportInfo
        , SetMemInfo setMemInfo
        , CommonSettings msg.settingsInfo
        , CommonStrCmds  msg.strCmdInfo
        ] ++ getStringCmd ++ setParam ++ getParam

module BackgroundState where

-- Elm standard library
import Maybe
import List (..)

-- local source
import CommonState as Common
import CommonState (..)
import DevicePacket (..)
import DeviceFlash (..)
import Byte (..)

type alias BackgroundState = { deviceConnected  : Bool
                             , deviceVersion    : Maybe MpVersion
                             , waitingForDevice : Bool
                             , currentContext   : ByteString
                             , extAwaitingPing  : Bool
                             , extRequest       : ExtensionRequest
                             , mediaImport      : MediaImport
                             , memoryManage     : MemoryManageState
                             , common           : CommonState
                             }

default : BackgroundState
default = { deviceConnected  = False
          , deviceVersion    = Nothing
          , waitingForDevice = False
          , currentContext   = ""
          , extAwaitingPing  = False
          , extRequest       = NoRequest
          , mediaImport      = NoMediaImport
          , memoryManage     = NotManaging
          , common           = Common.default
          }

type MemoryManageState =
      NotManaging
    | MemManageRequested
    | MemManageWaiting
    | MemRead
    | MemReadWaiting
    | MemReadSuccess  (ParentNode, List Favorite)
    | MemWrite        (List SendPacket)
    | MemWriteWaiting (List SendPacket)
    | MemWriteSuccess
    | MemManageError  String

type MediaImport =
      NoMediaImport
    | MediaImportRequested    FileId
    | MediaImportStart        (List SendPacket)
    | MediaImportStartWaiting (List SendPacket)
    | MediaImport             (List SendPacket)
    | MediaImportWaiting      (List SendPacket)
    | MediaImportError        String
    | MediaImportSuccess

mediaImportActive : BackgroundState -> Bool
mediaImportActive s = case s.mediaImport of
    NoMediaImport             -> False
    MediaImportError _        -> False
    MediaImportSuccess        -> False
    _                         -> True

type ExtensionRequest =
      ExtWantsCredentials     { context : ByteString }

    | ExtNeedsPassword        { context : ByteString
                              , login   : ByteString
                              }

    | ExtCredentials          { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNoCredentials

    | ExtWantsToWrite         { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNeedsNewContext      { context  : ByteString
                              , login    : ByteString
                              , password : ByteString
                              }

    | ExtNeedsToWritePassword { context  : ByteString
                              , password : ByteString
                              }

    | ExtWriteComplete        { context  : ByteString }

    | ExtNotWritten

    | NoRequest

extensionRequestToLog : ExtensionRequest -> Maybe String
extensionRequestToLog d = case d of
    ExtWantsCredentials {context} ->
        Just <| "> requesting credentials for " ++ context
    ExtWantsToWrite {context, login, password} ->
        Just <| "> requesting to write credentials for " ++ context
    ExtNeedsNewContext {context, login, password} ->
        Just <| "> adding new credentials for " ++ context
    ExtNoCredentials    -> Just "access denied or no credentials"
    ExtNotWritten       -> Just "access denied"
    ExtWriteComplete _  -> Just "credentials written"
    ExtCredentials   _  -> Just "credentials retrieved"
    _ -> Nothing

type BackgroundAction = SetHidConnected     Bool
                      | SetWaitingForDevice Bool
                      | SetExtAwaitingPing  Bool
                      | SetExtRequest       ExtensionRequest
                      | SetMediaImport      MediaImport
                      | Interpret             ReceivedPacket
                      | CommonAction        CommonAction
                      | NoOp

apply : List BackgroundAction -> BackgroundState -> BackgroundState
apply actions state = foldr update state actions

update : BackgroundAction -> BackgroundState -> BackgroundState
update action s =
    let updateCommon a = Common.update a s.common
    in case action of
        SetHidConnected b ->
            if not b
            then apply
               [ CommonAction (SetDeviceStatus NotConnected)
               , if mediaImportActive s
                 then SetMediaImport (MediaImportError "device disconnected")
                 else NoOp
               ]
               {s | deviceConnected <-  False}
            else {s | deviceConnected <- True}
        SetExtAwaitingPing b -> {s | extAwaitingPing <- b}
        SetExtRequest d -> {s | extRequest <- d}
        SetMediaImport t -> setMedia t s
        SetWaitingForDevice b -> {s | waitingForDevice <- b}
        CommonAction (SetDeviceStatus c) ->
            let s' = {s | common <- updateCommon (SetDeviceStatus c)}
            in if c /= s.common.deviceStatus
               then update
                        ( if mediaImportActive s && (c == Locked || c == NotConnected)
                          then SetMediaImport (MediaImportError "interrupted by device")
                          else NoOp )
                    {s' | common <-
                            Common.update
                                (AppendToLog (connectToLog c))
                                s'.common
                        , currentContext <- ""
                        , deviceVersion  <- if c == NotConnected
                                            then Nothing
                                            else s.deviceVersion
                    }
               else s
        CommonAction (StartImportMedia p) ->
            if not (mediaImportActive s)
            then setMedia (MediaImportRequested p) s
            else s
        CommonAction a -> {s | common <- updateCommon a}
        Interpret p -> interpret p s
        NoOp -> s


interpret : ReceivedPacket -> BackgroundState -> BackgroundState
interpret packet s =
    case packet of
        ReceivedGetLogin ml -> case ml of
            Just l ->
                {s | extRequest <- case s.extRequest of
                          ExtWantsCredentials c ->
                              ExtNeedsPassword {c | login = l}
                          _ -> NoRequest
                }
            Nothing -> {s | extRequest <- ExtNoCredentials}
        ReceivedGetPassword mp -> case mp of
            Just p ->
                {s | extRequest <- case s.extRequest of
                          ExtNeedsPassword c ->
                              ExtCredentials {c | password = p}
                          _ -> NoRequest
                }
            Nothing -> {s | extRequest <- ExtNoCredentials}
        ReceivedSetLogin r ->
            {s | extRequest <- case s.extRequest of
                     ExtWantsToWrite c ->
                         if r == Done
                         then ExtNeedsToWritePassword { c - login }
                         else ExtNotWritten
                     _ -> NoRequest
            }
        ReceivedSetPassword r ->
            {s | extRequest <- case s.extRequest of
                     ExtNeedsToWritePassword c ->
                         if r == Done
                         then ExtWriteComplete { c - password }
                         else ExtNotWritten
                     _ -> NoRequest
            }
        ReceivedSetContext r ->
            case r of
                ContextSet -> case s.extRequest of
                    ExtWantsCredentials c ->
                        {s | currentContext <- c.context}
                    ExtWantsToWrite c ->
                        {s | currentContext <- c.context}
                    ExtNeedsPassword c  ->
                        {s | currentContext <- c.context}
                    ExtNeedsToWritePassword c ->
                        {s | currentContext <- c.context}
                    -- this fall-through would be: we have no idea what
                    -- context we set so we just keep the original state
                    _ -> s
                UnknownContext -> case s.extRequest of
                    ExtWantsToWrite c ->
                        {s | extRequest <- ExtNeedsNewContext c}
                    ExtWantsCredentials _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsPassword _ ->
                        {s | extRequest <- ExtNoCredentials}
                    ExtNeedsToWritePassword _ ->
                        {s | extRequest <- ExtNotWritten}
                    _ -> s
                NoCardForContext ->
                    update (CommonAction (SetDeviceStatus NoCard)) s
        ReceivedAddContext r ->
            {s | extRequest <- case s.extRequest of
                     ExtNeedsNewContext c ->
                         if r == Done
                         then ExtWantsToWrite c
                         else ExtNotWritten
                     _ -> NoRequest
            }
        ReceivedGetStatus st ->
            update (CommonAction (SetDeviceStatus (case st of
                       PacketNoCard     -> NoCard
                       PacketLocked     -> Locked
                       PacketLockScreen -> Locked
                       PacketUnlocked   -> Unlocked
                    ))) s
        ReceivedGetVersion v ->
                appendToLog
                    ("device is "
                        ++ v.version ++ " "
                        ++ toString v.flashMemSize
                        ++ "MBit")
                {s | deviceVersion <- Just v}
        ReceivedImportMediaStart r ->
            case s.mediaImport of
                MediaImportStartWaiting ps ->
                    if r == Done
                    then setMedia (MediaImport ps) s
                    else setMedia (MediaImportError "Import start failed") s
                _ -> setMedia (MediaImportError (unexpected "ImportMediaStart")) s
        ReceivedImportMedia r ->
            case s.mediaImport of
                MediaImportWaiting (p::ps) ->
                    if r == Done
                    then setMedia (MediaImport ps) s
                    else setMedia (MediaImportError "Import write failed") s
                _ -> setMedia (MediaImportError (unexpected "ImportMedia")) s
        ReceivedImportMediaEnd r ->
            case s.mediaImport of
                MediaImportWaiting [] ->
                    if r == Done
                    then setMedia MediaImportSuccess s
                    else setMedia (MediaImportError "Import end-write failed") s
                _ -> setMedia (MediaImportError (unexpected "ImportMediaEnd")) s
        x -> appendToLog
                ("Error: received unhandled packet " ++ toString x)
                s

setMedia : MediaImport -> BackgroundState -> BackgroundState
setMedia imp s =
    let c             = s.common
        s'            = {s | mediaImport <- imp}
        updateInfo i =
            {s' | common <- updateCommon (SetImportInfo i)}
        updateCommon a = Common.update a s.common
    in case imp of
        MediaImportError str    -> updateInfo (ImportError str)
        MediaImportRequested id -> updateInfo (ImportRequested id)
        MediaImportStart ps     -> case s.common.importInfo of
            ImportRequested id ->
                updateInfo (Importing id (length ps) (length ps))
            _ -> updateInfo
                (ImportError (unexpected "MediaImportStart"))
        MediaImport ps          -> case s.common.importInfo of
            Importing id _ total   ->
                updateInfo (Importing id (length ps) total)
            _ -> updateInfo
                    (ImportError (unexpected "MediaImport"))
        MediaImportSuccess      -> case s.common.importInfo of
            Importing id _ _
                -> updateInfo (Imported id)
            _ -> updateInfo
                    (ImportError (unexpected "MediaImportSuccess"))
        _ -> s'


fromResult :  Result Error ReceivedPacket -> BackgroundAction
fromResult r = case r of
    Err err -> appendToLog' ("HID Error: " ++ err)
    Ok p    -> Interpret p


appendToLog' str = CommonAction (AppendToLog str)
appendToLog str state = update (appendToLog' str) state

unexpected str = "Received unexpected " ++ str ++  " from device"


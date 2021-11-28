module GUI.SLEConnections
    ( RafSiiStatus
    , SleServiceStatus(..)
    , setupRAFConnection
    , addRafConnection
    , updateRafStatus
    ) where

import           RIO

import           GI.Gtk

import           Data.PUS.Config

-- import           Protocol.ProtocolInterfaces

import           GUI.StatusEntry
import           GUI.StatusButton
import           GUI.Utils


data RafSiiStatus = RafSiiStatus
    { _rafFrame   :: !Frame
    , _rafBtBind  :: !Button
    , _rafBtStart :: !Button
    , _rafStatus  :: !Entry
    }


data SleServiceStatus =
    SleServiceUninit
    | SleServiceInit
    | SleServiceBound
    | SleServiceActive
    deriving (Eq, Ord, Enum, Show)

instance Display SleServiceStatus where
    display SleServiceUninit = "UNINIT"
    display SleServiceInit   = "INIT"
    display SleServiceBound  = "BOUND"
    display SleServiceActive = "ACTIVE"



addRafConnection :: Box -> RafSiiStatus -> IO ()
addRafConnection parent status = do
    boxPackStart parent (_rafFrame status) False False 5

setupRAFConnection :: SLERafConfig -> IO RafSiiStatus
setupRAFConnection rafCfg = do
    builder <- builderNewFromResource "/auris/data/SLEStatus.glade"

    frame   <- getObject builder "sleRAFFrame" Frame

    sii     <- getObject builder "entryRAFSII" Entry
    peer    <- getObject builder "entryRAFPeer" Entry
    port    <- getObject builder "entryRAFPort" Entry
    status  <- getObject builder "entryRAFStatus" Entry

    bind    <- getObject builder "buttonRAFBind" Button
    start   <- getObject builder "buttonRAFStart" Button

    vers    <- getObject builder "labelRAFVersion" Label

    entrySetText sii  (cfgSleRafSII rafCfg)
    entrySetText peer (cfgSleRafPeerID rafCfg)
    entrySetText port (cfgSleRafPort rafCfg)

    labelSetLabel vers (textDisplay (cfgSleRafVersion rafCfg))

    widgetSetName status "error-entry"
    widgetSetName bind "yellow-button"
    widgetSetName start "yellow-button"

    let g = RafSiiStatus { _rafFrame   = frame
                         , _rafBtBind  = bind
                         , _rafBtStart = start
                         , _rafStatus  = status
                         }

    updateRafStatus g SleServiceUninit

    pure g


bindLabel :: Text 
bindLabel = "Bind"

unbindLabel :: Text 
unbindLabel ="Unbind"

startLabel :: Text 
startLabel = "Start"

stopLabel :: Text 
stopLabel = "Stop"


updateRafStatus :: RafSiiStatus -> SleServiceStatus -> IO () 
updateRafStatus g SleServiceUninit = do 
    widgetSetName (_rafStatus g) "error-entry"
    buttonSetLabel (_rafBtBind g) bindLabel 
    buttonSetLabel (_rafBtStart g) startLabel 
    widgetSetSensitive (_rafBtBind g) True 
    widgetSetSensitive (_rafBtStart g) False 

updateRafStatus g SleServiceInit = do 
    widgetSetName (_rafStatus g) "warn-entry"
    buttonSetLabel (_rafBtBind g) bindLabel 
    buttonSetLabel (_rafBtStart g) startLabel 
    widgetSetSensitive (_rafBtBind g) True 
    widgetSetSensitive (_rafBtStart g) False     

updateRafStatus g SleServiceBound = do 
    widgetSetName (_rafStatus g) "warn-entry"
    buttonSetLabel (_rafBtBind g) unbindLabel 
    buttonSetLabel (_rafBtStart g) startLabel 
    widgetSetSensitive (_rafBtBind g) True 
    widgetSetSensitive (_rafBtStart g) True         

updateRafStatus g SleServiceActive = do 
    widgetSetName (_rafStatus g) "green-entry"
    buttonSetLabel (_rafBtBind g) unbindLabel 
    buttonSetLabel (_rafBtStart g) stopLabel 
    widgetSetSensitive (_rafBtBind g) True 
    widgetSetSensitive (_rafBtStart g) True         